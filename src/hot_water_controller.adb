-- This program provides control logic and logging for the control of a pump in
-- a split system hot water system. It uses custom hardware to read the
-- temperature of the tank and panels using industrial Pt100 temperature sensors
-- System control is provided by a model 3B Raspberry Pi.
-- Author    : David Haley
-- Created   : 02/11/2017
-- Last Edit : 20/08/2022
-- 20230917 : Exception termination rearranged to ensure termination when an
-- exception is rased in a task, preventing its completion.
-- 20220820 :  Events_and_Errors move to DJH.Events_and_Errors.
-- 20220715 : -SV removed from Controller_State entries.
-- 20220531 : Starting of boost delayed so that the clock is likely to be valid
-- before the next boost time is read.
-- 20220523 : Signal handling provided to ensure orderly shutdown with systemd
-- or ctrl c.
-- 20210624 : Logging of time changes provided.
-- 20210417 : Hot_Area_SV replaced by Average_Difference_SV
-- 20210331 : Pump start condition changed to require zero or positive area
-- under the temperature difference curve. Increment Run_Time removed.
-- Stop condition changed to >= which corrected an existing error, the minimum
-- was as specified; however one additional second was recorded for each run.
-- 20210306 : Start_Boost added to Initialise
-- 20210303 : Unified version of date and time strings used
-- 20210221 : Exception_Stop entry added to Main_Loop.
-- 20210220 : Enhanced logging of exceptions added.
-- 20190406 : Automatic boost feature added
-- 20190307 : Logging file redirection removed to data logger
-- 20190306 : Cold_Delay_SV processed to reduce the number of pump starts when
-- small mounts of cold water are added to the tank, particularly late in the
-- day or at night.
-- 20190226 : log file text changed, entries start with date and time, pump
-- accumulated time at startup recorded in event file
-- 20190124 : Controller version transferred to Global_Data to facilate transfer
-- to the user interface.
-- 20190121 : Updated to timed version watchdog fix, relinked etc.
-- 20171124 Timebase of main loop changed from Ada.Calendar to Ada.Real_Time.
-- Change of timebase alone did not correct the problem because the step forward
-- in time resulted in loop running multiple times per seond. The full fix was
-- to reread the clock and make the loop run on exact seconds.
-- 20171108 Corrected Shutdown Message
-- 20171106 Controller_State.Increment_Run_Time made unconditional

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Calendar;
with Ada.Command_Line; use Ada.Command_Line;
with DJH.Date_and_Time_Strings; use DJH.Date_and_Time_Strings;
with DJH.Events_and_Errors; use DJH.Events_and_Errors;
with RPi_Watchdog;
with Linux_Signals; use Linux_Signals;
with Pump_Controller_Types; use Pump_Controller_Types;
with Global_Data; use Global_Data;
with Configuration; use Configuration;
with Temperature; use Temperature;
with User_Interface_Server; use User_Interface_Server;
with Data_Logger; use Data_Logger;
with Boost; use Boost;

procedure Hot_Water_Controller is

   package Controller_Watchdog is new RPi_Watchdog;
   use Controller_Watchdog;

   task Main_Loop is
      entry Start;
      entry Stop;
      entry Exception_Stop;
      entry Watchdog_Delay;
   end Main_Loop;

   procedure Initialise is

   begin -- Initialise
      Start_Events;
      Start_Logger;
      Put_Event ( "HWS Pump Controller version " & Controller_Version &
                    " started, accumulated pump run seconds:" &
                    Accumulated_Times'Image
                    (Controller_State.Accumulated_Pump_Run_Time));
      Main_Loop.Start;
      Handlers.Install; -- Ctrl C and SIGTERM Handelers
      Start_User_Interface;
      Main_loop.Watchdog_Delay;
      -- allow for multiple current pump cycles before enabling watchdog;
      Put_Event ("Watchdog enabled");
      Enable_Watchdog;
      Start_Boost;
      -- Last task to start delayed return to allow home automation to start.
   end Initialise;

   task body Main_Loop is

      Watchdog_Enable_Count : constant Natural := 100;
      Loop_Interval : constant Integer := 1; -- Main loop runs once per second
      Main_Loop_Interval : constant Time_Span := Seconds (Loop_interval);
      Current_Seconds : Seconds_Count;
      Current_Time_Span : Time_Span;
      Next_Time : Time := Clock + Main_Loop_Interval;
      Tank, Panel : Temperatures;
      Run_Main_Loop : Boolean := True;
      Main_Loop_Counter : Natural := 0;
      Old_Time : Ada.Calendar.Time :=
        Ada.Calendar."+" (Ada.Calendar.Clock,
                          Standard.Duration (Loop_Interval));

   begin -- Main_Loop
      accept Start;
      while Run_Main_Loop loop
         select
            when Handlers.Signal_Stop or Ctrl_C_Stop=>
               accept Stop do
                  if Handlers.Signal_Stop then
                     Put_Event ("Shutdown initiated by SYSTERM");
                  elsif Ctrl_C_Stop then
                     Put_Event ("Shutdown initiated by crtl c");
                  end if; -- Handlers.Signal_Stop
                  Controller_State.Pump_Stop;
                  Disable_Watchdog;
                  Stop_Sampling_Temperature;
                  Stop_Boost;
                  Stop_Logger;
                  Stop_User_Interface;
                  Stop_Events;
                  Run_Main_Loop := False;
               end Stop;
         or
            when Main_Loop_Counter = Watchdog_Enable_Count =>
               accept Watchdog_Delay;
         or
            accept Exception_Stop do
               Controller_State.Pump_Stop;
               select
                  Stop_Boost;
               or
                  delay 66.0;
                  abort Boost_Task;
               end select;
               select
                  Stop_Logger;
               or
                  delay 66.0;
                  abort Logger;
               end select;
               select
                  Stop_User_Interface;
               or
                  delay 3.3;
                  abort UI_Server;
               end select;
               Disable_Watchdog;
               select
                  Stop_Sampling_Temperature;
               or
                  delay 1.1;
                  abort Sample_Temperature;
               end select;
               Stop_Events;
               Run_Main_Loop := False;
            end Exception_Stop;
         or
            delay until Next_Time;
            -- The following code is intended to prevent the main loop running
            -- much faster than once per second, even if a substantial step
            -- forward in time occurs which is typical of RPi startup when the
            -- Clock is set from an NTP server;
            if Clock - Next_Time > Main_Loop_Interval * 2 then
               Put_Event ("Time changed, old time " &
                            Date_String (Full_Date, Old_Time) & ", " &
                            Time_String (Old_Time));
            end if; -- Clock - Next_Time > Main_Loop_Interval * 2
            Split (Clock, Current_Seconds, Current_Time_Span);
            Next_Time := Time_Of (Current_Seconds, Main_Loop_Interval);
            Old_Time := Ada.Calendar."+" (Ada.Calendar.Clock,
                                          Standard.Duration (Loop_Interval));
            Kick_Watchdog_High;
            Reset_Progress;
            Average_Temperature (Tank, Panel);
            Controller_State.Write_Temperature (Tank, Panel);
            Samplmpling_Progressing;
            Kick_Watchdog_Low;
            -- At this point in the execution cycle slightly more half the cycle
            -- time should have elapsed as indicated from the temperature
            -- measurement process.
            -- Start of pump control logic
            If Panel > Tank + Start_Difference and
              Tank < Maximum_Tank_Temperature and
              Controller_State.Average_Difference >= 0.0 then
               Controller_State.Pump_Start;
            elsif (Panel < Tank + Stop_Difference and
                     Controller_State.Pump_Run_Time >= Minimum_Pump_Run_Time)
              or Tank >= Maximum_Tank_Temperature then
               Controller_State.Pump_Stop;
            end if; -- End of pump control logic
            if Tank > Alarm_Temperature then
               Controller_State.Set_Fault (Tank_Temperature);
            end if; -- Tank > Alarm_Temperature
            if Main_Loop_Counter < Watchdog_Enable_Count then
               -- The above test is more or less redundant because Natural'Last
               -- should a large enough that the controller would never run long
               -- enough for Main_Loop_Counter to overflow but it ensures
               -- = test passes in the Watchdog_Delay entry.
               Main_Loop_Counter := Main_Loop_Counter + 1;
            end if; -- Main_Loop_Counter <= Watchdog_Enable_Count
         end select;
      end loop; -- Run_Main_Loop
   end Main_Loop;

begin -- Hot_Water_Controller
   Initialise;
   Main_Loop.Stop; -- Only returns after main loop stopped
   Handlers.Remove; -- Ctrl C and SIGTERM Handelers
   Set_Exit_Status (Success);
exception
   when Event : others =>
      Put_Error ("HW controller unhandled exception", Event);
      select
         Main_Loop.Exception_Stop;
      or
         delay 150.0;
         abort Main_Loop;
      end select;
      Handlers.Remove; -- Ctrl C and SIGTERM Handelers
      Set_Exit_Status (Failure);
end Hot_Water_Controller;
