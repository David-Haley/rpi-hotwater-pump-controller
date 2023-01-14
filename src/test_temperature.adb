-- Author    : David Haley
-- Created   : 21/10/2017
-- Last Edit : 31/03/2021
-- 20210331 : Increment_Run_Time removed.
-- 20210303 : Unified date and time strings used
-- 06/11/2017 Controller_State.Increment_Run_Time made unconditional
-- 28/10/2017 updated to allow testing of data logger

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with RPi_GPIO; use RPi_GPIO;
with RPi_SPI; use RPi_SPI;
with RPi_Watchdog;
with Pump_Controller_Types; use Pump_Controller_Types;
with Temperature; use Temperature;
with data_Logger; use Data_Logger;
with DJH.Date_and_Time_Strings; use DJH.Date_and_Time_Strings;
with Configuration; use Configuration;
with Global_Data; use Global_Data;

procedure Test_Temperature is

   package My_Watchdog is new RPi_Watchdog; use My_Watchdog;

   Test_Requested : Character;

   task Main_Loop is
      Entry Stop;
   end Main_Loop;

   task body Main_Loop is

      Run_Main_Loop_Interval : constant Duration := 1.0;
      Next_Time : Time := Clock + Run_Main_Loop_Interval;
      Tank : Temperatures;
      Panel : Temperatures;

   begin -- Main_Loop
      loop
         select
            accept Stop;
            exit;
         or
            delay until Next_Time;
            Next_Time := Next_Time + Run_Main_Loop_Interval;
            Kick_Watchdog_High;
            Reset_Progress;
            Average_Temperature (Tank, Panel);
            Controller_State.Write_Temperature (Tank, Panel);
            Put (Time_String);
            Temperature_IO.Put (Tank, 4, 1, 0);
            Put (",");
            Temperature_IO.Put (Panel, 4, 1, 0);
            New_Line;
            Samplmpling_Progressing;
            -- Pump control logic
            If Panel > Tank + Start_Difference then
               Controller_State.Pump_Start;
            elsif Panel < Tank + Stop_Difference then
               Controller_State.Pump_Stop;
            end if; -- pump control logic
            Kick_Watchdog_Low;
         end select;
      end loop;  -- Run_Main_Loop
      Put_Line ("Main Loop Exiting");
   end Main_Loop;

   procedure Initialise is

   begin -- Initialise
      Put_Line ("Test Temperature 20171028");
      Put_Line ("0 Exit");
      Put_Line ("Reading Configuration");
      delay 10.0;
      Put_Line ("Enabling_Watchdog");
      Enable_Watchdog;
   end Initialise;

begin -- Test_Temperature
   Initialise;
   loop
      Put ("Test? ");
      Get (Test_Requested);
      case Test_Requested is
         when '0' =>
            Put_Line ("Disabling_Watchdog ready for exit");
            Disable_Watchdog;
            Put_Line ("Stopping main loop for exit");
            Main_Loop.Stop;
            Put_Line ("Stop Logging");
            Stop_Logger;
            Put_Line ("Stopping Temperature_Sampling");
            Stop_Sampling_Temperature;
            Put_Line ("Ending Tests");
            exit;
         when others =>
            Put_Line ("Invalid Test Request");
      end case; -- Test_Requested
   end loop;
end Test_Temperature;


