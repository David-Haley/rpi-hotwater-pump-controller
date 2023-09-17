-- This package provides for periodic operation of the boost element for the
-- purposes of sanitising (killing Leagionella) and for comfort.
-- Author    : David Haley
-- Created   : 04/04/2019
-- Last Edit : 16/09/2023
-- 20230916 : Boost task renamed to Boost_Task to avoid conflict with package
-- name. Boost start delayed to allow home automation to start first.
-- 20220820 :  Events_and_Errors move to DJH.Events_and_Errors.
-- 20220729 : Comfortable temperature reached event added and reset event
-- wording changed.
-- 20220723 : Event added for comfort reset.
-- 20220719 : Correction to test that calls Clear_Is_Comfortable, including
-- Time_Zone so local time rather than UTC is used.
-- 20220715 : Indirect call to Boost entries converted to renames and _SV
-- removed from Controller_State entries.
-- 20220714 : Clear_Is_Comfortable is now called at Comfort_Hour.
-- 20220607 : Correction to safety turn off.
-- 20220531 : On start up update boost time if it is in the past 
-- 20220523 : Automatic Boost for comfort added.
-- 20210722 : Manual Boost facility added.
-- 20210306 : Start_Boost added
-- 20210303 : Unified version of date and time strings used.
-- 20210220 : Now uses Events and Errors
-- 20190416 : Boost production build without debug code.
-- 20190408 : sense of comparison between clock and Next_Boost_Time_SV
-- corrected. Found by inspection, debug code retained.
-- 20190407 : Debug code inserted

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Arithmetic; use Ada.Calendar.Arithmetic;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Directories; use Ada.Directories;
with DJH.Date_and_Time_Strings; use DJH.Date_and_Time_Strings;
with DJH.Events_and_Errors; use DJH.Events_and_Errors;
with Pump_Controller_Types; use Pump_Controller_Types;
with Configuration; use Configuration;
with Global_Data; use Global_Data;
with Home_Automation; use Home_Automation;

package body Boost is

   Next_Boost_File_Name : String := "Next_Boost.";
   Current_Extension : constant String := "txt";
   Backup_Extension : constant String := "bak";
   Temporary_Extension : constant String := "tmp";
   Run_Interval : constant Duration := 60.0;

   function Next_Boost_Time (T : in Time) return time is
   
      -- returns a time where the hour is the Sanitise_Hour as specified in the
      -- configuration file. This time can be in the past if the hour in T is
      -- greater than the hour the Sanitise_Hour.

      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      This_Hour : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number;
      Sub_Second : Second_Duration;
      Leap_Second : Boolean;

   begin -- Next_Boost_Time
      Split (T, Year, Month, Day,
             This_Hour, Minute, Second, Sub_Second,
             Leap_Second, UTC_Time_Offset (T));
      return Time_Of (Year, Month, Day, Boost_Hour, 0, 0, 0.0, Leap_Second,
                      UTC_Time_Offset (T));
   end Next_Boost_Time;

   procedure Read_Next_Boost_Time (Boost_Time : out Boost_Times) is
   
      -- This procedure reads the next boost time file if it exists. If
      -- the file does not exist or contains a bad value it attempts to read the
      -- backup file. If the backup cannot be read or contains a bad value then
      -- new files are created containing a suitable time which is returned; If
      -- either the current file or the backup is read successfully the result
      -- of the read is returned.

      Boost_File : File_Type;
      Success : Boolean := True;

   begin -- Read_Next_Boost_Time
      Boost_Time.Next_Boost_Time := Next_Boost_Time (Clock);
      Boost_Time.Mandatory_Boost_Time := Boost_Time.Next_Boost_Time;
      begin -- read current file
         Open (Boost_File, In_file,
               Next_Boost_File_Name & Current_Extension);
         Boost_Time.Next_Boost_Time := Value (Get_Line (Boost_File));
         Boost_Time.Mandatory_Boost_Time := Value (Get_Line (Boost_File));
         Close (Boost_File);
      exception
         when Event: others =>
            -- do nothing and drop through to read backup file
            Put_Error ("Reading next boost primary", Event);
            if Is_Open (Boost_File) then
               Close (Boost_File);
            end if; --  Is_Open (Boost_File)
            Success := False;
      end; -- read current file
      if not Success then
         Success := True;
         begin -- read backup file
            Open (Boost_File, In_file,
                  Next_Boost_File_Name & Backup_Extension);
            Boost_Time.Next_Boost_Time := Value (Get_Line (Boost_File));
            Boost_Time.Mandatory_Boost_Time := Value (Get_Line (Boost_File));
            Close (Boost_File);
            -- Create a matching current file, this allows the update process to
            -- proceed normally
            Create (Boost_File, Out_File,
                    Next_Boost_File_Name & Current_Extension);
            Put_Line (Boost_File, Image (Boost_Time.Next_Boost_Time));
            Put_line (Boost_File, Image (Boost_Time.Mandatory_Boost_Time));
            Close (Boost_File);
         exception
            when Event : others =>
               -- do nothing and drop through to create new files
               Put_Error ("Reading next boost backup", Event);
               if Is_Open (Boost_File) then
                  Close (Boost_File);
               end if; --  Is_Open (Boost_File)
               Success := False;
         end; -- read backup file
      end if; -- Success
      if not Success then
         -- if execution reaches this code the initial value of Next and
         -- Mandatory will be returned. This will be the case even if an
         -- exception is raised when attempting to create new files;
         begin -- Creat new files
            Put_Event ("Next Boost files not found, time set to: " &
                         Image (Boost_Time.Next_Boost_Time, False,
                         UTC_Time_Offset));
            Create (Boost_File, Out_File, Next_Boost_File_Name &
                      Backup_Extension);
            Put_Line (Boost_File, Image (Boost_Time.Next_Boost_Time));
            Put_Line (Boost_File, Image (Boost_Time.Mandatory_Boost_Time));
            Close (Boost_File);
            Create (Boost_File, Out_File, Next_Boost_File_Name &
                      Current_Extension);
            Put_Line (Boost_File, Image (Boost_Time.Next_Boost_Time));
            Put_Line (Boost_File, Image (Boost_Time.Mandatory_Boost_Time));
            Close (Boost_File);
         exception
            when Event : others =>
               Controller_State.Set_Fault (Boost_Failure);
               Put_Error ("Creating next boost", Event);
               if Is_Open (Boost_File) then
                  Close (Boost_File);
               end if; --  Is_Open (Boost_File)
         end; -- Create new files
      end if; -- not Success
   end Read_Next_Boost_Time;

   procedure Update_Next_Boost_Time is
   
      -- This procedure updated the file next boost time stored in the filing
      -- system. It attempts to maintain two copies of the file, that is, a
      -- current file and a backup file. When a new value is to be written a
      -- a temporary file is creared, the old backup deleted then a sequence of
      -- renames to maintain the current and backup file names.

      Boost_File : File_Type;
      Boost_Time : Boost_Times;

   begin -- Update_Next_Boost_Time
      Boost_Time := Controller_State.Next_Boost;
      Create (Boost_File, Out_File,
              Next_Boost_File_Name & Temporary_Extension);
      Put_Line (Boost_File, Image (Boost_Time.Next_Boost_Time));
      Put_Line (Boost_File, Image (Boost_Time.Mandatory_Boost_Time));
      Close (Boost_File);
      if Exists (Next_Boost_File_Name & Backup_Extension) and then
        Exists (Next_Boost_File_Name & Current_Extension) then
         -- Only delete backup if there is also a current file
         Delete_File (Next_Boost_File_Name & Backup_Extension);
         Rename (Next_Boost_File_Name & Current_Extension,
                 Next_Boost_File_Name & Backup_Extension);
      elsif Exists (Next_Boost_File_Name & Current_Extension) then
         -- There is no backup proceed to rename current file
         Rename (Next_Boost_File_Name & Current_Extension,
                 Next_Boost_File_Name & Backup_Extension);
      end if; -- tests on current and backup file existance
      Rename (Next_Boost_File_Name & Temporary_Extension,
              Next_Boost_File_Name & Current_Extension);
   exception
      when Event : others =>
         Controller_State.Set_Fault (Boost_Failure);
         Put_Error ("Update next boost",  Event);
   end Update_Next_Boost_Time;

   function Is_Next_Day (Old_Time, New_Time : Time) return Boolean is

      Year : Year_Number;
      Month  : Month_Number;
      Day, Next_Day : Day_Number;
      Seconds : Day_Duration;

   begin -- Is_Next_Day
      Split (Old_Time, Year, Month, Day, Seconds);
      Split (New_Time, Year, Month, Next_Day, Seconds);
      return Next_Day > Day;
   end Is_Next_Day;

   function Calculate_Next_Run_Time return Time is

      Year : Year_Number;
      Month  : Month_Number;
      Day : Day_Number;
      Seconds : Day_Duration;
      This_Time : Time;

   begin -- Calculate_Next_Run_Time
      This_Time := Clock;
      if Is_Next_Day (This_Time, This_Time + Run_Interval) then
         Split (This_Time + Run_Interval, Year, Month, Day, Seconds);
         return Ada.Calendar.Time_Of (Year, Month, Day, 0.0);
      else
         Split (This_Time, Year, Month, Day, Seconds);
         Seconds := Duration (Integer (Seconds / Run_Interval)) * Run_Interval
           + Run_Interval;
         return Ada.Calendar.Time_Of (Year, Month, Day, Seconds);
      end if; -- Next_Day (This_Time, This_Time + Run_Interval)
   end Calculate_Next_Run_Time;

   task body Boost_Task is

      Safe_Count : constant Natural := 10;
      -- after Safe_Count consecutive sufficiently high temperature readings the
      -- temperature can be considered safe.
      Boost_Limit : constant Natural := 10 * 60; -- 10 hours
      -- After Boost_Limit minutes the boost is turned off and an an alarm
      -- raised if the required temperature has not been reached.
      Run_Boost : Boolean;
      Next_Time : Time := Calculate_Next_Run_Time;
      Time_Zone : Time_Zones.Time_Offset := UTC_Time_Offset;
      Previous_Hour : Hour_Number := Hour (Next_Time, Time_Zone);
      Boost_Time, Previous_Boost_Time : Boost_Times;
      Over_Comfort_Count, Over_Safe_Count : Natural := 0;
      Boost_is_On, Is_Sanitise : Boolean := False;
      Remaining_Minutes : Natural := Boost_Limit;

   begin -- Boost
      accept Start_Boost do
         Run_Boost := True;
         Read_Next_Boost_Time (Boost_Time);
         Controller_State.Write_Next_Boost_Time (Boost_Time);
         Previous_Boost_Time := Boost_Time;
         delay 120.0; -- Allow time for Home automation to start
         Put_Event ("Boost task starting");
         if Boost_Time.Mandatory_Boost_Time < Next_Boost_Time (Clock) then
            Boost_Time.Next_Boost_Time := Next_Boost_Time (Clock);
            Boost_Time.Mandatory_Boost_Time := Boost_Time.Next_Boost_Time;
            Controller_State.Write_Next_Boost_Time (Boost_Time);
            Put_Event ("Boost time corrected, was in past");
         end if; -- Boost_Time.Mandatory_Boost_Time < Next_Boost_Time (Clock)
      end; -- Start_Boost
      while Run_Boost loop
         select
            accept Stop_Boost do
               Run_Boost := False;
            end; -- Stop_Boost
         or
            delay until Next_Time;
            if Clock - Next_Time > 2.0 * Run_interval then
               -- prevents loop running multiple times in quick sccession if
               -- there is a substantial step in time.
               Next_Time := Calculate_Next_Run_Time;
            else
               Next_Time := Next_Time + Run_Interval;
            end if; -- Clock - Next_Time > 2.0 * Run_interval
            Time_Zone := UTC_Time_Offset;
            Boost_Time := Controller_State.Next_Boost;
            -- Read state variable, may be changed by user interface
            if Controller_State.Tank_Temperature >= Comfort_Temperature then
               Over_Comfort_Count := Over_Comfort_Count + 1;
               if Controller_State.Tank_Temperature >= Sanitise_Temperature then
                  Over_Safe_Count := Over_Safe_Count + 1;
               else
                  Over_Safe_Count := 0;
               end if; -- Controller_State.Tank_Temperature >= ...
            else
               Over_Comfort_Count := 0;
               Over_Safe_Count := 0;
            end if; -- Controller_State.Tank_Temperature ...
            if Over_Comfort_Count >= Safe_Count then
               if not Controller_State.Is_Comfortable then
                  if not Boost_Is_On then
                     Put_Event ("Comfort temperature reached");
                  end if; -- not Boost_Is_On
                  Controller_State.Set_Is_Comfortable;
               end if; -- not Controller_State.Is_Comfortable
               if Boost_is_On and not Is_Sanitise then
                  Boost_is_On := not Request_Boost_Off;
                  Put_Event ("Request Boost off (comfort temperature reached)");
               end if; -- Boost_is_On and not Is_Sanitise
            end if; -- Over_Comfort_Count >= Safe_Count
            if Over_Safe_Count >= Safe_Count then
               -- wait for Safe_Count readings (minutes) before increasing next
               -- boost time
               Boost_Time.Next_Boost_Time :=
                 Next_Boost_Time (Clock) + Day_Count (Sanitise_Day);
               Boost_Time.Mandatory_Boost_Time := Boost_Time.Next_Boost_Time;
               Controller_State.Write_Next_Boost_Time (Boost_Time);
               -- This may occur many times but because effectively only the day
               -- can be incremented the stored value will typically only change
               -- once in a day
               if Boost_is_On then
                  Boost_is_On := not Request_Boost_Off;
                  Is_Sanitise := False;
                  Put_Event ("Request Boost off (safe temperature reached)");
               end if; -- Boost_is_On
            end if; -- Over_Safe_Count >= Safe_Count
            if Controller_State.Next_Boost /= Previous_Boost_Time then
               -- This should happen at most once per day or if a manual boost
               -- is requestes so it will not wear out the SD card
               Update_Next_Boost_Time; -- updates file
               if Boost_Time.Next_Boost_Time = Boost_Time.Mandatory_Boost_Time
               then
                  Put_Event ("Automatic Boost Time update " &
                               Image (Boost_Time.Next_Boost_Time, False,
                               UTC_Time_Offset));
               else
                  Put_Event ("Manual Boost Time Update " &
                               Image (Boost_Time.Next_Boost_Time, False,
                               UTC_Time_Offset));
               end if; -- Boost_Time.Next_Boost_Time = ...
               Previous_Boost_Time := Controller_State.Next_Boost;
            end if; -- Controller_State.Next_Boost /= Previous_Boost_Time
            if Hour (Boost_Time.Next_Boost_Time) = Hour (Clock) and
              Boost_Time.Next_Boost_Time <= Clock and not Boost_is_On then
               -- Initiate sanitie boost, terminated at Sanitise_Temperature
               Boost_is_On := Request_Boost_On;
               Is_Sanitise := True;
               Remaining_Minutes := Boost_Limit;
               if Boost_Time.Next_Boost_Time = Boost_Time.Mandatory_Boost_Time
               then
                  Put_Event ("Automatic safety Request Boost On");
               else
                  Put_Event ("Manual Request Boost On");
               end if; -- Boost_Time.Next_Boost_Time = ...
            elsif Hour (Boost_Time.Next_Boost_Time) = Hour (Clock) and
              not Controller_State.Is_Comfortable and not Boost_is_On then
               -- Initiate comfort, terminates at Comfort_Temperature
               Boost_is_On := Request_Boost_On;
               Remaining_Minutes := Boost_Limit;
               Put_Event ("Automatic comfort Request Boost On");
            elsif Boost_is_On then
               if Remaining_Minutes > 0 then
                  Remaining_Minutes := Remaining_Minutes - 1;
               else
                  Boost_is_On := not Request_Boost_Off;
                  Is_Sanitise := False;
                  Put_Event ("Request Boost off (time expired)");
                  raise Boost_Failed with "Boost time expired";
               end if; -- Remaining_Minutes > 0
            end if; --  Hour (Boost_Tine.Next_Boost_Time) = Hour (Clock) ...
            if Controller_State.Is_Comfortable and Previous_Hour < Comfort_Hour
              and Comfort_Hour <= Hour (Next_Time, Time_Zone) then
               -- Check that Comfort_Temperature is reached at least once per
               -- day. The test should become true even if there has been a
               -- significant jump forward in time.
               Controller_State.Clear_Is_Comfortable;
               Put_Event ("Reset to cold");
            end if; -- Controller_State.Is_Comfortable and Previous_Hour < ...
            Previous_Hour := Hour (Next_Time, Time_Zone);
         end select;
      end loop; -- Run_Boost
   exception
      when Event : others =>
         Controller_State.Set_Fault (Boost_Failure);
         Put_Error ("Boost Task", Event);
   end Boost_Task;

end Boost;
