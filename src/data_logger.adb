-- This package provides data logging for the Pump controller
-- Author    : David Haley
-- Created   : 14/10/2017
-- Last Edit : 11/05/2023
-- 20230511 : Log files compacted by removal of leading spaces.
-- 20220820 :  Events_and_Errors move to DJH.Events_and_Errors.
-- 20220715 : Indirect calls to Logger.Start and Stop_Logger converted to
-- renames ans -SV removed frim Controller_State entries;
-- 20210722 : Correction to Read_Accumulated_Hours
-- 20210303 : Unified version of date and time strings used.
-- 20210220 : Event and error management removed to Events_and_Errors.
-- Start_Logger added.
-- 20200109 : Data log format changed to include pump run seconds instead of
-- '0' or '1' for the pump running or not running.
-- 20190813 : Corrections to Logger task body. Previous_Time initialised to be
-- Logging_Interval before Next_Time. Update of Previous_Time is now immediately
-- before the update of Next_Time.
-- 20190731 : Previous_Time added to Logger to ensure that a change of day is
-- detected if Next_Time is subject to a big step forward in time. Calculation
-- of Next_Time changed to prevent multiple log entries being generated for
-- effectively the same real time if there is a substantial change in Clock, due
-- to a restart or NTP correction.
-- 20190405 : File Commit made on exact hour
-- 20190307 : Export of Put_Error and Put_Event, redirection of standard error
-- and standard output managed here.
-- 20190226 : Absence of accumilated time files logged in error file.
-- 20190216 : Read_File_Commit_Time added, this necesittated the creation of the
-- protected type File_Commit_Times.
-- 20171108 Flushing of Standard_Output and Standard_Error added to file
-- commit sequence;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Directories; use Ada.Directories;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with DJH.Date_and_Time_Strings; use DJH.Date_and_Time_Strings;
with DJH.Events_and_Errors; use DJH.Events_and_Errors;
with Pump_Controller_Types; use Pump_Controller_Types;
with Global_Data; use Global_Data;

package body Data_Logger is

   Accumulation_File_Name : String := "Total_Seconds.";
   Current_Extension : constant String := "txt";
   Backup_Extension : constant String := "bak";
   Temporary_Extension : constant String := "tmp";

   Logging_File : File_Type;
   Log_Interval : constant Day_Duration := 60.0;
   -- create a log entry at 1 minute inerevals
   File_Commit_Interval : constant Duration := 3600.0;
   -- Commit logging files once per hour

   function On_The_Hour (T : in Time) return Time is
      -- Effectively rounds T down such that minutes and seconds are zero

      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      This_Hour : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number;
      Sub_Second : Second_Duration;
      Leap_Second : Boolean;

   begin -- On_The_Hour
      Split (T, Year, Month, Day,
             This_Hour, Minute, Second, Sub_Second,
             Leap_Second, UTC_Time_Offset (T));
      return Time_Of (Year, Month, Day,
                      This_Hour, 0, 0, 0.0,
                      Leap_Second, UTC_Time_Offset (T));
   end On_The_Hour;

   protected type File_Commit_Times is

      procedure Set_Next_File_Commit;
      -- Sets time for next file commit

      function Get_Next_File_Commit return Time;
      -- Gets time of next file commit

   private
      File_Commit_Time : Time := On_The_Hour (Clock) + File_Commit_Interval;
      -- Once per hour on the hour, first commit could beless than one hour
      -- after start
   end File_Commit_Times;

   protected body File_Commit_Times is

      procedure Set_Next_File_Commit is
         -- Sets time for next file commit

      begin -- Set_Next_File_Commit
         File_Commit_Time := File_Commit_Time +File_Commit_Interval;
      end Set_Next_File_Commit;

      function Get_Next_File_Commit return Time is
         -- Gets time of next file commit

      begin -- Get_Next_File_Commit
         return File_Commit_Time;
      end Get_Next_File_Commit;

   end File_Commit_Times;

   Logging_File_Commit_Time : File_Commit_Times;

   function Read_File_Commit_Time return Time is
      -- Returns time of next file commit, that is, Flush (xx)

   begin -- Read_File_Commit_Time
      return Logging_File_Commit_Time.Get_Next_File_Commit;
   end Read_File_Commit_Time;

   function Read_Accumulated_Hours return Accumulated_Times is
      -- This function reads the existing accumulated time file if it exists. If
      -- the file does not exist or contains a bad value it attempts to read the
      -- backup file. If the backup cannot be read or contains a bad value then
      -- new files are created containing 0 and the function returns 0; If
      -- either the current file or the backup is read successfully the result
      -- of the read is returned.

      Accumulation_File : File_Type;
      Result : Accumulated_Times := 0;

   begin -- Read_Accumulated_Hours
      begin -- read current file
         Open (Accumulation_File, In_file,
               Accumulation_File_Name & Current_Extension);
         Accumulated_IO.Get (Accumulation_File, Result);
         Close (Accumulation_File);
         return Result;
      exception
         when Event: others =>
            -- do nothing and drop through to read backup file
            Put_Error ("Reading accumulated time primary", Event);
            if Is_Open (Accumulation_File) then
               Close (Accumulation_File);
            end if; -- Is_Open (Accumulation_File)
      end; -- read current file
      begin -- read backup file
         Open (Accumulation_File, In_file,
               Accumulation_File_Name & Backup_Extension);
         Accumulated_IO.Get (Accumulation_File, Result);
         Close (Accumulation_File);
         -- Create a matching current file, this allows the update process to
         -- proceed normally
         Create (Accumulation_File, Out_File,
                 Accumulation_File_Name & Current_Extension);
         Accumulated_IO.Put (Accumulation_File, Result);
         Close (Accumulation_File);
         return Result;
      exception
         when Event : others =>
            -- do nothing and drop through to create new files
            Put_Error ("Reading accumulated time backup", Event);
            if Is_Open (Accumulation_File) then
               Close (Accumulation_File);
            end if; -- Is_Open (Accumulation_File)
      end; -- read backup file
      begin -- Creat new files
         Put_Event ("Accumulated time files not found, " &
                     "accumulated time set to 0");
         Create (Accumulation_File, Out_File,
                 Accumulation_File_Name & Backup_Extension);
         Accumulated_IO.Put (Accumulation_File, Result);
         Close (Accumulation_File);
         Create (Accumulation_File, Out_File,
                 Accumulation_File_Name & Current_Extension);
         Accumulated_IO.Put (Accumulation_File, Result);
         Close (Accumulation_File);
      exception
         when Event : others =>
            Controller_State.Set_Fault (Accumulated_Time_File);
            Put_Error ("Creating accumulated time files", Event);
            if Is_Open (Accumulation_File) then
               Close (Accumulation_File);
            end if; -- Is_Open (Accumulation_File)
      end; -- Create new files
      return Result;
      -- if execution reaches this code the initial value of result will be
      -- returned. This will be the case even if an exception is raised when
      -- attempting to create new files;
   end Read_Accumulated_Hours;

   procedure Update_Accumulated_Time is
      -- This procedure updated the file accumulated time stored in the filing
      -- system. It attempts to maintain two copies of the file, that is, a
      -- current file and a backup file. When a new value is to be written a
      -- a temporary file is creared, the old backup deleted then a sequence of
      -- renames to maintain the current and backup file names.

      Accumulation_File : File_Type;

   begin -- Update_Accumulated_Time
      Create (Accumulation_File, Out_File,
              Accumulation_File_Name & Temporary_Extension);
      Accumulated_IO.Put (Accumulation_File,
                          Controller_State.Accumulated_Pump_Run_Time);
      Close (Accumulation_File);
      if Exists (Accumulation_File_Name & Backup_Extension) and then
        Exists (Accumulation_File_Name & Current_Extension) then
         -- Only delete backup if there is also a current file
         Delete_File (Accumulation_File_Name & Backup_Extension);
         Rename (Accumulation_File_Name & Current_Extension,
                 Accumulation_File_Name & Backup_Extension);
      elsif Exists (Accumulation_File_Name & Current_Extension) then
         -- There is no backup proceed to rename current file
         Rename (Accumulation_File_Name & Current_Extension,
                 Accumulation_File_Name & Backup_Extension);
      end if; -- tests on current and backup file existance
      Rename (Accumulation_File_Name & Temporary_Extension,
              Accumulation_File_Name & Current_Extension);
   exception
      when Event : others =>
         Controller_State.Set_Fault (Accumulated_Time_File);
         Put_Error ("Updating accumulated time", Event);
   end Update_Accumulated_Time;

   function Logging_Path (This_Time : Time) return string is
      -- returns "YYYY" representing the current year

   begin -- Logging_Path
      return Reverse_Date_String (This_Time) (1..4);
   end Logging_Path;

   function Logging_File_Name (This_Time : Time) return string is
      -- returns "YYYYMMDD.csv" where YYYYMMDD represents the current date

   begin -- Logging_File_Name
      return '/' & Reverse_Date_String (This_Time) & ".csv";
   end Logging_File_Name;

   procedure Open_Log_File (Logging_File : in out File_Type;
                            This_Time : in Time) is

   begin -- Open_Log_File
      if not (Exists (Logging_Path (This_Time))) then
         Create_Directory (Logging_Path (This_Time));
         -- exceptions may be raised later if the name exists and is a file not
         -- a directory
      end if; -- not (Exists (Logging_Path (This_Time)))
      if Exists (Logging_Path (This_Time) & Logging_File_Name (This_Time)) then
         Open (Logging_File, Append_File, Logging_Path  (This_Time) &
                 Logging_File_Name (This_Time));
      else
         Create (Logging_File, Out_File, Logging_Path (This_Time) &
                   Logging_File_Name (This_Time));
         Put_Line (Logging_File, "Time, Tank Temperature, Panel Temperature, " &
                     "Pump Output"); -- write header in logging file
      end if;
      -- Exists (Logging_Path (This_Time) & Logging_File_Name (This_Time))
   end Open_Log_File;

   function Is_Next_Day (Old_Time, New_Time : Time) return Boolean is

      Year : Year_Number;
      Month  : Month_Number;
      Day, Next_Day : Day_Number;
      Seconds : Day_Duration;

   begin -- Is_Next_Day
      Split (Old_Time, Year, Month, Day, Seconds);
      Split (New_Time, Year, Month, Next_Day, Seconds);
      return Next_Day /= Day;
   end Is_Next_Day;

   function Calculate_Next_Log_Entry return Time is

      Year : Year_Number;
      Month  : Month_Number;
      Day : Day_Number;
      Seconds : Day_Duration;
      This_Time : Time;

   begin -- Calculate_Next_Log_Entry
      This_Time := Clock;
      if Is_Next_Day (This_Time, This_Time + Log_Interval) then
         Split (This_Time + Log_Interval, Year, Month, Day, Seconds);
         return Ada.Calendar.Time_Of (Year, Month, Day, 0.0);
      else
         Split (This_Time, Year, Month, Day, Seconds);
         Seconds := Duration (Integer (Seconds / Log_Interval)) * Log_Interval
           + Log_Interval;
         return Ada.Calendar.Time_Of (Year, Month, Day, Seconds);
      end if; -- Next_Day (This_Time, This_Time + Log_Interval)
   end Calculate_Next_Log_Entry;

   Procedure Start_Logging (Logging_File : in out File_Type;
                            Next_Log_Entry : out Time) is

   begin -- Start_Logging
      Next_Log_Entry := Calculate_Next_Log_Entry;
      Open_Log_File (Logging_File, Next_Log_Entry);
   exception
      when Event : others =>
         Controller_State.Set_Fault (Log_File);
         Put_Error ("Start logging", Event);
   end Start_Logging;

   Procedure Put_Log_Entry (Logging_File : in out File_Type;
                            Logger_Pump_Time : in out Accumulated_Times) is

      Current_Run_Time : Accumulated_Times renames
        Controller_State.Accumulated_Pump_Run_Time;
        Output_Buffer : String (1 .. 5);

   begin -- Put_Log_Entry
      Put (Logging_File, Time_String & ',');
      Temperature_IO.Put (Output_Buffer,
                          Controller_State.Tank_Temperature, 1, 0);
      Put (Logging_File, Trim (Output_Buffer, Both));
      Put (Logging_File, ',');
      Temperature_IO.Put (Output_Buffer,
                          Controller_State.Panel_Temperature, 1, 0);
      Put (Logging_File, Trim (Output_Buffer, Both));
      Put (Logging_File, ',');
      Accumulated_IO.Put (Output_Buffer, Current_Run_Time - Logger_Pump_Time);
      -- Write running time of pump in seconds that has occured between log
      -- entries. If the pump has run for the whole time since the log entry,
      -- this may be 60 plus or minus a second dependent on task timing.
      Put_Line (Logging_File, Trim (Output_Buffer, Both));
      Logger_Pump_Time := Current_Run_Time;
   exception
      when Event : others =>
         Controller_State.Set_Fault (Log_File);
         Put_Error ("Log file entry", Event);
   end Put_Log_Entry;

   task body Logger is
   
      Run_Logger : Boolean;
      Next_Time, Previous_Time : Time;
      Previous_Pump_Time, Logger_Pump_Time : Accumulated_Times;

   begin -- Logger
      accept Start do
         -- Task inialisation delayed until package level initialisation is
         -- complete.
         Run_Logger := True;
         Controller_State.Write_Accumulated_Time (Read_Accumulated_Hours);
         Previous_Pump_Time := Controller_State.Accumulated_Pump_Run_Time;
         Logger_Pump_Time := Controller_State.Accumulated_Pump_Run_Time;
         Start_Logging (Logging_File, Next_Time);
         Previous_Time := Next_Time - Log_Interval;
      end; -- Start
      While Run_Logger loop
         select
            accept Stop do
               Run_Logger := False;
            end; -- Stop
         or 
            delay until Next_Time;
            if Clock - Next_Time > Log_Interval then
               -- a big step forward in time has occurred
               Next_Time := Calculate_Next_Log_Entry;
            end if; -- Next_Time - Clock > Log_Interval
            if Is_Next_Day (Previous_Time, Next_Time) then
               -- Day has changed hence a new logging file is required
               Close (Logging_File);
               Previous_Time := Next_Time;
               -- N.B. Start_Logging updates Next_Time!
               Start_Logging (Logging_File, Next_Time);
            else
               Previous_Time := Next_Time;
               Next_Time := Next_Time + Log_Interval;
            end if; -- Is_Next_Day (Previous_Time, Next_Time)
            Put_Log_Entry (Logging_File, Logger_Pump_Time);
            if Clock >= Logging_File_Commit_Time.Get_Next_File_Commit then
               Flush (Logging_File);
               if Controller_State.Accumulated_Pump_Run_Time >
                 Previous_Pump_Time then
                  Update_Accumulated_Time;
                  Previous_Pump_Time :=
                    Controller_State.Accumulated_Pump_Run_Time;
               end if; -- Only update if the pump has run
               Logging_File_Commit_Time.Set_Next_File_Commit;
            end if; -- Clock >= File_Commit_Time
         end select;
      end loop; -- Run_Logger
      Close (Logging_File);
      if Controller_State.Accumulated_Pump_Run_Time >
        Previous_Pump_Time then
         Update_Accumulated_Time;
      end if; -- Only update if the pump has run
   end Logger;

end Data_Logger;
