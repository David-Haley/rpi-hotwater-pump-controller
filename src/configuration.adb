-- This package provides for reading of the the configuration File
-- "Configuration.txt" and provides functions that return the configuration
-- parameters that have been read in.
-- Author    : David Haley
-- Created   : 14/10/2017
-- Last Edit : 08/10/2025

-- 20251008 : Brightness setting (Backlight) added for LCD_Display.
-- 20250501 : Barrier added to prevent values being read before they are
-- defined, by being read from the configuration file.
-- 20220715 : Sanities_Hour renamed to Boost_Hour, renames replaces extra layer
-- of function and procedure call.
-- 20220714 : Comfort_Hour_P and related functions added.
-- 20220522 : Removal of redundant source text "User_Configuration." in
-- User_Configuration body functions and procedures.
-- 20220520 : Comfort_Temperature and Reload_Configuration added.
-- User_Configuration is protected to allow fir the reload functionality.
-- 20190407 : Sanitise functionality added, tidy up of raising exceptions
-- unnecessary context removed
-- 20190306 : Maximum_Hot_Delay added as a configuration item

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Exceptions; use Ada.Exceptions;
with DJH.Events_and_Errors; use DJH.Events_and_Errors;
with DJH.Parse_CSV;

package body Configuration is

   Configuration_File_Name : constant string := "Configuration.csv";
   
   protected User_Configuration is

      entry Start_Difference (Result : out Start_Differences);

      entry Stop_Difference (Result : out Stop_Differences);

      entry Maximum_Tank_Temperature (Result : out Maximum_Tank_Temperatures);

      entry Alarm_Temperature (Result : out Alarm_Temperatures);

      entry Minimum_Pump_Run_Time (Result : out Minimun_Pump_Run_Times);

      entry Maximum_Hot_Delay (Result : out Maximum_Hot_Delays);

      entry Tank_Slope (Result : out Controller_Reals);

      entry Tank_Offset (Result : out Controller_Reals);

      entry Panel_Slope (Result : out Controller_Reals);

      entry Panel_Offset (Result : out Controller_Reals);

      entry Sanitise_Temperature (Result : out Sanitise_Temperatures);

      entry Sanitise_Day (Result : out Sanitise_Days);

      entry Boost_Hour (Result : out Boost_Hours);
      
      entry Comfort_Temperature (Result : out Comfort_Temperatures);
      
      entry Comfort_Hour (Result : out Comfort_Hours);
      
      entry LCD_Brightness (Result : out Backlight_Brightness);
      
      procedure Read_Configuration;
   
   private
      Defined : Boolean := False;
      Start_Difference_P : Start_Differences;
      Stop_Difference_P : Stop_Differences;
      Maximum_Tank_Temperature_P : Maximum_Tank_Temperatures;
      Alarm_Temperature_P : Alarm_Temperatures;
      Minimum_Pump_Run_Time_P : Minimun_Pump_Run_Times;
      Maximum_Hot_Delay_P : Maximum_Hot_Delays;
      Tank_Slope_P, Tank_Offset_P,
        Panel_Slope_P, Panel_Offset_P : Controller_Reals;
      Sanitise_Temperature_P : Sanitise_Temperatures;
      Sanitise_Day_P : Sanitise_Days;
      Boost_Hour_P : Boost_Hours;
      Comfort_Temperature_P : Comfort_Temperatures;
      Comfort_Hour_P : Comfort_Hours;
      LCD_Brightness_P : Backlight_Brightness;
   end User_Configuration;
   
   protected body User_Configuration is

      entry Start_Difference (Result : out Start_Differences) when Defined is
      
      begin -- Start_Difference
         Result := Start_Difference_P;
      end Start_Difference;

      entry Stop_Difference (Result : out Stop_Differences) when Defined is
      
      begin -- Stop_Difference
         Result := Stop_Difference_P;
      end Stop_Difference;

      entry Maximum_Tank_Temperature (Result : out Maximum_Tank_Temperatures)
        when Defined is
        
      begin -- Maximum_Tank_Temperature
         Result := Maximum_Tank_Temperature_P;
      end Maximum_Tank_Temperature;

      entry Alarm_Temperature (Result : out Alarm_Temperatures) when Defined is
      
      begin -- Alarm_Temperature
         Result := Alarm_Temperature_P;
      end Alarm_Temperature;

      entry Minimum_Pump_Run_Time (Result : out Minimun_Pump_Run_Times)
        when Defined is
      
      begin -- Minimum_Pump_Run_Time
         Result := Minimum_Pump_Run_Time_P;
      end Minimum_Pump_Run_Time;

      entry Maximum_Hot_Delay (Result : out Maximum_Hot_Delays) when Defined is
      
      begin -- Maximum_Hot_Delay
         Result := Maximum_Hot_Delay_P;
      end Maximum_Hot_Delay;

      entry Tank_Slope (Result : out Controller_Reals) when Defined is
      
      begin -- Tank_Slope
         Result := Tank_Slope_P;
      end Tank_Slope;

      entry Tank_Offset (Result : out Controller_Reals) when Defined is
      
      begin -- Tank_Offset
         Result := Tank_Offset_P;
      end Tank_Offset;

      entry Panel_Slope (Result : out Controller_Reals) when Defined is
      
      begin --
         Result := Panel_Slope_P;
      end;

      entry Panel_Offset (Result : out Controller_Reals) when Defined is
      
      begin -- Panel_Offset
         Result := Panel_Offset_P;
      end Panel_Offset;

      entry Sanitise_Temperature (Result : out Sanitise_Temperatures)
        when Defined is
      
      begin -- Sanitise_Temperature
         Result := Sanitise_Temperature_P;
      end Sanitise_Temperature;

      entry Sanitise_Day (Result : out Sanitise_Days) when Defined is
      
      begin -- Sanitise_Day
         Result := Sanitise_Day_P;
      end Sanitise_Day;

      entry Boost_Hour (Result : out Boost_Hours) when Defined is
      
      begin -- Boost_Hour
         Result := Boost_Hour_P;
      end Boost_Hour;
      
      entry Comfort_Temperature (Result : out Comfort_Temperatures)
        when Defined is
      
      begin -- Comfort_Temperature
         Result := Comfort_Temperature_P;
      end Comfort_Temperature;

      entry Comfort_Hour (Result : out Comfort_Hours) when Defined is
      
      begin -- Comfort_Hour
         Result := Comfort_Hour_P;
      end Comfort_Hour;
      
      entry LCD_Brightness (Result : out Backlight_Brightness) when Defined is
      
      begin -- LCD_Brightness
         Result := LCD_Brightness_P;
      end LCD_Brightness;

      procedure Read_Configuration is

         type configuration_Items is
           (Start_Difference, Stop_Difference, Maximum_Tank_Temperature,
            Alarm_Temperature, Minimum_Pump_Run_Time, Maximum_Hot_Delay,
            Tank_Slope, Tank_Offset, Panel_Slope, Panel_Offset,
            Sanitise_Temperature, Sanitise_Day, Boost_Hour,
            Comfort_Temperature, Comfort_Hour, Backlight);
            
         package Parser is new DJH.Parse_CSV (Configuration_Items);
         use Parser;

         procedure Read_Fields is
         
            Last : Positive;
            -- last character of number representstion, not used

         begin -- Read_Fields
            begin -- Read Start_Difference
               Temperature_IO.Get (Get_Value (Start_Difference),
                                   Start_Difference_P, Last);
            exception
               when E : others =>
                  raise Configuration_Error with "Start Difference - " &
                  Exception_Message (E);
            end; -- Read Start_Difference
            begin -- Read Stop_Difference
               Temperature_IO.Get (Get_Value (Stop_Difference),
                                   Stop_Difference_P, Last);
            exception
               when E : others =>
                  raise Configuration_Error with "Stop Difference - " &
                  Exception_Message (E);
            end; -- Read Stop_Difference
            begin -- Maximum_Tank_Temperature
               Temperature_IO.Get
               (Get_Value (Maximum_Tank_Temperature),
                           Maximum_Tank_Temperature_P, Last);
            exception
               when E : others =>
                  raise Configuration_Error with "Maximum_Tank_Temperature - " &
                  Exception_Message (E);
            end; -- Read Maximum_Tank_Temperature
            begin -- Read Alarm_Temperature
               Temperature_IO.Get (Get_Value (Alarm_Temperature),
                                   Alarm_Temperature_P, Last);
            exception
               when E : others =>
                  raise Configuration_Error with "Alarm_Temperature - " &
                  Exception_Message (E);
            end; -- Read Alarm_Temperature
            begin -- Read Minimum_Pump_Run_Time
               Min_Pump_Run_IO.Get (Get_Value (Minimum_Pump_Run_Time),
                                    Minimum_Pump_Run_Time_P, Last);
            exception
               when E : others =>
                  raise Configuration_Error with "Minimum Pump Run Time - " &
                  Exception_Message (E);
            end; -- Read Minimum_Pump_Run_Time
            begin -- Read Maximum_Hot_Delay
               Max_Hot_Delay_IO.Get (Get_Value (Maximum_Hot_Delay),
                                     Maximum_Hot_Delay_P, Last);
            exception
               when E : others =>
                  raise Configuration_Error with "Maximum Hot Delay - " &
                  Exception_Message (E);
            end; -- Read Maximum_Hot_Delay
            begin -- Read Tank_Slope
               Controller_Real_IO.Get (Get_Value (Tank_Slope),
                                       Tank_Slope_P, Last);
            exception
               when E : others =>
                  raise Configuration_Error with "Tank Slope - " &
                  Exception_Message (E);
            end; -- Read Tank_Slope
            begin -- Read Tank_Offset
               Controller_Real_IO.Get (Get_Value (Tank_Offset),
                                       Tank_Offset_P, Last);
            exception
               when E : others =>
                  raise Configuration_Error with "Tank Offset - " &
                  Exception_Message (E);
            end; -- Read Tank_Offset
            begin -- Read Panel_Slope
               Controller_Real_IO.Get (Get_Value (Panel_Slope),
                                       Panel_Slope_P, Last);
            exception
               when E : others =>
                  raise Configuration_Error with "Panel Slope - " &
                  Exception_Message (E);
            end; -- Panel_Slope
            begin -- Read Panel_Offset
               Controller_Real_IO.Get (Get_Value (Panel_Offset),
                                       Panel_Offset_P, Last);
            exception
               when E : others =>
                  raise Configuration_Error with "Panel Offset - " &
                  Exception_Message (E);
            end; -- Panel_Offset
            begin -- Sanitise_Temperature
               Temperature_IO.Get (Get_Value (Sanitise_Temperature),
                                   Sanitise_Temperature_P, Last);
            exception
               when E : others =>
                  raise Configuration_Error with "Sanitise Temperature - " &
                  Exception_Message (E);
            end; -- Sanitise_Temperature
            begin -- Sanitise_Day
               Sanitise_Day_IO.Get (Get_Value (Sanitise_Day),
                                    Sanitise_Day_P, Last);
            exception
               when E : others =>
                  raise Configuration_Error with "Sanitise Day - " &
                  Exception_Message (E);
            end; -- Sanitise_Day
            begin -- Boost_Hour
               Boost_Hour_IO.Get (Get_Value (Boost_Hour),
                                     Boost_Hour_P, Last);
            exception
               when E : others =>
                  raise Configuration_Error with "Boost_Hour - " &
                  Exception_Message (E);
            end; -- Boost_Hour
            begin -- Comfort_Temperature
               Temperature_IO.Get (Get_Value (Comfort_Temperature),
                                   Comfort_Temperature_P, Last);
            exception
               when E : others =>
                  raise Configuration_Error with "Comfort Temperature - " &
                  Exception_Message (E);
            end; -- Comfort_Temperature
            begin -- Comfort_Hour
               Comfort_Hour_IO.Get (Get_Value (Comfort_Hour),
                                     Comfort_Hour_P, Last);
            exception
               when E : others =>
                  raise Configuration_Error with "Comfort_Hour - " &
                  Exception_Message (E);
            end; -- Comfort_Hour
            begin -- LCD_Brightness
               LCD_Brightness_P :=
                  Backlight_Brightness'Value (Get_Value (Backlight));
            exception
               when E : others =>
                  raise Configuration_Error with "LCD_Brightness - " &
                  Exception_Message (E);
            end;-- LCD_Brightness
            if Start_Difference_P <= Stop_Difference_P then
               raise Configuration_Error with
                 "Start difference must be greater than stop difference";
            end if; -- Start_Difference_P <= Stop_Difference_P
            if Alarm_Temperature_P <= Maximum_Tank_Temperature_P then
               raise Configuration_Error with
                 "Maximum temperature must be less than Alarm temperature";
            end if; -- Alarm_Temperature_P <= Maximum_Tank_Temperature_P
         end Read_Fields;

      begin -- Read_Configuration
         begin -- read header
            if Exists (Configuration_File_Name) then
               Read_Header (Configuration_File_Name);
               Put_Event ("Read " & Configuration_File_Name & ' ' & 
               Local_Image (Modification_Time (Configuration_File_Name)));
            else
               raise Configuration_Error with Configuration_File_Name &
                 "not found";
            end if; -- Exists (Configuration_File_Name)
         exception
            when E : others =>
               raise Configuration_Error with
                 "Configuration file " & Configuration_File_Name & " - " &
                 Exception_Message (E);
         end; -- read header
         if Next_Row then
            Read_Fields;
         else
            raise Configuration_Error with
              "Configuration file " & Configuration_File_Name & " no data row";
         end if; -- Next_Row
         Close_CSV;
         Defined := True;
      end Read_Configuration;
      
   end User_Configuration;

   function Start_Difference return Start_Differences is
   
      Result : Start_Differences;
      
   begin -- Start_Difference
      User_Configuration.Start_Difference (Result);
      return Result;
   end Start_Difference;

   function Stop_Difference return Stop_Differences is
   
      Result : Stop_Differences;
      
   begin -- Stop_Difference
      User_Configuration.Stop_Difference (Result);
      return Result;
   end Stop_Difference; 

   function Maximum_Tank_Temperature return Maximum_Tank_Temperatures is
   
      Result : Maximum_Tank_Temperatures;
      
   begin -- Maximum_Tank_Temperature
      User_Configuration.Maximum_Tank_Temperature (Result);
      return Result;
   end Maximum_Tank_Temperature;
     
   function Alarm_Temperature return Alarm_Temperatures is
   
      Result : Alarm_Temperatures;
      
   begin -- Alarm_Temperature
      User_Configuration.Alarm_Temperature (Result);
      return Result;
   end Alarm_Temperature;

   function Minimum_Pump_Run_Time return Minimun_Pump_Run_Times is
   
      Result : Minimun_Pump_Run_Times;
      
   begin -- Minimum_Pump_Run_Time
      User_Configuration.Minimum_Pump_Run_Time (Result);
      return Result;
   end Minimum_Pump_Run_Time;

   function Maximum_Hot_Delay return Maximum_Hot_Delays is
   
      Result : Maximum_Hot_Delays;
      
   begin -- Maximum_Hot_Delay
      User_Configuration.Maximum_Hot_Delay (Result);
      return Result;
   end Maximum_Hot_Delay;

   function Tank_Slope return Controller_Reals is
   
      Result : Controller_Reals;
      
   begin -- Tank_Slope
      User_Configuration.Tank_Slope (Result);
      return Result;
   end Tank_Slope;

   function Tank_Offset return Controller_Reals is
   
      Result : Controller_Reals;
      
   begin -- Tank_Offset
      User_Configuration.Tank_Offset (Result);
      return Result;
   end Tank_Offset;

   function Panel_Slope return Controller_Reals is
   
      Result : Controller_Reals;
      
   begin -- Panel_Slope
      User_Configuration.Panel_Slope (Result);
      return Result;
   end Panel_Slope;

   function Panel_Offset return Controller_Reals is
   
      Result : Controller_Reals;
      
   begin -- Panel_Offset
      User_Configuration.Panel_Offset (Result);
      return Result;
   end Panel_Offset;

   function Sanitise_Temperature return Sanitise_Temperatures is
   
      Result : Sanitise_Temperatures;
      
   begin -- Sanitise_Temperature
      User_Configuration.Sanitise_Temperature (Result);
      return Result;
   end Sanitise_Temperature;

   function Sanitise_Day return Sanitise_Days is
   
      Result : Sanitise_Days;
      
   begin -- Sanitise_Day
      User_Configuration.Sanitise_Day (Result);
      return Result;
   end Sanitise_Day;

   function Boost_Hour return Boost_Hours is
   
      Result : Boost_Hours;
      
   begin -- Boost_Hour
      User_Configuration.Boost_Hour (Result);
      return Result;
   end Boost_Hour;
   
   function Comfort_Temperature return Comfort_Temperatures is
   
      Result : Comfort_Temperatures;
      
   begin -- Comfort_Temperature
      User_Configuration.Comfort_Temperature (Result);
      return Result;
   end Comfort_Temperature;
   
   function Comfort_Hour return Comfort_Hours is
   
      Result : Comfort_Hours;
      
   begin -- Comfort_Hour
      User_Configuration.Comfort_Hour (Result);
      return Result;
   end Comfort_Hour;
   
   function LCD_Brightness return Backlight_Brightness is

      Result : Backlight_Brightness;

   begin -- LCD_Brightness
      User_Configuration.LCD_Brightness (Result);
      return Result;
   end LCD_Brightness;

begin -- Configuration
   User_Configuration.Read_Configuration;
end Configuration;
