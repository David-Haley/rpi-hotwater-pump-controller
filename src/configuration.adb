-- This package provides for reading of the the configuration File
-- "Configuration.txt" and provides functions that return the configuration
-- parameters that have been read in.
-- Author    : David Haley
-- Created   : 14/10/2017
-- Last Edit : 15/07/2022
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
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with DJH.Parse_CSV;

package body Configuration is

   Configuration_File_Name : constant string := "Configuration.csv";
   
   protected body User_Configuration is

      function Start_Difference return Start_Differences is
        (Start_Difference_P);

      function Stop_Difference return Stop_Differences is (Stop_Difference_P);

      function Maximum_Tank_Temperature return Maximum_Tank_Temperatures is
        (Maximum_Tank_Temperature_P);

      function Alarm_Temperature return Alarm_Temperatures is
        (Alarm_Temperature_P);

      function Minimum_Pump_Run_Time return Minimun_Pump_Run_Times is
        (Minimum_Pump_Run_Time_P);

      function Maximum_Hot_Delay return Maximum_Hot_Delays is
        (Maximum_Hot_Delay_P);

      function Tank_Slope return Controller_Reals is (Tank_Slope_P);

      function Tank_Offset return Controller_Reals is (Tank_Offset_P);

      function Panel_Slope return Controller_Reals is (Panel_Slope_P);

      function Panel_Offset return Controller_Reals is (Panel_Offset_P);

      function Sanitise_Temperature return Sanitise_Temperatures is
        (Sanitise_Temperature_P);

      function Sanitise_Day return Sanitise_Days is (Sanitise_Day_P);

      function Boost_Hour return Boost_Hours is (Boost_Hour_P);
      
      function Comfort_Temperature return Comfort_Temperatures is
        (Comfort_Temperature_P);

      function Comfort_Hour return Comfort_Hours is (Comfort_Hour_P);

      procedure Read_Configuration is

         type configuration_Items is
           (Start_Difference, Stop_Difference, Maximum_Tank_Temperature,
            Alarm_Temperature, Minimum_Pump_Run_Time, Maximum_Hot_Delay,
            Tank_Slope, Tank_Offset, Panel_Slope, Panel_Offset,
            Sanitise_Temperature, Sanitise_Day, Boost_Hour,
            Comfort_Temperature, Comfort_Hour);
            
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
            if Start_Difference_P <= Stop_Difference_P then
               raise Configuration_Error with
                 "Start difference must be greater than stop difference";
            end if; -- Start_Difference_P <= Stop_Difference_P
            if Alarm_Temperature_P <= Maximum_Tank_Temperature_P then
               raise Configuration_Error with
                 "Maximum temperature must be less than Alarm temperature";
            end if; -- Alarm_Temperature_P <= Maximum_Tank_Temperature_P
         end Read_Fields;

      begin -- Reload_Configuration
         begin -- read header
            Read_Header (Configuration_File_Name);
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
      end Read_Configuration;
      
   end User_Configuration;

begin -- Configuration
   User_Configuration.Read_Configuration;
end Configuration;
