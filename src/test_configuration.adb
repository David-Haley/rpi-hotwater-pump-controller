-- This is a test program for configuration package
-- Author    : David Haley
-- Created   : 14/10/2017
-- Last Edit : 08/10/2025

-- 20251008 : LCD_Brightness added.
-- 20220715 : Sanitise_Hour renamed to Boost_Hour
-- 20220714 : Comfort_Hour added.
-- 20220520 : Comfort_Temperature added.
-- 20190404 : Sanitise ... configuration elements added
-- 20190306 : Configuration element Maximum_Hot_Delay added

with Ada.Text_IO; use Ada.Text_IO;
with Pump_Controller_Types; use Pump_Controller_Types;
with Configuration; use Configuration;

procedure Test_Configuration is

begin -- Test_Configuration
   Put_Line ("Test Configuration version 20251008");
   Put ("Start:");
   Temperature_IO.Put (Start_Difference, 3, 2, 0);
   New_Line;
   Put ("Stop:");
   Temperature_IO.Put (Stop_Difference, 3, 2, 0);
   New_Line;
   Put ("Maximum:");
   Temperature_IO.Put (Maximum_Tank_Temperature, 3, 2, 0);
   New_Line;
   Put ("Alarm:");
   Temperature_IO.Put (Alarm_Temperature, 3, 2, 0);
   New_Line;
   Put ("Pump Time:");
   Min_Pump_Run_IO.Put (Minimum_Pump_Run_Time, 4);
   New_Line;
   Put ("Hot Delay:");
   Max_Hot_Delay_IO.Put (Maximum_Hot_Delay, 5);
   New_Line;
   Put ("Tank m:");
   Controller_Real_IO.Put (Tank_Slope, 3, 4, 3);
   New_Line;
   Put ("Tank c:");
   Controller_Real_IO.Put (Tank_Offset, 3, 4, 3);
   New_Line;
   Put ("Panel m:");
   Controller_Real_IO.Put (Panel_Slope, 3, 4, 3);
   New_Line;
   Put ("Panel c:");
   Controller_Real_IO.Put (Panel_Offset, 3, 4, 3);
   New_Line;
   Put ("Sanitise Temperature:");
   Temperature_IO.Put (Sanitise_Temperature, 3, 2, 0);
   New_Line;
   Put ("Sanitise Days:");
   Sanitise_Day_IO.Put (Sanitise_Day, 3);
   New_Line;
   Put ("Boost Hour:");
   Boost_Hour_IO.Put (Boost_Hour, 3);
   New_Line;
   Put ("Comfort Temperature:");
   Temperature_IO.Put (Comfort_Temperature, 3, 2, 0);
   New_Line;
   Put ("Comfort Hour:");
   Comfort_Hour_IO.Put (Comfort_Hour, 3);
   New_Line;
   Put_Line ("LCD Brightness:" & LCD_Brightness'Img);
   Put_Line ("Test Complete");
end Test_Configuration;
