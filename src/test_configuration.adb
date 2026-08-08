-- This is a test program for configuration package
-- Author    : David Haley
-- Created   : 14/10/2017
-- Last Edit : 08/08/2026

--  20260808 : Compiler style warning removed.
--  20260807 : Full precision of slope and ofsett displayed, some descriptions
--  improved.
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
   Put_Line ("Test Configuration version 20260807");
   Put ("Start Temperature:");
   Temperature_IO.Put (Start_Difference, 3, 2, 0);
   New_Line;
   Put ("Stop Temperature:");
   Temperature_IO.Put (Stop_Difference, 3, 2, 0);
   New_Line;
   Put ("Maximum Tank Temperature:");
   Temperature_IO.Put (Maximum_Tank_Temperature, 3, 2, 0);
   New_Line;
   Put ("Alarm Temperature:");
   Temperature_IO.Put (Alarm_Temperature, 3, 2, 0);
   New_Line;
   Put_Line ("Minimum Pump Run Time:" & Minimum_Pump_Run_Time'Img);
   Put_Line ("Maximum Hot Delay:" & Maximum_Hot_Delay'Img);
   Put_Line ("Tank Slope:" & Tank_Slope'Img);
   Put_Line ("Tank Offset:" & Tank_Offset'Img);
   Put_Line ("Panel Slope:" & Panel_Slope'Img);
   Put_Line ("Panel Offset:" & Panel_Offset'Img);
   Put ("Sanitise Temperature:");
   Temperature_IO.Put (Sanitise_Temperature, 3, 2, 0);
   New_Line;
   Put_Line ("Sanitise Day:" & Sanitise_Day'Img);
   Put_Line ("Boost Hour:" & Boost_Hour'Img);
   Put ("Comfort Temperature:");
   Temperature_IO.Put (Comfort_Temperature, 3, 2, 0);
   New_Line;
   Put_Line ("Comfort Hour:" & Comfort_Hour'Img);
   Put_Line ("LCD Brightness:" & LCD_Brightness'Img);
   Put_Line ("Test Complete");
end Test_Configuration;
