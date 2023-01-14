-- This package defined the common data types used within the Pump Controller
-- packages.
-- Author    : David Haley
-- Created   : 14/10/2017
-- Last Edit : 15/07/2022
-- 20220715Sanitise_Hours renamed to Boost_Hours and IO etc.
-- 20220714 : Comfort_Hours and Comfort_Hour_IO added.
-- 20220520 : Comfort_Temperature added, some un_used IO packages removed;
-- 20210417 : Hot_Areas deleted, Temperature_Differences used instead.
-- 20210330 : Temperature_Differences and Temperature_Difference_IO added.
-- Hot_Areas and Hot_Area_IO added.
-- 20190406 : subtypes Sanatise_Temperatures, Sanatise_Days and Sanatise_Hours
-- Boost_Failure added to Fault_Types
-- added along with associated instantion of Text_IO
-- 20190306 : subtype Maximum_Hot_Delays added
-- 20190124 : Version_String added
-- 03/11/2017 Controller_States removed to Global_Data.
-- 24/10/2017 Configurations removed to Read Configuration

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Arithmetic; use Ada.Calendar.Arithmetic;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;

package Pump_Controller_Types is

   type Controller_Reals is digits 15; -- IEEE and maximum for RPi
   subtype Temperatures is Controller_Reals range -10.0 .. 110.0;
   -- should be sufficient to cover the full A/D range
   subtype Temperature_Differences is Controller_Reals range
     Temperatures'First - Temperatures'Last ..
       Temperatures'Last - Temperatures'First;
   subtype Start_Differences is Temperatures range 3.0 .. 15.0;
   subtype Stop_Differences is Temperatures range 1.0 .. 10.0;
   subtype Maximum_Tank_Temperatures is Temperatures range 60.0 .. 90.0;
   subtype Alarm_Temperatures is Temperatures range 65.0 .. 95.0;
   subtype Sanitise_Temperatures is Temperatures range 55.0 .. 70.0;
   subtype Comfort_Temperatures is Temperatures range 35.0 .. 55.0;
   subtype Sanitise_Days is Day_Count range 0 .. 7;
   subtype Boost_Hours is Hour_Number
     with Static_Predicate => Boost_Hours in 0 .. 6 | 18 .. 23;
   subtype Comfort_Hours is Hour_Number
     with Static_Predicate => Comfort_Hours in 0 .. 6;
   subtype Maximum_Hot_Delays is Natural range 0 .. 1800;
   subtype Minimun_Pump_Run_Times is Natural range 60 .. 300;
   subtype Day_Seconds is Natural range 0 .. (23 * 60 + 59) * 60 + 59;
   subtype Accumulated_Times is Natural;
   subtype Version_String is String (1 .. 8); -- YYYYMMDD

   type Fault_Types is (Accumulated_Time_File, Log_File, Tank_Temperature,
                       Boost_Failure);
   type Fault_Tables is array (Fault_Types) of Boolean;

   type Boost_Times is record
      Next_Boost_Time, Mandatory_Boost_Time : Time :=
        Ada.Calendar.Formatting.Time_Of (2019, 04, 04, 0.0);
      -- any valid date will do preferably in the past
   end record; -- Boost_Times

   package Temperature_IO is new Ada.Text_IO.Float_IO (Temperatures);
   package Temperature_Difference_IO is new
     Ada.Text_IO.Float_IO (Temperature_Differences);
   package Sanitise_Day_IO is new Integer_IO (Sanitise_Days);
   package Boost_Hour_IO is new Integer_IO (Boost_Hours);
   package Comfort_Hour_IO is new Integer_IO (Comfort_Hours);
   package Min_Pump_Run_IO is new
     Ada.Text_IO.Integer_IO (Minimun_Pump_Run_Times);
   package Max_Hot_Delay_IO is new
     Ada.Text_IO.Integer_IO (Maximum_Hot_Delays);
   package Controller_Real_IO is new Ada.Text_IO.Float_IO (Controller_Reals);
   package Pump_Run_Time_IO is new Ada.Text_IO.Integer_IO (Day_Seconds);
   package Accumulated_IO is new Ada.Text_IO.Integer_IO (Accumulated_Times);
   package Fault_IO is new Ada.Text_IO.Enumeration_IO (Fault_Types);
   package Boolean_IO is new Ada.Text_IO.Enumeration_IO (Boolean);

end Pump_Controller_Types;
