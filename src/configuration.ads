-- This package provides for reading of the the configuration File
-- "Configuration.txt" and provides functions that return the configuration
-- parameters that have been read in.
-- Author    : David Haley
-- Created   : 14/10/2017
-- Last Edit : 15/07/2022
-- 20220715 : Sanities_Hour renamed to Boost_Hour, renames replaces extra layer
-- of function and procedure call.
-- 20220714 : Comfort_Hour added.
-- 20220520 : Comfort_Temperature added and Reload_Configuration added.
-- 20190404 : Sanitise functionality added
-- 20190306 : Maximum_Hot_Delay added as a configuration item, all private part
-- transferred to the body of the package because the configuration record is
-- not exported.

with Pump_Controller_Types; use Pump_Controller_Types;

package Configuration is
   
   protected User_Configuration is

      function Start_Difference return Start_Differences;

      function Stop_Difference return Stop_Differences;

      function Maximum_Tank_Temperature return Maximum_Tank_Temperatures;

      function Alarm_Temperature return Alarm_Temperatures;

      function Minimum_Pump_Run_Time return Minimun_Pump_Run_Times;

      function Maximum_Hot_Delay return Maximum_Hot_Delays;

      function Tank_Slope return Controller_Reals;

      function Tank_Offset return Controller_Reals;

      function Panel_Slope return Controller_Reals;

      function Panel_Offset return Controller_Reals;

      function Sanitise_Temperature return Sanitise_Temperatures;

      function Sanitise_Day return Sanitise_Days;

      function Boost_Hour return Boost_Hours;
      
      function Comfort_Temperature return Comfort_Temperatures;
      
      function Comfort_Hour return Comfort_Hours;
      
      procedure Read_Configuration;
   
   private
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
   end User_Configuration;

   function Start_Difference return Start_Differences renames
     User_Configuration.Start_Difference;

   function Stop_Difference return Stop_Differences renames
     User_Configuration.Stop_Difference;

   function Maximum_Tank_Temperature return Maximum_Tank_Temperatures renames
     User_Configuration.Maximum_Tank_Temperature;

   function Alarm_Temperature return Alarm_Temperatures renames
     User_Configuration.Alarm_Temperature;

   function Minimum_Pump_Run_Time return Minimun_Pump_Run_Times renames
     User_Configuration.Minimum_Pump_Run_Time;

   function Maximum_Hot_Delay return Maximum_Hot_Delays renames
     User_Configuration.Maximum_Hot_Delay;

   function Tank_Slope return Controller_Reals renames
     User_Configuration.Tank_Slope;

   function Tank_Offset return Controller_Reals renames
     User_Configuration.Tank_Offset;

   function Panel_Slope return Controller_Reals renames
     User_Configuration.Panel_Slope;

   function Panel_Offset return Controller_Reals renames
     User_Configuration.Panel_Offset;

   function Sanitise_Temperature return Sanitise_Temperatures renames
     User_Configuration.Sanitise_Temperature;

   function Sanitise_Day return Sanitise_Days renames
     User_Configuration.Sanitise_Day;

   function Boost_Hour return Boost_Hours renames
     User_Configuration.Boost_Hour;
   
   function Comfort_Temperature return Comfort_Temperatures renames
     User_Configuration.Comfort_Temperature;

   function Comfort_Hour return Comfort_Hours renames
     User_Configuration.Comfort_Hour;
   
   procedure Reload_Configuration renames
      User_Configuration.Read_Configuration;
   
   Configuration_Error : Exception;

end Configuration;
