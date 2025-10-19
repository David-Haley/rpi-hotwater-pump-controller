-- This package provides for reading of the the configuration File
-- "Configuration.txt" and provides functions that return the configuration
-- parameters that have been read in.
-- Author    : David Haley
-- Created   : 14/10/2017
-- Last Edit : 08/10/2025

-- 20251008 : Brightness setting added for LCD_Display.
-- 20250501 : Barrier added to prevent values being read before they are
-- defined, by being read from the configuration file.
-- 20220715 : Sanities_Hour renamed to Boost_Hour, renames replaces extra layer
-- of function and procedure call.
-- 20220714 : Comfort_Hour added.
-- 20220520 : Comfort_Temperature added and Reload_Configuration added.
-- 20190404 : Sanitise functionality added
-- 20190306 : Maximum_Hot_Delay added as a configuration item, all private part
-- transferred to the body of the package because the configuration record is
-- not exported.

with Pump_Controller_Types; use Pump_Controller_Types;
with DFR0555_Display; use DFR0555_Display;

package Configuration is

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
   
   Function LCD_Brightness return Backlight_Brightness;
   
   Configuration_Error : Exception;

end Configuration;
