-- This package provides a level of abstraction for the home automation
-- interface. If there is no home automation interface this package may do
-- nothing or something else, for example, send an email. If the request
-- transmission is successful the (or intentionally does nothing) True should be
-- returned. Returning False indicates that there was a failure
-- Author    : David Haley
-- Created   : 04/04/2019
-- Last Edit : 04/04/2019

package Home_Automation is

   function Request_Boost_On return Boolean;
   -- Turns boost element on via home automation

   function Request_Boost_Off return Boolean;
   -- Turns boost element off via home automation

end Home_Automation;
