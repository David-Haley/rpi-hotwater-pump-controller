-- This package is the implementation of Home_Automation specific to Vera MiOS.
-- Specifically the VeraEdge installation at 19 Bluebell Street
-- Author    : David Haley
-- Created   : 06/04/2019
-- Last Edit : 06/05/2022
-- 20220506 : Now uses unified DJH.Vera
-- 20191010 : BluebellSt domain name changed to 19Bluebell4161.net.au, also new
-- DNS server does not require fully qualified local names.

with DJH.Vera; use DJH.Vera;

package body Home_Automation is

   VeraEdge : constant String := "veraedge";
   Boost_Switch : constant Devices := 23;

   function Request_Boost_On return Boolean is
      -- Turns boost element on via home automation

   begin -- Request_Boost_On
      return Switch_On (VeraEdge, Boost_Switch);
   end Request_Boost_On;

   function Request_Boost_Off return Boolean is
      -- Turns boost element off via home automation

   begin -- Request_Boost_Off
      return Switch_Off (VeraEdge, Boost_Switch);
   end Request_Boost_Off;

end Home_Automation;
