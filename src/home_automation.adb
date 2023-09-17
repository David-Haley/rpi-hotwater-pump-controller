-- This package is the implementation of Home_Automation specific to Vera MiOS.
-- Specifically the VeraEdge installation at 19 Bluebell Street
-- Author    : David Haley
-- Created   : 06/04/2019
-- Last Edit : 16/09/2023
-- 20230916 : Local management of exceptions raised in calls to Vera, return
-- failure and log. rather than propagating exception.
-- 20220506 : Now uses unified DJH.Vera
-- 20191010 : BluebellSt domain name changed to 19Bluebell4161.net.au, also new
-- DNS server does not require fully qualified local names.

with DJH.Vera; use DJH.Vera;
with DJH.Events_and_Errors; use DJH.Events_and_Errors;

package body Home_Automation is

   VeraEdge : constant String := "veraedge";
   Boost_Switch : constant Devices := 23;

   function Request_Boost_On return Boolean is
      -- Turns boost element on via home automation

      Result : Boolean;

   begin -- Request_Boost_On
      begin -- Vera exception block
         Result := Switch_On (VeraEdge, Boost_Switch);
      exception
         when Event : others =>
            Put_Error ("Request_Boost_On", Event);
            Result := False;
      end; -- Vera exception block
      return Result;
   end Request_Boost_On;

   function Request_Boost_Off return Boolean is
      -- Turns boost element off via home automation

      Result : Boolean;

   begin -- Request_Boost_Off
      begin -- Vera exception block
         Result := Switch_Off (VeraEdge, Boost_Switch);
      exception
         when Event : others =>
            Put_Error ("Request_Boost_Off", Event);
            Result := False;
      end; -- Vera exception block
      return Result;
   end Request_Boost_Off;

end Home_Automation;
