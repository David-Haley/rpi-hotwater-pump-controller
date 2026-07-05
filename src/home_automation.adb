--  This package is the implementation of Home_Automation intended for use with
--  Home Assistant but could work with other automation products that have MQTT
--  support. It is highly flexible in that the broker, user name, topic and
--  field name are all specified by a configuration file Home_Automation.json.
--  The password is obfusscated

-- Author    : David Haley
-- Created   : 05/07/2026
-- Last Edit : 05/07/2026

with Ada.Real_Time; use Ada.Real_Time;
with GNATCOLL.JSON; use GNATCOLL.JSON;
with Common_Configuration; use Common_Configuration;
with DJH.JSON_Configuration;
with MQTT_Client; use MQTT_Client;
with DJH.Events_and_Errors; use DJH.Events_and_Errors;

package body Home_Automation is

   package Configuration is new
      DJH.JSON_Configuration (Parameters, Configuration_File, Encrypted);
   use Configuration;

   Valid_Configuration : Boolean;
   Request_Handle, Acknowledge_Handle : MQTT_Handle;

   function Acknowledge (Request : in Boolean) return Boolean is

      Rx_String : constant String :=
        Receive_Blocking (Acknowledge_Handle, Seconds (59), Seconds (120));
      --  It is assumed that repeated messages will be sent every 60 s until
      --  the automation responds with the boost state matches the requested
      --  state.

      Parsed : Read_Result;

   begin -- Acknowledge
      if Rx_String'Length = 0 then
         --  Timed out or stale data
         return False;
      else
         Parsed := Read (Rx_String);
         if Parsed.Success then
            return Get (Parsed.Value,
                        Get_Value (Common_Configuration.Acknowledge_Field))
              = Request;
         else
            return False;
         end if; -- Parsed.Success
      end if; -- Rx_String'Length = 0
   end Acknowledge;

   function Request_Boost_On return Boolean is
      --  Turns boost element on via home automation

      Automation_JSON : constant JSON_Value := Create_Object;

   begin -- Request_Boost_On
      if Valid_Configuration then
         Set_Field (Automation_JSON, Get_Value (Request_Field), True);
         Send (Request_Handle, Write (Automation_JSON));
         return Acknowledge (True);
      else
         return False;
      end if; -- Valid_Configuration
      exception
         when Event : others =>
            Put_Error ("Request_Boost_On", Event);
            return False;
   end Request_Boost_On;

   function Request_Boost_Off return Boolean is
      --  Turns boost element off via home automation

      Automation_JSON : constant JSON_Value := Create_Object;

   begin -- Request_Boost_Off
      if Valid_Configuration then
         Set_Field (Automation_JSON, Get_Value (Request_Field), False);
         Send (Request_Handle, Write (Automation_JSON));
         return Acknowledge (False);
      else
         return False;
      end if; -- Valid_Configuration
      exception
         when Event : others =>
            Put_Error ("Request_Boost_Off", Event);
            return False;
   end Request_Boost_Off;

begin -- Home_Automation
   Valid_Configuration := Configuration_File_Exists;
   if Valid_Configuration then
      Read_Configuration;
      Connect_Tx (Get_Value (Broker),
                  Get_Value (User),
                  Get_Value (Password),
                  Get_Value (Request_Topic),
                  Request_Handle,
                  2,
                  Keep_Alive_Times'Last);
      Connect_Rx (Get_Value (Broker),
                  Get_Value (User),
                  Get_Value (Password),
                  Get_Value (Acknowledge_Topic),
                  Acknowledge_Handle,
                  2,
                  Keep_Alive_Times'Last);
   else
      raise JSON_Configuration_Error with Configuration_File& " missing";
   end if; -- Valid_Configuration
exception
   when Event: others =>
      Put_Error ("Home_Automation", Event);
      Valid_Configuration := False;
end Home_Automation;
