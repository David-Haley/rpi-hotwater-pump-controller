-- This program is the client that provides the user interface for the pump
-- controller.
-- Author    : David Haley
-- Created   : 29/10/2017
-- Last Edit : 24/07/2021
-- 20210724 : Manual Boost implemented
-- 20210306: Default name HW-Pump-Cont corrected.
-- 20210304 : procedure Send_Command replaces Process_Task declaration.
-- Exception handler removed
-- 20171109 : Correction to command loop, now exits on shutdown
-- 20171107 : Converted to UDP

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions; use Ada.Exceptions;
with User_Interface_Client;

procedure Pump_UI is

   procedure Process_Comands (Controller_Name : String) is

      package User_Interface is new User_Interface_Client (Controller_Name);
      use User_Interface;

   begin -- Process_Comands
      Run_UI;
   end Process_Comands;

begin -- Pump_UI
   if Argument_Count = 1 then
      Process_Comands (Argument (1));
   else
      Process_Comands ("HW-Pump-Cont");
   end if; -- Argument_Count = 1
exception
   when E : others =>
      Put_Line ("Pump_UI - " & Exception_Message (E));
   raise;
end Pump_UI;
