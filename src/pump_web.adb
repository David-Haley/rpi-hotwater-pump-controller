-- This program is a web server that provides equivalent functionality to
-- Pump_UI (status display and command entry) but is accessed from a browser
-- over HTTP instead of an ANSI terminal.
-- Author    : David Haley
-- Created   : 06/08/2026
--
-- Usage: pump_web [controller_name [http_port]]

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions; use Ada.Exceptions;
with GNAT.Sockets; use GNAT.Sockets;
with User_Interface_Web;

procedure Pump_Web is

   Default_Controller_Name : constant String := "HW-Pump-Cont";
   Default_HTTP_Port : constant Port_Type := 8080;

   procedure Process_Requests (Controller_Name : String;
                               HTTP_Port : Port_Type) is

      package Web_Interface is new User_Interface_Web (Controller_Name);
      use Web_Interface;

   begin -- Process_Requests
      Run_UI (HTTP_Port);
   end Process_Requests;

begin -- Pump_Web
   case Argument_Count is
      when 0 =>
         Process_Requests (Default_Controller_Name, Default_HTTP_Port);
      when 1 =>
         Process_Requests (Argument (1), Default_HTTP_Port);
      when others =>
         Process_Requests (Argument (1), Port_Type'Value (Argument (2)));
   end case; -- Argument_Count
exception
   when E : others =>
      Put_Line ("Pump_Web - " & Exception_Message (E));
   raise;
end Pump_Web;
