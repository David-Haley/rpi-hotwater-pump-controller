-- This package provides server component for the user interface.
-- Author    : David Haley
-- Created   : 29/10/2017
-- Last Edit : 07/05/2025

-- 20250507 : Start_User_Interface remoced to reduce potential for startup
-- deadlock.
-- 20250503 : Controller_State prefix removed from all Global_Data references.
-- 20230916 : Run_Controller and Stop_User_Interface removed, UI_Server task
-- with Stop made public. No fixed Client Address, allows multiple instances of
-- user interface on the same host.
-- 20220820 : Events_and_Errors move to DJH.Events_and_Errors.
-- 20220715 : _SV removed from all Controller_State entries.
-- 20220529 : Stop_Controller request removed removed, now shutdown through 
-- systemd/systemctl interface. User_Input added to allow an ineffective
-- Manual_Boost to be sent.
-- 20220523 : Stop_User_Interface added.
-- 20220522 : Is_Comfortable added;
-- 20210725 : Interface version checking applied to all transactions.
-- 20210721 : Manual Boost facility added and logging of fault table clearing.
-- 20210417 : Hot_Area and Hot_Area_SV replaced by Average_Difference and
-- Average_Difference_SV respecuively.
-- 20210330 : Cold_Lockout_Time changed to Hot_Area and Controller_Time added.
-- 20210306 : Debug Put_Line removed
-- 20210304 : Reinstated two ports
-- 20210303 : setting of cliant port removed;
-- 20190406 : Access 0rovided to Status.Next_Boost_Time
-- 20190307 : Access provided to Cold_Time_SV
-- 20190216 : Access provided to the next file commit time.
-- 20190124 : User interface updated to show both controller and user interface
-- versions;
-- 20171107 Converted to UDP

with Ada.Streams; use Ada.Streams;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with GNAT.Sockets; use GNAT.Sockets;
with DJH.Events_and_Errors; use DJH.Events_and_Errors;
with Pump_Controller_Types; use Pump_Controller_Types;
with Global_Data; use Global_Data;
with Shared_User_Interface; use Shared_User_Interface;
with Data_Logger; use Data_Logger;
with Boost; use Boost;

package body User_Interface_Server is

   task body UI_Server is

      Run_User_Interface : Boolean := True;
      RX_Socket, TX_Socket : Socket_Type;
      Server_Address : Sock_Addr_Type := (Family => Family_Inet,
                                          Addr => Any_Inet_Addr,
                                          Port => Server_Port);
      procedure Process_Requests is

         Request_Record : Request_Records;
         RX_Buffer : Request_Buffers;
         for RX_Buffer'Address use Request_Record'Address;
         pragma Import (Ada, RX_Buffer);
         Last : Stream_Element_Offset;
         Client_Address : Sock_Addr_Type;
         Boost_Time : Boost_Times;

      begin -- Process_Requests
         begin -- Rx exception block
            Receive_Socket (RX_Socket, RX_Buffer, Last, Client_Address);
         exception when Socket_Error =>
            null; -- an exception is raised if there is a receive timeout
         end; -- Rx exception block
         if Last > 0 and then
           Interface_Version = Request_Record.User_Interface_Version then
            declare -- scope of TX_Buffer
               Status : Status_Records (Request_Record.Request);
               TX_Buffer : Response_Buffers;
               for TX_Buffer'Address use Status'Address;
               pragma Import (Ada, TX_Buffer);
            begin -- scope of TX_Buffer
               case Request_Record.Request is
               when Get_Status =>
                  Status.Controller_Version := Controller_Version;
                  Status.Controller_Time := Clock;
                  Status.Tank_Temperature := Tank_Temperature;
                  Status.Panel_Temperature := Panel_Temperature;
                  Status.Pump_Run := Pump_Run;
                  Status.Is_Comfortable := Is_Comfortable;
                  Status.Average_Difference := Average_Difference;
                  Status.Pump_Run_Time := Pump_Run_Time;
                  Status.Accumulated_Pump_Run_Time := Accumulated_Pump_Run_Time;
                  Status.Controller_Up_Time := Up_Time;
                  Status.Next_File_Commit_Time := Read_File_Commit_Time;
                  Status.Next_Boost_Time := Next_Boost;
                  Status.Fault_Table := Read_Fault_Table;
               when Clear_Fault_Table =>
                  Put_Event ("Fault Table Cleared");
                  for Fault_Index in Fault_Types loop
                     Clear_Fault (Fault_Index);
                  end loop; --Fault_Index in Fault_Types
               when Manual_Boost =>
                  Status.User_Input := Request_Record.User_Input;
                  if Request_Record.User_Input then
                     Boost_Time := Next_Boost;
                     Request_Record.Next_Boost_Time.Next_Boost_Time :=
                       Next_Boost_Time (Request_Record.Next_Boost_Time.
                       Next_Boost_Time);
                     if Request_Record.Next_Boost_Time.Next_Boost_Time <=
                       Boost_Time.Mandatory_Boost_Time then
                        Boost_Time.Next_Boost_Time :=
                          Request_Record.Next_Boost_Time.Next_Boost_Time;
                        Write_Next_Boost_Time (Boost_Time);
                     end if; -- Request_Record.Next_Boost_Time.Next_Boost_Time 
                  end if; -- Request_Record.User_Input
               when others =>
                  null; -- Acknowledged no additional data sent
               end case; -- Request_Record.Request
               Send_Socket (TX_Socket, TX_Buffer, Last, Client_Address);
            end; -- scope of TX_Buffer
         elsif Last > 0 then
            Put_Event ("Request received from wrong version UI: " &
                         Request_Record.User_Interface_Version);
         end if; -- Last > 0 and then ...
      end Process_Requests;

   begin -- UI_Server
      Create_Socket (RX_Socket, Family_Inet, Socket_Datagram);
      Set_Socket_Option (RX_Socket, Socket_Level, (Receive_Timeout, 3.0));
      -- Allows server loop to potentially accept Stop regularly.
      Bind_Socket (RX_Socket, Server_Address);
      Create_Socket (TX_Socket, Family_Inet, Socket_Datagram);
      while Run_User_Interface loop
         select
            accept Stop do
               Run_User_Interface := False;
            end Stop;
         else
            Process_Requests;
         end select;
      end loop; -- Run_User_Interface
      Close_Socket (RX_Socket);
      Close_Socket (TX_Socket);
      exception
         when Event: others =>
            Put_Error ("UI_Server", Event); 
   end UI_Server;

end User_Interface_Server;
