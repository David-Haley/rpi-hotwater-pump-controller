-- This package provides the shared information to support a distributed user
-- interface.
-- Author    : David Haley
-- Created   : 29/10/2017
-- Last Edit : 29/05/2022
-- 20220529 : Stop_Controller request removed removed, now shutdown through 
-- systemd/systemctl interface. User_Input added to allow an ineffective
-- Manual_Boost to be sent.
-- 20220522 : Is_Comfortable added
-- 20210725 : Interface version checking applied to all transactions.
-- 20210724 : Manual Boost facility added.
-- 20210417 : Hot_Area replaced by Average_Difference
-- 20210331 : Cold_Lockout_Time changed to Hot_Area, Controller_Time added to
-- Status_Records;
-- 20210304 : Distinct ports for each dierction reinstated, port numbers
-- increased to 50001 and 50002.
-- 20210303 : The Response_Port (client) is removed providing for reception on
-- a system determined port, hence making multiple instance of the cliant
-- possible on the same machine.
-- 20190406 : Next_Boost_Time added
-- 20190307 : Cold_Lockout_Time added
-- 20190216 : Next_File_Commit_Time added.
-- 20190124 : Separate user interface and controller versions provided
-- 20171107 buffer types added
-- 20171107 Converted to UDP interface

with Ada.Streams; use Ada.Streams;
with Ada.Calendar; use Ada.Calendar;
with GNAT.Sockets; use GNAT.Sockets;
with Pump_Controller_Types; use Pump_Controller_Types;

package Shared_User_Interface is

   Interface_Version : constant Version_String := "20220529";
   Server_Port : Port_Type := 50001;
   Client_Port : Port_Type := Server_Port + 1;
   -- Having different port numbers for each direction allows for the the user
   -- interface to run on the same machine as the server or externally.

   type Requests is (Clear_Fault_Table, Exit_User_Interface, Get_Status,
                     Manual_Boost, Refresh_Screen);

   type Request_Records (Request : Requests := Get_Status) is record
      User_Interface_Version: Version_String := Interface_Version;
      case Request is
         when Manual_Boost =>
            User_Input : Boolean := False;
            Next_Boost_Time : Boost_Times;
         when others =>
            null;
      end case; -- Request
   end record; -- Request_Records

   type Status_Records (Request : Requests := Get_Status) is record
      User_Interface_Version: Version_String := Interface_Version;
      case Request is
         when Get_Status =>
            Controller_Version : Version_String := "YYYYMMDD";
            Controller_Time : Time;
            Panel_Temperature, Tank_Temperature : Temperatures;
            Pump_Run, Is_Comfortable : Boolean;
            Pump_Run_Time : Day_Seconds;
            Average_Difference : Temperature_Differences;
            Accumulated_Pump_Run_Time : Accumulated_Times;
            Controller_Up_Time : Accumulated_Times;
            Next_File_Commit_Time : Time;
            Next_Boost_Time : Boost_Times;
            Fault_Table : Fault_Tables;
         when Manual_Boost =>
            User_Input : Boolean := False;
         when others =>
            null;
      end case;
   end record;

   subtype Request_Buffers is Stream_Element_Array
     (1 .. (Request_Records'Size + 7) / Stream_Element'Size);
   subtype Response_Buffers is Stream_Element_Array
     (1 .. (Status_Records'Size + 7) / Stream_Element'Size);

end Shared_User_Interface;
