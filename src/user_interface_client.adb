-- This package provides client component for a locally distributed user
-- interface. It ptovides a display of the current status and allows commands to
-- be sent to the controller programme
-- Author    : David Haley
-- Created   : 29/10/2017
-- Last Edit : 31/05/2022
-- 20220531 : Corrected issue with timeout when geting manual boost date.
-- 20220529 : Stop_Controller request removed removed, now shutdown through 
-- systemd/systemctl interface.
-- 20220522 : Receive timeout proviede in Receive_Response to for provide
-- orderly shutdown when there is no reception from server.
-- 20220522 : Is_Comfortable added;
-- 20210805 : Flow animation improved and Next_Second added.
-- 20210727 : Criteria for recalculating Next_Time corrected.
-- 20210725 : Ensure only valid requests are sent, Interface version checking
-- applied to all transactions.
-- 20210724 : Manual Boost implemented
-- 20210417 : Hot_Area replaced by Average_Difference (Average Temperature
-- Difference) order of display rearranged.
-- 20210331 : In display of temperature difference Controller_Real_IO
-- replaced by Temperature_Difference_IO. Cold_Lockout_Time changed to Hot_Area.
-- Controller_Time added
-- 20210306: Default name HW-Pump-Cont corrected.
-- 20210305 : Tidy up two sockets reinstated Exception handler added.
-- 20210304 : Port to Windows to provide remote access;
-- 20191024 : Accumulated_Pump_Run_Time now displayed as HHHHHH:MM:SS
-- 20190906 : Previous pump run duration displayed.
-- 20190731 : Bar graph resolution increased to 10C
-- 20190417 : Correction to 60 bar graph, else clause added.
-- 20190416 : Termperature bar graphs added.
-- 20190406 : Automatic boost supported
-- 20190404 : Background colour added to pump animation. Fault table capacity
-- increased, annunciator text only.
-- 20190323 : In the pump animation FG_Blue changed to FG_Cyan
-- 20190322 : Differential temperature added and pump run time relocated up one
-- line. Colour added to pump animation.
-- 20190307 : Cold_Lockout_Time displayed
-- 20190216 : Display next file commit time in message area;
-- 20190124 : User interface updated to show both controller and user interface
-- versions and graphics updated for greater accuracy;
-- 20171110 : Seconds counts converted to Days, hours, minutes and seconds
-- 20171108 Converted to UDP

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams; use Ada.Streams;
with Ada.Exceptions; use Ada.Exceptions;
with GNAT.Sockets; use GNAT.Sockets;
with Pump_Controller_Types; use Pump_Controller_Types;
with Shared_User_Interface; use Shared_User_Interface;
with DJH.Date_and_Time_Strings; use DJH.Date_and_Time_Strings;
with ANSI_Console; use ANSI_console;

package body User_Interface_Client is

   Controller_Address : Sock_Addr_Type := (Family => Family_Inet,
                                           Addr => Addresses (Get_Host_By_Name
                                             (Controller_Name), 1),
                                           Port => Server_Port);

   RX_Address : Sock_Addr_Type := (Family => Family_Inet,
                                   Addr => Any_Inet_Addr,
                                   Port => Client_Port);

   Screen_Width : constant := 71;

   Screen_Template : constant array (Y_Pos) of String (1 .. Screen_Width) :=
     (
--     00000000001111111111222222222233333333334444444444555555555566666666667
--     01234567890123456789012345678901234567890123456789012345678901234567890
      "Pump Controller version: YYYYMMDD                       Time: HH:MM:SS ",
      "User Interface version: YYYYMMDD  Build: 20220531    +-------------+--+",
      "Up Time:                                            /  Panel      /   |",
      "Temperature Difference: -TU.t C                    / Temperature /    |",
      "Average Temperature Difference: -HTU.t C          /    HTU.t C  /     |",
      "Next Boost:                                   +--+-------------+      |",
      " +-------------+----> Hot                     |                       |",
      " |             |                              |                       |",
      " |             | +----< Cold Supply           |                       |",
      " |             | |                            |                       |",
      " |             | |  Pump   Cold to Panel      |                       |",
      " |             +-+--( )-----------------------+                       |",
      " |    Tank     |    Run Time:             Previous Run:               |",
      " | Temperature |    Total Run Time:                                   |",
      " |    TU.t C   +------------------------------------------------------+",
      " +-------------+      Hot from Panel      +---------------------------+",
      "                                          | Command:                  |",
      "+-----------------------------------------+ Clear fault table         |",
      "|           Fault Annunciators            | Exit ui                   |",
      "|                                         | Manual boost              |",
      "|                                         | Refresh screen            |",
      "+-----------------------------------------+---------------------------+",
      "| Messages:                                                           |",
      "+---------------------------------------------------------------------+"
     );

   task Process_Requests is
      entry RX_Ready;
      entry Start;
      entry Finished;
   end Process_Requests;

   task Update_Screen is
      entry Finished;
   end Update_Screen;

   procedure Run_UI is

   begin -- Run_UI
      Process_Requests.Start;
      Update_Screen.Finished;
   end Run_UI;

   procedure Put_Coloured_Text (Text : in String;
                                X : in X_Pos; Y : in Y_Pos;
                                FG : in FG_Colours;
                                BG : BG_Colours := BG_Black) is

   begin -- Put_Coloured_Text
      Goto_XY (X, Y);
      Set_FG_Colour (FG);
      Set_BG_Colour (BG);
      Put (Text);
      Set_FG_Colour (FG_White);
      Set_BG_Colour (BG_Black);
   end Put_Coloured_Text;

   task body Process_Requests is

      function Next_Second return Time is

         -- returns a value which is exactly on the next second

         Update_Interval : constant Day_Duration := 1.0; -- Run at 1 Hz
         Next_Time : Time := Clock + Update_Interval;

      begin -- Next_Second
         return Time_Of (Ada.Calendar.Formatting.Year (Next_Time),
                         Ada.Calendar.Formatting.Month (Next_Time),
                         Ada.Calendar.Formatting.Day (Next_Time),
                         Hour (Next_Time),
                         Minute (Next_Time),
                         Second (Next_Time));
         -- Sub_Second defaults to Zero
      end Next_Second;

      procedure Get_Boost_Date (Boost_Date : out Time) is

         procedure Message (Text : in String) is

         begin -- Message
            Put_Coloured_Text (Text, X_Pos'First, 5, FG_Red);
            Bleep;
            delay 3.0;
         end;

         Date_In : Date_Strings := Date_String (Full_Date);
         Entered_Date : Time;
         Text : Unbounded_String;
         Prompt : constant String := "Enter date: ";
         Prompt_Y : constant Y_Pos := 3;
         Valid : Boolean;

      begin -- Get_Boost_Date
         loop -- get valid date
            Valid := True;
            Boost_Date := Clock;
            Entered_Date := Boost_Date;
            Clear_Screen;
            Goto_XY (X_Pos'First, Y_Pos'First);
            Put ("Hot Water Pump Controller User Interface");
            Goto_XY (X_Pos'First, 1);
            Put ("Manual Boost Date Dialogue");
            Valid := True;
            Goto_XY (X_Pos'First, Prompt_Y);
            Put (Prompt);
            Put (Date_In);
            Goto_XY (Prompt'Length, Prompt_Y);
            Get_Line (Text);
            Trim (Text, Both);
            begin -- date exception block
               if Date_Strings'Length = Length (Text) then
                  Entered_Date := Get_Date (To_String (Text));
               elsif Length (Text) = 0 then
                  null; -- Date unchanged
               elsif Date_Strings'Length > Length (Text) then
                  Entered_Date := Get_Date (Replace_Slice (Date_In, 1,
                                            Length (text), To_String (Text)));
               else
                  Message ("Too much text entered");
                  Valid := False;
               end if; -- Date_Strings'Length = Length (Text)
            exception
               when others =>
                  Message ("Invalid date, must be Gregorian date in DD/MM/YYYY"
                             & " format");
                  Valid := False;
            end; -- date exception block
            exit when Valid;
         end loop; -- valid date
         Boost_Date := Entered_Date;
      end Get_Boost_Date;

      TX_Socket : Socket_Type;

      procedure Send_Request (Request_Record : in Request_Records) is
         -- Sends requests via UDP to user interface server.

         TX_Buffer : Request_Buffers;
         for TX_Buffer'Address use Request_Record'Address;
         pragma Import (Ada, TX_Buffer);
         Last : Stream_Element_Offset;

      begin -- Send_Request
         Send_Socket (TX_Socket, TX_Buffer, Last, Controller_Address);
      end Send_Request;

      Run_Process_Requests : Boolean := True;
      User_Input : Character;
      User_Input_Available : Boolean;
      Request : Requests;

   begin -- Process_Requests
      accept Start;
      Create_Socket (TX_Socket, Family_Inet, Socket_Datagram); -- UDP socket
      accept RX_Ready;
      while Run_Process_Requests loop
         delay until Next_Second;
         Get_Immediate (User_Input, User_Input_Available);
         if User_Input_Available then
            -- process user generated commands
            case User_Input is
            when 'c' | 'C' =>
               Request := Clear_Fault_Table;
            when 'e' | 'E' =>
               Request := Exit_User_Interface;
            when 'm' | 'M' =>
               Request := Manual_Boost;
            when 'r' | 'R' =>
               Request := Refresh_Screen;
            when others =>
               Request := Get_Status;
               -- ensures that a valid request is generated
            end case; -- User_Input
            declare -- Request_Record
               Request_Record : Request_Records (Request);
            begin -- Request_Record
               if Request_Record.Request = Manual_Boost then
                  -- Sends request to server which will not alter boost time but
                  -- is returned to prevent a receive timeout.
                  Request_Record.User_Input := False;
                  Send_Request (Request_Record);
                  delay 2.0; -- wait for previous request to be processed
                  Get_Boost_Date
                    (Request_Record.Next_Boost_Time.Next_Boost_Time);
                  Request_Record.User_Input := True;
               end if; -- Request_Record.Request = Manual_Boost
               Send_Request (Request_Record);
               if Request_Record.Request = Exit_User_Interface then
                  Run_Process_Requests := False;
               end if; -- Request_Record.Request = Exit_User_Interface
            end; -- Request_Record
         else
            declare -- Request_Record
               Request_Record : Request_Records (Get_Status);
            begin -- Request_Record
               Send_Request (Request_Record);
            end; -- Request_Record
         end if; -- User_Input_Available
      end loop; -- Run_Process_Commands
      Close_Socket (TX_Socket);
      accept Finished;
   end Process_Requests;

   task body Update_Screen is

      RX_Socket : Socket_Type;
      Saved_Commit_Time : Time := Clock;
      Saved_Pump_Run, Previous_Pump_Run : Day_Seconds := 0;
      Run_Screen_Update : Boolean := True;
      Supress_Timeout : Boolean := False;

      type Pump_Amnimation_States is mod 4;
      Pump_Animation_State : Pump_Amnimation_States := 0;
      type Flow_Amnimation_States is mod 7;
      Flow_Animation_State : Flow_Amnimation_States := 0;

      procedure Put_Screen_Template is

      begin -- Put_Screen_Template
         Clear_Screen;
         for Y in Y_Pos loop
            Goto_XY (0, Y);
            Put (Screen_Template (Y));
         end loop;
         Goto_XY (24, 1);
         Put (Interface_Version);
      end Put_Screen_Template;

      procedure Receive_Response is

         procedure Animate_Pump (Running : in Boolean) is

            Pump_X : X_Pos := 21;
            Pump_Y : Y_Pos := 11;
            Cold_X : X_Pos := 41;
            Cold_Y : Y_Pos := 10;
            Hot_X : X_Pos := 17;
            Hot_Y : Y_Pos := 15;

            subtype Flow_String is String (1 .. 4);

            Animation : constant array (Pump_Amnimation_States) of
              String (1 .. 1):= ("-", "\", "|", "/");
            Cold_Animation : constant array (Flow_Amnimation_States) of
              Flow_String := (">>> ", " >>>", "  >>", "   >", "    ", ">   ",
                              ">>  ");
            Hot_Animation : constant array (Flow_Amnimation_States) of
              Flow_String := (" <<<", "<<< ", "<<  ", "<   ", "    ", "   <",
                              "  <<");
            No_Flow : constant Flow_String := "    ";

         begin -- Animate_Pump
            if Running then
               Put_Coloured_Text (Animation (Pump_Animation_State), Pump_X,
                                  Pump_Y, FG_Cyan);
               Put_Coloured_Text (Cold_Animation (Flow_Animation_State),
                                  Cold_X, Cold_Y, FG_Blue, BG_Cyan);
               Put_Coloured_Text (Hot_Animation (Flow_Animation_State),
                                  Hot_X, Hot_Y, FG_Yellow, BG_Red);
               Pump_Animation_State := Pump_Animation_State + 1;
               Flow_Animation_State := Flow_Animation_State + 1;
            else
               Goto_XY (Pump_X, Pump_Y);
               Put (' ');
               Goto_XY (Cold_X, Cold_Y);
               Put (No_Flow);
               Goto_XY (Hot_X, Hot_Y);
               Put (No_Flow);
            end if; -- Running
         end Animate_Pump;

         procedure Put_Message (Message : in String) is

            Message_X : constant X_Pos := 12;
            Message_Y :  constant Y_Pos := 22;

         begin -- Put_Message
            Goto_XY (0, Message_Y);
            Put (Screen_Template (Message_Y));
            Goto_XY (Message_X, Message_Y);
            Put (Message);
         end Put_Message;

         procedure Put (Fault_Table : in Fault_Tables) is

            Fault_Row_1 : constant Y_Pos := 19;
            Fault_Row_2 : constant Y_Pos := Fault_Row_1 + 1;

            FG : FG_Colours;
            BG : BG_Colours;

         begin -- Put
            for Fault_Index in Fault_Types loop
               if Fault_Table (Fault_Index) then
                  FG := FG_White;
                  BG := BG_Red;
                     Put_Coloured_Text ("Pump Log", 2, Fault_Row_1, FG, BG);
               else
                  FG := FG_Black;
                  BG := BG_Green;
               end if; -- Fault_Table (Fault_Index)
               case Fault_Index is
                  when Accumulated_Time_File =>
                     Put_Coloured_Text ("Pump Log", 2, Fault_Row_1, FG, BG);
                  when Log_File =>
                     Put_Coloured_Text ("Data Log", 11, Fault_Row_1, FG, BG);
                  when Tank_Temperature =>
                     Put_Coloured_Text ("Tank Temperature", 20, Fault_Row_1,
                                        FG, BG);
                 when Boost_Failure =>
                     Put_Coloured_Text ("Auto Boost", 2, Fault_Row_2, FG, BG);
               end case; -- Fault_Index
            end loop; -- Fault_Index in Fault_Types
         end Put;

         procedure Bar_Graph (X : in X_Pos; Y : in Y_Pos;
                              Temperature : in Temperatures) is

            Bar_Character : constant Character := ' ';

         begin -- Bar_Graph
            Goto_XY (X, Y);
            if Temperature > 0.0 then
               Set_BG_Colour (BG_Blue);
            else
               Set_BG_Colour (BG_Black);
            end if; -- Temperature > 0.0
            Put (Bar_Character);
            Goto_XY (X + 1, Y);
            if  Temperature > 10.0 then
               Set_BG_Colour (BG_Blue);
            else
               Set_BG_Colour (BG_Black);
            end if; -- Temperature > 10.0
            Put (Bar_Character);
            Goto_XY (X, Y - 1);
            if Temperature > 20.0 then
               Set_BG_Colour (BG_Cyan);
            else
               Set_BG_Colour (BG_Black);
            end if; -- Temperature > 20.0
            Put (Bar_Character);
            Goto_XY (X + 1, Y - 1);
            if Temperature > 30.0 then
               Set_BG_Colour (BG_Cyan);
            else
               Set_BG_Colour (BG_Black);
            end if; -- Temperature > 30.0
            Put (Bar_Character);
            Goto_XY (X, Y - 2);
            if Temperature > 40.0 then
               Set_BG_Colour (BG_Yellow);
            else
               Set_BG_Colour (BG_Black);
            end if; -- Temperature > 40.0
            Put (Bar_Character);
            Goto_XY (X + 1, Y - 2);
            if Temperature > 50.0 then
               Set_BG_Colour (BG_Yellow);
            else
               Set_BG_Colour (BG_Black);
            end if; -- Temperature > 50.0
            Put (Bar_Character);
            Goto_XY (X, Y - 3);
            if Temperature > 60.0 then
               Set_BG_Colour (BG_Red);
            else
               Set_BG_Colour (BG_Black);
            end if; -- Temperature > 60.0
            Put (Bar_Character);
            Goto_XY (X + 1, Y - 3);
            if Temperature > 70.0 then
               Set_BG_Colour (BG_Red);
            else
               Set_BG_Colour (BG_Black);
            end if; -- Temperature > 70.0
            Put (Bar_Character);
            Goto_XY (X, Y - 4);
            if Temperature > 80.0 then
               Set_BG_Colour (BG_Red);
            else
               Set_BG_Colour (BG_Black);
            end if; -- Temperature > 80.0
            Put (Bar_Character);
            Goto_XY (X + 1, Y - 4);
            if Temperature > 90.0 then
               Set_BG_Colour (BG_Red);
            else
               Set_BG_Colour (BG_Black);
            end if; -- Temperature > 90.0
            Put (Bar_Character);
            Set_FG_Colour (FG_White);
            Set_BG_Colour (BG_Black);
         end Bar_Graph;

         Status : Status_Records;
         RX_Buffer : Response_Buffers;
         for RX_Buffer'Address use Status'Address;
         pragma Import (Ada, RX_Buffer);
         Last : Stream_Element_Offset;

      begin -- Receive_Response
         if not Supress_Timeout then
            Goto_XY (53, 16); -- Position at Command Prompt
         end if; -- not Supress_Timeout
         Receive_Socket (RX_Socket, RX_Buffer, Last);
         if Status.User_Interface_Version /= Interface_Version then
            raise Version_Mismatch with "Server Version: " &
              Status.User_Interface_Version & " does not match "
              & Interface_Version;
         end if; -- Status.Version /= Interface_Version
         case Status.Request is
            when Get_Status =>
               Supress_Timeout := False;
               Goto_XY (62, 0);
               Put (Time_String (Status.Controller_Time));
               GoTo_XY (25, 0);
               Put (Status.Controller_Version);
               Goto_XY (5, 14);
               Temperature_IO.Put (Status.Tank_Temperature, 3, 1, 0);
               Bar_Graph (12, 11, Status.Tank_Temperature);
               Goto_XY (54, 4);
               Temperature_IO.Put (Status.Panel_Temperature, 4, 1, 0);
               Bar_Graph (67, 6, Status.Panel_Temperature);
               Goto_XY (23, 3);
               Temperature_Difference_IO.Put (Status.Panel_Temperature -
                                         Status.Tank_Temperature, 4, 1, 0);
               Animate_Pump (Status.Pump_Run);
               Goto_XY (3, 16);
               if Status.Is_Comfortable then
                  Put ("Comfortable");
               else
                  Put ("   Cold    ");
               end if; -- Status.Is_Comfortable
               Goto_XY (30, 12);
               Put (Elapsed_Seconds (Status.Pump_Run_Time, Exclude_Days));
               if Status.Pump_Run_Time = 0 and Saved_Pump_Run > 0 then
                  Previous_Pump_Run := Saved_Pump_Run;
               end if; --  Status.Pump_Run_Time = 0 and Saved_Pump_Run > 0
                  Saved_Pump_Run := Status.Pump_Run_Time;
               Goto_XY (56, 12);
               Put (Elapsed_Seconds (Previous_Pump_Run, Exclude_Days));
               Goto_XY (36, 13);
               Put (Elapsed_Seconds (Status.Accumulated_Pump_Run_Time,
                    Hours_More_Than_24));
               Goto_XY (9, 2);
               Put (Elapsed_Seconds (Status.Controller_Up_Time, Include_Days));
               Goto_XY (31, 4);
               Temperature_Difference_IO.Put (Status.Average_Difference, 5, 1,
                                              0);
               Goto_XY (12, 5);
               Put (Image (Status.Next_Boost_Time.Next_Boost_Time, False,
                    UTC_Time_Offset));
               if Saved_Commit_Time /= Status.Next_File_Commit_Time then
                  Saved_Commit_Time := Status.Next_File_Commit_Time;
                  Put_Message ("Next log file commit time " &
                                 Time_String (Saved_Commit_Time));
               end if; -- Saved_Commit_Time /= Status.Next_File_Commit_Time
               Put (Status.Fault_Table);
            when Clear_Fault_Table =>
               Put_Message ("Fault Table Cleared " & Time_String);
            when Exit_User_Interface =>
               Run_Screen_Update := False;
            when Refresh_Screen =>
               Put_Screen_Template;
               Put_Message ("Next log file commit time " &
                              Time_String (Saved_Commit_Time));
            when Manual_Boost =>
               Put_Screen_Template;
               Supress_Timeout := True;
               if Status.User_Input then
                  Put_Message ("Boost time updated");
               else
                  Put_Message ("Getting next boost time");
               end if; -- Status.User_Input
         end case;
      exception
         when Socket_Error =>
            if not Supress_Timeout then
               Put_Message ("No reception from server, press 'e' to exit");
               Run_Screen_Update := False;
            end if; -- not Supress_Timeout
      end Receive_Response;

   begin -- Update_Screen
      Create_Socket (RX_Socket, Family_Inet, Socket_Datagram); -- UDP socket
      Set_Socket_Option (RX_Socket, Socket_Level, (Receive_Timeout, 10.0));
      Bind_Socket (RX_Socket, RX_Address);
      Put_Screen_Template;
      Process_Requests.RX_Ready;
      while Run_Screen_Update loop
         Receive_Response;
      end loop;
      Process_Requests.Finished;
      Clear_Screen;
      Goto_XY (X_Pos'First, Y_Pos'First);
      Close_Socket (RX_Socket);
      accept Finished;
   end Update_Screen;

begin -- User_Interface_Client
   null;
exception
   when E : others =>
      Bleep;
      delay 5.0; -- provide time to observe screen prior to exception message
      Clear_Screen;
      Put_Line ("User_Interface_Client - " & Exception_Message (E));
      raise;
end User_Interface_Client;
