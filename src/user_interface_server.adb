--  This package provides server component for the user interface.
--  Author    : David Haley
--  Created   : 29/10/2017
--  Last Edit : 09/08/2026

--  20260809 : MQTT Publisher and integrated web server.
--  20260619 : Compiler warnings removed.
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

with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with GNATCOLL.JSON; use GNATCOLL.JSON;
with MQTT_Client; use MQTT_Client;
with DJH.JSON_Configuration;
with DJH.Date_and_Time_Strings; use DJH.Date_and_Time_Strings;
with DJH.Events_and_Errors; use DJH.Events_and_Errors;
with Pump_Controller_Types; use Pump_Controller_Types;
with Common_Status_Configuration; use Common_Status_Configuration;

package body User_Interface_Server is

   package Parser is new
   DJH.JSON_Configuration (Parameters, Configuration_File, Encrypted);
   use Parser;

   Publish_Interval : constant Duration := 1.0;

   task body UI_Server is

      procedure Publish (Status_Record : in Status_Records;
                         Publish_Handle : in MQTT_Handle) is

         Celsius : constant Character := 'C';
         Good : constant String := "Good";
         Bad : constant String := "Bad";
         Status_JSON : constant JSON_Value := Create_Object;
         Temp_String : String (1 .. 5);
         Difference : Temperature_Differences;

      begin -- Publish
         Set_Field (Status_JSON, "Controller_Version",
                    Status_Record. Controller_Version);
         Set_Field (Status_JSON,"Controller_Time",
                    Time_String (Status_Record.Controller_Time));
         Set_Field (Status_JSON,"Controller_Up_Time",
                    Elapsed_Seconds (Status_Record.Controller_Up_Time,
                                     Include_Days));
         Temperature_IO.Put (Temp_String, Status_Record.Panel_Temperature,
                             1, 0);
         Set_Field (Status_JSON, "Panel_Temperature", Temp_String & Celsius);
         Temperature_IO.Put (Temp_String, Status_Record.Tank_Temperature,
                             1, 0);
         Set_Field (Status_JSON, "Tank_Temperature", Temp_String & Celsius);
         Difference := Status_Record.Panel_Temperature -
           Status_Record.Tank_Temperature;
         Temperature_Difference_IO.Put (Temp_String, Difference, 1, 0);
         Set_Field (Status_JSON, "Temperature_Difference",
                    Temp_String & Celsius);
         Temperature_Difference_IO.Put (Temp_String,
                                        Status_Record.Average_Difference, 1, 0);
         Set_Field (Status_JSON, "Average_Difference", Temp_String & Celsius);
         if Status_Record.Is_Comfortable then
            Set_Field (Status_JSON, "Is_Comfortable", "Comfortable");
         else
            Set_Field (Status_JSON, "Is_Comfortable", "Cold");
         end if; -- Status_Record.Is_Comfortable
         if Status_Record.Pump_Run then
            Set_Field (Status_JSON,"Pump_Run", "Runing");
         else
            Set_Field (Status_JSON,"Pump_Run", "Stoped");
         end if; -- Status_Record.Pump_Run
         Set_Field (Status_JSON,"Pump_Run_Time",
                    Elapsed_Seconds (Status_Record.Pump_Run_Time,
                                     Exclude_Days));
         Set_Field (Status_JSON,"Previous_Run_Duration",
                    Elapsed_Seconds (Status_Record.Previous_Run_Duration,
                                     Exclude_Days));
         Set_Field (Status_JSON,"Previous_Run_Time",
                    Time_String (Status_Record.Previous_Run_Time));
         Set_Field (Status_JSON,"Accumulated_Pump_Run_Time",
                    Elapsed_Seconds (Status_Record.Accumulated_Pump_Run_Time,
                                     Hours_More_Than_24));
         Set_Field (Status_JSON,"Next_File_Commit_Time",
                    Time_String (Status_Record.Next_File_Commit_Time));
         Set_Field (Status_JSON,"Next_Boost_Time",
                    Image (Status_Record.Next_Boost_Time.Next_Boost_Time, False,
                    UTC_Time_Offset));
         if Status_Record.Fault_Table (Accumulated_Time_File) then
            Set_Field (Status_JSON,"Pump_Log", Bad);
         else
            Set_Field (Status_JSON,"Pump_Log", Good);
         end if; -- Status_Record.Fault_Table (Accumulated_Time_File)
         if Status_Record.Fault_Table (Log_File) then
            Set_Field (Status_JSON,"Data_Log", Bad);
         else
            Set_Field (Status_JSON,"Data_Log", Good);
         end if; -- Status_Record.Fault_Table (Log_File)
         if Status_Record.Fault_Table (Tank_Temperature) then
            Set_Field (Status_JSON,"Tank_Temperature", Bad);
         else
            Set_Field (Status_JSON,"Tank_Temperature", Good);
         end if; -- Status_Record.Fault_Table (Tank_Temperature)
         if Status_Record.Fault_Table (Boost_Failure) then
            Set_Field (Status_JSON,"Auto_Boost", Bad);
         else
            Set_Field (Status_JSON,"Auto_Boost", Good);
         end if; -- Status_Record.Fault_Table (Boost_Failure)
         Send (Publish_Handle, Write (Status_JSON));
      end Publish;

      Run_User_Interface : Boolean := True;
      Status_Record : Status_Records;
      Publish_Handle : MQTT_Handle;
      Connect_Count : Natural := 0;
      Next_Time : Time := Clock;

   begin -- UI_Serve
      if Configuration_File_Exists then
         Read_Configuration;
         Connect_Tx (Get_Value (Broker),
                     Get_Value (User),
                     Get_Value (Password),
                     Get_Value (Status_Topic),
                     Publish_Handle);
      else
         raise JSON_Configuration_Error with Configuration_File & " missing";
      end if; -- Valid_Configuration
      loop -- Until Connected 
         delay 0.1;
         exit when Is_Connected_Tx (Publish_Handle) or Connect_Count > 100;
         -- Allows up to 10s to connect
         Connect_Count := @ + 1;
      end loop; -- Until Connected
      if not Is_Connected_Tx (Publish_Handle) then
         raise MQTT_Error with "UIServer connection timed out";
      end if; -- not Is_Connected_Tx (Publish_Handle)
      Status_Record := Get_Status;
      while Run_User_Interface loop
         begin -- UI_Server exception block
            select
               accept Get_Status (Status : out Status_Records) do
                  Status := Status_Record;
               end Get_Status;
               accept Stop do
                  Run_User_Interface := False;
               end Stop;
            or
               --  Only intended to run at approximately intervals, some
               --  randomness in delay should reduce potential conflict with
               --  essential controller processes.
               delay until Next_Time;
               Status_Record := Get_Status;
               Publish (Status_Record, Publish_Handle);
               Next_Time := Clock + Publish_Interval;
            end select;
         exception -- UI_Server exception block
            when Event: others =>
               Put_Error ("UI_Server loop", Event); 
         end; -- UI_Server exception block
      end loop; -- Run_User_Interface
      Disconnect (Publish_Handle);
   exception -- UI_Server exception block
      when Event: others =>
         Put_Error ("UI_Server", Event);
   end UI_Server;

end User_Interface_Server;
