-- This package defined the global variables used within the Pump Controller
-- packages.
-- Author    : David Haley
-- Created   : 24/10/2017
-- Last Edit : 20/08/2022
-- 20220820 :  Events_and_Errors move to DJH.Events_and_Errors.
-- 20220729 : Comfortable temteratire reached event added and reset event
-- wording changed.
-- 20220723 : Event added for comfort reset.
-- 20220719 : Correction to test that calls Clear_Is_Comfortable, including
-- Time_Zone so local time rather than UTC is used.
-- 20220715 : Sanitise_Hour changed to Boost_Hour, various renames replaced
-- indirect calls to entries and _SV removed from entries.
-- 20220714 : The hour at which Is_Comfortable is reset is now user configurable
-- via Comfort_Hour. 
-- 20220607 : correction to safety boost logic.
-- 20220529 : Stop_Controller request removed removed, now shutdown through 
-- systemd/systemctl interface.
-- 20220523 : SIGTERM and ctrl C handlers provided to allow orderly shutdown.
-- 20220522 : Is_Comfortable added, Controller_States removed.
-- 20200506 : Port to native compiler and C language interface to SPI.
-- 20210725 : Interface version checking applied to all UI transactions.
-- 20210722 : Mandatory_Boost_Time added.
-- 20210624 : Logging of time changes provided.
-- 20210417 : Hot_Area_SV replaced by Average_Difference_SV.
-- 20210331 : Cold_Time_SV changed to HotArea_SV to reflect integration of
-- temperature difference. Increment_Run_time removed;
-- 20210306 : Start_Boost added
-- 20210304 : Two ports reinstated.
-- 20210304 : Unifued date and time strings, Client_Port removed allows for
-- multiple instances of the user interface even on the same machine.
-- 20210221 : Exception_Stop entry added to Main_Loop.
-- 20210220 : enhanced management of event and exception logging
-- 20200109 : Data log format changed to include pump run seconds instead of
-- '0' or '1' for the pump running or not running.
-- 20191010 : BluebellSt domain name changed to 19Bluebell4161.net.au, also new
-- DNS server does not require fully qualified local names.
-- 20190813 : Correction to Logger task in Data_Logger. Was not creating a new
-- file for each day.
-- 20190731 : Provision of better management for big steps forward in Clock.
-- 20190416 : Boost production build without debug code.
-- 20190408 : Boost logic corrected, debug code retained
-- 20190407 : Boost debug build
-- 20190406 : Next_Boost_Time etc added
-- 20190307 : Centralised event logging in data_logger package. Export Cold_Time
-- to user interface.
-- 20190306 : Cold_Time etc added
-- 20190226 : New build with accumulated pump time being recorded in event file
-- and absence of accumulated time files being recorded in error file.
-- 20190216 : New build with next logging file commit time made available to
-- user interface.
-- 20190205 : New build with second order correction to temperature added.
-- 20190124 : Controller_Version added
-- 20171109 : Pump_Run_Time set to 0 when pump is not running
-- 20171106 : Controller up time added
-- 20171103 : Controller_States migrated here

with Ada.Text_IO; use Ada.Text_IO;
with Pump_Controller_Types; use Pump_Controller_Types;
with Configuration; use Configuration;

package body Global_Data is

   function Controller_Version return Version_String is ("20220820");

   protected body Controller_State is

      function Tank_Temperature return Temperatures is (Tank_Tempetature_SV);

      function Panel_Temperature return Temperatures is (Panel_Temperature_SV);

      function Pump_Run return Boolean is (Pump_Run_SV);

      function Pump_Run_Time return Day_Seconds is (Pump_Run_Time_SV);

      function Accumulated_Pump_Run_Time return Accumulated_Times is
        (Accumulated_Pump_Run_Time_SV);

      function Average_Difference return Temperature_Differences is
        (Average_Difference_SV);

      function Up_Time return Accumulated_Times is (Up_Time_SV);

      function Next_Boost return Boost_Times is (Boost_Time_SV);
      
      function Is_Comfortable return Boolean is (Is_Comfortable_SV);

      function Read_Fault_Table return Fault_Tables is (Fault_Table);

      procedure Write_Temperature (Tank : in Temperatures;
                                   Panel : in Temperatures) is

         -- Assumed that Write_Temperature is called once per second to update
         -- time dependent state variables.

      begin -- Write_Temperature
         Panel_Temperature_SV := Panel;
         Tank_Tempetature_SV := Tank;
         Last_Write := Last_Write + 1;
         Difference_Buffer (Last_Write) :=
           Panel_Temperature_SV - Tank_Tempetature_SV;
         if Maximum_Hot_Delay > 0 then
            Average_Difference_SV := 0.0;
            for I in Difference_indices range 0 ..
              Difference_indices (Maximum_Hot_Delay - 1) loop
               Average_Difference_SV := Average_Difference_SV +
                 (Difference_Buffer (Last_Write - I) /
                      Controller_Reals (Maximum_Hot_Delay));
            end loop; -- I in Difference_indices range 0 ...
         end if; -- Maximum_Hot_Delay > 0
         Up_Time_SV := Up_Time_SV + 1;
         if Pump_Run_SV then
            Pump_Run_Time_SV := Pump_Run_Time_SV + 1;
            Accumulated_Pump_Run_Time_SV := Accumulated_Pump_Run_Time_SV + 1;
         else
            Pump_Run_Time_SV := 0;
         end if; -- Pump_Run_SV
      end Write_Temperature;

      procedure Pump_Start is

      begin -- Pump_Start
         Pump_Run_SV := True;
         Write_Pin (Pin_High, Pump_Relay);
      end Pump_Start;

      procedure Pump_Stop is

      begin -- Pump_Stop
         Pump_Run_SV := False;
         Write_Pin (Pin_Low, Pump_Relay);
      end Pump_Stop;

      procedure Write_Accumulated_Time (Run_Time : in Accumulated_Times) is

      begin -- Write_Accumulated_Time
         Accumulated_Pump_Run_Time_SV := Run_Time;
      end Write_Accumulated_Time;

      procedure Write_Next_Boost_Time (Boost_Time : in Boost_Times) is

      begin -- Write_Next_Boost_Time
         Boost_Time_SV := Boost_Time;
      end Write_Next_Boost_Time;
      
      procedure Set_Is_Comfortable is
      
      begin -- Set_Is_Comfortable
         Controller_State.Is_Comfortable_SV := True;
      end Set_Is_Comfortable;
      
      procedure Clear_Is_Comfortable is
      
      begin -- Clear_Is_Comfortable
         Controller_State.Is_Comfortable_SV := False;
      end Clear_Is_Comfortable;

      procedure Set_Fault (Fault_Type : Fault_Types) is

      begin -- Set_Fault
         Fault_Table (Fault_Type) := True;
         Write_Pin (Pin_High, Fault_LED);
      end Set_Fault;

      procedure Clear_Fault (Fault_Type : Fault_Types) is

         Is_Fault : Boolean := False;

      begin -- Clear_Fault
         Fault_Table (Fault_Type) := False;
         for Fault_index in Fault_Types loop
            Is_Fault := Is_Fault or Fault_Table (Fault_Index);
         end loop; -- Fault_index
         if not Is_Fault then
            Write_Pin (Pin_Low, Fault_LED);
         end if; -- not Is_Fault
      end Clear_Fault;

   end Controller_State;

begin -- Global_Data
   Bind_Pin (Pump_Relay, Out_Pin);
   Write_Pin (Pin_Low, Pump_Relay);
   Bind_Pin (Fault_LED, Out_Pin);
   Write_Pin (Pin_Low, Fault_LED);
end Global_Data;
