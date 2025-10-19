-- This package defined the global variables used within the Pump Controller
-- packages.
-- Author    : David Haley
-- Created   : 24/10/2017
-- Last Edit : 19/10/2025

-- 20251019 : Setting of Fault table items now logged. Fault LED extinguished on
-- normal exit.
-- 20251017 : The pump stop logic changed to delay stopping when the
-- Maximum_Tank_Temperature is exceded. This should prevent short cycling by
-- ensuring some overshoot in temperature.
-- 20251012 : LCD display added.
-- 20250912 : Better value of correction cooeficient for Second order correction
-- in temperature.adb.
-- 20250911 : Second order correction precision increased in temperature.adb.
-- 20250506 : Start_Logger and Start_User_Interface removed, to remove startup
-- deadlock.
-- 20250503 : Barriers added to ensure that values are defined before being
-- read.
-- 20250501 : Updated due to removal of Start_Events from DJH.Events_and_Errors,
-- logging of configurstion file date and time added.
-- 20230913 : Home_Automation exception management revised, exception
-- termination revised and user interface only uses one fixes port allowing for
-- multiple instances on the same host.
-- 20230511 : Log files compacted by removal of leading spaces.
-- 20220820 :  Events_and_Errors move to DJH.Events_and_Errors.
-- 20220729 : Comfortable temperature reached event added and reset event
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
with RPi_GPIO; use RPi_GPIO;
with Pump_Controller_Types; use Pump_Controller_Types;
with Configuration; use Configuration;

package body Global_Data is

   type Difference_indices is mod Maximum_Hot_Delays'Last;

   type Difference_Buffers is Array (Difference_indices) of
     Temperature_Differences;

   Pump_Relay : constant GPIO_Pins := Gen1;
   Fault_LED : constant GPIO_Pins := Gen2;

   function Controller_Version return Version_String is ("20251019");
   
   -- Barriers have only been provided where a value could be undefined during
   -- startup. Barriers are not required where the variables are actually
   -- initialised in in this package.

   protected Controller_State is

      entry Tank_Temperature (Result : out Temperatures);

      entry Panel_Temperature (Result : out Temperatures);

      function Pump_Run return Boolean;

      function Pump_Run_Time return Day_Seconds;

      entry Average_Difference (Result : out Temperature_Differences);

      entry Accumulated_Pump_Run_Time (Result : out Accumulated_Times);

      function Up_Time return Accumulated_Times;

      entry Next_Boost (Result : out Boost_Times);
      
      function Is_Comfortable return Boolean;

      function Read_Fault_Table return Fault_Tables;

      procedure Write_Temperature (Tank : in Temperatures;
                                   Panel : in Temperatures);

      procedure Pump_Start;

      procedure Pump_Stop;

      procedure Write_Accumulated_Time (Run_Time : in Accumulated_Times);

      procedure Write_Next_Boost_Time (Boost_Time : in Boost_Times);
      
      procedure Set_Is_Comfortable;
      
      procedure Clear_Is_Comfortable;

      procedure Set_Fault (Fault_Type : in Fault_Types);

      procedure Clear_Fault (Fault_Type : in Fault_Types);

   private
      Defined_Temperature, Defined_Accumulated_Time, Defined_Boost_Time
        : Boolean := False; -- Initialise barriers
      Panel_Temperature_SV, Tank_Tempetature_SV : Temperatures := 0.0;
      Difference_Buffer : Difference_Buffers := (others => 0.0);
      -- Contains history of Panel_Temperature - Tank_Temperature
      Last_Write : Difference_indices := Difference_indices'First;
      -- Recordes last element of Difference_Buffer which was updated
      Average_Difference_SV : Temperature_Differences := 0.0;
      Pump_Run_SV : Boolean := False;
      Pump_Run_Time_SV : Day_Seconds := 0;
      Accumulated_Pump_Run_Time_SV : Accumulated_Times := 0;
      Up_Time_SV : Accumulated_Times := 0;
      Boost_Time_SV : Boost_Times;
      Is_Comfortable_SV : Boolean := False;
      -- Records if Comfortable temperature has been reached since Comfort_Hour
      -- each day.
      Fault_Table : Fault_Tables := (others => False);
      -- Initialised to no fault
   end Controller_State;

   protected body Controller_State is

      entry Tank_Temperature (Result : out Temperatures)
        when Defined_Temperature is
        
      begin -- Tank_Temperature
         Result := Tank_Tempetature_SV;
      end Tank_Temperature;

      entry Panel_Temperature (Result : out Temperatures)
        when Defined_Temperature is
      
      begin -- Panel_Temperature
         Result := Panel_Temperature_SV;
      end Panel_Temperature;

      function Pump_Run return Boolean is (Pump_Run_SV);

      function Pump_Run_Time return Day_Seconds is (Pump_Run_Time_SV);

      entry Accumulated_Pump_Run_Time (result : out Accumulated_Times)
        when Defined_Accumulated_Time is
        
      begin -- Accumulated_Pump_Run_Time
         Result := Accumulated_Pump_Run_Time_SV;
      end Accumulated_Pump_Run_Time;

      entry Average_Difference (Result : out Temperature_Differences)
        when Defined_Temperature is
        
      begin -- Average_Difference
         Result := Average_Difference_SV;
      end Average_Difference;

      function Up_Time return Accumulated_Times is (Up_Time_SV);

      entry Next_Boost (Result : out Boost_Times) when Defined_Boost_Time is
        
      begin -- Next_Boost
         Result := Boost_Time_SV;
      end Next_Boost;
      
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
         Up_Time_SV := @ + 1;
         if Pump_Run_SV then
            Pump_Run_Time_SV := @ + 1;
            Accumulated_Pump_Run_Time_SV := @ + 1;
         else
            Pump_Run_Time_SV := 0;
         end if; -- Pump_Run_SV
         Defined_Temperature := True;
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
         Defined_Accumulated_Time := True;
      end Write_Accumulated_Time;

      procedure Write_Next_Boost_Time (Boost_Time : in Boost_Times) is

      begin -- Write_Next_Boost_Time
         Boost_Time_SV := Boost_Time;
         Defined_Boost_Time := True;
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
            Is_Fault := @ or Fault_Table (Fault_Index);
         end loop; -- Fault_index
         if not Is_Fault then
            Write_Pin (Pin_Low, Fault_LED);
         end if; -- not Is_Fault
      end Clear_Fault;

   end Controller_State;
   
   -- Externally visible subprograms related to Controller state are below.

   function Tank_Temperature return Temperatures is
   
    Result : Temperatures;
   
   begin -- Tank_Temperature
      Controller_State.Tank_Temperature (Result);
      return Result;
   end Tank_Temperature;

   function Panel_Temperature return Temperatures is
   
    Result : Temperatures;
   
   begin -- Panel_Temperature
      Controller_State.Panel_Temperature (Result);
      return Result;
   end Panel_Temperature;

   function Pump_Run return Boolean is (Controller_State.Pump_Run);

   function Pump_Run_Time return Day_Seconds is
     (Controller_State.Pump_Run_Time);

   function Average_Difference return Temperature_Differences is
   
    Result : Temperature_Differences;
   
   begin -- Average_Difference
      Controller_State.Average_Difference (Result);
      return Result;
   end Average_Difference;

   function Accumulated_Pump_Run_Time return Accumulated_Times is
   
      Result : Accumulated_Times;
      
   begin -- Accumulated_Pump_Run_Time
      Controller_State.Accumulated_Pump_Run_Time (Result);
      return Result;
   end Accumulated_Pump_Run_Time;

   function Up_Time return Accumulated_Times is (Controller_State.Up_Time);

   function Next_Boost return Boost_Times is
   
    Result : Boost_Times;
   
   begin -- Next_Boost
      Controller_State.Next_Boost (Result);
      return Result;
   end Next_Boost;

   function Is_Comfortable return Boolean is (Controller_State.Is_Comfortable);

   function Read_Fault_Table return Fault_Tables is
     (Controller_State.Read_Fault_Table);

   procedure Write_Temperature (Tank : in Temperatures;
                                Panel : in Temperatures) is
                                
   begin -- Write_Temperature
      Controller_State.Write_Temperature (Tank, Panel);
   end Write_Temperature;

   procedure Pump_Start is
   
   begin -- Pump_Start
      Controller_State.Pump_Start;
   end Pump_Start;

   procedure Pump_Stop is
   
   begin -- Pump_Stop
      Controller_State.Pump_Stop;
   end Pump_Stop;

   procedure Write_Accumulated_Time (Run_Time : in Accumulated_Times) is
   
   begin -- Write_Accumulated_Time
      Controller_State.Write_Accumulated_Time (Run_Time);
   end Write_Accumulated_Time;

   procedure Write_Next_Boost_Time (Boost_Time : in Boost_Times) is
   
   begin -- Write_Next_Boost_Time
      Controller_State.Write_Next_Boost_Time (Boost_Time);
   end Write_Next_Boost_Time;

   procedure Set_Is_Comfortable is
   
   begin -- Set_Is_Comfortable
      Controller_State.Set_Is_Comfortable;
   end Set_Is_Comfortable;

   procedure Clear_Is_Comfortable is
   
   begin -- Clear_Is_Comfortable
      Controller_State.Clear_Is_Comfortable;
   end Clear_Is_Comfortable;

   procedure Set_Fault (Fault_Type : in Fault_Types) is
   
   begin -- Set_Fault
      Controller_State.Set_Fault (Fault_Type);
   end Set_Fault;

   procedure Clear_Fault (Fault_Type : in Fault_Types) is
   
   begin -- Clear_Fault
      Controller_State.Clear_Fault (Fault_Type);
   end Clear_Fault;

begin -- Global_Data
   Bind_Pin (Pump_Relay, Out_Pin);
   Write_Pin (Pin_Low, Pump_Relay);
   Bind_Pin (Fault_LED, Out_Pin);
   Write_Pin (Pin_Low, Fault_LED);
end Global_Data;
