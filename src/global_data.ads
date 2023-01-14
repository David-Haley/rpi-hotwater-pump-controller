-- This package defined the global variables used within the Pump Controller
-- packages.
-- Author    : David Haley
-- Created   : 24/10/2017
-- Last Edit : 15/07/2022
-- 20220715 : Sanitise_Hour changed to Boost_Hour, various renames replaced
-- indirect calls to entries and _SV removed from entries.
-- 20220511 : Is_Comfortable added, Controller_States removed.
-- 20210720 : Mandatory_Boost_Time added.
-- 20210417 : Hot_Area_SV replaced by Average_Difference_SV.
-- 20210330 : Cold_Time_SV changed to HotArea_SV to reflect integration of
-- temperature difference. Increment_Run_time removed;
-- 20190404 : Next_Boost_Time etc added
-- 20190307 : IO pins declaration for fault LED and Pump moved to private here.
-- 20190306 : Cold_Time etc added
-- 20190124 : Controller_Version added
-- 06/11/2017 Controller up time added
-- 03/11/2017 Controller_States migrated here

with Ada.Calendar; use Ada.Calendar;
with RPi_GPIO; use RPi_GPIO;
with Pump_Controller_Types; use Pump_Controller_Types;

package Global_Data is

   type Difference_indices is mod Maximum_Hot_Delays'Last;

   type Difference_Buffers is Array (Difference_indices) of
     Temperature_Differences;

   function Controller_Version return Version_String;

   protected Controller_State is

      function Tank_Temperature return Temperatures;

      function Panel_Temperature return Temperatures;

      function Pump_Run return Boolean;

      function Pump_Run_Time return Day_Seconds;

      function Average_Difference return Temperature_Differences;

      function Accumulated_Pump_Run_Time return Accumulated_Times;

      function Up_Time return Accumulated_Times;

      function Next_Boost return Boost_Times;
      
      function Is_Comfortable return Boolean;

      function Read_Fault_Table return Fault_Tables;

      procedure Write_Temperature (Tank : in Temperatures;
                                   Panel : in Temperatures);
      -- Assumed that Write_Temperature is called once per second to update
      -- time dependent state variables.

      procedure Pump_Start;

      procedure Pump_Stop;

      procedure Write_Accumulated_Time (Run_Time : in Accumulated_Times);

      procedure Write_Next_Boost_Time (Boost_Time : in Boost_Times);
      
      procedure Set_Is_Comfortable;
      
      procedure Clear_Is_Comfortable;

      procedure Set_Fault (Fault_Type : in Fault_Types);

      procedure Clear_Fault (Fault_Type : in Fault_Types);

   private
      Panel_Temperature_SV, Tank_Tempetature_SV : Temperatures := 0.0;
      Difference_Buffer : Difference_Buffers := (others => 0.0);
      -- Contains history of Panel_Temperatire - Tank_Temperature
      Last_Write : Difference_indices := Difference_indices'First;
      -- Recordes last element of Difference_Buffer which was updated
      Average_Difference_SV : Temperature_Differences := 0.0;
      Pump_Run_SV : Boolean := False;
      Pump_Run_Time_SV : Day_Seconds := 0;
      Accumulated_Pump_Run_Time_SV : Accumulated_Times := 0;
      Up_Time_SV : Accumulated_Times := 0;
      Boost_Time_SV : Boost_Times;
      Is_Comfortable_SV : Boolean := False;
      -- Records if Comfortable temteratire has been reached since 0:00:00 each
      -- day.
      Fault_Table : Fault_Tables := (others => False);
   end Controller_State;

private
   Pump_Relay : constant GPIO_Pins := Gen1;
   Fault_LED : constant GPIO_Pins := Gen2;
end Global_Data;
