--  This package defined the global variables used within the Pump Controller
--  packages.
--  Author    : David Haley
--  Created   : 24/10/2017
--  Last Edit : 08/08/2026

--  202260808 : Declaration of Status_Records moved here and Get_Status added.
--  The intention is to minimise overhead of serving data to the user interface.
--  File commit time and associated subprograms moved here.
--  20260619 : Compiler warnings removed.
-- 20250502 : Barriers added to ensure that values are defined before being
-- read.
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

with Pump_Controller_Types; use Pump_Controller_Types;
with Ada.Calendar; use Ada.Calendar;

package Global_Data is

   type Status_Records is record
      Controller_Version : Version_String := "YYYYMMDD";
      Controller_Time : Time;
      Panel_Temperature, Tank_Temperature : Temperatures;
      Pump_Run, Is_Comfortable  : Boolean;
      Pump_Run_Time, Previous_Run_Duration : Day_Seconds;
      Previous_Run_Time : Time;
      Average_Difference : Temperature_Differences;
      Accumulated_Pump_Run_Time : Accumulated_Times;
      Controller_Up_Time : Accumulated_Times;
      Next_File_Commit_Time : Time;
      Next_Boost_Time : Boost_Times;
      Fault_Table : Fault_Tables;
   end record;

   function Controller_Version return Version_String;

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

   function Get_Status return Status_Records;

   function Get_Next_File_Commit_Time return Time;
   -- Returns time of next file commit, that is, Flush (xx)

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

   procedure Set_Next_File_Commit_Time;
   --  Adds one hour to the previous file commit time.

end Global_Data;
