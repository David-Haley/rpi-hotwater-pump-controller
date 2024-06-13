-- Reads the Event_Log file to estimate the running time and cost of using the
-- boost element. The cost energy usage and cost requires the boost element
-- Watts and Electricity tariff to be read from "HW_Cost_Parameters.csv".
-- To calculate pump running costs, data is read from daily log files and it is 
-- assumed that the feedin (Export_Tariff) applies.

-- Author    : David Haley
-- Created   : 11/06/2024
-- Last Edit : 14/06/2024

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Calendar; use Ada.Calendar;
--- with Ada.Calendar.Time_Zones;
with Ada.Calendar.Arithmetic; use Ada.Calendar.Arithmetic;
with Ada.Calendar.Formatting;
-- with Ada.Containers;
with Ada.Containers.Ordered_Maps;
with DJH.Parse_CSV;
with DJH.Date_and_Time_Strings; use DJH.Date_and_Time_Strings;

procedure HW_Cost is

   Parameter_File_Name : constant String := "HW_Cost_Parameters.csv";
   Event_File_Name : constant String := "Event_Log.txt";
   CSV : constant String := "csv";

   type My_Real is digits 15;
   subtype kWh is My_Real;
   type Dollars is delta 0.01 range 0.00 .. 10000.00;
   subtype Cents is My_Real range 0.0 .. 99.9999999999999;
   subtype Watts is Positive;
   subtype Seconds is Natural;

   type Parameter_Stores is record
      Pump_Power : Watts;
      Max_Boost :  Ada.Calendar.Formatting.Hour_Number;
      Boost_Power : Watts;
      Import_Tariff, Export_Tariff : Cents;
   end record; -- Parameter_Stores
   
   type Day_Data is record
      Pump_Time, Boost_Time : Seconds := 0;
   end record; -- Day_Data
   
   package Day_Stores is new
      Ada.Containers.Ordered_Maps (Ada.Calendar.Time, Day_Data);
   use Day_Stores;
   
   procedure Get_Parameters (Parameter_Store : out Parameter_Stores) is
   
      type Cost_Parameters is
        (Pump_Watts, Max_Boost, Boost_Watts, Import_Tariff, Export_Tariff);
        
      package Parse_Parameters is new DJH.Parse_CSV (Cost_Parameters);
      use Parse_Parameters;
         
   begin -- Get_Parameters
      Read_Header (Parameter_File_Name);
      if not Next_Row then
         raise Ada.IO_Exceptions.Status_Error with "No cost parameters found";
      end if; -- not Next_Row
      Parameter_Store.Pump_Power := Watts'Value (Get_Value (Pump_Watts));
      Parameter_Store.Max_Boost := 
        Ada.Calendar.Formatting.Hour_Number'Value (Get_Value (Max_Boost));
      Parameter_Store.Boost_Power := Watts'Value (Get_Value (Boost_Watts));
      Parameter_Store.Import_Tariff := Cents'Value (Get_Value (Import_Tariff));
      Parameter_Store.Export_Tariff := Cents'Value (Get_Value (Export_Tariff));
      Close_CSV;
   end Get_Parameters;
      
   function Trim_Date (Date : in Time) return Time is
   
   begin -- Trim_Date
      return Time_Of (Year (Date), Month (Date), Day (Date));
   end Trim_Date;
   
   procedure Read_Event_File (Day_Store : out Day_Stores.Map;
                            Parameter_Store : in Parameter_Stores) is
                            
      type Events is (Date, Time, Message);
   
      package Parse_Logs is new DJH.Parse_CSV (Events);
      use Parse_Logs;
      
      Day_Element : Day_Data;
      Boost_On : Boolean := False;
      Event_Time, Previous_Event_Time : Ada.Calendar.Time;
   
   begin -- Read_Event_File
      Clear (Day_Store);
      No_Header (Event_File_Name);
      while Next_Row loop
         Event_Time := Get_Date (Trim (Get_Value (Date), Both),
           Time_of_Day (Trim (Get_Value (Time), Both)));
         if Row_Number = 1 then
            Previous_Event_Time := Event_Time;
         end if; -- Row_Number = 1
         if index (Get_Value (Message), "boost on", Forward, Lower_Case_Map)
           > 0 then
            if Boost_On then
               -- Missing Boost Off
               Put_Line ("Warning ""Boost Off"" missing after " &
                 Date_String (Full_Date, Event_Time) & " run time of" &
                 Parameter_Store.Max_Boost'Img & " hours assumed");
               Day_Element.Boost_Time :=
                  Seconds (Ada.Calendar.Formatting.Seconds_Of
                  (Parameter_Store.Max_Boost, 0, 0)); 
               include (Day_Store, Trim_Date (Event_Time), Day_Element);
            end if; -- Boost_On
            Boost_ON := True;
            Previous_Event_Time := Event_Time;
         elsif index (Get_Value (Message), "boost off", Forward,
           Lower_Case_Map) > 0 then
            if Event_Time - Previous_Event_Time <
               Ada.Calendar.Formatting.Seconds_Of (Parameter_Store.Max_Boost,
                                                   0,
                                                   0) then
              Day_Element.Boost_Time :=
                Seconds (Ada.Calendar."-" (Event_Time, Previous_Event_Time));
                -- This is very clumsey but necessary because of two instances
                -- of "-" being visible via use clauses.
            else
               -- missing event or events
               Put_Line ("Warning missing events after " &
                 Date_String (Full_Date, Event_Time) & " run time of" &
                 Parameter_Store.Max_Boost'Img & " hours assumed");
               Day_Element.Boost_Time :=
                 Seconds (Ada.Calendar.Formatting.Seconds_Of
                 (Parameter_Store.Max_Boost, 0, 0));
            end if; -- Event_Time - Previous_Event_Time < ...
            include (Day_Store, Trim_Date (Event_Time), Day_Element);
            Boost_On := False;
            Previous_Event_Time := Event_Time;
         end if; --  index (Get_Value (Message), "boost on", Forward, ...
      end loop; -- Next_Row
      Close_CSV;
   end Read_Event_File;
   
   procedure Read_Log_Files (Start_Date, End_Date : in Time;
                             Day_Store : in out Day_Stores.Map) is
                             
      procedure Read_Log_File (File_Name : in String;
                               Log_Date : in Time;
                               Day_Store : in out Day_Stores.Map) is
                               
         type Logs is (Time, Tank_Temperature, Panel_Temperature, Pump_Output);

         package Parse_Logs is new DJH.Parse_CSV (Logs);
         use Parse_Logs;
         
         Day_Value : Day_Data;
         
      begin -- Read_Log_File
         if Exists (File_Name) then
            Read_Header (File_Name, True); -- No '_' in header
            while Next_Row loop
               Day_Value.Pump_Time := @ + 
                 Seconds'Value (Get_Value (Pump_Output));
            end loop;
            if Contains (Day_Store, Log_Date) then
               Day_Store (Log_Date).Pump_Time := Day_Value.Pump_Time;
            else
               Include (Day_Store, Log_Date, Day_Value);
            end if; -- Contains (Day_Store, Log_Date)
            Close_CSV;
         else
            Put_Line ("Warning " & File_Name & "not found");
         end if; -- Exists (File_Name)
      end Read_Log_File;
   
      Log_Date : Time := Start_Date;
      
   begin -- Read_Log_Files
      while Log_Date <= End_Date loop
         Read_Log_File (Compose (Date_String (Year_Only, Log_Date),
                                 Reverse_Date_String (Log_Date),
                                 CSV),
                        Log_Date,
                        Day_Store);
         Log_Date := @ + Day_Duration'Last;
      end loop; -- Log_Date <= End_Date
   end Read_Log_Files;
   
   procedure Report (Start_Date, End_Date : in Time;
                     Parameter_Store : in Parameter_Stores;
                     Day_Store : in Day_Stores.Map) is
      
      function Cost (Time : in Seconds;
                     Power : in Watts;
                     Tariff : in Cents) return Dollars is
      Jouls_per_kWh : constant My_Real := 3600.0 * 1000.00;
      
      begin -- Cost
         return Dollars (My_Real (Time * Power) * My_Real (Tariff)
           / Jouls_per_kWh);
      end Cost;
                     
      Report_File : File_Type;
      Accumulated_Pump, Accumulated_Boost : Seconds := 0;
      Day_Cost, Accumulated_Cost : Dollars;
                     
   begin -- Report
      Create (Report_File, Out_File, Compose ("", "Cost_" &
        Reverse_Date_String (Start_Date) & "_" &
        Reverse_Date_String (End_Date), CSV));
      Put_Line (Report_File, """Date"",""Pump Seconds"",""Boost Seconds""" &
                ",""Cost"",""Accumulated Cost""");
      for I in Iterate (Day_Store) loop
         if Key (I) >= Start_Date and Key (I) <= End_Date then
            Day_Cost := Cost (Element (I).Pump_Time, 
                              Parameter_Store.Pump_Power,
                              Parameter_Store.Export_Tariff) +
                        Cost (Element(I).Boost_Time,
                              Parameter_Store.Boost_Power,
                              Parameter_Store.Import_Tariff);
            Accumulated_Pump := @ + Element (I).Pump_Time;
            Accumulated_Boost := @ + Element(I).Boost_Time;
            Accumulated_Cost := Cost (Accumulated_Pump, 
                                      Parameter_Store.Pump_Power,
                                      Parameter_Store.Export_Tariff) +
                                Cost (Accumulated_Boost,
                                      Parameter_Store.Boost_Power,
                                      Parameter_Store.Import_Tariff);
            Put (Report_File, Date_String (Full_Date, Key (I)) & ",");
            Put (Report_File, Trim (Element (I).Pump_Time'Img, Both) & ",");
            Put (Report_File,
                 Trim (Element (I).Boost_Time'Img ,Both) & ",");
            Put (Report_File, Trim (Day_Cost'Img, Both) & ",");
            Put (Report_File, Trim (Accumulated_Cost'Img, Both) & ",");
            New_Line (Report_File);
         end if; -- Key (I) >= Start_Date and Key (I) <= End_Date
      end loop; -- I in Iterate (Day_Store)
      Close (Report_File);
   end Report;
   
   Parameter_Store : Parameter_Stores;
   Day_Store : Day_Stores.Map;
   Start_Date, End_Date : Time;
   
begin -- HW_Cost
   Put_Line ("HW_Cost version 20240614");
   if Argument_Count = 2 then
      Start_Date := Get_Date (Argument (1));
      End_Date := Get_Date (Argument (2));
      -- The above lines will raise an exception if either of the stings entered
      -- does not represent a valid Gregorian dates.
      Get_Parameters (Parameter_Store);
      Read_Event_File (Day_Store, Parameter_Store);
      Read_Log_Files (Start_Date, End_Date, Day_Store);
      Report (Start_Date, End_Date, Parameter_Store, Day_Store);
   else
      Put_Line ("Usage: hw_cost Start_Date End_Date");
      Put_Line ("Dates must be in the form DD/MM/YYY");
      Put_Line ("File " & Parameter_File_Name & "Must exist in the directory");
      Put_Line ("which the program is executed from, along with Event_Log.txt");
      Put_Line ("and the year directories containing the log files.");
   end if; -- Argument_Count = 2
end HW_Cost;
