--  Reads the Event_Log file to estimate the running time and cost of using the
--  boost element. The cost energy usage and cost requires the boost element
--  Watts and Electricity tariff to be read from "HW_Cost_Parameters.csv".
--  To calculate pump running costs, data is read from daily log files and it is 
--  assumed that the feedin (Export_Tariff) applies. Solar energy is estimated
--  based in the flow read from the flow meter (assumed constant) and the
--  temperature difference between the panel and tank temperatures and the time
--  the pump runs. Clearly this is a fairly rough estimate due to the
--  uncertanty in the small temperature difference and the flow meter. The solar
--  energy is costed using the Import_Tarriff because this is what will
--  substitute for any deficiency in solar energy.

--  Author    : David Haley
--  Created   : 11/06/2024
--  Last Edit : 17/04/2026

--  20260417 : Apply trapezium rule to energy calculation.
--  20260327: Estimated savings added.

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Arithmetic; use Ada.Calendar.Arithmetic;
with Ada.Calendar.Formatting;
with Ada.Containers.Ordered_Maps;
with DJH.Parse_CSV;
with DJH.Date_and_Time_Strings; use DJH.Date_and_Time_Strings;

procedure HW_Cost is

   Parameter_File_Name : constant String := "HW_Cost_Parameters.csv";
   Event_File_Name : constant String := "Event_Log.txt";
   CSV : constant String := "csv";

   type My_Real is digits 15;
   type Dollars is delta 0.01 range 0.00 .. 10000.00;
   subtype Cents is My_Real range 0.0 .. 99.9999999999999;
   subtype Watts is Positive;
   subtype Seconds is Natural;
   subtype Joules is My_Real;

   type Parameter_Stores is record
      Pump_Power : Watts;
      Max_Boost :  Ada.Calendar.Formatting.Hour_Number;
      Boost_Power : Watts;
      Import_Tariff, Export_Tariff : Cents;
      Mass_Flow : My_Real; -- l/m
   end record; -- Parameter_Stores
   
   type Day_Data is record
      Pump_Time, Boost_Time : Seconds := 0;
      Solar_Energy : Joules := 0.0;
   end record; -- Day_Data
   
   Specific_Heat : constant My_Real := 4.19e3;
   --  J/(K*kg) at 50 C and constant pressure
   Density : constant My_Real := 0.98802; -- kg/l at 50 C and one atmosphere
   Flow_Conversion : constant My_Real := Density / 60.0; -- l/minute to kg/s
   Power_Conversion : constant My_Real := Specific_Heat * Flow_Conversion;
   --  W/(K*(l/minute))
   J_Conversion : constant My_Real := 1.0 / (1000.0 * 3600.0); -- kWh/J
   Test_Temperature : constant My_Real := 80.0;
   --  If the tank temperature exceeds this temperature the pump is probably not
   --  running so the solar energy should not be counted.
   
   package Day_Stores is new
      Ada.Containers.Ordered_Maps (Ada.Calendar.Time, Day_Data);
   use Day_Stores;
   
   procedure Get_Parameters (Parameter_Store : out Parameter_Stores) is
   
      type Cost_Parameters is (Pump_Watts, Max_Boost, Boost_Watts,
        Import_Tariff, Export_Tariff, Mass_Flow);
        
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
      Parameter_Store.Mass_Flow := My_Real'Value (Get_Value (Mass_Flow));
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
                             Day_Store : in out Day_Stores.Map;
                             Parameter_Store : in Parameter_Stores) is
                             
      procedure Read_Log_File (File_Name : in String;
                               Log_Date : in Time;
                               Day_Store : in out Day_Stores.Map;
                               Parameter_Store : in Parameter_Stores) is
                               
         type Logs is (Time, Tank_Temperature, Panel_Temperature, Pump_Output);

         package Parse_Logs is new DJH.Parse_CSV (Logs);
         use Parse_Logs;
         
         Day_Value : Day_Data;
         Previous_Temperature_Difference : My_Real := 0.0;
         
      begin -- Read_Log_File
         if Exists (File_Name) then
            Read_Header (File_Name, True); -- No '_' in header
            while Next_Row loop
               if My_Real'Value (Get_Value (Panel_Temperature)) <
                 Test_Temperature
               then
                  Day_Value.Pump_Time := @ + 
                    Seconds'Value (Get_Value (Pump_Output));
                  Day_Value.Solar_Energy := @ + 
                    (My_Real (Seconds'Value (Get_Value (Pump_Output))) *
                    (My_Real'Value (Get_Value (Panel_Temperature)) - 
                    My_Real'Value (Get_Value (Tank_Temperature)) +
                    Previous_Temperature_Difference) / 2.0);
                    -- Use trapezium rule
                  Previous_Temperature_Difference :=
                    (My_Real'Value (Get_Value (Panel_Temperature)) - 
                    My_Real'Value (Get_Value (Tank_Temperature)));
               end if; -- My_Real'Value (Get_Value (Panel_Temperature)) < ..
            end loop; -- Next_Row
            Day_Value.Solar_Energy :=
              @ * Power_Conversion * Parameter_Store.Mass_Flow;
            if Contains (Day_Store, Log_Date) then
               Day_Store (Log_Date).Pump_Time := Day_Value.Pump_Time;
               Day_Store (Log_Date).Solar_Energy := Day_Value.Solar_Energy;
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
                        Day_Store,
                        Parameter_Store);
         Log_Date := @ + Day_Duration'Last;
      end loop; -- Log_Date <= End_Date
   end Read_Log_Files;
   
   procedure Report (Start_Date, End_Date : in Time;
                     Parameter_Store : in Parameter_Stores;
                     Day_Store : in Day_Stores.Map) is
      
      function Cost (Time : in Seconds;
                     Power : in Watts;
                     Tariff : in Cents) return Dollars is
      
      begin -- Cost
         return Dollars (My_Real (Time * Power) * My_Real (Tariff)
           * J_Conversion);
      end Cost;
      
      function Cost (Solar_Energy : Joules;
                     Tariff : in Cents) return Dollars is
      
      begin -- Cost
         return Dollars (Solar_Energy * My_Real (Tariff)
           * J_Conversion);
      end Cost;
                     
      Report_File : File_Type;
      Accumulator : Day_Data;
      Accumulated_Cost, Accumulated_Saving : Dollars;
                     
   begin -- Report
      for I in Iterate (Day_Store) loop
         if Key (I) >= Start_Date and Key (I) <= End_Date then
            Accumulator.Pump_Time := @ + Element (I).Pump_Time;
            Accumulator.Boost_Time := @ + Element(I).Boost_Time;
            Accumulator.Solar_Energy := @ + Element (I).Solar_Energy;
         end if; -- Key (I) >= Start_Date and Key (I) <= End_Date
      end loop; -- I in Iterate (Day_Store)
      Create (Report_File, Out_File, Compose ("", "Cost_" &
        Reverse_Date_String (Start_Date) & "_" &
        Reverse_Date_String (End_Date), CSV));
      Put_Line (Report_File, """Pump Seconds"",""Boost Seconds""," &
                """Accumulated Cost"",""Accumulated_Saving""");
      Accumulated_Cost := Cost (Accumulator.Pump_Time, 
                                Parameter_Store.Pump_Power,
                                Parameter_Store.Export_Tariff) +
                          Cost (Accumulator.Boost_Time,
                                Parameter_Store.Boost_Power,
                                Parameter_Store.Import_Tariff);
      Accumulated_Saving := Cost (Accumulator.Solar_Energy,
                                  Parameter_Store.Import_Tariff);
      Put (Report_File, Trim (Accumulator.Pump_Time'Img, Both) & ",");
      Put (Report_File, Trim (Accumulator.Boost_Time'Img ,Both) & ",");
      Put (Report_File, Trim (Accumulated_Cost'Img, Both) & ",");
      Put (Report_File, Trim (Accumulated_Saving'Img, Both));
      New_Line (Report_File);
      Close (Report_File);
   end Report;
   
   Parameter_Store : Parameter_Stores;
   Day_Store : Day_Stores.Map;
   Start_Date, End_Date : Time;
   
begin -- HW_Cost
   Put_Line ("HW_Cost version 20260327");
   if Argument_Count = 2 then
      Start_Date := Get_Date (Argument (1));
      End_Date := Get_Date (Argument (2));
      -- The above lines will raise an exception if either of the stings entered
      -- does not represent a valid Gregorian dates.
      Get_Parameters (Parameter_Store);
      Read_Event_File (Day_Store, Parameter_Store);
      Read_Log_Files (Start_Date, End_Date, Day_Store, Parameter_Store);
      Report (Start_Date, End_Date, Parameter_Store, Day_Store);
   else
      Put_Line ("Usage: hw_cost Start_Date End_Date");
      Put_Line ("Dates must be in the form DD/MM/YYYY");
      Put_Line ("File " & Parameter_File_Name & "Must exist in the directory");
      Put_Line ("which the program is executed from, along with Event_Log.txt");
      Put_Line ("and the year directories containing the log files.");
   end if; -- Argument_Count = 2
end HW_Cost;
