-- This package continuously samples the tank and panel temperatures. The
-- package exports average temperatures which have been corrected by the
-- configuration temperature calibration parameters.
-- Author    : David Haley
-- Created   : 20/10/2017
-- Last Edit : 12/09/2025

-- 20250912 : Better value of correction cooeficient used, error reduced to one
-- tenth of an ADC count.
-- 20250911 : Precision of second order increased, error reduced to much less
-- than one ADC count.
-- 20230916 :  Sample_Temperature declaration moved to specification.
-- 20220506 : Port to native compiler and direct interface to C Buffer size
-- increased from 128 to 574 to give maximum sample frequency between tenth and
-- eleventh harmonics of 48 .. 52 Hz.
-- 20190731 : Next_Time compared to Clock to detect big steps in time, When
-- these occur Next_Time is adjusted to ensure regular sampling.
-- 20190306 : Initialisation of circular buffer added Sample_Temperature renamed
-- and restructured to remove global variable Take_Samples
-- 20190205 : Second order correction applied to average temperature
-- 20190124 : Sampling rate reduced to 128 Hz;
-- 23/11/2017 Time base changed from Ada.Calendar to Ada.Real_Time to correct a
-- startup issue resulting in irregular operation of the watchdog whilst the RPi
-- time is being corrected as a result of update from NTP.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Configuration; use Configuration;
with AD7091r2;

package body Temperature is

   package Controller_AD is new AD7091r2; use Controller_AD;

   Buffer_Size : constant := 574;
   type Buffer_Indices is mod Buffer_Size;
   type Circular_Buffers is array (Buffer_Indices) of A_Volt_Arrays;
   subtype Sample_Sums is Natural range 0 ..
     Buffer_Size * (Natural (A_Volts'Last) + 1);

   protected type Temperature_Data_Type is

      procedure Write_Buffer (Temperature_Sample : in A_Volt_Arrays);

      procedure Get_Sums (Tank_Sum : out Sample_Sums;
                          Panel_Sum : out Sample_Sums);
      procedure Reset_Progress;

      entry Is_Progress;

   private
      Buffer_Index : Buffer_Indices := 0;
      Average_Calculated : Boolean := False;
      Circular_Buffer : Circular_Buffers := (others => (others => 0));
      Progress_Count : Natural := 0;
   end Temperature_Data_Type;

   Temperature_Data : Temperature_Data_Type;

   procedure Average_Temperature (Tank : out Temperatures;
                                  Panel : out Temperatures) is

      function Second_Order_Correction (Temp : in Temperatures)
                                        return Temperatures is

         -- This function applies a second order corection to the straight line
         -- approximation of temperature between 0 C and 100 C. After corection
         -- There is no error at 0 C, 50 C and 100 C. For other temperatures the
         -- error is reduced to a maximum of 0.0025 C, which is approximately a
         -- tenth of one count of the twelve bit ADC.

         C_50 : constant Controller_Reals := 1.499610341517860E-04;
         T : Controller_Reals := Controller_Reals (Temp);

      begin -- Second_Order_Correction
         return Temperatures (T + (C_50 * (T ** 2 - 100.0 * T)));
      end Second_Order_Correction;

      Tank_Sum : Sample_Sums;
      Panel_Sum : Sample_Sums;

   begin -- Average_Temperature
      Temperature_Data.Get_Sums (Tank_Sum, Panel_Sum);
      Tank := Controller_Reals (Tank_Sum) / Controller_Reals (Buffer_Size) *
        Tank_Slope + Tank_Offset;
      Tank := Second_Order_Correction (Tank);
      Panel := Controller_Reals (Panel_Sum) / Controller_Reals (Buffer_Size) *
        Panel_Slope + Panel_Offset;
      Panel := Second_Order_Correction (Panel);
   end Average_Temperature;

   procedure Reset_Progress is
      -- Resets internal progress indicators

   begin -- Reset_Progress
      Temperature_Data.Reset_Progress;
   end Reset_Progress;

   procedure Samplmpling_Progressing is
   Take_Samples : Boolean := True;
      -- Returns when progress is indicated. Criteria half buffer has been
      -- filled and both averages have been calculated.

   begin -- Samplmpling_Progressing
      Temperature_Data.Is_Progress;
   end Samplmpling_Progressing;

   task body Sample_Temperature is

      Take_Samples : Boolean := True;
      Sample_Interval : constant Time_Span :=
        Microseconds (1000000 / Buffer_Size);
      -- Cycle buffer once per second
      Next_Time : Time := Clock + Sample_Interval;

   begin -- Sample_Temperature
      while Take_Samples loop
         select
            accept Stop do
               Take_Samples := False;
            end Stop;
         or
            delay until Next_Time;
            Temperature_Data.Write_Buffer (AD_Read);
            -- read one sample
            if Clock - Next_Time > Seconds (10) then
               -- big step forward in time, step back not possible
               Next_Time := Clock + Sample_Interval;
            else
               Next_Time := Next_Time + Sample_Interval;
            end if; -- Clock - Next_Time > Seconds (10)
         end select;
      end loop; -- Take_Samples
   end Sample_Temperature;

   protected body Temperature_Data_Type is

      procedure Write_Buffer (Temperature_Sample : in A_Volt_Arrays) is

      begin -- Write_Buffer
         Circular_Buffer (Buffer_Index) := Temperature_Sample;
         Buffer_Index := Buffer_Index + 1;
         Progress_Count := Progress_Count + 1;
      end Write_Buffer;

      procedure Get_Sums (Tank_Sum : out Sample_Sums;
                          Panel_Sum : out Sample_Sums) is

      begin -- Get_Sums
         Tank_Sum := 0;
         Panel_Sum := 0;
         for Index in Buffer_Indices loop
            Tank_Sum := Tank_Sum + Sample_Sums (Circular_Buffer (Index) (0));
            -- channel 0 is Tank channel
            Panel_Sum := Panel_Sum + Sample_Sums (Circular_Buffer (Index) (1));
         end loop; --Index in Buffer_Indices
         Average_Calculated := True;
      end Get_Sums;

      procedure Reset_Progress is

      begin -- Reset_Progress
         Average_Calculated := False;
         Progress_Count := 0;
      end Reset_Progress;

      entry Is_Progress when Average_Calculated and
        Progress_Count > Buffer_Size / 2 is

      begin -- Is_Progress
         Average_Calculated := False;
         Progress_Count := 0;
      end Is_Progress;

   end Temperature_Data_Type;

end Temperature;
