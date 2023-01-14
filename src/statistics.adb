-- Basic statistics, calculates Mean and Variance.
-- Author    : David Haley
-- Created   : 09/10/2017
-- Last Edit : 13/10/2017
-- 12/10/2017: Minimum and Maximum functions added
-- 13/10/2017: use My_Float, Frequency added;

with Ada.Numerics.Generic_Elementary_Functions;

package body Statistics is

   Sample_Count : Natural := 0;
   Sum, Square_Sum : My_Float := 0.0;
   Min_Data : Data_Sample := Data_Sample'Last;
   Max_Data : Data_Sample := Data_Sample'First;
   Occurance_Count : array (Data_Sample) of Natural := (others => 0);

   package My_Numerics is new Ada.Numerics.Generic_Elementary_Functions (My_Float);
   use My_Numerics;

   procedure Clear is
      -- Clears the data accumulators.

   begin -- Clear
      Sample_Count := 0;
      Sum := 0.0;
      Square_Sum := 0.0;
      Min_Data := Data_Sample'Last;
      Max_Data := Data_Sample'First;
      for I in Data_Sample loop
         Occurance_Count (I) := 0;
      end loop; -- I in Data_Sample
   end Clear;


   procedure Sample (Data : in Data_Sample) is
      -- Adds a sample to the data accummulators.

   begin -- Sample_Data
      Sample_Count := Sample_Count + 1;
      Sum := Sum + My_Float(Data);
      Square_Sum := Square_Sum + My_Float (Data) ** 2;
      Occurance_Count (Data) := Occurance_Count (Data) + 1;
      if Data < Min_Data then
         Min_Data := Data;
      end if; -- Data < Min_Data
      if Data > Max_Data then
         Max_Data := Data;
      end if; -- Data > Max_Data
   end Sample;

   function Count return Natural is
      -- Returns the count of Samples that have been added to the data
      -- accummulators.

   begin -- Count
      return Sample_Count;
   end Count;

   function Mean return My_Float is
      -- Returns the Mean or average for Samples that have been added to the
      -- data accummulators.

   begin -- Mean
      if Sample_Count < 2 then
         raise Insufficient_Samples;
      else
         return Sum / My_Float (Sample_Count);
      end if; -- Sample_Count < 2
   end Mean;

   function Variance return My_Float is
      -- Returns the Variance or standard deviation for Samples that have been
      -- added to the data accummulators.

   begin -- Variance
      if Sample_Count < 2 then
         raise Insufficient_Samples;
      else
         return sqrt ((Square_Sum - Sum ** 2 / My_Float (Sample_Count)) /
                        My_Float (Sample_Count - 1));
      end if; -- Sample_Count < 2
   end Variance;

   function Minimum return Data_Sample is
      -- Returns the minimum value of the samples that have been added to the
      -- data accumulators.

   begin -- Minimum
      return Min_Data;
   end Minimum;

   function Maximum return Data_Sample is
      -- Returns the maximum value of the samples that have been added to the
      -- data accumulators.
   begin -- Maximum
      return Max_Data;
   end Maximum;

   function Frequency (Data : in Data_Sample) return Natural is
   -- Returns the number of occurrances of samples entered with the specified
   -- value Data. return values can be used to build a histogram.

   begin -- Frequency
      return Occurance_Count (Data);
   end Frequency;

end Statistics;
