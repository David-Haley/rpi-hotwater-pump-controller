-- Basic statistics, calculates Mean and Variance.
-- Author    : David Haley
-- Created   : 09/10/2017
-- Last Edit : 13/10/2017
-- 12/10/2017: Minimum and Maximum functions added
-- 13/10/2017: use My_Float, Frequency added;

with Ada.Exceptions; use Ada.Exceptions;

generic
   type Data_Sample is range <>;

package Statistics is

   type My_Float is digits 15;

   procedure Clear;
   -- Clears the data accumulators.

   procedure Sample (Data : in Data_Sample);
   -- Adds a sample to the data accummulators.

   function Count return Natural;
   -- Returns the count of Samples that have been added to the data
   -- accummulators.

   function Mean return My_Float;
   -- Returns the Mean or average for Samples that have been added to the data
   -- accummulators.

   function Variance return My_Float;
   -- Returns the Variance or standard deviation for Samples that have been
   -- added to the data accummulators.

   function Minimum return Data_Sample;
   -- Returns the minimum value of the samples that have been added to the data
   -- accumulators.

   function Maximum return Data_Sample;
   -- Returns the maximum value of the samples that have been added to the data
   -- accumulators.

   function Frequency (Data : in Data_Sample) return Natural;
   -- Returns the number of occurrances of of samples entered of the specified
   -- Value in the data. return values can be used to build a histogram.

   Insufficient_Samples : Exception;
   -- Raised if Mean or Variance is called when Count is less than two.

end Statistics;
