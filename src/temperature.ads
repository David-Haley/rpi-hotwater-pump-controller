-- This package continuously samples the tank and panel temperatures. The
-- package exports average temperatures which have been corrected by the
-- configuration temperature calibration parameters.
-- Author    : David Haley
-- Created   : 20/10/2017
-- Last Edit : 16/09/2023
-- 20230916 :  Sample_Temperature declaration moved to specification.

with Pump_Controller_Types; use Pump_Controller_Types;

package Temperature is

   procedure Average_Temperature (Tank : out Temperatures;
                                  Panel : out Temperatures);

   procedure Reset_Progress;
   -- Resets internal progress indicators

   procedure Samplmpling_Progressing;
   -- Returns when progress is indicated. Criteria half buffer has been
   -- filled and both averages have been calculated.

   task Sample_Temperature is
      entry Stop;
   end Sample_Temperature;

   procedure Stop_Sampling_Temperature renames Sample_Temperature.Stop;
   -- Stops sampling process prior to shutdown

end Temperature;
