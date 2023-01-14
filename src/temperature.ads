-- This package continuously samples the tank and panel temperatures. The
-- package exports average temperatures which have been corrected by the
-- configuration temperature calibration parameters.
-- Author    : David Haley
-- Created   : 20/10/2017
-- Last Edit : 24/10/2017

with Pump_Controller_Types; use Pump_Controller_Types;

package Temperature is

   procedure Average_Temperature (Tank : out Temperatures;
                                  Panel : out Temperatures);

   procedure Reset_Progress;
   -- Resets internal progress indicators

   procedure Samplmpling_Progressing;
   -- Returns when progress is indicated. Criteria half buffer has been
   -- filled and both averages have been calculated.

   procedure Stop_Sampling_Temperature;
   -- Stops sampling process prior to shutdown

end Temperature;
