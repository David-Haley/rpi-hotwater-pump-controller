-- This package provides an asynchronous interface to the DFRobot 0555
-- (2 x 16) display. The purpose of asynchronous interface is to allow the hot
-- water controller to carry out its primary function with the LCD display
-- either not present or failed.

-- Author:    David Haley
-- Created:   12/10/2025
-- Last Edit: 12/10/2025

with DFR0555_display; use DFR0555_display;

package Local_Display is

   subtype LCD_Strings is Display_Strings;
    
   procedure Clear_LCD;
   -- Enqueues request to clear the display.
   
   procedure Put_LCD (Line_1, Line_2 : in LCD_Strings);
   -- Enqueues request to update both lines of the display
   
   procedure Put_LCD_Line_1 (Line : in LCD_Strings);
   -- Enqueues request to update the top line of the display
   
   procedure Put_LCD_Line_2 (Line : in LCD_Strings);
   -- Enqueues request to update the bottom line of the display
   
   procedure Stop_LCD;
   -- Enqueses request to blank the display and terminate the task supporting
   -- the display.

end Local_Display;
