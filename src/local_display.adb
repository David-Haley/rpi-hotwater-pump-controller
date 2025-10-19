-- This package provides an asynchronous interface to the DFRobot 0555
-- (2 x 16) display. The purpose of asynchronous interface is to allow the hot
-- water controller to carry out its primary function with the LCD display
-- either not present or failed.

-- Author:    David Haley
-- Created:   12/10/2025
-- Last Edit: 12/10/2025

with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Configuration; use Configuration;
with DJH.Events_and_Errors; use DJH.Events_and_Errors;
with DFR0555_display; use DFR0555_display;

package body Local_Display is

   type Request_Types is (LCD_Clear, LCD_1_2, LCD_1, LCD_2, LCD_Stop);
   
   type Requests  (Request_Type : Request_Types) is record
      case Request_Type is
      when LCD_Clear | LCD_Stop =>
         null;
      when LCD_1_2 =>
         Line_1, Line_2 : Display_Strings;
      when LCD_1 | LCD_2 =>
         Line : Display_Strings;
      end case; -- Request_Type
   end record; -- Requests
   
   package Queue_Holders is new Ada.Containers.Indefinite_Holders (Requests);
   use Queue_Holders;
   
   package Request_Int is new
     Ada.Containers.Synchronized_Queue_Interfaces (Queue_Holders.Holder);
   package Request_Queues is new
     Ada.Containers.Unbounded_Synchronized_Queues (Request_Int);
   use Request_Queues;
   
   Request_Queue : Request_Queues.Queue;
    
   procedure Clear_LCD is

      -- Enqueues request to clear the display.

      Request : Requests (LCD_Clear);
      
   begin -- Clear_LCD
      Request_Queue.Enqueue (To_Holder (Request));
   end Clear_LCD;
   
   procedure Put_LCD (Line_1, Line_2 : in LCD_Strings) is
   
      -- Enqueues request to update both lines of the display

      Request : Requests (LCD_1_2) := (LCD_1_2, Line_1, Line_2);
      
   begin -- Put_LCD
      Request_Queue.Enqueue (To_Holder (Request));
   end Put_LCD;
   
   procedure Put_LCD_Line_1 (Line : in LCD_Strings) is
   
      -- Enqueues request to update the top line of the display

      Request : Requests (LCD_1) := (LCD_1, Line);
      
   begin -- Put_LCD_Line_1
      Request_Queue.Enqueue (To_Holder (Request));
   end Put_LCD_Line_1;
   
   procedure Put_LCD_Line_2 (Line : in LCD_Strings) is
   
      -- Enqueues request to update the bottom line of the display

      Request : Requests (LCD_2) := (LCD_2, Line);
      
   begin -- Put_LCD_Line_2
      Request_Queue.Enqueue (To_Holder (Request));
   end Put_LCD_Line_2;
   
   procedure Stop_LCD is
   
      -- Enqueses request to blank the display and terminate the task supporting
      -- the display.

      Request : Requests (LCD_Stop);
      
   begin --  Stop_LCD
      Request_Queue.Enqueue (To_Holder (Request));
   end Stop_LCD;
   
   task Process_Requests is
   end Process_Requests;
   
   task body Process_Requests is
   
   Ready : Boolean := False;
   Run : Boolean := True;
   Request : Holder;
   
   begin -- Process_Requests
      select
         delay 60.0;
         then abort
         begin -- Initialise LCD display exception block
            Enable_Display;
            Clear;
            Set_Brightness (LCD_Brightness);
            Backlight_On;
            Ready := True;
         exception
            when E: others =>
               Put_Error ("Local_Display initialisation failed", E);
            Ready := False;
         end; -- Initialise LCD display exception block
      end select;
      loop -- process one request
         Request_Queue.Dequeue (Request);
         if Ready then
            select
               delay 0.5;
               -- to ensure the Request_Queue does not grow indefinitely the
               -- queue needs to be dequeued at a faster rate than the main
               -- loop.
               then abort
               begin -- processing exception block
                  Ready := False;
                  case Element (Request).Request_Type is
                     when LCD_Clear =>
                        Clear;
                     when LCD_1_2 =>
                        Put_Line (0, Element (Request).Line_1);
                        Put_Line (1, Element (Request).Line_2);
                     when LCD_1 =>
                        Put_Line (0, Element (Request).Line);
                     when LCD_2 =>
                        Put_Line (1, Element (Request).Line);
                     when LCD_Stop =>
                        Disable_Display;
                  end case; -- Element (Request).Request_Type
                  Ready := True;
               exception
                  when E: others =>
                     Put_Error ("Process_Request error", E);
                     Ready := False;
               end; -- processing exception block
            end select;
         end if; -- Ready
         exit when Element (Request).Request_Type = LCD_Stop;
      end loop; -- process one request
   end Process_Requests;

end Local_Display;
