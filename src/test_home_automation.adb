--  This program allows the home automation code and configuration to be tested
--  independent of the boost logic.

--  Author :    David Haley
--  Created :   17/07/2026
--  Last Edit : 17/07/2026

with Ada.Text_IO; use Ada.Text_IO;
with DJH.Events_and_Errors; use DJH.Events_and_Errors;
with Home_Automation; use Home_Automation;

procedure Test_Home_Automation is

   Program_Name : constant String := "Test Home Automation";
   Version : constant String := "20260717";
   Command : Character := '?';
   
begin --  Test_Home_Automation
   Put_Line (Program_Name & " version " & Version);
   Put_Event (Program_Name & " version " & Version & " started");
   while Command /= 'q' and Command /= 'Q' loop
      case Command is
         when '?' =>
            Put_Line ("1 : Turn boost on");
            Put_Line ("0 : Turn Boost off");
            Put_Line ("Q : Quit");
         when '1' =>
            Put_Line ("Command was boost on");
            Put_Event ("Boost on request");
            if Request_Boost_On then
               Put_Event ("On request successful");
               Put_Line ("On request successful");
            else
               Put_Event ("On request failed");
               Put_Line ("On request failed");
            end if; -- Request_Boost_On
         when '0' =>
            Put_Line ("Command was boost off");
            Put_Event ("Boost off request");
            if Request_Boost_Off then
               Put_Event ("Off request successful");
               Put_Line ("Off request successful");
            else
               Put_Event ("Off request failed");
               Put_Line ("Off request failed");
            end if; -- Request_Boost_Off
         when others =>
            Put_Line ("Invalid command : '" & Command & "'");
      end case; -- Command
      Put ("Command: ");
      Get_Immediate (Command);
      New_Line;
   end loop; -- Command /= 'q' and Command /= 'Q'
   Put_Event (Program_Name & " normal exit");
   Put_Line (Program_Name & " normal exit");
   Stop_Events;
end Test_Home_Automation;