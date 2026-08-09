--  This package provides server component for the user interface.
--  Author    : David Haley
--  Created   : 29/10/2017
--  Last Edit : 09/08/2026

--  20260809 : Get_Status added.
--  20250507 : Start_User_Interface remoced to reduce potential for startup
--  deadlock.
--  20230916 : UI_Server declaration moved here Start and Stop added;
--  20220523 : Stop_User_Interface added.

with Global_Data; use Global_Data;

package User_Interface_Server is

   task UI_Server is
      entry Get_Status (Status : out Status_Records);
      entry Stop;
   end UI_Server;

   procedure Stop_User_Interface renames UI_Server.Stop;

end User_Interface_Server;
