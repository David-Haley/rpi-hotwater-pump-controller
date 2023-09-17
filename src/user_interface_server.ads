-- This package provides server component for the user interface.
-- Author    : David Haley
-- Created   : 29/10/2017
-- Last Edit : 17/09/2023
-- 20230916 : UI_Server declaration moved here Start and Stop added;
-- 20220523 : Stop_User_Interface added.

package User_Interface_Server is

   task UI_Server is
      entry Start;
      entry Stop;
   end UI_Server;

   procedure Start_User_Interface renames UI_Server.Start;

   procedure Stop_User_Interface renames UI_Server.Stop;

end User_Interface_Server;
