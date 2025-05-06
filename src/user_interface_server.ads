-- This package provides server component for the user interface.
-- Author    : David Haley
-- Created   : 29/10/2017
-- Last Edit : 07/05/2025

-- 20250507 : Start_User_Interface remoced to reduce potential for startup
-- deadlock.
-- 20230916 : UI_Server declaration moved here Start and Stop added;
-- 20220523 : Stop_User_Interface added.

package User_Interface_Server is

   task UI_Server is
      entry Stop;
   end UI_Server;

   procedure Stop_User_Interface renames UI_Server.Stop;

end User_Interface_Server;
