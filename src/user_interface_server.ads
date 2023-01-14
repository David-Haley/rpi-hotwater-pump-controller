-- This package provides server component for the user interface.
-- Author    : David Haley
-- Created   : 29/10/2017
-- Last Edit : 23/05/2022
-- 20220523 : Stop_User_Interface added.

package User_Interface_Server is

   function Run_Controller return Boolean;
   -- Returns True when the user interface is running, returns false after a
   -- either Stop_User_Interface is called or a stop request has been received
   -- via the user interface.
   
   procedure Stop_User_Interface;
   -- Allows internal shutdown of user interface

end User_Interface_Server;
