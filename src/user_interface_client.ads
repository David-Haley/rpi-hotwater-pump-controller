-- This package provides the client component for a locally distibuted user
-- interface.
-- Author    : David Haley
-- Created   : 29/10/2017
-- Last Edit : 23/07/2021
-- 20210723 : Command reading made internal
-- 20210305 : made generic
-- 20210304 : procedure Send_Command replaces Process_Task declaration.
-- 20171106 Converted to UDP

with Shared_User_Interface; use Shared_User_Interface;

generic

   Controller_Name : String;

package User_Interface_Client is

   procedure Run_UI;

   Version_Mismatch : exception;

end User_Interface_Client;
