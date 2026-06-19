-- This package provides client component for a distributed user interface.
-- It provides a display of the current status and allows commands to be sent to
-- the controller programme.

-- Author    : David Haley
-- Created   : 29/10/2017
-- Last Edit : 19/06/2026

--  20260619 : Compiler warnings removed.
-- 20230916 : Descriptive comment above updated.
-- 20210723 : Command reading made internal
-- 20210305 : made generic
-- 20210304 : procedure Send_Command replaces Process_Task declaration.
-- 20171106 Converted to UDP

generic

   Controller_Name : String;

package User_Interface_Client is

   procedure Run_UI;

   Version_Mismatch : exception;

end User_Interface_Client;
