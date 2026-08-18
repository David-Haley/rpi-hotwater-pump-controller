-- This package provides data logging for the Pump controller
-- Author    : David Haley
-- Created   : 21/10/2017
-- Last Edit : 08/08/2026

--  20260808 : File commit timing logic moved to Global_Data
--  20260619 : Compilerwarnings removed.
-- 20250506 : Start_Logger removed, to avoid startup deadlock.
-- 20220715 : Indirect calls to Logger.Start and Stop_Loggerconverted to renames
-- 20210220 : Event and error management removed to Events_and_Errors.
-- Start_Logger added.
-- 20190404 : Export of On_The_Hour
-- 20190307 : Export of Put_Error and Put_Event, redirection of standard error
-- and standard output managed here.
-- 20190216 : Read_File_Commit_Time added.

package Data_Logger is

   task Logger is
      -- Logger task declaration
      entry Stop;
   end Logger;

   procedure Stop_Logger renames Logger.Stop;
   -- Stops data logging

end Data_Logger;
