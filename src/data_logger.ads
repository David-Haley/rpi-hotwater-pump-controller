-- This package provides data logging for the Pump controller
-- Author    : David Haley
-- Created   : 21/10/2017
-- Last Edit : 15/07/2022
-- 20220715 : Indirect calls to Logger.Start and Stop_Loggerconverted to renames
-- 20210220 : Event and error management removed to Events_and_Errors.
-- Start_Logger added.
-- 20190404 : Export of On_The_Hour
-- 20190307 : Export of Put_Error and Put_Event, redirection of standard error
-- and standard output managed here.
-- 20190216 : Read_File_Commit_Time added.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;

package Data_Logger is

   task Logger is
      -- Logger task declaration
      entry Start;
      entry Stop;
   end Logger;

   procedure Start_Logger renames Logger.Start;
   -- Starts data logging.

   procedure Stop_Logger renames Logger.Stop;
   -- Stops data logging

   function Read_File_Commit_Time return Time;
   -- Returns time of next file commit, that is, Flush (xx)

   function On_The_Hour (T : in Time) return Time;
   -- Effectively rounds T down such that minutes and seconds are zero

end Data_Logger;
