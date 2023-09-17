-- This package provides for periodic operation of the boost element of for the
-- purposes of sanitising (killing Leagionella) and for comfort.
-- Author    : David Haley
-- Created   : 04/04/2019
-- Last Edit : 16/09/2023
-- 20230916 : Boost task renamed to Boost_Task to avoid conflict with package
-- name.
-- 20220715 : Indirect call to Boost entries converted to renames.
-- 20220531 : Next_Boost_Time made available
-- 20220523 : Descrioption of package changed.
-- 20210306 : Start_Boost added
-- 20190405 : Boost_Exception added

with Ada.Calendar; use Ada.Calendar;
with Ada.Exceptions; use Ada.Exceptions;

package Boost is

   task Boost_Task is
      -- Boost task declaration
      entry Start_Boost;
      entry Stop_Boost;
   end Boost_Task;

   procedure Start_Boost renames Boost_Task.Start_Boost;
   -- Starts Boost task running

   procedure Stop_Boost renames Boost_Task.Stop_Boost;
   -- Termibates Boost task
   
   function Next_Boost_Time (T : in Time) return time;
   -- returns a time where the hour is the Sanitise_Hour as specified in the
   -- configuration file. This time can be in the past if the hour in T is
   -- greater than the hour the Sanitise_Hour.

   Boost_Failed : Exception;

end Boost;
