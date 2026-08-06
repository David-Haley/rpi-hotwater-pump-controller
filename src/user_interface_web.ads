-- This package provides a web based client component for a distributed user
-- interface. It provides equivalent functionality to User_Interface_Client
-- (status display and command entry) but rendered as HTML pages served over
-- HTTP rather than as an ANSI terminal display.
-- Author    : David Haley
-- Created   : 06/08/2026

with GNAT.Sockets;

generic

   Controller_Name : String;

package User_Interface_Web is

   procedure Run_UI (HTTP_Port : in GNAT.Sockets.Port_Type := 8080);
   -- Starts the HTTP server on HTTP_Port and services browser requests,
   -- relaying status and command requests to the controller via the same
   -- protocol used by User_Interface_Client. Does not return.

end User_Interface_Web;
