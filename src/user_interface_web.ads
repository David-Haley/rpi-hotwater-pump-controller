-- This package provides the web based user interface. It is served directly
-- by hot_water_controller: status is read via User_Interface_Server.UI_Server
-- and commands (clear faults, manual boost) are issued directly to
-- Global_Data. There is no separate wire protocol or standalone client
-- program any more.
-- Author    : David Haley
-- Created   : 06/08/2026
-- Last Edit : 18/08/2026

-- 20260818 : HTTP layer replaced by AWS (Ada Web Server), removing the
-- hand-rolled GNAT.Sockets accept loop, connection queue and worker pool.
-- AWS owns its own tasking, so Web_UI is no longer a task: Start_Web_UI /
-- Stop_Web_UI are now plain procedures wrapping AWS.Server.Start / Shutdown.
-- 20260816 : De-genericised and integrated directly into
-- hot_water_controller; Run_UI replaced by the Web_UI task starting and
-- stopping itself, matching User_Interface_Server / Data_Logger.

package User_Interface_Web is

   procedure Start_Web_UI;

   procedure Stop_Web_UI;

end User_Interface_Web;
