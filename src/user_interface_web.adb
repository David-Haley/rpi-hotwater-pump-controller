-- This package provides the web based user interface. It renders the same
-- information and commands that pump_ui / pump_web used to provide over a
-- separate UDP protocol, but as HTML served over HTTP, reading and writing
-- Global_Data directly since it now runs in the same process as the
-- controller. The equivalent of the old terminal commands are provided as
-- follows: status is refreshed on every page load / auto refresh;
-- 'c' (Clear_Fault_Table) and 'm' (Manual_Boost) are provided as web forms;
-- 'e' (Exit_User_Interface) has no web equivalent, the server simply runs
-- until the controller is stopped via systemd/ctrl c.
-- Author    : David Haley
-- Created   : 06/08/2026
-- Last Edit : 18/08/2026

-- 20260818 : HTTP serving moved to AWS (Ada Web Server), replacing the
-- hand-rolled GNAT.Sockets accept loop, connection queue and worker pool
-- with a single Dispatch callback; AWS owns request parsing, connection
-- pooling and shutdown, so Web_UI is no longer a task.
-- 20260819 : Corrections to command button operations.
-- 20260816 : De-genericised and integrated directly into
-- hot_water_controller. Status is read via
-- User_Interface_Server.UI_Server.Get_Status and commands are issued
-- directly to Global_Data instead of round tripping over UDP. Manual_Boost
-- now clamps the requested time between Clock and the existing
-- Mandatory_Boost_Time instead of overwriting Mandatory_Boost_Time.

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with AWS.Server;
with AWS.Status; use AWS.Status;
with AWS.Response;
with AWS.Parameters;
with AWS.Messages;
with Machine_Properties;
with Pump_Controller_Types; use Pump_Controller_Types;
with Configuration; use Configuration;
with Global_Data; use Global_Data;
with User_Interface_Server; use User_Interface_Server;
with DJH.Date_and_Time_Strings; use DJH.Date_and_Time_Strings;
with DJH.Events_and_Errors; use DJH.Events_and_Errors;

package body User_Interface_Web is

   HTTP_Port : constant := 8080;
   Max_Connection : constant := 4;
   Status_Refresh_Seconds : constant := 2;

   CSS : constant String :=
     "body{font-family:system-ui,sans-serif;margin:0 auto;max-width:640px;" &
     "padding:1rem;background:#10161c;color:#e6edf3}" &
     "h1{font-size:1.3rem}h2{font-size:1rem;margin-top:1.5rem}" &
     ".meta{color:#9aa7b2;font-size:0.85rem}" &
     ".grid{display:grid;grid-template-columns:" &
     "repeat(auto-fit,minmax(150px,1fr));gap:0.6rem}" &
     ".card{background:#1b2430;border-radius:8px;padding:0.6rem 0.8rem}" &
     ".card .label{font-size:0.75rem;color:#9aa7b2}" &
     ".card .value{font-size:1.1rem;font-weight:600}" &
     ".faults{display:flex;flex-wrap:wrap;gap:0.4rem}" &
     ".badge{display:inline-block;padding:0.2rem 0.6rem;border-radius:999px;" &
     "font-size:0.8rem}" &
     ".badge.good{background:#1e4620;color:#7ee787}" &
     ".badge.bad{background:#4a1f24;color:#ff9492}" &
     ".badge.idle{background:#33404d;color:#c9d1d9}" &
     ".commands{display:flex;gap:0.6rem;margin-top:0.6rem;flex-wrap:wrap}" &
     ".commands form{margin:0}" &
     "button,.button{background:#2f6feb;color:#fff;border:none;" &
     "border-radius:6px;padding:0.5rem 1rem;font-size:0.9rem;" &
     "text-decoration:none;cursor:pointer}" &
     "input[type=date]{padding:0.4rem;border-radius:6px;" &
     "border:1px solid #33404d;background:#1b2430;color:#e6edf3}" &
     ".error{color:#ff9492}.ok{color:#7ee787}";

   -------------------------------------------------------------------------
   -- Direct calls into Global_Data / User_Interface_Server, replacing the
   -- old UDP round trip.
   -------------------------------------------------------------------------

   function Fetch_Status (Status : out Status_Records) return Boolean is

   begin -- Fetch_Status
      UI_Server.Get_Status (Status);
      return True;
   exception
      when others =>
         -- Most likely UI_Server has already terminated during shutdown.
         return False;
   end Fetch_Status;

   function Clear_Faults return Boolean is

   begin -- Clear_Faults
      for F in Fault_Types loop
         Global_Data.Clear_Fault (F);
      end loop; -- F in Fault_Types
      Put_Event ("Fault table cleared");
      return True;
   exception
      when others =>
         return False;
   end Clear_Faults;

   function Request_Boost (Requested_Time : in Time) return Boolean is

      Current : constant Boost_Times := Global_Data.Next_Boost;
      Clamped_Time : Time := Requested_Time;

   begin -- Request_Boost
      if Clamped_Time < Clock then
         Clamped_Time := Clock;
      elsif Clamped_Time > Current.Mandatory_Boost_Time then
         Clamped_Time := Current.Mandatory_Boost_Time;
      end if; -- Clamped_Time < Clock
      Global_Data.Write_Next_Boost_Time
        ((Next_Boost_Time => Clamped_Time,
          Mandatory_Boost_Time => Current.Mandatory_Boost_Time));
      return True;
   exception
      when others =>
         return False;
   end Request_Boost;

   -------------------------------------------------------------------------
   -- HTML rendering
   -------------------------------------------------------------------------

   function Format_Temperature (T : in Temperatures) return String is

      Buffer : String (1 .. 8);

   begin -- Format_Temperature
      Temperature_IO.Put (Buffer, T, Aft => 1, Exp => 0);
      return Trim (Buffer, Left);
   end Format_Temperature;

   function Format_Difference (D : in Temperature_Differences) return String is

      Buffer : String (1 .. 8);

   begin -- Format_Difference
      Temperature_Difference_IO.Put (Buffer, D, Aft => 1, Exp => 0);
      return Trim (Buffer, Left);
   end Format_Difference;

   function Iso_Date (T : in Time) return String is

      function Pad (N : in Natural) return String is

         Image : constant String := Trim (N'Image, Left);

      begin -- Pad
         if Image'Length = 1 then
            return "0" & Image;
         else
            return Image;
         end if; -- Image'Length = 1
      end Pad;

   begin -- Iso_Date
      return Trim (Ada.Calendar.Formatting.Year (T)'Image, Left) & "-" &
        Pad (Ada.Calendar.Formatting.Month (T)) & "-" &
        Pad (Ada.Calendar.Formatting.Day (T));
   end Iso_Date;

   function Page_Header (Title : in String; Refresh_Seconds : in Natural := 0)
                         return String is

      -- A meta refresh runs on the browser's navigation timer, independent
      -- of JavaScript, so it fires even while a blocking confirm() dialog
      -- (e.g. Clear Fault Table) is open, dismissing the dialog before the
      -- user can answer it. A setTimeout-driven reload queues behind the
      -- same JS event loop that confirm() blocks, so it waits until the
      -- dialog is answered. If the delay elapsed while the dialog was open,
      -- the reload becomes due the instant the user answers it, and can
      -- then race the browser's own navigation to the form's POST target,
      -- aborting the submission; Auto_Refresh_Suspended, set by the form's
      -- onsubmit handler once the user confirms, guards against that.
      Refresh_Tag : constant String :=
        (if Refresh_Seconds > 0 then
           "<script>var Auto_Refresh_Suspended=false;" &
             "setTimeout(function(){if(!Auto_Refresh_Suspended)" &
             "location.reload();}," &
             Trim (Natural'Image (Refresh_Seconds * 1000), Left) &
             ");</script>"
         else "");

   begin -- Page_Header
      return
        "<!DOCTYPE html><html><head><meta charset=""utf-8"">" &
        "<meta name=""viewport"" " &
        "content=""width=device-width, initial-scale=1"">" & Refresh_Tag &
        "<title>" & Title & "</title><style>" & CSS &
        "</style></head><body><h1>" & Title & "</h1>";
   end Page_Header;

   Page_Footer : constant String := "</body></html>";

   function Card (Label, Value : in String) return String is

   begin -- Card
      return "<div class=""card""><div class=""label"">" & Label &
        "</div><div class=""value"">" & Value & "</div></div>";
   end Card;

   function Render_Status_Page return Unbounded_String is

      Status : Status_Records;
      Success : constant Boolean := Fetch_Status (Status);
      Result : Unbounded_String;

   begin -- Render_Status_Page
      Result := To_Unbounded_String
        (Page_Header ("Hot Water Pump Controller", Status_Refresh_Seconds));
      if not Success then
         Append (Result, "<p class=""error"">Controller status temporarily " &
                   "unavailable, retrying automatically.</p>");
      else
         declare
            Diff : constant Temperature_Differences :=
              Status.Panel_Temperature - Status.Tank_Temperature;
         begin
            Append (Result, "<p class=""meta"">Controller " &
                      Machine_Properties.Machine_Name & " &mdash; version " &
                      Status.Controller_Version & " &mdash; controller time " &
                      Time_String (Status.Controller_Time) & "</p>");
            Append (Result, "<div class=""grid"">");
            Append (Result, Card ("Panel Temperature",
                    Format_Temperature (Status.Panel_Temperature) &
                      " &deg;C"));
            Append (Result, Card ("Tank Temperature",
                    Format_Temperature (Status.Tank_Temperature) &
                      " &deg;C"));
            Append (Result, Card ("Temperature Difference",
                    Format_Difference (Diff) & " &deg;C"));
            Append (Result, Card ("Average Difference",
                    Format_Difference (Status.Average_Difference) &
                      " &deg;C"));
            Append (Result, Card ("Pump",
                    (if Status.Pump_Run then
                        "<span class=""badge good"">Running</span>"
                     else "<span class=""badge idle"">Idle</span>")));
            Append (Result, Card ("Comfort",
                    (if Status.Is_Comfortable then
                        "<span class=""badge good"">Comfortable</span>"
                     else "<span class=""badge bad"">Cold</span>")));
            Append (Result, Card ("Pump Run Time",
                    Elapsed_Seconds (Status.Pump_Run_Time, Exclude_Days)));
            Append (Result, Card ("Previous Run",
                    Elapsed_Seconds (Status.Previous_Run_Duration,
                                     Exclude_Days) & " at " &
                      Time_String (Status.Previous_Run_Time)));
            Append (Result, Card ("Total Pump Run Time",
                    Elapsed_Seconds (Status.Accumulated_Pump_Run_Time,
                                     Hours_More_Than_24)));
            Append (Result, Card ("Controller Up Time",
                    Elapsed_Seconds (Status.Controller_Up_Time,
                                     Include_Days)));
            Append (Result, Card ("Next Boost",
                    Image (Status.Next_Boost_Time.Next_Boost_Time, False,
                          UTC_Time_Offset)));
            Append (Result, Card ("Next Log Commit",
                    Time_String (Status.Next_File_Commit_Time)));
            Append (Result, "</div>");
            Append (Result, "<h2>Fault Annunciators</h2><div class=""faults"">");
            for Fault_Index in Fault_Types loop
               declare
                  Label : constant String :=
                    (case Fault_Index is
                        when Accumulated_Time_File => "Pump Log",
                        when Log_File => "Data Log",
                        when Tank_Temperature => "Tank Temperature",
                        when Boost_Failure => "Auto Boost");
                  Faulted : constant Boolean := Status.Fault_Table (Fault_Index);
               begin
                  Append (Result, "<span class=""badge " &
                            (if Faulted then "bad" else "good") & """>" &
                            Label & "</span>");
               end;
            end loop; -- Fault_Index in Fault_Types
            Append (Result, "</div>");
         end;
      end if; -- not Success
      Append (Result, "<h2>Commands</h2><div class=""commands"">");
      Append (Result, "<form method=""post"" action=""/clear_fault_table""" &
                " onsubmit=""if(confirm('Clear fault table?'))" &
                "{Auto_Refresh_Suspended=true;return true;}return false;""" &
                "><button type=""submit"">Clear Fault Table</button></form>");
      Append (Result, "<a class=""button"" href=""/manual_boost"">" &
                "Manual Boost</a>");
      Append (Result, "</div>");
      Append (Result, Page_Footer);
      return Result;
   end Render_Status_Page;

   function Render_Boost_Form return Unbounded_String is

      Result : Unbounded_String := To_Unbounded_String (Page_Header
        ("Manual Boost"));

   begin -- Render_Boost_Form
      Append (Result, "<p>Enter the date on which the mandatory boost " &
                "should next occur.</p>");
      Append (Result, "<form method=""post"" action=""/manual_boost"">" &
                "<input type=""date"" name=""date"" value=""" &
                Iso_Date (Clock) & """ required> " &
                "<button type=""submit"">Set Boost Date</button></form>");
      Append (Result, "<p><a href=""/"">Back to status</a></p>");
      Append (Result, Page_Footer);
      return Result;
   end Render_Boost_Form;

   function Render_Boost_Result (Request : in AWS.Status.Data)
                                 return Unbounded_String is

      Date_Field : constant String :=
        AWS.Parameters.Get (AWS.Status.Parameters (Request), "date");
      Result : Unbounded_String := To_Unbounded_String (Page_Header
        ("Manual Boost"));

   begin -- Render_Boost_Result
      if Date_Field'Length = 10 and then Date_Field (Date_Field'First + 4) = '-'
        and then Date_Field (Date_Field'First + 7) = '-' then
         declare
            Base : constant Natural := Date_Field'First - 1;
            Requested_Year : constant Year_Number :=
              Year_Number'Value (Date_Field (Base + 1 .. Base + 4));
            Requested_Month : constant Month_Number :=
              Month_Number'Value (Date_Field (Base + 6 .. Base + 7));
            Requested_Day : constant Day_Number :=
              Day_Number'Value (Date_Field (Base + 9 .. Base + 10));
            Requested_Time : constant Time :=
              Ada.Calendar.Formatting.Time_Of
                (Requested_Year, Requested_Month, Requested_Day, Boost_Hour,
                 0, 0, 0.0, False, UTC_Time_Offset (Clock));
            Success : constant Boolean := Request_Boost (Requested_Time);
         begin
            if Success then
               Append (Result, "<p class=""ok"">Boost date updated, " &
                         "requested " & Date_Field &
                         "; the actual time is kept between now and the " &
                         "mandatory boost time.</p>");
            else
               Append (Result, "<p class=""error"">Boost date not " &
                         "updated.</p>");
            end if; -- Success
         exception
            when others =>
               Append (Result, "<p class=""error"">Invalid date " &
                         "supplied.</p>");
         end;
      else
         Append (Result, "<p class=""error"">Invalid date supplied.</p>");
      end if; -- Date_Field'Length = 10 and then ...
      Append (Result, "<p><a href=""/"">Back to status</a></p>");
      Append (Result, Page_Footer);
      return Result;
   end Render_Boost_Result;

   function Render_Clear_Result return Unbounded_String is

      Success : constant Boolean := Clear_Faults;
      Result : Unbounded_String := To_Unbounded_String (Page_Header
        ("Clear Fault Table"));

   begin -- Render_Clear_Result
      if Success then
         Append (Result, "<p class=""ok"">Fault table cleared.</p>");
      else
         Append (Result, "<p class=""error"">Fault table not cleared.</p>");
      end if; -- Success
      Append (Result, "<p><a href=""/"">Back to status</a></p>");
      Append (Result, Page_Footer);
      return Result;
   end Render_Clear_Result;

   -------------------------------------------------------------------------
   -- HTTP dispatch and server lifecycle (AWS owns request parsing,
   -- connection pooling and the accept loop).
   -------------------------------------------------------------------------

   function Dispatch (Request : in AWS.Status.Data) return AWS.Response.Data
   is

      Method : constant AWS.Status.Request_Method := AWS.Status.Method
        (Request);
      URI : constant String := AWS.Status.URI (Request);

   begin -- Dispatch
      if Method = AWS.Status.GET and then URI = "/" then
         return AWS.Response.Build
           ("text/html; charset=utf-8", Render_Status_Page,
            Cache_Control => AWS.Messages.Prevent_Cache);
      elsif Method = AWS.Status.GET and then URI = "/manual_boost" then
         return AWS.Response.Build
           ("text/html; charset=utf-8", Render_Boost_Form,
            Cache_Control => AWS.Messages.Prevent_Cache);
      elsif Method = AWS.Status.POST and then URI = "/manual_boost" then
         return AWS.Response.Build
           ("text/html; charset=utf-8", Render_Boost_Result (Request),
            Cache_Control => AWS.Messages.Prevent_Cache);
      elsif Method = AWS.Status.POST and then URI = "/clear_fault_table" then
         return AWS.Response.Build
           ("text/html; charset=utf-8", Render_Clear_Result,
            Cache_Control => AWS.Messages.Prevent_Cache);
      else
         return AWS.Response.Build
           ("text/html", "<h1>Not Found</h1>", Status_Code => AWS.Messages.S404);
      end if; -- Method = AWS.Status.GET and then URI = "/"
   end Dispatch;

   WS : AWS.Server.HTTP;

   procedure Start_Web_UI is

   begin -- Start_Web_UI
      AWS.Server.Start
        (WS, "Hot Water Pump Controller", Callback => Dispatch'Access,
         Port => HTTP_Port, Max_Connection => Max_Connection);
      Put_Event ("Web_UI listening on port" & Natural'Image (HTTP_Port));
   exception
      when Event : others =>
         Put_Error ("Web_UI", Event);
   end Start_Web_UI;

   procedure Stop_Web_UI is

   begin -- Stop_Web_UI
      AWS.Server.Shutdown (WS);
      Put_Event ("Web_UI Stopped");
   exception
      when Event : others =>
         Put_Error ("Web_UI", Event);
   end Stop_Web_UI;

end User_Interface_Web;
