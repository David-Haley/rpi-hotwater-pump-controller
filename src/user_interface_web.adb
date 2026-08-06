-- This package provides web based client component for a distributed user
-- interface. It relays status and command requests to the controller using
-- exactly the same UDP protocol as User_Interface_Client, but renders the
-- result as HTML served over HTTP instead of drawing an ANSI terminal
-- screen. The equivalent of the terminal commands are provided as follows:
-- 'r' (Refresh_Screen) is implicit in every page load / auto refresh;
-- 'c' (Clear_Fault_Table) and 'm' (Manual_Boost) are provided as web forms;
-- 'e' (Exit_User_Interface) has no web equivalent, the server simply runs
-- until the process is stopped (matching how the controller itself is now
-- stopped via systemd rather than an in-band request).
-- Author    : David Haley
-- Created   : 06/08/2026

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Streams; use Ada.Streams;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Exceptions; use Ada.Exceptions;
with GNAT.Sockets; use GNAT.Sockets;
with Pump_Controller_Types; use Pump_Controller_Types;
with Shared_User_Interface; use Shared_User_Interface;
with DJH.Date_and_Time_Strings; use DJH.Date_and_Time_Strings;

package body User_Interface_Web is

   Queue_Capacity : constant := 8;
   Worker_Count : constant := 4;
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
   -- Controller_Link relays requests to the controller over UDP, using the
   -- same wire protocol as User_Interface_Client. A single task serialises
   -- access to the socket so any number of HTTP worker tasks can safely
   -- call it concurrently.
   -------------------------------------------------------------------------

   task Controller_Link is
      entry Fetch_Status (Status : out Status_Records;
                          Previous_Run : out Day_Seconds; Success : out Boolean);
      entry Clear_Faults (Success : out Boolean);
      entry Request_Boost (Requested_Time : in Time; Success : out Boolean);
   end Controller_Link;

   task body Controller_Link is

      Client_Socket : Socket_Type;

      Controller_Address : constant Sock_Addr_Type :=
        (Family => Family_Inet,
         Addr => Addresses (Get_Host_By_Name (Controller_Name), 1),
         Port => Server_Port);

      Client_Address : constant Sock_Addr_Type :=
        (Family => Family_Inet, Addr => Any_Inet_Addr, Port => Any_Port);

      Version_Mismatch : exception;

      function Round_Trip (Request_Record : in Request_Records)
                           return Status_Records is

         TX_Buffer : Request_Buffers;
         for TX_Buffer'Address use Request_Record'Address;
         pragma Import (Ada, TX_Buffer);
         TX_Last : Stream_Element_Offset;
         Status : Status_Records;
         RX_Buffer : Response_Buffers;
         for RX_Buffer'Address use Status'Address;
         pragma Import (Ada, RX_Buffer);
         RX_Last : Stream_Element_Offset;

      begin -- Round_Trip
         Send_Socket (Client_Socket, TX_Buffer, TX_Last, Controller_Address);
         Receive_Socket (Client_Socket, RX_Buffer, RX_Last);
         if Status.User_Interface_Version /= Interface_Version then
            raise Version_Mismatch with "Controller UI version " &
              Status.User_Interface_Version & " does not match " &
              Interface_Version;
         end if; -- Status.User_Interface_Version /= Interface_Version
         return Status;
      end Round_Trip;

      Last_Pump_Run_Time, Previous_Pump_Run_Time : Day_Seconds := 0;

   begin -- Controller_Link
      Create_Socket (Client_Socket, Family_Inet, Socket_Datagram);
      Set_Socket_Option (Client_Socket, Socket_Level, (Receive_Timeout, 5.0));
      Bind_Socket (Client_Socket, Client_Address);
      loop
         select
            accept Fetch_Status (Status : out Status_Records;
                                 Previous_Run : out Day_Seconds;
                                 Success : out Boolean) do
               declare
                  Request_Record : constant Request_Records :=
                    (Request => Get_Status,
                     User_Interface_Version => Interface_Version);
               begin
                  Status := Round_Trip (Request_Record);
                  if Status.Pump_Run_Time = 0 and Last_Pump_Run_Time > 0 then
                     Previous_Pump_Run_Time := Last_Pump_Run_Time;
                  end if; -- Status.Pump_Run_Time = 0 and Last_Pump_Run_Time > 0
                  Last_Pump_Run_Time := Status.Pump_Run_Time;
                  Success := True;
               exception
                  when others =>
                     Success := False;
               end;
               Previous_Run := Previous_Pump_Run_Time;
            end Fetch_Status;
         or
            accept Clear_Faults (Success : out Boolean) do
               declare
                  Request_Record : constant Request_Records :=
                    (Request => Clear_Fault_Table,
                     User_Interface_Version => Interface_Version);
                  Status : Status_Records;
                  pragma Unreferenced (Status);
               begin
                  Status := Round_Trip (Request_Record);
                  Success := True;
               exception
                  when others =>
                     Success := False;
               end;
            end Clear_Faults;
         or
            accept Request_Boost (Requested_Time : in Time;
                                  Success : out Boolean) do
               declare
                  Request_Record : constant Request_Records :=
                    (Request => Manual_Boost,
                     User_Interface_Version => Interface_Version,
                     User_Input => True,
                     Next_Boost_Time => (Next_Boost_Time => Requested_Time,
                                         Mandatory_Boost_Time => Requested_Time));
                  Status : Status_Records;
                  pragma Unreferenced (Status);
               begin
                  Status := Round_Trip (Request_Record);
                  Success := True;
               exception
                  when others =>
                     Success := False;
               end;
            end Request_Boost;
         end select;
      end loop; -- forever
   exception
      when E : others =>
         Put_Line ("Controller_Link - " & Exception_Message (E));
   end Controller_Link;

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

      Refresh_Tag : constant String :=
        (if Refresh_Seconds > 0 then
           "<meta http-equiv=""refresh"" content=""" &
             Trim (Refresh_Seconds'Image, Left) & """>"
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
      Previous_Run : Day_Seconds;
      Success : Boolean;
      Result : Unbounded_String;

   begin -- Render_Status_Page
      Controller_Link.Fetch_Status (Status, Previous_Run, Success);
      Result := To_Unbounded_String
        (Page_Header ("Hot Water Pump Controller", Status_Refresh_Seconds));
      if not Success then
         Append (Result, "<p class=""error"">No response from controller """
                   & Controller_Name & """, retrying automatically.</p>");
      else
         declare
            Diff : constant Temperature_Differences :=
              Status.Panel_Temperature - Status.Tank_Temperature;
         begin
            Append (Result, "<p class=""meta"">Controller " & Controller_Name
                      & " &mdash; version " & Status.Controller_Version &
                      " &mdash; controller time " &
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
            Append (Result, Card ("Previous Run Time",
                    Elapsed_Seconds (Previous_Run, Exclude_Days)));
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
                " onsubmit=""return confirm('Clear fault table?');"">" &
                "<button type=""submit"">Clear Fault Table</button></form>");
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

   function Render_Boost_Result (Form_Body : in String) return Unbounded_String;
   function Render_Clear_Result return Unbounded_String;

   -------------------------------------------------------------------------
   -- HTTP protocol helpers
   -------------------------------------------------------------------------

   function Read_Line (Stream : not null Stream_Access) return String is

      Buffer : Unbounded_String;
      Ch : Character;

   begin -- Read_Line
      loop
         Character'Read (Stream, Ch);
         exit when Ch = ASCII.LF;
         if Ch /= ASCII.CR then
            Append (Buffer, Ch);
         end if; -- Ch /= ASCII.CR
      end loop;
      return To_String (Buffer);
   exception
      when others =>
         return To_String (Buffer);
   end Read_Line;

   function Read_Body (Stream : not null Stream_Access; Length : in Natural)
                       return String is

      Buffer : Unbounded_String;
      Ch : Character;

   begin -- Read_Body
      for I in 1 .. Length loop
         Character'Read (Stream, Ch);
         Append (Buffer, Ch);
      end loop; -- I in 1 .. Length
      return To_String (Buffer);
   exception
      when others =>
         return To_String (Buffer);
   end Read_Body;

   function URL_Decode (Encoded : in String) return String is

      Result : Unbounded_String;
      I : Positive := Encoded'First;

   begin -- URL_Decode
      while I <= Encoded'Last loop
         case Encoded (I) is
            when '+' =>
               Append (Result, ' ');
               I := I + 1;
            when '%' =>
               if I + 2 <= Encoded'Last then
                  Append (Result, Character'Val (Integer'Value
                          ("16#" & Encoded (I + 1 .. I + 2) & "#")));
                  I := I + 3;
               else
                  I := I + 1;
               end if; -- I + 2 <= Encoded'Last
            when others =>
               Append (Result, Encoded (I));
               I := I + 1;
         end case; -- Encoded (I)
      end loop; -- I <= Encoded'Last
      return To_String (Result);
   exception
      when others =>
         return Encoded;
   end URL_Decode;

   function Get_Form_Value (Form_Body : in String; Key : in String)
                            return String is

      Search_Key : constant String := Key & "=";
      Start : Natural := 0;
      Stop : Natural;

   begin -- Get_Form_Value
      if Form_Body'Length >= Search_Key'Length then
         for I in Form_Body'First .. Form_Body'Last - Search_Key'Length + 1 loop
            if Form_Body (I .. I + Search_Key'Length - 1) = Search_Key then
               Start := I + Search_Key'Length;
               exit;
            end if; -- Form_Body (I .. I + Search_Key'Length - 1) = Search_Key
         end loop; -- I in Form_Body'First .. Form_Body'Last - Search_Key'Length + 1
      end if; -- Form_Body'Length >= Search_Key'Length
      if Start = 0 then
         return "";
      end if; -- Start = 0
      Stop := Start;
      while Stop <= Form_Body'Last and then Form_Body (Stop) /= '&' loop
         Stop := Stop + 1;
      end loop; -- Stop <= Form_Body'Last and then Form_Body (Stop) /= '&'
      return URL_Decode (Form_Body (Start .. Stop - 1));
   end Get_Form_Value;

   function Render_Boost_Result (Form_Body : in String) return Unbounded_String is

      Date_Field : constant String := Get_Form_Value (Form_Body, "date");
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
              Ada.Calendar.Formatting.Time_Of (Requested_Year, Requested_Month,
                                               Requested_Day);
            Success : Boolean;
         begin
            Controller_Link.Request_Boost (Requested_Time, Success);
            if Success then
               Append (Result, "<p class=""ok"">Boost date updated to " &
                         Date_Field & ".</p>");
            else
               Append (Result, "<p class=""error"">No response from " &
                         "controller, boost date not confirmed.</p>");
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

      Success : Boolean;
      Result : Unbounded_String := To_Unbounded_String (Page_Header
        ("Clear Fault Table"));

   begin -- Render_Clear_Result
      Controller_Link.Clear_Faults (Success);
      if Success then
         Append (Result, "<p class=""ok"">Fault table cleared.</p>");
      else
         Append (Result, "<p class=""error"">No response from " &
                   "controller.</p>");
      end if; -- Success
      Append (Result, "<p><a href=""/"">Back to status</a></p>");
      Append (Result, Page_Footer);
      return Result;
   end Render_Clear_Result;

   procedure Send_Response (Stream : not null Stream_Access;
                            Status_Line : in String; Body_Text : in String;
                            Content_Type : in String :=
                              "text/html; charset=utf-8") is

      Header : constant String :=
        Status_Line & ASCII.CR & ASCII.LF &
        "Content-Type: " & Content_Type & ASCII.CR & ASCII.LF &
        "Content-Length: " & Trim (Body_Text'Length'Image, Left) & ASCII.CR &
        ASCII.LF & "Connection: close" & ASCII.CR & ASCII.LF &
        "Cache-Control: no-store" & ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF;

   begin -- Send_Response
      String'Write (Stream, Header & Body_Text);
   end Send_Response;

   -------------------------------------------------------------------------
   -- Connection queue and worker pool
   -------------------------------------------------------------------------

   type Socket_Array is array (1 .. Queue_Capacity) of Socket_Type;

   protected Connection_Queue is
      entry Put (Socket : in Socket_Type);
      entry Get (Socket : out Socket_Type);
   private
      Sockets : Socket_Array;
      Head, Tail, Count : Natural := 0;
   end Connection_Queue;

   protected body Connection_Queue is

      entry Put (Socket : in Socket_Type) when Count < Queue_Capacity is
      begin -- Put
         Tail := Tail mod Queue_Capacity + 1;
         Sockets (Tail) := Socket;
         Count := Count + 1;
      end Put;

      entry Get (Socket : out Socket_Type) when Count > 0 is
      begin -- Get
         Head := Head mod Queue_Capacity + 1;
         Socket := Sockets (Head);
         Count := Count - 1;
      end Get;

   end Connection_Queue;

   procedure Handle_Connection (Socket : in Socket_Type) is

      Stream : constant Stream_Access := GNAT.Sockets.Stream (Socket);
      Request_Line : constant String := Read_Line (Stream);
      Space_1, Space_2 : Natural;

   begin -- Handle_Connection
      Space_1 := Index (Request_Line, " ");
      if Space_1 = 0 then
         Close_Socket (Socket);
         return;
      end if; -- Space_1 = 0
      Space_2 := Index (Request_Line (Space_1 + 1 .. Request_Line'Last), " ");
      if Space_2 = 0 then
         Close_Socket (Socket);
         return;
      end if; -- Space_2 = 0
      declare
         Method : constant String := Request_Line (Request_Line'First ..
                                                    Space_1 - 1);
         Path : constant String := Request_Line (Space_1 + 1 .. Space_2 - 1);
         Content_Length : Natural := 0;
      begin
         loop -- consume headers
            declare
               Header_Line : constant String := Read_Line (Stream);
            begin
               exit when Header_Line'Length = 0;
               if Header_Line'Length > 15 and then To_Upper
                 (Header_Line (Header_Line'First .. Header_Line'First + 14))
                 = "CONTENT-LENGTH:" then
                  Content_Length := Natural'Value (Trim
                    (Header_Line (Header_Line'First + 15 ..
                                  Header_Line'Last), Both));
               end if; -- Header_Line'Length > 15 and then ...
            end;
         end loop; -- consume headers
         declare
            Form_Body : constant String :=
              (if Content_Length > 0 then Read_Body (Stream, Content_Length)
               else "");
         begin
            if Method = "GET" and then Path = "/" then
               Send_Response (Stream, "HTTP/1.1 200 OK",
                              To_String (Render_Status_Page));
            elsif Method = "GET" and then Path = "/manual_boost" then
               Send_Response (Stream, "HTTP/1.1 200 OK",
                              To_String (Render_Boost_Form));
            elsif Method = "POST" and then Path = "/manual_boost" then
               Send_Response (Stream, "HTTP/1.1 200 OK",
                              To_String (Render_Boost_Result (Form_Body)));
            elsif Method = "POST" and then Path = "/clear_fault_table" then
               Send_Response (Stream, "HTTP/1.1 200 OK",
                              To_String (Render_Clear_Result));
            else
               Send_Response (Stream, "HTTP/1.1 404 Not Found",
                              "<h1>Not Found</h1>");
            end if; -- Method = "GET" and then Path = "/"
         end;
      end;
      Close_Socket (Socket);
   exception
      when others =>
         begin
            Close_Socket (Socket);
         exception
            when others =>
               null;
         end;
   end Handle_Connection;

   task type Worker;

   task body Worker is

      Socket : Socket_Type;

   begin -- Worker
      loop
         Connection_Queue.Get (Socket);
         Handle_Connection (Socket);
      end loop; -- forever
   end Worker;

   Pool : array (1 .. Worker_Count) of Worker;
   pragma Unreferenced (Pool);

   -------------------------------------------------------------------------
   -- Run_UI
   -------------------------------------------------------------------------

   procedure Run_UI (HTTP_Port : in GNAT.Sockets.Port_Type := 8080) is

      Server_Socket, Client_Socket : Socket_Type;
      Server_Address : constant Sock_Addr_Type :=
        (Family => Family_Inet, Addr => Any_Inet_Addr, Port => HTTP_Port);
      Client_Address : Sock_Addr_Type;

   begin -- Run_UI
      Create_Socket (Server_Socket, Family_Inet, Socket_Stream);
      Set_Socket_Option (Server_Socket, Socket_Level, (Reuse_Address, True));
      Bind_Socket (Server_Socket, Server_Address);
      Listen_Socket (Server_Socket);
      Put_Line ("Pump_Web listening on port " & Trim (HTTP_Port'Image, Left) &
                  ", relaying to controller """ & Controller_Name & """");
      loop
         Accept_Socket (Server_Socket, Client_Socket, Client_Address);
         Connection_Queue.Put (Client_Socket);
      end loop; -- forever
   end Run_UI;

end User_Interface_Web;
