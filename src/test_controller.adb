-- Author    : David Haley
-- Created   : 18/09/2017
-- Last Edit : 13/10/2017

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with RPi_GPIO; use RPi_GPIO;
with RPi_SPI; use RPi_SPI;
with AD7091R2;
with RPi_Watchdog;
with Statistics;

procedure Test_Controller is

   package My_AD is new AD7091R2; use My_AD;
   package My_Watchdog is new RPi_Watchdog; use My_Watchdog;

   type Int_A_Volts is range A_Volts'First .. A_Volts'Last;
   package Channel_0_Stats is new Statistics (Int_A_Volts);
   package Channel_1_Stats is new Statistics (Int_A_Volts);

   Test_Requested : Character;

   Pump_Relay : constant GPIO_Pins := Gen1;
   Fault_LED  : constant GPIO_Pins := Gen2;
   Test_Reads : Natural := 100000;

   task Kick_Watchdog is
      entry Stop_Watchdog;
      -- Stop kicking watchdog
   end Kick_Watchdog;

   task body Kick_Watchdog is

      Kick_Interval : constant Duration := 0.5;
      Watchdog_State : Boolean := False;
      Exit_Now : Boolean := False;
      Next_Time : Time := Clock + Kick_Interval;

   begin -- Kick_Watchdog
      while not Exit_Now loop
         select
            accept Stop_Watchdog do
               Exit_Now := True;
            end Stop_Watchdog;
         or
            delay until Next_Time;
            if Watchdog_State then
               Kick_Watchdog_Low;
               Watchdog_State := False;
            else
               Kick_Watchdog_High;
               Watchdog_State := True;
            end if; -- Watchdog
            Next_Time := Next_Time + Kick_Interval;
         end select;
      end loop;  -- not Exit_Now
   end Kick_Watchdog;

   procedure Initialise_Pins is

   begin -- Initialise_Pins
      Bind_Pin (Pump_Relay, Out_Pin);
      Write_Pin (Pin_Low, Pump_Relay);
      Bind_Pin (Fault_LED, Out_Pin);
      Write_Pin (Pin_Low, Fault_LED);
      Put_Line ("Test Controller 20171013");
      Put_Line ("0 Exit");
      Put_Line ("1 Turn On Pump Output");
      Put_Line ("2 Turn Off Pump Output");
      Put_Line ("3 Turn On Fault LED");
      Put_Line ("4 Turn Off Fault LED");
      Put_Line ("5 Enable Hardware Watchdog");
      Put_Line ("6 Disable Hardware Watchdog");
      Put_Line ("7 Stop Watchdog Task (resets RPi if watchdog is enabled)");
      Put_Line ("8 Read RAW A/D data" & Natural'Image (Test_Reads) & " Times");
      Put_Line ("9 Reset A/D Test Statistics");
   end Initialise_Pins;

   procedure Test_AD is

      AD_Result : A_Volt_Arrays;
   begin -- Test_AD
      Put_Line ("Testing A/D Converter");
      for I in Natural range 1 .. Test_Reads loop
         AD_Result := AD_Read;
         Channel_0_Stats.Sample (Int_A_Volts (AD_Result (0)));
         Channel_1_Stats.Sample (Int_A_Volts (AD_Result (1)));
      end loop; -- I in Natural range 1 .. Test_Reads
      Put_Line ("Channel (0) Results");
      Put_Line ("Test_Reads:" & Natural'Image (Channel_0_Stats.Count));
      Put_Line ("Mean:" & Channel_0_Stats.My_Float'Image (
                Channel_0_Stats.Mean));
      Put_Line ("Minimum:" & Int_A_Volts'Image (Channel_0_Stats.Minimum));
      Put_Line ("Maximum:" & Int_A_Volts'Image (Channel_0_Stats.Maximum));
      for I in Channel_0_Stats.Minimum .. Channel_0_Stats.Maximum loop
         Put_Line ('[' & Int_A_Volts'Image (I) & ']' &
                     Natural'Image (Channel_0_Stats.Frequency (I)));
      end loop; -- I in Channel_0_Stats.Minimum .. Channel_0_Stats.Maximum
      Put_Line ("Variance:" & Channel_0_Stats.My_Float'Image (
                Channel_0_Stats.Variance));
      Put_Line ("Channel (1) Results");
      Put_Line ("Test_Reads:" & Natural'Image (Channel_1_Stats.Count));
      Put_Line ("Mean:" & Channel_1_Stats.My_Float'Image (
                Channel_1_Stats.Mean));
      Put_Line ("Minimum:" & Int_A_Volts'Image (Channel_1_Stats.Minimum));
      Put_Line ("Maximum:" & Int_A_Volts'Image (Channel_1_Stats.Maximum));
      for I in Channel_1_Stats.Minimum .. Channel_1_Stats.Maximum loop
         Put_Line ('[' & Int_A_Volts'Image (I) & ']' &
                     Natural'Image (Channel_1_Stats.Frequency (I)));
      end loop; -- I in Channel_1_Stats.Minimum .. Channel_1_Stats.Maximum
      Put_Line ("Variance:" & Channel_1_Stats.My_Float'Image (
                Channel_1_Stats.Variance));
   end Test_AD;

begin -- Test_Controller
   Initialise_Pins;
   loop
      Put ("Test? ");
      Get (Test_Requested);
      case Test_Requested is
         when '0' =>
            Put_Line ("Disabling_Watchdog ready for exit");
            Disable_Watchdog;
            Put_Line ("Stopping Watchdog task ready for exit");
            Kick_Watchdog.Stop_Watchdog;
            Put_Line ("Ending Tests");
            exit;
         when '1' =>
            Put_Line ("Pump Output On");
            Write_Pin (Pin_High, Pump_Relay);
         when '2' =>
            Put_Line ("Pump Output Off");
            Write_Pin (Pin_Low, Pump_Relay);
         when '3' =>
            Put_Line ("Fault LED On");
            Write_Pin (Pin_High, Fault_LED);
         when '4' =>
            Put_Line ("Fault LED Off");
            Write_Pin (Pin_Low, Fault_LED);
         when '5' =>
            Put_Line ("Enable_Watchdog");
            Enable_Watchdog;
         when '6' =>
            Put_Line ("Disable_Watchdog");
            Disable_Watchdog;
         when '7' =>
            Put_Line ("Stop Watchdog task");
            Kick_Watchdog.Stop_Watchdog;
         when '8' =>
            Test_AD;
         when '9' =>
            Put_Line ("Resetting A/D Statistics");
            Channel_0_Stats.Clear;
            Channel_1_Stats.Clear;
         when others =>
            Put_Line ("Invalid Test Request");
      end case; -- Test_Requested
   end loop;
end Test_Controller;


