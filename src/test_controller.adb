-- Program for testing and calibration ot the hot water pump controller
-- hardware.

-- Author    : David Haley
-- Created   : 18/09/2017
-- Last Edit : 06/08/2025

-- 20250806 : Sampling rate matched to application rate, to reduce mains
-- frequency interference.
-- 20250804 : With and use for rpi_spi removed, spi dependancy limited to
-- AD7091R2 package. SPI now directly linked to C device driver.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with RPi_GPIO; use RPi_GPIO;
with AD7091R2;
with RPi_Watchdog;
with DJH.Statistics;

procedure Test_Controller is

   package My_AD is new AD7091R2;
   use My_AD;
   
   package My_Watchdog is new RPi_Watchdog; use My_Watchdog;

   type Int_A_Volts is range A_Volts'First .. A_Volts'Last;
   
   package ADC_Stats is new DJH.Statistics (Int_A_Volts);
   use ADC_Stats;

   Pump_Relay : constant GPIO_Pins := Gen1;
   Fault_LED  : constant GPIO_Pins := Gen2;

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
               Kick_Watchdog_High;
            else
               Kick_Watchdog_Low;
            end if; -- Watchdog
            Watchdog_State := not Watchdog_State;
            Next_Time := Next_Time + Kick_Interval;
         end select;
      end loop;  -- not Exit_Now
      Kick_Watchdog_Low;
   end Kick_Watchdog;

   procedure Initialise_Pins is

   begin -- Initialise_Pins
      Bind_Pin (Pump_Relay, Out_Pin);
      Write_Pin (Pin_Low, Pump_Relay);
      Bind_Pin (Fault_LED, Out_Pin);
      Write_Pin (Pin_Low, Fault_LED);
   end Initialise_Pins;
   
   function Menu return character is
   
	Test_Requested : Character;
   
   begin -- Menu
      Put_Line ("0 Exit");
      Put_Line ("1 Turn On Pump Output");
      Put_Line ("2 Turn Off Pump Output");
      Put_Line ("3 Turn On Fault LED");
      Put_Line ("4 Turn Off Fault LED");
      Put_Line ("5 Enable Hardware Watchdog");
      Put_Line ("6 Disable Hardware Watchdog");
      Put_Line ("7 Stop Watchdog Task (resets RPi if watchdog is enabled)");
      Put_Line ("8 Read RAW A/D data");
      Put_Line ("9 Reset A/D Test Statistics");
      Put ("Test? ");
      Get (Test_Requested);
      return Test_Requested;
   end Menu;

   procedure Test_AD (Channel_0_Stats, Channel_1_Stats : in out Data_Stores) is

      Test_Reads : constant Positive := 100000;
      Sample_Frequency : constant Positive := 574;
		-- Highest sample rate orthogonal to harmonics of 48 Hz and 52 Hz
		Interval : constant Duration := 1.0 / Duration (Sample_Frequency);
		Next_Time : Time;
      AD_Result : A_Volt_Arrays;
      
   begin -- Test_AD
      Put_Line ("Testing A/D Converter for" & Test_Reads'Img & " Samples " &
                "Sample Rate :" & Sample_Frequency'Img & " Test Time :" &
                Natural'Image (Test_Reads / Sample_Frequency) & " s");
      Next_Time := Clock;
      for I in Natural range 1 .. Test_Reads loop
         AD_Result := AD_Read;
         Sample (Channel_0_Stats, Int_A_Volts (AD_Result (0)));
         Sample (Channel_1_Stats, Int_A_Volts (AD_Result (1)));
         if I mod 2000 = 0 then
				Put ('.');
			end if; -- I mod 2000 = 0
         Next_Time := @ + Interval;
         Delay until Next_Time;
      end loop; -- I in Natural range 1 .. Test_Reads
      New_Line;
      Put_Line ("Channel (0) ""Tank"" Results");
      Put_Line ("Mean:" & Mean (Channel_0_Stats)'Img &
                "  Variance:" & Variance (Channel_0_Stats)'Img);
      for I in Minimum (Channel_0_Stats) .. Maximum (Channel_0_Stats) loop
         Put_Line ('[' & I'Img & ']' & Frequency (Channel_0_Stats, I)'Img);
      end loop; -- in Minimum (Channel_0_Stats) .. Maximum (Channel_0_Stats)
      Put_Line ("Channel (1) ""Panel"" Results");
      Put_Line ("Mean:" & Mean (Channel_1_Stats)'Img &
                "  Variance:" & Variance (Channel_1_Stats)'Img);
      for I in Minimum( Channel_1_Stats) .. Maximum (Channel_1_Stats) loop
         Put_Line ('[' & I'Img & ']' & Frequency (Channel_1_Stats, I)'Img);
      end loop; -- I in Minimum( Channel_1_Stats) .. Maximum (Channel_1_Stats)
   end Test_AD;
   
   Channel_0_Stats, Channel_1_Stats : ADC_Stats.Data_Stores;

begin -- Test_Controller
   Put_Line ("Test Controller 20250806");
   Initialise_Pins;
   loop
      case Menu is
         when '0' =>
            Put_Line ("Pump Output Off");
            Write_Pin (Pin_Low, Pump_Relay);
            Put_Line ("Fault LED Off");
            Write_Pin (Pin_Low, Fault_LED);
            Put_Line ("Disabling_Watchdog ready for exit");
            Disable_Watchdog;
            if not Kick_Watchdog'Terminated then
					Put_Line ("Stopping Watchdog task ready for exit");
					Kick_Watchdog.Stop_Watchdog;
            end if; -- not Kick_Watchdog'Terminated
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
            Test_AD (Channel_0_Stats, Channel_1_Stats);
         when '9' =>
            Put_Line ("Resetting A/D Statistics");
            Clear (Channel_0_Stats);
            Clear (Channel_1_Stats);
         when others =>
            Put_Line ("Invalid Test Request");
      end case; -- Menu
   end loop;
end Test_Controller;


