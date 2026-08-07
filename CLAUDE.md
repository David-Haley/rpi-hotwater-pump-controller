# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

Ada software suite controlling the circulating pump of a split-system solar hot water
system, running on a Raspberry Pi 3B+ (Pi OS Trixie) with purpose-built hardware:
PT100 RTD sensors read via a 12-bit ADC (SPI), a relay for the pump (GPIO), a
hardware watchdog, and an I2C DFR0555 2x16 LCD. The controller starts the pump when
there's a sufficient temperature differential between the rooftop panel and the tank,
and can trigger the tank's electric boost element via MQTT to Home Assistant when
there isn't enough insolation.

## Build

This repo depends on sibling repositories checked out alongside it (referenced by
relative path in `build_all.gpr`, e.g. `../Pi_Common`, `../DJH`, `../Pi_Common_C`).
They must exist at `../Pi_Common`, `../DJH`, `../Pi_Common_C` relative to this repo
for a build to succeed.

```sh
gprbuild -P build_all.gpr
```

Binaries land in `bin/`, object/ALI files in `obj/` (both gitignored). The compiler
runs with `-gnatwa -gnatVa` plus GNAT style checks (`-gnatyk -gnaty3 -gnatya -gnatyl
-gnatyn -gnatyr`); style/warning messages are non-fatal but should generally be kept
at zero — recent commit history (see `Last Edit` headers in source files) shows an
ongoing effort to drive warnings to zero.

Toolchain: native 64-bit GNAT (gcc-based), `-gnat2022`. The code is also expected to
build directly on the target Raspberry Pi if needed.

### System dependencies

- `libmosquitto` (MQTT)
- `libgpiod` (GPIO)
- `GNATCOLL.JSON` (JSON config read/write)
- I2C enabled (LCD) and SPI enabled (ADC) on the Pi

## Running / testing

There is no automated test framework — "tests" are standalone executables you run
manually and inspect the output of. They read/write relative-path files
(`Configuration.json`, `Home_Automation.json`, log/CSV files), so run them with cwd
set to a directory containing the right files — `bin/Test_Area/` is the existing
scratch directory with sample `Configuration.json` and `Home_Automation.json`:

```sh
cd bin/Test_Area
../test_configuration      # verifies Configuration.json read/write round-trips
../test_controller          # ADC/RTD calibration tool, interactive
../test_home_automation     # exercises the MQTT/Home Assistant interface
```

`hot_water_controller` itself is designed to run as a systemd service without root
(see `hw-pump.service`) and does not require an explicit start call for the data
logger or user-interface server — see the `20250507`/`20250502` notes in
`hot_water_controller.adb` about avoiding startup deadlocks.

## Architecture

### Programs (each a `Main` in `build_all.gpr`, source in `src/`)

- **hot_water_controller** — the actual controller: reads temperatures, drives the
  pump relay, runs boost scheduling, logs data/events, serves UDP status to UI
  clients, drives the LCD, feeds the hardware watchdog. Runs as a systemd service.
- **pump_ui** — ANSI terminal UI client, talks to `hot_water_controller` over UDP.
  Being deprecated in favour of `pump_web`.
- **pump_web** — browser-based UI client over HTTP, functionally equivalent to
  `pump_ui`, using the same UDP protocol underneath. Hand-rolled HTTP server on
  `GNAT.Sockets` (not the AWS library, which is present as a sibling repo but
  currently unused/unbuilt for this purpose). May eventually be folded directly
  into `hot_water_controller`.
- **configure_home_automation** — interactive tool that writes
  `Home_Automation.json`, including basic obfuscation of the MQTT password (see the
  `PASSWORD` byte array in the JSON — not real encryption, just obfuscation).
- **test_controller** — RTD/ADC calibration tool; aims for <1 ADC count error
  (~25 mC) across 0–100°C, total error budget <1.0°C differential.
- **test_configuration** / **test_home_automation** — round-trip verification of
  their respective JSON config files.
- **hw_cost** — offline calculator for electricity cost / solar savings from logged
  data, given a configurable cost/flow-rate and a command-line date range.

### Shared UDP status/control protocol

`shared_user_interface.ads` defines the wire protocol between `hot_water_controller`
(server, `user_interface_server.ads`, port 50001) and UI clients (`pump_ui`,
`pump_web`, via the generic `user_interface_client.ads` / `user_interface_web.ads`).
`Request_Records`/`Status_Records` are variant records marshalled directly as
`Stream_Element_Array`s sized off `'Size`. **`Interface_Version` in
`shared_user_interface.ads` must be bumped whenever these record layouts change** —
version mismatch is checked on every transaction and raises `Version_Mismatch`.
`pump_ui` and `pump_web` are separate client instantiations of the same generic
interface package sharing this protocol, not independent implementations.

### Core control packages (`src/`)

- `pump_controller_types.ads` — shared numeric subtypes/ranges (temperatures,
  differentials, hour-of-day windows for boost/comfort, etc.) and their `Text_IO`
  instantiations. Change ranges here first if a physical limit changes.
- `global_data.ads` — protected/task-safe shared state (current temperatures, pump
  run state, accumulated run time, fault table, next boost time) read by the UI
  server and written by the control loop. Has startup barriers (added 2025-05-02)
  so readers block until values are first defined.
- `configuration.ads` — reads `Configuration.json` (JSON via GNATCOLL) into typed,
  range-constrained accessor functions; raises `Configuration_Error`.
- `temperature.ad?`, `boost.ads`, `data_logger.ads`, `local_display.ads` — sensor
  reading, boost-element scheduling task, event/data logging task, and the
  asynchronous LCD interface (deliberately async so the controller keeps working if
  the LCD is absent/failed).
- `home_automation.ads` — thin abstraction over the MQTT call to Home Assistant
  (`Request_Boost_On`/`Off`); swappable for another mechanism (e.g. email) without
  touching `boost.ads`.

### External shared code (sibling repos, not in this repo)

- `../DJH` — general-purpose utility packages (`DJH.Events_and_Errors`,
  `DJH.Date_and_Time_Strings`, `DJH.JSON_Configuration`, `DJH.Statistics`,
  `DJH.Parse_CSV`, `DJH.Vera`, `DJH.One_Time`).
- `../Pi_Common` / `../Pi_Common_C` — Raspberry Pi hardware bindings (`RPi_GPIO`,
  `RPi_Watchdog`, `AD7091R2` ADC driver, `SPI_Interface`, `I2C_Interface`,
  `DFR0555_Display`, `MQTT_Client`, plus the C GPIO/I2C/SPI drivers).

When tracing a call into one of these packages, its source lives in the sibling
repo, not under this repo's `src/`.
