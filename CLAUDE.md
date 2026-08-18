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
- AWS (Ada Web Server), `gprinstall`-registered system-wide at
  `/usr/share/gpr/aws.gpr`; `build_all.gpr` pulls it in via a plain `with
  "aws";` (no build step needed). AWS transitively brings in `gnatcoll_core`
  (superset of what Pi_Common's own `lib_gnatcoll.gpr`/`lib_gnatcoll_minimal.gpr`
  provide, so `build_all.gpr` does not `with` those directly — doing so
  alongside AWS causes a "unit cannot belong to several projects" conflict,
  since both would claim the same GNATCOLL unit names from different source
  trees), `xmlada`, and links `libssl`/`libcrypto` (statically links AWS/
  GNATCOLL/XML-Ada; only OpenSSL ends up as a new runtime shared-library
  dependency).
- I2C enabled (LCD) and SPI enabled (ADC) on the Pi

## Running / testing

There is no automated test framework — "tests" are standalone executables you run
manually and inspect the output of. They read/write relative-path files
(`Configuration.json`, `Home_Automation.json`, log/CSV files), so run them with cwd
set to a directory containing the right files — `bin/Test_Area/` is the existing
scratch directory with sample `Configuration.json` and `Home_Automation.json`:

```sh
cd bin/Test_Area
../test_configuration      # reads Configuration.json and prints every parsed value
../test_controller          # ADC/RTD calibration tool, interactive
../test_home_automation     # interactive menu that sends real boost on/off
                             # requests over MQTT using Home_Automation.json
```

`hot_water_controller` itself is designed to run as a systemd service without root
(see `hw-pump.service`) and does not require an explicit start call for the data
logger or user-interface server — see the `20250507` note in
`hot_water_controller.adb` about avoiding startup deadlocks.

## Architecture

### Programs (each a `Main` in `build_all.gpr`, source in `src/`)

- **hot_water_controller** — the actual controller: reads temperatures, drives the
  pump relay, runs boost scheduling, logs data/events, publishes status over MQTT,
  serves a browser-based web UI directly (see below), drives the LCD, feeds the
  hardware watchdog. Runs as a systemd service.
- **configure_home_automation** — interactive tool that writes
  `Home_Automation.json`, including basic encryption of the MQTT password (see the
  `PASSWORD` byte array in the JSON — a one-time-pad-style cipher via
  `DJH.One_Time`, not a standard crypto algorithm).
- **test_controller** — RTD/ADC calibration tool; aims for <1 ADC count error
  (~25 mC) across 0–100°C, total error budget <1.0°C differential.
- **test_configuration** — reads `Configuration.json` and prints every parsed value
  for visual comparison against the file; does not exercise writing.
- **test_home_automation** — interactive menu that calls `Home_Automation`
  directly to send real boost-on/off requests over MQTT, independent of the
  boost-scheduling logic in `boost.ads`.
- **hw_cost** — offline calculator for electricity cost / solar savings from logged
  data, given a configurable cost/flow-rate and a command-line date range.

### Embedded web UI

`user_interface_web.ads/.adb` implements the browser-based UI (status display,
clear-fault-table and manual-boost forms) as an `AWS.Server.HTTP` instance — a
single `Dispatch` callback routes `AWS.Status.Data` requests by method/URI and
returns `AWS.Response.Data`; AWS itself owns the accept loop, connection
pooling and request parsing. There is no wire protocol or separate client
process — `Dispatch` reads status via `User_Interface_Server.UI_Server.Get_Status`
and issues commands straight to `Global_Data` (`Clear_Fault`,
`Write_Next_Boost_Time`). Because AWS's `HTTP` server object manages its own
tasking, `User_Interface_Web` no longer has a `Web_UI` task: `Start_Web_UI`
and `Stop_Web_UI` are plain procedures wrapping `AWS.Server.Start`/`Shutdown`,
called explicitly from `hot_water_controller.adb`'s `Initialise`/shutdown
paths.

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
  `DJH.Date_and_Time_Strings`, `DJH.JSON_Configuration`, `DJH.Statistics`).
- `../Pi_Common` / `../Pi_Common_C` — Raspberry Pi hardware bindings (`RPi_GPIO`,
  `RPi_Watchdog`, `AD7091R2` ADC driver, `DFR0555_Display`, `MQTT_Client`,
  `ANSI_Console`, `Linux_Signals`, plus the underlying C GPIO/I2C/SPI drivers used
  internally by those packages).

When tracing a call into one of these packages, its source lives in the sibling
repo, not under this repo's `src/`.
