# Hot Water Controller

## What it Does
Suite of programs with the primary functionality to control the circulating pump for a split system solar hot water system. The main hardware features are two temperature sensors, one for the panel (rooftop collector) and another for the tank. A pump which circulates cold water from the tank to be heated in the rooftop collector. The controller which runs the pump when there is a suitable temperature differential between the panel and roof sensors. The controller can also run the boost element in the tank when there is insufficient insolation. The latter is achieved via an MQTT interface to Home Assistant (home automation software).

## The Controller
The controller is purpose built hardware which provides analogue signal conditioning for PT100 RTDs with a 12 bit ADC. A relay to switch the pump on and off and a hardware watchdog to reboot the system if it stops working. The processing power is a RPi 3B+ running Pi OS (Trixie). Apart from the device drivers the software is written in Ada and built using the gnat (gcc) native 64bit tool chain. It can build on the target hardware if required.

## Individual programs

### hot_water_controller

This program does the actual control of the pump, provides data logging, event and error reporting. It has been designed to run as a systemd service and does not require root privilege. It also serves the user interface directly: a web server on port 8080 providing status display and command entry (clear fault table, manual boost), reading and writing the controller's state in-process rather than over a network protocol.

### configure_home_automation

This produces the JSON configuration file required for hot_water_controller to access the MQTT broker, for the purpose of controlling the boost element, via home automation. The broker host, user name and topic are configurable. The user password is obfuscated.

### configure_user_interface

This produces the JSON configuration file required for hot_water_controller to access the MQTT broker to publish the controller status. The broker host, user name and topic are configurable. The user password is obfuscated.

### test_controller

A test program for the controller hardware it allows calibration of the RTD condition to reduce the theoretical error at 0C to 100C to one count of the ADC, approximately 25mC. The total error budget including the RTDs should be less than 1.0C differential.

### test_configuration

A simple test program to verify the correct reading and writing of the configuration file.

### hw_cost

This program can calculate the cost of electricity used and estimate the cost saving due to captured solar energy. The parameters, for example cost of electricity and the flow rate are read from a configuration file. An arbitrary date range for the calculation can be specified on the command line.

## Dependencies

libmosquitto : MQTT

libgpiod : GPIO binary inputs and outputs

GNATCOLL.JSON (reading and writing of configuration files)

I2C must be enabled (local 2 line by 16 character display)

SPI must be enabled (ADC read and write)

Various packages from DJH Pi_Common and Pi_Common_C repositories