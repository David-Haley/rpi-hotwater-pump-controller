--  Configuration interface between Home_Wutomation and its configuration
--  program.

--  Author    : Devid Haley
--  Created   : 05/07/2026
--  Last_edit : 05/07/2026

package Common_Configuration is

   type Parameters is (Broker, User, Password, Request_Topic, Request_Field,
     Acknowledge_Topic, Acknowledge_Field);

   Configuration_File : constant String := "Home_Automation.json";

   function Encrypted (Parameter : Parameters) return Boolean;

end Common_Configuration;