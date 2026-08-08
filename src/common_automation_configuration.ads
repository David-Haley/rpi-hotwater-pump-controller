--  Configuration interface between Home_Wutomation and its configuration
--  program.

--  Author    : Devid Haley
--  Created   : 05/07/2026
--  Last_edit : 08/08/2026

--  20260808 : Remamed to Common_Automation_Configuration.

package Common_Automation_Configuration is

   type Parameters is (Broker, User, Password, Request_Topic, Request_Field,
     Acknowledge_Topic, Acknowledge_Field);

   Configuration_File : constant String := "Home_Automation.json";

   function Encrypted (Parameter : Parameters) return Boolean;

end Common_Automation_Configuration;