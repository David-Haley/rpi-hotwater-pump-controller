--  Configuration interface between User_Interface_Server and its configuration
--  program.

--  Author    : Devid Haley
--  Created   : 08/08/2026
--  Last_edit : 08/08/2026

package Common_Status_Configuration is

   type Parameters is (Broker, User, Password, Status_Topic);

   Configuration_File : constant String := "User_Interface.json";

   function Encrypted (Parameter : Parameters) return Boolean;

end Common_Status_Configuration;