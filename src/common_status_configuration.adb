--  Configuration interface between User_Interface_Client and its configuration
--  program.

--  Author    : Devid Haley
--  Created   : 08/08/2026
--  Last_edit : 08/08/2026

package body Common_Status_Configuration is

   function Encrypted (Parameter : Parameters) return Boolean is
     (Parameter = Password);

end Common_Status_Configuration;