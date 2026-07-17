--  Configuration interface between Home_Automation and its configuration
--  program.

--  Author    : Devid Haley
--  Created   : 05/07/2026
--  Last_edit : 05/07/2026

package body Common_Configuration is

   function Encrypted (Parameter : Parameters) return Boolean is
     (Parameter = Password);

end Common_Configuration;