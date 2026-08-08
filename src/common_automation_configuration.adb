--  Configuration interface between Home_Automation and its configuration
--  program.

--  Author    : Devid Haley
--  Created   : 05/07/2026
--  Last_edit : 08/08/2026

--  20260808 : Remamed to Common_Automation_Configuration.

package body Common_Automation_Configuration is

   function Encrypted (Parameter : Parameters) return Boolean is
     (Parameter = Password);

end Common_Automation_Configuration;