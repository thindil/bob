-- Copyright (c) 2019-2021 Bartek thindil Jasicki <thindil@laeran.pl>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

with Commands;

-- ****h* Config/Config
-- FUNCTION
-- Provide code for manipulate configuration files
-- SOURCE
package Config is
-- ****

   -- ****v* Config/Config.Version
   -- FUNCTION
   -- Version of the program
   -- SOURCE
   Version: constant String := "3.1";
   -- ****

   -- ****f* Config/Config.Load_Config
   -- FUNCTION
   -- Load specified configuration file
   -- PARAMETERS
   -- Bob_Commands_List - The list of the program commands
   -- FileName          - Path (absolute or relative) to configuration file
   --                     which will be loaded
   -- SOURCE
   procedure Load_Config
     (Bob_Commands_List: in out Commands.Commands_Container.Map;
      File_Name: String := ".bob.yml") with
      Pre => File_Name'Length > 0,
      Test_Case => (Name => "Test_LoadConfig", Mode => Nominal);
      -- ****

end Config;
