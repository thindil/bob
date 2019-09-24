-- Copyright (c) 2019 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Directories; use Ada.Directories;
with Yaml.Dom; use Yaml.Dom;
with Yaml.Dom.Loading; use Yaml.Dom.Loading;
with Yaml.Source; use Yaml.Source;
with Yaml.Source.File; use Yaml.Source.File;

package body Config is

   procedure LoadConfig is
      ConfigFile: Pointer;
   begin
      if not Exists(".bob.yml") then
         return;
      end if;
      ConfigFile := As_Source(".bob.yml");
      declare
         Document: Document_Reference := From_Source(ConfigFile);
      begin
         null;
      end;
   end LoadConfig;

end Config;
