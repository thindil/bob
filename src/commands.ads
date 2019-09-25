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

with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

package Commands is

   type Command_Record is record
      Execute: Unbounded_String;
      Description: Unbounded_String;
   end record;

   package Commands_Container is new Hashed_Maps(Unbounded_String,
      Command_Record, Ada.Strings.Unbounded.Hash, "=");

   Commands_List: Commands_Container.Map;

   procedure ExecuteCommand;

end Commands;
