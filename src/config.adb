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
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Commands; use Commands;

package body Config is

   procedure LoadConfig is
      ConfigFile: File_Type;
      Name, Execute, Line, Description: Unbounded_String;
   begin
      if not Exists(".bob.yml") then
         return;
      end if;
      Open(ConfigFile, In_File, ".bob.yml");
      while not End_Of_File(ConfigFile) loop
         Line := Trim(To_Unbounded_String(Get_Line(ConfigFile)), Both);
         if Line = To_Unbounded_String("- command:") then
            Name := Null_Unbounded_String;
            Execute := Null_Unbounded_String;
            Description := Null_Unbounded_String;
         end if;
         if Length(Line) > 4 and then Slice(Line, 1, 4) = "name" then
            Name := Unbounded_Slice(Line, 7, Length(Line));
         elsif Length(Line) > 7 and then Slice(Line, 1, 7) = "execute" then
            Execute := Unbounded_Slice(Line, 10, Length(Line));
         elsif Length(Line) > 11
           and then Slice(Line, 1, 11) = "description" then
            Description := Unbounded_Slice(Line, 14, Length(Line));
         end if;
         if Name /= Null_Unbounded_String and
           Execute /= Null_Unbounded_String then
            Commands_List.Include
              (Name, (Execute => Execute, Description => Description));
         end if;
      end loop;
      Close(ConfigFile);
   end LoadConfig;

end Config;
