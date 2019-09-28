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

with Ada.Containers; use Ada.Containers;
with Ada.Directories; use Ada.Directories;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Commands; use Commands;

package body Config is

   procedure LoadConfig is
      ConfigFile: File_Type;
      Name, Line, Description, Key, Value: Unbounded_String;
      SeparatorPosition: Natural;
      Execute: UnboundedString_Container.Vector;
      Variables: Variables_Container.Map;
      type Items_Type is (COMMAND, VARIABLE);
      ItemType: Items_Type;
   begin
      if not Exists(".bob.yml") then
         return;
      end if;
      Open(ConfigFile, In_File, ".bob.yml");
      while not End_Of_File(ConfigFile) loop
         Line := Trim(To_Unbounded_String(Get_Line(ConfigFile)), Both);
         if Line = To_Unbounded_String("- command:") then
            Name := Null_Unbounded_String;
            Execute.Clear;
            Variables.Clear;
            Description := Null_Unbounded_String;
            Line := Trim(To_Unbounded_String(Get_Line(ConfigFile)), Both);
         end if;
         SeparatorPosition := Index(Line, "-");
         if SeparatorPosition /= 1 then
            SeparatorPosition := Index(Line, ":");
         end if;
         Key := Unbounded_Slice(Line, 1, SeparatorPosition - 1);
         if Key = To_Unbounded_String("execute") or
           Key = To_Unbounded_String("variables") then
            Value := Null_Unbounded_String;
         else
            Value :=
              Unbounded_Slice(Line, SeparatorPosition + 2, Length(Line));
         end if;
         if Key = To_Unbounded_String("name") then
            Name := Value;
         elsif Key = To_Unbounded_String("description") then
            Description := Value;
         elsif Key = To_Unbounded_String("execute") then
            ItemType := COMMAND;
         elsif Key = To_Unbounded_String("variables") then
            ItemType := VARIABLE;
         else
            if ItemType = COMMAND then
               Execute.Append(Value);
            else
               SeparatorPosition := Index(Value, "=");
               Variables.Include
                 (Unbounded_Slice(Value, 1, SeparatorPosition - 2),
                  Unbounded_Slice
                    (Value, SeparatorPosition + 2, Length(Value)));
            end if;
         end if;
         if Name /= Null_Unbounded_String and Execute.Length > 0 and
           Description /= Null_Unbounded_String then
            Commands_List.Include
              (Name,
               (Execute => Execute, Description => Description,
                Variables => Variables));
         end if;
      end loop;
      Close(ConfigFile);
   end LoadConfig;

end Config;
