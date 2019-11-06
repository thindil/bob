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
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Commands; use Commands;

package body Config is

   procedure LoadConfig(FileName: String := ".bob.yml") is
      ConfigFile: File_Type;
      Name, Line, Description, Key, Value, Output: Unbounded_String;
      SeparatorPosition: Natural;
      Execute, Flags: UnboundedString_Container.Vector;
      Variables: Variables_Container.Map;
      type Items_Type is (COMMAND, VARIABLE, FLAG);
      ItemType: Items_Type;
      procedure AddCommand is
      begin
         if Flags.Contains(To_Unbounded_String("windowsonly")) and
           Directory_Separator = '/' then
            return;
         end if;
         if Flags.Contains(To_Unbounded_String("unixonly")) and
           Directory_Separator = '\' then
            return;
         end if;
         if Commands_List.Contains(Name) then
            Put_Line
              ("Can't add command '" & To_String(Name) &
               "'. There is one declared with that name.");
         elsif Name /= Null_Unbounded_String then
            if Execute.Length = 0 then
               Put_Line
                 ("Can't add command '" & To_String(Name) &
                  "'. No commands to execute are entered.");
            elsif Description = Null_Unbounded_String then
               Put_Line
                 ("Can't add command '" & To_String(Name) &
                  "'. No command description provided.");
            else
               Commands_List.Include
                 (Name,
                  (Execute => Execute, Description => Description,
                   Variables => Variables, Output => Output, Flags => Flags));
            end if;
         end if;
      end AddCommand;
   begin
      if not Exists(FileName) then
         Put_Line("File: '" & FileName & "' doesn't exists.");
         return;
      end if;
      Open(ConfigFile, In_File, FileName);
      while not End_Of_File(ConfigFile) loop
         Line := Trim(To_Unbounded_String(Get_Line(ConfigFile)), Both);
         if Length(Line) = 0 or Element(Line, 1) = '#' then
            goto End_Of_Loop;
         end if;
         if Length(Line) > 10 and then Slice(Line, 1, 10) = "- include:" then
            SeparatorPosition := Index(Line, ":");
            Value :=
              Unbounded_Slice(Line, SeparatorPosition + 2, Length(Line));
            LoadConfig(To_String(Value));
         end if;
         if Line = To_Unbounded_String("- command:") then
            AddCommand;
            Name := Null_Unbounded_String;
            Execute.Clear;
            Variables.Clear;
            Flags.Clear;
            Description := Null_Unbounded_String;
            Output := To_Unbounded_String("standard");
            Line := Trim(To_Unbounded_String(Get_Line(ConfigFile)), Both);
         end if;
         SeparatorPosition := Index(Line, "-");
         if SeparatorPosition /= 1 then
            SeparatorPosition := Index(Line, ":");
         end if;
         Key := Unbounded_Slice(Line, 1, SeparatorPosition - 1);
         if Key = To_Unbounded_String("execute") or
           Key = To_Unbounded_String("variables") or
           Key = To_Unbounded_String("flags") then
            Value := Null_Unbounded_String;
         else
            Value :=
              Unbounded_Slice(Line, SeparatorPosition + 2, Length(Line));
         end if;
         if Key = To_Unbounded_String("name") then
            Name := Value;
         elsif Key = To_Unbounded_String("description") then
            Description := Value;
         elsif Key = To_Unbounded_String("output") then
            Output := Value;
         elsif Key = To_Unbounded_String("execute") then
            ItemType := COMMAND;
         elsif Key = To_Unbounded_String("variables") then
            ItemType := VARIABLE;
         elsif Key = To_Unbounded_String("flags") then
            ItemType := FLAG;
         else
            case ItemType is
               when COMMAND =>
                  Execute.Append(Value);
               when FLAG =>
                  Flags.Append(Value);
               when VARIABLE =>
                  SeparatorPosition := Index(Value, "=");
                  Variables.Include
                    (Unbounded_Slice(Value, 1, SeparatorPosition - 2),
                     Unbounded_Slice
                       (Value, SeparatorPosition + 2, Length(Value)));
            end case;
         end if;
         <<End_Of_Loop>>
      end loop;
      AddCommand;
      Close(ConfigFile);
   end LoadConfig;

end Config;
