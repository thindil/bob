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

with Ada.Containers; use Ada.Containers;
with Ada.Directories; use Ada.Directories;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Commands; use Commands;
with Messages; use Messages;

package body Config is

   procedure Load_Config(File_Name: String := ".bob.yml") is
      Config_File: File_Type;
      Name, Line, Description, Key, Value, Output: Unbounded_String :=
        Null_Unbounded_String;
      Separator_Position: Natural := 0;
      Execute, Flags: Unbounded_String_Container.Vector :=
        Unbounded_String_Container.Empty_Vector;
      Variables: Variables_Container.Map := Variables_Container.Empty_Map;
      type Items_Type is (COMMAND, VARIABLE, FLAG) with
         Default_Value => COMMAND;
      Item_Type: Items_Type := COMMAND;
      procedure Add_Command is
      begin
         if Flags.Contains
             (Item => To_Unbounded_String(Source => "windowsonly")) and
           Directory_Separator = '/' then
            return;
         end if;
         if Flags.Contains
             (Item => To_Unbounded_String(Source => "unixonly")) and
           Directory_Separator = '\' then
            return;
         end if;
         if Commands_List.Contains(Key => Name) then
            ShowMessage
              (Text =>
                 "Can't add command '" & To_String(Source => Name) &
                 "'. There is one declared with that name.");
         elsif Name /= Null_Unbounded_String then
            if Execute.Length = 0 then
               ShowMessage
                 (Text =>
                    "Can't add command '" & To_String(Source => Name) &
                    "'. No commands to execute are entered.");
            elsif Description = Null_Unbounded_String then
               ShowMessage
                 (Text =>
                    "Can't add command '" & To_String(Source => Name) &
                    "'. No command description provided.");
            elsif Output = Null_Unbounded_String then
               ShowMessage
                 (Text =>
                    "Can't add command '" & To_String(Source => Name) &
                    "'. No command result output provided.");
            else
               Commands_List.Include
                 (Key => Name,
                  New_Item =>
                    (Execute => Execute, Description => Description,
                     Variables => Variables, Output => Output,
                     Flags => Flags));
            end if;
         end if;
      end Add_Command;
   begin
      -- Check if selected configuration file exist
      if not Exists(Name => File_Name) then
         ShowMessage(Text => "File: '" & File_Name & "' doesn't exists.");
         return;
      end if;
      Open(File => Config_File, Mode => In_File, Name => File_Name);
      Read_Config_File_Loop :
      while not End_Of_File(File => Config_File) loop
         Line :=
           Trim
             (Source =>
                To_Unbounded_String(Source => Get_Line(File => Config_File)),
              Side => Both);
         if Length(Source => Line) = 0 or
           Element(Source => Line, Index => 1) = '#' then
            goto End_Of_Loop;
         end if;
         -- Check compatybility of configuration file version and the program
         -- version
         if Length(Source => Line) > 8
           and then Slice(Source => Line, Low => 1, High => 8) =
             "version:" then
            Separator_Position := Index(Source => Line, Pattern => ":");
            Value :=
              Unbounded_Slice
                (Source => Line, Low => Separator_Position + 2,
                 High => Length(Source => Line));
            if Float'Value(To_String(Source => Value)) >
              Float'Value(Version) then
               ShowMessage
                 (Text =>
                    "Can't add commands from configuration file '" &
                    File_Name & "'. It require Bob in version at least: '" &
                    To_String(Source => Value) & "' while your version is '" &
                    Version & "'.");
               return;
            end if;
         end if;
         -- Include other configuration file
         if Length(Line) > 10 and then Slice(Line, 1, 10) = "- include:" then
            Separator_Position := Index(Line, ":");
            Value :=
              Unbounded_Slice(Line, Separator_Position + 2, Length(Line));
            Load_Config(To_String(Value));
            goto End_Of_Loop;
         end if;
         -- Add a command
         if Line = To_Unbounded_String("- command:") then
            Add_Command;
            Name := Null_Unbounded_String;
            Execute.Clear;
            Variables.Clear;
            Flags.Clear;
            Description := Null_Unbounded_String;
            Output := To_Unbounded_String("standard");
            goto End_Of_Loop;
         end if;
         -- Parse configuration settings
         Separator_Position := Index(Line, "-");
         if Separator_Position /= 1 then
            Separator_Position := Index(Line, ":");
         end if;
         if Separator_Position = 0 then
            ShowMessage
              ("Command '" & To_String(Name) & "' invalid entry: '" &
               To_String(Line) & "'");
            goto End_Of_Loop;
         end if;
         Key := Unbounded_Slice(Line, 1, Separator_Position - 1);
         if Key in To_Unbounded_String("execute") |
               To_Unbounded_String("variables") |
               To_Unbounded_String("flags") then
            Value := Null_Unbounded_String;
         else
            if Separator_Position + 2 >= Length(Line) then
               ShowMessage
                 ("Command: '" & To_String(Name) & "' empty value for key: '" &
                  To_String(Key) & "'.");
               Value := Null_Unbounded_String;
            else
               Value :=
                 Unbounded_Slice(Line, Separator_Position + 2, Length(Line));
            end if;
         end if;
         if Key = To_Unbounded_String("name") then
            Name := Value;
         elsif Key = To_Unbounded_String("description") then
            Description := Value;
         elsif Key = To_Unbounded_String("output") then
            Output := Value;
         elsif Key = To_Unbounded_String("execute") then
            Item_Type := COMMAND;
         elsif Key = To_Unbounded_String("variables") then
            Item_Type := VARIABLE;
         elsif Key = To_Unbounded_String("flags") then
            Item_Type := FLAG;
         else
            case Item_Type is
               when COMMAND =>
                  Execute.Append(Value);
               when FLAG =>
                  Flags.Append(Value);
               when VARIABLE =>
                  Separator_Position := Index(Value, "=");
                  Variables.Include
                    (Unbounded_Slice(Value, 1, Separator_Position - 2),
                     Unbounded_Slice
                       (Value, Separator_Position + 2, Length(Value)));
            end case;
         end if;
         <<End_Of_Loop>>
      end loop Read_Config_File_Loop;
      Add_Command;
      Close(Config_File);
   end Load_Config;

end Config;
