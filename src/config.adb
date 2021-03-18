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

with Ada.Containers;
with Ada.Directories;
with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNAT.OS_Lib;
with Messages;

package body Config is

   procedure Load_Config
     (Bob_Commands_List: in out Commands.Commands_Container.Map;
      File_Name: String := ".bob.yml") is
      use Ada.Directories;
      use Ada.Strings;
      use Ada.Strings.Unbounded;
      use Ada.Text_IO;
      use Commands;
      use Messages;

      Config_File: File_Type;
      Name, Line, Description, Key, Value, Output: Unbounded_String :=
        Null_Unbounded_String;
      Separator_Position: Natural := 0;
      Execute, Flags: Unbounded_String_Container.Vector :=
        Unbounded_String_Container.Empty_Vector;
      Variables: Variables_Container.Map := Variables_Container.Empty_Map;
      type Items_Type is (COMMAND, VARIABLE, FLAG);
      Default_Item_Type: constant Items_Type := COMMAND;
      Item_Type: Items_Type := Default_Item_Type;
      procedure Add_Command
        (The_Commands_List: in out Commands_Container.Map) is
         use Ada.Containers;
         use GNAT.OS_Lib;

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
         if The_Commands_List.Contains(Key => Name) then
            Show_Message
              (Text =>
                 "Can't add command '" & To_String(Source => Name) &
                 "'. There is one declared with that name.");
         elsif Name /= Null_Unbounded_String then
            if Execute.Length = 0 then
               Show_Message
                 (Text =>
                    "Can't add command '" & To_String(Source => Name) &
                    "'. No commands to execute are entered.");
            elsif Description = Null_Unbounded_String then
               Show_Message
                 (Text =>
                    "Can't add command '" & To_String(Source => Name) &
                    "'. No command description provided.");
            elsif Output = Null_Unbounded_String then
               Show_Message
                 (Text =>
                    "Can't add command '" & To_String(Source => Name) &
                    "'. No command result output provided.");
            else
               The_Commands_List.Include
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
         Show_Message(Text => "File: '" & File_Name & "' doesn't exists.");
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
               Show_Message
                 (Text =>
                    "Can't add commands from configuration file '" &
                    File_Name & "'. It require Bob in version at least: '" &
                    To_String(Source => Value) & "' while your version is '" &
                    Version & "'.");
               return;
            end if;
         end if;
         -- Include other configuration file
         if Length(Source => Line) > 10
           and then Slice(Source => Line, Low => 1, High => 10) =
             "- include:" then
            Separator_Position := Index(Source => Line, Pattern => ":");
            Value :=
              Unbounded_Slice
                (Source => Line, Low => Separator_Position + 2,
                 High => Length(Source => Line));
            Load_Config
              (Bob_Commands_List => Bob_Commands_List,
               File_Name => To_String(Source => Value));
            goto End_Of_Loop;
         end if;
         -- Add a command
         if Line = To_Unbounded_String(Source => "- command:") then
            Add_Command(The_Commands_List => Bob_Commands_List);
            Name := Null_Unbounded_String;
            Execute.Clear;
            Variables.Clear;
            Flags.Clear;
            Description := Null_Unbounded_String;
            Output := To_Unbounded_String(Source => "standard");
            goto End_Of_Loop;
         end if;
         -- Parse configuration settings
         Separator_Position := Index(Source => Line, Pattern => "-");
         if Separator_Position /= 1 then
            Separator_Position := Index(Source => Line, Pattern => ":");
         end if;
         if Separator_Position = 0 then
            Show_Message
              (Text =>
                 "Command '" & To_String(Source => Name) &
                 "' invalid entry: '" & To_String(Source => Line) & "'");
            goto End_Of_Loop;
         end if;
         Key :=
           Unbounded_Slice
             (Source => Line, Low => 1, High => Separator_Position - 1);
         if Key in To_Unbounded_String(Source => "execute") |
               To_Unbounded_String(Source => "variables") |
               To_Unbounded_String(Source => "flags") then
            Value := Null_Unbounded_String;
         else
            if Separator_Position + 2 >= Length(Source => Line) then
               Show_Message
                 (Text =>
                    "Command: '" & To_String(Source => Name) &
                    "' empty value for key: '" & To_String(Source => Key) &
                    "'.");
               Value := Null_Unbounded_String;
            else
               Value :=
                 Unbounded_Slice
                   (Source => Line, Low => Separator_Position + 2,
                    High => Length(Source => Line));
            end if;
         end if;
         if Key = To_Unbounded_String(Source => "name") then
            Name := Value;
         elsif Key = To_Unbounded_String(Source => "description") then
            Description := Value;
         elsif Key = To_Unbounded_String(Source => "output") then
            Output := Value;
         elsif Key = To_Unbounded_String(Source => "execute") then
            Item_Type := COMMAND;
         elsif Key = To_Unbounded_String(Source => "variables") then
            Item_Type := VARIABLE;
         elsif Key = To_Unbounded_String(Source => "flags") then
            Item_Type := FLAG;
         else
            case Item_Type is
               when COMMAND =>
                  Execute.Append(New_Item => Value);
               when FLAG =>
                  Flags.Append(New_Item => Value);
               when VARIABLE =>
                  Separator_Position := Index(Source => Value, Pattern => "=");
                  Variables.Include
                    (Key =>
                       Unbounded_Slice
                         (Source => Value, Low => 1,
                          High => Separator_Position - 2),
                     New_Item =>
                       Unbounded_Slice
                         (Source => Value, Low => Separator_Position + 2,
                          High => Length(Source => Value)));
            end case;
         end if;
         <<End_Of_Loop>>
      end loop Read_Config_File_Loop;
      Add_Command(The_Commands_List => Bob_Commands_List);
      Close(File => Config_File);
   end Load_Config;

end Config;
