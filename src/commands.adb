-- Copyright (c) 2019-2020 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with GNAT.Expect; use GNAT.Expect;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Messages; use Messages;

package body Commands is

   procedure ExecuteCommand(Key: Unbounded_String) is
      Command, Arguments, ArgumentNumber: Unbounded_String :=
        Null_Unbounded_String;
      VariableStarts, NumberPosition: Natural := 1;
   begin
      if not Commands_List.Contains(Key) then
         ShowMessage
           ("No available command with name '" & To_String(Key) & "'.");
         return;
      end if;
      -- Load environment variables if needed
      declare
         EvaluateVariables: constant Boolean :=
           Commands_List(Key).Flags.Contains
             (To_Unbounded_String("evaluatevariables"));
         Descriptor: Process_Descriptor;
         Args: Argument_List_Access;
         Result: Expect_Match;
      begin
         for I in Commands_List(Key).Variables.Iterate loop
            -- Just set environment variable
            if not EvaluateVariables then
               Set
                 (To_String(Variables_Container.Key(I)),
                  To_String(Commands_List(Key).Variables(I)));
               -- If proper flag is set, evaluate environment variable before
               -- set it
            else
               Args :=
                 Argument_String_To_List
                   (To_String(Commands_List(Key).Variables(I)));
               Non_Blocking_Spawn
                 (Descriptor, Args(Args'First).all,
                  Args(Args'First + 1 .. Args'Last));
               Expect(Descriptor, Result, ".+", 10_000);
               case Result is
                  when Expect_Timeout =>
                     ShowMessage
                       ("Failed to evaluate variable '" &
                        To_String(Variables_Container.Key(I)) & "'.");
                     Close(Descriptor);
                     return;
                  when 1 =>
                     Set
                       (To_String(Variables_Container.Key(I)),
                        Expect_Out(Descriptor));
                  when others =>
                     null;
               end case;
               Close(Descriptor);
            end if;
         end loop;
      end;
      for Execute of Commands_List(Key).Execute loop
         if Length(Execute) = 0 then
            goto End_Of_Loop;
         end if;
         -- Replace variables with command line arguments (if needed)
         VariableStarts := 1;
         loop
            VariableStarts := Index(Execute, "$", VariableStarts);
            exit when VariableStarts = 0 or VariableStarts = Length(Execute);
            if not Is_Digit(Element(Execute, VariableStarts + 1)) then
               goto End_Of_Command_Line_Loop;
            end if;
            NumberPosition := VariableStarts + 1;
            ArgumentNumber := Null_Unbounded_String;
            loop
               Append(ArgumentNumber, Element(Execute, NumberPosition));
               NumberPosition := NumberPosition + 1;
               exit when NumberPosition > Length(Execute);
               exit when not Is_Digit(Element(Execute, NumberPosition));
            end loop;
            if Argument_Count <= Positive'Value(To_String(ArgumentNumber)) then
               ShowMessage
                 ("You didn't entered enough arguments for this command. Please check it description for information what should be entered.");
               return;
            end if;
            Replace_Slice
              (Execute, VariableStarts, NumberPosition - 1,
               Argument(Positive'Value(To_String(ArgumentNumber)) + 1));
            <<End_Of_Command_Line_Loop>>
            VariableStarts := VariableStarts + 1;
         end loop;
         -- Replace variables with enviroment variables values (if needed)
         VariableStarts := 1;
         loop
            VariableStarts := Index(Execute, "$", VariableStarts);
            exit when VariableStarts = 0 or VariableStarts = Length(Execute);
            if not Is_Alphanumeric(Element(Execute, VariableStarts + 1)) then
               goto End_Of_Variables_Loop;
            end if;
            NumberPosition := VariableStarts + 1;
            ArgumentNumber := Null_Unbounded_String;
            loop
               Append(ArgumentNumber, Element(Execute, NumberPosition));
               NumberPosition := NumberPosition + 1;
               exit when NumberPosition > Length(Execute);
               exit when not Is_Alphanumeric(Element(Execute, NumberPosition));
            end loop;
            if not Ada.Environment_Variables.Exists
                (To_String(ArgumentNumber)) then
               ShowMessage
                 ("Variable: " & To_String(ArgumentNumber) &
                  " doesn't exists.");
               return;
            end if;
            Replace_Slice
              (Execute, VariableStarts, NumberPosition - 1,
               Value(To_String(ArgumentNumber)));
            <<End_Of_Variables_Loop>>
            VariableStarts := VariableStarts + 1;
         end loop;
         -- Split command line
         declare
            StartIndex: Positive range 1 .. 2;
            EndIndex: Integer range -1 .. Length(Execute) + 3;
         begin
            case Element(Execute, 1) is
               when ''' =>
                  StartIndex := 2;
                  EndIndex := Index(Execute, "'", 2) - 1;
               when '"' =>
                  StartIndex := 2;
                  EndIndex := Index(Execute, """", 2) - 1;
               when others =>
                  StartIndex := 1;
                  EndIndex := Index(Execute, " ", 2) - 1;
            end case;
            if EndIndex < 1 then
               EndIndex := Length(Execute);
            end if;
            Command := Unbounded_Slice(Execute, StartIndex, EndIndex);
            case Element(Execute, 1) is
               when ''' | '"' =>
                  EndIndex := EndIndex + 3;
               when others =>
                  EndIndex := EndIndex + 2;
            end case;
            if EndIndex < Length(Execute) then
               Arguments :=
                 Unbounded_Slice(Execute, EndIndex, Length(Execute));
            end if;
         end;
         -- Translate path if needed
         if Commands_List(Key).Flags.Contains
             (To_Unbounded_String("windowspath")) and
           Directory_Separator = '/' then
            Translate(Arguments, To_Mapping("\", "/"));
         elsif Commands_List(Key).Flags.Contains
             (To_Unbounded_String("unixpath")) and
           Directory_Separator = '\' then
            Translate(Arguments, To_Mapping("/", "\"));
         end if;
         -- Enter selected directory
         if Command = To_Unbounded_String("cd") then
            Arguments :=
              To_Unbounded_String(Normalize_Pathname(To_String(Arguments)));
            if not Ada.Directories.Exists(To_String(Arguments)) then
               ShowMessage
                 ("Directory: '" & To_String(Arguments) & "' doesn't exists.");
               return;
            end if;
            Set_Directory(To_String(Arguments));
            goto End_Of_Loop;
         end if;
         if Locate_Exec_On_Path(To_String(Command)) = null then
            ShowMessage
              ("Command: '" & To_String(Command) & "' doesn't exists.");
            return;
         end if;
         -- Execute command
         Command :=
           To_Unbounded_String(Locate_Exec_On_Path(To_String(Command)).all);
         declare
            FileDescriptor: File_Descriptor;
            Output: constant String := To_String(Commands_List(Key).Output);
            ReturnCode: Integer;
         begin
            if Output = "standard" then
               FileDescriptor := Standout;
            elsif Output = "error" then
               FileDescriptor := Standerr;
            else
               if Ada.Directories.Exists(Output) then
                  Delete_File(Output);
               end if;
               FileDescriptor := Create_Output_Text_File(Output);
               if FileDescriptor = Invalid_FD then
                  ShowMessage
                    ("Error during executing '" & To_String(Execute) &
                     "'. Can't create '" & Output & "' as the output file.");
                  return;
               end if;
            end if;
            Spawn
              (To_String(Command),
               Argument_String_To_List(To_String(Arguments)).all,
               FileDescriptor, ReturnCode);
            if ReturnCode > 0 then
               ShowMessage
                 ("Error during executing '" & To_String(Execute) & "'");
               return;
            end if;
         end;
         <<End_Of_Loop>>
         Command := Null_Unbounded_String;
         Arguments := Null_Unbounded_String;
      end loop;
   end ExecuteCommand;

end Commands;
