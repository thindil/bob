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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings; use Ada.Strings;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.String_Split; use GNAT.String_Split;

package body Commands is

   procedure ExecuteCommand is
      Key: constant Unbounded_String := To_Unbounded_String(Argument(1));
      Tokens: Slice_Set;
      Command, Arguments, ArgumentNumber: Unbounded_String :=
        Null_Unbounded_String;
      VariableStarts, NumberPosition: Natural := 1;
   begin
      -- Load enviroment variables if need
      for I in Commands_List(Key).Variables.Iterate loop
         Set
           (To_String(Variables_Container.Key(I)),
            To_String(Commands_List(Key).Variables(I)));
      end loop;
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
               Put_Line
                 ("You didn't entered enough arguments for this command. Please check it description for information what should be entered.");
               return;
            end if;
            Replace_Slice
              (Execute, VariableStarts, NumberPosition - 1,
               Argument(Positive'Value(To_String(ArgumentNumber)) + 1));
            <<End_Of_Command_Line_Loop>>
            VariableStarts := VariableStarts + 1;
         end loop;
         -- Replace variables with enviroment variables values
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
               Put_Line
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
         Create(Tokens, To_String(Execute), " ", Multiple);
         for J in 2 .. Slice_Count(Tokens) loop
            Append(Arguments, " " & Slice(Tokens, J));
         end loop;
         Trim(Arguments, Both);
         -- Move to the selected directory
         if Slice(Tokens, 1) = "cd" then
            if not Ada.Directories.Exists
                (Current_Directory & Directory_Separator &
                 To_String(Arguments)) then
               Put_Line
                 ("Directory: '" & Current_Directory & Directory_Separator &
                  To_String(Arguments) & "' doesn't exists.");
               return;
            end if;
            Set_Directory
              (Current_Directory & Directory_Separator &
               To_String(Trim(Arguments, Both)));
            goto End_Of_Loop;
         end if;
         if Locate_Exec_On_Path(Slice(Tokens, 1)) = null then
            Put_Line("Command: '" & Slice(Tokens, 1) & "' doesn't exists.");
            return;
         end if;
         Append(Command, Locate_Exec_On_Path(Slice(Tokens, 1)).all);
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
                  Put_Line
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
               Put_Line("Error during executing '" & To_String(Execute) & "'");
               return;
            end if;
         end;
         <<End_Of_Loop>>
         Command := Null_Unbounded_String;
         Arguments := Null_Unbounded_String;
      end loop;
   end ExecuteCommand;

end Commands;
