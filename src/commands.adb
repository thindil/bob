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

   procedure Execute_Command(Key: Unbounded_String) is
      Command, Arguments, Argument_Number: Unbounded_String :=
        Null_Unbounded_String;
      Variable_Starts, Number_Position: Natural := 1;
      Command_Path: GNAT.OS_Lib.String_Access;
      File_Desc: File_Descriptor := 0;
      Output: constant String :=
        To_String(Source => Commands_List(Key).Output);
   begin
      if not Commands_List.Contains(Key => Key) then
         ShowMessage
           (Text =>
              "No available command with name '" & To_String(Source => Key) &
              "'.");
         return;
      end if;
      -- Load environment variables if needed
      Load_Environment_Variables_Block :
      declare
         Evaluate_Variables: constant Boolean :=
           Commands_List(Key).Flags.Contains
             (Item => To_Unbounded_String(Source => "evaluatevariables"));
         Descriptor: Process_Descriptor; --## rule line off IMPROPER_INITIALIZATION
         Args: Argument_List_Access;
         Result: Expect_Match := -1;
      begin
         Set_Environment_Variables_Loop :
         for I in Commands_List(Key).Variables.Iterate loop
            -- Just set environment variable
            if not Evaluate_Variables then
               Set
                 (Name =>
                    To_String
                      (Source => Variables_Container.Key(Position => I)),
                  Value =>
                    To_String(Source => Commands_List(Key).Variables(I)));
               -- If proper flag is set, evaluate environment variable before
               -- set it
            else
               Args :=
                 Argument_String_To_List
                   (Arg_String =>
                      To_String(Source => Commands_List(Key).Variables(I)));
               Non_Blocking_Spawn
                 (Descriptor => Descriptor, Command => Args(Args'First).all,
                  Args => Args(Args'First + 1 .. Args'Last));
               Expect
                 (Descriptor => Descriptor, Result => Result, Regexp => ".+",
                  Timeout => 10_000);
               case Result is
                  when Expect_Timeout =>
                     ShowMessage
                       (Text =>
                          "Failed to evaluate variable '" &
                          To_String
                            (Source =>
                               Variables_Container.Key(Position => I)) &
                          "'.");
                     Close(Descriptor => Descriptor);
                     return;
                  when 1 =>
                     Set
                       (Name =>
                          To_String
                            (Source => Variables_Container.Key(Position => I)),
                        Value => Expect_Out(Descriptor => Descriptor));
                  when others =>
                     null;
               end case;
               Close(Descriptor => Descriptor);
            end if;
         end loop Set_Environment_Variables_Loop;
      end Load_Environment_Variables_Block;
      if Output = "standard" then
         File_Desc := Standout;
      elsif Output = "error" then
         File_Desc := Standerr;
      else
         if Ada.Directories.Exists(Name => Output) then
            Delete_File(Name => Output);
         end if;
         File_Desc := Create_Output_Text_File(Name => Output);
         if File_Desc = Invalid_FD then
            ShowMessage
              (Text =>
                 "Error during executing '" & To_String(Source => Key) &
                 "'. Can't create '" & Output & "' as the output file.");
            return;
         end if;
      end if;
      Execute_Command_Loop :
      for Execute of Commands_List(Key).Execute loop
         if Length(Source => Execute) = 0 then
            goto End_Of_Loop;
         end if;
         -- Replace variables with command line arguments (if needed)
         Variable_Starts := 1;
         Replace_Variable_With_Argument_Loop :
         loop
            Variable_Starts :=
              Index
                (Source => Execute, Pattern => "$", From => Variable_Starts);
            exit Replace_Variable_With_Argument_Loop when Variable_Starts =
              0 or
              Variable_Starts = Length(Source => Execute);
            if not Is_Digit
                (Item =>
                   Element
                     (Source => Execute, Index => Variable_Starts + 1)) then
               goto End_Of_Command_Line_Loop;
            end if;
            Number_Position := Variable_Starts + 1;
            Argument_Number := Null_Unbounded_String;
            Replace_With_Argument_Loop :
            loop
               Append
                 (Source => Argument_Number,
                  New_Item =>
                    Element(Source => Execute, Index => Number_Position));
               Number_Position := Number_Position + 1;
               exit Replace_With_Argument_Loop when Number_Position >
                 Length(Source => Execute)
                 or else not Is_Digit
                   (Item =>
                      Element(Source => Execute, Index => Number_Position));
            end loop Replace_With_Argument_Loop;
            if Argument_Count <=
              Positive'Value(To_String(Source => Argument_Number)) then
               ShowMessage
                 (Text =>
                    "You didn't entered enough arguments for this command. Please check it description for information what should be entered.");
               if Output not in "standard" | "error"
                 and then Ada.Directories.Exists(Name => Output) then
                  Delete_File(Name => Output);
               end if;
               return;
            end if;
            Replace_Slice
              (Source => Execute, Low => Variable_Starts,
               High => Number_Position - 1,
               By =>
                 Argument
                   (Number =>
                      Positive'Value(To_String(Source => Argument_Number)) +
                      1));
            <<End_Of_Command_Line_Loop>>
            Variable_Starts := Variable_Starts + 1;
         end loop Replace_Variable_With_Argument_Loop;
         -- Replace variables with enviroment variables values (if needed)
         Variable_Starts := 1;
         Replace_Variables_With_Environment_Loop :
         loop
            Variable_Starts :=
              Index
                (Source => Execute, Pattern => "$", From => Variable_Starts);
            exit Replace_Variables_With_Environment_Loop when Variable_Starts =
              0 or
              Variable_Starts = Length(Source => Execute);
            if not Is_Alphanumeric
                (Item =>
                   Element
                     (Source => Execute, Index => Variable_Starts + 1)) then
               goto End_Of_Variables_Loop;
            end if;
            Number_Position := Variable_Starts + 1;
            Argument_Number := Null_Unbounded_String;
            loop
               Append
                 (Source => Argument_Number,
                  New_Item =>
                    Element(Source => Execute, Index => Number_Position));
               Number_Position := Number_Position + 1;
               exit when Number_Position > Length(Execute)
                 or else not Is_Alphanumeric
                   (Element(Execute, Number_Position));
            end loop;
            if not Ada.Environment_Variables.Exists
                (To_String(Argument_Number)) then
               ShowMessage
                 ("Variable: " & To_String(Argument_Number) &
                  " doesn't exists.");
               if Output not in "standard" | "error"
                 and then Ada.Directories.Exists(Output) then
                  Delete_File(Output);
               end if;
               return;
            end if;
            Replace_Slice
              (Execute, Variable_Starts, Number_Position - 1,
               Value(To_String(Argument_Number)));
            <<End_Of_Variables_Loop>>
            Variable_Starts := Variable_Starts + 1;
         end loop Replace_Variables_With_Environment_Loop;
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
               if Output not in "standard" | "error"
                 and then Ada.Directories.Exists(Output) then
                  Delete_File(Output);
               end if;
               return;
            end if;
            Set_Directory(To_String(Arguments));
            goto End_Of_Loop;
         end if;
         Command_Path := Locate_Exec_On_Path(To_String(Command));
         if Command_Path = null then
            ShowMessage
              ("Command: '" & To_String(Command) & "' doesn't exists.");
            if Output not in "standard" | "error"
              and then Ada.Directories.Exists(Output) then
               Delete_File(Output);
            end if;
            return;
         end if;
         -- Execute command
         Command := To_Unbounded_String(Command_Path.all);
         declare
            ReturnCode: Integer;
            Arguments_List: Argument_List_Access :=
              Argument_String_To_List(To_String(Arguments));
         begin
            Spawn
              (To_String(Command), Arguments_List.all, File_Desc, ReturnCode);
            Free(Arguments_List);
            if ReturnCode > 0 then
               ShowMessage
                 ("Error during executing '" & To_String(Execute) & "'");
               if Output not in "standard" | "error"
                 and then Ada.Directories.Exists(Output) then
                  Delete_File(Output);
               end if;
               return;
            end if;
         end;
         <<End_Of_Loop>>
         Free(Command_Path);
         Command := Null_Unbounded_String;
         Arguments := Null_Unbounded_String;
      end loop Execute_Command_Loop;
   end Execute_Command;

end Commands;
