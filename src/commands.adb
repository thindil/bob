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

with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings;
with Ada.Strings.Maps;
with GNAT.Expect;
with GNAT.OS_Lib;
with Messages;

package body Commands is

   procedure Execute_Command
     (Key: Unbounded_String;
      Bob_Commands_List: in out Commands_Container.Map) is
      use Ada.Characters.Handling;
      use Ada.Command_Line;
      use Ada.Directories;
      use Ada.Environment_Variables;
      use Ada.Strings.Maps;
      use GNAT.OS_Lib;
      use Messages;

      Command, Arguments, Argument_Number: Unbounded_String :=
        Null_Unbounded_String;
      Variable_Starts, Number_Position: Natural := 1;
      Command_Path: GNAT.OS_Lib.String_Access;
      File_Desc: File_Descriptor := 0;
      procedure Delete_Output_File
        (The_Commands_List: in out Commands_Container.Map) is
      begin
         if To_String(Source => The_Commands_List(Key).Output) not in
             "standard" | "error"
           and then Ada.Directories.Exists
             (Name => To_String(Source => The_Commands_List(Key).Output)) then
            Delete_File
              (Name => To_String(Source => The_Commands_List(Key).Output));
         end if;
      end Delete_Output_File;
   begin
      if not Bob_Commands_List.Contains(Key => Key) then
         Show_Message
           (Text =>
              "No available command with name '" & To_String(Source => Key) &
              "'.");
         return;
      end if;
      -- Load environment variables if needed
      Load_Environment_Variables_Block :
      declare
         use GNAT.Expect;

         Evaluate_Variables: constant Boolean :=
           Bob_Commands_List(Key).Flags.Contains
             (Item => To_Unbounded_String(Source => "evaluatevariables"));
         Descriptor: Process_Descriptor; --## rule line off IMPROPER_INITIALIZATION
         Args: Argument_List_Access;
         Result: Expect_Match := -1;
      begin
         Set_Environment_Variables_Loop :
         for I in Bob_Commands_List(Key).Variables.Iterate loop
            -- If proper flag is set, evaluate environment variable before
            -- set it
            if Evaluate_Variables then
               Args :=
                 Argument_String_To_List
                   (Arg_String =>
                      To_String
                        (Source => Bob_Commands_List(Key).Variables(I)));
               Non_Blocking_Spawn
                 (Descriptor => Descriptor, Command => Args(Args'First).all,
                  Args => Args(Args'First + 1 .. Args'Last));
               Expect
                 (Descriptor => Descriptor, Result => Result, Regexp => ".+",
                  Timeout => 10_000);
               case Result is
                  when Expect_Timeout =>
                     Show_Message
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
               -- Just set environment variable
            else
               Set
                 (Name =>
                    To_String
                      (Source => Variables_Container.Key(Position => I)),
                  Value =>
                    To_String(Source => Bob_Commands_List(Key).Variables(I)));
            end if;
         end loop Set_Environment_Variables_Loop;
      end Load_Environment_Variables_Block;
      if Bob_Commands_List(Key).Output = "standard" then
         File_Desc := Standout;
      elsif Bob_Commands_List(Key).Output = "error" then
         File_Desc := Standerr;
      else
         if Ada.Directories.Exists
             (Name => To_String(Source => Bob_Commands_List(Key).Output)) then
            Delete_File
              (Name => To_String(Source => Bob_Commands_List(Key).Output));
         end if;
         File_Desc :=
           Create_Output_Text_File
             (Name => To_String(Source => Bob_Commands_List(Key).Output));
         if File_Desc = Invalid_FD then
            Show_Message
              (Text =>
                 "Error during executing '" & To_String(Source => Key) &
                 "'. Can't create '" &
                 To_String(Source => Bob_Commands_List(Key).Output) &
                 "' as the output file.");
            return;
         end if;
      end if;
      Execute_Command_Loop :
      for Execute of Bob_Commands_List(Key).Execute loop
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
            exit Replace_Variable_With_Argument_Loop when Variable_Starts in
                0 | Length(Source => Execute);
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
               Show_Message
                 (Text =>
                    "You didn't entered enough arguments for this command. Please check it description for information what should be entered.");
               Delete_Output_File(The_Commands_List => Bob_Commands_List);
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
            exit Replace_Variables_With_Environment_Loop when Variable_Starts in
                0 | Length(Source => Execute);
            if not Is_Alphanumeric
                (Item =>
                   Element
                     (Source => Execute, Index => Variable_Starts + 1)) then
               goto End_Of_Variables_Loop;
            end if;
            Number_Position := Variable_Starts + 1;
            Argument_Number := Null_Unbounded_String;
            Set_Argument_Number_Loop :
            loop
               Append
                 (Source => Argument_Number,
                  New_Item =>
                    Element(Source => Execute, Index => Number_Position));
               Number_Position := Number_Position + 1;
               exit Set_Argument_Number_Loop when Number_Position >
                 Length(Source => Execute)
                 or else not Is_Alphanumeric
                   (Item =>
                      Element(Source => Execute, Index => Number_Position));
            end loop Set_Argument_Number_Loop;
            if not Ada.Environment_Variables.Exists
                (Name => To_String(Source => Argument_Number)) then
               Show_Message
                 (Text =>
                    "Variable: " & To_String(Source => Argument_Number) &
                    " doesn't exists.");
               Delete_Output_File(The_Commands_List => Bob_Commands_List);
               return;
            end if;
            Replace_Slice
              (Source => Execute, Low => Variable_Starts,
               High => Number_Position - 1,
               By => Value(Name => To_String(Source => Argument_Number)));
            <<End_Of_Variables_Loop>>
            Variable_Starts := Variable_Starts + 1;
         end loop Replace_Variables_With_Environment_Loop;
         -- Split command line
         Split_Command_Line_Block :
         declare
            Start_Index: Positive range 1 .. 2;
            End_Index: Integer range -1 .. Length(Source => Execute) + 3;
         begin
            case Element(Source => Execute, Index => 1) is
               when ''' =>
                  Start_Index := 2;
                  End_Index :=
                    Index(Source => Execute, Pattern => "'", From => 2) - 1;
               when '"' =>
                  Start_Index := 2;
                  End_Index :=
                    Index(Source => Execute, Pattern => """", From => 2) - 1;
               when others =>
                  Start_Index := 1;
                  End_Index :=
                    Index(Source => Execute, Pattern => " ", From => 2) - 1;
            end case;
            if End_Index < 1 then
               End_Index := Length(Source => Execute);
            end if;
            Command :=
              Unbounded_Slice
                (Source => Execute, Low => Start_Index, High => End_Index);
            case Element(Source => Execute, Index => 1) is
               when ''' | '"' =>
                  End_Index := End_Index + 3;
               when others =>
                  End_Index := End_Index + 2;
            end case;
            if End_Index < Length(Source => Execute) then
               Arguments :=
                 Unbounded_Slice
                   (Source => Execute, Low => End_Index,
                    High => Length(Source => Execute));
            end if;
         end Split_Command_Line_Block;
         -- Translate path if needed
         if Bob_Commands_List(Key).Flags.Contains
             (Item => To_Unbounded_String(Source => "windowspath")) and
           Directory_Separator = '/' then
            Translate
              (Source => Arguments,
               Mapping => To_Mapping(From => "\", To => "/"));
         elsif Bob_Commands_List(Key).Flags.Contains
             (Item => To_Unbounded_String(Source => "unixpath")) and
           Directory_Separator = '\' then
            Translate
              (Source => Arguments,
               Mapping => To_Mapping(From => "/", To => "\"));
         end if;
         -- Enter selected directory
         if Command = To_Unbounded_String(Source => "cd") then
            Arguments :=
              To_Unbounded_String
                (Source =>
                   Normalize_Pathname(Name => To_String(Source => Arguments)));
            if not Ada.Directories.Exists
                (Name => To_String(Source => Arguments)) then
               Show_Message
                 (Text =>
                    "Directory: '" & To_String(Source => Arguments) &
                    "' doesn't exists.");
               Delete_Output_File(The_Commands_List => Bob_Commands_List);
               return;
            end if;
            Set_Directory(Directory => To_String(Source => Arguments));
            goto End_Of_Loop;
         end if;
         Command_Path :=
           Locate_Exec_On_Path(Exec_Name => To_String(Source => Command));
         if Command_Path = null then
            Show_Message
              (Text =>
                 "Command: '" & To_String(Source => Command) &
                 "' doesn't exists.");
            Delete_Output_File(The_Commands_List => Bob_Commands_List);
            return;
         end if;
         -- Execute command
         Command := To_Unbounded_String(Source => Command_Path.all);
         Execute_Command_Block :
         declare
            Return_Code: Integer;
            Arguments_List: Argument_List_Access :=
              Argument_String_To_List
                (Arg_String => To_String(Source => Arguments));
         begin
            Spawn
              (Program_Name => To_String(Source => Command),
               Args => Arguments_List.all, Output_File_Descriptor => File_Desc,
               Return_Code => Return_Code);
            Free(Arg => Arguments_List);
            if Return_Code > 0 then
               Show_Message
                 (Text =>
                    "Error during executing '" & To_String(Source => Execute) &
                    "'");
               Delete_Output_File(The_Commands_List => Bob_Commands_List);
               return;
            end if;
         end Execute_Command_Block;
         <<End_Of_Loop>>
         Free(X => Command_Path);
         Command := Null_Unbounded_String;
         Arguments := Null_Unbounded_String;
      end loop Execute_Command_Loop;
   end Execute_Command;

end Commands;
