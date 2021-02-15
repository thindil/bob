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

with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Containers; use Ada.Containers;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;
with Commands; use Commands;
with Config; use Config;
with Messages; use Messages;

procedure Bob is
begin
   -- Load local configuration file
   LoadConfig;
   -- Show list of available commands
   if Argument_Count = 0
     or else
     (Argument(Number => 1) = "help" or
      (not Commands_List.Contains
         (Key => To_Unbounded_String(Source => Argument(Number => 1))) and
       Argument(Number => 1) not in "about" | "config" | "show")) then
      -- Show info about unknown command
      if Argument_Count > 0
        and then
        (not Commands_List.Contains
           (Key => To_Unbounded_String(Source => Argument(Number => 1))) and
         Argument(Number => 1) /= "help") then
         ShowMessage(Text => "Unknown command '" & Argument(Number => 1) & "'.");
      end if;
      Put_Line(Item => "Available commands are:");
      Put_Line(Item => "##### Build-in commands #####");
      declare
         StringLength: Positive := 6;
      begin
         Get_String_Length_Loop :
         for I in Commands_List.Iterate loop
            if Length(Commands_Container.Key(I)) > StringLength then
               StringLength := Length(Commands_Container.Key(I));
            end if;
         end loop Get_String_Length_Loop;
         declare
            procedure AddEntry(Key, Description: Unbounded_String) is
               KeyString: String(1 .. StringLength) := (others => ' ');
            begin
               KeyString(1 .. Length(Key)) := To_String(Key);
               Put_Line(KeyString & " - " & To_String(Description));
            end AddEntry;
         begin
            AddEntry
              (To_Unbounded_String("help"),
               To_Unbounded_String
                 ("show all available commands (this screen)"));
            AddEntry
              (To_Unbounded_String("about"),
               To_Unbounded_String
                 ("show the program version and license info"));
            AddEntry
              (To_Unbounded_String("config"),
               To_Unbounded_String
                 ("rename the selected file to .bob.yml or add it content to the existing .bob.yml"));
            AddEntry
              (To_Unbounded_String("show"),
               To_Unbounded_String
                 ("show the content of the selected local command"));
            Put_Line("##### Local commands ########");
            Get_Commands_Loop :
            for I in Commands_List.Iterate loop
               if not Commands_List(I).Flags.Contains
                   (To_Unbounded_String("internal")) then
                  AddEntry
                    (Commands_Container.Key(I), Commands_List(I).Description);
               end if;
            end loop Get_Commands_Loop;
         end;
      end;
      -- Show information about the program
   elsif Argument(Number => 1) = "about" then
      Put_Line("Bob v" & Version & " Not Intelligent Console Assistant");
      New_Line;
      Put_Line("Copyright (C) 2019-2021 Bartek thindil Jasicki");
      New_Line;
      Put_Line
        ("This program is free software: you can redistribute it and/or modify");
      Put_Line
        ("it under the terms of the GNU General Public License as published by");
      Put_Line
        ("the Free Software Foundation, either version 3 of the License, or");
      Put_Line("(at your option) any later version.");
      New_Line;
      Put_Line
        ("This program is distributed in the hope that it will be useful,");
      Put_Line
        ("but WITHOUT ANY WARRANTY; without even the implied warranty of");
      Put_Line
        ("MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the");
      Put_Line("GNU General Public License for more details.");
      New_Line;
      Put_Line
        ("You should have received a copy of the GNU General Public License");
      Put_Line
        ("along with this program.  If not, see <https://www.gnu.org/licenses/>.");
      -- Convert file to .bob.yml or add its command to it
   elsif Argument(Number => 1) = "config" then
      if Argument_Count < 2 then
         ShowMessage
           ("You have to enter the name of the file which will be added to .bob.yml");
         return;
      end if;
      if not Exists(".bob.yml") then
         Copy_File(Argument(Number => 2), ".bob.yml");
         ShowMessage
           ("File '" & Argument(Number => 2) & "' was copied as .bob.yml",
            Normal);
      else
         declare
            Source_File, Config_File: File_Type;
         begin
            Open(Source_File, In_File, Argument(Number => 2));
            Open(Config_File, Append_File, ".bob.yml");
            Copy_Configuration_Loop :
            while not End_Of_File(Source_File) loop
               Put_Line(Config_File, Get_Line(Source_File));
            end loop Copy_Configuration_Loop;
            Close(Config_File);
            Close(Source_File);
         end;
         ShowMessage
           ("File '" & Argument(Number => 2) &
            "' content was copied to .bob.yml file",
            Normal);
      end if;
      -- Show the content of the selected command
   elsif Argument(Number => 1) = "show" then
      if Argument_Count < 2 then
         ShowMessage
           ("You have to enter the name of the command which content you want to see.");
         return;
      end if;
      if not Commands_List.Contains(To_Unbounded_String(Argument(2))) then
         ShowMessage
           ("Command: '" & Argument(Number => 2) & "' doesn't exists.");
         return;
      end if;
      for I in Commands_List.Iterate loop
         if Commands_Container.Key(I) =
           To_Unbounded_String(Argument(Number => 2)) then
            Put_Line("##### Variables ####");
            if Commands_List(I).Variables.Length = 0 then
               Put_Line("no variables declared");
            end if;
            List_Variables_Loop :
            for J in Commands_List(I).Variables.Iterate loop
               Put_Line
                 (To_String(Variables_Container.Key(J)) & " = " &
                  To_String(Commands_List(I).Variables(J)));
            end loop List_Variables_Loop;
            Put_Line("##### Commands #####");
            List_Commands_Loop :
            for Command of Commands_List(I).Execute loop
               Put_Line(To_String(Command));
            end loop List_Commands_Loop;
            Put_Line("##### Flags ########");
            if Commands_List(I).Flags.Length = 0 then
               Put_Line("no flags assigned");
            end if;
            List_Flags_Loop :
            for Flag of Commands_List(I).Flags loop
               Put_Line(To_String(Flag));
            end loop List_Flags_Loop;
            Put_Line("##### Output #######");
            Put_Line(To_String(Commands_List(I).Output));
         end if;
      end loop;
      -- Execute entered command
   else
      ExecuteCommand(To_Unbounded_String(Argument(Number => 1)));
   end if;
exception
   when An_Exception : others =>
      declare
         ErrorFile: File_Type;
         FilePath: constant String :=
           Current_Directory & Directory_Separator & "error.log";
      begin
         ShowMessage
           ("Oops, something bad happen and program crashed. Please, remember what you done before crash and report this problem at https://www.laeran.pl/repositories/bob/wiki?name=Contact and attach (if possible) file '" &
            FilePath & "'.");
         New_Line;
         if Exists(FilePath) then
            Open(ErrorFile, Append_File, FilePath);
         else
            Create(ErrorFile, Append_File, FilePath);
         end if;
         Put_Line(ErrorFile, Ada.Calendar.Formatting.Image(Clock));
         Put_Line(ErrorFile, Version);
         Put_Line(ErrorFile, "Exception: " & Exception_Name(An_Exception));
         Put_Line("Exception: " & Exception_Name(An_Exception));
         Put_Line(ErrorFile, "Message: " & Exception_Message(An_Exception));
         Put_Line("Message: " & Exception_Message(An_Exception));
         Put_Line
           (ErrorFile, "-------------------------------------------------");
         Put(ErrorFile, Symbolic_Traceback(An_Exception));
         Put_Line(Symbolic_Traceback(An_Exception));
         Put_Line
           (ErrorFile, "-------------------------------------------------");
         Close(ErrorFile);
      end;
end Bob;
