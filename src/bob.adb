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
         ShowMessage
           (Text => "Unknown command '" & Argument(Number => 1) & "'.");
      end if;
      Put_Line(Item => "Available commands are:");
      Put_Line(Item => "##### Build-in commands #####");
      Show_Commands_Block :
      declare
         String_Length: Positive := 6;
         procedure Add_Entry(Key, Description: Unbounded_String) is
            Key_String: String(1 .. String_Length) := (others => ' ');
         begin
            Key_String(1 .. Length(Source => Key)) := To_String(Source => Key);
            Put_Line
              (Item => Key_String & " - " & To_String(Source => Description));
         end Add_Entry;
      begin
         Get_String_Length_Loop :
         for I in Commands_List.Iterate loop
            if Length(Source => Commands_Container.Key(Position => I)) >
              String_Length then
               String_Length :=
                 Length(Source => Commands_Container.Key(Position => I));
            end if;
         end loop Get_String_Length_Loop;
         Add_Entry
           (Key => To_Unbounded_String(Source => "help"),
            Description =>
              To_Unbounded_String
                (Source => "show all available commands (this screen)"));
         Add_Entry
           (Key => To_Unbounded_String(Source => "about"),
            Description =>
              To_Unbounded_String
                (Source => "show the program version and license info"));
         Add_Entry
           (Key => To_Unbounded_String(Source => "config"),
            Description =>
              To_Unbounded_String
                (Source =>
                   "rename the selected file to .bob.yml or add it content to the existing .bob.yml"));
         Add_Entry
           (Key => To_Unbounded_String(Source => "show"),
            Description =>
              To_Unbounded_String
                (Source => "show the content of the selected local command"));
         Put_Line(Item => "##### Local commands ########");
         Get_Commands_Loop :
         for I in Commands_List.Iterate loop
            if not Commands_List(I).Flags.Contains
                (Item => To_Unbounded_String(Source => "internal")) then
               Add_Entry
                 (Key => Commands_Container.Key(Position => I),
                  Description => Commands_List(I).Description);
            end if;
         end loop Get_Commands_Loop;
      end Show_Commands_Block;
      -- Show information about the program
   elsif Argument(Number => 1) = "about" then
      Put_Line
        (Item => "Bob v" & Version & " Not Intelligent Console Assistant");
      New_Line;
      Put_Line(Item => "Copyright (C) 2019-2021 Bartek thindil Jasicki");
      New_Line;
      Put_Line
        (Item =>
           "This program is free software: you can redistribute it and/or modify");
      Put_Line
        (Item =>
           "it under the terms of the GNU General Public License as published by");
      Put_Line
        (Item =>
           "the Free Software Foundation, either version 3 of the License, or");
      Put_Line(Item => "(at your option) any later version.");
      New_Line;
      Put_Line
        (Item =>
           "This program is distributed in the hope that it will be useful,");
      Put_Line
        (Item =>
           "but WITHOUT ANY WARRANTY; without even the implied warranty of");
      Put_Line
        (Item =>
           "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the");
      Put_Line(Item => "GNU General Public License for more details.");
      New_Line;
      Put_Line
        (Item =>
           "You should have received a copy of the GNU General Public License");
      Put_Line
        (Item =>
           "along with this program.  If not, see <https://www.gnu.org/licenses/>.");
      -- Convert file to .bob.yml or add its command to it
   elsif Argument(Number => 1) = "config" then
      if Argument_Count < 2 then
         ShowMessage
           (Text =>
              "You have to enter the name of the file which will be added to .bob.yml");
         return;
      end if;
      if not Exists(Name => ".bob.yml") then
         Copy_File
           (Source_Name => Argument(Number => 2), Target_Name => ".bob.yml");
         ShowMessage
           (Text =>
              "File '" & Argument(Number => 2) & "' was copied as .bob.yml",
            MType => Normal);
         return;
      end if;
      Copy_Configuration_Block :
      declare
         Source_File, Config_File: File_Type;
      begin
         Open
           (File => Source_File, Mode => In_File,
            Name => Argument(Number => 2));
         Open(File => Config_File, Mode => Append_File, Name => ".bob.yml");
         Copy_Configuration_Loop :
         while not End_Of_File(File => Source_File) loop
            Put_Line
              (File => Config_File, Item => Get_Line(File => Source_File));
         end loop Copy_Configuration_Loop;
         Close(File => Config_File);
         Close(File => Source_File);
      end Copy_Configuration_Block;
      ShowMessage
        (Text =>
           "File '" & Argument(Number => 2) &
           "' content was copied to .bob.yml file",
         MType => Normal);
      -- Show the content of the selected command
   elsif Argument(Number => 1) = "show" then
      if Argument_Count < 2 then
         ShowMessage
           (Text =>
              "You have to enter the name of the command which content you want to see.");
         return;
      end if;
      if not Commands_List.Contains
          (Key => To_Unbounded_String(Source => Argument(Number => 2))) then
         ShowMessage
           (Text =>
              "Command: '" & Argument(Number => 2) & "' doesn't exists.");
         return;
      end if;
      Show_Command_Content_Loop :
      for I in Commands_List.Iterate loop
         if Commands_Container.Key(Position => I) =
           To_Unbounded_String(Source => Argument(Number => 2)) then
            Put_Line(Item => "##### Variables ####");
            if Commands_List(I).Variables.Length = 0 then
               Put_Line(Item => "no variables declared");
            end if;
            List_Variables_Loop :
            for J in Commands_List(I).Variables.Iterate loop
               Put_Line
                 (Item =>
                    To_String
                      (Source => Variables_Container.Key(Position => J)) &
                    " = " &
                    To_String(Source => Commands_List(I).Variables(J)));
            end loop List_Variables_Loop;
            Put_Line(Item => "##### Commands #####");
            List_Commands_Loop :
            for Command of Commands_List(I).Execute loop
               Put_Line(Item => To_String(Source => Command));
            end loop List_Commands_Loop;
            Put_Line(Item => "##### Flags ########");
            if Commands_List(I).Flags.Length = 0 then
               Put_Line(Item => "no flags assigned");
            end if;
            List_Flags_Loop :
            for Flag of Commands_List(I).Flags loop
               Put_Line(Item => To_String(Flag));
            end loop List_Flags_Loop;
            Put_Line(Item => "##### Output #######");
            Put_Line(Item => To_String(Source => Commands_List(I).Output));
         end if;
      end loop Show_Command_Content_Loop;
      -- Execute entered command
   else
      ExecuteCommand
        (Key => To_Unbounded_String(Source => Argument(Number => 1)));
   end if;
exception
   when An_Exception : others =>
      Unhandled_Exception_Block :
      declare
         Error_File: File_Type;
         File_Path: constant String :=
           Current_Directory & Directory_Separator & "error.log";
      begin
         ShowMessage
           ("Oops, something bad happen and program crashed. Please, remember what you done before crash and report this problem at https://www.laeran.pl/repositories/bob/wiki?name=Contact and attach (if possible) file '" &
            File_Path & "'.");
         New_Line;
         if Exists(File_Path) then
            Open(Error_File, Append_File, File_Path);
         else
            Create(Error_File, Append_File, File_Path);
         end if;
         Put_Line(Error_File, Ada.Calendar.Formatting.Image(Clock));
         Put_Line(Error_File, Version);
         Put_Line(Error_File, "Exception: " & Exception_Name(An_Exception));
         Put_Line("Exception: " & Exception_Name(An_Exception));
         Put_Line(Error_File, "Message: " & Exception_Message(An_Exception));
         Put_Line("Message: " & Exception_Message(An_Exception));
         Put_Line
           (Error_File, "-------------------------------------------------");
         Put(Error_File, Symbolic_Traceback(An_Exception));
         Put_Line(Symbolic_Traceback(An_Exception));
         Put_Line
           (Error_File, "-------------------------------------------------");
         Close(Error_File);
      end Unhandled_Exception_Block;
end Bob;
