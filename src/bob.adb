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

with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;
with Commands; use Commands;
with Config; use Config;

procedure Bob is
   Version: constant String := "2.0";
begin
   LoadConfig;
   if Argument_Count = 0
     or else
     (Argument(1) = "help" or
      (not Commands_List.Contains(To_Unbounded_String(Argument(1))) and
       Argument(1) /= "about")) then
      Put_Line("Available commands are:");
      Put_Line("help - show all available commands (this screen)");
      Put_Line("about - show the program version and license info");
      for I in Commands_List.Iterate loop
         Put_Line
           (To_String(Commands_Container.Key(I)) & " - " &
            To_String(Commands_List(I).Description));
      end loop;
   elsif Argument(1) = "about" then
      Put_Line("Bob v" & Version & " Not Intelligent Console Assistant");
      New_Line;
      Put_Line("Copyright (C) 2019 Bartek thindil Jasicki");
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
   else
      ExecuteCommand;
   end if;
exception
   when An_Exception : others =>
      declare
         ErrorFile: File_Type;
         FilePath: constant String :=
           Current_Directory & Directory_Separator & "error.log";
      begin
         if Exists(FilePath) then
            Open(ErrorFile, Append_File, FilePath);
         else
            Create(ErrorFile, Append_File, FilePath);
         end if;
         Put_Line(ErrorFile, Ada.Calendar.Formatting.Image(Clock));
         Put_Line(ErrorFile, Version);
         Put_Line(ErrorFile, "Exception: " & Exception_Name(An_Exception));
         Put_Line(ErrorFile, "Message: " & Exception_Message(An_Exception));
         Put_Line
           (ErrorFile, "-------------------------------------------------");
         Put(ErrorFile, Symbolic_Traceback(An_Exception));
         Put_Line
           (ErrorFile, "-------------------------------------------------");
         Close(ErrorFile);
         Put_Line
           ("Oops, something bad happen and program crashed. Please, remember what you done before crash and report this problem at https://github.com/thindil/bob/issues (or if you prefer, on mail thindil@laeran.pl) and attach (if possible) file '" &
            FilePath & "'.");
      end;
end Bob;
