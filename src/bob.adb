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

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Commands; use Commands;
with Config; use Config;

procedure Bob is
begin
   LoadConfig;
   if Argument_Count = 0 or else Argument(1) = "help" then
      Put_Line("Available commands are:");
      Put_Line("help - show all available commands (this screen)");
      for I in Commands_List.Iterate loop
         Put_Line
           (To_String(Commands_Container.Key(I)) & " - " &
            To_String(Commands_List(I).Description));
      end loop;
   else
      ExecuteCommand;
   end if;
end Bob;
