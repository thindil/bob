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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package body Messages is

   procedure Show_Message
     (Text: String; Message_Type: Messages_Types := Default_Message_Type) is
   begin
      -- If operating system is Unix and message type is Error, color it in red
      if Directory_Separator = '/' and then Message_Type = ERROR then
         Put(Item => ESC & "[31m");
      end if;
      Put_Line(Item => Text);
      -- Reset text color in terminal if needed
      if Directory_Separator = '/' and then Message_Type /= NORMAL then
         Put(Item => ESC & "[0m");
      end if;
      if Message_Type = ERROR then
         Set_Exit_Status(Code => Failure);
      end if;
   end Show_Message;

end Messages;
