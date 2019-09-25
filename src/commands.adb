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
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.String_Split; use GNAT.String_Split;

package body Commands is

   procedure ExecuteCommand is
      Key: constant Unbounded_String := To_Unbounded_String(Argument(1));
      Success: Boolean;
      Tokens, SubTokens: Slice_Set;
      Command, Arguments: Unbounded_String := Null_Unbounded_String;
   begin
      Create(Tokens, To_String(Commands_List(Key).Execute), "&&");
      for I in 1 .. Slice_Count(Tokens) loop
         if Slice(Tokens, I)'Length = 0 then
            goto End_Of_Loop;
         end if;
         Create(SubTokens, Slice(Tokens, I), " ");
         if Slice(SubTokens, 1)'Length > 0 then
            Append(Command, Locate_Exec_On_Path(Slice(SubTokens, 1)).all);
         else
            Append(Command, Slice(SubTokens, 2));
         end if;
         for I in 2 .. Slice_Count(SubTokens) loop
            Append(Arguments, " " & Slice(SubTokens, I));
         end loop;
         Spawn(To_String(Command), Argument_String_To_List(To_String(Arguments)).all, Success);
         if not Success then
            Put_Line("Error during executing '" & To_String(Command) & "'");
            exit;
         end if;
         Command := Null_Unbounded_String;
         Arguments := Null_Unbounded_String;
         <<End_Of_Loop>>
      end loop;
   end ExecuteCommand;

end Commands;
