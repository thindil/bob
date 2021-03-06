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

with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

-- ****h* Commands/Commands
-- FUNCTION
-- Provide code for manipulate configured Bob commands
-- SOURCE
package Commands is
-- ****

   -- ****t* Commands/Commands.Unbounded_String_Container
   -- FUNCTION
   -- Used to store commands to execute
   -- SOURCE
   package Unbounded_String_Container is new Vectors(Index_Type => Positive,
      Element_Type => Unbounded_String);
   -- ****

   -- ****t* Commands/Commands.Variables_Container
   -- FUNCTION
   -- Used to store enviroment variables for the command
   -- SOURCE
   package Variables_Container is new Hashed_Maps(Key_Type => Unbounded_String,
      Element_Type => Unbounded_String, Hash => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=");
   -- ****

   -- ****s* Commands/Commands.Command_Record
   -- FUNCTION
   -- Data structure for the Bob commands.
   -- PARAMETERS
   -- Execute     - Commands or programs to execute with this command.
   -- Variables   - Environment variables for this command.
   -- Description - Description of the command. Will be show to the user with
   --               help command.
   -- Output      - Where to send output of this command. Possible options are:
   --               "standard" standard output (default), "error" standard
--               error output, anything other will be treated as path to file
   --               where output should be redirected.
   -- Flags       - Flags assigned to this command. Possible options are:
   --               "unixonly" command available only on Unix systems,
   --               "windowsonly" command available only on Windows,
   --               "internal" internal command, don't show on list of
   --               available commands, "evaluratevariables" treat command
   --               variables as programs to execute and assign result to the
   --               variables.
   -- SOURCE
   type Command_Record is record
      Execute: Unbounded_String_Container.Vector;
      Variables: Variables_Container.Map;
      Description: Unbounded_String;
      Output: Unbounded_String;
      Flags: Unbounded_String_Container.Vector;
   end record;
   -- ****

   -- ****d* Commands/Empty_Command
   -- FUNCTION
   -- Empty command constant
   -- SOURCE
   Empty_Command: constant Command_Record :=
     (Execute => Unbounded_String_Container.Empty_Vector,
      Variables => Variables_Container.Empty_Map,
      Description => Null_Unbounded_String,
      Output => To_Unbounded_String(Source => "standard"),
      Flags => Unbounded_String_Container.Empty_Vector);
   -- ****

   -- ****t* Commands/Commands.Commands_Container
   -- FUNCTION
   -- Used to store all available Bob commands.
   -- SOURCE
   package Commands_Container is new Ordered_Maps(Key_Type => Unbounded_String,
      Element_Type => Command_Record);
   -- ****

   -- ****f* Commands/Commands.Execute_Command
   -- FUNCTION
   -- Execute selected Bob command
   -- PARAMETERS
   -- Bob_Commands_List - The list of the program commands
   -- Key               - Command name which the user entered as a first
   --                     parameter for the program
   -- SOURCE
   procedure Execute_Command
     (Key: Unbounded_String;
      Bob_Commands_List: in out Commands_Container.Map) with
      Pre => Key /= Null_Unbounded_String,
      Test_Case => (Name => "Test_ExecuteCommand", Mode => Nominal);
      -- ****

end Commands;
