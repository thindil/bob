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

-- ****h* Messages/Messages
-- FUNCTION
-- Provides code to show the program messages to the user
-- SOURCE
package Messages is
-- ****

   -- ****t* Messages/Messages.Messages_Types
   -- FUNCTION
   -- Types of messages: Normal or Error
   -- SOURCE
   type Messages_Types is (NORMAL, ERROR);
   -- ****

   -- ****f* Messages/Messages.Show_Message
   -- FUNCTION
   -- Show selected message to the user. On Unix Error type messages are
   -- colored
   -- PARAMETERS
   -- Text         - Text to show to the user
   -- Message_Type - Type of message. Default is Error
   -- SOURCE
   procedure Show_Message(Text: String; Message_Type: Messages_Types := ERROR);
   -- ****

end Messages;
