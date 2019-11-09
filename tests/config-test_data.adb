--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with Ada.Directories; use Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

package body Config.Test_Data is

   procedure Set_Up (Gnattest_T : in out Test) is
      pragma Unreferenced (Gnattest_T);
      ConfigFile: File_Type;
   begin
      Create(ConfigFile, Out_File, ".bob.yml");
      Put_Line(ConfigFile, "- command:");
      Put_Line(ConfigFile, "  name: test");
      Put_Line(ConfigFile, "  execute:");
      Put_Line(ConfigFile, "    - ls -a .");
      Put_Line(ConfigFile, "  description: test command");
      Put_Line(ConfigFile, "  output: standard");
      Put_Line(ConfigFile, "- command:");
      Put_Line(ConfigFile, "  name: test");
      Put_Line(ConfigFile, "  execute:");
      Put_Line(ConfigFile, "    - ls -a .");
      Put_Line(ConfigFile, "  description: test double command");
      Put_Line(ConfigFile, "- command:");
      Put_Line(ConfigFile, "  name: test2");
      Put_Line(ConfigFile, "  description: test incomplete command");
      Put_Line(ConfigFile, "- command:");
      Put_Line(ConfigFile, "  name: test3");
      Put_Line(ConfigFile, "  execute:");
      Put_Line(ConfigFile, "  description: test double command");
      Put_Line(ConfigFile, "- command:");
      Put_Line(ConfigFile, "  name: test4");
      Put_Line(ConfigFile, "  execute:");
      Put_Line(ConfigFile, "    - ls -a .");
      Put_Line(ConfigFile, "  description: test empty output command");
      Put_Line(ConfigFile, "  output:");
      Put_Line(ConfigFile, "- command:");
      Put_Line(ConfigFile, "  name:");
      Put_Line(ConfigFile, "  execute:");
      Put_Line(ConfigFile, "    - ls -a .");
      Put_Line(ConfigFile, "  description: test command without name");
      Close(ConfigFile);
   end Set_Up;

   procedure Tear_Down (Gnattest_T : in out Test) is
      pragma Unreferenced (Gnattest_T);
   begin
      null;
   end Tear_Down;end Config.Test_Data;
