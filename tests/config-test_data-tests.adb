--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Config.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Containers; use Ada.Containers;
with Commands; use Commands;

--  begin read only
--  end read only
package body Config.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   procedure Wrap_Test_Load_Config_748c50_546b4c
     (Bob_Commands_List: in out Commands.Commands_Container.Map;
      File_Name: String := ".bob.yml") is
   begin
      begin
         pragma Assert(File_Name'Length > 0);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(config.ads:0):Test_LoadConfig test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Config.Load_Config
        (Bob_Commands_List, File_Name);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(config.ads:0:):Test_LoadConfig test commitment violated");
      end;
   end Wrap_Test_Load_Config_748c50_546b4c;
--  end read only

--  begin read only
   procedure Test_Load_Config_test_loadconfig(Gnattest_T: in out Test);
   procedure Test_Load_Config_748c50_546b4c(Gnattest_T: in out Test) renames
     Test_Load_Config_test_loadconfig;
--  id:2.2/748c505de8907e18/Load_Config/1/0/test_loadconfig/
   procedure Test_Load_Config_test_loadconfig(Gnattest_T: in out Test) is
      procedure Load_Config
        (Bob_Commands_List: in out Commands.Commands_Container.Map;
         File_Name: String := ".bob.yml") renames
        Wrap_Test_Load_Config_748c50_546b4c;
--  end read only

      pragma Unreferenced(Gnattest_T);
      Commands_List: Commands_Container.Map;

   begin

      Load_Config(Commands_List);
      Assert(Commands_List.Length = 1, "Failed to load all commands.");
      Set_Exit_Status(Code => Success);

--  begin read only
   end Test_Load_Config_test_loadconfig;
--  end read only

--  begin read only
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
   null;
--  begin read only
--  end read only
end Config.Test_Data.Tests;
