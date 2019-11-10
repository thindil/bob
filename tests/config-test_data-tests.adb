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
   procedure Wrap_Test_LoadConfig_6c9f25_6dc4a6 (FileName: String := ".bob.yml") 
   is
   begin
      begin
         pragma Assert
           (FileName'Length > 0);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(config.ads:0):Test_LoadConfig test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Config.LoadConfig (FileName);
      begin
         pragma Assert
           (True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(config.ads:0:):Test_LoadConfig test commitment violated");
      end;
   end Wrap_Test_LoadConfig_6c9f25_6dc4a6;
--  end read only

--  begin read only
   procedure Test_LoadConfig_test_loadconfig (Gnattest_T : in out Test);
   procedure Test_LoadConfig_6c9f25_6dc4a6 (Gnattest_T : in out Test) renames Test_LoadConfig_test_loadconfig;
--  id:2.2/6c9f251425eed056/LoadConfig/1/0/test_loadconfig/
   procedure Test_LoadConfig_test_loadconfig (Gnattest_T : in out Test) is
   procedure LoadConfig (FileName: String := ".bob.yml") renames Wrap_Test_LoadConfig_6c9f25_6dc4a6;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Commands_List.Clear;
      LoadConfig;
      Assert(Commands_List.Length = 1, "Failed to load all commands.");

--  begin read only
   end Test_LoadConfig_test_loadconfig;
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
