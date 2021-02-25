--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Commands.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

with Config; use Config;

--  begin read only
--  end read only
package body Commands.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   procedure Wrap_Test_Execute_Command_96c470_533ac5 (Key: Unbounded_String) 
   is
   begin
      begin
         pragma Assert
           (Key /= Null_Unbounded_String);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(commands.ads:0):Test_ExecuteCommand test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Commands.Execute_Command (Key);
      begin
         pragma Assert
           (True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(commands.ads:0:):Test_ExecuteCommand test commitment violated");
      end;
   end Wrap_Test_Execute_Command_96c470_533ac5;
--  end read only

--  begin read only
   procedure Test_Execute_Command_test_executecommand (Gnattest_T : in out Test);
   procedure Test_Execute_Command_96c470_533ac5 (Gnattest_T : in out Test) renames Test_Execute_Command_test_executecommand;
--  id:2.2/96c4708ae6b283e4/Execute_Command/1/0/test_executecommand/
   procedure Test_Execute_Command_test_executecommand (Gnattest_T : in out Test) is
   procedure Execute_Command (Key: Unbounded_String) renames Wrap_Test_Execute_Command_96c470_533ac5;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

        LoadConfig;
        -- Test existing command
        Execute_Command(To_Unbounded_String("test"));
        -- Test non-existing command
        Execute_Command(To_Unbounded_String("about"));
        Assert(True, "This test can only crash.");

--  begin read only
   end Test_Execute_Command_test_executecommand;
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
end Commands.Test_Data.Tests;
