#!/usr/bin/env sh

case $1 in
   release)
      releasedir="usr"
      gprclean -P bob.gpr
      gprbuild -p bob.gpr -XMode=release
      mkdir -p "$releasedir"/share/doc
      mkdir -p "$releasedir"/bin
      cp bin/bob "$releasedir"/bin
      cp README.md "$releasedir"/share/doc
      cp COPYING "$releasedir"/share/doc
      cp CHANGELOG.md "$releasedir"/share/doc
      gprclean -P bob.gpr
      ;;
   debug)
      gprclean -P bob.gpr
      gprbuild -P bob.gpr
      ;;
   analyze)
      gprclean -P bob.gpr
      gprbuild -p bob.gpr -XMode=analyze
      ;;
   createtests)
      gnattest -P bob.gpr
      ;;
   tests)
      gprbuild -P tests/driver/test_driver.gpr
      ;;
   docs)
      ./generatedocs.py
      ;;
   gcov)
      mkdir -p gcov
      cd gcov
      ~/gnat/bin/gcov -f ../obj/*.o
      cd ..
      ;;
   gprof)
      gprof bin/bob bin/gmon.out > gprofreport.txt
      ;;
   help)
      echo "release       - Build the program in release mode"
      echo "debug         - Build the program in debug mode"
      echo "analyze       - Build the program in analyze mode for gcov and gprof"
      echo "createtests   - Regenerate unit tests"
      echo "tests         - Build unit tests"
      echo "docs          - Generate code documentation"
      echo "gcov          - Generate gcov reports for each file in gcov directory. You may need to change gcov path in this script to work"
      echo "gprof         - Generate gprof report in main directory"
      echo "help          - This screen"
      ;;
   *)
      echo "Unknown command, possible options are: release, debug, createtests, tests, docs, gcov, gprof, help"
      ;;
esac
