#!/bin/sh

if [ -n $1 ] && [ "$1" = "tests" ]
then
   # run unit tests instead of the program
   cd tests/driver || exit
   ./test_runner
else
   # run the program
   bin/bob "$@"
fi
