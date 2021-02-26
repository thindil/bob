## Bugs reporting

Bugs are not only problems or the program crashes, but also typos. If you
find any bug in the program, please report it at options available at [contact page](https://www.laeran.pl/repositories/bob/wiki?name=Contact).

### Some general hints about reporting bugs

* In "Title" field try to write short but not too general description of
  problem. Good example: "The program crash when running command". Bad
  example: "The program crashes often."
* In body/comment field try to write that much information about problem as
  possible. In most cases more information is better than less. General rule
  of good problem report is give enough information which allow to reproduce
  problem by other people. It may be in form of steps which are needed for
  cause problem.
* If the program crashed, in most cases it should create file *error.log* in
  current directory. It will be a lot of help if you can attach that file to
  the bug report. Each bug information in this file contains: date when crash
  happens, version of the program used, source code file and line in this
  file. If the program can't  discover source code file, it will write memory
  address instead. You can check this last information by using command
  `addr2line` in directory where *bob* executable file is. Example:

  `addr2line -e bob [here full list of memory addresses from error.log]`

### Example of bug report:

Title: "The program crashed when trying to run command"

Body:

1. In terminal, enter: bob mycommand
2. Press enter
3. The program crashes

## Features propositions

If you want to talk/propose changes in any existing the prorgam feature or
mechanic, feel free to contact me options available at [contact page](https://www.laeran.pl/repositories/bob/wiki?name=Contact). General rule about propositions is same as for bugs reports - please,
try to write that much information as possible. This help us all better
understand purpose of your changes.

## Code propositions

### General information

If you want to start help in the program development, please consider starts
from something easy like fixing bugs. Before you been want to add new feature
to the program, please contact with me by options available at [contact page](https://www.laeran.pl/repositories/bob/wiki?name=Contact). Same as with features proposition - your code may "collide" with
my work and it this moment you may just lose time by working on it. So it is
better that we first discuss your proposition. In any other case, fell free
to fix my code.

### Coding standard

The full description of coding style used by the project, you can find on the
[Coding Standard](https://www.laeran.pl/repositories/bob/wiki?name=Coding%20Standard) page.
There also you will find some information about the testing process of the
project.

#### Code comments formatting

The program uses [ROBODoc](https://rfsber.home.xs4all.nl/Robo/) for generating
code documentation. When you write your own code, please add proper header
documentation to it. If you use Vim/NeoVim, the easiest way is to use plugin
[RoboVim](https://github.com/thindil/robovim). Example of documentation
header:

    1 -- ****f* Utils/GetRandom
    2 -- FUNCTION
    3 -- Return random number from Min to Max range
    4 -- PARAMETERS
    5 -- Min - Starting value from which generate random number
    6 -- Max - End value from which generate random number
    7 -- RESULT
    8 -- Random number between Min and Max
    9 -- SOURCE
    10 function GetRandom(Min, Max: Integer) return Integer;
    11 -- ****

1 - Documentation header. Bob uses `-- ****[letter]* [package]/[itemname]`
format for documentation headers.

2-9 - Documentation. For all available options, please refer to ROBODoc
documentation. Bob uses `-- ` for start all documenation lines.

10 - Source code of item.

11 - Documentation footer. Bob uses `-- ****` for closing documentation.

How to generate the code documentation is described in main *README.md* file.

### Code submission

A preferred way to submit your code is to use [tickets](https://www.laeran.pl/repositories/bob/ticket)
on the project page. Please attach to that ticket file with diff changes,
the best if done with command `fossil diff`. But any other diff program will
work. In that situation, please add information which program was used to
create the diff file. If you prefer you can also use other options from
[contact page](https://www.laeran.pl/repositories/bob/wiki?name=Contact).

## Additional debugging options

### Code analysis

To enable check for `gcov` (code coverage) and `gprof` (code profiling) compile
the program with mode `analyze` (in main project directory, where *bob.gpr*
file is):

`gprbuild -P bob.gpr -XMode=analyze`

or, if you have Bob installed, command:

`bob analyze`

More information about code coverage and profiling, you can find in proper
documentation for both programs.
