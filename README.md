## General information

*NOTE*: Bob is in an early stage of the development so, some of described
things here are not implemented yet. Additionally, many things may change
or break, use at your own risk.

Bob is Not Intelligent console assistant. Bod doesn't try to replace any
existing shells or build systems. It is designed for extend them. Very often
when you have few projects to work, they use different commands or build
systems to maintain. To organize it often you have to add or global aliases
to your shell or create scripts which grow over time to maintain it. Bob
trying to solve this problem by adding ability to create local aliases for
each directory. It doing this by creating configuration files (in YAML) with
defined aliases in each directory. When Bob is executed in selected directory,
it reads main configuration file of the program and configuration file
(if exists) for selected directory for aliases.

### Configuration file

Bob looking for file `.bob.yml` in current directory (where it is executed).
If it find it, it reads all available commands from here. Sample default
configuration:

     - command:
       name: list
       execute:
           - ls .
       description: Show content of current directory

Explanation:

* `-command:`   - Each command for Bob must starts from this line. Each
                  configuration file may have as much as you want commands
                  defined.
* `name: list`  - Name of the selected command. It will be used as argument for
                  Bob. In this example, full command for Bob will be `bob list`
* `execute:`    - Mark start of list of commands or programs to execute when
                  running this Bob command.
* `- [command]` - Each command or program which will be executed during
                  running this Bob command must starts with hypen. If you want
                  to made that command maximum portable, use relative paths
                  instead of absolute. *Note:* command `cd` will not be
                  executed. It is used by the program for set proper
                  directory. Each command can execute as much as you want
                  other commands or programs.
* `description` - Command description, will be show on run `bob help` (or just
                  `bob`) command.

## Build the program from sources

To build you need:

* compiler - GCC with enabled Ada support or (best option) GNAT from:

  https://www.adacore.com/download/

  It is recommended to use GNAT GPL 2019 to compile the program.
  The program does not work with old compilers (like GCC 4.9) since it
  lacks full support for Ada 2012.

If you have all the required packages, navigate to the main directory(where
this file is) to compile:

* Easiest way to compile the program is use Gnat Programming Studio included
  in GNAT. Just run GPS, select *bob.gpr* as a project file and select
  option `Build All`.

* If you prefer using console: in main source code directory type
  `gprbuild -P bob.gpr` for debug mode build or for release mode:
  `gprbuild -P bob.gpr -XMode=release`.

## Running it

When you trying to run the program, use `bob` executable from `bin` directory.

To get list of available commands/aliases for Bob, run it or without any
arguments or with argument `help`:

`bin/bob` will show list of available commands

`bin/bob help` will show list of available commands too
