## General information

Bob is Not Intelligent console assistant. Bod doesn't try to replace any
existing shells or build systems. It is designed for extend them. Often
when you have few projects to work, they use different commands or build
systems to maintain. To organize it, often you have to add or global aliases
to your shell or create scripts which grow over time to maintain it. Bob
trying to solve this problem by adding ability to create local aliases for
each directory. It is doing this by creating configuration files (in YAML) with
defined aliases in each directory. When Bob is executed in selected directory,
it reads configuration file (if exists) for selected directory for aliases.

**Note:** This version of README.md is about development version of the
program. Some things may be different in released version of Bob.

### Configuration file

Bob looking for file `.bob.yml` in current directory (where it is executed).
If it find it, it reads all available commands from here. Sample
configuration:

     - command:
       name: list
       variables:
           - DIRECTORY = /home/user
       execute:
           - ls $DIRECTORY
       description: Show content of $DIRECTORY directory
       output: standard
       flags:
           - unixonly

Explanation:

* `-command:`          - Each command for Bob must starts from this line.
                         Each configuration file may have as much as you
                         want commands defined.
* `name: list`         - Name of the selected command. It will be used as
                         argument for Bob. In this example, full command for
                         Bob will be `bob list`.
* `variables:`         - Mark start of list of environmental variables which
                         will be added when running this Bob command.
* `- [name] = [value]` - Each environment variable which will be added
                         during running this Bob command must starts with
                         hypen. [name] is the name of the variable which will
                         be added, [value] is the value of the variable which
                         will be added. Each command can have as much as you
                         want variables.
* `execute:`           - Mark start of list of commands or programs to execute
                         when running this Bob command.
* `- [command]`        - Each command or program which will be executed during
                         running this Bob command must starts with hypen. If
                         you want to made that command maximum portable, use
                         relative paths instead of absolute. If you want to
                         execute command which contains space, put it in
                         quotes or apostrofes. *Note:* command `cd` will not
                         be executed. It is used by the program for set proper
                         directory. Each command can execute as much as you
                         want other commands or programs.
* `description:`       - Command description, will be show on run `bob help`
                         (or just `bob`) command.
* `output:`            - Optional parameter. Redirect command output to the
                         selected place. Possible options are: `standard`:
                         redirect output to standard output, `error`: redirect
                         output to standard error output, any other value will
                         be treated as name for file to which redirect output.
                         Path to the file should be relative to the current
                         directory.
* `flags:`             - Mark start of list of command's flags. Optional.
* `- [flag]`           - Each flag assigned to this Bob command. Possible
                         values are: `unixonly`: command is avaialable only on
                         Unix-like systems, `windowsonly`: command is available
                         only on Windows systems, `internal`: command is not
                         visible on commands list, but can be normally run (can
                         be useful for recursive calls), `evaluatevariables`:
                         command variables are treated as commands to run and
                         they result will be assigned to that variable.

Commands to execute can have also variables used in they definitions. Variables
starts with sign `$`. `$` and number after it means argument from command line
entered during executing Bob command. For example, `$2` means second argument
after Bob command name. Thus if you enter: `bob dosomething one two`, `$2`
will be replaced by word `two`. `$` and any alphanumeric character(s) means
environment variable with that name, available during executing this Bob
command. In this example, it is variable `$DIRECTORY`. For more advanced, real
life example, look at it own *.bob.yml* file.

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
  `gprbuild -P bob.gpr -XMode=release`. If you have already installed **Bob**
  you can also use it to build the program. For debug mode: `bob debug`
  and for release mode: `bob release`.

## Running it

When you trying to run the program, use `bob` executable from `bin` directory.

To get list of available commands/aliases for Bob, run it or without any
arguments or with argument `help`:

`bin/bob` will show list of available commands

`bin/bob help` will show list of available commands too

## Generating code documentation

To generate (or regenerate) code documentation, you need ROBODoc If you have
it, in main program directory (where this file is) enter terminal command:
`others/generatedocs.py`. For more information about this script, please look
[here](https://github.com/thindil/roboada#generatedocspy). This version of
script have set all default settings for Bob code. If you have Bob installed,
you can also use for it command: `bob docs`.

## Contributing to the project
For detailed informations about contributing to the project (bugs reporting,
ideas propositions, code conduct, etc.), see [CONTRIBUTING.md](CONTRIBUTING.md)

## Licenses

Bob is available under [GPLv3](COPYING) license.

----

That's all for now, as usual, probably I forgot about something important ;)

Bartek thindil Jasicki
