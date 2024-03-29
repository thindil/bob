## General information

Bob is Not Intelligent console assistant. Bod doesn't try to replace any
existing shells or build systems. It is designed for extend them. Often
when you have few projects to work, they use different commands or build
systems to maintain. To organize it, typically you have to add or global aliases
to your shell or create scripts which grow over time to maintain it. Bob
trying to solve this problem by adding ability to create local aliases for
each directory. It is doing this by creating configuration files (in YAML) with
defined aliases in each directory. When Bob is executed in selected directory,
it reads configuration file (if exists) for selected directory for aliases.

**INFO:** This project is no longer maintained. Feel free to clone it and take
care about it.

### Configuration file

Bob looking for file `.bob.yml` in current directory (where it is executed).
If it finds it, it reads all available commands from here. Sample
configuration:

     version: 3.0
     - include: ../.bob.yml
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

* `version: [version]` - Optional entry. Minimal required version of the
                         program to load this configuration file. If present,
                         the program will load the rest of the configuration
                         file only when the program version will be same or
                         higher.
* `- include: [path]`  - Optional entry. Add it, if you want to include any
                         other Bob's configuration file. Path must be
                         absolute or relative to the current configuration
                         file.
* `-command:`          - Each command for Bob must starts from this line.
                         Each configuration file may have as much as you
                         want commands defined.
* `name: list`         - Name of the selected command. It will be used as
                         argument for Bob. In this example, full command for
                         Bob will be `bob list`.
* `variables:`         - Mark start of list of environmental variables which
                         will be added when running this Bob command.
* `- [name] = [value]` - Each environment variable which will be added
                         during running this Bob command must start with
                         hypen. [name] is the name of the variable which will
                         be added, [value] is the value of the variable which
                         will be added. Each command can have as much as you
                         want variables.
* `execute:`           - Mark start of list of commands or programs to execute
                         when running this Bob command.
* `- [command]`        - Each command or program which will be executed during
                         running this Bob command must start with hypen. If
                         you want to make that command maximum portable, use
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
                         values are: `unixonly`: command is available only on
                         Unix-like systems, `windowsonly`: command is available
                         only on Windows systems, `internal`: command is not
                         visible on commands list, but can be normally run (can
                         be useful for recursive calls), `evaluatevariables`:
                         command variables are treated as commands to run, and
                         they result will be assigned to that variable,
                         `windowspath`: paths in the command are in Windows
                         style (use \ for directory separator), will be
                         translated on Unix, `unixpath`: paths in the command
                         are in Unix style (use / for directory separator),
                         will be translated on Windows.

Commands to execute can have also variables used in the definitions. Variables
start with sign `$`. `$` and number after it means argument from command line
entered during executing Bob command. For example, `$2` means second argument
after Bob command name. Thus, if you enter: `bob dosomething one two`, `$2`
will be replaced by word `two`. `$` and any alphanumeric character(s) means
environment variable with that name, available during executing this Bob
command. In this example, it is variable `$DIRECTORY`. For more advanced, real
life example, look at it own *.bob.yml* file.

## Build the program from sources

### Docker way

You can use Docker images `adabuild` and `adabuildwin64` from the project
[dockerada](https://github.com/thindil/dockerada). They contain all libraries
and compiler needed to build the game. Recommended version is `9`

To build Linux version of the program, download `adabuild:9` image and type in
console:

`docker run --rm -v [path to source code]:/app ghcr.io/thindil/adabuild:9 /bin/bash -c "cd /app && gprbuild -p -P bob.gpr -XMode=release"`

To build Windows 64-bit version of the program, download `adabuildwin64:9` image
and type in console:

`docker run --rm -v [path to source code]:/app ghcr.io/thindil/adabuildwin64:9 /bin/bash -c "cd /app && gprbuild -p -P bob.gpr -XMode=release --target=x86_64-windows"`

### Classic way

To build you need:

* compiler - GCC with enabled Ada support or GNAT from:

  https://www.adacore.com/download/

  The program does not work with old compilers (like GCC 4.9) since it
  lacks full support for Ada 2012.

If you have it, navigate to the main directory(where this file is) to compile:

* The easiest way to compile the program is use Gnat Programming Studio
  included in GNAT. Just run GPS, select *bob.gpr* as a project file and select
  option `Build All`.

* If you prefer using console: in main source code directory type
  `gprbuild -P bob.gpr` for debug mode build or for release mode:
  `gprbuild -P bob.gpr -XMode=release`. If you have already installed **Bob**
  you can also use it to build the program. For debug mode: `bob debug`
  and for release mode: `bob release`.

## Running it

When you are trying to run the program, use `bob` executable from `bin`
directory.

To get list of available commands/aliases for Bob, run it or without any
arguments or with argument `help`:

`bin/bob` will show list of available commands

`bin/bob help` will show list of available commands too

### Testing versions

Here are available testing versions of the game. You can find them
in [Actions](https://github.com/thindil/bob/actions?query=workflow%3A"Continuous+Integration").
Just select option from the list of results to see Artifacts list.
To use them, just extract file(s) from the archive.

## Generating code documentation

To generate (or regenerate) code documentation, you need [ROBODoc](https://rfsber.home.xs4all.nl/Robo/)
and [Tcl](http://tcl.tk) interpreter. If you have them, in the main program
directory (where this file is) enter terminal command:
`others/generatedocs.tcl`. For more information about this script, please look
[here](https://github.com/thindil/roboada#generatedocspy). This version of
script have set all default settings for Bob code. If you have Bob installed,
you can also use for that the command: `bob docs`.

## Contributing to the project
For detailed information about contributing to the project (bugs reporting,
ideas propositions, code conduct, etc.), see [CONTRIBUTING.md](CONTRIBUTING.md)

## Licenses

Bob is available under [GPLv3](COPYING) license.

----

That's all for now, as usual, probably I forgot about something important ;)

Bartek thindil Jasicki
