## General information

*NOTE*: Bob is in a very early stage of the development so, most of described
things here are not implemented yet. Additionally, many things may change
or break, use at your own risk.

Bob is Not Inteligent console assistant. Bod doesn't try to replace any
existing shells or build systems. It is designed for extend them. Very often
when you have few projects to work, they use different commands or build
systems to maintain. To organize it often you have to add or global aliases
to your shell or create scripts which grow over time to maintain it. Bob
trying to solve this problem by ability to create local aliases for each
directory. It doing this by creating configuration files (in YAML) with
defined aliases in each directory. When Bob is excuted in selected directory,
it reads main configuration file of the program and configuration file
(if exists) for selected directory for aliases.

More information will be added *soon*(TM).

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
