# Changelog
All notable changes to this project will be documented in this file.

## [3.1] - 2021-03-28

### Added
- Command to show the content of the selected local Bob command
- Error exit status to the program when something wrong was happened

### Changed
- Updated contributing guide

### Fixed
- Lack of default value for config item type
- Memory leaks in parsing commands
- Creating output file for commands

## [3.0] - 2020-11-11

### Added
- Option to translate path between windows and unix
- Command to convert any file to Bob configuration or add its content to
  existing Bob configuration file

### Changed
- Updated README.md

### Fixed
- Typos in README.md, CONTRIBUTING.md
- Crash on invalid command entry in config file

## [2.0] - 2019-11-11

### Added
- Better formatting of list of available commands
- Option to set output for commands
- Check for correctness of command configuration
- Option to set command for selected operating system only (Windows or Unix)
- Option to set command as internal: don't show that command on available
  commands list
- Option to set command variables to evaluate
- Support for commands with spaces in names in executing commands
- Option to include other Bob's configuration files
- Option to set minimal required version of the program
- Coloring error messages on Unix systems

### Changed
- Show whole command on error
- Show available commands sorted alphabetically
- Updated README.md
- More information on crash screen

### Fixed
- Problems with cd command on Debian
- Some typos in contributing guide
- Typos in README.md
- Crash on comment after command mark in config file
- Crash on load command with empty value for any setting
- Possible crash on non existing command

## [1.0] - 2019-10-02
Initial release
