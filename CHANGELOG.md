# Changelog
All notable changes to this project will be documented in this file.

## [Unreleased]

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

## [1.0] - 2019-10-02
Initial release
