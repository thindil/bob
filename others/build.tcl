#!/bin/sh
# the next line restarts using tclsh \
exec tclsh "$0" ${1+"$@"}

# Check if the script was started from the proper location (root directory)
if {[file exists bob.gpr] == 0} {
   puts {This script must be run in the directory where bob.gpr file is}
   return
}

# Set the target for the compilation. If no arguments, use system default
if {$argc == 0} {
   if {$tcl_platform(os) == "Linux"} {
      set target x86_64-linux-gnu
   } else {
      set target x86_64-windows
   }
} else {
   set target [lindex $argv 0]
}

# Check if correct target was set
if {$target != "x86_64-linux-gnu" && $target != "x86_64-windows" && $target != "arm-linux-gnueabihf"} {
   puts {Invalid compilation target. Allowed options are x86_64-linux-gnu, x86_64-windows or arm-linux-gnueabihf}
   return
}

# Clean and compile the game
exec gprclean -P bob.gpr --target=$target >@stdout
exec gprbuild -p -P bob.gpr -XMode=release --target=$target >@stdout
puts -nonewline {Copying files and directories ... }
if {$target != "x86_64-windows"} {
   file mkdir usr/bin usr/share/doc/bob
   file copy bin/bob usr/bin
   file copy README.md usr/share/doc/bob
   file copy COPYING usr/share/doc/bob
   file copy CHANGELOG.md usr/share/doc/bob
   file copy CONTRIBUTING.md usr/share/doc/bob
} else {
   file mkdir [file join release doc]
   file copy bin [file join release bin]
   file copy README.md [file join release doc]
   file copy COPYING [file join release doc]
   file copy CHANGELOG.md [file join release doc]
   file copy CONTRIBUTING.md [file join release doc]
}
puts {done}
exec gprclean -P bob.gpr --target=$target >@stdout
