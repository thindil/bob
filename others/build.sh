#!/usr/bin/env sh

gprbuild -p bob.gpr -XMode=release
mkdir -p usr/share/doc/bob
mkdir -p usr/bin
cp bin/bob usr/bin
cp README.md usr/share/doc/bob
cp COPYING usr/share/doc/bob
cp CHANGELOG.md usr/share/doc/bob
cp CONTRIBUTING.md usr/share/doc/bob
