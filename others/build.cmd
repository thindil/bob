@ECHO OFF
set PATH=%PATH%;C:/GNAT/2019/bin
gprbuild -P bob.gpr -XMode=release
XCOPY /S bin release\
XCOPY README.md release\doc\
XCOPY COPYING release\doc\
XCOPY CHANGELOG.md release\doc\
XCOPY CONTRIBUTING.md release\doc\
gprclean -P bob.gpr
