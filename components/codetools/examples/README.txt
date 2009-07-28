Examples for the CodeTools

Some basic examples run out of the box.
Some examples require the path of the FPC sources, which can be given via the
environment variable FPCDIR.
If you see an error message like this:

Scanning FPC sources may take a while ...
TDefinePool.CreateFPCSrcTemplate FPCSrcDir does not exist: ...

Then you must set the FPCDIR variable and start the example again. For example:
Under linux:
  export FPCDIR=/home/username/freepascal/fpc

For instance the FPC 2.2.0 source directory looks like this:
  compiler
  fv
  ide
  installer
  Makefile
  Makefile.fpc
  packages
  rtl
  tests
  utils

See here for more information:
http://wiki.lazarus.freepascal.org/Installing_Lazarus#FPC_Sources

The examples will scan the directories and store the result in a file named
'codetools.config'. So the next time you start any of the examples it does
not need to scan.

List of environment variables:
FPCDIR       = path to FPC source directory
PP           = path of the Free Pascal compiler. For example /usr/bin/ppc386.
LAZARUSDIR   = path of the lazarus sources
FPCTARGET    = FPC target OS like linux, win32, darwin
FPCTARGETCPU = FPC target cpu like i386, x86_64, arm

