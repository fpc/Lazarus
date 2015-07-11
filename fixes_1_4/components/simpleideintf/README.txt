SimpleIDEIntf
=============

This package simulates a simple IDE to allow testing IDE experts at command
line. It fills several objects and functions of the IDEIntf.

For example:
examples/testh2pastool.lpi

The h2paswizard package provides an IDE menu item, a dialog and several text
tools to parse and change sources. The example project demonstrates how to
use the SimpleIDEIntf package to test the text tools of the h2paswizard with
a command line program, which compiles fast and can be debugged easier than
the package in the IDE.

./testh2pastool files/h2pastest.pas

