The FPDoc documentation manager
===============================

This project simplifies the maintenance of (local) documentation.
A configuration wizard helps in the setup of the basic RTL, FCL and LCL docs.
Configuration and output is kept in a single user selectable directory.

A package FPDocEngine is supplied for use in commandline or GUI applications.
uMakeSkel is a copy of parts of the FPDoc and MakeSkel programs, modified
with workarounds for known problems with these FPC tools.

Organization
------------
A dedicated directory contains all package specifications and documentation.
INI files are created for the manager itself and every package.

Release notes 1.0
-----------------

The FPDoc Manager requires FPC 2.7 (rev. 19947) for proper operation of the
FPDoc units.

For reference to the proper FPDoc units please update the dGlobals.pp reference
in the project file, to point to your FPC/utils/fpdoc/ directory.

A package FPDocEngine.lpk has been created for the FPDoc units, please move it
into your $FPC/utils/fpdoc/ directory and compile the package there.
This package has been removed again, because it introduces complications when
a mix of FPC 2.6 and 2.7 units was used.

DoDi
