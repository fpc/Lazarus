The FPDoc documentation manager
===============================

This project simplifies the maintenance of (local) documentation.
A configuration wizard helps in the setup of the basic RTL, FCL and LCL docs.
Configuration and output is kept in a single user selectable directory.

A package FPDocEngine is supplied for use in commandline or GUI applications.
uMakeSkel is a copy of parts of the FPDoc and MakeSkel programs, modified
with workarounds for known problems with these FPC tools.

Release notes 1.0
-----------------

The FPDoc Manager requires FPC 2.7 (rev. 19947) for proper operation of the
FPDoc units.

A package FPDocEngine.lpk has been created for the FPDoc units, please move it
into your $FPC/utils/fpdoc/ directory and compile the package there.

The project (FPDocManager.lpi) did not fully compile on my machine. Unit
dw_HTML could not be found, even if all other units of the package worked.
Adding the unit directly to the project made it compile, please remove or
update this entry as required, and give hints on how to fix the problem.

DoDi
