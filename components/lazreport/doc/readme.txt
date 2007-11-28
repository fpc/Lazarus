OVERVIEW

LazReport is a group of components to add reporting capabilities to applications,
it uses a visual designer to create banded reports and includes a report engine
with previewer and includes a interpreter to run user scripts.  Report designer can
be invoked at runtime. 


LICENSE

LazReport is based on FreeReport 2.32 and thanks to Fast Reports Inc. it's
available under modified LGPL, the same license as the Lazarus LCL.
see files LazReport/ license.txt, license-rus.txt and license-lazreport.txt
for detail.


AUTHORS

FreeReport was created for Fast Reports Inc.
LazReport initial port was made by Olivier Guilbaud.
Lazarus integration and fixes by Jesus Reyes A.
Many contributors, see contributors.txt file


INSTALL

To install LazReport under lazarus IDE:

1. Open LazReport Package. Menu: Components->Open package file (.lpk)...
2. Open file $LazarusPath/components/lazreport/source/lazreport.lpk
3. Compile
4. Install

Next time lazarus is started, it should show a LazReport tab in component
palette.


DOCUMENTATION.

Specific docs for LazReport are yet to be written, but there is valuable information
in $lazreportPath/doc directory, the file fr_eng.sxw is an OpenOffice word document
explaining FastReport v2.32 and most things should be applied to LazReport too.

Some tests are provided in samples directory to help user to get started in LazReport.


NOTES.

Originally using LazReport under Linux/x86 some global localization variables were
initializated using unit Libc, however the use of libc is not recommended by
developers of FPC, "The FPC libc unit is a Kylix compatibility unit that is only
compiled for linux/x86, since that is the only target Kylix compiles for".
Therefore, libc unit is no longer used by default in LazReport but can be enabled
by uncommenting the UseLibC define line in file lr_vers.inc


BUG REPORTS.

Please report problems using the freepascal bugtracker: 
http://www.freepascal.org/mantis/main_page.php, project: "Lazarus Packages", Category
"LazReport", for patches please submit a bug report and attach the patch to it.
