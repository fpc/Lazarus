This is the cleandirs demonstration program for Lazarus.

It demonstrates 3 things:

1. How to use services and the TEventLog component (cleandirs)
2. How to use RTTI controls (confcleandirs)
3. How to write a TCustomApplication descendent (cleandir)

These are actually 3 programs:

cleandir: 
=========

A command-line tool. It will clean all directories according to
the configuration file given. If no configuration file is specified,
it will look for one in the standard user-specific and global locations.

cleandirs: 
==========
a service application (daemon) that can be installed and run as a service.
It performs the same action as the cleandir program, at a given time of 
the day. It cleans only standard directories specified in the configuration 
file.

confcleandirs: 
==============

A GUI program that can be created to create a configuration file
for the cleandirs and cleandir program. it can be used to (test)
run the algorithm as well, and shows the log in a window.

The configuration works as follows:

Directories:
------------
  Directories that should be cleaned. 
  Each directory can be enabled or disabled.
  If the recurse flag is set, subdirectories will be checked as well.

Locations:
----------
  Locations where files should be moved to. 
  The tool can create subdirectories of the given location, using
  file extension, year, month, day, date, hour min and time:
  for instance extension, year, month will create a subdirectory
   doc/2007/10
  if a file with extension .doc is moved to this location in october of 2007.

Actions:
--------
  A set of actions to be performed on the files:

Location: 
  File will be copied to the location. 
  This is the name of one of the locations defined above.
Extensions:
  A space separated list of extensions to apply the action on.
  The extensions should not have the initial dot character.
  e.g.: 
     zip gz z bz2 rar 
  will match all archives
Casesensitive:
  Are extensions case-sensitive ?
Delete: 
  File will be deleted after it was succesfully copied.
Compress: 
  File will be compressed (like gzip).
MincompressSize: 
  File will only be compressed if it's size is larger than the value indicated here.

Global options:
---------------
LogOnly: 
  Only log the actions, do not actually execute them.
  (this can only be set in code, it is set automatically when testing)

Log All Files:
  Check if you want the tool to show all files it encounters while
  checking the directories
 
All Actions:
  By default, only the first matching action is executed. If this
  option is set, then all actions that match the file are executed.

  If one of the actions has the "Delete" flag set, the file will be 
  deleted only after all actions were tried.

Stop On Error:
  No further files will be treated.
  Default is to continue with the next file in case of error.

The service can be installed on windows with 
cleandirs --install --config=c:\path\to\config\file.cfg
it can then be controlled from the service manager.

Running on linux happens wih
cleandirs -r -c path/to/config/file.cfg

If no config file is specified, a file named CleanDirs.cfg (case sensitive)
is searched 
- in /etc or in ~/.config/ for unix
- in c:\documents and settings\username\local settings\
  or next to the application binary in Windows.

Enjoy !

Michael.

Missing file on windows
-----------------------
The cleandirs needs the fclel.res resource file on windows. It is provided by
the FCL, but not distributed with fpc 2.2.0. You can copy  
lazarus\fpc\2.2.0\source\packages\fcl-base\src\win\fclel.res
into this example directory.

Vincent
