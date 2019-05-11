
This directory contains Lazarus design-time support for the SQLDB REST Bridge
functionality in FPC (3.3.1 or 3.2.0).

To install:

1. Compile (don't install) the sqldbrestschemadesigner.lpk package in the design directory.
2. Compile and install the lazsqldbrest.lpk package found in this directory.

You can also compile this with FPC 3.0.4. 

1. Make a directory 'src' below this directory. (the name must be src)
2. Copy the files from the FPC SVN repository to the newly made subdirectory src of this directory.
   In particular, all files in directory packages/fcl-web/src/restbridge must be copied.
Then proceed as described above.

The SQLDBRESTBRidge support and examples are explained in the WIKI:
http://wiki.freepascal.org/SQLDBRestBridge#Lazarus

Author: Michael Van Canneyt 
