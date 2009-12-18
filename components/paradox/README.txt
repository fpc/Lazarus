This package installs Paradox support in the lazarus IDE:

TParadox (Data Access Tab) is a TDataset descendent that enables you to open
and see Paradox tables. It has support for Blobs and Graphics. There is no
need to have the BDE from Borland (Codegear) installed.The TParadox
component itself is located in the FCL, and is an implementation
based on an open source C library called pxlib.
This means that you need to install the PXlib library, which you can 
download from Sourceforge:

http://pxlib.sourceforge.net/

The C library can be installed on most linux systems using the native package manager.
For windows, a pre-compiled binary is available from the sourceforge site.
The TParadox component code has been tested with version 0.6.2 of the library,
and should also work with version 0.6.3.


The component needs version 2.4.0 of Free Pascal, earlier versions contain a bug
that prevents a TParadox dataset from opening.

Enjoy !
 

