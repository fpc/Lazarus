Example program for the MS SQL Server and Sybase connectors in Lazarus.


These connectors require the FreeTDS shared library (dblib.dll/.so/.dylib), which at least on Windows requires libiconv2.dll for UTF8 support.
These can be downloaded via www.freetds.org and are provided by a lot of Linux distributions.
As a courtesy, FreePascal has a downloadable version for Windows at:
ftp://ftp.freepascal.org/fpc/contrib/windows/

The program will ask you for a database type (Sybase or MS SQL Server), username, password, server etc. and then connect.
Then it will query the server for database server information and show the results in a dbgrid.

It demonstrates:
- enabling/disabling connections in code
- getting database data into a dbgrid
- using modal forms
- getting combobox values
- handling database errors
- terminating a program

Works for me on Windows Lazarus/FPC trunk x86.

More databae functionality and techniques are shown in the LazDataDesktop project in the <lazarusdir>\tools\LazDataDesktop directory.
Reinier Olislagers, 31 March 2012