Printer4Lazarus package
=======================

This package add some components.
  TPrinterSetupDialog  : for update properties of selected printer
  TPrintDialog         : for select and/or update an printer before printing

Win32 :
 Native implementation 
  
Linux :
  Yous must install CUPS and libcups v1.1.19 or more.

FAQ : 
  Q:If I use Printers unit, the call of printer object générate an exception "Access Violation"
  R:Add in uses clause of your project, osPrinters
