Printer4Lazarus package
=======================

This package add some components.

Win32 :
  TPrinterSetupDialog  : for update properties of selected printer
  TPrinterDialog       : for select and/or update an printer
  
  
Linux :
  For moment, not use this package, use the cups4lazarus project (if you have CUPS installed)


FAQ : 
  Q:If I use Printers unit, the call of printer object générate an exception "Access Violation"
  R:Add in uses clause of your project, WinPrinters (for Win32) or CUPSPrinter (for linux)
