Printer4Lazarus package
=======================

This package adds some components.
  TPrinterSetupDialog - for update properties of selected printer
  TPrintDialog        - for select and/or update an printer before printing
  TPageSetupDialog    - to select margins (currently only under Windows)

Win32:
  Native implementation 
 
  Notes:
  1.  TPrinter.CanRenderCopies return information if printer driver is able to print more then one copy at once. 
      Not all printers drivers support that feature (n that case programmer should print document requested times)

  2.  TPrintDialog.Options indicate which controls should be visible in dialog. 
      TPrintDialog.PrintToFile indicate that output will be redirected to file.
      
  3.  Known Issue: After adjusting resolution using Printer.AdvancedProperties. The DevMode structure
      shows the change correcly, but getting the value through Printer.XDPI the value is the old one.
      However after using the PrintSetup dialog, it returns the correct value. Other changes like
      the selected paper are correctly modified after AdvancedProperties, ... (?).

  
  
Linux and Mac OS X (gtk):
  Yous must install CUPS and libcups v1.1.19 or more.

Mac OS X (Carbon):
  Uses CUPS by default. You can use native implementation by adding compiler switch -dNativePrint.

  Notes: TPrinterSetupDialog is not supported.


FAQ: 
  Q: If I use Printers unit, the call of printer object generate an exception "Access Violation".
  A: Add in uses clause of your project, osPrinters.

