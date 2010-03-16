unit CarbonPrinting;

{$mode macpas}
{$packenum 1}
{$macro on}
{$inline on}
{$CALLING MWPASCAL}
{$ALIGN MAC68K}

interface

uses
  Classes, SysUtils,
{$ifdef ver2_2_0}
  FPCMacOSAll;
{$else}
  MacOSAll;
{$endif}

// functions missing in MacOSAll
type
  PMPaper    = ^SInt32; { an opaque 32-bit type }
  PMPaperPtr = ^PMPaper;  { when a var xx:PMPaper parameter can be nil, it is changed to xx: PMPaperPtr }
  PMServer   = ^SInt32; { an opaque 32-bit type }

  function PMPaperGetName(paper: PMPaper; var paperName: CFStringRef): OSStatus; external name '_PMPaperGetName';

  function PMServerCreatePrinterList(server: PMServer; var printerList: CFArrayRef): OSStatus; external name '_PMServerCreatePrinterList';

  function PMPrinterGetName(printer: PMPrinter): CFStringRef; external name '_PMPrinterGetName';
  function PMPrinterGetState(printer: PMPrinter; var state: PMPrinterState): OSStatus; external name '_PMPrinterGetState';
  function PMPrinterIsDefault(printer: PMPrinter): Boolean; external name '_PMPrinterIsDefault';
  function PMPrinterIsRemote(printer: PMPrinter; var isRemoteP: Boolean ): OSStatus; external name '_PMPrinterIsRemote';
  function PMPrinterGetPaperList(printer: PMPrinter; var paperList: CFArrayRef): OSStatus; external name '_PMPrinterGetPaperList';

  function PMCreatePageFormatWithPMPaper(var pageFormat: PMPageFormat; paper: PMPaper): OSStatus; external name '_PMCreatePageFormatWithPMPaper';
  function PMGetPageFormatPaper(pageFormat: PMPageFormat; var paper: PMPaper): OSStatus; external name '_PMGetPageFormatPaper';

  function PMSessionBeginCGDocument(printSession: PMPrintSession; printSettings: PMPrintSettings;
    pageFormat: PMPageFormat): OSStatus; external name '_PMSessionBeginCGDocument';
  function PMSessionSetCurrentPMPrinter(session: PMPrintSession; printer: PMPrinter): OSStatus; external name '_PMSessionSetCurrentPMPrinter';
  function PMSessionGetCGGraphicsContext(session: PMPrintSession; var context: CGContextRef): OSStatus; external name '_PMSessionGetCGGraphicsContext';


implementation

end.

