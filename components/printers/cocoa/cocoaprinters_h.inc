{%MainUnit ../osprinters.pas}

{$modeswitch objectivec1}
{$H+}

uses
  // fpc
  MacOSAll, CocoaAll, Classes, SysUtils,
  // lcl-widgetset
  CocoaUtils,
  // lcl
  Printers, LCLType,
  // Cocoa Print
  CocoaPrintCanvas;

type
  { TCocoaPrinterView }
  
  TCocoaPrinterView = objcclass(NSView)
  public
    //Image: NSImage;
    Canvas: TCocoaPrinterCanvas;
    // TPrintDialog info
    PageMin, PageMax, PageFrom, PageTo: Integer;
    function initWithFrame(frameRect: NSRect): id; override;
    procedure dealloc(); override;
    // drawing
    procedure drawRect(dirtyRect: NSRect); override;
    // manual paging, see https://developer.apple.com/library/mac/documentation/Cocoa/Conceptual/Printing/osxp_pagination/osxp_pagination.html
    function knowsPageRange(range: NSRangePointer): Boolean; override;
    function rectForPage(page: NSInteger): NSRect; override;
    function calculatePrintHeight: Double; message 'calculatePrintHeight';
    function calculatePrintWidth: Double; message 'calculatePrintWidth';
    function updateSize(ADoSetFrame: Boolean): NSRect; message 'updateSize:';
  end;

  { TCocoaPrinter }

  TCocoaPrinter = class(Printers.TPrinter)
  private
    FDefaultPrinter: String;
    FPrintView: TCocoaPrinterView;
    FPrintOp: NSPrintOperation;
    FPrintInfo: NSPrintInfo;

    function GetCurrentCarbonPrinter: PMPrinter;
    function GetPrintSession: PMPrintSession;
    function GetPrintSettings: PMPrintSettings;
    function GetPageFormat: PMPageFormat;

    function CreatePageFormat(APaper: String): PMPageFormat;
    
    function ValidatePageFormat: Boolean;
    function ValidatePrintSettings: Boolean;

    procedure BeginPage;
    procedure EndPage;
    
    procedure FindDefaultPrinter;
    function  GetOutputResolution: PMResolution;

    function DoDoGetPaperName(APageFormat: PMPageFormat): string;

  protected
    procedure DoBeginDoc; override;
    procedure DoNewPage; override;
    procedure DoEndDoc(aAborded : Boolean); override;
    procedure DoAbort; override;

    procedure DoEnumPrinters(Lst : TStrings); override;
    procedure DoResetPrintersList; override;

    procedure DoEnumPapers(Lst : TStrings); override;
    function DoGetPaperName(): string; override;
    function DoGetDefaultPaperName: string; override;
    procedure DoSetPaperName(aName : string); override;
    function DoGetPaperRect(aName : string; Var aPaperRc : TPaperRect) : Integer; override;

    function DoSetPrinter(aName : string): Integer; override;

    function DoGetCopies : Integer; override;
    procedure DoSetCopies(aValue : Integer); override;
    function DoGetOrientation: TPrinterOrientation; override;
    procedure DoSetOrientation(aValue : TPrinterOrientation); override;

    function GetXDPI: Integer; override;
    function GetYDPI: Integer; override;
    function GetPrinterType: TPrinterType; override;
    function DoGetPrinterState: TPrinterState; override;
    function DoGetDefaultCanvasClass: TPrinterCanvasRef; override;

    function GetCanPrint: Boolean; override;
    function GetCanRenderCopies : Boolean; override;
    procedure RawModeChanging; override;
    procedure DoDestroy; override;
  private
    procedure Validate;
    procedure UpdatePrinter;
  public
    constructor Create; override;
    function Write(const {%H-}Buffer; {%H-}Count:Integer; var {%H-}Written: Integer): Boolean; override;
    // Warning not portable properties here
    //property CurrentPrinterName: String read GetCurrentPrinterName;
    property PrintSession: PMPrintSession read GetPrintSession;
    property PrintSettings: PMPrintSettings read GetPrintSettings;
    property PageFormat: PMPageFormat read GetPageFormat;
    property PrintOperation: NSPrintOperation read FPrintOp;
    property PrintInfo: NSPrintInfo read FPrintInfo;
    property PrintView: TCocoaPrinterView read FPrintView;
  end;

