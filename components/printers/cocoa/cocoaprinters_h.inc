{%MainUnit ../osprinters.pas}

{$modeswitch objectivec1}
{$H+}

uses
  // fpc
  MacOSAll, CocoaAll, Classes, SysUtils,
  // lcl-widgetset
  CocoaUtils,
  // lcl
  Printers, LCLType;

type
  { TCocoaPrinterView }
  
  TCocoaPrinterView = objcclass(NSView)
  public
    Image: NSImage;
    Size: NSSize;
    function initWithFrame(frameRect: NSRect): id; override;
    procedure dealloc(); override;
    procedure drawRect(dirtyRect: NSRect); override;
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
  end;

