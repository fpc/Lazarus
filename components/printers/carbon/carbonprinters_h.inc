{%MainUnit ../osprinters.pas}
uses
{$ifdef ver2_2_0}
  FPCMacOSAll,
{$else}
  MacOSAll,
{$endif}
  Classes, SysUtils, Printers, LCLType, CarbonCanvas, CarbonPrinting;

type
  { TCarbonPrinterContext }
  
  TCarbonPrinterContext = class(TCarbonDeviceContext)
  protected
    function GetSize: TPoint; override;
  public
    procedure Release;
    procedure Reset; override;
  end;

  { TCarbonPrinter }

  TCarbonPrinter = class(TPrinter)
  private
    FPrintSession: PMPrintSession;
    FPrintSettings: PMPrintSettings;
    FPageFormat: PMPageFormat;
    FBeginDocumentStatus: OSStatus;
    FNewPageStatus: OSStatus;
    FPrinterContext: TCarbonPrinterContext;
    FPrinterArray: CFArrayRef;
    FPaperArray: CFArrayRef;
    FDefaultPrinter: String;
    
    procedure CreatePrintSession;
    procedure CreatePrintSettings;
    function GetCurrentPrinter: PMPrinter;
    function GetCurrentPrinterName: String;
    
    function CreatePageFormat(APaper: String): PMPageFormat;
    
    function ValidatePageFormat: Boolean;
    function ValidatePrintSettings: Boolean;

    procedure BeginPage;
    procedure EndPage;
    
    procedure FindDefaultPrinter;
    procedure BeginEnumPrinters(Lst: TStrings);
    procedure EndEnumPrinters;
    procedure BeginEnumPapers(Lst: TStrings);
    procedure EndEnumPapers;
  protected
    procedure DoBeginDoc; override;
    procedure DoNewPage; override;
    procedure DoEndDoc(aAborded : Boolean); override;
    procedure DoAbort; override;

    procedure DoEnumPrinters(Lst : TStrings); override;
    procedure DoResetPrintersList; override;

    procedure DoEnumPapers(Lst : TStrings); override;
    function DoGetPaperName: string; override;
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
    function GetPrinterType: TPrinterType;override;
    function DoGetPrinterState: TPrinterState;override;
    function GetCanPrint: Boolean;override;
    function GetCanRenderCopies : Boolean;override;
    procedure RawModeChanging; override;
  public
    procedure Validate;
    procedure UpdatePrinter;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Write(const Buffer; Count:Integer; var Written: Integer): Boolean; override;
    // Warning not portable functions here
    property CurrentPrinterName: String read GetCurrentPrinterName;
    property PrintSession: PMPrintSession read FPrintSession;
    property PrintSettings: PMPrintSettings read FPrintSettings;
    property PageFormat: PMPageFormat read FPageFormat;
    // Warning it is a not portable property
    property Handle: TCarbonPrinterContext read FPrinterContext;
  end;

