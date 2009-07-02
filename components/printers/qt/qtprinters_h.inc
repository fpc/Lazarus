{%MainUnit ../osprinters.pas}
uses
  {$IF NOT DEFINED(USE_QT_44) or NOT DEFINED(USE_QT_45)}
  {$IFDEF WINDOWS}Windows,{$ENDIF}
  {$IFDEF UNIX}CUPSDyn,{$ENDIF}
  {$ENDIF}
  Classes, SysUtils, Printers, qt4, qtobjects,
  LCLType, LCLProc;

type
  { TQtPrinters }
  TQtPrinters = class(TPrinter)
  private
    FPagesEnum: TStrings;
    FPrintSession: Integer;
    FPrintSettings: Integer;
    FPageFormat: QPrinterPageSize;
    FBeginDocumentStatus: QPrinterPrinterState;
    FNewPageStatus: QPrinterPrinterState;
    FPrinterHandle: TQtPrinter;

    procedure CreatePrintSettings;
    procedure BeginPage;
    procedure EndPage;
    
    procedure EnumQPrinters(Lst: TStrings);
    procedure EnumQPapers(Lst: TStrings);
    function GetColorMode: QPrinterColorMode;
    function GetFullPage: Boolean;
    function GetPageOrder: QPrinterPageOrder;
    procedure SetColorMode(const AValue: QPrinterColorMode);
    procedure SetFullPage(const AValue: Boolean);
    procedure SetPageOrder(const AValue: QPrinterPageOrder);
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
    function GetPaperSize(Const Str: String): QPrinterPageSize;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Write(const Buffer; Count:Integer; var Written: Integer): Boolean; override;

    property ColorMode: QPrinterColorMode read GetColorMode write SetColorMode;
    property FullPage: Boolean read GetFullPage write SetFullPage;
    property Handle: TQtPrinter read FPrinterHandle;
    property PageOrder: QPrinterPageOrder read GetPageOrder write SetPageOrder;
  end;

