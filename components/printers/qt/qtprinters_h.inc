{%MainUnit ../osprinters.pas}
uses
  Classes, SysUtils, Printers, {$IFDEF LCLQt5}qt5{$ELSE}qt4{$ENDIF}, qtobjects,
  LCLType, LCLProc;

type
  TPaperRec = record
    PageSize: QPrinterPageSize;
    PaperName: string;
    PageRect: TRect;
    PaperRect: TRect;
  end;

  { TQtPrinters }
  TQtPrinters = class(TPrinter)
  private
    FPrinterHandle: TQtPrinter;
    FDefaultPaperIndex: Integer;
    FPapers: array of TPaperRec;

    procedure BeginPage;
    procedure EndPage;
    
    procedure CachePapers(OnlySupportedByPrinter:boolean);
    procedure EnumQPrinters(Lst: TStrings);
    function IndexOfPaper(const Paper: string; RetDefault:boolean=true): Integer;
    function IndexOfPageSize(const PageSize: QPrinterPageSize): Integer;
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
    procedure DoDestroy; override;
  public
    procedure Validate;
    function GetPaperSize(Const Str: String): {$IFDEF LCLQt5}QPagedPaintDevicePageSize{$ELSE}QPrinterPageSize{$ENDIF};
  public
    constructor Create; override;
    function Write(const {%H-}Buffer; {%H-}Count:Integer; out Written: Integer): Boolean; override;

    property ColorMode: QPrinterColorMode read GetColorMode write SetColorMode;
    property FullPage: Boolean read GetFullPage write SetFullPage;
    property Handle: TQtPrinter read FPrinterHandle;
    property PageOrder: QPrinterPageOrder read GetPageOrder write SetPageOrder;
  end;

