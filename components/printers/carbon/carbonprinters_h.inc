{%MainUnit ../osprinters.pas}
uses
  Classes,SysUtils,Printers,LCLType;

type
  { TCarbonPrinter }

  TCarbonPrinter = class(TPrinter)
  private
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
    function GetHandlePrinter : HDC;
    procedure SetHandlePrinter(aValue : HDC);
    procedure RawModeChanging; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Write(const Buffer; Count:Integer; var Written: Integer): Boolean; override;
    //Warning not portable functions here
    //Warning it is a not potable property
    property Handle : HDC read GetHandlePrinter write SetHandlePrinter;
  end;


