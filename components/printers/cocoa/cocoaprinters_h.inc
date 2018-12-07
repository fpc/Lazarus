{%MainUnit ../osprinters.pas}

{$modeswitch objectivec1}
{$H+}

uses
  // fpc
  MacOSAll, CocoaAll, Classes, SysUtils,
  // lcl-widgetset
  CocoaUtils, CocoaGDIObjects, CocoaPrivate,
  // lcl
  Printers, LCLType;

type

  TCocoaPrinter = class;

  { TCocoaPrinterView }
  
  TCocoaPrinterView = objcclass(NSView)
  public
    // TPrintDialog info
    PageNum: Integer;
    Doc: CGPDFDocumentRef;
    // drawing
    procedure drawRect(dirtyRect: NSRect); override;
    // manual paging, see https://developer.apple.com/library/mac/documentation/Cocoa/Conceptual/Printing/osxp_pagination/osxp_pagination.html
    function knowsPageRange(range: NSRangePointer): LCLObjCBoolean; override;
    function rectForPage(page: NSInteger): NSRect; override;
    function isFlipped: LCLObjCBoolean; override;
  end;


  TObjType = (otPrinters, otPapers);

  { TPrinterEnumerator }

  TPrinterEnumerator = class
  private
    fCurrent: univptr;
    fObjType: TObjType;
    fArray: CFArrayRef;
    fIndex: Integer;
    fPrinterRef: PMPrinter;
  public
    constructor Create(objType: TObjType; aPrinterRef:PMPrinter=nil);
    destructor Destroy; override;
    function MoveNext: boolean;
    function GetEnumerator: TPrinterEnumerator;
    property Current: univptr read fCurrent;
  end;

  { TCocoaPrinterCanvas }

  TCocoaPrinterCanvas = class(TPrinterCanvas)
  private
    function GetCGContext: CGContextRef;
  protected
    procedure DoEllipse(const Bounds: TRect); override;
    procedure DoEllipseFill(const Bounds: TRect); override;
    procedure DoEllipseAndFill(const Bounds: TRect); override;
    property CGContext: CGContextRef read GetCGContext;
  end;

  { TCocoaPrinter }

  TCocoaPrinter = class(Printers.TPrinter)
  private
    fStream: TMemoryStream;
    fDocStarted: boolean;
    fPapersList: array of TPaperItem;
    procedure ResetPapersList(aPrinter: PMPrinter);
    function GetDefaultPaperFromPPDFile: string;
    procedure StartRawModePrintJob(Sender: TObject);
  private
    FDefaultPrinter: String;
    pdfContext: CGContextRef;
    pdfData: CFMutableDataRef;
    pdfMediabox: CGRect;

    FPrinterContext: TCocoaContext;

    function GetCurrentPrinterName: string;
    function GetCurrentPrinter: PMPrinter;
    function GetPMPrinterEnumerator(objType: TObjType; printerRef:PMPrinter=nil): TPrinterEnumerator;

    procedure FindDefaultPrinter;
    function  GetOutputResolution: PMResolution;

  protected
    procedure DoBeginDoc; override;
    procedure DoBeginPage; override;
    procedure DoEndPage; override;
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
  public
    procedure Validate;
    procedure UpdatePrinter;
  public
    constructor Create; override;
    function Write(const {%H-}Buffer; {%H-}Count:Integer; out Written: Integer): Boolean; override;
    procedure CheckPrinterList;
    // Warning not portable properties here
  end;

