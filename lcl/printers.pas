{
 /***************************************************************************
                                Printers.pas
                                ------------
                            Basic Printer object
                               
 ****************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Olivier Guilbaud
}
unit Printers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Graphics;
type
  TPrinter = Class;
  EPrinter = class(Exception);

  TPrinterOrientation = (poPortrait,poLandscape,poReverseLandscape,poReversePortrait);
  TPrinterCapability  = (pcCopies, pcOrientation, pcCollation);
  TPrinterCapabilities= Set of TPrinterCapability;
  TPrinterState       = (psNoDefine,psReady,psPrinting,psStopped);
  TPrinterType        = (ptLocal,ptNetWork);

  {
   This object it's a base class for TCanvas for TPrinter Object.
   Few properties it's replicate for can create an TPrinterCavas not
   associated with TPrinter or override few values.
   
   BeginDoc,NewPage and EndDoc it's called in Printer.BeginDoc ...

   PaperWidth:  physical width of paper
   PaperHeight: Physical height of paper
   PageWidth:   Printable width on page
   PageHeight:  Printable height of paper
  }

  { TPrinterCanvas }

  TPrinterCanvas = class(TCanvas)
  private
    fPrinter      : TPrinter;
    fTitle        : String;
    fPageNum      : Integer;
    fTopMargin    : Integer;
    fLeftMargin   : Integer;
    fBottomMargin : Integer;
    fRightMargin  : Integer;
    fPaperWidth   : Integer;
    fPaperHeight  : Integer;
    fOrientation  : TPrinterOrientation;
    fXDPI,fYDPI    : Integer;

    function GetOrientation: TPrinterOrientation;
    function GetPageHeight: Integer;
    function GetPageWidth: Integer;
    function GetPaperHeight: Integer;
    function GetPaperWidth: Integer;
    function GetTitle: string;
    function GetXDPI: Integer;
    function GetYDPI: Integer;
    procedure SetOrientation(const AValue: TPrinterOrientation);
    procedure SetPaperHeight(const AValue: Integer);
    procedure SetPaperWidth(const AValue: Integer);
    procedure SetTitle(const AValue: string);
    function HasDefaultMargins: boolean;
    procedure SetXDPI(const AValue: Integer);
    procedure SetYDPI(const AValue: Integer);
  protected
    function GetLeftMargin: Integer;
    function GetTopMargin: Integer;
    function GetBottomMargin: Integer;
    function GetRightMargin: Integer;
  public
    constructor Create(APrinter: TPrinter); virtual;
    procedure BeginDoc; virtual;
    procedure NewPage;  virtual;
    procedure BeginPage; virtual;
    procedure EndPage; virtual;
    procedure EndDoc; virtual;
    procedure Changing; override;

    property Printer : TPrinter read fPrinter;
    
    property Title : string read GetTitle write SetTitle;
    property PageHeight : Integer read GetPageHeight;
    property PageWidth  : Integer read GetPageWidth;
    property PaperWidth : Integer read GetPaperWidth write SetPaperWidth;
    property PaperHeight: Integer read GetPaperHeight write SetPaperHeight;
    property PageNumber : Integer read fPageNum;
    property TopMargin : Integer read GetTopMargin write FTopMargin;
    property LeftMargin: Integer read GetLeftMargin write FLeftMargin;
    property BottomMargin: Integer read GetBottomMargin write FBottomMargin;
    property RightMargin: Integer read GetRightMargin write FRightMargin;
    property Orientation: TPrinterOrientation read GetOrientation Write SetOrientation;
    property XDPI: Integer read GetXDPI write SetXDPI;
    property YDPI: Integer read GetYDPI write SetYDPI;

  end;

  TPrinterCanvasRef = Class of TPrinterCanvas;
  
  { TFilePrinterCanvas }

  TFilePrinterCanvas = class(TPrinterCanvas)
  protected
    FOutputFileName: string;
  public
    property OutputFileName : string read FOutputFileName write FOutputFileName;
  end;

  TFilePrinterCanvasClass = class of TFilePrinterCanvas;

  TPaperRect = Record
    PhysicalRect : TRect;
    WorkRect     : TRect;
  end;

  TPaperItem = record
    PaperName: string[40];
    PaperRect: TPaperRect;
  end;

  { TPaperSize }

  TPaperSize = Class(TObject)
  private
    //The width and length are in points;
    //there are 72 points per inch.

    fOwnedPrinter      : TPrinter;
    fSupportedPapers   : TStringList;  //List of Paper supported by the current printer
    fLastPrinterIndex  : Integer;      //Last index of printer used

    function GetDefaultPaperName: string;
    function GetPhysPaperHeight: Integer;
    function GetPaperName: string;
    function GetPaperRect: TPaperRect;
    function GetPhysPaperWidth: Integer;
    function GetSupportedPapers: TStrings;
    procedure SetPaperName(const AName: string);
    function PaperRectOfName(const AName: string) : TPaperRect;
    procedure CheckSupportedPapers;
  private
    fInternalPapers    : array of TPaperItem;
    fDefaultPapers     : boolean;
    fDefaultPaperIndex : Integer;
    procedure CreateInternalPapers;
    procedure FillDefaultPapers;
    function GetDefaultPaperRect(const AName: string; var APaperRect:TPaperRect): Integer;
    function IndexOfDefaultPaper(const AName: string): Integer;
  public
    constructor Create(aOwner : TPrinter); overload;
    destructor Destroy; override;

    property DefaultPapers   : boolean read fDefaultPapers;
    property Width           : Integer read GetPhysPaperWidth;
    property Height          : Integer read GetPhysPaperHeight;
    property PaperName       : string read GetPaperName write SetPaperName;
    property DefaultPaperName: string read GetDefaultPaperName;

    property PaperRect       : TPaperRect read GetPaperRect;
    property SupportedPapers : TStrings read GetSupportedPapers;

    property PaperRectOf[aName : string] : TPaperRect read PaperRectOfName;
  end;

  TPrinterFlags = set of
    (
      pfPrinting,                //Printing
      pfAborted,                 //Abort  process
      pfDestroying,              //Printer object is being destroyed
      pfPrintersValid,           //fPrinters list is valid
      pfRawMode                  //Printer is in raw mode
    );

  { TPrinter }

  TPrinter = class(TObject)
  private
    fCanvas      : TCanvas;      //Active canvas object
    FFileName    : string;       //Filename for output file
    fFonts       : TStrings;     //Accepted font by printer
    fPageNumber  : Integer;      //Current page number
    fPrinters    : TStrings;     //Printers names list
    fPrinterIndex: Integer;      //selected printer index
    fTitle       : string;       //Title of current document
    //fCapabilities: TPrinterCapabilities;
    fPaperSize   : TPaperSize;
    fCanvasClass : TPrinterCanvasRef;
    fBins        : TStrings;
    fFlags       : TPrinterFlags;

    function GetAborted: Boolean;
    function GetCanvas: TCanvas;
    procedure CheckPrinting(Value: Boolean);
    function GetCanvasClass: TPrinterCanvasRef;
    function GetCopies: Integer;
    function GetFonts: TStrings;
    function GetOrientation: TPrinterOrientation;
    function GetPageHeight: Integer;
    function GetPageWidth: Integer;
    function GetPaperSize: TPaperSize;
    Function GetBinName: string;
    function GetDefaultBinName: string;
    function GetPrinterIndex: integer;
    function GetPrinterName: string;
    function GetPrinters: TStrings;
    function GetPrinting: Boolean;
    function GetRawMode: boolean;
    procedure SetCanvasClass(const AValue: TPrinterCanvasRef);
    procedure SetCopies(AValue: Integer);
    procedure SetOrientation(const AValue: TPrinterOrientation);
    procedure SetPrinterIndex(AValue: integer);
    procedure SetRawMode(const AValue: boolean);
    procedure SetBinName(const aName: string);
  protected
     procedure SelectCurrentPrinterOrDefault;
     
     procedure DoBeginDoc; virtual;
     procedure DoNewPage; virtual;
     procedure DoBeginPage; virtual;
     procedure DoEndPage; virtual;
     procedure DoEndDoc(aAborded : Boolean); virtual;
     procedure DoAbort; virtual;
     procedure DoResetPrintersList; virtual;
     procedure DoResetFontsList; virtual;
     
     procedure DoEnumPrinters(Lst : TStrings); virtual;
     procedure DoEnumFonts(Lst : TStrings); virtual;
     procedure DoEnumPapers(Lst : TStrings); virtual;
     procedure DoEnumBins(Lst : TStrings); virtual;
     procedure DoInitialization; virtual;
     function DoSetPrinter(aName : string): Integer; virtual;
     function DoGetCopies : Integer; virtual;
     procedure DoSetCopies(aValue : Integer); virtual;
     function DoGetOrientation: TPrinterOrientation; virtual;
     procedure DoSetOrientation(aValue : TPrinterOrientation); virtual;
     function DoGetDefaultPaperName: string; virtual;
     function DoGetPaperName: string; virtual;
     procedure DoSetPaperName(aName : string); virtual;
     function DoGetDefaultBinName: string; virtual;
     function DoGetBinName: string; virtual;
     procedure DoSetBinName(aName: string); virtual;
     function DoGetPaperRect(aName : string; Var aPaperRc : TPaperRect) : Integer; virtual;
     function DoGetPrinterState: TPrinterState; virtual;
     procedure DoDestroy; virtual;

     function GetPrinterType : TPrinterType; virtual;
     function GetCanPrint : Boolean; virtual;
     function GetCanRenderCopies : Boolean; virtual;
     function GetXDPI: Integer; virtual;
     function GetYDPI: Integer; virtual;
     function GetBins: TStrings; virtual;
     procedure CheckRawMode(const Value: boolean; Msg:string='');
     procedure RawModeChanging; virtual;
     procedure PrinterSelected; virtual;
     function  DoGetDefaultCanvasClass: TPrinterCanvasRef; virtual;

     property PrinterFlags: TPrinterFlags read fFlags write fFlags;
  public
     constructor Create; virtual;
     destructor Destroy; override;

     procedure Abort;
     procedure BeginDoc;
     procedure EndDoc;
     procedure NewPage;
     procedure BeginPage;
     procedure EndPage;
     procedure Refresh;
     procedure SetPrinter(aName : String);
     Procedure RestoreDefaultBin; virtual;
     function  Write(const Buffer; Count:Integer; out Written: Integer): Boolean; virtual;
     function  Write(const s: ansistring): boolean; overload;

     property PrinterIndex : integer read GetPrinterIndex write SetPrinterIndex;
     property PrinterName: string read GetPrinterName;
     property PaperSize : TPaperSize read GetPaperSize;
     property Orientation: TPrinterOrientation read GetOrientation write SetOrientation;
     property PrinterState : TPrinterState read DoGetPrinterState;
     property Copies : Integer read GetCopies write SetCopies;
     property Printers: TStrings read GetPrinters;
     property FileName: string read FFileName write FFileName;
     property Fonts: TStrings read GetFonts;
     property Canvas: TCanvas read GetCanvas;
     property CanvasClass: TPrinterCanvasRef read GetCanvasClass write SetCanvasClass;
     property PageHeight: Integer read GetPageHeight;
     property PageWidth: Integer read GetPageWidth;
     property PageNumber : Integer read fPageNumber;
     property Aborted: Boolean read GetAborted;
     property Printing: Boolean read GetPrinting;
     property Title: string read fTitle write fTitle;
     property PrinterType : TPrinterType read GetPrinterType;
     property CanPrint : Boolean read GetCanPrint;
     property CanRenderCopies : Boolean read GetCanRenderCopies;
     property XDPI : Integer read GetXDPI;
     property YDPI : Integer read GetYDPI;
     property RawMode: boolean read GetRawMode write SetRawMode;
     property DefaultBinName: string read GetDefaultBinName;
     property BinName: string read GetBinName write SetBinName;
     property SupportedBins: TStrings read GetBins;
  end;
  
// TPrinter it's an basic object. If you override this object,
// you must create an instance.
var
  Printer: TPrinter = nil;
  
implementation

{ TPrinter }

constructor TPrinter.Create;
begin
  if ClassType=TPrinter then
    raise Exception.Create('TPrinter is an abstract base class.'
    +' Please use a printer implementation like the package printers4lazarus.');
  Inherited Create;
  fPrinterIndex:=-1;  //By default, use the default printer
  fCanvas:=nil;
  fPaperSize:=nil;
  fBins:=nil;
  fTitle:='';
end;

destructor TPrinter.Destroy;
begin
  Include(fFlags, pfDestroying);
  DoDestroy;
  inherited Destroy;
end;

//Abort the current document
procedure TPrinter.Abort;
begin
  //Check if Printer print otherwise, exception
  CheckPrinting(True);

  DoAbort;
  
  Include(fFlags, pfAborted);
  EndDoc;
end;

//Begin a new document
procedure TPrinter.BeginDoc;
begin
  //Check if Printer not printing otherwise, exception
  CheckPrinting(False);
  
  //If not selected printer, set default printer
  SelectCurrentPrinterOrDefault;

  Include(fFlags, pfPrinting);
  Exclude(fFlags, pfAborted);
  fPageNumber := 1;
  
  if not RawMode then begin
    Canvas.Refresh;
    TPrinterCanvas(Canvas).BeginDoc;
  end;
  //Call the specifique Begindoc
  DoBeginDoc;

  BeginPage;

  // Set font resolution
  if not RawMode then
    Canvas.Font.PixelsPerInch := YDPI;
end;

//End the current document
procedure TPrinter.EndDoc;
begin

  EndPage;

  //Check if Printer print otherwise, exception
  CheckPrinting(True);

  if not RawMode then
    TPrinterCanvas(Canvas).EndDoc;
  
  DoEndDoc(pfAborted in fFlags);

  Exclude(fFlags, pfPrinting);
  Exclude(fFlags, pfAborted);
  fPageNumber := 0;
end;

//Create an new page
procedure TPrinter.NewPage;
begin
  if TMethod(@Self.DoNewPage).Code = Pointer(@TPrinter.DoNewPage) then
  begin
    // DoNewPage has not been overriden, use the new method
    EndPage;
    BeginPage;
  end else
  begin
    // Use the old method as DoNewPage has been overriden in descendat TPrinter
    CheckPrinting(True);
    Inc(fPageNumber);
    if not RawMode then
      TPrinterCanvas(Canvas).NewPage;
    DoNewPage;
  end;
end;

procedure TPrinter.BeginPage;
begin
  CheckPrinting(True);
  inc(fPageNumber);
  if not RawMode then
    TPrinterCanvas(Canvas).BeginPage;
  DoBeginPage;
end;

procedure TPrinter.EndPage;
begin
  if not RawMode then
    TPrinterCanvas(Canvas).EndPage;
  DoEndPage;
end;

//Clear Printers & Fonts list
procedure TPrinter.Refresh;
var
  OldPrinter: string;
begin
  //Check if Printer not printing otherwise, exception
  CheckPrinting(False);

  if FPrinterIndex>=0 then
    OldPrinter := fPrinters[FPrinterIndex]
  else
    OldPrinter := '';

  if Assigned(fPrinters) then
  begin
    DoResetPrintersList;
    FreeAndNil(fPrinters);
  end;
  
  if Assigned(fFonts) then
  begin
    DoResetFontsList;
    FreeAndNil(fFonts);
  end;

  // need to refill printers here otherwise
  // it wont be filled on getting printers
  // due to only one initialization
  GetPrinters;

  fPrinterIndex:=-1;

  // try to locate old selected printer
  if OldPrinter<>'' then
    SetPrinter(OldPrinter);
end;

//Set the current printer
procedure TPrinter.SetPrinter(aName: String);
var
  i,oldIndex : Integer;
begin
  if aName='*' then begin
    // select default printer
    OldIndex := FPrinterIndex;
    fPrinterIndex := -1; // avoid to remember last printer
    Refresh;
    if Printers.count>0 then begin
      i:= doSetprinter(FPrinters[0]); // now first printer is default
      if i<>0 then begin
        // something went wrong, try to restore old printer
        if OldIndex>=0 then
          FPrinterIndex := doSetPrinter(FPrinters[OldIndex]);
        raise EPrinter.Create('Unable to set default printer!');
      end else
        FPrinterIndex := i;
    end;
  end else
  if (Printers.Count>0) then
  begin
    if (aName<>'') then
    begin
      //Printer changed ?
      if fPrinters.IndexOf(aName)<>fPrinterIndex then
      begin
        i:=DoSetPrinter(aName);
        if i<0 then
          raise EPrinter.Create(Format('Printer "%s" doesn''t exist.',[aName]));
        fPrinterIndex:=i;
      end;
    end;
  end;
  PrinterSelected;
end;

procedure TPrinter.RestoreDefaultBin;
begin
  DoSetBinName(DoGetDefaultBinName);
end;

function TPrinter.Write(const Buffer; Count: Integer; out Written: Integer): Boolean;
begin
  result := False;
  Written := 0;
end;

function TPrinter.Write(const s: ansistring): boolean;
var
  Written: integer;
begin
  Result := Write(S[1], Length(S), Written);
end;

//Return an Canvas object
function TPrinter.GetCanvas: TCanvas;
begin
  Result := nil;
  
  CheckRawMode(False, 'Canvas not allowed in Raw Mode');
  
  if not Assigned(fCanvas) then
  begin
    if not Assigned(CanvasClass) then
      raise Exception.Create('Canvas Class not defined.');

    fCanvas:=CanvasClass.Create(Self);
  end;
  
  Result:=fCanvas;
end;

function TPrinter.GetAborted: Boolean;
begin
  Result := (pfAborted in fFlags);
end;

//Raise error if Printer.Printing is not Value
procedure TPrinter.CheckPrinting(Value: Boolean);
begin
  if Printing<>Value then
  begin
    if Value then
      raise EPrinter.Create('Printer is not printing')
    else
      raise Eprinter.Create('Printer is printing');
  end;
end;

function TPrinter.GetCanvasClass: TPrinterCanvasRef;
begin
  if RawMode then
    result := nil
  else
  if FCanvasClass=nil then
    Result := DoGetDefaultCanvasClass
  else
    Result := FCanvasClass;
end;

procedure TPrinter.CheckRawMode(const Value: boolean; Msg: string);
begin
  if RawMode<>Value then
  begin
    if msg='' then
      if Value then
        Msg:='Printer is in Raw Mode'
      else
        Msg:='Printer is not in Raw Mode';
    raise EPrinter.Create(msg);
  end;
end;

procedure TPrinter.RawModeChanging;
begin
  //
end;

procedure TPrinter.PrinterSelected;
begin
end;

function TPrinter.DoGetDefaultCanvasClass: TPrinterCanvasRef;
begin
  result := TPrinterCanvas;
end;

//Get current copies number
function TPrinter.GetCopies: Integer;
Var i : Integer;
begin
  Result:=1;
  i:=DoGetCopies;
  if i>0 then
    Result:=i;
end;

//Return & initialize the Fonts list
function TPrinter.GetFonts: TStrings;
begin
  if not Assigned(fFonts) then
    fFonts:=TStringList.Create;
  Result:=fFonts;

  //Only 1 initialization
  if fFonts.Count=0 then
    DoEnumFonts(fFonts);
end;

function TPrinter.GetOrientation: TPrinterOrientation;
begin
  Result:=DoGetOrientation;
end;

// Returns the height in points (pixels) of printable area
function TPrinter.GetPageHeight: Integer;
begin
  Result:=0;
  if (Printers.Count>0) then
    with PaperSize.PaperRect.WorkRect do
      Result:=Bottom-Top;
end;

// Returns the width in points (pixels) of the printable area
function TPrinter.GetPageWidth: Integer;
begin
  Result:=0;
  if (Printers.Count>0) then
    with PaperSize.PaperRect.WorkRect do
      // PageWidth is the size in "pixels" of the printable area
      Result:=Right-Left;
end;

function TPrinter.GetPaperSize: TPaperSize;
begin
  if not Assigned(fPaperSize)  then
    fPaperSize:=TPaperSize.Create(self);
  Result:=fPaperSize;
end;

function TPrinter.GetBinName: string;
begin
  result := doGetBinName;
end;

function TPrinter.GetDefaultBinName: string;
begin
  result := doGetDefaultBinName;
end;

//Return the current selected printer
function TPrinter.GetPrinterIndex: integer;
begin
  Result:=fPrinterIndex;
  if (Result<0) and (Printers.Count>0) then
     Result:=0; //printer by default
end;

function TPrinter.GetPrinterName: string;
begin
  if PrinterIndex<0 then
    result := ''
  else
    result := Printers[PrinterIndex];
end;

//Return & initialize the printers list
function TPrinter.GetPrinters: TStrings;
begin
  if not Assigned(fPrinters) then
    fPrinters:=TStringList.Create;
  Result:=fPrinters;
  
  //Only 1 initialization
  if [pfPrintersValid, pfDestroying]*fFlags = [] then begin
    Include(fFlags, pfPrintersValid);
    DoEnumPrinters(fPrinters);
    if FPrinters.Count>0 then
      SelectCurrentPrinterOrDefault;
    DoInitialization;
  end;
end;

function TPrinter.GetPrinting: Boolean;
begin
  result := (pfPrinting in fFlags);
end;

function TPrinter.GetRawMode: boolean;
begin
  result := (pfRawMode in fFlags);
end;

procedure TPrinter.SetCanvasClass(const AValue: TPrinterCanvasRef);
begin
  FCanvasClass := AValue;
end;

//Return XDPI
function TPrinter.GetXDPI: Integer;
begin
  Result:=1;
end;

//Return YDPI
function TPrinter.GetYDPI: Integer;
begin
  Result:=1;
end;

function TPrinter.GetBins: TStrings;
begin
  if fBins=nil then
    fBins := TStringList.Create;

  doEnumBins(fBins);

  result := fBins;
end;

//Set copies number
procedure TPrinter.SetCopies(AValue: Integer);
begin
  CheckPrinting(False);
  if aValue<1 then aValue:=1;
  if Printers.Count>0 then
    DoSetCopies(aValue)
  else
    raise EPrinter.Create('No printers found.');
end;

procedure TPrinter.SetOrientation(const AValue: TPrinterOrientation);
begin
  DoSetOrientation(aValue);
end;

//Set selected printer
procedure TPrinter.SetPrinterIndex(AValue: integer);
Var aName : String;
begin
  if fPrinterIndex=AValue then exit;
  CheckPrinting(False);
  if Printers.Count>0 then
  begin
    if AValue=-1 then
      aName:='*'
    else
      if (AValue>=0) and (AValue<Printers.Count) then
        aName:=Printers.Strings[AValue]
      else
        raise EPrinter.Create('Printer index out of range!');
    SetPrinter(aName);
    DoResetFontsList;
  end
  else
    raise EPrinter.Create('No printers defined!');
end;

procedure TPrinter.SetRawMode(const AValue: boolean);
begin
  if AValue<>RawMode then begin
    CheckPrinting(False);
    RawModeChanging;
    if AValue then
      Include(fFlags, pfRawMode)
    else
      Exclude(fFlags, pfRawMode);
  end;
end;

procedure TPrinter.SetBinName(const aName: string);
begin
  CheckPrinting(False);
  DoSetBinName(aName);
end;

//If not Printer selected, Select the default printer
procedure TPrinter.SelectCurrentPrinterOrDefault;
begin
  if (fPrinterIndex<0) and (Printers.Count>0) then
    PrinterIndex:=0;
end;

procedure TPrinter.DoBeginDoc;
begin
  //Override this method
end;

procedure TPrinter.DoNewPage;
begin
  //Override this method
end;

procedure TPrinter.DoBeginPage;
begin
  //Override this method
end;

procedure TPrinter.DoEndPage;
begin
  //Override this method
end;

procedure TPrinter.DoEndDoc(aAborded : Boolean);
begin
  //Override this method
end;

procedure TPrinter.DoAbort;
begin
 //Override this method
end;

procedure TPrinter.DoResetPrintersList;
begin
 //Override this method
  Exclude(fFlags, pfPrintersValid);
end;

procedure TPrinter.DoResetFontsList;
begin
  if fFonts<>nil then
    fFonts.Clear;
end;

//Initialize the Lst with all definied printers
procedure TPrinter.DoEnumPrinters(Lst: TStrings);
begin
 //Override this method
 //Warning: The default printer must be the first printer
 //          (fPrinters[0])
end;

//Initialize the Lst with all supported fonts
procedure TPrinter.DoEnumFonts(Lst: TStrings);
begin
 //Override this method
end;

//Initialize the Lst with all supported papers names
procedure TPrinter.DoEnumPapers(Lst: TStrings);
begin
  //DebugLn(['TPrinter.DoEnumPapers ',dbgsName(Self)]);
  
 //Override this method
end;

procedure TPrinter.DoEnumBins(Lst : TStrings);
begin
  // Override this method
end;

// This method is called once after the printer list
// is obtained for the first time.
procedure TPrinter.DoInitialization;
begin
  //Override this method
end;


//Set the current printer
function TPrinter.DoSetPrinter(aName : string): Integer;
begin
  //Override this method. The result must be
  //the index of aName printer in Printers list
  //if the aName doesn't exist, return -1
  Result:=-1;
end;

//Get the current copies nulbers
function TPrinter.DoGetCopies: Integer;
begin
 //Override this method
 Result:=1;
end;

//Set copies number
procedure TPrinter.DoSetCopies(aValue: Integer);
begin
 //Override this method
end;

//Return current paper orientation
function TPrinter.DoGetOrientation: TPrinterOrientation;
begin
  Result:=poPortrait;
  //Override this method
end;

//Set paper Orientation
procedure TPrinter.DoSetOrientation(aValue: TPrinterOrientation);
begin
 //Override this method
end;

//Return the default paper name for the selected printer
function TPrinter.DoGetDefaultPaperName: string;
begin
  Result:='';
  //Override this methode
end;

//Return selected paper name for the current printer
function TPrinter.DoGetPaperName: string;
begin
  Result:='';
  //Override this method
end;

procedure TPrinter.DoSetPaperName(aName: string);
begin
  //Override this method
end;

function TPrinter.DoGetDefaultBinName: string;
begin
  Result:='';
end;

function TPrinter.DoGetBinName: string;
begin
  result := '';
end;

procedure TPrinter.DoSetBinName(aName: string);
begin
  if SupportedBins.Count>0 then
    DebugLn('Warning: bin %s is not allowed',[aName]);
end;

//Initialise aPaperRc with the aName paper rect
//Result : -1 no result
//          0 aPaperRc.WorkRect is a margins
//          1 aPaperRc.WorkRect is really the work rect
function TPrinter.DoGetPaperRect(aName : string; var aPaperRc: TPaperRect): Integer;
begin
  Result:=-1;
  //Override this method
end;

//Get a state of current printer
function TPrinter.DoGetPrinterState: TPrinterState;
begin
  //Override this method
  Result:=psNoDefine;
end;

procedure TPrinter.DoDestroy;
begin
  if Printing then
    Abort;

  fBins.free;

  if Assigned(fCanvas) then
    fCanvas.Free;

  if Assigned(fPaperSize) then
     fPaperSize.Free;


  if Assigned(fPrinters) then
  begin
    DoResetPrintersList;
    FreeAndNil(fPrinters);
  end;

  if Assigned(fFonts) then
  begin
    DoResetFontsList;
    FreeAndNil(fFonts);
  end;
end;

//Return the type of selected printer
function TPrinter.GetPrinterType: TPrinterType;
begin
  Result:=ptLocal;
end;

//Return True if selected printer is able to print
function TPrinter.GetCanPrint: Boolean;
begin
  Result:=True;
end;

function TPrinter.GetCanRenderCopies: Boolean;
begin
  Result:=True;
end;

{ TPaperSize }

procedure TPaperSize.CreateInternalPapers;
  procedure add(AnIndex:Integer; aname:string; aPhysRect,aWrkRect:TRect);
  begin
    with fInternalPapers[AnIndex] do begin
      PaperName := aName;
      PaperRect.PhysicalRect := aPhysRect;
      PaperRect.WorkRect := aWrkRect;
    end;
  end;
  function PRRect(const ALeft,ATop,ARight,ABottom: Integer): TRect;
  begin
    Result.Left := ALeft;
    Result.Top := ATop;
    Result.Right  := round(ARight * FOwnedPrinter.XDPI / 72);
    Result.Bottom := round(ABottom * FOwnedPrinter.XDPI / 72);
  end;
begin
  if Length(fInternalPapers)=0 then
  begin
    SetLength(fInternalPapers, 3);
    add(0, 'Letter',    PRRect(0, 0, 612,  792 ), PRRect(0,   0,   612, 792 ));
    add(1, 'A4',        PRRect(0, 0, 595,  892 ), PRRect(0,   0,   595, 892 ));
    add(2, 'Legal',     PRRect(0, 0, 612,  1008), PRRect(0,   0,   612, 1008));
  end;
end;

procedure TPaperSize.FillDefaultPapers;
var
  i: Integer;
begin
  FSupportedPapers.Clear;
  CreateInternalPapers;
  for i:=0 to Length(FInternalPapers)-1 do
    FSupportedPapers.Add(FInternalPapers[i].PaperName);
  FDefaultPaperIndex := 0;
  FDefaultPapers := true;
end;

function TPaperSize.GetDefaultPaperName: string;
begin
  CheckSupportedPapers;

  if fDefaultPapers then
    Result := FSupportedPapers[0]
  else
    Result := fOwnedPrinter.DoGetDefaultPaperName;
end;

function TPaperSize.GetDefaultPaperRect(const AName: string;
  var APaperRect:TPaperRect): Integer;
var
  PR: TPaperRect;
begin
  Result := IndexOfDefaultPaper(AName);
  if Result>=0 then
  PR:=FInternalPapers[Result].PaperRect;
  if FOwnedPrinter.Orientation in [poPortrait, poReversePortrait] then
  begin
    APaperRect.PhysicalRect := PR.PhysicalRect;
    APaperRect.WorkRect     := PR.WorkRect;
  end else
  begin
    APaperRect.PhysicalRect.Left   := 0;
    APaperRect.PhysicalRect.Top    := 0;
    APaperRect.PhysicalRect.Right  := PR.PhysicalRect.Bottom;
    APaperRect.Physicalrect.Bottom := PR.PhysicalRect.Right;

    APaperRect.WorkRect.Left   := PR.WorkRect.Top;
    APaperRect.WorkRect.Top    := PR.PhysicalRect.Right-PR.WorkRect.Right;
    APaperRect.WorkRect.Right  := PR.WorkRect.Bottom;
    APaperRect.WorkRect.Bottom := PR.PhysicalRect.Right-PR.Workrect.Left;
  end;
end;

function TPaperSize.GetPhysPaperHeight: Integer;
begin
  result := PaperRect.PhysicalRect.Bottom - PaperRect.PhysicalRect.Top;
end;

function TPaperSize.GetPaperName: string;
begin
  CheckSupportedPapers;

  if fDefaultPapers then
    Result := SupportedPapers[FDefaultPaperIndex]
  else
    Result := fOwnedPrinter.DoGetPaperName;

  if Result='' then
    Result:=DefaultPaperName;
end;

function TPaperSize.GetPaperRect: TPaperRect;
begin
  Result:=PaperRectOfName(PaperName);
end;

function TPaperSize.GetPhysPaperWidth: Integer;
begin
  result := PaperRect.PhysicalRect.Right - PaperRect.PhysicalRect.Left;
end;

function TPaperSize.GetSupportedPapers: TStrings;
begin
  CheckSupportedPapers;

  Result:=fSupportedPapers;
end;

function TPaperSize.IndexOfDefaultPaper(const AName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i:=0 to Length(fInternalPapers)-1 do
    if CompareText(fInternalPapers[i].PaperName, AName)=0 then
    begin
      Result := i;
      break;
    end;
end;

procedure TPaperSize.SetPaperName(const AName: string);
begin
  if SupportedPapers.IndexOf(aName)<>-1 then
  begin
    if aName<>PaperName then
    begin
      if fDefaultPapers then
        FDefaultPaperIndex := IndexOfDefaultPaper(AName)
      else
        FOwnedPrinter.DoSetPaperName(aName)
    end;
  end
  else
    raise EPrinter.Create(Format('Paper "%s" not supported!',[aName]));
end;

//Return an TPaperRect corresponding at an paper name
function TPaperSize.PaperRectOfName(const AName: string): TPaperRect;
var TmpPaperRect : TPaperRect;
    Margins      : Integer;
begin
  FillChar(Result,SizeOf(Result),0);

  if SupportedPapers.IndexOf(AName)<>-1 then
  begin

    if fDefaultPapers then
      Margins := GetDefaultPaperRect(AName, TmpPaperRect)
    else
      Margins := fOwnedPrinter.DoGetPaperRect(aName,TmpPaperRect);

    if Margins>=0 then
      Result := TmpPaperRect
    else
      raise EPrinter.Create(Format('The paper "%s" has no defined rectangle!',[aName]));

  end
  else raise EPrinter.Create(Format('Paper "%s" not supported!',[aName]));
end;

procedure TPaperSize.CheckSupportedPapers;
begin
  if (fSupportedPapers.Count=0) or
     (fLastPrinterIndex<>fOwnedPrinter.PrinterIndex) then
  begin
    fOwnedPrinter.SelectCurrentPrinterOrDefault;

    fSupportedPapers.Clear;
    fDefaultPapers := false;
    //DebugLn(['TPaperSize.GetSupportedPapers ',dbgsName(fOwnedPrinter),' ',dbgsName(Printer),' ',fOwnedPrinter=Printer]);
    fOwnedPrinter.DoEnumPapers(fSupportedPapers);

    if fSupportedPapers.Count=0 then
      FillDefaultPapers;

    fLastPrinterIndex:=fOwnedPrinter.PrinterIndex;
  end;
end;

constructor TPaperSize.Create(aOwner: TPrinter);
begin
  if not assigned(aOwner) then
    raise Exception.Create('TMediaSize.Create, aOwner must be defined!');
  Inherited Create;

  fLastPrinterIndex:=-2;
  fOwnedPrinter:=aOwner;
  fSupportedPapers:=TStringList.Create;
end;

destructor TPaperSize.Destroy;
begin
  fSupportedPapers.Free;

  inherited Destroy;
end;

{ TPrinterCanvas }

function TPrinterCanvas.GetTitle: string;
begin
  if Assigned(fPrinter) then
    Result:=fPrinter.Title
  else
    Result:=fTitle;
end;

function TPrinterCanvas.GetXDPI: Integer;
begin
  if Printer<>nil then
    result := Printer.XDPI
  else
  if fXDPI <= 0 then
    result := 300
  else
    result := fXDPI;
end;

function TPrinterCanvas.GetYDPI: Integer;
begin
  if Printer<>nil then
    result := Printer.YDPI
  else
  if fYDPI <= 0 then
    result := 300
  else
    result := fYDPI;
end;

procedure TPrinterCanvas.SetOrientation(const AValue: TPrinterOrientation);
begin
  if Assigned(fPrinter) then
    fPrinter.Orientation := AValue
  else
    fOrientation := AValue;
end;

function TPrinterCanvas.GetOrientation: TPrinterOrientation;
begin
  if fPrinter<>nil then
    result := fPrinter.Orientation
  else
    result := fOrientation;
end;

function TPrinterCanvas.GetPageHeight: Integer;
begin
  if Assigned(fPrinter) and HasDefaultMargins then
    Result:=fPrinter.PageHeight
  else
    Result:= PaperHeight - TopMargin - BottomMargin;
end;

function TPrinterCanvas.GetPageWidth: Integer;
begin
  if Assigned(fPrinter) and HasDefaultMargins then
    Result:=fPrinter.PageWidth
  else
    Result:= PaperWidth - LeftMargin - RightMargin;
end;

function TPrinterCanvas.GetPaperHeight: Integer;
begin
  if Assigned(fPrinter) then
    result := fPrinter.PaperSize.Height
  else
  if fPaperHeight<=0 then
    result :=  round(YDPI * 842 / 72)   // default to A4 paper
  else
    result := fPaperHeight;
end;

function TPrinterCanvas.GetPaperWidth: Integer;
begin
  if Assigned(fPrinter) then
    result := fPrinter.PaperSize.Width
  else
  if fPaperWidth<=0 then
    result := round(XDPI * 595 / 72)    // default to A4 paper
  else
    result := fPaperWidth;
end;

procedure TPrinterCanvas.SetPaperHeight(const AValue: Integer);
begin
  fPaperHeight := AValue;
end;

procedure TPrinterCanvas.SetPaperWidth(const AValue: Integer);
begin
  fPaperWidth := AValue;
end;

procedure TPrinterCanvas.SetTitle(const AValue: string);
begin
  if Assigned(fPrinter) then
    fPrinter.Title:=aValue
  else
    fTitle:=aValue;
end;

function TPrinterCanvas.HasDefaultMargins: boolean;
begin
  result := (FLeftMargin=0) and (FRightMargin=0) and
            (FTopMargin=0) and (FBottomMargin=0);
end;

procedure TPrinterCanvas.SetXDPI(const AValue: Integer);
begin
  fXDPI := AValue;
end;

procedure TPrinterCanvas.SetYDPI(const AValue: Integer);
begin
  fYDPI := AValue;
end;

constructor TPrinterCanvas.Create(APrinter: TPrinter);
begin
  inherited Create;
  fPrinter:=aPrinter;
end;

procedure TPrinterCanvas.Changing;
begin
  if Assigned(fPrinter)  then
    fPrinter.CheckPrinting(True);
  inherited Changing;
end;

procedure TPrinterCanvas.BeginDoc;
begin
  fPageNum:=1;
end;

procedure TPrinterCanvas.NewPage;
begin
  BeginPage;
end;

procedure TPrinterCanvas.BeginPage;
begin
  Inc(fPageNum);
end;

procedure TPrinterCanvas.EndPage;
begin

end;

procedure TPrinterCanvas.EndDoc;
begin
  //No special action
end;

function TPrinterCanvas.GetLeftMargin: Integer;
begin
  if (fLeftMargin=0) and (fPrinter<>nil) then
    Result:=fPrinter.PaperSize.PaperRect.WorkRect.Left
  else
    Result:=fLeftMargin;
end;

function TPrinterCanvas.GetTopMargin: Integer;
begin
  if (fTopMargin=0) and (fPrinter<>nil) then
    Result:=fPrinter.PaperSize.PaperRect.WorkRect.Top
  else
    Result:=fTopMargin;
end;

function TPrinterCanvas.GetBottomMargin: Integer;
begin
  if (fBottomMargin=0) and (fPrinter<>nil) then
  begin
    with fPrinter.Papersize.PaperRect do
      Result := PhysicalRect.Bottom-WorkRect.Bottom;
  end else
    Result := fBottomMargin;
end;

function TPrinterCanvas.GetRightMargin: Integer;
var
  PR: TPaperRect;
begin
  if (fRightMargin=0) and (fPrinter<>nil) then
  begin
    PR:=fPrinter.Papersize.PaperRect;
    Result := PR.PhysicalRect.Right-PR.WorkRect.Right;
  end else
    Result := fRightMargin;
end;


procedure doFreePrinter;
begin
  if Assigned(Printer) then
    Printer.Free;
  Printer := nil;
end;

initialization
  RegisterInterfaceFinalizationHandler(@doFreePrinter);

end.
