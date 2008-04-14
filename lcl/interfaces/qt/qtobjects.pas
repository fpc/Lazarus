{
 *****************************************************************************
 *                              QtObjects.pas                                *
 *                              --------------                               *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit qtobjects;

{$mode objfpc}{$H+}

interface

{$I qtdefines.inc}

uses
  // Bindings
  qt4,
  // Free Pascal
  Classes, SysUtils, Types,
  // LCL
  LCLType, LCLIntf, Menus, LCLProc, Graphics;

type
  // forward declarations
  TQtImage = class;
  TQtFontMetrics = class;

  { TQtObject }
  TQtObject = class(TObject)
  private
    FUpdateCount: Integer;
    FInEventCount: Integer;
    FReleaseInEvent: Boolean;
  public
    FEventHook: QObject_hookH;
    TheObject: QObjectH;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Release; virtual;
  public
    procedure AttachEvents; virtual;
    procedure DetachEvents; virtual;
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl; virtual; abstract;
    procedure BeginEventProcessing;
    procedure EndEventProcessing;
    function InEvent: Boolean;
  public
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    function InUpdate: Boolean;
  end;

  { TQtResource }

  TQtResource = class(TObject)
  public
    Owner: TObject;
    FShared: Boolean;
  end;

  { TQtAction }

  TQtAction = class(TObject)
  private
    FIcon: QIconH;
  public
    Handle: QActionH;
    MenuItem: TMenuItem;
  public
    constructor Create(const AHandle: QActionH);
    destructor Destroy; override;
  public
    procedure SlotTriggered(checked: Boolean = False); cdecl;
  public
    procedure setChecked(p1: Boolean);
    procedure setCheckable(p1: Boolean);
    procedure setEnabled(p1: Boolean);
    procedure setIcon(const AIcon: QIconH);
    procedure setImage(const AImage: TQtImage);
    procedure setVisible(p1: Boolean);
  end;

  { TQtImage }

  TQtImage = class(TObject)
  private
    FData: PByte;
    FDataOwner: Boolean;
  public
    Handle: QImageH;
  public
    constructor Create;
    constructor Create(vHandle: QImageH); overload;
    constructor Create(AData: PByte; width: Integer; height: Integer; format: QImageFormat; const ADataOwner: Boolean = False); overload;
    constructor Create(AData: PByte; width: Integer; height: Integer; bytesPerLine: Integer; format: QImageFormat; const ADataOwner: Boolean = False); overload;
    destructor Destroy; override;
    function AsIcon(AMode: QIconMode = QIconNormal; AState: QIconState = QIconOff): QIconH;
    function AsPixmap(flags: QtImageConversionFlags = QtAutoColor): QPixmapH;
    function AsBitmap(flags: QtImageConversionFlags = QtAutoColor): QBitmapH;
    procedure CopyFrom(AImage: QImageH; x, y, w, h: integer);
  public
    function height: Integer;
    function width: Integer;
    function bits: PByte;
    function numBytes: Integer;
    procedure invertPixels(InvertMode: QImageInvertMode = QImageInvertRgb);
  end;

  { TQtFont }

  TQtFont = class(TQtResource)
  private
    FMetrics: TQtFontMetrics;
    function GetMetrics: TQtFontMetrics;
  public
    Widget: QFontH;
    Angle: Integer;
  public
    constructor Create(CreateHandle: Boolean; Const AShared: Boolean = False); virtual;
    destructor Destroy; override;
  public
    function getPointSize: Integer;
    function getPixelSize: Integer;
    function getWeight: Integer;
    function getItalic: Boolean;
    function getBold: Boolean;
    function getUnderline: Boolean;
    function getStrikeOut: Boolean;
    function getFamily: WideString;

    procedure setPointSize(p1: Integer);
    procedure setPixelSize(p1: Integer);
    procedure setWeight(p1: Integer);
    procedure setBold(p1: Boolean);
    procedure setItalic(b: Boolean);
    procedure setUnderline(p1: Boolean);
    procedure setStrikeOut(p1: Boolean);
    procedure setRawName(p1: string);
    procedure setFamily(p1: string);
    procedure family(retval: PWideString);
    function fixedPitch: Boolean;
    
    property Metrics: TQtFontMetrics read GetMetrics;
  end;

  { TQtFontMetrics }

  TQtFontMetrics = class(TObject)
  private
  public
    Widget: QFontMetricsH;
  public
    constructor Create(Parent: QFontH); virtual;
    destructor Destroy; override;
  public
    function height: Integer;
    function width(p1: PWideString): Integer;
    function ascent: Integer;
    function descent: Integer;
    function leading: Integer;
    function maxWidth: Integer;
    procedure boundingRect(retval: PRect; r: PRect; flags: Integer; text: PWideString; tabstops: Integer = 0; tabarray: PInteger = nil);
    function charWidth(str: WideString; pos: Integer): Integer;
  end;

  { TQtBrush }

  TQtBrush = class(TQtResource)
  private
  public
    Widget: QBrushH;
    constructor Create(CreateHandle: Boolean; const AShared: Boolean = False); virtual;
    destructor Destroy; override;
    function getColor: PQColor;
    procedure setColor(AColor: PQColor);
    procedure setStyle(style: QtBrushStyle);
    procedure setTexture(pixmap: QPixmapH);
    procedure setTextureImage(image: QImageH);
  end;

  { TQtPen }

  TQtPen = class(TQtResource)
  private
  public
    Widget: QPenH;
    constructor Create(CreateHandle: Boolean; Const AShared: Boolean = False); virtual;
    destructor Destroy; override;
  public
    function Width: Integer;
    function Style: QtPenStyle;
    function getColor: TQColor;
    procedure setStyle(AStyle: QtPenStyle);
    procedure setBrush(brush: QBrushH);
    procedure setWidth(p1: Integer);
    procedure setColor(p1: TQColor);
  end;


  { TQtRegion }

  TQtRegion = class(TQtResource)
  private
  public
    Widget: QRegionH;
    constructor Create(CreateHandle: Boolean); virtual; overload;
    constructor Create(CreateHandle: Boolean; X1,Y1,X2,Y2: Integer;
      Const RegionType: QRegionRegionType = QRegionRectangle); virtual; overload;
    constructor Create(CreateHandle: Boolean; Poly: QPolygonH;
      Const Fill: QtFillRule = QtWindingFill); virtual; overload;
    destructor Destroy; override;
    function GetRegionType: integer;
    function getBoundingRect: TRect;
  end;

  // NOTE: PQtDCData was a pointer to a structure with QPainter information
  //       about current state, currently this functionality is implemented
  //       using native functions qpainter_save and qpainter_restore. If in
  //       future it needs to save/restore aditional information, PQtDCData
  //       should point to a structure holding the additional information.
  //       see SaveDC and RestoreDC for more information.
  //       for example: what about textcolor, it's currently not saved....

  {
  TQtDCData = record
  end;
  PQtDCData = ^TQtDCData;
  }
  PQtDCData = pointer;

  { TQtDeviceContext }

  TQtDeviceContext = class(TObject)
  private
    FPenPos: TQtPoint;
    FOwnPainter: Boolean;
    SelFont: TQTFont;
    SelBrush: TQTBrush;
    SelPen: TQtPen;
    PenColor: TQColor;
    procedure RestorePenColor;
    procedure RestoreTextColor;
  public
    { public fields }
    Widget: QPainterH;
    Parent: QWidgetH;
    ParentPixmap: QPixmapH;
    vBrush: TQtBrush;
    vFont: TQtFont;
    vImage: QImageH;
    vPen: TQtPen;
    vRegion: TQtRegion;
    vBackgroundBrush: TQtBrush;
    vClipRect: PRect;         // is the cliprect paint event give to us
    vClipRectDirty: boolean;  // false=paint cliprect is still valid
    vTextColor: TColor;
  public
    { Our own functions }
    constructor Create(AWidget: QWidgetH; const APaintEvent: Boolean = False); virtual;
    constructor CreateFromPainter(APainter: QPainterH);
    destructor Destroy; override;
    procedure CreateObjects;
    function CreateDCData: PQtDCDATA;
    function RestoreDCData(var DCData: PQtDCData): boolean;
    procedure DebugClipRect(const msg: string);
    procedure setImage(AImage: TQtImage);
    procedure CorrectCoordinates(var ARect: TRect);
  public
    { Qt functions }
    
    procedure qDrawPlainRect(x, y, w, h: integer; AColor: PQColor = nil;
      lineWidth: Integer = 1; FillBrush: QBrushH = nil);
    procedure qDrawShadeRect(x, y, w, h: integer; Palette: QPaletteH = nil; Sunken: Boolean = False;
      lineWidth: Integer = 1; midLineWidth: Integer = 0; FillBrush: QBrushH = nil);
    procedure qDrawWinPanel(x, y, w, h: integer; Palette: QPaletteH = nil; Sunken: Boolean = False;
      lineWidth: Integer = 1; FillBrush: QBrushH = nil);
    
    procedure drawPoint(x1: Integer; y1: Integer);
    procedure drawRect(x1: Integer; y1: Integer; w: Integer; h: Integer);
    procedure drawRoundRect(x, y, w, h, rx, ry: Integer);
    procedure drawText(x: Integer; y: Integer; s: PWideString); overload;
    procedure drawText(x,y,w,h,flags: Integer; s:PWideString); overload;
    procedure drawLine(x1: Integer; y1: Integer; x2: Integer; y2: Integer);
    procedure drawEllipse(x: Integer; y: Integer; w: Integer; h: Integer);
    procedure drawPixmap(p: PQtPoint; pm: QPixmapH; sr: PRect);
    procedure eraseRect(ARect: PRect);
    procedure fillRect(ARect: PRect; ABrush: QBrushH); overload;
    procedure fillRect(x, y, w, h: Integer; ABrush: QBrushH); overload;
    procedure fillRect(x, y, w, h: Integer); overload;

    procedure getBrushOrigin(retval: PPoint);
    function getClipping: Boolean;
    procedure getPenPos(retval: PPoint);
    function getWorldMatrix: QMatrixH;
    procedure setBrushOrigin(x, y: Integer);
    procedure setPenPos(x, y: Integer);

    function font: TQtFont;
    procedure setFont(f: TQtFont);
    function brush: TQtBrush;
    procedure setBrush(ABrush: TQtBrush);
    function BackgroundBrush: TQtBrush;
    function pen: TQtPen;
    function setPen(APen: TQtPen): TQtPen;
    function SetBkColor(Color: TcolorRef): TColorRef;
    function SetBkMode(BkMode: Integer): Integer;
    function getDeviceSize: TPoint;
    function getRegionType(ARegion: QRegionH): integer;
    function getClipRegion: TQtRegion;
    procedure setClipping(const AValue: Boolean);
    procedure setClipRegion(ARegion: QRegionH; AOperation: QtClipOperation = QtReplaceClip);
    procedure setRegion(ARegion: TQtRegion);
    procedure drawImage(targetRect: PRect; image: QImageH; sourceRect: PRect;
      mask: QImageH; maskRect: PRect; flags: QtImageConversionFlags = QtAutoColor);
    procedure rotate(a: Double);
    procedure save;
    procedure restore;
    procedure translate(dx: Double; dy: Double);
  end;
  
  { TQtPixmap }

  TQtPixmap = class(TObject)
  protected
    FHandle: QPixmapH;
  public
    constructor Create(p1: PSize); virtual;
    destructor Destroy; override;
  public
    property Handle: QPixmapH read FHandle;

    function getHeight: Integer;
    function getWidth: Integer;
    procedure grabWidget(AWidget: QWidgetH; x: Integer = 0; y: Integer = 0; w: Integer = -1; h: Integer = -1);
    procedure grabWindow(p1: Cardinal; x: Integer = 0; y: Integer = 0; w: Integer = -1; h: Integer = -1);
    procedure toImage(retval: QImageH);
    class procedure fromImage(retval: QPixmapH; image: QImageH; flags: QtImageConversionFlags = QtAutoColor);
  end;
  
  { TQtIcon }

  TQtIcon = class(TObject)
  protected
    FHandle: QIconH;
  public
    constructor Create;
    destructor Destroy; override;
  public
    property Handle: QIconH read FHandle;
  end;
  
  { TQtSystemTrayIcon }

  TQtSystemTrayIcon = class(TObject)
  private
  public
    Handle: QSystemTrayIconH;
    IconHandle: QIconH; // For external use
  public
    constructor Create(vIcon: QIconH); virtual;
    destructor Destroy; override;
  public
    class function TIconToQIconH(const AIcon: TIcon): QIconH; // Helper function for TTrayIcon
    procedure setContextMenu(menu: QMenuH);
    procedure setIcon(icon: QIconH);
    procedure setToolTip(tip: WideString);
    procedure show;
    procedure hide;
  end;
  
  { TQtButtonGroup }
  
  TQtButtonGroup = class(TObject)
  private
  public
    constructor Create(AParent: QObjectH); virtual;
    destructor Destroy; override;
    Handle: QButtonGroupH;
  public
    procedure AddButton(AButton: QAbstractButtonH); overload;
    procedure AddButton(AButton: QAbstractButtonH; Id: Integer); overload;
    function ButtonFromId(id: Integer): QAbstractButtonH;
    procedure RemoveButton(AButton: QAbstractButtonH);
    function GetExclusive: Boolean;
    procedure SetExclusive(AExclusive: Boolean);
    procedure SignalButtonClicked(AButton: QAbstractButtonH); cdecl;
  end;
  
  { TQtClipboard }
  
  TQtClipboard = class(TQtObject)
  private
    {$IFNDEF MSWINDOWS}
    FClipChanged: Boolean;
    FClipCounter: Integer;
    FClipDataChangedHook: QClipboard_hookH;
    {$ENDIF}
    FClipBoardFormats: TStringList;
    FOnClipBoardRequest: TClipboardRequestEvent;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure AttachEvents; override;
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl; override;

    function Clipboard: QClipboardH; inline;
    
    function getMimeData(AMode: QClipboardMode): QMimeDataH;
    procedure setMimeData(AMimeData: QMimeDataH; AMode: QClipboardMode);
    procedure Clear(AMode: QClipboardMode);
    
    function FormatToMimeType(AFormat: TClipboardFormat): String;
    function RegisterFormat(AMimeType: String): TClipboardFormat;
    function GetData(ClipboardType: TClipboardType; FormatID: TClipboardFormat;
      Stream: TStream): boolean;
    function GetFormats(ClipboardType: TClipboardType; var Count: integer;
      var List: PClipboardFormat): boolean;
    function GetOwnerShip(ClipboardType: TClipboardType; OnRequestProc: TClipboardRequestEvent;
      FormatCount: integer; Formats: PClipboardFormat): boolean;
      
    procedure signalDataChanged; cdecl;
  end;

  { TQtPrinter }
  
  TQtPrinter = class(TQtObject)
  protected
    FHandle: QPrinterH;
  private
    function getCollateCopies: Boolean;
    function getColorMode: QPrinterColorMode;
    function getCreator: WideString;
    function getDevType: Integer;
    function getDocName: WideString;
    function getDoubleSidedPrinting: Boolean;
    function getFontEmbedding: Boolean;
    function getFullPage: Boolean;
    function getOutputFormat: QPrinterOutputFormat;
    function getPaperSource: QPrinterPaperSource;
    function getPrintProgram: WideString;
    function getPrintRange: QPrinterPrintRange;
    procedure setCollateCopies(const AValue: Boolean);
    procedure setColorMode(const AValue: QPrinterColorMode);
    procedure setCreator(const AValue: WideString);
    procedure setDocName(const AValue: WideString);
    procedure setDoubleSidedPrinting(const AValue: Boolean);
    procedure setFontEmbedding(const AValue: Boolean);
    procedure setFullPage(const AValue: Boolean);
    procedure setOutputFormat(const AValue: QPrinterOutputFormat);
    procedure setPaperSource(const AValue: QPrinterPaperSource);
    procedure setPrinterName(const AValue: WideString);
    function getPrinterName: WideString;
    procedure setOutputFileName(const AValue: WideString);
    function getOutputFileName: WideString;
    procedure setOrientation(const AValue: QPrinterOrientation);
    function getOrientation: QPrinterOrientation;
    procedure setPageSize(const AValue: QPrinterPageSize);
    function getPageSize: QPrinterPageSize;
    procedure setPageOrder(const AValue: QPrinterPageOrder);
    function getPageOrder: QPrinterPageOrder;
    procedure setPrintProgram(const AValue: WideString);
    procedure setPrintRange(const AValue: QPrinterPrintRange);
    procedure setResolution(const AValue: Integer);
    function getResolution: Integer;
    function getNumCopies: Integer;
    procedure setNumCopies(const AValue: Integer);
    function getPrinterState: QPrinterPrinterState;
  public
    constructor Create; override;
    destructor Destroy; override;
    function NewPage: Boolean;
    function Abort: Boolean;
    procedure setFromPageToPage(Const AFromPage, AToPage: Integer);
    function fromPage: Integer;
    function toPage: Integer;
    function PaintEngine: QPaintEngineH;
    function PageRect: TRect;
    function PaperRect: TRect;
    function PrintEngine: QPrintEngineH;
    
    property Collate: Boolean read getCollateCopies write setCollateCopies;
    property ColorMode: QPrinterColorMode read getColorMode write setColorMode;
    property Creator: WideString read getCreator write setCreator;
    property DocName: WideString read getDocName write setDocName;
    property DoubleSidedPrinting: Boolean read getDoubleSidedPrinting write setDoubleSidedPrinting;
    property DeviceType: Integer read getDevType;
    property FontEmbedding: Boolean read getFontEmbedding write setFontEmbedding;
    property FullPage: Boolean read getFullPage write setFullPage;
    property Handle: QPrinterH read FHandle;
    property NumCopies: Integer read getNumCopies write setNumCopies;
    property Orientation: QPrinterOrientation read getOrientation write setOrientation;
    property OutputFormat: QPrinterOutputFormat read getOutputFormat write setOutputFormat;
    property OutputFileName: WideString read getOutputFileName write setOutputFileName;
    property PageOrder: QPrinterPageOrder read getPageOrder write setPageOrder;
    property PageSize: QPrinterPageSize read getPageSize write setPageSize;
    property PaperSource: QPrinterPaperSource read getPaperSource write setPaperSource;
    property PrinterName: WideString read getPrinterName write setPrinterName;
    property PrintRange: QPrinterPrintRange read getPrintRange write setPrintRange;
    property PrinterState: QPrinterPrinterState read getPrinterState;
    property PrintProgram: WideString read getPrintProgram write setPrintProgram;
    property Resolution: Integer read getResolution write setResolution;
  end;
  
  { TQtTimer }

  TQtTimer = class(TQtObject)
  private
    FCallbackFunc: TFNTimerProc;
    FId: Integer;
    FAppObject: QObjectH;
  public
    constructor CreateTimer(Interval: integer; const TimerFunc: TFNTimerProc; App: QObjectH); virtual;
    destructor Destroy; override;
  public
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl; override;
  end;
  
  { TQtStringList }

  TQtStringList = class(TStrings)
  private
    FHandle: QStringListH;
    FOwnHandle: Boolean;
  protected
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
  public
    constructor Create;
    constructor Create(Source: QStringListH);
    destructor Destroy; override;

    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
    property Handle: QStringListH read FHandle;
  end;

const
  LCLQt_Destroy = QEventType(Ord(QEventUser) + $1000);
  
procedure TQColorToColorRef(const AColor: TQColor; out AColorRef: TColorRef);
procedure ColorRefToTQColor(const AColorRef: TColorRef; var AColor:TQColor);
procedure DebugRegion(const msg: string; Rgn: QRegionH);

function CheckGDIObject(const AGDIObject: HGDIOBJ; const AMethodName: String; AParamName: String = ''): Boolean;
function CheckBitmap(const ABitmap: HBITMAP; const AMethodName: String; AParamName: String = ''): Boolean;
//function CheckCursor(const ACursor: HCURSOR; const AMethodName: String; AParamName: String = ''): Boolean;

function QtDefaultPrinter: TQtPrinter;
function Clipboard: TQtClipboard;
function QtDefaultContext: TQtDeviceContext;
function QtScreenContext: TQtDeviceContext;

implementation

uses
  qtproc;
  
const
  ClipbBoardTypeToQtClipboard: array[TClipboardType] of QClipboardMode =
  (
{ctPrimarySelection  } QClipboardSelection,
{ctSecondarySelection} QClipboardSelection,
{ctClipboard         } QClipboardClipboard
  );

const
  SQTWSPrefix = 'TQTWidgetSet.';

var
  FClipboard: TQtClipboard = nil;
  FDefaultContext: TQtDeviceContext = nil;
  FScreenContext: TQtDeviceContext = nil;
  FPrinter: TQtPrinter = nil;

{------------------------------------------------------------------------------
  Name:    CheckGDIObject
  Params:  GDIObject   - Handle to a GDI Object (TQTFont, ...)
           AMethodName - Method name
           AParamName  - Param name
  Returns: If the GDIObject is valid

  Remark: All handles for GDI objects must be pascal objects so we can
 distinguish between them
 ------------------------------------------------------------------------------}
function CheckGDIObject(const AGDIObject: HGDIOBJ; const AMethodName: String;
  AParamName: String): Boolean;
begin
  {$note TODO: make TQTImage a TQtResource}
  Result := (TObject(AGDIObject) is TQtResource) or (TObject(AGDIObject) is TQtImage);
  if Result then Exit;

  if Pos('.', AMethodName) = 0 then
    DebugLn(SQTWSPrefix + AMethodName + ' Error - invalid GDIObject ' +
      AParamName + ' = ' + DbgS(AGDIObject) + '!')
  else
    DebugLn(AMethodName + ' Error - invalid GDIObject ' + AParamName + ' = ' +
      DbgS(AGDIObject) + '!');
end;

{------------------------------------------------------------------------------
  Name:    CheckBitmap
  Params:  Bitmap      - Handle to a bitmap (TQTBitmap)
           AMethodName - Method name
           AParamName  - Param name
  Returns: If the bitmap is valid
 ------------------------------------------------------------------------------}
function CheckBitmap(const ABitmap: HBITMAP; const AMethodName: String;
  AParamName: String): Boolean;
begin
  Result := TObject(ABitmap) is TQTImage;
  if Result then Exit;

  if Pos('.', AMethodName) = 0 then
    DebugLn(SQTWSPrefix + AMethodName + ' Error - invalid bitmap ' +
      AParamName + ' = ' + DbgS(ABitmap) + '!')
  else
    DebugLn(AMethodName + ' Error - invalid bitmap ' + AParamName + ' = ' +
      DbgS(ABitmap) + '!');
end;

{------------------------------------------------------------------------------
  Name:    CheckCursor
  Params:  Cursor      - Handle to a cursor (TCarbonCursor)
           AMethodName - Method name
           AParamName  - Param name
  Returns: If the cursor is valid
 ------------------------------------------------------------------------------}
(*
function CheckCursor(const ACursor: HCURSOR; const AMethodName: String;
  AParamName: String): Boolean;
begin
  Result := TObject(ACursor) is TQTCursor;
  if Result then Exit;
  
  if Pos('.', AMethodName) = 0 then
    DebugLn(SQTWSPrefix + AMethodName + ' Error - invalid cursor ' +
      AParamName + ' = ' + DbgS(Cursor) + '!')
  else
    DebugLn(AMethodName + ' Error - invalid cursor ' + AParamName + ' = ' +
      DbgS(ACursor) + '!');
end;
*)

  

function QtDefaultContext: TQtDeviceContext;
begin
  if FDefaultContext = nil then
    FDefaultContext := TQtDeviceContext.Create(nil);
  Result := FDefaultContext;
end;

function QtScreenContext: TQtDeviceContext;
begin
  if FScreenContext = nil then
    FScreenContext := TQtDeviceContext.Create(QApplication_desktop(), False);
  Result := FScreenContext;
end;
  
{ TQtObject }

constructor TQtObject.Create;
begin
  FEventHook := nil;
  FUpdateCount := 0;
  FInEventCount := 0;
  FReleaseInEvent := False;
end;

destructor TQtObject.Destroy;
begin
  if TheObject <> nil then
  begin
    DetachEvents;
    QObject_deleteLater(TheObject);
    TheObject := nil;
  end;
  inherited Destroy;
end;

procedure TQtObject.Release;
begin
  if InEvent then
    FReleaseInEvent := True
  else
    Free;
end;

procedure TQtObject.AttachEvents;
var
  Method: TMethod;
begin
  FEventHook := QObject_hook_create(TheObject);
  TEventFilterMethod(Method) := @EventFilter;
  QObject_hook_hook_events(FEventHook, Method);
end;

procedure TQtObject.DetachEvents;
begin
  if FEventHook <> nil then
  begin
    QObject_hook_destroy(FEventHook);
    FEventHook := nil;
  end;
end;

procedure TQtObject.BeginEventProcessing;
begin
  inc(FInEventCount);
end;

procedure TQtObject.EndEventProcessing;
begin
  if FInEventCount > 0 then
    dec(FInEventCount);
  if (FInEventCount = 0) and FReleaseInEvent then
    Free;
end;

function TQtObject.InEvent: Boolean;
begin
  Result := FInEventCount > 0;
end;

procedure TQtObject.BeginUpdate;
begin
  inc(FUpdateCount);
end;

procedure TQtObject.EndUpdate;
begin
  if FUpdateCount > 0 then
    dec(FUpdateCount);
end;

function TQtObject.InUpdate: Boolean;
begin
  Result := FUpdateCount > 0;
end;

{ TQtAction }

{------------------------------------------------------------------------------
  Method: TQtAction.Create

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TQtAction.Create(const AHandle: QActionH);
begin
  Handle := AHandle;
  FIcon := nil;
end;

{------------------------------------------------------------------------------
  Method: TQtAction.Destroy

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TQtAction.Destroy;
begin
  if FIcon <> nil then
    QIcon_destroy(FIcon);

  if Handle <> nil then
    QAction_destroy(Handle);

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TQtAction.SlotTriggered

  Callback for menu item click
 ------------------------------------------------------------------------------}
procedure TQtAction.SlotTriggered(checked: Boolean); cdecl;
begin
  if Assigned(MenuItem) and Assigned(MenuItem.OnClick) then
   MenuItem.OnClick(Self.MenuItem);
end;

{------------------------------------------------------------------------------
  Method: TQtAction.setChecked

  Checks or unchecks a menu entry
  
  To mimic the behavior LCL should have we added code to handle
 setCheckable automatically
 ------------------------------------------------------------------------------}
procedure TQtAction.setChecked(p1: Boolean);
begin
  if p1 then setCheckable(True)
  else setCheckable(False);

  QAction_setChecked(Handle, p1);
end;

{------------------------------------------------------------------------------
  Method: TQtAction.setCheckable

  Set's if a menu can be checked. Is false by default
 ------------------------------------------------------------------------------}
procedure TQtAction.setCheckable(p1: Boolean);
begin
  QAction_setCheckable(Handle, p1);
end;

{------------------------------------------------------------------------------
  Method: TQtAction.setEnabled
 ------------------------------------------------------------------------------}
procedure TQtAction.setEnabled(p1: Boolean);
begin
  QAction_setEnabled(Handle, p1);
end;

procedure TQtAction.setIcon(const AIcon: QIconH);
begin
  QAction_setIcon(Handle, AIcon);
end;

procedure TQtAction.setImage(const AImage: TQtImage);
begin
  if FIcon <> nil then
  begin
    QIcon_destroy(FIcon);
    FIcon := nil;
  end;
  
  if AImage <> nil then
    FIcon := AImage.AsIcon()
  else
    FIcon := QIcon_create();

  setIcon(FIcon);
end;

{------------------------------------------------------------------------------
  Method: TQtAction.setVisible
 ------------------------------------------------------------------------------}
procedure TQtAction.setVisible(p1: Boolean);
begin
  QAction_setVisible(Handle, p1);
end;

{ TQtImage }

constructor TQtImage.Create;
begin
  Handle := QImage_create();
  FData := nil;
  FDataOwner := False;
end;

{------------------------------------------------------------------------------
  Method: TQtImage.Create

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TQtImage.Create(vHandle: QImageH);
begin
  Handle := vHandle;
  FData := nil;
  FDataOwner := False;
end;

{------------------------------------------------------------------------------
  Method: TQtImage.Create

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TQtImage.Create(AData: PByte; width: Integer; height: Integer;
  format: QImageFormat; const ADataOwner: Boolean = False);
begin
  FData := AData;
  FDataOwner := ADataOwner;

  if FData = nil then
    Handle := QImage_create(width, height, format)
  else
    Handle := QImage_create(FData, width, height, format);
end;

constructor TQtImage.Create(AData: PByte; width: Integer; height: Integer;
  bytesPerLine: Integer; format: QImageFormat; const ADataOwner: Boolean);
begin
  FData := AData;
  FDataOwner := ADataOwner;

  if FData = nil then
    Handle := QImage_create(width, height, format)
  else
    Handle := QImage_create(FData, width, height, bytesPerLine, format);
end;

{------------------------------------------------------------------------------
  Method: TQtImage.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TQtImage.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtImage.Destroy Handle:', dbgs(Handle));
  {$endif}

  if Handle <> nil then
    QImage_destroy(Handle);
  if (FDataOwner) and (FData <> nil) then
    FreeMem(FData);

  inherited Destroy;
end;

function TQtImage.AsIcon(AMode: QIconMode = QIconNormal; AState: QIconState = QIconOff): QIconH;
var
  APixmap: QPixmapH;
begin
  APixmap := AsPixmap;
  Result := QIcon_create();
  if Result <> nil then
    QIcon_addPixmap(Result, APixmap, AMode, AState);
  QPixmap_destroy(APixmap);
end;

function TQtImage.AsPixmap(flags: QtImageConversionFlags = QtAutoColor): QPixmapH;
begin
  Result := QPixmap_create();
  QPixmap_fromImage(Result, Handle, flags);
end;

function TQtImage.AsBitmap(flags: QtImageConversionFlags = QtAutoColor): QBitmapH;
begin
  Result := QBitmap_create();
  QBitmap_fromImage(Result, Handle, flags);
end;

procedure TQtImage.CopyFrom(AImage: QImageH; x, y, w, h: integer);
begin
  QImage_copy(AImage, Handle, x, y, w, h);
end;

{------------------------------------------------------------------------------
  Method: TQtImage.height
  Params:  None
  Returns: The height of the image
 ------------------------------------------------------------------------------}
function TQtImage.height: Integer;
begin
  Result := QImage_height(Handle);
end;

{------------------------------------------------------------------------------
  Method: TQtImage.width
  Params:  None
  Returns: The width of the image
 ------------------------------------------------------------------------------}
function TQtImage.width: Integer;
begin
  Result := QImage_width(Handle);
end;

{------------------------------------------------------------------------------
  Method: TQtImage.bits
  Params:  None
  Returns: The internal array of bits of the image
 ------------------------------------------------------------------------------}
function TQtImage.bits: PByte;
begin
  Result := QImage_bits(Handle);
end;

{------------------------------------------------------------------------------
  Method: TQtImage.numBytes
  Params:  None
  Returns: The number of bytes the image occupies in memory
 ------------------------------------------------------------------------------}
function TQtImage.numBytes: Integer;
begin
  Result := QImage_numBytes(Handle);
end;

procedure TQtImage.invertPixels(InvertMode: QImageInvertMode = QImageInvertRgb);
begin
  QImage_invertPixels(Handle, InvertMode);
end;

{ TQtFont }

function TQtFont.GetMetrics: TQtFontMetrics;
begin
  if FMetrics = nil then
    FMetrics := TQtFontMetrics.Create(Widget);
  Result := FMetrics;
end;

{------------------------------------------------------------------------------
  Function: TQtFont.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtFont.Create(CreateHandle: Boolean; Const AShared: Boolean = False);
begin
  {$ifdef VerboseQt}
    WriteLn('TQtFont.Create CreateHandle: ', dbgs(CreateHandle));
  {$endif}

  if CreateHandle then
    Widget := QFont_create;
  
  FShared := AShared;
  FMetrics := nil;
end;

{------------------------------------------------------------------------------
  Function: TQtFont.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtFont.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtFont.Destroy');
  {$endif}

  FMetrics.Free;
  if not FShared and (Widget <> nil) then
    QFont_destroy(Widget);

  inherited Destroy;
end;

function TQtFont.getPointSize: Integer;
begin
  Result := QFont_pointSize(Widget);
end;

procedure TQtFont.setPointSize(p1: Integer);
begin
  if p1 > 0 then
    QFont_setPointSize(Widget, p1);
end;

function TQtFont.getPixelSize: Integer;
begin
  Result := QFont_pixelSize(Widget);
end;

procedure TQtFont.setPixelSize(p1: Integer);
begin
  if p1 > 0 then
    QFont_setPixelSize(Widget, p1);
end;

function TQtFont.getWeight: Integer;
begin
  Result := QFont_weight(Widget);
end;

function TQtFont.getItalic: Boolean;
begin
  Result := QFont_italic(Widget);
end;

function TQtFont.getBold: Boolean;
begin
  Result := QFont_bold(Widget);
end;

function TQtFont.getUnderline: Boolean;
begin
  Result := QFont_underline(Widget);
end;

function TQtFont.getStrikeOut: Boolean;
begin
  Result := QFont_strikeOut(Widget);
end;

function TQtFont.getFamily: WideString;
begin
  QFont_family(Widget, @Result);
end;

procedure TQtFont.setWeight(p1: Integer);
begin
  QFont_setWeight(Widget, p1);
end;

procedure TQtFont.setBold(p1: Boolean);
begin
  QFont_setBold(Widget, p1);
end;

procedure TQtFont.setItalic(b: Boolean);
begin
  QFont_setItalic(Widget, b);
end;

procedure TQtFont.setUnderline(p1: Boolean);
begin
  QFont_setUnderline(Widget, p1);
end;

procedure TQtFont.setStrikeOut(p1: Boolean);
begin
  QFont_setStrikeOut(Widget, p1);
end;

procedure TQtFont.setRawName(p1: string);
var
  Str: WideString;
begin
  Str := GetUtf8String(p1);

  QFont_setRawName(Widget, @Str);
end;

procedure TQtFont.setFamily(p1: string);
var
  Str: WideString;
begin
  Str := GetUtf8String(p1);

  QFont_setFamily(Widget, @Str);
end;

procedure TQtFont.family(retval: PWideString);
begin
  QFont_family(Widget, retval);
end;

function TQtFont.fixedPitch: Boolean;
begin
  Result := QFont_fixedPitch(Widget);
end;

{ TQtFontMetrics }

constructor TQtFontMetrics.Create(Parent: QFontH);
begin
  Widget := QFontMetrics_create(Parent);
end;

destructor TQtFontMetrics.Destroy;
begin
  QFontMetrics_destroy(Widget);

  inherited Destroy;
end;

function TQtFontMetrics.height: Integer;
begin
  Result := QFontMetrics_height(Widget);
end;

function TQtFontMetrics.width(p1: PWideString): Integer;
begin
  Result := QFontMetrics_width(Widget, p1);
end;

function TQtFontMetrics.ascent: Integer;
begin
  Result := QFontMetrics_ascent(Widget);
end;

function TQtFontMetrics.descent: Integer;
begin
  Result := QFontMetrics_descent(Widget);
end;

function TQtFontMetrics.leading: Integer;
begin
  Result := QFontMetrics_leading(Widget);
end;

function TQtFontMetrics.maxWidth: Integer;
begin
  Result := QFontMetrics_maxWidth(Widget);
end;

procedure TQtFontMetrics.boundingRect(retval: PRect; r: PRect; flags: Integer; text: PWideString; tabstops: Integer = 0; tabarray: PInteger = nil);
begin
  QFontMetrics_boundingRect(Widget, retval, r, flags, text, tabstops, tabarray);
end;

function TQtFontMetrics.charWidth(str: WideString; pos: Integer): Integer;
begin
  Result := QFontMetrics_charWidth(Widget, @str, pos);
end;

{ TQtBrush }

{------------------------------------------------------------------------------
  Function: TQtBrush.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtBrush.Create(CreateHandle: Boolean; Const AShared: Boolean = False);
begin
  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtBrush.Create CreateHandle: ', dbgs(CreateHandle));
  {$endif}

  if CreateHandle then Widget := QBrush_create;
  
  FShared := AShared;
end;

{------------------------------------------------------------------------------
  Function: TQtBrush.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtBrush.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtBrush.Destroy');
  {$endif}

  if not FShared and (Widget <> nil) then
    QBrush_destroy(Widget);

  inherited Destroy;
end;

function TQtBrush.getColor: PQColor;
begin
  Result := QBrush_Color(Widget);
end;

procedure TQtBrush.setColor(AColor: PQColor);
begin
  QBrush_setColor(Widget, AColor);
end;

{------------------------------------------------------------------------------
  Function: TQtBrush.setStyle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtBrush.setStyle(style: QtBrushStyle);
begin
  QBrush_setStyle(Widget, style);
end;

procedure TQtBrush.setTexture(pixmap: QPixmapH);
begin
  QBrush_setTexture(Widget, pixmap);
end;

procedure TQtBrush.setTextureImage(image: QImageH);
var
  TempImage: QImageH;
begin
  // workaround thurther deletion of original image
  // When image is deleted its data will be deleted too
  // If image has been created with predefined data then it will be owner of it
  // => it will Free owned data => brush will be invalid
  // as workaround we are copying an original image so qt create new image with own data
  TempImage := QImage_create();
  QImage_copy(image, TempImage, 0, 0, QImage_width(image), QImage_height(image));
  QBrush_setTextureImage(Widget, TempImage);
  QImage_destroy(TempImage);
end;

{ TQtPen }

{------------------------------------------------------------------------------
  Function: TQtPen.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtPen.Create(CreateHandle: Boolean; Const AShared: Boolean = False);
begin
  {$ifdef VerboseQt}
    WriteLn('TQtPen.Create CreateHandle: ', dbgs(CreateHandle));
  {$endif}
  
  if CreateHandle then
    Widget := QPen_create;
  FShared := AShared;
end;

{------------------------------------------------------------------------------
  Function: TQtPen.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtPen.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtPen.Destroy');
  {$endif}

  if not FShared and (Widget <> nil) then
    QPen_destroy(Widget);

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtPen.setBrush
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}

procedure TQtPen.setBrush(brush: QBrushH);
begin
  QPen_setBrush(Widget, brush);
end;

{------------------------------------------------------------------------------
  Function: TQtPen.setStyle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtPen.setStyle(AStyle: QtPenStyle);
begin
  QPen_setStyle(Widget, AStyle);
end;

{------------------------------------------------------------------------------
  Function: TQtPen.setWidth
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtPen.setWidth(p1: Integer);
begin
  QPen_setWidth(Widget, p1);
end;


{------------------------------------------------------------------------------
  Function: TQtPen.Style
  Params:  None
  Returns: QPenStyle
 ------------------------------------------------------------------------------}
function TQtPen.Style: QtPenStyle;
begin
  Result := QPen_Style(Widget);
end;

function TQtPen.getColor: TQColor;
begin
  QPen_color(Widget, @Result);
end;


{------------------------------------------------------------------------------
  Function: TQtPen.Width
  Params:  None
  Returns: integer , width of current pen

 ------------------------------------------------------------------------------}
function TQtPen.Width: Integer;
begin
  Result := QPen_Width(Widget);
end;

{------------------------------------------------------------------------------
  Function: TQtPen.setColor
  Params:  p1: TQColor
  Returns: Nothing
  Setting pen color. 
 ------------------------------------------------------------------------------}
procedure TQtPen.setColor(p1: TQColor);
begin
  QPen_setColor(Widget, @p1);
end;


{ TQtRegion }

{------------------------------------------------------------------------------
  Function: TQtRegion.Create
  Params:  CreateHandle: Boolean
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtRegion.Create(CreateHandle: Boolean);
begin
  {$ifdef VerboseQt}
    WriteLn('TQtRegion.Create CreateHandle: ', dbgs(CreateHandle));
  {$endif}

  // Creates the widget
  if CreateHandle then Widget := QRegion_create;
end;

{------------------------------------------------------------------------------
  Function: TQtRegion.Create (CreateRectRgn)
  Params:  CreateHandle: Boolean; X1,Y1,X2,Y2:Integer
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtRegion.Create(CreateHandle: Boolean; X1,Y1,X2,Y2:Integer;
  Const RegionType: QRegionRegionType = QRegionRectangle);
begin
  {$ifdef VerboseQt}
    WriteLn('TQtRegion.Create CreateHandle: ', dbgs(CreateHandle));
  {$endif}

  // Creates the widget
  if CreateHandle then Widget := QRegion_create(X1,Y1,X2,Y2, RegionType);
end;

constructor TQtRegion.Create(CreateHandle: Boolean; Poly: QPolygonH;
  Const Fill: QtFillRule = QtWindingFill);
begin
  {$ifdef VerboseQt}
    WriteLn('TQtRegion.Create polyrgn CreateHandle: ', dbgs(CreateHandle));
  {$endif}
  if CreateHandle then Widget := QRegion_create(Poly, Fill);
end;


{------------------------------------------------------------------------------
  Function: TQtRegion.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtRegion.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtRegion.Destroy');
  {$endif}

  QRegion_destroy(Widget);

  inherited Destroy;
end;

function TQtRegion.GetRegionType: integer;
var
  R: TRect;
begin
  try
    if QRegion_isEmpty(Widget) then
      Result := NULLREGION
    else
    begin
      QRegion_boundingRect(Widget, @R);
      if QRegion_contains(Widget, PRect(@R)) then
        Result := SIMPLEREGION
      else
        Result := COMPLEXREGION;
    end;
  except
    Result := ERROR;
  end;
end;

function TQtRegion.getBoundingRect: TRect;
begin
  QRegion_boundingRect(Widget, @Result);
end;

{ TQtDeviceContext }

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtDeviceContext.Create(AWidget: QWidgetH; const APaintEvent: Boolean = False);
var
  W: Integer;
  H: Integer;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtDeviceContext.Create (',
     ' WidgetHandle: ', dbghex(PtrInt(AWidget)),
     ' FromPaintEvent:',BoolToStr(APaintEvent),' )');
  {$endif}

  {NOTE FOR QT DEVELOPERS: Whenever you call TQtDeviceContext.Create() outside
   of TQtWidgetSet.BeginPaint() SET APaintEvent TO FALSE !}
  if AWidget = nil then
  begin
    Parent := nil;
    ParentPixmap := QPixmap_Create(10, 10);
    Widget := QPainter_Create(QPaintDeviceH(ParentPixmap));
  end
  else
  begin
    Parent := AWidget;
    if not APaintEvent then
    begin
      {avoid paints on null pixmaps !}
      W := QWidget_width(Parent);
      H := QWidget_height(Parent);
      
      if W <= 0 then W := 1;
      if H <= 0 then H := 1;
      
      ParentPixmap := QPixmap_Create(W, H);
      Widget := QPainter_create(QPaintDeviceH(ParentPixmap));
    end
    else
    begin
      Widget := QPainter_create(QWidget_to_QPaintDevice(Parent));
    end;
  end;
  FOwnPainter := True;
  CreateObjects;
  FPenPos.X := 0;
  FPenPos.Y := 0;
end;

constructor TQtDeviceContext.CreateFromPainter(APainter: QPainterH);
begin
  Widget := APainter;
  Parent := nil;
  FOwnPainter := False;
  CreateObjects;
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtDeviceContext.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtDeviceContext.Destroy');
  {$endif}

  if (vClipRect <> nil) then
    dispose(vClipRect);

  if Parent <> nil then
  begin  
    vFont.Widget := nil;
    vFont.Free;
    vBrush.Widget := nil;
    vBrush.Free;
    vPen.Widget := nil;
    vPen.Free;
    vRegion.Widget := nil;
    vRegion.Free;
    vBackgroundBrush.Widget := nil;
    vBackgroundBrush.Free;
  end;
  
  if (Widget <> nil) and FOwnPainter then
    QPainter_destroy(Widget);

  if ParentPixmap <> nil then
    QPixmap_destroy(ParentPixmap);

  inherited Destroy;
end;

procedure TQtDeviceContext.CreateObjects;
begin
  if Parent <> nil then
  begin
    vFont := TQtFont.Create(False);
    vFont.Owner := Self;

    vBrush := TQtBrush.Create(False);
    vBrush.Owner := Self;

    vPen := TQtPen.Create(False);
    vPen.Owner := Self;

    vRegion := TQtRegion.Create(False);
    vRegion.Owner := Self;

    vBackgroundBrush := TQtBrush.Create(False);
    vBackgroundBrush.Owner := Self;
  end else
  begin
    vBrush := TQtBrush(GetStockObject(WHITE_BRUSH));
		vBackgroundBrush := vBrush;
    vPen := TQtPen(GetStockObject(BLACK_PEN));
  end;

  vTextColor := ColorToRGB(clWindowText);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.DebugClipRect
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.DebugClipRect(const msg: string);
var
  Rgn: QRegionH;
  ok: boolean;
begin
  ok := getClipping;
  Write(Msg, 'DC: HasClipping=', ok);
  
  if Ok then
  begin
    Rgn := QRegion_Create;
    QPainter_ClipRegion(Widget, Rgn);
    DebugRegion('', Rgn);
    QRegion_Destroy(Rgn);
  end
  else
    WriteLn;
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.setImage
  Params:  None
  Returns: Nothing
  
  This function will destroy the previous DC handle and generate
 a new one based on an image. This is necessary because when painting
 is being done to a TBitmap, LCL will create a completely abstract DC,
 using GetDC(0), and latter use SelectObject to associate that DC
 with the Image.
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.setImage(AImage: TQtImage);
begin
  {$ifdef VerboseQt}
  writeln('TQtDeviceContext.setImage() ');
  {$endif}
  vImage := AImage.Handle;
  
  QPainter_destroy(Widget);

  Widget := QPainter_Create(vImage);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.CorrectCoordinates
  Params:  None
  Returns: Nothing
  
  If you draw an image with negative coordinates
 (for example x: -50 y: -50 w: 100 h: 100), the result is not well
 defined in Qt, and could well be: (x: 0 y: 0 w: 100 h: 100)
  This method corrects the coordinates, cutting the result, so we draw:
 (x: 0 y: 0 w: 50 h: 50)
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.CorrectCoordinates(var ARect: TRect);
begin
  if ARect.Left < 0 then ARect.Left := 0;

  if ARect.Top < 0 then ARect.Top := 0;

{  if ARect.Right > MaxRight then ARect.Right := MaxRight;

  if ARect.Bottom > MaxBottom then ARect.Bottom := MaxBottom;}
end;

procedure TQtDeviceContext.qDrawPlainRect(x, y, w, h: integer; AColor: PQColor = nil;
  lineWidth: Integer = 1; FillBrush: QBrushH = nil);
begin
  if AColor = nil then
    AColor := BackgroundBrush.getColor;
  q_DrawPlainRect(Widget, x, y, w, h, AColor, lineWidth, FillBrush);
end;

procedure TQtDeviceContext.qDrawShadeRect(x, y, w, h: integer; Palette: QPaletteH = nil; Sunken: Boolean = False;
  lineWidth: Integer = 1; midLineWidth: Integer = 0; FillBrush: QBrushH = nil);
begin
  if Palette = nil then
    Palette := QWidget_palette(Parent);
  q_DrawShadeRect(Widget, x, y, w, h, Palette, Sunken, lineWidth, midLineWidth, FillBrush);
end;

procedure TQtDeviceContext.qDrawWinPanel(x, y, w, h: integer;
  Palette: QPaletteH; Sunken: Boolean; lineWidth: Integer; FillBrush: QBrushH);
var
  i: integer;
begin
  if Palette = nil then
    Palette := QWidget_palette(Parent);

  // since q_DrawWinPanel doesnot supports lineWidth we should do it ourself
  for i := 1 to lineWidth - 1 do
  begin
    q_DrawWinPanel(Widget, x, y, w, h, Palette, Sunken);
    inc(x);
    inc(y);
    dec(w, 2);
    dec(h, 2);
  end;
  q_DrawWinPanel(Widget, x, y, w, h, Palette, Sunken, FillBrush);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.CreateDCData
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtDeviceContext.CreateDCData: PQtDCDATA;
begin
  {$ifdef VerboseQt}
  writeln('TQtDeviceContext.CreateDCData() ');
  {$endif}
  Qpainter_save(Widget);
  Result := nil; // doesn't matter;
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.RestoreDCData
  Params:  DCData, dummy in current implementation
  Returns: true if QPainter state was successfuly restored
 ------------------------------------------------------------------------------}
function TQtDeviceContext.RestoreDCData(var DCData: PQtDCData):boolean;
begin
  {$ifdef VerboseQt}
  writeln('TQtDeviceContext.RestoreDCData() ');
  {$endif}
  QPainter_restore(Widget);
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.RestorePenColor
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.RestorePenColor;
begin
  {$ifdef VerboseQt}
  writeln('TQtDeviceContext.RestorePenColor() ');
  {$endif}
  Qpainter_setPen(Widget, @PenColor);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.RestoreTextColor
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.RestoreTextColor;
var
  CurPen: QPenH;
  TxtColor: TQColor;
begin
  {$ifdef VerboseQt}
  writeln('TQtDeviceContext.RestoreTextColor() ');
  {$endif}
  CurPen := QPainter_Pen(Widget);
  QPen_color(CurPen, @PenColor);
  TxtColor := PenColor;
  ColorRefToTQColor(vTextColor, TxtColor);
  Qpainter_setPen(Widget, @txtColor);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.drawRect
  Params:  None
  Returns: Nothing

  Draws a rectangle. Helper function of winapi.Rectangle
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.drawRect(x1: Integer; y1: Integer; w: Integer; h: Integer);
begin
  {$ifdef VerboseQt}
  writeln('TQtDeviceContext.drawRect() x1: ',x1,' y1: ',y1,' w: ',w,' h: ',h);
  {$endif}
  QPainter_drawRect(Widget, x1, y1, w, h);
end;

procedure TQtDeviceContext.drawRoundRect(x, y, w, h, rx, ry: Integer);
begin
  QPainter_drawRoundRect(Widget, x, y, w, h, rx, ry);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.drawText
  Params:  None
  Returns: Nothing

  Draws a Text. Helper function of winapi.TextOut
  
  Qt does not draw the text starting at Y position and downwards, like LCL.

  Instead, Y becomes the baseline for the text and it´s drawn upwards.
  
  To get a correct behavior we need to sum the text´s height to the Y coordinate.
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.drawText(x: Integer; y: Integer; s: PWideString);
var
  AFont: TQtFont;
  ARect: TRect;
  dy: Integer;
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.drawText TargetX: ', X, ' TargetY: ', Y);
  {$endif}

  AFont := Font;
  with AFont do
    if Angle <> 0 then
    begin
      Translate(x, y);
      Rotate(-0.1 * Angle);
      Translate(-x, -y);
    end;

  // what about AFont.Metrics.descent and AFont.Metrics.leading ?
  y := y + AFont.Metrics.ascent;

  // manual check for clipping
  if getClipping then
  begin
    dy := AFont.Metrics.height;
    ARect := getClipRegion.getBoundingRect;
    if (y + dy < ARect.Top) or (y > ARect.Bottom) or
       (x > ARect.Right) then
      Exit;
  end;

  RestoreTextColor;
  
  QPainter_drawText(Widget, x, y, s);
  
  RestorePenColor;
  
  {$ifdef VerboseQt}
  WriteLn(' Font metrics height: ', AFont.Metrics.height, ' Angle: ',
    Round(0.1 * AFont.Angle));
  {$endif}
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.DrawText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.DrawText(x, y, w, h, flags: Integer; s: PWideString);
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.drawText x: ', X, ' Y: ', Y,' w: ',w,' h: ',h);
  {$endif}

  with Font do
    if Angle <> 0 then
    begin
      Translate(x, y);
      Rotate(-0.1 * Angle);
      Translate(-x, -y);
      // todo: something wrong with coordinates happen after that
    end;
  RestoreTextColor;
  QPainter_DrawText(Widget, x, y, w, h, Flags, s);
  RestorePenColor;
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.drawLine
  Params:  None
  Returns: Nothing

  Draws a Text. Helper function for winapi.LineTo
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.drawLine(x1: Integer; y1: Integer; x2: Integer; y2: Integer);
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.drawLine x1: ', X1, ' Y1: ', Y1,' x2: ',x2,' y2: ',y2);
  {$endif}
  QPainter_drawLine(Widget, x1, y1, x2, y2);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.drawEllipse
  Params:  None
  Returns: Nothing

  Draws a ellipse. Helper function for winapi.Ellipse
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.drawEllipse(x: Integer; y: Integer; w: Integer; h: Integer);
begin
  QPainter_drawEllipse(Widget, x, y, w, h);
end;

procedure TQtDeviceContext.drawPixmap(p: PQtPoint; pm: QPixmapH; sr: PRect);
begin
  QPainter_drawPixmap(Widget, p, pm, sr);
end;

procedure TQtDeviceContext.eraseRect(ARect: PRect);
begin
  QPainter_eraseRect(Widget, ARect);
end;

procedure TQtDeviceContext.fillRect(ARect: PRect; ABrush: QBrushH);
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.fillRect() from PRect');
  {$endif}
  QPainter_fillRect(Widget, ARect, ABrush);
end;

procedure TQtDeviceContext.fillRect(x, y, w, h: Integer; ABrush: QBrushH);
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.fillRect() x: ',x,' y: ',y,' w: ',w,' h: ',h);
  {$endif}
  QPainter_fillRect(Widget, x, y, w, h, ABrush);
end;

procedure TQtDeviceContext.fillRect(x, y, w, h: Integer);
begin
  fillRect(x, y, w, h, BackgroundBrush.Widget);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.drawPoint
  Params:  x1,y1 : Integer
  Returns: Nothing

  Draws a point. Helper function of winapi. DrawFocusRect
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.drawPoint(x1: Integer; y1: Integer);
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.drawPoint() x1: ',x1,' y1: ',y1);
  {$endif}
  QPainter_drawPoint(Widget, x1, y1);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.setBrushOrigin
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.setBrushOrigin(x, y: Integer);
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.setBrushOrigin() x: ',x,' y: ',y);
  {$endif}
  QPainter_setBrushOrigin(Widget, x, y);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.brushOrigin
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.getBrushOrigin(retval: PPoint);
var
  QtPoint: TQtPoint;
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.brushOrigin() ');
  {$endif}

  QPainter_brushOrigin(Widget, @QtPoint);
  retval^.x := QtPoint.x;
  retval^.y := QtPoint.y;
end;

function TQtDeviceContext.getClipping: Boolean;
begin
  Result := QPainter_hasClipping(Widget);
end;

procedure TQtDeviceContext.getPenPos(retval: PPoint);
begin
  retval^.x := FPenPos.x;
  retval^.y := FPenPos.y;
end;

function TQtDeviceContext.getWorldMatrix: QMatrixH;
begin
  Result := QPainter_worldMatrix(Widget);
end;

procedure TQtDeviceContext.setPenPos(x, y: Integer);
begin
  FPenPos.X := x;
  FPenPos.Y := y;
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.font
  Params:  None
  Returns: The current font object of the DC
 ------------------------------------------------------------------------------}
function TQtDeviceContext.font: TQtFont;
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.font()');
  {$endif}

  {when Parent=nil we'll create vFont on demand, otherwise
    we'll get 1 unfreed mem block}
  if (Parent = nil) and (vFont = nil) then
    vFont := TQtFont(GetStockObject(SYSTEM_FONT));

  if vFont <> nil then
    vFont.Widget := QPainter_font(Widget);
    
  if SelFont=nil then
    Result := vFont
  else
    Result := SelFont;
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.setFont
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.setFont(f: TQtFont);
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.setFont() ');
  {$endif}
  SelFont := F;
  if (f.Widget <> nil) and (Widget <> nil) {and (Parent <> nil)} then
  begin
    QPainter_setFont(Widget, QFontH(f.Widget));
    
    {when Parent=nil we'll create vFont on demand, otherwise
     we'll get 1 unfreed mem block}
    if (Parent = nil) and (vFont = nil) then
      vFont := TQtFont(GetStockObject(SYSTEM_FONT));
      
    vFont.Angle := f.Angle;
  end;
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.brush
  Params:  None
  Returns: The current brush object of the DC
 ------------------------------------------------------------------------------}
function TQtDeviceContext.brush: TQtBrush;
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.brush() ');
  {$endif}
  
  if vBrush <> nil then
    vBrush.Widget := QPainter_brush(Widget);
    
  if SelBrush=nil then
    Result := vBrush
  else
    Result := SelBrush;
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.setBrush
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.setBrush(ABrush: TQtBrush);
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.setBrush() ');
  {$endif}
  SelBrush := ABrush;
  if (ABrush.Widget <> nil) and (Widget <> nil) then
    QPainter_setBrush(Widget, ABrush.Widget);
end;

function TQtDeviceContext.BackgroundBrush: TQtBrush;
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.backgroundBrush() ');
  {$endif}
  vBackgroundBrush.Widget := QPainter_background(Widget);
  result := vBackGroundBrush;
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.pen
  Params:  None
  Returns: The current pen object of the DC
 ------------------------------------------------------------------------------}
function TQtDeviceContext.pen: TQtPen;
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.pen() ');
  {$endif}
  
  if vPen <> nil then
    vPen.Widget := QPainter_pen(Widget);
    
  if SelPen=nil then
    Result := vPen
  else
    Result := SelPen;
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.setPen
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtDeviceContext.setPen(APen: TQtPen): TQtPen;
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.setPen() ');
  {$endif}
  Result := pen;
  SelPen := APen;
  if (APen <> nil) and (APen.Widget <> nil) and (Widget <> nil) then
    QPainter_setPen(Widget, APen.Widget);
end;

procedure TQColorToColorRef(const AColor: TQColor; out AColorRef: TColorRef);
begin
  AColorRef:=(( AColor.r shr 8) and $FF)
       or (AColor.g  and $ff00)
       or ((AColor.b  shl 8) and $ff0000);
end;

procedure ColorRefToTQColor(const AColorRef: TColorRef; var AColor:TQColor);
begin
  AColor.r:=(AColorRef and $ff);
  AColor.r:=AColor.r+(AColor.r shl 8);
  AColor.g:=(AColorRef and $ff00);
  AColor.g:=AColor.g+(AColor.g shr 8);
  AColor.b:=(AColorRef and $ff0000) shr 8;
  AColor.b:=AColor.b+(AColor.b shr 8);
  AColor.ColorSpec := Ord(QColorRGB);
  AColor.Alpha := 65535;
  AColor.Pad := 0;
end;

procedure DebugRegion(const msg: string; Rgn: QRegionH);
var
  R: TRect;
  ok: boolean;
begin
  Write(Msg);
  ok := QRegion_isEmpty(Rgn);
  QRegion_BoundingRect(Rgn, @R);
  WriteLn(' Empty=',Ok,' Rect=', dbgs(R));
end;

function QtDefaultPrinter: TQtPrinter;
begin
  if FPrinter = nil then
    FPrinter := TQtPrinter.Create;
  Result := FPrinter;
end;

function Clipboard: TQtClipboard;
begin
  if FClipboard = nil then
    FClipboard := TQtClipboard.Create;
  Result := FClipboard;
end;

function TQtDeviceContext.SetBkColor(Color: TcolorRef): TColorRef;
var
  NColor: TQColor;
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.setBKColor() ');
  {$endif}
  NColor := BackgroundBrush.getColor^;
  TQColorToColorRef(NColor, Result);
  ColorRefToTQColor(ColorToRGB(Color), NColor);
  BackgroundBrush.setColor(@NColor);
end;

function TQtDeviceContext.SetBkMode(BkMode: Integer): Integer;
var
  Mode: QtBGMode;
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.setBKMode() ');
  {$endif}
  result := 0;
  if Widget <> nil then
  begin
    Mode := QPainter_BackgroundMode(Widget);
    if Mode = QtOpaqueMode then
      result := OPAQUE
    else
      result := TRANSPARENT;

    if BkMode = OPAQUE then
      Mode := QtOpaqueMode
    else
      Mode := QtTransparentMode;
    QPainter_SetBackgroundMode(Widget, Mode);
  end;
end;

function TQtDeviceContext.getDeviceSize: TPoint;
var
  device: QPaintDeviceH;
begin
  device := QPainter_device(Widget);
  Result.x := QPaintDevice_width(device);
  Result.y := QPaintDevice_height(device);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.getRegionType
  Params:  QRegionH
  Returns: Region type
 ------------------------------------------------------------------------------}
function TQtDeviceContext.getRegionType(ARegion: QRegionH): integer;
var
  R: TRect;
begin
  try
    if QRegion_isEmpty(ARegion) then
      Result := NULLREGION
    else
    begin
      QRegion_boundingRect(ARegion, @R);
      if QRegion_contains(ARegion, PRect(@R)) then
        Result := SIMPLEREGION
      else
        Result := COMPLEXREGION;
    end;
  except
    Result := ERROR;
  end;
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.region
  Params:  None
  Returns: The current clip region
 ------------------------------------------------------------------------------}
function TQtDeviceContext.getClipRegion: TQtRegion;
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.region() ');
  {$endif}
  if vRegion.Widget=nil then
    vRegion.Widget := QRegion_Create();
    
  QPainter_clipRegion(Widget,  vRegion.Widget);
  Result := vRegion;
end;

procedure TQtDeviceContext.setClipping(const AValue: Boolean);
begin
  QPainter_setClipping(Widget, AValue);
end;

procedure TQtDeviceContext.setClipRegion(ARegion: QRegionH;
  AOperation: QtClipOperation = QtReplaceClip);
begin
  QPainter_SetClipRegion(Widget, ARegion, AOperation);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.setRegion
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.setRegion(ARegion: TQtRegion);
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.setRegion() ');
  {$endif}
  if (ARegion.Widget <> nil) and (Widget <> nil) then
    setClipRegion(ARegion.Widget);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.drawImage
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.drawImage(targetRect: PRect;
     image: QImageH; sourceRect: PRect;
      mask: QImageH; maskRect: PRect; flags: QtImageConversionFlags = QtAutoColor);
var
  LocalRect: TRect;
  APixmap, ATemp: QPixmapH;
  AMask: QBitmapH;
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.drawImage() ');
  {$endif}
  LocalRect := targetRect^;
  if mask <> nil then
  begin
    // TODO: check maskRect
    APixmap := QPixmap_create();
    try
      QPixmap_fromImage(APixmap, image, flags);
      ATemp := QPixmap_create();
      try
        // QBitmap_fromImage raises assertion in the qt library
        QPixmap_fromImage(ATemp, mask, flags);
        AMask := QBitmap_create(ATemp);
        try
          QPixmap_setMask(APixmap, AMask);
          QPainter_drawPixmap(Widget, PRect(@LocalRect), APixmap, sourceRect);
        finally
          QBitmap_destroy(AMask);
        end;
      finally
        QPixmap_destroy(ATemp);
      end;
    finally
      QPixmap_destroy(APixmap);
    end;
  end
  else
    QPainter_drawImage(Widget, PRect(@LocalRect), image, sourceRect, flags);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.rotate
  Params:  None
  Returns: Nothing
  
  Rotates the coordinate system
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.rotate(a: Double);
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.rotate() ');
  {$endif}
  QPainter_rotate(Widget, a);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.save
  Params:  None
  Returns: Nothing
  
  Saves the state of the canvas
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.save;
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.save() ');
  {$endif}
  QPainter_save(Widget);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.restore
  Params:  None
  Returns: Nothing
  
  Restores the state of the canvas
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.restore;
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.restore() ');
  {$endif}
  QPainter_restore(Widget);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.translate
  Params:  None
  Returns: Nothing
  
  Tranlates the coordinate system
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.translate(dx: Double; dy: Double);
begin
  {$ifdef VerboseQt}
  WriteLn('TQtDeviceContext.translate() ');
  {$endif}
  QPainter_translate(Widget, dx, dy);
end;

{ TQtPixmap }

constructor TQtPixmap.Create(p1: PSize);
begin
  FHandle := QPixmap_create(p1);
end;

destructor TQtPixmap.Destroy;
begin
  if FHandle <> nil then
    QPixmap_destroy(FHandle);

  inherited Destroy;
end;

function TQtPixmap.getHeight: Integer;
begin
  Result := QPixmap_height(Handle);
end;

function TQtPixmap.getWidth: Integer;
begin
  Result := QPixmap_width(Handle);
end;

procedure TQtPixmap.grabWidget(AWidget: QWidgetH; x: Integer = 0; y: Integer = 0; w: Integer = -1; h: Integer = -1);
begin
  QPixmap_grabWidget(FHandle, AWidget, x, y, w, h);
end;

procedure TQtPixmap.grabWindow(p1: Cardinal; x: Integer; y: Integer; w: Integer; h: Integer);
begin
  QPixmap_grabWindow(FHandle, p1, x, y, w, h);
end;

procedure TQtPixmap.toImage(retval: QImageH);
begin
  QPixmap_toImage(FHandle, retval);
end;

class procedure TQtPixmap.fromImage(retval: QPixmapH; image: QImageH; flags: QtImageConversionFlags = QtAutoColor);
begin
  QPixmap_fromImage(retval, image, flags);
end;

{ TQtSystemTrayIcon }

constructor TQtSystemTrayIcon.Create(vIcon: QIconH);
begin
  inherited Create;

  handle := QSystemTrayIcon_create(vicon, nil);
end;

destructor TQtSystemTrayIcon.Destroy;
begin
  QSystemTrayIcon_destroy(handle);

  inherited Destroy;
end;

{*******************************************************************
*  TQtSystemTrayIcon.TIconToQIconH ()
*
*  DESCRIPTION:    Converts a TIcon to a QIconH
*******************************************************************}
class function TQtSystemTrayIcon.TIconToQIconH(const AIcon: TIcon): QIconH;
var
  Pixmap: QPixmapH;
begin
  if (AIcon <> nil) and (AIcon.Handle <> 0) then
  begin
    Pixmap := QPixmap_create();
    QPixmap_fromImage(Pixmap, TQtImage(AIcon.Handle).Handle);

    Result := QIcon_create(Pixmap);
    QPixmap_destroy(Pixmap);
  end
  else
    Result := QIcon_create();
end;

procedure TQtSystemTrayIcon.setContextMenu(menu: QMenuH);
begin
  QSystemTrayIcon_setContextMenu(handle, menu);
end;

procedure TQtSystemTrayIcon.setIcon(icon: QIconH);
begin
  QSystemTrayIcon_setIcon(handle, icon);
end;

procedure TQtSystemTrayIcon.setToolTip(tip: WideString);
begin
  QSystemTrayIcon_setToolTip(handle, @tip)
end;

procedure TQtSystemTrayIcon.show;
begin
  QSystemTrayIcon_show(handle);
end;

procedure TQtSystemTrayIcon.hide;
begin
  QSystemTrayIcon_hide(handle);
end;

{ TQtButtonGroup }

constructor TQtButtonGroup.Create(AParent: QObjectH);
begin
  inherited Create;

  Handle := QButtonGroup_create(AParent);
end;

destructor TQtButtonGroup.Destroy;
begin
  QButtonGroup_destroy(Handle);
  inherited Destroy;
end;

procedure TQtButtonGroup.AddButton(AButton: QAbstractButtonH); overload;
begin
  QButtonGroup_addButton(Handle, AButton);
end;

procedure TQtButtonGroup.AddButton(AButton: QAbstractButtonH; id: Integer); overload;
begin
  QButtonGroup_addButton(Handle, AButton, id);
end;

function TQtButtonGroup.ButtonFromId(id: Integer): QAbstractButtonH;
begin
  Result := QButtonGroup_button(Handle, id);
end;

procedure TQtButtonGroup.RemoveButton(AButton: QAbstractButtonH);
begin
  QButtonGroup_removeButton(Handle, AButton);
end;

procedure TQtButtonGroup.SetExclusive(AExclusive: Boolean);
begin
  QButtonGroup_setExclusive(Handle, AExclusive);
end;

function TQtButtonGroup.GetExclusive: Boolean;
begin
  Result := QButtonGroup_exclusive(Handle);
end;

procedure TQtButtonGroup.SignalButtonClicked(AButton: QAbstractButtonH); cdecl;
begin
  {todo}
end;

{ TQtClipboard }

constructor TQtClipboard.Create;
begin
  inherited Create;
  {$IFNDEF MSWINDOWS}
  FClipChanged := True;
  FClipCounter := 0;
  {$ENDIF}
  FOnClipBoardRequest := nil;
  FClipBoardFormats := TStringList.Create;
  FClipBoardFormats.Add('foo'); // 0 is reserved
  TheObject := QApplication_clipBoard;
  AttachEvents;
end;

destructor TQtClipboard.Destroy;
begin
  DetachEvents;
  FClipBoardFormats.Free;
  inherited Destroy;
end;

procedure TQtClipboard.AttachEvents;
{$IFNDEF MSWINDOWS}
var
  Method: TMethod;
{$ENDIF}
begin
  inherited AttachEvents;
  {$IFNDEF MSWINDOWS}
  FClipDataChangedHook := QClipboard_hook_create(TheObject);
  QClipboard_dataChanged_Event(Method) := @signalDataChanged;
  QClipboard_hook_hook_dataChanged(FClipDataChangedHook, Method);
  {$ENDIF}
end;

procedure TQtClipboard.signalDataChanged; cdecl;
begin
  {$IFNDEF MSWINDOWS}
  {$IFDEF VERBOSE_QT_CLIPBOARD}
  writeln('signalDataChanged()');
  {$ENDIF}
  FClipChanged := True;
  inc(FClipCounter);
  {$ENDIF}
end;

function TQtClipboard.EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl;
begin
  BeginEventProcessing;
  Result := False;

  if QEvent_type(Event) = QEventClipboard then
  begin
    {$IFDEF MSWINDOWS}
    Result := True;
    {$ELSE}
    Result := FClipChanged;
    {$IFDEF VERBOSE_QT_CLIPBOARD}
    writeln('TQtClipboard.EventFilter() RESULT=',Result);
    {$ENDIF}
    
    if FClipCounter > 0 then
      dec(FClipCounter);

    FClipChanged := FClipCounter > 0;
    {$ENDIF}
    QEvent_accept(Event);

    // Clipboard is changed, but we have no ability at moment to pass that info
    // to LCL since LCL has no support for that event
  end;
  EndEventProcessing;
end;

function TQtClipboard.Clipboard: QClipboardH;
begin
  Result := QClipboardH(TheObject);
end;

function TQtClipboard.getMimeData(AMode: QClipboardMode): QMimeDataH;
begin
  Result := QClipboard_mimeData(Clipboard, AMode);
end;

procedure TQtClipboard.setMimeData(AMimeData: QMimeDataH; AMode: QClipboardMode);
begin
  QClipboard_setMimeData(Clipboard, AMimeData, AMode);
end;

procedure TQtClipboard.Clear(AMode: QClipboardMode);
begin
  QClipboard_clear(ClipBoard, AMode);
end;

function TQtClipboard.FormatToMimeType(AFormat: TClipboardFormat): String;
begin
  if FClipBoardFormats.Count > Integer(AFormat) then
    Result := FClipBoardFormats[AFormat]
  else
    Result := '';
end;

function TQtClipboard.RegisterFormat(AMimeType: String): TClipboardFormat;
var
  Index: Integer;
begin
  Index := FClipBoardFormats.IndexOf(AMimeType);
  if Index < 0 then
    Index := FClipBoardFormats.Add(AMimeType);
  Result := Index;
end;

function TQtClipboard.GetData(ClipboardType: TClipboardType;
  FormatID: TClipboardFormat; Stream: TStream): boolean;
var
  QtMimeData: QMimeDataH;
  MimeType: WideString;
  Data: QByteArrayH;
  p: PAnsiChar;
  s: Integer;
begin
  Result := False;
  QtMimeData := getMimeData(ClipbBoardTypeToQtClipboard[ClipBoardType]);
  MimeType := FormatToMimeType(FormatID);
  Data := QByteArray_create();
  QMimeData_data(QtMimeData, Data, @MimeType);
  s := QByteArray_size(Data);
  p := QByteArray_data(Data);
  Stream.Write(p^, s);
  // what to do with p? FreeMem or nothing?
  QByteArray_destroy(Data);
  Result := True;
end;

function TQtClipboard.GetFormats(ClipboardType: TClipboardType;
  var Count: integer; var List: PClipboardFormat): boolean;
var
  QtMimeData: QMimeDataH;
  QtList: QStringListH;
  i: Integer;
  Str: WideString;
begin
  Result := False;
  Count := 0;
  List := nil;

  QtMimeData := getMimeData(ClipbBoardTypeToQtClipboard[ClipBoardType]);

  QtList := QStringList_create;
  QMimeData_formats(QtMimeData, QtList);

  try
    Count := QStringList_size(QtList);
    {$IFNDEF MSWINDOWS}
    if Count > 0 then
    {$ENDIF}
      GetMem(List, Count * SizeOf(TClipboardFormat));

    for i := 0 to Count - 1 do
    begin
      QStringList_at(QtList, @Str, i);
      Str := UTF8Encode(Str);
      List[i] := RegisterFormat(Str);
    end;

    Result := Count > 0;

  finally
    QStringList_destroy(QtList);
  end;
end;

function TQtClipboard.GetOwnerShip(ClipboardType: TClipboardType;
  OnRequestProc: TClipboardRequestEvent; FormatCount: integer;
  Formats: PClipboardFormat): boolean;

  procedure PutOnClipBoard;
  var
    MimeType: WideString;
    MimeData: QMimeDataH;
    Data: QByteArrayH;
    DataStream: TMemoryStream;
    I: Integer;
  begin
    MimeData := QMimeData_create();
    DataStream := TMemoryStream.Create;
    
    for I := 0 to FormatCount - 1 do
    begin
      DataStream.Size := 0;
      DataStream.Position := 0;

      MimeType := FormatToMimeType(Formats[I]);
      FOnClipBoardRequest(Formats[I], DataStream);

      Data := QByteArray_create(PAnsiChar(DataStream.Memory), DataStream.Size);
      QMimeData_setData(MimeData, @MimeType, Data);
      QByteArray_destroy(Data);
    end;

    DataStream.Free;
    setMimeData(MimeData, ClipbBoardTypeToQtClipboard[ClipBoardType]);
    // do not destroy MimeData!!!
  end;
begin
  Result := False;
  if (FormatCount = 0) or (OnRequestProc = nil) then
  begin
  { The LCL indicates it doesn't have the clipboard data anymore
    and the interface can't use the OnRequestProc anymore.}
    FOnClipBoardRequest := nil;
    Result := True;
  end else
  begin
  { clear OnClipBoardRequest to prevent destroying the LCL clipboard,
    when emptying the clipboard}
    FOnClipBoardRequest := nil;
    {Clipboard is cleared each time when setMimeData is called !}
    {$IFNDEF MSWINDOWS}
    Clear(ClipbBoardTypeToQtClipboard[ClipBoardType]);
    {$ENDIF}
    FOnClipBoardRequest := OnRequestProc;
    PutOnClipBoard;
    Result := True;
  end;
end;

{ TQtPrinter }

constructor TQtPrinter.Create;
begin
  inherited Create;
  FHandle := QPrinter_create();
end;

destructor TQtPrinter.Destroy;
begin
  if FHandle <> nil then
    QPrinter_destroy(FHandle);
  inherited Destroy;
end;

function TQtPrinter.getCollateCopies: Boolean;
begin
  Result := QPrinter_collateCopies(FHandle);
end;

function TQtPrinter.getColorMode: QPrinterColorMode;
begin
  Result := QPrinter_colorMode(FHandle);
end;

function TQtPrinter.getCreator: WideString;
var
  Str: WideString;
begin
  QPrinter_creator(FHandle, @Str);
  Result := UTF8Encode(Str);
end;

function TQtPrinter.getDevType: Integer;
begin
  Result := QPrinter_devType(FHandle);
end;

function TQtPrinter.getDocName: WideString;
var
  Str: WideString;
begin
  QPrinter_docName(FHandle, @Str);
  Result := UTF8Encode(Str);
end;

function TQtPrinter.getDoubleSidedPrinting: Boolean;
begin
  Result := QPrinter_doubleSidedPrinting(FHandle);
end;

function TQtPrinter.getFontEmbedding: Boolean;
begin
  Result := QPrinter_fontEmbeddingEnabled(FHandle);
end;

function TQtPrinter.getFullPage: Boolean;
begin
  Result := QPrinter_fullPage(FHandle);
end;

procedure TQtPrinter.setOutputFormat(const AValue: QPrinterOutputFormat);
begin
  QPrinter_setOutputFormat(FHandle, AValue);
end;

procedure TQtPrinter.setPaperSource(const AValue: QPrinterPaperSource);
begin
  QPrinter_setPaperSource(FHandle, AValue);
end;

function TQtPrinter.getOutputFormat: QPrinterOutputFormat;
begin
  Result := QPrinter_outputFormat(FHandle);
end;

function TQtPrinter.getPaperSource: QPrinterPaperSource;
begin
  Result := QPrinter_paperSource(FHandle);
end;

function TQtPrinter.getPrintProgram: WideString;
var
  Str: WideString;
begin
  QPrinter_printProgram(FHandle, @Str);
  Result := UTF8Encode(Str);
end;

function TQtPrinter.getPrintRange: QPrinterPrintRange;
begin
  Result := QPrinter_printRange(FHandle);
end;

procedure TQtPrinter.setCollateCopies(const AValue: Boolean);
begin
  QPrinter_setCollateCopies(FHandle, AValue);
end;

procedure TQtPrinter.setColorMode(const AValue: QPrinterColorMode);
begin
  QPrinter_setColorMode(FHandle, AValue);
end;

procedure TQtPrinter.setCreator(const AValue: WideString);
var
  Str: WideString;
begin
  Str := GetUtf8String(AValue);
  QPrinter_setCreator(FHandle, @Str);
end;

procedure TQtPrinter.setDocName(const AValue: WideString);
var
  Str: WideString;
begin
  Str := GetUtf8String(AValue);
  QPrinter_setDocName(FHandle, @Str);
end;

procedure TQtPrinter.setDoubleSidedPrinting(const AValue: Boolean);
begin
  QPrinter_setDoubleSidedPrinting(FHandle, AValue);
end;

procedure TQtPrinter.setFontEmbedding(const AValue: Boolean);
begin
  QPrinter_setFontEmbeddingEnabled(FHandle, AValue);
end;

procedure TQtPrinter.setFullPage(const AValue: Boolean);
begin
  QPrinter_setFullPage(FHandle, AValue);
end;

procedure TQtPrinter.setPrinterName(const AValue: WideString);
var
  Str: WideString;
begin
  Str := GetUtf8String(AValue);
  QPrinter_setPrinterName(FHandle, @Str);
end;

function TQtPrinter.getPrinterName: WideString;
var
  Str: WideString;
begin
  QPrinter_printerName(FHandle, @Str);
  Result := UTF8Encode(Str);
end;

procedure TQtPrinter.setOutputFileName(const AValue: WideString);
var
  Str: WideString;
begin
  Str := GetUtf8String(AValue);
  QPrinter_setOutputFileName(FHandle, @Str);
end;

function TQtPrinter.getOutputFileName: WideString;
var
  Str: WideString;
begin
  QPrinter_outputFileName(FHandle, @Str);
  Result := UTF8Encode(Str);
end;

procedure TQtPrinter.setOrientation(const AValue: QPrinterOrientation);
begin
  QPrinter_setOrientation(FHandle, AValue);
end;

function TQtPrinter.getOrientation: QPrinterOrientation;
begin
  Result := QPrinter_orientation(FHandle);
end;

procedure TQtPrinter.setPageSize(const AValue: QPrinterPageSize);
begin
  QPrinter_setPageSize(FHandle, AValue);
end;

function TQtPrinter.getPageSize: QPrinterPageSize;
begin
  Result := QPrinter_pageSize(FHandle);
end;

procedure TQtPrinter.setPageOrder(const AValue: QPrinterPageOrder);
begin
  QPrinter_setPageOrder(FHandle, AValue);
end;

function TQtPrinter.getPageOrder: QPrinterPageOrder;
begin
  Result := QPrinter_pageOrder(FHandle);
end;

procedure TQtPrinter.setPrintProgram(const AValue: WideString);
var
  Str: WideString;
begin
  Str := GetUtf8String(AValue);
  QPrinter_setPrintProgram(FHandle, @Str);
end;

procedure TQtPrinter.setPrintRange(const AValue: QPrinterPrintRange);
begin
  QPrinter_setPrintRange(FHandle, AValue);
end;

procedure TQtPrinter.setResolution(const AValue: Integer);
begin
  QPrinter_setResolution(FHandle, AValue);
end;

function TQtPrinter.getResolution: Integer;
begin
  Result := QPrinter_resolution(FHandle);
end;

function TQtPrinter.getNumCopies: Integer;
begin
  Result := QPrinter_numCopies(FHandle);
end;

procedure TQtPrinter.setNumCopies(const AValue: Integer);
begin
  QPrinter_setNumCopies(FHandle, AValue);
end;

function TQtPrinter.getPrinterState: QPrinterPrinterState;
begin
  Result := QPrinter_printerState(FHandle);
end;

function TQtPrinter.NewPage: Boolean;
begin
  Result := QPrinter_newPage(FHandle);
end;

function TQtPrinter.Abort: Boolean;
begin
  Result := QPrinter_abort(FHandle);
end;

procedure TQtPrinter.setFromPageToPage(const AFromPage, AToPage: Integer);
begin
  QPrinter_setFromTo(FHandle, AFromPage, AToPage);
end;

function TQtPrinter.fromPage: Integer;
begin
  Result := QPrinter_fromPage(FHandle);
end;

function TQtPrinter.toPage: Integer;
begin
  Result := QPrinter_toPage(FHandle);
end;

function TQtPrinter.PaintEngine: QPaintEngineH;
begin
  Result := QPrinter_paintEngine(FHandle);
end;

function TQtPrinter.PageRect: TRect;
begin
  QPrinter_pageRect(FHandle, @Result);
end;

function TQtPrinter.PaperRect: TRect;
begin
  QPrinter_paperRect(FHandle, @Result);
end;

function TQtPrinter.PrintEngine: QPrintEngineH;
begin
  Result := QPrinter_printEngine(FHandle);
end;


{ TQtTimer }

{------------------------------------------------------------------------------
  Function: TQtTimer.CreateTimer
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtTimer.CreateTimer(Interval: integer;
  const TimerFunc: TFNTimerProc; App: QObjectH);
begin
  inherited Create;
  FAppObject := App;

  FCallbackFunc := TimerFunc;

  TheObject := QTimer_create(App);

  QTimer_setInterval(QTimerH(TheObject), Interval);

  AttachEvents;

  // start timer and get ID
  QTimer_start(QTimerH(TheObject), Interval);
  FId := QTimer_timerId(QTimerH(TheObject));

  {$ifdef VerboseQt}
    WriteLn('TQtTimer.CreateTimer: Interval = ', Interval, ' ID = ', FId);
  {$endif}
end;

{------------------------------------------------------------------------------
  Function: TQtTimer.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtTimer.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtTimer.CreateTimer: Destroy. ID = ', FId);
  {$endif}

  FCallbackFunc := nil;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtTimer.EventFilter
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtTimer.EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl;
begin
  BeginEventProcessing;
  Result := False;

  if QEvent_type(Event) = QEventTimer then
  begin
    Result := True;

    QEvent_accept(Event);

    if Assigned(FCallbackFunc) then
      FCallbackFunc;
  end;
  EndEventProcessing;
end;

{ TQtIcon }

constructor TQtIcon.Create;
begin
  FHandle := QIcon_create();
end;

destructor TQtIcon.Destroy;
begin
  if FHandle <> nil then
    QIcon_destroy(FHandle);
    
  inherited Destroy;
end;

{ TQtStringList }

function TQtStringList.Get(Index: Integer): string;
var
  W: Widestring;
begin
  QStringList_at(FHandle, @W, Index);
  Result := Utf8Encode(W);
end;

function TQtStringList.GetCount: Integer;
begin
  Result := QStringList_size(FHandle);
end;

constructor TQtStringList.Create;
begin
  FHandle := QStringList_create();
  FOwnHandle := True;
end;

constructor TQtStringList.Create(Source: QStringListH);
begin
  FHandle := Source;
  FOwnHandle := False;
end;

destructor TQtStringList.Destroy;
begin
  if FOwnHandle then
    QStringList_destroy(FHandle);
  inherited Destroy;
end;

procedure TQtStringList.Clear;
begin
  QStringList_clear(FHandle);
end;

procedure TQtStringList.Delete(Index: Integer);
begin
  QStringList_removeAt(FHandle, Index);
end;

procedure TQtStringList.Insert(Index: Integer; const S: string);
var
  W: WideString;
begin
  W := GetUtf8String(S);
  QStringList_insert(FHandle, Index, @W);
end;

end.



