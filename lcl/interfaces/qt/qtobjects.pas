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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    * 
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
  LCLType, LCLIntf, Menus, LCLProc, Graphics, ClipBrd, ExtCtrls, Interfacebase;

type
  // forward declarations
  TQActions = Array of QActionH;
  TQtImage = class;
  TQtFontMetrics = class;
  TQtTimer = class;
  TRop2OrCompositionSupport = (rocNotSupported, rocSupported, rocUndefined);

  { TQtObject }
  TQtObject = class(TObject)
  private
    FUpdateCount: Integer;
    FInEventCount: Integer;
    FReleaseInEvent: Boolean;
  public
    FDeleteLater: Boolean;
    FEventHook: QObject_hookH;
    FDestroyedHook: QObject_hookH;
    TheObject: QObjectH;
    constructor Create; virtual; overload;
    destructor Destroy; override;
    procedure Release; virtual;
  public
    procedure AttachEvents; virtual;
    procedure DetachEvents; virtual;
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl; virtual; abstract;
    procedure Destroyed; cdecl; virtual;
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
    FSelected: Boolean;
  end;

  { TQtActionGroup }

  TQtActionGroup = class(TObject)
  private
    FActions: TQActions;
    FGroupIndex: integer;
    FHandle: QActionGroupH;
    function getEnabled: boolean;
    function getExclusive: boolean;
    function getVisible: boolean;
    procedure setEnabled(const AValue: boolean);
    procedure setExclusive(const AValue: boolean);
    procedure setVisible(const AValue: boolean);
  public
    constructor Create(const AParent: QObjectH = nil);
    destructor Destroy; override;
    function addAction(action: QActionH): QActionH; overload;
    function addAction(text: WideString): QActionH; overload;
    function addAction(icon: QIconH; text: WideString): QActionH; overload;
    procedure removeAction(action: QActionH);
    function actions: TQActions;
    function checkedAction: QActionH;
    procedure setDisabled(ADisabled: Boolean);
    property Enabled: boolean read getEnabled write setEnabled;
    property Exclusive: boolean read getExclusive write setExclusive;
    property GroupIndex: integer read FGroupIndex write FGroupIndex;
    property Handle: QActionGroupH read FHandle;
    property Visible: boolean read getVisible write setVisible;
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
    function depth: Integer;
    function dotsPerMeterX: Integer;
    function dotsPerMeterY: Integer;
    function bits: PByte;
    function numBytes: Integer;
    function bytesPerLine: Integer;
    procedure invertPixels(InvertMode: QImageInvertMode = QImageInvertRgb);
    function getFormat: QImageFormat;
  end;

  { TQtFont }

  TQtFont = class(TQtResource)
  private
    FDefaultFont: QFontH;
    FMetrics: TQtFontMetrics;
    function GetMetrics: TQtFontMetrics;
    function GetDefaultFont: QFontH;
  public
    Widget: QFontH;
    Angle: Integer;
  public
    constructor Create(CreateHandle: Boolean); virtual;
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
    function getStyleStategy: QFontStyleStrategy;

    procedure setPointSize(p1: Integer);
    procedure setPixelSize(p1: Integer);
    procedure setWeight(p1: Integer);
    procedure setBold(p1: Boolean);
    procedure setItalic(b: Boolean);
    procedure setUnderline(p1: Boolean);
    procedure setStrikeOut(p1: Boolean);
    procedure setRawName(p1: string);
    procedure setFamily(p1: string);
    procedure setStyleStrategy(s: QFontStyleStrategy);
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
    function width(p1: PWideString): Integer; overload;
    function width(p1: PWideString; ALen: Integer): Integer; overload;
    function ascent: Integer;
    function descent: Integer;
    function leading: Integer;
    function maxWidth: Integer;
    procedure boundingRect(retval: PRect; r: PRect; flags: Integer; text: PWideString; tabstops: Integer = 0; tabarray: PInteger = nil);
    function charWidth(str: WideString; pos: Integer): Integer;
    function averageCharWidth: Integer;
    function elidedText(const AText: WideString;
      const AMode: QtTextElideMode; const AWidth: Integer;
      const AFlags: Integer = 0): WideString;
  end;

  { TQtBrush }

  TQtBrush = class(TQtResource)
  private
    function getStyle: QtBrushStyle;
    procedure setStyle(style: QtBrushStyle);
  public
    Widget: QBrushH;
    constructor Create(CreateHandle: Boolean); virtual;
    destructor Destroy; override;
    function getColor: PQColor;
    function GetLBStyle(out AStyle: LongWord; out AHatch: PtrUInt): Boolean;
    procedure setColor(AColor: PQColor);
    procedure setTexture(pixmap: QPixmapH);
    procedure setTextureImage(image: QImageH);
    property Style: QtBrushStyle read getStyle write setStyle;
  end;

  { TQtPen }

  TQtPen = class(TQtResource)
  private
    FIsExtPen: Boolean;
  public
    Widget: QPenH;
    constructor Create(CreateHandle: Boolean); virtual;
    destructor Destroy; override;
  public
    function getCapStyle: QtPenCapStyle;
    function getColor: TQColor;
    function getCosmetic: Boolean;
    function getJoinStyle: QtPenJoinStyle;
    function getWidth: Integer;
    function getStyle: QtPenStyle;
    function getDashPattern: TQRealArray;

    procedure setCapStyle(pcs: QtPenCapStyle);
    procedure setColor(p1: TQColor);
    procedure setCosmetic(b: Boolean);
    procedure setJoinStyle(pcs: QtPenJoinStyle);
    procedure setStyle(AStyle: QtPenStyle);
    procedure setBrush(brush: QBrushH);
    procedure setWidth(p1: Integer);
    procedure setDashPattern(APattern: PDWord; ALength: DWord);

    property IsExtPen: Boolean read FIsExtPen write FIsExtPen;
  end;


  { TQtRegion }

  TQtRegion = class(TQtResource)
  private
    FPolygon: QPolygonH;
    function GetIsPolyRegion: Boolean;
  public
    Widget: QRegionH;
    constructor Create(CreateHandle: Boolean); virtual; overload;
    constructor Create(CreateHandle: Boolean; X1,Y1,X2,Y2: Integer;
      Const RegionType: QRegionRegionType = QRegionRectangle); virtual; overload;
    constructor Create(CreateHandle: Boolean; Poly: QPolygonH;
      Const Fill: QtFillRule = QtWindingFill); virtual; overload;
    destructor Destroy; override;
    function containsPoint(X,Y: Integer): Boolean;
    function containsRect(R: TRect): Boolean;
    function intersects(R: TRect): Boolean; overload;
    function intersects(Rgn: QRegionH): Boolean; overload;
    function GetRegionType: integer;
    function getBoundingRect: TRect;
    function numRects: Integer;
    procedure translate(dx, dy: Integer);
    property IsPolyRegion: Boolean read GetIsPolyRegion;
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
    FSupportRasterOps: TRop2OrCompositionSupport;
    FSupportComposition: TRop2OrCompositionSupport;
    FRopMode: Integer;
    FPenPos: TQtPoint;
    FOwnPainter: Boolean;
    SelFont: TQtFont;
    SelBrush: TQtBrush;
    SelPen: TQtPen;
    PenColor: TQColor;
    FMetrics: TQtFontMetrics;
    function GetMetrics: TQtFontMetrics;
    function GetRop: Integer;
    function DeviceSupportsComposition: Boolean;
    function DeviceSupportsRasterOps: Boolean;
    function R2ToQtRasterOp(AValue: Integer): QPainterCompositionMode;
    procedure RestorePenColor;
    procedure RestoreTextColor;
    procedure SetRop(const AValue: Integer);
  public
    { public fields }
    Widget: QPainterH;
    Parent: QWidgetH;
    ParentPixmap: QPixmapH;
    vBrush: TQtBrush;
    vFont: TQtFont;
    vImage: TQtImage;
    vPen: TQtPen;
    vRegion: TQtRegion;
    vBackgroundBrush: TQtBrush;
    vClipRect: PRect;         // is the cliprect paint event give to us
    vClipRectDirty: boolean;  // false=paint cliprect is still valid
    vTextColor: TColorRef;
    vMapMode: Integer;
  public
    { Our own functions }
    constructor Create(AWidget: QWidgetH; const APaintEvent: Boolean = False); virtual;
    constructor CreatePrinterContext(ADevice: QPrinterH); virtual;
    constructor CreateFromPainter(APainter: QPainterH);
    destructor Destroy; override;
    procedure CreateObjects;
    procedure DestroyObjects;
    function CreateDCData: PQtDCDATA;
    function RestoreDCData(var DCData: PQtDCData): boolean;
    procedure DebugClipRect(const msg: string);
    procedure setImage(AImage: TQtImage);
    procedure CorrectCoordinates(var ARect: TRect);
    function GetLineLastPixelPos(PrevPos, NewPos: TPoint): TPoint;
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
    procedure drawPolyLine(P: PPoint; NumPts: Integer);
    procedure drawPolygon(P: PPoint; NumPts: Integer; FillRule: QtFillRule = QtOddEvenFill);
    procedure eraseRect(ARect: PRect);
    procedure fillRect(ARect: PRect; ABrush: QBrushH); overload;
    procedure fillRect(x, y, w, h: Integer; ABrush: QBrushH); overload;
    procedure fillRect(x, y, w, h: Integer); overload;

    procedure getBrushOrigin(retval: PPoint);
    function getClipping: Boolean;
    function getCompositionMode: QPainterCompositionMode;
    procedure setCompositionMode(mode: QPainterCompositionMode);
    procedure getPenPos(retval: PPoint);
    function getWorldTransform: QTransformH;
    procedure setBrushOrigin(x, y: Integer);
    procedure setPenPos(x, y: Integer);

    function font: TQtFont;
    procedure setFont(AFont: TQtFont);
    function brush: TQtBrush;
    procedure setBrush(ABrush: TQtBrush);
    function BackgroundBrush: TQtBrush;
    function GetBkColor: TColorRef;
    function pen: TQtPen;
    function setPen(APen: TQtPen): TQtPen;
    function SetBkColor(Color: TColorRef): TColorRef;
    function SetBkMode(BkMode: Integer): Integer;
    function getDepth: integer;
    function getDeviceSize: TPoint;
    function getRegionType(ARegion: QRegionH): integer;
    function getClipRegion: TQtRegion;
    procedure setClipping(const AValue: Boolean);
    procedure setClipRect(const ARect: TRect);
    procedure setClipRegion(ARegion: QRegionH; AOperation: QtClipOperation = QtReplaceClip);
    procedure setRegion(ARegion: TQtRegion);
    procedure drawImage(targetRect: PRect; image: QImageH; sourceRect: PRect;
      mask: QImageH; maskRect: PRect; flags: QtImageConversionFlags = QtAutoColor);
    function PaintEngine: QPaintEngineH;
    procedure rotate(a: Double);
    procedure setRenderHint(AHint: QPainterRenderHint; AValue: Boolean);
    procedure save;
    procedure restore;
    procedure translate(dx: Double; dy: Double);
    property Metrics: TQtFontMetrics read GetMetrics;
    property Rop2: Integer read GetRop write SetRop;
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
    procedure addPixmap(pixmap: QPixmapH; mode: QIconMode = QIconNormal; state: QIconState = QIconOff);
    property Handle: QIconH read FHandle;
  end;
  
  { TQtCursor }

  TQtCursor = class(TObject)
  protected
    FHandle: QCursorH;
  public
    constructor Create;
    constructor Create(pixmap: QPixmapH; hotX: Integer  = -1; hotY: Integer = -1);
    constructor Create(shape: QtCursorShape);
    destructor Destroy; override;
    property Handle: QCursorH read FHandle;
  end;
  
  { TQtSystemTrayIcon }

  TQtSystemTrayIcon = class(TObject)
  private
    FHook: QSystemTrayIcon_hookH;
  public
    Handle: QSystemTrayIconH;
    FTrayIcon: TCustomTrayIcon;
  public
    constructor Create(vIcon: QIconH); virtual;
    destructor Destroy; override;
  public
    procedure setContextMenu(menu: QMenuH);
    procedure setIcon(icon: QIconH);
    procedure setToolTip(tip: WideString);
    procedure signalActivated(AReason: QSystemTrayIconActivationReason); cdecl;
    procedure showBaloonHint(const ATitle, AHint: String;
      const AFlag: QSystemTrayIconMessageIcon; const ATimeOut: Integer);
    procedure Show;
    procedure Hide;
  end;
  
  { TQtButtonGroup }
  
  TQtButtonGroup = class(TObject)
  private
  public
    Handle: QButtonGroupH;
    constructor Create(AParent: QObjectH); virtual;
    destructor Destroy; override;
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
    FLockClip: Boolean;
    FClipDataChangedHook: QClipboard_hookH;
    {$IFDEF HASX11}
    FClipSelectionChangedHook: QClipboard_hookH;
    FSelTimer: TQtTimer; // timer for keyboard X11 selection
    FSelFmtCount: Integer;
    FLockX11Selection: Integer;
    {$ENDIF}
    FClipChanged: Boolean;
    FClipBoardFormats: TStringList;
    FOnClipBoardRequest: Array[TClipBoardType] of TClipboardRequestEvent;
    function IsClipboardChanged: Boolean;
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
    {$IFDEF HASX11}
    procedure BeginX11SelectionLock;
    procedure EndX11SelectionLock;
    function InX11SelectionLock: Boolean;
    procedure signalSelectionChanged; cdecl;
    procedure selectionTimer;
    {$ENDIF}
  end;

  { TQtPrinter }
  
  TQtPrinter = class(TObject)
  protected
    FHandle: QPrinterH;
    FPrinterContext: TQtDeviceContext;
  private
    FPrinterActive: Boolean;
    function GetDuplexMode: QPrinterDuplexMode;
    function getPrinterContext: TQtDeviceContext;
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
    procedure SetDuplexMode(AValue: QPrinterDuplexMode);
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
    constructor Create; virtual; overload;
    constructor Create(AMode: QPrinterPrinterMode); virtual; overload;
    destructor Destroy; override;

    function DefaultPrinter: WideString;
    function GetAvailablePrinters(Lst: TStrings): Boolean;
    
    procedure beginDoc;
    procedure endDoc;
    
    function NewPage: Boolean;
    function Abort: Boolean;
    procedure setFromPageToPage(Const AFromPage, AToPage: Integer);
    function fromPage: Integer;
    function toPage: Integer;
    function PaintEngine: QPaintEngineH;
    function PageRect: TRect; overload;
    function PaperRect: TRect; overload;
    function PageRect(AUnits: QPrinterUnit): TRect; overload;
    function PaperRect(AUnits: QPrinterUnit): TRect; overload;
    function PrintEngine: QPrintEngineH;
    function GetPaperSize(AUnits: QPrinterUnit): TSize;
    procedure SetPaperSize(ASize: TSize; AUnits: QPrinterUnit);
    function SupportedResolutions: TPtrIntArray;
    
    property Collate: Boolean read getCollateCopies write setCollateCopies;
    property ColorMode: QPrinterColorMode read getColorMode write setColorMode;
    property Creator: WideString read getCreator write setCreator;
    property DeviceType: Integer read getDevType;
    property DocName: WideString read getDocName write setDocName;
    property DoubleSidedPrinting: Boolean read getDoubleSidedPrinting write setDoubleSidedPrinting;
    property Duplex: QPrinterDuplexMode read GetDuplexMode write SetDuplexMode;
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
    property PrinterContext: TQtDeviceContext read getPrinterContext;
    property PrinterName: WideString read getPrinterName write setPrinterName;
    property PrinterActive: Boolean read FPrinterActive;
    property PrintRange: QPrinterPrintRange read getPrintRange write setPrintRange;
    property PrinterState: QPrinterPrinterState read getPrinterState;
    property PrintProgram: WideString read getPrintProgram write setPrintProgram;
    property Resolution: Integer read getResolution write setResolution;
  end;
  
  { TQtTimer }

  TQtTimer = class(TQtObject)
  private
    FTimerHook: QTimer_hookH;
    FCallbackFunc: TWSTimerProc;
    FId: Integer;
    FAppObject: QObjectH;
    function getTimerEnabled: Boolean;
    procedure setTimerEnabled(const AValue: Boolean);
  public
    constructor CreateTimer(Interval: integer; const TimerFunc: TWSTimerProc; App: QObjectH); virtual;
    destructor Destroy; override;
    procedure AttachEvents; override;
    procedure DetachEvents; override;
    procedure signalTimeout; cdecl;
    property TimerEnabled: Boolean read getTimerEnabled write setTimerEnabled;
    property TimerID: Integer read FId;
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

  { TQtWidgetPalette }

  TQtWidgetPalette = class(TObject)
  private
    procedure initializeSysColors;
    function ColorChangeNeeded(const AColor: TQColor;
      const ATextRole: Boolean): Boolean;
  protected
    FForceColor: Boolean;
    FInReload: Boolean;
    FWidget: QWidgetH;
    FWidgetRole: QPaletteColorRole;
    FTextRole: QPaletteColorRole;
    FDefaultColor: TQColor;
    FCurrentColor: TQColor;
    FDefaultTextColor: TQColor;
    FCurrentTextColor: TQColor;
    FDisabledColor: TQColor;
    FDisabledTextColor: TQColor;
    FHandle: QPaletteH;
  public
    constructor Create(AWidgetColorRole: QPaletteColorRole;
      AWidgetTextColorRole: QPaletteColorRole; AWidget: QWidgetH);
    destructor Destroy; override;
    procedure ReloadPaletteBegin; // used in QEventPaletteChange !
    procedure ReloadPaletteEnd; // used in QEventPaletteChange !
    procedure setColor(const AColor: PQColor); overload;
    procedure setTextColor(const AColor: PQColor);
    property Handle: QPaletteH read FHandle;
    property CurrentColor: TQColor read FCurrentColor;
    property CurrentTextColor: TQColor read FCurrentTextColor;
    property DefaultColor: TQColor read FDefaultColor;
    property DefaultTextColor: TQColor read FDefaultTextColor;
    property DisabledColor: TQColor read FDisabledColor;
    property DisabledTextColor: TQColor read FDisabledTextColor;
    property InReload: Boolean read FInReload;
    property ForceColor: Boolean read FForceColor write FForceColor;
  end;

  {TQtObjectDump}

  TQtObjectDump = class(TObject) // helper class to dump complete children tree
  private
    FRoot: QObjectH;
    FObjList: TFPList;
    FList: TStrings;
    procedure Iterator(ARoot: QObjectH);
    procedure AddToList(AnObject: QObjectH);
  public
    constructor Create(AnObject: QObjectH);
    destructor Destroy; override;
    procedure DumpObject;
    function findWidgetByName(const AName: WideString): QWidgetH;
    function IsWidget(AnObject: QObjectH): Boolean;
    function GetObjectName(AnObject: QObjectH): WideString;
    function InheritsQtClass(AnObject: QObjectH; AQtClass: WideString): Boolean;
    property List: TStrings read FList;
    property ObjList: TFPList read FObjList;
  end;


const
  LCLQt_Destroy = QEventType(Ord(QEventUser) + $1000);
  
procedure TQColorToColorRef(const AColor: TQColor; out AColorRef: TColorRef);
procedure ColorRefToTQColor(const AColorRef: TColorRef; var AColor:TQColor);
function EqualTQColor(const Color1, Color2: TQColor): Boolean;
procedure DebugRegion(const msg: string; Rgn: QRegionH);

function CheckGDIObject(const AGDIObject: HGDIOBJ; const AMethodName: String; AParamName: String = ''): Boolean;
function CheckBitmap(const ABitmap: HBITMAP; const AMethodName: String; AParamName: String = ''): Boolean;

function QtDefaultPrinter: TQtPrinter;
function Clipboard: TQtClipboard;
function QtDefaultContext: TQtDeviceContext;
function QtScreenContext: TQtDeviceContext;

procedure AssignQtFont(FromFont: QFontH; ToFont: QFontH);

implementation

uses
  Controls, qtproc;

const
  ClipbBoardTypeToQtClipboard: array[TClipboardType] of QClipboardMode =
  (
{ctPrimarySelection  } QClipboardSelection,
{ctSecondarySelection} QClipboardSelection,
{ctClipboard         } QClipboardClipboard
  );

const
  Rop2CompSupported: Array[Boolean] of TRop2OrCompositionSupport =
    (rocNotSupported, rocSupported);

const
  SQTWSPrefix = 'TQTWidgetSet.';

{$IFDEF HASX11}
  // defined here to reduce includes (qtint)
  LCLQt_ClipboardPrimarySelection = QEventType(Ord(QEventUser) + $1004);
{$ENDIF}

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
  {$note CheckGDIObject TODO: make TQTImage a TQtResource}
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

function QtDefaultContext: TQtDeviceContext;
begin
  if FDefaultContext = nil then
    FDefaultContext := TQtDeviceContext.Create(nil, False);
  Result := FDefaultContext;
end;

function QtScreenContext: TQtDeviceContext;
begin
  if FScreenContext = nil then
    FScreenContext := TQtDeviceContext.Create(QApplication_desktop(), False);
  Result := FScreenContext;
end;

procedure AssignQtFont(FromFont: QFontH; ToFont: QFontH);
var
  FntFam: WideString;
begin
  QFont_family(FromFont, @FntFam);
  QFont_setFamily(ToFont, @FntFam);
  if QFont_pixelSize(FromFont) > 0 then
    QFont_setPixelSize(ToFont, QFont_pixelSize(FromFont))
  else
    QFont_setPointSize(ToFont, QFont_pointSize(FromFont));
  QFont_setWeight(ToFont, QFont_weight(FromFont));
  QFont_setBold(ToFont, QFont_bold(FromFont));
  QFont_setItalic(ToFont, QFont_italic(FromFont));
  QFont_setUnderline(ToFont, QFont_underline(FromFont));
  QFont_setStrikeOut(ToFont, QFont_strikeOut(FromFont));
  QFont_setStyle(ToFont, QFont_style(FromFont));
  QFont_setStyleStrategy(ToFont, QFont_styleStrategy(FromFont));
end;

{ TQtObject }

constructor TQtObject.Create;
begin
  FDeleteLater := False;
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
    if FDeleteLater then
      QObject_deleteLater(TheObject)
    else
      QObject_destroy(TheObject);
    TheObject := nil;
  end;
  inherited Destroy;
end;

procedure TQtObject.Release;
begin
  if InEvent then
  begin
    FDeleteLater := True;
    FReleaseInEvent := True;
  end else
    Free;
end;

procedure TQtObject.AttachEvents;
begin
  FEventHook := QObject_hook_create(TheObject);
  QObject_hook_hook_events(FEventHook, @EventFilter);
  FDestroyedHook := QObject_hook_create(TheObject);
  QObject_hook_hook_destroyed(FDestroyedHook, @Destroyed);
end;

procedure TQtObject.DetachEvents;
begin
  if FEventHook <> nil then
  begin
    QObject_hook_destroy(FEventHook);
    FEventHook := nil;
  end;
  if FDestroyedHook <> nil then
  begin
    QObject_hook_destroy(FDestroyedHook);
    FDestroyedHook := nil;
  end;
end;

procedure TQtObject.Destroyed; cdecl;
begin
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
  begin
    Handle := QImage_create(width, height, format);
    QImage_fill(Handle, 0);
  end
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

function TQtImage.depth: Integer;
begin
  Result := QImage_depth(Handle);
end;

function TQtImage.dotsPerMeterX: Integer;
begin
  Result := QImage_dotsPerMeterX(Handle);
end;

function TQtImage.dotsPerMeterY: Integer;
begin
  Result := QImage_dotsPerMeterY(Handle);
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

function TQtImage.bytesPerLine: Integer;
begin
  Result := QImage_bytesPerLine(Handle);
end;

procedure TQtImage.invertPixels(InvertMode: QImageInvertMode = QImageInvertRgb);
begin
  QImage_invertPixels(Handle, InvertMode);
end;

function TQtImage.getFormat: QImageFormat;
begin
  Result := QImage_format(Handle);
end;

{ TQtFont }

function TQtFont.GetMetrics: TQtFontMetrics;
begin
  if FMetrics = nil then
  begin
    if Widget = nil then
      FMetrics := TQtFontMetrics.Create(getDefaultFont)
    else
      FMetrics := TQtFontMetrics.Create(Widget);
  end;
  Result := FMetrics;
end;

{------------------------------------------------------------------------------
  Function: TQtFont.GetDefaultFont
  Params:  None
  Returns: QFontH
  If our Widget is nil then we have to ask for default application font.
 ------------------------------------------------------------------------------}
function TQtFont.GetDefaultFont: QFontH;
begin
  if FDefaultFont = nil then
  begin
    FDefaultFont := QFont_create();
    QApplication_font(FDefaultFont);
  end;
  Result := FDefaultFont;
end;

{------------------------------------------------------------------------------
  Function: TQtFont.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtFont.Create(CreateHandle: Boolean);
begin
  {$ifdef VerboseQt}
    WriteLn('TQtFont.Create CreateHandle: ', dbgs(CreateHandle));
  {$endif}

  if CreateHandle then
    Widget := QFont_create
  else
    Widget := nil;
  
  FShared := False;
  FMetrics := nil;
  FDefaultFont := nil;
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

  if FMetrics <> nil then
    FMetrics.Free;

  if not FShared and (Widget <> nil) then
    QFont_destroy(Widget);
    
  if FDefaultFont <> nil then
    QFont_destroy(FDefaultFont);

  inherited Destroy;
end;

function TQtFont.getPointSize: Integer;
begin
  if Widget = nil then
    Result := QFont_pointSize(getDefaultFont)
  else
    Result := QFont_pointSize(Widget);
end;

procedure TQtFont.setPointSize(p1: Integer);
begin
  if p1 > 0 then
    QFont_setPointSize(Widget, p1);
end;

function TQtFont.getPixelSize: Integer;
begin
  if Widget = nil then
    Result := QFont_pixelSize(getDefaultFont)
  else
    Result := QFont_pixelSize(Widget);
end;

procedure TQtFont.setPixelSize(p1: Integer);
begin
  if p1 > 0 then
    QFont_setPixelSize(Widget, p1);
end;

function TQtFont.getWeight: Integer;
begin
  if Widget = nil then
    Result := QFont_weight(getDefaultFont)
  else
    Result := QFont_weight(Widget);
end;

function TQtFont.getItalic: Boolean;
begin
  if Widget = nil then
    Result := QFont_italic(getDefaultFont)
  else
    Result := QFont_italic(Widget);
end;

function TQtFont.getBold: Boolean;
begin
  if Widget = nil then
    Result := QFont_bold(getDefaultFont)
  else
    Result := QFont_bold(Widget);
end;

function TQtFont.getUnderline: Boolean;
begin
  if Widget = nil then
    Result := QFont_underline(getDefaultFont)
  else
    Result := QFont_underline(Widget);
end;

function TQtFont.getStrikeOut: Boolean;
begin
  if Widget = nil then
    Result := QFont_strikeOut(getDefaultFont)
  else
    Result := QFont_strikeOut(Widget);
end;

function TQtFont.getFamily: WideString;
begin
  if Widget = nil then
    QFont_family(getDefaultFont, @Result)
  else
    QFont_family(Widget, @Result);
end;

function TQtFont.getStyleStategy: QFontStyleStrategy;
begin
  if Widget = nil then
    Result := QFont_styleStrategy(getDefaultFont)
  else
    Result := QFont_styleStrategy(Widget);
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

procedure TQtFont.setStyleStrategy(s: QFontStyleStrategy);
begin
  QFont_setStyleStrategy(Widget, s);
end;

procedure TQtFont.family(retval: PWideString);
begin
  if Widget = nil then
    QFont_family(getDefaultFont, retval)
  else
    QFont_family(Widget, retval);
end;

function TQtFont.fixedPitch: Boolean;
begin
  if Widget = nil then
    Result := QFont_fixedPitch(getDefaultFont)
  else
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

function TQtFontMetrics.width(p1: PWideString; ALen: Integer): Integer;
begin
  Result := QFontMetrics_width(Widget, p1, ALen);
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

function TQtFontMetrics.averageCharWidth: Integer;
begin
  Result := QFontMetrics_averageCharWidth(Widget);
end;

function TQtFontMetrics.elidedText(const AText: WideString;
  const AMode: QtTextElideMode; const AWidth: Integer;
  const AFlags: Integer = 0): WideString;
begin
  QFontMetrics_elidedText(Widget, @Result, @AText, AMode, AWidth, AFlags);
end;

{ TQtBrush }

{------------------------------------------------------------------------------
  Function: TQtBrush.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtBrush.Create(CreateHandle: Boolean);
begin
  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtBrush.Create CreateHandle: ', dbgs(CreateHandle));
  {$endif}

  if CreateHandle then
    Widget := QBrush_create
  else
    Widget := nil;
  
  FShared := False;
  FSelected := False;
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

  if not FShared and (Widget <> nil) and not FSelected then
    QBrush_destroy(Widget);

  inherited Destroy;
end;

function TQtBrush.getColor: PQColor;
begin
  Result := QBrush_Color(Widget);
end;

function TQtBrush.GetLBStyle(out AStyle: LongWord; out AHatch: PtrUInt
  ): Boolean;
begin
  Result := Widget <> nil;
  if not Result then
    exit;

  AStyle := BS_SOLID;
  if Style in [QtHorPattern, QtVerPattern, QtCrossPattern,
                    QtBDiagPattern, QtFDiagPattern, QtDiagCrossPattern] then
    AStyle := BS_HATCHED
  else
    AHatch := 0;
  case Style of
    QtNoBrush: AStyle := BS_NULL;
    QtHorPattern: AHatch := HS_HORIZONTAL;
    QtVerPattern: AHatch := HS_VERTICAL;
    QtCrossPattern: AHatch := HS_CROSS;
    QtBDiagPattern: AHatch := HS_BDIAGONAL;
    QtFDiagPattern: AHatch := HS_FDIAGONAL;
    QtDiagCrossPattern: AHatch := HS_DIAGCROSS;
    QtTexturePattern: AStyle := BS_PATTERN;
  end;
end;

procedure TQtBrush.setColor(AColor: PQColor);
begin
  QBrush_setColor(Widget, AColor);
end;

function TQtBrush.getStyle: QtBrushStyle;
begin
  Result := QBrush_style(Widget);
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
constructor TQtPen.Create(CreateHandle: Boolean);
begin
  {$ifdef VerboseQt}
    WriteLn('TQtPen.Create CreateHandle: ', dbgs(CreateHandle));
  {$endif}
  
  if CreateHandle then
    Widget := QPen_create
  else
    Widget := nil;
  FShared := False;
  FIsExtPen := False;
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

function TQtPen.getCapStyle: QtPenCapStyle;
begin
  Result := QPen_capStyle(Widget);
end;

function TQtPen.getWidth: Integer;
begin
  Result := QPen_width(Widget);
end;

function TQtPen.getStyle: QtPenStyle;
begin
  Result := QPen_style(Widget);
end;

function TQtPen.getDashPattern: TQRealArray;
begin
  QPen_dashPattern(Widget, @Result);
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

procedure TQtPen.setDashPattern(APattern: PDWord; ALength: DWord);
var
  QtPattern: TQRealArray;
  i: integer;
begin
  SetLength(QtPattern, ALength);
  for i := 0 to ALength - 1 do
    QtPattern[i] := APattern[i];
  QPen_setDashPattern(Widget, @QtPattern);
end;

procedure TQtPen.setJoinStyle(pcs: QtPenJoinStyle);
begin
  QPen_setJoinStyle(Widget, pcs);
end;

function TQtPen.getColor: TQColor;
begin
  QPen_color(Widget, @Result);
end;

function TQtPen.getCosmetic: Boolean;
begin
  Result := QPen_isCosmetic(Widget);
end;

function TQtPen.getJoinStyle: QtPenJoinStyle;
begin
  Result := QPen_joinStyle(Widget);
end;

procedure TQtPen.setCapStyle(pcs: QtPenCapStyle);
begin
  QPen_setCapStyle(Widget, pcs);
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

procedure TQtPen.setCosmetic(b: Boolean);
begin
  QPen_setCosmetic(Widget, b);
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
  FPolygon := nil;
  // Creates the widget
  if CreateHandle then
    Widget := QRegion_create()
  else
    Widget := nil;
end;

{------------------------------------------------------------------------------
  Function: TQtRegion.Create (CreateRectRgn)
  Params:  CreateHandle: Boolean; X1,Y1,X2,Y2:Integer
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtRegion.Create(CreateHandle: Boolean; X1,Y1,X2,Y2:Integer;
  Const RegionType: QRegionRegionType = QRegionRectangle);
var
  W, H: Integer;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtRegion.Create CreateHandle: ', dbgs(CreateHandle));
  {$endif}
  FPolygon := nil;
  // Creates the widget
  // Note that QRegion_create expects a width and a height,
  // and not a X2, Y2 bottom-right point
  if CreateHandle then
  begin
    W := X2 - X1;
    H := Y2 - Y1;
    if W < 0 then
      W := 0;
    if H < 0 then
      H := 0;
    Widget := QRegion_create(X1, Y1, W, H, RegionType);
  end else
    Widget := nil;
end;

constructor TQtRegion.Create(CreateHandle: Boolean; Poly: QPolygonH;
  Const Fill: QtFillRule = QtWindingFill);
begin
  {$ifdef VerboseQt}
    WriteLn('TQtRegion.Create polyrgn CreateHandle: ', dbgs(CreateHandle));
  {$endif}
  FPolygon := nil;
  if CreateHandle then
  begin
    FPolygon := QPolygon_create(Poly);
    Widget := QRegion_create(FPolygon, Fill);
  end else
    Widget := nil;
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
  if FPolygon <> nil then
    QPolygon_destroy(FPolygon);
  if Widget <> nil then
    QRegion_destroy(Widget);

  inherited Destroy;
end;

function TQtRegion.GetIsPolyRegion: Boolean;
begin
  Result := FPolygon <> nil;
end;

function TQtRegion.containsPoint(X, Y: Integer): Boolean;
var
  P: TQtPoint;
begin
  P.X := X;
  P.Y := Y;
  Result := QRegion_contains(Widget, PQtPoint(@P));
end;

function TQtRegion.containsRect(R: TRect): Boolean;
begin
  Result := QRegion_contains(Widget, PRect(@R));
end;

function TQtRegion.intersects(R: TRect): Boolean;
begin
  Result := QRegion_intersects(Widget, PRect(@R));
end;

function TQtRegion.intersects(Rgn: QRegionH): Boolean;
begin
  Result := QRegion_intersects(Widget, Rgn);
end;

function TQtRegion.GetRegionType: integer;
begin
  try
    if not IsPolyRegion and QRegion_isEmpty(Widget) then
      Result := NULLREGION
    else
    begin
      if IsPolyRegion or (QRegion_numRects(Widget) > 1) then
        Result := COMPLEXREGION
      else
        Result := SIMPLEREGION;
    end;
  except
    Result := ERROR;
  end;
end;

function TQtRegion.getBoundingRect: TRect;
begin
  if IsPolyRegion then
    QPolygon_boundingRect(FPolygon, @Result)
  else
    QRegion_boundingRect(Widget, @Result);
end;

function TQtRegion.numRects: Integer;
begin
  Result := QRegion_numRects(Widget);
end;

procedure TQtRegion.translate(dx, dy: Integer);
begin
  QRegion_translate(Widget, dx, dy);
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
  Parent := nil;
  ParentPixmap := nil;
  FMetrics := nil;

  if AWidget = nil then
  begin
    ParentPixmap := QPixmap_Create(10, 10);
    Widget := QPainter_Create(QPaintDeviceH(ParentPixmap));
  end else
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
    end else
      Widget := QPainter_create(QWidget_to_QPaintDevice(Parent));
  end;

  FRopMode := R2_COPYPEN;
  FOwnPainter := True;
  CreateObjects;
  FPenPos.X := 0;
  FPenPos.Y := 0;
end;

constructor TQtDeviceContext.CreatePrinterContext(ADevice: QPrinterH);
begin
  FMetrics := nil;
  Parent := nil;
  Widget := QPainter_Create(ADevice);
  FRopMode := R2_COPYPEN;
  FOwnPainter := True;
  CreateObjects;
  FPenPos.X := 0;
  FPenPos.Y := 0;
end;

constructor TQtDeviceContext.CreateFromPainter(APainter: QPainterH);
begin
  FMetrics := nil;
  FRopMode := R2_COPYPEN;
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

  if FMetrics <> nil then
    FreeThenNil(FMetrics);

  DestroyObjects;

  if (Widget <> nil) and FOwnPainter then
  begin
    QPainter_destroy(Widget);
    Widget := nil;
  end;

  if ParentPixmap <> nil then
  begin
    QPixmap_destroy(ParentPixmap);
    ParentPixmap := nil;
  end;

  inherited Destroy;
end;

procedure TQtDeviceContext.CreateObjects;
begin
  FSupportComposition := rocUndefined;
  FSupportRasterOps := rocUndefined;

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

  vTextColor := ColorToRGB(clWindowText);

  vMapMode := MM_TEXT;
end;

procedure TQtDeviceContext.DestroyObjects;
begin
  // vFont creates widget and copies font into it => we should destroy it
  //vFont.Widget := nil;
  FreeAndNil(vFont);
  //WriteLn('Destroying brush: ', PtrUInt(vBrush), ' ', ClassName, ' ', PtrUInt(Self));
  vBrush.Widget := nil;
  FreeAndNil(vBrush);
  vPen.Widget := nil;
  FreeAndNil(vPen);
  if vRegion.Widget <> nil then
  begin
    QRegion_destroy(vRegion.Widget);
    vRegion.Widget := nil;
  end;
  FreeAndNil(vRegion);
  vBackgroundBrush.Widget := nil;
  FreeAndNil(vBackgroundBrush);
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
  vImage := AImage;
  
  QPainter_destroy(Widget);

  Widget := QPainter_Create(vImage.Handle);
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

function TQtDeviceContext.GetLineLastPixelPos(PrevPos, NewPos: TPoint): TPoint;
begin
  Result := NewPos;

  if NewPos.X > PrevPos.X then
    dec(Result.X)
  else
  if NewPos.X < PrevPos.X then
    inc(Result.X);

  if NewPos.Y > PrevPos.Y then
    dec(Result.Y)
  else
  if NewPos.Y < PrevPos.Y then
    inc(Result.Y);
end;

procedure TQtDeviceContext.qDrawPlainRect(x, y, w, h: integer; AColor: PQColor = nil;
  lineWidth: Integer = 1; FillBrush: QBrushH = nil);
begin
  if AColor = nil then
    AColor := BackgroundBrush.getColor;
  // stop asserts from qtlib
  if (w < x) or (h < y) then
    exit;
  q_DrawPlainRect(Widget, x, y, w, h, AColor, lineWidth, FillBrush);
end;

procedure TQtDeviceContext.qDrawShadeRect(x, y, w, h: integer; Palette: QPaletteH = nil; Sunken: Boolean = False;
  lineWidth: Integer = 1; midLineWidth: Integer = 0; FillBrush: QBrushH = nil);
var
  AppPalette: QPaletteH;
begin
  if (w < 0) or (h < 0) then
    exit;
  AppPalette := nil;
  if Palette = nil then
  begin
    if Parent = nil then
    begin
      AppPalette := QPalette_create();
      QApplication_palette(AppPalette);
      Palette := AppPalette;
    end else
      Palette := QWidget_palette(Parent);
  end;
  q_DrawShadeRect(Widget, x, y, w, h, Palette, Sunken, lineWidth, midLineWidth, FillBrush);
  if AppPalette <> nil then
  begin
    QPalette_destroy(AppPalette);
    Palette := nil;
  end;
end;

procedure TQtDeviceContext.qDrawWinPanel(x, y, w, h: integer;
  Palette: QPaletteH; Sunken: Boolean; lineWidth: Integer; FillBrush: QBrushH);
var
  i: integer;
  AppPalette: QPaletteH;
begin

  if (w < 0) or (h < 0) then
    exit;

  AppPalette := nil;
  if Palette = nil then
  begin
    if Parent = nil then
    begin
      AppPalette := QPalette_create();
      QApplication_palette(AppPalette);
      Palette := AppPalette;
    end else
      Palette := QWidget_palette(Parent);
  end;
  // since q_DrawWinPanel doesnot supports lineWidth we should do it ourself
  for i := 1 to lineWidth - 2 do
  begin
    q_DrawWinPanel(Widget, x, y, w, h, Palette, Sunken);
    inc(x);
    inc(y);
    dec(w, 2);
    dec(h, 2);
  end;

  if lineWidth > 1 then
    q_DrawWinPanel(Widget, x, y, w, h, Palette, Sunken, FillBrush)
  else
  begin
    if FillBrush = nil then
      q_DrawShadePanel(Widget, x, y, w, h, Palette, Sunken, 1, QPalette_background(Palette))
    else
      q_DrawShadePanel(Widget, x, y, w, h, Palette, Sunken, 1, FillBrush);
  end;
  if AppPalette <> nil then
  begin
    QPalette_destroy(AppPalette);
    Palette := nil;
  end;
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
  QPainter_save(Widget);
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

function TQtDeviceContext.DeviceSupportsComposition: Boolean;
var
  Engine: QPaintEngineH;
  AType: QPaintEngineType;
begin

  Result := (Widget <> nil) and QPainter_isActive(Widget);

  if not Result then
    exit;

  Result := FSupportComposition = rocSupported;

  if (FSupportComposition <> rocUndefined) then
    exit;

  Engine := QPainter_paintEngine(Widget);

  if Engine <> nil then
  begin
    AType := QPaintEngine_type(Engine);
    Result := not (AType in
      [QPaintEngineX11, QPaintEngineWindows,
       QPaintEngineQuickDraw, QPaintEngineCoreGraphics,
       QPaintEngineQWindowSystem]);

    FSupportComposition := Rop2CompSupported[Result];
  end;
end;

function TQtDeviceContext.DeviceSupportsRasterOps: Boolean;
var
  Engine: QPaintEngineH;
  AType: QPaintEngineType;
begin

  Result := (Widget <> nil) and QPainter_isActive(Widget);

  if not Result then
    exit;

  Result := FSupportRasterOps = rocSupported;

  if (FSupportRasterOps <> rocUndefined) then
    exit;

  Engine := QPainter_paintEngine(Widget);
  if Engine <> nil then
  begin
    AType := QPaintEngine_type(Engine);
    Result := not (AType in
      [QPaintEngineQuickDraw, QPaintEngineCoreGraphics,
       QPaintEngineQWindowSystem]);

    FSupportRasterOps := Rop2CompSupported[Result];
  end;
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.R2ToQtRasterOp
  Params:  Raster ops binary operator
  Returns: QPainterCompositionMode
 ------------------------------------------------------------------------------}
function TQtDeviceContext.R2ToQtRasterOp(AValue: Integer): QPainterCompositionMode;
begin
  Result := QPainterCompositionMode_SourceOver;

  if not DeviceSupportsRasterOps then
    exit;

  case AValue of

    R2_BLACK: if DeviceSupportsComposition then
                Result := QPainterCompositionMode_Clear;

    R2_COPYPEN: Result := QPainterCompositionMode_SourceOver; // default
    R2_MASKNOTPEN: Result := QPainterRasterOp_NotSourceAndDestination;
    R2_MASKPEN: Result := QPainterRasterOp_SourceAndDestination;
    R2_MASKPENNOT: Result := QPainterRasterOp_SourceAndNotDestination;
    R2_MERGENOTPEN: Result := QPainterCompositionMode_SourceOver; // unsupported
    R2_MERGEPEN: Result := QPainterRasterOp_SourceOrDestination;
    R2_MERGEPENNOT: Result := QPainterCompositionMode_SourceOver; // unsupported

    R2_NOP: if DeviceSupportsComposition then
              Result := QPainterCompositionMode_Destination;
    R2_NOT: if DeviceSupportsComposition then
              Result := QPainterCompositionMode_SourceOut; // unsupported

    R2_NOTCOPYPEN: Result := QPainterRasterOp_NotSource;
    R2_NOTMASKPEN: Result := QPainterRasterOp_NotSourceOrNotDestination;
    R2_NOTMERGEPEN: Result := QPainterRasterOp_NotSourceAndNotDestination;
    R2_NOTXORPEN: Result := QPainterRasterOp_NotSourceXorDestination;

    R2_WHITE: if DeviceSupportsComposition then
                Result := QPainterCompositionMode_Screen;

    R2_XORPEN: Result := QPainterRasterOp_SourceXorDestination;
  end;
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

function TQtDeviceContext.GetRop: Integer;
begin
  Result := FRopMode;
end;

function TQtDeviceContext.GetMetrics: TQtFontMetrics;
begin
  Result := Font.Metrics;
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
  QPainter_setPen(Widget, @txtColor);
end;

procedure TQtDeviceContext.SetRop(const AValue: Integer);
var
  QtROPMode: QPainterCompositionMode;
begin
  FRopMode := AValue;
  QtRopMode := R2ToQtRasterOp(AValue);
  if QPainter_compositionMode(Widget) <> QtRopMode then
    QPainter_setCompositionMode(Widget, QtROPMode);
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
  QPainter_drawRoundedRect(Widget, x, y, w, h, rx, ry);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.drawText
  Params:  None
  Returns: Nothing

  Draws a Text. Helper function of winapi.TextOut
  
  Qt does not draw the text starting at Y position and downwards, like LCL.

  Instead, Y becomes the baseline for the text and it's drawn upwards.
  
  To get a correct behavior we need to sum the text's height to the Y coordinate.
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.drawText(x: Integer; y: Integer; s: PWideString);
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.drawText TargetX: ', X, ' TargetY: ', Y);
  {$endif}

  // First translate and then rotate, that makes the
  // correct rotation effect that we want
  if Font.Angle <> 0 then
  begin
    Translate(x, y);
    Rotate(-0.1 * Font.Angle);
  end;

  // what about Metrics.descent and Metrics.leading ?
  y := y + Metrics.ascent;

  RestoreTextColor;

  // The ascent is only applied here, because it also needs
  // to be rotated
  if Font.Angle <> 0 then
    QPainter_drawText(Widget, 0, Metrics.ascent, s)
  else
    QPainter_drawText(Widget, x, y, s);
  
  RestorePenColor;
  
  // Restore previous angle
  if Font.Angle <> 0 then
  begin
    y := y - Metrics.ascent;
    Rotate(0.1 * Font.Angle);
    Translate(-x, -y);
  end;

  {$ifdef VerboseQt}
  WriteLn(' Font metrics height: ', Metrics.height, ' Angle: ',
    Round(0.1 * Font.Angle));
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

  // First translate and then rotate, that makes the
  // correct rotation effect that we want
  if Font.Angle <> 0 then
  begin
    Translate(x, y);
    Rotate(-0.1 * Font.Angle);
  end;

  RestoreTextColor;
  if Font.Angle <> 0 then
    QPainter_DrawText(Widget, 0, 0, w, h, Flags, s)
  else
    QPainter_DrawText(Widget, x, y, w, h, Flags, s);
  RestorePenColor;

  // Restore previous angle
  if Font.Angle <> 0 then
  begin
    Rotate(0.1 * Font.Angle);
    Translate(-x, -y);
  end;
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

procedure TQtDeviceContext.drawPolyLine(P: PPoint; NumPts: Integer);
var
  QtPoints: PQtPoint;
  i: integer;
  LastPoint: TPoint;
begin
  GetMem(QtPoints, NumPts * SizeOf(TQtPoint));
  for i := 0 to NumPts - 2 do
    QtPoints[i] := QtPoint(P[i].x, P[i].y);

  LastPoint := P[NumPts - 1];
  if NumPts > 1 then
    LastPoint := GetLineLastPixelPos(P[NumPts - 2], LastPoint);
  QtPoints[NumPts - 1] := QtPoint(LastPoint.X, LastPoint.Y);

  QPainter_drawPolyline(Widget, QtPoints, NumPts);
  FreeMem(QtPoints);
end;

procedure TQtDeviceContext.drawPolygon(P: PPoint; NumPts: Integer;
  FillRule: QtFillRule);
var
  QtPoints: PQtPoint;
  i: integer;
  LastPoint: TPoint;
begin
  GetMem(QtPoints, NumPts * SizeOf(TQtPoint));
  for i := 0 to NumPts - 2 do
    QtPoints[i] := QtPoint(P[i].x, P[i].y);

  LastPoint := P[NumPts - 1];
  if NumPts > 1 then
    LastPoint := GetLineLastPixelPos(P[NumPts - 2], LastPoint);
  QtPoints[NumPts - 1] := QtPoint(LastPoint.X, LastPoint.Y);

  QPainter_drawPolygon(Widget, QtPoints, NumPts, FillRule);
  FreeMem(QtPoints);
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

function TQtDeviceContext.getCompositionMode: QPainterCompositionMode;
begin
  Result := QPainter_compositionMode(Widget);
end;

procedure TQtDeviceContext.getPenPos(retval: PPoint);
begin
  retval^.x := FPenPos.x;
  retval^.y := FPenPos.y;
end;

function TQtDeviceContext.getWorldTransform: QTransformH;
begin
  Result := QPainter_worldTransform(Widget);
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

  if SelFont = nil then
  begin
    if vFont <> nil then
    begin
      if vFont.Widget <> nil then
        QFont_destroy(vFont.Widget);
    end;
    Result := vFont;
  end
  else
    Result := SelFont;
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.setFont
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.setFont(AFont: TQtFont);
var
  QFnt: QFontH;
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.setFont() ');
  {$endif}
  SelFont := AFont;
  if (AFont.Widget <> nil) and (Widget <> nil) then
  begin
    if not FOwnPainter then
    begin
      QFnt := QPainter_font(Widget);
      AssignQtFont(AFont.Widget, QFnt);
    end else
      QPainter_setFont(Widget, QFontH(AFont.Widget));
    vFont.Angle := AFont.Angle;
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
    
  if SelBrush = nil then
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
  if SelBrush <> nil then
    SelBrush.FSelected := False;
  SelBrush := ABrush;
  if SelBrush <> nil then
    SelBrush.FSelected := True;
    
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

function TQtDeviceContext.GetBkColor: TColorRef;
var
  TheBrush: QBrushH;
  TheColor: TQColor;
begin
  TheBrush := QPainter_background(Widget);
  TheColor := QBrush_color(TheBrush)^;
  TQColorToColorRef(TheColor, Result);
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
    
  if SelPen = nil then
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
  AColorRef := ((AColor.r shr 8) and $FF) or
                (AColor.g and $FF00) or
               ((AColor.b shl 8) and $FF0000);
end;

procedure ColorRefToTQColor(const AColorRef: TColorRef; var AColor:TQColor);
begin
  QColor_fromRgb(@AColor, Red(AColorRef),Green(AColorRef),Blue(AColorRef));
end;

function EqualTQColor(const Color1, Color2: TQColor): Boolean;
begin
  Result := (Color1.r = Color2.r) and
    (Color1.g = Color2.g) and
    (Color1.b = Color2.b);
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

function TQtDeviceContext.SetBkColor(Color: TColorRef): TColorRef;
var
  NColor: TQColor;
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.setBKColor() ');
  {$endif}
  Result := GetBkColor;
  ColorRefToTQColor(ColorToRGB(TColor(Color)), NColor);
  BackgroundBrush.setColor(@NColor);
end;

function TQtDeviceContext.SetBkMode(BkMode: Integer): Integer;
var
  Mode: QtBGMode;
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.setBKMode() ');
  {$endif}
  Result := 0;
  if Widget <> nil then
  begin
    Mode := QPainter_BackgroundMode(Widget);
    if Mode = QtOpaqueMode then
      Result := OPAQUE
    else
      Result := TRANSPARENT;

    if BkMode = OPAQUE then
      Mode := QtOpaqueMode
    else
      Mode := QtTransparentMode;
    QPainter_SetBackgroundMode(Widget, Mode);
  end;
end;

function TQtDeviceContext.getDepth: integer;
var
  device: QPaintDeviceH;
begin
  device := QPainter_device(Widget);
  Result := QPaintDevice_depth(Device);
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
begin
  try
    if QRegion_isEmpty(ARegion) then
      Result := NULLREGION
    else
    begin
      if QRegion_numRects(ARegion) = 1 then
        Result := SIMPLEREGION
      else
        Result := COMPLEXREGION;
    end;
  except
    Result := ERROR;
  end;
end;

procedure TQtDeviceContext.setCompositionMode(mode: QPainterCompositionMode);
begin
  QPainter_setCompositionMode(Widget, mode);
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
  if vRegion.Widget <> nil then
  begin
    QRegion_destroy(vRegion.Widget);
    vRegion.Widget := nil;
  end;
  if vRegion.Widget = nil then
    vRegion.Widget := QRegion_Create();

  QPainter_clipRegion(Widget,  vRegion.Widget);
  Result := vRegion;
end;

procedure TQtDeviceContext.setClipping(const AValue: Boolean);
begin
  QPainter_setClipping(Widget, AValue);
end;

procedure TQtDeviceContext.setClipRect(const ARect: TRect);
begin
  QPainter_setClipRect(Widget, @ARect);
end;

procedure TQtDeviceContext.setClipRegion(ARegion: QRegionH;
  AOperation: QtClipOperation = QtReplaceClip);
begin
  {with QPaintEngine11 QtNoClip & empty region makes disaster}
  if (AOperation = QtNoClip) and QRegion_isEmpty(ARegion)
     and (QPaintEngine_type(PaintEngine) = QPaintEngineX11) then
    setClipping(False)
  else
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
  ScaledImage: QImageH;
  ScaledMask: QImageH;
  NewRect: TRect;

  function NeedScaling: boolean;
  var
    R: TRect;
    TgtW, TgtH,
    ClpW, ClpH: integer;
  begin

    if not getClipping or EqualRect(LocalRect, sourceRect^) then
      exit(False);

    R := getClipRegion.getBoundingRect;

    TgtW := LocalRect.Right - LocalRect.Left;
    TgtH := LocalRect.Right - LocalRect.Left;
    ClpW := R.Right - R.Left;
    ClpH := R.Bottom - R.Top;

    Result := PtInRect(R, Point(R.Left + 1, R.Top + 1)) and
      (ClpW <= TgtW) and (ClpH <= TgtH);
  end;

begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.drawImage() ');
  {$endif}
  ScaledImage := nil;
  LocalRect := targetRect^;

  if mask <> nil then
  begin
    if NeedScaling then
    begin
      ScaledImage := QImage_create();
      QImage_copy(Image, ScaledImage, 0, 0, QImage_width(Image), QImage_height(Image));
      QImage_scaled(ScaledImage, ScaledImage, LocalRect.Right - LocalRect.Left,
            LocalRect.Bottom - LocalRect.Top);
      NewRect := sourceRect^;
      NewRect.Right := (LocalRect.Right - LocalRect.Left) + sourceRect^.Left;
      NewRect.Bottom := (LocalRect.Bottom - LocalRect.Top) + sourceRect^.Top;
    end;
    // TODO: check maskRect
    APixmap := QPixmap_create();
    try
      if ScaledImage <> nil then
        QPixmap_fromImage(APixmap, ScaledImage, flags)
      else
        QPixmap_fromImage(APixmap, image, flags);
      ATemp := QPixmap_create();
      try
        // QBitmap_fromImage raises assertion in the qt library
        if ScaledImage <> nil then
        begin
          ScaledMask := QImage_create();
          QImage_copy(Mask, ScaledMask, 0, 0, QImage_width(Mask), QImage_height(Mask));
          QImage_scaled(ScaledMask, ScaledMask, LocalRect.Right - LocalRect.Left,
              LocalRect.Bottom - LocalRect.Top);
          QPixmap_fromImage(ATemp, ScaledMask, flags);
          QImage_destroy(ScaledMask);
        end else
          QPixmap_fromImage(ATemp, mask, flags);
        AMask := QBitmap_create(ATemp);
        try
          QPixmap_setMask(APixmap, AMask);
          if ScaledImage <> nil then
            QPainter_drawPixmap(Widget, PRect(@LocalRect), APixmap, @NewRect)
          else
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
    if ScaledImage <> nil then
      QImage_destroy(ScaledImage);
  end else
  begin
    {$note TQtDeviceContext.drawImage workaround - possible qt4 bug with QPainter & RGB32 images.}
    {Workaround: we must convert image to ARGB32 , since we can get strange
     results with RGB32 images on Linux and Win32 if DstRect <> sourceRect.
     Explanation: Look at #11713 linux & win screenshoots.
     Note: This is slower operation than QImage_scaled() we used before.}
    if not EqualRect(LocalRect, sourceRect^) and
      (QImage_format(Image) = QImageFormat_RGB32) then
    begin

      ScaledImage := QImage_create();
      try

        QImage_convertToFormat(Image, ScaledImage, QImageFormat_ARGB32);

        if NeedScaling then
        begin
          QImage_scaled(ScaledImage, ScaledImage, LocalRect.Right - LocalRect.Left,
          LocalRect.Bottom - LocalRect.Top);

          NewRect := sourceRect^;
          NewRect.Right := (LocalRect.Right - LocalRect.Left) + sourceRect^.Left;
          NewRect.Bottom := (LocalRect.Bottom - LocalRect.Top) + sourceRect^.Top;

          QPainter_drawImage(Widget, PRect(@LocalRect), ScaledImage, @NewRect, flags);
        end else
          QPainter_drawImage(Widget, PRect(@LocalRect), ScaledImage, sourceRect, flags);

      finally
        QImage_destroy(ScaledImage);
      end;

    end else
    begin
      if NeedScaling then
      begin
        ScaledImage := QImage_create();
        try
          QImage_copy(Image, ScaledImage, 0, 0, QImage_width(Image), QImage_height(Image));
          QImage_scaled(ScaledImage, ScaledImage, LocalRect.Right - LocalRect.Left,
            LocalRect.Bottom - LocalRect.Top);
          NewRect := sourceRect^;
          NewRect.Right := (LocalRect.Right - LocalRect.Left) + sourceRect^.Left;
          NewRect.Bottom := (LocalRect.Bottom - LocalRect.Top) + sourceRect^.Top;
          QPainter_drawImage(Widget, PRect(@LocalRect), ScaledImage, @NewRect, flags);
        finally
          QImage_destroy(ScaledImage);
        end;
      end else
        QPainter_drawImage(Widget, PRect(@LocalRect), image, sourceRect, flags);
    end;
  end;
end;

function TQtDeviceContext.PaintEngine: QPaintEngineH;
begin
  Result := QPainter_paintEngine(Widget);
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

procedure TQtDeviceContext.setRenderHint(AHint: QPainterRenderHint; AValue: Boolean);
begin
  QPainter_setRenderHint(Widget, AHint, AValue);
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

  if vIcon <> nil then
    Handle := QSystemTrayIcon_create(vicon, nil)
  else
    Handle := QSystemTrayIcon_create();
  FHook := QSystemTrayIcon_hook_create(Handle);
  QSystemTrayIcon_hook_hook_activated(FHook, @signalActivated);
end;

destructor TQtSystemTrayIcon.Destroy;
begin
  QSystemTrayIcon_hook_destroy(FHook);
  QSystemTrayIcon_destroy(Handle);
  
  inherited Destroy;
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

procedure TQtSystemTrayIcon.signalActivated(
  AReason: QSystemTrayIconActivationReason); cdecl;
var
  MousePos: TQtPoint;
begin
  if not Assigned(FTrayIcon) then
    exit;

  QCursor_pos(@MousePos);
  {$note: TODO: Mouse events of trayicon can be catched
   in QApplication event filter (TQtWidgetSet.EventFilter),
   so OnMouseDown and OnMouseUp can be properly sent.
   Check if it works ok on qtwin32 and qtmac and
   then replace this blind calls to mouse events.
   To get systryicon object handle in application event filter
   add property "lclsystrayicon" to this handle.}
  case AReason of
    QSystemTrayIconTrigger:
      begin
        if Assigned(FTrayIcon.OnMouseDown) then
          FTrayIcon.OnMouseDown(FTrayIcon, mbLeft, [], MousePos.x, MousePos.y);
        if Assigned(FTrayIcon.OnClick) then
          FTrayIcon.OnClick(FTrayIcon);
        if Assigned(FTrayIcon.OnMouseUp) then
          FTrayIcon.OnMouseUp(FTrayIcon, mbLeft, [], MousePos.x, MousePos.y);
      end;
    QSystemTrayIconDoubleClick:
      begin
        if Assigned(FTrayIcon.OnMouseDown) then
          FTrayIcon.OnMouseDown(FTrayIcon, mbLeft, [], MousePos.x, MousePos.y);

        if Assigned(FTrayIcon.OnDblClick) then
          FTrayIcon.OnDblClick(FTrayIcon);

        if Assigned(FTrayIcon.OnMouseUp) then
          FTrayIcon.OnMouseUp(FTrayIcon, mbLeft, [], MousePos.x, MousePos.y);
      end;
    QSystemTrayIconMiddleClick:
      begin
        if Assigned(FTrayIcon.OnMouseDown) then
          FTrayIcon.OnMouseDown(FTrayIcon, mbMiddle, [], MousePos.x, MousePos.y);
        if Assigned(FTrayIcon.OnMouseUp) then
          FTrayIcon.OnMouseUp(FTrayIcon, mbMiddle, [], MousePos.x, MousePos.y);
      end;
    QSystemTrayIconContext:
      begin
        if Assigned(FTrayIcon.OnMouseDown) then
          FTrayIcon.OnMouseDown(FTrayIcon, mbRight, [], MousePos.x, MousePos.y);

        if Assigned(FTrayIcon.OnMouseUp) then
          FTrayIcon.OnMouseUp(FTrayIcon, mbRight, [], MousePos.x, MousePos.y);
      end;
  end;
end;

procedure TQtSystemTrayIcon.showBaloonHint(const ATitle, AHint: String;
  const AFlag: QSystemTrayIconMessageIcon; const ATimeOut: Integer);
var
  WHint: WideString;
  WTitle: WideString;
begin
  WHint := GetUTF8String(AHint);
  WTitle := GetUTF8String(ATitle);
  QSystemTrayIcon_showMessage(Handle, @WTitle, @WHint, AFlag, ATimeOut);
end;

procedure TQtSystemTrayIcon.Show;
begin
  QSystemTrayIcon_show(handle);
end;

procedure TQtSystemTrayIcon.Hide;
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
var
  ClipboardType: TClipboardType;
begin
  inherited Create;
  FLockClip := False;
  for ClipboardType := Low(TClipBoardType) to High(TClipBoardType) do
    FOnClipBoardRequest[ClipBoardType] := nil;
  FClipBoardFormats := TStringList.Create;
  FClipBoardFormats.Add('foo'); // 0 is reserved
  TheObject := QApplication_clipBoard;
  {$IFDEF HASX11}
  FLockX11Selection := 0;
  FSelTimer := TQtTimer.CreateTimer(10, @selectionTimer, TheObject);
  {$ENDIF}
  AttachEvents;
end;

destructor TQtClipboard.Destroy;
begin
  DetachEvents;
  {$IFDEF HASX11}
  if FSelTimer <> nil then
    FSelTimer.Free;
  {$ENDIF}
  FClipBoardFormats.Free;
  inherited Destroy;
end;

procedure TQtClipboard.AttachEvents;
begin
  inherited AttachEvents;
  FClipDataChangedHook := QClipboard_hook_create(TheObject);
  QClipboard_hook_hook_dataChanged(FClipDataChangedHook, @signalDataChanged);
  {$IFDEF HASX11}
  FClipSelectionChangedHook := QClipboard_hook_create(TheObject);
  QClipboard_hook_hook_selectionChanged(FClipSelectionChangedHook,
    @signalSelectionChanged);
  {$ENDIF}
end;

procedure TQtClipboard.signalDataChanged; cdecl;
begin
  {$IFDEF VERBOSE_QT_CLIPBOARD}
  writeln('signalDataChanged()');
  {$ENDIF}
  FClipChanged := IsClipboardChanged;
end;

{$IFDEF HASX11}
procedure TQtClipboard.BeginX11SelectionLock;
begin
  inc(FLockX11Selection);
end;

procedure TQtClipboard.EndX11SelectionLock;
begin
  dec(FLockX11Selection);
end;

function TQtClipboard.InX11SelectionLock: Boolean;
begin
  Result := FLockX11Selection > 0;
end;

procedure TQtClipboard.signalSelectionChanged; cdecl;
var
  TempMimeData: QMimeDataH;
  WStr: WideString;
  Clip: TClipBoard;
begin
  {$IFDEF VERBOSE_QT_CLIPBOARD}
  writeln('signalSelectionChanged() OWNER?=', QClipboard_ownsSelection(Self.clipboard),
  ' FOnClipBoardRequest ? ',FOnClipBoardRequest[ctPrimarySelection] <> nil);
  {$ENDIF}
  if InX11SelectionLock then
    exit;
  TempMimeData := getMimeData(QClipboardSelection);
  if (TempMimeData <> nil) and
  (QMimeData_hasText(TempMimeData) or QMimeData_hasHtml(TempMimeData) or
    QMimeData_hasURLS(TempMimeData)) then
  begin
    QMimeData_text(TempMimeData, @WStr);
    // do not touch LCL's selection if shift is down
    // since in that case event is tracked via FSelTimer
    // until shift depressed.
    if QApplication_keyboardModifiers() and QtShiftModifier <> 0 then
      exit;
    // do complete primaryselection cleanup at LCL side
    // so it asks for clip from qt (no matter is it owner or not).
    BeginUpdate;
    Clip := Clipbrd.Clipboard(ctPrimarySelection);
    Clip.OnRequest := nil;
    FOnClipBoardRequest[ctPrimarySelection] := nil;
    Clip.AsText := UTF8Decode(WStr);
    EndUpdate;
  end;
end;

procedure TQtClipboard.selectionTimer;
var
  RptEvent: QLCLMessageEventH;
begin
  if FOnClipBoardRequest[ctPrimarySelection] = nil then
  begin
    FSelTimer.TimerEnabled := False;
    exit;
  end;
  if QApplication_keyboardModifiers() and QtShiftModifier = 0 then
  begin
    FSelTimer.TimerEnabled := False;
    RptEvent :=  QLCLMessageEvent_create(LCLQt_ClipboardPrimarySelection,
       Ord(ctPrimarySelection), FSelFmtCount, 0, 0);
    QCoreApplication_postEvent(ClipBoard, RptEvent);
  end;
end;

{$ENDIF}

function TQtClipboard.IsClipboardChanged: Boolean;
var
  TempMimeData: QMimeDataH;
  Str: WideString;
  Str2: WideString;
begin
  Result := not FLockClip;
  if FLockClip then
    exit;
  // FLockClip: here we know that our clipboard is not changed by LCL Clipboard
  FLockClip := True;
  try
    TempMimeData := getMimeData(QClipboardClipboard);
    if (TempMimeData <> nil) and
    (QMimeData_hasText(TempMimeData) or QMimeData_hasHtml(TempMimeData) or
      QMimeData_hasURLS(TempMimeData)) then
    begin
      QMimeData_text(TempMimeData, @Str);
      Str := UTF16ToUTF8(Str);

      Str2 := Clipbrd.Clipboard.AsText;

      Result := Str <> Str2;
      if Result then
        Clipbrd.Clipboard.AsText := Str;
    end;
  finally
    FLockClip := False;
  end;
end;

function TQtClipboard.EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl;
{$IFDEF HASX11}
var
  ClipboardType: TClipboardType;
  FormatCount: PtrUint;
  Modifiers: QtKeyboardModifiers;

  procedure PutSelectionOnClipBoard;
  var
    MimeType: WideString;
    MimeData: QMimeDataH;
    Data: QByteArrayH;
    DataStream: TMemoryStream;
    I: Integer;
    Clip: TClipboard;
  begin
    // We must track this event if shift is down, since
    // we are doing keyboard selection.
    // When shift is depressed, selectionTimer will trigger
    // another event which will pass this point
    // and assign selection to qt selection clipboard.
    if Modifiers and QtShiftModifier <> 0 then
    begin
      if not FSelTimer.TimerEnabled then
        FSelTimer.TimerEnabled := True;
      exit;
    end;
    if FSelTimer.TimerEnabled then
      FSelTimer.TimerEnabled := False;

    Clip := Clipbrd.Clipboard(ClipboardType);
    MimeData := QMimeData_create();
    DataStream := TMemoryStream.Create;
    for I := 0 to FormatCount - 1 do
    begin
      DataStream.Size := 0;
      DataStream.Position := 0;
      MimeType := FormatToMimeType(Clip.Formats[I]);
      FOnClipBoardRequest[ClipboardType](Clip.Formats[I], DataStream);
      Data := QByteArray_create(PAnsiChar(DataStream.Memory), DataStream.Size);
      if (QByteArray_length(Data) > 1) and QByteArray_endsWith(Data, #0) then
        QByteArray_chop(Data, 1);
      QMimeData_setData(MimeData, @MimeType, Data);
      QByteArray_destroy(Data);
    end;
    DataStream.Free;
    // we must "wake up" QMimeData text property, otherwise
    // some non ascii chars could be eaten (possible qt bug)
    QMimeData_text(MimeData, @MimeType);
    setMimeData(MimeData, ClipbBoardTypeToQtClipboard[ClipBoardType]);
    // do not destroy MimeData!!!
  end;
{$ENDIF}
begin
  BeginEventProcessing;
  Result := False;

  {$IFDEF HASX11}
  if QEvent_type(Event) = LCLQt_ClipboardPrimarySelection then
  begin
    ClipboardType := TClipBoardType(QLCLMessageEvent_getMsg(QLCLMessageEventH(Event)));
    FormatCount := QLCLMessageEvent_getWParam(QLCLMessageEventH(Event));
    Modifiers := QtKeyboardModifiers(QLCLMessageEvent_getLParam(QLCLMessageEventH(Event)));
    if FOnClipBoardRequest[ClipboardType] <> nil then
      PutSelectionOnClipboard;
    Result := True;
    QEvent_accept(Event);
  end;
  {$ENDIF}

  if QEvent_type(Event) = QEventClipboard then
  begin
    Result := FClipChanged;
    // Clipboard is changed, but we have no ability at moment to pass that info
    // to LCL since LCL has no support for that event
    // so we are using signalDataChanged() to pass changes to Clipbrd.Clipboard
    if FClipChanged then
      FClipChanged := False;
    QEvent_accept(Event);
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
    GetMem(List, Count * SizeOf(TClipboardFormat));
    
    for i := 0 to Count - 1 do
    begin
      QStringList_at(QtList, @Str, i);
      Str := UTF16ToUTF8(Str);
      List[i] := RegisterFormat(Str);
    end;

    Result := True;

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
    {$IFDEF HASX11}
    Event: QLCLMessageEventH;
    {$ENDIF}
  begin
    {$IFDEF HASX11}
    // we must delay assigning selection to qt clipboard
    // so generate our private event
    if ClipboardType <> ctClipboard then
    begin
      FSelFmtCount := FormatCount;
      Event :=  QLCLMessageEvent_create(LCLQt_ClipboardPrimarySelection,
         Ord(ClipboardType), FormatCount, PtrUInt(QApplication_keyboardModifiers()), 0);
      QCoreApplication_postEvent(ClipBoard, Event);
      exit;
    end;
    {$ENDIF}
    MimeData := QMimeData_create();
    DataStream := TMemoryStream.Create;
    for I := 0 to FormatCount - 1 do
    begin
      DataStream.Size := 0;
      DataStream.Position := 0;
      MimeType := FormatToMimeType(Formats[I]);
      FOnClipBoardRequest[ClipboardType](Formats[I], DataStream);
      Data := QByteArray_create(PAnsiChar(DataStream.Memory), DataStream.Size);
      if (QByteArray_length(Data) > 1) and QByteArray_endsWith(Data, #0) then
        QByteArray_chop(Data, 1);
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
    FOnClipBoardRequest[ClipboardType] := nil;
    Result := True;
  end else
  begin
    if FLockClip then
      exit;
    {FLockClip: we are sure that this request comes from LCL Clipboard}
    FLockClip := True;
    try
      { clear OnClipBoardRequest to prevent destroying the LCL clipboard,
        when emptying the clipboard}
      FOnClipBoardRequest[ClipBoardType] := nil;
      {$IFDEF HASX11}
      // if we are InUpdate , then change is asked
      // from selectionChanged trigger, so don't do anything
      if (ClipboardType <> ctClipBoard) and InUpdate then
      begin
        Result := True;
        exit;
      end;
      {$ENDIF}
      FOnClipBoardRequest[ClipBoardType] := OnRequestProc;
      PutOnClipBoard;
      Result := True;
    finally
      FLockClip := False;
    end;
  end;
end;

{ TQtPrinter }

constructor TQtPrinter.Create;
begin
  FPrinterActive := False;
  FHandle := QPrinter_create(QPrinterHighResolution);
end;

constructor TQtPrinter.Create(AMode: QPrinterPrinterMode);
begin
  FPrinterActive := False;
  FHandle := QPrinter_create(AMode);
end;

destructor TQtPrinter.Destroy;
begin
  endDoc;
  if FHandle <> nil then
    QPrinter_destroy(FHandle);
  inherited Destroy;
end;

{returns default system printer}
function TQtPrinter.DefaultPrinter: WideString;
var
  prnName: WideString;
  PrnInfo: QPrinterInfoH;
begin
  PrnInfo := QPrinterInfo_create();
  QPrinterInfo_defaultPrinter(PrnInfo);
  QPrinterInfo_printerName(PrnInfo, @PrnName);
  QPrinterInfo_destroy(PrnInfo);
  if PrnName = '' then
    PrnName := 'unknown';
  Result := UTF8ToUTF16(PrnName);
end;

{returns available list of printers.
 if there's no printer on system result will be false.
 Default sys printer is always 1st in the list.}
function TQtPrinter.GetAvailablePrinters(Lst: TStrings): Boolean;
var
  Str: WideString;
  PrnName: WideString;
  i: Integer;
  PrnInfo: QPrinterInfoH;
  Prntr: QPrinterInfoH;
  PrnList: TPtrIntArray;
begin
  Result := False;
  Str := DefaultPrinter;
  // EnumQPrinters(Lst);
  PrnInfo := QPrinterInfo_create();
  try
    Lst.Clear;
    QPrinterInfo_availablePrinters(@PrnList);
    for i := Low(PrnList) to High(PrnList) do
    begin
      Prntr := QPrinterInfoH(PrnList[i]);
      if Assigned(Prntr) and not QPrinterInfo_isNull(Prntr) then
      begin
        QPrinterInfo_printerName(Prntr, @PrnName);
        if QPrinterInfo_isDefault(Prntr) then
          Lst.Insert(0, UTF8ToUTF16(PrnName))
        else
          Lst.Add(UTF8ToUTF16(PrnName));
      end;
    end;
  finally
    QPrinterInfo_destroy(PrnInfo);
  end;

  i := Lst.IndexOf(Str);
  if i > 0 then
    Lst.Move(i, 0);
  Result := Lst.Count > 0;
end;

procedure TQtPrinter.beginDoc;
begin
  getPrinterContext;
  FPrinterActive := FPrinterContext <> nil;
end;

procedure TQtPrinter.endDoc;
begin
  if FPrinterContext <> nil then
  begin
    if QPainter_isActive(FPrinterContext.Widget) then
      QPainter_end(FPrinterContext.Widget);
    FPrinterContext.Free;
    FPrinterContext := nil;
  end;
  FPrinterActive := False;
end;

function TQtPrinter.getPrinterContext: TQtDeviceContext;
begin
  if FPrinterContext = nil then
    FPrinterContext := TQtDeviceContext.CreatePrinterContext(Handle);
  Result := FPrinterContext;
end;

function TQtPrinter.GetDuplexMode: QPrinterDuplexMode;
begin
  Result := QPrinter_duplex(FHandle);
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
  Result := UTF16ToUTF8(Str);
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
  Result := UTF16ToUTF8(Str);
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
  Result := UTF16ToUTF8(Str);
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

procedure TQtPrinter.SetDuplexMode(AValue: QPrinterDuplexMode);
begin
  QPrinter_setDuplex(FHandle, AValue);
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
  Result := UTF16ToUTF8(Str);
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
  Result := UTF16ToUTF8(Str);
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
  QPrinter_setPaperSize(FHandle, AValue);
end;

function TQtPrinter.getPageSize: QPrinterPageSize;
begin
  Result := QPrinter_paperSize(FHandle);
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

function TQtPrinter.PageRect(AUnits: QPrinterUnit): TRect;
var
  R: QRectFH;
begin
  R := QRectF_create();
  QPrinter_pageRect(FHandle, R, AUnits);
  QRectF_toRect(R, @Result);
  QRectF_destroy(R);
end;

function TQtPrinter.PaperRect(AUnits: QPrinterUnit): TRect;
var
  R: QRectFH;
begin
  R := QRectF_create();
  QPrinter_paperRect(FHandle, R, AUnits);
  QRectF_toRect(R, @Result);
  QRectF_destroy(R);
end;

function TQtPrinter.PrintEngine: QPrintEngineH;
begin
  Result := QPrinter_printEngine(FHandle);
end;

function TQtPrinter.GetPaperSize(AUnits: QPrinterUnit): TSize;
var
  SizeF: QSizeFH;
begin
  SizeF := QSizeF_create(0, 0);
  QPrinter_paperSize(FHandle, SizeF, AUnits);
  Result.cx := Round(QSizeF_width(SizeF));
  Result.cy := Round(QSizeF_height(SizeF));
  QSizeF_destroy(SizeF);
end;

procedure TQtPrinter.SetPaperSize(ASize: TSize; AUnits: QPrinterUnit);
var
  SizeF: QSizeFH;
begin
  SizeF := QSizeF_create(@ASize);
  try
    QPrinter_setPaperSize(FHandle, SizeF, AUnits);
  finally
    QSizeF_destroy(SizeF);
  end;
end;

function TQtPrinter.SupportedResolutions: TPtrIntArray;
begin
  QPrinter_supportedResolutions(FHandle, @Result);
end;


{ TQtTimer }

{------------------------------------------------------------------------------
  Function: TQtTimer.CreateTimer
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtTimer.CreateTimer(Interval: integer;
  const TimerFunc: TWSTimerProc; App: QObjectH);
begin
  inherited Create;
  FDeleteLater := True;
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

procedure TQtTimer.AttachEvents;
begin
  FTimerHook := QTimer_hook_create(QTimerH(TheObject));
  QTimer_hook_hook_timeout(FTimerHook, @signalTimeout);
  inherited AttachEvents;
end;

procedure TQtTimer.DetachEvents;
begin
  QTimer_stop(QTimerH(TheObject));
  if FTimerHook <> nil then
    QTimer_hook_destroy(FTimerHook);
  inherited DetachEvents;
end;

procedure TQtTimer.signalTimeout; cdecl;
begin
  if Assigned(FCallbackFunc) then
    FCallbackFunc;
end;

function TQtTimer.getTimerEnabled: Boolean;
begin
  if TheObject <> nil then
    Result := QTimer_isActive(QTimerH(TheObject))
  else
    Result := False;
end;

procedure TQtTimer.setTimerEnabled(const AValue: Boolean);
begin
  if (TheObject <> nil) and (getTimerEnabled <> AValue) then
  begin
    if AValue then
      QTimer_start(QTimerH(TheObject))
    else
      QTimer_stop(QTimerH(TheObject));
  end;
end;

{------------------------------------------------------------------------------
  Function: TQtTimer.EventFilter
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtTimer.EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl;
begin
  Result := False;
  QEvent_accept(Event);
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

procedure TQtIcon.addPixmap(pixmap: QPixmapH; mode: QIconMode = QIconNormal; state: QIconState = QIconOff);
begin
  QIcon_addPixmap(Handle, pixmap, mode, state);
end;

{ TQtStringList }

function TQtStringList.Get(Index: Integer): string;
var
  W: Widestring;
begin
  QStringList_at(FHandle, @W, Index);
  Result := UTF16ToUTF8(W);
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

{ TQtCursor }

constructor TQtCursor.Create;
begin
  FHandle := QCursor_create();
end;

constructor TQtCursor.Create(pixmap: QPixmapH; hotX: Integer  = -1; hotY: Integer = -1);
begin
  FHandle := QCursor_create(pixmap, hotX, hotY);
end;

constructor TQtCursor.Create(shape: QtCursorShape);
begin
  FHandle := QCursor_create(shape);
end;

destructor TQtCursor.Destroy;
begin
  if FHandle <> nil then
    QCursor_destroy(FHandle);
    
  inherited Destroy;
end;

{ TQtWidgetPalette }

procedure TQtWidgetPalette.initializeSysColors;
var
  Palette: QPaletteH;
begin
  FillChar(FCurrentColor, SizeOf(FCurrentColor), 0);
  FillChar(FCurrentColor, SizeOf(FCurrentTextColor), 0);
  Palette := QPalette_create();
  try
    QApplication_palette(Palette);
    FDefaultColor := QPalette_color(Palette, QPaletteActive, FWidgetRole)^;
    FDefaultTextColor := QPalette_color(Palette, QPaletteActive, FTextRole)^;
    FDisabledColor := QPalette_color(Palette, QPaletteDisabled, FWidgetRole)^;
    FDisabledTextColor := QPalette_color(Palette, QPaletteDisabled, FTextRole)^;
  finally
    QPalette_destroy(Palette);
  end;
end;

constructor TQtWidgetPalette.Create(AWidgetColorRole: QPaletteColorRole;
  AWidgetTextColorRole: QPaletteColorRole; AWidget: QWidgetH);
begin
  FInReload := False;
  FForceColor := False;
  FWidget := AWidget;
  FWidgetRole := AWidgetColorRole;
  FTextRole := AWidgetTextColorRole;
  initializeSysColors;

  // ugly qt mac bug
  {$IFDEF DARWIN}
  if QWidget_backgroundRole(FWidget) <> FWidgetRole then
  begin
    QWidget_setBackgroundRole(FWidget, FWidgetRole);
    QWidget_setForegroundRole(FWidget, FTextRole);
  end;
  {$ENDIF}

  FHandle := QPalette_create();
end;

destructor TQtWidgetPalette.Destroy;
begin
  if FHandle <> nil then
    QPalette_destroy(FHandle);
  inherited Destroy;
end;

function TQtWidgetPalette.ColorChangeNeeded(const AColor: TQColor;
  const ATextRole: Boolean): Boolean;
begin
  if ATextRole then
    Result := not (EqualTQColor(AColor, FDefaultTextColor) and
      EqualTQColor(FCurrentTextColor, FDefaultTextColor))
  else
    Result := not (EqualTQColor(AColor, FDefaultColor) and
      EqualTQColor(FCurrentColor, FDefaultColor));
end;

procedure TQtWidgetPalette.setColor(const AColor: PQColor);
begin
  if not ColorChangeNeeded(AColor^, False) and not FInReload and not FForceColor then
    exit;

  QPalette_setColor(FHandle, QPaletteActive, FWidgetRole, AColor);
  QPalette_setColor(FHandle, QPaletteInActive, FWidgetRole, AColor);

  if EqualTQColor(AColor^, FDefaultColor) then
    QPalette_setColor(FHandle, QPaletteDisabled, FWidgetRole, @FDisabledColor)
  else
    QPalette_setColor(FHandle, QPaletteDisabled, FWidgetRole, AColor);

  QWidget_setPalette(FWidget, FHandle);
  FCurrentColor := AColor^;
end;

procedure TQtWidgetPalette.setTextColor(const AColor: PQColor);
begin
  if not ColorChangeNeeded(AColor^, True) and not FInReload and not FForceColor then
    exit;
  QPalette_setColor(FHandle, QPaletteActive, FTextRole, AColor);
  QPalette_setColor(FHandle, QPaletteInActive, FTextRole, AColor);
  if EqualTQColor(AColor^, FDefaultTextColor) or
     EqualTQColor(FCurrentColor, FDefaultColor) then
    QPalette_setColor(FHandle, QPaletteDisabled, FTextRole, @FDisabledTextColor)
  else
    QPalette_setColor(FHandle, QPaletteDisabled, FTextRole, AColor);
  QWidget_setPalette(FWidget, FHandle);
  FCurrentTextColor := AColor^;
end;

procedure TQtWidgetPalette.ReloadPaletteBegin;
var
  AOldCurrent, AOldText: TQColor;
begin
  FInReload := True;
  AOldCurrent := FCurrentColor;
  AOldText := FCurrentTextColor;
  initializeSysColors;
  FCurrentColor := AOldCurrent;
  FCurrentTextColor := AOldText;
end;

procedure TQtWidgetPalette.ReloadPaletteEnd;
begin
  FInReload := False;
end;

{ TQtActionGroup }

constructor TQtActionGroup.Create(const AParent: QObjectH);
begin
  FGroupIndex := 0;
  Initialize(FActions);
  FHandle := QActionGroup_create(AParent);
end;

destructor TQtActionGroup.Destroy;
begin
  if FHandle <> nil then
    QActionGroup_destroy(FHandle);
  Finalize(FActions);
  FActions := nil;
  inherited Destroy;
end;

function TQtActionGroup.getEnabled: boolean;
begin
  Result := QActionGroup_isEnabled(FHandle);
end;

function TQtActionGroup.getExclusive: boolean;
begin
  Result := QActionGroup_isExclusive(FHandle);
end;

function TQtActionGroup.getVisible: boolean;
begin
  Result := QActionGroup_isVisible(FHandle);
end;

procedure TQtActionGroup.setEnabled(const AValue: boolean);
begin
  QActionGroup_setEnabled(FHandle, AValue);
end;

procedure TQtActionGroup.setExclusive(const AValue: boolean);
begin
  QActionGroup_setExclusive(FHandle, AValue);
end;

procedure TQtActionGroup.setVisible(const AValue: boolean);
begin
  QActionGroup_setVisible(FHandle, AValue);
end;

function TQtActionGroup.addAction(action: QActionH): QActionH;
begin
  Result := QActionGroup_addAction(FHandle, action);
end;

function TQtActionGroup.addAction(text: WideString): QActionH;
var
  WStr: WideString;
begin
  WStr := GetUTF8String(text);
  Result := QActionGroup_addAction(FHandle, @WStr);
end;

function TQtActionGroup.addAction(icon: QIconH; text: WideString): QActionH;
var
  WStr: WideString;
begin
  WStr := GetUTF8String(text);
  Result := QActionGroup_addAction(FHandle, icon, @WStr);
end;

procedure TQtActionGroup.removeAction(action: QActionH);
begin
  QActionGroup_removeAction(FHandle, action);
end;

function TQtActionGroup.actions: TQActions;
var
  i: Integer;
  Arr: TPtrIntArray;
begin
  QActionGroup_actions(FHandle, @Arr);
  SetLength(FActions, length(Arr));
  for i := 0 to High(Arr) do
    FActions[i] := QActionH(Arr[i]);
  Result := FActions;
end;

function TQtActionGroup.checkedAction: QActionH;
begin
  Result := QActionGroup_checkedAction(FHandle);
end;

procedure TQtActionGroup.setDisabled(ADisabled: Boolean);
begin
  QActionGroup_setDisabled(FHandle, ADisabled);
end;

{ TQtObjectDump }

procedure TQtObjectDump.Iterator(ARoot: QObjectH);
var
  i: Integer;
  Children: TPtrIntArray;
begin
  QObject_children(ARoot, @Children);
  AddToList(ARoot);
  for i := 0 to High(Children) do
    Iterator(QObjectH(Children[i]))
end;

procedure TQtObjectDump.AddToList(AnObject: QObjectH);
// var
//  ObjName: WideString;
begin
  if AnObject <> nil then
  begin
    // QObject_objectName(AnObject, @ObjName);
    if FObjList.IndexOf(AnObject) < 0 then
    begin
      FList.Add(dbghex(PtrUInt(AnObject)));
      FObjList.Add(AnObject);
    end else
      raise Exception.Create('TQtObjectDump: Duplicated object in list '+dbghex(PtrUInt(AnObject)));
  end;
end;

procedure TQtObjectDump.DumpObject;
begin
  if FRoot = nil then
    raise Exception.Create('TQtObjectDump: Invalid FRoot '+dbghex(PtrUInt(FRoot)));
  Iterator(FRoot);
end;

function TQtObjectDump.findWidgetByName(const AName: WideString): QWidgetH;
var
  j: Integer;
  WS: WideString;
begin
  Result := nil;
  if AName = '' then
    exit;
  for j := 0 to FObjList.Count - 1 do
  begin
    QObject_objectName(QObjectH(FObjList.Items[j]), @WS);
    if (WS = AName) and QObject_isWidgetType(QObjectH(FObjList.Items[j])) then
    begin
      Result := QWidgetH(FObjList.Items[j]);
      break;
    end;
  end;
end;

function TQtObjectDump.IsWidget(AnObject: QObjectH): Boolean;
begin
  if AnObject <> nil then
    Result := QObject_IsWidgetType(AnObject)
  else
    Result := False;
end;

function TQtObjectDump.GetObjectName(AnObject: QObjectH): WideString;
begin
  Result := '';
  if AnObject = nil then
    exit;
  QObject_objectName(AnObject, @Result);
end;

function TQtObjectDump.InheritsQtClass(AnObject: QObjectH;
  AQtClass: WideString): Boolean;
begin
  if (AnObject = nil) or (AQtClass = '') then
    Result := False
  else
    Result := QObject_inherits(AnObject, @AQtClass);
end;

constructor TQtObjectDump.Create(AnObject: QObjectH);
begin
  FRoot := AnObject;
  FList := TStringList.Create;
  FObjList := TFPList.Create;
end;

destructor TQtObjectDump.Destroy;
begin
  FList.Free;
  FObjList.Free;
  inherited Destroy;
end;

end.





