{ $Id$ 
                         -------------------------------
                         gtk2def.pp  -  Type definitions
                         ------------------------------- 
 
 @created(Tue Nov 20st WET 2007)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.net>)                       

 This unit contains type definitions needed in the GTK2 <-> LCL interface
 
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}


unit Gtk2Def;
 
{$mode objfpc} {$H+}

interface

uses
  // RTL
  Classes, SysUtils, glib2, gdk2pixbuf, pango, gdk2, gtk2,
  // LazUtils
  DynHashArray, LazLoggerBase,
  // LCL
  Gtk2Extra,
  LCLIntf, LCLProc, LCLType, LCLMemManager,
  GraphType, Gtk2Globals, Graphics {for TColor};

{$ifdef TraceGdiCalls}
const
  MaxTraces    = 5;
  MaxCallBacks = 11;
type
  TCallBacksArray = array[0..MaxCallBacks] of Pointer;
  PCallBacksArray = ^TCallBacksArray;
{$endif}

// styles -------------------------------------------------------------------
type
  TLazGtkStyle = (
    lgsGTK_Default, // without anything
    lgsDefault,     // with rc file
    lgsButton,
    lgsLabel,
    lgsWindow,
    lgsCheckbox,
    lgsRadiobutton,
    lgsMenu,
    lgsMenuBar,
    lgsMenuitem,
    lgsList,
    lgsVerticalScrollbar,
    lgsHorizontalScrollbar,
    lgsTooltip,
    lgsVerticalPaned,
    lgsHorizontalPaned,
    lgsNotebook,
    lgsStatusBar,
    lgsHScale,
    lgsVScale,
    lgsGroupBox,
    lgsTreeView,      // for gtk2
    lgsToolBar,       // toolbar
    lgsToolButton,    // button placed on toolbar
    lgsCalendar,      // button placed on toolbar
    lgsScrolledWindow,
    lgsComboBox,
    // user defined
    lgsUserDefined
    );

const
  LazGtkStyleNames: array[TLazGtkStyle] of string = (
    'gtk_default',
    'default',
    'button',
    'label',
    'window',
    'checkbox',
    'radiobutton',
    'menu',
    'menubar',
    'menuitem',
    'list',
    'vertical scrollbar',
    'horizontal scrollbar',
    'tooltip',
    'vertical paned',
    'horizontal paned',
    'notebook',
    'statusbar',
    'hscale',
    'vscale',
    'groupbox',
    'treeview',
    'toolbar',
    'toolbutton',
    'calendar',
    'scrolled window',
    'combobox',
    ''
    );


const
  // drag target type for on drop files event invoking
  FileDragTarget: TGtkTargetEntry = (target: 'text/uri-list'; flags: 0; info: 0;);

type
  TGDIType = (gdiBitmap, gdiBrush, gdiFont, gdiPen, gdiRegion, gdiPalette);
  TGDIBitmapType = (gbBitmap, gbPixmap, gbPixbuf);

  TGtkDeviceContext = class;

  TGtkIntfFont = PPangoLayout;

  PGDIRGB = ^TGDIRGB;
  TGDIRGB = record
    Red,
    Green,
    Blue: Byte;
  end;

  TGDIColorFlag = (cfColorAllocated);
  TGDIColorFlags = set of TGDIColorFlag;

  TGDIColor = record
    ColorRef: TColorRef;    //Color passed - can be a SYSCOLOR or RGB
    ColorFlags: TGDIColorFlags;
    Color: TGDKColor;       //Actual GDK Color(If any) for use with GC's
    Colormap: PGDKColormap; //Colormap GDKColor was allocated with
  end;
  PGDIColor = ^TGDIColor;

  { Create a GDIObject with NewGDIObject. Then RefCount is 1.
    Free a GDIObject with DeleteObject. This will decrease the RefCount
    and when 0 calls DisposeGDIObject. }
  PGDIObject = ^TGDIObject;
  TGDIObject = record
    RefCount: integer; // see ReleaseGDIObject, ReferenceGDIObject
    DCCount: integer; // number of DeviceContexts using this GDIObject
    Shared: Boolean; // stock or system object which skips DeleteObject calls
    Owner: TGtkDeviceContext;
    {$ifdef TraceGdiCalls}
    StackAddrs: TCallBacksArray;
    {$endif}
    Next: PGDIObject; // 'Next' is used by the internal mem manager
    case GDIType: TGDIType of
      gdiBitmap: (
        Depth: integer;
        SystemVisual : Boolean;
        Visual : PGDKVisual;
        Colormap : PGDKColormap;
        case GDIBitmapType: TGDIBitmapType of
          gbBitmap: (GDIBitmapObject: PGdkBitmap); // pixmap with depth 1
          gbPixmap: (GDIPixmapObject: record // normal pixmap
                      Image: PGdkPixmap;     // imagedata
                      Mask: PGdkBitmap;      // the mask for images with 1 bit alpha and pixmap not supporting alpha
                    end);
          gbPixbuf: (GDIPixbufObject: PGdkPixbuf);
      );
      gdiBrush: (
        // ToDo: add bitmap mask
        IsNullBrush: Boolean;
        GDIBrushColor: TGDIColor;
        GDIBrushFill: TGdkFill;
        GDIBrushPixMap: PGdkPixmap;
      );
      gdiFont: (
        GDIFontObject: TGtkIntfFont;
        LogFont: TLogFont;// font info is stored as well, for later query font params
        UnTransfFontHeight: Integer;
      );
      gdiPen: (
        IsNullPen : Boolean;//GDK will bomb with a NULL Pen Hatch
        IsExtPen: Boolean;
        GDIPenColor: TGDIColor;
        GDIPenWidth: DWord;
        GDIPenStyle: DWord;
        GDIPenDashes: Pgint8;
        GDIPenDashesCount: DWord;
        UnTransfPenWidth: DWord;
      );
      gdiRegion: (
        GDIRegionObject: PGdkRegion;
          { ! Always without the DCOrigin
            GDIObjects can exists without DCs and so they are independent

            - When the DCOrigin is moved, the region is not moved automatically
            - Any clipping operation must be mapped, *before* applying it to the
              GDIRegionObject, and *after* reading it
          }
      );
      gdiPalette: (
        //Is this the system palette?
        SystemPalette : Boolean;

        //or, Has it been added to the system palette?
        PaletteRealized: Boolean;

        //Type of visual expected
        VisualType: TGdkVisualType;

        //Actual visual created
        PaletteVisual: PGDKVisual;

        //Colormap for mapping colors
        PaletteColormap: PGDKColormap;

        //For mapping from Index to RGB
        RGBTable: TDynHashArray;
        IndexTable: TDynHashArray;
      );
  end;

  TDevContextTextMetric = record
    lBearing: LongInt;
    rBearing: LongInt;
    TextMetric: TTextMetric;
    IsDoubleByteChar: boolean;
    IsMonoSpace: boolean;
  end;

  TDeviceContextsFlag = (
    dcfPenSelected, // pen changed and needs selecting
    dcfPenInvalid,  // pen is not a valid GDIObject
    dcfTextMetricsValid,
    dcfDoubleBuffer  // Drawable is a double buffer
    );
  TDeviceContextsFlags = set of TDeviceContextsFlag;

  TDevContextsColorType = (
    dccNone,
    dccCurrentBackColor,
    dccCurrentTextColor,
    dccGDIBrushColor,
    dccGDIPenColor
    );

  TDevContextSelectedColorsType = (
    dcscCustom,
    dcscPen,
    dcscBrush,
    dcscFont
    );


  { TGtkDeviceContext }


  TGtkDeviceContextClass = class of TGtkDeviceContext;
  TGtkDeviceContext = class
  private
    FClipRegion: PGdiObject;
    FCurrentBitmap: PGdiObject;
    FCurrentBrush: PGdiObject;
    FCurrentFont: PGdiObject;
    FCurrentPalette: PGdiObject;
    FCurrentPen: PGdiObject;
    FGC: pgdkGC;
    FGCValues: TGdkGCValues;

    FHasTransf: Boolean; // is any viewport/affine transformation applied?

    FDrawable: PGDKDrawable; // either the gdk_window of the owner
                             // or the gdk_bitmap/pixmap of the selected image
                             // or the double buffer (OriginalDrawable will hold the original)

    FPixbuf: PGdkPixbuf;     // pixbuf reference for when the drawable comes from a pixbuf

    FOriginalDrawable: PGDKDrawable; // only set if dcfDoubleBuffer in DCFlags

    FWidget: PGtkWidget;     // the owner (in case of a windowDC)

    FWithChildWindows: boolean;// this DC covers sub gdkwindows

    FFlags: TDeviceContextsFlags;
    FSelectedColors: TDevContextSelectedColorsType;

    FOwnedGDIObjects: array[TGDIType] of PGdiObject;

    // viewport/affine transformations
    FMapMode: Integer; // current viewport/window mapping mode
    FViewPortExt: TPoint; // current viewport extent
    FViewPortOrg: TPoint; // current viewport origin
    FWindowExt: TPoint; // current window extent
    FWindowOrg: TPoint; // current window origin

    function GetClipRectangle: TGdkRectangle;
    function GetGDIObjects(ID: TGDIType): PGdiObject;
    function GetOffset: TPoint;
    function GetOwnedGDIObjects(ID: TGDIType): PGdiObject;
    procedure SetClipRegion(const AValue: PGdiObject);
    procedure SetCurrentBitmap(const AValue: PGdiObject);
    procedure SetCurrentBrush(const AValue: PGdiObject);
    procedure SetCurrentFont(const AValue: PGdiObject);
    procedure SetCurrentPalette(const AValue: PGdiObject);
    procedure SetCurrentPen(const AValue: PGdiObject);
    procedure ChangeGDIObject(var GDIObject: PGdiObject;
                              const NewValue: PGdiObject);
    procedure SetGDIObjects(ID: TGDIType; const AValue: PGdiObject);
    procedure SetMapMode(AValue: Integer);
    procedure SetOwnedGDIObjects(ID: TGDIType; const AValue: PGdiObject);
    procedure SetSelectedColors(AValue: TDevContextSelectedColorsType);

    function GetGC: pgdkGC;

    // winapi
    function  GetROP2: Integer;
    procedure SetROP2(AROP: Integer);
    procedure SetViewPortExt(const AValue: TPoint);
    procedure SetViewPortOrg(const AValue: TPoint);
    procedure SetWindowExt(const AValue: TPoint);
    procedure SetWindowOrg(AValue: TPoint);
  protected
    function CreateGC: PGdkGC; virtual;

    procedure CreateFont; virtual;
    procedure CreateBrush; virtual;
    procedure CreatePen; virtual;
    procedure CreateBitmap; virtual;

    // winapi
    function SelectBitmap(AGdiObject: PGdiObject): PGdiObject; virtual;
    function SelectPen(AGdiObject: PGdiObject): PGdiObject; virtual;

    // viewport/affine transformations
    procedure TransfUpdateFont; virtual;
    procedure TransfUpdatePen; virtual;
    // brushes not transformed!
  public
    {$ifdef TraceGdiCalls}
    StackAddrs: TCallBacksArray;
    {$endif}
    PenPos: TPoint;
    BkMode: Integer;
    CurrentTextColor: TGDIColor;
    CurrentBackColor: TGDIColor;
    DCTextMetric: TDevContextTextMetric; // only valid if dcfTextMetricsValid set
    PaintRectangle: TRect;// set during paint, BeginPaint/EndPaint
    SavedContext: TGtkDeviceContext; // linked list of saved DCs
    Antialiasing: Boolean;

    constructor Create; virtual;
    destructor Destroy; override;
    procedure CreateGDIObject(AGDIType: TGDIType);
    procedure SelectBrushProps; virtual;
    procedure SelectTextProps; virtual;
    procedure SelectPenProps; virtual;
    procedure SelectRegion;
    // device handles
    procedure SetWidget(AWidget: PGtkWidget; AWindow: PGdkWindow;
                        AWithChildWindows: Boolean; ADoubleBuffer: PGdkDrawable = nil);
    function HasGC: Boolean;
    procedure ResetGCClipping;
    procedure Clear;
    function GetFont: PGdiObject;
    function GetBrush: PGdiObject;
    function GetPen: PGdiObject;
    function GetBitmap: PGdiObject;
    function GetFunction: TGdkFunction;
    function IsNullBrush: boolean;
    function IsNullPen: boolean;
    function SelectObject(AGdiObject: PGdiObject): PGdiObject;
    procedure SetTextMetricsValid(AValid: Boolean); // temp helper, to allow flag manipulation
    procedure RemovePixbuf; // called to remove the stored pixbuf (because, f.e., the pixmap was modified)

    // viewport/affine transformations
    procedure InvTransfPoint(var X1, Y1: Integer);
    function InvTransfPointIndirect(const P: TPoint): TPoint; // point can be const
    procedure InvTransfRect(var X1, Y1, X2, Y2: Integer);
    function InvTransfRectIndirect(const R: TRect): TRect; // rect can be const
    procedure InvTransfExtent(var ExtX, ExtY: Integer);
    function InvTransfExtentIndirect(const Extent: TPoint): TPoint; // extent can be const
    procedure TransfAngles(var Angle1, Angle2: Integer);
    procedure TransfNormalize(var Lower, Higher: Integer);
    procedure TransfPoint(var X1, Y1: Integer);
    function TransfPointIndirect(const P: TPoint): TPoint; // point can be const
    procedure TransfRect(var X1, Y1, X2, Y2: Integer);
    function TransfRectIndirect(const R: TRect): TRect; // rect can be const
    procedure TransfExtent(var ExtX, ExtY: Integer);
    function TransfExtentIndirect(const Extent: TPoint): TPoint; // extent can be const

    // help functions
    function CopyDataFrom(ASource: TGtkDeviceContext; AClearSource, AMoveGDIOwnerShip, ARestore: Boolean): Boolean;
    function FillRect(ARect: TRect; ABrush: HBrush; SkipRop: Boolean): Boolean;
    procedure DrawTextWithColors(AText: PChar; ALength: LongInt; X, Y: Integer; FGColor, BGColor: PGdkColor);

    // device origin
    property Offset: TPoint read GetOffset;
    // drawing settings
    property CurrentBitmap: PGdiObject read FCurrentBitmap write SetCurrentBitmap;
    property CurrentFont: PGdiObject read FCurrentFont write SetCurrentFont;
    property CurrentPen: PGdiObject read FCurrentPen write SetCurrentPen;
    property CurrentBrush: PGdiObject read FCurrentBrush write SetCurrentBrush;
    property CurrentPalette: PGdiObject read FCurrentPalette write SetCurrentPalette;
    property ClipRect: TGdkRectangle read GetClipRectangle;
    property ClipRegion: PGdiObject read FClipRegion write SetClipRegion;
    property GCValues: TGdkGCValues read FGCValues;
    property GDIObjects[ID: TGDIType]: PGdiObject read GetGDIObjects write SetGDIObjects;
    // viewport/window and affine transformations
    property HasTransf: Boolean read FHasTransf;
    property MapMode: Integer read FMapMode write SetMapMode;
    property ViewPortExt: TPoint read FViewPortExt write SetViewPortExt;
    property ViewPortOrg: TPoint read FViewPortOrg write SetViewPortOrg;
    property WindowExt: TPoint read FWindowExt write SetWindowExt;
    property WindowOrg: TPoint read FWindowOrg write SetWindowOrg;
    // control
    property SelectedColors: TDevContextSelectedColorsType read FSelectedColors write SetSelectedColors;
    property Flags: TDeviceContextsFlags read FFlags write FFlags;
    property OwnedGDIObjects[ID: TGDIType]: PGdiObject read GetOwnedGDIObjects write SetOwnedGDIObjects;
    property Drawable: PGDKDrawable read FDrawable;
    property Pixbuf: PGdkPixbuf read FPixbuf;
    property Widget: PGtkWidget read FWidget; // the owner
    property GC: pgdkGC read GetGC write FGC;
    property WithChildWindows: Boolean read FWithChildWindows;
    // winapi
    property ROP2: Integer read GetRop2 write SetRop2;
  end;

  // memory system for TDeviceContext(s) ---------------------------------------------

  { TDeviceContextMemManager }

  TDeviceContextMemManager = class(TLCLMemManager)
  private
    FDeviceContextClass: TGtkDeviceContextClass;
  protected
    procedure FreeFirstItem; override;
  public
    constructor Create(AClass: TGtkDeviceContextClass);
    procedure DisposeDeviceContext(ADeviceContext: TGtkDeviceContext);
    function NewDeviceContext: TGtkDeviceContext;
  end;


  TWidgetInfoFlag = (
    wwiNotOnParentsClientArea,
    wwiValidQueuedEvent,              // Mark this widgetinfo as valid queued proc
                                      // see gtk2wsmenus.pp: gtkWSPopupMenuDeactivate
    wwiDeactivating,                  // during gtk deactivate
    wwiActivating,                    // during gtk activate
    wwiNoEraseBkgnd,                  // erase background is disabled for widget
    wwiInvalidEvent,                  // special mark for widgetinfo
                                      // see gtkchanged_editbox and
                                      // gtkchanged_editbox_backspace in gtkcallback.inc
    wwiTabWidgetFocusCheck            // TabWidget have nasty behaviour when clicked
                                      // by mouse: switches focus here and there, so
                                      // focused control triggers OnExit and it looks
                                      // like it triggered OnEnter.issue #20493
    );
  TWidgetInfoFlags = set of TWidgetInfoFlag;
  tGtkStateEnumRange = 0..31;
  tGtkStateEnum = set of tGtkStateEnumRange;

  // Info needed by the API of a HWND (=Widget)
  PWidgetInfo = ^TWidgetInfo;
  TWidgetInfo = record
    LCLObject: TObject;               // the object which created this widget
    ClientWidget: PGTKWidget;         // the widget which contains the childwidgets
                                      // used to be "fixed" or "core-child"
    CoreWidget: PGTKWidget;           // the widget which implements the main functionality
                                      // For a TListBox the GTKList is the CoreWidget
                                      // and the scrollbox around it is the handle
                                      // So in most cases handle = CoreWidget
    UpdateRect: TRect;                // used by LM_Paint, beginpaint etc
    WndProc: Integer;                 // window data
    Style: Integer;
    ExStyle: Integer;
    EventMask: TGdkEventMask;
    DoubleBuffer: PGdkPixmap;
    CursorPos: integer;               // needed for delayed SetSelStart
    SelLength: integer;               // needed for delayed SetSelLength
    ControlCursor: HCursor;           // current widget cursor
    Flags: TWidgetInfoFlags;
    ChangeLock: Integer;              // lock events
    PaintDepth: integer;              // increased/decreased by Begin/EndPaint
    DataOwner: Boolean;               // Set if the UserData should be freed when the info is freed
    UserData: Pointer;
    FormBorderStyle: Integer;         // used only by forms
    FormWindowState: TGdkEventWindowState; // used only by forms to stop infinite loops eg. issue #16505
  end;

  //TODO: remove
  PWinWidgetInfo = ^TWidgetInfo;
  TWinWidgetInfo = TWidgetInfo;
  //--


const
  GdkTrue = true;
  GdkFalse = false;


  GTK_STYLE_BASE = 20;// see GTK_STATE_NORMAL..GTK_STATE_INSENSITIVE,
  GTK_STYLE_TEXT = 21;// see tGtkStateEnum, and see TGtkWidgetSet.SetWidgetColor


type
  TGdkPixBufBuffer = Pguchar;


const
  GDK_VOIDSYMBOL = $FFFFFF;

  GDK_KEY_ISO_Level5_Shift = $FE11;
  GDK_KEY_ISO_Level5_Latch = $FE12;
  GDK_KEY_ISO_Level5_Lock = $FE13;

// MWE:
// Additional GDK_KEY_xxx definitions, not defined in GDK. Since GDK (on Linux)
// simply passes the X vvalue I definde those extra here as GDKX_KEY_xxx
// I don't know what the values are in win32 so I assume the same
// Original source: /usr/X11R6/include/X11/XF86keysym.h

// Keys found on some "Internet" keyboards.
const
  GDKX_KEY_Standby          = $1008FF10;
  GDKX_KEY_AudioLowerVolume = $1008FF11;
  GDKX_KEY_AudioMute        = $1008FF12;
  GDKX_KEY_AudioRaiseVolume = $1008FF13;
  GDKX_KEY_AudioPlay        = $1008FF14;
  GDKX_KEY_AudioStop        = $1008FF15;
  GDKX_KEY_AudioPrev        = $1008FF16;
  GDKX_KEY_AudioNext        = $1008FF17;
  GDKX_KEY_HomePage         = $1008FF18;
  GDKX_KEY_Mail             = $1008FF19;
  GDKX_KEY_Start            = $1008FF1A;
  GDKX_KEY_Search           = $1008FF1B;
  GDKX_KEY_AudioRecord      = $1008FF1C;

// These are sometimes found on PDA's (e.g. Palm, PocketPC or elsewhere)
  GDKX_KEY_Calculator       = $1008FF1D;
  GDKX_KEY_Memo             = $1008FF1E;
  GDKX_KEY_ToDoList         = $1008FF1F;
  GDKX_KEY_Calendar         = $1008FF20;
  GDKX_KEY_PowerDown        = $1008FF21;
  GDKX_KEY_ContrastAdjust   = $1008FF22;
  GDKX_KEY_RockerUp         = $1008FF23;
  GDKX_KEY_RockerDown       = $1008FF24;
  GDKX_KEY_RockerEnter      = $1008FF25;

// Some more "Internet" keyboard symbols
  GDKX_KEY_Back             = $1008FF26;
  GDKX_KEY_Forward          = $1008FF27;
  GDKX_KEY_Stop             = $1008FF28;
  GDKX_KEY_Refresh          = $1008FF29;
  GDKX_KEY_PowerOff         = $1008FF2A;
  GDKX_KEY_WakeUp           = $1008FF2B;
  GDKX_KEY_Eject            = $1008FF2C;
  GDKX_KEY_ScreenSaver      = $1008FF2D;
  GDKX_KEY_WWW              = $1008FF2E;
  GDKX_KEY_Sleep            = $1008FF2F;
  GDKX_KEY_Favorites        = $1008FF30;
  GDKX_KEY_AudioPause       = $1008FF31;
  GDKX_KEY_AudioMedia       = $1008FF32;
  GDKX_KEY_MyComputer       = $1008FF33;
  GDKX_KEY_VendorHome       = $1008FF34;
  GDKX_KEY_LightBulb        = $1008FF35;
  GDKX_KEY_Shop             = $1008FF36;
  GDKX_KEY_History          = $1008FF37;
  GDKX_KEY_OpenURL          = $1008FF38;
  GDKX_KEY_AddFavorite      = $1008FF39;
  GDKX_KEY_HotLinks         = $1008FF3A;
  GDKX_KEY_BrightnessAdjust = $1008FF3B;
  GDKX_KEY_Finance          = $1008FF3C;
  GDKX_KEY_Community        = $1008FF3D;

  GDKX_KEY_Launch0          = $1008FF40;
  GDKX_KEY_Launch1          = $1008FF41;
  GDKX_KEY_Launch2          = $1008FF42;
  GDKX_KEY_Launch3          = $1008FF43;
  GDKX_KEY_Launch4          = $1008FF44;
  GDKX_KEY_Launch5          = $1008FF45;
  GDKX_KEY_Launch6          = $1008FF46;
  GDKX_KEY_Launch7          = $1008FF47;
  GDKX_KEY_Launch8          = $1008FF48;
  GDKX_KEY_Launch9          = $1008FF49;
  GDKX_KEY_LaunchA          = $1008FF4A;
  GDKX_KEY_LaunchB          = $1008FF4B;
  GDKX_KEY_LaunchC          = $1008FF4C;
  GDKX_KEY_LaunchD          = $1008FF4D;
  GDKX_KEY_LaunchE          = $1008FF4E;
  GDKX_KEY_LaunchF          = $1008FF4F;


function InternalNewPGDIObject: PGDIObject;
procedure InternalDisposePGDIObject(GDIObject: PGdiObject);
type
  TReferenceGDIObject = procedure(GDIObject: PGdiObject) of object;
  TReleaseGDIObject = function(GDIObject: PGdiObject): boolean of object;
var
  ReleaseGDIObject: TReleaseGDIObject; // see TGtkWidgetSet.ReleaseGDIObject
  ReferenceGDIObject: TReferenceGDIObject; // see TGtkWidgetSet.ReferenceGDIObject

{$IFDEF DebugLCLComponents}
var
  DebugGtkWidgets: TDebugLCLItems = nil;
  DebugGdiObjects: TDebugLCLItems = nil;
  DebugDeviceContexts: TDebugLCLItems = nil;
{$ENDIF}

procedure GtkDefDone;

function dbgs(g: TGDIType): string; overload;
function dbgs(const r: TGDKRectangle): string; overload;
function dbgs(r: PGDKRectangle): string; overload;

procedure SetLayoutText(ALayout: PPangoLayout; AText: PChar; ALength: PtrInt);

implementation

uses
  // until all code is transfered to objects, these circles are needed;
  Gtk2Int, Gtk2Proc, Gtk2FontCache, Gtk2WinApiWindow;

{$IFOpt R+}{$Define RangeChecksOn}{$Endif}

// memory system for PGDIObject(s) ---------------------------------------------
type
  TGDIObjectMemManager = class(TLCLMemManager)
  protected
    procedure FreeFirstItem; override;
  public
    procedure DisposeGDIObjectMem(AGDIObject: PGDIObject);
    function NewGDIObjectMem: PGDIObject;
  end;

const
  GDIObjectMemManager: TGDIObjectMemManager = nil;

function InternalNewPGDIObject: PGDIObject;
begin
  if GDIObjectMemManager=nil then begin
    GDIObjectMemManager:=TGDIObjectMemManager.Create;
    GDIObjectMemManager.MinimumFreeCount:=1000;
  end;
  Result:=GDIObjectMemManager.NewGDIObjectMem;
  {$IFDEF DebugLCLComponents}
  DebugGdiObjects.MarkCreated(Result,'NewPGDIObject');
  {$ENDIF}
end;

procedure InternalDisposePGDIObject(GDIObject: PGdiObject);
begin
  {$IFDEF DebugLCLComponents}
  DebugGdiObjects.MarkDestroyed(GDIObject);
  {$ENDIF}
  GDIObjectMemManager.DisposeGDIObjectMem(GDIObject);
end;

{ TGDIObjectMemManager }

procedure TGDIObjectMemManager.FreeFirstItem;
var AGDIObject: PGDIObject;
begin
  AGDIObject:=PGDIObject(FFirstFree);
  PGDIObject(FFirstFree):=AGDIObject^.Next;
  Dispose(AGDIObject);
  //DebugLn('TGDIObjectMemManager.DisposeGDIObject A FFreedCount=',FFreedCount);
  {$R-}
  inc(FFreedCount);
  {$IfDef RangeChecksOn}{$R+}{$Endif}
end;

procedure TGDIObjectMemManager.DisposeGDIObjectMem(AGDIObject: PGDIObject);
begin
  //DebugLn('TGDIObjectMemManager.DisposeGDIObjectMem ',DbgS(AGDIObject));
  if AGDIObject^.RefCount<>0 then
    RaiseGDBException('');
  if (FFreeCount<FMinFree) or (FFreeCount<((FCount shr 3)*FMaxFreeRatio)) then
  begin
    // add AGDIObject to Free list
    AGDIObject^.Next:=PGDIObject(FFirstFree);
    PGDIObject(FFirstFree):=AGDIObject;
    inc(FFreeCount);
  end else begin
    // free list full -> free the ANode
    Dispose(AGDIObject);
    //DebugLn('TGDIObjectMemManager.DisposeGDIObjectMem B FFreedCount=',FFreedCount);
    {$R-}
    inc(FFreedCount);
    {$IfDef RangeChecksOn}{$R+}{$Endif}
  end;
  dec(FCount);
end;

function TGDIObjectMemManager.NewGDIObjectMem: PGDIObject;
begin
  if FFirstFree<>nil then begin
    // take from free list
    Result:=PGDIObject(FFirstFree);
    PGDIObject(FFirstFree):=Result^.Next;
    dec(FFreeCount);
  end else begin
    // free list empty -> create new node
    New(Result);
    // DebugLn('TGDIObjectMemManager.NewGDIObjectMem FAllocatedCount=',FAllocatedCount);
    {$R-}
    inc(FAllocatedCount);
    {$IfDef RangeChecksOn}{$R+}{$Endif}
  end;
  FillChar(Result^, SizeOf(TGDIObject), 0);
  inc(FCount);
  //DebugLn('TGDIObjectMemManager.NewGDIObjectMem ',DbgS(Result));
end;



{ TDeviceContextMemManager }

procedure TDeviceContextMemManager.FreeFirstItem;
var ADeviceContext: TGtkDeviceContext;
begin
  ADeviceContext:=TGtkDeviceContext(FFirstFree);
  TGtkDeviceContext(FFirstFree):=ADeviceContext.SavedContext;
  //DebugLn('TDeviceContextMemManager.FreeFirstItem FFreedCount=',FFreedCount);
  ADeviceContext.Free;
  {$R-}
  inc(FFreedCount);
  {$IfDef RangeChecksOn}{$R+}{$Endif}
end;

constructor TDeviceContextMemManager.Create(AClass: TGtkDeviceContextClass);
begin
  inherited Create;
  FDeviceContextClass := AClass;
end;

procedure TDeviceContextMemManager.DisposeDeviceContext(
  ADeviceContext: TGtkDeviceContext);
begin
  if (FFreeCount<FMinFree) or (FFreeCount<((FCount shr 3)*FMaxFreeRatio))
  then begin
    // add ADeviceContext to Free list
    ADeviceContext.SavedContext:=TGtkDeviceContext(FFirstFree);
    TGtkDeviceContext(FFirstFree):=ADeviceContext;
    inc(FFreeCount);
  end
  else begin
    // free list full -> free the ANode
    //DebugLn('TDeviceContextMemManager.DisposeDeviceContext FFreedCount=',FFreedCount);
    ADeviceContext.Free;
    {$R-}
    inc(FFreedCount);
    {$IfDef RangeChecksOn}{$R+}{$Endif}
  end;
  dec(FCount);
end;

function TDeviceContextMemManager.NewDeviceContext: TGtkDeviceContext;
begin
  if FFirstFree <> nil
  then begin
    // take from free list
    Result := TGtkDeviceContext(FFirstFree);
    TGtkDeviceContext(FFirstFree) := Result.SavedContext;
    Dec(FFreeCount);
    Result.Clear;
  end
  else begin
    // free list empty -> create new node
    Result := FDeviceContextClass.Create;
    //DebugLn('TDeviceContextMemManager.NewDeviceContext FAllocatedCount=',FAllocatedCount);
    {$R-}
    inc(FAllocatedCount);
    {$IfDef RangeChecksOn}{$R+}{$Endif}
  end;
  Inc(FCount);
end;


//------------------------------------------------------------------------------

procedure GtkDefInit;
begin
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets:=TDebugLCLItems.Create('GtkDefInit.DebugGtkWidgets');
  DebugGdiObjects:=TDebugLCLItems.Create('GtkDefInit.DebugGdiObjects');
  DebugDeviceContexts:=TDebugLCLItems.Create('GtkDefInit.DebugDeviceContexts');
  {$ENDIF}
end;

procedure GtkDefDone;
begin
  GDIObjectMemManager.Free;
  GDIObjectMemManager:=nil;
  {$IFDEF DebugLCLComponents}
  FreeAndNil(DebugGtkWidgets);
  FreeAndNil(DebugGdiObjects);
  FreeAndNil(DebugDeviceContexts);
  {$ENDIF}
end;

function dbgs(g: TGDIType): string;
begin
  case g of
  gdiBitmap: Result:='gdiBitmap';
  gdiBrush: Result:='gdiBrush';
  gdiFont: Result:='gdiFont';
  gdiPen: Result:='gdiPen';
  gdiRegion: Result:='gdiRegion';
  gdiPalette: Result:='gdiPalette';
  else Result:='<?? unknown gdi type '+dbgs(ord(g))+'>';
  end;
end;

function dbgs(const r: TGDKRectangle): string;
begin
  Result:=dbgs(Bounds(r.x,r.y,r.width,r.height));
end;

function dbgs(r: PGDKRectangle): string;
begin
  if r=nil then
    Result:='nil'
  else
    Result:=dbgs(r^);
end;

{$i gtk2devicecontext.inc}

initialization
  GtkDefInit;


end.
