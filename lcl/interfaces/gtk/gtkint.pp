{
 /***************************************************************************
                         GTKINT.pp  -  GTKInterface Object
                             -------------------

                   Initial Revision  : Thu July 1st CST 1999


 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
 }

unit GtkInt;

{$mode objfpc}
{$LONGSTRINGS ON}

interface

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}


{$I gtkdefines.inc}

uses
  {$IFDEF WIN32}
  // use windows unit first,
  // if not, Rect and Point are taken from the windows unit instead of classes.
  Windows,
  {$ENDIF}
  {$IFDEF UNIX}
  // use unix units first,
  // if not, TSize is taken from the unix unit instead of types.
  ctypes, baseunix, unix,
  {$ENDIF}
  {$IFDEF TraceGdiCalls}
  LineInfo,
  {$ENDIF}
  // rtl+fcl
  Types, Classes, SysUtils,
  // LazUtils
  FPCAdds, LazUTF8,
  // gtk
  {$IFDEF gtk2}
    glib2, gdk2pixbuf, gdk2, gtk2, Pango, gtk2proc,
    {$ifdef HasGdk2X}
      gdk2x,
    {$endif}
  {$ELSE}
    glib, gdk, gtk, gdkpixbuf,
  {$ENDIF}
  // Target OS specific
  {$ifdef HasX}
  x, xlib,
  {$endif}
  Math, // after gtk to get the correct Float type
  // LCL
  LCLPlatformDef, InterfaceBase,
  FileUtil, Translations, ExtDlgs, Dialogs, Controls, Forms, LCLStrConsts,
  LMessages, LCLProc, LCLIntf, LCLType, DynHashArray, GraphType, GraphMath,
  Graphics, Menus, Maps, LazLoggerBase, LazFileUtils, LazStringUtils, Themes,
  // widgetset
  GtkDebug, GtkFontCache, gtkDef, GtkProc, gtkMsgQueue, GtkExtra, WSLCLClasses;

type

  { TGTKWidgetSet }

  TGTKWidgetSet = class(TWidgetSet)
  private
    FMultiThreadingEnabled: boolean;
    FocusTimer: cardinal;
    FAppActive: Boolean;
    FLastFocusIn: PGtkWidget;
    FLastFocusOut: PGtkWidget;
    function GetAppActive: Boolean;
    procedure SetAppActive(const AValue: Boolean);
  protected
    FKeyStateList_: TFPList; // Keeps track of which keys are pressed
    FDeviceContexts: TDynHashArray;// hasharray of HDC
    FGDIObjects: TDynHashArray;    // hasharray of PGdiObject
    FMessageQueue: TGtkMessageQueue; // queue of PMsg (must be thread safe!)
    WaitingForMessages: boolean;
    MovedPaintMessageCount: integer;// how many paint messages moved to he end of the queue

    FRCFilename: string;
    FRCFileParsed: boolean;
    FRCFileAge: integer;
    FGTKToolTips: PGtkToolTips;

    FLogHandlerID: guint; // ID returend by set_handler

    FStockNullBrush: HBRUSH;
    FStockBlackBrush: HBRUSH;
    FStockLtGrayBrush: HBRUSH;
    FStockGrayBrush: HBRUSH;
    FStockDkGrayBrush: HBRUSH;
    FStockWhiteBrush: HBRUSH;

    FStockNullPen: HPEN;
    FStockBlackPen: HPEN;
    FStockWhitePen: HPEN;

    FSysColorBrushes: array[0..MAX_SYS_COLORS] of HBrush;

    FWaitHandles: PWaitHandleEventHandler;
    {$ifdef unix}
    FChildSignalHandlers: PChildSignalEventHandler;
    {$else}
    {$IFDEF VerboseGtkToDos}{$warning no declaration of FChildSignalHandlers for this OS}{$ENDIF}
    {$endif}

    {$Ifdef GTK2}
    FDefaultFontDesc: PPangoFontDescription;
    {$Endif}
    FDefaultFont: TGtkIntfFont;
    FStockSystemFont: HFONT;
    FExtUTF8OutCache: Pointer;
    FExtUTF8OutCacheSize: integer;
    FGlobalCursor: HCursor;
    
    FDCManager: TDeviceContextMemManager;
    FDockImage: PGtkWidget;
    FDragImageList: PGtkWidget;
    FDragImageListIcon: PGtkWidget;
    FDragHotStop: TPoint;

    function CreateThemeServices: TThemeServices; override;
    function GetDeviceContextClass: TGtkDeviceContextClass; virtual; abstract;
  public
    procedure InitStockItems; virtual;
    procedure FreeStockItems; virtual;
    procedure InitSystemColors;
    procedure InitSystemBrushes; virtual;
    procedure FreeSystemBrushes; virtual;
    procedure PassCmdLineOptions; override;
   
{$ifdef Unix}
    procedure InitSynchronizeSupport;
    procedure ProcessChildSignal;
    procedure PrepareSynchronize(AObject: TObject);
{$endif}  

    procedure HandlePipeEvent(AData: PtrInt; AFlags: dword);

    // styles
    procedure FreeAllStyles; virtual;
    function GetCompStyle(Sender : TObject) : Longint; virtual;

    // create and destroy
    function CreateAPIWidget(AWinControl: TWinControl): PGtkWidget;
    function OldCreateStatusBarPanel(StatusBar: TObject; Index: integer): PGtkWidget;
    function CreateSimpleClientAreaWidget(Sender: TObject;
      NotOnParentsClientArea: boolean): PGtkWidget;
    procedure DestroyEmptySubmenu(Sender: TObject);virtual;
    procedure DestroyConnectedWidget(Widget: PGtkWidget;
                                     CheckIfDestroying: boolean);virtual;
    function  RecreateWnd(Sender: TObject): Integer; virtual;

    // clipboard
    procedure SetClipboardWidget(TargetWidget: PGtkWidget);virtual;

    // device contexts
    function IsValidDC(const DC: HDC): Boolean;virtual;
    function NewDC: TGtkDeviceContext;virtual;
    function FindDCWithGDIObject(GDIObject: PGdiObject): TGtkDeviceContext;virtual;
    procedure DisposeDC(aDC: TGtkDeviceContext);virtual;
    function CreateDCForWidget(AWidget: PGtkWidget; AWindow: PGdkWindow;
                               AWithChildWindows: Boolean; ADoubleBuffer: PgdkDrawable = nil): HDC;
    function GetDoubleBufferedDC(Handle: HWND): HDC;

    // GDIObjects
    function IsValidGDIObject(const AGDIObj: HGDIOBJ): Boolean; virtual;
    function IsValidGDIObjectType(const GDIObject: HGDIOBJ;
                                  const GDIType: TGDIType): Boolean;virtual;
    function NewGDIObject(const GDIType: TGDIType): PGdiObject;virtual;
    procedure DisposeGDIObject(GdiObject: PGdiObject);virtual;
    function ReleaseGDIObject(GdiObject: PGdiObject): boolean;virtual;
    procedure ReferenceGDIObject(GdiObject: PGdiObject);virtual;
    function CreateDefaultBrush: PGdiObject;virtual;
    function CreateDefaultFont: PGdiObject;virtual;
    function CreateDefaultPen: PGdiObject;virtual;
    function CreateDefaultGDIBitmap: PGdiObject;virtual;
    procedure UpdateDCTextMetric(DC: TGtkDeviceContext); virtual;
    {$Ifdef GTK2}
    function GetDefaultFontDesc(IncreaseReferenceCount: boolean): PPangoFontDescription;
    {$Endif}
    function GetDefaultGtkFont(IncreaseReferenceCount: boolean): TGtkIntfFont;
    function GetGtkFont(DC: TGtkDeviceContext): TGtkIntfFont;
    function CreateRegionCopy(SrcRGN: hRGN): hRGN; override;
    function DCClipRegionValid(DC: HDC): boolean; override;
    function CreateEmptyRegion: hRGN; override;

    // images
    procedure LoadPixbufFromLazResource(const ResourceName: string;
      var Pixbuf: PGdkPixbuf);
    function InternalGetDIBits(DC: HDC; Bitmap: HBitmap; StartScan, NumScans: UINT;
      BitSize : Longint; Bits: Pointer; var BitInfo: BitmapInfo; Usage: UINT; DIB : Boolean): Integer;virtual;
    function RawImage_DescriptionFromDrawable(out ADesc: TRawImageDescription; ADrawable: PGdkDrawable; ACustomAlpha: Boolean): boolean;
    function RawImage_DescriptionFromPixbuf(out ADesc: TRawImageDescription; APixbuf: PGdkPixbuf): boolean;
    function RawImage_FromDrawable(out ARawImage: TRawImage; ADrawable, AAlpha: PGdkDrawable; ARect: PRect = nil): boolean;
    function RawImage_FromPixbuf(out ARawImage: TRawImage; APixbuf: PGdkPixbuf; ARect: PRect = nil): boolean;
    function RawImage_SetAlpha(var ARawImage: TRawImage; AAlpha: PGdkPixmap; ARect: PRect = nil): boolean;
    function RawImage_AddMask(var ARawImage: TRawImage; AMask: PGdkBitmap; ARect: PRect = nil): boolean;
    function StretchCopyArea(DestDC: HDC; X, Y, Width, Height: Integer;
      SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer;
      Mask: HBITMAP; XMask, YMask: Integer;
      Rop: Cardinal): Boolean;

    // RC file
    procedure SetRCFilename(const AValue: string);virtual;
    procedure CheckRCFilename;virtual;
    procedure ParseRCFile;virtual;

    // forms and dialogs
    procedure BringFormToFront(Sender: TObject);
    procedure UntransientWindow(GtkWindow: PGtkWindow);
    // misc
    function GetCaption(Sender : TObject) : String; virtual;
    procedure WordWrap(DC: HDC; AText: PChar; MaxWidthInPixel: integer;
      var Lines: PPChar; var LineCount: integer);

    procedure ResizeChild(Sender : TObject; Left,Top,Width,Height : Integer);virtual;
    procedure RemoveCallbacks(Widget: PGtkWidget); virtual;

    // for gtk specific components:
    procedure SetLabelCaption(const ALabel: PGtkLabel; const ACaption: String
                              {$IFDEF Gtk1}
                              ; const AComponent: TComponent = nil;
                                const ASignalWidget: PGTKWidget = nil;
                                const ASignal: PChar = nil{$ENDIF}); virtual; abstract;
    procedure SetWidgetColor(const AWidget: PGtkWidget;
                             const FGColor, BGColor: TColor;
                             const Mask: tGtkStateEnum);
    procedure SetWidgetFont(const AWidget : PGtkWidget;const AFONT : tFont); virtual; abstract;
    procedure SetCallbackEx(const AMsg: LongInt; const AGTKObject: PGTKObject;
                          const ALCLObject: TObject; Direct: boolean); virtual;
    procedure SetCallbackDirect(const AMsg: LongInt; const AGTKObject: PGTKObject;
                          const ALCLObject: TObject);
    procedure SetCallback(const AMsg: LongInt; const AGTKObject: PGTKObject;
                          const ALCLObject: TObject);
    procedure SetCommonCallbacks(const AGTKObject: PGTKObject; const ALCLObject: TObject); virtual;
    function  LCLtoGtkMessagePending: boolean;virtual;
    procedure SendCachedGtkMessages;virtual;
    // show, hide and invalidate
    procedure SetVisible(Sender: TObject; const AVisible: Boolean); virtual;
    
    // Drag ImageLsit
    function DragImageList_BeginDrag(APixmap: PGdkPixmap; AMask: PGdkBitmap; AHotSpot: TPoint): Boolean;
    procedure DragImageList_EndDrag;
    function DragImageList_DragMove(X, Y: Integer): Boolean;
    function DragImageList_SetVisible(NewVisible: Boolean): Boolean;
    
  public
    function LCLPlatform: TLCLPlatform; override;
    // Application
    procedure AppInit(var ScreenInfo: TScreenInfo); override;
    procedure AppProcessMessages; override;
    procedure AppWaitMessage; override;
    procedure AppTerminate; override;
    procedure AppMinimize; override;
    procedure AppRestore; override;
    procedure AppBringToFront; override;
    procedure AppSetTitle(const ATitle: string); override;
    // notebook
    procedure AddDummyNoteBookPage(NoteBookWidget: PGtkNoteBook);virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SendCachedLCLMessages; override;
    function  DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor; override;
    procedure DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor); override;
    procedure DCRedraw(CanvasHandle: HDC); override;
    procedure SetDesigning(AComponent: TComponent); override;

    // helper routines needed by interface methods
    // |-forms
    procedure UpdateTransientWindows; virtual;
    // |-listbox
    procedure SetSelectionMode(Sender: TObject; Widget: PGtkWidget;
                               MultiSelect, ExtendedSelect: boolean); virtual;
    function ForceLineBreaks(DC : hDC; Src: PChar; MaxWidthInPixels : Longint;
                             ConvertAmpersandsToUnderScores: Boolean) : PChar;

    // create and destroy
    function CreateTimer(Interval: integer; TimerProc: TWSTimerProc) : THandle; override;
    function DestroyTimer(TimerHandle: THandle) : boolean; override;
    procedure DestroyLCLComponent(Sender: TObject);virtual;

    // for gtk controls not part of the LCL:
    procedure FinishCreateHandle(const AWinControl: TWinControl; Widget: PGtkWidget; const AParams: TCreateParams);

    {$I gtkwinapih.inc}
    {$I gtklclintfh.inc}

  public

    // special methods and properties to track app activation / deactivation
    procedure StartFocusTimer;
    property AppActive: Boolean read GetAppActive write SetAppActive;
    property LastFocusIn: PGtkWidget read FLastFocusIn write FLastFocusIn;
    property LastFocusOut: PGtkWidget read FLastFocusOut write FLastFocusOut;

    property RCFilename: string read FRCFilename write SetRCFilename;
    property MultiThreadingEnabled: boolean read FMultiThreadingEnabled;
  end;

{$I gtklistslh.inc}
{$I gtkfiledialogutilsh.inc}

var
  GTKWidgetSet: TGTKWidgetSet;

implementation

uses
////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To get as little as possible circles,
// uncomment only those units with implementation
////////////////////////////////////////////////////
// GtkWSActnList,
 GtkWSButtons,
 GtkWSCalendar,
 GtkWSCheckLst,
 GtkWSComCtrls,
 GtkWSControls,
// GtkWSDbCtrls,
// GtkWSDBGrids,
 GtkWSDialogs,
// GtkWSEditBtn,
 GtkWSExtCtrls,
 GtkWSExtDlgs,
// GtkWSFileCtrl,
 GtkWSForms,
 GtkWSGrids,
// GtkWSImgList,
// GtkWSMaskEdit,
 GtkWSMenus,
 GtkWSPairSplitter,
 GtkWSSpin,
 GtkWSStdCtrls,
// GtkWSToolwin,
////////////////////////////////////////////////////
  GtkWSPrivate,
  GtkThemes,
  Buttons, StdCtrls, PairSplitter,
  GTKWinApiWindow, ComCtrls, Calendar, Spin,
  ExtCtrls, FileCtrl, LResources, gtkglobals;

{$I gtklistsl.inc}
{$I gtkfiledialogutils.inc}
{$I gtkwidgetset.inc}
{$I gtkwinapi.inc}
{$I gtklclintf.inc}


procedure InternalInit;
var
  c: TClipboardType;
begin
  gtk_handler_quark := g_quark_from_static_string('gtk-signal-handlers');

  MouseCaptureWidget := nil;
  MouseCaptureType := mctGTK;

  LastLeft:=EmptyLastMouseClick;
  LastMiddle:=EmptyLastMouseClick;
  LastRight:=EmptyLastMouseClick;

  // clipboard
  ClipboardSelectionData:=TFPList.Create;
  for c:=Low(TClipboardType) to High(TClipboardType) do begin
    ClipboardTypeAtoms[c]:=0;
    ClipboardHandler[c]:=nil;
    //ClipboardIgnoreLossCount[c]:=0;
    ClipboardTargetEntries[c]:=nil;
    ClipboardTargetEntryCnt[c]:=0;
  end;

  // charset encodings
  {$IFDEF Gtk1}
  SystemCharSetIsUTF8:=not NeedRTLAnsi;
  {$ENDIF}

  CharSetEncodingList := TList.Create;
  CreateDefaultCharsetEncodings;
  
  InitDesignSignalMasks;
end;

procedure InternalFinal;
var i: integer;
  ced: PClipboardEventData;
  c: TClipboardType;
begin
  // clipboard
  for i:=0 to ClipboardSelectionData.Count-1 do begin
    ced:=PClipboardEventData(ClipboardSelectionData[i]);
    if ced^.Data.Data<>nil then FreeMem(ced^.Data.Data);
    Dispose(ced);
  end;
  for c:=Low(TClipboardType) to High(TClipboardType) do
    FreeClipboardTargetEntries(c);
  ClipboardSelectionData.Free;
  ClipboardSelectionData:=nil;
  
  // charset encodings
  if CharSetEncodingList<>nil then begin
    ClearCharSetEncodings;
    CharSetEncodingList.Free;
    CharSetEncodingList:=nil;
  end;
end;


initialization
{$IFDEF GTK1}
  {$I gtkimages.lrs}
{$ENDIF}
  InternalInit;

finalization
  InternalFinal;

end.
