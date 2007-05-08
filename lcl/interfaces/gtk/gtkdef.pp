{ $Id$
                         ------------------------------ 
                         gtkdef.pp  -  Type definitions
                         ------------------------------ 
 
 @created(Wed Jan 24st WET 2001)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)                       

 This unit contains type definitions needed in the GTK <-> LCL interface
 
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


unit GTKDef;
 
{$mode objfpc} 
{$LONGSTRINGS ON}

interface

uses
  {$IFDEF gtk2}
  glib2, gdk2pixbuf, pango, gdk2, gtk2,
  {$ELSE}
  glib, gdk, gtk, {$Ifndef NoGdkPixbufLib}gdkpixbuf,{$EndIf}
  {$ENDIF}
  Classes, SysUtils, LCLIntf, LCLProc, LCLType, LCLMemManager, DynHashArray,
  GraphType;
  
{$ifdef TraceGdiCalls}
const
  MaxTraces    = 5;
  MaxCallBacks = 11;
type
  TCallBacksArray = array[0..MaxCallBacks] of Pointer;
  PCallBacksArray = ^TCallBacksArray;
{$endif}

type
  TGDIType = (gdiBitmap, gdiBrush, gdiFont, gdiPen, gdiRegion, gdiPalette);
  TGDIBitmapType = (gbBitmap, gbPixmap{obsolete:, gbImage});

  {$IFDEF Gtk1}
  TGtkIntfFont = PGDKFont;
  {$ELSE}
  TGtkIntfFont = PPangoLayout;
  {$ENDIF}

  PGDIRGB = ^TGDIRGB;
  TGDIRGB = record
    Red,
    Green,
    Blue: Byte;
  end;

  {obsolete:
  PGDI_RGBImage = ^TGDI_RGBImage;
  TGDI_RGBImage = record
    Height,
    Width: Integer;
    Depth: Byte;
    Data: array[0..0] of TGDIRGB;
  end;}
  
  TGDIColorFlag = (cfColorAllocated);
  TGDIColorFlags = set of TGDIColorFlag;
  
  TGDIColor = record
    ColorRef : TColorRef;    //Color passed - can be a SYSCOLOR or RGB
    ColorFlags: TGDIColorFlags;
    Color: TGDKColor;       //Actual GDK Color(If any) for use with GC's
    Colormap : PGDKColormap; //Colormap GDKColor was allocated with
  end;
  PGDIColor = ^TGDIColor;

  PGDIObject = ^TGDIObject;
  TGDIObject = record
    RefCount: integer;
    {$ifdef TraceGdiCalls}
    StackAddrs: TCallBacksArray;
    {$endif}
    Next: PGDIObject; // 'Next' is used by the internal mem manager
    case GDIType: TGDIType of
      gdiBitmap: (
        GDIBitmapMaskObject: PGdkPixmap;
        Depth: integer;
        SystemVisual : Boolean;
        Visual : PGDKVisual;
        Colormap : PGDKColormap;
        case GDIBitmapType: TGDIBitmapType of
          gbBitmap: (GDIBitmapObject: PGdkBitmap); // pixmap with depth 1
          gbPixmap: (GDIPixmapObject: PGdkPixmap); // normal pixmap
          {obsolete: gbImage : (GDI_RGBImageObject: PGDI_RGBImage);}
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
      );
      gdiPen: (
        IsNullPen : Boolean;//GDK will bomb with a NULL Pen Hatch
        GDIPenColor: TGDIColor;
        GDIPenWidth: Integer;
        GDIPenStyle: Word;
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
    
  { TDeviceContext }

  TDeviceContext = class
  public
    WithChildWindows: boolean;// this DC covers sub gdkwindows
  
    // device handles
    DCWidget: PGtkWidget; // the owner
    Drawable: PGDKDrawable;
    OriginalDrawable: PGDKDrawable; // only set if dcfDoubleBuffer in DCFlags
    GC: pgdkGC;
    GCValues: TGdkGCValues;

    // origins
    Origin: TPoint;
    SpecialOrigin: boolean;
    PenPos: TPoint;
    
    {$ifdef TraceGdiCalls}
    StackAddrs: TCallBacksArray;
    {$endif}
    
    // drawing settings
    CurrentBitmap: PGdiObject;
    CurrentFont: PGdiObject;
    CurrentPen: PGdiObject;
    CurrentBrush: PGdiObject;
    CurrentPalette: PGdiObject;
    CurrentTextColor: TGDIColor;
    CurrentBackColor: TGDIColor;
    ClipRegion: hRGN;
    DCTextMetric: TDevContextTextMetric; // only valid if dcfTextMetricsValid set
    
    // control
    SelectedColors: TDevContextSelectedColorsType;
    SavedContext: TDeviceContext; // linked list of saved DCs
    DCFlags: TDeviceContextsFlags;

    procedure Clear;
    function GetGC: pgdkGC;
  end;
  
  
  TWidgetInfoFlag = (
    wwiNotOnParentsClientArea
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
    ControlCursor: HCursor;           // cursor, that control contain
    Flags: TWidgetInfoFlags;
    ChangeLock: Integer;              // lock events
    DataOwner: Boolean;               // Set if the UserData should be freed when the info is freed
    UserData: Pointer;
  end;
  
  //TODO: remove
  PWinWidgetInfo = ^TWidgetInfo;
  TWinWidgetInfo = TWidgetInfo;
  //--
  
  
// clipboard
type
  TClipboardEventData = record
    TimeID: Cardinal;
    Waiting: boolean;
    Stopping: boolean;
    Data: TGtkSelectionData;
  end;
  PClipboardEventData = ^TClipboardEventData;
  
  TGtkClipboardFormat = (
    gfCLASS, gfCOMPOUND_TEXT, gfDELETE, gfFILE_NAME, gfHOST_NAME, gfLENGTH,
    gfMULTIPLE, gfNAME, gfOWNER_OS, gfPROCESS, gfSTRING, gfTARGETS, gfTEXT,
    gfTIMESTAMP, gfUSER, gfUTF8_STRING);
    
  TGtkClipboardFormats = set of TGtkClipboardFormat;

const
  GtkClipboardFormatName: array[TGtkClipboardFormat] of string = (
      'CLASS', 'COMPOUND_TEXT', 'DELETE', 'FILE_NAME', 'HOST_NAME', 'LENGTH',
      'MULTIPLE', 'NAME', 'OWNER_OS', 'PROCESS', 'STRING', 'TARGETS', 'TEXT',
      'TIMESTAMP', 'USER', 'UTF8_STRING'
    );
  
const
  GdkTrue = {$IFDEF Gtk2}true{$ELSE}1{$ENDIF};
  GdkFalse = {$IFDEF Gtk2}false{$ELSE}0{$ENDIF};


  GTK_STYLE_BASE = 20;// see GTK_STATE_NORMAL..GTK_STATE_INSENSITIVE,
  GTK_STYLE_TEXT = 21;// see tGtkStateEnum, and see TGtkWidgetSet.SetWidgetColor


type
  TGdkPixBufBuffer = {$IFDEF Gtk2}Pguchar{$ELSE}PChar{$ENDIF};
  
 
{$IFDEF GTK2}
const
  GDK_VOIDSYMBOL = $FFFFFF;
{$ENDIF}
 
// MWE: All the IFDEFs for GTK2 annoyed me so I defined all (most) constants here
{$IFNDEF GTK2}
  {$I gtkkeysyms.inc}
{$ENDIF}

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

function NewDeviceContext: TDeviceContext;
procedure DisposeDeviceContext(DeviceContext: TDeviceContext);

type
  TCreateGCForDC = procedure(DC: TDeviceContext) of object;
var
  CreateGCForDC: TCreateGCForDC = nil;

{$IFDEF DebugLCLComponents}
var
  DebugGtkWidgets: TDebugLCLItems = nil;
  DebugGdiObjects: TDebugLCLItems = nil;
  DebugDeviceContexts: TDebugLCLItems = nil;
{$ENDIF}

procedure GtkDefDone;

implementation


{$IFOpt R+}{$Define RangeChecksOn}{$Endif}

// memory system for PGDIObject(s) ---------------------------------------------
type
  TGDIObjectMemManager = class(TLCLMemManager)
  protected
    procedure FreeFirstItem; override;
  public
    procedure DisposeGDIObject(AGDIObject: PGDIObject);
    function NewGDIObject: PGDIObject;
  end;
  
const
  GDIObjectMemManager: TGDIObjectMemManager = nil;

function InternalNewPGDIObject: PGDIObject;
begin
  if GDIObjectMemManager=nil then begin
    GDIObjectMemManager:=TGDIObjectMemManager.Create;
    GDIObjectMemManager.MinimumFreeCount:=1000;
  end;
  Result:=GDIObjectMemManager.NewGDIObject;
  {$IFDEF DebugLCLComponents}
  DebugGdiObjects.MarkCreated(Result,'NewPGDIObject');
  {$ENDIF}
end;

procedure InternalDisposePGDIObject(GDIObject: PGdiObject);
begin
  {$IFDEF DebugLCLComponents}
  DebugGdiObjects.MarkDestroyed(GDIObject);
  {$ENDIF}
  GDIObjectMemManager.DisposeGDIObject(GDIObject);
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

procedure TGDIObjectMemManager.DisposeGDIObject(AGDIObject: PGDIObject);
begin
  //DebugLn('TGDIObjectMemManager.DisposeGDIObject ',DbgS(AGDIObject));
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
    //DebugLn('TGDIObjectMemManager.DisposeGDIObject B FFreedCount=',FFreedCount);
    {$R-}
    inc(FFreedCount);
    {$IfDef RangeChecksOn}{$R+}{$Endif}
  end;
  dec(FCount);
end;

function TGDIObjectMemManager.NewGDIObject: PGDIObject;
begin
  if FFirstFree<>nil then begin
    // take from free list
    Result:=PGDIObject(FFirstFree);
    PGDIObject(FFirstFree):=Result^.Next;
    dec(FFreeCount);
  end else begin
    // free list empty -> create new node
    New(Result);
    // DebugLn('TGDIObjectMemManager.NewGDIObject FAllocatedCount=',FAllocatedCount);
    {$R-}
    inc(FAllocatedCount);
    {$IfDef RangeChecksOn}{$R+}{$Endif}
  end;
  FillChar(Result^, SizeOf(TGDIObject), 0);
  inc(FCount);
  //DebugLn('TGDIObjectMemManager.NewGDIObject ',DbgS(Result));
end;


// memory system for TDeviceContext(s) ---------------------------------------------
type
  TDeviceContextMemManager = class(TLCLMemManager)
  protected
    procedure FreeFirstItem; override;
  public
    procedure DisposeDeviceContext(ADeviceContext: TDeviceContext);
    function NewDeviceContext: TDeviceContext;
  end;

const
  DeviceContextMemManager: TDeviceContextMemManager = nil;

function NewDeviceContext: TDeviceContext;
begin
  if DeviceContextMemManager=nil then begin
    DeviceContextMemManager:=TDeviceContextMemManager.Create;
    DeviceContextMemManager.MinimumFreeCount:=1000;
  end;
  Result:=DeviceContextMemManager.NewDeviceContext;
  {$IFDEF DebugLCLComponents}
  DebugDeviceContexts.MarkCreated(Result,'NewDeviceContext');
  {$ENDIF}
end;

procedure DisposeDeviceContext(DeviceContext: TDeviceContext);
begin
  {$IFDEF DebugLCLComponents}
  DebugDeviceContexts.MarkDestroyed(DeviceContext);
  {$ENDIF}
  DeviceContextMemManager.DisposeDeviceContext(DeviceContext);
end;

{ TDeviceContextMemManager }

procedure TDeviceContextMemManager.FreeFirstItem;
var ADeviceContext: TDeviceContext;
begin
  ADeviceContext:=TDeviceContext(FFirstFree);
  TDeviceContext(FFirstFree):=ADeviceContext.SavedContext;
  //DebugLn('TDeviceContextMemManager.FreeFirstItem FFreedCount=',FFreedCount);
  ADeviceContext.Free;
  {$R-}
  inc(FFreedCount);
  {$IfDef RangeChecksOn}{$R+}{$Endif}
end;

procedure TDeviceContextMemManager.DisposeDeviceContext(
  ADeviceContext: TDeviceContext);
begin
  if (FFreeCount<FMinFree) or (FFreeCount<((FCount shr 3)*FMaxFreeRatio)) then
  begin
    // add ADeviceContext to Free list
    ADeviceContext.SavedContext:=TDeviceContext(FFirstFree);
    TDeviceContext(FFirstFree):=ADeviceContext;
    inc(FFreeCount);
  end else begin
    // free list full -> free the ANode
    //DebugLn('TDeviceContextMemManager.DisposeDeviceContext FFreedCount=',FFreedCount);
    ADeviceContext.Free;
    {$R-}
    inc(FFreedCount);
    {$IfDef RangeChecksOn}{$R+}{$Endif}
  end;
  dec(FCount);
end;

function TDeviceContextMemManager.NewDeviceContext: TDeviceContext;
begin
  if FFirstFree<>nil then begin
    // take from free list
    Result:=TDeviceContext(FFirstFree);
    TDeviceContext(FFirstFree):=Result.SavedContext;
    dec(FFreeCount);
  end else begin
    // free list empty -> create new node
    Result:=TDeviceContext.Create;
    //DebugLn('TDeviceContextMemManager.NewDeviceContext FAllocatedCount=',FAllocatedCount);
    {$R-}
    inc(FAllocatedCount);
    {$IfDef RangeChecksOn}{$R+}{$Endif}
  end;
  Result.Clear;
  inc(FCount);
end;


//------------------------------------------------------------------------------

{ TDeviceContext }

procedure TDeviceContext.Clear;
begin
  DCWidget:=nil;
  Drawable:=nil;
  GC:=nil;
  FillChar(GCValues, SizeOf(GCValues), #0);

  Origin.X:=0;
  Origin.Y:=0;
  SpecialOrigin:=false;
  PenPos.X:=0;
  PenPos.Y:=0;
  
  CurrentBitmap:=nil;
  CurrentFont:=nil;
  CurrentPen:=nil;
  CurrentBrush:=nil;
  CurrentPalette:=nil;
  FillChar(CurrentTextColor,SizeOf(CurrentTextColor),0);
  FillChar(CurrentBackColor,SizeOf(CurrentBackColor),0);
  ClipRegion:=0;
  
  SelectedColors:=dcscCustom;
  SavedContext:=nil;
  DCFlags:=[];
end;

function TDeviceContext.GetGC: pgdkGC;
begin
  if GC=nil then
    CreateGCForDC(Self);
  Result:=GC;
end;

procedure GtkDefInit;
begin
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets:=TDebugLCLItems.Create;
  DebugGdiObjects:=TDebugLCLItems.Create;
  DebugDeviceContexts:=TDebugLCLItems.Create;
  {$ENDIF}
end;

procedure GtkDefDone;
begin
  GDIObjectMemManager.Free;
  GDIObjectMemManager:=nil;
  DeviceContextMemManager.Free;
  DeviceContextMemManager:=nil;
  {$IFDEF DebugLCLComponents}
  FreeAndNil(DebugGtkWidgets);
  FreeAndNil(DebugGdiObjects);
  FreeAndNil(DebugDeviceContexts);
  {$ENDIF}
end;

initialization
  GtkDefInit;

finalization

end.
