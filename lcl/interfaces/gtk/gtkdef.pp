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
 *  See the file COPYING.LCL, included in this distribution,                 *
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
  Classes, LCLIntf, LCLProc, LCLType, LCLMemManager, DynHashArray, GraphType;

type
  TGDIType = (gdiBitmap, gdiBrush, gdiFont, gdiPen, gdiRegion, gdiPalette);
  TGDIBitmapType = (gbBitmap, gbPixmap{obsolete:, gbImage});

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
      {$Ifdef GTK2}
        GDIFontObject: PPangoFontDescription;
        StrikeOut : gboolean;//Description can't set these so we use these
        Underline : gboolean;//instead of an additional AttributeList
      {$else}
        GDIFontObject: PGdkFont;
        LogFont: TLogFont;// for now font info is stored as well, for later query font params
      {$EndIf}
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
    
  TDeviceContext = class
  public
    // device handles
    Wnd: HWND; // the owner PGtkWidget
    GC: pgdkGC;
    Drawable: PGDKDrawable;
    OriginalDrawable: PGDKDrawable; // only set if dcfDoubleBuffer in DCFlags
    
    // origins
    Origin: TPoint;
    SpecialOrigin: boolean;
    PenPos: TPoint;
    
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
  end;
  
  
  TWidgetInfoFlag = (
    wwiNotOnParentsClientArea
    );
  TWidgetInfoFlags = set of TWidgetInfoFlag;

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

// fpc 1.0.10 doesn't link exported variables correctly
// we fix this for gtk1 and define a constant
{$IFDEF VER1_0}
{$IFDEF gtk1}
   GTK_MAJOR_VERSION = 1;
   GTK_MINOR_VERSION = 2;
{$ENDIF}
{$ENDIF}  

function NewPGDIObject: PGDIObject;
procedure DisposePGDIObject(GDIObject: PGdiObject);

function NewDeviceContext: TDeviceContext;
procedure DisposeDeviceContext(DeviceContext: TDeviceContext);


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

function NewPGDIObject: PGDIObject;
begin
  if GDIObjectMemManager=nil then begin
    GDIObjectMemManager:=TGDIObjectMemManager.Create;
    GDIObjectMemManager.MinimumFreeCount:=1000;
  end;
  Result:=GDIObjectMemManager.NewGDIObject;
end;

procedure DisposePGDIObject(GDIObject: PGdiObject);
begin
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
  if AGDIObject^.RefCount>0 then
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
end;

procedure DisposeDeviceContext(DeviceContext: TDeviceContext);
begin
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
  Wnd:=0;
  GC:=nil;
  Drawable:=nil;
  
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

finalization
  GDIObjectMemManager.Free;
  GDIObjectMemManager:=nil;
  DeviceContextMemManager.Free;
  DeviceContextMemManager:=nil;

end.

{ =============================================================================

  $Log$
  Revision 1.66  2005/03/07 21:59:45  vincents
  changed hexstr(cardinal()) for pointers to dbgs() and other 64-bits fixes   from Peter Vreman

  Revision 1.65  2005/03/04 12:21:55  mattias
  fixed TShape FPCanvas issue

  Revision 1.64  2005/02/18 09:44:30  vincents
  added constants for gtk1 version, fixes fpc 1.0.x compilation

  Revision 1.63  2004/08/18 09:31:21  mattias
  removed obsolete unit vclglobals

  Revision 1.62  2004/08/10 17:34:13  mattias
  implemented font cache for gtk, which accelerates switching fonts

  Revision 1.61  2004/05/16 23:24:41  marc
  + Added WSBitBtn interface
  + Implemented WSBitBtn interface for gtk

  Revision 1.60  2004/05/11 12:16:47  mattias
  replaced writeln by debugln

  Revision 1.59  2004/04/19 09:30:04  marc
  * Fixed compilation for gtk2

  Revision 1.58  2004/04/18 23:55:39  marc
  * Applied patch from Ladislav Michl
  * Changed the way TControl.Text is resolved
  * Added setting of text to TWSWinControl

  Revision 1.57  2004/03/24 01:21:41  marc
  * Simplified signals for gtkwsbutton

  Revision 1.56  2004/03/22 19:10:04  mattias
  implemented icons for TPage in gtk, mask for TCustomImageList

  Revision 1.55  2004/03/06 15:37:43  mattias
  fixed FreeDC

  Revision 1.54  2004/02/27 00:42:41  marc
  * Interface CreateComponent splitup
  * Implemented CreateButtonHandle on GTK interface
    on win32 interface it still needs to be done
  * Changed ApiWizz to support multilines and more interfaces

  Revision 1.53  2004/02/04 22:17:09  mattias
  removed workaround VirtualCreate

  Revision 1.52  2004/01/12 23:56:10  mattias
  improved double buffering, only one issue left: parent gdkwindow paint messages

  Revision 1.51  2004/01/10 22:34:20  mattias
  started double buffering for gtk intf

  Revision 1.50  2003/10/17 03:21:21  ajgenius
  fix GTK2 compiling for new Keyboard changes

  Revision 1.49  2003/10/16 23:54:27  marc
  Implemented new gtk keyevent handling

  Revision 1.48  2003/09/19 00:41:51  ajgenius
  remove USE_PANGO define since pango now apears to work properly.

  Revision 1.47  2003/09/18 09:21:03  mattias
  renamed LCLLinux to LCLIntf

  Revision 1.46  2003/09/15 03:10:46  ajgenius
  PANGO support for GTK2 now works.. sorta. TextOut/ExtTextOut broken?

  Revision 1.45  2003/09/09 20:46:38  ajgenius
  more implementation toward pango for gtk2

  Revision 1.44  2003/09/09 17:16:24  ajgenius
  start implementing pango routines for GTK2

  Revision 1.43  2003/09/06 20:23:53  ajgenius
  fixes for gtk2
  added more wrappers for gtk1/gtk2 converstion and sanity
  removed pointless version $Ifdef GTK2 etc
  IDE now "runs" Tcontrol drawing/using problems
  renders it unuseable however

  Revision 1.42  2003/08/29 21:21:07  mattias
  fixes for gtk2

  Revision 1.41  2003/07/02 10:02:51  mattias
  fixed TPaintStruct

  Revision 1.40  2002/08/19 15:15:24  mattias
  implemented TPairSplitter

  Revision 1.39  2002/08/17 23:41:34  mattias
  many clipping fixes

  Revision 1.38  2003/06/13 10:09:04  mattias
  fixed Set/GetPixel

  Revision 1.37  2003/05/27 15:04:00  mattias
  small fixes for debugger without file

  Revision 1.36  2003/05/19 08:16:33  mattias
  fixed allocation of dc backcolor

  Revision 1.35  2003/03/26 19:25:27  mattias
  added transient deactivation option and updated localization

  Revision 1.34  2003/03/15 18:32:38  mattias
  implemented transient windows for all cases

  Revision 1.33  2003/01/27 13:49:16  mattias
  reduced speedbutton invalidates, added TCanvas.Frame

  Revision 1.32  2003/01/24 11:58:00  mattias
  fixed clipboard waiting and kwrite targets

  Revision 1.31  2002/12/15 11:52:28  mattias
  started gtk2 interface

  Revision 1.30  2002/12/05 22:16:30  mattias
  double byte char font started

  Revision 1.29  2002/10/30 17:43:35  lazarus
  AJ: added IsNullBrush checks to reduce pointless color allocations & GDK function calls

  Revision 1.28  2002/10/20 21:54:03  lazarus
  MG: fixes for 1.1

  Revision 1.27  2002/10/20 19:03:56  lazarus
  AJ: minor fixes for FPC 1.1

  Revision 1.26  2002/10/17 15:09:32  lazarus
  MG: made mouse capturing more strict

  Revision 1.25  2002/10/10 19:43:16  lazarus
  MG: accelerated GetTextMetrics

  Revision 1.24  2002/10/08 14:10:02  lazarus
  MG: added TDeviceContext.SelectedColors

  Revision 1.23  2002/10/08 13:42:23  lazarus
  MG: added TDevContextColorType

  Revision 1.22  2002/10/08 10:08:46  lazarus
  MG: accelerated GDIColor allocating

  Revision 1.21  2002/10/07 20:50:58  lazarus
  MG: accelerated SelectGDKPenProps

  Revision 1.20  2002/10/01 10:05:48  lazarus
  MG: changed PDeviceContext into class TDeviceContext

  Revision 1.19  2002/09/18 17:07:28  lazarus
  MG: added patch from Andrew

  Revision 1.18  2002/09/12 05:56:15  lazarus
  MG: gradient fill, minor issues from Andrew

  Revision 1.17  2002/09/10 06:49:20  lazarus
  MG: scrollingwincontrol from Andrew

  Revision 1.16  2002/09/06 16:38:25  lazarus
  MG: added GetDCOffset

  Revision 1.15  2002/08/30 12:32:22  lazarus
  MG: MoveWindowOrgEx, Splitted FWinControls/FControls, TControl drawing, Better DesignerDrawing, ...

  Revision 1.14  2002/08/21 14:44:18  lazarus
  MG: accelerated synedit

  Revision 1.13  2002/08/21 14:06:40  lazarus
  MG: added TDeviceContextMemManager

  Revision 1.12  2002/08/21 10:46:37  lazarus
  MG: fixed unreleased gdiRegions

  Revision 1.11  2002/08/21 08:13:37  lazarus
  MG: accelerated new/dispose of GdiObjects

  Revision 1.10  2002/08/15 15:46:49  lazarus
  MG: added changes from Andrew (Clipping)

  Revision 1.9  2002/06/09 14:00:41  lazarus
  MG: fixed persistent caret and implemented Form.BorderStyle=bsNone

  Revision 1.8  2002/06/04 15:17:23  lazarus
  MG: improved TFont for XLFD font names

  Revision 1.7  2002/06/01 08:41:28  lazarus
  MG: DrawFramControl now uses gtk style, transparent STrechBlt

  Revision 1.6  2002/05/10 06:05:56  lazarus
  MG: changed license to LGPL

  Revision 1.5  2002/02/03 00:24:01  lazarus
  TPanel implemented.
  Basic graphic primitives split into GraphType package, so that we can
  reference it from interface (GTK, Win32) units.
  New Frame3d canvas method that uses native (themed) drawing (GTK only).
  New overloaded Canvas.TextRect method.
  LCLLinux and Graphics was split, so a bunch of files had to be modified.

  Revision 1.4  2001/11/12 16:56:08  lazarus
  MG: CLIPBOARD

  Revision 1.3  2001/03/27 21:12:54  lazarus
  MWE:
    + Turned on longstrings
    + modified memotest to add lines

  Revision 1.2  2001/01/25 21:38:57  lazarus
  MWE:
    * fixed lil bug I commetted yesterday (listbox crash)

  Revision 1.1  2001/01/24 23:26:40  lazarus
  MWE:
    = moved some types to gtkdef
    + added WinWidgetInfo
    + added some initialization to Application.Create

}
