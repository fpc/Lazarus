{ 
 /*************************************************************************** 
                         GTKINT.pp  -  GTKInterface Object
                             ------------------- 
 
                   Initial Revision  : Thu July 1st CST 1999 
 
 
 ***************************************************************************/ 
 
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
 
unit GtkInt;
 
{$mode objfpc} 
{$LONGSTRINGS ON}

interface

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}
 
{ $DEFINE VerboseMouseBugfix}

{$IFDEF win32}
{$DEFINE NoGdkPixbufLib}
{$ELSE}
{off $DEFINE NoGdkPixbufLib}
{$ENDIF}
{off $Define Critical_Sections_Support}

uses
  InterfaceBase, {$Ifndef NoGdkPixbufLib}gdkpixbuf,{$EndIf} gtk, gdk,
  glib, SysUtils, LMessages, Classes, Controls, Forms, VclGlobals, 
  LCLLinux, LCLType, gtkDef, DynHashArray, LazQueue, GraphType, 
  GraphicsMath;

type
  TgtkObject = class(TInterfaceBase)
  private
    FKeyStateList: TList; // Keeps track of which keys are pressed
    FDeviceContexts: TDynHashArray;// hasharray of HDC
    FGDIObjects: TDynHashArray;    // hasharray of PGdiObject
    FMessageQueue: TLazQueue;      // queue of PMsg
    FPaintMessages: TDynHashArray; // hasharray of PLazQueueItem
    FRCFilename: string;
    FRCFileParsed: boolean;
    FWidgetsWithResizeRequest: TDynHashArray; // hasharray of PGtkWidget
    FGTKToolTips: PGtkToolTips;
    FAccelGroup: PgtkAccelGroup;
    FTimerData : TList;       // keeps track of timer event structures
    FDefaultFont : PGdkFont;
    FNoteBookCloseBtnPixmapImg: PGdkPixmap;
    FNoteBookCloseBtnPixmapMask: PGdkPixmap;

    FStockNullBrush: HBRUSH;
    FStockBlackBrush: HBRUSH;
    FStockLtGrayBrush: HBRUSH;
    FStockGrayBrush: HBRUSH;
    FStockDkGrayBrush: HBRUSH;
    FStockWhiteBrush: HBRUSH;
    
    FStockNullPen: HPEN;
    FStockBlackPen: HPEN;
    FStockWhitePen: HPEN;
    
    FStockSystemFont : HFONT;

    Function CreateSystemFont : hFont;

    Procedure HookSignals(Sender : TObject);  //hooks all signals for controls
    procedure CreateComponent(Sender : TObject);
    procedure DestroyLCLControl(Sender : TObject);
    procedure AddChild(Parent,Child : Pointer; Left,Top: Integer);
    procedure ResizeChild(Sender : TObject; Left,Top,Width,Height : Integer);
    procedure AssignSelf(Child ,Data : Pointer);
    procedure ReDraw(Child : Pointer);
    procedure SetClipboardWidget(TargetWidget: PGtkWidget);
    
    function IsValidDC(const DC: HDC): Boolean;
    function IsValidGDIObject(const GDIObject: HGDIOBJ): Boolean;
    function IsValidGDIObjectType(const GDIObject: HGDIOBJ;
                                  const GDIType: TGDIType): Boolean;
    function NewGDIObject(const GDIType: TGDIType): PGdiObject;
    procedure DisposeGDIObject(GdiObject: PGdiObject);
    function NewDC: PDeviceContext;
    procedure DisposeDC(pDC: PDeviceContext);
    function CreateDefaultBrush: PGdiObject;
    function CreateDefaultFont: PGdiObject;
    function CreateDefaultPen: PGdiObject;
    procedure LoadXPMFromLazResource(const ResourceName: string;
      Window: PGdkWindow; var PixmapImg, PixmapMask: PGdkPixmap);
    procedure LoadFromXPMFile(Bitmap: TObject; Filename: PChar);
    procedure LoadFromPixbufFile(Bitmap: TObject; Filename: PChar);

    procedure SetRCFilename(const AValue: string);
    procedure ParseRCFile;

    procedure ShowHide(Sender : TObject);
    procedure GetNoteBookCloseBtnPixmap(Window: PGdkWindow;
                                        var Img, Mask: PGdkPixmap);
    procedure AddDummyNoteBookPage(NoteBookWidget: PGtkNoteBook);
    procedure RemoveDummyNoteBookPage(NoteBookWidget: PGtkNotebook);
    procedure UpdateNotebookPageTab(ANoteBook, APage: TObject);
    procedure AddNBPage(ANoteBook, APage: TObject; Index: Integer);
    procedure RemoveNBPage(ANoteBook: TObject; Index: Integer);
    procedure MoveNBPage(ANoteBook, APage: TObject; NewIndex: Integer);

    procedure SetText(Child,Data : Pointer);
    procedure SetColor(Sender : TObject);
    Procedure SetPixel(Sender : TObject; Data : Pointer);
    Procedure GetPixel(Sender : TObject; Data : Pointer);
    function  GetValue(Sender : TObject; Data : pointer) : integer;
    function  SetValue(Sender : TObject; Data : pointer) : integer;
    function  SetProperties (Sender: TObject) : integer;
    procedure AttachMenu(Sender: TObject);
    procedure SetColorDialogColor(ColorSelection: PGtkColorSelection;
      Color: TColor);
    
    function HashPaintMessage(p: pointer): integer;
    function FindPaintMessage(HandleWnd: HWnd): PLazQueueItem;
    
    procedure SetResizeRequest(Widget: PGtkWidget);
    procedure UnsetResizeRequest(Widget: PGtkWidget);

    procedure SetWindowSizeAndPosition(Window: PGtkWindow;
      AWinControl: TWinControl);
    procedure SendCachedLCLMessages;
    function  LCLtoGtkMessagePending: boolean;
    procedure SendCachedGtkMessages;
    procedure SetCallback(Msg : LongInt; Sender : TObject); override;
    procedure RemoveCallbacks(Sender : TObject); override;
  public
    constructor Create; 
    destructor Destroy; override;
    function  GetText(Sender: TControl; var Text: String): Boolean; override;
    procedure SetLabel(Sender : TObject; Data : Pointer);
    function  IntSendMessage3(LM_Message : Integer; Sender : TObject;
                              data : pointer) : integer; override;
    procedure HandleEvents; override;
    procedure WaitMessage; override;
    procedure AppTerminate; override;
    procedure Init; override;
    function  UpdateHint(Sender: TObject): Integer; override;
    function  RecreateWnd(Sender: TObject): Integer; override;

    {$I gtkwinapih.inc}
    
    property RCFilename: string read FRCFilename write SetRCFilename;
  end;

{$I gtklistslh.inc}

implementation

uses 
  Graphics, Buttons, Menus, GTKWinApiWindow, StdCtrls, ComCtrls, CListBox,
  KeyMap, Calendar, Arrow, Spin, CommCtrl, ExtCtrls, Dialogs, FileCtrl,
  LResources;

{$I gtklistsl.inc}


{const
  TARGET_ENTRYS = 3;
}

var
//  target_table : array[0..TARGET_ENTRYS - 1] of TgtkTargetEntry;

  //drag icons
  //TrashCan_Open : PgdkPixmap;
  //TrashCan_Open_Mask : PGdkPixmap;
  //TrashCan_Closed : PGdkPixmap;
  //TrashCan_Closed_Mask : PGdkPixmap;
  Drag_Icon : PgdkPixmap;
  Drag_Mask : PgdkPixmap;

  //Dragging : Boolean;

  MCaptureHandle: HWND;

const
  DblClickTime = 250;// 250 miliseconds or less between clicks is a double click
  DblClickThreshold = 3;// max Movement between two clicks of a DblClick

type
  TLastMouseClick = record
    Down: boolean;
    TheTime: TDateTime; // last Down time
    ClickCount: integer;
    Component: TComponent;
    Window: PGdkWindow;
    WindowPoint: TPoint;
  end;

const
  EmptyLastMouseClick: TLastMouseClick =
    (Down: false; TheTime: -1; ClickCount: 0; Component: nil;
     Window: nil; WindowPoint: (X: 0; Y: 0));

var
  LastLeft, LastMiddle, LastRight: TLastMouseClick;
  LastFileSelectRow : gint;


  // mouse cursors
  Cursor_Watch    : pGDKCursor;
  Cursor_Arrow    : pGDKCursor;
  Cursor_Cross    : pGDKCursor;
  Cursor_Hand1    : pGDKCursor;
  Cursor_XTerm    : pGDKCursor;
  Cursor_StdArrow : pGDKCursor;
  Cursor_HSplit   : pGDKCursor;
  Cursor_VSplit   : pGDKCursor;
  Cursor_SizeNWSE : pGDKCursor;
  Cursor_SizeNS   : pGDKCursor;
  Cursor_SizeNESW : pGDKCursor;
  Cursor_SizeWE   : pGDKCursor;
  Cursor_Help     : pGDKCursor;

  Styles : TStrings;

const
  KEYMAP_VKUNKNOWN = $10000;
  KEYMAP_TOGGLE    = $20000;
  KEYMAP_EXTENDED  = $40000;

// PDB: note this is a hack. Windows maintains a system wide
//      system color table we will have to have our own
//      to be able to do the translations required from
//      window manager to window manager this means every
//      application will carry its own color table
//      we set the defaults here to reduce the initial
//      processing of creating a default table
// MWE: Naaaaah, not a hack, just something temporary
const
  SysColorMap: array [0..MAX_SYS_COLORS] of DWORD = (
    $C0C0C0,     {COLOR_SCROLLBAR}
    $808000,     {COLOR_BACKGROUND}
    $800000,     {COLOR_ACTIVECAPTION}
    $808080,     {COLOR_INACTIVECAPTION}
    $C0C0C0,     {COLOR_MENU}
    $FFFFFF,     {COLOR_WINDOW}
    $000000,     {COLOR_WINDOWFRAME}
    $000000,     {COLOR_MENUTEXT}
    $000000,     {COLOR_WINDOWTEXT}
    $FFFFFF,     {COLOR_CAPTIONTEXT}
    $C0C0C0,     {COLOR_ACTIVEBORDER}
    $C0C0C0,     {COLOR_INACTIVEBORDER}
    $808080,     {COLOR_APPWORKSPACE}
    $800000,     {COLOR_HIGHLIGHT}
    $FFFFFF,     {COLOR_HIGHLIGHTTEXT}
    $D0D0D0,     {COLOR_BTNFACE}
    $808080,     {COLOR_BTNSHADOW}
    $808080,     {COLOR_GRAYTEXT}
    $000000,     {COLOR_BTNTEXT}
    $C0C0C0,     {COLOR_INACTIVECAPTIONTEXT}
    $F0F0F0,     {COLOR_BTNHIGHLIGHT}
    $000000,     {COLOR_3DDKSHADOW}
    $C0C0C0,     {COLOR_3DLIGHT}
    $000000,     {COLOR_INFOTEXT}
    $E1FFFF,     {COLOR_INFOBK}
    $000000,     {unasigned}
    $000000,     {COLOR_HOTLIGHT}
    $000000,     {COLOR_GRADIENTACTIVECAPTION}
    $000000      {COLOR_GRADIENTINACTIVECAPTION}
  ); {end _SysColors}

type

  //Defined in gtksignal.c
  PGtkHandler = ^TGtkHandler;
  TGtkHandler = record
    id:               guint;           
    next:             PGtkHandler;      
    prev:             PGtkHandler;     
    flags:            guint; // -->  blocked : 20 bits,
                             //      object_signal : 1 bit,
                             //      after : 1 bit,
                             //      no_marshal : 1 bit
    ref_count:        guint16;         
    signal_id:        guint16;         
    func:             TGtkSignalFunc;   
    func_data:        gpointer;        
    destroy_func:     TGtkSignalDestroy;
  end;

const
  bmSignalAfter = $00200000;
  
type
  { lazarus GtkInterface definition for additional timer data, not in gtk }
  PGtkITimerInfo = ^TGtkITimerinfo;
  TGtkITimerInfo = record
    Handle   : hWND;
    IDEvent  : Cardinal;
    TimerFunc: TFNTimerProc;
  end;
  
  
var
  gtk_handler_quark: TGQuark;
  FOldTimerData: TList; // list of PGtkITimerinfo


// Internal Paint message:
const
  LM_GTKPaint = LM_INTERFACEFIRST;
  
type
  TLMGtkPaint = packed record
    Msg: Cardinal;
    Widget: PGtkWidget;
    Unused1: integer;
    Unused2: integer;
  end;


const
  TARGET_STRING = 1;
  TARGET_ROOTWIN = 2;

{off $DEFINE DEBUG_CLIPBOARD}
var
  // All clipboard events are handled by only one widget - the ClipboardWidget 
  // This widget is typically the main form
  ClipboardWidget: PGtkWidget;
  // each selection has an gtk identifier (an atom)
  ClipboardTypeAtoms: array[TClipboardType] of cardinal;
  // each active request will procduce an TClipboardEventData stored in this list
  ClipboardSelectionData: TList; // list of PClipboardEventData
  // each selection can have an user defined handler (normally set by the lcl)
  ClipboardHandler: array[TClipboardType] of TClipboardRequestEvent;
  // boolean array, telling what gtk format is automatically supported by
  // gtk interface and not by the program (the lcl)
  ClipboardExtraGtkFormats: array[TClipboardType,TGtkClipboardFormat] of boolean;
  // lists of supported targets
  ClipboardTargetEntries: array[TClipboardType] of PGtkTargetEntry;
  ClipboardTargetEntryCnt: array[TClipboardType] of integer;

  // each main widget that was resized by the gtk is stored here
  // (hasharray of PGtkWidget)
  FWidgetsResized: TDynHashArray;
  // each fixed widget that was resized by the gtk is stored here
  // (hasharray of PGtkWidget)
  FFixWidgetsResized: TDynHashArray;

const
  aGtkJustification: array[TAlignment] of TGTKJustification =
    (GTK_JUSTIFY_LEFT,GTK_JUSTIFY_RIGHT,GTK_JUSTIFY_CENTER);
    
  aGtkSelectionMode: Array[Boolean] of TGtkSelectionMode =
    (GTK_SELECTION_SINGLE,GTk_SELECTION_EXTENDED);

  { file dialog }
  
type
  PFileSelHistoryEntry = ^TFileSelHistoryEntry;
  TFileSelHistoryEntry = record
      Filename: PChar;
      MenuItem: PGtkWidget;
    end;

  PFileSelFilterEntry = ^TFileSelFilterEntry;
  TFileSelFilterEntry = record
      Description: PChar;
      Mask: PChar;
      FilterIndex: integer;
      MenuItem: PGtkWidget;
    end;
    
  { Menu }
  
type
  TCheckMenuItemDrawProc =
    procedure (check_menu_item:PGtkCheckMenuItem; area:PGdkRectangle); cdecl;
    
  TMenuSizeRequestProc =
    procedure (widget:PGtkWidget; requisition:PGtkRequisition); cdecl;

const
  OldCheckMenuItemDrawProc: TCheckMenuItemDrawProc = nil;
  OldMenuSizeRequestProc: TMenuSizeRequestProc = nil;
  OldCheckMenuItemToggleSize: integer = 0;


// some callbacks
function gtkRealizeCB(Widget: PGtkWidget; Data: Pointer): GBoolean; cdecl; forward;
function gtkRealizeAfterCB(Widget: PGtkWidget; Data: Pointer): GBoolean; cdecl; forward;
function gtkMouseBtnPress(widget: PGtkWidget; event : pgdkEventButton;
  data: gPointer) : GBoolean; forward; cdecl;
function gtkMotionNotify(Widget:PGTKWidget; event: PGDKEventMotion;
  data: gPointer): GBoolean; forward; cdecl;
function gtkMouseBtnRelease(widget: PGtkWidget; event : pgdkEventButton;
  data: gPointer) : GBoolean; forward; cdecl;
function gtkDrawAfter(Widget: PGtkWidget; area: PGDKRectangle;
  data: gPointer) : GBoolean; forward; cdecl;
function gtkExposeEventAfter(Widget: PGtkWidget; Event : PGDKEventExpose;
  Data: gPointer): GBoolean; forward; cdecl;


{$I dragicons.inc}
{$I gtkproc.inc}
{$I gtkcallback.inc}
{$I gtkobject.inc}
{$I gtkwinapi.inc}

procedure InternalInit;
var c: TClipboardType;
begin
  gtk_handler_quark := g_quark_from_static_string('gtk-signal-handlers');
  
  MCaptureHandle := 0;
  LastLeft:=EmptyLastMouseClick;
  LastMiddle:=EmptyLastMouseClick;
  LastRight:=EmptyLastMouseClick;
  FOldTimerData:=TList.Create;
  
  // clipboard
  ClipboardSelectionData:=TList.Create;
  for c:=Low(TClipboardType) to High(TClipboardType) do begin
    ClipboardTypeAtoms[c]:=0;
    ClipboardHandler[c]:=nil;
    //ClipboardIgnoreLossCount[c]:=0;
    ClipboardTargetEntries[c]:=nil;
    ClipboardTargetEntryCnt[c]:=0;
  end;
  
  InitDesignSignalMasks;
end;

procedure InternalFinal;
var i: integer;
  t: PGtkITimerinfo;
  ced: PClipboardEventData;
  c: TClipboardType;
begin
  // timer
  for i:=0 to FOldTimerData.Count-1 do begin
    t:=PGtkITimerinfo(FOldTimerData[i]);
    dispose(t);
  end;
  FOldTimerData.Free;
  FOldTimerData:=nil;
  
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
end;


initialization
  //writeln('gtkint.pp - initialization');
  {$I gtkimages.lrs}
  InternalInit;

finalization
  InternalFinal;

end.

{ =============================================================================

  $Log$
  Revision 1.71  2002/09/06 22:32:21  lazarus
  Enabled cursor property + property editor.

  Revision 1.70  2002/09/05 12:11:44  lazarus
  MG: TNotebook is now streamable

  Revision 1.69  2002/09/03 08:07:20  lazarus
  MG: image support, TScrollBox, and many other things from Andrew

  Revision 1.68  2002/09/02 19:10:28  lazarus
  MG: TNoteBook now starts with no Page and TPage has no auto names

  Revision 1.67  2002/09/01 16:11:22  lazarus
  MG: double, triple and quad clicks now works

  Revision 1.66  2002/08/28 09:40:49  lazarus
  MG: reduced paint messages and DC getting/releasing

  Revision 1.65  2002/08/27 06:40:50  lazarus
  MG: ShortCut support for buttons from Andrew

  Revision 1.64  2002/08/22 16:43:35  lazarus
  MG: improved theme support from Andrew

  Revision 1.63  2002/08/21 14:06:40  lazarus
  MG: added TDeviceContextMemManager

  Revision 1.62  2002/08/21 08:13:37  lazarus
  MG: accelerated new/dispose of gdiobjects

  Revision 1.61  2002/08/19 20:34:48  lazarus
  MG: improved Clipping, TextOut, Polygon functions

  Revision 1.60  2002/08/19 18:00:02  lazarus
  MG: design signals for gtk internal widgets

  Revision 1.59  2002/08/17 15:45:33  lazarus
  MG: removed ClientRectBugfix defines

  Revision 1.58  2002/08/15 15:46:49  lazarus
  MG: added changes from Andrew (Clipping)

  Revision 1.57  2002/08/15 13:37:57  lazarus
  MG: started menuitem icon, checked, radio and groupindex

  Revision 1.56  2002/08/13 07:08:24  lazarus
  MG: added gdkpixbuf.pp and changes from Andrew Johnson

  Revision 1.55  2002/08/08 18:05:46  lazarus
  MG: added graphics extensions from Andrew Johnson

  Revision 1.54  2002/08/05 10:45:04  lazarus
  MG: TMenuItem.Caption can now be set after creation

  Revision 1.53  2002/07/20 13:47:03  lazarus
  MG: fixed eventmask for realized windows

  Revision 1.52  2002/06/26 15:11:09  lazarus
  MG: added new tool: Guess misplaced $IFDEF/$ENDIF

  Revision 1.51  2002/06/21 16:59:15  lazarus
  MG: TControl.Cursor is now set, reduced auto reaction of widgets in design mode

  Revision 1.50  2002/06/11 13:41:10  lazarus
  MG: fixed mouse coords and fixed mouse clicked thru bug

  Revision 1.49  2002/06/09 07:08:43  lazarus
  MG: fixed window jumping

  Revision 1.48  2002/06/08 17:16:04  lazarus
  MG: added close buttons and images to TNoteBook and close buttons to source editor

  Revision 1.47  2002/06/07 06:40:18  lazarus
  MG: gtk HandleEvents will now process all pending events

  Revision 1.46  2002/06/06 07:23:24  lazarus
  MG: small fixes to reduce form repositioing

  Revision 1.45  2002/05/30 14:11:12  lazarus
  MG: added filters and history to TOpenDialog

  Revision 1.44  2002/05/29 21:44:38  lazarus
  MG: improved TCommon/File/OpenDialog, fixed TListView scrolling and broder

  Revision 1.43  2002/05/28 19:39:45  lazarus
  MG: added gtk rc file support and started stule dependent syscolors

  Revision 1.42  2002/05/28 14:58:30  lazarus
  MG: added scrollbars for TListView

  Revision 1.41  2002/05/20 14:19:03  lazarus
  MG: activated the clientrect bugfixes

  Revision 1.40  2002/05/13 14:47:01  lazarus
  MG: fixed client rectangles, TRadioGroup, RecreateWnd

  Revision 1.39  2002/05/12 04:56:20  lazarus
  MG: client rect bugs nearly completed

  Revision 1.38  2002/05/10 06:05:56  lazarus
  MG: changed license to LGPL

  Revision 1.37  2002/05/09 12:41:29  lazarus
  MG: further clientrect bugfixes

  Revision 1.36  2002/05/06 08:50:36  lazarus
  MG: replaced logo, increased version to 0.8.3a and some clientrectbugfix

  Revision 1.35  2002/03/31 22:01:38  lazarus
  MG: fixed unreleased/unpressed Ctrl/Alt/Shift

  Revision 1.34  2002/03/29 19:11:38  lazarus
  Added Triple Click
  Shane

  Revision 1.33  2002/03/27 00:33:54  lazarus
  MWE:
    * Cleanup in lmessages
    * Added Listview selection and notification events
    + introduced commctrl

  Revision 1.32  2002/03/25 17:59:20  lazarus
  GTK Cleanup
  Shane

  Revision 1.31  2002/03/14 20:28:49  lazarus
  Bug fix for Mattias.
  Fixed spinedit so you can now get the value and set the value.
  Shane

  Revision 1.30  2002/02/25 18:46:06  lazarus
  MG: fixed nested record parsing

  Revision 1.29  2002/02/25 16:48:13  lazarus
  MG: new IDE window layout system

  Revision 1.28  2002/02/20 19:11:48  lazarus
  Minor tweaks, default font caching.

  Revision 1.27  2002/02/18 22:46:11  lazarus
  Implented TMenuItem.ShortCut (not much tested).

  Revision 1.26  2002/02/03 00:24:01  lazarus
  TPanel implemented.
  Basic graphic primitives split into GraphType package, so that we can
  reference it from interface (GTK, Win32) units.
  New Frame3d canvas method that uses native (themed) drawing (GTK only).
  New overloaded Canvas.TextRect method.
  LCLLinux and Graphics was split, so a bunch of files had to be modified.

  Revision 1.25  2001/12/06 13:39:36  lazarus
  Added TArrow component
  Shane

  Revision 1.24  2001/12/05 17:23:45  lazarus
  Added Calendar component
  Shane

  Revision 1.23  2001/11/13 18:50:10  lazarus
  Changes to facilitate the toggle between form and unit
  Shane

  Revision 1.22  2001/11/12 16:56:08  lazarus
  MG: CLIPBOARD

  Revision 1.21  2001/10/08 08:05:08  lazarus
  MG: fixed TColorDialog set color

  Revision 1.20  2001/10/07 07:28:33  lazarus
  MG: fixed setpixel and TCustomForm.OnResize event

  Revision 1.19  2001/09/30 08:34:51  lazarus
  MG: fixed mem leaks and fixed range check errors

  Revision 1.18  2001/07/01 23:33:13  lazarus
  MG: added WaitMessage and HandleEvents is now non blocking

  Revision 1.17  2001/06/26 21:44:32  lazarus
  MG: reduced paint messages

  Revision 1.15  2001/06/14 14:57:59  lazarus
  MG: small bugfixes and less notes

  Revision 1.14  2001/06/04 07:50:42  lazarus
  MG: close application object in gtkint.pp

  Revision 1.13  2001/04/06 22:25:14  lazarus
  * TTimer uses winapi-interface now instead of sendmessage-interface, stoppok

  Revision 1.12  2001/03/27 21:12:54  lazarus
  MWE:
    + Turned on longstrings
    + modified memotest to add lines

  Revision 1.11  2001/03/19 18:51:57  lazarus
  MG: added dynhasharray and renamed tsynautocompletion

  Revision 1.10  2001/03/19 14:44:22  lazarus
  MG: fixed many unreleased DC and GDIObj bugs

  Revision 1.7  2001/02/20 16:53:27  lazarus
  Changes for wordcompletion and many other things from Mattias.
  Shane

  Revision 1.6  2001/02/06 18:19:38  lazarus
  Shane

  Revision 1.5  2001/02/01 19:34:50  lazarus
  TScrollbar created and a lot of code added.

  It's cose to working.
  Shane

  Revision 1.4  2001/01/24 23:26:40  lazarus
  MWE:
    = moved some types to gtkdef
    + added WinWidgetInfo
    + added some initialization to Application.Create

  Revision 1.3  2001/01/23 23:33:55  lazarus
  MWE:
    - Removed old LM_InvalidateRect
    - did some cleanup in old  code
    + added some comments  on gtkobject data (gtkproc)

  Revision 1.2  2000/07/30 21:48:33  lazarus
  MWE:
    = Moved ObjectToGTKObject to GTKProc unit
    * Fixed array checking in LoadPixmap
    = Moved LM_SETENABLED to API func EnableWindow and EnableMenuItem
    ~ Some cleanup

  Revision 1.1  2000/07/13 10:28:29  michael
  + Initial import

}
