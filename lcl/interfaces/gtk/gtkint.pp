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

{ $DEFINE VerboseTimer}
{ $DEFINE VerboseMouseBugfix}
{ $DEFINE RaiseExceptionOnNilPointers}

{$IFDEF win32}
{$DEFINE NoGdkPixbufLib}
{$ELSE}
{off $DEFINE NoGdkPixbufLib}
{$ENDIF}
{off $Define Critical_Sections_Support}

{off $Define Disable_GC_SysColors}

uses
  InterfaceBase,
  {$IFDEF gtk2}
  glib2, gdk2pixbuf, gdk2, gtk2,
  {$ELSE}
  glib, gdk, gtk, {$Ifndef NoGdkPixbufLib}gdkpixbuf,{$EndIf}
  {$ENDIF}
  SysUtils, LMessages, Classes, Controls, Forms, LCLStrConsts, VclGlobals,
  LCLProc, LCLLinux, LCLType, gtkDef, DynHashArray, LazQueue,
  GraphType, GraphMath;


type
  TgtkObject = class(TInterfaceBase)
  protected
    FKeyStateList: TList; // Keeps track of which keys are pressed
    FDeviceContexts: TDynHashArray;// hasharray of HDC
    FGDIObjects: TDynHashArray;    // hasharray of PGdiObject
    FMessageQueue: TLazQueue;      // queue of PMsg
    FPaintMessages: TDynHashArray; // hasharray of PLazQueueItem
    WaitingForMessages: boolean;

    FRCFilename: string;
    FRCFileParsed: boolean;
    FRCFileAge: integer;
    FWidgetsWithResizeRequest: TDynHashArray; // hasharray of PGtkWidget
    FGTKToolTips: PGtkToolTips;
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
    
    FDefaultFont : PGdkFont;
    FStockSystemFont : HFONT;

    Function CreateSystemFont : hFont;

    procedure InitStockItems; virtual;
    procedure FreeStockItems; virtual;
    procedure PassCmdLineOptions; override;
    
    // styles
    procedure FreeAllStyles; virtual;

    procedure ReDraw(Child : Pointer);virtual;
    procedure ShowHide(Sender : TObject);virtual;

    Function GetCompStyle(Sender : TObject) : Longint; Virtual;
    Procedure HookSignals(Sender : TObject); virtual;  //hooks all signals for controls
    function CreateComboBox(ComboBoxObject: TObject): Pointer;
    function CreateAPIWidget(AWinControl: TWinControl): PGtkWidget;
    function CreateForm(ACustomForm: TCustomForm): PGtkWidget;
    procedure CreateComponent(Sender : TObject);virtual;
    Procedure FinishComponentCreate(Sender : TObject; Handle : Pointer;
      SetupProps : Boolean); Virtual;
    procedure DestroyLCLControl(Sender : TObject);virtual;
    procedure AddChild(Parent,Child : Pointer; Left,Top: Integer);virtual;
    procedure AssignSelf(Child ,Data : Pointer);virtual;
    procedure SetClipboardWidget(TargetWidget: PGtkWidget);virtual;
    
    // device contexts
    function IsValidDC(const DC: HDC): Boolean;virtual;
    function NewDC: TDeviceContext;virtual;
    procedure DisposeDC(aDC: TDeviceContext);virtual;
    function CreateDCForWidget(TheWidget: PGtkWidget; TheWindow: PGdkWindow): HDC;
    
    // GDIObjects
    function IsValidGDIObject(const GDIObject: HGDIOBJ): Boolean;virtual;
    function IsValidGDIObjectType(const GDIObject: HGDIOBJ;
                                  const GDIType: TGDIType): Boolean;virtual;
    function NewGDIObject(const GDIType: TGDIType): PGdiObject;virtual;
    procedure DisposeGDIObject(GdiObject: PGdiObject);virtual;
    procedure SelectGDKBrushProps(DC: HDC);virtual;
    procedure SelectGDKTextProps(DC: HDC);virtual;
    procedure SelectGDKPenProps(DC: HDC);virtual;
    function CreateDefaultBrush: PGdiObject;virtual;
    function CreateDefaultFont: PGdiObject;virtual;
    function CreateDefaultPen: PGdiObject;virtual;
    procedure UpdateDCTextMetric(DC: TDeviceContext);

    // images
    procedure LoadXPMFromLazResource(const ResourceName: string;
      Window: PGdkWindow; var PixmapImg, PixmapMask: PGdkPixmap);virtual;
    procedure LoadFromXPMFile(Bitmap: TObject; Filename: PChar);virtual;
    procedure LoadFromPixbufFile(Bitmap: TObject; Filename: PChar);virtual;
    procedure LoadFromPixbufData(Bitmap : hBitmap; Data : PByte);virtual;
    function InternalGetDIBits(DC: HDC; Bitmap: HBitmap; StartScan, NumScans: UINT;
      BitSize : Longint; Bits: Pointer; var BitInfo: BitmapInfo; Usage: UINT; DIB : Boolean): Integer;virtual;

    procedure SetRCFilename(const AValue: string);virtual;
    procedure CheckRCFilename;virtual;
    procedure ParseRCFile;virtual;

    // notebook
    procedure GetNoteBookCloseBtnPixmap(Window: PGdkWindow;
                                        var Img, Mask: PGdkPixmap);virtual;
    procedure AddDummyNoteBookPage(NoteBookWidget: PGtkNoteBook);virtual;
    procedure RemoveDummyNoteBookPage(NoteBookWidget: PGtkNotebook);virtual;
    procedure UpdateNotebookPageTab(ANoteBook, APage: TObject);virtual;
    procedure AddNBPage(ANoteBook, APage: TObject; Index: Integer);virtual;
    procedure RemoveNBPage(ANoteBook: TObject; Index: Integer);virtual;
    procedure MoveNBPage(ANoteBook, APage: TObject; NewIndex: Integer);virtual;

    // listview
    procedure ListViewChangeItem(TheListView: TObject; Index: integer);
    procedure ListViewAddItem(TheListView: TObject);
    
    // listbox
    function GetTopIndex(Sender: TObject): integer;
    function SetTopIndex(Sender: TObject; NewTopIndex: integer): integer;

    // forms
    procedure BringFormToFront(Sender: TObject);
    procedure SetWindowSizeAndPosition(Window: PGtkWindow;
      AWinControl: TWinControl);virtual;
    procedure ShowModal(Sender: TObject); virtual;
    procedure UpdateTransientWindows; virtual;

    Function GetCaption(Sender : TObject) : String; virtual;
    function  GetText(Sender: TComponent; var Text: String): Boolean; virtual;
    procedure SetText(Child,Data : Pointer);virtual;
    procedure SetLabel(Sender : TObject; Data : Pointer); virtual;
    procedure SetColor(Sender : TObject);virtual;
    Procedure SetPixel(Sender : TObject; Data : Pointer);virtual;
    Procedure GetPixel(Sender : TObject; Data : Pointer);virtual;
    function  GetValue(Sender : TObject; Data : pointer) : integer;virtual;
    function  SetValue(Sender : TObject; Data : pointer) : integer;virtual;
    function  SetProperties (Sender: TObject) : integer;virtual;
    procedure AttachMenu(Sender: TObject);virtual;
    procedure SetColorDialogColor(ColorSelection: PGtkColorSelection;
      Color: TColor);virtual;
    function ForceLineBreaks(DC : hDC; Src: PChar; MaxWidthInPixels : Longint;
      ProcessAmpersands : Boolean) : PChar;
    procedure WordWrap(DC: HDC; AText: PChar; MaxWidthInPixel: integer;
      var Lines: PPChar; var LineCount: integer);

    // control functions for messages, callbacks
    procedure ResizeChild(Sender : TObject; Left,Top,Width,Height : Integer);virtual;
    procedure SetResizeRequest(Widget: PGtkWidget);virtual;
    procedure UnsetResizeRequest(Widget: PGtkWidget);virtual;

    function HashPaintMessage(p: pointer): integer;virtual;
    function FindPaintMessage(HandleWnd: HWnd): PLazQueueItem;virtual;

    procedure RemoveCallbacks(Sender : TObject); virtual;
    function  RecreateWnd(Sender: TObject): Integer; virtual;
  public
    // for gtk specific components:
    procedure SetCallback(Msg : LongInt; Sender : TObject); virtual;
    procedure SendPaintMessagesForInternalWidgets(AWinControl: TWinControl);
    function  LCLtoGtkMessagePending: boolean;virtual;
    procedure SendCachedGtkMessages;virtual;
    procedure RealizeWidgetSize(Widget: PGtkWidget;
      NewWidth, NewHeight: integer);
  public
    constructor Create; 
    destructor Destroy; override;
    function  IntSendMessage3(LM_Message : Integer; Sender : TObject;
                              data : pointer) : integer; override;
    procedure HandleEvents; override;
    procedure WaitMessage; override;
    procedure SendCachedLCLMessages; override;
    procedure AppTerminate; override;
    procedure AppInit; override;

    function CreateTimer(Interval: integer; TimerFunc: TFNTimerProc) : integer; override;
    function DestroyTimer(TimerHandle: integer) : boolean; override;

    {$I gtkwinapih.inc}
    
  public
    property RCFilename: string read FRCFilename write SetRCFilename;
  end;

{$I gtklistslh.inc}

implementation

uses
  Graphics, Buttons, Menus, GTKWinApiWindow, StdCtrls, ComCtrls, CListBox,
  KeyMap, Calendar, Arrow, Spin, CommCtrl, ExtCtrls, Dialogs, FileCtrl,
  LResources, Math, gtkglobals, gtkproc;

{$I gtklistsl.inc}

{$I gtkobject.inc}
{$I gtkwinapi.inc}

procedure InternalInit;
var
  c: TClipboardType;
  cr: TCursor;
begin
  gtk_handler_quark := g_quark_from_static_string('gtk-signal-handlers');

  MouseCaptureWidget := nil;
  MouseCapureByLCL := false;
  
  LastLeft:=EmptyLastMouseClick;
  LastMiddle:=EmptyLastMouseClick;
  LastRight:=EmptyLastMouseClick;

  // clipboard
  ClipboardSelectionData:=TList.Create;
  for c:=Low(TClipboardType) to High(TClipboardType) do begin
    ClipboardTypeAtoms[c]:=0;
    ClipboardHandler[c]:=nil;
    //ClipboardIgnoreLossCount[c]:=0;
    ClipboardTargetEntries[c]:=nil;
    ClipboardTargetEntryCnt[c]:=0;
  end;
  
  // mouse cursors
  for cr:=Low(GDKMouseCursors) to High(GDKMouseCursors) do begin
    GDKMouseCursors[cr]:=nil;
    CursorToGDKCursor[cr]:=GDK_LEFT_PTR;
  end;
  CursorToGDKCursor[crDefault]  := GDK_LEFT_PTR;
  CursorToGDKCursor[crNone]     := GDK_LEFT_PTR;
  CursorToGDKCursor[crArrow]    := GDK_Arrow;
  CursorToGDKCursor[crCross]    := GDK_Cross;
  CursorToGDKCursor[crIBeam]    := GDK_XTerm;
  CursorToGDKCursor[crSize]     := GDK_FLEUR;
  CursorToGDKCursor[crSizeNESW] := GDK_BOTTOM_LEFT_CORNER;
  CursorToGDKCursor[crSizeNS]   := GDK_SB_V_DOUBLE_ARROW;
  CursorToGDKCursor[crSizeNWSE] := GDK_TOP_LEFT_CORNER;
  CursorToGDKCursor[crSizeWE]   := GDK_SB_H_DOUBLE_ARROW;
  CursorToGDKCursor[crUpArrow]  := GDK_LEFT_PTR;
  CursorToGDKCursor[crHourGlass]:= GDK_CLOCK;
  CursorToGDKCursor[crDrag]     := GDK_SAILBOAT;
  CursorToGDKCursor[crNoDrop]   := GDK_IRON_CROSS;
  CursorToGDKCursor[crHSplit]   := GDK_SB_H_DOUBLE_ARROW;
  CursorToGDKCursor[crVSplit]   := GDK_SB_V_DOUBLE_ARROW;
  CursorToGDKCursor[crMultiDrag]:= GDK_SAILBOAT;
  CursorToGDKCursor[crSQLWait]  := GDK_LEFT_PTR;
  CursorToGDKCursor[crNo]       := GDK_LEFT_PTR;
  CursorToGDKCursor[crAppStart] := GDK_LEFT_PTR;
  CursorToGDKCursor[crHelp]     := GDK_QUESTION_ARROW;
  CursorToGDKCursor[crHandPoint]:= GDK_Hand1;
  CursorToGDKCursor[crSizeAll]  := GDK_FLEUR;

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
  Revision 1.122  2003/03/15 18:32:38  mattias
  implemented transient windows for all cases

  Revision 1.121  2003/03/15 09:42:49  mattias
  fixed transient windows

  Revision 1.120  2003/03/09 21:13:32  mattias
  localized gtk interface

  Revision 1.119  2003/02/24 11:51:44  mattias
  combobox height can now be set, added OI item height option

  Revision 1.118  2003/01/27 13:49:16  mattias
  reduced speedbutton invalidates, added TCanvas.Frame

  Revision 1.117  2003/01/06 12:00:16  mattias
  implemented fsStayOnTop+bsNone for forms under gtk (useful for splash)

  Revision 1.116  2002/12/27 17:12:38  mattias
  added more Delphi win32 compatibility functions

  Revision 1.115  2002/12/27 10:23:40  mattias
  implemented TListBox.TopIndex

  Revision 1.114  2002/12/27 08:46:32  mattias
  changes for fpc 1.1

  Revision 1.113  2002/12/17 16:32:12  mattias
  freeing GDIObjects without AppTerminate

  Revision 1.112  2002/12/15 11:52:28  mattias
  started gtk2 interface

  Revision 1.111  2002/02/09 01:48:23  mattias
  renamed TinterfaceObject.Init to AppInit and TWinControls can now contain childs in gtk

  Revision 1.110  2002/12/04 20:39:15  mattias
  patch from Vincent: clean ups and fixed crash on destroying window

  Revision 1.109  2002/12/03 09:20:53  mattias
  cleaned up

  Revision 1.108  2002/12/03 09:11:36  mattias
  cleaned up

  Revision 1.107  2002/11/25 11:37:19  mattias
  applied patch from Vasily

  Revision 1.106  2002/11/23 13:50:04  mattias
  removed unused variable

  Revision 1.105  2002/11/23 13:48:44  mattias
  added Timer patch from Vincent Snijders

  Revision 1.104  2002/11/12 13:16:05  lazarus
  MG: fixed TListView with more than 2 columns

  Revision 1.103  2002/11/12 10:53:43  lazarus
  MG: fixed setting gdk pen style

  Revision 1.102  2002/11/12 10:16:18  lazarus
  MG: fixed TMainMenu creation

  Revision 1.101  2002/11/09 18:13:33  lazarus
  MG: fixed gdkwindow checks

  Revision 1.100  2002/11/09 15:02:07  lazarus
  MG: fixed LM_LVChangedItem, OnShowHint, small bugs

  Revision 1.99  2002/11/02 22:25:36  lazarus
  MG: implemented TMethodList and Application Idle handlers

  Revision 1.98  2002/10/30 12:37:25  lazarus
  MG: mouse cursors are now allocated on demand

  Revision 1.97  2002/10/30 00:08:09  lazarus
  MG: finished ParseRCFile

  Revision 1.96  2002/10/28 18:17:02  lazarus
  MG: impoved focussing, unfocussing on destroy and fixed unit search

  Revision 1.95  2002/10/26 15:15:50  lazarus
  MG: broke LCL<->interface circles

  Revision 1.94  2002/10/24 22:10:39  lazarus
  AJ: More changes for better code reuse between gnome & gtk interfaces

  Revision 1.93  2002/10/21 03:23:35  lazarus
  AJ: rearranged GTK init stuff for proper GNOME init & less duplication between interfaces

  Revision 1.92  2002/10/20 21:49:10  lazarus
  MG: fixes for fpc1.1

  Revision 1.91  2002/10/17 21:00:17  lazarus
  MG: fixed uncapturing of mouse

  Revision 1.90  2002/10/15 22:28:05  lazarus
  AJ: added forcelinebreaks

  Revision 1.89  2002/10/15 07:01:29  lazarus
  MG: fixed timer checking

  Revision 1.88  2002/10/14 19:00:49  lazarus
  MG: fixed zombie timers

  Revision 1.87  2002/10/14 14:29:50  lazarus
  AJ: Improvements to TUpDown; Added TStaticText & GNOME DrawText

  Revision 1.86  2002/10/10 19:43:16  lazarus
  MG: accelerated GetTextMetrics

  Revision 1.85  2002/10/10 08:51:13  lazarus
  MG: added paint messages for some gtk internal widgets

  Revision 1.84  2002/10/08 23:44:00  lazarus
  AJ: started GNOME interface & modified gtk interface so everything is public/protected

  Revision 1.83  2002/10/08 13:42:23  lazarus
  MG: added TDevContextColorType

  Revision 1.82  2002/10/07 20:50:58  lazarus
  MG: accelerated SelectGDKPenProps

  Revision 1.81  2002/10/06 17:55:45  lazarus
  MG: JITForms now sets csDesigning before creation

  Revision 1.80  2002/10/03 18:04:46  lazarus
  MG: started customdrawitem

  Revision 1.79  2002/10/03 14:47:31  lazarus
  MG: added TComboBox.OnPopup+OnCloseUp+ItemWidth

  Revision 1.78  2002/10/01 10:12:34  lazarus
  MG: added SendCachedLCLMessages to interfacebase for wysiwyg

  Revision 1.77  2002/10/01 10:05:48  lazarus
  MG: changed PDeviceContext into class TDeviceContext

  Revision 1.76  2002/09/19 16:45:54  lazarus
  MG: fixed Menu.Free and gdkwindow=nil bug

  Revision 1.75  2002/09/18 17:07:28  lazarus
  MG: added patch from Andrew

  Revision 1.74  2002/09/16 15:56:01  lazarus
  Resize cursors in designer.

  Revision 1.73  2002/09/12 05:56:16  lazarus
  MG: gradient fill, minor issues from Andrew

  Revision 1.72  2002/09/10 06:49:20  lazarus
  MG: scrollingwincontrol from Andrew

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
