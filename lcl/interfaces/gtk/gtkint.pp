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

{$DEFINE Use_KeyStateList} // keep track of keystates instead of using OS
                           // This is the old mode and might be removed


{$IFDEF win32}
{$DEFINE NoGdkPixbufLib}
{$ELSE}
{off $DEFINE NoGdkPixbufLib}
{$ENDIF}
{off $Define Critical_Sections_Support}

{off $Define Disable_GC_SysColors}

{$IFDEF gtk2}
  {$IFDEF NoGdkPixbufLib}
    {$UNDEF NoGdkPixbufLib}
  {$EndIF}
{$EndIF}

uses
  {$IFDEF WIN32}
  // use windows unit first,
  // if not, Rect and Point are taken from the windows unit instead of classes.
  Windows,
  {$ENDIF}
  // rtl+fcl
  Classes, SysUtils, FPCAdds,
  // interfacebase
  InterfaceBase,
  // gtk
  {$IFDEF gtk2}
  glib2, gdk2pixbuf, gdk2, gtk2, Pango,
  {$ELSE}
  glib, gdk, gtk, {$Ifndef NoGdkPixbufLib}gdkpixbuf,{$EndIf} GtkFontCache,
  {$ENDIF}
  // Target OS specific
  {$IFDEF UNIX}
  x, xlib,
  {$ENDIF}
  // LCL
  ExtDlgs, Dialogs, Controls, Forms, LCLStrConsts, LMessages,
  LCLProc, LCLIntf, LCLType, gtkDef, DynHashArray, gtkMsgQueue,
  GraphType, GraphMath, Graphics, Menus;


type
  TGTKWidgetSet = class(TWidgetSet)
  protected
    FKeyStateList_: TList; // Keeps track of which keys are pressed
    FDeviceContexts: TDynHashArray;// hasharray of HDC
    FGDIObjects: TDynHashArray;    // hasharray of PGdiObject
    FMessageQueue: TGtkMessageQueue;      // queue of PMsg
    WaitingForMessages: boolean;

    FRCFilename: string;
    FRCFileParsed: boolean;
    FRCFileAge: integer;
    FWidgetsWithResizeRequest: TDynHashArray; // hasharray of PGtkWidget
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

    {$Ifdef GTK2}
    FDefaultFontDesc: PPangoFontDescription;
    {$Else}
    FDefaultFont: PGdkFont;
    {$EndIf}
    FStockSystemFont: HFONT;
    FExtUTF8OutCache: Pointer;
    FExtUTF8OutCacheSize: integer;

    Function CreateSystemFont : hFont;

    procedure InitStockItems; virtual;
    procedure FreeStockItems; virtual;
    procedure PassCmdLineOptions; override;

    // styles
    procedure FreeAllStyles; virtual;
    Function GetCompStyle(Sender : TObject) : Longint; Virtual;

    // create and destroy
    function CreateComboBox(ComboBoxObject: TObject): Pointer;
    function CreateAPIWidget(AWinControl: TWinControl): PGtkWidget;
    function CreateForm(ACustomForm: TCustomForm): PGtkWidget; virtual;
    function CreateListView(ListViewObject: TObject): PGtkWidget; virtual;
    function CreatePairSplitter(PairSplitterObject: TObject): PGtkWidget;
    function CreateStatusBar(StatusBar: TObject): PGtkWidget;
    function OldCreateStatusBarPanel(StatusBar: TObject; Index: integer): PGtkWidget;
    function CreateSimpleClientAreaWidget(Sender: TObject;
      NotOnParentsClientArea: boolean): PGtkWidget;
    function CreateToolBar(ToolBarObject: TObject): PGtkWidget;
    procedure DestroyEmptySubmenu(Sender: TObject);virtual;
    procedure DestroyConnectedWidget(Widget: PGtkWidget;
                                     CheckIfDestroying: boolean);virtual;
    function  RecreateWnd(Sender: TObject): Integer; virtual;
    procedure AssignSelf(Child, Data: Pointer);virtual;

    // clipboard
    procedure SetClipboardWidget(TargetWidget: PGtkWidget);virtual;

    // device contexts
    function IsValidDC(const DC: HDC): Boolean;virtual;
    function NewDC: TDeviceContext;virtual;
    procedure DisposeDC(aDC: TDeviceContext);virtual;
    function CreateDCForWidget(TheWidget: PGtkWidget; TheWindow: PGdkWindow;
      WithChildWindows: boolean): HDC;
    function GetDoubleBufferedDC(Handle: HWND): HDC;

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
    procedure UpdateDCTextMetric(DC: TDeviceContext); virtual;
    {$Ifdef GTK2}
    function GetDefaultFontDesc(IncreaseReferenceCount: boolean): PPangoFontDescription;
    {$Else}
    function GetDefaultFont(IncreaseReferenceCount: boolean): PGDKFont;
    {$EndIf}
    function CreateRegionCopy(SrcRGN: hRGN): hRGN; override;
    function DCClipRegionValid(DC: HDC): boolean; override;
    function CreateEmptyRegion: hRGN; override;

    // images
    {$IfNDef NoGdkPixbufLib}
    procedure LoadPixbufFromLazResource(const ResourceName: string;
      var Pixbuf: PGdkPixbuf);
    {$EndIf}
    procedure LoadFromXPMFile(Bitmap: TObject; Filename: PChar);virtual;
    procedure LoadFromPixbufFile(Bitmap: TObject; Filename: PChar);virtual;
    procedure LoadFromPixbufData(Bitmap : hBitmap; Data : PByte);virtual;
    function InternalGetDIBits(DC: HDC; Bitmap: HBitmap; StartScan, NumScans: UINT;
      BitSize : Longint; Bits: Pointer; var BitInfo: BitmapInfo; Usage: UINT; DIB : Boolean): Integer;virtual;
    function GetWindowRawImageDescription(GDKWindow: PGdkWindow;
      Desc: PRawImageDescription): boolean;
    function GetRawImageFromGdkWindow(GDKWindow: PGdkWindow;
      MaskBitmap: PGdkBitmap; const SrcRect: TRect;
      var NewRawImage: TRawImage): boolean;
    function GetRawImageMaskFromGdkBitmap(MaskBitmap: PGdkBitmap;
      const SrcRect: TRect; var RawImage: TRawImage): boolean;
    function StretchCopyArea(DestDC: HDC; X, Y, Width, Height: Integer;
      SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer;
      Mask: HBITMAP; XMask, YMask: Integer;
      Rop: Cardinal): Boolean;

    // RC file
    procedure SetRCFilename(const AValue: string);virtual;
    procedure CheckRCFilename;virtual;
    procedure ParseRCFile;virtual;

    // notebook
    procedure AddDummyNoteBookPage(NoteBookWidget: PGtkNoteBook);virtual;

    // forms and dialogs
    procedure BringFormToFront(Sender: TObject);
    procedure SetWindowSizeAndPosition(Window: PGtkWindow;
      AWinControl: TWinControl);virtual;
    procedure UntransientWindow(GtkWindow: PGtkWindow);
    procedure InitializeFileDialog(FileDialog: TFileDialog;
      var SelWidget: PGtkWidget; Title: PChar);
    procedure InitializeFontDialog(FontDialog: TFontDialog;
      var SelWidget: PGtkWidget; Title: PChar);
    procedure InitializeCommonDialog(ADialog: TObject; AWindow: PGtkWidget);
    function CreateOpenDialogFilter(OpenDialog: TOpenDialog;
      SelWidget: PGtkWidget): string;
    procedure CreatePreviewDialogControl(PreviewDialog: TPreviewFileDialog;
      SelWidget: PGtkWidget);
    procedure InitializeOpenDialog(OpenDialog: TOpenDialog;
      SelWidget: PGtkWidget);

    // misc
    Function GetCaption(Sender : TObject) : String; virtual;
    procedure WordWrap(DC: HDC; AText: PChar; MaxWidthInPixel: integer;
      var Lines: PPChar; var LineCount: integer);
    procedure OldUpdateStatusBarPanels(StatusBar: TObject;
                                    StatusBarWidget: PGtkWidget); virtual;
    procedure OldUpdateStatusBarPanel(StatusBar: TObject; Index: integer;
                                   StatusPanelWidget: PGtkWidget); virtual;

    procedure ResizeChild(Sender : TObject; Left,Top,Width,Height : Integer);virtual;
    procedure RemoveCallbacks(Widget: PGtkWidget); virtual;
    function ROP2ModeToGdkFunction(Mode: Integer): TGdkFunction;
    function gdkFunctionToROP2Mode(aFunction: TGdkFunction): Integer;
  public
    // for gtk specific components:
    procedure SetLabelCaption(const ALabel: PGtkLabel; const ACaption: String; const AComponent: TComponent; const ASignalWidget: PGTKWidget; const ASignal: PChar); virtual;
    procedure SetCallback(const AMsg: LongInt; const AGTKObject: PGTKObject; const ALCLObject: TObject); virtual;
    procedure SendPaintMessagesForInternalWidgets(AWinControl: TWinControl);
    function  LCLtoGtkMessagePending: boolean;virtual;
    procedure SendCachedGtkMessages;virtual;
    procedure RealizeWidgetSize(Widget: PGtkWidget;
                                NewWidth, NewHeight: integer); virtual;
    procedure FinishComponentCreate(const ALCLObject: TObject;
              const AGTKObject: Pointer; const ASetupProps : Boolean); virtual;

    // show, hide and invalidate
    procedure ShowHide(Sender : TObject);virtual;

    // control functions for messages, callbacks
    procedure HookSignals(const AGTKObject: PGTKObject; const ALCLObject: TObject); virtual;  //hooks all signals for controls
  public
    constructor Create;
    destructor Destroy; override;
    procedure HandleEvents; override;
    procedure WaitMessage; override;
    procedure SendCachedLCLMessages; override;
    procedure AppTerminate; override;
    procedure AppInit(var ScreenInfo: TScreenInfo); override;
    procedure AppMinimize; override;
    procedure AppBringToFront; override;
    function  DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor; override;
    procedure DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor); override;
    procedure DCRedraw(CanvasHandle: HDC); override;
    procedure SetDesigning(AComponent: TComponent); override;
    
    // helper routines needed by interface methods
    procedure UnsetResizeRequest(Widget: PGtkWidget);virtual;
    procedure SetResizeRequest(Widget: PGtkWidget);virtual;
    // |-forms
    procedure UpdateTransientWindows; virtual;
    // |-listbox
    procedure SetSelectionMode(Sender: TObject; Widget: PGtkWidget;
                               MultiSelect, ExtendedSelect: boolean); virtual;
    function ForceLineBreaks(DC : hDC; Src: PChar; MaxWidthInPixels : Longint;
      ProcessAmpersands : Boolean) : PChar;
  
    // create and destroy
    function CreateComponent(Sender : TObject): THandle; override;
    function CreateTimer(Interval: integer; TimerFunc: TFNTimerProc) : integer; override;
    function DestroyTimer(TimerHandle: integer) : boolean; override;
    procedure DestroyLCLComponent(Sender: TObject);virtual;

    {$I gtkwinapih.inc}
    {$I gtklclintfh.inc}

  public
    property RCFilename: string read FRCFilename write SetRCFilename;
  end;

{$I gtklistslh.inc}

var
  GTKWidgetSet: TGTKWidgetSet;

implementation

uses
////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To get as litle as posible circles,
// uncomment only those units with implementation
////////////////////////////////////////////////////
// GtkWSActnList,
 GtkWSArrow,
 GtkWSButtons,
 GtkWSCalendar,
 GtkWSCheckLst,
// GtkWSCListBox,
 GtkWSComCtrls,
 GtkWSControls,
// GtkWSDbCtrls,
// GtkWSDBGrids,
 GtkWSDialogs,
// GtkWSDirSel,
// GtkWSEditBtn,
 GtkWSExtCtrls,
// GtkWSExtDlgs,
// GtkWSFileCtrl,
 GtkWSForms,
// GtkWSGrids,
// GtkWSImgList,
// GtkWSMaskEdit,
 GtkWSMenus,
// GtkWSPairSplitter,
 GtkWSSpin,
 GtkWSStdCtrls,
// GtkWSToolwin,
////////////////////////////////////////////////////
  Buttons, StdCtrls, PairSplitter, Math,
  GTKWinApiWindow, ComCtrls, CListBox, Calendar, Arrow, Spin, CommCtrl,
  ExtCtrls, FileCtrl, LResources, gtkglobals, gtkproc;
  
const
  GtkNil = nil;

{$I gtklistsl.inc}
{$I gtkobject.inc}
{$I gtkwinapi.inc}
{$I gtklclintf.inc}


procedure InternalInit;
var
  c: TClipboardType;
  cr: TCursor;
begin
  gtk_handler_quark := g_quark_from_static_string('gtk-signal-handlers');

  MouseCaptureWidget := nil;
  MouseCaptureType := mctGTK;

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
  CursorToGDKCursor[crSizeNW]   := GDK_TOP_LEFT_CORNER;
  CursorToGDKCursor[crSizeN]    := GDK_TOP_SIDE;
  CursorToGDKCursor[crSizeNE]   := GDK_TOP_RIGHT_CORNER;
  CursorToGDKCursor[crSizeW]    := GDK_LEFT_SIDE;
  CursorToGDKCursor[crSizeE]    := GDK_RIGHT_SIDE;
  CursorToGDKCursor[crSizeSW]   := GDK_BOTTOM_LEFT_CORNER;
  CursorToGDKCursor[crSizeS]    := GDK_BOTTOM_SIDE;
  CursorToGDKCursor[crSizeSE]   := GDK_BOTTOM_RIGHT_CORNER;
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
  {$I gtkimages.lrs}
  InternalInit;

finalization
  InternalFinal;

end.

{ =============================================================================

  $Log$
  Revision 1.222  2005/02/28 18:21:43  mattias
  fixed gtk file dialog with empty filter  from Collin

  Revision 1.221  2005/02/05 09:09:52  mattias
  implemented TListView for gtk2   From Andrew Haines

  Revision 1.220  2005/02/03 21:10:07  vincents
  fixed compilation of gtk2 interface on windows

  Revision 1.219  2005/01/13 22:07:10  mattias
  added mouse cursors for 8 uni directions, imlemented for gtk

  Revision 1.218  2005/01/08 11:03:18  mattias
  implemented TPen.Mode=pmXor  from Jesus

  Revision 1.217  2004/10/16 15:36:49  mattias
  implemented gtkwscomctrls.TGtkWSStatusBar

  Revision 1.216  2004/09/24 21:34:14  micha
  convert LM_CREATE message to interface methods
  remove SendMsgToInterface, CNSendMessage and related methods
  remove TWidgetSet.IntSendMessage3; all LCL to interface messages have been converted

  Revision 1.215  2004/09/24 19:02:38  micha
  convert LM_MOVEPAGE message to interface method

  Revision 1.214  2004/09/24 18:00:51  micha
  convert LM_NB_UPDATETAB message to interface method

  Revision 1.213  2004/09/24 15:31:01  micha
  convert LM_LB_GETTOPINDEX and LM_LB_SETTOPINDEX message to interface methods

  Revision 1.212  2004/09/24 14:50:57  micha
  convert LM_SETDESIGNING message to TWidgetSet method

  Revision 1.211  2004/09/24 07:52:35  micha
  convert LM_SETPROPERTIES message to interface method for TCustomTrackBar
  remove message LM_SETPROPERTIES, conversion done

  Revision 1.210  2004/09/19 18:50:28  micha
  convert LM_SETVALUE message to interface methods

  Revision 1.209  2004/09/18 17:07:57  micha
  convert LM_GETVALUE message to interface method

  Revision 1.208  2004/09/18 12:43:15  micha
  convert LM_DESTROY message to interface methods

  Revision 1.207  2004/09/18 10:52:48  micha
  convert LM_SCREENINIT message to interface method (integrated with TWidgetSet.AppInit(var ScreenInfo)

  Revision 1.206  2004/09/16 14:32:31  micha
  convert LM_SETSELMODE message to interface method

  Revision 1.205  2004/09/14 10:06:26  micha
  convert LM_REDRAW message to interface method (in twidgetset)

  Revision 1.204  2004/09/13 19:06:04  micha
  convert LM_ADDPAGE and LM_REMOVEPAGE messages to new interface methods

  Revision 1.203  2004/09/13 13:13:46  micha
  convert LM_SHOWMODAL to interface methods

  Revision 1.202  2004/09/12 19:50:35  micha
  convert LM_SETSIZE message to new interface method

  Revision 1.201  2004/09/12 13:11:50  micha
  convert LM_GETPIXEL and LM_SETPIXEL to interface methods (of twidgetset, DCGetPixel and DCSetPixel)

  Revision 1.200  2004/09/11 13:38:37  micha
  convert LM_BRINGTOFRONT message to interface method
  NOTE: was only used for tapplication, not from other controls

  Revision 1.199  2004/09/11 13:06:48  micha
  convert LM_ADDCHILD message to interface method

  Revision 1.198  2004/09/10 20:19:13  micha
  convert LM_CLB_G/SETCHECKED to interface methods

  Revision 1.197  2004/09/10 18:58:22  micha
  convert LM_ATTACHMENU to interface method

  Revision 1.196  2004/09/10 17:59:57  micha
  convert LM_APPENDTEXT to interface method

  Revision 1.195  2004/09/10 16:28:51  mattias
  implemented very rudimentary TTabControl

  Revision 1.194  2004/09/10 14:38:29  micha
  convert lm_gettext to new interface methods
  remove lm_settext replacement settext methods in twidgetsets

  Revision 1.193  2004/09/10 09:43:13  micha
  convert LM_SETLABEL message to interface methods

  Revision 1.192  2004/09/08 20:47:17  micha
  convert LM_SHOWHIDE message to new intf method TWSWinControl.ShowHide

  Revision 1.191  2004/09/08 19:09:34  micha
  convert LM_SETCOLOR message to new intf method TWSWinControl.SetColor

  Revision 1.190  2004/09/07 10:26:16  micha
  fix logs to get rid of comment level 2 warning

  Revision 1.189  2004/09/07 09:44:46  micha
  convert lcl messages to new interface using methods: LM_G/SETSELSTART, LM_G/SETSELLEN, LM_G/SETLIMITTEXT

  Revision 1.188  2004/09/02 09:16:59  mattias
  improved double byte char fonts for gtk1, started synedit UTF8 support

  Revision 1.187  2004/08/27 08:55:22  micha
  implement tapplication.minimize for win32, stub for gtk

  Revision 1.186  2004/08/18 20:49:02  mattias
  simple forms can now be child controls

  Revision 1.185  2004/08/18 09:31:21  mattias
  removed obsolete unit vclglobals

  Revision 1.184  2004/08/10 17:34:13  mattias
  implemented font cache for gtk, which accelerates switching fonts

  Revision 1.183  2004/05/22 14:35:32  mattias
  fixed button return key

  Revision 1.182  2004/05/18 23:10:41  marc
  * Started to move TListview to the WS interface

  Revision 1.181  2004/04/18 23:55:39  marc
  * Applied patch from Ladislav Michl
  * Changed the way TControl.Text is resolved
  * Added setting of text to TWSWinControl

  Revision 1.180  2004/04/03 16:47:46  mattias
  implemented converting gdkbitmap to RawImage mask

  Revision 1.179  2004/03/26 21:20:54  vincents
  Fixed line endings

  Revision 1.178  2004/03/24 01:21:41  marc
  * Simplified signals for gtkwsbutton

  Revision 1.177  2004/03/19 00:53:34  marc
  * Removed all ComponentCreateHandle routines

  Revision 1.176  2004/03/19 00:03:15  marc
  * Moved the implementation of (GTK)ButtonCreateHandle to the new
    (GTK)WSButton class

  Revision 1.175  2004/03/18 22:35:52  mattias
  improved TCustomListView.ItemAdded with an Index param  from Andrew

  Revision 1.174  2004/03/17 19:59:56  marc
  * Fixes some typos and changes uses clause cases

  Revision 1.173  2004/03/17 00:34:37  marc
  * Interface reconstruction. Created skeleton units, classes and wscontrols

  Revision 1.172  2004/03/05 00:31:52  marc
  * Renamed TGtkObject to TGtkWidgetSet

  Revision 1.171  2004/03/05 00:14:02  marc
  * Renamed TInterfaceBase to TWidgetSet

  Revision 1.170  2004/02/27 00:42:41  marc
  * Interface CreateComponent splitup
  * Implemented CreateButtonHandle on GTK interface
    on win32 interface it still needs to be done
  * Changed ApiWizz to support multilines and more interfaces

  Revision 1.169  2004/02/23 18:24:38  mattias
  completed new TToolBar

  Revision 1.168  2004/02/21 15:37:33  mattias
  moved compiler options to project menu, added -CX for smartlinking

  Revision 1.167  2004/01/22 11:23:36  mattias
  started MaskBlt for gtkIF and applied patch for dir dlg in env opts from Vincent

  Revision 1.166  2004/01/10 22:34:20  mattias
  started double buffering for gtk intf

  Revision 1.165  2004/01/09 20:03:13  mattias
  implemented new statusbar methods in gtk intf

  Revision 1.164  2004/01/04 16:44:33  mattias
  updated gtk2 package

  Revision 1.163  2004/01/03 11:57:47  mattias
  applied implementation for LM_LB_GETINDEXAT  from Vincent

  Revision 1.162  2003/12/25 14:17:07  mattias
  fixed many range check warnings

  Revision 1.161  2003/12/16 14:01:27  mattias
  fixed compilation gtk and fpc 1.9

  Revision 1.160  2003/11/27 23:02:30  mattias
  removed menutype.pas

  Revision 1.159  2003/11/26 21:30:19  mattias
  reduced unit circles, fixed fpImage streaming

  Revision 1.158  2003/11/26 00:23:47  marc
  * implemented new LCL(check|enable)Menuitem functions
  * introduced the lclintf inc files to win32

  Revision 1.157  2003/11/24 11:03:07  marc
  * Splitted winapi*.inc into a winapi and a lcl interface communication part

  Revision 1.156  2003/11/10 16:15:32  micha
  cleanups; win32 fpimage support

  Revision 1.155  2003/11/03 22:37:41  mattias
  fixed vert scrollbar, implemented GetDesignerDC

  Revision 1.154  2003/11/01 10:27:41  mattias
  fpc 1.1 fixes, started scrollbar hiding, started polymorphing client areas

  Revision 1.153  2003/10/16 23:54:27  marc
  Implemented new gtk keyevent handling

  Revision 1.152  2003/09/22 19:17:26  ajgenius
  begin implementing GtkTreeView for ListBox/CListBox

  Revision 1.151  2003/09/22 15:34:07  ajgenius
  use GtkImage and Pixbuf for GTK2 instead of Deprecated GtkPixmap

  Revision 1.150  2003/09/20 13:27:49  mattias
  varois improvements for ParentColor from Micha

  Revision 1.149  2003/09/19 00:41:51  ajgenius
  remove USE_PANGO define since pango now apears to work properly.

  Revision 1.148  2003/09/18 17:23:04  ajgenius
  start using GtkTextView for Gtk2 Memo

  Revision 1.147  2003/09/18 09:21:03  mattias
  renamed LCLLinux to LCLIntf

  Revision 1.146  2003/09/15 03:10:46  ajgenius
  PANGO support for GTK2 now works.. sorta. TextOut/ExtTextOut broken?

  Revision 1.145  2003/09/12 17:40:45  ajgenius
  fixes for GTK2(accel groups, menu accel, 'draw'),
  more work toward Pango(DrawText now works, UpdateDCTextMetric mostly works)

  Revision 1.144  2003/09/09 20:46:38  ajgenius
  more implementation toward pango for gtk2

  Revision 1.143  2003/09/05 19:29:38  mattias
  Success: The first gtk2 application ran without error

  Revision 1.142  2003/09/04 10:51:30  mattias
  fixed default size of preview widget

  Revision 1.141  2003/09/02 21:32:56  mattias
  implemented TOpenPictureDialog

  Revision 1.140  2003/08/28 09:10:00  mattias
  listbox and comboboxes now set sort and selection at handle creation

  Revision 1.139  2003/08/19 12:23:23  mattias
  moved types from graphtype.pp back to graphics.pp

  Revision 1.138  2003/08/18 13:21:23  mattias
  renamed lazqueue to lazlinkedlist, patch from Jeroen

  Revision 1.137  2003/08/13 16:18:58  mattias
  started check compiler options

  Revision 1.136  2003/07/29 00:28:43  marc
  + Implemented GetCursorPos

  Revision 1.135  2003/07/06 17:53:34  mattias
  updated polish localization

  Revision 1.134  2003/07/02 15:56:15  mattias
  fixed win32 painting and started creating bitmaps from rawimages

  Revision 1.133  2003/07/02 10:02:51  mattias
  fixed TPaintStruct

  Revision 1.132  2003/06/23 09:42:09  mattias
  fixes for debugging lazarus

  Revision 1.131  2002/08/19 15:15:24  mattias
  implemented TPairSplitter

  Revision 1.130  2002/08/17 23:41:34  mattias
  many clipping fixes

  Revision 1.129  2003/06/03 10:29:22  mattias
  implemented updates between source marks and breakpoints

  Revision 1.128  2003/05/18 10:42:58  mattias
  implemented deleting empty submenus

  Revision 1.127  2003/04/29 19:00:43  mattias
  added package gtkopengl

  Revision 1.126  2003/04/29 13:35:39  mattias
  improved configure build lazarus dialog

  Revision 1.125  2003/04/08 00:09:03  mattias
  added LM_APPENDTEXT from hernan

  Revision 1.124  2003/04/03 17:42:13  mattias
  added exception handling for createpixmapindirect

  Revision 1.123  2003/03/17 13:00:35  mattias
  improved but not fixed transient windows

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
  MG: TNoteBook now starts with no Page and TCustomPage has no auto names

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
