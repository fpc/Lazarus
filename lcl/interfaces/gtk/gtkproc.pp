{
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
unit GTKProc;

{$mode objfpc}{$H+}

interface

{$IFDEF win32}
{$DEFINE NoGdkPixbufLib}
{$ELSE}
{off $DEFINE NoGdkPixbufLib}
{$ENDIF}

uses
  SysUtils, Classes,
  {$Ifndef Win32}
  X, XLib,//Font retrieval
  {$EndIf}
  InterfaceBase,
  {$IFDEF gtk2}
  glib2, gdk2pixbuf, gdk2, gtk2,
  {$ELSE}
  glib, gdk, gtk, {$Ifndef NoGdkPixbufLib}gdkpixbuf,{$EndIf}
  {$ENDIF}
  LMessages, Controls, Forms, VclGlobals,
  LCLStrConsts, LCLLinux, LCLType, gtkDef, DynHashArray, LazQueue, GraphType,
  GraphMath, Graphics, Buttons, Menus, GTKWinApiWindow, StdCtrls, ComCtrls,
  CListBox, KeyMap, Calendar, Arrow, Spin, CommCtrl, ExtCtrls, Dialogs,
  FileCtrl, LResources, Math, GTKGlobals;


procedure laz_gdk_gc_set_dashes(gc:PGdkGC; dash_offset:gint;
  dashlist:Pgint8; n:gint); cdecl; external gdkdll name 'gdk_gc_set_dashes';



(* GTKCallback.inc headers *)
procedure EventTrace(const TheMessage : string; data : pointer);
function gtkNoteBookCloseBtnClicked(Widget: PGtkWidget;
  Data: Pointer): GBoolean; cdecl;
function gtkRealizeCB(Widget: PGtkWidget; Data: Pointer): GBoolean; cdecl;
function gtkRealizeAfterCB(Widget: PGtkWidget; Data: Pointer): GBoolean; cdecl;
function gtkshowCB( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
function gtkHideCB( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
function gtkactivateCB(widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
function gtkchangedCB( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
function gtkchanged_editbox( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
function gtkdaychanged(Widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
function gtkDrawAfter(Widget: PGtkWidget; area: PGDKRectangle;
  data: gPointer) : GBoolean; cdecl;
function gtkExposeEventAfter(Widget: PGtkWidget; Event : PGDKEventExpose;
  Data: gPointer): GBoolean; cdecl;
function gtkfrmactivateAfter( widget: PGtkWidget; Event : PgdkEventFocus;
  data: gPointer) : GBoolean; cdecl;
function gtkfrmdeactivateAfter( widget: PGtkWidget; Event : PgdkEventFocus;
  data: gPointer) : GBoolean; cdecl;
function GTKMap(Widget: PGTKWidget; Data: gPointer): GBoolean; cdecl;
function GTKKeyUpDown(Widget: PGtkWidget; Event : pgdkeventkey;
  Data: gPointer) : GBoolean; cdecl;
function GTKFocusCB(widget: PGtkWidget; event:PGdkEventFocus; data: gPointer) : GBoolean; cdecl;
function GTKFocusCBAfter(widget: PGtkWidget; event:PGdkEventFocus; data: gPointer) : GBoolean; cdecl;
function GTKKillFocusCB(widget: PGtkWidget; event:PGdkEventFocus; data: gPointer) : GBoolean; cdecl;
function GTKKillFocusCBAfter(widget: PGtkWidget; event:PGdkEventFocus; data: gPointer) : GBoolean; cdecl;
function gtkdestroyCB(widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
function gtkdeleteCB( widget : PGtkWidget; event : PGdkEvent; data : gPointer) : GBoolean; cdecl;
function gtkresizeCB( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
function gtkMonthChanged(Widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
procedure DeliverMouseMoveMessage(Widget:PGTKWidget; Event: PGDKEventMotion;
  AWinControl: TWinControl);
function ControlGetsMouseMoveBefore(AControl: TControl): boolean;
function gtkMotionNotify(Widget:PGTKWidget; Event: PGDKEventMotion;
  Data: gPointer): GBoolean; cdecl;
function GTKMotionNotifyAfter(widget:PGTKWidget; event: PGDKEventMotion;
  data: gPointer): GBoolean; cdecl;
function ControlGetsMouseDownBefore(AControl: TControl): boolean;
procedure DeliverMouseDownMessage(widget: PGtkWidget; event : pgdkEventButton;
  AWinControl: TWinControl);
function gtkMouseBtnPress(widget: PGtkWidget; event : pgdkEventButton;
  data: gPointer) : GBoolean; cdecl;
function gtkMouseBtnPressAfter(widget: PGtkWidget; event : pgdkEventButton;
  data: gPointer) : GBoolean; cdecl;
function ControlGetsMouseUpBefore(AControl: TControl): boolean;
procedure DeliverMouseUpMessage(widget: PGtkWidget; event : pgdkEventButton;
  AWinControl: TWinControl);
function gtkMouseBtnRelease(widget: PGtkWidget; event : pgdkEventButton;
  data: gPointer) : GBoolean; cdecl;
function gtkMouseBtnReleaseAfter(widget: PGtkWidget; event : pgdkEventButton;
  data: gPointer) : GBoolean; cdecl;
function gtkclickedCB( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
function gtkOpenDialogRowSelectCB( widget : PGtkWidget; row : gint;
  column : gint; event : pgdkEventButton; data : gPointer ) : GBoolean; cdecl;
function gtkDialogOKclickedCB( widget: PGtkWidget;
  data: gPointer) : GBoolean; cdecl;
function gtkDialogCancelclickedCB(widget: PGtkWidget; data: gPointer): GBoolean;cdecl;
function gtkDialogHelpclickedCB(widget: PGtkWidget; data: gPointer): GBoolean;cdecl;
function gtkDialogApplyclickedCB(widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkDialogCloseQueryCB(widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
procedure UpdateDetailView(OpenDialog: TOpenDialog);
function GTKDialogKeyUpDownCB(Widget: PGtkWidget; Event : pgdkeventkey;
  Data: gPointer) : GBoolean; cdecl;
function GTKDialogRealizeCB(Widget: PGtkWidget; Data: Pointer): GBoolean; cdecl;
function GTKDialogFocusInCB(widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function GTKDialogSelectRowCB(widget: PGtkWidget; Row, Column: gInt;
  bevent: pgdkEventButton; data: gPointer): GBoolean; cdecl;
function GTKDialogMenuActivateCB(widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkDialogDestroyCB(widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkPressedCB( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
function gtkEnterCB(widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
function gtkLeaveCB(widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
function gtkMoveCursorCB(widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
function gtksize_allocateCB(widget: PGtkWidget; size :pGtkAllocation;
  data: gPointer) : GBoolean; cdecl;
function gtksize_allocate_client(widget: PGtkWidget; size :pGtkAllocation;
  data: gPointer) : GBoolean; cdecl;
function gtkswitchpage(widget: PGtkWidget; page: Pgtkwidget; pagenum : integer;
  data: gPointer) : GBoolean; cdecl;
function gtkconfigureevent( widget: PGtkWidget; event : PgdkEventConfigure;
  data: gPointer) : GBoolean; cdecl;
function gtkreleasedCB( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
function gtkInsertText( widget: PGtkWidget; char : pChar; NewTextLength : Integer; Position : pgint; data: gPointer) : GBoolean; cdecl;
function gtkDeleteText( widget: PGtkWidget; Startpos, EndPos : Integer; data: gPointer) : GBoolean; cdecl;
function gtkSetEditable( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
function gtkMoveWord( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
function gtkMovePage( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
function gtkMoveToRow( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
function gtkMoveToColumn( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
function gtkKillChar( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
function gtkKillWord( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
function gtkKillLine( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
function gtkCutToClip( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
function gtkCopyToClip( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
function gtkPasteFromClip( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
function gtkValueChanged(widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
function gtkTimerCB(Data: gPointer): gint; cdecl;
function gtkFocusInNotifyCB (widget : PGtkWidget; event : PGdkEvent;
  data : gpointer) : GBoolean; cdecl;
function gtkFocusOutNotifyCB (widget : PGtkWidget; event : PGdkEvent;
  data : gpointer) : GBoolean; cdecl;
function GTKHScrollCB(Adjustment: PGTKAdjustment; data: GPointer): GBoolean; cdecl;
function GTKVScrollCB(Adjustment: PGTKAdjustment;
  data: GPointer): GBoolean; cdecl;
function GTKKeySnooper(Widget: PGtkWidget; Event: PGdkEventKey;
  FuncData: gPointer): gInt; cdecl;
function gtkYearChanged(Widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
procedure ClipboardSelectionReceivedHandler(TargetWidget: PGtkWidget;
  SelectionData: PGtkSelectionData; TimeID: cardinal; Data: Pointer); cdecl;
procedure ClipboardSelectionRequestHandler(TargetWidget: PGtkWidget;
  SelectionData: PGtkSelectionData; Info: cardinal; TimeID: cardinal;
  Data: Pointer); cdecl;
function ClipboardSelectionLostOwnershipHandler(TargetWidget: PGtkWidget;
  EventSelection: PGdkEventSelection;  Data: Pointer): cardinal; cdecl;
Procedure GTKStyleChanged(Widget: PGtkWidget; previous_style : PGTKStyle; Data: Pointer); cdecl;

function DeliverPostMessage(const Target: Pointer; var TheMessage): GBoolean;
function DeliverMessage(const Target: Pointer; var AMessage): Integer;

(* gtkDragCallback.inc headers *)
Function edit_drag_data_received(widget : pgtkWidget;
			          Context : pGdkDragContext;
			          X : Integer;
			          Y : Integer;
			          seldata : pGtkSelectionData;
			          info : Integer;
			          time : Integer;
                                  data : pointer) : GBoolean; cdecl;
Function edit_source_drag_data_get(widget : pgtkWidget;
			          Context : pGdkDragContext;
			          Selection_data : pGtkSelectionData;
			          info : Integer;
			          time : Integer;
                                  data : pointer) : GBoolean; cdecl;
Function Edit_source_drag_data_delete (widget : pGtkWidget;
			                context : pGdkDragContext;
			                data : gpointer): gBoolean ; cdecl;

(* gtkListViewCallback.inc headers *)

function gtkLVHScroll(AList: PGTKCList; AData: gPointer): GBoolean; cdecl;
function gtkLVVScroll(AList: PGTKCList; AData: gPointer): GBoolean; cdecl;
function gtkLVAbortColumnResize(AList: PGTKCList; AData: gPointer): GBoolean; cdecl;
function gtkLVResizeColumn(AList: PGTKCList; AColumn, AWidth: Integer; AData: gPointer): GBoolean; cdecl;
function gtkLVClickColumn(AList: PGTKCList; AColumn: Integer; AData: gPointer): GBoolean; cdecl;
function gtkLVRowMove(AList: PGTKCList; AnOldIdx, ANewIdx: Integer; AData: gPointer): GBoolean; cdecl;
function gtkLVSelectRow(AList: PGTKCList; ARow, AColumn: Integer; AEvent: PGDKEventButton; AData: gPointer): GBoolean; cdecl;
function gtkLVUnSelectRow(AList: PGTKCList; ARow, AColumn: Integer; AEvent: PGDKEventButton; AData: gPointer): GBoolean; cdecl;
function gtkLVToggleFocusRow(AList: PGTKCList; AData: gPointer): GBoolean; cdecl;
function gtkLVSelectAll(AList: PGTKCList; AData: gPointer): GBoolean; cdecl;
function gtkLVUnSelectAll(AList: PGTKCList; AData: gPointer): GBoolean; cdecl;
function gtkLVEndSelection(AList: PGTKCList; AData: gPointer): GBoolean; cdecl;

(* gtkComboBoxCallback.inc headers *)

function gtkComboBoxShowCB(widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkComboBoxHideCB(widget: PGtkWidget; data: gPointer): GBoolean; cdecl;

procedure Set_RC_Name(Sender : TObject; AWidget: PGtkWidget);

procedure RaiseException(const Msg: string);
function CreatePChar(const s: string): PChar;
function ComparePChar(P1, P2: PChar): boolean;
function FindChar(c: char; p:PChar; Max: integer): integer;
function GtkWidgetIsA(Widget: PGtkWidget; AType: TGtkType): boolean;
function WidgetIsDestroyingHandle(Widget: PGtkWidget): boolean;
procedure SetWidgetIsDestroyingHandle(Widget: PGtkWidget);
function ComponentIsDestroyingHandle(AWinControl: TWinControl): boolean;
function LockOnChange(GtkObject: PGtkObject; LockOffset: integer): integer;
procedure SetComboBoxText(ComboWidget: PGtkCombo; NewText: PChar);
function GetComboBoxItemIndex(ComboBox: TComboBox): integer;
procedure SetComboBoxItemIndex(ComboBox: TComboBox; Index: integer);
function GtkPaintMessageToPaintMessage(GtkPaintMsg: TLMGtkPaint): TLMPaint;
function NewGDIRawImage(const AWidth, AHeight: Integer; const ADepth: Byte): PGDIRawImage;
function AllocGDKColor(const AColor: LongInt): TGDKColor;
function CopyDCData(DestinationDC, SourceDC: TDeviceContext): Boolean;
Function RegionType(RGN : PGDKRegion) : Longint;
Procedure SelectGDIRegion(const DC: HDC);
Procedure FreeGDIColor(var GDIColor : TGDIColor);
Procedure AllocGDIColor(DC : hDC; var GDIColor : TGDIColor);
procedure SetGDIColorRef(var GDIColor : TGDIColor; NewColorRef: TColorRef);
Procedure EnsureGCColor(DC: hDC; ColorType: TDevContextsColorType;
  IsSolidBrush: Boolean; AsBackground: Boolean);

function GetIndexAsKey(p: pointer): pointer;
function GetRGBAsKey(p: pointer): pointer;

function PaletteIndexExists(Pal : PGDIObject; I : longint): Boolean;
function PaletteRGBExists(Pal : PGDIObject; RGB : longint): Boolean;
function PaletteAddIndex(Pal : PGDIObject; I, RGB : Longint): Boolean;
function PaletteDeleteIndex(Pal : PGDIObject; I : Longint): Boolean;
function PaletteIndexToRGB(Pal : PGDIObject; I : longint): longint;
function PaletteRGBToIndex(Pal : PGDIObject; RGB : longint): longint;
Procedure InitializePalette(Pal : PGDIObject; Entries : PPALETTEENTRY; RGBCount : Longint);

function GTKEventState2ShiftState(KeyState: Word): TShiftState;
function KeyToListCode(KeyCode, VirtKeyCode: Word; Extended: boolean): integer;
procedure GetGTKKeyInfo(const Event: PGDKEventKey; var KeyCode,VirtualKey: Word;
  var SysKey, Extended, Toggle: Boolean);
procedure StoreCommonDialogSetup(ADialog: TCommonDialog);
procedure DestroyCommonDialogAddOns(ADialog: TCommonDialog);
function ObjectToGTKObject(const AObject: TObject): PGtkObject;
function GetMainWidget(const Widget: Pointer): Pointer;
procedure SetMainWidget(const ParentWidget, ChildWidget: Pointer);
function GetFixedWidget(const Widget: Pointer): Pointer;
procedure SetFixedWidget(const ParentWidget, FixedWidget: Pointer);
Procedure FixedMoveControl(Parent, Child : PGTKWIdget; Left, Top : Longint);
Procedure FixedPutControl(Parent, Child : PGTKWIdget; Left, Top : Longint);
Function GetControlWindow(Control: Pointer) : PGDKWindow;
function GetDCOffset(DC: TDeviceContext): TPoint;
function CreateWidgetInfo(const Widget: Pointer): PWinWidgetInfo;
function GetWidgetInfo(const Widget: Pointer; const Create: Boolean): PWinWidgetInfo;
procedure FreeWinWidgetInfo(Widget: Pointer);
procedure DestroyWidget(Widget: PGtkWidget);
function GetGtkNoteBookDummyPage(ANoteBookWidget: PGtkNoteBook): PGtkWidget;
procedure SetGtkNoteBookDummyPage(ANoteBookWidget: PGtkNoteBook;
  DummyWidget: PGtkWidget);
procedure UpdateNoteBookClientWidget(ANoteBook: TObject);
procedure SetLCLObject(const Widget: Pointer; const AnObject: TObject);
function GetLCLObject(const Widget: Pointer): TObject;
function GetParentLCLObject(Widget: PGtkWidget): TObject;
procedure SetHiddenLCLObject(const Widget: Pointer; const AnObject: TObject);
function GetHiddenLCLObject(const Widget: Pointer): TObject;
function GetWidgetOrigin(TheWidget: PGtkWidget): TPoint;
function GetWidgetClientOrigin(TheWidget: PGtkWidget): TPoint;
function TranslateGdkPointToClientArea(SourceWindow: PGdkWindow;
  SourcePos: TPoint;  DestinationWidget: PGtkWidget): TPoint;
procedure ReleaseMouseCapture(OnlyIfCapturedByLCL: boolean);
procedure UpdateMouseCaptureControl;
procedure SetCursor(AWinControl : TWinControl; Data: Pointer);
function GtkWindowIsModal(GtkWindow: PGtkWindow): boolean;

type
  TConnectSignalFlag = (
    csfAfter,            // connect after signal
    csfConnectRealize,   // auto connect realize handler
    csfUpdateSignalMask, // extend signal mask for gdkwindow
    csfDesignOnly        // mark signal as design only
    );
  TConnectSignalFlags = set of TConnectSignalFlag;

  TDesignSignalType = (
    dstUnknown,
    dstMousePress,
    dstMouseMotion,
    dstMouseRelease,
    dstDrawAfter,
    dstExposeAfter
    );
  TDesignSignalTypes = set of TDesignSignalType;

  TDesignSignalMask = longint;

const
  DesignSignalNames: array[TDesignSignalType] of PChar = (
    '',
    'button-press-event',
    'motion-notify-event',
    'button-release-event',
    'draw',
    'expose-event'
    );

  DesignSignalAfter: array[TDesignSignalType] of boolean =
    (false,false,false,false,true,true);

  DesignSignalFuncs: array[TDesignSignalType] of Pointer = (
    nil,
    @gtkMouseBtnPress,
    @gtkMotionNotify,
    @gtkMouseBtnRelease,
    @gtkDrawAfter,
    @gtkExposeEventAfter
    );

var
  DesignSignalMasks: array[TDesignSignalType] of TDesignSignalMask;
  

procedure InitDesignSignalMasks;
function DesignSignalNameToType(Name: PChar; After: boolean): TDesignSignalType;
function GetDesignSignalMask(Widget: PGtkWidget): TDesignSignalMask;
procedure SetDesignSignalMask(Widget: PGtkWidget; NewMask: TDesignSignalMask);
function GetDesignOnlySignalFlag(Widget: PGtkWidget;
  DesignSignalType: TDesignSignalType): boolean;
procedure ConnectSignal(const AnObject:gtk_Object; const ASignal: PChar;
  const ACallBackProc: Pointer; LCLComponent: TComponent;
  const ReqSignalMask: TGdkEventMask; SFlags: TConnectSignalFlags);
procedure ConnectSignal(const AnObject:gtk_Object; const ASignal: PChar;
  const ACallBackProc: Pointer; LCLComponent: TComponent;
  const ReqSignalMask: TGdkEventMask);
procedure ConnectSignalAfter(const AnObject:gtk_Object; const ASignal: PChar;
  const ACallBackProc: Pointer; LCLComponent: TComponent;
  const ReqSignalMask: TGdkEventMask);
procedure ConnectSignal(const AnObject:gtk_Object; const ASignal: PChar;
  const ACallBackProc: Pointer; LCLComponent: TComponent);
procedure ConnectSignalAfter(const AnObject:gtk_Object; const ASignal: PChar;
  const ACallBackProc: Pointer; LCLComponent: TComponent);
procedure ConnectInternalWidgetsSignals(AWidget: PGtkWidget;
  AWinControl: TWinControl);
function GetAccelGroup(const Widget: PGtkWidget;
  CreateIfNotExists: boolean): PGTKAccelGroup;
procedure SetAccelGroup(const Widget: PGtkWidget;
  const AnAccelGroup: PGTKAccelGroup);
procedure FreeAccelGroup(const Widget: PGtkWidget);
procedure RegroupAccelerator(Widget: PGtkWidget);
procedure ClearAccelKey(Widget: PGtkWidget);
procedure Accelerate(Component: TComponent; const Widget : PGtkWidget;
  const Key: guint; Mods: TGdkModifierType; const Signal : string);
procedure Accelerate(Component: TComponent; const Widget : PGtkWidget;
  const Msg : TLMShortCut; const Signal : string);
procedure ShareWindowAccelGroups(AWindow: PGtkWidget);
procedure UnshareWindowAccelGroups(AWindow: PGtkWidget);
procedure GetGdkPixmapFromGraphic(LCLGraphic: TGraphic;
  var IconImg, IconMask: PGdkPixmap; var Width, Height: integer);
procedure GetGdkPixmapFromMenuItem(LCLMenuItem: TMenuItem;
  var IconImg, IconMask: PGdkPixmap; var Width, Height: integer);
function MENU_ITEM_CLASS(widget: PGtkWidget): PGtkMenuItemClass;
function CHECK_MENU_ITEM_CLASS(widget: PGtkWidget): PGtkCheckMenuItemClass;
function GetRadioMenuItemGroup(LCLMenuItem: TMenuItem): PGSList;
function GetRadioMenuItemGroup(MenuItem: PGtkRadioMenuItem): PGSList;
procedure UpdateRadioGroupChecks(RadioGroup: PGSList);
procedure DrawMenuItemIcon(MenuItem: PGtkCheckMenuItem;
  Area: PGdkRectangle); cdecl;
procedure MenuSizeRequest(widget:PGtkWidget; requisition:PGtkRequisition); cdecl;
procedure SetMenuItemLabelText(LCLMenuItem: TMenuItem;
  MenuItemWidget: PGtkWidget);
procedure CreateInnerMenuItem(LCLMenuItem: TMenuItem;
  MenuItemWidget: PGtkWidget);
function CreateMenuItem(LCLMenuItem: TMenuItem): Pointer;
procedure SaveSizeNotification(Widget: PGtkWidget);
procedure SaveClientSizeNotification(FixWidget: PGtkWidget);
function CreateTopologicalSortedWidgets(HashArray: TDynHashArray): TList;
Procedure ReportNotObsolete(const Texts : String);
function TGDKColorToTColor(value : TGDKColor) : TColor;
function TColortoTGDKColor(value : TColor) : TGDKColor;
procedure UpdateSysColorMap(Widget: PGtkWidget);
function WaitForClipbrdAnswerDummyTimer(Client: Pointer): gint; cdecl;
function WaitForClipboardAnswer(c: PClipboardEventData): boolean;
function RequestSelectionData(ClipboardWidget: PGtkWidget;
  ClipboardType: TClipboardType;  FormatID: cardinal): TGtkSelectionData;
procedure FreeClipboardTargetEntries(ClipboardType: TClipboardType);

Function CreateFormContents(var FormWidget : Pointer) : Pointer;

function IndexOfStyle(const WName : String): integer;
Procedure ReleaseStyle(const WName : String);
function GetStyle(const WName : String) : PGTKStyle;
Function GetStyleWidget(WName : String) : PGTKWidget;
function GetDefaultFont : PGDKFont;
Function GetSysGCValues(Color : TColorRef) : TGDKGCValues;

Function DeleteAmpersands(var Str : String) : Longint;

function Ampersands2Underscore(Src: PChar) : PChar;
function RemoveAmpersands(Src: PChar; LineLength : Longint) : PChar;

Procedure GetTextExtentIgnoringAmpersands(Font : PGDKFont; Str : PChar;
  LineLength : Longint; lbearing, rbearing, width, ascent, descent : Pgint);
function FontIsDoubleByteCharsFont(TheFont: PGdkFont): boolean;

Function GDKPixel2GDIRGB(Pixel : Longint; Visual : PGDKVisual;
  Colormap : PGDKColormap) : TGDIRGB;

Function GetWindowDecorations(AForm : TCustomForm) : Longint;
Function GetWindowFunction(AForm : TCustomForm) : Longint;

Procedure FillScreenFonts(ScreenFonts : TStrings);

function GetGDKMouseCursor(Cursor: TCursor): PGdkCursor;
Procedure FreeGDKCursors;

var
  X11Display : Pointer;
  
implementation


procedure Set_RC_Name(Sender : TObject; AWidget: PGtkWidget);
var RCName: string;
  AComponent: TComponent;
begin
  if (AWidget=nil) or (not (Sender is TComponent)) then exit;

  // check if a unique name can be created
  AComponent:=TComponent(Sender);
  while (AComponent<>nil) and (AComponent.Name<>'') do begin
    AComponent:=AComponent.Owner;
  end;
  if (AComponent=nil) or (AComponent=TComponent(Application)) then begin
    // create unique name
    AComponent:=TComponent(Sender);
    RCName:=AComponent.Name;
    while (AComponent<>nil) do begin
      AComponent:=TComponent(AComponent.Owner);
      if (AComponent<>nil) and (AComponent.Name<>'') then
        RCName:=AComponent.Name+'_'+RCName;
    end;
    gtk_widget_set_name(AWidget,PChar(RCName));
    gtk_widget_set_rc_style(AWidget);
  end;
  if (Sender is TCustomForm)
  and ((Application.MainForm=TCustomForm(Sender))
    or (Application.MainForm=nil))
  then
    UpdateSysColorMap(AWidget);
end;

{$I gtkproc.inc}
{$I gtkcallback.inc}

initialization
  X11Display := nil;
  
Finalization
  {$IfNdef Win32}
  If X11Display <> nil then
    XCloseDisplay(X11Display);
  {$EndIf}
  X11Display := nil;

end.

