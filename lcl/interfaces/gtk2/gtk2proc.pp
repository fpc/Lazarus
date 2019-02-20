{
                  ----------------------------------
                  gtk2proc.pp  - gtk 2 interface procs
                  ----------------------------------

 This unit contains procedures/functions needed for the gtk 2 <-> LCL interface
}
{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit Gtk2Proc;

{$mode objfpc}{$H+}

interface

{$I gtk2defines.inc}

uses
  {$IFDEF windows}
    // use windows unit first,
    // if not, Rect and Point are taken from the windows unit instead of classes.
    Windows, // needed for keyboard handling
  {$endif}
  {$IFDEF Unix}
    unix,
  {$ENDIF}
  SysUtils, Classes,
  {$IFDEF HasX}
    XAtom, // clipboard
    X, XLib, // Font retrieval and Keyboard handling
  {$ENDIF}
  InterfaceBase,
  // gtk2
    glib2, gdk2pixbuf, gdk2, gtk2, Pango,
    {$ifdef HasGdk2X}
    gdk2x,
    {$endif}
  // Other units
  Math, // Math after gtk to get the correct Float type
  Types,
  // LCL
  LMessages, LCLProc, LCLIntf, LCLType, GraphType, Graphics,
  LResources, Controls, Forms, Buttons, Menus, StdCtrls, ComCtrls, ExtCtrls,
  Dialogs, ExtDlgs, ImgList, LCLMessageGlue,
  // LazUtils
  Masks, FileUtil, LazFileUtils, LazLoggerBase, LazUTF8, DynHashArray,
  // Gtk2
  Gtk2FontCache, Gtk2Globals, Gtk2Def, Gtk2Extra, {%H-}Gtk2Debug;

const
  GtkListItemGtkListTag = 'GtkList';
  GtkListItemLCLListTag = 'LCLList';


type
  PPWaitHandleEventHandler = ^PWaitHandleEventHandler;
  PWaitHandleEventHandler = ^TWaitHandleEventHandler;
  TWaitHandleEventHandler = record
    Handle: THandle;
    GIOChannel: pgiochannel;
    GSourceID: guint;
    UserData: PtrInt;
    OnEvent: TWaitHandleEvent;
    PrevHandler: PWaitHandleEventHandler;
    NextHandler: PWaitHandleEventHandler;
  end;

{$ifdef UNIX}
  PPChildSignalEventHandler = ^PChildSignalEventHandler;
  PChildSignalEventHandler = ^TChildSignalEventHandler;
  TChildSignalEventHandler = record
    PID: TPid;
    UserData: PtrInt;
    OnEvent: TChildExitEvent;
    PrevHandler: PChildSignalEventHandler;
    NextHandler: PChildSignalEventHandler;
  end;

{$endif}

var
  GTKAPIWidget_Type: GType = 0;

// GTKCallback.inc headers
procedure EventTrace(const {%H-}TheMessage: string; {%H-}data: pointer);
function gtkNoteBookCloseBtnClicked({%H-}Widget: PGtkWidget;
  Data: Pointer): GBoolean; cdecl;
function gtkRealizeCB(Widget: PGtkWidget; Data: Pointer): GBoolean; cdecl;
function gtkRealizeAfterCB(Widget: PGtkWidget; Data: Pointer): GBoolean; cdecl;
function gtkshowCB( {%H-}widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkHideCB( {%H-}widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkactivateCB(widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkchangedCB( widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
procedure gtkchanged_editbox_delete_text(Widget: PGtkWidget;
  {%H-}AStartPos, {%H-}AEndPos: gint; {%H-}data: gPointer); cdecl;
procedure gtkchanged_editbox_insert_text(Widget: PGtkWidget; {%H-}ANewText: gChar;
  {%H-}ANewTextLength: gint; {%H-}APosition: pgint; {%H-}data: gPointer); cdecl;
function gtkchanged_editbox( widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkchanged_editbox_delete(widget: PGtkWidget;
  {%H-}AType: TGtkDeleteType; {%H-}APos: gint; {%H-}data: gPointer): GBoolean; cdecl;
function gtkdaychanged(Widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtktoggledCB( widget: PGtkWidget; data: gPointer): GBoolean; cdecl;

function gtkExposeEvent(Widget: PGtkWidget; Event: PGDKEventExpose;
  Data: gPointer): GBoolean; cdecl;
function gtkExposeEventAfter(Widget: PGtkWidget; Event: PGDKEventExpose;
  Data: gPointer): GBoolean; cdecl;
function gtkfrmactivateAfter( widget: PGtkWidget; {%H-}Event: PgdkEventFocus;
  data: gPointer): GBoolean; cdecl;
function gtkfrmdeactivateAfter( widget: PGtkWidget; {%H-}Event: PgdkEventFocus;
  data: gPointer): GBoolean; cdecl;

function GTKKeyPress(Widget: PGtkWidget; Event: pgdkeventkey;
  Data: gPointer): GBoolean; cdecl;
function GTKKeyPressAfter(Widget: PGtkWidget; Event: pgdkeventkey;
  Data: gPointer): GBoolean; cdecl;
function GTKKeyRelease(Widget: PGtkWidget; Event: pgdkeventkey;
  Data: gPointer): GBoolean; cdecl;
function GTKKeyReleaseAfter(Widget: PGtkWidget; Event: pgdkeventkey;
  Data: gPointer): GBoolean; cdecl;

function GTKFocusCB(widget: PGtkWidget; {%H-}event:PGdkEventFocus;
                    data: gPointer): GBoolean; cdecl;
function GTKKillFocusCB({%H-}widget: PGtkWidget; {%H-}event:PGdkEventFocus;
                        {%H-}data: gPointer): GBoolean; cdecl;
function GTKKillFocusCBAfter(widget: PGtkWidget; {%H-}event:PGdkEventFocus;
                             data: gPointer): GBoolean; cdecl;
function gtkdestroyCB(widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
procedure DestroyWindowFromPointCB(Widget: PGtkWidget; {%H-}data: gPointer); cdecl;
function gtkdeleteCB({%H-}widget: PGtkWidget; {%H-}event: PGdkEvent;
                     data: gPointer): GBoolean; cdecl;
function gtkresizeCB({%H-}widget: PGtkWidget; {%H-}data: gPointer): GBoolean; cdecl;
function gtkMonthChanged({%H-}Widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
procedure DeliverMouseMoveMessage({%H-}Widget:PGTKWidget; Event: PGDKEventMotion;
                                  AWinControl: TWinControl);

function gtkMotionNotify(Widget:PGTKWidget; Event: PGDKEventMotion;
                         Data: gPointer): GBoolean; cdecl;
function GTKMotionNotifyAfter(widget:PGTKWidget; event: PGDKEventMotion;
                              data: gPointer): GBoolean; cdecl;

function DeliverMouseDownMessage({%H-}widget: PGtkWidget; event: pgdkEventButton;
                                  AWinControl: TWinControl): PtrInt;

function gtk2ScrollBarMouseBtnPress({%H-}widget: PGtkWidget; {%H-}event: pgdkEventButton;
  {%H-}data: gPointer): GBoolean; cdecl;

function gtk2ScrollBarMouseBtnRelease({%H-}widget: PGtkWidget; {%H-}event: pgdkEventButton;
  {%H-}data: gPointer): GBoolean; cdecl;


function gtkMouseBtnPress(widget: PGtkWidget; event: pgdkEventButton;
                          data: gPointer): GBoolean; cdecl;
function gtkMouseBtnPressAfter(widget: PGtkWidget; event: pgdkEventButton;
                               data: gPointer): GBoolean; cdecl;
function ControlGetsMouseUpBefore({%H-}AControl: TControl): boolean;
function DeliverMouseUpMessage(widget: PGtkWidget; event: pgdkEventButton;
                                AWinControl: TWinControl): boolean;
function gtkMouseBtnRelease(widget: PGtkWidget; event: pgdkEventButton;
                            data: gPointer): GBoolean; cdecl;
function gtkMouseBtnReleaseAfter(widget: PGtkWidget; event: pgdkEventButton;
                                 data: gPointer): GBoolean; cdecl;
function gtkclickedCB( widget: PGtkWidget; data: gPointer): GBoolean; cdecl;

function gtkEnterCB(widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkLeaveCB(widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtksize_allocateCB(widget: PGtkWidget; {%H-}size :pGtkAllocation;
  data: gPointer): GBoolean; cdecl;
function gtksize_allocate_client({%H-}widget: PGtkWidget; {%H-}size :pGtkAllocation;
  data: gPointer): GBoolean; cdecl;
function gtkconfigureevent( widget: PGtkWidget; event: PgdkEventConfigure;
  data: gPointer): GBoolean; cdecl;
function gtkInsertText(widget: PGtkWidget; char: pChar; NewTextLength:
                     Integer; Position: pgint; data: gPointer): GBoolean; cdecl;
function gtkSetEditable( {%H-}widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkMoveWord( {%H-}widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkMovePage( {%H-}widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkMoveToRow( {%H-}widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkMoveToColumn( {%H-}widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkKillChar( {%H-}widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkKillWord( {%H-}widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkKillLine( {%H-}widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkCutToClip( {%H-}widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkCopyToClip( {%H-}widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkPasteFromClip( {%H-}widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkValueChanged({%H-}widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkTimerCB(Data: gPointer): gBoolean; cdecl;
function gtkFocusInNotifyCB (widget: PGtkWidget; {%H-}event: PGdkEvent;
  data: gpointer): GBoolean; cdecl;
function gtkFocusOutNotifyCB (widget: PGtkWidget; {%H-}event: PGdkEvent;
  data: gpointer): GBoolean; cdecl;

// PGtkAdjustment cb
function GTKHScrollCB(Adjustment: PGTKAdjustment; data: GPointer): GBoolean; cdecl;
function GTKVScrollCB(Adjustment: PGTKAdjustment;
  data: GPointer): GBoolean; cdecl;

// PGtkRange cb
function Gtk2RangeScrollCB(ARange: PGtkRange; AScrollType: TGtkScrollType;
  AValue: gdouble; AWidgetInfo: PWidgetInfo): gboolean; cdecl;
function Gtk2RangeScrollPressCB(Widget: PGtkWidget;
  {%H-}Event: PGdkEventButton; {%H-}Data: gPointer): gboolean; cdecl;
function Gtk2RangeScrollReleaseCB(Widget: PGtkWidget;
  {%H-}Event: PGdkEventButton; {%H-}Data: gPointer): gboolean; cdecl;
function Gtk2RangeUbuntuScrollCB(Adjustment: PGTKAdjustment; data: GPointer): GBoolean; cdecl;

procedure Gtk2RangeValueChanged(ARange: PGTKRange; data: GPointer); cdecl;

function Gtk2ScrolledWindowScrollCB(AScrollWindow: PGtkScrolledWindow;
  AEvent: PGdkEventScroll; AWidgetInfo: PWidgetInfo): gboolean; cdecl;
function GTKCheckMenuToggeledCB(AMenuItem: PGTKCheckMenuItem;
                                AData: gPointer): GBoolean; cdecl;
function GTKKeySnooper({%H-}Widget: PGtkWidget; Event: PGdkEventKey;
  FuncData: gPointer): gInt; cdecl;
function gtkYearChanged({%H-}Widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
procedure GtkDragDataReceived({%H-}widget:PGtkWidget; context:PGdkDragContext;
  {%H-}x:gint; {%H-}y:gint; selection_data:PGtkSelectionData; {%H-}info:guint; time:guint; Data: gPointer);cdecl;


// clipboard
procedure ClipboardSelectionReceivedHandler({%H-}TargetWidget: PGtkWidget;
  SelectionData: PGtkSelectionData; TimeID: guint32; {%H-}Data: Pointer); cdecl;
procedure ClipboardSelectionRequestHandler({%H-}TargetWidget: PGtkWidget;
  SelectionData: PGtkSelectionData; {%H-}Info: cardinal; {%H-}TimeID: cardinal;
  {%H-}Data: Pointer); cdecl;
function ClipboardSelectionLostOwnershipHandler({%H-}TargetWidget: PGtkWidget;
  EventSelection: PGdkEventSelection;  {%H-}Data: Pointer): cardinal; cdecl;
{$IFDEF EventTrace}
procedure GTKStyleChanged(Widget: PGtkWidget; previous_style :
  PGTKStyle; Data: Pointer); cdecl;
procedure GTKStyleChangedAfter(Widget: PGtkWidget; previous_style : PGTKStyle;
  Data: Pointer); cdecl;
{$ENDIF}
function gtkListBoxSelectionChangedAfter(widget: PGtkWidget;
                                      data: gPointer): GBoolean; cdecl;

// drag callbacks
function edit_drag_data_received({%H-}widget: pgtkWidget;
             Context: pGdkDragContext;
             {%H-}X: Integer;
             {%H-}Y: Integer;
             seldata: pGtkSelectionData;
             {%H-}info: Integer;
             time: Integer;
                                  data: pointer): GBoolean; cdecl;
function edit_source_drag_data_get({%H-}widget: pgtkWidget;
             {%H-}Context: pGdkDragContext;
             Selection_data: pGtkSelectionData;
             info: Integer;
             {%H-}time: Integer;
                                  data: pointer): GBoolean; cdecl;
function Edit_source_drag_data_delete ({%H-}widget: pGtkWidget;
                   {%H-}context: pGdkDragContext;
                   {%H-}data: gpointer): gBoolean ; cdecl;

// combobox callbacks
function gtkComboBoxShowAfter({%H-}widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkComboBoxHideAfter({%H-}widget: PGtkWidget; data: gPointer): GBoolean; cdecl;

// notebook callbacks
function PageIconWidgetExposeAfter(Widget: PGtkWidget; Event: PGDKEventExpose;
  Data: gPointer): GBoolean; cdecl;

// callbacks for menu items
procedure DrawMenuItemIcon(MenuItem: PGtkCheckMenuItem; Area: PGdkRectangle); cdecl;
procedure MenuSizeRequest(widget:PGtkWidget; requisition:PGtkRequisition); cdecl;

//==============================================================================
type
  TDestroyConnectedWidgetCB = procedure(Widget: PGtkWidget;
                                      CheckIfDestroying: boolean) of object;
var
  DestroyConnectedWidgetCB: TDestroyConnectedWidgetCB; // set by the TGtkWidgetSet

//==============================================================================
// functions

function RectFromGdkRect(const AGdkRect: TGdkRectangle): TRect;
function GdkRectFromRect(const R: TRect): TGdkRectangle;
function AlignToGtkAlign(Align: TAlignment): gfloat;
function GtkScrollTypeToScrollCode(ScrollType: TGtkScrollType): LongWord;
function Gtk2TranslateScrollStyle(const SS:TScrollStyle):TPoint;

// debugging
function GtkWidgetIsA(Widget: PGtkWidget; AType: TGtkType): boolean;
function GetWidgetClassName(Widget: PGtkWidget): string;
function GetWidgetDebugReport(Widget: PGtkWidget): string;
function GetWindowDebugReport(AWindow: PGDKWindow): string;
function GetStyleDebugReport(AStyle: PGTKStyle): string;
function GetRCStyleDebugReport(AStyle: PGtkRcStyle): string;
function GetPangoDescriptionReport(Desc: PPangoFontDescription): string;
function WidgetFlagsToString(Widget: PGtkWidget): string;
function GdkColorToStr(Color: PGDKColor): string;
function GetWidgetStyleReport(Widget: PGtkWidget): string;
procedure BeginGDKErrorTrap;
procedure EndGDKErrorTrap;
function dbgGRect(const ARect: PGDKRectangle): string; overload;


// gtk resources
procedure Set_RC_Name(Sender: TObject; AWidget: PGtkWidget);

// messages
function DeliverPostMessage(const Target: Pointer; var TheMessage): GBoolean;
function DeliverMessage(const Target: Pointer; var AMessage): PtrInt;

// PChar
function CreatePChar(const s: string): PChar;
function ComparePChar(P1, P2: PChar): boolean;
function FindChar(c: char; p:PChar; Max: integer): integer;
function FindLineLen(p:PChar; Max: integer): integer;

// flags
function WidgetIsDestroyingHandle(Widget: PGtkWidget): boolean;
procedure SetWidgetIsDestroyingHandle(Widget: PGtkWidget);
function ComponentIsDestroyingHandle(AWinControl: TWinControl): boolean;
function LockOnChange(GtkObject: PGtkObject; LockOffset: integer): integer;
procedure InvalidateLastWFPResult(AControl: TWinControl; const ABounds: TRect);

// glib
procedure MoveGListLinkBehind(First, Item, After: PGList);
procedure MoveGListLink(First: PGList; FromIndex, ToIndex: integer);

// properties
function ObjectToGTKObject(const AnObject: TObject): PGtkObject;
function GetMainWidget(const Widget: Pointer): Pointer;
procedure SetMainWidget(const ParentWidget, ChildWidget: Pointer);
function GetFixedWidget(const Widget: Pointer): Pointer;
procedure SetFixedWidget(const ParentWidget, FixedWidget: Pointer);
function GetControlWindow(Widget: Pointer): PGDKWindow;

function CreateWidgetInfo(const AWidget: Pointer): PWidgetInfo;
function CreateWidgetInfo(const AWidget: Pointer; const AObject: TObject;
                          const AParams: TCreateParams): PWidgetInfo;
function GetWidgetInfo(const AWidget: Pointer): PWidgetInfo;
function GetWidgetInfo(const AWidget: Pointer; const ACreate: Boolean): PWidgetInfo;
procedure FreeWidgetInfo(AWidget: Pointer);

procedure DestroyWidget(Widget: PGtkWidget);
procedure SetLCLObject(const Widget: Pointer; const AnObject: TObject);
function GetLCLObject(const Widget: Pointer): TObject;
function GetNearestLCLObject(Widget: PGtkWidget): TObject;
function GetWinControlWidget(Child: PGtkWidget): PGtkWidget;
function GetWinControlFixedWidget(Child: PGtkWidget): PGtkWidget;
function FindFixedChildListItem(ParentFixed: PGtkFixed; Child: PGtkWidget): PGList;
function FindFixedLastChildListItem(ParentFixed: PGtkFixed): PGList;
function GetFixedChildListWidget(Item: PGList): PGtkWidget;

// fixed widgets
function CreateFixedClientWidget(WithWindow: Boolean = true): PGTKWidget;
procedure FixedMoveControl(Parent, Child: PGTKWidget; Left, Top: Longint);
procedure FixedPutControl(Parent, Child: PGTKWidget; Left, Top: Longint);

// forms
procedure SetFormShowInTaskbar(AForm: TCustomForm;
                               const AValue: TShowInTaskbar);
procedure SetGtkWindowShowInTaskbar(AGtkWindow: PGtkWindow; Value: boolean);
procedure SetWindowFullScreen(AForm: TCustomForm; const AValue: Boolean);
procedure GrabKeyBoardToForm(AForm: TCustomForm);
procedure ReleaseKeyBoardFromForm({%H-}AForm: TCustomForm);
procedure GrabMouseToForm(AForm: TCustomForm);
procedure ReleaseMouseFromForm({%H-}AForm: TCustomForm);
procedure GtkWindowShowModal(AForm: TCustomForm; GtkWindow: PGtkWindow);

// label
procedure SetLabelAlignment(LabelWidget: PGtkLabel;
  const NewAlignment: TAlignment);

// paint messages
function DoDeliverPaintMessage(const Target: TObject; var PaintMsg: TLMPaint): PtrInt;
function GtkPaintMessageToPaintMessage(var GtkPaintMsg: TLMGtkPaint;
  FreeGtkPaintMsg: boolean): TLMPaint;
procedure FinalizePaintMessage(Msg: PLMessage);
procedure FinalizePaintTagMsg(Msg: PMsg);

// region
function RegionType(RGN: PGDKRegion): Longint;
function CreateRectGDKRegion(const ARect: TRect): PGDKRegion;
function GDKRegionAsString(RGN: PGDKRegion): string;

// color
procedure FreeGDIColor(GDIColor: PGDIColor);
procedure AllocGDIColor({%H-}DC: hDC; GDIColor: PGDIColor);
procedure BuildColorRefFromGDKColor(var GDIColor: TGDIColor);
procedure SetGDIColorRef(var GDIColor: TGDIColor; NewColorRef: TColorRef);
procedure EnsureGCColor(DC: hDC; ColorType: TDevContextsColorType;
  IsSolidBrush, AsBackground: Boolean);
procedure CopyGDIColor(var SourceGDIColor, DestGDIColor: TGDIColor);
function AllocGDKColor(const AColor: TColorRef): TGDKColor;
function TGDKColorToTColor(const value: TGDKColor): TColor;
function TColortoTGDKColor(const value: TColor): TGDKColor;
procedure UpdateSysColorMap(Widget: PGtkWidget; Lgs: TLazGtkStyle);
function IsBackgroundColor(Color: TColor): boolean;

procedure RealizeGDKColor(ColorMap: PGdkColormap; Color: PGDKColor);
procedure RealizeGtkStyleColor(Style: PGTKStyle; Color: PGDKColor);
function GetSysGCValues(Color: TColorRef; {%H-}ThemeWidget: PGtkWidget): TGDKGCValues;

function GDKPixel2GDIRGB(Pixel: Longint; Visual: PGDKVisual;
  Colormap: PGDKColormap): TGDIRGB;

function CompareGDIColor(const Color1, Color2: TGDIColor): boolean;
function CompareGDIFill(const Fill1, Fill2: TGdkFill): boolean;
function CompareGDIBrushes(Brush1, Brush2: PGdiObject): boolean;

// palette
function PaletteIndexExists(Pal: PGDIObject; I: longint): Boolean;
function PaletteRGBExists(Pal: PGDIObject; RGB: longint): Boolean;
function PaletteAddIndex(Pal: PGDIObject; I, RGB: Longint): Boolean;
function PaletteDeleteIndex(Pal: PGDIObject; I: Longint): Boolean;
function PaletteIndexToRGB(Pal: PGDIObject; I: longint): longint;
function PaletteRGBToIndex(Pal: PGDIObject; RGB: longint): longint;
procedure InitializePalette(const Pal: PGDIObject; const Entries: PPaletteEntry;
                            const RGBCount: Longint);
function GetIndexAsKey(p: pointer): pointer;
function GetRGBAsKey(p: pointer): pointer;


// Keyboard functions
type
  TVKeyUTF8Char = array[0..7] of Char;
  TVKeyInfo = record
    KeyCode: array[Boolean] of Byte; // false is primary keycode, true the keycode of the other key when 2 keys exist (like CTRL or extended key)
    KeySym: array[0..7] of Integer;
    KeyChar: array[0..3] of TVKeyUTF8Char;
  end;

const
  GdkKeymap: PGdkKeymap = nil;
  GdkKeyMapChangedID: gulong = 0;
procedure InitKeyboardTables;
procedure DoneKeyboardTables;
procedure DisconnectGdkKeymapChangedSignal;
function GetVKeyInfo(const AVKey: Byte): TVKeyInfo;
function GTKEventStateToShiftState(KeyState: LongWord): TShiftState;
procedure gdk_event_key_get_string(Event: PGDKEventKey; var theString: Pointer);
procedure gdk_event_key_set_string(Event: PGDKEventKey; const NewString: PChar);
function gdk_event_get_type(Event: Pointer): TGdkEventType;
procedure RememberKeyEventWasHandledByLCL(Event: PGdkEventKey;
                                          BeforeEvent: boolean);
function KeyEventWasHandledByLCL(Event: PGdkEventKey;
                                 BeforeEvent: boolean): boolean;
function HandleGTKKeyUpDown(AWidget: PGtkWidget; AEvent: PGdkEventKey;
  AData: gPointer; ABeforeEvent, AHandleDown: Boolean; const AEventName: PGChar
  ) : GBoolean;

// ----

// common dialogs
procedure StoreCommonDialogSetup(ADialog: TCommonDialog);
procedure DestroyCommonDialogAddOns(ADialog: TCommonDialog);
procedure PopulateFileAndDirectoryLists(FileSelection: PGtkFileSelection;
                                        const Mask: string);

// notebook
function IsTTabControl(AWidget: PGtkWidget): Boolean;
procedure UpdateNoteBookClientWidget(ANoteBook: TObject);
function GetGtkNoteBookPageCount(ANoteBookWidget: PGtkNoteBook): integer;
procedure UpdateNotebookPageTab(ANoteBook, APage: TObject);
procedure UpdateNotebookTabFont(APage: TWinControl; AFont: TFont);

// coordinate transformation
function GetWidgetOrigin(TheWidget: PGtkWidget): TPoint;
function GetWidgetClientOrigin(TheWidget: PGtkWidget): TPoint;
function GetWidgetClientRect(TheWidget: PGtkWidget): TRect;
function TranslateGdkPointToClientArea(SourceWindow: PGdkWindow;
  SourcePos: TPoint;  DestinationWidget: PGtkWidget): TPoint;
function SubtractScoll(AWidget: PGtkWidget; APosition: TPoint): TPoint;

// mouse capturing
function GetDefaultMouseCaptureWidget(Widget: PGtkWidget): PGtkWidget;
procedure ReleaseMouseCapture;
procedure ReleaseCaptureWidget(Widget : PGtkWidget);

// window group
function GetGtkWindowGroup(Widget: PGtkWidget): PGtkWindowGroup;

const
  // for now return the same value, in the future we may want to return an
  // offset of -1 so we can use 0 as error (now crDefault = 0)
  // In the current situation, a TCursor is passed as hCursor. Since both are
  // ordinals, the compiler won't complain
  PREDEFINED_CURSOR_OFFSET = 0; //-1;



// designing
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
    dstExposeAfter
    );
  TDesignSignalTypes = set of TDesignSignalType;

  TDesignSignalMask = longint;

const
  DesignSignalBefore: array[TDesignSignalType] of boolean = (
    true,  // dstUnknown
    true,  // dstMousePress
    true,  // dstMouseMotion
    true,  // dstMouseRelease
    false  // dstExposeAfter
    );

  DesignSignalAfter: array[TDesignSignalType] of boolean = (
    false, // dstUnknown
    false, // dstMousePress
    false, // dstMouseMotion
    false, // dstMouseRelease
    false  // dstExposeAfter
    );

  DesignSignalNames: array[TDesignSignalType] of PChar = (
    '',
    'button-press-event',
    'motion-notify-event',
    'button-release-event',
    'expose-event'
    );

  DesignSignalFuncs: array[TDesignSignalType] of Pointer = (
    nil,
    @gtkMouseBtnPress,
    @gtkMotionNotify,
    @gtkMouseBtnRelease,
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

// signals
// new signal procs, these will obsolete the old ones
// new signalshandlers are attached locally in the new WSxxx classes
// they also have PWidgetInfo as data (and not the TControl)
// signals are now also handled dedicated and locally, so no case statements
// anymore in signal handlers
procedure SignalConnect(const AWidget: PGTKWidget; const ASignal: PChar;
  const AProc: Pointer; const AInfo: PWidgetInfo);
procedure SignalConnectAfter(const AWidget: PGTKWidget; const ASignal: PChar;
  const AProc: Pointer; const AInfo: PWidgetInfo);

// old signal procs
// since they are used in attachcallbacks, and they pass TControl as data
// One day attachsignals gets removed.
procedure ConnectSignal(const AnObject: PGTKObject; const ASignal: PChar;
  const ACallBackProc: Pointer; const ALCLObject: TObject;
  const AReqSignalMask: TGdkEventMask; const ASFlags: TConnectSignalFlags);
procedure ConnectSignal(const AnObject: PGTKObject; const ASignal: PChar;
  const ACallBackProc: Pointer; const ALCLObject: TObject;
  const AReqSignalMask: TGdkEventMask);
procedure ConnectSignalAfter(const AnObject:PGTKObject; const ASignal: PChar;
  const ACallBackProc: Pointer; const ALCLObject: TObject;
  const AReqSignalMask: TGdkEventMask);
procedure ConnectSignal(const AnObject:PGTKObject; const ASignal: PChar;
  const ACallBackProc: Pointer; const ALCLObject: TObject);
procedure ConnectSignalAfter(const AnObject:PGTKObject; const ASignal: PChar;
  const ACallBackProc: Pointer; const ALCLObject: TObject);

procedure ConnectInternalWidgetsSignals(AWidget: PGtkWidget;
  AWinControl: TWinControl);

//--

// accelerators
function DeleteAmpersands(var Str: String): Longint;
function Ampersands2Underscore(Src: PChar): PChar;
function Ampersands2Underscore(const ASource: String): String;
function EscapeUnderscores(const Str: String): String; inline;
function RemoveAmpersands(Src: PChar; LineLength: Longint): PChar;
function RemoveAmpersands(const ASource: String): String;
procedure LabelFromAmpersands(var AText, APattern: String; var AAccelChar: Char);

function GetAccelGroup(const Widget: PGtkWidget;
  CreateIfNotExists: boolean): PGTKAccelGroup;
procedure SetAccelGroup(const Widget: PGtkWidget;
  const AnAccelGroup: PGTKAccelGroup);
procedure FreeAccelGroup(const Widget: PGtkWidget);
procedure RegroupAccelerator(Widget: PGtkWidget);
procedure ClearAccelKey(Widget: PGtkWidget);
procedure Accelerate(Component: TComponent; const Widget: PGtkWidget;
  const Key: guint; Mods: TGdkModifierType; const Signal: string);
procedure Accelerate(Component: TComponent; const Widget: PGtkWidget;
  const NewShortCut: TShortCut; const Signal: string);
procedure ShareWindowAccelGroups(AWindow: PGtkWidget);
procedure UnshareWindowAccelGroups(AWindow: PGtkWidget);

// pixbuf
procedure LoadPixbufFromLazResource(const ResourceName: string;
  var Pixbuf: PGdkPixbuf);
procedure LoadXPMFromLazResource(const ResourceName: string;
  Window: PGdkWindow; var PixmapImg, PixmapMask: PGdkPixmap);

function CreatePixbufFromDrawable(ASource: PGdkDrawable; AColorMap:PGdkColormap; AIncludeAplha: Boolean; ASrcX, ASrcY, ADstX, ADstY, AWidth, AHeight :longint): PGdkPixbuf;


// pixmaps
procedure SetGCRasterOperation(TheGC: PGDKGC; Rop: Cardinal);
procedure MergeClipping(DestinationDC: TGtkDeviceContext; DestinationGC: PGDKGC;
  X,Y,Width,Height: integer; ClipMergeMask: PGdkBitmap;
  ClipMergeMaskX, ClipMergeMaskY: integer;
  out NewClipMask: PGdkBitmap);
function CreatePixbufFromImageAndMask(ASrc: PGdkDrawable; ASrcX, ASrcY, ASrcWidth,
  ASrcHeight: integer; ASrcColorMap: PGdkColormap; ASrcMask: PGdkBitmap): PGdkPixbuf;
function ScalePixmapAndMask({%H-}AScaleGC: PGDKGC; AScaleMethod: TGdkInterpType;
  ASrc: PGdkPixmap; ASrcX, ASrcY, ASrcWidth, ASrcHeight: integer;
  ASrcColorMap: PGdkColormap; ASrcMask: PGdkBitmap;
  ADstWidth, ADstHeight: Integer; FlipHorz, FlipVert: Boolean;
  out ADst, ADstMask: PGdkPixmap): Boolean;

// obsolete:
function GetGdkImageBitsPerPixel(Image: PGdkImage): cardinal;
function CreateGdkMaskBitmap(AImage, AMask: HBITMAP): PGdkBitmap;
function CreateGdkMaskBitmap(AImageMask, AMask: PGdkBitmap): PGdkBitmap;
function ExtractGdkBitmap(Bitmap: PGdkBitmap; const SrcRect: TRect): PGdkBitmap;
procedure CheckGdkImageBitOrder(AImage: PGdkImage; AData: PByte; ADataCount: Integer);


// menus
function MENU_ITEM_CLASS(widget: PGtkWidget): PGtkMenuItemClass;
function CHECK_MENU_ITEM_CLASS(widget: PGtkWidget): PGtkCheckMenuItemClass;
procedure LockRadioGroupOnChange(RadioGroup: PGSList; const ADelta: Integer);
procedure UpdateRadioGroupChecks(RadioGroup: PGSList);
procedure UpdateInnerMenuItem(LCLMenuItem: TMenuItem; MenuItemWidget: PGtkWidget);
procedure UpdateInnerMenuItem(LCLMenuItem: TMenuItem; MenuItemWidget: PGtkWidget;
                              NewShortCut, ShortCutKey2: TShortCut);

// statusbar
function CreateStatusBarPanel({%H-}StatusBar: TObject; {%H-}Index: integer): PGtkWidget;
procedure UpdateStatusBarPanels(StatusBar: TObject;
                                StatusBarWidget: PGtkWidget);
procedure UpdateStatusBarPanel(StatusBar: TObject; Index: integer;
                               StatusPanelWidget: PGtkWidget);

// list
function gtkListGetSelectionMode(list: PGtkList): TGtkSelectionMode;cdecl;

// sizing
procedure SaveSizeNotification(Widget: PGtkWidget);
procedure SaveClientSizeNotification(FixWidget: PGtkWidget);
procedure SendSizeNotificationToLCL(aWidget: PGtkWidget);
function CreateTopologicalSortedWidgets(HashArray: TDynHashArray): TFPList;
procedure GetGTKDefaultWidgetSize(AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; {%H-}WithThemeSpace: Boolean);
procedure SendCachedGtkResizeNotifications;
procedure ResizeHandle(LCLControl: TWinControl);
procedure SetWidgetSizeAndPosition(LCLControl: TWinControl);// for child controls
procedure SetWindowSizeAndPosition(Window: PGtkWindow; AWinControl: TWinControl);// for top level control
procedure GetWidgetRelativePosition(aWidget: PGtkWidget; out Left, Top: integer);
procedure UnsetResizeRequest(Widget: PGtkWidget);
procedure SetResizeRequest(Widget: PGtkWidget);
function WidgetSizeIsEditable(Widget: PGtkWidget): boolean;

// debug
procedure ReportNotObsolete(const Texts: String);

// screen
function GetScreenWidthMM(GdkValue: boolean = false): integer;
function GetScreenHeightMM(GdkValue: boolean = false): integer;

// clipboard
function WaitForClipboardAnswer(c: PClipboardEventData): boolean;
function RequestSelectionData(ClipboardWidget: PGtkWidget;
  ClipboardType: TClipboardType;  FormatID: PtrUInt): TGtkSelectionData;
procedure FreeClipboardTargetEntries(ClipboardType: TClipboardType);
function GdkAtomToStr(const Atom: TGdkAtom): string;

// forms
function CreateFormContents({%H-}AForm: TCustomForm;
  var FormWidget: Pointer; AWidgetInfo: PWidgetInfo = nil): Pointer;

// styles
type
  PStyleObject = ^TStyleObject;
  TStyleObject = Record
    Style: PGTKStyle;
    Owner: PGtkWidget;  // The widget that we hold a reference to.
    Widget: PGTKWidget; // This is the style widget.
    FrameBordersValid: boolean;
    FrameBorders: TRect;
  end;

var
  StandardStyles: array[TLazGtkStyle] of PStyleObject;

function IndexOfStyle(aStyle: TLazGtkStyle): integer;
function IndexOfStyleWithName(const WName: String): integer;
procedure ReleaseAllStyles;
procedure ReleaseStyle(aStyle: TLazGtkStyle);
procedure ReleaseStyleWithName(const WName: String);
function GetStyle(aStyle: TLazGtkStyle): PGTKStyle;
function GetStyleWithName(const WName: String): PGTKStyle;
function GetStyleWidget(aStyle: TLazGtkStyle): PGTKWidget;
function GetStyleWidgetWithName(const WName: String): PGTKWidget;
function GetStyleGroupboxFrameBorders: TRect;
function GetStyleNotebookFrameBorders: TRect;

function GetStyleFormFrameBorders(WithMenu: boolean): TRect;

procedure StyleFillRectangle(drawable: PGDKDrawable; GC: PGDKGC;
                             Color: TColorRef; x, y, width, height: gint;
                             AClipArea: PGdkRectangle);
function StyleForegroundColor(Color: TColorRef; DefaultColor: PGDKColor): PGDKColor;
procedure UpdateWidgetStyleOfControl(AWinControl: TWinControl);

// fonts
function LoadDefaultFont: TGtkIntfFont;
function FontIsDoubleByteCharsFont(TheFont: TGtkIntfFont): boolean;
function FontIsMonoSpaceFont(TheFont: TGtkIntfFont): boolean;

function LoadDefaultFontDesc: PPangoFontDescription;

procedure GetTextExtentIgnoringAmpersands(TheFont: TGtkIntfFont;
  Str: PChar; StrLength: integer;
  lbearing, rbearing, width, ascent, descent: Pgint);
function GetDefaultFontName: string;
{$IFDEF GTK2OLDENUMFONTFAMILIES}
procedure FillScreenFonts(ScreenFonts: TStrings);
{$ENDIF}
function GetTextHeight(DCTextMetric: TDevContextTextMetric): integer;
{$IFDEF HasX}
function  XGetWorkarea(out ax,ay,awidth,aheight:gint): gint;
{$ENDIF}


// decoration
function GetWindowDecorations(AForm: TCustomForm): Longint;
function GetWindowFunction(AForm: TCustomForm): Longint;

// functions for easier GTK2<->GTK1 Compatibility/Consistency  ---->
function gtk_widget_get_xthickness(Style: PGTKStyle): gint; overload;
function gtk_widget_get_ythickness(Style: PGTKStyle): gint; overload;

function gtk_widget_get_xthickness(Widget: PGTKWidget): gint; overload;
function gtk_widget_get_ythickness(Widget: PGTKWidget): gint; overload;
function GetGtkContainerBorderWidth(Widget: PGtkContainer): gint;


  function gtk_class_get_type(aclass: Pointer): TGtkType;

  //we wrap our own versions to handle nil tests -->
  function gtk_object_get_class(anobject: Pointer): Pointer;
  function gtk_window_get_modal(window:PGtkWindow):gboolean;

  //we wrap our own versions to do gtk1 style result = new region -->
  function gdk_region_union_with_rect(region:PGdkRegion;
                                      rect:PGdkRectangle): PGdkRegion;
  function gdk_region_intersect(source1:PGdkRegion;
                                source2:PGdkRegion): PGdkRegion;
  function gdk_region_union(source1:PGdkRegion; source2:PGdkRegion): PGdkRegion;
  function gdk_region_subtract(source1:PGdkRegion;
                               source2:PGdkRegion): PGdkRegion;
  function gdk_region_xor(source1:PGdkRegion; source2:PGdkRegion): PGdkRegion;

  //mimic GDKFont Routines With Pango -->
  procedure gdk_text_extents(TheFont: TGtkIntfFont;
        Str: PChar; StrLength: integer;
        lbearing, rbearing, width, ascent, descent: Pgint);


{$ifdef HasX}
// X functions
function FormToX11Window(const AForm: TCustomForm): X.TWindow;
{$endif}
function FindFocusWidget(AWidget: PGtkWidget): PGtkWidget;

const
  gtkevent_window_state_event = 'window-state-event';

function GTKWindowStateEventCB(widget: PGtkWidget;
                               state: PGdkEventWindowState;
                               data: gpointer): gboolean; cdecl;

function gtkMouseWheelCB({%H-}widget: PGtkWidget; event: PGdkEventScroll;
                         data: gPointer): GBoolean; cdecl;

implementation

uses
  {$IFDEF StaticXinerama} Xinerama, {$ENDIF}
  Gtk2WSPrivate, URIParser, Gtk2Int, Gtk2WinApiWindow;

const
  KCINFO_FLAG_SHIFT         = $01;
  //KCINFO_FLAG_CTRL          = $02;
  KCINFO_FLAG_ALTGR         = $04;
  //KCINFO_FLAG_KEY_MASK      = $07;
  KCINFO_FLAG_EXT           = $10; // extended key
  //KCINFO_FLAG_TOGGLE        = $20; // toggle key

  KCINFO_FLAG_SHIFT_XOR_NUM = $40; // second vkey should be used when numlock <>shift
  //KCINFO_FLAG_MULTI_MASK    = $C0; // key has more than one VK


type
  TKeyCodeInfo = record
    VKey1: Byte;
    VKey2: Byte; // second code to be used depending on the type of MULTI_VK flag
    Flags: Byte; // indicates if Alt | Ctrl | Shift is needed
                 // extended state
  end;

var
  MKeyCodeInfo: array[Byte] of TKeyCodeInfo;
  MVKeyInfo: array[Byte] of TVKeyInfo;

  // Modifier keys can be set by a modmap and don't have to be the same on all systems
  // Some defaults are set here incase we didn't find them
type
  TModifier = record
    Mask: TGdkModifierType;    // if UseValue is set, the modifier is set when the masked state matches the value
    Value: TGdkModifierType;   // otherwise any nonzero value will match
    UseValue: Boolean;
  end;

var
  MModifiers: array[TShiftStateEnum] of TModifier;

{$ifdef UseOwnShiftState}
{$ifdef HasX}
  // KeyStateMap is a quick index to scan the results of a XQueryKeymap
  // which returns for the 256 possible keycodes a boolean bit array (32 bytes)
  // Shift is set when the mask for the Keymapkeys_return[index] is set
var
  MKeyStateMap: array of record
    Index: Byte; // KeyCode shr 3
    Mask: Byte;  // 1 shl (KeyCode and 7);
    Enum: TShiftStateEnum;
  end;
{$endif}
{$endif}

type
  // TLCLHandledKeyEvent is used to remember, if an gdk key event was already
  // handled.
  TLCLHandledKeyEvent = class
  public
    thetype: TGdkEventType;
    window: PGdkWindow;
    send_event: gint8;
    time: guint32;
    state : guint;
    keyval: guint;
    hardware_keycode : guint16;
    constructor Create(Event: PGdkEventKey);
    function IsEqual(Event: PGdkEventKey): boolean;
  end;

  TWinControlAccess = class(TWinControl)
  end;

{ TLCLHandledKeyEvent }

constructor TLCLHandledKeyEvent.Create(Event: PGdkEventKey);
begin
  thetype:=gdk_event_get_type(Event);
  window:=Event^.window;
  send_event:=Event^.send_event;
  time:=Event^.time;
  state:=Event^.state;
  keyval:=Event^.keyval;
  // event^.length ?
  // event^._string ?
  hardware_keycode:=event^.hardware_keycode;
end;

function TLCLHandledKeyEvent.IsEqual(Event: PGdkEventKey): boolean;
begin
  Result:=(gdk_event_get_type(Event)=thetype)
      and (window=Event^.window)
      and (send_event=Event^.send_event)
      and (time=Event^.time)
      and (keyval=Event^.keyval)
      ;
end;

var
  // LCLHandledKeyEvents stores the last handled key event (handled by the LCL)
  // Reason: The gtk sends the same key event to several widgets. The gtk intf
  // only wants to send them once to the LCL.
  LCLHandledKeyEvents: TFPList; // list of TLCLHandledKeyEvent
  LCLHandledKeyAfterEvents: TFPList; // list of TLCLHandledKeyEvent

var
  GdkTrapIsSet: Boolean;
  GdkTrapCalls: Integer;

procedure Set_RC_Name(Sender: TObject; AWidget: PGtkWidget);
var RCName: string;
  AComponent: TComponent;
begin
  {$IFDEF NoStyle}
  exit;
  {$ENDIF}
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
    // do not set widget name issue #21661
    // gtk_widget_set_name(AWidget,PChar(RCName));
    //debugln('Set_RC_Name ',GetWidgetDebugReport(AWidget),' RCName="',RCName,'"');
    gtk_widget_set_rc_style(AWidget);
  end;
end;

procedure InitGTKProc;
var
  lgs: TLazGtkStyle;
begin
  //MKeySymToVKMap := TMap.Create(itu4, SizeOf(TVKeyRecord));
  // UTF8 is max 4 bytes, acombined makes it 8
  //MSymCharToVKMap := TMap.Create(itu8, SizeOf(TVKeyRecord));

  // fill initial modifier list
  FillByte(MModifiers, SizeOf(MModifiers), 0);
  // keyboard
  {$ifndef UseOwnShiftState}
  MModifiers[ssCaps].Mask     := GDK_LOCK_MASK;
  MModifiers[ssNum].Mask      := GDK_MOD3_MASK; //todo: check this I've 2 here,but 3 was the original code
  MModifiers[ssScroll].Mask   := GDK_MOD5_MASK; //todo: check this I've ssAltGr here, but ssScroll was the original code
  MModifiers[ssShift].Mask    := GDK_SHIFT_MASK;
  MModifiers[ssCtrl].Mask     := GDK_CONTROL_MASK;
  MModifiers[ssAlt].Mask      := GDK_MOD1_MASK;
  MModifiers[ssSuper].Mask    := GDK_MOD4_MASK;
  MModifiers[ssAltGr].Mask    := GDK_RELEASE_MASK;
  {$endif}
  // mouse
  MModifiers[ssLeft].Mask     := GDK_BUTTON1_MASK;
  MModifiers[ssMiddle].Mask   := GDK_BUTTON2_MASK;
  MModifiers[ssRight].Mask    := GDK_BUTTON3_MASK;
  MModifiers[ssExtra1].Mask   := GDK_BUTTON4_MASK;
  MModifiers[ssExtra2].Mask   := GDK_BUTTON5_MASK;

  FillChar(MKeyCodeInfo, SizeOf(MKeyCodeInfo), $FF);
  FillChar(MVKeyInfo, SizeOf(MVKeyInfo), 0);


  GdkTrapIsSet := False;
  GdkTrapCalls := 0;
  LCLHandledKeyEvents:=nil;
  LCLHandledKeyAfterEvents:=nil;

  for lgs:=Low(TLazGtkStyle) to High(TLazGtkStyle) do
    StandardStyles[lgs]:=nil;
end;

procedure DoneGTKProc;
begin
  DoneKeyboardTables;
//  FreeAndNil(MKeySymToVKMap);
//  FreeAndNil(MSymCharToVKMap);
end;

{ Callbacks for events }

{$I gtk2proc.inc}
{$I gtk2callback.inc}

initialization
  InitGTKProc;

finalization
  DoneGTKProc;

end.

