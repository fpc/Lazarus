{
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
unit GTKProc;

{$mode objfpc}{$H+}

interface

{$IFDEF win32}
{off $DEFINE NoGdkPixbufLib}
{$ELSE}
{off $DEFINE NoGdkPixbufLib}
{$ENDIF}

{off $DEFINE GDK_ERROR_TRAP_FLUSH}
{$DEFINE REPORT_GDK_ERRORS}

{off $DEFINE VerboseAccelerator}

{$IFDEF Unix}
  {$DEFINE HasX}
  {$IFDEF Gtk1}
    {$DEFINE HasGtkX}
  {$ENDIF}
{$ENDIF}

uses
  {$IFDEF win32}
    // use windows unit first,
    // if not, Rect and Point are taken from the windows unit instead of classes.
    Windows, // needed for keyboard handling
  {$endif}
  {$IFDEF Unix}
    baseunix, unix,
  {$ENDIF}
  SysUtils, Classes, FPCAdds,
  {$IFDEF HasX}
    XAtom, X, XLib, XUtil, //Font retrieval and Keyboard handling
  {$ENDIF}
  InterfaceBase,
  {$IFDEF gtk2}
    glib2, gdk2pixbuf, gdk2, gtk2, Pango,
  {$ELSE}
    glib, gdk, gtk, {$Ifndef NoGdkPixbufLib}gdkpixbuf,{$EndIf}
  {$ENDIF}
  LMessages, LCLProc, LCLStrConsts, LCLIntf, LCLType, DynHashArray, Maps,
  GraphType, GraphMath, Graphics, GTKWinApiWindow, LResources, Controls, Forms,
  Buttons, Menus, StdCtrls, ComCtrls, CommCtrl, ExtCtrls, Dialogs, ExtDlgs,
  FileUtil, ImgList, GtkFontCache, GTKGlobals, gtkDef;



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

{$IFDEF gtk2}
const
  gdkdll = gdklib;
{$ENDIF}

{$IFDEF GTK1}
  function GDK_GET_CURRENT_DESKTOP(): gint;
  function GDK_WINDOW_GET_DESKTOP(Window: PGdkWindowPrivate): gint;
  function GDK_WINDOW_SET_DESKTOP(Window: PGdkWindowPrivate; Desktop: gint): gint;
  procedure GDK_WINDOW_ACTIVATE(Window: PGdkWindowPrivate);
  procedure GDK_WINDOW_MAXIMIZE(Window: PGdkWindowPrivate);
  procedure GDK_WINDOW_MINIMIZE(Window: PGdkWindowPrivate);
  function GDK_WINDOW_GET_MAXIMIZED(Window: PGdkWindowPrivate): gboolean;
  procedure GDK_WINDOW_SHOW_IN_TASKBAR(Window: PGdkWindowPrivate; Show: Boolean);
{$ENDIF}
  

{$IFNDEF GTK2}
  function  GTK_TYPE_WIDGET : TGTKType; cdecl; external gtkdll name 'gtk_widget_get_type';
  function  GTK_TYPE_CONTAINER: TGTKType; cdecl; external gtkdll name 'gtk_container_get_type';
  function  GTK_TYPE_BIN : TGTKType; cdecl; external gtkdll name 'gtk_bin_get_type';
  function  GTK_TYPE_HBOX : TGTKType; cdecl; external gtkdll name 'gtk_hbox_get_type';
  function  GTK_TYPE_SCROLLED_WINDOW: TGTKType; cdecl; external gtkdll name 'gtk_scrolled_window_get_type';
  function  GTK_TYPE_COMBO : TGTKType; cdecl; external gtkdll name 'gtk_combo_get_type';
  function  GTK_TYPE_WINDOW : TGTKType; cdecl; external gtkdll name 'gtk_window_get_type';
  function  GTK_TYPE_MENU : TGTKType; cdecl; external gtkdll name 'gtk_menu_get_type';
  function  GTK_TYPE_MENU_ITEM : TGTKType; cdecl; external gtkdll name 'gtk_menu_item_get_type';
  function  GTK_TYPE_MENU_BAR : TGTKType; cdecl; external gtkdll name 'gtk_menu_bar_get_type';
  function  GTK_TYPE_RADIO_MENU_ITEM : TGTKType; cdecl; external gtkdll name 'gtk_radio_menu_item_get_type';
  function  GTK_TYPE_CHECK_MENU_ITEM : TGTKType; cdecl; external gtkdll name 'gtk_check_menu_item_get_type';
  function  GTK_TYPE_TEXT : TGTKType; cdecl; external gtkdll name 'gtk_text_get_type';
  function  GTK_TYPE_ENTRY : TGTKType; cdecl; external gtkdll name 'gtk_entry_get_type';
  function  GTK_TYPE_RANGE : TGTKType; cdecl; external gtkdll name 'gtk_range_get_type';
  function  GTK_TYPE_SCROLLBAR: TGTKType; cdecl; external gtkdll name 'gtk_scrollbar_get_type';
  function  GTK_TYPE_HSCROLLBAR: TGTKType; cdecl; external gtkdll name 'gtk_hscrollbar_get_type';
  function  GTK_TYPE_VSCROLLBAR: TGTKType; cdecl; external gtkdll name 'gtk_vscrollbar_get_type';
  function  GTK_TYPE_LIST_ITEM: TGTKType; cdecl; external gtkdll name 'gtk_list_item_get_type';
{$ENDIF}

// missing gtk2 functions/vars
{$IFDEF GTK2}
{$IFDEF Unix}
  var
    gdk_display: PDisplay; external gdkdll name 'gdk_display';

  function gdk_screen_get_default: PGdkScreen; cdecl; external gdklib;
{$ENDIF UNIX}
{$ENDIF GTK2}


procedure laz_gdk_gc_set_dashes(gc:PGdkGC; dash_offset:gint;
  dashlist:Pgint8; n:gint); cdecl; external gdkdll name 'gdk_gc_set_dashes';


// GTKCallback.inc headers
procedure EventTrace(const TheMessage: string; data: pointer);
function gtkNoteBookCloseBtnClicked(Widget: PGtkWidget;
  Data: Pointer): GBoolean; cdecl;
function gtkRealizeCB(Widget: PGtkWidget; Data: Pointer): GBoolean; cdecl;
function gtkRealizeAfterCB(Widget: PGtkWidget; Data: Pointer): GBoolean; cdecl;
function gtkshowCB( widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkHideCB( widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkactivateCB(widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkchangedCB( widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkchanged_editbox( widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkdaychanged(Widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtktoggledCB( widget: PGtkWidget; data: gPointer): GBoolean; cdecl;

{$IFDEF Gtk2}
function GTKWindowStateEventCB(widget: PGtkWidget;
                               state: PGdkEventWindowState;
                               data: gpointer): gboolean; cdecl;
{$ENDIF}

{$Ifdef GTK1}
function gtkDrawAfter(Widget: PGtkWidget; area: PGDKRectangle;
  data: gPointer): GBoolean; cdecl;
{$EndIf}
function gtkExposeEventAfter(Widget: PGtkWidget; Event: PGDKEventExpose;
  Data: gPointer): GBoolean; cdecl;
function gtkfrmactivateAfter( widget: PGtkWidget; Event: PgdkEventFocus;
  data: gPointer): GBoolean; cdecl;
function gtkfrmdeactivateAfter( widget: PGtkWidget; Event: PgdkEventFocus;
  data: gPointer): GBoolean; cdecl;
function GTKMap(Widget: PGTKWidget; Data: gPointer): GBoolean; cdecl;
function GTKKeyUpDown(Widget: PGtkWidget; Event: pgdkeventkey;
  Data: gPointer): GBoolean; cdecl;
function GTKKeyUpDownAfter(Widget: PGtkWidget; Event: pgdkeventkey;
  Data: gPointer): GBoolean; cdecl;
function GTKFocusCB(widget: PGtkWidget; event:PGdkEventFocus;
                    data: gPointer): GBoolean; cdecl;
function GTKFocusCBAfter(widget: PGtkWidget; event:PGdkEventFocus;
                         data: gPointer): GBoolean; cdecl;
function GTKKillFocusCB(widget: PGtkWidget; event:PGdkEventFocus;
                        data: gPointer): GBoolean; cdecl;
function GTKKillFocusCBAfter(widget: PGtkWidget; event:PGdkEventFocus;
                             data: gPointer): GBoolean; cdecl;
function gtkdestroyCB(widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkdeleteCB(widget: PGtkWidget; event: PGdkEvent;
                     data: gPointer): GBoolean; cdecl;
function gtkresizeCB(widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkMonthChanged(Widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
procedure DeliverMouseMoveMessage(Widget:PGTKWidget; Event: PGDKEventMotion;
                                  AWinControl: TWinControl);
function ControlGetsMouseMoveBefore(AControl: TControl): boolean;
function gtkMotionNotify(Widget:PGTKWidget; Event: PGDKEventMotion;
                         Data: gPointer): GBoolean; cdecl;
function GTKMotionNotifyAfter(widget:PGTKWidget; event: PGDKEventMotion;
                              data: gPointer): GBoolean; cdecl;
function ControlGetsMouseDownBefore(AControl: TControl;
                                    AWidget: PGtkWidget): boolean;
procedure DeliverMouseDownMessage(widget: PGtkWidget; event: pgdkEventButton;
                                  AWinControl: TWinControl);
function gtkMouseBtnPress(widget: PGtkWidget; event: pgdkEventButton;
                          data: gPointer): GBoolean; cdecl;
function gtkMouseBtnPressAfter(widget: PGtkWidget; event: pgdkEventButton;
                               data: gPointer): GBoolean; cdecl;
function ControlGetsMouseUpBefore(AControl: TControl): boolean;
procedure DeliverMouseUpMessage(widget: PGtkWidget; event: pgdkEventButton;
                                AWinControl: TWinControl);
function gtkMouseBtnRelease(widget: PGtkWidget; event: pgdkEventButton;
                            data: gPointer): GBoolean; cdecl;
function gtkMouseBtnReleaseAfter(widget: PGtkWidget; event: pgdkEventButton;
                                 data: gPointer): GBoolean; cdecl;
function gtkclickedCB( widget: PGtkWidget; data: gPointer): GBoolean; cdecl;

function gtkDialogSelectRowCB(widget: PGtkWidget; Row, Column: gInt;
  bevent: pgdkEventButton; data: gPointer): GBoolean; cdecl;
function gtkDialogOKclickedCB( widget: PGtkWidget;
  data: gPointer): GBoolean; cdecl;
function gtkDialogCancelclickedCB(widget: PGtkWidget; data: gPointer): GBoolean;cdecl;
function gtkDialogHelpclickedCB(widget: PGtkWidget; data: gPointer): GBoolean;cdecl;
function gtkDialogApplyclickedCB(widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkDialogCloseQueryCB(widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
procedure UpdateDetailView(OpenDialog: TOpenDialog);
function GTKDialogKeyUpDownCB(Widget: PGtkWidget; Event: pgdkeventkey;
  Data: gPointer): GBoolean; cdecl;
function GTKDialogRealizeCB(Widget: PGtkWidget; Data: Pointer): GBoolean; cdecl;
function GTKDialogFocusInCB(widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function GTKDialogMenuActivateCB(widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkDialogDestroyCB(widget: PGtkWidget; data: gPointer): GBoolean; cdecl;

function gtkPressedCB( widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkEnterCB(widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkLeaveCB(widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkMoveCursorCB(widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtksize_allocateCB(widget: PGtkWidget; size :pGtkAllocation;
  data: gPointer): GBoolean; cdecl;
function gtksize_allocate_client(widget: PGtkWidget; size :pGtkAllocation;
  data: gPointer): GBoolean; cdecl;
function gtkswitchpage(widget: PGtkWidget; page: Pgtkwidget; pagenum: integer;
  data: gPointer): GBoolean; cdecl;
function gtkconfigureevent( widget: PGtkWidget; event: PgdkEventConfigure;
  data: gPointer): GBoolean; cdecl;
function gtkreleasedCB(widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkInsertText(widget: PGtkWidget; char: pChar; NewTextLength:
                     Integer; Position: pgint; data: gPointer): GBoolean; cdecl;
function gtkDeleteText(widget: PGtkWidget; Startpos, EndPos: Integer;
                       data: gPointer): GBoolean; cdecl;
function gtkSetEditable( widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkMoveWord( widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkMovePage( widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkMoveToRow( widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkMoveToColumn( widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkKillChar( widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkKillWord( widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkKillLine( widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkCutToClip( widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkCopyToClip( widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkPasteFromClip( widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkValueChanged(widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkTimerCB(Data: gPointer): {$IFDEF Gtk2}gBoolean{$ELSE}gint{$ENDIF}; cdecl;
function gtkFocusInNotifyCB (widget: PGtkWidget; event: PGdkEvent;
  data: gpointer): GBoolean; cdecl;
function gtkFocusOutNotifyCB (widget: PGtkWidget; event: PGdkEvent;
  data: gpointer): GBoolean; cdecl;
function GTKHScrollCB(Adjustment: PGTKAdjustment; data: GPointer): GBoolean; cdecl;
function GTKVScrollCB(Adjustment: PGTKAdjustment;
  data: GPointer): GBoolean; cdecl;
function GTKCheckMenuToggeledCB(AMenuItem: PGTKCheckMenuItem;
                                AData: gPointer): GBoolean; cdecl;
function GTKKeySnooper(Widget: PGtkWidget; Event: PGdkEventKey;
  FuncData: gPointer): gInt; cdecl;
function gtkYearChanged(Widget: PGtkWidget; data: gPointer): GBoolean; cdecl;

// clipboard
procedure ClipboardSelectionReceivedHandler(TargetWidget: PGtkWidget;
  SelectionData: PGtkSelectionData; TimeID: cardinal; Data: Pointer); cdecl;
procedure ClipboardSelectionRequestHandler(TargetWidget: PGtkWidget;
  SelectionData: PGtkSelectionData; Info: cardinal; TimeID: cardinal;
  Data: Pointer); cdecl;
function ClipboardSelectionLostOwnershipHandler(TargetWidget: PGtkWidget;
  EventSelection: PGdkEventSelection;  Data: Pointer): cardinal; cdecl;

procedure GTKStyleChanged(Widget: PGtkWidget; previous_style :
  PGTKStyle; Data: Pointer); cdecl;
function gtkListBoxSelectionChangedAfter(widget: PGtkWidget;
                                      data: gPointer): GBoolean; cdecl;

function gtkListSelectChild(widget: PGtkWidget;child : PGtkWidget;
                                      data: gPointer): GBoolean; cdecl;

// gtkDragCallback.inc headers
Function edit_drag_data_received(widget: pgtkWidget;
             Context: pGdkDragContext;
             X: Integer;
             Y: Integer;
             seldata: pGtkSelectionData;
             info: Integer;
             time: Integer;
                                  data: pointer): GBoolean; cdecl;
Function edit_source_drag_data_get(widget: pgtkWidget;
             Context: pGdkDragContext;
             Selection_data: pGtkSelectionData;
             info: Integer;
             time: Integer;
                                  data: pointer): GBoolean; cdecl;
Function Edit_source_drag_data_delete (widget: pGtkWidget;
                   context: pGdkDragContext;
                   data: gpointer): gBoolean ; cdecl;

// gtklistviewcallbacks.inc headers
function gtkLVAbortColumnResize(AList: PGTKCList; AData: gPointer): GBoolean; cdecl;
function gtkLVResizeColumn(AList: PGTKCList; AColumn, AWidth: Integer;
                           AData: gPointer): GBoolean; cdecl;
function gtkLVClickColumn(AList: PGTKCList; AColumn: Integer;
                          AData: gPointer): GBoolean; cdecl;

// gtkcomboboxcallbacks.inc headers
function gtkComboBoxShowAfter(widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkComboBoxHideAfter(widget: PGtkWidget; data: gPointer): GBoolean; cdecl;

// gtkpagecallbacks.inc headers
function PageIconWidgetExposeAfter(Widget: PGtkWidget; Event: PGDKEventExpose;
  Data: gPointer): GBoolean; cdecl;
{$IfNdef GTK2}
function PageIconWidgetDrawAfter(Widget: PGtkWidget; area: PGDKRectangle;
  data: gPointer): GBoolean; cdecl;
{$EndIf}

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

// debugging
function GtkWidgetIsA(Widget: PGtkWidget; AType: TGtkType): boolean;
function GetWidgetClassName(Widget: PGtkWidget): string;
function GetWidgetDebugReport(Widget: PGtkWidget): string;
function GetWindowDebugReport(AWindow: PGDKWindow): string;
function GetStyleDebugReport(AStyle: PGTKStyle): string;
function GetRCStyleDebugReport(AStyle: PGtkRcStyle): string;
{$IFDEF Gtk2}
function GetPangoDescriptionReport(Desc: PPangoFontDescription): string;
{$ENDIF}
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

// glib
procedure MoveGListLinkBehind(First, Item, After: PGList);
procedure MoveGListLink(First: PGList; FromIndex, ToIndex: integer);

// properties
function ObjectToGTKObject(const AnObject: TObject): PGtkObject;
function GetMainWidget(const Widget: Pointer): Pointer;
procedure SetMainWidget(const ParentWidget, ChildWidget: Pointer);
function GetFixedWidget(const Widget: Pointer): Pointer;
procedure SetFixedWidget(const ParentWidget, FixedWidget: Pointer);
Function GetControlWindow(Widget: Pointer): PGDKWindow;

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
procedure SetHiddenLCLObject(const Widget: Pointer; const AnObject: TObject);
function GetHiddenLCLObject(const Widget: Pointer): TObject;
function GetWinControlWidget(Child: PGtkWidget): PGtkWidget;
function GetWinControlFixedWidget(Child: PGtkWidget): PGtkWidget;
function FindFixedChildListItem(ParentFixed: PGtkFixed; Child: PGtkWidget): PGList;
function FindFixedLastChildListItem(ParentFixed: PGtkFixed): PGList;
function GetFixedChildListWidget(Item: PGList): PGtkWidget;

// fixed widgets
function CreateFixedClientWidget: PGTKWidget;
Procedure FixedMoveControl(Parent, Child: PGTKWIdget; Left, Top: Longint);
Procedure FixedPutControl(Parent, Child: PGTKWidget; Left, Top: Longint);

// caret
procedure HideCaretOfWidgetGroup(ChildWidget: PGtkWidget;
  var MainWidget: PGtkWidget; var CaretWasVisible: boolean);
  
// forms
procedure SetFormShowInTaskbar(AForm: TCustomForm;
                                    const AValue: TShowInTaskbar);
procedure SetGtkWindowShowInTaskbar(AGtkWindow: PGtkWindow; Value: boolean);
procedure SetWindowFullScreen(AForm: TCustomForm; const AValue: Boolean);
procedure GrabKeyBoardToForm(AForm: TCustomForm);
procedure ReleaseKeyBoardFromForm(AForm: TCustomForm);
procedure GrabMouseToForm(AForm: TCustomForm);
procedure ReleaseMouseFromForm(AForm: TCustomForm);

// combobox
procedure SetComboBoxText(ComboWidget: PGtkCombo; NewText: PChar);
function GetComboBoxText(ComboWidget: PGtkCombo): string;
function GetComboBoxItemIndex(ComboBox: TCustomComboBox): integer;
procedure SetComboBoxItemIndex(ComboBox: TCustomComboBox; Index: integer);

// label
procedure SetLabelAlignment(LabelWidget: PGtkLabel; 
  const NewAlignment: TAlignment);

// paint messages
function GtkPaintMessageToPaintMessage(var GtkPaintMsg: TLMGtkPaint;
  FreeGtkPaintMsg: boolean): TLMPaint;
procedure FinalizePaintMessage(Msg: PLMessage);
procedure FinalizePaintTagMsg(Msg: PMsg);

// DC
function GetDCOffset(DC: TDeviceContext): TPoint;
function CopyDCData(DestinationDC, SourceDC: TDeviceContext): Boolean;

// region
Function RegionType(RGN: PGDKRegion): Longint;
Procedure SelectGDIRegion(const DC: HDC);
function CreateRectGDKRegion(const ARect: TRect): PGDKRegion;
function GDKRegionAsString(RGN: PGDKRegion): string;

// color
Procedure FreeGDIColor(GDIColor: PGDIColor);
Procedure AllocGDIColor(DC: hDC; GDIColor: PGDIColor);
procedure BuildColorRefFromGDKColor(var GDIColor: TGDIColor);
procedure SetGDIColorRef(var GDIColor: TGDIColor; NewColorRef: TColorRef);
Procedure EnsureGCColor(DC: hDC; ColorType: TDevContextsColorType;
  IsSolidBrush, AsBackground: Boolean);
procedure CopyGDIColor(var SourceGDIColor, DestGDIColor: TGDIColor);
function AllocGDKColor(const AColor: LongInt): TGDKColor;
function TGDKColorToTColor(const value: TGDKColor): TColor;
function TColortoTGDKColor(const value: TColor): TGDKColor;
procedure UpdateSysColorMap(Widget: PGtkWidget);
function IsBackgroundColor(Color: TColor): boolean;

procedure RealizeGDKColor(ColorMap: PGdkColormap; Color: PGDKColor);
procedure RealizeGtkStyleColor(Style: PGTKStyle; Color: PGDKColor);
Function GetSysGCValues(Color: TColorRef; ThemeWidget: PGtkWidget): TGDKGCValues;

Function GDKPixel2GDIRGB(Pixel: Longint; Visual: PGDKVisual;
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
    KeyCode: Byte;
    KeySym: array[0..7] of Integer;
    KeyChar: array[0..3] of TVKeyUTF8Char;
  end;

procedure InitKeyboardTables;
procedure DoneKeyboardTables;
function CharToVKandFlags(const AUTF8Char: TVKeyUTF8Char): Word;
function GetVKeyInfo(const AVKey: Byte): TVKeyInfo;
function IsToggleKey(const AVKey: Byte): Boolean;
function GTKEventState2ShiftState(KeyState: Word): TShiftState;
//function KeyToListCode_(KeyCode, VirtKeyCode: Word; Extended: boolean): integer;
procedure gdk_event_key_get_string(Event: PGDKEventKey; var theString: Pointer);
procedure gdk_event_key_set_string(Event: PGDKEventKey; const NewString: PChar);
function gdk_event_get_type(Event: Pointer): TGdkEventType;
procedure RememberKeyEventWasHandledByLCL(Event: PGdkEventKey;
                                          BeforeEvent: boolean);
function KeyEventWasHandledByLCL(Event: PGdkEventKey;
                                 BeforeEvent: boolean): boolean;
function HandleGTKKeyUpDown(Widget: PGtkWidget; Event: PGdkEventKey;
  Data: gPointer; BeforeEvent: boolean) : GBoolean;

// ----

// common dialogs
procedure StoreCommonDialogSetup(ADialog: TCommonDialog);
procedure DestroyCommonDialogAddOns(ADialog: TCommonDialog);
procedure PopulateFileAndDirectoryLists(FileSelection: PGtkFileSelection;
                                        const Mask: string);

// notebook
function GetGtkNoteBookDummyPage(ANoteBookWidget: PGtkNoteBook): PGtkWidget;
procedure SetGtkNoteBookDummyPage(ANoteBookWidget: PGtkNoteBook;
  DummyWidget: PGtkWidget);
procedure UpdateNoteBookClientWidget(ANoteBook: TObject);
function GetGtkNoteBookPageCount(ANoteBookWidget: PGtkNoteBook): integer;
procedure RemoveDummyNoteBookPage(NoteBookWidget: PGtkNotebook);
procedure UpdateNotebookPageTab(ANoteBook, APage: TObject);

// coordinate transformation
function GetWidgetOrigin(TheWidget: PGtkWidget): TPoint;
function GetWidgetClientOrigin(TheWidget: PGtkWidget): TPoint;
function TranslateGdkPointToClientArea(SourceWindow: PGdkWindow;
  SourcePos: TPoint;  DestinationWidget: PGtkWidget): TPoint;
procedure SetCursor(AWinControl: TWinControl; ACursor: TCursor);

// mouse capturing
procedure CaptureMouseForWidget(Widget: PGtkWidget; Owner: TMouseCaptureType);
function GetDefaultMouseCaptureWidget(Widget: PGtkWidget): PGtkWidget;
procedure ReleaseMouseCapture;
procedure ReleaseCaptureWidget(Widget : PGtkWidget);
procedure UpdateMouseCaptureControl;

// mouse cursor
function GetGDKMouseCursor(Cursor: TCursor): PGdkCursor;
Procedure FreeGDKCursors;

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
{$Ifdef GTK1}
    dstDrawAfter,
{$EndIf}
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
{$Ifdef GTK1}
    false, // dstDrawAfter
{$Endif GTK1}
    false  // dstExposeAfter
    );

  DesignSignalAfter: array[TDesignSignalType] of boolean = (
    false, // dstUnknown
    false, // dstMousePress
    false, // dstMouseMotion
    false, // dstMouseRelease
{$Ifdef GTK1}
    false, // dstDrawAfter
{$Endif GTK1}
    false  // dstExposeAfter
    );

  DesignSignalNames: array[TDesignSignalType] of PChar = (
    '',
    'button-press-event',
    'motion-notify-event',
    'button-release-event',
{$Ifdef GTK1}
    'draw',
{$Endif GTK1}
    'expose-event'
    );

  DesignSignalFuncs: array[TDesignSignalType] of Pointer = (
    nil,
    @gtkMouseBtnPress,
    @gtkMotionNotify,
    @gtkMouseBtnRelease,
{$Ifdef GTK1}
    @gtkDrawAfter,
{$Endif GTK1}
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
  
{$IFDEF GTK1}
function G_OBJECT(p: Pointer): PGtkObject;
function G_CALLBACK(p: Pointer): TGTKSignalFunc;
{$ENDIF}
//--
  
// accelerators
Function DeleteAmpersands(var Str: String): Longint;
function Ampersands2Underscore(Src: PChar): PChar;
function Ampersands2Underscore(const ASource: String): String;
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

// pixmaps
procedure GetGdkPixmapFromGraphic(LCLGraphic: TGraphic;
  var IconImg, IconMask: PGdkPixmap; var Width, Height: integer);
Procedure SetGCRasterOperation(TheGC: PGDKGC; Rop: Cardinal);
Procedure MergeClipping(DestinationDC: TDeviceContext; DestinationGC: PGDKGC;
  X,Y,Width,Height: integer; ClipMergeMask: PGdkPixmap;
  ClipMergeMaskX, ClipMergeMaskY: integer;
  var NewClipMask: PGdkPixmap);
Procedure ResetGCClipping(DC: HDC; GC: PGDKGC);
function ScalePixmap(ScaleGC: PGDKGC;
  SrcPixmap: PGdkPixmap; SrcX, SrcY, SrcWidth, SrcHeight: integer;
  SrcColorMap: PGdkColormap;
  NewWidth, NewHeight: integer;
  var NewPixmap: PGdkPixmap): Boolean;
procedure DrawImageListIconOnWidget(ImgList: TCustomImageList;
  Index: integer; DestWidget: PGTKWidget);
procedure DrawImageListIconOnWidget(ImgList: TCustomImageList;
  Index: integer; DestWidget: PGTKWidget;
  CenterHorizontally, CenterVertically: boolean;
  DestLeft, DestTop: integer);
function GetPGdkImageBitsPerPixel(Image: PGdkImage): cardinal;
{$IfDef Win32}
Procedure gdk_window_copy_area(Dest: PGDKWindow; GC: PGDKGC;
  DestX, DestY: Longint; SRC: PGDKWindow; XSRC, YSRC, Width, Height: Longint);
{$EndIf}
function CreateGdkBitmap(Window: PGdkWindow; Width, Height: integer): PGdkBitmap;
function ExtractGdkBitmap(Bitmap: PGdkBitmap; const SrcRect: TRect): PGdkBitmap;

// menus
function MENU_ITEM_CLASS(widget: PGtkWidget): PGtkMenuItemClass;
function CHECK_MENU_ITEM_CLASS(widget: PGtkWidget): PGtkCheckMenuItemClass;
procedure LockRadioGroupOnChange(RadioGroup: PGSList; const ADelta: Integer);
procedure UpdateRadioGroupChecks(RadioGroup: PGSList);
procedure UpdateInnerMenuItem(LCLMenuItem: TMenuItem;
  MenuItemWidget: PGtkWidget);
function CreateMenuItem(LCLMenuItem: TMenuItem): Pointer;
procedure GetGdkPixmapFromMenuItem(LCLMenuItem: TMenuItem;
  var IconImg, IconMask: PGdkPixmap; var Width, Height: integer);

// statusbar
function CreateStatusBarPanel(StatusBar: TObject; Index: integer): PGtkWidget;
procedure UpdateStatusBarPanels(StatusBar: TObject;
                                StatusBarWidget: PGtkWidget);
procedure UpdateStatusBarPanel(StatusBar: TObject; Index: integer;
                               StatusPanelWidget: PGtkWidget);

// sizing
procedure SaveSizeNotification(Widget: PGtkWidget);
procedure SaveClientSizeNotification(FixWidget: PGtkWidget);
function CreateTopologicalSortedWidgets(HashArray: TDynHashArray): TFPList;
procedure GetGTKDefaultWidgetSize(AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);

Procedure ReportNotObsolete(const Texts: String);

function WaitForClipboardAnswer(c: PClipboardEventData): boolean;
function RequestSelectionData(ClipboardWidget: PGtkWidget;
  ClipboardType: TClipboardType;  FormatID: cardinal): TGtkSelectionData;
procedure FreeClipboardTargetEntries(ClipboardType: TClipboardType);
function GdkAtomToStr(const Atom: TGdkAtom): string;

// forms
function CreateFormContents(AForm: TCustomForm; var FormWidget: Pointer): Pointer;

// styles
function IndexOfStyle(aStyle: TLazGtkStyle): integer;
function IndexOfStyleWithName(const WName: String): integer;
procedure ReleaseAllStyles;
procedure ReleaseStyle(aStyle: TLazGtkStyle);
procedure ReleaseStyleWithName(const WName: String);
function GetStyle(aStyle: TLazGtkStyle): PGTKStyle;
function GetStyleWithName(const WName: String): PGTKStyle;
function GetStyleWidget(aStyle: TLazGtkStyle): PGTKWidget;
function GetStyleWidgetWithName(const WName: String): PGTKWidget;
procedure StyleFillRectangle(drawable: PGDKDrawable; GC: PGDKGC;
                             Color: TColorRef; x, y, width, height: gint);
function StyleForegroundColor(Color: TColorRef; DefaultColor: PGDKColor): PGDKColor;
procedure UpdateWidgetStyleOfControl(AWinControl: TWinControl);

// fonts
function LoadDefaultFont: TGtkIntfFont;
function FontIsDoubleByteCharsFont(TheFont: PGdkFont): boolean;
function FontIsMonoSpaceFont(TheFont: PGdkFont): boolean;
{$Ifdef GTK2}
function FontIsDoubleByteCharsFont(TheFont: PPangoFontDescription): boolean;
function FontIsMonoSpaceFont(TheFont: PPangoFontDescription): boolean;
function LoadDefaultFontDesc: PPangoFontDescription;
procedure GetTextExtentIgnoringAmpersands(FontDesc: PPangoFontDescription; Str: PChar;
  LineLength: Longint; lbearing, rbearing, width, ascent, descent: Pgint);
{$ENDIF}
{$IFDEF GTK1}
procedure GetTextExtentIgnoringAmpersands(FontDesc: PGDKFont; Str: PChar;
  LineLength: Longint; lbearing, rbearing, width, ascent, descent: Pgint);
{$EndIf}
function GetDefaultFontName: string;
procedure FillScreenFonts(ScreenFonts: TStrings);
function GetTextHeight(DCTextMetric: TDevContextTextMetric): integer;
{$IFDEF HasX}
function  XGetWorkarea(var ax,ay,awidth,aheight:gint): gint;
{$ENDIF}


// decoration
Function GetWindowDecorations(AForm: TCustomForm): Longint;
Function GetWindowFunction(AForm: TCustomForm): Longint;

// functions for easier GTK2<->GTK1 Compatibility/Consistency  ---->
function gtk_widget_get_xthickness(Style: PGTKStyle): gint; overload;
function gtk_widget_get_ythickness(Style: PGTKStyle): gint; overload;

function gtk_widget_get_xthickness(Widget: PGTKWidget): gint; overload;
function gtk_widget_get_ythickness(Widget: PGTKWidget): gint; overload;
function GetGtkContainerBorderWidth(Widget: PGtkContainer): gint;

{$Ifdef GTK1}
  type
     PGtkOldEditable = PGtkEditable;

  function gtk_class_get_type(aclass: Pointer): TGtkType;

  //routines to mimic GObject routines/behaviour-->
  procedure g_signal_emit_by_name(anObject:PGtkObject; name:Pgchar;
           args: array of const);
           cdecl; overload; external gtkdll name 'gtk_signal_emit_by_name';
  procedure g_signal_emit_by_name(anObject:PGtkObject; name:Pgchar);
           cdecl; overload; external gtkdll name 'gtk_signal_emit_by_name';

  Procedure g_signal_handlers_destroy(anObject: PGtkObject);
           cdecl; external gtkdll name 'gtk_signal_handlers_destroy';
  Procedure g_signal_stop_emission_by_name(anObject: PGtkObject;
           detailed_signal: Pgchar);
           cdecl; external gtkdll name 'gtk_signal_emit_stop_by_name';
  Function g_signal_connect(anObject: PGtkObject; name: Pgchar;
           func: TGtkSignalFunc; func_data: gpointer): guint;
           cdecl; external gtkdll name 'gtk_signal_connect';
  Function g_signal_connect_after(anObject: PGtkObject; name: Pgchar;
           func: TGtkSignalFunc; func_data: gpointer): guint;
           cdecl; external gtkdll name 'gtk_signal_connect_after';
  Function g_signal_lookup(name: Pgchar; anObject: TGTKType): guint;
           cdecl; external gtkdll name 'gtk_signal_lookup';

  //routines to mimic similar GTK2 routines/behaviour-->
  function gtk_object_get_class(anobject: Pointer): Pointer;
  Function gtk_window_get_modal(window:PGtkWindow):gboolean;
  Function gtk_bin_get_child(bin: PGTKBin): PGTKWidget;
  Procedure gtk_menu_item_set_right_justified(menu_item: PGtkMenuItem;
                                              right_justified: gboolean);
  Function gtk_check_menu_item_get_active(menu_item: PGtkCheckMenuItem): gboolean;
  Procedure gtk_menu_append(menu: PGTKWidget; Item: PGtkWidget);
  Procedure gtk_menu_insert(menu: PGtkWidget; Item: PGTKWidget; Index: gint);
  Procedure gtk_menu_bar_insert(menubar: PGtkWidget; Item: PGTKWidget; Index: gint);
  Function gtk_image_new: PGTKWidget;
  Function gtk_toolbar_new: PGTKWidget;
  Procedure gtk_color_selection_get_current_color(colorsel: PGTKColorSelection;
                                                  Color: PGDKColor);
  Procedure gtk_color_selection_set_current_color(colorsel: PGTKColorSelection;
                                                  Color: PGDKColor);

  //routines to mimic similar GDK2 routines/behaviour-->
  procedure gdk_image_unref(Image: PGdkImage);
  Procedure gdk_colormap_query_color(colormap: PGDKColormap; Pixel: gulong;
                                     Result: PGDKColor);

  //Wrapper around misnamed "regions" routines -->
  Function gdk_region_intersect(source1:PGdkRegion; source2:PGdkRegion): PGdkRegion;
  Function gdk_region_union(source1:PGdkRegion; source2:PGdkRegion): PGdkRegion;
  Function gdk_region_subtract(source1:PGdkRegion; source2:PGdkRegion): PGdkRegion;
  Function gdk_region_xor(source1:PGdkRegion; source2:PGdkRegion): PGdkRegion;
  function gdk_region_copy(region: PGDKRegion): PGDKRegion;
  function gdk_region_rectangle(rect: PGdkRectangle): PGDKRegion;

  //routines to mimic similar GDK2 routines/behaviour-->
  Function gdk_pixmap_create_from_xpm_d (window: PGdkWindow;
                             var mask: PGdkBitmap; transparent_color: PGdkColor;
                             data: PPgchar): PGdkPixmap;
  Function gdk_pixmap_colormap_create_from_xpm_d (window: PGdkWindow;
                       colormap: PGdkColormap; var mask: PGdkBitmap;
                       transparent_color: PGdkColor; data: PPgchar): PGdkPixmap;
  Function gdk_pixmap_colormap_create_from_xpm (window: PGdkWindow;
                    colormap: PGdkColormap; var mask: PGdkBitmap;
                    transparent_color: PGdkColor; filename: Pgchar): PGdkPixmap;

  {$IfNDef NoGdkPixbufLib}
  Procedure gdk_pixbuf_render_pixmap_and_mask(pixbuf: PGdkPixbuf;
    var pixmap_return: PGdkPixmap; var mask_return: PGdkBitmap;
    alpha_threshold: gint);
  {$EndIf}
  
  //Wrapper around window functions like gtk2 -->
  Function gdk_drawable_get_depth(Drawable: PGDKDrawable): gint;
  Procedure gdk_drawable_get_size(Drawable: PGDKDrawable; Width, Height: PGInt);
  Function gdk_drawable_get_image(Drawable: PGDKDrawable;
                                  x, y, width, height: gint): PGdkImage;
  Function gdk_drawable_get_colormap(Drawable: PGDKDrawable): PGdkColormap;
  
  {$IFDEF UseXinerama}
  // Xinerama
  function GetFirstScreen: Boolean;
  {$ENDIF}
var
  FirstScreen: TPoint;
{$EndIF GTK1}

{$Ifdef GTK2}
  function gtk_class_get_type(aclass: Pointer): TGtkType;

  //we wrap our own versions to handle nil tests -->
  function gtk_object_get_class(anobject: Pointer): Pointer;
  Function gtk_window_get_modal(window:PGtkWindow):gboolean;

  //we wrap our own versions to do gtk1 style result = new region -->
  Function gdk_region_union_with_rect(region:PGdkRegion;
                                      rect:PGdkRectangle): PGdkRegion;
  Function gdk_region_intersect(source1:PGdkRegion;
                                source2:PGdkRegion): PGdkRegion;
  Function gdk_region_union(source1:PGdkRegion; source2:PGdkRegion): PGdkRegion;
  Function gdk_region_subtract(source1:PGdkRegion;
                               source2:PGdkRegion): PGdkRegion;
  Function gdk_region_xor(source1:PGdkRegion; source2:PGdkRegion): PGdkRegion;

  //mimic GDKFont Routines With Pango -->
  Procedure gdk_text_extents(FontDesc: PPangoFontDescription; Str: PChar;
        LineLength: Longint; lbearing, rbearing, width, ascent, descent: Pgint);
{$EndIf}

{$IFDEF HasGtkX}
// X functions
function FormToX11Window(const AForm: TCustomForm): X.TWindow;
{$ENDIF}

implementation


uses
  {$IFDEF StaticXinerama} Xinerama, {$ENDIF}
  dynlibs;

const
  VKEY_FLAG_SHIFT    = $01;
  VKEY_FLAG_CTRL     = $02;
  VKEY_FLAG_ALT      = $04;
  VKEY_FLAG_KEY_MASK = $07;
  VKEY_FLAG_EXT      = $10; // extended key
  VKEY_FLAG_MULTI_VK = $20; // key has more than one VK


type
  PVKeyRecord = ^TVKeyRecord;
  TVKeyRecord = record
    VKey: Byte;
    Flags: Byte; // indicates if Alt | Ctrl | Shift is needed
                 // extended state
  end;
  
var
  MKeyCodeToVK: array[Byte] of Byte;
  MVKeyInfo: array[Byte] of TVKeyInfo;
  MKeySymToVKMap: TMap;  // keysym ->TVKeyRecord
  MSymCharToVKMap: TMap; //char->TVKeyRecord
  
type
  // TLCLHandledKeyEvent is used to remember, if an gdk key event was already
  // handled.
  TLCLHandledKeyEvent = class
  public
    thetype: TGdkEventType;
    window: PGdkWindow;
    send_event: gint8;
    time: guint32;
    constructor Create(Event: PGdkEventKey);
    function IsEqual(Event: PGdkEventKey): boolean;
  end;

{ TLCLHandledKeyEvent }

constructor TLCLHandledKeyEvent.Create(Event: PGdkEventKey);
begin
  thetype:=gdk_event_get_type(Event);
  window:=Event^.window;
  send_event:=Event^.send_event;
  time:=Event^.time;
end;

function TLCLHandledKeyEvent.IsEqual(Event: PGdkEventKey): boolean;
begin
  Result:=(gdk_event_get_type(Event)=thetype)
      and (window=Event^.window)
      and (send_event=Event^.send_event)
      and (time=Event^.time);
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
    gtk_widget_set_name(AWidget,PChar(RCName));
    //debugln('Set_RC_Name ',GetWidgetDebugReport(AWidget),' RCName="',RCName,'"');
    gtk_widget_set_rc_style(AWidget);
  end;
end;

{$I gtkproc.inc}
{$I gtkcallback.inc}

procedure InitGTKProc;
var
  lgs: TLazGtkStyle;
begin
  MKeySymToVKMap := TMap.Create(itu4, SizeOf(TVKeyRecord));
  // UTF8 is max 4 bytes, acombined makes it 8
  MSymCharToVKMap := TMap.Create(itu8, SizeOf(TVKeyRecord));


  FillChar(MKeyCodeToVK, SizeOf(MKeyCodeToVK), $FF);
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
  FreeAndNil(MKeySymToVKMap);
  FreeAndNil(MSymCharToVKMap);
end;

{$IFDEF GTK1}
function GDK_GET_CURRENT_DESKTOP(): gint;
var
  XDisplay: PDisplay;
  XScreen: PScreen;
  XWindow: TWindow;
  AtomType: x.TAtom;
  Format: gint;
  nitems: gulong;
  bytes_after: gulong;
  current_desktop: pguint;
begin
  Result := -1;

  xdisplay := gdk_display;
  xscreen := XDefaultScreenOfDisplay(xdisplay);
  xwindow := XRootWindowOfScreen(xscreen);

  XGetWindowProperty (xdisplay, xwindow,
             XInternAtom(xdisplay, '_NET_CURRENT_DESKTOP', false),
             0, MaxInt, False, XA_CARDINAL, @atomtype, @format, @nitems,
             @bytes_after, gpointer(@current_desktop));

  if (atomtype = XA_CARDINAL) and (format = 32) and  (nitems > 0) then
  begin
    Result := current_desktop[0];
    XFree (current_desktop);
  end;
end;


function GDK_WINDOW_GET_DESKTOP(Window: PGdkWindowPrivate): gint;
var
  xdisplay: PDisplay;
  xwindow: TWindow;

  atomtype: x.TAtom;
  format: gint;
  nitems: gulong;
  bytes_after: gulong;
  current_desktop: pguint;
begin
  Result := -1;
  XWindow := GDK_WINDOW_XWINDOW (Window);
  XDisplay := GDK_WINDOW_XDISPLAY (Window);
  XGetWindowProperty (xdisplay, xwindow,
             XInternAtom(xdisplay, '_NET_WM_DESKTOP', false),
             0, MaxInt, False, XA_CARDINAL, @atomtype, @format, @nitems,
             @bytes_after, gpointer(@current_desktop));

  if (atomtype = XA_CARDINAL) and (format = 32) and  (nitems > 0) then
  begin
    Result := current_desktop[0];
    XFree (current_desktop);
  end;
end;

function GDK_WINDOW_SET_DESKTOP(Window: PGdkWindowPrivate; Desktop: gint): gint;
var
  XDisplay: PDisplay;
  XScreen: PScreen;
  XRootWindow,
  XWindow: TWindow;
  XEvent: TXClientMessageEvent;
  _NET_WM_DESKTOP: Integer;
begin

  Result := -1;

  XDisplay := GDK_WINDOW_XDISPLAY (Window);
  XScreen := XDefaultScreenOfDisplay(xdisplay);
  XRootWindow := XRootWindowOfScreen(xscreen);
  XWindow := GDK_WINDOW_XWINDOW (Window);

  _NET_WM_DESKTOP := XInternAtom(xdisplay, '_NET_WM_DESKTOP', false);

  XEvent._type := ClientMessage;
  XEvent.window := XWindow;
  XEvent.message_type := _NET_WM_DESKTOP;
  XEvent.format := 32;
  XEvent.data.l[0] := Desktop;

  XSendEvent(XDisplay, XRootWindow, False, SubstructureNotifyMask, PXEvent(@XEvent));
end;


procedure GDK_WINDOW_ACTIVATE(Window: PGdkWindowPrivate);
var
  XDisplay: PDisplay;
  XScreen: PScreen;
  aXRootWindow,
  XWindow: x.TWindow;
  XEvent: xlib.TXClientMessageEvent;
  _NET_ACTIVE_WINDOW: Integer;
begin
  if (Window=nil) or (gdk.destroyed(Window^)<>0) then exit;

  XDisplay := GDK_WINDOW_XDISPLAY (Window);
  if XDisplay=nil then exit;
  XScreen := XDefaultScreenOfDisplay(xdisplay);
  if XScreen=nil then exit;
  aXRootWindow := XRootWindowOfScreen(xscreen);
  if aXRootWindow=0 then exit;
  XWindow := GDK_WINDOW_XWINDOW (Window);
  if XWindow=0 then exit;

  _NET_ACTIVE_WINDOW := XInternAtom(xdisplay, '_NET_ACTIVE_WINDOW', false);

  XEvent._type := ClientMessage;
  XEvent.window := XWindow;
  XEvent.message_type := _NET_ACTIVE_WINDOW;
  XEvent.format := 32;
  XEvent.data.l[0] := 1; //Message is from program
  XEvent.data.l[1] := CurrentTime;
  XEvent.data.l[2] := 0; // Applications current active window

  XSendEvent(XDisplay, aXRootWindow, False, SubstructureNotifyMask, PXEvent(@XEvent));
end;

procedure GDK_WINDOW_MAXIMIZE(Window: PGdkWindowPrivate);
const
  _NET_WM_STATE_REMOVE    =    0;   // remove/unset property
  _NET_WM_STATE_ADD       =    1;   // add/set property
  _NET_WM_STATE_TOGGLE    =    2;   // toggle property
var
  XDisplay: PDisplay;
  XScreen: PScreen;
  aXRootWindow,
  XWindow: TWindow;
  XEvent: TXClientMessageEvent;
  _NET_WM_STATE,
  _NET_WM_STATE_MAXIMIZED_VERT,
  _NET_WM_STATE_MAXIMIZED_HORZ: Integer;

begin
  XDisplay := GDK_WINDOW_XDISPLAY (Window);
  XScreen := XDefaultScreenOfDisplay(xdisplay);
  aXRootWindow := XRootWindowOfScreen(xscreen);
  XWindow := GDK_WINDOW_XWINDOW (Window);

  _NET_WM_STATE := XInternAtom(xdisplay, '_NET_WM_STATE', false);
  _NET_WM_STATE_MAXIMIZED_VERT := XInternAtom(xdisplay, '_NET_WM_STATE_MAXIMIZED_VERT', false);
  _NET_WM_STATE_MAXIMIZED_HORZ := XInternAtom(xdisplay, '_NET_WM_STATE_MAXIMIZED_HORZ', false);

  XEvent._type := ClientMessage;
  XEvent.window := XWindow;
  XEvent.message_type := _NET_WM_STATE;
  XEvent.format := 32;
  XEvent.data.l[0] := _NET_WM_STATE_ADD;
  XEvent.data.l[1] := _NET_WM_STATE_MAXIMIZED_HORZ;
  XEvent.data.l[2] := _NET_WM_STATE_MAXIMIZED_VERT;

  XSendEvent(XDisplay, aXRootWindow, False, SubstructureNotifyMask, PXEvent(@XEvent));
end;



procedure GDK_WINDOW_MINIMIZE(Window: PGdkWindowPrivate);
const
  _NET_WM_STATE_REMOVE    =    0;   // remove/unset property
  _NET_WM_STATE_ADD       =    1;   // add/set property
  _NET_WM_STATE_TOGGLE    =    2;   // toggle property
var
  XDisplay: PDisplay;
  XScreen: PScreen;
  XWindow: x.TWindow;
  _NET_WM_STATE,
  _NET_WM_STATE_HIDDEN: Integer;
  atomtype: x.TAtom;
  format: gint;
  nitems: gulong;
  bytes_after: gulong;
  windowstates: Pcuchar;
  X: Integer;

begin
  XDisplay := GDK_WINDOW_XDISPLAY (Window);
  XScreen := XDefaultScreenOfDisplay(xdisplay);
  XWindow := GDK_WINDOW_XWINDOW (Window);

  _NET_WM_STATE := XInternAtom(xdisplay, '_NET_WM_STATE', false);
  _NET_WM_STATE_HIDDEN := XInternAtom(xdisplay, '_NET_WM_STATE_HIDDEN', false);
  
  XGetWindowProperty (xdisplay, xwindow, _NET_WM_STATE             ,
             0, MaxInt, False, XA_CARDINAL, @atomtype, @format, @nitems,
             @bytes_after, @windowstates);
  if (atomtype = XA_CARDINAL) and (format = 32) and  (nitems > 0) then
  begin
    // Check to see if the window is already minimized...
    for X := 0 to nitems do begin
      if windowstates[X] = _NET_WM_STATE_HIDDEN then begin
        XFree (windowstates);
        exit;
      end;
    end;
    XFree (windowstates);
  end;
  
  XIconifyWindow(XDisplay, XWindow, XScreenNumberOfScreen(XScreen));
end;

function GDK_WINDOW_GET_MAXIMIZED(Window: PGdkWindowPrivate): gboolean;
var
  xdisplay: PDisplay;
  xwindow: TWindow;

  atomtype: x.TAtom;
  format: gint;
  nitems: gulong;
  bytes_after: gulong;
  state_array: pguint;
  _NET_WM_STATE,
  _NET_WM_STATE_MAXIMIZED_VERT,
  _NET_WM_STATE_MAXIMIZED_HORZ: x.TAtom;
  X: Integer;
begin
  Result := False;
  XWindow := GDK_WINDOW_XWINDOW (Window);
  XDisplay := GDK_WINDOW_XDISPLAY (Window);
  
  _NET_WM_STATE := XInternAtom(xdisplay, '_NET_WM_STATE', false);
  _NET_WM_STATE_MAXIMIZED_VERT := XInternAtom(xdisplay, '_NET_WM_STATE_MAXIMIZED_VERT', false);
  _NET_WM_STATE_MAXIMIZED_HORZ := XInternAtom(xdisplay, '_NET_WM_STATE_MAXIMIZED_HORZ', false);

  XGetWindowProperty (xdisplay, xwindow,
             _NET_WM_STATE,
             0, MaxInt, False, XA_ATOM, @atomtype, @format, @nitems,
             @bytes_after, gpointer(@state_array));

  if (atomtype = XA_ATOM) and (format = 32) and  (nitems > 0) then
  begin
    for X := 0 to nitems-1 do begin
      if
      (state_array[X] = _NET_WM_STATE_MAXIMIZED_VERT)
      or
      (state_array[X] = _NET_WM_STATE_MAXIMIZED_HORZ)
      then Result := True;
      
      if Result then Break;
    end;
    XFree (state_array);
  end;
end;

procedure GDK_WINDOW_SHOW_IN_TASKBAR(Window: PGdkWindowPrivate; Show: Boolean);
// this is a try to hide windows from the taskbar.
// Unpleasantly, some windowmangers like metacity also hides form the Alt-Tab
// cycle.
// This feature is therefore disabled on default.
{$IFDEF EnableHideFromTaskBar}
var
  XDisplay: PDisplay;
  XScreen: PScreen;
  XRootWindow,
  XWindow: TWindow;
  XEvent: TXClientMessageEvent;
  _NET_WM_STATE,
  _NET_WM_STATE_SKIP_TASKBAR: clong;
{$ENDIF}
begin
  {$IFDEF EnableHideFromTaskBar}
  // GTK1: reshowing does not work, so a modal form will hide the whole application
  // GTK

  XDisplay := GDK_WINDOW_XDISPLAY (Window);
  XScreen := XDefaultScreenOfDisplay(xdisplay);
  XRootWindow := XRootWindowOfScreen(xscreen);
  XWindow := GDK_WINDOW_XWINDOW (Window);

  _NET_WM_STATE := XInternAtom(xdisplay, '_NET_WM_STATE', false);
  _NET_WM_STATE_SKIP_TASKBAR := XInternAtom(xdisplay, '_NET_WM_STATE_SKIP_TASKBAR', false);

  XEvent._type := ClientMessage;
  XEvent.window := XWindow;
  XEvent.message_type := _NET_WM_STATE;
  XEvent.format := 32;
  if Show then
    XEvent.data.l[0] := 1
  else
    XEvent.data.l[0] := 0;// 0=Remove 1=Add 2=Toggle
  XEvent.data.l[1] := _NET_WM_STATE_SKIP_TASKBAR;

  XSendEvent(XDisplay, XRootWindow, False, SubstructureNotifyMask, @XEvent);
  {$ENDIF}
end;


{$ENDIF}

initialization
  InitGTKProc;

finalization
  DoneGTKProc;

end.
