{ $Id$
                  ----------------------------------
                  gtkproc.pp  -  gtk interface procs
                  ----------------------------------

 @lastmod($Date$)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)

 This unit contains procedures/functions needed for the gtk <-> LCL interface
}
{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit GTKProc;

{$mode objfpc}{$H+}

interface

{$I gtkdefines.inc}

uses
  {$IFDEF windows}
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
    {$ifdef HasGdk2X}
    gdk2x,
    {$endif}
  {$ELSE}
    glib, gdk, gtk, gdkpixbuf,
  {$ENDIF}
  Math, // Math after gtk to get the correct Float type
  Masks, LazUTF8, FileUtil, LazFileUtils, LazStringUtils, DynHashArray,
  LMessages, LCLMessageGlue, LCLProc, LCLStrConsts, LCLIntf, LCLType,
  GraphType, GraphMath, Graphics, Controls, Forms, Menus,
  StdCtrls, ComCtrls, ExtCtrls, Dialogs, ExtDlgs,
  ImgList, GtkFontCache, GTKGlobals, GtkDef, GtkExtra, GtkDebug;

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
{$Ifdef GTK1}
function gtkDrawCB(Widget: PGtkWidget; area: PGDKRectangle;
  data: gPointer): GBoolean; cdecl;
function gtkDrawAfterCB(Widget: PGtkWidget; area: PGDKRectangle;
  data: gPointer): GBoolean; cdecl;
{$EndIf}
function gtkExposeEvent(Widget: PGtkWidget; Event: PGDKEventExpose;
  Data: gPointer): GBoolean; cdecl;
function gtkExposeEventAfter(Widget: PGtkWidget; Event: PGDKEventExpose;
  Data: gPointer): GBoolean; cdecl;
function gtkfrmactivateAfter( widget: PGtkWidget; Event: PgdkEventFocus;
  data: gPointer): GBoolean; cdecl;
function gtkfrmdeactivateAfter( widget: PGtkWidget; Event: PgdkEventFocus;
  data: gPointer): GBoolean; cdecl;
function GTKMap(Widget: PGTKWidget; Data: gPointer): GBoolean; cdecl;

function GTKKeyPress(Widget: PGtkWidget; Event: pgdkeventkey;
  Data: gPointer): GBoolean; cdecl;
function GTKKeyPressAfter(Widget: PGtkWidget; Event: pgdkeventkey;
  Data: gPointer): GBoolean; cdecl;
function GTKKeyRelease(Widget: PGtkWidget; Event: pgdkeventkey;
  Data: gPointer): GBoolean; cdecl;
function GTKKeyReleaseAfter(Widget: PGtkWidget; Event: pgdkeventkey;
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
procedure DestroyWindowFromPointCB(Widget: PGtkWidget; data: gPointer); cdecl;
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
function DeliverMouseUpMessage(widget: PGtkWidget; event: pgdkEventButton;
                                AWinControl: TWinControl): boolean;
function gtkMouseBtnRelease(widget: PGtkWidget; event: pgdkEventButton;
                            data: gPointer): GBoolean; cdecl;
function gtkMouseBtnReleaseAfter(widget: PGtkWidget; event: pgdkEventButton;
                                 data: gPointer): GBoolean; cdecl;
function gtkclickedCB( widget: PGtkWidget; data: gPointer): GBoolean; cdecl;

function gtkEnterCB(widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtkLeaveCB(widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
function gtksize_allocateCB(widget: PGtkWidget; size :pGtkAllocation;
  data: gPointer): GBoolean; cdecl;
function gtksize_allocate_client(widget: PGtkWidget; size :pGtkAllocation;
  data: gPointer): GBoolean; cdecl;
function gtkconfigureevent( widget: PGtkWidget; event: PgdkEventConfigure;
  data: gPointer): GBoolean; cdecl;
function gtkInsertText(widget: PGtkWidget; char: pChar; NewTextLength:
                     Integer; Position: pgint; data: gPointer): GBoolean; cdecl;
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

// PGtkAdjustment cb
function GTKHScrollCB(Adjustment: PGTKAdjustment; data: GPointer): GBoolean; cdecl;
function GTKVScrollCB(Adjustment: PGTKAdjustment;
  data: GPointer): GBoolean; cdecl;

{$ifdef gtk2}
// PGtkRange cb
function Gtk2RangeScrollCB(ARange: PGtkRange; AScrollType: TGtkScrollType;
  AValue: gdouble; AWidgetInfo: PWidgetInfo): gboolean; cdecl;
{$endif}

function GTKCheckMenuToggeledCB(AMenuItem: PGTKCheckMenuItem;
                                AData: gPointer): GBoolean; cdecl;
function GTKKeySnooper(Widget: PGtkWidget; Event: PGdkEventKey;
  FuncData: gPointer): gInt; cdecl;
function gtkYearChanged(Widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
procedure GtkDragDataReceived(widget:PGtkWidget; context:PGdkDragContext;
  x:gint; y:gint; selection_data:PGtkSelectionData; info:guint; time:guint; Data: gPointer);cdecl;


// clipboard
procedure ClipboardSelectionReceivedHandler(TargetWidget: PGtkWidget;
  SelectionData: PGtkSelectionData; TimeID: guint32; Data: Pointer); cdecl;
procedure ClipboardSelectionRequestHandler(TargetWidget: PGtkWidget;
  SelectionData: PGtkSelectionData; Info: cardinal; TimeID: cardinal;
  Data: Pointer); cdecl;
function ClipboardSelectionLostOwnershipHandler(TargetWidget: PGtkWidget;
  EventSelection: PGdkEventSelection;  Data: Pointer): cardinal; cdecl;

procedure GTKStyleChanged(Widget: PGtkWidget; previous_style :
  PGTKStyle; Data: Pointer); cdecl;
function gtkListBoxSelectionChangedAfter(widget: PGtkWidget;
                                      data: gPointer): GBoolean; cdecl;

// gtkDragCallback.inc headers
function edit_drag_data_received(widget: pgtkWidget;
             Context: pGdkDragContext;
             X: Integer;
             Y: Integer;
             seldata: pGtkSelectionData;
             info: Integer;
             time: Integer;
                                  data: pointer): GBoolean; cdecl;
function edit_source_drag_data_get(widget: pgtkWidget;
             Context: pGdkDragContext;
             Selection_data: pGtkSelectionData;
             info: Integer;
             time: Integer;
                                  data: pointer): GBoolean; cdecl;
function Edit_source_drag_data_delete (widget: pGtkWidget;
                   context: pGdkDragContext;
                   data: gpointer): gBoolean ; cdecl;

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

function RectFromGdkRect(const AGdkRect: TGdkRectangle): TRect;
function GdkRectFromRect(const R: TRect): TGdkRectangle;
function AlignToGtkAlign(Align: TAlignment): gfloat;
{$ifdef gtk2}
function GtkScrollTypeToScrollCode(ScrollType: TGtkScrollType): LongWord;
{$endif}

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
//function CreatePChar(const s: string): PChar;
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
procedure SetHiddenLCLObject(const Widget: Pointer; const AnObject: TObject);
function GetHiddenLCLObject(const Widget: Pointer): TObject;
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
procedure ReleaseKeyBoardFromForm(AForm: TCustomForm);
procedure GrabMouseToForm(AForm: TCustomForm);
procedure ReleaseMouseFromForm(AForm: TCustomForm);

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
procedure AllocGDIColor(DC: hDC; GDIColor: PGDIColor);
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
function GetSysGCValues(Color: TColorRef; ThemeWidget: PGtkWidget): TGDKGCValues;

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

procedure InitKeyboardTables;
procedure DoneKeyboardTables;
function GetVKeyInfo(const AVKey: Byte): TVKeyInfo;
function GTKEventStateToShiftState(KeyState: Word): TShiftState;
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
function SubtractScoll(AWidget: PGtkWidget; APosition: TPoint): TPoint;

// mouse capturing
procedure CaptureMouseForWidget(Widget: PGtkWidget; Owner: TMouseCaptureType);
function GetDefaultMouseCaptureWidget(Widget: PGtkWidget): PGtkWidget;
procedure ReleaseMouseCapture;
procedure ReleaseCaptureWidget(Widget : PGtkWidget);
procedure UpdateMouseCaptureControl;


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
    @gtkDrawAfterCB,
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
  
//--
  
// accelerators
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

function CreatePixbufFromDrawable(ASource: PGdkDrawable; AColorMap:PGdkColormap; AIncludeAplha: Boolean; ASrcX, ASrcY, ADstX, ADstY, AWidth, AHeight :longint): PGdkPixbuf;


// pixmaps
procedure SetGCRasterOperation(TheGC: PGDKGC; Rop: Cardinal);
procedure MergeClipping(DestinationDC: TGtkDeviceContext; DestinationGC: PGDKGC;
  X,Y,Width,Height: integer; ClipMergeMask: PGdkBitmap;
  ClipMergeMaskX, ClipMergeMaskY: integer;
  var NewClipMask: PGdkBitmap);
function CreatePixbufFromImageAndMask(ASrc: PGdkDrawable; ASrcX, ASrcY, ASrcWidth,
  ASrcHeight: integer; ASrcColorMap: PGdkColormap; ASrcMask: PGdkBitmap): PGdkPixbuf;
function ScalePixmapAndMask(AScaleGC: PGDKGC; AScaleMethod: TGdkInterpType;
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
function CreateStatusBarPanel(StatusBar: TObject; Index: integer): PGtkWidget;
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
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
procedure SendCachedGtkResizeNotifications;
procedure ResizeHandle(LCLControl: TWinControl);
procedure SetWidgetSizeAndPosition(LCLControl: TWinControl);// for child controls
procedure SetWindowSizeAndPosition(Window: PGtkWindow; AWinControl: TWinControl);// for top level control
procedure GetWidgetRelativePosition(aWidget: PGtkWidget; var Left, Top: integer);
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
function CreateFormContents(AForm: TCustomForm;
  var FormWidget: Pointer; AWidgetInfo: PWidgetInfo = nil): Pointer;

// styles
type
  PStyleObject = ^TStyleObject;
  TStyleObject = Record
    Obj: PGtkObject;
    Style: PGTKStyle;
    Widget: PGTKWidget;
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
{$IFDEF Gtk2}
function GetStyleFormFrameBorders(WithMenu: boolean): TRect;
{$ENDIF}
procedure StyleFillRectangle(drawable: PGDKDrawable; GC: PGDKGC;
                             Color: TColorRef; x, y, width, height: gint);
function StyleForegroundColor(Color: TColorRef; DefaultColor: PGDKColor): PGDKColor;
procedure UpdateWidgetStyleOfControl(AWinControl: TWinControl);

// fonts
function LoadDefaultFont: TGtkIntfFont;
function FontIsDoubleByteCharsFont(TheFont: TGtkIntfFont): boolean;
function FontIsMonoSpaceFont(TheFont: TGtkIntfFont): boolean;
{$Ifdef GTK2}
function LoadDefaultFontDesc: PPangoFontDescription;
{$ENDIF}
procedure GetTextExtentIgnoringAmpersands(TheFont: TGtkIntfFont;
  Str: PChar; StrLength: integer;
  lbearing, rbearing, width, ascent, descent: Pgint);
function GetDefaultFontName: string;
procedure FillScreenFonts(ScreenFonts: TStrings);
function GetTextHeight(DCTextMetric: TDevContextTextMetric): integer;
{$IFDEF HasX}
function  XGetWorkarea(var ax,ay,awidth,aheight:gint): gint;
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

{$Ifdef GTK2}
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
{$EndIf}

{$ifdef HasX}
// X functions
function FormToX11Window(const AForm: TCustomForm): X.TWindow;
{$endif}
function FindFocusWidget(AWidget: PGtkWidget): PGtkWidget;

implementation


uses
  {$IFDEF StaticXinerama} Xinerama, {$ENDIF}
  dynlibs, GtkWSPrivate, URIParser, GtkInt;

const
  KCINFO_FLAG_SHIFT         = $01;
  KCINFO_FLAG_CTRL          = $02;
  KCINFO_FLAG_ALTGR         = $04;
  KCINFO_FLAG_KEY_MASK      = $07;
  KCINFO_FLAG_EXT           = $10; // extended key
  KCINFO_FLAG_TOGGLE        = $20; // toggle key

  KCINFO_FLAG_SHIFT_XOR_NUM = $40; // second vkey should be used when numlock <>shift
  KCINFO_FLAG_MULTI_MASK    = $C0; // key has more than one VK


type
  PKeyCodeInfo = ^TKeyCodeInfo;
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
  // Shift is set when the mask for the Keymapkeys_return[index] is set
var
  MKeyStateMap: array of record
    Index: Byte;
    Mask: Byte;
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
  //MKeySymToVKMap := TMap.Create(itu4, SizeOf(TVKeyRecord));
  // UTF8 is max 4 bytes, acombined makes it 8
  //MSymCharToVKMap := TMap.Create(itu8, SizeOf(TVKeyRecord));
  
  // fill initial modifier list
  FillByte(MModifiers, SizeOf(MModifiers), 0);
  // keyboard
  MModifiers[ssCaps].Mask     := GDK_LOCK_MASK;
  MModifiers[ssNum].Mask      := GDK_MOD3_MASK; //todo: check this I've 2 here,but 3 was the original code
  MModifiers[ssScroll].Mask   := GDK_MOD5_MASK; //todo: check this I've ssAltGr here, but ssScroll was the original code
  {$ifndef UseOwnShiftState}
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


initialization
  InitGTKProc;

finalization
  DoneGTKProc;

end.
