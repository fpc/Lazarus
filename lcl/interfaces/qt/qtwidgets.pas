{
 *****************************************************************************
 *                              QtWidgets.pas                                *
 *                              --------------                               *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

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
unit qtwidgets;

{$mode delphi}{$H+}

interface

uses
  // Bindings
{$ifdef USE_QT_4_3}
  qt43,
{$else}
  qt4,
{$endif}
  qtobjects,
  // Free Pascal
  Classes, SysUtils, Types,
  // LCL
  LCLType, LCLProc, LCLIntf, LMessages, Buttons, Forms, Controls, ComCtrls, CommCtrl,
  ExtCtrls, StdCtrls, Menus;

type
  TPaintData = record
    ClipRect: Prect;
    ClipRegion: QRegionH;
  end;
  
type
  { TQtWidget }

  TQtWidget = class(TObject)
  private
    FProps: TStringList;
    FPaintData: TPaintData;
    function GetProps(const AnIndex: String): pointer;
    function QtKeyToLCLKey(key: Integer): Word;
    procedure DeliverMessage(var Msg);
    procedure SetProps(const AnIndex: String; const AValue: pointer);
  protected
    function CreateWidget(const Params: TCreateParams):QWidgetH; virtual;
    procedure SetGeometry; virtual;
  public
    AVariant: QVariantH;
    Widget: QWidgetH;
    LCLObject: TWinControl;
  public
    constructor Create(const AWinControl: TWinControl; const AParams: TCreateParams); virtual;
    constructor CreatePage(const AWinControl: TWinControl; const AParams: TCreateParams);
    destructor Destroy; override;
    function  GetContainerWidget: QWidgetH; virtual;
  public
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl; virtual;
    procedure SlotShow(vShow: Boolean); cdecl;
    procedure SlotClose; cdecl;
    procedure SlotDestroy; cdecl;
    procedure SlotFocus(FocusIn: Boolean); cdecl;
    procedure SlotKey(Event: QEventH); cdecl;
    procedure SlotMouse(Event: QEventH); cdecl;
    procedure SlotMouseMove(Event: QEventH); cdecl;
    procedure SlotMouseWheel(Event: QEventH); cdecl;
    procedure SlotPaint(Event: QEventH); cdecl;
    procedure SlotResize; cdecl;
    procedure SlotContextMenu; cdecl;
  public
    procedure SetColor(const Value: PQColor); virtual;
    procedure SetTextColor(const Value: PQColor); virtual;
    procedure SetCursor(const ACursor: QCursorH); virtual;
    procedure Update;
    procedure Repaint;
    procedure setWindowTitle(Str: PWideString);
    procedure WindowTitle(Str: PWideString);
    procedure Hide;
    procedure Show;
    function getVisible: boolean;
    procedure setEnabled(p1: Boolean);
    procedure setVisible(visible: Boolean);
    function windowModality: QtWindowModality;
    procedure setWindowModality(windowModality: QtWindowModality);
    procedure setParent(parent: QWidgetH);
    procedure setWindowFlags(_type: QtWindowFlags);
    function windowFlags: QtWindowFlags;
    procedure setWidth(p1: Integer);
    procedure setHeight(p1: Integer);
    procedure setTabOrder(p1, p2: TQtWidget);
    
    property Props[AnIndex:String]:pointer read GetProps write SetProps;
    property PaintData: TPaintData read FPaintData write FPaintData;
  end;

  { TQtFrame }

  TQtFrame = class(TQtWidget)
  private
  protected
    function CreateWidget(const AParams: TCreateParams):QWidgetH; override;
  public
    destructor Destroy; override;
    procedure setFrameStyle(p1: Integer);
    procedure setFrameShape(p1: QFrameShape);
    procedure setFrameShadow(p1: QFrameShadow);
    procedure setTextColor(const Value: PQColor); override;
  end;

  { TQtAbstractButton }

  TQtAbstractButton = class(TQtWidget)
  private
  public
    procedure SetColor(const Value: PQColor); override;
    procedure SetText(text: PWideString);
    procedure Text(retval: PWideString);
    function  isChecked: Boolean;
    procedure setChecked(p1: Boolean);
    procedure SignalPressed; cdecl;
    procedure SignalReleased; cdecl;
    procedure SignalClicked(Checked: Boolean = False); cdecl;
    procedure SignalClicked2; cdecl;
    procedure SignalToggled(Checked: Boolean); cdecl;
  end;

  { TQtPushButton }

  TQtPushButton = class(TQtAbstractButton)
  private
  protected
    function CreateWidget(const AParams: TCreateParams): QWidgetH; override;
  public
    destructor Destroy; override;
    procedure SlotClicked; cdecl;
  end;

  { TQtMainWindow }

  TQtMenuBar = class;
  TQtToolBar = class;

  TQtMainWindow = class(TQtWidget)
  private
  protected
    function CreateWidget(const AParams: TCreateParams):QWidgetH; override;
  public
    LayoutWidget: QBoxLayoutH;
{$ifdef USE_QT_4_3}
    MDIAreaHandle: QMDIAreaH;
{$endif}
    CentralWidget: QWidgetH;
    Splitter: QSplitterH;
    MenuBar: TQtMenuBar;
    ToolBar: TQtToolBar;
    Canvas: TQtDeviceContext;
    destructor Destroy; override;
    function GetContainerWidget: QWidgetH; override;
    procedure setTabOrders;
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl; override;
    procedure SlotWindowStateChange; cdecl;
    procedure setShowInTaskBar(AValue: Boolean);
  end;

  { TQtStaticText }

  TQtStaticText = class(TQtFrame)
  private
  protected
    function CreateWidget(const AParams: TCreateParams):QWidgetH; override;
  public
    destructor Destroy; override;
    procedure SetText(text: PWideString);
    procedure Text(retval: PWideString);
  end;

  { TQtTimer }

  TQtTimer = class(TQtWidget)
  private
    CallbackFunc: TFNTimerProc;
    Id: Integer;
    AppObject: QObjectH;
  public
    constructor CreateTimer(Interval: integer; const TimerFunc: TFNTimerProc; App: QObjectH); virtual;
    destructor Destroy; override;
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl; override;
  end;

  { TQtCheckBox }

  TQtCheckBox = class(TQtAbstractButton)
  protected
    function CreateWidget(const AParams: TCreateParams):QWidgetH; override;
    procedure SetGeometry; override;
  public
    destructor Destroy; override;
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl; override;
    function CheckState: QtCheckState;
    procedure setCheckState(state: QtCheckState);
    procedure signalStateChanged(p1: Integer); cdecl;
  end;

  { TQtRadioButton }

  TQtRadioButton = class(TQtAbstractButton)
  protected
    function CreateWidget(const AParams: TCreateParams):QWidgetH; override;
    procedure SetGeometry; override;
  public
    destructor Destroy; override;
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl; override;
  end;

  { TQtGroupBox }

  TQtGroupBox = class(TQtWidget)
  protected
    function CreateWidget(const AParams: TCreateParams):QWidgetH; override;
  private
  public
    destructor Destroy; override;
  public
    ButtonGroup: TQtButtonGroup;
    BoxLayout: QGridLayoutH;
  end;

  { TQtAbstractSlider , inherited by TQtScrollBar, TQtTrackBar }

  TQtAbstractSlider = class(TQtWidget)
  private
    FSliderPressed: Boolean;
    FSliderReleased: Boolean;
  protected
    function CreateWidget(const AParams: TCreateParams):QWidgetH; override;
  public
    destructor Destroy; override;

    procedure setInvertedAppereance(p1: Boolean); virtual;
    procedure setInvertedControls(p1: Boolean); virtual;

    procedure setMaximum(p1: Integer); virtual;
    procedure setMinimum(p1: Integer); virtual;

    procedure setOrientation(p1: QtOrientation); virtual;
    procedure setPageStep(p1: Integer); virtual;
    procedure setRange(minimum: Integer; maximum: Integer); virtual;
    procedure setSingleStep(p1: Integer); virtual;
    procedure setSliderDown(p1: Boolean); virtual;
    procedure setSliderPosition(p1: Integer); virtual;
    procedure setTracking(p1: Boolean); virtual;
    procedure setValue(p1: Integer); virtual; 
    procedure SlotSliderMoved(p1: Integer); cdecl; virtual;
    procedure SlotValueChanged(p1: Integer); cdecl; virtual;
    procedure SlotRangeChanged(minimum: Integer; maximum: Integer); cdecl; virtual;
    procedure SlotSliderPressed; cdecl;
    procedure SlotSliderReleased; cdecl;
    property SliderPressed: Boolean read FSliderPressed;
    property SliderReleased: Boolean read FSliderReleased;
  end;

  { TQtScrollBar }
  
  TQtScrollBar = class(TQtAbstractSlider)
  private
  protected
    function CreateWidget(const AParams: TCreateParams):QWidgetH; override;
  end;	

  { TQtToolBar }
  
  TQtToolBar = class(TQtWidget)
  private
  protected
    function CreateWidget(const AParams: TCreateParams):QWidgetH; override;
  end;
  
  { TQtToolButton }

  TQtToolButton = class(TQtAbstractButton)
  private
  protected
    function CreateWidget(const AParams: TCreateParams):QWidgetH; override;
  end;
  
  { TQtTrackBar }
  
  TQtTrackBar = class(TQtAbstractSlider)
  private
  protected
    function CreateWidget(const AParams: TCreateParams):QWidgetH; override;
  public
    procedure SetTickPosition(Value: QSliderTickPosition);
    procedure SetTickInterval(Value: Integer);
    procedure SlotSliderMoved(p1: Integer); cdecl; override;
    procedure SlotValueChanged(p1: Integer); cdecl; override;
    procedure SlotRangeChanged(minimum: Integer; maximum: Integer); cdecl; override;
  end;

  { TQtLineEdit }

  TQtLineEdit = class(TQtWidget)
  private
  protected
    function CreateWidget(const AParams: TCreateParams):QWidgetH; override;
  public
    destructor Destroy; override;
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl; override;
    procedure SetColor(const Value: PQColor); override;
    procedure SignalTextChanged(p1: PWideString); cdecl;
  end;

  { TQtTextEdit }

  TQtTextEdit = class(TQtWidget)
  private
  protected
    function CreateWidget(const AParams: TCreateParams):QWidgetH; override;
  public
    destructor Destroy; override;
    procedure SetColor(const Value: PQColor); override;
    procedure SetAlignment(const AAlignment: TAlignment);
    procedure SignalTextChanged; cdecl;
  end;

  { TQtTabWidget }

  TQtTabWidget = class(TQtWidget)
  private
  protected
    function CreateWidget(const AParams: TCreateParams):QWidgetH; override;
  public
    destructor Destroy; override;
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl; override;
  public
    function insertTab(index: Integer; page: QWidgetH; p2: PWideString): Integer;
    procedure SetTabPosition(ATabPosition: QTabWidgetTabPosition);
    procedure SignalCurrentChanged(Index: Integer); cdecl;
  end;

  { TQtComboBox }

  TQtComboBox = class(TQtWidget)
  private
  protected
    function CreateWidget(const AParams: TCreateParams):QWidgetH; override;
  public
    destructor Destroy; override;
    procedure SetColor(const Value: PQColor); override;
    function currentIndex: Integer;
    procedure setCurrentIndex(index: Integer);
  public
    procedure SlotChange(p1: PWideString); cdecl;
    procedure SlotSelect(index: Integer); cdecl;
  end;

  { TQtAbstractSpinBox}
  
  TQtAbstractSpinBox = class(TQtWidget)
  private
  protected
    function CreateWidget(const AParams: TCreateParams):QWidgetH; override;
  public
    destructor Destroy; override;
    function IsReadOnly: Boolean;
    procedure SetReadOnly(r: Boolean);
    procedure SignalEditingFinished; cdecl;
  end;

  { TQtFloatSpinBox }

  TQtFloatSpinBox = class(TQtAbstractSpinBox)
  private
  protected
    function CreateWidget(const AParams: TCreateParams):QWidgetH; override;
  public
    procedure SignalValueChanged(p1: Double); cdecl;
  end;
  
  { TQtSpinBox }

  TQtSpinBox = class(TQtAbstractSpinBox)
  private
  protected
    function CreateWidget(const AParams: TCreateParams):QWidgetH; override;
  public
    procedure SignalValueChanged(p1: Integer); cdecl;
  end;

  { TQtAbstractScrollArea }

  TQtAbstractScrollArea = class(TQtFrame)
  private
    FCornerWidget: TQtWidget;
    FViewPortWidget: TQtWidget;
    FHScrollbar: TQtScrollBar;
    FVScrollbar: TQtScrollbar;
  protected
    function CreateWidget(const AParams: TCreateParams):QWidgetH; override;
  public
    destructor Destroy; override;
  public
    function cornerWidget: TQtWidget;
    function horizontalScrollBar: TQtScrollBar;
    function verticalScrollBar: TQtScrollBar;
    function viewport: TQtWidget;
    procedure SetColor(const Value: PQColor); override;
    procedure setCornerWidget(AWidget: TQtWidget);
    procedure setHorizontalScrollBar(AScrollBar: TQtScrollBar);
    procedure setScrollStyle(AScrollStyle: TScrollStyle);
    procedure setTextColor(const Value: PQColor); override;
    procedure setVerticalScrollBar(AScrollBar: TQtScrollBar);
    procedure viewportNeeded;
  end;

  { TQtAbstractItemView }

  TQtAbstractItemView = class(TQtAbstractScrollArea)
  public
  end;

  { TQtListView }

  TQtListView = class(TQtAbstractItemView)
  public
  end;

  { TQtListWidget }

  TQtListWidget = class(TQtListView)
  private
  protected
    function CreateWidget(const AParams: TCreateParams):QWidgetH; override;
  public
    FList: TStrings;
    destructor Destroy; override;
    procedure SlotSelectionChange(current: QListWidgetItemH; previous: QListWidgetItemH); cdecl;
    procedure SignalItemDoubleClicked(item: QListWidgetItemH); cdecl;
    procedure SignalItemClicked(item: QListWidgetItemH); cdecl;
    function currentRow: Integer;
    procedure setCurrentRow(row: Integer);
  end;
  
  { TQtHeaderView }

  TQtHeaderView = class (TQtAbstractItemView)
  private
  protected
    function CreateWidget(const AParams: TCreateParams):QWidgetH; override;
  public
    destructor Destroy; override;
    procedure SignalSectionClicked(logicalIndex: Integer) cdecl;
  end;

  { TQtTreeView }
  
  TQtTreeView = class (TQtAbstractItemView)
  private
  protected
    function CreateWidget(const AParams: TCreateParams):QWidgetH; override;
  public
    destructor Destroy; override;
  end;
  
  { TQtTreeWidget }

  TQtTreeWidget = class(TQtTreeView)
  private
    Header: TQtHeaderView;
  protected
    function CreateWidget(const AParams: TCreateParams):QWidgetH; override;
  public
    destructor Destroy; override;
    
    function currentRow: Integer;
    procedure setCurrentRow(row: Integer);
    
    procedure SignalItemPressed(item: QTreeWidgetItemH; column: Integer) cdecl;
    procedure SignalItemClicked(item: QTreeWidgetItemH; column: Integer) cdecl;
    procedure SignalItemDoubleClicked(item: QTreeWidgetItemH; column: Integer) cdecl;
    procedure SignalItemActivated(item: QTreeWidgetItemH; column: Integer) cdecl;
    procedure SignalItemEntered(item: QTreeWidgetItemH; column: Integer) cdecl;
    procedure SignalItemChanged(item: QTreeWidgetItemH; column: Integer) cdecl;
    procedure SignalitemExpanded(item: QTreeWidgetItemH) cdecl;
    procedure SignalItemCollapsed(item: QTreeWidgetItemH) cdecl;
    procedure SignalCurrentItemChanged(current: QTreeWidgetItemH; previous: QTreeWidgetItemH) cdecl;
    procedure SignalItemSelectionChanged; cdecl;
  end;

  { TQtMenu }

  TQtMenu = class(TQtWidget)
  private
    FIcon: QIconH;
  public
    MenuItem: TMenuItem;
  public
    constructor Create(const AParent: QWidgetH); overload;
    constructor Create(const AHandle: QMenuH); overload;
    destructor Destroy; override;
  public
    procedure SlotDestroy; cdecl;
    procedure SlotTriggered(checked: Boolean = False); cdecl;
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl; override;
  public
    procedure PopUp(pos: PQtPoint; at: QActionH = nil);
    function actionHandle: QActionH;
    function addMenu(title: PWideString): TQtMenu;
    function addSeparator: TQtMenu;
    procedure setChecked(p1: Boolean);
    procedure setCheckable(p1: Boolean);
    procedure setHasSubmenu(AValue: Boolean);
    procedure setIcon(AIcon: QIconH);
    procedure setImage(AImage: TQtImage);
    procedure setSeparator(AValue: Boolean);
  end;

  { TQtMenuBar }

  TQtMenuBar = class(TQtWidget)
  private
    FVisible: Boolean;
  public
    constructor Create(const AParent: QWidgetH); overload;
    destructor Destroy; override;
  public
    function addMenu(title: PWideString): TQtMenu;
    function addSeparator: TQtMenu;
  end;

  { TQtProgressBar }

  TQtProgressBar = class(TQtWidget)
  private
  protected
    function CreateWidget(const AParams: TCreateParams):QWidgetH; override;
  public
    destructor Destroy; override;
  public
    procedure setRange(minimum: Integer; maximum: Integer);
    procedure setTextVisible(visible: Boolean);
    procedure setAlignment(alignment: QtAlignment);
    procedure setTextDirection(textDirection: QProgressBarDirection);
    procedure setValue(value: Integer);
    procedure setOrientation(p1: QtOrientation);
    procedure setInvertedAppearance(invert: Boolean);
    procedure SignalValueChanged(Value: Integer); cdecl;
  end;

  { TQtStatusBar }

  TQtStatusBar = class(TQtWidget)
  private
  protected
    function CreateWidget(const AParams: TCreateParams):QWidgetH; override;
  public
    destructor Destroy; override;
  public
    APanels: Array of QLabelH;
    procedure showMessage(text: PWideString; timeout: Integer = 0);
  end;
  
  { TQtDialog }
  
  TQtDialog = class(TQtWidget)
  private
  public
    constructor Create(parent: QWidgetH = nil; f: QtWindowFlags = 0); overload;
    destructor Destroy; override;
  public
    function exec: Integer;
  end;
  
  { TQtCalendar }

  TQtCalendar = class(TQtWidget)
  private
  protected
    function CreateWidget(const AParams: TCreateParams):QWidgetH; override;
  public
    AYear, AMonth, ADay: Word;
    destructor Destroy; override;
    procedure SignalActivated(ADate: QDateH); cdecl;
    procedure SignalClicked(ADate: QDateH); cdecl;
    procedure SignalSelectionChanged; cdecl;
    procedure SignalCurrentPageChanged(p1, p2: Integer); cdecl;
  end;
  
implementation
const
  AlignmentMap: array[TAlignment] of QtAlignment =
  (
{taLeftJustify } QtAlignLeft,
{taRightJustify} QtAlignRight,
{taCenter      } QtAlignHCenter
  );

{ TQtWidget }

{------------------------------------------------------------------------------
  Function: TQtWidget.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtWidget.Create(const AWinControl: TWinControl; const AParams: TCreateParams);
begin
  // Initializes the properties
  FProps := NiL;
  LCLObject := AWinControl;

  // Creates the widget
  Widget := CreateWidget(AParams);
  {$ifdef VerboseQt}
  DebugLn('TQtWidget.Create: Self:%x Widget:%x was created for control %s',
    [ptrint(Self), ptrint(Widget), LCLObject.Name]);
  {$endif}
  
  // set Handle->QWidget map
  AVariant := QVariant_Create(Int64(ptruint(Self)));
  QObject_setProperty(QObjectH(Widget), 'lclwidget', AVariant);
  
  fillchar(FpaintData, sizeOf(FPaintData), 0);

  // Sets it's initial properties
  SetGeometry;
  
  // set focus policy
  if AWinControl.TabStop then
    QWidget_setFocusPolicy(Widget, QtStrongFocus);

  // Set mouse move messages policy
  QWidget_setMouseTracking(Widget, True);
end;

{------------------------------------------------------------------------------
  Function: TQtWidget.CreatePage
  Params:  None
  Returns: Nothing
  
  Special constructor for notebook pages.
  Pages should be created without a parent for Qt
 ------------------------------------------------------------------------------}
constructor TQtWidget.CreatePage(const AWinControl: TWinControl;
  const AParams: TCreateParams);
begin
  // Initializes the properties
  LCLObject := AWinControl;

  // Creates the widget
  Widget := QWidget_create;

  SetGeometry;
end;

{------------------------------------------------------------------------------
  Function: TQtWidget.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtWidget.Destroy;
begin

  QVariant_destroy(AVariant);
  
  {$ifdef VerboseQt}
//    WriteLn('Calling QWidget_destroy');
  {$endif}

  if Widget<>nil then
  begin
    QWidget_destroy(QWidgetH(Widget));
    Widget:=nil;
  end;
  
  if FProps<>nil then
  begin
    FProps.Free;
    FProps:=nil;
  end;

  if FPaintData.ClipRegion<>nil then
  begin
    QRegion_Destroy(FPaintData.ClipRegion);
    FPaintData.ClipRegion:=nil;
  end;

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtWidget.GetContainerWidget
  Params:  None
  Returns: The widget of the control on top of which other controls
           should be placed
 ------------------------------------------------------------------------------}
function TQtWidget.GetContainerWidget: QWidgetH;
begin
  Result := Widget;
end;

{$IFDEF VerboseQt}
function EventTypeToStr(Event:QEventH):string;
begin
  case QEvent_type(Event) of
    QEventNone: result:='QEventNone';
    QEventTimer: result:='QEventTimer';
    QEventMouseButtonPress: result:='QEventMouseButtonPress';
    QEventMouseButtonRelease: result:='QEventMouseButtonRelease';
    QEventMouseButtonDblClick: result:='QEventMouseButtonDblClick';
    QEventMouseMove: result:='QEventMouseMove';
    QEventKeyPress: result:='QEventKeyPress';
    QEventKeyRelease: result:='QEventKeyRelease';
    QEventFocusIn: result:='QEventFocusIn';
    QEventFocusOut: result:='QEventFocusOut';
    QEventEnter: result:='QEventEnter';
    QEventLeave: result:='QEventLeave';
    QEventPaint: result:='QEventPaint';
    QEventMove: result:='QEventMove';
    QEventResize: result:='QEventResize';
    QEventCreate: result:='QEventCreate';
    QEventDestroy: result:='QEventDestroy';
    QEventShow: result:='QEventShow';
    QEventHide: result:='QEventHide';
    QEventClose: result:='QEventClose';
    QEventQuit: result:='QEventQuit';
    QEventParentChange: result:='QEventParentChange';
    QEventThreadChange: result:='QEventThreadChange';
    QEventWindowActivate: result:='QEventWindowActivate';
    QEventWindowDeactivate: result:='QEventWindowDeactivate';
    QEventShowToParent: result:='QEventShowToParent';
    QEventHideToParent: result:='QEventHideToParent';
    QEventWheel: result:='QEventWheel';
    QEventWindowTitleChange: result:='QEventWindowTitleChange';
    QEventWindowIconChange: result:='QEventWindowIconChange';
    QEventApplicationWindowIconChange: result:='QEventApplicationWindowIconChange';
    QEventApplicationFontChange: result:='QEventApplicationFontChange';
    QEventApplicationLayoutDirectionChange: result:='QEventApplicationLayoutDirectionChange';
    QEventApplicationPaletteChange: result:='QEventApplicationPaletteChange';
    QEventPaletteChange: result:='QEventPaletteChange';
    QEventClipboard: result:='QEventClipboard';
    QEventSpeech: result:='QEventSpeech';
    QEventMetaCall: result:='QEventMetaCall';
    QEventSockAct: result:='QEventSockAct';
    QEventShortcutOverride: result:='QEventShortcutOverride';
    QEventDeferredDelete: result:='QEventDeferredDelete';
    QEventDragEnter: result:='QEventDragEnter';
    QEventDragMove: result:='QEventDragMove';
    QEventDragLeave: result:='QEventDragLeave';
    QEventDrop: result:='QEventDrop';
    QEventDragResponse: result:='QEventDragResponse';
    QEventChildAdded: result:='QEventChildAdded';
    QEventChildPolished: result:='QEventChildPolished';
    QEventChildRemoved: result:='QEventChildRemoved';
    QEventShowWindowRequest: result:='QEventShowWindowRequest';
    QEventPolishRequest: result:='QEventPolishRequest';
    QEventPolish: result:='QEventPolish';
    QEventLayoutRequest: result:='QEventLayoutRequest';
    QEventUpdateRequest: result:='QEventUpdateRequest';
    QEventUpdateLater: result:='QEventUpdateLater';
    QEventEmbeddingControl: result:='QEventEmbeddingControl';
    QEventActivateControl: result:='QEventActivateControl';
    QEventDeactivateControl: result:='QEventDeactivateControl';
    QEventContextMenu: result:='QEventContextMenu';
    QEventInputMethod: result:='QEventInputMethod';
    QEventAccessibilityPrepare: result:='QEventAccessibilityPrepare';
    QEventTabletMove: result:='QEventTabletMove';
    QEventLocaleChange: result:='QEventLocaleChange';
    QEventLanguageChange: result:='QEventLanguageChange';
    QEventLayoutDirectionChange: result:='QEventLayoutDirectionChange';
    QEventStyle: result:='QEventStyle';
    QEventTabletPress: result:='QEventTabletPress';
    QEventTabletRelease: result:='QEventTabletRelease';
    QEventOkRequest: result:='QEventOkRequest';
    QEventHelpRequest: result:='QEventHelpRequest';
    QEventIconDrag: result:='QEventIconDrag';
    QEventFontChange: result:='QEventFontChange';
    QEventEnabledChange: result:='QEventEnabledChange';
    QEventActivationChange: result:='QEventActivationChange';
    QEventStyleChange: result:='QEventStyleChange';
    QEventIconTextChange: result:='QEventIconTextChange';
    QEventModifiedChange: result:='QEventModifiedChange';
    QEventWindowBlocked: result:='QEventWindowBlocked';
    QEventWindowUnblocked: result:='QEventWindowUnblocked';
    QEventWindowStateChange: result:='QEventWindowStateChange';
    QEventMouseTrackingChange: result:='QEventMouseTrackingChange';
    QEventToolTip: result:='QEventToolTip';
    QEventWhatsThis: result:='QEventWhatsThis';
    QEventStatusTip: result:='QEventStatusTip';
    QEventActionChanged: result:='QEventActionChanged';
    QEventActionAdded: result:='QEventActionAdded';
    QEventActionRemoved: result:='QEventActionRemoved';
    QEventFileOpen: result:='QEventFileOpen';
    QEventShortcut: result:='QEventShortcut';
    QEventWhatsThisClicked: result:='QEventWhatsThisClicked';
    QEventAccessibilityHelp: result:='QEventAccessibilityHelp';
    QEventToolBarChange: result:='QEventToolBarChange';
    QEventApplicationActivated: result:='QEventApplicationActivated';
    QEventApplicationDeactivated: result:='QEventApplicationDeactivated';
    QEventQueryWhatsThis: result:='QEventQueryWhatsThis';
    QEventEnterWhatsThisMode: result:='QEventEnterWhatsThisMode';
    QEventLeaveWhatsThisMode: result:='QEventLeaveWhatsThisMode';
    QEventZOrderChange: result:='QEventZOrderChange';
    QEventHoverEnter: result:='QEventHoverEnter';
    QEventHoverLeave: result:='QEventHoverLeave';
    QEventHoverMove: result:='QEventHoverMove';
    QEventAccessibilityDescription: result:='QEventAccessibilityDescription';
    QEventParentAboutToChange: result:='QEventParentAboutToChange';
    QEventWinEventAct: result:='QEventWinEventAct';
    QEventAcceptDropsChange: result:='QEventAcceptDropsChange';
    QEventMenubarUpdated: result:='QEventMenubarUpdated';
    QEventZeroTimerEvent: result:='QEventZeroTimerEvent';
    QEventUser: result:='QEventUser';
    QEventMaxUser: result:='QEventMaxUser';
  end;
end;
{$ENDIF}

{------------------------------------------------------------------------------
  Function: TQtWidget.EventFilter
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtWidget.EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl;
begin
  Result := False;

  QEvent_accept(Event);

  {$ifdef VerboseQt}
  WriteLn('TQtWidget.EventFilter: Sender=', IntToHex(ptrint(Sender),8),
    ' LCLObject=', dbgsName(LCLObject),
    ' Event=', EventTypeToStr(Event));
  {$endif}

  case QEvent_type(Event) of
   QEventShow: SlotShow(True);
   QEventHide: SlotShow(False);
   QEventClose: SlotClose;
   QEventDestroy: SlotDestroy;
   QEventFocusIn: SlotFocus(True);
   QEventFocusOut: SlotFocus(False);
   QEventKeyPress: SlotKey(Event);
   QEventKeyRelease: SlotKey(Event);
   QEventMouseButtonPress: SlotMouse(Event);
   QEventMouseButtonRelease: SlotMouse(Event);
   QEventMouseButtonDblClick: SlotMouse(Event);
   QEventMouseMove: SlotMouseMove(Event);
   QEventWheel: SlotMouseWheel(Event);
   QEventResize: SlotResize;
   QEventPaint: SlotPaint(Event);
   QEventContextMenu: SlotContextMenu;
  else
   QEvent_ignore(Event);
  end;

{  GtkWidgetSet.SetCallback(LM_WINDOWPOSCHANGED, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_EXPOSEEVENT, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_KEYDOWN, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_KEYUP, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_CHAR, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_MOUSEMOVE, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_LBUTTONDOWN, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_LBUTTONUP, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_RBUTTONDOWN, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_RBUTTONUP, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_MBUTTONDOWN, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_MBUTTONUP, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_MOUSEWHEEL, AGTKObject, AComponent);}
end;

{------------------------------------------------------------------------------
  Function: TQtWidget.SlotShow
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtWidget.SlotShow(vShow: Boolean); cdecl;
var
  Msg: TLMShowWindow;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtWidget.SlotShow Name', LCLObject.Name, ' vShow: ', dbgs(vShow));
  {$endif}

  FillChar(Msg, SizeOf(Msg), #0);

  Msg.Msg := LM_SHOWWINDOW;
  Msg.Show := vShow;

  DeliverMessage(Msg);
end;

{------------------------------------------------------------------------------
  Function: TQtWidget.Close
  Params:  None
  Returns: Nothing

  Note: LCL uses LM_CLOSEQUERY to set the form visibility and if we don´t send this
 message, you won´t be able to show a form twice.
 ------------------------------------------------------------------------------}
procedure TQtWidget.SlotClose; cdecl;
var
  Msg: TLMessage;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtWidget.SlotClose');
  {$endif}

  FillChar(Msg, SizeOf(Msg), #0);

  Msg.Msg := LM_CLOSEQUERY;

  DeliverMessage(Msg);
end;

{------------------------------------------------------------------------------
  Function: TQtWidget.SlotDestroy
  Params:  None
  Returns: Nothing

  Currently commented because it was raising exception on software exit
 ------------------------------------------------------------------------------}
procedure TQtWidget.SlotDestroy; cdecl;
var
  Msg: TLMessage;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtWidget.SlotDestroy');
  {$endif}

  FillChar(Msg, SizeOf(Msg), #0);

  Msg.Msg := LM_DESTROY;

  DeliverMessage(Msg);
end;

{------------------------------------------------------------------------------
  Function: TQtWidget.SlotFocus
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtWidget.SlotFocus(FocusIn: Boolean); cdecl;
var
  Msg: TLMessage;
begin
  {$ifdef VerboseFocus}
    WriteLn('TQtWidget.SlotFocus In=',FocusIn,' For ', dbgsname(LCLObject));
  {$endif}

  FillChar(Msg, SizeOf(Msg), #0);

  if FocusIn then Msg.Msg := LM_SETFOCUS
  else Msg.Msg := LM_KILLFOCUS;

  DeliverMessage(Msg);

  {$ifdef VerboseFocus}
    WriteLn('TQtWidget.SlotFocus END');
  {$endif}
end;

{------------------------------------------------------------------------------
  Function: TQtWidget.SlotKey
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtWidget.SlotKey(Event: QEventH); cdecl;
var
  Msg: TLMKey;
  KeyboardModifiers: QtKeyboardModifiers;
  AltModifier: Boolean;
  Text: WideString;
  UTF8Char: TUTF8Char;
  RepeatCount: Integer;
begin
  {$ifdef VerboseQt}
    Write('TQtWidget.SlotKey');
  {$endif}

  FillChar(Msg, SizeOf(Msg), #0);

  {------------------------------------------------------------------------------
   Translates a Qt4 Key to a LCL VK_* key
   ------------------------------------------------------------------------------}
  Msg.CharCode := QtKeyToLCLKey(QKeyEvent_key(QKeyEventH(Event)));
  
  {------------------------------------------------------------------------------
   Detects special keys (shift, alt, control, etc)
   ------------------------------------------------------------------------------}
  KeyboardModifiers := QKeyEvent_modifiers(QKeyEventH(Event));

  AltModifier := (QtAltModifier and KeyboardModifiers) <> $0;

  {------------------------------------------------------------------------------
   Loads the UTF-8 character associated with the keypress, if any
   ------------------------------------------------------------------------------}
  QKeyEvent_text(QKeyEventH(Event), @Text);

  {------------------------------------------------------------------------------
   Sends the adequate key messages
   ------------------------------------------------------------------------------}
  if AltModifier then
  begin
    if QEvent_type(Event) = QEventKeyRelease then Msg.Msg := CN_SYSKEYUP
    else if QEvent_type(Event) = QEventKeyPress then Msg.Msg := CN_SYSKEYDOWN;
  end
  else
  begin
    if QEvent_type(Event) = QEventKeyRelease then Msg.Msg := CN_KEYUP
    else if QEvent_type(Event) = QEventKeyPress then Msg.Msg := CN_KEYDOWN;
  end;

  {$ifdef VerboseQt}
    WriteLn(' message: ', Msg.Msg);
  {$endif}
  try
    LCLObject.WindowProc(TLMessage(Msg));
  except
    Application.HandleException(nil);
  end;
  
  { Also sends a utf-8 key event for key down }

  if (QEvent_type(Event) = QEventKeyPress) and (Length(Text) <> 0) then
  begin
    RepeatCount := 0;
    UTF8Char := TUTF8Char(Text);

    LCLObject.IntfUTF8KeyPress(UTF8Char, RepeatCount, False);
  end;
end;

{------------------------------------------------------------------------------
  Function: TQtWidget.SlotMouse
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtWidget.SlotMouse(Event: QEventH); cdecl;
var
  Msg: TLMMouse;
  MousePos: PQtPoint;
  Mbutton: QTMouseButtons;
  Modifiers: QtKeyboardModifiers;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtWidget.SlotMouse');
  {$endif}

  FillChar(Msg, SizeOf(Msg), #0);
  
  MousePos := QMouseEvent_pos(QMouseEventH(Event));
  Msg.Keys := 0;
  
  Modifiers := QInputEvent_modifiers(QInputEventH(Event));
  if Modifiers and qtShiftModifier <> 0 then Msg.Keys := Msg.Keys or MK_SHIFT;
  if Modifiers and qtControlModifier<>0 then Msg.Keys := Msg.Keys or MK_CONTROL;
  { TODO: add support for ALT, META and NUMKEYPAD }

  Msg.XPos := SmallInt(MousePos^.X);
  Msg.YPos := SmallInt(MousePos^.Y);
  
  MButton := QmouseEvent_Button(QMouseEventH(Event));

  case QEvent_type(Event) of
   QEventMouseButtonPress:
    begin
      case MButton of
        QtLeftButton:
          begin
            Msg.Msg := LM_LBUTTONDOWN;
            Msg.Keys := MK_LBUTTON;
          end;
        QtRightButton:
          begin
            Msg.Msg := LM_RBUTTONDOWN;
            Msg.Keys := MK_RBUTTON;
          end;
        QtMidButton:
          begin
            Msg.Msg := LM_MBUTTONDOWN;
            Msg.Msg := MK_MBUTTON;
          end;
      end;
      DeliverMessage(Msg);
      Msg.Msg := LM_PRESSED;
      DeliverMessage(Msg);
    end;
   QEventMouseButtonRelease:
   begin
      case MButton of
        QtLeftButton:
          begin
            Msg.Msg := LM_LBUTTONUP;
            Msg.Keys := MK_LBUTTON;
          end;
        QtRightButton:
          begin
            Msg.Msg := LM_RBUTTONUP;
            Msg.Keys := MK_RBUTTON;
          end;
        QtMidButton:
          begin
            Msg.Msg := LM_MBUTTONUP;
            Msg.Msg := MK_MBUTTON;
          end;
      end;
      DeliverMessage(Msg);
     { Clicking on buttons operates differently, because QEventMouseButtonRelease
       is sent if you click a control, drag the mouse out of it and release, but
       buttons should not be clicked on this case. }
     if not (LCLObject is TCustomButton) then
     begin
       Msg.Msg := LM_CLICKED;
       DeliverMessage(Msg);
     end;

     Msg.Msg := LM_RELEASED;
   end;
   QEventMouseButtonDblClick: Msg.Msg := LM_CLICKED;
  end;
  DeliverMessage(Msg);
end;

{------------------------------------------------------------------------------
  Function: TQtWidget.SlotMouseMove
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtWidget.SlotMouseMove(Event: QEventH); cdecl;
var
  Msg: TLMMouseMove;
  MousePos: PQtPoint;
begin
  FillChar(Msg, SizeOf(Msg), #0);
  
  MousePos := QMouseEvent_pos(QMouseEventH(Event));

  //QCursor_pos(@MousePos);

  Msg.XPos := SmallInt(MousePos^.X);
  Msg.YPos := SmallInt(MousePos^.Y);

  Msg.Msg := LM_MOUSEMOVE;

  DeliverMessage(Msg);
end;

{------------------------------------------------------------------------------
  Function: TQtWidget.SlotMouseWheel
  Params:  None
  Returns: Nothing

  Qt stores the delta in 1/8 of a degree
  Most mouses scroll 15 degrees each time
 
  Msg.WheelData: -1 for up, 1 for down
 ------------------------------------------------------------------------------}
procedure TQtWidget.SlotMouseWheel(Event: QEventH); cdecl;
var
  Msg: TLMMouseEvent;
  MousePos: PQtPoint;
begin
  FillChar(Msg, SizeOf(Msg), #0);

  MousePos := QWheelEvent_pos(QWheelEventH(Event));

  Msg.Msg := LM_MOUSEWHEEL;

  Msg.X := SmallInt(MousePos^.X);
  Msg.Y := SmallInt(MousePos^.Y);

  Msg.WheelDelta := QWheelEvent_delta(QWheelEventH(Event)) div 120;
  
  NotifyApplicationUserInput(Msg.Msg);
  DeliverMessage(Msg);
end;

{------------------------------------------------------------------------------
  Function: TQtWidget.SlotPaint
  Params:  None
  Returns: Nothing

  Sends a LM_PAINT message to the LCL. This is for windowed controls only
 ------------------------------------------------------------------------------}
procedure TQtWidget.SlotPaint(Event: QEventH); cdecl;
var
  Msg: TLMPaint;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtWidget.SlotPaint');
  {$endif}

  if (LCLObject is TWinControl) then
  begin
    FillChar(Msg, SizeOf(Msg), #0);

    Msg.Msg := LM_PAINT;
    Msg.DC := 0;

    // send paint message
    try

      // Saving clip rect and clip region
      with PaintData do
      begin
        ClipRegion := QPaintEvent_Region(QPaintEventH(Event));
        if ClipRect=nil then
          New(ClipRect);
        QPaintEvent_Rect(QPaintEventH(Event), ClipRect);
      end;

      try
        LCLObject.WindowProc(TLMessage(Msg));
      finally
        Dispose(PaintData.ClipRect);
        Fillchar(FPaintData, SizeOf(FPaintData), 0);
      end;
    except
      Application.HandleException(nil);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Function: TQtWidget.SlotResize
  Params:  None
  Returns: Nothing

  Sends a LM_SIZE message to the LCL.
 ------------------------------------------------------------------------------}
procedure TQtWidget.SlotResize; cdecl;
var
  Msg: TLMSize;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtWidget.SlotResize');
  {$endif}

  FillChar(Msg, SizeOf(Msg), #0);

  Msg.Msg := LM_SIZE;

  case QWidget_windowState(Widget) of
   QtWindowMinimized: Msg.SizeType := SIZEICONIC;
   QtWindowMaximized: Msg.SizeType := SIZEFULLSCREEN;
   QtWindowFullScreen: Msg.SizeType := SIZEFULLSCREEN;
  else
   Msg.SizeType := SIZENORMAL;
  end;

  Msg.SizeType := Msg.SizeType or Size_SourceIsInterface;

  Msg.Width := QWidget_width(Widget);
  Msg.Height := QWidget_height(Widget);

  DeliverMessage(Msg);
end;

procedure TQtWidget.SlotContextMenu; cdecl;
begin
  if Assigned(LCLObject.PopupMenu) then
   LCLObject.PopupMenu.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

{------------------------------------------------------------------------------
  Function: TQtWidget.SetColor
  Params:  QColorH
  Returns: Nothing

  Changes the color of a widget
 ------------------------------------------------------------------------------}
procedure TQtWidget.SetColor(const Value: PQColor);
var
  Palette: QPaletteH;
begin
  Palette := QPalette_create(QWidget_palette(Widget));
  try
    QPalette_setColor(Palette, QPaletteWindow, Value);
    QWidget_setPalette(Widget, Palette);
  finally
    QPalette_destroy(Palette);
  end;
end;

{------------------------------------------------------------------------------
  Function: TQtWidget.SetTextColor
  Params:  QColorH
  Returns: Nothing

  Changes the text color of a widget
 ------------------------------------------------------------------------------}
procedure TQtWidget.SetTextColor(const Value: PQColor);
var
  Palette: QPaletteH;
begin
  Palette := QPalette_create(QWidget_palette(Widget));
  try
    QPalette_setColor(Palette, QPaletteText, Value);
    QWidget_setPalette(Widget, Palette);
  finally
    QPalette_destroy(Palette);
  end;
end;

procedure TQtWidget.SetCursor(const ACursor: QCursorH);
begin
  if ACursor <> nil then
    QWidget_setCursor(Widget, ACursor);
end;

{------------------------------------------------------------------------------
  Function: TQtWidget.Update
  Params:  None
  Returns: Nothing

  Schedules a paint event for processing when Qt returns to the main event loop
 ------------------------------------------------------------------------------}
procedure TQtWidget.Update;
begin
  QWidget_update(Widget);
end;

{------------------------------------------------------------------------------
  Function: TQtWidget.Repaint
  Params:  None
  Returns: Nothing

  Repaints the control imediately
 ------------------------------------------------------------------------------}
procedure TQtWidget.Repaint;
begin
  QWidget_repaint(Widget);
end;

procedure TQtWidget.setWindowTitle(Str: PWideString);
begin
  QWidget_setWindowTitle(Widget, Str);
end;

procedure TQtWidget.WindowTitle(Str: PWideString);
begin
  QWidget_WindowTitle(Widget, Str);
end;

procedure TQtWidget.Hide;
begin
  QWidget_hide(Widget);
end;

procedure TQtWidget.Show;
begin
  QWidget_show(Widget);
end;

function TQtWidget.getVisible: boolean;
begin
  Result := QWidget_isVisible(Widget);
end;

procedure TQtWidget.setEnabled(p1: Boolean);
begin
  QWidget_setEnabled(Widget, p1);
end;

procedure TQtWidget.setVisible(visible: Boolean);
begin
  QWidget_setVisible(Widget, visible);
end;

function TQtWidget.windowModality: QtWindowModality;
begin
  Result := QWidget_windowModality(Widget);
end;

procedure TQtWidget.setWindowModality(windowModality: QtWindowModality);
begin
  QWidget_setWindowModality(Widget, windowModality);
end;

procedure TQtWidget.setParent(parent: QWidgetH);
begin
  QWidget_setParent(Widget, parent);
end;

procedure TQtWidget.setWindowFlags(_type: QtWindowFlags);
begin
  QWidget_setWindowFlags(Widget, _type);
end;

function TQtWidget.windowFlags: QtWindowFlags;
begin
  Result := QWidget_windowFlags(Widget);
end;

procedure TQtWidget.setWidth(p1: Integer);
var
   R: TRect;
begin
  QWidget_geometry(Widget, @R);
  R.Right := p1;
  QWidget_setGeometry(Widget,@R);
end;

procedure TQtWidget.setHeight(p1: Integer);
var
   R: TRect;
begin
  QWidget_geometry(Widget, @R);
  R.Bottom := p1;
  QWidget_setGeometry(Widget, @R);
end;

procedure TQtWidget.setTabOrder(p1, p2: TQtWidget);
begin
  QWidget_setTabOrder(p1.Widget, p2.Widget);
end;

{------------------------------------------------------------------------------
  Function: TQtWidget.QtKeyToLCLKey
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtWidget.QtKeyToLCLKey(key: Integer): Word;
begin
  case key of
    QtKey_Escape: Result := VK_ESCAPE;
    QtKey_Tab: Result := VK_TAB;
//    QtKey_Backtab = 16777218 { $1000002 }; ????
//    QtKey_Backspace = 16777219 { $1000003 };
    QtKey_Return: Result := VK_RETURN;
    QtKey_Enter: Result := VK_RETURN;
    QtKey_Insert: Result := VK_RETURN;
    QtKey_Delete: Result := VK_RETURN;
    QtKey_Pause: Result := VK_PAUSE;
    QtKey_Print: Result := VK_PRINT;
//    QtKey_SysReq = 16777226 { $100000a };
//    QtKey_Clear = 16777227 { $100000b };
    QtKey_Home: Result := VK_HOME;
    QtKey_End: Result := VK_END;
    QtKey_Left: Result := VK_LEFT;
    QtKey_Up: Result := VK_UP;
    QtKey_Right: Result := VK_RIGHT;
    QtKey_Down: Result := VK_DOWN;
    QtKey_PageUp: Result := VK_PRIOR;
    QtKey_PageDown: Result := VK_NEXT;
    QtKey_Shift: Result := VK_LSHIFT;     // There is also RSHIFT
    QtKey_Control: Result := VK_LCONTROL; // There is also RCONTROL
{    QtKey_Meta: Result := VK_META;
    QtKey_Alt: Result := VK_ALT;
    QtKey_CapsLock: Result := VK_CAPSLOCK;
    QtKey_NumLock: Result := VK_NUMLOCK;
    QtKey_ScrollLock = 16777254  $1000026 ;}
    QtKey_F1: Result := VK_F1;
    QtKey_F2: Result := VK_F2;
    QtKey_F3: Result := VK_F3;
    QtKey_F4: Result := VK_F4;
    QtKey_F5: Result := VK_F5;
    QtKey_F6: Result := VK_F6;
    QtKey_F7: Result := VK_F7;
    QtKey_F8: Result := VK_F8;
    QtKey_F9: Result := VK_F9;
    QtKey_F10: Result := VK_F10;
    QtKey_F11: Result := VK_F11;
    QtKey_F12: Result := VK_F12;
    QtKey_F13: Result := VK_F13;
    QtKey_F14: Result := VK_F14;
    QtKey_F15: Result := VK_F15;
    QtKey_F16: Result := VK_F16;
    QtKey_F17: Result := VK_F17;
    QtKey_F18: Result := VK_F18;
    QtKey_F19: Result := VK_F19;
    QtKey_F20: Result := VK_F20;
    QtKey_F21: Result := VK_F21;
    QtKey_F22: Result := VK_F22;
    QtKey_F23: Result := VK_F23;
    QtKey_F24: Result := VK_F24;
{    QtKey_F25 = 16777288  $1000048 ;
    QtKey_F26 = 16777289  $1000049 ;
    QtKey_F27 = 16777290  $100004a ;
    QtKey_F28 = 16777291  $100004b ;
    QtKey_F29 = 16777292  $100004c ;
    QtKey_F30 = 16777293  $100004d ;
    QtKey_F31 = 16777294  $100004e ;
    QtKey_F32 = 16777295  $100004f ;
    QtKey_F33 = 16777296  $1000050 ;
    QtKey_F34 = 16777297  $1000051 ;
    QtKey_F35 = 16777298  $1000052 ;}
(*    QtKey_Super_L = 16777299 { $1000053 };
    QtKey_Super_R = 16777300 { $1000054 };
    QtKey_Menu = 16777301 { $1000055 };
    QtKey_Hyper_L = 16777302 { $1000056 };
    QtKey_Hyper_R = 16777303 { $1000057 };
    QtKey_Help = 16777304 { $1000058 };
    QtKey_Direction_L = 16777305 { $1000059 };
    QtKey_Direction_R = 16777312 { $1000060 };
    QtKey_Space = 32 { $20 };
    QtKey_Any = 32 { $20 };
    QtKey_Exclam = 33 { $21 };
    QtKey_QuoteDbl = 34 { $22 };
    QtKey_NumberSign = 35 { $23 };
    QtKey_Dollar = 36 { $24 };
    QtKey_Percent = 37 { $25 };
    QtKey_Ampersand = 38 { $26 };
    QtKey_Apostrophe = 39 { $27 };
    QtKey_ParenLeft = 40 { $28 };
    QtKey_ParenRight = 41 { $29 };
    QtKey_Asterisk = 42 { $2a };
    QtKey_Plus = 43 { $2b };
    QtKey_Comma = 44 { $2c };
    QtKey_Minus = 45 { $2d };
    QtKey_Period = 46 { $2e };
    QtKey_Slash = 47 { $2f };*)
    QtKey_0: Result := VK_0;
    QtKey_1: Result := VK_1;
    QtKey_2: Result := VK_2;
    QtKey_3: Result := VK_3;
    QtKey_4: Result := VK_4;
    QtKey_5: Result := VK_5;
    QtKey_6: Result := VK_6;
    QtKey_7: Result := VK_7;
    QtKey_8: Result := VK_8;
    QtKey_9: Result := VK_9;
//    QtKey_Colon = 58 { $3a };
//    QtKey_Semicolon = 59 { $3b };
//    QtKey_Less = 60 { $3c };
//    QtKey_Equal = 61 { $3d };
//    QtKey_Greater = 62 { $3e };
//    QtKey_Question = 63 { $3f };
//    QtKey_At = 64 { $40 };
    QtKey_A: Result := VK_A;
    QtKey_B: Result := VK_B;
    QtKey_C: Result := VK_C;
    QtKey_D: Result := VK_D;
    QtKey_E: Result := VK_E;
    QtKey_F: Result := VK_F;
    QtKey_G: Result := VK_G;
    QtKey_H: Result := VK_H;
    QtKey_I: Result := VK_I;
    QtKey_J: Result := VK_J;
    QtKey_K: Result := VK_K;
    QtKey_L: Result := VK_L;
    QtKey_M: Result := VK_M;
    QtKey_N: Result := VK_N;
    QtKey_O: Result := VK_O;
    QtKey_P: Result := VK_P;
    QtKey_Q: Result := VK_Q;
    QtKey_R: Result := VK_R;
    QtKey_S: Result := VK_S;
    QtKey_T: Result := VK_T;
    QtKey_U: Result := VK_U;
    QtKey_V: Result := VK_V;
    QtKey_W: Result := VK_W;
    QtKey_X: Result := VK_X;
    QtKey_Y: Result := VK_Y;
    QtKey_Z: Result := VK_Z;
(*    QtKey_BracketLeft = 91 { $5b };
    QtKey_Backslash = 92 { $5c };
    QtKey_BracketRight = 93 { $5d };
    QtKey_AsciiCircum = 94 { $5e };
    QtKey_Underscore = 95 { $5f };
    QtKey_QuoteLeft = 96 { $60 };
    QtKey_BraceLeft = 123 { $7b };
    QtKey_Bar = 124 { $7c };
    QtKey_BraceRight = 125 { $7d };
    QtKey_AsciiTilde = 126 { $7e };
    QtKey_nobreakspace = 160 { $a0 };
    QtKey_exclamdown = 161 { $a1 };
    QtKey_cent = 162 { $a2 };
    QtKey_sterling = 163 { $a3 };
    QtKey_currency = 164 { $a4 };
    QtKey_yen = 165 { $a5 };
    QtKey_brokenbar = 166 { $a6 };
    QtKey_section = 167 { $a7 };
    QtKey_diaeresis = 168 { $a8 };
    QtKey_copyright = 169 { $a9 };
    QtKey_ordfeminine = 170 { $aa };
    QtKey_guillemotleft = 171 { $ab };
    QtKey_notsign = 172 { $ac };
    QtKey_hyphen = 173 { $ad };
    QtKey_registered = 174 { $ae };
    QtKey_macron = 175 { $af };
    QtKey_degree = 176 { $b0 };
    QtKey_plusminus = 177 { $b1 };
    QtKey_twosuperior = 178 { $b2 };
    QtKey_threesuperior = 179 { $b3 };
    QtKey_acute = 180 { $b4 };
    QtKey_mu = 181 { $b5 };
    QtKey_paragraph = 182 { $b6 };
    QtKey_periodcentered = 183 { $b7 };
    QtKey_cedilla = 184 { $b8 };
    QtKey_onesuperior = 185 { $b9 };
    QtKey_masculine = 186 { $ba };
    QtKey_guillemotright = 187 { $bb };
    QtKey_onequarter = 188 { $bc };
    QtKey_onehalf = 189 { $bd };
    QtKey_threequarters = 190 { $be };
    QtKey_questiondown = 191 { $bf };
    QtKey_Agrave = 192 { $c0 };
    QtKey_Aacute = 193 { $c1 };
    QtKey_Acircumflex = 194 { $c2 };
    QtKey_Atilde = 195 { $c3 };
    QtKey_Adiaeresis = 196 { $c4 };
    QtKey_Aring = 197 { $c5 };
    QtKey_AE = 198 { $c6 };
    QtKey_Ccedilla = 199 { $c7 };
    QtKey_Egrave = 200 { $c8 };
    QtKey_Eacute = 201 { $c9 };
    QtKey_Ecircumflex = 202 { $ca };
    QtKey_Ediaeresis = 203 { $cb };
    QtKey_Igrave = 204 { $cc };
    QtKey_Iacute = 205 { $cd };
    QtKey_Icircumflex = 206 { $ce };
    QtKey_Idiaeresis = 207 { $cf };
    QtKey_ETH = 208 { $d0 };
    QtKey_Ntilde = 209 { $d1 };
    QtKey_Ograve = 210 { $d2 };
    QtKey_Oacute = 211 { $d3 };
    QtKey_Ocircumflex = 212 { $d4 };
    QtKey_Otilde = 213 { $d5 };
    QtKey_Odiaeresis = 214 { $d6 };
    QtKey_multiply = 215 { $d7 };
    QtKey_Ooblique = 216 { $d8 };
    QtKey_Ugrave = 217 { $d9 };
    QtKey_Uacute = 218 { $da };
    QtKey_Ucircumflex = 219 { $db };
    QtKey_Udiaeresis = 220 { $dc };
    QtKey_Yacute = 221 { $dd };
    QtKey_THORN = 222 { $de };
    QtKey_ssharp = 223 { $df };
    QtKey_division = 247 { $f7 };
    QtKey_ydiaeresis = 255 { $ff };
    QtKey_Multi_key = 16781600 { $1001120 };
    QtKey_Codeinput = 16781623 { $1001137 };
    QtKey_SingleCandidate = 16781628 { $100113c };
    QtKey_MultipleCandidate = 16781629 { $100113d };
    QtKey_PreviousCandidate = 16781630 { $100113e };
    QtKey_Mode_switch = 16781694 { $100117e };
    QtKey_Kanji = 16781601 { $1001121 };
    QtKey_Muhenkan = 16781602 { $1001122 };
    QtKey_Henkan = 16781603 { $1001123 };
    QtKey_Romaji = 16781604 { $1001124 };
    QtKey_Hiragana = 16781605 { $1001125 };
    QtKey_Katakana = 16781606 { $1001126 };
    QtKey_Hiragana_Katakana = 16781607 { $1001127 };
    QtKey_Zenkaku = 16781608 { $1001128 };
    QtKey_Hankaku = 16781609 { $1001129 };
    QtKey_Zenkaku_Hankaku = 16781610 { $100112a };
    QtKey_Touroku = 16781611 { $100112b };
    QtKey_Massyo = 16781612 { $100112c };
    QtKey_Kana_Lock = 16781613 { $100112d };
    QtKey_Kana_Shift = 16781614 { $100112e };
    QtKey_Eisu_Shift = 16781615 { $100112f };
    QtKey_Eisu_toggle = 16781616 { $1001130 };
    QtKey_Hangul = 16781617 { $1001131 };
    QtKey_Hangul_Start = 16781618 { $1001132 };
    QtKey_Hangul_End = 16781619 { $1001133 };
    QtKey_Hangul_Hanja = 16781620 { $1001134 };
    QtKey_Hangul_Jamo = 16781621 { $1001135 };
    QtKey_Hangul_Romaja = 16781622 { $1001136 };
    QtKey_Hangul_Jeonja = 16781624 { $1001138 };
    QtKey_Hangul_Banja = 16781625 { $1001139 };
    QtKey_Hangul_PreHanja = 16781626 { $100113a };
    QtKey_Hangul_PostHanja = 16781627 { $100113b };
    QtKey_Hangul_Special = 16781631 { $100113f };
    QtKey_Dead_Grave = 16781904 { $1001250 };
    QtKey_Dead_Acute = 16781905 { $1001251 };
    QtKey_Dead_Circumflex = 16781906 { $1001252 };
    QtKey_Dead_Tilde = 16781907 { $1001253 };
    QtKey_Dead_Macron = 16781908 { $1001254 };
    QtKey_Dead_Breve = 16781909 { $1001255 };
    QtKey_Dead_Abovedot = 16781910 { $1001256 };
    QtKey_Dead_Diaeresis = 16781911 { $1001257 };
    QtKey_Dead_Abovering = 16781912 { $1001258 };
    QtKey_Dead_Doubleacute = 16781913 { $1001259 };
    QtKey_Dead_Caron = 16781914 { $100125a };
    QtKey_Dead_Cedilla = 16781915 { $100125b };
    QtKey_Dead_Ogonek = 16781916 { $100125c };
    QtKey_Dead_Iota = 16781917 { $100125d };
    QtKey_Dead_Voiced_Sound = 16781918 { $100125e };
    QtKey_Dead_Semivoiced_Sound = 16781919 { $100125f };
    QtKey_Dead_Belowdot = 16781920 { $1001260 };
    QtKey_Dead_Hook = 16781921 { $1001261 };
    QtKey_Dead_Horn = 16781922 { $1001262 };
    QtKey_Back = 16777313 { $1000061 };
    QtKey_Forward = 16777314 { $1000062 };
    QtKey_Stop = 16777315 { $1000063 };
    QtKey_Refresh = 16777316 { $1000064 };
    QtKey_VolumeDown = 16777328 { $1000070 };
    QtKey_VolumeMute = 16777329 { $1000071 };
    QtKey_VolumeUp = 16777330 { $1000072 };
    QtKey_BassBoost = 16777331 { $1000073 };
    QtKey_BassUp = 16777332 { $1000074 };
    QtKey_BassDown = 16777333 { $1000075 };
    QtKey_TrebleUp = 16777334 { $1000076 };
    QtKey_TrebleDown = 16777335 { $1000077 };
    QtKey_MediaPlay = 16777344 { $1000080 };
    QtKey_MediaStop = 16777345 { $1000081 };
    QtKey_MediaPrevious = 16777346 { $1000082 };
    QtKey_MediaNext = 16777347 { $1000083 };
    QtKey_MediaRecord = 16777348 { $1000084 };
    QtKey_HomePage = 16777360 { $1000090 };
    QtKey_Favorites = 16777361 { $1000091 };
    QtKey_Search = 16777362 { $1000092 };
    QtKey_Standby = 16777363 { $1000093 };
    QtKey_OpenUrl = 16777364 { $1000094 };
    QtKey_LaunchMail = 16777376 { $10000a0 };
    QtKey_LaunchMedia = 16777377 { $10000a1 };
    QtKey_Launch0 = 16777378 { $10000a2 };
    QtKey_Launch1 = 16777379 { $10000a3 };
    QtKey_Launch2 = 16777380 { $10000a4 };
    QtKey_Launch3 = 16777381 { $10000a5 };
    QtKey_Launch4 = 16777382 { $10000a6 };
    QtKey_Launch5 = 16777383 { $10000a7 };
    QtKey_Launch6 = 16777384 { $10000a8 };
    QtKey_Launch7 = 16777385 { $10000a9 };
    QtKey_Launch8 = 16777386 { $10000aa };
    QtKey_Launch9 = 16777387 { $10000ab };
    QtKey_LaunchA = 16777388 { $10000ac };
    QtKey_LaunchB = 16777389 { $10000ad };
    QtKey_LaunchC = 16777390 { $10000ae };
    QtKey_LaunchD = 16777391 { $10000af };
    QtKey_LaunchE = 16777392 { $10000b0 };
    QtKey_LaunchF = 16777393 { $10000b1 };
    QtKey_MediaLast = 16842751 { $100ffff };
    QtKey_unknown = 33554431 { $1ffffff };*)
  else
    Result := VK_UNKNOWN;
  end;
end;

function TQtWidget.GetProps(const AnIndex: String): pointer;
var
  i: Integer;
begin
  if (Fprops<>nil) then
  begin
    i:=Fprops.IndexOf(AnIndex);
    if i>=0 then
    begin
      result:=Fprops.Objects[i];
      exit;
    end;
  end;
  result := nil;
end;

procedure TQtWidget.DeliverMessage(var Msg);
begin
  try
    LCLObject.WindowProc(TLMessage(Msg));
  except
    Application.HandleException(nil);
  end;
end;

procedure TQtWidget.SetProps(const AnIndex: String; const AValue: pointer);
var
  i: Integer;
begin
  if FProps=nil then
  begin
    FProps:=TStringList.Create;
    //FProps.CaseSensitive:=false;
    FProps.Sorted:=true;
  end;
  i := Fprops.IndexOf(AnIndex);
  if i < 0 then
    i := FProps.Add(AnIndex);
  Fprops.Objects[i] := TObject(AValue);
end;

function TQtWidget.CreateWidget(const Params: TCreateParams): QWidgetH;
var
  Parent: QWidgetH;
begin
  Parent := TQtWidget(LCLObject.Parent.Handle).Widget;
  Widget := QWidget_create(Parent);
  Result := Widget;
end;

procedure TQtWidget.SetGeometry;
begin
  with LCLObject do
    QWidget_setGeometry(Widget, Left, Top, Width, Height);
end;

{ TQtAbstractButton }

{------------------------------------------------------------------------------
  Function: TQtAbstractButton.SetColor
  Params:  QColorH
  Returns: Nothing

  Changes the color of a widget
 ------------------------------------------------------------------------------}
procedure TQtAbstractButton.SetColor(const Value: PQColor);
var
  Palette: QPaletteH;
begin
  Palette := QPalette_create(QWidget_palette(Widget));
  try
    QPalette_setColor(Palette, QPaletteButton, Value);
    QWidget_setPalette(Widget, Palette);
  finally
    QPalette_destroy(Palette);
  end;
end;

{------------------------------------------------------------------------------
  Function: TQtAbstractButton.SetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtAbstractButton.SetText(text: PWideString);
begin
  QAbstractButton_setText(QAbstractButtonH(Widget), text);
end;

{------------------------------------------------------------------------------
  Function: TQtAbstractButton.Text
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtAbstractButton.Text(retval: PWideString);
begin
  QAbstractButton_text(QAbstractButtonH(Widget), retval);
end;

{------------------------------------------------------------------------------
  Function: TQtAbstractButton.isChecked
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtAbstractButton.isChecked: Boolean;
begin
  Result := QAbstractButton_isChecked(QAbstractButtonH(Widget));
end;

{------------------------------------------------------------------------------
  Function: TQtAbstractButton.setChecked
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtAbstractButton.setChecked(p1: Boolean);
begin
  QAbstractButton_setChecked(QAbstractButtonH(Widget), p1);
end;

{------------------------------------------------------------------------------
  Function: TQtAbstractButton.SignalPressed
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtAbstractButton.SignalPressed; cdecl;
var
  Msg: TLMessage;
begin
  FillChar(Msg, SizeOf(Msg), #0);
  Msg.Msg := LM_KEYDOWN;
  DeliverMessage(Msg);
end;

{------------------------------------------------------------------------------
  Function: TQtAbstractButton.SignalReleased
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtAbstractButton.SignalReleased; cdecl;
var
  Msg: TLMessage;
begin
  FillChar(Msg, SizeOf(Msg), #0);
  Msg.Msg := LM_KEYUP;
  DeliverMessage(Msg);
end;

{------------------------------------------------------------------------------
  Function: TQtAbstractButton.SignalClicked
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtAbstractButton.SignalClicked(Checked: Boolean = False); cdecl;
var
  Msg: TLMessage;
begin
  FillChar(Msg, SizeOf(Msg), #0);
  Msg.Msg := LM_CHANGED;
  DeliverMessage(Msg);
end;

{------------------------------------------------------------------------------
  Function: TQtAbstractButton.SignalClicked2
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtAbstractButton.SignalClicked2; cdecl;
var
  Msg: TLMessage;
begin
  FillChar(Msg, SizeOf(Msg), #0);
  Msg.Msg := LM_CLICKED;
  DeliverMessage(Msg);
end;

{------------------------------------------------------------------------------
  Function: TQtAbstractButton.SignalToggled
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtAbstractButton.SignalToggled(Checked: Boolean); cdecl;
begin
 {use this for TToggleButton }
end;


{ TQtPushButton }

function TQtPushButton.CreateWidget(const AParams: TCreateParams): QWidgetH;
var
  Str: WideString;
  Parent: QWidgetH;
begin
  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtPushButton.Create Left:', dbgs(LCLObject.Left), ' Top:', dbgs(LCLObject.Top));
  {$endif}

  Str := UTF8Decode(LCLObject.Caption);
  Parent := TQtWidget(LCLObject.Parent.Handle).GetContainerWidget;
  Result := QPushButton_create(@Str, Parent);
end;

{------------------------------------------------------------------------------
  Function: TQtPushButton.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtPushButton.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtPushButton.Destroy');
  {$endif}

  QPushButton_destroy(QPushButtonH(Widget));
  Widget:=nil;

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtPushButton.SlotClicked
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtPushButton.SlotClicked; cdecl;
var
  Msg: TLMessage;
begin
  Msg.Msg := LM_CLICKED;

  try
    LCLObject.WindowProc(TLMessage(Msg));
  except
    Application.HandleException(nil);
  end;

{  if (TLMessage(AMessage).Msg=LM_PAINT)
  or (TLMessage(AMessage).Msg=LM_INTERNALPAINT)
  or (TLMessage(AMessage).Msg=LM_GtkPaint) then
    CurrentSentPaintMessageTarget:=TObject(Target);

  try
    if TObject(Target) is TControl
    then TControl(Target).WindowProc(TLMessage(Msg))
    else TObject(Target).Dispatch(TLMessage(Msg));
  except
    Application.HandleException(nil);
  end;

  CurrentSentPaintMessageTarget:=nil;}
end;

{ TQtMainWindow }

function TQtMainWindow.CreateWidget(const AParams: TCreateParams): QWidgetH;
var
  w: QWidgetH;
  r: TRect;
{$ifdef USE_QT_4_3}
  mdihandle: QMdiAreaH;
  toolbar: QToolBarH;
{$endif}
begin
  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtMainWindow.CreateWidget Name: ', LCLObject.Name);
  {$endif}

  w := QApplication_activeWindow;

  if not Assigned(w) and not ((Application.MainForm <> nil) and (Application.MainForm.Visible)) then
  begin
    Result := QMainWindow_create(nil, QtWindow);
    
    MenuBar := TQtMenuBar.Create(Result);
    
    {------------------------------------------------------------------------------
      To make sure Qt will manage automatically the size of the menu,
      we need to both use setMenuBar and also create a central widget
      on the form, on top of which, the other components will be placed

      If we are supporting MDI, then the MDIArea widget can be used as
      central widget. If not, we create an empty widget.

      In both cases the widget is transparent to events, so the main
      window will receive and handle them
     ------------------------------------------------------------------------------}

    if Assigned(Application.MainForm) and Assigned(Application.MainForm.Menu) then
      QMainWindow_setMenuBar(QMainWindowH(Result), QMenuBarH(MenuBar.Widget));
     
    {$ifdef USE_QT_4_3}
      if (Application.MainForm <> nil) and (Application.MainForm.FormStyle = fsMDIForm) then
      begin
        MDIAreaHandle := QMdiArea_create(Result);
        CentralWidget := MDIAreaHandle;
      end
      else
      begin
        CentralWidget := QWidget_create(Result);
        MDIAreaHandle := nil
      end;
      QMainWindow_setCentralWidget(QMainWindowH(Result), CentralWidget);
      QMainWindow_setDockOptions(QMainWindowH(Result), QMainWindowAnimatedDocks);
    {$else}
      CentralWidget := QWidget_create(Result);

      QMainWindow_setCentralWidget(QMainWindowH(Result), CentralWidget);
    {$endif}
  end
  else
  begin
    {$ifdef USE_QT_4_3}
      if (LCLObject is TCustomForm) and (TCustomForm(LCLObject).FormStyle = fsMDIChild) then
      begin
        Result := QMdiSubWindow_create(niL, QtWindow);
      
        mdiHandle := TQtMainWindow(Application.MainForm.Handle).MDIAreaHandle;
        if Assigned(mdiHandle) then
          QMdiArea_addSubWindow(mdiHandle, QMdiSubWindowH(Result), QtWindow);
      end
      else
        Result := QWidget_create(nil, QtWindow);
    {$else}
      Result := QWidget_create(nil, QtWindow);
    {$endif}
    
    // Main menu bar
    MenuBar := TQtMenuBar.Create(Result);
    CentralWidget := QWidget_create(Result);

    LayoutWidget := QBoxLayout_create(QBoxLayoutTopToBottom, Result);
    QBoxLayout_setSpacing(LayoutWidget, 0);
    QLayout_setContentsMargins(LayoutWidget, 0, 0, 0, 0);
    QLayout_setMenuBar(LayoutWidget, MenuBar.Widget);
    QLayout_addWidget(LayoutWidget, CentralWidget);
    QWidget_setLayout(Result, QLayoutH(LayoutWidget));
  end;
end;

{------------------------------------------------------------------------------
  Function: TQtMainWindow.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtMainWindow.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtMainWindow.Destroy');
  {$endif}

  if Widget <> nil then
  begin
    QMainWindow_destroy(QMainWindowH(Widget));

    Widget := nil;
  end;

  { The main window takes care of the menubar handle}
  
  MenuBar.Widget := nil;
  MenuBar.Free;

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtMainWindow.GetContainerWidget
  Params:  None
  Returns: Nothing

  The main window has a special container widget to handle the size of the menu
 ------------------------------------------------------------------------------}
function TQtMainWindow.GetContainerWidget: QWidgetH;
begin
  {$ifdef USE_QT_4_3}
    if (CentralWidget <> nil) then
      Result := CentralWidget
    else
      Result := Widget;
  {$else}
    if CentralWidget <> nil then
      Result := CentralWidget
    else
      Result := Widget;
  {$endif}
end;

{------------------------------------------------------------------------------
  Function: TQtMainWindow.setTabOrders
  Params:  None
  Returns: Nothing
  
  Sets the tab order of all controls on a form.
 ------------------------------------------------------------------------------}
procedure TQtMainWindow.setTabOrders;
var
 i: Integer;
 Form: TForm;
 List: TFPList;
begin
 Form := TForm(LCLObject);

 List := TFPList.Create;
 Form.GetTabOrderList(List);

 for i := 0 to List.Count - 2 do
 begin
   setTabOrder(TQtWidget(TWinControl(List.Items[i]).Handle),
    TQtWidget(TWinControl(List.Items[i + 1]).Handle));

   {$ifdef VerboseQt}
     WriteLn('Setting Tab Order first: ', TWinControl(List.Items[i]).Name, ' second: ',
      TWinControl(List.Items[i + 1]).Name);
   {$endif}
 end;

 { The last element points to the first }
 if List.Count > 1 then
 begin
   setTabOrder(TQtWidget(TWinControl(List.Items[List.Count - 1]).Handle),
    TQtWidget(TWinControl(List.Items[0]).Handle));

   {$ifdef VerboseQt}
     WriteLn('Setting Tab Order first: ', TWinControl(List.Items[List.Count - 1]).Name, ' second: ',
      TWinControl(List.Items[0]).Name);
   {$endif}
 end;

 List.Free;
end;

{------------------------------------------------------------------------------
  Function: TQtMainWindow.EventFilter
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtMainWindow.EventFilter(Sender: QObjectH; Event: QEventH): Boolean;
  cdecl;
begin
  Result := False;

  case QEvent_type(Event) of
   QEventWindowStateChange: SlotWindowStateChange;
  else
   // Inherited Callbacks
   inherited EventFilter(Sender, Event);
  end;
end;

{------------------------------------------------------------------------------
  Function: TQtMainWindow.SlotWindowStateChange
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtMainWindow.SlotWindowStateChange; cdecl;
var
  Msg: TLMSize;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtMainWindow.SlotWindowStateChange');
  {$endif}

  FillChar(Msg, SizeOf(Msg), #0);

  Msg.Msg := LM_SIZE;

  case QWidget_windowState(Widget) of
   QtWindowMinimized: Msg.SizeType := SIZEICONIC;
   QtWindowMaximized: Msg.SizeType := SIZEFULLSCREEN;
   QtWindowFullScreen: Msg.SizeType := SIZEFULLSCREEN;
  else
   Msg.SizeType := SIZENORMAL;
  end;

  Msg.SizeType := Msg.SizeType or Size_SourceIsInterface;

  Msg.Width := QWidget_width(Widget);
  Msg.Height := QWidget_height(Widget);

  try
    LCLObject.WindowProc(TLMessage(Msg));
  except
    Application.HandleException(nil);
  end;
end;

procedure TQtMainWindow.setShowInTaskBar(AValue: Boolean);
var
  w: QWidgetH;
  Flags: QtWindowFlags;
  Visible: Boolean;
begin
  if not AValue then
  begin
    w := TQtMainWindow(Application.MainForm.Handle).Widget;
    if w <> Widget then
    begin
      Visible := getVisible;
      Flags := windowFlags;
      setParent(w);
      setWindowFlags(Flags);
      setVisible(Visible);
    end;
  end
  else
  begin
    Visible := getVisible;
    Flags := windowFlags;
    setParent(nil);
    setWindowFlags(Flags);
    setVisible(Visible);
  end;
end;

{ TQtStaticText }

function TQtStaticText.CreateWidget(const AParams: TCreateParams): QWidgetH;
var
  Str: WideString;
  Parent: QWidgetH;
begin
  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtStaticText.Create');
  {$endif}

  Parent := TQtWidget(LCLObject.Parent.Handle).GetContainerWidget;
  Result := QLabel_create(Parent);
  QWidget_setAutoFillBackground(Result, True);
end;

{------------------------------------------------------------------------------
  Function: TQtStaticText.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtStaticText.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtStaticText.Destroy');
  {$endif}

  QLabel_destroy(QLabelH(Widget));
  Widget:=nil;

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtStaticText.SetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtStaticText.SetText(text: PWideString);
begin
  QLabel_setText(QLabelH(Widget), text);
end;

{------------------------------------------------------------------------------
  Function: TQtStaticText.Text
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtStaticText.Text(retval: PWideString);
begin
  QLabel_text(QLabelH(Widget), retval);
end;

{ TQtTimer }

{------------------------------------------------------------------------------
  Function: TQtTimer.CreateTimer
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtTimer.CreateTimer(Interval: integer;
  const TimerFunc: TFNTimerProc; App: QObjectH);
var
  Method: TMethod;
  Hook : QObject_hookH;
begin
  AppObject := App;

  Id := QObject_startTimer(AppObject, Interval);

  CallbackFunc := TimerFunc;

  // Callback Event

  Hook := QObject_hook_create(AppObject);

  TEventFilterMethod(Method) := EventFilter;

  QObject_hook_hook_events(Hook, Method);
end;

{------------------------------------------------------------------------------
  Function: TQtTimer.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtTimer.Destroy;
begin
  QObject_killTimer(AppObject, id);

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtTimer.EventFilter
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtTimer.EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl;
begin
  Result:=False;

  if QEvent_type(Event) = QEventTimer then
  begin
    QEvent_accept(Event);
    
    if Assigned(CallbackFunc) then CallbackFunc;
  end;
end;

{ TQtCheckBox }

function TQtCheckBox.CreateWidget(const AParams: TCreateParams): QWidgetH;
var
  Str: WideString;
  Parent: QWidgetH;
begin
  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtCheckBox.Create');
  {$endif}
  
  if (LCLObject.Parent is TCustomCheckGroup) then
  begin
    Result := QCheckBox_create;
    
    QGridLayout_addWidget(TQtGroupBox(LCLObject.Parent.Handle).BoxLayout, Result);

    if TQtGroupBox(LCLObject.Parent.Handle).ButtonGroup.GetExclusive then
    TQtGroupBox(LCLObject.Parent.Handle).ButtonGroup.SetExclusive(False);

    TQtGroupBox(LCLObject.Parent.Handle).ButtonGroup.AddButton(QCheckBoxH(Result));
  end
  else
  begin
    Parent := TQtWidget(LCLObject.Parent.Handle).GetContainerWidget;
    Result := QCheckBox_create(Parent);
  end;

end;

procedure TQtCheckBox.SetGeometry;
begin
  if LCLObject.Parent is TCustomCheckGroup then
    exit;
  inherited SetGeometry;
end;

{------------------------------------------------------------------------------
  Function: TQtCheckBox.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtCheckBox.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtCheckBox.Destroy');
  {$endif}

  QCheckBox_destroy(QCheckBoxH(Widget));
  Widget:=nil;

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtCheckBox.EventFilter
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtCheckBox.EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl;
begin
  Result := False;
  
  inherited EventFilter(Sender, Event);
end;

{------------------------------------------------------------------------------
  Function: TQtCheckBox.CheckState
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtCheckBox.CheckState: QtCheckState;
begin
  Result := QCheckBox_checkState(QCheckBoxH(Widget));
end;

{------------------------------------------------------------------------------
  Function: TQtCheckBox.setCheckState
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtCheckBox.setCheckState(state: QtCheckState);
begin
  QCheckBox_setCheckState(QCheckBoxH(Widget), state);
end;

{------------------------------------------------------------------------------
  Function: TQtCheckBox.signalStateChanged
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtCheckBox.signalStateChanged(p1: Integer); cdecl;
var
  Msg: TLMessage;
begin
  FillChar(Msg, SizeOf(Msg), #0);
  Msg.Msg := LM_CHANGED;
  DeliverMessage(Msg);
end;

{ TQtRadioButton }

function TQtRadioButton.CreateWidget(const AParams: TCreateParams): QWidgetH;
var
  Str: WideString;
  Parent: QWidgetH;
begin
  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtRadioButton.Create');
  {$endif}

  if (LCLObject.Parent is TCustomRadioGroup) then
  begin
    Result := QRadioButton_create;

{$ifdef QT_HIDDEN_BUTTON_WORKAROUND}
    if LCLObject.Name = 'HiddenRadioButton' then
      QWidget_hide(Result)
    else
    begin
{$endif}
      QGridLayout_addWidget(TQtGroupBox(LCLObject.Parent.Handle).BoxLayout, Result);

      if not TQtGroupBox(LCLObject.Parent.Handle).ButtonGroup.GetExclusive then
      TQtGroupBox(LCLObject.Parent.Handle).ButtonGroup.SetExclusive(True);

      TQtGroupBox(LCLObject.Parent.Handle).ButtonGroup.AddButton(QRadioButtonH(Result));
{$ifdef QT_HIDDEN_BUTTON_WORKAROUND}
    end;
{$endif}
  end
  else
  begin
    Parent := TQtWidget(LCLObject.Parent.Handle).GetContainerWidget;
    Result := QRadioButton_create(Parent);
  end;
end;

procedure TQtRadioButton.SetGeometry;
begin
  if LCLObject.Parent is TCustomRadioGroup then
    exit;
  inherited SetGeometry;
end;

{------------------------------------------------------------------------------
  Function: TQtRadioButton.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtRadioButton.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtRadioButton.Destroy');
  {$endif}

  QRadioButton_destroy(QRadioButtonH(Widget));
  Widget:=nil;

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtRadioButton.EventFilter
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtRadioButton.EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl;
begin
  Result := False;

  // Inherited Callbacks
  inherited EventFilter(Sender, Event);
end;

{ TQtGroupBox }

function TQtGroupBox.CreateWidget(const AParams: TCreateParams): QWidgetH;
var
  Parent: QWidgetH;
  R: TRect;
  Method: TMethod;
begin
  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtGroupBox.Create ');
  {$endif}
  Parent := TQtWidget(LCLObject.Parent.Handle).GetContainerWidget;
  Result := QGroupBox_create(Parent);
end;


{------------------------------------------------------------------------------
  Function: TQtGroupBox.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtGroupBox.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtGroupBox.Destroy');
  {$endif}

  QGroupBox_destroy(QGroupBoxH(Widget));
  Widget:=nil;

  inherited Destroy;
end;

{ TQtFrame }

function TQtFrame.CreateWidget(const AParams: TCreateParams): QWidgetH;
var
  Parent: QWidgetH;
begin
  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtFrame.Create');
  {$endif}
  Parent := TQtWidget(LCLObject.Parent.Handle).GetContainerWidget;
  Result := QFrame_create(Parent);
  QWidget_setAutoFillBackground(Result, True);
end;

{------------------------------------------------------------------------------
  Function: TQtFrame.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtFrame.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtFrame.Destroy');
  {$endif}

  QFrame_destroy(QFrameH(Widget));
  Widget:=nil;

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtFrame.setFrameStyle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtFrame.setFrameStyle(p1: Integer);
begin
  QFrame_setFrameStyle(QFrameH(Widget), p1);
end;

{------------------------------------------------------------------------------
  Function: TQtFrame.setFrameShape
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtFrame.setFrameShape(p1: QFrameShape);
begin
  QFrame_setFrameShape(QFrameH(Widget), p1);
end;

{------------------------------------------------------------------------------
  Function: TQtFrame.setFrameShadow
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtFrame.setFrameShadow(p1: QFrameShadow);
begin
  QFrame_setFrameShadow(QFrameH(Widget), p1);
end;

{------------------------------------------------------------------------------
  Function: TQtFrame.setTextColor
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtFrame.setTextColor(const Value: PQColor);
var
  Palette: QPaletteH;
begin
  Palette := QPalette_create(QWidget_palette(Widget));
  try
    QPalette_setColor(Palette, QPaletteForeground, Value);
    QWidget_setPalette(Widget, Palette);
  finally
    QPalette_destroy(Palette);
  end;
end;

function TQtAbstractSlider.CreateWidget(const AParams: TCreateParams
  ): QWidgetH;
var
  Parent: QWidgetH;
begin
  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtAbstractSlider.Create');
  {$endif}
  
  FSliderPressed := False;
  FSliderReleased:= False;

  Parent := TQtWidget(LCLObject.Parent.Handle).GetContainerWidget;
  Result := QAbstractSlider_create(Parent);
end;

{------------------------------------------------------------------------------
  Function: TQtAbstractSlider.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtAbstractSlider.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtAbstractSlider.Destroy');
  {$endif}
  QAbstractSlider_destroy(QAbstractSliderH(Widget));
  Widget:=nil;
  
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtAbstractSlider.rangeChanged
  Params:  minimum,maximum: Integer
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtAbstractSlider.SlotRangeChanged(minimum: Integer; maximum: Integer); cdecl;
begin
  { TODO: find out what needs to be done on rangeChanged event
    Possibilities: repaint or recount pageSize() }
 {$ifdef VerboseQt}
  writeln('TQtAbstractSlider.rangeChanged() to min=',minimum,' max=',maximum);
 {$endif}
end;

{------------------------------------------------------------------------------
  Function: TQtAbstractSlider.setInvertedAppereance
  Params:  p1: Boolean
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtAbstractSlider.setInvertedAppereance(p1: Boolean);
begin
  QAbstractSlider_setInvertedAppearance(QAbstractSliderH(Widget), p1);
end;

{------------------------------------------------------------------------------
  Function: TQtAbstractSlider.setInvertedControls
  Params:  p1: Boolean
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtAbstractSlider.setInvertedControls(p1: Boolean);
begin
  QAbstractSlider_setInvertedControls(QAbstractSliderH(Widget), p1);
end;

{------------------------------------------------------------------------------
  Function: TQtAbstractSlider.setMaximum
  Params:  p1: Integer
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtAbstractSlider.setMaximum(p1: Integer);
begin
  QAbstractSlider_setMaximum(QAbstractSliderH(Widget), p1);
end;

{------------------------------------------------------------------------------
  Function: TQtAbstractSlider.setMinimum
  Params:  p1: Integer
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtAbstractSlider.setMinimum(p1: Integer);
begin
  QAbstractSlider_setMinimum(QAbstractSliderH(Widget), p1);
end;

{------------------------------------------------------------------------------
  Function: TQtAbstractSlider.setOrientation
  Params:  p1: QtOrientation (QtHorizontal or QtVertical)
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtAbstractSlider.setOrientation(p1: QtOrientation);
begin
  QAbstractSlider_setOrientation(QAbstractSliderH(Widget), p1);
end;

{------------------------------------------------------------------------------
  Function: TQtAbstractSlider.setPageStep
  Params:  p1: Integer
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtAbstractSlider.setPageStep(p1: Integer);
begin
  QAbstractSlider_setPageStep(QAbstractSliderH(Widget), p1);
end;

{------------------------------------------------------------------------------
  Function: TQtAbstractSlider.setRange
  Params:  minimum,maximum: Integer
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtAbstractSlider.setRange(minimum: Integer; maximum: Integer);
begin
  QAbstractSlider_setRange(QAbstractSliderH(Widget), minimum, maximum);
end;

{------------------------------------------------------------------------------
  Function: TQtAbstractSlider.setSingleStep
  Params:  p1: Integer
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtAbstractSlider.setSingleStep(p1: Integer);
begin
  QAbstractSlider_setSingleStep(QAbstractSliderH(Widget), p1);
end;

{------------------------------------------------------------------------------
  Function: TQtAbstractSlider.setSliderDown
  Params:  p1: Boolean
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtAbstractSlider.setSliderDown(p1: Boolean);
begin
  QAbstractSlider_setSliderDown(QAbstractSliderH(Widget), p1);
end;


{------------------------------------------------------------------------------
  Function: TQtAbstractSlider.setSliderPosition
  Params:  p1: Integer
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtAbstractSlider.setSliderPosition(p1: Integer);
begin
  QAbstractSlider_setSliderPosition(QAbstractSliderH(Widget), p1);
end;

{------------------------------------------------------------------------------
  Function: TQtAbstractSlider.setTracking
  Params:  p1: Boolean
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtAbstractSlider.setTracking(p1: Boolean);
begin
  QAbstractSlider_setTracking(QAbstractSliderH(Widget), p1);
end;

{-----------------------------------------------------------------------------
  Function: TQtAbstractSlider.setValue
  Params:  p1: Integer
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtAbstractSlider.setValue(p1: Integer);
begin
  QAbstractSlider_setValue(QAbstractSliderH(Widget), p1);
end;

procedure TQtAbstractSlider.SlotSliderMoved(p1: Integer); cdecl;
var
   Msg: PLMessage;
   LMScroll: TLMScroll;
begin
 {$ifdef VerboseQt}
  writeln('TQtAbstractSlider.sliderMoved() to pos=',p1);
 {$endif}
 
  FillChar(Msg, SizeOf(Msg), #0);
  FillChar(LMScroll, SizeOf(LMScroll), #0);

  LMScroll.ScrollBar := LCLObject.Handle;
   
  if QAbstractSlider_orientation(QAbstractSliderH(Widget)) = QtHorizontal then
  LMScroll.Msg := LM_HSCROLL
  else
  LMScroll.Msg := LM_VSCROLL;

  LMScroll.Pos := p1;
  LMScroll.ScrollCode := SIF_POS; { SIF_TRACKPOS }

  Msg := @LMScroll;
  
  try
    if (TScrollBar(LCLObject).Position <> p1)
    and (Assigned(LCLObject.Parent)) then
    LCLObject.Parent.WindowProc(Msg^);
  except
    Application.HandleException(nil);
  end;
end;

procedure TQtAbstractSlider.SlotSliderPressed; cdecl;
begin
 {$ifdef VerboseQt}
  writeln('TQtAbstractSlider.sliderPressed()');
 {$endif}
 FSliderPressed := True;
 FSliderReleased := False;
end;

procedure TQtAbstractSlider.SlotSliderReleased; cdecl;
begin
 {$ifdef VerboseQt}
  writeln('TQtAbstractSlider.sliderReleased()');
 {$endif}
 FSliderPressed := False;
 FSliderReleased := True;
end;

procedure TQtAbstractSlider.SlotValueChanged(p1: Integer); cdecl;
var
  Msg: PLMessage;
  LMScroll: TLMScroll;
begin
 {$ifdef VerboseQt}
  writeln('TQtAbstractSlider.SlotValueChanged()');
 {$endif}
 
  FillChar(Msg, SizeOf(Msg), #0);
  FillChar(LMScroll, SizeOf(LMScroll), #0);

  LMScroll.ScrollBar := LCLObject.Handle;

  if QAbstractSlider_orientation(QAbstractSliderH(Widget)) = QtHorizontal then
  LMScroll.Msg := LM_HSCROLL
  else
  LMScroll.Msg := LM_VSCROLL;
  
  LMScroll.Pos := p1;
  LMScroll.ScrollCode := SIF_POS;

  Msg := @LMScroll;
  try
    if not SliderPressed and Assigned(LCLObject.Parent)
    and (p1 <> TScrollBar(LCLObject).Position) then
    begin
      LCLObject.Parent.WindowProc(Msg^);
    end;
  except
    Application.HandleException(nil);
  end;
end;


{ TQtScrollBar }

function TQtScrollBar.CreateWidget(const AParams: TCreateParams): QWidgetH;
var
  Parent: QWidgetH;
begin
  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtScrollBar.Create');
  {$endif}
  Parent := TQtWidget(LCLObject.Parent.Handle).GetContainerWidget;
  Result := QScrollBar_create(Parent);
end;

{ TQtToolBar }

function TQtToolBar.CreateWidget(const AParams: TCreateParams):QWidgetH;
var
  Parent: QWidgetH;
begin
  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtToolBar.Create');
  {$endif}
  Parent := TQtWidget(LCLObject.Parent.Handle).GetContainerWidget;
  Result := QToolBar_create(Parent);
end;

{ TQtToolButton }

function TQtToolButton.CreateWidget(const AParams: TCreateParams):QWidgetH;
var
  Parent: QWidgetH;
begin
  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtToolButton.Create');
  {$endif}
  Parent := TQtWidget(LCLObject.Parent.Handle).GetContainerWidget;
  Result := QToolButton_create(Parent);
end;

{ TQtTrackBar }

function TQtTrackBar.CreateWidget(const AParams: TCreateParams): QWidgetH;
var
  Parent: QWidgetH;
begin
  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtTrackBar.Create');
  {$endif}
  Parent := TQtWidget(LCLObject.Parent.Handle).GetContainerWidget;
  Result := QSlider_create(Parent);
end;

{------------------------------------------------------------------------------
  Function: TQtTrackBar.setTickPosition
  Params:  Value: QSliderTickPosition
  Returns: Nothing
 ------------------------------------------------------------------------------ }
procedure TQtTrackBar.setTickPosition(Value: QSliderTickPosition);
begin
  QSlider_setTickPosition(QSliderH(Widget), Value);
end;

{------------------------------------------------------------------------------
  Function: TQtTrackBar.setTickInterval
  Params:  Value: Integer
  Returns: Nothing
 ------------------------------------------------------------------------------ }
procedure TQtTrackBar.SetTickInterval(Value: Integer);
begin
  QSlider_setTickInterval(QSliderH(Widget), Value);
end;

procedure TQtTrackBar.SlotSliderMoved(p1: Integer); cdecl;
var
  Msg: TLMessage;
begin
 {$ifdef VerboseQt}
  writeln('TQtTrackBar.SlotSliderMoved()');
 {$endif}
  FillChar(Msg, SizeOf(Msg), #0);

  Msg.Msg := LM_CHANGED;
  try
    if (TTrackBar(LCLObject).Position<>p1) then
      LCLObject.WindowProc(Msg);
  except
    Application.HandleException(nil);
  end;
end;

procedure TQtTrackBar.SlotValueChanged(p1: Integer); cdecl;
var
  Msg: TLMessage;
begin
 {$ifdef VerboseQt}
  writeln('TQtTrackBar.SlotValueChanged()');
 {$endif}

  FillChar(Msg, SizeOf(Msg), #0);

  Msg.Msg := LM_CHANGED;
  try
    if not SliderPressed and (TTrackBar(LCLObject).Position<>p1) then
      LCLObject.WindowProc(Msg);
  except
    Application.HandleException(nil);
  end;
end;

procedure TQtTrackBar.SlotRangeChanged(minimum: Integer; maximum: Integer); cdecl;
var
  Msg: TLMessage;
begin
 {$ifdef VerboseQt}
  writeln('TQtTrackBar.SlotRangeChanged()');
 {$endif}
  FillChar(Msg, SizeOf(Msg), #0);

  Msg.Msg := LM_CHANGED;
  try
    DeliverMessage(Msg);
  except
    Application.HandleException(nil);
  end;
end;


{ TQtLineEdit }

function TQtLineEdit.CreateWidget(const AParams: TCreateParams): QWidgetH;
var
  Parent: QWidgetH;
  Str: WideString;
begin
  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtLineEdit.Create');
  {$endif}
  Parent := TQtWidget(LCLObject.Parent.Handle).GetContainerWidget;
  Str := UTF8Decode((LCLObject as TCustomEdit).Text);
  Result := QLineEdit_create(@Str, Parent);
end;

{------------------------------------------------------------------------------
  Function: TQtLineEdit.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtLineEdit.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtLineEdit.Destroy');
  {$endif}

  QLineEdit_destroy(QLineEditH(Widget));
  Widget:=nil;

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtLineEdit.EventFilter
  Params:  QObjectH, QEventH
  Returns: boolean

  Overrides TQtWidget EventFilter()
 ------------------------------------------------------------------------------}
function TQtLineEdit.EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl;
begin
  Result := False;
  case QEvent_type(Event) of
    QEventFocusIn:
    begin
      if QFocusEvent_reason(QFocusEventH(Event)) in
        [QtTabFocusReason,QtBacktabFocusReason,QtActiveWindowFocusReason,
         QtShortcutFocusReason, QtOtherFocusReason] then
      begin
        // it would be better if we have AutoSelect published from TCustomEdit
        // then TMaskEdit also belongs here.
        if ((LCLObject is TEdit) and (TEdit(LCLObject).AutoSelect)) then
          QLineEdit_selectAll(QLineEditH(Widget));
      end;
    end;
  end;
  inherited EventFilter(Sender, Event);
end;

{------------------------------------------------------------------------------
  Function: TQtLineEdit.SetColor
  Params:  QColorH
  Returns: Nothing

  Changes the color of a widget
 ------------------------------------------------------------------------------}
procedure TQtLineEdit.SetColor(const Value: PQColor);
var
  Palette: QPaletteH;
begin
  Palette := QPalette_create(QWidget_palette(Widget));
  try
    QPalette_setColor(Palette, QPaletteBase, Value);
    QWidget_setPalette(Widget, Palette);
  finally
    QPalette_destroy(Palette);
  end;
end;

{------------------------------------------------------------------------------
  Function: TQtLineEdit.SignalTextChanged
  Params:  PWideString
  Returns: Nothing

  Fires OnChange() event of TCustomEdit
 ------------------------------------------------------------------------------}
procedure TQtLineEdit.SignalTextChanged(p1: PWideString); cdecl;
var
   Msg: TLMessage;
begin
   FillChar(Msg, SizeOf(Msg), #0);
   Msg.Msg := CM_TEXTCHANGED;
   DeliverMessage(Msg);
end;

{ TQtTextEdit }

function TQtTextEdit.CreateWidget(const AParams: TCreateParams): QWidgetH;
var
  Parent: QWidgetH;
  Str: WideString;
begin
  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtTextEdit.Create');
  {$endif}

  Parent := TQtWidget(LCLObject.Parent.Handle).GetContainerWidget;
  Str := (LCLObject as TCustomMemo).Text;
  Result := QTextEdit_create(Parent);
  QTextEdit_setAlignment(QTextEditH(Result), AlignmentMap[(LCLObject as TCustomMemo).Alignment]);
  QTextEdit_setPlainText(QTextEditH(Result), @Str);

  QTextEdit_setReadOnly(QTextEditH(Result), (LCLObject as TCustomMemo).ReadOnly);

  if (LCLObject as TCustomMemo).WordWrap then
     QTextEdit_setLineWrapMode(QTextEditH(Result),QTextEditWidgetWidth)
  else
     QTextEdit_setLineWrapMode(QTextEditH(Result),QTextEditNoWrap);
end;

{------------------------------------------------------------------------------
  Function: TQtTextEdit.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtTextEdit.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtTextEdit.Destroy');
  {$endif}

  QTextEdit_destroy(QTextEditH(Widget));
  Widget:=nil;

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtTextEdit.SetColor
  Params:  QColorH
  Returns: Nothing

  Changes the color of a widget
 ------------------------------------------------------------------------------}
procedure TQtTextEdit.SetColor(const Value: PQColor);
var
  Palette: QPaletteH;
begin
  Palette := QPalette_create(QWidget_palette(Widget));
  try
    QPalette_setColor(Palette, QPaletteBase, Value);
    QWidget_setPalette(Widget, Palette);
  finally
    QPalette_destroy(Palette);
  end;
end;

procedure TQtTextEdit.SetAlignment(const AAlignment: TAlignment);
var
  TextCursor: QTextCursorH;
begin
  // QTextEdit supports alignment for every paragraph. We need to align all text.
  // So, we should select all text, set format, and clear selection
  
  // 1. Select all text
  QTextEdit_selectAll(QTextEditH(Widget));
  
  // 2. Set format
  QTextEdit_setAlignment(QTextEditH(Widget), AlignmentMap[(LCLObject as TCustomMemo).Alignment]);
  
  // 3. Clear selection. To unselect all document we must create new text cursor,
  // get format from Text Edit, clear selection in cursor and set it back to Text Edit
  TextCursor := QTextCursor_create();
  QTextEdit_textCursor(QTextEditH(Widget), TextCursor);
  QTextCursor_clearSelection(TextCursor);
  QTextEdit_setTextCursor(QTextEditH(Widget), TextCursor);
  QTextCursor_destroy(TextCursor);
end;

{------------------------------------------------------------------------------
  Function: TQtTextEdit.SignalTextChanged
  Params:  none
  Returns: Nothing

  Fires OnChange() event of TCustomMemo
 ------------------------------------------------------------------------------}
procedure TQtTextEdit.SignalTextChanged; cdecl;
var
   Msg: TLMessage;
begin
   FillChar(Msg, SizeOf(Msg), #0);
   Msg.Msg := CM_TEXTCHANGED;
   DeliverMessage(Msg);
end;

{ TQtTabWidget }

function TQtTabWidget.CreateWidget(const AParams: TCreateParams): QWidgetH;
var
  Parent: QWidgetH;
begin
  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtTabWidget.Create');
  {$endif}
  Parent := TQtWidget(LCLObject.Parent.Handle).GetContainerWidget;
  Result := QTabWidget_create(Parent);
end;

{------------------------------------------------------------------------------
  Function: TQtTabWidget.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtTabWidget.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtTabWidget.Destroy');
  {$endif}

  QTabWidget_destroy(QTabWidgetH(Widget));
  Widget:=nil;

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtTabWidget.EventFilter
  Params:  QObjectH, QEventH
  Returns: boolean

  Overrides TQtWidget EventFilter()
 ------------------------------------------------------------------------------}
function TQtTabWidget.EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl;
begin
  Result := False;
  inherited EventFilter(Sender, Event);
end;

{------------------------------------------------------------------------------
  Function: TQtTabWidget.insertTab
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtTabWidget.insertTab(index: Integer; page: QWidgetH; p2: PWideString): Integer;
begin
  Result := QTabWidget_insertTab(QTabWidgetH(Widget), index, page, p2);
end;

{------------------------------------------------------------------------------
  Function: TQtTabWidget.setTabPosition
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtTabWidget.SetTabPosition(ATabPosition: QTabWidgetTabPosition);
begin
  QTabWidget_setTabPosition(QTabWidgetH(Widget), ATabPosition);
end;

{------------------------------------------------------------------------------
  Function: TQtTabWidget.SignalCurrentChanged
  Params:  None
  Returns: Nothing
           Changes ActivePage of TPageControl
 ------------------------------------------------------------------------------}
procedure TQtTabWidget.SignalCurrentChanged(Index: Integer); cdecl;
var
  Msg: TLMNotify;
  Hdr: TNmHdr;
begin

  FillChar(Msg, SizeOf(Msg), #0);

  Msg.Msg := CN_NOTIFY;
  
  Hdr.hwndFrom := LCLObject.Handle;
  Hdr.Code := TCN_SELCHANGE;
  Hdr.idFrom := Index;
  
  Msg.NMHdr := @Hdr;
  try
    DeliverMessage(Msg);
  except
    Application.HandleException(nil);
  end;
end;


{ TQtComboBox }

function TQtComboBox.CreateWidget(const AParams: TCreateParams): QWidgetH;
var
  Parent: QWidgetH;
  Str: WideString;
  i: Integer;
  data: QVariantH;
begin
  {------------------------------------------------------------------------------
    Creates dummy data

    This data is required, passing nil to QComboBox_addItem will cause a crash
   ------------------------------------------------------------------------------}
  data := QVariant_create(10);

  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtComboBox.Create');
  {$endif}
  Parent := TQtWidget(LCLObject.Parent.Handle).GetContainerWidget;
  Result := QComboBox_create(Parent);

  // Add the items to the combo box
  for i := 0 to (LCLObject as TCustomComboBox).Items.Count - 1 do
  begin
    Str := UTF8Decode((LCLObject as TCustomComboBox).Items.Strings[i]);
    QComboBox_addItem(QComboBoxH(Result), @Str, data);
  end;

  // Clean up
  QVariant_destroy(data);
end;

{------------------------------------------------------------------------------
  Function: TQtComboBox.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtComboBox.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtComboBox.Destroy');
  {$endif}

  QComboBox_destroy(QComboBoxH(Widget));
  Widget:=nil;

  inherited Destroy;
end;

procedure TQtComboBox.SetColor(const Value: PQColor);
begin
  inherited SetColor(Value);
end;

{------------------------------------------------------------------------------
  Function: TQtComboBox.currentIndex
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtComboBox.currentIndex: Integer;
begin
  Result := QComboBox_currentIndex(QComboBoxH(Widget));
end;

{------------------------------------------------------------------------------
  Function: TQtGroupBox.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtComboBox.setCurrentIndex(index: Integer);
begin
  QComboBox_setCurrentIndex(QComboBoxH(Widget), index);
end;

procedure TQtComboBox.SlotChange(p1: PWideString); cdecl;
var
  Msg: TLMessage;
begin
  FillChar(Msg, SizeOf(Msg), #0);

  Msg.Msg := LM_CHANGED;

  try
    LCLObject.WindowProc(TLMessage(Msg));
  except
    Application.HandleException(nil);
  end;
end;

procedure TQtComboBox.SlotSelect(index: Integer); cdecl;
var
  Msg: TLMessage;
begin
  FillChar(Msg, SizeOf(Msg), #0);

  Msg.Msg := LM_SELCHANGE;

  try
    LCLObject.WindowProc(TLMessage(Msg));
  except
    Application.HandleException(nil);
  end;
end;

{ TQtAbstractSpinBox }

function TQtAbstractSpinBox.CreateWidget(const AParams: TCreateParams): QWidgetH;
var
  Parent: QWidgetH;
begin
  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtAbstractSpinBox.Create');
  {$endif}
  Parent := TQtWidget(LCLObject.Parent.Handle).GetContainerWidget;
  Result := QAbstractSpinBox_create(Parent);
end;

{------------------------------------------------------------------------------
  Function: TQtAbstractSpinBox.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtAbstractSpinBox.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtAbstractSpinBox.Destroy');
  {$endif}

  QAbstractSpinBox_destroy(QAbstractSpinBoxH(Widget));
  Widget:=nil;

  inherited Destroy;
end;

function TQtAbstractSpinBox.IsReadOnly: Boolean;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtAbstractSpinBox.IsReadOnly');
  {$endif}
  Result := QAbstractSpinBox_isReadOnly(QAbstractSpinBoxH(Widget));
end;

procedure TQtAbstractSpinBox.SetReadOnly(r: Boolean);
begin
  {$ifdef VerboseQt}
    WriteLn('TQtAbstractSpinBox.SetReadOnly');
  {$endif}
  QAbstractSpinBox_setReadOnly(QAbstractSpinBoxH(Widget), r);
end;

procedure TQtAbstractSpinBox.SignalEditingFinished; cdecl;
var
   Msg: TLMessage;
   s: WideString;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtAbstractSpinBox.SignalEditingFinished');
  {$endif}
  FillChar(Msg, SizeOf(Msg), #0);
  { TODO: Find out which message should be sended here
    problem:
     everything is fine when we work with mouse, or
     press TabKey to select next control, but if we
     connect OnKeyDown and say eg. VK_RETURN: SelectNext(ActiveControl, true, true)
     then spinedit text is always selected, nothing important but looks ugly.}
//  Msg.Msg := LM_EXIT;
//  DeliverMessage(Msg);
end;

{ TQtFloatSpinBox }

function TQtFloatSpinBox.CreateWidget(const AParams: TCreateParams): QWidgetH;
var
  Parent: QWidgetH;
begin
  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtFloatSpinBox.Create');
  {$endif}
  Parent := TQtWidget(LCLObject.Parent.Handle).GetContainerWidget;
  Result := QDoubleSpinBox_create(Parent);
end;

procedure TQtFloatSpinBox.SignalValueChanged(p1: Double); cdecl;
var
   Msg: TLMessage;
begin
  FillChar(Msg, SizeOf(Msg), #0);
  Msg.Msg := CM_TEXTCHANGED;
  DeliverMessage(Msg);
end;

{ TQtSpinBox }

function TQtSpinBox.CreateWidget(const AParams: TCreateParams): QWidgetH;
var
  Parent: QWidgetH;
begin
  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtSpinBox.Create');
  {$endif}
  Parent := TQtWidget(LCLObject.Parent.Handle).GetContainerWidget;
  Result := QSpinBox_create(Parent);
end;

procedure TQtSpinBox.SignalValueChanged(p1: Integer); cdecl;
var
   Msg: TLMessage;
begin
  FillChar(Msg, SizeOf(Msg), #0);
  Msg.Msg := CM_TEXTCHANGED;
  DeliverMessage(Msg);
end;

function TQtListWidget.CreateWidget(const AParams: TCreateParams): QWidgetH;
var
  Parent: QWidgetH;
  Text: WideString;
  i: Integer;
begin
  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQListWidget.Create');
  {$endif}
  Parent := TQtWidget(LCLObject.Parent.Handle).GetContainerWidget;
  Result := QListWidget_create(Parent);

  // Sets the initial items
  for I := 0 to TCustomListBox(LCLObject).Items.Count - 1 do
  begin
    Text := UTF8Decode(TCustomListBox(LCLObject).Items.Strings[i]);
    QListWidget_addItem(QListWidgetH(Result), @Text);
  end;
end;

{------------------------------------------------------------------------------
  Function: TQtListWidget.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtListWidget.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtListWidget.Destroy');
  {$endif}

  QListWidget_destroy(QListWidgetH(Widget));
  Widget:=nil;

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtListWidget.SlotSelectionChange
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtListWidget.SlotSelectionChange(current: QListWidgetItemH;
  previous: QListWidgetItemH); cdecl;
var
  Msg: TLMessage;
begin
  FillChar(Msg, SizeOf(Msg), #0);

  Msg.Msg := LM_SELCHANGE;

  try
    LCLObject.WindowProc(TLMessage(Msg));
  except
    Application.HandleException(nil);
  end;
end;

{------------------------------------------------------------------------------
  Function: TQtListWidget.SignalItemDoubleClicked
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtListWidget.SignalItemDoubleClicked(item: QListWidgetItemH); cdecl;
var
  Msg: TLMessage;
begin
  FillChar(Msg, SizeOf(Msg), #0);
  Msg.Msg := LM_LBUTTONDBLCLK;
  try
    LCLObject.WindowProc(TLMessage(Msg));
  except
    Application.HandleException(nil);
  end;
end;

{------------------------------------------------------------------------------
  Function: TQtListWidget.SignalItemClicked
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtListWidget.SignalItemClicked(item: QListWidgetItemH); cdecl;
var
  Msg: TLMessage;
begin
  FillChar(Msg, SizeOf(Msg), #0);
  Msg.Msg := LM_CLICKED;
  try
    LCLObject.WindowProc(TLMessage(Msg));
  except
    Application.HandleException(nil);
  end;
end;

{------------------------------------------------------------------------------
  Function: TQtListWidget.currentRow
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtListWidget.currentRow: Integer;
begin
  Result := QListWidget_currentRow(QListWidgetH(Widget));
end;

{------------------------------------------------------------------------------
  Function: TQtListWidget.setCurrentRow
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtListWidget.setCurrentRow(row: Integer);
begin
  QListWidget_setCurrentRow(QListWidgetH(Widget), row);
end;


  { TQtHeaderView }
  
{------------------------------------------------------------------------------
  Function: TQtHeaderView.CreateWidget
  Params:  None
  Returns: Widget (QHeaderViewH)
 ------------------------------------------------------------------------------}
function TQtHeaderView.CreateWidget(const AParams: TCreateParams):QWidgetH;
var
  Parent: QWidgetH;
begin
  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtHeaderView.Create');
  {$endif}
  Parent := TQtWidget(LCLObject.Parent.Handle).GetContainerWidget;
  Result := QHeaderView_create(QtHorizontal, Parent);
end;

{------------------------------------------------------------------------------
  Function: TQtHeaderView.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtHeaderView.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtHeaderView.Destroy');
  {$endif}

  QHeaderView_destroy(QHeaderViewH(Widget));
  Widget:=nil;

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtHeaderView.SignalSectionClicked
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtHeaderView.SignalSectionClicked(logicalIndex: Integer) cdecl;
var
  Msg: TLMNotify;
  NMLV: TNMListView;
begin

  FillChar(Msg, SizeOf(Msg), #0);
  FillChar(NMLV, SizeOf(NMLV), #0);
  
  Msg.Msg := CN_NOTIFY;
  NMLV.hdr.hwndfrom := LCLObject.Handle;
  NMLV.hdr.code := LVN_COLUMNCLICK;
  NMLV.iItem := -1;
  NMLV.iSubItem := logicalIndex;
  
  Msg.NMHdr := @NMLV.hdr;
  
  DeliverMessage(Msg);
  
end;

  { TQtTreeView }

{------------------------------------------------------------------------------
  Function: TQtTreeView.CreateWidget
  Params:  None
  Returns: Widget (QTreeViewH)
 ------------------------------------------------------------------------------}
function TQtTreeView.CreateWidget(const AParams: TCreateParams):QWidgetH;
var
  Parent: QWidgetH;
begin
  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtTreeView.Create');
  {$endif}
  Parent := TQtWidget(LCLObject.Parent.Handle).GetContainerWidget;
  Result := QTreeView_create(Parent);
end;

{------------------------------------------------------------------------------
  Function: TQtTreeView.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtTreeView.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtTreeView.Destroy');
  {$endif}

  QTreeView_destroy(QTreeViewH(Widget));
  Widget:=nil;

  inherited Destroy;
end;


  { TQtTreeWidget }

{------------------------------------------------------------------------------
  Function: TQtTreeWidget.CreateWidget
  Params:  None
  Returns: Widget (QTreeWidgetH)
 ------------------------------------------------------------------------------}
function TQtTreeWidget.CreateWidget(const AParams: TCreateParams):QWidgetH;
var
  Parent: QWidgetH;
  Hook: QHeaderView_hookH;
  Method: TMethod;
begin
  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtTreeWidget.Create');
  {$endif}
  Parent := TQtWidget(LCLObject.Parent.Handle).GetContainerWidget;
  Result := QTreeWidget_create(Parent);
  
  Header := TQtHeaderView.Create(LCLObject, AParams);
  
  Hook := QHeaderView_hook_create(Header.Widget);
  
  TEventFilterMethod(Method) := Header.EventFilter;

  QObject_hook_hook_events(Hook, Method);
  
  QHeaderView_sectionClicked_Event(Method) := Header.SignalSectionClicked;
  QHeaderView_hook_hook_sectionClicked(QHeaderView_hook_create(Header.Widget), Method);

  
  QTreeView_setHeader(QTreeViewH(Result), QHeaderViewH(Header.Widget));
end;

{------------------------------------------------------------------------------
  Function: TQtTreeWidget.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtTreeWidget.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtTreeWidget.Destroy');
  {$endif}

  if Assigned(Header) then
  Header.Free;

  QTreeWidget_destroy(QTreeWidgetH(Widget));
  Widget:=nil;

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtTreeWidget.CurrentRow
  Params:  None
  Returns: Integer
 ------------------------------------------------------------------------------}
function TQtTreeWidget.currentRow: Integer;
var
  TWI: QTreeWidgetItemH;
begin
  TWI := QTreeWidget_currentItem(QTreeWidgetH(Widget));
  Result := QTreeWidget_indexOfTopLevelItem(QTreeWidgetH(Widget), TWI);
end;

{------------------------------------------------------------------------------
  Function: TQtTreeWidget.setCurrentRow
  Params:  Integer
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtTreeWidget.setCurrentRow(row: Integer);
var
  TWI: QTreeWidgetItemH;
begin
  TWI := QTreeWidget_topLevelItem(QTreeWidgetH(Widget), Row);
  QTreeWidget_setCurrentItem(QTreeWidgetH(Widget), TWI);
end;

{------------------------------------------------------------------------------
  Function: TQtTreeWidget.SignalItemPressed
  Params:  Integer
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtTreeWidget.SignalItemPressed(item: QTreeWidgetItemH; column: Integer) cdecl;
var
  Msg: TLMNotify;
  NMLV: TNMListView;
begin
  FillChar(Msg, SizeOf(Msg), #0);
  FillChar(NMLV, SizeOf(NMLV), #0);

  Msg.Msg := LM_PRESSED;
  
  NMLV.hdr.hwndfrom := LCLObject.Handle;
  NMLV.hdr.code := LVN_ITEMCHANGED;

  NMLV.iItem := QTreeWidget_indexOfTopLevelItem(QTreeWidgetH(Widget), Item);

  NMLV.iSubItem := Column;
  NMLV.uNewState := NM_KEYDOWN;
  NMLV.uChanged := LVIS_SELECTED;

  Msg.NMHdr := @NMLV.hdr;

  DeliverMessage(Msg);
end;

{------------------------------------------------------------------------------
  Function: TQtTreeWidget.SignalItemClicked
  Params:  Integer
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtTreeWidget.SignalItemClicked(item: QTreeWidgetItemH; column: Integer) cdecl;
var
  Msg: TLMNotify;
  NMLV: TNMListView;
  R: TRect;
  Pt: TPoint;
begin

  FillChar(Msg, SizeOf(Msg), #0);
  FillChar(NMLV, SizeOf(NMLV), #0);

  Msg.Msg := LM_CLICKED;

  NMLV.hdr.hwndfrom := LCLObject.Handle;
  NMLV.hdr.code := NM_CLICK;

  NMLV.iItem := QTreeWidget_indexOfTopLevelItem(QTreeWidgetH(Widget), Item);

  NMLV.iSubItem := Column;
  NMLV.uNewState := NM_CLICK;
  NMLV.uChanged := LVIS_SELECTED;
  QTreeWidget_visualItemRect(QTreeWidgetH(Widget), @R, Item);
  
  pt.X := R.Left;
  pt.Y := R.Top;

  NMLV.ptAction := pt;

  Msg.NMHdr := @NMLV.hdr;
  
  DeliverMessage(Msg);
  
end;

{------------------------------------------------------------------------------
  Function: TQtTreeWidget.SignalItemDoubleClicked
  Params:  Integer
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtTreeWidget.SignalItemDoubleClicked(item: QTreeWidgetItemH; column: Integer) cdecl;
var
  Msg: TLMNotify;
  NMLV: TNMListView;
begin
  FillChar(Msg, SizeOf(Msg), #0);
  FillChar(NMLV, SizeOf(NMLV), #0);
  
  Msg.Msg := LM_LBUTTONDBLCLK;

  NMLV.hdr.hwndfrom := LCLObject.Handle;
  NMLV.hdr.code := NM_DBLCLK;

  NMLV.iItem := QTreeWidget_indexOfTopLevelItem(QTreeWidgetH(Widget), Item);

  NMLV.iSubItem := Column;
  NMLV.uNewState := NM_DBLCLK;
  NMLV.uChanged := LVIS_SELECTED;
  // LVIF_STATE;
  
  Msg.NMHdr := @NMLV.hdr;
  
  DeliverMessage( Msg);
end;

{------------------------------------------------------------------------------
  Function: TQtTreeWidget.SignalItemActivated
  Params:  Integer
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtTreeWidget.SignalItemActivated(item: QTreeWidgetItemH; column: Integer) cdecl;
var
  Msg: TLMNotify;
  NMLV: TNMListView;
begin
  FillChar(Msg, SizeOf(Msg), #0);
  FillChar(NMLV, SizeOf(NMLV), #0);

  Msg.Msg := CN_NOTIFY;

  NMLV.hdr.hwndfrom := LCLObject.Handle;
  NMLV.hdr.code := LVN_ITEMCHANGED;

  NMLV.iItem := QTreeWidget_indexOfTopLevelItem(QTreeWidgetH(Widget), Item);

  NMLV.iSubItem := Column;
  NMLV.uNewState := LVIS_FOCUSED;
  NMLV.uChanged := LVIF_STATE;

  Msg.NMHdr := @NMLV.hdr;

  DeliverMessage( Msg);
end;

{------------------------------------------------------------------------------
  Function: TQtTreeWidget.SignalItemEntered
  Params:  Integer
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtTreeWidget.SignalItemEntered(item: QTreeWidgetItemH; column: Integer) cdecl;
var
  Msg: TLMessage;
begin
  FillChar(Msg, SizeOf(Msg), #0);
  Msg.Msg := LM_ENTER;
  try
    LCLObject.WindowProc(TLMessage(Msg));
  except
    Application.HandleException(nil);
  end;
end;

{------------------------------------------------------------------------------
  Function: TQtTreeWidget.SignalItemChanged
  Params:  Integer
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtTreeWidget.SignalItemChanged(item: QTreeWidgetItemH; column: Integer) cdecl;
var
  Msg: TLMessage;
begin
  FillChar(Msg, SizeOf(Msg), #0);
  Msg.Msg := LM_CHANGED;
  try
    LCLObject.WindowProc(TLMessage(Msg));
  except
    Application.HandleException(nil);
  end;
end;

{------------------------------------------------------------------------------
  Function: TQtTreeWidget.SignalItemExpanded
  Params:  Integer
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtTreeWidget.SignalitemExpanded(item: QTreeWidgetItemH) cdecl;
begin
{fixme}
end;

{------------------------------------------------------------------------------
  Function: TQtTreeWidget.SignalItemCollapsed
  Params:  Integer
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtTreeWidget.SignalItemCollapsed(item: QTreeWidgetItemH) cdecl;
begin
{fixme}
end;

{------------------------------------------------------------------------------
  Function: TQtTreeWidget.SignalCurrentItemChanged
  Params:  Integer
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtTreeWidget.SignalCurrentItemChanged(current: QTreeWidgetItemH; previous: QTreeWidgetItemH) cdecl;
var
  Msg: TLMNotify;
  NMLV: TNMListView;
  AParent: QTreeWidgetItemH;
  AIndex: Integer;
  ASubIndex: Integer;
begin

  FillChar(Msg, SizeOf(Msg), #0);
  FillChar(NMLV, SizeOf(NMLV), #0);

  Msg.Msg := CN_NOTIFY;

  NMLV.hdr.hwndfrom := LCLObject.Handle;
  NMLV.hdr.code := LVN_ITEMCHANGING;
  
  NMLV.iItem := QTreeWidget_indexOfTopLevelItem(QTreeWidgetH(Widget), Current);
  
  AParent := QTreeWidgetItem_parent(Current);
  
  if AParent <> NiL then
    ASubIndex := QTreeWidgetItem_indexOfChild(AParent, Current)
  else
    ASubIndex := 0;
    
  NMLV.iSubItem := ASubIndex;
  NMLV.uNewState := LVIS_SELECTED;
  NMLV.uChanged := LVIF_STATE;

  Msg.NMHdr := @NMLV.hdr;
  
  if Current <> Previous then
  DeliverMessage(Msg);
  
end;

{------------------------------------------------------------------------------
  Function: TQtTreeWidget.SignalItemSelectionChanged
  Params:  Integer
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtTreeWidget.SignalItemSelectionChanged; cdecl;
var
  Msg: TLMNotify;
  NMLV: TNMListView;
  Item: QTreeWidgetItemH;
  AParent: QTreeWidgetItemH;
  AIndex: Integer;
  ASubIndex: Integer;
begin
  FillChar(Msg, SizeOf(Msg), #0);
  FillChar(NMLV, SizeOf(NMLV), #0);

  Msg.Msg := CN_NOTIFY;


  NMLV.hdr.hwndfrom := LCLObject.Handle;
  NMLV.hdr.code := LVN_ITEMCHANGED;


  Item := QTreeWidget_currentItem(QTreeWidgetH(Widget));
  AIndex := QTreeWidget_indexOfTopLevelItem(QTreeWidgetH(Widget), Item);
  AParent := QTreeWidgetItem_parent(Item);
  if AParent <> NiL then
    ASubIndex := QTreeWidgetItem_indexOfChild(AParent, Item)
  else
    ASubIndex := 0;

  NMLV.iItem := AIndex;
  NMLV.iSubItem := ASubIndex;
  NMLV.uNewState := LVIS_SELECTED;
  NMLV.uChanged := LVIF_STATE;


  Msg.NMHdr := @NMLV.hdr;

  DeliverMessage(Msg);
  
end;


{ TQtMenu }

constructor TQtMenu.Create(const AParent: QWidgetH);
begin
  Widget := QMenu_Create(AParent);
  FIcon := nil;
end;

constructor TQtMenu.Create(const AHandle: QMenuH);
begin
  Widget := AHandle;
  FIcon := nil;
end;

destructor TQtMenu.Destroy;
begin
  if FIcon <> nil then
    QIcon_destroy(FIcon);
  inherited Destroy;
end;

procedure TQtMenu.SlotDestroy; cdecl;
begin
  Widget := nil;
end;

procedure TQtMenu.PopUp(pos: PQtPoint; at: QActionH);
begin
  QMenu_Popup(QMenuH(Widget), pos, at);
end;

function TQtMenu.actionHandle: QActionH;
begin
  Result := QMenu_menuAction(QMenuH(Widget));
end;

function TQtMenu.addMenu(title: PWideString): TQtMenu;
begin
  Result := TQtMenu.Create(QMenu_addMenu(QMenuH(Widget), title));
end;

function TQtMenu.addSeparator: TQtMenu;
begin
  Result := TQtMenu.Create(QMenu_addMenu(QMenuH(Widget), nil));
  Result.setSeparator(True);
end;

procedure TQtMenu.setChecked(p1: Boolean);
begin
  if p1 then setCheckable(True)
  else setCheckable(False);

  QAction_setChecked(ActionHandle, p1);
end;

procedure TQtMenu.setCheckable(p1: Boolean);
begin
  QAction_setCheckable(ActionHandle, p1);
end;

procedure TQtMenu.setHasSubmenu(AValue: Boolean);
begin
  if AValue then
    QAction_setMenu(ActionHandle, QMenuH(Widget))
  else
    QAction_setMenu(ActionHandle, nil);
end;

procedure TQtMenu.setIcon(AIcon: QIconH);
begin
  QMenu_setIcon(QMenuH(Widget), AIcon)
end;

procedure TQtMenu.setImage(AImage: TQtImage);
begin
  if FIcon <> nil then
  begin
    QIcon_destroy(FIcon);
    FIcon := nil;
  end;

  if AImage <> nil then
    FIcon := AImage.AsIcon()
  else
    FIcon := QIcon_create();
    
  setIcon(FIcon);
end;

procedure TQtMenu.setSeparator(AValue: Boolean);
begin
  QAction_setSeparator(ActionHandle, AValue);
end;

{------------------------------------------------------------------------------
  Method: TQtMenu.SlotTriggered

  Callback for menu item click
 ------------------------------------------------------------------------------}
procedure TQtMenu.SlotTriggered(checked: Boolean); cdecl;
begin
  if Assigned(MenuItem) and Assigned(MenuItem.OnClick) then
   MenuItem.OnClick(Self.MenuItem);
end;

function TQtMenu.EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl;
begin
  Result := False;

  case QEvent_type(Event) of
    QEventDestroy: SlotDestroy;
  end;
end;

{ TQtMenuBar }

constructor TQtMenuBar.Create(const AParent: QWidgetH);
begin
  Widget := QMenuBar_create(AParent);
  FVisible := False;
  setVisible(FVisible);
end;

destructor TQtMenuBar.Destroy;
begin
  inherited Destroy;
end;

function TQtMenuBar.addMenu(title: PWideString): TQtMenu;
begin
  if not FVisible then
  begin
    FVisible := True;
    setVisible(FVisible);
  end;
  Result := TQtMenu.Create(QMenuBar_addMenu(QMenuBarH(Widget), title));
end;

function TQtMenuBar.addSeparator: TQtMenu;
begin
  if not FVisible then
  begin
    FVisible := True;
    setVisible(FVisible);
  end;
  Result := TQtMenu.Create(QMenuBar_addMenu(QMenuBarH(Widget), nil));
  Result.setSeparator(True);
end;

{ TQtProgressBar }

function TQtProgressBar.CreateWidget(const AParams: TCreateParams): QWidgetH;
var
  Parent: QWidgetH;
  Text: WideString;
begin
  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQProgressBar.Create');
  {$endif}
  Parent := TQtWidget(LCLObject.Parent.Handle).GetContainerWidget;
  Result := QProgressBar_create(Parent);
end;

{------------------------------------------------------------------------------
  Function: TQtProgressBar.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtProgressBar.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtProgressBar.Destroy');
  {$endif}

  QProgressBar_destroy(QProgressBarH(Widget));
  Widget:=nil;

  inherited Destroy;
end;

procedure TQtProgressBar.setRange(minimum: Integer; maximum: Integer);
begin
  QProgressBar_setRange(QProgressBarH(Widget), minimum, maximum);
end;

procedure TQtProgressBar.setTextVisible(visible: Boolean);
begin
  QProgressBar_setTextVisible(QProgressBarH(Widget), visible);
end;

procedure TQtProgressBar.setAlignment(alignment: QtAlignment);
begin
  QProgressBar_setAlignment(QProgressBarH(Widget), alignment);
end;

procedure TQtProgressBar.setTextDirection(textDirection: QProgressBarDirection);
begin
  QProgressBar_setTextDirection(QProgressBarH(Widget), textDirection);
end;

procedure TQtProgressBar.setValue(value: Integer);
begin
  QProgressBar_setValue(QProgressBarH(Widget), value);
end;

procedure TQtProgressBar.setOrientation(p1: QtOrientation);
begin
  QProgressBar_setOrientation(QProgressBarH(Widget), p1);
end;

procedure TQtProgressBar.setInvertedAppearance(invert: Boolean);
begin
  QProgressBar_setInvertedAppearance(QProgressBarH(Widget), invert);
end;

procedure TQtProgressBar.SignalValueChanged(Value: Integer);
var
  Msg: TLMessage;
begin
  FillChar(Msg, SizeOf(Msg), #0);
  Msg.Msg := LM_CHANGED;
  DeliverMessage(Msg);
end;

{ TQtStatusBar }

function TQtStatusBar.CreateWidget(const AParams: TCreateParams): QWidgetH;
var
  Parent: QWidgetH;
begin
  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtStatusBar.Create');
  {$endif}
  
  SetLength(APanels, 0);
  Parent := TQtWidget(LCLObject.Parent.Handle).GetContainerWidget;
  Result := QStatusBar_create(Parent);
  
  {TODO: this should be made in initializeWND?
  if (LCLObject as TStatusBar).SimplePanel then
  begin;
    Text := UTF8Decode((LCLObject as TStatusBar).SimpleText);
    showMessage(@Text);
  end;
  }
end;

{------------------------------------------------------------------------------
  Function: TQtStatusBar.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtStatusBar.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtStatusBar.Destroy');
  {$endif}

  QStatusBar_destroy(QStatusBarH(Widget));
  Widget:=nil;

  inherited Destroy;
end;

procedure TQtStatusBar.showMessage(text: PWideString; timeout: Integer);
begin
  QStatusBar_showMessage(QStatusBarH(Widget), text, timeout);
end;

{ TQtDialog }

constructor TQtDialog.Create(parent: QWidgetH; f: QtWindowFlags);
begin
  Widget := QDialog_create(parent, f);
end;

destructor TQtDialog.Destroy;
begin
  QDialog_destroy(QDialogH(Widget));
  Widget:=nil;
  
  inherited Destroy;
end;

function TQtDialog.exec: Integer;
begin
  Result := QDialog_exec(QDialogH(Widget));
end;

{ TQtAbstractScrollArea }

{------------------------------------------------------------------------------
  Function: TQtAbstractScrollArea.CreateWidget
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtAbstractScrollArea.CreateWidget(const AParams: TCreateParams):QWidgetH;
var
  Parent: QWidgetH;
begin
  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtAbstractScrollArea.Create');
  {$endif}
  FViewPortWidget := NiL;
  Parent := TQtWidget(LCLObject.Parent.Handle).GetContainerWidget;
  Result := QAbstractScrollArea_create(Parent);
end;

{------------------------------------------------------------------------------
  Function: TQtAbstractScrollArea.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtAbstractScrollArea.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQAbstractScrollArea.Destroy');
  {$endif}
  if Assigned(FViewPortWidget) then
  FViewPortWidget.Free;
  QAbstractScrollArea_destroy(QAbstractScrollAreaH(Widget));
  Widget:=nil;

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtAbstractScrollArea.cornerWidget
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtAbstractScrollArea.cornerWidget: TQtWidget;
begin
  {$ifdef VerboseQt}
    WriteLn('TQAbstractScrollArea.cornerWidget');
  {$endif}
  Result := FCornerWidget;
end;

{------------------------------------------------------------------------------
  Function: TQtAbstractScrollArea.setColor
  Params:  TQtWidget
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtAbstractScrollArea.setColor(const Value: PQColor);
var
  Palette: QPaletteH;
begin
  Palette := QPalette_create(QWidget_palette(Widget));
  try
    QPalette_setColor(Palette, QPaletteBase, Value);
    QWidget_setPalette(Widget, Palette);
  finally
    QPalette_destroy(Palette);
  end;
end;

{------------------------------------------------------------------------------
  Function: TQtAbstractScrollArea.setCornerWidget
  Params:  TQtWidget
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtAbstractScrollArea.setCornerWidget(AWidget: TQtWidget);
begin
  {$ifdef VerboseQt}
    WriteLn('TQAbstractScrollArea.setCornerWidget');
  {$endif}
  FCornerWidget := AWidget;
  if Assigned(FCornerWidget) then
  QAbstractScrollArea_setCornerWidget(QAbstractScrollAreaH(Widget), FCornerWidget.Widget)
  else
  QAbstractScrollArea_setCornerWidget(QAbstractScrollAreaH(Widget), NiL);
end;

{------------------------------------------------------------------------------
  Function: TQtAbstractScrollArea.setTextColor
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtAbstractScrollArea.setTextColor(const Value: PQColor);
var
  Palette: QPaletteH;
begin
  Palette := QPalette_create(QWidget_palette(Widget));
  try
    QPalette_setColor(Palette, QPaletteText, Value);
    QWidget_setPalette(Widget, Palette);
  finally
    QPalette_destroy(Palette);
  end;
end;

{------------------------------------------------------------------------------
  Function: TQtAbstractScrollArea.setHorizontalScrollbar
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtAbstractScrollArea.setHorizontalScrollBar(AScrollBar: TQtScrollBar);
begin
  {$ifdef VerboseQt}
    WriteLn('TQAbstractScrollArea.setHorizontalScrollBar');
  {$endif}
  FHScrollbar := AScrollBar;
  if Assigned(FHScrollBar) then
  QAbstractScrollArea_setHorizontalScrollBar(QAbstractScrollAreaH(Widget), QScrollBarH(FHScrollBar.Widget));
end;

{------------------------------------------------------------------------------
  Function: TQtAbstractScrollArea.setVerticalScrollbar
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtAbstractScrollArea.setVerticalScrollBar(AScrollBar: TQtScrollBar);
begin
  {$ifdef VerboseQt}
    WriteLn('TQAbstractScrollArea.setVerticalScrollBar');
  {$endif}
  FVScrollBar := AScrollBar;
  if Assigned(FVScrollBar) then
  QAbstractScrollArea_setVerticalScrollBar(QAbstractScrollAreaH(Widget), QScrollBarH(FVScrollBar.Widget));
end;

{------------------------------------------------------------------------------
  Function: TQtAbstractScrollArea.horizontalScrollbar
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtAbstractScrollArea.horizontalScrollBar: TQtScrollBar;
begin
  {$ifdef VerboseQt}
    WriteLn('TQAbstractScrollArea.horizontalScrollBar');
  {$endif}
  Result := FHScrollBar;
end;

{------------------------------------------------------------------------------
  Function: TQtAbstractScrollArea.verticalScrollbar
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtAbstractScrollArea.verticalScrollBar: TQtScrollBar;
begin
  {$ifdef VerboseQt}
    WriteLn('TQAbstractScrollArea.verticalScrollBar');
  {$endif}
  Result := FVScrollBar;
end;

{------------------------------------------------------------------------------
  Function: TQtAbstractScrollArea.viewport
  Params:  None
  Returns: viewport widget of QAbstractScrollArea
 ------------------------------------------------------------------------------}
function TQtAbstractScrollArea.viewport: TQtWidget;
begin
  viewportNeeded;
  Result := FViewPortWidget;
end;

{------------------------------------------------------------------------------
  Function: TQtAbstractScrollArea.viewportNeeded
  Params:  None
  Returns: Nothing
           Creates viewport widget for QAbstractScrollArea
 ------------------------------------------------------------------------------}
procedure TQtAbstractScrollArea.viewportNeeded;
var
  AParams: TCreateParams;
  Hook: QWidget_hookH;
  Method: TMethod;
begin
  if FViewPortWidget <> NiL
  then
    exit;

  FViewPortWidget := TQtWidget.Create(LCLObject, AParams);
  
  Hook := QWidget_hook_create(FViewPortWidget.Widget);
  TEventFilterMethod(Method) := FViewPortWidget.EventFilter;
  QObject_hook_hook_events(Hook, Method);

  QAbstractScrollArea_setViewport(QAbstractScrollAreaH(Widget), FViewPortWidget.Widget);
end;

{------------------------------------------------------------------------------
  Function: TQtAbstractScrollArea.setScrollStyle
  Params:  None
  Returns: Nothing
           Setting scrollbar''s policy (LCL TScrollStyle)
 -----------------------------------------------------------------------------}
procedure TQtAbstractScrollArea.setScrollStyle(AScrollStyle: TScrollStyle);
begin
  {$ifdef VerboseQt}
    WriteLn('TQAbstractScrollArea.setScrollStyle');
  {$endif}
  case AScrollStyle of
    ssNone:
    begin
      QAbstractScrollArea_setVerticalScrollBarPolicy(QAbstractScrollAreaH(Widget), QtScrollBarAlwaysOff);
      QAbstractScrollArea_setHorizontalScrollBarPolicy(QAbstractScrollAreaH(Widget), QtScrollBarAlwaysOff);
    end;
    ssHorizontal:
    begin
      QAbstractScrollArea_setHorizontalScrollBarPolicy(QAbstractScrollAreaH(Widget), QtScrollBarAlwaysOn);
    end;
    ssVertical:
    begin
     QAbstractScrollArea_setVerticalScrollBarPolicy(QAbstractScrollAreaH(Widget), QtScrollBarAlwaysOn);
    end;
    ssBoth:
    begin
      QAbstractScrollArea_setVerticalScrollBarPolicy(QAbstractScrollAreaH(Widget), QtScrollBarAlwaysOn);
      QAbstractScrollArea_setHorizontalScrollBarPolicy(QAbstractScrollAreaH(Widget), QtScrollBarAlwaysOn);
    end;
    ssAutoHorizontal:
    begin
      QAbstractScrollArea_setHorizontalScrollBarPolicy(QAbstractScrollAreaH(Widget), QtScrollBarAsNeeded);
    end;
    ssAutoVertical:
    begin
      QAbstractScrollArea_setVerticalScrollBarPolicy(QAbstractScrollAreaH(Widget), QtScrollBarAsNeeded);
    end;
    ssAutoBoth:
    begin
      QAbstractScrollArea_setHorizontalScrollBarPolicy(QAbstractScrollAreaH(Widget), QtScrollBarAsNeeded);
      QAbstractScrollArea_setVerticalScrollBarPolicy(QAbstractScrollAreaH(Widget), QtScrollBarAsNeeded);
    end;
  end;
end;

  { TQtCalendar }

{------------------------------------------------------------------------------
  Function: TQtCalendar.CreateWidget
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtCalendar.CreateWidget(const AParams: TCreateParams):QWidgetH;
var
  Parent: QWidgetH;
begin
  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtCalendar.Create');
  {$endif}
  Parent := TQtWidget(LCLObject.Parent.Handle).GetContainerWidget;
  Result := QCalendarWidget_create(Parent);
end;

{------------------------------------------------------------------------------
  Function: TQtCalendar.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtCalendar.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtCalendar.Destroy');
  {$endif}
  QCalendarWidget_destroy(QCalendarWidgetH(Widget));
  Widget:=nil;

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtCalendar.SignalActivated
  Params:  None
  Returns: Nothing
           Sends signal when RETURN pressed on selected date.
 ------------------------------------------------------------------------------}
procedure TQtCalendar.SignalActivated(ADate: QDateH); cdecl;
var
  Msg: TLMessage;
  y,m,d: Integer;
begin
  {this one triggers if we press RETURN on selected date
   shell we send KeyDown here ?!?}
  FillChar(Msg, SizeOf(Msg), #0);
  Msg.Msg := LM_DAYCHANGED;
  y := QDate_year(ADate);
  m := QDate_month(ADate);
  d := QDate_day(ADate);
  if (y <> aYear) or (m <> aMonth)
  or (d <> aDay) then
  DeliverMessage(Msg);
end;

{------------------------------------------------------------------------------
  Function: TQtCalendar.SignalClicked
  Params:  None
  Returns: Nothing
           Sends msg LM_DAYCHANGED when OldDate<>NewDate
 ------------------------------------------------------------------------------}
procedure TQtCalendar.SignalClicked(ADate: QDateH); cdecl;
var
  Msg: TLMessage;
  y,m,d: Integer;
begin
//  writeln('TQtCalendar.signalClicked');
  FillChar(Msg, SizeOf(Msg), #0);
  Msg.Msg := LM_DAYCHANGED;
  y := QDate_year(ADate);
  m := QDate_month(ADate);
  d := QDate_day(ADate);
  if (y <> aYear) or (m <> aMonth)
  or (d <> aDay) then
  DeliverMessage(Msg);
end;

{------------------------------------------------------------------------------
  Function: TQtCalendar.SignalSelectionChanged
  Params:  None
  Returns: Nothing

  Notes: no event for date changed by keyboard ?!?
   always triggers even if selection isn't changed ...
   this is not Qt4 bug ... tested with pure Qt C++ app
 ------------------------------------------------------------------------------}
procedure TQtCalendar.SignalSelectionChanged; cdecl;
var
  Msg: TLMessage;
begin
//  writeln('TQtCalendar.SignalSelectionChanged');

  FillChar(Msg, SizeOf(Msg), #0);
  Msg.Msg := LM_DAYCHANGED;
  DeliverMessage(Msg);
end;

{------------------------------------------------------------------------------
  Function: TQtCalendar.SignalCurrentPageChanged
  Params:  None
  Returns: Nothing

  Notes: fixme what's wrong with those values ?!?
   with pure Qt C++ app this works ok, but via bindings get
   impossible year & month values ...
 ------------------------------------------------------------------------------}
procedure TQtCalendar.SignalCurrentPageChanged(p1, p2: Integer); cdecl;
var
  Msg: TLMessage;
begin
  // writeln('TQtCalendar.SignalCurrentPageChanged p1=',p1,' p2=',p2);

  FillChar(Msg, SizeOf(Msg), #0);
  if AYear<>p1 then
  begin
    Msg.Msg := LM_YEARCHANGED;
    DeliverMessage(Msg);
  end;

  if AMonth<>p2 then
  begin
    Msg.Msg := LM_MONTHCHANGED;
    DeliverMessage(Msg);
  end;
end;

end.



