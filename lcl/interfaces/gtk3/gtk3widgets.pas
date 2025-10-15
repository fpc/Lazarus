{
 *****************************************************************************
 *                               gtk3widgets.pas                             *
 *                               -------------                               *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

unit Gtk3Widgets;

{$mode objfpc}{$H+}
{$i gtk3defines.inc}

interface

uses
  Classes, SysUtils, types, math,
  // LCL
  Controls, StdCtrls, ExtCtrls, Buttons, ComCtrls, Graphics, Dialogs, Forms, Menus, ExtDlgs,
  Spin, CheckLst, PairSplitter, LCLType, LMessages, LCLMessageGlue, LCLIntf,
  // LazUtils
  GraphType,
  // GTK3
  LazGtk3, LazGdk3, LazGObject2, LazGLib2, LazCairo1, LazPango1, LazGdkPixbuf2,
  gtk3objects, gtk3procs, gtk3private, Gtk3CellRenderer;

type
  TByteSet = set of byte;

  // records
  TPaintData = record
    PaintWidget: PGtkWidget;
    ClipRect: PRect;
    ClipRegion: Pcairo_region_t;
  end;

  TDefaultRGBA = record
    R: Double;
    G: Double;
    B: Double;
    Alpha: Double;
  end;

  TGtk3WidgetType = (wtWidget, wtStaticText, wtProgressBar, wtLayout,
    wtContainer, wtMenuBar, wtMenu, wtMenuItem, wtEntry, wtSpinEdit,
    wtNotebook, wtTabControl, wtComboBox, wtPanel,
    wtGroupBox, wtCalendar, wtTrackBar, wtScrollBar,
    wtScrollingWin, wtListBox, wtListView, wtCheckListBox, wtMemo, wtTreeModel,
    wtCustomControl, wtToolbar, wtScrollingWinControl,
    wtWindow, wtDialog, wtHintWindow, wtGLArea);
  TGtk3WidgetTypes = set of TGtk3WidgetType;

  TGtk3GroupBoxType = (gbtGroupBox, gbtCheckGroup, gbtRadioGroup);

  { TGtk3Widget }

  TGtk3Widget = class(TGtk3Object, IUnknown)
  private
    FCairoContext: Pcairo_t;
    FShape: PGdkPixbuf;
    FContext: HDC;
    FPaintData: TPaintData;
    FDrawSignal: GULong; // needed by designer
    FFont: PPangoFontDescription;
    class function MoveTabFocus(aWidget: PGtkWidget;
      aDirection: TGtkDirectionType; aData: gPointer): gBoolean; cdecl; static;
    class function WidgetEvent(widget: PGtkWidget; event: PGdkEvent; data: GPointer): gboolean; cdecl; static; {main event filter of widget}
  strict private
    FCentralWidgetRGBA: array [0{GTK_STATE_NORMAL}..4{GTK_STATE_INSENSITIVE}] of TDefaultRGBA;
    FEnterLeaveTime: Cardinal;
    FFocusableByMouse: Boolean; {shell we call SetFocus on mouse down. Default = False}
    FHasCaret: boolean;
    FOwner: PGtkWidget;
    FProps: TStringList;
    FWidgetMapped: boolean;
    FWidgetRGBA: array [0{GTK_STATE_NORMAL}..4{GTK_STATE_INSENSITIVE}] of TDefaultRGBA;
    function CanSendLCLMessage: Boolean;
    function GetCairoContext: Pcairo_t;
    function GetEnabled: Boolean;
    function GetFont: PPangoFontDescription;
    function GetStyleContext: PGtkStyleContext;
    function GetVisible: Boolean;
    procedure SetEnabled(AValue: Boolean);
    procedure SetFont(AValue: PPangoFontDescription);
    procedure SetShape(AValue: PGdkPixbuf);
    procedure SetStyleContext({%H-}AValue: PGtkStyleContext);
    class procedure DestroyWidgetEvent({%H-}w: PGtkWidget;{%H-}data:gpointer); cdecl; static;
    class function DrawWidget(AWidget: PGtkWidget; AContext: Pcairo_t; Data: gpointer): gboolean; cdecl; static;
    class procedure MapWidget(AWidget: PGtkWidget; Data: gPointer); cdecl; static; {GtkWindow never sends this signal !}
    class function MouseEnterNotify(aWidget: PGtkWidget; aEvent: PGdkEventCrossing; aData: gpointer): gboolean; cdecl; static;
    class function MouseLeaveNotify(aWidget: PGtkWidget; aEvent: PGdkEventCrossing; aData: gpointer): gboolean; cdecl; static;
    class function ScrollEvent(AWidget: PGtkWidget; AEvent: PGdkEvent; AData: GPointer): GBoolean; cdecl; static;
    class procedure SizeAllocate(AWidget: PGtkWidget; AGdkRect: PGdkRectangle; Data: gpointer); cdecl; static;
    class procedure WidgetHide({%H-}AWidget:PGtkWidget; AData:gpointer); cdecl; static;
    class procedure WidgetShow({%H-}AWidget:PGtkWidget; AData:gpointer); cdecl; static;

    class procedure DragDataReceived(aWidget: PGtkWidget;
      aContext: PGdkDragContext; x: gint; y: gint;
      selection_data: PGtkSelectionData; info: guint; time: guint;
      aData: gPointer); cdecl; static;

  protected
    FCentralWidget: PGtkWidget;
    FHasPaint: Boolean;
    FKeysToEat: TByteSet;
    FParams: TCreateParams;
    fText: string;
    FOwnWidget: Boolean;
    FWidget: PGtkWidget;
    FWidgetType: TGtk3WidgetTypes;
    // IUnknown implementation
    function QueryInterface(constref iid: TGuid; out obj): LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function _AddRef: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function _Release: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function IsDesigning: boolean; virtual;
    function EatArrowKeys(const AKey: Word): Boolean; virtual;
    function getText: String; virtual;
    procedure setText(const AValue: String); virtual;
    function GetContext: HDC; virtual;
    function CreateWidget(const {%H-}Params: TCreateParams):PGtkWidget; virtual;
    procedure DestroyWidget; virtual;
    procedure DoBeforeLCLPaint; virtual;

    function GetColor: TColor; virtual;
    procedure SetColor(AValue: TColor); virtual;
    function GetFontColor: TColor; virtual;
    procedure SetFontColor(AValue: TColor); virtual;
    function GetWidget:PGtkWidget;
    procedure ConnectSizeAllocateSignal(ToWidget: PGtkWidget); virtual;
    procedure SetVisible(AValue: Boolean); virtual;
  public
    LCLObject: TWinControl;
    LCLWidth: integer; {setted up only TWSControl.SetBounds}
    LCLHeight: integer; {setted up only TWSControl.SetBounds}
  public
    constructor Create(const AWinControl: TWinControl; const AParams: TCreateParams); virtual; overload;
    constructor CreateFrom(const AWinControl: TWinControl; AWidget: PGtkWidget); virtual;

    procedure InitializeWidget; virtual;

    procedure DeInitializeWidget;
    procedure RecreateWidget;
    procedure DestroyNotify({%H-}AWidget: PGtkWidget); virtual;
    destructor Destroy; override;

    function CanFocus: Boolean; virtual;
    function GetFocusableByMouse: Boolean;
    function getClientOffset: TPoint; virtual;
    function getWidgetPos: TPoint; virtual;

    procedure OffsetMousePos(const aGlobalX, aGlobalY: double; APoint: PPoint); virtual;

    function ClientToScreen(var P:TPoint):boolean; virtual;
    function ScreenToClient(var P: TPoint): Integer;

    function DeliverMessage(var Msg; const AIsInputEvent: Boolean = False): LRESULT; virtual;
    function GtkEventKey(Sender: PGtkWidget; Event: PGdkEvent; AKeyPress: Boolean): Boolean; virtual; cdecl;
    function GtkEventMouse(Sender: PGtkWidget; Event: PGdkEvent): Boolean; virtual; cdecl;
    function GtkEventMouseMove(Sender: PGtkWidget; Event: PGdkEvent): Boolean; virtual; cdecl;
    function GtkEventPaint(Sender: PGtkWidget; AContext: Pcairo_t): Boolean; virtual; cdecl;
    procedure GtkEventFocus(Sender: PGtkWidget; Event: PGdkEvent); cdecl;
    procedure GtkEventDestroy; cdecl;

    function IsValidHandle: Boolean;
    function IsWidgetOk: Boolean; virtual;
    function IsIconic: Boolean; virtual;

    function getType: TGType;
    function getTypeName: PgChar;

    procedure lowerWidget; virtual;
    procedure raiseWidget; virtual;
    procedure stackUnder(AWidget: PGtkWidget); virtual;

    function GetCapture: TGtk3Widget; virtual;
    function SetCapture: HWND; virtual;

    function getClientRect: TRect; virtual;
    function getClientBounds: TRect; virtual;

    procedure SetBounds(ALeft,ATop,AWidth,AHeight:integer); virtual;
    procedure SetLclFont(const AFont:TFont); virtual;
    procedure SetWindowShape(AShape: PGdkPixBuf; AWindow: PGdkWindow); virtual;

    function GetContainerWidget: PGtkWidget; virtual;
    function GetPosition(out APoint: TPoint): Boolean; virtual;
    procedure Release; override;
    procedure Hide; virtual;
    function getParent: TGtk3Widget;
    function GetWindow: PGdkWindow; virtual;
    procedure Move(ALeft, ATop: Integer);
    procedure Activate; virtual;
    procedure preferredSize(var PreferredWidth, PreferredHeight: integer; {%H-}WithThemeSpace: Boolean); virtual;
    procedure Repaint(const ARect: PRect = nil); virtual;
    procedure SetCursor(ACursor: HCURSOR);
    procedure SetFocus; virtual;
    procedure SetParent(AParent: TGtk3Widget; const ALeft, ATop: Integer); virtual;
    procedure Show; virtual;
    procedure ShowAll; virtual;
    procedure Update(ARect: PRect); virtual;
    property CairoContext: Pcairo_t read GetCairoContext;
    property Color: TColor read GetColor write SetColor;
    property Context: HDC read GetContext;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Font: PPangoFontDescription read GetFont write SetFont;
    property FontColor: TColor read GetFontColor write SetFontColor;
    property HasCaret: boolean read FHasCaret write FHasCaret;
    property KeysToEat: TByteSet read FKeysToEat write FKeysToEat;
    property PaintData: TPaintData read FPaintData write FPaintData;
    property Shape: PGdkPixbuf read FShape write SetShape;
    property StyleContext: PGtkStyleContext read GetStyleContext write SetStyleContext;
    property Text: String read getText write setText;
    property Visible: Boolean read GetVisible write SetVisible;
    property Widget: PGtkWidget read GetWidget;
    property WidgetMapped: boolean read FWidgetMapped write FWidgetMapped; {very important. Gtk3 does not give us reliable informations about this state get_mapped returns true, but actually map event isn't arrived yet.}
    property WidgetType: TGtk3WidgetTypes read FWidgetType;
  end;

  { TGtk3Editable }

  TGtk3Editable = class(TGtk3Widget)
  private
    function GetReadOnly: Boolean;
    procedure SetReadOnly(AValue: Boolean);
  protected
    PrivateCursorPos: Integer; // used only for delayed selStart and selLength
    PrivateSelection: Integer;
    function getCaretPos: TPoint; virtual;
    procedure SetCaretPos(AValue: TPoint); virtual;
  public
    function getSelStart: Integer; virtual;
    function getSelLength: Integer; virtual;
    procedure setSelStart(AValue: Integer); virtual;
    procedure setSelLength(AValue: Integer); virtual;
    property CaretPos: TPoint read GetCaretPos write SetCaretPos;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
  end;

  { TGtk3Entry }

  TGtk3Entry = class(TGtk3Editable)
  private
    function GetAlignment: TAlignment;
    procedure SetAlignment(AValue: TAlignment);
  strict private
    class procedure EntryChanged({%H-}AEntry: PGtkEntryBuffer; AData: GPointer); cdecl; static;
    class procedure InsertText(editable: PGtkEditable; aNewText: PgChar; anewtextlen: gint;
      var pos:Pgint; data: gpointer);cdecl; static;
    class procedure EntrySizeAllocate(AWidget: PGtkWidget; AGdkRect: PGdkRectangle; Data: gpointer); cdecl; static;
  protected
    procedure ConnectSizeAllocateSignal(ToWidget: PGtkWidget); override;
    function EatArrowKeys(const AKey: Word): Boolean; override;
    function getText: String; override;
    procedure setText(const AValue: String); override;
    function CreateWidget(const {%H-}Params: TCreateParams):PGtkWidget; override;
  public
    procedure preferredSize(var PreferredWidth, PreferredHeight: integer; {%H-}WithThemeSpace: Boolean); override;
    procedure InitializeWidget; override;
    procedure SetEchoMode(AVisible: Boolean);
    procedure SetMaxLength(AMaxLength: Integer);
    procedure SetPasswordChar(APasswordChar: Char);
    procedure SetNumbersOnly(ANumbersOnly:boolean);
    procedure SetTextHint(const AHint:string);
    procedure SetFrame(const aborder:boolean);
    procedure SetSelText(const ASelText: string);
    function GetTextHint:string;
    function IsWidgetOk: Boolean; override;
    property Alignment: TAlignment read GetAlignment write SetAlignment;
    property TextHint:string read GetTextHint write SetTextHint;
  end;

  { TGtk3SpinEdit }

  TGtk3SpinEdit = class(TGtk3Entry)
  strict  private
    class procedure SpinValueChanged({%H-}aSpin: PGtkSpinButton; aData: gpointer); cdecl; static;
  private
    function GetMaximum: Double;
    function GetMinimum: Double;
    function GetNumDigits: Integer;
    function GetNumeric: Boolean;
    function GetStep: Double;
    function GetValue: Double;
    procedure SetNumDigits(AValue: Integer);
    procedure SetNumeric(AValue: Boolean);
    procedure SetStep(AValue: Double);
    procedure SetValue(AValue: Double);
  protected
    function CreateWidget(const {%H-}Params: TCreateParams):PGtkWidget; override;
    function EatArrowKeys(const {%H-}AKey: Word): Boolean; override;
  public
    procedure InitializeWidget; override;
    function IsWidgetOk: Boolean; override;
    procedure SetRange(AMin, AMax: Double);
    property Minimum: Double read GetMinimum;
    property Maximum: Double read GetMaximum;
    property Numeric: Boolean read GetNumeric write SetNumeric;
    property NumDigits: Integer read GetNumDigits write SetNumDigits;
    property Step: Double read GetStep write SetStep;
    property Value: Double read GetValue write SetValue;
  end;

  { TGtk3Range }

  TGtk3Range = class(TGtk3Widget)
  strict private
    class procedure RangeChanged(ARange:PGtkRange;AData:gPointer); cdecl; static;
  private
    function GetPosition: Integer; reintroduce;
    function GetRange: TPoint;
    procedure SetPosition(AValue: Integer);
    procedure SetRange(AValue: TPoint);
  public
    procedure InitializeWidget; override;
    procedure SetStep(AStep: Integer; APageSize: Integer);
    property Range: TPoint read GetRange write SetRange;
    property Position: Integer read GetPosition write SetPosition;
  end;

  { TGtk3TrackBar }

  TGtk3TrackBar = class(TGtk3Range)
  private
    FOrientation: TTrackBarOrientation;
    function GetReversed: Boolean;
    procedure SetReversed(AValue: Boolean);
  protected
    function CreateWidget(const {%H-}Params: TCreateParams):PGtkWidget; override;
  public
    procedure SetBounds(ALeft,ATop,AWidth,AHeight:integer); override;
    function GetTrackBarOrientation: TTrackBarOrientation;
    procedure SetScalePos(AValue: TTrackBarScalePos);
    procedure SetTickMarks(AValue: TTickMark; ATickStyle: TTickStyle);
    property Reversed: Boolean read GetReversed write SetReversed;
  end;

  { TGtk3ScrollBar }

  TGtk3ScrollBar = class(TGtk3Range)
  protected
    class procedure ScrollBarValueChanged(adjustment: PGtkAdjustment; data:gpointer);cdecl; static;
    function CreateWidget(const {%H-}Params: TCreateParams):PGtkWidget; override;
  public
    procedure SetParams;
  end;

  { TGtk3ProgressBar }

  TGtk3ProgressBar = class(TGtk3Widget)
  private
    function GetOrientation: TProgressBarOrientation;
    function GetPosition: Integer; reintroduce;
    function GetShowText: Boolean;
    function GetStyle: TProgressBarStyle;
    procedure SetOrientation(AValue: TProgressBarOrientation);
    procedure SetPosition(AValue: Integer);
    procedure SetShowText(AValue: Boolean);
    procedure SetStyle(AValue: TProgressBarStyle);
  protected
    function CreateWidget(const {%H-}Params: TCreateParams):PGtkWidget; override;
  public
    procedure InitializeWidget; override;
    property Orientation: TProgressBarOrientation read GetOrientation write SetOrientation;
    property Position: Integer read GetPosition write SetPosition;
    property ShowText: Boolean read GetShowText write SetShowText;
    property Style: TProgressBarStyle read GetStyle write SetStyle;
  end;

  { TGtk3Calendar }

  TGtk3Calendar = class(TGtk3Widget)
  protected
    function CreateWidget(const {%H-}Params: TCreateParams):PGtkWidget; override;
  public
    procedure GetDate(out AYear, AMonth, ADay: LongWord);
    procedure SetDate(const AYear, AMonth, ADay: LongWord);
    procedure SetDisplayOptions(const ADisplayOptions: TGtkCalendarDisplayOptions);
  end;

  { TGtk3StaticText }

  TGtk3StaticText = class(TGtk3Widget)
  private
    function GetAlignment: TAlignment;
    function GetStaticBorderStyle: TStaticBorderStyle;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetStaticBorderStyle(AValue: TStaticBorderStyle);
  protected
    function getText: String; override;
    procedure setText(const AValue: String); override;
    function CreateWidget(const {%H-}Params: TCreateParams):PGtkWidget; override;
  public
    property Alignment: TAlignment read GetAlignment write SetAlignment;
    property StaticBorderStyle: TStaticBorderStyle read GetStaticBorderStyle write SetStaticBorderStyle;
  end;

  { TGtk3Container }

  TGtk3Container = class(TGtk3Widget)
  protected
    procedure SetVisible(AValue: Boolean); override;
  public
    procedure InitializeWidget; override;
  end;

  { TGtk3Page }

  TGtk3Page = class(TGtk3Container)
  private
    FPageBox: PGtkBox;
    FImageWidget: PGtkImage;
    FPageLabel: PGtkLabel;
    FCloseButton: PGtkButton;
    function GetCloseButtonVisible: boolean;
    procedure SetCloseButtonVisible(AValue: boolean);
  strict private
    class procedure TabSheetLayoutSizeAllocate(AWidget: PGtkWidget;
      AGdkRect: PGdkRectangle; Data: gpointer); cdecl; static;
    class procedure TabCloseClicked(aButton: PGtkButton; aData: gPointer);
      cdecl; static;
  protected
    procedure DoBeforeLCLPaint; override;
    procedure setText(const AValue: String); override;
    function CreateWidget(const Params: TCreateParams):PGtkWidget; override;
    procedure DestroyWidget; override;
  public
    destructor Destroy; override;
    function ClientToScreen(var P:TPoint):boolean; override;
    function getClientOffset:TPoint; override;
    function getClientRect: TRect; override;
    procedure setTabImage(aBitmap: TBitmap);
    property CloseButtonVisible: boolean read GetCloseButtonVisible write SetCloseButtonVisible;
  end;

  { TGtk3NoteBook }

  TGtk3NoteBook = class (TGtk3Container)
  private
    FDefaultClientRect:TRect;
  protected
    function CreateWidget(const {%H-}Params: TCreateParams):PGtkWidget; override;
  public
    procedure InitializeWidget; override;
    function GetTabSize(AWinControl: TWinControl): integer; {returns size of tab. Height if orientation is top/bottom, width if orientation is left/right}
    function getClientRect: TRect; override;
    function getPagesCount: integer;
    procedure InsertPage(ACustomPage: TCustomPage; AIndex: Integer);
    procedure MovePage(ACustomPage: TCustomPage; ANewIndex: Integer);
    procedure RemovePage(AIndex: Integer);
    procedure SetPageIndex(AIndex: Integer);
    procedure SetShowTabs(const AShowTabs: Boolean);
    procedure SetTabPosition(const ATabPosition: TTabPosition);
    procedure SetTabLabelText(AChild: TCustomPage; const AText: String);
    function GetTabLabelText(AChild: TCustomPage): String;
    property DefaultClientRect: TRect read FDefaultClientRect write FDefaultClientRect; //measured in gtk3wscomctrls.getDefaultClientRect
  end;

  { TGtk3Bin }

  TGtk3Bin = class(TGtk3Container)
  end;


  { TGtk3Paned }

  TGtk3Paned = class(TGtk3Container)
  protected
    function CreateWidget(const {%H-}Params: TCreateParams):PGtkWidget; override;
  end;

  { TGtk3SplitterSide }

  TGtk3SplitterSide = class(TGtk3Container)
  protected
    function CreateWidget(const {%H-}Params: TCreateParams):PGtkWidget; override;
  end;


  { TGtk3MenuShell }

  TGtk3MenuShell = class(TGtk3Container)
  public
    MenuObject: TMenu;
    constructor Create(const AMenu: TMenu; AMenuBar: PGtkMenuBar); virtual; overload;
    procedure Insert(AMenuShell: PGtkMenuShell; APosition: Integer);
    procedure InitializeWidget; override;
  end;

  { TGtk3MenuBar }

  TGtk3MenuBar = class(TGtk3MenuShell)
  protected
    function CreateWidget(const {%H-}Params: TCreateParams):PGtkWidget; override;
  end;

  { TGtk3Menu }

  TGtk3Menu = class(TGtk3MenuShell)
  protected
    function CreateWidget(const {%H-}Params: TCreateParams):PGtkWidget; override;
  public
    PopupPoint: TPoint;
    constructor CreateFromMenuItem(const AMenuItem: TMenuItem); virtual; overload;
  end;

  { TGtk3MenuItem }

  TGtk3MenuItem = class(TGtk3Bin)
  private
    function GetCaption: string;
    procedure SetCaption(const AValue: string);
  strict private
    class procedure MenuItemActivated(AItem:PGtkMenuItem;AData:GPointer); cdecl; static;
    class function MenuItemEvent(AWidget:PGtkWidget;event:PGdkEvent; data:GPointer):gboolean; cdecl; static;
  protected
    function CreateWidget(const {%H-}Params: TCreateParams):PGtkWidget; override;
  public
    Lock:integer;
    MenuItem: TMenuItem;
    constructor Create(const AMenuItem: TMenuItem); virtual; overload;
    procedure InitializeWidget; override;
    procedure SetCheck(ACheck:boolean);
    property Caption: string read GetCaption write SetCaption;
  end;

  { TGtk3ScrollableWin }

  TGtk3ScrollableWin = class(TGtk3Container)
  private
    FBorderStyle: TBorderStyle;
    FHBarInitialized, FVBarInitialized: boolean;
    function GetHScrollBarPolicy: TGtkPolicyType;
    function GetVScrollBarPolicy: TGtkPolicyType;
    procedure SetBorderStyle(AValue: TBorderStyle);
    procedure SetHScrollBarPolicy(AValue: TGtkPolicyType); virtual;
    procedure SetVScrollBarPolicy(AValue: TGtkPolicyType); virtual;
  protected
    class procedure ScrolledLayoutSizeAllocate(AWidget: PGtkWidget;
      AGdkRect: PGdkRectangle; Data: gpointer); cdecl; static; {very important, see note inside method}

    class function RangeChangeValue(ARange: PGtkRange; AScrollType: TGtkScrollType;
      AValue: gdouble; AData: gPointer): gboolean; cdecl; static;
    class procedure RangeValueChanged(range: PGtkRange; data: gpointer); cdecl; static;
  public
    LCLVAdj: PGtkAdjustment; // used to keep LCL values
    LCLHAdj: PGtkAdjustment; // used to keep LCL values
    function ClientToScreen(var P:TPoint):boolean; override;
    procedure DestroyWidget; override;
    {result = true if scrollbar is pressed by mouse, AMouseOver if mouse is over scrollbar pressed or not.}
    class function CheckIfScrollbarPressed(scrollbar: PGtkWidget; out AMouseOver:
       boolean; const ACheckModifier: TGdkModifierTypeIdx): boolean;
    procedure InitializeWidget; override;
    procedure OffsetMousePos(const aGlobalX, aGlobalY: double; APoint: PPoint); override;
    procedure SetScrollBarsSignalHandlers(const AIsHorizontalScrollBar: boolean);
    function getClientBounds: TRect; override;
    function getViewport: PGtkViewport; virtual;
    function getHorizontalScrollbar: PGtkScrollbar; virtual; abstract;
    function getVerticalScrollbar: PGtkScrollbar; virtual; abstract;
    function getScrolledWindow: PGtkScrolledWindow; virtual; abstract;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle;
    property HScrollBarPolicy: TGtkPolicyType read GetHScrollBarPolicy write SetHScrollBarPolicy;
    property VScrollBarPolicy: TGtkPolicyType read GetVScrollBarPolicy write SetVScrollBarPolicy;
  end;

  { TGtk3Memo }

  TGtk3Memo = class(TGtk3ScrollableWin)
  strict private
    class procedure MemoTextChanged(aBuffer: PGtkTextBuffer; aData: gPointer); cdecl; static;
  private
    function GetAlignment: TAlignment;
    function GetCaretPos: TPoint;
    function GetReadOnly: Boolean;
    function GetWantTabs: Boolean;
    function GetWordWrap: Boolean;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetCaretPos(AValue: TPoint);
    procedure SetReadOnly(AValue: Boolean);
    procedure SetWantTabs(AValue: Boolean);
    procedure SetWordWrap(AValue: Boolean);
  protected
    function getText: String; override;
    procedure setText(const AValue: String); override;
    function CreateWidget(const {%H-}Params: TCreateParams):PGtkWidget; override;
    function EatArrowKeys(const {%H-}AKey: Word): Boolean; override;
  public
    function getHorizontalScrollbar: PGtkScrollbar; override;
    function getVerticalScrollbar: PGtkScrollbar; override;
    function GetScrolledWindow: PGtkScrolledWindow; override;
  public
    function getSelStart: Integer; virtual;
    function getSelLength: Integer; virtual;
    procedure InitializeWidget; override;
    procedure setSelStart(AValue: Integer); virtual;
    procedure setSelLength(AValue: Integer); virtual;
    procedure setSelText(const ANewSelText: string); virtual;

    property Alignment: TAlignment read GetAlignment write SetAlignment;
    property CaretPos: TPoint read GetCaretPos write SetCaretPos;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property WantTabs: Boolean read GetWantTabs write SetWantTabs;
    property WordWrap: Boolean read GetWordWrap write SetWordWrap;
  end;

  { TGtk3ListBox }

  TGtk3ListBox = class(TGtk3ScrollableWin)
  strict private
    class procedure ListBoxSelectionChanged(ASelection: PGtkTreeSelection;
      AData: GPointer); cdecl; static;
  private
    FListBoxStyle: TListBoxStyle;
    function GetItemIndex: Integer;
    function GetMultiSelect: Boolean;
    procedure SetItemIndex(AValue: Integer);
    procedure SetListBoxStyle(AValue: TListBoxStyle);
    procedure SetMultiSelect(AValue: Boolean);
  protected
    function CreateWidget(const {%H-}Params: TCreateParams):PGtkWidget; override;
    function EatArrowKeys(const {%H-}AKey: Word): Boolean; override;
    procedure SetColor(AValue: TColor); override;
  public
    procedure InitializeWidget; override;
    function getHorizontalScrollbar: PGtkScrollbar; override;
    function getVerticalScrollbar: PGtkScrollbar; override;
    function GetScrolledWindow: PGtkScrolledWindow; override;
  public
    function GetSelCount: Integer;
    function GetSelection: PGtkTreeSelection;
    function GetItemRect(const AIndex: integer): TRect;
    function GetIndexAtXY(const X, Y: integer): integer;
    function GetItemSelected(const AIndex: Integer): Boolean;
    function GetScrollWidth: integer;
    procedure SelectItem(const AIndex: Integer; ASelected: Boolean);
    procedure SetTopIndex(const AIndex: Integer);
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property MultiSelect: Boolean read GetMultiSelect write SetMultiSelect;
    property ListBoxStyle: TListBoxStyle read FListBoxStyle write SetListBoxStyle;
  end;

  { TGtk3CheckListBox }

  TGtk3CheckListBox = class(TGtk3ListBox)
  strict private
    class procedure CheckListBoxDataFunc(tree_column: PGtkTreeViewColumn;
      cell: PGtkCellRenderer; tree_model: PGtkTreeModel; iter: PGtkTreeIter;
      data: gPointer); cdecl; static;
    class procedure CheckListBoxToggle(
      cellrenderertoggle: PGtkCellRendererToggle; arg1: PGChar; AData: GPointer
      ); cdecl; static;
  protected
    function CreateWidget(const {%H-}Params: TCreateParams): PGtkWidget; override;
  end;

  { TGtk3ListView }

  TGtk3ListView = class(TGtk3ScrollableWin)
  strict private
    class procedure ListViewColumnClicked(column: PGtkTreeViewColumn;
      AData: GPointer); cdecl; static;
    class procedure ListViewGetPixbufDataFuncForColumn(
      tree_column: PGtkTreeViewColumn; cell: PGtkCellRenderer;
      tree_model: PGtkTreeModel; iter: PGtkTreeIter; AData: GPointer); cdecl; static;
    class function ListViewItemPreSelected(selection: PGtkTreeSelection;
      model: PGtkTreeModel; path: PGtkTreePath;
      path_is_currently_selected: GBoolean; AData: GPointer): GBoolean; cdecl; static;
    class procedure ListViewItemSelected(ASelection: PGtkTreeSelection;
      AData: GPointer); cdecl; static;
  private
    FPreselectedIndices: TFPList;
    FImages: TFPList;
    FIsTreeView: Boolean;
    FViewStyle: TViewStyle;
  protected
    function CreateWidget(const {%H-}Params: TCreateParams):PGtkWidget; override;
    function EatArrowKeys(const {%H-}AKey: Word): Boolean; override;
    procedure SetColor(AValue: TColor); override;
    class function selection_changed(AIconView: PGtkIconView; aData: gPointer):gboolean; cdecl; static;
  public
    destructor Destroy; override;
    {interface implementation}
    function getHorizontalScrollbar: PGtkScrollbar; override;
    function getVerticalScrollbar: PGtkScrollbar; override;
    function GetScrolledWindow: PGtkScrolledWindow; override;
    procedure ClearImages;
    procedure ColumnDelete(AIndex: Integer);
    function ColumnGetWidth(AIndex: Integer): Integer;
    procedure ColumnInsert(AIndex: Integer; AColumn: TListColumn);
    procedure SetAlignment(AIndex: Integer; {%H-}AColumn: TListColumn; AAlignment: TAlignment);
    procedure SetColumnAutoSize(AIndex: Integer; {%H-}AColumn: TListColumn; AAutoSize: Boolean);
    procedure SetColumnCaption(AIndex: Integer; {%H-}AColumn: TListColumn; const ACaption: String);
    procedure SetColumnMaxWidth(AIndex: Integer; {%H-}AColumn: TListColumn; AMaxWidth: Integer);
    procedure SetColumnMinWidth(AIndex: Integer; {%H-}AColumn: TListColumn; AMinWidth: Integer);
    procedure SetColumnWidth(AIndex: Integer; {%H-}AColumn: TListColumn; AWidth: Integer);
    procedure SetColumnVisible(AIndex: Integer; {%H-}AColumn: TListColumn; AVisible: Boolean);
    procedure ColumnSetSortIndicator(const AIndex: Integer; const {%H-}AColumn: TListColumn; const ASortIndicator: TSortIndicator);

    procedure UpdateItem(AIndex:integer;AItem: TListItem);
    procedure ItemDelete(AIndex: Integer);
    function  ItemDisplayRect(AIndex: Integer; ASubItem: integer; ACode: TDisplayCode): TRect;
    procedure ItemInsert(AIndex: Integer; AItem: TListItem);
    function ItemPosition(AIndex: integer): TPoint;
    procedure ItemSetText(AIndex, ASubIndex: Integer; AItem: TListItem; const AText: String);
    procedure ItemSetImage(AIndex, ASubIndex: Integer; AItem: TListItem);
    procedure ItemSetState(const AIndex: Integer; const {%H-}AItem: TListItem; const AState: TListItemState;
      const AIsSet: Boolean);
    function ItemGetState(const AIndex: Integer; const {%H-}AItem: TListItem; const AState: TListItemState;
      out AIsSet: Boolean): Boolean;
    procedure setItemWidth(const AImageListWidth: integer=0); //calculates width of vsIcon item. Param aWidth is imageList.Width
    procedure ScrollToRow(const ARow: integer);
    procedure UpdateImageCellsSize;

    property Images: TFPList read FImages write FImages;
    property IsTreeView: Boolean read FIsTreeView;
    property ViewStyle: TViewStyle read FViewStyle;
  end;

  { TGtk3Box }

  TGtk3Box = class(TGtk3Container)

  end;

  { TGtk3StatusBarPanel }

  TGtk3StatusBarPanel = class(TCollectionItem)
  private
    //FFixed: PGtkFixed;
    FLabel: PGtkLabel;
    function GetPanelText: string;
    function GetPanelWidth: integer;
    procedure SetPanelText(AValue: string);
    procedure SetPanelWidth(AValue: integer);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    property PanelText: string read GetPanelText write SetPanelText;
    property PanelWidth: integer read GetPanelWidth write SetPanelWidth;
  end;

  { TGtk3StatusBarPanels }

  TGtk3StatusBarPanels = class(TCollection)
  private
    function GetPanel(AIndex: integer): TGtk3StatusBarPanel;
  protected
    FStatusBar: TGtk3Widget;
  public
    constructor Create(AStatusBar: TGtk3Widget);
    function Add: TGtk3StatusBarPanel;
    function AddPanel(APanel: TStatusPanel; ABox: PGtkBox): TGtk3StatusBarPanel;
    property Panels[AIndex: integer]: TGtk3StatusBarPanel read GetPanel;
  end;

  { TGtk3StatusBar }

  TGtk3StatusBar = class(TGtk3Box)
  protected
    FPanels: TGtk3StatusBarPanels;
    FSimplePanel: PGtkLabel;
    function CreateWidget(const {%H-}Params: TCreateParams):PGtkWidget; override;
  public
    destructor Destroy; override;
    procedure SetPanelText(const AText: string; const APanelIndex: integer);
    procedure SetSimpleText(const AText: string);
    procedure UpdateStatusBar(AStatusBar: TStatusBar);
  end;

  { TGtk3Panel }

  TGtk3Panel = class(TGtk3Bin)
  strict private
    class procedure PanelLayoutSizeAllocate(AWidget: PGtkWidget;
      AGdkRect: PGdkRectangle; Data: gpointer); cdecl; static;
  private
    FBorderStyle: TBorderStyle;
    procedure SetBorderStyle(AValue: TBorderStyle);
  protected
    function CreateWidget(const {%H-}Params: TCreateParams):PGtkWidget; override;
    procedure DoBeforeLCLPaint; override;
    procedure setText(const AValue: String); override;
  public
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle;
  end;

  { TGtk3GroupBox }

  TGtk3GroupBox = class(TGtk3Bin)
  strict private
    class procedure GroupBoxLayoutSizeAllocate(AWidget: PGtkWidget;
      AGdkRect: PGdkRectangle; Data: gpointer); cdecl; static;
    class procedure GroupBoxSizeAllocate(AWidget: PGtkWidget; AGdkRect: PGdkRectangle; Data: gpointer); cdecl; static;
  private
    FGroupBoxType:TGtk3GroupBoxType;
    function GetInnerClientRect(Frame:PGtkWidget):TRect;
  protected
    procedure DoBeforeLCLPaint; override;
    procedure ConnectSizeAllocateSignal(ToWidget: PGtkWidget); override;
    function CreateWidget(const {%H-}Params: TCreateParams):PGtkWidget; override;
    function getText: String; override;
    procedure setText(const AValue: String); override;
  public
    function getClientOffset:TPoint; override;
    function getClientRect:TRect; override;
    property GroupBoxType: TGtk3GroupBoxType read FGroupBoxType write FGroupBoxType;
  end;

  { TGtk3ComboBox }

  TGtk3ComboBox = class(TGtk3Bin)
  private
    FCellView: PGtkCellView;
    function GetItemIndex: Integer;
    procedure SetDroppedDown(AValue: boolean);
    procedure SetItemIndex(AValue: Integer);
    function GetDroppedDown: boolean;
  strict private
    class procedure ComboSizeAllocate(AWidget: PGtkWidget; AGdkRect: PGdkRectangle; Data: gpointer); cdecl; static;
    class procedure ComboBoxChanged({%H-}ACombo: PGtkComboBox; AData: gpointer); cdecl; static;
    class procedure NotifySignal(AObject: PGObject; pspec: PGParamSpec; AData: GPointer); cdecl; static;
  protected
    procedure ConnectSizeAllocateSignal(ToWidget:PGtkWidget);override;
    function CreateWidget(const {%H-}Params: TCreateParams):PGtkWidget; override;
    function EatArrowKeys(const AKey: Word): Boolean; override;
    function getText: String; override;
    procedure setText(const AValue: String); override;
  public
    procedure DumpPrivateStructValues(const ADbgEvent: String);
  public
    function CanFocus: Boolean; override;
    procedure SetBounds(ALeft,ATop,AWidth,AHeight:integer); override;
    procedure SetFocus; override;
    function GetCellView: PGtkCellView;
    function GetPopupWidget: PGtkWidget;
    function GetButtonWidget: PGtkWidget;
    function GetArrowWidget: PGtkWidget;
    function getSelStart: integer;
    function getSelLength: integer;
    function getMaxLength: integer;
    procedure SetMaxLength(const AMaxLength: integer);
    procedure SetSelStart(const ANewStart: integer);
    procedure SetSelLength(const ANewLength: integer);
    procedure InitializeWidget; override;
    property DroppedDown: boolean read GetDroppedDown write SetDroppedDown;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
  end;

  { TGtk3Button }

  TGtk3Button = class(TGtk3Bin)
  private
    FMargin: Integer;
    FLayout: Integer;
    FSpacing: Integer;
    FImage: TBitmap;
    function getLayout: Integer;
    function getMargin: Integer;
    procedure SetLayout(AValue: Integer);
    procedure SetMargin(AValue: Integer);
    procedure SetSpacing(AValue: Integer);
  strict private
    class function ButtonMouseEvent(aWidget: PGtkWidget; aEvent: PGdkEvent;
      aData: gpointer): gboolean; cdecl; static;
  protected
    procedure SetImage(AImage:TBitmap);
    function getText: String; override;
    procedure setText(const AValue: String); override;
    function CreateWidget(const {%H-}Params: TCreateParams):PGtkWidget; override;
  public
    destructor Destroy; override;
    procedure InitializeWidget; override;
    function IsWidgetOk: Boolean; override;
    procedure SetDefault(const ADefault: Boolean);
    property Layout: Integer read getLayout write SetLayout;
    property Margin: Integer read getMargin write SetMargin;
    property Spacing: Integer read FSpacing write SetSpacing;
    property Image:TBitmap read fImage write SetImage;
  end;

  { TGtk3ToggleButton }

  TGtk3ToggleButton = class(TGtk3Button)
  strict private
    class procedure ButtonToggled(AWidget: PGtkToggleButton; AData: gPointer);
      cdecl; static;
  protected
    function CreateWidget(const {%H-}Params: TCreateParams):PGtkWidget; override;
  public
    procedure InitializeWidget; override;
  end;

  { TGtk3CheckBox }

  TGtk3CheckBox = class(TGtk3ToggleButton)
  private
    function GetState: TCheckBoxState;
    procedure SetState(AValue: TCheckBoxState);
  protected
    function CreateWidget(const {%H-}Params: TCreateParams):PGtkWidget; override;
  public
    procedure SetBounds(ALeft,ATop,AWidth,AHeight:integer); override;
    property State: TCheckBoxState read GetState write SetState;
  end;

  { TGtk3RadioButton }

  TGtk3RadioButton = class(TGtk3CheckBox)
  protected
    function CreateWidget(const {%H-}Params: TCreateParams):PGtkWidget; override;
  public
    function getClientRect:TRect; override;
    procedure InitializeWidget; override;
  end;

  { TGtk3CustomControl }

  TGtk3CustomControl = class(TGtk3ScrollableWin)
    protected
      function CreateWidget(const {%H-}Params: TCreateParams):PGtkWidget; override;
      function EatArrowKeys(const {%H-}AKey: Word): Boolean; override;
    public
      procedure Update(ARect: PRect); override;
      procedure DoBeforeLCLPaint; override;
      procedure InitializeWidget; override;
      function getViewport: PGtkViewport; override;
      procedure preferredSize(var PreferredWidth, PreferredHeight: integer; {%H-}WithThemeSpace: Boolean); override;
      function getClientRect: TRect; override;
      function getHorizontalScrollbar: PGtkScrollbar; override;
      function getVerticalScrollbar: PGtkScrollbar; override;
      function GetScrolledWindow: PGtkScrolledWindow; override;
  end;

  { TGtk3ToolBar }

  TGtk3ToolBar = class(TGtk3CustomControl)
  public
    function CreateWidget(const {%H-}Params: TCreateParams):PGtkWidget; override;
  end;

  { TGtk3ScrollingWinControl }

  TGtk3ScrollingWinControl = class(TGtk3CustomControl)
  strict private
    class procedure ScrollingWinControlFixedSizeAllocate(AWidget: PGtkWidget;
      AGdkRect: PGdkRectangle; Data: gpointer); cdecl; static;
    protected
      function CreateWidget(const {%H-}Params: TCreateParams):PGtkWidget; override;
  end;

  { TGtk3Splitter }

  TGtk3Splitter = class(TGtk3Panel)
  public
  end;


  { TGtk3Window }

  TGtk3Window = class(TGtk3ScrollingWinControl) {we are TGtk3Bin actually, but it won't hurt since we need scroll}
  private
    FIcon: PGdkPixBuf;
    FScrollWin: PGtkScrolledWindow;
    FMenuBar: PGtkMenuBar;
    FBox: PGtkBox;
    function GetSkipTaskBarHint: Boolean;
    function GetTitle: String;
    procedure SetIcon(AValue: PGdkPixBuf);
    procedure SetSkipTaskBarHint(AValue: Boolean);
    procedure SetTitle(const AValue: String);
  strict private
    class function WindowMapEvent(awidget:PGtkWindow;AEvent: PGdkEventAny; adata: gpointer): gboolean; cdecl; static; //uses lcl-window-first-map data.
    class function WindowMoveEvent(awidget: PGtkWindow; AEvent: PGdkEventConfigure; adata: gpointer): gboolean; cdecl; static;
    class procedure WindowSizeAllocate(AWidget: PGtkWidget; AGdkRect: PGdkRectangle; Data: gpointer); cdecl; static;
    class function WindowStateSignal(AWidget: PGtkWidget; AEvent: PGdkEvent; AData: gPointer): gboolean; cdecl; static;
  protected
    FFirstMapRect: TRect;
    procedure ConnectSizeAllocateSignal(ToWidget: PGtkWidget); override;
    function CreateWidget(const {%H-}Params: TCreateParams):PGtkWidget; override;
    function EatArrowKeys(const {%H-}AKey: Word): Boolean; override;
    function getText: String; override;
    procedure setText(const AValue: String); override;
  public
    // function getClientBounds: TRect; override;
    function getViewport: PGtkViewport; override;
    function getClientRect: TRect; override;
    function getHorizontalScrollbar: PGtkScrollbar; override;
    function getVerticalScrollbar: PGtkScrollbar; override;
    function GetScrolledWindow: PGtkScrolledWindow; override;
    procedure InitializeWidget; override;
    function ShowState(nstate:integer):boolean; // winapi ShowWindow
    procedure UpdateWindowState; // LCL WindowState
    class function decoration_flags(Aform: TCustomForm): TGdkWMDecoration;
  public
    procedure DoBeforeLCLPaint; override;
    procedure SetBounds(ALeft,ATop,AWidth,AHeight:integer); override;
    destructor Destroy; override;
    procedure Activate; override;
    procedure ActivateWindow(AEvent: PGdkEvent);
    function CloseQuery: Boolean;
    function GetWindow: PGdkWindow; override;
    function GetMenuBar: PGtkMenuBar;
    function GetBox: PGtkBox;
    function GetWindowState: TGdkWindowState;
    property Icon: PGdkPixBuf read FIcon write SetIcon;
    property SkipTaskBarHint: Boolean read GetSkipTaskBarHint write SetSkipTaskBarHint;
    property Title: String read GetTitle write SetTitle;
  end;

  { TGtk3HintWindow }

  TGtk3HintWindow = class(TGtk3Window)
  protected
    function CreateWidget(const {%H-}Params: TCreateParams):PGtkWidget; override;
  end;

  { TGtk3Dialog }

  TGtk3Dialog = class(TGtk3Widget)
  private
    class function CloseCB(dlg:TGtk3Dialog): GBoolean; cdecl;
    class function CloseQueryCB(w:PGtkWidget;dlg:TGtk3Dialog): GBoolean; cdecl;
    class function DestroyCB(dlg:TGtk3Dialog): GBoolean; cdecl;
    class function ResponseCB(response_id:gint; dlg: TGtk3Dialog): GBoolean; cdecl;
    class function RealizeCB(dlg:TGtk3Dialog): GBoolean; cdecl;
  protected
    function response_handler(response_id:TGtkResponseType):boolean; virtual;
    function close_handler():boolean; virtual;
    procedure SetCallbacks; virtual;
    function CreateWidget(const {%H-}Params: TCreateParams):PGtkWidget; override;
  public
    CommonDialog: TCommonDialog;
    procedure InitializeWidget; override;
    procedure CloseDialog; virtual;
  end;

  { TGtk3FileDialog }

  TGtk3FileDialog = class(TGtk3Dialog)
  private
  protected
    function CreateWidget(const {%H-}Params: TCreateParams): PGtkWidget; override;
  public
    constructor Create(const ACommonDialog: TCommonDialog); virtual; overload;
  end;

  { TGtk3FontSelectionDialog }

  TGtk3FontSelectionDialog = class(TGtk3Dialog)
  protected
    function response_handler(resp_id:TGtkResponseType):boolean; override;
  public
    procedure InitializeWidget; override;
    constructor Create(const ACommonDialog: TCommonDialog); virtual; overload;
  end;

  { TGtk3ColorSelectionDialog }

  TGtk3ColorSelectionDialog = class(TGtk3Dialog)
  public
    procedure InitializeWidget; override;
    constructor Create(const ACommonDialog: TCommonDialog); virtual; overload;
  end;

  { TGtk3newColorSelectionDialog }

  TGtk3newColorSelectionDialog = class(TGtk3Dialog)
  protected
    function response_handler(resp_id:TGtkResponseType):boolean; override;
  public
    constructor Create(const ACommonDialog: TCommonDialog); virtual; overload;
    procedure InitializeWidget; override;
    class procedure color_to_rgba(clr:TColor;out rgba:TgdkRGBA);
    class function rgba_to_color(const rgba:TgdkRGBA):TColor;
  end;

  { TGtk3GLArea }
  TGtk3GLArea = class(TGtk3Widget)
  protected
    function CreateWidget(const {%H-}Params: TCreateParams): PGtkWidget; override;
  public
    procedure Update({%H-}ARect: PRect); override;
  end;

  { TGtk3DesignWidget }

  TGtk3DesignWidget = class(TGtk3Window)
  protected
    FDesignContext: HDC;
    procedure BringDesignerToFront;
    procedure ResizeDesigner;
    function CreateWidget(const {%H-}Params: TCreateParams): PGtkWidget; override;
    function GetContext: HDC; override;
  public
    procedure InitializeWidget; override;
    function GtkEventPaint(Sender: PGtkWidget; AContext: Pcairo_t): Boolean; override; cdecl;
    procedure lowerWidget; override;
    procedure raiseWidget; override;
    property DesignContext: HDC read FDesignContext;
  end;

const
  LISTVIEW_DEFAULT_COLUMN = 1;

implementation

uses {$IFDEF GTK3DEBUGKEYPRESS}TypInfo,{$ENDIF}gtk3int, gtk3caret, imglist,
  uriparser, lclproc, LazLogger;

const

  GDK_DEFAULT_EVENTS_MASK = [
    GDK_EXPOSURE_MASK, {2}
    GDK_POINTER_MOTION_MASK, {4}
    GDK_POINTER_MOTION_HINT_MASK, {8}
    GDK_BUTTON_MOTION_MASK, {16}
    GDK_BUTTON1_MOTION_MASK, {32}
    GDK_BUTTON2_MOTION_MASK, {64}
    GDK_BUTTON3_MOTION_MASK, {128}
    GDK_BUTTON_PRESS_MASK, {256}
    GDK_BUTTON_RELEASE_MASK, {512}
    GDK_KEY_PRESS_MASK, {1024}
    GDK_KEY_RELEASE_MASK, {2048}
    GDK_ENTER_NOTIFY_MASK, {4096}
    GDK_LEAVE_NOTIFY_MASK, {8192}
    GDK_FOCUS_CHANGE_MASK, {16384}
    GDK_STRUCTURE_MASK, {32768}
    GDK_PROPERTY_CHANGE_MASK, {65536}
    GDK_VISIBILITY_NOTIFY_MASK, {131072}
    GDK_PROXIMITY_IN_MASK, {262144}
    GDK_PROXIMITY_OUT_MASK, {524288}
    GDK_SUBSTRUCTURE_MASK, {1048576}
    GDK_SCROLL_MASK, {2097152}
    GDK_TOUCH_MASK {4194304}
 // GDK_SMOOTH_SCROLL_MASK {8388608} //there is a bug in GTK3, see https://stackoverflow.com/questions/11775161/gtk3-get-mouse-scroll-direction
  ];

function Gtk3EventToStr(AEvent: TGdkEventType): String;
begin
  Result := 'GDK_NOTHING';
  case AEvent of
    GDK_DELETE: Result := 'GDK_DELETE';
    GDK_DESTROY: Result := 'GDK_DESTROY';
    GDK_EXPOSE: Result := 'GDK_EXPOSE';
    GDK_MOTION_NOTIFY: Result := 'GDK_MOTION_NOTIFY';
    GDK_BUTTON_PRESS: Result := 'GDK_BUTTON_PRESS';
    GDK_2BUTTON_PRESS: Result := 'GDK_2BUTTON_PRESS';
    GDK_3BUTTON_PRESS: Result := 'GDK_3BUTTON_PRESS';
    GDK_BUTTON_RELEASE: Result := 'GDK_BUTTON_RELEASE';
    GDK_KEY_PRESS: Result := 'GDK_KEY_PRESS';
    GDK_KEY_RELEASE: Result := 'GDK_KEY_RELEASE';
    GDK_ENTER_NOTIFY: Result := 'GDK_ENTER_NOTIFY';
    GDK_LEAVE_NOTIFY: Result := 'GDK_LEAVE_NOTIFY';
    GDK_FOCUS_CHANGE: Result := 'GDK_FOCUS_CHANGE';
    GDK_CONFIGURE: Result := 'GDK_CONFIGURE';
    GDK_MAP: Result := 'GDK_MAP';
    GDK_UNMAP: Result := 'GDK_UNMAP';
    GDK_PROPERTY_NOTIFY: Result := 'GDK_PROPERTY_NOTIFY';
    GDK_SELECTION_CLEAR: Result := 'GDK_SELECTION_CLEAR';
    GDK_SELECTION_REQUEST: Result := 'GDK_SELECTION_REQUEST';
    GDK_SELECTION_NOTIFY: Result := 'GDK_SELECTION_NOTIFY';
    GDK_PROXIMITY_IN: Result := 'GDK_PROXIMITY_IN';
    GDK_PROXIMITY_OUT: Result := 'GDK_PROXIMITY_OUT';
    GDK_DRAG_ENTER: Result := 'GDK_DRAG_ENTER';
    GDK_DRAG_LEAVE: Result := 'GDK_DRAG_LEAVE';
    GDK_DRAG_MOTION_: Result := 'GDK_DRAG_MOTION_';
    GDK_DRAG_STATUS_: Result := 'GDK_DRAG_STATUS_';
    GDK_DROP_START: Result := 'GDK_DROP_START';
    GDK_DROP_FINISHED: Result := 'GDK_DROP_FINISHED';
    GDK_CLIENT_EVENT: Result := 'GDK_CLIENT_EVENT';
    GDK_VISIBILITY_NOTIFY: Result := 'GDK_VISIBILITY_NOTIFY';
    GDK_SCROLL: Result := 'GDK_SCROLL';
    GDK_WINDOW_STATE: Result := 'GDK_WINDOW_STATE';
    GDK_SETTING: Result := 'GDK_SETTING';
    GDK_OWNER_CHANGE: Result := 'GDK_OWNER_CHANGE';
    GDK_GRAB_BROKEN: Result := 'GDK_GRAB_BROKEN';
    GDK_DAMAGE: Result := 'GDK_DAMAGE';
    GDK_TOUCH_BEGIN: Result := 'GDK_TOUCH_BEGIN';
    GDK_TOUCH_UPDATE: Result := 'GDK_TOUCH_UPDATE';
    GDK_TOUCH_END: Result := 'GDK_TOUCH_END';
    GDK_TOUCH_CANCEL: Result := 'GDK_TOUCH_CANCEL';
    GDK_EVENT_LAST: Result := 'GDK_EVENT_LAST';
    else
      Result := 'UNKNOWN GDK EVENT';
  end;
end;

function GtkModifierStateToShiftState(AState: TGdkModifierType;
    AIsKeyEvent: Boolean): Cardinal;
begin
  Result := 0;
  if GDK_SHIFT_MASK in AState  then
    Result := Result or MK_SHIFT;
  if GDK_CONTROL_MASK in AState  then
    Result := Result or MK_CONTROL;
  if GDK_MOD1_MASK in AState  then
  begin
    if AIsKeyEvent then
      Result := Result or KF_ALTDOWN
    else
      Result := Result or MK_ALT;
  end;
end;

{$i gtk3lclcombobox.inc}
{$i gtk3lclentry.inc}
{$i gtk3lclbutton.inc}
{$i gtk3lclspinbutton.inc}
{$i gtk3lclframe.inc}
{$i gtk3lclnotebook.inc}

class function TGtk3Widget.WidgetEvent(widget: PGtkWidget; event: PGdkEvent; data: GPointer): gboolean; cdecl;
var
  AForm: TCustomForm;
begin
  {$IFDEF GTK3DEBUGCOMBOBOX}
  if (Data <> nil) and (wtComboBox in TGtk3Widget(Data).WidgetType) and
    (event^.type_ <> GDK_MOTION_NOTIFY) then
  begin
    if (Widget = TGtk3ComboBox(Data).GetPopupWidget) then
    DebugLn('***** TGtk3Widget.WidgetEvent(MENU triggered ',dbgsName(TGtk3Widget(Data).LCLObject),
      ' ',Gtk3EventToStr(event^.type_))
    else
    if (Widget = TGtk3ComboBox(Data).GetButtonWidget) then
    DebugLn('***** TGtk3Widget.WidgetEvent(BUTTON triggered ',dbgsName(TGtk3Widget(Data).LCLObject),
      ' ',Gtk3EventToStr(event^.type_))
    else
    if (Widget = PGtkWidget(TGtk3ComboBox(Data).GetCellView)) then
    DebugLn('***** TGtk3Widget.WidgetEvent(CELLVIEW triggered ',dbgsName(TGtk3Widget(Data).LCLObject),
      ' ',Gtk3EventToStr(event^.type_))
    else
    if (Widget = TGtk3ComboBox(Data).Widget) then
      DebugLn('***** TGtk3Widget.WidgetEvent(EVENTBOX triggered ',dbgsName(TGtk3Widget(Data).LCLObject),
        ' ',Gtk3EventToStr(event^.type_));
  end;
  {$ENDIF}
  {$IFDEF GTK3DEBUGCORE}
  // if event^.type_ = GDK_EXPOSE then
  if event^.type_ <> GDK_MOTION_NOTIFY then
  begin
    if TGtk3Widget(Data).LCLObject is TCustomForm then
    begin
      writeln('TGtk3Widget.WidgetEvent triggered ',dbgsName(TGtk3Widget(Data).LCLObject),
        ' ',Gtk3EventToStr(event^.type_),' ',GetTickCount64);
    end;
  end;
  {$ENDIF}
  Result := gtk_false;
  if Assigned(Application) and Application.Terminated then
      exit;
  case event^.type_ of
  GDK_DELETE:
    begin
      //DebugLn('****** GDK_DELETE FOR ',dbgsName(TGtk3Widget(Data).LCLObject),' main_level=',dbgs(gtk_main_level));
      if wtWindow in TGtk3Widget(Data).WidgetType then
      begin
        // check against wrong gtk3 behaviour about modal windows.
        if Assigned(TGtk3Window(Data).LCLObject) then
        begin
          AForm := TCustomForm(TGtk3Window(Data).LCLObject);
          if (Application.ModalLevel > 0) then
          begin
            if gtk_application_get_active_window(Gtk3WidgetSet.Gtk3Application) <> PGtkWindow(TGtk3Window(Data).Widget) then
              AForm := nil;
          end;
        end else
          AForm := nil;
        if (AForm <> nil) then
          TGtk3Window(Data).CloseQuery;
        // let lcl destroy widget
        Result := True;
      end;
    end;
  GDK_DESTROY:
    begin
     // DebugLn('****** GDK_DESTROY FOR ' + dbgsName(TGtk3Widget(Data).LCLObject));
    end;
  GDK_EXPOSE:
    begin
      DebugLn('****** GDK_EXPOSE FOR ' + dbgsName(TGtk3Widget(Data).LCLObject));
      // Gtk3DrawWidget is attached to 'draw' signal, Expose event doesn't trigger
      // under gtk3.
       // we use 'draw' signal Gtk3DrawEvent()
      // Result := TGtk3Widget(Data).GtkEventPaint(Widget, Event);
    end;
  GDK_MOTION_NOTIFY:
    begin
      if wtWindow in TGtk3Widget(Data).WidgetType then
      begin
        if Widget = TGtk3Widget(Data).Widget then
          exit;
      end;
      Result := TGtk3Widget(Data).GtkEventMouseMove(Widget, Event);
    end;
  GDK_BUTTON_PRESS:
    begin
      // set focus before gtk does that, so we have same behaviour as other ws
      if TGtk3Widget(Data).GetFocusableByMouse and
        not TGtk3Widget(Data).LCLObject.Focused and
        TGtk3Widget(Data).LCLObject.CanFocus then
      begin
        //FIXME: combobox updates popup-shown property too late
        // so we dont know yet if its dropped down or not
        if (wtComboBox in TGtk3Widget(Data).WidgetType) then
        begin
          TGtk3ComboBox(Data).DumpPrivateStructValues('GDK_BUTTON_PRESS btn='+IntToStr(Event^.button.button));
        end;
        if (wtWindow in TGtk3Widget(Data).WidgetType) then
        begin
          TGtk3Widget(Data).Activate;
        end else
          LCLIntf.SetFocus(HWND(TGtk3Widget(Data)));
      end;

      if TGtk3Widget(Data).LCLObject is TButtonControl then exit;

      Result:=TGtk3Widget(Data).GtkEventMouse(Widget , Event);
    end;
  GDK_2BUTTON_PRESS:
    begin
      // set focus before gtk does that, so we have same behaviour as other ws
      if TGtk3Widget(Data).GetFocusableByMouse and
        not TGtk3Widget(Data).LCLObject.Focused and
        TGtk3Widget(Data).LCLObject.CanFocus then
          LCLIntf.SetFocus(HWND(TGtk3Widget(Data)));

      if TGtk3Widget(Data).LCLObject is TButtonControl then exit;
      Result := TGtk3Widget(Data).GtkEventMouse(Widget , Event);
    end;
  GDK_3BUTTON_PRESS:
    begin
      // set focus before gtk does that, so we have same behaviour as other ws
      if TGtk3Widget(Data).GetFocusableByMouse and
        not TGtk3Widget(Data).LCLObject.Focused and
        TGtk3Widget(Data).LCLObject.CanFocus then
          LCLIntf.SetFocus(HWND(TGtk3Widget(Data)));

      if TGtk3Widget(Data).LCLObject is TButtonControl then exit;


      Result := TGtk3Widget(Data).GtkEventMouse(Widget , Event);
    end;
  GDK_BUTTON_RELEASE:
    begin
      {if not ((csClickEvents in TGtk3Widget(Data).LCLObject.ControlStyle) and
         (csClicked in TGtk3Widget(Data).LCLObject.ControlState)) then }

      if TGtk3Widget(Data).LCLObject is TButtonControl then exit;

      Result := TGtk3Widget(Data).GtkEventMouse(Widget , Event);
    end;
  GDK_KEY_PRESS:
    begin
      if Widget^.has_focus or Widget^.is_toplevel then
        Result := TGtk3Widget(Data).GtkEventKey(Widget, Event, True);
    end;
  GDK_KEY_RELEASE:
    begin
      if Widget^.has_focus or Widget^.is_toplevel then // or (Widget = TGtk3Widget(data).GetContainerWidget) then
        Result := TGtk3Widget(Data).GtkEventKey(Widget, Event, False);
    end;
  GDK_ENTER_NOTIFY:
    begin
    end;
  GDK_LEAVE_NOTIFY:
    begin
    end;
  GDK_FOCUS_CHANGE:
    begin
      if wtComboBox in TGtk3Widget(Data).WidgetType then
      begin
        TGtk3ComboBox(Data).DumpPrivateStructValues('GDK_FOCUS_CHANGE='+IntToStr(Event^.focus_change.in_));
        //FIXME: combobox updates popup-shown property too late
        // so we dont know yet if its dropped down or not
        if TGtk3ComboBox(Data).DroppedDown then
          exit;
      end;
      if not (csNoFocus in TWinControl(TGtk3Widget(Data).LCLObject).ControlStyle) then
        TGtk3Widget(Data).GtkEventFocus(Widget, Event);
    end;
  GDK_CONFIGURE:
    begin
      //DebugLn('****** GDK_CONFIGURE FOR ',dbgsName(TGtk3Widget(Data).LCLObject));
    end;
  GDK_MAP:
    begin
      //DebugLn('****** GDK_MAP FOR ',dbgsName(TGtk3Widget(Data).LCLObject));
    end;
  GDK_UNMAP:
    begin
       // DebugLn('****** GDK_UNMAP FOR ',dbgsName(TGtk3Widget(Data).LCLObject));
    end;
  GDK_PROPERTY_NOTIFY:
    begin
      // DebugLn('****** GDK_PROPERTY_NOTIFY FOR ',dbgsName(TGtk3Widget(Data).LCLObject));
    end;
  GDK_CLIENT_EVENT:
    begin
      // DebugLn('****** GDK_CLIENT_EVENT FOR ',dbgsName(TGtk3Widget(Data).LCLObject));
    end;
  GDK_VISIBILITY_NOTIFY:
    begin
      // ONLY HERE WE CAN CATCH Activate/Deactivate but problem is that
      // PGtkWindow does not update active property properly
      // so PGtkWindow(Widget)^.is_active returns TRUE even if window isn't active anymore
      if wtWindow in TGtk3Widget(Data).WidgetType then
        TGtk3Window(Data).ActivateWindow(Event);
      // Result := TGtk3Widget(Data).GtkEventShowHide(Widget, Event);
      // DebugLn('****** GDK_VISIBILITY_NOTIFY FOR ' + dbgsName(TGtk3Widget(Data).LCLObject));
    end;
  GDK_SCROLL:
    begin
      // DebugLn('****** GDK_SCROLL ' + dbgsName(TGtk3Widget(Data).LCLObject));
      // Result := TGtk3Widget(Data).GtkEventMouseWheel(Widget, Event);
    end;
  GDK_WINDOW_STATE:
    begin
      // DebugLn('****** GDK_WINDOW_STATE FOR ' + dbgsName(TGtk3Widget(Data).LCLObject));
      // this doesn't work as expected ... must use GDK_CONFIGURE to get active status ?!?
    end;
  GDK_GRAB_BROKEN:  //could be broken eg. because of popupmenu
    begin
      DebugLn('****** GDK_GRAB_BROKEN (no problem if popupmenu is activated) ' + dbgsName(TGtk3Widget(Data).LCLObject));
    end;
  otherwise
    DebugLn('****** GDK unhandled event type ' + dbgsName(TGtk3Widget(Data).LCLObject));
   // DebugLn(event^.type_);
  end;
end;

class function TGtk3Widget.DrawWidget(AWidget: PGtkWidget; AContext: Pcairo_t; Data: gpointer): gboolean; cdecl;
var
  ARect: TGdkRectangle;
begin
  Result := False;
  if Data <> nil then
  begin
    gdk_cairo_get_clip_rectangle(AContext, @ARect);
    Result := TGtk3Widget(Data).GtkEventPaint(AWidget, AContext);
  end;
end;

class procedure TGtk3Widget.MapWidget(AWidget: PGtkWidget; Data: gPointer); cdecl;
var
  Allocation: TGtkAllocation;
  ARect: TRect;
  AWindow: PGdkWindow;
  xx,yy,w,h: Gint;
begin
  AWidget^.get_allocation(@Allocation);
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('**** Gtk3MapWidget ....',dbgsName(TGtk3Widget(Data).LCLObject));
  with Allocation do
    DebugLn(' Allocation ',Format('x %d y %d w %d h %d',[x,y,width,height]));
  {$ENDIF}
  TGtk3Widget(Data).WidgetMapped := True;
  ARect := TGtk3Widget(Data).LCLObject.BoundsRect;
  {$IFDEF GTK3DEBUGCORE}
  with ARect do
    DebugLn(' Rect ',Format('x %d y %d w %d h %d',[Left,Top,Right - Left, Bottom - Top]));
  {$ENDIF}
  if ARect.Left<ARect.Right then ;

  AWindow := AWidget^.get_window;
  // at least TPanel needs this
  if Gtk3IsGdkWindow(AWindow) and (g_object_get_data(AWindow,'lclwidget') = nil) then
    g_object_set_data(AWindow,'lclwidget', TGtk3Widget(Data));
  if (AWindow <> nil) and AWidget^.get_has_window then
  begin
    // do resize to lcl size when mapping widget
    gdk_window_set_events(AWindow, GDK_DEFAULT_EVENTS_MASK);
  {  if not (wtWindow in TGtk3Widget(Data).WidgetType) then
    begin }
      with TGtk3Widget(Data).LCLObject do
      begin
        xx := Left;
        yy := Top;
        w := Width;
        h := Height;
      end;
      TGtk3Widget(Data).BeginUpdate;
      AWindow^.move(xx, yy);
      AWindow^.resize(w, h);
      TGtk3Widget(Data).EndUpdate;
  {  end else
    begin
      // DebugLn('TGtk3Window is mapped , setting lclwidget property to PGdkWindow ...');
      // now we set 'lclwidget' to our window.
      // g_object_set_data(AWindow,'lclwidget', TGtk3Widget(Data));
    end;}
  end else
  begin
    if wtMemo in TGtk3Widget(Data).WidgetType then
    begin
      // gdk_window_get_geometry(AWindow, @xx,@yy,@w,@h);
      // gdk_window_get_position(AWindow, @xx,@yy);
      // DebugLn(' ***** Window ',Format('x %d y %d w %d h %d',[xx,yy,w,h]),' lclobject ',dbgsName(TGtk3Widget(Data).LCLObject));
    end;
  end;
end;

function SubtractScroll(AWidget: PGtkWidget; APosition: TPoint): TPoint;
begin
  Result := APosition;
  if Gtk3IsScrolledWindow(AWidget) then
  begin
    with gtk_scrolled_window_get_hadjustment(PGtkScrolledWindow(AWidget))^ do
      dec(Result.x, Trunc(value - lower));
    with gtk_scrolled_window_get_vadjustment(PGtkScrolledWindow(AWidget))^ do
      dec(Result.y, Trunc(value - lower));
  end;
end;

class function TGtk3Widget.ScrollEvent(AWidget: PGtkWidget; AEvent: PGdkEvent; AData: GPointer): GBoolean; cdecl;
var
  AWinControl: TWinControl;
  AState: Cardinal;
  ShiftState: TShiftState;
  MappedXY: TPoint;
  MessE : TLMMouseEvent;
begin
  Result := False;
  if AWidget=nil then ;

  AWinControl := TGtk3Widget(AData).LCLObject;

  if AEvent^.scroll.send_event = NO_PROPAGATION_TO_PARENT then
    exit;

  MappedXY := Point(Round(AEvent^.Scroll.X),Round(AEvent^.scroll.Y));
  AState := GtkModifierStateToShiftState(AEvent^.scroll.state, False);
  ShiftState := [];
  if AState and MK_SHIFT <> 0 then
    ShiftState := ShiftState + [ssShift];
  if AState and MK_CONTROL <> 0 then
    ShiftState := ShiftState + [ssCtrl];
  if AState and MK_ALT <> 0 then
    ShiftState := ShiftState + [ssAlt];

  TGtk3Widget(AData).OffsetMousePos(AEvent^.scroll.x_root, AEvent^.scroll.y_root, @MappedXY);

  FillChar(MessE{%H-},SizeOf(MessE),0);
  MessE.Msg := LM_MOUSEWHEEL;
  case AEvent^.scroll.direction of
    GDK_SCROLL_UP, GDK_SCROLL_RIGHT {0}: MessE.WheelDelta := 120;
    GDK_SCROLL_DOWN, GDK_SCROLL_LEFT {1}: MessE.WheelDelta := -120;
    GDK_SCROLL_SMOOTH:
      begin
        if AEvent^.scroll.delta_y <> 0 then
        begin
          if AEvent^.scroll.delta_y > 0 then
            MessE.WheelDelta := -120
          else
            MessE.WheelDelta := 120;
          //TODO: find in settings default wheel scroll distance
          //MessE.WheelDelta := -Round((120 * AEvent^.scroll.delta_y) / 10);
        end else
        if AEvent^.scroll.delta_x <> 0 then
        begin
          if AEvent^.scroll.delta_x > 0 then
            MessE.WheelDelta := -120
          else
            MessE.WheelDelta := 120;
        end else
          exit;
      end;
  else
  begin
    DebugLn('WARNING: ',dbgsName(aWinControl),' unhandled scrollDirection event ',dbgs(Ord(AEvent^.scroll.direction)));
    exit;
  end;
  end;
  MessE.X := SmallInt(MappedXY.X);
  MessE.Y := SmallInt(MappedXY.Y);
  MessE.State := ShiftState;
  MessE.UserData := AWinControl;
  MessE.Button := 0;

  NotifyApplicationUserInput(AWinControl, PLMessage(@MessE)^);
  Result := TGtk3Widget(AData).DeliverMessage(MessE) <> 0;
  AEvent^.scroll.send_event := NO_PROPAGATION_TO_PARENT;

  //DebugLn('Gtk3ScrollEvent for ', dbgsName(TGtk3Widget(AData).LCLObject),' Result ',dbgs(Result));
end;

class procedure TGtk3Widget.SizeAllocate(AWidget: PGtkWidget; AGdkRect: PGdkRectangle; Data: gpointer); cdecl;
var
  Msg: TLMSize;
  NewSize: TSize;
  ACtl: TGtk3Widget;
  AState: TGdkWindowState;
  Alloc: TGtkAllocation;
begin
  if AWidget=nil then ;

  ACtl := TGtk3Widget(Data);

  {$IFDEF GTK3DEBUGSIZE}
  if Assigned(ACtl.LCLObject) then
  begin
    with ACtl.LCLObject do
      writeln(Format('TGtk3Widget.SizeAllocate %s Gdk x %d y %d w %d h %d  LCL l %d t %d w %d h %d applied w %d h %d cliRect %s',[dbgsName(ACtl.LCLObject), AGdkRect^.x, AGdkRect^.y, AGdkRect^.width, AGdkRect^.height, Left, Top, Width, Height, ACtl.LCLWidth, ACtl.LCLHeight, dbgs(ACtl.LCLObject.ClientRect)]));
  end;
  {$ENDIF}
  // return size w/o frame
  NewSize.cx := AGdkRect^.width;
  NewSize.cy := AGdkRect^.height;

  //writeln(format('Gkt3SizeAllocate w=%d h=%d',[NewSize.cx,NewSize.cy]));

  if not Assigned(ACtl.LCLObject) then exit;

  // do not loop with LCL  !
  if not (csDesigning in ACtl.LCLObject.ComponentState) then
  begin
    if ACtl.InUpdate then
      exit;
  end;

  FillChar(Msg{%H-}, SizeOf(Msg), #0);

  Msg.Msg := LM_SIZE;
  Msg.SizeType := SIZE_RESTORED;

  if ACtl is TGtk3Window then
  begin
    AState := TGtk3Window(ACtl).getWindowState;
    if GDK_WINDOW_STATE_ICONIFIED in AState  then
      Msg.SizeType := SIZE_MINIMIZED
    else
    if GDK_WINDOW_STATE_MAXIMIZED in AState  then
      Msg.SizeType := SIZE_MAXIMIZED
    else
    if GDK_WINDOW_STATE_FULLSCREEN in AState  then
      Msg.SizeType := SIZE_FULLSCREEN;

  end;

  Msg.SizeType := Msg.SizeType or Size_SourceIsInterface;

  if ACtl.WidgetType*[wtScrollBar, wtHintWindow]<>[] then
  begin
    Msg.Width := ACtl.LCLObject.Width;//Word(NewSize.cx);
    Msg.Height := ACtl.LCLObject.Height;//Word(NewSize.cy);
  end else
  if {ACtl is TGtk3Window} ACtl.WidgetType*[wtWindow,wtDialog,
     {wtScrollingWinControl,}wtScrollingWin,wtNotebook,wtContainer]<>[] then
  begin
    Msg.Width := Word(NewSize.cx);
    Msg.Height := Word(NewSize.cy);
  end else
  begin
    Msg.Width := ACtl.LCLObject.Width;//Word(NewSize.cx);
    Msg.Height := ACtl.LCLObject.Height;//Word(NewSize.cy);
  end;

  ACtl.DeliverMessage(Msg);

end;

class procedure TGtk3Widget.WidgetHide({%H-}AWidget: PGtkWidget; AData: gpointer); cdecl;
var
  Msg: TLMShowWindow;
  Gtk3Widget: TGtk3Widget;
begin
  Gtk3Widget := TGtk3Widget(AData);
  {do not pass message to LCL if LCL setted up control visibility}
  if Gtk3Widget.inUpdate then
    exit;
  // DebugLn('SEND LM_HIDE FOR ',dbgsName(Gtk3Widget.LCLObject));
  FillChar(Msg{%H-}, SizeOf(Msg), #0);

  Msg.Msg := LM_SHOWWINDOW;
  Msg.Show := False;

  Gtk3Widget.DeliverMessage(Msg);
end;

class procedure TGtk3Widget.WidgetShow({%H-}AWidget: PGtkWidget; AData: gpointer); cdecl;
var
  Msg: TLMShowWindow;
  Gtk3Widget: TGtk3Widget;
begin
  Gtk3Widget := TGtk3Widget(AData);
  {do not pass message to LCL if LCL setted up control visibility}
  if Gtk3Widget.inUpdate then
    exit;
  // DebugLn('SEND LM_SHOW FOR ',dbgsName(Gtk3Widget.LCLObject));
  FillChar(Msg{%H-}, SizeOf(Msg), #0);

  Msg.Msg := LM_SHOWWINDOW;
  Msg.Show := True;

  Gtk3Widget.DeliverMessage(Msg);
end;

{ TGtk3SplitterSide }

function TGtk3SplitterSide.CreateWidget(const Params: TCreateParams): PGtkWidget;
begin
  Result:=TGtkScrolledWindow.new(nil, nil);
end;

{ TGtk3Paned }

function TGtk3Paned.CreateWidget(const Params: TCreateParams): PGtkWidget;
const
  ornt:array[TPairSplitterType] of TGtkOrientation=(
    GTK_ORIENTATION_HORIZONTAL,
    GTK_ORIENTATION_VERTICAL
    );
begin
  Result:=TGtkPaned.new(ornt[TPairSplitter(Self.LCLObject).SplitterType]);
end;

{ TGtk3Widget }

function TGtk3Widget.GtkEventMouseMove(Sender: PGtkWidget; Event: PGdkEvent
  ): Boolean; cdecl;
var
  Msg: TLMMouseMove;
  MousePos: TPoint;
  ADisplay: PGdkDisplay;
  ASeat: PGdkSeat;
  ADevice: PGdkDevice;
  X, Y: gint;
  AMask: TGdkModifierType;
  {$IFDEF GTK3DEBUGEVENTS}
  R: TRect;
  {$ENDIF}
begin
  Result := False;

  {$IFDEF GTK3DEBUGEVENTS}
  R := GetClientBounds;
  DebugLn(['GtkEventMouseMove: ',dbgsName(LCLObject),' Send=',dbgs(Event^.motion.send_event),
  ' state=',dbgs(LongInt(event^.motion.state)),
  ' x=',dbgs(Round(event^.motion.x)),
  ' y=',dbgs(Round(event^.motion.y)),
  ' x_root=',dbgs(Round(event^.motion.x_root)),
  ' y_root=',dbgs(Round(event^.motion.y_root)),
  ' STOP PROCESSING ? ',dbgs(Event^.motion.send_event = NO_PROPAGATION_TO_PARENT),
  ' GtkBounds ',dbgs(R),' LCLBounds ',dbgs(LCLObject.BoundsRect),' W=',dbgs(LCLObject.Width)]
  );
  {$ENDIF}

  if Event^.motion.send_event = NO_PROPAGATION_TO_PARENT then
    exit;

  FillChar(Msg{%H-}, SizeOf(Msg), #0);

  //we use GDK_POINTER_MOTION_HINT_MASK, so we cannot trust Event^.motion position
  if Event^.motion.is_hint = 1 then
  begin
    ADisplay := gtk_widget_get_display(Sender);
    ASeat := gdk_display_get_default_seat(ADisplay);
    ADevice := gdk_seat_get_pointer(ASeat);
    gdk_window_get_device_position(Event^.motion.window, ADevice, @X, @Y, @AMask);
  end else
  begin
    X := Round(Event^.motion.x);
    Y := Round(Event^.motion.y);
    AMask := Event^.motion.state;
  end;

  MousePos.x := X;
  MousePos.y := Y;

  OffsetMousePos(Event^.motion.x_root, Event^.motion.y_root, @MousePos);

  Msg.XPos := SmallInt(MousePos.X);
  Msg.YPos := SmallInt(MousePos.Y);

  if Mouse.CursorPos=MousePos then exit;

  Msg.Keys := GdkModifierStateToLCL(aMask, False);

  Msg.Msg := LM_MOUSEMOVE;

  NotifyApplicationUserInput(LCLObject, PLMessage(@Msg)^);
  if Widget^.get_parent <> nil then
    Event^.motion.send_event := NO_PROPAGATION_TO_PARENT;
  DeliverMessage(Msg, True);
end;

function TGtk3Widget.GtkEventPaint(Sender: PGtkWidget; AContext: Pcairo_t
  ): Boolean; cdecl;
var
  Msg: TLMPaint;
  AStruct: TPaintStruct;
  AClipRect: TGdkRectangle;
  localClip:TRect;
  P: TPoint;
  AScrolledWin: PGtkScrolledWindow;
  ACaret: TGtk3Caret;
  HScrollPolicy, VScrollPolicy: TGtkPolicyType;
  {$IFDEF GTK3DEBUGDESIGNER}
  dx, dy: double;
  allocation: TGtkAllocation;
  {$ENDIF}
begin
  Result := False;

  if not FHasPaint then
    exit;

  if Self is TGtk3DesignWidget then
  begin
    //writeln('WARNING: DesignWidget should not be called here !');
    exit;
  end;

  FillChar(Msg{%H-}, SizeOf(Msg), #0);

  Msg.Msg := LM_PAINT;
  FillChar(AStruct{%H-}, SizeOf(TPaintStruct), 0);
  Msg.PaintStruct := @AStruct;

  with PaintData do
  begin
    PaintWidget := Sender;
    ClipRegion := nil;
    gdk_cairo_get_clip_rectangle(AContext, @AClipRect);

    {$IFDEF GTK3DEBUGEVENTS}
    if (Self is TGtk3ScrollableWin) and not (LCLObject is TCustomForm) then
    begin
      //cairo_get_current_point(AContext, @dx, @dy);
      cairo_user_to_device(AContext, @dx, @dy);
      writeln(Format('PaintEvent: CairoClip %s dx %2.2n dy %2.2n',[dbgs(RectFromGdkRect(AClipRect)), dx, dy]));
    end;
    {$ENDIF}

    localClip:=RectFromGdkRect(AClipRect);
    ClipRect := @localClip;
  end;

  FCairoContext := AContext;
  Msg.DC := BeginPaint(HWND(Self), AStruct);
  FContext := Msg.DC;

  Msg.PaintStruct^.rcPaint := PaintData.ClipRect^;
  Msg.PaintStruct^.hdc := FContext;

  try
    try
      if wtScrollingWinControl in WidgetType then
      begin
        P := getClientOffset;
        AScrolledWin := TGtk3ScrollingWinControl(Self).GetScrolledWindow;
        AScrolledWin^.get_policy(@HScrollPolicy, @VScrollPolicy);
        if HScrollPolicy < GTK_POLICY_NEVER then
          P.X := P.X + Round(AScrolledWin^.get_hadjustment^.get_value);
        if VScrollPolicy < GTK_POLICY_NEVER then
          P.Y := P.Y + Round(AScrolledWin^.get_vadjustment^.get_value);
        TGtk3DeviceContext(Msg.DC).ScrollbarsOffset := Point(P.X, P.Y);
        cairo_translate(AContext, -P.X, -P.Y);
        with TGtk3DeviceContext(Msg.DC).fncOrigin do
        begin
          X := X - P.X;
          Y := Y - P.Y;
        end;
      end;
      DoBeforeLCLPaint;
      LCLObject.WindowProc(TLMessage(Msg));
      if HasCaret and not (csDesigning in LCLObject.ComponentState) then
      begin
        ACaret := TGtk3Caret(g_object_get_data(Sender,'gtk3-caret'));
        if ACaret.Visible then
          ACaret.CairoDrawCaret(FCairoContext);
      end;
      if wtScrollingWinControl in WidgetType then
        cairo_translate(AContext, P.X, P.Y);
    finally
      FCairoContext := nil;
      Fillchar(FPaintData, SizeOf(FPaintData), 0);
      FContext := 0;
      EndPaint(HWND(Self), AStruct);
    end;
  except
    Application.HandleException(nil);
  end;
end;

procedure TGtk3Widget.GtkEventFocus(Sender: PGtkWidget; Event: PGdkEvent);
  cdecl;
var
  Msg: TLMessage;
  ACaret: TGtk3Caret;
begin
  {$IF DEFINED(GTK3DEBUGEVENTS) OR DEFINED(GTK3DEBUGFOCUS)}
  DebugLn('TGtk3Widget.GtkEventFocus ',dbgsName(LCLObject),' FocusIn ',dbgs(Event^.focus_change.in_ <> 0));
  {$ENDIF}
  FillChar(Msg{%H-}, SizeOf(Msg), #0);
  if Event^.focus_change.in_ <> 0 then
    Msg.Msg := LM_SETFOCUS
  else
    Msg.Msg := LM_KILLFOCUS;

  if HasCaret then
  begin
    ACaret := TGtk3Caret(g_object_get_data(PGObject(getContainerWidget),'gtk3-caret'));
    if ACaret.RespondToFocus then
    begin
      if Msg.Msg = LM_SETFOCUS then
        ACaret.Show
      else
        ACaret.Hide;
    end;
  end;

  DeliverMessage(Msg);
end;

procedure TGtk3Widget.GtkEventDestroy; cdecl;
var
  Msg: TLMessage;
begin
  FillChar(Msg{%H-}, SizeOf(Msg), #0);
  Msg.Msg := LM_DESTROY;
  DeliverMessage(Msg);
  Release;
end;

function TGtk3Widget.IsValidHandle: Boolean;
begin
  Result := Assigned(FWidget) and Gtk3IsWidget(FWidget) and not FWidget^.in_destruction;
end;

function TGtk3Widget.IsWidgetOk: Boolean;
begin
  Result := Gtk3IsWidget(FWidget);
end;

function TGtk3Widget.IsIconic: Boolean;
begin
  Result := False;
  if IsWidgetOk then
  begin
    if FWidget^.get_window <> nil then
      Result := GDK_WINDOW_STATE_ICONIFIED in gdk_window_get_state(FWidget^.get_window);
  end;
end;

function TGtk3Widget.getType: TGType;
begin
  Result := getContainerWidget^.g_type_instance.g_class^.g_type;
end;

function TGtk3Widget.getTypeName: PgChar;
begin
  Result := g_type_name(getType);
end;

procedure TGtk3Widget.lowerWidget;
begin
  if Gtk3IsGdkWindow(FWidget^.window) then
    FWidget^.window^.lower;
end;

procedure TGtk3Widget.raiseWidget;
begin
  if Gtk3IsGdkWindow(FWidget^.window) then
    FWidget^.window^.raise_;
end;

procedure TGtk3Widget.stackUnder(AWidget: PGtkWidget);
begin
  // FWidget^.
end;

function TGtk3Widget.GetCapture: TGtk3Widget;
var
  AHandle: HWND;
begin
  AHandle := HwndFromGtkWidget(gtk_grab_get_current);
  if AHandle <> 0 then
    Result := TGtk3Widget(AHandle);
end;

function TGtk3Widget.SetCapture: HWND;
begin
  Result := HWND(GetCapture);
  gtk_grab_add(GetContainerWidget);
end;

function TGtk3Widget.GtkEventKey(Sender: PGtkWidget; Event: PGdkEvent; AKeyPress: Boolean): Boolean;
  cdecl;
const
  CN_KeyDownMsgs: array[Boolean] of UINT = (CN_KEYDOWN, CN_SYSKEYDOWN);
  CN_KeyUpMsgs: array[Boolean] of UINT = (CN_KEYUP, CN_SYSKEYUP);
  LM_KeyDownMsgs: array[Boolean] of UINT = (LM_KEYDOWN, LM_SYSKEYDOWN);
  LM_KeyUpMsgs: array[Boolean] of UINT = (LM_KEYUP, LM_SYSKEYUP);
  CN_CharMsg: array[Boolean] of UINT = (CN_CHAR, CN_SYSCHAR);
  LM_CharMsg: array[Boolean] of UINT = (LM_CHAR, LM_SYSCHAR);
var
  AEvent: TGdkEventKey;
  Msg: TLMKey;
  CharMsg: TLMChar;
  AEventString: String;
  KeyValue, ACharCode: Word;
  LCLModifiers: Word;
  IsSysKey: Boolean;
  UTF8Char: TUTF8Char;
  AChar: Char;
  IsArrowKey: Boolean;
  TempWidget: HWND;
  {$IFDEF GTK3DEBUGKEYPRESS}
  Info: PTypeInfo;
  {$ENDIF}
begin
  //TODO: finish LCL messaging
  Result := False;
  AEvent := Event^.key;
  FillChar(Msg{%H-}, SizeOf(Msg), 0);
  AEventString := AEvent.string_;

  TempWidget := HwndFromGtkWidget(Sender);
  {$IFDEF GTK3DEBUGKEYPRESS}
  if TempWidget = 0 then
    writeln('***** warning: no gtk3widget ! *****')
  else
     writeln('GtkEventKey: Gtk3Widget ',dbgsName(TGtk3Widget(TempWidget)));
  {$ENDIF}

  if gdk_keyval_is_lower(AEvent.keyval) then
    KeyValue := Word(gdk_keyval_to_upper(AEvent.keyval))
  else
    KeyValue := Word(AEvent.keyval);
  // state=16 = numlock= on.

  LCLModifiers := GtkModifierStateToShiftState(AEvent.state, True);


  if length(AEventString) = 0 then
  begin
    if KeyValue = GDK_KEY_Alt_L then
      LCLModifiers := LCLModifiers or KF_ALTDOWN
    else
    if (KeyValue = GDK_KEY_Control_L) or (KeyValue = GDK_KEY_Control_R)  then
      LCLModifiers := LCLModifiers or MK_CONTROL
    else
    if (KeyValue = GDK_KEY_Shift_L) or (KeyValue = GDK_KEY_Shift_R) then
      LCLModifiers := LCLModifiers or MK_SHIFT;
    // writeln('MODIFIERS BY KEYS ',LCLModifiers);
  end;

  IsSysKey := LCLModifiers and KF_ALTDOWN <> 0;

  if not AKeyPress then
    LCLModifiers := LCLModifiers or KF_UP;

  // else
  //  writeln('KeyRelease: ',dbgsName(LCLObject),' Dump state=',AEvent.state,' hwkey=',KeyCode,' keyvalue=',KeyValue,' modifier=',AEvent.Bitfield0.is_modifier);

  // this is just for testing purposes.
  ACharCode := GdkKeyToLCLKey(KeyValue);

  {$IFDEF GTK3DEBUGKEYPRESS}
  writeln('==== ACharCode=',ACharCode,' KeyValue=',KeyValue);
  {$ENDIF}

  if KeyValue > VK_UNDEFINED then
    KeyValue := ACharCode; // VK_UNKNOWN;

  IsArrowKey := (AEventString='') and ((ACharCode = VK_UP) or (ACharCode = VK_DOWN) or (ACharCode = VK_LEFT) or (ACharCode = VK_RIGHT));

  {$IFDEF GTK3DEBUGKEYPRESS}
  Info := TypeInfo(TGdkModifierType);
  if AKeyPress then
    writeln('EVENT KeyPress: ',dbgsName(LCLObject),' Dump state=',SetToString(Info, LongInt(AEvent.state), True),' keyvalue=',KeyValue,' modifier=',AEvent.Bitfield0.is_modifier,
    ' KeyValue ',KeyValue,' MODIFIERS ',LCLModifiers,' CharCode ',ACharCode,' EAT ',EatArrowKeys(ACharCode),' Window ? ',Sender^.window = GetWindow)
  else
    writeln('EVENT KeyRelease: ',dbgsName(LCLObject),' Dump state=',SetToString(Info, LongInt(AEvent.state), True),' keyvalue=',KeyValue,' modifier=',AEvent.Bitfield0.is_modifier,
    ' KeyValue ',KeyValue,' MODIFIERS ',LCLModifiers,' CharCode ',ACharCode,
    ' EAT ',EatArrowKeys(ACharCode));
  {$ENDIF}

  if (ACharCode <> VK_UNKNOWN) then
  begin
    if AKeyPress then
      Msg.Msg := CN_KeyDownMsgs[IsSysKey]
    else
      Msg.Msg := CN_KeyUpMsgs[IsSysKey];
    Msg.CharCode := ACharCode;
    Msg.KeyData := PtrInt((KeyValue shl 16) or (LCLModifiers shl 16) or $0001);

    NotifyApplicationUserInput(LCLObject, PLMessage(@Msg)^);

    if not CanSendLCLMessage then
      exit;

    if (DeliverMessage(Msg, True) <> 0) or (Msg.CharCode = VK_UNKNOWN) then
    begin
      {$IFDEF GTK3DEBUGKEYPRESS}
      writeln('<==== CN_KeyDownMsgs handled ... exiting');
      {$ENDIF}
      if ([wtEntry,wtMemo] * WidgetType <>[]) then
        exit(false)
      else
        exit(True);
    end;

    if not CanSendLCLMessage then
      exit;

    if AKeyPress then
      Msg.Msg := LM_KeyDownMsgs[IsSysKey]
    else
      Msg.Msg := LM_KeyUpMsgs[IsSysKey];
    Msg.CharCode := ACharCode;
    Msg.KeyData := PtrInt((KeyValue shl 16) or (LCLModifiers shl 16) or $0001);

    NotifyApplicationUserInput(LCLObject, PLMessage(@Msg)^);

    if not CanSendLCLMessage then
      exit;

    {$warning workaround for GtkTreeView key bindings.Must find out what LCL does with
     this keys.}
    if {IsArrowKey and} ([wtListBox,wtListView,wtEntry,wtMemo,wtComboBox] * WidgetType <> []) then
    // let gtk3 select cell for now. Must check what LCL does with arrow keys
    // since gtk3 becomes crazy after delivery of this message
    else
    if (DeliverMessage(Msg, True) <> 0) or (Msg.CharCode = 0) then
    begin
      Result := (Msg.CharCode = 0) or IsArrowKey;
      {$IFDEF GTK3DEBUGKEYPRESS}
      writeln('<=== LM_KeyDownMsgs handled ... exiting ',dbgs(ACharCode),' Result=',dbgs(Result),' AKeyPress=',dbgs(AKeyPress));
      {$ENDIF}
      exit;
    end;

    if not CanSendLCLMessage then
      exit;
  end;

  if AKeyPress and (length(AEventString) > 0) then
  begin
    UTF8Char := AEventString;
    // TODO: If not IsControlKey
    Result := LCLObject.IntfUTF8KeyPress(UTF8Char, 1, IsSysKey);

    if not CanSendLCLMessage then
      exit;

    if Result then
    begin
      {$IFDEF GTK3DEBUGKEYPRESS}
      writeln('LCLObject.IntfUTF8KeyPress handled ... exiting');
      {$ENDIF}
      exit;
    end;

    // create the CN_CHAR / CN_SYSCHAR message
    FillChar(CharMsg{%H-}, SizeOf(CharMsg), 0);
    CharMsg.Msg := CN_CharMsg[IsSysKey];
    CharMsg.KeyData := Msg.KeyData;
    AChar := AEventString[1];
    CharMsg.CharCode := Word(AChar);
    NotifyApplicationUserInput(LCLObject, PLMessage(@CharMsg)^);

    if not CanSendLCLMessage then
      exit;

    Result := (DeliverMessage(CharMsg, True) <> 0) or (CharMsg.CharCode = VK_UNKNOWN) or IsArrowKey;

    if not CanSendLCLMessage then
      exit;

    if Result then
    begin
      {$IFDEF GTK3DEBUGKEYPRESS}
      writeln('<=== CN_CharMsg handled ... exiting');
      {$ENDIF}
      exit;
    end;

    //Send a LM_(SYS)CHAR
    CharMsg.Msg := LM_CharMsg[IsSysKey];

    NotifyApplicationUserInput(LCLObject, PLMessage(@CharMsg)^);

    if not CanSendLCLMessage then
      exit;

    DeliverMessage(CharMsg, True);

    if not CanSendLCLMessage then
      exit;
  end;
  if AKeyPress then
  begin
    {$IFDEF GTK3DEBUGKEYPRESS}
    if (Msg.CharCode in FKeysToEat) then
    begin
      writeln('EVENT: ******* KeyPress charcode is in keys to eat (FKeysToEat), charcode=',dbgs(Msg.CharCode),' window ? ',Sender^.window = Self.GetWindow);
    end else
      writeln('EVENT: KeyPress Result = False Window ? ', Sender^.window = Self.GetWindow);
    {$ENDIF}
    Result := gtk_false;
    // (TempWidget = GetFocus) and (Msg.CharCode in FKeysToEat);
  end;
end;

function TGtk3Widget.GtkEventMouse(Sender: PGtkWidget; Event: PGdkEvent): Boolean;
  cdecl;
var
  Msg: TLMMouse;
  MsgPopup : TLMMouse;
  MousePos: TPoint;
  MButton: guint;
  SavedHandle: PtrUInt;
begin
  Result := gtk_false;
  {$IF DEFINED(GTK3DEBUGEVENTS) OR DEFINED(GTK3DEBUGMOUSE)}
  writeLn('TGtk3Widget.GtkEventMouse ',dbgsName(LCLObject),
    ' propagate=',dbgs(not (Event^.button.send_event = NO_PROPAGATION_TO_PARENT)),' Exit ? ',Event^.button.send_event = NO_PROPAGATION_TO_PARENT,
    ' Event.Type=',Event^.type_,' Capture=',LCLintf.GetCapture);
  {$ENDIF}
  if Event^.button.send_event = NO_PROPAGATION_TO_PARENT then
    exit(gtk_true);

  SavedHandle := PtrUInt(Self);

  FillChar(Msg{%H-}, SizeOf(Msg), #0);

  Msg.Keys := GdkModifierStateToLCL(Event^.button.state, False);

  MousePos.x := Round(Event^.button.x);
  MousePos.y := Round(Event^.button.y);

  OffsetMousePos(Event^.button.x_root, Event^.button.y_root, @MousePos);

  Msg.XPos := SmallInt(MousePos.X);
  Msg.YPos := SmallInt(MousePos.Y);

  MButton := Event^.button.button;

  case Event^.type_ of
    GDK_BUTTON_PRESS:
      begin
        if MButton = GTK3_LEFT_BUTTON then
        begin
          Msg.Msg := LM_LBUTTONDOWN;
          Msg.Keys := Msg.Keys or MK_LBUTTON;
        end
        else
        if MButton = GTK3_RIGHT_BUTTON then
        begin
          Msg.Msg := LM_RBUTTONDOWN;
          Msg.Keys := Msg.Keys or MK_RBUTTON;
        end
        else
        if MButton = GTK3_MIDDLE_BUTTON then
        begin
          Msg.Msg := LM_MBUTTONDOWN;
          Msg.Keys := Msg.Keys or MK_MBUTTON;
        end;
      end;
    GDK_2BUTTON_PRESS: //-> double click GDK_DOUBLE_BUTTON_PRESS
      begin
        if MButton = GTK3_LEFT_BUTTON then
        begin
          Msg.Msg := LM_LBUTTONDBLCLK;
          Msg.Keys := Msg.Keys or MK_LBUTTON;
        end
        else
        if MButton = GTK3_RIGHT_BUTTON then
        begin
          Msg.Msg := LM_RBUTTONDBLCLK;
          Msg.Keys := Msg.Keys or MK_RBUTTON;
        end
        else
        if MButton = GTK3_MIDDLE_BUTTON then
        begin
          Msg.Msg := LM_MBUTTONDBLCLK;
          Msg.Keys := Msg.Keys or MK_MBUTTON;
        end;
      end;
    GDK_BUTTON_RELEASE:
      begin
        if MButton = GTK3_LEFT_BUTTON then
        begin
          Msg.Msg := LM_LBUTTONUP;
          Msg.Keys := Msg.Keys or MK_LBUTTON;
        end
        else
        if MButton = GTK3_RIGHT_BUTTON then
        begin
          Msg.Msg := LM_RBUTTONUP;
          Msg.Keys := Msg.Keys or MK_RBUTTON;
        end
        else
        if MButton = GTK3_MIDDLE_BUTTON then
        begin
          Msg.Msg := LM_MBUTTONUP;
          Msg.Keys := Msg.Keys or MK_MBUTTON;
        end;
      end;
    else
      DebugLn(Format('WARNING: GtkEventMouse() unhandled event type %d.',[Ord(Event^.type_)]))
  end;

  {$IF DEFINED(GTK3DEBUGEVENTS) OR DEFINED(GTK3DEBUGMOUSE)}
  DebugLn('TGtk3Widget.GtkEventMouse ',dbgsName(LCLObject),
    ' msg=',dbgs(msg.Msg), ' point=',dbgs(Msg.XPos),',',dbgs(Msg.YPos));
  {$ENDIF}
  NotifyApplicationUserInput(LCLObject, PLMessage(@Msg)^);
  Event^.button.send_event := NO_PROPAGATION_TO_PARENT;

  if (SavedHandle <> PtrUInt(Self)) or (LCLObject = nil) or (FWidget = nil) then
    exit;
  Result := DeliverMessage(Msg, True) <> 0;

  if not Result and (Msg.Msg = LM_RBUTTONDOWN) then
  begin
    MsgPopup := Msg;
    MsgPopup.Msg := LM_CONTEXTMENU;
    MsgPopup.XPos := SmallInt(Round(Event^.button.x_root));
    MsgPopup.YPos := SmallInt(Round(Event^.button.y_root));

    if (SavedHandle <> PtrUInt(Self)) or (LCLObject = nil) or (FWidget = nil) then
      exit;

    Result := DeliverMessage(MsgPopup, True) <> 0;
  end;

  if wtPanel in WidgetType then
    Result := GDK_EVENT_STOP;
end;

function TGtk3Widget.GetVisible: Boolean;
begin
  Result := Assigned(FWidget) and FWidget^.visible;
end;

procedure TGtk3Widget.SetEnabled(AValue: Boolean);
begin
  if IsWidgetOK then
    FWidget^.set_sensitive(AValue);
end;

procedure TGtk3Widget.SetFont(AValue: PPangoFontDescription);
var
  NewFont: PPangoFontDescription;
begin
  if IsWidgetOk then
  begin
    GetContainerWidget^.override_font(AValue);
    NewFont := pango_font_description_copy(AValue); //keep description, otherwise we can easily crash in some circumstances
    if Assigned(FFont) then
      FFont^.free;
    FFont := NewFont;
  end;
end;

procedure TGtk3Widget.SetShape(AValue: PGdkPixbuf);
begin
  if FShape=AValue then Exit;
  if FShape <> nil then
    FShape^.unref;
  FShape := AValue;
end;

procedure TGtk3Widget.SetFontColor(AValue: TColor);
var
  AColor: TGdkRGBA;
  i: TGtkStateType;
begin
  if IsWidgetOK then
  begin
    AColor := TColortoTGdkRGBA(ColorToRgb(AValue));
    if FWidget <> GetContainerWidget then
    begin
      with FWidget^ do
      begin
        for i := GTK_STATE_NORMAL to GTK_STATE_INSENSITIVE do
          override_color(TGtkStateFlags(1 shl (i - 1)), @AColor);
      end;
    end;
    with GetContainerWidget^ do
    begin
      for i := GTK_STATE_NORMAL to GTK_STATE_INSENSITIVE do
        override_color(TGtkStateFlags(1 shl (i - 1)), @AColor);
    end;
  end;
end;

procedure TGtk3Widget.SetColor(AValue: TColor);
var
  AColor: TGdkRGBA;
  i: TGtkStateType;
begin
  if IsWidgetOK then
  begin
    AColor := TColortoTGdkRGBA(ColorToRgb(AValue));
    if FWidget <> GetContainerWidget then
    begin
      with FWidget^ do
      begin
        for i := GTK_STATE_NORMAL to GTK_STATE_INSENSITIVE do
          if AValue = clDefault then
            override_background_color(TGtkStateFlags(1 shl (i - 1)), nil)
          else
            override_background_color(TGtkStateFlags(1 shl (i - 1)), @AColor);
      end;
    end;
    with GetContainerWidget^ do
    begin
      for i := GTK_STATE_NORMAL to GTK_STATE_INSENSITIVE do
      begin
        if AValue = clDefault then
          override_background_color(TGtkStateFlags(1 shl (i - 1)), nil)
        else
          override_background_color(TGtkStateFlags(1 shl (i - 1)), @AColor);
      end;
    end;
  end;
end;

function TGtk3Widget.GetStyleContext: PGtkStyleContext;
begin
  Result := nil;
  if IsWidgetOK then
    Result := GetContainerWidget^.get_style_context;
end;

function TGtk3Widget.GetFont: PPangoFontDescription;
var
  AContext: PPangoContext;
begin
  Result := nil;
  if IsWidgetOK then
  begin
    AContext := GetContainerWidget^.get_pango_context;
    Result := pango_context_get_font_description(AContext);
  end;
end;

function TGtk3Widget.CanSendLCLMessage: Boolean;
begin
  Result := IsWidgetOk and (LCLObject <> nil);
end;

function TGtk3Widget.GetCairoContext: Pcairo_t;
begin
  Result := FCairoContext;
end;

function TGtk3Widget.GetEnabled: Boolean;
begin
  Result := False;
  if IsWidgetOK then
    Result := FWidget^.get_sensitive;
end;

function TGtk3Widget.GetFontColor: TColor;
var
  AStyle: PGtkStyleContext;
  AGdkRGBA: TGdkRGBA;
begin
  Result := clDefault;
  if IsWidgetOK then
  begin
    AStyle := GetStyleContext;
    AStyle^.get_background_color(GTK_STATE_FLAG_NORMAL, @AGdkRGBA);
    Result := TGdkRGBAToTColor(AGdkRGBA);
  end;
end;

function TGtk3Widget.GetColor: TColor;
var
  AStyle: PGtkStyleContext;
  AColor: TGdkRGBA;
begin
  Result := clDefault;
  if IsWidgetOK then
  begin
    AStyle := GetStyleContext;
    AStyle^.get_background_color(GTK_STATE_FLAG_NORMAL, @AColor);
    Result := TGdkRGBAToTColor(AColor);
  end;
end;

procedure TGtk3Widget.SetStyleContext(AValue: PGtkStyleContext);
begin
  {$NOTE Gtk3: Find a nice way to assign StyleContext}
  {if IsWidgetOK then
    GetContainerWidget^.set_style(AValue);}
end;

class procedure TGtk3Widget.DestroyWidgetEvent(w: PGtkWidget; data: gpointer); cdecl;
begin
  {$IFDEF GTK3DEBUGCORE}
  writeln('DestroyWidgetEvent entered ',Assigned(w),' Data ? ',Assigned(data));
  {$ENDIF}
  if Assigned(data) then
    TGtk3Widget(Data).FWidget:=nil;
end;

function TGtk3Widget.getText: String;
begin
  Result := fText; // default text storage
end;

procedure TGtk3Widget.setText(const AValue: String);
begin
  fText:=AValue;
  // DebugLn('WARNING: ',dbgsName(LCLObject),' self=',dbgsName(Self),' does not implement setText !');
end;

procedure TGtk3Widget.SetVisible(AValue: Boolean);
begin
  if IsWidgetOK then
    FWidget^.Visible := AValue;
end;

function TGtk3Widget.QueryInterface(constref iid: TGuid; out obj): LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  if GetInterface(iid, obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TGtk3Widget._AddRef: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := -1; // no ref counting
end;

function TGtk3Widget._Release: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := -1;
end;

function TGtk3Widget.IsDesigning: boolean;
begin
  Result := Assigned(LCLObject) and (csDesigning in LCLObject.ComponentState)
    and not (Self is TGtk3DesignWidget);
end;

function TGtk3Widget.EatArrowKeys(const AKey: Word): Boolean;
begin
  Result := AKey in [VK_LEFT, VK_UP, VK_RIGHT, VK_DOWN];
end;

function TGtk3Widget.GetContext: HDC;
begin
  Result := FContext;
end;

function TGtk3Widget.CreateWidget(const Params: TCreateParams): PGtkWidget;
begin
  Result := PGtkWidget(TGtkWidget.newv(32, 0 ,nil));
end;

function TGtk3Widget.GetWidget:PGtkWidget;
begin
  Result := FWidget;
end;

procedure TGtk3Widget.DestroyWidget;
var
  ATemp: PGtkWidget;
begin
  if HasCaret and IsValidHandle then
    GTK3WidgetSet.DestroyCaret(HWND(Self));

  if IsValidHandle and FOwnWidget then
  begin
    FOwnWidget:=false;
    {$IFDEF GTK3DEBUGCORE}
    DbgOut(#10'destroying '+Classname+' ... ');
    {$ENDIF}
    ATemp := FWidget;
    FWidget := nil;
    ATemp^.destroy_;
    {$IFDEF GTK3DEBUGCORE}
    DbgOut(Classname+' destroyed.'+#10);
    {$ENDIF}
  end;
  FWidget := nil;
  if Assigned(FShape) then
  begin
    FShape^.unref;
    FShape := nil;
  end;
  if Assigned(FFont) then
  begin
    FFont^.free;
    FFont := nil;
  end;
end;

procedure TGtk3Widget.DoBeforeLCLPaint;
begin
  //
end;

constructor TGtk3Widget.Create(const AWinControl: TWinControl;
  const AParams: TCreateParams);
begin
  inherited Create;
  LCLWidth := 0;
  LCLHeight := 0;
  FContext := 0;
  FWidgetMapped := False;
  FHasPaint := False;
  FWidget := nil;
  FOwner := nil;
  FCentralWidget := nil;
  FOwnWidget := True;
  // Initializes the properties
  FProps := nil;
  LCLObject := AWinControl;
  FKeysToEat := [VK_TAB, VK_RETURN, VK_ESCAPE];
  // FHasPaint := False;

  FParams := AParams;
  InitializeWidget;
end;

constructor TGtk3Widget.CreateFrom(const AWinControl: TWinControl;
  AWidget: PGtkWidget);
begin
  inherited Create;
  FContext := 0;
  FWidgetMapped := False;
  FHasPaint := False;
  FWidget := nil;
  FOwner := nil;
  FCentralWidget := nil;
  FOwnWidget := False;
  // Initializes the properties
  FProps := nil;
  LCLObject := AWinControl;
  FWidget := AWidget;
  // FKeysToEat := [VK_TAB, VK_RETURN, VK_ESCAPE];
  // FHasPaint := False;
end;

class function TGtk3Widget.MouseEnterNotify(aWidget: PGtkWidget; aEvent: PGdkEventCrossing; aData: gpointer): gboolean; cdecl;
var
  Msg: TLMessage;
begin
  Result := gtk_false;
  Msg.Msg := LM_MOUSEENTER;
  TGtk3Widget(aData).DeliverMessage(Msg, True);
  if Assigned(TGtk3Widget(aData).LCLObject) and (csDesigning in TGtk3Widget(aData).LCLObject.ComponentState) then
    Result := gtk_true;
end;

class function TGtk3Widget.MouseLeaveNotify(aWidget: PGtkWidget; aEvent: PGdkEventCrossing; aData: gpointer): gboolean; cdecl;
var
  Msg: TLMessage;
begin
  Result := gtk_false;
  Msg.Msg := LM_MOUSELEAVE;

  if Gtk3IsLayout(aWidget) and (aWidget^.get_parent = TGtk3Widget(aData).Widget) then
    exit;
  TGtk3Widget(aData).DeliverMessage(Msg, True);
  if Assigned(TGtk3Widget(aData).LCLObject) and (csDesigning in TGtk3Widget(aData).LCLObject.ComponentState) then
    Result := gtk_true;
end;

class procedure TGtk3Widget.DragDataReceived(aWidget:PGtkWidget; aContext: PGdkDragContext;
  x:gint; y:gint; selection_data: PGtkSelectionData; info:guint; time:guint; aData: gPointer);cdecl;
var
  S: TStringList;
  I: Integer;
  FileName, DecodedFileName: String;
  Files: array of String;
  Form: TControl;
  Result: Boolean;
  U: TURI;
begin
  Result := gtk_false;
  if selection_data^.get_data <> nil then // data is list of uri
  try
    SetLength(Files{%H-}, 0);
    S := TStringList.Create;
    try
      S.Text := PChar(selection_data^.get_data);

      for i := 0 to S.Count - 1 do
      begin
        FileName := S[I];

        if FileName = '' then Continue;
        // uri = protocol://hostname/file name
        U := ParseURI(FileName);
        if (SameText(U.Host, 'localhost') or (U.Host = '')) and SameText(U.Protocol, 'file')
          and URIToFileName(FileName, DecodedFileName) then // convert uri of local files to file name
        begin
          FileName := DecodedFileName;
        end;
        // otherwise: protocol and hostname are preserved!

        if FileName = '' then Continue;
        SetLength(Files, Length(Files) + 1);
        Files[High(Files)] := FileName;
        //DebugLn('GtkDragDataReceived ' + DbgS(I) + ': ' + PChar(FileName));
      end;
    finally
      S.Free;
    end;

    if Length(Files) > 0 then
    begin
      Form := nil;
      if (TObject(TGtk3Widget(aData).LCLObject) is TWinControl) then
        Form := TWinControl(TGtk3Widget(aData).LCLObject).IntfGetDropFilesTarget;

      if Form is TCustomForm then
        TCustomForm(Form).IntfDropFiles(Files)
      else
        if (Application <> nil) and (Application.MainForm <> nil) then
          Application.MainForm.IntfDropFiles(Files);

      if Application <> nil then
        Application.IntfDropFiles(Files);

      Result := gtk_true;
    end;
  except
    Application.HandleException(nil);
  end;

  gtk_drag_finish(aContext, Result, false, time);
end;

class function TGtk3Widget.MoveTabFocus(aWidget: PGtkWidget;
  aDirection: TGtkDirectionType; aData: gPointer): gBoolean; cdecl;
var
  aForm: TCustomForm;
begin
  Result := gtk_true;  // we are usually toplevel here
  aForm := GetParentForm(TGtk3Widget(aData).LCLObject);
  if Assigned(aForm.ActiveControl) then
    aForm.ActiveControl.PerformTab(aDirection = GTK_DIR_TAB_FORWARD);
end;

procedure TGtk3Widget.InitializeWidget;
var
  ARect: TGdkRectangle;
  ARgba: TGdkRGBA;
  i: TGtkStateType;
begin
  FHasCaret := False;
  FFocusableByMouse := False;
  FCentralWidget := nil;
  FCairoContext := nil;
  FContext := 0;
  FEnterLeaveTime := 0;
  FShape := nil;

  FWidgetType := [wtWidget];
  FWidget := CreateWidget(FParams);

  if not (wtWindow in FWidgetType) then
  begin
    FWidget^.show_all;
    with ARect do
    begin
      x := LCLObject.Left;
      y := LCLObject.Top;
      width := LCLObject.Width;
      height := LCLObject.Height;
    end;
    FWidget^.set_allocation(@ARect);
  end;
  LCLIntf.SetProp(HWND(Self),'lclwidget',Self);

  // connect events
  // move signal connections into attach events
  if not gtk_widget_get_realized(FWidget) then
    FWidget^.set_events(GDK_DEFAULT_EVENTS_MASK);
  g_signal_connect_data(FWidget, 'destroy', TGCallback(@DestroyWidgetEvent), Self, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(FWidget, 'event', TGCallback(@WidgetEvent), Self, nil, G_CONNECT_DEFAULT);

  g_signal_connect_data(GetContainerWidget, 'enter-notify-event', TGCallback(@MouseEnterNotify), Self, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(getContainerWidget, 'leave-notify-event', TGCallback(@MouseLeaveNotify), Self, nil, G_CONNECT_DEFAULT);

  g_signal_connect_data(FWidget,'drag_data_received',TGCallback(@DragDataReceived), Self, nil, G_CONNECT_DEFAULT);

  g_signal_connect_data(FWidget, 'focus', TGCallback(@MoveTabFocus), Self, nil, G_CONNECT_DEFAULT);
  if FWidget <> GetContainerWidget then
    g_signal_connect_data(GetContainerWidget, 'focus', TGCallback(@MoveTabFocus), Self, nil, G_CONNECT_DEFAULT);

  for i := GTK_STATE_NORMAL to GTK_STATE_INSENSITIVE do
  begin
    FWidget^.get_style_context^.get_background_color(TGtkStateFlags(1 shl (i - 1)), @ARgba);
    with FWidgetRGBA[i] do
    begin
      R := ARgba.red;
      G := ARgba.green;
      B := ARgba.blue;
      Alpha := ARgba.alpha;
    end;
  end;

  if (FCentralWidget <> nil) and (FCentralWidget <> FWidget) then
  begin
    FCentralWidget^.set_events(GDK_DEFAULT_EVENTS_MASK);
    g_signal_connect_data(FCentralWidget, 'event', TGCallback(@WidgetEvent), Self, nil, G_CONNECT_DEFAULT);
    for i := GTK_STATE_NORMAL to GTK_STATE_INSENSITIVE do
    begin
      FCentralWidget^.get_style_context^.get_background_color(TGtkStateFlags(1 shl (i - 1)), @ARgba);
      with FCentralWidgetRGBA[i] do
      begin
        R := ARgba.red;
        G := ARgba.green;
        B := ARgba.blue;
        Alpha := ARgba.alpha;
      end;
    end;
  end else
  begin
    for i := GTK_STATE_NORMAL to GTK_STATE_INSENSITIVE do
      FCentralWidgetRGBA[i] := FWidgetRGBA[i];
  end;
  FDrawSignal := g_signal_connect_data(GetContainerWidget,'draw', TGCallback(@DrawWidget), Self, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(GetContainerWidget,'scroll-event', TGCallback(@ScrollEvent), Self, nil, G_CONNECT_DEFAULT);

  // must hide all by default ???
  //FWidget^.hide;

  g_signal_connect_data(FWidget,'hide', TGCallback(@WidgetHide), Self, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(FWidget,'show', TGCallback(@WidgetShow), Self, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(FWidget,'map', TGCallback(@MapWidget), Self, nil, G_CONNECT_DEFAULT);
  ConnectSizeAllocateSignal(FWidget);
  if IsDesigning then
  begin
    {$IFDEF GTK3DEBUGDESIGNER}
    writeln('Designer: initializing ',dbgsName(Self),' LCLObj=',dbgsName(LCLobject),' set can focus to false.');
    {$ENDIF}
    gtk_widget_set_can_focus(Widget, False);
    gtk_widget_set_can_focus(GetContainerWidget, False);
  end;
  // g_signal_connect_data(FWidget, 'motion_notify_event', TGCallback(@Gtk3MotionNotifyEvent), LCLObject, nil, 0);
end;

procedure TGtk3Widget.ConnectSizeAllocateSignal(ToWidget:PGtkWidget);
begin
  g_signal_connect_data(ToWidget,'size-allocate',TGCallback(@SizeAllocate), Self, nil, G_CONNECT_DEFAULT);
end;

procedure TGtk3Widget.DeInitializeWidget;
begin

end;

procedure TGtk3Widget.RecreateWidget;
begin

end;

procedure TGtk3Widget.DestroyNotify(AWidget: PGtkWidget);
begin

end;

destructor TGtk3Widget.Destroy;
begin
  DestroyWidget;
  inherited Destroy;
end;

function TGtk3Widget.CanFocus: Boolean;
begin
  Result := False;
  if IsWidgetOK then
    Result := FWidget^.can_focus or GetContainerWidget^.can_focus;
end;

function TGtk3Widget.GetFocusableByMouse: Boolean;
begin
  Result := FFocusableByMouse;
end;

function TGtk3Widget.getClientOffset: TPoint;
var
  Allocation: TGtkAllocation;
  R: TRect;
begin
  {offset between inner and outer rect of widget.
   It tricky since some widgets have regular offset eg
   Parent (FWidget) = (120,80) Child (FCentralWidget) = (2,2)
   but some are
   Parent (FWidget) = (120,80) Child (FCentralWidget) = (122,82).
   Such widgets are usually those with FCentralWidget^.get_has_window}
  Result := Point(0, 0);
  if Widget <> getContainerWidget then
  begin
    GetContainerWidget^.get_allocation(@Allocation);
    Result.X := Allocation.X;
    Result.Y := Allocation.Y;
  end else
    exit;
  R := getClientBounds;
  Result := Point(Result.x + R.Left, Result.y + R.Top);
end;

function TGtk3Widget.getWidgetPos: TPoint;
var
  Allocation: TGtkAllocation;
begin
  Result := Point(0, 0);
  if IsWidgetOk then
  begin
    FWidget^.get_allocation(@Allocation);
    Result := Point(Allocation.X, Allocation.Y);
  end;
end;

procedure TGtk3Widget.OffsetMousePos(const aGlobalX, aGlobalY: double;
  APoint: PPoint);
begin
  with getClientOffset do
  begin
    dec(APoint^.x, x);
    dec(APoint^.y, y);
  end;
end;

function TGtk3Widget.ClientToScreen(var P: TPoint): boolean;
var
  TempAlloc: TGtkAllocation;
  Pt: TPoint;
  w,tw:PgtkWidget;
  x,y:integer;
  gw:PgdkWindow;
begin
  Result := False;
  Pt := Point(0, 0);

  if not IsWidgetOk then
  begin
    DebugLn('TGtk3Widget.ClientToScreen invalid widget ...');
    exit;
  end;

  { most usable source
    https://stackoverflow.com/questions/2088962/how-do-you-find-the-absolute-position-of-a-gtk-widget-in-a-window
  }

  w:=fWidget;
  tw:=w^.get_toplevel;
  gw:=tw^.window;
  while Assigned(w) {and (w<>tw)} do
  begin
    w^.get_allocation(@TempAlloc);
    pt.X:=pt.X+TempAlloc.X;
    pt.Y:=pt.Y+TempAlloc.Y;
    w:=w^.parent;
  end;

  gw^.get_origin(@x,@y);
  pt.x+=x;
  pt.y+=y;
  p:=pt;
  Result:=true;

end;

function TGtk3Widget.ScreenToClient(var P: TPoint): Integer;
var
  AGtkWidget: PGtkWidget;
  AWindow: PGdkWindow;
  X,Y: Integer;
  Allocation: TGtkAllocation;
begin
  Result:=-1;
  AGtkWidget := GetContainerWidget;
  if Assigned(AGtkWidget) and Gtk3IsGdkWindow(AGtkWidget^.window) then
  begin
    AWindow := AGtkWidget^.window;
    PGdkWindow(AWindow)^.get_origin(@X, @Y);
    AGtkWidget^.get_allocation(@Allocation);
    if not AGtkWidget^.get_has_window and (AGtkWidget^.get_parent <> nil) then
    begin
      AGtkWidget^.get_allocation(@Allocation);
      P.X := P.X - X - Allocation.x;
      P.Y := P.Y - Y - Allocation.y;
      exit;
    end;
  end else
  if Gtk3IsGdkWindow(fWidget^.window) then
  begin
    AWindow := fWidget^.window;
    PGdkWindow(AWindow)^.get_origin(@X, @Y);
  end else
  begin
    fWidget^.get_allocation(@Allocation);
    P.X := P.X - X - Allocation.x;
    P.Y := P.Y - Y - Allocation.y;
    exit;
  end;
  dec(P.X, X);
  dec(P.Y, Y);
end;

function TGtk3Widget.DeliverMessage(var Msg; const AIsInputEvent: Boolean
  ): LRESULT;
begin
  Result := LRESULT(AIsInputEvent);
  if LCLObject = nil then
    Exit;
  try
    if LCLObject.HandleAllocated then
    begin
      LCLObject.WindowProc(TLMessage(Msg));
      Result := TLMessage(Msg).Result;
    end;
  except
    Application.HandleException(nil);
  end;
end;

function TGtk3Widget.getClientRect: TRect;
var
  AAlloc: TGtkAllocation;
begin
  Result := LCLObject.BoundsRect;
  if not IsWidgetOK then
    exit;
  if GetContainerWidget^.get_realized then
  begin
    GetContainerWidget^.get_allocation(@AAlloc);
    Result := Rect(AAlloc.x, AAlloc.y, AAlloc.width + AAlloc.x,AAlloc.height + AAlloc.y);
  end else
  if FWidget^.get_realized then
  begin
    FWidget^.get_allocation(@AAlloc);
    Result := Rect(AAlloc.x, AAlloc.y, AAlloc.width + AAlloc.x,AAlloc.height + AAlloc.y);
  end;
  Types.OffsetRect(Result, -Result.Left, -Result.Top);
end;

function TGtk3Widget.getClientBounds: TRect;
var
  AAlloc: TGtkAllocation;
begin
  Result := Rect(0, 0, 0, 0);
  if IsWidgetOk then
  begin
    if FWidget^.get_realized then
    begin
      FWidget^.get_allocation(@AAlloc);
      Result := Rect(AAlloc.x, AAlloc.y, AAlloc.width + AAlloc.x,AAlloc.height + AAlloc.y);
    end else
    if GetContainerWidget^.get_realized then
    begin
      GetContainerWidget^.get_allocation(@AAlloc);
      Result := Rect(AAlloc.x, AAlloc.y, AAlloc.width + AAlloc.x,AAlloc.height + AAlloc.y);
    end;
  end;
end;

procedure TGtk3Widget.SetBounds(ALeft,ATop,AWidth,AHeight:integer);
var
  ARect: TGdkRectangle;
  Alloc: TGtkAllocation;
begin
  if (Widget=nil) then
    exit;

  LCLWidth := AWidth;
  LCLHeight := AHeight;
  ARect.x := ALeft;
  ARect.y := ATop;
  ARect.width := AWidth;
  ARect.Height := AHeight;
  with Alloc do
  begin
    x := ALeft;
    y := ATop;
    width := AWidth;
    height := AHeight;
  end;

  if Self is TGtk3Button then
  begin
    AWidth:=Max(1,AWidth-4);
    AHeight:=Max(1,AHeight-4);
  end;

  BeginUpdate;
  try
    {fixes gtk3 assertion}
    if not Widget^.get_realized then
      Widget^.realize;

    //this should be removed in future.
    Widget^.set_size_request(AWidth,AHeight);

    if Gtk3IsContainer(Widget) then // according to the gtk3 docs only GtkContainer should call this
      Widget^.size_allocate(@ARect);

    if Widget^.get_visible then
      Widget^.set_allocation(@Alloc);

    if LCLObject.Parent <> nil then
      Move(ALeft, ATop);

    // we must trigger get_preferred_width after changing size
    Widget^.queue_resize;
    if [wtCustomControl, wtScrollingWinControl] * WidgetType <> [] then
    begin
      if Gtk3IsGdkWindow(Widget^.get_window) then
        Widget^.get_window^.process_updates(True);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TGtk3Widget.SetLclFont(const AFont:TFont);
var
  AGtkFont: PPangoFontDescription;
  APangoStyle: TPangoStyle;
  Family: String;
  Stretch: TPangoStretch;
  Weight: TPangoWeight;
begin
  if not IsWidgetOk then exit;
  if IsFontNameDefault(AFont.Name) then
  begin
    AGtkFont := Self.Font;
    Stretch := PANGO_STRETCH_NORMAL;
    Weight := PANGO_WEIGHT_NORMAL;
  end else
  begin
    Family := AFont.Name;
    ExtractPangoFontFaceSuffixes(Family, Stretch, Weight);
    AGtkFont := TPangoFontDescription.new;
    AGtkFont^.set_family(PgChar(Family));
  end;

  if Stretch <> PANGO_STRETCH_NORMAL then
    AGtkFont^.set_stretch(Stretch);

  if AFont.Size <> 0 then
    AGtkFont^.set_size(Abs(AFont.Size) * PANGO_SCALE);

  if (fsBold in AFont.Style) and (Weight < PANGO_WEIGHT_SEMIBOLD) then
    // bold is specified by the fsBold flag only
    AGtkFont^.set_weight(PANGO_WEIGHT_BOLD)
  else if (Weight <> PANGO_WEIGHT_NORMAL) then
    AGtkFont^.set_weight(Weight);

  if fsItalic in AFont.Style then
    APangoStyle := PANGO_STYLE_ITALIC
  else
    APangoStyle := PANGO_STYLE_NORMAL;
  AGtkFont^.set_style(APangoStyle);

  Font := AGtkFont;
  FontColor := AFont.Color;
end;

procedure TGtk3Widget.SetWindowShape(AShape: PGdkPixBuf; AWindow: PGdkWindow);
var
  imageSurface: Pcairo_surface_t;
  ARegion: Pcairo_region_t;
  ACairoRect: Tcairo_rectangle_int_t;
begin
  if (AWindow = nil) or not Gtk3IsGdkWindow(AWindow) then
    exit;
  if AShape = nil then
  begin
    ACairoRect.x := 0;
    ACairoRect.y := 0;
    ACairoRect.width := AWindow^.get_width;
    ACairoRect.height := AWindow^.get_height;
    ARegion := cairo_region_create_rectangle(@ACairoRect);
    gdk_window_shape_combine_region(AWindow, ARegion, 0, 0);
    cairo_region_destroy(ARegion);
  end else
  begin
    //TODO: check on scaled displays.
    imageSurface := gdk_cairo_surface_create_from_pixbuf(AShape, 1, AWindow);
    ARegion := gdk_cairo_region_create_from_surface(imageSurface);
    gdk_window_shape_combine_region(AWindow, ARegion, 0, 0);
    cairo_region_destroy(ARegion);
    cairo_surface_destroy(imageSurface);
  end;
end;

function TGtk3Widget.GetContainerWidget: PGtkWidget;
begin
  if Assigned(FCentralWidget) then
    Result := FCentralWidget
  else
    Result := FWidget;
end;

function TGtk3Widget.GetPosition(out APoint: TPoint): Boolean;
var
  GdkWindow: PGdkWindow;
  GtkLeft, GtkTop: GInt;
  Alloc:TGtkAllocation;
  prnt:TGtk3Widget;
  wtype:TGType;
begin
  fWidget^.get_allocation(@Alloc);
  if (alloc.X=-1) and (alloc.Y=-1) and (alloc.height=1) and (alloc.width=1) then
  // default allocation
  else
  begin
    APoint.X:=alloc.X;
    APoint.Y:=alloc.Y;
  end;

  prnt:=self.GetParent; // TGtk3Widget
  if (prnt<>nil) then
  begin
    wtype:=prnt.getType; // parent widget type
    if (wtype<>gtk_fixed_get_type()) and
       (wtype<>gtk_layout_get_type()) then
    begin
      // widget is not on a normal client area. e.g. TPage
      Apoint.X:=0;
      APoint.Y:=0;
      Result:=true;
    end
    else
    if (wtype=gtk_fixed_get_type()) and
       prnt.Widget^.get_has_window then
    begin
      // widget on a fixed, but fixed w/o window
      prnt.Widget^.get_allocation(@alloc);
      Dec(Apoint.X, alloc.x);
      Dec(APoint.Y, alloc.y);
      Result:=true;
    end;
  end;

  if (self.getType=gtk_window_get_type()) then
  begin
    GdkWindow:=Self.Widget^.window;
    if (GdkWindow<>nil) and (Self.FWidget^.get_mapped) then
    begin
      // window is mapped = window manager has put the window somewhere
      gdk_window_get_root_origin(GdkWindow, @GtkLeft, @GtkTop);
      APoint.X := GtkLeft;
      APoint.Y := GtkTop;
      Result:=true;
    end else
    begin
      // the gtk has not yet put the window to the final position
      // => the gtk/gdk position is not reliable
      // => use the LCL coords
      Apoint.X:=LCLObject.Left;
      Apoint.Y:=LCLObject.Top;
      Result:=true;
    end;
    //DebugLn(['TGtk3WidgetSet.GetWindowRelativePosition ',GetWidgetDebugReport(aWidget),' Left=',Left,' Top=',Top,' GdkWindow=',GdkWindow<>nil]);
  end;
  //DebugLn(['TGtk3WidgetSet.GetWindowRelativePosition ',GetWidgetDebugReport(aWidget),' Left=',Left,' Top=',Top]);
end;

procedure TGtk3Widget.Release;
begin
  LCLObject := nil;
  Free;
end;

procedure TGtk3Widget.Hide;
begin
  if Assigned(FWidget) then
    FWidget^.hide;
end;

function TGtk3Widget.getParent: TGtk3Widget;
begin
  Result := Gtk3WidgetFromGtkWidget(Widget^.get_parent);
end;

function TGtk3Widget.GetWindow: PGdkWindow;
begin
  Result := FWidget^.window;
end;

procedure TGtk3Widget.Move(ALeft, ATop: Integer);
var
  AParent: TGtk3Widget;
  XOffset, YOffset: Integer;
  aWindow: PGdkWindow;
begin
  AParent := getParent;
  if (AParent <> nil) then
  begin
    XOffset := 0;
    YOffset := 0;
    if (wtContainer in AParent.WidgetType) then
      PGtkFixed(AParent.GetContainerWidget)^.move(FWidget, ALeft, ATop)
    else
    if (wtLayout in AParent.WidgetType) then
    begin
      aWindow := PGtkLayout(AParent.GetContainerWidget)^.get_bin_window;
      if Gtk3IsGdkWindow(aWindow) then
        aWindow^.get_position(@XOffset, @YOffset);
      PGtkLayout(AParent.GetContainerWidget)^.move(FWidget, ALeft - XOffset, ATop - YOffset);
    end;
  end;
end;

procedure TGtk3Widget.Activate;
begin
  if IsWidgetOK then
  begin
    if not FWidget^.visible then
      exit;
    if Gtk3IsGdkWindow(FWidget^.window) then
      FWidget^.window^.raise_
    else
    begin
      FWidget^.get_parent_window^.raise_;
    end;
    if FWidget^.can_focus then
      FWidget^.grab_focus;
  end;
end;

procedure TGtk3Widget.preferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
var
  AMinH: gint;
  AMinW: gint;
  AWidget:PGtkWidget;
  {$IFDEF GTK3DEBUGPREFERREDSIZE}
  ABorder: TGtkBorder;
  {$ENDIF}
begin
  if IsWidgetOK then
  begin
    if [wtComboBox] * WidgetType <> [] then
      AWidget := Widget
    else
      AWidget := getContainerWidget;
    {$IFDEF GTK3DEBUGPREFERREDSIZE}
     AWidget^.get_size_request(@AMinW, @AMinH);
     DebugLn('>',dbgsName(LCLObject),'.preferredSize W=',dbgs(PreferredWidth),' H=',dbgs(PreferredHeight),' WithThemeSpace ',dbgs(WithThemeSpace),' AMinW=',dbgs(AMinW),' AMinH=',dbgs(AMinH));
    {$ENDIF}
    AWidget^.get_preferred_height(@AMinH, @PreferredHeight);
    AWidget^.get_preferred_width(@AMinW, @PreferredWidth);
    {$IFDEF GTK3DEBUGPREFERREDSIZE}
    if WithThemeSpace then
    begin
      AWidget^.get_style_context^.get_margin(GTK_STATE_FLAG_NORMAL, @ABorder);
      with ABorder do
        DebugLn('BorderSpaces ',Format('L %d T %d R %d B %d',[Left, Top, Right, Bottom]));
      AWidget^.get_style_context^.get_padding(GTK_STATE_FLAG_NORMAL, @ABorder);
      with ABorder do
        DebugLn('Padding ',Format('L %d T %d R %d B %d',[Left, Top, Right, Bottom]));
    end;
    DebugLn('<',dbgsName(LCLObject),'.preferredSize W=',dbgs(PreferredWidth),' H=',dbgs(PreferredHeight),' WithThemeSpace ',dbgs(WithThemeSpace),' AMinH=',dbgs(AMinH),' AMinW=',dbgs(AMinW));
    {$ENDIF}
    if [wtComboBox] * WidgetType <> [] then
    begin
      PreferredWidth := 0;
    end;
  end;
end;

procedure TGtk3Widget.Repaint(const ARect: PRect);
var
  aLayout: PGtkLayout;
  aWindow: PGdkWindow;
  cr, tmpCtx: Pcairo_t;
  aRegion: Pcairo_region_t;
  tmpSurf: Pcairo_surface_t;
  ACairoRect: Tcairo_rectangle_int_t;
begin
  if [wtLayout] * WidgetType <> [] then
  begin
    aLayout := PGtkLayout(GetContainerWidget);
    if Gtk3IsGdkWindow(aLayout^.get_bin_window) then
    begin
      //TODO: implement ARect
      aWindow := aLayout^.get_bin_window;
      cr := gdk_cairo_create(aWindow);
      aRegion := gdk_window_get_visible_region(aWindow);
      cairo_region_get_extents(aRegion, @ACairoRect);
      tmpSurf := cairo_surface_create_similar(cairo_get_target(cr), CAIRO_CONTENT_COLOR_ALPHA, ACairoRect.Width, ACairoRect.Height);
      tmpCtx := cairo_create(tmpSurf);
      cairo_translate(tmpCtx, ACairoRect.X, ACairoRect.Y);
      gtk_widget_draw(aLayout, tmpCtx);
      cairo_set_source_surface(cr, tmpSurf, 0, 0);
      cairo_paint(cr);
      cairo_destroy(tmpCtx);
      cairo_surface_destroy(tmpSurf);
      cairo_destroy(cr);
      cairo_region_destroy(aRegion);
      exit;
    end;
  end;
  GetContainerWidget^.queue_draw;
  if GetContainerWidget^.get_has_window and Gtk3IsGdkWindow(GetContainerWidget^.window) then
    GetContainerWidget^.window^.process_updates(True);
end;

procedure TGtk3Widget.SetCursor(ACursor: HCURSOR);
begin
  if IsWidgetOk then
  begin
    if GetContainerWidget^.get_has_window and Gtk3IsGdkWindow(GetContainerWidget^.window) then
      SetWindowCursor(GetContainerWidget^.window, HCURSOR(TGtk3Cursor(ACursor).Handle), False, True)
    else
    if Widget^.get_has_window and Gtk3IsGdkWindow(Widget^.window) then
      SetWindowCursor(Widget^.window, HCURSOR(TGtk3Cursor(ACursor).Handle), False, True)
    else // fallback for window-less widgets
    if Assigned(self.getParent) then
      Self.getParent.SetCursor(ACursor);
  end;
end;

procedure TGtk3Widget.SetFocus;
begin
  if GetContainerWidget^.can_focus then
    GetContainerWidget^.grab_focus
  else
  if FWidget^.can_focus then
    FWidget^.grab_focus;
end;

procedure TGtk3Widget.SetParent(AParent: TGtk3Widget; const ALeft, ATop: Integer
  );
begin
  if FWidget=nil then exit;;
  if wtLayout in AParent.WidgetType then
    PGtkLayout(AParent.GetContainerWidget)^.put(FWidget, ALeft, ATop)
  else
  if wtContainer in AParent.WidgetType then
    PGtkFixed(AParent.GetContainerWidget)^.put(FWidget, ALeft, ATop)
  else
  if wtNotebook in AParent.WidgetType then
    // do nothing !
  else
    FWidget^.set_parent(AParent.GetContainerWidget);
end;

procedure TGtk3Widget.Show;
begin
  if IsValidHandle then
  begin
    FWidget^.show;
  end;
end;

procedure TGtk3Widget.ShowAll;
begin
  if IsValidHandle then
    FWidget^.show_all;
end;

procedure TGtk3Widget.Update(ARect: PRect);
begin
  if IsWidgetOK then
  begin
    if (ARect <> nil) then
    begin
      if (aRect^.Width > 0) and (ARect^.Height > 0) then
      begin
        with ARect^ do
          FWidget^.queue_draw_area(Left, Top, Right - Left, Bottom - Top);
        if FWidget <> GetContainerWidget then
          with ARect^ do
            GetContainerWidget^.queue_draw_area(Left, Top, Right - Left, Bottom - Top);
      end;
    end else
    begin
      //FWidget^.queue_draw;
      if FWidget <> GetContainerWidget then
        GetContainerWidget^.queue_draw
      else
        FWidget^.queue_draw;
    end;
  end;
end;

{ TGtk3StatusBarPanel }

function TGtk3StatusBarPanel.GetPanelText: string;
begin
  Result := '';
  if Assigned(FLabel) then
    Result := StrPas(gtk_label_get_text(FLabel));
end;

function TGtk3StatusBarPanel.GetPanelWidth: integer;
begin
  Result := 0;
  if Assigned(FLabel) then
  begin
    Result := gtk_widget_get_allocated_width(FLabel);
  end;
end;

procedure TGtk3StatusBarPanel.SetPanelText(AValue: string);
begin
  gtk_label_set_text(FLabel,PgChar(AValue));
end;

procedure TGtk3StatusBarPanel.SetPanelWidth(AValue: integer);
begin
  gtk_widget_set_size_request(FLabel, AValue, -1);
end;

constructor TGtk3StatusBarPanel.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
end;

destructor TGtk3StatusBarPanel.Destroy;
begin
  if Assigned(FLabel) then
  begin
    gtk_widget_hide(FLabel);
    FLabel^.destroy_;
    FLabel := nil;
  end;
  inherited Destroy;
end;

{ TGtk3StatusBarPanels }

function TGtk3StatusBarPanels.GetPanel(AIndex: integer): TGtk3StatusBarPanel;
begin
  Result := Items[AIndex] as TGtk3StatusBarPanel;
end;

constructor TGtk3StatusBarPanels.Create(AStatusBar: TGtk3Widget);
begin
  inherited Create(TGtk3StatusBarPanel);
  FStatusBar := AStatusBar;
end;

function TGtk3StatusBarPanels.Add: TGtk3StatusBarPanel;
begin
  Result := inherited Add as TGtk3StatusBarPanel;
end;

function TGtk3StatusBarPanels.AddPanel(APanel: TStatusPanel; ABox: PGtkBox
  ): TGtk3StatusBarPanel;
var
  argba: TGdkRGBA;
begin
  Result := Self.Add;
  Result.FLabel := gtk_label_new(PgChar(APanel.Text));
  Result.FLabel^.set_size_request(APanel.Width, -1);
  Result.FLabel^.set_xalign(0.00);
  Result.FLabel^.set_ellipsize(PANGO_ELLIPSIZE_END);
  Result.FLabel^.set_line_wrap(False);
  gtk_widget_set_halign(Result.FLabel, GTK_ALIGN_FILL);
  gtk_widget_set_valign(Result.FLabel, GTK_ALIGN_CENTER);
  Result.FLabel^.set_hexpand(False);
  gtk_box_pack_start(ABox, Result.FLabel, False, False, 0);
  Result.FLabel^.show_all;
end;

{ TGtk3StatusBar }

function TGtk3StatusBar.CreateWidget(const Params: TCreateParams): PGtkWidget;
var
  AContext: PGtkStyleContext;
  i: Integer;
begin
  Result := TGtkEventBox.new;
  FCentralWidget := TGtkHBox.new(GTK_ORIENTATION_HORIZONTAL, 1);
  PGtkBox(FCentralWidget)^.set_homogeneous(False);
  PGtkEventBox(Result)^.add(FCentralWidget);
  AContext := gtk_widget_get_style_context(FCentralWidget);
  gtk_style_context_add_class(AContext, 'statusbar');
  FPanels := TGtk3StatusBarPanels.Create(Self);
  if TStatusBar(LCLObject).SimplePanel then
  begin
    FSimplePanel := TGtkLabel.New(nil);
    gtk_widget_set_halign(FSimplePanel, GTK_ALIGN_START);
    gtk_widget_set_hexpand(FSimplePanel, True);
    FSimplePanel^.set_ellipsize(PANGO_ELLIPSIZE_END);
    gtk_box_pack_start(PGtkBox(FCentralWidget), FSimplePanel, True, True, 0);
    FSimplePanel^.show;
  end else
  begin
    for i := 0 to TStatusBar(LCLObject).Panels.Count - 1 do
      FPanels.AddPanel(TStatusBar(LCLObject).Panels[i], PGtkBox(FCentralWidget));
  end;
end;

destructor TGtk3StatusBar.Destroy;
begin
  if Assigned(FPanels) then
    FPanels.Free;
  inherited Destroy;
end;

procedure TGtk3StatusBar.SetPanelText(const AText: string;
  const APanelIndex: integer);
begin
  with FPanels.Items[APanelIndex] as TGtk3StatusBarPanel do
    PanelText := AText;
end;

procedure TGtk3StatusBar.SetSimpleText(const AText: string);
begin
  {%H-}FSimplePanel^.set_text(PgChar(AText));
end;

procedure TGtk3StatusBar.UpdateStatusBar(AStatusBar: TStatusBar);
var
  i: Integer;
  APanel: TGtk3StatusBarPanel;
begin
  if AStatusBar.SimplePanel then
  begin
    if Assigned(FPanels) and (FPanels.Count > 0) then
      FPanels.Clear;
    if not Assigned(FSimplePanel) then
      FSimplePanel := TGtkLabel.New(nil);
    gtk_widget_set_halign(FSimplePanel, GTK_ALIGN_START);
    gtk_widget_set_hexpand(FSimplePanel, True);
    FSimplePanel^.set_ellipsize(PANGO_ELLIPSIZE_END);
    gtk_box_pack_start(PGtkBox(FCentralWidget), FSimplePanel, True, True, 0);
    FSimplePanel^.show;
    {%H-}FSimplePanel^.set_text(PgChar(AStatusBar.SimpleText));
  end else
  begin
   if Assigned(FSimplePanel) then
    begin
      FSimplePanel^.hide;
      FSimplePanel^.destroy_;
      FSimplePanel := nil;
    end;
    while FPanels.Count > AStatusBar.Panels.Count do
      FPanels.Delete(FPanels.Count -1);
    for i := 0 to AStatusBar.Panels.Count - 1 do
    begin
      if FPanels.Count - 1 < i then
        APanel := FPanels.AddPanel(AStatusBar.Panels[i], PGtkBox(FCentralWidget))
      else
        APanel := FPanels.Items[i] as TGtk3StatusBarPanel;
      APanel.SetPanelWidth(AStatusBar.Panels[i].Width);
      APanel.SetPanelText(AStatusBar.Panels[i].Text);
    end;
  end;
end;

{ TGtk3Panel }

class procedure TGtk3Panel.PanelLayoutSizeAllocate(AWidget: PGtkWidget;
  AGdkRect: PGdkRectangle; Data: gpointer); cdecl;
var
  HSize,VSize: integer;
  uWidth, uHeight: guint;
begin
  HSize := AGdkRect^.Width;
  VSize := AGdkRect^.Height;

  PGtkLayout(aWidget)^.get_size(@uWidth, @uHeight);
  if (uWidth <> HSize) or (uHeight <> VSize) then
    PGtkLayout(aWidget)^.set_size(HSize, VSize);

  if not TGtk3Widget(Data).InUpdate and TGtk3Widget(Data).LCLObject.ClientRectNeedsInterfaceUpdate then
    TGtk3Widget(Data).LCLObject.DoAdjustClientRectChange;
end;

procedure TGtk3Panel.SetBorderStyle(AValue: TBorderStyle);
begin
  if FBorderStyle = AValue then Exit;
  FBorderStyle := AValue;
  PGtkLayout(Widget)^.set_border_width(Ord(AValue));
end;

function TGtk3Panel.CreateWidget(const Params: TCreateParams): PGtkWidget;
var
  AGdkRGBA: TGdkRGBA;
begin
  FHasPaint := True;
  FBorderStyle := bsNone;

  FWidgetType := [wtWidget, wtLayout, wtPanel];
  Result := TGtkLayout.new(nil, nil);
  Result^.set_has_window(True);
  // as GtkFixed have no child control here - nobody triggers resizing
  // GNOME takes care of it, but other WM - not
  // this is here to make TGtk3Panel shown under Plasma
  //Result^.set_size_request(LCLObject.Width,LCLObject.Height);

  if not (csDesigning in LCLObject.ComponentState) then
    g_object_set(PGObject(Result), 'resize-mode', [GTK_RESIZE_QUEUE, nil]);
  PGtkLayout(Result)^.set_size(1, 1);
  g_signal_connect_data(Result,'size-allocate',TGCallback(@PanelLayoutSizeAllocate), Self, nil, G_CONNECT_DEFAULT);

  // AColor := Result^.style^.bg[0];
  // writeln('BG COLOR R=',AColor.red,' G=',AColor.green,' B=',AColor.blue);
  // now we make panel completely transparent.
  // SetColor must usr override_background_color for panel
  // we must implement cairo_pattern_t since background can be brush
  AGdkRGBA.alpha := 0;
  AGdkRGBA.red := 0; // AColor.Red / 65535.00;
  AGdkRGBA.blue := 0; // AColor.Blue / 65535.00;
  AGdkRGBA.green := 0; // AColor.green / 65535.00;
  Result^.override_background_color(GTK_STATE_FLAG_NORMAL, @AGdkRGBA);
  Result^.override_background_color([GTK_STATE_FLAG_ACTIVE], @AGdkRGBA);
  Result^.override_background_color([GTK_STATE_FLAG_FOCUSED], @AGdkRGBA);
  Result^.override_background_color([GTK_STATE_FLAG_PRELIGHT], @AGdkRGBA);
  Result^.override_background_color([GTK_STATE_FLAG_SELECTED], @AGdkRGBA);
  Result^.show_all;
end;

procedure TGtk3Panel.DoBeforeLCLPaint;
var
  DC: TGtk3DeviceContext;
  NColor: TColor;
begin
  inherited DoBeforeLCLPaint;
  if not Visible then
    exit;
  DC := TGtk3DeviceContext(Context);
  NColor := LCLObject.Color;
  if (NColor <> clNone) and (NColor <> clDefault) then
  begin
    DC.CurrentBrush.Color := ColorToRGB(NColor);
    DC.fillRect(0, 0, LCLObject.Width, LCLObject.Height);
  end;

  if BorderStyle <> bsNone then
  begin
    DC.CurrentPen.Color := ColorToRGB(clBtnShadow); // not sure what color to use here?
    DC.drawRect(0, 0, LCLObject.Width, LCLObject.Height, False, True);
  end;
end;

procedure TGtk3Panel.setText(const AValue: String);
begin
  if FText = AValue then
    exit;
  FText := AValue;
  if Self.Visible then
    Widget^.queue_draw;
end;

{ TGtk3GroupBox }

class procedure TGtk3GroupBox.GroupBoxLayoutSizeAllocate(
  AWidget: PGtkWidget; AGdkRect: PGdkRectangle; Data: gpointer); cdecl;
var
  HSize,VSize: integer;
  uWidth, uHeight: guint;
begin
  HSize := AGdkRect^.Width;
  VSize := AGdkRect^.Height;

  PGtkLayout(aWidget)^.get_size(@uWidth, @uHeight);
  if (uWidth <> HSize) or (uHeight <> VSize) then
    PGtkLayout(aWidget)^.set_size(HSize, VSize);

  if not TGtk3Widget(Data).InUpdate and TGtk3Widget(Data).LCLObject.ClientRectNeedsInterfaceUpdate then
    TGtk3Widget(Data).LCLObject.DoAdjustClientRectChange;
end;

function TGtk3GroupBox.CreateWidget(const Params: TCreateParams): PGtkWidget;
begin
  FHasPaint := True;
  FGroupBoxType := gbtGroupBox;
  FWidgetType := [wtWidget, wtLayout, wtGroupBox];
  Result := LCLGtkFrameNew;
  FCentralWidget := TGtkLayout.new(nil, nil);
  PGtkBin(Result)^.add(FCentralWidget);
  FCentralWidget^.set_has_window(True);
  PGtkFrame(result)^.set_label_align(0.1,0.5);
  if not (csDesigning in LCLObject.ComponentState) then
    g_object_set(PGObject(FCentralWidget), 'resize-mode', [GTK_RESIZE_QUEUE, nil]);
  PGtkLayout(FCentralWidget)^.set_size(1, 1);
  g_signal_connect_data(FCentralWidget,'size-allocate',TGCallback(@GroupBoxLayoutSizeAllocate), Self, nil, G_CONNECT_DEFAULT);
  Result^.show_all;
  Result^.hide;
end;

function TGtk3GroupBox.getText: String;
begin
  Result := '';
  if IsWidgetOK then
  begin
    if PGtkFrame(Widget)^.get_label_widget = nil then
      exit;
    Result := {%H-}ReplaceUnderscoresWithAmpersands(PGtkFrame(Widget)^.get_label);
  end;
end;

procedure TGtk3GroupBox.setText(const AValue: String);
begin
  if IsWidgetOK then
  begin
    if AValue = '' then
      PGtkFrame(Widget)^.set_label_widget(nil)
    else
    begin
      if PGtkFrame(Widget)^.get_label_widget = nil then
        PGtkFrame(Widget)^.set_label_widget(TGtkLabel.new(''));
      {%H-}PGtkFrame(Widget)^.set_label(PgChar({%H-}ReplaceAmpersandsWithUnderscores(AValue)));
    end;
  end;
end;

function TGtk3GroupBox.getClientOffset: TPoint;
var
  Allocation: TGtkAllocation;
  R: TRect;
begin
  Self.Widget^.get_allocation(@Allocation);
  Result.X := -Allocation.X;
  Result.Y := -Allocation.Y;
  R := getClientBounds;
  Result := Point(Result.x + R.Left, Result.y + R.Top);
end;

{$IF DEFINED(GTK3DEBUGSIZE) OR DEFINED(GTK3DEBUGGROUPBOX)}
procedure ContainerChildrenCallback(widget: PGtkWidget; data: gpointer); cdecl;
begin
  // This callback is called for each child of the GtkFixed container
  WriteLn('TGtk3GroupBox Child widget pointer: ', PtrUInt(widget),' ACtl=',dbgsName(TGtk3WIdget(data)));

  // Example: Print the widget type name
  WriteLn('TGtk3GroupBox Widget type: ', gtk_widget_get_name(widget));
end;
{$ENDIF}

class procedure TGtk3GroupBox.GroupBoxSizeAllocate(AWidget:PGtkWidget;AGdkRect:
  PGdkRectangle;Data:gpointer);cdecl;
var
  Msg: TLMSize;
  NewSize: TSize;
  ACtl: TGtk3GroupBox;
  AState: TGdkWindowState;
  Alloc: TGtkAllocation;
  AList:PGList;
  AFixed: PGtkFixed;
  i:Integer;
begin
  if AWidget=nil then ;

  ACtl := TGtk3GroupBox(Data);

  {$IF DEFINED(GTK3DEBUGSIZE) OR DEFINED(GTK3DEBUGGROUPBOX)}
  with AGdkRect^ do
    DebugLn('**** GroupBoxSizeAllocate **** ....',dbgsName(ACtl.LCLObject),
      ' ',Format('GTK x %d y %d w %d h %d',[x, y, width, height]),
      Format(' LCL W=%d H=%d LLW %d LLH %d upd=%s',[ACtl.LCLObject.Width, ACtl.LCLObject.Height, ACtl.LCLWidth, ACtl.LCLHeight, BoolToStr(ACtl.InUpdate, True)]));
  {$ENDIF}

  with Alloc do
  begin
    x := AGdkRect^.x;
    y := AGdkRect^.y;
    Width := AGdkRect^.width;
    Height := AGdkRect^.height;
  end;

  gtk_widget_set_clip(AWidget, @Alloc);

  if not Assigned(ACtl.LCLObject) then exit;

  // return size w/o frame
  NewSize.cx := AGdkRect^.width;
  NewSize.cy := AGdkRect^.height;

  if not (csDesigning in ACtl.LCLObject.ComponentState) then
  begin
    if ACtl.InUpdate then
      exit;
  end;

  {$IF DEFINED(GTK3DEBUGSIZE) OR DEFINED(GTK3DEBUGGROUPBOX)}
  if not ACtl.LCLObject.AutoSize and (ACtl.LCLWidth > 0) and (ACtl.LCLHeight > 0) and
    ACtl.LCLObject.ClientRectNeedsInterfaceUpdate then
  begin
    if (AGdkRect^.Width = ACtl.LCLWidth) and (AGdkRect^.Height = ACtl.LCLHeight) then
    begin
      //ACtl.LCLObject.DoAdjustClientRectChange;
      AFixed := PGtkFixed(ACtl.getContainerWidget);
      if AFixed^.compute_expand(GTK_ORIENTATION_VERTICAL) then
        AFixed^.resize_children;
      // PGtkLayout(AFixed)^.set_size(AFixed^.get_allocated_width, AFixed^.get_allocated_height);
      gtk_container_foreach(AFixed, @ContainerChildrenCallback, ACtl);
      exit;
    end;
  end;
  {$ENDIF}

  if ACtl.LCLObject.ClientRectNeedsInterfaceUpdate then
    ACtl.LCLObject.DoAdjustClientRectChange;

  FillChar(Msg{%H-}, SizeOf(Msg), #0);

  Msg.Msg := LM_SIZE;
  Msg.SizeType := SIZE_RESTORED;

  Msg.SizeType := Msg.SizeType or Size_SourceIsInterface;

  Msg.Width := Word(NewSize.cx);
  Msg.Height := Word(NewSize.cy);
  ACtl.DeliverMessage(Msg);
end;

{This routine is used as long as gtk3 is beta and getClientRect needs debugging}
function TGtk3GroupBox.GetInnerClientRect(Frame: PGtkWidget): TRect;
var
  Allocation: TGdkRectangle;
  AStyleContext: PGtkStyleContext;
  Padding, Border: TGtkBorder;
  LabelWidget: PGtkWidget;
  FinalRect: TGdkRectangle;
  minH:gint;
  natH:gint;
begin
  Result := Rect(0, 0, 0, 0);

  gtk_widget_get_allocation(Frame, @Allocation);
  LabelWidget := gtk_frame_get_label_widget(PGtkFrame(Frame));

  AStyleContext := gtk_widget_get_style_context(Frame);
  gtk_style_context_get_padding(AStyleContext, GTK_STATE_FLAG_NORMAL, @Padding);
  gtk_style_context_get_border(AStyleContext, GTK_STATE_FLAG_NORMAL, @Border);

  FinalRect.X := Allocation.X + Border.Left + Padding.Left;
  FinalRect.Y := Allocation.Y + Border.Top + Padding.Top;
  FinalRect.Width := Allocation.Width - Border.Left - Border.Right - Padding.Left - Padding.Right;
  FinalRect.Height := Allocation.Height - Border.Top - Border.Bottom - Padding.Top - Padding.Bottom;

  if PGtkFrame(Frame)^.get_shadow_type > GTK_SHADOW_NONE then
  begin
    // this looks like a bug in gtk3, that's why I separated this part of code. Zeljan.
    if (Border.left = 0) and (Border.Right = 0) then
      dec(FinalRect.Width, 2);
    if (Border.Top = 0) and (Border.Bottom = 0) then
      dec(FinalRect.Height, 2);
  end;

  if Assigned(LabelWidget) then
  begin
    PGtkLabel(LabelWidget)^.get_preferred_height(@minH, @natH);
    FinalRect.Y := FinalRect.Y + natH;
    FinalRect.Height := FinalRect.Height - natH;
    {$IF DEFINED(GTK3DEBUGSIZE) OR DEFINED(GTK3DEBUGGROUPBOX)}
    writeln('LabelAllocation  LCLObject.Caption=',LCLObject.Caption,' LabelText=',PGtkLabel(LabelWidget)^.get_text,' MinH=',MinH,' NatH=',NatH);
    {$ENDIF}
  end;

  Result := RectFromGdkRect(FinalRect);
end;

procedure TGtk3GroupBox.DoBeforeLCLPaint;
var
  DC: TGtk3DeviceContext;
  NColor: TColor;
begin
  inherited DoBeforeLCLPaint;
  if not Visible then
    exit;
  DC := TGtk3DeviceContext(Context);
  NColor := LCLObject.Color;
  if (NColor <> clNone) and (NColor <> clDefault) then
  begin
    DC.CurrentBrush.Color := ColorToRGB(NColor);
    DC.fillRect(0, 0, getContainerWidget^.get_allocated_width, getContainerWidget^.get_allocated_height);
  end;
end;

procedure TGtk3GroupBox.ConnectSizeAllocateSignal(ToWidget:PGtkWidget);
begin
  g_signal_connect_data(ToWidget,'size-allocate',TGCallback(@GroupBoxSizeAllocate), Self, nil, G_CONNECT_DEFAULT);
end;


function TGtk3GroupBox.getClientRect:TRect;
var
  Alloc:TGtkAllocation;
  R: TRect;
begin
  Result := GetInnerClientRect(Widget);
  Types.OffsetRect(Result, -Result.Left, -Result.Top);
end;

{ TGtk3Editable }

function gtk3EditableDelayedSelStart(AData: Pointer): gboolean; cdecl;
var
  AWidget: PGtkEditable;
  AEditable: TGtk3Editable;
begin
  Result := False;
  AEditable := TGtk3Editable(AData);
  AWidget := PGtkEditable(TGtk3Widget(AData).Widget);
  if (AEditable.PrivateCursorPos <> -1) and (AEditable.PrivateSelection <> -1) then
  begin
    gtk_editable_select_region(AWidget,AEditable.PrivateCursorPos, AEditable.PrivateSelection);
    // gtk_editable_set_position(AWidget, TGtk3Editable(AData).PrivateCursorPos);
  end;
  AEditable.PrivateCursorPos := -1;
  AEditable.PrivateSelection := -1;
  g_idle_remove_by_data(AData);
end;

function TGtk3Editable.GetReadOnly: Boolean;
begin
  Result := False;
  if IsWidgetOK then
    Result := not PGtkEditable(Widget)^.get_editable;
end;

procedure TGtk3Editable.SetReadOnly(AValue: Boolean);
begin
  if IsWidgetOK then
    PGtkEditable(Widget)^.set_editable(not AValue);
end;

function TGtk3Editable.getCaretPos: TPoint;
begin
  Result := Point(0, 0);
  if not IsWidgetOk then
    exit;
  Result.X := PGtkEditable(Widget)^.get_position;
end;

procedure TGtk3Editable.SetCaretPos(AValue: TPoint);
begin
  if not IsWidgetOk then
    exit;
  PGtkEditable(Widget)^.set_position(AValue.X);
end;

function TGtk3Editable.getSelStart: Integer;
var
  AStart: gint;
  AStop: gint;
begin
  Result := 0;
  if not IsWidgetOk then
    exit;
  if PGtkEditable(Widget)^.get_selection_bounds(@AStart, @AStop) then
  begin
    Result := AStart;
  end;
end;

function TGtk3Editable.getSelLength: Integer;
var
  AStart: gint;
  AStop: gint;
begin
  Result := 0;
  if not IsWidgetOk then
    exit;
  if PGtkEditable(Widget)^.get_selection_bounds(@AStart, @AStop) then
  begin
    Result := AStop - AStart;
  end;
end;

procedure TGtk3Editable.setSelStart(AValue: Integer);
var
  ASelStart:gint;
  ASelStop:gint;
begin
  if not IsWidgetOk then
    exit;
  CaretPos := Point(AValue, 0);
  PGtkEditable(FWidget)^.get_selection_bounds(@ASelStart, @ASelStop);
  if ASelStop < AValue then
    ASelStop := AValue;

  if InUpdate then
  begin
    PrivateCursorPos := ASelStart;
    PrivateSelection := AValue;
  end;

  PGtkEditable(FWidget)^.select_region(AValue, ASelStop);
end;

procedure TGtk3Editable.setSelLength(AValue: Integer);
var
  AStart: gint;
  AStop: gint;
begin
  if not IsWidgetOk then
    exit;
  PGtkEditable(Widget)^.get_selection_bounds(@AStart, @AStop);
  AStart := CaretPos.X;
  // DebugLn('TGtk3Editable.SetSelLength ',dbgsName(LCLObject),' value=',dbgs(AValue),' AStart=',dbgs(AStart),' InUpdate ',dbgs(InUpdate));
  if InUpdate then
  begin
    PrivateCursorPos := AStart;
    PrivateSelection := AValue;
    // g_idle_add(@gtk3EditableDelayedSelStart, Self)
    // setDelayed later
    PGtkEditable(Widget)^.select_region(AStart, AStart + AValue)
  end else
    PGtkEditable(Widget)^.select_region(AStart, AStart + AValue);
end;

{ TGtk3Entry }

function TGtk3Entry.GetAlignment: TAlignment;
var
  AFloat: GFloat;
begin
  Result := taLeftJustify;
  if not IsWidgetOk then
    exit;
  AFloat := PGtkEntry(Widget)^.get_alignment;
  if AFloat = 1 then
    Result := taRightJustify
  else
  if AFloat = 0.5 then
    Result := taCenter;
end;

procedure TGtk3Entry.SetAlignment(AValue: TAlignment);
var
  AFloat: GFloat;
begin
  if not IsWidgetOk then
    exit;
  case AValue of
    taCenter: AFloat := 0.5;
    taRightJustify: AFloat := 1.0;
    else
      AFloat := 0;;
  end;
  PGtkEntry(Widget)^.set_alignment(AFloat);
end;

function TGtk3Entry.EatArrowKeys(const AKey: Word): Boolean;
begin
  Result := AKey in [VK_UP, VK_DOWN];
end;

class procedure TGtk3Entry.EntryChanged({%H-}AEntry: PGtkEntryBuffer; AData: GPointer); cdecl;
var
  Msg: TLMessage;
  S:String;
  I:Integer;
  ASpin: TGtk3SpinEdit;
  fl: double;
begin
  FillChar(Msg{%H-}, SizeOf(Msg), 0);
  Msg.Msg := CM_TEXTCHANGED;
  if [wtSpinEdit] * TGtk3Widget(AData).WidgetType <> [] then
  begin
    ASpin := TGtk3SpinEdit(AData);
    if not TGtk3Widget(AData).InUpdate  then
    begin
      S := TGtk3SpinEdit(adata).getText;
      I := 0;
      if (ASpin.NumDigits = 0) then
      begin
        if TryStrToInt(S, I) then
        begin
          if (I >= Round(TGtk3SpinEdit(adata).Minimum)) and (I<= Round(TGtk3SpinEdit(adata).Maximum)) then
            PGtkSpinButton(TGtk3SpinEdit(adata).Widget)^.set_value(I);
        end;
      end else
      begin
        fl := 0;
        if TryStrToFloat(S, fl) then
        begin
          if (fl >= TGtk3SpinEdit(adata).Minimum) and (fl<= TGtk3SpinEdit(adata).Maximum) then
            PGtkSpinButton(TGtk3SpinEdit(adata).Widget)^.set_value(fl);
        end;
      end;
    end;
    exit; // value-changed should trigger
  end;
  if not TGtk3Widget(AData).InUpdate then
    TGtk3Widget(AData).DeliverMessage(Msg);
end;

class procedure TGtk3Entry.InsertText(editable: PGtkEditable; aNewText: PgChar; anewtextlen: gint;
  var pos:Pgint; data: gpointer);cdecl;
var
  i:integer;
  edt:TGtk3Entry;
  //s: string;
begin
  if not Assigned(data) then
    exit;

  edt := TGtk3Entry(data);
  if [wtSpinEdit] * TGtk3Widget(data).WidgetType <> [] then
  begin
    (*
    if pos <> nil then
      s := pos^.ToString
    else
      s := 'nil';
    writeln('SpinEdit.InsertText text=',edt.getText,' newText="',aNewText,'"',' newLen=',anewTextLen,' pos=',s);
    *)
  end else
  if (edt.LCLObject as TCustomEdit).NumbersOnly then
  begin
    for i := 0 to anewtextlen-1 do
    begin
       if not (aNewText[i] in ['0'..'9']) then
       begin
         g_signal_stop_emission_by_name(PGObject(editable), 'insert-text');
         exit;
       end;
    end;
  end;
end;

class procedure TGtk3Entry.EntrySizeAllocate(AWidget:PGtkWidget;AGdkRect:
  PGdkRectangle;Data:gpointer);cdecl;
var
  Msg: TLMSize;
  NewSize: TSize;
  ACtl: TGtk3Entry;
  AState: TGdkWindowState;
  Alloc: TGtkAllocation;
begin
  if AWidget=nil then ;

  ACtl := TGtk3Entry(Data);

  {$IF DEFINED(GTK3DEBUGENTRY) OR DEFINED(GTK3DEBUGENTRY)}
  with AGdkRect^ do
    DebugLn('**** EntrySizeAllocate **** ....',dbgsName(ACtl.LCLObject),
      ' ',Format('GTK x %d y %d w %d h %d',[x, y, width, height]),
      Format(' LCL W=%d H=%d LLW %d LLH %d',[ACtl.LCLObject.Width, ACtl.LCLObject.Height, ACtl.LCLWidth, ACtl.LCLHeight]));
  {$ENDIF}

  with Alloc do
  begin
    x := AGdkRect^.x;
    y := AGdkRect^.y;
    Width := AGdkRect^.width;
    Height := AGdkRect^.height;
  end;

  //fix layout, especially for GtkSpinButton
  if [wtSpinEdit] * ACtl.WidgetType <> [] then
    gtk_widget_set_clip(AWidget, @Alloc);

  if not Assigned(ACtl.LCLObject) then exit;

  // return size w/o frame
  NewSize.cx := AGdkRect^.width;
  NewSize.cy := AGdkRect^.height;


  if not (csDesigning in ACtl.LCLObject.ComponentState) then
  begin
    if ACtl.InUpdate then
      exit;
  end;

  if ((NewSize.cx <> ACtl.LCLObject.Width) or (NewSize.cy <> ACtl.LCLObject.Height) or
     ACtl.LCLObject.ClientRectNeedsInterfaceUpdate) then
  begin
    ACtl.LCLObject.DoAdjustClientRectChange;
  end;

  FillChar(Msg{%H-}, SizeOf(Msg), #0);

  Msg.Msg := LM_SIZE;
  Msg.SizeType := SIZE_RESTORED;

  Msg.SizeType := Msg.SizeType or Size_SourceIsInterface;

  Msg.Width := Word(NewSize.cx);
  Msg.Height := Word(NewSize.cy);
  ACtl.DeliverMessage(Msg);
end;

procedure TGtk3Entry.ConnectSizeAllocateSignal(ToWidget:PGtkWidget);
begin
  g_signal_connect_data(ToWidget,'size-allocate',TGCallback(@EntrySizeAllocate), Self, nil, G_CONNECT_DEFAULT);
end;

function TGtk3Entry.getText: String;
begin
  if IsValidHandle and IsWidgetOk then
    Result := StrPas(PGtkEntry(Widget)^.get_text)
  else
    Result := '';
end;

procedure TGtk3Entry.setText(const AValue: String);
begin
  if IsValidHandle and IsWidgetOK then
    {%H-}PGtkEntry(Widget)^.set_text(PgChar(AValue));
end;

function TGtk3Entry.CreateWidget(const Params: TCreateParams): PGtkWidget;
begin
  Result := LCLGtkEntryNew;
  FWidgetType := FWidgetType + [wtEntry];
  fText:=Params.Caption;
  PrivateCursorPos := -1;
  PrivateSelection := -1;
end;

procedure TGtk3Entry.InitializeWidget;
begin
  inherited InitializeWidget;

  Widget^.set_size_request(fParams.Width,fParams.Height);
  PgtkEntry(Widget)^.set_text(PgChar(fParams.Caption));

  Self.SetTextHint(TCustomEdit(Self.LCLObject).TextHint);
  Self.SetNumbersOnly(TCustomEdit(Self.LCLObject).NumbersOnly);

  g_signal_connect_data(Widget, 'changed', TGCallback(@EntryChanged), Self, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(Widget, 'insert-text', TGCallback(@InsertText), Self, nil, G_CONNECT_DEFAULT);
end;

procedure TGtk3Entry.preferredSize(var PreferredWidth,PreferredHeight:integer;
  WithThemeSpace:Boolean);
begin
  inherited preferredSize(PreferredWidth,PreferredHeight,WithThemeSpace);
  PreferredWidth := 0;
end;

procedure TGtk3Entry.SetPasswordChar(APasswordChar: Char);
var
  PWChar: Integer;
begin
  if IsWidgetOK then
  begin
    PWChar := ord(APasswordChar);
    if (PWChar < 192) or (PWChar = ord('*')) then
      PWChar := 9679;
    PGtkEntry(Widget)^.set_invisible_char(PWChar);
  end;
end;

procedure TGtk3Entry.SetNumbersOnly(ANumbersOnly:boolean);
const
  ips:array[boolean]of TGtkInputPurpose=(GTK_INPUT_PURPOSE_FREE_FORM,GTK_INPUT_PURPOSE_NUMBER);
begin
  // this is not enough for numeric input - it is just a hint for GUI
  if IsWidgetOK then
    PGtkEntry(Widget)^.set_input_purpose(ips[ANumbersOnly]);
end;

procedure TGtk3Entry.SetTextHint(const AHint: string);
begin
  if IsWidgetOK and (Ahint<>'') then
    PGtkEntry(Widget)^.set_placeholder_text(PgChar(AHint));
end;

procedure TGtk3Entry.SetFrame(const aborder: boolean);
begin
  if IsWidgetOk then
    PGtkEntry(Widget)^.set_has_frame(aborder);
end;

procedure TGtk3Entry.SetSelText(const ASelText: string);
var
  AEntry: PGtkEntry;
  AText: Pgchar;
  APos: SizeInt;
begin
  if not IsWidgetOK then
    exit;
  AEntry := PGtkEntry(Widget);
  AText := AEntry^.get_text;
  if AText = nil then
    exit;
  APos := Pos(aSelText, StrPas(AText));
  if APos > 0 then
    PGtkEditable(AEntry)^.select_region(APos - 1, APos - 1 + length(ASelText));
end;

function TGtk3Entry.GetTextHint:string;

begin
  if IsWidgetOK then
    Result:=PGtkEntry(Widget)^.get_placeholder_text()
  else
    Result:='';
end;

procedure TGtk3Entry.SetEchoMode(AVisible: Boolean);
begin
  if IsWidgetOK then
    PGtkEntry(Widget)^.set_visibility(AVisible);
end;

procedure TGtk3Entry.SetMaxLength(AMaxLength: Integer);
begin
  if IsWidgetOK then
  begin
    PGtkEntry(Widget)^.set_max_length(AMaxLength);
    PGtkEntry(Widget)^.set_width_chars(AMaxLength);
  end;

end;

function TGtk3Entry.IsWidgetOk: Boolean;
begin
  Result := (Widget <> nil) and Gtk3IsEntry(Widget);
end;

{ TGtk3SpinEdit }

function TGtk3SpinEdit.GetMaximum: Double;
var
  AFloat: gdouble;
begin
  Result := 0;
  if IsWidgetOk then
    PGtkSpinButton(Widget)^.get_range(@AFloat ,@Result);
end;

function TGtk3SpinEdit.GetMinimum: Double;
var
  AFloat: gdouble;
begin
  Result := 0;
  if IsWidgetOk then
    PGtkSpinButton(Widget)^.get_range(@Result ,@AFloat);
end;

function TGtk3SpinEdit.GetNumDigits: Integer;
begin
  Result := 0;
  if IsWidgetOk then
    Result := Integer(PGtkSpinButton(Widget)^.get_digits);
end;

function TGtk3SpinEdit.GetNumeric: Boolean;
begin
  Result := False;
  if IsWidgetOk then
    Result := PGtkSpinButton(Widget)^.get_numeric;
end;

function TGtk3SpinEdit.GetStep: Double;
var
  AFloat: Double;
begin
  Result := 0;
  if IsWidgetOk then
    PGtkSpinButton(Widget)^.get_increments(@Result, @AFloat);
end;

function TGtk3SpinEdit.GetValue: Double;
begin
  Result := 0;
  if IsWidgetOk then
    Result := PGtkSpinButton(Widget)^.get_value;
end;

procedure TGtk3SpinEdit.SetNumDigits(AValue: Integer);
begin
  if IsWidgetOk then
    PGtkSpinButton(Widget)^.set_digits(GUint(AValue));
end;

procedure TGtk3SpinEdit.SetNumeric(AValue: Boolean);
begin
  if IsWidgetOk then
    PGtkSpinButton(Widget)^.set_numeric(AValue);
end;

procedure TGtk3SpinEdit.SetStep(AValue: Double);
var
  AStep: gdouble;
  APage: gdouble;
begin
  if IsWidgetOk then
  begin
    PGtkSpinButton(Widget)^.get_increments(@AStep, @APage);
    PGtkSpinButton(Widget)^.set_increments(AValue, APage);
  end;
end;

procedure TGtk3SpinEdit.SetValue(AValue: Double);
begin
  if IsWidgetOk then
  begin
    PGtkSpinButton(Widget)^.set_value(AValue);
  end;
end;

function TGtk3SpinEdit.CreateWidget(const Params: TCreateParams): PGtkWidget;
var
  ASpin: TCustomSpinEdit;
begin
  PrivateCursorPos := -1;
  PrivateSelection := -1;
  ASpin := TCustomSpinEdit(LCLObject);
  FWidgetType := FWidgetType + [wtSpinEdit];
  Result := LCLGtkSpinButtonNew;
  PGtkSpinButton(Result)^.set_range(ASpin.MinValue, ASpin.MaxValue);
  PGtkSpinButton(Result)^.set_increments(ASpin.Increment, ASpin.Increment * 10); //page param gtk default is 10 * step.
end;

function TGtk3SpinEdit.EatArrowKeys(const AKey: Word): Boolean;
begin
  Result := False;
end;

class procedure TGtk3SpinEdit.SpinValueChanged(aSpin:PGtkSpinButton;aData:
  gpointer);cdecl;
var
  Msg: TLMessage;
begin
  FillChar(Msg{%H-}, SizeOf(Msg), 0);
  Msg.Msg := CM_TEXTCHANGED;
  if not TGtk3Widget(AData).InUpdate then
    TGtk3Widget(AData).DeliverMessage(Msg);
end;

procedure TGtk3SpinEdit.InitializeWidget;
begin
  inherited InitializeWidget;
  g_signal_connect_data(Widget, 'value-changed', TGCallback(@SpinValueChanged), Self, nil, G_CONNECT_DEFAULT);
end;

function TGtk3SpinEdit.IsWidgetOk: Boolean;
begin
  Result := (Widget <> nil) and Gtk3IsSpinButton(Widget);
end;

procedure TGtk3SpinEdit.SetRange(AMin, AMax: Double);
begin
  if IsWidgetOk then
    PGtkSpinButton(Widget)^.set_range(AMin, AMax);
end;

{ TGtk3Range }

class procedure TGtk3Range.RangeChanged({%H-}ARange: PGtkRange; AData: gPointer); cdecl;
var
  Msg: TLMessage;
begin
  if AData <> nil then
  begin
    if TGtk3Widget(AData).InUpdate then
      Exit;
    FillChar(Msg{%H-}, SizeOf(Msg), #0);
    Msg.Msg := LM_CHANGED;
    TGtk3Widget(AData).DeliverMessage(Msg);
  end;
end;

function TGtk3Range.GetPosition: Integer;
begin
  Result := 0;
  if IsWidgetOK then
    Result := Round(PGtkRange(Widget)^.get_value);
end;

function TGtk3Range.GetRange: TPoint;
begin
  Result := Point(0, 0);
  if IsWidgetOK then
    PGtkRange(Widget)^.get_slider_range(@Result.X, @Result.Y);
end;

procedure TGtk3Range.SetPosition(AValue: Integer);
begin
  if IsWidgetOK then
    PGtkRange(Widget)^.set_value(gDouble(AValue));
end;

procedure TGtk3Range.SetRange(AValue: TPoint);
var
  dx,dy: gdouble;
begin
  if IsWidgetOK then
  begin
    dx := AValue.X;
    dy := AValue.Y;
    PGtkRange(Widget)^.set_range(dx, dy);
  end;
end;

procedure TGtk3Range.InitializeWidget;
begin
  inherited InitializeWidget;
  g_signal_connect_data(GetContainerWidget, 'value-changed', TGCallback(@RangeChanged), Self, nil, G_CONNECT_DEFAULT);
end;

procedure TGtk3Range.SetStep(AStep: Integer; APageSize: Integer);
begin
  if IsWidgetOk then
    PGtkRange(Widget)^.set_increments(gDouble(AStep), gDouble(APageSize));
end;

{ TGtk3TrackBar }

function TGtk3TrackBar.GetReversed: Boolean;
begin
  Result := False;
  if IsWidgetOK then
    Result := PGtkScale(Widget)^.get_inverted;
end;

procedure TGtk3TrackBar.SetReversed(AValue: Boolean);
begin
  if IsWidgetOK then
    PGtkScale(Widget)^.set_inverted(AValue);
end;

function TGtk3TrackBar.CreateWidget(const Params: TCreateParams): PGtkWidget;
var
  ATrack: TCustomTrackBar;
begin
  ATrack := TCustomTrackBar(LCLObject);
  FWidgetType := FWidgetType + [wtTrackBar];

 { Result := TGtkHBox.new(1,0);
  fCentralWidget:=PGtkWidget(TGtkScale.new(Ord(ATrack.Orientation), nil));
  PgtkBox(Result)^.add(fCentralWidget);}

  Result :=PGtkWidget(TGtkScale.new(TGtkOrientation(ATrack.Orientation), nil));

  FOrientation := ATrack.Orientation;
  if ATrack.Reversed then
    PGtkScale(Result)^.set_inverted(True);
  PGtkScale(Result)^.set_digits(0);
end;

procedure TGtk3TrackBar.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  Widget^.set_size_request(AWidth,AHeight);
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

function TGtk3TrackBar.GetTrackBarOrientation: TTrackBarOrientation;
begin
  Result := FOrientation;
end;

procedure TGtk3TrackBar.SetScalePos(AValue: TTrackBarScalePos);
begin
  if IsWidgetOK then
    PGtkScale(Widget)^.set_value_pos(TGtkPositionType(AValue));
end;

procedure TGtk3TrackBar.SetTickMarks(AValue: TTickMark; ATickStyle: TTickStyle);
var
  i,cnt,fldw: Integer;
  Track:TCustomTrackbar;
const
  tick_map:array[TTrackBarOrientation,0..1] of TGtkPositionType =
    ((GTK_POS_TOP,GTK_POS_BOTTOM), // trHorizontal
     (GTK_POS_LEFT,GTK_POS_RIGHT) // trVertical
    );
begin
  if IsWidgetOK then
  begin
    PGtkScale(Widget)^.set_draw_value(ATickStyle <> tsNone);
    if ATickStyle = tsNone then
      PGtkScale(Widget)^.clear_marks
    else
    begin
      PGtkScale(Widget)^.clear_marks;
      Track:=TCustomTrackbar(LCLObject);
      cnt:=round(abs(Track.max-Track.min)/Track.LineSize);
      // highly-dense marks just enlarge GtkScale automatically
      // it is up to user concent to do this
      if Track.Orientation=trHorizontal then
        fldw:=Track.Width
      else
        fldw:=Track.Height;
      if cnt*Track.LineSize<fldw then
      for i := Track.Min to Track.Max do
      begin
        if AValue in [tmBoth, tmTopLeft] then
          PGtkScale(Widget)^.add_mark(gDouble(i), tick_map[Track.Orientation,0], nil);
        if AValue in [tmBoth, tmBottomRight] then
          PGtkScale(Widget)^.add_mark(gDouble(i), tick_map[Track.Orientation,1], nil);
      end;
    end;
  end;
end;

{ TGtk3ScrollBar }
class procedure TGtk3ScrollBar.ScrollBarValueChanged(adjustment:PGtkAdjustment;data:
  gpointer);cdecl;
var
  scr: TScrollBar;
begin
  scr := TScrollbar(TGtk3Widget(data).LCLObject);
  {$note this must be fixed. Do not call LCL directly from here but send proper messages}
  scr.SetParams(
     round(adjustment^.value),
     round(adjustment^.lower),
     round(adjustment^.upper),
     round(adjustment^.page_size));
end;

function TGtk3ScrollBar.CreateWidget(const Params: TCreateParams): PGtkWidget;
var
  AScrollbar: TCustomScrollBar;
  ARange: PGtkRange;
begin
  AScrollBar := TCustomScrollBar(LCLObject);
  FWidgetType := FWidgetType + [wtScrollBar];
  Result := TGtkScrollbar.new(TGtkOrientation(AScrollBar.Kind), nil);
  ARange := PGtkRange(Result);
  with AScrollBar do
  begin
    ARange^.adjustment^.configure(Position, Min, Max + PageSize,
      SmallChange, LargeChange, PageSize);
    ARange^.adjustment^.set_value(Position);
    g_signal_connect_data(ARange^.adjustment,
         'value-changed', TGCallback(@ScrollBarValueChanged), Self, nil, G_CONNECT_DEFAULT);
  end;
end;

procedure TGtk3ScrollBar.SetParams;
var
  ARange: PGtkRange;
begin
  if not IsWidgetOk then
    exit;
  ARange := PGtkRange(Widget);
  with TCustomScrollbar(LCLObject) do
  begin
    ARange^.adjustment^.configure(Position, Min, Max,
      SmallChange, LargeChange, PageSize);
    ARange^.adjustment^.set_value(Position);
    ARange^.adjustment^.changed;
  end;
end;

{ TGtk3Calendar }

function TGtk3Calendar.CreateWidget(const Params: TCreateParams): PGtkWidget;
begin
  FWidgetType := [wtWidget, wtCalendar];
  Result := TGtkFrame.new(nil);
  FCentralWidget := TGtkCalendar.new;
  PGtkContainer(Result)^.add(FCentralWidget);
  FCentralWidget^.set_can_focus(True);
end;

procedure TGtk3Calendar.GetDate(out AYear, AMonth, ADay: LongWord);
begin
  AYear := 0;
  AMonth := 0;
  ADay := 0;
  if IsWidgetOk then
    PGtkCalendar(GetContainerWidget)^.get_date(@AYear, @AMonth, @ADay);
end;

procedure TGtk3Calendar.SetDate(const AYear, AMonth, ADay: LongWord);
begin
  if IsWidgetOK then
  begin
    PGtkCalendar(GetContainerWidget)^.select_month(AMonth, AYear);
    PGtkCalendar(GetContainerWidget)^.select_day(ADay);
  end;
end;

procedure TGtk3Calendar.SetDisplayOptions(
  const ADisplayOptions: TGtkCalendarDisplayOptions);
begin
  if IsWidgetOK then
    PGtkCalendar(GetContainerWidget)^.set_display_options(ADisplayOptions);
end;

{ TGtk3StaticText }

function TGtk3StaticText.GetAlignment: TAlignment;
var
  X: gfloat;
  Y: gfloat;
begin
  Result := taLeftJustify;
  if IsWidgetOK then
  begin
    PGtkLabel(GetContainerWidget)^.get_alignment(@X, @Y);
    if X = 1 then
      Result := taRightJustify
    else
    if X = 0.5 then
      Result := taCenter;
  end;
end;

function TGtk3StaticText.GetStaticBorderStyle: TStaticBorderStyle;
var
  AShadowType: TGtkShadowType;
begin
  Result := sbsNone;
  if IsWidgetOK then
  begin
    AShadowType := PGtkFrame(Widget)^.get_shadow_type;
    if AShadowType = GTK_SHADOW_ETCHED_IN then
      Result := sbsSingle
    else
    if AShadowType = GTK_SHADOW_IN then
      Result := sbsSunken;
  end;
end;

procedure TGtk3StaticText.SetAlignment(AValue: TAlignment);
begin
  if IsWidgetOk then
    PGtkLabel(GetContainerWidget)^.set_alignment(AGtkJustificationF[AValue], 0);
end;

procedure TGtk3StaticText.SetStaticBorderStyle(AValue: TStaticBorderStyle);
begin
  if IsWidgetOK then
    PGtkFrame(Widget)^.set_shadow_type(StaticBorderShadowMap[AValue]);
end;

function TGtk3StaticText.getText: String;
begin
  Result := '';
  if IsWidgetOk then
    Result := PGtkLabel(getContainerWidget)^.get_text;
end;

procedure TGtk3StaticText.setText(const AValue: String);
begin
  if IsWidgetOk then
    PGtkLabel(getContainerWidget)^.set_text(PgChar(AValue));
end;

function TGtk3StaticText.CreateWidget(const Params: TCreateParams): PGtkWidget;
var
  AStaticText: TCustomStaticText;
begin
  FWidgetType := FWidgetType + [wtStaticText];
  AStaticText := TCustomStaticText(LCLObject);
  Result := TGtkFrame.new('');
  PGtkFrame(Result)^.set_shadow_type(StaticBorderShadowMap[AStaticText.BorderStyle]);
  FCentralWidget := TGtkLabel.new('');
  FCentralWidget^.set_has_window(True);
  PGtkFrame(Result)^.set_label_widget(nil);
  PGtkFrame(Result)^.add(FCentralWidget);
  PGtkLabel(FCentralWidget)^.set_alignment(AGtkJustificationF[AStaticText.Alignment], 0.0);
end;

{ TGtk3ProgressBar }

function TGtk3ProgressBar.GetOrientation: TProgressBarOrientation;
var
  AOrientation: TGtkOrientation;
begin
  Result := pbHorizontal;
  if IsWidgetOk then
  begin
    AOrientation := PGtkOrientable(getContainerWidget)^.get_orientation;
    if AOrientation = GTK_ORIENTATION_HORIZONTAL then
    begin
      if PGtkProgressBar(getContainerWidget)^.get_inverted then
        Result := pbRightToLeft
      else
        Result := pbHorizontal;
    end else
    begin
      if PGtkProgressBar(getContainerWidget)^.get_inverted then
        Result := pbTopDown
      else
        Result := pbVertical;
    end;
  end;
end;

function TGtk3ProgressBar.GetPosition: Integer;
begin
  Result := 0;
  if IsWidgetOk then
    Result := Round(PGtkProgressBar(GetContainerWidget)^.get_fraction);
end;

function TGtk3ProgressBar.GetShowText: Boolean;
begin
  Result := False;
  if IsWidgetOK then
    Result := PGtkProgressBar(GetContainerWidget)^.get_show_text;
end;

function TGtk3ProgressBar.GetStyle: TProgressBarStyle;
begin
  Result := pbstNormal;
  if Assigned(LCLObject) and IsWidgetOk then
    Result := TCustomProgressBar(LCLObject).Style;
end;

procedure TGtk3ProgressBar.SetOrientation(AValue: TProgressBarOrientation);
begin
  if IsWidgetOk then
  begin
    case AValue of
      pbHorizontal,pbRightToLeft:
      begin
        PGtkOrientable(GetContainerWidget)^.set_orientation(GTK_ORIENTATION_HORIZONTAL);
        PGtkProgressBar(GetContainerWidget)^.set_inverted(AValue = pbRightToLeft);
      end;
      pbVertical, pbTopDown:
      begin
        PGtkOrientable(GetContainerWidget)^.set_orientation(GTK_ORIENTATION_VERTICAL);
        PGtkProgressBar(GetContainerWidget)^.set_inverted(AValue = pbVertical);
      end;
    end;
  end;
end;

procedure TGtk3ProgressBar.SetPosition(AValue: Integer);
var
  ABar: TCustomProgressBar;
  fraction: gDouble;
begin
  if not Assigned(LCLObject) or not IsWidgetOK then
    exit;
  ABar := TCustomProgressBar(LCLObject);
  if ((ABar.Max - ABar.Min) <> 0) then
    fraction := (AValue - ABar.Min) / (ABar.Max - ABar.Min)
  else
    fraction := 0;
  PGtkProgressBar(GetContainerWidget)^.set_fraction(fraction);
end;

procedure TGtk3ProgressBar.SetShowText(AValue: Boolean);
begin
  if IsWidgetOK then
    PGtkProgressBar(GetContainerWidget)^.set_show_text(AValue);
end;

function ProgressPulseTimeout(data: gpointer): gboolean; cdecl;
begin
  Result := {%H-}PtrUInt(g_object_get_data(data, 'lclprogressbarstyle')) = 1;
  if Result then
    PGtkProgressBar(Data)^.pulse;
end;

procedure ProgressDestroy(data: gpointer); cdecl;
begin
  g_source_remove({%H-}PtrUInt(data));
end;

procedure TGtk3ProgressBar.SetStyle(AValue: TProgressBarStyle);
begin
  if IsWidgetOk then
  begin
    g_object_set_data(GetContainerWidget,'lclprogressbarstyle', {%H-}Pointer(PtrUInt(Ord(AValue))));
    if AValue = pbstNormal then
    begin
      Position := TCustomProgressBar(LCLObject).Position;
    end else
    begin
      g_object_set_data_full(GetContainerWidget, 'timeout',
        {%H-}Pointer(PtrUInt(g_timeout_add(100, @ProgressPulseTimeout, GetContainerWidget))), @ProgressDestroy);
      PGtkProgressBar(GetContainerWidget)^.pulse;
    end;
  end;
end;

{we must override preferred width since gtk3 have strange opinion about minimum width of progress bar}
procedure get_progress_preferred_width(widget: PGtkWidget; minimum_width: Pgint; natural_width: Pgint); cdecl;
var
  Handle: HWND;
begin
  Handle := HwndFromGtkWidget(Widget);
  if Handle <> 0 then
  begin
    minimum_width^ := TGtk3Widget(Handle).LCLObject.Width;
    natural_width^ := TGtk3Widget(Handle).LCLObject.Width;
  end else
  begin
    minimum_width^ := 0;
    natural_width^ := 0;
    DebugLn('ERROR: get_progress_preferred_width cannot find GtkWidget LCL Handle ....');
  end;
end;

{we must override preferred height since gtk3 have strange opinion about height of progress bar}
procedure get_progress_preferred_height(widget: PGtkWidget; minimum_height: Pgint; natural_height: Pgint); cdecl;
var
  Handle: HWND;
begin
  Handle := HwndFromGtkWidget(Widget);
  if Handle <> 0 then
  begin
    minimum_height^ := TGtk3Widget(Handle).LCLObject.Height;
    natural_height^ := TGtk3Widget(Handle).LCLObject.Height;
    // TODO: get spacing from style property
    // Widget^.get_style_context^.get_style_property();
  end else
  begin
    minimum_height^ := 0;
    natural_height^ := 0;
    DebugLn('ERROR: get_progress_preferred_height cannot find GtkWidget LCL Handle ....');
  end;
end;

function TGtk3ProgressBar.CreateWidget(const Params: TCreateParams): PGtkWidget;
var
  AProgress: TCustomProgressBar;
begin
  AProgress := TCustomProgressBar(LCLObject);
  if AProgress=nil then ;
  FWidgetType := FWidgetType + [wtProgressBar];
  Result := TGtkEventBox.new;
  FCentralWidget := TGtkProgressBar.new;
  PGtkEventBox(Result)^.add(FCentralWidget);
  FCentralWidget^.set_can_focus(True);
end;

var
  AProgressClassHookInitialized: Boolean = False;

procedure TGtk3ProgressBar.InitializeWidget;
var
  AClass: PGTypeClass;
begin
  inherited InitializeWidget;
  //TODO: move hook check variable code into Gtk3WidgetSet.
  if not AProgressClassHookInitialized then
  begin
    AProgressClassHookInitialized := True;
    AClass := g_type_class_ref(gtk_progress_bar_get_type);
    PGtkWidgetClass(AClass)^.get_preferred_width := @get_progress_preferred_width;
    PGtkWidgetClass(AClass)^.get_preferred_height := @get_progress_preferred_height;
    g_type_class_unref(AClass);
  end;
end;

{ TGtk3Container }

function disableMouseButtonEvent(widget: PGtkWidget; event: PGdkEvent; user_data: gpointer): gboolean; cdecl;
var
  AEvent: TGdkEvent;
begin
  Result := TGtk3Widget(user_data).GtkEventMouse(Widget, Event);
  {TODO: check if we can block button_press also}
  if event^.type_ = GDK_BUTTON_RELEASE then
    Result := True;
end;

function motionNotifyEvent(widget: PGtkWidget; event: PGdkEvent; user_data: gpointer): gboolean; cdecl;
begin
  TGtk3Widget(user_data).GtkEventMouseMove(widget, event);
  Result := True;
end;

procedure TGtk3Container.SetVisible(AValue: Boolean);
begin
  if not AValue then
  begin
    if [wtNotebook, wtWindow] * WidgetType = [] then
    begin
      Widget^.set_no_show_all(True);
      Widget^.hide;
    end;
  end;
  inherited SetVisible(AValue);
end;

procedure TGtk3Container.InitializeWidget;
begin
  inherited InitializeWidget;
  if IsDesigning then
  begin
    (*
    if (Widget <> getContainerWidget) then
    begin
      //Widget^.set_events([GDK_BUTTON_PRESS_MASK, GDK_BUTTON_RELEASE_MASK]);
      g_signal_connect_data(Widget, 'button-press-event', TGCallback(@disableMouseButtonEvent), Self, Nil, G_CONNECT_DEFAULT);
      g_signal_connect_data(Widget, 'button-release-event', TGCallback(@disableMouseButtonEvent), Self, Nil, G_CONNECT_DEFAULT);
      g_signal_connect_data(Widget, 'motion-notify-event', TGCallback(@motionNotifyEvent), Self, nil, G_CONNECT_DEFAULT);
    end;
    *)
    g_signal_connect_data(GetContainerWidget, 'button-press-event', TGCallback(@disableMouseButtonEvent), Self, Nil, G_CONNECT_DEFAULT);
    g_signal_connect_data(GetContainerWidget, 'button-release-event', TGCallback(@disableMouseButtonEvent), Self, Nil, G_CONNECT_DEFAULT);
    g_signal_connect_data(GetContainerWidget, 'motion-notify-event', TGCallback(@motionNotifyEvent), Self, nil, G_CONNECT_DEFAULT);
  end;
end;

{ TGtk3Page }

procedure TGtk3Page.DoBeforeLCLPaint;
var
  DC: TGtk3DeviceContext;
  NColor: TColor;
begin
  inherited DoBeforeLCLPaint;
  if not Visible then
    exit;
  DC := TGtk3DeviceContext(Context);
  NColor := LCLObject.Color;
  if (NColor <> clNone) and (NColor <> clDefault) then
  begin
    DC.CurrentBrush.Color := ColorToRGB(NColor);
    DC.fillRect(0, 0, LCLObject.Width, LCLObject.Height);
  end;
end;

procedure TGtk3Page.setText(const AValue: String);
var
  bs:string;
begin
  inherited;
  if Assigned(FPageLabel) then
  begin
    bs:=ReplaceAmpersandsWithUnderscores(Avalue);
    FPageLabel^.set_markup_with_mnemonic(PChar(bs));
  end;
end;

function TGtk3Page.GetCloseButtonVisible: boolean;
begin
  Result := Assigned(FCloseButton) and FCloseButton^.is_visible;
end;

procedure TGtk3Page.SetCloseButtonVisible(AValue: boolean);
begin
  if Assigned(FCloseButton) then
    FCloseButton^.set_visible(AValue);
end;

class procedure TGtk3Page.TabSheetLayoutSizeAllocate(
  AWidget: PGtkWidget; AGdkRect: PGdkRectangle; Data: gpointer); cdecl;
var
  HSize,VSize: integer;
  uWidth, uHeight: guint;
begin
  HSize := AGdkRect^.Width;
  VSize := AGdkRect^.Height;

  PGtkLayout(aWidget)^.get_size(@uWidth, @uHeight);
  if (uWidth <> HSize) or (uHeight <> VSize) then
    PGtkLayout(aWidget)^.set_size(HSize, VSize);

  if TGtk3Widget(Data).LCLObject.ClientRectNeedsInterfaceUpdate then
    TGtk3Widget(Data).LCLObject.DoAdjustClientRectChange;
end;

class procedure TGtk3Page.TabCloseClicked(aButton: PGtkButton; aData: gPointer); cdecl;
begin
  with TCustomTabControl(TCustomPage(TGtk3Page(aData).LCLObject).Parent) do
    DoCloseTabClicked(TCustomPage(TGtk3Page(aData).LCLObject));
end;

function TGtk3Page.CreateWidget(const Params: TCreateParams): PGtkWidget;
var
  image: PGtkImage;
begin
  FWidgetType := FWidgetType + [wtLayout];
  FPageBox := TGtkBox.new(GTK_ORIENTATION_HORIZONTAL, 4);

  FImageWidget := TGtkImage.new;

  FPageLabel:= TGtkLabel.new(PChar(Params.Caption));
  FPageLabel^.set_use_underline(true);
  image := gtk_image_new_from_icon_name('window-close', GTK_ICON_SIZE_MENU);
  FCloseButton := gtk_button_new();
  gtk_button_set_relief(PGtkButton(FCloseButton), GTK_RELIEF_NONE);
  gtk_container_add(PGtkContainer(FCloseButton), image);
  gtk_widget_set_name(FCloseButton, 'tab-close-button'); // optional styling via css.

  FPageBox^.pack_start(FImageWidget, False, False, 0);
  FPageBox^.pack_start(FPageLabel, False, False, 0);
  FPageBox^.pack_start(FCloseButton, False, False, 0);
  FPageBox^.show_all;

  FImageWidget^.hide;

  Self.FHasPaint:=true;
  // ref it to save it in case TabVisible is set to false
  FPageBox^.ref;

  Result := TGtkBox.new(GTK_ORIENTATION_HORIZONTAL, 0);
  FCentralWidget := TGtkLayout.new(nil, nil);
  FCentralWidget^.set_app_paintable(True);
  PGtkBox(Result)^.pack_start(FCentralWidget, True , True, 0);
  FCentralWidget^.set_has_window(True);
  if not (csDesigning in LCLObject.ComponentState) then
    g_object_set(PGObject(FCentralWidget), 'resize-mode', [GTK_RESIZE_QUEUE, nil]);
  gtk_layout_set_size(PGtkLayout(FCentralWidget), 1, 1);

  g_signal_connect_data(FCentralWidget,'size-allocate',TGCallback(@TabSheetLayoutSizeAllocate), Self, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(FCloseButton, 'clicked', TGCallback(@TabCloseClicked), Self, nil, G_CONNECT_DEFAULT);
end;

procedure TGtk3Page.DestroyWidget;
begin
  // unref it to allow it to be destroyed
  FPageBox^.unref;
  inherited DestroyWidget;
end;

destructor TGtk3Page.Destroy;
begin
  g_idle_remove_by_data(fCentralWidget);
  inherited Destroy;
end;

function TGtk3Page.ClientToScreen(var P: TPoint): boolean;
var
  aParent: PGtkWidget;
  Alloc, Allocation: TGtkAllocation;
begin

  if Assigned(LCLObject.Parent) then
    Result := TGtk3Widget(LCLObject.Parent.Handle).ClientToScreen(P)
  else
    Result := inherited ClientToScreen(P);

  aParent := gtk_widget_get_parent(Widget);
  aParent^.get_allocation(@Alloc);
  Self.Widget^.get_allocation(@Allocation);

  P.X := P.X - (Alloc.X - Allocation.X);
  P.Y := P.Y - (Alloc.Y - Allocation.Y);
end;

function TGtk3Page.getClientOffset: TPoint;
begin
  Result := Point(0, 0);
end;

function TGtk3Page.getClientRect: TRect;
var
  AParent: PGtkWidget;
  AParentObject: TGtk3Widget;
begin
  Result := Rect(0, 0, 0, 0);
  if Assigned(LCLObject.Parent) and (LCLObject.Parent.HandleAllocated) then
  begin
    if not WidgetMapped then
    begin
      Result :=  TGtk3Widget(LCLObject.Parent.Handle).getClientRect;
      exit;
    end;
  end;

  if not WidgetMapped then
  begin
    AParent := Widget^.get_parent;
    AParentObject := TGtk3Widget(HwndFromGtkWidget(AParent));
    if AParentObject <> nil then
      Result := AParentObject.getClientRect
    else
      Result := inherited getClientRect;
  end else
    Result := inherited getClientRect;
  // DebugLn('TGtk3Page.GetClientRect Result=',dbgs(Result),' Realized ',dbgs(getContainerWidget^.get_realized));
end;

procedure TGtk3Page.setTabImage(aBitmap: TBitmap);
var
  APixBuf: PGdkPixbuf;
begin
  if Assigned(aBitmap) then
    APixBuf := TGtk3Image(aBitmap.Handle).Handle^.copy
  else
    aPixBuf := nil;
  if aPixBuf = nil then
  begin
    FImageWidget^.set_from_pixbuf(nil);
    FImageWidget^.set_visible(False);
  end else
  begin
    FImageWidget^.set_from_pixbuf(aPixBuf);
    FImageWidget^.set_visible(True);
    aPixbuf^.unref;
  end;
end;

{ TGtk3NoteBook }

function NotebookPageRealToLCLIndex(const ATabControl: TCustomTabControl; AIndex: integer): integer;
var
  I: Integer;
begin
  Result := AIndex;
  if csDesigning in ATabControl.ComponentState then exit;
  I := 0;
  while (I < ATabControl.PageCount) and (I <= Result) do
  begin
    if not ATabControl.Page[I].TabVisible then Inc(Result);
    Inc(I);
  end;
end;

// function GtkNotebookAfterSwitchPage(widget: PGtkWidget; pagenum: integer; data: gPointer): GBoolean; cdecl;
procedure GtkNotebookAfterSwitchPage(widget: PGtkWidget; {%H-}page: PGtkWidget; pagenum: integer; data: gPointer); cdecl;
var
  Mess: TLMNotify;
  NMHdr: tagNMHDR;
  LCLPageIndex: Integer;
begin
  if widget=nil then ;
  if TGtk3Widget(Data).InUpdate then
    exit;
  {page is deleted}
 { DebugLn('GtkNotebookAfterSwitchPage ');
  if TGtk3NoteBook(Data).getPagesCount < TCustomTabControl(TGtk3NoteBook(Data).LCLObject).PageCount then
  begin
    DebugLn('GtkNotebookAfterSwitchPage PageIsDeleted');
    exit;
  end;}
  FillChar(Mess{%H-}, SizeOf(Mess), 0);
  Mess.Msg := LM_NOTIFY;
  FillChar(NMHdr{%H-}, SizeOf(NMHdr), 0);
  NMHdr.code := TCN_SELCHANGE;
  NMHdr.hwndFrom := HWND(TGtk3Widget(Data));
  LCLPageIndex := NotebookPageRealToLCLIndex(TCustomTabControl(TGtk3Widget(Data).LCLObject), pagenum);  //use this to set pageindex to the correct page.
  NMHdr.idFrom := LCLPageIndex;
  Mess.NMHdr := @NMHdr;
  TGtk3Widget(Data).DeliverMessage(Mess);
end;

function BackNoteBookSignal(AData: Pointer): gboolean; cdecl;
var
  AWidget: PGtkNotebook;
  APageNum: PtrInt;
  ACurrentPage: gint;
begin
  Result := False;
  AWidget := PGtkNoteBook(AData);
  if not Gtk3IsWidget(AWidget) then
    exit;
  if g_object_get_data(AWidget,'switch-page-signal-stopped') <> nil then
  begin
    Result := True;
    APageNum := {%H-}PtrInt(g_object_get_data(AWidget,'switch-page-signal-stopped'));
    ACurrentPage := AWidget^.get_current_page;
    g_object_set_data(AWidget,'switch-page-signal-stopped', nil);
    {$IFDEF GTK3DEBUGNOTEBOOK}
    DebugLn('BackNoteBookSignal back notebook switch-page signal currpage=',dbgs(AWidget^.get_current_page),' blockedPage ',dbgs(APageNum));
    {$ENDIF}
    if ACurrentPage<0 then ;
    // must hook into notebook^.priv to unlock APageNum
    // AWidget^.set_current_page(AWidget^.get_current_page);
    // g_object_thaw_notify(AWidget^.get_nth_page(AWidget^.get_current_page));
    // PGtkFixed(AWidget^.get_nth_page(AWidget^.get_current_page))^.
    // g_signal_emit_by_name(AWidget,'switch-page',[AWidget^.get_nth_page(APageNum), APageNum, gPointer(HwndFromGtkWidget(AWidget)), nil{AWidget, True, gPointer(HwndFromGtkWidget(AWidget))}]);
    // AWidget^.notify('page');
    // g_signal_stop_emission_by_name(AWidget, 'switch-page');
    // g_signal_emit_by_name(AWidget,'switch-page',[G_TYPE_NONE, AWidget, AWidget^.get_nth_page(AWidget^.get_current_page), AWidget^.get_current_page, gPointer(HwndFromGtkWidget(AWidget))]);
  end;
  g_idle_remove_by_data(AData);
end;

procedure GtkNotebookSwitchPage(widget: PGtkWidget; {%H-}page: PGtkWidget; pagenum: integer; data: gPointer); cdecl;
var
  Mess: TLMNotify;
  NMHdr: tagNMHDR;
  c1,c2:integer;
begin
  if TGtk3Widget(Data).InUpdate then
    exit;
  {$IFDEF GTK3DEBUGNOTEBOOK}
  DebugLn('GtkNotebookSwitchPage Data ',dbgHex({%H-}PtrUInt(Data)),' Realized ',dbgs(Widget^.get_realized),' pageNum=',dbgs(pageNum));
  {$ENDIF}

  {page is deleted}
 { c1:=TGtk3NoteBook(Data).getPagesCount;
  c2:=TCustomTabControl(TGtk3NoteBook(Data).LCLObject).PageCount;
  if c1 < c2 then
  begin
    DebugLn('GtkNotebookSwitchPage PageIsDeleted ');
    exit;
  end;}

  FillChar(Mess{%H-}, SizeOf(Mess), 0);
  Mess.Msg := LM_NOTIFY;
  FillChar(NMHdr{%H-}, SizeOf(NMHdr), 0);
  NMHdr.code := TCN_SELCHANGING;
  NMHdr.hwndFrom := HWND(TGtk3Widget(Data));
  NMHdr.idFrom := NotebookPageRealToLCLIndex(TCustomTabControl(TGtk3Widget(Data).LCLObject), pagenum);  //use this to set pageindex to the correct page.
  // DebugLn('LCLObject ',dbgsName(TGtk3Widget(Data).LCLObject),' IdFrom ',dbgs(NMHdr.idFrom));
  Mess.NMHdr := @NMHdr;
  Mess.Result := 0;
  // DebugLn('LCLObject ',dbgsName(TGtk3Widget(Data).LCLObject),' sending message ....');
  TGtk3Widget(Data).DeliverMessage(Mess);
  if Mess.Result <> 0 then
  begin
    g_object_set_data(Widget,'switch-page-signal-stopped', {%H-}GPointer(pageNum));
    g_signal_stop_emission_by_name(PGObject(Widget), 'switch-page');
    // GtkNotebookAfterSwitchPage(Widget, page, pagenum, data);
    g_idle_add(@BackNoteBookSignal, Widget);
    Exit;
  end;
end;

function TGtk3NoteBook.CreateWidget(const Params: TCreateParams): PGtkWidget;
var
  Alloc:TGtkAllocation;
begin
  FWidgetType := FWidgetType + [wtNotebook];
  Result := LCLGtkNotebookNew;
  FCentralWidget := Result;

  if (nboHidePageListPopup in TCustomTabControl(LCLObject).Options) then
    PGtkNoteBook(FCentralWidget)^.popup_disable
  else
    PGtkNoteBook(FCentralWidget)^.popup_enable;

  Alloc.x := Params.X;
  Alloc.y := Params.Y;
  Alloc.Width := Params.Width;
  Alloc.Height := Params.Height;

  g_signal_connect_data(FCentralWidget,'switch-page', TGCallback(@GtkNotebookSwitchPage), Self, nil, G_CONNECT_DEFAULT);
  // this one triggers after above switch-page
  g_signal_connect_data(FCentralWidget,'switch-page', TGCallback(@GtkNotebookAfterSwitchPage), Self, nil, G_CONNECT_DEFAULT);
  PGtkNotebook(Result)^.set_scrollable(True);

  FCentralWidget^.show_all;
  FCentralWidget^.size_allocate(@Alloc);
end;

procedure TGtk3NoteBook.InitializeWidget;
begin
  FDefaultClientRect := Rect(0, 0, 0, 0);
  inherited;
  SetTabPosition(TCustomTabControl(LCLObject).TabPosition);
end;

function TGtk3NoteBook.GetTabSize(AWinControl:TWinControl):integer;
var
  AWidget: PGtkWidget;
  Alloc:TGtkAllocation;
  APage:PGtkWidget;
  APageAlloc:TGtkAllocation;
  R:TRect;
begin
  Result := 0;
  if not WidgetMapped then
    Result := DefaultClientRect.Height
  else
  begin
    R := getClientRect;
    if PGtkNotebook(GetContainerWidget)^.tab_pos in [GTK_POS_TOP, GTK_POS_BOTTOM] then
      Result := GetContainerWidget^.get_allocated_height - R.Height
    else
      Result := GetContainerWidget^.get_allocated_width - R.Width;
  end;
end;

function TGtk3NoteBook.getClientRect: TRect;
var
  AAlloc: TGtkAllocation;
  ACurrentPage: gint;
  APage: PGtkWidget;
  ATabSheet:HWND;
begin
  Result := Rect(0, 0, 0, 0);
  ACurrentPage := -1;
  if not WidgetMapped then
  begin
    if not IsRectEmpty(FDefaultClientRect) then
      Result := DefaultClientRect
    else
      exit;
  end else
  if PGtkNoteBook(GetContainerWidget)^.get_n_pages = 0 then
  begin
    GetContainerWidget^.get_allocation(@AAlloc);
    Result := RectFromGtkAllocation(AAlloc);
    Types.OffsetRect(Result, -Result.Left, -Result.Top);
  end else
  begin
    ACurrentPage := PGtkNoteBook(GetContainerWidget)^.get_current_page;
    if (ACurrentPage >= 0) then
    begin
      APage := PGtkNoteBook(GetContainerWidget)^.get_nth_page(ACurrentPage);
      ATabSheet := HwndFromGtkWidget(APage);
      if (ATabSheet <> 0) and TGtk3Widget(ATabSheet).WidgetMapped then
        APage^.get_allocation(@AAlloc)
      else
        GetContainerWidget^.get_allocation(@AAlloc);
      Result := RectFromGtkAllocation(AAlloc);
      Types.OffsetRect(Result, -Result.Left, -Result.Top);
    end;
  end;
  // DebugLn('<TGtk3NoteBook.getClientRect Style Result ',dbgs(Result),' ACurrentPage=',ACurrentPage.ToString);
end;

function TGtk3NoteBook.getPagesCount: integer;
begin
  Result := 0;
  if IsWidgetOk then
    Result := PGtkNoteBook(GetContainerWidget)^.get_n_pages;
end;

//debugging
procedure EnumerateChildren(ANotebook: PGtkNoteBook);
var
  AList: PGList;
  i: Integer;
  AWidget: PGtkWidget;
  AMinimumH, ANaturalH, ANaturalW, AMinimumW: gint;
begin
  AList := ANoteBook^.get_children;
  for i := 0 to g_list_length(AList) - 1 do
  begin
    AWidget := PGtkWidget(g_list_nth_data(AList, I));
    AWidget^.get_preferred_height(@AMinimumH, @ANaturalH);
    AWidget^.get_preferred_width(@AMinimumW, @ANaturalW);
    DebugLn(Format('Child[%d] MinH %d NatH %d MinW %d NatW %d ALLOCW %d ALLOCH %d child_type %s',
      [I, AMinimumH, ANaturalH, AMinimumW, ANaturalW,
      AWidget^.get_allocated_width, AWidget^.get_allocated_height, g_type_name(ANotebook^.child_type)]));
  end;
  g_list_free(AList);
end;

procedure TGtk3NoteBook.InsertPage(ACustomPage: TCustomPage; AIndex: Integer);
var
  Gtk3Page: TGtk3Page;
  AMinSize, ANaturalSize: gint;
  Bmp: TBitmap;
  ImageIndex:integer;
  HasIcon: Boolean;
  IconSize: Types.TSize;
  TheNotebook:TCustomTabControl;
  Appi:integer;
begin
  if IsWidgetOK then
  begin
    Gtk3Page := TGtk3Page(ACustomPage.Handle);
    Gtk3Page.CloseButtonVisible := nboShowCloseButtons in TCustomTabControl(LCLObject).Options;

    TheNoteBook:=TCustomTabControl(LCLObject);
    HasIcon:=false;
    IconSize:=Size(0,0);
    ImageIndex := TheNoteBook.GetImageIndex(AIndex);
    Appi:=TheNoteBook.Font.PixelsPerInch;
    if (TheNoteBook.Images<>nil)
    and (ImageIndex >= 0)
    and (ImageIndex < TheNoteBook.Images.Count) then
    begin
      // page has valid image
      IconSize := TheNoteBook.Images.SizeForPPI[TheNoteBook.ImagesWidth, Appi];
      HasIcon := (IconSize.cx>0) and (IconSize.cy>0);
    end;
    if HasIcon then
    begin
      Bmp := TBitmap.Create;
      TCustomTabControl(LCLObject).Images.ResolutionForPPI[IconSize.cx,Appi,1].GetBitmap(ImageIndex, Bmp);
      Gtk3Page.setTabImage(Bmp);
      Bmp.Free;
    end else
      Gtk3Page.setTabImage(nil);
    with PGtkNoteBook(GetContainerWidget)^ do
      insert_page(Gtk3Page.Widget, Gtk3Page.FPageBox, AIndex);
  end;
end;

procedure TGtk3NoteBook.MovePage(ACustomPage: TCustomPage; ANewIndex: Integer);
begin
  if IsWidgetOK then
    PGtkNoteBook(GetContainerWidget)^.reorder_child(TGtk3Widget(ACustomPage.Handle).Widget, ANewIndex);
end;

procedure TGtk3NoteBook.RemovePage(AIndex: Integer);
var
  AMinSizeW, AMinSizeH, ANaturalSizeW, ANaturalSizeH: gint;
  NB: PGtkNotebook;
begin
  if IsWidgetOK then
  begin
    NB:=PGtkNotebook(GetContainerWidget);
    NB^.remove_page(AIndex);
    NB^.get_preferred_width(@AMinSizeW, @ANaturalSizeW);
    NB^.get_preferred_height(@AMinSizeH, @ANaturalSizeH);
    NB^.resize_children;
  end;
end;

procedure TGtk3NoteBook.SetPageIndex(AIndex: Integer);
begin
  if IsWidgetOK then
  begin
    PGtkNotebook(GetContainerWidget)^.set_current_page(AIndex);
  end;
end;

procedure TGtk3NoteBook.SetShowTabs(const AShowTabs: Boolean);
begin
  if IsWidgetOK then
    PGtkNoteBook(GetContainerWidget)^.set_show_tabs(AShowTabs);
end;

procedure TGtk3NoteBook.SetTabPosition(const ATabPosition: TTabPosition);
const
  GtkPositionTypeMap: array[TTabPosition] of TGtkPositionType =
  (
    { tpTop    } GTK_POS_TOP,
    { tpBottom } GTK_POS_BOTTOM,
    { tpLeft   } GTK_POS_LEFT,
    { tpRight  } GTK_POS_RIGHT
  );
begin
  if IsWidgetOK then
    PGtkNoteBook(GetContainerWidget)^.set_tab_pos(GtkPositionTypeMap[ATabPosition]);
end;

procedure TGtk3NoteBook.SetTabLabelText(AChild: TCustomPage; const AText: String);
begin
  if IsWidgetOK then
    TGtk3Widget(AChild.Handle).setText(AText);
end;

function TGtk3NoteBook.GetTabLabelText(AChild: TCustomPage): String;
begin
  if IsWidgetOK then
    Result := TGtk3Widget(AChild.Handle).getText
  else
    Result := '';
end;

{ TGtk3MenuShell }

procedure TGtk3MenuShell.Insert(AMenuShell: PGtkMenuShell; APosition: Integer);
begin
  if IsWidgetOK then
    PGtkMenuShell(Widget)^.insert(AMenuShell, APosition);
end;

constructor TGtk3MenuShell.Create(const AMenu: TMenu; AMenuBar: PGtkMenuBar);
begin
  inherited Create;
  MenuObject := AMenu;
  FCentralWidget := nil;
  if AMenuBar <> nil then
  begin
    FOwnWidget := False;
    FWidget := AMenuBar;
  end else
    FOwnWidget := True;
  // Initializes the properties
  // FKeysToEat := [VK_TAB, VK_RETURN, VK_ESCAPE];
  // FHasPaint := False;

  // FParams := AParams;
  InitializeWidget;
end;

procedure TGtk3MenuShell.InitializeWidget;
begin
  if FOwnWidget then
    FWidget := CreateWidget(FParams);

  LCLIntf.SetProp(HWND(Self),'lclwidget',Self);
end;


{ TGtk3MenuBar }

function TGtk3MenuBar.CreateWidget(const Params: TCreateParams): PGtkWidget;
begin
  FWidgetType := [wtWidget, wtMenuBar];
  Result := TGtkMenuBar.new;
  PGtkMenuBar(Result)^.set_pack_direction(MenuDirection[TMenu(MenuObject).UseRightToLeftAlignment]);
end;

{ TGtk3Menu }

function TGtk3Menu.CreateWidget(const Params: TCreateParams): PGtkWidget;
begin
  FWidgetType := [wtWidget, wtMenu];
  Result := TGtkMenu.new;
end;

constructor TGtk3Menu.CreateFromMenuItem(const AMenuItem: TMenuItem);
begin
  inherited Create(AMenuItem.GetParentMenu, nil);
end;

{ TGtk3MenuItem }

function TGtk3MenuItem.GetCaption: string;
begin
  Result := '';
  if IsWidgetOK then
    Result := PGtkMenuItem(FWidget)^.get_label;
end;

procedure TGtk3MenuItem.SetCaption(const AValue: string);
begin
  if IsWidgetOK then
    PGtkMenuItem(FWidget)^.set_label(PgChar(AValue));
end;

function TGtk3MenuItem.CreateWidget(const Params: TCreateParams): PGtkWidget;
var
  ndx:integer;
  pl:PGsList;
  parentMenu:TMenuItem;
  picon:PGtkImage;
  pmenu:PGtkMenuItem;
  pimgmenu:PgtkImageMenuItem absolute pmenu;
  img:TGtk3Image;
begin
  Result:=nil;
  FWidgetType := [wtWidget, wtMenuItem];
  if MenuItem.Caption = cLineCaption then
    Result := TGtkSeparatorMenuItem.new
  else
  if (MenuItem.HasIcon) then
  begin
    pimgmenu := TGtkImageMenuItem.new();
    MenuItem.UpdateImage(true);
    img:=Tgtk3Image(MenuItem.Bitmap.Handle);
    picon := TGtkImage.new_from_pixbuf(img.Handle);
    pimgmenu^.set_image(picon);
    pimgmenu^.set_always_show_image(true);
    Result:=pimgmenu;
  end else
  if MenuItem.RadioItem and not MenuItem.HasIcon then
  begin
    Result := TGtkRadioMenuItem.new(nil);
    if Assigned(menuItem.Parent) then
    begin
      ndx:=menuItem.Parent.IndexOf(MenuItem);
      if (ndx>0) then
      begin
        ParentMenu:=menuItem.Parent.Items[ndx-1];
        if (ParentMenu.GroupIndex=MenuItem.GroupIndex) then
        begin
          pl:=PGtkRadioMenuItem(TGtk3MenuItem(ParentMenu.Handle).Widget)^.get_group;
          PGtkRadioMenuItem(Result)^.set_group(pl);
        end;
      end;
    end;
    //PGtkRadioMenuItem(Result)^.set_active(MenuItem.Checked);
  end
  else
  if MenuItem.IsCheckItem and not MenuItem.HasIcon then
  begin
    Result := TGtkCheckMenuItem.new;
    PGtkCheckMenuItem(Result)^.set_active(MenuItem.Checked);
  end
  else
    Result := TGtkMenuItem.new;

  if Assigned(Result) and (MenuItem.Caption <> cLineCaption) {and not MenuItem.HasIcon} then
  begin
    PGtkMenuItem(Result)^.use_underline := True;
    PGtkMenuItem(Result)^.set_label(PgChar(ReplaceAmpersandsWithUnderscores(MenuItem.Caption)));
    PGtkMenuItem(Result)^.set_sensitive(MenuItem.Enabled);
  end;


end;

constructor TGtk3MenuItem.Create(const AMenuItem: TMenuItem);
begin
  inherited Create;
  MenuItem := AMenuItem;
  FOwnWidget := True;
  // Initializes the properties
  // FKeysToEat := [VK_TAB, VK_RETURN, VK_ESCAPE];
  // FHasPaint := False;

  // FParams := AParams;
  InitializeWidget;
end;

class function TGtk3MenuItem.MenuItemEvent({%H-}AWidget: PGtkWidget; event: PGdkEvent; {%H-}data: GPointer): gboolean; cdecl;
begin
  Result := False;

  if not Assigned(Application) or (Assigned(Application) and Application.Terminated) then
      exit;

  //DebugLn('TGtk3MenuItem.MenuItemEvent triggered ',dbgsName(TGtk3MenuItem(Data).MenuItem),
  //  ' ',Gtk3EventToStr(event^.type_));

  case event^.type_ of
  GDK_DELETE:
    begin
      // DebugLn('****** GDK_DELETE FOR ',dbgsName(TGtk3Widget(Data).LCLObject),' main_level=',dbgs(gtk_main_level));
    end;
  GDK_DESTROY:
    begin
     // DebugLn('****** GDK_DESTROY FOR ' + dbgsName(TGtk3Widget(Data).LCLObject));
    end;
  GDK_EXPOSE:
    begin
      // DebugLn('****** GDK_EXPOSE FOR ' + dbgsName(TGtk3Widget(Data).LCLObject));
      // TGtk3Widget.DrawWidget is attached to 'draw' signal, Expose event doesn't trigger
      // under gtk3.
       // we use 'draw' signal Gtk3DrawEvent()
      // Result := TGtk3Widget(Data).GtkEventPaint(Widget, Event);
    end;
  GDK_MOTION_NOTIFY:
    begin
      // Result := TGtk3Widget(Data).GtkEventMouseMove(Widget, Event);
    end;
  GDK_BUTTON_PRESS:
    begin
      // Result := TGtk3Widget(Data).GtkEventMouse(Widget , Event);
    end;
  GDK_2BUTTON_PRESS:
    begin
      // if not TGtk3Widget(Data).LCLObject.Focused and TGtk3Widget(Data).LCLObject.CanFocus then
      //  LCLIntf.SetFocus(HWND(TGtk3Widget(Data)));
      // Result := TGtk3Widget(Data).GtkEventMouse(Widget , Event);
    end;
  GDK_3BUTTON_PRESS:
    begin
      // if not TGtk3Widget(Data).LCLObject.Focused and TGtk3Widget(Data).LCLObject.CanFocus then
      //  LCLIntf.SetFocus(HWND(TGtk3Widget(Data)));
      // Result := TGtk3Widget(Data).GtkEventMouse(Widget , Event);
    end;
  GDK_BUTTON_RELEASE:
    begin
      // Result := TGtk3Widget(Data).GtkEventMouse(Widget , Event);
    end;
  GDK_KEY_PRESS:
    begin
      // if Widget^.has_focus then // or (Widget = TGtk3Widget(data).GetContainerWidget) then
      //  Result := TGtk3Widget(Data).GtkEventKey(Widget, Event, True);
    end;
  GDK_KEY_RELEASE:
    begin
      // if Widget^.has_focus then // or (Widget = TGtk3Widget(data).GetContainerWidget) then
      //  Result := TGtk3Widget(Data).GtkEventKey(Widget, Event, False);
    end;

  GDK_ENTER_NOTIFY:
    begin
      // TGtk3Widget(Data).GtkEventMouseEnterLeave(Widget, Event);
    end;
  GDK_LEAVE_NOTIFY:
    begin
      // TGtk3Widget(Data).GtkEventMouseEnterLeave(Widget, Event);
    end;
  GDK_FOCUS_CHANGE:
    begin
      //
    end;
  GDK_CONFIGURE:
    begin
      // GDK_CONFIGURE
    end;
  GDK_MAP:
    begin
      // DebugLn('****** GDK_MAP FOR ',dbgsName(TGtk3Widget(Data).LCLObject));
    end;
  GDK_UNMAP:
    begin
      // DebugLn('****** GDK_UNMAP FOR ',dbgsName(TGtk3Widget(Data).LCLObject));
    end;
  GDK_PROPERTY_NOTIFY:
    begin
      // DebugLn('****** GDK_PROPERTY_NOTIFY FOR ',dbgsName(TGtk3Widget(Data).LCLObject));
    end;
  GDK_CLIENT_EVENT:
    begin
      // DebugLn('****** GDK_CLIENT_EVENT FOR ',dbgsName(TGtk3Widget(Data).LCLObject));
    end;
  GDK_VISIBILITY_NOTIFY:
    begin
      // Result := TGtk3Widget(Data).GtkEventShowHide(Widget, Event);
      // DebugLn('****** GDK_VISIBILITY_NOTIFY FOR ' + dbgsName(TGtk3Widget(Data).LCLObject));
    end;
  GDK_SCROLL:
    begin
      // DebugLn('****** GDK_SCROLL ' + dbgsName(TGtk3Widget(Data).LCLObject));
    end;
  else
    DebugLn('TGtk3MenuItem.MenuItemEvent unhandled event.');
  end;
end;

class procedure TGtk3MenuItem.MenuItemActivated({%H-}AItem: PGtkMenuItem; AData: GPointer); cdecl;
var
  Msg: TLMessage;
begin
  // DebugLn('Gtk3MenuItemActivated ',dbgsName(TGtk3MenuItem(Adata)));
  if Assigned(TGtk3MenuItem(AData).MenuItem) and (TGtk3MenuItem(AData).Lock=0) then
  begin
    inc(TGtk3MenuItem(AData).Lock);
    try
      FillChar(Msg{%H-}, SizeOf(Msg), #0);
      Msg.Msg := LM_ACTIVATE;
      TGtk3MenuItem(AData).MenuItem.Dispatch(Msg);
    finally
      dec(TGtk3MenuItem(AData).Lock);
    end;
  end;
end;

procedure TGtk3MenuItem.InitializeWidget;
begin
  FWidget := CreateWidget(FParams);
  LCLIntf.SetProp(HWND(Self),'lclwidget',Self);

  // move signal connections into attach events
  FWidget^.set_events(GDK_DEFAULT_EVENTS_MASK);
  g_signal_connect_data(FWidget, 'event', TGCallback(@MenuItemEvent), Self, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(FWidget,'activate',TGCallBack(@MenuItemActivated), Self, nil, G_CONNECT_DEFAULT);
  // must hide all by default
  // FWidget^.hide;
end;

procedure TGtk3MenuItem.SetCheck(ACheck: boolean);
begin
  if Self.IsValidHandle and (lock=0)  then
    PGtkCheckMenuItem(fWidget)^.active:=ACheck;
end;


{ TGtk3ScrollableWin}

function TGtk3ScrollableWin.GetHScrollBarPolicy: TGtkPolicyType;
var
  AScrollWin: PGtkScrolledWindow;
  APolicy: TGtkPolicyType;
begin
  Result := GTK_POLICY_AUTOMATIC;
  AScrollWin := getScrolledWindow;
  if not Gtk3IsScrolledWindow(AScrollWin) then
    exit;
  AScrollWin^.get_policy(@Result, @APolicy);
end;

function TGtk3ScrollableWin.GetVScrollBarPolicy: TGtkPolicyType;
var
  AScrollWin: PGtkScrolledWindow;
  APolicy: TGtkPolicyType;
begin
  Result := GTK_POLICY_AUTOMATIC;
  AScrollWin := getScrolledWindow;
  if not Gtk3IsScrolledWindow(AScrollWin) then
    exit;
  AScrollWin^.get_policy(@APolicy, @Result);
end;

procedure TGtk3ScrollableWin.SetBorderStyle(AValue: TBorderStyle);
begin
  if FBorderStyle=AValue then Exit;
  FBorderStyle:=AValue;
  if IsWidgetOK then
  begin
    if AValue = bsNone then
      getScrolledWindow^.set_shadow_type(GTK_SHADOW_NONE)
    else
      getScrolledWindow^.set_shadow_type(GTK_SHADOW_ETCHED_IN);
  end;
end;

procedure TGtk3ScrollableWin.SetHScrollBarPolicy(AValue: TGtkPolicyType);
var
  AScrollWin: PGtkScrolledWindow;
  APolicyH, APolicyV: TGtkPolicyType;
begin
  AScrollWin := getScrolledWindow;
  if not Gtk3IsScrolledWindow(AScrollWin) or IsDesigning then
    exit;
  AScrollWin^.get_policy(@APolicyH, @APolicyV);
  AScrollWin^.set_policy(AValue, APolicyV);
end;

procedure TGtk3ScrollableWin.SetVScrollBarPolicy(AValue: TGtkPolicyType);
var
  AScrollWin: PGtkScrolledWindow;
  APolicyH, APolicyV: TGtkPolicyType;
begin
  AScrollWin := getScrolledWindow;
  if not Gtk3IsScrolledWindow(AScrollWin) or IsDesigning then
    exit;
  AScrollWin^.get_policy(@APolicyH, @APolicyV);
  AScrollWin^.set_policy(APolicyH, AValue);
end;

class procedure TGtk3ScrollableWin.ScrolledLayoutSizeAllocate(
  AWidget: PGtkWidget; AGdkRect: PGdkRectangle; Data: gpointer); cdecl;
var
  hadj, vadj: PGtkAdjustment;
  //aWindow: PGdkWindow;
  //aCtl: TGtk3Widget absolute Data;
  HSize,VSize: integer;
  uWidth, uHeight: guint;
begin
  {Note: Gtk expects that we set content size and then it calculates scrollbar values. LCL
   is doing opposite, it sets scrollbar values eg via SetScrollInfo and then content should
   be automatically calculated by widgetset. Gtk is crazy about it.
   So, we are in charge here to help both. We save adjusted values
   in setscrollinfo in LCLVAdj and LCLHAdj, so after GtkLayout sends size-allocate with accurate
   content size we apply LCL values to adjustments and everybody is happy.
   TODO: eg TTreeView editor, if scrollbar position is not at lower pos, showing
   editor moves scrollbar to pos 0, if we apply LCL saved value here, then
   editor won't show at all. Maybe moving editor and showing should take into
   account scrollbar position and calculate x,y offset.}

  hadj := PGtkScrollable(aWidget)^.get_hadjustment;
  vadj := PGtkScrollable(aWidget)^.get_vadjustment;

  HSize := Max(AGdkRect^.Width, Round(hAdj^.upper));
  VSize := Max(AGdkRect^.Height, Round(vAdj^.upper));

  PGtkLayout(aWidget)^.get_size(@uWidth, @uHeight);
  if (uWidth <> HSize) or (uHeight <> VSize) then
    PGtkLayout(aWidget)^.set_size(HSize, VSize);

  if not TGtk3Widget(Data).InUpdate and TGtk3Widget(Data).LCLObject.ClientRectNeedsInterfaceUpdate then
    TGtk3Widget(Data).LCLObject.DoAdjustClientRectChange;
end;

class function TGtk3ScrollableWin.CheckIfScrollbarPressed(scrollbar: PGtkWidget; out AMouseOver: boolean;
  const ACheckModifier: TGdkModifierTypeIdx): boolean;
var
  display: PGdkDisplay;
  seat: PGdkSeat;
  pointer: PGdkDevice;
  screen: PGdkScreen;
  x, y, win_x, win_y: gint;
  allocation: TGtkAllocation;
  state: TGdkModifierType;
begin
  Result := False;
  AMouseOver := False;

  display := gdk_display_get_default();
  seat := gdk_display_get_default_seat(display);

  // Get the pointer device (mouse)
  pointer := gdk_seat_get_pointer(seat);

  if pointer = nil then
  begin
    DebugLn('WARNING: No pointer device available');
    Exit;
  end;

  screen := scrollbar^.get_screen;
  if (screen = nil) then
    screen := gdk_screen_get_default;

  gdk_device_get_position(pointer, @screen, @x, @y);
  gdk_window_get_origin(gtk_widget_get_window(scrollbar), @win_x, @win_y);

  // Translate the pointer position to the scrollbar's local coordinates
  x := x - win_x;
  y := y - win_y;

  // Get the scrollbar's allocation (local coordinates)
  gtk_widget_get_allocation(scrollbar, @allocation);

  // Check if the pointer is within the scrollbar's allocation
  if (x >= allocation.x) and (x < allocation.x + allocation.width) and
     (y >= allocation.y) and (y < allocation.y + allocation.height) then
  begin
    // Get the button state
    gdk_device_get_state(pointer, gtk_widget_get_window(scrollbar), nil, @state);
    AMouseOver := True;
    Result := (ACheckModifier in state);
    {$IFDEF GTK3DEBUGSCROLL}
    if Result then
      DebugLn(Format('Scrollbar is pressed and being dragged pointer x %d y %d',[x, y]))
    else
      DebugLn(Format('Mouse is over the scrollbar but not pressed pointer x %d y %d', [x, y]));
    {$ENDIF}
  end else
  begin
    {$IFDEF GTK3DEBUGSCROLL}
    DebugLn('**** Mouse is not over the scrollbar ****');
    {$ENDIF}
  end;
end;

procedure TGtk3ScrollableWin.InitializeWidget;
begin
  LCLVAdj := nil;
  LCLHAdj := nil;
  inherited InitializeWidget;
end;

procedure TGtk3ScrollableWin.OffsetMousePos(const aGlobalX, aGlobalY: double;
  APoint: PPoint);
var
  ScrolledWindow: PGtkScrolledWindow;
  Viewport: PGtkWidget;
  WinX, WinY: gint;
  LocalX, LocalY: gint;
  ClientX, ClientY: gint;
  aWindow: PGdkWindow;
begin
  inherited OffsetMousePos(aGlobalX, aGlobalY, APoint);
  if [wtCustomControl, wtScrollingWinControl, wtWindow] * WidgetType = [] then
    exit;
  ScrolledWindow := GetScrolledWindow;
  if not Gtk3IsScrolledWindow(ScrolledWindow) then
    exit;
  Viewport := gtk_bin_get_child(PGtkBin(ScrolledWindow));
  if not Gtk3IsWidget(ViewPort) then
    exit;
  aWindow := GetContainerWidget^.get_window;
  if not Gtk3IsGdkWindow(aWindow) then
    exit;
  gdk_window_get_origin(aWindow, @WinX, @WinY);
  LocalX := Trunc(aGlobalX) - WinX;
  LocalY := Trunc(aGlobalY) - WinY;

  if gtk_widget_translate_coordinates(GetContainerWidget, Viewport, LocalX, LocalY, @ClientX, @ClientY) then
  begin
    // writeln(Format('Mouse clicked at: Global=(%.2f, %.2f), Local=(%d, %d), Client=(%d, %d)',
    //  [aGlobalX, aGlobalY, LocalX, LocalY, ClientX, ClientY]));
    aPoint^.X := ClientX;
    aPoint^.Y := ClientY;
  end;
end;

class function TGtk3ScrollableWin.RangeChangeValue(ARange: PGtkRange;
  AScrollType: TGtkScrollType; AValue: gdouble; AData: gPointer): gboolean;
  cdecl;
var
  Msg: TLMVScroll;
  MaxValue: gdouble;
  StateFlags: TGtkStateFlags;
  ACtl: TGtk3ScrollableWin;
begin
  Result := gtk_false;

  {$IFDEF GTK3DEBUGSCROLL}
  DebugLn(Format('>TGtk3ScrollableWin.RangeChangeValue Value: %d', [Round(AValue)]),' IsHScrollBar ',dbgs(PGtkOrientable(ARange)^.get_orientation = GTK_ORIENTATION_HORIZONTAL),' InUpdate=',dbgs(TGtk3Widget(AData).InUpdate));
  {$ENDIF}
  ACtl := TGtk3ScrollableWin(aData);
  if PGtkOrientable(ARange)^.get_orientation = GTK_ORIENTATION_HORIZONTAL then
    Msg.Msg := LM_HSCROLL
  else
    Msg.Msg := LM_VSCROLL;

  if ARange^.adjustment^.page_size > 0 then
  begin
    {we must use cached values since gtk3 has it's own meaning about page_size}
    if Msg.Msg = LM_HSCROLL then
      MaxValue := ACtl.LCLHAdj^.upper - ACtl.LCLHAdj^.page_size
    else
      MaxValue := ACtl.LCLVAdj^.upper - ACtl.LCLVAdj^.page_size;
  end else
    MaxValue := ARange^.adjustment^.upper;

  if (AValue > MaxValue) then
    AValue := MaxValue
  else if (AValue < ARange^.adjustment^.lower) then
    AValue := ARange^.adjustment^.lower;

  with Msg do
  begin
    Pos := Round(AValue);
    if Pos < High(SmallPos) then
      SmallPos := Pos
    else
      SmallPos := High(SmallPos);
    {$note to get this correct we must use TQtWidget.CreateFrom() for scrollbars}
    ScrollBar := HWND(TGtk3Widget(AData)); // HWND({%H-}PtrUInt(ARange));
    ScrollCode := Gtk3ScrollTypeToScrollCode(AScrollType);
  end;
  TGtk3Widget(AData).DeliverMessage(Msg, True);

  if Msg.Scrollcode = SB_THUMBTRACK then
  begin
    StateFlags := ARange^.get_state_flags;
    if not (GTK_STATE_FLAG_ACTIVE in StateFlags) then
    begin
      Msg.ScrollCode := SB_THUMBPOSITION;
      TGtk3Widget(AData).DeliverMessage(Msg, False);
      Msg.ScrollCode := SB_ENDSCROLL;
      TGtk3Widget(AData).DeliverMessage(Msg, False);
    end;
  end else
    ARange^.set_state_flags([GTK_STATE_FLAG_ACTIVE], True);

  if ([wtScrollingWinControl, wtWindow, wtHintWindow, wtDialog] * TGtk3Widget(AData).WidgetType <> []) and
  ((Msg.ScrollCode = SB_LINEUP) or (Msg.ScrollCode = SB_LINEDOWN)) then
    Result := gtk_true;
  {$IFDEF GTK3DEBUGSCROLL}
  DebugLn('<RangeChangeValue: Result=',dbgs(Result),' FuturePos=', dbgs(Msg.Pos),' ScrollCode=',dbgs(Msg.ScrollCode),' InUpdate=',dbgs(TGtk3Widget(AData).InUpdate));
  {$ENDIF}
end;

class procedure TGtk3ScrollableWin.RangeValueChanged(range: PGtkRange;
  data: gpointer); cdecl;
var
  PrevValue, CurrentValue, Delta: gdouble;
  Control: TGtk3ScrollableWin;
  Msg: TLMVScroll;
  APressed, AMouseOver: boolean;
  Adjustment: PGtkAdjustment;
begin
  Control := TGtk3ScrollableWin(data);
  {$IFDEF GTK3DEBUGSCROLL}
  writeln('>TGtk3ScrollableWin.RangeValueChanged ', dbgsName(Control.LCLObject), ' InUpdate=', Control.InUpdate);
  {$ENDIF}
  if Control.InUpdate then
  begin
    {$IFDEF GTK3DEBUGSCROLL}
    writeln('<TGtk3ScrollableWin.RangeValueChanged exiting because of InUpdate lock.');
    {$ENDIF}
    exit;
  end else
  begin
    {$IFDEF GTK3DEBUGSCROLL}
    writeln('  setting InUpdate lock.');
    {$ENDIF}
    Control.BeginUpdate;
  end;

  Adjustment := gtk_range_get_adjustment(range);
  CurrentValue := gtk_adjustment_get_value(Adjustment);
  PrevValue := gtk_adjustment_get_lower(Adjustment); // Store the previous position before it changes

  Delta := CurrentValue - PrevValue;

  if Delta <> 0 then
  begin
    if gtk_orientable_get_orientation(PGtkOrientable(range)) = GTK_ORIENTATION_VERTICAL then
    begin
      Msg.Msg := LM_VSCROLL;
    end
    else
    begin
      Msg.Msg := LM_HSCROLL;
    end;

    APressed := Control.CheckIfScrollbarPressed(PGtkScrollBar(range), AMouseOver, GDK_BUTTON1_MASK);
    if APressed then
      Msg.ScrollCode := SB_THUMBTRACK
    else
      Msg.ScrollCode := SB_THUMBPOSITION;

    Msg.Pos := Round(CurrentValue);
    Msg.ScrollBar := HWND(Control);

    Control.DeliverMessage(Msg);
  end;

  {$IFDEF GTK3DEBUGSCROLL}
  WriteLn('<TGtk3ScrollableWin.RangeValueChanged: CurrentValue=', CurrentValue:0:2, ', PrevValue=', PrevValue:0:2,
          ', Delta=', Delta:0:2, ', InUpdate=', Control.InUpdate, ' releasing lock ...');
  {$ENDIF}
  Control.EndUpdate;
end;

function TGtk3ScrollableWin.ClientToScreen(var P: TPoint): boolean;
var
  gx, gy, wx, wy: gint;
begin

  if not Widget^.is_toplevel then
  begin
    Result := getContainerWidget^.translate_coordinates(GetContainerWidget^.get_toplevel, P.X, P.Y, @gx, @gy);
    if Result then
    begin
      gdk_window_get_origin(gtk_widget_get_toplevel(GetContainerWidget)^.window, @wx, @wy);
      P := Point(gx + wx, gy + wy);
    end else
      Result := inherited ClientToScreen(P);
  end else
    Result := inherited ClientToScreen(P);
end;

procedure TGtk3ScrollableWin.DestroyWidget;
begin
  if Assigned(LCLHAdj) then
    LCLHAdj^.unref;
  if Assigned(LCLVAdj) then
    LCLVAdj^.unref;
  LCLHAdj := nil;
  LCLVAdj := nil;
  inherited DestroyWidget;
end;

procedure TGtk3ScrollableWin.SetScrollBarsSignalHandlers(const
  AIsHorizontalScrollBar:boolean);
begin
  {TODO: create real instances for scrollbars via TGtk3Widget.CreateFrom() ?}
  if IsDesigning then
    exit;
  FBorderStyle := bsNone;
  if AIsHorizontalScrollBar then
  begin
    if not FHBarInitialized then
      g_signal_connect_data(getHorizontalScrollbar, 'change-value',
        TGCallback(@RangeChangeValue), Self, nil, G_CONNECT_DEFAULT);
    FHBarInitialized := True;
  end else
  begin
    if not FVBarInitialized then
      g_signal_connect_data(getVerticalScrollbar, 'change-value',
        TGCallback(@RangeChangeValue), Self, nil, G_CONNECT_DEFAULT);
    FVBarInitialized := True;
  end;
end;

{$IFDEF GTK3DEBUGSIZE}
function GetViewportClientAreaWithScrollbars(ScrolledWindow: PGtkScrolledWindow): TRect;
var
  Viewport: PGtkViewport;
  ViewportAllocation, HScrollbarAllocation, VScrollbarAllocation: TGtkAllocation;
  Padding, Border: TGtkBorder;
  HScrollbar, VScrollbar: PGtkWidget;
  HScrollbarHeight, VScrollbarWidth: Integer;
  ScrollPolicyH, ScrollPolicyV: TGtkPolicyType;
  StyleContext: PGtkStyleContext;
begin

  FillChar(Result, SizeOf(Result), 0);
  HScrollbarHeight := 0;
  VScrollbarWidth := 0;

  Viewport := PGtkViewport(gtk_bin_get_child(PGtkBin(ScrolledWindow)));
  if Viewport = nil then
  begin
    writeln('Viewport not found.');
    Exit;
  end;

  gtk_widget_get_allocation(PGtkWidget(Viewport), @ViewportAllocation);

  //Get the style context for padding and border
  StyleContext := gtk_widget_get_style_context(PGtkWidget(Viewport));
  FillChar(Padding, SizeOf(Padding), 0);
  FillChar(Border, SizeOf(Border), 0);


  gtk_style_context_get_padding(StyleContext, GTK_STATE_FLAG_NORMAL, @Padding);
  gtk_style_context_get_border(StyleContext, GTK_STATE_FLAG_NORMAL, @Border);

  // Check scrollbar visibility policies
  gtk_scrolled_window_get_policy(ScrolledWindow, @ScrollPolicyH, @ScrollPolicyV);

  //Check if the horizontal scrollbar is visible
  HScrollbar := gtk_scrolled_window_get_hscrollbar(ScrolledWindow);
  if (HScrollbar <> nil) and gtk_widget_get_visible(HScrollbar) and (ScrollPolicyH <> GTK_POLICY_NEVER) then
  begin
    gtk_widget_get_allocation(HScrollbar, @HScrollbarAllocation);
    HScrollbarHeight := HScrollbarAllocation.height;
  end;

  //Check if the vertical scrollbar is visible
  VScrollbar := gtk_scrolled_window_get_vscrollbar(ScrolledWindow);
  if (VScrollbar <> nil) and gtk_widget_get_visible(VScrollbar) and (ScrollPolicyV <> GTK_POLICY_NEVER) then
  begin
    gtk_widget_get_allocation(VScrollbar, @VScrollbarAllocation);
    VScrollbarWidth := VScrollbarAllocation.width;
  end;

  //Now we calculate the client area.
  Result.Left := ViewportAllocation.x + Border.left + Padding.left;
  Result.Top := ViewportAllocation.y + Border.top + Padding.top;
  Result.Right := ViewportAllocation.x + ViewportAllocation.width - Border.right - Padding.right - VScrollbarWidth;
  Result.Bottom := ViewportAllocation.y + ViewportAllocation.height - Border.bottom - Padding.bottom - HScrollbarHeight;

  writeln(Format('Client Area Calculation: Left=%d, Top=%d, Right=%d, Bottom=%d',
    [Result.Left, Result.Top, Result.Right, Result.Bottom]));
end;
{$ENDIF}

function TGtk3ScrollableWin.getClientBounds: TRect;
var
  Allocation: TGtkAllocation;
  x:gint;
  y:gint;
  w:gint;
  h:gint;
  AViewport:PGtkViewport;
  AWindow:PGdkWindow;
  VOffset, HOffset:gint;
  Bar:PGtkScrollbar;
begin
  Result := Rect(0, 0, 0, 0);
  if IsWidgetOK then
  begin
    if [wtLayout] * WidgetType <> [] then
    begin
      Result := Rect(0, 0, 0, 0);
      AWindow := PGtkLayout(getContainerWidget)^.get_window;
      if Assigned(AWindow) and Gtk3IsGdkWindow(AWindow) then
      begin
        Bar := getHorizontalScrollbar;
        if (Bar <> nil) and Gtk3IsWidget(Bar) and Bar^.get_visible and GTK3WidgetSet.OverlayScrolling then
          HOffset := Bar^.get_allocated_height
        else
          HOffset := 0;
        Bar := getVerticalScrollbar;
        if (Bar <> nil) and Gtk3IsWidget(Bar) and Bar^.get_visible and GTK3WidgetSet.OverlayScrolling then
          VOffset := Bar^.get_allocated_width
        else
          VOffset := 0;
        AWindow^.get_geometry(@x, @y, @w, @h);
        Result := Bounds(x, y, w - VOffset, h - HOffset);
      end;
      OffsetRect(Result, -Result.Left, -Result.Top);
      exit;
    end;
    AViewport := getViewPort;
    if Assigned(AViewport) and Gtk3IsGdkWindow(AViewport^.get_view_window) then
    begin
      AWindow := AViewport^.get_view_window;
      if Assigned(AWindow) then
      begin
        Bar := getHorizontalScrollbar;
        if (Bar <> nil) and Gtk3IsWidget(Bar) and Bar^.get_visible and GTK3WidgetSet.OverlayScrolling then
          HOffset := Bar^.get_allocated_height
        else
          HOffset := 0;
        Bar := getVerticalScrollbar;
        if (Bar <> nil) and Gtk3IsWidget(Bar) and Bar^.get_visible and GTK3WidgetSet.OverlayScrolling then
          VOffset := Bar^.get_allocated_width
        else
          VOffset := 0;
        AViewPort^.get_view_window^.get_geometry(@x, @y, @w, @h);
        Result := Bounds(x, y, w - VOffset, h - HOffset);
      end else
      begin
        AViewPort^.get_allocation(@Allocation);
        Result := RectFromGtkAllocation(Allocation);
      end;
    end else
    begin
      getContainerWidget^.get_allocation(@Allocation);
      Result := RectFromGtkAllocation(Allocation);
    end;
  end;
  if (Self is TGtk3Window) then
    exit;
  {$IFDEF GTK3DEBUGSIZE}
  if Assigned(AViewPort) then
    DebugLn('TGtk3ScrollableWin.getClientBounds result ',dbgs(Result),' from viewport ',dbgsName(LCLObject))
  else
    ;//DebugLn('TGtk3ScrollableWin.getClientBounds result ',dbgs(Result),' no viewport ',dbgsName(LCLObject));
  {$ENDIF}
end;

function TGtk3ScrollableWin.getViewport:PGtkViewport;
begin
  Result := nil;
end;

{ TGtk3Memo }

class procedure TGtk3Memo.MemoTextChanged(aBuffer: PGtkTextBuffer; aData: gPointer); cdecl;
var
  Mess: TLMessage;
begin
  Mess := Default(TLMessage);
  if (aData = nil) then
    exit;
  Mess.Msg := CM_TEXTCHANGED;
  LCLMessageGlue.DeliverMessage(TGtk3Widget(aData).LCLObject, Mess);
end;

function TGtk3Memo.CreateWidget(const Params: TCreateParams): PGtkWidget;
var
  AMemo: TCustomMemo;
  ABuffer: PGtkTextBuffer;
  AScrollStyle: TGtkScrollStyle;
begin

  FKeysToEat := [];
  AMemo := TCustomMemo(LCLObject);

  FWidgetType := FWidgetType + [wtMemo, wtScrollingWin];
  Result := PGtkScrolledWindow(TGtkScrolledWindow.new(nil, nil));

  FCentralWidget := PGtkTextView(TGtkTextView.new);

  FCentralWidget^.set_has_window(True);

  if AMemo.WordWrap then
    PGtkTextView(FCentralWidget)^.set_wrap_mode(GTK_WRAP_WORD)
  else
    PGtkTextView(FCentralWidget)^.set_wrap_mode(GTK_WRAP_NONE);

  ABuffer := PGtkTextBuffer^.new(PGtkTextTagTable^.new);
  {%H-}ABuffer^.set_text(PgChar(AMemo.Text), -1);
  PGtkTextView(FCentralWidget)^.set_buffer(ABuffer);

  PGtkScrolledWindow(Result)^.add(FCentralWidget);

  AScrollStyle := Gtk3TranslateScrollStyle(AMemo.ScrollBars);

  //memo without scrollbars still grows if policy is NEVER, so cheat gtk.
  if AScrollStyle.Horizontal = GTK_POLICY_NEVER then
    AScrollStyle.Horizontal := GTK_POLICY_EXTERNAL;
  if AScrollStyle.Vertical = GTK_POLICY_NEVER then
    AScrollStyle.Vertical := GTK_POLICY_EXTERNAL;

  PGtkScrolledWindow(Result)^.set_policy(AScrollStyle.Horizontal, AScrollStyle.Vertical);

  PGtkScrolledWindow(Result)^.set_shadow_type(BorderStyleShadowMap[AMemo.BorderStyle]);
  PGtkScrolledWindow(Result)^.get_vscrollbar^.set_can_focus(False);
  PGtkScrolledWindow(Result)^.get_hscrollbar^.set_can_focus(False);

  FCentralWidget^.set_can_focus(True);
  PGtkScrolledWindow(Result)^.set_can_focus(False);
end;

procedure TGtk3Memo.InitializeWidget;
var
  ATextView: PGtkTextView;
begin
  inherited InitializeWidget;
  ATextView := PGtkTextView(getContainerWidget);
  g_signal_connect_data(ATextView^.get_buffer,'changed', TGCallBack(@MemoTextChanged), Self, nil, G_CONNECT_DEFAULT);
end;


function TGtk3Memo.EatArrowKeys(const AKey: Word): Boolean;
begin
  Result := False;
end;

function TGtk3Memo.getHorizontalScrollbar: PGtkScrollbar;
begin
  Result := nil;
  if not IsWidgetOk then
    exit;
  Result := PGtkScrollBar(PGtkScrolledWindow(Widget)^.get_hscrollbar);
  if Result <> nil then
    SetScrollBarsSignalHandlers(True);
end;

function TGtk3Memo.getVerticalScrollbar: PGtkScrollbar;
begin
  Result := nil;
  if not IsWidgetOk then
    exit;
  Result := PGtkScrollBar(PGtkScrolledWindow(Widget)^.get_vscrollbar);
  if Result <> nil then
    SetScrollBarsSignalHandlers(False);
end;

function TGtk3Memo.GetScrolledWindow: PGtkScrolledWindow;
begin
  if IsWidgetOK then
    Result := PGtkScrolledWindow(Widget)
  else
    Result := nil;
end;

function TGtk3Memo.getSelStart: Integer;
var
  AStart, AStop: gint;
  ATextView: PGtkTextView;
  ATextBuffer: PGtkTextBuffer;
  ATextMark: PGtkTextMark;
  ATextIter, AStartIter, AEndIter: TGtkTextIter;
begin
  Result := 0;
  ATextView := PGtkTextView(GetContainerWidget);
  ATextBuffer := gtk_text_view_get_buffer(ATextView);
  ATextMark := gtk_text_buffer_get_insert(ATextBuffer);
  gtk_text_buffer_get_iter_at_mark(ATextBuffer, @ATextIter, ATextMark);

  Result := gtk_text_iter_get_offset(@ATextIter);
  if getSelLength = 0 then Exit;

  if not gtk_text_buffer_get_selection_bounds(ATextBuffer, @AStartIter, @AEndIter) then
    exit;

  AStart := gtk_text_iter_get_offset(@AStartIter);
  AStop := gtk_text_iter_get_offset(@AEndIter);

  Result := Min(AStart, AStop);
end;

function TGtk3Memo.getSelLength: Integer;
var
  ATextView: PGtkTextView;
  ATextBuffer: PGtkTextBuffer;
  AStartIter, AEndIter: TGtkTextIter;
begin
  Result := 0;
  ATextView := PGtkTextView(GetContainerWidget);
  ATextBuffer := gtk_text_view_get_buffer(ATextView);
  if not gtk_text_buffer_get_selection_bounds(ATextBuffer, @AStartIter, @AEndIter) then
    exit;
  Result := Abs(gtk_text_iter_get_offset(@AEndIter) - gtk_text_iter_get_offset(@AStartIter));
end;

procedure TGtk3Memo.setSelStart(AValue: Integer);
var
  AIter: TGtkTextIter;
  ATextView: PGtkTextView;
begin
  if not IsWidgetOk then
    exit;
  ATextView := PGtkTextView(GetContainerWidget);
  gtk_text_buffer_get_iter_at_offset(ATextView^.get_buffer, @AIter, AValue);
  gtk_text_buffer_place_cursor(ATextView^.get_buffer, @AIter);
end;

procedure TGtk3Memo.setSelLength(AValue: Integer);
var
  AStart: gint;
  AStop: gint;
  ATextView: PGtkTextView;
  ATextBuffer: PGtkTextBuffer;
  AStartIter, AEndIter: TGtkTextIter;
begin
  if not IsWidgetOk then
    exit;
  ATextView := PGtkTextView(GetContainerWidget);
  ATextBuffer := ATextView^.get_buffer;
  AStart := getSelStart;
  gtk_text_buffer_get_iter_at_offset(ATextBuffer, @AStartIter, AStart);
  gtk_text_buffer_get_iter_at_offset(ATextBuffer, @AEndIter, AStart + AValue);
  gtk_text_buffer_select_range(ATextBuffer, @AStartIter, @AEndIter);
end;

procedure TGtk3Memo.setSelText(const ANewSelText: string);
var
  StartIter, EndIter: TGtkTextIter;
  AText: PChar;
  StartPos: gint;
  Buffer: PGtkTextBuffer;
begin
  Buffer := PGtkTextView(GetContainerWidget)^.get_buffer;
  gtk_text_buffer_get_start_iter(Buffer, @StartIter);
  gtk_text_buffer_get_end_iter(Buffer, @EndIter);
  AText := gtk_text_buffer_get_text(Buffer, @StartIter, @EndIter, False);
  StartPos := Pos(ANewSelText, StrPas(AText)) - 1;
  if StartPos >= 0 then
  begin
    gtk_text_buffer_get_iter_at_offset(Buffer, @StartIter, StartPos);
    gtk_text_buffer_get_iter_at_offset(Buffer, @EndIter, StartPos + Length(ANewSelText));
    gtk_text_buffer_select_range(Buffer, @StartIter, @EndIter);
  end;
end;

function TGtk3Memo.GetAlignment: TAlignment;
var
  AJustification: TGtkJustification;
begin
  Result := taLeftJustify;
  if IsWidgetOk then
  begin
    AJustification := PGtkTextView(GetContainerWidget)^.get_justification;
    if AJustification = GTK_JUSTIFY_RIGHT then
      Result := taRightJustify
    else
    if AJustification = GTK_JUSTIFY_CENTER then
      Result := taCenter;
  end;
end;

function TGtk3Memo.GetCaretPos: TPoint;
var
  ATextView: PGtkTextView;
  ATextBuffer: PGtkTextBuffer;
  AIter: TGtkTextIter;
  AOffset: Integer;
  Rect: TGdkRectangle;
  YTop, YBottom: gint;
begin
  ATextView := PGtkTextView(GetContainerWidget);
  ATextBuffer := gtk_text_view_get_buffer(ATextView);

  AOffset := GetSelStart - GetSelLength;
  gtk_text_buffer_get_iter_at_offset(ATextBuffer, @AIter, AOffset);
  gtk_text_view_get_iter_location(ATextView, @AIter, @Rect);
  gtk_text_view_get_line_yrange(ATextView, @AIter, @YTop, @YBottom);
  Result.Y := gtk_text_iter_get_line(@AIter);
  if Rect.y > YTop then
    Result.Y := Result.Y + ((Rect.y - YTop) div (YBottom - YTop));
  Result.X := gtk_text_iter_get_line_offset(@AIter);
end;

function TGtk3Memo.GetReadOnly: Boolean;
begin
  Result := True;
  if IsWidgetOk then
    Result := not PGtkTextView(GetContainerWidget)^.get_editable;
end;

function TGtk3Memo.GetWantTabs: Boolean;
begin
  Result := False;
  if IsWidgetOK then
    Result := PGtkTextView(GetContainerWidget)^.get_accepts_tab;
end;

function TGtk3Memo.GetWordWrap: Boolean;
begin
  Result := True;
  if IsWidgetOk then
    Result := PGtkTextView(GetContainerWidget)^.get_wrap_mode = GTK_WRAP_WORD;
end;

procedure TGtk3Memo.SetAlignment(AValue: TAlignment);
begin
  if IsWidgetOk then
    PGtkTextView(GetContainerWidget)^.set_justification(AGtkJustification[AValue]);
end;

procedure TGtk3Memo.SetCaretPos(AValue: TPoint);
var
  Iter: TGtkTextIter;
  ABuffer: PGtkTextBuffer;
begin
  ABuffer := gtk_text_view_get_buffer(PGtkTextView(getContainerWidget));
  gtk_text_buffer_get_iter_at_offset(ABuffer, @Iter, AValue.X);
  gtk_text_buffer_place_cursor(ABuffer, @Iter);
end;

procedure TGtk3Memo.SetReadOnly(AValue: Boolean);
begin
  if IsWidgetOk then
    PGtkTextView(GetContainerWidget)^.set_editable(not AValue);
end;

procedure TGtk3Memo.SetWantTabs(AValue: Boolean);
begin
  if IsWidgetOK then
    PGtkTextView(GetContainerWidget)^.set_accepts_tab(AValue);
end;

procedure TGtk3Memo.SetWordWrap(AValue: Boolean);
begin
  if IsWidgetOk then
  begin
    if AValue then
      PGtkTextView(GetContainerWidget)^.set_wrap_mode(GTK_WRAP_WORD)
    else
      PGtkTextView(GetContainerWidget)^.set_wrap_mode(GTK_WRAP_NONE);
  end;
end;

function TGtk3Memo.getText: String;
var
  ABuffer: PGtkTextBuffer;
  AIter: TGtkTextIter;
  ALastIter: TGtkTextIter;
begin
  Result := '';
  if IsWidgetOk then
  begin
    ABuffer := PGtkTextView(FCentralWidget)^.get_buffer;
    ABuffer^.get_start_iter(@AIter);
    ABuffer^.get_end_iter(@ALastIter);
    Result := ABuffer^.get_text(@AIter, @ALastIter, False);
  end;
  // DebugLn('TGtk3Memo.getText Result=',Result);
end;

procedure TGtk3Memo.setText(const AValue: String);
var
  ABuffer: PGtkTextBuffer;
  AIter: PGtkTextIter;
begin
  // DebugLn('TGtk3Memo.setText AValue=',AValue);
  if IsWidgetOk then
  begin
    ABuffer := PGtkTextView(FCentralWidget)^.get_buffer;
    ABuffer^.set_text(PgChar(AValue), -1);
    AIter:=nil;
    ABuffer^.get_start_iter(AIter);
    ABuffer^.place_cursor(AIter);
  end;
end;

{ TGtk3ListBox }

class procedure TGtk3ListBox.ListBoxSelectionChanged({%H-}ASelection: PGtkTreeSelection; AData: GPointer); cdecl;
var
  Msg: TLMessage;
begin
  // DebugLn('Gtk3ListBoxSelectionChanged ');
  FillChar(Msg{%H-}, SizeOf(Msg), #0);
  Msg.Msg := LM_SELCHANGE;
  if not TGtk3Widget(AData).InUpdate then
    TGtk3Widget(AData).DeliverMessage(Msg, False);
end;

function TGtk3ListBox.CreateWidget(const Params: TCreateParams): PGtkWidget;
var
  AListBox: TCustomListBox;
  ListStore: PGtkListStore;
  ItemList: TGtkListStoreStringList;
  AColumn: PGtkTreeViewColumn;
  Renderer : PGtkCellRenderer;
begin
  FListBoxStyle := lbStandard;

  FWidgetType := FWidgetType + [wtTreeModel, wtListBox, wtScrollingWin];
  AListBox := TCustomListBox(LCLObject);


  Result := PGtkScrolledWindow(TGtkScrolledWindow.new(nil, nil));
  Result^.show;

  ListStore := gtk_list_store_new (2, [G_TYPE_STRING, G_TYPE_POINTER, nil]);
  FCentralWidget := TGtkTreeView.new_with_model(PGtkTreeModel(ListStore));
  PGtkTreeView(FCentralWidget)^.set_headers_visible(False);
  g_object_unref (liststore);

  ItemList := TGtkListStoreStringList.Create(PGtkListStore(PGtkTreeView(FCentralWidget)^.get_model), 0, LCLObject);
  g_object_set_data(PGObject(FCentralWidget),GtkListItemLCLListTag, ItemList);

  Renderer := LCLIntfCellRenderer_New();

  g_object_set_data(PGObject(renderer), 'lclwidget', Self);

  AColumn := gtk_tree_view_column_new_with_attributes ('LISTITEMS', renderer,
             ['text', 0, nil]);

  g_object_set_data(PGObject(AColumn), 'lclwidget', Self);

  // maybe create GtkCellLayout class with our implementation and set that layout
  // to AColumn
  // PGtkCellLayout(AColumn)^.set_cell_data_func()
  // PGtkCellLayout(AColumn)^.set_cell_data_func(renderer, @LCLIntfRenderer_GtkCellLayoutDataFunc, Self, nil);
  AColumn^.set_cell_data_func(renderer, @LCLIntfRenderer_ColumnCellDataFunc, Self, nil);

  PGtkTreeView(FCentralWidget)^.append_column(AColumn);

  AColumn^.set_clickable(True);

  // AColumn^set_cell_data_func(AColumn, renderer, @LCLIntfRenderer_ColumnCellDataFunc, Self, nil);

  PGtkScrolledWindow(Result)^.add(FCentralWidget);

  PGtkScrolledWindow(Result)^.get_vscrollbar^.set_can_focus(False);
  PGtkScrolledWindow(Result)^.get_hscrollbar^.set_can_focus(False);
  PGtkScrolledWindow(Result)^.set_policy(GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
  FListBoxStyle := AListBox.Style;
  if FListBoxStyle <> lbOwnerDrawVariable then
  begin
    AColumn^.set_sizing(GTK_TREE_VIEW_COLUMN_FIXED);
    PGtkTreeView(FCentralWidget)^.set_fixed_height_mode(True);
  end;

end;

function TGtk3ListBox.EatArrowKeys(const AKey: Word): Boolean;
begin
  Result := False;
end;

procedure TGtk3ListBox.SetColor(AValue: TColor);
var
  ADisabledColor, BgColor: TGdkRGBA;
begin

  BgColor := TColortoTGdkRGBA(ColorToRgb(AValue));

  getContainerWidget^.get_style_context^.get_background_color([GTK_STATE_FLAG_INSENSITIVE], @ADisabledColor);
  //override all
  if AValue = clDefault then
    gtk_widget_override_background_color(getContainerWidget, GTK_STATE_FLAG_NORMAL, nil)
  else
    gtk_widget_override_background_color(getContainerWidget, GTK_STATE_FLAG_NORMAL, @BgColor);
  //return system highlight color
  BgColor := TColortoTGdkRGBA(ColorToRgb(clHighlight));
  gtk_widget_override_background_color(getContainerWidget, [GTK_STATE_FLAG_SELECTED], @BgColor);
  gtk_widget_override_background_color(getContainerWidget, [GTK_STATE_FLAG_INSENSITIVE], @ADisabledColor);
end;

procedure TGtk3ListBox.InitializeWidget;
begin
  inherited InitializeWidget;
  if not IsDesigning then
    g_signal_connect_data(GetSelection, 'changed', TGCallback(@ListBoxSelectionChanged), Self, nil, G_CONNECT_DEFAULT);
end;

function TGtk3ListBox.getHorizontalScrollbar: PGtkScrollbar;
begin
  Result := nil;
  if not IsWidgetOk then
    exit;
  Result := PGtkScrollBar(PGtkScrolledWindow(Widget)^.get_hscrollbar);
end;

function TGtk3ListBox.getVerticalScrollbar: PGtkScrollbar;
begin
  Result := nil;
  if not IsWidgetOk then
    exit;
  Result := PGtkScrollBar(PGtkScrolledWindow(Widget)^.get_vscrollbar);
end;

function TGtk3ListBox.GetScrolledWindow: PGtkScrolledWindow;
begin
  if IsWidgetOK then
    Result := PGtkScrolledWindow(Widget)
  else
    Result := nil;
end;

function TGtk3ListBox.GetItemIndex: Integer;
var
  TreeView: PGtkTreeView;
  Path: PGtkTreePath;
  Column: PGtkTreeViewColumn;
  Selection: PGtkTreeSelection;
begin
  Result := -1;
  if Gtk3IsWidget(FWidget) then
  begin
    Path := nil;
    Column := nil;
    TreeView := PGtkTreeView(GetContainerWidget);
    TreeView^.get_cursor(@Path, @Column);
    if Path <> nil then
    begin
      Result := gtk_tree_path_get_indices(Path)^;
      if Result = 0 then
      begin
        Selection := TreeView^.get_selection;
        if not Selection^.path_is_selected(Path) then
          Result := -1;
      end;
    end;
  end;
end;

function TGtk3ListBox.GetMultiSelect: Boolean;
var
  Selection: PGtkTreeSelection;
begin
  if IsWidgetOk then
  begin
    Selection := GetSelection;
    if Selection <> nil then
      Result := Selection^.get_mode <> GTK_SELECTION_SINGLE;
  end;
end;

procedure TGtk3ListBox.SetItemIndex(AValue: Integer);
var
  TreeView: PGtkTreeView;
  Selection: PGtkTreeSelection;
  Path: PGtkTreePath;
begin
  if Gtk3IsWidget(FWidget) then
  begin
    TreeView := PGtkTreeView(GetContainerWidget);
    Selection := GetSelection;
    if (AValue < 0) then
      Path := nil
    else
      Path := gtk_tree_path_new_from_indices(AValue, [-1]);

    // if singleselection mode then selection = itemindex
    if Path <> nil then
    begin
      gtk_tree_view_set_cursor(TreeView, Path, nil, False);
    end else
    begin
      Path := gtk_tree_path_new_from_indices(0, [-1]);
        gtk_tree_view_set_cursor(TreeView, Path, nil, False);
      gtk_tree_selection_unselect_all(Selection);
    end;

    if Path <> nil then
      gtk_tree_path_free(Path);
  end;
end;

procedure TGtk3ListBox.SetListBoxStyle(AValue: TListBoxStyle);
begin
  if FListBoxStyle=AValue then Exit;
  FListBoxStyle:=AValue;
end;

procedure TGtk3ListBox.SetMultiSelect(AValue: Boolean);
var
  Selection: PGtkTreeSelection;
begin
  if IsWidgetOk then
  begin
    Selection := GetSelection;
    if Selection <> nil then
    begin
      if AValue then
        Selection^.set_mode(GTK_SELECTION_MULTIPLE)
      else
        Selection^.set_mode(GTK_SELECTION_SINGLE);
    end;
  end;
end;

function TGtk3ListBox.GetSelCount: Integer;
var
  Selection: PGtkTreeSelection;
  Rows: PGList;
  ListStoreModel: PGtkTreeModel;
begin
  Result := 0;
  if not Gtk3IsWidget(FWidget) then
    exit;
  Selection := GetSelection;
  if Selection = nil then
    exit;
  Rows := Selection^.get_selected_rows(@ListStoreModel);
  Result := g_list_length(Rows);
  g_list_free(Rows);
end;

function TGtk3ListBox.GetSelection: PGtkTreeSelection;
begin
  if not IsWidgetOk then
    exit(nil);
  Result := PGtkTreeView(GetContainerWidget)^.get_selection;
end;

function TGtk3ListBox.GetItemRect(const AIndex: integer): TRect;
var
  AModel: PGtkTreeModel;
  Iter: TGtkTreeIter;
  AGdkRect: TGdkRectangle;
  ACol: TGtkTreeViewColumn;
begin
  Result := Rect(0, 0, 0, 0);
  AModel := PGtkTreeView(getContainerWidget)^.model;
  if AModel = nil then
    exit;
  if AModel^.iter_nth_child(@Iter, nil, AIndex) then
  begin
    ACol := gtk_tree_view_get_column(PGtkTreeView(getContainerWidget), 0)^;
    gtk_tree_view_get_cell_area(PGtkTreeView(getContainerWidget), AModel^.get_path(@Iter), @ACol, @AGdkRect);
    Result := RectFromGdkRect(AGdkRect);
  end;
end;

function TGtk3ListBox.GetIndexAtXY(const X, Y: integer): integer;
var
  Path: PGtkTreePath;
  Column: PGtkTreeViewColumn;
  CellX, CellY: Integer;
  Indices: PInteger;
begin
  Result := -1;
  if gtk_tree_view_get_path_at_pos(PGtkTreeView(getContainerWidget), X, Y,
    @Path, @Column, @CellX, @CellY) then
  begin
    Indices := gtk_tree_path_get_indices(Path);
    if Assigned(Indices) then
      Result := Indices^;
    gtk_tree_path_free(Path);
  end;
end;

function TGtk3ListBox.GetItemSelected(const AIndex: Integer): Boolean;
var
  ASelection: PGtkTreeSelection;
  AModel: PGtkTreeModel;
  Item: TGtkTreeIter;
begin
  Result := False;

  if not IsWidgetOK then
    exit;

  AModel := PGtkTreeView(GetContainerWidget)^.model;

  if AModel = nil then
    exit;

  ASelection := GetSelection;

  if ASelection = nil then
    exit;

  if AModel^.iter_nth_child(@Item, nil, AIndex) then
    Result := ASelection^.iter_is_selected(@Item);
end;

function TGtk3ListBox.GetScrollWidth: integer;
begin
  Result := Round(getHorizontalScrollbar^.get_adjustment^.get_upper);
end;

procedure TGtk3ListBox.SelectItem(const AIndex: Integer; ASelected: Boolean);
var
  ASelection: PGtkTreeSelection;
  AModel: PGtkTreeModel;
  Iter: TGtkTreeIter;
begin
  if not IsWidgetOK then
    exit;

  AModel := PGtkTreeView(getContainerWidget)^.model;

  if AModel = nil then
    exit;

  ASelection := GetSelection;

  if AModel^.iter_nth_child(@Iter, nil, AIndex) then
  begin
    case ASelected of
      True:
        if not ASelection^.iter_is_selected(@Iter) then
          ASelection^.select_iter(@Iter);
      False:
        if ASelection^.iter_is_selected(@Iter) then
          ASelection^.unselect_iter(@Iter);
    end;
  end;
end;

procedure TGtk3ListBox.SetTopIndex(const AIndex: Integer);
var
  AModel: PGtkTreeModel;
  Iter: TGtkTreeIter;
  APath: PGtkTreePath;
begin
  AModel := PGtkTreeView(getContainerWidget)^.model;

  if not AModel^.iter_nth_child(@Iter, nil, AIndex) then
    exit;
  APath := AModel^.get_path(@Iter);
  PGtkTreeView(getContainerWidget)^.scroll_to_cell(APath, nil, False, 0.0, 0.0);
  APath^.free;
end;

{ TGtk3CheckListBox }

class procedure TGtk3CheckListBox.CheckListBoxDataFunc({%H-}tree_column: PGtkTreeViewColumn;
  cell: PGtkCellRenderer; tree_model: PGtkTreeModel; iter: PGtkTreeIter; {%H-}data: gPointer); cdecl;
var
  b: byte;
  ADisabled: gboolean;
  AValue: TCheckBoxState;
begin
  B := 0;
  ADisabled := False;
  gtk_tree_model_get(tree_model, iter, [gtk3CLBState, @b, -1]);
  gtk_tree_model_get(tree_model, iter, [gtk3CLBDisabled, @ADisabled, -1]);
  AValue := TCheckBoxState(b); // TCheckBoxState is 4 byte
  g_object_set(cell, 'inconsistent', [gboolean(AValue = cbGrayed), nil]);
  if AValue <> cbGrayed then
    gtk_cell_renderer_toggle_set_active(PGtkCellRendererToggle(cell), AValue = cbChecked);

  g_object_set(cell, 'activatable', [gboolean(not ADisabled), nil]);
end;

class procedure TGtk3CheckListBox.CheckListBoxToggle({%H-}cellrenderertoggle : PGtkCellRendererToggle;
  arg1 : PGChar; AData: GPointer); cdecl;
var
  Mess: TLMessage;
  Param: PtrInt;
  Iter : TGtkTreeIter;
  TreeView: PGtkTreeView;
  ListStore: PGtkTreeModel;
  Path: PGtkTreePath;
  AState: TCheckBoxState;
begin
  Val(arg1, Param);

  TreeView := PGtkTreeView(TGtk3CheckListBox(AData).GetContainerWidget);
  ListStore := gtk_tree_view_get_model(TreeView);
  if gtk_tree_model_iter_nth_child(ListStore, @Iter, nil, Param) then
    begin
      TCustomCheckListBox(TGtk3Widget(AData).LCLObject).Toggle(Param);
      AState := TCustomCheckListBox(TGtk3Widget(AData).LCLObject).State[Param];
      gtk_list_store_set(PGtkListStore(ListStore), @Iter, [gtk3CLBState,
        Byte(AState), -1]);
    end;


  Path := gtk_tree_path_new_from_indices(Param, [-1]);
  if Path <> nil then
  begin
    gtk_tree_view_set_cursor(TreeView, Path, nil, False);
    gtk_tree_path_free(Path);
  end;

  FillChar(Mess{%H-}, SizeOf(Mess), #0);
  Mess.Msg := LM_CHANGED;

  Mess.Result := 0;
  Mess.WParam := Param;
  LCLMessageGlue.DeliverMessage(TGtk3Widget(AData).LCLObject, Mess);

end;

function TGtk3CheckListBox.CreateWidget(const Params: TCreateParams
  ): PGtkWidget;
var
  ACheckListBox: TCustomCheckListBox;
  ListStore: PGtkListStore;
  ItemList: TGtkListStoreStringList;
  AColumn: PGtkTreeViewColumn;
  Toggle: PGtkCellRendererToggle;
  Renderer : PGtkCellRenderer;
begin
  FWidgetType := FWidgetType + [wtTreeModel, wtListBox, wtCheckListBox, wtScrollingWin];
  ACheckListBox := TCustomCheckListBox(LCLObject);
  FListBoxStyle := lbStandard;

  Result := PGtkScrolledWindow(TGtkScrolledWindow.new(nil, nil));
  Result^.show;

  ListStore := gtk_list_store_new (4, [G_TYPE_UCHAR, G_TYPE_STRING, G_TYPE_POINTER, G_TYPE_BOOLEAN, nil]);
  FCentralWidget := TGtkTreeView.new_with_model(PGtkTreeModel(ListStore));
  PGtkTreeView(FCentralWidget)^.set_headers_visible(False);
  g_object_unref (liststore);

  AColumn := gtk_tree_view_column_new;

  // checkable column
  Toggle := gtk_cell_renderer_toggle_new;
  g_object_set_data(PGObject(Toggle), 'lclwidget', Self);

  AColumn^.set_title('CHECKBINS');
  AColumn^.pack_start(Toggle, True);
  AColumn^.set_cell_data_func(Toggle, TGtkTreeCellDataFunc(@CheckListBoxDataFunc), Self, nil);
  Toggle^.set_active(True);
  PGtkTreeView(FCentralWidget)^.append_column(AColumn);
  AColumn^.set_clickable(True);

  g_signal_connect_data(Toggle, 'toggled', TGCallback(@CheckListBoxToggle), Self, nil, G_CONNECT_DEFAULT);

  Renderer := LCLIntfCellRenderer_New(); // gtk_cell_renderer_text_new;

  g_object_set_data(PGObject(Renderer), 'lclwidget', Self);

  AColumn := gtk_tree_view_column_new_with_attributes ('LISTITEMS', Renderer,
             ['text', 1, nil]);

  g_object_set_data(PGObject(AColumn), 'lclwidget', Self);

  // AColumn^.pack_start(Renderer, True);

  AColumn^.set_cell_data_func(Renderer, @LCLIntfRenderer_ColumnCellDataFunc, Self, nil);

  PGtkTreeView(FCentralWidget)^.append_column(AColumn);

  ItemList := TGtkListStoreStringList.Create(PGtkListStore(PGtkTreeView(FCentralWidget)^.get_model), 1, LCLObject);
  g_object_set_data(PGObject(FCentralWidget),GtkListItemLCLListTag, ItemList);


  AColumn^.set_clickable(True);

  PGtkScrolledWindow(Result)^.add(FCentralWidget);


  PGtkScrolledWindow(Result)^.get_vscrollbar^.set_can_focus(False);
  PGtkScrolledWindow(Result)^.get_hscrollbar^.set_can_focus(False);
  PGtkScrolledWindow(Result)^.set_policy(GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
  FListBoxStyle := ACheckListBox.Style;
  if ACheckListBox.MultiSelect then
    PGtkTreeView(FCentralWidget)^.get_selection^.set_mode(GTK_SELECTION_MULTIPLE)
  else
    PGtkTreeView(FCentralWidget)^.get_selection^.set_mode(GTK_SELECTION_SINGLE);
end;

{ TGtk3ListView }

class function TGtk3ListView.ListViewItemPreSelected({%H-}selection: PGtkTreeSelection; {%H-}model: PGtkTreeModel;
  path: PGtkTreePath; path_is_currently_selected: GBoolean; AData: GPointer): GBoolean; cdecl;
begin
  if path_is_currently_selected then ;
  // DebugLn('Gtk3WS_ListViewItemSelected ,path selected ',dbgs(path_is_currently_selected));
  // this function is called *before* the item is selected
  // The result should be True to allow the Item to change selection
  Result := True;

  if (AData = nil) or TGtk3Widget(AData).InUpdate then
    exit;

  if not Assigned(TGtk3ListView(AData).FPreselectedIndices) then
    TGtk3ListView(AData).FPreselectedIndices := TFPList.Create;

  if TGtk3ListView(AData).FPreselectedIndices.IndexOf({%H-}Pointer(PtrInt(gtk_tree_path_get_indices(path)^))) = -1 then
    TGtk3ListView(AData).FPreselectedIndices.Add({%H-}Pointer(PtrInt(gtk_tree_path_get_indices(path)^)));
end;

class procedure TGtk3ListView.ListViewItemSelected(ASelection: PGtkTreeSelection; AData: GPointer); cdecl;
var
  AList, TmpList: PGList;
  Msg: TLMNotify;
  NM: TNMListView;
  Path: PGtkTreePath;
  Indices: Integer;
  i, j: Integer;
  B: Boolean;
begin
  if (AData = nil) or TGtk3Widget(AData).InUpdate then
    exit;
  if not Assigned(TGtk3ListView(AData).FPreselectedIndices) then
    exit;

  AList := gtk_tree_selection_get_selected_rows(ASelection, nil);
  TGtk3Widget(AData).BeginUpdate; // Prevents entering Gtk3WS_ListViewItemPreSelected

  try
    for i := 0 to TGtk3ListView(AData).FPreselectedIndices.Count - 1 do
    begin
      FillChar(Msg{%H-}, SizeOf(Msg), 0);
      Msg.Msg := CN_NOTIFY;
      FillChar(NM{%H-}, SizeOf(NM), 0);
      NM.hdr.hwndfrom := HWND(TGtk3Widget(AData));
      NM.hdr.code := LVN_ITEMCHANGED;
      NM.iItem := {%H-}PtrInt(TGtk3ListView(AData).FPreselectedIndices.Items[i]);
      NM.iSubItem := 0;
      B := False;

      TmpList := AList;
      while TmpList <> nil do
      begin
        Path := PGtkTreePath(TmpList^.data);
        if Assigned(Path) then
        begin
          Indices := gtk_tree_path_get_indices(Path)^;
          B := Indices = {%H-}PtrInt(TGtk3ListView(AData).FPreselectedIndices.Items[i]);
          if B then
            break;
        end;
        TmpList := TmpList^.next;
      end;

      if not B then
        NM.uOldState := LVIS_SELECTED
      else
        NM.uNewState := LVIS_SELECTED;
      NM.uChanged := LVIF_STATE;
      Msg.NMHdr := @NM.hdr;
      LCLMessageGlue.DeliverMessage(TGtk3Widget(AData).LCLObject, Msg);
    end;
  finally
    FreeAndNil(TGtk3ListView(AData).FPreselectedIndices);
    if AList <> nil then
    begin
      TmpList := AList;
      while TmpList <> nil do
      begin
        Path := PGtkTreePath(TmpList^.data);
        if Assigned(Path) then
          gtk_tree_path_free(Path);
        TmpList := TmpList^.next;
      end;
      g_list_free(AList);
    end;

    TGtk3Widget(AData).EndUpdate;
  end;
end;

type
  TCustomListViewHack = class(TCustomListView);

procedure TGtk3ListView.setItemWidth(const AImageListWidth: integer);
var
  aImgWidth, aBorders: gint;
  AListView: TCustomListViewHack;
begin
  aImgWidth := AImageListWidth;
  aListView := TCustomListViewHack(LCLObject);
  if not (AListView.ViewStyle in [vsSmallIcon, vsIcon]) then
    exit;
  if not Gtk3IsWidget(getContainerWidget) then
    exit;
  if aImgWidth <= 0 then
  begin
    if AListView.ViewStyle = vsIcon then
      gtk_icon_size_lookup(Ord(GTK_ICON_SIZE_DIALOG), @aImgWidth, @aBorders)
    else
      gtk_icon_size_lookup(Ord(GTK_ICON_SIZE_LARGE_TOOLBAR), @aImgWidth, @aBorders);
  end;
  PGtkIconView(GetContainerWidget)^.set_item_width(aImgWidth);
end;

function TGtk3ListView.CreateWidget(const Params: TCreateParams): PGtkWidget;
var
  AListView: TCustomListViewHack;
  AScrollStyle: TGtkScrollStyle;
  PtrType: GType;
  TreeModel: PGtkTreeModel;
  iter:TGtkTreeIter;
  pxb:PGdkPixbuf;
  err:gint;
begin
  FImages := nil;
  FPreselectedIndices := nil;
  FWidgetType := FWidgetType + [wtTreeModel, wtListView, wtScrollingWin];
  AListView := TCustomListViewHack(LCLObject);
  Result := PGtkScrolledWindow(TGtkScrolledWindow.new(nil, nil));

  PtrType := G_TYPE_POINTER;
  FViewStyle := AListView.ViewStyle;
  if ViewStyle in [vsIcon,vsSmallIcon] then
  begin
    TreeModel := PGtkTreeModel(gtk_list_store_new(3, [
      G_TYPE_POINTER, // ListItem pointer
      G_TYPE_STRING, // text
     gdk_pixbuf_get_type() // pixbuf
     ]));
    FCentralWidget := TGtkIconView.new_with_model(TreeModel);
    PGtkIconView(FCentralWidget)^.set_text_column(1);
    PGtkIconView(FCentralWidget)^.set_pixbuf_column(2);
    PGtkIconView(FCentralWidget)^.selection_mode:=GTK_SELECTION_SINGLE;
  end
  else
  begin
    TreeModel := PGtkTreeModel(gtk_list_store_newv(1, @PtrType));
    FCentralWidget := TGtkTreeView.new_with_model(TreeModel);
  end;

  FIsTreeView := not (ViewStyle in [vsIcon,vsSmallIcon]);

  FCentralWidget^.set_has_window(True);
  FCentralWidget^.show;

  PGtkScrolledWindow(Result)^.add(FCentralWidget);
  //PGtkScrolledWindow(Result)^.set_focus_child(FCentralWidget);

  AScrollStyle := Gtk3TranslateScrollStyle(AListView.ScrollBars);
  // gtk3 scrolled window hates GTK_POLICY_NONE
  PGtkScrolledWindow(Result)^.set_policy(AScrollStyle.Horizontal, AScrollStyle.Vertical);
  PGtkScrolledWindow(Result)^.set_shadow_type(BorderStyleShadowMap[AListView.BorderStyle]);
  PGtkScrolledWindow(Result)^.get_vscrollbar^.set_can_focus(False);
  PGtkScrolledWindow(Result)^.get_hscrollbar^.set_can_focus(False);
  g_object_unref (PGObject(TreeModel));
  PGtkScrolledWindow(Result)^.set_can_focus(False);
  PGtkTreeView(FCentralWidget)^.set_can_focus(True);
  if FIsTreeView then
  begin
    gtk_tree_selection_set_select_function(PGtkTreeView(FCentralWidget)^.get_selection, TGtkTreeSelectionFunc(@ListViewItemPreSelected),
      Self, nil);
    g_signal_connect_data(PGtkTreeView(FCentralWidget)^.get_selection, 'changed', TGCallback(@ListViewItemSelected), Self, nil, G_CONNECT_DEFAULT);

    PGtkTreeView(FCentralWidget)^.set_headers_visible(AListView.ShowColumnHeaders and (AListView.ViewStyle = vsReport));
    PGtkTreeView(FCentralWidget)^.resize_children;

  end else
  begin
    g_signal_connect_data (PGtkIconView(FCentralWidget), 'selection-changed',
                        TGCallback(@selection_changed), Self, nil, G_CONNECT_DEFAULT);
  end;
end;

class function TGtk3ListView.selection_changed(AIconView: PGtkIconView;
  aData: gPointer): gboolean; cdecl;
var
  pl, tmp: PGList;
  pndx: PGint;
  i, cnt: gint;
  Msg: TLMNotify;
  NM: TNMListView;
  ctl: TGtk3ListView;
begin
  Result := gtk_false;
  ctl := TGtk3ListView(aData);
  pl := PGtkIconView(ctl.GetContainerWidget)^.get_selected_items();

  if Assigned(pl) then
  begin
    try
      tmp := pl;
      while Assigned(tmp) do
      begin
        pndx := PGtkTreePath(tmp^.data)^.get_indices_with_depth(@cnt);
        // lv := TListView(ctl.LCLObject);
        ctl.BeginUpdate;
        try
          for i := 0 to cnt - 1 do
          begin
            FillChar(Msg{%H-}, SizeOf(Msg), 0);
            Msg.Msg := CN_NOTIFY;
            FillChar(NM{%H-}, SizeOf(NM), 0);
            NM.hdr.hwndfrom := HWND(ctl);
            NM.hdr.code := LVN_ITEMCHANGED;
            NM.iItem := {%H-}PtrInt(pndx^);
            NM.iSubItem := 0;
            NM.uNewState := LVIS_SELECTED;
            NM.uChanged := LVIF_STATE;
            Msg.NMHdr := @NM.hdr;
            ctl.DeliverMessage(Msg);
            inc(pndx);
          end;
        finally
          ctl.EndUpdate;
        end;
        gtk_tree_path_free(PGtkTreePath(tmp^.data));

        tmp := tmp^.next;
      end;
    finally
      g_list_free(pl);
    end;
  end;
end;

function TGtk3ListView.EatArrowKeys(const AKey: Word): Boolean;
begin
  Result := False;
end;

procedure TGtk3ListView.SetColor(AValue: TColor);
var
  ADisabledColor, BgColor: TGdkRGBA;
begin

  BgColor := TColortoTGdkRGBA(ColorToRgb(AValue));

  getContainerWidget^.get_style_context^.get_background_color([GTK_STATE_FLAG_INSENSITIVE], @ADisabledColor);
  //override all
  if AValue = clDefault then
    gtk_widget_override_background_color(getContainerWidget, GTK_STATE_FLAG_NORMAL, nil)
  else
    gtk_widget_override_background_color(getContainerWidget, GTK_STATE_FLAG_NORMAL, @BgColor);
  //return system highlight color
  BgColor := TColortoTGdkRGBA(ColorToRgb(clHighlight));
  gtk_widget_override_background_color(getContainerWidget, [GTK_STATE_FLAG_SELECTED], @BgColor);
  gtk_widget_override_background_color(getContainerWidget, [GTK_STATE_FLAG_INSENSITIVE], @ADisabledColor);
end;

destructor TGtk3ListView.Destroy;
begin
  ClearImages;
  FreeAndNil(FImages);
  FreeAndNil(FPreselectedIndices);
  inherited Destroy;
end;

function TGtk3ListView.getHorizontalScrollbar: PGtkScrollbar;
begin
  Result := nil;
  if not IsWidgetOk then
    exit;
  Result := PGtkScrollBar(PGtkScrolledWindow(Widget)^.get_hscrollbar);
end;

function TGtk3ListView.getVerticalScrollbar: PGtkScrollbar;
begin
  Result := nil;
  if not IsWidgetOk then
    exit;
  Result := PGtkScrollBar(PGtkScrolledWindow(Widget)^.get_vscrollbar);
end;

function TGtk3ListView.GetScrolledWindow: PGtkScrolledWindow;
begin
  if IsWidgetOK then
    Result := PGtkScrolledWindow(Widget)
  else
    Result := nil;
end;

procedure TGtk3ListView.ClearImages;
var
  i: Integer;
begin
  if Assigned(FImages) then
  begin
    for i := FImages.Count - 1 downto 0 do
      if FImages[i] <> nil then
        g_object_unref(FImages[i]);
    FImages.Clear;
  end;
end;

procedure TGtk3ListView.ColumnDelete(AIndex: Integer);
var
  AColumn: PGtkTreeViewColumn;
begin
  if IsWidgetOK and IsTreeView then
  begin
    AColumn := PGtkTreeView(GetContainerWidget)^.get_column(AIndex);
    if (AColumn<>nil) then
      PGtkTreeView(GetContainerWidget)^.remove_column(AColumn);
  end;
end;

function TGtk3ListView.ColumnGetWidth(AIndex: Integer): Integer;
var
  AColumn: PGtkTreeViewColumn;
begin
  Result := 0;
  if IsWidgetOK and IsTreeView then
  begin
    AColumn := PGtkTreeView(GetContainerWidget)^.get_column(AIndex);
    if (AColumn<>nil) then
      Result := AColumn^.get_width;
  end;
end;

class procedure TGtk3ListView.ListViewGetPixbufDataFuncForColumn(tree_column: PGtkTreeViewColumn;
  {%H-}cell: PGtkCellRenderer; tree_model: PGtkTreeModel; iter: PGtkTreeIter; AData: GPointer); cdecl;
var
  ListItem: TListItem;
  AImages: TFPList;
  // Widgets: PTVWidgets;
  ListColumn: TListColumn;
  ImageIndex: Integer;
  ColumnIndex: Integer;
  APath: PGtkTreePath;
  gv:TGValue;
  pb:PgdkPixbuf;
begin
  fillchar(gv,sizeof(gv),0);
  gv.init(G_TYPE_OBJECT);
  gv.set_instance(nil);
  PGtkCellRendererPixbuf(cell)^.set_property('pixbuf',@gv);

  gtk_tree_model_get(tree_model, iter, [0, @ListItem, -1]);

  ListColumn := TListColumn(g_object_get_data(tree_column, 'TListColumn'));
  if ListColumn = nil then
    Exit;
  ColumnIndex := ListColumn.Index;
  AImages := TGtk3ListView(AData).Images;
  if AImages = nil then
    Exit;
  ImageIndex := -1;

  if (ListItem = nil) and TCustomListView(TGtk3Widget(AData).LCLObject).OwnerData then
  begin
    APath := gtk_tree_model_get_path(tree_model,iter);
    ListItem := TCustomListView(TGtk3Widget(AData).LCLObject).Items[gtk_tree_path_get_indices(APath)^];
    gtk_tree_path_free(APath);
  end;

  if ListItem = nil then
    Exit;

  if ColumnIndex = 0 then
    ImageIndex := ListItem.ImageIndex
  else
    if ColumnIndex -1 <= ListItem.SubItems.Count-1 then
      ImageIndex := ListItem.SubItemImages[ColumnIndex-1];

  if (ImageIndex > -1) and (ImageIndex <= AImages.Count-1) then
    pb:=TGtk3Image(TBitmap(AImages.Items[ImageIndex]).Handle).Handle^.copy
  else
    pb:=nil;

  gv.set_instance(pb);
  PGtkCellRendererPixbuf(cell)^.set_property('pixbuf',@gv);

  if Assigned(pb) then
    g_object_unref(pb);
end;

class procedure TGtk3ListView.ListViewColumnClicked(column: PGtkTreeViewColumn; AData: GPointer); cdecl;
var
  AColumn: TListColumn;
  Msg: TLMNotify;
  NM: TNMListView;
begin
  AColumn := TListColumn(g_object_get_data(PGObject(column), 'TListColumn'));

  if (AColumn = nil) or (AData = nil) then
    exit;

  FillChar(Msg{%H-}, SizeOf(Msg), 0);
  Msg.Msg := CN_NOTIFY;

  FillChar(NM{%H-}, SizeOf(NM), 0);
  NM.hdr.hwndfrom := {%H-}PtrUInt(AData);
  NM.hdr.code := LVN_COLUMNCLICK;
  NM.iItem := -1;
  NM.iSubItem := AColumn.Index;
  Msg.NMHdr := @NM.hdr;
  LCLMessageGlue.DeliverMessage(TGtk3Widget(AData).LCLObject, Msg);
end;

procedure TGtk3ListView.ColumnInsert(AIndex: Integer; AColumn: TListColumn);
var
  AGtkColumn: PGtkTreeViewColumn;
  PixRenderer,
  TextRenderer: PGtkCellRenderer;
begin
  if not IsWidgetOK or not IsTreeView then
    exit;
  AGtkColumn := TGtkTreeViewColumn.new;

  PixRenderer := gtk_cell_renderer_pixbuf_new();
  TextRenderer := LCLIntfCellRenderer_New;

  AGtkColumn^.pack_start(PixRenderer, False);
  AGtkColumn^.pack_start(TextRenderer, True);

  AGtkColumn^.set_cell_data_func(PixRenderer, @ListViewGetPixbufDataFuncForColumn, Self, nil);


  AGtkColumn^.set_cell_data_func(PGtkCellRenderer(TextRenderer), TGtkTreeCellDataFunc(@LCLIntfCellRenderer_CellDataFunc), Self, nil);

  //store the TColumn in the column data for callbacks
  g_object_set_data(AGtkColumn, PgChar('TListColumn'), gpointer(AColumn));

  g_object_set_data(AGtkColumn, 'pix_renderer', PixRenderer);
  g_object_set_data(AGtkColumn, 'text_renderer', TextRenderer);

  g_signal_connect_data(AGtkColumn,'clicked', TGCallback(@ListViewColumnClicked), Self, nil, G_CONNECT_DEFAULT);
  PGtkTreeView(GetContainerWidget)^.insert_column(AGtkColumn, AIndex);
  AGtkColumn^.set_clickable(True);

end;

procedure TGtk3ListView.SetAlignment(AIndex: Integer; AColumn: TListColumn;
  AAlignment: TAlignment);
var
  AGtkColumn: PGtkTreeViewColumn;
  AFloat: Double;
  AList: PGList;
  textrenderer: PGtkCellRenderer;
  Value: TGValue;
begin
  if not IsWidgetOK or not IsTreeView then
    exit;
  AGtkColumn := PGtkTreeView(getContainerWidget)^.get_column(AIndex);
  if AGtkColumn = nil then
    exit;

  case AAlignment of
    taRightJustify: AFloat := 1;
    taCenter: AFloat := 0.5;
    else
      AFloat := 0;
  end;


  AList := PGtkCellLayout(AGtkColumn)^.get_cells;
  // AList := gtk_tree_view_column_get_cell_renderers(AColumn);
  textrenderer := PGtkCellRenderer(g_list_last(AList)^.data);
  g_list_free(AList);

  Value.g_type := G_TYPE_FLOAT;
  Value.set_float(AFloat);
  g_object_set_property(textrenderer, PChar('xalign'), @Value);

  {now we call set alignment because it calls update over visible rows in col}
  AGtkColumn^.set_alignment(AFloat);
end;

procedure TGtk3ListView.SetColumnAutoSize(AIndex: Integer;
  AColumn: TListColumn; AAutoSize: Boolean);
const
  SizingMap: array[Boolean] of TGtkTreeViewColumnSizing = (
    GTK_TREE_VIEW_COLUMN_FIXED {2},
    GTK_TREE_VIEW_COLUMN_AUTOSIZE {1}
  );
var
  AGtkColumn: PGtkTreeViewColumn;
begin
  if not IsWidgetOK or not IsTreeView then
    exit;
  AGtkColumn := PGtkTreeView(getContainerWidget)^.get_column(AIndex);
  if AGtkColumn <> nil then
  begin
    AGtkColumn^.set_resizable(True);
    AGtkColumn^.set_sizing(SizingMap[AAutoSize]);
  end;
end;

procedure TGtk3ListView.SetColumnCaption(AIndex: Integer; AColumn: TListColumn;
  const ACaption: String);
var
  AGtkColumn: PGtkTreeViewColumn;
begin
  if not IsWidgetOK or not IsTreeView then
    exit;
  AGtkColumn := PGtkTreeView(getContainerWidget)^.get_column(AIndex);
  if AGtkColumn <> nil then
  begin
    AGtkColumn^.set_title(PgChar(ACaption));
  end;
end;

procedure TGtk3ListView.SetColumnMaxWidth(AIndex: Integer;
  AColumn: TListColumn; AMaxWidth: Integer);
var
  AGtkColumn: PGtkTreeViewColumn;
begin
  if not IsWidgetOK or not IsTreeView then
    exit;
  AGtkColumn := PGtkTreeView(getContainerWidget)^.get_column(AIndex);
  if AGtkColumn <> nil then
  begin
    if AMaxWidth <= 0 then
      AGtkColumn^.set_max_width(10000)
    else
      AGtkColumn^.set_max_width(AMaxWidth);
  end;
end;

procedure TGtk3ListView.SetColumnMinWidth(AIndex: Integer;
  AColumn: TListColumn; AMinWidth: Integer);
var
  AGtkColumn: PGtkTreeViewColumn;
begin
  if not IsWidgetOK or not IsTreeView then
    exit;
  AGtkColumn := PGtkTreeView(getContainerWidget)^.get_column(AIndex);
  if AGtkColumn <> nil then
    AGtkColumn^.set_min_width(AMinWidth);
end;

procedure TGtk3ListView.SetColumnWidth(AIndex: Integer; AColumn: TListColumn;
  AWidth: Integer);
var
  AGtkColumn: PGtkTreeViewColumn;
begin
  if not IsWidgetOK or not IsTreeView then
    exit;
  AGtkColumn := PGtkTreeView(getContainerWidget)^.get_column(AIndex);
  if AGtkColumn <> nil then
  begin
    AGtkColumn^.set_fixed_width(AWidth + Ord(AWidth < 1));
  end;
  //  AGtkColumn^.set_widget();
end;

procedure TGtk3ListView.SetColumnVisible(AIndex: Integer; AColumn: TListColumn;
  AVisible: Boolean);
var
  AGtkColumn: PGtkTreeViewColumn;
begin
  if not IsWidgetOK or not IsTreeView then
    exit;
  AGtkColumn := PGtkTreeView(getContainerWidget)^.get_column(AIndex);
  if AGtkColumn <> nil then
  begin
    AGtkColumn^.set_visible(AVisible and (TListView(LCLObject).ViewStyle in [vsList, vsReport]));
  end;
end;

procedure TGtk3ListView.ColumnSetSortIndicator(const AIndex: Integer;
  const AColumn: TListColumn; const ASortIndicator: TSortIndicator);
const
  GtkOrder : array [ TSortIndicator] of TGtkSortType = (GTK_SORT_ASCENDING {0}, GTK_SORT_ASCENDING {0}, GTK_SORT_DESCENDING {1});
var
  AGtkColumn: PGtkTreeViewColumn;
begin
  AGtkColumn := PGtkTreeView(getContainerWidget)^.get_column(AIndex);

  if AGtkColumn <> nil then
  begin
    if ASortIndicator = siNone then
      AGtkColumn^.set_sort_indicator(false)
    else
    begin
      AGtkColumn^.set_sort_indicator(true);
      AgtkColumn^.set_sort_order(GtkOrder[ASortIndicator]);
    end;
  end;
end;

procedure TGtk3ListView.ItemDelete(AIndex: Integer);
var
  AModel: PGtkTreeModel;
  Iter: TGtkTreeIter;
begin
  if IsTreeView then
    AModel := PGtkTreeView(getContainerWidget)^.get_model
  else
    AModel := PGtkIconView(getContainerWidget)^.get_model;
  if gtk_tree_model_iter_nth_child(AModel, @Iter, nil, AIndex) then
    gtk_list_store_remove(PGtkListStore(AModel), @Iter);
end;

function TGtk3ListView.ItemDisplayRect(AIndex: Integer; ASubItem: integer;
  ACode: TDisplayCode): TRect;
var
  {%H-}AModel: PGtkTreeModel;
  Iter: TGtkTreeIter;
  Column: PGtkTreeViewColumn;
  Path: PGtkTreePath;
  ItemRect: TGdkRectangle;
  cell: PGtkCellRenderer;
  y, x: gint;
begin
  Result := Rect(0, 0, 0, 0);
  if IsTreeView then
    AModel := PGtkTreeView(getContainerWidget)^.get_model
  else
    AModel := PGtkIconView(getContainerWidget)^.get_model;
  Path := gtk_tree_path_new_from_indices(AIndex, [-1]);
  try
    if Self.IsTreeView then
    begin
      Column := gtk_tree_view_get_column(PGtkTreeView(GetContainerWidget), ASubItem);
      gtk_tree_view_get_cell_area(PGtkTreeView(GetContainerWidget), Path, Column, @ItemRect);
    end else
      gtk_icon_view_get_cell_rect(PGtkIconView(getContainerWidget), Path, nil, @ItemRect);
    Result := RectFromGdkRect(ItemRect);
  finally
    gtk_tree_path_free(Path);
  end;
end;

procedure TGtk3ListView.ItemInsert(AIndex: Integer; AItem: TListItem);
var
  AModel: PGtkTreeModel;
  Iter: TGtkTreeIter;
  NewIndex: Integer;
  bmp:TBitmap;
  pxb:PGdkPixbuf;
  w,h: gint;
begin
  if not IsWidgetOK then
    exit;
  if IsTreeView then
    AModel := PGtkTreeView(getContainerWidget)^.get_model
  else
    AModel := PGtkIconView(getContainerWidget)^.get_model;

  if AIndex = -1 then
    NewIndex := AModel^.iter_n_children(nil)
  else
    NewIndex := AIndex;

  if IsTreeView then
    gtk_list_store_insert_with_values(PGtkListStore(AModel), @Iter, NewIndex,
      [0, Pointer(AItem), -1])
  else
  begin
    bmp:=TBitmap.Create;
    if Assigned(TListView(LCLObject).LargeImages) then
      TListView(LCLObject).LargeImages.GetBitmap(AIndex,bmp)
    else
    begin
      gtk_icon_size_lookup(Ord(GTK_ICON_SIZE_LARGE_TOOLBAR), @w, @h);
      bmp.SetSize(w, h);
    end;
    pxb:=TGtk3Image(bmp.Handle).Handle^.copy;
    gtk_list_store_insert_with_values(PGtkListStore(AModel), @Iter, NewIndex,
      [0, Pointer(AItem),
       1, PChar(AItem.Caption),
       2, pxb, -1] );
    // list_store takes ownership, so unref and ref again.
    g_object_unref(pxb);
    if not Assigned(FImages) then
      FImages := TFPList.Create;
    g_object_ref(pxb);
    FImages.Add(pxb);
    bmp.Free;
  end;
end;

function TGtk3ListView.ItemPosition(AIndex: integer): TPoint;
var
  x, y: gint;
begin
  Result := ItemDisplayRect(AIndex, 0, drBounds).TopLeft;
  if IsTreeView then
  begin
    gtk_tree_view_convert_bin_window_to_widget_coords(
      PGtkTreeView(GetContainerWidget),
      Result.x, Result.y, @x, @y);
    Result.x := x;
    Result.y := y;
  end;
end;

procedure TGtk3ListView.UpdateItem(AIndex:integer;AItem: TListItem);
var
  Path: PGtkTreePath;
  ItemRect: TGdkRectangle;
  AModel: PGtkTreeModel;
  Iter: TGtkTreeIter;
  bmp:TBitmap;
  pxb:PGdkPixbuf;
  w,h: gint;
begin
  if IsTreeView then
  begin
    Path := gtk_tree_path_new_from_indices(AIndex, [-1]);
    PGtkTreeView(GetContainerWidget)^.get_cell_area(Path, nil, @ItemRect);
    gtk_tree_path_free(Path);
  end else
  begin
    Path := gtk_tree_path_new_from_indices(AIndex, [-1]);
    AModel:=PGtkIconView(GetContainerWidget)^.get_model;
    AModel^.get_iter(@iter,path);

    bmp := TBitmap.Create;
    if (TCustomListViewHack(LCLObject).ViewStyle = vsIcon) and Assigned(TCustomListViewHack(LCLObject).LargeImages) then
      TCustomListViewHack(LCLObject).LargeImages.GetBitmap(AItem.ImageIndex, bmp)
    else
    if (TCustomListViewHack(LCLObject).ViewStyle = vsSmallIcon) and Assigned(TCustomListViewHack(LCLObject).SmallImages) then
      TCustomListViewHack(LCLObject).SmallImages.GetBitmap(AItem.ImageIndex, bmp)
    else
    begin
      gtk_icon_size_lookup(Ord(GTK_ICON_SIZE_LARGE_TOOLBAR), @w, @h);
      bmp.SetSize(w, h);
    end;
    pxb := TGtk3Image(Bmp.Handle).Handle^.copy;
    gtk_list_store_set(PGtkListStore(AModel), @Iter,
      [0, Pointer(AItem),
       1, PChar(AItem.Caption),
       2, pxb, -1] );
    g_object_unref(pxb);
    if not Assigned(FImages) then
      FImages := TFPList.Create;
    g_object_ref(pxb);
    FImages.Add(pxb);
    gtk_tree_path_free(Path);
    bmp.Free;
  end;
end;

procedure TGtk3ListView.ItemSetText(AIndex, ASubIndex: Integer;
  AItem: TListItem; const AText: String);
var
  Path: PGtkTreePath;
  ItemRect: TGdkRectangle;
  AModel: PGtkTreeModel;
  Iter: TGtkTreeIter;
begin
  if not IsWidgetOK then
    exit;

  if IsTreeView then
  begin
    Path := gtk_tree_path_new_from_indices(AIndex, [-1]);
    if GetContainerWidget^.get_realized then
    begin
      PGtkTreeView(GetContainerWidget)^.get_cell_area(Path, nil, @ItemRect);
      // here may be optimization
    end;
    gtk_tree_path_free(Path);
  end else
  begin
    UpdateItem(AIndex,AItem);
  end;
  if GetContainerWidget^.get_visible and (ItemRect.height <> 0) then // item is visible
    GetContainerWidget^.queue_draw;
end;

procedure TGtk3ListView.ItemSetImage(AIndex, ASubIndex: Integer; AItem: TListItem);
var
  Path: PGtkTreePath;
  ItemRect: TGdkRectangle;
  AModel: PGtkTreeModel;
  Iter: TGtkTreeIter;
begin
  if not IsWidgetOK then
    exit;

  if IsTreeView then
  begin
    Path := gtk_tree_path_new_from_indices(AIndex, [-1]);
    if GetContainerWidget^.get_realized then
    begin
      PGtkTreeView(GetContainerWidget)^.get_cell_area(Path, nil, @ItemRect);
      // here may be optimization
    end;
    gtk_tree_path_free(Path);
  end else
  begin
    UpdateItem(AIndex,AItem);
  end;
  if GetContainerWidget^.get_visible and (ItemRect.height <> 0) then // item is visible
    GetContainerWidget^.queue_draw;
end;

procedure TGtk3ListView.ItemSetState(const AIndex: Integer;
  const AItem: TListItem; const AState: TListItemState; const AIsSet: Boolean);
var
  Path: PGtkTreePath;
  ATreeSelection: PGtkTreeSelection;
begin
  if not IsWidgetOK then
    exit;

  case AState of
    lisCut,
    lisDropTarget:
    begin
      //TODO: do something with the rowcolor ?
    end;

    lisFocused:
    begin
      Path := gtk_tree_path_new_from_string(PgChar(IntToStr(AIndex)));
      if IsTreeView then
      begin
        if AIsSet then
          PGtkTreeView(getContainerWidget)^.set_cursor(Path, nil, False)
        else
          PGtkTreeView(GetContainerWidget)^.set_cursor(Path, nil, False);
      end else
      begin
        PGtkIconView(GetContainerWidget)^.set_cursor(Path, nil, False); // valgrind says leak
      end;
      if Path <> nil then
        gtk_tree_path_free(Path);
    end;

    lisSelected:
    begin
      Path := gtk_tree_path_new_from_string(PgChar(IntToStr(AIndex)));
      if IsTreeView then
      begin
        ATreeSelection := PGtkTreeView(GetContainerWidget)^.get_selection;
        if AIsSet and not ATreeSelection^.path_is_selected(Path) then
        begin
          ATreeSelection^.select_path(Path);
          // BroadcastMsg := True;
        end else
        if not AIsSet and ATreeSelection^.path_is_selected(Path) then
        begin
          ATreeSelection^.unselect_path(Path);
          // BroadcastMsg := True;
        end;
      end else
      begin
        if AIsSet and not PGtkIconView(GetContainerWidget)^.path_is_selected(Path) then
        begin
          PGtkIconView(GetContainerWidget)^.select_path(Path);
          // BroadCastMsg := True;
        end else
        if not AIsSet and PGtkIconView(GetContainerWidget)^.path_is_selected(Path) then
        begin
          PGtkIconView(GetContainerWidget)^.unselect_path(Path);
          // BroadCastMsg := True;
        end;
      end;
      if Path <> nil then
        gtk_tree_path_free(Path);
      // if BroadcastMsg then
      //  BroadCastListSelection(ALV, {%H-}PtrUInt(MainView), AIndex, not AIsSet);
    end;
  end;

end;

function TGtk3ListView.ItemGetState(const AIndex: Integer;
  const AItem: TListItem; const AState: TListItemState; out AIsSet: Boolean
  ): Boolean;
var
  Path: PGtkTreePath;
  Column: PPGtkTreeViewColumn;
  Cell: PPGtkCellRenderer;
  APath: PGtkTreePath;
  AStr: PChar;
begin
  Result := False;
  AIsSet := False;
  if not IsWidgetOK then
    exit;
  case AState of
    lisCut,
    lisDropTarget:
    begin
      //TODO: do something with the rowcolor ?
    end;
    lisFocused:
    begin
      Path := nil;
      Column := nil;
      Cell := nil;
      if IsTreeView then
        PGtkTreeView(GetContainerWidget)^.get_cursor(@Path, Column)
      else
        PGtkIconView(GetContainerWidget)^.get_cursor(@Path, Cell);
      if Assigned(Path) then
      begin
        AStr := gtk_tree_path_to_string(Path);
        AIsSet := (StrToIntDef(AStr,-1) = AIndex);
        if AStr <> nil then
          g_free(AStr);
        gtk_tree_path_free(Path);
        Result := True;
      end;
    end;

    lisSelected:
    begin
      APath := gtk_tree_path_new_from_string(PChar(IntToStr(AIndex)));
      if IsTreeView then
        AIsSet := PGtkTreeView(GetContainerWidget)^.get_selection^.path_is_selected(APath)
      else
        AIsSet := PGtkIconView(GetContainerWidget)^.path_is_selected(APath);

      if APath <> nil then
        gtk_tree_path_free(APath);
      Result := True;
    end;
  end;
end;

procedure TGtk3ListView.ScrollToRow(const ARow: integer);
var
  ATreePath: PGtkTreePath;
begin
  ATreePath := gtk_tree_path_new_from_indices(ARow, [-1]);
  if IsTreeView then
    gtk_tree_view_scroll_to_cell(PGtkTreeView(getContainerWidget), ATreePath, nil, False, 0.0, 0.0)
  else
    gtk_icon_view_scroll_to_path(PGtkIconView(getContainerWidget), ATreePath, False, 0.0, 0.0);
  gtk_tree_path_free(ATreePath);
end;

procedure TGtk3ListView.UpdateImageCellsSize;
begin
  // must get renderer via property
  // gtk_tree_view_column_get_cell_renderers
end;

{ TGtk3ComboBox }

function TGtk3ComboBox.GetItemIndex: Integer;
begin
  Result := -1;
  if Assigned(FWidget) and Gtk3IsComboBox(Widget) then
    Result := PGtkComboBox(Widget)^.get_active;
end;

procedure TGtk3ComboBox.SetDroppedDown(AValue: boolean);
begin
  if Assigned(FWidget) and Gtk3IsComboBox(Widget) then
  begin
    if AValue then
      PGtkComboBox(Widget)^.popup
    else
      PGtkComboBox(Widget)^.popdown;
  end;
end;

procedure TGtk3ComboBox.SetItemIndex(AValue: Integer);
begin
  if IsWidgetOK and Gtk3IsComboBox(Widget) then
    PGtkComboBox(Widget)^.set_active(AValue);
end;

function TGtk3ComboBox.GetCellView: PGtkCellView;
var
  AList: PGList;
  i: Integer;
begin
  if FCellView = nil then
  begin
    AList := PGtkComboBox(Widget)^.get_children;
    for i := 0 to g_list_length(AList) -1 do
    begin
      if Gtk3IsCellView(g_list_nth(AList, i)^.data) then
      begin
        FCellView := PGtkCellView(g_list_first(AList)^.data);
        break;
      end;
    end;
    g_list_free(AList);
  end;
  Result := FCellView;
end;

function TGtk3ComboBox.GetPopupWidget: PGtkWidget;
begin
  Result := nil;
  if not IsWidgetOk then
    exit;
  if PGtkComboBox(Widget)^.priv3^.popup_widget <> nil then
    Result := PGtkComboBox(Widget)^.priv3^.popup_widget
  else
  if PGtkComboBox(Widget)^.priv3^.tree_view <> nil then
    Result := PGtkComboBox(Widget)^.priv3^.tree_view;
end;

function TGtk3ComboBox.GetButtonWidget: PGtkWidget;
begin
  Result := nil;
  if not IsWidgetOk then
    exit;
  // button is of type GtkToggleButton
  if PGtkComboBox(Widget)^.priv3^.button <> nil then
    Result := PGtkComboBox(Widget)^.priv3^.button;
end;

function TGtk3ComboBox.GetArrowWidget: PGtkWidget;
begin
  Result := nil;
  if not IsWidgetOk then
    exit;
  // arrow is type is GtkIcon
  if PGtkComboBox(Widget)^.priv3^.arrow <> nil then
    Result := PGtkComboBox(Widget)^.priv3^.arrow;
end;

function TGtk3ComboBox.getSelStart: integer;
var
  AStartPos, AEndPos: gint;
begin
  Result := 0;
  if PGtkComboBox(Widget)^.has_entry then
  begin
    PGtkEditable(PGtkComboBox(Widget)^.get_child)^.get_selection_bounds(@AStartPos, @AEndPos);
    Result := AStartPos;
  end;
end;

function TGtk3ComboBox.getSelLength: integer;
var
  AStartPos, AEndPos: gint;
begin
  Result := 0;
  if PGtkComboBox(Widget)^.has_entry then
  begin
    if PGtkEditable(PGtkComboBox(Widget)^.get_child)^.get_selection_bounds(@AStartPos, @AEndPos) then
      Result := AEndPos - AStartPos;
  end;
end;

function TGtk3ComboBox.getMaxLength: integer;
begin
  Result := 0;
  if PGtkComboBox(Widget)^.has_entry then
    Result := PGtkEntry(PGtkComboBox(Widget)^.get_child)^.get_max_length;
end;

procedure TGtk3ComboBox.SetMaxLength(const AMaxLength: integer);
begin
  if PGtkComboBox(Widget)^.has_entry then
  begin
    PGtkEntry(PGtkComboBox(Widget)^.get_child)^.set_max_length(AMaxLength);
  end;
end;

procedure TGtk3ComboBox.SetSelStart(const ANewStart: integer);
var
  AStartPos, AEndPos: gint;
begin
  if PGtkComboBox(Widget)^.has_entry then
  begin
    //PGtkEditable(PGtkComboBox(Widget)^.get_child)^.get_selection_bounds(@AStartPos, @AEndPos);
    //if AEndPos < ANewStart then
    //  AEndPos := ANewStart;
    AStartPos := ANewStart;
    AEndPos := AStartPos + 1;
    PGtkEditable(PGtkComboBox(Widget)^.get_child)^.select_region(AStartPos, AEndPos);
  end;
end;

procedure TGtk3ComboBox.SetSelLength(const ANewLength: integer);
var
  AStartPos, AEndPos: gint;
begin
  if PGtkEditable(PGtkComboBox(Widget)^.get_child)^.get_selection_bounds(@AStartPos, @AEndPos) then
  begin
    PGtkEditable(PGtkComboBox(Widget)^.get_child)^.select_region(AStartPos, AStartPos + ANewLength);
  end;
end;

function TGtk3ComboBox.CreateWidget(const Params: TCreateParams): PGtkWidget;
var
  ACombo: TCustomComboBox;
  ListStore: PGtkListStore;
  ItemList: TGtkListStoreStringList;
  Renderer: PGtkCellRenderer;
  bs: string;
  pos: gint;
begin
  FWidgetType := FWidgetType + [wtTreeModel, wtComboBox];
  ACombo := TCustomComboBox(LCLObject);
  ListStore := gtk_list_store_new (2, [G_TYPE_STRING, G_TYPE_POINTER, nil]);
  // LCLGtkComboBox is introduced because of inability to control width and height
  // of control.
  if ACombo.Style.HasEditBox then
    Result := LCLGtkComboBoxNewWithModelAndEntry(PGtkTreeModel(ListStore))
  else
    Result := LCLGtkComboBoxNewWithModel(PGtkTreeModel(ListStore));

  if ACombo.Style.HasEditBox then
  begin
    ItemList := TGtkListStoreStringList.Create(PGtkListStore(PGtkComboBox(Result)^.get_model), 0, LCLObject);
    g_object_set_data(PGObject(Result), GtkListItemLCLListTag, ItemList);

    PGtkComboBox(Result)^.set_entry_text_column(0);
    // do not allow combo button to get focus, entry should take focus
    if PGtkComboBox(Result)^.priv3^.button <> nil then
      PGtkComboBox(Result)^.priv3^.button^.set_can_focus(False);

    bs := Self.LCLObject.Caption;
    pos := 0;
    {%H-}PGtkEditable(PGtkComboBox(Result)^.get_child)^.insert_text(pgChar(PChar(bs)),length(bs),@pos);

    // set lclwidget data to entry
    g_object_set_data(PGtkComboBox(Result)^.get_child, 'lclwidget', Self);
    // when we scroll with mouse wheel over entry our scrollevent doesn't catch entry
    // but parent control with window (eg. form), so we are settint all events mask to
    // catch all mouse events on gtkentry.
    if IsDesigning then
    begin
      // maybe set disabled
      {$note this does not work, must make search via children list}
      (*
      g_signal_connect_data(PGtkComboBox(Result)^.priv3^.button, 'button-press-event', TGCallback(@disableMouseButtonEvent), Self, Nil, G_CONNECT_DEFAULT);
      g_signal_connect_data(PGtkComboBox(Result)^.priv3^.button, 'button-release-event', TGCallback(@disableMouseButtonEvent), Self, Nil, G_CONNECT_DEFAULT);

      g_signal_connect_data(PGtkComboBox(Result)^.priv3^.arrow, 'button-press-event', TGCallback(@disableMouseButtonEvent), Self, Nil, G_CONNECT_DEFAULT);
      g_signal_connect_data(PGtkComboBox(Result)^.priv3^.arrow, 'button-release-event', TGCallback(@disableMouseButtonEvent), Self, Nil, G_CONNECT_DEFAULT);
      *)
      PGtkComboBox(Result)^.priv3^.button^.set_sensitive(gtk_false);
      PGtkComboBox(Result)^.priv3^.arrow^.set_sensitive(gtk_false);

      g_signal_connect_data(PGtkEntry(PGtkComboBox(Result)^.get_child), 'button-press-event', TGCallback(@disableMouseButtonEvent), Self, Nil, G_CONNECT_DEFAULT);
      g_signal_connect_data(PGtkEntry(PGtkComboBox(Result)^.get_child), 'button-release-event', TGCallback(@disableMouseButtonEvent), Self, Nil, G_CONNECT_DEFAULT);

      // g_signal_connect_data(PGtkEntry(PGtkComboBox(Result)^.get_child), 'motion-notify-event', TGCallback(@motionNotifyEvent), Self, nil, G_CONNECT_DEFAULT);
      PGtkEntry(PGtkComboBox(Result)^.get_child)^.set_can_focus(False);
    end else
      PGtkEntry(PGtkComboBox(Result)^.get_child)^.set_events(GDK_DEFAULT_EVENTS_MASK);
  end else
  begin
    // FCentralWidget := PGtkWidget(TGtkComboBox.new_with_model(PGtkTreeModel(ListStore)));
    FCentralWidget := Result;

    ItemList := TGtkListStoreStringList.Create(PGtkListStore(PGtkComboBox(FCentralWidget)^.get_model), 0, LCLObject);
    g_object_set_data(PGObject(FCentralWidget), GtkListItemLCLListTag, ItemList);

    renderer := LCLIntfCellRenderer_New();
    g_object_set_data(PGObject(renderer), 'lclwidget', Self);

    gtk_cell_layout_clear(PGtkCellLayout(FCentralWidget));
    gtk_cell_layout_pack_start(PGtkCellLayout(FCentralWidget), renderer, True);
    if not ACombo.Style.IsOwnerDrawn then
      gtk_cell_layout_set_attributes(PGtkCellLayout(FCentralWidget), renderer, ['text', 0, nil]);
    gtk_cell_layout_set_cell_data_func(PGtkCellLayout(FCentralWidget), renderer,
      @LCLIntfCellRenderer_CellDataFunc, Self, nil);

    if Assigned(PGtkComboBox(Result)^.priv3^.cell_view) then
      g_object_set_data(PGObject(PGtkComboBox(Result)^.priv3^.cell_view), 'lclwidget', Self);
    if Assigned(PGtkComboBox(Result)^.priv3^.button) then
      g_object_set_data(PGObject(PGtkComboBox(Result)^.priv3^.button), 'lclwidget', Self);
    if Assigned(PGtkComboBox(Result)^.priv3^.arrow) then
      g_object_set_data(PGObject(PGtkComboBox(Result)^.priv3^.arrow), 'lclwidget', Self);

    FCentralWidget := nil;   //FWidget will be returned from getContainerWidget
    // we need cell renderer, but we need f***g GtkEventBox too
    // maybe an workaround is possible for csDropDownList (use entry with readonly param).
    // if we have GtkEventBox, then ComboBox becomes FCentralWidget.
    // Maybe the best thing would be to organize complete combo around GtkEntry
    // Anyway , I dont see any mouse button event in this case, only when entry_set_above_child is used.
    // FCentralWidget := PGtkComboBox(TGtkComboBox.new_with_model(PGtkTreeModel(ListStore)));
    // PGtkEventBox(Result)^.add(FCentralWidget);
    // ItemList := TGtkListStoreStringList.Create(PGtkListStore(PGtkComboBox(FCentralWidget)^.get_model), 0, LCLObject);
    // g_object_set_data(PGObject(FCentralWidget), GtkListItemLCLListTag, ItemList);
    // PGtkEventBox(Result)^.set_visible_window(True);
  end;
  g_object_unref(ListStore);
  PGtkComboBox(Result)^.set_id_column(0);
  Result^.show;
end;

function TGtk3ComboBox.EatArrowKeys(const AKey: Word): Boolean;
begin
  Result := AKey in [VK_UP, VK_DOWN];
end;

function TGtk3ComboBox.getText: String;
begin
  Result := '';
  if Gtk3IsComboBox(Widget) then
  begin
    with PGtkComboBox(Widget)^ do
    begin
      if has_entry then
      begin
        Result := StrPas(PGtkEntry(get_child)^.Text);
      end else begin
        Result := active_id;
      end;
    end;
  end;
end;

procedure TGtk3ComboBox.setText(const AValue: String);
begin
  if Gtk3IsComboBox(Widget) then
  begin
    with PGtkComboBox(Widget)^ do
    begin
      if has_entry then begin
        {%H-}PGtkEntry(get_child)^.Text := Pgchar(AValue);
      end else begin
        //active_id := Pgchar(AValue); TODO: Wait until property becomes writeble
      end;
    end;
  end;
end;

procedure TGtk3ComboBox.DumpPrivateStructValues(const ADbgEvent: String);
var
  AComboWidget: PGtkComboBox;
  APrivate: PGtkComboBoxPrivate;
begin
  exit;
  AComboWidget := PGtkComboBox(Widget);
  APrivate := PGtkComboBoxPrivate(AComboWidget^.priv3);
  DebugLn('** COMBO DUMP OF PGtkComboBoxPrivate struct EVENT=',ADbgEvent);
  DebugLn('BUTTON=',dbgHex({%H-}PtrUInt(APrivate^.button)),' ARROW=',dbgHex({%H-}PtrUInt(APrivate^.arrow)),
    ' SCROLLEDWINDOW=',dbgHex({%H-}PtrUInt(APrivate^.scrolled_window)),
    ' CELLVIEW=',dbgHex({%H-}PtrUInt(APrivate^.cell_view)),
    ' CELLAREA=',dbgHex({%H-}PtrUInt(APrivate^.area)));
  DebugLn(' PrivatePopupW ',dbgHex({%H-}PtrUInt(APrivate^.popup_widget)),
  ' PrivatePopupWin ',dbgHex({%H-}PtrUInt(APrivate^.popup_window)),' TreeView ',dbgHex({%H-}PtrUInt(APrivate^.tree_view)));
  if Gtk3IsWidget(APrivate^.popup_widget) then
  begin
    DebugLn('POPUPWIDGET VISIBLE ',dbgs(APrivate^.popup_widget^.get_visible),
      ' PopupInProgress=',dbgs(APrivate^.popup_in_progress),' POPUPSHOWN=',
      dbgs(APrivate^.popup_shown),' POPUPIDLE_ID=',dbgs(APrivate^.popup_idle_id));
    if Gtk3IsMenu(APrivate^.popup_widget) then
      DebugLn('POPUPWIDGET IS MENU ')
    else
    if Gtk3IsMenuItem(APrivate^.popup_widget) then
      DebugLn('POPUPWIDGET IS MENUITEM ');
  end;
end;

function TGtk3ComboBox.CanFocus: Boolean;
begin
  Result := False;
  if IsWidgetOK then
  begin
    if PGtkComboBox(FWidget)^.has_entry then
      Result := PGtkComboBox(FWidget)^.get_child^.can_focus
    else
    if GetButtonWidget <> nil then
      Result := GetButtonWidget^.can_focus;
  end;
end;

procedure TGtk3ComboBox.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
var
  ARect: TGdkRectangle;
  Alloc: TGtkAllocation;
begin
  if (Widget=nil) then
    exit;

  LCLWidth := AWidth;
  LCLHeight := AHeight;
  ARect.x := ALeft;
  ARect.y := ATop;
  ARect.width := AWidth;
  ARect.Height := AHeight;
  with Alloc do
  begin
    x := ALeft;
    y := ATop;
    width := AWidth;
    height := AHeight;
  end;

  BeginUpdate;
  try
    {fixes gtk3 assertion}
    if not Widget^.get_realized then
      Widget^.realize;


    Widget^.size_allocate(@ARect);

    if Widget^.get_visible then
      Widget^.set_allocation(@Alloc);

    if LCLObject.Parent <> nil then
      Move(ALeft, ATop);
  finally
    EndUpdate;
  end;
end;

procedure TGtk3ComboBox.SetFocus;
begin
  {$IFDEF GTK3DEBUGFOCUS}
  DebugLn('TGtk3ComboBox.SetFocus LCLObject ',dbgsName(LCLObject),' WidgetOK ',dbgs(IsWidgetOK),
  ' FWidget <> GetContainerWidget ',dbgs(FWidget <> GetContainerWidget));
  {$ENDIF}
  if Assigned(LCLObject) then
  begin
    if IsWidgetOK then
    begin
      if PGtkComboBox(FWidget)^.has_entry then
        FWidget^.grab_focus
      else
      if GetButtonWidget <> nil then
        GetButtonWidget^.grab_focus;
    end else
      inherited SetFocus;
  end else
    inherited SetFocus;
end;

class procedure TGtk3ComboBox.ComboBoxChanged({%H-}ACombo: PGtkComboBox; AData: gpointer); cdecl;
var
  Msg: TLMessage;
begin
  if AData <> nil then
  begin
    if TGtk3Widget(AData).InUpdate then
      Exit;
    FillChar(Msg{%H-}, SizeOf(Msg), #0);
    Msg.Msg := LM_CHANGED;
    TGtk3Widget(AData).DeliverMessage(Msg);
  end;
end;

function GtkPopupCloseUp(AData: Pointer): gboolean; cdecl;
begin
  LCLSendCloseUpMsg(TGtk3Widget(AData).LCLObject);
  Result := False;// stop the timer
end;

class procedure TGtk3ComboBox.NotifySignal(AObject: PGObject; pspec: PGParamSpec; AData: GPointer); cdecl;
var
  AValue: TGValue;
  ComboBox: TCustomComboBox;
begin
  if pspec^.name = 'popup-shown' then
  begin
    ComboBox := TCustomComboBox(TGtk3Widget(AData).LCLObject);
    AValue.g_type := G_TYPE_BOOLEAN;
    g_object_get_property(AObject, pspec^.name, @AValue); // get property value
    if AValue.data[0].v_int = 0 then // if 0 = False then it is close up
      g_timeout_add(0,@GtkPopupCloseUp, AData)
    else // in other case it is drop down
    begin
      ComboBox.IntfGetItems;
      LCLSendDropDownMsg(ComboBox);
    end;
  end;
end;

procedure Gtk3ComboMenuRealized({%H-}AWidget: PGtkWidget; AData: gPointer); cdecl;
begin
  DebugLn('Gtk3ComboMenuRealized *****',dbgsName(TGtk3ComboBox(AData).LCLObject));
end;

procedure TGtk3ComboBox.InitializeWidget;
begin
  inherited InitializeWidget;

  if IsDesigning then
    exit;
  // appears-as-list make it appear as list ... no way, its read only property.
  //OnChange
  g_signal_connect_data(GetContainerWidget, 'changed', TGCallback(@ComboBoxChanged), Self, nil, G_CONNECT_DEFAULT);
  //OnCloseUp
  g_signal_connect_data(GetContainerWidget, 'notify', TGCallback(@NotifySignal), Self, nil, G_CONNECT_DEFAULT);

  //TODO: if we have an entry then use CreateFrom() to create TGtk3Entry
  if Gtk3IsEntry(PGtkComboBox(FWidget)^.get_child) then
  begin
    g_object_set_data(PGtkComboBox(FWidget)^.get_child, 'lclwidget', Self);
    g_signal_connect_data(PGtkComboBox(FWidget)^.get_child, 'event', TGCallback(@WidgetEvent), Self, nil, G_CONNECT_DEFAULT);
  end;
  if GetCellView <> nil then
  begin
    gtk_widget_set_events(FCellView, GDK_DEFAULT_EVENTS_MASK);
    g_object_set_data(FCellView, 'lclwidget', Self);
    g_signal_connect_data(FCellView, 'event', TGCallback(@WidgetEvent), Self, nil, G_CONNECT_DEFAULT);
  end;
  // set to all combo widgets lclwidget data, so we will easy find TGtk3ComboBox in events.
  if PGtkComboBox(GetContainerWidget)^.priv3^.button <> nil then
  begin
    g_object_set_data(PGObject(PGtkComboBox(GetContainerWidget)^.priv3^.button), 'lclwidget', Self);
    // invalid signal for type GtkCssCustomGadget
    // g_signal_connect_data(PGObject(PGtkComboBox(GetContainerWidget)^.priv3^.button), 'event', TGCallback(@WidgetEvent), Self, nil, G_CONNECT_DEFAULT);
  end;
  if PGtkComboBox(GetContainerWidget)^.priv3^.popup_widget <> nil then
  begin
    g_object_set_data(PGObject(PGtkComboBox(GetContainerWidget)^.priv3^.popup_widget), 'lclwidget', Self);
    // invalid signal for type GtkCssCustomGadget
    // g_signal_connect_data(PGObject(PGtkComboBox(GetContainerWidget)^.priv3^.popup_widget), 'event', TGCallback(@WidgetEvent), Self, nil, G_CONNECT_DEFAULT);

    //Stop gtk asserts. We must use signals on popup widget or event.
    //PGtkComboBox(GetContainerWidget)^.priv3^.popup_widget^.set_has_window(True);
    //PGtkComboBox(GetContainerWidget)^.priv3^.popup_widget^.set_can_focus(True);
    // g_signal_connect_data(PGObject(PGtkComboBox(GetContainerWidget)^.priv3^.popup_widget), 'map', TGCallback(@Gtk3ComboMenuRealized), Self, nil, 0);
  end;
  if PGtkComboBox(GetContainerWidget)^.priv3^.area <> nil then
    g_object_set_data(PGObject(PGtkComboBox(GetContainerWidget)^.priv3^.area), 'lclwidget', Self);
  // if combo doesnt use menu
  if PGtkComboBox(GetContainerWidget)^.priv3^.tree_view <> nil then
    g_object_set_data(PGObject(PGtkComboBox(GetContainerWidget)^.priv3^.tree_view), 'lclwidget', Self);
  // real popup menu
  if PGtkComboBox(GetContainerWidget)^.priv3^.box <> nil then
    g_object_set_data(PGObject(PGtkComboBox(GetContainerWidget)^.priv3^.box), 'lclwidget', Self);

end;

function TGtk3ComboBox.GetDroppedDown: boolean;
var
  AValue: TGValue;
begin
  Result := False;
  if Assigned(FWidget) and Gtk3IsComboBox(Widget) then
  begin
    AValue.g_type := G_TYPE_BOOLEAN;
    g_object_get_property(PGObject(Widget), 'popup-shown', @AValue);
    Result := AValue.data[0].v_int <> 0;
  end;
end;

class procedure TGtk3ComboBox.ComboSizeAllocate(AWidget:PGtkWidget; AGdkRect: PGdkRectangle;
  Data:gpointer); cdecl;
var
  Msg: TLMSize;
  NewSize: TSize;
  ACtl: TGtk3ComboBox;
  AState: TGdkWindowState;
  Alloc: TGtkAllocation;
begin
  if AWidget=nil then ;

  ACtl := TGtk3ComboBox(Data);

  {$IF DEFINED(GTK3DEBUGCOMBOBOX) OR DEFINED(GTK3DEBUGSIZE)}
  with AGdkRect^ do
    DebugLn('**** ComboSizeAllocate **** ....',dbgsName(ACtl.LCLObject),
      ' ',Format('GTK x %d y %d w %d h %d',[x, y, width, height]),
      Format(' LCL W=%d H=%d LLW %d LLH %d',[ACtl.LCLObject.Width, ACtl.LCLObject.Height, ACtl.LCLWidth, ACtl.LCLHeight]));
  {$ENDIF}

  with Alloc do
  begin
    x := AGdkRect^.x;
    y := AGdkRect^.y;
    Width := AGdkRect^.width;
    Height := AGdkRect^.height;
  end;

  //gtk3 combobox is pretty ugly with it's layout
  gtk_widget_set_clip(AWidget, @Alloc);

  if not Assigned(ACtl.LCLObject) then exit;

  // return size w/o frame
  NewSize.cx := AGdkRect^.width;
  NewSize.cy := AGdkRect^.height;


  if not (csDesigning in ACtl.LCLObject.ComponentState) then
  begin
    if ACtl.InUpdate then
      exit;
  end;

  if ((NewSize.cx <> ACtl.LCLObject.Width) or (NewSize.cy <> ACtl.LCLObject.Height) or
     ACtl.LCLObject.ClientRectNeedsInterfaceUpdate) then
  begin
    {TODO: check if this is needed}
    ACtl.LCLObject.DoAdjustClientRectChange;
  end;

  FillChar(Msg{%H-}, SizeOf(Msg), #0);

  Msg.Msg := LM_SIZE;
  Msg.SizeType := SIZE_RESTORED;

  Msg.SizeType := Msg.SizeType or Size_SourceIsInterface;

  Msg.Width := Word(NewSize.cx);
  Msg.Height := Word(NewSize.cy);
  ACtl.DeliverMessage(Msg);
end;

procedure TGtk3ComboBox.ConnectSizeAllocateSignal(ToWidget:PGtkWidget);
begin
  g_signal_connect_data(ToWidget,'size-allocate',TGCallback(@ComboSizeAllocate), Self, nil, G_CONNECT_DEFAULT);
end;

{ TGtk3Button }

function TGtk3Button.getLayout: Integer;
begin
  Result := FLayout;
  // PGtkButton(FWidget)^.get_image_position;
end;

function TGtk3Button.getMargin: Integer;
begin
  Result := FMargin;
end;

procedure TGtk3Button.SetLayout(AValue: Integer);
begin
  FLayout := AValue;
  if IsWidgetOk then
  begin
    PGtkButton(FWidget)^.set_image_position(TGtkPositionType(AValue));
    // set margin and spacing when layout is changed
    SetMargin(FMargin);
  end;
end;

procedure TGtk3Button.SetMargin(AValue: Integer);
begin
  FMargin := AValue;
  if not IsWidgetOK then
    exit;
  if FMargin = -1 then
    PGtkButton(FWidget)^.set_alignment(0.5, 0.5)
  else
  begin
    case FLayout of
      0 {GTK_POS_LEFT}: PGtkButton(FWidget)^.set_alignment(0, 0.5);
      1 {GTK_POS_RIGHT}: PGtkButton(FWidget)^.set_alignment(1.0, 0.5);
      2 {GTK_POS_TOP}: PGtkButton(FWidget)^.set_alignment(0.5, 0);
      3 {GTK_POS_BOTTOM}: PGtkButton(FWidget)^.set_alignment(0.5, 1);
    end;
  end;
end;

procedure TGtk3Button.SetSpacing(AValue: Integer);
var
  ATGValue: TGValue;
  AImage: PGtkWidget;
begin
  // if FSpacing=AValue then Exit;
  FSpacing:=AValue;
  if AValue < 0 then
    FSpacing := 2;
  ATGValue.g_type := G_TYPE_INT;
  ATGValue.set_int(AValue);

  // no way under gtk3 ... we cannot set style property image-spacing
  // so we are using cheat
  AImage := PGtkButton(FWidget)^.get_image;
  if AImage <> nil then
  begin
    if AValue < 0 then
      AVAlue := 0;
    //TODO: margin depends on layout ! This is ok for left (default) layout
    PGtkImage(AImage)^.set_margin_right(AValue);
  end;
end;

procedure TGtk3Button.SetImage(AImage: TBitmap);
begin
  if Assigned(fImage) then
    fImage.free;
  fImage:=AImage;
end;

function TGtk3Button.getText: String;
begin
  if IsWidgetOK then
    Result := {%H-}ReplaceUnderscoresWithAmpersands(PGtkButton(FWidget)^.get_label())
  else
    Result := '';
end;

procedure TGtk3Button.setText(const AValue: String);
begin
  if IsWidgetOk then
  begin
    {%H-}PGtkButton(FWidget)^.set_label(PgChar({%H-}ReplaceAmpersandsWithUnderscores(AValue)));
  end;
end;

function TGtk3Button.CreateWidget(const Params: TCreateParams): PGtkWidget;
var
  btn:PGtkButton absolute Result;
begin
  Result := LCLGtkButtonNew;

  btn^.set_use_underline(true);

  if not IsDesigning then
    LCLObject.ControlStyle:=LCLObject.ControlStyle+[csClickEvents];

  FMargin := -1;
  FLayout := ord(GTK_POS_LEFT);
  FSpacing := 2; // default gtk3 spacing is 2
end;

destructor TGtk3Button.Destroy;
begin
  SetImage(nil);
  inherited Destroy;
end;

class function TGtk3Button.ButtonMouseEvent(aWidget: PGtkWidget; aEvent: PGdkEvent; aData: gpointer): gboolean; cdecl;
begin
  Result := TGtk3Widget(aData).GtkEventMouse(aWidget, aEvent);
end;

function ButtonMotionNotifyEvent(widget: PGtkWidget; event: PGdkEvent; user_data: gpointer): gboolean; cdecl;
begin
  TGtk3Widget(user_data).GtkEventMouseMove(widget, event);
  Result := True;
end;

procedure TGtk3Button.InitializeWidget;
begin
  inherited InitializeWidget;
  if not IsDesigning then
  begin
    g_signal_connect_data(GetContainerWidget, 'button-press-event', TGCallback(@ButtonMouseEvent), Self, Nil, G_CONNECT_DEFAULT);
    g_signal_connect_data(GetContainerWidget, 'button-release-event', TGCallback(@ButtonMouseEvent), Self, Nil, G_CONNECT_DEFAULT);
  end;
end;

function TGtk3Button.IsWidgetOk: Boolean;
begin
  Result := (FWidget <> nil) and Gtk3IsButton(FWidget);
end;

procedure TGtk3Button.SetDefault(const ADefault: Boolean);
begin
  if IsWidgetOk then
    GetContainerWidget^.set_can_default(ADefault);
end;

{ TGtk3ToggleButton }
class procedure TGtk3ToggleButton.ButtonToggled({%H-}AWidget: PGtkToggleButton; AData: gPointer); cdecl;
var
  Msg: TLMessage;
begin
  FillChar(Msg{%H-}, SizeOf(Msg), 0);
  Msg.Msg := LM_CHANGED;
  if (TGtk3Widget(AData).LCLObject <> nil) and not TGtk3Widget(AData).InUpdate then
    TGtk3Widget(AData).DeliverMessage(Msg, False);
end;

procedure TGtk3ToggleButton.InitializeWidget;
begin
  inherited InitializeWidget;
  if not IsDesigning then
    g_signal_connect_data(FWidget, 'toggled', TGCallback(@ButtonToggled), Self, nil, G_CONNECT_DEFAULT);
end;

function TGtk3ToggleButton.CreateWidget(const Params: TCreateParams): PGtkWidget;
var
  btn: PGtkToggleButton;
begin
  btn := TGtkToggleButton.new;
  btn^.use_underline := True;
  Result := PGtkWidget(btn);
end;

{ TGtk3CheckBox }

function TGtk3CheckBox.GetState: TCheckBoxState;
begin
  Result := cbUnchecked;
  if IsWidgetOk then
  begin
    if PGtkCheckButton(FWidget)^.get_inconsistent then
      Result := cbGrayed
    else
    if PGtkCheckButton(FWidget)^.get_active then
      Result := cbChecked;
  end;
end;

procedure TGtk3CheckBox.SetState(AValue: TCheckBoxState);
begin
  if IsWidgetOK then
  begin
    if AValue = cbGrayed then
      PGtkCheckButton(FWidget)^.set_inconsistent(True)
    else
      PGtkCheckButton(FWidget)^.set_active(AValue = cbChecked);
  end;
end;

function TGtk3CheckBox.CreateWidget(const Params: TCreateParams): PGtkWidget;
var
  check: PGtkCheckButton;
begin
  check := TGtkCheckButton.new;
  Result := PGtkWidget(check);
  check^.set_use_underline(True);
end;

procedure TGtk3CheckBox.SetBounds(ALeft,ATop,AWidth,AHeight:integer);
var
  Alloc:TGtkAllocation;
begin
  if LCLObject.Name = 'HiddenRadioButton' then
    exit;
  LCLWidth := AWidth;
  LCLHeight := AHeight;
  // not needed
  // Widget^.set_size_request(AWidth, AHeight);
  Alloc.x := ALeft;
  Alloc.y := ATop;
  Alloc.width := AWidth;
  Alloc.height := AHeight;
  Widget^.set_allocation(@Alloc);
  Move(ALeft, ATop);
end;

{ TGtk3RadioButton }

function TGtk3RadioButton.CreateWidget(const Params: TCreateParams): PGtkWidget;
var
  btn: PGtkRadioButton;
  w: PGtkWidget;
  ctl, Parent: TWinControl;
  rb: TRadioButton;
  //pl: PGsList;
  i: Integer;
begin
  if Self.LCLObject.Name='HiddenRadioButton' then
    exit;
  btn := TGtkRadioButton.new(nil);
  btn^.use_underline := True;
  Result := PGtkWidget(btn);
  ctl := Self.LCLObject;
  if Assigned(ctl) then
  begin
    Parent := ctl.Parent;
    if (Parent is TRadioGroup) then
    begin
      if (TRadioGroup(Parent).Items.Count>0) then
      begin
        rb := TRadioButton(Parent.Controls[0]);
        if rb<>ctl then
        begin
          w := TGtk3RadioButton(rb.Handle).Widget;
          //pl := PGtkRadioButton(w)^.get_group;
          //PGtkRadioButton(Result)^.set_group(pl);
          PGtkRadioButton(Result)^.join_group(PGtkRadioButton(w));
        end;
      end
    end
    else
    begin
      for i := 0 to Parent.ControlCount - 1 do
        if (Parent.Controls[i] is TRadioButton) and
           TWinControl(Parent.Controls[i]).HandleAllocated then
        begin
          rb := TRadioButton(Parent.Controls[i]);
          w := TGtk3RadioButton(rb.Handle).Widget;
          //pl := PGtkRadioButton(w)^.get_group;
          //PGtkRadioButton(Result)^.set_group(pl);
          PGtkRadioButton(Result)^.join_group(PGtkRadioButton(w));
          Break;
        end;
    end;
  end;
end;

procedure TGtk3RadioButton.InitializeWidget;
begin
  if Self.LCLObject.Name='HiddenRadioButton' then
  begin
    exit;
   { PGtkRadioButton(Self.Widget)^.set_group(nil);
   // PGtkRadioButton(Self.Widget)^.set_inconsistent(true);
    PGtkRadioButton(Self.Widget)^.set_visible(false);}
  end;
  inherited InitializeWidget;
end;

function TGtk3RadioButton.getClientRect:TRect;
var
  Alloc:TGtkAllocation;
  R: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  //Famous "HiddenRadioButton"
  if (Widget = nil) then
    exit;
  Widget^.get_allocation(@Alloc);
  Result := Bounds(Alloc.x, Alloc.y, Alloc.Width, Alloc.Height);
  Types.OffsetRect(Result, -Result.Left, -Result.Top);
end;


{ TGtk3CustomControl }

function TGtk3CustomControl.CreateWidget(const Params: TCreateParams): PGtkWidget;
begin
  FHasPaint := True;
  FKeysToEat := [];

  FWidgetType := [wtWidget, wtLayout, wtTabControl, wtScrollingWin, wtCustomControl];

  // this hack is requred for controls without custom WS classes
  if LCLObject is TUpDown then
    include(FWidgetType,wtSpinEdit);

  Result := PGtkScrolledWindow(TGtkScrolledWindow.new(nil, nil));

  FCentralWidget := TGtkLayout.new(nil, nil);

  PGtkScrolledWindow(Result)^.add(FCentralWidget);

  PGtkScrolledWindow(Result)^.set_policy(GTK_POLICY_EXTERNAL, GTK_POLICY_EXTERNAL);

  Result^.set_can_focus(False);
  FCentralWidget^.set_can_focus(True);
  FCentralWidget^.set_app_paintable(True);
  PGtkScrolledWindow(Result)^.set_shadow_type(BorderStyleShadowMap[TCustomControl(LCLObject).BorderStyle]);
  if not (csDesigning in LCLObject.ComponentState) then
    g_object_set(PGObject(FCentralWidget), 'resize-mode', [GTK_RESIZE_QUEUE, nil]);
  gtk_layout_set_size(PGtkLayout(FCentralWidget), 1, 1);

  g_signal_connect_data(FCentralWidget,'size-allocate',TGCallback(@ScrolledLayoutSizeAllocate), Self, nil, G_CONNECT_DEFAULT);

  with PGtkScrolledWindow(Result)^.get_vadjustment^ do
    LCLVAdj := gtk_adjustment_new(value, lower, upper, step_increment, page_increment, page_size);
  with PGtkScrolledWindow(Result)^.get_hadjustment^ do
    LCLHAdj := gtk_adjustment_new(value, lower, upper, step_increment, page_increment, page_size);
end;

function TGtk3CustomControl.EatArrowKeys(const AKey: Word): Boolean;
begin
  Result := False;
end;

procedure TGtk3CustomControl.Update(ARect: PRect);
begin
  if IsWidgetOK then
  begin
    if (ARect <> nil) then
    begin
      if (aRect^.Width > 0) and (ARect^.Height > 0) then
      begin
        with ARect^ do
          GetContainerWidget^.queue_draw_area(Left, Top, Right - Left, Bottom - Top);
      end;
    end else
      GetContainerWidget^.queue_draw;
  end;
end;

procedure TGtk3CustomControl.DoBeforeLCLPaint;
var
  DC: TGtk3DeviceContext;
  NColor: TColor;
  P: TPoint;
begin
  inherited DoBeforeLCLPaint;
  if not Visible then
    exit;

  DC := TGtk3DeviceContext(Context);

  NColor := LCLObject.Color;
  P.X := Round(GetScrolledWindow^.get_hadjustment^.get_value);
  P.Y := Round(GetScrolledWindow^.get_vadjustment^.get_value);
  if (NColor <> clNone) and (NColor <> clDefault) then
  begin
    DC.CurrentBrush.Color := ColorToRGB(NColor);
    DC.fillRect(P.X, P.Y, LCLObject.Width, LCLObject.Height);
  end;

  if BorderStyle <> bsNone then
  begin
    DC.CurrentPen.Color := ColorToRGB(clBtnShadow); // not sure what color to use here?
    DC.drawRect(P.X, P.Y, LCLObject.Width, LCLObject.Height, False, True);
  end;
end;

procedure TGtk3CustomControl.InitializeWidget;
begin
  inherited InitializeWidget;
  if not IsDesigning then
  begin
    g_signal_connect_data(gtk_scrolled_window_get_hscrollbar(GetScrolledWindow), 'change-value',
      TGCallback(@RangeChangeValue), Self, nil, G_CONNECT_DEFAULT);
    g_signal_connect_data(gtk_scrolled_window_get_vscrollbar(GetScrolledWindow), 'change-value',
      TGCallback(@RangeChangeValue), Self, nil, G_CONNECT_DEFAULT);

    g_signal_connect_data(PGtkRange(gtk_scrolled_window_get_hscrollbar(GetScrolledWindow)),'value-changed',
      TGCallback(@RangeValueChanged), Self, nil, G_CONNECT_DEFAULT);
    g_signal_connect_data(PGtkRange(gtk_scrolled_window_get_vscrollbar(GetScrolledWindow)),'value-changed',
      TGCallback(@RangeValueChanged), Self, nil, G_CONNECT_DEFAULT);
  end;
end;

function TGtk3CustomControl.getViewport:PGtkViewport;
begin
  Result := PGtkViewport(PGtkScrolledWindow(Widget)^.get_child);
end;

procedure TGtk3CustomControl.preferredSize(var PreferredWidth,PreferredHeight:
  integer;WithThemeSpace:Boolean);
begin
  inherited preferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);
  if [wtCustomControl] * WidgetType <> [] then
  begin
    PreferredWidth := 0;
    PreferredHeight := 0;
  end;
end;

function TGtk3CustomControl.getClientRect: TRect;
var
  Allocation: TGtkAllocation;
  R: TRect;
  w, h, x, y, VOffset, HOffset: gint;
  AViewPort: PGtkViewport;
  Bar:PGtkScrollbar;
  AWindow: PGdkWindow;
  AHorzPolicy, AVertPolicy: TGtkPolicyType;
begin
  if [wtLayout] * WidgetType <> [] then
  begin
    Result := Rect(0, 0, 0, 0);
    AWindow := PGtkLayout(getContainerWidget)^.get_window;
    if (AWindow <> nil) and Gtk3IsGdkWindow(AWindow) then
    begin
      HOffset := 0;
      VOffset := 0;
      gtk_scrolled_window_get_policy(PGtkScrolledWindow(Widget), @AHorzPolicy, @AVertPolicy);
      if AHorzPolicy < GTK_POLICY_NEVER then
      begin
        Bar := getHorizontalScrollbar;
        if (Bar <> nil) and Gtk3IsWidget(Bar) and Bar^.get_visible and GTK3WidgetSet.OverlayScrolling then
          HOffset := Bar^.get_allocated_height;
      end;
      if AVertPolicy < GTK_POLICY_NEVER then
      begin
        Bar := getVerticalScrollbar;
        if (Bar <> nil) and Gtk3IsWidget(Bar) and Bar^.get_visible and GTK3WidgetSet.OverlayScrolling then
          VOffset := Bar^.get_allocated_width
      end;

      Result := Rect(0, 0, AWindow^.get_width - VOffset, AWindow^.get_height - HOffset);
    end else
    begin
      //we are not ready, provide at least scrolledwindow size as clientrect for now.
      Result := Rect(0, 0, PGtkLayout(GetContainerWidget)^.get_allocated_width, PGtkLayout(GetContainerWidget)^.get_allocated_height);
      if (Result.Width <= 1) and (Result.Height <= 1) then
        Result := Rect(0, 0, Widget^.get_allocated_width, Widget^.get_allocated_height);
    end;
  end else //we are wtContext - GtkFixed based.
  begin
    AViewport := getViewport;
    if Gtk3IsViewPort(AViewPort) and Gtk3IsGdkWindow(AViewPort^.get_view_window) then
    begin
      AViewPort^.get_view_window^.get_geometry(@x, @y, @w, @h);

      Bar := getHorizontalScrollbar;
      if (Bar <> nil) and Gtk3IsWidget(Bar) and Bar^.get_visible and GTK3WidgetSet.OverlayScrolling then
        HOffset := Bar^.get_allocated_height
      else
        HOffset := 0;

      Bar := getVerticalScrollbar;
      if (Bar <> nil) and Gtk3IsWidget(Bar) and Bar^.get_visible and GTK3WidgetSet.OverlayScrolling then
        VOffset := Bar^.get_allocated_width
      else
        VOffset := 0;

      Result := Rect(0, 0, AViewPort^.get_view_window^.get_width - VOffset, AViewPort^.get_view_window^.get_height - HOffset);

      {$IFDEF GTK3DEBUGSIZE}
      DebugLn('TGtk3CustomControl.GetClientRect via Viewport ',dbgsName(LCLObject),' Result ',dbgs(Result),' X=',dbgs(X),' Y=',dbgs(Y),' AllocH=',dbgs(AViewPort^.get_allocated_height),' OffsetH=',HOffset.ToString,' VOffset=',VOffset.ToString);
      getContainerWidget^.get_allocation(@Allocation);
      with ALlocation do
      begin
        DebugLn(Format('  GtkFixed alloc x %d y %d width %d height %d',[x, y, width, height]));
      end;
      {$ENDIF}

      exit; // we are done here
    end else
    begin
      FCentralWidget^.get_allocation(@Allocation);
      if (Allocation.x = -1) and (Allocation.y = -1) and (Allocation.width <= 1) and (Allocation.Height <= 1) then
        FWidget^.get_allocation(@Allocation);
    end;

    with Allocation do
      R := Rect(x, y, width + x, height + y);

    if IsRectEmpty(R) then
      R := Rect(0, 0, 0, 0);

    Result := R;
    {$IFDEF GTK3DEBUGSIZE}
    DebugLn('TGtk3CustomControl.GetClientRect via GtkFixed ',dbgsName(LCLObject),' Result=',dbgs(Result));
    {$ENDIF}

    // DebugLn('TGtk3CustomControl.GetClientRect normal ',dbgsName(LCLObject),' Result ',dbgs(Result));
    Types.OffsetRect(Result, -Result.Left, -Result.Top);

  end;
end;

function TGtk3CustomControl.getHorizontalScrollbar: PGtkScrollbar;
var
  HPolicy:TGtkPolicyType;
  VPolicy:TGtkPolicyType;
begin
  Result := nil;
  if not IsWidgetOk then
    exit;
  PGtkScrolledWindow(Widget)^.get_policy(@HPolicy, @VPolicy);
  if HPolicy >= GTK_POLICY_NEVER then
    exit;
  Result := PGtkScrollBar(PGtkScrolledWindow(Widget)^.get_hscrollbar);
  g_object_set_data(Result,'lclwidget',Self);
end;

function TGtk3CustomControl.getVerticalScrollbar: PGtkScrollbar;
var
  VPolicy:TGtkPolicyType;
  HPolicy:TGtkPolicyType;
begin
  Result := nil;
  if not IsWidgetOk then
    exit;
  PGtkScrolledWindow(Widget)^.get_policy(@HPolicy, @VPolicy);
  if VPolicy >= GTK_POLICY_NEVER then
    exit;
  Result := PGtkScrollBar(PGtkScrolledWindow(Widget)^.get_vscrollbar);
  g_object_set_data(Result,'lclwidget',Self);
end;

function TGtk3CustomControl.GetScrolledWindow: PGtkScrolledWindow;
begin
  if IsWidgetOK then
    Result := PGtkScrolledWindow(Widget)
  else
    Result := nil;
end;

{ TGtk3ToolBar }

function TGtk3ToolBar.CreateWidget(const Params: TCreateParams): PGtkWidget;
begin
  Result := inherited CreateWidget(Params);
  Include(FWidgetType, wtToolbar);
end;

{ TGtk3ScrollingWinControl }

class procedure TGtk3ScrollingWinControl.ScrollingWinControlFixedSizeAllocate(AWidget: PGtkWidget;
  AGdkRect: PGdkRectangle; Data: gpointer); cdecl;
var
  hadj, vadj: PGtkAdjustment;
  HSize,VSize: Integer;
begin
  //writeln('GtkFixed size-allocate x=',AGdkRect^.x,' Y=',AGdkRect^.y,' Width=',AGdkRect^.width,' H=',AGdkRect^.height,' VAdj ? ',Assigned(TGtk3CustomControl(Data).LCLVAdj));
  VSize := 0;
  HSize := 0;
  hadj := TGtk3ScrollableWin(Data).GetScrolledWindow^.get_hadjustment;
  vadj := TGtk3ScrollableWin(Data).GetScrolledWindow^.get_vadjustment;

  if Assigned(TGtk3ScrollableWin(Data).LCLVAdj) and Gtk3IsAdjustment(vadj) then
    with TGtk3ScrollableWin(Data).LCLVAdj^ do
    begin
      VSize := Round(upper);
      if page_size > 0 then
        VSize := VSize + Round(upper - page_size);
    end;

  if Assigned(TGtk3ScrollableWin(Data).LCLHAdj) and Gtk3IsAdjustment(hadj) then
    with TGtk3ScrollableWin(Data).LCLHAdj^ do
    begin
      HSize := Round(upper);
      if page_size > 0 then
        HSize := HSize + Round(upper - page_size);
    end;

  HSize := Max(AGdkRect^.Width, HSize);
  VSize := Max(AGdkRect^.Height, VSize);

  //TODO: check if call resizing is needed when GtkFixed equals new size !

  PGtkFixed(Awidget)^.set_size_request(HSize, VSize);

  {TODO: eg treeview editor, if scrollbar value > 0 then editor resets position to 0,
   to fix this we must position editor at y pos - adjustment.value}
  if Assigned(TGtk3ScrollableWin(Data).LCLVAdj) and Gtk3IsAdjustment(vadj) then
    with TGtk3ScrollableWin(Data).LCLVAdj^ do
      vadj^.configure({vadj^.}value, lower, upper, step_increment, page_increment, page_size);

  if Assigned(TGtk3ScrollableWin(Data).LCLHAdj) and Gtk3IsAdjustment(hadj) then
    with TGtk3ScrollableWin(Data).LCLHAdj^ do
      hadj^.configure({hadj^.}value, lower, upper, step_increment, page_increment, page_size);

  if TGtk3ScrollableWin(Data).LCLObject.ClientRectNeedsInterfaceUpdate then
    TGtk3ScrollableWin(Data).LCLObject.DoAdjustClientRectChange;

end;

function TGtk3ScrollingWinControl.CreateWidget(const Params: TCreateParams
  ): PGtkWidget;
begin
  FHasPaint := True;
  Result := inherited CreateWidget(Params);
  Include(FWidgetType, wtScrollingWinControl);
end;

{ TGtk3Window }

function TGtk3Window.GetTitle: String;
begin
  if Gtk3IsGtkWindow(fWidget) then
    Result:=PGtkWindow(fWidget)^.get_title()
  else
    Result:=''
end;

procedure TGtk3Window.SetIcon(AValue: PGdkPixBuf);
begin
  // if FIcon=AValue then Exit;
  if Assigned(FIcon) then
  begin
    FIcon^.unref;
    FIcon := nil;
  end;
  if Gtk3IsGdkPixbuf(AValue) then
    FIcon := PGdkPixbuf(AValue)^.copy
  else
    FIcon := nil;
  //  DebugLn('Setting icon ',dbgHex(PtrUInt(FIcon)),' AppIcon ',dbgHex(PtrUInt(GTK3WidgetSet.AppIcon)));
  if Gtk3IsGtkWindow(fWidget) then
    PGtkWindow(Widget)^.set_icon(FIcon);
end;

function TGtk3Window.GetSkipTaskBarHint: Boolean;
begin
  Result := False;
  if Gtk3IsGtkWindow(fWidget) then
    Result := PGtkWindow(Widget)^.get_skip_taskbar_hint;
end;

procedure TGtk3Window.SetSkipTaskBarHint(AValue: Boolean);
begin
  if Gtk3IsGtkWindow(fWidget) then
    PGtkWindow(Widget)^.set_skip_taskbar_hint(AValue);
end;

procedure TGtk3Window.SetTitle(const AValue: String);
begin
  if Gtk3IsGtkWindow(fWidget) then
    {%H-}PGtkWindow(FWidget)^.set_title(PGChar(AValue));
end;

class function TGtk3Window.WindowStateSignal(AWidget: PGtkWidget;
  AEvent: PGdkEvent; AData: gPointer): gboolean; cdecl;
var
  Msg: TLMSize;
  AState: TGdkWindowState;
  //AScreen: PGdkScreen;
  msk: TGdkWindowState;
begin
  Result := False;
  FillChar(Msg{%H-}, SizeOf(Msg), #0);

  Msg.Msg := LM_SIZE;
  Msg.SizeType := SIZE_RESTORED;

  msk:=AEvent^.window_state.changed_mask;
  AState:=AEvent^.window_state.new_window_state;

  if GDK_WINDOW_STATE_ICONIFIED in msk then
  begin
    if GDK_WINDOW_STATE_ICONIFIED in AState then
      Msg.SizeType := SIZE_MINIMIZED
  end else
  if GDK_WINDOW_STATE_MAXIMIZED in msk then
  begin
    if GDK_WINDOW_STATE_MAXIMIZED in AState then
      Msg.SizeType := SIZE_MAXIMIZED
  end else
  if GDK_WINDOW_STATE_FULLSCREEN in msk then
  begin
    if GDK_WINDOW_STATE_FULLSCREEN in AState then
      Msg.SizeType := SIZE_FULLSCREEN
  end else
  if GDK_WINDOW_STATE_FOCUSED in msk then
  begin
    {$IFDEF GTK3DEBUGWINDOWSTATE}
    if GDK_WINDOW_STATE_FOCUSED in AState then
      DebugLn('Gtk3WindowState: Focused')
    else
      DebugLn('Gtk3WindowState: Defocused');
    {$ENDIF}
    exit;
  end else
  if GDK_WINDOW_STATE_WITHDRAWN in msk then
  begin
    {$IFDEF GTK3DEBUGWINDOWSTATE}
    if GDK_WINDOW_STATE_WITHDRAWN in AState then
      DebugLn('Gtk3WindowState: Shown')
    else
      DebugLn('Gtk3WindowState: Hidden');
    {$ENDIF}
    exit;
  end else
  begin
    //DebugLn(format('other changes state=%.08x mask=%.08x',[AState,msk]));
    exit;
  end;

  Msg.SizeType := Msg.SizeType or Size_SourceIsInterface;

  Msg.Width := Word(AWidget^.window^.get_width);
  Msg.Height := Word(AWidget^.window^.get_height);
  {$IFDEF GTK3DEBUGWINDOWSTATE}
  DebugLn('GetWindowState SizeType=',dbgs(Msg.SizeType),' realized ',dbgs(AWidget^.get_realized));
  {$ENDIF}
  TGtk3Window(AData).DeliverMessage(Msg);
  // DeliverMessage(Msg);
end;

class procedure TGtk3Window.WindowSizeAllocate(AWidget:PGtkWidget;AGdkRect:
  PGdkRectangle;Data:gpointer);cdecl;
var
  Msg: TLMSize;
  NewSize: TSize;
  ACtl: TGtk3Window;
  AState: TGdkWindowState;
  Alloc: TGtkAllocation;
  ADefW, ADefH: gint;
begin
  if AWidget=nil then ;

  ACtl := TGtk3Window(Data);

  //When gtk3 form is shown for the first time under some window manager there's
  //geometry out of sync. We fix it via map-event and WidgetMapped property.
  if g_object_get_data(aWidget,'lcl-window-first-map') <> nil then
  begin
    g_object_set_data(aWidget,'lcl-window-first-map', nil);
    if not IsRectEmpty(TGtk3Window(ACtl).FFirstMapRect) then
    begin
      with TGtk3Window(ACtl) do
        Types.OffsetRect(FFirstMapRect, -FFirstMapRect.Left, -FFirstMapRect.Top);
      AGdkRect^ := GdkRectFromRect(TGtk3Window(ACtl).FFirstMapRect);
      TGtk3Window(ACtl).FFirstMapRect := Rect(0, 0, 0, 0);
    end;
  end;

  {$IFDEF GTK3DEBUGFORMS}
  if Assigned(ACtl.LCLObject) then
  begin
    with ACtl.LCLObject do
    begin
      writeln(Format('TGtk3Window.WindowSizeAllocate %s Gdk x %d y %d w %d h %d  LCL l %d t %d w %d h %d applied w %d h %d cliRect %s WMap %s',[dbgsName(ACtl.LCLObject), AGdkRect^.x, AGdkRect^.y, AGdkRect^.width, AGdkRect^.height, Left, Top, Width, Height, ACtl.LCLWidth, ACtl.LCLHeight, dbgs(ACtl.LCLObject.ClientRect), BoolToStr(ACtl.WidgetMapped, True)]));
    end;
    if (AGdkRect^.x = 0) and (AGdkRect^.y = 0) and (AGdkRect^.width = 200) and (AGdkRect^.height = 200) and ACtl.WidgetMapped then
    begin
      // this is wrong size. This one is sent by kwin.
      writeln('***** Dump size values  for ',G_OBJECT_TYPE_NAME(AWidget));
      with PGtkWindow(AWidget)^ do
      begin
        get_allocation(@Alloc);
        get_size(@NewSize.cx, @NewSize.cy);
        get_default_size(@ADefW, @ADefH);
      end;
      with Alloc do
        writeln(Format('WSA Alloc x %d y %d w %d h %d NS w %d h %d DEF w %d h %d', [x, y, width, height, NewSize.cx, NewSize.cy, ADefW, ADefH]));

    end;
  end;
  {$ENDIF}

  NewSize.cx := AGdkRect^.width;
  NewSize.cy := AGdkRect^.height;

  //writeln(format('Gkt3SizeAllocate w=%d h=%d',[NewSize.cx,NewSize.cy]));

  if not Assigned(ACtl.LCLObject) then exit;

  // do not loop with LCL  !
  if not (csDesigning in ACtl.LCLObject.ComponentState) and ACtl.InUpdate then
    exit;

  FillChar(Msg{%H-}, SizeOf(Msg), #0);

  Msg.Msg := LM_SIZE;
  Msg.SizeType := SIZE_RESTORED;

  AState := TGtk3Window(ACtl).getWindowState;
  if GDK_WINDOW_STATE_ICONIFIED in AState  then
    Msg.SizeType := SIZE_MINIMIZED
  else
  if GDK_WINDOW_STATE_MAXIMIZED in AState  then
    Msg.SizeType := SIZE_MAXIMIZED
  else
  if GDK_WINDOW_STATE_FULLSCREEN in AState  then
    Msg.SizeType := SIZE_FULLSCREEN;

  Msg.SizeType := Msg.SizeType or Size_SourceIsInterface;

  Msg.Width := Word(NewSize.cx);
  Msg.Height := Word(NewSize.cy);
  ACtl.DeliverMessage(Msg);
end;

class function TGtk3Window.WindowMapEvent(awidget: PGtkWindow; AEvent: PGdkEventAny; adata: gpointer): gboolean; cdecl;
var
  wx:gint;
  wy:gint;
  w:gint;
  h:gint;
  Alloc:TGtkAllocation;
begin
  if Gtk3IsGtkWindow(aWidget) then
  begin
    gtk_window_get_position(aWidget, @wx, @wy);
    gtk_window_get_size(aWidget, @w, @h);
  end;
  gtk_widget_get_allocation(aWidget, @Alloc);
  //under some window managers there's discrepancy gdk window have it's default gtk size, not lcl one
  //so sends 2 size-allocate events. I've introduced "lcl-window-first-map" gobject data to control
  //if we are out of sync. Alloc contains correct data.
  if not TGtk3Widget(AData).WidgetMapped and ((Alloc.width <> w) or (Alloc.height <> h) or (wx <> Alloc.x) or (wy <> Alloc.y)) then
  begin
    g_object_set_data(aWidget,'lcl-window-first-map', aData);
    TGtk3Window(aData).FFirstMapRect := RectFromGdkRect(Alloc);
  end;
  TGtk3Widget(AData).WidgetMapped := True;
  Result := gtk_false;
end;

class function TGtk3Window.WindowMoveEvent(awidget: PGtkWindow; AEvent: PGdkEventConfigure; adata: gpointer): gboolean; cdecl;
var
  MoveMsg: TLMMove;
begin
  if Gtk3IsGtkWindow(aWidget) then
  begin
    with MoveMsg do
    begin
      Result := 0;
      Msg := LM_MOVE;
      MoveType := Move_SourceIsInterface;
      XPos := SmallInt(AEvent^.x);
      YPos := SmallInt(AEvent^.y);
    end;
    Result := TGtk3Window(aData).DeliverMessage(MoveMsg) <> 0;
  end else
    Result := gtk_false;
end;

procedure TGtk3Window.ConnectSizeAllocateSignal(ToWidget:PGtkWidget);
begin
  g_signal_connect_data(ToWidget,'size-allocate',TGCallback(@WindowSizeAllocate), Self, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(ToWidget,'map-event',TGCallback(@WindowMapEvent), Self, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(ToWidget,'configure-event',TGCallback(@WindowMoveEvent), Self, nil, G_CONNECT_DEFAULT);
end;

class function TGtk3Window.decoration_flags(Aform: TCustomForm): TGdkWMDecoration;
var
  icns:TBorderIcons;
  bs:TFormBorderStyle;
begin
  Result := [];
  icns:=AForm.BorderIcons;
  bs:=AForm.BorderStyle;

  case bs of
  bsSingle: Include(Result, GDK_DECOR_TITLE{GDK_DECOR_BORDER});
  bsDialog:
      Result += [GDK_DECOR_BORDER, GDK_DECOR_TITLE];
  bsSizeable:
    begin
      if biMaximize in icns then
        Include(Result, GDK_DECOR_MAXIMIZE);
      if biMinimize in icns then
        Include(Result, GDK_DECOR_MINIMIZE);
      Result += [GDK_DECOR_BORDER, GDK_DECOR_RESIZEH, GDK_DECOR_TITLE];
    end;
  bsSizeToolWin:
    Result += [GDK_DECOR_BORDER, GDK_DECOR_RESIZEH, GDK_DECOR_TITLE];
  bsToolWindow:
    Include(Result, GDK_DECOR_BORDER);
  bsNone: Result := [];
  end;

  if GDK_DECOR_TITLE in Result then
  if biSystemMenu in icns then
     Include(Result, GDK_DECOR_MENU);
end;

procedure TGtk3Window.DoBeforeLCLPaint;
var
  DC: TGtk3DeviceContext;
  NColor: TColor;
begin
  inherited DoBeforeLCLPaint;
  if not Visible then
    exit;

  DC := TGtk3DeviceContext(Context);

  NColor := LCLObject.Color;
  if (NColor <> clNone) and (NColor <> clDefault) and (NColor <> clForm) then
  begin
    DC.CurrentBrush.Color := ColorToRGB(NColor);
    DC.fillRect(0, 0, LCLObject.Width, LCLObject.Height);
  end;

  if BorderStyle <> bsNone then
  begin
    DC.CurrentPen.Color := ColorToRGB(clBtnShadow); // not sure what color to use here?
    DC.drawRect(0, 0, LCLObject.Width, LCLObject.Height, False, True);
  end;
end;

function TGtk3Window.ShowState(nstate:integer):boolean; // winapi ShowWindow
var
  AState: TGdkWindowState;
begin
  if not Gtk3IsGtkWindow(fWidget) then
    exit(false);
  case nstate of
    SW_HIDE: PGtkWindow(FWidget)^.hide;
    SW_SHOWNORMAL:
      begin
        AState:=fWidget^.window^.get_state;
        if GDK_WINDOW_STATE_ICONIFIED in AState then
          PgtkWindow(fWidget)^.deiconify
        else if GDK_WINDOW_STATE_MAXIMIZED in AState then
          PgtkWindow(fWidget)^.unmaximize
        else if GDK_WINDOW_STATE_FULLSCREEN in AState then
          PgtkWindow(fWidget)^.unfullscreen
        else
          PgtkWindow(fWidget)^.show;
      end;
    SW_SHOWMAXIMIZED: PgtkWindow(fWidget)^.maximize;
    SW_MINIMIZE: PgtkWindow(fWidget)^.iconify;
    SW_SHOWFULLSCREEN: PgtkWindow(fWidget)^.fullscreen;
  else
    PgtkWindow(fWidget)^.show;
  end;
  Result:=true
end;

procedure TGtk3Window.UpdateWindowState; // LCL WindowState
const
  ShowCommands: array[TWindowState] of Integer =
      (SW_SHOWNORMAL, SW_MINIMIZE, SW_SHOWMAXIMIZED, SW_SHOWFULLSCREEN);
begin
  ShowState(ShowCommands[TCustomForm(LCLObject).WindowState]);
end;

function TGtk3Window.CreateWidget(const Params: TCreateParams): PGtkWidget;
var
  AForm: TCustomForm;
  decor: TGdkWMDecoration;
begin
  FIcon := nil;
  FFirstMapRect := Rect(0, 0, 0, 0);

  FHasPaint := True;
  FMenuBar := nil;
  AForm := TCustomForm(LCLObject);

  if not Assigned(LCLObject.Parent) then
  begin
    if FWidgetType = [wtHintWindow] then
      Result := TGtkWindow.new(GTK_WINDOW_POPUP)
    else
      Result := TGtkWindow.new(GTK_WINDOW_TOPLEVEL);
    FWidget:=Result;
    FWidget^.set_events(GDK_DEFAULT_EVENTS_MASK);
    Title:=Params.Caption;
    if FWidgetType = [wtHintWindow] then
      decor := []
    else
      decor:=decoration_flags(AForm);
    gtk_window_set_decorated(PGtkWindow(Result),(decor <> []));
    if AForm.AlphaBlend then
      gtk_widget_set_opacity(Result, TForm(LCLObject).AlphaBlendValue/255);
    if not gtk_window_get_decorated(PGtkWindow(Result)) then
    begin
      if FWidgetType = [wtHintWindow] then
        gtk_window_set_type_hint(PGtkWindow(Result), GDK_WINDOW_TYPE_HINT_TOOLTIP)
      else
        gtk_window_set_type_hint(PGtkWindow(Result), GDK_WINDOW_TYPE_HINT_UTILITY);
    end;
    if FWidgetType = [wtHintWindow] then
      FWidgetType := [wtWidget, wtLayout, wtScrollingWin, wtScrollingWinControl, wtWindow, wtHintWindow]
    else
      FWidgetType := [wtWidget, wtLayout, wtScrollingWin, wtScrollingWinControl, wtWindow];
  end else
  begin
    Result := PGtkScrolledWindow(TGtkScrolledWindow.new(nil, nil));
    FWidgetType := [wtWidget, wtLayout, wtScrollingWin, wtCustomControl]
  end;
  Text := Params.Caption;

  FBox := TGtkVBox.new(GTK_ORIENTATION_VERTICAL, 0);

  FScrollWin := PGtkScrolledWindow(TGtkScrolledWindow.new(nil, nil));
  g_object_set_data(FScrollWin,'lclscrollingwindow',GPointer(1));
  g_object_set_data(PGObject(FScrollWin), 'lclwidget', Self);

  FCentralWidget := TGtkLayout.new(nil, nil);
  FScrollWin^.add(FCentralWidget);
  FScrollWin^.show;
  FBox^.pack_end(FScrollWin, True, True, 0);
  FBox^.show;

  FScrollWin^.get_vscrollbar^.set_can_focus(False);
  FScrollWin^.get_hscrollbar^.set_can_focus(False);
  FScrollWin^.set_policy(GTK_POLICY_NEVER, GTK_POLICY_NEVER);
  PGtkContainer(Result)^.add(FBox);

  g_signal_connect_data(Result,'window-state-event', TGCallback(@WindowStateSignal), Self, nil, G_CONNECT_DEFAULT);

  g_object_set(PGObject(FCentralWidget), 'resize-mode', [GTK_RESIZE_QUEUE, nil]);
  gtk_layout_set_size(PGtkLayout(FCentralWidget), 1, 1);

  g_signal_connect_data(FCentralWidget,'size-allocate',TGCallback(@ScrolledLayoutSizeAllocate), Self, nil, G_CONNECT_DEFAULT);

  with PGtkScrolledWindow(FScrollWin)^.get_vadjustment^ do
    LCLVAdj := gtk_adjustment_new(value, lower, upper, step_increment, page_increment, page_size);
  with PGtkScrolledWindow(FScrollWin)^.get_hadjustment^ do
    LCLHAdj := gtk_adjustment_new(value, lower, upper, step_increment, page_increment, page_size);

  gtk_widget_realize(Result);

  if wtHintWindow in FWidgetType then
  begin
    PGtkWindow(Result)^.show_all;
  end else
  begin
    if not Assigned(LCLObject.Parent) then
      gdk_window_set_decorations(Result^.window, decor);

    if not (csDesigning in AForm.ComponentState) then
      UpdateWindowState;
  end;
  Result^.Hide; // issue #41412
end;

function TGtk3Window.EatArrowKeys(const AKey: Word): Boolean;
begin
  Result := False;
end;

function TGtk3Window.getText: String;
begin
  // query widget
  Result:=Title;
  // return cached
  if Result='' then
    Result := inherited GetText;
end;

procedure TGtk3Window.setText(const AValue: String);
begin
  // set cached text
  inherited SetText(AValue);
  // set widget text
  Title := AValue;
end;

{$IFDEF GTK3DEBUGFORMS}
procedure ChildCallback(Child: PGtkWidget; Data: gpointer); cdecl;
var
  Level: PtrInt;
  S: string;
  I:Integer;
begin
  // Log the child widget type
  S := '';
  Level := PtrInt(Data^);
  for I := 1 to Level - 1 do
    S := S + ' ';
  WriteLn(S + 'Found child widget of type: ', g_type_name(PGObject(Child)^.g_type_instance.g_class^.g_type));
  if Gtk3IsContainer(Child) then
  begin
    inc(Level);
    gtk_container_foreach(PGtkContainer(Child), @ChildCallback, @Level);
  end;
end;
{$ENDIF}

function TGtk3Window.getViewport:PGtkViewport;
var
  W: PGtkWidget;
  {$IFDEF GTK3DEBUGFORMS}
  AInt: PtrInt;
  {$ENDIF}
begin
  W := FScrollWin^.get_child;
  if Gtk3IsViewPort(W) then
    Result := PGtkViewport(W)
  else
    Result := nil;
  {$IFDEF GTK3DEBUGFORMS}
  if Result <> nil then
    writeln('TGtk3Window.GetViewport: result class is ' +g_type_name(PGObject(Result)^.g_type_instance.g_class^.g_type)+ '. Found type=',g_type_name(PGObject(W)^.g_type_instance.g_class^.g_type),' g_type=',PGObject(W)^.g_type_instance.g_class^.g_type,' g_type_is_a=',g_type_is_a(PGObject(W)^.g_type_instance.g_class^.g_type, gtk_viewport_get_type))
  else
  begin
    writeln('TGtk3Window.GetViewport: result is nil, check if we are GtkLayout ? ',Gtk3IsLayout(W));
    if Gtk3IsLayout(W) then
    begin
      AInt := 1;
      gtk_container_foreach(PGtkContainer(W), @ChildCallback, @AInt);
    end;
  end;
  {$ENDIF}
end;

function TGtk3Window.getClientRect: TRect;
var
  Allocation: TGtkAllocation;
  R: TRect;
  w: gint;
  h: gint;
  x: gint;
  y: gint;
  AViewPort: PGtkViewport;
  MenuSize:Integer;
begin
  AViewPort := PGtkViewPort(FCentralWidget^.get_parent);
  if WidgetMapped and Gtk3IsViewPort(AViewPort) and Gtk3IsGdkWindow(AViewPort^.get_view_window) then
  begin
    AViewPort^.get_view_window^.get_geometry(@x, @y, @w, @h);
    Result := Rect(0, 0, AViewPort^.get_view_window^.get_width, AViewPort^.get_view_window^.get_height);
    // DebugLn('GetClientRect via Viewport ',dbgsName(LCLObject),' Result ',dbgs(Result));
    exit;
  end else
  begin
    if not FCentralWidget^.get_realized and not FCentralWidget^.get_mapped then
    begin
      // calculate our own client rect somehow.
      if (LCLObject is TCustomForm) then
      begin
        MenuSize := 0;
        if (TCustomForm(LCLObject).Menu <> nil) or (FMenuBar <> nil) then
          MenuSize := GetSystemMetrics(SM_CYMENU)
        else
          MenuSize := 0;
        Allocation.x := LCLObject.Left;
        Allocation.y := LCLObject.Top;
        Allocation.width := LCLObject.Width - 1; // border
        Allocation.Height := LCLObject.Height - MenuSize - 1;
      end else
      begin
        Allocation.X := -1;
        Allocation.Y := -1;
        Allocation.Width := 1;
        Allocation.Height := 1;
      end;
    end else
      FCentralWidget^.get_allocation(@Allocation);
  end;

  with Allocation do
    R := Rect(x, y, width + x, height + y);

  if IsRectEmpty(R) then
    R := Rect(0, 0, 0, 0);

  Result := R;
  Types.OffsetRect(Result, -Result.Left, -Result.Top);

  if GTK3WidgetSet.OverlayScrolling and getHorizontalScrollbar^.is_visible then
    Result.Height := Result.Height - getHorizontalScrollbar^.get_allocated_height;
  if GTK3WidgetSet.OverlayScrolling and getVerticalScrollbar^.is_visible then
    Result.Width := Result.Width - getVerticalScrollbar^.get_allocated_width;

  {$IFDEF GTK3DEBUGFORMS}
  DebugLn('TGtk3Window.GetClientRect ',dbgsName(LCLObject),' Result ',dbgs(Result),' CentralWidget mapped ? ',dbgs(FCentralWidget^.get_mapped),' Realized ? ',dbgs(FCentralWidget^.get_realized));
  {$ENDIF}
end;

procedure TGtk3Window.SetBounds(ALeft,ATop,AWidth,AHeight:integer);
var
  ARect: TGdkRectangle;
  Geometry: TGdkGeometry;
  AHints: TGdkWindowHints;
  AFixedWidthHeight: Boolean;
  AForm: TCustomForm;
  AMinSize, ANaturalSize: gint;
  Alloc:TGtkAllocation;
  x, y: gint;
begin
  AForm := TCustomForm(LCLObject);
  BeginUpdate;
  ARect.x := ALeft;
  ARect.y := ATop;
  ARect.width := AWidth;
  ARect.Height := AHeight;
  try
    {fixes gtk3 assertion}
    Widget^.get_preferred_width(@AMinSize, @ANaturalSize);
    Widget^.get_preferred_height(@AMinSize, @ANaturalSize);
    Widget^.get_allocation(@Alloc);
    {$IFDEF GTK3DEBUGFORMS}
    with Alloc do
      DebugLn(Format('TGtk3Window.setBounds(%d, %d, %d, %d) Natural w=%d h=%d alloc x %d y %d w %d h %d',[ALeft, ATop ,AWidth, AHeight, ANaturalSize, ANaturalSize2, x, y, width, height]));
    {$ENDIF}

    Widget^.size_allocate(@ARect);
    if Gtk3IsGtkWindow(fWidget)
        and not (csDesigning in AForm.ComponentState) {and (AForm.Parent = nil) and (AForm.ParentWindow = 0)} then
    begin
      AFixedWidthHeight := AForm.BorderStyle in [bsDialog, bsSingle, bsToolWindow];
      with Geometry do
      begin
        if not AFixedWidthHeight and (AForm.Constraints.MinWidth > 0) then
          min_width := AForm.Constraints.MinWidth
        else
          min_width := AForm.Width;
        if not AFixedWidthHeight and (AForm.Constraints.MaxWidth > 0) then
          max_width := AForm.Constraints.MaxWidth
        else
        max_width := AForm.Width;
        if not AFixedWidthHeight and (AForm.Constraints.MinHeight > 0) then
          min_height := AForm.Constraints.MinHeight
        else
          min_height := AForm.Height;
        if not AFixedWidthHeight and (AForm.Constraints.MaxHeight > 0) then
          max_height := AForm.Constraints.MaxHeight
        else
          max_height := AForm.Height;

        base_width := AForm.Width;
        base_height := AForm.Height;
        width_inc := 1;
        height_inc := 1;
        min_aspect := 0;
        max_aspect := 1;
        win_gravity := PGtkWindow(Widget)^.get_gravity
      end;

      if AFixedWidthHeight then
        PGtkWindow(Widget)^.set_geometry_hints(nil, @Geometry,
          [GDK_HINT_POS, GDK_HINT_MIN_SIZE, GDK_HINT_MAX_SIZE])
      else
      begin
        if AForm.BorderStyle <> bsNone then
        begin
          AHints := [GDK_HINT_POS, GDK_HINT_BASE_SIZE];
          if (AForm.Constraints.MinHeight > 0) or (AForm.Constraints.MinWidth > 0) then
            Include(AHints, GDK_HINT_MIN_SIZE);
          if (AForm.Constraints.MaxHeight > 0) or (AForm.Constraints.MaxWidth > 0) then
            Include(AHints, GDK_HINT_MAX_SIZE);

          PGtkWindow(Widget)^.set_geometry_hints(nil, @Geometry, AHints);
        end;
      end;
    end;

    if Gtk3IsGtkWindow(FWidget) then
    begin
      //PGtkWindow(Widget)^.set_default_size(AWidth, AHeight);
      PGtkWindow(Widget)^.set_resizable(true);
      {$IF DEFINED(GTK3DEBUGCORE) OR DEFINED(GTK3DEBUGSIZE)}
      writeln('Window ',dbgsName(LCLObject),' move/size ',dbgs(Bounds(ALeft, ATop, AWidth, AHeight)));
      {$ENDIF}
      PGtkWindow(Widget)^.resize(AWidth, AHeight);

      {Must apply transient window origin here. Not sure if this is needed for decorated windows,
       but non decorated with popupparent must align.}
      x := 0;
      y := 0;
      if not PGtkWindow(Widget)^.get_decorated and (PGtkWindow(Widget)^.transient_for <> nil) then
        PGtkWindow(Widget)^.transient_for^.window^.get_origin(@x, @y);
      PGtkWindow(Widget)^.move(ALeft + x, ATop + y);
    end;
  finally
    EndUpdate;
  end;
end;

function TGtk3Window.getHorizontalScrollbar: PGtkScrollbar;
begin
  Result := nil;
  if not IsWidgetOk then
    exit;
  Result := PGtkScrollBar(FScrollWin^.get_hscrollbar);
end;

function TGtk3Window.getVerticalScrollbar: PGtkScrollbar;
begin
  Result := nil;
  if not IsWidgetOk then
    exit;
  Result := PGtkScrollBar(FScrollWin^.get_vscrollbar);
end;

function TGtk3Window.GetScrolledWindow: PGtkScrolledWindow;
begin
  if IsWidgetOK then
    Result := FScrollWin
  else
    Result := nil;
end;

procedure TGtk3Window.InitializeWidget;
begin
  inherited InitializeWidget;
  if not IsDesigning then
  begin
    g_signal_connect_data(gtk_scrolled_window_get_hscrollbar(GetScrolledWindow), 'change-value',
      TGCallback(@RangeChangeValue), Self, nil, G_CONNECT_DEFAULT);
    g_signal_connect_data(gtk_scrolled_window_get_vscrollbar(GetScrolledWindow), 'change-value',
      TGCallback(@RangeChangeValue), Self, nil, G_CONNECT_DEFAULT);

    g_signal_connect_data(PGtkRange(gtk_scrolled_window_get_hscrollbar(GetScrolledWindow)),'value-changed',
      TGCallback(@RangeValueChanged), Self, nil, G_CONNECT_DEFAULT);
    g_signal_connect_data(PGtkRange(gtk_scrolled_window_get_vscrollbar(GetScrolledWindow)),'value-changed',
      TGCallback(@RangeValueChanged), Self, nil, G_CONNECT_DEFAULT);
  end;
end;

destructor TGtk3Window.Destroy;
begin
  if Gtk3IsGdkPixbuf(FIcon) then
  begin
    FIcon^.unref;
    FIcon := nil;
  end;
  inherited Destroy;
end;

procedure TGtk3Window.Activate;
begin
  if Gtk3IsGtkWindow(fWidget) then
  begin
    if Gtk3IsGdkWindow(PGtkWindow(FWidget)^.window) then
    begin
      PGtkWindow(FWidget)^.window^.raise_;
      PGtkWindow(FWidget)^.present;
      PGtkWindow(FWidget)^.activate;
    end;
  end;
end;

procedure TGtk3Window.ActivateWindow(AEvent: PGdkEvent);
var
  MsgActivate: TLMActivate;
  FIsActivated: Boolean;
begin
  if not Gtk3IsGtkWindow(FWidget) then exit;

  //gtk3 does not handle activate/deactivate at all
  //even cannot catch it via GDK_FOCUS event ?!?
  FillChar(MsgActivate{%H-}, SizeOf(MsgActivate), #0);
  MsgActivate.Msg := LM_ACTIVATE;

  if (AEvent <> nil) and PGtkWindow(Widget)^.is_active then
    MsgActivate.Active := WA_ACTIVE
  else
    MsgActivate.Active := WA_INACTIVE;
  MsgActivate.ActiveWindow := HWND(Self);

  // DebugLn('TGtk3Window.ActivateWindow ',dbgsName(LCLObject),' Active ',dbgs(PGtkWindow(Widget)^.is_active),
  // ' CustomFormActive ',dbgs(TCustomForm(LCLObject).Active));
  FIsActivated := TCustomForm(LCLObject).Active;
  {do not send activate if form is already activated,
   also do not send activate if TCustomForm.Parent is assigned
   since it's form embedded into another control or form}
  if (Boolean(MsgActivate.Active) = FIsActivated) or (LCLObject.Parent <> nil) then
  else
  begin
    // DebugLn('TGtk3Window.ActivateWindow Active ',dbgs(MsgActivate.Active = WA_ACTIVE),
    //  ' Message delivery to lcl ',dbgs(MsgActivate.Active));
    DeliverMessage(MsgActivate);
  end;
end;

function TGtk3Window.CloseQuery: Boolean;
var
  Msg : TLMessage;
begin
  {$IFDEF GTK3DEBUGCORE}
    DebugLn('TGtk3Window.CloseQuery');
  {$ENDIF}
  FillChar(Msg{%H-}, SizeOf(Msg), 0);

  Msg.Msg := LM_CLOSEQUERY;

  DeliverMessage(Msg);

  Result := False;
end;

function TGtk3Window.GetWindow: PGdkWindow;
begin
  Result := FWidget^.window;
end;

function TGtk3Window.GetMenuBar: PGtkMenuBar;
var
  ABox:PGtkBox;
begin
  if not Assigned(FMenuBar) then
  begin
    FMenuBar := TGtkMenuBar.new; // our menubar (needed for main menu)
    // MenuBar
    //  -> Menu    Menu2
    //    Item 1   Item 3
    //    Item 2
    g_object_set_data(Widget,'lclmenubar',GPointer(1));
    ABox := PGtkBox(PGtkWindow(Widget)^.get_child);
    ABox^.pack_start(FMenuBar, False, False, 0);
  end;
  Result := FMenuBar;
end;

function TGtk3Window.GetBox: PGtkBox;
begin
  Result := FBox;
end;

function TGtk3Window.GetWindowState: TGdkWindowState;
begin
  Result := [];
  if IsWidgetOK and (FWidget^.get_realized) then
    Result := FWidget^.window^.get_state;
end;

{ TGtk3HintWindow }

function TGtk3HintWindow.CreateWidget(const Params: TCreateParams): PGtkWidget;
var
  AForm: THintWindow;
begin
  FText := '';
  FHasPaint := True;
  FWidgetType := [wtHintWindow];
  Result := inherited CreateWidget(Params);
end;

{ TGtk3Dialog }

procedure TGtk3Dialog.SetCallbacks;
begin
  // common callbacks for all kind of dialogs
  g_signal_connect_data(fWidget,
    'destroy', TGCallback(@TGtk3Dialog.DestroyCB), Self, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(fWidget,
    'delete-event', TGCallback(@TGtk3Dialog.CloseQueryCB), Self, nil, G_CONNECT_DEFAULT);

  g_signal_connect_data(fWidget,
    'response', TGCallback(@Tgtk3DIalog.ResponseCB), Self, nil, G_CONNECT_DEFAULT);

  g_signal_connect_data(fWidget,
    'close', TGCallback(@Tgtk3DIalog.CloseCB), Self, nil, G_CONNECT_DEFAULT);

  g_signal_connect_data(fWidget,
    'realize', TGCallback(@Tgtk3Dialog.RealizeCB), Self, nil, G_CONNECT_DEFAULT);
end;

class function Tgtk3Dialog.RealizeCB(dlg: TGtk3Dialog): GBoolean; cdecl;
begin
  Result := False;
  if (dlg=nil) then exit;
  // actually key intercepion is not required
  {if dlg.FWidget^.get_has_window and Gtk3IsGdkWindow(dlg.FWidget^.window) then
  begin
    gdk_window_set_events(dlg.FWidget^.window,
      gdk_window_get_events(dlg.FWidget^.window)
        or GDK_KEY_RELEASE_MASK or GDK_KEY_PRESS_MASK);

  end;}

  if (wtDialog in dlg.WidgetType) then
  begin
    if Assigned(dlg.CommonDialog) then
      TCommonDialog(dlg.CommonDialog).DoShow;
  end;
  Result := True;
end;


class function TGtk3Dialog.DestroyCB(dlg:TGtk3Dialog): GBoolean; cdecl;
begin
  Result := True;
//  if (AWidget=nil) then ;
  if not Assigned(dlg) then exit;
  dlg.CommonDialog.UserChoice := mrCancel;
  dlg.CommonDialog.Close;
end;

class function TGtk3Dialog.ResponseCB(response_id:gint; dlg: TGtk3Dialog): GBoolean; cdecl;
begin
  if Assigned(dlg) then
    Result:=dlg.response_handler(TGtkResponseType(response_id))
  else
    Result:= false;
end;

function TGtk3Dialog.response_handler(response_id:TGtkResponseType):boolean;
begin
 (* case response_id of
  GTK_RESPONSE_NONE:;
  GTK_RESPONSE_REJECT: ;
  GTK_RESPONSE_ACCEPT:;
  GTK_RESPONSE_DELETE_EVENT:;
  GTK_RESPONSE_OK:;
  GTK_RESPONSE_CANCEL:;
  GTK_RESPONSE_CLOSE:;
  GTK_RESPONSE_YES:;
  GTK_RESPONSE_NO:;
  GTK_RESPONSE_APPLY:;
  GTK_RESPONSE_HELP:;
  end;*)
  if response_id=GTK_RESPONSE_YES then
  begin
    Self.CommonDialog.UserChoice:=mrYes;
  end else
  if response_id=GTK_RESPONSE_NO then
  begin
    Self.CommonDialog.UserChoice:=mrNo;
  end else
  if response_id=GTK_RESPONSE_OK then
  begin
    Self.CommonDialog.UserChoice:=mrOk;
  end else
  if response_id=GTK_RESPONSE_CANCEL then
  begin
    Self.CommonDialog.UserChoice:=mrCancel;
  end else
  if response_id=GTK_RESPONSE_CLOSE then
  begin
    Self.CommonDialog.UserChoice:=mrClose;
  end;
  Result:=false;
end;

function TGtk3Dialog.close_handler(): boolean;
begin
  Result:=false;
end;

class function TGtk3Dialog.CloseCB(dlg: TGtk3Dialog): GBoolean;
  cdecl;
begin
  if Assigned(dlg) then
    Result:=dlg.close_handler()
  else
    Result:= true;
end;

class function TGtk3Dialog.CloseQueryCB(w:PGtkWidget;dlg:TGtk3Dialog): GBoolean;
  cdecl;
var
  theDialog : TCommonDialog;
  CanClose: boolean;
  //AHandle: HWND;
begin
  Result := False; // true = do nothing, false = destroy or hide window
  if (dlg=nil) then exit;
  // data is not the commondialog. Get it manually.
//  AHandle := HwndFromGtkWidget(AWidget);
  if (dlg <> nil) and (wtDialog in TGtk3Widget(dlg).WidgetType) then
  begin
    theDialog := dlg.CommonDialog;
    if theDialog = nil then exit;
    if theDialog.OnCanClose<>nil then
    begin
      CanClose:=True;
      theDialog.DoCanClose(CanClose);
      Result := not CanClose;
    end;
  end;
end;

function TGtk3Dialog.CreateWidget(const Params: TCreateParams): PGtkWidget;
begin
  FWidgetType := [wtWidget, wtDialog];
  Result := TGtkDialog.new;
  DebugLn('WARNING: TGtk3Dialog.CreateWidget should be used in real dialog constructor .');
end;

procedure TGtk3Dialog.InitializeWidget;
begin
  g_object_set_data(FWidget,'lclwidget', Self);
end;

procedure TGtk3Dialog.CloseDialog;
begin
  if fWidget<>nil then
    fWidget^.destroy_;
end;


{ TGtk3FileDialog }

function TGtk3FileDialog.CreateWidget(const Params: TCreateParams): PGtkWidget;
begin
  DebugLn('ERROR: TGtk3FileDialog.CreateWidget error.');
  // Result := nil;
  Result := TGtkFileChooserDialog.new;
  // gtk_file_chooser_dialog_new();
end;

constructor TGtk3FileDialog.Create(const ACommonDialog: TCommonDialog);
var
  FileDialog: TFileDialog absolute ACommonDialog;
  Action: TGtkFileChooserAction;
  Button1: String;
  AFileDialog: PGtkFileChooserDialog;
  AParams: TCreateParams;
begin
  inherited Create;
  FOwnWidget := True;
  // Initializes the properties
  FKeysToEat := [VK_TAB, VK_RETURN, VK_ESCAPE];
  FWidgetType := [wtWidget, wtDialog];

  // FHasPaint := False;
  CommonDialog := ACommonDialog;
  // Defines an action for the dialog and creates it
  Action := GTK_FILE_CHOOSER_ACTION_OPEN;
  Button1 := GTK_STOCK_OPEN;

  if (FileDialog is TSaveDialog) or (FileDialog is TSavePictureDialog) then
  begin
    Action := GTK_FILE_CHOOSER_ACTION_SAVE;
    Button1 := GTK_STOCK_SAVE;
  end
  else
  if FileDialog is TSelectDirectoryDialog then
    Action := GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER;

  FWidget := gtk_file_chooser_dialog_new(PgChar(FileDialog.Title), nil,
    Action, PChar(GTK_STOCK_CANCEL),
    [GTK_RESPONSE_CANCEL, PChar(Button1), GTK_RESPONSE_OK, nil]);

  AFileDialog := PGtkFileChooserDialog(FWidget);
  if FileDialog is TSaveDialog then
  begin
    gtk_file_chooser_set_do_overwrite_confirmation(PGtkFileChooser(AFileDialog),
      ofOverwritePrompt in TOpenDialog(FileDialog).Options);
  end;

  if FileDialog.InitialDir <> '' then
    gtk_file_chooser_set_current_folder(PGtkFileChooser(AFileDialog), Pgchar(FileDialog.InitialDir));

  if gtk_file_chooser_get_action(PGtkFileChooser(AFileDialog)) in
    [GTK_FILE_CHOOSER_ACTION_SAVE, GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER]
  then
    gtk_file_chooser_set_current_name(PGtkFileChooser(AFileDialog), Pgchar(FileDialog.FileName));

  InitializeWidget;
end;

{ TGtk3FontSelectionDialog }

procedure TGtk3FontSelectionDialog.InitializeWidget;
var
  fnt:TFont;
  pch:PgtkFontChooser;
  fontDesc: PPangoFontDescription;
  family: String;
  stretch: TPangoStretch;
  weight: TPangoWeight;
begin
  fWidget:=TGtkFontChooserDialog.new(PChar(CommonDialog.Title),nil);
  fontDesc := TPangoFontDescription.new;
  try
    fnt:=TFontDialog(CommonDialog).Font;
    if fnt.Size = 0 then
      FontDesc^.set_size(10 * PANGO_SCALE)
    else
      FontDesc^.set_size(fnt.Size * PANGO_SCALE);

    family := fnt.Name;
    ExtractPangoFontFaceSuffixes(family, stretch, weight);
    fontDesc^.set_family(PChar(family));

    if (fsBold in fnt.Style) and (weight < PANGO_WEIGHT_SEMIBOLD) then
      // bold is specified by the fsBold flag only
      fontDesc^.set_weight(PANGO_WEIGHT_BOLD)
    else
      fontDesc^.set_weight(weight);

    if fsItalic in fnt.Style then
    begin
      // we need to specify the exact style for the font dialog
      if PangoFontHasItalicFace(fWidget^.get_pango_context, family) then
        fontDesc^.set_style(PANGO_STYLE_ITALIC)
      else
        fontDesc^.set_style(PANGO_STYLE_OBLIQUE);
    end
    else
      fontDesc^.set_style(PANGO_STYLE_NORMAL);

    if (stretch = PANGO_STRETCH_NORMAL) then
      fontDesc^.set_stretch(GetPangoFontDefaultStretch(family))
    else
      fontDesc^.set_stretch(stretch);

    pch:=PGtkFontChooser(fWidget);
    pch^.set_font_desc(fontDesc);
  finally
    fontDesc^.free;
  end;
  inherited InitializeWidget;
end;

function TGtk3FontSelectionDialog.response_handler(resp_id: TGtkResponseType): boolean;
var
  fnt:TFont;
  pch:PgtkFontChooser;
  pfc:PPangoFontFace;
  pfd:PPangoFontDescription;
  fnts:TfontStyles;
  family: Pgchar;
begin
  if resp_id=GTK_RESPONSE_OK then
  begin
    fnt:=TFontDialog(CommonDialog).Font;
    pch:=PGtkFontChooser(fWidget);
    pfc:=pch^.get_font_face();
    pfd:=pfc^.describe;
    { this stuff is implemened in gtk3objects.Tgtk3Font.UpdateLogFont
      so this is backward mapping of properties }

    family := pfd^.get_family();
    fnt.Name:=AppendPangoFontFaceSuffixes(family, pfd^.get_stretch, pfd^.get_weight);
    fnt.Size:=pch^.get_font_size() div PANGO_SCALE;

    fnts:=[];
    if pfd^.get_weight >= PANGO_WEIGHT_SEMIBOLD then
     include(fnts,fsBold);

    // do not differentiate oblique and italic
    if (pfd^.get_style >= PANGO_STYLE_OBLIQUE) then
      include(fnts,fsItalic);
    fnt.Style:=fnts;
  end;
  Result:=inherited response_handler(resp_id);
end;

constructor TGtk3FontSelectionDialog.Create(const ACommonDialog: TCommonDialog);
begin
  inherited Create;
  FOwnWidget := True;
  // Initializes the properties
  FKeysToEat := [VK_TAB, VK_RETURN, VK_ESCAPE];
  FWidgetType := [wtWidget, wtDialog];

  // FHasPaint := False;
  CommonDialog := ACommonDialog;
  InitializeWidget;
  Self.SetCallbacks;
end;

{ TGtk3ColorSelectionDialog }

procedure TGtk3ColorSelectionDialog.InitializeWidget;
var
  clr:TColor;
  rgba:TGdkRGBA;
begin
  fWidget := TGtkColorSelectionDialog.new(PChar(Self.CommonDialog.Title));
  clr:=ColorToRgb(TColorDialog(Self.CommonDialog).Color);
  rgba.red:=Red(clr)/255;
  rgba.blue:=Blue(clr)/255;
  rgba.green:=Green(clr)/255;
  rgba.alpha:=(clr shl 24)/255;
  gtk_color_selection_set_current_rgba (
     PgtkColorSelection(PGtkColorSelectionDialog(fWidget)^.color_selection),
     @rgba);
end;

constructor TGtk3ColorSelectionDialog.Create(const ACommonDialog: TCommonDialog
  );
begin
  inherited Create;
  FOwnWidget := True;
  // Initializes the properties
  FKeysToEat := [VK_TAB, VK_RETURN, VK_ESCAPE];
  FWidgetType := [wtWidget, wtDialog];

  // FHasPaint := False;
  CommonDialog := ACommonDialog;
  TGtk3Widget(Self).InitializeWidget;
  Self.SetCallbacks;
end;


{ TGtk3newColorSelectionDialog }

procedure TGtk3newColorSelectionDialog.InitializeWidget;
var
  rgba:TGdkRGBA;
  dlg:TColorDialog;
begin
  dlg:=TColorDialog(CommonDialog);
  fWidget:= TGtkColorChooserDialog.new(PChar(Self.CommonDialog.Title), nil);
  self.color_to_rgba(dlg.Color, rgba);
  PGtkColorChooser(fWidget)^.use_alpha:=(cdShowAlphaChannel in dlg.Options);
  if (cdPreventFullOpen in dlg.Options) then // drop basic palette that way
    PGtkColorChooser(fWidget)^.add_palette(GTK_ORIENTATION_HORIZONTAL,9,10,nil);
  PGtkColorChooser(fWidget)^.set_rgba(@rgba);
  inherited;
end;

function TGtk3newColorSelectionDialog.response_handler(resp_id: TGtkResponseType): boolean;
var
  clr:TColor;
  rgba:TGdkRGBA;
begin
  if resp_id=GTK_RESPONSE_OK then
  begin
    PGtkColorChooser(fWidget)^.get_rgba(@rgba);
    clr:=self.rgba_to_color(rgba);
    if not PGtkColorChooser(fWidget)^.use_alpha then
      clr:=clr and $00ffffff;
    TColorDialog(Self.CommonDialog).Color:=clr;
  end;
  Result:=inherited response_handler(resp_id);
end;

constructor TGtk3newColorSelectionDialog.Create(const ACommonDialog: TCommonDialog
  );
begin
  inherited Create;
  FOwnWidget := True;
  // Initializes the properties
  LCLObject := nil;
  FKeysToEat := [VK_TAB, VK_RETURN, VK_ESCAPE];
  FWidgetType := [wtWidget, wtDialog];

  // FHasPaint := False;
  CommonDialog := ACommonDialog;
  TGtk3Widget(Self).InitializeWidget;
  Self.SetCallbacks;
end;

class procedure TGtk3newColorSelectionDialog.color_to_rgba(clr: TColor; out
  rgba: TgdkRGBA);
begin
  rgba := TColortoTGdkRGBA(clr);
end;

class function TGtk3newColorSelectionDialog.rgba_to_color(const rgba: TgdkRGBA
  ): TColor;
begin
  Result := TGdkRGBAToTColor(rgba);
end;


{ TGtk3GLArea }

procedure TGtk3GLArea.Update(ARect: PRect);
begin
  if IsWidgetOK then
    PGtkGLArea(Widget)^.queue_render;
end;

function TGtk3GLArea.CreateWidget(const Params: TCreateParams): PGtkWidget;
begin
  FWidgetType := [wtWidget, wtGLArea];
  Result := TGtkGLArea.new;
end;

{ TGtk3DesignWidget }

procedure TGtk3DesignWidget.BringDesignerToFront;
begin
  {$IFDEF GTK3DEBUGDESIGNER}
  writeln('>BringDesignerToFront ');
  {$ENDIF}
  if Gtk3IsGdkWindow(getContainerWidget^.window) then
    getContainerWidget^.window^.raise_;
  {$IFDEF GTK3DEBUGDESIGNER}
  writeln('<BringDesignerToFront ');
  {$ENDIF}
end;

procedure TGtk3DesignWidget.ResizeDesigner;
var
  R: TGdkRectangle;
begin
  // Design control must be same as form area,
  gtk_widget_get_allocation(getContainerWidget, @R);
  with R do
  begin
    //gtk_widget_set_allocation(FDesignControl, @R);
    //gtk_widget_queue_resize(FDesignControl);
  end;
end;

function Gtk3DrawDesigner(AWidget: PGtkWidget; AContext: Pcairo_t; Data: gpointer): gboolean; cdecl;
var
  ARect: TGdkRectangle;
begin
  Result := False;
  if Data <> nil then
  begin
    gdk_cairo_get_clip_rectangle(AContext, @ARect);
    {$IFDEF GTK3DEBUGDESIGNER}
    writeln('>Gtk3DrawDesigner ');
    {$ENDIF}
    Result := TGtk3DesignWidget(Data).GtkEventPaint(AWidget, AContext);
    // workaround for lcl painted widgets until we found why gtk3 sends wrong rect
    // if (TGtk3Widget(Data).FHasPaint) and
    if (ARect.height < (TGtk3DesignWidget(Data).getContainerWidget^.get_allocated_height div 4) ) then
    begin
      {$IFDEF GTK3DEBUGDESIGNER}
      with ARect do
        writeln('Queued new draw for designer ?!? x=',x,' y=',y,' w=',width,' h=',height);
      {$ENDIF}
      //do not queue any draw for now
      //with ARect do
      //  AWidget^.queue_draw_area(x, y , width, height);
    end;
    {$IFDEF GTK3DEBUGDESIGNER}
    writeln('<Gtk3DrawDesigner ');
    {$ENDIF}
  end;
end;

function TGtk3DesignWidget.CreateWidget(const Params: TCreateParams
  ): PGtkWidget;
begin
  Result := inherited CreateWidget(Params);
  gtk_widget_set_has_window(Widget, True);
  gtk_widget_set_has_window(GetContainerWidget, True);
end;

function TGtk3DesignWidget.GetContext: HDC;
begin
  if FDesignContext <> 0 then
    Result := FDesignContext
  else
    Result := inherited GetContext;
end;

procedure TGtk3DesignWidget.InitializeWidget;
begin
  inherited InitializeWidget;
  g_signal_handler_disconnect(getContainerWidget, FDrawSignal);
  g_signal_connect_data(getContainerWidget,'draw', TGCallback(@Gtk3DrawDesigner), Self, nil, G_CONNECT_DEFAULT);
  BringDesignerToFront;
end;

function TGtk3DesignWidget.GtkEventPaint(Sender: PGtkWidget; AContext: Pcairo_t
  ): Boolean; cdecl;
var
  Msg: TLMPaint;
  AStruct: TPaintStruct;
  AClipRect: TGdkRectangle;
  localClip:TRect;
  P: TPoint;
  AScrolledWin: PGtkScrolledWindow;
  HScrollPolicy, VScrollPolicy: TGtkPolicyType;
begin
  Result := gtk_false;

  if not FHasPaint then
    exit;

  FillChar(Msg{%H-}, SizeOf(Msg), #0);

  Msg.Msg := LM_PAINT;

  FillChar(AStruct{%H-}, SizeOf(TPaintStruct), 0);
  Msg.PaintStruct := @AStruct;

  with PaintData do
  begin
    PaintWidget := Sender;
    ClipRegion := nil;
    gdk_cairo_get_clip_rectangle(AContext, @AClipRect);
    localClip:=RectFromGdkRect(AClipRect);
    ClipRect := @localClip;
  end;
  FContext := 0;
  FCairoContext := AContext;
  Msg.DC := BeginPaint(HWND(Self), AStruct);
  FDesignContext := Msg.DC;
  FContext := Msg.DC;
  Msg.PaintStruct^.rcPaint := PaintData.ClipRect^;
  Msg.PaintStruct^.hdc := FDesignContext;

  P := getClientOffset;
  TGtk3DeviceContext(Msg.DC).translate(P);
  try
    try
      if wtScrollingWinControl in WidgetType then
      begin
        P := getClientOffset;
        AScrolledWin := TGtk3ScrollingWinControl(Self).GetScrolledWindow;
        AScrolledWin^.get_policy(@HScrollPolicy, @VScrollPolicy);
        if HScrollPolicy < GTK_POLICY_NEVER then
          P.X := P.X + Round(AScrolledWin^.get_hadjustment^.get_value);
        if VScrollPolicy < GTK_POLICY_NEVER then
          P.Y := P.Y + Round(AScrolledWin^.get_vadjustment^.get_value);
        TGtk3DeviceContext(Msg.DC).ScrollbarsOffset := Point(P.X, P.Y);
        cairo_translate(AContext, -P.X, -P.Y);
        with TGtk3DeviceContext(Msg.DC).fncOrigin do
        begin
          X := X - P.X;
          Y := Y - P.Y;
        end;
      end;

      //DoBeforeLCLPaint;
      {$IFDEF GTK3DEBUGDESIGNER}
      writeln('>TGtk3DesignWidget.Paint DC=',dbgHex(Msg.DC),' offset=',dbgs(P),' surface=',cairo_surface_get_type(cairo_get_target(AContext)));
      {$ENDIF}
      LCLObject.WindowProc(TLMessage(Msg));
      if wtScrollingWinControl in WidgetType then
        cairo_translate(AContext, P.X, P.Y);
      {$IFDEF GTK3DEBUGDESIGNER}
      writeln('<TGtk3DesignWidget.Paint');
      {$ENDIF}
    finally
      FCairoContext := nil;
      Fillchar(FPaintData, SizeOf(FPaintData), 0);
      FContext := 0;
      FDesignContext := 0;
      EndPaint(HWND(Self), AStruct);
    end;
  except
    Application.HandleException(nil);
  end;
end;

procedure TGtk3DesignWidget.lowerWidget;
begin
  inherited lowerWidget;
  BringDesignerToFront;
end;

procedure TGtk3DesignWidget.raiseWidget;
begin
  inherited raiseWidget;
  BringDesignerToFront;
end;

end.

