{
  Copyright (C) 2011 Felipe Monteiro de Carvalho

  License: The same modifying LGPL with static linking exception as the LCL

  This unit should be a repository for various custom drawn components,
  such as a custom drawn version of TButton, of TEdit, of TPageControl, etc,
  eventually forming a full set of custom drawn components.
}
unit CustomDrawnControls;

{$mode objfpc}{$H+}

interface

uses
  // FPC
  Classes, SysUtils, contnrs, Math, types,
  // LazUtils
  LazUTF8,
  // LCL -> Use only TForm, TWinControl, TCanvas, TLazIntfImage
  LCLType, LCLProc, LCLIntf, LCLMessageGlue, LMessages, Messages,
  Forms, Graphics, Controls,
  // Other LCL units are only for types
  StdCtrls, ExtCtrls, ComCtrls, Buttons,
  //
  customdrawndrawers;

type
  { TCDControl }

  TCDControl = class(TCustomControl)
  protected
    FDrawStyle: TCDDrawStyle;
    FDrawer: TCDDrawer;
    FState: TCDControlState;
    FStateEx: TCDControlStateEx;
    procedure CalculatePreferredSize(var PreferredWidth,
      PreferredHeight: integer; WithThemeSpace: Boolean); override;
    procedure SetState(const AValue: TCDControlState); virtual;
    procedure PrepareCurrentDrawer(); virtual;
    procedure SetDrawStyle(const AValue: TCDDrawStyle); virtual;
    function GetClientRect: TRect; override;
    function GetControlId: TCDControlID; virtual;
    procedure CreateControlStateEx; virtual;
    procedure PrepareControlState; virtual;
    procedure PrepareControlStateEx; virtual;
    // keyboard
    procedure DoEnter; override;
    procedure DoExit; override;
    // mouse
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    //
    property DrawStyle: TCDDrawStyle read FDrawStyle write SetDrawStyle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LCLWSCalculatePreferredSize(var PreferredWidth,
      PreferredHeight: integer; WithThemeSpace, AAutoSize, AAllowUseOfMeasuresEx: Boolean);
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
    // Methods for use by LCL-CustomDrawn
    procedure DrawToCanvas(ACanvas: TCanvas);
  end;
  TCDControlClass = class of TCDControl;

  TCDScrollBar = class;

  { TCDScrollableControl }

  TCDScrollableControl = class(TCDControl)
  private
    FRightScrollBar, FBottomScrollBar: TCDScrollBar;
    FSpacer: TCDControl;
    FScrollBars: TScrollStyle;
    procedure SetScrollBars(AValue: TScrollStyle);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars;
  end;

  // ===================================
  // Standard Tab
  // ===================================

  { TCDButtonControl }

  TCDButtonControl = class(TCDControl)
  protected
    // This fields are set by descendents
    FHasOnOffStates: Boolean;
    FIsGrouped: Boolean;
    FGroupIndex: Integer;
    FAllowGrayed: Boolean;
    // keyboard
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure KeyUp(var Key: word; Shift: TShiftState); override;
    // mouse
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    // button state change
    procedure DoUncheckButton(); virtual;
    procedure DoCheckIfFirstButtonInGroup();
    procedure DoButtonDown(); virtual;
    procedure DoButtonUp(); virtual;
    procedure RealSetText(const Value: TCaption); override;
    function GetChecked: Boolean;
    procedure SetChecked(AValue: Boolean);
    function GetCheckedState: TCheckBoxState;
    procedure SetCheckedState(AValue: TCheckBoxState);
    // properties
    property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed default False;
    property Checked: Boolean read GetChecked write SetChecked default False;
    //property Down: Boolean read GetDown write SetDown;
    property State: TCheckBoxState read GetCheckedState write SetCheckedState default cbUnchecked;
  public
  end;

  { TCDButton }

  TCDButton = class(TCDButtonControl)
  private
    FGlyph: TBitmap;
    FKind: TBitBtnKind;
    FModalResult: TModalResult;
    procedure SetModalResult(const AValue: TModalResult);
    procedure SetGlyph(AValue: TBitmap);
    procedure SetKind(AKind: TBitBtnKind);
 protected
    FBState: TCDButtonStateEx;
    procedure Click; override;
    function GetControlId: TCDControlID; override;
    procedure CreateControlStateEx; override;
    procedure PrepareControlStateEx; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property Caption;
    property Color;
    property Constraints;
    property DrawStyle;
    property Enabled;
    property Font;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property Kind: TBitBtnKind read FKind write SetKind default bkCustom;
//    property IsToggleBox: Boolean read FGlyph write SetGlyph;
    property ModalResult: TModalResult read FModalResult write SetModalResult default mrNone;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
  end;

  { TCDToggleBox -- a button that latches into the pressed state when
    checked. Reuses TCDButton's drawing path; setting FHasOnOffStates
    in the constructor makes TCDButtonControl.DoButtonUp toggle csfOn /
    csfOff on each click. PrepareControlState then projects csfOn into
    csfSunken so DrawButton renders the latched state as visually
    pressed (matching Win32 / GTK / Qt behaviour). }
  TCDToggleBox = class(TCDButton)
  protected
    procedure PrepareControlState; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AllowGrayed;
    property Checked;
    property State;
  end;

  { TCDEdit }

  TCDEdit = class(TCDControl)
  private
    DragDropStarted: boolean;
    FCaretTimer: TTimer;
    FLines: TStrings;
    FOnChange: TNotifyEvent;
    FReadOnly: Boolean;
    function GetCaretPos: TPoint;
    function GetLeftTextMargin: Integer;
    function GetMultiLine: Boolean;
    function GetRightTextMargin: Integer;
    function GetText: string;
    function GetPasswordChar: Char;
    procedure HandleCaretTimer(Sender: TObject);
    procedure DoClearSelection;
    procedure DoManageVisibleTextStart;
    procedure SetCaretPost(AValue: TPoint);
    procedure SetLeftTextMargin(AValue: Integer);
    procedure SetLines(AValue: TStrings);
    procedure SetMultiLine(AValue: Boolean);
    procedure SetRightTextMargin(AValue: Integer);
    procedure SetText(AValue: string);
    procedure SetPasswordChar(AValue: Char);
    function MousePosToCaretPos(X, Y: Integer): TPoint;
  protected
    FEditState: TCDEditStateEx; // Points to the same object as FStateEx, so don't Free!
    function GetControlId: TCDControlID; override;
    procedure CreateControlStateEx; override;
    procedure RealSetText(const Value: TCaption); override; // to update on caption changes, don't change this as it might break descendents
    // for descendents to override
    procedure DoChange; virtual;
    // keyboard
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure KeyUp(var Key: word; Shift: TShiftState); override;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
    // mouse
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetCurrentLine(): string;
    procedure SetCurrentLine(AStr: string);
    function IsSomethingSelected: Boolean;
    { Replace any selected text with empty and clear the selection
      state. Used before external text edits that must operate on the
      post-selection buffer. }
    procedure DoDeleteSelection;
    property LeftTextMargin: Integer read GetLeftTextMargin write SetLeftTextMargin;
    property RightTextMargin: Integer read GetRightTextMargin write SetRightTextMargin;
    // selection info in a format compatible with TEdit
    function GetSelStartX: Integer;
    function GetSelLength: Integer;
    procedure SetSelStartX(ANewX: Integer);
    procedure SetSelLength(ANewLength: Integer);
    property CaretPos: TPoint read GetCaretPos write SetCaretPost;
    // IME pre-edit text. Drawn inline at the caret with an underline
    // but NOT inserted into Lines until the IME commits. Setter
    // invalidates and stores in FEditState; the drawer reads the
    // state and renders.
    procedure SetPreedit(const AText: AnsiString;
      ACursorBegin, ACursorEnd: LongInt);
    function GetPreeditText: AnsiString;
    // Pixel X of the caret within this control's client area, taking
    // VisibleTextStart and any current preedit into account. Used by
    // the Wayland host to send set_cursor_rectangle so the IME's
    // candidate window pops up next to the caret. Y is line-top.
    function GetCaretPixelRect: TRect;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property Color;
    property DrawStyle;
    property Enabled;
    property Lines: TStrings read FLines write SetLines;
    property MultiLine: Boolean read GetMultiLine write SetMultiLine default False;
    property PasswordChar: Char read GetPasswordChar write SetPasswordChar default #0;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property TabStop default True;
    property Text : string read GetText write SetText stored false; // This is already stored in Lines
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TCDCheckBox }

  TCDCheckBox = class(TCDButtonControl)
  protected
    function GetControlId: TCDControlID; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AllowGrayed default False;
    property Checked;
    property DrawStyle;
    property Caption;
    property Enabled;
    property TabStop default True;
    property State;
  end;

  { TCDRadioButton }

  TCDRadioButton = class(TCDButtonControl)
  protected
    function GetControlId: TCDControlID; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Caption;
    property Checked;
    property DrawStyle;
    property Enabled;
    property TabStop default True;
  end;

  TKeyboardInputBehavior = (kibAutomatic, kibRequires, kibDoesntRequire);

  { TCDComboBox }

  TCDComboBox = class(TCDEdit)
  private
    FIsClickingButton: Boolean;
    FItemIndex: Integer;
    FItems: TStrings;
    FKeyboardInputBehavior: TKeyboardInputBehavior;
    function GetItems: TStrings;
    procedure ShowSelectItemDialogResult(ASelectedItem: Integer);
    procedure SetItemIndex(AValue: Integer);
    procedure SetItems(AValue: TStrings);
    procedure SetKeyboardInputBehavior(AValue: TKeyboardInputBehavior);
  protected
    function GetControlId: TCDControlID; override;
    // mouse
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Items: TStrings read GetItems write SetItems;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    // This allows controlling the virtual keyboard behavior, mostly for Android
    property KeyboardInputBehavior: TKeyboardInputBehavior read FKeyboardInputBehavior write SetKeyboardInputBehavior;
  end;

  { TCDPositionedControl }

  TCDPositionedControl = class(TCDControl)
  private
    DragDropStarted: boolean;
    FLastMouseDownPos: TPoint;
    FPositionAtMouseDown: Integer;
    FButton: TCDControlState; // the button currently being clicked
    FBtnClickTimer: TTimer;
    // fields
    FMax: Integer;
    FMin: Integer;
    FOnChange, FOnChangeByUser: TNotifyEvent;
    FPageSize: Integer;
    FPosition: Integer;
    procedure SetMax(AValue: Integer);
    procedure SetMin(AValue: Integer);
    procedure SetPageSize(AValue: Integer);
    procedure SetPosition(AValue: Integer);
    procedure DoClickButton(AButton: TCDControlState; ALargeChange: Boolean);
    procedure HandleBtnClickTimer(ASender: TObject);
  protected
    FSmallChange, FLargeChange: Integer;
    FPCState: TCDPositionedCStateEx;
    // One can either move by dragging the slider
    // or by putting the slider where the mouse is
    FMoveByDragging: Boolean;
    function GetPositionFromMousePosWithMargins(X, Y, ALeftMargin, ARightMargin: Integer;
       AIsHorizontal, AAcceptMouseOutsideStrictArea: Boolean): integer;
    function GetPositionFromMousePos(X, Y: Integer): integer; virtual; abstract;
    function GetPositionDisplacementWithMargins(AOldMousePos, ANewMousePos: TPoint;
      ALeftMargin, ARightMargin: Integer; AIsHorizontal: Boolean): Integer;
    function GetPositionDisplacement(AOldMousePos, ANewMousePos: TPoint): Integer; virtual; abstract;
    function GetButtonFromMousePos(X, Y: Integer): TCDControlState; virtual;
    procedure CreateControlStateEx; override;
    procedure PrepareControlStateEx; override;
    // keyboard
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    // mouse
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    //
    property PageSize: Integer read FPageSize write SetPageSize;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Which arrow button is currently being held / clicked, or [] for
      none. Public accessor over the private FButton so an
      OnChangeByUser handler can tell an arrow gesture apart from a
      trough click or a thumb drag. }
    function CurrentButton: TCDControlState;
  published
    property Max: Integer read FMax write SetMax;
    property Min: Integer read FMin write SetMin;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChangeByUser: TNotifyEvent read FOnChangeByUser write FOnChangeByUser;
    property Position: Integer read FPosition write SetPosition;
  end;

  { TCDScrollBar }

  TCDScrollBar = class(TCDPositionedControl)
  private
    FKind: TScrollBarKind;
    procedure SetKind(AValue: TScrollBarKind);
  protected
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure GetBorderSizes(out ALeft, ARight: Integer);
  protected
    function GetPositionFromMousePos(X, Y: Integer): integer; override;
    function GetButtonFromMousePos(X, Y: Integer): TCDControlState; override;
    function GetPositionDisplacement(AOldMousePos, ANewMousePos: TPoint): Integer; override;
    function GetControlId: TCDControlID; override;
    procedure PrepareControlState; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Public accessor for the inherited protected FSmallChange /
      FLargeChange. Hosts that want the trough click to scroll one
      page and arrow clicks to step a fraction need to set these
      explicitly -- the defaults of 1 / 5 are tiny relative to grid
      content. }
    procedure SetIncrements(ASmall, ALarge: Integer);
  published
    property DrawStyle;
    property Enabled;
    property Kind: TScrollBarKind read FKind write SetKind;
    property PageSize;
    property TabStop default True;
  end;

  {@@
    TCDGroupBox is a custom-drawn group box control
  }

  { TCDGroupBox }

  TCDGroupBox = class(TCDControl)
  protected
    function GetControlId: TCDControlID; override;
    procedure RealSetText(const Value: TCaption); override; // to update on caption changes
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AutoSize;
    property Caption;
    property DrawStyle;
    property Enabled;
    property TabStop default False;
  end;

  { TCDPanel }

  TCDPanel = class(TCDControl)
  private
    FBevelInner: TPanelBevel;
    FBevelOuter: TPanelBevel;
    FBevelWidth: TBevelWidth;
    procedure SetBevelInner(AValue: TPanelBevel);
    procedure SetBevelOuter(AValue: TPanelBevel);
    procedure SetBevelWidth(AValue: TBevelWidth);
  protected
    FPState: TCDPanelStateEx;
    function GetControlId: TCDControlID; override;
    procedure CreateControlStateEx; override;
    procedure PrepareControlStateEx; override;
    procedure RealSetText(const Value: TCaption); override; // to update on caption changes
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    //property AutoSize;
    property BevelInner: TPanelBevel read FBevelInner write SetBevelInner default bvNone;
    property BevelOuter: TPanelBevel read FBevelOuter write SetBevelOuter default bvRaised;
    property BevelWidth: TBevelWidth read FBevelWidth write SetBevelWidth default 1;
    property Caption;
    property DrawStyle;
    property Enabled;
    property TabStop default False;
  end;

  // ===================================
  // Additional Tab
  // ===================================

  { TCDStaticText }

  TCDStaticText = class(TCDControl)
  protected
    function GetControlId: TCDControlID; override;
    procedure RealSetText(const Value: TCaption); override; // to update on caption changes
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Caption;
    property DrawStyle;
    property Enabled;
    property TabStop default False;
  end;

  // ===================================
  // Common Controls Tab
  // ===================================

  {@@
    TCDTrackBar is a custom-drawn trackbar control
  }

  { TCDTrackBar }

  TCDTrackBar = class(TCDPositionedControl)
  private
    FOrientation: TTrackBarOrientation;
    procedure SetOrientation(AValue: TTrackBarOrientation);
  protected
    function GetPositionFromMousePos(X, Y: Integer): integer; override;
    function GetPositionDisplacement(AOldMousePos, ANewMousePos: TPoint): Integer; override;
    function GetControlId: TCDControlID; override;
    procedure PrepareControlState; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //procedure Paint; override;
  published
    property Align;
    property Color;
    property DrawStyle;
    property Enabled;
    property Orientation: TTrackBarOrientation read FOrientation write SetOrientation default trHorizontal;
    property TabStop default True;
  end;

  { TCDProgressBar }

  TCDProgressBar = class(TCDControl)
  private
    //DragDropStarted: boolean;
    FBarShowText: Boolean;
    // fields
    FMin: integer;
    FMax: integer;
    FOrientation: TProgressBarOrientation;
    FPosition: integer;
    FOnChange: TNotifyEvent;
    FSmooth: Boolean;
    FStyle: TProgressBarStyle;
    procedure SetBarShowText(AValue: Boolean);
    procedure SetMax(AValue: integer);
    procedure SetMin(AValue: integer);
    procedure SetOrientation(AValue: TProgressBarOrientation);
    procedure SetPosition(AValue: integer);
    procedure SetSmooth(AValue: Boolean);
    procedure SetStyle(AValue: TProgressBarStyle);
  protected
    FPBState: TCDProgressBarStateEx;
    function GetControlId: TCDControlID; override;
    procedure CreateControlStateEx; override;
    procedure PrepareControlStateEx; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BarShowText: Boolean read FBarShowText write SetBarShowText;
    property Color;
    property DrawStyle;
    property Enabled;
    property Max: integer read FMax write SetMax default 10;
    property Min: integer read FMin write SetMin default 0;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Orientation: TProgressBarOrientation read FOrientation write SetOrientation;// default prHorizontal;
    property Position: integer read FPosition write SetPosition;
    property Smooth: Boolean read FSmooth write SetSmooth;
    property Style: TProgressBarStyle read FStyle write SetStyle;
  end;

  { TCDListBox -- the customdrawn host for TCustomListBox.

    Owns a TStringList (Items), a TBits (Selected), and inherits a
    vertical TCDScrollBar from TCDScrollableControl. Mouse + keyboard
    nav / selection are wired here; the drawer renders the visible
    rows + the highlight.

    Selection model:
      MultiSelect=False              => single (clicking sets one item)
      MultiSelect=True, ExtendedSelect=False
                                     => each click toggles
      MultiSelect=True, ExtendedSelect=True
                                     => Shift-extend, Ctrl-toggle. }

  TCDListBox = class(TCDControl)
  private
    FItems:             TStringList;
    FSelected:          TBits;
    FItemIndex:         Integer;
    FAnchor:            Integer;
    FTopIndex:          Integer;
    FItemHeight:        Integer;
    FMultiSelect:       Boolean;
    FExtendedSelect:    Boolean;
    FSorted:            Boolean;
    FOnChange:          TNotifyEvent;
    FOnSelectionChange: TNotifyEvent;
    { Type-ahead state. Letters typed within FSearchTimeoutMs of each
      other accumulate into a search prefix; we jump to the first
      item whose Caption starts with that prefix. After timeout the
      prefix resets so the next letter starts a fresh search. Works
      with multibyte UTF-8 (kanji etc.) when the IME route delivers
      commit_string -- see TypeAheadAdvance. }
    FSearchPrefix:      AnsiString;
    FSearchExpiry:      TDateTime;
    { Vertical scrollbar -- created lazily on first SetParent so that
      parenting it doesn't cascade into HandleNeeded on us before our
      own Parent is set. TCDScrollableControl cannot be used here
      because its constructor unconditionally adds the scrollbar as a
      child, which trips the handle-creation chain when Items.Add fires
      before the LCL host has hooked us up via InjectCDControl from
      ShowHide. }
    FScrollBar:         TCDScrollBar;
    procedure ScrollBarChanged(Sender: TObject);
    function  GetItems: TStrings;
    procedure SetItems(AValue: TStrings);
    procedure SetItemIndex(AValue: Integer);
    procedure SetTopIndex(AValue: Integer);
    procedure SetMultiSelect(AValue: Boolean);
    procedure SetExtendedSelect(AValue: Boolean);
    procedure SetSorted(AValue: Boolean);
    procedure SetItemHeight(AValue: Integer);
    function  ComputeItemHeight: Integer;
    function  GetVisibleCount: Integer;
    procedure EnsureScrollBar;
    procedure UpdateScrollBar;
    procedure UpdateSelectionForClick(Index: Integer; Shift: TShiftState);
    procedure EnsureVisible(Index: Integer);
    procedure FireSelectionChange;
  protected
    FLBState: TCDListBoxStateEx;
    function  GetControlId: TCDControlID; override;
    procedure CreateControlStateEx; override;
    procedure PrepareControlStateEx; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function  DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure Resize; override;
    procedure SetParent(NewParent: TWinControl); override;
    procedure ItemsChanged(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ItemAtPos(X, Y: Integer): Integer;
    procedure TypeAheadAdvance(const AText: AnsiString);
    function ItemRect(Index: Integer): TRect;
    function GetSelected(AIndex: Integer): Boolean;
    procedure SetSelected(AIndex: Integer; ASel: Boolean);
    procedure SelectRange(ALow, AHigh: Integer; ASel: Boolean);
    function GetSelCount: Integer;
    property Selected[Index: Integer]: Boolean read GetSelected write SetSelected;
  published
    property Color;
    property Enabled;
    property Font;
    property Items: TStrings read GetItems write SetItems;
    property ItemHeight: Integer read FItemHeight write SetItemHeight default 0;
    property ItemIndex: Integer read FItemIndex write SetItemIndex default -1;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
    property ExtendedSelect: Boolean read FExtendedSelect write SetExtendedSelect default True;
    property Sorted: Boolean read FSorted write SetSorted default False;
    property TabStop default True;
    property TopIndex: Integer read FTopIndex write SetTopIndex default 0;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
  end;

  { TCDCheckListBox -- TCDListBox with a per-item checkbox column.
    Click on the checkbox column toggles the item's State; click on the
    text column behaves like a normal listbox selection. VK_SPACE on the
    current item toggles its State. Header rows occupy the full row
    width with no checkbox and ignore toggle events. }

  TCDCheckListBoxClickEvent = procedure(Sender: TObject; AIndex: Integer) of object;

  TCDCheckListBox = class(TCDListBox)
  private
    FStates:        array of TCheckBoxState;
    FItemEnabled:   array of Boolean;
    FHeader:        array of Boolean;
    FAllowGrayed:   Boolean;
    FHeaderColor:   TColor;
    FHeaderBgColor: TColor;
    FOnClickCheck:  TCDCheckListBoxClickEvent;
    procedure SyncStateArrays;
    function  HitsCheckColumn(X: Integer): Boolean;
    procedure ToggleAtClick(AIndex: Integer);
  protected
    FCLBState: TCDCheckListBoxStateEx;
    function  GetControlId: TCDControlID; override;
    procedure CreateControlStateEx; override;
    procedure PrepareControlStateEx; override;
    procedure ItemsChanged(Sender: TObject); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetCheckWidth: Integer;
    function GetItemState(AIndex: Integer): TCheckBoxState;
    procedure SetItemState(AIndex: Integer; AValue: TCheckBoxState);
    function GetItemEnabled(AIndex: Integer): Boolean;
    procedure SetItemEnabled(AIndex: Integer; AValue: Boolean);
    function GetItemHeader(AIndex: Integer): Boolean;
    procedure SetItemHeader(AIndex: Integer; AValue: Boolean);
    function GetItemChecked(AIndex: Integer): Boolean;
    procedure SetItemChecked(AIndex: Integer; AValue: Boolean);
    procedure Toggle(AIndex: Integer);
    property State[AIndex: Integer]: TCheckBoxState read GetItemState write SetItemState;
    property ItemEnabled[AIndex: Integer]: Boolean read GetItemEnabled write SetItemEnabled;
    property Header[AIndex: Integer]: Boolean read GetItemHeader write SetItemHeader;
    property Checked[AIndex: Integer]: Boolean read GetItemChecked write SetItemChecked;
  published
    property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed default False;
    property HeaderColor: TColor read FHeaderColor write FHeaderColor default clInfoText;
    property HeaderBackgroundColor: TColor read FHeaderBgColor write FHeaderBgColor default clInfoBk;
    property OnClickCheck: TCDCheckListBoxClickEvent read FOnClickCheck write FOnClickCheck;
  end;

  { TCDListView }

  TCDListView = class(TCDScrollableControl)
  private
    //DragDropStarted: boolean;
    // fields
    FColumns: TListColumns;
    //FIconOptions: TIconOptions;
    FListItems: TCDListItems;
    //FProperties: TListViewProperties;
    FShowColumnHeader: Boolean;
    FViewStyle: TViewStyle;
    function GetProperty(AIndex: Integer): Boolean;
    procedure SetColumns(AValue: TListColumns);
    procedure SetProperty(AIndex: Integer; AValue: Boolean);
    procedure SetShowColumnHeader(AValue: Boolean);
    procedure SetViewStyle(AValue: TViewStyle);
  protected
{    // keyboard
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure KeyUp(var Key: word; Shift: TShiftState); override;
    // mouse
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;}
  protected
    FLVState: TCDListViewStateEx;
    function GetControlId: TCDControlID; override;
    procedure CreateControlStateEx; override;
    procedure PrepareControlStateEx; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Color;
    property TabStop default True;
    property Columns: TListColumns read FColumns write SetColumns;
    property Enabled;
    //property GridLines: Boolean index Ord(lvpGridLines) read GetProperty write SetProperty default False;
    property Items: TCDListItems read FListItems;
    property ScrollBars;
    property ShowColumnHeader: Boolean read FShowColumnHeader write SetShowColumnHeader default True;
    property ViewStyle: TViewStyle read FViewStyle write SetViewStyle default vsList;
  end;

  { TCDToolBar }

  { TCDStatusBar -- the customdrawn host for TStatusBar.

    Holds a borrowed TCollection reference to the LCL TStatusBar's
    Panels (TStatusPanels needs a TStatusBar to construct, so we can't
    own one of our own; the injected bridge sets PanelsRef on every
    PrepareControlState). Plus SimpleText / SimplePanel; the drawer
    renders a sunken-top strip with per-panel bevels following the
    LCL's "Width=0 panels share the leftover" layout rule. }
  TCDStatusBar = class(TCDControl)
  private
    FPanelsRef:   TCollection;     { borrowed reference; never freed }
    FSimpleText:  TCaption;
    FSimplePanel: Boolean;
    FSizeGrip:    Boolean;
    procedure SetSimpleText(const AValue: TCaption);
    procedure SetSimplePanel(AValue: Boolean);
  protected
    FSBState: TCDStatusBarStateEx;
    function  GetControlId: TCDControlID; override;
    procedure CreateControlStateEx; override;
    procedure PrepareControlStateEx; override;
    class function GetControlClassDefaultSize: TSize; override;
  public
    constructor Create(AOwner: TComponent); override;
    property PanelsRef: TCollection read FPanelsRef write FPanelsRef;
  published
    property Color;
    property Font;
    property Enabled;
    property SimpleText:  TCaption read FSimpleText write SetSimpleText;
    property SimplePanel: Boolean  read FSimplePanel write SetSimplePanel default True;
    property SizeGrip:    Boolean  read FSizeGrip write FSizeGrip default True;
  end;

  TCDToolBar = class(TCDControl)
  private
    // fields
    FFlat: Boolean;
    FShowCaptions: Boolean;
    FItems: TFPList;
    procedure SetFlat(AValue: Boolean);
    procedure SetShowCaptions(AValue: Boolean);
  protected
    FTBState: TCDToolBarStateEx;
    function GetControlId: TCDControlID; override;
    procedure CreateControlStateEx; override;
    procedure PrepareControlStateEx; override;
    // mouse
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseLeave; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function InsertItem(AKind: TCDToolbarItemKind; AIndex: Integer): TCDToolBarItem;
    function AddItem(AKind: TCDToolbarItemKind): TCDToolBarItem;
    procedure DeleteItem(AIndex: Integer);
    function GetItem(AIndex: Integer): TCDToolBarItem;
    function GetItemCount(): Integer;
    function GetItemWithMousePos(APosInControl: TPoint): TCDToolBarItem;
    function GetItemWithMousePos(APosInControl: TPoint; out OnArrow: Boolean): TCDToolBarItem;
    function IsPosInButton(APosInControl: TPoint; AItem: TCDToolBarItem; AItemX: Integer): Boolean;
  published
    property Flat: Boolean read FFlat write SetFlat default true;
    property ShowCaptions: Boolean read FShowCaptions write SetShowCaptions default false;
    property DrawStyle;
  end;

  { TCDTabControl }

  { TCDCustomTabControl }

  TCDCustomTabControl = class;

  { TCDTabSheet }

  TCDTabSheet = class(TCustomControl)
  private
    CDTabControl: TCDCustomTabControl;
    FTabVisible: Boolean;
  protected
    procedure RealSetText(const Value: TCaption); override; // to update on caption changes
    procedure SetParent(NewParent: TWinControl); override; // For being created by the LCL resource reader
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
  published
    property Caption;
    property Color;
    property Font;
    property TabVisible: Boolean read FTabVisible write FTabVisible;
  end;

  // If the sender is a TCDPageControl, APage will contain the page,
  // but if it is a TCDTabControl APage will be nil
  TOnUserAddedPage = procedure (Sender: TObject; APage: TCDTabSheet) of object;

  TCDCustomTabControl = class(TCDControl)
  private
    FOnUserAddedPage: TOnUserAddedPage;
    FTabIndex: Integer;
    FTabs: TStringList;
    FOnChanging: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOptions: TCTabControlOptions;
    procedure SetOptions(AValue: TCTabControlOptions);
    //procedure MouseEnter; override;
    //procedure MouseLeave; override;
    procedure SetTabIndex(AValue: Integer); virtual;
    procedure SetTabs(AValue: TStringList);
    function MousePosToTabIndex(X, Y: Integer): Integer;
  protected
    FTabCState: TCDCTabControlStateEx;
    function GetControlId: TCDControlID; override;
    procedure CreateControlStateEx; override;
    procedure PrepareControlStateEx; override;
    procedure CorrectTabIndex();
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    //procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    property Options: TCTabControlOptions read FOptions write SetOptions;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetTabCount: Integer;
    property Tabs: TStringList read FTabs write SetTabs;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnUserAddedPage: TOnUserAddedPage read FOnUserAddedPage write FOnUserAddedPage;
    property TabIndex: integer read FTabIndex write SetTabIndex;
  end;

//  TTabSelectedEvent = procedure(Sender: TObject; ATab: TTabItem;
//    ASelected: boolean) of object;

  TCDTabControl = class(TCDCustomTabControl)
  published
    property Color;
    property Enabled;
    property Font;
    property Tabs;
    property TabIndex;
    property OnChanging;
    property OnChange;
    property OnUserAddedPage;
  end;

  { TCDPageControl }

  TCDPageControl = class(TCDCustomTabControl)
  private
    function GetActivePage: TCDTabSheet;
    function GetPageCount: integer;
    function GetPageIndex: integer;
    procedure SetActivePage(Value: TCDTabSheet);
    procedure SetPageIndex(Value: integer);
    procedure UpdateAllDesignerFlags;
    procedure UpdateDesignerFlags(APageIndex: integer);
    procedure PositionTabSheet(ATabSheet: TCDTabSheet);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function InsertPage(aIndex: integer; S: string): TCDTabSheet;
    procedure RemovePage(aIndex: integer);
    function AddPage(S: string): TCDTabSheet; overload;
    procedure AddPage(APage: TCDTabSheet); overload;
    function GetPage(aIndex: integer): TCDTabSheet;
    { Content rect inside the tab control, excluding the tab-header strip.
      Used by the customdrawn host's TCDWSCustomTabControl to lay out
      LCL TTabSheets (Align := alClient by default) inside the content
      area, the same way Win32's TabControlClientOffset / TCM_AdjustRect
      does it natively. ASize is the requested size of the host LCL
      TPageControl -- NOT this injected control's own Width/Height,
      which would be circular when this control is itself alClient'd
      to the host's ClientRect. }
    function GetTabContentRect(ASize: TSize): TRect;
    property PageCount: integer read GetPageCount;
    // Used by the property editor in customdrawnextras
    function FindNextPage(CurPage: TCDTabSheet;
      GoForward, CheckTabVisible: boolean): TCDTabSheet;
    procedure SelectNextPage(GoForward: boolean; CheckTabVisible: boolean = True);
  published
    property Align;
    property ActivePage: TCDTabSheet read GetActivePage write SetActivePage;
    property DrawStyle;
    property Caption;
    property Color;
    property Enabled;
    property Font;
    property PageIndex: integer read GetPageIndex write SetPageIndex;
    property Options;
    property ParentColor;
    property ParentFont;
    property TabStop default True;
    property TabIndex;
    property OnChanging;
    property OnChange;
    property OnUserAddedPage;
  end;

  // ===================================
  // Misc Tab
  // ===================================

  { TCDSpinEdit }

  TCDSpinEdit = class(TCDEdit)
  private
    FDecimalPlaces: Byte;
    FIncrement: Double;
    FMaxValue: Double;
    FMinValue: Double;
    FValue: Double;
    FUpDown: TUpDown;
    procedure SetDecimalPlaces(AValue: Byte);
    procedure SetIncrement(AValue: Double);
    procedure SetMaxValue(AValue: Double);
    procedure SetMinValue(AValue: Double);
    procedure UpDownChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: SmallInt; Direction: TUpDownDirection);
    procedure SetValue(AValue: Double);
    procedure DoUpdateText;
    procedure DoUpdateUpDown;
  protected
    procedure DoChange; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DecimalPlaces: Byte read FDecimalPlaces write SetDecimalPlaces default 0;
    property Increment: Double read FIncrement write SetIncrement;
    property MinValue: Double read FMinValue write SetMinValue;
    property MaxValue: Double read FMaxValue write SetMaxValue;
    property Value: Double read FValue write SetValue;
  end;

implementation

const
  sTABSHEET_DEFAULT_NAME = 'CTabSheet';

{ TCDControl }

procedure TCDControl.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  PrepareControlState;
  PrepareControlStateEx;
  FDrawer.CalculatePreferredSize(Canvas, GetControlId(), FState, FStateEx,
    PreferredWidth, PreferredHeight, WithThemeSpace, True);
end;

procedure TCDControl.SetState(const AValue: TCDControlState);
begin
  if AValue <> FState then
  begin
    FState := AValue;
    Invalidate;
  end;
end;

procedure TCDControl.PrepareCurrentDrawer;
var
  OldDrawer: TCDDrawer;
begin
  OldDrawer := FDrawer;
  FDrawer := GetDrawer(FDrawStyle);
  if FDrawer = nil then FDrawer := GetDrawer(dsCommon); // avoid exceptions in the object inspector if an invalid drawer is selected
  if FDrawer = nil then raise Exception.Create('[TCDControl.PrepareCurrentDrawer] No registered drawers were found. Please add the unit customdrawn_common to your uses clause and also the units of any other utilized drawers.');
  if OldDrawer <> FDrawer then FDrawer.LoadPalette();
end;

procedure TCDControl.SetDrawStyle(const AValue: TCDDrawStyle);
begin
  if FDrawStyle = AValue then exit;
  FDrawStyle := AValue;
  Invalidate;
  PrepareCurrentDrawer();

  //FCurrentDrawer.SetClientRectPos(Self);
end;

function TCDControl.GetClientRect: TRect;
begin
  // Disable this, since although it works in Win32, it doesn't seam to work in LCL-Carbon
  //if (FCurrentDrawer = nil) then
    Result := inherited GetClientRect()
  //else
    //Result := FCurrentDrawer.GetClientRect(Self);
end;

function TCDControl.GetControlId: TCDControlID;
begin
  Result := cidControl;
end;

procedure TCDControl.CreateControlStateEx;
begin
  FStateEx := TCDControlStateEx.Create;
end;

procedure TCDControl.PrepareControlState;
begin
  if Focused then FState := FState + [csfHasFocus]
  else FState := FState - [csfHasFocus];

  if Enabled then FState := FState + [csfEnabled]
  else FState := FState - [csfEnabled];
end;

procedure TCDControl.PrepareControlStateEx;
begin
  if Parent <> nil then FStateEx.ParentRGBColor := Parent.GetRGBColorResolvingParent
  else FStateEx.ParentRGBColor := clSilver;
  FStateEx.FPParentRGBColor := TColorToFPColor(FStateEx.ParentRGBColor);

  if Color = clDefault then FStateEx.RGBColor := FDrawer.GetControlDefaultColor(GetControlId())
  else FStateEx.RGBColor := GetRGBColorResolvingParent;
  FStateEx.FPRGBColor := TColorToFPColor(FStateEx.RGBColor);

  FStateEx.Caption := Caption;
  FStateEx.Font := Font;
  FStateEx.AutoSize := AutoSize;
end;

procedure TCDControl.DoEnter;
begin
  Invalidate;
  inherited DoEnter;
end;

procedure TCDControl.DoExit;
begin
  Invalidate;
  inherited DoExit;
end;

procedure TCDControl.EraseBackground(DC: HDC);
begin

end;

procedure TCDControl.Paint;
begin
  inherited Paint;

  DrawToCanvas(Canvas);
end;

procedure TCDControl.DrawToCanvas(ACanvas: TCanvas);
var
  lSize: TSize;
  lControlId: TCDControlID;
begin
  PrepareCurrentDrawer();

  lSize := Size(Width, Height);
  lControlId := GetControlId();
  PrepareControlState;
  PrepareControlStateEx;
  FDrawer.DrawControl(ACanvas, Point(0, 0), lSize, lControlId, FState, FStateEx);
end;

procedure TCDControl.MouseEnter;
begin
  FState := FState + [csfMouseOver];
  inherited MouseEnter;
end;

procedure TCDControl.MouseLeave;
begin
  FState := FState - [csfMouseOver];
  inherited MouseLeave;
end;

procedure TCDControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  { Do NOT call SetFocus here. The mouse-event router
    (CallbackMouseDown -> CDSetFocusToControl) already assigns focus
    using the host+intf model (parent LCL control = ALCLControl,
    injected = AIntfControl). Calling SetFocus on the CD control
    itself goes through WS.SetFocus(self.Handle), which calls
    CDSetFocusToControl(self, NIL) -- treating the CD control as a
    top-level focusable. For an injected CD control, that conflicts
    with the host+intf assignment that fires immediately afterwards:
    the two CDSetFocusToControl calls thrash FocusedControl /
    FocusedIntfControl and each fires a CMExit on the injected, which
    cleared edit-control selections via DoExit's DoClearSelection. }
end;

constructor TCDControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateControlStateEx;
  PrepareCurrentDrawer();
end;

destructor TCDControl.Destroy;
begin
  FStateEx.Free;
  inherited Destroy;
end;

// A CalculatePreferredSize which is utilized by LCL-CustomDrawn
procedure TCDControl.LCLWSCalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace, AAutoSize, AAllowUseOfMeasuresEx: Boolean);
begin
  PrepareControlState;
  PrepareControlStateEx;
  FStateEx.AutoSize := AAutoSize;
  FDrawer.CalculatePreferredSize(Canvas, GetControlId(), FState, FStateEx,
    PreferredWidth, PreferredHeight, WithThemeSpace, AAllowUseOfMeasuresEx);
end;

{ TCDComboBox }

function TCDComboBox.GetItems: TStrings;
begin
  Result := FItems;
end;

procedure TCDComboBox.ShowSelectItemDialogResult(ASelectedItem: Integer);
begin
  SetItemIndex(ASelectedItem);
end;

procedure TCDComboBox.SetItemIndex(AValue: Integer);
var
  lValue: Integer;
  lText: String;
begin
  lValue := AValue;

  // First basic check
  if lValue >= FItems.Count then lValue := FItems.Count - 1;
  if lValue < -1 then lValue := -1;

  // Check if the text changed too, because it might differ from the choosen item
  FItemIndex:=lValue;
  if lValue >= 0 then
  begin
    lText := FItems.Strings[lValue];
    if Lines.Text = lText then Exit;
    Text := lText;
  end;
  Invalidate;
end;

procedure TCDComboBox.SetItems(AValue: TStrings);
begin
  if Assigned(FItems) then
    FItems.Assign(AValue)
  else
    FItems := AValue;
end;

procedure TCDComboBox.SetKeyboardInputBehavior(AValue: TKeyboardInputBehavior);
begin
  if FKeyboardInputBehavior=AValue then Exit;
  FKeyboardInputBehavior:=AValue;
  if AValue = kibRequires then ControlStyle := ControlStyle + [csRequiresKeyboardInput]
  else ControlStyle := ControlStyle + [csRequiresKeyboardInput];
end;

function TCDComboBox.GetControlId: TCDControlID;
begin
  Result := cidComboBox;
end;

procedure TCDComboBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
begin
  if (X > Width - Height) then
  begin
    FIsClickingButton := True;
    FEditState.ExtraButtonState := FEditState.ExtraButtonState + [csfSunken];
    Invalidate;
    Exit;
  end;

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCDComboBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
begin
  if FIsClickingButton then
  begin
    FIsClickingButton := False;
    FEditState.ExtraButtonState := FEditState.ExtraButtonState - [csfSunken];
    Invalidate;
    if (X > Width - Height) then
    begin
      // Call the combobox dialog
      LCLIntf.OnShowSelectItemDialogResult := @ShowSelectItemDialogResult;
      LCLIntf.ShowSelectItemDialog(FItems, Self.ClientToScreen(Point(Left, Top+Height)));

      Exit;
    end;
  end;

  inherited MouseUp(Button, Shift, X, Y);
end;

constructor TCDComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // The keyboard input is mostly an annoyance in the combobox in Android,
  // but we offer the property RequiresKeyboardInput to override this setting
  ControlStyle := ControlStyle - [csRequiresKeyboardInput];

  FItems := TStringList.Create;
end;

destructor TCDComboBox.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

{ TCDPanel }

function TCDPanel.GetControlId: TCDControlID;
begin
  Result := cidPanel;
end;

procedure TCDPanel.CreateControlStateEx;
begin
  FPState := TCDPanelStateEx.Create;
  FStateEx := FPState;
end;

procedure TCDPanel.PrepareControlStateEx;
begin
  inherited PrepareControlStateEx;
  FPState.BevelInner := FBevelInner;
  FPState.BevelOuter := FBevelOuter;
  FPState.BevelWidth := FBevelWidth;
end;

procedure TCDPanel.SetBevelInner(AValue: TPanelBevel);
begin
  if FBevelInner=AValue then Exit;
  FBevelInner:=AValue;
  if not (csLoading in ComponentState) then Invalidate;
end;

procedure TCDPanel.SetBevelOuter(AValue: TPanelBevel);
begin
  if FBevelOuter=AValue then Exit;
  FBevelOuter:=AValue;
  if not (csLoading in ComponentState) then Invalidate;
end;

procedure TCDPanel.SetBevelWidth(AValue: TBevelWidth);
begin
  if FBevelWidth=AValue then Exit;
  FBevelWidth:=AValue;
  if not (csLoading in ComponentState) then Invalidate;
end;

procedure TCDPanel.RealSetText(const Value: TCaption);
begin
  inherited RealSetText(Value);
  if not (csLoading in ComponentState) then Invalidate;
end;

constructor TCDPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 170;
  Height := 50;
  TabStop := False;
  AutoSize := False;
end;

destructor TCDPanel.Destroy;
begin
  inherited Destroy;
end;

{ TCDScrollableControl }

procedure TCDScrollableControl.SetScrollBars(AValue: TScrollStyle);
begin
  if FScrollBars=AValue then Exit;
  FScrollBars:=AValue;

  if AValue = ssNone then
  begin
    FSpacer.Visible := False;
    FRightScrollBar.Visible := False;
    FBottomScrollBar.Visible := False;
  end
  else if AValue in [ssHorizontal, ssAutoHorizontal] then
  begin
    FSpacer.Visible := False;
    FRightScrollBar.Visible := False;
    FBottomScrollBar.BorderSpacing.Bottom := 0;
    FBottomScrollBar.Align := alRight;
    FBottomScrollBar.Visible := True;
  end
  else if AValue in [ssVertical, ssAutoVertical] then
  begin
    FSpacer.Visible := False;
    FRightScrollBar.BorderSpacing.Bottom := 0;
    FRightScrollBar.Align := alRight;
    FRightScrollBar.Visible := True;
    FBottomScrollBar.Visible := False;
  end
  else // ssBoth, ssAutoBoth
  begin
    FSpacer.Visible := True;

    // alRight and alBottom seam to work differently, so here we don't need the spacing
    FRightScrollBar.BorderSpacing.Bottom := 0;
    FRightScrollBar.Align := alRight;
    FRightScrollBar.Visible := True;

    // Enough spacing to fit the FSpacer
    FBottomScrollBar.BorderSpacing.Right := FBottomScrollBar.Height;
    FBottomScrollBar.Align := alBottom;
    FBottomScrollBar.Visible := True;
  end;
end;

constructor TCDScrollableControl.Create(AOwner: TComponent);
var
  lWidth: Integer;
begin
  inherited Create(AOwner);

  FRightScrollBar := TCDScrollBar.Create(nil);
  FRightScrollBar.Kind := sbVertical;
  FRightScrollBar.Visible := False;
  FRightScrollBar.Parent := Self;
  // Invert the dimensions because they are not automatically inverted in Loading state
  lWidth := FRightScrollBar.Width;
  FRightScrollBar.Width := FRightScrollBar.Height;
  FRightScrollBar.Height := lWidth;

  FBottomScrollBar := TCDScrollBar.Create(nil);
  FBottomScrollBar.Kind := sbHorizontal;
  FBottomScrollBar.Visible := False;
  FBottomScrollBar.Parent := Self;

  FSpacer := TCDControl.Create(nil);
  FSpacer.Color := FDrawer.Palette.BtnFace;
  FSpacer.Visible := False;
  FSpacer.Parent := Self;
  FSpacer.Width := FRightScrollBar.Width;
  FSpacer.Height := FBottomScrollBar.Height;
  FSpacer.AnchorSide[akRight].Control := Self;
  FSpacer.AnchorSide[akRight].Side := asrBottom;
  FSpacer.AnchorSide[akBottom].Control := Self;
  FSpacer.AnchorSide[akBottom].Side := asrBottom;
  FSpacer.Anchors := [akRight, akBottom];
end;

destructor TCDScrollableControl.Destroy;
begin
  FRightScrollBar.Free;
  FBottomScrollBar.Free;
  FSpacer.Free;
  inherited Destroy;
end;

{ TCDButtonDrawer }

procedure TCDButtonControl.KeyDown(var Key: word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);

  if (Key = VK_SPACE) or (Key = VK_RETURN) then
    DoButtonDown();
end;

procedure TCDButtonControl.KeyUp(var Key: word; Shift: TShiftState);
begin
  if (Key = VK_SPACE) or (Key = VK_RETURN) then
  begin
    DoButtonUp();
    Self.Click; // TCustomControl does not respond to LM_CLICKED
  end;

  inherited KeyUp(Key, Shift);
end;

procedure TCDButtonControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  DoButtonDown();

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCDButtonControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  DoButtonUp();

  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCDButtonControl.MouseEnter;
begin
  Invalidate;
  inherited MouseEnter;
end;

procedure TCDButtonControl.MouseLeave;
begin
  Invalidate;
  inherited MouseLeave;
end;

procedure TCDButtonControl.DoUncheckButton;
var
  NewState: TCDControlState;
begin
  NewState := FState + [csfOff] - [csfOn, csfPartiallyOn];
  SetState(NewState);
end;

procedure TCDButtonControl.DoCheckIfFirstButtonInGroup;
var
  NewState: TCDControlState;
  i: Integer;
  lControl: TControl;
begin
  // Start with the checked value
  NewState := FState + [csfOn] - [csfOff, csfPartiallyOn];

  // Search for other buttons in the group in the same parent
  if Parent <> nil then
  begin
    for i := 0 to Parent.ControlCount - 1 do
    begin
      lControl := Parent.Controls[i];
      if (lControl is TCDButtonControl) and
        (lControl <> Self) and
        (TCDButtonControl(lControl).FGroupIndex = FGroupIndex) then
      begin
        NewState := FState + [csfOff] - [csfOn, csfPartiallyOn];
        Break;
      end;
    end;
  end;

  SetState(NewState);
end;

procedure TCDButtonControl.DoButtonDown();
var
  NewState: TCDControlState;
begin
  NewState := FState;
  if not (csfSunken in FState) then NewState := FState + [csfSunken];
  SetState(NewState);
end;

procedure TCDButtonControl.DoButtonUp();
var
  i: Integer;
  lControl: TControl;
  NewState: TCDControlState;
begin
  NewState := FState;
  if csfSunken in FState then NewState := NewState - [csfSunken];

  // For grouped buttons, call DoButtonUp for all other buttons on the same parent
  if FIsGrouped then
  begin
    NewState := NewState + [csfOn] - [csfOff, csfPartiallyOn];
    if Parent <> nil then
    begin
      for i := 0 to Parent.ControlCount - 1 do
      begin
        lControl := Parent.Controls[i];
        if (lControl is TCDButtonControl) and
          (lControl <> Self) and
          (TCDButtonControl(lControl).FGroupIndex = FGroupIndex) then
          TCDButtonControl(lControl).DoUncheckButton();
      end;
    end;
  end
  // Only for buttons with checked/down states
  // TCDCheckbox, TCDRadiobutton, TCDButton configured as TToggleButton
  else if FHasOnOffStates then
  begin
    if FAllowGrayed then
    begin
      if csfOn in FState then
        NewState := NewState + [csfOff] - [csfOn, csfPartiallyOn]
      else if csfPartiallyOn in FState then
        NewState := NewState + [csfOn] - [csfOff, csfPartiallyOn]
      else
        NewState := NewState + [csfPartiallyOn] - [csfOn, csfOff];
    end
    else
    begin
      if csfOn in FState then
        NewState := NewState + [csfOff] - [csfOn]
      else
        NewState := NewState + [csfOn] - [csfOff];
    end;
  end;

  SetState(NewState);
end;

procedure TCDButtonControl.RealSetText(const Value: TCaption);
begin
  inherited RealSetText(Value);
  Invalidate;
end;

function TCDButtonControl.GetChecked: Boolean;
begin
  Result := csfOn in FState;
end;

procedure TCDButtonControl.SetChecked(AValue: Boolean);
var
  NewState: TCDControlState;
begin
  // In grouped elements when setting to true we do the full group sequence
  // but when setting to false we just uncheck the element
  if FIsGrouped and AValue then DoButtonUp()
  else
  begin
    if AValue then NewState := FState + [csfOn] - [csfOff, csfPartiallyOn]
    else NewState := FState + [csfOff] - [csfOn, csfPartiallyOn];
    SetState(NewState);
  end;
end;

function TCDButtonControl.GetCheckedState: TCheckBoxState;
begin
  if csfOn in FState then Result := cbChecked
  else if csfPartiallyOn in FState then
  begin
    if FAllowGrayed then
      Result := cbGrayed
    else
      Result := cbChecked;
  end
  else Result := cbUnchecked;
end;

procedure TCDButtonControl.SetCheckedState(AValue: TCheckBoxState);
var
  NewState: TCDControlState;
begin
  case AValue of
    cbUnchecked:  NewState := FState + [csfOff] - [csfOn, csfPartiallyOn];
    cbChecked:    NewState := FState + [csfOn] - [csfOff, csfPartiallyOn];
    cbGrayed:
    begin
      if FAllowGrayed then
        NewState := FState + [csfPartiallyOn] - [csfOn, csfOff]
      else
        NewState := FState + [csfOn] - [csfOff, csfPartiallyOn];
    end;
  end;
  SetState(NewState);
end;

{ TCDEdit }

procedure TCDEdit.SetLeftTextMargin(AValue: Integer);
begin
  if FEditState.LeftTextMargin = AValue then Exit;
  FEditState.LeftTextMargin := AValue;
  Invalidate;
end;

procedure TCDEdit.SetLines(AValue: TStrings);
begin
  if FLines=AValue then Exit;
  FLines.Assign(AValue);
  DoChange();
  Invalidate;
end;

procedure TCDEdit.SetMultiLine(AValue: Boolean);
begin
  if FEditState.MultiLine=AValue then Exit;
  FEditState.MultiLine := AValue;
  Invalidate;
end;

procedure TCDEdit.SetRightTextMargin(AValue: Integer);
begin
  if FEditState.RightTextMargin = AValue then Exit;
  FEditState.RightTextMargin := AValue;
  Invalidate;
end;

procedure TCDEdit.SetText(AValue: string);
begin
  Lines.Text := aValue;
end;

procedure TCDEdit.SetPasswordChar(AValue: Char);
begin
  if AValue=FEditState.PasswordChar then Exit;
  FEditState.PasswordChar := AValue;
  Invalidate;
end;

function TCDEdit.GetControlId: TCDControlID;
begin
  Result := cidEdit;
end;

procedure TCDEdit.CreateControlStateEx;
begin
  FEditState := TCDEditStateEx.Create;
  FStateEx := FEditState;
end;

procedure TCDEdit.RealSetText(const Value: TCaption);
begin
  inherited RealSetText(Value);
  Lines.Text := Value;
  Invalidate;
end;

procedure TCDEdit.DoChange;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TCDEdit.HandleCaretTimer(Sender: TObject);
begin
  if FEditState.EventArrived then
  begin
    FEditState.CaretIsVisible := True;
    FEditState.EventArrived := False;
  end
  else FEditState.CaretIsVisible := not FEditState.CaretIsVisible;

  Invalidate;
end;

function TCDEdit.GetLeftTextMargin: Integer;
begin
  Result := FEditState.LeftTextMargin;
end;

function TCDEdit.GetCaretPos: TPoint;
begin
  Result := FEditState.CaretPos;
end;

function TCDEdit.GetMultiLine: Boolean;
begin
  Result := FEditState.MultiLine;
end;

function TCDEdit.GetRightTextMargin: Integer;
begin
  Result := FEditState.RightTextMargin;
end;

function TCDEdit.GetText: string;
begin
  if Multiline then
    result := Lines.Text
  else if Lines.Count = 0 then
    result := ''
  else
    result := Lines[0];
end;

function TCDEdit.GetPasswordChar: Char;
begin
  Result := FEditState.PasswordChar;
end;

procedure TCDEdit.DoDeleteSelection;
var
  lSelLeftPos, lSelRightPos, lSelLength: Integer;
  lControlText, lTextLeft, lTextRight: string;
begin
  if IsSomethingSelected then
  begin
    lSelLeftPos := FEditState.SelStart.X;
    if FEditState.SelLength < 0 then lSelLeftPos := lSelLeftPos + FEditState.SelLength;
    lSelRightPos := FEditState.SelStart.X;
    if FEditState.SelLength > 0 then lSelRightPos := lSelRightPos + FEditState.SelLength;
    lSelLength := FEditState.SelLength;
    if lSelLength < 0 then lSelLength := lSelLength * -1;
    lControlText := GetCurrentLine();

    // Text left of the selection
    lTextLeft := UTF8Copy(lControlText, FEditState.VisibleTextStart.X, lSelLeftPos-FEditState.VisibleTextStart.X+1);

    // Text right of the selection
    lTextRight := UTF8Copy(lControlText, lSelLeftPos+lSelLength+1, Length(lControlText));

    // Execute the deletion
    SetCurrentLine(lTextLeft + lTextRight);

    // Correct the caret position
    FEditState.CaretPos.X := Length(lTextLeft);
  end;

  DoClearSelection;
end;

procedure TCDEdit.DoClearSelection;
begin
  FEditState.SelStart.X := 1;
  FEditState.SelStart.Y := 0;
  FEditState.SelLength := 0;
end;

// Imposes sanity limits to the visible text start
// and also imposes sanity limits on the caret
procedure TCDEdit.DoManageVisibleTextStart;
var
  lVisibleText, lLineText: String;
  lVisibleTextCharCount: Integer;
  lAvailableWidth: Integer;
begin
  // Moved to the left and we need to adjust the text start
  FEditState.VisibleTextStart.X := Min(FEditState.CaretPos.X+1, FEditState.VisibleTextStart.X);

  // Moved to the right and we need to adjust the text start
  lLineText := GetCurrentLine();
  lVisibleText := UTF8Copy(lLineText, FEditState.VisibleTextStart.X, Length(lLineText));
  lAvailableWidth := Width
   - FDrawer.GetMeasures(TCDEDIT_LEFT_TEXT_SPACING)
   - FDrawer.GetMeasures(TCDEDIT_RIGHT_TEXT_SPACING);
  lVisibleTextCharCount := Canvas.TextFitInfo(lVisibleText, lAvailableWidth);
  FEditState.VisibleTextStart.X := Max(FEditState.CaretPos.X-lVisibleTextCharCount+1, FEditState.VisibleTextStart.X);

  // Moved upwards and we need to adjust the text start
  FEditState.VisibleTextStart.Y := Min(FEditState.CaretPos.Y, FEditState.VisibleTextStart.Y);

  // Moved downwards and we need to adjust the text start
  FEditState.VisibleTextStart.Y := Max(FEditState.CaretPos.Y-FEditState.FullyVisibleLinesCount, FEditState.VisibleTextStart.Y);

  // Impose limits in the caret too
  FEditState.CaretPos.X := Min(FEditState.CaretPos.X, UTF8Length(lLineText));
  FEditState.CaretPos.Y := Min(FEditState.CaretPos.Y, FEditState.Lines.Count-1);
  FEditState.CaretPos.Y := Max(FEditState.CaretPos.Y, 0);
end;

procedure TCDEdit.SetCaretPost(AValue: TPoint);
begin
  FEditState.CaretPos.X := AValue.X;
  FEditState.CaretPos.Y := AValue.Y;
  Invalidate;
end;

// Result.X -> returns a zero-based position of the caret
function TCDEdit.MousePosToCaretPos(X, Y: Integer): TPoint;
var
  lStrLen, i: PtrInt;
  lVisibleStr, lCurChar: String;
  lPos, lCurCharLen: Integer;
  lBestDiff: Cardinal = $FFFFFFFF;
  lLastDiff: Cardinal = $FFFFFFFF;
  lCurDiff, lBestMatch: Integer;
begin
  // Find the best Y position
  lPos := Y - FDrawer.GetMeasures(TCDEDIT_TOP_TEXT_SPACING);
  Result.Y := lPos div FEditState.LineHeight;
  Result.Y := Min(Result.Y, FEditState.FullyVisibleLinesCount);
  Result.Y := Min(Result.Y, FEditState.Lines.Count-1);
  if Result.Y < 0 then
  begin
    Result.X := 1;
    Result.Y := 0;
    Exit;
  end;

  // Find the best X position
  Canvas.Font := Font;
  lVisibleStr := FLines.Strings[Result.Y];
  lVisibleStr := UTF8Copy(lVisibleStr, FEditState.VisibleTextStart.X, Length(lVisibleStr));
  lVisibleStr := TCDDrawer.VisibleText(lVisibleStr, FEditState.PasswordChar);
  lStrLen := UTF8Length(lVisibleStr);
  lPos := FDrawer.GetMeasures(TCDEDIT_LEFT_TEXT_SPACING);
  lBestMatch := 0;
  for i := 0 to lStrLen do
  begin
    lCurDiff := X - lPos;
    if lCurDiff < 0 then lCurDiff := lCurDiff * -1;

    if lCurDiff < lBestDiff then
    begin
      lBestDiff := lCurDiff;
      lBestMatch := i;
    end;

    // When the diff starts to grow we already found the caret pos, so exit
    if lCurDiff > lLastDiff then Break
    else lLastDiff := lCurDiff;

    if i <> lStrLen then
    begin
      lCurChar := UTF8Copy(lVisibleStr, i+1, 1);
      lCurCharLen := Canvas.TextWidth(lCurChar);
      lPos := lPos + lCurCharLen;
    end;
  end;

  Result.X := lBestMatch+(FEditState.VisibleTextStart.X-1);
  Result.X := Min(Result.X, FEditState.VisibleTextStart.X+lStrLen-1);
end;

function TCDEdit.IsSomethingSelected: Boolean;
begin
  Result := FEditState.SelLength <> 0;
end;

procedure TCDEdit.DoEnter;
begin
  FCaretTimer.Enabled := True;
  FEditState.CaretIsVisible := True;
  inherited DoEnter;
end;

procedure TCDEdit.DoExit;
begin
  FCaretTimer.Enabled := False;
  FEditState.CaretIsVisible := False;
  DoClearSelection();
  inherited DoExit;
end;

procedure TCDEdit.KeyDown(var Key: word; Shift: TShiftState);
var
  lLeftText, lRightText, lOldText: String;
  lOldTextLength: PtrInt;
  lKeyWasProcessed: Boolean = True;
begin
  inherited KeyDown(Key, Shift);

  lOldText := GetCurrentLine();
  lOldTextLength := UTF8Length(lOldText);
  FEditState.SelStart.Y := FEditState.CaretPos.Y;//ToDo: Change this when proper multi-line selection is implemented

  case Key of
  // Backspace
  VK_BACK:
  begin
    // Selection backspace
    if IsSomethingSelected() then
      DoDeleteSelection()
    // Normal backspace
    else if FEditState.CaretPos.X > 0 then
    begin
      lLeftText := UTF8Copy(lOldText, 1, FEditState.CaretPos.X-1);
      lRightText := UTF8Copy(lOldText, FEditState.CaretPos.X+1, lOldTextLength);
      SetCurrentLine(lLeftText + lRightText);
      Dec(FEditState.CaretPos.X);
      DoManageVisibleTextStart();
      Invalidate;
    end;
  end;
  // DEL
  VK_DELETE:
  begin
    // Selection delete
    if IsSomethingSelected() then
      DoDeleteSelection()
    // Normal delete
    else if FEditState.CaretPos.X < lOldTextLength then
    begin
      lLeftText := UTF8Copy(lOldText, 1, FEditState.CaretPos.X);
      lRightText := UTF8Copy(lOldText, FEditState.CaretPos.X+2, lOldTextLength);
      SetCurrentLine(lLeftText + lRightText);
      Invalidate;
    end;
  end;
  VK_LEFT:
  begin
    if (FEditState.CaretPos.X > 0) then
    begin
      // Selecting to the left
      if [ssShift] = Shift then
      begin
        if FEditState.SelLength = 0 then FEditState.SelStart.X := FEditState.CaretPos.X;
        Dec(FEditState.SelLength);
      end
      // Normal move to the left
      else FEditState.SelLength := 0;

      Dec(FEditState.CaretPos.X);
      DoManageVisibleTextStart();
      FEditState.CaretIsVisible := True;
      Invalidate;
    end
    // if we are not moving, at least deselect
    else if ([ssShift] <> Shift) then
    begin
      FEditState.SelLength := 0;
      Invalidate;
    end;
  end;
  VK_HOME:
  begin
    if (FEditState.CaretPos.X > 0) then
    begin
      // Selecting to the left
      if [ssShift] = Shift then
      begin
        if FEditState.SelLength = 0 then
        begin
          FEditState.SelStart.X := FEditState.CaretPos.X;
          FEditState.SelLength := -1 * FEditState.CaretPos.X;
        end
        else
          FEditState.SelLength := -1 * FEditState.SelStart.X;
      end
      // Normal move to the left
      else FEditState.SelLength := 0;

      FEditState.CaretPos.X := 0;
      DoManageVisibleTextStart();
      FEditState.CaretIsVisible := True;
      Invalidate;
    end
    // if we are not moving, at least deselect
    else if (FEditState.SelLength <> 0) and ([ssShift] <> Shift) then
    begin
      FEditState.SelLength := 0;
      Invalidate;
    end;
  end;
  VK_RIGHT:
  begin
    if FEditState.CaretPos.X < lOldTextLength then
    begin
      // Selecting to the right
      if [ssShift] = Shift then
      begin
        if FEditState.SelLength = 0 then FEditState.SelStart.X := FEditState.CaretPos.X;
        Inc(FEditState.SelLength);
      end
      // Normal move to the right
      else FEditState.SelLength := 0;

      Inc(FEditState.CaretPos.X);
      DoManageVisibleTextStart();
      FEditState.CaretIsVisible := True;
      Invalidate;
    end
    // if we are not moving, at least deselect
    else if ([ssShift] <> Shift) then
    begin
      FEditState.SelLength := 0;
      Invalidate;
    end;
  end;
  VK_END:
  begin
    if FEditState.CaretPos.X < lOldTextLength then
    begin
      // Selecting to the right
      if [ssShift] = Shift then
      begin
        if FEditState.SelLength = 0 then
          FEditState.SelStart.X := FEditState.CaretPos.X;
        FEditState.SelLength := lOldTextLength - FEditState.SelStart.X;
      end
      // Normal move to the right
      else FEditState.SelLength := 0;

      FEditState.CaretPos.X := lOldTextLength;
      DoManageVisibleTextStart();
      FEditState.CaretIsVisible := True;
      Invalidate;
    end
    // if we are not moving, at least deselect
    else if (FEditState.SelLength <> 0) and ([ssShift] <> Shift) then
    begin
      FEditState.SelLength := 0;
      Invalidate;
    end;
  end;
  VK_UP:
  begin
    if (FEditState.CaretPos.Y > 0) then
    begin
      // Selecting downwards
      {if [ssShift] = Shift then
      begin
        if FEditState.SelLength = 0 then FEditState.SelStart.X := FEditState.CaretPos.X;
        Dec(FEditState.SelLength);
      end
      // Normal move downwards
      else} FEditState.SelLength := 0;

      Dec(FEditState.CaretPos.Y);
      DoManageVisibleTextStart();
      FEditState.CaretIsVisible := True;
      Invalidate;
    end
    // if we are not moving, at least deselect
    else if ([ssShift] <> Shift) then
    begin
      FEditState.SelLength := 0;
      Invalidate;
    end;
  end;
  VK_DOWN:
  begin
    if FEditState.CaretPos.Y < FLines.Count-1 then
    begin
      {// Selecting to the right
      if [ssShift] = Shift then
      begin
        if FEditState.SelLength = 0 then FEditState.SelStart.X := FEditState.CaretPos.X;
        Inc(FEditState.SelLength);
      end
      // Normal move to the right
      else} FEditState.SelLength := 0;

      Inc(FEditState.CaretPos.Y);
      DoManageVisibleTextStart();
      FEditState.CaretIsVisible := True;
      Invalidate;
    end
    // if we are not moving, at least deselect
    else if ([ssShift] <> Shift) then
    begin
      FEditState.SelLength := 0;
      Invalidate;
    end;
  end;
  VK_RETURN:
  begin
    if not MultiLine then Exit;
    // Selection delete
    if IsSomethingSelected() then
      DoDeleteSelection();
    // If the are no contents at the moment, add two lines, because the first one always exists for the user
    if FLines.Count = 0 then
    begin
      FLines.Add('');
      FLines.Add('');
      FEditState.CaretPos := Point(0, 1);
    end
    else
    begin
      // Get the two halves of the text separated by the cursor
      lLeftText := UTF8Copy(lOldText, 1, FEditState.CaretPos.X);
      lRightText := UTF8Copy(lOldText, FEditState.CaretPos.X+1, lOldTextLength);
      // Move the right part to a new line
      SetCurrentLine(lLeftText);
      FLines.Insert(FEditState.CaretPos.Y+1, lRightText);
      FEditState.CaretPos := Point(0, FEditState.CaretPos.Y+1);
    end;
    Invalidate;
  end;

  else
    lKeyWasProcessed := False;
  end; // case

  if lKeyWasProcessed then
  begin
    FEditState.EventArrived := True;
    Key := 0;
  end;
end;

procedure TCDEdit.KeyUp(var Key: word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);

  // copy, paste, cut, etc
  if Shift = [ssCtrl] then
  begin
    case Key of
    VK_C:
    begin
    end;
    end;
  end;
end;

procedure TCDEdit.UTF8KeyPress(var UTF8Key: TUTF8Char);
var
  lLeftText, lRightText, lOldText: String;
begin
  inherited UTF8KeyPress(UTF8Key);

  // ReadOnly disables key input
  if FReadOnly then Exit;

  // LCL-Carbon sends Backspace as a UTF-8 Char
  // LCL-Qt sends arrow left,right,up,down (#28..#31), <enter>, ESC, etc
  // Don't handle any non-char keys here because they are already handled in KeyDown
  if (UTF8Key[1] in [#0..#$1F,#$7F]) or
    ((UTF8Key[1]=#$c2) and (UTF8Key[2] in [#$80..#$9F])) then Exit;

  DoDeleteSelection;

  // Normal characters
  lOldText := GetCurrentLine();
  lLeftText := UTF8Copy(lOldText, 1, FEditState.CaretPos.X);
  lRightText := UTF8Copy(lOldText, FEditState.CaretPos.X+1, UTF8Length(lOldText));
  SetCurrentLine(lLeftText + UTF8Key + lRightText);
  Inc(FEditState.CaretPos.X);
  DoManageVisibleTextStart();
  FEditState.EventArrived := True;
  FEditState.CaretIsVisible := True;
  Invalidate;
end;

procedure TCDEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  DragDropStarted := True;

  // Caret positioning
  FEditState.CaretPos := MousePosToCaretPos(X, Y);
  FEditState.SelLength := 0;
  FEditState.SelStart.X := FEditState.CaretPos.X;
  FEditState.SelStart.Y := FEditState.CaretPos.Y;
  FEditState.EventArrived := True;
  FEditState.CaretIsVisible := True;
  Invalidate;
end;

procedure TCDEdit.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  inherited MouseMove(Shift, X, Y);

  // Mouse dragging selection
  if DragDropStarted then
  begin
    FEditState.CaretPos := MousePosToCaretPos(X, Y);
    FEditState.SelLength := FEditState.CaretPos.X - FEditState.SelStart.X;
    FEditState.EventArrived := True;
    FEditState.CaretIsVisible := True;
    Invalidate;
  end;
end;

procedure TCDEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  DragDropStarted := False;
end;

procedure TCDEdit.MouseEnter;
begin
  inherited MouseEnter;
end;

procedure TCDEdit.MouseLeave;
begin
  inherited MouseLeave;
end;

constructor TCDEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 80;
  Height := 25;
  TabStop := True;
  ControlStyle := ControlStyle - [csAcceptsControls] + [csRequiresKeyboardInput];

  // State information
  FLines := TStringList.Create;
  FEditState.VisibleTextStart := Point(1, 0);
  FEditState.Lines := FLines;
  FEditState.PasswordChar := #0;

  // Caret code
  FCaretTimer := TTimer.Create(Self);
  FCaretTimer.OnTimer := @HandleCaretTimer;
  FCaretTimer.Interval := 500;
  FCaretTimer.Enabled := False;
end;

destructor TCDEdit.Destroy;
begin
  FLines.Free;
  inherited Destroy;
  //FCaretTimer.Free; Don't free here because it is assigned with a owner
end;

function TCDEdit.GetCurrentLine: string;
begin
  if (FEditState.Lines.Count = 0) or (FEditState.CaretPos.Y >= FEditState.Lines.Count) then
    Result := ''
  else Result := FLines.Strings[FEditState.CaretPos.Y];
end;

procedure TCDEdit.SetCurrentLine(AStr: string);
begin
  if (FEditState.Lines.Count = 0) or (FEditState.CaretPos.Y >= FEditState.Lines.Count) then
  begin
    FEditState.Lines.Text := AStr;
    FEditState.VisibleTextStart.X := 1;
    FEditState.VisibleTextStart.Y := 0;
    FEditState.CaretPos.X := 0;
    FEditState.CaretPos.Y := 0;
  end
  else FLines.Strings[FEditState.CaretPos.Y] := AStr;
  DoChange();
end;

procedure TCDEdit.SetPreedit(const AText: AnsiString;
  ACursorBegin, ACursorEnd: LongInt);
begin
  FEditState.PreeditText := AText;
  FEditState.PreeditCursorBegin := ACursorBegin;
  FEditState.PreeditCursorEnd := ACursorEnd;
  Invalidate;
end;

function TCDEdit.GetPreeditText: AnsiString;
begin
  Result := FEditState.PreeditText;
end;

function TCDEdit.GetCaretPixelRect: TRect;
var
  lLine, lLeft: AnsiString;
  lLineHeight, lCaretX: Integer;
begin
  { Use the same caret-position calculation as TCDDrawerCommon.DrawCaret:
    take the substring from VisibleTextStart to CaretPos and measure its
    pixel width. We use a freshly-created bitmap canvas since this
    method may be called from outside paint, when Self.Canvas isn't
    valid. The font set on Self propagates to the canvas. }
  Result := Rect(0, 0, 1, 16);
  if FEditState = nil then Exit;
  if (FEditState.Lines = nil) or (FEditState.Lines.Count = 0) then Exit;
  if (FEditState.CaretPos.Y < 0) or (FEditState.CaretPos.Y >= FEditState.Lines.Count) then Exit;

  lLine := FEditState.Lines.Strings[FEditState.CaretPos.Y];
  { Measure from VisibleTextStart.X (1-based) to CaretPos.X. }
  lLeft := UTF8Copy(lLine, FEditState.VisibleTextStart.X,
                    FEditState.CaretPos.X - FEditState.VisibleTextStart.X + 1);
  Canvas.Font.Assign(Font);
  lCaretX := Canvas.TextWidth(lLeft) + FEditState.LeftTextMargin;
  if FEditState.PreeditText <> '' then
    Inc(lCaretX, Canvas.TextWidth(FEditState.PreeditText));
  lLineHeight := Canvas.TextHeight('Wg');
  if lLineHeight <= 0 then lLineHeight := 16;
  Result := Rect(lCaretX,
                 FEditState.CaretPos.Y * lLineHeight,
                 lCaretX + 1,
                 (FEditState.CaretPos.Y + 1) * lLineHeight);
end;

function TCDEdit.GetSelStartX: Integer;
begin
  { LCL's TCustomEdit.SelStart contract is the leftmost edge of the
    selection, or the caret position when there is no selection.
    Internally FEditState.SelStart.X is only the selection anchor; it
    can be stale after normal caret motion. }
  if FEditState.SelLength = 0 then
    Exit(FEditState.CaretPos.X);
  Result := FEditState.SelStart.X;
  if FEditState.SelLength < 0 then
    Result := Result + FEditState.SelLength;
end;

function TCDEdit.GetSelLength: Integer;
begin
  Result := FEditState.SelLength;
  if Result < 0 then Result := Result * -1;
end;

procedure TCDEdit.SetSelStartX(ANewX: Integer);
begin
  if FEditState.SelStart.X = ANewX then Exit;
  FEditState.SelStart.X := ANewX;
  if FEditState.SelLength = 0 then
    FEditState.CaretPos.X := ANewX;
  Invalidate;
end;

procedure TCDEdit.SetSelLength(ANewLength: Integer);
begin
  if FEditState.SelLength = ANewLength then Exit;
  FEditState.SelLength := ANewLength;
  if FEditState.SelLength = 0 then
    FEditState.CaretPos.X := FEditState.SelStart.X;
  Invalidate;
end;

{ TCDCheckBox }

function TCDCheckBox.GetControlId: TCDControlID;
begin
  Result := cidCheckBox;
end;

constructor TCDCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 75;
  Height := 17;
  TabStop := True;
  ControlStyle := ControlStyle - [csAcceptsControls];
  AutoSize := True;
  FHasOnOffStates := True;
  FState := FState + [csfOff];
end;

destructor TCDCheckBox.Destroy;
begin
  inherited Destroy;
end;

{ TCDButton }

procedure TCDButton.SetModalResult(const AValue: TModalResult);
begin
  if AValue=FModalResult then exit;
  FModalResult:=AValue;
end;

procedure TCDButton.SetGlyph(AValue: TBitmap);
begin
  if FGlyph=AValue then Exit;
  FGlyph.Assign(AValue);
  Invalidate;
end;

procedure TCDButton.SetKind(AKind: TBitBtnKind);
var
  ACaption: string;
  Shortcutpos: Integer;
  BitBtnImage: Integer;
  C: TCustomBitmap;
begin
  if AKind <> FKind then begin
    FKind:= AKind;
    if FKind = bkCustom then exit; // if changed to custom, don't touch other settings
    ModalResult:= BitBtnModalResults[AKind];
    ACaption:= GetButtonCaption(BitBtnImages[AKind]);
    Shortcutpos:= DeleteAmpersands(ACaption);
    Caption:= ACaption;
    if Shortcutpos > 0 then begin
      //ShortcutVal:= ACaption[Shortcutpos];
    end;
    BitBtnImage:= BitBtnImages[AKind];
    if BitBtnImage <> idButtonBase then begin
      C := GetDefaultButtonIcon(BitBtnImage);
      try
        Glyph.Assign(C);
      finally
        C.Free;
      end;
    end;
  end;
end;

procedure TCDButton.Click;
var
  Form : TCustomForm;
begin
  Form := GetParentForm(Self);

  { First we mimic the TBitBtn behavior
    A TBitBtn with Kind = bkClose should
    - Close the ParentForm if ModalResult = mrNone.
      It should not set ParentForm.ModalResult in this case
    - Close a non-modal ParentForm if ModalResult in [mrNone, mrClose]
    - In all other cases it should behave like any other TBitBtn
  }
  if (FKind = bkClose) then
  begin
    if (Form <> nil) then
    begin
      if (FModalResult = mrNone) or
         ((FModalResult = mrClose) and not (fsModal in Form.FormState)) then
      begin
        Form.Close;
        Exit;
      end;
    end;
  end;
  if ModalResult <> mrNone
  then begin
    if Form <> nil then Form.ModalResult := ModalResult;
  end;
  inherited Click;
end;

function TCDButton.GetControlId: TCDControlID;
begin
  Result := cidButton;
end;

procedure TCDButton.CreateControlStateEx;
begin
  FBState := TCDButtonStateEx.Create;
  FStateEx := FBState;
end;

procedure TCDButton.PrepareControlStateEx;
begin
  inherited PrepareControlStateEx;
  FBState.Glyph := FGlyph;
end;

constructor TCDButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TabStop := True;
  Width := 75;
  Height := 25;
  ParentFont := True;
  FGlyph := TBitmap.Create;
end;

destructor TCDButton.Destroy;
begin
  FGlyph.Free;
  inherited Destroy;
end;

{ TCDToggleBox }

constructor TCDToggleBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHasOnOffStates := True;   { DoButtonUp now flips csfOn / csfOff each click }
end;

procedure TCDToggleBox.PrepareControlState;
begin
  inherited PrepareControlState;
  { Latch the visual sunken state to the logical on/off state, so the
    button stays "pressed-looking" while checked instead of popping
    back up after each click. The transient mouse-press csfSunken from
    DoButtonDown is overlaid on top of this and clears on MouseUp. }
  if csfOn in FState then FState := FState + [csfSunken]
  else                     FState := FState - [csfSunken];
end;

{ TCDRadioButton }

function TCDRadioButton.GetControlId: TCDControlID;
begin
  Result := cidRadioButton;
end;

constructor TCDRadioButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width := 75;
  Height := 17;
  TabStop := True;
  ControlStyle := ControlStyle - [csAcceptsControls];
  AutoSize := True;
  FHasOnOffStates := True;
  FIsGrouped := True;
  FGroupIndex := -2; // special value for TCDRadioButton
  DoCheckIfFirstButtonInGroup();
end;

destructor TCDRadioButton.Destroy;
begin
  inherited Destroy;
end;

{ TCDPositionedControl }

procedure TCDPositionedControl.SetMax(AValue: Integer);
begin
  if FMax=AValue then Exit;
  FMax:=AValue;

  if AValue < FMin then FMax := FMin
  else FMax := AValue;

  if FPosition > FMax then FPosition := FMax;

  if not (csLoading in ComponentState) then Invalidate;
end;

procedure TCDPositionedControl.SetMin(AValue: Integer);
begin
  if FMin=AValue then Exit;

  if AValue > FMax then FMin := FMax
  else FMin:=AValue;

  if FPosition < FMin then FPosition := FMin;

  if not (csLoading in ComponentState) then Invalidate;
end;

procedure TCDPositionedControl.SetPageSize(AValue: Integer);
begin
  if FPageSize=AValue then Exit;
  FPageSize:=AValue;
  if not (csLoading in ComponentState) then Invalidate;
end;

procedure TCDPositionedControl.SetPosition(AValue: Integer);
var
  EffectiveMax: Integer;
begin
  { When PageSize > 0 (Win32 / scrollbar convention) the highest
    valid Position is Max - PageSize -- Position is the index of the
    first visible item, the page covers Position..Position+PageSize-1.
    For PageSize=0 (TrackBar etc.) Position can reach Max as before.

    Clamp the incoming value first, then bail if the clamped result
    matches the current Position. The previous order (assign first,
    clamp after, fire OnChange unconditionally) emitted spurious
    OnChange / OnChangeByUser notifications when the user kept
    pressing past the boundary -- callers like DoClickButton check
    `NewPosition <> Position` *before* assignment, miss the clamp,
    and still fire OnChangeByUser. Downstream that drove the LCL
    scrollbar's "+1 follows winapi bug" branch in DoScroll, which
    pushed LCL.Position to Max-PageSize+1 and then mis-routed
    subsequent gestures. }
  EffectiveMax := FMax;
  if FPageSize > 0 then EffectiveMax := EffectiveMax - FPageSize;
  if EffectiveMax < FMin then EffectiveMax := FMin;
  if AValue > EffectiveMax then AValue := EffectiveMax;
  if AValue < FMin then AValue := FMin;

  if FPosition = AValue then Exit;
  FPosition := AValue;

  // Don't do OnChange during loading
  if not (csLoading in ComponentState) then
  begin
    if Assigned(OnChange) then OnChange(Self);
    Invalidate;
  end;
end;

procedure TCDPositionedControl.DoClickButton(AButton: TCDControlState; ALargeChange: Boolean);
var
  lChange, OldPosition, NewPosition: Integer;
begin
  if ALargeChange then lChange := FLargeChange
  else lChange := FSmallChange;
  if csfLeftArrow in AButton then NewPosition := Position - lChange
  else if csfRightArrow in AButton then NewPosition := Position + lChange
  else Exit;

  { SetPosition clamps to [Min, Max-PageSize], so the requested
    NewPosition may collapse to the current FPosition at the
    boundary. Compare FPosition before/after to fire OnChangeByUser
    only on a real change. }
  OldPosition := FPosition;
  Position := NewPosition;
  if (FPosition <> OldPosition) and Assigned(FOnChangeByUser) then
    FOnChangeByUser(Self);
end;

procedure TCDPositionedControl.HandleBtnClickTimer(ASender: TObject);
var
  lButton: TCDControlState;
  lMousePos: TPoint;
begin
  lMousePos := ScreenToClient(Mouse.CursorPos);
  lButton := GetButtonFromMousePos(lMousePos.X, lMousePos.Y);
  if lButton = FButton then DoClickButton(FButton, True);
end;

function TCDPositionedControl.GetPositionFromMousePosWithMargins(X, Y,
  ALeftMargin, ARightMargin: Integer; AIsHorizontal, AAcceptMouseOutsideStrictArea: Boolean): integer;
var
  lCoord, lSize: Integer;
begin
  Result := -1;

  if AIsHorizontal then
  begin
    lCoord := X;
    lSize := Width;
  end
  else
  begin
    lCoord := Y;
    lSize := Height;
  end;

  if lCoord > lSize - ARightMargin then
  begin
    if AAcceptMouseOutsideStrictArea then Result := FMax;
    Exit;
  end
  else if lCoord < ALeftMargin then
  begin
    if AAcceptMouseOutsideStrictArea then Result := FMin;
    Exit;
  end
  else Result := FMin + (lCoord - ALeftMargin) * (FMax - FMin + 1) div (lSize - ARightMargin - ALeftMargin);

  // sanity check
  if Result > FMax then Result := FMax;
  if Result < FMin then Result := FMin;
end;

function TCDPositionedControl.GetPositionDisplacementWithMargins(AOldMousePos,
  ANewMousePos: TPoint; ALeftMargin, ARightMargin: Integer; AIsHorizontal: Boolean): Integer;
var
  lCoord, lSize, lCurPos: Integer;
begin
  if AIsHorizontal then
  begin
    lCoord := ANewMousePos.X-AOldMousePos.X;
    lSize := Width;
  end
  else
  begin
    lCoord := ANewMousePos.Y-AOldMousePos.Y;
    lSize := Height;
  end;

  Result := FMin + lCoord * (FMax - FMin + 1) div (lSize - ARightMargin - ALeftMargin);
  lCurPos := Result + FPositionAtMouseDown;

  // sanity check
  if lCurPos > FMax then Result := FMax - FPositionAtMouseDown;
  if lCurPos < FMin then Result := FMin - FPositionAtMouseDown;
end;

function TCDPositionedControl.GetButtonFromMousePos(X, Y: Integer): TCDControlState;
begin
  Result := [];
end;

procedure TCDPositionedControl.CreateControlStateEx;
begin
  FPCState := TCDPositionedCStateEx.Create;
  FStateEx := FPCState;
end;

procedure TCDPositionedControl.PrepareControlStateEx;
var
  Range, PosRange: Integer;
begin
  inherited PrepareControlStateEx;

  Range := FMax - FMin;
  { Position can move only across (Range - PageSize) when PageSize > 0
    (the rest of the trough is covered by the thumb itself). FloatPos
    must therefore divide by that, not by Range, or the thumb-bottom
    never reaches the trough-bottom. }
  PosRange := Range;
  if FPageSize > 0 then PosRange := Range - FPageSize;
  if PosRange > 0 then FPCState.FloatPos := FPosition / PosRange
  else FPCState.FloatPos := 0.0;

  FPCState.PosCount := Range + 1;
  FPCState.Position := FPosition - FMin;

  { FloatPageSize is the thumb size as a fraction of the trough.
    With PageSize / Range, a 5-of-8-visible listbox gets a 5/8 thumb. }
  if Range > 0 then FPCState.FloatPageSize := FPageSize / Range
  else FPCState.FloatPageSize := 1.0;
end;

procedure TCDPositionedControl.KeyDown(var Key: word; Shift: TShiftState);
var
  NewPosition, OldPosition: Integer;
  HandledKey: Boolean;
begin
  inherited KeyDown(Key, Shift);

  { 0 used to be the "no movement" sentinel, but 0 is a perfectly
    valid Position. Any unhandled key (Tab, letters, ...) fell into
    the clamp+assign branch and snapped Position to Max(FMin, 0). }
  NewPosition := FPosition;
  HandledKey := True;
  case Key of
    VK_LEFT, VK_DOWN:  NewPosition := FPosition - FSmallChange;
    VK_RIGHT, VK_UP:   NewPosition := FPosition + FSmallChange;
    VK_PRIOR:          NewPosition := FPosition - FLargeChange;
    VK_NEXT:           NewPosition := FPosition + FLargeChange;
  else
    HandledKey := False;
  end;

  if HandledKey then
  begin
    { Don't pre-clamp here -- SetPosition clamps to [Min, Max-PageSize].
      Compare FPosition before/after the assignment so OnChangeByUser
      only fires on real movement. }
    OldPosition := FPosition;
    Position := NewPosition;
    if (FPosition <> OldPosition) and Assigned(FOnChangeByUser) then
      FOnChangeByUser(Self);
  end;
end;

procedure TCDPositionedControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  NewPosition, OldPosition: Integer;
begin
  SetFocus;
  if FMoveByDragging then
  begin
    FLastMouseDownPos := Point(X, Y);
    FPositionAtMouseDown := Position;
    DragDropStarted := True;
  end
  else
  begin
    NewPosition := GetPositionFromMousePos(X, Y);
    DragDropStarted := True;
    if NewPosition >= 0 then
    begin
      OldPosition := FPosition;
      Position := NewPosition;
      if (FPosition <> OldPosition) and Assigned(FOnChangeByUser) then
        FOnChangeByUser(Self);
    end;
  end;

  // Check if any buttons were clicked
  FButton := GetButtonFromMousePos(X, Y);
  FState := FState + FButton;
  if FButton <> [] then
  begin
    DoClickButton(FButton, False);
    FBtnClickTimer.Enabled := True;
  end;

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCDPositionedControl.MouseMove(Shift: TShiftState; X, Y: integer);
var
  NewPosition, OldPosition: Integer;
begin
  if DragDropStarted then
  begin
    if FMoveByDragging then
      NewPosition := FPositionAtMouseDown + GetPositionDisplacement(FLastMouseDownPos, Point(X, Y))
    else
      NewPosition := GetPositionFromMousePos(X, Y);
    if NewPosition >= 0 then
    begin
      OldPosition := FPosition;
      Position := NewPosition;
      if (FPosition <> OldPosition) and Assigned(FOnChangeByUser) then
        FOnChangeByUser(Self);
    end;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TCDPositionedControl.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  DragDropStarted := False;
  FBtnClickTimer.Enabled := False;
  FState := FState - [csfLeftArrow, csfRightArrow];
  { Also clear FButton itself -- MouseDown sets it to the clicked
    arrow and the original code only cleared FState. FButton is what
    the click-repeat timer (and now CurrentButton) compare against,
    so leaving it stuck to the last-clicked arrow let
    OnChangeByUser-handlers misclassify subsequent unrelated trough
    clicks as arrow gestures. }
  FButton := [];
  Invalidate;
  inherited MouseUp(Button, Shift, X, Y);
end;

constructor TCDPositionedControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSmallChange := 1;
  FLargeChange := 5;
  FMin := 0;
  FMax := 10;
  FPosition := 0;
  FBtnClickTimer := TTimer.Create(nil);
  FBtnClickTimer.Enabled := False;
  FBtnClickTimer.Interval := 100;
  FBtnClickTimer.OnTimer := @HandleBtnClickTimer;
end;

destructor TCDPositionedControl.Destroy;
begin
  FBtnClickTimer.Free;
  inherited Destroy;
end;

function TCDPositionedControl.CurrentButton: TCDControlState;
begin
  Result := FButton;
end;

{ TCDScrollBar }

procedure TCDScrollBar.SetKind(AValue: TScrollBarKind);
begin
  if FKind=AValue then Exit;
  FKind:=AValue;

  if not (csLoading in ComponentState) then Invalidate;
end;

procedure TCDScrollBar.GetBorderSizes(out ALeft, ARight: Integer);
begin
  ALeft := FDrawer.GetMeasures(TCDSCROLLBAR_LEFT_SPACING) +
    FDrawer.GetMeasures(TCDSCROLLBAR_LEFT_BUTTON_POS) +
    FDrawer.GetMeasures(TCDSCROLLBAR_BUTTON_WIDTH);
  ARight := FDrawer.GetMeasures(TCDSCROLLBAR_RIGHT_SPACING) +
    FDrawer.GetMeasures(TCDSCROLLBAR_RIGHT_BUTTON_POS) +
    FDrawer.GetMeasures(TCDSCROLLBAR_BUTTON_WIDTH);
end;

function TCDScrollBar.GetPositionFromMousePos(X, Y: Integer): integer;
var
  lLeftBorder, lRightBorder: Integer;
begin
  GetBorderSizes(lLeftBorder, lRightBorder);

  Result := GetPositionFromMousePosWithMargins(X, Y, lLeftBorder, lRightBorder, FKind = sbHorizontal, False);
end;

function TCDScrollBar.GetButtonFromMousePos(X, Y: Integer): TCDControlState;
var
  lCoord, lLeftBtnPos, lRightBtnPos: Integer;
begin
  Result := [];
  lLeftBtnPos := FDrawer.GetMeasures(TCDSCROLLBAR_LEFT_BUTTON_POS);
  lRightBtnPos := FDrawer.GetMeasures(TCDSCROLLBAR_RIGHT_BUTTON_POS);
  if FKind = sbHorizontal then
  begin
    lCoord := X;
    if lLeftBtnPos < 0 then lLeftBtnPos := Width + lLeftBtnPos;
    if lRightBtnPos < 0 then lRightBtnPos := Width + lRightBtnPos;
  end
  else
  begin
    lCoord := Y;
    if lLeftBtnPos < 0 then lLeftBtnPos := Height + lLeftBtnPos;
    if lRightBtnPos < 0 then lRightBtnPos := Height + lRightBtnPos;
  end;

  if (lCoord > lLeftBtnPos) and (lCoord < lLeftBtnPos +
    FDrawer.GetMeasures(TCDSCROLLBAR_BUTTON_WIDTH)) then Result := [csfLeftArrow]
  else if (lCoord > lRightBtnPos) and (lCoord < lRightBtnPos +
    FDrawer.GetMeasures(TCDSCROLLBAR_BUTTON_WIDTH)) then Result := [csfRightArrow];
end;

function TCDScrollBar.GetPositionDisplacement(AOldMousePos, ANewMousePos: TPoint
  ): Integer;
var
  lLeftBorder, lRightBorder: Integer;
begin
  GetBorderSizes(lLeftBorder, lRightBorder);

  Result := GetPositionDisplacementWithMargins(AOldMousePos, ANewMousePos,
    lLeftBorder, lRightBorder, FKind = sbHorizontal);
end;

function TCDScrollBar.GetControlId: TCDControlID;
begin
  Result:= cidScrollBar;
end;

procedure TCDScrollBar.PrepareControlState;
begin
  inherited PrepareControlState;

  if FKind = sbHorizontal then
    FState := FState + [csfHorizontal] - [csfVertical, csfRightToLeft, csfTopDown]
  else FState := FState + [csfVertical] - [csfHorizontal, csfRightToLeft, csfTopDown];
end;

constructor TCDScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 121;
  Height := 17;
  FMax := 100;
  FMoveByDragging := True;
end;

procedure TCDScrollBar.KeyDown(var Key: word; Shift: TShiftState);
var
  OldPosition, Step: Integer;
begin
  { Override TCDPositionedControl.KeyDown rather than chaining: the
    inherited version maps VK_DOWN -> -FSmallChange (trackbar
    convention, up = higher value), but a scrollbar must scroll its
    content in the direction of the arrow key. Don't call inherited
    here, otherwise we'd run that mapping AND fire OnChangeByUser
    twice for the same keypress. }
  Step := 0;
  case Key of
    VK_PRIOR: Step := -FLargeChange;
    VK_NEXT:  Step := +FLargeChange;
    VK_HOME:
    begin
      OldPosition := FPosition;
      Position := FMin;
      if (FPosition <> OldPosition) and Assigned(FOnChangeByUser) then
        FOnChangeByUser(Self);
      Exit;
    end;
    VK_END:
    begin
      OldPosition := FPosition;
      Position := FMax;
      if (FPosition <> OldPosition) and Assigned(FOnChangeByUser) then
        FOnChangeByUser(Self);
      Exit;
    end;
  else
    if FKind = sbHorizontal then
      case Key of
        VK_LEFT:  Step := -FSmallChange;
        VK_RIGHT: Step := +FSmallChange;
      end
    else
      case Key of
        VK_UP:    Step := -FSmallChange;
        VK_DOWN:  Step := +FSmallChange;
      end;
  end;
  if Step = 0 then Exit;

  OldPosition := FPosition;
  Position := FPosition + Step;
  if (FPosition <> OldPosition) and Assigned(FOnChangeByUser) then
    FOnChangeByUser(Self);
end;

procedure TCDScrollBar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Coord, ButtonW, Trough, Range, PosRange, ThumbSize, ThumbStart, OldPos: Integer;
begin
  { Defer arrow-button clicks to the inherited handler. }
  if GetButtonFromMousePos(X, Y) <> [] then
  begin
    inherited MouseDown(Button, Shift, X, Y);
    Exit;
  end;
  if FLargeChange <= 0 then
  begin
    inherited MouseDown(Button, Shift, X, Y);
    Exit;
  end;
  Range := FMax - FMin;
  if Range <= 0 then
  begin
    inherited MouseDown(Button, Shift, X, Y);
    Exit;
  end;

  { Pixel-coordinate hit-test against where the drawer actually draws
    the thumb. Position-value space hit-tests miss because most of the
    trough maps to Position-values near FPosition, not to the visible
    thumb extent. Geometry matches DrawScrollBar in customdrawn_common.pas:
      ThumbSize  = PageSize / Range * Trough  (clamped to >=5)
      ThumbStart = ButtonWidth
                 + Position/(Range-PageSize) * (Trough - ThumbSize) }
  ButtonW := FDrawer.GetMeasures(TCDSCROLLBAR_BUTTON_WIDTH);
  if FKind = sbHorizontal then
  begin
    Coord  := X;
    Trough := Width - 2 * ButtonW;
  end
  else
  begin
    Coord  := Y;
    Trough := Height - 2 * ButtonW;
  end;
  if Trough <= 0 then
  begin
    inherited MouseDown(Button, Shift, X, Y);
    Exit;
  end;

  if FPageSize > 0 then ThumbSize := Round(FPageSize / Range * Trough)
  else ThumbSize := 16;
  if ThumbSize < 5 then ThumbSize := 5;
  if ThumbSize > Trough then ThumbSize := Trough;

  if FPageSize > 0 then PosRange := Range - FPageSize
  else PosRange := Range;
  if PosRange <= 0 then ThumbStart := ButtonW
  else ThumbStart := ButtonW + Round(FPosition / PosRange * (Trough - ThumbSize));

  { SetPosition clamps to [Min, Max-PageSize]; comparing FPosition
    before/after lets OnChangeByUser fire only on real movement, not
    on a boundary click that gets clamped back. }
  if Coord < ThumbStart then
  begin
    OldPos := FPosition;
    Position := FPosition - FLargeChange;
    if (FPosition <> OldPos) and Assigned(FOnChangeByUser) then
      FOnChangeByUser(Self);
    Exit;
  end;
  if Coord > ThumbStart + ThumbSize then
  begin
    OldPos := FPosition;
    Position := FPosition + FLargeChange;
    if (FPosition <> OldPos) and Assigned(FOnChangeByUser) then
      FOnChangeByUser(Self);
    Exit;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

destructor TCDScrollBar.Destroy;
begin
  inherited Destroy;
end;

procedure TCDScrollBar.SetIncrements(ASmall, ALarge: Integer);
begin
  if ASmall < 1 then ASmall := 1;
  if ALarge < 1 then ALarge := 1;
  FSmallChange := ASmall;
  FLargeChange := ALarge;
end;

{ TCDGroupBox }

function TCDGroupBox.GetControlId: TCDControlID;
begin
  Result := cidGroupBox;
end;

procedure TCDGroupBox.RealSetText(const Value: TCaption);
begin
  inherited RealSetText(Value);
  if not (csLoading in ComponentState) then Invalidate;
end;

constructor TCDGroupBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 100;
  Height := 100;
  TabStop := False;
  AutoSize := True;
end;

destructor TCDGroupBox.Destroy;
begin
  inherited Destroy;
end;

{ TCDStaticText }

function TCDStaticText.GetControlId: TCDControlID;
begin
  Result:=cidStaticText;
end;

procedure TCDStaticText.RealSetText(const Value: TCaption);
begin
  inherited RealSetText(Value);
  Invalidate;
end;

constructor TCDStaticText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 70;
  Height := 20;
  TabStop := False;
  ControlStyle := ControlStyle - [csAcceptsControls];
end;

destructor TCDStaticText.Destroy;
begin
  inherited Destroy;
end;

{ TCDTrackBar }

procedure TCDTrackBar.SetOrientation(AValue: TTrackBarOrientation);
var
  lOldWidth: Integer;
begin
  if FOrientation=AValue then Exit;

  // Invert the width and the height, but not if the property comes from the LFM
  // because the width was already inverted in the designer and stored in the new value
  if not (csLoading in ComponentState) then
  begin
    lOldWidth := Width;
    Width := Height;
    Height := lOldWidth;
  end;

  // Set the property and redraw
  FOrientation:=AValue;
  if not (csLoading in ComponentState) then
    Invalidate;
end;

function TCDTrackBar.GetPositionFromMousePos(X, Y: Integer): integer;
var
  lLeftBorder, lRightBorder: Integer;
begin
  lLeftBorder := FDrawer.GetMeasures(TCDTRACKBAR_LEFT_SPACING);
  lRightBorder := FDrawer.GetMeasures(TCDTRACKBAR_RIGHT_SPACING);

  Result := GetPositionFromMousePosWithMargins(X, Y, lLeftBorder, lRightBorder, FOrientation = trHorizontal, True);
end;

function TCDTrackBar.GetPositionDisplacement(AOldMousePos, ANewMousePos: TPoint
  ): Integer;
begin
  Result := 0; // not used anyway
end;

function TCDTrackBar.GetControlId: TCDControlID;
begin
  Result := cidTrackBar;
end;

procedure TCDTrackBar.PrepareControlState;
begin
  inherited PrepareControlState;
  case FOrientation of
  trHorizontal: FState := FState + [csfHorizontal] - [csfVertical, csfRightToLeft, csfTopDown];
  trVertical: FState := FState + [csfVertical] - [csfHorizontal, csfRightToLeft, csfTopDown];
  end;
end;

constructor TCDTrackBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Height := 25;
  Width := 100;

  TabStop := True;
end;

destructor TCDTrackBar.Destroy;
begin
  inherited Destroy;
end;

{ TCDProgressBar }

procedure TCDProgressBar.SetMax(AValue: integer);
begin
  if FMax=AValue then Exit;
  FMax:=AValue;
  if not (csLoading in ComponentState) then Invalidate;
end;

procedure TCDProgressBar.SetBarShowText(AValue: Boolean);
begin
  if FBarShowText=AValue then Exit;
  FBarShowText:=AValue;
  if not (csLoading in ComponentState) then Invalidate;
end;

procedure TCDProgressBar.SetMin(AValue: integer);
begin
  if FMin=AValue then Exit;
  FMin:=AValue;
  if not (csLoading in ComponentState) then Invalidate;
end;

procedure TCDProgressBar.SetOrientation(AValue: TProgressBarOrientation);
begin
  if FOrientation=AValue then Exit;
  FOrientation:=AValue;
  if not (csLoading in ComponentState) then Invalidate;
end;

procedure TCDProgressBar.SetPosition(AValue: integer);
begin
  if FPosition=AValue then Exit;
  FPosition:=AValue;
  if not (csLoading in ComponentState) then Invalidate;
end;

procedure TCDProgressBar.SetSmooth(AValue: Boolean);
begin
  if FSmooth=AValue then Exit;
  FSmooth:=AValue;
  if not (csLoading in ComponentState) then
    Invalidate;
end;

procedure TCDProgressBar.SetStyle(AValue: TProgressBarStyle);
begin
  if FStyle=AValue then Exit;
  FStyle:=AValue;
  if not (csLoading in ComponentState) then Invalidate;
end;

function TCDProgressBar.GetControlId: TCDControlID;
begin
  Result := cidProgressBar;
end;

procedure TCDProgressBar.CreateControlStateEx;
begin
  FPBState := TCDProgressBarStateEx.Create;
  FStateEx := FPBState;
end;

procedure TCDProgressBar.PrepareControlStateEx;
begin
  inherited PrepareControlStateEx;
  if FMax <> FMin then FPBState.PercentPosition := (FPosition-FMin)/(FMax-FMin)
  else FPBState.PercentPosition := 1.0;
  FPBState.BarShowText := FBarShowText;
  FPBState.Style := FStyle;
  case FOrientation of
  pbHorizontal:  FState := FState + [csfHorizontal] - [csfVertical, csfRightToLeft, csfTopDown];
  pbVertical:    FState := FState + [csfVertical] - [csfHorizontal, csfRightToLeft, csfTopDown];
  pbRightToLeft: FState := FState + [csfRightToLeft] - [csfVertical, csfHorizontal, csfTopDown];
  pbTopDown:     FState := FState + [csfTopDown] - [csfVertical, csfRightToLeft, csfHorizontal];
  end;
  FPBState.Smooth := FSmooth;
end;

constructor TCDProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 100;
  Height := 20;
  FMax := 100;
  TabStop := False;
end;

destructor TCDProgressBar.Destroy;
begin
  inherited Destroy;
end;

{ TCDListBox }

constructor TCDListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 150;
  Height := 100;
  TabStop := True;
  ControlStyle := ControlStyle + [csAcceptsControls];  { for child scrollbar }
  FItems := TStringList.Create;
  FItems.OnChange := @ItemsChanged;
  FSelected := TBits.Create;
  FItemIndex := -1;
  FAnchor    := -1;
  FTopIndex  := 0;
  FItemHeight := 0;          { 0 = compute from font }
  FMultiSelect := False;
  FExtendedSelect := True;
  FSorted := False;
  { FScrollBar is created lazily in EnsureScrollBar -- doing it here
    would parent it under us before we have a parent of our own,
    cascading into HandleNeeded on the listbox while it has no
    parent and tripping "Control has no parent window". }
end;

procedure TCDListBox.EnsureScrollBar;
begin
  if FScrollBar <> nil then Exit;
  if Parent = nil then Exit;          { still detached; defer }
  FScrollBar := TCDScrollBar.Create(Self);
  FScrollBar.Kind := sbVertical;
  FScrollBar.Align := alRight;
  FScrollBar.Width := 16;
  FScrollBar.Visible := False;
  FScrollBar.Parent := Self;
  FScrollBar.OnChange := @ScrollBarChanged;
end;

procedure TCDListBox.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);
  if NewParent <> nil then
  begin
    EnsureScrollBar;
    UpdateScrollBar;
  end;
end;

destructor TCDListBox.Destroy;
begin
  FItems.Free;
  FSelected.Free;
  inherited Destroy;
end;

function TCDListBox.GetControlId: TCDControlID;
begin
  Result := cidListBox;
end;

procedure TCDListBox.CreateControlStateEx;
begin
  FLBState := TCDListBoxStateEx.Create;
  FStateEx := FLBState;
end;

procedure TCDListBox.PrepareControlStateEx;
begin
  inherited PrepareControlStateEx;
  FLBState.Items     := FItems;
  FLBState.Selected  := FSelected;
  FLBState.ItemIndex := FItemIndex;
  FLBState.TopIndex  := FTopIndex;
  FLBState.ItemHeight := ComputeItemHeight;
  FLBState.MultiSelect := FMultiSelect;
  FLBState.ExtendedSelect := FExtendedSelect;
end;

function TCDListBox.GetItems: TStrings;
begin
  Result := FItems;
end;

procedure TCDListBox.SetItems(AValue: TStrings);
begin
  if AValue = nil then FItems.Clear
  else FItems.Assign(AValue);
  { ItemsChanged fires via FItems.OnChange }
end;

procedure TCDListBox.ItemsChanged(Sender: TObject);
begin
  { Reset selection bounds, clamp ItemIndex into range, push the
    scrollbar to match the new item count. }
  if FItemIndex >= FItems.Count then FItemIndex := FItems.Count - 1;
  if (FAnchor    >= FItems.Count) or (FAnchor < 0) then FAnchor := FItemIndex;
  if FTopIndex   >= FItems.Count then FTopIndex := Max(0, FItems.Count - GetVisibleCount);
  FSelected.Size := FItems.Count;
  UpdateScrollBar;
  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TCDListBox.ScrollBarChanged(Sender: TObject);
begin
  { Scrollbar position is the row index of the first visible row. }
  if FScrollBar = nil then Exit;
  if FTopIndex <> FScrollBar.Position then
  begin
    FTopIndex := FScrollBar.Position;
    Invalidate;
  end;
end;

procedure TCDListBox.SetItemIndex(AValue: Integer);
begin
  if AValue < -1 then AValue := -1;
  if AValue >= FItems.Count then AValue := FItems.Count - 1;
  if FItemIndex = AValue then Exit;
  FItemIndex := AValue;
  { Single-select: selection follows ItemIndex. }
  if not FMultiSelect then
  begin
    FSelected.Clearall;
    if FItemIndex >= 0 then FSelected[FItemIndex] := True;
  end;
  EnsureVisible(FItemIndex);
  Invalidate;
  FireSelectionChange;
end;

procedure TCDListBox.SetTopIndex(AValue: Integer);
var
  MaxTop: Integer;
begin
  MaxTop := Max(0, FItems.Count - GetVisibleCount);
  if AValue < 0 then AValue := 0;
  if AValue > MaxTop then AValue := MaxTop;
  if FTopIndex = AValue then Exit;
  FTopIndex := AValue;
  if FScrollBar <> nil then FScrollBar.Position := AValue;
  Invalidate;
end;

procedure TCDListBox.SetMultiSelect(AValue: Boolean);
begin
  if FMultiSelect = AValue then Exit;
  FMultiSelect := AValue;
  if not FMultiSelect then
  begin
    { Collapse selection back to single ItemIndex. }
    FSelected.Clearall;
    if FItemIndex >= 0 then FSelected[FItemIndex] := True;
    Invalidate;
    FireSelectionChange;
  end;
end;

procedure TCDListBox.SetExtendedSelect(AValue: Boolean);
begin
  FExtendedSelect := AValue;
end;

procedure TCDListBox.SetSorted(AValue: Boolean);
begin
  if FSorted = AValue then Exit;
  FSorted := AValue;
  FItems.Sorted := AValue;
end;

procedure TCDListBox.SetItemHeight(AValue: Integer);
begin
  if AValue < 0 then AValue := 0;
  if FItemHeight = AValue then Exit;
  FItemHeight := AValue;
  UpdateScrollBar;
  Invalidate;
end;

function TCDListBox.ComputeItemHeight: Integer;
begin
  if FItemHeight > 0 then Exit(FItemHeight);
  Canvas.Font.Assign(Font);
  Result := Canvas.TextHeight('Wg') + 2;
  if Result < 14 then Result := 14;
end;

function TCDListBox.GetVisibleCount: Integer;
var
  H: Integer;
begin
  H := Height - 4;  { 2px frame top + bottom }
  Result := H div ComputeItemHeight;
  if Result < 1 then Result := 1;
end;

procedure TCDListBox.UpdateScrollBar;
var
  Vis: Integer;
begin
  if FScrollBar = nil then Exit;
  Vis := GetVisibleCount;
  { Win32 / GTK / standard scrollbar convention: Max is the full count
    and PageSize is the visible-window size. Position ranges
    [0, Max - PageSize] -- the index of the first visible row.
    SetPosition clamps for us when PageSize > 0. The drawer reads
    PageSize / Max for the thumb fraction, so the thumb covers the
    visible-fraction of the trough. }
  FScrollBar.Min := 0;
  FScrollBar.Max := FItems.Count;
  FScrollBar.PageSize    := Vis;
  FScrollBar.FLargeChange := Vis;        { matches PageUp/PageDown + trough-click }
  if FItems.Count > Vis then
  begin
    FScrollBar.Visible := True;
    FScrollBar.Position := FTopIndex;
  end
  else
    FScrollBar.Visible := False;
end;

procedure TCDListBox.EnsureVisible(Index: Integer);
var
  Vis: Integer;
begin
  if (Index < 0) or (Index >= FItems.Count) then Exit;
  Vis := GetVisibleCount;
  if Index < FTopIndex then SetTopIndex(Index)
  else if Index >= FTopIndex + Vis then SetTopIndex(Index - Vis + 1);
end;

procedure TCDListBox.FireSelectionChange;
begin
  if Assigned(FOnSelectionChange) then FOnSelectionChange(Self);
end;

function TCDListBox.GetSelected(AIndex: Integer): Boolean;
begin
  Result := False;
  if (AIndex < 0) or (AIndex >= FItems.Count) then Exit;
  if AIndex >= FSelected.Size then Exit;
  Result := FSelected[AIndex];
end;

procedure TCDListBox.SetSelected(AIndex: Integer; ASel: Boolean);
begin
  if (AIndex < 0) or (AIndex >= FItems.Count) then Exit;
  if FSelected.Size < FItems.Count then FSelected.Size := FItems.Count;
  FSelected[AIndex] := ASel;
  Invalidate;
  FireSelectionChange;
end;

procedure TCDListBox.SelectRange(ALow, AHigh: Integer; ASel: Boolean);
var
  i, lo, hi: Integer;
begin
  lo := ALow; hi := AHigh;
  if lo > hi then begin i := lo; lo := hi; hi := i; end;
  if lo < 0 then lo := 0;
  if hi >= FItems.Count then hi := FItems.Count - 1;
  if FSelected.Size < FItems.Count then FSelected.Size := FItems.Count;
  for i := lo to hi do FSelected[i] := ASel;
  Invalidate;
  FireSelectionChange;
end;

function TCDListBox.GetSelCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Min(FSelected.Size, FItems.Count) - 1 do
    if FSelected[i] then Inc(Result);
end;

function TCDListBox.ItemAtPos(X, Y: Integer): Integer;
var
  Row: Integer;
begin
  Result := -1;
  if Y < 2 then Exit;
  Row := (Y - 2) div ComputeItemHeight;
  Result := FTopIndex + Row;
  if (Result < 0) or (Result >= FItems.Count) then Result := -1;
end;

function TCDListBox.ItemRect(Index: Integer): TRect;
var
  H, T: Integer;
begin
  Result := Rect(0, 0, 0, 0);
  if (Index < FTopIndex) or (Index >= FTopIndex + GetVisibleCount + 1) then Exit;
  H := ComputeItemHeight;
  T := 2 + (Index - FTopIndex) * H;
  Result := Rect(2, T, Width - 2 -
    IfThen((FScrollBar <> nil) and FScrollBar.Visible, FScrollBar.Width, 0),
    T + H);
end;

procedure TCDListBox.UpdateSelectionForClick(Index: Integer; Shift: TShiftState);
begin
  if FMultiSelect and FExtendedSelect and (ssShift in Shift) and (FAnchor >= 0) then
  begin
    FSelected.Clearall;
    SelectRange(FAnchor, Index, True);
    FItemIndex := Index;
  end
  else if FMultiSelect and FExtendedSelect and (ssCtrl in Shift) then
  begin
    if FSelected.Size < FItems.Count then FSelected.Size := FItems.Count;
    FSelected[Index] := not FSelected[Index];
    FItemIndex := Index;
    FAnchor := Index;
  end
  else if FMultiSelect and not FExtendedSelect then
  begin
    if FSelected.Size < FItems.Count then FSelected.Size := FItems.Count;
    FSelected[Index] := not FSelected[Index];
    FItemIndex := Index;
    FAnchor := Index;
  end
  else
  begin
    FSelected.Clearall;
    if Index >= 0 then FSelected[Index] := True;
    FItemIndex := Index;
    FAnchor := Index;
  end;
  EnsureVisible(FItemIndex);
  Invalidate;
  FireSelectionChange;
end;

procedure TCDListBox.DoEnter;
begin
  inherited DoEnter;
  Invalidate;
end;

procedure TCDListBox.DoExit;
begin
  inherited DoExit;
  Invalidate;
end;

procedure TCDListBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Idx: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button <> mbLeft then Exit;
  SetFocus;
  Idx := ItemAtPos(X, Y);
  if Idx < 0 then Exit;
  UpdateSelectionForClick(Idx, Shift);
end;

procedure TCDListBox.KeyDown(var Key: Word; Shift: TShiftState);
var
  NewIndex, Vis: Integer;
  i: Integer;
begin
  inherited KeyDown(Key, Shift);
  if FItems.Count = 0 then Exit;
  Vis := GetVisibleCount;
  NewIndex := FItemIndex;
  case Key of
    VK_UP:    if NewIndex > 0                  then Dec(NewIndex) else NewIndex := 0;
    VK_DOWN:  if NewIndex < FItems.Count - 1   then Inc(NewIndex) else NewIndex := FItems.Count - 1;
    VK_PRIOR: NewIndex := Max(0, NewIndex - Vis);
    VK_NEXT:  NewIndex := Min(FItems.Count - 1, NewIndex + Vis);
    VK_HOME:  NewIndex := 0;
    VK_END:   NewIndex := FItems.Count - 1;
    VK_A:
      if (Shift = [ssCtrl]) and FMultiSelect then
      begin
        for i := 0 to FItems.Count - 1 do FSelected[i] := True;
        Invalidate; FireSelectionChange;
        Key := 0;
        Exit;
      end;
  else
    Exit;
  end;
  if NewIndex < 0 then Exit;
  Key := 0;
  if FMultiSelect and FExtendedSelect and (ssShift in Shift) then
  begin
    if FAnchor < 0 then FAnchor := FItemIndex;
    if FAnchor < 0 then FAnchor := NewIndex;
    FSelected.Clearall;
    SelectRange(FAnchor, NewIndex, True);
    FItemIndex := NewIndex;
    EnsureVisible(NewIndex);
    Invalidate;
    FireSelectionChange;
  end
  else
  begin
    SetItemIndex(NewIndex);   { also clears multi-select to single in non-multi mode }
    FAnchor := NewIndex;
  end;
end;

function TCDListBox.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
var
  Lines: Integer;
begin
  Result := True;
  Lines := WheelDelta div 40;
  if Lines = 0 then Lines := IfThen(WheelDelta > 0, 1, -1);
  SetTopIndex(FTopIndex - Lines);
end;

procedure TCDListBox.UTF8KeyPress(var UTF8Key: TUTF8Char);
begin
  inherited UTF8KeyPress(UTF8Key);
  { Filter control codes (Tab/Enter/Esc/etc.) -- they're handled by
    KeyDown. Printable chars feed type-ahead. Same range TCDEdit's
    UTF8KeyPress filter uses. }
  if (UTF8Key = '') or (UTF8Key[1] in [#0..#$1F, #$7F]) then Exit;
  TypeAheadAdvance(UTF8Key);
end;

procedure TCDListBox.TypeAheadAdvance(const AText: AnsiString);
const
  TimeoutSec = 1.0;
var
  i: Integer;
  Item, Prefix: AnsiString;
begin
  { Reset the prefix when typing pauses for >1s -- otherwise old
    characters from the previous search carry into a fresh one.
    DateTime arithmetic: 1 day = 1.0, so 1/86400 seconds. }
  if Now > FSearchExpiry then FSearchPrefix := '';
  FSearchPrefix := FSearchPrefix + AText;
  FSearchExpiry := Now + TimeoutSec / 86400.0;
  Prefix := UTF8LowerCase(FSearchPrefix);
  for i := 0 to FItems.Count - 1 do
  begin
    Item := UTF8LowerCase(FItems.Strings[i]);
    if Pos(Prefix, Item) = 1 then
    begin
      ItemIndex := i;     { setter handles selection + EnsureVisible }
      Exit;
    end;
  end;
end;

procedure TCDListBox.Resize;
begin
  inherited Resize;
  UpdateScrollBar;
end;

{ TCDCheckListBox }

constructor TCDCheckListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAllowGrayed   := False;
  FHeaderColor   := clInfoText;
  FHeaderBgColor := clInfoBk;
  SyncStateArrays;
end;

function TCDCheckListBox.GetControlId: TCDControlID;
begin
  Result := cidCheckListBox;
end;

procedure TCDCheckListBox.CreateControlStateEx;
begin
  FCLBState := TCDCheckListBoxStateEx.Create;
  FLBState  := FCLBState;
  FStateEx  := FCLBState;
end;

procedure TCDCheckListBox.PrepareControlStateEx;
begin
  inherited PrepareControlStateEx;
  FCLBState.States                := FStates;
  FCLBState.ItemEnabled           := FItemEnabled;
  FCLBState.Header                := FHeader;
  FCLBState.HeaderColor           := ColorToRGB(FHeaderColor);
  FCLBState.HeaderBackgroundColor := ColorToRGB(FHeaderBgColor);
  FCLBState.CheckWidth            := GetCheckWidth;
end;

procedure TCDCheckListBox.SyncStateArrays;
var
  OldLen, NewLen, i: Integer;
begin
  NewLen := Items.Count;
  OldLen := Length(FStates);
  if OldLen = NewLen then Exit;
  SetLength(FStates,      NewLen);
  SetLength(FItemEnabled, NewLen);
  SetLength(FHeader,      NewLen);
  for i := OldLen to NewLen - 1 do
  begin
    FStates[i]      := cbUnchecked;
    FItemEnabled[i] := True;
    FHeader[i]      := False;
  end;
end;

procedure TCDCheckListBox.ItemsChanged(Sender: TObject);
begin
  SyncStateArrays;
  inherited ItemsChanged(Sender);
end;

function TCDCheckListBox.GetCheckWidth: Integer;
begin
  if FDrawer <> nil then
    Result := FDrawer.GetMeasures(TCDCHECKBOX_SQUARE_HEIGHT) + 6
  else
    Result := 21;  { fallback before the drawer is bound }
end;

function TCDCheckListBox.HitsCheckColumn(X: Integer): Boolean;
begin
  { 4px frame margin + checkbox square. Generous left edge so the user
    can click anywhere in the small leading band, not just on the glyph. }
  Result := (X >= 2) and (X < 4 + GetCheckWidth - 2);
end;

procedure TCDCheckListBox.ToggleAtClick(AIndex: Integer);
const
  NextStateMap: array[TCheckBoxState] of array[Boolean] of TCheckBoxState =
  (
    {cbUnchecked} (cbChecked,   cbGrayed),
    {cbChecked  } (cbUnchecked, cbUnchecked),
    {cbGrayed   } (cbChecked,   cbChecked)
  );
begin
  if (AIndex < 0) or (AIndex >= Length(FStates)) then Exit;
  if (AIndex < Length(FHeader))      and FHeader[AIndex]            then Exit;
  if (AIndex < Length(FItemEnabled)) and (not FItemEnabled[AIndex]) then Exit;
  FStates[AIndex] := NextStateMap[FStates[AIndex]][FAllowGrayed];
  Invalidate;
  if Assigned(FOnClickCheck) then FOnClickCheck(Self, AIndex);
end;

procedure TCDCheckListBox.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Idx: Integer;
begin
  Idx := ItemAtPos(X, Y);
  if (Button = mbLeft) and (Idx >= 0) and HitsCheckColumn(X)
     and ((Idx >= Length(FHeader)) or not FHeader[Idx]) then
  begin
    SetFocus;
    ToggleAtClick(Idx);
    Exit;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCDCheckListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_SPACE) and (Shift = []) and (ItemIndex >= 0) then
  begin
    ToggleAtClick(ItemIndex);
    Key := 0;
    Exit;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TCDCheckListBox.Toggle(AIndex: Integer);
begin
  ToggleAtClick(AIndex);
end;

function TCDCheckListBox.GetItemState(AIndex: Integer): TCheckBoxState;
begin
  if (AIndex >= 0) and (AIndex < Length(FStates)) then
    Result := FStates[AIndex]
  else
    Result := cbUnchecked;
end;

procedure TCDCheckListBox.SetItemState(AIndex: Integer; AValue: TCheckBoxState);
begin
  if (AIndex < 0) or (AIndex >= Length(FStates)) then Exit;
  if FStates[AIndex] = AValue then Exit;
  FStates[AIndex] := AValue;
  Invalidate;
end;

function TCDCheckListBox.GetItemEnabled(AIndex: Integer): Boolean;
begin
  if (AIndex >= 0) and (AIndex < Length(FItemEnabled)) then
    Result := FItemEnabled[AIndex]
  else
    Result := True;
end;

procedure TCDCheckListBox.SetItemEnabled(AIndex: Integer; AValue: Boolean);
begin
  if (AIndex < 0) or (AIndex >= Length(FItemEnabled)) then Exit;
  if FItemEnabled[AIndex] = AValue then Exit;
  FItemEnabled[AIndex] := AValue;
  Invalidate;
end;

function TCDCheckListBox.GetItemHeader(AIndex: Integer): Boolean;
begin
  if (AIndex >= 0) and (AIndex < Length(FHeader)) then
    Result := FHeader[AIndex]
  else
    Result := False;
end;

procedure TCDCheckListBox.SetItemHeader(AIndex: Integer; AValue: Boolean);
begin
  if (AIndex < 0) or (AIndex >= Length(FHeader)) then Exit;
  if FHeader[AIndex] = AValue then Exit;
  FHeader[AIndex] := AValue;
  Invalidate;
end;

function TCDCheckListBox.GetItemChecked(AIndex: Integer): Boolean;
begin
  Result := GetItemState(AIndex) <> cbUnchecked;
end;

procedure TCDCheckListBox.SetItemChecked(AIndex: Integer; AValue: Boolean);
begin
  if AValue then SetItemState(AIndex, cbChecked)
  else           SetItemState(AIndex, cbUnchecked);
end;

{ TCDListView }

function TCDListView.GetProperty(AIndex: Integer): Boolean;
begin
  Result := False;
end;

procedure TCDListView.SetColumns(AValue: TListColumns);
begin
  if FColumns=AValue then Exit;
  FColumns:=AValue;
  if not (csLoading in ComponentState) then Invalidate;
end;

procedure TCDListView.SetProperty(AIndex: Integer; AValue: Boolean);
begin

end;

procedure TCDListView.SetShowColumnHeader(AValue: Boolean);
begin
  if FShowColumnHeader=AValue then Exit;
  FShowColumnHeader:=AValue;
  if not (csLoading in ComponentState) then Invalidate;
end;

procedure TCDListView.SetViewStyle(AValue: TViewStyle);
begin
  if FViewStyle=AValue then Exit;
  FViewStyle:=AValue;
  if not (csLoading in ComponentState) then Invalidate;
end;

function TCDListView.GetControlId: TCDControlID;
begin
  Result := cidListView;
end;

procedure TCDListView.CreateControlStateEx;
begin
  FLVState := TCDListViewStateEx.Create;
  FStateEx := FLVState;
end;

procedure TCDListView.PrepareControlStateEx;
begin
  inherited PrepareControlStateEx;
  FLVState.Items := FListItems;
  FLVState.Columns := FColumns;
  FLVState.ViewStyle := FViewStyle;
  FLVState.ShowColumnHeader := FShowColumnHeader;
end;

constructor TCDListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 250;
  Height := 150;
  FColumns := TListColumns.Create(nil);
  FListItems := TCDListItems.Create();
  TabStop := True;
  FShowColumnHeader := True;
//  FProperties: TListViewProperties;
//  FViewStyle: TViewStyle;

  ScrollBars := ssBoth;
end;

destructor TCDListView.Destroy;
begin
  FColumns.Free;
  FListItems.Free;
  inherited Destroy;
end;


{ TCDToolBar }

procedure TCDToolbar.SetFlat(AValue: Boolean);
begin
  if FFlat = AValue then Exit;
  FFlat := AValue;
  if not (csLoading in ComponentState) then Invalidate;
end;

procedure TCDToolBar.SetShowCaptions(AValue: Boolean);
begin
  if FShowCaptions = AValue then Exit;
  FShowCaptions := AValue;
  if not (csLoading in ComponentState) then Invalidate;
end;

function TCDToolBar.GetControlId: TCDControlID;
begin
  Result := cidToolBar;
end;

procedure TCDToolBar.CreateControlStateEx;
var
  i: Integer;
begin
  FTBState := TCDToolBarStateEx.Create;
  FStateEx := FTBState;
end;

procedure TCDToolBar.PrepareControlStateEx;
var
  i, lX: Integer;
  lCursorPos: TPoint;
  lCurItem: TCDToolBarItem;
begin
  inherited PrepareControlStateEx;
  FTBState.Flat := FFlat;
  FTBState.ShowCaptions := FShowCaptions;
  FTBState.Items := FItems;
  FTBState.ToolBarHeight := Height;

  // Handle mouse over items
  lCursorPos := Mouse.CursorPos;
  lCursorPos := ScreenToClient(lCursorPos);
  lX := 0;
  for i := 0 to GetItemCount()-1 do
  begin
    lCurItem := GetItem(i);
    lCurItem.State := lCurItem.State - [csfMouseOver];
    if IsPosInButton(lCursorPos, lCurItem, lX) then
      lCurItem.State := lCurItem.State + [csfMouseOver];
    if lCurItem.Down then
      lCurItem.State := lCurItem.State + [csfSunken];
    lX := lX + lCurItem.Width;
  end;
end;

procedure TCDToolBar.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  inherited MouseMove(Shift, X, Y);
  Invalidate;
end;

procedure TCDToolBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  lCurItem: TCDToolBarItem;
  lOnArrow: Boolean;
begin
  inherited MouseDown(Button, Shift, X, Y);
  lCurItem := GetItemWithMousePos(Point(X, Y), lOnArrow);
  if (lCurItem = nil) or not lCurItem.Enabled then Exit;
  if lCurItem.Kind in [tikButton, tikCheckButton, tikDropDownButton] then
  begin
    lCurItem.State := lCurItem.State + [csfSunken];
    if (lCurItem.Kind = tikDropDownButton) and lOnArrow then
      lCurItem.State := lCurItem.State + [csfSunkenArrow] - [csfSunken];
    Invalidate();
  end;
end;

procedure TCDToolBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  i: Integer;
  lCurItem, item: TCDToolBarItem;
  DoInvalidate: Boolean = False;
  ArrowClicked: Boolean = False;
begin
  inherited MouseUp(Button, Shift, X, Y);
  lCurItem := GetItemWithMousePos(Point(X, Y));
  if (lCurItem = nil) or (not lCurItem.Enabled) then Exit;

  // click the selected checkbutton if applicable
  if lCurItem.Kind in [tikCheckButton] then
  begin
    lCurItem.Down := not lCurItem.Down;
    DoInvalidate := True;
  end else
  if lCurItem.Kind = tikDropDownButton then
    ArrowClicked := csfSunkenArrow in lCurItem.State;

  // up all buttons
  for i := 0 to GetItemCount()-1 do
  begin
    item := GetItem(i);
    if item.Kind in [tikButton, tikCheckButton, tikDropDownButton] then
    begin
      item.State := item.State - [csfSunken, csfSunkenArrow];
      DoInvalidate := True;
    end;
  end;

  if DoInvalidate then Invalidate;

  if Assigned(lCurItem.OnClick) and (lCurItem.Kind in [tikButton, tikCheckButton, tikDropDownButton]) then
    lCurItem.OnClick(lCurItem, ArrowClicked);
end;

procedure TCDToolBar.MouseLeave;
begin
  inherited MouseLeave;
  Invalidate;
end;

constructor TCDToolBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Height := GetDrawer(dsDefault).GetMeasures(TCDTOOLBAR_DEFAULT_HEIGHT);
  Align := alTop;
  FItems := TFPList.Create();
  TabStop := False;
  FFlat := True;
end;

destructor TCDToolBar.Destroy;
begin
  while FItems.Count > 0 do
    DeleteItem(0);
  FItems.Free;
  inherited Destroy;
end;

function TCDToolBar.InsertItem(AKind: TCDToolbarItemKind; AIndex: Integer): TCDToolBarItem;
var
  lNewItem: TCDToolBarItem;
begin
  lNewItem := TCDToolBarItem.Create;
  lNewItem.Kind := AKind;
  lNewItem.Enabled := true;
  FItems.Insert(AIndex, lNewItem);
  Result := lNewItem;
  PrepareCurrentDrawer();
  case AKind of
    tikButton, tikCheckButton:
      Result.Width := FDrawer.GetMeasures(TCDTOOLBAR_ITEM_BUTTON_DEFAULT_WIDTH);
    tikDropDownButton:
      Result.Width := FDrawer.GetMeasures(TCDTOOLBAR_ITEM_BUTTON_DEFAULT_WIDTH)
        + FDrawer.GetMeasures(TCDTOOLBAR_ITEM_ARROW_RESERVED_WIDTH);
    tikSeparator, tikDivider:
      Result.Width := FDrawer.GetMeasures(TCDTOOLBAR_ITEM_SEPARATOR_DEFAULT_WIDTH);
  end;
end;

function TCDToolBar.AddItem(AKind: TCDToolbarItemKind): TCDToolBarItem;
begin
  Result := InsertItem(AKind, FItems.Count);
end;

procedure TCDToolBar.DeleteItem(AIndex: Integer);
var
  item: TCDToolbarItem;
begin
  if (AIndex < 0) or (AIndex >= FItems.Count) then Exit;
  item := TCDToolbarItem(FItems[AIndex]);
  item.Free;
  FItems.Delete(AIndex);
end;

function TCDToolBar.GetItem(AIndex: Integer): TCDToolBarItem;
begin
  Result := nil;
  if (AIndex < 0) or (AIndex >= FItems.Count) then Exit;
  Result := TCDToolBarItem(FItems.Items[AIndex]);
end;

function TCDToolBar.GetItemCount: Integer;
begin
  Result := FItems.Count;
end;

function TCDToolBar.GetItemWithMousePos(APosInControl: TPoint): TCDToolBarItem;
var
  onArrow: Boolean;
begin
  Result := GetItemWithMousePos(APosInControl, onArrow);
end;

function TCDToolbar.GetItemWithMousePos(APosInControl: TPoint;
  out OnArrow: Boolean): TCDToolBarItem;
var
  i, lX: Integer;
  lCurItem: TCDToolBarItem;
  lArrowWidth: Integer = -1;
begin
  Result := nil;
  OnArrow := false;
  lX := 0;
  for i := 0 to FItems.Count-1 do
  begin
    lCurItem := GetItem(i);
    if IsPosInButton(APosInControl, lCurItem, lX) then
    begin
      if (lCurItem.Kind = tikDropDownButton) then
      begin
        if lArrowWidth = -1 then
          lArrowWidth := FDrawer.GetMeasures(TCDTOOLBAR_ITEM_ARROW_RESERVED_WIDTH);
        if (APosInControl.X > lX + lCurItem.Width - lArrowWidth) then
          OnArrow := true;
      end;
      Exit(lCurItem);
    end;
    lX := lX + lCurItem.Width;
  end;
end;


function TCDToolBar.IsPosInButton(APosInControl: TPoint; AItem: TCDToolBarItem;
  AItemX: Integer): Boolean;
var
  lSize: TSize;
begin
  lSize.CY := Height;
  lSize.CX := AItem.Width;
  Result := (APosInControl.X > AItemX) and (APosInControl.X < AItemX + lSize.CX) and
    (APosInControl.Y > 0) and (APosInControl.Y < lSize.CY);
end;

{ TCDStatusBar }

constructor TCDStatusBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 240;
  Height := 19;
  TabStop := False;
  Align := alBottom;
  AutoSize := True;
  FSimplePanel := True;
  FSizeGrip := True;
end;

function TCDStatusBar.GetControlId: TCDControlID;
begin
  Result := cidStatusBar;
end;

class function TCDStatusBar.GetControlClassDefaultSize: TSize;
begin
  { Return (1, 1) so when this injected control is alClient inside the
    LCL TStatusBar, the parent's TWinControl.CalculatePreferredSize
    Max(WS_GetPreferredSize, child_GetDefaultHeight) takes the WS
    value -- the font-derived height -- not the inherited TControl
    default of (75, 50) which would inflate the bar. The injected
    control's actual size always comes from alClient, never this. }
  Result.cx := 1;
  Result.cy := 1;
end;

procedure TCDStatusBar.CreateControlStateEx;
begin
  FSBState := TCDStatusBarStateEx.Create;
  FStateEx := FSBState;
end;

procedure TCDStatusBar.PrepareControlStateEx;
begin
  inherited PrepareControlStateEx;
  FSBState.Panels      := FPanelsRef;
  FSBState.SimpleText  := FSimpleText;
  FSBState.SimplePanel := FSimplePanel;
  FSBState.SizeGrip    := FSizeGrip;
end;

procedure TCDStatusBar.SetSimpleText(const AValue: TCaption);
begin
  if FSimpleText = AValue then Exit;
  FSimpleText := AValue;
  Invalidate;
end;

procedure TCDStatusBar.SetSimplePanel(AValue: Boolean);
begin
  if FSimplePanel = AValue then Exit;
  FSimplePanel := AValue;
  Invalidate;
end;

{ TCDTabSheet }

procedure TCDTabSheet.RealSetText(const Value: TCaption);
var
  lIndex: Integer;
begin
  inherited RealSetText(Value);
  lIndex := CDTabControl.Tabs.IndexOfObject(Self);
  if lIndex >= 0 then
    CDTabControl.Tabs.Strings[lIndex] := Value;
  CDTabControl.Invalidate;
end;

procedure TCDTabSheet.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);
  // Code adding tabs added via the object inspector
  if (csLoading in ComponentState) and (NewParent is TCDPageControl) then
  begin
    CDTabControl := NewParent as TCDCustomTabControl;
    TCDPageControl(CDTabControl).AddPage(Self);
  end;
end;

constructor TCDTabSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  TabStop := False;
  ParentColor := True;
  parentFont := True;
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csDesignFixedBounds, csDoubleClicks, csDesignInteractive];
  //ControlStyle := ControlStyle + [csAcceptsControls, csDesignFixedBounds,
  //  csNoDesignVisible, csNoFocus];
end;

destructor TCDTabSheet.Destroy;
var
  lIndex: Integer;
begin
  // We should support deleting the tabsheet directly too,
  // and then it should update the tabcontrol
  // This is important mostly for the designer
  if CDTabControl <> nil then
  begin
    lIndex := CDTabControl.FTabs.IndexOfObject(Self);
    if lIndex >= 0 then
    begin
      CDTabControl.FTabs.Delete(lIndex);
      CDTabControl.CorrectTabIndex();
    end;
  end;

  inherited Destroy;
end;

procedure TCDTabSheet.EraseBackground(DC: HDC);
begin

end;

procedure TCDTabSheet.Paint;
var
  lSize: TSize;
begin
  if CDTabControl <> nil then
  begin
    lSize := Size(Width, Height);
    CDTabControl.FDrawer.DrawTabSheet(Canvas, Point(0, 0), lSize, CDTabControl.FState,
      CDTabControl.FTabCState);
  end;
end;

{ TCDCustomTabControl }

procedure TCDCustomTabControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  lTabIndex: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);

  lTabIndex := MousePosToTabIndex(X, Y);

  if lTabIndex >=0 then
  begin
    if Self is TCDPageControl then
      (Self as TCDPageControl).PageIndex := lTabIndex
    else
      TabIndex := lTabIndex;
  end;
end;

procedure TCDCustomTabControl.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
var
  lTabIndex, lCloseButtonSize: Integer;
  lNewPage: TCDTabSheet;
  lCloseButtonPos: TPoint;
begin
  inherited MouseUp(Button, Shift, X, Y);

  lTabIndex := MousePosToTabIndex(X, Y);

  // Check if the add button was clicked
  if (nboShowAddTabButton in Options) and (lTabIndex = Tabs.Count) then
  begin
    if Self is TCDPageControl then
    begin
      lNewPage := (Self as TCDPageControl).AddPage('New Page');
      if Assigned(OnUserAddedPage) then OnUserAddedPage(Self, lNewPage);
    end
    else
    begin
      Tabs.Add('New Tab');
      if Assigned(OnUserAddedPage) then OnUserAddedPage(Self, nil);
    end;
  end
  // Check if a close button was clicked
  else if (nboShowCloseButtons in Options) and (lTabIndex >= 0) then
  begin
    FTabCState.CurTabIndex := lTabIndex;
    lCloseButtonPos.X := FDrawer.GetMeasuresEx(Canvas, TCDCTABCONTROL_CLOSE_BUTTON_POS_X, FState, FStateEx);
    lCloseButtonPos.Y := FDrawer.GetMeasuresEx(Canvas, TCDCTABCONTROL_CLOSE_BUTTON_POS_Y, FState, FStateEx);
    lCloseButtonSize := FDrawer.GetMeasures(TCDCTABCONTROL_CLOSE_TAB_BUTTON_WIDTH);
    if (X >= lCloseButtonPos.X) and (X <= lCloseButtonPos.X + lCloseButtonSize) and
       (Y >= lCloseButtonPos.Y) and (Y <= lCloseButtonPos.Y + lCloseButtonSize) then
    begin
      if Self is TCDPageControl then (Self as TCDPageControl).RemovePage(lTabIndex)
      else Tabs.Delete(lTabIndex);
    end;
  end;
end;

procedure TCDCustomTabControl.SetOptions(AValue: TCTabControlOptions);
begin
  if FOptions=AValue then Exit;
  FOptions:=AValue;
  Invalidate;
end;

procedure TCDCustomTabControl.SetTabIndex(AValue: Integer);
begin
  if FTabIndex = AValue then Exit;
  if Assigned(OnChanging) then OnChanging(Self);
  FTabIndex := AValue;
  if Assigned(OnChange) then OnChange(Self);
  Invalidate;
end;

procedure TCDCustomTabControl.SetTabs(AValue: TStringList);
begin
  if FTabs=AValue then Exit;
  FTabs.Assign(AValue);
  CorrectTabIndex();
  Invalidate;
end;

function TCDCustomTabControl.MousePosToTabIndex(X, Y: Integer): Integer;
var
  i: Integer;
  CurStartLeftPos: Integer = 0;
  CurStartTopPos: Integer = 0;
  VisiblePagesStarted: Boolean = False;
  lLastTab, lTabWidth, lTabHeight: Integer;
begin
  Result := -1;

  if nboShowAddTabButton in Options then lLastTab := Tabs.Count
  else lLastTab := Tabs.Count - 1;

  for i := 0 to lLastTab do
  begin
    if (i = FTabCState.LeftmostTabVisibleIndex) or (nboMultiLine in Options) then
      VisiblePagesStarted := True;

    if VisiblePagesStarted then
    begin
      FTabCState.CurTabIndex := i;
      lTabWidth := FDrawer.GetMeasuresEx(Canvas, TCDCTABCONTROL_TAB_WIDTH, FState, FTabCState);
      lTabHeight := FDrawer.GetMeasuresEx(Canvas, TCDCTABCONTROL_TAB_HEIGHT, FState, FTabCState);
      if (nboMultiLine in Options) and (lTabWidth+CurStartLeftPos > Width) then
      begin
        Inc(CurStartTopPos, lTabHeight);
        CurStartLeftPos := 0;
      end;

      if (X > CurStartLeftPos) and
        (X < CurStartLeftPos + lTabWidth) and
        (Y < lTabHeight+CurStartTopPos) and
        (Y >= CurStartTopPos)
        then
      begin
        Exit(i);
      end;
      CurStartLeftPos := CurStartLeftPos + lTabWidth;
    end;
  end;
end;

function TCDCustomTabControl.GetControlId: TCDControlID;
begin
  Result := cidCTabControl;
end;

procedure TCDCustomTabControl.CreateControlStateEx;
begin
  FTabCState := TCDCTabControlStateEx.Create;
  FStateEx := FTabCState;
end;

procedure TCDCustomTabControl.PrepareControlStateEx;
begin
  inherited PrepareControlStateEx;

  FTabCState.Tabs := Tabs;
  FTabCState.TabIndex := TabIndex;
  FTabCState.TabCount := GetTabCount();
  FTabCState.Options := FOptions;
end;

constructor TCDCustomTabControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width := 232;
  Height := 184;
  TabStop := True;

  ParentColor := True;
  ParentFont := True;
  ControlStyle := ControlStyle + [csAcceptsControls, csDesignInteractive];

  // FTabs should hold only visible tabs
  FTabs := TStringList.Create;
end;

destructor TCDCustomTabControl.Destroy;
begin
  FTabs.Free;

  inherited Destroy;
end;

function TCDCustomTabControl.GetTabCount: Integer;
begin
  Result := 0;
  if FTabs <> nil then Result := FTabs.Count;
end;

procedure TCDCustomTabControl.CorrectTabIndex;
begin
  if FTabIndex >= FTabs.Count then SetTabIndex(FTabs.Count - 1);
end;

{ TCDPageControl }

function TCDPageControl.AddPage(S: string): TCDTabSheet;
//  InsertPage(FPages.Count, S);
var
  NewPage: TCDTabSheet;
begin
  NewPage := TCDTabSheet.Create(Owner);
  NewPage.Parent := Self;
  NewPage.CDTabControl := Self;
  NewPage.Caption := S;

  PositionTabSheet(NewPage);

  FTabs.AddObject(S, NewPage);

  SetActivePage(NewPage);

  Result := NewPage;
end;

procedure TCDPageControl.AddPage(APage: TCDTabSheet);
begin
  APage.CDTabControl := Self;
  PositionTabSheet(APage);
  FTabs.AddObject(APage.Caption, APage);
  SetActivePage(APage);
end;

function TCDPageControl.GetPage(AIndex: integer): TCDTabSheet;
begin
  if (AIndex >= 0) and (AIndex < FTabs.Count) then
    Result := TCDTabSheet(FTabs.Objects[AIndex])
  else
    Result := nil;
end;

function TCDPageControl.InsertPage(aIndex: integer; S: string): TCDTabSheet;
var
  NewPage: TCDTabSheet;
begin
  NewPage := TCDTabSheet.Create(Owner);
  NewPage.Parent := Self;
  NewPage.CDTabControl := Self;
  NewPage.Caption := S;

  PositionTabSheet(NewPage);

  FTabs.InsertObject(AIndex, S, NewPage);

  SetActivePage(NewPage);
  Result := NewPage;
end;

procedure TCDPageControl.RemovePage(aIndex: integer);
begin
  if (AIndex < 0) or (AIndex >= FTabs.Count) then Exit;

  Application.ReleaseComponent(TComponent(FTabs.Objects[AIndex]));

  FTabs.Delete(aIndex);
  if FTabIndex >= FTabs.Count then SetPageIndex(FTabIndex-1);

  Invalidate;
end;

function TCDPageControl.FindNextPage(CurPage: TCDTabSheet;
  GoForward, CheckTabVisible: boolean): TCDTabSheet;
var
  I, TempStartIndex: integer;
begin
  if FTabs.Count <> 0 then
  begin
    //StartIndex := FPages.IndexOfObject(CurPage);
    TempStartIndex := FTabs.IndexOfObject(CurPage);
    if TempStartIndex = -1 then
      if GoForward then
        TempStartIndex := FTabs.Count - 1
      else
        TempStartIndex := 0;
    I := TempStartIndex;
    repeat
      if GoForward then
      begin
        Inc(I);
        if I = FTabs.Count then
          I := 0;
      end
      else
      begin
        if I = 0 then
          I := FTabs.Count;
        Dec(I);
      end;
      Result := TCDTabSheet(FTabs.Objects[I]);
      if not CheckTabVisible or Result.Visible then
        Exit;
    until I = TempStartIndex;
  end;
  Result := nil;
end;

procedure TCDPageControl.SelectNextPage(GoForward: boolean;
  CheckTabVisible: boolean = True);
var
  Page: TCDTabSheet;
begin
  Page := FindNextPage(ActivePage, GoForward, CheckTabVisible);
  if (Page <> nil) and (Page <> ActivePage) then
    SetActivePage(Page);
end;

constructor TCDPageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle - [csAcceptsControls];
end;

destructor TCDPageControl.Destroy;
begin
  inherited Destroy;
end;

procedure TCDPageControl.SetActivePage(Value: TCDTabSheet);
var
  i: integer;
  CurPage: TCDTabSheet;
begin
  for i := 0 to FTabs.Count - 1 do
  begin
    CurPage := TCDTabSheet(FTabs.Objects[i]);
    if CurPage = Value then
    begin
      PositionTabSheet(CurPage);
      CurPage.BringToFront;
      CurPage.Visible := True;

      // Check first, Tab is Visible?
      SetTabIndex(i);
    end
    else if CurPage <> nil then
    begin
      //CurPage.Align := alNone;
      //CurPage.Height := 0;
      CurPage.Visible := False;
    end;
  end;

  Invalidate;
end;

procedure TCDPageControl.SetPageIndex(Value: integer);
begin
  if (Value > -1) and (Value < FTabs.Count) then
  begin
    SetTabIndex(Value);
    ActivePage := GetPage(Value);
  end;
end;

procedure TCDPageControl.UpdateAllDesignerFlags;
var
  i: integer;
begin
  for i := 0 to FTabs.Count - 1 do
    UpdateDesignerFlags(i);
end;

procedure TCDPageControl.UpdateDesignerFlags(APageIndex: integer);
var
  CurPage: TCDTabSheet;
begin
  CurPage := GetPage(APageIndex);
  if APageIndex <> fTabIndex then
    CurPage.ControlStyle := CurPage.ControlStyle + [csNoDesignVisible]
  else
    CurPage.ControlStyle := CurPage.ControlStyle - [csNoDesignVisible];
end;

procedure TCDPageControl.PositionTabSheet(ATabSheet: TCDTabSheet);
var
  lIndex: Integer;
  lClientArea: TRect;
begin
  lIndex := FTabs.IndexOfObject(ATabSheet);
  FTabCState.TabIndex := lIndex;
  PrepareControlState;
  PrepareControlStateEx;
  lClientArea := FDrawer.GetClientArea(Canvas, Size(Width, Height), GetControlId, FState, FStateEx);

  ATabSheet.BorderSpacing.Top := lClientArea.Top;
  ATabSheet.BorderSpacing.Left := lClientArea.Left;
  ATabSheet.BorderSpacing.Right := Width - lClientArea.Right;
  ATabSheet.BorderSpacing.Bottom := Height - lClientArea.Bottom;
  ATabSheet.Align := alClient;
end;

function TCDPageControl.GetTabContentRect(ASize: TSize): TRect;
begin
  PrepareControlState;
  PrepareControlStateEx;
  Result := FDrawer.GetClientArea(Canvas, ASize, GetControlId,
                                  FState, FStateEx);
end;

function TCDPageControl.GetActivePage: TCDTabSheet;
begin
  Result := GetPage(FTabIndex);
end;

function TCDPageControl.GetPageCount: integer;
begin
  Result := FTabs.Count;
end;

function TCDPageControl.GetPageIndex: integer;
begin
  Result := FTabIndex;
end;

{ TCDSpinEdit }

procedure TCDSpinEdit.UpDownChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
begin
  { Use the NewValue passed in, NOT FUpDown.Position. TCustomUpDown.Click
    fires OnChanging/OnChangingEx via CanChange BEFORE assigning Position
    := FCanChangePos, so FUpDown.Position is still the OLD value at this
    point. Reading it would lag by one click on every click. }
  Value := NewValue / Power(10, FDecimalPlaces);
  { SetValue updates the injected FValue and Text but does not fire
    DoChange, so the host LCL TCustomFloatSpinEdit never receives the
    CM_TEXTCHANGED that TCDIntfSpinEdit.DoChange dispatches. }
  DoChange;
end;

procedure TCDSpinEdit.SetIncrement(AValue: Double);
begin
  if FIncrement=AValue then Exit;
  FIncrement:=AValue;
  DoUpdateUpDown;
end;

procedure TCDSpinEdit.SetDecimalPlaces(AValue: Byte);
begin
  if FDecimalPlaces=AValue then Exit;
  FDecimalPlaces:=AValue;
  DoUpdateUpDown;
  DoUpdateText;
end;

procedure TCDSpinEdit.SetMaxValue(AValue: Double);
begin
  if FMaxValue=AValue then Exit;
  FMaxValue:=AValue;
  if FValue > FMaxValue then Value := FMaxValue;
  DoUpdateUpDown;
end;

procedure TCDSpinEdit.SetMinValue(AValue: Double);
begin
  if FMinValue=AValue then Exit;
  FMinValue:=AValue;
  if FValue < FMinValue then Value := FMinValue;
  DoUpdateUpDown;
end;

procedure TCDSpinEdit.SetValue(AValue: Double);
begin
  if FValue=AValue then Exit;
  if FValue < FMinValue then Exit;
  if FValue > FMaxValue then Exit;
  FValue:=AValue;
  DoUpdateText;
  DoUpdateUpDown;
end;

procedure TCDSpinEdit.DoUpdateText;
begin
  if FDecimalPlaces > 0 then Text := FloatToStr(FValue)
  else Text := IntToStr(Round(FValue));
  Invalidate;
end;

procedure TCDSpinEdit.DoUpdateUpDown;
begin
  FUpDown.Min := Round(FMinValue * Power(10, FDecimalPlaces));
  FUpDown.Max := Round(FMaxValue * Power(10, FDecimalPlaces));
  FUpDown.Position := Round(FValue * Power(10, FDecimalPlaces));
end;

procedure TCDSpinEdit.DoChange;
var
  lValue: Double;
begin
  { Read Text, not Caption. TCDEdit declares Text as a shadowing
    property whose getter returns Lines[0] (the actual editor content),
    but its GetText function is private+non-override, so it does not
    replace TControl.GetText. Caption (= TControl.GetText -> RealGetText
    -> FCaption) returns the stale FCaption that was last assigned via
    the LCL Caption setter, NOT what the user typed -- so TryStrToFloat
    parses '' and FValue stays at 0. }
  if SysUtils.TryStrToFloat(Text, lValue) then FValue := lValue;
  DoUpdateUpDown;
  inherited DoChange;
end;

constructor TCDSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  { TCDEdit.Create strips csAcceptsControls (TEdit-style controls don't
    typically host children). TCDSpinEdit DOES host a TUpDown child, so
    re-enable it; without this, AlignControls is suppressed when our
    bounds change and the alRight TUpDown sticks at its initial offset
    (e.g. x=63 from the 80x25 default size) instead of tracking the
    right edge after the LCL spin's actual bounds (e.g. 100x26) cascade
    in via alClient. }
  ControlStyle := ControlStyle + [csAcceptsControls];

  FUpDown := TUpDown.Create(Self);
  FUpDown.Align := alRight;
  FUpDown.Parent := Self;
  FUpDown.OnChangingEx := @UpDownChangingEx;

  FMinValue := 0;
  FMaxValue := 100;
  FIncrement := 1;

  DoUpdateText();
end;

destructor TCDSpinEdit.Destroy;
begin
  inherited Destroy;
end;

end.
