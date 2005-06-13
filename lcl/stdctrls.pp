{
 /***************************************************************************
                               stdctrls.pp
                               -----------

                   Initial Revision : Tue Oct 19 CST 1999

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

unit StdCtrls;

{$mode objfpc}{$H+}
{off $Define NewCheckBox}

interface


uses
  Classes, SysUtils, LCLStrConsts, LCLType, LCLProc, LMessages, Graphics,
  GraphType, GraphMath, ExtendedStrings, LCLIntf, ClipBrd, ActnList, Controls,
  Forms;

type

  { TScrollBar }

  TScrollStyle = (ssNone, ssHorizontal, ssVertical, ssBoth,
    ssAutoHorizontal, ssAutoVertical, ssAutoBoth);

  TScrollCode = (
    // !!! Beware. The position of these enums must correspond to the SB_xxx
    // values in LCLType  (Delphi compatibility, not our decision)
    scLineUp,   // = SB_LINEUP
    scLineDown, // = SB_LINEDOWN
    scPageUp,   // = SB_PAGEUP
    scPageDown, // = SB_PAGEDOWN
    scPosition, // = SB_THUMBPOSITION
    scTrack,    // = SB_THUMBTRACK
    scTop,      // = SB_TOP
    scBottom,   // = SB_BOTTOM
    scEndScroll // = SB_ENDSCROLL
    );

  TScrollEvent = procedure(Sender: TObject; ScrollCode: TScrollCode;
                           var ScrollPos: Integer) of object;

  TCustomScrollBar = class(TWinControl)
  private
    FKind: TScrollBarKind;
    FPosition: Integer;
    FMin: Integer;
    FMax: Integer;
    FPageSize: Integer;
    FRTLFactor: Integer;
    FSmallChange: TScrollBarInc;
    FLargeChange: TScrollBarInc;
    FOnChange: TNotifyEvent;
    FOnScroll: TScrollEvent;
    procedure DoScroll(var Message: TLMScroll);
    function NotRightToLeft: Boolean;
    procedure SetKind(Value: TScrollBarKind);
    procedure SetMax(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure SetPosition(Value: Integer);
    procedure SetPageSize(Value: Integer);
    procedure CNHScroll(var Message: TLMHScroll); message LM_HSCROLL;
    procedure CNVScroll(var Message: TLMVScroll); message LM_VSCROLL;
    procedure CNCtlColorScrollBar(var Message: TLMessage); message CN_CTLCOLORSCROLLBAR;
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Change; dynamic;
    procedure Scroll(ScrollCode: TScrollCode; var ScrollPos: Integer); dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetParams(APosition, AMin, AMax: Integer);
  public
    property Kind: TScrollBarKind read FKind write SetKind default sbHorizontal;
    property LargeChange: TScrollBarInc read FLargeChange write FLargeChange default 1;
    property Max: Integer read FMax write SetMax default 100;
    property Min: Integer read FMin write SetMin default 0;
    property PageSize: Integer read FPageSize write SetPageSize;
    property Position: Integer read FPosition write SetPosition default 0;
    property SmallChange: TScrollBarInc read FSmallChange write FSmallChange default 1;
    property TabStop default true;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnScroll: TScrollEvent read FOnScroll write FOnScroll;
  end;
  
  
  { TScrollBar }
  
  TScrollBar = class(TCustomScrollBar)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Kind;
    property LargeChange;
    property Max;
    property Min;
    property PageSize;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property Position;
    property ShowHint;
    property SmallChange;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnScroll;
    property OnStartDrag;
  end;


  { TCustomGroupBox }

  TCustomGroupBox = class (TWinControl) {class(TCustomControl) }
  protected
  public
    constructor Create(AOwner: TComponent); Override;
    function CanTab: boolean; override;
  end;


  { TGroupBox }

  TGroupBox = class(TCustomGroupBox)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Caption;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property Ctl3D;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDockDrop;
    property OnDockOver;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;


  { TCustomComboBox }

  TComboBoxStyle = (csDropDown, csSimple, csDropDownList, csOwnerDrawFixed,
                    csOwnerDrawVariable);

  TOwnerDrawState = TBaseOwnerDrawState;

  TDrawItemEvent = procedure(Control: TWinControl; Index: Integer;
    ARect: TRect; State: TOwnerDrawState) of object;
  TMeasureItemEvent = procedure(Control: TWinControl; Index: Integer;
    var Height: Integer) of object;

  { TCustomComboBox }

  TCustomComboBox = class(TWinControl)
  private
    FAutoDropDown: Boolean;
    FCanvas: TCanvas;
    FDropDownCount: Integer;
    FDroppedDown: boolean;
    FItemHeight: integer;
    FItemIndex: integer;
    FItemWidth: integer;
    FItems: TStrings;
    fMaxLength: integer;
    FOnChange: TNotifyEvent;
    FOnCloseUp: TNotifyEvent;
    FOnDrawItem: TDrawItemEvent;
    FOnDropDown: TNotifyEvent;
    FOnMeasureItem: TMeasureItemEvent;
    FOnSelect: TNotifyEvent;
    FSelLength: integer;
    FSelStart: integer;
    FSorted: boolean;
    FStyle: TComboBoxStyle;
    FArrowKeysTraverseList: Boolean;
    FReturnArrowState: Boolean; //used to return the state of arrow keys from termporary change
    function GetDroppedDown: Boolean;
    function GetItemWidth: Integer;
    procedure SetItemWidth(const AValue: Integer);
    procedure SetItems(Value: TStrings);
    procedure LMDrawListItem(var TheMessage: TLMDrawListItem); message LM_DrawListItem;
    procedure LMMeasureItem(var TheMessage: TLMMeasureItem); message LM_MeasureItem;
    procedure CNCommand(var TheMessage: TLMCommand); message CN_Command;
    procedure UpdateSorted;
    procedure SetArrowKeysTraverseList(Value: Boolean);
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure DrawItem(Index: Integer; ARect: TRect;
      State: TOwnerDrawState); virtual;
    procedure LMChange(var msg); message LM_CHANGED;
    procedure Change; dynamic;
    procedure Select; dynamic;
    procedure DropDown; dynamic;
    procedure CloseUp; dynamic;
    procedure AdjustDropDown; virtual;

    function GetItemCount: Integer; //override;
    function GetItemHeight: Integer; virtual;
    function GetSelLength: integer; virtual;
    function GetSelStart: integer; virtual;
    function GetSelText: string; virtual;
    function GetItemIndex: integer; virtual;
    function GetMaxLength: integer; virtual;
    procedure InitializeWnd; override;
    function SelectItem(const AnItem: String): Boolean;
    procedure SetDropDownCount(const AValue: Integer); virtual;
    procedure SetDroppedDown(const AValue: Boolean); virtual;
    procedure SetItemHeight(const AValue: Integer); virtual;
    procedure SetItemIndex(Val: integer); virtual;
    procedure SetMaxLength(Val: integer); virtual;
    procedure SetSelLength(Val: integer); virtual;
    procedure SetSelStart(Val: integer); virtual;
    procedure SetSelText(const Val: string); virtual;
    procedure SetSorted(Val: boolean); virtual;
    procedure SetStyle(Val: TComboBoxStyle); virtual;
    procedure RealSetText(const AValue: TCaption); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure ControlKeyDown(var Key: Word; Shift: TShiftState); override;

    property DropDownCount: Integer read FDropDownCount write SetDropDownCount default 8;
    property ItemHeight: Integer read GetItemHeight write SetItemHeight;
    property ItemWidth: Integer read GetItemWidth write SetItemWidth;
    property MaxLength: integer read GetMaxLength write SetMaxLength default -1;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
    property OnDrawItem: TDrawItemEvent read FOnDrawItem write FOnDrawItem;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnMeasureItem: TMeasureItemEvent
      read FOnMeasureItem write FOnMeasureItem;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
    property Sorted: boolean read FSorted write SetSorted;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddItem(const Item: String; AnObject: TObject); //override;
    procedure AddHistoryItem(const Item: string; MaxHistoryCount: integer;
                             SetAsText, CaseSensitive: boolean);
    procedure AddHistoryItem(const Item: string; AnObject: TObject;
                   MaxHistoryCount: integer; SetAsText, CaseSensitive: boolean);
    procedure Clear; virtual;
    procedure ClearSelection; //override;
    property DroppedDown: Boolean read GetDroppedDown write SetDroppedDown;
    procedure MeasureItem(Index: Integer; var TheHeight: Integer); virtual;
    procedure SelectAll;

    property AutoDropDown: Boolean
                           read FAutoDropDown write FAutoDropDown default False;
    property ArrowKeysTraverseList: Boolean read FArrowKeysTraverseList
                                    write SetArrowKeysTraverseList default True;
    property Canvas: TCanvas read FCanvas;
    property Items: TStrings read FItems write SetItems;
    property ItemIndex: integer read GetItemIndex write SetItemIndex default -1;
    property SelLength: integer read GetSelLength write SetSelLength;
    property SelStart: integer read GetSelStart write SetSelStart;
    property SelText: String read GetSelText write SetSelText;
    property Style: TComboBoxStyle read FStyle write SetStyle;
    property Text;
  published
    property TabStop default true;
  end;


  { TComboBox }

  TComboBox = class(TCustomComboBox)
  published
    property Align;
    property Anchors;
    property ArrowKeysTraverseList;
    property AutoDropDown;
    property BorderSpacing;
    property Ctl3D;
    property DropDownCount;
    property Enabled;
    property Font;
    property ItemHeight;
    property Items;
    property ItemIndex default -1; // keep after Items
    property ItemWidth;
    property MaxLength;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnCloseUp;
    property OnDblClick;
    property OnDrawItem;
    property OnDropDown;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnSelect;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property Style;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
  end;


  { TCustomListBox }

  TListBoxStyle = (lbStandard, lbOwnerDrawFixed, lbOwnerDrawVariable);
  TSelectionChangeEvent = procedure(Sender: TObject; User: boolean) of object;

  { TCustomListBox }

  TCustomListBox = class(TWinControl)
  private
    FCacheValid: Boolean;
    FCanvas: TCanvas;
    FClickOnSelChange: boolean;
    FClickTriggeredBySelectionChange: Boolean;
    FExtendedSelect: boolean;
    FIntegralHeight: boolean;
    FItemHeight: Integer;
    FItemIndex: integer;
    FItems: TStrings;
    FLockSelectionChange: integer;
    FMultiSelect: boolean;
    FOnDrawItem: TDrawItemEvent;
    FOnMeasureItem: TMeasureItemEvent;
    FOnSelectionChange: TSelectionChangeEvent;
    FSorted: boolean;
    FStyle: TListBoxStyle;
    FTopIndex: integer;
    function GetTopIndex: Integer;
    procedure SetTopIndex(const AValue: Integer);
    procedure UpdateSelectionMode;
    procedure UpdateSorted;
    procedure LMDrawListItem(var TheMessage: TLMDrawListItem); message LM_DrawListItem;
    procedure LMMeasureItem(var TheMessage: TLMMeasureItem); message LM_MeasureItem;
    procedure LMSelChange(var TheMessage); message LM_SelChange;
    procedure WMLButtonDown(Var Message: TLMLButtonDown); message LM_LBUTTONDOWN;
    procedure SendItemSelected(Index: integer; IsSelected: boolean);
  protected
    procedure AssignItemDataToCache(const AIndex: Integer; const AData: Pointer); virtual; // called to store item data while the handle isn't created
    procedure AssignCacheToItemData(const AIndex: Integer; const AData: Pointer); virtual; // called to restore the itemdata after a handle is created
    procedure Loaded; override;
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
    procedure CheckIndex(const AIndex: Integer);
    function GetItemHeight: Integer;
    function GetItemIndex: integer; virtual;
    function GetSelCount: integer;
    function GetSelected(Index: integer): boolean;
    function GetCachedDataSize: Integer; virtual; // returns the amount of data needed per item
    function GetCachedData(const AIndex: Integer): Pointer;
    procedure SetExtendedSelect(Val: boolean); virtual;
    procedure SetItemIndex(Val: integer); virtual;
    procedure SetItems(Value: TStrings); virtual;
    procedure SetItemHeight(Value: Integer);
    procedure SetMultiSelect(Val: boolean); virtual;
    procedure SetSelected(Index: integer; Val: boolean);
    procedure SetSorted(Val: boolean); virtual;
    procedure SetStyle(Val: TListBoxStyle); virtual;
    procedure DrawItem(Index: Integer; ARect: TRect;
      State: TOwnerDrawState); virtual;
    procedure DoSelectionChange(User: Boolean); virtual;
    procedure SendItemIndex;
  protected
    property OnMeasureItem: TMeasureItemEvent
                                       read FOnMeasureItem write FOnMeasureItem;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function GetIndexAtY(Y: integer): integer;
    function ItemAtPos(const Pos: TPoint; Existing: Boolean): Integer;
    function ItemRect(Index: Integer): TRect;
    function ItemVisible(Index: Integer): boolean;
    function ItemFullyVisible(Index: Integer): boolean;
    procedure MakeCurrentVisible;
    procedure MeasureItem(Index: Integer; var TheHeight: Integer); virtual;
    procedure Clear; virtual;
    procedure LockSelectionChange;
    procedure UnlockSelectionChange;
  public
    { to be called by widgetset backend }
    // equivalent of LM_SELCHANGED
    procedure IntfSelectionChanged; virtual;
  public
    property Align;
    property Anchors;
    property BorderStyle default bsSingle;
    property Canvas: TCanvas read FCanvas;
    property ClickOnSelChange: boolean read FClickOnSelChange
               write FClickOnSelChange default true; // true is Delphi behaviour
    property Constraints;
    property ExtendedSelect: boolean read FExtendedSelect write SetExtendedSelect;
    property Font;
    property IntegralHeight: boolean read FIntegralHeight write FIntegralHeight; // not implemented
    property ItemHeight: Integer read GetItemHeight write SetItemHeight;
    property ItemIndex: integer read GetItemIndex write SetItemIndex;
    property Items: TStrings read FItems write SetItems;
    property MultiSelect: boolean read FMultiSelect write SetMultiSelect;
    property OnChangeBounds;
    property OnClick;
    property OnDblClick;
    property OnDrawItem: TDrawItemEvent read FOnDrawItem write FOnDrawItem;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnSelectionChange: TSelectionChangeEvent read FOnSelectionChange
                                                      write FOnSelectionChange;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property SelCount: integer read GetSelCount;
    property Selected[Index: integer]: boolean read GetSelected write SetSelected;
    property ShowHint;
    property Sorted: boolean read FSorted write SetSorted;
    property Style: TListBoxStyle read FStyle write SetStyle;
    property TabOrder;
    property TabStop default true;
    property TopIndex: Integer read GetTopIndex write SetTopIndex;
    property Visible;
  end;


  { TListBox }

  TListBox = class(TCustomListBox)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property BorderStyle;
    property ClickOnSelChange;
    property Constraints;
    property ExtendedSelect;
    property Font;
    property IntegralHeight;
    property Items;
    property ItemHeight;
    property MultiSelect;
    property OnChangeBounds;
    property OnClick;
    property OnDblClick;
    property OnDrawItem;
    property OnEnter;
    property OnExit;
    property OnKeyPress;
    property OnKeyDown;
    property OnKeyUp;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnSelectionChange;
    property ParentShowHint;
    property ParentFont;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property Style;
    property TabOrder;
    property TabStop;
    property TopIndex;
    property Visible;
  end;


  { TCustomEdit }

  TEditCharCase = (ecNormal, ecUppercase, ecLowerCase);
  TEchoMode = (emNormal, emNone, emPassword);

  { TCustomEdit }

  TCustomEdit = class(TWinControl)
  private
    FCharCase: TEditCharCase;
    FEchoMode: TEchoMode;
    FMaxLength: Integer;
    FModified: Boolean;
    FPasswordChar: Char;
    FReadOnly: Boolean;
    FOnChange: TNotifyEvent;
    FSelLength: integer;
    FSelStart: integer;
    function GetModified: Boolean;
    procedure SetCharCase(Value: TEditCharCase);
    procedure SetMaxLength(Value: Integer);
    procedure SetModified(Value: Boolean);
    procedure SetPasswordChar(const AValue: Char);
    procedure SetReadOnly(Value: Boolean);
  Protected
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer); override;
    procedure CreateWnd; override;
    procedure CMTextChanged(Var Message: TLMessage); message CM_TextChanged;
    procedure Change; dynamic;
    function GetSelLength: integer; virtual;
    function GetSelStart: integer; virtual;
    function GetSelText: string; virtual;
    procedure InitializeWnd; override;
    procedure SetEchoMode(Val: TEchoMode); virtual;
    procedure SetSelLength(Val: integer); virtual;
    procedure SetSelStart(Val: integer); virtual;
    procedure SetSelText(const Val: string); virtual;
    procedure RealSetText(const Value: TCaption); override;
    function ChildClassAllowed(ChildClass: TClass): boolean; override;
    procedure ControlKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SelectAll;
    procedure ClearSelection; virtual;
    procedure CopyToClipboard; virtual;
    procedure CutToClipboard; virtual;
    procedure PasteFromClipboard; virtual;
  public
    property BorderStyle;
    property CharCase: TEditCharCase read FCharCase write SetCharCase default ecNormal;
    property EchoMode: TEchoMode read FEchoMode write SetEchoMode default emNormal;
    property MaxLength: Integer read FMaxLength write SetMaxLength default -1;
    property Modified: Boolean read GetModified write SetModified;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property PasswordChar: Char read FPasswordChar write SetPasswordChar default #0;
    property PopupMenu;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default false;
    property SelLength: integer read GetSelLength write SetSelLength;
    property SelStart: integer read GetSelStart write SetSelStart;
    property SelText: String read GetSelText write SetSelText;
    property TabOrder;
    property TabStop default true;
    property Text;
  end;


  { TMemoScrollbar }

  TMemoScrollbar = class(TControlScrollBar)
  protected
    function GetHorzScrollBar: TControlScrollBar; override;
    function GetVertScrollBar: TControlScrollBar; override;
  public
    property Increment;
    property Page;
    property Smooth;
    property Position;
    property Range;
    property Size;
    property Visible;
  end;


  { TCustomMemo }

  TCustomMemo = class(TCustomEdit)
  private
    FHorzScrollBar: TMemoScrollBar;
    FLines: TStrings;
    FScrollBars: TScrollStyle;
    FVertScrollBar: TMemoScrollBar;
    FWordWrap: Boolean;
    procedure SetHorzScrollBar(const AValue: TMemoScrollBar);
    procedure SetVertScrollBar(const AValue: TMemoScrollBar);
    function StoreScrollBars: boolean;
  protected
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
    function  RealGetText: TCaption; override;
    procedure RealSetText(const Value: TCaption); override;
    procedure SetLines(const Value: TStrings);
    procedure SetWordWrap(const Value: boolean);
    procedure SetScrollBars(const Value: TScrollStyle);
    procedure InitializeWnd; override;
    procedure Loaded; override;
    function WordWrapIsStored: boolean; virtual;
    procedure ControlKeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Append(const Value: String);
    procedure Clear;
  public
    property Lines: TStrings read FLines write SetLines;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars;
    property WordWrap: Boolean read FWordWrap write SetWordWrap stored WordWrapIsStored default true;
    //property Font: TFont read FFont write FFont;
    property HorzScrollBar: TMemoScrollBar
      read FHorzScrollBar write SetHorzScrollBar stored StoreScrollBars;
    property VertScrollBar: TMemoScrollBar
      read FVertScrollBar write SetVertScrollBar stored StoreScrollBars;
  end;


  { TEdit }

  TEdit = class(TCustomEdit)
  published
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property Color;
    property Constraints;
    property CharCase;
    property DragMode;
    property EchoMode;
    property Enabled;
    property MaxLength;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    Property OnKeyDown;
    property OnKeyPress;
    Property OnKeyUp;
    Property OnMouseDown;
    Property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property Text;
    property Visible;
  end;


  { TMemo }

  TMemo = class(TCustomMemo)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Color;
    property Constraints;
    property Font;
    property Lines;
    property MaxLength;
    property OnChange;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseEnter;
    property OnMouseLeave;
    property ParentFont;
    property PopupMenu;
    property ReadOnly;
    property ScrollBars;
    property TabOrder;
    property TabStop;
    property Visible;
    property WordWrap;
  end;


  { TCustomStaticText }

  TStaticBorderStyle = (sbsNone, sbsSingle, sbsSunken);

  TCustomStaticText = class(TWinControl)
  private
    FAlignment: TAlignment;
    FFocusControl: TWinControl;
    FShowAccelChar: boolean;
    FStaticBorderStyle: TStaticBorderStyle;
    procedure SetAlignment(Value: TAlignment);
    procedure SetStaticBorderStyle(Value: TStaticBorderStyle);
    procedure WMActivate(var Message: TLMActivate); message LM_ACTIVATE;
  protected
    function GetLabelText: String ; virtual;
    procedure RealSetText(const AValue: TCaption); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetFocusControl(Val: TWinControl); virtual;
    procedure SetShowAccelChar(Val: boolean); virtual;
    {$IFNDEF EnablePreferredSize}
    procedure DoAutoSize; override;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property BorderStyle: TStaticBorderStyle read FStaticBorderStyle write SetStaticBorderStyle default sbsNone;
    property FocusControl: TWinControl read FFocusControl write SetFocusControl;
    property ShowAccelChar: boolean read FShowAccelChar write SetShowAccelChar default true;
  end;


  { TStaticText }

  TStaticText = class(TCustomStaticText)
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property BorderStyle;
    property Caption;
    property Color;
    property Constraints;
    property FocusControl;
    property Font;
    property OnChangeBounds;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property ParentFont;
    property ShowAccelChar;
    property Visible;
  end;


  { TButtonControl }

  TButtonControl = class(TWinControl)
  private
    FClicksDisabled: Boolean;
    FOnChange: TNotifyEvent;
    FUseOnChange: boolean;
    function IsCheckedStored: boolean;
    function UseOnChangeIsStored: boolean;
  protected
    fLastCheckedOnChange: boolean;
    function GetChecked: Boolean; virtual;
    procedure SetChecked(Value: Boolean); virtual;
    procedure DoOnChange; virtual;
    procedure Click; override;
    function ColorIsStored: boolean; override;
    procedure Loaded; override;
  protected
    property Checked: Boolean read GetChecked write SetChecked stored IsCheckedStored default False;
    property ClicksDisabled: Boolean read FClicksDisabled write FClicksDisabled;
    property UseOnChange: boolean read FUseOnChange write FUseOnChange stored UseOnChangeIsStored;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

  { TButtonActionLink - Finish me }

  TButtonActionLink = class(TWinControlActionLink)
  protected
    FClientButton: TButtonControl;
    procedure AssignClient(AClient: TObject); override;
    function IsCheckedLinked: Boolean; override;
    procedure SetChecked(Value: Boolean); override;
  end;

  TButtonActionLinkClass = class of TButtonActionLink;


  { TCustomCheckBox }

  // ToDo: delete TLeftRight when in classesh.inc
  TLeftRight = taLeftJustify..taRightJustify;

  TCheckBoxState = (cbUnchecked, cbChecked, cbGrayed);

  { TCustomCheckBox }

  TCustomCheckBox = class(TButtonControl)
  private
    FAllowGrayed: Boolean;
    FState: TCheckBoxState;
    FShortCut: TShortcut;
    procedure SetState(Value: TCheckBoxState);
    function GetState: TCheckBoxState;
    procedure DoChange(var Msg); message LM_CHANGED;
  protected
    function RetrieveState: TCheckBoxState;
    procedure InitializeWnd; override;
    procedure Toggle; virtual;
    function DialogChar(var Message: TLMKey): boolean; override;
    function GetChecked: Boolean; override;
    procedure SetChecked(Value: Boolean); override;
    procedure RealSetText(const Value: TCaption); override;
    procedure ApplyChanges; virtual;
    procedure Loaded; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
                      X, Y: Integer); override;
  public
    constructor Create(TheOwner: TComponent); override;
  public
    property AutoSize default true;
    property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed default false;
    property State: TCheckBoxState read GetState write SetState;
    property TabStop default true;
    property UseOnChange;
    property OnChange;
  end;

{$IFNDef NewCheckBox}
  // Normal checkbox
  TCheckBox = class(TCustomCheckBox)
  protected
    {$IFNDEF EnablePreferredSize}
    procedure DoAutoSize; override;
    {$ENDIF}
  published
    property Action;
    property Align;
    property AllowGrayed;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property Caption;
    property Checked;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Hint;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property State;
    property TabOrder;
    property TabStop;
    property UseOnChange;
    property Visible;
  end;
{$Else NewCheckBox}
  // new checkbox
  TCBAlignment = (alLeftJustify, alRightJustify);

  TCheckBoxStyle = (cbsSystem, cbsCrissCross, cbsCheck);

  TCheckBox = Class(TCustomControl)
  Private
    FAllowGrayed,
    FWordWrap,
    FAttachTextToBox: Boolean;
    FAlignment: TCBAlignment;
    FState : TCheckBoxState;
    FCheckBoxStyle: TCheckBoxStyle;
    FMouseIsDragging,
    FMouseInControl: Boolean;
  Protected
    Procedure DoAutoSize; Override;
    Procedure SetAlignment(Value: TCBAlignment);
    Procedure SetState(Value: TCheckBoxState);

    Function GetChecked: Boolean;
    procedure SetChecked(Value: Boolean);
    procedure SetCheckBoxStyle(Value: TCheckBoxStyle);
    procedure SetAttachTextToBox(Value: Boolean);

    procedure CMMouseEnter(var Message: TLMMouse); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TLMMouse); message CM_MOUSELEAVE;
    Procedure WMMouseDown(var Message: TLMMouseEvent); Message LM_LBUTTONDOWN;
    Procedure WMMouseUp(var Message: TLMMouseEvent); Message LM_LBUTTONUP;
    Procedure WMKeyDown(var Message: TLMKeyDown); Message LM_KeyDown;
    Procedure WMKeyUp(var Message: TLMKeyUp); Message LM_KeyUp;
  public
    procedure Paint; Override;
    Procedure PaintCheck(var PaintRect: TRect);
    Procedure PaintText(var PaintRect: TRect);

    Constructor Create(AOwner: TComponent); Override;
    Function CheckBoxRect: TRect;
    procedure Click; Override;

    Property MouseInControl: Boolean read FMouseInControl;
    Property MouseIsDragging: Boolean read FMouseIsDragging;
  published
    property Alignment: TCBAlignment read FAlignment write SetAlignment;
    Property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed;
    Property Checked: Boolean read GetChecked write SetChecked;
    property State: TCheckBoxState read FState write SetState;
    property CheckBoxStyle: TCheckBoxStyle read FCheckBoxStyle write SetCheckBoxStyle;
    property AttachToBox: Boolean read FAttachTextToBox write SetAttachTextToBox default True;

    property Align;
    Property AutoSize;
    property WordWrap: Boolean read FWordWrap write FWordWrap;
    property TabStop;

    property Anchors;
    property Constraints;
    property Hint;
    property Font;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property Visible;
    property Caption;
    property Enabled;
    property ShowHint;
    property ParentFont;
    property ParentShowHint;
    property TabOrder;
  end;
{$EndIf NewCheckBox}


  { TToggleBox }

  TToggleBox = class(TCustomCheckBox)
  private
  public
    constructor Create(TheOwner: TComponent); override;
  published
    property AllowGrayed;
    property Anchors;
    property BorderSpacing;
    property Caption;
    property Checked;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Hint;
    property OnChange;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property State;
    property TabOrder;
    property TabStop;
    property UseOnChange;
    property Visible;
  end;


  { TRadioButton }

  TRadioButton = class(TCustomCheckBox)
  protected
    function DialogChar(var Message: TLMKey): boolean; override;
    procedure RealSetText(const Value: TCaption); override;
    {$IFNDEF EnablePreferredSize}
    procedure DoAutoSize; override;
    {$ENDIF}
  public
    constructor Create(TheOwner: TComponent); override;
  published
    property Align;
    property AllowGrayed;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property Caption;
    property Checked;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Hint;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property State;
    property TabOrder;
    property TabStop;
    property UseOnChange;
    property Visible;
  end;


  { TCustomLabel }

  TCustomLabel = class(TGraphicControl)
  Private
    FAlignment: TAlignment;
    FFocusControl: TWinControl;
    FShowAccelChar: Boolean;
    FWordWrap: Boolean;
    FLayout: TTextLayout;
    Procedure FontChange(Sender: TObject);
  protected
    function  CanTab: boolean; override;
    procedure CalcSize(var AWidth, AHeight: integer);
    procedure DoAutoSize; override;
    function  DialogChar(var Message: TLMKey): boolean; override;
    procedure CMTextChanged(var Message: TLMSetText); message CM_TEXTCHANGED;

    procedure WMActivate(var Message: TLMActivate); message LM_ACTIVATE;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function  GetShowAccelChar: Boolean;
    function  GetAlignment: TAlignment;
    function  GetTransparent: boolean;
    procedure SetAlignment(Value: TAlignment);
    procedure SetFocusControl(Value: TWinControl);
    procedure SetLayout(Value: TTextLayout);
    procedure SetShowAccelChar(Value: Boolean);
    procedure SetTransparent(NewTransparent: boolean);
    procedure SetWordWrap(Value: Boolean);
    procedure Loaded; override;

    property Alignment: TAlignment read GetAlignment write SetAlignment;
    property FocusControl: TWinControl read FFocusControl write SetFocusControl;
    property Layout: TTextLayout read FLayout write SetLayout default tlTop;
    property ShowAccelChar: Boolean read GetShowAccelChar write SetShowAccelChar default true;
    property Transparent: boolean read GetTransparent write SetTransparent default true;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default false;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    property AutoSize default True;
  end;
  
  
  { TLabel }

  TLabel = class(TCustomLabel)
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property Caption;
    property Color;
    property Constraints;
    property Enabled;
    property FocusControl;
    property Font;
    property Layout;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnChangeBounds;
    property OnResize;
  end;

var
  DefaultButtonControlUseOnChange: boolean;

procedure Register;

implementation

uses
  WSControls, WSStdCtrls; // Widgetset uses circle is allowed


type
  TMemoStrings = class(TStrings)
  private
    FMemo: TCustomMemo;
    FMemoWidgetClass: TWSCustomMemoClass;
  protected
    function Get(Index: Integer): String; override;
    function GetCount: Integer; override;
  public
    constructor Create(AMemo: TCustomMemo);
    procedure Clear; override;
    procedure Delete(index: Integer); override;
    procedure Insert(index: Integer; const S: String); override;

    property MemoWidgetClass: TWSCustomMemoClass read FMemoWidgetClass write FMemoWidgetClass;
  end;

procedure Register;
begin
  RegisterComponents('Standard',[TLabel,TEdit,TMemo,TToggleBox,TCheckBox,
       TRadioButton,TListBox,TComboBox,TScrollBar,TGroupBox,TStaticText]);
end;


{$I customgroupbox.inc}
{$I customcombobox.inc}
{$I customlistbox.inc}
{$I custommemo.inc}
{$I customedit.inc}
{$I customlabel.inc}
{$I customcheckbox.inc}

{$I scrollbar.inc}
{$I memoscrollbar.inc}
{$I memo.inc}
{$I memostrings.inc}

{$I edit.inc}
{$I buttoncontrol.inc}

{$I checkbox.inc}

{$I radiobutton.inc}
{$I togglebox.inc}

{$I customstatictext.inc}

initialization
  DefaultButtonControlUseOnChange:=false;

end.

{ =============================================================================

  $Log$
  Revision 1.207  2005/06/13 08:04:38  vincents
  fixed crashed with csOwnerDrawVariable combobox style (bug 934)    from Jesus

  Revision 1.206  2005/05/26 22:15:51  mattias
  fixed triggering TListBox.Click when clicking on selected item

  Revision 1.205  2005/05/06 10:21:38  micha
  prevent double onclick event when using click-on-selchange

  Revision 1.204  2005/05/06 09:39:49  micha
  introduce intfselectionchanged procedure for widgetsets that do not send LM_SELCHANGED automatically
  fix senditemindex to cause onclick event

  Revision 1.203  2005/05/02 09:17:08  mattias
  started TButtonActionLink

  Revision 1.202  2005/04/27 12:37:28  micha
  implement/fix button/label shortcut accelchar handling

  Revision 1.201  2005/04/19 15:06:30  mattias
  fixed small aesthetical editoroptions bugs

  Revision 1.200  2005/04/05 19:41:19  mattias
  accelerated TMemo on gtk, gtk2 and win32 interface  from Andrew Haines

  Revision 1.199  2005/03/20 21:12:15  vincents
  made TCustomCombobox.Clear virtual

  Revision 1.198  2005/03/10 09:02:11  mattias
  handle tab key in ControlKeyDown in TCustomEdit and TCustomComboBox

  Revision 1.197  2005/03/08 10:32:47  mattias
  BorderStyle for TCustomEdit in win32 intf  from Jesus

  Revision 1.196  2005/02/26 17:08:41  marc
  * Reworked listviews to match new interface

  Revision 1.195  2005/02/21 20:15:27  mattias
  fixed componentpalette adding via double click

  Revision 1.194  2005/02/21 13:54:26  mattias
  added navigation key check for up/down already handled

  Revision 1.193  2005/02/19 18:23:27  mattias
  made TListBox.Clear virtual

  Revision 1.192  2005/02/11 16:26:26  micha
  select index of item in list, if text occurs in list

  Revision 1.191  2005/02/04 15:36:50  mattias
  published TComboBox.ItemIndex  from Sergios

  Revision 1.190  2005/02/04 15:24:56  mattias
  published TEdit.OnEditingDone

  Revision 1.189  2005/02/03 15:10:23  micha
  implement shortcut handling, tcustomlabel accelerator focuscontrol functionality

  Revision 1.188  2005/01/26 17:36:02  mattias
  added error message for TStaticText.BorderStyle not implemented during designing

  Revision 1.187  2005/01/21 14:18:11  micha
  implement TCustomLabel.Layout

  Revision 1.186  2005/01/21 13:38:10  micha
  implement TCustomLabel.Transparent

  Revision 1.185  2005/01/20 16:58:16  mattias
  published TComboBox.OnDbClick

  Revision 1.184  2005/01/17 16:42:35  mattias
  improved TLabel autosizing

  Revision 1.183  2005/01/11 21:36:36  micha
  remove TStaticText.Layout property, not supported by delphi, hard to implement

  Revision 1.182  2005/01/11 13:25:23  vincents
  TCustomCheckBox.AllowGray defaults to false.

  Revision 1.181  2005/01/08 22:13:21  vincents
  TLabel.ShowAccelChar default value is True

  Revision 1.180  2005/01/07 21:53:48  micha
  label can not bew focused anymore, hide tabstop/taborder

  Revision 1.179  2005/01/07 21:36:37  micha
  publish Layout property (compatibility with situation before swap with tstatictext)

  Revision 1.178  2005/01/07 20:51:11  micha
  swap TCustomStaticText and TCustomLabel

  Revision 1.177  2005/01/01 19:36:40  mattias
  fixed loading TRadioButton.Checked

  Revision 1.176  2004/12/31 11:59:47  mattias
  published TEdit.Color - only useful under windows, gtk1 ignores it

  Revision 1.175  2004/12/27 19:40:59  mattias
  published BorderSpacing for many controls

  Revision 1.174  2004/12/27 16:51:19  mattias
  implemented dialog to edit basic help db settings

  Revision 1.173  2004/12/16 20:13:27  vincents
  made some protected TCustomCombobox properties public, so they can be used by the win32 widget set.

  Revision 1.172  2004/12/13 16:43:37  mattias
  fixed loading project flags and added RTTI example for readonly properties

  Revision 1.171  2004/11/03 14:18:35  mattias
  implemented preferred size for controls for theme depending AutoSizing

  Revision 1.170  2004/09/25 15:05:38  mattias
  implemented Rename Identifier

  Revision 1.169  2004/09/22 19:05:58  mattias
  various fixes for TCustomMemo, RTTIControls, FindReferences

  Revision 1.168  2004/09/22 14:50:18  micha
  convert LM_SETPROPERTIES message for tcustomlabel to interface methods

  Revision 1.167  2004/09/17 10:56:25  micha
  convert LM_SHORTCUT message to interface methods

  Revision 1.166  2004/09/10 17:59:57  micha
  convert LM_APPENDTEXT to interface method

  Revision 1.165  2004/09/10 09:43:12  micha
  convert LM_SETLABEL message to interface methods

  Revision 1.164  2004/08/30 10:49:20  mattias
  fixed focus catch for combobox csDropDownList

  Revision 1.163  2004/08/26 19:09:33  mattias
  moved navigation key handling to TApplication and added options for custom navigation

  Revision 1.162  2004/08/22 12:10:56  mattias
  better theming for some dialogs

  Revision 1.161  2004/08/18 09:31:21  mattias
  removed obsolete unit vclglobals

  Revision 1.160  2004/08/17 19:01:36  mattias
  gtk intf now ignores size notifications of unrealized widgets

  Revision 1.159  2004/08/15 22:31:51  mattias
  fixed fpc 1.0.10 compilation

  Revision 1.158  2004/08/13 16:40:47  mazen
  + TCharater type used to allow UTF8 keyboard with gtk2

  Revision 1.157  2004/08/13 10:20:19  mattias
  fixed codetools ConstSet, implemented notifying TApplication whenmenu popups

  Revision 1.156  2004/08/05 21:20:47  mattias
  moved designer/abstractformeditor.pp to ideintf/formeditingintf.pas

  Revision 1.155  2004/08/04 09:57:17  mattias
  TStaticText.CanTab=false

  Revision 1.154  2004/07/16 21:49:00  mattias
  added RTTI controls

  Revision 1.153  2004/07/13 10:34:15  mattias
  fixed lcl package unit file name checklist.pas

  Revision 1.152  2004/07/10 18:17:30  mattias
  added Delphi ToDo support, Application.WndProc, small bugfixes  from Colin

  Revision 1.151  2004/07/07 22:26:58  mattias
  fixed showing grabers for boundless components

  Revision 1.150  2004/07/03 11:11:08  mattias
  TGTKListStringList now keeps selection on Put and Move

  Revision 1.149  2004/06/27 09:34:23  mattias
  fixed TStringGrid goEditing   from Jesus

  Revision 1.148  2004/06/14 12:54:02  micha
  fix designer cursor to not set Form.Cursor directly

  Revision 1.147  2004/06/10 22:07:58  vincents
  listbox style changes are notified to the widgetset

  Revision 1.146  2004/05/30 14:02:30  mattias
  implemented OnChange for TRadioButton, TCheckBox, TToggleBox and some more docking stuff

  Revision 1.145  2004/05/21 18:34:44  mattias
  readded protected TWinControl.BorderStyle

  Revision 1.144  2004/05/21 18:12:17  mattias
  quick fixed crashing property overloading BorderStyle

  Revision 1.143  2004/05/21 11:13:18  micha
  add measureitem to tcustomlistbox just like tcustomcombobox has

  Revision 1.142  2004/05/21 09:03:54  micha
  implement new borderstyle
  - centralize to twincontrol (protected)
  - public expose at tcustomcontrol to let interface access it

  Revision 1.141  2004/04/18 23:55:39  marc
  * Applied patch from Ladislav Michl
  * Changed the way TControl.Text is resolved
  * Added setting of text to TWSWinControl

  Revision 1.140  2004/04/02 19:39:46  mattias
  fixed checking empty mask raw image

  Revision 1.139  2004/03/12 15:48:57  mattias
  fixed 1.0.x compilation

  Revision 1.138  2004/03/08 22:36:01  mattias
  added TWinControl.ParentFormInitializeWnd

  Revision 1.137  2004/03/08 00:48:05  mattias
  moved TOnwerDrawState to StdCtrls

  Revision 1.136  2004/03/07 09:37:20  mattias
  added workaround for AutoSize in TCustomLabel

  Revision 1.135  2004/02/24 20:26:50  mattias
  published some TRadioButton properties

  Revision 1.134  2004/02/23 23:15:12  mattias
  improved FindDragTarget

  Revision 1.133  2004/02/23 20:06:05  mattias
  published TLabel.OnMouseXXX

  Revision 1.132  2004/02/22 10:43:20  mattias
  added child-parent checks

  Revision 1.131  2004/02/13 18:21:31  mattias
  fixed combo chane

  Revision 1.130  2004/02/09 19:52:52  mattias
  implemented ByteOrder for TLazIntfImage and added call of to LM_SETFONT

  Revision 1.129  2004/02/06 16:58:58  mattias
  updated polish translation

  Revision 1.128  2004/02/05 13:53:38  mattias
  fixed GetConstraints for win32 intf

  Revision 1.127  2004/02/05 09:45:33  mattias
  implemented Actions for TSpeedButton, TMenuItem, TCheckBox

  Revision 1.126  2004/02/04 23:30:18  mattias
  completed TControl actions

  Revision 1.125  2004/02/04 22:17:09  mattias
  removed workaround VirtualCreate

  Revision 1.124  2004/02/04 12:59:07  mattias
  added TToolButton.Action and published some props

  Revision 1.123  2004/02/04 11:09:40  mattias
  added DefineProperties check for check lfm

  Revision 1.122  2004/02/04 00:21:40  mattias
  added SelectDirectory and TListBox.ItemVisible

  Revision 1.121  2004/02/04 00:04:37  mattias
  added some TEdit ideas to TSpinEdit

  Revision 1.120  2004/02/02 12:44:45  mattias
  implemented interface constraints

  Revision 1.119  2004/02/02 00:41:06  mattias
  TScrollBar now automatically checks Align and Anchors for useful values

  Revision 1.118  2004/01/27 21:32:11  mattias
  improved changing style of controls

  Revision 1.117  2004/01/21 10:19:16  micha
  enable tabstops for controls; implement tabstops in win32 intf

  Revision 1.116  2004/01/12 15:04:41  mattias
  implemented TCustomListBox.ItemAtPos

  Revision 1.115  2004/01/11 11:57:54  mattias
  implemented TCustomListBox.ItemRect for gtk1 intf

  Revision 1.114  2004/01/06 17:58:06  mattias
  fixed setting TRadioButton.Caption for gtk

  Revision 1.113  2004/01/03 20:36:29  mattias
  published TEdit.Enabled

  Revision 1.112  2003/11/28 23:24:57  mattias
  implemented Clean Directories

  Revision 1.111  2003/11/27 19:40:34  mattias
  added TListBox.PopupMenu

  Revision 1.110  2003/11/08 14:12:48  mattias
  fixed scrollbar events under gtk from Colin

  Revision 1.109  2003/11/01 18:58:15  mattias
  added clipboard support for TCustomEdit from Colin

  Revision 1.108  2003/10/16 23:54:27  marc
  Implemented new gtk keyevent handling

  Revision 1.107  2003/09/26 18:19:40  ajgenius
  add minor TEdit/TMemo properties for delphi compatiblitity

  Revision 1.106  2003/09/23 08:00:46  mattias
  improved OnEnter for gtkcombo

  Revision 1.105  2003/09/18 11:24:29  mattias
  started TDBMemo

  Revision 1.104  2003/09/18 09:21:03  mattias
  renamed LCLLinux to LCLIntf

  Revision 1.103  2003/08/28 09:10:00  mattias
  listbox and comboboxes now set sort and selection at handle creation

  Revision 1.102  2003/08/26 08:12:33  mattias
  applied listbox/combobox patch from Karl

  Revision 1.101  2003/07/30 13:03:44  mattias
  replaced label with memo

  Revision 1.100  2003/07/07 23:58:43  marc
  + Implemented TCheckListBox.Checked[] property

  Revision 1.99  2003/06/23 09:42:09  mattias
  fixes for debugging lazarus

  Revision 1.98  2003/06/16 22:47:19  mattias
  fixed keeping TForm.Visible=false

  Revision 1.97  2003/06/13 14:38:01  mattias
  fixed using streamed clientwith/height for child anchors

  Revision 1.96  2003/06/12 16:18:23  mattias
  applied TComboBox fix for grabbing keys from Yoyong

  Revision 1.95  2003/06/10 17:23:34  mattias
  implemented tabstop

  Revision 1.94  2003/06/10 15:58:39  mattias
  started TLabeledEdit

  Revision 1.93  2003/06/10 13:35:54  mattias
  implemented TComboBox dropdown from Yoyong

  Revision 1.92  2003/06/07 09:34:21  mattias
  added ambigius compiled unit test for packages

  Revision 1.91  2003/04/29 13:35:39  mattias
  improved configure build lazarus dialog

  Revision 1.90  2003/04/16 22:59:35  mattias
  added TMaskEdit from Tony

  Revision 1.89  2003/04/15 08:54:27  mattias
  fixed TMemo.WordWrap

  Revision 1.88  2003/04/11 17:10:20  mattias
  added but not implemented ComboBoxDropDown

  Revision 1.87  2003/04/04 16:35:24  mattias
  started package registration

  Revision 1.86  2003/03/29 17:20:05  mattias
  added TMemoScrollBar

  Revision 1.85  2003/03/28 23:03:38  mattias
  started TMemoScrollbar

  Revision 1.84  2003/03/25 16:56:57  mattias
  implemented TButtonControl.UseOnChange

  Revision 1.83  2003/03/25 16:29:53  mattias
  fixed sending TButtonControl.OnClick on every change

  Revision 1.82  2003/03/17 20:53:16  mattias
  removed SetRadioButtonGroupMode

  Revision 1.81  2003/03/17 20:50:30  mattias
  fixed TRadioGroup.ItemIndex=-1

  Revision 1.80  2003/03/17 08:51:09  mattias
  added IsWindowVisible

  Revision 1.79  2003/03/11 07:46:43  mattias
  more localization for gtk- and win32-interface and lcl

  Revision 1.78  2003/03/09 17:44:12  mattias
  finshed Make Resourcestring dialog and implemented TToggleBox

  Revision 1.77  2003/03/08 21:51:57  mattias
  make resource string dialog nearly complete

  Revision 1.76  2003/02/28 15:49:43  mattias
  fixed initial size

  Revision 1.75  2003/01/24 13:07:33  mattias
  fixed TListBox.BorderStyle=bsNone

  Revision 1.74  2003/01/01 10:46:59  mattias
  fixes for win32 listbox/combobox from Karl Brandt

  Revision 1.73  2002/12/28 21:44:51  mattias
  further cleanup

  Revision 1.72  2002/12/27 10:34:55  mattias
  message view scrolls to message

  Revision 1.71  2002/12/27 08:46:32  mattias
  changes for fpc 1.1

  Revision 1.70  2002/12/22 23:25:34  mattias
  fixed setting TEdit properties after creating handle

  Revision 1.69  2002/12/12 17:47:45  mattias
  new constants for compatibility

  Revision 1.68  2002/11/27 14:37:37  mattias
  added form editor options for rubberband and colors

  Revision 1.67  2002/11/16 11:22:56  mbukovjan
  Fixes to MaxLength. TCustomMemo now has MaxLength, too.

  Revision 1.66  2002/11/12 10:16:14  lazarus
  MG: fixed TMainMenu creation

  Revision 1.65  2002/10/26 15:15:47  lazarus
  MG: broke LCL<->interface circles

  Revision 1.64  2002/10/26 11:20:30  lazarus
  MG: broke some interfaces.pp circles

  Revision 1.63  2002/10/25 09:47:37  lazarus
  MG: added inputdialog.inc

  Revision 1.62  2002/10/25 08:25:43  lazarus
  MG: broke circle stdctrls.pp <-> forms.pp

  Revision 1.61  2002/10/24 19:35:34  lazarus
  AJ: Fixed forms <-> stdctrls circular uses

  Revision 1.60  2002/10/24 10:05:51  lazarus
  MG: broke graphics.pp <-> clipbrd.pp circle

  Revision 1.59  2002/10/23 20:47:26  lazarus
  AJ: Started Form Scrolling
      Started StaticText FocusControl
      Fixed Misc Dialog Problems
      Added TApplication.Title

  Revision 1.58  2002/10/21 15:51:27  lazarus
  AJ: moved TCustomStaticText code to include/customstatictext.inc

  Revision 1.57  2002/10/20 22:57:18  lazarus
  AJ:switched to gtk_widget_newv to work around array of const

  Revision 1.56  2002/10/20 21:54:03  lazarus
  MG: fixes for 1.1

  Revision 1.55  2002/10/18 16:08:09  lazarus
  AJ: Partial HintWindow Fix; Added Screen.Font & Font.Name PropEditor; Started to fix ComboBox DropDown size/pos

  Revision 1.54  2002/10/14 14:29:50  lazarus
  AJ: Improvements to TUpDown; Added TStaticText & GNOME DrawText

  Revision 1.53  2002/10/04 14:24:14  lazarus
  MG: added DrawItem to TComboBox/TListBox

  Revision 1.52  2002/10/03 18:04:46  lazarus
  MG: started customdrawitem

  Revision 1.51  2002/10/03 14:47:30  lazarus
  MG: added TComboBox.OnPopup+OnCloseUp+ItemWidth

  Revision 1.50  2002/10/03 00:08:50  lazarus
  AJ: TCustomLabel Autosize, TCustomCheckbox '&' shortcuts started

  Revision 1.49  2002/10/02 16:16:40  lazarus
  MG: accelerated unitdependencies

  Revision 1.48  2002/10/02 14:23:22  lazarus
  MG: added various history lists

  Revision 1.47  2002/10/01 18:00:03  lazarus
  AJ: Initial TUpDown, minor property additions to improve reading Delphi created forms.

  Revision 1.46  2002/09/27 20:52:22  lazarus
  MWE: Applied patch from "Andrew Johnson" <aj_genius@hotmail.com>

  Here is the run down of what it includes -

   -Vasily Volchenko's Updated Russian Localizations

   -improvements to GTK Styles/SysColors
   -initial GTK Palette code - (untested, and for now useless)

   -Hint Windows and Modal dialogs now try to stay transient to
    the main program form, aka they stay on top of the main form
    and usually minimize/maximize with it.

   -fixes to Form BorderStyle code(tool windows needed a border)

   -fixes DrawFrameControl DFCS_BUTTONPUSH to match Win32 better
    when flat

   -fixes DrawFrameControl DFCS_BUTTONCHECK to match Win32 better
    and to match GTK theme better. It works most of the time now,
    but some themes, noteably Default, don't work.

   -fixes bug in Bitmap code which broke compiling in NoGDKPixbuf
    mode.

   -misc other cleanups/ fixes in gtk interface

   -speedbutton's should now draw correctly when flat in Win32

   -I have included an experimental new CheckBox(disabled by
    default) which has initial support for cbGrayed(Tri-State),
    and WordWrap, and misc other improvements. It is not done, it
    is mostly a quick hack to test DrawFrameControl
    DFCS_BUTTONCHECK, however it offers many improvements which
    can be seen in cbsCheck/cbsCrissCross (aka non-themed) state.

   -fixes Message Dialogs to more accurately determine
    button Spacing/Size, and Label Spacing/Size based on current
    System font.
   -fixes MessageDlgPos, & ShowMessagePos in Dialogs
   -adds InputQuery & InputBox to Dialogs

   -re-arranges & somewhat re-designs Control Tabbing, it now
    partially works - wrapping around doesn't work, and
    subcontrols(Panels & Children, etc) don't work. TabOrder now
    works to an extent. I am not sure what is wrong with my code,
    based on my other tests at least wrapping and TabOrder SHOULD
    work properly, but.. Anyone want to try and fix?

   -SynEdit(Code Editor) now changes mouse cursor to match
    position(aka over scrollbar/gutter vs over text edit)

   -adds a TRegion property to Graphics.pp, and Canvas. Once I
    figure out how to handle complex regions(aka polygons) data
    properly I will add Region functions to the canvas itself
    (SetClipRect, intersectClipRect etc.)

   -BitBtn now has a Stored flag on Glyph so it doesn't store to
    lfm/lrs if Glyph is Empty, or if Glyph is not bkCustom(aka
    bkOk, bkCancel, etc.) This should fix most crashes with older
    GDKPixbuf libs.

  Revision 1.45  2002/09/18 17:07:24  lazarus
  MG: added patch from Andrew

  Revision 1.44  2002/09/09 07:26:42  lazarus
  MG: started TCollectionPropertyEditor

  Revision 1.43  2002/09/08 19:09:55  lazarus
  Fixed and simplified TRadioButton

  Revision 1.42  2002/09/07 12:14:50  lazarus
  EchoMode for TCustomEdit. emNone not implemented for GTK+, falls back to emPassword
  behaviour.

  Revision 1.41  2002/09/05 10:12:06  lazarus

  New dialog for multiline caption of TCustomLabel.
  Prettified TStrings property editor.
  Memo now has automatic scrollbars (not fully working), WordWrap and Scrollbars property
  Removed saving of old combo text (it broke things and is not needed). Cleanups.

  Revision 1.40  2002/09/03 11:32:49  lazarus

  Added shortcut keys to labels
  Support for alphabetically sorting the properties
  Standardize message and add shortcuts ala Kylix
  Published BorderStyle, unpublished BorderWidth
  ShowAccelChar and FocusControl
  ShowAccelChar and FocusControl for TLabel, escaped ampersands now work.

  Revision 1.39  2002/09/03 08:07:19  lazarus
  MG: image support, TScrollBox, and many other things from Andrew

  Revision 1.38  2002/08/30 06:46:03  lazarus

  Use comboboxes. Use history. Prettify the dialog. Preselect text on show.
  Make the findreplace a dialog. Thus removing resiying code (handled by Anchors now anyway).
  Make Anchors work again and publish them for various controls.
  SelStart and Co. for TEdit, SelectAll procedure for TComboBox and TEdit.
  Clean up and fix some bugs for TComboBox, plus selection stuff.

  Revision 1.37  2002/08/27 18:45:13  lazarus
  MG: propedits text improvements from Andrew, uncapturing, improved comobobox

  Revision 1.36  2002/08/27 14:33:37  lazarus
  MG: fixed designer component deletion

  Revision 1.35  2002/08/25 13:31:35  lazarus
  MG: replaced C-style operators

  Revision 1.34  2002/08/24 06:51:22  lazarus
  MG: from Andrew: style list fixes, autosize for radio/checkbtns

  Revision 1.33  2002/08/19 20:34:47  lazarus
  MG: improved Clipping, TextOut, Polygon functions

  Revision 1.32  2002/08/17 15:45:32  lazarus
  MG: removed ClientRectBugfix defines

  Revision 1.31  2002/07/23 07:40:51  lazarus
  MG: fixed get widget position for inherited gdkwindows

  Revision 1.30  2002/05/20 14:19:03  lazarus
  MG: activated the clientrect bugfixes

  Revision 1.29  2002/05/13 14:47:00  lazarus
  MG: fixed client rectangles, TRadioGroup, RecreateWnd

  Revision 1.28  2002/05/10 06:05:50  lazarus
  MG: changed license to LGPL

  Revision 1.27  2002/05/09 12:41:28  lazarus
  MG: further clientrect bugfixes

  Revision 1.26  2002/04/22 13:07:45  lazarus
  MG: fixed AdjustClientRect of TGroupBox

  Revision 1.25  2002/04/21 06:53:54  lazarus
  MG: fixed save lrs to test dir

  Revision 1.24  2002/04/18 08:09:03  lazarus
  MG: added include comments

  Revision 1.23  2002/04/18 07:53:08  lazarus
  MG: fixed find declaration of forward def class

  Revision 1.22  2002/03/25 17:59:19  lazarus
  GTK Cleanup
  Shane

  Revision 1.21  2002/02/20 23:33:24  lazarus
  MWE:
    + Published OnClick for TMenuItem
    + Published PopupMenu property for TEdit and TMemo (Doesn't work yet)
    * Fixed debugger running twice
    + Added Debugger output form
    * Enabled breakpoints

  Revision 1.20  2002/02/03 00:24:01  lazarus
  TPanel implemented.
  Basic graphic primitives split into GraphType package, so that we can
  reference it from interface (GTK, Win32) units.
  New Frame3d canvas method that uses native (themed) drawing (GTK only).
  New overloaded Canvas.TextRect method.
  LCLLinux and Graphics was split, so a bunch of files had to be modified.

  Revision 1.19  2002/01/09 22:49:25  lazarus
  MWE: Converted to Unix fileformat

  Revision 1.18  2002/01/09 22:47:29  lazarus
  MWE: published OnClick for checkbox family

  Revision 1.17  2001/12/07 20:12:15  lazarus
  Added a watch dialog.
  Shane

  Revision 1.16  2001/10/19 14:27:43  lazarus
  MG: fixed customradiogroup OnClick + ItemIndex

  Revision 1.15  2001/06/14 14:57:58  lazarus
  MG: small bugfixes and less notes

  Revision 1.14  2001/03/27 21:12:53  lazarus
  MWE:
    + Turned on longstrings
    + modified memotest to add lines

  Revision 1.13  2001/02/02 14:23:38  lazarus
  Start of code completion code.
  Shane

  Revision 1.12  2001/02/01 16:45:19  lazarus
  Started the code completion.
  Shane

  Revision 1.11  2001/01/28 21:06:07  lazarus
  Changes for TComboBox events KeyPress Focus.
  Shane

  Revision 1.10  2001/01/11 20:16:47  lazarus
  Added some TImageList code.
  Added a bookmark resource with 10 resource images.
  Removed some of the IFDEF's in mwCustomEdit around the inherited code.
  Shane

  Revision 1.8  2001/01/05 17:44:37  lazarus
  ViewUnits1, ViewForms1 and MessageDlg are all loaded from their resources and all controls are auto-created on them.
  There are still a few problems with some controls so I haven't converted all forms.
  Shane

  Revision 1.7  2001/01/04 15:09:05  lazarus
  Tested TCustomEdit.Readonly, MaxLength and CharCase.
  Shane

  Revision 1.6  2001/01/04 13:52:00  lazarus
  Minor changes to TEdit.
  Not tested.
  Shane

  Revision 1.5  2000/12/29 15:04:07  lazarus
  Added more images to the resource.
  Shane

  Revision 1.4  2000/12/01 15:50:39  lazarus
  changed the TCOmponentInterface SetPropByName.  It works for a few properties, but not all.
  Shane

  Revision 1.3  2000/11/29 21:22:35  lazarus
  New Object Inspector code
  Shane

  Revision 1.2  2000/07/16 12:45:01  lazarus
  Added procedure ListBox.Clear (changes by chris, added by stoppok)

  Revision 1.1  2000/07/13 10:28:24  michael
  + Initial import

  Revision 1.28  2000/07/09 20:41:20  lazarus
  Added Attachsignals method to custombobobox, stoppok

  Revision 1.27  2000/06/29 21:07:08  lazarus
  some more published properties for combobox, stoppok

  Revision 1.26  2000/06/24 21:30:19  lazarus
  *** empty log message ***

  Revision 1.25  2000/06/16 13:33:21  lazarus
  Created a new method for adding controls to the toolbar to be dropped onto the form!
  Shane

  Revision 1.24  2000/05/30 22:28:41  lazarus
  MWE:
    Applied patches from Vincent Snijders:
    + Added GetWindowRect
    * Fixed horz label alignment
    + Added vert label alignment

  Revision 1.23  2000/05/08 12:54:19  lazarus
  Removed some writeln's
  Added alignment for the TLabel.  Isn't working quite right.
  Added the shell code for WindowFromPoint and GetParent.
  Added FindLCLWindow
  Shane

  Revision 1.22  2000/04/18 20:06:39  lazarus
  Added some functions to Compiler.pp

  Revision 1.21  2000/04/13 21:25:16  lazarus
  MWE:
    ~ Added some docu and did some cleanup.
  Hans-Joachim Ott <hjott@compuserve.com>:
    * TMemo.Lines works now.
    + TMemo has now a property Scrollbar.
    = TControl.GetTextBuf revised :-)
    + Implementation for CListBox columns added
    * Bug in TGtkCListStringList.Assign corrected.

  Revision 1.20  2000/04/10 14:03:06  lazarus
  Added SetProp and GetProp winapi calls.
  Added ONChange to the TEdit's published property list.
  Shane

  Revision 1.19  2000/03/30 21:57:45  lazarus
  MWE:
    + Added some general functions to Get/Set the Main/Fixed/CoreChild
      widget
    + Started with graphic scalig/depth stuff. This is way from finished

  Hans-Joachim Ott <hjott@compuserve.com>:
    + Added some improvements for TMEMO

  Revision 1.18  2000/03/30 18:07:54  lazarus
  Added some drag and drop code
  Added code to change the unit name when it's saved as a different name.  Not perfect yet because if you are in a comment it fails.

  Shane

  Revision 1.17  2000/02/28 19:16:04  lazarus
  Added code to the FILE CLOSE to check if the file was modified.  HAven't gotten the application.messagebox working yet though.  It won't stay visible.
  Shane

  Revision 1.16  2000/02/24 21:15:30  lazarus
  Added TCustomForm.GetClientRect and RequestAlign to try and get the controls to align correctly when a MENU is present.  Not Complete yet.

  Fixed the bug in TEdit that caused it not to update it's text property.  I will have to
  look at TMemo to see if anything there was affected.

  Added SetRect to WinAPI calls
  Added AdjustWindowRectEx to WINAPI calls.
  Shane

  Revision 1.15  2000/02/22 22:19:50  lazarus
  TCustomDialog is a descendant of TComponent.
  Initial cuts a form's proper Close behaviour.

  Revision 1.14  2000/02/22 21:51:40  lazarus
  MWE: Removed some double (or triple) event declarations.
       The latest compiler doesn't like it

  Revision 1.13  2000/02/21 17:38:04  lazarus
  Added modalresult to TCustomForm
  Added a View Units dialog box
  Added a View Forms dialog box
  Added a New Unit menu selection
  Added a New Form menu selection
  Shane

  Revision 1.12  2000/02/18 19:38:53  lazarus
  Implemented TCustomForm.Position
  Better implemented border styles. Still needs some tweaks.
  Changed TComboBox and TListBox to work again, at least partially.
  Minor cleanups.

  Revision 1.11  2000/01/04 19:16:09  lazarus
  Stoppok:
     - new messages LM_GETVALUE, LM_SETVALUE, LM_SETPROPERTIES
     - changed trackbar, progressbar, checkbox to use above messages
     - some more published properties for above components
       (all properties derived from TWinControl)
     - new functions SetValue, GetValue, SetProperties in gtk-interface

  Revision 1.10  1999/12/30 19:04:13  lazarus
   - Made TRadiobutton work again
   - Some more cleanups to checkbox code
           stoppok

  Revision 1.9  1999/12/30 10:38:59  lazarus

    Some changes to Checkbox code.
      stoppok

  Revision 1.8  1999/12/29 01:30:02  lazarus

    Made groupbox working again.
      stoppok

  Revision 1.7  1999/12/18 18:27:32  lazarus
  MWE:
    Rearranged some events to get a LM_SIZE, LM_MOVE and LM_WINDOWPOSCHANGED
    Initialized the TextMetricstruct to zeros to clear unset values
    Get mwEdit to show more than one line
    Fixed some errors in earlier commits

  Revision 1.6  1999/12/07 01:19:26  lazarus
  MWE:
    Removed some double events
    Changed location of SetCallBack
    Added call to remove signals
    Restructured somethings
    Started to add default handlers in TWinControl
    Made some parts of TControl and TWinControl more delphi compatible
    ... and lots more ...

  Revision 1.5  1999/11/01 01:28:30  lazarus
  MWE: Implemented HandleNeeded/CreateHandle/CreateWND
       Now controls are created on demand. A call to CreateComponent shouldn't
       be needed. It is now part of CreateWnd

  Revision 1.4  1999/10/27 17:27:07  lazarus
  Added alot of changes and TODO: statements
  shane

  Revision 1.3  1999/10/25 17:38:52  lazarus
  More stuff added for compatability.  Most stuff added was put in the windows.pp file.  CONST scroll bar messages and such.  2 functions were also added to that unit that needs to be completed.
  Shane

  Revision 1.2  1999/10/22 21:01:51  lazarus

        Removed calls to InterfaceObjects except for controls.pp. Commented
        out any gtk depend lines of code.     MAH

  Revision 1.1  1999/10/19 19:16:51  lazarus
  renamed stdcontrols.pp stdctrls.pp
  Shane

  Revision 1.9  1999/08/21 13:57:41  lazarus
  Implemented TListBox.BorderStyle. The listbox is scrollable now.

  Revision 1.8  1999/08/14 10:05:56  lazarus
  Added TListBox ItemIndex property. Made ItemIndex public for TComboBox and TListBox.

  Revision 1.7  1999/08/11 20:41:34  lazarus

  Minor changes and additions made.  Lazarus may not compile due to these changes

  Revision 1.6  1999/08/07 17:59:23  lazarus

        buttons.pp   the DoLeave and DoEnter were connected to the wrong
                     event.

        The rest were modified to use the new SendMessage function.   MAH

 }


