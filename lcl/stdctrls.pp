{ 
 /*************************************************************************** 
                               StdCtrls.pp
                               -----------
 
                   Initial Revision  : Tue Oct 19 CST 1999 
 
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
 
{
@author()
@created()
@lastmod()

Detailed description of the Unit.
}

unit StdCtrls;

{$mode objfpc}{$H+}
{off $Define NewCheckBox}
interface


uses
  VCLGlobals, Classes, SysUtils, LCLType, Graphics, GraphType, LMessages,
  Controls, Forms, ExtendedStrings;


type

  { TScrollBar }

  TScrollStyle = (ssNone, ssHorizontal, ssVertical, ssBoth,
    ssAutoHorizontal, ssAutoVertical, ssAutoBoth);

  TScrollCode = (scLineUp, scLineDown, scPageUp, scPageDown, scPosition,
                 scTrack, scTop, scBottom, scEndScroll);

  TScrollEvent = procedure(Sender: TObject; ScrollCode: TScrollCode;
                           var ScrollPos: Integer) of object;

  TScrollBar = class(TWinControl)
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
    procedure CNHScroll(var Message: TLMHScroll); message CN_HSCROLL;
    procedure CNVScroll(var Message: TLMVScroll); message CN_VSCROLL;
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
  published
    property Align;
    property Anchors;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Kind: TScrollBarKind read FKind write SetKind default sbHorizontal;
    property LargeChange: TScrollBarInc read FLargeChange write FLargeChange default 1;
    property Max: Integer read FMax write SetMax default 100;
    property Min: Integer read FMin write SetMin default 0;
    property PageSize: Integer read FPageSize write SetPageSize;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property Position: Integer read FPosition write SetPosition default 0;
    property ShowHint;
    property SmallChange: TScrollBarInc read FSmallChange write FSmallChange default 1;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnScroll: TScrollEvent read FOnScroll write FOnScroll;
    property OnStartDrag;
  end;


  { TCustomGroupBox }

  TCustomGroupBox = class (TWinControl) {class(TCustomControl) }
  protected
  public
    constructor Create(AOwner : TComponent); Override;
  end;
  
  
  { TGroupBox }
                                                                                                   
  TGroupBox = class(TCustomGroupBox)
  published
    property Align;
    property Anchors;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
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
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
  end;


  { TCustomComboBox }

  TComboBoxStyle = (csDropDown, csSimple, csDropDownList, csOwnerDrawFixed,
                    csOwnerDrawVariable);

  TDrawItemEvent = procedure(Control: TWinControl; Index: Integer;
    ARect: TRect; State: TOwnerDrawState) of object;
  TMeasureItemEvent = procedure(Control: TWinControl; Index: Integer;
    var Height: Integer) of object;

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
    FOnChange : TNotifyEvent;
    FOnCloseUp: TNotifyEvent;
    FOnDrawItem: TDrawItemEvent;
    FOnDropDown: TNotifyEvent;
    FOnMeasureItem: TMeasureItemEvent;
    FOnSelect: TNotifyEvent;
    FSelLength: integer;
    FSelStart: integer;
    FSorted : boolean;
    FStyle : TComboBoxStyle;
    function GetDroppedDown: Boolean;
    function GetItemWidth: Integer;
    procedure SetItemWidth(const AValue: Integer);
    procedure SetItems(Value : TStrings);
    procedure LMDrawListItem(var TheMessage : TLMDrawListItem); message LM_DrawListItem;
    procedure CNCommand(var TheMessage : TLMCommand); message CN_Command;
  protected
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
    procedure DrawItem(Index: Integer; ARect: TRect;
      State: TOwnerDrawState); virtual;
    procedure DoChange(var msg); message LM_CHANGED;
    procedure Change; dynamic;
    procedure Loaded; override;
    procedure Select; dynamic;
    procedure DropDown; dynamic;
    procedure CloseUp; dynamic;
    procedure AdjustDropDown; virtual;

    function GetItemCount: Integer; //override;
    function GetItemHeight: Integer; virtual;
    function GetSelLength : integer; virtual;
    function GetSelStart : integer; virtual;
    function GetSelText : string; virtual;
    function GetItemIndex : integer; virtual;
    function GetMaxLength : integer; virtual;
    procedure InitializeWnd; override;
    function SelectItem(const AnItem: String): Boolean;
    procedure SetDropDownCount(const AValue: Integer); virtual;
    procedure SetDroppedDown(const AValue: Boolean); virtual;
    procedure SetItemHeight(const AValue: Integer); virtual;
    procedure SetItemIndex(Val : integer); virtual;
    procedure SetMaxLength(Val : integer); virtual;
    procedure SetSelLength(Val : integer); virtual;
    procedure SetSelStart(Val : integer); virtual;
    procedure SetSelText(const Val : string); virtual;
    procedure SetSorted(Val : boolean); virtual;
    procedure SetStyle(Val : TComboBoxStyle); virtual;

    property DropDownCount: Integer read
      FDropDownCount write SetDropDownCount default 8;
    property Items: TStrings read FItems write SetItems;
    property ItemHeight: Integer read GetItemHeight write SetItemHeight;
    property ItemIndex: integer read GetItemIndex write SetItemIndex;
    property ItemWidth: Integer read GetItemWidth write SetItemWidth;
    property MaxLength: integer read GetMaxLength write SetMaxLength default 0;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
    property OnDrawItem: TDrawItemEvent read FOnDrawItem write FOnDrawItem;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnMeasureItem: TMeasureItemEvent
      read FOnMeasureItem write FOnMeasureItem;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
    property Sorted: boolean read FSorted write SetSorted;
    property Style: TComboBoxStyle read FStyle write SetStyle;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure AddItem(const Item: String; AnObject: TObject); //override;
    procedure AddHistoryItem(const Item: string; MaxHistoryCount: integer;
      SetAsText, CaseSensitive: boolean);
    procedure AddHistoryItem(const Item: string; AnObject: TObject;
      MaxHistoryCount: integer; SetAsText, CaseSensitive: boolean);
    procedure Clear; //override;
    procedure ClearSelection; //override;
    property DroppedDown: Boolean read GetDroppedDown write SetDroppedDown;
    procedure MeasureItem(Index: Integer; var TheHeight: Integer); virtual;
    procedure SelectAll;

    property AutoDropDown: Boolean
      read FAutoDropDown write FAutoDropDown default False;
    property Canvas: TCanvas read FCanvas;
    property SelLength: integer read GetSelLength write SetSelLength;
    property SelStart: integer read GetSelStart write SetSelStart;
    property SelText: String read GetSelText write SetSelText;
  end;
  
  
  { TComboBox }

  TComboBox = class(TCustomComboBox)
  public
    property ItemIndex;
  published
    property Anchors;
    property Ctl3D;
    property DropDownCount;
    property Enabled;
    property Font;
    property ItemHeight;
    property ItemWidth;
    property Items;
    property MaxLength;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property Sorted;
    property Style;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnDropDown;
    property OnEnter;
    property OnExit;
    property OnKeyPress;
    Property OnKeyDown;
    Property OnKeyUp;
  end;
    

  { TCustomListBox }
    
  TListBoxStyle = (lbStandard, lbOwnerDrawFixed, lbOwnerDrawVariable);
  
  TCustomListBox = class(TWinControl)
  private
    FBorderStyle : TBorderStyle;
    FCanvas: TCanvas;
    FExtendedSelect, FMultiSelect : boolean;
    FItems : TStrings;
    FItemHeight: Integer;
    FItemIndex: integer;
    FOnDrawItem: TDrawItemEvent;
    FSorted : boolean;
    FStyle : TListBoxStyle;
    procedure UpdateSelectionMode;
    procedure UpdateSorted;
    procedure LMDrawListItem(var TheMessage : TLMDrawListItem); message LM_DrawListItem;
    procedure SendItemSelected(Index: integer; IsSelected: boolean);
  protected
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
    function GetItemHeight: Integer;
    function GetItemIndex : integer; virtual;
    function GetSelCount : integer;
    function GetSelected(Index : integer) : boolean;
    procedure SetBorderStyle(Val : TBorderStyle); virtual;
    procedure SetExtendedSelect(Val : boolean); virtual;
    procedure SetItemIndex(Val : integer); virtual;
    procedure SetItems(Value : TStrings); virtual;
    procedure SetItemHeight(Value: Integer);
    procedure SetMultiSelect(Val : boolean); virtual;
    procedure SetSelected(Index : integer; Val : boolean);
    procedure SetSorted(Val : boolean); virtual;
    procedure SetStyle(Val : TListBoxStyle); virtual;
    procedure DrawItem(Index: Integer; ARect: TRect;
      State: TOwnerDrawState); virtual;
  protected
    property BorderStyle : TBorderStyle read FBorderStyle write SetBorderStyle;
    property ExtendedSelect : boolean read FExtendedSelect write SetExtendedSelect;
    property Sorted : boolean read FSorted write SetSorted;
    property Style : TListBoxStyle read FStyle write SetStyle;
    property ItemHeight: Integer read GetItemHeight write SetItemHeight;
    property OnDrawItem: TDrawItemEvent read FOnDrawItem write FOnDrawItem;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    property Canvas: TCanvas read FCanvas;
    property ItemIndex : integer read GetItemIndex write SetItemIndex;
    property Items : TStrings read FItems write SetItems;
    property MultiSelect : boolean read FMultiSelect write SetMultiSelect;
    property SelCount : integer read GetSelCount;
    property Selected[Index : integer] : boolean read GetSelected write SetSelected;
  end;
  
  
  { TListBox }
    
  TListBox = class(TCustomListBox)
  published
    property Align;
    property Anchors;
    property BorderStyle;
    property ExtendedSelect;
    property Items;
    property ItemHeight;
    property MultiSelect;
    property ParentShowHint;
    property ShowHint;
    property Sorted;
    property Style;
    property TabOrder;
    property TabStop;
    property Visible;
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
    property OnResize;
  end;


  { TCustomEdit }

  TEditCharCase = (ecNormal, ecUppercase, ecLowerCase);
  TEchoMode = (emNormal, emNone, emPassword);

  TCustomEdit = class(TWinControl)
  private
    FCharCase : TEditCharCase;
    FEchoMode : TEchoMode;
    FMaxLength : Integer;
    FModified : Boolean;
    FReadOnly : Boolean;
    FOnChange : TNotifyEvent;
    FSelLength : integer;
    FSelStart : integer;
    function GetModified : Boolean;
    procedure SetCharCase(Value : TEditCharCase);
    procedure SetMaxLength(Value : Integer);
    procedure SetModified(Value : Boolean);
    procedure SetReadOnly(Value : Boolean);
  Protected
    Procedure DoAutoSize; Override;

    procedure CMTextChanged(Var Message : TLMessage); message CM_TextChanged;
    procedure Change; dynamic;
    function GetSelLength : integer; virtual;
    function GetSelStart : integer; virtual;
    function GetSelText : string; virtual;
    procedure InitializeWnd; override;
    procedure SetEchoMode(Val : TEchoMode); virtual;
    procedure SetSelLength(Val : integer); virtual;
    procedure SetSelStart(Val : integer); virtual;
    procedure SetSelText(const Val : string); virtual;

    property OnChange : TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SelectAll;
    property CharCase : TEditCharCase read FCharCase write SetCharCase default ecNormal;
    property EchoMode : TEchoMode read FEchoMode write SetEchoMode default emNormal;
    property MaxLength : Integer read FMaxLength write SetMaxLength default 0;
    property ReadOnly : Boolean read FReadOnly write SetReadOnly default false;
    property SelLength: integer read GetSelLength write SetSelLength;
    property SelStart: integer read GetSelStart write SetSelStart;
    property SelText: String read GetSelText write SetSelText;
    property Modified : Boolean read GetModified write SetModified;
    property Text;
  published
    property TabStop;
    property TabOrder;
  end;
  
  
  { TCustomMemo }

  TCustomMemo = class(TCustomEdit)
  private
    FFont : TFont;
    FLines: TStrings;
    FScrollBars: TScrollStyle;
    FWordWrap: Boolean;
  protected
    procedure SetLines(Value : TStrings);
    procedure SetWordWrap(const Value : boolean);
    procedure SetScrollBars(const Value : TScrollStyle);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Append(const Value : String);
    property Lines: TStrings read FLines write SetLines;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars;
    property WordWrap: Boolean read FWordWrap write SetWordWrap;
    property Font : TFont read FFont write FFont;
  end;
  
  
  { TEdit }

  TEdit = class(TCustomEdit)
  published
    property AutoSize;
    property Anchors;
    property CharCase;
    property DragMode;
    property EchoMode;
    property MaxLength;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnKeyPress;
    Property OnKeyDown;
    Property OnKeyUp;
  end;
  
  
  { TMemo }

  TMemo = class(TCustomMemo)
  published
    property Align;
    property Anchors;
    property Color;
    property Font;
    property Lines;
    property PopupMenu;
    property ReadOnly;
    property ScrollBars;
    property Tabstop;
    property Visible;
    property WordWrap;
    property OnChange;
    property OnEnter;
    property OnExit;
    property OnKeyPress;
    Property OnKeyDown;
    Property OnKeyUp;
  end;


  { TCustomLabel }

  TCustomLabel = class(TWinControl)
  private
    FAlignment : TAlignment;
    FWordWrap : Boolean;
    FLayout : TTextLayout;
    FFocusControl : TWinControl;
    FShowAccelChar : boolean;
    procedure SetAlignment(Value : TAlignment);
    procedure SetLayout(Value : TTextLayout);
    procedure SetWordWrap(Value : Boolean);
    procedure WMActivate(var Message: TLMActivate); message LM_ACTIVATE;
  protected
    function GetLabelText: String ; virtual;
    procedure DoAutoSize; Override;
    procedure Notification(AComponent : TComponent; Operation : TOperation); override;
    procedure SetFocusControl(Val : TWinControl); virtual;
    procedure SetShowAccelChar(Val : boolean); virtual;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property FocusControl : TWinControl read FFocusControl write SetFocusControl;
    property Layout: TTextLayout read FLayout write SetLayout default tlBottom;
    property ShowAccelChar : boolean read FShowAccelChar write SetShowAccelChar default true;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default false;
  public
    constructor Create(AOwner : TComponent); override;
  end;


  { TLabel }

  TLabel = class(TCustomLabel)
  published
    property Align;
    property Alignment;
    property AutoSize;
    property Anchors;
    property Caption;
    property Color;
    property FocusControl;
    property Font;
    property Visible;
    property Layout;
    property ShowAccelChar;
    property WordWrap;
  end;


  { TButtonControl }

  TButtonControl = class(TWinControl)
  private
    FClicksDisabled: Boolean;
    function IsCheckedStored: boolean;
  protected
    function GetChecked: Boolean; virtual;
    procedure SetChecked(Value: Boolean); virtual;
    property Checked: Boolean read GetChecked write SetChecked stored IsCheckedStored default False;
    property ClicksDisabled: Boolean read FClicksDisabled write FClicksDisabled;
  public
    constructor Create(AOwner: TComponent); override;
  end;


  { TCustomCheckBox }

  // ToDo: delete TLeftRight when in classesh.inc
  TLeftRight = taLeftJustify..taRightJustify;

  TCheckBoxState = (cbUnchecked, cbChecked, cbGrayed);

  TCustomCheckBox = class(TButtonControl)
  private
    // FAlignment: TLeftRight;
    FAllowGrayed: Boolean;
    FState: TCheckBoxState;
    FShortCut : TLMShortcut;
    procedure SetState(Value: TCheckBoxState);
    function GetState : TCheckBoxState;
  protected
    procedure InitializeWnd; override;
    procedure Toggle; virtual;
    function GetChecked: Boolean; override;
    procedure SetChecked(Value: Boolean); override;
    procedure SetText(const Value: TCaption); override;
    procedure ApplyChanges; virtual;
    property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed;
    property State: TCheckBoxState read GetState write SetState;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property TabOrder;
    property TabStop;
  end;

{$IFNDef NewCheckBox}
  TCheckBox = class(TCustomCheckBox)
  protected
    procedure DoAutoSize; Override;
    procedure SetText(const Value: TCaption); Override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoSize;
    property AllowGrayed;
    property Anchors;
    property Caption;
    property Checked;
    property State;
    property Visible;
    property Enabled;
    property OnEnter;
    property OnExit;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Hint;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;
{$Else NewCheckBox}
  TCBAlignment = (alLeftJustify, alRightJustify);

  TCheckBoxStyle = (cbsSystem, cbsCrissCross, cbsCheck);

  TCheckBox = Class(TCustomControl)
  Private
    FAllowGrayed,
    FWordWrap,
    FAttachTextToBox : Boolean;
    FAlignment : TCBAlignment;
    FState  : TCheckBoxState;
    FCheckBoxStyle : TCheckBoxStyle;
    FMouseIsDragging,
    FMouseInControl: Boolean;
  Protected
    Procedure DoAutoSize; Override;
    Procedure SetAlignment(Value : TCBAlignment);
    Procedure SetState(Value : TCheckBoxState);

    Function GetChecked : Boolean;
    procedure SetChecked(Value : Boolean);
    procedure SetCheckBoxStyle(Value : TCheckBoxStyle);
    procedure SetAttachTextToBox(Value : Boolean);

    procedure CMMouseEnter(var Message: TLMMouse); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TLMMouse); message CM_MOUSELEAVE;
    Procedure WMMouseDown(var Message : TLMMouseEvent); Message LM_LBUTTONDOWN;
    Procedure WMMouseUp(var Message : TLMMouseEvent); Message LM_LBUTTONUP;
    Procedure WMKeyDown(var Message : TLMKeyDown); Message LM_KeyDown;
    Procedure WMKeyUp(var Message : TLMKeyUp); Message LM_KeyUp;
  public
    procedure Paint; Override;
    Procedure PaintCheck(var PaintRect: TRect);
    Procedure PaintText(var PaintRect: TRect);

    Constructor Create(AOwner: TComponent); Override;
    Function CheckBoxRect : TRect;
    procedure Click; Override;

    Property MouseInControl : Boolean read FMouseInControl;
    Property MouseIsDragging : Boolean read FMouseIsDragging;
  published
    property Alignment : TCBAlignment read FAlignment write SetAlignment;
    Property AllowGrayed : Boolean read FAllowGrayed write FAllowGrayed;
    Property Checked : Boolean read GetChecked write SetChecked;
    property State : TCheckBoxState read FState write SetState;
    property CheckBoxStyle : TCheckBoxStyle read FCheckBoxStyle write SetCheckBoxStyle;
    property AttachToBox : Boolean read FAttachTextToBox write SetAttachTextToBox default True;

    property Align;
    Property AutoSize;
    property WordWrap : Boolean read FWordWrap write FWordWrap;
    //property OnMouseEnter;
    //property OnMouseExit;
    property TabStop;

    property Anchors;
    property Constraints;
    property Hint;
    property Font;
    property OnClick;
    property OnDblClick;
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
    property OnEnter;
    property OnExit;
  end;
{$EndIf NewCheckBox}

  TToggleBox = class(TCustomCheckBox)
  private
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AllowGrayed;
    property Anchors;
    property Caption;
    property Checked;
    property State;
    property Visible;
    property Enabled;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Hint;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
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
  end;


  {TRadioButton}
   
  TRadioButton = class(TCustomCheckBox)
  protected
    procedure DoAutoSize; override;
    procedure SetText(const Value: TCaption); override;
  public
    constructor Create (AOwner: TComponent); override;
  published
    property Anchors;
    property AutoSize;
    property AllowGrayed;
    property Caption;
    property Checked;
    property State;
    property Visible;
    property Enabled;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Hint;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
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
  end;

  {TStaticText}
  TStaticBorderStyle = (sbsNone, sbsSingle, sbsSunken);

  TCustomStaticText = class(TCustomControl)
  Private
    FAlignment: TAlignment;
    FBorderStyle: TStaticBorderStyle;
    FShowAccelChar: Boolean;
    Procedure FontChange(Sender : TObject);
  protected
    Procedure DoAutoSize; Override;
    Procedure CMTextChanged(var Message: TLMSetText); message CM_TEXTCHANGED;

    Procedure SetAlignment(Value : TAlignment);
    Function GetAlignment : TAlignment;
    Procedure SetBorderStyle(Value : TStaticBorderStyle);
    Function GetBorderStyle : TStaticBorderStyle;
    Procedure SetShowAccelChar(Value : Boolean);
    Function GetShowAccelChar : Boolean;

    property Alignment: TAlignment read GetAlignment write SetAlignment;
    property BorderStyle: TStaticBorderStyle read GetBorderStyle write SetBorderStyle;
    property ShowAccelChar: Boolean read GetShowAccelChar write SetShowAccelChar;
  public
    constructor Create(AOwner: TComponent); override;
    Procedure Paint; override;
  end;

  TStaticText = class(TCustomStaticText)
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BorderStyle;
    property Caption;
    property Color;
    property Constraints;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

Function DeleteAmpersands(var Str : String) : Longint;

implementation

uses LCLLinux, Interfaces, Math, GraphicsMath;


type
   TSelection = record
      Startpos, EndPos: Integer;
   end;

   TMemoStrings = class(TStrings)
   private
      FMemo: TCustomMemo;
   protected
      function Get(Index : Integer): String; override;
      function GetCount: Integer; override;
   public
      constructor Create(AMemo: TCustomMemo);
      procedure Clear; override;
      procedure Delete(index : Integer); override;
      procedure Insert(index: Integer; const S: String); override;
   end;

 { TComboBoxStrings = class(TStrings)
  private
    ComboBox: TCustomComboBox;
  protected
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    function Add(const S: string): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    function IndexOf(const S: string): Integer; override;
    procedure Insert(Index: Integer; const S: string); override;
  end;
    }

const
  SScrollBarRange = 'ScrollBar property out of range';

Function DeleteAmpersands(var Str : String) : Longint;
var
  I : Integer;
  Tmp : String;
begin
  I := 1;
  Result := -1;
  SetLength(Tmp,0);
  While I <= Length(Str) do
    Case Str[I] of
      '&' :
         If I + 1 <= Length(Str) then begin
           If Str[I+1] = '&' then begin
             Inc(I,2);
             Tmp := Tmp + '&';
           end
           else begin
             If Result  < 0 then
               Result := Length(Tmp) + 1;
             Inc(I,1);
           end;
         end
         else
           Inc(I,1);
      else begin
        Tmp := Tmp + Str[I];
        Inc(I,1);
      end;
    end;
  SetLength(Str,0);
  Str := Tmp;
end;

{$IFDef NewCheckBox}
Procedure TCheckbox.DoAutoSize;
var
  R : TRect;
  DC : hDC;
begin
  If AutoSizing or not AutoSize then
    Exit;
  if (not HandleAllocated) or (csLoading in ComponentState) then exit;
  AutoSizing := True;
  DC := GetDC(Handle);
  Try
    R := Rect(0,0, Width, Height);
    DrawText(DC, PChar(Caption), Length(Caption), R,
      DT_CalcRect or DT_NOPrefix);
    If R.Right > Width then
      Width := R.Right + 25;
    If R.Bottom > Height then
      Height := R.Bottom + 2;
  Finally
    ReleaseDC(Handle, DC);
    AutoSizing := False;
  end;
end;

Function TCheckBox.GetChecked : Boolean;
begin
  Result := (State = cbChecked);
end;

Procedure TCheckBox.SetChecked(Value : Boolean);
begin
  If Value then
    State := cbChecked
  else
    State := cbUnchecked
end;

procedure TCheckBox.SetCheckBoxStyle(Value : TCheckBoxStyle);
begin
  FCheckBoxStyle := Value;
  Invalidate;
end;

procedure TCheckBox.SetAttachTextToBox(Value : Boolean);
begin
  FAttachTextToBox := Value;
  Invalidate;
end;

Procedure TCheckbox.SetAlignment(Value : TCBAlignment);
begin
  FAlignment := Value;
  Invalidate;
end;

Procedure TCheckbox.SetState(Value : TCheckBoxState);
begin
  If Value = cbGrayed then begin
    If AllowGrayed then
      FState := Value
    else
      FState := cbUnchecked;
  end
  else
    FState := Value;
  Invalidate;
end;

Procedure TCheckbox.CMMouseEnter(var Message: TLMMouse);
begin
  if not MouseInControl
  and Enabled and (GetCapture = 0)
  then begin
    FMouseInControl := True;
    Invalidate;
  end;
end;

procedure TCheckbox.CMMouseLeave(var Message: TLMMouse);
begin
  if MouseInControl
  and Enabled and (GetCapture = 0)
  and not MouseIsDragging
  then begin
    FMouseInControl := False;
    Invalidate;
  end;
end;

Procedure TCheckbox.WMMouseDown(var Message : TLMMouseEvent);
begin
  if Enabled then
    If not MouseInControl then
      FMouseInControl := True;
  if MouseInControl and Enabled then begin
    FMouseIsDragging := True;
    Invalidate;
  end;
end;

Procedure TCheckbox.WMMouseUp(var Message : TLMMouseEvent);
begin
  If MouseInControl and Enabled then begin
    FMouseIsDragging := False;
    Case State of
      cbUnchecked :
       begin
          If AllowGrayed then
            State := cbGrayed
          else
            State := cbChecked;
        end;
      cbGrayed :
        State := cbChecked;
      cbChecked :
        State := cbUnchecked;
    end;
    Click;
  end;
end;

Procedure TCheckbox.WMKeyDown(var Message : TLMKeyDown);
begin
  ControlState := ControlState -  [csClicked];
  Case Message.CharCode of
    32:
      begin
        FMouseInControl := True;
        Invalidate;
      end;
    27:
      If MouseInControl then begin
        FMouseInControl := False;
        Invalidate;
      end;
  end;
  Message.Result := 1
end;

Procedure TCheckbox.WMKeyUp(var Message : TLMKeyUp);
begin
  Case Message.CharCode of
    32:
      begin
        If MouseInControl then begin
          FMouseInControl := False;
          Case State of
            cbUnchecked :
              begin
                If AllowGrayed then
                  State := cbGrayed
                else
                  State := cbChecked;
              end;
            cbGrayed :
              State := cbChecked;
            cbChecked :
              State := cbUnchecked;
          end;
          Click;
        end;
      end;
  end;
  Message.Result := 1
end;

Procedure TCheckBox.PaintCheck(var PaintRect: TRect);

  Procedure DrawBorder(Highlight, Shadow : TColor; Rect : TRect; Down : Boolean);
  begin
    With Canvas, Rect do begin
      Pen.Style := psSolid;
      If Down then
        Pen.Color := shadow
      else
        Pen.Color := Highlight;
      MoveTo(Left, Top);
      LineTo(Right - 1,Top);
      MoveTo(Left, Top);
      LineTo(Left,Bottom - 1);
      If Down then
        Pen.Color := Highlight
      else
        Pen.Color := shadow;
      MoveTo(Left,Bottom - 1);
      LineTo(Right - 1,Bottom - 1);
      MoveTo(Right - 1, Top);
      LineTo(Right - 1,Bottom);
    end;
  end;

var
  FD1, FD2 : TPoint;
  BD1, BD2 : TPoint;
  APaintRect : TRect;
  DrawFlags : Longint;
begin
  If CheckBoxStyle <> cbsSystem then begin
    If (State = cbGrayed) or (not Enabled) then begin
      If (MouseInControl and MouseIsDragging) or (not Enabled) then
        Canvas.Brush.Color := clBtnFace
      else
        Canvas.Brush.Color := clBtnHighlight;
      Canvas.FillRect(CheckBoxRect);
      Canvas.Pen.Color := clBtnShadow;
    end
    else begin
      If MouseInControl and MouseIsDragging then
        Canvas.Brush.Color := clBtnFace
      else
        Canvas.Brush.Color := clWindow;
      Canvas.FillRect(CheckBoxRect);
      Canvas.Pen.Color := clWindowText;
    end;
    If State <> cbUnchecked then begin
      Case CheckBoxStyle of
        cbsCrissCross:
          begin
            Canvas.Pen.Width := 1;

            {Backward Diagonal}
              BD1 := Point(CheckBoxRect.Left + 3,CheckBoxRect.Top + 3);
              BD2 := Point(CheckBoxRect.Right - 3,CheckBoxRect.Bottom - 3);

              Canvas.MoveTo(BD1.X + 1, BD1.Y);
              Canvas.LineTo(BD2.X, BD2.Y - 1);{Top Line}
              Canvas.MoveTo(BD1.X, BD1.Y);
              Canvas.LineTo(BD2.X, BD2.Y);{Center Line}
              Canvas.MoveTo(BD1.X, BD1.Y + 1);
              Canvas.LineTo(BD2.X - 1, BD2.Y);{Bottom Line}

            {Forward Diagonal}
              FD1 := Point(CheckBoxRect.Left + 3,CheckBoxRect.Bottom - 4);
              FD2 := Point(CheckBoxRect.Right - 3,CheckBoxRect.Top + 2);

              Canvas.MoveTo(FD1.X, FD1.Y - 1);
              Canvas.LineTo(FD2.X - 1, FD2.Y);{Top Line}
              Canvas.MoveTO(FD1.X, FD1.Y);
              Canvas.LineTo(FD2.X, FD2.Y);{Center Line}
              Canvas.MoveTo(FD1.X + 1, FD1.Y);
              Canvas.LineTo(FD2.X, FD2.Y + 1);{Bottom Line}

            Canvas.Pen.Width := 0;
          end;
        cbsCheck:
          begin
            Canvas.Pen.Width := 1;

            {Short Diagonal}
              BD1 := Point(CheckBoxRect.Left + 4,CheckBoxRect.Bottom - 8);
              BD2 := Point(CheckBoxRect.Left + 4,CheckBoxRect.Bottom - 5);

              Canvas.MoveTO(BD1.X - 1, BD1.Y);
              Canvas.LineTo(BD2.X - 1, BD2.Y);{Left Line}
              Canvas.MoveTo(BD1.X, BD1.Y + 1);
              Canvas.LineTo(BD2.X, BD2.Y + 1);{Right Line}

            {Long Diagonal}
              FD1 := Point(CheckBoxRect.Left + 5,CheckBoxRect.Bottom - 6);
              FD2 := Point(CheckBoxRect.Right - 3,CheckBoxRect.Top + 2);

              Canvas.MoveTo(FD1.X,FD1.Y);
              Canvas.LineTo(FD2.X, FD2.Y);{Top Line}
              Canvas.MoveTo(FD1.X, FD1.Y + 1);
              Canvas.LineTo(FD2.X, FD2.Y + 1);{Center Line}
              Canvas.MoveTo(FD1.X, FD1.Y + 2);
              Canvas.LineTo(FD2.X, FD2.Y + 2);{Bottom Line}

            Canvas.Pen.Width := 0;
          end;
      end;
    end;
    DrawBorder(clBtnHighlight, clBtnShadow, CheckBoxRect, True);
    InflateRect(APaintRect, -1, -1);
    DrawBorder(clBtnFace, clBlack, APaintRect, True);
  end
  else begin
    DrawFlags:=DFCS_BUTTONPUSH + DFCS_FLAT;
    If MouseInControl and Enabled then
      Inc(DrawFlags,DFCS_CHECKED);
    DrawFrameControl(Canvas.Handle, PaintRect, DFC_BUTTON, DrawFlags);

    DrawFlags:=DFCS_BUTTONCHECK;
    if Checked or (State = cbGrayed) then inc(DrawFlags,DFCS_PUSHED);
    if not Enabled then inc(DrawFlags,DFCS_INACTIVE);
    If MouseInControl and Enabled then
      Inc(DrawFlags,DFCS_CHECKED);

    APaintRect := CheckBoxRect;
    DrawFrameControl(Canvas.Handle, APaintRect, DFC_BUTTON, DrawFlags);
  end;
end;

Procedure TCheckBox.PaintText(var PaintRect: TRect);
var
  Sz : Integer;
  AR : TRect;
  dish, dis : TColor;

  Procedure DoDrawText(theRect : TRect);
  var
    TextStyle : TTextStyle;
  begin
    With TextStyle do begin
      Layout     := tlCenter;
      SingleLine := False;
      Clipping   := True;
      ExpandTabs := False;
      ShowPrefix := False;
      Wordbreak  := Wordwrap;
      Opaque     := False;
      SystemFont := CheckBoxStyle = cbsSystem;
    end;

    Case Alignment of
      alLeftJustify:
        begin
          If not FAttachTextToBox then begin
            TextStyle.Alignment  := taLeftJustify;
          end
          else
            TextStyle.Alignment  := taRightJustify;
        end;
      alRightJustify:
        begin
          If not FAttachTextToBox then begin
            TextStyle.Alignment  := taRightJustify;
          end
          else
            TextStyle.Alignment  := taLeftJustify;
        end;
    end;
    Canvas.TextRect(theRect, 0, 0, Caption, TextStyle);
  end;

  Procedure DoDisabledTextRect(Rect : TRect; Highlight, Shadow : TColor);
  var
    FC : TColor;
  begin
    FC := Canvas.Font.Color;
    Canvas.Font.Color := Highlight;
    OffsetRect(Rect, 1, 1);
    DoDrawText(Rect);
    Canvas.Font.Color := Shadow;
    OffsetRect(Rect, -1, -1);
    DoDrawText(Rect);
    Canvas.Font.Color := FC;
  end;

begin
  If Caption = '' then
    exit;
  Sz := CheckBoxRect.Right - CheckBoxRect.Left;
  AR.Top := PaintRect.Top;
  AR.Bottom := PaintRect.Bottom;
  If Alignment = alRightJustify then begin
    AR.Left := PaintRect.Left + Sz + 6;
    AR.Right := PaintRect.Right;
  end
  else begin
    AR.Left := PaintRect.Left;
    AR.Right := PaintRect.Right - Sz - 6;
  end;
  dish := clBtnHighlight;
  dis := clBtnShadow;
  Canvas.Font := Self.Font;
  If Enabled then begin
    If CheckBoxStyle = cbsSystem then
      Canvas.Font.Color := clBtnText;
    DoDrawText(AR)
  end
  else
    DoDisabledTextRect(AR,dish,dis);
end;

procedure TCheckbox.Paint;
var
  PaintRect: TRect;
begin
  PaintRect := Rect(0, 0, Width, Height);
  Canvas.Color := clBtnFace;

  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(ClientRect);
  PaintCheck(PaintRect);
  PaintText(PaintRect);
end;

Constructor TCheckbox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  controlstyle := controlstyle - [csAcceptsControls];
  Alignment := alRightJustify;
  FAttachTextToBox := True
end;

Function TCheckBox.CheckBoxRect : TRect;
var
  Sz : Integer;
begin
  Sz := 13;
  Result.Top := (Height div 2) - (Sz div 2);
  Result.Bottom := Result.Top + Sz;
  If Alignment = alRightJustify then begin
    Result.Left := 2;
    Result.Right := Result.Left + Sz;
  end
  else begin
    Result.Right := Width - 2;
    Result.Left := Result.Right - Sz;
  end;
end;

procedure TCheckBox.Click;
begin
  If Assigned(OnClick) then
    OnClick(Self);
end;
{$EndIf NewCheckbox}

{$I customgroupbox.inc}
{$I customcombobox.inc}                                                                                            
{$I customlistbox.inc}
{$I custommemo.inc}
{$I customedit.inc}
{$I customlabel.inc}
{$I customcheckbox.inc}

{$I scrollbar.inc} 
{$I memo.inc}
{$I memostrings.inc}

{$I edit.inc}
{$I buttoncontrol.inc}

{$IFNDef NewCheckBox}
  {$I checkbox.inc}
{$EndIf Not NewCheckbox}

{$I radiobutton.inc}
{$I togglebox.inc}

{ TCustomStaticText }
Procedure TCustomStaticText.DoAutoSize;
var
  R : TRect;
  DC : hDC;
begin
  If AutoSizing or not AutoSize then
    Exit;
  if (not HandleAllocated) or (csLoading in ComponentState) then exit;
  AutoSizing := True;
  DC := GetDC(Handle);
  Try
    R := Rect(0,0, Width, Height);
    If BorderStyle <> sbsNone then
      InflateRect(R, -2, -2);
    SelectObject(DC, Font.Handle);
    DrawText(DC, PChar(Caption), Length(Caption), R,
      DT_CalcRect or DT_NoPrefix or DT_WordBreak);
    If BorderStyle <> sbsNone then
      InflateRect(R, 2, 2);

    Width := R.Right - R.Left;
    Height := R.Bottom - R.Top;
  Finally
    ReleaseDC(Handle, DC);
    AutoSizing := False;
  end;
end;

Procedure TCustomStaticText.SetAlignment(Value : TAlignment);
begin
  If FAlignment <> Value then begin
    FAlignment := Value;
    Invalidate;
  end;
end;

Function TCustomStaticText.GetAlignment : TAlignment;
begin
  Result := FAlignment;
end;

Procedure TCustomStaticText.SetBorderStyle(Value : TStaticBorderStyle);
begin
  If FBorderStyle <> Value then begin
    FBorderStyle := Value;
    Invalidate;
  end;
end;

Function TCustomStaticText.GetBorderStyle : TStaticBorderStyle;
begin
  Result := FBorderStyle;
end;

Procedure TCustomStaticText.SetShowAccelChar(Value : Boolean);
begin
  If FShowAccelChar <> Value then begin
    FShowAccelChar := Value;
    Invalidate;
  end;
end;

procedure TCustomStaticText.CMTextChanged(var Message: TLMSetText);
begin
  Invalidate;
end;

Function TCustomStaticText.GetShowAccelChar : Boolean;
begin
  Result := FShowAccelChar;
end;

constructor TCustomStaticText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Font.OnChange := @FontChange;
  ControlStyle := [csSetCaption, csOpaque, csClickEvents, csDoubleClicks, csReplicatable];
  Width := 65;
  Height := 17;
end;

Procedure TCustomStaticText.Paint;
var
  TR : TTextStyle;
  R : TRect;
begin
  R := Rect(0,0,Width,Height);
  Canvas.Color := Self.Color;
  Canvas.Font := Self.Font;
  With Canvas do begin
    FillRect(R);
    If BorderStyle <> sbsNone then begin
      InflateRect(R,-2,-2);
      Pen.Style := psSolid;
      If BorderStyle = sbsSunken then
        Pen.Color := clBtnShadow
      else
        Pen.Color := clBtnHighlight;
      MoveTo(0, 0);
      LineTo(Width - 1,0);
      MoveTo(0, 0);
      LineTo(0,Height - 1);
      If BorderStyle = sbsSunken then
        Pen.Color := clBtnHighlight
      else
        Pen.Color := clBtnShadow;
      MoveTo(0,Height - 1);
      LineTo(Width - 1,Height - 1);
      MoveTo(Width - 1, 0);
      LineTo(Width - 1,Height);
    end;
    FillChar(TR,SizeOf(TR),0);
    With TR do begin
      Alignment := Self.Alignment;
      WordBreak := True;
      Clipping := True;
      ShowPrefix := ShowAccelChar;
    end;
    TextRect(R, 0, 0, Caption, TR);
  end;
end;

Procedure TCustomStaticText.FontChange(Sender : TObject);
begin
  If Caption > '' then
    Invalidate;
end;

end.

{ =============================================================================

  $Log$
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


