{
  Copyright (C) 2011 Felipe Monteiro de Carvalho

  License: The same modifying LGPL with static linking exception as the LCL

  This unit should be a repository for various custom drawn components,
  such as a custom drawn version of TButton, of TEdit, of TPageControl, etc,
  eventually forming a full set of custom drawn components.
}
unit customdrawncontrols;

{$mode objfpc}{$H+}

interface

uses
  // FPC
  Classes, SysUtils, contnrs, Math, types,
  // LazUtils
  lazutf8,
  // LCL -> Use only TForm, TWinControl, TCanvas, TLazIntfImage
  Graphics, Controls, LCLType, LCLIntf, LCLMessageGlue,
  LMessages, Messages, LCLProc, Forms,
  // Other LCL units are only for types
  StdCtrls, ExtCtrls, ComCtrls,
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
    //
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
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

  TCDButtonControl = class(TCDControl)
  private
  protected
    // This fields are set by descendents
    FHasOnOffStates: Boolean;
    FHasPartiallyOnState: Boolean;
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
    procedure DoButtonDown(); virtual;
    procedure DoButtonUp(); virtual;
    procedure RealSetText(const Value: TCaption); override;
  public
    //property Down: Boolean read GetDown write SetDown;
  end;

  { TCDButton }

  TCDButton = class(TCDButtonControl)
  private
  protected
    function GetControlId: TCDControlID; override;
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

  { TCDEdit }

  TCDEdit = class(TCDControl)
  private
    DragDropStarted: boolean;
    FCaretTimer: TTimer;
    FEditState: TCDEditStateEx; // Points to the same object as FStateEx, so don't Free!
    procedure HandleCaretTimer(Sender: TObject);
    procedure DoDeleteSelection;
    procedure DoClearSelection;
    procedure DoManageVisibleTextStart;
    function GetText: string;
    procedure SetText(AValue: string);
    function MousePosToCaretPos(X, Y: Integer): TPoint;
    function IsSomethingSelected: Boolean;
  protected
    function GetControlId: TCDControlID; override;
    procedure CreateControlStateEx; override;
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
  published
    property Align;
    property Anchors;
    property AutoSize;
    property Color;
    property DrawStyle;
    property TabStop default True;
    property Text: string read GetText write SetText;
  end;

  { TCDCheckBox }

  TCDCheckBox = class(TCDButtonControl)
  private
    FAllowGrayed: Boolean;
    FCheckedState: TCheckBoxState;
  protected
    procedure DoButtonUp(); override;
    function GetControlId: TCDControlID; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed default False;
    property DrawStyle;
    property Caption;
    property TabStop default True;
    property State: TCheckBoxState read FCheckedState write FCheckedState default cbUnchecked;
  end;

  { TCDRadioButton }

  TCDRadioButton = class(TCDButtonControl)
  private
    function GetChecked: Boolean;
    procedure SetChecked(AValue: Boolean);
  protected
    function GetControlId: TCDControlID; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Checked: Boolean read GetChecked write SetChecked default False;
    property DrawStyle;
    property Caption;
    property TabStop default True;
  end;

  { TCDPositionedControl }

  TCDPositionedControl = class(TCDControl)
  private
    DragDropStarted: boolean;
    FButton: TCDControlState; // the button currently being clicked
    FBtnClickTimer: TTimer;
    // fields
    FMax: Integer;
    FMin: Integer;
    FOnChange: TNotifyEvent;
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
    function GetPositionFromMousePosWithMargins(X, Y, ALeftMargin, ARightMargin: Integer;
       AIsHorizontal, AAcceptMouseOutsideStrictArea: Boolean): integer;
    function GetPositionFromMousePos(X, Y: Integer): integer; virtual; abstract;
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
  published
    property Max: Integer read FMax write SetMax;
    property Min: Integer read FMin write SetMin;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Position: Integer read FPosition write SetPosition;
  end;

  { TCDScrollBar }

  TCDScrollBar = class(TCDPositionedControl)
  private
    FKind: TScrollBarKind;
    procedure SetKind(AValue: TScrollBarKind);
  protected
    function GetPositionFromMousePos(X, Y: Integer): integer; override;
    function GetButtonFromMousePos(X, Y: Integer): TCDControlState; override;
    function GetControlId: TCDControlID; override;
    procedure PrepareControlState; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DrawStyle;
    property Kind: TScrollBarKind read FKind write SetKind;
    property PageSize;
    property TabStop default True;
  end;

  {@@
    TCDGroupBox is a custom-drawn group box control
  }

  { TCDGroupBox }

  TCDGroupBox = class(TCDControl)
  private
    function GetControlId: TCDControlID; override;
  protected
    procedure RealSetText(const Value: TCaption); override; // to update on caption changes
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AutoSize;
    property DrawStyle;
    property Caption;
    property TabStop default False;
  end;

  // ===================================
  // Additional Tab
  // ===================================

  { TCDStaticText }

  TCDStaticText = class(TCDControl)
  private
    function GetControlId: TCDControlID; override;
  protected
    procedure RealSetText(const Value: TCaption); override; // to update on caption changes
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DrawStyle;
    property Caption;
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
    function GetControlId: TCDControlID; override;
    procedure PrepareControlState; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //procedure Paint; override;
  published
    property Color;
    property DrawStyle;
    property Orientation: TTrackBarOrientation read FOrientation write SetOrientation default trHorizontal;
    property TabStop default True;
  end;

  { TCDProgressBar }

  TCDProgressBar = class(TCDControl)
  private
    DragDropStarted: boolean;
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
    property Max: integer read FMax write SetMax default 10;
    property Min: integer read FMin write SetMin default 0;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Orientation: TProgressBarOrientation read FOrientation write SetOrientation;// default prHorizontal;
    property Position: integer read FPosition write SetPosition;
    property Smooth: Boolean read FSmooth write SetSmooth;
    property Style: TProgressBarStyle read FStyle write SetStyle;
  end;

  { TCDListView }

  TCDListView = class(TCDScrollableControl)
  private
    DragDropStarted: boolean;
    // fields
    FColumns: TListColumns;
    FIconOptions: TIconOptions;
    FListItems: TCDListItems;
    FProperties: TListViewProperties;
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
    //property GridLines: Boolean index Ord(lvpGridLines) read GetProperty write SetProperty default False;
    property Items: TCDListItems read FListItems;
    property ScrollBars;
    property ShowColumnHeader: Boolean read FShowColumnHeader write SetShowColumnHeader default True;
    property ViewStyle: TViewStyle read FViewStyle write SetViewStyle default vsList;
  end;

  { TCDTabControl }

  { TCDCustomTabControl }

  TCDCustomTabControl = class;

  { TCDCustomTabSheet }

  { TCDTabSheet }

  TCDTabSheet = class(TCustomControl)
  private
    CDTabControl: TCDCustomTabControl;
    FTabVisible: Boolean;
  protected
    procedure RealSetText(const Value: TCaption); override; // to update on caption changes
    procedure SetParent(NewParent: TWinControl); override; // For being created by the LCL resource reader
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

  TCDCustomTabControl = class(TCDControl)
  private
    FTabIndex: Integer;
    FTabs: TStringList;
    FOnChanging: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOptions: TNoteBookOptions;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    //procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    //procedure MouseEnter; override;
    //procedure MouseLeave; override;
    procedure SetTabIndex(AValue: Integer); virtual;
    procedure SetTabs(AValue: TStringList);
    procedure SetOptions(AValue: TNoteBookOptions);
  protected
    FTabCState: TCDCTabControlStateEx;
    function GetControlId: TCDControlID; override;
    procedure CreateControlStateEx; override;
    procedure PrepareControlStateEx; override;
    procedure CorrectTabIndex();
    property Options: TNoteBookOptions read FOptions write SetOptions;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetTabCount: Integer;
    property Tabs: TStringList read FTabs write SetTabs;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property TabIndex: integer read FTabIndex write SetTabIndex;
  end;

//  TTabSelectedEvent = procedure(Sender: TObject; ATab: TTabItem;
//    ASelected: boolean) of object;

  TCDTabControl = class(TCDCustomTabControl)
  published
    property Color;
    property Font;
    property Tabs;
    property TabIndex;
    property OnChanging;
    property OnChange;
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
    property Font;
    property PageIndex: integer read GetPageIndex write SetPageIndex;
    property Options;
    property ParentColor;
    property ParentFont;
    property TabStop default True;
    property TabIndex;
    property OnChanging;
    property OnChange;
  end;

implementation

resourcestring
  sTABSHEET_DEFAULT_NAME = 'CTabSheet';

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
  PrepareCurrentDrawer();
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
  inherited Destroy;
end;

{ TCDControl }

procedure TCDControl.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  PrepareControlState;
  PrepareControlStateEx;
  FDrawer.CalculatePreferredSize(Canvas, GetControlId(), FState, FStateEx,
    PreferredWidth, PreferredHeight, WithThemeSpace);
end;

procedure TCDControl.PrepareCurrentDrawer;
begin
  FDrawer := GetDrawer(FDrawStyle);
  if FDrawer = nil then FDrawer := GetDrawer(dsCommon); // avoid exceptions in the object inspector if an invalid drawer is selected
  if FDrawer = nil then raise Exception.Create('No registered drawers were found');
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

  if Color = clDefault then FStateEx.RGBColor := FDrawer.GetControlDefaultColor(GetControlId())
  else FStateEx.RGBColor := GetRGBColorResolvingParent;

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
var
  ABmp: TBitmap;
  lSize: TSize;
  lControlId: TCDControlID;
begin
  inherited Paint;

  PrepareCurrentDrawer();

  ABmp := TBitmap.Create;
  try
    ABmp.Width := Width;
    ABmp.Height := Height;
    lSize := Size(Width, Height);
    lControlId := GetControlId();
    PrepareControlState;
    PrepareControlStateEx;
    FDrawer.DrawControl(ABmp.Canvas, Point(0, 0),
      lSize, lControlId, FState, FStateEx);
    Canvas.Draw(0, 0, ABmp);
  finally
    ABmp.Free;
  end;
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
  SetFocus();
end;

constructor TCDControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateControlStateEx;
end;

destructor TCDControl.Destroy;
begin
  FStateEx.Free;
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

procedure TCDButtonControl.DoButtonDown();
begin
  if not (csfSunken in FState) then
  begin
    FState := FState + [csfSunken];
    Invalidate;
  end;
end;

procedure TCDButtonControl.DoButtonUp();
begin
  if csfSunken in FState then
  begin
    FState := FState - [csfSunken];
    Invalidate;
  end;
  // Only for buttons with checked/down states
  if FHasOnOffStates then
  begin
    if FHasPartiallyOnState then
    begin
      if csfOn in FState then
        FState := FState + [csfOff] - [csfOn, csfPartiallyOn]
      else if csfPartiallyOn in FState then
        FState := FState + [csfOn] - [csfOff, csfPartiallyOn]
      else
        FState := FState + [csfPartiallyOn] - [csfOn, csfOff];
    end
    else
    begin
      if csfOn in FState then
        FState := FState + [csfOff] - [csfOn]
      else
        FState := FState + [csfOn] - [csfOff];
    end;
  end;
end;

procedure TCDButtonControl.RealSetText(const Value: TCaption);
begin
  inherited RealSetText(Value);
  Invalidate;
end;

{ TCDEdit }

function TCDEdit.GetText: string;
begin
  Result := Caption;
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
    lControlText := Text;

    // Text left of the selection
    lTextLeft := UTF8Copy(lControlText, FEditState.VisibleTextStart.X, lSelLeftPos-FEditState.VisibleTextStart.X+1);

    // Text right of the selection
    lTextRight := UTF8Copy(lControlText, lSelLeftPos+lSelLength+1, Length(lControlText));

    // Execute the deletion
    Text := lTextLeft + lTextRight;

    // Correct the caret position
    FEditState.CaretPos.X := Length(lTextLeft);
  end;

  DoClearSelection;
end;

procedure TCDEdit.DoClearSelection;
begin
  FEditState.SelStart.X := 1;
  FEditState.SelLength := 0;
end;

procedure TCDEdit.DoManageVisibleTextStart;
var
  lText: String;
  lVisibleTextCharCount: Integer;
  lAvailableWidth: Integer;
begin
  // Moved to the left and we need to adjust the text start
  FEditState.VisibleTextStart.X := Min(FEditState.CaretPos.X+1, FEditState.VisibleTextStart.X);

  // Moved to the right and we need to adjust the text start
  lText := UTF8Copy(Text, FEditState.VisibleTextStart.X, Length(Text));
  lAvailableWidth := Width
   - FDrawer.GetMeasures(TCDEDIT_LEFT_TEXT_SPACING)
   - FDrawer.GetMeasures(TCDEDIT_RIGHT_TEXT_SPACING);
  lVisibleTextCharCount := Canvas.TextFitInfo(lText, lAvailableWidth);
  FEditState.VisibleTextStart.X := Max(FEditState.CaretPos.X-lVisibleTextCharCount, FEditState.VisibleTextStart.X);
end;

procedure TCDEdit.SetText(AValue: string);
begin
  Caption := AValue;
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
  Canvas.Font := Font;
  lVisibleStr := UTF8Copy(Text, FEditState.VisibleTextStart.X, Length(Text));
  lStrLen := UTF8Length(lVisibleStr);
  lPos := FDrawer.GetMeasures(TCDEDIT_LEFT_TEXT_SPACING);
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

  lOldText := Text;
  lOldTextLength := UTF8Length(Text);

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
      Text := lLeftText + lRightText;
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
      Text := lLeftText + lRightText;
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
    end;
  end;

  else
    lKeyWasProcessed := False;
  end; // case

  if lKeyWasProcessed then FEditState.EventArrived := True;
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

  // LCL-Carbon sends Backspace as a UTF-8 Char
  // LCL-Qt sends arrow left,right,up,down (#28..#31), <enter>, ESC, etc
  // Don't handle any non-char keys here because they are already handled in KeyDown
  if UTF8Key[1] in [#0..#31] then Exit;

  DoDeleteSelection;

  // Normal characters
  lOldText := Text;
  lLeftText := UTF8Copy(lOldText, 1, FEditState.CaretPos.X);
  lRightText := UTF8Copy(lOldText, FEditState.CaretPos.X+1, UTF8Length(lOldText));
  Text := lLeftText + UTF8Key + lRightText;
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
  ControlStyle := ControlStyle - [csAcceptsControls];

  // State information
  FEditState.VisibleTextStart := Point(1, 1);

  // Caret code
  FCaretTimer := TTimer.Create(Self);
  FCaretTimer.OnTimer := @HandleCaretTimer;
  FCaretTimer.Interval := 500;
  FCaretTimer.Enabled := False;

  PrepareCurrentDrawer();
end;

destructor TCDEdit.Destroy;
begin
  inherited Destroy;
end;

{ TCDCheckBox }

procedure TCDCheckBox.DoButtonUp;
begin
  inherited DoButtonUp;

  if AllowGrayed then
  begin
    case FCheckedState of
    cbUnchecked: FCheckedState := cbGrayed;
    cbGrayed: FCheckedState := cbChecked;
    else
      FCheckedState := cbUnchecked;
    end;
  end
  else
  begin
    if FCheckedState in [cbUnchecked, cbGrayed] then FCheckedState := cbChecked
    else FCheckedState := cbUnchecked;
  end;

  Invalidate;
end;

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

  PrepareCurrentDrawer();
end;

destructor TCDCheckBox.Destroy;
begin
  inherited Destroy;
end;

{ TCDButton }

function TCDButton.GetControlId: TCDControlID;
begin
  Result := cidButton;
end;

constructor TCDButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TabStop := True;
  Width := 75;
  Height := 25;
  ParentFont := True;
  PrepareCurrentDrawer();
end;

destructor TCDButton.Destroy;
begin
  inherited Destroy;
end;

{ TCDRadioButton }

procedure TCDRadioButton.SetChecked(AValue: Boolean);
begin
  if (AValue and (csfOn in FState)) or
   ((not AValue) and (csfOff in FState)) then Exit;

  if AValue then FState := FState + [csfOn] - [csfOff]
  else FState := FState + [csfOff] - [csfOn];

  Invalidate;
end;

function TCDRadioButton.GetChecked: Boolean;
begin
  Result := csfOn in FState;
end;

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

  PrepareCurrentDrawer();
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
begin
  if FPosition=AValue then Exit;
  FPosition:=AValue;

  if FPosition > FMax then FPosition := FMax;
  if FPosition < FMin then FPosition := FMin;

  // Don't do OnChange during loading
  if not (csLoading in ComponentState) then
  begin
    if Assigned(OnChange) then OnChange(Self);
    Invalidate;
  end;
end;

procedure TCDPositionedControl.DoClickButton(AButton: TCDControlState; ALargeChange: Boolean);
var
  lChange: Integer;
begin
  if ALargeChange then lChange := FLargeChange
  else lChange := FSmallChange;
  if csfLeftArrow in AButton then Position := Position - lChange
  else if csfRightArrow in AButton then Position := Position + lChange;
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
    if AAcceptMouseOutsideStrictArea then Result := FMax
    else Exit;
  end
  else if lCoord < ALeftMargin then
  begin
    if AAcceptMouseOutsideStrictArea then Result := FMin
    else Exit;
  end
  else Result := FMin + (lCoord - ALeftMargin) * (FMax - FMin + 1) div (lSize - ARightMargin - ALeftMargin);

  // sanity check
  if Result > FMax then Result := FMax;
  if Result < FMin then Result := FMin;
end;

function TCDPositionedControl.GetButtonFromMousePos(X, Y: Integer): TCDControlState;
begin
  Result := [];
end;

procedure TCDPositionedControl.CreateControlStateEx;
begin
  inherited CreateControlStateEx;
  FPCState := TCDPositionedCStateEx.Create;
  FStateEx := FPCState;
end;

procedure TCDPositionedControl.PrepareControlStateEx;
begin
  inherited PrepareControlStateEx;

  if FMin < FMax then FPCState.FloatPos := FPosition / (FMax - FMin)
  else FPCState.FloatPos := 0.0;

  FPCState.PosCount := FMax - FMin + 1;
  FPCState.Position := FPosition - FMin;

  if FMin < FMax then FPCState.FloatPageSize := FPageSize / (FMax - FMin)
  else FPCState.FloatPageSize := 1.0;
end;

procedure TCDPositionedControl.KeyDown(var Key: word; Shift: TShiftState);
var
  NewPosition: Integer;
begin
  inherited KeyDown(Key, Shift);

  if (Key = VK_LEFT) or (Key = VK_DOWN) then
    NewPosition := FPosition - FSmallChange;
  if (Key = VK_UP) or (Key = VK_RIGHT) then
    NewPosition := FPosition + FSmallChange;
  if (Key = VK_PRIOR) then
    NewPosition := FPosition - FLargeChange;
  if (Key = VK_NEXT) then
    NewPosition := FPosition + FLargeChange;

  // sanity check
  if NewPosition >= 0 then
  begin
    if NewPosition > FMax then NewPosition := FMax;
    if NewPosition < FMin then NewPosition := FMin;

    Position := NewPosition;
  end;
end;

procedure TCDPositionedControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  NewPosition: Integer;
begin
  SetFocus;
  NewPosition := GetPositionFromMousePos(X, Y);
  DragDropStarted := True;
  if NewPosition >= 0 then Position := NewPosition;

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
  NewPosition: Integer;
begin
  if DragDropStarted then
  begin
    NewPosition := GetPositionFromMousePos(X, Y);
    if NewPosition > 0 then Position := NewPosition;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TCDPositionedControl.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  DragDropStarted := False;
  FBtnClickTimer.Enabled := False;
  FState := FState - [csfLeftArrow, csfRightArrow];
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

{ TCDScrollBar }

procedure TCDScrollBar.SetKind(AValue: TScrollBarKind);
begin
  if FKind=AValue then Exit;
  FKind:=AValue;

  if not (csLoading in ComponentState) then Invalidate;
end;

function TCDScrollBar.GetPositionFromMousePos(X, Y: Integer): integer;
var
  lLeftBorder, lRightBorder: Integer;
begin
  lLeftBorder := FDrawer.GetMeasures(TCDSCROLLBAR_LEFT_SPACING);
  lRightBorder := FDrawer.GetMeasures(TCDSCROLLBAR_RIGHT_SPACING);

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
end;

destructor TCDScrollBar.Destroy;
begin
  inherited Destroy;
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

  PrepareCurrentDrawer();

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
var
  lOldWidth: Integer;
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
  PrepareCurrentDrawer();
end;

destructor TCDProgressBar.Destroy;
begin
  inherited Destroy;
end;

{ TCDListView }

function TCDListView.GetProperty(AIndex: Integer): Boolean;
begin

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

  PrepareCurrentDrawer();

  ScrollBars := ssBoth;
end;

destructor TCDListView.Destroy;
begin
  FColumns.Free;
  FListItems.Free;
  inherited Destroy;
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
  if (csLoading in ComponentState) and
    (NewParent <> nil) and (NewParent is TCDPageControl) then
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
  i: Integer;
  CurPage: TCDTabSheet;
  CurStartLeftPos: Integer = 0;
  VisiblePagesStarted: Boolean = False;
  lTabWidth, lTabHeight: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);

  for i := 0 to Tabs.Count - 1 do
  begin
    if i = FTabCState.LeftmostTabVisibleIndex then
      VisiblePagesStarted := True;

    if VisiblePagesStarted then
    begin
      FTabCState.TabIndex := i;
      lTabWidth := FDrawer.GetMeasuresEx(Canvas, TCDCTABCONTROL_TAB_WIDTH, FState, FTabCState);
      lTabHeight := FDrawer.GetMeasuresEx(Canvas, TCDCTABCONTROL_TAB_HEIGHT, FState, FTabCState);
      if (X > CurStartLeftPos) and
        (X < CurStartLeftPos + lTabWidth) and
        (Y < lTabHeight) then
      begin
        if Self is TCDPageControl then
          (Self as TCDPageControl).PageIndex := i
        else
          TabIndex := i;

        Exit;
      end;
      CurStartLeftPos := CurStartLeftPos + lTabWidth;
    end;
  end;
end;

procedure TCDCustomTabControl.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin

  inherited MouseUp(Button, Shift, X, Y);
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

procedure TCDCustomTabControl.SetOptions(AValue: TNoteBookOptions);
begin
  if FOptions=AValue then Exit;
  FOptions:=AValue;
  Invalidate;
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

  PrepareCurrentDrawer();

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
  if FTabIndex >= FTabs.Count then SetTabIndex(FTabIndex-1);

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
  lTabHeight, lIndex: Integer;
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

end.

