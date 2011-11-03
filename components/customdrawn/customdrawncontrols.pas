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
  // fpimage
  fpcanvas, fpimgcanv, fpimage,
  // LCL -> Use only TForm, TWinControl, TCanvas, TLazIntfImage
  Graphics, Controls, LCLType, LCLIntf, IntfGraphics,
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
    // mouse
    procedure MouseEnter; override;
    procedure MouseLeave; override;
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

  // ===================================
  // Standard Tab
  // ===================================

  TCDButtonControl = class(TCDControl)
  private
  protected
    // keyboard
    procedure DoEnter; override;
    procedure DoExit; override;
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
    property Anchors;
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
    function GetControlId: TCDControlID; override;
    procedure CreateControlStateEx; override;
    procedure HandleCaretTimer(Sender: TObject);
    procedure DoDeleteSelection;
    procedure DoManageVisibleTextStart;
    function GetText: string;
    procedure SetText(AValue: string);
  protected
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
    property Color;
    property TabStop default True;
    property Text: string read GetText write SetText;
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
    property DrawStyle;
    property Caption;
    property TabStop default False;
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

  // ===================================
  // Common Controls Tab
  // ===================================

  {@@
    TCDTrackBar is a custom-drawn trackbar control
  }

  { TCDTrackBar }

  TCDTrackBar = class(TCDControl)
  private
    DragDropStarted: boolean;
    // fields
    FMin: integer;
    FMax: integer;
    FPosition: integer;
    FOnChange: TNotifyEvent;
    procedure SetMax(Value: integer);
    procedure SetMin(Value: integer);
    procedure SetPosition(Value: integer);
    //
    function GetPositionFromMousePos(X, Y: Integer): integer;
  protected
    FTBState: TCDTrackBarStateEx;
    function GetControlId: TCDControlID; override;
    procedure CreateControlStateEx; override;
    procedure PrepareControlStateEx; override;
    procedure Changed; virtual;
    // keyboard
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
    procedure MouseLeave; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //procedure Paint; override;
  published
    property Color;
    property DrawStyle;
    property Max: integer read FMax write SetMax default 10;
    property Min: integer read FMin write SetMin default 0;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Position: integer read FPosition write SetPosition;
    property TabStop default True;
  end;

  { TCDListView }

(*  TCDListView = class(TCDControl)
  private
    DragDropStarted: boolean;
    // fields
  protected
    // keyboard
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
    procedure MouseLeave; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
  published
    property Color;
    property TabStop default True;
  end;*)

  {TCDTabControl}

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
  FEditState.CaretIsVisible := not FEditState.CaretIsVisible;
  Invalidate;
end;

procedure TCDEdit.DoDeleteSelection;
begin
  FEditState.SelStart := 1;
  FEditState.SelLength := 0;
end;

procedure TCDEdit.DoManageVisibleTextStart;
var
  lText: String;
  lVisibleTextCharCount: Integer;
  lAvailableWidth: Integer;
begin
  // Moved to the left and we need to adjust the text start
  FEditState.VisibleTextStart := Min(FEditState.CaretPos+1, FEditState.VisibleTextStart);

  // Moved to the right and we need to adjust the text start
  lText := Copy(Text, FEditState.VisibleTextStart, Length(Text));
  lAvailableWidth := Width
   - FDrawer.GetMeasures(TCDEDIT_LEFT_TEXT_SPACING)
   - FDrawer.GetMeasures(TCDEDIT_RIGHT_TEXT_SPACING);
  lVisibleTextCharCount := Canvas.TextFitInfo(lText, lAvailableWidth);
  FEditState.VisibleTextStart := Max(FEditState.CaretPos-lVisibleTextCharCount, FEditState.VisibleTextStart);
end;

procedure TCDEdit.SetText(AValue: string);
begin
  Caption := AValue;
end;

procedure TCDEdit.DoEnter;
begin
  inherited DoEnter;

  FCaretTimer.Enabled := True;
  FEditState.CaretIsVisible := True;
  Invalidate;
end;

procedure TCDEdit.DoExit;
begin
  inherited DoExit;

  FCaretTimer.Enabled := False;
  FEditState.CaretIsVisible := False;
  Invalidate;
end;

procedure TCDEdit.KeyDown(var Key: word; Shift: TShiftState);
var
  lLeftText, lRightText, lOldText: String;
begin
  inherited KeyDown(Key, Shift);

  lOldText := Text;

  case Key of
  // Backspace
  VK_BACK:
  begin
    // Selection backspace
    if FEditState.SelLength > 0 then
      DoDeleteSelection()
    // Normal backspace
    else if FEditState.CaretPos > 0 then
    begin
      lLeftText := Copy(lOldText, 1, FEditState.CaretPos-1);
      lRightText := Copy(lOldText, FEditState.CaretPos+1, Length(lOldText));
      Text := lLeftText + lRightText;
      Dec(FEditState.CaretPos);
      DoManageVisibleTextStart();
      Invalidate;
    end;
  end;
  // DEL
  VK_DELETE:
  begin
    // Selection delete
    if FEditState.SelLength > 0 then
      DoDeleteSelection()
    // Normal delete
    else if FEditState.CaretPos < Length(lOldText) then
    begin
      lLeftText := Copy(lOldText, 1, FEditState.CaretPos);
      lRightText := Copy(lOldText, FEditState.CaretPos+2, Length(lOldText));
      Text := lLeftText + lRightText;
      Invalidate;
    end;
  end;
  VK_LEFT:
  begin
    if (FEditState.CaretPos > 0) then
    begin
      // Selecting to the left
      if ssShift in Shift then
      begin
        Dec(FEditState.SelLength);
        if FEditState.SelStart < 0 then FEditState.SelStart := FEditState.CaretPos;
      end
      // Normal move to the left
      else FEditState.SelLength := 0;

      Dec(FEditState.CaretPos);
      DoManageVisibleTextStart();
      FEditState.CaretIsVisible := True;
      Invalidate;
    end;
  end;
  VK_RIGHT:
  begin
    if FEditState.CaretPos < Length(lOldText) then
    begin
      // Selecting to the right
      if ssShift in Shift then
      begin
        Inc(FEditState.SelLength);
        if FEditState.SelStart < 0 then FEditState.SelStart := FEditState.CaretPos;
      end
      // Normal move to the right
      else FEditState.SelLength := 0;

      Inc(FEditState.CaretPos);
      DoManageVisibleTextStart();
      FEditState.CaretIsVisible := True;
      Invalidate;
    end;
  end;

  end; // case
end;

procedure TCDEdit.KeyUp(var Key: word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
end;

procedure TCDEdit.UTF8KeyPress(var UTF8Key: TUTF8Char);
var
  lLeftText, lRightText, lOldText: String;
begin
  inherited UTF8KeyPress(UTF8Key);

  // LCL Carbon sends Backspace as a UTF-8 Char
  // Don't handle it here because it is already handled in KeyDown
  if UTF8Key = #8 then Exit;

  DoDeleteSelection;

  // Normal characters
  lOldText := Text;
  lLeftText := Copy(lOldText, 1, FEditState.CaretPos);
  lRightText := Copy(lOldText, FEditState.CaretPos+1, Length(lOldText));
  Text := lLeftText + UTF8Key + lRightText;
  Inc(FEditState.CaretPos);
  FEditState.CaretIsVisible := True;
  Invalidate;
end;

procedure TCDEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCDEdit.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  inherited MouseMove(Shift, X, Y);
end;

procedure TCDEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
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
  Width := 100;
  Height := 30;
  TabStop := True;
  ControlStyle := [csCaptureMouse, csClickEvents,
    csDoubleClicks, csReplicatable];

  // State information
  FEditState.VisibleTextStart := 1;
  FEditState.SelStart := -1;

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
  ControlStyle := [csCaptureMouse, csClickEvents,
    csDoubleClicks, csReplicatable];
  AutoSize := True;

  PrepareCurrentDrawer();
end;

destructor TCDCheckBox.Destroy;
begin
  inherited Destroy;
end;

{ TCDCustomTabControl }

procedure TCDCustomTabControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  i: Integer;
  CurPage: TCDTabSheet;
  CurStartLeftPos: Integer = 0;
  VisiblePagesStarted: Boolean = False;
  lTabWidth: Integer;
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
      if (X > CurStartLeftPos) and
        (X < CurStartLeftPos + lTabWidth) and
        (Y < FDrawer.GetMeasuresEx(Canvas, TCDCTABCONTROL_TAB_HEIGHT, FState, FTabCState)) then
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
  if Parent <> nil then FStateEx.ParentRGBColor := Parent.GetRGBBackgroundColor
  else FStateEx.ParentRGBColor := clSilver;

  if Color = clDefault then FStateEx.RGBColor := FDrawer.GetControlColor(GetControlId())
  else FStateEx.RGBColor := GetRGBBackgroundColor;

  FStateEx.Caption := Caption;
  FStateEx.Font := Font;
  FStateEx.AutoSize := AutoSize;
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

procedure TCDButtonControl.DoEnter;
begin
  Invalidate;

  inherited DoEnter;
end;

procedure TCDButtonControl.DoExit;
begin
  Invalidate;

  inherited DoExit;
end;

procedure TCDButtonControl.KeyDown(var Key: word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);

  if (Key = VK_SPACE) or (Key = VK_RETURN) then
    DoButtonDown();
end;

procedure TCDButtonControl.KeyUp(var Key: word; Shift: TShiftState);
begin
  if (Key = VK_SPACE) or (Key = VK_RETURN) then
    DoButtonUp();

  inherited KeyUp(Key, Shift);
end;

procedure TCDButtonControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if not Focused then
    SetFocus;
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
end;

procedure TCDButtonControl.RealSetText(const Value: TCaption);
begin
  inherited RealSetText(Value);
  Invalidate;
end;

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
  //Color := clTeal;
  ParentFont := True;
  Color := $00F1F5F5;
  PrepareCurrentDrawer();
end;

destructor TCDButton.Destroy;
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
  Invalidate;
end;

constructor TCDGroupBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 100;
  Height := 100;
  TabStop := False;
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csDoubleClicks, csReplicatable];
end;

destructor TCDGroupBox.Destroy;
begin
  inherited Destroy;
end;

{ TCDTrackBar }

procedure TCDTrackBar.SetMax(Value: integer);
begin
  if Value = FMax then
    Exit;
  FMax := Value;
  Invalidate;
end;

procedure TCDTrackBar.SetMin(Value: integer);
begin
  if Value = FMin then
    Exit;
  FMin := Value;
  Invalidate;
end;

procedure TCDTrackBar.SetPosition(Value: integer);
begin
  if Value = FPosition then Exit;
  FPosition := Value;
  Invalidate;
end;

function TCDTrackBar.GetPositionFromMousePos(X, Y: integer): integer;
var
  lLeftBorder, lRightBorder: Integer;
begin
  lLeftBorder := FDrawer.GetMeasures(TCDTRACKBAR_LEFT_SPACING);
  lRightBorder := FDrawer.GetMeasures(TCDTRACKBAR_RIGHT_SPACING);

  if X > Width - lRightBorder then Result := FMax
  else if X < lLeftBorder then Result := FMin
  else Result := FMin + (X - lLeftBorder) * (FMax - FMin + 1) div (Width - lRightBorder - lLeftBorder);

  // sanity check
  if Result > FMax then Result := FMax;
  if Result < FMin then Result := FMin;
end;

function TCDTrackBar.GetControlId: TCDControlID;
begin
  Result := cidTrackBar;
end;

procedure TCDTrackBar.CreateControlStateEx;
begin
  FTBState := TCDTrackBarStateEx.Create;
  FStateEx := FTBState;
end;

procedure TCDTrackBar.PrepareControlStateEx;
begin
  inherited PrepareControlStateEx;
  FTBState.Min := FMin;
  FTBState.Max := FMax;
  FTBState.Position := FPosition;
end;

procedure TCDTrackBar.Changed;
begin

end;

procedure TCDTrackBar.DoEnter;
begin
  inherited DoEnter;
end;

procedure TCDTrackBar.DoExit;
begin
  inherited DoExit;
end;

procedure TCDTrackBar.KeyDown(var Key: word; Shift: TShiftState);
var
  NewPosition: Integer;
begin
  inherited KeyDown(Key, Shift);
  if (Key = 37) or (Key = 40) then
    NewPosition := FPosition - (FMax - FMin) div 10;
  if (Key = 38) or (Key = 39) then
    NewPosition := FPosition + (FMax - FMin) div 10;

  // sanity check
  if NewPosition > FMax then NewPosition := FMax;
  if NewPosition < FMin then NewPosition := FMin;

  Position := NewPosition;
end;

procedure TCDTrackBar.KeyUp(var Key: word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
end;

procedure TCDTrackBar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
var
  NewPosition: Integer;
begin
  SetFocus;

  NewPosition := GetPositionFromMousePos(X, Y);

  DragDropStarted := True;

  Position := NewPosition;

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCDTrackBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  DragDropStarted := False;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCDTrackBar.MouseMove(Shift: TShiftState; X, Y: integer);
var
  NewPosition: Integer;
begin
  if DragDropStarted then
  begin
    NewPosition := GetPositionFromMousePos(X, Y);
    Position := NewPosition;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TCDTrackBar.MouseEnter;
begin
  inherited MouseEnter;
end;

procedure TCDTrackBar.MouseLeave;
begin
  inherited MouseLeave;
end;

constructor TCDTrackBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Height := 25;
  Width := 100;

  DrawStyle := dsExtra1;
  PrepareCurrentDrawer();

  FMax := 10;
  FMin := 0;
  TabStop := True;
end;

destructor TCDTrackBar.Destroy;
begin
  inherited Destroy;
end;

{procedure TCDTrackBar.Paint;
var
  AImage: TLazIntfImage = nil;
  ABmp: TBitmap = nil;
  lCanvas: TFPImageCanvas = nil;
begin
  ABmp := TBitmap.Create;
  try
    ABmp.Width := Width;
    ABmp.Height := Height;
    AImage := ABmp.CreateIntfImage;
    lCanvas := TFPImageCanvas.Create(AImage);
    // First step of the drawing: FCL TFPCustomCanvas for fast pixel access
    FCurrentDrawer.DrawToIntfImage(lCanvas, AImage, Self);
    ABmp.LoadFromIntfImage(AImage);
    Canvas.Draw(0, 0, ABmp);
  finally
    if lCanvas <> nil then
      lCanvas.Free;
    if AImage <> nil then
      AImage.Free;
    ABmp.Free;
  end;
end;}

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
  if (NewParent <> nil) and (NewParent is TCDPageControl) then
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

