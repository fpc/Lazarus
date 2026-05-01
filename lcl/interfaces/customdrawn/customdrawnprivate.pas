unit customdrawnprivate;

{$mode objfpc}{$H+}

interface

uses
  // rtl+ftl
  Types, Classes, SysUtils,
  // LCL
  Controls, Graphics, stdctrls, extctrls, comctrls,
  customdrawnproc, customdrawncontrols, lcltype, lclproc, lclintf,
  lmessages, spin;

type

  // Standard Tab

  { TCDIntfButton }

  TCDIntfButton = class(TCDButton)
  protected
    procedure KeyUp(var Key: word; Shift: TShiftState); override;
    procedure PrepareControlState; override;
  public
    LCLControl: TCustomButton;
  end;

  { TCDIntfEdit }

  TCDIntfEdit = class(TCDEdit)
  protected
    // for descendents to override
    procedure DoChange; override;
    procedure PrepareControlState; override;
  public
    LCLControl: TCustomEdit;
  end;

  TCDIntfCheckBox = class(TCDCheckBox)
  protected
    procedure KeyUp(var Key: word; Shift: TShiftState); override;
    procedure PrepareControlState; override;
  public
    LCLControl: TCustomCheckBox;
  end;

  TCDIntfRadioButton = class(TCDRadioButton)
  protected
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure KeyUp(var Key: word; Shift: TShiftState); override;
    procedure DoButtonUp; override;
    procedure PrepareControlState; override;
  public
    LCLControl: TCustomCheckBox;
  end;

  TCDIntfComboBox = class(TCDComboBox)
  public
    LCLControl: TCustomComboBox;
  end;

  { TCDIntfListBox -- bridges typing / selection on the injected
    TCDListBox back to the LCL TCustomListBox so the LCL fires
    OnSelectionChange / LM_SELCHANGE etc. as user code expects. }
  TCDIntfListBox = class(TCDListBox)
  protected
    procedure DoSelectionChange(Sender: TObject);
    procedure PrepareControlState; override;
  public
    LCLControl: TCustomListBox;
    constructor Create(AOwner: TComponent); override;
  end;

  TCDIntfScrollBar = class(TCDScrollBar)
  public
    LCLControl: TCustomScrollBar;
  end;

  // Additional Tab

  TCDIntfStaticText = class(TCDStaticText)
  public
    LCLControl: TStaticText;
  end;

  // Common Controls Tab

  TCDIntfProgressBar = class(TCDProgressBar)
  public
    LCLControl: TCustomProgressBar;
  end;

  TCDIntfTrackBar = class(TCDTrackBar)
  public
    LCLControl: TCustomTrackBar;
  end;

  TCDIntfPageControl = class(TCDPageControl)
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
  public
    LCLControl: TCustomTabControl;
  end;

  // Misc Tab

  TCDIntfSpinEdit = class(TCDSpinEdit)
  protected
    procedure DoChange; override;
  public
    LCLControl: TCustomFloatSpinEdit;
  end;

// These are default message handlers which backends might use to simplify their code
// They convert a message sent to the form into a message to the correct sub-control
procedure CallbackMouseUp(AWindowHandle: TCDForm; x, y: Integer; Button: TMouseButton; ShiftState: TShiftState = []);
procedure CallbackMouseDown(AWindowHandle: TCDForm; x, y: Integer; Button: TMouseButton; ShiftState: TShiftState = []);
procedure CallbackMouseMove(AWindowHandle: TCDForm; x, y: Integer; ShiftState: TShiftState = []);
procedure CallbackKeyDown(AWindowHandle: TCDForm; AKey: Word);
procedure CallbackKeyUp(AWindowHandle: TCDForm; AKey: Word);
procedure CallbackKeyChar(AWindowHandle: TCDForm; AKeyData: Word; AChar: TUTF8Char);
function IsIntfControl(AControl: TWinControl): Boolean;

implementation

uses customdrawnint, LCLMessageGlue, customdrawndrawers, Clipbrd;

procedure CallbackMouseUp(AWindowHandle: TCDForm; x, y: Integer; Button: TMouseButton; ShiftState: TShiftState = []);
var
  lTarget: TWinControl;
  lEventPos: TPoint;
  lEventEndsInsideTheControl: Boolean;
begin
  lTarget := AWindowHandle.LastMouseDownControl;
  AWindowHandle.LastMouseDownControl := nil;
  if lTarget = nil then lTarget := FindControlWhichReceivedEvent(
    AWindowHandle.LCLForm, AWindowHandle.Children, x, y);
  lEventPos := FormPosToControlPos(lTarget, x, y);
  LCLSendMouseUpMsg(lTarget, lEventPos.x, lEventPos.y, Button, ShiftState);

  // Send a click only if the event ends inside the control
//  DebugLn(Format('CallbackMouseUp lEventPos X=%d y=%d lTarget CX=%d CY=%d',
//    [lEventPos.X, lEventPos.y, lTarget.Width, lTarget.Height]));
  lEventEndsInsideTheControl := (lEventPos.X >= 0) and (lEventPos.Y >= 0)
    and (lEventPos.X <= lTarget.Width) and (lEventPos.Y <= lTarget.Height);
  if lEventEndsInsideTheControl then LCLSendClickedMsg(lTarget);

  // If this is a interface control, send the message to the main LCL control too
  if IsIntfControl(lTarget) then
  begin
    lTarget := lTarget.Parent;
    LCLSendMouseUpMsg(lTarget, lEventPos.x, lEventPos.y, Button, ShiftState);
    if lEventEndsInsideTheControl then LCLSendClickedMsg(lTarget);
  end;

  // Form scrolling
  AWindowHandle.IsScrolling := False;
end;

procedure CallbackMouseDown(AWindowHandle: TCDForm; x, y: Integer; Button: TMouseButton; ShiftState: TShiftState = []);
var
  lTarget: TWinControl;
  lIntfTarget: TWinControl = nil;
  lEventPos: TPoint;
begin
{  // if mouse-click, focus-change, mouse-click, cursor hasn't moved:
  // simulate double click, assume focus change due to first mouse-click
  if (MouseDownFocusStatus = mfFocusChanged) and (MouseDownFocusWindow = Window)
      and (GetTickCount - MouseDownTime <= GetDoubleClickTime)
      and CheckMouseMovement then
  begin
    PostMessage(Window, WM_LBUTTONDBLCLK, WParam, LParam);
  end;

  MouseDownTime := GetTickCount;
  MouseDownWindow := Window;
  MouseDownFocusWindow := 0;
  MouseDownFocusStatus := mfFocusSense;
  GetCursorPos(MouseDownPos);}

  lTarget := FindControlWhichReceivedEvent(AWindowHandle.LCLForm, AWindowHandle.Children, x, y);
  //DebugLn(Format('CallbackMouseDown lEventPos X=%d y=%d lTarget %s:%s',
  //  [lEventPos.X, lEventPos.y, lTarget.Name, lTarget.ClassName]));
  AWindowHandle.LastMouseDownControl := lTarget;
  AWindowHandle.FocusedControl := lTarget;
  AWindowHandle.FocusedIntfControl := nil;
  lEventPos := FormPosToControlPos(lTarget, x, y);

  LCLSendMouseDownMsg(lTarget, lEventPos.x, lEventPos.y, Button, ShiftState);

  // If this is a interface control, send the message to the main LCL control too
  if IsIntfControl(lTarget) then
  begin
    lIntfTarget := lTarget;
    AWindowHandle.FocusedIntfControl := lTarget;
    lTarget := lTarget.Parent;

    LCLSendMouseDownMsg(lTarget, lEventPos.x, lEventPos.y, Button, ShiftState);
  end;

  // If the target is focusable, a mouse down will give it focus
  CDWidgetset.CDSetFocusToControl(lTarget, lIntfTarget);

  // Check if we are scrolling the form
  if lTarget = AWindowHandle.LCLForm then
  begin
    AWindowHandle.IsScrolling := True;
    AWindowHandle.LastMousePos := lEventPos;
  end;
end;

procedure CallbackMouseMove(AWindowHandle: TCDForm; x, y: Integer; ShiftState: TShiftState = []);
var
  lTarget: TWinControl;
  lEventPos: TPoint;
  lOldScrollY: Integer;
begin
  if AWindowHandle.LastMouseDownControl = nil then
    lTarget := FindControlWhichReceivedEvent(AWindowHandle.LCLForm, AWindowHandle.Children, x, y)
  else
    lTarget := AWindowHandle.LastMouseDownControl;

  //DebugLn(Format('[CallbackMouseMove] X=%d Y=%d Control=%s', [X, Y, lTarget.Name]));

  lEventPos := FormPosToControlPos(lTarget, x, y);
  LCLSendMouseMoveMsg(lTarget, lEventPos.x, lEventPos.y, ShiftState);

  // If this is a interface control, send the message to the main LCL control too
  if IsIntfControl(lTarget) then
  begin
    lTarget := lTarget.Parent;

    LCLSendMouseMoveMsg(lTarget, lEventPos.x, lEventPos.y, ShiftState);
  end;

  // form scrolling
  if AWindowHandle.IsScrolling then
  begin
    lOldScrollY := AWindowHandle.ScrollY;
    AWindowHandle.ScrollY := AWindowHandle.LastMousePos.Y - lEventPos.Y + AWindowHandle.ScrollY;
    AWindowHandle.SanityCheckScrollPos();
    if AWindowHandle.ScrollY <> lOldScrollY then LCLIntf.InvalidateRect(HWND(AWindowHandle), nil, False);
  end;
end;

function HandleIntfTextShortcut(AControl: TWinControl; AKey: Word): Boolean;
var
  LEdit: TCustomEdit;
  LCombo: TCustomComboBox;
  S: string;
begin
  Result := False;
  if GetKeyShiftState <> [ssModifier] then Exit;

  LEdit := nil;
  if AControl is TCDIntfEdit then
    LEdit := TCDIntfEdit(AControl).LCLControl
  else if AControl is TCDIntfSpinEdit then
    LEdit := TCDIntfSpinEdit(AControl).LCLControl;

  if LEdit <> nil then
  begin
    case AKey of
      VK_C:
        begin
          LEdit.CopyToClipboard;
          Result := True;
        end;
      VK_V:
        begin
          if not LEdit.ReadOnly then
            LEdit.PasteFromClipboard;
          Result := True;
        end;
      VK_X:
        begin
          if not LEdit.ReadOnly then
            LEdit.CutToClipboard;
          Result := True;
        end;
    end;
    Exit;
  end;

  if not (AControl is TCDIntfComboBox) then Exit;
  LCombo := TCDIntfComboBox(AControl).LCLControl;
  if (LCombo = nil) or not LCombo.Style.HasEditBox then Exit;

  case AKey of
    VK_C:
      begin
        if LCombo.SelText <> '' then Clipboard.AsText := LCombo.SelText;
        Result := True;
      end;
    VK_V:
      begin
        if not LCombo.ReadOnly then
        begin
          S := Clipboard.AsText;
          if S <> '' then LCombo.SelText := S;
        end;
        Result := True;
      end;
    VK_X:
      begin
        if not LCombo.ReadOnly and (LCombo.SelText <> '') then
        begin
          Clipboard.AsText := LCombo.SelText;
          LCombo.SelText := '';
        end;
        Result := True;
      end;
  end;
end;

procedure CallbackKeyDown(AWindowHandle: TCDForm; AKey: Word);
var
  lTarget, lScope, lBubble, lIntfTarget: TWinControl;
  lIsTab, lTabDirForward: Boolean;
  lTabNextControl: TWinControl;
  i: Integer;
begin
  lTarget := AWindowHandle.GetFocusedControl();
  {$ifdef VerboseCDEvents}
   DebugLn(Format('CallbackKeyDown FocusedControl=%s:%s AKey=%x', [lTarget.Name, lTarget.ClassName, AKey]));
  {$endif}

  lIsTab := AKey = VK_TAB;
  LCLSendKeyDownEvent(lTarget, AKey, 0, True, False);

  // If this is a interface control, send the message to the main LCL control too
  if IsIntfControl(lTarget) then
  begin
    lIntfTarget := lTarget;
    lTarget := lTarget.Parent;
    {$ifdef VerboseCDEvents}
     DebugLn(Format('CallbackKeyDown IsIntfControl, sending msg to Parent=%s:%s', [lTarget.Name, lTarget.ClassName]));
    {$endif}
    LCLSendKeyDownEvent(lTarget, AKey, 0, True, False);
    { Customdrawn owns the edit surface, so the default clipboard
      shortcuts must run here. This is after both KeyDown deliveries so
      application handlers can still consume the key first. }
    if (AKey <> 0) and HandleIntfTextShortcut(lIntfTarget, AKey) then
      AKey := 0;
  end;

  { Bubble unhandled keys up the parent chain so container controls can
    intercept (e.g. TPageControl's KeyDown switches pages on Ctrl+Tab
    when nboKeyboardTabSwitch is in Options). Win32 LCL gets this from
    the OS routing WM_KEYDOWN; in customdrawn we must do it ourselves.
    The BeforeEvent (CN_KEYDOWN) chain fires KeyDown on each parent;
    if any parent handles it, AKey becomes 0 and the bubble stops.
    Plain Tab (no Ctrl) is handled by the SelectNext logic below, so
    don't bubble it -- otherwise a TPageControl ancestor's KeyDown
    would treat it as part of its own tab handling. Modified Tab
    (Ctrl+Tab, Ctrl+Shift+Tab) DOES bubble so the page control can
    switch pages. }
  if (AKey <> 0) and not ((AKey = VK_TAB)
    and (LCLIntf.GetKeyState(VK_CONTROL) >= 0)) then
  begin
    lBubble := lTarget.Parent;
    while (lBubble <> nil) and (AKey <> 0) do
    begin
      LCLSendKeyDownEvent(lBubble, AKey, 0, True, False);
      lBubble := lBubble.Parent;
    end;
  end;

  // If the control didn't eat the tab, then circle around controls
  // Shift+Tab circles in the opposite direction
  lIsTab := lIsTab and (AKey = VK_TAB);
  if (lTarget.Parent <> nil) and lIsTab then
  begin
    lTabDirForward := LCLIntf.GetKeyState(VK_SHIFT) = 0;
    { Walk up to the topmost container so SelectNext sees the full,
      form-wide tab order. Calling lTarget.Parent.SelectNext alone
      restricts traversal to the immediate parent's tab list, so a
      control inside a TGroupBox / TPanel / TPageControl page can
      never escape its container with Tab. }
    lScope := lTarget.Parent;
    while lScope.Parent <> nil do
      lScope := lScope.Parent;
    lScope.SelectNext(lTarget, lTabDirForward, True);
  end
  // slightly different code when the currently selected item is the form itself
  else if lIsTab then
  begin
    // find the first TWinControl and select it
    lTabNextControl := nil;
    for i := 0 to lTarget.ControlCount - 1 do
      if lTarget.Controls[i] is TWinControl then
      begin
        lTabNextControl := TWinControl(lTarget.Controls[i]);
        Break;
      end;

    if lTabNextControl <> nil then lTabNextControl.SetFocus();
  end;
end;

procedure CallbackKeyUp(AWindowHandle: TCDForm; AKey: Word);
var
  lTarget: TWinControl;
begin
  lTarget := AWindowHandle.GetFocusedControl();
  if lTarget = nil then Exit;
  {$ifdef VerboseCDEvents}
   DebugLn(Format('CallbackKeyUp FocusedControl=%s:%s', [lTarget.Name, lTarget.ClassName]));
  {$endif}

  LCLSendKeyUpEvent(lTarget, AKey, 0, True, False);

  // If this is a interface control, send the message to the main LCL control too
  if IsIntfControl(lTarget) then
  begin
    lTarget := lTarget.Parent;
    LCLSendKeyUpEvent(lTarget, AKey, 0, True, False);
  end;
end;

procedure CallbackKeyChar(AWindowHandle: TCDForm; AKeyData: Word; AChar: TUTF8Char);
var
  lTarget: TWinControl;
  lCharCode: Word;
begin
  lTarget := AWindowHandle.GetFocusedControl();
  {$ifdef VerboseCDEvents}
   DebugLn(Format('CallbackKeyChar FocusedControl=%s:%s', [lTarget.Name, lTarget.ClassName]));
  {$endif}

  if lTarget = nil then Exit; // Fixes a crash
  if Length(AChar) = 1 then
    lCharCode := Byte(AChar[1])
  else
    lCharCode:=0;

  // Do not use LCLSendUTF8KeyPress! See Lazarus issues: #40211 and #40224
  if AChar<>'' then lTarget.IntfUTF8KeyPress(AChar, 1, False);
  if lCharCode <> 0 then LCLSendCharEvent(lTarget, lCharCode, AKeyData, True, False, True);

  // If this is a interface control, send the message to the main LCL control too
  if IsIntfControl(lTarget) then
  begin
    lTarget := lTarget.Parent;
    // Do not use LCLSendUTF8KeyPress! See Lazarus issues: #40211 and #40224
    if AChar<>'' then lTarget.IntfUTF8KeyPress(AChar, 1, False);
    if lCharCode <> 0 then LCLSendCharEvent(lTarget, lCharCode, AKeyData, True, False, True);
  end;
end;

function IsIntfControl(AControl: TWinControl): Boolean;
begin
  Result := (AControl <> nil) and (AControl.Parent <> nil);
  if Result then Result :=
    // Standard Tab
    (AControl is TCDIntfButton) or (AControl is TCDIntfEdit) or (AControl is TCDIntfCheckBox) or
    (AControl is TCDIntfRadioButton) or (AControl is TCDIntfComboBox) or (AControl is TCDIntfScrollBar) or
    // Additional Tab
    (AControl is TCDIntfStaticText) or
    // Common Controls Tab
    (AControl is TCDIntfProgressBar) or (AControl is TCDIntfTrackBar) or
    (AControl is TCDIntfPageControl) or
    (AControl is TCDIntfListBox) or
    // Misc Tab
    (AControl is TCDIntfSpinEdit)
    ;
end;

{ TCDIntfEdit }

procedure TCDIntfEdit.DoChange;
var
  Msg: TLMessage;
begin
  inherited DoChange;

  // TCustomEdit responds only to CM_TEXTCHANGED, it doesn't respond to LM_CHANGED. TComboBox responds to LM_CHANGED
  FillChar(Msg{%H-}, SizeOf(Msg), 0);
  Msg.Msg := CM_TEXTCHANGED;
  DeliverMessage(LCLControl, Msg);
end;

{ TCDIntfSpinEdit -- like TCDIntfEdit, push CM_TEXTCHANGED to the LCL
  control whenever the visible text changes (typing, arrow click). The
  LCL TCustomFloatSpinEdit.TextChanged sets FValueChanged := True so
  the next read of LCL.Value re-fetches via WS.GetValue, which returns
  the injected control's FValue. Without this, the LCL side's cached
  FValue stays at its initial value (0), and Spin1.Value reports the old value. }

procedure TCDIntfSpinEdit.DoChange;
var
  Msg: TLMessage;
begin
  inherited DoChange;
  if LCLControl <> nil then
  begin
    FillChar(Msg{%H-}, SizeOf(Msg), 0);
    Msg.Msg := CM_TEXTCHANGED;
    DeliverMessage(LCLControl, Msg);
  end;
end;

{ TCDIntfListBox -- forward selection / item-index changes to the
  LCL TCustomListBox so OnClick / OnSelectionChange / OnChange fire
  exactly when user code expects. The injected control owns the
  state; the LCL side just gets the message and invokes its event
  handlers (and re-fetches via the WS class for property reads). }

constructor TCDIntfListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnSelectionChange := @DoSelectionChange;
end;

procedure TCDIntfListBox.DoSelectionChange(Sender: TObject);
begin
  if LCLControl = nil then Exit;
  LCLSendSelectionChangedMsg(LCLControl);
  LCLSendClickedMsg(LCLControl);
end;

procedure TCDIntfListBox.PrepareControlState;
begin
  inherited PrepareControlState;
  { The LCL focuses the user-facing TCustomListBox; this injected
    drawer-rendered child never gets focus per the LCL. Sync
    csfHasFocus from LCLControl so the drawer shows the right
    selection-highlight colour and focus rect. }
  if Assigned(LCLControl) and LCLControl.Focused then
    FState := FState + [csfHasFocus]
  else
    FState := FState - [csfHasFocus];
end;

{ TCDIntfPageControl -- forward mouse-click tab changes to the LCL.
  TCDCustomTabControl.MouseDown sets only the injected control's
  PageIndex, drawing the new indicator. The LCL TCustomTabControl is
  unaware, so its FPageIndex is stale, ShowCurrentPage is never
  called, and every TabSheet's Visible stays True -- meaning every
  page's contents render at once (overlaid). Sync the LCL side here. }

procedure TCDIntfPageControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (LCLControl <> nil) and (LCLControl.PageIndex <> Self.PageIndex) then
    LCLControl.PageIndex := Self.PageIndex;
end;

{ Sync csfHasFocus from the user-facing LCL control. TCDControl reads
  Self.Focused, but Self here is an injected TCDIntf*-control whose
  parent is the LCL control itself -- the LCL focuses the LCL control,
  never the injected one, so Self.Focused is always False and the
  drawer never paints a focus rect. Override to query LCLControl. }

procedure TCDIntfButton.PrepareControlState;
begin
  inherited PrepareControlState;
  if Assigned(LCLControl) and LCLControl.Focused then
    FState := FState + [csfHasFocus]
  else
    FState := FState - [csfHasFocus];
end;

procedure TCDIntfEdit.PrepareControlState;
begin
  inherited PrepareControlState;
  if Assigned(LCLControl) and LCLControl.Focused then
    FState := FState + [csfHasFocus]
  else
    FState := FState - [csfHasFocus];
end;

procedure TCDIntfCheckBox.PrepareControlState;
begin
  inherited PrepareControlState;
  if Assigned(LCLControl) and LCLControl.Focused then
    FState := FState + [csfHasFocus]
  else
    FState := FState - [csfHasFocus];
end;

procedure TCDIntfRadioButton.PrepareControlState;
begin
  inherited PrepareControlState;
  if Assigned(LCLControl) and LCLControl.Focused then
    FState := FState + [csfHasFocus]
  else
    FState := FState - [csfHasFocus];
end;

{ Forward Space/Enter activation from the injected TCDIntf*-control to
  the user's LCL control. TCDButtonControl.KeyUp only fires Self.Click,
  which lands on the injected control and not on the user-facing TButton
  / TCheckBox / TRadioButton -- so the LCL OnClick handler never runs.
  The mouse path in CallbackMouseUp does this same dispatch via the
  IsIntfControl check; the keyboard path didn't. }

procedure TCDIntfButton.KeyUp(var Key: word; Shift: TShiftState);
var
  WasActivator: Boolean;
begin
  WasActivator := (Key = VK_SPACE) or (Key = VK_RETURN);
  inherited KeyUp(Key, Shift);
  if WasActivator and Assigned(LCLControl) then
    LCLSendClickedMsg(LCLControl);
end;

procedure TCDIntfCheckBox.KeyUp(var Key: word; Shift: TShiftState);
var
  WasActivator: Boolean;
begin
  WasActivator := (Key = VK_SPACE) or (Key = VK_RETURN);
  inherited KeyUp(Key, Shift);
  if WasActivator and Assigned(LCLControl) then
    LCLSendClickedMsg(LCLControl);
end;

procedure TCDIntfRadioButton.KeyUp(var Key: word; Shift: TShiftState);
var
  WasActivator: Boolean;
begin
  WasActivator := (Key = VK_SPACE) or (Key = VK_RETURN);
  inherited KeyUp(Key, Shift);
  if WasActivator and Assigned(LCLControl) then
    LCLSendClickedMsg(LCLControl);
end;

{ Arrow-key navigation between radios in the same LCL group. Up/Left
  move to the previous sibling radio; Down/Right to the next; selecting
  the new one as it gains focus, mirroring Win32/GTK convention. Tab
  alone can't be used because LCL marks unchecked radios with
  TabStop=False, so they're skipped by Tab traversal. }

procedure TCDIntfRadioButton.KeyDown(var Key: word; Shift: TShiftState);
var
  LCLParent: TWinControl;
  i, MyIdx, Step, J, N: Integer;
  Target: TCustomCheckBox;
begin
  case Key of
    VK_UP, VK_LEFT:    Step := -1;
    VK_DOWN, VK_RIGHT: Step := +1;
  else
    inherited KeyDown(Key, Shift);
    Exit;
  end;

  if (LCLControl = nil) or (LCLControl.Parent = nil) then
  begin
    inherited KeyDown(Key, Shift);
    Exit;
  end;

  LCLParent := LCLControl.Parent;

  { Find Self's index among the LCL parent's TRadioButton children. }
  MyIdx := -1;
  N := 0;
  for i := 0 to LCLParent.ControlCount - 1 do
    if LCLParent.Controls[i] is TRadioButton then
    begin
      if LCLParent.Controls[i] = LCLControl then MyIdx := N;
      Inc(N);
    end;
  if (MyIdx < 0) or (N < 2) then
  begin
    inherited KeyDown(Key, Shift);
    Exit;
  end;

  { Walk to the next/prev radio with wrap-around. }
  J := (MyIdx + Step + N) mod N;
  Target := nil;
  N := 0;
  for i := 0 to LCLParent.ControlCount - 1 do
    if LCLParent.Controls[i] is TRadioButton then
    begin
      if N = J then
      begin
        Target := TCustomCheckBox(LCLParent.Controls[i]);
        Break;
      end;
      Inc(N);
    end;

  if (Target = nil) or (Target = LCLControl) then Exit;
  Target.SetFocus;
  TRadioButton(Target).Checked := True;   { selects target; unchecks Self via DoButtonUp override }
  Key := 0;                               { event consumed }
end;

{ Mutual-exclusion fix.
  TCDRadioButton.DoButtonUp walks Parent.Controls[] looking for sibling
  TCDButtonControls with the same FGroupIndex to uncheck. But for the
  injected case, InjectCDControl reparents the TCDIntfRadioButton onto
  its own LCL TRadioButton -- so Parent.Controls is empty (or contains
  only Self) and no sibling is ever found.
  The actual sibling injected radios live one level up, as the CDControl
  fields of the LCL TRadioButtons that share the same LCL parent. Walk
  that list instead and uncheck via the corresponding injected control. }

procedure TCDIntfRadioButton.DoButtonUp;
var
  i: Integer;
  LCLParent: TWinControl;
  Sibling: TControl;
  SiblingHandle: TCDWinControl;
begin
  inherited DoButtonUp;   { handles state, draws checked }
  if (LCLControl = nil) or (LCLControl.Parent = nil) then Exit;

  LCLParent := LCLControl.Parent;
  for i := 0 to LCLParent.ControlCount - 1 do
  begin
    Sibling := LCLParent.Controls[i];
    if (Sibling = LCLControl) or not (Sibling is TCustomCheckBox) then Continue;
    if not (Sibling is TRadioButton) then Continue;     { only uncheck radios }
    if TWinControl(Sibling).Handle = 0 then Continue;
    SiblingHandle := TCDWinControl(TWinControl(Sibling).Handle);
    if (SiblingHandle.CDControl is TCDIntfRadioButton) then
      TCDIntfRadioButton(SiblingHandle.CDControl).DoUncheckButton;
  end;
end;

end.
