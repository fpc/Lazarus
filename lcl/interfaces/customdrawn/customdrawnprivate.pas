unit customdrawnprivate;

{$mode objfpc}{$H+}

interface

uses
  // rtl+ftl
  Types, Classes, SysUtils,
  // LCL
  Controls, Graphics, Forms, stdctrls, checklst, extctrls, comctrls,
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

  { TCDIntfToggleBox -- bridges the latching button toggle back to the
    LCL TCustomCheckBox so OnChange fires whenever the user flips the
    state via mouse / Space / Enter. Without LCLSendChangedMsg the LCL's
    cached FState would not see the change and Checked would report the old value. }
  TCDIntfToggleBox = class(TCDToggleBox)
  protected
    procedure DoButtonUp; override;
    procedure KeyUp(var Key: word; Shift: TShiftState); override;
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

  { TCDIntfCheckListBox -- adds the click-check bridge on top of the
    listbox bridge. When the user toggles a checkbox (via mouse hit
    on the check column or VK_SPACE), TCDCheckListBox fires
    OnClickCheck; we translate that into LM_CHANGED with the row index
    in WParam, which TCustomCheckListBox.DoChange picks up to call its
    own ClickCheck (and the user's OnClickCheck). }
  TCDIntfCheckListBox = class(TCDCheckListBox)
  protected
    procedure DoSelectionChange(Sender: TObject);
    procedure DoClickCheck(Sender: TObject; AIndex: Integer);
    procedure PrepareControlState; override;
  public
    LCLControl: TCustomCheckListBox;
    constructor Create(AOwner: TComponent); override;
  end;

  TCDIntfScrollBar = class(TCDScrollBar)
  public
    LCLControl: TCustomScrollBar;
    { Wire-up target for FOnChangeByUser. Pushes the new Position into
      the LCL TScrollBar and synthesises the LM_HSCROLL / LM_VSCROLL
      message so any TScrollBar.OnScroll subscriber (and any parent
      that intercepts via CNHScroll / CNVScroll) sees the change.
      Without this, the inner customdrawn drawer mutates its own
      FPosition on click but the LCL side never hears about it -- the
      bar appears interactive but does not notify the host. }
    procedure HandleChangeByUser(Sender: TObject);
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

  TCDIntfStatusBar = class(TCDStatusBar)
  protected
    procedure PrepareControlState; override;
  public
    LCLControl: TStatusBar;
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
  Mess: TLMContextMenu;
begin
  if AWindowHandle.InputDisabled then Exit;
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

  { Synthesize LM_CONTEXTMENU on right-click release. Win32 gets this
    from the OS; on customdrawn nobody else fires it, so TControl's
    PopupMenu / OnContextPopup never trigger. The LCL handler at
    TControl.WMContextMenu reads Pos from the message in screen
    coords. On Wayland true global coords are unavailable, so use
    form-local coords consistently with this backend's ClientToScreen
    compromise for popup placement. }
  if (Button = mbRight) and lEventEndsInsideTheControl then
  begin
    FillChar(Mess, SizeOf(Mess), 0);
    Mess.Msg := LM_CONTEXTMENU;
    Mess.XPos := x;
    Mess.YPos := y;
    DeliverMessage(lTarget, Mess);
  end;

  // Form scrolling
  AWindowHandle.IsScrolling := False;
end;

procedure CallbackMouseDown(AWindowHandle: TCDForm; x, y: Integer; Button: TMouseButton; ShiftState: TShiftState = []);
var
  lTarget: TWinControl;
  lIntfTarget: TWinControl = nil;
  lEventPos: TPoint;
  lMsg: TLMessage;
begin
  if AWindowHandle.InputDisabled then Exit;
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

  { Cancel any in-flight tooltip hint -- LM_BUTTONDOWN goes through
    NotifyUserInputHandler's `else` branch and hits CancelHint. }
  FillChar(lMsg, SizeOf(lMsg), 0);
  lMsg.Msg := LM_LBUTTONDOWN;
  NotifyApplicationUserInput(lTarget, lMsg);

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
  lMsg: TLMessage;
begin
  if AWindowHandle.InputDisabled then Exit;
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

  { Drive Application's hint mechanism. Other widgetsets (win32, carbon,
    fpgui ...) call NotifyApplicationUserInput from their input event paths;
    TApplication.NotifyUserInputHandler reacts to LM_MOUSEMOVE by
    starting the hint timer (which ultimately calls ShowHintWindow),
    and to anything else by cancelling an in-flight hint. Without this
    hook the entire Application.HintControl / THintWindow path is
    inactive and TControl.Hint never shows a tooltip. }
  FillChar(lMsg, SizeOf(lMsg), 0);
  lMsg.Msg := LM_MOUSEMOVE;
  NotifyApplicationUserInput(lTarget, lMsg);

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
  if AWindowHandle.InputDisabled then Exit;
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
  if AWindowHandle.InputDisabled then Exit;
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
  if AWindowHandle.InputDisabled then Exit;
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
    (AControl is TCDIntfRadioButton) or (AControl is TCDIntfToggleBox) or
    (AControl is TCDIntfComboBox) or (AControl is TCDIntfScrollBar) or
    // Additional Tab
    (AControl is TCDIntfStaticText) or
    // Common Controls Tab
    (AControl is TCDIntfProgressBar) or (AControl is TCDIntfTrackBar) or
    (AControl is TCDIntfPageControl) or
    (AControl is TCDIntfListBox) or
    (AControl is TCDIntfCheckListBox) or
    (AControl is TCDIntfStatusBar) or
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

  { Push fresh surrounding text + cursor rectangle to the IME via
    zwp_text_input_v3. ibus / fcitx5 use surrounding text for typo
    correction and word disambiguation; without re-sending after each
    keystroke they see stale context and suggest candidates for the previous text. }
  {$ifdef CD_Wayland}
  if Assigned(LCLControl) then
    CDWidgetset.WLPushTextInputContext(LCLControl);
  {$endif}
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
  state; the LCL side receives the message and invokes its event
  handlers (and re-fetches via the WS class for property reads). }

{ TCDIntfScrollBar }

procedure TCDIntfScrollBar.HandleChangeByUser(Sender: TObject);
var
  Msg: TLMScroll;
  Delta: Integer;
  ScrollCode: SmallInt;
begin
  if LCLControl = nil then Exit;
  Delta := Position - LCLControl.Position;
  { Map the user gesture to an SB_* code so the host (TCustomGrid etc.)
    can apply its own per-action semantics: arrows step one row, trough
    one page (excluding fixed header), thumb tracks absolute. Without
    this every gesture is SB_THUMBPOSITION, which makes trough clicks
    scroll one row too far (the grid otherwise subtracts the fixed
    header) and arrow clicks step PageSize/10 instead of one row.
    CurrentButton tells us when an arrow is pressed; otherwise we look
    at the delta magnitude. }
  if csfLeftArrow in CurrentButton then
    ScrollCode := SB_LINEUP
  else if csfRightArrow in CurrentButton then
    ScrollCode := SB_LINEDOWN
  else if Delta = FLargeChange then
    ScrollCode := SB_PAGEDOWN
  else if Delta = -FLargeChange then
    ScrollCode := SB_PAGEUP
  else
    ScrollCode := SB_THUMBPOSITION;
  { For non-thumb gestures the LCL CNHScroll / CNVScroll handler
    re-derives NewPos from FPosition + FLargeChange (or - FSmallChange
    etc.). To prevent it double-counting our pre-applied delta, roll
    the inner FPosition back to LCLControl.Position before dispatch.
    The grid will scroll, then push back the new position via
    SetScrollInfo, which ApplyParams syncs to inner. }
  if ScrollCode <> SB_THUMBPOSITION then
    Position := LCLControl.Position
  else if LCLControl.Position <> Position then
    LCLControl.Position := Position;
  FillChar(Msg, SizeOf(Msg), 0);
  if Kind = sbHorizontal then Msg.Msg := LM_HSCROLL
                         else Msg.Msg := LM_VSCROLL;
  Msg.ScrollCode := ScrollCode;
  Msg.SmallPos := SmallInt(Position);
  Msg.Pos := Position;
  if LCLControl.HandleAllocated then Msg.ScrollBar := LCLControl.Handle;
  LCLControl.Dispatch(Msg);
end;

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

{ TCDIntfStatusBar }

procedure TCDIntfStatusBar.PrepareControlState;
begin
  inherited PrepareControlState;
  if LCLControl <> nil then
  begin
    PanelsRef    := LCLControl.Panels;
    SimpleText   := LCLControl.SimpleText;
    SimplePanel  := LCLControl.SimplePanel;
    SizeGrip     := LCLControl.SizeGrip;
  end;
end;

{ TCDIntfCheckListBox }

constructor TCDIntfCheckListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnSelectionChange := @DoSelectionChange;
  OnClickCheck      := @DoClickCheck;
end;

procedure TCDIntfCheckListBox.DoSelectionChange(Sender: TObject);
begin
  if LCLControl = nil then Exit;
  LCLSendSelectionChangedMsg(LCLControl);
  LCLSendClickedMsg(LCLControl);
end;

procedure TCDIntfCheckListBox.DoClickCheck(Sender: TObject; AIndex: Integer);
begin
  if LCLControl = nil then Exit;
  { TCustomCheckListBox.DoChange handles LM_CHANGED, reads WParam as the
    row index, and fires the LCL's OnClickCheck handler. }
  LCLSendChangedMsg(LCLControl, AIndex);
end;

procedure TCDIntfCheckListBox.PrepareControlState;
begin
  inherited PrepareControlState;
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

{ TCDIntfToggleBox }

procedure TCDIntfToggleBox.DoButtonUp;
begin
  inherited DoButtonUp;   { flips csfOn / csfOff via FHasOnOffStates }
  { Notify the LCL the state has changed so TCustomCheckBox.DoChange
    re-fetches via RetrieveState and fires OnChange. WParam is unused
    by TCustomCheckBox's handler. }
  if LCLControl <> nil then LCLSendChangedMsg(LCLControl, 0);
end;

procedure TCDIntfToggleBox.KeyUp(var Key: word; Shift: TShiftState);
var
  WasActivator: Boolean;
begin
  WasActivator := (Key = VK_SPACE) or (Key = VK_RETURN);
  inherited KeyUp(Key, Shift);
  { TCDButtonControl.KeyUp routes Space/Enter through DoButtonUp, which
    our override has already notified the LCL about. Mirror the LCL
    OnClick that the mouse path also delivers via CallbackMouseUp's
    IsIntfControl branch -- the keyboard path has no equivalent. }
  if WasActivator and Assigned(LCLControl) then
    LCLSendClickedMsg(LCLControl);
end;

procedure TCDIntfToggleBox.PrepareControlState;
begin
  inherited PrepareControlState;
  if Assigned(LCLControl) and LCLControl.Focused then
    FState := FState + [csfHasFocus]
  else
    FState := FState - [csfHasFocus];
end;

end.
