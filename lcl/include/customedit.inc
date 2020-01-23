{%MainUnit ../stdctrls.pp}

{******************************************************************************
                                     TEdit
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

procedure TCustomEdit.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  inherited CalculatePreferredSize(PreferredWidth,PreferredHeight,WithThemeSpace);
  // ignore width
  PreferredWidth:=0;
end;

procedure TCustomEdit.CreateParams(var Params: TCreateParams);
const
  AlignmentStyle: array[TAlignment] of DWord = (
{ taLeftJustify  } ES_LEFT,
{ taRightJustify } ES_RIGHT,
{ taCenter       } ES_CENTER
  );
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_AUTOHSCROLL or AlignmentStyle[Alignment];
  if ReadOnly then
    Params.Style := Params.Style or ES_READONLY;
  if NumbersOnly then
    Params.Style := Params.Style or ES_NUMBER;
  if not HideSelection then
    Params.Style := Params.Style or ES_NOHIDESEL;
end;

procedure TCustomEdit.InitializeWnd;
begin
  inherited InitializeWnd;
  TWSCustomEditClass(WidgetSetClass).SetCharCase(Self, FCharCase);
  TWSCustomEditClass(WidgetSetClass).SetEchoMode(Self, FEchoMode);
  TWSCustomEditClass(WidgetSetClass).SetMaxLength(Self, FMaxLength);
  TWSCustomEditClass(WidgetSetClass).SetPasswordChar(Self, FPasswordChar);
  TWSCustomEditClass(WidgetSetClass).SetReadOnly(Self, FReadOnly);
  TWSCustomEditClass(WidgetSetClass).SetAlignment(Self, FAlignment);
  TWSCustomEditClass(WidgetSetClass).SetCaretPos(Self, fCaretPos);
  TWSCustomEditClass(WidgetSetClass).SetSelStart(Self, FSelStart);
  TWSCustomEditClass(WidgetSetClass).SetSelLength(Self, FSelLength);
  if WidgetSet.GetLCLCapability(lcTextHint) = LCL_CAPABILITY_YES then
    TWSCustomEditClass(WidgetSetClass).SetTextHint(Self, FTextHint)
  else
    if CanShowEmulatedTextHint then
      ShowEmulatedTextHint(True);
end;

{------------------------------------------------------------------------------
  Method:  TCustomEdit.Create
  Params:  none
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TCustomEdit.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  //FCompStyle is set here because TEdit inherits from this.
  //TCustomMemo also inherits from here but it's create changes fcompstyle to csMemo
  ControlStyle := ControlStyle - [csCaptureMouse] + [csRequiresKeyboardInput];
  FCompStyle := csEdit;
  FMaxLength := 0;
  FHideSelection := True;
  ParentColor := False;
  TabStop := true;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
  FEchoMode := emNormal;
  BorderStyle := bsSingle;
  FAutoSelect := True;
  FAutoSelected := False;
  FTextChangedByRealSetText := False;
  FTextChangedLock := False;
  AutoSize := True;
  // Accessibility
  AccessibleRole := larTextEditorSingleline;
  FTextHint := '';
end;

procedure TCustomEdit.AddHandlerOnChange(const AnOnChangeEvent: TNotifyEvent;
  AsFirst: Boolean);
begin
  if FOnChangeHandler=nil then
    FOnChangeHandler := TMethodList.Create;
  FOnChangeHandler.Add(TMethod(AnOnChangeEvent), not AsFirst);
end;

{------------------------------------------------------------------------------
  Method: TCustomEdit.Clear
  Params: ---
  Returns: nothing

  Clear the text.
 ------------------------------------------------------------------------------}
procedure TCustomEdit.Clear;
begin
  Text := '';
end;

{------------------------------------------------------------------------------
  Method: TCustomEdit.GetSelText
  Params: ---
  Returns: selected text

  Returns the selected part of text-field.
 ------------------------------------------------------------------------------}
function TCustomEdit.GetSelText : string;
begin
  Result := UTF8Copy(Text, SelStart + 1, SelLength)
end;

function TCustomEdit.GetTextHint: TTranslateString;
begin
  Result := FTextHint;
end;

{------------------------------------------------------------------------------
  Setter for CaretPos

  The index of the first line is zero

  The index of the caret before the first char is zero

  If there is a selection, the caret is considered to be right after
  the last selected char, being that "last" here means the right-most char.
 ------------------------------------------------------------------------------}
procedure TCustomEdit.SetCaretPos(const Value: TPoint);
begin
  fCaretPos:=Value;
  if HandleAllocated then
    TWSCustomEditClass(WidgetSetClass).SetCaretPos(Self, Value);
end;

{------------------------------------------------------------------------------
  Method: TCustomEdit.SetSelText
  Params: val - new string for text-field
  Returns: nothings

  Replace the selected part of text-field with "val".
 ------------------------------------------------------------------------------}
procedure TCustomEdit.SetSelText(const Val : string);
begin
  if HandleAllocated then
    TWSCustomEditClass(WidgetSetClass).SetSelText(Self, Val);
end;

{------------------------------------------------------------------------------
  Method: TCustomEdit.GetSelStart
  Params: ---
  Returns: starting index of selected text

  Returns starting index of selected text
 ------------------------------------------------------------------------------}
function TCustomEdit.GetSelStart : integer;
begin
  if HandleAllocated then
    FSelStart:= TWSCustomEditClass(WidgetSetClass).GetSelStart(Self);
  Result:= FSelStart;
end;

{------------------------------------------------------------------------------
  Method: TCustomEdit.SetSelStart
  Params: val -
  Returns: nothing

  Sets starting index for selected text.
 ------------------------------------------------------------------------------}
procedure TCustomEdit.SetSelStart(Val : integer);
begin
  FSelStart:= Val;
  if HandleAllocated then
    TWSCustomEditClass(WidgetSetClass).SetSelStart(Self, Val);
end;

{------------------------------------------------------------------------------
  Method: TCustomEdit.GetSelLength
  Params: ---
  Returns: length of selected text

  Returns length of selected text
 ------------------------------------------------------------------------------}
function TCustomEdit.GetSelLength : integer;
begin
  if HandleAllocated then
    FSelLength := TWSCustomEditClass(WidgetSetClass).GetSelLength(Self);
  Result := FSelLength;
end;

{------------------------------------------------------------------------------
  Method: TCustomEdit.SetSelLength
  Params: val -
  Returns: nothing

  Sets length of selected text.
 ------------------------------------------------------------------------------}
procedure TCustomEdit.SetSelLength(Val : integer);
begin
  if Val<0 then Val:=0;
  FSelLength := Val;
  if HandleAllocated then
    TWSCustomEditClass(WidgetSetClass).SetSelLength(Self, Val);
end;

{------------------------------------------------------------------------------
  Method: TCustomEdit.SelectAll
  Params: -
  Returns: nothing

  Select entire text.
 ------------------------------------------------------------------------------}
procedure TCustomEdit.SelectAll;
begin
  if Text <> '' then
  begin
    SetSelStart(0);
    SetSelLength(UTF8Length(Text));
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomEdit.ClearSelection
  Params: -
  Returns: nothing

  Delete selected text.
 ------------------------------------------------------------------------------}
procedure TCustomEdit.ClearSelection;
begin
  if SelLength > 0 then
    SelText := '';
end;

{------------------------------------------------------------------------------
  Method: TCustomEdit.ClearSelection
  Params: -
  Returns: nothing

  Copy selected text to clipboard.
 ------------------------------------------------------------------------------}
procedure TCustomEdit.CopyToClipboard;
begin
  if HandleAllocated then
    TWSCustomEditClass(WidgetSetClass).Copy(Self);
end;

{------------------------------------------------------------------------------
  Method: TCustomEdit.ClearSelection
  Params: -
  Returns: nothing

  Move selected text to clipboard.
 ------------------------------------------------------------------------------}
procedure TCustomEdit.CutToClipboard;
begin
  if HandleAllocated then
    TWSCustomEditClass(WidgetSetClass).Cut(Self);
end;

destructor TCustomEdit.Destroy;
begin
  FOnChangeHandler.Free;

  inherited Destroy;
end;

procedure TCustomEdit.PasteFromClipboard;
begin
  if HandleAllocated then
    TWSCustomEditClass(WidgetSetClass).Paste(Self);
end;

procedure TCustomEdit.Undo;
begin
  if HandleAllocated then
    TWSCustomEditClass(WidgetSetClass).Undo(Self);
end;

{------------------------------------------------------------------------------
  Method:  TCustomEdit.GetModified
  Params:  none
  Returns: FModified
 ------------------------------------------------------------------------------}
function TCustomEdit.GetModified : Boolean;
begin
  Result := FModified;
end;

function TCustomEdit.GetCanUndo: Boolean;
begin
  if HandleAllocated then
    Result := TWSCustomEditClass(WidgetSetClass).GetCanUndo(Self)
  else
    Result := False;
end;

function TCustomEdit.GetNumbersOnly: Boolean;
begin
  Result := FNumbersOnly;
end;

function TCustomEdit.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

{------------------------------------------------------------------------------
  Method:  TCustomEdit.SetCharCase
  Params:  Value to set FCharCase to
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TCustomEdit.SetCharCase(Value : TEditCharCase);
begin
  if FCharCase <> value then
  begin
    FCharCase := Value;
    // update interface, it might do the case conversion itself.
    if HandleAllocated then
      TWSCustomEditClass(WidgetSetClass).SetCharCase(Self, Value);
    case FCharCase of
      ecUpperCase: Text := UTF8UpperCase(Text);
      ecLowerCase: Text := UTF8LowerCase(Text);
    end;
  end;
end;

procedure TCustomEdit.SetHideSelection(const AValue: Boolean);
begin
  if FHideSelection <> AValue then
  begin
    FHideSelection := AValue;
    if HandleAllocated then
      TWSCustomEditClass(WidgetSetClass).SetHideSelection(Self, AValue);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCustomEdit.SetMaxLength
  Params:  Value to set FMaxLength to
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TCustomEdit.SetMaxLength(Value : Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value <> MaxLength then
  begin
    FMaxLength := Value;
    if HandleAllocated then
      TWSCustomEditClass(WidgetSetClass).SetMaxLength(Self, Value);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCustomEdit.SetModified
  Params:  Value to set FModified to
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TCustomEdit.SetModified(Value : Boolean);
begin
  FModified := Value;
end;

procedure TCustomEdit.SetPasswordChar(const AValue: Char);
begin
  if FPasswordChar=AValue then exit;

  FPasswordChar:=AValue;
  case FPasswordChar of
    #0: EchoMode := emNormal;
    ' ': EchoMode := emNone;
  else
    EchoMode:=emPassword;
  end;
  if HandleAllocated and (FEmulatedTextHintStatus=thsHidden) then
    TWSCustomEditClass(WidgetSetClass).SetPasswordChar(Self, AValue);
end;

class procedure TCustomEdit.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCustomEdit;
  RegisterPropertyToSkip(TCustomEdit, 'BevelInner', 'VCL compatibility property', '');
  RegisterPropertyToSkip(TCustomEdit, 'BevelKind',  'VCL compatibility property', '');
  RegisterPropertyToSkip(TCustomEdit, 'BevelOuter', 'VCL compatibility property', '');
  RegisterPropertyToSkip(TCustomEdit, 'OEMConvert', 'VCL compatibility property', '');
  RegisterPropertyToSkip(TCustomEdit, 'TextHintFontColor','Used in a previous version of Lazarus','');
  RegisterPropertyToSkip(TCustomEdit, 'TextHintFontStyle','Used in a previous version of Lazarus','');
end;

function TCustomEdit.CanShowEmulatedTextHint: Boolean;
begin
  Result := HandleAllocated and
            (WidgetSet.GetLCLCapability(lcTextHint)=LCL_CAPABILITY_NO) and
            (([csDesigning,csLoading] * ComponentState) = []) and
            (FTextHint <> '') and
            (Text = '') and
            (not Focused);
end;

function TCustomEdit.CreateEmulatedTextHintFont: TFont;
begin
  Result := TWSCustomEditClass(WidgetSetClass).CreateEmulatedTextHintFont(Self);
end;

{------------------------------------------------------------------------------
  Method:  TCustomEdit.SetEchoMode
  Params:  Value to set FModified to
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TCustomEdit.SetEchoMode(Val : TEchoMode);
begin
  if FEchoMode=Val then exit;
  
  FEchoMode:= Val;
  case FEchoMode of
    emNormal :
      PasswordChar := #0;
    emPassWord :
      if (PasswordChar=#0) or (PasswordChar=' ')
        then PasswordChar := '*';
    emNone :
      PasswordChar := ' ';
  end;
  if HandleAllocated then
    TWSCustomEditClass(WidgetSetClass).SetEchoMode(Self, Val);
end;

{------------------------------------------------------------------------------
  Method:  TCustomEdit.SetNumbersOnly
  Params:  Value to set FNumbersOnly to
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TCustomEdit.SetNumbersOnly(Value: Boolean);
begin
  if FNumbersOnly <> Value then
  begin
    FNumbersOnly := Value;
    if HandleAllocated then
      TWSCustomEditClass(WidgetSetClass).SetNumbersOnly(Self, Value);
  end;
end;


{------------------------------------------------------------------------------
  Method:  TCustomEdit.SetReadOnly
  Params:  Value to set FReadOnly to
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TCustomEdit.SetReadOnly(Value : Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    if HandleAllocated then
      TWSCustomEditClass(WidgetSetClass).SetReadOnly(Self, Value);
  end;
end;

{------------------------------------------------------------------------------
  function TCustomEdit.ChildClassAllowed(ChildClass: TClass): boolean;
------------------------------------------------------------------------------}
function TCustomEdit.ChildClassAllowed(ChildClass: TClass): boolean;
begin
  // no children
  Result := False;
  if Widgetset.GetLCLCapability(lcAllowChildControlsInNativeControls) = LCL_CAPABILITY_YES then Result := True;
end;

class function TCustomEdit.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 80;
  Result.CY := 23;
end;

procedure TCustomEdit.KeyUpAfterInterface(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUpAfterInterface(Key, Shift);
  if Key = VK_RETURN then
  begin
    EditingDone;
    if FAutoSelect then
    begin
      SelectAll;
      if (SelText = Text) then FAutoSelected := True;
    end;//End if FAutoSelect
  end;//End if Key=VK_RETURN
end;

procedure TCustomEdit.WMChar(var Message: TLMChar);
begin
  // all normal characters are handled by the Edit
  //debugln('TCustomEdit.WMChar ',DbgSName(Self),' ',dbgs(Message.CharCode));
  if KeyDataToShiftState(Message.KeyData) * [ssCtrl, ssAlt] = [] then
    Message.Result := 1  // eat normal keys, so they don't trigger accelerators
  else
    inherited WMChar(Message);
end;

procedure TCustomEdit.WMKillFocus(var Message: TLMKillFocus);
begin
  inherited WMKillFocus(Message);

  if CanShowEmulatedTextHint then ShowEmulatedTextHint;
end;

procedure TCustomEdit.WMSetFocus(var Message: TLMSetFocus);
begin
  if FEmulatedTextHintStatus=thsShowing then HideEmulatedTextHint;

  inherited WMSetFocus(Message);
end;

procedure TCustomEdit.WndProc(var Message: TLMessage);
begin
  if not((Message.msg=CM_TEXTCHANGED) and (FEmulatedTextHintStatus<>thsHidden)) then //eat CM_TEXTCHANGED
    inherited WndProc(Message);
end;

procedure TCustomEdit.ShouldAutoAdjust(var AWidth, AHeight: Boolean);
begin
  AWidth := True;
  AHeight := not AutoSize;
end;

procedure TCustomEdit.CMWantSpecialKey(var Message: TCMWantSpecialKey);
begin
  {$ifdef darwin}
  // don't allow LCL to handle arrow keys for edit controls
  if Message.CharCode in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN] then
    Message.Result := 1
  else
  {$endif}
    inherited;
end;

procedure TCustomEdit.MouseUp(Button: TMouseButton; Shift:TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  //AutoSelect when left mouse is clicked for the 1st time after having focus
  if (Button = mbLeft) then
  begin
    if (FAutoSelect and not FAutoSelected) then
    begin
      SelectAll;
      if (SelText = Text) then FAutoSelected := True;
    end;//End if (FAutoSelect and not FAutoSelected)
  end;//End if (Button = mbLeft)
end;

procedure TCustomEdit.RealSetText(const AValue: TCaption);
begin
  if (FEmulatedTextHintStatus=thsShowing) and (AValue<>'') then
    HideEmulatedTextHint;

  FTextChangedByRealSetText := True;
  Modified := False;
  inherited RealSetText(AValue);
  FTextChangedByRealSetText := False;

  if (FEmulatedTextHintStatus=thsHidden) and CanShowEmulatedTextHint then
    ShowEmulatedTextHint;
end;

procedure TCustomEdit.RemoveAllHandlersOfObject(AnObject: TObject);
begin
  inherited RemoveAllHandlersOfObject(AnObject);

  if FOnChangeHandler<>nil then
    FOnChangeHandler.RemoveAllMethodsOfObject(AnObject);
end;

procedure TCustomEdit.RemoveHandlerOnChange(
  const AnOnChangeEvent: TNotifyEvent);
begin
  if FOnChangeHandler<>nil then
    FOnChangeHandler.Remove(TMethod(AnOnChangeEvent));
end;

function TCustomEdit.RealGetText: TCaption;
begin
  if FEmulatedTextHintStatus=thsShowing then
    Result := ''
  else
    Result := inherited RealGetText;
end;

{------------------------------------------------------------------------------
  Method:  TCustomEdit.SetModified
  Params:  Value to set FModified to
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TCustomEdit.TextChanged;
var
  Cur, Temp: String;
  CPos: TPoint;
  SStart, SLen: Integer;
begin
  //debugln('TCustomEdit.TextChanged ',DbgSName(Self));
  if FTextChangedLock then
    Exit;
  if FCharCase in [ecUppercase, ecLowercase] then
  begin
    // use a local variable to reduce amounts of widgetset calls
    Cur := Text;
    //check to see if the charcase should affect the text.
    if FCharCase = ecUppercase then
      Temp := UTF8UpperCase(Cur)
    else
      Temp := UTF8LowerCase(Cur);
    if (Temp <> Cur) then
    begin
      CPos := CaretPos;
      SStart := SelStart;
      SLen := SelLength;
      FTextChangedLock := True;
      try
        Text := Temp;
      finally
        FTextChangedLock := False;
      end;
      SelStart  := SStart;
      SelLength := SLen;
      CaretPos  := CPos;
    end;
  end;
  if not (wcfCreatingHandle in FWinControlFlags) then
  begin
    if ([csLoading,csDestroying]*ComponentState=[]) then
    begin
      if not FTextChangedByRealSetText then
        Modified := True;
      Change;
    end;
  end;
end;

procedure TCustomEdit.Change;
begin
  Changed;
  if Assigned(FOnChange) then FOnChange(Self);
  if Assigned(FOnChangeHandler) then
    FOnChangeHandler.CallNotifyEvents(Self);
end;

procedure TCustomEdit.DoEnter;
begin
  //AutoSelect when DoEnter is fired by keyboard
  if (FAutoSelect and not (csLButtonDown in ControlState)) then
  begin
    SelectAll;
    if (SelText = Text) then FAutoSelected := True;
  end;//End if FAutoSelect
  inherited DoEnter;
end;

procedure TCustomEdit.DoExit;
begin
  FAutoSelected := False;
  inherited DoExit;
end;

procedure TCustomEdit.EditingDone;
begin
  if not ReadOnly then
    inherited EditingDone;
end;

procedure TCustomEdit.FontChanged(Sender: TObject);
var
  HintFont: TObject;
begin
  if (FEmulatedTextHintStatus=thsHidden) then
    inherited FontChanged(Sender)
  else
  begin
    HintFont := CreateEmulatedTextHintFont;
    try
      inherited FontChanged(HintFont);
    finally
      HintFont.Free;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Getter for CaretPos

  The index of the first line is zero

  The index of the caret before the first char is zero

  If there is a selection, the caret is considered to be right after
  the last selected char, being that "last" here means the right-most char.
 ------------------------------------------------------------------------------}
function TCustomEdit.GetCaretPos: TPoint;
begin
  if HandleAllocated then
    fCaretPos := TWSCustomEditClass(WidgetSetClass).GetCaretPos(Self);
  Result:=fCaretPos;
end;

procedure TCustomEdit.SetTextHint(const AValue: TTranslateString);
begin
  if (FTextHint = AValue) then Exit;
  FTextHint := AValue;
  if (WidgetSet.GetLCLCapability(lcTextHint) = LCL_CAPABILITY_YES) and HandleAllocated then
    TWSCustomEditClass(WidgetSetClass).SetTextHint(Self, AValue);
  if (FEmulatedTextHintStatus=thsShowing) and (FTextHint = '') then
  begin
    HideEmulatedTextHint;
  end
  else
  begin
    if CanShowEmulatedTextHint then ShowEmulatedTextHint;
  end;
end;

procedure TCustomEdit.ShowEmulatedTextHint(const ForceShow: Boolean);
var
  HintFont: TFont;
begin
  if (FEmulatedTextHintStatus<>thsHidden) and not ForceShow then
    Exit;

  FEmulatedTextHintStatus := thsChanging;

  HintFont := CreateEmulatedTextHintFont;
  try
    TWSCustomEditClass(WidgetSetClass).SetFont(Self, HintFont);
  finally
    HintFont.Free;
  end;

  TWSCustomEditClass(WidgetSetClass).SetText(Self, Self.TextHint);
  TWSCustomEditClass(WidgetSetClass).SetPasswordChar(Self, #0);

  FEmulatedTextHintStatus := thsShowing;
end;

procedure TCustomEdit.HideEmulatedTextHint;
begin
  if FEmulatedTextHintStatus<>thsShowing then
    Exit;

  FEmulatedTextHintStatus := thsChanging;

  TWSCustomEditClass(WidgetSetClass).SetFont(Self, Font);
  TWSCustomEditClass(WidgetSetClass).SetPasswordChar(Self, PasswordChar);
  TWSCustomEditClass(WidgetSetClass).SetText(Self, '');

  FEmulatedTextHintStatus := thsHidden;
end;

procedure TCustomEdit.SetAlignment(const AValue: TAlignment);
begin
  if FAlignment = AValue then
    exit;
  FAlignment := AValue;
  if HandleAllocated then
    TWSCustomEditClass(WidgetSetClass).SetAlignment(Self, FAlignment);
end;




// included by stdctrls.pp
