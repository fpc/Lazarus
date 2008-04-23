{%MainUnit ../stdctrls.pp}

{******************************************************************************
                                     TEdit
 ******************************************************************************

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

procedure TCustomEdit.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  inherited CalculatePreferredSize(PreferredWidth,PreferredHeight,WithThemeSpace);
  // ignore width
  PreferredWidth:=0;
end;

procedure TCustomEdit.CreateWnd;
begin
  inherited CreateWnd;
  TWSCustomEditClass(WidgetSetClass).SetCharCase(Self, FCharCase);
  TWSCustomEditClass(WidgetSetClass).SetEchoMode(Self, FEchoMode);
  TWSCustomEditClass(WidgetSetClass).SetMaxLength(Self, FMaxLength);
  TWSCustomEditClass(WidgetSetClass).SetPasswordChar(Self, FPasswordChar);
  TWSCustomEditClass(WidgetSetClass).SetReadOnly(Self, FReadOnly);
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
  ControlStyle := ControlStyle - [csCaptureMouse];
  FCompStyle := csEdit;
  FMaxLength:= -1;
  ParentColor := false;
  TabStop := true;
  SetInitialBounds(0,0,GetControlClassDefaultSize.X,GetControlClassDefaultSize.Y);
  FEchoMode := emNormal;
  BorderStyle := bsSingle;
  FAutoSelect := False;
  FAutoSelected := False;
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
  Result := Copy(Text, SelStart + 1, SelLength)
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
  TWSCustomEditClass(WidgetSetClass).SetCaretPos(Self, Value);
end;

{------------------------------------------------------------------------------
  Method: TCustomEdit.SetSelText
  Params: val - new string for text-field
  Returns: nothings

  Replace the selected part of text-field with "val".
 ------------------------------------------------------------------------------}
procedure TCustomEdit.SetSelText(const Val : string);
var
  OldText, NewText: string;
  OldPos: Integer;
begin
  OldPos := SelStart;
  OldText:=Text;
  NewText:=LeftStr(OldText,SelStart)+Val
             +RightStr(OldText,length(OldText)-SelStart-SelLength);
  Text:=NewText;
  SelStart := OldPos + Length(Val);
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
  Result:= FSelLength;
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
  if Text <> '' then begin
    SetSelStart(0);
    SetSelLength(Length(Text));
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
  if (EchoMode = emNormal) and (SelLength > 0) then
    Clipboard.AsText := SelText;
end;

{------------------------------------------------------------------------------
  Method: TCustomEdit.ClearSelection
  Params: -
  Returns: nothing

  Move selected text to clipboard.
 ------------------------------------------------------------------------------}
procedure TCustomEdit.CutToClipboard;
begin
  CopyToClipboard;
  ClearSelection;
end;

procedure TCustomEdit.PasteFromClipboard;
begin
  if Clipboard.HasFormat(CF_TEXT) then
    SelText := Clipboard.AsText;
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
    if FCharCase = ecUpperCase then
      Text := Uppercase(Text)
    else if FCharCase = ecLowerCase then
      Text := Lowercase(Text);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCustomEdit.SetMaxLength
  Params:  Value to set FMaxLength to
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TCustomEdit.SetMaxLength(Value : Integer);
begin
  if Value=MaxLength then exit;
  FMaxLength := Value;
  if HandleAllocated then
    TWSCustomEditClass(WidgetSetClass).SetMaxLength(Self, Value);
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
  if HandleAllocated then
    TWSCustomEditClass(WidgetSetClass).SetPasswordChar(Self, AValue);
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
  Method:  TCustomEdit.SetReadOnly
  Params:  Value to set FReadOnly to
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TCustomEdit.SetReadOnly(Value : Boolean);
begin
  if FReadOnly <> Value then begin
    FReadOnly := Value;
    if HandleAllocated then
      TWSCustomEditClass(WidgetSetClass).SetReadOnly(Self, Value);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCustomEdit.RealSetText
  Params:  Override of text setup to watch for max length
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TCustomEdit.RealSetText(const Value: TCaption);
begin
  if (MaxLength > 0) and (Length(Value) > MaxLength) then
    inherited RealSetText(Copy(Value, 1, MaxLength))
  else
    inherited RealSetText(Value);
end;

{------------------------------------------------------------------------------
  function TCustomEdit.ChildClassAllowed(ChildClass: TClass): boolean;
------------------------------------------------------------------------------}
function TCustomEdit.ChildClassAllowed(ChildClass: TClass): boolean;
begin
  // no childs
  Result := False;
end;

class function TCustomEdit.GetControlClassDefaultSize: TPoint;
begin
  Result.X:=80;
  Result.Y:=23;
end;

procedure TCustomEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
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
  if (KeyDataToShiftState(Message.KeyData) * [ssCtrl, ssAlt] = [])
      and (Message.CharCode in [ord('A')..ord('Z'),ord('a')..ord('z')]) then
    // eat normal keys, so they don't trigger accelerators
    Message.Result := 1
  else
    inherited WMChar(Message);
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

{------------------------------------------------------------------------------
  Method:  TCustomEdit.SetModified
  Params:  Value to set FModified to
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TCustomEdit.TextChanged;
var
  Temp : String;
begin
  //debugln('TCustomEdit.TextChanged ',DbgSName(Self));
  
  //check to see if the charcase should affect the text.
  if FCharCase = ecUppercase then
  begin
    Temp := Uppercase(Text);
    if Temp <> Text then Text := Temp;
  end
  else
  if FCharCase = ecLowercase then
  begin
    Temp := Lowercase(Text);
    if Temp <> Text then Text := Temp;
  end;

  if not (wcfCreatingHandle in FWinControlFlags) then
  begin
    Modified := True;
    if HandleAllocated then
      Change;
  end;
end;

procedure TCustomEdit.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
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

{------------------------------------------------------------------------------
  Getter for CaretPos

  The index of the first line is zero

  The index of the caret before the first char is zero

  If there is a selection, the caret is considered to be right after
  the last selected char, being that "last" here means the right-most char.
 ------------------------------------------------------------------------------}
function TCustomEdit.GetCaretPos: TPoint;
begin
  Result := TWSCustomEditClass(WidgetSetClass).GetCaretPos(Self);
end;


// included by stdctrls.pp
