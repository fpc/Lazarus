{%MainUnit ../spin.pp}

{
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

{-----------------------------------------------------------------------------}
procedure TCustomFloatSpinEdit.UpdateControl;
begin
  if MaxValue<MinValue then FMaxValue:=MinValue;
  if FValue<FMinValue then FValue:=fMinValue;
  if FValue>FMaxValue then FValue:=fMaxValue;
  if (not HandleAllocated) then exit;
  if ([csLoading,csDestroying]*ComponentState<>[]) then begin
    FUpdatePending := true;
    exit;
  end;
  TWSCustomFloatSpinEditClass(WidgetSetClass).UpdateControl(Self);
  FValueChanged := true;
  FUpdatePending := false;
end;

function TCustomFloatSpinEdit.ValueIsStored: boolean;
begin
  Result:=true; // fpc bug, default value is always 0
end;

procedure TCustomFloatSpinEdit.TextChanged;
var
  lPrevValue: single;
begin
  lPrevValue := FValue;
  FValueChanged := true;
  if Value = lPrevValue then exit;
  Modified := true;
  FValueEmpty := false;
  if HandleAllocated and (not (csLoading in ComponentState)) then Change;
end;

procedure TCustomFloatSpinEdit.SetMaxValue(const AValue: single);
begin
  if FMaxValue=AValue then exit;
  FMaxValue:=AValue;
  UpdateControl;
end;

function TCustomFloatSpinEdit.GetModified: Boolean;
begin
  Result := FModified;
end;

function TCustomFloatSpinEdit.GetSelLength: integer;
begin
  if HandleAllocated then
    FSelLength := TWSCustomFloatSpinEditClass(WidgetSetClass).GetSelLength(Self);
  Result:= FSelLength;
end;

function TCustomFloatSpinEdit.GetSelStart: integer;
begin
  if HandleAllocated then
    FSelStart:= TWSCustomFloatSpinEditClass(WidgetSetClass).GetSelStart(Self);
  Result:= FSelStart;
end;

function TCustomFloatSpinEdit.GetSelText: String;
begin
  Result:= Copy(Text, SelStart + 1, SelLength)
end;

function TCustomFloatSpinEdit.MaxValueIsStored: boolean;
begin
  Result:=true; // fpc bug, default value is always 0
end;

function TCustomFloatSpinEdit.MinValueIsStored: boolean;
begin
  Result:=true; // fpc bug, default value is always 0
end;


{-----------------------------------------------------------------------------}

procedure TCustomFloatSpinEdit.SetMinValue(const AValue: single);
begin
  if FMinValue=AValue then exit;
  FMinValue:=AValue;
  UpdateControl;
end;

procedure TCustomFloatSpinEdit.SetModified(const AValue: Boolean);
begin
  FModified := AValue;
end;

procedure TCustomFloatSpinEdit.SetSelLength(const AValue: integer);
begin
  FSelLength:= AValue;
  if HandleAllocated then
    TWSCustomFloatSpinEditClass(WidgetSetClass).SetSelLength(Self, FSelLength);
end;

procedure TCustomFloatSpinEdit.SetSelStart(const AValue: integer);
begin
  FSelStart:= AValue;
  if HandleAllocated then
    TWSCustomFloatSpinEditClass(WidgetSetClass).SetSelStart(Self, FSelStart);
end;

procedure TCustomFloatSpinEdit.SetSelText(const AValue: String);
var
  OldText, NewText: string;
begin
  OldText:=Text;
  NewText:=LeftStr(OldText,SelStart)+AValue
             +RightStr(OldText,length(OldText)-SelStart-SelLength);
  Text:=NewText;
end;

procedure TCustomFloatSpinEdit.SetValueEmpty(const AValue: boolean);
begin
  if FValueEmpty=AValue then exit;
  FValueEmpty:=AValue;
  UpdateControl;
end;

procedure TCustomFloatSpinEdit.SetIncrement(const NewIncrement: single);
begin
  if NewIncrement = FIncrement then exit;
  FIncrement := NewIncrement;
  UpdateControl;
end;

procedure TCustomFloatSpinEdit.InitializeWnd;
var
  ASelStart, ASelLength : integer;
begin
  inherited InitializeWnd;
  UpdateControl;
  if FSelStart <> FSelLength then begin
    ASelStart:= FSelStart;
    ASelLength:= FSelLength;
    SelStart:= ASelStart;
    SelLength:= ASelLength;
  end;
end;

procedure TCustomFloatSpinEdit.Loaded;
begin
  inherited Loaded;
  if FUpdatePending then UpdateControl;
end;

procedure TCustomFloatSpinEdit.Change;
begin
  if [csLoading,csDestroying,csDesigning]*ComponentState<>[] then exit;
  EditingDone;
  if Assigned(FOnChange) then FOnChange(Self);
end;

{-----------------------------------------------------------------------------}
Procedure TCustomFloatSpinEdit.SetValue(const num : Single);
begin
  if FValue = Num then exit;
  FValue := Num;
  // clear FValueChanged to prevent getting the old value from the widget
  FValueChanged := false;
  FUpdatePending := true;
  UpdateControl;
end;

{-----------------------------------------------------------------------------}
Function TCustomFloatSpinEdit.GetValue: Single;
begin
  if HandleAllocated and FValueChanged 
    and not (wcfCreatingHandle in FWinControlFlags) then 
  begin
    FValue := TWSCustomFloatSpinEditClass(WidgetSetClass).GetValue(Self);
    FValueChanged := false;
  end;
  Result := fValue;
end;

{-----------------------------------------------------------------------------}
procedure TCustomFloatSpinEdit.SetDecimals(Num : Integer);
begin
  if fDecimals = Num then exit;
  fDecimals := Num;
  UpdateControl;
end;

{-----------------------------------------------------------------------------}
constructor TCustomFloatSpinEdit.Create(TheOwner : TComponent);
begin
  inherited Create(TheOwner);
  fCompStyle := csSpinEdit;
  
  FIncrement := 1;
  fDecimals := 2;
  fValue := 0;
  fMinValue := 0;
  fMaxValue := 100;
  FUpdatePending := true;
  FValueChanged := true;

  SetInitialBounds(0,0,50,23);
  ParentColor := false;
  TabStop := true;
end;

{-----------------------------------------------------------------------------}
destructor TCustomFloatSpinEdit.Destroy;
begin
  inherited Destroy;
end;

procedure TCustomFloatSpinEdit.FinalizeWnd;
begin
  GetValue;
  inherited FinalizeWnd;
end;

procedure TCustomFloatSpinEdit.SelectAll;
begin
  if Text <> '' then begin
    SetSelStart(0);
    SetSelLength(Length(Text));
  end;
end;

procedure TCustomFloatSpinEdit.ClearSelection;
begin
  if SelLength > 0 then
    SelText := '';
end;

procedure TCustomFloatSpinEdit.CopyToClipboard;
begin
  Clipboard.AsText := SelText;
end;

procedure TCustomFloatSpinEdit.CutToClipboard;
begin
  CopyToClipboard;
  ClearSelection;
end;

procedure TCustomFloatSpinEdit.PasteFromClipboard;
begin
  if Clipboard.HasFormat(CF_TEXT) then
    SelText := Clipboard.AsText;
end;

{ TCustomSpinEdit }

function TCustomSpinEdit.GetIncrement: integer;
begin
  Result:=round(FIncrement);
end;

function TCustomSpinEdit.GetMaxValue: integer;
begin
  Result:=round(FMaxValue);
end;

function TCustomSpinEdit.GetMinValue: integer;
begin
  Result:=round(FMinValue);
end;

function TCustomSpinEdit.GetValue: integer;
begin
  Result:=round(inherited GetValue);
end;

procedure TCustomSpinEdit.SetIncrement(const AValue: integer);
begin
  if Increment = AValue then exit;
  inherited SetIncrement(AValue);
end;

procedure TCustomSpinEdit.SetMaxValue(const AValue: integer);
begin
  if MaxValue=AValue then exit;
  inherited SetMaxValue(AValue);
end;

procedure TCustomSpinEdit.SetMinValue(const AValue: integer);
begin
  if MinValue=AValue then exit;
  inherited SetMinValue(AValue);
end;

procedure TCustomSpinEdit.SetValue(const AValue: integer);
begin
  if Value=AValue then exit;
  inherited SetValue(AValue);
end;

constructor TCustomSpinEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fDecimals:=0;
end;

// included by spin.pp
