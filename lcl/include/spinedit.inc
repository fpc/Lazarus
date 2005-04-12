{%MainUnit ../spin.pp}

{
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

{-----------------------------------------------------------------------------}
procedure TCustomSpinEdit.UpdateControl;
begin
  if MaxValue<MinValue then fMaxValue:=MinValue;
  if Value<fMinValue then Value:=fMinValue;
  if Value>fMaxValue then Value:=fMaxValue;
  if (not HandleAllocated) then exit;
  if ([csLoading,csDestroying]*ComponentState<>[]) then begin
    fValueNeedsUpdate:=true;
    exit;
  end;
  TWSCustomSpinEditClass(WidgetSetClass).UpdateControl(Self);
  fValueNeedsUpdate:=false;
end;

function TCustomSpinEdit.ValueIsStored: boolean;
begin
  Result:=true; // fpc bug, default value is always 0
end;

procedure TCustomSpinEdit.CMTextChanged(var Message: TLMessage);
begin
  if Value=fLastValueOnChange then exit;
  fLastValueOnChange:=Value;
  Modified := True;
  FValueEmpty:=false;
  if HandleAllocated and (not (csLoading in ComponentState)) then Change;
end;

procedure TCustomSpinEdit.SetMaxValue(const AValue: single);
begin
  if FMaxValue=AValue then exit;
  FMaxValue:=AValue;
  UpdateControl;
end;

function TCustomSpinEdit.ClimbRateIsStored: boolean;
begin
  Result := FClimbRate <> 1;
end;

function TCustomSpinEdit.GetModified: Boolean;
begin
  Result := FModified;
end;

function TCustomSpinEdit.GetSelLength: integer;
begin
  if HandleAllocated then
    FSelLength := TWSCustomSpinEditClass(WidgetSetClass).GetSelLength(Self);
  Result:= FSelLength;
end;

function TCustomSpinEdit.GetSelStart: integer;
begin
  if HandleAllocated then
    FSelStart:= TWSCustomSpinEditClass(WidgetSetClass).GetSelStart(Self);
  Result:= FSelStart;
end;

function TCustomSpinEdit.GetSelText: String;
begin
  Result:= Copy(Text, SelStart + 1, SelLength)
end;

function TCustomSpinEdit.MaxValueIsStored: boolean;
begin
  Result:=true; // fpc bug, default value is always 0
end;

function TCustomSpinEdit.MinValueIsStored: boolean;
begin
  Result:=true; // fpc bug, default value is always 0
end;


{-----------------------------------------------------------------------------}

procedure TCustomSpinEdit.SetMinValue(const AValue: single);
begin
  if FMinValue=AValue then exit;
  FMinValue:=AValue;
  UpdateControl;
end;

procedure TCustomSpinEdit.SetModified(const AValue: Boolean);
begin
  FModified := AValue;
end;

procedure TCustomSpinEdit.SetSelLength(const AValue: integer);
begin
  FSelLength:= AValue;
  if HandleAllocated then
    TWSCustomSpinEditClass(WidgetSetClass).SetSelLength(Self, FSelLength);
end;

procedure TCustomSpinEdit.SetSelStart(const AValue: integer);
begin
  FSelStart:= AValue;
  if HandleAllocated then
    TWSCustomSpinEditClass(WidgetSetClass).SetSelStart(Self, FSelStart);
end;

procedure TCustomSpinEdit.SetSelText(const AValue: String);
var
  OldText, NewText: string;
begin
  OldText:=Text;
  NewText:=LeftStr(OldText,SelStart)+AValue
             +RightStr(OldText,length(OldText)-SelStart-SelLength);
  Text:=NewText;
end;

procedure TCustomSpinEdit.SetValueEmpty(const AValue: boolean);
begin
  if FValueEmpty=AValue then exit;
  FValueEmpty:=AValue;
  UpdateControl;
end;

procedure TCustomSpinEdit.SetClimbRate(const Num : Single);
begin
  if fClimbRate = Num then exit;
  fClimbRate := Num;
  UpdateControl;
end;

procedure TCustomSpinEdit.InitializeWnd;
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

procedure TCustomSpinEdit.Loaded;
begin
  inherited Loaded;
  if fValueNeedsUpdate then UpdateControl;
end;

procedure TCustomSpinEdit.Change;
begin
  if [csLoading,csDestroying,csDesigning]*ComponentState<>[] then exit;
  EditingDone;
  if Assigned(FOnChange) then FOnChange(Self);
end;

{-----------------------------------------------------------------------------}
Procedure TCustomSpinEdit.SetValue(const num : Single);
begin
  if FValue = Num then exit;
  FValue := Num;
  fLastValueOnChange := FValue;
  fValueNeedsUpdate:=true;
  UpdateControl;
end;

{-----------------------------------------------------------------------------}
Function TCustomSpinEdit.GetValue: Single;
begin
  if HandleAllocated and (not fValueNeedsUpdate) then begin
    FValue := TWSCustomSpinEditClass(WidgetSetClass).GetValue(Self);
  end;
  Result := fValue;
end;

{-----------------------------------------------------------------------------}
procedure TCustomSpinEdit.SetDecimals(Num : Integer);
begin
  if fDecimals = Num then exit;
  fDecimals := Num;
  UpdateControl;
end;

{-----------------------------------------------------------------------------}
constructor TCustomSpinEdit.Create(TheOwner : TComponent);
begin
  inherited Create(TheOwner);
  fCompStyle := csSpinEdit;
  
  fClimbRate := 1;
  fDecimals := 2;
  fValue := 0;
  fValueNeedsUpdate := true;
  fMinValue := 0;
  fMaxValue := 100;

  SetInitialBounds(0,0,50,20);
  ParentColor := false;
  TabStop := true;
end;

{-----------------------------------------------------------------------------}
destructor TCustomSpinEdit.Destroy;
begin
  inherited Destroy;
end;

procedure TCustomSpinEdit.DestroyComponent;
begin
  GetValue;
  inherited DestroyComponent;
end;

procedure TCustomSpinEdit.SelectAll;
begin
  if Text <> '' then begin
    SetSelStart(0);
    SetSelLength(Length(Text));
  end;
end;

procedure TCustomSpinEdit.ClearSelection;
begin
  if SelLength > 0 then
    SelText := '';
end;

procedure TCustomSpinEdit.CopyToClipboard;
begin
  Clipboard.AsText := SelText;
end;

procedure TCustomSpinEdit.CutToClipboard;
begin
  CopyToClipboard;
  ClearSelection;
end;

procedure TCustomSpinEdit.PasteFromClipboard;
begin
  if Clipboard.HasFormat(CF_TEXT) then
    SelText := Clipboard.AsText;
end;

// included by spin.pp

