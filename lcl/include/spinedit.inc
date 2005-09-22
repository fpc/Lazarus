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
procedure TCustomFloatSpinEdit.UpdateControl;
begin
  if MaxValue<MinValue then fMaxValue:=MinValue;
  if Value<fMinValue then Value:=fMinValue;
  if Value>fMaxValue then Value:=fMaxValue;
  if (not HandleAllocated) then exit;
  if ([csLoading,csDestroying]*ComponentState<>[]) then begin
    fValueNeedsUpdate:=true;
    exit;
  end;
  TWSCustomFloatSpinEditClass(WidgetSetClass).UpdateControl(Self);
  fValueNeedsUpdate:=false;
end;

function TCustomFloatSpinEdit.ValueIsStored: boolean;
begin
  Result:=true; // fpc bug, default value is always 0
end;

procedure TCustomFloatSpinEdit.TextChanged;
begin
  if Value=fLastValueOnChange then exit;
  fLastValueOnChange:=Value;
  Modified := True;
  FValueEmpty:=false;
  if HandleAllocated and (not (csLoading in ComponentState)) then Change;
end;

procedure TCustomFloatSpinEdit.SetMaxValue(const AValue: single);
begin
  if FMaxValue=AValue then exit;
  FMaxValue:=AValue;
  UpdateControl;
end;

function TCustomFloatSpinEdit.ClimbRateIsStored: boolean;
begin
  Result := FClimbRate <> 1;
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

procedure TCustomFloatSpinEdit.SetClimbRate(const Num : Single);
begin
  if fClimbRate = Num then exit;
  fClimbRate := Num;
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
  if fValueNeedsUpdate then UpdateControl;
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
  fLastValueOnChange := FValue;
  fValueNeedsUpdate:=true;
  UpdateControl;
end;

{-----------------------------------------------------------------------------}
Function TCustomFloatSpinEdit.GetValue: Single;
begin
  if HandleAllocated and (not fValueNeedsUpdate) then begin
    FValue := TWSCustomFloatSpinEditClass(WidgetSetClass).GetValue(Self);
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
destructor TCustomFloatSpinEdit.Destroy;
begin
  inherited Destroy;
end;

procedure TCustomFloatSpinEdit.DestroyComponent;
begin
  GetValue;
  inherited DestroyComponent;
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

function TCustomSpinEdit.GetClimbRate: integer;
begin
  Result:=round(FClimbRate);
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

procedure TCustomSpinEdit.SetClimbRate(const AValue: integer);
begin
  if ClimbRate=AValue then exit;
  inherited SetClimbRate(AValue);
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

