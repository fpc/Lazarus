{ TCustomUpDown

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

Problems -
  - Trying to remove component in form editor freezes Lazarus
  - Doesn't draw Arrows
  - Associate Key down and Tabbing(VK_Up, VK_Down)
}

constructor TCustomUpDown.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCompStyle := csPanel;
  ControlStyle := ControlStyle  - [csDoubleClicks] +
    [csClickEvents, csOpaque,  csReplicatable, csNoFocus];
  MinBtn := TSpeedButton.Create(Self);
  With TSpeedButton(MinBtn) do begin
    Parent := Self;
    TabStop := False;
    ControlStyle := MinBtn.ControlStyle + [csNoFocus];
    OnMouseDown := @MinBtnMouseDown;
    OnMouseUp := @BtnMouseUp;
    Show;
  end;
  MaxBtn := TSpeedButton.Create(Self);
  With TSpeedButton(MaxBtn) do begin
    Parent := Self;
    TabStop := False;
    ControlStyle := MaxBtn.ControlStyle + [csNoFocus];
    OnMouseDown := @MaxBtnMouseDown;
    OnMouseUp := @BtnMouseUp;
    Show;
  end;
  InheritedChangeBounds := True;
  SetBounds(0,0,17,31);
  InheritedChangeBounds := False;
  BTimerProc := nil;
  BTimerBounds := Rect(0,0,0,0);
  FArrowKeys := True;
  FMax := 100;
  FIncrement := 1;
  FAlignButton := udRight;
  FOrientation := udVertical;
  FThousands := True;
end;

destructor TCustomUpDown.Destroy;
begin
  FAssociate := nil;
  InheritedChangeBounds := True;
  inherited destroy;
end;

procedure TCustomUpDown.ChangeBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  ARect : TRect;
begin
  If InheritedChangeBounds then
    Inherited ChangeBounds(ALeft, ATop, AWidth, AHeight)
  else begin
    InheritedChangeBounds := True;
    ARect := ClientRect;
    InvalidateRect(Handle, @ARect, False);
    SetAlignButton(FAlignButton);
    SetOrientation(FOrientation);
    SetPosition(FPosition);
    InheritedChangeBounds := False;
    Inherited ChangeBounds(ALeft, ATop, AWidth, AHeight);
  end;
end;

Procedure TCustomUpDown.MinBtnClick;
begin
  If not CanChange then
    exit;
  If Position - Increment >= Min then
    Position := Position - Increment
  else
    If Wrap then
      Position := Max + (Position - Increment - Min) + 1
    else
      Position := Min;
  Click(btPrev);
end;

Procedure TCustomUpDown.BTimerExec(Sender : TObject);
begin
  If Assigned(BTimerProc) then
    If PtInRect(BTimerBounds,Mouse.CursorPos) then
      BTimerProc;
end;

Procedure TCustomUpDown.MinBtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  BTimerProc := @MinBtnClick;
  BTimerProc;
  BTimerBounds := Bounds(MinBtn.ClientOrigin.X,
    MinBtn.ClientOrigin.Y, MinBtn.Width,MinBtn.Height);
  If Not Assigned(BTimer) then
    BTimer := TTimer.Create(Self);;
  With BTimer do begin
    Enabled := False;
    Interval := 100;
    OnTimer := @BTimerExec;
    Enabled := True;
  end;
end;

Procedure TCustomUpDown.MaxBtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  BTimerProc := @MaxBtnClick;
  BTimerProc;
  BTimerBounds := Bounds(MaxBtn.ClientOrigin.X,
    MaxBtn.ClientOrigin.Y, MaxBtn.Width,MaxBtn.Height);
  If Not Assigned(BTimer) then
    BTimer := TTimer.Create(Self);
  With BTimer do begin
    Enabled := False;
    Interval := 100;
    OnTimer := @BTimerExec;
    Enabled := True;
  end;
end;

Procedure TCustomUpDown.BtnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  If Assigned(BTimer) then begin
    BTimer.Free;
    BTimer := nil;
    BTimerBounds := Rect(0,0,0,0);
    BTimerProc := nil;
  end;
end;

Procedure TCustomUpDown.MaxBtnClick;
begin
  If not CanChange then
    exit;
  If Position + Increment <= Max then
    Position := Position + Increment
  else
    If Wrap then
      Position := Min + (Position + Increment - Max) - 1
    else
      Position := Max;
  Click(btNext);
end;

Function TCustomUpDown.CanChange: Boolean;
begin
  Result := True;
  if Assigned(FOnChanging) then
    FOnChanging(Self, Result);
end;

procedure TCustomUpDown.Click(Button: TUDBtnType);
begin
  if Assigned(FOnClick) then FOnClick(Self, Button);
end;

procedure TCustomUpDown.SetAssociate(Value: TWinControl);
var
  I: Integer;

  function IsClass(ClassType: TClass; const Name: string): Boolean;
  begin
    Result := True;
    while ClassType <> nil do
    begin
      if ClassType.ClassNameIs(Name) then Exit;
      ClassType := ClassType.ClassParent;
    end;
    Result := False;
  end;

begin
  if Value <> nil then
    for I := 0 to Parent.ControlCount - 1 do
      if (Parent.Controls[I] is TCustomUpDown) and
        (Parent.Controls[I] <> Self)
      then
        if TCustomUpDown(Parent.Controls[I]).Associate = Value then
          raise Exception.CreateFmt('%s is already associated with %s',
            [Value.Name, Parent.Controls[I].Name]);

  if FAssociate <> nil then
  begin
    FAssociate.OnKeyDown := OldKeyDown;
    OldKeyDown := nil;
    FAssociate := nil;
  end;

  if (Value <> nil) and (Value.Parent = Self.Parent) and
    not (Value is TCustomUpDown) and
    not (Value is TCustomTreeView) and not (Value is TCustomListView)
  then
  begin
    FAssociate := Value;
    SetOrientation(FOrientation);
    SetPosition(FPosition);
    OldKeyDown := Value.OnKeyDown;
    Value.OnKeyDown := @AssociateKeyDown;
    Value.SetText(IntToStr(FPosition));
  end;
end;

Procedure TCustomUpDown.AssociateKeyDown(Sender: TObject; var Key: Word; ShiftState : TShiftState);
begin
  If Assigned(OldKeyDown) then
    OldKeyDown(Sender,Key, ShiftState);
  If ArrowKeys then
    If ShiftState = [] then
      Case Key of
        VK_Up,
        VK_Right:
          MaxBtnClick;
        VK_Down,
        VK_Left:
          MinBtnClick;
      end;
end;

procedure TCustomUpDown.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FAssociate) then
    if HandleAllocated then
      SetAssociate(nil);
end;

function TCustomUpDown.GetPosition: SmallInt;
var
  av,I : Smallint;
  str : string;
  InvalidNumber : Boolean;
begin
  If Associate <> nil then begin
    str := Associate.GetText;
    InvalidNumber := False;
    For I := Length(str) downto 1 do
      case str[I] of
        ',' : Delete(Str,I,1);
        '0'..'9':begin end;
        else
          InvalidNumber := True;
      end;
    If not InvalidNumber then
      AV := Trunc(StrToFloat(str))
    else begin
      AV := FPosition;
      Position := AV - 1;
      Position := AV;
      Result := FPosition;
      exit;
    end;
    If AV <> FPosition then
      FPosition := AV - 1;
    If AV > FMax then
      AV := FMax;
    If AV < FMin then
      AV := FMin;
    Position := AV;
  end;
  Result := FPosition;
end;

procedure TCustomUpDown.SetMin(Value: SmallInt);
begin
  if Value <> FMin then
  begin
    FMin := Value;
    If FPosition < FMin then
      Position := FMin;
  end;
end;

procedure TCustomUpDown.SetMax(Value: SmallInt);
begin
  if Value <> FMax then
  begin
    FMax := Value;
    If FPosition > FMax then
      Position := FMax;
  end;
end;

procedure TCustomUpDown.SetIncrement(Value: Integer);
begin
  if Value <> FIncrement then
    FIncrement := Value;
end;

procedure TCustomUpDown.SetPosition(Value: SmallInt);
var
  str : String;
begin
  FPosition := Value;
  If Thousands then
    Str := FloatToStrF(FPosition, ffNumber, 0, 0)
  else
    str := IntToStr(FPosition);
  if (not (csDesigning in ComponentState)) and (FAssociate <> nil) then
    FAssociate.SetText(Str);
end;

procedure TCustomUpDown.SetOrientation(Value: TUDOrientation);
begin
  FOrientation := Value;
  If Value = udHorizontal then begin
    MinBtn.SetBounds(0,0,Width div 2,Height);
    MaxBtn.SetBounds(Width div 2,0,Width div 2, ClientHeight);
  end
  else begin
    MaxBtn.SetBounds(0,0,Width,Height div 2);
    MinBtn.SetBounds(0,Height div 2,Width, ClientHeight div 2);
  end;
  SetAlignButton(FAlignButton);
end;

procedure TCustomUpDown.SetAlignButton(Value: TUDAlignButton);
begin
  FAlignButton := Value;
  If assigned(Associate) then begin
    If Value = udLeft then
      Left := Associate.Left - Width
    else
      Left := Associate.Left + Associate.Width;
    Height := Associate.Height;
    Top := Associate.Top;
  end;
end;

procedure TCustomUpDown.SetArrowKeys(Value: Boolean);
begin
  if Value <> FArrowKeys then
    FArrowKeys := Value;
end;

procedure TCustomUpDown.SetThousands(Value: Boolean);
begin
  if Value <> FThousands then
    FThousands := Value;
end;

procedure TCustomUpDown.SetWrap(Value: Boolean);
begin
  if Value <> FWrap then
    FWrap := Value;
end;
