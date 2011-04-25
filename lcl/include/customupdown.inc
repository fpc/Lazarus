{%MainUnit ../comctrls.pp}
{ TCustomUpDown

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

Problems -
  - Doesn't draw Themed Arrows/doesn't match system colors
  - Associate Key down and Tabbing(VK_Up, VK_Down)
}
Type
  { TUpDownButton }

  TUpDownButton = Class(TSpeedButton)
  private
    BTimer : TTimer;
    FUpDown : TCustomUpDown;
    FButtonType : TUDBtnType;
  protected
    procedure ButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoubleClick(Sender: TObject);
  public
    constructor CreateWithParams(UpDown : TCustomUpDown;
      ButtonType : TUDBtnType);

    procedure Click; override;
    procedure Paint; override;
  end;

procedure TUpDownButton.ButtonMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then begin
    With FUpDown do begin
      BTimerProc := @Self.Click;
      BTimerBounds := Bounds(Self.ClientOrigin.X, Self.ClientOrigin.Y,
        Self.Width,Self.Height);
      If Not Assigned(BTimer) then
        BTimer := TTimer.Create(FUpDown);
      With BTimer do begin
        Enabled := False;
        Interval := 300;
        OnTimer := @BTimerExec;
        Enabled := True;
      end;
    end;
  end;
end;

procedure TUpDownButton.ButtonMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  With FUpDown do
    If Assigned(BTimer) then begin
      BTimer.Free;
      BTimer := nil;
      BTimerBounds := Rect(0,0,0,0);
      BTimerProc := nil;
    end;
end;

procedure TUpDownButton.DoubleClick(Sender: TObject);
begin
  Click;
end;

procedure TUpDownButton.Click;
begin
  With FUpDown do begin
    If not CanChange then
      exit;
    Case FButtonType of
      btPrev :
        begin
          If Position - Increment >= Min then
            Position := Position - Increment
          else
            If Wrap then
              Position := Max + (Position - Increment - Min) + 1
          else
            Position := Min;
        end;
      btNext :
        begin
          If Position + Increment <= Max then
            Position := Position + Increment
          else
            If Wrap then
              Position := Min + (Position + Increment - Max) - 1
          else
            Position := Max;
        end;
        
    end;
    Click(FButtonType);
  end;
end;

constructor TUpDownButton.CreateWithParams(UpDown : TCustomUpDown;
  ButtonType : TUDBtnType);
begin
  Inherited Create(UpDown);
  FUpDown := UpDown;
  FButtonType := ButtonType;

  Parent := FUpDown;
  ControlStyle := ControlStyle + [csNoFocus, csNoDesignSelectable];
  OnMouseDown := @ButtonMouseDown;
  OnMouseUp := @ButtonMouseUp;
  OnDblClick := @DoubleClick;
end;

procedure TUpDownButton.Paint;
var
  tmp : double;
  ax, ay, ah, aw : integer;
  j : integer;
begin
  Inherited Paint;

  if Enabled then
    Canvas.Pen.Color := clBtnText //Not perfect, but it works
  else
    Canvas.Pen.Color := clGrayText;

  ah := height div 2;
  aw := width div 2;

  if (FUpDown.Orientation = udHorizontal) then begin
    tmp := double(ah+1)/2;
    if (tmp > aw) then begin
      ah := 2*aw - 1;
      aw := (ah+1) div 2;
    end
    else begin
      aw := RoundToInt(tmp);
      ah := 2*aw - 1;
    end;
    aw := max(aw, 3);
    ah := max(ah, 5);
  end
  else begin
    tmp := double(aw+1)/2;

    if (tmp > ah) then begin
      aw := 2*ah - 1;
      ah := (aw+1) div 2;
    end
    else begin
      ah := RoundToInt(tmp);
      aw := 2*ah - 1;
    end;
    ah := max(ah, 3);
    aw := max(aw, 5);
  end;

  ax := (width - aw) div 2;
  ay := (height - ah) div 2;

  Case FButtonType of
    btPrev :
      begin
        If FUpDown.Orientation = udVertical then begin
          for j := 0 to aw div 2 do begin
            Canvas.MoveTo(ax + j, ay + j);
            Canvas.LineTo(ax + aw - j, ay + j);
          end;
        end
        else
          for j := 0 to ah div 2 do begin
            Canvas.MoveTo(ax + aw - j - 2, ay + j);
            Canvas.LineTo(ax + aw - j - 2, ay + ah - j - 1);
          end;
      end;
    btNext :
      begin
        If FUpDown.Orientation = udVertical then begin
          for j := 0 to aw div 2 do begin
            Canvas.MoveTo(ax + j, ay + ah - j - 1);
            Canvas.LineTo(ax + aw - j, ay + ah - j - 1);
          end;
        end
        else
          for j := 0 to ah div 2 do begin
            Canvas.MoveTo(ax + j, ay + j);
            Canvas.LineTo(ax + j, ay + ah - j - 1);
          end;
     end
  end;
end;

{ TCustomUpDown }

constructor TCustomUpDown.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle  - [csDoubleClicks] +
                  [csClickEvents, csOpaque, csReplicatable, csNoFocus];
  FOrientation := udVertical;
  MinBtn := TUpDownButton.CreateWithParams(Self, btPrev);
  MaxBtn := TUpDownButton.CreateWithParams(Self, btNext);
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
  BTimerProc := nil;
  BTimerBounds := Rect(0,0,0,0);
  FArrowKeys := True;
  FMax := 100;
  FIncrement := 1;
  FAlignButton := udRight;
  FThousands := True;
end;

destructor TCustomUpDown.Destroy;
begin
  FAssociate := nil;
  inherited destroy;
end;

procedure TCustomUpDown.BTimerExec(Sender : TObject);
begin
  If Assigned(BTimerProc)
     and PtInRect(BTimerBounds,Mouse.CursorPos) then begin
    if TTimer(Sender).Interval > 100
      then TTimer(Sender).Interval := TTimer(Sender).Interval - 25;
    BTimerProc;
  end;
end;

procedure TCustomUpDown.UpdateUpDownPositionText;
begin
  if (not (csDesigning in ComponentState)) and (FAssociate <> nil) 
  then begin
    if Thousands 
    then FAssociate.Caption := FloatToStrF(FPosition, ffNumber, 0, 0)
    else FAssociate.Caption := IntToStr(FPosition);
  end;
end;

class procedure TCustomUpDown.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCustomUpDown;
end;

procedure TCustomUpDown.UpdateOrientation;
begin
  If FOrientation = udHorizontal then begin
    MinBtn.SetBounds(0,0,ClientWidth div 2,ClientHeight);
    MaxBtn.SetBounds(ClientWidth div 2,0,ClientWidth div 2,ClientHeight);
  end
  else begin
    MaxBtn.SetBounds(0,0,ClientWidth,ClientHeight div 2);
    MinBtn.SetBounds(0,ClientHeight div 2,ClientWidth,ClientHeight div 2);
  end;
end;

procedure TCustomUpDown.UpdateAlignButtonPos;
var
  NewWidth: Integer;
  NewLeft: Integer;
  NewHeight: Integer;
  NewTop: Integer;
begin
  If Assigned(Associate) then begin
    if FAlignButton in [udLeft,udRight] then begin
      NewWidth := Width;
      NewHeight := Associate.Height;
      If FAlignButton = udLeft then
        NewLeft := Associate.Left - NewWidth
      else
        NewLeft := Associate.Left + Associate.Width;
      NewTop := Associate.Top;
    end else begin
      NewWidth := Associate.Width;
      NewHeight := Height;
      NewLeft := Associate.Left;
      If FAlignButton = udTop then
        NewTop := Associate.Top - NewHeight
      else
        NewTop := Associate.Top + Associate.Height;
    end;
    SetBounds(NewLeft,NewTop,NewWidth,NewHeight);
  end;
end;

{procedure TCustomUpDown.ChangeBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  ARect : TRect;
begin
  If InheritedChangeBounds or (csLoading in ComponentState) then
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
end;}

function TCustomUpDown.CanChange: Boolean;
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
  OtherControl: TControl;
begin
  // check that no other updown component is associated to the new Associate
  if (Value <> FAssociate) and (Value<>nil) then
    for I := 0 to Parent.ControlCount - 1 do begin
      OtherControl:=Parent.Controls[I];
      if (OtherControl is TCustomUpDown) and (OtherControl <> Self) then
        if TCustomUpDown(OtherControl).Associate = Value then
          raise Exception.CreateFmt(rsIsAlreadyAssociatedWith,
                                    [Value.Name,OtherControl.Name]);
     end;

  // disconnect old Associate
  if FAssociate <> nil then
  begin
    FAssociate.RemoveAllHandlersOfObject(Self);
    FAssociate := nil;
  end;

  // connect new Associate
  if (Value <> nil) and (Value.Parent = Self.Parent)
  and not (Value is TCustomUpDown) and not (Value is TCustomTreeView)
  and not (Value is TCustomListView)
  then
  begin
    FAssociate := Value;
    UpdateUpDownPositionText;
    UpdateAlignButtonPos;
    FAssociate.AddHandlerOnKeyDown(@AssociateKeyDown,true);
    FAssociate.AddHandlerOnChangeBounds(@OnAssociateChangeBounds,true);
  end;
end;

procedure TCustomUpDown.AssociateKeyDown(Sender: TObject; var Key: Word;
  ShiftState : TShiftState);
begin
  If ArrowKeys and (ShiftState = []) then begin
    case FOrientation of
      udVertical:
        case Key of
          VK_Up:
            TCustomSpeedButton(MaxBtn).Click;
          VK_Down:
            TCustomSpeedButton(MinBtn).Click;
        end;
      udHorizontal:
        case Key of
          VK_Left:
            TCustomSpeedButton(MinBtn).Click;
          VK_Right:
            TCustomSpeedButton(MaxBtn).Click;
        end;
    end;
  end
end;

procedure TCustomUpDown.OnAssociateChangeBounds(Sender: TObject);
begin
  UpdateAlignButtonPos;
end;

procedure TCustomUpDown.DoOnResize;
begin
  inherited DoOnResize;
  UpdateOrientation;
end;

procedure TCustomUpDown.SetEnabled(Value: Boolean);
begin
  MinBtn.Enabled := Value;
  MaxBtn.Enabled := Value;
  inherited SetEnabled(Value);
end;

class function TCustomUpDown.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 17;
  Result.CY := 31;
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
    str := Trim(Associate.Caption);
    InvalidNumber := str = '';
    For I := Length(str) downto 1 do
      if str[I] = DefaultFormatSettings.ThousandSeparator then
        Delete(Str,I,1)
      else if str[I] in ['0'..'9'] then
      else begin
        InvalidNumber := True;
        Break;
      end;
    If not InvalidNumber then
      AV := SmallInt(TruncToInt(StrToFloat(str)))
    else begin
      Result := FPosition;
      Exit;
    end;
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
begin
  if FPosition = Value then exit;
  FPosition := Value;
  UpdateUpDownPositionText;
end;

procedure TCustomUpDown.SetOrientation(Value: TUDOrientation);
begin
  if FOrientation = Value then exit;
  FOrientation := Value;
  UpdateOrientation;
end;

procedure TCustomUpDown.SetAlignButton(Value: TUDAlignButton);
begin
  if FAlignButton = Value then exit;
  FAlignButton := Value;
  UpdateAlignButtonPos;
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

// included by comctrls.pp


