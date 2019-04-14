{%MainUnit ../comctrls.pp}
{ TCustomUpDown

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

Problems -
  - Doesn't draw Themed Arrows/doesn't match system colors
  - Associate Key down and Tabbing(VK_Up, VK_Down)
}
Type
  { TUpDownButton }

  TUpDownButton = Class(TSpeedButton)
  private
    FMouseTimer : TTimer;
    FUpDown : TCustomUpDown;
    FButtonType : TUDBtnType;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer
      ); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure DblClick; override;
  public
    constructor CreateWithParams(UpDown : TCustomUpDown;
      ButtonType : TUDBtnType);

    procedure Click; override;
    procedure Paint; override;
  end;

procedure TUpDownButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then begin
    With FUpDown do begin
      FMouseTimerEvent := @Self.Click;
      FMouseDownBounds := Bounds(Self.ClientOrigin.X, Self.ClientOrigin.Y,
        Self.Width,Self.Height);
      If Not Assigned(FMouseTimer) then
        FMouseTimer := TTimer.Create(FUpDown);
      With FMouseTimer do begin
        Enabled := False;
        Interval := 300;
        OnTimer := @BTimerExec;
        Enabled := True;
      end;
    end;
  end;
end;

procedure TUpDownButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  With FUpDown do
    If Assigned(FMouseTimer) then begin
      FreeAndNil(FMouseTimer);
      FMouseDownBounds := Rect(0,0,0,0);
      FMouseTimerEvent := nil;
    end;
end;

procedure TUpDownButton.DblClick;
begin
  Click;
end;

procedure TUpDownButton.Click;
begin
  with FUpDown do
  begin
    FCanChangePos := Position;
    FCanChangeDir := updNone;

    case FButtonType of
      btPrev :
        begin
          FCanChangeDir := updDown;

          if FCanChangePos - Increment >= Min then
            FCanChangePos := FCanChangePos - Increment
          else
            if Wrap then
              FCanChangePos := Max + (FCanChangePos - Increment - Min) + 1
          else
            FCanChangePos := Min;
        end;
      btNext :
        begin
          FCanChangeDir := updUp;

          if FCanChangePos + Increment <= Max then
            FCanChangePos := FCanChangePos + Increment
          else
            If Wrap then
              FCanChangePos := Min + (FCanChangePos + Increment - Max) - 1
          else
            FCanChangePos := Max;
        end;
        
    end;
    if not CanChange then Exit;
    Position := FCanChangePos;

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
  FUseWS := IsWSComponentInheritsFrom(TCustomUpDown, TWSCustomUpDown);
  FOrientation := udVertical;

  if not FUseWS then begin
    FMinBtn := TUpDownButton.CreateWithParams(Self, btPrev);
    FMaxBtn := TUpDownButton.CreateWithParams(Self, btNext);
  end;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
  FArrowKeys := True;
  FMax := 100;
  FMinRepeatInterval := 100;
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
var
  AInterval:Integer;
begin
  If Assigned(FMouseTimerEvent)
     and PtInRect(FMouseDownBounds,Mouse.CursorPos) then begin
    AInterval := TTimer(Sender).Interval;
    if AInterval > FMinRepeatInterval then begin
      AInterval := AInterval - 25;
      if AInterval < FMinRepeatInterval then AInterval := FMinRepeatInterval;
      TTimer(Sender).Interval := AInterval;
    end;
    FMouseTimerEvent;
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

procedure TCustomUpDown.InitializeWnd;
begin
  inherited InitializeWnd;
  if not FUseWS then Exit;
  TWSCustomUpDownClass(WidgetSetClass).SetMaxPosition(Self, FMax);
  TWSCustomUpDownClass(WidgetSetClass).SetPosition(Self, FPosition);
  TWSCustomUpDownClass(WidgetSetClass).SetIncrement(Self, FIncrement);
  TWSCustomUpDownClass(WidgetSetClass).SetWrap(Self, FWrap);
  TWSCustomUpDownClass(WidgetSetClass).SetOrientation(Self, FOrientation);
  TWSCustomUpDownClass(WidgetSetClass).SetUseArrowKeys(Self, FArrowKeys);
end;

procedure TCustomUpDown.UpdateOrientation;
var
  d, r: Integer;
begin
  if FUseWS then Exit;

  If FOrientation = udHorizontal then begin
    d:=ClientWidth div 2;
    r:=ClientWidth mod 2;
    FMinBtn.SetBounds(0,0,d,ClientHeight);
    FMaxBtn.SetBounds(d+r,0,d,ClientHeight);
  end
  else begin
    d:=ClientHeight div 2;
    r:=ClientHeight mod 2;
    FMaxBtn.SetBounds(0,0,ClientWidth,d);
    FMinBtn.SetBounds(0,d+r,ClientWidth,d);
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

function TCustomUpDown.CanChange: Boolean;
begin
  Result := True;

  if Assigned(FOnChanging) then
    FOnChanging(Self, Result);

  if Assigned(FOnChangingEx) then
    FOnChangingEx(Self, Result, FCanChangePos, FCanChangeDir);
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
    FAssociate.AddHandlerOnEnabledChanged(@OnAssociateChangeEnabled,true);
    FAssociate.AddHandlerOnVisibleChanged(@OnAssociateChangeVisible,true);
    FAssociate.AddHandlerOnMouseWheel(@AssociateMouseWheel,true);
  end;
end;

procedure TCustomUpDown.AdjustPos(incPos: Boolean);
var
  anewpos: Integer;
begin
  if FUseWS then begin
    if incPos then anewpos := Position + Increment
    else anewpos := Position - Increment;

    if (anewpos < Min) then anewpos := Min
    else if (anewpos > Max) then anewpos := Max;
    SetPosition(anewpos);
  end else begin
    if incPos then TCustomSpeedButton(FMaxBtn).Click
    else TCustomSpeedButton(FMinBtn).Click;
  end;

end;

procedure TCustomUpDown.AssociateKeyDown(Sender: TObject; var Key: Word;
  ShiftState : TShiftState);
var
  ConsumeKey: Boolean;
begin
  ConsumeKey := False;
  if ArrowKeys and (ShiftState = []) then
  begin
    case FOrientation of
      udVertical:
        case Key of
          VK_Up:
            begin
              AdjustPos(True);
              ConsumeKey := True;
            end;
          VK_Down:
            begin
              AdjustPos(False);
              ConsumeKey := True;
            end;
        end;
      udHorizontal:
        case Key of
          VK_Left:
            begin
              AdjustPos(False);
              ConsumeKey := True;
            end;
          VK_Right:
            begin
              AdjustPos(True);
              ConsumeKey := True;
            end;
        end;
    end;
  end;
  if ConsumeKey then
    Key := 0;
end;

procedure TCustomUpDown.AssociateMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);

begin
  //debugln('TCustomUpDown.AssociateMouseWheel A: Handled = ',DbgS(Handled));
  if (WheelDelta > 0) then
  begin
    AdjustPos(True);
    Handled := True;
  end
  else if (WheelDelta < 0) then
  begin
    AdjustPos(False);
    Handled := True;
  end;
  //debugln('TCustomUpDown.AssociateMouseWheel End: Handled = ',DbgS(Handled));
end;

procedure TCustomUpDown.OnAssociateChangeBounds(Sender: TObject);
begin
  UpdateAlignButtonPos;
end;

procedure TCustomUpDown.OnAssociateChangeEnabled(Sender: TObject);
begin
  if Assigned(FAssociate) then
    SetEnabled(FAssociate.Enabled);
end;

procedure TCustomUpDown.OnAssociateChangeVisible(Sender: TObject);
begin
  if Assigned(FAssociate) then
    SetVisible(FAssociate.Visible);
end;

function TCustomUpDown.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  if not Result and not FUseWS then
    TCustomSpeedButton(FMinBtn).Click;
end;

function TCustomUpDown.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelUp(Shift, MousePos);
  if not Result and not FUseWS then
    TCustomSpeedButton(FMaxBtn).Click;
end;

function TCustomUpDown.DoMouseWheelLeft(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelLeft(Shift, MousePos);
  if not Result then
    if (Orientation=udHorizontal) and not FUseWS  then
      TCustomSpeedButton(FMinBtn).Click;
end;

function TCustomUpDown.DoMouseWheelRight(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelRight(Shift, MousePos);
  if not Result then
    if (Orientation=udHorizontal) and not FUseWS then
      TCustomSpeedButton(FMaxBtn).Click;
end;

procedure TCustomUpDown.DoSetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  inherited;
  UpdateOrientation;
end;

procedure TCustomUpDown.SetEnabled(Value: Boolean);
begin
  FMinBtn.Enabled := Value;
  FMaxBtn.Enabled := Value;
  inherited SetEnabled(Value);
end;

class function TCustomUpDown.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 17;
  Result.CY := 31;
end;

procedure TCustomUpDown.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  case Orientation of
  udHorizontal:
    begin
      PreferredWidth:=31;
      PreferredHeight:=17;
    end;
  udVertical:
    begin
      PreferredWidth:=17;
      PreferredHeight:=31;
    end;
  end;
end;

procedure TCustomUpDown.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FAssociate) then
    SetAssociate(nil);
end;

function TCustomUpDown.GetPosition: SmallInt;
var
  av,I : Integer;
  str : string;
  InvalidNumber : Boolean;
begin
  If Associate <> nil then begin
    str := Trim(Associate.Caption);
    str := StringReplace(str, DefaultFormatSettings.ThousandSeparator, '', [rfReplaceAll]);
    if not TryStrToInt(str, AV) then
    begin
      Result := FPosition;
      Exit;
    end;
    //this will also correct for AV > High(SmallInt) or AV < Low(SMallInt)
    If AV > FMax then
      AV := FMax;
    If AV < FMin then
      AV := FMin;
    Position := AV;
  end;
  Result := FPosition;
end;

function TCustomUpDown.GetFlat: Boolean;
begin
  if FUseWS then
    Result := false
  else if FMinBtn<>nil then
    Result := (FMinBtn as TSpeedButton).Flat
  else
    Result := False;
end;

procedure TCustomUpDown.SetMin(Value: SmallInt);
begin
  if Value <> FMin then
  begin
    FMin := Value;
    If FPosition < FMin then
      Position := FMin;
    if FUseWS then
      TWSCustomUpDownClass(WidgetSetClass).SetMinPosition(Self, FMin);
  end;
end;

procedure TCustomUpDown.SetMinRepeatInterval(AValue: Byte);
begin
  if FMinRepeatInterval = AValue then Exit;
  FMinRepeatInterval := AValue;
  if FMinRepeatInterval < 25 then FMinRepeatInterval := 25;
end;

procedure TCustomUpDown.SetMax(Value: SmallInt);
begin
  if Value <> FMax then
  begin
    FMax := Value;
    If FPosition > FMax then
      Position := FMax;
    if FUseWS then
      TWSCustomUpDownClass(WidgetSetClass).SetMaxPosition(Self, FMax);
  end;
end;

procedure TCustomUpDown.SetIncrement(Value: Integer);
begin
  if Value <> FIncrement then begin
    FIncrement := Value;
    if FUseWS then
      TWSCustomUpDownClass(WidgetSetClass).SetIncrement(Self, FIncrement);
  end;
end;

procedure TCustomUpDown.SetPosition(Value: SmallInt);
begin
  if FPosition = Value then exit;
  FPosition := Value;
  if FUseWS then
    TWSCustomUpDownClass(WidgetSetClass).SetPosition(Self, FPosition);
  UpdateUpDownPositionText;
end;

procedure TCustomUpDown.SetOrientation(Value: TUDOrientation);
begin
  if FOrientation = Value then exit;
  FOrientation := Value;
  if FUseWS then
    TWSCustomUpDownClass(WidgetSetClass).SetOrientation(Self, FOrientation);

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
  if Value <> FArrowKeys then begin
    FArrowKeys := Value;
    if FUseWS then
      TWSCustomUpDownClass(WidgetSetClass).SetUseArrowKeys(Self, Value);
  end;
end;

procedure TCustomUpDown.SetThousands(Value: Boolean);
begin
  if Value <> FThousands then
    FThousands := Value;
end;

procedure TCustomUpDown.SetFlat(Value: Boolean);
begin
  if FUseWS then Exit; // todo: not supported by WS yet
  if Flat = Value then Exit;

  (FMinBtn as TSpeedButton).Flat := Value;
  (FMaxBtn as TSpeedButton).Flat := Value;
end;

procedure TCustomUpDown.SetWrap(Value: Boolean);
begin
  if Value <> FWrap then
    FWrap := Value;
  if FUseWS then
    TWSCustomUpDownClass(WidgetSetClass).SetWrap(Self, FWrap);
end;

// included by comctrls.pp

