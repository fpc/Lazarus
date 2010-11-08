{%MainUnit ../stdctrls.pp}

{******************************************************************************
                                TCustomStaticText
 ******************************************************************************

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
}

{------------------------------------------------------------------------------
  Method: TCustomStaticText.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TCustomStaticText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCompStyle := csStaticText;
  fAlignment := taLeftJustify;
  FShowAccelChar:= true;
  FStaticBorderStyle:=sbsNone;
  ControlStyle := ControlStyle + [csReplicatable];
  Transparent := True;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

{------------------------------------------------------------------------------
  Method: TCustomStaticText.GetLabelText
  Params: None
  Returns: Nothing

 ------------------------------------------------------------------------------}
function TCustomStaticText.GetLabelText: String;
begin
  Result := Caption;  
end;

procedure TCustomStaticText.RealSetText(const AValue: TCaption);
begin
  if Text=AValue then exit;
  inherited RealSetText(AValue);
  InvalidatePreferredSize;
  AdjustSize;
end;

{------------------------------------------------------------------------------
  procedure TCustomStaticText.Notification(AComponent: TComponent;
    Operation: TOperation);
 ------------------------------------------------------------------------------}
procedure TCustomStaticText.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FFocusControl) and (Operation = opRemove) then
    FFocusControl:= nil;
end;

{------------------------------------------------------------------------------
  Method: TCustomStaticText.SetAlignment
  Params: Value - new proterty value
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TCustomStaticText.SetAlignment(Value: TAlignment);
begin
  if fAlignment <> value then begin
    fAlignment:= value;
    if HandleAllocated then
      TWSCustomStaticTextClass(WidgetSetClass).SetAlignment(Self, Value);
  end;
end;

function TCustomStaticText.GetTransparent: Boolean;
begin
  Result := not (csOpaque in ControlStyle);
end;

{------------------------------------------------------------------------------
  Method: TCustomStaticText.SetFocusControl
  Params: Val - new property value
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TCustomStaticText.SetFocusControl(Val: TWinControl);
begin
  if Val <> FFocusControl then 
  begin
    if FFocusControl <> nil then
      FFocusControl.RemoveFreeNotification(Self);
    FFocusControl := Val;
    if Val <> nil then 
      Val.FreeNotification(Self);
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomStaticText.SetShowAccelChar
  Params: Val - new property value
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TCustomStaticText.SetShowAccelChar(Val: boolean);
begin
  if Val <> FShowAccelChar then begin
    FShowAccelChar:= Val;
    if HandleAllocated then begin
      TWSWinControlClass(WidgetSetClass).SetText(Self,Caption);
      InvalidatePreferredSize;
    end;
  end;
end;

function TCustomStaticText.DialogChar(var Message: TLMKey): boolean;
begin
  if IsAccel(Message.CharCode, Caption)
  and (FFocusControl <> nil) and (FFocusControl.CanFocus) then
  begin
    FFocusControl.SetFocus;
    Result := true;
  end else
    Result := inherited;
end;

class function TCustomStaticText.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 65;
  Result.CY := 17;
end;

procedure TCustomStaticText.SetStaticBorderStyle(Value : TStaticBorderStyle);
begin
  if FStaticBorderStyle <> Value then
  begin
    FStaticBorderStyle := Value;
    if HandleAllocated then
    begin
      TWSCustomStaticTextClass(WidgetsetClass).SetStaticBorderStyle(Self, Value);
      if AutoSize then
      begin
        InvalidatePreferredSize;
        AdjustSize;
      end;
    end;
  end;
end;

procedure TCustomStaticText.SetTransparent(const AValue: Boolean);
begin
  if Transparent = AValue then
    Exit;
  if AValue then
    ControlStyle := ControlStyle - [csOpaque]
  else
    ControlStyle := ControlStyle + [csOpaque];
  Invalidate;
end;

{------------------------------------------------------------------------------
  Method: TCustomStaticText.WMActivate
  Params: Message: Activation message
  Returns: Nothing

  Received when the label has a registered activation shortcut for focuscontrol.
 ------------------------------------------------------------------------------}
procedure TCustomStaticText.WMActivate(var Message: TLMActivate);
begin
  if (FFocusControl <> nil) and (FFocusControl.CanFocus) then
    FFocusControl.SetFocus;
end;

class procedure TCustomStaticText.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCustomStaticText;
end;

// included by stdctrls.pp
