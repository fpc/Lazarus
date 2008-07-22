{%MainUnit ../stdctrls.pp}

{******************************************************************************
                                TCustomStaticText
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
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
  ControlStyle := ControlStyle + [csOpaque, csReplicatable];
  SetInitialBounds(0,0,GetControlClassDefaultSize.X,GetControlClassDefaultSize.Y);
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

{------------------------------------------------------------------------------
  Method: TCustomStaticText.SetFocusControl
  Params: Val - new property value
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TCustomStaticText.SetFocusControl(Val: TWinControl);
begin
  if Val <> FFocusControl then begin
    FFocusControl:= Val;
    if Val <> nil then Val.FreeNotification(Self);
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

class function TCustomStaticText.GetControlClassDefaultSize: TPoint;
begin
  Result.X:=65;
  Result.Y:=17;
end;

procedure TCustomStaticText.SetStaticBorderStyle(Value : TStaticBorderStyle);
begin
  if FStaticBorderStyle <> Value then
  begin
    FStaticBorderStyle := Value;
    if HandleAllocated then
      TWSCustomStaticTextClass(WidgetsetClass).SetStaticBorderStyle(Self, Value);
  end;
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

// included by stdctrls.pp
