{%MainUnit ../stdctrls.pp}

{******************************************************************************
                                TCustomStaticText
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
  SetInitialBounds(0, 0, 65, 17);
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

Procedure TCustomStaticText.SetStaticBorderStyle(Value : TStaticBorderStyle);

  procedure RaiseNotImplemented;
  begin
    raise Exception.Create('TCustomStaticText.SetStaticBorderStyle is not implemented yet');
  end;

begin
  If FStaticBorderStyle <> Value then begin
    if ([csDesigning,csLoading]*ComponentState=[csDesigning]) then
      RaiseNotImplemented;
    FStaticBorderStyle := Value;
    Invalidate;
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
