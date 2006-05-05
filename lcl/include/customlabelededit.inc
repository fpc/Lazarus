{%MainUnit ../extctrls.pp}

{******************************************************************************
                                 TBoundLabel
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

{ TCustomLabeledEdit }

procedure TCustomLabeledEdit.SetLabelPosition(const Value: TLabelPosition);
begin
  if Value=FLabelPosition then exit;
  FLabelPosition := Value;
  DoPositionLabel;
end;

procedure TCustomLabeledEdit.SetLabelSpacing(const Value: Integer);
begin
  if Value=FLabelSpacing then exit;
  FLabelSpacing:=Value;
  DoPositionLabel;
end;

procedure TCustomLabeledEdit.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FEditLabel = nil then exit;
  FEditLabel.Parent := Parent;
  FEditLabel.Visible := Visible;
end;

procedure TCustomLabeledEdit.SetName(const Value: TComponentName);
begin
  if (csDesigning in ComponentState)
  and ((FEditLabel.Text='') or
       (AnsiCompareText(FEditLabel.Caption, Name) = 0))
  then
    FEditLabel.Caption := Value;
  inherited SetName(Value);
  if csDesigning in ComponentState then
    Text := '';
end;

procedure TCustomLabeledEdit.DoSetBounds(ALeft, ATop, AWidth, AHeight: Integer
  );
begin
  inherited DoSetBounds(ALeft, ATop, AWidth, AHeight);
  DoPositionLabel;
end;

procedure TCustomLabeledEdit.DoPositionLabel;
var
  P: TPoint;
begin
  if FEditLabel = nil then exit;
  case FLabelPosition of
    lpAbove: P := Point(Left, Top - FEditLabel.Height - FLabelSpacing);
    lpBelow: P := Point(Left, Top + Height + FLabelSpacing);
    lpLeft : P := Point(Left - FEditLabel.Width - FLabelSpacing,
                    Top + ((Height - FEditLabel.Height) div 2));
    lpRight: P := Point(Left + Width + FLabelSpacing,
                    Top + ((Height - FEditLabel.Height) div 2));
  end;
  FEditLabel.SetBounds(P.x, P.y, FEditLabel.Width, FEditLabel.Height);
end;

procedure TCustomLabeledEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FEditLabel) and (Operation = opRemove) then
    FEditLabel := nil;
end;

procedure TCustomLabeledEdit.CMVisibleChanged(var Msg: TLMessage);
begin
  inherited CMVisiblechanged(Msg);
  if FEditLabel<>nil then
    FEditLabel.Visible:=Visible;
end;

procedure TCustomLabeledEdit.CMEnabledChanged(var Msg: TLMessage);
begin
  inherited CMEnabledChanged(Msg);
  if FEditLabel<>nil then
    FEditLabel.Enabled:=Enabled;
end;

constructor TCustomLabeledEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FLabelPosition := lpAbove;
  FLabelSpacing := 3;
  CreateInternalLabel;
  SetInitialBounds(0,0,80,25);
end;

procedure TCustomLabeledEdit.CreateInternalLabel;
begin
  if FEditLabel<>nil then exit;
  FEditLabel := TBoundLabel.Create(Self);
  Include(FEditLabel.ComponentStyle, csSubComponent);
  Include(FEditLabel.ControlStyle, csNoDesignSelectable);
  FEditLabel.FreeNotification(Self);
  FEditLabel.FocusControl := Self;
end;

// included by extctrls.pp
