{%MainUnit ../extctrls.pp}

{******************************************************************************
                                 TBoundLabel
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

class procedure TCustomLabeledEdit.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCustomLabeledEdit;
end;

procedure TCustomLabeledEdit.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FEditLabel = nil then exit;
  DoPositionLabel; 
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

procedure TCustomLabeledEdit.DoPositionLabel;
begin
  if FEditLabel = nil then exit;
  if Parent<>nil then
    Parent.DisableAlign; 
  //DebugLn(['TCustomLabeledEdit.DoPositionLabel ']);  
  FEditLabel.Parent := Parent;
  FEditLabel.Visible := Visible;
  case FLabelPosition of
    lpAbove: 
      begin
        FEditLabel.Anchors:=[akLeft,akBottom]; 
        FEditLabel.AnchorParallel(akLeft,0,Self); 
        FEditLabel.AnchorToNeighbour(akBottom,FLabelSpacing,Self); 
      end; 
    lpBelow: 
      begin
        FEditLabel.Anchors:=[akLeft,akTop]; 
        FEditLabel.AnchorParallel(akLeft,0,Self); 
        FEditLabel.AnchorToNeighbour(akTop,FLabelSpacing,Self); 
      end; 
    lpLeft : 
      begin
        FEditLabel.Anchors:=[akRight,akTop]; 
        FEditLabel.AnchorToNeighbour(akRight,FLabelSpacing,Self); 
        FEditLabel.AnchorVerticalCenterTo(Self); 
      end; 
    lpRight:
      begin
        FEditLabel.Anchors:=[akLeft,akTop]; 
        FEditLabel.AnchorToNeighbour(akLeft,FLabelSpacing,Self); 
        FEditLabel.AnchorVerticalCenterTo(Self); 
      end; 
  end;
  if Parent<>nil then
    Parent.EnableAlign; 
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
end;

procedure TCustomLabeledEdit.CreateInternalLabel;
begin
  if FEditLabel<>nil then exit;
  FEditLabel := TBoundLabel.Create(Self);
  //Include(FEditLabel.ComponentStyle,csSubComponent);
  FEditLabel.ControlStyle := FEditLabel.ControlStyle + [csNoDesignSelectable];
  FEditLabel.FreeNotification(Self);
  FEditLabel.FocusControl := Self;
end;

// included by extctrls.pp
