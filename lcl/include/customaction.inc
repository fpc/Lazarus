{%MainUnit ../actnlist.pas}

{
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

constructor TCustomAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := True;
  FImageIndex := -1;
  FVisible := True;
  FSecondaryShortCuts := nil; // no list as default
  FHelpType := htContext;
end;

destructor TCustomAction.Destroy;
begin
  FreeAndNil(FImage);
  FreeAndNil(FMask);
  if Assigned(FSecondaryShortCuts) then
    FreeAndNil(FSecondaryShortCuts);
  inherited Destroy;
end;

procedure TCustomAction.AssignTo(Dest: TPersistent);
var
  Target: TCustomAction;
begin
  if Dest=Self then exit;
  if Dest is TCustomAction then begin
    Target:=TCustomAction(Dest);
    Target.AutoCheck := AutoCheck;
    Target.Caption := Caption;
    Target.Category := Category;
    Target.Checked := Checked;
    Target.Enabled := Enabled;
    Target.GroupIndex := GroupIndex;
    Target.HelpContext := HelpContext;
    Target.HelpKeyword := HelpKeyword;
    Target.HelpType := HelpType;
    Target.Hint := Hint;
    Target.ImageIndex := ImageIndex;
    Target.SecondaryShortCuts := SecondaryShortCuts;
    Target.ShortCut := ShortCut;
    Target.Tag := Tag;
    Target.Visible := Visible;
    Target.OnExecute := OnExecute;
    Target.OnUpdate := OnUpdate;
    Target.OnChange := OnChange;
    Target.OnHint := OnHint;
  end else
    inherited AssignTo(Dest);
end;

procedure TCustomAction.SetAutoCheck(Value: Boolean);
var
  I: Integer;
begin
  if Value = FAutoCheck then exit;
  for I := 0 to FClients.Count - 1 do
    TActionLink(FClients[I]).SetAutoCheck(Value);
  FAutoCheck := Value;
  Change;
end;

procedure TCustomAction.SetCaption(const Value: TTranslateString);
var
  I: Integer;
begin
  if Value = FCaption then exit;
  for I := 0 to FClients.Count - 1 do
    TActionLink(FClients[I]).SetCaption(Value);
  FCaption := Value;
  Change;
end;

procedure TCustomAction.SetChecked(Value: Boolean);
var
  I: Integer;
  Action: TContainedAction;
begin
  if FChecking or (Value=FChecked) then exit;
  FChecking := True;
  try
    for I := 0 to FClients.Count - 1 do
      TActionLink(FClients[I]).SetChecked(Value);
    FChecked := Value;
    if (FGroupIndex > 0) and FChecked then
      for I := 0 to ActionList.ActionCount - 1 do
      begin
        Action := ActionList[I];
        if (Action <> Self)
        and (Action is TCustomAction)
        and (TCustomAction(Action).FGroupIndex = FGroupIndex) then
          TCustomAction(Action).Checked := False;
      end;
    Change;
  finally
    FChecking := False;
  end;
end;

procedure TCustomAction.SetEnabled(Value: Boolean);
var
  I: Integer;
begin
  if Value = FEnabled then exit;
  if ActionList<>nil then
  begin
    if ActionList.State = asSuspended then
    begin
      FEnabled := Value;
      exit;
    end;
    if ActionList.State = asSuspendedEnabled then
    begin
      // enable for Delphi compatibility
      Value := True;
    end;
  end;
  for I := 0 to FClients.Count - 1 do
    TActionLink(FClients[I]).SetEnabled(Value);
  FEnabled := Value;
  Change;
end;

procedure TCustomAction.SetGroupIndex(const Value: Integer);
var
  I: Integer;
begin
  if Value = FGroupIndex then exit;
  FGroupIndex := Value;
  for I := 0 to FClients.Count - 1 do
    TActionLink(FClients[I]).SetGroupIndex(Value);
  Change;
end;

procedure TCustomAction.SetHelpType(Value: THelpType);
var
  I: Integer;
begin
  if Value = FHelpType then exit;
  for I := 0 to FClients.Count -1 do
    TActionLink(FClients[I]).SetHelpType(Value);
  FHelpType := Value;
  Change;
end;

procedure TCustomAction.SetHelpKeyword(const Value: string);
var
  I: Integer;
begin
  if Value = FHelpKeyword then exit;
  for I := 0 to FClients.Count -1 do
    TActionLink(FClients[I]).SetHelpKeyword(Value);
  FHelpKeyword := Value;
  Change;
end;

procedure TCustomAction.SetHelpContext(Value: THelpContext);
var
  I: Integer;
begin
  if Value = FHelpContext then exit;
  for I := 0 to FClients.Count - 1 do
    TActionLink(FClients[I]).SetHelpContext(Value);
  FHelpContext := Value;
  Change;
end;

procedure TCustomAction.SetHint(const Value: TTranslateString);
var
  I: Integer;
begin
  if Value = FHint then exit;
  for I := 0 to FClients.Count - 1 do
    TActionLink(FClients[I]).SetHint(Value);
  FHint := Value;
  Change;
end;

procedure TCustomAction.SetImageIndex(Value: TImageIndex);
var
  I: Integer;
begin
  if Value = FImageIndex then exit;
  for I := 0 to FClients.Count - 1 do
    TActionLink(FClients[I]).SetImageIndex(Value);
  FImageIndex := Value;
  Change;
end;

procedure TCustomAction.SetShortCut(Value: TShortCut);
var
  I: Integer;
begin
  if Value = FShortCut then exit;
  for I := 0 to FClients.Count - 1 do
    TActionLink(FClients[I]).SetShortCut(Value);
  FShortCut := Value;
  Change;
end;

procedure TCustomAction.SetVisible(Value: Boolean);
var
  I: Integer;
begin
  if Value = FVisible then exit;
  for I := 0 to FClients.Count - 1 do
    TActionLink(FClients[I]).SetVisible(Value);
  FVisible := Value;
  Change;
end;

procedure TCustomAction.SetName(const Value: TComponentName);
var
  AutoChangeCaption: Boolean;
begin
  AutoChangeCaption := (Name = Caption)
    and ((Owner = nil)
          or not (csLoading in Owner.ComponentState));
  inherited SetName(Value);
  if AutoChangeCaption and (FClients.Count = 0) then
    Caption := Value;
end;

function TCustomAction.DoHint(var HintStr: string): Boolean;
begin
  Result := True;
  if Assigned(FOnHint) then FOnHint(HintStr, Result);
end;

function TCustomAction.Execute: Boolean;
begin
  Result := False;
  if Assigned(ActionList) and (ActionList.State <> asNormal) then Exit;
  Update;
  if FAutoCheck then
    Checked := not Checked;
  Result := Enabled and inherited Execute;
end;

function TCustomAction.GetSecondaryShortCuts: TShortCutList;
begin
  if FSecondaryShortCuts = nil then
    FSecondaryShortCuts := TShortCutList.Create;
  Result := FSecondaryShortCuts;
end;

procedure TCustomAction.SetSecondaryShortCuts(const Value: TShortCutList);
begin
  if FSecondaryShortCuts = nil then begin
    if (Value=nil) or (Value.Count=0) then exit;
    FSecondaryShortCuts := TShortCutList.Create;
  end;
  FSecondaryShortCuts.Assign(Value);
end;

function TCustomAction.IsSecondaryShortCutsStored: Boolean;
begin
  Result := Assigned(FSecondaryShortCuts) and (FSecondaryShortCuts.Count > 0);
end;

function TCustomAction.HandleShortCut: Boolean;
begin
  Result := Execute;
end;

// included by actnlist.pas
