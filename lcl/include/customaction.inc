{%MainUnit ../actnlist.pas}

{
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

constructor TCustomAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := True;
  FImageIndex := -1;
  FVisible := True;
  FSecondaryShortCuts := nil;
  FHelpType := htContext;
end;

destructor TCustomAction.Destroy;
begin
  FImage.Free;
  FMask.Free;
  if Assigned(FSecondaryShortCuts) then
    FreeAndNil(FSecondaryShortCuts);
  inherited Destroy;
end;

procedure TCustomAction.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomAction then
    with TCustomAction(Dest) do
    begin
      AutoCheck := Self.AutoCheck;
      Caption := Self.Caption;
      Category := Self.Category;
      Checked := Self.Checked;
      Enabled := Self.Enabled;
      GroupIndex := Self.GroupIndex;
      HelpContext := Self.HelpContext;
      HelpKeyword := Self.HelpKeyword;
      HelpType := Self.HelpType;
      Hint := Self.Hint;
      ImageIndex := Self.ImageIndex;
      SecondaryShortCuts := Self.SecondaryShortCuts;
      ShortCut := Self.ShortCut;
      Tag := Self.Tag;
      Visible := Self.Visible;
      OnExecute := Self.OnExecute;
      OnUpdate := Self.OnUpdate;
      OnChange := Self.OnChange;
      OnHint := Self.OnHint;
    end else inherited AssignTo(Dest);
end;

procedure TCustomAction.SetAutoCheck(Value: Boolean);
var
  I: Integer;
begin
  if Value <> FAutoCheck then
  begin
    for I := 0 to FClients.Count - 1 do
      if TBasicActionLink(FClients[I]) is TActionLink then
        TActionLink(FClients[I]).SetAutoCheck(Value);
    FAutoCheck := Value;
    Change;
  end;
end;

procedure TCustomAction.SetCaption(const Value: string);
var
  I: Integer;
  Link: TActionLink;
begin
  if Value <> FCaption then
  begin
    for I := 0 to FClients.Count - 1 do
    begin
      Link := TObject(FClients.List^[I]) as TActionLink;
      if Assigned(Link) then
        Link.SetCaption(Value);
    end;
    FCaption := Value;
    Change;
  end;
end;

procedure TCustomAction.SetChecked(Value: Boolean);
var
  I: Integer;
  Link: TActionLink;
  Action: TContainedAction;
begin
  if FChecking then exit;
  FChecking := True;
  try
    if Value <> FChecked then
    begin
      for I := 0 to FClients.Count - 1 do
      begin
        Link := TObject(FClients.List^[I]) as TActionLink;
        if Assigned(Link) then
          Link.SetChecked(Value);
      end;
      FChecked := Value;
      if (FGroupIndex > 0) and FChecked then
        for I := 0 to ActionList.ActionCount - 1 do
        begin
          Action := ActionList.Actions[I];
          if (Action <> Self) and
             (TObject(Action) is TCustomAction) and
             (TCustomAction(Action).FGroupIndex = FGroupIndex) then
            TCustomAction(Action).Checked := False;
        end;
      Change;
    end;
  finally
    FChecking := False;
  end;
end;

procedure TCustomAction.SetEnabled(Value: Boolean);
var
  I: Integer;
  Link: TActionLink;
begin
  if Value <> FEnabled then
  begin
    if Assigned(ActionList) then
      if ActionList.State = asSuspended then
      begin
        FEnabled := Value;
        exit;
      end
      else
        if (ActionList.State = asSuspendedEnabled) then
          Value := True;
    for I := 0 to FClients.Count - 1 do
    begin
      Link := TObject(FClients.List^[I]) as TActionLink;
      if Assigned(Link) then
        TActionLink(Link).SetEnabled(Value);
    end;
    FEnabled := Value;
    Change;
  end;
end;

procedure TCustomAction.SetGroupIndex(const Value: Integer);
var
  I: Integer;
  Link: TActionLink;
begin
  if Value <> FGroupIndex then
  begin
    FGroupIndex := Value;
    for I := 0 to FClients.Count - 1 do
    begin
      Link := TObject(FClients.List^[I]) as TActionLink;
      if Assigned(Link) then
        Link.SetGroupIndex(Value);
    end;
    Change;
  end;
end;

procedure TCustomAction.SetHelpType(Value: THelpType);
var
  I: Integer;
begin
  if Value <> FHelpType then
  begin
    for I := 0 to FClients.Count -1 do
     if TBasicActionLink(FCLients[I]) is TActionLink then
       TActionLink(FClients[I]).SetHelpType(Value);
    FHelpType := Value;
    Change;
  end;
end;

procedure TCustomAction.SetHelpKeyword(const Value: string);
var
  I: Integer;
begin
  if Value <> FHelpKeyword then
  begin
    for I := 0 to FClients.Count -1 do
     if TBasicActionLink(FCLients[I]) is TActionLink then
       TActionLink(FClients[I]).SetHelpKeyword(Value);
    FHelpKeyword := Value;
    Change;
  end;
end;

procedure TCustomAction.SetHelpContext(Value: THelpContext);
var
  I: Integer;
  Link: TActionLink;
begin
  if Value <> FHelpContext then
  begin
    for I := 0 to FClients.Count - 1 do
    begin
      Link := TObject(FClients.List^[I]) as TActionLink;
      if Assigned(Link) then
        Link.SetHelpContext(Value);
    end;
    FHelpContext := Value;
    Change;
  end;
end;

procedure TCustomAction.SetHint(const Value: string);
var
  I: Integer;
  Link: TActionLink;
begin
  if Value <> FHint then
  begin
    for I := 0 to FClients.Count - 1 do
    begin
      Link := TObject(FClients.List^[I]) as TActionLink;
      if Assigned(Link) then
        Link.SetHint(Value);
    end;
    FHint := Value;
    Change;
  end;
end;

procedure TCustomAction.SetImageIndex(Value: TImageIndex);
var
  I: Integer;
  Link: TActionLink;
begin
  if Value <> FImageIndex then
  begin
    for I := 0 to FClients.Count - 1 do
    begin
      Link := TObject(FClients.List^[I]) as TActionLink;
      if Assigned(Link) then
        Link.SetImageIndex(Value);
    end;
    FImageIndex := Value;
    Change;
  end;
end;

procedure TCustomAction.SetShortCut(Value: TShortCut);
var
  I: Integer;
  Link: TActionLink;
begin
  if Value <> FShortCut then
  begin
    for I := 0 to FClients.Count - 1 do
    begin
      Link := TObject(FClients.List^[I]) as TActionLink;
      if Assigned(Link) then
        Link.SetShortCut(Value);
    end;
    FShortCut := Value;
    Change;
  end;
end;

procedure TCustomAction.SetVisible(Value: Boolean);
var
  I: Integer;
  Link: TActionLink;
begin
  if Value <> FVisible then
  begin
    for I := 0 to FClients.Count - 1 do
    begin
      Link := TObject(FClients.List^[I]) as TActionLink;
      if Assigned(Link) then
        TActionLink(FClients[I]).SetVisible(Value);
    end;
    FVisible := Value;
    Change;
  end;
end;

procedure TCustomAction.SetName(const Value: TComponentName);
var
  ChangeText: Boolean;
begin
  ChangeText := (Name = Caption) and ((Owner = nil) or
    not (csLoading in Owner.ComponentState));
  inherited SetName(Value);
  { Don't update caption to name if we've got clients connected. }
  if ChangeText and (FClients.Count = 0) then Caption := Value;
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
  if FSecondaryShortCuts = nil then
    FSecondaryShortCuts := TShortCutList.Create;
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
