{ TStatusPanels }

constructor TStatusPanels.Create(StatusBar: TStatusBar);
begin
  inherited Create(TStatusPanel);
  FStatusBar := StatusBar;
end;

function TStatusPanels.Add: TStatusPanel;
begin
  Result := TStatusPanel(inherited Add);
end;

function TStatusPanels.GetItem(Index: Integer): TStatusPanel;
begin
  Result := TStatusPanel(inherited GetItem(Index));
end;

function TStatusPanels.GetOwner: TPersistent;
begin
  Result := FStatusBar;
end;

procedure TStatusPanels.SetItem(Index: Integer; Value: TStatusPanel);
begin
  inherited SetItem(Index, Value);
end;

procedure TStatusPanels.Update(Item: TCollectionItem);
begin
end;

