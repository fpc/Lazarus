{ TViewColumn }

constructor TViewColumn.Create;
begin
   inherited;
   FAlignment :=caLeft;
   FCaption := '';
   FWidth := 50;
   FVisible := True;
   FMinWidth := 0;
   FMaxWidth := 0;
   FAutoSize := False;
end;

destructor TViewColumn.Destroy;
begin
  Inherited;
end;

procedure TViewColumn.SetAlignment(const AValue: TColumnAlignment);
begin
 if FAlignment = AValue then Exit;
 
 FAlignment := AValue;
 if Assigned(OnChange) then
   OnChange(self);
end;

procedure TViewColumn.SetCaption(const AValue: String);
begin
  if AValue = FCaption then Exit;
  
  FCaption := AValue;
  if Assigned(OnChange) then
     OnChange(self);
end;

procedure TViewColumn.SetWidth(const AValue: Integer);
begin
  if FWidth = AValue then Exit;

  FWidth := AValue;
  if Assigned(OnChange) then
     OnChange(self);
end;

procedure TViewColumn.SetMaxWidth(const AValue: Integer);
begin
  if FMaxWidth = AValue then Exit;

  FMaxWidth := AValue;
  if Assigned(OnChange) then
     OnChange(self);

end;

procedure TViewColumn.SetMinWidth(const AValue: Integer);
begin
  if FMinWidth = AValue then Exit;

  FMinWidth := AValue;
  if Assigned(OnChange) then
     OnChange(self);

end;

procedure TViewColumn.SetAutoSize(const AValue: Boolean);
begin
  if FAutoSize = AValue then Exit;

  FAutoSize := AValue;
  if Assigned(OnChange) then
     OnChange(self);
end;

procedure TViewColumn.SetVisible(const AValue: Boolean);
begin
  if FVisible = AValue then Exit;

  FVisible := AValue;
  if Assigned(OnChange) then
     OnChange(self);
end;

