{ TViewColumn }

constructor TViewColumn.Create;
begin
   inherited;
   FAlignment :=caLEft;
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

