constructor TViewColumns.Create(Aowner : TCustomListView);
Begin
   inherited Create;
   FItems := TList.Create;
   ListView := AOwner;
   FUpdating := False;
end;

Function TViewColumns.Add(const S : String): Integer;
var
  VC : TViewColumn;
Begin
   VC := TViewColumn.Create;
   VC.Caption := S;
   VC.OnChange := @ColumnChanged;
   Result := FItems.Add(VC);
   if Assigned(OnChange) then
   OnChange(self);
end;

Procedure TViewColumns.Delete(Index : Integer);
Begin
   if Index > FItems.Count-1 then exit;
   TViewColumn(FItems.Items[index]).Free;
   FItems.Delete(Index);
   if Assigned(OnChange) then
   OnChange(self);
end;


Destructor TViewColumns.Destroy;
var i: integer;
begin
  for i:=0 to FItems.Count-1 do
    TViewColumn(FItems.Items[i]).Free;
  FItems.Free;
  Inherited;
end;

function TViewColumns.GetItem(Index : Integer): TViewColumn;
begin
    Result := nil;
    if Index > FItems.Count-1 then exit;
    Result := TViewColumn(FItems.Items[Index]);
end;

Procedure TViewColumns.ColumnChanged(Sender : TObject);
begin
  if not FUpdating then
  if Assigned(OnChange) then
     OnChange(self);
end;

function TViewColumns.GetCount: Integer;
begin
  result := FItems.Count;
end;

Procedure TViewColumns.Clear;
var
  I : Integer;
begin
  Updating := True;
  For I := 0 to FItems.Count-1 do
     TViewColumns(FItems.Items[i]).Free;
  FItems.Clear;
  Updating := False;
end;

procedure TViewColumns.SetUpdating(const AValue: Boolean);
begin
   if FUpdating and Not AValue then
      Begin
        FUpdating := AValue;  //false
        ColumnChanged(self);
      end
      else
      FUpdating := AValue;
end;



