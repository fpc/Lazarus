{%MainUnit ../dbextctrls.pp}

{******************************************************************************
 	This file is part of the Lazarus Component Library (LCL)

 	See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************}
// included by dbextctrls.pp

{ Private Methods }

procedure TDBDateEdit.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
    Self.Date := FDataLink.Field.AsDateTime
  else
    Text := '';
end;

procedure TDBDateEdit.UpdateData(Sender: TObject);
begin
  if Text = '' then
    FDataLink.Field.Clear
  else
    FDataLink.Field.AsDateTime := Self.Date;
end;

function TDBDateEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TDBDateEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TDBDateEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TDBDateEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TDBDateEdit.SetDataSource(Value: TDataSource);
begin
  ChangeDataSource(Self, FDataLink, Value);
end;

procedure TDBDateEdit.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink);
end;

{ Protected Methods}
procedure TDBDateEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation=opRemove then
    if assigned(FDataLink) and (AComponent=DataSource) then
      DataSource:=nil;
end;

function TDBDateEdit.EditCanModify: Boolean;
begin
  Result := FDataLink.CanModify;
end;

procedure TDBDateEdit.EditEnter;
begin
  inherited;
  if not FDataLink.Editing then
    FDataLink.Reset;
end;

procedure TDBDateEdit.EditKeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_ESCAPE:
      //cancel out of editing by reset on Esc
      if FDataLink.Editing then begin
        FDataLink.Reset;
        Key := VK_UNKNOWN;
      end;
    VK_DELETE, VK_BACK:
      if not FDataLink.Edit then
        Key := VK_UNKNOWN;
  end;
end;

procedure TDBDateEdit.EditKeyPress(var Key: Char);
begin
  inherited;
  case Key of
    #32..#255: //standard keys
      if not assigned(FDataLink.Field) or not Field.IsValidChar(Key) or not FDataLink.Edit then
        Key:=#0;
  end;
end;

procedure TDBDateEdit.ButtonClick;
begin
  if not Focused then
    SetFocus;
  if FDataLink.CanModify then
    FDataLink.Edit;
  inherited;
end;

procedure TDBDateEdit.EditChange;
begin
  if FDataLink.Editing then
    FDataLink.Modified;
  inherited;
end;

procedure TDBDateEdit.EditExit;
begin
  inherited;
  if FDataLink.Editing then
    FDataLink.UpdateRecord
  else
    FDataLink.Reset;
end;

procedure TDBDateEdit.Reset;
begin
  FDataLink.Reset;
  inherited;
end;

{ Public Methods }
constructor TDBDateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := @DataChange;
  FDataLink.OnUpdateData := @UpdateData;
end;

destructor TDBDateEdit.Destroy;
begin
  FDataLink.Destroy;
  inherited Destroy;
end;

function TDBDateEdit.ExecuteAction(AAction: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(AAction) or
            assigned(FDataLink) and FDataLink.ExecuteAction(AAction);
end;

function TDBDateEdit.UpdateAction(AAction: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(AAction) or
            assigned(FDataLink) and FDataLink.UpdateAction(AAction);
end;
