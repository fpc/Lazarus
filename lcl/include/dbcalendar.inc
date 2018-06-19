{%MainUnit ../dbctrls.pp}

{******************************************************************************
                                     TDBCalendar
                    data aware Calendar, base found in dbctrls.pp
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

// included by dbctrls.pp

{ Private Methods }

//update the caption on next record etc...
procedure TDBCalendar.DataChange(Sender: TObject);
begin
  if FDatalink.Active and (FDataLink.Field <> nil) then
    UpdateDate(FDatalink.Field.Text)
  else
    UpdateDate('');
end;

procedure TDBCalendar.UpdateData(Sender: TObject);
begin
  FDataLink.Field.Text := Text;
end;

function TDBCalendar.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TDBCalendar.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TDBCalendar.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TDBCalendar.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TDBCalendar.SetReadOnly(Value: Boolean);
begin
  inherited;
  FDataLink.ReadOnly := Value;
end;

procedure TDBCalendar.SetDate(const AValue: String);
begin
  inherited Date := AValue;
  DataChange(Self);
end;

procedure TDBCalendar.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TDBCalendar.SetDataSource(Value: TDataSource);
begin
  ChangeDataSource(Self,FDataLink,Value);
end;

procedure TDBCalendar.UpdateDate(const AValue: string);
begin
  if AValue='' then
    // TODO: do dbcalendar needs some visual feedback
    //       that current date is invalid?
  else
    inherited date := AValue;
end;

procedure TDBCalendar.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink);
end;

procedure TDBCalendar.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) then begin
    if (FDataLink<>nil) and (AComponent=DataSource) then
      DataSource:=nil;
  end;
end;

procedure TDBCalendar.EditingDone;
begin
  FDataLink.UpdateRecord;
  inherited EditingDone;
end;

function TDBCalendar.ExecuteAction(AAction: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(AAction) or
            (FDataLink <> nil) and FDataLink.ExecuteAction(AAction);
end;

function TDBCalendar.UpdateAction(AAction: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(AAction) or
            (FDataLink <> nil) and FDataLink.UpdateAction(AAction);
end;

{ Public Methods }
constructor TDBCalendar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := @DataChange;
  FDataLink.OnUpdateData := @UpdateData;
end;

destructor TDBCalendar.Destroy;
begin
  FDataLink.Destroy;
  inherited Destroy;
end;

