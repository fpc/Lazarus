{%MainUnit ../dbctrls.pas}

{******************************************************************************
                                     TDBCalendar
                    data aware Calendar, base found in dbctrls.pp
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

procedure TDBCalendar.EditingChange(Sender: TObject);
begin
  //ok to hide the 'real' readonly state we must override it
  //and so here when the data changes we need to set the
  //'real' value to match the state of the FieldLink
  //i.e. always ReadOnly unless its editing
  inherited ReadOnly := not FDataLink.Editing;
end;

procedure TDBCalendar.UpdateData(Sender: TObject);
begin
  FDataLink.Field.Text := Text;
end;

procedure TDBCalendar.FocusRequest(Sender: TObject);
begin
  //the FieldLink has requested the control
  //recieve focus for some reason..
  //perhaps an error occured?
  SetFocus;
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
  //we want to override the readonly state so we can
  //reflect the state of the Datalink/Field
  Result := FDataLink.ReadOnly;
end;

procedure TDBCalendar.SetReadOnly(Value: Boolean);
begin
  //we want to override the readonly state so we can
  //reflect the state of the Datalink/Field, so changing
  //readonly changes the DataLink to ReadOnly, and when Editing
  //changes the 'real' Readonly state will be updated to match
  //according to the editing flag, which will always be false if
  //this is true anyway. so I think all should be happy...
  FDataLink.ReadOnly := Value;
end;

procedure TDBCalendar.SetDate(const AValue: String);
begin
  inherited Date := AValue;
  DataChange(Self);
end;

procedure TDBCalendar.SetDataField(Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TDBCalendar.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

procedure TDBCalendar.UpdateDate(const AValue: string);
begin
  if AValue='' then
    // TODO: do dbcalendar needs some visual feedback
    //       that current date is invalid?
  else
    inherited date := AValue;
end;

{ Protected Methods}
procedure TDBCalendar.Loaded;
begin
  inherited Loaded;
  //need to make sure the state is updated on first load
  if (csDesigning in ComponentState) then
    DataChange(Self);
end;

procedure TDBCalendar.Notification(AComponent: TComponent;
  Operation: TOperation);
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

{ Public Methods }
constructor TDBCalendar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := @DataChange;
  FDataLink.OnEditingChange := @EditingChange;
  FDataLink.OnUpdateData := @UpdateData;

  inherited ReadOnly := True;//start read only
end;

destructor TDBCalendar.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

