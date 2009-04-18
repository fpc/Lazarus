{%MainUnit ../dbctrls.pas}

{******************************************************************************
                                     TDBCalendar
                    data aware Calendar, base found in dbctrls.pp
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
  FDataLink.OnUpdateData := @UpdateData;
end;

destructor TDBCalendar.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

