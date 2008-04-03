{%MainUnit ../dbctrls.pas}
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

{ TDBComboBox }

function TDBComboBox.GetDataField: string;
begin
  Result:=FDataLink.FieldName;
end;

function TDBComboBox.GetDataSource: TDataSource;
begin
  Result:=FDataLink.DataSource;
end;

function TDBComboBox.GetField: TField;
begin
  Result:=FDataLink.Field;
end;

procedure TDBComboBox.Change;
begin
  //need to override this to make sure the datalink gets notified
  //its been modified, then when post etc, it will call
  //updatedata to update the field data with current value
  if FDatalink.Edit then
  begin
    FDataLink.Modified;
    inherited change;
  end else
    UpdateText;
end;


function TDBComboBox.GetReadOnly: Boolean;
begin
  Result:=FDataLink.ReadOnly;
end;

procedure TDBComboBox.SetDataField(const AValue: string);
begin
  FDataLink.FieldName:=AValue;
end;

procedure TDBComboBox.SetDataSource(const AValue: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource:=AValue;
  if AValue <> nil then
    AValue.FreeNotification(Self);
end;

procedure TDBComboBox.SetReadOnly(const AValue: Boolean);
begin
  FDataLink.ReadOnly:=AValue;
end;

procedure TDBComboBox.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink);
end;


procedure TDBComboBox.DataChange(Sender: TObject);
begin
  if not (Style=csSimple) and DroppedDown then
    Exit;
  UpdateText;
end;

procedure TDBComboBox.EditingChange(Sender: TObject);
begin
  // ToDo
end;

procedure TDBComboBox.Notification(AComponent: TComponent; Operation: TOperation
  );
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) then begin
    if (FDataLink<>nil) and (AComponent=DataSource) then
      DataSource:=nil;
  end;
end;

procedure TDBComboBox.UpdateData(Sender: TObject);

begin
  FDataLink.Field.Text := text;
  FDataLink.Field.AsString := text;
end;

procedure TDBComboBox.FocusRequest(Sender: TObject);
begin
  //the FieldLink has requested the control
  //receive focus for some reason..
  //perhaps an error occured?
  SetFocus;
end;

procedure TDBComboBox.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then
    DataChange(Self);
end;

procedure TDBComboBox.EditingDone;
begin
  FDataLink.UpdateRecord;
  inherited EditingDone;
end;

procedure TDBComboBox.UpdateText;
begin
  if FDataLink.Field <> nil then
    // ToDo: use Field.Text
      Text := FDataLink.Field.DisplayText
  else
    if csDesigning in ComponentState then
      Text := Name
    else
      Text := '';
end;

procedure TDBComboBox.ActiveChange(Sender: TObject);

begin
  if FDatalink.Active then datachange(sender)
  else
    begin
    text := '';
    FDataLink.reset;
    end;
end;

constructor TDBComboBox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle:=ControlStyle+[csReplicatable];
  FDataLink:=TFieldDataLink.Create;
  FDataLink.Control:=Self;
  FDataLink.OnDataChange:=@DataChange;
  FDataLink.OnUpdateData:=@UpdateData;
  FDataLInk.OnActiveChange := @ActiveChange;
  FDataLink.OnEditingChange:=@EditingChange;
end;

destructor TDBComboBox.Destroy;
begin
  FDataLink.Free;
  FDataLink:=nil;
  inherited Destroy;
end;

// included by dbctrls.pas
