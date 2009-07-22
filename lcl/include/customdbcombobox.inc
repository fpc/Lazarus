{%MainUnit ../dbctrls.pas}

{******************************************************************************
                                     TDBListBox
                    data aware ListBox, base found in dbctrls.pp
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

// included by dbctrls.pas

{TCustomDBComboBox}

function TCustomDBComboBox.GetDataField: string;
begin
  Result:=FDataLink.FieldName;
end;

function TCustomDBComboBox.GetDataSource: TDataSource;
begin
  Result:=FDataLink.DataSource;
end;

function TCustomDBComboBox.GetField: TField;
begin
  Result:=FDataLink.Field;
end;

procedure TCustomDBComboBox.Change;
begin
  //need to override this to make sure the datalink gets notified
  //its been modified, then when post etc, it will call
  //updatedata to update the field data with current value
  if FDatalink.Active then
    if FDatalink.Edit then
    begin
      FDataLink.Modified;
      inherited Change;
    end else
      UpdateText
  else
    inherited Change;
end;


function TCustomDBComboBox.GetReadOnly: Boolean;
begin
  Result:=FDataLink.ReadOnly;
end;

procedure TCustomDBComboBox.SetDataField(const AValue: string);
begin
  FDataLink.FieldName:=AValue;
end;

procedure TCustomDBComboBox.SetDataSource(const AValue: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    ChangeDataSource(Self,FDataLink,AValue);
end;

procedure TCustomDBComboBox.SetReadOnly(const AValue: Boolean);
begin
  FDataLink.ReadOnly:=AValue;
end;

procedure TCustomDBComboBox.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink);
end;

procedure TCustomDBComboBox.DataChange(Sender: TObject);
begin
  if not (Style=csSimple) and DroppedDown then
    Exit;
  UpdateText;
end;

procedure TCustomDBComboBox.EditingChange(Sender: TObject);
begin
  // ToDo
end;

procedure TCustomDBComboBox.Notification(AComponent: TComponent; Operation: TOperation
  );
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) then begin
    if (FDataLink<>nil) and (AComponent=DataSource) then
      DataSource:=nil;
  end;
end;

procedure TCustomDBComboBox.UpdateData(Sender: TObject);
begin
  //
end;

procedure TCustomDBComboBox.FocusRequest(Sender: TObject);
begin
  //the FieldLink has requested the control
  //receive focus for some reason..
  //perhaps an error occured?
  SetFocus;
end;

procedure TCustomDBComboBox.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then
    DataChange(Self);
end;

procedure TCustomDBComboBox.EditingDone;
begin
  FDataLink.UpdateRecord;
  inherited EditingDone;
end;

procedure TCustomDBComboBox.UpdateText;
begin
  if csDesigning in ComponentState then
    Text := Name
  else
    Text := '';
end;

procedure TCustomDBComboBox.ActiveChange(Sender: TObject);

begin
  if FDatalink.Active then DataChange(Sender)
  else
    begin
    Text := '';
    FDataLink.Reset;
    end;
end;

constructor TCustomDBComboBox.Create(TheOwner: TComponent);
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

destructor TCustomDBComboBox.Destroy;
begin
  FDataLink.Free;
  FDataLink:=nil;
  inherited Destroy;
end;

