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

// included by dbctrls.pp

{ Private Methods }

//update the Selected item on next record etc...
procedure TCustomDBListBox.DataChange(Sender: TObject);
begin
  //
end;

procedure TCustomDBListBox.EditingChange(Sender: TObject);
begin
  //
end;

procedure TCustomDBListBox.UpdateData(Sender: TObject);
begin
  //
end;

procedure TCustomDBListBox.FocusRequest(Sender: TObject);
begin
  //the FieldLink has requested the control
  //recieve focus for some reason..
  //perhaps an error occured?
  SetFocus;
end;

function TCustomDBListBox.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TCustomDBListBox.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TCustomDBListBox.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TCustomDBListBox.SetItems(Values : TStrings);
begin
  Items.Assign(Values);
  DataChange(Self);
end;

//we want to override the readonly state so we can
//reflect the state of the Datalink/Field
function TCustomDBListBox.GetReadOnly: Boolean;
begin
  //we want to override the readonly state so we can
  //reflect the state of the Datalink/Field
  Result := FDataLink.ReadOnly;
end;

procedure TCustomDBListBox.SetReadOnly(Value: Boolean);
begin
  //we want to override the readonly state so we can
  //reflect the state of the Datalink/Field, so changing
  //readonly changes the DataLink to ReadOnly, and when Editing
  //changes the 'real' Readonly state will be updated to match
  //according to the editing flag, which will always be false if
  //this is true anyway. so I think all should be happy...
  FDataLink.ReadOnly := Value;
end;

procedure TCustomDBListBox.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TCustomDBListBox.SetDataSource(Value: TDataSource);
begin
  ChangeDataSource(Self,FDataLink,Value);
end;

procedure TCustomDBListBox.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink);
end;

{ Protected Methods}
procedure TCustomDBListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key,Shift);
  if Key=VK_ESCAPE then begin
    //cancel out of editing by reset on esc
    FDataLink.Reset;
    Key := VK_UNKNOWN;
  end else if (Key<>VK_UNKNOWN) then begin
    //make sure we call edit to ensure the datset is in edit,
    //this is for where the datasource is in autoedit, so we aren't
    //read only even though the dataset isn't really in edit
    //if this validates false make sure the entry doesn't change
    //since listbox doesn't have its own read only yet we gots to fake it
    //here
    if FDataLink.Edit then
       exit;
    Key := VK_UNKNOWN;
  end;
end;

procedure TCustomDBListBox.Loaded;
begin
  inherited Loaded;
  //need to make sure the state is updated on first load
  if (csDesigning in ComponentState) then
    DataChange(Self);
end;

procedure TCustomDBListBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  // if the datasource is being removed then we need to make sure
  // we are updated or we can get AV/Seg's *cough* as I foolishly
  // discovered firsthand....
  if (Operation=opRemove) then begin
    if (FDataLink<>nil) and (AComponent=DataSource) then
      DataSource:=nil;
  end;
end;

procedure TCustomDBListBox.Click;
begin
  //make sure we are in modify mode if can edit
  //so if a user changed the selection it can be
  //updated, and if not canel out ala ReadOnly
  if not FDataLink.Edit then
  begin
    // restore value
    DataChange(self);
    Exit;
  end;
  inherited Click;
  FDataLink.Modified;
end;

procedure TCustomDBListBox.EditingDone;
begin
  FDataLink.UpdateRecord;
  inherited EditingDone;
end;

{ Public Methods }
constructor TCustomDBListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := @DataChange;
  FDataLink.OnEditingChange := @EditingChange;
  FDataLink.OnUpdateData := @UpdateData;
end;

destructor TCustomDBListBox.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

