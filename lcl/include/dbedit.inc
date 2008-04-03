{%MainUnit ../dbctrls.pas}

{******************************************************************************
                                     TDBEdit
                    data aware Edit, base found in dbctrls.pp
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
procedure TDBEdit.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then begin
    //use the right EditMask if any
    //EditMask := FDataLink.Field.EditMask; doesn't exist yet

    //if we are focused its possible to edit,
    //if the field is currently modifiable
    if Focused and FDataLink.CanModify then begin
      //display the real text since we can modify it
      //Text := FDataLink.Field.DisplayText//this is wrong, but Text seems Broken
      Text := FDatalink.Field.Text;
      SelectAll;
    end else
      //otherwise display the pretified/formated text since we can't
      EditText := FDataLink.Field.DisplayText;
  end
  else begin
    //todo: uncomment this when TField implements EditMask
    //EditMask := ''
    Text := '';
  end;
end;

procedure TDBEdit.ActiveChange(Sender: TObject);
begin
  if FDatalink.Active then 
    DataChange(Sender)
  else begin
    Text := '';
    FDataLink.Reset;
  end;
end;

procedure TDBEdit.LayoutChange(Sender: TObject);
begin
  DataChange(Sender);
end;

procedure TDBEdit.UpdateData(Sender: TObject);
begin
  //the field is being updated, probably for post
  //so we are getting called to make sure its
  //up-to-date and matches any modifications
  //since its possible to have a mask for say
  //date or currency we need to make sure the
  //text is valid before we update this is in
  //case for instance they call table.post via
  //a keyboard shortcut while still focused, before
  //the changes have been validated
  ValidateEdit;
  FDataLink.Field.Text := Text;
  //FDataLink.Field.AsString := Text;// I shouldn't have to do this, but text seems broken
end;

procedure TDBEdit.FocusRequest(Sender: TObject);
begin
  //the FieldLink has requested the control
  //recieve focus for some reason..
  //perhaps an error occured?
  SetFocus;
end;

function TDBEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TDBEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TDBEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TDBEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TDBEdit.SetReadOnly(Value: Boolean);
begin
  inherited;
  FDataLink.ReadOnly := Value;
end;

procedure TDBEdit.SetDataField(Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TDBEdit.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

function TDBEdit.IsReadOnly: boolean;
begin
  result := true;
  if FDatalink.Active and not Self.ReadOnly then
    result := (Field=nil) or Field.ReadOnly;
end;

procedure TDBEdit.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink);
end;

{ Protected Methods}
procedure TDBEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key,Shift);
  if Key=VK_ESCAPE then begin
    //cancel out of editing by reset on esc
    FDataLink.Reset;
    SelectAll;
    Key := VK_UNKNOWN;
  end else
  if Key=VK_DELETE then begin
    if not IsReadOnly then
      FDatalink.Edit;
  end;
end;

procedure TDBEdit.KeyPress(var Key: char);
  function CanAcceptKey: boolean;
  var
    aField: TField;
  begin
    result := False;
    if FDataLink.Active then begin
      aField := Field;
      if aField<>nil then begin
        Result := aField.IsValidChar(Key) and not aField.Calculated and
          (aField.DataType<>ftAutoInc);
      end;
    end;
  end;
begin
  inherited KeyPress(Key);
  case key of
    #8: // special keys
      if not IsReadOnly then
        FDatalink.Edit
      else
        Key:=#0;
        
    #32..#255: //standard keys
      if CanAcceptKey and not IsReadOnly then
        FDatalink.Edit
      else
        Key:=#0;
  end;
end;

procedure TDBEdit.Loaded;
begin
  inherited Loaded;
  //need to make sure the state is updated on first load
  if (csDesigning in ComponentState) then
    DataChange(Self);
end;

procedure TDBEdit.Notification(AComponent: TComponent;
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

function TDBEdit.EditCanModify: Boolean;
begin
  //should follow the FieldLink for this one
  Result := FDataLink.Edit;
end;

function TDBEdit.GetEditText: string;
begin
  if not (csDesigning in ComponentState) and not FDatalink.Active then begin
    Result := '';
    exit;
  end;
  Result:=inherited GetEditText;
end;

procedure TDBEdit.Change;
begin
  //need to override this to make sure the datalink gets notified
  //its been modified, then when post etc, it will call
  //updatedata to update the field data with current value
  FDataLink.Modified;

  inherited Change;
end;

procedure TDBEdit.Reset;
begin
  //need to override this to make sure the datalink gets reset
  //if the changes get canceled
  FDataLink.reset;

  inherited Reset;
end;

procedure TDBEdit.EditingDone;
begin
  inherited EditingDone;
  if FDataLink.CanModify and FDatalink.Editing then begin
    FUpdatingRecord := True;
    FDataLink.UpdateRecord;
    FUpdatingRecord := False;
  end;
end;

procedure TDBEdit.WMSetFocus(var Message: TLMSetFocus);
begin
  inherited WMSetFocus(Message);
  FDatalink.Reset;
end;

procedure TDBEdit.WMKillFocus(var Message: TLMKillFocus);
begin
  if not FUpdatingRecord then begin
    inherited WMKillFocus(Message);
    FDatalink.Reset;
  end else
    FUpdatingRecord := False;
end;

{ Public Methods }
constructor TDBEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := @DataChange;
  FDataLink.OnUpdateData := @UpdateData;
  FDataLink.OnActiveChange := @ActiveChange;
  FDataLink.OnLayoutChange := @LayoutChange;
end;

destructor TDBEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

