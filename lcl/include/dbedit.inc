{%MainUnit ../dbctrls.pas}

{******************************************************************************
                                     TDBEdit
                    data aware Edit, base found in dbctrls.pp
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
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
    if Focused and FDataLink.CanModify then
      //display the real text since we can modify it
      Text := FDataLink.Field.DisplayText//this is wrong, but Text seems Broken
    else
      //otherwise display the pretified/formated text since we can't
      EditText := FDataLink.Field.DisplayText;
  end
  else begin
    EditMask := '';
    Text := '';
  end;
end;

procedure TDBEdit.ActiveChange(Sender: TObject);

begin
  if FDatalink.Active then datachange(sender)
  else
    begin
    text := '';
    FDataLink.reset;
    end;
end;


procedure TDBEdit.EditingChange(Sender: TObject);
begin
  //ok to hide the 'real' readonly state we must override it
  //and so here when the data changes we need to set the
  //'real' value to match the state of the FieldLink
  //i.e. always ReadOnly unless its editing
  inherited ReadOnly := not FDataLink.Editing;
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
  FDataLink.Field.AsString := Text;// I shouldn't have to do this, but text seems broken
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
  //we want to override the readonly state so we can
  //reflect the state of the Datalink/Field
  Result := FDataLink.ReadOnly;
end;

procedure TDBEdit.SetReadOnly(Value: Boolean);
begin
  //we want to override the readonly state so we can
  //reflect the state of the Datalink/Field, so changing
  //readonly changes the DataLink to ReadOnly, and when Editing
  //changes the 'real' Readonly state will be updated to match
  //according to the editing flag, which will always be false if
  //this is true anyway. so I think all should be happy...
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

{ Protected Methods}
procedure TDBEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key,Shift);
  if Key=VK_ESCAPE then begin
    //cancel out of editing by reset on esc
    FDataLink.Reset;
    SelectAll;
    Key := VK_UNKNOWN;
  end else if (Key<>VK_UNKNOWN) then begin
    //make sure we call edit to ensure the datset is in edit,
    //this is for where the datasource is in autoedit, so we aren't
    //read only even though the dataset isn't realy in edit
    FDataLink.Edit;
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

procedure TDBEdit.SetFocus;
begin
  if not Focused then begin
    inherited SetFocus;
    FDataLink.Reset;//force call to data changed to update text(display vs actual)
  end;
end;

procedure TDBEdit.EditingDone;
begin
  inherited EditingDone;
  if FDataLink.CanModify then
    FDataLink.UpdateRecord;
end;

{ Public Methods }
constructor TDBEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := @DataChange;
  FDataLink.OnEditingChange := @EditingChange;
  FDataLink.OnUpdateData := @UpdateData;
  FDataLInk.OnActiveChange := @ActiveChange;

  inherited ReadOnly := True;//start read only
end;

destructor TDBEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

{ =============================================================================

  $Log$
  Revision 1.11  2005/01/06 13:22:24  micha
  fix access violation with dbedit (from loesje)

  Revision 1.10  2004/09/01 11:12:04  mattias
  replaced KeyPress dependencies with KeyDown

  Revision 1.9  2004/08/30 16:37:58  mattias
  added OnUTF8KeyPresss

  Revision 1.8  2004/08/22 22:57:35  mattias
  added OnActiveChange  from Joost

  Revision 1.7  2004/08/13 16:40:47  mazen
  + TCharater type used to allow UTF8 keyboard with gtk2

  Revision 1.6  2004/08/08 20:51:15  mattias
  replaced TDBEdit.WMKillFocus by EditingDone, Change Class basically working

  Revision 1.5  2004/08/05 19:33:48  vincents
  allow backspace as a valid input

  Revision 1.4  2004/04/10 17:58:57  mattias
  implemented mainunit hints for include files

  Revision 1.3  2003/09/22 15:03:19  ajgenius
  partly fixed streaming of DBCalendar, and opRemove notification of DBText DBEdit DBCalendar

  Revision 1.2  2003/09/18 14:36:17  ajgenius
  added TFieldDataLink.FocusControl/OnFocusRequest

  Revision 1.1  2003/09/14 18:40:55  ajgenius
  add initial TFieldDataLink, TDBEdit and TDBText


}

