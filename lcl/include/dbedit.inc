{%MainUnit ../dbctrls.pp}

{******************************************************************************
                                     TDBEdit
                    data aware Edit, base found in dbctrls.pp
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

// included by dbctrls.pp
{.$define UpdDisplay}

{ Private Methods }

//update the caption on next record etc...
procedure TDBEdit.DataChange(Sender: TObject);
var
  DataLinkField: TField;
begin
  DataLinkField := FDataLink.Field;
  if DataLinkField <> nil then begin
    //use Field EditMask by default
    if not FCustomEditMask then
      EditMask := DataLinkField.EditMask;
    Alignment := DataLinkField.Alignment;

    //if we are focused its possible to edit,
    //if the field is currently modifiable
    if FFocusedDisplay and FDatalink.CanModify then begin
      //display the real text since we can modify it
      RestoreMask(DatalinkField.Text);
    end else
      //otherwise display the pretified/formated text since we can't
      DisableMask(DataLinkField.DisplayText);
    if (DataLinkField.DataType in [ftString, ftFixedChar, ftWidestring, ftFixedWideChar])
      and (MaxLength = 0) then
      MaxLength := DatalinkField.Size;
  end
  else begin
    if not FCustomEditMask then
      EditMask := '';
    Text := '';
    MaxLength := 0;
  end;
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
  Modified := False;
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

procedure TDBEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TDBEdit.SetDataSource(Value: TDataSource);
begin
  ChangeDataSource(Self,FDataLink,Value);
end;

procedure TDBEdit.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink);
end;

{ Protected Methods}
procedure TDBEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key,Shift);
  case key of
    VK_ESCAPE:
      begin
        //cancel out of editing by reset on esc, but only when in editing mode
        if FDataLink.Editing then begin
          FDataLink.Reset;
          SelectAll;
          Key := VK_UNKNOWN;
        end;
      end;
    VK_DELETE, VK_BACK:
      begin
        if not FieldIsEditable(FDatalink.Field) or not FDataLink.Edit then
          Key := VK_UNKNOWN;
      end;
    VK_RETURN:
      if FDatalink.Editing then // soner: take changes, without this you must
        FDatalink.UpdateRecord; // change focus to other dbctrl
  end;
end;

procedure TDBEdit.UTF8KeyPress(var UTF8Key: TUTF8Char);
var
  CharKey: Char;
begin
  inherited UTF8KeyPress(UTF8Key);
  //If the pressed key is unicode then map the char to #255
  //Necessary to keep the TField.IsValidChar check
  if Length(UTF8Key) = 1 then
    CharKey := UTF8Key[1]
  else
    CharKey := #255;

  //handle standard keys
  if CharKey in [#32..#255] then
  begin
    if not FieldCanAcceptKey(FDataLink.Field, CharKey) or not FDatalink.Edit then
      UTF8Key := '';
  end;
end;

procedure TDBEdit.Notification(AComponent: TComponent; Operation: TOperation);
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
  Result := FDataLink.CanModify;
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

procedure TDBEdit.WMSetFocus(var Message: TLMSetFocus);
var
  EditOnFocus: Boolean;
begin
  // update text before inherited so DoEnter has the new text
  if not FFocusedDisplay then
  begin
    FFocusedDisplay := True;
    // some widgetsets do not notify clipboard actions properly. Put at edit state at entry
    EditOnFocus := WidgetSet.GetLCLCapability(lcReceivesLMClearCutCopyPasteReliably) = LCL_CAPABILITY_NO;
    if EditOnFocus then
    begin
      if FDataLink.Edit then
        RestoreMask(FDataLink.Field.Text);
    end
    else
      FDataLink.Reset;
  end;
  inherited WMSetFocus(Message);
end;

procedure TDBEdit.WMKillFocus(var Message: TLMKillFocus);
begin
  inherited WMKillFocus(Message);
  FFocusedDisplay := False;
  if csDestroying in ComponentState then Exit;
  if FDatalink.Editing then
  begin
    FDatalink.UpdateRecord;
    //check for Focused before disabling the mask since SetFocus can be called
    //inside events propagated by WMKillFocus or UpdateRecord
    if not Focused then
    begin
      DisableMask(FDataLink.Field.DisplayText);
      //reset the modified flag that is changed after setting the text
      FDataLink.IsModified := False;
    end;
  end
  else
    FDatalink.Reset;
end;

procedure TDBEdit.WndProc(var Message: TLMessage);
begin
  case Message.Msg of
    LM_CLEAR,
    LM_CUT,
    LM_PASTE:
      begin
        if FDataLink.CanModify then
        begin
          //LCL changes the Text before LM_PASTE is called and not after like Delphi. Issue 20330
          //When Edit is called the Text property is reset to the previous value
          //Add a workaround while bug is not fixed
          FDataLink.OnDataChange := nil;
          FDatalink.Edit;
          FDataLink.Modified;
          FDataLink.OnDataChange := @DataChange;
          inherited WndProc(Message);
        end
        else
          Message.Result := 1; // prevent calling default window proc
      end;
    else
      inherited WndProc(Message);
  end;
end;

{ Public Methods }
constructor TDBEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := @DataChange;
  FDataLink.OnUpdateData := @UpdateData;
end;

destructor TDBEdit.Destroy;
begin
  FDataLink.Destroy;
  inherited Destroy;
end;

function TDBEdit.ExecuteAction(AAction: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(AAction) or
            (FDataLink <> nil) and FDataLink.ExecuteAction(AAction);
end;

function TDBEdit.UpdateAction(AAction: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(AAction) or
            (FDataLink <> nil) and FDataLink.UpdateAction(AAction);
end;

