{%MainUnit ../dbctrls.pp}

{******************************************************************************
                                     TDBListBox
                    data aware ListBox, base found in dbctrls.pp
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

// included by dbctrls.pp

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
  FDataLink.Modified;
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

procedure TCustomDBComboBox.CloseUp;
begin
  if [csLoading,csDestroying,csDesigning]*ComponentState<>[] then exit;
  if not ReadOnly then
    EditingDone;
 inherited CloseUp;
end;

procedure TCustomDBComboBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) then begin
    if (FDataLink<>nil) and (AComponent=DataSource) then
      DataSource:=nil;
  end;
end;

procedure TCustomDBComboBox.DoEditingDone;
begin
  FDataLink.UpdateRecord;
  inherited DoEditingDone;
end;

procedure TCustomDBComboBox.WndProc(var Message: TLMessage);
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

constructor TCustomDBComboBox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle:=ControlStyle+[csReplicatable];
  FDataLink:=TFieldDataLink.Create;
  FDataLink.Control:=Self;
  FDataLink.OnDataChange:=@DataChange;
  FDataLink.OnUpdateData:=@UpdateData;
end;

destructor TCustomDBComboBox.Destroy;
begin
  FDataLink.Destroy;
  inherited Destroy;
end;

function TCustomDBComboBox.ExecuteAction(AAction: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(AAction) or
            (FDataLink <> nil) and FDataLink.ExecuteAction(AAction);
end;

function TCustomDBComboBox.UpdateAction(AAction: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(AAction) or
            (FDataLink <> nil) and FDataLink.UpdateAction(AAction);
end;

