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
  inherited Change;
  FDetectedEvents := FDetectedEvents or DBCBEVENT_CHANGE;
  PostMessage(Handle, LM_DEFERREDEDIT, 0, 0);
end;

procedure TCustomDBComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key=VK_ESCAPE then begin
    //cancel out of editing by reset on esc
    if FDatalink.EditingSource then
      FDatalink.DataSet.Cancel
    else
      FDataLink.Reset;
    SelectAll;
    Key := VK_UNKNOWN;
  end;
end;

procedure TCustomDBComboBox.UpdateRecord;
begin
  if FDatalink.Editing and FDatalink.IsModified then
    FDataLink.UpdateRecord;
end;

function TCustomDBComboBox.GetReadOnly: Boolean;
begin
  Result:=FDataLink.ReadOnly;
end;

function TCustomDBComboBox.DoEdit: boolean;
var
  oldDataChange: TNotifyEvent;

  procedure RestoreDataChange;
  begin
    FDataLink.OnDataChange := oldDataChange;
    if not result then begin
      FDatalink.Reset;
      SelectAll;
    end;
  end;

begin

  oldDataChange := FDataLink.OnDataChange;
  FDataLink.OnDataChange := nil;

  try
    try

      result := FDatalink.Edit;
      RestoreDataChange;

      if result then begin
        FDatalink.Modified;
        if FDetectedEvents and DBCBEVENT_CHANGE <> 0 then
          inherited Change;
        if FDetectedEvents and DBCBEVENT_SELECT <> 0 then begin
          if FDetectedEvents and DBCBEVENT_WHEEL <> 0 then
            FDatalink.UpdateRecord;
          DoOnSelect;
        end;
      end;
      if FDetectedEvents and DBCBEVENT_CLOSEUP <> 0 then
        DoOnCloseUp;

    except
      on E: Exception do begin

        result := false;
        RestoreDataChange;
        if FDetectedEvents and DBCBEVENT_CLOSEUP <> 0 then
          DoOnCloseUp;

        raise;
      end;
    end;

  finally
    FDetectedEvents := 0;
  end;
end;

procedure TCustomDBComboBox.DoOnCloseUp;
begin
  if Assigned(OnCloseUp) then OnCloseUp(Self);
  if AutoSelect then
  begin
    SelectAll;
    if (SelText = Text) then
      AutoSelected := True;
  end;//End if FAutoSelect
  FDetectedEvents := 0;   // reset because closeup may occur without editing
end;

procedure TCustomDBComboBox.DoOnSelect;
begin
  inherited Select;
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

procedure TCustomDBComboBox.LMDeferredEdit(var Message: TLMessage);
begin
  DoEdit;
end;

procedure TCustomDBComboBox.CloseUp;
begin
  if [csLoading,csDestroying,csDesigning]*ComponentState<>[] then exit;
  if FDetectedEvents and DBCBEVENT_CHANGE = 0 then
    DoOnCloseUp
  else
    FDetectedEvents := FDetectedEvents or DBCBEVENT_CLOSEUP;
end;

procedure TCustomDBComboBox.Select;
begin
  FDetectedEvents := FDetectedEvents or DBCBEVENT_SELECT;
end;

function TCustomDBComboBox.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  FDetectedEvents := FDetectedEvents or DBCBEVENT_WHEEL;
end;

procedure TCustomDBComboBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) then begin
    if (FDataLink<>nil) and (AComponent=DataSource) then
      DataSource:=nil;
  end;
end;

procedure TCustomDBComboBox.EditingDone;
begin
  UpdateRecord;
  inherited EditingDone;
end;

procedure TCustomDBComboBox.WndProc(var Message: TLMessage);
begin
  case Message.Msg of
    LM_CLEAR,
    LM_CUT,
    LM_PASTE:
      begin
        if FDataLink.CanModify then
          inherited WndProc(Message)
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

