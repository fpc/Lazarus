{%MainUnit ../dbctrls.pas}
{
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

function TDBComboBox.GetComboText: string;
var
  i: Integer;
begin
  if Style in [csDropDown,csSimple] then
    Result:=Text
  else begin
    i:=ItemIndex;
    if i<0 then
      Result:=''
    else
      Result:=Items[i];
  end;
end;

procedure TDBComboBox.SetComboText(const NewText: string);
var
  i: Integer;
begin
  if NewText=GetComboText then exit;
  if NewText='' then
    i:=-1
  else
    i:=Items.IndexOf(NewText);
  ItemIndex:=i;
  if ItemIndex>=0 then Exit;
  if Style in [csDropDown,csSimple] then Text:=NewText;
end;

procedure TDBComboBox.DataChange(Sender: TObject);
begin
  if not (Style=csSimple) and DroppedDown then Exit;
  if FDataLink.Field <> nil then
    // ToDo: use Field.Text
    SetComboText(FDataLink.Field.DisplayText)
  else
    if csDesigning in ComponentState then
      SetComboText(Name)
    else
      SetComboText('');
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
  // ToDo: use Field.Text
  FDataLink.Field.AsString:=GetComboText;
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

constructor TDBComboBox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle:=ControlStyle+[csReplicatable];
  FDataLink:=TFieldDataLink.Create;
  FDataLink.Control:=Self;
  FDataLink.OnDataChange:=@DataChange;
  FDataLink.OnUpdateData:=@UpdateData;
  FDataLink.OnEditingChange:=@EditingChange;
end;

destructor TDBComboBox.Destroy;
begin
  FDataLink.Free;
  FDataLink:=nil;
  inherited Destroy;
end;

// included by dbctrls.pas

