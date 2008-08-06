{%MainUnit ../dbctrls.pas}
{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

{ TDBCheckBox }

function TDBCheckBox.GetDataField: string;
begin
  Result:=FDataLink.FieldName;
end;

function TDBCheckBox.GetDataSource: TDataSource;
begin
  Result:=FDataLink.DataSource;
end;

function TDBCheckBox.GetField: TField;
begin
  Result:=FDataLink.Field;
end;

function TDBCheckBox.GetReadOnly: Boolean;
begin
  Result:=FDataLink.ReadOnly;
end;

procedure TDBCheckBox.SetDataField(const AValue: string);
begin
  FDataLink.FieldName:=AValue;
end;

procedure TDBCheckBox.SetDataSource(const AValue: TDataSource);
begin
  if AValue=DataSource then exit;
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    ChangeDataSource(Self,FDataLink,AValue);
end;

procedure TDBCheckBox.SetReadOnly(const AValue: Boolean);
begin
  FDataLink.ReadOnly:=AValue;
end;

procedure TDBCheckBox.SetValueCheck(const AValue: string);
begin
  if FValueCheck=AValue then exit;
  FValueCheck:=AValue;
  DataChange(Self);
end;

procedure TDBCheckBox.SetValueUncheck(const AValue: string);
begin
  if FValueUncheck=AValue then exit;
  FValueUncheck:=AValue;
  DataChange(Self);
end;

function TDBCheckBox.GetFieldCheckState: TCheckBoxState;
var
  FieldText: string;
begin
  if FDatalink.Field=nil then begin
    Result:=cbUnchecked;
    exit;
  end;
  if FDataLink.Field.IsNull then
    Result:=cbGrayed
  else if FDataLink.Field.DataType = ftBoolean then begin
    if FDataLink.Field.AsBoolean then
      Result:=cbChecked
    else
      Result:=cbUnchecked;
  end else begin
    Result:=cbGrayed;
    // ToDo: use Field.Text
    FieldText:=FDataLink.Field.DisplayText;
    if ValueEqualsField(FValueCheck,FieldText) then
      Result:=cbChecked
    else if ValueEqualsField(FValueUncheck,FieldText) then
      Result:=cbUnchecked;
  end;
end;

procedure TDBCheckBox.DataChange(Sender: TObject);
begin
  State:=GetFieldCheckState;
  // if original state was unchecked and after editing is checked
  // but if before post, edits are cancelled, FlastcheckedOnChange has a
  // wrong value it won't detect changes next time, correct here.
  fLastCheckedOnChange:=Checked;
end;

procedure TDBCheckBox.UpdateData(Sender: TObject);
var
  NewFieldText: string;
begin
  if State = cbGrayed then
    FDataLink.Field.Clear
  else
    if FDataLink.Field.DataType = ftBoolean then
      FDataLink.Field.AsBoolean:=Checked
    else begin
      if Checked then
        NewFieldText:=FValueCheck
      else
        NewFieldText:=FValueUncheck;
      // ToDo: use Field.Text
      FDataLink.Field.AsString:=Trim(NewFieldText);
    end;
end;

procedure TDBCheckBox.FocusRequest(Sender: TObject);
begin
  //the FieldLink has requested the control
  //recieve focus for some reason..
  //perhaps an error occured?
  SetFocus;
end;

procedure TDBCheckBox.Notification(AComponent: TComponent; Operation: TOperation
  );
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) then begin
    if (FDataLink<>nil) and (AComponent=DataSource) then
      DataSource:=nil;
  end;
end;

procedure TDBCheckBox.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then
    DataChange(Self);
end;

procedure TDBCheckBox.EditingDone;
begin
  if FDatalink.Edit then begin
    FDatalink.Modified;
    FDataLink.UpdateRecord;
  end else
    State:=GetFieldCheckState;
  inherited EditingDone;
end;

function TDBCheckBox.ValueEqualsField(const AValue, AFieldText: string
  ): boolean;
begin
  Result:=AnsiCompareText(AValue,AFieldText)=0;
end;

procedure TDBCheckBox.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink);
end;

constructor TDBCheckBox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FValueCheck:='True';
  FValueUncheck:='False';

  ControlStyle:=ControlStyle+[csReplicatable];
  State:=cbUnchecked;
  FDataLink:=TFieldDataLink.Create;
  FDataLink.Control:=Self;
  FDataLink.OnDataChange:=@DataChange;
  FDataLink.OnUpdateData:=@UpdateData;
end;

destructor TDBCheckBox.Destroy;
begin
  FDataLink.Free;
  FDataLink:=nil;
  inherited Destroy;
end;

// included by dbctrls.pas
