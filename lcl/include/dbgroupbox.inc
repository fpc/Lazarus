{%MainUnit ../dbctrls.pp}
{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{ TDBGroupBox }

function TDBGroupBox.GetDataField: string;
begin
  Result:=FDataLink.FieldName;
end;

function TDBGroupBox.GetDataSource: TDataSource;
begin
  Result:=FDataLink.DataSource;
end;

function TDBGroupBox.GetField: TField;
begin
  Result:=FDataLink.Field;
end;

procedure TDBGroupBox.SetDataField(const AValue: string);
begin
  FDataLink.FieldName:=AValue;
end;

procedure TDBGroupBox.SetDataSource(const AValue: TDataSource);
begin
  ChangeDataSource(Self,FDataLink,AValue);
end;

procedure TDBGroupBox.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink);
end;

procedure TDBGroupBox.DataChange(Sender: TObject);
begin
  if FDataLink.Field<>nil then
    Caption:=FDataLink.Field.DisplayText
  else
    Caption:='';
end;

procedure TDBGroupBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) then begin
    if (FDataLink<>nil) and (AComponent=DataSource) then
      DataSource:=nil;
  end;
end;

constructor TDBGroupBox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FDataLink:=TFieldDataLink.Create;
  FDataLink.Control:=Self;
  FDataLink.OnDataChange:=@DataChange;
end;

destructor TDBGroupBox.Destroy;
begin
  FDataLink.Destroy;
  inherited Destroy;
end;

function TDBGroupBox.ExecuteAction(AAction: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(AAction) or
            (FDataLink <> nil) and FDataLink.ExecuteAction(AAction);
end;

function TDBGroupBox.UpdateAction(AAction: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(AAction) or
            (FDataLink <> nil) and FDataLink.UpdateAction(AAction);
end;

// included by dbctrls.pp
