{%MainUnit ../dbctrls.pas}
{
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

procedure TDBGroupBox.Loaded;
begin
  inherited Loaded;
  //need to make sure the state is updated on first load
  if (csDesigning in ComponentState) then
    DataChange(Self);
end;

procedure TDBGroupBox.Notification(AComponent: TComponent; Operation: TOperation
  );
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
  FDataLink.Free;
  FDataLink:=nil;
  inherited Destroy;
end;

// included by dbctrls.pas
