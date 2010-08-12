{%MainUnit ../dbctrls.pp}

{******************************************************************************
                                     TDBText
                    data aware label, base found in dbctrls.pp
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

{ Private Methods}

//update the caption on next record etc...
procedure TDBText.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
    Caption := FDataLink.Field.DisplayText
  else
    Caption := '';
end;

procedure TDBText.ActiveChange(Sender: TObject);

begin
  if FDatalink.Active then datachange(sender)
    else text := '';
end;

function TDBText.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TDBText.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TDBText.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TDBText.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TDBText.SetDataSource(Value: TDataSource);
begin
  ChangeDataSource(Self,FDataLink,Value);
end;

procedure TDBText.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink);
end;

{ Protected Methods}
procedure TDBText.Loaded;
begin
  inherited Loaded;
  //need to make sure the state is updated on first load
  if (csDesigning in ComponentState) then
    DataChange(Self);
end;

procedure TDBText.Notification(AComponent: TComponent;
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

class procedure TDBText.WSRegisterClass;
const
  Done: Boolean = False;
begin
  if Done then
    Exit;
  inherited WSRegisterClass;
  RegisterPropertyToSkip(TDBText, 'Caption', 'Removed in 0.9.29. DB control should not save/load their data from stream.', '');
  Done := True;
end;

{ Public Methods}
constructor TDBText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := @DataChange;
  FDataLInk.OnActiveChange := @ActiveChange;
end;

destructor TDBText.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;
