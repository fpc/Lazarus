{%MainUnit ../dbctrls.pp}

{******************************************************************************
                                     TDBText
                    data aware label, base found in dbctrls.pp
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

procedure TDBText.SetDataField(Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TDBText.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
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

{
  $Log$
  Revision 1.4  2004/08/22 22:57:35  mattias
  added OnActiveChange  from Joost

  Revision 1.3  2004/04/10 17:58:57  mattias
  implemented mainunit hints for include files

  Revision 1.2  2003/09/22 15:03:19  ajgenius
  partly fixed streaming of DBCalendar, and opRemove notification of DBText DBEdit DBCalendar

  Revision 1.1  2003/09/14 18:40:55  ajgenius
  add initial TFieldDataLink, TDBEdit and TDBText


}
