{%MainUnit ../dbctrls.pas}
{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

{ TDBImage }

function TDBImage.GetDataField: string;
begin
  Result:=FDataLink.FieldName;
end;

function TDBImage.GetDataSource: TDataSource;
begin
  Result:=FDataLink.DataSource;
end;

function TDBImage.GetField: TField;
begin
  Result:=FDataLink.Field;
end;

function TDBImage.GetReadOnly: Boolean;
begin
  Result:=FDataLink.ReadOnly;
end;

procedure TDBImage.SetAutoDisplay(const AValue: Boolean);
begin
  if FAutoDisplay=AValue then exit;
  FAutoDisplay:=AValue;
  if FAutoDisplay then LoadPicture;
end;

procedure TDBImage.SetDataField(const AValue: string);
begin
  FDataLink.FieldName:=AValue;
end;

procedure TDBImage.SetDataSource(const AValue: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource:=AValue;
  if AValue <> nil then
    AValue.FreeNotification(Self);
end;

procedure TDBImage.SetReadOnly(const AValue: Boolean);
begin
  FDataLink.ReadOnly:=AValue;
end;

procedure TDBImage.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) then begin
    if (FDataLink<>nil) and (AComponent=DataSource) then
      DataSource:=nil;
  end;
end;

procedure TDBImage.DataChange(Sender: TObject);
begin
  Picture.Graphic:=nil;
  FPictureLoaded:=False;
  if AutoDisplay then LoadPicture;
end;

procedure TDBImage.UpdateData(Sender: TObject);
begin
  if Picture.Graphic is TBitmap then
    FDataLink.Field.Assign(Picture.Graphic)
  else
    FDataLink.Field.Clear;
end;

procedure TDBImage.LoadPicture;
begin
  if not FPictureLoaded
  and (not Assigned(FDataLink.Field) or FDataLink.Field.IsBlob) then
    Picture.Assign(FDataLink.Field);
end;

procedure TDBImage.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then
    DataChange(Self);
end;

constructor TDBImage.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle:=ControlStyle+[csReplicatable];
  FAutoDisplay:=True;
  FQuickDraw:=true;
  FDataLink:=TFieldDataLink.Create;
  FDataLink.Control:=Self;
  FDataLink.OnDataChange:=@DataChange;
  FDataLink.OnUpdateData:=@UpdateData;
end;

destructor TDBImage.Destroy;
begin
  FDataLink.Free;
  FDataLink:=nil;
  inherited Destroy;
end;

// included by dbctrls.pas
