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
procedure TDBImage.Change;
begin
  //need to override this to make sure the datalink gets notified
  //its been modified, then when post etc, it will call
  //updatedata to update the field data with current value
  FDataLink.Modified;
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
    ChangeDataSource(Self,FDataLink,AValue);
end;

procedure TDBImage.SetReadOnly(const AValue: Boolean);
begin
  FDataLink.ReadOnly:=AValue;
end;

procedure TDBImage.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink);
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
  FUpdatingRecord := True;
  Picture.Graphic:=nil;
  FPictureLoaded:=False;
  if AutoDisplay then LoadPicture;
  FUpdatingRecord := False;
end;

procedure TDBImage.UpdateData(Sender: TObject);

var s        : Tstream;
    fe       : String;
    i        : Integer;

begin
  if not assigned(Picture.Graphic) or (Picture.Graphic.Empty) then
    begin
    FDataLink.Field.Clear;
    end
  else
    begin
    fe := Picture.Graphic.GetFileExtensions;
    s := FDataLink.DataSet.CreateBlobStream(FDataLink.Field,bmwrite);
    try
      i := pos(';',fe);
      if i > 0 then fe := copy(fe,1,i-1);
      s.WriteAnsiString(fe);
      Picture.Graphic.SaveToStream(s);
    finally
      s.Free;
    end;
    end;
end;

procedure TDBImage.ActiveChange(Sender: TObject);
begin
  if FDatalink.Active then datachange(sender)
  else
    begin
    Picture.Clear;
    FDataLink.reset;
    end;
end;

procedure TDBImage.PictureChanged(Sender: TObject);
begin
  Inherited;
  if not FUpdatingRecord then
    Change;
end;

procedure TDBImage.LoadPicture;

var s        : Tstream;
    GraphExt : string;
    gc       : TGraphicClass;
    AGraphic : TGraphic;
    
begin
  if not FPictureLoaded then
    begin
    FUpdatingRecord := True;
    if not assigned(FDatalink.Field) then Picture.Assign(FDatalink.Field)
    else
    if FDatalink.field.IsBlob then
      begin
      if FDatalink.field is TBlobField then
        begin
        if FDatalink.Field.IsNull then
          begin
          Picture.Clear;
          exit;
          end;
        s := FDataLink.DataSet.CreateBlobStream(FDataLink.Field,bmRead);
        if (S=Nil) or (s.Size = 0) then
          begin
          Picture.Clear;
          exit;
          end;
        AGraphic := nil;
        try
          GraphExt :=  s.ReadAnsiString;

          gc := GetGraphicClassForFileExtension(GraphExt);
          if assigned(gc) then
            begin
            AGraphic := gc.Create;
            AGraphic.LoadFromStream(s);

            Picture.Assign(AGraphic);
            end;
        finally
          if assigned(AGraphic) then AGraphic.Free;
          s.Free;
        end {try}

        end
      else
        Picture.Assign(FDataLink.FField);
        
      end;
    FUpdatingRecord := False;
    end;
end;

procedure TDBImage.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then
    DataChange(Self);
end;

class procedure TDBImage.WSRegisterClass;
const
  Done: Boolean = False;
begin
  if Done then
    Exit;
  inherited WSRegisterClass;
  RegisterPropertyToSkip(TDBImage, 'Picture', 'Removed in 0.9.29. DB control should not save/load their data from stream.', '');
  Done := True;
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
  FDataLInk.OnActiveChange := @ActiveChange;
  FUpdatingRecord := False;
end;

destructor TDBImage.Destroy;
begin
  FDataLink.Free;
  FDataLink:=nil;
  inherited Destroy;
end;

// included by dbctrls.pas
