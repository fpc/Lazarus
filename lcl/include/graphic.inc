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

constructor TGraphic.Create;
begin
  VirtualCreate;
end;

constructor TGraphic.VirtualCreate;
begin
  Inherited Create;
end;

procedure TGraphic.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not (Filer.Ancestor is TGraphic) or
        not Equals(TGraphic(Filer.Ancestor))
    else
      Result := not Empty;
  end;

begin
  Filer.DefineBinaryProperty('Data', @ReadData, @WriteData, DoWrite);
end;

procedure TGraphic.Changed(Sender: TObject);
begin
  FModified := True;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TGraphic.Progress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Sender, Stage, PercentDone, RedrawNow, R, Msg);
end;

function TGraphic.Equals(Graphic: TGraphic): Boolean;
var
  SelfImage, GraphicsImage: TMemoryStream;
  IsEmpty: boolean;
begin
  Result := (Graphic <> nil) and (ClassType = Graphic.ClassType);
  if not Result then exit;
  IsEmpty:=Empty;
  Result:=(IsEmpty=Graphic.Empty);
  if (not Result) or IsEmpty or (Self=Graphic) then exit;
  // ToDo: check for same resource
  SelfImage := TMemoryStream.Create;
  try
    WriteData(SelfImage);
    GraphicsImage := TMemoryStream.Create;
    try
      Graphic.WriteData(GraphicsImage);
      Result := (SelfImage.Size = GraphicsImage.Size) and
        CompareMem(SelfImage.Memory, GraphicsImage.Memory, SelfImage.Size);
    finally
      GraphicsImage.Free;
    end;
  finally
    SelfImage.Free;
  end;
end;

procedure TGraphic.ReadData(Stream: TStream);
begin
  LoadFromStream(Stream);
end;

procedure TGraphic.SaveToFile(const Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(Filename, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TGraphic.LoadFromFile(const Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(Filename, fmOpenRead{ or fmShareDenyWrite});
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TGraphic.WriteData(Stream: TStream);
begin
  SaveToStream(Stream);
end;

function TGraphic.GetTransparent: Boolean;
begin
  Result := FTransparent;
end;

procedure TGraphic.SetModified(Value: Boolean);
begin
  if Value then
    Changed(Self)
  else
    FModified := False;
end;

procedure TGraphic.SetTransparent(Value: Boolean);
begin
  if Value <> FTransparent then begin
    FTransparent := Value;
    Changed(Self);
  end;
end;


