{%MainUnit ../graphics.pp}

{******************************************************************************
                               TicnsIcon
 ******************************************************************************

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

{ TIcnsList }

function TIcnsList.GetItem(Index: Integer): PIcnsRec;
begin
  Result := inherited Get(Index);
end;

procedure TIcnsList.SetItem(Index: Integer; const AValue: PIcnsRec);
begin
  inherited Put(Index, AValue);
end;

procedure TIcnsList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
    Dispose(PIcnsRec(Ptr));
  inherited Notify(Ptr, Action);
end;

function TIcnsList.Add(AIconType: TicnsIconType; ARawImage: TRawImage): Integer;
var
  Rec: PIcnsRec;
begin
  New(Rec);
  Rec^.IconType := AIconType;
  Rec^.RawImage := ARawImage;
  Result := inherited Add(Rec);
end;

{ TIcnsIcon }

procedure TIcnsIcon.IcnsAdd(AIconType: TicnsIconType; ARawImage: TRawImage);

  function GetMaskList: TIcnsList;
  begin
    if FMaskList = nil then
      FMaskList := TIcnsList.Create;
    Result := FMaskList;
  end;

  function GetImageList: TIcnsList;
  begin
    if FImageList = nil then
      FImageList := TIcnsList.Create;
    Result := FImageList;
  end;

begin
  if AIconType in icnsMaskTypes
  then GetMaskList.Add(AIconType, ARawImage)
  else GetImageList.Add(AIconType, ARawImage);
end;

procedure TIcnsIcon.IcnsProcess;

  procedure MergeMask(var AImage, AMask: TRawImage);
  var
    LazIntfImage, LazIntfMask: TLazIntfImage;
    Col, Row: Integer;
    Color: TFpColor;
  begin
    if AMask.Description.Depth = 1 then
    begin
      // merge mask
      AImage.Description.MaskBitOrder := AMask.Description.MaskBitOrder;
      AImage.Description.MaskLineEnd := AMask.Description.MaskLineEnd;
      AImage.Description.MaskBitsPerPixel := AMask.Description.MaskBitsPerPixel;
      AImage.Description.MaskShift := AMask.Description.MaskShift;
      AImage.MaskSize := AMask.MaskSize;
      AImage.Mask := ReallocMem(AImage.Mask, AMask.MaskSize);
      Move(AMask.Mask^, AImage.Mask^, AMask.MaskSize);
    end
    else
    begin
      LazIntfImage := TLazIntfImage.Create(AImage, False);
      LazIntfMask := TLazIntfImage.Create(AMask, False);
      for Row := 0 to LazIntfImage.Height - 1 do
        for Col := 0 to LazIntfImage.Width - 1 do
        begin
          Color := LazIntfImage.Colors[Col,Row];
          Color.alpha := LazIntfMask.Colors[Col,Row].alpha;
          LazIntfImage.Colors[Col,Row] := Color;
        end;
      LazIntfMask.Free;
      LazIntfImage.Free;
    end;
  end;

var
  i, AIndex: integer;
  IconType: TicnsIconType;
  ImagesForMask: TicnsIconTypes;
  IconImage: TIconImage;
begin
  // merge separate image and masc rawdata together

  if FMaskList <> nil then
  begin
    for i := 0 to FMaskList.Count - 1 do
    begin
      ImagesForMask := icnsMaskToImageMap[FMaskList[i]^.IconType];
      for AIndex := 0 to FImageList.Count - 1 do
        if FImageList[AIndex]^.IconType in ImagesForMask then
          MergeMask(FImageList[AIndex]^.RawImage, FMaskList[i]^.RawImage);
      // dispose RawImage since no more needed
      FMaskList[i]^.RawImage.FreeData;
    end;
    FreeAndNil(FMaskList);
  end;

  IconType := iitNone;
  for i := 0 to FImageList.Count - 1 do
  begin
    // todo: we have no jpeg 2000 reader to decompress their data => skip for now
    if FImageList[i]^.IconType in icnsWithAlpha then
      Continue;

    // Add image
    with TSharedIcon(FSharedImage) do
    begin
      IconImage := GetImagesClass.Create(FImageList[i]^.RawImage);
      Add(IconImage);
    end;
    if FImageList[i]^.IconType > IconType then
    begin
      IconType := FImageList[i]^.IconType;
      FCurrent := TSharedIcon(FSharedImage).Count - 1;
    end;
  end;
  FreeAndNil(FImageList);
end;

constructor TIcnsIcon.Create;
begin
  inherited Create;
  FImageList := nil;
  FMaskList := nil;
end;

destructor TIcnsIcon.Destroy;
begin
  inherited Destroy;
  FImageList.Free;
  FMaskList.Free;
end;

procedure TIcnsIcon.ReadData(Stream: TStream);
var
  Resource: TIconFamilyResource;
  Position: Int64;
begin
  Position := Stream.Position;
  Stream.Read(Resource, SizeOf(Resource));
  if Resource.resourceType = kIconFamilyType then
  begin
    Stream.Position := Position;
    LoadFromStream(Stream, BEtoN(Resource.resourceSize))
  end else
  begin
    Stream.Position := Position;
    LoadFromStream(Stream);
  end;
end;

procedure TIcnsIcon.ReadStream(AStream: TMemoryStream; ASize: Longint);
var
  Resource: TIconFamilyResource;

  IntfImage: TLazIntfImage;
  ImgReader: TLazReaderIcnsPart;
  LazReader: ILazImageReader;
  RawImage: TRawImage;
begin
  AStream.Read(Resource, SizeOf(Resource));

  if (Resource.resourceType <> kIconFamilyType) then
    raise EInvalidGraphic.Create('Stream is not an ICNS type');

  IntfImage := nil;
  ImgReader := nil;

  Resource.resourceSize := BEtoN(Resource.resourceSize);

  if ASize > Resource.resourceSize then
    ASize := Resource.resourceSize;

  while AStream.Position < ASize do
  begin
    if IntfImage = nil
    then IntfImage := TLazIntfImage.Create(0,0)
    else IntfImage.SetSize(0,0);

    if ImgReader = nil
    then ImgReader := TLazReaderIcnsPart.Create;

    if Supports(ImgReader, ILazImageReader, LazReader)
    then LazReader.UpdateDescription := True
    else IntfImage.DataDescription := QueryDescription([riqfRGB, riqfAlpha, riqfMask]); // fallback to default

    ImgReader.ImageRead(AStream, IntfImage);
    IntfImage.GetRawImage(RawImage, True);

    IcnsAdd(ImgReader.IconType, RawImage);
  end;

  LazReader := nil;
  IntfImage.Free;
  ImgReader.Free;

  IcnsProcess;
end;

procedure TIcnsIcon.WriteStream(AStream: TMemoryStream);
begin
  //
end;

class function TIcnsIcon.GetFileExtensions: string;
begin
  Result := 'icns';
end;

function TIcnsIcon.LazarusResourceTypeValid(const ResourceType: string): boolean;
begin
  Result := (UpperCase(ResourceType) = 'ICNS');
end;
