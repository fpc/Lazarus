// included by graphics.pp

{******************************************************************************
                         TPortableNetworkGraphic
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

{ TPortableNetworkGraphic }

procedure TPortableNetworkGraphic.LoadFromLazarusResource(const ResName: String
  );
var
  ms:TMemoryStream;
  res:TLResource;
begin
  res:=LazarusResources.Find(ResName);
  if (res<>nil) and (res.Value<>'') and (res.ValueType='PNG') then begin
    ms:=TMemoryStream.Create;
    try
      ms.Write(res.Value[1],length(res.Value));
      ms.Position:=0;
      LoadFromStream(ms);
    finally
      ms.Free;
    end;
  end;
end;

procedure TPortableNetworkGraphic.ReadStream(Stream: TStream; Size: Longint);
{$IFDEF UseFPImage}
var
  IntfImg: TLazIntfImage;
  PNGReader: TFPReaderPNG;
  ImgHandle, ImgMaskHandle: HBitmap;
begin
  UnshareImage;
  if Size = 0 then begin
    Width:=0;
    Height:=0;
    exit;
  end;
  StoreOriginalStream(Stream);

  IntfImg:=nil;
  PNGReader:=nil;
  try
    IntfImg:=TLazIntfImage.Create(0,0);
    PNGReader:=TFPReaderPNG.Create;
    FImage.SaveStream.Position:=0;
    IntfImg.LoadFromStream(FImage.SaveStream,PNGReader);
    IntfImg.CreateBitmap(ImgHandle,ImgMaskHandle);
    Handle:=ImgHandle;
    MaskHandle:=ImgMaskHandle;
  finally
    IntfImg.Free;
    PNGReader.Free;
  end;
end;
{$ELSE}
begin
  RaiseGDBException('TPortableNetworkGraphic.ReadStream needs FPImage');
end;
{$ENDIF}

procedure TPortableNetworkGraphic.WriteStream(Stream: TStream;
  WriteSize: Boolean);
{$IFDEF UseFPImage}

  Procedure DoWriteStreamSize(DestStream: TStream; Size: longint);
  begin
    if WriteSize then
      DestStream.WriteBuffer(Size, SizeOf(Size));
  end;

  procedure DoWriteOriginal;
  begin
    DoWriteStreamSize(Stream,FImage.SaveStream.Size);
    FImage.SaveStream.Position:=0;
    Stream.CopyFrom(FImage.SaveStream,FImage.SaveStream.Size);
  end;

var
  MemStream: TMemoryStream;
  IntfImg: TLazIntfImage;
  {$IFDEF HasPNGWriter}
  PNGWriter: TFPWriterPNG;
  {$ENDIF}
begin
  if (FImage.SaveStream<>nil) and (FImage.SaveStream.Size>0) then begin
    DoWriteOriginal;
    exit;
  end;

  RaiseGDBException('TPortableNetworkGraphic.WriteStream png writer not implemented yet');

  // write image in XPM format to temporary stream
  MemStream:=TMemoryStream.Create;
  IntfImg:=nil;
  {$IFDEF HasPNGWriter}
  PNGWriter:=nil;
  {$ENDIF}
  try
    IntfImg:=TLazIntfImage.Create(0,0);
    IntfImg.LoadFromBitmap(Handle,0);
    {$IFDEF HasPNGWriter}
    PNGWriter:=TFPWriterPNG.Create;
    IntfImg.SaveToStream(MemStream,PNGWriter);
    FreeAndNil(PNGWriter);
    {$ENDIF}
    FreeAndNil(IntfImg);
    // save stream, so that further saves will be fast
    MemStream.Position:=0;
    FreeAndNil(FImage.FSaveStream);
    FImage.SaveStream:=MemStream;
    MemStream:=nil;
    // copy savestream to destination stream
    Stream.CopyFrom(FImage.SaveStream,FImage.SaveStream.Size);
  finally
    MemStream.Free;
    IntfImg.Free;
    {$IFDEF HasPNGWriter}
    PNGWriter.Free;
    {$ENDIF}
  end;
end;
{$ELSE}
begin
  RaiseGDBException('TPortableNetworkGraphic.WriteStream needs FPImage');
end;
{$ENDIF}


// included by graphics.pp

