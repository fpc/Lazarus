{%MainUnit ../graphics.pp}

{******************************************************************************
                                TJPegImage
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{ TJPEGImage }

constructor TJPEGImage.Create;
begin
  inherited Create;
  FPerformance := jpBestQuality;
  FProgressiveEncoding := False;
  FGrayScale := False;
  FQuality := 75;
end;

procedure TJPEGImage.Compress;
var
  TempStream: TMemoryStream;
begin
  TempStream := TMemoryStream.Create;
  try
    FreeSaveStream;
    SaveToStream(TempStream);
    TempStream.Position := 0;
    LoadFromStream(TempStream);
  finally
    TempStream.Free;
  end;
end;

class function TJPEGImage.IsStreamFormatSupported(Stream: TStream): Boolean;
var
  Pos: Int64;
  SOI: Word;
begin
  Pos := Stream.Position;
  try
    Stream.Read(SOI, SizeOf(SOI));
    Result := SOI = NtoLE($D8FF);
  finally
    Stream.Position := Pos;
  end;
end;

procedure TJPEGImage.FinalizeReader(AReader: TFPCustomImageReader);
begin
  FProgressiveEncoding := TFPReaderJPEG(AReader).ProgressiveEncoding;
  FGrayScale := TFPReaderJPEG(AReader).GrayScale;
  inherited;
end;

class function TJPEGImage.GetFileExtensions: string;
begin
  Result := 'jpeg;jpg;jpe;jfif';
end;

class function TJPEGImage.GetReaderClass: TFPCustomImageReaderClass;
begin
  Result := TFPReaderJPEG;
end;

class function TJPEGImage.GetSharedImageClass: TSharedRasterImageClass;
begin
  Result := TSharedJPEGImage;
end;

class function TJPEGImage.GetWriterClass: TFPCustomImageWriterClass;
begin
  Result := TFPWriterJPEG;
end;

procedure TJPEGImage.InitializeReader(AImage: TLazIntfImage; AReader: TFPCustomImageReader);
begin
  inherited;
  TFPReaderJPEG(AReader).Performance := Performance;
  TFPReaderJPEG(AReader).Scale := Scale;
end;

procedure TJPEGImage.InitializeWriter(AImage: TLazIntfImage; AWriter: TFPCustomImageWriter);
begin
  inherited;
  if not(AWriter is TFPWriterJPEG) then Exit;
  TFPWriterJPEG(AWriter).ProgressiveEncoding := ProgressiveEncoding;
  TFPWriterJPEG(AWriter).CompressionQuality := CompressionQuality;
  TFPWriterJPEG(AWriter).GrayScale := GrayScale;
end;

procedure TJPEGImage.SetCompressionQuality(AValue: TJPEGQualityRange);
begin
  if FQuality = AValue then Exit;
  FQuality := AValue;
  FreeSaveStream;
end;

procedure TJPEGImage.SetGrayScale(AValue: Boolean);
begin
  if FGrayScale = AValue then Exit;
  FGrayScale := AValue;
  FreeSaveStream;
end;
