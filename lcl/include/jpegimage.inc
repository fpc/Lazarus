{%MainUnit ../graphics.pp}

{******************************************************************************
                                TJPegImage
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

{ TJPEGImage }

constructor TJPEGImage.Create;
begin
  inherited Create;
  FPerformance := jpBestQuality;
  FProgressiveEncoding := False;
  FGrayScale := False;
  FQuality := 75;
end;

procedure TJPEGImage.FinalizeReader(AReader: TFPCustomImageReader);
begin
  FProgressiveEncoding := TFPReaderJPEG(AReader).ProgressiveEncoding;
  FGrayScale := TFPReaderJPEG(AReader).GrayScale;
  inherited;
end;

class function TJPEGImage.GetFileExtensions: string;
begin
  Result := 'jpg;jpeg;jpe;jfif';
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
  TFPReaderJPEG(AReader).OnProgress := @Progress;
end;

procedure TJPEGImage.InitializeWriter(AImage: TLazIntfImage; AWriter: TFPCustomImageWriter);
begin
  inherited;
  TFPWriterJPEG(AWriter).ProgressiveEncoding := ProgressiveEncoding;
  TFPWriterJPEG(AWriter).CompressionQuality := CompressionQuality;
  TFPWriterJPEG(AWriter).OnProgress := @Progress;
end;

