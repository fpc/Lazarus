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

procedure TJPEGImage.InitFPImageReader(IntfImg: TLazIntfImage; ImgReader: TFPCustomImageReader);
var
  JPEGReader: TFPReaderJPEG;
begin
  if ImgReader is TFPReaderJPEG then
  begin
    JPEGReader := TFPReaderJPEG(ImgReader);
    JPEGReader.Performance := Performance;
    JPEGReader.OnProgress := @Progress;
  end;
  inherited InitFPImageReader(IntfImg, ImgReader);
end;

procedure TJPEGImage.FinalizeFPImageReader(ImgReader: TFPCustomImageReader);
var
  JPEGReader: TFPReaderJPEG;
begin
  if ImgReader is TFPReaderJPEG then
  begin
    JPEGReader := TFPReaderJPEG(ImgReader);
    FProgressiveEncoding := JPEGReader.ProgressiveEncoding;
    FGrayScale := JPEGReader.GrayScale;
  end;
  inherited FinalizeFPImageReader(ImgReader);
end;

procedure TJPEGImage.InitFPImageWriter(IntfImg: TLazIntfImage; ImgWriter: TFPCustomImageWriter);
var
  JPEGWriter: TFPWriterJPEG;
begin
  if ImgWriter is TFPWriterJPEG then
  begin
    JPEGWriter := TFPWriterJPEG(ImgWriter);
    if JPEGWriter <> nil then
    begin
      JPEGWriter.ProgressiveEncoding := ProgressiveEncoding;
      JPEGWriter.CompressionQuality := CompressionQuality;
      JPEGWriter.OnProgress := @Progress;
    end;
  end;
  inherited InitFPImageWriter(IntfImg, ImgWriter);
end;

class function TJPEGImage.GetDefaultFPReader: TFPCustomImageReaderClass;
begin
  Result := TFPReaderJPEG;
end;

class function TJPEGImage.GetDefaultFPWriter: TFPCustomImageWriterClass;
begin
  Result := TFPWriterJPEG;
end;

constructor TJPEGImage.Create;
begin
  inherited Create;
  FPerformance := jpBestQuality;
  FProgressiveEncoding := False;
  FGrayScale := False;
  FQuality := 75;
end;

class function TJPEGImage.GetFileExtensions: string;
begin
  Result := 'jpg;jpeg';
end;


