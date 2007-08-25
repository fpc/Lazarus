{%MainUnit ../graphics.pp}

{******************************************************************************
                         TPortableNetworkGraphic
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

{ TPortableNetworkGraphic }

procedure TPortableNetworkGraphic.InitFPImageWriter(IntfImg: TLazIntfImage; ImgWriter: TFPCustomImageWriter);
begin
  inherited InitFPImageWriter(IntfImg, ImgWriter);
  if ImgWriter is TFPWriterPNG then
  begin
    TFPWriterPNG(ImgWriter).UseAlpha := True;
    TFPWriterPNG(ImgWriter).Indexed := PaletteAllocated or
      (PixelFormat in [pf1bit, pf4bit, pf8bit]);
    TFPWriterPNG(ImgWriter).WordSized := False;
  end;
end;

class function TPortableNetworkGraphic.GetFileExtensions: string;
begin
  Result:='png';
end;

class function TPortableNetworkGraphic.GetDefaultFPReader: TFPCustomImageReaderClass;
begin
  Result:=TLazReaderPNG;
end;

class function TPortableNetworkGraphic.GetDefaultFPWriter: TFPCustomImageWriterClass;
begin
  Result:=TFPWriterPNG;
end;

// included by graphics.pp
