{%MainUnit ../graphics.pp}

{******************************************************************************
                         TPortableNetworkGraphic
 ******************************************************************************

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

{ TPortableNetworkGraphic }

class function TPortableNetworkGraphic.GetFileExtensions: string;
begin
  Result:='png';
end;

class function TPortableNetworkGraphic.GetReaderClass: TFPCustomImageReaderClass;
begin
  Result := TLazReaderPNG;
end;

class function TPortableNetworkGraphic.GetSharedImageClass: TSharedRasterImageClass;
begin
  Result := TSharedPortableNetworkGraphic;
end;

class function TPortableNetworkGraphic.GetWriterClass: TFPCustomImageWriterClass;
begin
  Result := TLazWriterPNG;
end;

procedure TPortableNetworkGraphic.InitializeWriter(AImage: TLazIntfImage; AWriter: TFPCustomImageWriter);
var
  W: TFPWriterPNG absolute AWriter;
begin
  inherited InitializeWriter(AImage, AWriter);
  if AWriter is TFPWriterPNG
  then W.Indexed := W.Indexed or PaletteAllocated;
end;

// included by graphics.pp
