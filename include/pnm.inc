{%MainUnit ../graphics.pp}

{******************************************************************************
                         TPortableAnyMapGraphic
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

{ TPortableAnyMapGraphic }

class function TPortableAnyMapGraphic.GetFileExtensions: string;
begin
  Result:='pbm;pgm;ppm';
end;

class function TPortableAnyMapGraphic.GetReaderClass: TFPCustomImageReaderClass;
begin
  Result := TFPReaderPNM;
end;

class function TPortableAnyMapGraphic.GetSharedImageClass: TSharedRasterImageClass;
begin
  Result := TSharedPortableAnyMapGraphic;
end;

class function TPortableAnyMapGraphic.GetWriterClass: TFPCustomImageWriterClass;
begin
  Result := TFPWriterPNM;
end;


// included by graphics.pp
