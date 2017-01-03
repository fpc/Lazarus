{%MainUnit ../graphics.pp}

{******************************************************************************
                         TPortableAnyMapGraphic
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
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

class function TPortableAnyMapGraphic.IsStreamFormatSupported(Stream: TStream): Boolean;
var
  Pos: Int64;
  C: array [0..1] of Char;
  r: integer;
begin
  Pos := Stream.Position;
  try
    r := Stream.Read(C, 2);
    if r = 2 then
    begin
      Result := (C[0] = 'P');
      Result := Result and ((Ord(C[1])-Ord('0')) in [1..6]); // P1,P2,..,P6
    end else begin
      Result := False;
    end;
  finally
    Stream.Position := Pos;
  end;
end;

class function TPortableAnyMapGraphic.GetWriterClass: TFPCustomImageWriterClass;
begin
  Result := TFPWriterPNM;
end;


// included by graphics.pp
