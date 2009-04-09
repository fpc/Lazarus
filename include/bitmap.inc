{%MainUnit ../graphics.pp}

{******************************************************************************
                                    TBitmap
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

function TestStreamIsBMP(const AStream: TStream): boolean;
var
  Signature: array[0..1] of Char;
  ReadSize: Integer;
  OldPosition: TStreamSeekType;
begin
  OldPosition:=AStream.Position;
  ReadSize:=AStream.Read(Signature, SizeOf(Signature));
  Result:=(ReadSize=2) and (Signature[0]='B') and (Signature[1]='M');
  //debugln('TestStreamIsBMP ',DbgStr(Signature[0]),' ',DbgStr(Signature[1]));
  AStream.Position:=OldPosition;
end;


{ TBitmap }

class function TBitmap.GetFileExtensions: string;
begin
  Result:='bmp';
end;

procedure TBitmap.LoadFromStream(AStream: TStream; ASize: Cardinal);
var
  TempStream: TMemoryStream;
  Header: TBitmapFileHeader;
begin
  if AStream is TResourceStream then
  begin
    FillChar(Header, SizeOf(Header), 0);

    Header.bfType := $4d42;
    Header.bfSize := SizeOf(Header) + ASize;
    Header.bfOffBits := SizeOf(Header);
    {$IFDEF ENDIAN_BIG}
    swap(Header.bfType);
    swap(Header.bfSize);
    swap(Header.bfOffBits);
    {$ENDIF}

    TempStream := TMemoryStream.Create;
    try
      TempStream.SetSize(Header.bfSize);
      TempStream.Write(Header, SizeOf(Header));
      TempStream.CopyFrom(AStream, ASize);
      TempStream.Position := 0;
      inherited LoadFromStream(TempStream, ASize);
    finally
      TempStream.Free;
    end;
  end
  else
    inherited LoadFromStream(AStream, ASize);
end;

class function TBitmap.GetReaderClass: TFPCustomImageReaderClass;
begin
  Result := TLazReaderBMP;
end;

class function TBitmap.GetSharedImageClass: TSharedRasterImageClass;
begin
  Result := TSharedBitmap;
end;
    
class function TBitmap.GetWriterClass: TFPCustomImageWriterClass;
begin
  Result := TFPWriterBMP;
end;

procedure TBitmap.InitializeReader(AImage: TLazIntfImage; AReader: TFPCustomImageReader);
var
  LazReader: TLazReaderBMP absolute AReader;
begin
  inherited;
  
  // TransparentMode
  //   tmAuto: use left bottom pixel
  //   tmFixed: use color
  //
  // TransparentColor:
  //   clDefault: use left, bottom pixel color as transparent color (*)
  //   clNone: load image opaque (*)
  //   otherwise: use TransparentColor as transparent color
  //
  //   (*) these are Lazarus extentions

  if (TransparentMode = tmAuto) or (TransparentColor = clDefault)
  then begin
    LazReader.MaskMode := lrmmAuto;
  end
  else begin
    if TransparentColor = clNone
    then begin
      LazReader.MaskMode := lrmmNone;
    end
    else begin
      LazReader.MaskMode := lrmmColor;
      LazReader.MaskColor := TColorToFPColor(TransparentColor);
    end;
  end;
end;

procedure TBitmap.InitializeWriter(AImage: TLazIntfImage; AWriter: TFPCustomImageWriter);
begin
  inherited;

  // set BPP
  // we can also look at PixelFormat, but it can be inexact
  TFPWriterBMP(AWriter).BitsPerPixel := AImage.DataDescription.Depth;
end;

