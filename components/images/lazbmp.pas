{ Copyright (C) 2003 Mattias Gaertner

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit LazBMP;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, FPImage, IntfGraphics, Graphics,
  FPReadBMP, FPWriteBMP,
  ClipBrd;

type

  TBMPImage = class(TFPImageBitmap)
  protected
    procedure InitFPImageReader(ImgReader: TFPCustomImageReader); override;
    procedure FinalizeFPImageReader(ImgReader: TFPCustomImageReader); override;
    procedure InitFPImageWriter(ImgWriter: TFPCustomImageWriter); override;
  public
    constructor Create; override;
    class function GetFileExtensions: string; override;
    class function GetDefaultFPReader: TFPCustomImageReaderClass; override;
    class function GetDefaultFPWriter: TFPCustomImageWriterClass; override;
  end;

const
  DefaultBMPMimeType = 'image/bmp';

procedure Register;
procedure UnRegister;

implementation

{ TBMPImage }

procedure TBMPImage.InitFPImageReader(ImgReader: TFPCustomImageReader);
begin
  if ImgReader is TFPReaderBMP then begin
  end;
  inherited InitFPImageReader(ImgReader);
end;

procedure TBMPImage.FinalizeFPImageReader(ImgReader: TFPCustomImageReader);
begin
  if ImgReader is TFPReaderBMP then begin
  end;
  inherited FinalizeFPImageReader(ImgReader);
end;

procedure TBMPImage.InitFPImageWriter(ImgWriter: TFPCustomImageWriter);
var
  BMPWriter: TFPWriterBMP;
begin
  if ImgWriter is TFPWriterBMP then begin
    BMPWriter:=TFPWriterBMP(ImgWriter);
    if BMPWriter<>nil then ;
  end;
  inherited InitFPImageWriter(ImgWriter);
end;

class function TBMPImage.GetDefaultFPReader: TFPCustomImageReaderClass;
begin
  Result:=TFPReaderBMP;
end;

class function TBMPImage.GetDefaultFPWriter: TFPCustomImageWriterClass;
begin
  Result:=TFPWriterBMP;
end;

constructor TBMPImage.Create;
begin
  inherited Create;
end;

class function TBMPImage.GetFileExtensions: string;
begin
  Result:='bmp';
end;

procedure Register;
begin
  TPicture.RegisterFileFormat('bmp', 'BMP Image File', TBMPImage);
  TPicture.RegisterClipboardFormat(RegisterClipboardFormat(DefaultBMPMimeType),
    TBMPImage);
end;

procedure UnRegister;
begin
  TPicture.UnregisterGraphicClass(TBMPImage);
end;

end.
