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
unit LazTGA;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, FPImage, IntfGraphics, Graphics,
  FPReadTGA, FPWriteTGA,
  ClipBrd;

type
  TTGAImage = class(TFPImageBitmap)
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
  DefaultTGAMimeType = 'image/tga';

procedure Register;
procedure UnRegister;

implementation

{ TTGAImage }

procedure TTGAImage.InitFPImageReader(ImgReader: TFPCustomImageReader);
begin
  if ImgReader is TFPReaderTarga then begin
  end;
  inherited InitFPImageReader(ImgReader);
end;

procedure TTGAImage.FinalizeFPImageReader(ImgReader: TFPCustomImageReader);
begin
  if ImgReader is TFPReaderTarga then begin
  end;
  inherited FinalizeFPImageReader(ImgReader);
end;

procedure TTGAImage.InitFPImageWriter(ImgWriter: TFPCustomImageWriter);
var
  TGAWriter: TFPWriterTarga;
begin
  if ImgWriter is TFPWriterTarga then begin
    TGAWriter:=TFPWriterTarga(ImgWriter);
    if TGAWriter<>nil then ;
  end;
  inherited InitFPImageWriter(ImgWriter);
end;

class function TTGAImage.GetDefaultFPReader: TFPCustomImageReaderClass;
begin
  Result:=TFPReaderTarga;
end;

class function TTGAImage.GetDefaultFPWriter: TFPCustomImageWriterClass;
begin
  Result:=TFPWriterTarga;
end;

constructor TTGAImage.Create;
begin
  inherited Create;
end;

class function TTGAImage.GetFileExtensions: string;
begin
  Result:='tga';
end;

procedure Register;
begin
  TPicture.RegisterFileFormat('tga', 'TGA Image File', TTGAImage);
  TPicture.RegisterClipboardFormat(RegisterClipboardFormat(DefaultTGAMimeType),
    TTGAImage);
end;

procedure UnRegister;
begin
  TPicture.UnregisterGraphicClass(TTGAImage);
end;

end.

