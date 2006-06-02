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
unit LazXPM;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, FPImage, IntfGraphics, Graphics,
  FPReadXPM, FPWriteXPM,
  ClipBrd;

type
  TXPMImage = class(TFPImageBitmap)
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
  DefaultXPMMimeType = 'image/xpm';

procedure Register;
procedure UnRegister;

implementation

{ TXPMImage }

procedure TXPMImage.InitFPImageReader(ImgReader: TFPCustomImageReader);
var
  XPMReader: TFPReaderXPM;
begin
  if ImgReader is TFPReaderXPM then begin
  end;
  inherited InitFPImageReader(ImgReader);
end;

procedure TXPMImage.FinalizeFPImageReader(ImgReader: TFPCustomImageReader);
var
  XPMReader: TFPReaderXPM;
begin
  if ImgReader is TFPReaderXPM then begin
    XPMReader:=TFPReaderXPM(ImgReader);
  end;
  inherited FinalizeFPImageReader(ImgReader);
end;

procedure TXPMImage.InitFPImageWriter(ImgWriter: TFPCustomImageWriter);
var
  XPMWriter: TFPWriterXPM;
begin
  if ImgWriter is TFPWriterXPM then begin
    XPMWriter:=TFPWriterXPM(ImgWriter);
    if XPMWriter<>nil then ;
  end;
  inherited InitFPImageWriter(ImgWriter);
end;

class function TXPMImage.GetDefaultFPReader: TFPCustomImageReaderClass;
begin
  Result:=TFPReaderXPM;
end;

class function TXPMImage.GetDefaultFPWriter: TFPCustomImageWriterClass;
begin
  Result:=TFPWriterXPM;
end;

constructor TXPMImage.Create;
begin
  inherited Create;
end;

class function TXPMImage.GetFileExtensions: string;
begin
  Result:='xpm';
end;

procedure Register;
begin
  TPicture.RegisterFileFormat('xpm', 'XPM Image File', TXPMImage);
  TPicture.RegisterClipboardFormat(RegisterClipboardFormat(DefaultXPMMimeType),
    TXPMImage);
end;

procedure UnRegister;
begin
  TPicture.UnregisterGraphicClass(TXPMImage);
end;

end.
