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
unit LazPNM;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, FPImage, IntfGraphics, Graphics,
  FPReadPNM, FPWritePNM,
  ClipBrd;

type

  TPNMImage = class(TFPImageBitmap)
  public
    constructor Create; override;
    class function GetFileExtensions: string; override;
    class function GetDefaultFPReader: TFPCustomImageReaderClass; override;
    class function GetDefaultFPWriter: TFPCustomImageWriterClass; override;
  end;

const
  DefaultPPMMimeType = 'image/ppm';
  DefaultPGMMimeType = 'image/pgm';
  DefaultPBMMimeType = 'image/pbm';

procedure Register;
procedure UnRegister;

implementation

{ TPNMImage }

class function TPNMImage.GetDefaultFPReader: TFPCustomImageReaderClass;
begin
  Result:=TFPReaderPNM;
end;

class function TPNMImage.GetDefaultFPWriter: TFPCustomImageWriterClass;
begin
  Result:=TFPWriterPNM;
end;

constructor TPNMImage.Create;
begin
  inherited Create;
end;

class function TPNMImage.GetFileExtensions: string;
begin
  Result:='ppm;pgm;pbm';
end;

procedure Register;
begin
  TPicture.RegisterFileFormat('ppm', 'PNM Image File', TPNMImage);
  TPicture.RegisterFileFormat('pgm', 'PNM Image File', TPNMImage);
  TPicture.RegisterFileFormat('pbm', 'PNM Image File', TPNMImage);
  TPicture.RegisterClipboardFormat(RegisterClipboardFormat(DefaultPPMMimeType),
    TPNMImage);
  TPicture.RegisterClipboardFormat(RegisterClipboardFormat(DefaultPGMMimeType),
    TPNMImage);
  TPicture.RegisterClipboardFormat(RegisterClipboardFormat(DefaultPBMMimeType),
    TPNMImage);
end;

procedure UnRegister;
begin
  TPicture.UnregisterGraphicClass(TPNMImage);
end;

end.

