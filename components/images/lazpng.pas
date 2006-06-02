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
unit LazPNG;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, FPImage, IntfGraphics, Graphics,
  FPReadPNG, FPWritePNG,ClipBrd;

type

  TPNGImage = class(TFPImageBitmap)
  public
    constructor Create; override;
    class function GetFileExtensions: string; override;
    class function GetDefaultFPReader: TFPCustomImageReaderClass; override;
    class function GetDefaultFPWriter: TFPCustomImageWriterClass; override;
  end;

const
  DefaultPNGMimeType = 'image/png';

procedure Register;
procedure UnRegister;

implementation

{ TPNGImage }

class function TPNGImage.GetDefaultFPReader: TFPCustomImageReaderClass;
begin
  Result:=TFPReaderPNG;
end;

class function TPNGImage.GetDefaultFPWriter: TFPCustomImageWriterClass;
begin
  Result:=TFPWriterPNG;
end;

constructor TPNGImage.Create;
begin
  inherited Create;
end;

class function TPNGImage.GetFileExtensions: string;
begin
  Result:='png';
end;

procedure Register;
begin
  TPicture.RegisterFileFormat('png', 'PNG Image File', TPNGImage);
  TPicture.RegisterClipboardFormat(RegisterClipboardFormat(DefaultPNGMimeType),
    TPNGImage);
end;

procedure UnRegister;
begin
  TPicture.UnregisterGraphicClass(TPNGImage);
end;

end.
