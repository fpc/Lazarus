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

  { TSharedTGAImage }

  TSharedTGAImage = class(TSharedCustomBitmap)
  end;

  { TTGAImage }

  TTGAImage = class(TFPImageBitmap)
  protected
    class function GetReaderClass: TFPCustomImageReaderClass; override;
    class function GetWriterClass: TFPCustomImageWriterClass; override;
    class function GetSharedImageClass: TSharedRasterImageClass; override;
  public
    class function GetFileExtensions: string; override;
  end;

const
  DefaultTGAMimeType = 'image/tga';

procedure Register;
procedure UnRegister;

implementation

{ TTGAImage }

class function TTGAImage.GetReaderClass: TFPCustomImageReaderClass;
begin
  Result:=TFPReaderTarga;
end;

class function TTGAImage.GetWriterClass: TFPCustomImageWriterClass;
begin
  Result:=TFPWriterTarga;
end;

class function TTGAImage.GetSharedImageClass: TSharedRasterImageClass;
begin
  Result:=TSharedTGAImage;
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

