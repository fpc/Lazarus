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
unit LazJPG;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, FPImage, IntfGraphics, Graphics,
  FPReadJPEG, FPWriteJPEG,
  ClipBrd;

type
  TJPGQualityRange = TFPJPEGCompressionQuality;
  TJPGPerformance = TJPEGReadPerformance;

  TJPGImage = class(TFPImageBitmap)
  private
    FPerformance: TJPGPerformance;
    FProgressiveEncoding: boolean;
    FQuality: TJPGQualityRange;
  protected
    procedure InitFPImageReader(ImgReader: TFPCustomImageReader); override;
    procedure FinalizeFPImageReader(ImgReader: TFPCustomImageReader); override;
    procedure InitFPImageWriter(ImgWriter: TFPCustomImageWriter); override;
  public
    constructor Create; override;
    class function GetFileExtensions: string; override;
    class function GetDefaultFPReader: TFPCustomImageReaderClass; override;
    class function GetDefaultFPWriter: TFPCustomImageWriterClass; override;
  public
    property CompressionQuality: TJPGQualityRange read FQuality write FQuality;
    property ProgressiveEncoding: boolean read FProgressiveEncoding;
    property Performance: TJPGPerformance read FPerformance write FPerformance;
  end;

const
  DefaultJPGMimeType = 'image/jpeg';

procedure Register;
procedure UnRegister;

implementation

{ TJPGImage }

procedure TJPGImage.InitFPImageReader(ImgReader: TFPCustomImageReader);
var
  JPGReader: TFPReaderJPEG;
begin
  if ImgReader is TFPReaderJPEG then begin
    JPGReader:=TFPReaderJPEG(ImgReader);
    JPGReader.Performance:=Performance;
  end;
  inherited InitFPImageReader(ImgReader);
end;

procedure TJPGImage.FinalizeFPImageReader(ImgReader: TFPCustomImageReader);
var
  JPGReader: TFPReaderJPEG;
begin
  if ImgReader is TFPReaderJPEG then begin
    JPGReader:=TFPReaderJPEG(ImgReader);
    FProgressiveEncoding:=JPGReader.ProgressiveEncoding;
  end;
  inherited FinalizeFPImageReader(ImgReader);
end;

procedure TJPGImage.InitFPImageWriter(ImgWriter: TFPCustomImageWriter);
var
  JPGWriter: TFPWriterJPEG;
begin
  if ImgWriter is TFPWriterJPEG then begin
    JPGWriter:=TFPWriterJPEG(ImgWriter);
    if JPGWriter<>nil then ;
    JPGWriter.ProgressiveEncoding:=ProgressiveEncoding;
    JPGWriter.CompressionQuality:=CompressionQuality;
  end;
  inherited InitFPImageWriter(ImgWriter);
end;

class function TJPGImage.GetDefaultFPReader: TFPCustomImageReaderClass;
begin
  Result:=TFPReaderJPEG;
end;

class function TJPGImage.GetDefaultFPWriter: TFPCustomImageWriterClass;
begin
  Result:=TFPWriterJPEG;
end;

constructor TJPGImage.Create;
begin
  inherited Create;
  FPerformance:=jpBestQuality;
  FProgressiveEncoding:=false;
  FQuality:=75;
end;

class function TJPGImage.GetFileExtensions: string;
begin
  Result:='jpg;jpeg';
end;

procedure Register;
begin
  TPicture.RegisterFileFormat('jpg', 'JPEG Image File', TJPGImage);
  TPicture.RegisterFileFormat('jpeg', 'JPEG Image File', TJPGImage);
  TPicture.RegisterClipboardFormat(RegisterClipboardFormat(DefaultJPGMimeType),
    TJPGImage);
end;

procedure UnRegister;
begin
  TPicture.UnregisterGraphicClass(TJPGImage);
end;

end.

