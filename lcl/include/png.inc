// included by graphics.pp

{******************************************************************************
                         TPortableNetworkGraphic
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

{ TPortableNetworkGraphic }

{$IFNDEF DisableFPImage}
function TPortableNetworkGraphic.GetFPReaderForFileExt(
  const FileExtension: string): TFPCustomImageReaderClass;
begin
  Result:=TFPReaderPNG;
end;

function TPortableNetworkGraphic.GetFPWriterForFileExt(
  const FileExtension: string): TFPCustomImageWriterClass;
begin
  Result:=TFPWriterPNG;
end;
{$ENDIF}

function TPortableNetworkGraphic.LazarusResourceTypeValid(
  const ResourceType: string): boolean;
begin
  Result:=(ResourceType='PNG');
end;

procedure TPortableNetworkGraphic.ReadStream(Stream: TStream; UseSize: boolean;
  Size: Longint);
begin
{$IFNDEF DisableFPImage}
  ReadStreamWithFPImage(Stream,UseSize,Size,TFPReaderPNG);
{$ELSE}
  RaiseGDBException('TPortableNetworkGraphic.ReadStream needs FPImage');
{$ENDIF}
end;

procedure TPortableNetworkGraphic.WriteStream(Stream: TStream;
  WriteSize: Boolean);
begin
{$IFNDEF DisableFPImage}
  WriteStreamWithFPImage(Stream,WriteSize,TFPWriterPNG);
{$ELSE}
  RaiseGDBException('TPortableNetworkGraphic.WriteStream needs FPImage');
{$ENDIF}
end;

function TPortableNetworkGraphic.GetDefaultMimeType: string;
begin
  Result:='image/png';
end;


// included by graphics.pp

