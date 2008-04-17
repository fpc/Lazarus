{%MainUnit ../graphics.pp}

{******************************************************************************
                              TFPImageBitmap
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

{ TFPImageBitmap }

class function TFPImageBitmap.GetFileExtensions: string;
begin
  Result:='';
end;

class function TFPImageBitmap.IsFileExtensionSupported(
  const FileExtension: string): boolean;
var
  Extensions: String;
  StartPos: Integer;
  EndPos: Integer;
  i: Integer;
  Ext: String;
begin
  Result:=false;
  if FileExtension='' then exit;
  Extensions:=GetFileExtensions;
  if Extensions='' then exit;
  Ext:=FileExtension;
  if Ext[1]='.'  then begin
    Ext:=copy(Ext,2,length(Ext));
    if Ext='' then exit;
  end;
  StartPos:=1;
  while StartPos<=length(Extensions) do begin
    if not (Extensions[StartPos] in [';',' ']) then begin
      EndPos:=StartPos;
      while (EndPos<=length(Extensions)) and (Extensions[EndPos]<>';') do
        inc(EndPos);
      if EndPos-StartPos=length(Ext) then begin
        i:=1;
        while (i<=length(Ext))
        and (upcase(Extensions[StartPos+i-1])=upcase(Ext[i])) do
          inc(i);
        if i>length(Ext) then begin
          Result:=true;
          exit;
        end;
      end;
      StartPos:=EndPos;
    end else
      inc(StartPos);
  end;
end;

class function TFPImageBitmap.GetFPReaderForFileExt(const FileExtension: string
  ): TFPCustomImageReaderClass;
begin
  if IsFileExtensionSupported(FileExtension) then
    Result:=GetDefaultFPReader
  else
    Result:=nil;
end;

class function TFPImageBitmap.GetFPWriterForFileExt(const FileExtension: string
  ): TFPCustomImageWriterClass;
begin
  if IsFileExtensionSupported(FileExtension) then
    Result:=GetDefaultFPWriter
  else
    Result:=nil;
end;

function TFPImageBitmap.GetBitmapNativeType: TBitmapNativeType;
begin
  Result := bnNone;
end;

class function TFPImageBitmap.GetDefaultFPReader: TFPCustomImageReaderClass;
begin
  Result:=nil;
end;

class function TFPImageBitmap.GetDefaultFPWriter: TFPCustomImageWriterClass;
begin
  Result:=nil;
end;

function TFPImageBitmap.LazarusResourceTypeValid(const ResourceType: string
  ): boolean;
begin
  Result:=IsFileExtensionSupported(ResourceType);
end;

procedure TFPImageBitmap.ReadStream(Stream: TStream; UseSize: boolean;
  Size: Longint);
begin
  ReadStreamWithFPImage(Stream,UseSize,Size,GetDefaultFPReader);
end;

procedure TFPImageBitmap.WriteStream(Stream: TStream; WriteSize: Boolean);
begin
  WriteStreamWithFPImage(Stream,WriteSize,GetDefaultFPWriter);
end;

function TFPImageBitmap.GetDefaultMimeType: string;
var
  DefaultFileExt: String;
  i: Integer;
begin
  DefaultFileExt:=GetFileExtensions;
  i:=1;
  while (i<=length(DefaultFileExt)) and (DefaultFileExt[i]<>';') do
    inc(i);
  if i<=length(DefaultFileExt) then
    DefaultFileExt:=copy(DefaultFileExt,1,i);
  Result:='image/'+DefaultFileExt;
end;


