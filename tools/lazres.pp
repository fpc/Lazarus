{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Name:
       lazres - creates an lazarus resource file from files

  Synopsis:
       lazres resourcefilename filename1 [filename2 ... filenameN]

  Description:
       lazres creates a lazarus resource file from files.

}
program LazRes;

{$mode objfpc}{$H+}

uses Classes, SysUtils, LResources;

function StreamIsFormInTextFormat(Stream: TMemoryStream): boolean;
const
  FormTextStart = 'object ';
var s: string;
  OldPos: integer;
begin
  SetLength(s,length(FormTextStart));
  OldPos:=Stream.Position;
  Stream.Read(s[1],length(s));
  Result:=AnsiCompareText(s,FormTextStart)=0;
  Stream.Position:=OldPos;
end;

function StreamIsFormInFCLFormat(Stream: TMemoryStream): boolean;
const
  FormFCLStart = 'TPF0';
var s: string;
  OldPos: integer;
begin
  SetLength(s,length(FormFCLStart));
  OldPos:=Stream.Position;
  Stream.Read(s[1],length(s));
  Result:=s=FormFCLStart;
  Stream.Position:=OldPos;
end;

procedure ConvertFormToText(Stream: TMemoryStream);
var TextStream: TMemoryStream;
begin
  try
    TextStream:=TMemoryStream.Create;
    FormDataToText(Stream,TextStream);
    TextStream.Position:=0;
    Stream.Clear;
    Stream.CopyFrom(TextStream,TextStream.Size);
    Stream.Position:=0;
  except
    on E: Exception do begin
      writeln('ERROR: unable to convert Delphi form to text: '+E.Message);
    end;
  end;
end;

var
  ResourceFilename,BinFilename,BinExt,ResourceName,ResourceType:String;
  a:integer;
  ResFileStream,BinFileStream:TFileStream;
  ResMemStream,BinMemStream:TMemoryStream;

begin
  if ParamCount<2 then begin
    writeln('Usage: ',ExtractFileName(ParamStr(0))
       ,' resourcefilename filename1 [filename2 ... filenameN]');
    exit;
  end;

  // check that all resources exists and are not the destination file
  for a:=2 to ParamCount do begin
    if not FileExists(ParamStr(a)) then begin
      writeln('ERROR: file not found: ',ParamStr(a));
      exit;
    end;
    if ExpandFileName(ParamStr(a))=ExpandFileName(ParamStr(1)) then begin
      writeln('ERROR: resourcefilename = file',a);
      exit;
    end;
  end;

  ResourceFilename:=ParamStr(1);
  try
    ResFileStream:=TFileStream.Create(ResourceFilename,fmCreate);
  except
    writeln('ERROR: unable to create file '''+ResourceFilename+'''');
    halt(1);
  end;
  ResMemStream:=TMemoryStream.Create;
  try
    for a:=2 to ParamCount do begin
      BinFilename:=ParamStr(a);
      write(BinFilename);
      try
        BinFileStream:=TFileStream.Create(BinFilename,fmOpenRead);
        BinMemStream:=TMemoryStream.Create;
        try
          BinMemStream.CopyFrom(BinFileStream,BinFileStream.Size);
          BinMemStream.Position:=0;
          BinExt:=uppercase(ExtractFileExt(BinFilename));
          if (BinExt='.LFM') or (BinExt='.DFM') or (BinExt='.XFM') then begin
            ResourceType:='FORMDATA';
            ConvertFormToText(BinMemStream);
            ResourceName:=FindLFMClassName(BinMemStream);
            if ResourceName='' then begin
              writeln(' ERROR: no resourcename');
              halt(2);
            end;
            write(' ResourceName='''+ResourceName
                  +''' Type='''+ResourceType+'''');
            LFMtoLRSstream(BinMemStream,ResMemStream);
          end else begin
            ResourceType:=copy(BinExt,2,length(BinExt)-1);
            ResourceName:=ExtractFileName(BinFilename);
            ResourceName:=copy(ResourceName,1
               ,length(ResourceName)-length(BinExt));
            if ResourceName='' then begin
              writeln(' ERROR: no resourcename');
              halt(2);
            end;
            write(
              ' ResourceName='''+ResourceName+''' Type='''+ResourceType+'''');
            BinaryToLazarusResourceCode(BinMemStream,ResMemStream
               ,ResourceName,ResourceType);
          end;
        finally
          BinFileStream.Free;
          BinMemStream.Free;
        end;
      except
        writeln('  ERROR: unable to read file '''+BinFilename+'''');
        halt(3);
      end;
      writeln('');
    end;
    ResMemStream.Position:=0;
    ResFileStream.CopyFrom(ResMemStream,ResMemStream.Size);
  finally
    ResMemStream.Free;
    ResFileStream.Free;
  end;
end.

