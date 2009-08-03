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
       lazres resourcefilename @filelist

  Description:
       lazres creates a lazarus resource file from files.

}
program LazRes;

{$mode objfpc}{$H+}

uses Classes, SysUtils, FileUtil, LCLProc, LResources;

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
      debugln('ERROR: unable to convert Delphi form to text: '+E.Message);
    end;
  end;
end;

var
  ResourceFilename,FullResourceFilename,BinFilename,BinExt,ResourceName,ResourceType:String;
  a:integer;
  ResFileStream,BinFileStream:TFileStream;
  ResMemStream,BinMemStream:TMemoryStream;
  FileList:TStringList;
  S: String;
begin
  if ParamCount<2 then begin
    debugln('Usage: ',ExtractFileName(ParamStrUTF8(0))
       ,' resourcefilename filename1 [filename2 ... filenameN]');
    debugln('       ',ExtractFileName(ParamStrUTF8(0))
       ,' resourcefilename @filelist');
    exit;
  end;
  FileList:=TStringList.Create;
  try
    if ParamStrUTF8(2)[1] = '@' then
    begin
      S := ParamStrUTF8(2);
      Delete(S, 1, 1);
      S := ExpandFileNameUTF8(S);
      if not FileExistsUTF8(S) then 
      begin
        debugln('ERROR: file list not found: ',S);
        exit;
      end;
      FileList.LoadFromFile(UTF8ToSys(S));
      for a:=FileList.Count-1 downto 0 do
        if FileList[a]='' then
          FileList.Delete(a);
    end
    else for a:=2 to ParamCount do FileList.Add(ParamStrUTF8(a));
    
    ResourceFilename := ParamStrUTF8(1);
    FullResourceFilename := ExpandFileNameUTF8(ResourceFilename);
    // check that all resources exists and are not the destination file
    for a:=0 to FileList.Count-1 do begin
      S := FileList[a]; 
      if not FileExistsUTF8(S) 
      then begin
        debugln('ERROR: file not found: ', S);
        exit;
      end;
      if ExpandFileNameUTF8(S) = FullResourceFilename
      then begin
        debugln(['ERROR: resourcefilename = file', a]);
        exit;
      end;
    end;
  
    try
      ResFileStream:=TFileStream.Create(UTF8ToSys(ResourceFilename),fmCreate);
    except
      debugln('ERROR: unable to create file ''', ResourceFilename, '''');
      halt(1);
    end;
    ResMemStream:=TMemoryStream.Create;
    try
      for a:=0 to FileList.Count-1 do begin
        BinFilename:=FileList[a];
        dbgout(BinFilename);
        try
          BinFileStream:=TFileStream.Create(UTF8ToSys(BinFilename),fmOpenRead);
          BinMemStream:=TMemoryStream.Create;
          try
            BinMemStream.CopyFrom(BinFileStream,BinFileStream.Size);
            BinMemStream.Position:=0;
            BinExt:=uppercase(ExtractFileExt(BinFilename));
            if (BinExt='.LFM') or (BinExt='.DFM') or (BinExt='.XFM') 
            then begin
              ResourceType:='FORMDATA';
              ConvertFormToText(BinMemStream);
              ResourceName:=FindLFMClassName(BinMemStream);
              if ResourceName='' then begin
                debugln(' ERROR: no resourcename');
                halt(2);
              end;
              dbgout(' ResourceName=''', ResourceName, ''' Type=''', ResourceType, '''');
              LFMtoLRSstream(BinMemStream,ResMemStream);
            end 
            else begin
              ResourceType:=copy(BinExt,2,length(BinExt)-1);
              ResourceName:=ExtractFileName(BinFilename);
              ResourceName:=copy(ResourceName,1
                 ,length(ResourceName)-length(BinExt));
              if ResourceName='' then begin
                debugln(' ERROR: no resourcename');
                halt(2);
              end;
              dbgout(' ResourceName=''', ResourceName, ''' Type=''', ResourceType+'''');
              BinaryToLazarusResourceCode(BinMemStream,ResMemStream
                 ,ResourceName,ResourceType);
            end;
          finally
            BinFileStream.Free;
            BinMemStream.Free;
          end;
        except
          debugln('  ERROR: unable to read file ''', BinFilename, '''');
          halt(3);
        end;
        debugln('');
      end;
      ResMemStream.Position:=0;
      ResFileStream.CopyFrom(ResMemStream,ResMemStream.Size);
    finally
      ResMemStream.Free;
      ResFileStream.Free;
    end;
  finally   
    FileList.Free;
  end;
end.

