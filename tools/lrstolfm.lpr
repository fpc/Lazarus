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
       lrstolfm - shows the lfm contents of a lrs file.

  Synopsis:
       lrstolfm resourcefilename [resourcename]

  Description:
       lrstolfm reads the given lrs file. If resource name is given as second
       parameter this resource is searched, otherwise the first entry.
}
program lrstolfm;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, LResources, FileUtil;
  
function FindResourceInLRS(const ResourceName: string; List: TStrings): integer;
const
  Pattern = 'LazarusResources.Add(''';
var
  Line: string;
  s: String;
begin
  Result:=0;
  while (Result<List.Count) do begin
    Line:=List[Result];
    if (length(Line)>length(Pattern))
    and ((strlcomp(PChar(Line),Pattern,length(Pattern)))=0) then begin
      if (ResourceName='') then
        exit;
      s:=Pattern+ResourceName+''',';
      if (strlcomp(PChar(Line),PChar(s),length(s))=0) then
        exit;
    end;
    inc(Result);
  end;
  Result:=-1;
end;

function ExtractResource(HeaderIndex: integer; LRS: TStrings): TMemoryStream;
var
  i: LongInt;
  p: Integer;
  Line: string;
  StartPos: LongInt;
  CharID: Integer;
  c: Char;
begin
  Result:=TMemoryStream.Create;
  i:=HeaderIndex+1;
  while (i<LRS.Count) do begin
    Line:=LRS[i];
    if (Line<>'') and (Line[1]=']') then exit;// found the end of this resource
    p:=1;
    while (p<=length(Line)) do begin
      case Line[p] of
      '''':
        // string constant
        begin
          inc(p);
          while p<=length(Line) do begin
            if Line[p]<>'''' then begin
              // read normal characters
              StartPos:=p;
              while (p<=length(Line)) and (Line[p]<>'''') do inc(p);
              Result.Write(Line[StartPos],p-StartPos);
            end else if (p<length(Line)) and (Line[p+1]='''') then begin
              // read '
              Result.Write(Line[p],1);
              inc(p,2);
            end else begin
              // end of string constant found
              inc(p);
              break;
            end;
          end;
        end;
      '#':
        // special character
        begin
          inc(p);
          CharID:=0;
          while (p<=length(Line)) and (Line[p] in ['0'..'9']) do begin
            CharID:=CharID*10+ord(Line[p])-ord('0');
            inc(p);
          end;
          c:=chr(CharID);
          Result.Write(c,1);
        end;
      else
        inc(p);
      end;
    end;
    inc(i);
  end;
end;

var
  LRSFilename: String;
  ResourceName: String;
  LRS: TStringList;
  ResourceHeader: LongInt;
  ObjResource: TMemoryStream;
  TextResource: TMemoryStream;
  LFMText: string;
begin
  if (ParamCount<1) or (ParamCount>2) then begin
    writeln('Usage: ',ExtractFileName(ParamStr(0))
       ,' resourcefilename [resourcename]');
    exit;
  end;
  LRSFilename:=ParamStr(1);
  ResourceName:='';
  if ParamCount>=2 then
    ResourceName:=ParamStr(2);
  LRS:=TStringList.Create;
  LRS.LoadFromFile(LRSFilename);
  
  // find resource
  ResourceHeader:=FindResourceInLRS(ResourceName,LRS);
  if ResourceHeader<0 then
    raise Exception.Create('resource not found: '+ResourceName);

  // convert lrs format to binary format
  ObjResource:=ExtractResource(ResourceHeader,LRS);

  // convert binary format to lfm format
  TextResource:=TMemoryStream.Create;
  ObjResource.Position:=0;
  LRSObjectBinaryToText(ObjResource,TextResource);

  // write to stdout
  TextResource.Position:=0;
  SetLength(LFMText,TextResource.Size);
  TextResource.Read(LFMText[1],length(LFMText));
  write(LFMText);

  TextResource.Free;
  ObjResource.Free;
  LRS.Free;
end.

