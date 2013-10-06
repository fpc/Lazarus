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
  
procedure FindResourceInLRS(List: TStrings; var ResourceName: string; var Index: Integer; out ResType: String);
const
  Pattern = 'LazarusResources.Add(''';
var
  Line,
  ResName: String;
begin
  while (Index < List.Count) do
  begin
    Line := List[Index];
    if (Length(Line) > Length(Pattern)) and
       (Pos(Pattern, Line) = 1) then
    begin
      Delete(Line, 1, Length(Pattern));
      ResName := Copy(Line, 1, Pos(''',''', Line) - 1);
      if (ResourceName <> '') and (ResName <> ResourceName) then
        Continue;
      ResourceName := ResName;
      Delete(Line, 1, Length(ResName) + 3);
      ResType := Copy(Line, 1, Pos(''',[', Line) - 1);
      Exit;
    end;
    Inc(Index);
  end;
  Index := -1;
end;

function ExtractResource(LRS: TStrings; var Index: integer): TMemoryStream;
var
  p: Integer;
  Line: string;
  StartPos: LongInt;
  CharID: Integer;
  c: Char;
begin
  Result:=TMemoryStream.Create;
  inc(Index);
  while (Index < LRS.Count) do
  begin
    Line := LRS[Index];
    if (Line<>'') and (Line[1]=']') then exit;// found the end of this resource
    p := 1;
    while (p <= length(Line)) do
    begin
      case Line[p] of
      '''':
        // string constant
        begin
          inc(p);
          while p<=length(Line) do
          begin
            if Line[p]<>'''' then
            begin
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
    inc(Index);
  end;
end;

var
  LRSFilename, ResText,
  ResourceName, ResourceType: String;
  ResourceHeader: LongInt;
  LRS: TStringList;
  ObjResource, TextResource: TMemoryStream;
  FileStream: TFileStream;
begin
  if (ParamCount < 1) or (ParamCount > 2) then
  begin
    WriteLn('Usage: ', ExtractFileName(ParamStr(0)), ' resourcefilename [resourcename]');
    Exit;
  end;
  LRSFilename := ParamStr(1);
  ResourceName := '';
  if ParamCount >= 2 then
    ResourceName := ParamStr(2);
  LRS := TStringList.Create;
  LRS.LoadFromFile(LRSFilename);
  ResourceHeader := 0;

  if ResourceName = '@' then
  begin
    while True do
    begin
      // find resource
      ResourceName := '';
      FindResourceInLRS(LRS, ResourceName, ResourceHeader, ResourceType);
      if ResourceHeader < 0 then
        break;
      ObjResource := ExtractResource(LRS, ResourceHeader);
      ObjResource.Position := 0;
      FileStream := TFileStream.Create(ResourceName + '.' + ResourceType, fmCreate);
      try
        FileStream.CopyFrom(ObjResource, ObjResource.Size);
      finally
        FileStream.Free;
      end;
      ObjResource.Free;
    end;
  end
  else
  begin
    // find resource
    FindResourceInLRS(LRS, ResourceName, ResourceHeader, ResourceType);
    if ResourceHeader < 0 then
      raise Exception.Create('Resource not found: ' + ResourceName);

    // convert lrs format to binary format
    ObjResource := ExtractResource(LRS, ResourceHeader);
    ObjResource.Position := 0;

    // convert binary format to lfm format
    TextResource := TMemoryStream.Create;
    LRSObjectBinaryToText(ObjResource, TextResource);

    // write to stdout
    TextResource.Position := 0;
    SetLength(ResText, TextResource.Size);
    TextResource.Read(ResText[1], Length(ResText));
    Write(ResText);

    TextResource.Free;
    ObjResource.Free;
  end;
  LRS.Free;
end.

