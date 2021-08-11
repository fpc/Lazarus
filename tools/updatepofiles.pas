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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Name:
       updatepofiles - updates po files.

  Synopsis:
       updatepofiles [--searchdir=<dir>] [filenameA.rsj [filenameB.rsj ... filenameN.rsj]] filename1.pot [filename2.pot ... filenameN.pot]

  Description:
       Updatepofiles updates the .pot file from .rsj (or .rst/.lrj) files and merges new strings into
       all translated po files (filename1.*.po).

       Examples:

       1. Only update .po files:
          updatepofiles /path/to/project1.pot

       2. Update .pot file from .rsj and then .po files:
          updatepofiles /path/to/fileA.rsj /path/to/project1.pot

       3. Update .pot file from several .rsj files:
          updatepofiles /path/to/fileA.rsj /path/to/fileB.rsj /path/to/project1.pot

       4. Same as 2, but search .rsj in specified directory and its subdirectories:
          updatepofiles --searchdir=<dir> fileA.rsj /path/to/project1.pot

       Arguments can be repeated as needed, e. g.:

       updatepofiles /path/to/fileA.rsj /path/to/project1.pot /path/to/fileB.rsj /path/to/project2.pot

       will update project1.pot from fileA.rsj and project2.pot from fileB.rsj respectively.
}
program UpdatePoFiles;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Translations, LazFileUtils, LazUTF8, FileUtil;

const
  ResSearchDirParamName='--searchdir=';

var
  Files: TStringList;
  Prefix: string;
  ResFiles: array of TStringList;

procedure AddResFile(const PoIndex:Integer; const AResFile:string);
begin
  if PoIndex>(Length(ResFiles)-1) then
    SetLength(ResFiles, PoIndex+1);

  if ResFiles[PoIndex]=nil then
    ResFiles[PoIndex] := TStringList.Create;

  ResFiles[PoIndex].Add(AResFile);
end;

procedure ClearResFiles;
var
  i: Integer;
begin
  for i:=0 to Length(ResFiles)-1 do
    if ResFiles[i]<>nil then
      ResFiles[i].Free;
end;

function TryFindResFile(ResSearchDir, FileName: string): string;
var
  FileNameOnly: String;
  FL: TStringList;
  i: integer;
begin
  Result:='';
  if ResSearchDir<>'' then
  begin
    FileNameOnly:=ExtractFileName(FileName); //make sure that path to file is removed
    try
      FL:=FindAllFiles(ResSearchDir, FileNameOnly, True);
      if FL.Count>0 then
      begin
        Result:=FL[0];
        i:=1;
        while i<FL.Count do
        begin
          if FileAge(FL[i])>FileAge(Result) then
            Result:=FL[i];
          inc(i);
        end;
        writeln('Found resource string table file: "', Result, '"');
      end;
    finally
      FL.Free;
    end;
  end;
end;

function ParamsValid: boolean;
var
  i: Integer;
  ResSearchDir: String;
  CurParam: String;
  Filename: String;
  Ext: String;
  PoIndex: Integer;
  IsResFile: Boolean;
begin
  Result:=false;
  PoIndex:=0;
  ResSearchDir:='';

  if ParamCount<1 then
    exit;

  for i:=1 to ParamCount do
  begin
    CurParam:=ParamStrUTF8(i);
    if UTF8StartsText(ResSearchDirParamName, CurParam) then
    begin
      ResSearchDir:=RightStr(CurParam, Length(CurParam)-Length(ResSearchDirParamName));
      writeln('Current resource string table file search directory: "', ResSearchDir, '"');
    end
    else
    begin
      Filename:=CurParam;

      Ext:=ExtractFileExt(Filename);

      if (Ext<>'.pot') and (Ext<>'.rst') and (Ext<>'.lrj') and (Ext<>'.rsj') then
      begin
        writeln('ERROR: invalid file extension: "', Filename, '"');
        exit;
      end;

      IsResFile:=(Ext='.rst') or (Ext='.lrj') or (Ext='.rsj');

      if not FileExistsUTF8(Filename) then
      begin
        if IsResFile then
          Filename:=TryFindResFile(ResSearchDir, Filename); // if resource file does not exist, search it in specified directory

        if (Filename='') or (not IsResFile) then
        begin
          writeln('ERROR: file not found: "', CurParam, '"');
          exit;
        end;
      end;

      if Ext='.pot' then
      begin
        if Files=nil then
          Files:=TStringList.Create;
        Files.Add(Filename);
        inc(PoIndex);
        SetLength(ResFiles, Files.Count); // make sure Files and ResFiles are in sync
      end
      else
        AddResFile(PoIndex, FileName);
    end;
  end;
  Result:=PoIndex>0; // at least one .pot file should be found
end;

procedure UpdateAllPoFiles;
var
  i: Integer;
begin
  for i:=0 to Files.Count-1 do
    UpdatePoFile(ResFiles[i], Files[i]);
end;

begin
  Prefix:='';
  Files:=nil;

  if not ParamsValid then
    writeln('Usage: ',ExtractFileName(ParamStrUTF8(0))
       ,' [--searchdir=<dir>] [filenameA.rsj [filenameB.rsj ... filenameN.rsj]] filename1.pot [filename2.pot ... filenameN.pot]')
  else
    UpdateAllPoFiles;

  if Files<>nil then
    Files.Free;

  ClearResFiles;
end.

