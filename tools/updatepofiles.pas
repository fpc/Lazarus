{  $Id$  }
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
       updatepofiles filename1.pot [filename2.pot ... filenameN.pot]

  Description:
       updatepofiles updates the .pot file and merges new strings into
       all translated po files (filename1.*.po)

}
program UpdatePoFiles;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Translations, LazFileUtils, LazUTF8;
  
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

function ParamsValid: boolean;
var
  i: Integer;
  Filename: String;
  Ext: String;
  PoIndex: Integer;
begin
  Result:=false;
  PoIndex:=0;

  if ParamCount<1 then
    exit;

  for i:=1 to ParamCount do begin

    Filename:=ParamStrUTF8(i);

    Ext:=ExtractFileExt(Filename);

    if not FileExistsUTF8(Filename) then begin

      if (Ext='.rst') or (Ext='.lrj') or (Ext='.rsj') then
        continue; // ignore resource files

      writeln('ERROR: file not found: ',FileName);
      exit;
    end;

    if (Ext<>'.pot') and  (Ext<>'.rst') and (Ext<>'.lrj')  and (Ext<>'.rsj') then begin
      writeln('ERROR: invalid extension: ',Filename);
      exit;
    end;

    if Ext='.pot' then begin
      if Files=nil then
        Files:=TStringList.Create;
      Files.Add(Filename);
      inc(PoIndex);
      SetLength(ResFiles, Files.Count); // make sure Files and ResFiles are in sync
    end else
      AddResFile(PoIndex, FileName);
  end;
  Result:=true;
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
       ,' filename1.pot [filename2.pot ... filenameN.pot]')
  else
    UpdateAllPoFiles;

  if Files<>nil then
    Files.Free;

  ClearResFiles;
end.

