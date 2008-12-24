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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Name:
       updatepofiles - updates po files.

  Synopsis:
       updatepofiles filename1.po [filename2.po ... filenameN.po]

  Description:
       updatepofiles deletes doubles in the po file and merges new strings into
       all translated po files (filename1.*.po)

}
program UpdatePoFiles;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, FileUtil, Translations;
  
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
  Name: string;
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

      if (Ext='.rst') or (Ext='.lrt') then
        continue; // ignore resource files

      writeln('ERROR: file not found: ',FileName);
      exit;
    end;

    if (Ext<>'.po') and  (Ext<>'.rst') and (Ext<>'.lrt') then begin
      writeln('ERROR: invalid extension: ',Filename);
      exit;
    end;

    Name:=ExtractFileName(Filename);
    Name:=LeftStr(Name,length(Name)-length(Ext));
    if Pos('.',Name)>0 then begin
      writeln('ERROR: invalid unitname: ',Name);
      exit;
    end;

    if Ext='.po' then begin
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
       ,' filename1.po [filename2.po ... filenameN.po]')
  else
    UpdateAllPoFiles;

  if Files<>nil then
    Files.Free;

  ClearResFiles;
end.

