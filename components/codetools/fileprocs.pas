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

  Abstract:
    - simple file functions


  ToDo:
}
unit FileProcs;

{$ifdef FPC}{$mode objfpc}{$endif}{$H+}

interface

{$I codetools.inc}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils;

const
  // ToDo: find the constant in the fpc units.
  EndOfLine:shortstring={$IFDEF win32}#13+{$ENDIF}#10;

// files
function CompareFilenames(const Filename1, Filename2: string): integer;
function DirectoryExists(DirectoryName: string): boolean;
function ExtractFileNameOnly(const AFilename: string): string;
function FilenameIsAbsolute(TheFilename: string):boolean;
function ForceDirectory(DirectoryName: string): boolean;
procedure CheckIfFileIsExecutable(const AFilename: string);


implementation


// to get more detailed error messages consider the os
 {$IFDEF Linux}
uses
 {$IFDEF Ver1_0}
  Linux
 {$ELSE}
  Unix
 {$ENDIF}
 ;
 {$ENDIF}

function CompareFilenames(const Filename1, Filename2: string): integer;
begin
  {$IFDEF WIN32}
  Result:=AnsiCompareText(Filename1, Filename2);
  {$ELSE}
  Result:=AnsiCompareStr(Filename1, Filename2);
  {$ENDIF}
end;

function FileIsExecutable(const AFilename: string): boolean;
begin
  try
    CheckIfFileIsExecutable(AFilename);
    Result:=true;
  except
    Result:=false;
  end;
end;

procedure CheckIfFileIsExecutable(const AFilename: string);
var AText: string;
begin
  // TProcess does not report, if a program can not be executed
  // to get good error messages consider the OS
  if not FileExists(AFilename) then begin
    raise Exception.Create('file "'+AFilename+'" does not exist');
  end;
  {$IFDEF linux}
  if not{$IFDEF Ver1_0}Linux{$ELSE}Unix{$ENDIF}.Access(
    AFilename,{$IFDEF Ver1_0}Linux{$ELSE}Unix{$ENDIF}.X_OK) then
  begin
    AText:='"'+AFilename+'"';
    case LinuxError of
    sys_eacces: AText:='execute access denied for '+AText;
    sys_enoent: AText:='a directory component in '+AText
                          +' does not exist or is a dangling symlink';
    sys_enotdir: AText:='a directory component in '+Atext+' is not a directory';
    sys_enomem: AText:='insufficient memory';
    sys_eloop: AText:=AText+' has a circular symbolic link';
    else
      AText:=AText+' is not executable';
    end;
    raise Exception.Create(AText);
  end;
  {$ENDIF linux}

  // ToDo: windows and xxxbsd
end;

function ExtractFileNameOnly(const AFilename: string): string;
var ExtLen: integer;
begin
  Result:=ExtractFilename(AFilename);
  ExtLen:=length(ExtractFileExt(Result));
  Result:=copy(Result,1,length(Result)-ExtLen);
end;

function FilenameIsAbsolute(TheFilename: string):boolean;
begin
  Result:=(ExpandFileName(TheFilename)=TheFilename);
end;

function DirectoryExists(DirectoryName: string): boolean;
var sr: TSearchRec;
begin
  if (DirectoryName<>'')
  and (DirectoryName[length(DirectoryName)]=PathDelim) then
    DirectoryName:=copy(DirectoryName,1,length(DirectoryName)-1);
  if FindFirst(DirectoryName,faAnyFile,sr)=0 then
    Result:=((sr.Attr and faDirectory)>0)
  else
    Result:=false;
  FindClose(sr);
end;

function ForceDirectory(DirectoryName: string): boolean;
var i: integer;
  Dir: string;
begin
  DoDirSeparators(DirectoryName);
  i:=1;
  while i<=length(DirectoryName) do begin
    if DirectoryName[i]=PathDelim then begin
      Dir:=copy(DirectoryName,1,i-1);
      if not DirectoryExists(Dir) then begin
        Result:=CreateDir(Dir);
        if not Result then exit;
      end;
    end;
  end;
  Result:=true;
end;


end.


