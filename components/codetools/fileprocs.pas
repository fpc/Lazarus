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
  Classes, SysUtils, CodeToolsStrConsts;

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
function FileIsExecutable(const AFilename: string): boolean;
function FileIsReadable(const AFilename: string): boolean;
function FileIsWritable(const AFilename: string): boolean;
function FileIsText(const AFilename: string): boolean;
function TrimFilename(const AFilename: string): string;
function AppendPathDelim(const Path: string): string;
function SearchFileInPath(const Filename, BasePath, SearchPath,
                          Delimiter: string; SearchLoUpCase: boolean): string;

implementation


// to get more detailed error messages consider the os
{$IFNDEF win32}
uses
  {$IFDEF Ver1_0} Linux {$ELSE} Unix {$ENDIF};
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
    raise Exception.CreateFmt(ctsFileDoesNotExists,[AFilename]);
  end;
  {$IFNDEF win32}
  if not{$IFDEF Ver1_0}Linux{$ELSE}Unix{$ENDIF}.Access(
    AFilename,{$IFDEF Ver1_0}Linux{$ELSE}Unix{$ENDIF}.X_OK) then
  begin
    AText:='"'+AFilename+'"';
    case LinuxError of
    sys_eacces: AText:=Format(ctsExecuteAccessDeniedForFile,[AText]);
    sys_enoent: AText:=Format(ctsDirComponentDoesNotExistsOrIsDanglingSymLink,[AText]);
    sys_enotdir: AText:=Format(ctsDirComponentIsNotDir,[AText]);
    sys_enomem: AText:=ctsInsufficientMemory;
    sys_eloop: AText:=Format(ctsFileHasCircularSymLink,[AText]);
    else
      AText:=Format(ctsFileIsNotExecutable,[AText]);
    end;
    raise Exception.Create(AText);
  end;
  {$ENDIF}

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
  DoDirSeparators(TheFilename);
  {$IFDEF win32}
  // windows
  Result:=(copy(TheFilename,1,2)='\\') or ((length(TheFilename)>3) and 
     (upcase(TheFilename[1]) in ['A'..'Z']) and (copy(TheFilename,2,2)=':\'));
  {$ELSE}
  Result:=(TheFilename<>'') and (TheFilename[1]='/');
  {$ENDIF}
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

function FileIsReadable(const AFilename: string): boolean;
begin
  {$IFDEF win32}
  Result:=true;
  {$ELSE}
  Result:={$IFDEF Ver1_0}Linux{$ELSE}Unix{$ENDIF}.Access(
    AFilename,{$IFDEF Ver1_0}Linux{$ELSE}Unix{$ENDIF}.R_OK);
  {$ENDIF}
end;

function FileIsWritable(const AFilename: string): boolean;
begin
  {$IFDEF win32}
  Result:=((FileGetAttr(AFilename) and faReadOnly)>0);
  {$ELSE}
  Result:={$IFDEF Ver1_0}Linux{$ELSE}Unix{$ENDIF}.Access(
    AFilename,{$IFDEF Ver1_0}Linux{$ELSE}Unix{$ENDIF}.W_OK);
  {$ENDIF}
end;

function FileIsText(const AFilename: string): boolean;
var fs: TFileStream;
  Buf: string;
  Len, i: integer;
  NewLine: boolean;
begin
  Result:=false;
  try
    fs:=TFileStream.Create(AFilename,fmOpenRead);
    try
      // read the first 1024 bytes
      Len:=1024;
      if Len>fs.Size then Len:=fs.Size;
      if Len>0 then begin
        SetLength(Buf,Len);
        fs.Read(Buf[1],length(Buf));
        NewLine:=false;
        for i:=1 to length(Buf) do begin
          case Buf[i] of
          #0..#8,#11..#12,#14..#31: exit;
          #10,#13: NewLine:=true;
          end;
        end;
        if NewLine or (Len<1024) then
          Result:=true;
      end else
        Result:=true;
    finally
      fs.Free;
    end;
  except
  end;
end;

function TrimFilename(const AFilename: string): string;
// trim double path delims, heading and trailing spaces
// and special dirs . and ..
var SrcPos, DestPos, l, DirStart: integer;
  c: char;
begin
  Result:=AFilename;
  l:=length(AFilename);
  SrcPos:=1;
  DestPos:=1;
  
  // skip trailing spaces
  while (l>=1) and (AFilename[SrcPos]=' ') do dec(l);
  
  // skip heading spaces
  while (SrcPos<=l) and (AFilename[SrcPos]=' ') do inc(SrcPos);
  
  // trim double path delims and special dirs . and ..
  while (SrcPos<=l) do begin
    c:=AFilename[SrcPos];
    // check for double path delims
    if (c=PathDelim) then begin
      inc(SrcPos);
      {$IFDEF win32}
      if (DestPos>2)
      {$ELSE}
      if (DestPos>1)
      {$ENDIF}
      and (Result[DestPos-1]=PathDelim) then begin
        // skip second PathDelim
        continue;
      end;
      Result[DestPos]:=c;
      inc(DestPos);
      continue;
    end;
    // check for special dirs . and ..
    if (c='.') then begin
      if (SrcPos<l) then begin
        if (AFilename[SrcPos+1]=PathDelim) then begin
          // special dir ./
          // -> skip
          inc(SrcPos,2);
          continue;
        end else if (AFilename[SrcPos+1]='.')
        and (SrcPos+1=l) or (AFilename[SrcPos+2]=PathDelim) then
        begin
          // special dir ..
          //  1. ..      -> copy
          //  2. /..     -> skip .., keep /
          //  3. C:..    -> copy
          //  4. C:\..   -> skip .., keep C:\
          //  5. \\..    -> skip .., keep \\
          //  6. xxx../..   -> copy
          //  7. xxxdir/..  -> trim dir and skip ..
          if DestPos=1 then begin
            //  1. ..      -> copy
          end else if (DestPos=2) and (Result[1]=PathDelim) then begin
            //  2. /..     -> skip .., keep /
            inc(SrcPos,2);
            continue;
          {$IFDEF win32}
          end else if (DestPos=3) and (Result[2]=':')
          and (Result[1] in ['a'..'z','A'..'Z']) then begin
            //  3. C:..    -> copy
          end else if (DestPos=4) and (Result[2]=':') and (Result[3]=PathDelim)
          and (Result[1] in ['a'..'z','A'..'Z']) then begin
            //  4. C:\..   -> skip .., keep C:\
            inc(SrcPos,2);
            continue;
          end else if (DestPos=3) and (Result[1]=PathDelim)
          and (Result[2]=PathDelim) then
            //  5. \\..    -> skip .., keep \\
            inc(SrcPos,2);
            continue;
          {$ENDIF}
          end else begin
            DirStart:=DestPos;
            while (DirStart>1) and (Result[DirStart-1]<>PathDelim) do
              dec(DirStart);
            if (DestPos-DirStart=2) and (Result[DirStart]='.')
            and (Result[DirStart+1]='.') then begin
              //  6. xxx../..   -> copy
            end else begin
              //  7. xxxdir/..  -> trim dir and skip ..
              DestPos:=DirStart;
              inc(SrcPos,2);
              continue;
            end;
          end;
        end;
      end else begin
        // special dir . at end of filename
        if DestPos=1 then begin
          Result:='.';
          exit;
        end else begin
          // skip
          break;
        end;
      end;
    end;
    // copy directory
    repeat
      Result[DestPos]:=c;
      inc(DestPos);
      inc(SrcPos);
      if (SrcPos>l) then break;
      c:=AFilename[SrcPos];
      if c=PathDelim then break;
    until false;
  end;
  // trim result
  if DestPos<=length(AFilename) then
    SetLength(Result,DestPos-1);
end;

function AppendPathDelim(const Path: string): string;
begin
  if (Path<>'') and (Path[length(Path)]<>PathDelim) then
    Result:=Path+PathDelim
  else
    Result:=Path;
end;

function SearchFileInPath(const Filename, BasePath, SearchPath,
  Delimiter: string; SearchLoUpCase: boolean): string;
  
  function FileDoesExists(const AFilename: string): boolean;
  begin
    Result:=FileExists(AFilename);
    if Result then begin
      SearchFileInPath:=ExpandFilename(AFilename);
      exit;
    end;
    {$IFNDEF Win32}
    if SearchLoUpCase then begin

    end;
    {$ENDIF}
  end;
  
var
  p, StartPos, l: integer;
  CurPath, Base: string;
begin
//writeln('[SearchFileInPath] Filename="',Filename,'" BasePath="',BasePath,'" SearchPath="',SearchPath,'" Delimiter="',Delimiter,'"');
  if (Filename='') then begin
    Result:=Filename;
    exit;
  end;
  // check if filename absolute
  if FilenameIsAbsolute(Filename) then begin
    if FileDoesExists(Filename) then exit;
    Result:='';
    exit;
  end;
  Base:=ExpandFilename(AppendPathDelim(BasePath));
  // search in current directory
  if FileDoesExists(Base+Filename) then exit;
  // search in search path
  StartPos:=1;
  l:=length(SearchPath);
  while StartPos<=l do begin
    p:=StartPos;
    while (p<=l) and (pos(SearchPath[p],Delimiter)<1) do inc(p);
    CurPath:=Trim(copy(SearchPath,StartPos,p-StartPos));
    if CurPath<>'' then begin
      if not FilenameIsAbsolute(CurPath) then
        CurPath:=Base+CurPath;
      Result:=ExpandFilename(AppendPathDelim(CurPath)+Filename);
      if FileDoesExists(Result) then exit;
    end;
    StartPos:=p+1;
  end;
  Result:='';
end;



end.


