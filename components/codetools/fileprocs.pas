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
    - simple file functions and fpc additions
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
  
type
  TFPCStreamSeekType = int64;
  TFPCMemStreamSeekType = integer;


const
  SpecialChar = '#'; // used to use PathDelim, e.g. #\
  {$IFDEF win32}
  FileMask = '*.*';
  {$ELSE}
  FileMask = '*';
  {$ENDIF}
  {$ifdef win32}
  {$define CaseInsensitiveFilenames}
  {$endif}

function CompareFilenames(const Filename1, Filename2: string): integer;
function CompareFileExt(const Filename, Ext: string;
  CaseSensitive: boolean): integer;
function GetFilenameOnDisk(const AFilename: string): string;
function DirPathExists(DirectoryName: string): boolean;
function ExtractFileNameOnly(const AFilename: string): string;
function FilenameIsAbsolute(TheFilename: string):boolean;
function ForceDirectory(DirectoryName: string): boolean;
procedure CheckIfFileIsExecutable(const AFilename: string);
function FileIsExecutable(const AFilename: string): boolean;
function FileIsReadable(const AFilename: string): boolean;
function FileIsWritable(const AFilename: string): boolean;
function FileIsText(const AFilename: string): boolean;
function TrimFilename(const AFilename: string): string;
function CleanAndExpandFilename(const Filename: string): string;
function CleanAndExpandDirectory(const Filename: string): string;
function FileIsInPath(const Filename, Path: string): boolean;
function AppendPathDelim(const Path: string): string;
function ChompPathDelim(const Path: string): string;
function SearchFileInPath(const Filename, BasePath, SearchPath,
                          Delimiter: string; SearchLoUpCase: boolean): string;
function FilenameIsMatching(const Mask, Filename: string;
  MatchExactly: boolean): boolean;
function ClearFile(const Filename: string; RaiseOnError: boolean): boolean;


// debugging
procedure DebugLn;
procedure DebugLn(const s: string);
procedure DebugLn(const s1,s2: string);
procedure DebugLn(const s1,s2,s3: string);
procedure DebugLn(const s1,s2,s3,s4: string);
procedure DebugLn(const s1,s2,s3,s4,s5: string);
procedure DebugLn(const s1,s2,s3,s4,s5,s6: string);
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7: string);
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8: string);
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9: string);
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10: string);
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11: string);
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12: string);

procedure DbgOut(const s: string);
procedure DbgOut(const s1,s2: string);

function DbgS(const c: cardinal): string;
function DbgS(const i: integer): string;
function DbgS(const r: TRect): string;
function DbgS(const p: TPoint): string;
function DbgS(const p: pointer): string;
function DbgS(const e: extended): string;
function DbgS(const b: boolean): string;

function DbgS(const i1,i2,i3,i4: integer): string;


implementation

// to get more detailed error messages consider the os
{$IFNDEF win32}
uses
  {$IFDEF Ver1_0} Linux {$ELSE} Unix,BaseUnix {$ENDIF};
{$ENDIF}

var
  UpChars: array[char] of char;

{-------------------------------------------------------------------------------
  function ClearFile(const Filename: string; RaiseOnError: boolean): boolean;
-------------------------------------------------------------------------------}
function ClearFile(const Filename: string; RaiseOnError: boolean): boolean;
var
  fs: TFileStream;
begin
  if FileExists(Filename) then begin
    try
      fs:=TFileStream.Create(Filename,fmOpenWrite);
      fs.Size:=0;
      fs.Free;
    except
      on E: Exception do begin
        Result:=false;
        if RaiseOnError then raise;
        exit;
      end;
    end;
  end;
  Result:=true;
end;

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
  {$IFDEF win32}
  Result:=true;
  {$ELSE}
  {$IFDEF Ver1_0}
  Result:= Linux.Access(AFilename,Linux.X_OK);
  {$ELSE}
  Result:= BaseUnix.FpAccess(AFilename,BaseUnix.X_OK)=0;
  {$ENDIF}
  {$ENDIF}
end;

procedure CheckIfFileIsExecutable(const AFilename: string);
{$IFNDEF win32}
var AText: string;
{$ENDIF}
begin
  // TProcess does not report, if a program can not be executed
  // to get good error messages consider the OS
  if not FileExists(AFilename) then begin
    raise Exception.CreateFmt(ctsFileDoesNotExists,[AFilename]);
  end;
  {$IFNDEF win32}
  if not{$IFDEF Ver1_0}Linux.Access{$ELSE}(BaseUnix.FpAccess{$ENDIF}(
    AFilename,{$IFDEF Ver1_0}Linux{$ELSE}BaseUnix{$ENDIF}.X_OK){$IFNDEF Ver1_0}=0){$ENDIF} then
  begin
    AText:='"'+AFilename+'"';
    case {$ifdef ver1_0} LinuxError {$else} fpGetErrno {$endif} of
    {$IFDEF Ver1_0}sys_eacces{$ELSE}ESysEAcces{$ENDIF}:
      AText:='read access denied for '+AText;
    {$IFDEF Ver1_0}sys_enoent{$ELSE}ESysENoEnt{$ENDIF}:
      AText:='a directory component in '+AText
                          +' does not exist or is a dangling symlink';
    {$IFDEF Ver1_0}sys_enotdir{$ELSE}ESysENotDir{$ENDIF}:
      AText:='a directory component in '+Atext+' is not a directory';
    {$IFDEF Ver1_0}sys_enomem{$ELSE}ESysENoMem{$ENDIF}:
      AText:='insufficient memory';
    {$IFDEF Ver1_0}sys_eloop{$ELSE}ESysELoop{$ENDIF}:
      AText:=AText+' has a circular symbolic link';
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
  Result:=((length(TheFilename)>=2) and (TheFilename[1] in ['A'..'Z','a'..'z'])
           and (TheFilename[2]=':'))
     or ((length(TheFilename)>=2)
         and (TheFilename[1]='\') and (TheFilename[2]='\'));
  {$ELSE}
  Result:=(TheFilename<>'') and (TheFilename[1]='/');
  {$ENDIF}
end;

function GetFilenameOnDisk(const AFilename: string): string;
begin
  Result:=AFilename;
end;

function DirPathExists(DirectoryName: string): boolean;
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
      if not DirPathExists(Dir) then begin
        Result:=CreateDir(Dir);
        if not Result then exit;
      end;
    end;
    inc(i);
  end;
  Result:=true;
end;

function FileIsReadable(const AFilename: string): boolean;
begin
  {$IFDEF win32}
  Result:=true;
  {$ELSE}
  {$IFDEF Ver1_0}
  Result:= Linux.Access(AFilename,Linux.R_OK);
  {$ELSE}
  Result:= BaseUnix.FpAccess(AFilename,BaseUnix.R_OK)=0;
  {$ENDIF}
  {$ENDIF}
end;

function FileIsWritable(const AFilename: string): boolean;
begin
  {$IFDEF win32}
  Result:=((FileGetAttr(AFilename) and faReadOnly)=0);
  {$ELSE}
  {$IFDEF Ver1_0}
  Result:= Linux.Access(AFilename,Linux.W_OK);
  {$ELSE}
  Result:= BaseUnix.FpAccess(AFilename,BaseUnix.W_OK)=0;
  {$ENDIF}
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
      if Len>fs.Size then Len:=integer(fs.Size);
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

  function FilenameIsTrimmed(const TheFilename: string): boolean;
  var
    l: Integer;
    i: Integer;
  begin
    Result:=false;
    if TheFilename='' then begin
      Result:=true;
      exit;
    end;
    l:=length(TheFilename);
    // check heading spaces
    if TheFilename[1]=' ' then exit;
    // check trailing spaces
    if TheFilename[l]=' ' then exit;
    i:=1;
    while i<=l do begin
      case TheFilename[i] of

      PathDelim:
        // check for double path delimiter
        if (i<l) and (TheFilename[i+1]=PathDelim) then exit;

      '.':
        if (i=1) or (TheFilename[i-1]=PathDelim) then begin
          // check for . and .. directories
          if (i=l) or (TheFilename[i+1]=PathDelim) then exit;
          if (TheFilename[i+1]='.')
          and ((i=l-1) or (TheFilename[i+2]=PathDelim)) then exit;
        end;

      end;
      inc(i);
    end;
    Result:=true;
  end;

var SrcPos, DestPos, l, DirStart: integer;
  c: char;
begin
  Result:=AFilename;
  if FilenameIsTrimmed(Result) then exit;

  l:=length(AFilename);
  SrcPos:=1;
  DestPos:=1;

  // skip trailing spaces
  while (l>=1) and (AFilename[l]=' ') do dec(l);

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
        if (AFilename[SrcPos+1]=PathDelim)
        and ((DestPos=1) or (AFilename[SrcPos-1]=PathDelim)) then begin
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
          and (Result[2]=PathDelim) then begin
            //  5. \\..    -> skip .., keep \\
            inc(SrcPos,2);
            continue;
          {$ENDIF}
          end else if (DestPos>1) and (Result[DestPos-1]=PathDelim) then begin
            if (DestPos>3)
            and (Result[DestPos-2]='.') and (Result[DestPos-3]='.')
            and ((DestPos=4) or (Result[DestPos-4]=PathDelim)) then begin
              //  6. ../..   -> copy
            end else begin
              //  7. xxxdir/..  -> trim dir and skip ..
              DirStart:=DestPos-2;
              while (DirStart>1) and (Result[DirStart-1]<>PathDelim) do
                dec(DirStart);
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

{------------------------------------------------------------------------------
  function CleanAndExpandFilename(const Filename: string): string;
 ------------------------------------------------------------------------------}
function CleanAndExpandFilename(const Filename: string): string;
begin
  Result:=ExpandFilename(TrimFileName(Filename));
end;

{------------------------------------------------------------------------------
  function CleanAndExpandDirectory(const Filename: string): string;
 ------------------------------------------------------------------------------}
function CleanAndExpandDirectory(const Filename: string): string;
begin
  Result:=AppendPathDelim(CleanAndExpandFilename(Filename));
end;

{------------------------------------------------------------------------------
  function FileIsInPath(const Filename, Path: string): boolean;
 ------------------------------------------------------------------------------}
function FileIsInPath(const Filename, Path: string): boolean;
var
  ExpFile: String;
  ExpPath: String;
  l: integer;
begin
  if Path='' then begin
    Result:=false;
    exit;
  end;
  ExpFile:=TrimFilename(Filename);
  ExpPath:=AppendPathDelim(TrimFilename(Path));
  l:=length(ExpPath);
  Result:=(l>0) and (length(ExpFile)>l) and (ExpFile[l]=PathDelim)
          and (CompareFilenames(ExpPath,LeftStr(ExpFile,l))=0);
end;

function AppendPathDelim(const Path: string): string;
begin
  if (Path<>'') and (Path[length(Path)]<>PathDelim) then
    Result:=Path+PathDelim
  else
    Result:=Path;
end;

function ChompPathDelim(const Path: string): string;
begin
  if (Path<>'') and (Path[length(Path)]=PathDelim) then
    Result:=LeftStr(Path,length(Path)-1)
  else
    Result:=Path;
end;

function SearchFileInPath(const Filename, BasePath, SearchPath,
  Delimiter: string; SearchLoUpCase: boolean): string;
var
  p, StartPos, l: integer;
  CurPath, Base: string;
begin
//debugln('[SearchFileInPath] Filename="',Filename,'" BasePath="',BasePath,'" SearchPath="',SearchPath,'" Delimiter="',Delimiter,'"');
  if (Filename='') then begin
    Result:=Filename;
    exit;
  end;
  // check if filename absolute
  if FilenameIsAbsolute(Filename) then begin
    if FileExists(Filename) then begin
      Result:=ExpandFilename(Filename);
      exit;
    end else begin
      Result:='';
      exit;
    end;
  end;
  Base:=ExpandFilename(AppendPathDelim(BasePath));
  // search in current directory
  if FileExists(Base+Filename) then begin
    Result:=Base+Filename;
    exit;
  end;
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
      if FileExists(Result) then exit;
    end;
    StartPos:=p+1;
  end;
  Result:='';
end;


function FilenameIsMatching(const Mask, Filename: string;
  MatchExactly: boolean): boolean;
(*
  check if Filename matches Mask
  if MatchExactly then the complete Filename must match, else only the
  start

  Filename matches exactly or is a file/directory in a subdirectory of mask
  Mask can contain the wildcards * and ? and the set operator {,}
  The wildcards will _not_ match PathDelim
  If you need the asterisk, the question mark or the PathDelim as character
  just put the SpecialChar character in front of it.

  Examples:
    /abc           matches /abc, /abc/p, /abc/xyz/filename
                   but not /abcd
    /abc/x?z/www   matches /abc/xyz/www, /abc/xaz/www
                   but not /abc/x/z/www
    /abc/x*z/www   matches /abc/xz/www, /abc/xyz/www, /abc/xAAAz/www
                   but not /abc/x/z/www
    /abc/x\*z/www  matches /abc/x*z/www, /abc/x*z/www/ttt

    /a{b,c,d}e     matches /abe, /ace, /ade
*)

  function FindDirectoryStart(const AFilename: string;
    CurPos: integer): integer;
  begin
    Result:=CurPos;
    while (Result<=length(AFilename))
    and (AFilename[Result]=PathDelim) do
      inc(Result);
  end;

  function FindDirectoryEnd(const AFilename: string; CurPos: integer): integer;
  begin
    Result:=CurPos;
    while (Result<=length(AFilename)) do begin
      if AFilename[Result]=SpecialChar then
        inc(Result,2)
      else if (AFilename[Result]=PathDelim) then
        break
      else
        inc(Result);
    end;
  end;

  function CharsEqual(c1, c2: char): boolean;
  begin
    {$ifdef CaseInsensitiveFilenames}
    Result:=(UpChars[c1]=UpChars[c2]);
    {$else}
    Result:=(c1=c2);
    {$endif}
  end;

var
  DirStartMask, DirEndMask,
  DirStartFile, DirEndFile,
  AsteriskPos,
  BracketMaskPos, BracketFilePos: integer;
begin
  //debugln('[FilenameIsMatching] Mask="',Mask,'" Filename="',Filename,'" MatchExactly=',MatchExactly);
  Result:=false;
  if (Filename='') then exit;
  if (Mask='') then begin
    Result:=true;  exit;
  end;
  // test every directory
  DirStartMask:=1;
  DirStartFile:=1;
  repeat
    // find start of directories
    DirStartMask:=FindDirectoryStart(Mask,DirStartMask);
    DirStartFile:=FindDirectoryStart(Filename,DirStartFile);
    // find ends of directories
    DirEndMask:=FindDirectoryEnd(Mask,DirStartMask);
    DirEndFile:=FindDirectoryEnd(Filename,DirStartFile);
    // debugln('  Compare "',copy(Mask,DirStartMask,DirEndMask-DirStartMask),'"',
    //   ' "',copy(Filename,DirStartFile,DirEndFile-DirStartFile),'"');
    // compare directories
    AsteriskPos:=0;
    BracketMaskPos:=0;
    while (DirStartMask<DirEndMask) and (DirStartFile<DirEndFile) do begin
//debugl('AAA1 ',DirStartMask,' ',Mask[DirStartMask],' - ',DirStartFile,' ',Filename[DirStartFile]);
      case Mask[DirStartMask] of
      '?':
        begin
          inc(DirStartMask);
          inc(DirStartFile);
          continue;
        end;
      '*':
        begin
          inc(DirStartMask);
          AsteriskPos:=DirStartMask;
          continue;
        end;
      '{':
        if BracketMaskPos<1 then begin
          inc(DirStartMask);
          BracketMaskPos:=DirStartMask;
          BracketFilePos:=DirStartFile;
          continue;
        end;
      ',':
        if BracketMaskPos>0 then begin
          // Bracket operator fits complete
          // -> skip rest of Bracket operator
          repeat
            inc(DirStartMask);
            if DirStartMask>=DirEndMask then exit; // error, missing }
            if Mask[DirStartMask]=SpecialChar then begin
              // special char -> next char is normal char
              inc(DirStartMask);
            end else if Mask[DirStartMask]='}' then begin
              // bracket found (= end of Or operator)
              inc(DirStartMask);
              break;
            end;
          until false;
          BracketMaskPos:=0;
          continue;
        end;
      '}':
        begin
          if BracketMaskPos>0 then begin
            // Bracket operator fits complete
            inc(DirStartMask);
            BracketMaskPos:=0;
            continue;
          end;
        end;
      end;
      if Mask[DirStartMask]=SpecialChar then begin
        // special char -> next char is normal char
        inc(DirStartMask);
        if (DirStartMask>=DirEndMask) then exit;
      end;
      // compare char
      if CharsEqual(Mask[DirStartMask],Filename[DirStartFile]) then begin
        inc(DirStartMask);
        inc(DirStartFile);
      end else begin
        // chars different
        if BracketMaskPos>0 then begin
          // try next Or
          repeat
            inc(DirStartMask);
            if DirStartMask>=DirEndMask then exit; // error, missing }
            if Mask[DirStartMask]=SpecialChar then begin
              // special char -> next char is normal char
              inc(DirStartMask);
            end else if Mask[DirStartMask]='}' then begin
              // bracket found (= end of Or operator)
              // -> filename does not match
              exit;
            end else if Mask[DirStartMask]=',' then begin
              // next Or found
              // -> reset filename position and compare
              inc(DirStartMask);
              DirStartFile:=BracketFilePos;
              break;
            end;
          until false;
        end else if AsteriskPos>0 then begin
          // * operator always fits
          inc(DirStartFile);
        end else begin
          // filename does not match
          exit;
        end;
      end;
    end;
    if BracketMaskPos>0 then exit;
    if (DirStartMask<DirEndmask) or (DirStartFile<DirEndFile) then exit;
    // find starts of next directories
    DirStartMask:=DirEndMask+1;
    DirStartFile:=DirEndFile+1;
  until (DirStartFile>length(Filename)) or (DirStartMask>length(Mask));

  DirStartMask:=FindDirectoryStart(Mask,DirStartMask);

  // check that complete mask matches
  Result:=(DirStartMask>length(Mask));

  if MatchExactly then begin
    DirStartFile:=FindDirectoryStart(Filename,DirStartFile);
    // check that the complete Filename matches
    Result:=(Result and (DirStartFile>length(Filename)));
  end;
  //debugl('  [FilenameIsMatching] Result=',Result,' ',DirStartMask,',',length(Mask),'  ',DirStartFile,',',length(Filename));
end;

function CompareFileExt(const Filename, Ext: string;
  CaseSensitive: boolean): integer;
var
  FileLen, FilePos, ExtLen, ExtPos: integer;
  FileChar, ExtChar: char;
begin
  FileLen:=length(Filename);
  ExtLen:=length(Ext);
  FilePos:=FileLen;
  while (FilePos>=1) and (Filename[FilePos]<>'.') do dec(FilePos);
  if FilePos<1 then begin
    // no extension in filename
    Result:=1;
    exit;
  end;
  // skip point
  inc(FilePos);
  ExtPos:=1;
  if (ExtPos<=ExtLen) and (Ext[1]='.') then inc(ExtPos);
  // compare extensions
  while true do begin
    if FilePos<=FileLen then begin
      if ExtPos<=ExtLen then begin
        FileChar:=Filename[FilePos];
        ExtChar:=Ext[ExtPos];
        if not CaseSensitive then begin
          FileChar:=UpChars[FileChar];
          ExtChar:=UpChars[ExtChar];
        end;
        if FileChar=ExtChar then begin
          inc(FilePos);
          inc(ExtPos);
        end else if FileChar>ExtChar then begin
          Result:=1;
          exit;
        end else begin
          Result:=-1;
          exit;
        end;
      end else begin
        // fileext longer than ext
        Result:=1;
        exit;
      end;
    end else begin
      if ExtPos<=ExtLen then begin
        // fileext shorter than ext
        Result:=-1;
        exit;
      end else begin
        // equal
        Result:=0;
        exit;
      end;
    end;
  end;
end;

procedure DebugLn;
begin
  DebugLn('');
end;

procedure DebugLn(const s: string);
begin
  if TextRec(Output).Mode<>fmClosed then
    writeln(s);
end;

procedure DebugLn(const s1, s2: string);
begin
  DebugLn(s1+s2);
end;

procedure DebugLn(const s1, s2, s3: string);
begin
  DebugLn(s1+s2+s3);
end;

procedure DebugLn(const s1, s2, s3, s4: string);
begin
  DebugLn(s1+s2+s3+s4);
end;

procedure DebugLn(const s1, s2, s3, s4, s5: string);
begin
  DebugLn(s1+s2+s3+s4+s5);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7+s8);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8, s9: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11,
  s12: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12);
end;

procedure DBGOut(const s: string);
begin
  if TextRec(Output).Mode<>fmClosed then
    write(s);
end;

procedure DBGOut(const s1, s2: string);
begin
  DbgOut(s1+s2);
end;

function DbgS(const c: cardinal): string;
begin
  Result:=IntToStr(c);
end;

function DbgS(const i: integer): string;
begin
  Result:=IntToStr(i);
end;

function DbgS(const r: TRect): string;
begin
  Result:=' l='+IntToStr(r.Left)+',t='+IntToStr(r.Top)
         +',r='+IntToStr(r.Right)+',b='+IntToStr(r.Bottom);
end;

function DbgS(const p: TPoint): string;
begin
  Result:=' x='+IntToStr(p.x)+',y='+IntToStr(p.y);
end;

function DbgS(const p: pointer): string;
begin
  Result:=HexStr(Cardinal(p),8);
end;

function DbgS(const e: extended): string;
begin
  Result:=FloatToStr(e);
end;

function DbgS(const b: boolean): string;
begin
  if b then Result:='True' else Result:='False';
end;

function DbgS(const i1, i2, i3, i4: integer): string;
begin
  Result:=dbgs(i1)+','+dbgs(i2)+','+dbgs(i3)+','+dbgs(i4);
end;

//------------------------------------------------------------------------------
procedure InternalInit;
var
  c: char;
begin
  for c:=Low(char) to High(char) do begin
    UpChars[c]:=upcase(c);
  end;
end;

initialization
  InternalInit;


end.


