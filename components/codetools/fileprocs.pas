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
  Classes, SysUtils, AVL_Tree,
  CodeToolsStrConsts;
  
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
function DirectoryIsWritable(const DirectoryName: string): boolean;
function ExtractFileNameOnly(const AFilename: string): string;
function FilenameIsAbsolute(const TheFilename: string):boolean;
function FilenameIsWinAbsolute(const TheFilename: string):boolean;
function FilenameIsUnixAbsolute(const TheFilename: string):boolean;
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
function GetTempFilename(const Path, Prefix: string): string;
function FindDiskFilename(const Filename: string): string;

function ComparePAnsiStringFilenames(Data1, data2: Pointer): integer;

type
  TCTPascalExtType = (petNone, petPAS, petPP, petP);

const
  CTPascalExtension: array[TCTPascalExtType] of string =
    ('', '.pas', '.pp', '.p');

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
procedure DbgOut(const s1,s2,s3: string);
procedure DbgOut(const s1,s2,s3,s4: string);
procedure DbgOut(const s1,s2,s3,s4,s5: string);
procedure DbgOut(const s1,s2,s3,s4,s5,s6: string);

function DbgS(const c: char): string;
function DbgS(const c: cardinal): string;
function DbgS(const i: integer): string;
function DbgS(const r: TRect): string;
function DbgS(const p: TPoint): string;
function DbgS(const p: pointer): string;
function DbgS(const e: extended): string;
function DbgS(const b: boolean): string;

function DbgS(const i1,i2,i3,i4: integer): string;
function DbgSName(const p: TObject): string;
function DbgStr(const StringWithSpecialChars: string): string;


type
  TFileStateCacheItemFlag = (
    fsciExists,    // file or directory exists
    fsciDirectory, // file exists and is directory
    fsciReadable,  // file is readable
    fsciWritable,  // file is writable
    fsciDirectoryReadable, // file is directory and can be searched
    fsciDirectoryWritable, // file is directory and new files can be created
    fsciText,      // file is text file (not binary)
    fsciExecutable // file is executable
    );
  TFileStateCacheItemFlags = set of TFileStateCacheItemFlag;

  { TFileStateCacheItem }

  TFileStateCacheItem = class
  private
    FFilename: string;
    FFlags: TFileStateCacheItemFlags;
    FTestedFlags: TFileStateCacheItemFlags;
    FTimeStamp: integer;
  public
    constructor Create(const TheFilename: string; NewTimeStamp: integer);
  public
    property Filename: string read FFilename;
    property Flags: TFileStateCacheItemFlags read FFlags;
    property TestedFlags: TFileStateCacheItemFlags read FTestedFlags;
    property TimeStamp: integer read FTimeStamp;
  end;

  { TFileStateCache }

  TFileStateCache = class
  private
    FFiles: TAVLTree;
    FTimeStamp: integer;
    FLockCount: integer;
    procedure SetFlag(AFile: TFileStateCacheItem;
                      AFlag: TFileStateCacheItemFlag; NewValue: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Lock;
    procedure Unlock;
    function Locked: boolean;
    procedure IncreaseTimeStamp;
    function FileExistsCached(const Filename: string): boolean;
    function DirPathExistsCached(const Filename: string): boolean;
    function DirectoryIsWritableCached(const DirectoryName: string): boolean;
    function FileIsExecutableCached(const AFilename: string): boolean;
    function FileIsReadableCached(const AFilename: string): boolean;
    function FileIsWritableCached(const AFilename: string): boolean;
    function FileIsTextCached(const AFilename: string): boolean;
    function FindFile(const Filename: string;
                      CreateIfNotExists: boolean): TFileStateCacheItem;
    function Check(const Filename: string; AFlag: TFileStateCacheItemFlag;
                   var AFile: TFileStateCacheItem; var FlagIsSet: boolean): boolean;
    procedure WriteDebugReport;
  public
    property TimeStamp: integer read FTimeStamp;
  end;
  
var
  FileStateCache: TFileStateCache;

function FileExistsCached(const Filename: string): boolean;
function DirPathExistsCached(const Filename: string): boolean;
function DirectoryIsWritableCached(const DirectoryName: string): boolean;
function FileIsExecutableCached(const AFilename: string): boolean;
function FileIsReadableCached(const AFilename: string): boolean;
function FileIsWritableCached(const AFilename: string): boolean;
function FileIsTextCached(const AFilename: string): boolean;

procedure InvalidateFileStateCache;
function CompareFileStateItems(Data1, Data2: Pointer): integer;
function CompareFilenameWithFileStateCacheItem(Key, Data: Pointer): integer;

const
  FileStateCacheItemFlagNames: array[TFileStateCacheItemFlag] of string = (
    'fsciExists',
    'fsciDirectory',
    'fsciReadable',
    'fsciWritable',
    'fsciDirectoryReadable',
    'fsciDirectoryWritable',
    'fsciText',
    'fsciExecutable'
    );

implementation

// to get more detailed error messages consider the os
{$IFNDEF win32}
uses
  Unix,BaseUnix;
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
      InvalidateFileStateCache;
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

function DirectoryIsWritable(const DirectoryName: string): boolean;
var
  TempFilename: String;
  fs: TFileStream;
  s: String;
begin
  TempFilename:=GetTempFilename(DirectoryName,'tstperm');
  Result:=false;
  try
    InvalidateFileStateCache;
    fs:=TFileStream.Create(TempFilename,fmCreate);
    s:='WriteTest';
    fs.Write(s[1],length(s));
    fs.Free;
    DeleteFile(TempFilename);
    Result:=true;
  except
  end;
end;

function GetTempFilename(const Path, Prefix: string): string;
var
  i: Integer;
  CurPath: String;
  CurName: String;
begin
  Result:=ExpandFilename(Path);
  CurPath:=AppendPathDelim(ExtractFilePath(Result));
  CurName:=Prefix+ExtractFileNameOnly(Result);
  i:=1;
  repeat
    Result:=CurPath+CurName+IntToStr(i)+'.tmp';
    if not FileExists(Result) then exit;
    inc(i);
  until false;
end;

function FindDiskFilename(const Filename: string): string;
// Searches for the filename case on disk.
// The file must exist.
// For example:
//   If Filename='file' and there is only a 'File' then 'File' will be returned.
var
  StartPos: Integer;
  EndPos: LongInt;
  FileInfo: TSearchRec;
  CurDir: String;
  CurFile: String;
  AliasFile: String;
  Ambiguous: Boolean;
begin
  Result:=Filename;
  if not FileExists(Filename) then exit;
  // check every directory and filename
  StartPos:=1;
  {$IFDEF Win32}
  // uppercase Drive letter and skip it
  if ((length(Result)>=2) and (Result[1] in ['A'..'Z','a'..'z'])
  and (Result[2]=':')) then begin
    StartPos:=3;
    if Result[1] in ['a'..'z'] then
      Result[1]:=upcase(Result[1]);
  end;
  {$ENDIF}
  repeat
    // skip PathDelim
    while (StartPos<=length(Result)) and (Result[StartPos]=PathDelim) do
      inc(StartPos);
    // find end of filename part
    EndPos:=StartPos;
    while (EndPos<=length(Result)) and (Result[EndPos]<>PathDelim) do
      inc(EndPos);
    if EndPos>StartPos then begin
      // search file
      CurDir:=copy(Result,1,StartPos-1);
      CurFile:=copy(Result,StartPos,EndPos-StartPos);
      AliasFile:='';
      Ambiguous:=false;
      if SysUtils.FindFirst(CurDir+FileMask,faAnyFile,FileInfo)=0 then
      begin
        repeat
          // check if special file
          if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='')
          then
            continue;
          if CompareText(FileInfo.Name,CurFile)=0 then begin
            //writeln('FindDiskFilename ',FileInfo.Name,' ',CurFile);
            if FileInfo.Name=CurFile then begin
              // file found, has already the correct name
              AliasFile:='';
              break;
            end else begin
              // alias found, but has not the correct name
              if AliasFile='' then begin
                AliasFile:=FileInfo.Name;
              end else begin
                // there are more than one candidate
                Ambiguous:=true;
                break;
              end;
            end;
          end;
        until SysUtils.FindNext(FileInfo)<>0;
      end;
      SysUtils.FindClose(FileInfo);
      if (AliasFile<>'') and (not Ambiguous) then begin
        // better filename found -> replace
        Result:=CurDir+AliasFile+copy(Result,EndPos,length(Result));
      end;
    end;
    StartPos:=EndPos+1;
  until StartPos>length(Result);
end;

function ComparePAnsiStringFilenames(Data1, data2: Pointer): integer;
var
  s1: String;
  s2: String;
begin
  s1:=PAnsiString(Data1)^;
  s2:=PAnsiString(Data1)^;
  Result:=CompareFilenames(s1,s2);
end;

function CompareFilenames(const Filename1, Filename2: string): integer;
begin
  {$IFDEF WIN32}
  Result:=CompareText(Filename1, Filename2);
  {$ELSE}
  Result:=CompareStr(Filename1, Filename2);
  {$ENDIF}
end;

function FileIsExecutable(const AFilename: string): boolean;
begin
  {$IFDEF win32}
  Result:=true;
  {$ELSE}
  Result:= BaseUnix.FpAccess(AFilename,BaseUnix.X_OK)=0;
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
  if not(BaseUnix.FpAccess(AFilename,BaseUnix.X_OK)=0) then
  begin
    AText:='"'+AFilename+'"';
    case fpGetErrno of
    ESysEAcces:
      AText:='read access denied for '+AText;
    ESysENoEnt:
      AText:='a directory component in '+AText
                          +' does not exist or is a dangling symlink';
    ESysENotDir:
      AText:='a directory component in '+Atext+' is not a directory';
    ESysENoMem:
      AText:='insufficient memory';
    ESysELoop:
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

function FilenameIsAbsolute(const TheFilename: string):boolean;
begin
  {$IFDEF win32}
  // windows
  Result:=FilenameIsWinAbsolute(TheFilename);
  {$ELSE}
  // unix
  Result:=FilenameIsUnixAbsolute(TheFilename);
  {$ENDIF}
end;

function FilenameIsWinAbsolute(const TheFilename: string): boolean;
begin
  Result:=((length(TheFilename)>=2) and (TheFilename[1] in ['A'..'Z','a'..'z'])
           and (TheFilename[2]=':'))
     or ((length(TheFilename)>=2)
         and (TheFilename[1]='\') and (TheFilename[2]='\'));
end;

function FilenameIsUnixAbsolute(const TheFilename: string): boolean;
begin
  Result:=(TheFilename<>'') and (TheFilename[1]='/');
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
  Result:= BaseUnix.FpAccess(AFilename,BaseUnix.R_OK)=0;
  {$ENDIF}
end;

function FileIsWritable(const AFilename: string): boolean;
begin
  {$IFDEF win32}
  Result:=((FileGetAttr(AFilename) and faReadOnly)=0);
  {$ELSE}
  Result:= BaseUnix.FpAccess(AFilename,BaseUnix.W_OK)=0;
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
      //debugl('FilenameIsMatching ',DirStartMask,' ',Mask[DirStartMask],' - ',DirStartFile,' ',Filename[DirStartFile]);
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

procedure DbgOut(const s1, s2, s3: string);
begin
  DbgOut(s1+s2+s3);
end;

procedure DbgOut(const s1, s2, s3, s4: string);
begin
  DbgOut(s1+s2+s3+s4);
end;

procedure DbgOut(const s1, s2, s3, s4, s5: string);
begin
  DbgOut(s1+s2+s3+s4+s5);
end;

procedure DbgOut(const s1, s2, s3, s4, s5, s6: string);
begin
  DbgOut(s1+s2+s3+s4+s5+s6);
end;

function DbgS(const c: char): string;
begin
  case c of
  ' '..#126: Result:=c;
  else
    Result:='#'+IntToStr(ord(c));
  end;
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
  Result:=HexStr(PtrInt(p),2*sizeof(PtrInt));
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

function DbgSName(const p: TObject): string;
begin
  if p=nil then
    Result:='nil'
  else if p is TComponent then
    Result:=TComponent(p).Name+':'+p.ClassName
  else
    Result:=p.ClassName;
end;

function DbgStr(const StringWithSpecialChars: string): string;
var
  i: Integer;
  s: String;
begin
  Result:=StringWithSpecialChars;
  i:=1;
  while (i<=length(Result)) do begin
    case Result[i] of
    ' '..#126: inc(i);
    else
      s:='#'+IntToStr(ord(Result[i]));
      Result:=copy(Result,1,i-1)+s+copy(Result,i+1,length(Result)-i);
      inc(i,length(s));
    end;
  end;
end;

function FileExistsCached(const Filename: string): boolean;
begin
  Result:=FileStateCache.FileExistsCached(Filename);
end;

function DirPathExistsCached(const Filename: string): boolean;
begin
  Result:=FileStateCache.DirPathExistsCached(Filename);
end;

function DirectoryIsWritableCached(const DirectoryName: string): boolean;
begin
  Result:=FileStateCache.DirectoryIsWritableCached(DirectoryName);
end;

function FileIsExecutableCached(const AFilename: string): boolean;
begin
  Result:=FileStateCache.FileIsExecutableCached(AFilename);
end;

function FileIsReadableCached(const AFilename: string): boolean;
begin
  Result:=FileStateCache.FileIsReadableCached(AFilename);
end;

function FileIsWritableCached(const AFilename: string): boolean;
begin
  Result:=FileStateCache.FileIsWritableCached(AFilename);
end;

function FileIsTextCached(const AFilename: string): boolean;
begin
  Result:=FileStateCache.FileIsTextCached(AFilename);
end;

procedure InvalidateFileStateCache;
begin
  FileStateCache.IncreaseTimeStamp;
end;

function CompareFileStateItems(Data1, Data2: Pointer): integer;
begin
  Result:=CompareFilenames(TFileStateCacheItem(Data1).FFilename,
                           TFileStateCacheItem(Data2).FFilename);
end;

function CompareFilenameWithFileStateCacheItem(Key, Data: Pointer): integer;
begin
  Result:=CompareFilenames(AnsiString(Key),TFileStateCacheItem(Data).FFilename);
  //debugln('CompareFilenameWithFileStateCacheItem Key=',AnsiString(Key),' Data=',TFileStateCacheItem(Data).FFilename,' Result=',dbgs(Result));
end;

//------------------------------------------------------------------------------
procedure InternalInit;
var
  c: char;
begin
  FileStateCache:=TFileStateCache.Create;
  for c:=Low(char) to High(char) do begin
    UpChars[c]:=upcase(c);
  end;
end;

{ TFileStateCacheItem }

constructor TFileStateCacheItem.Create(const TheFilename: string;
  NewTimeStamp: integer);
begin
  FFilename:=TheFilename;
  FTimeStamp:=NewTimeStamp;
end;

{ TFileStateCache }

procedure TFileStateCache.SetFlag(AFile: TFileStateCacheItem;
  AFlag: TFileStateCacheItemFlag; NewValue: boolean);
begin
  if AFile.FTimeStamp<>FTimeStamp then begin
    AFile.FTestedFlags:=[];
    AFile.FTimeStamp:=FTimeStamp;
  end;
  Include(AFile.FTestedFlags,AFlag);
  if NewValue then
    Include(AFile.FFlags,AFlag)
  else
    Exclude(AFile.FFlags,AFlag);
  //debugln('TFileStateCache.SetFlag AFile.Filename=',AFile.Filename,' ',FileStateCacheItemFlagNames[AFlag],'=',dbgs(AFlag in AFile.FFlags),' Valid=',dbgs(AFlag in AFile.FTestedFlags));
end;

constructor TFileStateCache.Create;
begin
  FFiles:=TAVLTree.Create(@CompareFileStateItems);
  FTimeStamp:=1; // one higher than default for new files
end;

destructor TFileStateCache.Destroy;
begin
  FFiles.FreeAndClear;
  FFiles.Free;
  inherited Destroy;
end;

procedure TFileStateCache.Lock;
begin
  inc(FLockCount);
end;

procedure TFileStateCache.Unlock;

  procedure RaiseTooManyUnlocks;
  begin
    raise Exception.Create('TFileStateCache.Unlock');
  end;

begin
  if FLockCount<=0 then RaiseTooManyUnlocks;
  dec(FLockCount);
end;

function TFileStateCache.Locked: boolean;
begin
  Result:=FLockCount>0;
end;

procedure TFileStateCache.IncreaseTimeStamp;
begin
  if Self<>nil then begin
    if FTimeStamp<maxLongint then
      inc(FTimeStamp)
    else
      FTimeStamp:=-maxLongint;
  end;
  //debugln('TFileStateCache.IncreaseTimeStamp FTimeStamp=',dbgs(FTimeStamp));
end;

function TFileStateCache.FileExistsCached(const Filename: string): boolean;
var
  AFile: TFileStateCacheItem;
begin
  Result := False;
  if Check(Filename,fsciExists,AFile,Result) then exit;
  Result:=FileExists(AFile.Filename);
  SetFlag(AFile,fsciExists,Result);
  {if not Check(Filename,fsciExists,AFile,Result) then begin
    WriteDebugReport;
    raise Exception.Create('');
  end;}
end;

function TFileStateCache.DirPathExistsCached(const Filename: string): boolean;
var
  AFile: TFileStateCacheItem;
begin
  Result := False;
  if Check(Filename,fsciDirectory,AFile,Result) then exit;
  Result:=DirPathExists(AFile.Filename);
  SetFlag(AFile,fsciDirectory,Result);
end;

function TFileStateCache.DirectoryIsWritableCached(const DirectoryName: string
  ): boolean;
var
  AFile: TFileStateCacheItem;
begin
  Result := False;
  if Check(DirectoryName,fsciDirectoryWritable,AFile,Result) then exit;
  Result:=DirectoryIsWritable(AFile.Filename);
  SetFlag(AFile,fsciDirectoryWritable,Result);
end;

function TFileStateCache.FileIsExecutableCached(
  const AFilename: string): boolean;
var
  AFile: TFileStateCacheItem;
begin
  Result := False;
  if Check(AFilename,fsciExecutable,AFile,Result) then exit;
  Result:=FileIsExecutable(AFile.Filename);
  SetFlag(AFile,fsciExecutable,Result);
end;

function TFileStateCache.FileIsReadableCached(const AFilename: string): boolean;
var
  AFile: TFileStateCacheItem;
begin
  Result := False;
  if Check(AFilename,fsciReadable,AFile,Result) then exit;
  Result:=FileIsReadable(AFile.Filename);
  SetFlag(AFile,fsciReadable,Result);
end;

function TFileStateCache.FileIsWritableCached(const AFilename: string): boolean;
var
  AFile: TFileStateCacheItem;
begin
  Result := False;
  if Check(AFilename,fsciWritable,AFile,Result) then exit;
  Result:=FileIsWritable(AFile.Filename);
  SetFlag(AFile,fsciWritable,Result);
end;

function TFileStateCache.FileIsTextCached(const AFilename: string): boolean;
var
  AFile: TFileStateCacheItem;
begin
  Result := False;
  if Check(AFilename,fsciText,AFile,Result) then exit;
  Result:=FileIsText(AFile.Filename);
  SetFlag(AFile,fsciText,Result);
end;

function TFileStateCache.FindFile(const Filename: string;
  CreateIfNotExists: boolean): TFileStateCacheItem;
var
  TrimmedFilename: String;
  ANode: TAVLTreeNode;
begin
  // make filename unique
  TrimmedFilename:=ChompPathDelim(TrimFilename(Filename));
  ANode:=FFiles.FindKey(PChar(TrimmedFilename),
                        @CompareFilenameWithFileStateCacheItem);
  if ANode<>nil then
    Result:=TFileStateCacheItem(ANode.Data)
  else if CreateIfNotExists then begin
    Result:=TFileStateCacheItem.Create(TrimmedFilename,FTimeStamp);
    FFiles.Add(Result);
    if FFiles.FindKey(PChar(TrimmedFilename),
                      @CompareFilenameWithFileStateCacheItem)=nil
    then begin
      WriteDebugReport;
      raise Exception.Create('');
    end;
  end else
    Result:=nil;
end;

function TFileStateCache.Check(const Filename: string;
  AFlag: TFileStateCacheItemFlag; var AFile: TFileStateCacheItem;
  var FlagIsSet: boolean): boolean;
begin
  AFile:=FindFile(Filename,true);
  if FTimeStamp=AFile.FTimeStamp then begin
    Result:=AFlag in AFile.FTestedFlags;
    FlagIsSet:=AFlag in AFile.FFlags;
  end else begin
    AFile.FTestedFlags:=[];
    AFile.FTimeStamp:=FTimeStamp;
    Result:=false;
    FlagIsSet:=false;
  end;
  //debugln('TFileStateCache.Check Filename=',Filename,' AFile.Filename=',AFile.Filename,' ',FileStateCacheItemFlagNames[AFlag],'=',dbgs(FlagIsSet),' Valid=',dbgs(Result));
end;

procedure TFileStateCache.WriteDebugReport;
var
  ANode: TAVLTreeNode;
  AFile: TFileStateCacheItem;
begin
  debugln('TFileStateCache.WriteDebugReport FTimeStamp=',dbgs(FTimeStamp));
  ANode:=FFiles.FindLowest;
  while ANode<>nil do begin
    AFile:=TFileStateCacheItem(ANode.Data);
    debugln('  "',AFile.Filename,'" TimeStamp=',dbgs(AFile.TimeStamp));
    ANode:=FFiles.FindSuccessor(ANode);
  end;
  debugln(' FFiles=',dbgs(FFiles.ConsistencyCheck));
  debugln(FFiles.ReportAsString);
end;

initialization
  InternalInit;

finalization
  FileStateCache.Free;
  FileStateCache:=nil;

end.


