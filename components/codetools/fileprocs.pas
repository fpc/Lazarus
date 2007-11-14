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
  Classes, SysUtils, AVL_Tree, CodeToolsStrConsts;

type
  TFPCStreamSeekType = int64;
  TFPCMemStreamSeekType = integer;
  PCharZ = Pointer;

const
  SpecialChar = '#'; // used to use PathDelim, e.g. #\
  {$IFDEF MSWindows}
  FileMask = '*.*';
  ExeExt = '.exe';
  {$ELSE}
  FileMask = '*';
  ExeExt = '';
  {$ENDIF}
  {$ifdef MSWindows}
  {$define CaseInsensitiveFilenames}
  {$endif}

type
  TCTSearchFileCase = (
    ctsfcDefault,  // e.g. case insensitive on windows
    ctsfcLoUpCase, // also search for lower and upper case
    ctsfcAllCase   // search case insensitive
    );

function CompareFilenames(const Filename1, Filename2: string): integer;
function CompareFileExt(const Filename, Ext: string;
                        CaseSensitive: boolean): integer;
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
function FilenameIsTrimmed(const TheFilename: string): boolean;
function FilenameIsTrimmed(StartPos: PChar; NameLen: integer): boolean;
function TrimFilename(const AFilename: string): string;
function CleanAndExpandFilename(const Filename: string): string;
function CleanAndExpandDirectory(const Filename: string): string;
function CreateRelativePath(const Filename, BaseDirectory: string;
                            UsePointDirectory: boolean = false): string;
function FileIsInPath(const Filename, Path: string): boolean;
function AppendPathDelim(const Path: string): string;
function ChompPathDelim(const Path: string): string;
function ClearFile(const Filename: string; RaiseOnError: boolean): boolean;
function GetTempFilename(const Path, Prefix: string): string;
function SearchFileInDir(const Filename, BaseDirectory: string;
                         SearchCase: TCTSearchFileCase): string;
function SearchFileInPath(const Filename, BasePath, SearchPath,
                      Delimiter: string; SearchCase: TCTSearchFileCase): string;
function FilenameIsMatching(const Mask, Filename: string;
                            MatchExactly: boolean): boolean;
function GetFilenameOnDisk(const AFilename: string): string;
function FindDiskFilename(const Filename: string): string;

function CompareAnsiStringFilenames(Data1, data2: Pointer): integer;
function CompareFilenameOnly(Filename: PChar; FilenameLen: integer;
   NameOnly: PChar; NameOnlyLen: integer; CaseSensitive: boolean): integer;

// searching .pas, .pp, .p
function FilenameIsPascalUnit(const Filename: string;
                              CaseSensitive: boolean = false): boolean;
function FilenameIsPascalUnit(Filename: PChar; FilenameLen: integer;
                              CaseSensitive: boolean = false): boolean;
function SearchPascalUnitInDir(const AnUnitName, BaseDirectory: string;
                               SearchCase: TCTSearchFileCase): string;
function SearchPascalUnitInPath(const AnUnitName, BasePath, SearchPath,
                      Delimiter: string; SearchCase: TCTSearchFileCase): string;

// searching .ppu
function SearchPascalFileInDir(const ShortFilename, BaseDirectory: string;
                               SearchCase: TCTSearchFileCase): string;
function SearchPascalFileInPath(const ShortFilename, BasePath, SearchPath,
                      Delimiter: string; SearchCase: TCTSearchFileCase): string;

function CreateAbsoluteSearchPath(const SearchPath, BaseDirectory: string): string;
function CreateRelativeSearchPath(const SearchPath, BaseDirectory: string): string;
function MinimizeSearchPath(const SearchPath: string): string;
function FindPathInSearchPath(APath: PChar; APathLen: integer;
                              SearchPath: PChar; SearchPathLen: integer): PChar;

type
  TCTPascalExtType = (petNone, petPAS, petPP, petP);

const
  CTPascalExtension: array[TCTPascalExtType] of string =
    ('', '.pas', '.pp', '.p');

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
    FChangeTimeStampHandler: array of TNotifyEvent;
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
                   out AFile: TFileStateCacheItem; var FlagIsSet: boolean): boolean;
    procedure WriteDebugReport;
    procedure AddChangeTimeStampHandler(const Handler: TNotifyEvent);
    procedure RemoveChangeTimeStampHandler(const Handler: TNotifyEvent);
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

// basic utility -> should go to RTL
function ComparePointers(p1, p2: Pointer): integer;
procedure MergeSort(List: PPointer; ListLength: PtrInt;
                    Compare: TListSortCompare);
function GetNextDelimitedItem(const List: string; Delimiter: char;
                              var Position: integer): string;
function AVLTreeHasDoubles(Tree: TAVLTree): TAVLTreeNode;

// debugging
procedure DebugLn(Args: array of const);
procedure DebugLn(const S: String; Args: array of const);// similar to Format(s,Args)
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

function DbgS(const c: char): string; overload;
function DbgS(const c: cardinal): string; overload;
function DbgS(const i: integer): string; overload;
function DbgS(const i: QWord): string; overload;
function DbgS(const i: int64): string; overload;
function DbgS(const r: TRect): string; overload;
function DbgS(const p: TPoint): string; overload;
function DbgS(const p: pointer): string; overload;
function DbgS(const e: extended; MaxDecimals: integer = 999): string; overload;
function DbgS(const b: boolean): string; overload;
function DbgSName(const p: TObject): string; overload;
function DbgSName(const p: TClass): string; overload;

function DbgS(const i1,i2,i3,i4: integer): string; overload;
function DbgStr(const StringWithSpecialChars: string): string;

function GetTicks: int64;

type
  TCTStackTracePointers = array of Pointer;
  TCTLineInfoCacheItem = record
    Addr: Pointer;
    Info: string;
  end;
  PCTLineInfoCacheItem = ^TCTLineInfoCacheItem;

procedure CTDumpStack;
function CTGetStackTrace(UseCache: boolean): string;
procedure CTGetStackTracePointers(var AStack: TCTStackTracePointers);
function CTStackTraceAsString(const AStack: TCTStackTracePointers;
                            UseCache: boolean): string;
function CTGetLineInfo(Addr: Pointer; UseCache: boolean): string;
function CompareCTLineInfoCacheItems(Data1, Data2: Pointer): integer;
function CompareAddrWithCTLineInfoCacheItem(Addr, Item: Pointer): integer;

var
  FPUpChars: array[char] of char;

implementation

// to get more detailed error messages consider the os
{$IFNDEF MSWindows}
uses
  Unix, BaseUnix;
{$ENDIF}

var
  LineInfoCache: TAVLTree = nil;
  LastTick: int64 = 0;

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
  TempFilename:=GetTempFilename(AppendPathDelim(DirectoryName),'tstperm');
  Result:=false;
  try
    fs:=TFileStream.Create(TempFilename,fmCreate);
    s:='WriteTest';
    fs.Write(s[1],length(s));
    fs.Free;
    if not DeleteFile(TempFilename) then
      InvalidateFileStateCache;
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
// if it does not exist, only the found path will be improved
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
  FileNotFound: Boolean;
begin
  Result:=Filename;
  // check every directory and filename
  StartPos:=1;
  {$IFDEF MSWindows}
  // uppercase Drive letter and skip it
  if ((length(Result)>=2) and (Result[1] in ['A'..'Z','a'..'z'])
  and (Result[2]=':')) then begin
    StartPos:=3;
    if Result[1] in ['a'..'z'] then
      Result[1]:=FPUpChars[Result[1]];
  end;
  {$ENDIF}
  FileNotFound:=false;
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
      end else
        FileNotFound:=true;
      SysUtils.FindClose(FileInfo);
      if FileNotFound then break;
      if (AliasFile<>'') and (not Ambiguous) then begin
        // better filename found -> replace
        Result:=CurDir+AliasFile+copy(Result,EndPos,length(Result));
      end;
    end;
    StartPos:=EndPos+1;
  until StartPos>length(Result);
end;

function CompareAnsiStringFilenames(Data1, data2: Pointer): integer;
var
  s1: String;
  s2: String;
begin
  s1:='';
  s2:='';
  Pointer(s1):=Data1;
  Pointer(s2):=Data2;
  Result:=CompareFilenames(s1,s2);
  Pointer(s1):=nil;
  Pointer(s2):=nil;
end;

function CompareFilenameOnly(Filename: PChar; FilenameLen: integer;
  NameOnly: PChar; NameOnlyLen: integer; CaseSensitive: boolean): integer;
// compare only the filename (without extension and path)
var
  EndPos: integer;
  StartPos: LongInt;
  p: Integer;
  l: LongInt;
  FilenameOnlyLen: Integer;
begin
  StartPos:=FilenameLen;
  while (StartPos>0) and (Filename[StartPos-1]<>PathDelim) do dec(StartPos);
  EndPos:=FilenameLen;
  while (EndPos>StartPos) and (Filename[EndPos]<>'.') do dec(EndPos);
  if (EndPos=StartPos) and (EndPos<FilenameLen) and (Filename[EndPos]<>'.') then
    EndPos:=FilenameLen;
  FilenameOnlyLen:=EndPos-StartPos;
  l:=FilenameOnlyLen;
  if l>NameOnlyLen then
    l:=NameOnlyLen;
  //DebugLn('CompareFilenameOnly NameOnly="',copy(NameOnly,1,NameOnlyLen),'" FilenameOnly="',copy(Filename,StartPos,EndPos-StartPos),'"');
  p:=0;
  if CaseSensitive then begin
    while p<l do begin
      Result:=ord(Filename[StartPos+p])-ord(NameOnly[p]);
      if Result<>0 then exit;
      inc(p);
    end;
  end else begin
    while p<l do begin
      Result:=ord(FPUpChars[Filename[StartPos+p]])-ord(FPUpChars[NameOnly[p]]);
      if Result<>0 then exit;
      inc(p);
    end;
  end;
  Result:=FilenameOnlyLen-NameOnlyLen;
end;

function CompareFilenames(const Filename1, Filename2: string): integer;
begin
  {$IFDEF CaseInsensitiveFilenames}
  Result:=CompareText(Filename1, Filename2);
  {$ELSE}
  //debugln(['CompareFilenames F1="',length(Filename1),'" F2="',length(Filename2),'"']);
  Result:=CompareStr(Filename1, Filename2);
  {$ENDIF}
end;

function FileIsExecutable(const AFilename: string): boolean;
begin
  {$IFDEF MSWindows}
  Result:=true;
  {$ELSE}
  Result:= BaseUnix.FpAccess(AFilename,BaseUnix.X_OK)=0;
  {$ENDIF}
end;

procedure CheckIfFileIsExecutable(const AFilename: string);
{$IFNDEF MSWindows}
var AText: string;
{$ENDIF}
begin
  // TProcess does not report, if a program can not be executed
  // to get good error messages consider the OS
  if not FileExists(AFilename) then begin
    raise Exception.CreateFmt(ctsFileDoesNotExists,[AFilename]);
  end;
  {$IFNDEF MSWindows}
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
  {$IFDEF MSWindows}
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
  {$IFDEF CaseInsensitiveFilenames}
  Result:=FindDiskFilename(Result);
  {$ENDIF}
end;

function DirPathExists(DirectoryName: string): boolean;
begin
  Result:=Sysutils.DirectoryExists(ChompPathDelim(DirectoryName));
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
  {$IFDEF MSWindows}
  Result:=true;
  {$ELSE}
  Result:= BaseUnix.FpAccess(AFilename,BaseUnix.R_OK)=0;
  {$ENDIF}
end;

function FileIsWritable(const AFilename: string): boolean;
begin
  {$IFDEF MSWindows}
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

function FilenameIsTrimmed(const TheFilename: string): boolean;
begin
  Result:=FilenameIsTrimmed(PChar(Pointer(TheFilename)),// pointer type cast avoids #0 check
                            length(TheFilename));
end;

function FilenameIsTrimmed(StartPos: PChar; NameLen: integer): boolean;
var
  i: Integer;
begin
  Result:=false;
  if NameLen<=0 then begin
    Result:=true;
    exit;
  end;
  // check heading spaces
  if StartPos[0]=' ' then exit;
  // check trailing spaces
  if StartPos[NameLen-1]=' ' then exit;
  // check ./ at start
  if (StartPos[0]='.') and (StartPos[1]=PathDelim) then exit;
  i:=0;
  while i<NameLen do begin
    if StartPos[i]<>PathDelim then
      inc(i)
    else begin
      inc(i);
      if i=NameLen then break;

      // check for double path delimiter
      if (StartPos[i]=PathDelim) then exit;

      if (StartPos[i]='.') and (i>0) then begin
        inc(i);
        // check /./ or /. at end
        if (StartPos[i]=PathDelim) or (i=NameLen) then exit;
        if StartPos[i]='.' then begin
          inc(i);
          // check /../ or /.. at end
          if (StartPos[i]=PathDelim) or (i=NameLen) then exit;
        end;
      end;
    end;
  end;
  Result:=true;
end;

function TrimFilename(const AFilename: string): string;
// trim double path delims, heading and trailing spaces
// and special dirs . and ..
var SrcPos, DestPos, l, DirStart: integer;
  c: char;
  MacroPos: LongInt;
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

  // trim double path delimiters and special dirs . and ..
  while (SrcPos<=l) do begin
    c:=AFilename[SrcPos];
    // check for double path delims
    if (c=PathDelim) then begin
      inc(SrcPos);
      {$IFDEF MSWindows}
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
          //  8. xxxdir/..  -> trim dir and skip ..
          if DestPos=1 then begin
            //  1. ..      -> copy
          end else if (DestPos=2) and (Result[1]=PathDelim) then begin
            //  2. /..     -> skip .., keep /
            inc(SrcPos,2);
            continue;
          {$IFDEF MSWindows}
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
              MacroPos:=DirStart;
              while MacroPos<DestPos do begin
                if (Result[MacroPos]='$')
                and (Result[MacroPos+1] in ['(','a'..'z','A'..'Z']) then begin
                  // 8. directory contains a macro -> keep
                  break;
                end;
                inc(MacroPos);
              end;
              if MacroPos=DestPos then begin
                DestPos:=DirStart;
                inc(SrcPos,2);
                continue;
              end;
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

function CreateRelativePath(const Filename, BaseDirectory: string;
  UsePointDirectory: boolean): string;
var
  FileNameLength: Integer;
  BaseDirLen: Integer;
  MinLen: Integer;
  SamePos: Integer;
  UpDirCount: Integer;
  BaseDirPos: Integer;
  ResultPos: Integer;
  i: Integer;
  FileNameRestLen: Integer;
begin
  Result:=Filename;
  if (BaseDirectory='') or (Filename='') then exit;

  {$IFDEF MSWindows}
  // check for different windows file drives
  if (CompareText(ExtractFileDrive(Filename),
                  ExtractFileDrive(BaseDirectory))<>0)
  then
    exit;
  {$ENDIF}

  FileNameLength:=length(Filename);
  BaseDirLen:=length(BaseDirectory);

  // skip matching directories
  MinLen:=FileNameLength;
  if MinLen>BaseDirLen then MinLen:=BaseDirLen;
  SamePos:=1;
  while (SamePos<=MinLen) do begin
    {$IFDEF CaseInsensitiveFilenames}
    if AnsiStrLIComp(@FileName[SamePos],@BaseDirectory[SamePos],1)=0
    {$ELSE}
    if FileName[SamePos]=BaseDirectory[SamePos]
    {$ENDIF}
    then
      inc(SamePos)
    else
      break;
  end;
  if (SamePos>MinLen)
  and (((SamePos<=BaseDirLen) and (BaseDirectory[SamePos]=PathDelim))
    or ((SamePos<=FileNameLength) and (Filename[SamePos]=PathDelim))
    or (BaseDirLen=FileNameLength))
  then begin
    // Filename lies in BaseDirectory
    // or Filename is parent directory of BaseDirectory
    // or Filename is BaseDirectory
  end else begin
    // difference found -> step back to path delimiter
    repeat
      dec(SamePos);
      if (SamePos<1) then exit;
    until (FileName[SamePos]=PathDelim);
  end;
  if (SamePos=1) and (Filename[1]=PathDelim) then exit;

  // calculate needed up directories
  UpDirCount:=0;
  BaseDirPos:=SamePos+1;
  while (BaseDirPos<=BaseDirLen) do begin
    if (BaseDirectory[BaseDirPos]=PathDelim) then
      inc(UpDirCount);
    inc(BaseDirPos);
  end;
  if (SamePos<BaseDirLen) and (BaseDirectory[BaseDirLen]<>PathDelim) then
    inc(UpDirCount);

  // create relative filename
  FileNameRestLen:=FileNameLength-SamePos;
  SetLength(Result,3*UpDirCount+FileNameRestLen);
  ResultPos:=1;
  for i:=1 to UpDirCount do begin
    Result[ResultPos]:='.';
    Result[ResultPos+1]:='.';
    Result[ResultPos+2]:=PathDelim;
    inc(ResultPos,3);
  end;
  if FileNameRestLen>0 then
    Move(Filename[SamePos+1],Result[ResultPos],FileNameRestLen);

  // use '.' for an Filename=BaseDirectory
  if UsePointDirectory and (Result='') and (Filename<>'') then
    Result:='.';
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
var
  Len: Integer;
begin
  Result:=Path;
  Len:=length(Result);
  while (Len>1) and (Result[Len]=PathDelim) do dec(Len);
  if Len<length(Result) then
    SetLength(Result,Len);
end;

function FilenameIsPascalUnit(const Filename: string;
  CaseSensitive: boolean): boolean;
var
  i: TCTPascalExtType;
begin
  for i:=Low(CTPascalExtension) to High(CTPascalExtension) do begin
    if CTPascalExtension[i]='' then continue;
    if CompareFileExt(Filename,CTPascalExtension[i],CaseSensitive)=0 then
      exit(true);
  end;
  Result:=false;
end;

function FilenameIsPascalUnit(Filename: PChar; FilenameLen: integer;
  CaseSensitive: boolean): boolean;
var
  StartPos: LongInt;
  ExtLen: Integer;
  e: TCTPascalExtType;
  i: Integer;
  p: PChar;
begin
  StartPos:=FilenameLen-1;
  while (StartPos>=0) and (Filename[StartPos]<>'.') do dec(StartPos);
  if StartPos<=0 then exit(false);
  // check extension
  ExtLen:=FilenameLen-StartPos;
  for e:=Low(CTPascalExtension) to High(CTPascalExtension) do begin
    if (CTPascalExtension[e]='') or (length(CTPascalExtension[e])<>ExtLen) then
      continue;
    i:=0;
    p:=PChar(Pointer(CTPascalExtension[e]));// pointer type cast avoids #0 check
    if CaseSensitive then begin
      while (i<ExtLen) and (p^=Filename[StartPos+i]) do begin
        inc(i);
        inc(p);
      end;
    end else begin
      while (i<ExtLen) and (FPUpChars[p^]=FPUpChars[Filename[StartPos+i]]) do
      begin
        inc(i);
        inc(p);
      end;
    end;
    if i=ExtLen then begin
      // check name is identifier
      i:=0;
      if not (Filename[i] in ['a'..'z','A'..'Z','_']) then exit(false);
      inc(i);
      while i<StartPos do begin
        if not (Filename[i] in ['a'..'z','A'..'Z','_','0'..'9']) then exit(false);
        inc(i);
      end;
      exit(true);
    end;
  end;
  Result:=false;
end;

function SearchPascalUnitInDir(const AnUnitName, BaseDirectory: string;
  SearchCase: TCTSearchFileCase): string;

  procedure RaiseNotImplemented;
  begin
    raise Exception.Create('not implemented');
  end;

var
  Base: String;
  FileInfo: TSearchRec;
  LowerCaseUnitname: String;
  UpperCaseUnitname: String;
  CurUnitName: String;
begin
  Base:=AppendPathDelim(BaseDirectory);
  Base:=TrimFilename(Base);
  // search file
  Result:='';
  if SearchCase=ctsfcAllCase then
    Base:=FindDiskFilename(Base);

  if SearchCase in [ctsfcDefault,ctsfcLoUpCase] then begin
    LowerCaseUnitname:=lowercase(AnUnitName);
    UpperCaseUnitname:=uppercase(AnUnitName);
  end else begin
    LowerCaseUnitname:='';
    UpperCaseUnitname:='';
  end;

  if SysUtils.FindFirst(Base+FileMask,faAnyFile,FileInfo)=0 then
  begin
    repeat
      // check if special file
      if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='')
      then
        continue;
      if not FilenameIsPascalUnit(FileInfo.Name,false) then continue;
      case SearchCase of
      ctsfcDefault,ctsfcLoUpCase:
        if (CompareFilenameOnly(PChar(Pointer(FileInfo.Name)),// pointer type cast avoids #0 check
                                length(FileInfo.Name),
                                PChar(Pointer(AnUnitName)),
                                length(AnUnitName),false)=0)
        then begin
          CurUnitName:=ExtractFilePath(FileInfo.Name);
          if CurUnitName=AnUnitName then begin
            Result:=FileInfo.Name;
            break;
          end else if ((LowerCaseUnitname=CurUnitName)
          or (UpperCaseUnitname=CurUnitName)) then begin
            Result:=FileInfo.Name;
          end;
        end;

      ctsfcAllCase:
        if (CompareFilenameOnly(PChar(Pointer(FileInfo.Name)),// pointer type cast avoids #0 check
                                length(FileInfo.Name),
                          PChar(Pointer(AnUnitName)),length(AnUnitName),true)=0)
        then begin
          Result:=FileInfo.Name;
          CurUnitName:=ExtractFilePath(FileInfo.Name);
          if CurUnitName=AnUnitName then
            break;
        end;

      else
        RaiseNotImplemented;
      end;
    until SysUtils.FindNext(FileInfo)<>0;
  end;
  SysUtils.FindClose(FileInfo);
  if Result<>'' then Result:=Base+Result;
end;

function SearchPascalUnitInPath(const AnUnitName, BasePath, SearchPath,
  Delimiter: string; SearchCase: TCTSearchFileCase): string;
var
  p, StartPos, l: integer;
  CurPath, Base: string;
begin
  Base:=ExpandFilename(AppendPathDelim(BasePath));
  // search in current directory
  Result:=SearchPascalUnitInDir(AnUnitName,Base,SearchCase);
  if Result<>'' then exit;
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
      CurPath:=ExpandFilename(AppendPathDelim(CurPath));
      Result:=SearchPascalUnitInDir(AnUnitName,CurPath,SearchCase);
      if Result<>'' then exit;
    end;
    StartPos:=p+1;
  end;
  Result:='';
end;

function SearchPascalFileInDir(const ShortFilename, BaseDirectory: string;
  SearchCase: TCTSearchFileCase): string;

  procedure RaiseNotImplemented;
  begin
    raise Exception.Create('not implemented');
  end;

var
  Base: String;
  FileInfo: TSearchRec;
  LowerCaseFilename: string;
  UpperCaseFilename: string;
begin
  Base:=AppendPathDelim(BaseDirectory);
  Base:=TrimFilename(Base);
  // search file
  Result:='';
  if SearchCase=ctsfcAllCase then
    Base:=FindDiskFilename(Base);
    
  if SearchCase in [ctsfcDefault,ctsfcLoUpCase] then begin
    LowerCaseFilename:=lowercase(ShortFilename);
    UpperCaseFilename:=uppercase(ShortFilename);
  end else begin
    LowerCaseFilename:='';
    UpperCaseFilename:='';
  end;
  
  if SysUtils.FindFirst(Base+FileMask,faAnyFile,FileInfo)=0 then
  begin
    repeat
      // check if special file
      if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='')
      then
        continue;
      case SearchCase of
      ctsfcDefault,ctsfcLoUpCase:
        if (ShortFilename=FileInfo.Name) then begin
          Result:=FileInfo.Name;
          break;
        end else if (LowerCaseFilename=FileInfo.Name)
        or (UpperCaseFilename=FileInfo.Name)
        then
          Result:=FileInfo.Name;

      ctsfcAllCase:
        if CompareText(ShortFilename,FileInfo.Name)=0 then begin
          Result:=FileInfo.Name;
          if ShortFilename=FileInfo.Name then break;
        end;

      else
        RaiseNotImplemented;
      end;
    until SysUtils.FindNext(FileInfo)<>0;
  end;
  SysUtils.FindClose(FileInfo);
  if Result<>'' then Result:=Base+Result;
end;

function SearchPascalFileInPath(const ShortFilename, BasePath, SearchPath,
  Delimiter: string; SearchCase: TCTSearchFileCase): string;
var
  p, StartPos, l: integer;
  CurPath, Base: string;
begin
  Base:=ExpandFilename(AppendPathDelim(BasePath));
  // search in current directory
  if not FilenameIsAbsolute(Base) then
    Base:='';
  if Base<>'' then begin
    Result:=SearchPascalFileInDir(ShortFilename,Base,SearchCase);
    if Result<>'' then exit;
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
      CurPath:=ExpandFilename(AppendPathDelim(CurPath));
      if FilenameIsAbsolute(CurPath) then begin
        Result:=SearchPascalFileInDir(ShortFilename,CurPath,SearchCase);
        if Result<>'' then exit;
      end;
    end;
    StartPos:=p+1;
  end;
  Result:='';
end;

function CreateAbsoluteSearchPath(const SearchPath, BaseDirectory: string
  ): string;
var
  PathLen: Integer;
  EndPos: Integer;
  StartPos: Integer;
  CurDir: String;
  NewCurDir: String;
  DiffLen: Integer;
  BaseDir: String;
begin
  Result:=SearchPath;
  if (SearchPath='') or (BaseDirectory='') then exit;
  BaseDir:=AppendPathDelim(BaseDirectory);

  PathLen:=length(Result);
  EndPos:=1;
  while EndPos<=PathLen do begin
    StartPos:=EndPos;
    while (Result[StartPos]=';') do begin
      inc(StartPos);
      if StartPos>PathLen then exit;
    end;
    EndPos:=StartPos;
    while (EndPos<=PathLen) and (Result[EndPos]<>';') do inc(EndPos);
    CurDir:=copy(Result,StartPos,EndPos-StartPos);
    if not FilenameIsAbsolute(CurDir) then begin
      NewCurDir:=BaseDir+CurDir;
      if NewCurDir<>CurDir then begin
        DiffLen:=length(NewCurDir)-length(CurDir);
        Result:=copy(Result,1,StartPos-1)+NewCurDir
                +copy(Result,EndPos,PathLen-EndPos+1);
        inc(EndPos,DiffLen);
        inc(PathLen,DiffLen);
      end;
    end;
    StartPos:=EndPos;
  end;
end;

function CreateRelativeSearchPath(const SearchPath, BaseDirectory: string
  ): string;
var
  PathLen: Integer;
  EndPos: Integer;
  StartPos: Integer;
  CurDir: String;
  NewCurDir: String;
  DiffLen: Integer;
begin
  Result:=SearchPath;
  if (SearchPath='') or (BaseDirectory='') then exit;

  PathLen:=length(Result);
  EndPos:=1;
  while EndPos<=PathLen do begin
    StartPos:=EndPos;
    while (Result[StartPos]=';') do begin
      inc(StartPos);
      if StartPos>PathLen then exit;
    end;
    EndPos:=StartPos;
    while (EndPos<=PathLen) and (Result[EndPos]<>';') do inc(EndPos);
    CurDir:=copy(Result,StartPos,EndPos-StartPos);
    if FilenameIsAbsolute(CurDir) then begin
      NewCurDir:=CreateRelativePath(CurDir,BaseDirectory);
      if NewCurDir<>CurDir then begin
        DiffLen:=length(NewCurDir)-length(CurDir);
        Result:=copy(Result,1,StartPos-1)+NewCurDir
                +copy(Result,EndPos,PathLen-EndPos+1);
        inc(EndPos,DiffLen);
        inc(PathLen,DiffLen);
      end;
    end;
    StartPos:=EndPos;
  end;
end;

function MinimizeSearchPath(const SearchPath: string): string;
// trim the paths, remove doubles and empty paths
var
  StartPos: Integer;
  EndPos: LongInt;
  NewPath: String;
begin
  Result:=SearchPath;
  StartPos:=1;
  while StartPos<=length(Result) do begin
    EndPos:=StartPos;
    while (EndPos<=length(Result)) and (Result[EndPos]<>';') do
      inc(EndPos);
    if StartPos<EndPos then begin
      // trim path and chomp PathDelim
      if (Result[EndPos-1]=PathDelim)
      or (not FilenameIsTrimmed(@Result[StartPos],EndPos-StartPos)) then begin
        NewPath:=ChompPathDelim(
                           TrimFilename(copy(Result,StartPos,EndPos-StartPos)));
        Result:=copy(Result,1,StartPos-1)+NewPath+copy(Result,EndPos,length(Result));
        EndPos:=StartPos+length(NewPath);
      end;
      // check if path already exists
      if FindPathInSearchPath(@Result[StartPos],EndPos-StartPos,
                              @Result[1],StartPos-1)<>nil
      then begin
        // remove path
        System.Delete(Result,StartPos,EndPos-StartPos+1);
      end else begin
        StartPos:=EndPos+1;
      end;
    end else begin
      // remove empty path
      System.Delete(Result,StartPos,1);
    end;
  end;
  if (Result<>'') and (Result[length(Result)]=';') then
    SetLength(Result,length(Result)-1);
end;

function FindPathInSearchPath(APath: PChar; APathLen: integer;
  SearchPath: PChar; SearchPathLen: integer): PChar;
var
  StartPos: Integer;
  EndPos: LongInt;
  NextStartPos: LongInt;
  CmpPos: LongInt;
begin
  Result:=nil;
  if SearchPath=nil then exit;
  if APath=nil then exit;
  // ignore trailing PathDelim at end
  while (APathLen>1) and (APath[APathLen-1]=PathDelim) do dec(APathLen);

  StartPos:=0;
  while StartPos<SearchPathLen do begin
    // find current path bounds
    NextStartPos:=StartPos;
    while (SearchPath[NextStartPos]<>';') and (NextStartPos<SearchPathLen) do
      inc(NextStartPos);
    EndPos:=NextStartPos;
    // ignore trailing PathDelim at end
    while (EndPos>StartPos+1) and (SearchPath[EndPos-1]=PathDelim) do
      dec(EndPos);
    // compare current path
    if EndPos-StartPos=APathLen then begin
      CmpPos:=0;
      while CmpPos<APathLen do begin
        if APath[CmpPos]<>SearchPath[StartPos+CmpPos] then
          break;
        inc(CmpPos);
      end;
      if CmpPos=APathLen then begin
        Result:=@SearchPath[StartPos];
        exit;
      end;
    end;
    StartPos:=NextStartPos+1;
  end;
end;

function SearchFileInDir(const Filename, BaseDirectory: string;
  SearchCase: TCTSearchFileCase): string;

  procedure RaiseNotImplemented;
  begin
    raise Exception.Create('not implemented');
  end;

var
  Base: String;
  ShortFile: String;
  FileInfo: TSearchRec;
begin
  Base:=AppendPathDelim(BaseDirectory);
  ShortFile:=Filename;
  if System.Pos(PathDelim,ShortFile)>0 then begin
    Base:=Base+ExtractFilePath(ShortFile);
    ShortFile:=ExtractFilename(ShortFile);
  end;
  Base:=TrimFilename(Base);
  case SearchCase of
  ctsfcDefault:
    begin
      Result:=Base+ShortFile;
      if not FileExistsCached(Result) then Result:='';
    end;
  ctsfcLoUpCase:
    begin
      Result:=Base+ShortFile;
      if not FileExistsCached(Result) then begin
        Result:=lowercase(Result);
        if not FileExistsCached(Result) then begin
          Result:=uppercase(Result);
          if not FileExistsCached(Result) then Result:='';
        end;
      end;
    end;
  ctsfcAllCase:
    begin
      // search file
      Result:='';
      Base:=FindDiskFilename(Base);
      if SysUtils.FindFirst(Base+FileMask,faAnyFile,FileInfo)=0 then
      begin
        repeat
          // check if special file
          if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='')
          then
            continue;
          if CompareText(FileInfo.Name,ShortFile)=0 then begin
            if FileInfo.Name=ShortFile then begin
              // file found, with correct name
              Result:=FileInfo.Name;
              break;
            end else begin
              // alias found, but has not the correct name
              Result:=FileInfo.Name;
            end;
          end;
        until SysUtils.FindNext(FileInfo)<>0;
      end;
      SysUtils.FindClose(FileInfo);
      if Result<>'' then Result:=Base+Result;
    end;
  else
    RaiseNotImplemented;
  end;
end;

function SearchFileInPath(const Filename, BasePath, SearchPath,
  Delimiter: string; SearchCase: TCTSearchFileCase): string;
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
    if SearchCase=ctsfcDefault then begin
      if FileExistsCached(Filename) then begin
        Result:=ExpandFilename(Filename);
      end else begin
        Result:='';
      end;
    end else
      Result:=SearchFileInPath(ExtractFilename(Filename),
        ExtractFilePath(BasePath),'',';',SearchCase);
    exit;
  end;
  Base:=ExpandFilename(AppendPathDelim(BasePath));
  // search in current directory
  Result:=SearchFileInDir(Filename,Base,SearchCase);
  if Result<>'' then exit;
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
      CurPath:=ExpandFilename(AppendPathDelim(CurPath));
      Result:=SearchFileInDir(Filename,CurPath,SearchCase);
      if Result<>'' then exit;
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
    Result:=(FPUpChars[c1]=FPUpChars[c2]);
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
          FileChar:=FPUpChars[FileChar];
          ExtChar:=FPUpChars[ExtChar];
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

function ComparePointers(p1, p2: Pointer): integer;
begin
  if p1>p2 then
    Result:=1
  else if p1<p2 then
    Result:=-1
  else
    Result:=0;
end;

procedure MergeSort(List: PPointer; ListLength: PtrInt;
  Compare: TListSortCompare);
var
  MergeList: PPointer;

  procedure Merge(Pos1, Pos2, Pos3: PtrInt);
  // merge two sorted arrays
  // the first array ranges Pos1..Pos2-1, the second ranges Pos2..Pos3
  var Src1Pos,Src2Pos,DestPos,cmp,i:PtrInt;
  begin
    while (Pos3>=Pos2) and (Compare(List[Pos2-1],List[Pos3])<=0) do
      dec(Pos3);
    if (Pos1>=Pos2) or (Pos2>Pos3) then exit;
    Src1Pos:=Pos2-1;
    Src2Pos:=Pos3;
    DestPos:=Pos3;
    while (Src2Pos>=Pos2) and (Src1Pos>=Pos1) do begin
      cmp:=Compare(List[Src1Pos],List[Src2Pos]);
      if cmp>0 then begin
        MergeList[DestPos]:=List[Src1Pos];
        dec(Src1Pos);
      end else begin
        MergeList[DestPos]:=List[Src2Pos];
        dec(Src2Pos);
      end;
      dec(DestPos);
    end;
    while Src2Pos>=Pos2 do begin
      MergeList[DestPos]:=List[Src2Pos];
      dec(Src2Pos);
      dec(DestPos);
    end;
    for i:=DestPos+1 to Pos3 do
      List[i]:=MergeList[i];
  end;

  procedure Sort(const Pos1, Pos2: PtrInt);
  // sort List from Pos1 to Pos2, usig MergeList as temporary buffer
  var cmp, mid: PtrInt;
  begin
    if Pos1>=Pos2 then begin
      // one element is always sorted -> nothing to do
    end else if Pos1+1=Pos2 then begin
      // two elements can be sorted easily
      cmp:=Compare(List[Pos1],List[Pos2]);
      if cmp>0 then begin
        MergeList[Pos1]:=List[Pos1];
        List[Pos1]:=List[Pos2];
        List[Pos2]:=MergeList[Pos1];
      end;
    end else begin
      mid:=(Pos1+Pos2) shr 1;
      Sort(Pos1,mid);
      Sort(mid+1,Pos2);
      Merge(Pos1,mid+1,Pos2);
    end;
  end;

// sort ascending
begin
  if ListLength<=1 then exit;
  GetMem(MergeList,SizeOf(Pointer)*ListLength);
  try
    Sort(0,ListLength-1);
  finally
    FreeMem(MergeList);
  end;
end;

function GetNextDelimitedItem(const List: string; Delimiter: char;
  var Position: integer): string;
var
  StartPos: LongInt;
begin
  StartPos:=Position;
  while (Position<=length(List)) and (List[Position]<>Delimiter) do
    inc(Position);
  Result:=copy(List,StartPos,Position-StartPos);
  if Position<=length(List) then inc(Position); // skip Delimiter
end;

function AVLTreeHasDoubles(Tree: TAVLTree): TAVLTreeNode;
var
  Next: TAVLTreeNode;
begin
  if Tree=nil then exit(nil);
  Result:=Tree.FindLowest;
  while Result<>nil do begin
    Next:=Tree.FindSuccessor(Result);
    if (Next<>nil) and (Tree.OnCompare(Result.Data,Next.Data)=0) then exit;
    Result:=Next;
  end;
end;

procedure DebugLn(Args: array of const);
var
  i: Integer;
begin
  for i:=Low(Args) to High(Args) do begin
    case Args[i].VType of
    vtInteger: DbgOut(dbgs(Args[i].vinteger));
    vtInt64: DbgOut(dbgs(Args[i].VInt64^));
    vtQWord: DbgOut(dbgs(Args[i].VQWord^));
    vtBoolean: DbgOut(dbgs(Args[i].vboolean));
    vtExtended: DbgOut(dbgs(Args[i].VExtended^));
{$ifdef FPC_CURRENCY_IS_INT64}
    // MWE:
    // fpc 2.x has troubles in choosing the right dbgs()
    // so we convert here
    vtCurrency: DbgOut(dbgs(int64(Args[i].vCurrency^)/10000 , 4));
{$else}
    vtCurrency: DbgOut(dbgs(Args[i].vCurrency^));
{$endif}
    vtString: DbgOut(Args[i].VString^);
    vtAnsiString: DbgOut(AnsiString(Args[i].VAnsiString));
    vtChar: DbgOut(Args[i].VChar);
    vtPChar: DbgOut(Args[i].VPChar);
    vtPWideChar: DbgOut(Args[i].VPWideChar);
    vtWideChar: DbgOut(Args[i].VWideChar);
    vtWidestring: DbgOut(WideString(Args[i].VWideString));
    vtObject: DbgOut(DbgSName(Args[i].VObject));
    vtClass: DbgOut(DbgSName(Args[i].VClass));
    vtPointer: DbgOut(Dbgs(Args[i].VPointer));
    else
      DbgOut('?unknown variant?');
    end;
  end;
  DebugLn;
end;

procedure DebugLn(const S: String; Args: array of const);
begin
  DebugLn(Format(S, Args));
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

function DbgS(const i: QWord): string;
begin
  Result:=IntToStr(i);
end;

function DbgS(const i: int64): string;
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
  Result:=HexStr(p-nil,2*sizeof(PtrInt));
end;

function DbgS(const e: extended; MaxDecimals: integer = 999): string;
begin
  Result:=copy(FloatToStr(e),1,MaxDecimals);
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

function DbgSName(const p: TClass): string;
begin
  if p=nil then
    Result:='nil'
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

function GetTicks: int64;
var
  CurTick: Int64;
begin
  CurTick:=round(Now*86400000);
  Result:=CurTick-LastTick;
  LastTick:=CurTick;
end;

procedure CTDumpStack;
begin
  DebugLn(CTGetStackTrace(true));
end;

function CTGetStackTrace(UseCache: boolean): string;
var
  bp: Pointer;
  addr: Pointer;
  oldbp: Pointer;
  CurAddress: Shortstring;
begin
  Result:='';
  { retrieve backtrace info }
  bp:=get_caller_frame(get_frame);
  while bp<>nil do begin
    addr:=get_caller_addr(bp);
    CurAddress:=CTGetLineInfo(addr,UseCache);
    //DebugLn('GetStackTrace ',CurAddress);
    Result:=Result+CurAddress+LineEnding;
    oldbp:=bp;
    bp:=get_caller_frame(bp);
    if (bp<=oldbp) or (bp>(StackBottom + StackLength)) then
      bp:=nil;
  end;
end;

procedure CTGetStackTracePointers(var AStack: TCTStackTracePointers);
var
  Depth: Integer;
  bp: Pointer;
  oldbp: Pointer;
begin
  // get stack depth
  Depth:=0;
  bp:=get_caller_frame(get_frame);
  while bp<>nil do begin
    inc(Depth);
    oldbp:=bp;
    bp:=get_caller_frame(bp);
    if (bp<=oldbp) or (bp>(StackBottom + StackLength)) then
      bp:=nil;
  end;
  SetLength(AStack,Depth);
  if Depth>0 then begin
    Depth:=0;
    bp:=get_caller_frame(get_frame);
    while bp<>nil do begin
      AStack[Depth]:=get_caller_addr(bp);
      inc(Depth);
      oldbp:=bp;
      bp:=get_caller_frame(bp);
      if (bp<=oldbp) or (bp>(StackBottom + StackLength)) then
        bp:=nil;
    end;
  end;
end;

function CTStackTraceAsString(const AStack: TCTStackTracePointers; UseCache: boolean
  ): string;
var
  i: Integer;
  CurAddress: String;
begin
  Result:='';
  for i:=0 to length(AStack)-1 do begin
    CurAddress:=CTGetLineInfo(AStack[i],UseCache);
    Result:=Result+CurAddress+LineEnding;
  end;
end;

function CTGetLineInfo(Addr: Pointer; UseCache: boolean): string;
var
  ANode: TAVLTreeNode;
  Item: PCTLineInfoCacheItem;
begin
  if UseCache then begin
    if LineInfoCache=nil then
      LineInfoCache:=TAVLTree.Create(@CompareCTLineInfoCacheItems);
    ANode:=LineInfoCache.FindKey(Addr,@CompareAddrWithCTLineInfoCacheItem);
    if ANode=nil then begin
      Result:=BackTraceStrFunc(Addr);
      New(Item);
      Item^.Addr:=Addr;
      Item^.Info:=Result;
      LineInfoCache.Add(Item);
    end else begin
      Result:=PCTLineInfoCacheItem(ANode.Data)^.Info;
    end;
  end else
    Result:=BackTraceStrFunc(Addr);
end;

function CompareCTLineInfoCacheItems(Data1, Data2: Pointer): integer;
begin
  Result:=ComparePointers(PCTLineInfoCacheItem(Data1)^.Addr,
                          PCTLineInfoCacheItem(Data2)^.Addr);
end;

function CompareAddrWithCTLineInfoCacheItem(Addr, Item: Pointer): integer;
begin
  Result:=ComparePointers(Addr,PCTLineInfoCacheItem(Item)^.Addr);
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
    FPUpChars[c]:=upcase(c);
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
  SetLength(FChangeTimeStampHandler,0);
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
var
  i: Integer;
begin
  if Self<>nil then begin
    if FTimeStamp<maxLongint then
      inc(FTimeStamp)
    else
      FTimeStamp:=-maxLongint;
    for i:=0 to length(FChangeTimeStampHandler)-1 do
      FChangeTimeStampHandler[i](Self);
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
  ANode:=FFiles.FindKey(Pointer(TrimmedFilename),
                        @CompareFilenameWithFileStateCacheItem);
  if ANode<>nil then
    Result:=TFileStateCacheItem(ANode.Data)
  else if CreateIfNotExists then begin
    Result:=TFileStateCacheItem.Create(TrimmedFilename,FTimeStamp);
    FFiles.Add(Result);
    if FFiles.FindKey(Pointer(TrimmedFilename),
                      @CompareFilenameWithFileStateCacheItem)=nil
    then begin
      DebugLn(format('FileStateCache.FindFile: "%s"',[FileName]));
      WriteDebugReport;
      raise Exception.Create('');
    end;
  end else
    Result:=nil;
end;

function TFileStateCache.Check(const Filename: string;
  AFlag: TFileStateCacheItemFlag; out AFile: TFileStateCacheItem;
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

procedure TFileStateCache.AddChangeTimeStampHandler(const Handler: TNotifyEvent
  );
begin
  SetLength(FChangeTimeStampHandler,length(FChangeTimeStampHandler)+1);
  FChangeTimeStampHandler[length(FChangeTimeStampHandler)-1]:=Handler;
end;

procedure TFileStateCache.RemoveChangeTimeStampHandler(
  const Handler: TNotifyEvent);
var
  i: Integer;
begin
  for i:=length(FChangeTimeStampHandler)-1 downto 0 do begin
    if Handler=FChangeTimeStampHandler[i] then begin
      if i<length(FChangeTimeStampHandler)-1 then
        System.Move(FChangeTimeStampHandler[i+1],FChangeTimeStampHandler[i],
                    SizeOf(TNotifyEvent)*(length(FChangeTimeStampHandler)-i-1));
      SetLength(FChangeTimeStampHandler,length(FChangeTimeStampHandler)-1);
    end;
  end;
end;

procedure FreeLineInfoCache;
var
  ANode: TAVLTreeNode;
  Item: PCTLineInfoCacheItem;
begin
  if LineInfoCache=nil then exit;
  ANode:=LineInfoCache.FindLowest;
  while ANode<>nil do begin
    Item:=PCTLineInfoCacheItem(ANode.Data);
    Dispose(Item);
    ANode:=LineInfoCache.FindSuccessor(ANode);
  end;
  LineInfoCache.Free;
  LineInfoCache:=nil;
end;

initialization
  {$IFDEF MEM_CHECK}CheckHeapWrtMemCnt('fileprocs.pas: initialization');{$ENDIF}
  InternalInit;

finalization
  FileStateCache.Free;
  FileStateCache:=nil;
  FreeLineInfoCache;

end.


