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

  Abstract:
    - simple file functions and fpc additions
    - all functions are thread safe unless explicitely stated
}
unit FileProcs;

{$mode objfpc}{$H+}

interface

{$I codetools.inc}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  {$IFDEF Windows}
  Windows,
  {$ENDIF}
  // RTL + FCL
  Classes, SysUtils, Laz_AVL_Tree,
  // CodeTools
  CodeToolsStrConsts,
  // LazUtils
  LazUtilities, LazLoggerBase, LazFileCache, LazFileUtils, LazUTF8, LazUTF8Classes;

type
  TFPCStreamSeekType = int64;
  TFPCMemStreamSeekType = integer;
  PCharZ = Pointer;

{$IF defined(Windows) or defined(darwin) or defined(HASAMIGA)}
{$define CaseInsensitiveFilenames}
{$ENDIF}
{$IF defined(CaseInsensitiveFilenames)}
{$define NotLiteralFilenames}
{$ENDIF}

const
  FilenamesCaseSensitive = {$IFDEF CaseInsensitiveFilenames}false{$ELSE}true{$ENDIF};// lower and upper letters are treated the same
  FilenamesLiteral = {$IFDEF NotLiteralFilenames}false{$ELSE}true{$ENDIF};// file names can be compared using = string operator

  SpecialChar = '#'; // used to use PathDelim, e.g. #\
  FileMask = AllFilesMask;
  {$IFDEF Windows}
  ExeExt = '.exe';
  {$ELSE}
    {$IFDEF NetWare}
    ExeExt = '.nlm';
    {$ELSE}
    ExeExt = '';
    {$ENDIF}
  {$ENDIF}

type
  TCTSearchFileCase = (
    ctsfcDefault,  // e.g. case insensitive on windows
    ctsfcLoUpCase, // also search for lower and upper case
    ctsfcAllCase   // search case insensitive
    );
  TCTFileAgeTime = longint;
  PCTFileAgeTime = ^TCTFileAgeTime;

// file operations
function FileDateToDateTimeDef(aFileDate: TCTFileAgeTime; const Default: TDateTime = 0): TDateTime;
function FilenameIsMatching(const Mask, Filename: string; MatchExactly: boolean): boolean;
function FindNextDirectoryInFilename(const Filename: string; var Position: integer): string;

function ClearFile(const Filename: string; RaiseOnError: boolean): boolean;
function GetTempFilename(const Path, Prefix: string): string;
function SearchFileInDir(const Filename, BaseDirectory: string;
                         SearchCase: TCTSearchFileCase): string; // not thread-safe
function SearchFileInPath(const Filename, BasePath, SearchPath, Delimiter: string;
                         SearchCase: TCTSearchFileCase): string; overload; // not thread-safe
function FindDiskFilename(const Filename: string): string;

const
  CTInvalidChangeStamp = LUInvalidChangeStamp;
  CTInvalidChangeStamp64 = LUInvalidChangeStamp64; // using a value outside integer to spot wrong types early

function CompareAnsiStringFilenames(Data1, Data2: Pointer): integer;
function CompareFilenameOnly(Filename: PChar; FilenameLen: integer;
   NameOnly: PChar; NameOnlyLen: integer; CaseSensitive: boolean): integer;

// searching .pas, .pp, .p
function FilenameIsPascalUnit(const Filename: string;
                              CaseSensitive: boolean = false): boolean;
function FilenameIsPascalUnit(Filename: PChar; FilenameLen: integer;
                              CaseSensitive: boolean = false): boolean;
function ExtractFileUnitname(Filename: string; WithNameSpace: boolean): string;
function IsPascalUnitExt(FileExt: PChar; CaseSensitive: boolean = false): boolean;
function SearchPascalUnitInDir(const AnUnitName, BaseDirectory: string;
                               SearchCase: TCTSearchFileCase): string;
function SearchPascalUnitInPath(const AnUnitName, BasePath, SearchPath,
                      Delimiter: string; SearchCase: TCTSearchFileCase): string;

// searching .ppu
function SearchPascalFileInDir(const ShortFilename, BaseDirectory: string;
                               SearchCase: TCTSearchFileCase): string;
function SearchPascalFileInPath(const ShortFilename, BasePath, SearchPath,
                      Delimiter: string; SearchCase: TCTSearchFileCase): string;

function ReadNextFPCParameter(const CmdLine: string; var Position: integer;
    out StartPos: integer): boolean;
function ExtractFPCParameter(const CmdLine: string; StartPos: integer): string;
function FindNextFPCParameter(const CmdLine, BeginsWith: string; var Position: integer): integer;
function GetLastFPCParameter(const CmdLine, BeginsWith: string; CutBegins: boolean = true): string;
function GetFPCParameterSrcFile(const CmdLine: string): string;

type
  TCTPascalExtType = (petNone, petPAS, petPP, petP);

const
  CTPascalExtension: array[TCTPascalExtType] of string =
    ('', '.pas', '.pp', '.p');

// store date locale independent, thread safe
const DateAsCfgStrFormat='YYYYMMDD';
const DateTimeAsCfgStrFormat='YYYY/MM/DD HH:NN:SS';
function DateToCfgStr(const Date: TDateTime; const aFormat: string = DateAsCfgStrFormat): string;
function CfgStrToDate(const s: string; out Date: TDateTime; const aFormat: string = DateAsCfgStrFormat): boolean;

procedure CTIncreaseChangeStamp(var ChangeStamp: integer); inline;
procedure CTIncreaseChangeStamp64(var ChangeStamp: int64); inline;
function SimpleFormat(const Fmt: String; const Args: Array of const): String;

// misc
function FileAgeToStr(aFileAge: longint): string;
function AVLTreeHasDoubles(Tree: TAVLTree): TAVLTreeNode;

// debugging
var
  CTConsoleVerbosity: integer = {$IFDEF VerboseCodetools}1{$ELSE}0{$ENDIF}; // 0=quiet, 1=normal, 2=verbose

procedure RaiseCatchableException(const Msg: string);
procedure RaiseAndCatchException;

procedure DebugLn(Args: array of const);
procedure DebugLn(const S: String; Args: array of const);// similar to Format(s,Args)
procedure DebugLn; inline;
procedure DebugLn(const s: string); inline;
procedure DebugLn(const s1,s2: string); inline;
procedure DebugLn(const s1,s2,s3: string); inline;
procedure DebugLn(const s1,s2,s3,s4: string); inline;
procedure DebugLn(const s1,s2,s3,s4,s5: string); inline;
procedure DebugLn(const s1,s2,s3,s4,s5,s6: string); inline;
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7: string); inline;
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8: string); inline;
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9: string); inline;
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10: string); inline;
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11: string); inline;
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12: string); inline;

procedure DbgOut(Args: array of const);
procedure DbgOut(const s: string); inline;
procedure DbgOut(const s1,s2: string); inline;
procedure DbgOut(const s1,s2,s3: string); inline;
procedure DbgOut(const s1,s2,s3,s4: string); inline;
procedure DbgOut(const s1,s2,s3,s4,s5: string); inline;
procedure DbgOut(const s1,s2,s3,s4,s5,s6: string); inline;

function DbgS(Args: array of const): string; overload;
function DbgS(const c: char): string; overload;
function DbgS(const c: cardinal): string; inline; overload;
function DbgS(const i: integer): string; inline; overload;
function DbgS(const i: QWord): string; inline; overload;
function DbgS(const i: int64): string; inline; overload;
function DbgS(const r: TRect): string; inline; overload;
function DbgS(const p: TPoint): string; inline; overload;
function DbgS(const p: pointer): string; inline; overload;
function DbgS(const e: extended; MaxDecimals: integer = 999): string; overload; inline;
function DbgS(const b: boolean): string; overload; inline;
function DbgS(const ms: TCustomMemoryStream; Count: PtrInt = -1): string; inline; overload;
function DbgSName(const p: TObject): string; overload; inline;
function DbgSName(const p: TClass): string; overload; inline;
function dbgMemRange(P: PByte; Count: integer; Width: integer = 0): string; inline;

function DbgS(const i1,i2,i3,i4: integer): string; overload; inline;
function DbgStr(const StringWithSpecialChars: string): string; overload;
function DbgStr(const StringWithSpecialChars: string; StartPos, Len: PtrInt): string; overload;
function DbgText(const StringWithSpecialChars: string;
                 KeepLines: boolean = true // true = add LineEnding for each line break
                 ): string; overload;

type
  TCTMemStat = class
  public
    Name: string;
    Sum: PtrUint;
  end;

  { TCTMemStats }

  TCTMemStats = class
  private
    function GetItems(const Name: string): PtrUint;
    procedure SetItems(const Name: string; const AValue: PtrUint);
  public
    Tree: TAVLTree; // tree of TCTMemStat sorted for Name with CompareText
    Total: PtrUInt;
    constructor Create;
    destructor Destroy; override;
    property Items[const Name: string]: PtrUint read GetItems write SetItems; default;
    procedure Add(const Name: string; Size: PtrUint);
    procedure WriteReport;
  end;

function CompareCTMemStat(Stat1, Stat2: TCTMemStat): integer;
function CompareNameWithCTMemStat(KeyAnsiString: Pointer; Stat: TCTMemStat): integer;

function GetTicks: int64; // not thread-safe

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
function CTGetLineInfo(Addr: Pointer; UseCache: boolean): string; // not thread safe
function CompareCTLineInfoCacheItems(Data1, Data2: Pointer): integer;
function CompareAddrWithCTLineInfoCacheItem(Addr, Item: Pointer): integer;


implementation

// to get more detailed error messages consider the os
{$IF not (defined(Windows) or defined(HASAMIGA))}
uses
  Unix;
{$ENDIF}

procedure CTIncreaseChangeStamp(var ChangeStamp: integer);
begin
  LazFileCache.LUIncreaseChangeStamp(ChangeStamp);
end;

procedure CTIncreaseChangeStamp64(var ChangeStamp: int64);
begin
  LazFileCache.LUIncreaseChangeStamp64(ChangeStamp);
end;

function SimpleFormat(const Fmt: String; const Args: array of const): String;
var
  Used: array of boolean;
  p: Integer;
  StartPos: Integer;

  procedure ReplaceArg(i: integer; var s: string);
  var
    Arg: String;
  begin
    if (i<Low(Args)) or (i>High(Args)) then exit;
    case Args[i].VType of
    vtInteger:    Arg:=dbgs(Args[i].vinteger);
    vtInt64:      Arg:=dbgs(Args[i].VInt64^);
    vtQWord:      Arg:=dbgs(Args[i].VQWord^);
    vtBoolean:    Arg:=dbgs(Args[i].vboolean);
    vtExtended:   Arg:=dbgs(Args[i].VExtended^);
    vtString:     Arg:=Args[i].VString^;
    vtAnsiString: Arg:=AnsiString(Args[i].VAnsiString);
    vtChar:       Arg:=Args[i].VChar;
    vtPChar:      Arg:=Args[i].VPChar;
    else exit;
    end;
    Used[i]:=true;
    ReplaceSubstring(s,StartPos,p-StartPos,Arg);
    p:=StartPos+length(Arg);
  end;

var
  RunIndex: Integer;
  FixedIndex: Integer;
begin
  Result:=Fmt;
  if Low(Args)>High(Args) then exit;
  SetLength(Used,High(Args)-Low(Args)+1);
  for RunIndex:=Low(Args) to High(Args) do
    Used[RunIndex]:=false;
  RunIndex:=Low(Args);
  p:=1;
  while p<=length(Result) do
  begin
    if Result[p]='%' then
    begin
      StartPos:=p;
      inc(p);
      case Result[p] of
      's':
        begin
          inc(p);
          ReplaceArg(RunIndex,Result);
          inc(RunIndex);
        end;
      '0'..'9':
        begin
          FixedIndex:=0;
          while (p<=length(Result)) and (Result[p] in ['0'..'9']) do
          begin
            if FixedIndex<High(Args) then
              FixedIndex:=FixedIndex*10+ord(Result[p])-ord('0');
            inc(p);
          end;
          if (p<=length(Result)) and (Result[p]=':') then
          begin
            inc(p);
            if (p<=length(Result)) and (Result[p]='s') then
              inc(p);
          end;
          ReplaceArg(FixedIndex,Result);
        end;
      else
        inc(p);
      end;
    end else
      inc(p);
  end;

  // append all missing arguments
  for RunIndex:=Low(Args) to High(Args) do
  begin
    if Used[RunIndex] then continue;
    Result+=',';
    StartPos:=length(Result)+1;
    p:=StartPos;
    ReplaceArg(RunIndex,Result);
  end;
end;

procedure RaiseCatchableException(const Msg: string);
begin
  { Raises an exception.
    gdb does not catch fpc Exception objects, therefore this procedure raises
    a standard AV which is catched by gdb. }
  DebugLn('ERROR in CodeTools: ',Msg);
  // creates an exception, that gdb catches:
  DebugLn('Creating gdb catchable error:');
  if (length(Msg) div (length(Msg) div 10000))=0 then ;
end;

procedure RaiseAndCatchException;
begin
  try
    if (length(ctsAddsDirToIncludePath) div (length(ctsAddsDirToIncludePath) div 10000))=0 then ;
  except
  end;
end;

var
  LineInfoCache: TAVLTree = nil;
  LastTick: int64 = 0;

function FileDateToDateTimeDef(aFileDate: TCTFileAgeTime; const Default: TDateTime
  ): TDateTime;
begin
  try
    Result:=FileDateToDateTime(aFileDate);
  except
    Result:=Default;
  end;
end;

{-------------------------------------------------------------------------------
  function ClearFile(const Filename: string; RaiseOnError: boolean): boolean;
-------------------------------------------------------------------------------}
function ClearFile(const Filename: string; RaiseOnError: boolean): boolean;
var
  fs: TFileStreamUTF8;
begin
  if FileExistsUTF8(Filename) then begin
    try
      InvalidateFileStateCache(Filename);
      fs:=TFileStreamUTF8.Create(Filename,fmOpenWrite);
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

function GetTempFilename(const Path, Prefix: string): string;
var
  i: Integer;
  CurPath: String;
  CurName: String;
begin
  Result:=ExpandFileNameUTF8(Path);
  CurPath:=AppendPathDelim(ExtractFilePath(Result));
  CurName:=Prefix+ExtractFileNameOnly(Result);
  i:=1;
  repeat
    Result:=CurPath+CurName+IntToStr(i)+'.tmp';
    if not FileExistsUTF8(Result) then exit;
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
  {$IFDEF Windows}
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
      if FindFirstUTF8(CurDir+FileMask,faAnyFile,FileInfo)=0 then
      begin
        repeat
          // check if special file
          if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='')
          then
            continue;
          if CompareFilenamesIgnoreCase(FileInfo.Name,CurFile)=0 then begin
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
              end;
            end;
          end;
        until FindNextUTF8(FileInfo)<>0;
      end else
        FileNotFound:=true;
      FindCloseUTF8(FileInfo);
      if FileNotFound then break;
      if (AliasFile<>'') and (not Ambiguous) then begin
        // better filename found -> replace
        Result:=CurDir+AliasFile+copy(Result,EndPos,length(Result));
      end;
    end;
    StartPos:=EndPos+1;
  until StartPos>length(Result);
end;

function CompareAnsiStringFilenames(Data1, Data2: Pointer): integer;
begin
  Result:=CompareFilenames(AnsiString(Data1),AnsiString(Data2));
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

function FilenameIsPascalUnit(const Filename: string;
  CaseSensitive: boolean): boolean;
begin
  Result:=(Filename<>'')
    and FilenameIsPascalUnit(PChar(Filename),length(Filename),CaseSensitive);
end;

function FilenameIsPascalUnit(Filename: PChar; FilenameLen: integer;
  CaseSensitive: boolean): boolean;
var
  ExtPos: LongInt;
  ExtLen: Integer;
  e: TCTPascalExtType;
  i: Integer;
  p: PChar;
begin
  if (Filename=nil) or (FilenameLen<2) then exit(false);
  ExtPos:=FilenameLen-1;
  while (ExtPos>0) and (Filename[ExtPos]<>'.') do dec(ExtPos);
  if ExtPos<=0 then exit(false);
  // check extension
  ExtLen:=FilenameLen-ExtPos;
  for e:=Low(CTPascalExtension) to High(CTPascalExtension) do begin
    if (CTPascalExtension[e]='') or (length(CTPascalExtension[e])<>ExtLen) then
      continue;
    i:=0;
    p:=PChar(Pointer(CTPascalExtension[e]));// pointer type cast avoids #0 check
    if CaseSensitive then begin
      while (i<ExtLen) and (p^=Filename[ExtPos+i]) do begin
        inc(i);
        inc(p);
      end;
    end else begin
      while (i<ExtLen) and (FPUpChars[p^]=FPUpChars[Filename[ExtPos+i]]) do
      begin
        inc(i);
        inc(p);
      end;
    end;
    if i<>ExtLen then continue;
    // check name is dotted identifier
    p:=@Filename[ExtPos];
    while (p>Filename) and (p[-1]<>PathDelim) do dec(p);
    repeat
      if not (p^ in ['a'..'z','A'..'Z','_']) then exit(false);
      inc(p);
      while (p^ in ['a'..'z','A'..'Z','_','0'..'9']) do inc(p);
      if p^<>'.' then exit(false);
      if p-Filename=ExtPos then exit(true);
      inc(p);
    until false;
  end;
  Result:=false;
end;

function ExtractFileUnitname(Filename: string; WithNameSpace: boolean): string;
var
  p: Integer;
begin
  Result:=ExtractFileNameOnly(Filename);
  if (Result='') or WithNameSpace then exit;
  // find last dot
  p:=length(Result);
  while p>0 do begin
    if Result[p]='.' then begin
      Delete(Result,1,p);
      exit;
    end;
    dec(p);
  end;
end;

function IsPascalUnitExt(FileExt: PChar; CaseSensitive: boolean): boolean;
// check if asciiz FileExt is a CTPascalExtension '.pp', '.pas'
var
  ExtLen: Integer;
  p: PChar;
  e: TCTPascalExtType;
  f: PChar;
begin
  Result:=false;
  if (FileExt=nil) then exit;
  ExtLen:=strlen(FileExt);
  if ExtLen=0 then exit;
  for e:=Low(CTPascalExtension) to High(CTPascalExtension) do begin
    if length(CTPascalExtension[e])<>ExtLen then
      continue;
    p:=PChar(Pointer(CTPascalExtension[e]));// pointer type cast avoids #0 check
    f:=FileExt;
    //debugln(['IsPascalUnitExt p="',dbgstr(p),'" f="',dbgstr(f),'"']);
    if CaseSensitive then begin
      while (p^=f^) and (p^<>#0) do begin
        inc(p);
        inc(f);
      end;
    end else begin
      while (FPUpChars[p^]=FPUpChars[f^]) and (p^<>#0) do
      begin
        inc(p);
        inc(f);
      end;
    end;
    if p^=#0 then
      exit(true);
  end;
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

  if FindFirstUTF8(Base+FileMask,faAnyFile,FileInfo)=0 then
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
          CurUnitName:=ExtractFileNameOnly(FileInfo.Name);
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
                                PChar(Pointer(AnUnitName)),length(AnUnitName),
                                false)=0)
        then begin
          Result:=FileInfo.Name;
          CurUnitName:=ExtractFileNameOnly(FileInfo.Name);
          if CurUnitName=AnUnitName then
            break;
        end;

      else
        RaiseNotImplemented;
      end;
    until FindNextUTF8(FileInfo)<>0;
  end;
  FindCloseUTF8(FileInfo);
  if Result<>'' then Result:=Base+Result;
end;

function SearchPascalUnitInPath(const AnUnitName, BasePath, SearchPath,
  Delimiter: string; SearchCase: TCTSearchFileCase): string;
var
  p, StartPos, l: integer;
  CurPath, Base: string;
begin
  Base:=AppendPathDelim(ExpandFileNameUTF8(BasePath));
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
      CurPath:=AppendPathDelim(ResolveDots(CurPath));
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
  
  if FindFirstUTF8(Base+FileMask,faAnyFile,FileInfo)=0 then
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
        // do not use CompareFilenamesIgnoreCase
        if SysUtils.CompareText(ShortFilename,FileInfo.Name)=0 then begin
          Result:=FileInfo.Name;
          if ShortFilename=FileInfo.Name then break;
        end;

      else
        RaiseNotImplemented;
      end;
    until FindNextUTF8(FileInfo)<>0;
  end;
  FindCloseUTF8(FileInfo);
  if Result<>'' then Result:=Base+Result;
end;

function SearchPascalFileInPath(const ShortFilename, BasePath, SearchPath,
  Delimiter: string; SearchCase: TCTSearchFileCase): string;
// search in each directory, first normal case, then lower case, then upper case
var
  p, StartPos, l: integer;
  CurPath, Base: string;
begin
  Base:=AppendPathDelim(LazFileUtils.ExpandFileNameUTF8(BasePath));
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
      CurPath:=AppendPathDelim(ResolveDots(CurPath));
      if FilenameIsAbsolute(CurPath) then begin
        Result:=SearchPascalFileInDir(ShortFilename,CurPath,SearchCase);
        if Result<>'' then exit;
      end;
    end;
    StartPos:=p+1;
  end;
  Result:='';
end;

function ReadNextFPCParameter(const CmdLine: string; var Position: integer; out
  StartPos: integer): boolean;
// reads till start of next FPC command line parameter, parses quotes ' and "
var
  c: Char;
begin
  StartPos:=Position;
  while (StartPos<=length(CmdLine)) and (CmdLine[StartPos] in [' ',#9,#10,#13]) do
    inc(StartPos);
  Position:=StartPos;
  while (Position<=length(CmdLine)) do begin
    c:=CmdLine[Position];
    case c of
    ' ',#9,#10,#13: break;
    '''','"':
      repeat
        inc(Position);
      until (Position>length(CmdLine)) or (CmdLine[Position]=c);
    end;
    inc(Position);
  end;
  Result:=StartPos<=length(CmdLine);
end;

function ExtractFPCParameter(const CmdLine: string; StartPos: integer): string;
// returns a single FPC command line parameter, resolves quotes ' and "
var
  p: Integer;
  c: Char;

  procedure Add;
  begin
    Result:=Result+copy(CmdLine,StartPos,p-StartPos);
  end;

begin
  Result:='';
  p:=StartPos;
  while (p<=length(CmdLine)) do begin
    c:=CmdLine[p];
    case c of
    ' ',#9,#10,#13: break;
    '''','"':
      begin
        Add;
        inc(p);
        StartPos:=p;
        while (p<=length(CmdLine)) do begin
          if CmdLine[p]=c then begin
            Add;
            inc(p);
            StartPos:=p;
            break;
          end;
          inc(p);
        end;
      end;
    end;
    inc(p);
  end;
  Add;
end;

function FindNextFPCParameter(const CmdLine, BeginsWith: string;
  var Position: integer): integer;
begin
  if BeginsWith='' then
    exit(-1);
  while ReadNextFPCParameter(CmdLine,Position,Result) do
    if LeftStr(ExtractFPCParameter(CmdLine,Result),length(BeginsWith))=BeginsWith
    then
      exit;
  Result:=-1;
end;

function GetLastFPCParameter(const CmdLine, BeginsWith: string;
  CutBegins: boolean): string;
var
  Param: String;
  p: Integer;
  StartPos: integer;
begin
  Result:='';
  if BeginsWith='' then
    exit;
  p:=1;
  while ReadNextFPCParameter(CmdLine,p,StartPos) do begin
    Param:=ExtractFPCParameter(CmdLine,StartPos);
    if LeftStr(Param,length(BeginsWith))=BeginsWith then begin
      Result:=Param;
      if CutBegins then
        System.Delete(Result,1,length(BeginsWith));
    end;
  end;
end;

function GetFPCParameterSrcFile(const CmdLine: string): string;
// the source file is the last parameter not starting with minus
var
  p: Integer;
  StartPos: integer;
begin
  p:=1;
  while ReadNextFPCParameter(CmdLine,p,StartPos) do begin
    if (CmdLine[StartPos]='-') then continue;
    Result:=ExtractFPCParameter(CmdLine,StartPos);
    if (Result='') or (Result[1]='-') then continue;
    exit;
  end;
  Result:='';
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
  Result:='';
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
      if not LazFileCache.FileExistsCached(Result) then Result:='';
    end;
  ctsfcLoUpCase:
    begin
      Result:=Base+ShortFile;
      if not LazFileCache.FileExistsCached(Result) then begin
        Result:=lowercase(Result);
        if not LazFileCache.FileExistsCached(Result) then begin
          Result:=uppercase(Result);
          if not LazFileCache.FileExistsCached(Result) then Result:='';
        end;
      end;
    end;
  ctsfcAllCase:
    begin
      // search file
      Base:=FindDiskFilename(Base);
      if FindFirstUTF8(Base+FileMask,faAnyFile,FileInfo)=0 then
      begin
        repeat
          // check if special file
          if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='')
          then
            continue;
          if (SysUtils.CompareText(FileInfo.Name,ShortFile)=0)
          or (CompareFilenamesIgnoreCase(FileInfo.Name,ShortFile)=0) then begin
            if FileInfo.Name=ShortFile then begin
              // file found, with correct name
              Result:=FileInfo.Name;
              break;
            end else begin
              // alias found, but has not the correct name
              Result:=FileInfo.Name;
            end;
          end;
        until FindNextUTF8(FileInfo)<>0;
      end;
      FindCloseUTF8(FileInfo);
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
    Result:='';
    exit;
  end;
  // check if filename absolute
  if FilenameIsAbsolute(Filename) then begin
    if SearchCase=ctsfcDefault then begin
      Result:=ResolveDots(Filename);
      if not LazFileCache.FileExistsCached(Result) then
        Result:='';
    end else
      Result:=SearchFileInPath(ExtractFilename(Filename),
        ExtractFilePath(BasePath),'',';',SearchCase);
    exit;
  end;
  Base:=AppendPathDelim(ExpandFileNameUTF8(BasePath));
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
      CurPath:=AppendPathDelim(ResolveDots(CurPath));
      Result:=SearchFileInDir(Filename,CurPath,SearchCase);
      if Result<>'' then exit;
    end;
    StartPos:=p+1;
  end;
  Result:='';
end;

function FilenameIsMatching(const Mask, Filename: string; MatchExactly: boolean
  ): boolean;
(*
  check if Filename matches Mask
  if MatchExactly then the complete Filename must match, else only the
  start

  Filename matches exactly or is a file/directory in a subdirectory of mask.
  Mask can contain the wildcards * and ? and the set operator {,}.
  The wildcards will *not* match PathDelim.
  You can nest the {} sets.
  If you need the asterisk, the question mark or the PathDelim as character
  just put the SpecialChar character in front of it (e.g. #*, #? #/).

  Examples:
    /abc             matches /abc, /abc/, /abc/p, /abc/xyz/filename
                     but not /abcd
    /abc/            matches /abc, /abc/, /abc//, but not /abc/.
    /abc/x?z/www     matches /abc/xyz/www, /abc/xaz/www
                     but not /abc/x/z/www
    /abc/x*z/www     matches /abc/xz/www, /abc/xyz/www, /abc/xAAAz/www
                     but not /abc/x/z/www
    /abc/x#*z/www    matches /abc/x*z/www, /abc/x*z/www/ttt
    /a{b,c,d}e       matches /abe, /ace, /ade
    *.p{as,p,}       matches a.pas, unit1.pp, b.p but not b.inc
    *.{p{as,p,},inc} matches a.pas, unit1.pp, b.p, b.inc but not c.lfm
*)
{off $DEFINE VerboseFilenameIsMatching}

  function Check(MaskP, FileP: PChar): boolean;
  var
    Level: Integer;
    MaskStart: PChar;
    FileStart: PChar;
  begin
    {$IFDEF VerboseFilenameIsMatching}
    debugln(['  Check Mask="',MaskP,'" FileP="',FileP,'"']);
    {$ENDIF}
    Result:=false;
    repeat
      case MaskP^ of
      #0:
        begin
          // the whole Mask fits the start of Filename
          // trailing PathDelim in FileP are ok
          {$IFDEF VerboseFilenameIsMatching}
          debugln(['  Check END Mask="',MaskP,'" FileP="',FileP,'"']);
          {$ENDIF}
          if FileP^=#0 then exit(true);
          if FileP^<>PathDelim then exit(false);
          while FileP^=PathDelim do inc(FileP);
          Result:=(FileP^=#0) or (not MatchExactly);
          exit;
        end;
      SpecialChar:
        begin
          // match on character
          {$IFDEF VerboseFilenameIsMatching}
          debugln(['  Check specialchar Mask="',MaskP,'" FileP="',FileP,'"']);
          {$ENDIF}
          inc(MaskP);
          if MaskP^=#0 then exit;
          if MaskP^<>FileP^ then exit;
          inc(MaskP);
          inc(FileP);
        end;
      PathDelim:
        begin
          // match PathDelim(s) or end of filename
          {$IFDEF VerboseFilenameIsMatching}
          debugln(['  Check PathDelim Mask="',MaskP,'" FileP="',FileP,'"']);
          {$ENDIF}
          if not (FileP^ in [#0,PathDelim]) then exit;
          // treat several PathDelim as one
          while MaskP^=PathDelim do inc(MaskP);
          while FileP^=PathDelim do inc(FileP);
          if MaskP^=#0 then
            exit((FileP^=#0) or not MatchExactly);
        end;
      '?':
        begin
          // match any one character, but PathDelim
          {$IFDEF VerboseFilenameIsMatching}
          debugln(['  Check any one char Mask="',MaskP,'" FileP="',FileP,'"']);
          {$ENDIF}
          if FileP^ in [#0,PathDelim] then exit;
          inc(MaskP);
          inc(FileP,UTF8CodepointSize(FileP));
        end;
      '*':
        begin
          // match 0 or more characters, but PathDelim
          {$IFDEF VerboseFilenameIsMatching}
          debugln(['  Check any chars Mask="',MaskP,'" FileP="',FileP,'"']);
          {$ENDIF}
          while MaskP^='*' do inc(MaskP);
          repeat
            if Check(MaskP,FileP) then exit(true);
            if FileP^ in [#0,PathDelim] then exit;
            inc(FileP);
          until false;
        end;
      '{':
        begin
          // OR options separated by comma
          {$IFDEF VerboseFilenameIsMatching}
          debugln(['  Check { Mask="',MaskP,'" FileP="',FileP,'"']);
          {$ENDIF}
          inc(MaskP);
          repeat
            if Check(MaskP,FileP) then begin
              {$IFDEF VerboseFilenameIsMatching}
              debugln(['  Check { option fits -> end']);
              {$ENDIF}
              exit(true);
            end;
            {$IFDEF VerboseFilenameIsMatching}
            debugln(['  Check { skip to next option ...']);
            {$ENDIF}
            // skip to next option in MaskP
            Level:=1;
            repeat
              case MaskP^ of
              #0: exit;
              SpecialChar:
                begin
                  inc(MaskP);
                  if MaskP^=#0 then exit;
                  inc(MaskP);
                end;
              '{': inc(Level);
              '}':
                begin
                  dec(Level);
                  if Level=0 then exit; // no option fits
                end;
              ',':
                if Level=1 then break;
              end;
              inc(MaskP);
            until false;
            {$IFDEF VerboseFilenameIsMatching}
            debugln(['  Check { next option: "',MaskP,'"']);
            {$ENDIF}
            inc(MaskP)
          until false;
        end;
      '}':
        begin
          {$IFDEF VerboseFilenameIsMatching}
          debugln(['  Check } Mask="',MaskP,'" FileP="',FileP,'"']);
          {$ENDIF}
          inc(MaskP);
        end;
      ',':
        begin
          // OR option fits => continue behind the {}
          {$IFDEF VerboseFilenameIsMatching}
          debugln(['  Check Skipping to end of {} Mask="',MaskP,'" ...']);
          {$ENDIF}
          Level:=1;
          repeat
            inc(MaskP);
            case MaskP^ of
            #0: exit;
            SpecialChar:
              begin
                inc(MaskP);
                if MaskP^=#0 then exit;
                inc(MaskP);
              end;
            '{': inc(Level);
            '}':
              begin
                dec(Level);
                if Level=0 then break;
              end;
            end;
          until false;
          {$IFDEF VerboseFilenameIsMatching}
          debugln(['  Check Skipped to end of {} Mask="',MaskP,'"']);
          {$ENDIF}
          inc(MaskP);
        end;
      #128..#255:
        begin
          // match UTF-8 characters
          {$IFDEF VerboseFilenameIsMatching}
          debugln(['  Check UTF-8 chars Mask="',MaskP,'" FileP="',FileP,'"']);
          {$ENDIF}
          MaskStart:=MaskP;
          FileStart:=FileP;
          while not (MaskP^ in [#0,SpecialChar,PathDelim,'?','*','{',',','}']) do
          begin
            if FileP^ in [#0,PathDelim] then exit;
            inc(MaskP,UTF8CodepointSize(MaskP));
            inc(FileP,UTF8CodepointSize(FileP));
          end;
          if CompareFilenames(MaskStart,MaskP-MaskStart,FileStart,FileP-FileStart)<>0 then
            exit;
        end;
      else
        // match ASCII characters
        repeat
          case MaskP^ of
          #0,SpecialChar,PathDelim,'?','*','{',',','}': break;
          {$IFDEF CaseInsensitiveFilenames}
          'a'..'z','A'..'Z':
            if FPUpChars[MaskP^]<>FPUpChars[FileP^] then exit;
          {$ENDIF}
          else
            if MaskP^<>FileP^ then exit;
          end;
          inc(MaskP);
          inc(FileP);
        until false;
      end;
    until false;
  end;

begin
  if Filename='' then exit(false);
  if Mask='' then exit(true);
  {$IFDEF VerboseFilenameIsMatching}
  debugln(['FilenameIsMatching2 Mask="',Mask,'" File="',Filename,'" Exactly=',MatchExactly]);
  {$ENDIF}

  Result:=Check(PChar(Mask),PChar(Filename));
end;

function FindNextDirectoryInFilename(const Filename: string;
  var Position: integer): string;
{ for example:
 Unix:
  '/a/b' -> returns first 'a', then 'b'
  '/a/' -> returns 'a', then ''
  '/a//' -> returns 'a', then '', then ''
  'a/b.pas' -> returns first 'a', then 'b.pas'
 Windows
  'C:\a\b.pas' -> returns first 'C:\', then 'a', then 'b.pas'
  'C:\a\' -> returns first 'C:\', then 'a', then ''
  'C:\a\\' -> returns first 'C:\', then 'a', then '', then ''
}
var
  StartPos: Integer;
begin
  if Position>length(Filename) then exit('');
  {$IFDEF Windows}
    if Position=1 then begin
      Result := ExtractUNCVolume(Filename);
      if Result<>'' then begin
        // is it like \\?\C:\Directory?  then also include the "C:\" part
        if (Result = '\\?\') and (Length(FileName) > 6) and
           (FileName[5] in ['a'..'z','A'..'Z']) and (FileName[6] = ':') and (FileName[7] = PathDelim)
        then
          Result := Copy(FileName, 1, 7);
        Position:=length(Result)+1;
        exit;
      end;
    end;
  {$ENDIF}
  if Filename[Position]=PathDelim then
    inc(Position);
  StartPos:=Position;
  while (Position<=length(Filename)) and (Filename[Position]<>PathDelim) do
    inc(Position);
  Result:=copy(Filename,StartPos,Position-StartPos);
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

function DateToCfgStr(const Date: TDateTime; const aFormat: string): string;
var
  NeedDate: Boolean;
  NeedTime: Boolean;
  Year: word;
  Month: word;
  Day: word;
  Hour: word;
  Minute: word;
  Second: word;
  MilliSecond: word;
  p: Integer;
  w: Word;
  StartP: Integer;
  s: String;
  l: Integer;
begin
  Result:=aFormat;
  NeedDate:=false;
  NeedTime:=false;
  for p:=1 to length(aFormat) do
    case aFormat[p] of
    'Y','M','D': NeedDate:=true;
    'H','N','S','Z': NeedTime:=true;
    end;
  if NeedDate then
    DecodeDate(Date,Year,Month,Day);
  if NeedTime then
    DecodeTime(Date,Hour,Minute,Second,MilliSecond);
  p:=1;
  while p<=length(aFormat) do begin
    case aFormat[p] of
    'Y': w:=Year;
    'M': w:=Month;
    'D': w:=Day;
    'H': w:=Hour;
    'N': w:=Minute;
    'S': w:=Second;
    'Z': w:=MilliSecond;
    else
      inc(p);
      continue;
    end;
    StartP:=p;
    repeat
      inc(p);
    until (p>length(aFormat)) or (aFormat[p]<>aFormat[p-1]);
    l:=p-StartP;
    s:=IntToStr(w);
    if length(s)<l then
      s:=StringOfChar('0',l-length(s))+s
    else if length(s)>l then
      raise Exception.Create('date format does not fit');
    ReplaceSubstring(Result,StartP,l,s);
    p:=StartP+length(s);
  end;
  //debugln('DateToCfgStr "',Result,'"');
end;

function CfgStrToDate(const s: string; out Date: TDateTime;
  const aFormat: string): boolean;

  procedure AddDecimal(var d: word; c: char); inline;
  begin
    d:=d*10+ord(c)-ord('0');
  end;

var
  i: Integer;
  Year, Month, Day, Hour, Minute, Second, MilliSecond: word;
begin
  //debugln('CfgStrToDate "',s,'"');
  if length(s)<>length(aFormat) then begin
    Date:=0.0;
    exit(false);
  end;
  try
    Year:=0;
    Month:=0;
    Day:=0;
    Hour:=0;
    Minute:=0;
    Second:=0;
    MilliSecond:=0;
    for i:=1 to length(aFormat) do begin
      case aFormat[i] of
      'Y': AddDecimal(Year,s[i]);
      'M': AddDecimal(Month,s[i]);
      'D': AddDecimal(Day,s[i]);
      'H': AddDecimal(Hour,s[i]);
      'N': AddDecimal(Minute,s[i]);
      'S': AddDecimal(Second,s[i]);
      'Z': AddDecimal(MilliSecond,s[i]);
      end;
    end;
    Date:=ComposeDateTime(EncodeDate(Year,Month,Day),EncodeTime(Hour,Minute,Second,MilliSecond));
    Result:=true;
  except
    Result:=false;
  end;
end;

procedure DebugLn(Args: array of const);
begin
  LazLoggerBase.Debugln(Args);
end;

procedure DebugLn(const S: String; Args: array of const);
begin
  LazLoggerBase.DebugLn(Format(S, Args));
end;

procedure DebugLn;
begin
  LazLoggerBase.DebugLn('');
end;

procedure DebugLn(const s: string);
begin
  LazLoggerBase.Debugln(s);
end;

procedure DebugLn(const s1, s2: string);
begin
  LazLoggerBase.Debugln(s1,s2);
end;

procedure DebugLn(const s1, s2, s3: string);
begin
  LazLoggerBase.Debugln(s1,s2,s3);
end;

procedure DebugLn(const s1, s2, s3, s4: string);
begin
  LazLoggerBase.Debugln(s1,s2,s3,s4);
end;

procedure DebugLn(const s1, s2, s3, s4, s5: string);
begin
  LazLoggerBase.Debugln(s1,s2,s3,s4,s5);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6: string);
begin
  LazLoggerBase.Debugln(s1,s2,s3,s4,s5,s6);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7: string);
begin
  LazLoggerBase.Debugln(s1,s2,s3,s4,s5,s6,s7);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8: string);
begin
  LazLoggerBase.Debugln(s1,s2,s3,s4,s5,s6,s7,s8);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8, s9: string);
begin
  LazLoggerBase.Debugln(s1,s2,s3,s4,s5,s6,s7,s8,s9);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10: string);
begin
  LazLoggerBase.Debugln(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11: string);
begin
  LazLoggerBase.Debugln(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11,
  s12: string);
begin
  LazLoggerBase.Debugln(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12);
end;

procedure DbgOut(Args: array of const);
begin
  LazLoggerBase.DbgOut(dbgs(Args));
end;

procedure DbgOut(const s: string);
begin
  LazLoggerBase.DbgOut(s);
end;

procedure DbgOut(const s1, s2: string);
begin
  LazLoggerBase.DbgOut(s1,s2);
end;

procedure DbgOut(const s1, s2, s3: string);
begin
  LazLoggerBase.DbgOut(s1,s2,s3);
end;

procedure DbgOut(const s1, s2, s3, s4: string);
begin
  LazLoggerBase.DbgOut(s1,s2,s3,s4);
end;

procedure DbgOut(const s1, s2, s3, s4, s5: string);
begin
  LazLoggerBase.DbgOut(s1,s2,s3,s4,s5);
end;

procedure DbgOut(const s1, s2, s3, s4, s5, s6: string);
begin
  LazLoggerBase.DbgOut(s1,s2,s3,s4,s5,s6);
end;

function DbgS(Args: array of const): string;
var
  i: Integer;
begin
  Result:='';
  for i:=Low(Args) to High(Args) do begin
    case Args[i].VType of
    vtInteger: Result:=Result+dbgs(Args[i].vinteger);
    vtInt64: Result:=Result+dbgs(Args[i].VInt64^);
    vtQWord: Result:=Result+dbgs(Args[i].VQWord^);
    vtBoolean: Result:=Result+dbgs(Args[i].vboolean);
    vtExtended: Result:=Result+dbgs(Args[i].VExtended^);
{$ifdef FPC_CURRENCY_IS_INT64}
    // MWE:
    // fpc 2.x has troubles in choosing the right dbgs()
    // so we convert here
    vtCurrency: Result:=Result+dbgs(int64(Args[i].vCurrency^)/10000 , 4);
{$else}
    vtCurrency: Result:=Result+dbgs(Args[i].vCurrency^);
{$endif}
    vtString: Result:=Result+Args[i].VString^;
    vtAnsiString: Result:=Result+AnsiString(Args[i].VAnsiString);
    vtChar: Result:=Result+Args[i].VChar;
    vtPChar: Result:=Result+Args[i].VPChar;
    vtPWideChar: Result:=Result+UnicodeToUTF8(ord(Args[i].VPWideChar^));
    vtWideChar: Result:=Result+UnicodeToUTF8(ord(Args[i].VWideChar));
    vtWidestring: Result:=Result+UTF8Encode(WideString(Args[i].VWideString));
    vtObject: Result:=Result+DbgSName(Args[i].VObject);
    vtClass: Result:=Result+DbgSName(Args[i].VClass);
    vtPointer: Result:=Result+Dbgs(Args[i].VPointer);
    else
      Result:=Result+'?unknown variant?';
    end;
  end;
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
  Result:=LazLoggerBase.DbgS(c);
end;

function DbgS(const i: integer): string;
begin
  Result:=LazLoggerBase.DbgS(i);
end;

function DbgS(const i: QWord): string;
begin
  Result:=LazLoggerBase.DbgS(i);
end;

function DbgS(const i: int64): string;
begin
  Result:=LazLoggerBase.DbgS(i);
end;

function DbgS(const r: TRect): string;
begin
  Result:=LazLoggerBase.DbgS(r);
end;

function DbgS(const p: TPoint): string;
begin
  Result:=LazLoggerBase.DbgS(p);
end;

function DbgS(const p: pointer): string;
begin
  Result:=LazLoggerBase.DbgS(p);
end;

function DbgS(const e: extended; MaxDecimals: integer = 999): string;
begin
  Result:=LazLoggerBase.DbgS(e,MaxDecimals);
end;

function DbgS(const b: boolean): string;
begin
  Result:=LazLoggerBase.DbgS(b);
end;

function DbgS(const i1, i2, i3, i4: integer): string;
begin
  Result:=LazLoggerBase.DbgS(i1,i2,i3,i4);
end;

function DbgS(const ms: TCustomMemoryStream; Count: PtrInt): string;
begin
  Result:=dbgMemStream(ms,Count);
end;

function DbgSName(const p: TObject): string;
begin
  Result:=LazLoggerBase.DbgSName(p);
end;

function DbgSName(const p: TClass): string;
begin
  Result:=LazLoggerBase.DbgSName(p);
end;

function dbgMemRange(P: PByte; Count: integer; Width: integer): string;
begin
  Result:=LazLoggerBase.dbgMemRange(P,Count,Width);
end;

function DbgStr(const StringWithSpecialChars: string): string;
var
  i: Integer;
  s: String;
begin
  Result:=StringWithSpecialChars;
  i:=length(Result);
  while (i>0) do begin
    case Result[i] of
    ' '..#126: ;
    else
      s:='#'+IntToStr(ord(Result[i]));
      ReplaceSubstring(Result,i,1,s);
    end;
    dec(i);
  end;
end;

function DbgStr(const StringWithSpecialChars: string; StartPos, Len: PtrInt): string;
begin
  Result:=dbgstr(copy(StringWithSpecialChars,StartPos,Len));
end;

function DbgText(const StringWithSpecialChars: string; KeepLines: boolean): string;
var
  i: Integer;
  s: String;
  c: Char;
  l: Integer;
begin
  Result:=StringWithSpecialChars;
  i:=1;
  while (i<=length(Result)) do begin
    c:=Result[i];
    case c of
    ' '..#126: inc(i);
    else
      if KeepLines and (c in [#10,#13]) then begin
        // replace line ending with system line ending
        if (i<length(Result)) and (Result[i+1] in [#10,#13])
        and (c<>Result[i+1]) then
          l:=2
        else
          l:=1;
        ReplaceSubstring(Result,i,l,LineEnding);
        inc(i,length(LineEnding));
      end else begin
        s:='#'+IntToStr(ord(c));
        ReplaceSubstring(Result,i,1,s);
        inc(i,length(s));
      end;
    end;
  end;
end;

function CompareCTMemStat(Stat1, Stat2: TCTMemStat): integer;
begin
  Result:=SysUtils.CompareText(Stat1.Name,Stat2.Name);
end;

function CompareNameWithCTMemStat(KeyAnsiString: Pointer; Stat: TCTMemStat): integer;
begin
  Result:=SysUtils.CompareText(AnsiString(KeyAnsiString),Stat.Name);
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
  Result:=LazUtilities.ComparePointers(PCTLineInfoCacheItem(Data1)^.Addr,
                          PCTLineInfoCacheItem(Data2)^.Addr);
end;

function CompareAddrWithCTLineInfoCacheItem(Addr, Item: Pointer): integer;
begin
  Result:=LazUtilities.ComparePointers(Addr,PCTLineInfoCacheItem(Item)^.Addr);
end;

function FileAgeToStr(aFileAge: longint): string;
begin
  Result:=DateTimeToStr(FileDateToDateTimeDef(aFileAge));
end;

//------------------------------------------------------------------------------

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

{ TCTMemStats }

function TCTMemStats.GetItems(const Name: string): PtrUint;
var
  Node: TAVLTreeNode;
begin
  Node:=Tree.FindKey(Pointer(Name),TListSortCompare(@CompareNameWithCTMemStat));
  if Node<>nil then
    Result:=TCTMemStat(Node.Data).Sum
  else
    Result:=0;
end;

procedure TCTMemStats.SetItems(const Name: string; const AValue: PtrUint);
var
  Node: TAVLTreeNode;
  NewStat: TCTMemStat;
begin
  Node:=Tree.FindKey(Pointer(Name),TListSortCompare(@CompareNameWithCTMemStat));
  if Node<>nil then begin
    if AValue<>0 then begin
      TCTMemStat(Node.Data).Sum:=AValue;
    end else begin
      Tree.FreeAndDelete(Node);
    end;
  end else begin
    if AValue<>0 then begin
      NewStat:=TCTMemStat.Create;
      NewStat.Name:=Name;
      NewStat.Sum:=AValue;
      Tree.Add(NewStat);
    end;
  end;
end;

constructor TCTMemStats.Create;
begin
  Tree:=TAVLTree.Create(TListSortCompare(@CompareCTMemStat));
end;

destructor TCTMemStats.Destroy;
begin
  Tree.FreeAndClear;
  FreeAndNil(Tree);
  inherited Destroy;
end;

procedure TCTMemStats.Add(const Name: string; Size: PtrUint);
var
  Node: TAVLTreeNode;
  NewStat: TCTMemStat;
begin
  inc(Total,Size);
  Node:=Tree.FindKey(Pointer(Name),TListSortCompare(@CompareNameWithCTMemStat));
  if Node<>nil then begin
    inc(TCTMemStat(Node.Data).Sum,Size);
  end else begin
    NewStat:=TCTMemStat.Create;
    NewStat.Name:=Name;
    NewStat.Sum:=Size;
    Tree.Add(NewStat);
  end;
end;

procedure TCTMemStats.WriteReport;

  function ByteToStr(b: PtrUint): string;
  const
    Units = 'KMGTPE';
  var
    i: Integer;
  begin
    i:=0;
    while b>10240 do begin
      inc(i);
      b:=b shr 10;
    end;
    Result:=dbgs(b);
    if i>0 then
      Result:=Result+Units[i];
  end;

var
  Node: TAVLTreeNode;
  CurStat: TCTMemStat;
begin
  DebugLn(['TCTMemStats.WriteReport Stats=',Tree.Count,' Total=',Total,' ',ByteToStr(Total)]);
  Node:=Tree.FindLowest;
  while Node<>nil do begin
    CurStat:=TCTMemStat(Node.Data);
    DebugLn(['  ',CurStat.Name,'=',CurStat.Sum,' ',ByteToStr(CurStat.Sum)]);
    Node:=Tree.FindSuccessor(Node);
  end;
end;

initialization
  {$IFDEF MEM_CHECK}CheckHeapWrtMemCnt('fileprocs.pas: initialization');{$ENDIF}
  FileStateCache:=TFileStateCache.Create;

finalization
  FileStateCache.Free;
  FileStateCache:=nil;
  FreeLineInfoCache;

end.


