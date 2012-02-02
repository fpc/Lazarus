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

{$ifdef FPC}
  {$mode objfpc}
{$else}
  // delphi? if so then Windows is not defined but instead MSWindows is defined => define Windows in this case
  {$ifdef MSWindows}
    {$define Windows}
  {$endif}
{$endif}{$H+}

interface

{$I codetools.inc}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, LazUTF8, LazDbgLog, LazFileCache, LazFileUtils,
  AVL_Tree, CodeToolsStrConsts;

type
  TFPCStreamSeekType = int64;
  TFPCMemStreamSeekType = integer;
  PCharZ = Pointer;

{$if defined(Windows) or defined(darwin)}
{$define CaseInsensitiveFilenames}
{$endif}
{$IF defined(CaseInsensitiveFilenames) or defined(darwin)}
{$DEFINE NotLiteralFilenames}
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

function CompareFilenames(const Filename1, Filename2: string): integer; inline;
function CompareFilenamesIgnoreCase(const Filename1, Filename2: string): integer; inline;
function CompareFileExt(const Filename, Ext: string;
                        CaseSensitive: boolean): integer; inline;
function CompareFilenameStarts(const Filename1, Filename2: string): integer; inline;
function CompareFilenames(Filename1: PChar; Len1: integer;
  Filename2: PChar; Len2: integer): integer; inline;
function DirPathExists(DirectoryName: string): boolean; inline;
function DirectoryIsWritable(const DirectoryName: string): boolean; inline;
function ExtractFileNameOnly(const AFilename: string): string; inline;
function FilenameIsAbsolute(const TheFilename: string):boolean; inline;
function FilenameIsWinAbsolute(const TheFilename: string):boolean; inline;
function FilenameIsUnixAbsolute(const TheFilename: string):boolean; inline;
function ForceDirectory(DirectoryName: string): boolean; inline;
procedure CheckIfFileIsExecutable(const AFilename: string); inline;
function FileIsExecutable(const AFilename: string): boolean; inline;
function FileIsReadable(const AFilename: string): boolean; inline;
function FileIsWritable(const AFilename: string): boolean; inline;
function FileIsText(const AFilename: string): boolean; inline;
function FileIsText(const AFilename: string; out FileReadable: boolean): boolean; inline;
function FilenameIsTrimmed(const TheFilename: string): boolean; inline;
function FilenameIsTrimmed(StartPos: PChar; NameLen: integer): boolean; inline;
function TrimFilename(const AFilename: string): string; inline;
function CleanAndExpandFilename(const Filename: string): string; inline;// empty string returns current directory
function CleanAndExpandDirectory(const Filename: string): string; inline;// empty string returns current directory
function TrimAndExpandFilename(const Filename: string): string; inline;// empty string returns empty string
function TrimAndExpandDirectory(const Filename: string): string; inline;// empty string returns empty string
function CreateRelativePath(const Filename, BaseDirectory: string;
                            UsePointDirectory: boolean = false): string; inline;
function FileIsInPath(const Filename, Path: string): boolean; inline;
function AppendPathDelim(const Path: string): string; inline;
function ChompPathDelim(const Path: string): string; inline;

// file operations
function FileExistsUTF8(const Filename: string): boolean; inline;
function FileAgeUTF8(const FileName: string): Longint; inline;
function DirectoryExistsUTF8(const Directory: string): Boolean; inline;
function ExpandFileNameUTF8(const FileName: string): string; inline;
function FindFirstUTF8(const Path: string; Attr: Longint; out Rslt: TSearchRec): Longint; inline;
function FindNextUTF8(var Rslt: TSearchRec): Longint; inline;
procedure FindCloseUTF8(var F: TSearchrec); inline;
function FileSetDateUTF8(const FileName: String; Age: Longint): Longint; inline;
function FileGetAttrUTF8(const FileName: String): Longint; inline;
function FileSetAttrUTF8(const Filename: String; Attr: longint): Longint; inline;
function DeleteFileUTF8(const FileName: String): Boolean; inline;
function RenameFileUTF8(const OldName, NewName: String): Boolean; inline;
function FileSearchUTF8(const Name, DirList : String): String; inline;
function FileIsReadOnlyUTF8(const FileName: String): Boolean; inline;
function GetCurrentDirUTF8: String; inline;
function SetCurrentDirUTF8(const NewDir: String): Boolean; inline;
function CreateDirUTF8(const NewDir: String): Boolean; inline;
function RemoveDirUTF8(const Dir: String): Boolean; inline;
function ForceDirectoriesUTF8(const Dir: string): Boolean; inline;
function FileDateToDateTimeDef(aFileDate: TCTFileAgeTime; const Default: TDateTime = 0): TDateTime;

function ClearFile(const Filename: string; RaiseOnError: boolean): boolean;
function GetTempFilename(const Path, Prefix: string): string;
function SearchFileInDir(const Filename, BaseDirectory: string;
                         SearchCase: TCTSearchFileCase): string;
function SearchFileInPath(const Filename, BasePath, SearchPath,
                      Delimiter: string; SearchCase: TCTSearchFileCase): string;
function FilenameIsMatching(const Mask, Filename: string;
                            MatchExactly: boolean): boolean;
function FindDiskFilename(const Filename: string): string;
{$IFDEF darwin}
function GetDarwinSystemFilename(Filename: string): string;
{$ENDIF}
function ReadAllLinks(const Filename: string;
                      ExceptionOnError: boolean): string; // if a link is broken returns ''
function TryReadAllLinks(const Filename: string): string; // if a link is broken returns Filename

const
  CTInvalidChangeStamp = LUInvalidChangeStamp;
  CTInvalidChangeStamp64 = LUInvalidChangeStamp64; // using a value outside integer to spot wrong types early
procedure CTIncreaseChangeStamp(var ChangeStamp: integer); inline;
procedure CTIncreaseChangeStamp64(var ChangeStamp: int64); inline;

function FileExistsCached(const AFilename: string): boolean; inline;
function DirPathExistsCached(const AFilename: string): boolean; inline;
function DirectoryIsWritableCached(const ADirectoryName: string): boolean; inline;
function FileIsExecutableCached(const AFilename: string): boolean; inline;
function FileIsReadableCached(const AFilename: string): boolean; inline;
function FileIsWritableCached(const AFilename: string): boolean; inline;
function FileIsTextCached(const AFilename: string): boolean; inline;
function FileAgeCached(const AFileName: string): Longint; inline;
function GetFilenameOnDisk(const AFilename: string): string; inline; deprecated; // use FindDiskFilename

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

// search paths
function CreateAbsoluteSearchPath(const SearchPath, BaseDirectory: string): string;
function CreateRelativeSearchPath(const SearchPath, BaseDirectory: string): string;
function MinimizeSearchPath(const SearchPath: string): string;
function FindPathInSearchPath(APath: PChar; APathLen: integer;
                              SearchPath: PChar; SearchPathLen: integer): PChar;

// FPC
function ReadNextFPCParameter(const CmdLine: string; var Position: integer;
    out StartPos: integer): boolean;

type
  TCTPascalExtType = (petNone, petPAS, petPP, petP);

const
  CTPascalExtension: array[TCTPascalExtType] of string =
    ('', '.pas', '.pp', '.p');

function FileAgeToStr(aFileAge: longint): string;

function UTF8ToSys(const s: string): string; inline;// as UTF8ToAnsi but more independent of widestringmanager
function SysToUTF8(const s: string): string; inline;// as AnsiToUTF8 but more independent of widestringmanager
function UTF8CharacterLength(p: PChar): integer; inline;

// environment
function ParamStrUTF8(Param: Integer): string;
function GetEnvironmentStringUTF8(Index : Integer): String;
function GetEnvironmentVariableUTF8(const EnvVar: String): String;

procedure InvalidateFileStateCache(const Filename: string = ''); inline;

// basic utility -> should go to RTL
function ComparePointers(p1, p2: Pointer): integer; inline;
procedure MergeSort(List: PPointer; ListLength: PtrInt;
                    Compare: TListSortCompare);
function GetNextDelimitedItem(const List: string; Delimiter: char;
                              var Position: integer): string;
function HasDelimitedItem(const List: string; Delimiter: char; FindItem: string
                          ): boolean;
function FindNextDelimitedItem(const List: string; Delimiter: char;
                               var Position: integer; FindItem: string): string;
function AVLTreeHasDoubles(Tree: TAVLTree): TAVLTreeNode;

const DateAsCfgStrFormat='YYYYMMDD';
function DateToCfgStr(const Date: TDateTime): string;
function CfgStrToDate(const s: string; out Date: TDateTime): boolean;


// debugging
procedure RaiseCatchableException(const Msg: string);
procedure RaiseAndCatchException;

type
  TCTDbgOutEvent = procedure(const s: string);
var
  CTDbgOutEvent: TCTDbgOutEvent = nil;

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

procedure DbgOut(Args: array of const);
procedure DbgOut(const s: string);
procedure DbgOut(const s1,s2: string);
procedure DbgOut(const s1,s2,s3: string);
procedure DbgOut(const s1,s2,s3,s4: string);
procedure DbgOut(const s1,s2,s3,s4,s5: string);
procedure DbgOut(const s1,s2,s3,s4,s5,s6: string);

function DbgS(Args: array of const): string; overload;
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
function DbgS(const ms: TCustomMemoryStream; Count: PtrInt = -1): string; overload;
function DbgSName(const p: TObject): string; overload;
function DbgSName(const p: TClass): string; overload;
function dbgMemRange(P: PByte; Count: integer; Width: integer = 0): string; overload;

function DbgS(const i1,i2,i3,i4: integer): string; overload;
function DbgStr(const StringWithSpecialChars: string): string; overload;
function DbgStr(const StringWithSpecialChars: string; StartPos, Len: PtrInt): string; overload;

function MemSizeString(const s: string): PtrUInt; inline;
function MemSizeFPList(const List: TFPList): PtrUInt; inline;
function GetStringRefCount(const s: string): PtrInt; inline;

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


implementation

// to get more detailed error messages consider the os
uses
{$IFDEF Windows}
  Windows;
{$ELSE}
  {$IFDEF darwin}
  MacOSAll,
  {$ENDIF}
  Unix, BaseUnix;
{$ENDIF}

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

function UTF8ToSys(const s: string): string;
begin
  Result:=LazUTF8.UTF8ToSys(s);
end;

function SysToUTF8(const s: string): string;
begin
  Result:=LazUTF8.SysToUTF8(s);
end;

function UTF8CharacterLength(p: PChar): integer;
begin
  Result:=LazUTF8.UTF8CharacterLength(p);
end;

function ParamStrUTF8(Param: Integer): string;
begin
  Result:=SysToUTF8(ObjPas.ParamStr(Param));
end;

function GetEnvironmentStringUTF8(Index: Integer): String;
begin
  Result:=SysToUTF8(SysUtils.GetEnvironmentString(Index));
end;

function GetEnvironmentVariableUTF8(const EnvVar: String): String;
begin
  Result:=SysToUTF8(SysUtils.GetEnvironmentVariable(UTF8ToSys(EnvVar)));
end;

function CompareFilenames(const Filename1, Filename2: string): integer;
begin
  Result:=LazFileUtils.CompareFilenames(Filename1,Filename2);
end;

function CompareFilenamesIgnoreCase(const Filename1, Filename2: string
  ): integer;
begin
  Result:=LazFileUtils.CompareFilenamesIgnoreCase(Filename1,Filename2);
end;

function CompareFileExt(const Filename, Ext: string; CaseSensitive: boolean
  ): integer;
begin
  Result:=LazFileUtils.CompareFileExt(Filename,Ext,CaseSensitive);
end;

function CompareFilenameStarts(const Filename1, Filename2: string): integer;
begin
  Result:=LazFileUtils.CompareFilenameStarts(Filename1,Filename2);
end;

function CompareFilenames(Filename1: PChar; Len1: integer; Filename2: PChar;
  Len2: integer): integer;
begin
  Result:=LazFileUtils.CompareFilenames(Filename1,Len1,Filename2,Len2);
end;

function DirPathExists(DirectoryName: string): boolean;
begin
  Result:=LazFileUtils.DirPathExists(DirectoryName);
end;

function DirectoryIsWritable(const DirectoryName: string): boolean;
begin
  Result:=LazFileUtils.DirectoryIsWritable(DirectoryName);
end;

function ExtractFileNameOnly(const AFilename: string): string;
begin
  Result:=LazFileUtils.ExtractFileNameOnly(AFilename);
end;

function FilenameIsAbsolute(const TheFilename: string): boolean;
begin
  Result:=LazFileUtils.FilenameIsAbsolute(TheFilename);
end;

function FilenameIsWinAbsolute(const TheFilename: string): boolean;
begin
  Result:=LazFileUtils.FilenameIsWinAbsolute(TheFilename);
end;

function FilenameIsUnixAbsolute(const TheFilename: string): boolean;
begin
  Result:=LazFileUtils.FilenameIsUnixAbsolute(TheFilename);
end;

function ForceDirectory(DirectoryName: string): boolean;
begin
  Result:=LazFileUtils.ForceDirectory(DirectoryName);
end;

procedure CheckIfFileIsExecutable(const AFilename: string);
begin
  LazFileUtils.CheckIfFileIsExecutable(AFilename);
end;

function FileIsExecutable(const AFilename: string): boolean;
begin
  Result:=LazFileUtils.FileIsExecutable(AFilename);
end;

function FileIsReadable(const AFilename: string): boolean;
begin
  Result:=LazFileUtils.FileIsReadable(AFilename);
end;

function FileIsWritable(const AFilename: string): boolean;
begin
  Result:=LazFileUtils.FileIsWritable(AFilename);
end;

function FileIsText(const AFilename: string): boolean;
begin
  Result:=LazFileUtils.FileIsText(AFilename);
end;

function FileIsText(const AFilename: string; out FileReadable: boolean
  ): boolean;
begin
  Result:=LazFileUtils.FileIsText(AFilename,FileReadable);
end;

function FilenameIsTrimmed(const TheFilename: string): boolean;
begin
  Result:=LazFileUtils.FilenameIsTrimmed(TheFilename);
end;

function FilenameIsTrimmed(StartPos: PChar; NameLen: integer): boolean;
begin
  Result:=LazFileUtils.FilenameIsTrimmed(StartPos,NameLen);
end;

function TrimFilename(const AFilename: string): string;
begin
  Result:=LazFileUtils.TrimFilename(AFilename);
end;

function CleanAndExpandFilename(const Filename: string): string;
begin
  Result:=LazFileUtils.CleanAndExpandFilename(Filename);
end;

function CleanAndExpandDirectory(const Filename: string): string;
begin
  Result:=LazFileUtils.CleanAndExpandDirectory(Filename);
end;

function TrimAndExpandFilename(const Filename: string): string;
begin
  Result:=LazFileUtils.TrimAndExpandFilename(Filename);
end;

function TrimAndExpandDirectory(const Filename: string): string;
begin
  Result:=LazFileUtils.TrimAndExpandDirectory(Filename);
end;

function CreateRelativePath(const Filename, BaseDirectory: string;
  UsePointDirectory: boolean): string;
begin
  Result:=LazFileUtils.CreateRelativePath(Filename,BaseDirectory,UsePointDirectory);
end;

function FileIsInPath(const Filename, Path: string): boolean;
begin
  Result:=LazFileUtils.FileIsInPath(Filename,Path);
end;

function AppendPathDelim(const Path: string): string;
begin
  Result:=LazFileUtils.AppendPathDelim(Path);
end;

function ChompPathDelim(const Path: string): string;
begin
  Result:=LazFileUtils.ChompPathDelim(Path);
end;

function FileExistsUTF8(const Filename: string): boolean;
begin
  Result:=LazFileUtils.FileExistsUTF8(Filename);
end;

function FileAgeUTF8(const FileName: string): Longint;
begin
  Result:=LazFileUtils.FileAgeUTF8(Filename);
end;

function DirectoryExistsUTF8(const Directory: string): Boolean;
begin
  Result:=LazFileUtils.DirectoryExistsUTF8(Directory);
end;

function ExpandFileNameUTF8(const FileName: string): string;
begin
  Result:=LazFileUtils.ExpandFileNameUTF8(Filename);
end;

function FindFirstUTF8(const Path: string; Attr: Longint; out Rslt: TSearchRec
  ): Longint;
begin
  Result:=LazFileUtils.FindFirstUTF8(Path,Attr,Rslt);
end;

function FindNextUTF8(var Rslt: TSearchRec): Longint;
begin
  Result:=LazFileUtils.FindNextUTF8(Rslt);
end;

procedure FindCloseUTF8(var F: TSearchrec);
begin
  LazFileUtils.FindCloseUTF8(F);
end;

function FileSetDateUTF8(const FileName: String; Age: Longint): Longint;
begin
  Result:=LazFileUtils.FileSetDateUTF8(FileName,Age);
end;

function FileGetAttrUTF8(const FileName: String): Longint;
begin
  Result:=LazFileUtils.FileGetAttrUTF8(FileName);
end;

function FileSetAttrUTF8(const Filename: String; Attr: longint): Longint;
begin
  Result:=LazFileUtils.FileSetAttrUTF8(FileName,Attr);
end;

function DeleteFileUTF8(const FileName: String): Boolean;
begin
  Result:=LazFileUtils.DeleteFileUTF8(FileName);
end;

function RenameFileUTF8(const OldName, NewName: String): Boolean;
begin
  Result:=LazFileUtils.RenameFileUTF8(OldName,NewName);
end;

function FileSearchUTF8(const Name, DirList: String): String;
begin
  Result:=LazFileUtils.FileSearchUTF8(Name,DirList);
end;

function FileIsReadOnlyUTF8(const FileName: String): Boolean;
begin
  Result:=LazFileUtils.FileIsReadOnlyUTF8(FileName);
end;

function GetCurrentDirUTF8: String;
begin
  Result:=LazFileUtils.GetCurrentDirUTF8;
end;

function SetCurrentDirUTF8(const NewDir: String): Boolean;
begin
  Result:=LazFileUtils.SetCurrentDirUTF8(NewDir);
end;

function CreateDirUTF8(const NewDir: String): Boolean;
begin
  Result:=LazFileUtils.CreateDirUTF8(NewDir);
end;

function RemoveDirUTF8(const Dir: String): Boolean;
begin
  Result:=LazFileUtils.RemoveDirUTF8(Dir);
end;

function ForceDirectoriesUTF8(const Dir: string): Boolean;
begin
  Result:=LazFileUtils.ForceDirectoriesUTF8(Dir);
end;

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
  fs: TFileStream;
begin
  if FileExistsUTF8(Filename) then begin
    try
      InvalidateFileStateCache(Filename);
      fs:=TFileStream.Create(UTF8ToSys(Filename),fmOpenWrite);
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

{------------------------------------------------------------------------------
  function ReadAllLinks(const Filename: string;
    ExceptionOnError: boolean): string;
 ------------------------------------------------------------------------------}
function ReadAllLinks(const Filename: string;
  ExceptionOnError: boolean): string;
{$IFNDEF WINDOWS}
var
  LinkFilename: string;
  AText: string;
  Depth: integer;
{$ENDIF}
begin
  Result:=Filename;
  {$IFDEF WINDOWS}

  {$ELSE}
  Depth:=0;
  while Depth<12 do begin
    inc(Depth);
    LinkFilename:=FpReadLink(Result);
    if LinkFilename='' then begin
      AText:='"'+Filename+'"';
      case fpGetErrno() of
      ESysEAcces:
        AText:='read access denied for '+AText;
      ESysENoEnt:
        AText:='a directory component in '+AText
                            +' does not exist or is a dangling symlink';
      ESysENotDir:
        AText:='a directory component in '+AText+' is not a directory';
      ESysENoMem:
        AText:='insufficient memory';
      ESysELoop:
        AText:=AText+' has a circular symbolic link';
      else
        // not a symbolic link, just a regular file
        exit;
      end;
      if (not ExceptionOnError) then begin
        Result:='';
        exit;
      end;
      raise EFOpenError.Create(AText);
    end else begin
      if not FilenameIsAbsolute(LinkFilename) then
        Result:=ExpandFileNameUTF8(ExtractFilePath(Result)+LinkFilename)
      else
        Result:=LinkFilename;
    end;
  end;
  // probably an endless loop
  if ExceptionOnError then
    raise EFOpenError.Create('too many links, maybe an endless loop.')
  else
    Result:='';
  {$ENDIF}
end;

function TryReadAllLinks(const Filename: string): string;
begin
  Result:=ReadAllLinks(Filename,false);
  if Result='' then
    Result:=Filename;
end;

{$IFDEF darwin}
function GetDarwinSystemFilename(Filename: string): string;
var
  s: CFStringRef;
  l: CFIndex;
begin
  if Filename='' then exit('');
  s:=CFStringCreateWithCString(nil,Pointer(Filename),kCFStringEncodingUTF8);
  l:=CFStringGetMaximumSizeOfFileSystemRepresentation(s);
  SetLength(Result,l);
  if Result<>'' then begin
    CFStringGetFileSystemRepresentation(s,@Result[1],length(Result));
    SetLength(Result,StrLen(PChar(Result)));
  end;
  CFRelease(s);
end;
{$ENDIF}

procedure CTIncreaseChangeStamp(var ChangeStamp: integer);
begin
  LazFileCache.LUIncreaseChangeStamp(ChangeStamp);
end;

procedure CTIncreaseChangeStamp64(var ChangeStamp: int64);
begin
  LazFileCache.LUIncreaseChangeStamp64(ChangeStamp);
end;

function FileExistsCached(const AFilename: string): boolean;
begin
  Result:=LazFileCache.FileExistsCached(AFilename);
end;

function DirPathExistsCached(const AFilename: string): boolean;
begin
  Result:=LazFileCache.DirPathExistsCached(AFilename);
end;

function DirectoryIsWritableCached(const ADirectoryName: string): boolean;
begin
  Result:=LazFileCache.DirectoryIsWritableCached(ADirectoryName);
end;

function FileIsExecutableCached(const AFilename: string): boolean;
begin
  Result:=LazFileCache.FileIsExecutableCached(AFilename);
end;

function FileIsReadableCached(const AFilename: string): boolean;
begin
  Result:=LazFileCache.FileIsReadableCached(AFilename);
end;

function FileIsWritableCached(const AFilename: string): boolean;
begin
  Result:=LazFileCache.FileIsWritableCached(AFilename);
end;

function FileIsTextCached(const AFilename: string): boolean;
begin
  Result:=LazFileCache.FileIsTextCached(AFilename);
end;

function FileAgeCached(const AFileName: string): Longint;
begin
  Result:=LazFileCache.FileAgeCached(AFileName);
end;

function GetFilenameOnDisk(const AFilename: string): string;
begin
  Result:=FindDiskFilename(AFilename);
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
  Base:=ExpandFileNameUTF8(AppendPathDelim(BasePath));
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
      CurPath:=ExpandFileNameUTF8(AppendPathDelim(CurPath));
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
        if CompareFilenamesIgnoreCase(ShortFilename,FileInfo.Name)=0 then begin
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
  Base:=ExpandFileNameUTF8(AppendPathDelim(BasePath));
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
      CurPath:=ExpandFileNameUTF8(AppendPathDelim(CurPath));
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
      if (NewCurDir<>CurDir) and (NewCurDir='') then
        NewCurDir:='.';
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
      if (Length(Result) > 0) and 
         (FindPathInSearchPath(@Result[StartPos],EndPos-StartPos, @Result[1],StartPos-1) <> nil)
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
  UseQuickCompare: Boolean;
  PathStr: String;
  CurFilename: String;
begin
  Result:=nil;
  if SearchPath=nil then exit;
  if (APath=nil) or (APathLen=0) then exit;
  // ignore trailing PathDelim at end
  while (APathLen>1) and (APath[APathLen-1]=PathDelim) do dec(APathLen);

  {$IFDEF CaseInsensitiveFilenames}
  UseQuickCompare:=false;
  {$ELSE}
    {$IFDEF NotLiteralFilenames}
    CmpPos:=0;
    while (CmpPos<APathLen) and (ord(APath[CmpPos]<128)) do inc(CmpPos);
    UseQuickCompare:=CmpPos=APathLen;
    {$ELSE}
    UseQuickCompare:=true;
    {$ENDIF}
  {$ENDIF}
  if not UseQuickCompare then begin
    SetLength(PathStr,APathLen);
    System.Move(APath^,PathStr[1],APathLen);
  end;

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
    if UseQuickCompare then begin
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
    end else if EndPos>StartPos then begin
      // use CompareFilenames
      CurFilename:='';
      SetLength(CurFilename,EndPos-StartPos);
      System.Move(SearchPath[StartPos],CurFilename[1],EndPos-StartPos);
      if CompareFilenames(PathStr,CurFilename)=0 then begin
        Result:=@SearchPath[StartPos];
        exit;
      end;
    end;
    StartPos:=NextStartPos+1;
  end;
end;

function ReadNextFPCParameter(const CmdLine: string; var Position: integer; out
  StartPos: integer): boolean;
begin
  StartPos:=Position;
  while (StartPos<=length(CmdLine)) and (CmdLine[StartPos] in [' ',#9,#10,#13]) do
    inc(StartPos);
  Position:=StartPos;
  while (Position<=length(CmdLine)) do begin
    case CmdLine[Position] of
    ' ',#9,#10,#13: break;
    '''':
      repeat
        inc(Position);
      until (Position>length(CmdLine)) or (CmdLine[Position]='''');
    '"':
      repeat
        inc(Position);
      until (Position>length(CmdLine)) or (CmdLine[Position]='''');
    end;
    inc(Position);
  end;
  Result:=StartPos<=length(CmdLine);
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
      if FindFirstUTF8(Base+FileMask,faAnyFile,FileInfo)=0 then
      begin
        repeat
          // check if special file
          if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='')
          then
            continue;
          if CompareFilenamesIgnoreCase(FileInfo.Name,ShortFile)=0 then begin
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
    Result:=Filename;
    exit;
  end;
  // check if filename absolute
  if FilenameIsAbsolute(Filename) then begin
    if SearchCase=ctsfcDefault then begin
      if FileExistsCached(Filename) then begin
        Result:=ExpandFileNameUTF8(Filename);
      end else begin
        Result:='';
      end;
    end else
      Result:=SearchFileInPath(ExtractFilename(Filename),
        ExtractFilePath(BasePath),'',';',SearchCase);
    exit;
  end;
  Base:=ExpandFileNameUTF8(AppendPathDelim(BasePath));
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
      CurPath:=ExpandFileNameUTF8(AppendPathDelim(CurPath));
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
  BracketMaskPos, BracketFilePos: integer;
  StopChar: LongInt;
  Fits: Boolean;

  function TryNextOr: boolean;
  begin
    Result:=false;
    if BracketMaskPos<1 then exit;
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
        exit(true);
      end;
    until false;
  end;

begin
  //debugln(['[FilenameIsMatching] Mask="',Mask,'" Filename="',Filename,'" MatchExactly=',MatchExactly]);
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
    //debugln('  Compare "',copy(Mask,DirStartMask,DirEndMask-DirStartMask),'"',
      // ' "',copy(Filename,DirStartFile,DirEndFile-DirStartFile),'"');
    // compare directories
    BracketMaskPos:=0;
    while (DirStartMask<DirEndMask) do begin
      //debugln(['FilenameIsMatching ',DirStartMask,' ',Mask[DirStartMask],' - ',DirStartFile,' ',Pchar(Filename)[DirStartFile-1]]);
      case Mask[DirStartMask] of
      '?':
        if DirStartFile<DirEndFile then begin
          inc(DirStartMask);
          inc(DirStartFile);
          continue;
        end else begin
          if not TryNextOr then exit;
        end;
      '*':
        begin
          inc(DirStartMask);
          Fits:=false;
          if (DirStartMask=DirEndMask) then begin
            Fits:=true;
          end else begin
            StopChar:=DirStartMask;
            if (BracketMaskPos>0) and (Mask[StopChar] in [',','}']) then begin
              while (StopChar<DirEndMask) and (Mask[StopChar]<>'}') do
                inc(StopChar);
              inc(StopChar);
            end;
            if StopChar>=DirEndMask then
              Fits:=true
            else begin
              while (DirStartFile<DirEndFile)
              and (not CharsEqual(Filename[DirStartFile],Mask[StopChar]))
              do
                inc(DirStartFile);
              Fits:=DirStartFile<DirEndFile;
            end;
          end;
          if (not Fits) and (not TryNextOr) then exit;
          continue;
        end;
      '{':
        if BracketMaskPos<1 then begin
          inc(DirStartMask);
          BracketMaskPos:=DirStartMask;
          BracketFilePos:=DirStartFile;
          continue;
        end else begin
          // treat as normal character
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
        end else begin
          // treat as normal character
        end;
      '}':
        if BracketMaskPos>0 then begin
          // Bracket operator fits complete
          inc(DirStartMask);
          BracketMaskPos:=0;
          continue;
        end else begin
          // treat as normal character
        end;
      end;
      if Mask[DirStartMask]=SpecialChar then begin
        // special char -> next char is normal char
        inc(DirStartMask);
        if (DirStartMask>=DirEndMask) then exit;
      end;
      // compare char
      if (DirStartFile<DirEndFile)
      and CharsEqual(Mask[DirStartMask],Filename[DirStartFile]) then begin
        inc(DirStartMask);
        inc(DirStartFile);
      end else begin
        // chars different
        if (BracketMaskPos=0) or (not TryNextOr) then begin
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

procedure InvalidateFileStateCache(const Filename: string = '');
begin
  LazFileCache.InvalidateFileStateCache(Filename);
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

function HasDelimitedItem(const List: string; Delimiter: char; FindItem: string
  ): boolean;
var
  p: Integer;
begin
  p:=1;
  Result:=FindNextDelimitedItem(List,Delimiter,p,FindItem)<>'';
end;

function FindNextDelimitedItem(const List: string; Delimiter: char;
  var Position: integer; FindItem: string): string;
begin
  while Position<=length(List) do begin
    Result:=GetNextDelimitedItem(List,Delimiter,Position);
    if Result=FindItem then exit;
  end;
  Result:='';
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

function DateToCfgStr(const Date: TDateTime): string;
begin
  try
    Result:=FormatDateTime(DateAsCfgStrFormat,Date);
  except
    Result:='';
  end;
  //debugln('DateToCfgStr "',Result,'"');
end;

function CfgStrToDate(const s: string; out Date: TDateTime): boolean;
var
  i: Integer;
  Year, Month, Day: word;
begin
  //debugln('CfgStrToDate "',s,'"');
  Result:=true;
  if length(s)<>length(DateAsCfgStrFormat) then begin
    Result:=false;
    exit;
  end;
  try
    Year:=0;
    Month:=0;
    Day:=0;
    for i:=1 to length(DateAsCfgStrFormat) do begin
      case DateAsCfgStrFormat[i] of
      'Y': Year:=Year*10+ord(s[i])-ord('0');
      'M': Month:=Month*10+ord(s[i])-ord('0');
      'D': Day:=Day*10+ord(s[i])-ord('0');
      end;
    end;
    Date:=EncodeDate(Year,Month,Day);
  except
    Result:=false;
  end;
end;

procedure DebugLn(Args: array of const);
begin
  DbgOut(Args);
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

procedure DbgOut(Args: array of const);
begin
  dbgout(dbgs(Args));
end;

procedure DbgOut(const s: string);
begin
  if Assigned(CTDbgOutEvent) then
    CTDbgOutEvent(s)
  else if TextRec(Output).Mode<>fmClosed then
    write(s);
end;

procedure DbgOut(const s1, s2: string);
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

function DbgS(const ms: TCustomMemoryStream; Count: PtrInt): string;
var
  OldPos: Int64;
begin
  if Count<0 then
    Count:=ms.Size-ms.Position;
  if Count=0 then
    exit('');
  OldPos:=ms.Position;
  try
    SetLength(Result,Count);
    ms.Read(Result[1],length(Result));
  finally
    ms.Position:=OldPos;
  end;
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

function dbgMemRange(P: System.PByte; Count: integer; Width: integer = 0): string;
const
  HexChars: array[0..15] of char = '0123456789ABCDEF';
  LineEnd: shortstring = LineEnding;
var
  i: Integer;
  NewLen: Integer;
  Dest: PChar;
  Col: Integer;
  j: Integer;
begin
  Result:='';
  if (p=nil) or (Count<=0) then exit;
  NewLen:=Count*2;
  if Width>0 then begin
    inc(NewLen,(Count div Width)*length(LineEnd));
  end;
  SetLength(Result,NewLen);
  Dest:=PChar(Result);
  Col:=1;
  for i:=0 to Count-1 do begin
    Dest^:=HexChars[PByte(P)[i] shr 4];
    inc(Dest);
    Dest^:=HexChars[PByte(P)[i] and $f];
    inc(Dest);
    inc(Col);
    if (Width>0) and (Col>Width) then begin
      Col:=1;
      for j:=1 to length(LineEnd) do begin
        Dest^:=LineEnd[j];
        inc(Dest);
      end;
    end;
  end;
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

function DbgStr(const StringWithSpecialChars: string; StartPos, Len: PtrInt
  ): string;
begin
  Result:=dbgstr(copy(StringWithSpecialChars,StartPos,Len));
end;

function MemSizeString(const s: string): PtrUInt;
begin
  Result:=LazDbgLog.MemSizeString(s);
end;

function MemSizeFPList(const List: TFPList): PtrUInt;
begin
  Result:=LazDbgLog.MemSizeFPList(List);
end;

function GetStringRefCount(const s: string): PtrInt;
begin
  Result:=LazDbgLog.GetStringRefCount(s);
end;

function CompareCTMemStat(Stat1, Stat2: TCTMemStat): integer;
begin
  Result:=SysUtils.CompareText(Stat1.Name,Stat2.Name);
end;

function CompareNameWithCTMemStat(KeyAnsiString: Pointer; Stat: TCTMemStat
  ): integer;
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
  Result:=ComparePointers(PCTLineInfoCacheItem(Data1)^.Addr,
                          PCTLineInfoCacheItem(Data2)^.Addr);
end;

function CompareAddrWithCTLineInfoCacheItem(Addr, Item: Pointer): integer;
begin
  Result:=ComparePointers(Addr,PCTLineInfoCacheItem(Item)^.Addr);
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


