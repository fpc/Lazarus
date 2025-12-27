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
    Caches for directories.
    The codetools work directory based, that means all define templates are the
    same for all files in a directory.
    That's why all the units in a directory use the same search paths and find
    the same files.
    
}
unit DirectoryCacher;

{$mode objfpc}{$H+}

{$IF FPC_FULLVERSION>30300}
  {$WARN 6018 off : Unreachable code of case else statement }
{$ENDIF}

interface

uses
  // RTL + FCL
  Classes, SysUtils, AVL_Tree,
  // CodeTools
  FileProcs,
  // LazUtils
  LazUTF8, LazFileCache, LazFileUtils, LazUtilities, LazStringUtils, LazDbgLog,
  AvgLvlTree;

// verbosity
{ $DEFINE CTDEBUG}
{ $DEFINE ShowTriedFiles}
{ $DEFINE ShowTriedUnits}
{ $DEFINE DebugDirCacheFindUnitSource}
{ $DEFINE DebugDirCacheFindIncFile}
{ $DEFINE VerboseFindNamespacedInc}

{$ifdef Windows}
{$define CaseInsensitiveFilenames}
{$endif}
{$IF defined(CaseInsensitiveFilenames) or defined(darwin)}
{$DEFINE NotLiteralFilenames}
{$ENDIF}

type
  TCTDirCacheString = (
    ctdcsUnitPath,
    ctdcsSrcPath,
    ctdcsIncludePath,
    ctdcsCompleteSrcPath, // including unit path, src path and compiled src paths
    ctdcsUnitLinks,
    ctdcsUnitSet,
    ctdcsFPCUnitPath,  // unit paths reported by FPC
    ctdcsNamespaces,
    ctdcsNamespacedIncludes // non empty = search include file via /namespaced/ parent folder
    );

  TCTDirCacheStringRecord = record
    Value: string;
    ConfigTimeStamp: integer;
  end;
  
  TCTDirectoryUnitSources = (
    ctdusUnitNormal, // e.g. AUnitName (case depends on OS) -> filename
    ctdusUnitCaseInsensitive, // AUnitName case insensitive -> filename
    ctdusInFilenameNormal, // unit 'in' filename -> filename
    ctdusInFilenameCaseInsensitive, // unit 'in' filename case insensitive -> filename
    ctdusUnitFileNormal, // AUnitName.ext (case depends on OS) -> filename
    ctdusUnitFileCaseInsensitive, // AUnitName.ext case insensitive -> filename
    ctdusPPUNormal, // UnitName (case depends on OS) => filename
    ctdusPPUCaseInsensitive // UnitName case insensitive => filename
    );

const
  ctdusCaseNormal      = [ctdusUnitNormal,
                          ctdusInFilenameNormal,
                          ctdusUnitFileNormal,
                          ctdusPPUNormal];
  ctdusCaseInsensitive = [ctdusUnitCaseInsensitive,
                          ctdusInFilenameCaseInsensitive,
                          ctdusUnitFileCaseInsensitive,
                          ctdusPPUCaseInsensitive];

type
  TCTStarDirectoryKind = (
    ctsdNone,
    ctsdStar,
    ctsdStarStar
    );
  TCTStarDirectoryKinds = set of TCTStarDirectoryKind;
const
  CTStarDirectoryKindNames: array[TCTStarDirectoryKind] of string = (
    'None',
    'Star',
    'StarStar'
    );

type

  { TUnitFileNameLink }

  TUnitFileNameLink = class
  public
    Unit_Name: string;
    Filename: string;
    function CalcMemSize: PtrUInt;
  end;

  TCTDirCacheUnitSrcRecord = record
    Files: TStringToStringTree;
    ConfigTimeStamp: integer;
    FileTimeStamp: integer;
  end;

  TCTDirectoryListingAttr = longint;
  PCTDirectoryListingAttr = ^TCTDirectoryListingAttr;
  TCTDirectoryListingSize = int64;
  PCTDirectoryListingSize = ^TCTDirectoryListingSize;

  TCTDirectoryListingHeader = packed record
    Time: TCTFileAgeTime;
    Attr: TCTDirectoryListingAttr;
    Size: TCTDirectoryListingSize;
  end;
  PCTDirectoryListingHeader = ^TCTDirectoryListingHeader;

  { TCTDirectoryListing }

  TCTDirectoryListing = class
  public
    FileTimeStamp: integer;
    Files: PChar; { each file: TCTDirectoryListingHeader+filename+#0
                    sorted: first case insensitive then sensitive }
    Count: integer; // number of filenames
    Size: PtrInt; // length of Files in bytes
    Starts: PInteger; // offsets of each file in Files
    destructor Destroy; override;
    procedure Clear;
    function CalcMemSize: PtrUInt;
    function GetFilename(Index: integer): PChar;
    function GetTime(Index: integer): TCTFileAgeTime;
    function GetAttr(Index: integer): TCTDirectoryListingAttr;
    function GetSize(Index: integer): TCTDirectoryListingSize;
  end;
  
  TCTOnIterateFile = procedure(const Filename: string) of object;
  TCTDirectoryCachePool = class;

  { TCTDirectoryBaseCache }

  TCTDirectoryBaseCache = class
  protected
    FDirectory: string;
    FPool: TCTDirectoryCachePool;
  public
    procedure CalcMemSize(Stats: TCTMemStats); virtual; abstract;
    function IndexOfFileCaseInsensitive(ShortFilename: PChar): integer; virtual; abstract; // ascii insensitive
    function IndexOfFileCaseSensitive(ShortFilename: PChar): integer; virtual; abstract;
    function FindFile(const ShortFilename: string;
                      const FileCase: TCTSearchFileCase): string; virtual; abstract;
    function FindIncludeFile(const IncFilename: string; AnyCase: boolean): string; virtual; abstract;
    function FindUnitSource(const AUnitName: string; AnyCase: boolean): string; virtual; abstract;
    procedure UpdateListing; virtual; abstract;
    property Directory: string read FDirectory; // with trailing pathdelim
    property Pool: TCTDirectoryCachePool read FPool;
  end;

  { TCTDirectoryCache }

  TCTDirectoryCache = class(TCTDirectoryBaseCache)
  private
    FRefCount: integer;
    FStrings: array[TCTDirCacheString] of TCTDirCacheStringRecord;
    FUnitLinksTree: TAVLTree; // tree of TUnitFileNameLink
    FUnitLinksTreeTimeStamp: integer;
    FListing: TCTDirectoryListing;
    FUnitSources: array[TCTDirectoryUnitSources] of TCTDirCacheUnitSrcRecord;
    function GetStrings(const AStringType: TCTDirCacheString): string;
    procedure SetStrings(const AStringType: TCTDirCacheString;
      const AValue: string);
    procedure ClearUnitLinks;
    function GetUnitSourceCacheValue(const UnitSrc: TCTDirectoryUnitSources;
                           const Search: string; var Filename: string): boolean;
    procedure AddToCache(const UnitSrc: TCTDirectoryUnitSources;
                         const Search, Filename: string);
  public
    constructor Create(const TheDirectory: string; ThePool: TCTDirectoryCachePool);
    destructor Destroy; override;
    procedure CalcMemSize(Stats: TCTMemStats); override;
    procedure Reference;
    procedure Release;
    function IndexOfFileCaseInsensitive(ShortFilename: PChar): integer; override;// ascii insensitive
    function IndexOfFileCaseSensitive(ShortFilename: PChar): integer; override;
    function FindFile(const ShortFilename: string;
                      const FileCase: TCTSearchFileCase): string; override;
    function FileAge(const ShortFilename: string): TCTFileAgeTime;
    function FileAttr(const ShortFilename: string): TCTDirectoryListingAttr;
    function FileSize(const ShortFilename: string): TCTDirectoryListingSize;
    // unit link (for fpc ppu files to fpc src file)
    function FindUnitLink(const AUnitName: string): string;
    function FindUnitInUnitSet(const AUnitName: string; SrcSearchRequiresPPU: boolean = true): string;
    // find unit source
    function FindUnitSource(const AUnitName: string; AnyCase: boolean): string; override;
    function FindUnitSourceInCleanSearchPath(const AUnitName,
                                  SearchPath: string; AnyCase: boolean): string; // search in unitpath
    function FindUnitSourceInCompletePath(var AUnitName, InFilename: string; // search in unitpath and unitpaths of output dirs
               AnyCase: boolean; FPCSrcSearchRequiresPPU: boolean = false;
               const AddNameSpaces: string = ''; WithNamespaces: boolean = true): string;
    // find ppu/dcu file
    function FindCompiledUnitInUnitSet(const AUnitName: string): string;
    function FindCompiledUnitInCompletePath(const AnUnitname: string; AnyCase: boolean): string;
    // include files
    function FindIncludeFile(const IncFilename: string; AnyCase: boolean): string; override;
    function FindIncludeFileInPath(IncFilename: string; AnyCase: boolean): string;
    function FindIncludeFileInCleanPath(IncFilename, SearchPath: string; AnyCase: boolean): string;
    function FindNamespacedIncludeFile(const IncFilename: string): string;

    procedure IterateFPCUnitsInSet(const Iterate: TCTOnIterateFile);
    procedure UpdateListing; override;
    procedure WriteListing;
    procedure Invalidate; inline;
    procedure GetFiles(var Files: TStrings; IncludeDirs: boolean = true); // relative to Directory
  public
    property RefCount: integer read FRefCount;
    property Strings[const AStringType: TCTDirCacheString]: string read GetStrings write SetStrings;
    property Listing: TCTDirectoryListing read FListing;
  end;

  { TCTStarDirectoryCache - a cache for a directory and its sub directories, e.g. searching in '/foo/**' }

  TCTStarDirectoryCache = class(TCTDirectoryBaseCache)
  private
    FKind: TCTStarDirectoryKind;
  public
    type
      TListingPosition = integer;
      TListingHeader = packed record
        SubDirIndex: TListingPosition;
      end;
      PListingHeader = ^TListingHeader;

      { TListing }

      TListing = class
        Files: PChar; // all files. Each TListingHeader+FilenameWithoutPath+#0
        Count: TListingPosition;
        Starts: PInteger; // Count offsets in Files
        Size: PtrInt; // length of Files in bytes
        SubDirs: TStringListUTF8Fast; // subdirectories, e.g. 'sub', 'sub/foo'
        FileTimeStamp: integer;
        constructor Create;
        destructor Destroy; override;
        procedure Clear;
        function CalcMemSize: PtrUInt;
        function GetShortFilename(Index: integer): PChar;
        function GetSubDir(Index: integer): String;
        function GetSubDirIndex(Index: integer): TListingPosition; // -1 is top lvl
        function GetSubDirFilename(Index: integer): String; // subdir+Filename
      end;
  private
    FListing: TListing;
  public
    constructor Create(const TheDirectory: string; TheKind: TCTStarDirectoryKind;
                       ThePool: TCTDirectoryCachePool);
    destructor Destroy; override;
    procedure CalcMemSize(Stats: TCTMemStats); override;
    function FindFile(const ShortFilename: string;
      const FileCase: TCTSearchFileCase): string; override; // returns relative filename
    function FindIncludeFile(const IncFilename: string; AnyCase: boolean): string; override; // returns relative filename
    function FindUnitSource(const AUnitName: string; AnyCase: boolean): string; override; // returns relative filename
    function IndexOfFileCaseInsensitive(ShortFilename: PChar): integer; override; // ascii insensitive
    function IndexOfFileCaseSensitive(ShortFilename: PChar): integer; override;
    procedure UpdateListing; override;
    procedure WriteListing;
    procedure Invalidate; inline;
  public
    property Kind: TCTStarDirectoryKind read FKind;
    property Listing: TListing read FListing;
  end;
  
  { TCTDirectoryCachePool }
  
  TCTDirCacheGetString = function(const ADirectory: string;
                                  const AStringType: TCTDirCacheString
                                  ): string of object;
  TCTDirCacheFindVirtualFile = function(const Filename: string): string of object;
  TCTGetUnitFromSet = function(const UnitSet, AnUnitName: string;
                               SrcSearchRequiresPPU: boolean): string of object;
  TCTGetCompiledUnitFromSet = function(const UnitSet, AnUnitName: string): string of object;
  TCTIterateFPCUnitsFromSet = procedure(const UnitSet: string;
                                     const Iterate: TCTOnIterateFile) of object;

  TCTDirectoryCachePool = class
  private
    FConfigTimeStamp: integer;
    FFileTimeStamp: integer;
    FDirectories: TAVLTree;// tree of TCTDirectoryCache
    FStarDirectories: array[TCTStarDirectoryKind] of TAVLTree;// trees of TCTStarDirectoryCache
    FOnFindVirtualFile: TCTDirCacheFindVirtualFile;
    FOnGetCompiledUnitFromSet: TCTGetCompiledUnitFromSet;
    FOnGetString: TCTDirCacheGetString;
    FOnGetUnitFromSet: TCTGetUnitFromSet;
    FOnIterateFPCUnitsFromSet: TCTIterateFPCUnitsFromSet;
    FStarDirectoryExcludes: TStrings;
    procedure DoRemove(ACache: TCTDirectoryCache);
    procedure OnFileStateCacheChangeTimeStamp(Sender: TObject;
                                              const AFilename: string);
    procedure SetStarDirectoryExcludes(const AValue: TStrings);
  public
    constructor Create;
    destructor Destroy; override;
    procedure CalcMemSize(Stats: TCTMemStats);
    procedure GetListing(const aDirectory: string; var Files: TStrings;
                         IncludeDirs: boolean = true); // relative to Directory
    function GetCache(const Directory: string;
                      CreateIfNotExists: boolean = true;
                      DoReference: boolean = true): TCTDirectoryCache;
    // the star caches use the above GetCache
    function GetStarCache(const Directory: string; Kind: TCTStarDirectoryKind;
                      CreateIfNotExists: boolean = true): TCTStarDirectoryCache;
    function GetBaseCache(const Directory: string;
                      CreateIfNotExists: boolean = true): TCTDirectoryBaseCache;
    function GetString(const Directory: string; AStringType: TCTDirCacheString;
                       UseCache: boolean = true): string;
    procedure IncreaseFileTimeStamp; inline;
    procedure IncreaseConfigTimeStamp; inline;
    function FileExists(Filename: string): boolean; overload;
    function FileExists(Filename: string; FileCase: TCTSearchFileCase): boolean; overload;
    function FileAge(Filename: string): TCTFileAgeTime;
    function FileAttr(Filename: string): TCTDirectoryListingAttr;
    function FileSize(Filename: string): TCTDirectoryListingSize;
    function FindUnitInUnitLinks(const Directory, AUnitName: string): string;
    function FindUnitInUnitSet(const Directory, AUnitName: string): string;
    function FindCompiledUnitInUnitSet(const Directory, AUnitName: string): string;
    procedure IterateFPCUnitsInSet(const Directory: string;
                                   const Iterate: TCTOnIterateFile);
    function FindDiskFilename(const Filename: string;
                              {%H-}SearchCaseInsensitive: boolean = false): string; // using Pascal case insensitivity, not UTF-8
    function FindIncludeFileInDirectory(Directory, IncFileName: string;
                                        AnyCase: boolean = false): string;
    function FindIncludeFileInCompletePath(Directory, IncFilename: string;
                                           AnyCase: boolean = false): string;
    function FindUnitInDirectory(const Directory, AUnitName: string;
                                 AnyCase: boolean = false): string;
    function FindVirtualFile(const Filename: string): string;
    function FindVirtualInclude(const Filename: string): string;
    function FindVirtualUnit(const AUnitName: string): string;
    function FindUnitSourceInCompletePath(const Directory: string;
                                          var AUnitName, InFilename: string;
                                          AnyCase: boolean = false): string;
    function FindCompiledUnitInCompletePath(const Directory: string;
                                            var AnUnitname: string;
                                            AnyCase: boolean = false): string;
    function FindCompiledUnitInPath(const BaseDirectory, UnitPath, AnUnitname: string;
                                    AnyCase: boolean = false): string; // result is not cached!
    property FileTimeStamp: integer read FFileTimeStamp;
    property ConfigTimeStamp: integer read FConfigTimeStamp;
    property OnGetString: TCTDirCacheGetString read FOnGetString write FOnGetString;
    property OnFindVirtualFile: TCTDirCacheFindVirtualFile read FOnFindVirtualFile
                                                   write FOnFindVirtualFile;
    property OnGetUnitFromSet: TCTGetUnitFromSet read FOnGetUnitFromSet
                                                 write FOnGetUnitFromSet;
    property OnGetCompiledUnitFromSet: TCTGetCompiledUnitFromSet
                 read FOnGetCompiledUnitFromSet write FOnGetCompiledUnitFromSet;
    property OnIterateFPCUnitsFromSet: TCTIterateFPCUnitsFromSet
                 read FOnIterateFPCUnitsFromSet write FOnIterateFPCUnitsFromSet;
    property StarDirectoryExcludes: TStrings read FStarDirectoryExcludes write SetStarDirectoryExcludes;
  end;
  
function CompareCTDirectoryCaches(Data1, Data2: Pointer): integer;
function CompareAnsiStringAndDirectoryCache(Dir, Cache: Pointer): integer;

function ComparePCharFirstCaseInsAThenCase(Data1, Data2: Pointer): integer; // insensitive ASCII then byte wise
function ComparePCharCaseInsensitiveASCII(Data1, Data2: Pointer): integer; // insensitive ASCII
function ComparePCharCaseSensitive(Data1, Data2: Pointer): integer; // byte wise

// star directories
function IsCTStarDirectory(const Directory: string;
  out p: integer  // returns position of pathdelim before the *
  ): TCTStarDirectoryKind;
function CompareCTStarDirectoryCaches(Data1, Data2: Pointer): integer;
function CompareAnsiStringAndStarDirectoryCache(Dir, Cache: Pointer): integer;

// unit links
function SearchUnitInUnitLinks(const UnitLinks, TheUnitName: string;
  var UnitLinkStart, UnitLinkEnd: integer; out Filename: string): boolean;
function CreateUnitLinksTree(const UnitLinks: string): TAVLTree; // tree of TUnitFileNameLink
function CompareUnitLinkNodes(NodeData1, NodeData2: Pointer): integer;
function CompareUnitNameWithUnitLinkNode(AUnitName: Pointer;
  NodeData: pointer): integer;

implementation

const
  DirListNameOffset = SizeOf(TCTDirectoryListingHeader);
type
  TWorkFileInfo = record
    Header: TCTDirectoryListingHeader;
    FileName: string;
  end;
  PWorkFileInfo = ^TWorkFileInfo;
  PPWorkFileInfo = ^PWorkFileInfo;

  TWorkStarFileInfo = record
    Header: TCTStarDirectoryCache.TListingHeader;
    FileName: string;
  end;
  PWorkStarFileInfo = ^TWorkStarFileInfo;
  PPWorkStarFileInfo = ^PWorkStarFileInfo;

function CompareWorkFileInfos(Data1, Data2: Pointer): integer;
var
  Info1: PWorkFileInfo absolute Data1;
  Info2: PWorkFileInfo absolute Data2;
begin
  Result:=ComparePCharFirstCaseInsAThenCase(PChar(Info1^.Filename),PChar(Info2^.Filename));
end;

function CompareWorkStarFileInfos(Data1, Data2: Pointer): integer;
var
  Info1: PWorkStarFileInfo absolute Data1;
  Info2: PWorkStarFileInfo absolute Data2;
begin
  Result:=ComparePCharFirstCaseInsAThenCase(PChar(Info1^.Filename),PChar(Info2^.Filename));
end;

function CompareCTDirectoryCaches(Data1, Data2: Pointer): integer;
var
  Dir1: TCTDirectoryCache absolute Data1;
  Dir2: TCTDirectoryCache absolute Data2;
begin
  Result:=CompareFilenames(Dir1.FDirectory,Dir2.FDirectory);
end;

function CompareAnsiStringAndDirectoryCache(Dir, Cache: Pointer): integer;
var
  Directory: AnsiString absolute Dir;
  DirCache: TCTDirectoryCache absolute Cache;
begin
  Result:=CompareFilenames(Directory,DirCache.FDirectory);
end;

function CompareCTStarDirectoryCaches(Data1, Data2: Pointer): integer;
var
  Dir1: TCTStarDirectoryCache absolute Data1;
  Dir2: TCTStarDirectoryCache absolute Data2;
begin
  Result:=CompareFilenames(Dir1.FDirectory,Dir2.FDirectory);
end;

function CompareAnsiStringAndStarDirectoryCache(Dir, Cache: Pointer): integer;
var
  Directory: AnsiString absolute Dir;
  DirCache: TCTStarDirectoryCache absolute Cache;
begin
  Result:=CompareFilenames(Directory,DirCache.FDirectory);
end;

function ComparePCharFirstCaseInsAThenCase(Data1, Data2: Pointer): integer;
begin
  Result:=ComparePCharCaseInsensitiveASCII(Data1,Data2);
  if Result=0 then
    Result:=ComparePCharCaseSensitive(Data1,Data2);
end;

function ComparePCharCaseInsensitiveASCII(Data1, Data2: Pointer): integer;
var
  p1: PChar absolute Data1;
  p2: PChar absolute Data2;
begin
  while (FPUpChars[p1^]=FPUpChars[p2^]) and (p1^<>#0) do begin
    inc(p1);
    inc(p2);
  end;
  Result:=ord(FPUpChars[p1^])-ord(FPUpChars[p2^]);
end;

function ComparePCharCaseInsensitiveASCII(Data1, Data2: Pointer; MaxCount: PtrInt): integer;
var
  p1: PChar absolute Data1;
  p2: PChar absolute Data2;
begin
  while (MaxCount>0) and  (FPUpChars[p1^]=FPUpChars[p2^]) and (p1^<>#0) do begin
    inc(p1);
    inc(p2);
    dec(MaxCount);
  end;
  if MaxCount=0 then
    Result:=0
  else
    Result:=ord(FPUpChars[p1^])-ord(FPUpChars[p2^]);
end;

function ComparePCharCaseSensitive(Data1, Data2: Pointer): integer;
var
  p1: PChar absolute Data1;
  p2: PChar absolute Data2;
begin
  while (p1^=p2^) and (p1^<>#0) do begin
    inc(p1);
    inc(p2);
  end;
  Result:=ord(p1^)-ord(p2^);
end;

function ComparePCharUnitNameWithFilename(UnitNameP, FilenameP: Pointer): integer;
{ Checks if UnitNameP is a dotted prefix of FilenameP.
  For example:
    a.b is prefix of a.B, a.b.c.d, A.b.c, a.b.c
      but not of a.bc
}
var
  AUnitName: PChar absolute UnitNameP;
  Filename: PChar absolute FilenameP;
  cu: Char;
  cf: Char;
begin
  repeat
    cu:=FPUpChars[AUnitName^];
    cf:=FPUpChars[Filename^];
    if cu=#0 then begin
      // the unit name fits the start of the file name
      if cf in [#0,'.'] then
        Result:=0
      else
        Result:=ord('.')-ord(cf);
      exit;
    end;
    if cu=cf then begin
      inc(AUnitName);
      inc(Filename);
    end else begin
      Result:=ord(cu)-ord(cf);
      exit;
    end;
  until false;
end;

function CheckLoUpCase(Find, Candidate: PChar; MaxCount: PtrInt): boolean;
var
  i: PtrInt;
  CurFind, CurCandidate: PChar;
  c: Char;
begin
  // check case sensitive
  CurFind:=Find;
  CurCandidate:=Candidate;
  i:=0;
  repeat
    if i=MaxCount then exit(true);
    if (CurFind^<>CurCandidate^) then break;
    if CurFind^=#0 then exit(true);
    inc(i);
    inc(CurFind);
    inc(CurCandidate);
  until false;

  // check lowercase Find
  CurFind:=Find;
  CurCandidate:=Candidate;
  i:=0;
  repeat
    if i=MaxCount then exit(true);
    c:=CurFind^;
    case c of
    'A'..'Z':
      if ord(c)+32<>ord(CurCandidate^) then break;
    else if c<>CurCandidate^ then break;
    end;
    if CurFind^=#0 then exit(true);
    inc(i);
    inc(CurFind);
    inc(CurCandidate);
  until false;

  // check uppercase Find
  CurFind:=Find;
  CurCandidate:=Candidate;
  i:=0;
  repeat
    if i=MaxCount then exit(true);
    if (FPUpChars[CurFind^]<>CurCandidate^) then break;
    if CurFind^=#0 then exit(true);
    inc(i);
    inc(CurFind);
    inc(CurCandidate);
  until false;

  Result:=false;
end;

function IsCTStarDirectory(const Directory: string; out p: integer
  ): TCTStarDirectoryKind;
var
  EndPos: SizeInt;
begin
  Result:=ctsdNone;
  p:=0;
  EndPos:=length(Directory);
  if EndPos<4 then exit;
  if Directory[EndPos]=PathDelim then
    dec(EndPos);
  if Directory[EndPos]<>'*' then exit;
  dec(EndPos);
  if Directory[EndPos]='*' then begin
    Result:=ctsdStarStar;
    dec(EndPos);
  end else
    Result:=ctsdStar;
  if (EndPos>0) and (Directory[EndPos]=PathDelim) then begin
    p:=EndPos;
    exit;
  end;
  Result:=ctsdNone;
end;

function SearchUnitInUnitLinks(const UnitLinks, TheUnitName: string;
  var UnitLinkStart, UnitLinkEnd: integer; out Filename: string): boolean;
var
  UnitLinkLen: integer;
  pe: TCTPascalExtType;
  AliasFilename: String;
begin
  Result:=false;
  Filename:='';
  if TheUnitName='' then exit;
  {$IFDEF ShowTriedFiles}
  DebugLn(['SearchUnitInUnitLinks length(UnitLinks)=',length(UnitLinks)]);
  {$ENDIF}
  if UnitLinkStart<1 then
    UnitLinkStart:=1;
  while UnitLinkStart<=length(UnitLinks) do begin
    while (UnitLinkStart<=length(UnitLinks))
    and (UnitLinks[UnitLinkStart] in [#10,#13]) do
      inc(UnitLinkStart);
    UnitLinkEnd:=UnitLinkStart;
    while (UnitLinkEnd<=length(UnitLinks)) and (UnitLinks[UnitLinkEnd]<>' ')
    do
      inc(UnitLinkEnd);
    UnitLinkLen:=UnitLinkEnd-UnitLinkStart;
    if UnitLinkLen>0 then begin
      {$IFDEF ShowTriedFiles}
      DebugLn(['  unit "',copy(UnitLinks,UnitLinkStart,UnitLinkEnd-UnitLinkStart),'" ',
        ComparePCharCaseInsensitiveA(Pointer(TheUnitName),@UnitLinks[UnitLinkStart],UnitLinkLen)]);
      {$ENDIF}
      if (UnitLinkLen=length(TheUnitName))
      and (ComparePCharCaseInsensitiveASCII(Pointer(TheUnitName),@UnitLinks[UnitLinkStart],
           UnitLinkLen)=0)
      then begin
        // unit found -> parse filename
        UnitLinkStart:=UnitLinkEnd+1;
        UnitLinkEnd:=UnitLinkStart;
        while (UnitLinkEnd<=length(UnitLinks))
        and (not (UnitLinks[UnitLinkEnd] in [#10,#13])) do
          inc(UnitLinkEnd);
        if UnitLinkEnd>UnitLinkStart then begin
          Filename:=copy(UnitLinks,UnitLinkStart,UnitLinkEnd-UnitLinkStart);
          if FileExistsCached(Filename) then begin
            Result:=true;
            exit;
          end;
          // try also different extensions
          for pe:=Low(TCTPascalExtType) to High(TCTPascalExtType) do begin
            if not FilenameExtIs(Filename,CTPascalExtension[pe]) then
            begin
              AliasFilename:=ChangeFileExt(Filename,'.pas');
              if FileExistsCached(AliasFilename) then begin
                Filename:=AliasFilename;
                Result:=true;
                exit;
              end;
            end;
          end;
        end;
        UnitLinkStart:=UnitLinkEnd;
      end else begin
        UnitLinkStart:=UnitLinkEnd+1;
        while (UnitLinkStart<=length(UnitLinks))
        and (not (UnitLinks[UnitLinkStart] in [#10,#13])) do
          inc(UnitLinkStart);
      end;
    end else
      break;
  end;
end;

function CreateUnitLinksTree(const UnitLinks: string): TAVLTree;
var
  UnitLinksTree: TAVLTree;
  UnitLinkLen: integer;
  UnitLinkStart: Integer;
  UnitLinkEnd: Integer;
  TheUnitName: String;
  Filename: String;
  NewNode: TUnitFileNameLink;
begin
  UnitLinksTree:=TAVLTree.Create(@CompareUnitLinkNodes);
  UnitLinkStart:=1;
  while UnitLinkStart<=length(UnitLinks) do begin
    while (UnitLinkStart<=length(UnitLinks))
    and (UnitLinks[UnitLinkStart] in [#10,#13]) do
      inc(UnitLinkStart);
    UnitLinkEnd:=UnitLinkStart;
    while (UnitLinkEnd<=length(UnitLinks)) and (UnitLinks[UnitLinkEnd]<>' ')
    do
      inc(UnitLinkEnd);
    UnitLinkLen:=UnitLinkEnd-UnitLinkStart;
    if UnitLinkLen>0 then begin
      TheUnitName:=copy(UnitLinks,UnitLinkStart,UnitLinkLen);
      if IsValidIdent(TheUnitName,true,true) then begin
        UnitLinkStart:=UnitLinkEnd+1;
        UnitLinkEnd:=UnitLinkStart;
        while (UnitLinkEnd<=length(UnitLinks))
        and (not (UnitLinks[UnitLinkEnd] in [#10,#13])) do
          inc(UnitLinkEnd);
        if UnitLinkEnd>UnitLinkStart then begin
          Filename:=copy(UnitLinks,UnitLinkStart,UnitLinkEnd-UnitLinkStart);
          NewNode:=TUnitFileNameLink.Create;
          NewNode.Unit_Name:=TheUnitName;
          NewNode.Filename:=Filename;
          UnitLinksTree.Add(NewNode);
        end;
        UnitLinkStart:=UnitLinkEnd;
      end else begin
        UnitLinkStart:=UnitLinkEnd+1;
        while (UnitLinkStart<=length(UnitLinks))
        and (not (UnitLinks[UnitLinkStart] in [#10,#13])) do
          inc(UnitLinkStart);
      end;
    end else
      break;
  end;
  Result:=UnitLinksTree;
end;

function CompareUnitLinkNodes(NodeData1, NodeData2: pointer): integer;
var Link1, Link2: TUnitFileNameLink;
begin
  Link1:=TUnitFileNameLink(NodeData1);
  Link2:=TUnitFileNameLink(NodeData2);
  Result:=CompareText(Link1.Unit_Name,Link2.Unit_Name);
end;

function CompareUnitNameWithUnitLinkNode(AUnitName: Pointer; NodeData: pointer): integer;
begin
  Result:=CompareText(String(AUnitName),TUnitFileNameLink(NodeData).Unit_Name);
end;

{ TCTDirectoryCache }

function TCTDirectoryCache.GetStrings(const AStringType: TCTDirCacheString): string;
begin
  //if AStringType=ctdcsUnitPath then DebugLn(['TCTDirectoryCache.GetStrings ctdcsUnitPath ',Directory,' ',FStrings[AStringType].ConfigTimeStamp,' ',Pool.ConfigTimeStamp]);
  if FStrings[AStringType].ConfigTimeStamp<>Pool.ConfigTimeStamp then begin
    Strings[AStringType]:=Pool.GetString(Directory,AStringType,false);
  end;
  Result:=FStrings[AStringType].Value;
end;

procedure TCTDirectoryCache.SetStrings(const AStringType: TCTDirCacheString;
  const AValue: string);
begin
  FStrings[AStringType].Value:=AValue;
  FStrings[AStringType].ConfigTimeStamp:=Pool.ConfigTimeStamp;
end;

procedure TCTDirectoryCache.ClearUnitLinks;
begin
  if FUnitLinksTree=nil then exit;
  FUnitLinksTree.FreeAndClear;
  FUnitLinksTree.Free;
  FUnitLinksTree:=nil
end;

procedure TCTDirectoryCache.UpdateListing;
var
  WorkingListing: PWorkFileInfo;
  WorkingListingCapacity, WorkingListingCount: integer;
  WorkingItem: PWorkFileInfo;
  FileInfo: TSearchRec;
  TotalLen: Integer;
  i: Integer;
  p: PChar;
  CurFilenameLen: Integer;
  NewCapacity: Integer;
  SortMap: PPWorkFileInfo;
begin
  if FListing.FileTimeStamp=Pool.FileTimeStamp then exit;
  FListing.Clear;
  FListing.FileTimeStamp:=Pool.FileTimeStamp;
  if not FilenameIsAbsolute(Directory) then
    exit;// virtual directory
  
  // Note: do not add a 'if not DirectoryExistsUTF8 then exit'.
  // This will not work on automounted directories. You must use FindFirstUTF8.

  // read the directory
  WorkingListing:=nil;
  WorkingListingCapacity:=0;
  WorkingListingCount:=0;
  SortMap:=nil;
  try
    if FindFirstUTF8(Directory+FileMask,faAnyFile,FileInfo)=0 then begin
      repeat
        // check if special file
        if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='')
        then
          continue;
        // add file
        if WorkingListingCount=WorkingListingCapacity then begin
          // grow WorkingListing
          if WorkingListingCapacity>0 then
            NewCapacity:=WorkingListingCapacity*2
          else
            NewCapacity:=64;
          ReAllocMem(WorkingListing,SizeOf(TWorkFileInfo)*NewCapacity);
          FillByte(WorkingListing[WorkingListingCount],
                   SizeOf(TWorkFileInfo)*(NewCapacity-WorkingListingCapacity),0);
          WorkingListingCapacity:=NewCapacity;
        end;
        WorkingItem:=@WorkingListing[WorkingListingCount];
        WorkingItem^.Header.Time:=FileInfo.UniversalTime;
        WorkingItem^.Header.Attr:=FileInfo.Attr;
        WorkingItem^.Header.Size:=FileInfo.Size;
        WorkingItem^.FileName:=FileInfo.Name;
        inc(WorkingListingCount);
      until FindNextUTF8(FileInfo)<>0;
    end;
    FindCloseUTF8(FileInfo);

    if WorkingListingCount=0 then exit;

    // sort the files
    GetMem(SortMap,WorkingListingCount*SizeOf(Pointer));
    for i:=0 to WorkingListingCount-1 do
      SortMap[i]:=@WorkingListing[i];
    MergeSortWithLen(PPointer(SortMap),WorkingListingCount,@CompareWorkFileInfos);

    // create listing
    TotalLen:=0;
    for i:=0 to WorkingListingCount-1 do
      inc(TotalLen,length(WorkingListing[i].FileName)+1+SizeOf(TCTDirectoryListingHeader));
    GetMem(FListing.Files,TotalLen);
    FListing.Size:=TotalLen;
    FListing.Count:=WorkingListingCount;
    GetMem(FListing.Starts,SizeOf(Integer)*WorkingListingCount);
    p:=FListing.Files;
    for i:=0 to WorkingListingCount-1 do begin
      FListing.Starts[i]:=p-FListing.Files;
      WorkingItem:=SortMap[i];
      PCTDirectoryListingHeader(p)^:=WorkingItem^.Header;
      inc(p,SizeOf(TCTDirectoryListingHeader));
      // filename
      CurFilenameLen:=length(WorkingItem^.FileName);
      if CurFilenameLen>0 then begin
        System.Move(WorkingItem^.FileName[1],p^,CurFilenameLen);
        inc(p,CurFilenameLen);
      end;
      p^:=#0;
      inc(p);
    end;
  finally
    ReAllocMem(SortMap,0);
    for i:=0 to WorkingListingCount-1 do
      WorkingListing[i].FileName:='';
    ReAllocMem(WorkingListing,0);
  end;
end;

function TCTDirectoryCache.GetUnitSourceCacheValue(
  const UnitSrc: TCTDirectoryUnitSources; const Search: string;
  var Filename: string): boolean;
var
  Files: TStringToStringTree;
begin
  //debugln(['TCTDirectoryCache.GetUnitSourceCacheValue START ',UnitSrc,' Search=',Search]);
  Files:=FUnitSources[UnitSrc].Files;
  if (FUnitSources[UnitSrc].FileTimeStamp<>Pool.FileTimeStamp)
  or (FUnitSources[UnitSrc].ConfigTimeStamp<>Pool.ConfigTimeStamp) then begin
    // cache is invalid -> clear to make it valid
    if Files<>nil then
      Files.Clear;
    FUnitSources[UnitSrc].FileTimeStamp:=Pool.FileTimeStamp;
    FUnitSources[UnitSrc].ConfigTimeStamp:=Pool.ConfigTimeStamp;
    Result:=false;
  end else begin
    // cache is valid
    if Files<>nil then begin
      Result:=Files.GetString(Search,Filename);
    end else begin
      Result:=false;
    end;
  end;
  //debugln(['TCTDirectoryCache.GetUnitSourceCacheValue END ',UnitSrc,' Search=',Search,' Result=',Result,' Filename=',Filename]);
end;

procedure TCTDirectoryCache.AddToCache(const UnitSrc: TCTDirectoryUnitSources;
  const Search, Filename: string);
var
  Files: TStringToStringTree;
  CaseSensitive: Boolean;
begin
  Files:=FUnitSources[UnitSrc].Files;
  if Files=nil then begin
    if UnitSrc in [ctdusUnitNormal,ctdusPPUNormal] then
      CaseSensitive:=FilenamesCaseSensitive
    else
      CaseSensitive:=UnitSrc in ctdusCaseNormal;
    Files:=TFilenameToStringTree.Create(CaseSensitive);
    FUnitSources[UnitSrc].Files:=Files;
  end;
  Files[Search]:=Filename;
end;

constructor TCTDirectoryCache.Create(const TheDirectory: string;
  ThePool: TCTDirectoryCachePool);
begin
  FDirectory:=TrimFilename(TheDirectory);
  if FDirectory='.' then
    FDirectory:=''
  else
    FDirectory:=AppendPathDelim(FDirectory);
  if (FDirectory<>'') and not FilenameIsAbsolute(FDirectory) then
    // bug: caller forgot to expand filename
    raise Exception.Create('directory not absolute "'+FDirectory+'"');
  FListing:=TCTDirectoryListing.Create;
  FPool:=ThePool;
  FRefCount:=1;
end;

destructor TCTDirectoryCache.Destroy;
var
  UnitSrc: TCTDirectoryUnitSources;
begin
  ClearUnitLinks;
  if Pool<>nil then Pool.DoRemove(Self);
  FreeAndNil(FListing);
  for UnitSrc:=Low(TCTDirectoryUnitSources) to High(TCTDirectoryUnitSources) do
    FreeAndNil(FUnitSources[UnitSrc].Files);
  inherited Destroy;
end;

procedure TCTDirectoryCache.CalcMemSize(Stats: TCTMemStats);
var
  cs: TCTDirCacheString;
  us: TCTDirectoryUnitSources;
  Node: TAVLTreeNode;
  m: PtrUInt;
begin
  Stats.Add('TCTDirectoryCache',PtrUInt(InstanceSize)
    +MemSizeString(FDirectory));

  m:=0;
  for cs:=Low(FStrings) to high(FStrings) do begin
    inc(m,SizeOf(TCTDirCacheStringRecord));
    inc(m,MemSizeString(FStrings[cs].Value));
  end;
  Stats.Add('TCTDirectoryCache.FStrings',m);

  m:=0;
  for us:=Low(FUnitSources) to high(FUnitSources) do begin
    inc(m,SizeOf(TCTDirectoryUnitSources));
    if FUnitSources[us].Files<>nil then
      inc(m,FUnitSources[us].Files.CalcMemSize);
  end;
  Stats.Add('TCTDirectoryCache.FUnitSources',m);

  if FUnitLinksTree<>nil then begin
    m:=PtrUInt(FUnitLinksTree.InstanceSize)
      +SizeOf(TAVLTreeNode)*PtrUInt(FUnitLinksTree.Count);
    Node:=FUnitLinksTree.FindLowest;
    while Node<>nil do begin
      inc(m,TUnitFileNameLink(Node.Data).CalcMemSize);
      Node:=FUnitLinksTree.FindSuccessor(Node);
    end;
    Stats.Add('TCTDirectoryCache.FUnitLinksTree',m);
  end;

  if FListing<>nil then
    Stats.Add('TCTDirectoryCache.FListing',FListing.CalcMemSize);
end;

procedure TCTDirectoryCache.Reference;
begin
  inc(FRefCount);
end;

procedure TCTDirectoryCache.Release;
begin
  if FRefCount<=0 then
    raise Exception.Create('TCTDirectoryCache.Release');
  dec(FRefCount);
  if FRefCount=0 then Free;
end;

function TCTDirectoryCache.IndexOfFileCaseInsensitive(ShortFilename: PChar): integer;
var
  Files: PChar;
  l: Integer;
  r: Integer;
  m: Integer;
  CurFilename: PChar;
  cmp: Integer;
begin
  Result:=-1;
  UpdateListing;
  Files:=FListing.Files;
  if Files=nil then exit;
  l:=0;
  r:=FListing.Count-1;
  while l<=r do begin
    m:=(l+r) shr 1;
    CurFilename:=@Files[FListing.Starts[m]+DirListNameOffset];
    cmp:=ComparePCharCaseInsensitiveASCII(ShortFilename,CurFilename);
    if cmp>0 then
      l:=m+1
    else if cmp<0 then
      r:=m-1
    else begin
      // found
      Result:=m;
      // continue search for lower index
      r:=m-1;
    end;
  end;
end;

function TCTDirectoryCache.IndexOfFileCaseSensitive(ShortFilename: PChar): integer;
var
  Files: PChar;
  l: Integer;
  r: Integer;
  m: Integer;
  CurFilename: PChar;
  cmp: Integer;
begin
  Result:=-1;
  UpdateListing;
  Files:=FListing.Files;
  if Files=nil then exit;
  l:=0;
  r:=FListing.Count-1;
  while l<=r do begin
    m:=(l+r) shr 1;
    CurFilename:=@Files[FListing.Starts[m]+DirListNameOffset];
    cmp:=ComparePCharFirstCaseInsAThenCase(ShortFilename,CurFilename);// pointer type cast avoids #0 check
    if cmp>0 then
      l:=m+1
    else if cmp<0 then
      r:=m-1
    else begin
      // found
      Result:=m;
      // continue search for lower index
      r:=m-1;
    end;
  end;
end;

function TCTDirectoryCache.FindUnitLink(const AUnitName: string): string;
var
  Node: TAVLTreeNode;
  Link: TUnitFileNameLink;
  AliasFilename: String;
  pe: TCTPascalExtType;
begin
  if (FUnitLinksTree=nil) or (FUnitLinksTreeTimeStamp<>Pool.FileTimeStamp) then
  begin
    ClearUnitLinks;
    FUnitLinksTreeTimeStamp:=Pool.FileTimeStamp;
    FUnitLinksTree:=CreateUnitLinksTree(Strings[ctdcsUnitLinks]);
  end;
  Node:=FUnitLinksTree.FindKey(Pointer(AUnitName),
                               @CompareUnitNameWithUnitLinkNode);
  if Node<>nil then begin
    Link:=TUnitFileNameLink(Node.Data);
    Result:=Link.Filename;
    if FileExistsCached(Result) then begin
      exit;
    end;
    // try different extensions too
    for pe:=Low(TCTPascalExtType) to High(TCTPascalExtType) do begin
      if not FilenameExtIs(Result,CTPascalExtension[pe]) then
      begin
        AliasFilename:=ChangeFileExt(Result,CTPascalExtension[pe]);
        if FileExistsCached(AliasFilename) then begin
          Link.Filename:=AliasFilename;
          Result:=AliasFilename;
          exit;
        end;
      end;
    end;
  end;
  Result:='';
end;

function TCTDirectoryCache.FindUnitInUnitSet(const AUnitName: string;
  SrcSearchRequiresPPU: boolean): string;
var
  UnitSet: string;
begin
  UnitSet:=Strings[ctdcsUnitSet];
  //debugln(['TCTDirectoryCache.FindUnitInUnitSet Directory="',Directory,'" UnitSet="',UnitSet,'" AUnitName="',AUnitName,'"']);
  Result:=Pool.OnGetUnitFromSet(UnitSet,AUnitName,SrcSearchRequiresPPU);
  //debugln(['TCTDirectoryCache.FindUnitInUnitSet Directory="',Directory,'" UnitSet="',dbgstr(UnitSet),'" AUnitName="',AUnitName,'" Result="',Result,'"']);
end;

function TCTDirectoryCache.FindCompiledUnitInUnitSet(const AUnitName: string
  ): string;
var
  UnitSet: string;
begin
  UnitSet:=Strings[ctdcsUnitSet];
  //debugln(['TCTDirectoryCache.FindCompiledUnitInUnitSet Directory="',Directory,'" UnitSet="',UnitSet,'" AUnitName="',AUnitName,'"']);
  Result:=Pool.OnGetCompiledUnitFromSet(UnitSet,AUnitName);
  //debugln(['TCTDirectoryCache.FindCompiledUnitInUnitSet Directory="',Directory,'" UnitSet="',dbgstr(UnitSet),'" AUnitName="',AUnitName,'" Result="',Result,'"']);
end;

function TCTDirectoryCache.FindFile(const ShortFilename: string;
  const FileCase: TCTSearchFileCase): string;
  
  procedure RaiseDontKnow;
  begin
    raise Exception.Create('do not know FileCase '+IntToStr(ord(FileCase)));
  end;
  
var
  i: Integer;
begin
  Result:='';
  i:=0;
  if ShortFilename='' then exit;
  if Directory<>'' then begin
    case FileCase of
    ctsfcDefault:
      {$IFDEF CaseInsensitiveFilenames}
      i:=IndexOfFileCaseInsensitive(Pointer(ShortFilename));// pointer type cast avoids #0 check
      {$ELSE}
      begin
        i:=IndexOfFileCaseSensitive(Pointer(ShortFilename));// pointer type cast avoids #0 check
        // just return the parameter
        if i>=0 then
          Result:=ShortFilename;
        exit;
      end;
      {$ENDIF}
    ctsfcAllCase,ctsfcLoUpCase:
      i:=IndexOfFileCaseInsensitive(Pointer(ShortFilename));// pointer type cast avoids #0 check
    else RaiseDontKnow;
    end;
    if i>=0 then
      Result:=FListing.GetFilename(i);
  end else begin
    // this is a virtual directory
    Result:=Pool.FindVirtualFile(ShortFilename);
  end;
end;

function TCTDirectoryCache.FindIncludeFile(const IncFilename: string;
  AnyCase: boolean): string;
var
  Files, CurFilename, IncExtP, CurExtP, IncFilenameP: PChar;
  Starts: PInteger;
  l, r, m, first, cmp, Best: Integer;
  AUnitName: String;
  Stop: Boolean;
  Ext, BestExt: TCTPascalIncExtType;
begin
  Result:='';
  {$IFDEF DebugDirCacheFindIncFile}
  //if (CompareText(AUnitName,DebugUnitName)=0) and (System.Pos(DebugDirPart,directory)>0) then
    DebugLn('TCTDirectoryCache.FindIncludeFile IncName="',IncFilename,'" AnyCase=',dbgs(AnyCase),' Directory=',Directory);
  {$ENDIF}
  {$IFDEF CaseInsensitiveFilenames}
  AnyCase:=true;
  {$ENDIF}
  if IncFilename='' then exit;
  if Directory<>'' then begin
    UpdateListing;
    Files:=FListing.Files;
    if Files=nil then exit;
    Starts:=FListing.Starts;

    // see fpc source scanner.pas function preproc_factor(eval: Boolean):texprvalue;
    // first search IncFilename
    // if IncFilename has not an ext of .inc, .pp, .pas, then search IncFilename plus .inc,.pp,.pas
    // Note: This means e.g. "a.b" will search "a.b", "a.b.inc", "a.b.pp" and "a.b.pas"

    IncFilenameP:=PChar(IncFilename);
    l:=length(IncFilename);
    while (l>0) and (IncFilename[l]<>'.') do dec(l);
    if l>0 then begin
      IncExtP:=@IncFilename[l];
      Ext:=IsPascalIncExt(IncExtP);
      if Ext>pietNone then
        AUnitName:=LeftStr(IncFilename,l-1)
      else begin
        IncExtP:=nil;
        AUnitName:=IncFilename;
      end;
    end else begin
      IncExtP:=nil;
      AUnitName:=IncFilename;
    end;

    // binary search the lowest filename matching the AUnitName
    {$IFDEF DebugDirCacheFindIncFile}
    //if (CompareText(AUnitName,DebugUnitName)=0) and (System.Pos(DebugDirPart,directory)>0) then
    //  WriteListing;
    {$ENDIF}
    l:=0;
    r:=FListing.Count-1;
    first:=-1;
    while l<=r do begin
      m:=(l+r) shr 1;
      CurFilename:=@Files[Starts[m]+DirListNameOffset];
      cmp:=ComparePCharUnitNameWithFilename(Pointer(AUnitName),CurFilename);
      if cmp>0 then
        l:=m+1
      else if cmp<0 then
        r:=m-1
      else begin
        first:=m;
        r:=m-1;
      end;
    end;
    if first<0 then exit;
    m:=first;

    // -> now find a filename with correct case and extension
    Best:=-1;
    BestExt:=high(TCTPascalIncExtType);
    while m<FListing.Count do begin
      CurFilename:=@Files[Starts[m]+DirListNameOffset];
      // check if filename has the right AUnitName prefix
      if (ComparePCharUnitNameWithFilename(Pointer(AUnitName),CurFilename)<>0)
      then
        break;

      // check if the filename fits
      CurExtP:=CurFilename+length(AUnitname);
      {$IFDEF DebugDirCacheFindIncFile}
      //if (CompareText(AUnitName,DebugUnitName)=0) and (System.Pos(DebugDirPart,directory)>0) then
        DebugLn('TCTDirectoryCache.FindIncludeFile NEXT "',CurFilename,'" ExtStart=',dbgstr(CurExtP^));
      {$ENDIF}
      Stop:=false;
      if IncExtP<>nil then begin
        // include file with extension
        if AnyCase then begin
          if ComparePCharCaseInsensitiveASCII(CurExtP,IncExtP)=0 then
            // any case with extension fits -> can't get any better
            Stop:=true;
        end else if CheckLoUpCase(IncFilenameP,CurFilename,length(IncFilename)+1) then
          Stop:=true; // mixed case with extension fits -> can't get any better
      end else begin
        // include file without extension -> search without and with default extension
        if (CurExtP^=#0) then begin
          if AnyCase or CheckLoUpCase(IncFilenameP,CurFilename,length(IncFilename)+1) then
            // file without extension fits a file without extension -> can't get any better
            Stop:=true;
        end else begin
          Ext:=IsPascalIncExt(CurExtP);
          if Ext>pietNone then begin
            // file without extension fits an include file with extension
            // Note: the compiler prefers file.inc over file.pas
            if (Best<0) or (BestExt>Ext) then begin
              if AnyCase or CheckLoUpCase(IncFilenameP,CurFilename,length(IncFilename)) then begin
                Best:=m;
                BestExt:=Ext;
              end;
            end;
          end;
        end;
      end;
      if Stop then begin
        Best:=m;
        break;
      end;
      inc(m);
    end;
    if Best>=0 then begin
      CurFilename:=@Files[Starts[Best]+DirListNameOffset];
      Result:=CurFilename;
      exit;
    end;

    {$IFDEF DebugDirCacheFindIncFile}
    if m<FListing.Count then
      //if (CompareText(AUnitName,DebugUnitName)=0) and (System.Pos(DebugDirPart,directory)>0) then
        DebugLn('TCTDirectoryCache.FindIncludeFile LAST ',CurFilename);
    {$ENDIF}
  end else begin
    // this is a virtual directory
    Result:=Pool.FindVirtualInclude(IncFilename);
    if Result<>'' then exit;
  end;
  Result:='';
end;

function TCTDirectoryCache.FindIncludeFileInPath(IncFilename: string;
  AnyCase: boolean): string;
var
  HasPathDelims: Boolean;
  SearchPath: String;
begin
  Result:='';
  {$IFDEF DebugDirCacheFindIncFile}
  //if (CompareText(AUnitName,DebugUnitName)=0) and (System.Pos(DebugDirPart,directory)>0) then
    DebugLn('TCTDirectoryCache.FindIncludeFileInPath IncName="',IncFilename,'" AnyCase=',dbgs(AnyCase),' Directory=',Directory);
  {$ENDIF}
  if IncFilename='' then exit;

  IncFilename:=ResolveDots(IncFilename);

  HasPathDelims:=(System.Pos('/',IncFilename)>0) or (System.Pos('\',IncFilename)>0);
  if HasPathDelims then begin
    Result:=Pool.FindIncludeFileInCompletePath(Directory,IncFilename,AnyCase);
    exit;
  end;

  SearchPath:=Strings[ctdcsIncludePath];
  Result:=FindIncludeFileInCleanPath(IncFilename,SearchPath,AnyCase);

  if (Result='') and (Strings[ctdcsNamespacedIncludes]<>'') then begin
    Result:=FindNamespacedIncludeFile(IncFilename);
  end;
end;

function TCTDirectoryCache.FindIncludeFileInCleanPath(IncFilename,
  SearchPath: string; AnyCase: boolean): string;
var
  StartPos, p: Integer;
  l: SizeInt;
  CurPath: String;
  IsAbsolute, HasPathDelims: Boolean;
begin
  Result:='';
  HasPathDelims:=(System.Pos('/',IncFilename)>0) or (System.Pos('\',IncFilename)>0);
  if HasPathDelims then
    exit;

  StartPos:=1;
  l:=length(SearchPath);
  while StartPos<=l do begin
    p:=StartPos;
    while (p<=l) and (SearchPath[p]<>';') do inc(p);
    CurPath:=Trim(copy(SearchPath,StartPos,p-StartPos));
    if CurPath<>'' then begin
      IsAbsolute:=FilenameIsAbsolute(CurPath);
      if (not IsAbsolute) and (Directory<>'') then begin
        CurPath:=Directory+CurPath;
        IsAbsolute:=true;
      end;
      //DebugLn('TCTDirectoryCache.FindIncludeFileInCleanPath CurPath="',CurPath,'"');
      if IsAbsolute then begin
        CurPath:=AppendPathDelim(CurPath);
        Result:=Pool.FindIncludeFileInDirectory(CurPath,IncFilename,AnyCase);
      end else if (CurPath='.') and (Directory='') then
        Result:=Pool.FindVirtualInclude(IncFilename)
      else
        Result:='';
      if Result<>'' then exit;
    end;
    StartPos:=p+1;
  end;
  Result:='';
end;

function TCTDirectoryCache.FindNamespacedIncludeFile(const IncFilename: string
  ): string;
// if Directory contains a '/namespaced/' then search IncFilename in sibling folders
// e.g. Directory='/home/user/fpcsrc/rtl/namespaced/windows/', IncFilename='wintypes.pp'
// search it in /home/user/fpcsrc/rtl/**
const
  NamespacedDir = PathDelim+'namespaced'+PathDelim;

  function Traverse(Cache: TCTDirectoryCache; Lvl: integer): string;
  var
    i: Integer;
    Dir: string;
    CurListing: TCTDirectoryListing;
    ChildCache: TCTDirectoryCache;
  begin
    Result:='';
    Dir:=ExtractFilename(Cache.Directory);
    if SameText(Dir,'backup') then exit;

    Result:=Cache.FindIncludeFile(IncFilename,True);
    if Result<>'' then begin
      Result:=Cache.Directory+Result;
      {$IFDEF VerboseFindNamespacedInc}
      debugln(['TCTDirectoryCache.FindNamespacedIncludeFile.Traverse ',Cache.Directory,' Inc="',IncFilename,'" Result="',Result,'"']);
      {$ENDIF}
      exit;
    end;
    if Lvl>4 then exit;
    inc(Lvl);

    CurListing:=Cache.Listing;
    for i:=0 to CurListing.Count-1 do begin
      if CurListing.GetAttr(i) and faDirectory=0 then continue;
      Dir:=Cache.Directory+CurListing.GetFilename(i);
      ChildCache:=Pool.GetCache(Dir,true,false);
      Result:=Traverse(ChildCache,Lvl);
      if Result<>'' then exit;
    end;
  end;

var
  p: SizeInt;
  Dir, SubDir: String;
  Cache: TCTDirectoryCache;
begin
  Result:='';
  if Pos(PathDelim,IncFilename)>0 then exit;

  p:=Pos(NamespacedDir,Directory);
  if p<1 then exit;
  {$IFDEF VerboseFindNamespacedInc}
  debugln(['TCTDirectoryCache.FindNamespacedIncludeFile ',Directory,' Inc="',IncFilename,'"']);
  {$ENDIF}
  Dir:=LeftStr(Directory,p);
  SubDir:=copy(Directory,p+length(NamespacedDir),length(Directory));
  if SubDir<>'' then begin
    // first search in same subdir aka the directory without /namespaced/
    Result:=Dir+SubDir+IncFilename;
    if Pool.FileExists(Result) then begin
      {$IFDEF VerboseFindNamespacedInc}
      debugln(['TCTDirectoryCache.FindNamespacedIncludeFile ',Directory,' Inc="',IncFilename,'" Result="',Result,'"']);
      {$ENDIF}
      exit;
    end;
  end;

  // then search in subdir 'src'
  Result:=Dir+'src'+IncFilename;
  if Pool.FileExists(Result) then begin
    {$IFDEF VerboseFindNamespacedInc}
    debugln(['TCTDirectoryCache.FindNamespacedIncludeFile ',Directory,' Inc="',IncFilename,'" Result="',Result,'"']);
    {$ENDIF}
    exit;
  end;

  // finally search recursively
  {$IFDEF VerboseFindNamespacedInc}
  debugln(['TCTDirectoryCache.FindNamespacedIncludeFile Dir=',Dir,' SubDir="',SubDir,'"']);
  {$ENDIF}

  Cache:=Pool.GetCache(Dir,true,false);
  Result:=Traverse(Cache,0);
end;

function TCTDirectoryCache.FileAge(const ShortFilename: string): TCTFileAgeTime;
var
  i: Integer;
begin
  Result:=-1;
  if ShortFilename='' then exit;
  if Directory='' then begin
    // this is a virtual directory
    exit;
  end;
  {$IFDEF CaseInsensitiveFilenames}
  i:=IndexOfFileCaseInsensitive(Pointer(ShortFilename));// pointer type cast avoids #0 check
  {$ELSE}
  i:=IndexOfFileCaseSensitive(Pointer(ShortFilename));// pointer type cast avoids #0 check
  {$ENDIF}
  if i>=0 then
    Result:=FListing.GetTime(i);
end;

function TCTDirectoryCache.FileAttr(const ShortFilename: string): TCTDirectoryListingAttr;
var
  i: Integer;
begin
  Result:=0;
  if ShortFilename='' then exit;
  if Directory='' then begin
    // this is a virtual directory
    exit;
  end;
  {$IFDEF CaseInsensitiveFilenames}
  i:=IndexOfFileCaseInsensitive(Pointer(ShortFilename));// pointer type cast avoids #0 check
  {$ELSE}
  i:=IndexOfFileCaseSensitive(Pointer(ShortFilename));// pointer type cast avoids #0 check
  {$ENDIF}
  if i>=0 then
    Result:=FListing.GetAttr(i);
end;

function TCTDirectoryCache.FileSize(const ShortFilename: string): TCTDirectoryListingSize;
var
  i: Integer;
begin
  Result:=-1;
  if ShortFilename='' then exit;
  if Directory='' then begin
    // this is a virtual directory
    exit;
  end;
  {$IFDEF CaseInsensitiveFilenames}
  i:=IndexOfFileCaseInsensitive(Pointer(ShortFilename));// pointer type cast avoids #0 check
  {$ELSE}
  i:=IndexOfFileCaseSensitive(Pointer(ShortFilename));// pointer type cast avoids #0 check
  {$ENDIF}
  if i>=0 then
    Result:=FListing.GetSize(i);
end;

function TCTDirectoryCache.FindUnitSource(const AUnitName: string;
  AnyCase: boolean): string;
{$IFDEF DebugDirCacheFindUnitSource}
const
  DebugUnitName = 'IDEDialogs';
  DebugDirPart = 'ideintf';
{$ENDIF}
var
  l, r, m, first: Integer;
  cmp: LongInt;
  CurFilename: PChar;
  Files: PChar;
  ExtStartPos: PChar;
  Starts: PInteger;
begin
  Result:='';
  {$IFDEF DebugDirCacheFindUnitSource}
  if (CompareText(AUnitName,DebugUnitName)=0) and (System.Pos(DebugDirPart,directory)>0) then
    DebugLn('TCTDirectoryCache.FindUnitSource AUnitName="',AUnitName,'" AnyCase=',dbgs(AnyCase),' Directory=',Directory);
  {$ENDIF}
  if AUnitName='' then exit;
  if Directory<>'' then begin
    UpdateListing;
    Files:=FListing.Files;
    if Files=nil then exit;
    Starts:=FListing.Starts;
    // binary search the lowest filename matching the unitname
    {$IFDEF DebugDirCacheFindUnitSource}
    if (CompareText(AUnitName,DebugUnitName)=0) and (System.Pos(DebugDirPart,directory)>0) then
      WriteListing;
    {$ENDIF}
    l:=0;
    r:=FListing.Count-1;
    first:=-1;
    while l<=r do begin
      m:=(l+r) shr 1;
      CurFilename:=@Files[Starts[m]+DirListNameOffset];
      cmp:=ComparePCharUnitNameWithFilename(Pointer(AUnitName),CurFilename);
      if cmp>0 then
        l:=m+1
      else if cmp<0 then
        r:=m-1
      else begin
        first:=m;
        r:=m-1;
      end;
    end;
    if first<0 then exit;
    m:=first;

    // -> now find a filename with correct case and extension
    while m<FListing.Count do begin
      CurFilename:=@Files[Starts[m]+DirListNameOffset];
      // check if filename has the right AUnitName prefix
      if (ComparePCharUnitNameWithFilename(Pointer(AUnitName),CurFilename)<>0)
      then
        break;

      // check if the filename fits
      ExtStartPos:=CurFilename+length(AUnitname);
      {$IFDEF DebugDirCacheFindUnitSource}
      if (CompareText(AUnitName,DebugUnitName)=0) and (System.Pos(DebugDirPart,directory)>0) then
        DebugLn('TCTDirectoryCache.FindUnitSource NEXT "',CurFilename,'" ExtStart=',dbgstr(ExtStartPos^));
      {$ENDIF}
      if IsPascalUnitExt(ExtStartPos) then begin
        // the extension is ok
        Result:=CurFilename;
        {$IFDEF DebugDirCacheFindUnitSource}
        if (CompareText(AUnitName,DebugUnitName)=0) and (System.Pos(DebugDirPart,directory)>0) then
          DebugLn('TCTDirectoryCache.FindUnitSource CHECKING CASE "',CurFilename,'"');
        {$ENDIF}
        if AnyCase then begin
          exit;
        end else begin
          // check case platform dependent
          {$IFDEF CaseInsensitiveFilenames}
          exit;
          {$ELSE}
          if (ExtractFileNameOnly(Result)=AUnitName)
          or (Result=lowercase(Result))
          or (Result=uppercase(Result)) then
            exit;
          {$ENDIF}
        end;
      end;
      inc(m);
    end;
    {$IFDEF DebugDirCacheFindUnitSource}
    if m<FListing.Count then
      if (CompareText(AUnitName,DebugUnitName)=0) and (System.Pos(DebugDirPart,directory)>0) then
        DebugLn('TCTDirectoryCache.FindUnitSource LAST ',CurFilename);
    {$ENDIF}
  end else begin
    // this is a virtual directory
    Result:=Pool.FindVirtualUnit(AUnitName);
    if Result<>'' then exit;
  end;
  Result:='';
end;

function TCTDirectoryCache.FindUnitSourceInCleanSearchPath(const AUnitName,
  SearchPath: string; AnyCase: boolean): string;
var
  p, StartPos, l: integer;
  CurPath: string;
  IsAbsolute: Boolean;
begin
  //if (CompareText(AUnitName,'UnitDependencies')=0) then
  //  DebugLn('TCTDirectoryCache.FindUnitSourceInCleanSearchPath AUnitName="',AUnitName,'" SearchPath="',SearchPath,'"');
  StartPos:=1;
  l:=length(SearchPath);
  while StartPos<=l do begin
    p:=StartPos;
    while (p<=l) and (SearchPath[p]<>';') do inc(p);
    CurPath:=Trim(copy(SearchPath,StartPos,p-StartPos));
    if CurPath<>'' then begin
      IsAbsolute:=FilenameIsAbsolute(CurPath);
      if (not IsAbsolute) and (Directory<>'') then begin
        CurPath:=Directory+CurPath;
        IsAbsolute:=true;
      end;
      //DebugLn('TCTDirectoryCache.FindUnitSourceInCleanSearchPath CurPath="',CurPath,'"');
      if IsAbsolute then begin
        CurPath:=AppendPathDelim(CurPath);
        Result:=Pool.FindUnitInDirectory(CurPath,AUnitName,AnyCase);
      end else if (CurPath='.') and (Directory='') then
        Result:=Pool.FindVirtualUnit(AUnitname)
      else
        Result:='';
      if Result<>'' then exit;
    end;
    StartPos:=p+1;
  end;
  Result:='';
end;

function TCTDirectoryCache.FindUnitSourceInCompletePath(var AUnitName, InFilename: string;
  AnyCase: boolean; FPCSrcSearchRequiresPPU: boolean; const AddNameSpaces: string;
  WithNamespaces: boolean): string;

  function FindInFilenameLowUp(aFilename: string): string;
  begin
    if AnyCase then
      Result:=Pool.FindDiskFilename(aFilename,true)
    else begin
      Result:=aFilename;
      if FileExistsCached(Result) then exit;
      {$IFNDEF CaseInsensitiveFilenames}
      Result:=ExtractFilePath(aFilename)+lowercase(ExtractFileName(aFilename));
      if FileExistsCached(Result) then exit;
      Result:=ExtractFilePath(aFilename)+uppercase(ExtractFileName(aFilename));
      if FileExistsCached(Result) then exit;
      {$ENDIF}
      Result:='';
    end;
  end;

  function FindInFilename(aFilename: string): string;
  var
    Ext: String;
  begin
    Result:='';
    if not FilenameIsAbsolute(aFilename) then
      exit;
    Ext:=ExtractFileExt(aFilename);
    if Ext='' then
      aFilename:=aFilename+'.pp'; // append default extension
    Result:=FindInFilenameLowUp(aFilename);
    if Result='' then begin
      if (Ext<>'') then exit;
      // search for secondary extension
      aFilename:=ChangeFileExt(aFilename,'.pas');
      Result:=FindInFilenameLowUp(aFilename);
      if Result='' then exit;
    end;
    InFilename:=CreateRelativePath(Result,Directory);
  end;

var
  UnitSrc: TCTDirectoryUnitSources;
  CurDir: String;
  SrcPath: string;
  NewUnitName, aNameSpace, aName, NameSpaces: String;
  p: SizeInt;
begin
  Result:='';
  {$IFDEF ShowTriedUnits}
  DebugLn('TCTDirectoryCache.FindUnitSourceInCompletePath AUnitName="',AUnitname,'" InFilename="',InFilename,'" Directory="',Directory,'"',BoolToStr(AddNameSpaces<>'',' ExtraNameSpaces="'+AddNameSpaces+'"',''));
  {$ENDIF}
  if InFilename<>'' then begin
    // uses IN parameter
    InFilename:=TrimFilename(GetForcedPathDelims(InFilename));
    if AnyCase then
      UnitSrc:=ctdusInFilenameCaseInsensitive
    else
      UnitSrc:=ctdusInFilenameNormal;
    if GetUnitSourceCacheValue(UnitSrc,InFilename,Result) then begin
      // found in cache
      if Result<>'' then begin
        // unit found
        if Directory<>'' then
          InFilename:=CreateRelativePath(Result,Directory);
      end else begin
        // unit not found
      end;
    end else begin
      // not found in cache -> search
      if FilenameIsAbsolute(InFilename) then begin
        // absolute filename
        Result:=FindInFilename(InFilename);
      end else begin
        // 'in'-filename has no complete path
        // -> search file relative to current directory
        CurDir:=Directory;
        if CurDir<>'' then begin
          Result:=FindInFilename(TrimFilename(CurDir+InFilename));
        end else begin
          // this is a virtual directory -> search virtual unit
          InFilename:=Pool.FindVirtualFile(InFilename);
          Result:=InFilename;
        end;
      end;
      AddToCache(UnitSrc,InFilename,Result);
    end;
  end else begin
    // normal unit name

    if WithNamespaces then begin
      NameSpaces:=MergeWithDelimiter(Strings[ctdcsNamespaces],AddNameSpaces,';');
      if NameSpaces<>'' then begin
        // search with additional namespaces, separated by semicolon
        //debugln(['TCTDirectoryCache.FindUnitSourceInCompletePath NameSpaces="',NameSpaces,'"']);
        repeat
          p:=Pos(';',NameSpaces);
          if p>0 then begin
            aNameSpace:=LeftStr(NameSpaces,p-1);
            Delete(NameSpaces,1,p);
          end else begin
            aNameSpace:=NameSpaces;
            NameSpaces:='';
          end;
          if IsValidIdent(aNameSpace,true,true) then begin
            aName:=aNameSpace+'.'+AUnitName;
            Result:=FindUnitSourceInCompletePath(aName,InFilename,AnyCase,
              FPCSrcSearchRequiresPPU,'',false);
            if Result<>'' then begin
              AUnitName:=RightStr(aName,length(aName)-length(aNameSpace)-1);
              exit;
            end;
          end;
        until NameSpaces='';
      end;
    end;

    if AnyCase then
      UnitSrc:=ctdusUnitCaseInsensitive
    else
      UnitSrc:=ctdusUnitNormal;
    if GetUnitSourceCacheValue(UnitSrc,AUnitName,Result) then begin
      // found in cache
      if Result<>'' then begin
        // unit found
      end else begin
        // unit not found
      end;
    end else begin
      // not found in cache -> search in complete source path

      if Directory='' then begin
        // virtual directory => search virtual unit
        Result:=Pool.FindVirtualUnit(AUnitName);
      end else begin
        // search in current directory
        Result:=FindUnitSource(AUnitName,AnyCase);
        if Result<>'' then
          Result:=Directory+Result;
      end;
      if Result='' then begin
        // search in search path
        SrcPath:=Strings[ctdcsCompleteSrcPath];
        Result:=FindUnitSourceInCleanSearchPath(AUnitName,SrcPath,AnyCase);
      end;
      if Result='' then begin
        // search in unit set
        {$IFDEF ShowTriedUnits}
        DebugLn(['TCTDirectoryCache.FindUnitSourceInCompletePath unit ',AUnitName,' not found in SrcPath="',SrcPath,'"  Directory="',Directory,'" searching in unitset ...']);
        {$ENDIF}
        Result:=FindUnitInUnitSet(AUnitName,FPCSrcSearchRequiresPPU);
        {$IFDEF ShowTriedUnits}
        if Result='' then begin
          DebugLn(['TCTDirectoryCache.FindUnitSourceInCompletePath unit ',AUnitName,' not found in unitlinks. Directory="',Directory,'"']);
        end;
        {$ENDIF}
      end;

      if AddNameSpaces='' then
        AddToCache(UnitSrc,AUnitName,Result);
    end;
    if Result<>'' then begin
      // improve unit name
      NewUnitName:=ExtractFileNameOnly(Result);
      if (NewUnitName<>lowercase(NewUnitName))
      and (AUnitName<>NewUnitName) then
        AUnitName:=NewUnitName;
    end;
  end;
  //DebugLn('TCTDirectoryCache.FindUnitSourceInCompletePath RESULT AUnitName="',AUnitName,'" InFilename="',InFilename,'" Result=',Result);
end;

function TCTDirectoryCache.FindCompiledUnitInCompletePath(
  const AnUnitname: string; AnyCase: boolean): string;
var
  UnitPath: string;
  UnitSrc: TCTDirectoryUnitSources;
begin
  Result:='';
  if AnyCase then
    UnitSrc:=ctdusPPUCaseInsensitive
  else
    UnitSrc:=ctdusPPUNormal;
  if GetUnitSourceCacheValue(UnitSrc,AnUnitname,Result) then begin
    //if AnUnitName='lazmkunit.ppu' then
    //  debugln(['TCTDirectoryCache.FindCompiledUnitInCompletePath cached ',Result]);
    // found in cache
    if Result<>'' then begin
      // unit found
    end else begin
      // unit not found
    end;
    //debugln(['TCTDirectoryCache.FindCompiledUnitInCompletePath Cached AnUnitname="',AnUnitname,'" Result="',Result,'"']);
  end else begin
    // not found in cache -> search

    // search in unit path
    UnitPath:=Strings[ctdcsUnitPath];
    Result:=Pool.FindCompiledUnitInPath(Directory,UnitPath,AnUnitname,AnyCase);
    //if AnUnitName='lazmkunit.ppu' then
    //  debugln(['TCTDirectoryCache.FindCompiledUnitInCompletePath CurDir="',Directory,'" UnitPath="',UnitPath,'" AnUnitname="',AnUnitname,'" Result=',Result]);
    if Result='' then begin
      // search in unit set
      Result:=FindCompiledUnitInUnitSet(AnUnitname);
    end;
    //if (Result='') then debugln(['TCTDirectoryCache.FindCompiledUnitInCompletePath CurDir="',Directory,'" UnitPath="',UnitPath,'" AnUnitname="',AnUnitname,'" Result=',Result]);

    AddToCache(UnitSrc,AnUnitname,Result);
  end;
end;

procedure TCTDirectoryCache.IterateFPCUnitsInSet(const Iterate: TCTOnIterateFile);
var
  UnitSet: string;
begin
  UnitSet:=Strings[ctdcsUnitSet];
  Pool.OnIterateFPCUnitsFromSet(UnitSet,Iterate);
end;

procedure TCTDirectoryCache.WriteListing;
var
  i: Integer;
  Filename: PChar;
begin
  writeln('TCTDirectoryCache.WriteListing Count=',FListing.Count,' Size=',FListing.Size);
  for i:=0 to FListing.Count-1 do begin
    Filename:=@FListing.Files[FListing.Starts[i]+DirListNameOffset];
    writeln(i,' "',Filename,'"');
  end;
end;

procedure TCTDirectoryCache.Invalidate;
begin
  FListing.FileTimeStamp:=CTInvalidChangeStamp;
end;

procedure TCTDirectoryCache.GetFiles(var Files: TStrings; IncludeDirs: boolean);
var
  ListedFiles: PChar;
  i: Integer;
  p: PChar;
begin
  if Files=nil then
    Files:=TStringList.Create;
  if (Self=nil) or (Directory='') then exit;
  UpdateListing;
  ListedFiles:=FListing.Files;
  for i:=0 to FListing.Count-1 do begin
    p:=@ListedFiles[FListing.Starts[i]];
    if IncludeDirs
    or ((PCTDirectoryListingHeader(p)^.Attr and faDirectory)=0) then
      Files.Add(PChar(p+DirListNameOffset));
  end;
end;

{ TCTStarDirectoryCache }

constructor TCTStarDirectoryCache.Create(const TheDirectory: string;
  TheKind: TCTStarDirectoryKind; ThePool: TCTDirectoryCachePool);
begin
  FDirectory:=TheDirectory;
  FKind:=TheKind;
  FPool:=ThePool;
  FListing:=TListing.Create;
end;

destructor TCTStarDirectoryCache.Destroy;
begin
  FListing.Free;
  FListing:=nil;
  inherited Destroy;
end;

procedure TCTStarDirectoryCache.CalcMemSize(Stats: TCTMemStats);
begin
  Stats.Add('TCTStarDirectoryCache',PtrUInt(InstanceSize)
    +MemSizeString(FDirectory));

  if FListing<>nil then
    Stats.Add('TCTStarDirectoryCache.FListing',FListing.CalcMemSize);
end;

function TCTStarDirectoryCache.FindFile(const ShortFilename: string;
  const FileCase: TCTSearchFileCase): string;

  procedure RaiseDontKnow;
  begin
    raise Exception.Create('do not know FileCase '+IntToStr(ord(FileCase)));
  end;

var
  i: Integer;
begin
  Result:='';
  if ShortFilename='' then exit;
  i:=0;
  case FileCase of
  ctsfcDefault:
    {$IFDEF CaseInsensitiveFilenames}
    i:=IndexOfFileCaseInsensitive(Pointer(ShortFilename));// pointer type cast avoids #0 check
    {$ELSE}
    i:=IndexOfFileCaseSensitive(Pointer(ShortFilename));// pointer type cast avoids #0 check
    {$ENDIF}
  ctsfcAllCase,ctsfcLoUpCase:
    i:=IndexOfFileCaseInsensitive(Pointer(ShortFilename));// pointer type cast avoids #0 check
  else RaiseDontKnow;
  end;
  if i>=0 then
    Result:=FListing.GetSubDirFilename(i);
end;

function TCTStarDirectoryCache.FindIncludeFile(const IncFilename: string;
  AnyCase: boolean): string;
var
  Files, IncExtP, CurExtP, CurFilename, IncFilenameP: PChar;
  Starts: PInteger;
  l, r, m, first, cmp: TListingPosition;
  AUnitName: String;
  Ext, BestExt: TCTPascalIncExtType;
  Best: Integer;
  Stop: Boolean;
begin
  Result:='';
  {$IFDEF DebugDirCacheFindIncFile}
  DebugLn('TCTStarDirectoryCache.FindIncludeFile ',CTStarDirectoryKindNames[Kind],' IncName="',IncFilename,'" AnyCase=',dbgs(AnyCase),' Directory=',Directory);
  {$ENDIF}
  if IncFilename='' then exit;
  UpdateListing;

  Files:=FListing.Files;
  if Files=nil then exit;
  Starts:=FListing.Starts;

  IncFilenameP:=PChar(IncFilename);
  l:=length(IncFilename);
  while (l>0) and (IncFilename[l]<>'.') do dec(l);
  if l>0 then begin
    IncExtP:=@IncFilename[l];
    AUnitName:=LeftStr(IncFilename,l-1);
  end else begin
    IncExtP:=nil;
    AUnitName:=IncFilename;
  end;

  // binary search the lowest filename matching the unitname
  {$IFDEF DebugDirCacheFindIncFile}
  WriteListing;
  {$ENDIF}
  l:=0;
  r:=FListing.Count-1;
  first:=-1;
  while l<=r do begin
    m:=(l+r) shr 1;
    CurFilename:=@Files[Starts[m]+SizeOf(TListingHeader)];
    cmp:=ComparePCharUnitNameWithFilename(Pointer(AUnitName),CurFilename);
    if cmp>0 then
      l:=m+1
    else if cmp<0 then
      r:=m-1
    else begin
      first:=m;
      r:=m-1;
    end;
  end;
  if first<0 then exit;
  m:=first;
  // -> now find a filename with correct case and extension
  Best:=-1;
  BestExt:=high(TCTPascalIncExtType);
  while m<FListing.Count do begin
    CurFilename:=@Files[Starts[m]+SizeOf(TListingHeader)];
    // check if filename has the right AUnitName prefix
    if (ComparePCharUnitNameWithFilename(Pointer(AUnitName),CurFilename)<>0)
    then
      break;

    // check if the filename fits
    CurExtP:=CurFilename+length(AUnitname);
    {$IFDEF DebugDirCacheFindIncFile}
    //if (CompareText(AUnitName,DebugUnitName)=0) and (System.Pos(DebugDirPart,directory)>0) then
      DebugLn('TCTDirectoryCache.FindIncludeFile NEXT "',CurFilename,'" ExtStart=',dbgstr(CurExtP^));
    {$ENDIF}
    Stop:=false;
    if IncExtP<>nil then begin
      // include file with extension
      if AnyCase then begin
        if ComparePCharCaseInsensitiveASCII(CurExtP,IncExtP)=0 then
          // any case with extension fits -> can't get any better
          Stop:=true;
      end else if CheckLoUpCase(IncFilenameP,CurFilename,length(IncFilename)+1) then
        Stop:=true; // mixed case with extension fits -> can't get any better
    end else begin
      // include file without extension -> search without and with default extension
      if (CurExtP^=#0) then begin
        if AnyCase or CheckLoUpCase(IncFilenameP,CurFilename,length(IncFilename)+1) then
          // file without extension fits a file without extension -> can't get any better
          Stop:=true;
      end else begin
        Ext:=IsPascalIncExt(CurExtP);
        if Ext>pietNone then begin
          // file without extension fits an include file with extension
          // Note: the compiler prefers file.inc over file.pas
          if (Best<0) or (BestExt>Ext) then begin
            if AnyCase or CheckLoUpCase(IncFilenameP,CurFilename,length(IncFilename)) then begin
              Best:=m;
              BestExt:=Ext;
            end;
          end;
        end;
      end;
    end;
    if Stop then begin
      Best:=m;
      break;
    end;
    inc(m);
  end;
  if Best>=0 then begin
    Result:=FListing.GetSubDirFilename(Best);
    exit;
  end;
  {$IFDEF DebugDirCacheFindUnitSource}
  if m<FListing.Count then
    //if (CompareText(AUnitName,DebugUnitName)=0) and (System.Pos(DebugDirPart,directory)>0) then
      DebugLn('TCTDirectoryCache.FindUnitSource LAST ',CurFilename);
  {$ENDIF}
end;

function TCTStarDirectoryCache.FindUnitSource(const AUnitName: string;
  AnyCase: boolean): string;
var
  Files, CurFilename: PChar;
  l, r, m: TListingPosition;
  Starts: PInteger;
  cmp, first: Integer;
  ExtStartPos: PChar;
  Found: Boolean;
begin
  Result:='';
  {$IFDEF DebugDirCacheFindUnitSource}
  DebugLn('TCTStarDirectoryCache.FindUnitSource ',CTStarDirectoryKindNames[Kind],' AUnitName="',AUnitName,'" AnyCase=',dbgs(AnyCase),' Directory=',Directory);
  {$ENDIF}
  if AUnitName='' then exit;
  UpdateListing;

  Files:=FListing.Files;
  if Files=nil then exit;
  Starts:=FListing.Starts;
  // binary search the lowest filename matching the unitname
  {$IFDEF DebugDirCacheFindUnitSource}
  WriteListing;
  {$ENDIF}
  l:=0;
  r:=FListing.Count-1;
  first:=-1;
  while l<=r do begin
    m:=(l+r) shr 1;
    CurFilename:=@Files[Starts[m]+SizeOf(TListingHeader)];
    cmp:=ComparePCharUnitNameWithFilename(Pointer(AUnitName),CurFilename);
    if cmp>0 then
      l:=m+1
    else if cmp<0 then
      r:=m-1
    else begin
      first:=m;
      r:=m-1;
    end;
  end;
  if first<0 then exit;
  m:=first;
  // -> now find a filename with correct case and extension
  while m<FListing.Count do begin
    CurFilename:=@Files[Starts[m]+SizeOf(TListingHeader)];
    // check if filename has the right AUnitName prefix
    if (ComparePCharUnitNameWithFilename(Pointer(AUnitName),CurFilename)<>0)
    then
      break;

    // check if the filename fits
    ExtStartPos:=CurFilename+length(AUnitname);
    {$IFDEF DebugDirCacheFindUnitSource}
    //if (CompareText(AUnitName,DebugUnitName)=0) and (System.Pos(DebugDirPart,directory)>0) then
      DebugLn('TCTDirectoryCache.FindUnitSource NEXT "',CurFilename,'" ExtStart=',dbgstr(ExtStartPos^));
    {$ENDIF}
    if IsPascalUnitExt(ExtStartPos) then begin
      // the extension is ok
      {$IFDEF DebugDirCacheFindUnitSource}
      //if (CompareText(AUnitName,DebugUnitName)=0) and (System.Pos(DebugDirPart,directory)>0) then
        DebugLn('TCTDirectoryCache.FindUnitSource CHECKING CASE "',CurFilename,'"');
      {$ENDIF}
      Found:=false;
      if AnyCase then begin
        Found:=true
      end else begin
        // check case platform dependent
        {$IFDEF CaseInsensitiveFilenames}
        Found:=true;
        {$ELSE}
        if (ExtractFileNameOnly(CurFilename)=AUnitName)
        or (CurFilename=lowercase(CurFilename))
        or (CurFilename=uppercase(CurFilename)) then
          Found:=true;
        {$ENDIF}
      end;
      if Found then begin
        Result:=FListing.GetSubDir(m);
        if Result<>'' then
          Result:=Result+PathDelim+CurFilename
        else
          Result:=CurFilename;
        exit;
      end;
    end;
    inc(m);
  end;
  {$IFDEF DebugDirCacheFindUnitSource}
  if m<FListing.Count then
    //if (CompareText(AUnitName,DebugUnitName)=0) and (System.Pos(DebugDirPart,directory)>0) then
      DebugLn('TCTDirectoryCache.FindUnitSource LAST ',CurFilename);
  {$ENDIF}
end;

function TCTStarDirectoryCache.IndexOfFileCaseInsensitive(ShortFilename: PChar
  ): integer;
var
  Files: PChar;
  l: Integer;
  r: Integer;
  m: Integer;
  CurFilename: PChar;
  cmp: Integer;
begin
  Result:=-1;
  UpdateListing;
  Files:=FListing.Files;
  if Files=nil then exit;
  // binary search for lowest match
  l:=0;
  r:=FListing.Count-1;
  while l<=r do begin
    m:=(l+r) shr 1;
    CurFilename:=@Files[FListing.Starts[m]+SizeOf(TListingHeader)];
    cmp:=ComparePCharCaseInsensitiveASCII(ShortFilename,CurFilename);
    if cmp>0 then
      l:=m+1
    else if cmp<0 then
      r:=m-1
    else begin
      // found
      Result:=m;
      // continue search for lower index
      r:=m-1;
    end;
  end;
end;

function TCTStarDirectoryCache.IndexOfFileCaseSensitive(ShortFilename: PChar
  ): integer;
var
  Files: PChar;
  l: Integer;
  r: Integer;
  m: Integer;
  CurFilename: PChar;
  cmp: Integer;
begin
  Result:=-1;
  UpdateListing;
  Files:=FListing.Files;
  if Files=nil then exit;
  l:=0;
  r:=FListing.Count-1;
  while l<=r do begin
    m:=(l+r) shr 1;
    CurFilename:=@Files[FListing.Starts[m]+DirListNameOffset];
    cmp:=ComparePCharFirstCaseInsAThenCase(ShortFilename,CurFilename);// pointer type cast avoids #0 check
    if cmp>0 then
      l:=m+1
    else if cmp<0 then
      r:=m-1
    else begin
      // found
      Result:=m;
      // continue search for lower index
      r:=m-1;
    end;
  end;
end;

procedure TCTStarDirectoryCache.UpdateListing;
var
  WorkingListing: PWorkStarFileInfo;
  WorkingListingCount: integer;
  WorkingListingCapacity: integer;
  Excludes: TStrings;

  function IsExcluded(const CurSubDir: string): boolean;
  var
    i: Integer;
    CurDir, ExcludeMask: String;
  begin
    CurDir:=ExtractFilename(CurSubDir);
    if (CurDir='*') or (CurDir='**') then exit(true);
    for i:=0 to Excludes.Count-1 do begin
      ExcludeMask:=Excludes[i];
      if FilenameIsMatching(ExcludeMask,CurSubDir,true,true)
          or FilenameIsMatching(ExcludeMask,CurDir,true,true) then
        exit(true);
    end;
    Result:=false;
  end;

  procedure TraverseDir(const CurSubDir: string; Level: integer);
  var
    SubDirIndex: TListingPosition;
    Dir: TCTDirectoryCache;
    DirListing: TCTDirectoryListing;
    i, NewCapacity: Integer;
    WorkingItem: PWorkStarFileInfo;
  begin
    if IsExcluded(CurSubDir) then exit;

    if Level=0 then
      SubDirIndex:=-1
    else
      SubDirIndex:=FListing.SubDirs.Add(CurSubDir);

    Dir:=Pool.GetCache(Directory+CurSubDir,true,false);
    Dir.UpdateListing;
    DirListing:=Dir.Listing;

    // first add files of this directory
    // Note: the special directores '.' , '..' and '' are not in the DirListing
    for i:=0 to DirListing.Count-1 do begin
      if DirListing.GetAttr(i) and faDirectory>0 then continue;
      // add file
      if WorkingListingCount=WorkingListingCapacity then begin
        // grow WorkingListing
        if WorkingListingCapacity>0 then
          NewCapacity:=WorkingListingCapacity*2
        else
          NewCapacity:=128;
        ReAllocMem(WorkingListing,SizeOf(TWorkStarFileInfo)*NewCapacity);
        FillByte(WorkingListing[WorkingListingCount],
                 SizeOf(TWorkStarFileInfo)*(NewCapacity-WorkingListingCapacity),0);
        WorkingListingCapacity:=NewCapacity;
      end;
      WorkingItem:=@WorkingListing[WorkingListingCount];
      WorkingItem^.Header.SubDirIndex:=SubDirIndex;
      WorkingItem^.FileName:=DirListing.GetFilename(i);
      inc(WorkingListingCount);
    end;

    // then add files of sub dirs
    case Kind of
      ctsdNone: exit;
      ctsdStar: if Level>0 then exit;
      ctsdStarStar: ;
    end;
    inc(Level);
    for i:=0 to DirListing.Count-1 do begin
      if DirListing.GetAttr(i) and faDirectory=0 then continue;
      // add sub directory
      if Level=1 then
        TraverseDir(DirListing.GetFilename(i),Level)
      else
        TraverseDir(CurSubDir+PathDelim+DirListing.GetFilename(i),Level);
    end;
  end;

var
  SortMap: PPWorkStarFileInfo;
  i, TotalLen: Integer;
  p: PCHar;
  WorkingItem: PWorkStarFileInfo;
  CurFilenameLen: SizeInt;
begin
  if FListing.FileTimeStamp=Pool.FileTimeStamp then exit;
  FListing.Clear;
  FListing.FileTimeStamp:=Pool.FileTimeStamp;
  if not FilenameIsAbsolute(Directory) then
    exit;// virtual directory

  // gather all sub dirs and files
  SortMap:=nil;
  WorkingListing:=nil;
  WorkingListingCount:=0;
  WorkingListingCapacity:=0;
  try
    Excludes:=Pool.StarDirectoryExcludes;
    TraverseDir('',0);

    if WorkingListingCount=0 then exit;

    // sort the files
    GetMem(SortMap,WorkingListingCount*SizeOf(Pointer));
    for i:=0 to WorkingListingCount-1 do
      SortMap[i]:=@WorkingListing[i];
    MergeSortWithLen(PPointer(SortMap),WorkingListingCount,@CompareWorkStarFileInfos);

    // create listing
    TotalLen:=0;
    for i:=0 to WorkingListingCount-1 do
      inc(TotalLen,length(WorkingListing[i].FileName)+1+SizeOf(TCTDirectoryListingHeader));
    GetMem(FListing.Files,TotalLen);
    FListing.Size:=TotalLen;
    FListing.Count:=WorkingListingCount;
    GetMem(FListing.Starts,SizeOf(Integer)*WorkingListingCount);
    p:=FListing.Files;
    for i:=0 to WorkingListingCount-1 do begin
      FListing.Starts[i]:=p-FListing.Files;
      WorkingItem:=SortMap[i];
      PListingHeader(p)^:=WorkingItem^.Header;
      inc(p,SizeOf(TListingHeader));
      // filename
      CurFilenameLen:=length(WorkingItem^.FileName);
      if CurFilenameLen>0 then begin
        System.Move(WorkingItem^.FileName[1],p^,CurFilenameLen);
        inc(p,CurFilenameLen);
      end;
      p^:=#0;
      inc(p);
    end;
  finally
    ReAllocMem(SortMap,0);
    for i:=0 to WorkingListingCount-1 do
      WorkingListing[i].FileName:='';
    ReAllocMem(WorkingListing,0);
  end;
end;

procedure TCTStarDirectoryCache.WriteListing;
var
  i: Integer;
  Filename: String;
begin
  writeln('TCTStarDirectoryCache.WriteListing Count=',FListing.Count,' Size=',FListing.Size);
  for i:=0 to FListing.Count-1 do begin
    Filename:=FListing.GetSubDirFilename(i);
    writeln(i,' "',Filename,'"');
  end;
end;

procedure TCTStarDirectoryCache.Invalidate;
begin
  FListing.FileTimeStamp:=CTInvalidChangeStamp;
end;

{ TCTStarDirectoryCache.TListing }

constructor TCTStarDirectoryCache.TListing.Create;
begin
  SubDirs:=TStringListUTF8Fast.Create;
end;

destructor TCTStarDirectoryCache.TListing.Destroy;
begin
  Clear;
  SubDirs.Free;
  SubDirs:=nil;
  inherited Destroy;
end;

procedure TCTStarDirectoryCache.TListing.Clear;
begin
  if Files<>nil then begin
    FreeMem(Files);
    Files:=nil;
    FreeMem(Starts);
    Starts:=nil;
  end;
  SubDirs.Clear;
  Count:=0;
  Size:=0;
end;

function TCTStarDirectoryCache.TListing.CalcMemSize: PtrUInt;
begin
  Result:=PtrUInt(InstanceSize)
  {%H-}+SizeOf(Pointer)*Count  // Starts
    +PtrUInt(Size); // Files
end;

function TCTStarDirectoryCache.TListing.GetShortFilename(Index: integer): PChar;

  procedure RaiseIndexOutOfBounds;
  begin
    raise Exception.Create('TCTStarDirectoryCache.TListing.GetPosition: Index out of bounds');
  end;

begin
  if (Index<0) or (Index>=Count) then
    RaiseIndexOutOfBounds;
  Result:=@Files[Starts[Index]+SizeOf(TListingHeader)];
end;

function TCTStarDirectoryCache.TListing.GetSubDir(Index: integer): String;

  procedure RaiseIndexOutOfBounds;
  begin
    raise Exception.Create('TCTStarDirectoryCache.TListing.GetSubDir: Index out of bounds');
  end;

var
  i: TListingPosition;
begin
  if (Index<0) or (Index>=Count) then
    RaiseIndexOutOfBounds;
  i:=PListingHeader(@Files[Starts[Index]])^.SubDirIndex;
  if i>=0 then
    Result:=SubDirs[i]
  else
    Result:='';
end;

function TCTStarDirectoryCache.TListing.GetSubDirIndex(Index: integer
  ): TListingPosition;

  procedure RaiseIndexOutOfBounds;
  begin
    raise Exception.Create('TCTStarDirectoryCache.TListing.GetSubDirIndex: Index out of bounds');
  end;

begin
  if (Index<0) or (Index>=Count) then
    RaiseIndexOutOfBounds;
  Result:=PListingHeader(@Files[Starts[Index]])^.SubDirIndex;
end;

function TCTStarDirectoryCache.TListing.GetSubDirFilename(Index: integer
  ): String;

  procedure RaiseIndexOutOfBounds;
  begin
    raise Exception.Create('TCTStarDirectoryCache.TListing.GetSubDirFilename: Index out of bounds');
  end;

var
  i: TListingPosition;
  f: PChar;
begin
  if (Index<0) or (Index>=Count) then
    RaiseIndexOutOfBounds;
  i:=PListingHeader(@Files[Starts[Index]])^.SubDirIndex;
  if i<0 then
    Result:=''
  else
    Result:=SubDirs[i]+PathDelim;
  f:=@Files[Starts[Index]+SizeOf(TListingHeader)];
  Result:=Result+f;
end;

{ TCTDirectoryCachePool }

procedure TCTDirectoryCachePool.DoRemove(ACache: TCTDirectoryCache);
begin
  FDirectories.Remove(ACache);
end;

procedure TCTDirectoryCachePool.OnFileStateCacheChangeTimeStamp(
  Sender: TObject; const AFilename: string);
var
  Dir: String;
  Cache: TCTDirectoryCache;
begin
  if AFilename='' then
    IncreaseFileTimeStamp
  else if FilenameIsAbsolute(AFilename) then begin
    Dir:=ExtractFilePath(AFilename);
    Cache:=GetCache(Dir,false,false);
    //debugln(['TCTDirectoryCachePool.OnFileStateCacheChangeTimeStamp Dir="',Dir,'" Cache=',Cache<>nil]);
    if Cache=nil then exit;
    Cache.Invalidate;
  end;
end;

procedure TCTDirectoryCachePool.SetStarDirectoryExcludes(const AValue: TStrings
  );
begin
  if FStarDirectoryExcludes.Equals(AValue) then Exit;
  FStarDirectoryExcludes.Assign(AValue);
  IncreaseConfigTimeStamp;
end;

constructor TCTDirectoryCachePool.Create;
var
  sk: TCTStarDirectoryKind;
begin
  FDirectories:=TAVLTree.Create(@CompareCTDirectoryCaches);
  for sk in TCTStarDirectoryKind do
    FStarDirectories[sk]:=TAVLTree.Create(@CompareCTStarDirectoryCaches);
  IncreaseFileTimeStamp;
  IncreaseConfigTimeStamp;
  if FileStateCache<>nil then
    FileStateCache.AddChangeTimeStampHandler(@OnFileStateCacheChangeTimeStamp);
  FStarDirectoryExcludes:=TStringListUTF8Fast.Create;
  FStarDirectoryExcludes.Delimiter:=';';
  FStarDirectoryExcludes.Add('.*');
end;

destructor TCTDirectoryCachePool.Destroy;
var
  Cache: TCTDirectoryCache;
  sk: TCTStarDirectoryKind;
begin
  if FileStateCache<>nil then
    FileStateCache.RemoveChangeTimeStampHandler(@OnFileStateCacheChangeTimeStamp);
  while FDirectories.Root<>nil do begin
    Cache:=TCTDirectoryCache(FDirectories.Root.Data);
    if Cache.RefCount<>1 then
      raise Exception.Create('TCTDirectoryCachePool.Destroy');
    Cache.Release;
    Cache:=nil;
  end;
  FDirectories.Free;
  FDirectories:=nil;
  for sk in TCTStarDirectoryKind do begin
    FStarDirectories[sk].Free;
    FStarDirectories[sk]:=nil;
  end;
  FreeAndNil(FStarDirectoryExcludes);
  inherited Destroy;
end;

procedure TCTDirectoryCachePool.CalcMemSize(Stats: TCTMemStats);
var
  Node: TAVLTreeNode;
  sk: TCTStarDirectoryKind;
begin
  Stats.Add('TCTDirectoryCachePool',PtrUInt(InstanceSize));
  Stats.Add('TCTDirectoryCachePool.Count',FDirectories.Count);
  Node:=FDirectories.FindLowest;
  while Node<>nil do begin
    TCTDirectoryCache(Node.Data).CalcMemSize(Stats);
    Node:=FDirectories.FindSuccessor(Node);
  end;
  for sk in TCTStarDirectoryKind do begin
    Stats.Add('TCTDirectoryCachePool.StarCount['+CTStarDirectoryKindNames[sk]+']',FStarDirectories[sk].Count);
    Node:=FStarDirectories[sk].FindLowest;
    while Node<>nil do begin
      TCTStarDirectoryCache(Node.Data).CalcMemSize(Stats);
      Node:=FDirectories.FindSuccessor(Node);
    end;
  end;
end;

procedure TCTDirectoryCachePool.GetListing(const aDirectory: string;
  var Files: TStrings; IncludeDirs: boolean);
begin
  GetCache(aDirectory,true,false).GetFiles(Files,IncludeDirs);
end;

function TCTDirectoryCachePool.GetCache(const Directory: string;
  CreateIfNotExists: boolean; DoReference: boolean): TCTDirectoryCache;
var
  Node: TAVLTreeNode;
  Dir: String;
begin
  Dir:=AppendPathDelim(TrimFilename(Directory));
  Node:=FDirectories.FindKey(Pointer(Dir),@CompareAnsiStringAndDirectoryCache);
  if Node<>nil then begin
    Result:=TCTDirectoryCache(Node.Data);
    if DoReference then
      Result.Reference;
  end else if DoReference or CreateIfNotExists then begin
    Dir:=FindDiskFilename(Directory);
    Result:=TCTDirectoryCache.Create(Dir,Self);
    FDirectories.Add(Result);
    if DoReference then
      Result.Reference;
  end else
    Result:=nil;
end;

function TCTDirectoryCachePool.GetStarCache(const Directory: string;
  Kind: TCTStarDirectoryKind; CreateIfNotExists: boolean
  ): TCTStarDirectoryCache;
var
  Dir: String;
  Node: TAVLTreeNode;
begin
  if Kind=ctsdNone then
    exit(nil);
  Dir:=AppendPathDelim(TrimFilename(Directory));
  Node:=FStarDirectories[Kind].FindKey(Pointer(Dir),@CompareAnsiStringAndStarDirectoryCache);
  if Node<>nil then begin
    Result:=TCTStarDirectoryCache(Node.Data);
  end else if CreateIfNotExists then begin
    Dir:=AppendPathDelim(FindDiskFilename(Directory));
    Result:=TCTStarDirectoryCache.Create(Dir,Kind,Self);
    FStarDirectories[Kind].Add(Result);
  end else
    Result:=nil;
end;

function TCTDirectoryCachePool.GetBaseCache(const Directory: string;
  CreateIfNotExists: boolean): TCTDirectoryBaseCache;
var
  l: integer;
  Star: TCTStarDirectoryKind;
begin
  Star:=IsCTStarDirectory(Directory,l);
  if Star=ctsdNone then
    Result:=GetCache(Directory,CreateIfNotExists,false)
  else
    Result:=GetStarCache(LeftStr(Directory,l),Star,CreateIfNotExists);
end;

function TCTDirectoryCachePool.GetString(const Directory: string;
  AStringType: TCTDirCacheString; UseCache: boolean): string;
var
  Cache: TCTDirectoryCache;
begin
  if UseCache then begin
    Cache:=GetCache(Directory,true,false);
    if Cache<>nil then
      Result:=Cache.Strings[AStringType]
    else
      Result:='';
  end else begin
    Result:=OnGetString(Directory,AStringType);
  end;
end;

procedure TCTDirectoryCachePool.IncreaseFileTimeStamp;
begin
  //DebugLn(['TCTDirectoryCachePool.IncreaseTimeStamp ']);
  CTIncreaseChangeStamp(FFileTimeStamp);
end;

procedure TCTDirectoryCachePool.IncreaseConfigTimeStamp;
begin
  //DebugLn(['TCTDirectoryCachePool.IncreaseConfigTimeStamp ']);
  CTIncreaseChangeStamp(FConfigTimeStamp);
end;

function TCTDirectoryCachePool.FileExists(Filename: string): boolean;
begin
  Result:=FileExists(Filename,ctsfcDefault);
end;

function TCTDirectoryCachePool.FileExists(Filename: string; FileCase: TCTSearchFileCase): boolean;
var
  Directory: String;
  Cache: TCTDirectoryCache;
  ShortFilename: String;
begin
  Filename:=TrimFilename(Filename);
  if Filename='' then exit(false);
  ShortFilename:=ExtractFilename(Filename);
  if (ShortFilename<>'') and (ShortFilename<>'.') and (ShortFilename<>'..') then
  begin
    if FilenameIsAbsolute(Filename) then begin
      Directory:=ExtractFilePath(Filename);
      Cache:=GetCache(Directory,true,false);
      Result:=Cache.FindFile(ShortFilename,FileCase)<>'';
    end else begin
      Result:=FindVirtualFile(Filename)<>'';
    end;
    exit;
  end;
  // fallback
  Result:=FileStateCache.FileExistsCached(Filename);
end;

function TCTDirectoryCachePool.FileAge(Filename: string): TCTFileAgeTime;
var
  Directory: String;
  Cache: TCTDirectoryCache;
  ShortFilename: String;
begin
  Filename:=TrimFilename(Filename);
  if (Filename<>'') and FilenameIsAbsolute(Filename) then begin
    ShortFilename:=ExtractFilename(Filename);
    if (ShortFilename<>'') and (ShortFilename<>'.') and (ShortFilename<>'..')
    then begin
      Directory:=ExtractFilePath(Filename);
      Cache:=GetCache(Directory,true,false);
      Result:=Cache.FileAge(ShortFilename);
      exit;
    end;
  end;
  // fallback
  Result:=FileStateCache.FileAgeCached(Filename);
end;

function TCTDirectoryCachePool.FileAttr(Filename: string): TCTDirectoryListingAttr;
var
  Directory: String;
  Cache: TCTDirectoryCache;
  ShortFilename: String;
begin
  Filename:=TrimFilename(Filename);
  if (Filename<>'') and FilenameIsAbsolute(Filename) then begin
    ShortFilename:=ExtractFilename(Filename);
    if (ShortFilename<>'') and (ShortFilename<>'.') and (ShortFilename<>'..')
    then begin
      Directory:=ExtractFilePath(Filename);
      Cache:=GetCache(Directory,true,false);
      Result:=Cache.FileAttr(ShortFilename);
      exit;
    end;
  end;
  // fallback
  Result:=0;
end;

function TCTDirectoryCachePool.FileSize(Filename: string): TCTDirectoryListingSize;
var
  Directory: String;
  Cache: TCTDirectoryCache;
  ShortFilename: String;
begin
  Filename:=TrimFilename(Filename);
  if (Filename<>'') and FilenameIsAbsolute(Filename) then begin
    ShortFilename:=ExtractFilename(Filename);
    if (ShortFilename<>'') and (ShortFilename<>'.') and (ShortFilename<>'..')
    then begin
      Directory:=ExtractFilePath(Filename);
      Cache:=GetCache(Directory,true,false);
      Result:=Cache.FileSize(ShortFilename);
      exit;
    end;
  end;
  // fallback
  Result:=-1;
end;

function TCTDirectoryCachePool.FindUnitInUnitLinks(const Directory,
  AUnitName: string): string;
  
  procedure RaiseDirNotAbsolute;
  begin
    raise Exception.Create('TCTDirectoryCachePool.FindUnitInUnitLinks not absolute Directory="'+Directory+'"');
  end;
  
var
  Cache: TCTDirectoryCache;
begin
  if (Directory<>'') and not FilenameIsAbsolute(Directory) then
    RaiseDirNotAbsolute;
  Cache:=GetCache(Directory,true,false);
  Result:=Cache.FindUnitLink(AUnitName);
end;

function TCTDirectoryCachePool.FindUnitInUnitSet(const Directory,
  AUnitName: string): string;

  procedure RaiseDirNotAbsolute;
  begin
    raise Exception.Create('TCTDirectoryCachePool.FindUnitInUnitSet not absolute Directory="'+Directory+'"');
  end;

var
  Cache: TCTDirectoryCache;
begin
  if (Directory<>'') and not FilenameIsAbsolute(Directory) then
    RaiseDirNotAbsolute;
  Cache:=GetCache(Directory,true,false);
  Result:=Cache.FindUnitInUnitSet(AUnitName);
end;

function TCTDirectoryCachePool.FindCompiledUnitInUnitSet(const Directory,
  AUnitName: string): string;

  procedure RaiseDirNotAbsolute;
  begin
    raise Exception.Create('TCTDirectoryCachePool.FindCompiledUnitInUnitSet not absolute Directory="'+Directory+'"');
  end;

var
  Cache: TCTDirectoryCache;
begin
  if (Directory<>'') and not FilenameIsAbsolute(Directory) then
    RaiseDirNotAbsolute;
  Cache:=GetCache(Directory,true,false);
  Result:=Cache.FindCompiledUnitInUnitSet(AUnitName);
end;

procedure TCTDirectoryCachePool.IterateFPCUnitsInSet(const Directory: string;
  const Iterate: TCTOnIterateFile);

  procedure RaiseDirNotAbsolute;
  begin
    raise Exception.Create('TCTDirectoryCachePool.IterateFPCUnitsInSet not absolute Directory="'+Directory+'"');
  end;

var
  Cache: TCTDirectoryCache;
begin
  if (Directory<>'') and not FilenameIsAbsolute(Directory) then
    RaiseDirNotAbsolute;
  Cache:=GetCache(Directory,true,false);
  Cache.IterateFPCUnitsInSet(Iterate);
end;

function TCTDirectoryCachePool.FindDiskFilename(const Filename: string;
  SearchCaseInsensitive: boolean): string;
var
  ADirectory: String;
  Cache: TCTDirectoryCache;
  DiskShortFilename: String;
begin
  Result:=ChompPathDelim(ResolveDots(Filename));
  if Result='' then exit;
  //debugln(['TCTDirectoryCachePool.FindDiskFilename Filename=',Result]);
  {$IF defined(NotLiteralFilenames) or defined(CaseInsensitiveFilenames)}
  {$ELSE}
  if (not SearchCaseInsensitive) then exit;
  {$ENDIF}
  ADirectory:=ExtractFilePath(Result);
  if ADirectory=Result then
    exit; // e.g. / under Linux
  if SearchCaseInsensitive then
    // search recursively all directory parts
    ADirectory:=AppendPathDelim(FindDiskFilename(ADirectory,true));
  Cache:=GetCache(ADirectory,true,false);
  //debugln(['TCTDirectoryCachePool.FindDiskFilename Dir=',Cache.Directory]);
  Result:=ExtractFileName(Result);
  DiskShortFilename:=Cache.FindFile(Result,ctsfcAllCase);
  //debugln(['TCTDirectoryCachePool.FindDiskFilename DiskShortFilename=',DiskShortFilename]);
  if DiskShortFilename<>'' then Result:=DiskShortFilename;
  Result:=Cache.Directory+Result;
end;

function TCTDirectoryCachePool.FindIncludeFileInDirectory(Directory,
  IncFileName: string; AnyCase: boolean): string;
var
  Cache: TCTDirectoryBaseCache;
begin
  Cache:=GetBaseCache(Directory,true);
  Result:=Cache.FindIncludeFile(IncFileName,AnyCase);
  if Result='' then exit;
  Result:=Cache.Directory+Result;
end;

function TCTDirectoryCachePool.FindUnitInDirectory(const Directory,
  AUnitName: string; AnyCase: boolean): string;
var
  Cache: TCTDirectoryBaseCache;
begin
  Cache:=GetBaseCache(Directory,true);
  Result:=Cache.FindUnitSource(AUnitName,AnyCase);
  if Result='' then exit;
  Result:=Cache.Directory+Result;
end;

function TCTDirectoryCachePool.FindVirtualFile(const Filename: string): string;
begin
  if Assigned(OnFindVirtualFile) then
    Result:=OnFindVirtualFile(Filename)
  else
    Result:='';
end;

function TCTDirectoryCachePool.FindVirtualInclude(const Filename: string
  ): string;

  function FindLowUpCase(const CurFilename: string): string;
  {$IFNDEF CaseInsensitiveFilenames}
  var
    AltFilename: String;
  {$ENDIF}
  begin
    // search mixed case
    Result:=FindVirtualFile(CurFilename);
    if Result<>'' then exit;
    {$IFNDEF CaseInsensitiveFilenames}
    // search lowercase
    AltFilename:=lowercase(CurFilename);
    if AltFilename<>CurFilename then begin
      Result:=FindVirtualFile(AltFilename);
      if Result<>'' then exit;
    end;
    // search uppercase
    AltFilename:=uppercase(CurFilename);
    if AltFilename<>CurFilename then begin
      Result:=FindVirtualFile(AltFilename);
      if Result<>'' then exit;
    end;
    {$ENDIF}
  end;

begin
  Result:=FindLowUpCase(Filename);
  if Result<>'' then exit;
  if Pos('.',Filename)=0 then begin
    // try default extensions
    Result:=FindLowUpCase(Filename+'.inc');
    if Result<>'' then exit;
    Result:=FindLowUpCase(Filename+'.pp');
    if Result<>'' then exit;
    Result:=FindLowUpCase(Filename+'.pas');
  end;
end;

function TCTDirectoryCachePool.FindVirtualUnit(const AUnitName: string): string;
var
  e: TCTPascalExtType;
  {$IFNDEF CaseInsensitiveFilenames}
  CurUnitName: String;
  {$ENDIF}
begin
  // search mixed case
  for e:=Low(CTPascalExtension) to High(CTPascalExtension) do begin
    if CTPascalExtension[e]='' then continue;
    Result:=FindVirtualFile(AUnitName+CTPascalExtension[e]);
    if Result<>'' then exit;
  end;
  {$IFNDEF CaseInsensitiveFilenames}
  // search lowercase
  CurUnitName:=lowercase(AUnitName);
  if CurUnitName<>AUnitName then begin
    for e:=Low(CTPascalExtension) to High(CTPascalExtension) do begin
      if CTPascalExtension[e]='' then continue;
      Result:=FindVirtualFile(CurUnitName+CTPascalExtension[e]);
      if Result<>'' then exit;
    end;
  end;
  // search uppercase
  CurUnitName:=uppercase(AUnitName);
  for e:=Low(CTPascalExtension) to High(CTPascalExtension) do begin
    if CTPascalExtension[e]='' then continue;
    Result:=FindVirtualFile(CurUnitName+uppercase(CTPascalExtension[e]));
    if Result<>'' then exit;
  end;
  Result:='';
  {$ENDIF}
end;

function TCTDirectoryCachePool.FindUnitSourceInCompletePath(
  const Directory: string; var AUnitName, InFilename: string; AnyCase: boolean): string;
var
  Cache: TCTDirectoryCache;
begin
  Cache:=GetCache(Directory,true,false);
  Result:=Cache.FindUnitSourceInCompletePath(AUnitName,InFilename,AnyCase);
end;

function TCTDirectoryCachePool.FindIncludeFileInCompletePath(Directory,
  IncFilename: string; AnyCase: boolean): string;
var
  Cache: TCTDirectoryCache;
begin
  Cache:=GetCache(Directory,true,false);
  Result:=Cache.FindIncludeFileInPath(IncFilename,AnyCase);
end;

function TCTDirectoryCachePool.FindCompiledUnitInCompletePath(
  const Directory: string; var AnUnitname: string; AnyCase: boolean): string;
var
  Cache: TCTDirectoryCache;
begin
  Cache:=GetCache(Directory,true,false);
  Result:=Cache.FindCompiledUnitInCompletePath(AnUnitname,AnyCase);
end;

function TCTDirectoryCachePool.FindCompiledUnitInPath(const BaseDirectory,
  UnitPath, AnUnitname: string; AnyCase: boolean): string;
var
  StartPos: Integer;
  l: Integer;
  p: Integer;
  CurPath: String;
  Cache: TCTDirectoryBaseCache;
  ShortFilename: String;
  SearchCase: TCTSearchFileCase;
  Base: String;
begin
  Result:='';
  Base:=AppendPathDelim(TrimFilename(BaseDirectory));
  // search in search path
  StartPos:=1;
  l:=length(UnitPath);
  ShortFilename:=AnUnitname+'.ppu';
  if AnyCase then
    SearchCase:=ctsfcAllCase
  else
    SearchCase:=ctsfcLoUpCase;
  while StartPos<=l do begin
    p:=StartPos;
    while (p<=l) and (UnitPath[p]<>';') do inc(p);
    CurPath:=TrimFilename(copy(UnitPath,StartPos,p-StartPos));
    if CurPath<>'' then begin
      if not FilenameIsAbsolute(CurPath) then
        CurPath:=Base+CurPath;
      if FilenameIsAbsolute(CurPath) then begin
        Cache:=GetBaseCache(CurPath,true);
        Result:=Cache.FindFile(ShortFilename,SearchCase);
        if Result<>'' then begin
          Result:=AppendPathDelim(CurPath)+Result;
          exit;
        end;
      end;
    end;
    StartPos:=p+1;
  end;
end;

{ TCTDirectoryListing }

destructor TCTDirectoryListing.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TCTDirectoryListing.Clear;
begin
  if Starts<>nil then begin
    FreeMem(Starts);
    Starts:=nil;
    Size:=0;
    FreeMem(Files);
    Files:=nil;
    Count:=0;
  end;
end;

function TCTDirectoryListing.CalcMemSize: PtrUInt;
begin
  Result:=PtrUInt(InstanceSize)
  {%H-}+SizeOf(Pointer)*Count  // Starts
    +PtrUInt(Size); // Files
end;

function TCTDirectoryListing.GetFilename(Index: integer): PChar;

  procedure RaiseIndexOutOfBounds;
  begin
    raise Exception.Create('TCTDirectoryListing.GetFilename: Index out of bounds');
  end;

begin
  if (Index<0) or (Index>=Count) then
    RaiseIndexOutOfBounds;
  Result:=@Files[Starts[Index]+DirListNameOffset];
end;

function TCTDirectoryListing.GetTime(Index: integer): TCTFileAgeTime;

  procedure RaiseIndexOutOfBounds;
  begin
    raise Exception.Create('TCTDirectoryListing.GetTime: Index out of bounds');
  end;

begin
  if (Index<0) or (Index>=Count) then
    RaiseIndexOutOfBounds;
  Result:=PCTDirectoryListingHeader(@Files[Starts[Index]])^.Time;
end;

function TCTDirectoryListing.GetAttr(Index: integer): TCTDirectoryListingAttr;

  procedure RaiseIndexOutOfBounds;
  begin
    raise Exception.Create('TCTDirectoryListing.GetAttr: Index out of bounds');
  end;

begin
  if (Index<0) or (Index>=Count) then
    RaiseIndexOutOfBounds;
  Result:=PCTDirectoryListingHeader(@Files[Starts[Index]])^.Attr;
end;

function TCTDirectoryListing.GetSize(Index: integer): TCTDirectoryListingSize;

  procedure RaiseIndexOutOfBounds;
  begin
    raise Exception.Create('TCTDirectoryListing.GetSize: Index out of bounds');
  end;

begin
  if (Index<0) or (Index>=Count) then
    RaiseIndexOutOfBounds;
  Result:=PCTDirectoryListingHeader(@Files[Starts[Index]])^.Size;
end;

{ TUnitFileNameLink }

function TUnitFileNameLink.CalcMemSize: PtrUInt;
begin
  Result:=PtrUInt(InstanceSize)
    +MemSizeString(Unit_Name)
    +MemSizeString(Filename);
end;

end.

