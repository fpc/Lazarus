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

interface

uses
  // RTL + FCL
  Classes, SysUtils, Laz_AVL_Tree,
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
    ctdcsNamespaces
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


  { TCTDirectoryCache }

  TCTDirectoryCache = class
  private
    FDirectory: string;
    FPool: TCTDirectoryCachePool;
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
    constructor Create(const TheDirectory: string;
                       ThePool: TCTDirectoryCachePool);
    destructor Destroy; override;
    procedure CalcMemSize(Stats: TCTMemStats);
    procedure Reference;
    procedure Release;
    function IndexOfFileCaseInsensitive(ShortFilename: PChar): integer;
    function IndexOfFileCaseSensitive(ShortFilename: PChar): integer;
    function FindUnitLink(const AUnitName: string): string;
    function FindUnitInUnitSet(const AUnitName: string;
                               SrcSearchRequiresPPU: boolean = true): string;
    function FindCompiledUnitInUnitSet(const AUnitName: string): string;
    function FindFile(const ShortFilename: string;
                      const FileCase: TCTSearchFileCase): string;
    function FileAge(const ShortFilename: string): TCTFileAgeTime;
    function FileAttr(const ShortFilename: string): TCTDirectoryListingAttr;
    function FileSize(const ShortFilename: string): TCTDirectoryListingSize;
    function FindUnitSource(const AUnitName: string; AnyCase: boolean): string;
    function FindUnitSourceInCleanSearchPath(const AUnitName,
                                  SearchPath: string; AnyCase: boolean): string;
    function FindUnitSourceInCompletePath(var AUnitName, InFilename: string;
               AnyCase: boolean; FPCSrcSearchRequiresPPU: boolean = false;
               const AddNameSpaces: string = ''): string;
    function FindCompiledUnitInCompletePath(const AnUnitname: string;
                                            AnyCase: boolean): string;
    procedure IterateFPCUnitsInSet(const Iterate: TCTOnIterateFile);
    procedure UpdateListing;
    procedure WriteListing;
    procedure Invalidate; inline;
    procedure GetFiles(var Files: TStrings; IncludeDirs: boolean = true); // relative to Directory
  public
    property Directory: string read FDirectory;
    property RefCount: integer read FRefCount;
    property Pool: TCTDirectoryCachePool read FPool;
    property Strings[const AStringType: TCTDirCacheString]: string read GetStrings write SetStrings;
    property Listing: TCTDirectoryListing read FListing;
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
    FOnFindVirtualFile: TCTDirCacheFindVirtualFile;
    FOnGetCompiledUnitFromSet: TCTGetCompiledUnitFromSet;
    FOnGetString: TCTDirCacheGetString;
    FOnGetUnitFromSet: TCTGetUnitFromSet;
    FOnIterateFPCUnitsFromSet: TCTIterateFPCUnitsFromSet;
    procedure DoRemove(ACache: TCTDirectoryCache);
    procedure OnFileStateCacheChangeTimeStamp(Sender: TObject;
                                              const AFilename: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure CalcMemSize(Stats: TCTMemStats);
    procedure GetListing(const aDirectory: string; var Files: TStrings;
                         IncludeDirs: boolean = true); // relative to Directory
    function GetCache(const Directory: string;
                      CreateIfNotExists: boolean = true;
                      DoReference: boolean = true): TCTDirectoryCache;
    function GetString(const Directory: string; AStringType: TCTDirCacheString;
                       UseCache: boolean = true): string;
    procedure IncreaseFileTimeStamp; inline;
    procedure IncreaseConfigTimeStamp; inline;
    function FileExists(Filename: string): boolean;
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
    function FindUnitInDirectory(const Directory, AUnitName: string;
                                 AnyCase: boolean = false): string;
    function FindVirtualFile(const Filename: string): string;
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
  end;
  
function CompareCTDirectoryCaches(Data1, Data2: Pointer): integer;
function CompareAnsiStringAndDirectoryCache(Dir, Cache: Pointer): integer;

function ComparePCharFirstCaseInsAThenCase(Data1, Data2: Pointer): integer; // insensitive ASCII then byte wise
function ComparePCharCaseInsensitiveA(Data1, Data2: Pointer): integer; // insensitive ASCII
function ComparePCharCaseSensitive(Data1, Data2: Pointer): integer; // byte wise

// unit links
function SearchUnitInUnitLinks(const UnitLinks, TheUnitName: string;
  var UnitLinkStart, UnitLinkEnd: integer; out Filename: string): boolean;
function CreateUnitLinksTree(const UnitLinks: string): TAVLTree; // tree of TUnitFileNameLink
function CompareUnitLinkNodes(NodeData1, NodeData2: Pointer): integer;
function CompareUnitNameWithUnitLinkNode(AUnitName: Pointer;
  NodeData: pointer): integer;

implementation

const
  NameOffset = SizeOf(TCTDirectoryListingHeader);
type
  TWorkFileInfo = record
    Header: TCTDirectoryListingHeader;
    FileName: string;
  end;
  PWorkFileInfo = ^TWorkFileInfo;
  PPWorkFileInfo = ^PWorkFileInfo;

function CompareWorkFileInfos(Data1, Data2: Pointer): integer;
var
  Info1: PWorkFileInfo absolute Data1;
  Info2: PWorkFileInfo absolute Data2;
begin
  Result:=ComparePCharFirstCaseInsAThenCase(PChar(Info1^.Filename),PChar(Info2^.Filename));
end;

function CompareCTDirectoryCaches(Data1, Data2: Pointer): integer;
begin
  Result:=CompareFilenames(TCTDirectoryCache(Data1).FDirectory,
                           TCTDirectoryCache(Data2).FDirectory);
end;

function CompareAnsiStringAndDirectoryCache(Dir, Cache: Pointer): integer;
begin
  Result:=CompareFilenames(AnsiString(Dir),TCTDirectoryCache(Cache).FDirectory);
end;

function ComparePCharFirstCaseInsAThenCase(Data1, Data2: Pointer): integer;
begin
  Result:=ComparePCharCaseInsensitiveA(Data1,Data2);
  if Result=0 then
    Result:=ComparePCharCaseSensitive(Data1,Data2);
end;

function ComparePCharCaseInsensitiveA(Data1, Data2: Pointer): integer;
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

function ComparePCharCaseInsensitiveA(Data1, Data2: Pointer; MaxCount: PtrInt): integer;
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
    a.b is prefix of a.b.c.d, A.b.c, a.b.c
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
      if (cf<>'.') then
        Result:=ord('.')-ord(cf)
      else
        Result:=0;
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
      and (ComparePCharCaseInsensitiveA(Pointer(TheUnitName),@UnitLinks[UnitLinkStart],
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
            if CompareFileExt(Filename,CTPascalExtension[pe],false)<>0 then
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
      if LazIsValidIdent(TheUnitName,true,true) then begin
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
  if Directory='' then exit;// virtual directory
  
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
          FillChar(WorkingListing[WorkingListingCount],
                   SizeOf(TWorkFileInfo)*(NewCapacity-WorkingListingCapacity),0);
          WorkingListingCapacity:=NewCapacity;
        end;
        WorkingItem:=@WorkingListing[WorkingListingCount];
        WorkingItem^.Header.Time:=FileInfo.Time;
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
    Assert(Assigned(FListing), 'TCTDirectoryCache.UpdateListing: FListing=Nil.');
    GetMem(FListing.Files,TotalLen);
    FListing.Size:=TotalLen;
    FListing.Count:=WorkingListingCount;
    GetMem(FListing.Starts,SizeOf(PChar)*WorkingListingCount);
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
  
  procedure RaiseDirNotAbsolute;
  begin
    raise Exception.Create('directory not absolute "'+FDirectory+'"');
  end;
  
begin
  FDirectory:=AppendPathDelim(TrimFilename(TheDirectory));
  if FDirectory='.' then FDirectory:='';
  if (FDirectory<>'') and not FilenameIsAbsolute(FDirectory) then
    RaiseDirNotAbsolute;
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
  UpdateListing;
  Files:=FListing.Files;
  if Files=nil then exit(-1);
  l:=0;
  r:=FListing.Count-1;
  while r>=l do begin
    m:=(l+r) shr 1;
    CurFilename:=@Files[FListing.Starts[m]+NameOffset];
    cmp:=ComparePCharCaseInsensitiveA(ShortFilename,CurFilename);
    if cmp>0 then
      l:=m+1
    else if cmp<0 then
      r:=m-1
    else begin
      Result:=m;
      exit;
    end;
  end;
  Result:=-1;
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
  UpdateListing;
  Files:=FListing.Files;
  if Files=nil then exit(-1);
  l:=0;
  r:=FListing.Count-1;
  while r>=l do begin
    m:=(l+r) shr 1;
    CurFilename:=@Files[FListing.Starts[m]+NameOffset];
    cmp:=ComparePCharFirstCaseInsAThenCase(ShortFilename,CurFilename);// pointer type cast avoids #0 check
    if cmp>0 then
      l:=m+1
    else if cmp<0 then
      r:=m-1
    else begin
      Result:=m;
      exit;
    end;
  end;
  Result:=-1;
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
      if CompareFileExt(Result,CTPascalExtension[pe],false)<>0 then
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
  l: Integer;
  r: Integer;
  m: Integer;
  cmp: LongInt;
  CurFilename: PChar;
  Files: PChar;
  ExtStartPos: PChar;
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
    // binary search the nearest filename
    {$IFDEF DebugDirCacheFindUnitSource}
    if (CompareText(AUnitName,DebugUnitName)=0) and (System.Pos(DebugDirPart,directory)>0) then
      WriteListing;
    {$ENDIF}
    
    l:=0;
    r:=FListing.Count-1;
    while r>=l do begin
      m:=(l+r) shr 1;
      CurFilename:=@Files[FListing.Starts[m]+NameOffset];
      cmp:=ComparePCharUnitNameWithFilename(Pointer(AUnitName),CurFilename);
      if cmp>0 then
        l:=m+1
      else if cmp<0 then
        r:=m-1
      else
        break;
    end;
    if cmp<>0 then exit;
    // m is now on a filename with the right prefix
    // go to the first pascal unit with the right unit name
    while (m>0)
    and (ComparePCharUnitNameWithFilename(Pointer(AUnitName),
                                     @Files[FListing.Starts[m-1]+NameOffset])=0)
    do
      dec(m);
    // -> now find a filename with correct case and extension
    while m<FListing.Count do begin
      CurFilename:=@Files[FListing.Starts[m]+NameOffset];
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

function TCTDirectoryCache.FindUnitSourceInCompletePath(var AUnitName,
  InFilename: string; AnyCase: boolean; FPCSrcSearchRequiresPPU: boolean;
  const AddNameSpaces: string): string;

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

    if Pos('.',AUnitName)<1 then begin
      // generic unit -> search with namespaces
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
          if LazIsValidIdent(aNameSpace,true,true) then begin
            aName:=aNameSpace+'.'+AUnitName;
            Result:=FindUnitSourceInCompletePath(aName,InFilename,AnyCase,
              FPCSrcSearchRequiresPPU,'');
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
      end;
      if Result='' then begin
        // search in search path
        SrcPath:=Strings[ctdcsCompleteSrcPath];
        Result:=FindUnitSourceInCleanSearchPath(AUnitName,SrcPath,AnyCase);
      end;
      if Result='' then begin
        // search in unit set
        {$IFDEF ShowTriedUnits}
        DebugLn(['TCTDirectoryCache.FindUnitSourceInCompletePath unit ',AUnitName,' not found in SrcPath="',SrcPath,'"  Directory="',Directory,'" searchin in unitset ...']);
        {$ENDIF}
        Result:=FindUnitInUnitSet(AUnitName,FPCSrcSearchRequiresPPU);
        {$IFDEF ShowTriedUnits}
        if Result='' then begin
          DebugLn(['TCTDirectoryCache.FindUnitSourceInCompletePath unit ',AUnitName,' not found in unitlinks. Directory="',Directory,'"']);
        end;
        {$ENDIF}
      end;

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
    Filename:=@FListing.Files[FListing.Starts[i]+NameOffset];
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
      Files.Add(PChar(p+NameOffset));
  end;
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

constructor TCTDirectoryCachePool.Create;
begin
  FDirectories:=TAVLTree.Create(@CompareCTDirectoryCaches);
  IncreaseFileTimeStamp;
  IncreaseConfigTimeStamp;
  if FileStateCache<>nil then
    FileStateCache.AddChangeTimeStampHandler(@OnFileStateCacheChangeTimeStamp);
end;

destructor TCTDirectoryCachePool.Destroy;
var
  Cache: TCTDirectoryCache;
begin
  if FileStateCache<>nil then
    FileStateCache.RemoveChangeTimeStampHandler(@OnFileStateCacheChangeTimeStamp);
  while FDirectories.Root<>nil do begin
    Cache:=TCTDirectoryCache(FDirectories.Root.Data);
    if Cache.RefCount<>1 then
      raise Exception.Create('TCTDirectoryCachePool.Destroy');
    Cache.Release;
  end;
  FDirectories.Free;
  inherited Destroy;
end;

procedure TCTDirectoryCachePool.CalcMemSize(Stats: TCTMemStats);
var
  Node: TAVLTreeNode;
begin
  Stats.Add('TCTDirectoryCachePool',PtrUInt(InstanceSize));
  Stats.Add('TCTDirectoryCachePool.Count',FDirectories.Count);
  Node:=FDirectories.FindLowest;
  while Node<>nil do begin
    TCTDirectoryCache(Node.Data).CalcMemSize(Stats);
    Node:=FDirectories.FindSuccessor(Node);
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
var
  Directory: String;
  Cache: TCTDirectoryCache;
  ShortFilename: String;
begin
  Filename:=TrimFilename(Filename);
  if Filename='' then exit(false);
  if FilenameIsAbsolute(Filename) then begin
    ShortFilename:=ExtractFilename(Filename);
    if (ShortFilename<>'') and (ShortFilename<>'.') and (ShortFilename<>'..')
    then begin
      Directory:=ExtractFilePath(Filename);
      Cache:=GetCache(Directory,true,false);
      Result:=Cache.FindFile(ShortFilename,ctsfcDefault)<>'';
      exit;
    end;
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

function TCTDirectoryCachePool.FindUnitInDirectory(const Directory,
  AUnitName: string; AnyCase: boolean): string;
var
  Cache: TCTDirectoryCache;
begin
  Cache:=GetCache(Directory,true,false);
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

function TCTDirectoryCachePool.FindVirtualUnit(const AUnitName: string): string;
var
  e: TCTPascalExtType;
  CurUnitName:String;
begin
  // search normal
  for e:=Low(CTPascalExtension) to High(CTPascalExtension) do begin
    if CTPascalExtension[e]='' then continue;
    Result:=FindVirtualFile(AUnitName+CTPascalExtension[e]);
    if Result<>'' then exit;
  end;
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
end;

function TCTDirectoryCachePool.FindUnitSourceInCompletePath(
  const Directory: string; var AUnitName, InFilename: string; AnyCase: boolean): string;
var
  Cache: TCTDirectoryCache;
begin
  Cache:=GetCache(Directory,true,false);
  Result:=Cache.FindUnitSourceInCompletePath(AUnitName,InFilename,AnyCase);
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
  Cache: TCTDirectoryCache;
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
        Cache:=GetCache(CurPath,true,false);
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
  Result:=@Files[Starts[Index]+NameOffset];
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

