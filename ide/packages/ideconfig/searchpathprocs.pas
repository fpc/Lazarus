{
  Functions for search paths maintained by the IDE, e.g. UnitPaths, IncludePath.

  The Lazarus IDE has some special rules for search paths:
  - Search paths are separated by semicolon
  - It uses TrimAndExpandFilename to trim leading and trailing spaces and expand ~ on Unix
  - It uses ResolveDots to normalize e.g. /foo/../bar to /bar and merges // to /
  - It normalizes AllowDirectorySeparators to PathDelim
  - A $(macro) at start is treated as an absolute filename
  - Star directories:
    /path/* matches all direct sub directories  /path/* (similar to fpc)
    /path/** matches all sub directories  /path/**
}
unit SearchPathProcs;

{$mode objfpc}{$H+}
{$ScopedEnums on}
{$ModeSwitch advancedrecords}

{$IF defined(Windows) or defined(darwin) or defined(HASAMIGA)}
{$define CaseInsensitiveFilenames}
{$IFDEF Windows}
  {$define HasUNCPaths}
{$ENDIF}
{$ENDIF}
{$IF defined(CaseInsensitiveFilenames)}
  {$define NotLiteralFilenames} // e.g. HFS+ normalizes file names
{$ENDIF}

interface

uses
  Classes, SysUtils,
  // LazUtils
  LazFileUtils, LazFileCache, FileUtil, AvgLvlTree, CompOptsIntf,
  CodeToolManager, DirectoryCacher, FileProcs;

type
  TSPMaskType = (
    None,
    Star, // matching all direct sub directories  /path/*, excluding /path itself
    StarStar // matching . and all sub directories  /path/**, including /path itself
    );

  TSPFileMaskRelation = (
    None,
    Equal,
    LeftMoreGeneral, // e.g. left is * and right is path
    RightMoreGeneral // e.g. right is ** and left is *
    );

  { TSPMaskRecord }

  TSPMaskRecord = record
    Len: integer;
    StartPos: PChar;
    EndPos: PChar; // without trailing PathDelim
    PathDelimCount: integer;
    LastPathDelim: PChar; // nil if no pathdelim
    MaskType: TSPMaskType;
    function FindPathDelim(Index: integer{starting at 1}): PChar;
  end;

// search paths
function TrimSearchPath(const SearchPath, BaseDirectory: string;
                  DeleteDoubles: boolean = false; ExpandPaths: boolean = false): string;
function MergeSearchPaths(const OldSearchPath, AddSearchPath: string): string;
procedure MergeSearchPaths(SearchPath: TStrings; const AddSearchPath: string);
function RemoveSearchPaths(const SearchPath, RemoveSearchPath: string): string;
function RemoveNonExistingPaths(const SearchPath, BaseDirectory: string): string;
function RebaseSearchPath(const SearchPath,
                          OldBaseDirectory, NewBaseDirectory: string;
                          SkipPathsStartingWithMacro: boolean): string;
function ShortenSearchPath(const SearchPath, BaseDirectory,
                           ChompDirectory: string): string;
function FormatSearchPath(const SearchPath: string;
                MaxLen: integer = 0): string;
function GetNextDirectoryInSearchPath(const SearchPath: string;
                                      var NextStartPos: integer): string;
function GetNextUsedDirectoryInSearchPath(const SearchPath,
                          FilterDir: string; var NextStartPos: integer): string;
function SearchPathToList(const SearchPath: string): TStringList;
function SearchDirectoryInSearchPath(const SearchPath, Directory: string;
    out DirRelation: TSPFileMaskRelation; DirStartPos: integer = 1): integer; overload;
function SearchDirectoryInSearchPath(const SearchPath, Directory: string;
                                     DirStartPos: integer = 1): integer; overload;
function SearchDirectoryInSearchPath(SearchPath: TStrings;
                    const Directory: string; DirStartPos: integer = 0): integer; overload;
function SearchDirectoryInMaskedSearchPath(const SearchPath, Directory: string;
                                   DirStartPos: integer = 1): integer; overload;

type
  TSPSearchFileFlag = (
    DontSearchInBasePath, // do not search in BasePath, search only in SearchPath.
    SearchLoUpCase,
    Executable // file must be executable
    );
  TSPSearchFileFlags = set of TSPSearchFileFlag;

function SearchFileInSearchPath(const Filename, BasePath: string;
  SearchPath: string; Flags: TSPSearchFileFlags = []): string; overload;
function SearchUnitInSearchPath(const AnUnitname, BasePath: string;
  SearchPath: string; AnyCase: boolean): string; overload;
procedure CollectFilesInSearchPath(const SearchPath: string;
                   Files: TFilenameToStringTree; const Value: string = ''); overload;

function FileIsInSPDirectory(const Filename: string; Directory{without **}: string;
                             MaskType: TSPMaskType): boolean; overload; // both must be ResolveDots
function FileIsInSPDirectory(const Filename: string; Directory{with **}: string): boolean; overload; // both must be ResolveDots

function FilenamePIsAbsolute(TheFilename: PChar): boolean;
function FilenamePIsUnixAbsolute(TheFilename: PChar): boolean;
function FilenamePIsWinAbsolute(TheFilename: PChar): boolean;

function RelateDirectoryMasks(const LeftDir: string; LeftStart: integer; const RightDir: string; RightStart: integer): TSPFileMaskRelation; overload;
function RelateDirectoryMasks(const Left, Right: TSPMaskRecord): TSPFileMaskRelation; overload;
function GetSPMaskRecord(const aDirectory: string; aStartPos: integer; out MaskRecord: TSPMaskRecord): boolean;
function GetSPMaskType(const aFilename: string): TSPMaskType;

function dbgs(t: TSPMaskType): string; overload;
function dbgs(r: TSPFileMaskRelation): string; overload;

implementation

{-------------------------------------------------------------------------------
  function TrimSearchPath(const SearchPath, BaseDirectory: string): boolean;

  - Removes empty paths.
  - Uses TrimFilename on every path.
  - If BaseDirectory<>'' then every relative Filename will be expanded.
  - removes doubles
-------------------------------------------------------------------------------}
function TrimSearchPath(const SearchPath, BaseDirectory: string;
  DeleteDoubles: boolean; ExpandPaths: boolean): string;
var
  CurPath: String;
  EndPos: Integer;
  StartPos: Integer;
  len, OtherStartPos: Integer;
  BaseDir: String;
  DirRelation: TSPFileMaskRelation;
begin
  Result:='';
  EndPos:=1;
  len:=length(SearchPath);
  BaseDir:=AppendPathDelim(TrimFilename(BaseDirectory));
  while EndPos<=len do
  begin
    StartPos:=EndPos;
    // skip empty paths and space chars at start
    while (StartPos<=len) and (SearchPath[StartPos] in [';',#0..#32]) do
      inc(StartPos);
    if StartPos>len then break;
    EndPos:=StartPos;
    while (EndPos<=len) and (SearchPath[EndPos]<>';') do inc(EndPos);
    CurPath:=copy(SearchPath,StartPos,EndPos-StartPos);
    if CurPath<>'' then
    begin
      // non empty path => expand, trim and normalize
      if ExpandPaths then
        CurPath:=TrimAndExpandDirectory(CurPath,BaseDir)
      else if (BaseDir<>'') and (not FilenameIsAbsolute(CurPath)) then
        CurPath:=BaseDir+CurPath;
      CurPath:=ChompPathDelim(ResolveDots(CurPath));
      if CurPath='' then CurPath:='.';
      if DeleteDoubles then
      begin
        // check if path already exists
        OtherStartPos:=SearchDirectoryInSearchPath(Result,CurPath,DirRelation);
        if OtherStartPos>0 then
          case DirRelation of
          TSPFileMaskRelation.Equal,
          TSPFileMaskRelation.LeftMoreGeneral:
            continue; // already exists -> skip
          TSPFileMaskRelation.RightMoreGeneral:
            ; // first search in a specific, then in all -> keep
          end;
      end;
      if Result<>'' then
        CurPath:=';'+CurPath;
      Result:=Result+CurPath;
    end;
  end;
end;

function MergeSearchPaths(const OldSearchPath, AddSearchPath: string): string;
var
  l: Integer;
  EndPos: Integer;
  StartPos: Integer;
  NewPath: String;
begin
  Result:=OldSearchPath;
  if Result='' then begin
    Result:=AddSearchPath;
    exit;
  end;
  l:=length(AddSearchPath);
  EndPos:=1;
  while EndPos<=l do begin
    StartPos:=EndPos;
    while (AddSearchPath[StartPos]=';') do begin
      inc(StartPos);
      if StartPos>l then exit;
    end;
    EndPos:=StartPos;
    while (EndPos<=l) and (AddSearchPath[EndPos]<>';') do inc(EndPos);
    if SearchDirectoryInSearchPath(Result,AddSearchPath,StartPos)<1 then
    begin
      // new path found -> add
      NewPath:=copy(AddSearchPath,StartPos,EndPos-StartPos);
      if Result<>'' then
        NewPath:=';'+NewPath;
      Result:=Result+NewPath;
    end;
  end;
end;

procedure MergeSearchPaths(SearchPath: TStrings; const AddSearchPath: string);
var
  l: Integer;
  EndPos: Integer;
  StartPos: Integer;
begin
  l:=length(AddSearchPath);
  EndPos:=1;
  while EndPos<=l do begin
    StartPos:=EndPos;
    while (AddSearchPath[StartPos]=';') do begin
      inc(StartPos);
      if StartPos>l then exit;
    end;
    EndPos:=StartPos;
    while (EndPos<=l) and (AddSearchPath[EndPos]<>';') do inc(EndPos);
    if SearchDirectoryInSearchPath(SearchPath,AddSearchPath,StartPos)<1 then
    begin
      // new path found -> add
      SearchPath.Add(copy(AddSearchPath,StartPos,EndPos-StartPos));
    end;
  end;
end;

function RemoveSearchPaths(const SearchPath, RemoveSearchPath: string): string;
var
  OldPathLen: Integer;
  EndPos: Integer;
  StartPos: Integer;
  ResultStartPos: Integer;
begin
  Result:=SearchPath;
  OldPathLen:=length(SearchPath);
  EndPos:=1;
  ResultStartPos:=1;
  repeat
    StartPos:=EndPos;
    while (StartPos<=OldPathLen) and (SearchPath[StartPos]=';') do
      inc(StartPos);
    if StartPos>OldPathLen then break;
    EndPos:=StartPos;
    while (EndPos<=OldPathLen) and (SearchPath[EndPos]<>';') do
      inc(EndPos);
    //DebugLn('RemoveSearchPaths Dir="',copy(SearchPath,StartPos,EndPos-StartPos),'" RemoveSearchPath="',RemoveSearchPath,'"');
    if SearchDirectoryInSearchPath(RemoveSearchPath,SearchPath,StartPos)>0 then
    begin
      // remove path -> skip
    end else begin
      // keep path -> copy
      if ResultStartPos>1 then begin
        Result[ResultStartPos]:=';';
        inc(ResultStartPos);
      end;
      while StartPos<EndPos do begin
        Result[ResultStartPos]:=SearchPath[StartPos];
        inc(ResultStartPos);
        inc(StartPos);
      end;
    end;
  until false;
  SetLength(Result,ResultStartPos-1);
end;

function RemoveNonExistingPaths(const SearchPath, BaseDirectory: string): string;
var
  StartPos: Integer;
  EndPos: LongInt;
  CurPath: String;
  MacroStartPos: LongInt;
begin
  Result:=SearchPath;
  StartPos:=1;
  while StartPos<=length(Result) do begin
    EndPos:=StartPos;
    while (EndPos<=length(Result)) and (Result[EndPos]=';') do inc(EndPos);
    if EndPos>StartPos then begin
      // empty paths, e.g. ;;;;
      // remove
      Result:=copy(Result,1,StartPos-1)+copy(Result,EndPos,length(Result));
      EndPos:=StartPos;
    end;
    while (EndPos<=length(Result)) and (Result[EndPos]<>';') do inc(EndPos);

    CurPath:=copy(Result,StartPos,EndPos-StartPos);

    // cut macros
    MacroStartPos:=System.Pos('$(',CurPath);
    if MacroStartPos>0 then begin
      CurPath:=copy(CurPath,1,MacroStartPos-1);
      if (CurPath<>'') and (CurPath[length(CurPath)]<>PathDelim) then
        CurPath:=ExtractFilePath(CurPath);
    end;

    // make path absolute
    if (CurPath<>'') and (not FilenameIsAbsolute(CurPath)) then
      CurPath:=AppendPathDelim(BaseDirectory)+CurPath;

    case ExtractFilename(CurPath) of
    '*','**': CurPath:=ExtractFilePath(CurPath);
    end;

    if ((CurPath='') and (MacroStartPos<1))
    or (not DirPathExistsCached(CurPath)) then begin
      // path does not exist -> remove
      Result:=copy(Result,1,StartPos-1)+copy(Result,EndPos+1,length(Result));
      EndPos:=StartPos;
    end else begin
      StartPos:=EndPos+1;
    end;
  end;
end;

function RebaseSearchPath(const SearchPath, OldBaseDirectory,
  NewBaseDirectory: string; SkipPathsStartingWithMacro: boolean): string;
// change every relative search path
var
  EndPos: Integer;
  StartPos: Integer;
  CurPath: String;
begin
  Result:=SearchPath;
  if CompareFilenames(OldBaseDirectory,NewBaseDirectory)=0 then exit;
  EndPos:=1;
  repeat
    StartPos:=EndPos;
    while (StartPos<=length(Result)) and (Result[StartPos]=';') do
      inc(StartPos);
    if StartPos>length(Result) then break;
    EndPos:=StartPos;
    while (EndPos<=length(Result)) and (Result[EndPos]<>';') do
      inc(EndPos);
    if EndPos>StartPos then begin
      CurPath:=copy(Result,StartPos,EndPos-StartPos);
      if (not FilenameIsAbsolute(CurPath))
      and ((not SkipPathsStartingWithMacro) or (CurPath[1]<>'$'))
      then begin
        CurPath:=TrimFilename(AppendPathDelim(OldBaseDirectory)+CurPath);
        CurPath:=CreateRelativePath(CurPath,NewBaseDirectory);
        Result:=copy(Result,1,StartPos-1)+CurPath
                   +copy(Result,EndPos,length(Result));
        EndPos:=StartPos+length(CurPath);
      end;
    end;
  until false;
end;

function ShortenSearchPath(const SearchPath, BaseDirectory,
  ChompDirectory: string): string;
// Every search path that is a subdirectory of ChompDirectory will be shortened.
// Before the test relative paths are expanded by BaseDirectory.
var
  BaseEqualsChompDir: boolean;

  function Normalize(var ADirectory: string): boolean;
  begin
    if FilenameIsAbsolute(ADirectory) then begin
      Result:=true;
    end else begin
      if BaseEqualsChompDir then
        Result:=false
      else begin
        Result:=true;
        ADirectory:=AppendPathDelim(BaseDirectory)+ADirectory;
      end;
    end;
    if Result then
      ADirectory:=AppendPathDelim(TrimFilename(ADirectory));
  end;

var
  PathLen: Integer;
  EndPos: Integer;
  StartPos: Integer;
  CurDir: String;
  NewCurDir: String;
  DiffLen: Integer;
begin
  Result:=SearchPath;
  if (SearchPath='') or (ChompDirectory='') then exit;

  PathLen:=length(Result);
  EndPos:=1;
  BaseEqualsChompDir:=CompareFilenames(BaseDirectory,ChompDirectory)=0;
  while EndPos<=PathLen do begin
    StartPos:=EndPos;
    while (Result[StartPos] in [';',#0..#32]) do begin
      inc(StartPos);
      if StartPos>PathLen then exit;
    end;
    EndPos:=StartPos;
    while (EndPos<=PathLen) and (Result[EndPos]<>';') do inc(EndPos);
    CurDir:=copy(Result,StartPos,EndPos-StartPos);
    NewCurDir:=CurDir;
    if Normalize(NewCurDir) then begin
      if CompareFilenames(NewCurDir,ChompDirectory)=0 then
        NewCurDir:='.'
      else if FileIsInPath(NewCurDir,ChompDirectory) then
        NewCurDir:=AppendPathDelim(CreateRelativePath(NewCurDir,BaseDirectory));
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

function FormatSearchPath(const SearchPath: string; MaxLen: integer): string;
var
  p: Integer;
begin
  if MaxLen=0 then
    MaxLen:=TLazCompilerOptions.ConsoleParamsMax;
  p:=1;
  repeat
    while (p<=length(SearchPath)) and (SearchPath[p]<>';') do inc(p);
    if p>MaxLen then
    begin
      if p<=length(SearchPath) then
      begin
        Result:=LeftStr(SearchPath,p-1)+'...';
        exit;
      end;
    end;
    inc(p);
  until p>length(SearchPath);
  Result:=SearchPath;
end;

function GetNextDirectoryInSearchPath(const SearchPath: string;
                                      var NextStartPos: integer): string;
var
  PathLen: Integer;
  CurStartPos: Integer;
begin
  PathLen:=length(SearchPath);
  if PathLen>0 then begin
    repeat
      while (NextStartPos<=PathLen)
      and (SearchPath[NextStartPos] in [';',#0..#32]) do
        inc(NextStartPos);
      CurStartPos:=NextStartPos;
      while (NextStartPos<=PathLen) and (SearchPath[NextStartPos]<>';') do
        inc(NextStartPos);
      Result:=TrimFilename(copy(SearchPath,CurStartPos,NextStartPos-CurStartPos));
      if Result<>'' then exit;
    until (NextStartPos>PathLen);
  end else begin
    NextStartPos:=1;
  end;
  Result:='';
end;

function GetNextUsedDirectoryInSearchPath(const SearchPath,
                    FilterDir: string; var NextStartPos: integer): string;
// searches next directory in search path,
// which is equal to FilterDir or is in FilterDir
begin
  while (NextStartPos<=length(SearchPath)) do begin
    Result:=GetNextDirectoryInSearchPath(SearchPath,NextStartPos);
    if (Result<>'') and PathIsInPath(Result,FilterDir) then
      exit;
  end;
  Result:=''
end;

function SearchPathToList(const SearchPath: string): TStringList;
var
  p: Integer;
  CurDir: String;
begin
  Result:=TStringList.Create;
  p:=1;
  repeat
    CurDir:=GetNextDirectoryInSearchPath(SearchPath,p);
    if CurDir='' then break;
    Result.Add(CurDir);
  until false;
end;

function SearchDirectoryInSearchPath(const SearchPath, Directory: string; out
  DirRelation: TSPFileMaskRelation; DirStartPos: integer): integer;
// -1 on not found
var
  Dir, Cur: TSPMaskRecord;
  PathLen: SizeInt;
  StartPos: Integer;
begin
  Result:=-1;
  DirRelation:=TSPFileMaskRelation.None;

  if not GetSPMaskRecord(Directory,DirStartPos,Dir) then exit;

  PathLen:=length(SearchPath);
  StartPos:=1;
  while StartPos<=PathLen do begin
    // skip spaces and empty paths
    while (SearchPath[StartPos] in [';',#0..#32]) do begin
      inc(StartPos);
      if StartPos>PathLen then exit;
    end;

    // compare paths
    if GetSPMaskRecord(SearchPath,StartPos,Cur) then begin
      DirRelation:=RelateDirectoryMasks(Cur,Dir);
      if DirRelation<>TSPFileMaskRelation.None then
        exit(StartPos);
    end;

    while (StartPos<=PathLen) and (SearchPath[StartPos]<>';') do inc(StartPos);
  end;
end;

function SearchDirectoryInSearchPath(const SearchPath, Directory: string;
  DirStartPos: integer): integer;
// -1 on not found
var
  PathLen: Integer;
  DirLen: Integer;
  EndPos: Integer;
  StartPos: Integer;
  DirEndPos: Integer;
  CurDirLen: Integer;
  CurDirEndPos: Integer;
begin
  Result:=-1;
  DirLen:=length(Directory);
  if (SearchPath='')
  or (Directory='') or (DirStartPos>DirLen) or (Directory[DirStartPos]=';') then
    exit;
  DirEndPos:=DirStartPos;
  while (DirEndPos<=DirLen) and (Directory[DirEndPos]<>';') do inc(DirEndPos);
  // ignore PathDelim at end
  if (DirEndPos>DirStartPos) and (Directory[DirEndPos-1]=PathDelim) then begin
    while (DirEndPos>DirStartPos) and (Directory[DirEndPos-1]=PathDelim) do
      dec(DirEndPos);
    // check if it is the root path '/'
    if DirEndPos=DirStartPos then DirEndPos:=DirStartPos+1;
  end;
  CurDirLen:=DirEndPos-DirStartPos;
  //DebugLn('SearchDirectoryInSearchPath Dir="',copy(Directory,DirStartPos,CurDirLen),'"');
  PathLen:=length(SearchPath);
  EndPos:=1;
  while EndPos<=PathLen do begin
    StartPos:=EndPos;
    while (SearchPath[StartPos] in [';',#0..#32]) do begin
      inc(StartPos);
      if StartPos>PathLen then exit;
    end;
    EndPos:=StartPos;
    while (EndPos<=PathLen) and (SearchPath[EndPos]<>';') do inc(EndPos);
    CurDirEndPos:=EndPos;
    // ignore PathDelim at end
    if (CurDirEndPos>StartPos) and (SearchPath[CurDirEndPos-1]=PathDelim) then
    begin
      while (CurDirEndPos>StartPos) and (SearchPath[CurDirEndPos-1]=PathDelim)
      do
        dec(CurDirEndPos);
      // check if it is the root path '/'
      if CurDirEndPos=StartPos then CurDirEndPos:=StartPos+1;
    end;
    //DebugLn('SearchDirectoryInSearchPath CurDir="',copy(SearchPath,StartPos,CurDirEndPos-StartPos),'"');
    if CurDirEndPos-StartPos=CurDirLen then begin
      // directories have same length -> compare chars
      if FileUtil.CompareFilenames(@SearchPath[StartPos],CurDirLen,
                          @Directory[DirStartPos],CurDirLen,
                          false)=0
      then begin
        // directory found
        Result:=StartPos;
        exit;
      end;
    end;
    StartPos:=EndPos;
  end;
end;

function SearchDirectoryInSearchPath(SearchPath: TStrings;
  const Directory: string; DirStartPos: integer): integer;
var
  DirLen: Integer;
  DirEndPos: Integer;
  CurDirLen: Integer;
  CurPath: string;
  CurPathLen: Integer;
begin
  Result:=-1;
  DirLen:=length(Directory);
  if (SearchPath.Count=0)
  or (Directory='') or (DirStartPos>DirLen) or (Directory[DirStartPos]=';') then
    exit;
  DirEndPos:=DirStartPos;
  while (DirEndPos<=DirLen) and (Directory[DirEndPos]<>';') do inc(DirEndPos);
  // ignore PathDelim at end
  if (DirEndPos>DirStartPos) and (Directory[DirEndPos-1]=PathDelim) then begin
    while (DirEndPos>DirStartPos) and (Directory[DirEndPos-1]=PathDelim) do
      dec(DirEndPos);
    // check if it is the root path '/'
    if DirEndPos=DirStartPos then DirEndPos:=DirStartPos+1;
  end;
  CurDirLen:=DirEndPos-DirStartPos;

  // search in all search paths
  Result:=SearchPath.Count-1;
  while Result>=0 do begin
    CurPath:=SearchPath[Result];
    CurPathLen:=length(CurPath);
    if CurPathLen>0 then
    begin
      while (CurPathLen>1) and (CurPath[CurPathLen]=PathDelim) do dec(CurPathLen);
    end;
    if (CurPathLen>0)
    and (FileUtil.CompareFilenames(@CurPath[1],CurPathLen,
                                   @Directory[DirStartPos],CurDirLen,
                                   false)=0)
    then begin
      // directory found
      exit;
    end;
    dec(Result);
  end;
end;

function SearchDirectoryInMaskedSearchPath(const SearchPath, Directory: string;
  DirStartPos: integer): integer;
var
  DirRelation: TSPFileMaskRelation;
begin
  Result:=SearchDirectoryInSearchPath(SearchPath,Directory,DirRelation,DirStartPos);
end;

function SearchFileInSearchPath(const Filename, BasePath: string;
  SearchPath: string; Flags: TSPSearchFileFlags): string;

  function Fits(const s: string): boolean;
  begin
    Result:=false;
    if s='' then exit;
    if (TSPSearchFileFlag.Executable in Flags) and not FileIsExecutableCached(s) then
      exit;
    SearchFileInSearchPath:=s;
    Result:=true;
  end;

var
  p: Integer;
  CurPath, Base: String;
  Cache: TCTDirectoryBaseCache;
  FileCase: TCTSearchFileCase;
begin
  if Filename='' then
    exit('');
  // check if filename absolute
  if FilenameIsAbsolute(Filename) then begin
    Result:=ResolveDots(Filename);
    if not FileExistsCached(Filename) then
      Result:='';
    exit;
  end;
  if ExtractFilePath(Filename)<>'' then
    exit('');

  if BasePath<>'' then
    Base:=CleanAndExpandDirectory(BasePath)
  else
    Base:='';

  // search in current directory
  if (Base<>'') and not (TSPSearchFileFlag.DontSearchInBasePath in Flags) then
  begin
    Result:=Base+Filename;
    if FileExistsCached(Result) {$IFDEF Unix}and not DirPathExistsCached(Result){$ENDIF} then
      exit;
  end;

  if TSPSearchFileFlag.SearchLoUpCase in Flags then
    FileCase:=ctsfcLoUpCase
  else
    FileCase:=ctsfcDefault;

  p:=1;
  repeat
    CurPath:=GetNextDirectoryInSearchPath(SearchPath,p);
    if CurPath='' then break;
    CurPath:=TrimAndExpandDirectory(CurPath,Base);

    Cache:=CodeToolBoss.DirectoryCachePool.GetBaseCache(CurPath);
    if Cache=nil then continue;
    Result:=Cache.FindFile(Filename,FileCase);
    if Result<>'' then
      if Fits(Cache.Directory+Result) then
        exit;
  until false;
  Result:='';
end;

function SearchUnitInSearchPath(const AnUnitname, BasePath: string;
  SearchPath: string; AnyCase: boolean): string;
var
  Base, CurPath: String;
  p: Integer;
  Cache: TCTDirectoryBaseCache;
begin
  Base:=AppendPathDelim(ExpandFileNameUTF8(BasePath));
  if BasePath<>'' then
  begin
    // search in current directory
    Result:=CodeToolBoss.DirectoryCachePool.FindUnitInDirectory(Base,AnUnitname,AnyCase);
    if Result<>'' then exit;
  end;
  // search in search path
  p:=1;
  repeat
    CurPath:=GetNextDirectoryInSearchPath(SearchPath,p);
    if CurPath='' then break;
    CurPath:=TrimAndExpandDirectory(CurPath,Base);

    Cache:=CodeToolBoss.DirectoryCachePool.GetBaseCache(CurPath);
    if Cache=nil then continue;
    Result:=Cache.FindUnitSource(AnUnitname,AnyCase);
    if Result<>'' then
      exit(Cache.Directory+Result);
  until false;
  Result:='';
end;

procedure CollectFilesInSearchPath(const SearchPath: string;
  Files: TFilenameToStringTree; const Value: string);

  procedure CollectFile(const aFilename: string);
  begin
    if Files.Contains(aFilename) then exit;
    Files.Add(aFilename,Value);
  end;

var
  p, i: Integer;
  Dir: String;
  Cache: TCTDirectoryBaseCache;
  StarCache: TCTStarDirectoryCache;
  DirCache: TCTDirectoryCache;
begin
  p:=1;
  repeat
    Dir:=GetNextDirectoryInSearchPath(SearchPath,p);
    if Dir='' then break;
    Cache:=CodeToolBoss.DirectoryCachePool.GetBaseCache(Dir);
    if Cache=nil then continue;
    Cache.UpdateListing;
    if Cache is TCTStarDirectoryCache then
    begin
      StarCache:=TCTStarDirectoryCache(Cache);
      for i:=0 to StarCache.Listing.Count-1 do
        CollectFile(StarCache.Listing.GetSubDirFilename(i));
    end else if Cache is TCTDirectoryCache then begin
      DirCache:=TCTDirectoryCache(Cache);
      for i:=0 to DirCache.Listing.Count-1 do
        CollectFile(DirCache.Directory+DirCache.Listing.GetFilename(i));
    end;
  until false;
end;

function FileIsInSPDirectory(const Filename: string; Directory: string;
  MaskType: TSPMaskType): boolean;
var
  l: SizeInt;
  p, PathDelimCount: Integer;
begin
  Result:=false;
  if Filename='' then exit;
  l:=length(Filename);
  if (l>1) and (Filename[1]='.') and (Filename[2]='.') then
  begin
    if (l=2) or (Filename[3]=PathDelim) then
      exit; // e.g. '../foo'
  end;

  Directory:=AppendPathDelim(Directory);

  case MaskType of
    TSPMaskType.None:
      if Filename='.' then
        exit(false)
      else
        Result:=CompareFilenames(ExtractFilePath(Filename),Directory)=0;
    TSPMaskType.Star:
      if Directory='' then
      begin
        // test if file is 'something/something'
        p:=1;
        while (p<=l) and (Filename[p]<>PathDelim) do inc(p);
        if (p=1) or (p>l) then exit;
        inc(p);
        while (p<=l) and (Filename[p]<>PathDelim) do inc(p);
        Result:=p>l;
      end else begin
        // test if file is 'directory/something/something'
        p:=l;
        while (p>0) and (Filename[p]<>PathDelim) do dec(p);
        if p<=2 then exit;
        dec(p);
        while (p>0) and (Filename[p]<>PathDelim) do dec(p);
        if p=0 then exit;
        Result:=CompareFilenames(LeftStr(Filename,p),Directory)=0;
      end;
    TSPMaskType.StarStar:
      if Directory='' then
      begin
        Result:=not FilenameIsAbsolute(Filename);
      end else begin
        p:=1;
        PathDelimCount:=0;
        while (p<=length(Directory)) do
        begin
          if Directory[p]=PathDelim then inc(PathDelimCount);
          inc(p);
        end;
        p:=1;
        while (p<=l) do
        begin
          if Filename[p]=PathDelim then
          begin
            dec(PathDelimCount);
            if PathDelimCount=0 then
            begin
              Result:=CompareFilenames(LeftStr(Filename,p),Directory)=0;
              exit;
            end;
          end;
          inc(p);
        end;
      end;
  end;
end;

function FileIsInSPDirectory(const Filename: string; Directory: string
  ): boolean;
var
  MaskType: TSPMaskType;
begin
  Directory:=ChompPathDelim(Directory);
  MaskType:=GetSPMaskType(Directory);
  if MaskType=TSPMaskType.None then
    Result:=FileIsInSPDirectory(Filename,Directory,MaskType)
  else
    Result:=FileIsInSPDirectory(Filename,ExtractFilePath(Directory),MaskType);
end;

function FilenamePIsAbsolute(TheFilename: PChar): boolean;
begin
  {$IFDEF Unix}
  Result:=FilenamePIsUnixAbsolute(TheFilename);
  {$ELSE}
  Result:=FilenamePIsWinAbsolute(TheFilename);
  {$ENDIF}
end;

function FilenamePIsUnixAbsolute(TheFilename: PChar): boolean;
begin
  Result:=(TheFilename<>nil) and (TheFilename^='/');
end;

function FilenamePIsWinAbsolute(TheFilename: PChar): boolean;
begin
  if TheFilename=nil then exit(false);
  {$ifdef wince}
  Result := TheFilename^ in AllowDirectorySeparators;
  {$else wince}
  Result:=(TheFilename^ in ['A'..'Z','a'..'z']) and (TheFilename[1]=':')
           and (TheFilename[2] in AllowDirectorySeparators)
      or ((TheFilename^ in AllowDirectorySeparators) and (TheFilename^=TheFilename[1]));
  {$endif wince}
end;

function RelateDirectoryMasks(const LeftDir: string; LeftStart: integer;
  const RightDir: string; RightStart: integer): TSPFileMaskRelation;
var
  Left, Right: TSPMaskRecord;
begin
  Result:=TSPFileMaskRelation.None;

  if not GetSPMaskRecord(LeftDir,LeftStart,Left) then exit;
  if not GetSPMaskRecord(RightDir,RightStart,Right) then exit;

  Result:=RelateDirectoryMasks(Left,Right);
end;

function RelateDirectoryMasks(const Left, Right: TSPMaskRecord
  ): TSPFileMaskRelation;

  function StarStarFits(const StarDir, OtherDir: TSPMaskRecord): boolean;
  begin
    // Note: StarDir and OtherDir are both absolute or both relative
    Result:=false;

    if (StarDir.PathDelimCount=0) then
    begin
      // StarDir is '**', matches any except '..' and '../foo'
      // Note: it matches '.'
      if (OtherDir.StartPos^='.') and (OtherDir.StartPos[1]='.')
          and ((OtherDir.Len=2) or (OtherDir.StartPos[2]=PathDelim)) then
        exit;
      Result:=true;
    end else if (StarDir.PathDelimCount<=OtherDir.PathDelimCount) then
    begin
      // StarDir is '/foo/**', matches '/foo/bar...'
      Result:=(CompareFilenames(StarDir.StartPos,StarDir.LastPathDelim-1-StarDir.StartPos,
                OtherDir.StartPos,OtherDir.FindPathDelim(StarDir.PathDelimCount)-1-OtherDir.StartPos)=0);
    end else if (StarDir.PathDelimCount=OtherDir.PathDelimCount+1)
        and (OtherDir.MaskType=TSPMaskType.None) then begin
      // check special case StarDir is /foo/** and OtherDir is /foo
      Result:=(CompareFilenames(StarDir.StartPos,StarDir.LastPathDelim-1-StarDir.StartPos,
               OtherDir.StartPos,OtherDir.Len)=0);
    end;
  end;

  function StarFits(const StarDir, OtherDir: TSPMaskRecord): boolean;
  begin
    // Note: StarDir and OtherDir are both absolute or both relative
    Result:=false;

    if (StarDir.PathDelimCount<>OtherDir.PathDelimCount) then
      exit;
    if (StarDir.PathDelimCount=0) then
    begin
      // StarDir is '*', matches any OtherDir without pathdelim except '.' and '..'
      if (OtherDir.StartPos^='.')
          and ( (OtherDir.Len=1)
            or ((OtherDir.Len=2) and (OtherDir.StartPos[1]='.')) ) then
        exit;
      Result:=true;
    end else begin
      // e.g. '/foo/*' matches '/foo/a'
      // Note: the directories are resolved, so OtherDir cannot be '/foo/..'
      Result:=(CompareFilenames(StarDir.StartPos,StarDir.LastPathDelim-1-StarDir.StartPos,
                      OtherDir.StartPos,OtherDir.LastPathDelim-1-OtherDir.StartPos)=0);
    end;
  end;

begin
  Result:=TSPFileMaskRelation.None;

  if FilenamePIsAbsolute(Left.StartPos)<>FilenamePIsAbsolute(Right.StartPos) then
    exit; // absolute and relative path don't match

  if Left.MaskType=Right.MaskType then
  begin
    // same mask type -> simple compare
    if CompareFilenames(Left.StartPos,Left.Len,Right.StartPos,Right.Len)=0 then
      Result:=TSPFileMaskRelation.Equal;
    exit;
  end;

  // different mask types

  // check TSPMaskType.StarStar **, matching all subs including directory itself
  if Left.MaskType=TSPMaskType.StarStar then
  begin
    if StarStarFits(Left,Right) then
      Result:=TSPFileMaskRelation.LeftMoreGeneral;
    exit;
  end;
  if Right.MaskType=TSPMaskType.StarStar then
  begin
    if StarStarFits(Right,Left) then
      Result:=TSPFileMaskRelation.RightMoreGeneral;
    exit;
  end;

  // check TSPMaskType.Star *, matching all direct sub directories, excluding the directory itself
  if Left.MaskType=TSPMaskType.Star then
  begin
    if StarFits(Left,Right) then
      Result:=TSPFileMaskRelation.LeftMoreGeneral;
    exit;
  end;
  if Right.MaskType=TSPMaskType.Star then
  begin
    if StarFits(Right,Left) then
      Result:=TSPFileMaskRelation.RightMoreGeneral;
    exit;
  end;
end;

function GetSPMaskRecord(const aDirectory: string; aStartPos: integer; out
  MaskRecord: TSPMaskRecord): boolean;
begin
  Result:=false;
  MaskRecord:=Default(TSPMaskRecord);
  with MaskRecord do begin
    if aStartPos>length(aDirectory) then
      exit;
    StartPos:=@aDirectory[aStartPos];
    EndPos:=StartPos;

    repeat
      case EndPos^ of
      #0,';': break;
      PathDelim:
        begin
          inc(PathDelimCount);
          LastPathDelim:=EndPos;
        end;
      end;
      inc(EndPos);
    until false;
    Len:=EndPos-StartPos;
    if Len=0 then exit;

    // ignore trailing pathdelim
    if EndPos[-1]=PathDelim then
    begin
      dec(EndPos);
      dec(PathDelimCount);
      if PathDelimCount=0 then
        LastPathDelim:=nil
      else begin
        dec(LastPathDelim);
        while LastPathDelim^<>PathDelim do dec(LastPathDelim);
      end;
      dec(Len);
      if Len=0 then exit;
    end;

    if EndPos[-1]='*' then
    begin
      if (LastPathDelim=nil) then
      begin
        if Len=1 then
          MaskType:=TSPMaskType.Star
        else if (Len=2) and (StartPos^='*') then
          MaskType:=TSPMaskType.StarStar;
      end else begin
        if EndPos-2=LastPathDelim then
          MaskType:=TSPMaskType.Star
        else if (EndPos-3=LastPathDelim) and (LastPathDelim[1]='*') then
          MaskType:=TSPMaskType.StarStar;
      end;
    end;
  end;
  Result:=true;
end;

function GetSPMaskType(const aFilename: string): TSPMaskType;
var
  l: SizeInt;
begin
  Result:=TSPMaskType.None;
  if aFilename='' then exit;
  l:=length(aFilename);
  if aFilename[l]<>'*' then exit;
  if (l=1) or (aFilename[l-1]=PathDelim) then
    exit(TSPMaskType.Star);
  if (aFilename[l-1]='*') and ((l=2) or (aFilename[l-2]=PathDelim)) then
    exit(TSPMaskType.StarStar);
end;

function dbgs(t: TSPMaskType): string;
begin
  case t of
    TSPMaskType.None: Result:='None';
    TSPMaskType.Star: Result:='Star';
    TSPMaskType.StarStar: Result:='StarStar';
  end;
end;

function dbgs(r: TSPFileMaskRelation): string;
begin
  case r of
    TSPFileMaskRelation.None: Result:='None';
    TSPFileMaskRelation.Equal: Result:='Equal';
    TSPFileMaskRelation.LeftMoreGeneral: Result:='LeftMoreGeneral';
    TSPFileMaskRelation.RightMoreGeneral: Result:='RightMoreGeneral';
  else
    Result:='?'{%H-};
  end;
end;

{ TSPMaskRecord }

function TSPMaskRecord.FindPathDelim(Index: integer): PChar;
begin
  Result:=StartPos;
  if (Result=nil) or (Index<1) then exit;
  while Result<EndPos do
  begin
    if Result^=PathDelim then
    begin
      if Index=1 then
        exit;
      dec(Index);
    end;
    inc(Result);
  end;
  Result:=nil;
end;

end.

