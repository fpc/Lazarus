unit SearchPathProcs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, LazFileCache, FileUtil;

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
function GetNextDirectoryInSearchPath(const SearchPath: string;
                                      var NextStartPos: integer): string;
function GetNextUsedDirectoryInSearchPath(const SearchPath,
                          FilterDir: string; var NextStartPos: integer): string;
function SearchPathToList(const SearchPath: string): TStringList;
function SearchDirectoryInSearchPath(const SearchPath, Directory: string;
                                     DirStartPos: integer = 1): integer;
function SearchDirectoryInSearchPath(SearchPath: TStrings;
                    const Directory: string; DirStartPos: integer = 0): integer;

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
  len: Integer;
  BaseDir: String;
begin
  Result:='';
  EndPos:=1;
  len:=length(SearchPath);
  BaseDir:=AppendPathDelim(TrimFilename(BaseDirectory));
  while EndPos<=len do begin
    StartPos:=EndPos;
    // skip empty paths and space chars at start
    while (StartPos<=len) and (SearchPath[StartPos] in [';',#0..#32]) do
      inc(StartPos);
    if StartPos>len then break;
    EndPos:=StartPos;
    while (EndPos<=len) and (SearchPath[EndPos]<>';') do inc(EndPos);
    CurPath:=copy(SearchPath,StartPos,EndPos-StartPos);
    if CurPath<>'' then begin
      // non empty path => expand, trim and normalize
      if ExpandPaths then
        CurPath:=TrimAndExpandDirectory(CurPath,BaseDir)
      else if (BaseDir<>'') and (not FilenameIsAbsolute(CurPath)) then
        CurPath:=BaseDir+CurPath;
      CurPath:=ChompPathDelim(TrimFilename(CurPath));
      if CurPath='' then CurPath:='.';
      // check if path already exists
      if (not DeleteDoubles) or (SearchDirectoryInSearchPath(Result,CurPath)<1)
      then begin
        if Result<>'' then
          CurPath:=';'+CurPath;
        if CurPath<>'' then
          Result:=Result+CurPath
        else
          Result:=Result+'.';
      end;
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

end.

