{
 **********************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 **********************************************************************
}
unit LazFileCache;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz_AVL_Tree,
  LazDbgLog, LazFileUtils;

type
  TFileStateCacheItemFlag = (
    fsciExists,    // file or directory exists
    fsciDirectory, // file exists and is directory
    fsciReadable,  // file is readable
    fsciWritable,  // file is writable
    fsciDirectoryReadable, // file is directory and can be searched
    fsciDirectoryWritable, // file is directory and new files can be created
    fsciText,      // file is text file (not binary)
    fsciExecutable,// file is executable
    fsciAge,        // file age is valid
    fsciPhysical    // physical filename is valid
    );
  TFileStateCacheItemFlags = set of TFileStateCacheItemFlag;

  { TFileStateCacheItem }

  TFileStateCacheItem = class
  private
    FAge: longint;
    FFilename: string;
    FFlags: TFileStateCacheItemFlags;
    FPhysicalFilename: string;
    FTestedFlags: TFileStateCacheItemFlags;
    FTimeStamp: int64;
  public
    constructor Create(const TheFilename: string; NewTimeStamp: int64);
    function CalcMemSize: PtrUint;
  public
    property Filename: string read FFilename;
    property PhysicalFilename: string read FPhysicalFilename;
    property Flags: TFileStateCacheItemFlags read FFlags;
    property TestedFlags: TFileStateCacheItemFlags read FTestedFlags;
    property TimeStamp: int64 read FTimeStamp;
    property Age: longint read FAge;
  end;

  TOnChangeFileStateTimeStamp = procedure(Sender: TObject;
                                          const AFilename: string) of object;

  { TFileStateCache }

  TFileStateCache = class
  private
    FFiles: TAVLTree; // tree of TFileStateCacheItem
    FTimeStamp: int64;
    FLockCount: integer;
    FChangeTimeStampHandler: array of TOnChangeFileStateTimeStamp;
    procedure SetFlag(AFile: TFileStateCacheItem;
                      AFlag: TFileStateCacheItemFlag; NewValue: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Lock;
    procedure Unlock;
    function Locked: boolean;
    procedure IncreaseTimeStamp(const AFilename: string);
    function FileExistsCached(const AFilename: string): boolean;
    function DirPathExistsCached(const AFilename: string): boolean;
    function DirectoryIsWritableCached(const DirectoryName: string): boolean;
    function FileIsExecutableCached(const AFilename: string): boolean;
    function FileIsReadableCached(const AFilename: string): boolean;
    function FileIsWritableCached(const AFilename: string): boolean;
    function FileIsTextCached(const AFilename: string): boolean;
    function FileAgeCached(const AFileName: string): Longint;
    function GetPhysicalFilenameCached(const AFileName: string; {%H-}EmptyOnError: boolean): string;
    function FindFile(const Filename: string;
                      CreateIfNotExists: boolean): TFileStateCacheItem;
    function Check(const Filename: string; AFlag: TFileStateCacheItemFlag;
                   out AFile: TFileStateCacheItem; var FlagIsSet: boolean): boolean;
    procedure AddChangeTimeStampHandler(const Handler: TOnChangeFileStateTimeStamp);
    procedure RemoveChangeTimeStampHandler(const Handler: TOnChangeFileStateTimeStamp);
    function CalcMemSize: PtrUint;
  public
    property TimeStamp: int64 read FTimeStamp;
  end;

var
  FileStateCache: TFileStateCache = nil;

function FileExistsCached(const AFilename: string): boolean;
function DirPathExistsCached(const AFilename: string): boolean;
function DirectoryIsWritableCached(const ADirectoryName: string): boolean;
function FileIsExecutableCached(const AFilename: string): boolean;
function FileIsReadableCached(const AFilename: string): boolean;
function FileIsWritableCached(const AFilename: string): boolean;
function FileIsTextCached(const AFilename: string): boolean;
function FileAgeCached(const AFileName: string): Longint;
function GetPhysicalFilenameCached(const AFilename: string; EmptyOnError: boolean): string;

procedure InvalidateFileStateCache(const Filename: string = ''); inline;
function CompareFileStateItems(Data1, Data2: Pointer): integer;
function CompareFilenameWithFileStateCacheItem(Key, Data: Pointer): integer;

const
  LUInvalidChangeStamp = Low(integer);
  LUInvalidChangeStamp64 = Low(int64); // using a value outside integer to spot wrong types early
procedure LUIncreaseChangeStamp(var ChangeStamp: integer); inline;
procedure LUIncreaseChangeStamp64(var ChangeStamp: int64); inline;

type
  TOnFileExistsCached = function(Filename: string): boolean of object;
  TOnFileAgeCached = function(Filename: string): longint of object;
var
  OnFileExistsCached: TOnFileExistsCached = nil;
  OnFileAgeCached: TOnFileAgeCached = nil;

implementation


function FileExistsCached(const AFilename: string): boolean;
begin
  if OnFileExistsCached<>nil then
    Result:=OnFileExistsCached(AFilename)
  else if FileStateCache<>nil then
    Result:=FileStateCache.FileExistsCached(AFilename)
  else
    Result:=FileExistsUTF8(AFilename);
end;

function DirPathExistsCached(const AFilename: string): boolean;
begin
  if FileStateCache<>nil then
    Result:=FileStateCache.DirPathExistsCached(AFilename)
  else
    Result:=DirPathExists(AFilename);
end;

function DirectoryIsWritableCached(const ADirectoryName: string): boolean;
begin
  if FileStateCache<>nil then
    Result:=FileStateCache.DirectoryIsWritableCached(ADirectoryName)
  else
    Result:=DirectoryIsWritable(ADirectoryName);
end;

function FileIsExecutableCached(const AFilename: string): boolean;
begin
  if FileStateCache<>nil then
    Result:=FileStateCache.FileIsExecutableCached(AFilename)
  else
    Result:=FileIsExecutable(AFilename);
end;

function FileIsReadableCached(const AFilename: string): boolean;
begin
  if FileStateCache<>nil then
    Result:=FileStateCache.FileIsReadableCached(AFilename)
  else
    Result:=FileIsReadable(AFilename);
end;

function FileIsWritableCached(const AFilename: string): boolean;
begin
  if FileStateCache<>nil then
    Result:=FileStateCache.FileIsWritableCached(AFilename)
  else
    Result:=FileIsWritable(AFilename);
end;

function FileIsTextCached(const AFilename: string): boolean;
begin
  if FileStateCache<>nil then
    Result:=FileStateCache.FileIsTextCached(AFilename)
  else
    Result:=FileIsText(AFilename);
end;

function FileAgeCached(const AFileName: string): Longint;
begin
  if OnFileAgeCached<>nil then
    Result:=OnFileAgeCached(AFilename)
  else if FileStateCache<>nil then
    Result:=FileStateCache.FileAgeCached(AFilename)
  else
    Result:=FileAgeUTF8(AFileName);
end;

function GetPhysicalFilenameCached(const AFilename: string;
  EmptyOnError: boolean): string;
var
  OnError: TPhysicalFilenameOnError;
begin
  if FileStateCache<>nil then
    Result:=FileStateCache.GetPhysicalFilenameCached(AFilename,EmptyOnError)
  else begin
    if EmptyOnError then
      OnError:=pfeEmpty
    else
      OnError:=pfeOriginal;
    writeln('GetPhysicalFilenameCached GGG1');
    Result:=GetPhysicalFilename(AFilename,OnError);
  end;
end;

procedure InvalidateFileStateCache(const Filename: string);
begin
  FileStateCache.IncreaseTimeStamp(Filename);
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

procedure LUIncreaseChangeStamp(var ChangeStamp: integer);
begin
  if ChangeStamp<High(ChangeStamp) then
    inc(ChangeStamp)
  else
    ChangeStamp:=LUInvalidChangeStamp+1;
end;

procedure LUIncreaseChangeStamp64(var ChangeStamp: int64);
begin
  if ChangeStamp<High(ChangeStamp) then
    inc(ChangeStamp)
  else
    ChangeStamp:=LUInvalidChangeStamp64+1;
end;

{ TFileStateCacheItem }

constructor TFileStateCacheItem.Create(const TheFilename: string;
  NewTimeStamp: int64);
begin
  FFilename:=TheFilename;
  FTimeStamp:=NewTimeStamp;
end;

function TFileStateCacheItem.CalcMemSize: PtrUint;
begin
  Result:=PtrUInt(InstanceSize)
    +MemSizeString(FFilename)
    +MemSizeString(FPhysicalFilename);
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
  //WriteStr(s, AFlag);
  //debugln('TFileStateCache.SetFlag AFile.Filename=',AFile.Filename,' ',s,'=',dbgs(AFlag in AFile.FFlags),' Valid=',dbgs(AFlag in AFile.FTestedFlags));
end;

constructor TFileStateCache.Create;
begin
  FFiles:=TAVLTree.Create(@CompareFileStateItems);
  LUIncreaseChangeStamp64(FTimeStamp); // one higher than default for new files
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

procedure TFileStateCache.IncreaseTimeStamp(const AFilename: string);
var
  i: Integer;
  AFile: TFileStateCacheItem;
begin
  if Self=nil then exit;
  if AFilename='' then begin
    // invalidate all
    LUIncreaseChangeStamp64(FTimeStamp);
  end else begin
    // invalidate single file
    AFile:=FindFile(AFilename,false);
    if AFile<>nil then
      AFile.FTestedFlags:=[];
  end;
  for i:=0 to length(FChangeTimeStampHandler)-1 do
    FChangeTimeStampHandler[i](Self,AFilename);
  //debugln('TFileStateCache.IncreaseTimeStamp FTimeStamp=',dbgs(FTimeStamp));
end;

function TFileStateCache.FileExistsCached(const AFilename: string): boolean;
var
  AFile: TFileStateCacheItem;
begin
  Result := False;
  if Check(AFilename,fsciExists,AFile,Result) then exit;
  Result:=FileExistsUTF8(AFile.Filename);
  SetFlag(AFile,fsciExists,Result);
  {if not Check(Filename,fsciExists,AFile,Result) then begin
    WriteDebugReport;
    raise Exception.Create('');
  end;}
end;

function TFileStateCache.DirPathExistsCached(const AFilename: string): boolean;
var
  AFile: TFileStateCacheItem;
begin
  Result := False;
  if Check(AFilename,fsciDirectory,AFile,Result) then exit;
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

function TFileStateCache.FileAgeCached(const AFileName: string): Longint;
var
  AFile: TFileStateCacheItem;
  Dummy: Boolean;
begin
  Dummy := False;
  if Check(AFilename,fsciAge,AFile,Dummy) then begin
    Result:=AFile.Age;
    exit;
  end;
  Result:=FileAge(AFile.Filename);
  AFile.FAge:=Result;
  Include(AFile.FTestedFlags,fsciAge);
end;

function TFileStateCache.GetPhysicalFilenameCached(const AFileName: string;
  EmptyOnError: boolean): string;
{$IFDEF Unix}
var
  AFile: TFileStateCacheItem;
  Dummy: Boolean;
{$ENDIF}
begin
  {$IFDEF Unix}
  Dummy := False;
  if Check(AFilename,fsciPhysical,AFile,Dummy) then begin
    Result:=AFile.PhysicalFilename;
    exit;
  end;
  Result:=ExtractFilePath(AFile.Filename);
  if Result<>'' then begin
    // use cache recursively for directory
    if (Result='.') or (Result='..') or (Result='/') then begin
      // no query required
    end else begin
      Result:=GetPhysicalFilenameCached(Result,true);
    end;
    if Result<>'' then begin
      Result:=AppendPathDelim(Result)+ExtractFilename(AFile.Filename);
      Result:=ReadAllLinks(Result,false);
    end;
  end else begin
    // no path
    Result:=ReadAllLinks(AFile.Filename,false);
  end;
  AFile.FPhysicalFilename:=Result;
  Include(AFile.FTestedFlags,fsciPhysical);
  if (Result='') and (not EmptyOnError) then
    Result:=AFileName;
  {$ELSE}
  Result:=AFileName;
  {$ENDIF}
end;

function TFileStateCache.FindFile(const Filename: string;
  CreateIfNotExists: boolean): TFileStateCacheItem;
var
  NormedFilename: String;
  ANode: TAVLTreeNode;
begin
  // make filename unique
  NormedFilename:=ChompPathDelim(ResolveDots(Filename));
  ANode:=FFiles.FindKey(Pointer(NormedFilename),
                        @CompareFilenameWithFileStateCacheItem);
  if ANode<>nil then
    Result:=TFileStateCacheItem(ANode.Data)
  else if CreateIfNotExists then begin
    Result:=TFileStateCacheItem.Create(NormedFilename,FTimeStamp);
    FFiles.Add(Result);
    if FFiles.FindKey(Pointer(NormedFilename),
                      @CompareFilenameWithFileStateCacheItem)=nil
    then begin
      //DebugLn(format('FileStateCache.FindFile: "%s"',[FileName]));
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
  //WriteStr(s, AFlag);
  //debugln('TFileStateCache.Check Filename=',Filename,' AFile.Filename=',AFile.Filename,' ',s,'=',dbgs(FlagIsSet),' Valid=',dbgs(Result));
end;

procedure TFileStateCache.AddChangeTimeStampHandler(
  const Handler: TOnChangeFileStateTimeStamp);
begin
  SetLength(FChangeTimeStampHandler,length(FChangeTimeStampHandler)+1);
  FChangeTimeStampHandler[length(FChangeTimeStampHandler)-1]:=Handler;
end;

procedure TFileStateCache.RemoveChangeTimeStampHandler(
  const Handler: TOnChangeFileStateTimeStamp);
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

function TFileStateCache.CalcMemSize: PtrUint;
var
  Node: TAVLTreeNode;
begin
  Result:=PtrUInt(InstanceSize)
    +PtrUInt(length(FChangeTimeStampHandler))*SizeOf(TNotifyEvent);
  if FFiles<>nil then begin
    inc(Result,PtrUInt(FFiles.InstanceSize)
      +PtrUInt(FFiles.Count)*PtrUInt(TAVLTreeNode.InstanceSize));
    Node:=FFiles.FindLowest;
    while Node<>nil do begin
      inc(Result,TFileStateCacheItem(Node.Data).CalcMemSize);
      Node:=FFiles.FindSuccessor(Node);
    end;
  end;
end;

initialization
  OnInvalidateFileStateCache:=@InvalidateFileStateCache;

end.

