unit CTFileChksums;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, md5, AvgLvlTree, LazFileUtils, LazFileCache;

type

  { TFileChecksums }

  TFileChecksums = class
  private
    FFiles: TAvgLvlTree;
  public
    type
      {$ScopedEnums on}
      TState = (
        None,
        Valid,
        FileNotFound,
        ReadError
        );
      {$ScopedEnums off}
      TStates = set of TState;

      TFile = class
      public
        Filename: string;
        Checksum: string;
        Age: int64;
        Size: int64;
        State: TState;
      end;

    constructor Create;
    destructor Destroy; override;
    function GetChecksum(aFilename: string; out Checksum: string): TState;
    function Find(const aFilename: string): TFile;
    function ComputeChecksum(const aFilename: string; out Checksum: string; out aSize: int64): TState;
  end;

function dbgs(s: TFileChecksums.TState): string; overload;

implementation

function CompareCSFiles(Item1, Item2: Pointer): integer;
var
  File1: TFileChecksums.TFile absolute Item1;
  File2: TFileChecksums.TFile absolute Item2;
begin
  Result:=CompareFilenames(File1.Filename,File2.Filename);
end;

function CompareFilenameWithCSFile(FileP, Item: Pointer): integer;
var
  aFilename: PAnsiString absolute FileP;
  aFile: TFileChecksums.TFile absolute Item;
begin
  Result:=CompareFilenames(aFilename^,aFile.Filename);
end;

function dbgs(s: TFileChecksums.TState): string;
begin
  Result:='';
  str(s,Result);
end;

{ TFileChecksums }

constructor TFileChecksums.Create;
begin
  FFiles:=TAvgLvlTree.Create(@CompareCSFiles);
end;

destructor TFileChecksums.Destroy;
begin
  inherited Destroy;
end;

function TFileChecksums.GetChecksum(aFilename: string; out Checksum: string): TState;
var
  aFile: TFile;
  Age: Int64;
begin
  Result:=TState.FileNotFound;
  Checksum:='';
  if (aFilename='') or not FilenameIsAbsolute(aFilename) then exit;
  aFilename:=ResolveDots(aFilename);
  aFile:=Find(aFilename);
  if aFile=nil then
  begin
    aFile:=TFile.Create;
    aFile.Filename:=aFilename;
    FFiles.Add(aFile);
  end;

  if not FileExistsCached(aFilename) then
  begin
    // file not found
    aFile.State:=TState.FileNotFound;
    exit(TState.FileNotFound);
  end;

  Age:=FileAgeCached(aFilename);
  if (aFile.State=TState.Valid) and (Age<>aFile.Age) then
    aFile.State:=TState.None;

  if (aFile.State=TState.Valid) then
  begin
    // cache valid
    Checksum:=aFile.Checksum;
    exit(aFile.State);
  end;

  aFile.State:=ComputeChecksum(aFilename,aFile.Checksum,aFile.Size);
  Checksum:=aFile.Checksum;
  Result:=aFile.State;
end;

function TFileChecksums.Find(const aFilename: string): TFile;
var
  p: PAnsiString;
  Node: TAvgLvlTreeNode;
begin
  Result:=nil;
  if aFilename='' then exit;
  p:=@aFilename;
  Node:=FFiles.FindKey(p,@CompareFilenameWithCSFile);
  if Node<>nil then
    Result:=TFile(Node.Data);
end;

function TFileChecksums.ComputeChecksum(const aFilename: string; out Checksum: string; out
  aSize: int64): TState;
const
  Hex = '0123456789ABCDEF';
  BufSize = 64*1024;
var
  aContext: TMDContext;
  aDigest: TMD5Digest;
  fs: TFileStream;
  i: Integer;
  b: Byte;
  Buf: PByte;
  Cnt: LongInt;
begin
  Checksum:='';
  try
    GetMem(Buf,BufSize);
    fs:=TFileStream.Create(aFilename,fmOpenRead or fmShareDenyNone);
    try
      aSize:=fs.Size;

      MD5Init(aContext);
      repeat
        Cnt:=fs.Read(Buf^,BufSize);
        if Cnt=0 then break;
        MD5Update(aContext,Buf^,Cnt);
      until false;
      MD5Final(aContext,aDigest);

      SetLength(Checksum,length(aDigest)*2);
      for i:=0 to high(aDigest) do
      begin
        b:=aDigest[i];
        Checksum[i*2+1]:=Hex[b shr 4+1];
        Checksum[i*2+2]:=Hex[b and $f+1];
      end;
      Result:=TState.Valid;
    finally
      Freemem(Buf);
      fs.Free;
    end;
  except
    Result:=TState.ReadError;
  end;
end;

end.

