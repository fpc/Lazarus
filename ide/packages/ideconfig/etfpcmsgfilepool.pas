unit etFPCMsgFilePool;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  // LazUtils
  LazFileUtils, LazFileCache, LConvEncoding,
  // CodeTools
  KeywordFuncLists, CodeToolsFPCMsgs, FileProcs, LinkScanner, CodeToolManager,
  // BuildIntf
  IDEExternToolIntf,
  // IdeConfig
  EnvironmentOpts, TransferMacros;

type
  TFPCMsgFilePool = class;

  { TFPCMsgFilePoolItem }

  TFPCMsgFilePoolItem = class
  private
    FMsgFile: TFPCMsgFile;
    FFilename: string;
    FPool: TFPCMsgFilePool;
    FLoadedFileAge: int64;
    fUseCount: integer;
  public
    constructor Create(aPool: TFPCMsgFilePool; const aFilename: string);
    destructor Destroy; override;
    property Pool: TFPCMsgFilePool read FPool;
    property Filename: string read FFilename;
    property LoadedFileAge: int64 read FLoadedFileAge;
    function GetMsg(ID: integer): TFPCMsgItem;
    property MsgFile: TFPCMsgFile read FMsgFile;
    property UseCount: integer read fUseCount;
  end;

  TETLoadFileEvent = procedure(aFilename: string; out s: string) of object;

  { TFPCMsgFilePool }

  TFPCMsgFilePool = class(TComponent)
  private
    fCritSec: TRTLCriticalSection;
    FDefaultEnglishFile: string;
    FDefaultTranslationFile: string;
    FFiles: TFPList; // list of TFPCMsgFilePoolItem sorted for loaded
    FOnLoadFile: TETLoadFileEvent;
    fPendingLog: TStrings;
    fMsgFileStamp: integer;
    fCurrentEnglishFile: string; // valid only if fMsgFileStamp=CompilerParseStamp
    fCurrentTranslationFile: string; // valid only if fMsgFileStamp=CompilerParseStamp
    procedure Log(Msg: string; AThread: TThread);
    procedure LogSync;
    procedure SetDefaultEnglishFile(AValue: string);
    procedure SetDefaultTranslationFile(AValue: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function LoadCurrentEnglishFile(UpdateFromDisk: boolean;
      AThread: TThread): TFPCMsgFilePoolItem; virtual; // don't forget UnloadFile
    function LoadFile(aFilename: string; UpdateFromDisk: boolean;
      AThread: TThread): TFPCMsgFilePoolItem; // don't forget UnloadFile
    procedure UnloadFile(var aFile: TFPCMsgFilePoolItem);
    procedure EnterCriticalsection;
    procedure LeaveCriticalSection;
    procedure GetMsgFileNames(CompilerFilename, TargetOS, TargetCPU: string;
      out anEnglishFile, aTranslationFile: string); virtual; // (main thread)
    property DefaultEnglishFile: string read FDefaultEnglishFile write SetDefaultEnglishFile;
    property DefaultTranslationFile: string read FDefaultTranslationFile write SetDefaultTranslationFile;
    property OnLoadFile: TETLoadFileEvent read FOnLoadFile write FOnLoadFile; // (main or workerthread)
  end;

var
  FPCMsgFilePool: TFPCMsgFilePool = nil;

function FPCMsgToMsgUrgency(Msg: TFPCMsgItem): TMessageLineUrgency;
function FPCMsgTypeToUrgency(const Typ: string): TMessageLineUrgency;


implementation

function FPCMsgToMsgUrgency(Msg: TFPCMsgItem): TMessageLineUrgency;
begin
  Result:=mluNone;
  if Msg=nil then exit;
  Result:=FPCMsgTypeToUrgency(Msg.ShownTyp);
  if Result<>mluNone then exit;
  Result:=FPCMsgTypeToUrgency(Msg.Typ);
  if Result=mluNone then begin
    //debugln(['FPCMsgToMsgUrgency Msg.ShownTyp="',Msg.ShownTyp,'" Msg.Typ="',Msg.Typ,'"']);
    Result:=mluVerbose3;
  end;
end;

function FPCMsgTypeToUrgency(const Typ: string): TMessageLineUrgency;
begin
  Result:=mluNone;
  if (Typ='') or (length(Typ)<>1) then exit;
  case UpChars[Typ[1]] of
  'F': Result:=mluFatal;
  'E': Result:=mluError;
  'W': Result:=mluWarning;
  'N': Result:=mluNote;
  'H': Result:=mluHint;
  'I': Result:=mluVerbose;  // info
  'L': Result:=mluProgress; // line number
  'C': Result:=mluVerbose;  // conditional: like IFDEFs
  'U': Result:=mluVerbose2; // used: found files
  'T': Result:=mluVerbose3; // tried: tried paths, general information
  'D': Result:=mluDebug;
  'X': Result:=mluProgress; // e.g. Size of Code
  'O': Result:=mluProgress; // e.g., "press enter to continue"
  else
    Result:=mluNone;
  end;
end;

{ TFPCMsgFilePoolItem }

constructor TFPCMsgFilePoolItem.Create(aPool: TFPCMsgFilePool;
  const aFilename: string);
begin
  inherited Create;
  FPool:=aPool;
  FFilename:=aFilename;
  FMsgFile:=TFPCMsgFile.Create;
end;

destructor TFPCMsgFilePoolItem.Destroy;
begin
  FreeAndNil(FMsgFile);
  FFilename:='';
  inherited Destroy;
end;

function TFPCMsgFilePoolItem.GetMsg(ID: integer): TFPCMsgItem;
begin
  Result:=FMsgFile.FindWithID(ID);
end;

{ TFPCMsgFilePool }

procedure TFPCMsgFilePool.Log(Msg: string; AThread: TThread);
begin
  EnterCriticalsection;
  try
    fPendingLog.Add(Msg);
  finally
    LeaveCriticalSection;
  end;
  if AThread<>nil then
    LogSync
  else
    TThread.Synchronize(AThread,@LogSync);
end;

procedure TFPCMsgFilePool.LogSync;
begin
  EnterCriticalsection;
  try
    dbgout(fPendingLog.Text);
  finally
    LeaveCriticalSection;
  end;
end;

procedure TFPCMsgFilePool.SetDefaultEnglishFile(AValue: string);
begin
  if FDefaultEnglishFile=AValue then Exit;
  FDefaultEnglishFile:=AValue;
  fMsgFileStamp:=-1;
end;

procedure TFPCMsgFilePool.SetDefaultTranslationFile(AValue: string);
begin
  if FDefaultTranslationFile=AValue then Exit;
  FDefaultTranslationFile:=AValue;
  fMsgFileStamp:=-1;
end;

constructor TFPCMsgFilePool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InitCriticalSection(fCritSec);
  FFiles:=TFPList.Create;
  fPendingLog:=TStringList.Create;
  fMsgFileStamp:=-1;
end;

destructor TFPCMsgFilePool.Destroy;
var
  i: Integer;
  Item: TFPCMsgFilePoolItem;
begin
  EnterCriticalsection;
  try
    // free unused files
    for i:=FFiles.Count-1 downto 0 do begin
      Item:=TFPCMsgFilePoolItem(FFiles[i]);
      if Item.fUseCount=0 then begin
        Item.Free;
        FFiles.Delete(i);
      end else begin
        if ExitCode=0 then
          debugln(['TFPCMsgFilePool.Destroy file still used: ',Item.Filename]);
      end;
    end;
    if FFiles.Count>0 then begin
      if ExitCode<>0 then
        exit;
      raise Exception.Create('TFPCMsgFilePool.Destroy some files are still used');
    end;
    FreeAndNil(FFiles);
    if FPCMsgFilePool=Self then
      FPCMsgFilePool:=nil;
    inherited Destroy;
    FreeAndNil(fPendingLog);
  finally
    LeaveCriticalSection;
  end;
  DoneCriticalsection(fCritSec);
end;

function TFPCMsgFilePool.LoadCurrentEnglishFile(UpdateFromDisk: boolean;
  AThread: TThread): TFPCMsgFilePoolItem;
var
  anEnglishFile: string;
  aTranslationFile: string;
begin
  Result:=nil;
  GetMsgFileNames(EnvironmentOptions.GetParsedCompilerFilename,'','',
    anEnglishFile,aTranslationFile);
  //writeln('TFPCMsgFilePool.LoadCurrentEnglishFile ',anEnglishFile);
  if not FilenameIsAbsolute(anEnglishFile) then exit;
  Result:=LoadFile(anEnglishFile,UpdateFromDisk,AThread);
end;

function TFPCMsgFilePool.LoadFile(aFilename: string; UpdateFromDisk: boolean;
  AThread: TThread): TFPCMsgFilePoolItem;
var
  IsMainThread: Boolean;

  procedure ResultOutdated;
  begin
    // cached file needs update
    if Result.fUseCount=0 then begin
      FFiles.Remove(Result);
      Result.Free;
    end;
    Result:=nil;
  end;

  function FileExists: boolean;
  begin
    if IsMainThread then
      Result:=FileExistsCached(aFilename)
    else
      Result:=FileExistsUTF8(aFilename);
  end;

  function FileAge: int64;
  begin
    if IsMainThread then
      Result:=FileAgeCached(aFilename)          // Returns universal time
    else
      Result:=UniversalFileAgeUTF8(aFilename);
  end;

var
  Item: TFPCMsgFilePoolItem;
  i: Integer;
  NewItem: TFPCMsgFilePoolItem;
  FileTxt: string;
  ms: TMemoryStream;
  Encoding: String;
begin
  Result:=nil;
  if aFilename='' then exit;
  aFilename:=TrimAndExpandFilename(aFilename);
  //Log('TFPCMsgFilePool.LoadFile '+aFilename,aThread);

  IsMainThread:=GetThreadID=MainThreadID;
  if UpdateFromDisk then begin
    if not FileExists then begin
      Log('TFPCMsgFilePool.LoadFile file not found: '+aFilename,AThread);
      exit;
    end;
  end;
  NewItem:=nil;
  ms:=nil;
  EnterCriticalsection;
  try
    // search the newest version in cache
    for i:=FFiles.Count-1 downto 0 do begin
      Item:=TFPCMsgFilePoolItem(FFiles[i]);
      if CompareFilenames(Item.Filename,aFilename)<>0 then continue;
      Result:=Item;
      break;
    end;
    if UpdateFromDisk then begin
      if (Result<>nil)
      and (FileAge<>Result.LoadedFileAge) then
        ResultOutdated;
    end else if Result=nil then begin
      // not yet loaded, not yet checked if file exists -> check now
      if not FileExists then
        exit;
    end;

    if Result<>nil then begin
      // share
      inc(Result.fUseCount);
    end else begin
      // load for the first time
      NewItem:=TFPCMsgFilePoolItem.Create(Self,aFilename);
      //Log('TFPCMsgFilePool.LoadFile '+dbgs(NewItem.FMsgFile<>nil)+' '+aFilename,aThread);
      if Assigned(OnLoadFile) then begin
        OnLoadFile(aFilename,FileTxt);
      end else begin
        ms:=TMemoryStream.Create;
        ms.LoadFromFile(aFilename);
        SetLength(FileTxt,ms.Size);
        ms.Position:=0;
        if FileTxt<>'' then
          ms.Read(FileTxt[1],length(FileTxt));
      end;
      // convert encoding
      Encoding:=GetDefaultFPCErrorMsgFileEncoding(aFilename);
      FileTxt:=ConvertEncoding(FileTxt,Encoding,EncodingUTF8);
      // parse
      NewItem.FMsgFile.LoadFromText(FileTxt);
      NewItem.FLoadedFileAge:=FileAge;
      // load successful
      Result:=NewItem;
      NewItem:=nil;
      FFiles.Add(Result);
      inc(Result.fUseCount);
      //log('TFPCMsgFilePool.LoadFile '+Result.Filename+' '+dbgs(Result.fUseCount),aThread);
    end;
  finally
    ms.Free;
    FreeAndNil(NewItem);
    LeaveCriticalSection;
  end;
end;

procedure TFPCMsgFilePool.UnloadFile(var aFile: TFPCMsgFilePoolItem);
var
  i: Integer;
  Item: TFPCMsgFilePoolItem;
  Keep: Boolean;
begin
  EnterCriticalsection;
  try
    if aFile.fUseCount<=0 then
      raise Exception.Create('TFPCMsgFilePool.UnloadFile already freed');
    if FFiles.IndexOf(aFile)<0 then
      raise Exception.Create('TFPCMsgFilePool.UnloadFile unknown, maybe already freed');
    dec(aFile.fUseCount);
    //log('TFPCMsgFilePool.UnloadFile '+aFile.Filename+' UseCount='+dbgs(aFile.fUseCount),aThread);
    if aFile.fUseCount>0 then exit;
    // not used anymore
    if not FileExistsUTF8(aFile.Filename) then begin
      Keep:=false;
    end else begin
      // file still exist on disk
      // => check if it is the newest version
      Keep:=true;
      for i:=FFiles.Count-1 downto 0 do begin
        Item:=TFPCMsgFilePoolItem(FFiles[i]);
        if Item=aFile then break;
        if CompareFilenames(Item.Filename,aFile.Filename)<>0 then continue;
        // there is already a newer version
        Keep:=false;
        break;
      end;
    end;
    if Keep then begin
      // this file is the newest version => keep it in cache
    end else begin
      //log('TFPCMsgFilePool.UnloadFile free: '+aFile.Filename,aThread);
      FFiles.Remove(aFile);
      aFile.Free;
    end;
  finally
    aFile:=nil;
    LeaveCriticalSection;
  end;
end;

procedure TFPCMsgFilePool.EnterCriticalsection;
begin
  System.EnterCriticalsection(fCritSec);
end;

procedure TFPCMsgFilePool.LeaveCriticalSection;
begin
  System.LeaveCriticalsection(fCritSec);
end;

procedure TFPCMsgFilePool.GetMsgFileNames(CompilerFilename, TargetOS,
  TargetCPU: string; out anEnglishFile, aTranslationFile: string);
var
  FPCVer: String;
  FPCSrcDir: String;
  aFilename: String;
  CompilerKind: TPascalCompiler;
begin
  if fMsgFileStamp<>CompilerParseStamp then begin
    fCurrentEnglishFile:=DefaultEnglishFile;
    fCurrentTranslationFile:=DefaultTranslationFile;
    // English msg file
    // => use fpcsrcdir/compiler/msg/errore.msg
    // the fpcsrcdir might depend on the FPC version
    FPCVer:=CodeToolBoss.CompilerDefinesCache.GetPCVersion(
              CompilerFilename,TargetOS,TargetCPU,false,CompilerKind);
    if CompilerKind<>pcFPC then
      ;// ToDo
    FPCSrcDir:=EnvironmentOptions.GetParsedFPCSourceDirectory(FPCVer);
    if FilenameIsAbsolute(FPCSrcDir) then begin
      // FPCSrcDir exists => use the errore.msg
      aFilename:=AppendPathDelim(FPCSrcDir)+GetForcedPathDelims('compiler/msg/errore.msg');
      if FileExistsCached(aFilename) then
        fCurrentEnglishFile:=aFilename;
    end;
    if not FileExistsCached(fCurrentEnglishFile) then begin
      // as fallback use the copy in the Codetools directory
      aFilename:=EnvironmentOptions.GetParsedLazarusDirectory;
      if FilenameIsAbsolute(aFilename) then begin
        aFilename:=AppendPathDelim(aFilename)+GetForcedPathDelims('components/codetools/fpc.errore.msg');
        if FileExistsCached(aFilename) then
          fCurrentEnglishFile:=aFilename;
      end;
    end;
    // translation msg file
    aFilename:=EnvironmentOptions.GetParsedCompilerMessagesFilename;
    if FilenameIsAbsolute(aFilename) and FileExistsCached(aFilename)
    and (CompareFilenames(aFilename,fCurrentEnglishFile)<>0) then
      fCurrentTranslationFile:=aFilename;
    fMsgFileStamp:=CompilerParseStamp;
  end;
  anEnglishFile:=fCurrentEnglishFile;
  aTranslationFile:=fCurrentTranslationFile;
end;


end.

