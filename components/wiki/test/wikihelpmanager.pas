unit WikiHelpManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, LazLogger, Wiki2HTMLConvert,
  MTProcs;

type
  TWikiHelp = class;

  { TW2HelpPage
    for future extensions and descendants }

  TW2HelpPage = class(TW2HTMLPage)
  public

  end;

  { TWiki2HelpConverter }

  TWiki2HelpConverter = class(TWiki2HTMLConverter)
  protected
  public
    constructor Create; override;
  end;

  { TWikiHelpThread }

  TWikiHelpThread = class(TThread)
  protected
    fLogMsg: string;
    procedure Execute; override;
    procedure MainThreadLog;
    procedure Log(Msg: string);
    procedure OnScanComplete; // called in thread at end
    procedure LoadWikiPage(Index: PtrInt; {%H-}Data: Pointer; {%H-}Item: TMultiThreadProcItem);
  public
    Help: TWikiHelp;
    XMLDirectory: string;
    ImagesDirectory: string;
  end;

  { TWikiHelp }

  TWikiHelp = class(TComponent)
  private
    FAborting: boolean;
    FConverter: TWiki2HelpConverter;
    FImagesDirectory: string;
    FScanning: boolean;
    FXMLDirectory: string;
    FCritSec: TRTLCriticalSection;
    FScanThread: TWikiHelpThread;
    procedure SetImagesDirectory(AValue: string);
    procedure SetXMLDirectory(AValue: string);
    procedure EnterCritSect;
    procedure LeaveCritSect;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartScan;
    procedure Abort;
    property Scanning: boolean read FScanning;
    property Aborting: boolean read FAborting;
    property XMLDirectory: string read FXMLDirectory write SetXMLDirectory; // directory where the wiki xml files are
    property ImagesDirectory: string read FImagesDirectory write SetImagesDirectory; // directory where the wiki image files are
    property Converter: TWiki2HelpConverter read FConverter;
  end;

var
  WikiHelp: TWikiHelp = nil;

implementation

{ TWiki2HelpConverter }

constructor TWiki2HelpConverter.Create;
begin
  inherited Create;
  fPageClass:=TW2HelpPage;
end;

{ TWikiHelpThread }

procedure TWikiHelpThread.Execute;
var
  FileInfo: TSearchRec;
  Files: TStringList;
  i: Integer;
  Filename: String;
begin
  Files:=nil;
  try
    Log('TWikiHelpThread.Execute START XMLDirectory="'+XMLDirectory+'"');

    Files:=TStringList.Create;
    try
      // get all wiki xml files
      if FindFirstUTF8(XMLDirectory+AllFilesMask,faAnyFile,FileInfo)=0 then begin
        repeat
          if CompareFileExt(FileInfo.Name,'.xml',false)=0 then
            Files.Add(FileInfo.Name);
        until FindNextUTF8(FileInfo)<>0;
      end;
      FindCloseUTF8(FileInfo);

      // add file names to converter
      for i:=0 to Files.Count-1 do begin
        Filename:=XMLDirectory+Files[i];
        Help.Converter.AddWikiPage(Filename,false);
      end;
    finally
      Files.Free;
    end;

    // load xml files
    ProcThreadPool.DoParallel(@LoadWikiPage,0,Help.Converter.Count-1);

    Log('TWikiHelpThread.Execute SCAN complete XMLDirectory="'+XMLDirectory+'"');
  except
    on E: Exception do begin
      Log('TWikiHelpThread.Execute error: '+E.Message);
    end;
  end;
  Synchronize(@OnScanComplete);
end;

procedure TWikiHelpThread.MainThreadLog;
// called in main thread
begin
  DebugLn(fLogMsg);
end;

procedure TWikiHelpThread.Log(Msg: string);
begin
  fLogMsg:=Msg;
  Synchronize(@MainThreadLog);
end;

procedure TWikiHelpThread.OnScanComplete;
// called in this thread
begin
  Help.EnterCritSect;
  try
    Help.FScanThread:=nil;
    Help.FScanning:=false;
  finally
    Help.LeaveCritSect;
  end;
end;

procedure TWikiHelpThread.LoadWikiPage(Index: PtrInt; Data: Pointer;
  Item: TMultiThreadProcItem);
var
  Page: TW2HelpPage;
begin
  Page:=TW2HelpPage(Help.Converter.Pages[Index]);
  Page.ParseWikiDoc;
end;

{ TWikiHelp }

procedure TWikiHelp.SetImagesDirectory(AValue: string);
var
  NewDir: String;
begin
  NewDir:=TrimAndExpandDirectory(TrimFilename(AValue));
  if FImagesDirectory=NewDir then Exit;
  FImagesDirectory:=NewDir;
end;

procedure TWikiHelp.SetXMLDirectory(AValue: string);
var
  NewDir: String;
begin
  NewDir:=TrimAndExpandDirectory(TrimFilename(AValue));
  if FXMLDirectory=NewDir then Exit;
  FXMLDirectory:=NewDir;
end;

procedure TWikiHelp.EnterCritSect;
begin
  EnterCriticalsection(FCritSec);
end;

procedure TWikiHelp.LeaveCritSect;
begin
  LeaveCriticalsection(FCritSec);
end;

constructor TWikiHelp.Create(AOwner: TComponent);
begin
  InitCriticalSection(FCritSec);
  inherited Create(AOwner);
  FConverter:=TWiki2HelpConverter.Create;
end;

destructor TWikiHelp.Destroy;
begin
  Abort;
  FreeAndNil(FConverter);
  inherited Destroy;
  DoneCriticalsection(FCritSec);
end;

procedure TWikiHelp.StartScan;
begin
  if not DirPathExists(XMLDirectory) then
    raise Exception.Create('TWikiHelp.StartScan XMLDirectory not found: '+XMLDirectory);
  if not DirPathExists(ImagesDirectory) then
    raise Exception.Create('TWikiHelp.StartScan ImagesDirectory not found: '+ImagesDirectory);
  EnterCritSect;
  try
    if Scanning then exit;
    FScanning:=true;
    FScanThread:=TWikiHelpThread.Create(true);
    FScanThread.FreeOnTerminate:=true;
    FScanThread.Help:=Self;
    FScanThread.XMLDirectory:=XMLDirectory;
    FScanThread.ImagesDirectory:=ImagesDirectory;
    {$IF FPC_FULLVERSION<=20403}
    FScanThread.Resume;
    {$ELSE}
    FScanThread.Start;
    {$ENDIF}
  finally
    LeaveCritSect;
  end;
end;

procedure TWikiHelp.Abort;
begin
  FAborting:=true;
  while Scanning do
    Sleep(10);
  FAborting:=false;
end;

end.

