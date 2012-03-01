unit WikiHelpManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, LazLogger, Wiki2HTMLConvert;

type
  TWikiHelp = class;

  { TWikiHelpThread }

  TWikiHelpThread = class(TThread)
  protected
    fLogMsg: string;
    procedure Execute; override;
    procedure MainThreadLog;
    procedure Log(Msg: string);
    procedure OnScanComplete; // called in thread at end
  public
    Help: TWikiHelp;
    XMLDirectory: string;
    ImagesDirectory: string;
  end;

  { TWikiHelp }

  TWikiHelp = class(TComponent)
  private
    FAborting: boolean;
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
  end;

var
  WikiHelp: TWikiHelp = nil;

implementation

{ TWikiHelpThread }

procedure TWikiHelpThread.Execute;
begin
  try
    Log('TWikiHelpThread.Execute START XMLDirectory="'+XMLDirectory+'"');

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

{ TWikiHelp }

procedure TWikiHelp.SetImagesDirectory(AValue: string);
var
  NewDir: String;
begin
  NewDir:=AppendPathDelim(TrimFilename(AValue));
  if FImagesDirectory=NewDir then Exit;
  FImagesDirectory:=NewDir;
end;

procedure TWikiHelp.SetXMLDirectory(AValue: string);
var
  NewDir: String;
begin
  NewDir:=AppendPathDelim(TrimFilename(AValue));
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
end;

destructor TWikiHelp.Destroy;
begin
  Abort;
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

