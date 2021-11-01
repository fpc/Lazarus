unit RemoteDebugger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, math, process,
  Forms, Dialogs, syncobjs,
  Maps, LazLogger, LazUTF8, lazCollections,
  DbgIntfBaseTypes, DbgIntfDebuggerBase,
  FpDebugDebugger, FpDebugDebuggerUtils, FpDebugDebuggerWorkThreads,
  // FpDebug
  {$IFDEF FPDEBUG_THREAD_CHECK} FpDbgCommon, {$ENDIF}
  FpDbgClasses, FpDbgInfo, FpErrorMessages, FpPascalBuilder, FpdMemoryTools,
  FpPascalParser, FPDbgController, FpDbgDwarfDataClasses, FpDbgDwarfFreePascal,
  FpDbgDwarf, FpDbgUtil,
  DebuggerPropertiesBase,
  FpDbgRsp;

type

  { TFpDebugDebuggerPropertiesRemoteServer }

  TFpDebugDebuggerPropertiesRemoteServer = class(TFpDebugDebuggerProperties)
  private
  const
    DEF_host = 'localhost';
    DEF_port = 1234;         // Default port for qemu
    DEF_uploadBinary = false;
  private
    //FRemoteConfig: TRemoteConfig;
    FHost: string;
    FPort: integer;
    FUploadBinary: boolean;

    FAfterConnectMonitorCmds: TXmlConfStringList;
    FAfterUploadMonitorCmds: TXmlConfStringList;
    FSkipUploadOfSectionList: TXmlConfStringList;
    // Temporary test code
    FAfterUploadBreakZero: boolean;

    function portIsStored: Boolean;
    function hostIsStored: Boolean;
    function uploadBinaryIsStored: Boolean;
    procedure SetAfterConnectMonitorCmds(AValue: TXmlConfStringList);
    procedure SetAfterUploadMonitorCmds(AValue: TXmlConfStringList);
    procedure SetSkipUploadOfSectionList(AValue: TXmlConfStringList);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Host: string read FHost write FHost stored hostIsStored;
    property Port: integer read FPort write FPort stored portIsStored default DEF_port;
    property UploadBinary: Boolean read FUploadBinary write FUploadBinary stored uploadBinaryIsStored default DEF_uploadBinary;
    property AfterConnectMonitorCmds: TXmlConfStringList read FAfterConnectMonitorCmds write SetAfterConnectMonitorCmds;
    property AfterUploadMonitorCmds: TXmlConfStringList read FAfterUploadMonitorCmds write SetAfterUploadMonitorCmds;
    property SkipUploadOfSectionList: TXmlConfStringList read FSkipUploadOfSectionList write SetSkipUploadOfSectionList;
    // Temporary test code
    property AfterUploadBreakZero: boolean read FAfterUploadBreakZero write FAfterUploadBreakZero default false;
  end;

  { TFpRemoteDebugger }

  TFpRemoteDebugger = class(TFpDebugDebugger)
  private
    procedure UpdateProcessConfig;
  public
    constructor Create(const AExternalDebugger: String); override;
    destructor Destroy; override;
    class function CreateProperties: TDebuggerProperties; override;
    class function Caption: String; override;
    function  RequestCommand(const ACommand: TDBGCommand;
                             const AParams: array of const;
                             const ACallback: TMethod): Boolean; override;
  end;

procedure Register;

implementation

{ TFpRemoteDebugger }

procedure TFpRemoteDebugger.UpdateProcessConfig;
var
  AProperties: TFpDebugDebuggerPropertiesRemoteServer;
begin
  AProperties := TFpDebugDebuggerPropertiesRemoteServer(GetProperties);
  TRemoteConfig(FProcessConFig).Host := AProperties.Host;
  TRemoteConfig(FProcessConFig).Port := AProperties.Port;
  TRemoteConfig(FProcessConFig).UploadBinary := AProperties.UploadBinary;
  TRemoteConfig(FProcessConFig).AfterUploadBreakZero := AProperties.AfterUploadBreakZero;
  TRemoteConfig(FProcessConFig).AfterConnectMonitorCmds.Assign(AProperties.AfterConnectMonitorCmds);
  TRemoteConfig(FProcessConFig).AfterUploadMonitorCmds.Assign(AProperties.AfterUploadMonitorCmds);
  TRemoteConfig(FProcessConFig).SkipSectionsList.Assign(AProperties.SkipUploadOfSectionList);
end;

constructor TFpRemoteDebugger.Create(const AExternalDebugger: String);
begin
  inherited Create(AExternalDebugger);
  if Assigned(FProcessConFig) then
    FreeAndNil(FProcessConFig);
  FProcessConFig := TRemoteConfig.Create;
end;

destructor TFpRemoteDebugger.Destroy;
begin
  TRemoteConfig(FProcessConFig).Free;
  inherited Destroy;
end;

class function TFpRemoteDebugger.CreateProperties: TDebuggerProperties;
begin
  Result := TFpDebugDebuggerPropertiesRemoteServer.Create;
end;

class function TFpRemoteDebugger.Caption: String;
begin
  Result:='FpDebug Dwarf debugger - remote';
end;

function TFpRemoteDebugger.RequestCommand(const ACommand: TDBGCommand;
  const AParams: array of const; const ACallback: TMethod): Boolean;
begin
  if (ACommand in [dcRun, dcStepOver, dcStepInto, dcStepOut, dcStepTo, dcRunTo, dcJumpto,
      dcStepOverInstr, dcStepIntoInstr, dcAttach]) and
     not assigned(FDbgController.MainProcess)
  then
  begin
    UpdateProcessConfig;
    FDbgController.ProcessConfig := FProcessConFig;
  end;

  Result := inherited RequestCommand(ACommand, AParams, ACallback);
end;

{ TFpDebugDebuggerPropertiesRemoteServer }

function TFpDebugDebuggerPropertiesRemoteServer.portIsStored: Boolean;
begin
  Result := DEF_port <> FPort;
end;

function TFpDebugDebuggerPropertiesRemoteServer.hostIsStored: Boolean;
begin
  Result := FHost <> DEF_host;
end;

function TFpDebugDebuggerPropertiesRemoteServer.uploadBinaryIsStored: Boolean;
begin
  Result := FUploadBinary <> DEF_uploadBinary;
end;

procedure TFpDebugDebuggerPropertiesRemoteServer.SetAfterConnectMonitorCmds(
  AValue: TXmlConfStringList);
begin
  FAfterConnectMonitorCmds.Assign(AValue);
end;

procedure TFpDebugDebuggerPropertiesRemoteServer.SetAfterUploadMonitorCmds(
  AValue: TXmlConfStringList);
begin
  FAfterUploadMonitorCmds.Assign(AValue);
end;

procedure TFpDebugDebuggerPropertiesRemoteServer.SetSkipUploadOfSectionList(
  AValue: TXmlConfStringList);
begin
  FSkipUploadOfSectionList.Assign(AValue);
end;

constructor TFpDebugDebuggerPropertiesRemoteServer.Create;
begin
  inherited Create;
  FHost := DEF_host;
  FPort := DEF_port;
  FUploadBinary := DEF_uploadBinary;

  FAfterConnectMonitorCmds := TXmlConfStringList.Create;
  FAfterUploadMonitorCmds := TXmlConfStringList.Create;
  FSkipUploadOfSectionList := TXmlConfStringList.Create;
end;

destructor TFpDebugDebuggerPropertiesRemoteServer.Destroy;
begin
  FAfterConnectMonitorCmds.Free;
  FAfterUploadMonitorCmds.Free;
  FSkipUploadOfSectionList.Free;
  inherited Destroy;
end;

procedure TFpDebugDebuggerPropertiesRemoteServer.Assign(Source: TPersistent);
var
  asource: TFpDebugDebuggerPropertiesRemoteServer;
begin
  if Source is TFpDebugDebuggerPropertiesRemoteServer then
  begin
    aSource := TFpDebugDebuggerPropertiesRemoteServer(Source);
    FHost := asource.FHost;
    FPort := asource.FPort;
    FUploadBinary := asource.FUploadBinary;

    if Assigned(asource.FAfterConnectMonitorCmds) then
      FAfterConnectMonitorCmds.Assign(aSource.FAfterConnectMonitorCmds);

    if Assigned(asource.FAfterUploadMonitorCmds) then
      FAfterUploadMonitorCmds.Assign(aSource.FAfterUploadMonitorCmds);

    if Assigned(asource.FSkipUploadOfSectionList) then
      FSkipUploadOfSectionList.Assign(aSource.FSkipUploadOfSectionList);

    FAfterUploadBreakZero := TFpDebugDebuggerPropertiesRemoteServer(Source).FAfterUploadBreakZero;
  end;
  inherited Assign(Source);
end;

procedure Register;
begin
  RegisterDebugger(TFpRemoteDebugger);
end;


end.

