unit ProjectDebugLink;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LazUtils
  Laz2_XMLCfg, LazLoggerBase,
  // DebuggerIntf
  DbgIntfDebuggerBase,
  // IdeDebugger
  IdeDebuggerOpts, IdeDebuggerBackendValueConv, Debugger,
  // IDE
  Project;

type

  { TProjectDebugLink }

  TProjectDebugLink = class(TProjectDebugLinkBase)
  private
    FProject: TProject;
    FDebuggerProperties: TDebuggerPropertiesConfigList; // named entries
    FDebuggerBackend: String;
    FBackendConverterConfig: TIdeDbgValueConvertSelectorList;
    FStoreDebuggerClassConfInSession: boolean;
    FDebuggerClassConfWasFromSession, FDebuggerClassConfWasFromLPI: boolean;
    FStoreBackendConverterConfigInSession: boolean;
    FBackendConverterConfigWasFromSession, FBackendConverterConfigWasFromLPI: boolean;
    FUseBackendConverterFromIDE: boolean;
    FUseBackendConverterFromProject: boolean;
    function GetCurrentDebuggerBackend: String;
    procedure SetDebuggerBackend(AValue: String);
    procedure SetProject(AValue: TProject);
    procedure SetStoreDebuggerClassConfInSession(AValue: boolean);
    procedure SetStoreBackendConverterConfigInSession(AValue: boolean);
    procedure SetUseBackendConverterFromIDE(AValue: boolean);
    procedure SetUseBackendConverterFromProject(AValue: boolean);
    procedure BackendConverterConfigChanged(Sender: TObject);
  protected
    procedure Clear; override;
    procedure BeforeReadProject; override;
    procedure AfterReadProject; override;
    procedure LoadFromLPI(aXMLConfig: TRttiXMLConfig; Path: string); override;
    procedure LoadFromSession(aXMLConfig: TRttiXMLConfig; Path: string); override;
    procedure SaveToLPI(aXMLConfig: TRttiXMLConfig; Path: string); override;
    procedure SaveToSession(aXMLConfig: TRttiXMLConfig; Path: string); override;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function DebuggerPropertiesConfigList: TDebuggerPropertiesConfigList;
    function CurrentDebuggerClass: TDebuggerClass;
    function DebuggerFilename: string;
    function GetParsedDebuggerFilename: string;
    function CurrentDebuggerPropertiesConfig: TDebuggerPropertiesConfig;
    procedure MarkDebuggerClassConfAsModified;

    property Project: TProject read FProject write SetProject;
    property CurrentDebuggerBackend: String read GetCurrentDebuggerBackend;
    property DebuggerBackend: String read FDebuggerBackend write SetDebuggerBackend;
    property StoreDebuggerClassConfInSession: boolean read FStoreDebuggerClassConfInSession
                                                   write SetStoreDebuggerClassConfInSession;
    property BackendConverterConfig: TIdeDbgValueConvertSelectorList read FBackendConverterConfig
                                                                    write FBackendConverterConfig;
    property StoreBackendConverterConfigInSession: boolean read FStoreBackendConverterConfigInSession
                                                        write SetStoreBackendConverterConfigInSession;
    property UseBackendConverterFromIDE: boolean read FUseBackendConverterFromIDE
                                              write SetUseBackendConverterFromIDE;
    property UseBackendConverterFromProject: boolean read FUseBackendConverterFromProject
                                                  write SetUseBackendConverterFromProject;
  end;

implementation

{ TProjectDebugLink }

constructor TProjectDebugLink.Create;
begin
  DebugLn(['TProjectDebugLink.Create: Project=', FProject]);
  FDebuggerProperties := TDebuggerPropertiesConfigList.Create;
  FBackendConverterConfig := TIdeDbgValueConvertSelectorList.Create;
  FBackendConverterConfig.OnChanged := @BackendConverterConfigChanged;
  FUseBackendConverterFromIDE := True;
  FUseBackendConverterFromProject := True;
  if DebugBossManager <> nil then
    DebugBossManager.DoBackendConverterChanged;
end;

destructor TProjectDebugLink.Destroy;
begin
  FreeAndNil(FDebuggerProperties);
  FreeAndNil(FBackendConverterConfig);
  inherited Destroy;
end;

function TProjectDebugLink.GetCurrentDebuggerBackend: String;
begin
  Result := FDebuggerBackend;
end;

procedure TProjectDebugLink.SetDebuggerBackend(AValue: String);
begin
  if FDebuggerBackend = AValue then Exit;
  FDebuggerBackend := AValue;
  FProject.SessionModified := True;
end;

procedure TProjectDebugLink.SetProject(AValue: TProject);
begin
  // A new project can have the same address as a previous freed project and this test
  //if FProject = AValue then Exit; // would prevent updating FProject.DebuggerLink.
  FProject := AValue;
  if Assigned(FProject) then
    FProject.DebuggerLink := Self;
end;

procedure TProjectDebugLink.SetStoreDebuggerClassConfInSession(AValue: boolean);
begin
  if FStoreDebuggerClassConfInSession = AValue then Exit;
  FStoreDebuggerClassConfInSession := AValue;
  FProject.Modified := True;
  FProject.SessionModified := True;
end;

procedure TProjectDebugLink.SetStoreBackendConverterConfigInSession(AValue: boolean);
begin
  if FStoreBackendConverterConfigInSession = AValue then Exit;
  FStoreBackendConverterConfigInSession := AValue;
  FProject.Modified := True;
  FProject.SessionModified := True;
end;

procedure TProjectDebugLink.SetUseBackendConverterFromIDE(AValue: boolean);
begin
  if FUseBackendConverterFromIDE = AValue then Exit;
  FUseBackendConverterFromIDE := AValue;
  FProject.SessionModified := True;
end;

procedure TProjectDebugLink.SetUseBackendConverterFromProject(AValue: boolean);
begin
  if FUseBackendConverterFromProject = AValue then Exit;
  FUseBackendConverterFromProject := AValue;
  FProject.SessionModified := True;
end;

procedure TProjectDebugLink.BackendConverterConfigChanged(Sender: TObject);
begin
  if FStoreBackendConverterConfigInSession then
    FProject.SessionModified := True
  else
    FProject.Modified := True;
end;

procedure TProjectDebugLink.Clear;
begin
  FUseBackendConverterFromIDE := True;
  FUseBackendConverterFromProject := True;
end;

procedure TProjectDebugLink.BeforeReadProject;
begin
  FDebuggerClassConfWasFromSession := False;
  FDebuggerClassConfWasFromLPI := False;
end;

procedure TProjectDebugLink.AfterReadProject;
begin
  if DebugBossManager <> nil then
    DebugBossManager.DoBackendConverterChanged;
end;

procedure TProjectDebugLink.LoadFromLPI(aXMLConfig: TRttiXMLConfig; Path: string);
begin
  FStoreDebuggerClassConfInSession := aXMLConfig.GetValue(Path+'Debugger/StoreDebuggerClassConfInSession/Value', False);
  if not FStoreDebuggerClassConfInSession then begin
    FDebuggerProperties.LoadFromXml(aXMLConfig, Path+'Debugger/ClassConfig/');
    FDebuggerClassConfWasFromLPI := True;
  end;
  FStoreBackendConverterConfigInSession := aXMLConfig.GetValue(Path+'Debugger/StoreBackendConverterConfigInSession/Value', False);
  if not FStoreBackendConverterConfigInSession then begin
    FBackendConverterConfig.LoadDataFromXMLConfig(aXMLConfig, Path+'Debugger/BackendConv/');
    ProjectValueConverterSelectorList := FBackendConverterConfig;
    FBackendConverterConfigWasFromLPI := True;
  end;
  // This is for backward compatibility (only trunk 2.1 did use this / Can be removed in some time after 2.2 / but needs LoadFromSession to change default to '')
  FDebuggerBackend := aXMLConfig.GetValue(Path+'Debugger/Backend/Value', '');
end;

procedure TProjectDebugLink.LoadFromSession(aXMLConfig: TRttiXMLConfig; Path: string);
begin
  // Load from LPI will have been called first => so if session has no value, we keep the LPI value (as for some time, the data was stored in the LPI)
  FDebuggerBackend := aXMLConfig.GetValue(Path+'Debugger/Backend/Value', FDebuggerBackend);
  if FStoreDebuggerClassConfInSession then begin
    FDebuggerProperties.LoadFromXml(aXMLConfig, Path+'Debugger/ClassConfig/');
    FDebuggerClassConfWasFromSession := True;
  end;
  FUseBackendConverterFromIDE :=     aXMLConfig.GetValue(Path+'Debugger/BackendConvOpts/UseBackendConverterFromIDE', True);
  FUseBackendConverterFromProject := aXMLConfig.GetValue(Path+'Debugger/BackendConvOpts/UseBackendConverterFromProject', True);
  if FStoreBackendConverterConfigInSession then begin
    FBackendConverterConfig.LoadDataFromXMLConfig(aXMLConfig, Path+'Debugger/BackendConv/');
    ProjectValueConverterSelectorList := FBackendConverterConfig;
    FBackendConverterConfigWasFromSession := True;
  end;
end;

procedure TProjectDebugLink.SaveToLPI(aXMLConfig: TRttiXMLConfig; Path: string);
begin
  aXMLConfig.DeletePath(Path+'Debugger/Backend'); // remove old value from trunk 2.1
  aXMLConfig.SetDeleteValue(Path+'Debugger/StoreDebuggerClassConfInSession/Value', FStoreDebuggerClassConfInSession, False);
  if not FStoreDebuggerClassConfInSession then
    FDebuggerProperties.SaveToXml(aXMLConfig, Path+'Debugger/ClassConfig/')
  else if FDebuggerClassConfWasFromLPI then
    aXMLConfig.DeletePath(Path+'Debugger/ClassConfig');
  FDebuggerClassConfWasFromSession := False;
  FDebuggerClassConfWasFromLPI := False;
  aXMLConfig.SetDeleteValue(Path+'Debugger/StoreBackendConverterConfigInSession/Value', FStoreBackendConverterConfigInSession, False);
  if not FStoreBackendConverterConfigInSession then
    FBackendConverterConfig.SaveDataToXMLConfig(aXMLConfig, Path+'Debugger/BackendConv/')
  else if FBackendConverterConfigWasFromLPI then
    aXMLConfig.DeletePath(Path+'Debugger/BackendConv');
  FBackendConverterConfigWasFromSession := False;
  FBackendConverterConfigWasFromLPI := False;
end;

procedure TProjectDebugLink.SaveToSession(aXMLConfig: TRttiXMLConfig; Path: string);
begin
  aXMLConfig.SetDeleteValue(Path+'Debugger/Backend/Value', DebuggerBackend, '');
  if FStoreDebuggerClassConfInSession then
    FDebuggerProperties.SaveToXml(aXMLConfig, Path+'Debugger/ClassConfig/')
  else if FDebuggerClassConfWasFromSession then
    aXMLConfig.DeletePath(Path+'Debugger/ClassConfig');
  FDebuggerClassConfWasFromSession := False;
  FDebuggerClassConfWasFromLPI := False;
  aXMLConfig.SetDeleteValue(Path+'Debugger/BackendConvOpts/UseBackendConverterFromIDE', FUseBackendConverterFromIDE, True);
  aXMLConfig.SetDeleteValue(Path+'Debugger/BackendConvOpts/UseBackendConverterFromProject', FUseBackendConverterFromProject, True);
  if FStoreBackendConverterConfigInSession then
    FBackendConverterConfig.SaveDataToXMLConfig(aXMLConfig, Path+'Debugger/BackendConv/')
  else if FBackendConverterConfigWasFromSession then
    aXMLConfig.DeletePath(Path+'Debugger/BackendConv');
  FBackendConverterConfigWasFromSession := False;
  FBackendConverterConfigWasFromLPI := False;
end;

function TProjectDebugLink.DebuggerPropertiesConfigList: TDebuggerPropertiesConfigList;
begin
  Result := FDebuggerProperties;
end;

function TProjectDebugLink.CurrentDebuggerClass: TDebuggerClass;
var
  DbgCfg: TDebuggerPropertiesConfig;
begin
  Result := nil;
  DbgCfg := CurrentDebuggerPropertiesConfig;
  if  DbgCfg<> nil then
    Result := DbgCfg.DebuggerClass;
end;

function TProjectDebugLink.DebuggerFilename: string;
var
  DbgCfg: TDebuggerPropertiesConfig;
begin
  Result := '';
  DbgCfg := CurrentDebuggerPropertiesConfig;
  if DbgCfg <> nil then
    Result := DbgCfg.DebuggerFilename;
end;

function TProjectDebugLink.GetParsedDebuggerFilename: string;
begin
  Result := DebuggerOptions.GetParsedDebuggerFilename(DebuggerFilename);
end;

function TProjectDebugLink.CurrentDebuggerPropertiesConfig: TDebuggerPropertiesConfig;
begin
  if Self = nil then
    exit(DebuggerOptions.CurrentDebuggerPropertiesConfig);

  Result := nil;
  if (CurrentDebuggerBackend <> '') then
    if (CurrentDebuggerBackend = 'IDE') then
      Result := DebuggerOptions.CurrentDebuggerPropertiesConfig
    else
      Result := DebuggerOptions.CurrentDebuggerPropertiesConfigEx(CurrentDebuggerBackend);

  if Result = nil then
    Result := FDebuggerProperties.CurrentDebuggerPropertiesConfig;

  // No project config?
  if Result = nil then
    Result := DebuggerOptions.CurrentDebuggerPropertiesConfig;
end;

procedure TProjectDebugLink.MarkDebuggerClassConfAsModified;
begin
  if FStoreDebuggerClassConfInSession then
    FProject.SessionModified := True
  else
    FProject.Modified := True;
end;

end.

