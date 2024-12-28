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
  IdeDebuggerOpts, IdeDebuggerBackendValueConv,
  IdeDebuggerValueFormatter, IdeDebuggerDisplayFormats,
  // IdeProject
  Project;

type

  { TProjectDebugLink }

  TProjectDebugLink = class(TProjectDebugLinkBase)
  private
    FDisplayFormatConfigs: TDisplayFormatConfig;
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
    FStoreDisplayFormatConfigsInSession: boolean;
    FValueFormatterConfigWasFromSession, FValueFormatterConfigWasFromLPI: boolean;
    FUseDisplayFormatConfigsFromIDE: boolean;
    FUseDisplayFormatConfigsFromProject: boolean;
    FStoreValueFormatterConfigInSession: boolean;
    FDisplayFormatConfigsWasFromSession, FDisplayFormatConfigsWasFromLPI: boolean;
    FUseValueFormatterFromIDE: boolean;
    FUseValueFormatterFromProject: boolean;
    FValueFormatterConfig: TIdeDbgValueFormatterSelectorList;
    function GetCurrentDebuggerBackend: String;
    procedure SetDebuggerBackend(AValue: String);
    procedure SetProject(AValue: TProject); override;
    procedure SetStoreDebuggerClassConfInSession(AValue: boolean);
    procedure SetStoreBackendConverterConfigInSession(AValue: boolean);
    procedure SetStoreDisplayFormatConfigsInSession(AValue: boolean);
    procedure SetStoreValueFormatterConfigInSession(AValue: boolean);
    procedure SetUseBackendConverterFromIDE(AValue: boolean);
    procedure SetUseBackendConverterFromProject(AValue: boolean);
    procedure BackendConverterConfigChanged(Sender: TObject);
    procedure SetUseDisplayFormatConfigsFromIDE(AValue: boolean);
    procedure SetUseDisplayFormatConfigsFromProject(AValue: boolean);
    procedure SetUseValueFormatterFromIDE(AValue: boolean);
    procedure SetUseValueFormatterFromProject(AValue: boolean);
    procedure DisplayFormatConfigsChanged(Sender: TObject);
    procedure ValueFormatterConfigChanged(Sender: TObject);
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
    property DisplayFormatConfigs: TDisplayFormatConfig read FDisplayFormatConfigs;
    property ValueFormatterConfig: TIdeDbgValueFormatterSelectorList read FValueFormatterConfig
                                                                     write FValueFormatterConfig;
  published
    property StoreDisplayFormatConfigsInSession: boolean read FStoreDisplayFormatConfigsInSession
                                                        write SetStoreDisplayFormatConfigsInSession default False;
    property UseDisplayFormatConfigsFromIDE: boolean read FUseDisplayFormatConfigsFromIDE
                                              write SetUseDisplayFormatConfigsFromIDE default True;
    property UseDisplayFormatConfigsFromProject: boolean read FUseDisplayFormatConfigsFromProject
                                                  write SetUseDisplayFormatConfigsFromProject default True;

    property StoreValueFormatterConfigInSession: boolean read FStoreValueFormatterConfigInSession
                                                        write SetStoreValueFormatterConfigInSession default False;
    property UseValueFormatterFromIDE: boolean read FUseValueFormatterFromIDE
                                              write SetUseValueFormatterFromIDE default True;
    property UseValueFormatterFromProject: boolean read FUseValueFormatterFromProject
                                                  write SetUseValueFormatterFromProject default True;
  end;

function GetDbgProjectLink: TProjectDebugLink;

property DbgProjectLink: TProjectDebugLink read GetDbgProjectLink;

implementation

var
  TheDbgProjectLink: TProjectDebugLink;

function GetDbgProjectLink: TProjectDebugLink;
begin
  if TheDbgProjectLink = nil then
    TheDbgProjectLink := TProjectDebugLink.Create;
  Result := TheDbgProjectLink;
end;

{ TProjectDebugLink }

constructor TProjectDebugLink.Create;
begin
  FDebuggerProperties := TDebuggerPropertiesConfigList.Create;
  FBackendConverterConfig := TIdeDbgValueConvertSelectorList.Create;
  FBackendConverterConfig.OnChanged := @BackendConverterConfigChanged;
  FUseBackendConverterFromIDE := True;
  FUseBackendConverterFromProject := True;
  FDisplayFormatConfigs := TDisplayFormatConfig.Create;
  FDisplayFormatConfigs.OnChanged := @DisplayFormatConfigsChanged;
  FUseDisplayFormatConfigsFromIDE := True;
  FUseDisplayFormatConfigsFromProject := True;
  FValueFormatterConfig := TIdeDbgValueFormatterSelectorList.Create;
  FValueFormatterConfig.OnChanged := @ValueFormatterConfigChanged;
  FUseValueFormatterFromIDE := True;
  FUseValueFormatterFromProject := True;
end;

destructor TProjectDebugLink.Destroy;
begin
  FreeAndNil(FDebuggerProperties);
  FreeAndNil(FBackendConverterConfig);
  FreeAndNil(FDisplayFormatConfigs);
  FreeAndNil(FValueFormatterConfig);
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
  if FProject <> AValue then
    Clear;
  FProject := AValue;
  if Assigned(FProject) then
    FProject.DebuggerLink := Self;
  FBackendConverterConfig.CallChangeNotifications;
  FValueFormatterConfig.CallChangeNotifications;
  FDisplayFormatConfigs.CallChangeNotifications;
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

procedure TProjectDebugLink.SetStoreDisplayFormatConfigsInSession(AValue: boolean);
begin
  if FStoreDisplayFormatConfigsInSession = AValue then Exit;
  FStoreDisplayFormatConfigsInSession := AValue;
  FProject.Modified := True;
  FProject.SessionModified := True;
end;

procedure TProjectDebugLink.SetStoreValueFormatterConfigInSession(
  AValue: boolean);
begin
  if FStoreValueFormatterConfigInSession = AValue then Exit;
  FStoreValueFormatterConfigInSession := AValue;
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

procedure TProjectDebugLink.SetUseDisplayFormatConfigsFromIDE(AValue: boolean);
begin
  if FUseDisplayFormatConfigsFromIDE = AValue then Exit;
  FUseDisplayFormatConfigsFromIDE := AValue;
  FProject.SessionModified := True;
end;

procedure TProjectDebugLink.SetUseDisplayFormatConfigsFromProject(AValue: boolean);
begin
  if FUseDisplayFormatConfigsFromProject = AValue then Exit;
  FUseDisplayFormatConfigsFromProject := AValue;
  FProject.SessionModified := True;
end;

procedure TProjectDebugLink.SetUseValueFormatterFromIDE(AValue: boolean);
begin
  if FUseValueFormatterFromIDE = AValue then Exit;
  FUseValueFormatterFromIDE := AValue;
  FProject.SessionModified := True;
end;

procedure TProjectDebugLink.SetUseValueFormatterFromProject(AValue: boolean);
begin
  if FUseValueFormatterFromProject = AValue then Exit;
  FUseValueFormatterFromProject := AValue;
  FProject.SessionModified := True;
end;

procedure TProjectDebugLink.DisplayFormatConfigsChanged(Sender: TObject);
begin
  if FStoreDisplayFormatConfigsInSession then
    FProject.SessionModified := True
  else
    FProject.Modified := True;
end;

procedure TProjectDebugLink.ValueFormatterConfigChanged(Sender: TObject);
begin
  if FStoreValueFormatterConfigInSession then
    FProject.SessionModified := True
  else
    FProject.Modified := True;
end;

procedure TProjectDebugLink.Clear;
begin
  FUseBackendConverterFromIDE := True;
  FUseBackendConverterFromProject := True;
  FStoreBackendConverterConfigInSession := False;

  FUseDisplayFormatConfigsFromIDE := True;
  FUseDisplayFormatConfigsFromProject := True;
  FStoreDisplayFormatConfigsInSession := False;

  FUseValueFormatterFromIDE := True;
  FUseValueFormatterFromProject := True;
  FStoreValueFormatterConfigInSession := False;

  FDebuggerBackend := '';

  FDebuggerProperties.Clear;
  FDisplayFormatConfigs.Clear;
  FValueFormatterConfig.Clear;
  FBackendConverterConfig.Clear;
end;

procedure TProjectDebugLink.BeforeReadProject;
begin
  FDebuggerClassConfWasFromSession := False;
  FDebuggerClassConfWasFromLPI := False;
  FDebuggerBackend := '';
end;

procedure TProjectDebugLink.AfterReadProject;
begin
  //
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
    FBackendConverterConfigWasFromLPI := True;
  end;
  // This is for backward compatibility (only trunk 2.1 did use this / Can be removed in some time after 2.2 / but needs LoadFromSession to change default to '')
  FDebuggerBackend := aXMLConfig.GetValue(Path+'Debugger/Backend/Value', '');

  if not FStoreDisplayFormatConfigsInSession then begin
    FDisplayFormatConfigs.LoadFromXml(aXMLConfig, Path+'Debugger/DisplayFormatConfigs/');
    FDisplayFormatConfigsWasFromLPI := True;
  end;

  if not FStoreValueFormatterConfigInSession then begin
    FValueFormatterConfig.LoadDataFromXMLConfig(aXMLConfig, Path+'Debugger/ValueFormatter/');
    FValueFormatterConfigWasFromLPI := True;
  end;

  aXMLConfig.ReadObject(Path+'Debugger/', Self);
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
    FBackendConverterConfigWasFromSession := True;
  end;

  if FStoreDisplayFormatConfigsInSession then begin
    FDisplayFormatConfigs.LoadFromXml(aXMLConfig, Path+'Debugger/DisplayFormatConfigs/');
    FDisplayFormatConfigsWasFromSession := True;
  end;

  if FStoreValueFormatterConfigInSession then begin
    FValueFormatterConfig.LoadDataFromXMLConfig(aXMLConfig, Path+'Debugger/ValueFormatter/');
    FValueFormatterConfigWasFromSession := True;
  end;

  aXMLConfig.ReadObject(Path+'Debugger/', Self);
end;

procedure TProjectDebugLink.SaveToLPI(aXMLConfig: TRttiXMLConfig; Path: string);
var
  Def: TProjectDebugLink;
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

  if not FStoreDisplayFormatConfigsInSession then
    FDisplayFormatConfigs.SaveToXml(aXMLConfig, Path+'Debugger/DisplayFormatConfigs/')
  else if FDisplayFormatConfigsWasFromLPI then
    aXMLConfig.DeletePath(Path+'Debugger/DisplayFormatConfigs');
  FDisplayFormatConfigsWasFromSession := False;
  FDisplayFormatConfigsWasFromLPI := False;

  if not FStoreValueFormatterConfigInSession then
    FValueFormatterConfig.SaveDataToXMLConfig(aXMLConfig, Path+'Debugger/ValueFormatter/')
  else if FValueFormatterConfigWasFromLPI then
    aXMLConfig.DeletePath(Path+'Debugger/ValueFormatter');
  FValueFormatterConfigWasFromSession := False;
  FValueFormatterConfigWasFromLPI := False;

  Def := TProjectDebugLink.Create;
  aXMLConfig.WriteObject(Path+'Debugger/', Self, Def);
  Def.Free;
end;

procedure TProjectDebugLink.SaveToSession(aXMLConfig: TRttiXMLConfig; Path: string);
var
  Def: TProjectDebugLink;
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

  if FStoreDisplayFormatConfigsInSession then
    FDisplayFormatConfigs.SaveToXml(aXMLConfig, Path+'Debugger/DisplayFormatConfigs/')
  else if FDisplayFormatConfigsWasFromSession then
    aXMLConfig.DeletePath(Path+'Debugger/DisplayFormatConfigs');
  FDisplayFormatConfigsWasFromSession := False;
  FDisplayFormatConfigsWasFromLPI := False;

  if FStoreValueFormatterConfigInSession then
    FValueFormatterConfig.SaveDataToXMLConfig(aXMLConfig, Path+'Debugger/ValueFormatter/')
  else if FValueFormatterConfigWasFromSession then
    aXMLConfig.DeletePath(Path+'Debugger/ValueFormatter');
  FValueFormatterConfigWasFromSession := False;
  FValueFormatterConfigWasFromLPI := False;

  Def := TProjectDebugLink.Create;
  aXMLConfig.WriteObject(Path+'Debugger/', Self, Def);
  Def.Free;
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

finalization
  FreeAndNil(TheDbgProjectLink);
end.

