unit lazopenapictrl;

{$mode objfpc}{$H+}

interface

uses
  Types, Classes, SysUtils, fpjson, fpopenapi.types, fpopenapi.pascaltypes, fpopenapi.objects, fpopenapi.codegen, IDECommands, ProjectIntf, MenuIntf;

Type
  TOpenAPIProjectType = (optClient,optServer);
  TOpenAPIProjectTypes = set of TOpenAPIProjectType;

  TIDEProjectType = (iptGUI,iptCmdLine,iptHTTPServer);

  { TLazOpenAPICodeGen }

  TLazOpenAPICodeGen = class(TOpenAPICodeGen)
  private
    FServerServiceModules,
    FServerURLS: TStringDynArray;
  Protected
    procedure PrepareAPIData(aData: TAPIData); override;
  Public
    Function ResolveUnitName(aKind : TUnitKind; aFull: Boolean = True) : String;
    Property ServerURLS : TStringDynArray Read FServerURLS;
    Property ServerServiceModules : TStringDynArray Read FServerServiceModules;
  end;

  { TOpenAPIHandler }

  TOpenAPIHandler = Class(TObject)
  public
    CmdToolsMenu : TIDECommandCategory;
    RefreshMenu : TIDEmenuCommand;
    OpenAPIWizardCommand : TIDECommand;
    procedure GenerateFiles(const aOpenAPIFile, aBaseOutputFile: string; aGenerator: TLazOpenAPICodeGen);
    function GetJSONFromYAML(const aOpenAPIFile: string): TJSONStringType;
    function OpenAPIConfigOK(out aOpenAPI, aConfig: string): Boolean;
    procedure SetProjectData(aProject: TLazProject; const aConfig, aOpenAPIFileName, aBaseFileName: string);
    procedure GetProjectData(aProject: TLazProject; out aConfig, aOpenAPIFileName, aBaseFileName: string);
    Procedure HandleRefreshOpenAPI(Sender : TObject);
    Procedure HandleProjectInspectorPopup(Sender : TObject);
    Function IsOpenAPIProject : Boolean;
    Function OpenAPIConfigOK : Boolean;
  end;

var
  OpenAPIHandler : TOpenAPIHandler;

implementation

uses
  fpyaml.parser,
  fpyaml.data,
  fpyaml.json,
  fpopenapi.reader,
  lazopenapistr,
  lazideintf,
  ideintf,
  IDEMsgIntf,
  IDEExternToolIntf;


{ TLazOpenAPICodeGen }

procedure TLazOpenAPICodeGen.PrepareAPIData(aData: TAPIData);
var
  I : Integer;
begin
  Inherited;
  SetLength(FServerURLS,0);
  if API.HasKeyWord(oakServers) then
    begin
    SetLength(FServerURLS,API.Servers.Count);
    For I:=0 to API.Servers.Count-1 do
      FServerURLS[i]:=API.Servers[I].Url;
    end;
  if GenerateServer then
    begin
    SetLength(FServerServiceModules,aData.ServiceCount);
    for I:=0 to aData.ServiceCount-1 do
      FServerServiceModules[i]:='T'+aData.Services[i].ServiceName+'Module';
    end;
end;

function TLazOpenAPICodeGen.ResolveUnitName(aKind: TUnitKind; aFull: Boolean): String;
begin
  Result:=ResolveUnit(aKind,aFull);
end;

{ TOpenAPIHandler }

Function TOpenAPIHandler.GetJSONFromYAML(const aOpenAPIFile : string) : TJSONStringType;

var
  lParser : TYAMLParser;
  lYAML : TYAMLStream;
  lJSON : TJSONData;

begin
  lYAML:=Nil;
  lJSON:=Nil;
  lParser:=TYAMLParser.Create(aOpenAPIFile);
  try
    lYAML:=lParser.Parse;
    lJSON:=YamlToJSON(lYAML);
    Result:=lJSON.FormatJSON();
  finally
    lYAML.Free;
    lJSON.Free;
    lParser.Free;
  end;
end;

Procedure TOpenAPIHandler.GenerateFiles(const aOpenAPIFile, aBaseOutputFile : string; aGenerator : TLazOpenAPICodeGen);

var
  Loader : TOpenAPIReader;
  API : TOpenAPI;
  lJSON : String;

begin
  Loader:=Nil;
  API:=TOpenAPI.Create;
  try
    Loader:=TOpenAPIReader.Create(Nil);
    if TYAMLParser.IsYamlFileName(aOpenAPIFile) then
      begin
      lJSON:=GetJSONFromYAML(aOpenAPIFile);
      Loader.ReadFromString(API,lJSON);
      end
    else
      // Assume JSON
      Loader.ReadFromFile(API,aOpenAPIFile);

    aGenerator.API:=API;
    aGenerator.BaseOutputFileName:=aBaseOutputFile;
    aGenerator.Execute;
  finally
    Loader.Free;
    API.Free;
  end;
end;

function TOpenAPIHandler.IsOpenAPIProject: Boolean;
begin
  Result:=(LazarusIDE.ActiveProject.CustomData.Values[SDataOpenAPIFile]<>'');
end;

function TOpenAPIHandler.OpenAPIConfigOK: Boolean;

var
  a,b : string;

begin
  Result:=OpenAPIConfigOK(a,b);
end;

function TOpenAPIHandler.OpenAPIConfigOK(out aOpenAPI,aConfig : string) : Boolean;


var
  lPath,lFileName : String;

begin
  lFileName:=LazarusIDE.ActiveProject.CustomData.Values[SDataOpenAPIFile];
  Result:=(lFileName<>'');
  if Not Result  then
    Exit;
  lPath:=ExtractFilePath(LazarusIDE.ActiveProject.ProjectInfoFile);
  lFileName:=ExpandFileName(lPath+lFileName);
  Result:=FileExists(lFileName);
  if Not Result then
    begin
    AddIDEMessage(mluFatal,format(SErrInvalidOpenAPIFile,[lFileName]),'',0,0,SOpenAPICodeGenerator);
    Exit;
    end;
  aOpenAPI:=lFileName;
  lFileName:=LazarusIDE.ActiveProject.CustomData.Values[SDataOpenAPIConfig];
  lFileName:=ExpandFileName(lPath+lFileName);
  Result:=FileExists(lFileName);
  if Not Result then
    AddIDEMessage(mluFatal,format(SErrInvalidOpenAPIConfigFile,[lFileName]),'',0,0,SOpenAPICodeGenerator)
  else
    aConfig:=lFileName;
end;

procedure TOpenAPIHandler.GetProjectData(aProject: TLazProject; out aConfig, aOpenAPIFileName, aBaseFileName: string);

var
  lPath: String;
begin
  lPath:=ExtractFilePath(aProject.ProjectInfoFile);
  With aProject.CustomData do
    begin
    aConfig:=ExpandFileName(lPath+Values[SDataOpenAPIConfig]);
    aOpenAPIFileName:=ExpandFileName(lPath+Values[SDataOpenAPIFile]);
    aBaseFileName:=ExpandFileName(lPath+Values[SDataOpenAPIBaseFileName]);
    end;
end;

procedure TOpenAPIHandler.SetProjectData(aProject: TLazProject; const aConfig, aOpenAPIFileName, aBaseFileName: string);
var
  lPath : String;

begin
  lPath:=ExtractFilePath(aProject.ProjectInfoFile);
  With aProject.CustomData do
    begin
    Add(SDataOpenAPIFile,ExtractRelativePath(lPath,aOpenAPIFileName));
    Add(SDataOpenAPIConfig,ExtractRelativePath(lPath,aConfig));
    Add(SDataOpenAPIBaseFileName,ExtractRelativePath(lPath,aBaseFileName));
    end;
end;

procedure TOpenAPIHandler.HandleRefreshOpenAPI(Sender: TObject);

var
  lPath,lBaseFileName,lOpenAPIFileName,lConfigFileName : String;
  lGen : TLazOpenAPICodeGen;

begin
  if not OpenAPIConfigOK(lOpenAPIFileName,lConfigFileName) then
    exit;
  lPath:=ExtractFilePath(LazarusIDE.ActiveProject.ProjectInfoFile);
  lBaseFileName:=ExpandFileName(lPath+LazarusIDE.ActiveProject.CustomData.Values[SDataOpenAPIBaseFileName]);
  lGen:=TLazOpenAPICodeGen.Create(Nil);
  try
    lGen.LoadConfig(lConfigFileName);
    lGen.SkipServerServiceImplementationModule:=True;
    GenerateFiles(lOpenAPIFileName,lBaseFileName,lGen);
  finally
    lGen.Free;
  end;
end;

procedure TOpenAPIHandler.HandleProjectInspectorPopup(Sender: TObject);

begin
  RefreshMenu.Visible:=IsOpenAPIProject;
end;


end.

