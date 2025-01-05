unit fraopenapisettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, EditBtn, StdCtrls, ComCtrls, ValEdit, fpopenapi.codegen, Types;

type

  { TGeneratorSettingsFrame }

  TGeneratorSettingsFrame = class(TFrame)
    btnLoadUUIDMap: TButton;
    btnLoadUUIDMap1: TButton;
    cbGenServer: TCheckBox;
    CBGenClient: TCheckBox;
    CBDelphiCode: TCheckBox;
    cbOpenFiles: TCheckBox;
    CBVerboseHeader: TCheckBox;
    CBEnums: TCheckBox;
    CBAsyncService: TCheckBox;
    cbCancelRequest: TCheckBox;
    CBSkipImplementation: TCheckBox;
    CBAbstractCalls: TCheckBox;
    cbAddToProject: TCheckBox;
    cbGenerateServerProxyModule: TCheckBox;
    cbProxyModuleFormFile: TCheckBox;
    edtServerProxyUnit: TEdit;
    edtServerProxyModule: TEdit;
    edtClientServiceImplementationUnit: TEdit;
    edtClientServiceInterfaceUnit: TEdit;
    edtClientServiceParentClass: TEdit;
    edtClientServiceParentUnit: TEdit;
    edtServerHandlerUnitName: TEdit;
    edtServerImplementationUnitName: TEdit;
    edtServerServiceParentClass: TEdit;
    edtServerServiceParentUnit: TEdit;
    edtServiceNameSuffix: TEdit;
    edtServiceNamePrefix: TEdit;
    edtSerializeUnit: TEdit;
    edtDtoUnit: TEdit;
    edtUnitExtension: TEdit;
    edtUnitSuffix: TEdit;
    edtUUIDMap: TFileNameEdit;
    edtServiceMapFile: TFileNameEdit;
    GBAutoNaming: TGroupBox;
    lblServerProxyUnit: TLabel;
    lblServiceNamePrefix: TLabel;
    Label2: TLabel;
    lblUUIDMap: TLabel;
    lblServerServiceParentUnit: TLabel;
    lblServerServiceParentClass: TLabel;
    lblServerImplementationUnitName: TLabel;
    lblServerHandlerUnitName: TLabel;
    lblClientServiceInterfaceUnit: TLabel;
    lblClientServiceImplementationUnit: TLabel;
    lblClientServiceParentClass: TLabel;
    lblClientServiceParentUnit: TLabel;
    lblDtoUnitName: TLabel;
    lblSerializeUnit: TLabel;
    lblServiceNameSuffix: TLabel;
    lblUnitNameExtension: TLabel;
    lblOpenAPIFile : TLabel;
    edtFile : TFileNameEdit;
    lblUnitSuffix: TLabel;
    lblUUIDMap1: TLabel;
    PCSettings: TPageControl;
    TSServiceMap: TTabSheet;
    TSUUIDMap: TTabSheet;
    TSData: TTabSheet;
    TSGeneral: TTabSheet;
    TSClient: TTabSheet;
    TSServer: TTabSheet;
    VLEUUIDMap: TValueListEditor;
    VLEServiceMap: TValueListEditor;
    procedure btnLoadUUIDMap1Click(Sender: TObject);
    procedure btnLoadUUIDMapClick(Sender: TObject);
    procedure cbGenerateServerProxyModuleChange(Sender: TObject);
    procedure HandleAbstract(Sender: TObject);
    procedure HandleSyncCheck(Sender: TObject);
    procedure TSGeneralContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
  private
    FGenerator: TOpenAPICodeGen;
    procedure CheckAbstract;
    procedure CheckAsync;
    procedure CheckProxyModule;
    function GetAddToProject: Boolean;
    function GetOpenAPIFileName: String;
    function GetOpenGeneratedFiles: Boolean;
    procedure LoadFileToEditor(aEditor: TValueListEditor; const aFilename, aDescription: String);
    procedure SetAddToProject(AValue: Boolean);
    procedure SetOpenAPIFileName(AValue: String);
    procedure SetOpenGeneratedFiles(AValue: Boolean);
  public
    procedure Clear;
    procedure InitFileNameEdits(Const aBaseDir : string);
    Procedure SaveSettings;
    procedure ShowSettings;
    procedure HideAdditionalControls(ShowClient: Boolean;ShowServer: Boolean);
    function Modified : Boolean;
    Property OpenAPIFileName : String Read GetOpenAPIFileName Write SetOpenAPIFileName;
    Property OpenGeneratedFiles : Boolean Read GetOpenGeneratedFiles Write SetOpenGeneratedFiles;
    Property AddFilesToProject : Boolean Read GetAddToProject Write SetAddToProject;
    Property Generator : TOpenAPICodeGen Read FGenerator Write FGenerator;
  end;


implementation

{$R *.lfm}

{ TGeneratorSettingsFrame }


procedure TGeneratorSettingsFrame.ShowSettings;

begin
  With Generator do
    begin
    cbGenServer.Checked:=GenerateServer;
    cbGenClient.Checked:=GenerateClient;
    CBAbstractCalls.Checked:=AbstractServiceCalls;
    CBSkipImplementation.Checked:=SkipServerServiceImplementationModule;
    CBAsyncService.Checked:=AsyncService;
    CheckAsync;
    CBDelphiCode.checked:=DelphiCode;
    CBVerboseHeader.Checked:=VerboseHeader;
    cbCancelRequest.Checked:=not ParentHasCancelRequest;
    CBEnums.Checked:=UseEnums;
    edtClientServiceImplementationUnit.Text:=ClientServiceImplementationUnit;
    edtClientServiceInterfaceUnit.Text:=ClientServiceInterfaceUnit;
    edtClientServiceParentClass.Text:=ClientServiceParentClass;
    edtClientServiceParentUnit.Text:=ClientServiceParentUnit;
    edtServerHandlerUnitName.Text:=ServerServiceInterfaceUnit;
    edtServerImplementationUnitName.Text:=ServerServiceImplementationUnit;
    edtServerServiceParentClass.Text:=ServerServiceParentClass;
    edtServerServiceParentUnit.Text:=ServerServiceParentUnit;
    edtServiceNameSuffix.Text:=ServiceNameSuffix;
    edtServiceNamePrefix.Text:=ServiceNamePrefix;
    edtSerializeUnit.Text:=SerializeUnit;
    edtDtoUnit.Text:=DtoUnit;
    edtUnitExtension.Text:=UnitExtension;
    edtUnitSuffix.Text:=UnitSuffix;
    cbGenerateServerProxyModule.Checked:=GenerateServerProxyModule;
    edtServerProxyModule.Text:=ServerProxyModuleName;
    edtServerProxyUnit.Text:=ServerProxyUnit;
    cbProxyModuleFormFile.Checked:=ServerProxyFormFile;
    CheckProxyModule;
    end;
end;

procedure TGeneratorSettingsFrame.HideAdditionalControls(ShowClient: Boolean; ShowServer: Boolean);
begin
  PCSettings.AnchorSideTop.Control:=edtFile;
  CBGenClient.Visible:=False;
  CBGenServer.Visible:=False;
  cbOpenFiles.Visible:=False;
  cbAddToProject.Visible:=False;
  if not ShowClient then
    begin
    PCSettings.ActivePage:=TSServer;
    TSClient.TabVisible:=False;
    end;
  if Not ShowServer then
    begin
    PCSettings.ActivePage:=TSClient;
    TSServer.TabVisible:=False;
    end;
end;

function TGeneratorSettingsFrame.Modified: Boolean;

begin
  Result:=False;
  With Generator do
    begin
    Result:=Result or (CBAbstractCalls.Checked<>AbstractServiceCalls);
    Result:=Result or (CBSkipImplementation.Checked<>SkipServerServiceImplementationModule);
    Result:=Result or (CBAsyncService.Checked<>AsyncService);
    Result:=Result or (CBDelphiCode.checked<>DelphiCode);
    Result:=Result or (CBVerboseHeader.Checked<>VerboseHeader);
    Result:=Result or (cbCancelRequest.Checked<>not ParentHasCancelRequest);
    Result:=Result or (CBEnums.Checked<>UseEnums);
    Result:=Result or (edtClientServiceImplementationUnit.Text<>ClientServiceImplementationUnit);
    Result:=Result or (edtClientServiceInterfaceUnit.Text<>ClientServiceInterfaceUnit);
    Result:=Result or (edtClientServiceParentClass.Text<>ClientServiceParentClass);
    Result:=Result or (edtClientServiceParentUnit.Text<>ClientServiceParentUnit);
    Result:=Result or (edtServerHandlerUnitName.Text<>ServerServiceInterfaceUnit);
    Result:=Result or (edtServerImplementationUnitName.Text<>ServerServiceImplementationUnit);
    Result:=Result or (edtServerServiceParentClass.Text<>ServerServiceParentClass);
    Result:=Result or (edtServerServiceParentUnit.Text<>ServerServiceParentUnit);
    Result:=Result or (edtServiceNameSuffix.Text<>ServiceNameSuffix);
    Result:=Result or (edtServiceNamePrefix.Text<>ServiceNamePrefix);
    Result:=Result or (edtSerializeUnit.Text<>SerializeUnit);
    Result:=Result or (edtDtoUnit.Text<>DtoUnit);
    Result:=Result or (edtUnitExtension.Text<>UnitExtension);
    Result:=Result or (edtUnitSuffix.Text<>UnitSuffix);
    Result:=Result or (cbGenerateServerProxyModule.Checked<>GenerateServerProxyModule);
    if GenerateServerProxyModule then
       begin
       Result:=Result or (edtServerProxyModule.Text<>ServerProxyModuleName);
       Result:=Result or (ServerProxyUnit<>edtServerProxyUnit.Text);
       Result:=Result or (ServerProxyFormFile<>cbProxyModuleFormFile.Checked);
       end;
    end;
end;

procedure TGeneratorSettingsFrame.CheckAsync;

begin
  cbCancelRequest.Enabled:=CBAsyncService.Checked;
end;

function TGeneratorSettingsFrame.GetAddToProject: Boolean;
begin
  Result:=cbAddToProject.Checked;
end;

function TGeneratorSettingsFrame.GetOpenAPIFileName: String;
begin
  Result:=edtFile.FileName;
end;

function TGeneratorSettingsFrame.GetOpenGeneratedFiles: Boolean;
begin
  result:=cbOpenFiles.Checked;
end;

procedure TGeneratorSettingsFrame.CheckAbstract;

begin
  CBSkipImplementation.Enabled:=CBAbstractCalls.Checked;
end;

procedure TGeneratorSettingsFrame.HandleSyncCheck(Sender: TObject);
begin
  CheckAsync
end;

procedure TGeneratorSettingsFrame.TSGeneralContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin

end;

procedure TGeneratorSettingsFrame.HandleAbstract(Sender: TObject);
begin
  CheckAbstract;
end;

procedure TGeneratorSettingsFrame.LoadFileToEditor(aEditor : TValueListEditor; const aFilename,aDescription : String);

begin
  if not FileExists(aFileName) then
    Raise EInOutError.CreateFmt('Cannot load %s: file %s does not exist.',[aDescription,aFileName]);
  aEditor.Strings.LoadFromFile(aFileName);
end;

procedure TGeneratorSettingsFrame.SetAddToProject(AValue: Boolean);
begin
  cbAddToProject.Checked:=aValue;
end;

procedure TGeneratorSettingsFrame.SetOpenAPIFileName(AValue: String);
begin
  edtFile.FileName:=aValue;
end;

procedure TGeneratorSettingsFrame.SetOpenGeneratedFiles(AValue: Boolean);
begin
  cbOpenFiles.Checked:=aValue;
end;

procedure TGeneratorSettingsFrame.Clear;
begin
  edtFile.FileName:='';
  edtUUIDMap.FileName:='';
  edtServiceMapFile.FileName:='';
  edtClientServiceImplementationUnit.Text:='';
  edtClientServiceInterfaceUnit.Text:='';
  edtClientServiceParentClass.Text:='';
  edtClientServiceParentUnit.Text:='';
  edtServerHandlerUnitName.Text:='';
  edtServerImplementationUnitName.Text:='';
  edtServerServiceParentClass.Text:='';
  edtServerServiceParentUnit.Text:='';
  edtServerProxyModule.Text:='';
  edtServiceNameSuffix.Text:='';
  edtServiceNamePrefix.Text:='';
  edtSerializeUnit.Text:='';
  edtDtoUnit.Text:='';
  edtUnitExtension.Text:='';
  edtUnitSuffix.Text:='';
end;

procedure TGeneratorSettingsFrame.InitFileNameEdits(const aBaseDir: string);
begin
  edtFile.InitialDir:=aBaseDir;
  edtUUIDMap.InitialDir:=aBaseDir;
  edtServiceMapFile.InitialDir:=aBaseDir;
end;

procedure TGeneratorSettingsFrame.btnLoadUUIDMapClick(Sender: TObject);
begin
  LoadFileToEditor(VLEUUIDMap,edtUUIDMap.FileName,'GUID map');
end;

procedure TGeneratorSettingsFrame.cbGenerateServerProxyModuleChange(Sender: TObject);
begin
  CheckProxyModule;
end;

procedure TGeneratorSettingsFrame.CheckProxyModule;

begin
  edtServerProxyModule.Enabled:=cbGenerateServerProxyModule.Checked;
  if not edtServerProxyModule.Enabled then
    edtServerProxyModule.Text:='';
  edtServerProxyUnit.Enabled:=cbGenerateServerProxyModule.Checked;
  if not edtServerProxyUnit.Enabled then
    edtServerProxyUnit.Text:='';
  cbProxyModuleFormFile.Enabled:=cbGenerateServerProxyModule.Checked;
  if not cbProxyModuleFormFile.Enabled then
    cbProxyModuleFormFile.Checked:=False;
end;

procedure TGeneratorSettingsFrame.btnLoadUUIDMap1Click(Sender: TObject);
begin
  LoadFileToEditor(VLEServiceMap,edtServiceMapFile.FileName,'service map');
end;

procedure TGeneratorSettingsFrame.SaveSettings;
begin
  With Generator do
    begin
    GenerateServer:=cbGenServer.Checked;
    GenerateClient:=cbGenClient.Checked;
    AbstractServiceCalls:=CBAbstractCalls.Checked;
    SkipServerServiceImplementationModule:=CBSkipImplementation.Checked;
    AsyncService:=CBAsyncService.Checked;
    DelphiCode:=CBDelphiCode.checked;
    VerboseHeader:=CBVerboseHeader.Checked;
    ParentHasCancelRequest:=not cbCancelRequest.Checked;
    UseEnums:=CBEnums.Checked;
    ClientServiceImplementationUnit:=edtClientServiceImplementationUnit.Text;
    ClientServiceInterfaceUnit:=edtClientServiceInterfaceUnit.Text;
    ClientServiceParentClass:=edtClientServiceParentClass.Text;
    ClientServiceParentUnit:=edtClientServiceParentUnit.Text;
    ServerServiceInterfaceUnit:=edtServerHandlerUnitName.Text;
    ServerServiceImplementationUnit:=edtServerImplementationUnitName.Text;
    ServerServiceParentClass:=edtServerServiceParentClass.Text;
    ServerServiceParentUnit:=edtServerServiceParentUnit.Text;
    ServiceNameSuffix:=edtServiceNameSuffix.Text;
    ServiceNamePrefix:=edtServiceNamePrefix.Text;
    SerializeUnit:=edtSerializeUnit.Text;
    DtoUnit:=edtDtoUnit.Text;
    UnitExtension:=edtUnitExtension.Text;
    UnitSuffix:=edtUnitSuffix.Text;
    GenerateServerProxyModule:=cbGenerateServerProxyModule.Checked;
    ServerProxyModuleName:=edtServerProxyModule.Text;
    ServerProxyUnit:=edtServerProxyUnit.Text;
    ServerProxyFormFile:=cbProxyModuleFormFile.Checked;
  end;
end;

end.

