unit reglazopenapi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, system.uitypes, fpopenapi.codegen, lazopenapictrl, LazIDEIntf, IDECommands, ProjectIntf;

Type
  { TOpenAPIProject }

  TOpenAPIProject = class(TProjectDescriptor)
  private
    FBaseDir: String;
    FGenerator: TLazOpenAPICodeGen;
    FHTTPPort: Word;
    FOpenAPIFileName : string;
    FUnitsBaseName : string;
    FHTTPThreaded : Boolean;
    FClientProgramType : TIDEProjectType;
    FServerProgramType : TIDEProjectType;
    procedure GenerateAPIRoutesRegistration(aSrc: TStrings);
    function GetBaseFileName: string;
  protected
    function DefaultBaseURL : String; virtual;
    function GetAllowedTypes : TOpenAPIProjectTypes; virtual; abstract;
    function CreateProjectSource(const aFileName : string): string; virtual; abstract;
    procedure GenerateServerCmdLineProjectSource(aSrc: TStrings);
    procedure GenerateClientCmdLineProjectSource(aSrc: TStrings);
    procedure GenerateHTTPServerProjectSource(aSrc: TStrings);
    procedure GenerateGUIProjectSource(aType : TOpenAPIProjectType; aSrc : TStrings);
    function ShowWizard(aTypes : TOpenAPIProjectTypes) : boolean;
    function EnsureFileSaved(const aFilename : String) : Boolean;
  Public
    constructor Create; override;
    destructor destroy; override;
    procedure Clear; virtual;
    function GetUnitNames(aType: TOpenAPIProjectType; forProject: Boolean): String;
    function DoInitDescriptor: TModalResult; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
    Property Generator : TLazOpenAPICodeGen read FGenerator;
    Property OpenAPIFileName : string Read FOpenAPIFileName;
    Property ClientProgramType : TIDEProjectType Read FClientProgramType;
    Property ServerProgramType : TIDEProjectType Read FServerProgramType;
    Property BaseDir : string Read FBaseDir;
    property UnitsBaseName : string read FUnitsBaseName;
    Property HTTPPort : Word Read FHTTPPort;
    Property HTTPThreaded : Boolean Read FHTTPThreaded;
  end;

  { TOpenAPIFormDescriptor }

  TOpenAPIFormDescriptor = class (TFileDescPascalUnitWithResource)
  private
    FProject: TOpenAPIProject;
  Public
     constructor Create(aProject : TOpenAPIProject);
     function GetInterfaceUsesSection: string; override;
     procedure AddClassDeclarations(aSrc : TStrings); virtual; abstract;
     function ProjectType : TOpenAPIProjectType;  virtual; abstract;
     function GetInterfaceSource(const Filename, SourceName, ResourceName: string): string; override;
     Property Project : TOpenAPIProject Read FProject;
  end;

  { TClientOpenAPIFormDescriptor }

  TClientOpenAPIFormDescriptor = class (TOpenAPIFormDescriptor)
    function GetResourceSource(const {%H-}ResourceName: string): string; override;
    function GetInterfaceUsesSection: string; override;
    function GetImplementationSource(const Filename, {%H-}SourceName, {%H-}ResourceName: string): string; override;
    function ProjectType : TOpenAPIProjectType; override;
    procedure AddClassDeclarations(aSrc : TStrings); override;
  end;

  { TServerOpenAPIFormDescriptor }

  TServerOpenAPIFormDescriptor = class (TOpenAPIFormDescriptor)
    function GetResourceSource(const {%H-}ResourceName: string): string; override;
    function GetInterfaceUsesSection: string; override;
    function GetImplementationSource(const Filename, {%H-}SourceName, {%H-}ResourceName: string): string; override;
    function ProjectType : TOpenAPIProjectType; override;
    procedure AddClassDeclarations(aSrc : TStrings); override;
  end;


  { TProjectOpenAPIClient }

  TProjectOpenAPIClient = Class (TOpenAPIProject)
  public
    constructor Create; override;
    function CreateProjectSource(const aFileName : string) : string; override;
    function GetAllowedTypes : TOpenAPIProjectTypes; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
  end;

  { TProjectOpenAPIServer }

  TProjectOpenAPIServer = Class (TOpenAPIProject)
  public
    constructor Create; override;
    function CreateProjectSource(const aFileName : string) : string; override;
    function GetAllowedTypes : TOpenAPIProjectTypes; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
  end;

  { TProjectOpenAPIClientAndServer }

  TProjectOpenAPIClientAndServer = Class (TOpenAPIProject)
  public
    constructor Create; override;
    function CreateProjectSource(const aFileName : string) : string; override;
    function GetAllowedTypes : TOpenAPIProjectTypes; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
  end;

procedure register;


implementation

uses
  CodeToolManager,
  CodeCache,
  MenuIntf,
  IDEMsgIntf,
  IDEExternToolIntf,
  IDEOptEditorIntf,
  IDEOptionsIntf,
  lazopenapistr,
  frmopenapiproject,
  frmopenapiwizard,
  fraopenapiprojectsettings,
  forms,
  controls;

Const
  OpenAPIOptionsIndex = ProjectOptionsMisc + 200;
  Bools : Array[Boolean] of string = ('False','True');

procedure ShowOpenAPIWizard(Sender: TObject);

var
  opts : TOpenFlags;

var
  frm : TOpenapiWizardForm;
  lGenerator : TLazOpenAPICodeGen;

begin
  opts:=[ofOnlyIfExists, ofRevert, ofAddToRecent, ofRegularFile];

  frm:=Nil;
  lGenerator:=TLazOpenAPICodeGen.Create(Application);
  try
    frm:=TOpenapiWizardForm.Create(Application);
    frm.InitFileNameEdits(ExtractFilePath(lazarusIDE.ActiveProject.ProjectInfoFile));
    frm.Generator:=lGenerator;
    if frm.ShowModal=mrOK then
      begin
      if frm.AddToProject then
        Include(opts,ofAddToProject);
      OpenAPIHandler.GenerateFiles(frm.OpenAPIFileName,frm.BaseFileName,lGenerator);
      if frm.OpenGeneratedFiles then
        begin
        LazarusIDE.DoOpenEditorFile(lGenerator.ResolveUnitName(ukDto),-1,-1,opts);
        LazarusIDE.DoOpenEditorFile(lGenerator.ResolveUnitName(ukSerialize),-1,-1,opts);
        if lGenerator.GenerateClient then
          begin
          LazarusIDE.DoOpenEditorFile(lGenerator.ResolveUnitName(ukClientServiceIntf),-1,-1,opts);
          LazarusIDE.DoOpenEditorFile(lGenerator.ResolveUnitName(ukClientServiceImpl),-1,-1,opts);
          end;
        if lGenerator.GenerateServer then
          begin
          LazarusIDE.DoOpenEditorFile(lGenerator.ResolveUnitName(ukServerServiceHandler),-1,-1,opts);
          LazarusIDE.DoOpenEditorFile(lGenerator.ResolveUnitName(ukServerServiceImpl),-1,-1,opts);
          end;
        end;
      end;
  finally
    lGenerator.Free;
    frm.Free;
  end;
end;


procedure register;

begin
  OpenAPIHandler:=TOpenAPIHandler.Create;
  RegisterProjectDescriptor(TProjectOpenAPIClient.Create);
  RegisterProjectDescriptor(TProjectOpenAPIServer.Create);
  // Todo
  // RegisterProjectDescriptor(TProjectOpenAPIClientAndServer.Create);
  // search shortcut category
  OpenAPIHandler.CmdToolsMenu:=IDECommandList.FindCategoryByName(CommandCategoryToolMenuName);
  // register shortcut
  OpenAPIHandler.OpenAPIWizardCommand:=RegisterIDECommand(OpenAPIHandler.CmdToolsMenu,
    SCMDOpenAPIWizard,
    SCMDOpenAPIWizardCaption,
    CleanIDEShortCut,
    CleanIDEShortCut, nil, @ShowOpenAPIWizard);
  // register menu item in View menu
  RegisterIDEMenuCommand(itmCustomTools,
    SCMDOpenAPIWizard,
    SCMDOpenAPIWizardCaption, nil, nil, OpenAPIHandler.OpenAPIWizardCommand);
  // Add refresh to popup menu
  OpenAPIHandler.RefreshMenu:=RegisterIDEMenuCommand(ProjInspMenuSectionFiles,
      SProjectRefreshOpenAPIName,SRegenerateOpenAPI,@OpenAPIHandler.HandleRefreshOpenAPI);
  ProjectInspectorItemsMenuRoot.AddHandlerOnShow(@OpenAPIHandler.HandleProjectInspectorPopup);

  RegisterIDEOptionsEditor(GroupProject,TLazOpenAPIProjectOptions, OpenAPIOptionsIndex);
end;


{ TOpenAPIProject }

destructor TOpenAPIProject.destroy;
begin
  FreeAndNil(FGenerator);
  Inherited;
end;

function TOpenAPIProject.GetBaseFileName : string;

var
  lBaseFileName : String;

begin
  lBaseFileName:=IncludeTrailingPathDelimiter(BaseDir);
  if GetAllowedTypes<>[optClient,optServer] then
    lBaseFileName:=lBaseFileName+UnitsBaseName
  else
    lBaseFileName:=IncludeTrailingPathDelimiter(lBaseFileName+'common')+UnitsBaseName;
  Result:=lBaseFileName;
end;

function TOpenAPIProject.DefaultBaseURL: String;
begin
  Result:='http://localhost:8080/REST/';
end;

function TOpenAPIProject.DoInitDescriptor: TModalResult;

  function TryCreateDir(const aDir : string) : Boolean;
  var
    lMsg : string;
  begin
    Result:=ForceDirectories(aDir);
    lMsg:=Format(SErrFailedToCreateProjectDir,[aDir]);
    AddIDEMessage(mluFatal,lMsg,'',0,0,SOpenAPICodeGenerator);
  end;

var
  lMsg : String;
  lBaseFileName : String;

begin
  if Not ShowWizard(GetAllowedTypes) then
    Exit(mrCancel);
  If not TryCreateDir(BaseDir) then
    Exit(mrAbort);
  lBaseFileName:=GetBaseFileName;
  if GetAllowedTypes=[optClient,optServer] then
    begin
    If not TryCreateDir(BaseDir+'client') then
      Exit(mrAbort);
    If not TryCreateDir(BaseDir+'server') then
      Exit(mrAbort);
    If not TryCreateDir(BaseDir+'common') then
      Exit(mrAbort);
    lBaseFileName:=IncludeTrailingPathDelimiter(BaseDir)+UnitsBaseName
    end
  else If not TryCreateDir(BaseDir) then
    Exit(mrAbort);
  try
    Generator.GenerateClient:=optClient in GetAllowedTypes;
    Generator.GenerateServer:=optServer in GetAllowedTypes;
    OpenAPIHandler.GenerateFiles(OpenAPIFileName,lBaseFileName,Generator);
    Result:=mrOK;
  except
    on E : Exception do
      begin
      lMsg:=Format(SErrFailedToGenerateAPI,[E.ClassName,E.Message]);
      AddIDEMessage(mluFatal,lMsg,'',0,0,SOpenAPICodeGenerator);
      Result:=mrAbort;
      end;
  end;
end;


function TOpenAPIProject.InitProject(AProject: TLazProject): TModalResult;
var
  lFileName,lConfig : string;
begin
  if optClient in GetAllowedTypes then
    lFileName:=BaseDir+'client.lpi'
  else
    lFileName:=BaseDir+'server.lpi';
  AProject.ProjectInfoFile:=lFileName;
  AProject.LazCompilerOptions.OtherUnitFiles:=BaseDir;
  lConfig:=ExtractFilePath(GetBaseFileName)+SConfigFileName;
  FGenerator.SaveConfig(lConfig);
  OpenAPIHandler.SetProjectData(aProject,lConfig,OpenApiFileName,GetBaseFileName);
  Result:=mrOK;
end;

function TOpenAPIProject.GetUnitNames(aType: TOpenAPIProjectType; forProject: Boolean): String;

  procedure AddToResult(aUnit : string);

  begin
    Result:=Result+', '+aUnit;
  end;

begin
  Result:=Generator.ResolveUnitName(ukDto,False);
  AddToResult(Generator.ResolveUnitName(ukSerialize,False));
  if aType=optClient then
    begin
    AddToResult(Generator.ResolveUnitName(ukClientServiceIntf,False));
    AddToResult(Generator.ResolveUnitName(ukClientServiceImpl,False));
    if Generator.GenerateServerProxyModule then
      AddToResult(Generator.ResolveUnitName(ukServerProxy,False));
    if ForProject then
      begin
      AddToResult('fpwebclient');
      AddToResult('fphttpwebclient');
      end;
    end
  else
    begin
    AddToResult(Generator.ResolveUnitName(ukServerServiceHandler,False));
    AddToResult(Generator.ResolveUnitName(ukServerServiceImpl,False));
    end;
end;

procedure TOpenAPIProject.GenerateAPIRoutesRegistration(aSrc: TStrings);

var
  aModule : string;

begin
  // Todo: add possibility of creating a form.
  For aModule in Generator.ServerServiceModules do
    aSrc.Add('  %s.RegisterAPIRoutes(BaseURL,%s);',[aModule,'False']);
end;

procedure TOpenAPIProject.GenerateHTTPServerProjectSource(aSrc: TStrings);

  procedure AddLn(const fmt : string; const aArgs : Array of const);
  begin
    aSrc.Add(fmt,aArgs);
  end;

  procedure AddLn(const aLine : string);
  begin
    aSrc.Add(aLine);
  end;

begin
  Addln('uses');
  Addln('  {$IFDEF UNIX}');
  Addln('  cthreads, cwstring,');
  Addln('  {$ENDIF}');
  Addln('  {$IFDEF HASAMIGA}');
  Addln('  athreads,');
  Addln('  {$ENDIF}');
  Addln('  Classes, fpHTTPApp, %s;',[GetUnitNames(optServer,True)]);
  Addln('');
  Addln('const');
  Addln('  BaseURL = ''REST/'';');
  Addln('');
  Addln('begin');
  GenerateAPIRoutesRegistration(aSrc);
  Addln('  Application.Title:=''OpenAPI server project'';');
  Addln('  Application.Port:=%d;',[HTTPPort]);
  Addln('  Application.Threaded:=%s;',[Bools[HTTPThreaded]]);
  Addln('  // Uncomment this if you wish to use SSL');
  Addln('  // Application.UseSSL:=True;');
  Addln('  Application.Initialize;');
  Addln('  Application.Run;');
  Addln('end.');
end;

procedure TOpenAPIProject.GenerateServerCmdLineProjectSource(aSrc: TStrings);

  procedure AddLn(const fmt : string; const aArgs : Array of const);
  begin
    aSrc.Add(fmt,aArgs);
  end;

  procedure AddLn(const aLine : string);
  begin
    aSrc.Add(aLine);
  end;

begin
  Addln('uses');
  Addln('  {$IFDEF UNIX}');
  Addln('  cthreads, cwstring,');
  Addln('  {$ENDIF}');
  Addln('  {$IFDEF HASAMIGA}');
  Addln('  athreads,');
  Addln('  {$ENDIF}');
  Addln('  Classes, CustApp, fphttpserver, httproute, %s;',[GetUnitNames(optServer,True)]);
  Addln('');
  Addln('Type');
  Addln('  TApplication = Class(TCustomApplication)');
  Addln('    FServer : TFPHttpServer;');
  Addln('    procedure HandleRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);');
  Addln('  protected');
  Addln('    procedure DoRun; override;');
  Addln('  public');
  Addln('    constructor Create(aOwner : TComponent); override;');
  Addln('    Property Server : TFPHttpServer Read FServer;');
  Addln('  end;');
  Addln('');
  Addln('constructor TApplication.Create(aOwner : TComponent);');
  Addln('');
  Addln('const');
  Addln('  BaseURL = ''/REST'';');
  Addln('');
  Addln('begin');
  Addln('  inherited;');
  GenerateAPIRoutesRegistration(aSrc);
  Addln('  FServer:=TFPHttpServer.Create(Self);');
  Addln('  FServer.Port:=%d;',[HTTPPort]);
  Addln('  FServer.OnRequest:=@HandleRequest;');
  Addln('  FServer.Threaded:=%s;',[Bools[HTTPThreaded]]);
  Addln('end;');
  Addln('');
  Addln('procedure TApplication.DoRun; ');
  Addln('');
  Addln('begin');
  Addln('  FServer.Active:=True;');
  if not HTTPThreaded then
    Addln('  // Note: code here will only be executed after the server stops!');
  Addln('end;');
  Addln('');
  Addln('procedure TApplication.HandleRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);');
  Addln('begin');
  Addln('  HTTPRouter.RouteRequest(aRequest,aResponse);');
  Addln('end;');
  Addln('');
  Addln('var');
  Addln('  Application : TApplication;');
  Addln('');
  Addln('begin');
  Addln('  Application:=TApplication.Create(Nil);');
  Addln('  Application.Initialize;');
  Addln('  Application.Run;');
  Addln('  Application.Free;');
  Addln('end.');
end;

procedure TOpenAPIProject.GenerateClientCmdLineProjectSource(aSrc: TStrings);

  procedure AddLn(const fmt : string; const aArgs : Array of const);
  begin
    aSrc.Add(fmt,aArgs);
  end;

  procedure AddLn(const aLine : string);
  begin
    aSrc.Add(aLine);
  end;

var
  S : String;

begin
  Addln('uses');
  Addln('  {$IFDEF UNIX}');
  Addln('  cthreads, cwstring,');
  Addln('  {$ENDIF}');
  Addln('  {$IFDEF HASAMIGA}');
  Addln('  athreads,');
  Addln('  {$ENDIF}');
  Addln('  Classes, CustApp, %s;',[GetUnitNames(optClient,True)]);
  Addln('');
  Addln('Type');
  Addln('  TApplication = Class(TCustomApplication)');
  if Generator.GenerateServerProxyModule then
    AddLn('    FProxy : %s;',[Generator.ServerProxyModuleName]);
  Addln('  protected');
  Addln('    procedure DoRun; override;');
  Addln('  public');
  Addln('    constructor Create(aOwner : TComponent); override;');
  if Generator.GenerateServerProxyModule then
    AddLn('    Property Proxy : %s read FProxy;',[Generator.ServerProxyModuleName]);
  Addln('  end;');
  Addln('');
  Addln('constructor TApplication.Create(aOwner : TComponent);');
  Addln('');
  Addln('begin');
  Addln('  inherited;');
  Addln('  DefaultWebClientClass:=TFPHTTPWebClient;');
  if Generator.GenerateServerProxyModule then
    Addln('  FProxy:=%s.Create(Self);',[Generator.ServerProxyModuleName]);
  Addln('end;');
  Addln('');
  Addln('procedure TApplication.DoRun; ');
  Addln('');
  Addln('begin');
  if Generator.GenerateServerProxyModule then
    begin
    Addln('  // here you can configure the proxy.');
    Addln('  // after it is configured, you can make calls to the service.');
    Addln('  // Typically you will want to set the base URL of the service.');
    S:=DefaultBaseURL;
    if Length(Generator.ServerURLs)>0 then
      S:=Generator.ServerURLs[0];
    Addln('  Proxy.BaseURL:=''%s'';',[S]);
    end;
  Addln('end;');
  Addln('');
  Addln('var');
  Addln('  Application : TApplication;');
  Addln('');
  Addln('begin');
  Addln('  Application:=TApplication.Create(Nil);');
  Addln('  Application.Initialize;');
  Addln('  Application.Run;');
  Addln('  Application.Free;');
  Addln('end.');
end;

procedure TOpenAPIProject.GenerateGUIProjectSource(aType: TOpenAPIProjectType; aSrc: TStrings);

  procedure AddLn(const fmt : string; const aArgs : Array of const);
  begin
    aSrc.Add(fmt,aArgs);
  end;

  procedure AddLn(const aLine : string);
  begin
    aSrc.Add(aLine);
  end;

var
  lModule,lVar : String;

begin
  Addln('uses');
  Addln('  {$IFDEF UNIX}');
  Addln('  cthreads, cwstring,');
  Addln('  {$ENDIF}');
  Addln('  {$IFDEF HASAMIGA}');
  Addln('  athreads,');
  Addln('  {$ENDIF}');
  AddLn('  Interfaces,');
  AddLn('  Forms,');
  AddLn('  %s;',[GetUnitNames(aType,True)]);
  Addln('');
  if (aType=optServer) then
    begin
    Addln('const');
    Addln('  BaseURL = ''/REST/'';');
    Addln('');
    end;
  Addln('begin');
  if aType=optClient then
    Addln('  DefaultWebClientClass:=TFPHTTPWebClient;')
  else
    GenerateAPIRoutesRegistration(aSrc);
  Addln('  Application.Initialize;');
  if (aType=optClient) and Generator.ServerProxyFormFile then
    begin
    lModule:=Generator.ServerProxyModuleName;
    lVar:=Copy(lModule,2,Length(lModule)-1);
    Addln('  Application.CreateForm(%s,%s);',[lModule,lVar]);
    end;
  Addln('  Application.Run;');
  Addln('  Application.Free;');
  Addln('end.');
end;

constructor TOpenAPIProject.Create;
begin
  Inherited;
  Clear;
end;

procedure TOpenAPIProject.Clear;

begin
  FreeAndNil(FGenerator);
  FGenerator:=TLazOpenAPICodeGen.Create(Nil);
  FOpenAPIFileName:='';
  FBaseDir:='';
  FUnitsBaseName:='';
  FClientProgramType:=Default(TIDEProjectType);
  FServerProgramType:=Default(TIDEProjectType);
end;

function TOpenAPIProject.ShowWizard(aTypes: TOpenAPIProjectTypes): boolean;

var
  Frm : TOpenAPIProjectForm;

begin
//  TOpenAPIProjectType = (optClient,optServer);
  Frm:=TOpenAPIProjectForm.Create(Application);
  try
    frm.AllowedTypes:=aTypes;
    frm.Generator:=Self.Generator;
    frm.BaseDir:=LazarusIDE.GetTestBuildDirectory;
    frm.OpenAPIFileName:='/home/michael/fpc/packages/fcl-openapi/examples/simpleservice.json';
    Result:=frm.ShowModal=mrOK;
    if Result then
      begin
      FOpenAPIFileName:=frm.OpenAPIFileName;
      FClientProgramType:=Frm.ClientProjectType;
      FServerProgramType:=Frm.ServerProjectType;
      FBaseDir:=IncludeTrailingPathDelimiter(Frm.BaseDir);
      FUnitsBaseName:=Frm.UnitsBaseName;
      FHTTPPort:=frm.HTTPPort;
      FHTTPThreaded:=frm.ThreadedServer;
      end;
  finally
    Frm.Free;
  end;
end;

function TOpenAPIProject.EnsureFileSaved(const aFilename: String): Boolean;

var
  Code: TCodeBuffer;
begin
  Code:=CodeToolBoss.FindFile(aFilename);
  Result:=Code<>nil;
  if not Result then
    AddIDEMessage(mluFatal,'File missing in codetools: "'+aFilename+'"','',0,0,SOpenAPICodeGenerator)
  else
    begin
    Result:=Code.Save;
    if not Result then
      AddIDEMessage(mluFatal,'Unable to write file "'+aFilename+'"','',0,0,SOpenAPICodeGenerator);
    end;
end;

function TOpenAPIProject.CreateStartFiles(AProject: TLazProject): TModalResult;

  Procedure AddFileToProject(aFileName : string);

  var
    lFile : TLazProjectFile;

  begin
    lFile:=aProject.CreateProjectFile(aFileName);
    lFile.IsPartOfProject:=True;
    aProject.AddFile(lFile,False);
  end;

var
  aFile : TLazProjectFile;
  lFileName : string;
  lForm : TOpenAPIFormDescriptor;

begin
  lFileName:=ChangeFileExt(AProject.ProjectInfoFile,'.lpr');
  aFile:=aProject.CreateProjectFile(lFileName);
  aFile.IsPartOfProject:=True;
  aProject.AddFile(aFile,False);
  aProject.MainFileID:=0;
  aProject.MainFile.SetSourceText(CreateProjectSource(lFileName),true);
  EnsureFileSaved(lFileName);
  if LazarusIDE.DoSaveProject([sfQuietUnitCheck])<>mrOk then exit;
  AddFileToProject(Generator.ResolveUnitName(ukDto));
  AddFileToProject(Generator.ResolveUnitName(ukSerialize));
  if optClient in GetAllowedTypes then
    begin
    AddFileToProject(Generator.ResolveUnitName(ukClientServiceIntf));
    AddFileToProject(Generator.ResolveUnitName(ukClientServiceImpl));
    if Generator.GenerateServerProxyModule then
      AddFileToProject(Generator.ResolveUnitName(ukServerProxy));
    if (ClientProgramType=iptGUI) then
      begin
      lForm:=TClientOpenAPIFormDescriptor.Create(Self);
      try
      LazarusIDE.DoNewEditorFile(lForm,'','',
                                 [nfIsPartOfProject,nfOpenInEditor,nfCreateDefaultSrc]);
      finally
        lForm.Free;
      end;
      end;
    end
  else
    begin
    AddFileToProject(Generator.ResolveUnitName(ukServerServiceHandler));
    if not Generator.SkipServerServiceImplementationModule then
      AddFileToProject(Generator.ResolveUnitName(ukServerServiceImpl));
    if (ServerProgramType=iptGUI) then
      begin
      lForm:=TServerOpenAPIFormDescriptor.Create(Self);
      try
      LazarusIDE.DoNewEditorFile(lForm,'','',
                                 [nfIsPartOfProject,nfOpenInEditor,nfCreateDefaultSrc]);
      finally
        lForm.Free;
      end;
      end;
    end;
  Result:=mrOK;
end;

{ TOpenAPIFormDescriptor }

constructor TOpenAPIFormDescriptor.Create(aProject: TOpenAPIProject);
begin
  Inherited Create;
  FProject:=aProject;
  ResourceClass:=TForm;
  UseCreateFormStatements:=True;
end;

function TOpenAPIFormDescriptor.GetInterfaceUsesSection: string;
begin
  Result:=inherited GetInterfaceUsesSection;
  Result:=Result+', '+Project.GetUnitNames(ProjectType,False);
end;

function TOpenAPIFormDescriptor.GetInterfaceSource(const Filename, SourceName, ResourceName: string): string;
var
  Src : TStrings;
begin
  Src:=TStringList.Create;
  try
    With Src do
      begin
      Add('type');
      Add('  T%s = class(%s)',[ResourceName,ResourceClass.ClassName]);
      AddClassDeclarations(Src);
      Add('  end;');
      Add('');
      if DeclareClassVariable then
        begin
        Add('var');
        Add('  %s: T%s;',[ResourceName,ResourceName]);
        Add('');
        end;
      end;
    Result:=Src.Text;
  finally
    Src.Free;
  end;
end;

{ TClientOpenAPIFormDescriptor }

function TClientOpenAPIFormDescriptor.GetResourceSource(const ResourceName: string): string;
var
  Src : TStrings;

begin
  Src:=TStringList.Create;
  try
    With Src do
      begin
      Add('object %s : T%s',[ResourceName,ResourceName]);
      Add('  Left = 320');
      Add('  Height = 480');
      Add('  Top = 320');
      Add('  Width = 640');
      Add('  OnCreate = HandleCreate');
      Add('end');
      end;
    Result:=Src.Text;
  finally
    Src.Free;
  end;
end;

function TClientOpenAPIFormDescriptor.GetInterfaceUsesSection: string;
begin
  Result:=Inherited GetInterfaceUsesSection;
  Result:=Result+', Forms, Dialogs, Controls';
end;

function TClientOpenAPIFormDescriptor.GetImplementationSource(const Filename, SourceName, ResourceName: string): string;
var
  src : TStrings;
  lModule,lURL : string;

begin
  Src:=TStringList.Create;
  try
    With Src do
      begin
      if GetResourceType=rtRes then
        begin
        Add('');
        Add('{$R *.lfm}');
        end;
      Add('');
      Add('procedure T%s.HandleCreate(Sender: TObject);',[ResourceName]);
      Add('begin');
      lModule:=Project.Generator.ServerProxyModuleName;
      lModule:=Copy(lModule,2,Length(lModule)-1);
      if not Project.Generator.ServerProxyFormFile then
        Add('%s:=T%s.Create(Self);',[lModule,lModule]);
      lURL:=Project.DefaultBaseURL;
      if Length(Project.Generator.ServerURLS)>0 then
        lURL:=Project.Generator.ServerURLS[0];
      Add('  %s.BaseURL:=''%s'';',[lModule,lURL]);
      Add('end;');
      Add('');
      end;
    Result:=Src.Text;
  finally
    Src.free;
  end;
  if GetResourceType=rtLRS then
    Result:=Result+inherited GetImplementationSource(Filename, SourceName, ResourceName);
end;

function TClientOpenAPIFormDescriptor.ProjectType: TOpenAPIProjectType;
begin
  Result:=optClient;
end;

procedure TClientOpenAPIFormDescriptor.AddClassDeclarations(aSrc: TStrings);
var
  lModule : String;
begin
  With aSrc do
    begin
    Add('    procedure HandleCreate(Sender : TObject);');
    Add('  Private');
    if not Project.Generator.ServerProxyFormFile then
      begin
      lModule:=Project.Generator.ServerProxyModuleName;
      lModule:=Copy(lModule,2,Length(lModule)-1);
      Add('    %s : T%s;',[lModule,lModule]);
      end;
    Add('');
    Add('  Public');
    Add('');
    end;
end;

{ TServerOpenAPIFormDescriptor }

function TServerOpenAPIFormDescriptor.GetResourceSource(const ResourceName: string): string;
var
  Src : TStrings;

begin
  Src:=TStringList.Create;
  try
    With Src do
      begin
      Add('object %s : T%s',[ResourceName,ResourceName]);
      Add('  Left = 320');
      Add('  Height = 480');
      Add('  Top = 320');
      Add('  Width = 640');
      Add('  object HTTPServer: TFPHTTPServer');
      Add('    Port = %d',[Project.HTTPPort]);
      Add('    Threaded = %s',[Bools[Project.HTTPThreaded]]);
      Add('    OnRequest = HandleRequest');
      Add('    Left = 56');
      Add('    Top = 48');
      Add('  end');
      Add('end');
      end;
    Result:=Src.Text;
  finally
    Src.Free;
  end;
end;

function TServerOpenAPIFormDescriptor.GetInterfaceUsesSection: string;
begin
  Result:=inherited GetInterfaceUsesSection;
  Result:=Result+', Forms, Dialogs, Controls';
end;

function TServerOpenAPIFormDescriptor.GetImplementationSource(const Filename, SourceName, ResourceName: string): string;
var
  src : TStrings;
begin
  Src:=TStringList.Create;
  try
    With Src do
      begin
      if GetResourceType=rtRes then
        begin
        Add('');
        Add('{$R *.lfm}');
        end;
      Add('');
      Add('procedure T%s.HandleRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);',[ResourceName]);
      Add('begin');
      Add('  HTTPRouter.RouteRequest(aRequest, aResponse);');
      Add('end;');
      Add('');
      end;
    Result:=Src.Text;
  finally
    Src.free;
  end;
  if GetResourceType=rtLRS then
    Result:=Result+inherited GetImplementationSource(Filename, SourceName, ResourceName);
end;

function TServerOpenAPIFormDescriptor.ProjectType: TOpenAPIProjectType;
begin
  Result:=optServer;
end;

procedure TServerOpenAPIFormDescriptor.AddClassDeclarations(aSrc: TStrings);
begin
  With aSrc do
    begin
    Add('    HTTPServer : TFPHTTPServer;');
    Add('    procedure HandleRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);');
    Add('  private');
    Add('');
    Add('  public');
    Add('');
    end;
end;

{ TProjectOpenAPIClient }

constructor TProjectOpenAPIClient.Create;
begin
  Inherited;
  Name:='OpenAPIClient';
end;

function TProjectOpenAPIClient.CreateProjectSource(const aFileName : string): string;
var
  Src : TStrings;
begin
  Src:=TStringList.Create;
  try
    Src.Add('program project1;');
    Src.Add('');
    Src.Add('{$mode objfpc}');
    Src.Add('{$h+}');
    Src.Add('');
    Case ClientProgramType of
      iptGUI : GenerateGUIProjectSource(optClient,Src);
      iptCmdLine : GenerateClientCmdLineProjectSource(Src);
    end;
    Src.SaveToFile(aFileName);
    Result:=Src.Text;
  finally
    Src.Free;
  end;
end;

function TProjectOpenAPIClient.GetAllowedTypes: TOpenAPIProjectTypes;
begin
  Result:=[optClient];
end;

function TProjectOpenAPIClient.GetLocalizedName: string;
begin
  Result:=SProjectOpenAPIClient;
end;

function TProjectOpenAPIClient.GetLocalizedDescription: string;
begin
  Result:=SProjectOpenAPIClientDescription;
end;

{ TProjectOpenAPIServer }

constructor TProjectOpenAPIServer.Create;
begin
  inherited Create;
  Name:='OpenAPIServer';
end;

function TProjectOpenAPIServer.CreateProjectSource(const aFileName: string): string;
var
  Src : TStrings;
begin
  Src:=TStringList.Create;
  try
    Src.Add('program project1;');
    Src.Add('');
    Src.Add('{$mode objfpc}');
    Src.Add('{$h+}');
    Src.Add('');
    Case ServerProgramType of
      iptGUI : GenerateGUIProjectSource(optServer,Src);
      iptCmdLine : GenerateServerCmdLineProjectSource(Src);
      iptHTTPServer : GenerateHTTPServerProjectSource(Src);
    end;
    Src.SaveToFile(aFileName);
    Result:=Src.Text;
  finally
    Src.Free;
  end;
end;

function TProjectOpenAPIServer.GetAllowedTypes: TOpenAPIProjectTypes;
begin
  Result:=[optServer];
end;

function TProjectOpenAPIServer.GetLocalizedName: string;
begin
  Result:=SProjectOpenAPIServer;
end;

function TProjectOpenAPIServer.GetLocalizedDescription: string;
begin
  Result:=SProjectOpenAPIServerDescription;
end;

{ TProjectOpenAPIClientAndServer }

constructor TProjectOpenAPIClientAndServer.Create;
begin
  inherited Create;
  Name:='OpenAPIClientAndServer';
end;

function TProjectOpenAPIClientAndServer.CreateProjectSource(const aFileName: string): string;
begin
  Result:='';
end;

function TProjectOpenAPIClientAndServer.GetAllowedTypes: TOpenAPIProjectTypes;
begin
  Result:=[optClient,optServer];
end;

function TProjectOpenAPIClientAndServer.GetLocalizedName: string;
begin
  Result:=SProjectOpenAPIClientServer;
end;

function TProjectOpenAPIClientAndServer.GetLocalizedDescription: string;
begin
  Result:=SProjectOpenAPIClientServerDescription;
end;

end.

