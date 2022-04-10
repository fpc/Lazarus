unit pjscontroller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process,
  // LazUtils
  LazLoggerBase, LazUtilities, FileUtil, LazFileUtils,
  // LCL
  Forms, Controls,
  // IdeIntf
  MacroIntf, MacroDefIntf, ProjectIntf, CompOptsIntf, LazIDEIntf,
  // pas2js
  SimpleWebSrvController, StrPas2JSDesign, PJSDsgnOptions, CodeToolManager,
  CodeCache;

Type

  { TServerInstance }

  TServerInstance = Class(TCollectionItem)
  private
    FlastProject: String;
    FPort: Word;
    FProcess: TProcess;
    FRunError: String;
    FServerName: String;
    FString: String;
    function GetRunning: Boolean;
  Protected
    Property Process : TProcess Read FProcess;
  Public
    Destructor Destroy; override;
    Procedure StartServer;
    Procedure StopServer;
    Property Port : Word Read FPort Write FPort;
    Property BaseDir : String Read FString Write FString;
    Property ServerName : String Read FServerName Write FServerName;
    Property Running : Boolean Read GetRunning;
    Property RunError : String Read FRunError;
    Property LastProject : String Read FlastProject Write Flastproject;
  end;

  { TServerInstanceList }

  TServerInstanceList = Class(TCollection)
  private
    function GetInstance(AIndex : Integer): TServerInstance;
  Public
    Function IndexOfPort(APort: Word) : integer;
    Function FindByPort(Aindex : Integer) : TServerInstance;
    Function AddInstance(aPort : Word; Const ABaseURL, aServerName : String) : TServerInstance;
    Property Instances [AIndex : Integer] : TServerInstance Read GetInstance; default;
  end;

  { TPJSController }

  TPJSController = Class
  Private
    FOnRefresh: TNotifyEvent;
    FServerInstances: TServerInstanceList;
    function GetPas2JSPath(const s: string; const {%H-}Data: PtrInt; var Abort: boolean): string;
    function GetPas2JSWebServerPath(const s: string; const {%H-}Data: PtrInt; var Abort: boolean): string;
    function GetPas2JSWebServerPort(const s: string; const {%H-}Data: PtrInt; var Abort: boolean): string;
    function GetPas2JSBrowser(const s: string; const {%H-}Data: PtrInt; var Abort: boolean): string;
    function GetPas2JSNodeJS(const s: string; const {%H-}Data: PtrInt; var Abort: boolean): string;
    function GetPas2jsProjectURL(const s: string; const {%H-}Data: PtrInt; var Abort: boolean): string;
    function OnProjectBuilding(Sender: TObject): TModalResult;
    function OnRunDebugInit(Sender: TObject; var Handled: boolean
      ): TModalResult;
    function OnRunWithoutDebugInit(Sender: TObject; var Handled: boolean): TModalResult;
    function GetHTMLFilename(aProject: TLazProject; UseTestDir: boolean): string;
    function GetWebDir(aProject: TLazProject): string;
    function RunProject(Sender: TObject; WithDebug: boolean; var Handled: boolean): TModalResult;
    function SaveHTMLFileToTestDir(aProject: TLazProject): boolean;
  Public
    Constructor Create;
    Destructor Destroy; override;
    Class Procedure DoneInstance;
    Class Function instance :  TPJSController;
    Procedure Hook; virtual;
    Procedure UnHook; virtual;
    Procedure RefreshView;
    Property ServerInstances : TServerInstanceList Read FServerInstances;
    Property OnRefresh : TNotifyEvent Read FOnRefresh Write FonRefresh;
  end;

Const
  // Custom settings in .lpi
  PJSProject = 'Pas2JSProject'; // Project is pas2js project
  PJSProjectWebBrowser =  'PasJSWebBrowserProject'; // Web browser project
  PJSProjectServiceWorker =  'PJSProjectServiceWorker'; // Service Worker project
  PJSProjectNodeJS =  'PJSProjectNodeJS'; // NodeJS project
  PJSProjectModule =  'PJSProjectModule'; // Module project
  PJSProjectVSCode =  'PJSProjectVSCode'; // VS Code project
  PJSProjectAtom =  'PJSProjectAtom'; // Atom project
  PJSProjectHTMLFile = 'PasJSHTMLFile';
  PJSIsProjectHTMLFile = 'PasJSIsProjectHTMLFile';
  PJSProjectMaintainHTML = 'MaintainHTML';
  PJSProjectManifestFile = 'PasJSManifestFile';
  PJSProjectCSSFile = 'PasJSCSSFile';
  PJSProjectUseBrowserConsole = 'BrowserConsole';
  PJSProjectRunAtReady = 'RunAtReady';
  PJSProjectPort = 'PasJSPort';
  PJSProjectURL = 'PasJSURL';


implementation

Var
  ctrl : TPJSController;

{ TServerInstanceList }

function TServerInstanceList.GetInstance(AIndex : Integer): TServerInstance;
begin
  Result:=Items[AIndex] as TServerInstance;
end;

function TServerInstanceList.IndexOfPort(APort: Word): integer;
begin
  Result:=Count-1;
  While (Result>=0) and (GetInstance(Result).Port<>APort) do Dec(Result);
end;

function TServerInstanceList.FindByPort(Aindex: Integer): TServerInstance;

Var
  I : Integer;

begin
  I:=IndexOfPort(Aindex);
  If I=-1 then
    Result:=nil
  else
    Result:=GetInstance(I);
end;

function TServerInstanceList.AddInstance(aPort: Word; const ABaseURL,
  aServerName: String): TServerInstance;
begin
  Result:=Add as TServerInstance;
  Result.Port:=aPort;
  Result.BaseDir:=ABaseURL;
  Result.ServerName:=aServerName;
end;

{ TServerInstance }

function TServerInstance.GetRunning: Boolean;
begin
  Result:=Assigned(FProcess);
  if Result then
    Result:=Process.Running;
end;

destructor TServerInstance.Destroy;
begin
  StopServer;
  FreeAndNil(FProcess);
  inherited;
end;

procedure TServerInstance.StartServer;
begin
  if Running then
    exit;
  If not Assigned(FProcess) then
    FProcess:=TProcess.Create(Nil);
  FProcess.Executable:=ServerName;
  FProcess.Parameters.Add('-q');
  FProcess.Parameters.Add('-p');
  FProcess.Parameters.Add(IntToStr(Port));
  FProcess.Parameters.AddStrings(PJSOptions.HTTPServerOpts);
  {$IFDEF WINDOWS}
  FProcess.Options:=[poNoConsole];
  {$ENDIF}
  if ConsoleVerbosity>=0 then
    DebugLN(['Starting server from Directory : ',BaseDir]);
  FProcess.CurrentDirectory:=BaseDir;
  try
    FProcess.Execute;
  except
    On E : Exception do
      begin
      FRunError:=E.Message;
      Raise;
      end;
  end;
  TPJSController.Instance.RefreshView;
end;

procedure TServerInstance.StopServer;
begin
  if Running then
    FProcess.Terminate(0);
  TPJSController.Instance.RefreshView;
end;

class procedure TPJSController.DoneInstance;

begin
  FreeAndNil(Ctrl)
end;

class function TPJSController.instance: TPJSController;

begin
  if ctrl=Nil then
    Ctrl:=TPJSController.Create;
  Result:=Ctrl;
end;

{ TPJSController }

function TPJSController.GetPas2JSPath(const s: string; const Data: PtrInt;
  var Abort: boolean): string;
begin
  Abort:=False;
  if (s<>'') and (ConsoleVerbosity>=0) then
    debugln(['Hint: (lazarus) [TPJSController.GetPas2JSPath] ignoring macro Pas2JS parameter "',s,'"']);
  Result:=PJSOptions.GetParsedCompilerFilename;
  if Result='' then
    Result:='pas2js'; // always return something to get nicer error messages
end;

function TPJSController.GetPas2JSWebServerPath(const s: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Abort:=False;
  if (s<>'') and (ConsoleVerbosity>=0) then
    debugln(['Hint: (lazarus) [TPJSController.GetPas2JSWebServerPath] ignoring macro Pas2JSWebServer parameter "',s,'"']);
  Result:=PJSOptions.GetParsedWebServerFilename;
  if Result='' then
    Result:=PJSDefaultWebServerName; // always return something to get nicer error messages
end;

function TPJSController.GetPas2JSWebServerPort(const s: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Abort:=False;
  if (s<>'') and (ConsoleVerbosity>=0) then
    debugln(['Hint: (lazarus) [TPJSController.GetPas2JSWebServerPort] ignoring macro Pas2JSWebServerPort parameter "',s,'"']);
  Result:=PJSOptions.GetParsedWebServerFilename;
  if Result='' then
    Result:=PJSDefaultWebServerName; // always return something to get nicer error messages
end;

function TPJSController.GetPas2JSBrowser(const s: string; const Data: PtrInt; var Abort: boolean): string;

begin
  Abort:=False;
  if (s<>'') and (ConsoleVerbosity>=0) then
    debugln(['Hint: (lazarus) [TPJSController.GetPas2JSBrowser] ignoring macro Pas2JSBrowser parameter "',s,'"']);
  Result:=PJSOptions.GetParsedBrowserFilename;
  if Result='' then
    Result:='firefox'; // always return something to get nicer error messages
end;

function TPJSController.GetPas2JSNodeJS(const s: string; const Data: PtrInt; var Abort: boolean): string;

begin
  Abort:=False;
  if (s<>'') and (ConsoleVerbosity>=0) then
    debugln(['Hint: (lazarus) [TPJSController.GetPas2JSNodeJS] ignoring macro Pas2JSNodeJS parameter "',s,'"']);
  Result:=PJSOptions.GetParsedNodeJSFilename;
  if Result='' then
    Result:='nodejs'+GetExeExt; // always return something to get nicer error messages
end;

function TPJSController.GetPas2jsProjectURL(const s: string; const Data: PtrInt; var Abort: boolean): string;

Var
  FN : String;

begin
  if (s<>'') and (ConsoleVerbosity>=0) then
    debugln(['Hint: (lazarus) [TPJSController.GetPas2jsProjectURL] ignoring macro Pas2JSProjectURL parameter "',s,'"']);

  if ConsoleVerbosity>0 then
    DebugLN(['LazarusIDE.ActiveProject.CustomData[PJSProjectWebBrowser]: ',LazarusIDE.ActiveProject.CustomData[PJSProjectWebBrowser]]);
  Abort:=LazarusIDE.ActiveProject.CustomData[PJSProjectWebBrowser]<>'1';
  if Abort then
    exit;
  if ConsoleVerbosity>0 then
    DebugLN(['LazarusIDE.ActiveProject.CustomData[PJSProjectURL]: ',LazarusIDE.ActiveProject.CustomData[PJSProjectURL]]);
  Result:=LazarusIDE.ActiveProject.CustomData[PJSProjectURL];
  if (Result='') then
    begin
    FN:=LazarusIDE.ActiveProject.CustomData[PJSProjectHTMLFile];
    if ConsoleVerbosity>0 then
      DebugLN(['LazarusIDE.ActiveProject.CustomData[PJSProjectHTMLFile]: ',LazarusIDE.ActiveProject.CustomData[PJSProjectHTMLFile]]);
    if (FN='') then
      FN:=ChangeFileExt(ExtractFileName(LazarusIDE.ActiveProject.ProjectInfoFile),'.html');
    Result:=LazarusIDE.ActiveProject.CustomData[PJSProjectPort];
    if (Result<>'') and (Result<>'0') then
      Result:=Format('http://localhost:%s/%s',[Result,FN])
    else
      {$IFDEF WINDOWS}
      Result:=Format('file:///%s',[ExtractFilePath(LazarusIDE.ActiveProject.ProjectInfoFile)+FN]);
      {$ELSE}
      Result:=Format('file://%s',[ExtractFilePath(LazarusIDE.ActiveProject.ProjectInfoFile)+FN]);
      {$ENDIF}
    end;
  Abort:=(Result='');
  if ConsoleVerbosity>0 then
    DebugLN(['GetPas2jsProjectURL : ',Result]);
end;

function TPJSController.OnProjectBuilding(Sender: TObject): TModalResult;
var
  aProject: TLazProject;
begin
  Result:=mrOk;
  aProject:=LazarusIDE.ActiveProject;
  debugln(['AAA1 TPJSController.OnProjectBuilding ']);
  if aProject=nil then exit;
  if aProject.IsVirtual then
    begin
    if not SaveHTMLFileToTestDir(aProject) then
      exit(mrCancel);
    end;
end;

function TPJSController.OnRunDebugInit(Sender: TObject; var Handled: boolean
  ): TModalResult;
begin
  debugln(['AAA2 TPJSController.OnRunDebugInit ']);
  Result:=RunProject(Sender,true,Handled);
end;

function TPJSController.OnRunWithoutDebugInit(Sender: TObject; var Handled: boolean): TModalResult;

Var
  ServerPort : Word;
  WebProject : Boolean;
  BaseDir : String;
  aInstance : TServerInstance;

begin
  debugln(['AAA3 TPJSController.OnRunWithoutDebugInit ']);
  Result:=mrOK;
  With LazarusIDE.ActiveProject do
    begin
    if ConsoleVerbosity>=0 then
      begin
      DebugLn(['Info: WebProject=',CustomData[PJSProjectWebBrowser]]);
      DebugLn(['Info: ServerPort=',CustomData[PJSProjectPort]]);
      DebugLn(['Info: BaseDir=',ProjectInfoFile]);
      end;
    WebProject:=CustomData[PJSProjectWebBrowser]='1';
    ServerPort:=StrToIntDef(CustomData[PJSProjectPort],0);
    BaseDir:=ExtractFilePath(ProjectInfoFile);
    end;
  // Exit if we don't need to do anything
  if Not (WebProject and (ServerPort>0)) then
    Exit;
  aInstance:=ServerInstances.FindByPort(ServerPort);
  If Ainstance<>Nil then
    begin
    if ConsoleVerbosity>=0 then
      DebugLn(['Info: Have instance running on port ',ServerPort]);
    if Not SameFileName(BaseDir,aInstance.BaseDir) then
      begin
      if ConsoleVerbosity>=0 then
        DebugLN(['Info: Instance on port ',ServerPort,' serves different directory: ',aInstance.BaseDir]);
      // We should ask the user what to do ?
      If aInstance.Running then
        aInstance.StopServer;
      end;
    end
  else
    begin
    Debugln(['Info: No instance running on port ',ServerPort, 'allocating it']);
    aInstance:=ServerInstances.AddInstance(ServerPort,BaseDir,PJSOptions.GetParsedWebServerFilename);
    end;
  aInstance.LastProject:=LazarusIDE.ActiveProject.ProjectInfoFile;
  aInstance.StartServer;
  Handled:=False;
end;

function TPJSController.GetHTMLFilename(aProject: TLazProject;
  UseTestDir: boolean): string;
var
  HTMLFile: TLazProjectFile;
begin
  Result:=aProject.CustomData.Values[PJSProjectHTMLFile];
  if Result='' then exit;
  if aProject.IsVirtual then
    begin
    HTMLFile:=aProject.FindFile(Result,[pfsfOnlyProjectFiles]);
    if HTMLFile=nil then
      exit('');
    Result:=HTMLFile.Filename;
    if (not FilenameIsAbsolute(Result)) and UseTestDir then
      Result:=AppendPathDelim(LazarusIDE.GetTestBuildDirectory)+Result;
    end
  else
    begin
    if not FilenameIsAbsolute(Result) then
      Result:=ExtractFilePath(aProject.ProjectInfoFile)+Result;
    HTMLFile:=aProject.FindFile(Result,[pfsfOnlyProjectFiles]);
    if HTMLFile=nil then
      exit('');
    Result:=HTMLFile.Filename;
    end;
end;

function TPJSController.GetWebDir(aProject: TLazProject): string;
var
  HTMLFilename: String;
begin
  Result:='';

  HTMLFilename:=GetHTMLFilename(aProject,true);
  if HTMLFilename<>'' then
    Result:=ExtractFilePath(HTMLFilename);

  if Result='' then
    if aProject.IsVirtual then
      Result:=LazarusIDE.GetTestBuildDirectory
    else
      Result:=ExtractFilePath(aProject.ProjectInfoFile);
end;

function TPJSController.RunProject(Sender: TObject; WithDebug: boolean;
  var Handled: boolean): TModalResult;
var
  aProject: TLazProject;
  IsWebProject: Boolean;
  ServerPort: Integer;
  WebDir, HTMLFilename: String;
  aServer: TSWSInstance;
begin
  Result:=mrOk;
  if Sender=nil then ;
  if WithDebug then ;

  aProject:=LazarusIDE.ActiveProject;
  if aProject=nil then exit;

  IsWebProject:=aProject.CustomData[PJSProjectWebBrowser]='1';
  ServerPort:=StrToIntDef(aProject.CustomData[PJSProjectPort],-1);
  if not IsWebProject or (ServerPort<0) then
    exit;

  Handled:=true;

  // compile
  Result:=LazarusIDE.DoBuildProject(crRun,[]);
  if Result<>mrOk then exit;

  // start web server
  WebDir:=GetWebDir(aProject);
  if WebDir='' then
    begin
    debugln(['Warning: TPJSController.RunProject missing webdir']);
    exit(mrCancel);
    end;
  aServer:=SimpleWebServerController.AddProjectServer(aProject,ServerPort,WebDir,true);
  if aServer=nil then
    exit(mrCancel);

  // start browser
  HTMLFilename:=GetHTMLFilename(aProject,true);
  if HTMLFilename='' then
    begin
    debugln(['Info: TPJSController.RunProject missing htmlfile']);
    exit(mrCancel);
    end;
  if not SimpleWebServerController.OpenBrowserWithServer(aServer,HTMLFilename) then
    exit(mrCancel);
end;

function TPJSController.SaveHTMLFileToTestDir(aProject: TLazProject): boolean;
var
  HTMLFilename, FullHTMLFilename: String;
  HTMLFile: TLazProjectFile;
  Code: TCodeBuffer;
begin
  // if project has a pas2js html filename, save it to the test directory
  Result:=false;
  HTMLFilename:=aProject.CustomData.Values[PJSProjectHTMLFile];
  debugln(['AAA7 TPJSController.SaveHTMLFileToTestDir ',HTMLFilename]);
  if (HTMLFilename='') then
    exit(true);
  if FilenameIsAbsolute(HTMLFilename) then
    begin
    debugln(['Warning: TPJSController.SaveHTMLFileToTestDir html file is absolute: "',HTMLFilename,'"']);
    exit(true);
    end;
  HTMLFile:=aProject.FindFile(HTMLFilename,[pfsfOnlyProjectFiles]);
  if HTMLFile=nil then
    begin
    debugln(['Warning: TPJSController.SaveHTMLFileToTestDir invalid aProject.CustomData.Values[',PJSProjectHTMLFile,']']);
    exit;
    end;
  HTMLFilename:=HTMLFile.Filename;

  Code:=CodeToolBoss.FindFile(HTMLFilename);
  if Code=nil then
    begin
    debugln(['Error: TPJSController.SaveHTMLFileToTestDir missing codebuffer "',HTMLFilename,'"']);
    exit;
    end;

  FullHTMLFilename:=AppendPathDelim(LazarusIDE.GetTestBuildDirectory)+HTMLFilename;
  if not Code.SaveToFile(FullHTMLFilename) then
    begin
    debugln(['Error: TPJSController.SaveHTMLFileToTestDir write error: ',Code.LastError,' File="',FullHTMLFilename,'"']);
    exit;
    end;

  Result:=true;
end;

constructor TPJSController.Create;
begin
  // Nothing for the moment
  FServerInstances:=TServerInstanceList.Create(TServerInstance);
end;

destructor TPJSController.Destroy;
begin
  Unhook;
  FreeAndNil(FServerInstances);
  inherited Destroy;
end;

procedure TPJSController.Hook;
begin
  IDEMacros.Add(TTransferMacro.Create('Pas2JS', '', pjsdPas2JSExecutable, @
    GetPas2JSPath, []));
  IDEMacros.Add(TTransferMacro.Create('Pas2JSWebServer', '', pjsdPas2JSWebServerExe, @
    GetPas2JSWebServerPath, []));
  IDEMacros.Add(TTransferMacro.Create('Pas2JSWebServerPort', '', pjsdPas2JSWebServerPort, @
    GetPas2JSWebServerPort, []));
  IDEMacros.Add(TTransferMacro.Create('Pas2JSBrowser', '',
    pjsdPas2JSSelectedBrowserExecutable, @GetPas2JSBrowser, []));
  IDEMacros.Add(TTransferMacro.Create('Pas2JSNodeJS', '',
    pjsdPas2JSSelectedNodeJSExcutable, @GetPas2JSNodeJS, []));
  IDEMacros.Add(TTransferMacro.Create('Pas2JSProjectURL', '',
    pjsdPas2JSCurrentProjectURL, @GetPas2jsProjectURL, []));
  LazarusIDE.AddHandlerOnProjectBuilding(@OnProjectBuilding);
  LazarusIDE.AddHandlerOnRunDebugInit(@OnRunDebugInit);
  LazarusIDE.AddHandlerOnRunWithoutDebugInit(@OnRunWithoutDebugInit);
end;

procedure TPJSController.UnHook;
begin
  // Nothing for the moment
end;

procedure TPJSController.RefreshView;
begin
  If Assigned(FOnRefresh) then
    FOnRefresh(Self);
end;

finalization
  TPJSController.DoneInstance;
end.

