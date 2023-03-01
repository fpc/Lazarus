unit PJSController;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process,
  // LazUtils
  LazLoggerBase, LazUtilities, FileUtil, LazFileUtils, AvgLvlTree,
  // LCL
  Forms, Controls, LazHelpIntf,
  // IdeIntf
  MacroIntf, MacroDefIntf, ProjectIntf, CompOptsIntf, IDEExternToolIntf,
  LazIDEIntf, ProjectGroupIntf,
  // pas2js
  SimpleWebSrvController, StrPas2JSDesign, PJSDsgnOptions, CodeToolManager,
  CodeCache;

Type

  { TPJSController }

  TPJSController = Class
  Private
    FMacroPas2js: TTransferMacro;
    FOnRefresh: TNotifyEvent;
    function GetPas2JSPath(const s: string; const {%H-}Data: PtrInt; var Abort: boolean): string;
    function GetPas2JSBrowser(const s: string; const {%H-}Data: PtrInt; var Abort: boolean): string;
    function GetPas2JSNodeJS(const s: string; const {%H-}Data: PtrInt; var Abort: boolean): string;
    function GetPas2JSElectron(const s: string; const {%H-}Data: PtrInt; var Abort: boolean): string;
    function GetPas2jsProjectURL(const s: string; const {%H-}Data: PtrInt; var Abort: boolean): string;
    function OnProjectBuilding(Sender: TObject): TModalResult;
    function OnProjectGroupRunLazbuild({%H-}Target: TPGCompileTarget;
      Tool: TAbstractExternalTool): boolean;
    function OnRunDebugInit(Sender: TObject; var Handled: boolean
      ): TModalResult;
    function OnRunWithoutDebugInit(Sender: TObject; var Handled: boolean): TModalResult;
    function RunProject(Sender: TObject; WithDebug: boolean; var Handled: boolean): TModalResult;
    function RunBrowserProject(aProject: TLazProject; WithDebug: boolean; var Handled: boolean): TModalResult;
    function RunNonBrowserProject(aProject: TLazProject; WithDebug: boolean; var Handled: boolean): TModalResult;
    function SaveHTMLFileToTestDir(aProject: TLazProject): boolean;
    class function GetProjectHTMLLegacyFilename(aProject: TLazProject): string;
  Public
    Constructor Create;
    Destructor Destroy; override;
    Class Procedure DoneInstance;
    Class Function Instance: TPJSController;
    Procedure Hook; virtual;
    Procedure UnHook; virtual;
    Procedure StoreMacros; virtual;
    // Determine project HTML file from custom data
    class function GetProjectHTMLFile(aProject: TLazProject): TLazProjectFile;
    class function GetProjectHTMLFilename(aProject: TLazProject): string;
    // Get filename to show in browser when running
    function GetHTMLFilename(aProject: TLazProject; UseTestDir: boolean): string; virtual;
    function GetWebDir(aProject: TLazProject): string; virtual; // disk directory for webserver
    function GetProjectLocation(aProject: TLazProject): string; virtual;
    function GetProjectURL(aProject: TLazProject): string; virtual;
    Property OnRefresh : TNotifyEvent Read FOnRefresh Write FonRefresh;
    property MacroPas2js: TTransferMacro read FMacroPas2js;
  end;

Const
  // Custom settings in .lpi
  PJSProject = 'Pas2JSProject'; // Project is pas2js project
  PJSProjectWebBrowser =  'PasJSWebBrowserProject'; // Web browser project
  PJSProjectHTMLFile = 'PasJSHTMLFile' deprecated 'use TPJSController.GetProjectHTMLFilename'; // No longer used
  PJSIsProjectHTMLFile = 'PasJSIsProjectHTMLFile';
  PJSProjectMaintainHTML = 'MaintainHTML';
  PJSProjectUseBrowserConsole = 'BrowserConsole';
  PJSProjectRunAtReady = 'RunAtReady';
  PJSProjectLocation = 'PasJSLocation';
  PJSProjectPort = 'PasJSPort';
  PJSProjectURL = 'PasJSURL';
  PJSProjectHTMLBaseDir = 'HTMLDir';

implementation

Var
  ctrl : TPJSController;

{ TPJSController }

class procedure TPJSController.DoneInstance;
begin
  FreeAndNil(Ctrl)
end;

class function TPJSController.Instance: TPJSController;
begin
  if Ctrl=Nil then
    Ctrl:=TPJSController.Create;
  Result:=Ctrl;
end;

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

function TPJSController.GetPas2JSBrowser(const s: string; const Data: PtrInt; var Abort: boolean): string;
var
  Params: TStringList;
begin
  Abort:=False;
  if (s<>'') and (ConsoleVerbosity>=0) then
    debugln(['Hint: (lazarus) [TPJSController.GetPas2JSBrowser] ignoring macro Pas2JSBrowser parameter "',s,'"']);
  Params:=TStringList.Create;
  try
    Result:=SimpleWebServerController.GetBrowser('',Params);
  finally
    Params.Free;
  end;
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

function TPJSController.GetPas2JSElectron(const s: string; const Data: PtrInt;
  var Abort: boolean): string;
begin
  Abort:=False;
  if (s<>'') and (ConsoleVerbosity>=0) then
    debugln(['Hint: (lazarus) [TPJSController.GetPas2JSElectron] ignoring macro Pas2JSElectron parameter "',s,'"']);
  Result:=PJSOptions.GetParsedElectronExe;
  if Result='' then
    Result:='electron'+GetExeExt; // always return something to get nicer error messages
end;

function TPJSController.GetPas2jsProjectURL(const s: string; const Data: PtrInt; var Abort: boolean): string;
Var
  aProject: TLazProject;
begin
  if (s<>'') and (ConsoleVerbosity>=0) then
    debugln(['Hint: (lazarus) [TPJSController.GetPas2jsProjectURL] ignoring macro Pas2JSProjectURL parameter "',s,'"']);

  aProject:=LazarusIDE.ActiveProject;
  if ConsoleVerbosity>0 then
    DebugLN(['Hint: (lazarus) [TPJSController.GetPas2jsProjectURL] LazarusIDE.ActiveProject.CustomData[PJSProjectWebBrowser]: ',aProject.CustomData[PJSProjectWebBrowser]]);
  Abort:=aProject.CustomData[PJSProjectWebBrowser]<>'1';
  if Abort then
    exit;
  if ConsoleVerbosity>0 then
    DebugLN(['Hint: (lazarus) [TPJSController.GetPas2jsProjectURL] LazarusIDE.ActiveProject.CustomData[PJSProjectURL]: ',aProject.CustomData[PJSProjectURL]]);
  Result:=GetProjectURL(aProject);
  Abort:=(Result='');
  if ConsoleVerbosity>0 then
    DebugLN(['Hint: (lazarus) [TPJSController.GetPas2jsProjectURL] Result="',Result,'"']);
end;


function TPJSController.OnProjectBuilding(Sender: TObject): TModalResult;
var
  aProject: TLazProject;
begin
  Result:=mrOk;
  aProject:=LazarusIDE.ActiveProject;
  if aProject=nil then exit;
  StoreMacros;
  if aProject.IsVirtual then
    begin
    if not SaveHTMLFileToTestDir(aProject) then
      exit(mrCancel);
    end;
end;

function TPJSController.OnProjectGroupRunLazbuild(Target: TPGCompileTarget;
  Tool: TAbstractExternalTool): boolean;
var
  Pas2jsFilename: String;
begin
  Result:=true;
  Pas2jsFilename:=PJSOptions.GetParsedCompilerFilename;
  if Pas2jsFilename<>'' then
    Tool.EnvironmentOverrides.Values['PAS2JS']:=Pas2jsFilename;
end;

function TPJSController.OnRunDebugInit(Sender: TObject; var Handled: boolean
  ): TModalResult;
begin
  Result:=RunProject(Sender,true,Handled);
end;

function TPJSController.OnRunWithoutDebugInit(Sender: TObject; var Handled: boolean): TModalResult;
begin
  Result:=RunProject(Sender,false,Handled);
end;

class function TPJSController.GetProjectHTMLFile(aProject: TLazProject
  ): TLazProjectFile;
var
  HTMLFilename: String;
  i: Integer;
begin
  for i:=0 to aProject.FileCount-1 do
    begin
    Result:=aProject.Files[i];
    if Result.IsPartOfProject
        and (Result.CustomData[PJSIsProjectHTMLFile]='1') then
      exit;
    end;
  Result:=nil;

  HTMLFilename:=GetProjectHTMLLegacyFilename(aProject);
  if HTMLFilename<>'' then
    Result:=aProject.FindFile(HTMLFilename,[pfsfOnlyProjectFiles]);
end;

class function TPJSController.GetProjectHTMLFilename(aProject: TLazProject): string;

Var
  aFile: TLazProjectFile;

begin
  aFile:=GetProjectHTMLFile(aProject);
  if aFile<>nil then
    Result:=aFile.GetFullFilename
  else
    Result:='';
end;

function TPJSController.GetHTMLFilename(aProject: TLazProject; UseTestDir: boolean): string;
begin
  Result:=GetProjectHTMLFileName(aProject);
  if Result='' then exit;
  if FilenameIsAbsolute(Result) then exit;

  if aProject.IsVirtual then
    begin
    if UseTestDir then
      Result:=AppendPathDelim(LazarusIDE.GetTestBuildDirectory)+Result;
    end
  else
    Result:=TrimFilename(AppendPathDelim(aProject.Directory)+Result);
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

function TPJSController.GetProjectLocation(aProject: TLazProject): string;
begin
  Result:=aProject.CustomData[PJSProjectLocation];
  IDEMacros.SubstituteMacros(Result);
end;

function TPJSController.GetProjectURL(aProject: TLazProject): string;
Var
  HTMLFilename, WebDir: String;
  Port: LongInt;
begin
  if aProject=nil then
    exit('');
  Result:=aProject.CustomData[PJSProjectURL];
  if Result<>'' then
    begin
    IDEMacros.SubstituteMacros(Result);
    exit;
    end;

  if Result='' then
    begin
    Port:=StrToIntDef(aProject.CustomData[PJSProjectPort],-1);
    HTMLFilename:=GetHTMLFilename(aProject,true);
    if HTMLFilename='' then
      begin
      if Port<=0 then
        HTMLFilename:=ChangeFileExt(aProject.ProjectInfoFile,'.html')
      else
        HTMLFilename:=ExtractFileNameOnly(aProject.ProjectInfoFile)+'.html';
      end
    else if Port>0 then
      begin
      WebDir:=GetWebDir(aProject);
      HTMLFilename:=CreateRelativePath(HTMLFilename,WebDir);
      end;
    HTMLFilename:=FilenameToURLPath(HTMLFilename);
    if Port<=0 then
      Result:='file://'+HTMLFilename
    else
      Result:='http://127.0.0.1:'+IntToStr(Port)+'/'+HTMLFilename;
    end;
end;

function TPJSController.RunProject(Sender: TObject; WithDebug: boolean;
  var Handled: boolean): TModalResult;
var
  aProject: TLazProject;
  IsWebProject, IsPSProject: Boolean;
begin
  Result:=mrOk;
  if Sender=nil then ;

  aProject:=LazarusIDE.ActiveProject;
  if aProject=nil then exit;

  IsWebProject:=aProject.CustomData[PJSProjectWebBrowser]='1';
  if IsWebProject then
    begin
    Result:=RunBrowserProject(aProject,WithDebug,Handled);
    exit;
    end;

  IsPSProject:=aProject.CustomData[PJSProject]='1';
  if IsPSProject then
    begin
    Result:=RunNonBrowserProject(aProject,WithDebug,Handled);
    exit;
    end;
end;

function TPJSController.RunBrowserProject(aProject: TLazProject;
  WithDebug: boolean; var Handled: boolean): TModalResult;
var
  ServerPort: Integer;
  WebDir, HTMLFilename, URL, WorkDir, Location: String;
  aServer: TSWSInstance;
  SWSLocation: TSWSLocation;
begin
  Result:=mrOk;
  aProject:=LazarusIDE.ActiveProject;

  if SimpleWebServerController.Options.ServerExe='compileserver'+GetExeExt then
    begin
    // simplewebservergui package has default value
    if CompareFilenames(ExtractFilename(PJSOptions.OldWebServerFileName),'compileserver'+GetExeExt)=0 then
      begin
      // user had used compileserver too -> migrate to simplewebservergui once
      SimpleWebServerController.Options.ServerExe:=PJSOptions.OldWebServerFileName;
      SimpleWebServerController.Options.SaveSafe;
      PJSOptions.OldWebServerFileName:='';
      PJSOptions.Save;
      end;
    end;

  Location:=aProject.CustomData[PJSProjectLocation];
  ServerPort:=StrToIntDef(aProject.CustomData[PJSProjectPort],-1);
  URL:=aProject.CustomData[PJSProjectURL];
  if (Location='') and (ServerPort<0) and (URL='') then
    exit; // compile normally and run the run-parameters

  // Run webproject with Debug: build, start webserver, open browser

  Handled:=true;

  // compile
  Result:=LazarusIDE.DoBuildProject(crRun,[]);
  if Result<>mrOk then exit;

  if WithDebug then ;

  if (Location<>'') or (ServerPort>=0) then
    begin
    // start http server
    WebDir:=GetWebDir(aProject);
    if WebDir='' then
      begin
      debugln(['Warning: TPJSController.RunProject missing webdir']);
      exit(mrCancel);
      end;
    if WebDir='' then
      exit(mrCancel);

    SWSLocation:=nil;
    if Location<>'' then
      begin
      Location:=GetProjectLocation(aProject);
      if Location='' then
        Location:=ExtractFileNameOnly(aProject.MainFile.Filename);
      SWSLocation:=SimpleWebServerController.AddProjectLocation(aProject,Location,WebDir,true);
      if SWSLocation=nil then
        exit(mrCancel);
      if SWSLocation.ErrorDesc<>'' then
        exit(mrCancel);
      end
    else
      begin
      aServer:=SimpleWebServerController.AddProjectServer(aProject,ServerPort,WebDir,true);
      if aServer=nil then
        exit(mrCancel);
      if aServer.ErrorDesc<>'' then
        exit(mrCancel);
      end;

    // start browser
    HTMLFilename:=GetHTMLFilename(aProject,true);
    if HTMLFilename='' then
      begin
      debugln(['Info: TPJSController.RunProject missing htmlfile']);
      exit(mrCancel);
      end;

    if SWSLocation<>nil then
      begin
      if not SimpleWebServerController.OpenBrowserWithLocation(SWSLocation,HTMLFilename) then
        exit(mrCancel);
      end
    else
      begin
      if not SimpleWebServerController.OpenBrowserWithServer(aServer,HTMLFilename) then
        exit(mrCancel);
      end;

    end
  else
    begin
    // start browser with user URL
    URL:=GetProjectURL(aProject);
    if aProject.IsVirtual then
      WorkDir:=LazarusIDE.GetTestBuildDirectory
    else
      WorkDir:=ExtractFilePath(aProject.ProjectInfoFile);
    if not SimpleWebServerController.OpenBrowserWithURL(URL,WorkDir) then
      exit(mrCancel);
    end;
end;

function TPJSController.RunNonBrowserProject(aProject: TLazProject;
  WithDebug: boolean; var Handled: boolean): TModalResult;
begin
  Result:=mrOk;

  if not WithDebug then
    exit; // compile normally and run the run-parameters

  if aProject=nil then ;

  // for now: redirect to run without debug
  Handled:=true;
  Result:=LazarusIDE.DoRunProjectWithoutDebug;
end;

function TPJSController.SaveHTMLFileToTestDir(aProject: TLazProject): boolean;
var
  HTMLFilename, FullHTMLFilename: String;
  HTMLFile: TLazProjectFile;
  Code: TCodeBuffer;
begin
  // if project has a pas2js html filename, save it to the test directory
  Result:=false;
  HTMLFilename:=GetProjectHTMLFilename(aProject);
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
    debugln(['Warning: TPJSController.SaveHTMLFileToTestDir invalid project filename [',HTMLFilename,']']);
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

class function TPJSController.GetProjectHTMLLegacyFilename(aProject: TLazProject
  ): string;
begin
  Result:=aProject.CustomData.Values[PJSProjectHTMLFile{%H-}];
  if Result='' then exit;
  DoDirSeparators(Result);
  if (not aProject.IsVirtual) and (not FilenameIsAbsolute(Result)) then
    Result:=TrimFilename(AppendPathDelim(aProject.Directory)+Result);
end;

constructor TPJSController.Create;
begin
  // Nothing for the moment
end;

destructor TPJSController.Destroy;
begin
  Unhook;
  inherited Destroy;
end;

procedure TPJSController.Hook;
begin
  FMacroPas2js:=IDEMacros.Add('Pas2JS', '', pjsdPas2JSExecutable, @GetPas2JSPath, [tmfLazbuild]);
  IDEMacros.Add(TTransferMacro.Create('Pas2JSBrowser', '',
    pjsdPas2JSSelectedBrowserExecutable, @GetPas2JSBrowser, []));
  IDEMacros.Add(TTransferMacro.Create('Pas2JSNodeJS', '',
    pjsdPas2JSSelectedNodeJSExcutable, @GetPas2JSNodeJS, []));
  IDEMacros.Add(TTransferMacro.Create('Pas2JSElectron', '',
    pjsdPas2JSSelectedElectronExcutable, @GetPas2JSElectron, []));
  IDEMacros.Add(TTransferMacro.Create('Pas2JSProjectURL', '',
    pjsdPas2JSCurrentProjectURL, @GetPas2jsProjectURL, []));
  LazarusIDE.AddHandlerOnProjectBuilding(@OnProjectBuilding);
  LazarusIDE.AddHandlerOnRunDebugInit(@OnRunDebugInit);
  LazarusIDE.AddHandlerOnRunWithoutDebugInit(@OnRunWithoutDebugInit);
  ProjectGroupManager.AddHandlerOnRunLazbuild(@OnProjectGroupRunLazbuild);
end;

procedure TPJSController.UnHook;
begin
  FMacroPas2js:=nil;
end;

procedure TPJSController.StoreMacros;
begin
  if PJSOptions=nil then exit;
  FMacroPas2js.LazbuildValue:=PJSOptions.GetParsedCompilerFilename;
end;

finalization
  TPJSController.DoneInstance;
end.


