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
  MacroIntf, MacroDefIntf, ProjectIntf, CompOptsIntf, LazIDEIntf,
  // pas2js
  SimpleWebSrvController, StrPas2JSDesign, PJSDsgnOptions, CodeToolManager,
  CodeCache;

Type

  { TPJSController }

  TPJSController = Class
  Private
    FOnRefresh: TNotifyEvent;
    function GetPas2JSPath(const s: string; const {%H-}Data: PtrInt; var Abort: boolean): string;
    function GetPas2JSWebServerPath(const s: string; const {%H-}Data: PtrInt; var Abort: boolean): string;
    function GetPas2JSWebServerPort(const s: string; const {%H-}Data: PtrInt; var Abort: boolean): string;
    function GetPas2JSBrowser(const s: string; const {%H-}Data: PtrInt; var Abort: boolean): string;
    function GetPas2JSNodeJS(const s: string; const {%H-}Data: PtrInt; var Abort: boolean): string;
    function GetPas2jsProjectURL(const s: string; const {%H-}Data: PtrInt; var Abort: boolean): string;
    procedure OnLoadSaveCustomData(Sender: TObject; Load: boolean;
      CustomData: TStringToStringTree; PathDelimChanged: boolean);
    function OnProjectBuilding(Sender: TObject): TModalResult;
    function OnRunDebugInit(Sender: TObject; var Handled: boolean
      ): TModalResult;
    function OnRunWithoutDebugInit(Sender: TObject; var Handled: boolean): TModalResult;
    function RunProject(Sender: TObject; WithDebug: boolean; var Handled: boolean): TModalResult;
    function SaveHTMLFileToTestDir(aProject: TLazProject): boolean;
  Public
    Constructor Create;
    Destructor Destroy; override;
    Class Procedure DoneInstance;
    Class Function instance :  TPJSController;
    Procedure Hook; virtual;
    Procedure UnHook; virtual;
    function GetHTMLFilename(aProject: TLazProject; UseTestDir: boolean): string; virtual;
    function GetWebDir(aProject: TLazProject): string; virtual;
    function GetProjectURL(aProject: TLazProject): string; virtual;
    Property OnRefresh : TNotifyEvent Read FOnRefresh Write FonRefresh;
  end;

Const
  // Custom settings in .lpi
  PJSProject = 'Pas2JSProject'; // Project is pas2js project
  PJSProjectWebBrowser =  'PasJSWebBrowserProject'; // Web browser project
  PJSProjectHTMLFile = 'PasJSHTMLFile';
  PJSIsProjectHTMLFile = 'PasJSIsProjectHTMLFile';
  PJSProjectMaintainHTML = 'MaintainHTML';
  PJSProjectUseBrowserConsole = 'BrowserConsole';
  PJSProjectRunAtReady = 'RunAtReady';
  PJSProjectPort = 'PasJSPort';
  PJSProjectURL = 'PasJSURL';


implementation

Var
  ctrl : TPJSController;

{ TPJSController }

class procedure TPJSController.DoneInstance;
begin
  FreeAndNil(Ctrl)
end;

class function TPJSController.instance: TPJSController;
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
  aProject: TLazProject;
begin
  if (s<>'') and (ConsoleVerbosity>=0) then
    debugln(['Hint: (lazarus) [TPJSController.GetPas2jsProjectURL] ignoring macro Pas2JSProjectURL parameter "',s,'"']);

  aProject:=LazarusIDE.ActiveProject;
  if ConsoleVerbosity>0 then
    DebugLN(['LazarusIDE.ActiveProject.CustomData[PJSProjectWebBrowser]: ',aProject.CustomData[PJSProjectWebBrowser]]);
  Abort:=aProject.CustomData[PJSProjectWebBrowser]<>'1';
  if Abort then
    exit;
  if ConsoleVerbosity>0 then
    DebugLN(['LazarusIDE.ActiveProject.CustomData[PJSProjectURL]: ',aProject.CustomData[PJSProjectURL]]);
  Result:=GetProjectURL(aProject);
  Abort:=(Result='');
  if ConsoleVerbosity>0 then
    DebugLN(['GetPas2jsProjectURL : ',Result]);
end;

procedure TPJSController.OnLoadSaveCustomData(Sender: TObject; Load: boolean;
  CustomData: TStringToStringTree; PathDelimChanged: boolean);
var
  fn: String;
  aProject: TLazProject;
begin
  if Sender is TLazProject then
    begin
    aProject:=TLazProject(Sender);
    if CustomData[PJSProjectWebBrowser]='1' then
      begin
      fn:=CustomData[PJSProjectHTMLFile];
      if fn<>'' then
        begin
        if Load then
          aProject.ConvertFromLPIFilename(fn)
        else
          aProject.ConvertToLPIFilename(fn);
        CustomData[PJSProjectHTMLFile]:=fn;
        end;
      end;
    end;
  if PathDelimChanged then ;
end;

function TPJSController.OnProjectBuilding(Sender: TObject): TModalResult;
var
  aProject: TLazProject;
begin
  Result:=mrOk;
  aProject:=LazarusIDE.ActiveProject;
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
  Result:=RunProject(Sender,true,Handled);
end;

function TPJSController.OnRunWithoutDebugInit(Sender: TObject; var Handled: boolean): TModalResult;
begin
  Result:=RunProject(Sender,false,Handled);
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

function TPJSController.GetProjectURL(aProject: TLazProject): string;
Var
  HTMLFilename, WebDir: String;
  Port: LongInt;
begin
  if aProject=nil then
    exit('');
  Result:=aProject.CustomData[PJSProjectURL];
  if Result<>'' then
    IDEMacros.SubstituteMacros(Result);

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
  IsWebProject: Boolean;
  ServerPort: Integer;
  WebDir, HTMLFilename, URL, WorkDir: String;
  aServer: TSWSInstance;
begin
  Result:=mrOk;
  if Sender=nil then ;

  aProject:=LazarusIDE.ActiveProject;
  if aProject=nil then exit;

  IsWebProject:=aProject.CustomData[PJSProjectWebBrowser]='1';
  if not IsWebProject then
    exit;

  if SimpleWebServerController.Options.ServerExe='compileserver'+GetExeExt then
    begin
    // simplewebservergui package has default value
    if CompareFilenames(ExtractFilename(PJSOptions.WebServerFileName),'compileserver'+GetExeExt)=0 then
      begin
      // this package has a compileserver -> for compatibility set our value
      SimpleWebServerController.Options.ServerExe:=PJSOptions.WebServerFileName;
      end;
    end;

  if not WithDebug then
    exit; // compile normally and run the run-parameters

  ServerPort:=StrToIntDef(aProject.CustomData[PJSProjectPort],-1);
  URL:=aProject.CustomData[PJSProjectURL];
  if (ServerPort<0) and (URL='') then
    exit;

  // Run webproject with Debug: build, start webserver, open browser

  Handled:=true;

  // compile
  Result:=LazarusIDE.DoBuildProject(crRun,[]);
  if Result<>mrOk then exit;

  if ServerPort>=0 then
    begin
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
    if aServer.ErrorDesc<>'' then
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

function TPJSController.SaveHTMLFileToTestDir(aProject: TLazProject): boolean;
var
  HTMLFilename, FullHTMLFilename: String;
  HTMLFile: TLazProjectFile;
  Code: TCodeBuffer;
begin
  // if project has a pas2js html filename, save it to the test directory
  Result:=false;
  HTMLFilename:=aProject.CustomData.Values[PJSProjectHTMLFile];
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
end;

destructor TPJSController.Destroy;
begin
  Unhook;
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
  LazarusIDE.AddHandlerOnLoadSaveCustomData(@OnLoadSaveCustomData);
end;

procedure TPJSController.UnHook;
begin
  // Nothing for the moment
end;

finalization
  TPJSController.DoneInstance;
end.

