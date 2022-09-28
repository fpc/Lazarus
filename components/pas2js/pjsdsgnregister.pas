unit PJSDsgnRegister;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, fpjson,
  // LCL
  Forms, Controls, Dialogs, LazHelpIntf,
  // LazUtils
  LazLoggerBase, LazFileUtils, FileUtil,
  // codetools
  CodeToolManager, CodeCache,
  // IdeIntf
  IDECommands, MenuIntf, ProjectIntf, CompOptsIntf, LazIDEIntf,
  IDEOptionsIntf, IDEOptEditorIntf, ComponentEditors, SrcEditorIntf, IDEMsgIntf,
  IDEDialogs, ProjectGroupIntf, IDEExternToolIntf, MacroIntf, PackageIntf,
  // Pas2js
  idehtml2class, PJSDsgnOptions, PJSDsgnOptsFrame, idedtstopas,
  frmpas2jsnodejsprojectoptions,
  frmpas2jsbrowserprojectoptions, PJSProjectOptions, idehtmltools,
  frmhtmltoform, PJSController, StrPas2JSDesign, ProjectGroup;

const
  ProjDescNamePas2JSWebApp = 'Web Application';
  ProjDescNamePas2JSProgressiveWebApp = 'Progressive Web Application';
  ProjDescNamePas2JSServiceWorker = 'Pas2JS Service Worker';
  ProjDescNamePas2JSElectronWebApp = 'Electron Web Application';
  ProjDescNamePas2JSNodeJSApp = 'NodeJS Application';
  ProjDescNamePas2JSModuleApp = 'Pas2JS Library';
  FileDescNameClassFromHTMLFile = 'Class definition from HTML file';
  SMessageViewHTMLToForm = 'HTML To Class conversion';

  DefaultIconSizes: array[0..9] of word = (16,24,32,48,72,96,128,192,384,512);

type

  { TProjectPas2JSWebApp - a project descriptor showing an option dialog to
    setup a html file, and using pas2js as compiler }

  TBrowserApplicationOption = (baoCreateHtml,        // Create template HTML page
                               baoMaintainHTML,      // Maintain the template HTML page
                               baoRunOnReady,        // Run in document.onReady
                               baoShowException,     // let RTL show uncaught exceptions
                               baoUseBrowserApp,     // Use browser app object
                               baoUseWASI,           // Use WASI browser app object
                               baoUseBrowserConsole, // use browserconsole unit to display Writeln()
                               baoUseModule,         // include as module as opposed to regular script
                               baoLocationOnSWS,       // add location
                               baoStartServer,       // Start simple server
                               baoUseURL             // Use this URL to run/show project in browser
                               );
  TBrowserApplicationOptions = set of TBrowserApplicationOption;

  TProjectPas2JSWebApp = class(TProjectDescriptor)
  private
    FHTMLFilename: string;
    FMainSrcFileName: string;
    FMainSrcName: string;
    FOptions: TBrowserApplicationOptions;
    FProjectLocation: string;
    FProjectPort: integer;
    FProjectURL: String;
    FProjectWasmURL : String;
    FScriptFilename: string;
    FServiceWorkerJSFilename: string;
    function GetHTMLFilename: string;
    function GetMainSrcFileName: string;
    function GetMainSrcName: string;
    function GetScriptFilename: string;
  protected
    procedure AddHTMLHead(Src: TStringList); virtual;
    procedure AddBody(Src: TStringList); virtual;
    function CreateHTMLFile(AProject: TLazProject; aScriptFileName: String
      ): TLazProjectFile; virtual;
    function CreateProjectSource: String; virtual;
    Function DoInitDescriptor: TModalResult; override;
    function GetNextPort: Word; virtual;
    function ShowOptionsDialog: TModalResult; virtual;
    function ShowModalOptions(Frm: TWebBrowserProjectOptionsForm): TModalResult; virtual;
    procedure EnableRunBrowser(AProject: TLazProject); virtual;
  public
    constructor Create; override;
    procedure Clear; virtual;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
    property Options : TBrowserApplicationOptions read FOptions Write Foptions;
    property ProjectLocation : string Read FProjectLocation Write FProjectLocation;
    property ProjectPort : integer Read FProjectPort Write FProjectPort;
    property ProjectURL : string Read FProjectURL Write FProjectURL;
    property MainSrcFileName: string read GetMainSrcFileName write FMainSrcFileName;
    property MainSrcName: string read GetMainSrcName write FMainSrcName;
    property HTMLFilename: string read GetHTMLFilename write FHTMLFilename;
    property ServiceWorkerJSFilename: string read FServiceWorkerJSFilename write FServiceWorkerJSFilename;
    property ScriptFilename: string read GetScriptFilename write FScriptFilename;
  end;

  { TProjectPas2JSServiceWorker - project descriptor for a pas2js service worker }

  TProjectPas2JSServiceWorker = class(TProjectDescriptor)
  public
    constructor Create; override;
    function CreateProjectSource: String; virtual;
    function InitServiceWorkerProject(AProject: TLazProject;
      LPRFilename: string): TModalResult; virtual;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;

  { TMultiProjectPas2JSWebApp - like TProjectPas2JSWebApp, except
    asks user for a the lpr filename and warns about overwrites }

  TMultiProjectPas2JSWebApp = class(TProjectPas2JSWebApp)
  private
    FLPGFilename: string;
    FOverwrites: TStrings;
    FProjectDir: string;
    FWebDir: string;
    procedure SetOverwrites(const AValue: TStrings);
  protected
    function CheckOverwriteFile(aFilename: string): string;
    function CheckOverwriteDir(aDir: string): string;
    function FileToWebFile(aFilename: string): string; virtual;
    function InteractiveForceDir(Dir: string; AutoDelete: boolean): boolean; virtual;
    function InteractiveSaveFile(aFilename: string): boolean; virtual;
    function InteractiveCopyFile(Src, Dest: string): boolean; virtual;
    function ShowModalOptions(Frm: TWebBrowserProjectOptionsForm
      ): TModalResult; override;
    function ProjectDirSelected: boolean; virtual; // called after user selected the lpr
    function CreateProjectGroup(AProject: TLazProject; LPIFiles: array of string): boolean; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear; override;
    property ProjectDir: string read FProjectDir write FProjectDir;
    property WebDir: string read FWebDir write FWebDir;
    property LPGFilename: string read FLPGFilename write FLPGFilename;
    property Overwrites: TStrings read FOverwrites write SetOverwrites; // list of overwrite warnings
  end;

  { TProjectPas2JSProgressiveWebApp - project descriptor creating
    serviceworker project, web app project, project group, manifest and icons }

  TProjectPas2JSProgressiveWebApp = class(TMultiProjectPas2JSWebApp)
  private
    FCSSStyleFilename: string;
    FIconSizes: TWordDynArray;
    FImagesDir: string;
    FManifestFilename: string;
    FServiceWorker: TProjectPas2JSServiceWorker;
    FServiceWorkerLPR: string;
  protected
    procedure AddHTMLHead(Src: TStringList); override;
    function ProjectDirSelected: boolean; override;
    function CreateManifestFile(AProject: TLazProject; AFileName: String
      ): TLazProjectFile; virtual;
    function CreateCSSStyle(AProject: TLazProject; AFileName: String
      ): TLazProjectFile; virtual;
    function CopyFavIcon: boolean; virtual;
    function CopyIcons: boolean; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
    property ServiceWorkerLPR: string read FServiceWorkerLPR write FServiceWorkerLPR;
    property CSSStyleFilename: string read FCSSStyleFilename write FCSSStyleFilename;
    property ImagesDir: string read FImagesDir write FImagesDir;
    property ManifestFilename: string read FManifestFilename write FManifestFilename;
    property IconSizes: TWordDynArray read FIconSizes write FIconSizes;
    property ServiceWorker: TProjectPas2JSServiceWorker read FServiceWorker write FServiceWorker; // set by Register
  end;

  { TProjectPas2JSElectronWebApp - project descriptor creating
    preload-, render-, web app- project, project group, and package.json }

  TProjectPas2JSElectronWebApp = class(TMultiProjectPas2JSWebApp)
  private
    FPackageJSON: string;
    FPreloadLPI: string;
    FPreloadLPR: string;
    FRenderLPI: string;
    FRenderLPR: string;
  protected
    procedure AddHTMLHead(Src: TStringList); override;
    procedure AddBody(Src: TStringList); override;
    function ProjectDirSelected: boolean; override;
    function CreatePreloadProject(AProject: TLazProject): boolean; virtual;
    function CreateRenderProject(AProject: TLazProject): boolean; virtual;
    function CreateWebAppProject(AProject: TLazProject): boolean; virtual;
    function CreatePackageJSON(AProject: TLazProject): TLazProjectFile; virtual;
    function ShowModalOptions(Frm: TWebBrowserProjectOptionsForm
      ): TModalResult; override;
  public
    constructor Create; override;
    procedure Clear; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
    property PreloadLPR: string read FPreloadLPR write FPreloadLPR;
    property PreloadLPI: string read FPreloadLPI write FPreloadLPI;
    property RenderLPR: string read FRenderLPR write FRenderLPR;
    property RenderLPI: string read FRenderLPI write FRenderLPI;
    property PackageJSON: string read FPackageJSON write FPackageJSON;
    property LPGFilename: string read FLPGFilename write FLPGFilename;
  end;

  { TProjectPas2JSNodeJSApp }

  TNodeJSApplicationOption = (naoUseNodeJSApp);      // Use NodeJS app object
  TNodeJSApplicationOptions = set of TNodeJSApplicationOption;

  TProjectPas2JSNodeJSApp = class(TProjectDescriptor)
  private
    FOptions: TNodeJSApplicationOptions;
  protected
    function CreateProjectSource: String; virtual;
    function ShowOptionsDialog: TModalResult; virtual;
  public
    constructor Create; override;
    Function DoInitDescriptor : TModalResult; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
    property Options : TNodeJSApplicationOptions Read FOptions Write FOptions;
  end;

  TProjectPas2JSModuleApp = class(TProjectDescriptor)
  protected
    function CreateProjectSource: String; virtual;
  public
    constructor Create; override;
    Function DoInitDescriptor : TModalResult; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;

  { TPas2JSHTMLClassDef }

  TPas2JSHTMLClassDef = class(TFileDescPascalUnit)
  private
    FUseWebWidgets : Boolean;
    FOptions : THTML2ClassOptions;
    procedure DoConvLog(Sender: TObject; const Msg: String);
  public
    constructor Create; override;
    destructor destroy; override;
    Function Initialized(NewFile: TLazProjectFile): TModalResult; override;
    function Init(var {%H-}NewFilename: string; {%H-}NewOwner: TObject;
                  var {%H-}NewSource: string; {%H-}Quiet: boolean): TModalResult; override;
    function ShowOptionDialog : TModalResult;
    function CreateSource(const Filename, SourceName,
                          {%H-}ResourceName: string): string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
  end;

  { TPas2JSDTSToPasUnitDef }

  TPas2JSDTSToPasUnitDef = class(TFileDescPascalUnit)
  private
    FConverter : TCreateUnitFromDTS;
  public
    constructor Create; override;
    destructor destroy; override;
    function Init(var {%H-}NewFilename: string; {%H-}NewOwner: TObject;
                  var {%H-}NewSource: string; {%H-}Quiet: boolean): TModalResult; override;
    function CreateSource(const {%H-}Filename, SourceName,
                          {%H-}ResourceName: string): string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
  end;

var
  PJSOptionsFrameID: integer = 1000;

Const
  // Position in project options dialog.
  Pas2JSOptionsIndex  = ProjectOptionsMisc + 100;

function dbgs(opts: TBrowserApplicationOptions): string; overload;

procedure Register;

implementation

Var
  SrcMnuItem,PrjMnuItem,PrjMnuItemAll : TIDEmenuCommand;

Type

  { TPas2JSHandler }

  TPas2JSHandler = Class(TObject)
  protected
    function AskUserFile(aUnitName,aHTMLFileName: String): string; virtual;
    function RefreshHTML(aFile: TLazProjectFile; out aSource: String): Boolean; virtual;
  public
    procedure DoConvLog(Sender: TObject; const Msg: String); virtual;
    Procedure OnRefreshHTMLFormContext(Sender : TObject); virtual;
    Procedure OnRefreshProjHTMLFormContext(Sender : TObject); virtual;
    Procedure OnRefreshProjHTMLFormAllContext(Sender : TObject); virtual;
    Procedure OnSrcEditPopup(Sender : TObject); virtual;
    Procedure OnPrjInspPopup(Sender : TObject); virtual;
  end;

Var
  Pas2JSHTMLClassDef : TPas2JSHTMLClassDef;
  Pas2JSDTSToPasUnitDef : TPas2JSDTSToPasUnitDef;
  Pas2JSHandler : TPas2JSHandler;

function dbgs(opts: TBrowserApplicationOptions): string;
var
  s: String;
  o: TBrowserApplicationOption;
begin
  Result:='';
  for o in Opts do
    begin
    if Result<>'' then Result:=Result+',';
    s:='';
    str(o,s);
    Result:=Result+s;
    end;
  Result:='['+s+']';
end;

procedure Register;
Var
  SrvWorker: TProjectPas2JSServiceWorker;
  PWA: TProjectPas2JSProgressiveWebApp;
begin
  Pas2JSHandler:=TPas2JSHandler.Create;
  if Assigned(Pas2JSHandler) then; // Silence compiler warning
  PJSOptions:=TPas2jsOptions.Create;
  PJSOptions.Load;
  TPJSController.Instance.Hook;

  // register new-project items
  RegisterProjectDescriptor(TProjectPas2JSWebApp.Create);
  PWA:=TProjectPas2JSProgressiveWebApp.Create;
  RegisterProjectDescriptor(PWA);
  SrvWorker:=TProjectPas2JSServiceWorker.Create;
  RegisterProjectDescriptor(SrvWorker);
  PWA.ServiceWorker:=SrvWorker;
  RegisterProjectDescriptor(TProjectPas2JSElectronWebApp.Create);
  RegisterProjectDescriptor(TProjectPas2JSNodeJSApp.Create);
  RegisterProjectDescriptor(TProjectPas2JSModuleApp.Create);
  Pas2JSHTMLClassDef:=TPas2JSHTMLClassDef.Create;
  RegisterProjectFileDescriptor(Pas2JSHTMLClassDef);
  Pas2JSDTSToPasUnitDef:=TPas2JSDTSToPasUnitDef.Create;
  RegisterProjectFileDescriptor(Pas2JSDTSToPasUnitDef);

  // add IDE options frame
  PJSOptionsFrameID:=RegisterIDEOptionsEditor(GroupEnvironment,TPas2jsOptionsFrame,
                                              PJSOptionsFrameID)^.Index;

  // Add project options frame
  RegisterIDEOptionsEditor(GroupProject,TPas2JSProjectOptionsFrame, Pas2JSOptionsIndex);

  // pop menu items
  SrcMnuItem:=RegisterIDEMenuCommand(SrcEditMenuSectionFirstStatic,
     'HTMLFormClassRefresh', pjsRefreshClassFromHTML,@Pas2JSHandler.OnRefreshHTMLFormContext);
  SourceEditorMenuRoot.AddHandlerOnShow(@Pas2JSHandler.OnSrcEditPopup);
  PrjMnuItem:=RegisterIDEMenuCommand(ProjInspMenuSectionFiles,
      'PrjHTMLFormClassRefresh',pjsRefreshClassFromHTML,@Pas2JSHandler.OnRefreshProjHTMLFormContext);
  PrjMnuItemAll:=RegisterIDEMenuCommand(ProjInspMenuSectionFiles,
      'PrjHTMLFormClassRefreshAll',pjsRefreshAllClassesFromHTML,@Pas2JSHandler.OnRefreshProjHTMLFormAllContext);
  ProjectInspectorItemsMenuRoot.AddHandlerOnShow(@Pas2JSHandler.OnPrjInspPopup);
end;

{ TProjectPas2JSWebApp }

constructor TProjectPas2JSWebApp.Create;
begin
  inherited Create;
  Name:=ProjDescNamePas2JSWebApp;
  Clear;
end;

procedure TProjectPas2JSWebApp.Clear;
begin
  // Reset options
  Flags:=DefaultProjectNoApplicationFlags;
  FMainSrcName:='Project1';
  FOptions:=[baoCreateHtml,baoMaintainHTML,baoLocationOnSWS];
  ProjectPort:=0;
  ProjectURL:='';
end;

function TProjectPas2JSWebApp.GetNextPort : Word;

begin
  Result:=PJSOptions.StartAtPort;
  if Result>=$ffff then
    Result:=1024
  else
    inc(Result);
  PJSOptions.StartAtPort:=Result;
  PJSOptions.Save;
end;

function TProjectPas2JSWebApp.ShowOptionsDialog : TModalResult;

  Function Co(o : TBrowserApplicationOption) : boolean;

  begin
    Result:=O in Options;
  end;

  Procedure So(AValue : Boolean; o : TBrowserApplicationOption);

  begin
    if AValue then
      Include(Foptions,O)
    else
      Exclude(Foptions,O)
  end;

var
  Frm: TWebBrowserProjectOptionsForm;
begin
  Frm:=TWebBrowserProjectOptionsForm.Create(Nil);
  With Frm do
    try
      CreateHTML:=CO(baoCreateHtml);
      MaintainHTML:=CO(baoCreateHtml) and Co(baoMaintainHTML);
      UseRunOnReady:=CO(baoRunOnReady);
      ShowUncaughtExceptions:=CO(baoShowException);
      UseBrowserConsole:=CO(baoUseBrowserConsole);

      UseBrowserApp:=CO(baoUseBrowserApp);
      UseWASI:=CO(baoUseWASI);
      WasmProgramURL:='';

      UseModule:=CO(baoUseModule);

      if CO(baoLocationOnSWS) then
        RunLocation:=true
      else if CO(baoStartServer) then
        RunServerAtPort:=true
      else if CO(baoUseURL) then
        RunBrowserWithURL:=true
      else
        RunDefault:=true;

      // We allocate the new port in all cases.
      ServerPort:=GetNextPort;
      URL:='';

      Result:=ShowModalOptions(Frm);
      if Result=mrOK then
        begin
        SO(CreateHTML,baoCreateHtml);
        SO(MaintainHTML,baoCreateHtml);
        SO(UseRunOnReady,baoRunOnReady);
        SO(UseBrowserConsole,baoUseBrowserConsole);
        SO(ShowUncaughtExceptions,baoShowException);

        SO(UseBrowserApp,baoUseBrowserApp);
        SO(UseWASI,baoUseWASI);
        FProjectWasmURL:=WasmProgramURL;

        SO(UseModule,baoUseModule);

        SO(RunLocation,baoLocationOnSWS);
        SO(RunServerAtPort,baoStartServer);
        SO(RunBrowserWithURL,baoUseURL);
        if baoLocationOnSWS in FOptions then
          Self.ProjectLocation:=Location
        else if baoStartServer in FOptions then
          Self.ProjectPort:=ServerPort
        else if baoUseURL in FOptions then
          Self.ProjectURL:=URL;

        end;
    finally
      Free;
    end;
end;

function TProjectPas2JSWebApp.ShowModalOptions(
  Frm: TWebBrowserProjectOptionsForm): TModalResult;
begin
  Result:=Frm.ShowModal;
end;

procedure TProjectPas2JSWebApp.EnableRunBrowser(AProject: TLazProject);
begin
  SetDefaultWebRunParams(AProject.RunParameters.GetOrCreate('Default'));
  AProject.CustomData.Values[PJSProject]:='1';
  AProject.CustomData.Values[PJSProjectWebBrowser]:='1';
  if baoLocationOnSWS in Options then
    begin
    AProject.CustomData.Values[PJSProjectLocation]:=ProjectLocation;
    AProject.CustomData.Remove(PJSProjectPort);
    AProject.CustomData.Remove(PJSProjectURL);
    end
  else if baoUseURL in Options then
    begin
    AProject.CustomData.Remove(PJSProjectLocation);
    AProject.CustomData.Remove(PJSProjectPort);
    AProject.CustomData.Values[PJSProjectURL]:=ProjectURL;
    end
  else
    begin
    AProject.CustomData.Remove(PJSProjectLocation);
    AProject.CustomData.Values[PJSProjectPort]:=IntToStr(ProjectPort);
    AProject.CustomData.Remove(PJSProjectURL);
    end;
  With AProject.CustomData do
    begin
    DebugLn(['Info: (pas2jsdsgn) ',PJSProjectWebBrowser,': ',Values[PJSProjectWebBrowser]]);
    DebugLn(['Info: (pas2jsdsgn) ',PJSProjectLocation,': ',Values[PJSProjectLocation]]);
    DebugLn(['Info: (pas2jsdsgn) ',PJSProjectPort,': ',Values[PJSProjectPort]]);
    DebugLn(['Info: (pas2jsdsgn) ',PJSProjectURL,': ',Values[PJSProjectURL]]);
    end;
end;

function TProjectPas2JSWebApp.DoInitDescriptor: TModalResult;
begin
  Clear;
  Result:=ShowOptionsDialog;
end;

function TProjectPas2JSWebApp.GetLocalizedName: string;
begin
  Result:=pjsdWebApplication;
end;

function TProjectPas2JSWebApp.GetLocalizedDescription: string;
begin
  Result:=pjsdWebAppDescription;
end;

function TProjectPas2JSWebApp.GetMainSrcFileName: string;
begin
  if FMainSrcFileName='' then
    FMainSrcFileName:=AnsiLowerCase(MainSrcName)+'.lpr';
  Result:=FMainSrcFileName;
end;

function TProjectPas2JSWebApp.GetHTMLFilename: string;
begin
  if FHTMLFilename='' then
    FHTMLFilename:=ChangeFileExt(MainSrcFileName,'.html');
  Result:=FHTMLFilename;
end;

function TProjectPas2JSWebApp.GetMainSrcName: string;
begin
  if FMainSrcName='' then
    FMainSrcName:='project1';
  Result:=FMainSrcName;
end;

function TProjectPas2JSWebApp.GetScriptFilename: string;
begin
  if FScriptFilename='' then
    FScriptFilename:=ChangeFileExt(MainSrcFileName,'.js');
  Result:=FScriptFilename;
end;

procedure TProjectPas2JSWebApp.AddHTMLHead(Src: TStringList);
begin
  Src.Add('  <meta http-equiv="Content-type" content="text/html; charset=utf-8">');
  Src.Add('  <title>'+MainSrcName+'</title>');
  Src.Add('  <meta name="viewport" content="width=device-width, initial-scale=1">');
end;

procedure TProjectPas2JSWebApp.AddBody(Src: TStringList);
begin
  Src.Add('  <script>');
  if baoShowException in Options then
    Src.Add('    rtl.showUncaughtExceptions=true;');
  if baoRunOnReady in Options then
    Src.Add('    window.addEventListener("load", rtl.run);')
  else
    Src.Add('    rtl.run();');
  Src.Add('  </script>');
  if baoUseBrowserConsole in Options then
    Src.Add('  <div id="pasjsconsole"></div>');
end;

function TProjectPas2JSWebApp.CreateHTMLFile(AProject: TLazProject;
  aScriptFileName: String): TLazProjectFile;

Var
  HTMLFile: TLazProjectFile;
  ScriptType: String;
  Src: TStringList;
begin
  HTMLFile:=AProject.CreateProjectFile(HTMLFilename);
  HTMLFile.IsPartOfProject:=true;
  AProject.AddFile(HTMLFile,false);
  ScriptType:='';
  if baoUseModule in Options then
    ScriptType:=' type="module"';
  Src:=TStringList.Create;
  try
    Src.Add('<!doctype html>');
    Src.Add('<html lang="en">');
    Src.Add('<head>');
    AddHTMLHead(Src);
    Src.Add('  <script'+ScriptType+' src="'+aScriptFileName+'"></script>');
    Src.Add('</head>');
    Src.Add('<body>');
    AddBody(Src);
    Src.Add('</body>');
    Src.Add('</html>');

    HTMLFile.SetSourceText(Src.Text);
  finally
    Src.Free;
  end;

  HTMLFile.CustomData[PJSIsProjectHTMLFile]:='1';
  if baoMaintainHTML in Options then
    AProject.CustomData.Values[PJSProjectMaintainHTML]:='1';
  if baoUseBrowserConsole in Options then
    AProject.CustomData[PJSProjectUseBrowserConsole]:='1';
  if baoRunOnReady in options then
    AProject.CustomData[PJSProjectRunAtReady]:='1';

  Result:=HTMLFile;
end;

function TProjectPas2JSWebApp.CreateProjectSource : String;

Var
  Src : TStrings;
  units : string;

  Procedure Add(aLine : String);
  begin
    Src.Add(aLine);
  end;

  Procedure AddLn(aLine : String);
  begin
    if (aLine<>'') then
      aLine:=aLine+';';
    Add(aLine);
  end;

begin
  Units:='';
  if baoUseBrowserConsole in Options then
    Units:=' BrowserConsole,';
  if baoUseBrowserApp in Options then
    begin
    Units:=Units+' BrowserApp,' ;
    if baoUseWASI in options then
      Units:=Units+' WASIHostApp,' ;
    end;
  Units:=Units+' JS, Classes, SysUtils, Web';
  Src:=TStringList.Create;
  try
    // create program source
    AddLn('program '+MainSrcName);
    AddLn('');
    Add('{$mode objfpc}');
    Add('');
    Add('uses');
    AddLn(units) ;
    Add('');
    if baoUseBrowserApp in Options then
      begin
      Add('Type');
      if baoUseWASI in Options then
        Add('  TMyApplication = Class(TWASIHostApplication)')
      else
        Add('  TMyApplication = Class(TBrowserApplication)');
      Add('  protected');
      AddLn('    procedure DoRun; override');
      Add('  public');
      AddLn('  end');
      Add('');
      AddLn('Procedure TMyApplication.DoRun');
      Add('begin');
      if ServiceWorkerJSFilename<>'' then
        AddLn('  RegisterServiceWorker(''/ServiceWorker.js'')');
      if baoUseWASI in Options then
        begin
        if FProjectWasmURL='' then
          FProjectWasmURL:='yourwebassembly.wasm';
        AddLn(Format('  StartWebAssembly(''%s'')',[FProjectWasmURL]));
        end
      else
        Add('  // Your code here');
      AddLn('end');
      Add('');
      Add('var');
      AddLn('  Application : TMyApplication');
      Add('');
      end;
    Add('begin');
    if Not (baoUseBrowserApp in Options) then
      Add('  // Your code here')
    else
      begin
      AddLn('  Application:=TMyApplication.Create(Nil)');
      AddLn('  Application.Initialize');
      AddLn('  Application.Run');
      end;
    Add('end.');
    Result:=Src.Text;
  finally
    Src.Free;
  end;
end;

function TProjectPas2JSWebApp.InitProject(AProject: TLazProject): TModalResult;

var
  MainFile: TLazProjectFile;
  CompOpts: TLazCompilerOptions;

begin
  Result:=inherited InitProject(AProject);
  debugln(['Info: [TProjectPas2JSWebApp.InitProject] MainSrcFileName=',MainSrcFileName]);
  MainFile:=AProject.CreateProjectFile(MainSrcFileName);
  MainFile.IsPartOfProject:=true;
  AProject.AddFile(MainFile,false);
  AProject.MainFileID:=0;
  CompOpts:=AProject.LazCompilerOptions;
  SetDefaultWebCompileOptions(CompOpts);
  CompOpts.TargetFilename:=ExtractFileNameOnly(MainFile.Filename);
  if baoUseModule in Options then
    CompOpts.TargetOS:='module';
  AProject.MainFile.SetSourceText(CreateProjectSource,true);

  EnableRunBrowser(AProject);

  // create html source
  if baoCreateHtml in Options then
    begin
    debugln(['Info: [TProjectPas2JSWebApp.InitProject] HTMLFilename=',HTMLFilename]);
    CreateHTMLFile(AProject,ScriptFilename);
    end;
end;

function TProjectPas2JSWebApp.CreateStartFiles(AProject: TLazProject
  ): TModalResult;
var
  MainFile: TLazProjectFile;
begin
  // open lpr in source editor
  MainFile:=AProject.MainFile;
  Result:=LazarusIDE.DoOpenEditorFile(MainFile.Filename,-1,-1,
                                      [ofProjectLoading,ofRegularFile]);
  if Result<>mrOK then
     exit;

  // open html in source editor
  if baoCreateHtml in Options then
    Result:=LazarusIDE.DoOpenEditorFile(HTMLFilename,-1,-1,
                                        [ofProjectLoading,ofRegularFile]);
end;

{ TProjectPas2JSServiceWorker }

function TProjectPas2JSServiceWorker.CreateProjectSource: String;
var
  Src: TStringList;

  procedure Add(const s: string);
  begin
    Src.Add(s);
  end;

begin
  Src:=TStringList.Create;
  try
    Add('program ServiceWorker;');
    Add('');
    Add('{$mode objfpc}');
    Add('');
    Add('uses');
    Add('  Classes, ServiceWorkerApp;');
    Add('');
    Add('const');
    Add('  YourCacheName = ''v1''; // usually increased with every version');
    Add('   // The cache is specific to your domain, so no need to include your app name.');
    Add('');
    Add('type');
    Add('');
    Add('  { TApplication }');
    Add('');
    Add('  TApplication = class(TServiceWorkerApplication)');
    Add('  public');
    Add('    constructor Create(AOwner: TComponent); override;');
    Add('  end;');
    Add('');
    Add('{ TApplication }');
    Add('');
    Add('constructor TApplication.Create(AOwner: TComponent);');
    Add('begin');
    Add('  inherited Create(AOwner);');
    Add('');
    Add('  FCacheName:=YourCacheName;');
    Add('  FResources:=[');
    Add('    ''/images/error.png''');
    Add('    ];');
    Add('  FallbackURL := ''/images/error.png'';');
    Add('end;');
    Add('');
    Add('var');
    Add('  App: TApplication;');
    Add('begin');
    Add('  App:=TApplication.Create(nil);');
    Add('  App.Run;');
    Add('end.');
    Result:=Src.Text;
  finally
    Src.Free;
  end;
end;

function TProjectPas2JSServiceWorker.InitServiceWorkerProject(
  AProject: TLazProject; LPRFilename: string): TModalResult;
var
  MainFile: TLazProjectFile;
  CompOpts: TLazCompilerOptions;
begin
  MainFile:=AProject.CreateProjectFile(LPRFilename);
  MainFile.IsPartOfProject:=true;
  AProject.AddFile(MainFile,false);
  AProject.MainFileID:=0;
  CompOpts:=AProject.LazCompilerOptions;
  SetDefaultNodeJSCompileOptions(CompOpts);
  CompOpts.TargetFilename:=ExtractFileNameOnly(LPRFilename);
  SetDefaultServiceWorkerRunParams(AProject.RunParameters.GetOrCreate('Default'));
  AProject.MainFile.SetSourceText(CreateProjectSource,true);
  AProject.CustomData.Values[PJSProject]:='1';
  Result:=mrOk;
end;

constructor TProjectPas2JSServiceWorker.Create;
begin
  inherited Create;
  Name:=ProjDescNamePas2JSServiceWorker;
  Flags:=DefaultProjectNoApplicationFlags-[pfRunnable];
end;

function TProjectPas2JSServiceWorker.GetLocalizedName: string;
begin
  Result:=pjsdServiceWorker;
end;

function TProjectPas2JSServiceWorker.GetLocalizedDescription: string;
begin
  Result:=pjsdServiceWorkerDescription;
end;

function TProjectPas2JSServiceWorker.InitProject(AProject: TLazProject
  ): TModalResult;
begin
  Result:=inherited InitProject(AProject);
  if Result<>mrOk then
    exit;
  Result:=InitServiceWorkerProject(AProject,'ServiceWorker.lpr');
end;

function TProjectPas2JSServiceWorker.CreateStartFiles(AProject: TLazProject
  ): TModalResult;
begin
  Result:=LazarusIDE.DoOpenEditorFile(AProject.MainFile.Filename,-1,-1,
                                      [ofProjectLoading,ofRegularFile]);
  if Result<>mrOK then
     exit;
end;

{ TMultiProjectPas2JSWebApp }

procedure TMultiProjectPas2JSWebApp.SetOverwrites(const AValue: TStrings);
begin
  if FOverwrites=AValue then Exit;
  FOverwrites.Assign(AValue);
end;

function TMultiProjectPas2JSWebApp.CheckOverwriteFile(aFilename: string
  ): string;
begin
  if (Overwrites.IndexOf(aFilename)<0) and FileExists(aFilename) then
    Overwrites.Add(aFilename);
  Result:=aFilename;
end;

function TMultiProjectPas2JSWebApp.CheckOverwriteDir(aDir: string): string;
begin
  aDir:=ChompPathDelim(aDir);
  if (Overwrites.IndexOf(aDir)<0) and FileExists(aDir) and not DirectoryExistsUTF8(aDir) then
    Overwrites.Add(aDir);
  Result:=AppendPathDelim(aDir);
end;

function TMultiProjectPas2JSWebApp.FileToWebFile(aFilename: string): string;
begin
  Result:=CreateRelativePath(aFilename,WebDir);
  Result:=FilenameToURLPath(Result);
end;

function TMultiProjectPas2JSWebApp.InteractiveForceDir(Dir: string;
  AutoDelete: boolean): boolean;
begin
  Dir:=ChompPathDelim(Dir);
  if DirectoryExistsUTF8(Dir) then
    exit(true);
  Result:=false;
  if FileExists(Dir) then
    begin
    if AutoDelete then
      begin
      debugln(['Info: [TMultiProjectPas2JSWebApp.InteractiveForceDir] DeleteFile "',Dir,'"']);
      if not DeleteFileUTF8(Dir) then
        begin
        IDEMessageDialog('Error','Unable to create directory "'+Dir+'", because unable to delete file.',mtError,[mbOK]);
        exit;
        end;
      end
    else
      begin
      IDEMessageDialog('Error','Unable to create directory "'+Dir+'", because file already exists.',mtError,[mbOK]);
      exit;
      end;
    end;
  debugln(['Info: [TMultiProjectPas2JSWebApp.InteractiveForceDir] ForceDirectories "',Dir,'"']);
  if not ForceDirectoriesUTF8(Dir) then
    begin
    IDEMessageDialog('Error','Unable to create directory "'+Dir+'".',mtError,[mbOK]);
    exit;
    end;
  Result:=true;
end;

function TMultiProjectPas2JSWebApp.InteractiveSaveFile(aFilename: string
  ): boolean;
var
  Code: TCodeBuffer;
begin
  Result:=false;
  Code:=CodeToolBoss.FindFile(aFilename);
  if Code=nil then
    begin
    debugln(['Error: [TMultiProjectPas2JSWebApp.SaveFile] 20220404130903 "',aFilename,'"']);
    IDEMessageDialog('Error','File missing in codetools: "'+aFilename+'"',mtError,[mbOk]);
    exit;
    end;
  debugln(['Info: [TMultiProjectPas2JSWebApp.InteractiveSaveFile] saving "',Code.Filename,'"']);
  if not Code.Save then
    begin
    IDEMessageDialog('Error','Unable to write file "'+aFilename+'"',mtError,[mbOk]);
    exit;
    end;
  Result:=true;
end;

function TMultiProjectPas2JSWebApp.InteractiveCopyFile(Src, Dest: string
  ): boolean;
begin
  debugln(['Info: [TMultiProjectPas2JSWebApp.InteractiveCopyFile] CopyFile "',Src,'" -> "',Dest,'"']);
  if CopyFile(Src,Dest) then
    exit(true);
  IDEMessageDialog('Error','Unable to copy file'+sLineBreak
    +Src+sLineBreak
    +'to'+sLineBreak
    +Dest,mtError,[mbOk]);
  Result:=false;
end;

function TMultiProjectPas2JSWebApp.ShowModalOptions(
  Frm: TWebBrowserProjectOptionsForm): TModalResult;
var
  CurProjDir: String;
  SaveDlg: TIDESaveDialog;
begin
  // hide unsupported options
  Frm.HideWASM;
  Frm.HideModule;
  Frm.HideRunLocation;

  Frm.Caption:=GetLocalizedName+' Options';
  Result:=inherited ShowModalOptions(Frm);
  if Result<>mrOk then exit;

  SaveDlg:=IDESaveDialogClass.Create(nil);
  try
    InitIDEFileDialog(SaveDlg);
    SaveDlg.Title:=pjsdNewProjectFile+' (.lpr)';
    SaveDlg.Filter:=pjsdProjectPascalFile+' (*.lpr;*.pas)|*.lpr;*.pas';

    if not SaveDlg.Execute then
      exit(mrCancel);

    MainSrcFileName:=SaveDlg.FileName;
    if not FilenameIsAbsolute(MainSrcFileName) then
      begin
      IDEMessageDialog(pjsdError, pjsdPleaseChooseAFileWithFullPath,
        mtError, [mbOk]);
      exit(mrCancel);
      end;

    MainSrcFileName:=CleanAndExpandFilename(MainSrcFileName);
    if CompareFileExt(MainSrcFileName,'.lpi')=0 then
      MainSrcFileName:=ChangeFileExt(MainSrcFileName,'.lpr');
    if ExtractFileExt(MainSrcFileName)='' then
      MainSrcFileName:=MainSrcFileName+'.lpr';
    if CompareFilenames(ExtractFileNameOnly(MainSrcFileName),MainSrcName)<>0 then
      MainSrcName:=ExtractFileNameOnly(MainSrcFileName);
    CurProjDir:=ExtractFilePath(MainSrcFileName);
    if CurProjDir='' then exit(mrCancel);
    CurProjDir:=AppendPathDelim(CurProjDir);
    ProjectDir:=CurProjDir;

    ProjectDirSelected;

    if Overwrites.Count>0 then
      begin
      if IDEMessageDialog(pjsdOverwrite, pjsdOverwriteFiles+sLineBreak+
        Overwrites.Text,
          mtConfirmation,[mbOk,mbCancel])<>mrOk then
        exit(mrCancel);
      end;

  finally
    SaveDlg.Free;
  end;
end;

function TMultiProjectPas2JSWebApp.ProjectDirSelected: boolean;
begin
  Result:=true;
  ScriptFilename:=ExtractFileNameOnly(MainSrcFileName)+'.js';
  CheckOverwriteFile(MainSrcFileName);
  CheckOverwriteFile(ChangeFileExt(MainSrcFileName,'.lpi'));
  LPGFilename:=CheckOverwriteFile(ChangeFileExt(MainSrcFileName,'.lpg'));
end;

function TMultiProjectPas2JSWebApp.CreateProjectGroup(AProject: TLazProject;
  LPIFiles: array of string): boolean;
var
  Grp: TProjectGroup;
  i: integer;
begin
  Result:=false;

  if AProject=nil then ;

  if not IDEProjectGroupManager.NewProjectGroup(false) then
  begin
    debugln(['Error: TMultiProjectPas2JSWebApp.CreateProjectGroup failed to create new project group ']);
    exit;
  end;
  Grp:=IDEProjectGroupManager.CurrentProjectGroup;
  Grp.FileName:=LPGFilename;

  for i:=low(LPIFiles) to high(LPIFiles) do
    Grp.AddTarget(LPIFiles[i]);

  if not IDEProjectGroupManager.SaveProjectGroup then
  begin
    debugln(['Error: TMultiProjectPas2JSWebApp.CreateProjectGroup failed writing project group "',Grp.FileName,'"']);
    exit;
  end;

  Result:=true;
end;

constructor TMultiProjectPas2JSWebApp.Create;
begin
  FOverwrites:=TStringList.Create;
  inherited Create;
end;

destructor TMultiProjectPas2JSWebApp.Destroy;
begin
  FreeAndNil(FOverwrites);
  inherited Destroy;
end;

procedure TMultiProjectPas2JSWebApp.Clear;
begin
  inherited Clear;
  FProjectDir:='';
  FWebDir:='';
  FLPGFilename:='quickstart.lpg';
  if FOverwrites<>nil then
    FOverwrites.Clear;
end;

{ TProjectPas2JSProgressiveWebApp }

procedure TProjectPas2JSProgressiveWebApp.AddHTMLHead(Src: TStringList);
var
  i: Integer;
  h, CurImgDir: String;
begin
  inherited AddHTMLHead(Src);
  Src.Add('  <link rel="stylesheet" href="'+FileToWebFile(CSSStyleFilename)+'" />');
  Src.Add('  <link rel="manifest" href="'+FileToWebFile(ManifestFilename)+'" />');
  Src.Add('  <meta name="apple-mobile-web-app-status-bar" content="#d04010" />');
  Src.Add('  <meta name="theme-color" content="#d04010" />');
  CurImgDir:=FileToWebFile(ChompPathDelim(ImagesDir));
  for i:=0 to high(IconSizes) do
    begin
    h:=IntToStr(IconSizes[i]);
    Src.Add('  <link rel="apple-touch-icon" href="'+CurImgDir+'/icon-'+h+'x'+h+'.png" />');
    end;
end;

function TProjectPas2JSProgressiveWebApp.ProjectDirSelected: boolean;
begin
  Result:=inherited ProjectDirSelected;

  ServiceWorkerLPR:=CheckOverwriteFile(ProjectDir+ServiceWorkerLPR);
  CheckOverwriteFile(ChangeFileExt(ServiceWorkerLPR,'.lpi'));

  WebDir:=CheckOverwriteDir(ProjectDir+WebDir);
  ImagesDir:=CheckOverwriteDir(WebDir+ImagesDir);

  HTMLFilename:=CheckOverwriteFile(WebDir+HTMLFilename);
  ManifestFilename:=CheckOverwriteFile(WebDir+ManifestFilename);
  CSSStyleFilename:=CheckOverwriteFile(WebDir+CSSStyleFilename);
end;

function TProjectPas2JSProgressiveWebApp.CreateManifestFile(
  AProject: TLazProject; AFileName: String): TLazProjectFile;
var
  Src: TStringList;
  i: Integer;
  h, ImgDir: String;
begin
  //debugln(['Info: [TProjectPas2JSProgressiveWebApp.CreateManifestFile] "',AFileName,'"']);
  Result:=AProject.CreateProjectFile(AFileName);
  Result.IsPartOfProject:=true;
  AProject.AddFile(Result,false);
  Src:=TStringList.Create;
  try
    Src.Add('{');
    Src.Add('  "name": "Your application name",');
    Src.Add('  "short_name": "Max12CharsSN",');
    Src.Add('  "start_url": "'+ExtractFileName(HTMLFilename)+'",');
    Src.Add('  "display": "standalone",');
    Src.Add('  "background_color": "#f0f0f0",');
    Src.Add('  "theme_color": "#d04030",');
    Src.Add('  "orientation": "portrait-primary",');
    Src.Add('  "icons": [');
    ImgDir:=CreateRelativePath(ChompPathDelim(ImagesDir),WebDir);
    for i:=0 to high(IconSizes) do
      begin
      h:=IntToStr(IconSizes[i]);
      h:=h+'x'+h;
      Src.Add('    {');
      Src.Add('      "src": "/'+ImgDir+'/icon-'+h+'.png",');
      Src.Add('      "type": "image/png", "sizes": "'+h+'"');
      h:='    }';
      if i<High(IconSizes) then
        h:=h+',';
      Src.Add(h);
      end;
    Src.Add('  ]');
    Src.Add('}');
    Result.SetSourceText(Src.Text);
  finally
    Src.Free;
  end;
end;

function TProjectPas2JSProgressiveWebApp.CreateCSSStyle(AProject: TLazProject;
  AFileName: String): TLazProjectFile;
var
  Src: TStringList;
begin
  //debugln(['Info: [TProjectPas2JSProgressiveWebApp.CreateCSSStyle] "',AFileName,'"']);
  Result:=AProject.CreateProjectFile(AFileName);
  Result.IsPartOfProject:=true;
  AProject.AddFile(Result,false);
  Src:=TStringList.Create;
  try
    Src.Add('body {');
    Src.Add('  background: #f0f0f0;');
    Src.Add('  font-family: "Arial";');
    Src.Add('  font-size: 1rem;');
    Src.Add('}');
    Result.SetSourceText(Src.Text);
  finally
    Src.Free;
  end;
end;

function TProjectPas2JSProgressiveWebApp.CopyFavIcon: boolean;
var
  FavIconFilename, SrcFilename: String;
begin
  Result:=false;
  // favicon.ico
  FavIconFilename:=WebDir+'favicon.ico';
  if not FileExistsUTF8(FavIconFilename) then
    begin
    SrcFilename:='$(LazarusDir)';
    IDEMacros.SubstituteMacros(SrcFilename);
    SrcFilename:=AppendPathDelim(SrcFilename)+'images'+PathDelim+'mainiconproject.ico';
    if not InteractiveCopyFile(SrcFilename,FavIconFilename) then exit;
    end;
  Result:=true;
end;

function TProjectPas2JSProgressiveWebApp.CopyIcons: boolean;
var
  i: Integer;
  Pkg: TIDEPackage;
  SrcDir, DstDir, h: String;
begin
  // copy lazarus/components/pas2jsdsgn/images/images/icon-*x*.png to ImagesDir
  Result:=false;
  Pkg:=PackageEditingInterface.FindPackageWithName('pas2jsdsgn');
  if Pkg=nil then
    begin
    IDEMessageDialog('Error','Where am I? Missing package pas2jsdsgn',mtError,[mbOk]);
    exit;
    end;
  SrcDir:=AppendPathDelim(Pkg.DirectoryExpanded)+'images'+PathDelim;
  DstDir:=AppendPathDelim(ImagesDir);
  for i:=0 to high(IconSizes) do
    begin
    h:=IntToStr(IconSizes[i]);
    h:='icon-'+h+'x'+h+'.png';
    if not InteractiveCopyFile(SrcDir+h,DstDir+h) then exit;
    end;
  Result:=true;
end;

constructor TProjectPas2JSProgressiveWebApp.Create;
begin
  inherited Create;
  Name:=ProjDescNamePas2JSProgressiveWebApp;
end;

destructor TProjectPas2JSProgressiveWebApp.Destroy;
begin
  inherited Destroy;
end;

procedure TProjectPas2JSProgressiveWebApp.Clear;
var
  i: Integer;
begin
  inherited Clear;
  FOptions:=[baoCreateHtml,baoMaintainHTML,baoUseBrowserApp,baoStartServer];
  FWebDir:='www';
  FImagesDir:='images';
  FHTMLFilename:='index.html';
  FManifestFilename:='manifest.json';
  FCSSStyleFilename:='style.css';
  FServiceWorkerLPR:='ServiceWorker.lpr';
  FServiceWorkerJSFilename:='/ServiceWorker.js';
  FLPGFilename:='pwa1.lpg';
  SetLength(FIconSizes,length(DefaultIconSizes));
  for i:=0 to high(DefaultIconSizes) do
    FIconSizes[i]:=DefaultIconSizes[i];
  FOverwrites.Clear;
end;

function TProjectPas2JSProgressiveWebApp.GetLocalizedName: string;
begin
  Result:=pjsdProgressiveWebApplication;
end;

function TProjectPas2JSProgressiveWebApp.GetLocalizedDescription: string;
begin
  Result:=pjsdProgressiveWebAppDescription;
end;

function TProjectPas2JSProgressiveWebApp.InitProject(AProject: TLazProject
  ): TModalResult;
var
  CurProjDir: String;
begin
  Result:=mrCancel;
  CurProjDir:=ChompPathDelim(ProjectDir);

  // create directories
  if not FilenameIsAbsolute(CurProjDir) then
    begin
    debugln(['Error (pas2jsdsgn): [20220403220423] TProjectPas2JSProgressiveWebApp.InitProject project directory not absolute: '+CurProjDir]);
    exit;
    end;
  if not InteractiveForceDir(CurProjDir,false) then
    begin
    IDEMessageDialog('Error','Unable to create directory "'+CurProjDir+'".',mtError,[mbOK]);
    exit;
    end;
  CurProjDir:=AppendPathDelim(CurProjDir);

  if not InteractiveForceDir(WebDir,true) then exit;
  if not InteractiveForceDir(ImagesDir,true) then exit;

  // create service worker project
  AProject.ProjectInfoFile:=ChangeFileExt(ServiceWorkerLPR,'.lpi');
  Result:=ServiceWorker.InitServiceWorkerProject(AProject,ServiceWorkerLPR);
  if Result<>mrOk then exit;

  Result:=mrOk;
end;

function TProjectPas2JSProgressiveWebApp.CreateStartFiles(AProject: TLazProject
  ): TModalResult;
var
  CompOpts: TLazCompilerOptions;
  TargetFilename, MainFilename: String;
begin
  Result:=mrCancel;

  // Note: InitProject has initialized the ServiceWorker project
  CompOpts:=AProject.LazCompilerOptions;
  TargetFilename:=AppendPathDelim(WebDir)+'ServiceWorker';

  // save ServiceWorker.lpr and open it in source editor
  if not InteractiveSaveFile(ServiceWorkerLPR) then exit;
  LazarusIDE.DoOpenEditorFile(ServiceWorkerLPR,-1,-1,
                                      [ofProjectLoading,ofRegularFile]);
  // save ServiceWorker.lpi
  if LazarusIDE.DoSaveProject([sfQuietUnitCheck])<>mrOk then exit;

  // delete ServiceWorker.lpr from project
  LazarusIDE.DoCloseEditorFile(ServiceWorkerLPR,[cfQuiet,cfProjectClosing]);
  if AProject.MainFileID<>0 then
    raise Exception.Create('20220405231537');
  AProject.MainFileID:=-1;
  AProject.RemoveUnit(0,false);

  // initialize PWA project
  AProject.ProjectInfoFile:=ChangeFileExt(MainSrcFileName,'.lpi');
  // create PWA lpr and index.html
  if inherited InitProject(AProject)<>mrOk then exit;
  // make sure lpi has same case as lpr
  MainFilename:=AProject.MainFile.Filename;
  AProject.ProjectInfoFile:=ChangeFileExt(MainFilename,'.lpi');

  // set PWA targetfilename
  TargetFilename:=AppendPathDelim(WebDir)+MainSrcName;
  CompOpts.TargetFilename:=TargetFilename;

  // save index.html
  if baoCreateHtml in Options then
    begin
    if not InteractiveSaveFile(HTMLFilename) then exit;
    end;

  // open lpr and index.html in source editor
  if inherited CreateStartFiles(AProject)<>mrOk then exit;

  // manifest.json
  CreateManifestFile(AProject,ManifestFilename);
  if not InteractiveSaveFile(ManifestFilename) then exit;

  // style.css
  CreateCSSStyle(AProject,CSSStyleFilename);
  if not InteractiveSaveFile(CSSStyleFilename) then exit;

  if not CopyFavIcon then exit;
  if not CopyIcons then exit;

  // save lpr
  if not InteractiveSaveFile(MainFilename) then exit;

  // save lpi
  if LazarusIDE.DoSaveProject([sfQuietUnitCheck])<>mrOk then exit;

  // project group
  if not CreateProjectGroup(AProject,[
    AProject.ProjectInfoFile,
    ChangeFileExt(ServiceWorkerLPR,'.lpi')
    ]) then exit;

  Result:=mrOk;
end;

{ TProjectPas2JSElectronWebApp }

procedure TProjectPas2JSElectronWebApp.AddHTMLHead(Src: TStringList);
begin
  inherited AddHTMLHead(Src);
  Src.Add('  <!-- https://developer.mozilla.org/en-US/docs/Web/HTTP/CSP -->');
  Src.Add('  <meta http-equiv="Content-Security-Policy" content="default-src ''self''; script-src ''self'' ''unsafe-inline''">');
  Src.Add('  <meta http-equiv="X-Content-Security-Policy" content="default-src ''self''; script-src ''self'' ''unsafe-inline''">');
  Src.Add('');
end;

procedure TProjectPas2JSElectronWebApp.AddBody(Src: TStringList);
begin
  Src.Add('  <h1>Hello World!</h1>');
  Src.Add('  <p>We are using Pas2JS <b><span id="pas2js-version"></span></b>,</p>');
  Src.Add('  <p>Node <b><span id="node-version"></span></b>,</p>');
  Src.Add('  <p>Chromium <b><span id="chrome-version"></span></b>,</p>');
  Src.Add('  <p>and Electron <b><span id="electron-version"></span></b>.</p>');
  Src.Add('  <p>Additionally, <span id="renderertext"></span></p>');
  inherited AddBody(Src);
end;

function TProjectPas2JSElectronWebApp.ProjectDirSelected: boolean;
begin
  Result:=inherited ProjectDirSelected;

  WebDir:=ProjectDir;

  PreloadLPR:=CheckOverwriteFile(ProjectDir+PreloadLPR);
  PreloadLPI:=CheckOverwriteFile(ChangeFileExt(PreloadLPR,'.lpi'));

  RenderLPR:=CheckOverwriteFile(ProjectDir+RenderLPR);
  RenderLPI:=CheckOverwriteFile(ChangeFileExt(RenderLPR,'.lpi'));

  HTMLFilename:=CheckOverwriteFile(ProjectDir+HTMLFilename);
  PackageJSON:=CheckOverwriteFile(ProjectDir+PackageJSON);
end;

function TProjectPas2JSElectronWebApp.CreatePreloadProject(AProject: TLazProject
  ): boolean;
var
  MainFile: TLazProjectFile;
  CompOpts: TLazCompilerOptions;
  Src: TStringList;
begin
  Result:=false;

  AProject.CustomData.Values[PJSProject]:='1';

  MainFile:=AProject.CreateProjectFile(PreloadLPR);
  MainFile.IsPartOfProject:=true;
  AProject.AddFile(MainFile,false);
  AProject.MainFileID:=0;
  CompOpts:=AProject.LazCompilerOptions;
  SetDefaultNodeJSCompileOptions(CompOpts);
  CompOpts.TargetFilename:=ExtractFileNameOnly(PreloadLPR);

  Src:=TStringList.Create;
  try
    Src.Add('program '+ExtractFileNameOnly(PreloadLPR)+';');
    Src.Add('');
    Src.Add('{$mode objfpc}');
    Src.Add('');
    Src.Add('uses');
    Src.Add('  JS, Classes, SysUtils, Web, libelectron, nodejs;');
    Src.Add('');
    Src.Add('procedure DoRun(event : TJSEvent);');
    Src.Add('');
    Src.Add('  Procedure ReplaceText(const aSelector, aText : String);');
    Src.Add('  Var');
    Src.Add('    el : TJSHTMLElement;');
    Src.Add('  begin');
    Src.Add('    el:=TJSHTMLElement(Document.getElementById(aSelector));');
    Src.Add('    if Assigned(el) then');
    Src.Add('      el.InnerText:=aText;');
    Src.Add('  end;');
    Src.Add('');
    Src.Add('begin');
    Src.Add('  ReplaceText(''pas2js-version'',{$i %PAS2JSVERSION%});');
    Src.Add('  ReplaceText(''chrome-version'',String(TNJSProcess.versions[''chrome'']));');
    Src.Add('  ReplaceText(''electron-version'',String(TNJSProcess.versions[''electron'']));');
    Src.Add('  ReplaceText(''node-version'',String(TNJSProcess.versions[''node'']));');
    Src.Add('end;');
    Src.Add('');
    Src.Add('begin');
    Src.Add('  console.log(''preload environment start'');');
    Src.Add('  console.log(electron);');
    Src.Add('  console.log(''preload environment done'');');
    Src.Add('  window.addEventListener(''DOMContentLoaded'',@DoRun);');
    Src.Add('end.');
    MainFile.SetSourceText(Src.Text,true);
  finally
    Src.Free;
  end;

  // save preload.lpr and open it in source editor
  if not InteractiveSaveFile(PreloadLPR) then exit;
  if LazarusIDE.DoOpenEditorFile(PreloadLPR,-1,-1,[ofProjectLoading,ofRegularFile])<>mrOk then exit;

  // save preload.lpi
  if LazarusIDE.DoSaveProject([sfQuietUnitCheck])<>mrOk then exit;

  // delete preload.lpr from project
  LazarusIDE.DoCloseEditorFile(PreloadLPR,[cfQuiet,cfProjectClosing]);
  if AProject.MainFileID<>0 then
    raise Exception.Create('20220418140809');
  AProject.MainFileID:=-1;
  AProject.RemoveUnit(0,false);

  Result:=true;
end;

function TProjectPas2JSElectronWebApp.CreateRenderProject(AProject: TLazProject
  ): boolean;
var
  MainFile: TLazProjectFile;
  CompOpts: TLazCompilerOptions;
  Src: TStringList;
begin
  Result:=false;

  AProject.ProjectInfoFile:=RenderLPI;
  AProject.CustomData.Values[PJSProject]:='1';

  MainFile:=AProject.CreateProjectFile(RenderLPR);
  MainFile.IsPartOfProject:=true;
  AProject.AddFile(MainFile,false);
  AProject.MainFileID:=0;
  CompOpts:=AProject.LazCompilerOptions;
  SetDefaultWebCompileOptions(CompOpts);
  CompOpts.TargetFilename:=ExtractFileNameOnly(RenderLPR);

  Src:=TStringList.Create;
  try
    Src.Add('program '+ExtractFileNameOnly(RenderLPR)+';');
    Src.Add('');
    Src.Add('{$mode objfpc}');
    Src.Add('');
    Src.Add('uses');
    Src.Add('  JS, Web;');
    Src.Add('');
    Src.Add('Var');
    Src.Add('  el : TJSHTMLElement;');
    Src.Add('');
    Src.Add('begin');
    Src.Add('  el:=Document.getHTMLElementById(''renderertext'');');
    Src.Add('  el.innerHTML:=''This text was produced in the Electron Renderer process using Pas2JS version <b>''+{$i %PAS2JSVERSION%}+''</b>'';');
    Src.Add('end.');
    MainFile.SetSourceText(Src.Text,true);
  finally
    Src.Free;
  end;

  // save render.lpr and open it in source editor
  if not InteractiveSaveFile(RenderLPR) then exit;
  if LazarusIDE.DoOpenEditorFile(RenderLPR,-1,-1,[ofProjectLoading,ofRegularFile])<>mrOk then exit;

  // save render.lpi
  if LazarusIDE.DoSaveProject([sfQuietUnitCheck])<>mrOk then exit;

  // delete render.lpr from project
  LazarusIDE.DoCloseEditorFile(RenderLPR,[cfQuiet,cfProjectClosing]);
  if AProject.MainFileID<>0 then
    raise Exception.Create('20220418141228');
  AProject.MainFileID:=-1;
  AProject.RemoveUnit(0,false);

  Result:=true;
end;

function TProjectPas2JSElectronWebApp.CreateWebAppProject(AProject: TLazProject
  ): boolean;
var
  MainFile: TLazProjectFile;
  CompOpts: TLazCompilerOptions;
  PreloadJS, Units: String;
  Src: TStringList;
begin
  Result:=false;

  PreloadJS:=ChangeFileExt(PreloadLPR,'.js');

  AProject.ProjectInfoFile:=ChangeFileExt(MainSrcFileName,'.lpi');
  AProject.CustomData.Values[PJSProject]:='1';

  MainFile:=AProject.CreateProjectFile(MainSrcFileName);
  MainFile.IsPartOfProject:=true;
  AProject.AddFile(MainFile,false);
  AProject.MainFileID:=0;
  CompOpts:=AProject.LazCompilerOptions;
  SetDefaultNodeJSCompileOptions(CompOpts);
  CompOpts.TargetFilename:=ExtractFileNameOnly(MainSrcName);

  Units:='';
  if baoUseBrowserConsole in Options then
    Units:=' BrowserConsole,';

  Src:=TStringList.Create;
  try
    Src.Add('program '+ExtractFileNameOnly(MainSrcName)+';');
    Src.Add('');
    Src.Add('{$mode objfpc}');
    Src.Add('');
    Src.Add('uses');
    Src.Add('  js, nodejs, node.fs,'+Units+' libelectron;');
    Src.Add('');
    Src.Add('Procedure createWindow(event : TEvent; accessibilitySupportEnabled : boolean);');
    Src.Add('Var');
    Src.Add('  opts : TBrowserWindowConstructorOptions;');
    Src.Add('  win : TBrowserWindow;');
    Src.Add('begin');
    Src.Add('  opts:=TBrowserWindowConstructorOptions.new;');
    Src.Add('  opts.width:=800;');
    Src.Add('  opts.height:=600;');
    Src.Add('  opts.webPreferences:=TWebPreferences.New;');
    Src.Add('  opts.webPreferences.preload:=NJS_Path.join(__dirname,'''+FileToWebFile(PreloadJS)+''');');
    Src.Add('  win:=Electron.TBrowserWindow.new(opts);');
    Src.Add('  win.loadFile(''index.html'');');
    Src.Add('end;');
    Src.Add('');
    Src.Add('begin');
    Src.Add('  electron.app.addListener(''ready'',@CreateWindow);');
    Src.Add('end.');
    MainFile.SetSourceText(Src.Text,false);
  finally
    Src.Free;
  end;

  // save lpr and open it in source editor
  if not InteractiveSaveFile(MainSrcFileName) then exit;
  if LazarusIDE.DoOpenEditorFile(MainSrcFileName,-1,-1,[ofProjectLoading,ofRegularFile])<>mrOk then exit;

  // save render.lpi
  if LazarusIDE.DoSaveProject([sfQuietUnitCheck])<>mrOk then exit;

  Result:=true;
end;

function TProjectPas2JSElectronWebApp.CreatePackageJSON(AProject: TLazProject
  ): TLazProjectFile;
var
  aFile: TLazProjectFile;
  Src: TStringList;
begin
  Result:=nil;
  aFile:=AProject.CreateProjectFile(PackageJSON);
  aFile.IsPartOfProject:=true;
  AProject.AddFile(aFile,false);

  Src:=TStringList.Create;
  try
    Src.Add('{');
    Src.Add('  "name": "quickstart-electron-app",');
    Src.Add('  "version": "1.0.0",');
    Src.Add('  "description": "Hello World!",');
    Src.Add('  "main": "'+FileToWebFile(ScriptFilename)+'",');
    Src.Add('  "author": "Lazarus - www.lazarus-ide.org",');
    Src.Add('  "license": "LGPL2",');
    Src.Add('  "scripts": {');
    Src.Add('    "start": "electron ."');
    Src.Add('  }');
    Src.Add('}');
    aFile.SetSourceText(Src.Text,false);
  finally
    Src.Free;
  end;

  if not InteractiveSaveFile(PackageJSON) then exit;
  if LazarusIDE.DoOpenEditorFile(PackageJSON,-1,-1,[ofProjectLoading,ofRegularFile])<>mrOk then exit;

  Result:=aFile;
end;

function TProjectPas2JSElectronWebApp.ShowModalOptions(
  Frm: TWebBrowserProjectOptionsForm): TModalResult;
begin
  Frm.HideRunOnReady;
  Frm.HideUseBrowserApp;
  Frm.HideRunHTTPServer;
  Result:=inherited ShowModalOptions(Frm);
end;

constructor TProjectPas2JSElectronWebApp.Create;
begin
  inherited Create;
  Name:=ProjDescNamePas2JSElectronWebApp;
end;

procedure TProjectPas2JSElectronWebApp.Clear;
begin
  inherited Clear;
  Options:=Options+[baoRunOnReady]-[baoUseBrowserApp];
  PreloadLPR:='preload.lpr';
  RenderLPR:='render.lpr';
  HTMLFilename:='index.html';
  PackageJSON:='package.json';
end;

function TProjectPas2JSElectronWebApp.GetLocalizedName: string;
begin
  Result:=pjsdElectronWebApplication;
end;

function TProjectPas2JSElectronWebApp.GetLocalizedDescription: string;
begin
  Result:=pjsdAWebApplicationUsingElectronToRunAsDesktopApplicat;
end;

function TProjectPas2JSElectronWebApp.InitProject(AProject: TLazProject
  ): TModalResult;
var
  CompOpts: TLazCompilerOptions;
  RunMode: TAbstractRunParamsOptionsMode;
begin
  AProject.Flags:=DefaultProjectNoApplicationFlags;
  AProject.CustomData.Values[PJSProject]:='1';

  // start with the preload project
  AProject.ProjectInfoFile:=PreloadLPI;

  // set compiler and TargetOS (Note: must match the last project)
  CompOpts:=AProject.LazCompilerOptions;
  SetDefaultNodeJSCompileOptions(CompOpts);

  // all three projects can run the electron app
  RunMode:=AProject.RunParameters.GetOrCreate('default');
  RunMode.UseLaunchingApplication:=true;
  RunMode.LaunchingApplicationPathPlusParams:='"$(Pas2jsElectron)" .';

  Result:=mrOk;
end;

function TProjectPas2JSElectronWebApp.CreateStartFiles(AProject: TLazProject
  ): TModalResult;
var
  RenderJS: String;
begin
  Result:=mrCancel;

  if not CreatePreloadProject(AProject) then exit;
  if not CreateRenderProject(AProject) then exit;
  if not CreateWebAppProject(AProject) then exit;

  RenderJS:=FileToWebFile(ChangeFileExt(RenderLPR,'.js'));
  if CreateHTMLFile(AProject,RenderJS)=nil then exit;
  if not InteractiveSaveFile(HTMLFilename) then exit;

  if CreatePackageJSON(AProject)=nil then exit;

  if not CreateProjectGroup(AProject,[
    AProject.ProjectInfoFile,
    PreloadLPI,
    RenderLPI
    ]) then exit;

  Result:=mrOk;
end;

{ TPas2JSDTSToPasUnitDef }

constructor TPas2JSDTSToPasUnitDef.Create;
begin
  inherited Create;
  FConverter:=TCreateUnitFromDTS.Create(Nil);
end;

destructor TPas2JSDTSToPasUnitDef.destroy;
begin
  FreeAndNil(FConverter);
  inherited destroy;
end;

function TPas2JSDTSToPasUnitDef.Init(var NewFilename: string;
  NewOwner: TObject; var NewSource: string; Quiet: boolean): TModalResult;
begin
  inherited Init(NewFilename, NewOwner, NewSource, Quiet);
  If FConverter.ShowOptionsDialog then
    Result:=mrOK
  else
    Result:=mrCancel;
end;

function TPas2JSDTSToPasUnitDef.CreateSource(const Filename, SourceName,
  ResourceName: string): string;
begin
  FConverter.TargetUnitName:=ChangeFileExt(ExtractFileName(SourceName),'');
  FConverter.Execute;
  Result:=FConverter.Source.Text;
end;

function TPas2JSDTSToPasUnitDef.GetLocalizedName: string;
begin
  Result:=rsCreateUnitFromTypeScript;
end;

function TPas2JSDTSToPasUnitDef.GetLocalizedDescription: string;
begin
  Result:=rsCreateUnitFromTypeScriptDescription;
end;

{ TPas2JSMenuHandler }

procedure TPas2JSHandler.DoConvLog(Sender: TObject; const Msg: String);
begin
  IDEMessagesWindow.AddCustomMessage(TMessageLineUrgency.mluProgress,Msg,'',0,0,SMessageViewHTMLToForm);
end;

procedure TPas2JSHandler.OnSrcEditPopup(Sender: TObject);

Var
  Editor : TSourceEditorInterface;
  aFile : TLazProjectFile;
  IsPas2JS : Boolean;

begin
  Editor:=SourceEditorManagerIntf.ActiveEditor;
  if Editor=nil then exit;
  aFile:=Editor.GetProjectFile;
  IsPas2JS:=LazarusIDE.ActiveProject.CustomData.Values[PJSProject]='1';
  SrcMnuItem.Visible:=IsPas2js and Assigned(AFile) and (aFile.CustomData.Values[SHTML2FormOptions]<>'');
end;

procedure TPas2JSHandler.OnPrjInspPopup(Sender: TObject);

Var
  Idx : Integer;
  isPas2JS, isFile,HasHTMLInfo,AllOK,AnyOK : Boolean;
  aFile : TLazProjectFile;

begin
  AllOK:=False;
  AnyOK:=False;
  Idx:=-1;
  IsPas2JS:=LazarusIDE.ActiveProject.CustomData.Values[PJSProject]='1';
  if IsPas2JS then
    begin
    AllOK:=True;
    if Assigned(LazarusIDE.GetProjectInspectorSelection) then
      Idx:=LazarusIDE.GetProjectInspectorSelection.Count-1;
    While (Idx>=0) do
      begin
      HasHTMLInfo:=False;
      isFile:=TObject(LazarusIDE.GetProjectInspectorSelection[Idx]) is TLazProjectFile;
      if IsFile then
        begin
        aFile:=TLazProjectFile(LazarusIDE.GetProjectInspectorSelection[Idx]);
        HasHTMLInfo:=(aFile.CustomData.Values[SHTML2FormOptions]<>'');
        end;
      AllOK:=AllOK and HasHTMLInfo;
      Dec(Idx);
      end;
    Idx:=LazarusIDE.ActiveProject.FileCount-1;
    While (not AnyOK) and (Idx>=0) do
      begin
      aFile:=LazarusIDE.ActiveProject.Files[Idx];
      HasHTMLInfo:=(aFile.CustomData.Values[SHTML2FormOptions]<>'');
      AnyOK:=AnyOK or HasHTMLInfo;
      Dec(Idx);
      end;
    end;
  PrjMnuItem.Visible:=AllOK;
  PrjMnuItemAll.Visible:=AnyOK;
end;

function TPas2JSHandler.AskUserFile(aUnitName,aHTMLFileName: String): string;

Var
  Dlg : TOpenDialog;

begin
  Result:='';
  if mrOK<>QuestionDlg(pjsdHTMLSourceFileNotFound,
                      Format(pjsdHTMLFileNotFOund,[aUnitName,aHTMLFileName]),mtInformation,
                      [mrOK,pjsdBtnSelectFile,mrCancel,pjsdButtonCancel],0) then
    Exit;
  Dlg:=TOpenDialog.Create(Application);
  try
    Dlg.Filter:=pjsdHTMLFilter;
    Dlg.FileName:=aHTMLFileName;
    Dlg.Options:=[ofFileMustExist];
    if Dlg.Execute then
      Result:=Dlg.FileName
  finally
    Dlg.Free;
  end;
end;

procedure TPas2JSHandler.OnRefreshHTMLFormContext(Sender: TObject);
Var
  Editor : TSourceEditorInterface;
  aFile : TLazProjectFile;
  aSource : String;

begin
  Editor:=SourceEditorManagerIntf.ActiveEditor;
  if Editor=nil then exit;
  aFile:=Editor.GetProjectFile;
  if not (Assigned(AFile) and (aFile.CustomData.Values[SHTML2FormOptions]<>'')) then exit;
  if RefreshHTML(aFile,aSource) then
    begin
    Editor.SourceText:=aSource;
    Editor.Modified:=True;
    end;
end;

function TPas2JSHandler.RefreshHTML(aFile: TLazProjectFile; out aSource: String
  ): Boolean;

Var
  aOptions: THTML2ClassOptions;
  CG : TFormCodeGen;
  Conv : THTMLToFormElements;
  aFileName : String;

begin
  Result:=False;
  Conv:=Nil;
  CG:=Nil;
  aOptions:=THTML2ClassOptions.Create;
  try
    aOptions.FromJSON(aFile.CustomData.Values[SHTML2FormOptions]);
    if Not FileExists(aOptions.HTMLFileName) then
       begin
       aFileName:=AskUserFile(aFile.Unit_Name, aOptions.HTMLFileName);
       if aFileName='' then
         exit;
       aOptions.HTMLFileName:=aFileName;
       aFile.CustomData.Values[SDesignHTMLFile]:=aFileName;
       aFile.CustomData.Values[SHTML2FormOptions]:=aOptions.asJSON(False);
       LazarusIDE.ActiveProject.Modified:=True;
       end;
    CG:=TFormCodeGen.Create(Nil);
    Conv:=THTMLToFormElements.Create(nil);
    Conv.LoadOptions(aOptions);
    Conv.LoadFromFile(aoptions.HTMLFileName);
    Conv.OnLog:=@DoConvLog;
    CG.LoadOptions(aoptions);
    CG.FormElements:=Conv.FormElements;
    Cg.OutputUnitName:=ChangeFileExt(ExtractFileName(aFile.Filename),'');
    CG.Execute;
    aSource:=CG.Source.Text;
    Result:=True;
  finally
    CG.Free;
    Conv.Free;
    aOPtions.Free;
  end;
end;

procedure TPas2JSHandler.OnRefreshProjHTMLFormContext(Sender: TObject);

Var
  Idx : Integer;
  isFile,HasHTMLInfo : Boolean;
  aFile : TLazProjectFile;
  aSource : string;


begin
  if not Assigned(LazarusIDE.GetProjectInspectorSelection) then exit;
  Idx:=LazarusIDE.GetProjectInspectorSelection.Count-1;
  While (Idx>=0) do
   begin
   HasHTMLInfo:=False;
   isFile:=TObject(LazarusIDE.GetProjectInspectorSelection[Idx]) is TLazProjectFile;
   if IsFile then
     begin
     aFile:=TLazProjectFile(LazarusIDE.GetProjectInspectorSelection[Idx]);
     HasHTMLInfo:=(aFile.CustomData.Values[SHTML2FormOptions]<>'');
     if HasHTMLInfo then
       begin
       If RefreshHTML(aFile,aSource) then
         aFile.SetSourceText(aSource);
       end;
     end;
   Dec(Idx);
   end;
end;

procedure TPas2JSHandler.OnRefreshProjHTMLFormAllContext(Sender: TObject);
Var
  Idx : Integer;
  HasHTMLInfo : Boolean;
  aFile : TLazProjectFile;
  aSource : string;


begin
  Idx:=LazarusIDE.ActiveProject.FileCount-1;
  While (Idx>=0) do
   begin
   HasHTMLInfo:=False;
   aFile:=LazarusIDE.ActiveProject.Files[Idx];
   HasHTMLInfo:=(aFile.CustomData.Values[SHTML2FormOptions]<>'');
   if HasHTMLInfo then
     begin
     If RefreshHTML(aFile,aSource) then
       aFile.SetSourceText(aSource);
     end;
   Dec(Idx);
   end;
end;

{ TPas2JSHTMLClassDef }

procedure TPas2JSHTMLClassDef.DoConvLog(Sender: TObject; const Msg: String);
begin
  IDEMessagesWindow.AddCustomMessage(TMessageLineUrgency.mluProgress,Msg,'',0,0,SMessageViewHTMLToForm);
end;

constructor TPas2JSHTMLClassDef.Create;
begin
  inherited Create;
  FUseWebWidgets:=False;
  FOptions:=THTML2ClassOptions.Create;
  Name:=FileDescNameClassFromHTMLFile;
end;

destructor TPas2JSHTMLClassDef.destroy;
begin
  FreeAndNil(FOptions);
  inherited destroy;
end;

function TPas2JSHTMLClassDef.Initialized(NewFile: TLazProjectFile
  ): TModalResult;
begin
  Result:=inherited Initialized(NewFile);
  if result=mrOK then
    NewFile.CustomData.Add(SHTML2FormOPtions,FOptions.asJSON(False));
end;

function TPas2JSHTMLClassDef.Init(var NewFilename: string; NewOwner: TObject;
  var NewSource: string; Quiet: boolean): TModalResult;
begin
  FOptions.Reset;
  Result:=ShowOptionDialog;
end;

function TPas2JSHTMLClassDef.ShowOptionDialog: TModalResult;

Var
  Frm : TfrmHTML2Form;

begin
  frm:=TfrmHTML2Form.Create(Nil);
  try
    frm.LoadOptions(FOptions);
    Result:=Frm.ShowModal;
    if Result=mrOK then
      frm.SaveOptions(FOptions);
  finally
    frm.Free;
  end;
end;

function TPas2JSHTMLClassDef.CreateSource(const Filename, SourceName, ResourceName: string): string;

Var
  CG : TFormCodeGen;
  Conv : THTMLToFormElements;
  HTMLFile : TLazProjectFile;

begin
  Conv:=Nil;
  CG:=TFormCodeGen.Create(Nil);
  try
    Conv:=THTMLToFormElements.Create(nil);
    Conv.LoadOptions(FOptions);
    Conv.LoadFromFile(Foptions.HTMLFileName);
    Conv.OnLog:=@DoConvLog;
    CG.LoadOptions(Foptions);
    CG.FormElements:=Conv.FormElements;
    Cg.OutputUnitName:=ChangeFileExt(ExtractFileName(FileName),'');
    CG.Execute;
    Result:=CG.Source.Text;
    if FOptions.AddHTMLToProject then
      begin
      LazarusIDE.DoOpenEditorFile(Foptions.HTMLFileName,-1,-1,[ofAddToProject]);
      HTMLFile:=LazarusIDE.ActiveProject.FindFile(Foptions.HTMLFileName,[pfsfOnlyProjectFiles]);
      HTMLFile.IsPartOfProject:=true;
      HTMLFile.CustomData.Add('HTMLClassFile',SourceName);
      end;
  finally
    CG.Free;
    Conv.Free;
  end;
end;


function TPas2JSHTMLClassDef.GetLocalizedName: string;
begin
  Result:=rsCreateClassFromHTMLName;
end;

function TPas2JSHTMLClassDef.GetLocalizedDescription: string;
begin
  Result:=rsCreateClassFromHTMLDescription;
end;

{ TProjectPas2JSNodeJSApp }

function TProjectPas2JSNodeJSApp.CreateProjectSource: String;
Var
  Src : TStrings;
  units : string;

  Procedure Add(aLine : String);

  begin
    Src.Add(aLine);
  end;

  Procedure AddLn(aLine : String);

  begin
    if (Aline<>'') then
      Aline:=Aline+';';
    Add(Aline);
  end;


begin
  Units:='';
  if naoUseNodeJSApp in Options then
    Units:=Units+' nodejsapp,' ;
  Units:=Units+' JS, Classes, SysUtils, nodeJS';
  Src:=TStringList.Create;
  try
    // create program source
    AddLn('program Project1');
    AddLn('');
    Add('{$mode objfpc}');
    Add('');
    Add('uses');
    AddLn(units) ;
    Add('');
    if naoUseNodeJSApp in Options then
      begin
      Add('Type');
        Add('  TMyApplication = Class(TNodeJSApplication)');
      AddLn('    procedure DoRun; override');
      AddLn('  end');
      Add('');
      AddLn('Procedure TMyApplication.DoRun');
      Add('');
      Add('begin');
      Add('  // Your code here');
      AddLn('  Terminate');
      AddLn('end');
      Add('');
      Add('var');
      AddLn('  Application : TMyApplication');
      Add('');
      end;
    Add('begin');
    if Not (naoUseNodeJSApp in Options) then
       Add('  // Your code here')
    else
       begin
       AddLn('  Application:=TMyApplication.Create(Nil)');
       AddLn('  Application.Initialize');
       AddLn('  Application.Run');
       end;
    Add('end.');
    Result:=Src.Text;
  finally
    Src.Free;
  end;
end;

function TProjectPas2JSNodeJSApp.ShowOptionsDialog: TModalResult;

  Function Co(o : TNodeJSApplicationOption) : boolean;

  begin
    Result:=O in Options;
  end;

  Procedure So(Value : Boolean; o : TNodeJSApplicationOption);

  begin
    if Value then
      Include(Foptions,O);
  end;


begin
  With TNodeJSProjectOptionsForm.Create(Nil) do
    try
      UseNodeJSApplication:=CO(naoUseNodeJSApp);
      Result:=ShowModal;
      if Result=mrOK then
        begin
        SO(UseNodeJSApplication,naoUseNodeJSApp);
        end;
    finally
      Free;
    end;
end;

constructor TProjectPas2JSNodeJSApp.Create;
begin
  inherited Create;
  Name:= ProjDescNamePas2JSNodeJSApp;
  Flags:=DefaultProjectNoApplicationFlags-[pfRunnable];
end;

function TProjectPas2JSNodeJSApp.DoInitDescriptor: TModalResult;
begin
  Result:=ShowOptionsDialog;
end;

function TProjectPas2JSNodeJSApp.GetLocalizedName: string;
begin
  Result:=pjsdNodeJSApplication;
end;

function TProjectPas2JSNodeJSApp.GetLocalizedDescription: string;
begin
  Result:=pjsdNodeJSAppDescription;
end;

function TProjectPas2JSNodeJSApp.InitProject(AProject: TLazProject ): TModalResult;

var
  MainFile : TLazProjectFile;
  CompOpts : TLazCompilerOptions;

begin
  Result:=inherited InitProject(AProject);
  MainFile:=AProject.CreateProjectFile('project1.lpr');
  MainFile.IsPartOfProject:=true;
  AProject.AddFile(MainFile,false);
  AProject.MainFileID:=0;
  AProject.CustomData.Values[PJSProject]:='1';
  CompOpts:=AProject.LazBuildModes.BuildModes[0].LazCompilerOptions;
  SetDefaultNodeJSCompileOptions(CompOpts);
  CompOpts.TargetFilename:=ExtractFileNameOnly(MainFile.Filename);

  SetDefaultNodeRunParams(AProject.RunParameters.GetOrCreate('Default'));

  // create program source
  AProject.MainFile.SetSourceText(CreateProjectSource,true);

  //AProject.AddPackageDependency('pas2js_rtl');
  //if naoUseNodeJSApp in Options then
  //  AProject.AddPackageDependency('fcl_base_pas2js');
end;

function TProjectPas2JSNodeJSApp.CreateStartFiles(AProject: TLazProject
  ): TModalResult;
begin
  Result:=LazarusIDE.DoOpenEditorFile(AProject.MainFile.Filename,-1,-1,
                                      [ofProjectLoading,ofRegularFile]);
end;

{ ----------------------------------------------------------------------
  Module
  ----------------------------------------------------------------------}

{ TProjectPas2JSModuleApp }

function TProjectPas2JSModuleApp.CreateProjectSource: String;
Var
  Src : TStrings;
  units : string;

  Procedure Add(aLine : String);

  begin
    Src.Add(aLine);
  end;

  Procedure AddLn(aLine : String);

  begin
    if (Aline<>'') then
      Aline:=Aline+';';
    Add(Aline);
  end;

begin
  Units:=' JS, Classes, SysUtils';
  Src:=TStringList.Create;
  try
    // create program source
    AddLn('library Project1');
    AddLn('');
    Add('{$mode objfpc}');
    Add('');
    Add('uses');
    AddLn(units) ;
    Add('');
    Add('// add functions/procedures here');
    Add('');
    Add('// Add your exports statement here: ');
    Add('// exports yourfunction {as functionalias} ;');
    Add('');
    Add('begin');
    Add('  // Your library initialization code here');
    Add('end.');
    Result:=Src.Text;
  finally
    Src.Free;
  end;
end;

constructor TProjectPas2JSModuleApp.Create;
begin
  inherited Create;
  Name:= ProjDescNamePas2JSModuleApp;
  Flags:=DefaultProjectNoApplicationFlags-[pfRunnable];
end;

function TProjectPas2JSModuleApp.DoInitDescriptor: TModalResult;
begin
  Result:=mrOK;
end;

function TProjectPas2JSModuleApp.GetLocalizedName: string;
begin
  Result:=pjsdModuleApplication;
end;

function TProjectPas2JSModuleApp.GetLocalizedDescription: string;
begin
  Result:=pjsdModuleAppDescription;
end;

function TProjectPas2JSModuleApp.InitProject(AProject: TLazProject ): TModalResult;

var
  MainFile : TLazProjectFile;
  CompOpts : TLazCompilerOptions;

begin
  Result:=inherited InitProject(AProject);
  MainFile:=AProject.CreateProjectFile('project1.lpr');
  MainFile.IsPartOfProject:=true;
  AProject.AddFile(MainFile,false);
  AProject.MainFileID:=0;
  AProject.CustomData.Values[PJSProject]:='1';
  CompOpts:=AProject.LazBuildModes.BuildModes[0].LazCompilerOptions;
  SetDefaultModuleCompileOptions(CompOpts);
  CompOpts.TargetFilename:='js/project1';

  SetDefaultWebRunParams(AProject.RunParameters.GetOrCreate('Default'));

  // create program source
  AProject.MainFile.SetSourceText(CreateProjectSource,true);

  //AProject.AddPackageDependency('pas2js_rtl');
  //if naoUseNodeJSApp in Options then
  //  AProject.AddPackageDependency('fcl_base_pas2js');
end;

function TProjectPas2JSModuleApp.CreateStartFiles(AProject: TLazProject
  ): TModalResult;
begin
  Result:=LazarusIDE.DoOpenEditorFile(AProject.MainFile.Filename,-1,-1,
                                      [ofProjectLoading,ofRegularFile]);
end;


finalization
  Pas2JSHandler.Free;

end.

