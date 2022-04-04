unit PJSDsgnRegister;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, fpjson,
  // LCL
  Forms, Controls, Dialogs,
  // LazUtils
  LazLoggerBase, LazFileUtils,
  // IdeIntf
  IDECommands, ToolbarIntf, MenuIntf, ProjectIntf, CompOptsIntf, LazIDEIntf,
  IDEOptionsIntf, IDEOptEditorIntf, ComponentEditors, SrcEditorIntf, IDEMsgIntf,
  IDEDialogs, IDEExternToolIntf,
  // Pas2js
  idehtml2class, PJSDsgnOptions, PJSDsgnOptsFrame, idedtstopas,
  frmpas2jswebservers, frmpas2jsnodejsprojectoptions,
  frmpas2jsbrowserprojectoptions, pjsprojectoptions, idehtmltools,
  frmhtmltoform, pjscontroller, strpas2jsdesign, CodeToolManager, CodeCache;


const
  ProjDescNamePas2JSWebApp = 'Web Application';
  ProjDescNamePas2JSProgressiveWebApp = 'Progressive Web Application';
  ProjDescNamePas2JSServiceWorker = 'Pas2JS Service Worker';
  ProjDescNamePas2JSNodeJSApp = 'NodeJS Application';
  ProjDescNamePas2JSModuleApp = 'Pas2JS Library';
  FileDescNameClassFromHTMLFile = 'Class definition from HTML file';
  SMessageViewHTMLToForm = 'HTML To Class conversion';

  DefaultIconSizes: array[0..7] of word = (72,96,128,144,152,192,384,512);

type

  { TProjectPas2JSWebApp }
  TBrowserApplicationOption = (baoCreateHtml,        // Create template HTML page
                               baoMaintainHTML,      // Maintain the template HTML page
                               baoRunOnReady,        // Run in document.onReady
                               baoUseBrowserApp,     // Use browser app object
                               baoUseBrowserConsole, // use browserconsole unit to display Writeln()
                               baoStartServer,       // Start simple server
                               baoUseURL,            // Use this URL to run/show project in browser
                               baoShowException,     // let RTL show uncaught exceptions
                               baoUseWASI,           // Use WASI browser app object
                               baoUseModule          // include as module as opposed to regular script
                               );
  TBrowserApplicationOptions = set of TBrowserApplicationOption;

  TProjectPas2JSWebApp = class(TProjectDescriptor)
  private
    FHTMLFilename: string;
    FMainSrcFileName: string;
    FMainSrcName: string;
    FOptions: TBrowserApplicationOptions;
    FProjectPort: integer;
    FProjectURL: String;
    FProjectWasmURL : String;
    function GetHTMLFilename: string;
    function GetMainSrcFileName: string;
    function GetMainSrcName: string;
  protected
    procedure AddHTMLHead(Src: TStringList); virtual;
    function CreateHTMLFile(AProject: TLazProject; AFileName: String
      ): TLazProjectFile; virtual;
    function CreateProjectSource: String; virtual;
    Function DoInitDescriptor : TModalResult; override;
    function GetNextPort: Word; virtual;
    function ShowOptionsDialog: TModalResult; virtual;
    function ShowModalOptions(Frm: TWebBrowserProjectOptionsForm): TModalResult; virtual;
  public
    constructor Create; override;
    procedure Clear; virtual;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
    property Options : TBrowserApplicationOptions read FOptions Write Foptions;
    property ProjectPort : integer Read FProjectPort Write FProjectPort;
    property ProjectURL : String Read FProjectURL Write FProjectURL;
    property MainSrcFileName: string read GetMainSrcFileName write FMainSrcFileName;
    property MainSrcName: string read GetMainSrcName write FMainSrcName;
    property HTMLFilename: string read GetHTMLFilename write FHTMLFilename;
  end;

  { TProjectPas2JSServiceWorker }

  TProjectPas2JSServiceWorker = class(TProjectDescriptor)
  protected
    function CreateProjectSource: String; virtual;
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;

  { TProjectPas2JSProgressiveWebApp }

  TProjectPas2JSProgressiveWebApp = class(TProjectPas2JSWebApp)
  private
    FCSSStyleFilename: string;
    FIconSizes: TWordDynArray;
    FImagesDir: string;
    FManifestFilename: string;
    FProjectDir: string;
    FServiceWorkerLPR: string;
    FWebDir: string;
  protected
    procedure AddHTMLHead(Src: TStringList); override;
    function ShowModalOptions(Frm: TWebBrowserProjectOptionsForm
      ): TModalResult; override;
    function CreateManifestFile(AProject: TLazProject; AFileName: String
      ): TLazProjectFile; virtual;
    function CreateCSSStyle(AProject: TLazProject; AFileName: String
      ): TLazProjectFile; virtual;
    function ForceDir(Dir: string; AutoDelete: boolean): boolean; virtual;
    function SaveFile(aFilename: string): boolean; virtual;
  public
    constructor Create; override;
    procedure Clear; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
    property ProjectDir: string read FProjectDir write FProjectDir;
    property ServiceWorkerLPR: string read FServiceWorkerLPR write FServiceWorkerLPR;
    property CSSStyleFilename: string read FCSSStyleFilename write FCSSStyleFilename; // without path (WebDir)
    property ImagesDir: string read FImagesDir write FImagesDir; // without path (WebDir)
    property WebDir: string read FWebDir write FWebDir;
    property ManifestFilename: string read FManifestFilename write FManifestFilename; // without path (WebDir)
    property IconSizes: TWordDynArray read FIconSizes write FIconSizes;
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

procedure Register;

implementation

Var
  SrcMnuItem,PrjMnuItem,PrjMnuItemAll : TIDEmenuCommand;

procedure ShowServerDialog(Sender: TObject);
begin
  TPasJSWebserverProcessesForm.Instance.Show;
  TPasJSWebserverProcessesForm.Instance.BringToFront;
end;

Type

   { TPas2JSMenuHandler }

   TPas2JSMenuHandler = Class(TObject)
     procedure DoConvLog(Sender: TObject; const Msg: String);
     Procedure OnRefreshHTMLFormContext(Sender : TObject);
     Procedure OnRefreshProjHTMLFormContext(Sender : TObject);
     Procedure OnRefreshProjHTMLFormAllContext(Sender : TObject);
     Procedure OnSrcEditPopup(Sender : TObject);
     Procedure OnPrjInspPopup(Sender : TObject);
   private
     function AskUserFile(aUnitName,aHTMLFileName: String): string;
     function RefreshHTML(aFile: TLazProjectFile; out aSource: String): Boolean;
   end;

Const
  sPas2JSWebserverName = 'Pas2JSWebservers';

Var
  Pas2JSHTMLClassDef : TPas2JSHTMLClassDef;
  Pas2JSDTSToPasUnitDef : TPas2JSDTSToPasUnitDef;
  MenuHandler : TPas2JSMenuHandler;

procedure Register;

Var
  ViewCategory : TIDECommandCategory;
  IDECommand : TIDECommand;

begin
  MenuHandler:=TPas2JSMenuHandler.Create;
  if Assigned(MenuHandler) then; // Silence compiler warning
  PJSOptions:=TPas2jsOptions.Create;
  PJSOptions.Load;
  TPJSController.Instance.Hook;
  // register new-project items
  RegisterProjectDescriptor(TProjectPas2JSWebApp.Create);
  RegisterProjectDescriptor(TProjectPas2JSProgressiveWebApp.Create);
  RegisterProjectDescriptor(TProjectPas2JSServiceWorker.Create);
  RegisterProjectDescriptor(TProjectPas2JSNodeJSApp.Create);
  RegisterProjectDescriptor(TProjectPas2JSModuleApp.Create);
  Pas2JSHTMLClassDef:=TPas2JSHTMLClassDef.Create;
  RegisterProjectFileDescriptor(Pas2JSHTMLClassDef);
  Pas2JSDTSToPasUnitDef:=TPas2JSDTSToPasUnitDef.Create;
  RegisterProjectFileDescriptor(Pas2JSDTSToPasUnitDef);

  // add IDE options frame
  PJSOptionsFrameID:=RegisterIDEOptionsEditor(GroupEnvironment,TPas2jsOptionsFrame,
                                              PJSOptionsFrameID)^.Index;
  ViewCategory := IDECommandList.FindCategoryByName(CommandCategoryViewName);
  if ViewCategory <> nil then
    begin
    IDECommand := RegisterIDECommand(ViewCategory,sPas2JSWebserverName,SPasJSWebserverCaption,
                                     CleanIDEShortCut,CleanIDEShortCut,Nil,@ShowServerDialog);
    if IDECommand <> nil then
      RegisterIDEButtonCommand(IDECommand);
    end;
  RegisterIdeMenuCommand(itmViewDebugWindows,sPas2JSWebserverName,SPasJSWebserverCaption,nil,@ShowServerDialog);
  // Add project options frame
  RegisterIDEOptionsEditor(GroupProject,TPas2JSProjectOptionsFrame, Pas2JSOptionsIndex);
  SrcMnuItem:=RegisterIDEMenuCommand(SrcEditMenuSectionFirstStatic {SrcEditMenuSectionFirstDynamic},
     'HTMLFormClassRefresh', pjsRefreshClassFromHTML,@MenuHandler.OnRefreshHTMLFormContext);
  SourceEditorMenuRoot.AddHandlerOnShow(@MenuHandler.OnSrcEditPopup);
  PrjMnuItem:=RegisterIDEMenuCommand(ProjInspMenuSectionFiles,
      'PrjHTMLFormClassRefresh',pjsRefreshClassFromHTML,@MenuHandler.OnRefreshProjHTMLFormContext);
  PrjMnuItemAll:=RegisterIDEMenuCommand(ProjInspMenuSectionFiles,
      'PrjHTMLFormClassRefreshAll',pjsRefreshAllClassesFromHTML,@MenuHandler.OnRefreshProjHTMLFormAllContext);
  ProjectInspectorItemsMenuRoot.AddHandlerOnShow(@MenuHandler.OnPrjInspPopup);
end;

{ TProjectPas2JSProgressiveWebApp }

procedure TProjectPas2JSProgressiveWebApp.AddHTMLHead(Src: TStringList);
var
  i: Integer;
  h, CurImgDir: String;
begin
  inherited AddHTMLHead(Src);
  Src.Add('  <link rel="stylesheet" href="'+CSSStyleFilename+'" />');
  Src.Add('  <link rel="manifest" href="'+ManifestFilename+'" />');
  Src.Add('  <meta name="apple-mobile-web-app-status-bar" content="#d04010" />');
  Src.Add('  <meta name="theme-color" content="#d04010" />');
  CurImgDir:=ChompPathDelim(ImagesDir);
  for i:=0 to high(IconSizes) do
  begin
    h:=IntToStr(IconSizes[i]);
    Src.Add('  <link rel="apple-touch-icon" href="'+CurImgDir+'/icon-'+h+'x'+h+'.png" />');
  end;
end;

function TProjectPas2JSProgressiveWebApp.ShowModalOptions(
  Frm: TWebBrowserProjectOptionsForm): TModalResult;
var
  CurProjDir, LPRFilename, CurWebDir: String;
  Overwrites: TStringList;

  function CheckOverwriteFile(aFilename: string): string;
  begin
    if FileExists(aFilename) then
      Overwrites.Add(aFilename);
    Result:=aFilename;
  end;

  function CheckOverwriteDir(aDir: string): string;
  begin
    aDir:=ChompPathDelim(aDir);
    if FileExists(aDir)  and not DirectoryExistsUTF8(aDir) then
      Overwrites.Add(aDir);
    Result:=AppendPathDelim(aDir);
  end;

begin
  // hide unsupported options
  Frm.HideWASM;
  Frm.HideModule;

  Result:=inherited ShowModalOptions(Frm);
  if Result<>mrOk then exit;

  CurProjDir:=LazSelectDirectory('Project directory');
  if CurProjDir='' then exit(mrCancel);
  CurProjDir:=AppendPathDelim(CleanAndExpandDirectory(CurProjDir));
  ProjectDir:=CurProjDir;

  Overwrites:=TStringList.Create;
  try
    LPRFilename:=CheckOverwriteFile(CurProjDir+ExtractFileName(MainSrcFileName));
    CheckOverwriteFile(ChangeFileExt(LPRFilename,'.lpi'));

    ServiceWorkerLPR:=CheckOverwriteFile(ProjectDir+ServiceWorkerLPR);
    CheckOverwriteFile(ChangeFileExt(ServiceWorkerLPR,'.lpi'));

    CurWebDir:=CheckOverwriteDir(CurProjDir+WebDir);
    CheckOverwriteDir(CurWebDir+ImagesDir);

    CheckOverwriteFile(CurWebDir+HTMLFilename);
    CheckOverwriteFile(CurWebDir+ManifestFilename);
    CheckOverwriteFile(CurWebDir+CSSStyleFilename);

    if Overwrites.Count>0 then
    begin
      if IDEMessageDialog('Overwrite?','Overwrite files:'+sLineBreak+Overwrites.Text,
          mtConfirmation,[mbOk,mbCancel])<>mrOk then
        exit(mrCancel);
    end;

  finally
    Overwrites.Free;
  end;

  MainSrcFileName:=LPRFilename;
  if CompareFilenames(ExtractFileNameOnly(MainSrcFileName),MainSrcName)<>0 then
    MainSrcName:=ExtractFileNameOnly(MainSrcFileName);
end;

function TProjectPas2JSProgressiveWebApp.CreateManifestFile(
  AProject: TLazProject; AFileName: String): TLazProjectFile;
var
  Src: TStringList;
  i: Integer;
  h: String;
begin
  Result:=AProject.CreateProjectFile(AFileName);
  Result.IsPartOfProject:=true;
  AProject.CustomData.Values[PJSProjectManifestFile]:=CreateRelativePath(ProjectDir,Result.Filename);
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
    for i:=0 to high(IconSizes) do
    begin
      h:=IntToStr(IconSizes[i]);
      h:=h+'x'+h;
      Src.Add('    {');
      Src.Add('      "src": "/'+ChompPathDelim(ImagesDir)+'/icon-'+h+'.png",');
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
  Result:=AProject.CreateProjectFile(AFileName);
  Result.IsPartOfProject:=true;
  AProject.CustomData.Values[PJSProjectCSSFile]:=CreateRelativePath(ProjectDir,Result.Filename);
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

function TProjectPas2JSProgressiveWebApp.ForceDir(Dir: string;
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
      if not DeleteFileUTF8(Dir) then
      begin
        IDEMessageDialog('Error','Unable to create directory "'+Dir+'", because unable to delete file.',mtError,[mbOK]);
        exit;
      end;
    end else begin
      IDEMessageDialog('Error','Unable to create directory "'+Dir+'", because file already exists.',mtError,[mbOK]);
      exit;
    end;
  end;
  if not ForceDirectoriesUTF8(Dir) then
  begin
    IDEMessageDialog('Error','Unable to create directory "'+Dir+'".',mtError,[mbOK]);
    exit;
  end;
  Result:=true;
end;

function TProjectPas2JSProgressiveWebApp.SaveFile(aFilename: string): boolean;
var
  Code: TCodeBuffer;
begin
  Result:=false;
  Code:=CodeToolBoss.FindFile(aFilename);
  if Code=nil then
  begin
    debugln(['Error: TProjectPas2JSProgressiveWebApp.SaveFile 20220404130903 ',aFilename]);
    IDEMessageDialog('Error','File missing in codetools: "'+aFilename+'"',mtError,[mbOk]);
    exit;
  end;
  if not Code.Save then
  begin
    IDEMessageDialog('Error','Unable to write file "'+aFilename+'"',mtError,[mbOk]);
    exit;
  end;
  Result:=true;
end;

constructor TProjectPas2JSProgressiveWebApp.Create;
var
  i: Integer;
begin
  inherited Create;
  Name:=ProjDescNamePas2JSProgressiveWebApp;
  FWebDir:='www';
  FImagesDir:='images';
  FHTMLFilename:='index.html';
  FManifestFilename:='manifest.json';
  FCSSStyleFilename:='style.css';
  FServiceWorkerLPR:='ServiceWorker.lpr';
  SetLength(FIconSizes,length(DefaultIconSizes));
  for i:=0 to high(DefaultIconSizes) do
    FIconSizes[i]:=DefaultIconSizes[i];
end;

procedure TProjectPas2JSProgressiveWebApp.Clear;
begin
  inherited Clear;
  FOptions:=[baoCreateHtml,baoMaintainHTML,baoUseBrowserApp];
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
  CurProjDir, CurWebDir, CurImagesDir, CurHTMLFilename, CurManifestFilename,
    CurCSSStyleFilename: String;
begin
  Result:=inherited InitProject(AProject);
  if Result<>mrOk then exit;
  Result:=mrCancel;

  // create directories
  CurProjDir:=ChompPathDelim(AProject.Directory);
  if not FilenameIsAbsolute(CurProjDir) then
  begin
    debugln(['Error (pas2jsdsgn): [20220403220423] TProjectPas2JSProgressiveWebApp.InitProject project directory not absolute: '+CurProjDir]);
    exit;
  end;
  if CompareFilenames(CurProjDir,ChompPathDelim(ProjectDir))<>0 then
  begin
    debugln(['Error (pas2jsdsgn): [20220403221017] TProjectPas2JSProgressiveWebApp.InitProject project directory has switched: '+CurProjDir]);
    exit;
  end;

  if not ForceDirectoriesUTF8(CurProjDir) then
  begin
    IDEMessageDialog('Error','Unable to create directory "'+CurProjDir+'".',mtError,[mbOK]);
    exit;
  end;

  CurProjDir:=AppendPathDelim(CurProjDir);
  CurWebDir:=AppendPathDelim(CurProjDir+WebDir);
  CurImagesDir:=AppendPathDelim(CurWebDir+ImagesDir);

  if not ForceDir(WebDir,true) then exit;
  if not ForceDir(ImagesDir,true) then exit;

  // index.html (created in inherited InitProject)
  CurHTMLFilename:=CurWebDir+HTMLFilename;
  SaveFile(CurHTMLFilename);

  // manifest.json
  CurManifestFilename:=CurWebDir+ManifestFilename;
  CreateManifestFile(AProject,CurManifestFilename);
  SaveFile(CurManifestFilename);

  // style.css
  CurCSSStyleFilename:=CurWebDir+CSSStyleFilename;
  CreateCSSStyle(AProject,CurCSSStyleFilename);
  SaveFile(CurCSSStyleFilename);

  // favicon.ico
  // images/icon-.png

  // serviceworker.lpr/lpi

  // save lpr
  // save lpi

  Result:=mrOk;
end;

function TProjectPas2JSProgressiveWebApp.CreateStartFiles(AProject: TLazProject
  ): TModalResult;
begin
  Result:=inherited CreateStartFiles(AProject);
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
var
  MainFile: TLazProjectFile;
  CompOpts: TLazCompilerOptions;
begin
  Result:=inherited InitProject(AProject);
  MainFile:=AProject.CreateProjectFile('ServiceWorker.lpr');
  MainFile.IsPartOfProject:=true;
  AProject.AddFile(MainFile,false);
  AProject.MainFileID:=0;
  CompOpts:=AProject.LazCompilerOptions;
  SetDefaultServiceWorkerCompileOptions(CompOpts);
  CompOpts.TargetFilename:='ServiceWorker';
  SetDefaultServiceWorkerRunParams(AProject.RunParameters.GetOrCreate('Default'));
  AProject.MainFile.SetSourceText(CreateProjectSource,true);
  AProject.CustomData.Values[PJSProject]:='1';
  AProject.CustomData.Values[PJSProjectServiceWorker]:='1';
end;

function TProjectPas2JSServiceWorker.CreateStartFiles(AProject: TLazProject
  ): TModalResult;
begin
  Result:=LazarusIDE.DoOpenEditorFile(AProject.MainFile.Filename,-1,-1,
                                      [ofProjectLoading,ofRegularFile]);
  if Result<>mrOK then
     exit;
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

procedure TPas2JSMenuHandler.DoConvLog(Sender: TObject; const Msg: String);
begin
  IDEMessagesWindow.AddCustomMessage(TMessageLineUrgency.mluProgress,Msg,'',0,0,SMessageViewHTMLToForm);
end;

procedure TPas2JSMenuHandler.OnSrcEditPopup(Sender: TObject);

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

procedure TPas2JSMenuHandler.OnPrjInspPopup(Sender: TObject);

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

function TPas2JSMenuHandler.AskUserFile(aUnitName,aHTMLFileName: String): string;

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

procedure TPas2JSMenuHandler.OnRefreshHTMLFormContext(Sender: TObject);
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

Function TPas2JSMenuHandler.RefreshHTML(aFile : TLazProjectFile; Out aSource : String) : Boolean;

Var
  aOptions: THTML2ClassOptions;
  CG : TFormCodeGen;
  Conv : THTMLToFormELements;
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
    Conv:=THTMLToFormELements.Create(nil);
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

procedure TPas2JSMenuHandler.OnRefreshProjHTMLFormContext(Sender: TObject);

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

procedure TPas2JSMenuHandler.OnRefreshProjHTMLFormAllContext(Sender: TObject);
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
  Conv : THTMLToFormELements;
  HTMLFile : TLazProjectFile;

begin
  Conv:=Nil;
  CG:=TFormCodeGen.Create(Nil);
  try
    Conv:=THTMLToFormELements.Create(nil);
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
      AddLn('    procedure doRun; override');
      AddLn('  end');
      Add('');
      AddLn('Procedure TMyApplication.doRun');
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
  AProject.CustomData.Values[PJSProjectNodeJS]:='1';
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

{ TProjectPas2JSWebApp }

constructor TProjectPas2JSWebApp.Create;
begin
  inherited Create;
  Name:=ProjDescNamePas2JSWebApp;
  Flags:=DefaultProjectNoApplicationFlags-[pfRunnable];
  FMainSrcName:='Project1';
end;

procedure TProjectPas2JSWebApp.Clear;
begin
  // Reset options
  FOptions:=[baoCreateHtml,baoMaintainHTML];
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
      UseBrowserApp:=CO(baoUseBrowserApp);
      UseBrowserConsole:=CO(baoUseBrowserConsole);
      StartHTTPServer:=CO(baoStartServer);
      UseRunOnReady:=CO(baoRunOnReady);
      UseWASI:=CO(baoUseWASI);
      UseModule:=CO(baoUseModule);
      ShowUncaughtExceptions:=CO(baoShowException);
      // We allocate the new port in all cases.
      ServerPort:=GetNextPort;
      URL:='';
      WasmProgramURL:='';
      if Not CO(baoStartServer) then
        UseURL:=CO(baoUseURL);
      Result:=ShowModalOptions(Frm);
      if Result=mrOK then
        begin
        SO(CreateHTML,baoCreateHtml);
        SO(MaintainHTML,baoCreateHtml);
        SO(UseBrowserApp,baoUseBrowserApp);
        SO(UseBrowserConsole,baoUseBrowserConsole);
        SO(StartHTTPServer,baoStartServer);
        SO(UseRunOnReady,baoRunOnReady);
        SO(ShowUncaughtExceptions,baoShowException);
        SO(UseWASI,baoUseWASI);
        SO(UseModule,baoUseModule);
        SO(StartHTTPServer,baoStartServer);
        Self.ProjectPort:=ServerPort;
        SO(UseURL,baoUseURL);
        if baoStartServer in FOptions then
          begin
          DebugLN(['Info: Start server port: ', Self.ProjectPort,'from: ',ServerPort]);
          end
        else
          begin
          if baoUseURL in Options then
            FProjectURL:=URL;
          end;
        end;
        FProjectWasmURL:=WasmProgramURL;
    finally
      Free;
    end;
end;

function TProjectPas2JSWebApp.ShowModalOptions(
  Frm: TWebBrowserProjectOptionsForm): TModalResult;
begin
  Result:=Frm.ShowModal;
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

procedure TProjectPas2JSWebApp.AddHTMLHead(Src: TStringList);
begin
  if Src=nil then ;
end;

function TProjectPas2JSWebApp.CreateHTMLFile(AProject: TLazProject;
  AFileName: String): TLazProjectFile;

Const
  ConsoleDiv = '<div id="pasjsconsole"></div>'+LineEnding;
Var
  HTMLFile : TLazProjectFile;
  ScriptType, RunScript,Content : String;
  Src: TStringList;

begin
  HTMLFile:=AProject.CreateProjectFile(HTMLFilename);
  HTMLFile.IsPartOfProject:=true;
  AProject.CustomData.Values[PJSProjectHTMLFile]:=HTMLFile.Filename;
  AProject.AddFile(HTMLFile,false);
  Content:='';
  ScriptType:='';
  RunScript:='';
  if baoUseBrowserConsole in Options then
    Content:=ConsoleDiv;
  if baoUseModule in Options then
    begin
    ScriptType:='type="module"';
    end
  else
    begin
    if baoShowException in Options then
      Runscript:='rtl.showUncaughtExceptions=true;'+LineEnding+'  '
    else
      RunScript:='';
    if baoRunOnReady in Options then
      RunScript:=Runscript+'window.addEventListener("load", rtl.run);'+LineEnding
    else
      RunScript:=Runscript+'rtl.run();'+LineEnding;
    RunScript:='  <script>'+LineEnding
              +'    '+RunScript
              +'  </script>'+LineEnding
    end;
  Src:=TStringList.Create;
  try
    Src.Add('<!doctype html>');
    Src.Add('<html lang="en">');
    Src.Add('<head>');
    Src.Add('  <meta http-equiv="Content-type" content="text/html; charset=utf-8">');
    Src.Add('  <meta name="viewport" content="width=device-width, initial-scale=1">');
    Src.Add('  <title>'+MainSrcName+'</title>');
    Src.Add('  <script '+ScriptType+' src="'+AFileName+'"></script>');
    AddHTMLHead(Src);
    Src.Add('</head>');
    Src.Add('<body>');
    Src.Add('  '+RunScript);
    Src.Add('  '+Content);
    Src.Add('</body>');
    Src.Add('</html>');
    HTMLFile.SetSourceText(Src.Text);
  finally
    Src.Free;
  end;
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
    if (Aline<>'') then
      Aline:=Aline+';';
    Add(Aline);
  end;


begin
  Units:='';
  if baoUseBrowserConsole in Options then
    Units:=' browserconsole,';
  if baoUseBrowserApp in Options then
    begin
    Units:=Units+' browserapp,' ;
    if baoUseWASI in options then
      Units:=Units+' wasihostapp,' ;
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
      AddLn('    procedure doRun; override');
      AddLn('  end');
      Add('');
      AddLn('Procedure TMyApplication.doRun');
      Add('');
      Add('begin');
      if baoUseWASI in Options then
        begin
        if FProjectWasmURL='' then
          FProjectWasmURL:='yourwebassembly.wasm';
        AddLn(Format('  StartWebAssembly(''%s'')',[FProjectWasmURL]));
        end
      else
        Add('  // Your code here');
      AddLn('  Terminate');
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
       AddLn('  Application.Free');
       end;
    Add('end.');
    Result:=Src.Text;
  finally
    Src.Free;
  end;
end;

function TProjectPas2JSWebApp.InitProject(AProject: TLazProject): TModalResult;

var
  MainFile,
  HTMLFile : TLazProjectFile;
  CompOpts: TLazCompilerOptions;

begin
  Result:=inherited InitProject(AProject);
  MainFile:=AProject.CreateProjectFile(MainSrcFileName);
  MainFile.IsPartOfProject:=true;
  AProject.AddFile(MainFile,false);
  AProject.MainFileID:=0;
  CompOpts:=AProject.LazCompilerOptions;
  SetDefaultWebCompileOptions(CompOpts);
  CompOpts.TargetFilename:=ExtractFileNameOnly(MainFile.Filename);
  if baoUseModule in Options then
    CompOpts.TargetOS:='module';
  SetDefaultWebRunParams(AProject.RunParameters.GetOrCreate('Default'));
  AProject.MainFile.SetSourceText(CreateProjectSource,true);
  AProject.CustomData.Values[PJSProject]:='1';
  AProject.CustomData.Values[PJSProjectWebBrowser]:='1';
  if baoUseURL in Options then
    begin
    AProject.CustomData.Remove(PJSProjectPort);
    AProject.CustomData.Values[PJSProjectURL]:=ProjectURL;
    end
  else
    begin
    AProject.CustomData.Values[PJSProjectPort]:=IntToStr(ProjectPort);
    AProject.CustomData.Remove(PJSProjectURL);
    end;
  With AProject.CustomData do
    begin
    DebugLn(['Info: (pas2jsdsgn) ',PJSProjectWebBrowser,': ',Values[PJSProjectWebBrowser]]);
    DebugLn(['Info: (pas2jsdsgn) ',PJSProjectPort,': ',Values[PJSProjectPort]]);
    DebugLn(['Info: (pas2jsdsgn) ',PJSProjectURL,': ',Values[PJSProjectURL]]);
    end;
  // create html source
  if baoCreateHtml in Options then
    begin
    HTMLFile:=CreateHTMLFile(aProject,HTMLFilename);
    HTMLFile.CustomData[PJSIsProjectHTMLFile]:='1';
    if baoMaintainHTML in Options then
      AProject.CustomData.Values[PJSProjectMaintainHTML]:='1';
    if baoUseBrowserConsole in Options then
      AProject.CustomData[PJSProjectWebBrowser]:='1';
    if baoRunOnReady in options then
      AProject.CustomData[PJSProjectRunAtReady]:='1';
    end;
  //AProject.AddPackageDependency('pas2js_rtl');
  //if baoUseBrowserApp in Options then
  //  AProject.AddPackageDependency('fcl_base_pas2js');
end;

function TProjectPas2JSWebApp.CreateStartFiles(AProject: TLazProject
  ): TModalResult;
var
  MainFile: TLazProjectFile;
begin
  MainFile:=AProject.MainFile;
  Result:=LazarusIDE.DoOpenEditorFile(MainFile.Filename,-1,-1,
                                      [ofProjectLoading,ofRegularFile]);
  if Result<>mrOK then
     exit;

  if baoCreateHtml in Options then
    Result:=LazarusIDE.DoOpenEditorFile(ChangeFileExt(MainFile.Filename,'.html'),-1,-1,
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
  AProject.CustomData.Values[PJSProjectModule]:='1';
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


end.

