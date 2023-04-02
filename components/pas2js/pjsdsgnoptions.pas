{ pas2js options

  Author: Mattias Gaertner
}
unit PJSDsgnOptions;

{$mode objfpc}{$H+}
{$Inline on}

interface

uses
  Classes, SysUtils,
  // LazUtils
  LazFileCache, LazConfigStorage, LazFileUtils, FileUtil,
  // Codetools
  DefineTemplates, FileProcs,
  // IdeIntf
  MacroIntf, BaseIDEIntf;

const
  PJSDsgnOptsFile = 'pas2jsdsgnoptions.xml';
  PJSDefaultCompiler = '$MakeExe(IDE,pas2js)';
  PJSDefaultDTS2Pas = '$MakeExe(IDE,dts2pas)';
  PJSDefaultDTS2PasService = 'https://www.freepascal.org/~michael/service/dts2pas.cgi';
  PJSDefaultStartAtPort = 3000; // compileserver default port
  PJSDefaultHTTPServerParams = '-s -p $()';
  PJSDefaultBrowser = '$MakeExe(IDE,firefox)';
  PJSDefaultNodeJS = '$MakeExe(IDE,nodejs)';
  PJSDefaultElectronExe = '$MakeExe(IDE,electron)';

Type
  TPas2jsCachedOption = (
    p2jcoCompilerFilename,
    p2jcoNodeJSFilename,
    p2jcoElectronFilename,
    p2jcoAtomTemplateDir,
    p2jcoVSCodeTemplateDir,
    p2jcoDTSToPas,
    p2jcoDTSToPasServiceURL
    );
  TPas2jsCachedOptions = set of TPas2jsCachedOption;

const
  p2jcoFilenames = [
    p2jcoCompilerFilename,
    p2jcoNodeJSFilename,
    p2jcoElectronFilename,
    p2jcoAtomTemplateDir,
    p2jcoVSCodeTemplateDir,
    p2jcoDTSToPas
  ];

type
  TPas2jsCachedValue = record
    RawValue: string;
    Stamp: int64;
    ParsedValue: string;
  end;
  PPas2jsCachedValue = ^TPas2jsCachedValue;

  { TPas2jsOptions }

  TPas2jsOptions = class
  private
    FBrowserFileName: String;
    FCachedOptions: array[TPas2jsCachedOption] of TPas2jsCachedValue;
    FChangeStamp: int64;
    FOldWebServerFileName: string;
    FSavedStamp: int64;
    FStartAtPort: Word;
    function GetAtomTemplateDir: String;
    function GetCompilerFilename: string;
    function GetDTS2Pas: String;
    function GetDTS2PasService: String;
    function GetElectronFileName: string;
    function GetVSCodeTemplateDir: String;
    function GetModified: boolean;
    function GetNodeJSFileName: string;
    function GetParsedOptionValue(Option: TPas2jsCachedOption): string;
    procedure SetAtomTemplateDir(AValue: String);
    procedure SetDTS2Pas(AValue: String);
    procedure SetDTS2PasService(AValue: String);
    procedure SetElectronFileName(AValue: string);
    procedure SetVSCodeTemplateDir(AValue: String);
    procedure SetModified(AValue: boolean);
    procedure SetCompilerFilename(AValue: string);
    procedure SetNodeJSFileName(AValue: string);
    procedure SetStartAtPort(AValue: Word);
    procedure SetCachedOption(Option: TPas2jsCachedOption; const AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure IncreaseChangeStamp; inline;
    procedure Load;
    procedure Save;
    function GetParsedCompilerFilename: string;
    function GetParsedNodeJSFilename: string;
    function GetParsedElectronExe: string;
    procedure LoadFromConfig(Cfg: TConfigStorage);
    procedure SaveToConfig(Cfg: TConfigStorage);
  public
    property ChangeStamp: int64 read FChangeStamp;
    property Modified: boolean read GetModified write SetModified;
    property CompilerFilename: string read GetCompilerFilename write SetCompilerFilename;
    property NodeJSFileName : string Read GetNodeJSFileName Write SetNodeJSFileName;
    property ElectronFileName : string Read GetElectronFileName Write SetElectronFileName;
    property AtomTemplateDir : String Read GetAtomTemplateDir Write SetAtomTemplateDir;
    property VSCodeTemplateDir : String Read GetVSCodeTemplateDir Write SetVSCodeTemplateDir;
    property DTS2Pas : String Read GetDTS2Pas Write SetDTS2Pas;
    property DTS2PasServiceURL : String Read GetDTS2PasService Write SetDTS2PasService;
    property StartAtPort : Word Read FStartAtPort Write SetStartAtPort;
  public
    property OldWebServerFileName: string read FOldWebServerFileName write FOldWebServerFileName;
    property BrowserFileName : String Read FBrowserFileName Write FBrowserFileName;
  end;

var
  PJSOptions: TPas2jsOptions = nil;

function GetStandardPas2jsExe: string;
function GetStandardElectron: string;
function GetStandardNodeJS: string;
function GetPas2jsQuality(Filename: string; out Msg: string): boolean;

implementation

uses
  strpas2jsdesign;

function GetStandardPas2jsExe: string;
begin
  Result:=PJSDefaultCompiler;
  if not IDEMacros.SubstituteMacros(Result) then
    Result:='pas2js'+GetExeExt;
end;

function GetStandardNodeJS: string;
begin
  Result:=PJSDefaultNodeJS;
  if not IDEMacros.SubstituteMacros(Result) then
    Result:='nodejs'+GetExeExt;
end;

function GetStandardElectron: string;
begin
  Result:='electron'+GetExeExt;
end;

function GetPas2jsQuality(Filename: string; out Msg: string): boolean;
var
  ShortFile: String;
begin
  Msg:='';
  Filename:=TrimFilename(Filename);
  if (Filename='') then begin
    Msg:=pjsdMissingPathToPas2js;
    exit(false);
  end;
  if not FileExistsCached(Filename) then begin
    Msg:=CTSafeFormat(pjsdFileNotFound, [Filename]);
    exit(false);
  end;
  if not DirPathExistsCached(ExtractFilePath(Filename)) then begin
    Msg:=CTSafeFormat(pjsdDirectoryNotFound, [ExtractFilePath(Filename)]);
    exit(false);
  end;
  if not FileIsExecutable(Filename) then begin
    Msg:=CTSafeFormat(pjsdFileNotExecutable, [Filename]);
    exit(false);
  end;
  ShortFile:=ExtractFileNameOnly(Filename);
  if not CompareText(LeftStr(ShortFile,length('pas2js')),'pas2js')<>0 then begin
    Msg:=pjsdFileNameDoesNotStartWithPas2js;
    exit(false);
  end;
  // run it
  //RunTool(Filename);
end;

{ TPas2jsOptions }

procedure TPas2jsOptions.SetModified(AValue: boolean);
begin
  if AValue then
    IncreaseChangeStamp
  else
    FSavedStamp:=FChangeStamp;
end;

function TPas2jsOptions.GetModified: boolean;
begin
  Result:=FSavedStamp<>FChangeStamp;
end;

function TPas2jsOptions.GetAtomTemplateDir: String;
begin
  Result:=FCachedOptions[p2jcoAtomTemplateDir].RawValue;
end;

function TPas2jsOptions.GetCompilerFilename: string;
begin
  Result:=FCachedOptions[p2jcoCompilerFilename].RawValue;
end;

function TPas2jsOptions.GetDTS2Pas: String;
begin
  Result:=FCachedOptions[p2jcoDTSToPas].RawValue
end;

function TPas2jsOptions.GetDTS2PasService: String;
begin
  Result:=FCachedOptions[p2jcoDTSToPasServiceURL].RawValue
end;

function TPas2jsOptions.GetElectronFileName: string;
begin
  Result:=FCachedOptions[p2jcoElectronFilename].RawValue;
end;

function TPas2jsOptions.GetVSCodeTemplateDir: String;
begin
  Result:=FCachedOptions[p2jcoVSCodeTemplateDir].RawValue;
end;

function TPas2jsOptions.GetNodeJSFileName: string;
begin
  Result:=FCachedOptions[p2jcoNodeJSFilename].RawValue;
end;

procedure TPas2jsOptions.SetCompilerFilename(AValue: string);
begin
  AValue:=TrimFilename(AValue);
  SetCachedOption(p2jcoCompilerFilename,AValue);
end;

procedure TPas2jsOptions.SetNodeJSFileName(AValue: string);
begin
  AValue:=TrimFilename(AValue);
  SetCachedOption(p2jcoNodeJSFilename,AValue);
end;

constructor TPas2jsOptions.Create;
var
  o: TPas2jsCachedOption;
begin
  FChangeStamp:=LUInvalidChangeStamp64;
  FCachedOptions[p2jcoCompilerFilename].RawValue:=PJSDefaultCompiler;
  FCachedOptions[p2jcoNodeJSFilename].RawValue:=PJSDefaultNodeJS;
  FCachedOptions[p2jcoElectronFilename].RawValue:=PJSDefaultElectronExe;
  FCachedOptions[p2jcoAtomTemplateDir].RawValue:='';
  FCachedOptions[p2jcoVSCodeTemplateDir].RawValue:='';
  FCachedOptions[p2jcoDTSToPas].RawValue:=PJSDefaultDTS2Pas;
  FCachedOptions[p2jcoDTSToPasServiceURL].RawValue:=PJSDefaultDTS2PasService;
  for o in TPas2jsCachedOption do
    FCachedOptions[o].Stamp:=LUInvalidChangeStamp64;
end;

destructor TPas2jsOptions.Destroy;
begin
  inherited Destroy;
end;

procedure TPas2jsOptions.IncreaseChangeStamp;
begin
  LUIncreaseChangeStamp64(FChangeStamp);
end;

procedure TPas2jsOptions.Load;
var
  Cfg: TConfigStorage;
begin
  Cfg:=GetIDEConfigStorage(PJSDsgnOptsFile,true);
  try
    LoadFromConfig(Cfg);
  finally
    Cfg.Free;
  end;
end;

procedure TPas2jsOptions.Save;
var
  Cfg: TConfigStorage;
begin
  Cfg:=GetIDEConfigStorage(PJSDsgnOptsFile,false);
  try
    SaveToConfig(Cfg);
  finally
    Cfg.Free;
  end;
end;

Const
  KeyCompiler = 'compiler/value';
  KeyHTTPServer = 'webserver/value';
  KeyBrowser = 'webbrowser/value';
  KeyNodeJS = 'nodejs/value';
  KeyElectronExe = 'electron/value';
  KeyAtomTemplate = 'atomtemplate/value';
  KeyVSCodeTemplate = 'vscodetemplate/value';
  KeyStartPortAt = 'webserver/startatport/value';
  KeyDTS2PasTool = 'dts2pastool/value';
  KeyDTS2PasServiceURL= 'dts2passerviceurl/value';

procedure TPas2jsOptions.LoadFromConfig(Cfg: TConfigStorage);

begin
  CompilerFilename:=Cfg.GetValue(KeyCompiler,PJSDefaultCompiler);
  NodeJSFileName:=Cfg.GetValue(KeyNodeJS,PJSDefaultNodeJS);
  ElectronFileName:=Cfg.GetValue(KeyElectronExe,PJSDefaultElectronExe);
  AtomTemplateDir:=Cfg.GetValue(KeyAtomTemplate,'');
  VSCodeTemplateDir:=Cfg.GetValue(KeyVSCodeTemplate,'');
  DTS2Pas:=cfg.GetValue(KeyDTS2PasTool,PJSDefaultDTS2Pas);
  DTS2PasServiceURL:=cfg.GetValue(KeyDTS2PasServiceURL,PJSDefaultDTS2PasService);
  StartAtPort :=Cfg.GetValue(KeyStartPortAt,PJSDefaultStartAtPort);

  // legacy
  FOldWebServerFileName:=Cfg.GetValue(KeyHTTPServer,'');
  BrowserFileName:=Cfg.GetValue(KeyBrowser,'');

  Modified:=false;
end;

procedure TPas2jsOptions.SaveToConfig(Cfg: TConfigStorage);

begin
  Cfg.SetDeleteValue(KeyCompiler,CompilerFilename,PJSDefaultCompiler);
  Cfg.SetDeleteValue(KeyStartPortAt,StartAtPort,PJSDefaultStartAtPort);
  Cfg.SetDeleteValue(KeyNodeJS,NodeJSFileName,PJSDefaultNodeJS);
  Cfg.SetDeleteValue(KeyElectronExe,ElectronFileName,PJSDefaultElectronExe);
  Cfg.SetDeleteValue(KeyAtomTemplate,AtomTemplateDir,'');
  Cfg.SetDeleteValue(KeyVSCodeTemplate,VSCodeTemplateDir,'');
  cfg.SetDeleteValue(KeyDTS2PasTool,DTS2Pas,PJSDefaultDTS2Pas);
  cfg.SetDeleteValue(KeyDTS2PasServiceURL,DTS2PasServiceURL,PJSDefaultDTS2PasService);

  // legacy
  cfg.SetDeleteValue(KeyHTTPServer,OldWebServerFileName,'');
  Cfg.SetDeleteValue(KeyBrowser,BrowserFileName,'');

  Modified:=false;
end;

function TPas2jsOptions.GetParsedCompilerFilename: string;
begin
  Result:=GetParsedOptionValue(p2jcoCompilerFilename);
end;

function TPas2jsOptions.GetParsedNodeJSFilename: string;
begin
  Result:=GetParsedOptionValue(p2jcoNodeJSFilename);
end;

function TPas2jsOptions.GetParsedElectronExe: string;
begin
  Result:=GetParsedOptionValue(p2jcoElectronFilename);
end;

function TPas2jsOptions.GetParsedOptionValue(Option: TPas2jsCachedOption
  ): string;
var
  p: PPas2jsCachedValue;
  IsFilename: Boolean;
begin
  p:=@FCachedOptions[Option];
  if p^.Stamp<>IDEMacros.BaseTimeStamp then
  begin
    p^.Stamp:=IDEMacros.BaseTimeStamp;
    p^.ParsedValue:=p^.RawValue;
    IDEMacros.SubstituteMacros(p^.ParsedValue);
    IsFilename:=Option in p2jcoFilenames;
    if IsFilename then
    begin
      p^.ParsedValue:=TrimFilename(p^.ParsedValue);
      if (p^.ParsedValue<>'')
          and not FilenameIsAbsolute(p^.ParsedValue) then
      begin
        if ExtractFilePath(p^.ParsedValue)='' then
          p^.ParsedValue:=FindDefaultExecutablePath(p^.ParsedValue)
        else
          p^.ParsedValue:=''; // not found
      end;
    end;
    if p^.ParsedValue='' then
    begin
      case Option of
        p2jcoCompilerFilename: p^.ParsedValue:=GetStandardPas2jsExe;
        p2jcoNodeJSFilename: p^.ParsedValue:=GetStandardNodeJS;
        p2jcoElectronFilename: p^.ParsedValue:=GetStandardElectron;
      end;
      if IsFilename and (p^.ParsedValue<>'')
          and not FilenameIsAbsolute(p^.ParsedValue) then
      begin
        if ExtractFilePath(p^.ParsedValue)='' then
          p^.ParsedValue:=FindDefaultExecutablePath(p^.ParsedValue)
        else
          p^.ParsedValue:=''; // not found
      end;
    end;
  end;
  Result:=p^.ParsedValue;
end;

procedure TPas2jsOptions.SetAtomTemplateDir(AValue: String);
begin
  AValue:=TrimFilename(AValue);
  SetCachedOption(p2jcoAtomTemplateDir,AValue);
end;

procedure TPas2jsOptions.SetDTS2Pas(AValue: String);
begin
  AValue:=TrimFilename(AValue);
  SetCachedOption(p2jcoDTSToPas,AValue);
end;

procedure TPas2jsOptions.SetDTS2PasService(AValue: String);
begin
  SetCachedOption(p2jcoDTSToPasServiceURL,AValue);
end;

procedure TPas2jsOptions.SetElectronFileName(AValue: string);
begin
  AValue:=TrimFilename(AValue);
  SetCachedOption(p2jcoElectronFilename,AValue);
end;

procedure TPas2jsOptions.SetVSCodeTemplateDir(AValue: String);
begin
  AValue:=TrimFilename(AValue);
  SetCachedOption(p2jcoVSCodeTemplateDir,AValue);
end;

procedure TPas2jsOptions.SetStartAtPort(AValue: Word);
begin
  if FStartAtPort=AValue then Exit;
  FStartAtPort:=AValue;
  IncreaseChangeStamp;
end;

procedure TPas2jsOptions.SetCachedOption(Option: TPas2jsCachedOption;
  const AValue: string);
begin
  if FCachedOptions[Option].RawValue=AValue then exit;
  FCachedOptions[Option].RawValue:=AValue;
  FCachedOptions[Option].Stamp:=InvalidIDEMacroStamp;
  IncreaseChangeStamp;
  IDEMacros.IncreaseBaseStamp;
end;

Procedure DonePSJOptions;

begin
  if PJSOptions<>nil then
  begin
    try
      if PJSOptions.Modified then
        PJSOptions.Save;
    except
    end;
    FreeAndNil(PJSOptions);
  end;
end;


finalization
  DonePSJOptions;
end.
