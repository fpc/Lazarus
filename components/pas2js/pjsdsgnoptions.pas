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
  DefineTemplates,
  // IdeIntf
  MacroIntf, BaseIDEIntf;

const
  PJSDsgnOptsFile = 'pas2jsdsgnoptions.xml';
  PJSDefaultCompiler = '$MakeExe(IDE,pas2js)';
  PJSDefaultWebServerName = 'compileserver';
  PJSDefaultWebServer = '$MakeExe(IDE,'+PJSDefaultWebServerName+')';
  PJSDefaultStartAtPort = 3000; // compileserver default port
  PJSDefaultHTTPServerParams = '-s -p $()';
  PJSDefaultBrowser = '$MakeExe(IDE,firefox)';
  PJSDefaultNodeJS = '$MakeExe(IDE,nodejs)';

Type
  TPas2jsCachedOption = (
    p2jcoCompilerFilename,
    p2jcoBrowserFilename,
    p2jcoHTTPServerFilename,
    p2jcoHTTPServerPort,
    p2jcoNodeJSFilename
    );
  TPas2jsCachedOptions = set of TPas2jsCachedOption;
const
  p2jcoFilenames = [
    p2jcoCompilerFilename,
    p2jcoBrowserFilename,
    p2jcoHTTPServerFilename,
    p2jcoNodeJSFilename
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
    FCachedOptions: array[TPas2jsCachedOption] of TPas2jsCachedValue;
    FChangeStamp: int64;
    FSavedStamp: int64;
    FStartAtPort: Word;
    function GetBrowserFileName: String;
    function GetCompilerFilename: string;
    function GetStartAtPort: Word;
    function GetWebServerFileName: string;
    function GetModified: boolean;
    function GetNodeJSFileName: string;
    function GetParsedOptionValue(Option: TPas2jsCachedOption): string;
    procedure SetBrowserFileName(AValue: String);
    procedure SetWebServerFileName(AValue: string);
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
    function GetParsedBrowserFilename: string;
    function GetParsedCompilerFilename: string;
    function GetParsedWebServerFilename: string;
    function GetParsedWebServerPort: string;
    function GetParsedNodeJSFilename: string;
    procedure LoadFromConfig(Cfg: TConfigStorage);
    procedure SaveToConfig(Cfg: TConfigStorage);
  public
    property CompilerFilename: string read GetCompilerFilename write SetCompilerFilename;
    Property WebServerFileName : string Read GetWebServerFileName Write SetWebServerFileName;
    Property NodeJSFileName : string Read GetNodeJSFileName Write SetNodeJSFileName;
    Property BrowserFileName : String Read GetBrowserFileName Write SetBrowserFileName;
    Property StartAtPort : Word Read GetStartAtPort Write SetStartAtPort;
    property ChangeStamp: int64 read FChangeStamp;
    property Modified: boolean read GetModified write SetModified;
  end;

var
  PJSOptions: TPas2jsOptions = nil;

function GetStandardPas2jsExe: string;
function GetStandardWebServerExe: string;
function GetStandardBrowser: string;
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

function GetStandardWebServerExe: string;

begin
  Result:=PJSDefaultWebServer;
  if not IDEMacros.SubstituteMacros(Result) then
    Result:='PJSDefaultHTTPServerCmd'+GetExeExt;
end;

function GetStandardBrowser: string;

begin
  Result:='$MakeExe(IDE,firefox)';
  if not IDEMacros.SubstituteMacros(Result) then
    begin
    Result:='$MakeExe(IDE,chrome)';
    {$ifdef windows}
    if not IDEMacros.SubstituteMacros(Result) then
      Result:='$MakeExe(IDE,iexplore)';
    {$else}
    {$ifdef darwin}
     if not IDEMacros.SubstituteMacros(Result) then
       Result:='$MakeExe(IDE,xdg-open)';
    {$endif}
     if not IDEMacros.SubstituteMacros(Result) then
       Result:='';
    {$endif}
    end;
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
    Msg:=SafeFormat(pjsdFileNotFound, [Filename]);
    exit(false);
  end;
  if not DirPathExistsCached(ExtractFilePath(Filename)) then begin
    Msg:=SafeFormat(pjsdDirectoryNotFound, [ExtractFilePath(Filename)]);
    exit(false);
  end;
  if not FileIsExecutable(Filename) then begin
    Msg:=SafeFormat(pjsdFileNotExecutable, [Filename]);
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

function TPas2jsOptions.GetBrowserFileName: String;
begin
  Result:=FCachedOptions[p2jcoBrowserFilename].RawValue;
end;

function TPas2jsOptions.GetCompilerFilename: string;
begin
  Result:=FCachedOptions[p2jcoCompilerFilename].RawValue;
end;

function TPas2jsOptions.GetStartAtPort: Word;
begin
  Result:=StrToIntDef(FCachedOptions[p2jcoHTTPServerPort].RawValue,PJSDefaultStartAtPort);
end;

function TPas2jsOptions.GetWebServerFileName: string;
begin
  Result:=FCachedOptions[p2jcoHTTPServerFilename].RawValue;
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
  FCachedOptions[p2jcoHTTPServerFilename].RawValue:=PJSDefaultWebServer;
  FCachedOptions[p2jcoNodeJSFilename].RawValue:=PJSDefaultNodeJS;
  FCachedOptions[p2jcoBrowserFilename].RawValue:=PJSDefaultBrowser;
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
  KeyStartPortAt = 'webserver/startatport/value';

procedure TPas2jsOptions.LoadFromConfig(Cfg: TConfigStorage);

begin
  CompilerFilename:=Cfg.GetValue(KeyCompiler,PJSDefaultCompiler);
  WebServerFileName:=Cfg.GetValue(KeyHTTPServer,PJSDefaultWebServer);
  BrowserFileName:=Cfg.GetValue(KeyBrowser,PJSDefaultBrowser);
  NodeJSFileName:=Cfg.GetValue(KeyNodeJS,PJSDefaultNodeJS);
  StartAtPort :=Cfg.GetValue(KeyStartPortAt,PJSDefaultStartAtPort);
  Modified:=false;
end;

procedure TPas2jsOptions.SaveToConfig(Cfg: TConfigStorage);

begin
  Cfg.SetDeleteValue(KeyCompiler,CompilerFilename,PJSDefaultCompiler);
  Cfg.SetDeleteValue(KeyHTTPServer,WebServerFileName,PJSDefaultWebServer);
  Cfg.SetDeleteValue(KeyStartPortAt,StartAtPort,PJSDefaultStartAtPort);
  Cfg.SetDeleteValue(KeyNodeJS,NodeJSFileName,PJSDefaultNodeJS);
  Cfg.SetDeleteValue(KeyBrowser,BrowserFileName,PJSDefaultBrowser);
  Modified:=false;
end;

function TPas2jsOptions.GetParsedCompilerFilename: string;
begin
  Result:=GetParsedOptionValue(p2jcoCompilerFilename);
end;

function TPas2jsOptions.GetParsedWebServerFilename: string;
begin
  Result:=GetParsedOptionValue(p2jcoHTTPServerFilename);
end;

function TPas2jsOptions.GetParsedWebServerPort: string;
begin
  Result:=GetParsedOptionValue(p2jcoHTTPServerPort);
end;

function TPas2jsOptions.GetParsedNodeJSFilename: string;
begin
  Result:=GetParsedOptionValue(p2jcoNodeJSFilename);
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
        p2jcoBrowserFilename: p^.ParsedValue:=GetStandardBrowser;
        p2jcoHTTPServerFilename: p^.ParsedValue:=GetStandardWebServerExe;
        p2jcoHTTPServerPort: p^.ParsedValue:=IntToStr(PJSDefaultStartAtPort);
        p2jcoNodeJSFilename: p^.ParsedValue:=GetStandardNodeJS;
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

function TPas2jsOptions.GetParsedBrowserFilename: string;
begin
  Result:=GetParsedOptionValue(p2jcoBrowserFilename);
end;

procedure TPas2jsOptions.SetBrowserFileName(AValue: String);
begin
  AValue:=TrimFilename(AValue);
  SetCachedOption(p2jcoBrowserFilename,AValue);
end;

procedure TPas2jsOptions.SetWebServerFileName(AValue: string);
begin
  AValue:=TrimFilename(AValue);
  SetCachedOption(p2jcoHTTPServerFilename,AValue);
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
