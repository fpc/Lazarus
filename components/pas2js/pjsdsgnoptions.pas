{ pas2js options

  Author: Mattias Gaertner
}
unit PJSDsgnOptions;

{$mode objfpc}{$H+}
{$Inline on}

interface

uses
  Classes, SysUtils, LazFileCache, LazConfigStorage, LazFileUtils, FileUtil,
  MacroIntf, BaseIDEIntf, IDEUtils, DefineTemplates;

const
  PJSDsgnOptsFile = 'pas2jsdsgnoptions.xml';
  PJSDefaultCompiler = '$MakeExe(IDE,pas2js)';
  PJSDefaultHTTPServer = '$MakeExe(IDE,simpleserver)';
  PJSDefaultStartAtPort = 4000; // Simpleserver default
  PJSDefaultBrowser = '$MakeExe(IDE,firefox)';
  PJSDefaultNodeJS = '$MakeExe(IDE,nodejs)';

Type
  { TPas2jsOptions }

  TPas2jsOptions = class
  private
    FBrowserFileName: String;
    FChangeStamp: int64;
    FHTTPServerFileName: string;
    FNodeJSFileName : String;
    FSavedStamp: int64;
    FCompilerFilename: string;
    FCompilerFilenameStamp: int64;
    FCompilerFilenameParsed: string;
    FStartAtPort: Word;
    function GetModified: boolean;
    procedure SetBrowserFileName(AValue: String);
    procedure SetHTTPServerFileName(AValue: string);
    procedure SetModified(AValue: boolean);
    procedure SetCompilerFilename(AValue: string);
    procedure SetNodeJSFileName(AValue: string);
    procedure SetStartAtPort(AValue: Word);
  public
    constructor Create;
    destructor Destroy; override;
    procedure IncreaseChangeStamp; inline;
    procedure Load;
    procedure Save;
    function GetParsedCompilerFilename: string;
    function GetParsedHTTPServerFilename: string;
    procedure LoadFromConfig(Cfg: TConfigStorage);
    procedure SaveToConfig(Cfg: TConfigStorage);
  public
    property CompilerFilename: string read FCompilerFilename write SetCompilerFilename;
    Property HTTPServerFileName : string Read FHTTPServerFileName Write SetHTTPServerFileName;
    Property NodeJSFileName : string Read FNodeJSFileName Write SetNodeJSFileName;
    Property BrowserFileName : String Read FBrowserFileName Write SetBrowserFileName;
    Property StartAtPort : Word Read FStartAtPort Write SetStartAtPort;
    property ChangeStamp: int64 read FChangeStamp;
    property Modified: boolean read GetModified write SetModified;
  end;

var
  PJSOptions: TPas2jsOptions = nil;

function GetStandardPas2jsExe: string;
function GetStandardHTTPServer: string;
function GetStandardBrowser: string;
function GetStandardNodeJS: string;
function GetPas2jsQuality(Filename: string; out Msg: string): boolean;

implementation

function GetStandardPas2jsExe: string;
begin
  Result:=PJSDefaultCompiler;
  if not IDEMacros.SubstituteMacros(Result) then
    Result:='pas2js';
end;

function GetStandardNodeJS: string;

begin
  Result:=PJSDefaultNodeJS;
  if not IDEMacros.SubstituteMacros(Result) then
    Result:='nodejs';
end;

function GetStandardHTTPServer: string;

begin
  Result:=PJSDefaultHTTPServer;
  if not IDEMacros.SubstituteMacros(Result) then
    Result:='simpleserver';
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
    Msg:='missing path to pas2js';
    exit(false);
  end;
  if not FileExistsCached(Filename) then begin
    Msg:='file "'+Filename+'" not found';
    exit(false);
  end;
  if not DirPathExistsCached(ExtractFilePath(Filename)) then begin
    Msg:='directory "'+ExtractFilePath(Filename)+'" not found';
    exit(false);
  end;
  if not FileIsExecutable(Filename) then begin
    Msg:='file "'+Filename+'" not executable';
    exit(false);
  end;
  ShortFile:=ExtractFileNameOnly(Filename);
  if not CompareText(LeftStr(ShortFile,length('pas2js')),'pas2js')<>0 then begin
    Msg:='file name does not start with "pas2js"';
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

procedure TPas2jsOptions.SetCompilerFilename(AValue: string);
begin
  if FCompilerFilename=AValue then Exit;
  FCompilerFilename:=AValue;
  IncreaseChangeStamp;
  IDEMacros.IncreaseBaseStamp;
end;

procedure TPas2jsOptions.SetNodeJSFileName(AValue: string);
begin
  if FNodeJSFileName=AValue then Exit;
  FNodeJSFileName:=AValue;
  Modified;
end;

constructor TPas2jsOptions.Create;

begin
  FChangeStamp:=LUInvalidChangeStamp64;
  FCompilerFilename:=PJSDefaultCompiler;
  FHTTPServerFileName:=PJSDefaultHTTPServer;
  FNodeJSFileName:=PJSDefaultNodeJS;
  FBrowserFileName:=PJSDefaultBrowser;
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
  HTTPServerFileName:=Cfg.GetValue(KeyHTTPServer,PJSDefaultHTTPServer);
  BrowserFileName:=Cfg.GetValue(KeyBrowser,PJSDefaultBrowser);
  NodeJSFileName:=Cfg.GetValue(KeyNodeJS,PJSDefaultNodeJS);
  StartAtPort :=Cfg.GetValue(KeyStartPortAt,PJSDefaultStartAtPort);
  Modified:=false;
end;

procedure TPas2jsOptions.SaveToConfig(Cfg: TConfigStorage);

begin
  Cfg.SetDeleteValue(KeyCompiler,CompilerFilename,PJSDefaultCompiler);
  Cfg.SetDeleteValue(KeyHTTPServer,HTTPServerFileName,PJSDefaultHTTPServer);
  Cfg.SetDeleteValue(KeyStartPortAt,StartAtPort,PJSDefaultStartAtPort);
  Cfg.SetDeleteValue(KeyNodeJS,NodeJSFileName,PJSDefaultNodeJS);
  Cfg.SetDeleteValue(KeyBrowser,BrowserFileName,PJSDefaultBrowser);
  Modified:=false;
end;


function TPas2jsOptions.GetParsedCompilerFilename: string;
begin
  if FCompilerFilenameStamp<>IDEMacros.BaseTimeStamp then begin
    FCompilerFilenameStamp:=IDEMacros.BaseTimeStamp;
    FCompilerFilenameParsed:=FCompilerFilename;
    IDEMacros.SubstituteMacros(FCompilerFilenameParsed);
    FCompilerFilenameParsed:=TrimFilename(FCompilerFilenameParsed);
    if (FCompilerFilenameParsed<>'')
    and not FilenameIsAbsolute(FCompilerFilenameParsed) then begin
      FCompilerFilenameParsed:=FindDefaultExecutablePath(FCompilerFilenameParsed);
    end;
  end;
  Result:=FCompilerFilenameParsed;
end;

function TPas2jsOptions.GetParsedHTTPServerFilename: string;
begin
  Result:=HTTPServerFileName;
  IDEMacros.SubstituteMacros(Result);
  if not FilenameIsAbsolute(Result) then
    Result:=FindDefaultExecutablePath(Result);
end;

procedure TPas2jsOptions.SetBrowserFileName(AValue: String);
begin
  if FBrowserFileName=AValue then Exit;
  FBrowserFileName:=AValue;
  IncreaseChangeStamp;
end;

procedure TPas2jsOptions.SetHTTPServerFileName(AValue: string);
begin
  if FHTTPServerFileName=AValue then Exit;
  FHTTPServerFileName:=AValue;
  IncreaseChangeStamp;
end;

procedure TPas2jsOptions.SetStartAtPort(AValue: Word);
begin
  if FStartAtPort=AValue then Exit;
  FStartAtPort:=AValue;
  IncreaseChangeStamp;
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
