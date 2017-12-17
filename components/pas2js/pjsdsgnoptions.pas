{ pas2js options

  Author: Mattias Gaertner
}
unit PJSDsgnOptions;

{$mode objfpc}{$H+}
{$Inline on}

interface

uses
  Classes, SysUtils, LazFileCache, LazConfigStorage, LazFileUtils, FileUtil,
  MacroIntf, BaseIDEIntf, IDEUtils,
  DefineTemplates;

const
  PJSDsgnOptsFile = 'pas2jsdsgnoptions.xml';
  PJSDefaultCompiler = '$MakeExe(IDE,pas2js)';

type

  { TPas2jsOptions }

  TPas2jsOptions = class
  private
    FChangeStamp: int64;
    FSavedStamp: int64;
    FCompilerFilename: string;
    FCompilerFilenameStamp: int64;
    FCompilerFilenameParsed: string;
    function GetModified: boolean;
    procedure SetModified(AValue: boolean);
    procedure SetCompilerFilename(AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure IncreaseChangeStamp; inline;
    procedure Load;
    procedure Save;
    procedure LoadFromConfig(Cfg: TConfigStorage);
    procedure SaveToConfig(Cfg: TConfigStorage);
    function GetParsedCompilerFilename: string;
  public
    property CompilerFilename: string read FCompilerFilename write SetCompilerFilename;
    property ChangeStamp: int64 read FChangeStamp;
    property Modified: boolean read GetModified write SetModified;
  end;

var
  PJSOptions: TPas2jsOptions = nil;

function GetStandardPas2jsExe: string;
function GetPas2jsQuality(Filename: string; out Msg: string): boolean;

implementation

function GetStandardPas2jsExe: string;
begin
  Result:='$MakeExe(IDE,pas2js)';
  if not IDEMacros.SubstituteMacros(Result) then
    Result:='pas2js';
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

constructor TPas2jsOptions.Create;
begin
  FChangeStamp:=LUInvalidChangeStamp64;
  FCompilerFilename:=PJSDefaultCompiler;
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

procedure TPas2jsOptions.LoadFromConfig(Cfg: TConfigStorage);
begin
  CompilerFilename:=Cfg.GetValue('compiler/value',PJSDefaultCompiler);
  Modified:=false;
end;

procedure TPas2jsOptions.SaveToConfig(Cfg: TConfigStorage);
begin
  Cfg.SetDeleteValue('compiler/value',CompilerFilename,PJSDefaultCompiler);
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

finalization
  FreeAndNil(PJSOptions);
end.

