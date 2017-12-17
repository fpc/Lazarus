unit pjscontroller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MacroIntf, MacroDefIntf, lazideintf;

Type

  { TPJSController }

  TPJSController = Class
  Private
    function GetPasJSBrowser(const s: string; const Data: PtrInt; var Abort: boolean): string;
    function GetPasJSNodeJS(const s: string; const Data: PtrInt; var Abort: boolean): string;
    function GetProjectURL(const s: string; const Data: PtrInt; var Abort: boolean): string;
  Public
    Constructor Create;
    Destructor Destroy; override;
    Procedure Hook; virtual;
    Procedure UnHook; virtual;
  end;

Const
  // Custom settings in .lpi
  PJSProjectWebBrowser =  'PasJSWebBrowserProject';
  PJSProjectURL = 'PasJSURL';
  PJSProjectHTMLFile = 'PasJSHTMLFile';
  PJSProjectPort = 'PasJSPort';

Function PJSController :  TPJSController;

implementation

uses PJSDsgnOptions;

Var
  ctrl : TPJSController;

Function PJSController :  TPJSController;

begin
  if ctrl=Nil then
    Ctrl:=TPJSController.Create;
  Result:=Ctrl;
end;

{ TPJSController }

function TPJSController.GetPasJSBrowser(const s: string; const Data: PtrInt;
  var Abort: boolean): string;
begin
  Abort:=False;
  Result:=PJSOptions.BrowserFileName;
  if Result='' then
    Result:=GetStandardBrowser;
end;

function TPJSController.GetPasJSNodeJS(const s: string; const Data: PtrInt;
  var Abort: boolean): string;
begin
  Abort:=False;
  Result:=PJSOptions.NodeJSFileName;
  if Result='' then
    Result:=GetStandardNodeJS;
end;

function TPJSController.GetProjectURL(const s: string; const Data: PtrInt; var Abort: boolean): string;

Var
  FN : String;

begin
  Abort:=LazarusIDE.ActiveProject.CustomData[PJSProjectWebBrowser]<>'1';
  if Abort then
    exit;
  Result:=LazarusIDE.ActiveProject.CustomData[PJSProjectURL];
  if (Result='') then
    begin
    FN:=LazarusIDE.ActiveProject.CustomData[PJSProjectHTMLFile];
    if (FN='') then
      FN:=ChangeFileExt(ExtractFileName(LazarusIDE.ActiveProject.ProjectInfoFile),'.html');
    Result:=LazarusIDE.ActiveProject.CustomData[PJSProjectPort];
    if (Result<>'') then
      Result:=Format('http://localhost:%s/%s',[Result,FN])
    end;
  Abort:=(Result='');
end;

constructor TPJSController.Create;
begin
  Hook;
end;

destructor TPJSController.Destroy;
begin
  Unhook;
  inherited Destroy;
end;

procedure TPJSController.Hook;
begin
  IDEMacros.Add(TTransferMacro.Create('Pas2JSBrowser','','Pas2JS selected Browser executable',@GetPasJSBrowser,[]));
  IDEMacros.Add(TTransferMacro.Create('Pas2JSNodeJS','','Pas2JS selected NodeJS xecutable',@GetPasJSNodeJS,[]));
  IDEMacros.Add(TTransferMacro.Create('Pas2JSProjectURL','','Pas2JS current project URL',@GetProjectURL,[]));
end;

procedure TPJSController.UnHook;
begin

end;

finalization
  FreeAndNil(Ctrl);
end.

