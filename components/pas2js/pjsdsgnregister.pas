unit PJSDsgnRegister;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ProjectIntf, CompOptsIntf, LazIDEIntf, IDEOptionsIntf,
  PJSDsgnOptsFrame, Forms, Controls;

const
  ProjDescNamePas2JSWebApp = 'Web Application';
  ProjDescNamePas2JSNodeJSApp = 'NodeJS Application';

resourcestring
  pjsdWebApplication = 'Web Application';
  pjsdWebAppDescription = 'A pas2js program running in the browser';
  pjsdNodeJSApplication = 'Node.js Application';
  pjsdNodeJSAppDescription = 'A pas2js program running in node.js';

type

  { TProjectPas2JSWebApp }

  TProjectPas2JSWebApp = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;

  { TProjectPas2JSNodeJSApp }

  TProjectPas2JSNodeJSApp = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;

var
  PJSOptionsFrameID: integer = 1000;

procedure Register;

implementation

procedure Register;
begin
  PJSOptions:=TPas2jsOptions.Create;
  PJSOptions.Load;
  // register new-project items
  RegisterProjectDescriptor(TProjectPas2JSWebApp.Create);
  RegisterProjectDescriptor(TProjectPas2JSNodeJSApp.Create);
  // add options frame
  PJSOptionsFrameID:=RegisterIDEOptionsEditor(GroupEnvironment,TPas2jsOptionsFrame,
                                              PJSOptionsFrameID)^.Index;
end;

{ TProjectPas2JSNodeJSApp }

constructor TProjectPas2JSNodeJSApp.Create;
begin
  inherited Create;
  Name:=ProjDescNamePas2JSWebApp;
  Flags:=DefaultProjectNoApplicationFlags-[pfRunnable];
end;

function TProjectPas2JSNodeJSApp.GetLocalizedName: string;
begin
  Result:=pjsdNodeJSApplication;
end;

function TProjectPas2JSNodeJSApp.GetLocalizedDescription: string;
begin
  Result:=pjsdNodeJSAppDescription;
end;

function TProjectPas2JSNodeJSApp.InitProject(AProject: TLazProject
  ): TModalResult;
var
  MainFile: TLazProjectFile;
  CompOpts: TLazCompilerOptions;
  NewSource: String;
begin
  Result:=inherited InitProject(AProject);

  MainFile:=AProject.CreateProjectFile('project1.lpr');
  MainFile.IsPartOfProject:=true;
  AProject.AddFile(MainFile,false);
  AProject.MainFileID:=0;

  CompOpts:=AProject.LazCompilerOptions;
  CompOpts.TargetFilename:='program1';
  CompOpts.Win32GraphicApp:=false;
  CompOpts.UnitOutputDirectory:='js';
  CompOpts.TargetFilename:='project1';
  CompOpts.SetAlternativeCompile(
    '$MakeExe(pas2js) -Jc -Jminclude -Tnodejs "-Fu$(ProjUnitPath)" $Name($(ProjFile))',true);

  // create program source
  NewSource:='program Project1;'+LineEnding
    +LineEnding
    +'{$mode objfpc}'+LineEnding
    +LineEnding
    +'uses'+LineEnding
    +'  NodeJS, JS, Classes, SysUtils;'+LineEnding
    +LineEnding
    +'begin'+LineEnding
    +'end.'+LineEnding
    +LineEnding;
  AProject.MainFile.SetSourceText(NewSource,true);

  AProject.AddPackageDependency('pas2js_rtl');
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
end;

function TProjectPas2JSWebApp.GetLocalizedName: string;
begin
  Result:=pjsdWebApplication;
end;

function TProjectPas2JSWebApp.GetLocalizedDescription: string;
begin
  Result:=pjsdWebAppDescription;
end;

function TProjectPas2JSWebApp.InitProject(AProject: TLazProject): TModalResult;
var
  MainFile, HTMLFile: TLazProjectFile;
  CompOpts: TLazCompilerOptions;
  NewSource: String;
begin
  Result:=inherited InitProject(AProject);

  MainFile:=AProject.CreateProjectFile('project1.lpr');
  MainFile.IsPartOfProject:=true;
  AProject.AddFile(MainFile,false);
  AProject.MainFileID:=0;

  CompOpts:=AProject.LazCompilerOptions;
  CompOpts.TargetFilename:='program1';
  CompOpts.Win32GraphicApp:=false;
  CompOpts.UnitOutputDirectory:='js';
  CompOpts.TargetFilename:='project1';
  CompOpts.SetAlternativeCompile(
    '$MakeExe(pas2js) -Jc -Jminclude -Tbrowser "-Fu$(ProjUnitPath)" $Name($(ProjFile))',true);

  // create program source
  NewSource:='program Project1;'+LineEnding
    +LineEnding
    +'{$mode objfpc}'+LineEnding
    +LineEnding
    +'uses'+LineEnding
    +'  JS, Classes, SysUtils, Web;'+LineEnding
    +LineEnding
    +'begin'+LineEnding
    +'end.'+LineEnding
    +LineEnding;
  AProject.MainFile.SetSourceText(NewSource,true);

  HTMLFile:=AProject.CreateProjectFile('project1.html');
  HTMLFile.IsPartOfProject:=true;
  AProject.AddFile(HTMLFile,false);
  // create html source
  NewSource:=
     '<!doctype html>'+LineEnding
    +'<html lang="en">'+LineEnding
    +'<head>'+LineEnding
    +'  <meta charset="utf-8">'+LineEnding
    +'  <title>Project1</title>'+LineEnding
    +'<style>'+LineEnding
    +'<script src="project1.js"></script>'+LineEnding
    +'</head>'+LineEnding
    +'<div>Project1</div>'+LineEnding
    +'<script>'+LineEnding
    +'  rtl.run();'+LineEnding
    +'</script>'+LineEnding
    +'<body>'+LineEnding;
  HTMLFile.SetSourceText(NewSource);

  AProject.AddPackageDependency('pas2js_rtl');
end;

function TProjectPas2JSWebApp.CreateStartFiles(AProject: TLazProject
  ): TModalResult;
begin
  Result:=LazarusIDE.DoOpenEditorFile(AProject.MainFile.Filename,-1,-1,
                                      [ofProjectLoading,ofRegularFile]);
  if Result<>mrOK then exit;
  Result:=LazarusIDE.DoOpenEditorFile('project1.html',-1,-1,
                                      [ofProjectLoading,ofRegularFile]);
end;

end.

