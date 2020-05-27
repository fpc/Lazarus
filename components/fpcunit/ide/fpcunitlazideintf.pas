{ Copyright (C) 2004 Vincent Snijders

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
  
  
  Abstract:
    This unit adds a new project type and a new unit type to the IDE.
    New Project Type:
      FPCUnit Application - A Free Pascal program for FPCUnit tests.
      
    New Unit Type:
      FPCUnit test - A unit with a unit test.

  See the README file for more information.
}
unit FPCUnitLazIDEIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazIDEIntf, ProjectIntf, Controls, Forms, testcaseopts;

type
  { TFPCUnitApplicationDescriptor }

  TFPCUnitApplicationDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles({%H-}AProject: TLazProject): TModalResult; override;
  end;
  
  { TFPCUnitConsoleApplicationDescriptor }

  TFPCUnitConsoleApplicationDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles({%H-}AProject: TLazProject): TModalResult; override;
  end;

  { TFileDescPascalUnitFPCUnitTestCase }

  TFileDescPascalUnitFPCUnitTestCase = class(TFileDescPascalUnit)
  private
    FTestCaseName: string;
    FCreateSetup: boolean;
    FCreateTearDown: boolean;
  public
    constructor Create; override;
    function CreateSource(const Filename, SourceName,
                          ResourceName: string): string; override;
    function GetInterfaceUsesSection: string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function GetInterfaceSource(const {%H-}Filename, {%H-}SourceName,
                                {%H-}ResourceName: string): string;override;
    function GetImplementationSource(const {%H-}Filename, {%H-}SourceName,
                                     {%H-}ResourceName: string): string; override;
    property TestCaseName: string read FTestCaseName write FTestCaseName;
    property CreateSetup: boolean read FCreateSetup write FCreateSetup;
    property CreateTeardown: boolean read FCreateTeardown write FCreateTeardown;
  end;

var
  ProjectDescriptorFPCUnitApplication: TFPCUnitApplicationDescriptor;
  ProjectDescriptorFPCUnitConsoleApp: TFPCUnitConsoleApplicationDescriptor;
  FileDescriptorFPCUnitTestCase: TFileDescPascalUnitFPCUnitTestCase;

procedure Register;

implementation

resourcestring
  sFPCUnTestApp = 'FPCUnit Test Application';
  sFPCUnTestAppDesc = 'An application to run FPCUnit test cases.';
  sFPCUnTestCase = 'FPCUnit Test Case';
  sFPCUnTestCaseDesc = 'A unit containing a FPCUnit Test Case.';
  sWriteYourOwnTest = 'Write your own test';
  sFPCUnConsoleTestApp = 'FPCUnit Console Test Application';
  sFPCUnConsoleTestDesc = 'An application to run FPCUnit test cases in console mode.';

procedure Register;
begin
  FileDescriptorFPCUnitTestCase:=TFileDescPascalUnitFPCUnitTestCase.Create;
  RegisterProjectFileDescriptor(FileDescriptorFPCUnitTestCase);
  ProjectDescriptorFPCUnitConsoleApp := TFPCUnitConsoleApplicationDescriptor.Create;
  RegisterProjectDescriptor(ProjectDescriptorFPCUnitConsoleApp);
  ProjectDescriptorFPCUnitApplication:=TFPCUnitApplicationDescriptor.Create;
  RegisterProjectDescriptor(ProjectDescriptorFPCUnitApplication);
end;

{ TFPCUnitApplicationDescriptor }

constructor TFPCUnitApplicationDescriptor.Create;
begin
  inherited Create;
  Name:='FPCUnit Application';
end;

function TFPCUnitApplicationDescriptor.GetLocalizedName: string;
begin
  Result:=sFPCUnTestApp;
end;

function TFPCUnitApplicationDescriptor.GetLocalizedDescription: string;
begin
  Result:=sFPCUnTestAppDesc;
end;

function TFPCUnitApplicationDescriptor.InitProject(AProject: TLazProject): TModalResult;
var
  le: string;
  NewSource: String;
  MainFile: TLazProjectFile;
begin
  inherited InitProject(AProject);

  MainFile:=AProject.CreateProjectFile('fpcunitproject1.lpr');
  MainFile.IsPartOfProject:=true;
  AProject.AddFile(MainFile,false);
  AProject.MainFileID:=0;
  AProject.UseAppBundle:=true;
  AProject.UseManifest:=true;
  AProject.LoadDefaultIcon;

  // create program source
  le:=LineEnding;
  NewSource:='program FPCUnitProject1;'+le
    +le
    +'{$mode objfpc}{$H+}'+le
    +le
    +'uses'+le
    +'  Interfaces, Forms, GuiTestRunner;'+le
    +le
    +'begin'+le
    +'  Application.Initialize;'+le
    +'  Application.CreateForm(TGuiTestRunner, TestRunner);'+le
    +'  Application.Run;'+le
    +'end.'+le
    +le;
  AProject.MainFile.SetSourceText(NewSource);

  // add
  AProject.AddPackageDependency('FCL');
  AProject.AddPackageDependency('LCL');
  AProject.AddPackageDependency('fpcunittestrunner');
  
  // compiler options
  AProject.LazCompilerOptions.UseLineInfoUnit:=true;
  AProject.LazCompilerOptions.Win32GraphicApp:=true;
  AProject.LazCompilerOptions.TargetFilename:='fpunitproject1';
  AProject.LazCompilerOptions.UnitOutputDirectory:='lib'+PathDelim+'$(TargetCPU)-$(TargetOS)';
  Result:=mrOK;
end;

function TFPCUnitApplicationDescriptor.CreateStartFiles(AProject: TLazProject): TModalResult;
begin
  LazarusIDE.DoNewEditorFile(FileDescriptorFPCUnitTestCase,'','',
                         [nfIsPartOfProject,nfOpenInEditor,nfCreateDefaultSrc]);
  Result:=mrOK;
end;

{ TFileDescPascalUnitFPCUnitTestCase }

constructor TFileDescPascalUnitFPCUnitTestCase.Create;
begin
  inherited Create;
  Name:='FPCUnit TestCase';
  DefaultFilename:='testcase.pas';
  DefaultSourceName:='TestCase1';
end;

function TFileDescPascalUnitFPCUnitTestCase.CreateSource(const Filename,
  SourceName, ResourceName: string): string;
var
  LE: string;
begin
  CreateSetup := false;
  CreateTeardown := false;
  LE:=LineEnding;
  with TTestCaseOptionsForm.Create(nil) do
  try
    edDefaultName.Text := 'T' + SourceName;
    ShowModal;
    if edDefaultName.Text <> '' then
      TestCaseName := edDefaultName.Text
    else
      TestCaseName:= 'T' + SourceName;
    if cbSetup.Checked then
      CreateSetup := True
    else
      CreateSetup := False;
    if cbTeardown.Checked then
      CreateTeardown := True
    else
      CreateTeardown := False;
  finally
    Free;
  end;
  Result:=
     'unit '+SourceName+';'+LE
    +LE
    +'{$mode objfpc}{$H+}'+LE
    +LE
    +'interface'+LE
    +LE
    +'uses'+LE
    +'  '+GetInterfaceUsesSection+';'+LE
    +LE
    +GetInterfaceSource(Filename,SourceName,ResourceName)
    +'implementation'+LE
    +LE
    +GetImplementationSource(Filename,SourceName,ResourceName)
    +'end.'+LE
    +LE;
end;

function TFileDescPascalUnitFPCUnitTestCase.GetInterfaceUsesSection: string;
begin
  Result:=inherited GetInterfaceUsesSection;
  Result:=Result+', fpcunit, testutils, testregistry';
end;

function TFileDescPascalUnitFPCUnitTestCase.GetLocalizedName: string;
begin
  Result:=sFPCUnTestCase;
end;

function TFileDescPascalUnitFPCUnitTestCase.GetLocalizedDescription: string;
begin
  Result:=sFPCUnTestCaseDesc;
end;

function TFileDescPascalUnitFPCUnitTestCase.GetInterfaceSource(const Filename,
  SourceName, ResourceName: string): string;
var
  le: string;
  setupMethod: string;
  teardownMethod: string;
  protectedSection: string;
begin
  le:=System.LineEnding;
  if CreateSetup or CreateTeardown then
    protectedSection := '  protected' + le;
  if CreateSetup then
    setupMethod := '    procedure SetUp; override;' + le;
  if CreateTeardown then
    teardownMethod := '    procedure TearDown; override;' + le;
  Result := 'type' + le
    + le
    +'  '+TestCaseName+'= class(TTestCase)'+le
    + protectedSection
    + setupMethod
    + teardownMethod
    +'  published'+le
    +'    procedure TestHookUp;'+le
    +'  end;'+le+le;
end;

function TFileDescPascalUnitFPCUnitTestCase.GetImplementationSource(
  const Filename, SourceName, ResourceName: string): string;
var
  le: string;
  setupMethod: string;
  teardownMethod: string;
begin
  le:=System.LineEnding;
  if CreateSetup then
  setupMethod :=  'procedure '+TestCaseName+'.SetUp;'+le
                  +'begin'+le
                  +le
                  +'end;'+le;
  if CreateTeardown then
  teardownMethod := 'procedure '+TestCaseName+'.TearDown;'+le
                   +'begin'+le
                   +le
                   +'end;'+le;
  Result:='procedure '+TestCaseName+'.TestHookUp;'+le
    +'begin'+le
    +'  Fail('+QuotedStr(sWriteYourOwnTest)+');'+le
    +'end;'+le
    +le
    +setupMethod
    +le
    +teardownMethod
    +le
    +'Initialization'+le
    +le
    +'  RegisterTest('+TestCaseName+');'
    +le;
end;

{ TFPCUnitConsoleApplicationDescriptor }

constructor TFPCUnitConsoleApplicationDescriptor.Create;
begin
  inherited Create;
  Name:='FPCUnit Console Application';
end;

function TFPCUnitConsoleApplicationDescriptor.GetLocalizedName: string;
begin
  Result:=sFPCUnConsoleTestApp;
end;

function TFPCUnitConsoleApplicationDescriptor.GetLocalizedDescription: string;
begin
  Result:=sFPCUnConsoleTestDesc;
end;

function TFPCUnitConsoleApplicationDescriptor.InitProject(
  AProject: TLazProject): TModalResult;
var
  NewSource: string;
  MainFile: TLazProjectFile;
begin
  inherited InitProject(AProject);

  MainFile:=AProject.CreateProjectFile('fpcunitproject1.lpr');
  MainFile.IsPartOfProject:=true;
  AProject.AddFile(MainFile,false);
  AProject.MainFileID:=0;

  // create program source
  {$i fpcunitproject1.inc}

  AProject.MainFile.SetSourceText(NewSource);

  // add FCL dependency
  AProject.AddPackageDependency('FCL');

  // compiler options
  AProject.LazCompilerOptions.UseLineInfoUnit:=true;
  AProject.LazCompilerOptions.TargetFilename:='fpunitproject1';
  AProject.LazCompilerOptions.UnitOutputDirectory:='lib'+PathDelim+'$(TargetCPU)-$(TargetOS)';
  Result:=mrOK;
end;

function TFPCUnitConsoleApplicationDescriptor.CreateStartFiles(
  AProject: TLazProject): TModalResult;
begin
  LazarusIDE.DoNewEditorFile(FileDescriptorFPCUnitTestCase,'','',
                         [nfIsPartOfProject,nfOpenInEditor,nfCreateDefaultSrc]);
  Result:=mrOK;
end;

end.

