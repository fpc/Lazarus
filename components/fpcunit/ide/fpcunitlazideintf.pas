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
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
  
  
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
  Classes, SysUtils, LazIDEIntf, ProjectIntf, Controls, Forms,testcaseopts;

type
  { TFPCUnitApplicationDescriptor }

  TFPCUnitApplicationDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    procedure InitProject(AProject: TLazProject); override;
    procedure CreateStartFiles(AProject: TLazProject); override;
  end;
  
  { TFPCUnitConsoleApplicationDescriptor }

  TFPCUnitConsoleApplicationDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    procedure InitProject(AProject: TLazProject); override;
    procedure CreateStartFiles(AProject: TLazProject); override;
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
    function GetInterfaceSource(const Filename, SourceName,
                                ResourceName: string): string;override;
    function GetImplementationSource(const Filename, SourceName,
                                     ResourceName: string): string; override;
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
  Result:='FPCUnit Test Application';
end;

function TFPCUnitApplicationDescriptor.GetLocalizedDescription: string;
var
  le: string;
begin
  le := System.LineEnding;
  Result:='FPCUnit Test Application'+le+le
          +'An application to run fpcunit test cases.'+le
          +'The program file is '
          +'automatically maintained by Lazarus.';
end;

procedure TFPCUnitApplicationDescriptor.InitProject(AProject: TLazProject);
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
  AProject.AddPackageDependency('FPCUnitTestRunner');
  
  // compiler options
  AProject.LazCompilerOptions.UseLineInfoUnit:=true;
end;

procedure TFPCUnitApplicationDescriptor.CreateStartFiles(AProject: TLazProject);
begin
  LazarusIDE.DoNewEditorFile(FileDescriptorFPCUnitTestCase,'','',
                         [nfIsPartOfProject,nfOpenInEditor,nfCreateDefaultSrc]);
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
  Result:='FPCUnit Test Case';
end;

function TFileDescPascalUnitFPCUnitTestCase.GetLocalizedDescription: string;
begin
  Result:='FPCUnit Test Case'#13
         +'A unit containing a FPCUnit Test Case.';
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
    +'  Fail(''Write your own test'');'+le
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
  Result:='FPCUnit Console Test Application';
end;

function TFPCUnitConsoleApplicationDescriptor.GetLocalizedDescription: string;
var
  le: string;
begin
  le := System.LineEnding;
  Result:='FPCUnit Console Test Application'+le+le
          +'An application to run fpcunit test cases in console mode.'+le
          +'The program file is '
          +'automatically maintained by Lazarus.';
end;

procedure TFPCUnitConsoleApplicationDescriptor.InitProject(
  AProject: TLazProject);
var
  le: string;
  NewSource: string;
  MainFile: TLazProjectFile;
begin
  inherited InitProject(AProject);

  MainFile:=AProject.CreateProjectFile('fpcunitproject1.lpr');
  MainFile.IsPartOfProject:=true;
  AProject.AddFile(MainFile,false);
  AProject.MainFileID:=0;

  // create program source
  le:=LineEnding;
  NewSource:='program FPCUnitProject1;'+le
    +le
    +'{$mode objfpc}{$H+}'+le
    +le
    +'uses'+le
    +'  custapp, classes, sysutils, fpcunit, testreport, testregistry;'+le
    +le
    +'Const'+le
    +'  ShortOpts = ''alh'';'+le
    +'  Longopts : Array[1..5] of String = ('+le
    +'  ''all'',''list'',''format:'',''suite:'',''help'');'+le
    +'  Version = ''Version 0.1'';'+le
    +le
    +'Type'+le
    +'  TTestRunner = Class(TCustomApplication)'+le
    +'  private'+le
    +'    FXMLResultsWriter: TXMLResultsWriter;'+le
    +'  protected'+le
    +'    procedure DoRun ; Override;'+le
    +'    procedure doTestRun(aTest: TTest); virtual;'+le
    +'  public'+le
    +'    constructor Create(AOwner: TComponent); override;'+le
    +'    destructor Destroy; override;'+le
    +'  end;'+le
    +le
    +'constructor TTestRunner.Create(AOwner: TComponent);'+le
    +'begin'+le
    +'  inherited Create(AOwner);'+le
    +'  FXMLResultsWriter := TXMLResultsWriter.Create;'+le
    +'end;'+le
    +le
    +'destructor TTestRunner.Destroy;'+le
    +'begin'+le
    +'  FXMLResultsWriter.Free;'+le
    +'end;'+le
    +le
    +'procedure TTestRunner.doTestRun(aTest: TTest);'+le
    +'var'+le
    +'  testResult: TTestResult;'+le
    +'begin'+le
    +'  testResult := TTestResult.Create;'+le
    +'  try'+le
    +'    testResult.AddListener(FXMLResultsWriter);'+le
    +'    FXMLResultsWriter.WriteHeader;'+le
    +'    aTest.Run(testResult);'+le
    +'    FXMLResultsWriter.WriteResult(testResult);'+le
    +'  finally'+le
    +'    testResult.Free;'+le
    +'  end;'+le
    +'end;'+le
    +le
    +'procedure TTestRunner.DoRun;'+le
    +'var'+le
    +'  I : Integer;'+le
    +'  S : String;'+le
    +'begin'+le
    +'  S:=CheckOptions(ShortOpts,LongOpts);'+le
    +'  If (S<>'''') then'+le
    +'    Writeln(S);'+le
    +'  if HasOption(''h'', ''help'') or (ParamCount = 0) then'+le
    +'  begin'+le
    +'    writeln(Title);'+le
    +'    writeln(Version);'+le
    +'    writeln(''Usage: '');'+le
    +'    writeln(''-l or --list to show a list of registered tests'');'+le
    +'    writeln(''default format is xml, add --format=latex to output the list as latex source'');'+le
    +'    writeln(''-a or --all to run all the tests and show the results in xml format'');'+le
    +'    writeln(''The results can be redirected to an xml file,'');'+le
    +'    writeln(''for example: ./testrunner --all > results.xml'');'+le
    +'    writeln(''use --suite=MyTestSuiteName to run only the tests in a single test suite class'');'+le
    +'  end;'+le
    +'  if HasOption(''l'', ''list'') then'+le
    +'  begin'+le
    +'    if HasOption(''format'') then'+le
    +'    begin'+le
    +'      if GetOptionValue(''format'') = ''latex'' then'+le
    +'        writeln(GetSuiteAsLatex(GetTestRegistry))'+le
    +'      else'+le
    +'        writeln(GetSuiteAsXML(GetTestRegistry));'+le
    +'    end'+le
    +'    else'+le
    +'      writeln(GetSuiteAsXML(GetTestRegistry));'+le
    +'  end;'+le
    +'  if HasOption(''a'', ''all'') then'+le
    +'  begin'+le
    +'    doTestRun(GetTestRegistry)'+le
    +'  end'+le
    +'  else'+le
    +'    if HasOption(''suite'') then'+le
    +'    begin'+le
    +'      S := '''';'+le
    +'      S:=GetOptionValue(''suite'');'+le
    +'      if S = '''' then'+le
    +'        for I := 0 to GetTestRegistry.Tests.count - 1 do'+le
    +'          writeln(GetTestRegistry[i].TestName)'+le
    +'      else'+le
    +'        for I := 0 to GetTestRegistry.Tests.count - 1 do'+le
    +'          if GetTestRegistry[i].TestName = S then'+le
    +'          begin'+le
    +'            doTestRun(GetTestRegistry[i]);'+le
    +'          end;'+le
    +'    end;'+le
    +'  Terminate;'+le
    +'end;'+le
    +le
    +'Var'+le
    +'  App : TTestRunner;'+le
    +le
    +'begin'+le
    +'  App:=TTestRunner.Create(Nil);'+le
    +'  App.Initialize;'+le
    +'  App.Title := ''FPCUnit Console Test Case runner.'';'+le
    +'  App.Run;'+le
    +'  App.Free;'+le
    +'end.'+le
    +le;
  AProject.MainFile.SetSourceText(NewSource);

  // add
  AProject.AddPackageDependency('FCL');
  AProject.AddPackageDependency('FPCUnitTestRunner');

  // compiler options
  AProject.LazCompilerOptions.UseLineInfoUnit:=true;
end;

procedure TFPCUnitConsoleApplicationDescriptor.CreateStartFiles(
  AProject: TLazProject);
begin
  LazarusIDE.DoNewEditorFile(FileDescriptorFPCUnitTestCase,'','',
                         [nfIsPartOfProject,nfOpenInEditor,nfCreateDefaultSrc]);
end;

end.

