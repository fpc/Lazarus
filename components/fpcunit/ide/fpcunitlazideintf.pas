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
  Classes, SysUtils, LazIDEIntf, ProjectIntf;

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

  { TFileDescPascalUnitFPCUnitTestCase }

  TFileDescPascalUnitFPCUnitTestCase = class(TFileDescPascalUnit)
  public
    constructor Create; override;
    function GetInterfaceUsesSection: string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function GetInterfaceSource(const Filename, SourceName,
                                ResourceName: string): string;override;
    function GetImplementationSource(const Filename, SourceName,
                                     ResourceName: string): string; override;
  end;

var
  ProjectDescriptorFPCUnitApplication: TFPCUnitApplicationDescriptor;
  FileDescriptorFPCUnitTestCase: TFileDescPascalUnitFPCUnitTestCase;
  
procedure Register;

implementation

procedure Register;
begin
  FileDescriptorFPCUnitTestCase:=TFileDescPascalUnitFPCUnitTestCase.Create;
  RegisterProjectFileDescriptor(FileDescriptorFPCUnitTestCase);
  ProjectDescriptorFPCUnitApplication:=TFPCUnitApplicationDescriptor.Create;
  RegisterProjectDescriptor(ProjectDescriptorFPCUnitApplication);
end;

{ TFPCUnitApplicationDescriptor }

constructor TFPCUnitApplicationDescriptor.Create;
begin
  inherited Create;
  Name:='FPCUnitApplication';
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
  Name:='FPCUnitTestCase';
  DefaultFilename:='testcase.pas';
  DefaultSourceName:='TestCase1';
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
  TestCaseName: string;
begin
  TestCaseName:= 'T'+SourceName;
  le:=System.LineEnding;
  Result:='type'+le
    +'  '+TestCaseName+'=class(TTestCase)'+le
    +'  published'+le
    +'    procedure TestHookUp;'+le
    +'  end;'+le+le;
end;

function TFileDescPascalUnitFPCUnitTestCase.GetImplementationSource(
  const Filename, SourceName, ResourceName: string): string;
var
  le: string;
  TestCaseName: string;
begin
  TestCaseName:= 'T'+SourceName;
  le:=System.LineEnding;
  Result:='procedure '+TestCaseName+'.TestHookUp;'+le
    +'begin'+le
    +'  Fail(''Write your own test'');'+le
    +'end;'+le
    +le
    +'Initialization'+le
    +le
    +'  RegisterTest('+TestCaseName+');'
    +le;
end;

end.

