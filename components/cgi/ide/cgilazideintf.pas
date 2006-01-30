{ Copyright (C) 2004 Mattias Gaertner

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
      CGI Application - A Free Pascal program for CGI
                     using TCgiApplication for the main source (normally hidden,
                     just like the .lpr file for a normal Application).
      
    New Unit Type:
      CGI Module - A unit with a TCGIDatamodule.

  See the README file for more information.
}
unit CGILazIDEIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cgiApp, cgiModules, LazIDEIntf, ProjectIntf,
  Controls, Forms;

type
  { TCGIApplicationDescriptor }

  TCGIApplicationDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;

  { TFileDescPascalUnitWithCGIDataModule }

  TFileDescPascalUnitWithCGIDataModule = class(TFileDescPascalUnitWithResource)
  public
    constructor Create; override;
    function GetInterfaceUsesSection: string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
  end;

var
  ProjectDescriptorCGIApplication: TCGIApplicationDescriptor;
  FileDescriptorCGIModule: TFileDescPascalUnitWithCGIDataModule;
  
procedure Register;

implementation

procedure Register;
begin
  FileDescriptorCGIModule:=TFileDescPascalUnitWithCGIDataModule.Create;
  RegisterProjectFileDescriptor(FileDescriptorCGIModule);
  ProjectDescriptorCGIApplication:=TCGIApplicationDescriptor.Create;
  RegisterProjectDescriptor(ProjectDescriptorCGIApplication);
end;

{ TCGIApplicationDescriptor }

constructor TCGIApplicationDescriptor.Create;
begin
  inherited Create;
  Name:='CGIApplication';
end;

function TCGIApplicationDescriptor.GetLocalizedName: string;
begin
  Result:='CGi Application';
end;

function TCGIApplicationDescriptor.GetLocalizedDescription: string;
begin
  Result:='CGi Application'#13#13'A CGI (Common Gateway Interface) program '
          +'in Free Pascal. The program file is '
          +'automatically maintained by Lazarus.';
end;

function TCGIApplicationDescriptor.InitProject(AProject: TLazProject): TModalResult;
var
  le: string;
  NewSource: String;
  MainFile: TLazProjectFile;
begin
  inherited InitProject(AProject);

  MainFile:=AProject.CreateProjectFile('cgiproject1.lpr');
  MainFile.IsPartOfProject:=true;
  AProject.AddFile(MainFile,false);
  AProject.MainFileID:=0;

  // create program source
  le:=LineEnding;
  NewSource:='program Project1;'+le
    +le
    +'{$mode objfpc}{$H+}'+le
    +le
    +'uses'+le
    +'  cgiModules;'+le
    +le
    +'var'+le
    +'  Applicaton: TModuledCGIApplication;'+le
    +'begin'+le
    +'  Application:=TModuledCGIApplication.Create(nil);'+le
    +'  Application.Initialize;'+le
    +'  Application.Run;'+le
    +'  Application.Free;'+le
    +'end.'+le
    +le;
  AProject.MainFile.SetSourceText(NewSource);

  // add
  AProject.AddPackageDependency('CGILaz');

  // compiler options
  AProject.LazCompilerOptions.Win32GraphicApp:=false;
  Result:= mrOK;
end;

function TCGIApplicationDescriptor.CreateStartFiles(
  AProject: TLazProject): TModalResult;
begin
  if AProject=nil then ;
  LazarusIDE.DoNewEditorFile(FileDescriptorCGIModule,'','',
                         [nfIsPartOfProject,nfOpenInEditor,nfCreateDefaultSrc]);
  Result:= mrOK;
end;

{ TFileDescPascalUnitWithCGIDataModule }

constructor TFileDescPascalUnitWithCGIDataModule.Create;
begin
  inherited Create;
  Name:='CGIModule';
  ResourceClass:=TCGIDataModule;
  UseCreateFormStatements:=true;
end;

function TFileDescPascalUnitWithCGIDataModule.GetInterfaceUsesSection: string;
begin
  Result:=inherited GetInterfaceUsesSection;
  Result:=Result+',LResources,cgiModules';
end;

function TFileDescPascalUnitWithCGIDataModule.GetLocalizedName: string;
begin
  Result:='CGI Module';
end;

function TFileDescPascalUnitWithCGIDataModule.GetLocalizedDescription: string;
begin
  Result:='CGi Module'#13
         +'A datamodule for CGI applications.';
end;

end.

