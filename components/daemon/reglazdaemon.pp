{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  This unit installs the IDE items.
  It is not needed by daemons.
}
unit RegLazDaemon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FormEditingIntf, ProjectIntf, NewItemIntf, LazIDEIntf,
  Controls, Forms;

Type

  { TDaemonMapperDescriptor }

  TDaemonMapperDescriptor = Class(TFileDescPascalUnitWithResource)
  Public
    Constructor Create; override;
    Function GetLocalizedName : String; override;
    Function GetLocalizedDescription : String; override;
    Function GetInterfaceUsesSection : String; override;
    function GetImplementationSource(const Filename, SourceName, ResourceName: string): string; override;
  end;
  
  { TDaemonFileDescriptor }

  TDaemonFileDescriptor = Class(TFileDescPascalUnitWithResource)
  Public
    Constructor Create; override;
    Function GetLocalizedName : String; override;
    Function GetLocalizedDescription : String; override;
    Function GetInterfaceUsesSection : String; override;
    function GetImplementationSource(const Filename, SourceName, ResourceName: string): string; override;
  end;
  
  { TDaemonProjectDescriptor }

  TDaemonProjectDescriptor = class(TProjectDescriptor)
  public
    constructor create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject) : TModalResult; override;
    function CreateStartFiles(AProject: TLazProject) : TModalResult; override;
  published
    { Published declarations }
  end;

Procedure Register;

implementation

uses daemonapp;

Resourcestring
  SDaemonApps     = 'Daemon (service) applications';
  SDaemonAppName  = 'Daemon (service) application';
  SDaemonAppDescr = 'Daemon (service) application is a non-gui application that runs in the background.';
  SDaemonName     = 'Daemon Module';
  SDaemonDescr    = 'Daemon Module for inclusion in a daemon application';
  SDaemonMapperName  = 'Daemon mapper';
  SDaemonMapperDescr = 'Daemon mapper for inclusion in a daemon application.'+LineEnding+
                       'Only one daemon mapper may exist in a daemon application.';

Procedure Register;

begin
  RegisterNewItemCategory(TNewIDEItemCategory.Create(SDaemonApps));
  RegisterProjectDescriptor(TDaemonProjectDescriptor.Create,SDaemonApps);
  RegisterProjectFileDescriptor(TDaemonFileDescriptor.Create,SDaemonApps);
  RegisterProjectFileDescriptor(TDaemonMapperDescriptor.Create,SDaemonApps);
  FormEditingHook.RegisterDesignerBaseClass(TDaemonMapper);
  FormEditingHook.RegisterDesignerBaseClass(TDaemon);
end;


{ TDaemonFileDescriptor }

constructor TDaemonFileDescriptor.Create;
begin
  inherited Create;
  ResourceClass:=TDaemon;
  Name:='Daemon Module';
  UseCreateFormStatements:=False;
end;

function TDaemonFileDescriptor.GetLocalizedName: String;
begin
  Result:=SDaemonName;
end;

function TDaemonFileDescriptor.GetLocalizedDescription: String;
begin
  Result:=SDaemonDescr;
end;

function TDaemonFileDescriptor.GetInterfaceUsesSection: String;
begin
  Result:=inherited GetInterfaceUsesSection+', DaemonApp';
end;

function TDaemonFileDescriptor.GetImplementationSource(const Filename,
  SourceName, ResourceName: string): string;
Var
  Src : TStrings;
begin
  Src:=TStringList.Create;
  try
  Result:=inherited GetImplementationSource(Filename, SourceName, ResourceName);
    With Src do
      begin
      Add('Procedure RegisterDaemon;');
      Add('begin');
      Add('  RegisterDaemonClass(T'+ResourceName+')');
      Add('end;');
      Add('');
      Add(Result);
      if GetResourceType = rtRes then
        Add('initialization');
      Add('  RegisterDaemon;');
      Result:=Text;
      end;
  finally
    Src.Free;
  end;
end;

{ TDaemonProjectDescriptor }

constructor TDaemonProjectDescriptor.create; 

begin
  Inherited;
  Flags:=Flags - [pfMainUnitHasCreateFormStatements];
  Name:='Daemon Application';
end;
        
        
function TDaemonProjectDescriptor.GetLocalizedName: string;
begin
  Result:=SDaemonAppName;
end;

function TDaemonProjectDescriptor.GetLocalizedDescription: string;
begin
  Result:=SDaemonAppDescr;
end;

function TDaemonProjectDescriptor.InitProject(AProject: TLazProject
  ): TModalResult;
  
Var
  F : TLazProjectFile;
  Src : TStrings;

begin
  Result:=Inherited InitProject(AProject);
  If (Result=mrOK) then
    begin
    AProject.AddPackageDependency('FCL');
    AProject.AddPackageDependency('LCL');
    AProject.AddPackageDependency('LazDaemon');
    AProject.Title:='Daemon application';
    AProject.LazCompilerOptions.Win32GraphicApp:=False;
    AProject.ProjectInfoFile:='project1.lpi';
    F:=AProject.CreateProjectFile('project1.lpr');
    F.IsPartOfProject:=True;
    AProject.AddFile(F,False);
    AProject.MainFileID:=0;
    Src:=TStringList.Create;
    try
      With Src do
        begin
        Add('Program project1;');
        Add('');
        Add('Uses');
        Add('{$IFDEF UNIX}{$IFDEF UseCThreads}');
        Add('  CThreads,');
        Add('{$ENDIF}{$ENDIF}');
        Add('  DaemonApp, lazdaemonapp');
        Add('  { add your units here };');
        Add('');
        Add('begin');
        Add('  Application.Initialize;');
        Add('  Application.Run;');
        Add('end.');
        end;
      F.SetSourceText(Src.Text);
    finally
      Src.Free;
    end;
    end;
end;

function TDaemonProjectDescriptor.CreateStartFiles(AProject: TLazProject): TModalResult;
  
Var
  FD : TProjectFileDescriptor;
  O : TNewFlags;
  
begin
  FD:=ProjectFileDescriptors.FindByName('Daemon Mapper');
  O:=[nfIsPartOfProject,nfOpenInEditor,nfCreateDefaultSrc];
  Result:=LazarusIDE.DoNewEditorFile(FD,'DaemonMapperUnit1.pas','',O);
  FD:=ProjectFileDescriptors.FindByName('Daemon Module');
  Result:=LazarusIDE.DoNewEditorFile(FD,'DaemonUnit1.pas','',O );
end;

{ TDaemonMapperDescriptor }

constructor TDaemonMapperDescriptor.Create;
begin
  inherited Create;
  Name:='Daemon Mapper';
  ResourceClass:=TDaemonMapper;
  UseCreateFormStatements:=False;
end;

function TDaemonMapperDescriptor.GetLocalizedName: String;
begin
  Result:=SDaemonMapperName;
end;

function TDaemonMapperDescriptor.GetLocalizedDescription: String;
begin
  Result:=SDaemonMapperDescr;
end;

function TDaemonMapperDescriptor.GetInterfaceUsesSection: String;
begin
  Result:=inherited GetInterfaceUsesSection+', DaemonApp';
end;

function TDaemonMapperDescriptor.GetImplementationSource(const Filename,
  SourceName, ResourceName: string): string;
Var
  Src : TStrings;
begin
  Src:=TStringList.Create;
  try
    Result:=inherited GetImplementationSource(Filename, SourceName, ResourceName);
    With Src do
      begin
      Add('Procedure RegisterMapper;');
      Add('begin');
      Add('  RegisterDaemonMapper(T'+ResourceName+')');
      Add('end;');
      Add('');
      Add(Result);
      if GetResourceType = rtRes then
        Add('initialization');
      Add('  RegisterMapper;');
      Result:=Text;
      end;
  finally
    Src.Free;
  end;
end;

end.

