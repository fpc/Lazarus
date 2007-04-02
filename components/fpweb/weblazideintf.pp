{
  Copyright (C) 2007 Michael Van Canneyt

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
}
{$mode objfpc}
{$H+}
unit WebLazIDEIntf;

interface

uses
  Classes, SysUtils, fpWeb, fpHTML, fpdatasetform,
  Controls, Dialogs, forms, LazIDEIntf, ProjectIntf;

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
  { TCGIApplicationDescriptor }

  TApacheApplicationDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;

  TCustomCGIApplicationDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;

  { TFileDescPascalUnitWithCGIDataModule }

  TFileDescWebDataModule = class(TFileDescPascalUnitWithResource)
  public
    constructor Create; override;
    function GetInterfaceUsesSection: string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function GetImplementationSource(const Filename, SourceName, ResourceName: string): string;override;
  end;
  
  TFileDescHTMLModule = class(TFileDescPascalUnitWithResource)
  public
    constructor Create; override;
    function GetInterfaceUsesSection: string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function GetImplementationSource(const Filename, SourceName, ResourceName: string): string;override;
  end;

var
  ProjectDescriptorCustomCGIApplication: TCustomCGIApplicationDescriptor;
  ProjectDescriptorCGIApplication: TCGIApplicationDescriptor;
  ProjectDescriptorApacheApplication: TApacheApplicationDescriptor;
  FileDescriptorHTMLModule: TFileDescHTMLModule;
  FileDescriptorWebModule: TFileDescWebDataModule;

procedure Register;

implementation

uses LazarusPackageIntf,FormEditingIntf;

Const
  fpWebTab = 'fpWeb';

Procedure RegisterHTMLComponents;
begin
  RegisterComponents(fpWebTab,[THTMLDatasetContentProducer,
                               THTMLSelectProducer,
                               THTMLDatasetSelectProducer])

end;

Procedure RegisterDatasetComponents;

begin
  RegisterComponents(fpWebTab,[THTMLDataSetFormShowProducer,
                               THTMLDataSetFormEditProducer,
                               THTMLDataSetFormGridProducer]);
end;


Procedure RegisterComponents;

begin
  RegisterUnit('fphtml',@RegisterHTMLComponents);
  RegisterUnit('fpdatasetform',@RegisterdatasetComponents);
end;

procedure Register;
begin
  RegisterComponents;
  FileDescriptorWebModule:=TFileDescWebDataModule.Create;
  RegisterProjectFileDescriptor(FileDescriptorWebModule);
  ProjectDescriptorCGIApplication:=TCGIApplicationDescriptor.Create;
  RegisterProjectDescriptor(ProjectDescriptorCGIApplication);
  FileDescriptorHTMLModule:=TFileDescHTMLModule.Create;
  RegisterProjectFileDescriptor(FileDescriptorHTMLModule);
  ProjectDescriptorCustomCGIApplication:=TCustomCGIApplicationDescriptor.Create;
  RegisterProjectDescriptor(ProjectDescriptorCustomCGIApplication);
  ProjectDescriptorApacheApplication:=TApacheApplicationDescriptor.Create;
  RegisterProjectDescriptor(ProjectDescriptorApacheApplication);
  FormEditingHook.RegisterDesignerBaseClass(TFPWebModule);
  FormEditingHook.RegisterDesignerBaseClass(TFPHTMLModule);
end;

{ TCGIApplicationDescriptor }

constructor TCGIApplicationDescriptor.Create;
begin
  inherited Create;
  Name:='CGI Application';
end;

function TCGIApplicationDescriptor.GetLocalizedName: string;
begin
  Result:='CGI Application';
end;

function TCGIApplicationDescriptor.GetLocalizedDescription: string;
begin
  Result:='CGI Application'#13#13'A CGI (Common Gateway Interface) program '
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
  NewSource:='program cgiproject1;'+le
    +le
    +'{$mode objfpc}{$H+}'+le
    +le
    +'uses'+le
    +'  fpWeb,fpCGI;'+le
    +le
    +'begin'+le
    +'  Application.Title:=''cgiproject1'';'+le
    +'  Application.Initialize;'+le
    +'  Application.Run;'+le
    +'end.'+le
    +le;
  AProject.MainFile.SetSourceText(NewSource);

  // add
  AProject.AddPackageDependency('WebLaz');

  // compiler options
  AProject.LazCompilerOptions.Win32GraphicApp:=false;
  Result:= mrOK;
end;

function TCGIApplicationDescriptor.CreateStartFiles(AProject: TLazProject): TModalResult;
begin
  LazarusIDE.DoNewEditorFile(FileDescriptorWebModule,'','',
                         [nfIsPartOfProject,nfOpenInEditor,nfCreateDefaultSrc]);
  Result:= mrOK;
end;

{ TCustomCGIApplicationDescriptor }

constructor TCustomCGIApplicationDescriptor.Create;
begin
  inherited Create;
  Name:='Custom CGI Application';
end;

function TCustomCGIApplicationDescriptor.GetLocalizedName: string;
begin
  Result:='Custom CGI Application';
end;

function TCustomCGIApplicationDescriptor.GetLocalizedDescription: string;
begin
  Result:='Custom CGI Application'#13#13'A CGI (Common Gateway Interface) program '
          +'in Free Pascal. The program file is '
          +'automatically maintained by Lazarus.';
end;

function TCustomCGIApplicationDescriptor.InitProject(AProject: TLazProject): TModalResult;
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
  NewSource:='program cgiproject1;'+le
    +le
    +'{$mode objfpc}{$H+}'+le
    +le
    +'uses'+le
    +'  Classes,SysUtils,httpDefs,custcgi;'+le
    +le
    +'Type'+le
    +'  TCGIApp = Class(TCustomCGIApplication)'+le
    +'  Public'+le
    +'    Procedure HandleRequest(ARequest : Trequest; AResponse : TResponse); override;'+le
    +'  end;'+le
    +le
    +'Procedure TCGIApp.HandleRequest(ARequest : Trequest; AResponse : TResponse);'+le
    +le
    +'begin'+le
    +'  // Your code here'+le
    +'end;'+le
    +le
    +'begin'+le
    +'  With TCGIApp.Create(Nil) do'+le
    +'    try'+le
    +'      Initialize;'+le
    +'      Run;'+le
    +'    finally'+le
    +'      Free;'+le
    +'    end;'+le
    +'end.'+le;
  AProject.MainFile.SetSourceText(NewSource);

  // add
  AProject.AddPackageDependency('FCL');
  AProject.AddPackageDependency('WebLaz');
  // compiler options
  AProject.LazCompilerOptions.Win32GraphicApp:=false;
  Result:= mrOK;
end;

function TCustomCGIApplicationDescriptor.CreateStartFiles(AProject: TLazProject): TModalResult;
begin
  Result:= mrOK;
end;

{ TFileDescWebDataModule }

constructor TFileDescWebDataModule.Create;
begin
  inherited Create;
  Name:='Web Module';
  ResourceClass:=TFPWebModule;
  UseCreateFormStatements:=true;
end;

function TFileDescWebDataModule.GetInterfaceUsesSection: string;
begin
  Result:=inherited GetInterfaceUsesSection;
  Result:=Result+',LResources,HTTPDefs, websession, fpHTTP,fpWeb';
end;

function TFileDescWebDataModule.GetLocalizedName: string;
begin
  Result:='Web Module';
end;

function TFileDescWebDataModule.GetLocalizedDescription: string;
begin
  Result:='WEB Module'#13
         +'A datamodule for WEB (HTTP) applications.';
end;

function TFileDescWebDataModule.GetImplementationSource(const Filename, SourceName, ResourceName: string): string;

begin
  Result:=Inherited GetImplementationSource(FileName,SourceName,ResourceName);
  Result:=Result+'  RegisterHTTPModule(''T'+ResourceName+''',T'+ResourceName+');'+LineEnding;
end;

{ TFileDescHTMLModule }

constructor TFileDescHTMLModule.Create;
begin
  inherited Create;
  Name:='HTML Module';
  ResourceClass:=TFPHTMLModule;
  UseCreateFormStatements:=true;
end;

function TFileDescHTMLModule.GetInterfaceUsesSection: string;
begin
  Result:=inherited GetInterfaceUsesSection;
  Result:=Result+',LResources,HTTPDefs,websession,fpHTTP,htmlwriter,htmlelements,fphtml';
end;

function TFileDescHTMLModule.GetLocalizedName: string;
begin
  Result:='HTML Web Module';
end;

function TFileDescHTMLModule.GetLocalizedDescription: string;
begin
  Result:='HTML WEB Module'#13
         +'A Web datamodule for producing strict HTML.';
end;

function TFileDescHTMLModule.GetImplementationSource(const Filename, SourceName, ResourceName: string): string;

begin
  Result:=Inherited GetImplementationSource(FileName,SourceName,ResourceName);
  Result:=Result+'  RegisterHTTPModule(''T'+ResourceName+''',T'+ResourceName+');'+LineEnding;
end;

{ TApacheApplicationDescriptor }

constructor TApacheApplicationDescriptor.Create;
begin
  inherited Create;
  Name:='Apache Module';
end;

function TApacheApplicationDescriptor.GetLocalizedName: string;
begin
  Result:='Apache Module';
end;

function TApacheApplicationDescriptor.GetLocalizedDescription: string;
begin
  Result:='Apache module'#13#13'An Apache loadable module '
          +'in Free Pascal. The main library file is '
          +'automatically maintained by Lazarus.';
end;

function TApacheApplicationDescriptor.InitProject(AProject: TLazProject): TModalResult;
var
  le: string;
  NewSource: String;
  MainFile: TLazProjectFile;
begin
  inherited InitProject(AProject);

  MainFile:=AProject.CreateProjectFile('mod_apache1.lpr');
  MainFile.IsPartOfProject:=true;
  AProject.AddFile(MainFile,false);
  AProject.MainFileID:=0;

  // create program source
  le:=LineEnding;
  NewSource:='Library mod_apache1;'+le
    +le
    +'{$mode objfpc}{$H+}'+le
    +le
    +'Uses'+le
    +'  fpWeb,lazweb,httpd,fpApache;'+le
    +le
    +'Const'+le
    +le
    +'{ The following constant is used to export the module record. It must '+le
    +'  always match the name in the LoadModule statement in the apache'+le
    +'  configuration file(s). It is case sensitive !}'+le
    +'  ModuleName=''mod_apache1'';'+le
    +le
    +'{ The following constant is used to determine whether the module will'+le
    +'  handle a request. It should match the name in the SetHandler statement'+le
    +'  in the apache configuration file(s). It is not case sensitive. }'+le
    +le
    +'  HandlerName=ModuleName;'+le
    +le
    +'Var'+le
    +'  DefaultModule : module; {$ifdef unix} public name ModuleName;{$endif unix}'+le
    +le
    +'{$ifdef windows}'+le
    +'Exports defaultmodule name ModuleName;'+le
    +'{$endif windows}'+le
    +le
    +'begin'+le
    +'  Application.Title:=''mod_apache1'';'+le
    +'  Application.ModuleName:=ModuleName;'+le
    +'  Application.HandlerName:=HandlerName;'+le
    +'  Application.SetModuleRecord(DefaultModule);'+le
    +'  Application.Initialize;'+le
    +'end.'+le
    +le;
  AProject.MainFile.SetSourceText(NewSource);

  // add
  AProject.AddPackageDependency('WebLaz');

  // compiler options
  AProject.LazCompilerOptions.Win32GraphicApp:=false;
  Result:= mrOK;
end;

function TApacheApplicationDescriptor.CreateStartFiles(AProject: TLazProject): TModalResult;
begin
  LazarusIDE.DoNewEditorFile(FileDescriptorWebModule,'','',
                         [nfIsPartOfProject,nfOpenInEditor,nfCreateDefaultSrc]);
  Result:= mrOK;
end;


end.

