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

{$IFNDEF VER2_2}
  {$DEFINE HasFastCGISupport}
{$ENDIF VER2_2}

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
  { TApacheApplicationDescriptor }

  TApacheApplicationDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;

  { TCustomCGIApplicationDescriptor }
  TCustomCGIApplicationDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;

{$IFDEF HasFastCGISupport}
  { TCustomFCGIApplicationDescriptor }
  TCustomFCGIApplicationDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;

  { TFCGIApplicationDescriptor }

  TFCGIApplicationDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;
{$ENDIF HasFastCGISupport}

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
{$IFDEF HasFastCGISupport}
  ProjectDescriptorCustomFCGIApplication: TCustomFCGIApplicationDescriptor;
  ProjectDescriptorFCGIApplication: TFCGIApplicationDescriptor;
{$ENDIF HasFastCGISupport}
  FileDescriptorHTMLModule: TFileDescHTMLModule;
  FileDescriptorWebModule: TFileDescWebDataModule;

procedure Register;

resourcestring
  rsCGIApplicati = 'CGI Application';
  rsCGIApplicati2 = 'CGI Application%sA CGI (Common Gateway Interface) '
    +'program in Free Pascal using webmodules. The program source is '
    +'automatically maintained by Lazarus.';
  rsCustomCGIApp = 'Custom CGI Application';
  rsCustomCGIApp2 = 'Custom CGI Application%sA CGI (Common Gateway Interface) '
    +'program in Free Pascal. The program source is automatically maintained '
    +'by Lazarus.';
  rsWebModule = 'Web Module';
  rsWEBModuleADa = 'WEB Module%sA datamodule for WEB (HTTP) applications.';
  rsHTMLWebModul = 'HTML Web Module';
  rsHTMLWEBModul2 = 'HTML WEB Module%sA Web datamodule for producing strict '
    +'HTML.';
  rsApacheModule = 'Apache Module';
  rsApacheModule2 = 'Apache module%sAn Apache loadable module in Free Pascal '
    +'using webmodules. The main library file is automatically maintained by '
    +'Lazarus.';
  rsCustomFastCG = 'Custom FastCGI Application';
  rsCustomFastCG2 = 'Custom FastCGI Application%sA FastCGI (Common Gateway '
    +'Interface) program in Free Pascal. The program source is automatically '
    +'maintained by Lazarus.';
  rsFastCGIAppli = 'FastCGI Application';
  rsFastCGIAppli2 = 'FastCGI Application%sA FastCGI (Common Gateway '
    +'Interface) program in Free Pascal using webmodules. The program source '
    +'is automatically maintained by Lazarus.';

implementation

uses LazarusPackageIntf,FormEditingIntf;

Const
  fpWebTab = 'fpWeb';

Procedure RegisterHTMLComponents;
begin
  RegisterComponents(fpWebTab,[THTMLDatasetContentProducer,
                               THTMLSelectProducer,
                               THTMLDatasetSelectProducer,
                               THTMLEntityProducer
                               {$IFNDEF VER2_2},THTMLPageProducer{$ENDIF}
                               ])

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
{$IFDEF HasFastCGISupport}
  ProjectDescriptorFCGIApplication:=TFCGIApplicationDescriptor.Create;
  RegisterProjectDescriptor(ProjectDescriptorFCGIApplication);
  ProjectDescriptorCustomFCGIApplication:=TCustomFCGIApplicationDescriptor.Create;
  RegisterProjectDescriptor(ProjectDescriptorCustomFCGIApplication);
{$ENDIF HasFastCGISupport}
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
  Result:=rsCGIApplicati;
end;

function TCGIApplicationDescriptor.GetLocalizedDescription: string;
begin
  Result:=Format(rsCGIApplicati2, [#13#13]);
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
    +'  fpCGI;'+le
    +le
    +'begin'+le
    +'  Application.Title:=''cgiproject1'';'+le
    +'  Application.Initialize;'+le
    +'  Application.Run;'+le
    +'end.'+le
    +le;
  AProject.MainFile.SetSourceText(NewSource);

  // add
  AProject.AddPackageDependency('FCL');
  AProject.AddPackageDependency('LCL');
  AProject.AddPackageDependency('WebLaz');

  // compiler options
  AProject.LazCompilerOptions.Win32GraphicApp:=false;
  AProject.Flags := AProject.Flags - [pfMainUnitHasCreateFormStatements];
  AProject.Flags := AProject.Flags - [pfRunnable];
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
  Result:=rsCustomCGIApp;
end;

function TCustomCGIApplicationDescriptor.GetLocalizedDescription: string;
begin
  Result:=Format(rsCustomCGIApp2, [#13#13]);
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
  AProject.Flags := AProject.Flags - [pfMainUnitHasCreateFormStatements];
  AProject.Flags := AProject.Flags - [pfRunnable];
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
  Result:=Result+',HTTPDefs, websession, fpHTTP,fpWeb';
end;

function TFileDescWebDataModule.GetLocalizedName: string;
begin
  Result:=rsWebModule;
end;

function TFileDescWebDataModule.GetLocalizedDescription: string;
begin
  Result:=Format(rsWEBModuleADa, [#13]);
end;

function TFileDescWebDataModule.GetImplementationSource(const Filename, SourceName, ResourceName: string): string;

begin
  Result:=Inherited GetImplementationSource(FileName,SourceName,ResourceName);
  if GetResourceType = rtRes then
    Result:=Result+'initialization'+LineEnding;
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
  Result:=Result+',HTTPDefs,websession,fpHTTP,htmlwriter,htmlelements,fphtml';
end;

function TFileDescHTMLModule.GetLocalizedName: string;
begin
  Result:=rsHTMLWebModul;
end;

function TFileDescHTMLModule.GetLocalizedDescription: string;
begin
  Result:=Format(rsHTMLWEBModul2, [#13]);
end;

function TFileDescHTMLModule.GetImplementationSource(const Filename, SourceName, ResourceName: string): string;

begin
  Result:=Inherited GetImplementationSource(FileName,SourceName,ResourceName);
  if GetResourceType = rtRes then
    Result:=Result+'initialization'+LineEnding;
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
  Result:=rsApacheModule;
end;

function TApacheApplicationDescriptor.GetLocalizedDescription: string;
begin
  Result:=Format(rsApacheModule2, [#13#13]);
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
    +'{$ifdef unix}'+le
    +'  cthreads,'+le
    +'{$endif}'+le
    +'  httpd,fpApache;'+le
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
    +'Exports defaultmodule name ModuleName;'+le
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
  AProject.ExecutableType:=petLibrary;
  AProject.Flags := AProject.Flags - [pfMainUnitHasCreateFormStatements];
  AProject.Flags := AProject.Flags - [pfRunnable];
  Result:= mrOK;
end;

function TApacheApplicationDescriptor.CreateStartFiles(AProject: TLazProject): TModalResult;
begin
  LazarusIDE.DoNewEditorFile(FileDescriptorWebModule,'','',
                         [nfIsPartOfProject,nfOpenInEditor,nfCreateDefaultSrc]);
  Result:= mrOK;
end;

{$IFDEF HasFastCGISupport}

{ TCustomFCGIApplicationDescriptor }

constructor TCustomFCGIApplicationDescriptor.Create;
begin
  inherited Create;
  Name:='Custom FastCGI Application';
end;

function TCustomFCGIApplicationDescriptor.GetLocalizedName: string;
begin
  Result:=rsCustomFastCG;
end;

function TCustomFCGIApplicationDescriptor.GetLocalizedDescription: string;
begin
  Result:=Format(rsCustomFastCG2, [#13#13]);
end;

function TCustomFCGIApplicationDescriptor.InitProject(AProject: TLazProject
  ): TModalResult;
var
  le: string;
  NewSource: String;
  MainFile: TLazProjectFile;
begin
  inherited InitProject(AProject);

  MainFile:=AProject.CreateProjectFile('fcgiproject1.lpr');
  MainFile.IsPartOfProject:=true;
  AProject.AddFile(MainFile,false);
  AProject.MainFileID:=0;

  // create program source
  le:=LineEnding;
  NewSource:='program fcgiproject1;'+le
    +le
    +'{$mode objfpc}{$H+}'+le
    +le
    +'uses'+le
    +'  Classes,SysUtils,httpDefs,custfcgi;'+le
    +le
    +'Type'+le
    +'  TFCGIApp = Class(TCustomFCGIApplication)'+le
    +'  Public'+le
    +'    Procedure HandleRequest(ARequest : Trequest; AResponse : TResponse); override;'+le
    +'  end;'+le
    +le
    +'Procedure TFCGIApp.HandleRequest(ARequest : Trequest; AResponse : TResponse);'+le
    +le
    +'begin'+le
    +'  // Your code here'+le
    +'end;'+le
    +le
    +'begin'+le
    +'  With TFCGIApp.Create(Nil) do'+le
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
  AProject.Flags := AProject.Flags - [pfMainUnitHasCreateFormStatements];
  AProject.Flags := AProject.Flags - [pfRunnable];
  Result:= mrOK;
end;

function TCustomFCGIApplicationDescriptor.CreateStartFiles(AProject: TLazProject
  ): TModalResult;
begin
  Result:= mrOK;
end;

{ TFCGIApplicationDescriptor }

constructor TFCGIApplicationDescriptor.Create;
begin
  inherited Create;
  Name:='FastCGI Application';
end;

function TFCGIApplicationDescriptor.GetLocalizedName: string;
begin
  Result:=rsFastCGIAppli;
end;

function TFCGIApplicationDescriptor.GetLocalizedDescription: string;
begin
  Result:=Format(rsFastCGIAppli2, [#13#13]);
end;

function TFCGIApplicationDescriptor.InitProject(AProject: TLazProject
  ): TModalResult;
var
  le: string;
  NewSource: String;
  MainFile: TLazProjectFile;
begin
  inherited InitProject(AProject);

  MainFile:=AProject.CreateProjectFile('fcgiproject1.lpr');
  MainFile.IsPartOfProject:=true;
  AProject.AddFile(MainFile,false);
  AProject.MainFileID:=0;

  // create program source
  le:=LineEnding;
  NewSource:='program fcgiproject1;'+le
    +le
    +'{$mode objfpc}{$H+}'+le
    +le
    +'uses'+le
    +'  fpFCGI;'+le
    +le
    +'begin'+le
    +'  Application.Title:=''fcgiproject1'';'+le
    +'  Application.Initialize;'+le
    +'  Application.Run;'+le
    +'end.'+le
    +le;
  AProject.MainFile.SetSourceText(NewSource);

  // add
  AProject.AddPackageDependency('FCL');
  AProject.AddPackageDependency('LCL');
  AProject.AddPackageDependency('WebLaz');

  // compiler options
  AProject.LazCompilerOptions.Win32GraphicApp:=false;
  AProject.Flags := AProject.Flags - [pfMainUnitHasCreateFormStatements];
  AProject.Flags := AProject.Flags - [pfRunnable];
  Result:= mrOK;
end;

function TFCGIApplicationDescriptor.CreateStartFiles(AProject: TLazProject
  ): TModalResult;
begin
  LazarusIDE.DoNewEditorFile(FileDescriptorWebModule,'','',
                         [nfIsPartOfProject,nfOpenInEditor,nfCreateDefaultSrc]);
  Result:= mrOK;
end;
{$ENDIF HasFastCGISupport}

end.

