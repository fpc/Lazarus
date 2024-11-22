unit reglazopenapi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazIDEIntf, IDECommands;

procedure register;

implementation

uses MenuIntf, frmopenapiwizard, forms, controls, fpopenapi.reader, fpopenapi.objects, fpopenapi.codegen;

Resourcestring
  SCMDOpenAPIWizard = 'ShowOpenAPICodeGenerator';
  SCMDOpenAPIWizardCaption = 'OpenAPI code generator...';

Type

  { TLazOpenAPICodeGen }

  TLazOpenAPICodeGen = class(TOpenAPICodeGen)
    Function ResolveFullFileName(aKind : TUnitKind) : String;
  end;

Procedure GenerateFiles(const aOpenAPIFile, aBaseOutputFile : string; aGenerator : TLazOpenAPICodeGen);

var
  Loader : TOpenAPIReader;
  API : TOpenAPI;

begin
  Loader:=Nil;
  API:=TOpenAPI.Create;
  try
    Loader:=TOpenAPIReader.Create(Nil);
    Loader.ReadFromFile(API,aOpenAPIFile);
    aGenerator.API:=API;
    aGenerator.BaseOutputFileName:=aBaseOutputFile;
    aGenerator.Execute;
  finally
    Loader.Free;
    API.Free;
  end;
end;

procedure ShowOpenAPIWizard(Sender: TObject);

var
  opts : TOpenFlags;

var
  frm : TOpenapiWizardForm;
  lGenerator : TLazOpenAPICodeGen;

begin
  opts:=[ofOnlyIfExists, ofRevert, ofAddToRecent, ofRegularFile];

  frm:=Nil;
  lGenerator:=TLazOpenAPICodeGen.Create(Application);
  try
    frm:=TOpenapiWizardForm.Create(Application);
    frm.InitFileNameEdits(ExtractFilePath(lazarusIDE.ActiveProject.ProjectInfoFile));
    frm.Generator:=lGenerator;
    if frm.ShowModal=mrOK then
      begin
      if frm.AddToProject then
        Include(opts,ofAddToProject);
      GenerateFiles(frm.OpenAPIFileName,frm.BaseFileName,lGenerator);
      if frm.OpenGeneratedFiles then
        begin
        LazarusIDE.DoOpenEditorFile(lGenerator.ResolveFullFileName(ukDto),-1,-1,opts);
        LazarusIDE.DoOpenEditorFile(lGenerator.ResolveFullFileName(ukSerialize),-1,-1,opts);
        if lGenerator.GenerateClient then
          begin
          LazarusIDE.DoOpenEditorFile(lGenerator.ResolveFullFileName(ukClientServiceIntf),-1,-1,opts);
          LazarusIDE.DoOpenEditorFile(lGenerator.ResolveFullFileName(ukClientServiceImpl),-1,-1,opts);
          end;
        if lGenerator.GenerateServer then
          begin
          LazarusIDE.DoOpenEditorFile(lGenerator.ResolveFullFileName(ukServerServiceHandler),-1,-1,opts);
          LazarusIDE.DoOpenEditorFile(lGenerator.ResolveFullFileName(ukServerServiceImpl),-1,-1,opts);
          end;
        end;
      end;
  finally
    lGenerator.Free;
    frm.Free;
  end;
end;



procedure register;

var
  CmdToolsMenu : TIDECommandCategory;
  OpenAPIWizardCommand : TIDECommand;

begin
  // search shortcut category
  CmdToolsMenu:=IDECommandList.FindCategoryByName(CommandCategoryToolMenuName);
  // register shortcut
  OpenAPIWizardCommand:=RegisterIDECommand(CmdToolsMenu,
    SCMDOpenAPIWizard,
    SCMDOpenAPIWizardCaption,
    CleanIDEShortCut,
    CleanIDEShortCut, nil, @ShowOpenAPIWizard);
  // register menu item in View menu
  RegisterIDEMenuCommand(itmCustomTools,
    SCMDOpenAPIWizard,
    SCMDOpenAPIWizardCaption, nil, nil, OpenAPIWizardCommand);

end;

{ TLazOpenAPICodeGen }

function TLazOpenAPICodeGen.ResolveFullFileName(aKind: TUnitKind): String;
begin
  Result:=ResolveUnit(aKind,True);
end;

end.

