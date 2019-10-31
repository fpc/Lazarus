unit IDETemplateProject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ContNrs,
  // LCL
  LResources, Forms, Controls, Graphics, Dialogs,
  // LazUtils
  LazFileUtils,
  // IdeIntf
  ProjectIntf, NewItemIntf, MenuIntf, BaseIDEIntf, LazIDEIntf,
  // ProjectTemplates
  ProjectTemplates, frmTemplateSettings, frmTemplateVariables;

type

  { TTemplateProjectDescriptor }

  TTemplateProjectDescriptor = class(TProjectDescriptor)
  Private
    FTemplate : TProjectTemplate;
    FProjectDirectory : String;
    FProjectName : String;
    FIgnoreExts,
    FVariables : TStrings;
    Function ShowOptionsDialog : TModalResult;
  public
    constructor Create(ATemplate : TProjectTemplate); overload;
    destructor destroy; override;
    Function DoInitDescriptor : TModalResult; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject) : TModalResult; override;
    function CreateStartFiles({%H-}AProject: TLazProject) : TModalResult; override;
    Property template : TProjectTemplate Read FTemplate Write FTemplate;
  end;
  
procedure Register;

implementation

Var
  IDETemplates : TProjectTemplates = nil;
  itmFileNewFromTemplate : TIDEMenuSection;
  MenuList : TObjectList;

Type
  { TIDEObject }

  TIDEObject=Class(TObject)
    FProjDesc : TTemplateProjectDescriptor;
    FProjMenu : TIDEMenuCommand;
    Constructor Create(AProjDesc : TTemplateProjectDescriptor;
                       AProjMenu : TIDEMenuCommand);
  end;

{ TIDEObject }

constructor TIDEObject.Create(AProjDesc: TTemplateProjectDescriptor;
                              AProjMenu: TIDEMenuCommand);

begin
  FPRojDesc:=AProjDesc;
  FPRojMenu:=AProjMenu;
end;

Const
  STemplateSettings = 'itmTemplateSettings';
  SItmtemplate = 'itmTemplate';
  
Resourcestring
  STemplateCategory = 'Template projects';
  SProjectTemplateSettings = 'Project templates options ...';
  SNewFromTemplate = 'New project from template';

{ ---------------------------------------------------------------------
  Configuration
  ---------------------------------------------------------------------}

Function GetTemplateDir : String;

begin
  With GetIDEConfigStorage('projtemplate.xml',True) do
    try
      Result:=GetValue('TemplateDir',AppendPathDelim(LazarusIDE.GetPrimaryConfigPath)+'templates');
    Finally
      Free;
    end;
end;

procedure SaveTemplateSettings;

begin
  With GetIDEConfigStorage('projtemplate.xml',False) do
    try
      SetValue('TemplateDir',IDETemplates.TemplateDir);
      WriteToDisk;
    Finally
      Free;
    end;
end;

{ ---------------------------------------------------------------------
  Registration
  ---------------------------------------------------------------------}

Procedure RegisterTemplateCategory;

begin
  NewIDEItems.Add(TNewIDEItemCategory.Create(STemplateCategory));
end;

Procedure DoProject(Sender : TObject);

Var
  I : Integer;
  Desc : TTemplateProjectDescriptor;

begin
  I:=MenuList.count-1;
  Desc:=Nil;
  While (Desc=Nil) and (I>=0) do
    begin
    With TIDEObject(MenuList[i]) do
      if FProjMenu=Sender then
        Desc:=FProjDesc;
    Dec(i);
    end;
  If Desc<>Nil then
    LazarusIDE.DoNewProject(Desc);
end;

procedure RegisterKnowntemplates;

Var
  I : Integer;
  ATemplate : TProjectTemplate;
  ProjDesc : TTemplateProjectDescriptor;
  ProjMenu : TIDEMenuCommand;

begin
  For I:=0 to IDETemplates.Count-1 do
    begin
    ATemplate:=IDETemplates[i];
    ProjDesc:=TTemplateProjectDescriptor.Create(ATemplate);
    RegisterProjectDescriptor(ProjDesc,STemplateCategory);
    ProjMenu:=RegisterIDEMenuCommand(itmFileNewFromTemplate,
                                     SItmtemplate+ATemplate.Name,
                                     ATemplate.Name,                                     
                                     Nil,@DoProject,Nil);
    MenuList.Add(TIDEObject.Create(ProjDesc,ProjMenu));
    end;
end;

procedure UnRegisterKnownTemplates;

Var
  I : Integer;

begin
  For I:=MenuList.Count-1 downto 0 do
    begin
    With TIDEObject(MenuList[i]) do
      begin
      ProjectDescriptors.UnregisterDescriptor(FProjDesc);
      FreeAndNil(FProjMenu);
      end;
    MenuList.Delete(I);
    end;
end;

procedure ChangeSettings(Sender : TObject);

begin
  With TTemplateSettingsForm.Create(Application) do
    Try
      Templates:=IDETemplates;
      if ShowModal=mrOK then
        begin
        SaveTemplateSettings;
        UnRegisterKnownTemplates;
        RegisterKnownTemplates;
        end;
    Finally
      Free;
    end;
end;

procedure Register;

begin
  RegisterIdeMenuCommand(itmOptionsDialogs,STemplateSettings,SProjectTemplateSettings,nil,@ChangeSettings);
  itmFileNewFromTemplate:=RegisterIDESubMenu(itmFileNew,
                                             'itmFileFromTemplate',
                                             SNewFromTemplate);
  IDETemplates:=TProjectTemplates.Create(GetTemplateDir);
  RegisterTemplateCategory;
  RegisterKnownTemplates;
end;


{ TTemplateProjectDescriptor }

function TTemplateProjectDescriptor.ShowOptionsDialog : TModalResult;

var
  I: Integer;
  
begin
  With TProjectVariablesForm.Create(Application) do
    try
      Caption:=Caption+' '+FTemplate.Name;
      FVariables.Assign(FTemplate.Variables);
      I:=FVariables.IndexOfName('ProjName');
      if (I<>-1) then
        FVariables.Delete(I);
      I:=FVariables.IndexOfName('ProjDir');
      if (I<>-1) then
        FVariables.Delete(I);
      Templates:=Templates;
      Variables:=FVariables;
      Result:=ShowModal;
      if Result=mrOK then
        begin
        FProjectDirectory:=IncludeTrailingPathDelimiter(ProjectDir);
        FProjectName:=ProjectName;
        FVariables.Values['ProjName']:=FProjectName;
        FVariables.Values['ProjDir']:=FProjectDirectory;
        end;
    finally
      Free;
    end;
end;


constructor TTemplateProjectDescriptor.Create(ATemplate : TProjectTemplate);
begin
  inherited Create;
  FTemplate:=ATemplate;
  If Assigned(FTemplate) then
    Name:=FTemplate.Name
  else
    Name:='Template Project';
  FVariables:=TStringList.Create;
  FIgnoreExts:=TStringList.Create;
  FIgnoreExts.CommaText:='.lpr,.lps,.lfm,.lrs,.ico,.res,.lpi,.bak';
end;

destructor TTemplateProjectDescriptor.destroy;
begin
  FreeAndNil(FIgnoreExts);
  FTemplate:=Nil;
  FreeAndNil(FVariables);
  Inherited;
end;


function TTemplateProjectDescriptor.GetLocalizedName: string;
begin
  Result:=FTemplate.Name;
end;

function TTemplateProjectDescriptor.GetLocalizedDescription: string;
begin
  Result:=FTemplate.Description;
end;


function TTemplateProjectDescriptor.DoInitDescriptor: TModalResult;

begin
  Result:=ShowOptionsDialog;
  If (Result=mrOK) then
    FTemplate.CreateProject(FProjectDirectory,FVariables);
end;


function TTemplateProjectDescriptor.InitProject(AProject: TLazProject) : TModalResult;

Var
  I : Integer;
  AFile: TLazProjectFile;
  FN : String;
  B : Boolean;
  L : TStringList;
  
begin
  AProject.AddPackageDependency('FCL');
  AProject.AddPackageDependency('LCL');
  AProject.Title:=FProjectName;
  AProject.UseAppBundle:=true;
  AProject.UseManifest:=true;
  AProject.LoadDefaultIcon;
  If Assigned(FTemplate) then
    begin
    FTemplate.CreateProjectDirs(FProjectDirectory,FVariables);
    AProject.ProjectInfoFile:=FProjectDirectory+FProjectName+'.lpi';
    For I:=0 to FTemplate.FileCount-1 do
      begin
      FN:=FTemplate.FileNames[I];
      B:=CompareText(ExtractFileExt(FN),'.lpr')=0;
      If B then
        begin
        FN:=FProjectDirectory+FTemplate.TargetFileName(FN,FVariables);
        AFile:=AProject.CreateProjectFile(FN);
        AFile.IsPartOfProject:=true;
        AProject.AddFile(AFile,Not B);
        AProject.MainFileID:=0;
        L:=TStringList.Create;
        try
          FTemplate.CreateFile(I,L,FVariables);
          AFile.SetSourceText(L.Text);
        Finally
          L.Free;
        end;
        end;
      end;
    Result:=mrOK;
    end
  else
    Result:=mrCancel;
end;

Function TTemplateProjectDescriptor.CreateStartFiles(AProject: TLazProject) : TModalresult;

Var
  I : Integer;
  E,FN : String;

begin
  if Assigned(FTemplate) then
    begin
    Result:=mrOK;
    For I:=0 to FTemplate.FileCount-1 do
      begin
      FN:=FTemplate.FileNames[I];
      E:=ExtractFileExt(FN);
      If (FIgnoreExts.IndexOf(E)=-1) then
        begin
        FN:=FProjectDirectory+FTemplate.TargetFileName(FN,FVariables);
        LazarusIDE.DoOpenEditorFile(FN, -1, -1, [ofProjectLoading,ofQuiet,ofAddToProject]);
        end;
      end;
    end
  else
    Result:=mrCancel;
end;

Initialization
  MenuList:=TObjectList.Create;
Finalization
  FreeAndNil(IDETemplates);
  FreeAndNil(MenuList);
end.
