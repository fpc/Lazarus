unit IDETemplateProject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  ProjectTemplates, ProjectIntf, LazIDEIntf;

type

  { TTemplateProjectDescriptor }

  TTemplateProjectDescriptor = class(TProjectDescriptor)
  Private
    FTemplate : TProjectTemplate;
    FProjectDirectory : String;
    FProjectName : String;
    FVariables : TStrings;
    Function ShowOptionsDialog : TModalResult;
  public
    constructor Create(ATemplate : TProjectTemplate);
    destructor destroy; override;
    Function DoInitDescriptor : TModalResult; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject) : TModalResult; override;
    function CreateStartFiles(AProject: TLazProject) : TModalResult; override;
    Property template : TProjectTemplate Read FTemplate Write FTemplate;
  published
    { Published declarations }
  end;
  
procedure Register;

implementation

uses frmtemplateVariables,ConfigStorage,newitemintf;

Var
  TemplateProjectDescriptor : TTemplateProjectDescriptor;
  IDETemplates : TProjectTemplates;
  
Const
  STemplateCategory = 'Template projects';
  
Procedure RegisterTemplateCategory;

begin
  NewIDEItems.Add(STemplateCategory);
end;

procedure RegisterTemplateProject(ATemplate : TProjectTemplate);

var
  ProjDesc: TTemplateProjectDescriptor;
  
begin
  ProjDesc:=TTemplateProjectDescriptor.Create(Atemplate);
  RegisterProjectDescriptor(ProjDesc,STemplateCategory);
end;

Function GetTemplateDir : String;

begin
  With GetIDEConfigStorage('projtemplate.xml',True) do
    try
      Result:=GetValue('TemplateDir',IncludeTrailingPathDelimiter(LazarusIDE.GetPrimaryConfigPath)+'templates');
    Finally
      Free;
    end;
end;

procedure Register;

Var
  I : Integer;
  D : String;
  
begin
  D:=GetTemplateDir;
  IDETemplates:=TProjectTemplates.Create(D);
  RegisterTemplateCategory;
  For I:=0 to IDETemplates.Count-1 do
    RegisterTemplateProject(IDETemplates[i]);
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


{ TTemplateProjectDescriptor }

function TTemplateProjectDescriptor.ShowOptionsDialog : TModalResult;

var
  I: Integer;
  
begin
  With TProjectVariablesForm.Create(Application) do
    try
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
      if SettingsChanged then
        SaveTemplateSettings;
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
end;

destructor TTemplateProjectDescriptor.destroy;
begin
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
  RFN : String;
  L : TStringList;
  
begin
  AProject.AddPackageDependency('FCL');
  AProject.AddPackageDependency('LCL');
  AProject.Title:=FProjectName;
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
        L:=TstringList.Create;
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
  E,FN,FN2 : String;
  B : Boolean;
  
begin
  if Assigned(FTemplate) then
    begin
    Result:=mrOK;
    For I:=0 to FTemplate.FileCount-1 do
      begin
      FN:=FTemplate.FileNames[I];
      E:=ExtractFileExt(FN);
      If (CompareText(E,'.lpr')<>0)
         and (CompareText(E,'.lfm')<>0) then
        begin
        FN:=FProjectDirectory+FTemplate.TargetFileName(FN,FVariables);
        LazarusIDE.DoOpenEditorFile(FN,-1,[ofAddToProject]);
        end;
      end;
    end
  else
    Result:=mrCancel;
end;

end.
