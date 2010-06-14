unit frmTemplateVariables;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Grids, ProjectTemplates, Buttons, StdCtrls, EditBtn;

type

  { TProjectVariablesForm }

  TProjectVariablesForm = class(TForm)
    BOK: TButton;
    BCancel: TButton;
    DEProject: TDirectoryEdit;
    EProjectName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    PDescription: TPanel;
    SGVariables: TStringGrid;
    procedure BOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ProjectVariablesFormShow(Sender: TObject);
  private
    FSChanged: Boolean;
    FTemplates: TProjectTemplates;
    { private declarations }
    FVariables : TStrings;
    function GetProjectDir: String;
    function GetProjectName: String;
    procedure SetVariables(const AValue: TStrings);
  public
    { public declarations }
    Property Templates : TProjectTemplates Read FTemplates Write FTemplates;
    Property ProjectName : String Read GetProjectName;
    Property ProjectDir : String Read GetProjectDir;
    Property Variables : TStrings Read FVariables Write SetVariables;
    Property SettingsChanged: Boolean Read FSChanged Write FSChanged;
  end;

var
  ProjectVariablesForm: TProjectVariablesForm;

implementation

{$R *.lfm}

resourcestring
  SVariable    = 'Variable';
  SValue       = 'Value';
  SDescription = 'Description';
  SNoAdditionalVars = 'This project has no additional variables.';
  //
  SNameforProject = '&Name for new project:';
  SCreateinDir    = 'Create in &directory:';
  SThisProject    = 'This project contains some additional variables. Please provide values for these variables.';
  STitle          = 'New project from template';
  SbtnOK          = 'OK';
  SbtnCancel      = 'Cancel';

{ TProjectVariablesForm }

procedure TProjectVariablesForm.ProjectVariablesFormShow(Sender: TObject);
begin
  SGVariables.Cells[0,0]:=SVariable;
  SGVariables.Cells[1,0]:=SValue;
  SGVariables.Cells[2,0]:=SDescription;
end;

procedure TProjectVariablesForm.BOKClick(Sender: TObject);

Var
  N,V : String;
  I : Integer;

begin
  For I:=0 to FVariables.Count-1 do
    begin
    V:='';
    N:='';
    FVariables.GetNameValue(I,N,V);
    V:=SGVariables.Cells[1,I+1];
    FVariables[i]:=N+'='+V;
    end;
end;

procedure TProjectVariablesForm.FormCreate(Sender: TObject);
begin
  Caption := STitle;
  Label1.Caption:= SNameforProject;
  Label2.Caption:= SCreateinDir;
  PDescription.Caption:= SThisProject;
  BCancel.Caption:= SbtnCancel;
  BOK.Caption:= SbtnOK;
end;

procedure TProjectVariablesForm.SetVariables(const AValue: TStrings);

Var
  N,V : String;
  I : Integer;
  
begin
  FVariables:=AValue;
  If (FVariables.Count=0) then
    begin
    SGVariables.Enabled:=False;
    PDescription.Caption:=SNoAdditionalVars;
    end
  else
    begin
    SGVariables.RowCount:=FVariables.Count+1;
    For I:=1 to FVariables.Count do
      begin
      V:='';
      N:='';
      FVariables.GetNameValue(I-1,N,V);
      SGVariables.Cells[0,I]:=N;
      SGVariables.Cells[1,I]:='';
      SGVariables.Cells[2,I]:=V;
      end;
    end;
end;

function TProjectVariablesForm.GetProjectDir: String;
begin
  Result:=DEProject.Text;
end;

function TProjectVariablesForm.GetProjectName: String;
begin
  Result:=EProjectName.Text;
end;

end.

