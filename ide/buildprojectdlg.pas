unit BuildProjectDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ExtCtrls, StdCtrls, IDEDialogs, LazarusIDEStrConsts, Project;

type
  TBuildCleanMode = (
    bcmIfNeeded,
    bcmNormal,
    bcmWithB  // with -B
    );
  TBuildCleanModes = set of TBuildCleanMode;

  TBuildCleanParams = record
    Mode: TBuildCleanMode;
    CleanOutputDirectory: boolean;
    CleanSrcDirs: string; // file masks separated by semicolon
  end;

  { TBuildProjectDialog }

  TBuildProjectDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    DependenciesGroupBox: TGroupBox;
    Panel1: TPanel;
    PkgClearOutDirsButton: TButton;
    PkgClearSrcDirsButton: TButton;
    PkgClearSrcDirsMaskEdit: TEdit;
    PkgClearSrcDirsMaskLabel: TLabel;
    ProjClearOutDirButton: TButton;
    ProjClearSrcDirMaskEdit: TEdit;
    ProjClearSrcDirMaskLabel: TLabel;
    ProjClearSrcDirsButton: TButton;
    ProjectGroupBox: TGroupBox;
    procedure FormCreate(Sender: TObject);
  private
    FProject: TProject;
  public
    procedure Init(AProject: TProject);
  end;

function ShowBuildProjectDialog(AProject: TProject): TModalResult;

implementation

function ShowBuildProjectDialog(AProject: TProject): TModalResult;
var
  BuildProjectDialog: TBuildProjectDialog;
begin
  BuildProjectDialog:=TBuildProjectDialog.Create(nil);
  try
    BuildProjectDialog.Init(AProject);
    Result:=BuildProjectDialog.ShowModal;
  finally
    BuildProjectDialog.Free;
  end;
end;

{$R *.lfm}

{ TBuildProjectDialog }

procedure TBuildProjectDialog.FormCreate(Sender: TObject);
begin
  Caption:='Build Project';
  PrProjectGroupBoxaption:='Compile project:';
  PrProjectGroupBoxtems.Add('Standard');
  PrProjectGroupBoxtems.Add('with -B');
  PrProjectGroupBoxtems.Add('Clean output directory');
  DeDependenciesGroupBoxaption:='Compile used packages:';
  DeDependenciesGroupBoxtems.Add('If needed');
  DeDependenciesGroupBoxtems.Add('Compile');
  DeDependenciesGroupBoxtems.Add('with -B');
  DeDependenciesGroupBoxtems.Add('Clean output directory');
end;

procedure TBuildProjectDialog.Init(AProject: TProject);
begin

end;

end.

