{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
 
  Abstract:
    Dialog to open/start a new project.
}
unit ProjectWizardDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, Controls, StdCtrls, Graphics, Dialogs, Buttons,
  // LazUtils
  LazFileUtils, LazLoggerBase,
  // BuildIntf
  PackageIntf,
  // IdeIntf
  IDEWindowIntf, IDEImagesIntf,
  // IDE
  LazarusIDEStrConsts, EnvironmentOpts;

type
  TProjectWizardSelectionType = (
    tpws_new,
    tpws_open,
    tpws_openRecent,
    tpws_droppedProject,
    tpws_examples,
    tpws_convert,
    tpws_closeIDE
  );

  { TProjectWizardDialog }

  TProjectWizardDialog = class(TForm)
    btnExamples: TBitBtn;
    btnNewProject: TBitBtn;
    btnConvertProject: TBitBtn;
    btnCloseIDE: TBitBtn;
    btnOpenProject: TBitBtn;
    cbRecentProjects: TComboBox;
    gbRecent: TGroupBox;
    procedure btnCloseIDEClick(Sender: TObject);
    procedure btnConvertProjectClick(Sender: TObject);
    procedure btnExamplesClick(Sender: TObject);
    procedure btnNewProjectClick(Sender: TObject);
    procedure btnOpenProjectClick(Sender: TObject);
    procedure cbRecentProjectsSelect(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure FormDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
  private
    FResult: TProjectWizardSelectionType;
    FDroppedProjectInfo: string;
  public
    property Result: TProjectWizardSelectionType read FResult;
    property DroppedProjectInfo: string read FDroppedProjectInfo;
  end;

function ShowProjectWizardDlg(out AProjectToOpen: String): TProjectWizardSelectionType;

implementation

{$R *.lfm}

function ShowProjectWizardDlg(out AProjectToOpen: String): TProjectWizardSelectionType;
var
  ProjectWizardDialog: TProjectWizardDialog;
begin
  Result := tpws_closeIDE;
  AProjectToOpen := '';
  ProjectWizardDialog := TProjectWizardDialog.create(nil);
  with ProjectWizardDialog do
  begin
    Caption:=lisProjectWizard;
    btnNewProject.caption:=lisPWNewProject;
    btnOpenProject.caption:=lisPWOpenProject;

    btnExamples.Enabled :=
        (PackageEditingInterface.FindPackageWithName('exampleprojects') <> Nil);

    btnExamples.Caption:=lisPWViewExampleProjects;
    btnConvertProject.caption:=lisPWConvertProject;
    gbRecent.Caption:=lisPWOpenRecentProject;
    btnCloseIDE.caption:=lisQuitLazarus;
    IDEImages.AssignImage(btnNewProject, 'menu_project_new');
    IDEImages.AssignImage(btnOpenProject, 'menu_project_open');
    IDEImages.AssignImage(btnExamples, 'camera');
    IDEImages.AssignImage(btnConvertProject, 'menu_tool_del_to_laz_project');
    IDEImages.AssignImage(btnCloseIDE, 'menu_exit');
    cbRecentProjects.Items.AddStrings(EnvironmentOptions.RecentProjectFiles);
  end;

  try
    if ProjectWizardDialog.ShowModal <> mrOk then
      Exit;
    Result := ProjectWizardDialog.Result;
    case Result of
      tpws_openRecent:
        AProjectToOpen := ProjectWizardDialog.cbRecentProjects.Text;
      tpws_droppedProject:
        AProjectToOpen := ProjectWizardDialog.DroppedProjectInfo;
    end;
  finally
    ProjectWizardDialog.free;
  end;
end;

{ TProjectWizardDialog }

procedure TProjectWizardDialog.btnNewProjectClick(Sender: TObject);
begin
  FResult := tpws_new;
end;

procedure TProjectWizardDialog.btnConvertProjectClick(Sender: TObject);
begin
  FResult := tpws_convert;
end;

procedure TProjectWizardDialog.btnExamplesClick(Sender: TObject);
begin
  FResult := tpws_examples;
end;

procedure TProjectWizardDialog.btnCloseIDEClick(Sender: TObject);
begin
  FResult := tpws_closeIDE;
end;

procedure TProjectWizardDialog.btnOpenProjectClick(Sender: TObject);
begin
  FResult := tpws_open;
end;

procedure TProjectWizardDialog.cbRecentProjectsSelect(Sender: TObject);
begin
  FResult := tpws_openRecent;
  if (Sender as TComboBox).Text<>'' then
    ModalResult:=mrOK;  // Exit dialog if something is selected.
end;

procedure TProjectWizardDialog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(self);
end;

procedure TProjectWizardDialog.FormCreate(Sender: TObject);
begin
  IDEDialogLayoutList.ApplyLayout(self);
end;

// How are FormDragDrop and FormDragOver triggered? Nothing is shown.
procedure TProjectWizardDialog.FormDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  //ShowMessage('TProjectWizardDialog.FormDragDrop: Source=' + Source.ClassName);
  debugln(['TProjectWizardDialog.FormDragDrop: XY=', X,':',Y, ', Sender', Sender, ', Source=', Source]);
end;

procedure TProjectWizardDialog.FormDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  //ShowMessage('TProjectWizardDialog.FormDragOver: Source=' + Source.ClassName + ', State=' + IntToStr(Ord(State)));
  debugln(['TProjectWizardDialog.FormDragOver: XY=', X,':',Y, ', Sender', Sender,
           ', Source=', Source, ', State=', State, ', Accept=', Accept]);
  Accept := True;
end;

// This works when AllowDropFiles is enabled.
procedure TProjectWizardDialog.FormDropFiles(Sender: TObject;
  const FileNames: array of string);
begin
  Assert(Length(FileNames)>0, 'TProjectWizardDialog.FormDropFiles: FileNames is empty.');
  debugln(['TProjectWizardDialog.FormDropFiles File[0]=', FileNames[0]]);
  if FilenameExtIs(FileNames[0], 'lpi') then begin
    FDroppedProjectInfo := FileNames[0];
    FResult := tpws_droppedProject;
    ModalResult := mrOK;
  end;
end;

end.

