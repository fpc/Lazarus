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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons,
  LazarusIDEStrConsts, IDEImagesIntf, EnvironmentOpts, StdCtrls;

type
  TProjectWizardSelectionType = (
    tpws_new,
    tpws_open,
    tpws_openRecent,
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
  private
    FResult: TProjectWizardSelectionType;
  public
    property Result: TProjectWizardSelectionType read FResult;
  end; 

function ShowProjectWizardDlg(out ARecentProject: String): TProjectWizardSelectionType;

implementation

{$R *.lfm}

function ShowProjectWizardDlg(out ARecentProject: String): TProjectWizardSelectionType;
var
  ProjectWizardDialog: TProjectWizardDialog;
begin
  Result := tpws_closeIDE;
  ARecentProject := '';
  ProjectWizardDialog := TProjectWizardDialog.create(nil);
  with ProjectWizardDialog do
  begin
    Caption:=lisProjectWizard;
    btnNewProject.caption:=lisPWNewProject;
    btnOpenProject.caption:=lisPWOpenProject;
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
    ARecentProject := ProjectWizardDialog.cbRecentProjects.Text;
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

end.

