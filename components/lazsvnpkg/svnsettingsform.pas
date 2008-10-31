{ Copyright (C) 2008 Darius Blaszijk

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

unit SVNSettingsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, ComCtrls, SVNClasses, LCLProc, Buttons, SVNAddProjectForm;

type
  { TSVNSettingsFrm }

  TSVNSettingsFrm = class(TForm)
    DeleteButton: TBitBtn;
    EditButton: TBitBtn;
    ButtonPanel: TButtonPanel;
    ProjectsListView: TListView;
    procedure DeleteButtonClick(Sender: TObject);
    procedure EditButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure UpdateProjectListView;
  end;

procedure ShowSVNSettingsFrm;

var
  SVNSettingsFrm: TSVNSettingsFrm;

implementation

procedure ShowSVNSettingsFrm;
begin
  SVNSettingsFrm.ShowModal;
end;

{ TSVNSettingsFrm }

procedure TSVNSettingsFrm.FormShow(Sender: TObject);
begin
  UpdateProjectListView;
end;

procedure TSVNSettingsFrm.UpdateProjectListView;
var
  count: integer;
  i: integer;
begin
  ProjectsListView.Clear;

  count := SVNSettings.ProjectCount;

  for i := 0 to Count - 1 do
    with ProjectsListView.Items.Add do
    begin
      Caption := SVNSettings.Path[i];
      Checked := SVNSettings.Active[i];
      SubItems.Add(SVNSettings.Repository[i]);
    end;
end;

procedure TSVNSettingsFrm.FormCreate(Sender: TObject);
begin
  SetColumn(ProjectsListView, 0, 250, rsProjectName);
  SetColumn(ProjectsListView, 1, 250, rsRepositoryPath);

  EditButton.Caption := rsEdit;
  DeleteButton.Caption := rsDelete;
end;

procedure TSVNSettingsFrm.EditButtonClick(Sender: TObject);
var
  AProject: string;
  ARepository: string;
  AChecked: boolean;
begin
  if Assigned(ProjectsListView.Selected) then
  begin
    AProject:=ProjectsListView.Selected.Caption;
    ARepository:=ProjectsListView.Selected.SubItems[0];
    AChecked:=ProjectsListView.Selected.Checked;

    if ShowSVNAddProjectFrm(AProject, ARepository, AChecked) = mrOK then
      UpdateProjectListView;
  end;
end;

procedure TSVNSettingsFrm.DeleteButtonClick(Sender: TObject);
begin
  if Assigned(ProjectsListView.Selected) then
  begin
    SVNSettings.DeleteProjectByIndex(ProjectsListView.Selected.Index);
    UpdateProjectListView;
  end;
end;

initialization
  {$I svnsettingsform.lrs}

end.

