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

unit SVNAddProjectForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, StdCtrls;

type

  { TSVNAddProjectFrm }

  TSVNAddProjectFrm = class(TForm)
    ActiveCheckBox: TCheckBox;
    RepositoryButton: TButton;
    ButtonPanel1: TButtonPanel;
    ProjectEdit: TEdit;
    RepositoryEdit: TEdit;
    ProjectLabel: TLabel;
    RepositoryLabel: TLabel;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    procedure FormCreate(Sender: TObject);
    procedure RepositoryButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

function ShowSVNAddProjectFrm: TModalResult;

implementation

{$R *.lfm}

uses
  SVNClasses, LazIDEIntf;

function ShowSVNAddProjectFrm: TModalResult;
var
  SVNAddProjectFrm: TSVNAddProjectFrm;
begin
  SVNAddProjectFrm := TSVNAddProjectFrm.Create(nil);

  SVNAddProjectFrm.ProjectEdit.Text:=LazarusIDE.ActiveProject.ProjectInfoFile;
  SVNAddProjectFrm.RepositoryEdit.Text:=LazarusIDE.ActiveProject.CustomSessionData.Values[SVN_REPOSITORY];

  try
    SVNAddProjectFrm.ActiveCheckBox.Checked:=StrToBool(LazarusIDE.ActiveProject.CustomSessionData.Values[SVN_ACTIVE]);
  except
    SVNAddProjectFrm.ActiveCheckBox.Checked:=False;
  end;

  Result := SVNAddProjectFrm.ShowModal;

  if Result = mrOK then
  begin
    LazarusIDE.ActiveProject.CustomSessionData.Values[SVN_REPOSITORY] := SVNAddProjectFrm.RepositoryEdit.Text;
    LazarusIDE.ActiveProject.CustomSessionData.Values[SVN_ACTIVE] := BoolToStr(SVNAddProjectFrm.ActiveCheckBox.Checked);
    LazarusIDE.ActiveProject.Modified:=True;
  end;

  SVNAddProjectFrm.Free;
end;

{ TSVNAddProjectFrm }

procedure TSVNAddProjectFrm.FormCreate(Sender: TObject);
begin
  ProjectLabel.Caption := rsProjectFilename;
  RepositoryLabel.Caption := rsRepositoryPath;
  ActiveCheckBox.Caption:=rsProjectIsActive;
  Caption:=rsProjectOptions;
end;

procedure TSVNAddProjectFrm.RepositoryButtonClick(Sender: TObject);
begin
  if SelectDirectoryDialog.Execute then
    RepositoryEdit.Text := SelectDirectoryDialog.FileName;
end;

end.

