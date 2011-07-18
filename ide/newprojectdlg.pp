{
  Author: Mattias Gaertner

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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Abstract:
    The new project dialog for lazarus.

}
unit NewProjectDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Graphics, Controls, Project, Buttons, ButtonPanel,
  StdCtrls, ProjectIntf, ExtCtrls, LazarusIDEStrConsts,
  IDEHelpIntf;

type

{ TNewProjectDialog }

  TNewProjectDialog = class(TForm)
    ButtonPanel: TButtonPanel;
    DescriptionGroupBox: TGroupBox;
    HelpLabel: TLabel;
    ListBox: TListBox;
    Panel1: TPanel;
    Splitter1: TSplitter;
    procedure HelpButtonClick(Sender: TObject);
    procedure ListBoxDblClick(Sender: TObject);
    procedure ListBoxSelectionChange(Sender: TObject; User: boolean);
  private
    FProjectDescriptor: TProjectDescriptor;
    procedure FillHelpLabel;
    procedure SetupComponents;
  public
    constructor Create(AOwner: TComponent); override;
    property ProjectDescriptor: TProjectDescriptor read FProjectDescriptor;
  end;

function ChooseNewProject(var ProjectDesc: TProjectDescriptor): TModalResult;

implementation

{$R *.lfm}

function ChooseNewProject(var ProjectDesc: TProjectDescriptor):TModalResult;
var
  NewProjectDialog: TNewProjectDialog;
begin
  ProjectDesc:=nil;
  NewProjectDialog:=TNewProjectDialog.Create(nil);
  try
    Result:=NewProjectDialog.ShowModal;
    if Result=mrOk then
      ProjectDesc:=NewProjectDialog.ProjectDescriptor;
  finally
    NewProjectDialog.Free;
  end;
end;

{ NewProjectDialog }

constructor TNewProjectDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption:=lisNPCreateANewProject;
  SetupComponents;
  FillHelpLabel;
end;

procedure TNewProjectDialog.FillHelpLabel;
begin
  FProjectDescriptor := TProjectDescriptor(ListBox.Items.Objects[ListBox.ItemIndex]);
  HelpLabel.Caption:=FProjectDescriptor.GetLocalizedDescription;
  HelpLabel.Width:=Self.ClientWidth-HelpLabel.Left-10;
end;

procedure TNewProjectDialog.SetupComponents;
var
  i: integer;
begin
  with ListBox do begin
    with Items do begin
      BeginUpdate;
      for i:=0 to ProjectDescriptors.Count-1 do begin
        if ProjectDescriptors[i].VisibleInNewDialog then
          AddObject(ProjectDescriptors[i].GetLocalizedName, ProjectDescriptors[i]);
      end;
      EndUpdate;
    end;
    ItemIndex:=0;
    OnSelectionChange:=@ListBoxSelectionChange;
  end;

  DescriptionGroupBox.Caption := lisCodeHelpDescrTag;

  ButtonPanel.OKButton.Caption := lisOk;
  ButtonPanel.HelpButton.Caption := lisMenuHelp;
  ButtonPanel.CancelButton.Caption := dlgCancel;
end;

procedure TNewProjectDialog.HelpButtonClick(Sender: TObject);
begin
  LazarusHelp.ShowHelpForIDEControl(Self);
end;

procedure TNewProjectDialog.ListBoxDblClick(Sender: TObject);
begin
  if ListBox.ItemAtPos(ListBox.ScreenToClient(Mouse.CursorPos),true) >= 0 then
    ModalResult:=mrOk;
end;

procedure TNewProjectDialog.ListBoxSelectionChange(Sender: TObject;
  User: boolean);
begin
  FillHelpLabel;
end;

end.

