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
  StdCtrls, ProjectIntf, ExtCtrls, ComCtrls, LazarusIDEStrConsts,
  IDEHelpIntf, IDEImagesIntf;

type

{ TNewProjectDialog }

  TNewProjectDialog = class(TForm)
    ButtonPanel: TButtonPanel;
    DescriptionGroupBox: TGroupBox;
    HelpLabel: TLabel;
    Tree: TTreeView;
    Panel1: TPanel;
    Splitter1: TSplitter;
    procedure HelpButtonClick(Sender: TObject);
    procedure TreeDblClick(Sender: TObject);
    procedure TreeSelectionChange(Sender: TObject);
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
  if Assigned(Tree.Selected) then
    if Assigned(Tree.Selected.Data) then
    begin
      FProjectDescriptor:=TProjectDescriptor(Tree.Selected.Data);
      HelpLabel.Caption:=FProjectDescriptor.GetLocalizedDescription;
      ButtonPanel.OKButton.Enabled:=true;
    end
    else
    begin
      FProjectDescriptor:=nil;
      HelpLabel.Caption:=lisChooseOneOfTheseItemsToCreateANewProject;
      ButtonPanel.OKButton.Enabled:=false;
    end;
end;

procedure TNewProjectDialog.SetupComponents;
var
  NIndexTemplate, NIndexFolder: integer;
  RootNode, ItemNode: TTreeNode;
  i: integer;
begin
  Tree.Images:=IDEImages.Images_16;
  NIndexFolder:=IDEImages.LoadImage(16, 'folder');
  NIndexTemplate:=IDEImages.LoadImage(16, 'template');

  with Tree do begin
    Items.BeginUpdate;

    RootNode:=Items.Add(nil, dlgProject);
    RootNode.ImageIndex:=NIndexFolder;
    RootNode.SelectedIndex:=NIndexFolder;

    for i:=0 to ProjectDescriptors.Count-1 do
      if ProjectDescriptors[i].VisibleInNewDialog then
      begin
        ItemNode:=Items.AddChildObject(RootNode, ProjectDescriptors[i].GetLocalizedName, ProjectDescriptors[i]);
        ItemNode.ImageIndex:=NIndexTemplate;
        ItemNode.SelectedIndex:=NIndexTemplate;
      end;

    FullExpand;
    Items.EndUpdate;

    //select first child node
    if Items.Count>0 then
      Selected:=Items[1];
  end;

  DescriptionGroupBox.Caption:=lisCodeHelpDescrTag;
  ButtonPanel.OKButton.Caption:=lisOk;
  ButtonPanel.CancelButton.Caption:=lisCancel;
  ButtonPanel.HelpButton.Caption:=lisHelp;
end;

procedure TNewProjectDialog.HelpButtonClick(Sender: TObject);
begin
  LazarusHelp.ShowHelpForIDEControl(Self);
end;

procedure TNewProjectDialog.TreeDblClick(Sender: TObject);
var
  Pnt: TPoint;
begin
  Pnt:=Tree.ScreenToClient(Mouse.CursorPos);
  if Assigned(Tree.GetNodeAt(Pnt.X, Pnt.Y)) then
    ModalResult:=mrOk;
end;

procedure TNewProjectDialog.TreeSelectionChange(Sender: TObject);
begin
  FillHelpLabel;
end;

end.

