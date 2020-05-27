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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Abstract:
    The new project dialog for lazarus.

}
unit NewProjectDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, Graphics, Controls, Buttons, ButtonPanel, StdCtrls, ExtCtrls, ComCtrls,
  // BuildIntf
  ProjectIntf,
  // IdeIntf
  IDEHelpIntf, IDEImagesIntf,
  // IDE
  LazarusIDEStrConsts, Project;

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
    procedure OkClick(Sender: TObject);
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
var
  ANode: TTreeNode;
begin
  ANode := Tree.Selected;
  if Assigned(ANode) and Assigned(ANode.Data) then
  begin
    FProjectDescriptor:=TProjectDescriptor(ANode.Data);
    HelpLabel.Caption:=FProjectDescriptor.GetLocalizedName + LineEnding+LineEnding
                      +FProjectDescriptor.GetLocalizedDescription;
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
  NIndexFolder:=IDEImages.LoadImage('folder');
  NIndexTemplate:=IDEImages.LoadImage('template');

  Tree.Items.BeginUpdate;
  RootNode:=Tree.Items.Add(nil, dlgProject);
  RootNode.ImageIndex:=NIndexFolder;
  RootNode.SelectedIndex:=NIndexFolder;
  for i:=0 to ProjectDescriptors.Count-1 do
    if ProjectDescriptors[i].VisibleInNewDialog then
    begin
      ItemNode:=Tree.Items.AddChildObject(RootNode, ProjectDescriptors[i].GetLocalizedName,
                                                    ProjectDescriptors[i]);
      ItemNode.ImageIndex:=NIndexTemplate;
      ItemNode.SelectedIndex:=NIndexTemplate;
    end;
  Tree.FullExpand;
  Tree.Items.EndUpdate;

  //select first child node
  with Tree do
    if Items.Count>0 then
      Selected:=Items[1];

  DescriptionGroupBox.Caption:=lisCodeHelpDescrTag;
end;

procedure TNewProjectDialog.HelpButtonClick(Sender: TObject);
begin
  LazarusHelp.ShowHelpForIDEControl(Self);
end;

procedure TNewProjectDialog.OkClick(Sender: TObject);
var
  ANode: TTreeNode;
begin
  ANode := Tree.Selected;
  if Assigned(ANode) and Assigned(ANode.Data) then
    ModalResult:=mrOk
  else
    ModalResult:=mrNone;
end;

procedure TNewProjectDialog.TreeSelectionChange(Sender: TObject);
begin
  FillHelpLabel;
end;

end.

