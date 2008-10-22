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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner
  
  Abstract:
    This unit defines a dialog for the lazarus options.
}
unit IdeOptionsDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, LResources, ComCtrls, ButtonPanel,
  EnvironmentOpts, LazarusIDEStrConsts, IDEWindowIntf, IDEOptionsIntf;

type
  { TIDEOptionsDialog }

  TIDEOptionsDialog = class(TForm)
    ButtonPanel: TButtonPanel;
    CategoryTree: TTreeView;

    procedure CategoryTreeChange(Sender: TObject; Node: TTreeNode);
    procedure HelpButtonClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private
    FOnLoadOptions: TOnLoadIDEOptions;
    FOnSaveOptions: TOnSaveIDEOptions;
    PrevEditor: TAbstractIDEOptionsEditor;

    function CheckValues: boolean;
    procedure LoadIDEOptions(Sender: TObject; AOptions: TAbstractIDEOptions);
    procedure SaveIDEOptions(Sender: TObject; AOptions: TAbstractIDEOptions);
    procedure CreateEditors;
  published
    property OnLoadIDEOptions: TOnLoadIDEOptions read FOnLoadOptions write FOnLoadOptions;
    property OnSaveIDEOptions: TOnSaveIDEOptions read FOnSaveOptions write FOnSaveOptions;
  public
    constructor Create(AOwner: TComponent); override;
    procedure OpenEditor(AEditor: TAbstractIDEOptionsEditorClass);
    procedure ReadSettings(AOptions: TAbstractIDEOptions);
    procedure WriteSettings(AOptions: TAbstractIDEOptions);
  end;

implementation

uses
  IDEContextHelpEdit;

{ TIDEOptionsDialog }

constructor TIDEOptionsDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  PrevEditor := nil;

  IDEDialogLayoutList.ApplyLayout(Self, Width, Height);
  Caption := lisMenuGeneralOptions;

  CreateEditors;

  ButtonPanel.OKButton.OnClick := @OKButtonClick;
  ButtonPanel.CancelButton.OnClick := @CancelButtonClick;
  ButtonPanel.HelpButton.OnClick := @HelpButtonClick;

  if CategoryTree.Items.Count > 0 then
    CategoryTree.Selected := CategoryTree.Items.GetFirstNode;
end;

procedure TIDEOptionsDialog.HelpButtonClick(Sender: TObject);
begin
  ShowContextHelpForIDE(Self);
end;

procedure TIDEOptionsDialog.CategoryTreeChange(Sender: TObject;
  Node: TTreeNode);
var
  AEditor: TAbstractIDEOptionsEditor;
begin
  while Node <> nil do
  begin
    if Node.Data <> nil then
      break;
    Node := Node.GetFirstChild;
  end;

  AEditor := TAbstractIDEOptionsEditor(Node.Data);
  if (AEditor <> nil) and (AEditor <> PrevEditor) then
  begin
    if PrevEditor <> nil then
      PrevEditor.Parent := nil;

    AEditor.Parent := Self;
    AEditor.Anchors := [akLeft, akTop, akRight, akBottom];
    AEditor.AnchorSideLeft.Side := asrBottom;
    AEditor.AnchorSideLeft.Control := CategoryTree;
    AEditor.AnchorSideTop.Control := Self;
    AEditor.AnchorSideRight.Side := asrBottom;
    AEditor.AnchorSideRight.Control := Self;
    AEditor.AnchorSideBottom.Side := asrTop;
    AEditor.AnchorSideBottom.Control := ButtonPanel;
    AEditor.BorderSpacing.Around := 6;
    AEditor.Visible := True;

    PrevEditor := AEditor;
  end;
end;

procedure TIDEOptionsDialog.OkButtonClick(Sender: TObject);
begin
  if not CheckValues then
    Exit;
  IDEDialogLayoutList.SaveLayout(Self);
  ModalResult := mrOk;
end;

procedure TIDEOptionsDialog.CancelButtonClick(Sender: TObject);
begin
  IDEDialogLayoutList.SaveLayout(Self);
  ModalResult := mrCancel;
end;

procedure TIDEOptionsDialog.ReadSettings(AOptions: TAbstractIDEOptions);

  procedure Traverse(Node: TTreeNode);
  begin
    if Node <> nil then
    begin
      if Node.Data <> nil then
        with TAbstractIDEOptionsEditor(Node.Data) do
          if SupportedOptionsClass = AOptions.ClassType then
            ReadSettings(AOptions);
      Traverse(Node.GetFirstChild);
      Traverse(Node.GetNextSibling);
    end;
  end;

begin
  Traverse(CategoryTree.Items.GetFirstNode);
end;

procedure TIDEOptionsDialog.WriteSettings(AOptions: TAbstractIDEOptions);

  procedure Traverse(Node: TTreeNode);
  begin
    if Node <> nil then
    begin
      if Node.Data <> nil then
        with TAbstractIDEOptionsEditor(Node.Data) do
          if SupportedOptionsClass = AOptions.ClassType then
            WriteSettings(AOptions);
      Traverse(Node.GetFirstChild);
      Traverse(Node.GetNextSibling);
    end;
  end;

begin
  Traverse(CategoryTree.Items.GetFirstNode);
end;

function TIDEOptionsDialog.CheckValues: boolean;

  function Traverse(Node: TTreeNode): Boolean;
  begin
    if Node <> nil then
    begin
      if Node.Data <> nil then
        Result := TAbstractIDEOptionsEditor(Node.Data).Check
      else
        Result := True;

      Result := Result and
                Traverse(Node.GetFirstChild) and
                Traverse(Node.GetNextSibling);
    end
    else
      Result := True;
  end;

begin
  Result := Traverse(CategoryTree.Items.GetFirstNode);
end;

procedure TIDEOptionsDialog.LoadIDEOptions(Sender: TObject; AOptions: TAbstractIDEOptions);
begin
  if Assigned(OnLoadIDEOptions) then
    OnLoadIDEOptions(Self, AOptions);
  ReadSettings(AOptions);
end;

procedure TIDEOptionsDialog.SaveIDEOptions(Sender: TObject; AOptions: TAbstractIDEOptions);
begin
  WriteSettings(AOptions);
  if Assigned(OnSaveIDEOptions) then
    OnSaveIDEOptions(Self, AOptions);
end;

procedure TIDEOptionsDialog.CreateEditors;
var
  Instance: TAbstractIDEOptionsEditor;
  GroupNode, ItemNode: TTreeNode;
  i, j: integer;
  Rec: PIDEOptionsGroupRec;
begin
  IDEEditorGroups.Resort;

  for i := 0 to IDEEditorGroups.Count - 1 do
  begin
    Rec := IDEEditorGroups[i];
    if Rec^.Items <> nil then
    begin
      GroupNode := CategoryTree.Items.AddChild(nil, Rec^.Caption);
      for j := 0 to Rec^.Items.Count - 1 do
      begin
        Instance := Rec^.Items[j]^.EditorClass.Create(Self);
        Instance.OnLoadIDEOptions := @LoadIDEOptions;
        Instance.OnSaveIDEOptions := @SaveIDEOptions;
        Instance.Setup;

        ItemNode := CategoryTree.Items.AddChild(GroupNode, Instance.GetTitle);
        ItemNode.Data := Instance;
      end;
      GroupNode.Expanded := True;
    end;
  end;
end;

procedure TIDEOptionsDialog.OpenEditor(AEditor: TAbstractIDEOptionsEditorClass);

  function Traverse(ANode: TTreeNode): TTreeNode;
  begin
    Result := nil;
    if ANode <> nil then
    begin
      if (ANode.Data <> nil) and (TObject(ANode.Data).ClassType = AEditor) then
        Result := ANode;
      if Result = nil then
        Result := Traverse(ANode.GetFirstChild);
      if Result = nil then
        Result := Traverse(ANode.GetNextSibling);
    end;
  end;

var
  Node: TTreeNode;
begin
  Node := Traverse(CategoryTree.Items.GetFirstNode);
  if Node <> nil then
    CategoryTree.Selected := Node;
end;

initialization
  {$I ideoptionsdlg.lrs}

end.

