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
    This unit defines a dialog for the lazarus environment options.

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
    PrevNode: TTreeNode;
    FEditors: TList;

    function CheckValues: boolean;
    procedure LoadIDEOptions(Sender: TObject; AOptions: TAbstractIDEOptions);
    procedure SaveIDEOptions(Sender: TObject; AOptions: TAbstractIDEOptions);
    procedure CreateEditors;
  published
    property OnLoadIDEOptions: TOnLoadIDEOptions read FOnLoadOptions write FOnLoadOptions;
    property OnSaveIDEOptions: TOnSaveIDEOptions read FOnSaveOptions write FOnSaveOptions;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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
  PrevNode := nil;

  IDEDialogLayoutList.ApplyLayout(Self, Width, Height);
  Caption := lisMenuGeneralOptions;

  FEditors := TList.Create;
  CreateEditors;

  ButtonPanel.OKButton.OnClick := @OKButtonClick;
  ButtonPanel.CancelButton.OnClick := @CancelButtonClick;
  ButtonPanel.HelpButton.OnClick := @HelpButtonClick;

  if CategoryTree.Items.Count > 0 then
    CategoryTree.Selected := CategoryTree.Items.GetFirstNode;
end;

destructor TIDEOptionsDialog.Destroy;
begin
  FEditors.Free;
  inherited Destroy;
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
  if PrevNode <> nil then
    TAbstractIDEOptionsEditor(PrevNode.Data).Parent := nil;

  if Node <> nil then
  begin
    AEditor := TAbstractIDEOptionsEditor(Node.Data);

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
  end;
  PrevNode := Node;
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
var
  i: integer;
begin
  for i := 0 to FEditors.Count - 1 do
    TAbstractIDEOptionsEditor(FEditors[i]).ReadSettings(AOptions);
end;

procedure TIDEOptionsDialog.WriteSettings(AOptions: TAbstractIDEOptions);
var
  i: integer;
begin
  for i := 0 to FEditors.Count - 1 do
    TAbstractIDEOptionsEditor(FEditors[i]).WriteSettings(AOptions);
end;

function TIDEOptionsDialog.CheckValues: boolean;
var
  i: integer;
begin
  Result := True;
  for i := 0 to FEditors.Count - 1 do
  begin
    Result := TAbstractIDEOptionsEditor(FEditors[i]).Check;
    if not Result then
      break;
  end;
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
  ANode: TTreeNode;
  i: integer;
begin
  for i := 0 to IDEEditors.Count - 1 do
  begin
    Instance := IDEEditors[i].Create(Self);
    Instance.OnLoadIDEOptions := @LoadIDEOptions;
    Instance.OnSaveIDEOptions := @SaveIDEOptions;
    Instance.Setup;
    FEditors.Add(Instance);

    ANode := CategoryTree.Items.AddChild(nil, Instance.GetTitle);
    ANode.Data := Instance;
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

