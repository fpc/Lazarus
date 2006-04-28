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
    Editor dialog to edit nodes of Help for IDE windows (controls).
}
unit IDEContextHelpEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Graphics, Dialogs,
  Buttons, ButtonPanel, StdCtrls, ComCtrls,
  // IDEIntf
  IDEWindowIntf, HelpIntf,
  // IDE
  IDEWindowHelp, LazarusIDEStrConsts, ExtCtrls;

type

  { TContextHelpEditorDlg }

  TContextHelpEditorDlg = class(TForm)
    CancelBitBtn: TBitBtn;
    OkBitBtn: TBitBtn;
    Panel1: TPanel;
    TestButton: TButton;
    CreateHelpNodeForControlButton: TButton;
    NodeNameEdit: TEdit;
    NodePathEdit: TEdit;
    NodeHasHelpCheckBox: TCheckBox;
    HelpNodePropertiesGroupBox: TGroupBox;
    NodePathLabel: TLabel;
    NodeNameLabel: TLabel;
    NodesGroupBox: TGroupBox;
    ControlsTreeView: TTreeView;
    HelpNodesTreeView: TTreeView;
    Splitter1: TSplitter;
    WindowControlsGroupBox: TGroupBox;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure HelpNodesTreeViewSelectionChanged(Sender: TObject);
  private
    FIDEWindow: TCustomForm;
    FInvoker: TObject;
    procedure SetIDEWindow(const AValue: TCustomForm);
    procedure SetInvoker(const AValue: TObject);
    procedure UpdateWindowControlsGroupBoxCaption;
    procedure FillControlsTreeView;
    procedure FillHelpNodesTreeView;
    procedure UpdateHelpNodePropertiesGroupBox;
  public
    property Invoker: TObject read FInvoker write SetInvoker;
    property IDEWindow: TCustomForm read FIDEWindow write SetIDEWindow;
  end;


function ShowContextHelpEditor(Sender: TObject): TModalresult;

implementation

function ShowContextHelpEditor(Sender: TObject): TModalresult;
var
  ContextHelpEditorDlg: TContextHelpEditorDlg;
begin
  ContextHelpEditorDlg:=TContextHelpEditorDlg.Create(nil);
  try
    ContextHelpEditorDlg.Invoker:=Sender;
    Result:=ContextHelpEditorDlg.ShowModal;
  finally
    ContextHelpEditorDlg.Free;
  end;
end;

{ TContextHelpEditorDlg }

procedure TContextHelpEditorDlg.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TContextHelpEditorDlg.FormCreate(Sender: TObject);
begin
  Caption:='Edit context help';
  
  TestButton.Caption:='Test';
  CreateHelpNodeForControlButton.Caption:='Create Help node';
  NodeHasHelpCheckBox.Caption:='Has Help';
  NodePathLabel.Caption:='Path';
  NodeNameLabel.Caption:='Name';
  NodesGroupBox.Caption:='Help entries';
  OkBitBtn.Caption:='Ok';
  CancelBitBtn.Caption:='Cancel';

  IDEDialogLayoutList.ApplyLayout(Self, 600, 450);
  
  LoadIDEWindowHelp;
  FillHelpNodesTreeView;
end;

procedure TContextHelpEditorDlg.HelpNodesTreeViewSelectionChanged(
  Sender: TObject);
begin
  UpdateHelpNodePropertiesGroupBox;
end;

procedure TContextHelpEditorDlg.SetInvoker(const AValue: TObject);
begin
  if FInvoker=AValue then exit;
  FInvoker:=AValue;
  if Invoker is TControl then begin
    IDEWindow:=GetParentForm(TControl(Invoker));
  end;
end;

procedure TContextHelpEditorDlg.UpdateWindowControlsGroupBoxCaption;
begin
  if IDEWindow<>nil then begin
    WindowControlsGroupBox.Caption:=dbgsName(IDEWindow);
    WindowControlsGroupBox.Enabled:=true;
  end else begin
    WindowControlsGroupBox.Caption:='No IDE window selected';
    WindowControlsGroupBox.Enabled:=false;
  end;
end;

procedure TContextHelpEditorDlg.FillControlsTreeView;

  procedure Add(AControl: TControl; ParentNode: TTreeNode);
  var
    i: Integer;
    NewNode: TTreeNode;
  begin
    NewNode:=ControlsTreeView.Items.AddChild(ParentNode,dbgsName(AControl));
    if AControl is TWinControl then begin
      for i:=0 to TWinControl(AControl).ControlCount-1 do
        Add(TWinControl(AControl).Controls[i],NewNode);
    end;
    NewNode.Expanded:=true;
  end;

begin
  ControlsTreeView.BeginUpdate;
  ControlsTreeView.Items.Clear;
  if IDEWindow<>nil then
    Add(IDEWindow,nil);
  ControlsTreeView.EndUpdate;
end;

procedure TContextHelpEditorDlg.FillHelpNodesTreeView;

  procedure Add(HelpNode: TIWHelpNode; ParentNode: TTreeNode);
  var
    i: Integer;
    NewNode: TTreeNode;
  begin
    NewNode:=ControlsTreeView.Items.AddChildObject(ParentNode,
                                                   HelpNode.Name,HelpNode);
    for i:=0 to HelpNode.Count-1 do
      Add(HelpNode[i],NewNode);
    NewNode.Expanded:=true;
  end;

begin
  HelpNodesTreeView.BeginUpdate;
  HelpNodesTreeView.Items.Clear;
  Add(IDEWindowHelpNodes.Root,nil);
  HelpNodesTreeView.EndUpdate;
end;

procedure TContextHelpEditorDlg.UpdateHelpNodePropertiesGroupBox;
var
  TVNode: TTreeNode;
  HelpNode: TIWHelpNode;
begin
  TVNode:=HelpNodesTreeView.Selected;
  if TVNode<>nil then begin
    HelpNode:=TIWHelpNode(TVNode.Data);
    HelpNodePropertiesGroupBox.Caption:=HelpNode.Name;
    NodeNameEdit.Text:=HelpNode.Name;
    NodePathEdit.Text:=HelpNode.Path;
    NodeHasHelpCheckBox.Checked:=HelpNode.HasHelp;
    HelpNodePropertiesGroupBox.Enabled:=true;
  end else begin
    HelpNodePropertiesGroupBox.Caption:='no node selected';
    HelpNodePropertiesGroupBox.Enabled:=false;
  end;
end;

procedure TContextHelpEditorDlg.SetIDEWindow(const AValue: TCustomForm);
begin
  if FIDEWindow=AValue then exit;
  FIDEWindow:=AValue;
  UpdateWindowControlsGroupBoxCaption;
  FillControlsTreeView;
end;

initialization
  {$I idecontexthelpedit.lrs}

end.

