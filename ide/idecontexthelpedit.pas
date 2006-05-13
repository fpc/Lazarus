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
  IDEWindowIntf, LazHelpIntf,
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
    procedure CreateHelpNodeForControlButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HelpNodesTreeViewSelectionChanged(Sender: TObject);
    procedure NodeHasHelpCheckBoxEditingDone(Sender: TObject);
    procedure NodeNameEditEditingDone(Sender: TObject);
    procedure NodePathEditEditingDone(Sender: TObject);
    procedure OkBitBtnClick(Sender: TObject);
    procedure TestButtonClick(Sender: TObject);
  private
    FIDEWindow: TCustomForm;
    FInvoker: TObject;
    FWorkingHelpNodes: TIWHelpTree;
    procedure SetIDEWindow(const AValue: TCustomForm);
    procedure SetInvoker(const AValue: TObject);
    procedure UpdateWindowControlsGroupBoxCaption;
    procedure FillControlsTreeView;
    procedure FillHelpNodesTreeView;
    procedure UpdateHelpNodePropertiesGroupBox;
    procedure SelectHelpNode(AControl: TControl);
    procedure SelectControlNode(AControl: TControl);
    function FindHelpTreeNode(HelpNode: TIWHelpNode): TTreeNode;
    function FindControlTreeNode(AControl: TControl): TTreeNode;
    function GetCurrentControl: TControl;
    function GetCurrentHelpNode: TIWHelpNode;
    procedure SaveHelpNodeProperties;
  public
    property Invoker: TObject read FInvoker write SetInvoker;
    property IDEWindow: TCustomForm read FIDEWindow write SetIDEWindow;
    property WorkingHelpNodes: TIWHelpTree read FWorkingHelpNodes;
  end;

var
  ContextHelpEditorDlg: TContextHelpEditorDlg = nil;

function ShowContextHelpEditor(Sender: TObject): TModalResult;
procedure ShowContextHelpForIDE(Sender: TObject);

implementation

function ShowContextHelpEditor(Sender: TObject): TModalResult;
begin
  // make sure there is only one editor at a time
  if ContextHelpEditorDlg<>nil then exit;
  
  ContextHelpEditorDlg:=TContextHelpEditorDlg.Create(nil);
  try
    ContextHelpEditorDlg.Invoker:=Sender;
    Result:=ContextHelpEditorDlg.ShowModal;
  finally
    ContextHelpEditorDlg.Free;
    ContextHelpEditorDlg:=nil;
  end;
end;

procedure ShowContextHelpForIDE(Sender: TObject);
begin
  if Sender is TControl then begin
    LoadIDEWindowHelp;
    IDEWindowHelpNodes.InvokeHelp(TControl(Sender));
  end else begin
    DebugLn('ShowContextHelpForIDE Sender=',dbgsName(Sender));
  end;
end;

{ TContextHelpEditorDlg }

procedure TContextHelpEditorDlg.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TContextHelpEditorDlg.CreateHelpNodeForControlButtonClick(
  Sender: TObject);
var
  AControl: TControl;
begin
  AControl:=GetCurrentControl;
  if AControl=nil then exit;
  WorkingHelpNodes.FindNodeForControl(AControl,true);
  FillHelpNodesTreeView;
  SelectHelpNode(AControl);
  SelectControlNode(AControl);
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
  FWorkingHelpNodes:=TIWHelpTree.Create;
  FWorkingHelpNodes.Assign(IDEWindowHelpNodes);
  FillHelpNodesTreeView;
end;

procedure TContextHelpEditorDlg.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FWorkingHelpNodes);
end;

procedure TContextHelpEditorDlg.HelpNodesTreeViewSelectionChanged(
  Sender: TObject);
begin
  UpdateHelpNodePropertiesGroupBox;
end;

procedure TContextHelpEditorDlg.NodeHasHelpCheckBoxEditingDone(Sender: TObject);
begin
  SaveHelpNodeProperties;
end;

procedure TContextHelpEditorDlg.NodeNameEditEditingDone(Sender: TObject);
begin
  SaveHelpNodeProperties;
end;

procedure TContextHelpEditorDlg.NodePathEditEditingDone(Sender: TObject);
begin
  SaveHelpNodeProperties;
end;

procedure TContextHelpEditorDlg.OkBitBtnClick(Sender: TObject);
begin
  WorkingHelpNodes.DeleteLeavesWithoutHelp;
  IDEWindowHelpNodes.Assign(WorkingHelpNodes);
  SaveIDEWindowHelp;
  ModalResult:=mrOk;
end;

procedure TContextHelpEditorDlg.TestButtonClick(Sender: TObject);
var
  AControl: TControl;
begin
  AControl:=GetCurrentControl;
  if AControl=nil then exit;
  WorkingHelpNodes.InvokeHelp(AControl);
end;

procedure TContextHelpEditorDlg.SetInvoker(const AValue: TObject);
var
  AControl: TControl;
begin
  if FInvoker=AValue then exit;
  FInvoker:=AValue;
  //DebugLn('TContextHelpEditorDlg.SetInvoker Invoker=',dbgsName(Invoker));
  if Invoker is TControl then begin
    AControl:=TControl(Invoker);
    IDEWindow:=GetParentForm(AControl);
    //DebugLn('TContextHelpEditorDlg.SetInvoker IDEWindow=',dbgsName(IDEWindow));
    WorkingHelpNodes.FindNodeForControl(AControl,true);
    FillHelpNodesTreeView;
    SelectHelpNode(AControl);
    SelectControlNode(AControl);
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
    NewNode:=ControlsTreeView.Items.AddChildObject(ParentNode,
                                                   dbgsName(AControl),AControl);
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
    NewNode:=HelpNodesTreeView.Items.AddChildObject(ParentNode,
                                                   HelpNode.Name,HelpNode);
    for i:=0 to HelpNode.Count-1 do
      Add(HelpNode[i],NewNode);
    NewNode.Expanded:=true;
  end;

begin
  HelpNodesTreeView.BeginUpdate;
  HelpNodesTreeView.Items.Clear;
  Add(WorkingHelpNodes.Root,nil);
  HelpNodesTreeView.EndUpdate;
end;

procedure TContextHelpEditorDlg.UpdateHelpNodePropertiesGroupBox;
var
  HelpNode: TIWHelpNode;
begin
  if (csDestroying in ComponentState) then exit;
  HelpNode:=GetCurrentHelpNode;
  if HelpNode<>nil then begin
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

procedure TContextHelpEditorDlg.SelectHelpNode(AControl: TControl);
var
  Node: TTreeNode;
begin
  Node:=FindHelpTreeNode(WorkingHelpNodes.FindNodeForControl(AControl));
  HelpNodesTreeView.Selected:=Node;
  //DebugLn('TContextHelpEditorDlg.SelectHelpNode Node=',dbgs(Node),' AControl=',dbgsName(AControl),' ',dbgs(HelpNodesTreeView.Selected));
end;

procedure TContextHelpEditorDlg.SelectControlNode(AControl: TControl);
var
  Node: TTreeNode;
begin
  Node:=FindControlTreeNode(AControl);
  ControlsTreeView.Selected:=Node;
end;

function TContextHelpEditorDlg.FindHelpTreeNode(HelpNode: TIWHelpNode
  ): TTreeNode;
  
  function Find(HNode: TIWHelpNode): TTreeNode;
  var
    ParentTreeNode: TTreeNode;
  begin
    if HNode=nil then exit(nil);
    if HNode.Parent=nil then begin
      Result:=HelpNodesTreeView.Items.FindTopLvlNode(HNode.Name);
    end else begin
      ParentTreeNode:=Find(HNode.Parent);
      if ParentTreeNode=nil then
        Result:=nil
      else
        Result:=ParentTreeNode.FindNode(HNode.Name);
    end;
  end;
  
begin
  Result:=Find(HelpNode);
end;

function TContextHelpEditorDlg.FindControlTreeNode(AControl: TControl
  ): TTreeNode;

  function Find(TheControl: TControl): TTreeNode;
  var
    ParentTreeNode: TTreeNode;
  begin
    if TheControl=nil then exit(nil);
    if TheControl.Parent=nil then begin
      Result:=ControlsTreeView.Items.FindTopLvlNode(dbgsName(TheControl));
    end else begin
      ParentTreeNode:=Find(TheControl.Parent);
      if ParentTreeNode=nil then
        Result:=nil
      else
        Result:=ParentTreeNode.FindNode(dbgsName(TheControl));
    end;
  end;

begin
  Result:=Find(AControl);
end;

function TContextHelpEditorDlg.GetCurrentControl: TControl;
var
  Node: TTreeNode;
begin
  Node:=ControlsTreeView.Selected;
  if Node=nil then exit(nil);
  Result:=TControl(Node.Data);
end;

function TContextHelpEditorDlg.GetCurrentHelpNode: TIWHelpNode;
var
  Node: TTreeNode;
begin
  Node:=HelpNodesTreeView.Selected;
  if Node=nil then exit(nil);
  Result:=TIWHelpNode(Node.Data);
end;

procedure TContextHelpEditorDlg.SaveHelpNodeProperties;
var
  HelpNode: TIWHelpNode;
begin
  HelpNode:=GetCurrentHelpNode;
  if HelpNode=nil then exit;
  HelpNode.Name:=NodeNameEdit.Text;
  HelpNode.Path:=NodePathEdit.Text;
  HelpNode.HasHelp:=NodeHasHelpCheckBox.Checked;
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

