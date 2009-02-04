{***************************************************************************
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
}
unit Compiler_Conditionals_Options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, FileProcs, Controls, LResources, Forms, ComCtrls,
  Menus, Dialogs,
  ProjectIntf, IDEImagesIntf,
  LazarusIDEStrConsts, CompOptsModes, CompOptCondPropsDlg;

type

  { TCompOptsConditionalsFrame }

  TCompOptsConditionalsFrame = class(TFrame)
    COCTreeView: TTreeView;
    COCPopupMenu: TPopupMenu;
    InsertAboveMenuItem: TMenuItem;
    InsertBelowMenuItem: TMenuItem;
    InsertChildMenuItem: TMenuItem;
    DeleteMenuItem: TMenuItem;
    PropertiesMenuItem: TMenuItem;
    MoveLvlDownMenuItem: TMenuItem;
    MoveLvlUpMenuItem: TMenuItem;
    MoveDownMenuItem: TMenuItem;
    MoveUpMenuItem: TMenuItem;
    procedure COCPopupMenuPopup(Sender: TObject);
    procedure COCTreeViewEditing(Sender: TObject; Node: TTreeNode; 
      var AllowEdit: Boolean);
    procedure DeleteMenuItemClick(Sender: TObject);
    procedure InsertAboveMenuItemClick(Sender: TObject);
    procedure InsertBelowMenuItemClick(Sender: TObject);
    procedure InsertChildMenuItemClick(Sender: TObject);
    procedure MoveDownMenuItemClick(Sender: TObject);
    procedure MoveLvlDownMenuItemClick(Sender: TObject);
    procedure MoveLvlUpMenuItemClick(Sender: TObject);
    procedure MoveUpMenuItemClick(Sender: TObject);
    procedure PropertiesMenuItemClick(Sender: TObject);
  private
    FAllowedValueTypes: TCOCValueTypes;
    FConditionals: TCompOptConditionals;
    FNodeTypeImageIDs: array[TCOCNodeType] of integer;
    procedure SetAllowedValueTypes(const AValue: TCOCValueTypes);
    procedure SetConditionals(const AValue: TCompOptConditionals);
    procedure FillTreeView;
    procedure ConsistencyCheck;
    function NodeToCaption(Node: TCompOptCondNode): string;
    function GetSelectedNode(out COCNode: TCompOptCondNode;
                        out TVNode: TTreeNode; RootAsDefault: boolean): boolean;
    procedure CreateNewNode(AttachMode: TNodeAttachMode);
  public
    constructor Create(TheOwner: TComponent); override;
    property Conditionals: TCompOptConditionals read FConditionals write SetConditionals;
    property AllowedValueTypes: TCOCValueTypes read FAllowedValueTypes write SetAllowedValueTypes;
  end;

implementation

{ TCompOptsConditionalsFrame }

procedure TCompOptsConditionalsFrame.COCPopupMenuPopup(Sender: TObject);
var
  COCNode: TCompOptCondNode;
  TVNode: TTreeNode;
  HasSelection: Boolean;
  NormalNodeIsSelectd: Boolean;
begin
  GetSelectedNode(COCNode,TVNode,false);
  HasSelection:=COCNode<>nil;
  NormalNodeIsSelectd:=HasSelection and (COCNode<>Conditionals.Root);
  InsertAboveMenuItem.Enabled:=NormalNodeIsSelectd;
  InsertBelowMenuItem.Enabled:=NormalNodeIsSelectd;
  InsertChildMenuItem.Enabled:=true;
  DeleteMenuItem.Enabled:=NormalNodeIsSelectd;
  PropertiesMenuItem.Enabled:=NormalNodeIsSelectd;
  MoveLvlDownMenuItem.Enabled:=NormalNodeIsSelectd and (TVNode.GetPrevSibling<>nil);
  MoveLvlUpMenuItem.Enabled:=NormalNodeIsSelectd and (COCNode.Parent.Parent<>nil);
  MoveDownMenuItem.Enabled:=NormalNodeIsSelectd and (TVNode.GetNextSibling<>nil);
  MoveUpMenuItem.Enabled:=NormalNodeIsSelectd and (TVNode.GetPrevSibling<>nil);
end;

procedure TCompOptsConditionalsFrame.COCTreeViewEditing(Sender: TObject; 
  Node: TTreeNode; var AllowEdit: Boolean);
begin

end;

procedure TCompOptsConditionalsFrame.DeleteMenuItemClick(Sender: TObject);
var
  COCNode: TCompOptCondNode;
  TVNode: TTreeNode;
begin
  if not GetSelectedNode(COCNode,TVNode,false) then exit;
  if Conditionals.Root=COCNode then exit;
  if MessageDlg('Delete?',
    'Delete the node "'+TVNode.Text+'"?',mtConfirmation,[mbYes,mbNo],0)<>mrYes
  then
    exit;
  TVNode.Delete;
  COCNode.Free;
  ConsistencyCheck;
end;

procedure TCompOptsConditionalsFrame.InsertAboveMenuItemClick(Sender: TObject);
begin
  CreateNewNode(naInsert);
end;

procedure TCompOptsConditionalsFrame.InsertBelowMenuItemClick(Sender: TObject);
begin
  CreateNewNode(naInsertBehind);
end;

procedure TCompOptsConditionalsFrame.InsertChildMenuItemClick(Sender: TObject);
begin
  CreateNewNode(naAddChildFirst);
end;

procedure TCompOptsConditionalsFrame.MoveDownMenuItemClick(Sender: TObject);
// move selected node below sibling
var
  COCNode: TCompOptCondNode;
  TVNode: TTreeNode;
begin
  if not GetSelectedNode(COCNode,TVNode,false) then exit;
  if TVNode.GetNextSibling=nil then exit;
  TVNode.MoveTo(TVNode.GetNextSibling,naInsertBehind);
  COCNode.Index:=COCNode.Index+1;
  ConsistencyCheck;
end;

procedure TCompOptsConditionalsFrame.MoveLvlDownMenuItemClick(Sender: TObject);
// make selected node a child of previous sibling
var
  COCNode: TCompOptCondNode;
  TVNode: TTreeNode;
  Sibling: TCompOptCondNode;
begin
  if not GetSelectedNode(COCNode,TVNode,false) then exit;
  if Conditionals.Root=COCNode then exit;
  if TVNode.GetPrevSibling=nil then exit;
  TVNode.MoveTo(TVNode.GetPrevSibling,naAddChild);
  Sibling:=COCNode.Parent.Childs[COCNode.Index-1];
  COCNode.Move(Sibling,Sibling.Count);
  ConsistencyCheck;
end;

procedure TCompOptsConditionalsFrame.MoveLvlUpMenuItemClick(Sender: TObject);
// make selected node a sibling of its parent (below parent)
var
  COCNode: TCompOptCondNode;
  TVNode: TTreeNode;
begin
  if not GetSelectedNode(COCNode,TVNode,false) then exit;
  if COCNode.Parent=nil then exit;
  if COCNode.Parent.Parent=nil then exit;
  TVNode.MoveTo(TVNode.Parent,naInsertBehind);
  COCNode.Move(COCNode.Parent.Parent,COCNode.Parent.Index+1);
  ConsistencyCheck;
end;

procedure TCompOptsConditionalsFrame.MoveUpMenuItemClick(Sender: TObject);
// move selected node above sibling
var
  COCNode: TCompOptCondNode;
  TVNode: TTreeNode;
begin
  if not GetSelectedNode(COCNode,TVNode,false) then exit;
  if TVNode.GetPrevSibling=nil then exit;
  TVNode.MoveTo(TVNode.GetPrevSibling,naInsert);
  COCNode.Index:=COCNode.Index-1;
  ConsistencyCheck;
end;

procedure TCompOptsConditionalsFrame.PropertiesMenuItemClick(Sender: TObject);
var
  COCNode: TCompOptCondNode;
  TVNode: TTreeNode;
begin
  if not GetSelectedNode(COCNode,TVNode,false) then exit;
  EditCompOptCondProperties(COCNode,AllowedValueTypes);
  TVNode.Text:=NodeToCaption(COCNode);
  TVNode.ImageIndex:=FNodeTypeImageIDs[COCNode.NodeType];
  TVNode.SelectedIndex:=TVNode.ImageIndex;
end;

procedure TCompOptsConditionalsFrame.SetConditionals(
  const AValue: TCompOptConditionals);
begin
  if FConditionals=AValue then exit;
  FConditionals:=AValue;
  FillTreeView;
end;

procedure TCompOptsConditionalsFrame.SetAllowedValueTypes(
  const AValue: TCOCValueTypes);
begin
  if FAllowedValueTypes=AValue then exit;
  FAllowedValueTypes:=AValue;
end;

procedure TCompOptsConditionalsFrame.FillTreeView;

  procedure Add(COCNode: TCompOptCondNode; ParentTVNode: TTreeNode);
  var
    TVNode: TTreeNode;
    i: Integer;
  begin
    if COCNode=nil then exit;
    TVNode:=COCTreeView.Items.AddChildObject(ParentTVNode,NodeToCaption(COCNode),COCNode);
    TVNode.ImageIndex:=FNodeTypeImageIDs[COCNode.NodeType];
    TVNode.SelectedIndex:=TVNode.ImageIndex;
    for i:=0 to COCNode.Count-1 do
      Add(COCNode.Childs[i],TVNode);
  end;

var
  i: Integer;
begin
  COCTreeView.BeginUpdate;
  COCTreeView.Items.Clear;
  if (Conditionals<>nil) and (Conditionals.Root<>nil)
  and (Conditionals.Root.Count>0) then begin
    for i:=0 to Conditionals.Root.Count-1 do
      Add(Conditionals.Root.Childs[i],nil);
  end;
  COCTreeView.EndUpdate;
  ConsistencyCheck;
end;

procedure TCompOptsConditionalsFrame.ConsistencyCheck;

  procedure CheckChilds(COCNode: TCompOptCondNode; ChildTVNode: TTreeNode); forward;

  procedure CheckNode(COCNode: TCompOptCondNode; TVNode: TTreeNode);
  begin
    if COCNode=nil then
      RaiseCatchableException('');
    if (TVNode=nil) then
      RaiseCatchableException('');
    if TVNode.Data=nil then
      RaiseCatchableException('');
    if COCNode<>TCompOptCondNode(TVNode.Data) then
      RaiseCatchableException(TCompOptCondNode(TObject(TVNode.Data)).Value+'<>'+COCNode.Value+' TVNode='+TVNode.Text);
    if NodeToCaption(COCNode)<>TVNode.Text then
      RaiseCatchableException(NodeToCaption(COCNode)+'<>'+TVNode.Text);
    if FNodeTypeImageIDs[COCNode.NodeType]<>TVNode.ImageIndex then
      RaiseCatchableException(dbgs(FNodeTypeImageIDs[COCNode.NodeType])+'<>'+dbgs(TVNode.ImageIndex));
    if FNodeTypeImageIDs[COCNode.NodeType]<>TVNode.SelectedIndex then
      RaiseCatchableException(dbgs(FNodeTypeImageIDs[COCNode.NodeType])+'<>'+dbgs(TVNode.SelectedIndex));
    CheckChilds(COCNode,TVNode.GetFirstChild);
  end;

  procedure CheckChilds(COCNode: TCompOptCondNode; ChildTVNode: TTreeNode);
  var
    i: Integer;
  begin
    if COCNode=nil then exit;
    for i:=0 to COCNode.Count-1 do begin
      CheckNode(COCNode.Childs[i],ChildTVNode);
      ChildTVNode:=ChildTVNode.GetNextSibling;
    end;
    if ChildTVNode<>nil then
      RaiseCatchableException('');
  end;

begin
  Conditionals.WriteDebugReport;
  COCTreeView.WriteDebugReport('  ',true);
  if (Conditionals=nil) or (Conditionals.Root=nil)
  or (Conditionals.Root.Count=0) then begin
    if COCTreeView.Items.Count>0 then
      RaiseCatchableException('');
  end else begin
    CheckChilds(Conditionals.Root,COCTreeView.Items.GetFirstNode);
  end;
end;

function TCompOptsConditionalsFrame.NodeToCaption(Node: TCompOptCondNode
  ): string;
begin
  case Node.NodeType of
  cocntNone: Result:='Noop';
  cocntIf: Result:='If '+Node.Value;
  cocntIfdef: Result:='IfDef '+Node.Value;
  cocntIfNdef: Result:='IfNDef '+Node.Value;
  cocntElseIf: Result:='ElseIf '+Node.Value;
  cocntElse: Result:='Else';
  cocntAddValue:
    begin
      case Node.ValueType of
      cocvtNone: Result:='Result:='+Node.Value;
      cocvtUnitPath: Result:='Add unit path: '+Node.Value;
      cocvtSrcPath: Result:='Add unit source path: '+Node.Value;
      cocvtIncludePath: Result:='Add include path: '+Node.Value;
      cocvtObjectPath: Result:='Add object path: '+Node.Value;
      cocvtLibraryPath: Result:='Add library path: '+Node.Value;
      cocvtDebugPath: Result:='Add debug path: '+Node.Value;
      cocvtLinkerOptions: Result:='Add linker options: '+Node.Value;
      cocvtCustomOptions: Result:='Add custom options: '+Node.Value;
      else
        Result:='(unknown ValueType)';
      end;
    end;
  else
    Result:='(unknown NodeType)';
  end;
  Result:=ValidUTF8String(Result);
end;

function TCompOptsConditionalsFrame.GetSelectedNode(out
  COCNode: TCompOptCondNode; out TVNode: TTreeNode;
  RootAsDefault: boolean): boolean;
begin
  COCNode:=nil;
  TVNode:=COCTreeView.Selected;
  if TVNode=nil then begin
    if RootAsDefault then begin
      TVNode:=nil;
      COCNode:=Conditionals.Root;
    end;
  end else begin
    COCNode:=TCompOptCondNode(TVNode.Data);
  end;
  Result:=COCNode<>nil;
end;

procedure TCompOptsConditionalsFrame.CreateNewNode(AttachMode: TNodeAttachMode
  );
var
  TVNode: TTreeNode;
  COCNode: TCompOptCondNode;
  s: String;
  NewTVNode: TTreeNode;
  NewCOCNode: TCompOptCondNode;
begin
  if not GetSelectedNode(COCNode,TVNode,true) then exit;
  NewCOCNode:=TCompOptCondNode.Create(COCNode.Owner);
  s:=NodeToCaption(NewCOCNode);
  NewTVNode:=COCTreeView.Items.AddObject(TVNode,s,NewCOCNode);
  if TVNode<>nil then
    NewTVNode.MoveTo(TVNode,AttachMode);
  NewTVNode.ImageIndex:=FNodeTypeImageIDs[NewCOCNode.NodeType];
  NewTVNode.SelectedIndex:=NewTVNode.ImageIndex;
  case AttachMode of
  naAdd: NewCOCNode.Move(COCNode.Parent,COCNode.Parent.Count);
  naAddFirst: NewCOCNode.Move(COCNode.Parent,0);
  naAddChild: NewCOCNode.Move(COCNode,COCNode.Count);
  naAddChildFirst: NewCOCNode.Move(COCNode,0);
  naInsert: NewCOCNode.Move(COCNode.Parent,COCNode.Index);
  naInsertBehind: NewCOCNode.Move(COCNode.Parent,COCNode.Index+1);
  end;
  if TVNode<>nil then
    TVNode.Expanded:=true;
  ConsistencyCheck;
end;

constructor TCompOptsConditionalsFrame.Create(TheOwner: TComponent);
var
  nt: TCOCNodeType;
begin
  inherited Create(TheOwner);
  FAllowedValueTypes:=[low(TCOCValueType)..high(TCOCValueType)];

  COCTreeView.Images := IDEImages.Images_24;

  for nt:=Low(TCOCNodeType) to High(TCOCNodeType) do
    FNodeTypeImageIDs[nt]:=-1;
  FNodeTypeImageIDs[cocntNone]:=-1;
  FNodeTypeImageIDs[cocntIf]:=IDEImages.LoadImage(24,'da_if');
  FNodeTypeImageIDs[cocntIfdef]:=IDEImages.LoadImage(24,'da_ifdef');
  FNodeTypeImageIDs[cocntIfNdef]:=IDEImages.LoadImage(24,'da_ifndef');
  FNodeTypeImageIDs[cocntElseIf]:=IDEImages.LoadImage(24,'da_elseif');
  FNodeTypeImageIDs[cocntElse]:=IDEImages.LoadImage(24,'da_else');
  FNodeTypeImageIDs[cocntAddValue]:=IDEImages.LoadImage(24,'da_define');

  InsertAboveMenuItem.Caption:=dlgCOCreateNodeAbove;
  InsertBelowMenuItem.Caption:=dlgCOCreateNodeBelow;
  InsertChildMenuItem.Caption:=dlgCOCreateChildNode;
  DeleteMenuItem.Caption:=lisCodeToolsDefsDeleteNode;
  PropertiesMenuItem.Caption:=lisCEProperties;
  MoveLvlDownMenuItem.Caption:=dlgCOMoveLevelDown;
  MoveLvlUpMenuItem.Caption:=dlgCOMoveLevelUp;
  MoveDownMenuItem.Caption:=dlgCOMoveDown;
  MoveUpMenuItem.Caption:=dlgCOMoveUp;
end;

initialization
  {$I compiler_conditionals_options.lrs}

end.

