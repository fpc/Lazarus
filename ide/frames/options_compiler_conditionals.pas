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
unit Options_Compiler_Conditionals;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileProcs, LResources, Forms, ComCtrls, Menus, Dialogs,
  ProjectIntf, IDEImagesIntf,
  CompOptsModes;

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
    FConditionals: TCompOptConditionals;
    FNodeTypeImageIDs: array[TCOCNodeType] of integer;
    procedure SetConditionals(const AValue: TCompOptConditionals);
    procedure FillTreeView;
    procedure ConsistencyCheck;
    function NodeToCaption(Node: TCompOptCondNode): string;
    function NodeToImageIndex(Node: TCompOptCondNode): integer;
    function GetSelectedNode(out COCNode: TCompOptCondNode;
                        out TVNode: TTreeNode; RootAsDefault: boolean): boolean;
    procedure CreateNewNode(AttachMode: TNodeAttachMode);
  public
    constructor Create(TheOwner: TComponent); override;
    property Conditionals: TCompOptConditionals read FConditionals write SetConditionals;
  end;

implementation

{ TCompOptsConditionalsFrame }

procedure TCompOptsConditionalsFrame.COCPopupMenuPopup(Sender: TObject);
var
  COCNode: TCompOptCondNode;
  TVNode: TTreeNode;
  HasSelection: Boolean;
begin
  GetSelectedNode(COCNode,TVNode);
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
begin

end;

procedure TCompOptsConditionalsFrame.SetConditionals(
  const AValue: TCompOptConditionals);
begin
  if FConditionals=AValue then exit;
  FConditionals:=AValue;
  FillTreeView;
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
    TVNode.StateIndex:=TVNode.ImageIndex;
    for i:=0 to COCNode.Count-1 do
      Add(COCNode.Childs[i],TVNode);
  end;

begin
  COCTreeView.BeginUpdate;
  COCTreeView.Items.Clear;
  if Conditionals<>nil then begin
    Add(Conditionals.Root,nil);
  end;
  COCTreeView.EndUpdate;
  ConsistencyCheck;
end;

procedure TCompOptsConditionalsFrame.ConsistencyCheck;

  procedure CheckNode(COCNode: TCompOptCondNode; TVNode: TTreeNode);
  var
    i: Integer;
    ChildTVNode: TTreeNode;
  begin
    if COCNode=nil then
      RaiseCatchableException('');
    if TVNode=nil then
      RaiseCatchableException('');
    if COCNode<>TCompOptCondNode(TVNode.Data) then
      RaiseCatchableException('');
    ChildTVNode:=TVNode.GetFirstChild;
    for i:=0 to COCNode.Count-1 do begin
      CheckNode(COCNode.Childs[i],ChildTVNode);
      ChildTVNode:=ChildTVNode.GetNextSibling;
    end;
  end;

begin
  if Conditionals=nil then begin
    if COCTreeView.Items.Count>0 then
      RaiseCatchableException('');
  end else begin
    CheckNode(Conditionals.Root,COCTreeView.Items.GetFirstNode);
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
      cocvtNone: Result:='Result:='+Node.Value,
      cocvtUnitPath: Result:='Add unit path: '+Node.Value,
      cocvtSrcPath: Result:='Add unit source path: '+Node.Value,
      cocvtIncludePath: Result:='Add include path: '+Node.Value,
      cocvtObjectPath: Result:='Add object path: '+Node.Value,
      cocvtLibraryPath: Result:='Add library path: '+Node.Value,
      cocvtDebugPath: Result:='Add debug path: '+Node.Value,
      cocvtLinkerOptions: Result:='Add linker options: '+Node.Value,
      cocvtCustomOptions: Result:='Add custom options: '+Node.Value,
      else
        Result:='(unknown ValueType)';
      end;
    end;
  else
    Result:='(unknown NodeType)';
  end;
  Result:=ValidUTF8String(Result);
end;

function TCompOptsConditionalsFrame.NodeToImageIndex(Node: TCompOptCondNode
  ): integer;
begin

end;

function TCompOptsConditionalsFrame.GetSelectedNode(out
  COCNode: TCompOptCondNode; out TVNode: TTreeNode;
  RootAsDefault: boolean): boolean;
begin
  COCNode:=nil;
  TVNode:=COCTreeView.Selected;
  if TVNode=nil then begin
    if RootAsDefault then begin
      TVNode:=COCTreeView.Items.GetFirstNode;
      COCNode:=TCompOptCondNode(TVNode.Data);
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
begin
  if not GetSelectedNode(COCNode,TVNode,true) then exit;
  NewCOCNode:=TCompOptCondNode.Create(COCNode.Owner);
  s:=NodeToCaption(COCNode);
  NewTVNode:=COCTreeView.Items.AddObject(TVNode,s,COCNode);
  NewTVNode.MoveTo(TVNode,naAddChildFirst);
  NewTVNode.ImageIndex:=FNodeTypeImageIDs[NewCOCNode.NodeType];
  NewTVNode.StateIndex:=NewTVNode.ImageIndex;
  ConsistencyCheck;
end;

constructor TCompOptsConditionalsFrame.Create(TheOwner: TComponent);
var
  nt: TCOCNodeType;
begin
  inherited Create(TheOwner);

  COCTreeView.Images := IDEImages.Images_24;
  COCTreeView.StateImages := IDEImages.Images_16;

  for nt:=Low(TCOCNodeType) to High(TCOCNodeType) do
    FNodeTypeImageIDs[nt]:=-1;
  FNodeTypeImageIDs[cocntNone]:=IDEImages.LoadImage(24,'da_none');
  FNodeTypeImageIDs[cocntIf]:=IDEImages.LoadImage(24,'da_if');
  FNodeTypeImageIDs[cocntIfdef]:=IDEImages.LoadImage(24,'da_ifdef');
  FNodeTypeImageIDs[cocntIfNdef]:=IDEImages.LoadImage(24,'da_ifndef');
  FNodeTypeImageIDs[cocntElseIf]:=IDEImages.LoadImage(24,'da_elseif');
  FNodeTypeImageIDs[cocntElse]:=IDEImages.LoadImage(24,'da_else');
  FNodeTypeImageIDs[cocntAddValue]:=IDEImages.LoadImage(24,'da_define');
end;

initialization
  {$I options_compiler_conditionals.lrs}

end.

