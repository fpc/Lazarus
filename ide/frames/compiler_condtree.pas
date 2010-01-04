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

  { TCompOptsExprEditor }

  TCompOptsExprEditor = class(TComponent)
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
    FDefaultNodeType: TCOCNodeType;
    FDefaultValue: string;
    FDefaultValueType: TCOCValueType;
    FRootNode: TTreeNode;
    FTreeView: TTreeView;
    FNodeTypeImageIDs: array[TCOCNodeType] of integer;
    procedure SetAllowedValueTypes(const AValue: TCOCValueTypes);
    procedure SetConditionals(const AValue: TCompOptConditionals);
    procedure SetRootNode(const AValue: TTreeNode);
    procedure SetTreeView(const AValue: TTreeView);
    procedure ClearTreeView;
    function NodeToCaption(Node: TCompOptCondNode): string;
    procedure FillTreeView;
    procedure ConsistencyCheck;
    function GetSelectedNode(out COCNode: TCompOptCondNode;
                        out TVNode: TTreeNode; RootAsDefault: boolean): boolean;
    procedure CreateNewNode(AttachMode: TNodeAttachMode);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure FillPopupMenu(aPopupMenu: TPopupMenu);
    procedure TreeViewEditing(Sender: TObject; Node: TTreeNode;
                              var AllowEdit: Boolean);
    procedure TreeViewEdited(Sender: TObject; Node: TTreeNode; var S: string);
    procedure Setup(NewTreeView: TTreeView; NewRootNode: TTreeNode;
      NewConditionals: TCompOptConditionals; NewAllowedValueTypes: TCOCValueTypes);
    property TreeView: TTreeView read FTreeView write SetTreeView;
    property RootNode: TTreeNode read FRootNode write SetRootNode;
    property Conditionals: TCompOptConditionals read FConditionals write SetConditionals;
    property AllowedValueTypes: TCOCValueTypes read FAllowedValueTypes write SetAllowedValueTypes;
    property DefaultNodeType: TCOCNodeType read FDefaultNodeType write FDefaultNodeType;
    property DefaultValueType: TCOCValueType read FDefaultValueType write FDefaultValueType;
    property DefaultValue: string read FDefaultValue write FDefaultValue;
  end;

  { TCompOptsConditionalsFrame }

  TCompOptsConditionalsFrame = class(TFrame)
    COCTreeView: TTreeView;
    COCPopupMenu: TPopupMenu;
    procedure COCPopupMenuPopup(Sender: TObject);
    procedure COCTreeViewEdited(Sender: TObject; Node: TTreeNode; var S: string
      );
    procedure COCTreeViewEditing(Sender: TObject; Node: TTreeNode; 
      var AllowEdit: Boolean);
  private
    FEditor: TCompOptsExprEditor;
    function GetAllowedValueTypes: TCOCValueTypes;
    function GetConditionals: TCompOptConditionals;
    procedure SetAllowedValueTypes(const AValue: TCOCValueTypes);
    procedure SetConditionals(const AValue: TCompOptConditionals);
  public
    constructor Create(TheOwner: TComponent); override;
    property Conditionals: TCompOptConditionals read GetConditionals write SetConditionals;
    property AllowedValueTypes: TCOCValueTypes read GetAllowedValueTypes write SetAllowedValueTypes;
    property Editor: TCompOptsExprEditor read FEditor;
  end;

implementation

{ TCompOptsConditionalsFrame }

procedure TCompOptsConditionalsFrame.COCPopupMenuPopup(Sender: TObject);
begin
  COCPopupMenu.Items.Clear;
  Editor.FillPopupMenu(COCPopupMenu);
end;

procedure TCompOptsConditionalsFrame.COCTreeViewEdited(Sender: TObject;
  Node: TTreeNode; var S: string);
begin
  Editor.TreeViewEdited(Sender,Node,S);
end;

procedure TCompOptsConditionalsFrame.COCTreeViewEditing(Sender: TObject; 
  Node: TTreeNode; var AllowEdit: Boolean);
begin
  Editor.TreeViewEditing(Sender,Node,AllowEdit);
end;

procedure TCompOptsConditionalsFrame.SetConditionals(
  const AValue: TCompOptConditionals);
begin
  Editor.Conditionals:=AValue;
end;

procedure TCompOptsConditionalsFrame.SetAllowedValueTypes(
  const AValue: TCOCValueTypes);
begin
  Editor.AllowedValueTypes:=AValue;
end;

function TCompOptsConditionalsFrame.GetConditionals: TCompOptConditionals;
begin
  Result:=Editor.Conditionals;
end;

function TCompOptsConditionalsFrame.GetAllowedValueTypes: TCOCValueTypes;
begin
  Result:=Editor.AllowedValueTypes;
end;

constructor TCompOptsConditionalsFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FEditor:=TCompOptsExprEditor.Create(Self);
  FEditor.TreeView:=COCTreeView;
end;

{ TCompOptsExprEditor }

procedure TCompOptsExprEditor.DeleteMenuItemClick(Sender: TObject);
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

procedure TCompOptsExprEditor.InsertAboveMenuItemClick(Sender: TObject);
begin
  CreateNewNode(naInsert);
end;

procedure TCompOptsExprEditor.InsertBelowMenuItemClick(Sender: TObject);
begin
  CreateNewNode(naInsertBehind);
end;

procedure TCompOptsExprEditor.InsertChildMenuItemClick(Sender: TObject);
begin
  CreateNewNode(naAddChildFirst);
end;

procedure TCompOptsExprEditor.MoveDownMenuItemClick(Sender: TObject);
// move selected node below sibling
var
  COCNode: TCompOptCondNode;
  TVNode: TTreeNode;
begin
  if not GetSelectedNode(COCNode,TVNode,false) then exit;
  if Conditionals.Root=COCNode then exit;
  if TVNode.GetNextSibling=nil then exit;
  TVNode.MoveTo(TVNode.GetNextSibling,naInsertBehind);
  COCNode.Index:=COCNode.Index+1;
  ConsistencyCheck;
end;

procedure TCompOptsExprEditor.MoveLvlDownMenuItemClick(Sender: TObject);
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

procedure TCompOptsExprEditor.MoveLvlUpMenuItemClick(Sender: TObject);
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

procedure TCompOptsExprEditor.MoveUpMenuItemClick(Sender: TObject);
// move selected node above sibling
var
  COCNode: TCompOptCondNode;
  TVNode: TTreeNode;
begin
  if not GetSelectedNode(COCNode,TVNode,false) then exit;
  if Conditionals.Root=COCNode then exit;
  if TVNode.GetPrevSibling=nil then exit;
  TVNode.MoveTo(TVNode.GetPrevSibling,naInsert);
  COCNode.Index:=COCNode.Index-1;
  ConsistencyCheck;
end;

procedure TCompOptsExprEditor.PropertiesMenuItemClick(Sender: TObject);
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

procedure TCompOptsExprEditor.SetAllowedValueTypes(const AValue: TCOCValueTypes
  );
var
  v: TCOCValueType;
begin
  if FAllowedValueTypes=AValue then exit;
  FAllowedValueTypes:=AValue;
  if not (FDefaultValueType in FAllowedValueTypes) then begin
    for v:=Low(TCOCValueTypes) to High(TCOCValueTypes) do begin
      if v in FAllowedValueTypes then begin
        DefaultValueType:=v;
        break;
      end;
    end;
  end;
end;

procedure TCompOptsExprEditor.SetConditionals(const AValue: TCompOptConditionals
  );
begin
  if FConditionals=AValue then exit;
  FConditionals:=AValue;
  FillTreeView;
end;

procedure TCompOptsExprEditor.SetRootNode(const AValue: TTreeNode);
begin
  if FRootNode=AValue then exit;
  if TreeView<>nil then begin
    TreeView.BeginUpdate;
    ClearTreeView;
  end;
  FRootNode:=AValue;
  if TreeView<>nil then begin
    FillTreeView;
    TreeView.EndUpdate;
  end;
end;

procedure TCompOptsExprEditor.SetTreeView(const AValue: TTreeView);
begin
  if FTreeView=AValue then exit;
  ClearTreeView;
  FTreeView:=AValue;
  if (FTreeView<>nil) and (FTreeView.Images=nil) then
    FTreeView.Images := IDEImages.Images_24;
  FillTreeView;
end;

procedure TCompOptsExprEditor.ClearTreeView;
begin
  if TreeView=nil then exit;
  TreeView.BeginUpdate;
  try
    if RootNode<>nil then begin
      while RootNode.GetLastChild<>nil do
        RootNode.GetLastChild.Delete;
    end else begin
      TreeView.Items.Clear;
    end;
  finally
    TreeView.EndUpdate;
  end;
end;

function TCompOptsExprEditor.NodeToCaption(Node: TCompOptCondNode): string;
begin
  case Node.NodeType of
  cocntNone: Result:='Comment: '+Node.Value;
  cocntIf: Result:='If '+Node.Value;
  cocntIfdef: Result:='IfDef '+Node.Value;
  cocntIfNdef: Result:='IfNDef '+Node.Value;
  cocntElseIf: Result:='ElseIf '+Node.Value;
  cocntElse: Result:='Else';
  cocntAddValue:
    begin
      case Node.ValueType of
      cocvtNone: Result:='None';
      cocvtResult: Result:='Result:=Result+'+Node.Value;
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
  cocntSetValue:
    begin
      case Node.ValueType of
      cocvtNone: Result:='None';
      cocvtResult: Result:='Result:='+Node.Value;
      cocvtUnitPath: Result:='Set unit path: '+Node.Value;
      cocvtSrcPath: Result:='Set unit source path: '+Node.Value;
      cocvtIncludePath: Result:='Set include path: '+Node.Value;
      cocvtObjectPath: Result:='Set object path: '+Node.Value;
      cocvtLibraryPath: Result:='Set library path: '+Node.Value;
      cocvtDebugPath: Result:='Set debug path: '+Node.Value;
      cocvtLinkerOptions: Result:='Set linker options: '+Node.Value;
      cocvtCustomOptions: Result:='Set custom options: '+Node.Value;
      else
        Result:='(unknown ValueType)';
      end;
    end;
  else
    Result:='(unknown NodeType)';
  end;
  Result:=ValidUTF8String(Result);
end;

procedure TCompOptsExprEditor.FillTreeView;

  procedure Add(COCNode: TCompOptCondNode; ParentTVNode: TTreeNode);
  var
    TVNode: TTreeNode;
    i: Integer;
  begin
    if COCNode=nil then exit;
    TVNode:=TreeView.Items.AddChildObject(ParentTVNode,NodeToCaption(COCNode),COCNode);
    TVNode.ImageIndex:=FNodeTypeImageIDs[COCNode.NodeType];
    TVNode.SelectedIndex:=TVNode.ImageIndex;
    for i:=0 to COCNode.Count-1 do
      Add(COCNode.Childs[i],TVNode);
  end;

var
  i: Integer;
begin
  if TreeView=nil then exit;
  TreeView.BeginUpdate;
  ClearTreeView;
  if (Conditionals<>nil) and (Conditionals.Root<>nil)
  and (Conditionals.Root.Count>0) then begin
    for i:=0 to Conditionals.Root.Count-1 do
      Add(Conditionals.Root.Childs[i],RootNode);
  end;
  TreeView.EndUpdate;
  ConsistencyCheck;
end;

procedure TCompOptsExprEditor.ConsistencyCheck;

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

var
  TVNode: TTreeNode;
begin
  if Conditionals=nil then exit;
  Conditionals.WriteDebugReport;
  if TreeView<>nil then begin
    TreeView.WriteDebugReport('  ',true);
    if (Conditionals=nil) or (Conditionals.Root=nil)
    or (Conditionals.Root.Count=0) then begin
      if (RootNode=nil) and (TreeView.Items.Count>0) then
        RaiseCatchableException('');
    end else begin
      if RootNode=nil then
        TVNode:=TreeView.Items.GetFirstNode
      else
        TVNode:=RootNode.GetFirstChild;
      CheckChilds(Conditionals.Root,TVNode);
    end;
  end;
end;

constructor TCompOptsExprEditor.Create(TheOwner: TComponent);
var
  nt: TCOCNodeType;
begin
  inherited Create(TheOwner);
  FAllowedValueTypes:=[low(TCOCValueType)..high(TCOCValueType)];

  for nt:=Low(TCOCNodeType) to High(TCOCNodeType) do
    FNodeTypeImageIDs[nt]:=-1;
  FNodeTypeImageIDs[cocntNone]:=-1;
  FNodeTypeImageIDs[cocntIf]:=IDEImages.LoadImage(24,'da_if');
  FNodeTypeImageIDs[cocntIfdef]:=IDEImages.LoadImage(24,'da_ifdef');
  FNodeTypeImageIDs[cocntIfNdef]:=IDEImages.LoadImage(24,'da_ifndef');
  FNodeTypeImageIDs[cocntElseIf]:=IDEImages.LoadImage(24,'da_elseif');
  FNodeTypeImageIDs[cocntElse]:=IDEImages.LoadImage(24,'da_else');
  FNodeTypeImageIDs[cocntAddValue]:=IDEImages.LoadImage(24,'da_define');
end;

destructor TCompOptsExprEditor.Destroy;
begin
  inherited Destroy;
end;

procedure TCompOptsExprEditor.FillPopupMenu(aPopupMenu: TPopupMenu);

  function Add(aCaption: string; OnClickEvent: TNotifyEvent): TMenuItem;
  begin
    Result:=TMenuItem.Create(Self);
    Result.Caption:=aCaption;
    Result.OnClick:=OnClickEvent;
    aPopupMenu.Items.Add(Result);
  end;

  function AddSeparator: TMenuItem;
  begin
    if aPopupMenu.Items.Count=0 then exit;
    Result:=TMenuItem.Create(Self);
    Result.Caption:='-';
    aPopupMenu.Items.Add(Result);
  end;

var
  COCNode: TCompOptCondNode;
  TVNode: TTreeNode;
  HasSelection: Boolean;
  NormalNodeIsSelected: Boolean;
  InConditionalNodes: Boolean;
begin
  GetSelectedNode(COCNode,TVNode,false);
  HasSelection:=COCNode<>nil;
  NormalNodeIsSelected:=HasSelection and (COCNode<>Conditionals.Root);
  InConditionalNodes:=(RootNode=nil)
                          or ((TVNode<>nil) and (TVNode.HasAsParent(RootNode)))
                          or (TVNode=RootNode);
  if NormalNodeIsSelected or InConditionalNodes then
    AddSeparator;
  if NormalNodeIsSelected then
    Add('Insert new node above',@InsertAboveMenuItemClick);
  if NormalNodeIsSelected then
    Add('Insert new node below',@InsertBelowMenuItemClick);
  if InConditionalNodes then
    Add('Insert new child node',@InsertChildMenuItemClick);
  if NormalNodeIsSelected then
    Add('Delete node ...',@DeleteMenuItemClick);
  if NormalNodeIsSelected then
    Add('Properties ...',@PropertiesMenuItemClick);
  if NormalNodeIsSelected and (TVNode.GetPrevSibling<>nil) then
    Add('Move node one level down',@MoveLvlDownMenuItemClick);
  if NormalNodeIsSelected and (COCNode.Parent.Parent<>nil) then
    Add('Move node one level up',@MoveLvlUpMenuItemClick);
  if NormalNodeIsSelected and (TVNode.GetNextSibling<>nil) then
    Add('Move node down',@MoveDownMenuItemClick);
  if NormalNodeIsSelected and (TVNode.GetPrevSibling<>nil) then
    Add('Move node up',@MoveUpMenuItemClick);
end;

procedure TCompOptsExprEditor.TreeViewEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  AllowEdit:=false;
end;

procedure TCompOptsExprEditor.TreeViewEdited(Sender: TObject; Node: TTreeNode;
  var S: string);
begin
  S:=Node.Text;
end;

procedure TCompOptsExprEditor.Setup(NewTreeView: TTreeView;
  NewRootNode: TTreeNode; NewConditionals: TCompOptConditionals;
  NewAllowedValueTypes: TCOCValueTypes);
begin
  TreeView:=nil;
  RootNode:=NewRootNode;
  Conditionals:=NewConditionals;
  AllowedValueTypes:=NewAllowedValueTypes;
  // activate
  TreeView:=NewTreeView;
end;

function TCompOptsExprEditor.GetSelectedNode(out COCNode: TCompOptCondNode; out
  TVNode: TTreeNode; RootAsDefault: boolean): boolean;
begin
  COCNode:=nil;
  TVNode:=TreeView.Selected;
  if (TVNode=nil) then begin
    if RootAsDefault then begin
      TVNode:=RootNode;
      COCNode:=Conditionals.Root;
    end;
  end else begin
    if (RootNode=nil) or TVNode.HasAsParent(RootNode) then
      COCNode:=TCompOptCondNode(TVNode.Data)
    else if (RootNode=TVNode) then begin
      TVNode:=RootNode;
      COCNode:=Conditionals.Root;
    end;
  end;
  Result:=COCNode<>nil;
end;

procedure TCompOptsExprEditor.CreateNewNode(AttachMode: TNodeAttachMode);
var
  TVNode: TTreeNode;
  COCNode: TCompOptCondNode;
  s: String;
  NewTVNode: TTreeNode;
  NewCOCNode: TCompOptCondNode;
begin
  if not GetSelectedNode(COCNode,TVNode,true) then exit;
  NewCOCNode:=TCompOptCondNode.Create(COCNode.Owner);
  NewCOCNode.NodeType:=DefaultNodeType;
  NewCOCNode.ValueType:=DefaultValueType;
  NewCOCNode.Value:=DefaultValue;
  s:=NodeToCaption(NewCOCNode);
  NewTVNode:=TreeView.Items.AddObject(TVNode,s,NewCOCNode);
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

initialization
  {$I compiler_conditionals_options.lrs}

end.

