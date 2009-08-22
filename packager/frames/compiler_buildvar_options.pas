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
unit Compiler_BuildVar_Options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, FileUtil, Controls, LResources, Forms, StdCtrls,
  Grids, Buttons, ExtCtrls, Dialogs, ComCtrls, Menus, AvgLvlTree,
  IDEImagesIntf, ProjectIntf, CompilerOptions,
  Compiler_Conditionals_Options, LazarusIDEStrConsts, CompOptsModes;

type
  TCBMNodeType = (
    cbmntNone,
    cbmntBuildMode,
    cbmntValues,
    cbmntValue,
    cbmntDefaultValue,
    cbmntDefaultValueEditor
    );

  { TCompOptBuildVarsFrame }

  TCompOptBuildVarsFrame = class(TFrame)
    BuildVarsGroupBox: TGroupBox;
    BuildVarsTreeView: TTreeView;
    BuildVarsTVPopupMenu: TPopupMenu;
    procedure BuildVarsTreeViewEdited(Sender: TObject; Node: TTreeNode;
      var S: string);
    procedure BuildVarsTreeViewEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure BuildVarsTreeViewStartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure BuildVarsTVPopupMenuPopup(Sender: TObject);
    procedure DeleteBuildModeClick(Sender: TObject);
    procedure NewBuildModeClick(Sender: TObject);
    procedure NewValueClick(Sender: TObject);
    procedure DeleteValueClick(Sender: TObject);
  private
    FBuildProperties: TIDEBuildProperties;
    fModeImgID: LongInt;
    fValuesImgID: LongInt;
    fValueImgID: LongInt;
    fDefValueImgID: LongInt;
    FEditors: TFPList;// list of TCompOptsExprEditor
    procedure SetBuildProperties(const AValue: TIDEBuildProperties);
    procedure RebuildTreeView;
    procedure TreeViewAddBuildMode(BuildProperty: TLazBuildProperty);
    procedure TreeViewAddValue(ValuesTVNode: TTreeNode; aValue: string);
    function GetNodeInfo(Node: TTreeNode; out BuildProperty: TLazBuildProperty): TCBMNodeType;
    function GetSelectedNode(out BuildProperty: TLazBuildProperty;
                             out NodeType: TCBMNodeType): TTreeNode;
    function GetBuildModeTVNode(BuildProperty: TLazBuildProperty): TTreeNode;
    function GetValuesTVNode(BuildProperty: TLazBuildProperty): TTreeNode;
    procedure FreeEditors;
    function GetEditor(BuildProperty: TLazBuildProperty): TCompOptsExprEditor;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property BuildProperties: TIDEBuildProperties read FBuildProperties write SetBuildProperties;
  end;

implementation

{ TCompOptBuildVarsFrame }

procedure TCompOptBuildVarsFrame.NewBuildModeClick(Sender: TObject);
var
  NewIdentifier: String;
  NewBuildProperty: TLazBuildProperty;
  SetResultNode: TCompOptCondNode;
begin
  NewIdentifier:=GlobalBuildProperties.GetUniqueModeName(BuildProperties);
  NewBuildProperty:=BuildProperties.Add(NewIdentifier);
  // add a node
  SetResultNode:=TCompOptCondNode.Create(NewBuildProperty.DefaultValue);
  SetResultNode.NodeType:=cocntSetValue;
  SetResultNode.ValueType:=cocvtResult;
  NewBuildProperty.DefaultValue.Root.AddLast(SetResultNode);
  // add to TreeView
  BuildVarsTreeView.BeginUpdate;
  TreeViewAddBuildMode(NewBuildProperty);
  BuildVarsTreeView.EndUpdate;
end;

procedure TCompOptBuildVarsFrame.NewValueClick(Sender: TObject);
var
  BuildProperty: TLazBuildProperty;
  NodeType: TCBMNodeType;
  i: Integer;
  NewValueStr: String;
  ValuesTVNode: TTreeNode;
begin
  GetSelectedNode(BuildProperty,NodeType);
  if BuildProperty=nil then exit;
  i:=1;
  repeat
    NewValueStr:=Format(lisValue2, [IntToStr(i)]);
    if BuildProperty.Values.IndexOf(NewValueStr)<0 then break;
    inc(i);
  until false;
  BuildProperty.Values.Add(NewValueStr);
  BuildVarsTreeView.BeginUpdate;
  ValuesTVNode:=GetValuesTVNode(BuildProperty);
  TreeViewAddValue(ValuesTVNode,NewValueStr);
  ValuesTVNode.Expand(true);
  BuildVarsTreeView.EndUpdate;
end;

procedure TCompOptBuildVarsFrame.DeleteValueClick(Sender: TObject);
var
  BuildProperty: TLazBuildProperty;
  NodeType: TCBMNodeType;
  SelTVNode: TTreeNode;
  aValue: String;
  i: LongInt;
begin
  SelTVNode:=GetSelectedNode(BuildProperty,NodeType);
  if NodeType<>cbmntValue then exit;
  aValue:=SelTVNode.Text;
  if MessageDlg(lisConfirmDelete,
    Format(lisDeleteValue, ['"', aValue, '"']),
    mtConfirmation,[mbYes,mbCancel],0)<>mrYes
  then exit;
  i:=BuildProperty.Values.IndexOf(aValue);
  if i>=0 then BuildProperty.Values.Delete(i);
  BuildVarsTreeView.BeginUpdate;
  SelTVNode.Delete;
  BuildVarsTreeView.EndUpdate;
end;

procedure TCompOptBuildVarsFrame.DeleteBuildModeClick(Sender: TObject);
var
  BuildProperty: TIDEBuildProperty;
  SelTVNode: TTreeNode;
  NodeType: TCBMNodeType;
  i: LongInt;
  Editor: TCompOptsExprEditor;
begin
  SelTVNode:=GetSelectedNode(TLazBuildProperty(BuildProperty),NodeType);
  if BuildProperty=nil then exit;
  if MessageDlg(lisConfirmDelete,
    Format(lisDeleteBuildMode, ['"', BuildProperty.Identifier, '"']),
    mtConfirmation,[mbYes,mbCancel],0)<>mrYes
  then exit;
  i:=BuildProperties.IndexOfIdentifier(BuildProperty.Identifier);
  Editor:=GetEditor(BuildProperty);
  FEditors.Remove(Editor);
  Editor.Free;
  BuildProperties.Delete(i);
  BuildVarsTreeView.BeginUpdate;
  SelTVNode.Delete;
  BuildVarsTreeView.EndUpdate;
end;

procedure TCompOptBuildVarsFrame.BuildVarsTVPopupMenuPopup(Sender: TObject);
var
  BuildProperty: TLazBuildProperty;
  NodeType: TCBMNodeType;
  Editor: TCompOptsExprEditor;

  function Add(const aCaption: string; const OnClickEvent: TNotifyEvent): TMenuItem;
  begin
    Result:=TMenuItem.Create(Self);
    Result.Caption:=aCaption;
    Result.OnClick:=OnClickEvent;
    BuildVarsTVPopupMenu.Items.Add(Result);
  end;

  function AddSeparator: TMenuItem;
  begin
    Result:=nil;
    if BuildVarsTVPopupMenu.Items.Count=0 then exit;
    Result:=TMenuItem.Create(Self);
    Result.Caption:='-';
    BuildVarsTVPopupMenu.Items.Add(Result);
  end;

begin
  BuildVarsTVPopupMenu.Items.Clear;
  GetSelectedNode(BuildProperty,NodeType);

  if NodeType in [cbmntBuildMode,cbmntValues,cbmntValue] then
    Add('New value',@NewValueClick);
  if NodeType in [cbmntValue] then
    Add('Delete value ...',@DeleteValueClick);
  AddSeparator;
  Add('New build mode',@NewBuildModeClick);
  if NodeType in [cbmntBuildMode] then
    Add('Delete build mode ...',@DeleteBuildModeClick);
  if NodeType in [cbmntDefaultValue,cbmntDefaultValueEditor] then begin
    Editor:=GetEditor(BuildProperty);
    Editor.FillPopupMenu(BuildVarsTVPopupMenu);
  end;
end;

procedure TCompOptBuildVarsFrame.BuildVarsTreeViewEditing(Sender: TObject;
  Node: TTreeNode; var AllowEdit: Boolean);
var
  BuildProperty: TLazBuildProperty;
  NodeType: TCBMNodeType;
begin
  NodeType:=GetNodeInfo(Node,BuildProperty);
  AllowEdit:=NodeType in [cbmntBuildMode,cbmntValue];
end;

procedure TCompOptBuildVarsFrame.BuildVarsTreeViewStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin

end;

procedure TCompOptBuildVarsFrame.BuildVarsTreeViewEdited(Sender: TObject;
  Node: TTreeNode; var S: string);
var
  BuildProperty: TLazBuildProperty;
  NodeType: TCBMNodeType;
  ConflictBuildProperty: TIDEBuildProperty;
  Index: LongInt;
begin
  NodeType:=GetNodeInfo(Node,BuildProperty);
  case NodeType of

  cbmntBuildMode:
    if S<>BuildProperty.Identifier then begin
      // rename build mode
      if (S='') or (not IsValidIdent(S)) then begin
        MessageDlg(lisCCOErrorCaption,
          Format(lisInvalidBuildModeTheBuildModeMustBeAPascalIdentifie, ['"',
            S, '"']),
          mtError,[mbCancel],0);
        S:=BuildProperty.Identifier;
        exit;
      end;
      ConflictBuildProperty:=BuildProperties.ModeWithIdentifier(S);
      if (ConflictBuildProperty<>nil) and (ConflictBuildProperty<>BuildProperty) then
      begin
        MessageDlg(lisCCOErrorCaption,
          Format(lisThereIsAlreadyABuildModeWithTheName, ['"', S, '"']),
          mtError,[mbCancel],0);
        S:=BuildProperty.Identifier;
        exit;
      end;
      BuildProperty.Identifier:=S;
    end;

  cbmntValue:
    begin
      Index:=Node.Index;
      Index:=BuildProperty.Values.IndexOf(S);
      if (Index>=0) and (Index<>Node.Index) then begin
        MessageDlg(lisCCOErrorCaption,
          Format(lisDuplicateFoundOfValue, ['"', S, '"']),
          mtError,[mbCancel],0);
        S:=BuildProperty.Values[Node.Index];
        exit;
      end;
      BuildProperty.Values[Node.Index]:=S;
    end;

  end;
end;

procedure TCompOptBuildVarsFrame.SetBuildProperties(
  const AValue: TIDEBuildProperties);
begin
  if FBuildProperties=AValue then exit;
  FBuildProperties:=AValue;
  RebuildTreeView;
end;

procedure TCompOptBuildVarsFrame.RebuildTreeView;
var
  i: Integer;
begin
  BuildVarsTreeView.BeginUpdate;
  BuildVarsTreeView.Items.Clear;
  FreeEditors;
  if BuildProperties<>nil then begin
    // first level: build modes
    for i:=0 to BuildProperties.Count-1 do
      TreeViewAddBuildMode(BuildProperties.Items[i]);
  end;
  BuildVarsTreeView.EndUpdate;
end;

procedure TCompOptBuildVarsFrame.TreeViewAddBuildMode(
  BuildProperty: TLazBuildProperty);
var
  TVNode: TTreeNode;
  ValuesTVNode: TTreeNode;
  Values: TStrings;
  i: Integer;
  DefValueTVNode: TTreeNode;
  Editor: TCompOptsExprEditor;
begin
  // create node for the build mode
  TVNode:=BuildVarsTreeView.Items.AddObject(nil,BuildProperty.Identifier,BuildProperty);
  TVNode.ImageIndex:=fModeImgID;
  TVNode.SelectedIndex:=TVNode.ImageIndex;
  // second level
  begin
    // parent node for values
    ValuesTVNode:=BuildVarsTreeView.Items.AddChild(TVNode, lisValues);
    ValuesTVNode.ImageIndex:=fValuesImgID;
    ValuesTVNode.SelectedIndex:=ValuesTVNode.ImageIndex;
    // a node for each value
    Values:=BuildProperty.Values;
    for i:=0 to Values.Count-1 do
      TreeViewAddValue(ValuesTVNode,Values[i]);
    // a node for the default value
    DefValueTVNode:=BuildVarsTreeView.Items.AddChild(TVNode,
      lisDefaultValue);
    DefValueTVNode.ImageIndex:=fDefValueImgID;
    DefValueTVNode.SelectedIndex:=DefValueTVNode.ImageIndex;
    // add default value nodes
    Editor:=TCompOptsExprEditor.Create(Self);
    Editor.DefaultNodeType:=cocntSetValue;
    Editor.DefaultValueType:=cocvtResult;
    FEditors.Add(Editor);
    Editor.Setup(BuildVarsTreeView,DefValueTVNode,
                 BuildProperty.DefaultValue as TCompOptConditionals,[cocvtResult]);
  end;
  //DebugLn(['TCompOptBuildModesFrame.TreeViewAddBuildMode ',TVNode.Text]);
  TVNode.Expand(true);
end;

procedure TCompOptBuildVarsFrame.TreeViewAddValue(ValuesTVNode: TTreeNode;
  aValue: string);
var
  ValueTVNode: TTreeNode;
begin
  ValueTVNode:=BuildVarsTreeView.Items.AddChild(ValuesTVNode,aValue);
  ValueTVNode.ImageIndex:=fValueImgID;
  ValueTVNode.SelectedIndex:=ValueTVNode.ImageIndex;
end;

function TCompOptBuildVarsFrame.GetNodeInfo(Node: TTreeNode; out
  BuildProperty: TLazBuildProperty): TCBMNodeType;

  function GetNodeType(CurNode: TTreeNode): TCBMNodeType;
  var
    ParentType: TCBMNodeType;
  begin
    if CurNode=nil then
      Result:=cbmntNone
    else if TObject(CurNode.Data) is TLazBuildProperty then begin
      BuildProperty:=TLazBuildProperty(CurNode.Data);
      Result:=cbmntBuildMode;
    end else begin
      ParentType:=GetNodeType(CurNode.Parent);
      case ParentType of
      cbmntBuildMode:
        if CurNode.Text=lisValues then
          Result:=cbmntValues
        else if CurNode.Text=lisDefaultValue then
          Result:=cbmntDefaultValue;
      cbmntValues:
        Result:=cbmntValue;
      cbmntDefaultValue:
        Result:=cbmntDefaultValueEditor;
      end;
    end;
  end;

begin
  BuildProperty:=nil;
  Result:=GetNodeType(Node);
end;

function TCompOptBuildVarsFrame.GetSelectedNode(out
  BuildProperty: TLazBuildProperty; out NodeType: TCBMNodeType): TTreeNode;
begin
  Result:=BuildVarsTreeView.Selected;
  NodeType:=GetNodeInfo(Result,BuildProperty);
end;

function TCompOptBuildVarsFrame.GetBuildModeTVNode(BuildProperty: TLazBuildProperty
  ): TTreeNode;
begin
  Result:=BuildVarsTreeView.Items.GetFirstNode;
  while (Result<>nil) and (TObject(Result.Data)<>BuildProperty) do
    Result:=Result.GetNextSibling;
end;

function TCompOptBuildVarsFrame.GetValuesTVNode(BuildProperty: TLazBuildProperty
  ): TTreeNode;
var
  BuildModeTVNode: TTreeNode;
begin
  BuildModeTVNode:=GetBuildModeTVNode(BuildProperty);
  if (BuildModeTVNode<>nil) then
    Result:=BuildModeTVNode.GetFirstChild
  else
    Result:=nil;
end;

procedure TCompOptBuildVarsFrame.FreeEditors;
var
  i: Integer;
begin
  for i:=0 to FEditors.Count-1 do
    TObject(FEditors[i]).Free;
  FEditors.Clear;
end;

function TCompOptBuildVarsFrame.GetEditor(BuildProperty: TLazBuildProperty
  ): TCompOptsExprEditor;
var
  i: Integer;
begin
  for i:=0 to FEditors.Count-1 do begin
    Result:=TCompOptsExprEditor(FEditors[i]);
    if Result.Conditionals=BuildProperty.DefaultValue then exit;
  end;
  Result:=nil;
end;

constructor TCompOptBuildVarsFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FEditors:=TFPList.Create;
  BuildVarsTreeView.Images := IDEImages.Images_24;
  fModeImgID:=IDEImages.LoadImage(24,'da_define');
  fValueImgID:=IDEImages.LoadImage(24,'da_define');
  fDefValueImgID:=IDEImages.LoadImage(24,'da_define');

  BuildVarsGroupBox.Caption:='Build modes';
end;

destructor TCompOptBuildVarsFrame.Destroy;
begin
  FreeEditors;
  FreeAndNil(FEditors);
  inherited Destroy;
end;

initialization
  {$I compiler_buildvar_options.lrs}

end.

