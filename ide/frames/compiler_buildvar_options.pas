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
  Classes, SysUtils, LCLProc, FileUtil, Controls, Forms, StdCtrls,
  Grids, Buttons, ExtCtrls, Dialogs, ComCtrls, Menus, AvgLvlTree,
  IDEImagesIntf, ProjectIntf, PackageIntf, CompilerOptions,
  Compiler_CondTree, LazarusIDEStrConsts, CompOptsModes;

type
  TCBMNodeType = (
    cbmntNone,
    cbmntBuildMacro,
    cbmntValues,
    cbmntValue,
    cbmntDefaultValue,
    cbmntDefaultValueEditor
    );

  { TCompOptBuildMacrosFrame }

  TCompOptBuildMacrosFrame = class(TFrame)
    BuildMacrosGroupBox: TGroupBox;
    BuildMacrosTreeView: TTreeView;
    BuildMacrosTVPopupMenu: TPopupMenu;
    procedure BuildMacrosTreeViewEdited(Sender: TObject; Node: TTreeNode;
      var S: string);
    procedure BuildMacrosTreeViewEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure BuildMacrosTreeViewStartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure BuildMacrosTVPopupMenuPopup(Sender: TObject);
    procedure DeleteBuildMacroClick(Sender: TObject);
    procedure NewBuildMacroClick(Sender: TObject);
    procedure NewValueClick(Sender: TObject);
    procedure DeleteValueClick(Sender: TObject);
  private
    FBuildMacros: TIDEBuildMacros;
    fVarImgID: LongInt;
    fValuesImgID: LongInt;
    fValueImgID: LongInt;
    fDefValueImgID: LongInt;
    FEditors: TFPList;// list of TCompOptsExprEditor
    procedure SetBuildMacros(const AValue: TIDEBuildMacros);
    procedure RebuildTreeView;
    procedure TreeViewAddBuildMacro(BuildProperty: TLazBuildMacro);
    procedure TreeViewAddValue(ValuesTVNode: TTreeNode; aValue: string);
    function GetNodeInfo(Node: TTreeNode; out BuildProperty: TLazBuildMacro): TCBMNodeType;
    function GetSelectedNode(out BuildProperty: TLazBuildMacro;
                             out NodeType: TCBMNodeType): TTreeNode;
    function GetBuildMacroTVNode(BuildProperty: TLazBuildMacro): TTreeNode;
    function GetValuesTVNode(BuildProperty: TLazBuildMacro): TTreeNode;
    procedure FreeEditors;
    function GetEditor(BuildProperty: TLazBuildMacro): TCompOptsExprEditor;
    function GetVariablePrefix: string;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property BuildMacros: TIDEBuildMacros read FBuildMacros write SetBuildMacros;
  end;

implementation

{$R *.lfm}

{ TCompOptBuildMacrosFrame }

procedure TCompOptBuildMacrosFrame.NewBuildMacroClick(Sender: TObject);
var
  NewIdentifier: String;
  NewBuildMacro: TLazBuildMacro;
  SetResultNode: TCompOptCondNode;
begin
  NewIdentifier:=DefaultBuildModeGraph.GetUniqueVarName(GetVariablePrefix,BuildMacros);
  NewBuildMacro:=BuildMacros.Add(NewIdentifier);
  // add a node
  SetResultNode:=TCompOptCondNode.Create(NewBuildMacro.DefaultValue);
  SetResultNode.NodeType:=cocntSetValue;
  SetResultNode.ValueType:=cocvtResult;
  NewBuildMacro.DefaultValue.Root.AddLast(SetResultNode);
  // add to TreeView
  BuildMacrosTreeView.BeginUpdate;
  TreeViewAddBuildMacro(NewBuildMacro);
  BuildMacrosTreeView.EndUpdate;
end;

procedure TCompOptBuildMacrosFrame.NewValueClick(Sender: TObject);
var
  BuildProperty: TLazBuildMacro;
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
  BuildMacrosTreeView.BeginUpdate;
  ValuesTVNode:=GetValuesTVNode(BuildProperty);
  TreeViewAddValue(ValuesTVNode,NewValueStr);
  ValuesTVNode.Expand(true);
  BuildMacrosTreeView.EndUpdate;
end;

procedure TCompOptBuildMacrosFrame.DeleteValueClick(Sender: TObject);
var
  BuildProperty: TLazBuildMacro;
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
  BuildMacrosTreeView.BeginUpdate;
  SelTVNode.Delete;
  BuildMacrosTreeView.EndUpdate;
end;

procedure TCompOptBuildMacrosFrame.DeleteBuildMacroClick(Sender: TObject);
var
  aBuildMacro: TIDEBuildMacro;
  SelTVNode: TTreeNode;
  NodeType: TCBMNodeType;
  i: LongInt;
  Editor: TCompOptsExprEditor;
begin
  SelTVNode:=GetSelectedNode(TLazBuildMacro(aBuildMacro),NodeType);
  if aBuildMacro=nil then exit;
  if MessageDlg(lisConfirmDelete,
    Format(lisDeleteBuildMacro, ['"', aBuildMacro.Identifier, '"']),
    mtConfirmation,[mbYes,mbCancel],0)<>mrYes
  then exit;
  i:=BuildMacros.IndexOfIdentifier(aBuildMacro.Identifier);
  Editor:=GetEditor(aBuildMacro);
  FEditors.Remove(Editor);
  Editor.Free;
  BuildMacros.Delete(i);
  BuildMacrosTreeView.BeginUpdate;
  SelTVNode.Delete;
  BuildMacrosTreeView.EndUpdate;
end;

procedure TCompOptBuildMacrosFrame.BuildMacrosTVPopupMenuPopup(Sender: TObject);
var
  BuildProperty: TLazBuildMacro;
  NodeType: TCBMNodeType;
  Editor: TCompOptsExprEditor;

  function Add(const aCaption: string; const OnClickEvent: TNotifyEvent): TMenuItem;
  begin
    Result:=TMenuItem.Create(Self);
    Result.Caption:=aCaption;
    Result.OnClick:=OnClickEvent;
    BuildMacrosTVPopupMenu.Items.Add(Result);
  end;

  function AddSeparator: TMenuItem;
  begin
    Result:=nil;
    if BuildMacrosTVPopupMenu.Items.Count=0 then exit;
    Result:=TMenuItem.Create(Self);
    Result.Caption:='-';
    BuildMacrosTVPopupMenu.Items.Add(Result);
  end;

begin
  BuildMacrosTVPopupMenu.Items.Clear;
  GetSelectedNode(BuildProperty,NodeType);

  if NodeType in [cbmntBuildMacro,cbmntValues,cbmntValue] then
    Add('New value',@NewValueClick);
  if NodeType in [cbmntValue] then
    Add('Delete value ...',@DeleteValueClick);
  AddSeparator;
  Add('New build macro',@NewBuildMacroClick);
  if NodeType in [cbmntBuildMacro] then
    Add('Delete build macro ...',@DeleteBuildMacroClick);
  if NodeType in [cbmntDefaultValue,cbmntDefaultValueEditor] then begin
    Editor:=GetEditor(BuildProperty);
    Editor.FillPopupMenu(BuildMacrosTVPopupMenu);
  end;
end;

procedure TCompOptBuildMacrosFrame.BuildMacrosTreeViewEditing(Sender: TObject;
  Node: TTreeNode; var AllowEdit: Boolean);
var
  BuildProperty: TLazBuildMacro;
  NodeType: TCBMNodeType;
begin
  NodeType:=GetNodeInfo(Node,BuildProperty);
  AllowEdit:=NodeType in [cbmntBuildMacro,cbmntValue];
end;

procedure TCompOptBuildMacrosFrame.BuildMacrosTreeViewStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin

end;

procedure TCompOptBuildMacrosFrame.BuildMacrosTreeViewEdited(Sender: TObject;
  Node: TTreeNode; var S: string);
var
  BuildProperty: TLazBuildMacro;
  NodeType: TCBMNodeType;
  ConflictBuildProperty: TIDEBuildMacro;
  Index: LongInt;
begin
  NodeType:=GetNodeInfo(Node,BuildProperty);
  case NodeType of

  cbmntBuildMacro:
    if S<>BuildProperty.Identifier then begin
      // rename build macro
      if (S='') or (not IsValidIdent(S)) then begin
        MessageDlg(lisCCOErrorCaption,
          Format(lisInvalidBuildMacroTheBuildMacroMustBeAPascalIdentifie, ['"',
            S, '"']),
          mtError,[mbCancel],0);
        S:=BuildProperty.Identifier;
        exit;
      end;
      ConflictBuildProperty:=BuildMacros.VarWithIdentifier(S);
      if (ConflictBuildProperty<>nil) and (ConflictBuildProperty<>BuildProperty) then
      begin
        MessageDlg(lisCCOErrorCaption,
          Format(lisThereIsAlreadyABuildMacroWithTheName, ['"', S, '"']),
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

procedure TCompOptBuildMacrosFrame.SetBuildMacros(
  const AValue: TIDEBuildMacros);
begin
  if FBuildMacros=AValue then exit;
  FBuildMacros:=AValue;
  RebuildTreeView;
end;

procedure TCompOptBuildMacrosFrame.RebuildTreeView;
var
  i: Integer;
begin
  BuildMacrosTreeView.BeginUpdate;
  BuildMacrosTreeView.Items.Clear;
  FreeEditors;
  if BuildMacros<>nil then begin
    // first level: build macros
    for i:=0 to BuildMacros.Count-1 do
      TreeViewAddBuildMacro(BuildMacros.Items[i]);
  end;
  BuildMacrosTreeView.EndUpdate;
end;

procedure TCompOptBuildMacrosFrame.TreeViewAddBuildMacro(
  BuildProperty: TLazBuildMacro);
var
  TVNode: TTreeNode;
  ValuesTVNode: TTreeNode;
  Values: TStrings;
  i: Integer;
  DefValueTVNode: TTreeNode;
  Editor: TCompOptsExprEditor;
begin
  // create node for the build macro
  TVNode:=BuildMacrosTreeView.Items.AddObject(nil,BuildProperty.Identifier,BuildProperty);
  TVNode.ImageIndex:=fVarImgID;
  TVNode.SelectedIndex:=TVNode.ImageIndex;
  // second level
  begin
    // parent node for values
    ValuesTVNode:=BuildMacrosTreeView.Items.AddChild(TVNode, lisValues);
    ValuesTVNode.ImageIndex:=fValuesImgID;
    ValuesTVNode.SelectedIndex:=ValuesTVNode.ImageIndex;
    // a node for each value
    Values:=BuildProperty.Values;
    for i:=0 to Values.Count-1 do
      TreeViewAddValue(ValuesTVNode,Values[i]);
    // a node for the default value
    DefValueTVNode:=BuildMacrosTreeView.Items.AddChild(TVNode,
      lisDefaultValue);
    DefValueTVNode.ImageIndex:=fDefValueImgID;
    DefValueTVNode.SelectedIndex:=DefValueTVNode.ImageIndex;
    // add default value nodes
    Editor:=TCompOptsExprEditor.Create(Self);
    Editor.DefaultNodeType:=cocntSetValue;
    Editor.DefaultValueType:=cocvtResult;
    FEditors.Add(Editor);
    Editor.Setup(BuildMacrosTreeView,DefValueTVNode,
                 BuildProperty.DefaultValue as TCompOptConditionals,[cocvtResult]);
  end;
  //DebugLn(['TCompOptBuildMacrosFrame.TreeViewAddBuildMacro ',TVNode.Text]);
  TVNode.Expand(true);
end;

procedure TCompOptBuildMacrosFrame.TreeViewAddValue(ValuesTVNode: TTreeNode;
  aValue: string);
var
  ValueTVNode: TTreeNode;
begin
  ValueTVNode:=BuildMacrosTreeView.Items.AddChild(ValuesTVNode,aValue);
  ValueTVNode.ImageIndex:=fValueImgID;
  ValueTVNode.SelectedIndex:=ValueTVNode.ImageIndex;
end;

function TCompOptBuildMacrosFrame.GetNodeInfo(Node: TTreeNode; out
  BuildProperty: TLazBuildMacro): TCBMNodeType;

  function GetNodeType(CurNode: TTreeNode): TCBMNodeType;
  var
    ParentType: TCBMNodeType;
  begin
    if CurNode=nil then
      Result:=cbmntNone
    else if TObject(CurNode.Data) is TLazBuildMacro then begin
      BuildProperty:=TLazBuildMacro(CurNode.Data);
      Result:=cbmntBuildMacro;
    end else begin
      ParentType:=GetNodeType(CurNode.Parent);
      case ParentType of
      cbmntBuildMacro:
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

function TCompOptBuildMacrosFrame.GetSelectedNode(out
  BuildProperty: TLazBuildMacro; out NodeType: TCBMNodeType): TTreeNode;
begin
  Result:=BuildMacrosTreeView.Selected;
  NodeType:=GetNodeInfo(Result,BuildProperty);
end;

function TCompOptBuildMacrosFrame.GetBuildMacroTVNode(BuildProperty: TLazBuildMacro
  ): TTreeNode;
begin
  Result:=BuildMacrosTreeView.Items.GetFirstNode;
  while (Result<>nil) and (TObject(Result.Data)<>BuildProperty) do
    Result:=Result.GetNextSibling;
end;

function TCompOptBuildMacrosFrame.GetValuesTVNode(BuildProperty: TLazBuildMacro
  ): TTreeNode;
var
  BuildMacroTVNode: TTreeNode;
begin
  BuildMacroTVNode:=GetBuildMacroTVNode(BuildProperty);
  if (BuildMacroTVNode<>nil) then
    Result:=BuildMacroTVNode.GetFirstChild
  else
    Result:=nil;
end;

procedure TCompOptBuildMacrosFrame.FreeEditors;
var
  i: Integer;
begin
  for i:=0 to FEditors.Count-1 do
    TObject(FEditors[i]).Free;
  FEditors.Clear;
end;

function TCompOptBuildMacrosFrame.GetEditor(BuildProperty: TLazBuildMacro
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

function TCompOptBuildMacrosFrame.GetVariablePrefix: string;
begin
  Result:='BuildMacro';
  if (BuildMacros=nil) or (BuildMacros.Owner=nil) then exit;
  if BuildMacros.Owner is TIDEPackage then
    Result:=TIDEPackage(BuildMacros.Owner).Name+'_macro';
end;

constructor TCompOptBuildMacrosFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FEditors:=TFPList.Create;
  BuildMacrosTreeView.Images := IDEImages.Images_24;
  fVarImgID:=IDEImages.LoadImage(24,'da_define');
  fValueImgID:=IDEImages.LoadImage(24,'da_define');
  fDefValueImgID:=IDEImages.LoadImage(24,'da_define');

  BuildMacrosGroupBox.Caption:=lisBuildMacros;
end;

destructor TCompOptBuildMacrosFrame.Destroy;
begin
  FreeEditors;
  FreeAndNil(FEditors);
  inherited Destroy;
end;

end.

