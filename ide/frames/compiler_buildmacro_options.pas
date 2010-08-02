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
unit Compiler_BuildMacro_Options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, FileUtil, Controls, Forms, StdCtrls,
  Grids, Buttons, ExtCtrls, Dialogs, ComCtrls, Menus, AvgLvlTree,
  IDEImagesIntf, ProjectIntf, PackageIntf, CompilerOptions,
  Compiler_CondTree, LazarusIDEStrConsts, CompOptsModes, PackageDefs;

type
  TCBMNodeType = (
    cbmntNone,
    cbmntBuildMacro,
    cbmntValues,
    cbmntValue
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
    procedure BuildMacrosTreeViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
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
    procedure TreeViewAddBuildMacro(aBuildMacro: TLazBuildMacro);
    procedure TreeViewAddValue(ValuesTVNode: TTreeNode; aValue: string);
    function GetNodeInfo(Node: TTreeNode; out BuildProperty: TLazBuildMacro): TCBMNodeType;
    function GetSelectedNode(out aBuildMacro: TLazBuildMacro;
                             out NodeType: TCBMNodeType): TTreeNode;
    function GetBuildMacroTVNode(aBuildMacro: TLazBuildMacro): TTreeNode;
    function GetValuesTVNode(aBuildMacro: TLazBuildMacro): TTreeNode;
    procedure FreeEditors;
    function GetMacroNamePrefix: string;
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
begin
  NewIdentifier:=DefaultBuildModeGraph.GetUniqueVarName(GetMacroNamePrefix,BuildMacros);
  NewBuildMacro:=BuildMacros.Add(NewIdentifier);
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
begin
  SelTVNode:=GetSelectedNode(TLazBuildMacro(aBuildMacro),NodeType);
  if aBuildMacro=nil then exit;
  if MessageDlg(lisConfirmDelete,
    Format(lisDeleteBuildMacro, ['"', aBuildMacro.Identifier, '"']),
    mtConfirmation,[mbYes,mbCancel],0)<>mrYes
  then exit;
  i:=BuildMacros.IndexOfIdentifier(aBuildMacro.Identifier);
  BuildMacros.Delete(i);
  BuildMacrosTreeView.BeginUpdate;
  SelTVNode.Delete;
  BuildMacrosTreeView.EndUpdate;
end;

procedure TCompOptBuildMacrosFrame.BuildMacrosTVPopupMenuPopup(Sender: TObject);
var
  aBuildMacro: TLazBuildMacro;
  NodeType: TCBMNodeType;

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
  GetSelectedNode(aBuildMacro,NodeType);

  if NodeType in [cbmntBuildMacro,cbmntValues,cbmntValue] then
    Add('New value',@NewValueClick);
  if NodeType in [cbmntValue] then
    Add('Delete value ...',@DeleteValueClick);
  AddSeparator;
  Add('New build macro',@NewBuildMacroClick);
  if NodeType in [cbmntBuildMacro] then
    Add('Delete build macro ...',@DeleteBuildMacroClick);
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

procedure TCompOptBuildMacrosFrame.BuildMacrosTreeViewKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin

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
  aBuildMacro: TLazBuildMacro);
var
  TVNode: TTreeNode;
  ValuesTVNode: TTreeNode;
  Values: TStrings;
  i: Integer;
begin
  // create node for the build macro
  TVNode:=BuildMacrosTreeView.Items.AddObject(nil,aBuildMacro.Identifier,aBuildMacro);
  TVNode.ImageIndex:=fVarImgID;
  TVNode.SelectedIndex:=TVNode.ImageIndex;
  // second level
  begin
    // parent node for values
    ValuesTVNode:=BuildMacrosTreeView.Items.AddChild(TVNode, lisValues);
    ValuesTVNode.ImageIndex:=fValuesImgID;
    ValuesTVNode.SelectedIndex:=ValuesTVNode.ImageIndex;
    // a node for each value
    Values:=aBuildMacro.Values;
    for i:=0 to Values.Count-1 do
      TreeViewAddValue(ValuesTVNode,Values[i]);
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
          Result:=cbmntValues;
      cbmntValues:
        Result:=cbmntValue;
      end;
    end;
  end;

begin
  BuildProperty:=nil;
  Result:=GetNodeType(Node);
end;

function TCompOptBuildMacrosFrame.GetSelectedNode(out
  aBuildMacro: TLazBuildMacro; out NodeType: TCBMNodeType): TTreeNode;
begin
  Result:=BuildMacrosTreeView.Selected;
  NodeType:=GetNodeInfo(Result,aBuildMacro);
end;

function TCompOptBuildMacrosFrame.GetBuildMacroTVNode(aBuildMacro: TLazBuildMacro
  ): TTreeNode;
begin
  Result:=BuildMacrosTreeView.Items.GetFirstNode;
  while (Result<>nil) and (TObject(Result.Data)<>aBuildMacro) do
    Result:=Result.GetNextSibling;
end;

function TCompOptBuildMacrosFrame.GetValuesTVNode(aBuildMacro: TLazBuildMacro
  ): TTreeNode;
var
  BuildMacroTVNode: TTreeNode;
begin
  BuildMacroTVNode:=GetBuildMacroTVNode(aBuildMacro);
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

function TCompOptBuildMacrosFrame.GetMacroNamePrefix: string;
begin
  Result:='BuildMacro';
  if (BuildMacros=nil) or (BuildMacros.Owner=nil) then exit;
  if BuildMacros.Owner is TPkgCompilerOptions then
    Result:=TPkgCompilerOptions(BuildMacros.Owner).LazPackage.Name+'_macro';
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

