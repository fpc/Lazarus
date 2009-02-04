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
unit Compiler_BuildModes_Options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Controls, LResources, Forms, StdCtrls, Grids,
  Buttons, ExtCtrls, Dialogs, ComCtrls, Menus,
  IDEImagesIntf, ProjectIntf, CompilerOptions,
  Compiler_Conditionals_Options, LazarusIDEStrConsts, CompOptsModes;

type
  TCBMNodeType = (
    cbmntNone,
    cbmntBuildMode,
    cbmntValues,
    cbmntValue,
    cbmntDefaultValue
    );

  { TCompOptBuildModesFrame }

  TCompOptBuildModesFrame = class(TFrame)
    BuildModesGroupBox: TGroupBox;
    BuildModesTreeView: TTreeView;
    BuildModeTVPopupMenu: TPopupMenu;
    procedure BuildModeTVPopupMenuPopup(Sender: TObject);
    procedure DeleteBuildModeClick(Sender: TObject);
    procedure NewBuildModeClick(Sender: TObject);
    procedure NewValueClick(Sender: TObject);
    procedure DeleteValueClick(Sender: TObject);
  private
    FBuildModes: TIDEBuildModes;
    fModeImgID: LongInt;
    fValuesImgID: LongInt;
    fValueImgID: LongInt;
    fDefValueImgID: LongInt;
    procedure SetBuildModes(const AValue: TIDEBuildModes);
    procedure RebuildTreeView;
    procedure TreeViewAddBuildMode(BuildMode: TLazBuildMode);
    procedure TreeViewAddValue(ValuesTVNode: TTreeNode; aValue: string);
    function GetSelectedNode(out BuildMode: TLazBuildMode;
                             out NodeType: TCBMNodeType): TTreeNode;
    function GetBuildModeTVNode(BuildMode: TLazBuildMode): TTreeNode;
    function GetValuesTVNode(BuildMode: TLazBuildMode): TTreeNode;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property BuildModes: TIDEBuildModes read FBuildModes write SetBuildModes;
  end;

implementation

{ TCompOptBuildModesFrame }

procedure TCompOptBuildModesFrame.NewBuildModeClick(Sender: TObject);
var
  NewIdentifier: String;
  NewBuildMode: TLazBuildMode;
begin
  NewIdentifier:=GlobalBuildModeSet.GetUniqueModeName(BuildModes);
  NewBuildMode:=BuildModes.Add(NewIdentifier);
  BuildModesTreeView.BeginUpdate;
  TreeViewAddBuildMode(NewBuildMode);
  BuildModesTreeView.EndUpdate;
end;

procedure TCompOptBuildModesFrame.NewValueClick(Sender: TObject);
var
  BuildMode: TLazBuildMode;
  NodeType: TCBMNodeType;
  i: Integer;
  NewValueStr: String;
  ValuesTVNode: TTreeNode;
begin
  GetSelectedNode(BuildMode,NodeType);
  if BuildMode=nil then exit;
  i:=1;
  repeat
    NewValueStr:=Format(lisValue2, [IntToStr(i)]);
    if BuildMode.Values.IndexOf(NewValueStr)<0 then break;
    inc(i);
  until false;
  BuildMode.Values.Add(NewValueStr);
  BuildModesTreeView.BeginUpdate;
  ValuesTVNode:=GetValuesTVNode(BuildMode);
  TreeViewAddValue(ValuesTVNode,NewValueStr);
  ValuesTVNode.Expand(true);
  BuildModesTreeView.EndUpdate;
end;

procedure TCompOptBuildModesFrame.DeleteValueClick(Sender: TObject);
var
  BuildMode: TLazBuildMode;
  NodeType: TCBMNodeType;
  SelTVNode: TTreeNode;
  aValue: String;
  i: LongInt;
begin
  SelTVNode:=GetSelectedNode(BuildMode,NodeType);
  if NodeType<>cbmntValue then exit;
  aValue:=SelTVNode.Text;
  if MessageDlg(lisConfirmDelete,
    Format(lisDeleteValue, ['"', aValue, '"']),
    mtConfirmation,[mbYes,mbCancel],0)<>mrYes
  then exit;
  i:=BuildMode.Values.IndexOf(aValue);
  if i>=0 then BuildMode.Values.Delete(i);
  BuildModesTreeView.BeginUpdate;
  SelTVNode.Delete;
  BuildModesTreeView.EndUpdate;
end;

procedure TCompOptBuildModesFrame.DeleteBuildModeClick(Sender: TObject);
var
  BuildMode: TIDEBuildMode;
  SelTVNode: TTreeNode;
  NodeType: TCBMNodeType;
  i: LongInt;
begin
  SelTVNode:=GetSelectedNode(BuildMode,NodeType);
  if BuildMode=nil then exit;
  if MessageDlg(lisConfirmDelete,
    Format(lisDeleteBuildMode, ['"', BuildMode.Identifier, '"']),
    mtConfirmation,[mbYes,mbCancel],0)<>mrYes
  then exit;
  i:=BuildModes.IndexOfIdentifier(BuildMode.Identifier);
  BuildModes.Delete(i);
  BuildModesTreeView.BeginUpdate;
  SelTVNode.Delete;
  BuildModesTreeView.EndUpdate;
end;

procedure TCompOptBuildModesFrame.BuildModeTVPopupMenuPopup(Sender: TObject);
var
  BuildMode: TLazBuildMode;
  NodeType: TCBMNodeType;

  function Add(const aCaption: string; const OnClickEvent: TNotifyEvent): TMenuItem;
  begin
    Result:=TMenuItem.Create(Self);
    Result.Caption:=aCaption;
    Result.OnClick:=OnClickEvent;
    BuildModeTVPopupMenu.Items.Add(Result);
  end;

  function AddSeparator: TMenuItem;
  begin
    Result:=nil;
    if BuildModeTVPopupMenu.Items.Count=0 then exit;
    Result:=TMenuItem.Create(Self);
    Result.Caption:='-';
    BuildModeTVPopupMenu.Items.Add(Result);
  end;

begin
  BuildModeTVPopupMenu.Items.Clear;
  GetSelectedNode(BuildMode,NodeType);

  if NodeType in [cbmntBuildMode,cbmntValues,cbmntValue] then
    Add('New value',@NewValueClick);
  if NodeType in [cbmntValue] then
    Add('Delete value ...',@DeleteValueClick);
  AddSeparator;
  Add('New build mode',@NewBuildModeClick);
  if NodeType in [cbmntBuildMode] then
    Add('Delete build mode ...',@DeleteBuildModeClick);
end;

procedure TCompOptBuildModesFrame.SetBuildModes(const AValue: TIDEBuildModes);
begin
  if FBuildModes=AValue then exit;
  FBuildModes:=AValue;
  RebuildTreeView;
end;

procedure TCompOptBuildModesFrame.RebuildTreeView;
var
  i: Integer;
begin
  BuildModesTreeView.BeginUpdate;
  BuildModesTreeView.Items.Clear;
  if BuildModes<>nil then begin
    // first level: build modes
    for i:=0 to BuildModes.Count-1 do
      TreeViewAddBuildMode(BuildModes.Items[i]);
  end;
  BuildModesTreeView.EndUpdate;
end;

procedure TCompOptBuildModesFrame.TreeViewAddBuildMode(BuildMode: TLazBuildMode
  );
var
  TVNode: TTreeNode;
  ValuesTVNode: TTreeNode;
  Values: TStrings;
  i: Integer;
  DefValueTVNode: TTreeNode;
begin
  // create node for the build mode
  TVNode:=BuildModesTreeView.Items.AddObject(nil,BuildMode.Identifier,BuildMode);
  TVNode.ImageIndex:=fModeImgID;
  TVNode.StateIndex:=TVNode.ImageIndex;
  TVNode.SelectedIndex:=TVNode.ImageIndex;
  // second level
  begin
    // parent node for values
    ValuesTVNode:=BuildModesTreeView.Items.AddChild(TVNode, lisValues);
    ValuesTVNode.ImageIndex:=fValuesImgID;
    ValuesTVNode.StateIndex:=ValuesTVNode.ImageIndex;
    ValuesTVNode.SelectedIndex:=ValuesTVNode.ImageIndex;
    // a node for each value
    Values:=BuildMode.Values;
    for i:=0 to Values.Count-1 do
      TreeViewAddValue(ValuesTVNode,Values[i]);
    // a node for the default value
    DefValueTVNode:=BuildModesTreeView.Items.AddChild(TVNode,
      lisDefaultValue);
    DefValueTVNode.ImageIndex:=fDefValueImgID;
    DefValueTVNode.StateIndex:=DefValueTVNode.ImageIndex;
    DefValueTVNode.SelectedIndex:=DefValueTVNode.ImageIndex;
    // ToDo: add default value nodes
  end;
  TVNode.Expand(true);
end;

procedure TCompOptBuildModesFrame.TreeViewAddValue(ValuesTVNode: TTreeNode;
  aValue: string);
var
  ValueTVNode: TTreeNode;
begin
  ValueTVNode:=BuildModesTreeView.Items.AddChild(ValuesTVNode,aValue);
  ValueTVNode.ImageIndex:=fValueImgID;
  ValueTVNode.StateIndex:=ValueTVNode.ImageIndex;
  ValueTVNode.SelectedIndex:=ValueTVNode.ImageIndex;
end;

function TCompOptBuildModesFrame.GetSelectedNode(out
  BuildMode: TLazBuildMode; out NodeType: TCBMNodeType): TTreeNode;

  function GetNodeType(Node: TTreeNode): TCBMNodeType;
  var
    ParentType: TCBMNodeType;
  begin
    if Node=nil then
      Result:=cbmntNone
    else if TObject(Node.Data) is TLazBuildMode then begin
      BuildMode:=TLazBuildMode(Node.Data);
      Result:=cbmntBuildMode;
    end else begin
      ParentType:=GetNodeType(Node.Parent);
      case ParentType of
      cbmntBuildMode:
        if Node.Text=lisValues then
          Result:=cbmntValues
        else if Node.Text=lisDefaultValue then
          Result:=cbmntDefaultValue;
      cbmntValues:
        Result:=cbmntValue;
      cbmntDefaultValue:
        // ToDo
        ;
      end;
    end;
  end;

begin
  BuildMode:=nil;
  Result:=BuildModesTreeView.Selected;
  NodeType:=GetNodeType(Result);
end;

function TCompOptBuildModesFrame.GetBuildModeTVNode(BuildMode: TLazBuildMode
  ): TTreeNode;
begin
  Result:=BuildModesTreeView.Items.GetFirstNode;
  while (Result<>nil) and (TObject(Result.Data)<>BuildMode) do
    Result:=Result.GetNextSibling;
end;

function TCompOptBuildModesFrame.GetValuesTVNode(BuildMode: TLazBuildMode
  ): TTreeNode;
var
  BuildModeTVNode: TTreeNode;
begin
  BuildModeTVNode:=GetBuildModeTVNode(BuildMode);
  if (BuildModeTVNode<>nil) then
    Result:=BuildModeTVNode.GetFirstChild
  else
    Result:=nil;
end;

constructor TCompOptBuildModesFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  BuildModesTreeView.Images := IDEImages.Images_24;
  fModeImgID:=IDEImages.LoadImage(24,'da_define');
  fValueImgID:=IDEImages.LoadImage(24,'da_define');
  fDefValueImgID:=IDEImages.LoadImage(24,'da_define');

  BuildModesGroupBox.Caption:='Build modes';
end;

destructor TCompOptBuildModesFrame.Destroy;
begin

  inherited Destroy;
end;

initialization
  {$I compiler_buildmodes_options.lrs}

end.

