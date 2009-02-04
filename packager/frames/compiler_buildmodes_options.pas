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

  { TCompOptBuildModesFrame }

  TCompOptBuildModesFrame = class(TFrame)
    BuildModesGroupBox: TGroupBox;
    BuildModesTreeView: TTreeView;
    BuildModeTVPopupMenu: TPopupMenu;
    procedure BuildModeTVPopupMenuPopup(Sender: TObject);
    procedure DeleteSpeedButtonClick(Sender: TObject);
    procedure NewSpeedButtonClick(Sender: TObject);
  private
    FBuildModes: TIDEBuildModes;
    fModeImgID: LongInt;
    fValuesImgID: LongInt;
    fValueImgID: LongInt;
    fDefValueImgID: LongInt;
    procedure SetBuildModes(const AValue: TIDEBuildModes);
    procedure RebuildTreeView;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property BuildModes: TIDEBuildModes read FBuildModes write SetBuildModes;
  end;

implementation

{ TCompOptBuildModesFrame }

procedure TCompOptBuildModesFrame.NewSpeedButtonClick(Sender: TObject);
{var
  NewIdentifier: String;}
begin
{  NewIdentifier:=GlobalBuildModeSet.GetUniqueModeName(BuildModes);
  BuildModes.Add(NewIdentifier);
  ModesGrid.RowCount:=BuildModes.Count;
  ModesGrid.Cells[0,BuildModes.Count-1]:=NewIdentifier;
  ModesGrid.Row:=BuildModes.Count-1;}
end;

procedure TCompOptBuildModesFrame.DeleteSpeedButtonClick(Sender: TObject);
{var
  BuildMode: TIDEBuildMode;
  i: LongInt;}
begin
{  if not GetSelectedBuildMode(BuildMode) then exit;
  i:=ModesGrid.Row;
  if MessageDlg('Confirm delete',
    'Delete build mode "'+BuildMode.Identifier+'"?',
    mtConfirmation,[mbYes,mbCancel],0)<>mrYes
  then exit;
  BuildModes.Delete(i);
  ModesGrid.DeleteColRow(false,i);
  if i=ModesGrid.RowCount then
    dec(i);
  ModesGrid.Row:=i;}
end;

procedure TCompOptBuildModesFrame.BuildModeTVPopupMenuPopup(Sender: TObject);
var
  SelTVNode: TTreeNode;
begin
  SelTVNode:=BuildModesTreeView.Selected;

  if SelTVNode=nil then begin
    // no node selected

  end else begin

  end;
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
  TVNode: TTreeNode;
  BuildMode: TLazBuildMode;
  j: Integer;
  Values: TStrings;
  ValueTVNode: TTreeNode;
  ValuesTVNode: TTreeNode;
  DefValueTVNode: TTreeNode;
begin
  BuildModesTreeView.BeginUpdate;
  BuildModesTreeView.Items.Clear;
  if BuildModes<>nil then begin
    // first level: build modes
    for i:=0 to BuildModes.Count-1 do begin
      BuildMode:=BuildModes.Items[i];
      // create node for the build mode
      TVNode:=BuildModesTreeView.Items.AddObject(nil,BuildMode.Identifier,BuildMode);
      TVNode.ImageIndex:=fModeImgID;
      TVNode.StateIndex:=TVNode.ImageIndex;
      TVNode.SelectedIndex:=TVNode.ImageIndex;
      // second level
      begin
        // parent node for values
        ValuesTVNode:=BuildModesTreeView.Items.AddChild(TVNode,'Values');
        ValuesTVNode.ImageIndex:=fValuesImgID;
        ValuesTVNode.StateIndex:=ValuesTVNode.ImageIndex;
        ValuesTVNode.SelectedIndex:=ValuesTVNode.ImageIndex;
        // a node for each value
        Values:=BuildMode.Values;
        for j:=0 to Values.Count-1 do begin
          ValueTVNode:=BuildModesTreeView.Items.AddChild(ValuesTVNode,Values[j]);
          ValueTVNode.ImageIndex:=fValueImgID;
          ValueTVNode.StateIndex:=ValueTVNode.ImageIndex;
          ValueTVNode.SelectedIndex:=ValueTVNode.ImageIndex;
        end;
        // a node for the default value
        DefValueTVNode:=BuildModesTreeView.Items.AddChild(TVNode,'Default value');
        DefValueTVNode.ImageIndex:=fDefValueImgID;
        DefValueTVNode.StateIndex:=DefValueTVNode.ImageIndex;
        DefValueTVNode.SelectedIndex:=DefValueTVNode.ImageIndex;
        // ToDo: add default value nodes
      end;
      TVNode.Expand(true);
    end;
  end;
  BuildModesTreeView.EndUpdate;
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

