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
  Classes, SysUtils, FileUtil, LResources, Forms, ComCtrls, Menus,
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
  private
    FConditionals: TCompOptConditionals;
    FNodeTypeImageIDs: array[TCOCNodeType] of integer;
    procedure SetConditionals(const AValue: TCompOptConditionals);
    procedure FillTreeView;
    function NodeToCaption(Node: TCompOptCondNode): string;
    function NodeToImageIndex(Node: TCompOptCondNode): integer;
  public
    constructor Create(TheOwner: TComponent); override;
    property Conditionals: TCompOptConditionals read FConditionals write SetConditionals;
  end;

implementation

{ TCompOptsConditionalsFrame }

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
    TVNode:=COCTreeView.Items.AddChild(ParentTVNode,NodeToCaption(COCNode));
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

