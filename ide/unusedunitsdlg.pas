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
    A dialog showing the unused units of the current unit
    (at cursor in source editor).
    With the ability to remove them automatically.
}
unit UnusedUnitsDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, LCLProc, LResources, Forms, Controls,
  ButtonPanel, ComCtrls,
  SrcEditorIntf, LazIDEIntf, IDEImagesIntf,
  CodeCache, CodeToolManager;

type

  { TUnusedUnitsDialog }

  TUnusedUnitsDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    UnitsTreeView: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure OkClick(Sender: TObject);
  private
    FUnits: TStrings;
    ImgIDInterface: LongInt;
    ImgIDImplementation: LongInt;
    ImgIDInitialization: LongInt;
    ImgIDNone: LongInt;
    procedure SetUnits(const AValue: TStrings);
    procedure RebuildUnitsTreeView;
  public
    function GetSelectedUnits: TStrings;
    property Units: TStrings read FUnits write SetUnits;
  end;


function ShowUnusedUnitsDialog: TModalResult;

implementation

function ShowUnusedUnitsDialog: TModalResult;
var
  UnusedUnitsDialog: TUnusedUnitsDialog;
  SrcEdit: TSourceEditorInterface;
  Code: TCodeBuffer;
  Units: TStringList;
  RemoveUnits: TStrings;
  i: Integer;
begin
  Result:=mrOk;
  if not LazarusIDE.BeginCodeTools then exit;

  // get cursor position
  SrcEdit:=SourceEditorWindow.ActiveEditor;
  if SrcEdit=nil then exit;
  Code:=TCodeBuffer(SrcEdit.CodeToolsBuffer);
  if Code=nil then exit;

  UnusedUnitsDialog:=nil;
  RemoveUnits:=nil;
  Units:=TStringList.Create;
  try
    if not CodeToolBoss.FindUnusedUnits(Code,Units) then begin
      DebugLn(['ShowUnusedUnitsDialog CodeToolBoss.FindUnusedUnits failed']);
      LazarusIDE.DoJumpToCodeToolBossError;
      exit(mrCancel);
    end;

    UnusedUnitsDialog:=TUnusedUnitsDialog.Create(nil);
    UnusedUnitsDialog.Units:=Units;
    if UnusedUnitsDialog.ShowModal=mrOk then begin
      RemoveUnits:=UnusedUnitsDialog.GetSelectedUnits;
      if RemoveUnits.Count>0 then begin
        for i:=0 to RemoveUnits.Count-1 do begin
          if not CodeToolBoss.RemoveUnitFromAllUsesSections(Code,RemoveUnits[i])
          then begin
            LazarusIDE.DoJumpToCodeToolBossError;
            exit(mrCancel);
          end;
        end;
      end;
    end;
  finally
    CodeToolBoss.SourceCache.ClearAllSourceLogEntries;
    RemoveUnits.Free;
    UnusedUnitsDialog.Free;
    Units.Free;
  end;
end;

{ TUnusedUnitsDialog }

procedure TUnusedUnitsDialog.FormCreate(Sender: TObject);
begin
  Caption:='Unused units';

  ButtonPanel1.OKButton.Caption:='Remove selected units';
  ButtonPanel1.OKButton.OnClick:=@OkClick;
  ButtonPanel1.CancelButton.Caption:='Cancel';

  UnitsTreeView.StateImages := IDEImages.Images_16;
  ImgIDInterface := IDEImages.LoadImage(16, 'ce_interface');
  ImgIDImplementation := IDEImages.LoadImage(16, 'ce_implementation');
  ImgIDInitialization := IDEImages.LoadImage(16, 'ce_initialization');
  ImgIDNone := IDEImages.LoadImage(16, 'ce_default');
end;

procedure TUnusedUnitsDialog.OkClick(Sender: TObject);
begin

end;

procedure TUnusedUnitsDialog.SetUnits(const AValue: TStrings);
begin
  if FUnits=AValue then exit;
  FUnits:=AValue;
  RebuildUnitsTreeView;
end;

procedure TUnusedUnitsDialog.RebuildUnitsTreeView;
var
  i: Integer;
  Unitname: string;
  Flags: string;
  UseInterface: Boolean;
  InImplUsesSection: Boolean;
  UseCode: Boolean;
  IntfTreeNode: TTreeNode;
  ImplTreeNode: TTreeNode;
  ParentNode: TTreeNode;
  TVNode: TTreeNode;
begin
  UnitsTreeView.BeginUpdate;
  UnitsTreeView.Items.Clear;
  IntfTreeNode:=UnitsTreeView.Items.Add(nil,'Interface');
  IntfTreeNode.StateIndex:=ImgIDInterface;
  ImplTreeNode:=UnitsTreeView.Items.Add(nil,'Implementation');
  ImplTreeNode.StateIndex:=ImgIDImplementation;
  if Units<>nil then
  begin
    for i:=0 to Units.Count-1 do
    begin
      Unitname:=Units.Names[i];
      Flags:=Units.ValueFromIndex[i];
      InImplUsesSection:=System.Pos(',implementation',Flags)>0;
      UseInterface:=System.Pos(',used',Flags)>0;
      UseCode:=System.Pos(',code',Flags)>0;
      if not UseInterface then begin
        if InImplUsesSection then
          ParentNode:=ImplTreeNode
        else
          ParentNode:=IntfTreeNode;
        TVNode:=UnitsTreeView.Items.AddChild(ParentNode,Unitname);
        if UseCode then
          TVNode.StateIndex:=ImgIDInitialization
        else
          TVNode.StateIndex:=ImgIDNone;
      end;
    end;
  end;
  IntfTreeNode.Expanded:=true;
  ImplTreeNode.Expanded:=true;
  UnitsTreeView.EndUpdate;
end;

function TUnusedUnitsDialog.GetSelectedUnits: TStrings;
var
  TVNode: TTreeNode;
begin
  Result:=TStringList.Create;
  TVNode:=UnitsTreeView.Items.GetFirstNode;
  while TVNode<>nil do begin
    if TVNode.MultiSelected and (TVNode.Level=1) then
      Result.Add(TVNode.Text);
    TVNode:=TVNode.GetNext;
  end;
end;

initialization
  {$I unusedunitsdlg.lrs}

end.

