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
  Classes, LCLProc, Forms, Controls, ComCtrls, StdCtrls, ExtCtrls, Buttons,
  SrcEditorIntf, LazIDEIntf, IDEImagesIntf,
  CodeCache, CodeToolManager,
  LazarusIDEStrConsts;

type

  { TUnusedUnitsDialog }

  TUnusedUnitsDialog = class(TForm)
    CancelBitBtn: TBitBtn;
    RemoveAllBitBtn: TBitBtn;
    RemoveSelectedBitBtn: TBitBtn;
    Panel1: TPanel;
    UnitsTreeView: TTreeView;
    procedure CancelBitBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RemoveAllBitBtnClick(Sender: TObject);
    procedure RemoveSelectedBitBtnClick(Sender: TObject);
    procedure UnitsTreeViewSelectionChanged(Sender: TObject);
  private
    FUnits: TStrings;
    ImgIDInterface: LongInt;
    ImgIDImplementation: LongInt;
    ImgIDInitialization: LongInt;
    ImgIDNone: LongInt;
    procedure SetUnits(const AValue: TStrings);
    procedure RebuildUnitsTreeView;
    procedure UpdateButtons;
  public
    function GetSelectedUnits: TStrings;
    function GetAllUnits: TStrings;
    property Units: TStrings read FUnits write SetUnits;
  end;


function ShowUnusedUnitsDialog: TModalResult;

implementation

{$R *.lfm}

function ShowUnusedUnitsDialog: TModalResult;
var
  UnusedUnitsDialog: TUnusedUnitsDialog;
  SrcEdit: TSourceEditorInterface;
  Code: TCodeBuffer;
  Units: TStringList;
  RemoveUnits: TStrings;
  i: Integer;
  DlgResult: TModalResult;
begin
  Result:=mrOk;
  if not LazarusIDE.BeginCodeTools then exit;

  // get cursor position
  SrcEdit:=SourceEditorManagerIntf.ActiveEditor;
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
    Units.Sort;

    UnusedUnitsDialog:=TUnusedUnitsDialog.Create(nil);
    UnusedUnitsDialog.Units:=Units;
    DlgResult:=UnusedUnitsDialog.ShowModal;
    if DlgResult=mrOk then
      RemoveUnits:=UnusedUnitsDialog.GetSelectedUnits
    else if DlgResult=mrAll then
      RemoveUnits:=UnusedUnitsDialog.GetAllUnits
    else
      RemoveUnits:=nil;
    if (RemoveUnits<>nil) and (RemoveUnits.Count>0) then begin
      SrcEdit.BeginUndoBlock{$IFDEF SynUndoDebugBeginEnd}('ShowUnusedUnitsDialog'){$ENDIF};
      try
        for i:=0 to RemoveUnits.Count-1 do begin
          if not CodeToolBoss.RemoveUnitFromAllUsesSections(Code,RemoveUnits[i])
          then begin
            LazarusIDE.DoJumpToCodeToolBossError;
            exit(mrCancel);
          end;
        end;
      finally
        SrcEdit.EndUndoBlock{$IFDEF SynUndoDebugBeginEnd}('ShowUnusedUnitsDialog'){$ENDIF};
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
  Caption:=lisUnusedUnits;

  RemoveSelectedBitBtn.Caption:=lisRemoveSelectedUnits;
  RemoveAllBitBtn.Caption:=lisRemoveAllUnits;
  CancelBitBtn.Caption:=dlgCancel;

  UnitsTreeView.StateImages := IDEImages.Images_16;
  ImgIDInterface := IDEImages.LoadImage(16, 'ce_interface');
  ImgIDImplementation := IDEImages.LoadImage(16, 'ce_implementation');
  ImgIDInitialization := IDEImages.LoadImage(16, 'ce_initialization');
  ImgIDNone := IDEImages.LoadImage(16, 'ce_default');
end;

procedure TUnusedUnitsDialog.RemoveAllBitBtnClick(Sender: TObject);
begin
  ModalResult:=mrAll;
end;

procedure TUnusedUnitsDialog.CancelBitBtnClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TUnusedUnitsDialog.RemoveSelectedBitBtnClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

procedure TUnusedUnitsDialog.UnitsTreeViewSelectionChanged(Sender: TObject);
begin
  UpdateButtons;
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
  AUnitname: string;
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
      AUnitname:=Units.Names[i];
      Flags:=Units.ValueFromIndex[i];
      InImplUsesSection:=System.Pos(',implementation',Flags)>0;
      UseInterface:=System.Pos(',used',Flags)>0;
      UseCode:=System.Pos(',code',Flags)>0;
      if not UseInterface then begin
        if InImplUsesSection then
          ParentNode:=ImplTreeNode
        else
          ParentNode:=IntfTreeNode;
        TVNode:=UnitsTreeView.Items.AddChild(ParentNode,AUnitname);
        if UseCode then
          TVNode.StateIndex:=ImgIDInitialization
        else
          TVNode.StateIndex:=ImgIDInterface;
      end;
    end;
  end;
  IntfTreeNode.Expanded:=true;
  ImplTreeNode.Expanded:=true;
  UnitsTreeView.EndUpdate;
  UpdateButtons;
end;

procedure TUnusedUnitsDialog.UpdateButtons;
var
  RemoveUnits: TStrings;
begin
  RemoveUnits:=GetSelectedUnits;
  RemoveSelectedBitBtn.Enabled:=RemoveUnits.Count>0;
  RemoveAllBitBtn.Enabled:=Units.Count>0;
  RemoveUnits.Free;
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

function TUnusedUnitsDialog.GetAllUnits: TStrings;
var
  TVNode: TTreeNode;
begin
  Result:=TStringList.Create;
  TVNode:=UnitsTreeView.Items.GetFirstNode;
  while TVNode<>nil do begin
    if (TVNode.Level=1) then
      Result.Add(TVNode.Text);
    TVNode:=TVNode.GetNext;
  end;
end;

end.

