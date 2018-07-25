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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
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
  Classes, SysUtils,
  // LCL
  LCLProc, Forms, Controls, ComCtrls, StdCtrls, ExtCtrls, Buttons, Dialogs,
  // Codetools
  CodeCache, CodeToolManager,
  // IdeIntf
  SrcEditorIntf, LazIDEIntf, IDEImagesIntf, IDEDialogs,
  // IDE
  LazarusIDEStrConsts;

type

  { TUnusedUnitsDialog }

  TUnusedUnitsDialog = class(TForm)
    CancelBitBtn: TBitBtn;
    ShowInitializationCheckBox: TCheckBox;
    RemoveAllBitBtn: TBitBtn;
    RemoveSelectedBitBtn: TBitBtn;
    Panel1: TPanel;
    UnitsTreeView: TTreeView;
    procedure CancelBitBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RemoveAllBitBtnClick(Sender: TObject);
    procedure RemoveSelectedBitBtnClick(Sender: TObject);
    procedure ShowInitializationCheckBoxClick(Sender: TObject);
    procedure UnitsTreeViewSelectionChanged(Sender: TObject);
  private
    FCode: TCodeBuffer;
    FUnits: TStrings;
    ImgIDInterface: LongInt;
    ImgIDImplementation: LongInt;
    ImgIDInitialization: LongInt;
    ImgIDNone: LongInt;
    procedure SetCode(AValue: TCodeBuffer);
    procedure SetUnits(const AValue: TStrings);
    procedure RebuildUnitsTreeView;
    procedure UpdateButtons;
  public
    function GetSelectedUnits: TStrings;
    function GetAllUnits: TStrings;
    property Units: TStrings read FUnits write SetUnits;
    property Code: TCodeBuffer read FCode write SetCode;
  end;


function ShowUnusedUnitsDialog: TModalResult;
function ShowUnusedUnitsDialog(Code: TCodeBuffer): TModalResult;

implementation

{$R *.lfm}

function ShowUnusedUnitsDialog: TModalResult;
var
  SrcEdit: TSourceEditorInterface;
  Code: TCodeBuffer;
begin
  // get cursor position
  Result:=mrAbort;
  SrcEdit:=SourceEditorManagerIntf.ActiveEditor;
  if SrcEdit=nil then exit;
  Code:=TCodeBuffer(SrcEdit.CodeToolsBuffer);
  if Code=nil then exit;
  Result:=ShowUnusedUnitsDialog(Code);
end;

function ShowUnusedUnitsDialog(Code: TCodeBuffer): TModalResult;
var
  UnusedUnitsDialog: TUnusedUnitsDialog;
  Units: TStringList;
  RemoveUnits: TStrings;
  i: Integer;
  DlgResult: TModalResult;
  SrcEdit: TSourceEditorInterface;
begin
  Result:=mrOk;
  if Code=nil then exit;
  if not LazarusIDE.BeginCodeTools then exit;

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
    UnusedUnitsDialog.Code:=Code;
    DlgResult:=UnusedUnitsDialog.ShowModal;
    if DlgResult=mrOk then
      RemoveUnits:=UnusedUnitsDialog.GetSelectedUnits
    else if DlgResult=mrAll then
      RemoveUnits:=UnusedUnitsDialog.GetAllUnits
    else
      RemoveUnits:=nil;
    if (RemoveUnits<>nil) and (RemoveUnits.Count>0) then begin
      LazarusIDE.DoOpenEditorFile(Code.Filename,-1,-1,[]);
      SrcEdit:=SourceEditorManagerIntf.SourceEditorIntfWithFilename(Code.Filename);
      if SrcEdit=nil then begin
        IDEMessageDialog(lisCCOErrorCaption,
          Format(lisUnableToOpen, [Code.Filename]),
          mtError,[mbCancel]);
        exit(mrCancel);
      end;

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
  ShowInitializationCheckBox.Caption:=lisShowUnitsWithInitialization;
  ShowInitializationCheckBox.Hint:=lisShowUnitsWithInitializationHint;
  RemoveSelectedBitBtn.Caption:=lisRemoveSelectedUnits;
  RemoveAllBitBtn.Caption:=lisRemoveAllUnits;
  CancelBitBtn.Caption:=lisCancel;

  UnitsTreeView.StateImages := IDEImages.Images_16;
  ImgIDInterface := IDEImages.LoadImage('ce_interface');
  ImgIDImplementation := IDEImages.LoadImage('ce_implementation');
  ImgIDInitialization := IDEImages.LoadImage('ce_initialization');
  ImgIDNone := IDEImages.LoadImage('ce_default');
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

procedure TUnusedUnitsDialog.ShowInitializationCheckBoxClick(Sender: TObject);
begin
  RebuildUnitsTreeView;
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

procedure TUnusedUnitsDialog.SetCode(AValue: TCodeBuffer);
begin
  if FCode=AValue then Exit;
  FCode:=AValue;
  if FCode<>nil then
    Caption:=Format(lisUnusedUnitsOf, [ExtractFilename(Code.Filename)]);
end;

procedure TUnusedUnitsDialog.RebuildUnitsTreeView;
var
  i: Integer;
  AUnitname: string;
  Flags: string;
  UseInterface: Boolean;
  InImplUsesSection: Boolean;
  UseCode, HideCode: Boolean;
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
      HideCode:=not ShowInitializationCheckBox.Checked;
      if not (UseInterface or (HideCode and UseCode)) then
      begin
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

