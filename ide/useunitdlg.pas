{ Copyright (C) 2011,

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

  Original version by Juha Manninen
  Icons added by Marcelo B Paula
  All available units added to the list by Anton Panferov
}
unit UseUnitDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, StdCtrls, ExtCtrls, Buttons,
  ButtonPanel, Dialogs, LCLProc, FileProcs, Graphics, LCLType, EditBtn, StrUtils,
  SourceEditor, LazIDEIntf, IDEImagesIntf, LazarusIDEStrConsts, ProjectIntf,
  Project, CodeCache, CodeToolManager, IdentCompletionTool, CodeAtom, CodeTree,
  PascalParserTool, ListFilterEdit, LinkScanner;

type

  { TUseUnitDialog }

  TUseUnitDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    AllUnitsCheckBox: TCheckBox;
    FilterEdit: TListFilterEdit;
    UnitsListBox: TListBox;
    SectionRadioGroup: TRadioGroup;
    procedure AllUnitsCheckBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SectionRadioGroupClick(Sender: TObject);
    procedure UnitsListBoxDblClick(Sender: TObject);
    procedure UnitsListBoxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure UnitsListBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    UnitImgInd: Integer;
    FMainUsedUnits: TStrings;
    FImplUsedUnits: TStrings;
    FProjUnits: TStringList;
    FOtherUnits: TStringList;
    function GetAvailableProjUnits(SrcEdit: TSourceEditor): TModalResult;
    procedure CreateOtherUnitsList;
    function SelectedUnit: string;
    function InterfaceSelected: Boolean;
    procedure DetermineUsesSection(ACode: TCodeBuffer; ACursorPos: TPoint);
  public

  end; 

function ShowUseUnitDialog: TModalResult;

implementation

{$R *.lfm}

function ShowUseUnitDialog: TModalResult;
var
  UseUnitDlg: TUseUnitDialog;
  SrcEdit: TSourceEditor;
  s: String;
  CTRes: Boolean;
begin
  Result:=mrOk;
  if not LazarusIDE.BeginCodeTools then exit;
  // get cursor position
  SrcEdit:=SourceEditorManager.ActiveEditor;
  UseUnitDlg:=TUseUnitDialog.Create(nil);
  try
    Result:=UseUnitDlg.GetAvailableProjUnits(SrcEdit);
    if Result<>mrOK then exit;
    // there is only main uses section in program/library/package
    if SrcEdit.GetProjectFile=Project1.MainUnitInfo then begin
      // only main (interface) section is available
      UseUnitDlg.SectionRadioGroup.Enabled := False
    end else begin
      // automatic choise of dest uses-section by cursor position
      UseUnitDlg.DetermineUsesSection(SrcEdit.CodeBuffer, SrcEdit.GetCursorTextXY);
    end;
    // Show the dialog.
    if UseUnitDlg.ShowModal=mrOk then begin
      s:=UseUnitDlg.SelectedUnit;
      if s <> '' then begin
        if UseUnitDlg.InterfaceSelected then
          CTRes:=CodeToolBoss.AddUnitToMainUsesSection(SrcEdit.CodeBuffer, s, '')
        else
          CTRes:=CodeToolBoss.AddUnitToImplementationUsesSection(SrcEdit.CodeBuffer, s, '');
        if not CTRes then begin
          LazarusIDE.DoJumpToCodeToolBossError;
          exit(mrCancel);
        end;
      end;
    end;
  finally
    UseUnitDlg.Free;
    CodeToolBoss.SourceCache.ClearAllSourceLogEntries;
  end;
end;


{ TUseUnitDialog }

procedure TUseUnitDialog.FormCreate(Sender: TObject);
begin
  // Internationalization
  Caption := dlgUseUnitCaption;
  SectionRadioGroup.Caption := dlgInsertSection;
  SectionRadioGroup.Items.Clear;
  SectionRadioGroup.Items.Add(dlgInsertInterface);
  SectionRadioGroup.Items.Add(dlgInsertImplementation);
  SectionRadioGroup.ItemIndex:=0;
  ButtonPanel1.OKButton.Caption:=lisOk;
  ButtonPanel1.CancelButton.Caption:=dlgCancel;
  UnitImgInd := IDEImages.LoadImage(16, 'item_unit');
  FProjUnits:=TStringList.Create;
end;

procedure TUseUnitDialog.FormDestroy(Sender: TObject);
begin
  FOtherUnits.Free;
  FProjUnits.Free;
  FImplUsedUnits.Free;
  FMainUsedUnits.Free;
end;

procedure TUseUnitDialog.SectionRadioGroupClick(Sender: TObject);
begin
  if Visible then
    FilterEdit.SetFocus;
end;

procedure TUseUnitDialog.AllUnitsCheckBoxChange(Sender: TObject);
var
  i: Integer;
begin
  if AllUnitsCheckBox.Checked then begin    // Add other units
    CreateOtherUnitsList;
    FilterEdit.Data.AddStrings(FOtherUnits);
  end
  else
  with FilterEdit.Data do begin             // Remove other units
    BeginUpdate;
    try
      for i := Count-1 downto 0 do
        if Assigned(Objects[i]) then Delete(i);
    finally
      EndUpdate;
    end;
  end;
  if Visible then
    FilterEdit.SetFocus;
  FilterEdit.InvalidateFilter;
end;

procedure TUseUnitDialog.UnitsListBoxDblClick(Sender: TObject);
begin
  if UnitsListBox.ItemIndex >= 0 then
    ModalResult := mrOK;
end;

procedure TUseUnitDialog.UnitsListBoxDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var ena: Boolean;
begin
  if Index < 0 then Exit;
  with UnitsListBox do
  begin
    Canvas.FillRect(ARect);
    ena := not Assigned(Items.Objects[Index]);
    if not (ena or (odSelected in State)) then
      UnitsListBox.Canvas.Font.Color := clGreen;
    IDEImages.Images_16.Draw(Canvas, 1, ARect.Top, UnitImgInd, ena);
    Canvas.TextRect(ARect, ARect.Left + 20, ARect.Top, Items[Index]);
  end;
end;

procedure TUseUnitDialog.UnitsListBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // A hack to prevent 'O' working as shortcut for OK-button.
  // Should be removed when issue #20599 is resolved.
  if (Key = VK_O) and (Shift = []) then
    Key:=VK_UNKNOWN;
end;

function TUseUnitDialog.GetAvailableProjUnits(SrcEdit: TSourceEditor): TModalResult;
var
  ProjFile: TUnitInfo;
  CurrentUnitName, s: String;
begin
  Result:=mrOk;
  FMainUsedUnits:=nil;
  FImplUsedUnits:=nil;
  if SrcEdit=nil then exit;
  Assert(Assigned(SrcEdit.CodeBuffer));
  if not CodeToolBoss.FindUsedUnitNames(SrcEdit.CodeBuffer,
                                        FMainUsedUnits,FImplUsedUnits)
  then begin
    DebugLn(['ShowUseProjUnitDialog CodeToolBoss.FindUsedUnitNames failed']);
    LazarusIDE.DoJumpToCodeToolBossError;
    exit(mrCancel);
  end;
  TStringList(FMainUsedUnits).CaseSensitive:=False;
  TStringList(FImplUsedUnits).CaseSensitive:=False;
  if SrcEdit.GetProjectFile is TUnitInfo then
    CurrentUnitName:=TUnitInfo(SrcEdit.GetProjectFile).Unit_Name
  else
    CurrentUnitName:='';
  // Add available unit names to list.
  ProjFile:=Project1.FirstPartOfProject;
  while ProjFile<>nil do begin
    s:=ProjFile.Unit_Name;
    if s=CurrentUnitName then       // current unit
      s:='';
    if (ProjFile<>Project1.MainUnitInfo) and (s<>'') then
      if (FMainUsedUnits.IndexOf(s) < 0) and (FImplUsedUnits.IndexOf(s) < 0) then
        FProjUnits.Add(s);
    ProjFile:=ProjFile.NextPartOfProject;
  end;
  FProjUnits.Sorted:=True;
  FilterEdit.Data.Assign(FProjUnits);
  if FilterEdit.Data.Count = 0 then
  begin
    AllUnitsCheckBox.Checked := True;
    if FilterEdit.Data.Count = 0 then Exit(mrCancel);
  end;
  FilterEdit.InvalidateFilter;
end;

procedure TUseUnitDialog.CreateOtherUnitsList;
var
  i: Integer; curUnit: string;
  SrcEdit: TSourceEditor;
begin
  if Assigned(FOtherUnits) then Exit;
  Screen.Cursor:=crHourGlass;
  try
    FOtherUnits := TStringList.Create;
    SrcEdit := SourceEditorManager.ActiveEditor;
    with CodeToolBoss do
      if GatherUnitNames(SrcEdit.CodeBuffer) then
      begin
        IdentifierList.Prefix := '';
        Assert(Assigned(FMainUsedUnits) and Assigned(FImplUsedUnits));
        for i := 0 to IdentifierList.GetFilteredCount - 1 do
        begin
          curUnit := IdentifierList.FilteredItems[i].Identifier;
          if (FMainUsedUnits.IndexOf(curUnit) < 0)
            and (FImplUsedUnits.IndexOf(curUnit) < 0)
            and (FOtherUnits.IndexOf(curUnit) < 0) then
              FOtherUnits.AddObject(IdentifierList.FilteredItems[i].Identifier, IdentifierList.FilteredItems[i]);
        end;
      end;
    FOtherUnits.Sort;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

function TUseUnitDialog.SelectedUnit: string;
var
  IdentItem: TIdentifierListItem;
  CodeBuf: TCodeBuffer;
begin
  with UnitsListBox do
    if ItemIndex >= 0 then
    begin
      IdentItem := TIdentifierListItem(Items.Objects[ItemIndex]);
      if Assigned(IdentItem) then
      begin
        Result := IdentItem.Identifier;
        CodeBuf := CodeToolBoss.FindUnitSource(
          SourceEditorManager.ActiveEditor.CodeBuffer, Result, '');
        if CodeBuf = nil then
          Exit(IdentItem.Identifier);
        Result := CodeToolBoss.GetSourceName(CodeBuf, True);
        if Result = '' then
          Result := IdentItem.Identifier;
      end else
        Result := Items[ItemIndex];
    end else
      Result := '';
end;

function TUseUnitDialog.InterfaceSelected: Boolean;
begin
  Result:=SectionRadioGroup.ItemIndex=0;
end;

procedure TUseUnitDialog.DetermineUsesSection(ACode: TCodeBuffer; ACursorPos: TPoint);
var
  CursorPos: TCodeXYPosition;
  CleanCursorPos: Integer;
  CursorNode: TCodeTreeNode;
begin
  if not CodeToolBoss.InitCurCodeTool(ACode) then Exit;
  with CodeToolBoss.CurCodeTool do
  begin
    CursorPos := CodeXYPosition(ACursorPos.X, ACursorPos.Y, ACode);
    ActivateGlobalWriteLock;
    try
      // build code tree
      BuildTreeAndGetCleanPos(trTillCursor,lsrEnd,CursorPos,CleanCursorPos,
                 [btSetIgnoreErrorPos,btLoadDirtySource,btCursorPosOutAllowed]);
      // find CodeTreeNode at cursor
      if (Tree.Root = nil) or (Tree.Root.StartPos > CleanCursorPos) then Exit;
      CursorNode := BuildSubTreeAndFindDeepestNodeAtPos(CleanCursorPos, True);
      if CursorNode.HasParentOfType(ctnImplementation) then
        SectionRadioGroup.ItemIndex := 1;
    finally
      DeactivateGlobalWriteLock
    end;
  end;
end;

end.

