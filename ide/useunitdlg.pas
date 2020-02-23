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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
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
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Buttons, ButtonPanel,
  Dialogs, LCLProc, FileProcs, Graphics, LCLType, SourceEditor, LazIDEIntf,
  IDEImagesIntf, LazarusIDEStrConsts, ProjectIntf, IDEWindowIntf, Project,
  CodeCache, CodeToolManager, IdentCompletionTool, CodeTree, ListFilterEdit,
  LinkScanner, EnvironmentOpts, MainIntf, LazFileUtils;

type

  TUseUnitDialogType = (udUseUnit, udOpenUnit);

  { TUseUnitDialog }

  TUseUnitDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    AllUnitsCheckBox: TCheckBox;
    FilterEdit: TListFilterEdit;
    UnitsListBox: TListBox;
    SectionRadioGroup: TRadioGroup;
    procedure AllUnitsCheckBoxChange(Sender: TObject);
    procedure FilterEditAfterFilter(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SectionRadioGroupClick(Sender: TObject);
    procedure UnitsListBoxDblClick(Sender: TObject);
    procedure UnitsListBoxDrawItem({%H-}Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure UnitsListBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure UnitsListBoxMeasureItem({%H-}Control: TWinControl; {%H-}Index: Integer;
      var AHeight: Integer);
  private
    UnitImgInd: Integer;
    FMainUsedUnits: TStringList;
    FImplUsedUnits: TStringList;
    FProjUnits, FOtherUnits: TStringList;
    DlgType: TUseUnitDialogType;
    procedure AddImplUsedUnits;
    function GetProjUnits(SrcEdit: TSourceEditor): Boolean;
    procedure CreateOtherUnitsList;
    function SelectedUnitFileName: string;
    function SelectedUnit: string;
    function InterfaceSelected: Boolean;
    procedure DetermineUsesSection(ACode: TCodeBuffer);
    procedure FillAvailableUnitsList;
  public

  end; 

function ShowUseUnitDialog(const DefText: string; const aDlgType: TUseUnitDialogType): TModalResult;

implementation

{$R *.lfm}

function ShowUseUnitDialog(const DefText: string; const aDlgType: TUseUnitDialogType): TModalResult;
var
  UseUnitDlg: TUseUnitDialog;
  SrcEdit: TSourceEditor;
  s: String;
  CTRes: Boolean;
  EnvOptions: TUseUnitDlgOptions;
begin
  Result:=mrOk;
  if not LazarusIDE.BeginCodeTools then begin
    debugln(['ShowUseUnitDialog LazarusIDE.BeginCodeTools failed']);
    exit;
  end;
  // get cursor position
  SrcEdit:=SourceEditorManager.ActiveEditor;
  if SrcEdit=nil then begin
    debugln(['ShowUseUnitDialog no SrcEdit']);
    exit;
  end;
  UseUnitDlg:=TUseUnitDialog.Create(nil);
  try
    UseUnitDlg.DlgType := aDlgType;
    case aDlgType of
      udUseUnit: UseUnitDlg.Caption := dlgUseUnitCaption;
      udOpenUnit: UseUnitDlg.Caption := lisOpenUnit;
    end;

    if not UseUnitDlg.GetProjUnits(SrcEdit) then begin
      debugln(['ShowUseUnitDialog UseUnitDlg.GetProjUnits(SrcEdit) failed: ',SrcEdit.FileName]);
      Exit(mrCancel);
    end;
    UseUnitDlg.FillAvailableUnitsList;
    // there is only main uses section in program/library/package
    if SrcEdit.GetProjectFile=Project1.MainUnitInfo then
      // only main (interface) section is available
      UseUnitDlg.SectionRadioGroup.Enabled := False
    else
      // automatic choice of dest uses-section by cursor position
      UseUnitDlg.DetermineUsesSection(SrcEdit.CodeBuffer);

    // Read recent properties
    EnvOptions := EnvironmentOptions.UseUnitDlgOptions;
    UseUnitDlg.AllUnitsCheckBox.Checked := EnvOptions.AllUnits;
    UseUnitDlg.SectionRadioGroup.ItemIndex := Ord(EnvOptions.AddToImplementation);
    UseUnitDlg.SectionRadioGroup.Visible := aDlgType=udUseUnit;

    if (UseUnitDlg.FilterEdit.Items.Count = 0)
    and UseUnitDlg.AllUnitsCheckBox.Checked then begin
      // No available units. This may not be a pascal source file.
      ShowMessage(dlgNoAvailableUnits);
      Exit(mrCancel);
    end;

    UseUnitDlg.FilterEdit.Text := DefText;

    // Show the dialog.
    if UseUnitDlg.ShowModal=mrOk then begin

      // Write recent properties
      EnvOptions.AllUnits := UseUnitDlg.AllUnitsCheckBox.Checked;
      if aDlgType=udUseUnit then
        EnvOptions.AddToImplementation := Boolean(UseUnitDlg.SectionRadioGroup.ItemIndex);
      EnvironmentOptions.UseUnitDlgOptions := EnvOptions;

      case aDlgType of
        udUseUnit:
        begin
          s:=UseUnitDlg.SelectedUnit;
          if s <> '' then begin
            if UseUnitDlg.InterfaceSelected then
              CTRes := CodeToolBoss.AddUnitToMainUsesSection(SrcEdit.CodeBuffer, s, '')
            else
              CTRes:=CodeToolBoss.AddUnitToImplementationUsesSection(SrcEdit.CodeBuffer, s, '');
            if not CTRes then begin
              LazarusIDE.DoJumpToCodeToolBossError;
              exit(mrCancel);
            end;
          end;
        end;
        udOpenUnit:
        begin
          s:=UseUnitDlg.SelectedUnitFileName;
          if FileExistsUTF8(s) then
            Result := MainIDEInterface.DoOpenEditorFile(s,-1,-1,[ofAddToRecent])
          else
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
  IDEDialogLayoutList.ApplyLayout(Self, 500, 460);
  AllUnitsCheckBox.Caption := dlgShowAllUnits;
  SectionRadioGroup.Caption := dlgInsertSection;
  SectionRadioGroup.Items.Clear;
  SectionRadioGroup.Items.Add(dlgInsertInterface);
  SectionRadioGroup.Items.Add(dlgInsertImplementation);
  ButtonPanel1.OKButton.Caption:=lisMenuOk;
  ButtonPanel1.CancelButton.Caption:=lisCancel;
  UnitImgInd := IDEImages.LoadImage('item_unit');
  FProjUnits:=TStringList.Create;
end;

procedure TUseUnitDialog.FormDestroy(Sender: TObject);
begin
  FOtherUnits.Free;
  FProjUnits.Free;
  FImplUsedUnits.Free;
  FMainUsedUnits.Free;
end;

procedure TUseUnitDialog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TUseUnitDialog.SectionRadioGroupClick(Sender: TObject);
var
  i: Integer;
begin
  if not Assigned(FImplUsedUnits) then Exit;
  if InterfaceSelected then
    AddImplUsedUnits
  else
    for i := FilterEdit.Items.Count - 1 downto 0 do
      if FilterEdit.Items.Objects[i] is TCodeTreeNode then
        FilterEdit.Items.Delete(i);
  FilterEdit.InvalidateFilter;
  if Visible then
    FilterEdit.SetFocus;
end;

procedure TUseUnitDialog.AllUnitsCheckBoxChange(Sender: TObject);
var
  i: Integer;
begin
  if not (Assigned(FMainUsedUnits) and Assigned(FImplUsedUnits)) then Exit;
  if AllUnitsCheckBox.Checked then begin    // Add other units
    if not Assigned(FOtherUnits) then
      CreateOtherUnitsList;
    FilterEdit.Items.AddStrings(FOtherUnits);
  end
  else
    for i := FilterEdit.Items.Count-1 downto 0 do
      if FilterEdit.Items.Objects[i] is TIdentifierListItem then
        FilterEdit.Items.Delete(i);
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
var
  ena: Boolean;
begin
  if Index < 0 then Exit;
  with UnitsListBox do
  begin
    Canvas.FillRect(ARect);
    ena := not Assigned(Items.Objects[Index]) or (Items.Objects[Index] is TCodeTreeNode);
    if not (ena or (odSelected in State)) then
      Canvas.Font.Color := clGreen;
    IDEImages.Images_16.Draw(Canvas, 1, (ARect.Top+ARect.Bottom-IDEImages.Images_16.Height) div 2, UnitImgInd, ena);
    if Items.Objects[Index] is TCodeTreeNode then
    begin
      // unit for moving: implementation->interface
      Canvas.Pen.Color := clBlue;
      Canvas.Pen.Width := 2;
      Canvas.MoveTo(ARect.Left + 13, ARect.Top + 16);
      Canvas.LineTo(ARect.Left + 13, ARect.Top + 8);
      Canvas.LineTo(ARect.Left + 10, ARect.Top + 11);
      Canvas.MoveTo(ARect.Left + 13, ARect.Top + 8);
      Canvas.LineTo(ARect.Left + 15, ARect.Top + 11);
    end;
    Canvas.TextRect(ARect, ARect.Left + IDEImages.Images_16.Width + 4, ARect.Top, Items[Index]);
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

procedure TUseUnitDialog.UnitsListBoxMeasureItem(Control: TWinControl;
  Index: Integer; var AHeight: Integer);
begin
  if (AHeight <= IDEImages.Images_16.Height) then
    AHeight := IDEImages.Images_16.Height + 2;
end;

procedure TUseUnitDialog.AddImplUsedUnits;
var
  i, j: Integer;
  newUnit: string;
  ImplNode: TObject;
begin
  if FImplUsedUnits.Count = 0 then Exit;
  i := 0; j := 0;
  ImplNode := FImplUsedUnits.Objects[0];
  newUnit := FImplUsedUnits[j];
  with FilterEdit.Items do
  begin
    BeginUpdate;
    try
      while i <= Count - 1 do
      begin
        if Assigned(Objects[i]) then Break;
        if CompareStr(FImplUsedUnits[j], Strings[i]) <= 0 then
        begin
          InsertObject(i, newUnit, ImplNode);
          Inc(j);
          if j >= FImplUsedUnits.Count then Exit;
          newUnit := FImplUsedUnits[j];
        end;
        Inc(i);
      end;
      if j < FImplUsedUnits.Count then
        for j := j to FImplUsedUnits.Count - 1 do
          if i < Count then
            InsertObject(i, FImplUsedUnits[j], ImplNode)
          else
            AddObject(FImplUsedUnits[j], ImplNode);
    finally
      EndUpdate;
    end;
  end;
end;

function TUseUnitDialog.GetProjUnits(SrcEdit: TSourceEditor): Boolean;
var
  ProjFile: TUnitInfo;
  CurrentUnitName, s: String;
  x: Integer;
begin
  Result := False;
  FreeAndNil(FMainUsedUnits);
  FreeAndNil(FImplUsedUnits);
  if SrcEdit = nil then Exit;
  Assert(Assigned(SrcEdit.CodeBuffer));
  if DlgType=udUseUnit then
  begin
    if not CodeToolBoss.FindUsedUnitNames(SrcEdit.CodeBuffer, TStrings(FMainUsedUnits),
                                                              TStrings(FImplUsedUnits))
    then begin
      DebugLn(['ShowUseProjUnitDialog CodeToolBoss.FindUsedUnitNames failed']);
      LazarusIDE.DoJumpToCodeToolBossError;
      Exit;
    end;
  end else
  begin
    // don't filter units in current uses sections - use empty lists
    FMainUsedUnits := TStringList.Create;
    FImplUsedUnits := TStringList.Create;
  end;
  Result := True;
  if Assigned(FMainUsedUnits) then begin
    FMainUsedUnits.Sorted := True;
    FMainUsedUnits.CaseSensitive := False;
  end;
  if Assigned(FImplUsedUnits) then begin
    FImplUsedUnits.Sorted := True;
    FImplUsedUnits.CaseSensitive := False;
  end;
  if SrcEdit.GetProjectFile is TUnitInfo then
    CurrentUnitName := TUnitInfo(SrcEdit.GetProjectFile).Unit_Name
  else
    CurrentUnitName := '';
  // Add available unit names to list
  ProjFile:=Project1.FirstPartOfProject;
  while ProjFile <> nil do begin
    s := ProjFile.Unit_Name;
    if s = CurrentUnitName then       // current unit
      s := '';
    if (ProjFile <> Project1.MainUnitInfo) and (s <> '') then
      if not FMainUsedUnits.Find(s, x) then
        FProjUnits.AddObject(s, ProjFile);
    ProjFile := ProjFile.NextPartOfProject;
  end;
  FProjUnits.Sorted := True;
end;

procedure TUseUnitDialog.CreateOtherUnitsList;
var
  i, x: Integer;
  curUnit: string;
  SrcEdit: TSourceEditor;
begin
  if not (Assigned(FMainUsedUnits) and Assigned(FImplUsedUnits)) then Exit;
  Screen.BeginWaitCursor;
  try
    FOtherUnits := TStringList.Create;
    FOtherUnits.Sorted := True;
    SrcEdit := SourceEditorManager.ActiveEditor;
    with CodeToolBoss do
      if GatherUnitNames(SrcEdit.CodeBuffer) then
      begin
        IdentifierList.Prefix := '';
        for i := 0 to IdentifierList.GetFilteredCount - 1 do
        begin
          curUnit := IdentifierList.FilteredItems[i].Identifier;
          if  not FMainUsedUnits.Find(curUnit, x)
          and not FImplUsedUnits.Find(curUnit, x)
          and not FProjUnits.Find(curUnit, x) then
            FOtherUnits.AddObject(IdentifierList.FilteredItems[i].Identifier,
                                  IdentifierList.FilteredItems[i]);
        end;
      end;
  finally
    Screen.EndWaitCursor;
  end;
end;

function TUseUnitDialog.SelectedUnit: string;
var
  IdentItem: TIdentifierListItem;
  CodeBuf: TCodeBuffer;
  s: String;
begin
  with UnitsListBox do
    if ItemIndex >= 0 then
    begin
      if Items.Objects[ItemIndex] is TIdentifierListItem then
      begin
        IdentItem := TIdentifierListItem(Items.Objects[ItemIndex]);
        Result := IdentItem.Identifier;
        with CodeToolBoss.SourceChangeCache.BeautifyCodeOptions do
          if WordExceptions.CheckExceptions(Result) then Exit;
        CodeBuf := CodeToolBoss.FindUnitSource(SourceEditorManager.ActiveEditor.CodeBuffer, Result, '');
        if Assigned(CodeBuf) then
        begin
          s := CodeToolBoss.GetSourceName(CodeBuf, True);
          if s <> '' then
            Result := s;
        end;
      end else
        Result := Items[ItemIndex];
    end else
      Result := '';
end;

function TUseUnitDialog.SelectedUnitFileName: string;
var
  CodeBuf: TCodeBuffer;
  AObj: TObject;
begin
  Result := '';
  if UnitsListBox.ItemIndex < 0 then
    Exit;
  AObj := UnitsListBox.Items.Objects[UnitsListBox.ItemIndex];
  if AObj is TIdentifierListItem then
  begin
    CodeBuf := CodeToolBoss.FindUnitSource(SourceEditorManager.ActiveEditor.CodeBuffer, TIdentifierListItem(AObj).Identifier, '');
    if Assigned(CodeBuf) then
      Result := CodeBuf.Filename;
  end else
  if AObj is TUnitInfo then
  begin
    Result := TUnitInfo(AObj).Filename;
  end;
end;

function TUseUnitDialog.InterfaceSelected: Boolean;
begin
  Result:=(not SectionRadioGroup.Enabled) or (SectionRadioGroup.ItemIndex=0);
end;

procedure TUseUnitDialog.DetermineUsesSection(ACode: TCodeBuffer);
var
  ImplUsesNode: TCodeTreeNode;
  i: Integer;
  Tool: TCodeTool;
begin
  CodeToolBoss.Explore(ACode,Tool,false);
  if Tool=nil then exit;
  // collect implementation use unit nodes
  ImplUsesNode := Tool.FindImplementationUsesNode;
  if Assigned(ImplUsesNode) then
    for i := 0 to FImplUsedUnits.Count - 1 do
      FImplUsedUnits.Objects[i] := ImplUsesNode;
  // update
  SectionRadioGroup.OnClick(SectionRadioGroup);
end;

procedure TUseUnitDialog.FillAvailableUnitsList;
var
  curUnit: String;
  i, x: Integer;
begin
  if not (Assigned(FMainUsedUnits) and Assigned(FImplUsedUnits)) then Exit;
  if not Assigned(FProjUnits) then Exit;
  FilterEdit.Items.Clear;
  for i := 0 to FProjUnits.Count - 1 do
  begin
    curUnit := FProjUnits[i];
    if  not FMainUsedUnits.Find(curUnit, x)
    and not FImplUsedUnits.Find(curUnit, x) then
      FilterEdit.Items.AddObject(FProjUnits[i], FProjUnits.Objects[i]);
  end;
  FilterEdit.InvalidateFilter;
end;

procedure TUseUnitDialog.FilterEditAfterFilter(Sender: TObject);
begin
  if (UnitsListBox.Count > 0) and (UnitsListBox.ItemIndex = -1) then
    UnitsListBox.ItemIndex := 0;
end;

end.

