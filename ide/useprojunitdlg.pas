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
  All available units added to the list by Anton
}
unit UseProjUnitDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, StdCtrls, ExtCtrls, Buttons,
  ButtonPanel, Dialogs, LCLProc, FileProcs, Graphics, LCLType, EditBtn, StrUtils,
  SourceEditor, LazIDEIntf, IDEImagesIntf, LazarusIDEStrConsts, ProjectIntf,
  Project, CodeCache, CodeToolManager, IdentCompletionTool, ListFilterEdit;

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
    procedure UnitsListBoxDblClick(Sender: TObject);
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
    procedure EnableOnlyInterface;
    function ChooseImageIndex(Str: String; Data: TObject; var IsEnabled: Boolean): Integer;
  public

  end; 

function ShowUseUnitDialog: TModalResult;

implementation

{$R *.lfm}

function ShowUseUnitDialog: TModalResult;
var
  UseProjUnitDlg: TUseUnitDialog;
  SrcEdit: TSourceEditor;
  s: String;
  CTRes: Boolean;
begin
  Result:=mrOk;
  if not LazarusIDE.BeginCodeTools then exit;
  // get cursor position
  SrcEdit:=SourceEditorManager.ActiveEditor;
  UseProjUnitDlg:=TUseUnitDialog.Create(nil);
  try
    Result:=UseProjUnitDlg.GetAvailableProjUnits(SrcEdit);
    if Result<>mrOK then exit;
    // there is only main uses section in program/library/package
    if SrcEdit.GetProjectFile=Project1.MainUnitInfo then
      UseProjUnitDlg.EnableOnlyInterface;
    // Show the dialog.
    if UseProjUnitDlg.ShowModal=mrOk then begin
      s:=UseProjUnitDlg.SelectedUnit;
      if s <> '' then begin
        if UseProjUnitDlg.InterfaceSelected then
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
    UseProjUnitDlg.Free;
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
  SectionRadioGroup.ItemIndex:=1;
  ButtonPanel1.OKButton.Caption:=lisOk;
  ButtonPanel1.CancelButton.Caption:=dlgCancel;
  UnitImgInd := IDEImages.LoadImage(16, 'item_unit');
  FilterEdit.Images4Listbox:=IDEImages.Images_16;
  FilterEdit.OnGetImageIndex:=@ChooseImageIndex;
  FProjUnits:=TStringList.Create;
end;

procedure TUseUnitDialog.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  FOtherUnits.Free;
  FProjUnits.Free;
  FImplUsedUnits.Free;
  FMainUsedUnits.Free;
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

function TUseUnitDialog.ChooseImageIndex(Str: String; Data: TObject;
                                         var IsEnabled: Boolean): Integer;
begin
  IsEnabled:=Data=Nil;
  Result:=UnitImgInd;
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
              FOtherUnits.AddObject(IdentifierList.FilteredItems[i].Identifier, TObject(1));
        end;
      end;
    FOtherUnits.Sort;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

function TUseUnitDialog.SelectedUnit: string;
begin
  Result:=UnitsListBox.Items[UnitsListBox.ItemIndex];
end;

function TUseUnitDialog.InterfaceSelected: Boolean;
begin
  Result:=SectionRadioGroup.ItemIndex=0;
end;

procedure TUseUnitDialog.EnableOnlyInterface;
begin
  SectionRadioGroup.ItemIndex := 0;
  SectionRadioGroup.Enabled := False;
end;

end.

