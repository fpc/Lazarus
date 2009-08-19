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
}
unit editor_general_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, LCLProc, LCLType,
  StdCtrls, SynEdit, Controls, ExtCtrls,
  EditorOptions, LazarusIDEStrConsts, IDEProcs, IDEOptionsIntf, SynBeautifier;

type
  TPreviewEditor = TSynEdit;
  { TEditorGeneralOptionsFrame }

  TEditorGeneralOptionsFrame = class(TAbstractIDEOptionsEditor)
    Bevel1: TBevel;
    Bevel2a: TBevel;
    Bevel2: TBevel;
    Bevel3a: TBevel;
    Bevel3: TBevel;
    Bevel5a: TBevel;
    Bevel5: TBevel;
    Bevel1a: TBevel;
    Bevel6: TBevel;
    Bevel6a: TBevel;
    BlockIndentComboBox: TComboBox;
    BlockIndentTypeComboBox: TComboBox;
    BlockIndentLabel: TLabel;
    AutoIndentCheckBox: TCheckBox;
    BlockIndentTypeLabel: TLabel;
    BlockGroupLabel: TLabel;
    EndKeyJumpsToNearestStartCheckBox: TCheckBox;
    KeepCursorXCheckBox: TCheckBox;
    OverwriteBlockCheckBox: TCheckBox;
    PersistentCursorCheckBox: TCheckBox;
    AlwaysVisibleCursorCheckBox: TCheckBox;
    CursorSkipsSelectionCheckBox: TCheckBox;
    HomeKeyJumpsToNearestStartCheckBox: TCheckBox;
    CursorGroupLabel: TLabel;
    PersistentBlockCheckBox: TCheckBox;
    TabIndentBlocksCheckBox: TCheckBox;
    SmartTabsCheckBox: TCheckBox;
    TabsToSpacesCheckBox: TCheckBox;
    HalfPageScrollCheckBox: TCheckBox;
    ScrollPastEndFileCheckBox: TCheckBox;
    ScrollPastEndLineCheckBox: TCheckBox;
    ScrollByOneLessCheckBox: TCheckBox;
    UndoGroupLabel: TLabel;
    UndoAfterSaveCheckBox: TCheckBox;
    GroupUndoCheckBox: TCheckBox;
    TabWidthsComboBox: TComboBox;
    TabWidthsLabel: TLabel;
    ScrollGroupLabel: TLabel;
    IndentsTabsGroupLabel: TLabel;
    UndoLimitComboBox: TComboBox;
    UndoLimitLabel: TLabel;
    procedure AlwaysVisibleCursorCheckBoxChange(Sender: TObject);
    procedure AutoIndentCheckBoxChange(Sender: TObject);
    procedure ComboboxOnChange(Sender: TObject);
    procedure ComboboxOnKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CursorSkipsSelectionCheckBoxChange(Sender: TObject);
    procedure ComboBoxOnExit(Sender: TObject);
    procedure EndKeyJumpsToNearestStartCheckBoxChange(Sender: TObject);
    procedure GroupUndoCheckBoxChange(Sender: TObject);
    procedure HalfPageScrollCheckBoxChange(Sender: TObject);
    procedure HomeKeyJumpsToNearestStartCheckBoxChange(Sender: TObject);
    procedure KeepCursorXCheckBoxChange(Sender: TObject);
    procedure OverwriteBlockCheckBoxChange(Sender: TObject);
    procedure PersistentBlockCheckBoxChange(Sender: TObject);
    procedure PersistentCursorCheckBoxChange(Sender: TObject);
    procedure ScrollByOneLessCheckBoxChange(Sender: TObject);
    procedure ScrollPastEndFileCheckBoxChange(Sender: TObject);
    procedure ScrollPastEndLineCheckBoxChange(Sender: TObject);
    procedure SmartTabsCheckBoxChange(Sender: TObject);
    procedure TabIndentBlocksCheckBoxChange(Sender: TObject);
    procedure TabsToSpacesCheckBoxChange(Sender: TObject);
  private
    FDefaultBookmarkImages: TImageList;
    function DefaultBookmarkImages: TImageList;
  public
    PreviewEdits: array of TPreviewEditor;
    procedure AddPreviewEdit(AEditor: TPreviewEditor);
    procedure SetPreviewOption(AValue: Boolean; AnOption: TSynEditorOption); overload;
    procedure SetPreviewOption(AValue: Boolean; AnOption: TSynEditorOption2); overload;

    constructor Create(AOwner: TComponent); override;
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{ TEditorGeneralOptionsFrame }

function TEditorGeneralOptionsFrame.GetTitle: String;
begin
  Result := lisMenuInsertGeneral;
end;

procedure TEditorGeneralOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  BlockIndentLabel.Caption := dlgBlockIndent;
  BlockIndentTypeLabel.Caption := dlgBlockIndent;
  BlockIndentTypeComboBox.Items.Add(dlgBlockIndentTypeSpace);
  BlockIndentTypeComboBox.Items.Add(dlgBlockIndentTypeCopy);
  BlockIndentTypeComboBox.Items.Add(dlgBlockIndentTypePos);
  TabWidthsLabel.Caption := dlgTabWidths;

  // undo
  UndoGroupLabel.Caption := dlgUndoGroupOptions;
  UndoAfterSaveCheckBox.Caption := dlgUndoAfterSave;
  GroupUndoCheckBox.Caption := dlgGroupUndo;
  UndoLimitLabel.Caption := dlgUndoLimit;

  // scroll
  ScrollGroupLabel.Caption := dlgScrollGroupOptions;
  HalfPageScrollCheckBox.Caption := dlgHalfPageScroll;
  ScrollByOneLessCheckBox.Caption := dlgScrollByOneLess;
  ScrollPastEndFileCheckBox.Caption := dlgScrollPastEndFile;
  ScrollPastEndLineCheckBox.Caption := dlgScrollPastEndLine;

  // indents, tabs
  IndentsTabsGroupLabel.Caption := dlgIndentsTabsGroupOptions;
  AutoIndentCheckBox.Caption := dlgAutoIndent;
  TabIndentBlocksCheckBox.Caption := dlgTabIndent;
  SmartTabsCheckBox.Caption := dlgSmartTabs;
  TabsToSpacesCheckBox.Caption := dlgTabsToSpaces;

  // caret + key navigation
  CursorGroupLabel.Caption := dlgCursorGroupOptions;
  KeepCursorXCheckBox.Caption := dlgKeepCursorX;
  PersistentCursorCheckBox.Caption := dlgPersistentCursor;
  AlwaysVisibleCursorCheckBox.Caption := dlgAlwaysVisibleCursor;
  CursorSkipsSelectionCheckBox.Caption := dlgCursorSkipsSelection;
  HomeKeyJumpsToNearestStartCheckBox.Caption := dlgHomeKeyJumpsToNearestStart;
  EndKeyJumpsToNearestStartCheckBox.Caption := dlgEndKeyJumpsToNearestStart;

  // Block
  BlockGroupLabel.Caption := dlgBlockGroupOptions;
  PersistentBlockCheckBox.Caption := dlgPersistentBlock;
  OverwriteBlockCheckBox.Caption := dlgOverwriteBlock;
end;

procedure TEditorGeneralOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  i: integer;
begin
  with AOptions as TEditorOptions do
  begin
    SetComboBoxText(BlockIndentComboBox, IntToStr(BlockIndent));
    SetComboBoxText(TabWidthsComboBox, IntToStr(TabWidth));
    BlockIndentTypeComboBox.ItemIndex := ord(BlockIndentType);

    // undo
    UndoAfterSaveCheckBox.Checked := UndoAfterSave;
    GroupUndoCheckBox.Checked := eoGroupUndo in SynEditOptions;
    SetComboBoxText(UndoLimitComboBox, IntToStr(UndoLimit));

    // scroll
    HalfPageScrollCheckBox.Checked := eoHalfPageScroll in SynEditOptions;
    ScrollByOneLessCheckBox.Checked := eoScrollByOneLess in SynEditOptions;
    ScrollPastEndFileCheckBox.Checked := eoScrollPastEoF in SynEditOptions;
    ScrollPastEndLineCheckBox.Checked := eoScrollPastEoL in SynEditOptions;

    // tabs, indents
    AutoIndentCheckBox.Checked := eoAutoIndent in SynEditOptions;
    TabIndentBlocksCheckBox.Checked := eoTabIndent in SynEditOptions;
    SmartTabsCheckBox.Checked := eoSmartTabs in SynEditOptions;
    TabsToSpacesCheckBox.Checked := eoTabsToSpaces in SynEditOptions;

    // cursor
    KeepCursorXCheckBox.Checked := eoKeepCaretX in SynEditOptions;
    PersistentCursorCheckBox.Checked := eoPersistentCaret in SynEditOptions;
    AlwaysVisibleCursorCheckBox.Checked := eoAlwaysVisibleCaret in SynEditOptions2;
    CursorSkipsSelectionCheckBox.Checked := eoCaretSkipsSelection in SynEditOptions2;
    HomeKeyJumpsToNearestStartCheckBox.Checked := eoEnhanceHomeKey in SynEditOptions;
    EndKeyJumpsToNearestStartCheckBox.Checked := eoEnhanceEndKey in SynEditOptions2;

    // block
    PersistentBlockCheckBox.Checked := eoPersistentBlock in SynEditOptions2;
    OverwriteBlockCheckBox.Checked := eoOverwriteBlock in SynEditOptions2;

    for i := Low(PreviewEdits) to High(PreviewEdits) do
      if PreviewEdits[i] <> nil then
        GetSynEditPreviewSettings(PreviewEdits[i]);
  end;
end;

procedure TEditorGeneralOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);

  procedure UpdateOptionFromBool(AValue: Boolean; AnOption: TSynEditorOption); overload;
  begin
    if AValue then
      TEditorOptions(AOptions).SynEditOptions := TEditorOptions(AOptions).SynEditOptions + [AnOption]
    else
      TEditorOptions(AOptions).SynEditOptions := TEditorOptions(AOptions).SynEditOptions - [AnOption];
  end;

  procedure UpdateOptionFromBool(AValue: Boolean; AnOption: TSynEditorOption2); overload;
  begin
    if AValue then
      TEditorOptions(AOptions).SynEditOptions2 := TEditorOptions(AOptions).SynEditOptions2 + [AnOption]
    else
      TEditorOptions(AOptions).SynEditOptions2 := TEditorOptions(AOptions).SynEditOptions2 - [AnOption];
  end;

var
  i: integer;
begin
  with AOptions as TEditorOptions do
  begin
    // undo
    UndoAfterSave := UndoAfterSaveCheckBox.Checked;
    UpdateOptionFromBool(GroupUndoCheckBox.Checked, eoGroupUndo);
    i := StrToIntDef(UndoLimitComboBox.Text, 32767);
    if i < 1 then
      i := 1;
    if i > 32767 then
      i := 32767;
    UndoLimit := i;

    // scroll
    UpdateOptionFromBool(HalfPageScrollCheckBox.Checked, eoHalfPageScroll);
    UpdateOptionFromBool(ScrollByOneLessCheckBox.Checked, eoScrollByOneLess);
    UpdateOptionFromBool(ScrollPastEndFileCheckBox.Checked, eoScrollPastEoF);
    UpdateOptionFromBool(ScrollPastEndLineCheckBox.Checked, eoScrollPastEoL);

    // tabs, indents
    UpdateOptionFromBool(AutoIndentCheckBox.Checked, eoAutoIndent);
    UpdateOptionFromBool(TabIndentBlocksCheckBox.Checked, eoTabIndent);
    UpdateOptionFromBool(SmartTabsCheckBox.Checked, eoSmartTabs);
    UpdateOptionFromBool(TabsToSpacesCheckBox.Checked, eoTabsToSpaces);

    i := StrToIntDef(TabWidthsComboBox.Text, 2);
    if i < 1 then
      i := 1;
    if i > 20 then
      i := 20;
    TabWidth := i;

    i := StrToIntDef(BlockIndentComboBox.Text, 2);
    if i < 1 then
      i := 1;
    if i > 20 then
      i := 20;
    BlockIndent := i;
    BlockIndentType := TSynBeautifierIndentType(BlockIndentTypeComboBox.ItemIndex);

    // cursor
    UpdateOptionFromBool(KeepCursorXCheckBox.Checked, eoKeepCaretX);
    UpdateOptionFromBool(PersistentCursorCheckBox.Checked, eoPersistentCaret);
    UpdateOptionFromBool(AlwaysVisibleCursorCheckBox.Checked, eoAlwaysVisibleCaret);
    UpdateOptionFromBool(CursorSkipsSelectionCheckBox.Checked, eoCaretSkipsSelection);
    UpdateOptionFromBool(HomeKeyJumpsToNearestStartCheckBox.Checked, eoEnhanceHomeKey);
    UpdateOptionFromBool(EndKeyJumpsToNearestStartCheckBox.Checked, eoEnhanceEndKey);

    // block
    UpdateOptionFromBool(PersistentBlockCheckBox.Checked, eoPersistentBlock);
    UpdateOptionFromBool(OverwriteBlockCheckBox.Checked, eoOverwriteBlock);
  end;
end;

class function TEditorGeneralOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEditorOptions;
end;

procedure TEditorGeneralOptionsFrame.SetPreviewOption(AValue: Boolean; AnOption: TSynEditorOption);
var
  a: Integer;
begin
  for a := Low(PreviewEdits) to High(PreviewEdits) do
  begin
    if PreviewEdits[a] <> nil then
      if AValue then
        PreviewEdits[a].Options := PreviewEdits[a].Options + [AnOption]
      else
        PreviewEdits[a].Options := PreviewEdits[a].Options - [AnOption];
  end;
end;

procedure TEditorGeneralOptionsFrame.SetPreviewOption(AValue: Boolean; AnOption: TSynEditorOption2);
var
  a: Integer;
begin
  for a := Low(PreviewEdits) to High(PreviewEdits) do
  begin
    if PreviewEdits[a] <> nil then
      if AValue then
        PreviewEdits[a].Options2 := PreviewEdits[a].Options2 + [AnOption]
      else
        PreviewEdits[a].Options2 := PreviewEdits[a].Options2 - [AnOption];
  end;
end;

procedure TEditorGeneralOptionsFrame.ComboboxOnChange(Sender: TObject);
var
  ComboBox: TComboBox absolute Sender;
begin
  if ComboBox.Items.IndexOf(ComboBox.Text) >= 0 then
    ComboBoxOnExit(Sender);
end;

procedure TEditorGeneralOptionsFrame.AutoIndentCheckBoxChange(Sender: TObject);
begin
  SetPreviewOption(AutoIndentCheckBox.Checked, eoAutoIndent);
end;

procedure TEditorGeneralOptionsFrame.AlwaysVisibleCursorCheckBoxChange(
  Sender: TObject);
begin
  SetPreviewOption(AlwaysVisibleCursorCheckBox.Checked, eoAlwaysVisibleCaret);
end;

procedure TEditorGeneralOptionsFrame.ComboboxOnKeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key = VK_S) then
    ComboBoxOnExit(Sender);
end;

procedure TEditorGeneralOptionsFrame.CursorSkipsSelectionCheckBoxChange(
  Sender: TObject);
begin
  SetPreviewOption(CursorSkipsSelectionCheckBox.Checked, eoCaretSkipsSelection);
end;

procedure TEditorGeneralOptionsFrame.ComboBoxOnExit(Sender: TObject);
var
  NewVal, a: Integer;
begin
  if Sender = BlockIndentComboBox then
  begin
    NewVal := StrToIntDef(BlockIndentComboBox.Text, PreviewEdits[1].BlockIndent);
    SetComboBoxText(BlockIndentComboBox, IntToStr(NewVal));
    for a := Low(PreviewEdits) to High(PreviewEdits) do
      if PreviewEdits[a] <> nil then
        PreviewEdits[a].BlockIndent := NewVal;
  end
  else
  if Sender = TabWidthsComboBox then
  begin
    NewVal := StrToIntDef(TabWidthsComboBox.Text, PreviewEdits[1].TabWidth);
    SetComboBoxText(TabWidthsComboBox, IntToStr(NewVal));
    for a := Low(PreviewEdits) to High(PreviewEdits) do
      if PreviewEdits[a] <> nil then
        PreviewEdits[a].TabWidth := NewVal;
  end
end;

procedure TEditorGeneralOptionsFrame.EndKeyJumpsToNearestStartCheckBoxChange(
  Sender: TObject);
begin
  SetPreviewOption(EndKeyJumpsToNearestStartCheckBox.Checked, eoEnhanceEndKey);
end;

procedure TEditorGeneralOptionsFrame.GroupUndoCheckBoxChange(Sender: TObject);
begin
  SetPreviewOption(GroupUndoCheckBox.Checked, eoGroupUndo);
end;

procedure TEditorGeneralOptionsFrame.HalfPageScrollCheckBoxChange(
  Sender: TObject);
begin
  SetPreviewOption(HalfPageScrollCheckBox.Checked, eoHalfPageScroll);
end;

procedure TEditorGeneralOptionsFrame.HomeKeyJumpsToNearestStartCheckBoxChange(
  Sender: TObject);
begin
  SetPreviewOption(HomeKeyJumpsToNearestStartCheckBox.Checked, eoEnhanceHomeKey);
end;

procedure TEditorGeneralOptionsFrame.KeepCursorXCheckBoxChange(Sender: TObject);
begin
  SetPreviewOption(KeepCursorXCheckBox.Checked, eoKeepCaretX);
end;

procedure TEditorGeneralOptionsFrame.OverwriteBlockCheckBoxChange(Sender: TObject);
begin
  SetPreviewOption(KeepCursorXCheckBox.Checked, eoOverwriteBlock);
end;

procedure TEditorGeneralOptionsFrame.PersistentBlockCheckBoxChange(Sender: TObject);
begin
  SetPreviewOption(PersistentBlockCheckBox.Checked, eoPersistentBlock);
end;

procedure TEditorGeneralOptionsFrame.PersistentCursorCheckBoxChange(
  Sender: TObject);
begin
  SetPreviewOption(PersistentCursorCheckBox.Checked, eoPersistentCaret);
end;

procedure TEditorGeneralOptionsFrame.ScrollByOneLessCheckBoxChange(
  Sender: TObject);
begin
  SetPreviewOption(ScrollByOneLessCheckBox.Checked, eoScrollByOneLess);
end;

procedure TEditorGeneralOptionsFrame.ScrollPastEndFileCheckBoxChange(
  Sender: TObject);
begin
  SetPreviewOption(ScrollPastEndFileCheckBox.Checked, eoScrollPastEoF);
end;

procedure TEditorGeneralOptionsFrame.ScrollPastEndLineCheckBoxChange(
  Sender: TObject);
begin
  SetPreviewOption(ScrollPastEndLineCheckBox.Checked, eoScrollPastEoL);
end;

procedure TEditorGeneralOptionsFrame.SmartTabsCheckBoxChange(Sender: TObject);
begin
  SetPreviewOption(SmartTabsCheckBox.Checked, eoSmartTabs);
end;

procedure TEditorGeneralOptionsFrame.TabIndentBlocksCheckBoxChange(
  Sender: TObject);
begin
  SetPreviewOption(TabIndentBlocksCheckBox.Checked, eoTabIndent);
end;

procedure TEditorGeneralOptionsFrame.TabsToSpacesCheckBoxChange(Sender: TObject
  );
begin
  SetPreviewOption(TabsToSpacesCheckBox.Checked, eoTabsToSpaces);
end;

function TEditorGeneralOptionsFrame.DefaultBookmarkImages: TImageList;
var
  i: integer;
begin
  if FDefaultBookmarkImages = nil then
  begin
    FDefaultBookmarkImages := TImageList.Create(Self);
    FDefaultBookmarkImages.Width := 11;
    FDefaultBookmarkImages.Height := 11;
    for i := 0 to 9 do
      FDefaultBookmarkImages.AddLazarusResource('bookmark' + IntToStr(i));
  end;
  Result := FDefaultBookmarkImages;
end;

procedure TEditorGeneralOptionsFrame.AddPreviewEdit(AEditor: TPreviewEditor);
begin
  SetLength(PreviewEdits, Length(PreviewEdits) + 1);
  PreviewEdits[Length(PreviewEdits)-1] := AEditor;
  if AEditor.BookMarkOptions.BookmarkImages = nil then
    AEditor.BookMarkOptions.BookmarkImages := DefaultBookmarkImages;
end;

constructor TEditorGeneralOptionsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  PreviewEdits := nil;
end;

initialization
  {$I editor_general_options.lrs}
  RegisterIDEOptionsEditor(GroupEditor, TEditorGeneralOptionsFrame, EdtOptionsGeneral);
end.

