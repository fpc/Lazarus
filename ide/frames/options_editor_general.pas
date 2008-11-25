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
unit options_editor_general;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Graphics, LCLProc, LCLType,
  StdCtrls, SynEdit, SynEditMarkupBracket, Controls, ExtCtrls,
  EditorOptions, LazarusIDEStrConsts, IDEProcs, IDEOptionsIntf;

type
  TPreviewEditor = TSynEdit;
  { TEditorGeneralOptionsFrame }

  TEditorGeneralOptionsFrame = class(TAbstractIDEOptionsEditor)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    BlockIndentComboBox: TComboBox;
    BlockIndentLabel: TLabel;
    BracketLabel: TLabel;
    BracketCombo: TComboBox;
    AutoIndentCheckBox: TCheckBox;
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
    EditorOptionsGroupBox: TCheckGroup;
    TabWidthsComboBox: TComboBox;
    TabWidthsLabel: TLabel;
    ScrollGroupLabel: TLabel;
    IndentsTabsGroupLabel: TLabel;
    UndoLimitComboBox: TComboBox;
    UndoLimitLabel: TLabel;
    procedure AutoIndentCheckBoxChange(Sender: TObject);
    procedure ComboboxOnChange(Sender: TObject);
    procedure ComboboxOnKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditorOptionsGroupBoxItemClick(Sender: TObject; Index: integer);
    procedure ComboBoxOnExit(Sender: TObject);
    procedure GroupUndoCheckBoxChange(Sender: TObject);
    procedure HalfPageScrollCheckBoxChange(Sender: TObject);
    procedure ScrollByOneLessCheckBoxChange(Sender: TObject);
    procedure ScrollPastEndFileCheckBoxChange(Sender: TObject);
    procedure ScrollPastEndLineCheckBoxChange(Sender: TObject);
    procedure SmartTabsCheckBoxChange(Sender: TObject);
    procedure TabIndentBlocksCheckBoxChange(Sender: TObject);
    procedure TabsToSpacesCheckBoxChange(Sender: TObject);
  public
    PreviewEdits: array of TPreviewEditor;
    procedure SetPreviewOption(AValue: Boolean; AnOption: TSynEditorOption);
    procedure SetPreviewOption2(AValue: Boolean; AnOption: TSynEditorOption2);

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
  EditorOptionsGroupBox.Caption := lismenueditoroptions;
  with EditorOptionsGroupBox do
  begin
    // selections
    Items.Add(dlgAltSetClMode);
    // visual effects
    Items.Add(dlgShowGutterHints);
    //Items.Add(dlgShowScrollHint);
    Items.Add(lisShowSpecialCharacters);
    //Items.Add(dlgUseSyntaxHighlight);
    // drag&drop
    Items.Add(dlgDragDropEd);
    Items.Add(dlgDropFiles);
    // caret + scrolling + key navigation
    Items.Add(dlgKeepCursorX);
    Items.Add(dlgPersistentCursor);
    Items.Add(dlgCursorSkipsSelection);
    Items.Add(dlgRightMouseMovesCursor);
    Items.Add(dlgHomeKeyJumpsToNearestStart);
    Items.Add(dlgAlwaysVisibleCursor);
    // spaces
    Items.Add(dlgTrimTrailingSpaces);
    // mouse
    Items.Add(dlgDoubleClickLine);
    Items.Add(dlgMouseLinks);
    Items.Add(dlgCloseButtonsNotebook);
    // copying
    Items.Add(dlgFindTextatCursor);
    Items.Add(dlgCopyWordAtCursorOnCopyNone);
  end;

  BracketCombo.Items.Add(dlgNoBracketHighlight);
  BracketCombo.Items.Add(dlgHighlightLeftOfCursor);
  BracketCombo.Items.Add(dlgHighlightRightOfCursor);
  BracketCombo.Items.Add(gldHighlightBothSidesOfCursor);

  BracketLabel.Caption := dlgBracketHighlight;
  BlockIndentLabel.Caption := dlgBlockIndent;
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
end;

procedure TEditorGeneralOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  i: integer;
begin
  with AOptions as TEditorOptions do
  begin
    with EditorOptionsGroupBox do
    begin
      Checked[Items.IndexOf(dlgAltSetClMode)] := eoAltSetsColumnMode in SynEditOptions;
      Checked[Items.IndexOf(dlgDragDropEd)] := eoDragDropEditing in SynEditOptions;
      Checked[Items.IndexOf(dlgDropFiles)]    := eoDropFiles in SynEditOptions;
      Checked[Items.IndexOf(dlgKeepCursorX)] := eoKeepCaretX in SynEditOptions;
      Checked[Items.IndexOf(dlgPersistentCursor)] := eoPersistentCaret in SynEditOptions;
      Checked[Items.IndexOf(dlgRightMouseMovesCursor)] := eoRightMouseMovesCursor in SynEditOptions;
      Checked[Items.IndexOf(lisShowSpecialCharacters)] := eoShowSpecialChars in SynEditOptions;
      Checked[Items.IndexOf(dlgTrimTrailingSpaces)] := eoTrimTrailingSpaces in SynEditOptions;
      Checked[Items.IndexOf(dlgDoubleClickLine)] := eoDoubleClickSelectsLine in SynEditOptions;
      Checked[Items.IndexOf(dlgHomeKeyJumpsToNearestStart)] := eoEnhanceHomeKey in SynEditOptions;
      Checked[Items.IndexOf(dlgCursorSkipsSelection)] := eoCaretSkipsSelection in SynEditOptions2;
      Checked[Items.IndexOf(dlgAlwaysVisibleCursor)] := eoAlwaysVisibleCaret in SynEditOptions2;

      Checked[Items.IndexOf(dlgCloseButtonsNotebook)] := ShowTabCloseButtons;
      Checked[Items.IndexOf(dlgMouseLinks)]   := CtrlMouseLinks;
      Checked[Items.IndexOf(dlgShowGutterHints)] := ShowGutterHints;
      Checked[Items.IndexOf(dlgFindTextatCursor)] := FindTextAtCursor;
      //Checked[Items.IndexOf(dlgUseSyntaxHighlight)] := UseSyntaxHighlight;
      Checked[Items.IndexOf(dlgCopyWordAtCursorOnCopyNone)] := CopyWordAtCursorOnCopyNone;
    end;

    if eoBracketHighlight in SynEditOptions then
      BracketCombo.ItemIndex := Ord(BracketHighlightStyle) + 1
    else
      BracketCombo.ItemIndex := 0;

    SetComboBoxText(BlockIndentComboBox, IntToStr(BlockIndent));
    SetComboBoxText(TabWidthsComboBox, IntToStr(TabWidth));

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

    for i := Low(PreviewEdits) to High(PreviewEdits) do
      if PreviewEdits[i] <> nil then
        GetSynEditPreviewSettings(PreviewEdits[i]);
  end;
end;

procedure TEditorGeneralOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);

  procedure UpdateOptionFromBool(AValue: Boolean; AnOption: TSynEditorOption);
  begin
    if AValue then
      TEditorOptions(AOptions).SynEditOptions := TEditorOptions(AOptions).SynEditOptions + [AnOption]
    else
      TEditorOptions(AOptions).SynEditOptions := TEditorOptions(AOptions).SynEditOptions - [AnOption];
  end;

  procedure UpdateOption(const CheckBoxName: String; AnOption: TSynEditorOption);
  var
    i: integer;
  begin
    i := EditorOptionsGroupBox.Items.IndexOf(CheckBoxName);
    UpdateOptionFromBool(EditorOptionsGroupBox.Checked[i], AnOption);
  end;

  procedure UpdateOption2(const CheckBoxName: String; AnOption: TSynEditorOption2);
  var
    i: integer;
  begin
      i := EditorOptionsGroupBox.Items.IndexOf(CheckBoxName);
      if EditorOptionsGroupBox.Checked[i] then
        TEditorOptions(AOptions).SynEditOptions2 := TEditorOptions(AOptions).SynEditOptions2 + [AnOption]
      else
        TEditorOptions(AOptions).SynEditOptions2 := TEditorOptions(AOptions).SynEditOptions2 - [AnOption];
  end;

var
  i: integer;
begin
  with AOptions as TEditorOptions do
  begin
    UpdateOption(dlgAltSetClMode, eoAltSetsColumnMode);
    UpdateOption(dlgDoubleClickLine, eoDoubleClickSelectsLine);
    UpdateOption(dlgDragDropEd, eoDragDropEditing);
    UpdateOption(dlgDropFiles, eoDropFiles);
    UpdateOption(dlgHomeKeyJumpsToNearestStart, eoEnhanceHomeKey);
    UpdateOption(dlgKeepCursorX, eoKeepCaretX);
    UpdateOption(dlgPersistentCursor, eoPersistentCaret);
    UpdateOption(dlgRightMouseMovesCursor, eoRightMouseMovesCursor);
    UpdateOption(lisShowSpecialCharacters, eoShowSpecialChars);
    UpdateOption(dlgTrimTrailingSpaces, eoTrimTrailingSpaces);
    UpdateOption(dlgDoubleClickLine, eoDoubleClickSelectsLine);
    UpdateOption(dlgHomeKeyJumpsToNearestStart, eoEnhanceHomeKey);
    UpdateOption2(dlgCursorSkipsSelection, eoCaretSkipsSelection);
    UpdateOption2(dlgAlwaysVisibleCursor, eoAlwaysVisibleCaret);

    ShowTabCloseButtons := CheckGroupItemChecked(EditorOptionsGroupBox, dlgCloseButtonsNotebook);
    CopyWordAtCursorOnCopyNone := CheckGroupItemChecked(EditorOptionsGroupBox, dlgCopyWordAtCursorOnCopyNone);
    ShowGutterHints := CheckGroupItemChecked(EditorOptionsGroupBox, dlgShowGutterHints);
    FindTextAtCursor := CheckGroupItemChecked(EditorOptionsGroupBox, dlgFindTextatCursor);
    //UseSyntaxHighlight := CheckGroupItemChecked(EditorOptionsGroupBox, dlgUseSyntaxHighlight);
    CtrlMouseLinks := CheckGroupItemChecked(EditorOptionsGroupBox, dlgMouseLinks);

    if BracketCombo.ItemIndex = 0 then
      TEditorOptions(AOptions).SynEditOptions := TEditorOptions(AOptions).SynEditOptions - [eoBracketHighlight]
    else
    begin
      TEditorOptions(AOptions).SynEditOptions := TEditorOptions(AOptions).SynEditOptions + [eoBracketHighlight];
      TEditorOptions(AOptions).BracketHighlightStyle := TSynEditBracketHighlightStyle(BracketCombo.ItemIndex - 1);
    end;

    if eoBracketHighlight in SynEditOptions then
      BracketCombo.ItemIndex := Ord(BracketHighlightStyle) + 1
    else
      BracketCombo.ItemIndex := 0;

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

procedure TEditorGeneralOptionsFrame.SetPreviewOption2(AValue: Boolean; AnOption: TSynEditorOption2);
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

procedure TEditorGeneralOptionsFrame.EditorOptionsGroupBoxItemClick(
  Sender: TObject; Index: integer);

  procedure SetOption(const CheckBoxName: String; AnOption: TSynEditorOption);
  var
    i: LongInt;
  begin
    i := EditorOptionsGroupBox.Items.IndexOf(CheckBoxName);
    if i < 0 then
      Exit;

    SetPreviewOption(EditorOptionsGroupBox.Checked[i], AnOption);
  end;

  procedure SetOption2(const CheckBoxName: String; AnOption: TSynEditorOption2);
  var
    i: LongInt;
  begin
    i := EditorOptionsGroupBox.Items.IndexOf(CheckBoxName);
    if i < 0 then
      Exit;
    SetPreviewOption2(EditorOptionsGroupBox.Checked[i], AnOption);
  end;

begin
  SetOption(dlgAltSetClMode, eoAltSetsColumnMode);
  SetOption(dlgDoubleClickLine, eoDoubleClickSelectsLine);
  SetOption(dlgDragDropEd, eoDragDropEditing);
  SetOption(dlgDropFiles, eoDropFiles);
  SetOption(dlgHomeKeyJumpsToNearestStart, eoEnhanceHomeKey);
  SetOption(dlgKeepCursorX, eoKeepCaretX);
  SetOption(dlgPersistentCursor, eoPersistentCaret);
  SetOption(dlgRightMouseMovesCursor, eoRightMouseMovesCursor);
  // not for Preview: SetOption('NoSelectionCheckBox',eoNoSelection);
  SetOption(lisShowSpecialCharacters, eoShowSpecialChars);
  //SetOption(dlgShowScrollHint, eoShowScrollHint);
  SetOption(dlgTrimTrailingSpaces, eoTrimTrailingSpaces);

  SetOption2(dlgCursorSkipsSelection, eoCaretSkipsSelection);
  SetOption2(dlgAlwaysVisibleCursor, eoAlwaysVisibleCaret);
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

procedure TEditorGeneralOptionsFrame.ComboboxOnKeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key = VK_S) then
    ComboBoxOnExit(Sender);
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
  else
  if Sender = BracketCombo then
  begin
    for a := Low(PreviewEdits) to High(PreviewEdits) do
      if PreviewEdits[a] <> nil then
      begin
        if BracketCombo.ItemIndex = 0 then
          PreviewEdits[a].Options := PreviewEdits[a].Options - [eoBracketHighlight]
        else
        begin
          PreviewEdits[a].Options := PreviewEdits[a].Options + [eoBracketHighlight];
          PreviewEdits[a].BracketHighlightStyle := TSynEditBracketHighlightStyle(BracketCombo.ItemIndex - 1);
        end;
      end;
  end;
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

constructor TEditorGeneralOptionsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  PreviewEdits := nil;
end;

initialization
  {$I options_editor_general.lrs}
  RegisterIDEOptionsEditor(GroupEditor, TEditorGeneralOptionsFrame, EdtOptionsGeneral);
end.

