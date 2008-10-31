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
  StdCtrls, SynEdit, Controls, ExtCtrls,
  EditorOptions, LazarusIDEStrConsts, IDEProcs, IDEOptionsIntf, SrcEditorIntf,
  KeyMapping;

type
  TPreviewEditor = TSynEdit;
  { TEditorGeneralOptionsFrame }

  TEditorGeneralOptionsFrame = class(TAbstractIDEOptionsEditor)
    BlockIndentComboBox: TComboBox;
    BlockIndentLabel: TLabel;
    EditorOptionsGroupBox: TCheckGroup;
    TabWidthsComboBox: TComboBox;
    TabWidthsLabel: TLabel;
    UndoLimitComboBox: TComboBox;
    UndoLimitLabel: TLabel;
    procedure ComboboxOnChange(Sender: TObject);
    procedure ComboboxOnKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditorOptionsGroupBoxItemClick(Sender: TObject; Index: integer);
    procedure ComboBoxOnExit(Sender: TObject);
  private
    { private declarations }
  public
    FHighlighterList: TStringList; // list of "ColorScheme" Data=TSrcIDEHighlighter
    FColorSchemes: TStringList;    // list of LanguageName=ColorScheme
    PreviewEdits: array of TPreviewEditor;
    PreviewSyn: TSrcIDEHighlighter;
    CurLanguageID: Integer;
    EditingKeyMap: TKeyCommandRelationList;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;

    function GetCurColorScheme(const LanguageName: String): String;
    function GetHighlighter(SynClass: TCustomSynClass;
      const ColorScheme: String; CreateIfNotExists: Boolean): TSrcIDEHighlighter;
    procedure ClearHighlighters;
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
    Items.Add(dlgAutoIdent);
    // visual effects
    Items.Add(dlgBracHighlight);
    Items.Add(dlgShowGutterHints);
    //Items.Add(dlgShowScrollHint);
    Items.Add(lisShowSpecialCharacters);
    Items.Add(dlgUseSyntaxHighlight);
    // drag&drop
    Items.Add(dlgDragDropEd);
    Items.Add(dlgDropFiles);
    // caret + scrolling + key navigation
    Items.Add(dlgHalfPageScroll);
    Items.Add(dlgKeepCursorX);
    Items.Add(dlgPersistentCursor);
    Items.Add(dlgCursorSkipsSelection);
    Items.Add(dlgRightMouseMovesCursor);
    Items.Add(dlgScrollByOneLess);
    Items.Add(dlgScrollPastEndFile);
    Items.Add(dlgScrollPastEndLine);
    Items.Add(dlgHomeKeyJumpsToNearestStart);
    Items.Add(dlgAlwaysVisibleCursor);
    // tabs
    Items.Add(dlgSmartTabs);
    Items.Add(dlgTabsToSpaces);
    Items.Add(dlgTabIndent);
    // spaces
    Items.Add(dlgTrimTrailingSpaces);
    // undo
    Items.Add(dlgUndoAfterSave);
    Items.Add(dlgGroupUndo);
    // mouse
    Items.Add(dlgDoubleClickLine);
    Items.Add(dlgMouseLinks);
    Items.Add(dlgCloseButtonsNotebook);
    // copying
    Items.Add(dlgFindTextatCursor);
    Items.Add(dlgCopyWordAtCursorOnCopyNone);
  end;
  BlockIndentLabel.Caption := dlgBlockIndent;
  UndoLimitLabel.Caption := dlgUndoLimit;
  TabWidthsLabel.Caption := dlgTabWidths;
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
      Checked[Items.IndexOf(dlgAutoIdent)]    := eoAutoIndent in SynEditOptions;
      Checked[Items.IndexOf(dlgBracHighlight)] := eoBracketHighlight in SynEditOptions;
      Checked[Items.IndexOf(dlgDragDropEd)] := eoDragDropEditing in SynEditOptions;
      Checked[Items.IndexOf(dlgDropFiles)]    := eoDropFiles in SynEditOptions;
      Checked[Items.IndexOf(dlgGroupUndo)] := eoGroupUndo in SynEditOptions;
      Checked[Items.IndexOf(dlgHalfPageScroll)] := eoHalfPageScroll in SynEditOptions;
      Checked[Items.IndexOf(dlgKeepCursorX)] := eoKeepCaretX in SynEditOptions;
      Checked[Items.IndexOf(dlgPersistentCursor)] := eoPersistentCaret in SynEditOptions;
      Checked[Items.IndexOf(dlgRightMouseMovesCursor)] := eoRightMouseMovesCursor in SynEditOptions;
      Checked[Items.IndexOf(dlgScrollByOneLess)] := eoScrollByOneLess in SynEditOptions;
      Checked[Items.IndexOf(dlgScrollPastEndFile)] := eoScrollPastEoF in SynEditOptions;
      Checked[Items.IndexOf(dlgScrollPastEndLine)] := eoScrollPastEoL in SynEditOptions;
      Checked[Items.IndexOf(lisShowSpecialCharacters)] := eoShowSpecialChars in SynEditOptions;
      Checked[Items.IndexOf(dlgSmartTabs)] := eoSmartTabs in SynEditOptions;
      Checked[Items.IndexOf(dlgTabsToSpaces)] := eoTabsToSpaces in SynEditOptions;
      Checked[Items.IndexOf(dlgTabIndent)]    := eoTabIndent in SynEditOptions;
      Checked[Items.IndexOf(dlgTrimTrailingSpaces)] := eoTrimTrailingSpaces in SynEditOptions;
      Checked[Items.IndexOf(dlgDoubleClickLine)] := eoDoubleClickSelectsLine in SynEditOptions;
      Checked[Items.IndexOf(dlgHomeKeyJumpsToNearestStart)] := eoEnhanceHomeKey in SynEditOptions;
      Checked[Items.IndexOf(dlgCursorSkipsSelection)] := eoCaretSkipsSelection in SynEditOptions2;
      Checked[Items.IndexOf(dlgAlwaysVisibleCursor)] := eoAlwaysVisibleCaret in SynEditOptions2;

      Checked[Items.IndexOf(dlgCloseButtonsNotebook)] := ShowTabCloseButtons;
      Checked[Items.IndexOf(dlgUndoAfterSave)] := UndoAfterSave;
      Checked[Items.IndexOf(dlgMouseLinks)]   := CtrlMouseLinks;
      Checked[Items.IndexOf(dlgShowGutterHints)] := ShowGutterHints;
      Checked[Items.IndexOf(dlgFindTextatCursor)] := FindTextAtCursor;
      Checked[Items.IndexOf(dlgUseSyntaxHighlight)] := UseSyntaxHighlight;
      Checked[Items.IndexOf(dlgCopyWordAtCursorOnCopyNone)] := CopyWordAtCursorOnCopyNone;
    end;

    SetComboBoxText(BlockIndentComboBox, IntToStr(BlockIndent));
    SetComboBoxText(UndoLimitComboBox, IntToStr(UndoLimit));
    SetComboBoxText(TabWidthsComboBox, IntToStr(TabWidth));

    PreviewSyn := GetHighlighter(TPreviewPasSyn, GetCurColorScheme(TPreviewPasSyn.GetLanguageName), True);
    CurLanguageID := HighlighterList.FindByClass(TCustomSynClass(PreviewSyn.ClassType));
    EditingKeyMap.Assign(KeyMap);

    for i := Low(PreviewEdits) to High(PreviewEdits) do
      if PreviewEdits[i] <> nil then
        with PreviewEdits[i] do
        begin
          if UseSyntaxHighlight then
            Highlighter := PreviewSyn;
          GetSynEditPreviewSettings(PreviewEdits[i]);
          EditingKeyMap.AssignTo(PreviewEdits[i].KeyStrokes, TSourceEditorWindowInterface);
          Lines.Text := HighlighterList[CurLanguageID].SampleSource;
          CaretXY := HighlighterList[CurLanguageID].CaretXY;
          TopLine := 1;
          LeftChar := 1;
        end;
  end;
end;

procedure TEditorGeneralOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);

  procedure UpdateOption(const CheckBoxName: String; AnOption: TSynEditorOption);
  var
    i: integer;
  begin
      i := EditorOptionsGroupBox.Items.IndexOf(CheckBoxName);
      if EditorOptionsGroupBox.Checked[i] then
        TEditorOptions(AOptions).SynEditOptions := TEditorOptions(AOptions).SynEditOptions + [AnOption]
      else
        TEditorOptions(AOptions).SynEditOptions := TEditorOptions(AOptions).SynEditOptions - [AnOption];
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
    UpdateOption(dlgAutoIdent, eoAutoIndent);
    UpdateOption(dlgBracHighlight,eoBracketHighlight);
    UpdateOption(dlgDoubleClickLine, eoDoubleClickSelectsLine);
    UpdateOption(dlgDragDropEd, eoDragDropEditing);
    UpdateOption(dlgDropFiles, eoDropFiles);
    UpdateOption(dlgGroupUndo, eoGroupUndo);
    UpdateOption(dlgHomeKeyJumpsToNearestStart, eoEnhanceHomeKey);
    UpdateOption(dlgHalfPageScroll, eoHalfPageScroll);
    UpdateOption(dlgKeepCursorX, eoKeepCaretX);
    UpdateOption(dlgPersistentCursor, eoPersistentCaret);
    UpdateOption(dlgRightMouseMovesCursor, eoRightMouseMovesCursor);
    UpdateOption(dlgScrollByOneLess, eoScrollByOneLess);
    UpdateOption(dlgScrollPastEndFile, eoScrollPastEoF);
    UpdateOption(dlgScrollPastEndLine, eoScrollPastEoL);
    UpdateOption(lisShowSpecialCharacters, eoShowSpecialChars);
    UpdateOption(dlgSmartTabs, eoSmartTabs);
    UpdateOption(dlgTabsToSpaces, eoTabsToSpaces);
    UpdateOption(dlgTabIndent, eoTabIndent);
    UpdateOption(dlgTrimTrailingSpaces, eoTrimTrailingSpaces);
    UpdateOption(dlgDoubleClickLine, eoDoubleClickSelectsLine);
    UpdateOption(dlgHomeKeyJumpsToNearestStart, eoEnhanceHomeKey);
    UpdateOption2(dlgCursorSkipsSelection, eoCaretSkipsSelection);
    UpdateOption2(dlgAlwaysVisibleCursor, eoAlwaysVisibleCaret);

    ShowTabCloseButtons := CheckGroupItemChecked(EditorOptionsGroupBox, dlgCloseButtonsNotebook);
    UndoAfterSave := CheckGroupItemChecked(EditorOptionsGroupBox, dlgUndoAfterSave);
    CopyWordAtCursorOnCopyNone := CheckGroupItemChecked(EditorOptionsGroupBox, dlgCopyWordAtCursorOnCopyNone);
    ShowGutterHints := CheckGroupItemChecked(EditorOptionsGroupBox, dlgShowGutterHints);
    FindTextAtCursor := CheckGroupItemChecked(EditorOptionsGroupBox, dlgFindTextatCursor);
    UseSyntaxHighlight := CheckGroupItemChecked(EditorOptionsGroupBox, dlgUseSyntaxHighlight);
    CtrlMouseLinks := CheckGroupItemChecked(EditorOptionsGroupBox, dlgMouseLinks);

    i := StrToIntDef(UndoLimitComboBox.Text, 32767);
    if i < 1 then
      i := 1;
    if i > 32767 then
      i := 32767;
    UndoLimit := i;

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

function TEditorGeneralOptionsFrame.GetCurColorScheme(const LanguageName: String): String;
begin
  if FColorSchemes = nil then
    Result := ''
  else
    Result := FColorSchemes.Values[LanguageName];
  if Result = '' then
    Result := EditorOpts.ReadColorScheme(LanguageName);
end;

function TEditorGeneralOptionsFrame.GetHighlighter(SynClass: TCustomSynClass;
  const ColorScheme: String; CreateIfNotExists: Boolean): TSrcIDEHighlighter;
var
  i: Integer;
begin
  if FHighlighterList = nil then
    FHighlighterList := TStringList.Create;
  for i := 0 to FHighlighterList.Count - 1 do
    if (FHighlighterList[i] = ColorScheme) and
      (TCustomSynClass(TSrcIDEHighlighter(fHighlighterList.Objects[i]).ClassType) =
      SynClass) then
    begin
      Result := TSrcIDEHighlighter(FHighlighterList.Objects[i]);
      exit;
    end;
  if CreateIfNotExists then
  begin
    Result := SynClass.Create(nil);
    EditorOpts.AddSpecialHilightAttribsToHighlighter(Result);
    FHighlighterList.AddObject(ColorScheme, Result);
    EditorOpts.ReadHighlighterSettings(Result, ColorScheme);
  end;
end;

procedure TEditorGeneralOptionsFrame.ClearHighlighters;
var
  i: Integer;
begin
  if FHighlighterList = nil then
    Exit;
  for i := 0 to FHighlighterList.Count - 1 do
    TSrcIDEHighlighter(FHighlighterList.Objects[i]).Free;
  FHighlighterList.Free;
end;

procedure TEditorGeneralOptionsFrame.EditorOptionsGroupBoxItemClick(
  Sender: TObject; Index: integer);

  procedure SetOption(const CheckBoxName: String; AnOption: TSynEditorOption);
  var
    a: Integer;
    i: LongInt;
  begin
    i := EditorOptionsGroupBox.Items.IndexOf(CheckBoxName);
    if i < 0 then
      Exit;

    for a := Low(PreviewEdits) to High(PreviewEdits) do
    begin
      if PreviewEdits[a] <> nil then
        if EditorOptionsGroupBox.Checked[i] then
          PreviewEdits[a].Options := PreviewEdits[a].Options + [AnOption]
        else
          PreviewEdits[a].Options := PreviewEdits[a].Options - [AnOption];
    end;
  end;

  procedure SetOption2(const CheckBoxName: String; AnOption: TSynEditorOption2);
  var
    a: Integer;
    i: LongInt;
  begin
    i := EditorOptionsGroupBox.Items.IndexOf(CheckBoxName);
    if i < 0 then
      Exit;
    for a := Low(PreviewEdits) to High(PreviewEdits) do
    begin
      if PreviewEdits[a] <> nil then
        if EditorOptionsGroupBox.Checked[i] then
          PreviewEdits[a].Options2 := PreviewEdits[a].Options2 + [AnOption]
        else
          PreviewEdits[a].Options2 := PreviewEdits[a].Options2 - [AnOption];
    end;
  end;

var
  a: Integer;
  i: LongInt;
begin
  SetOption(dlgAltSetClMode, eoAltSetsColumnMode);
  SetOption(dlgAutoIdent, eoAutoIndent);
  SetOption(dlgBracHighlight,eoBracketHighlight);
  SetOption(dlgDoubleClickLine, eoDoubleClickSelectsLine);
  SetOption(dlgDragDropEd, eoDragDropEditing);
  SetOption(dlgDropFiles, eoDropFiles);
  SetOption(dlgGroupUndo, eoGroupUndo);
  SetOption(dlgHomeKeyJumpsToNearestStart, eoEnhanceHomeKey);
  SetOption(dlgHalfPageScroll, eoHalfPageScroll);
  SetOption(dlgKeepCursorX, eoKeepCaretX);
  SetOption(dlgPersistentCursor, eoPersistentCaret);
  SetOption(dlgRightMouseMovesCursor, eoRightMouseMovesCursor);
  // not for Preview: SetOption('NoSelectionCheckBox',eoNoSelection);
  SetOption(dlgScrollByOneLess, eoScrollByOneLess);
  SetOption(dlgScrollPastEndFile, eoScrollPastEoF);
  SetOption(dlgScrollPastEndLine, eoScrollPastEoL);
  SetOption(lisShowSpecialCharacters, eoShowSpecialChars);
  //SetOption(dlgShowScrollHint, eoShowScrollHint);
  SetOption(dlgSmartTabs, eoSmartTabs);
  SetOption(dlgTabsToSpaces, eoTabsToSpaces);
  SetOption(dlgTabIndent, eoTabIndent);
  SetOption(dlgTrimTrailingSpaces, eoTrimTrailingSpaces);

  SetOption2(dlgCursorSkipsSelection, eoCaretSkipsSelection);
  SetOption2(dlgAlwaysVisibleCursor, eoAlwaysVisibleCaret);

  i := EditorOptionsGroupBox.Items.IndexOf(dlgUseSyntaxHighlight);
  for a := Low(PreviewEdits) to High(PreviewEdits) do
    if PreviewEdits[a] <> nil then
      if EditorOptionsGroupBox.Checked[i] then
        PreviewEdits[a].Highlighter := PreviewSyn
      else
        PreviewEdits[a].Highlighter := nil;
end;

procedure TEditorGeneralOptionsFrame.ComboboxOnChange(Sender: TObject);
var
  ComboBox: TComboBox absolute Sender;
begin
  if ComboBox.Items.IndexOf(ComboBox.Text) >= 0 then
    ComboBoxOnExit(Sender);
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
  end;
end;

constructor TEditorGeneralOptionsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  PreviewEdits := nil;
  EditingKeyMap := TKeyCommandRelationList.Create;
end;

destructor TEditorGeneralOptionsFrame.Destroy;
begin
  ClearHighlighters;
  FColorSchemes.Free;
  EditingKeyMap.Free;
  inherited Destroy;
end;

initialization
  {$I options_editor_general.lrs}
  RegisterIDEOptionsEditor(GroupEditor, TEditorGeneralOptionsFrame, EdtOptionsGeneral);
end.

