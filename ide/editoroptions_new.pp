{  $Id$  }
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
    Editor options container and editor options dialog.
    The editor options are stored in XML format in the
     ~/.lazarus/editoroptions.xml file.
    Currently only for TSynEdit.
}
unit EditorOptions_new;

{$mode objfpc}{$H+}

interface

uses
  // RTL, FCL
  Classes, Math, SysUtils,
  // LCL
  Buttons, ComCtrls, Controls, Dialogs, ExtCtrls, FileCtrl, Forms, Graphics,
  GraphType, LCLIntf, LCLProc, LCLType, LResources, StdCtrls,
  // synedit
  SynEdit, SynEditAutoComplete, SynEditHighlighter, SynEditKeyCmds,
  SynHighlighterCPP, SynHighlighterHTML, SynHighlighterJava, SynHighlighterLFM,
  SynHighlighterPas, SynHighlighterPerl, SynHighlighterPHP, SynHighlighterSQL,
  SynHighlighterPython, SynHighlighterUNIXShellScript, SynHighlighterXML,
  // codetools
  Laz_XMLCfg,
  // IDEIntf
  IDECommands, IDEWindowIntf, SrcEditorIntf,
  // IDE
  LazarusIDEStrConsts, IDEOptionDefs, IDEProcs, InputHistory, KeyMapping,
  KeymapSchemeDlg, LazConf, Spin,
  //PreferencesDialog
  OptionsEditorBase,
  // uses EditorOptions too for common declarations
  EditorOptions
  ;

type

  { TEditorOptionsForm }

  TEditorOptionsFormNew = class(TOptionsEditorForm)
    ForeGroundLabel: TLabel;
    BackGroundLabel: TLabel;
    EditorOptionsGroupBox: TCheckGroup;

    Notebook: TNotebook;
    ImageList:    TImageList;

    // general options
    GeneralPage: TPage;
    DisplayPage: TPage;
    KeymappingPage: TPage;
    ColorPage: TPage;
    CodetoolsPage: TPage;
    BlockIndentComboBox: TComboBox;
    BlockIndentLabel: TLabel;
    CodeFolding: TPage;
    UndoLimitComboBox: TComboBox;
    UndoLimitLabel: TLabel;
    TabWidthsComboBox: TComboBox;
    TabWidthsLabel: TLabel;

    // Display options
    MarginAndGutterGroupBox: TGroupBox;
    VisibleRightMarginCheckBox: TCheckBox;
    VisibleGutterCheckBox: TCheckBox;
    ShowLineNumbersCheckBox: TCheckBox;
    GutterColorButton: TColorButton;
    GutterColorLabel:  TLabel;
    GutterWidthComboBox: TComboBox;
    GutterWidthLabel:  TLabel;
    RightMarginComboBox: TComboBox;
    RightMarginLabel:  TLabel;
    RightMarginColorButton: TColorButton;
    RightMarginColorLabel: TLabel;
    EditorFontGroupBox: TGroupBox;
    EditorFontComboBox: TComboBox;
    EditorFontButton:  TButton;
    EditorFontLabel:   TLabel;
    EditorFontHeightLabel: TLabel;
    EditorFontHeightComboBox: TComboBox;
    ExtraLineSpacingLabel: TLabel;
    ExtraLineSpacingComboBox: TComboBox;
    DisplayPreview:    TPreviewEditor;

    // Key Mappings
    KeyMappingChooseSchemeButton: TButton;
    KeyMappingHelpLabel: TLabel;
    KeyMappingTreeView:  TTreeView;
    KeyMappingConsistencyCheckButton: TButton;

    // Color options
    LanguageComboBox: TComboBox;
    LanguageLabel: TLabel;
    FileExtensionsComboBox: TComboBox;
    FileExtensionsLabel: TLabel;
    ColorSchemeComboBox: TComboBox;
    ColorSchemeLabel: TLabel;
    ColorElementLabel: TLabel;
    ColorElementListBox: TListBox;
    TextAttributesGroupBox: TGroupBox;
    TextBoldCheckBox: TCheckBox;
    TextItalicCheckBox: TCheckBox;
    TextUnderlineCheckBox: TCheckBox;
    ForeGroundGroupBox: TGroupBox;
    ForeGroundColorButton: TColorButton;
    ForeGroundUseDefaultCheckBox: TCheckBox;
    BackGroundGroupBox: TGroupBox;
    BackGroundColorButton: TColorButton;
    BackGroundUseDefaultCheckBox: TCheckBox;
    SetAttributeToDefaultButton: TButton;
    SetAllAttributesToDefaultButton: TButton;
    ColorPreview:     TPreviewEditor;

    // Code Tools options
    AutomaticFeaturesGroupBox: TGroupBox;
    AutoIdentifierCompletionCheckBox: TCheckBox;
    AutoCodeParametersCheckBox: TCheckBox;
    AutoToolTipExprEvalCheckBox: TCheckBox;
    AutoToolTipSymbToolsCheckBox: TCheckBox;
    AutoDelayLabel: TLabel;
    AutoDelayTrackBar: TTrackBar;
    AutoDelayMinLabel: TLabel;
    AutoDelayMaxLabel: TLabel;
    
    // Code Folding
    Bevel1: TBevel;
    chkCodeFoldingEnabled: TCheckBox;
    lblDividerDrawLevel: TLabel;
    edDividerDrawLevel: TSpinEdit;

    // general
    procedure GeneralCheckBoxOnChange(Sender: TObject; Index: integer);
    procedure ComboBoxOnChange(Sender: TObject);
    procedure ComboBoxOnExit(Sender: TObject);
    procedure ComboBoxOnKeyDown(Sender: TObject;
                                var Key: Word; Shift: TShiftState);
    procedure ColorButtonColorChanged(Sender: TObject);

    // display
    procedure FontDialogApplyClicked(Sender: TObject);
    procedure EditorFontComboBoxEditingDone(Sender: TObject);
    procedure EditorFontButtonClick(Sender: TObject);
    procedure RightMarginColorButtonColorChanged(Sender: TObject);

    // key mapping
    procedure KeyMappingChooseSchemeButtonClick(Sender: TObject);
    procedure KeyMappingTreeViewMouseUp(Sender: TObject;
                      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure KeyMappingConsistencyCheckButtonClick(Sender: TObject);

    // color
    procedure ColorElementListBoxSelectionChange(Sender: TObject; User: Boolean);
    procedure ColorPreviewMouseUp(Sender: TObject; Button: TMouseButton;
                                  Shift: TShiftState; X, Y: Integer);
    procedure OnSpecialLineColors(Sender: TObject; Line: Integer;
                                  var Special: Boolean; var FG, BG: TColor);
    procedure SetAttributeToDefaultButtonClick(Sender: TObject);
    procedure SetAllAttributesToDefaultButtonClick(Sender: TObject);

    // code tools

    // Code Folding
    procedure chkCodeFoldingEnabledChange(Sender: TObject);

    // buttons at bottom
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private
    FormCreating: Boolean;
    PreviewSyn:   TCustomSyn;
    PreviewEdits: array[1..2] of TPreviewEditor;
    CurLanguageID: Integer;
    // current index in EditorOpts.EditOptHighlighterList
    CurHighlightElement: TSynHighlightElement;
    UpdatingColor: Boolean;
    fHighlighterList: TStringList; // list of "ColorScheme" Data=TCustomSyn
    fColorSchemes: TStringList;    // list of LanguageName=ColorScheme
    fFileExtensions: TStringList;  // list of LanguageName=FileExtensions
    EditingKeyMap: TKeyCommandRelationList;

    procedure SetComboBoxText(AComboBox: TComboBox; const AText: String);
    procedure FontDialogNameToFont(FontDialogName: String; AFont: TFont);
    procedure InvalidatePreviews;
    procedure SetPreviewSynInAllPreviews;
    procedure SetupButtonBar;

    // general
    procedure SetupGeneralPage(Page: Integer);

    // display
    procedure SetupDisplayPage(Page: Integer);

    // keymapping
    procedure SetupKeyMappingsPage(Page: Integer);
    function KeyMappingRelationToString(Index: Integer): String;
    function KeyMappingRelationToString(KeyRelation:
                                        TKeyCommandRelation): String;
    procedure FillKeyMappingTreeView;

    // color
    procedure SetupColorPage(Page: Integer);
    procedure ShowCurAttribute;
    procedure FindCurHighlightElement;
    function GetHighlighter(SynClass: TCustomSynClass;
      const ColorScheme: String; CreateIfNotExists: Boolean): TCustomSyn;
    procedure ClearHighlighters;
    procedure SaveAllHighlighters;
    procedure FillColorElementListBox;
    function GetCurColorScheme(const LanguageName: String): String;
    procedure SetCurColorScheme(const LanguageName, ColorScheme: String);
    procedure SaveAllColorSchemes;
    function GetCurFileExtensions(const LanguageName: String): String;
    procedure SetCurFileExtensions(const LanguageName, FileExtensions: String);
    procedure SaveAllFileExtensions;
    procedure SetColorElementsToDefaults(OnlySelected: Boolean);

    // code tools
    procedure SetupCodeToolsPage(Page: Integer);
    
    // Code Folding
    procedure SetupCodeFoldingPage(Page: integer);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation


const
  ValidAttribChars = ['a'..'z', 'A'..'Z', '_', '0'..'9'];

  DefaultColorScheme      = 'Default';


function CheckGroupItemChecked(CheckGroup: TCheckGroup;
  const Caption: string): Boolean;
begin
  Result:=CheckGroup.Checked[CheckGroup.Items.IndexOf(Caption)];
end;


function StrToValidXMLName(const s: String): String;
var
  i: Integer;
begin
  Result := s;
  // replace invalid characters
  for i := 1 to length(Result) do
    if (not (Result[i] in ValidAttribChars)) then
      Result[i] := '_';
end;

procedure CopyHiLightAttributeValues(Src, Dest: TSynHighlightElement);
begin
  Dest.Background := Src.Background;
  Dest.Foreground := Src.Foreground;
  Dest.Style      := Src.Style;
end;

{ TEditorOptionsFormNew }

constructor TEditorOptionsFormNew.Create(TheOwner: TComponent);
var
  a: Integer;
  s: String;
  i: Integer;
  Title: string;
begin
  inherited Create(TheOwner);
  FormCreating := True;
  Caption      := lismenueditoroptions;

  IDEDialogLayoutList.ApplyLayout(Self, 480, 480);

  NoteBook.PageIndex := 0;

  SetupGeneralPage(0);
  SetupDisplayPage(1);
  SetupKeyMappingsPage(2);
  SetupColorPage(3);
  SetupCodeToolsPage(4);
  SetupCodeFoldingPage(5);
  SetupButtonBar;

  UpdatingColor := False;
  CurHighlightElement := Nil;

  // create a temporary copy of the keymap for editing
  EditingKeyMap := TKeyCommandRelationList.Create;
  EditingKeyMap.Assign(EditorOpts.KeyMap);

  // initialize previews
  for a := Low(PreviewEdits) to High(PreviewEdits) do
    PreviewEdits[a] := Nil;
  s := GetCurColorScheme(TPreviewPasSyn.GetLanguageName);
  PreviewSyn := GetHighlighter(TPreviewPasSyn, s, True);
  CurLanguageID := EditorOpts.HighlighterList.FindByClass(
    TCustomSynClass(PreviewSyn.ClassType));

  PreviewEdits[1] := DisplayPreview;
  PreviewEdits[2] := ColorPreview;
  for a := Low(PreviewEdits) to High(PreviewEdits) do
    if PreviewEdits[a] <> Nil then
      with PreviewEdits[a] do
      begin
        if EditorOpts.UseSyntaxHighlight then
          Highlighter := PreviewSyn;
        EditorOpts.GetSynEditSettings(PreviewEdits[a]);
        EditingKeyMap.AssignTo(PreviewEdits[a].KeyStrokes,
          TSourceEditorWindowInterface);
        if a <> 3 then
        begin
          Lines.Text := EditorOpts.HighlighterList[CurLanguageID].SampleSource;
          PreviewEdits[a].Options :=
            PreviewEdits[a].Options + [eoNoCaret,
            eoNoSelection] - [eoBracketHighlight];
        end;
      end;

  // general options

  // display options

  // key mappings
  FillKeyMappingTreeView;

  // color options
  LanguageComboBox.Text := PreviewSyn.LanguageName;
  SetComboBoxText(LanguageComboBox, LanguageComboBox.Text);
  ColorSchemeComboBox.Text := GetCurColorScheme(PreviewSyn.LanguageName);
  SetComboBoxText(ColorSchemeComboBox, ColorSchemeComboBox.Text);
  FillColorElementListBox;
  FindCurHighlightElement;
  ShowCurAttribute;

  // code Tools options

  NoteBook.PageIndex := 0;
  FormCreating := False;
  //Indexing item
  for i:= 0 to NoteBook.PageCount-1 do
  begin
    Title:=NoteBook.Pages[i];
    ScanControlTextsForIndex(Title, NoteBook.Page[i]);
  end
end;

destructor TEditorOptionsFormNew.Destroy;
begin
  ClearHighlighters;
  fColorSchemes.Free;
  fFileExtensions.Free;
  EditingKeyMap.Free;
  inherited Destroy;
end;


// general

procedure TEditorOptionsFormNew.GeneralCheckBoxOnChange(Sender: TObject; Index: integer);
var
  a: Integer;
  NewColor: TColor;
  i: LongInt;

  procedure SetOption(const CheckBoxName: String; AnOption: TSynEditorOption);
  var
    a: Integer;
    i: LongInt;
  begin
    i:=EditorOptionsGroupBox.Items.IndexOf(CheckBoxName);
    if i<0 then begin
      DebugLn(['TEditorOptionsFormNew.GeneralCheckBoxOnChange.SetOption i<0']);
      exit;
    end;
    for a := Low(PreviewEdits) to High(PreviewEdits) do
      if PreviewEdits[a] <> Nil then
        if EditorOptionsGroupBox.Checked[i] then
          PreviewEdits[a].Options := PreviewEdits[a].Options + [AnOption]
        else
          PreviewEdits[a].Options := PreviewEdits[a].Options - [AnOption];
  end;

  procedure SetOption2(const CheckBoxName: String; AnOption: TSynEditorOption2);
  var
    a: Integer;
    i: LongInt;
  begin
    i:=EditorOptionsGroupBox.Items.IndexOf(CheckBoxName);
    if i<0 then exit;
    for a := Low(PreviewEdits) to High(PreviewEdits) do
      if PreviewEdits[a] <> Nil then
        if EditorOptionsGroupBox.Checked[i] then
          PreviewEdits[a].Options2 := PreviewEdits[a].Options2 + [AnOption]
        else
          PreviewEdits[a].Options2 := PreviewEdits[a].Options2 - [AnOption];
  end;

  // GeneralCheckBoxOnChange
begin
  if FormCreating then
    exit;
  // general
  SetOption(dlgAltSetClMode, eoAltSetsColumnMode);
  SetOption(dlgAutoIdent, eoAutoIndent);
  // not for Preview: SetOption(dlgBracHighlight,eoBracketHighlight);
  SetOption(dlgDoubleClickLine, eoDoubleClickSelectsLine);
  SetOption(dlgDragDropEd, eoDragDropEditing);
  SetOption(dlgDropFiles, eoDropFiles);
  SetOption(dlgGroupUndo, eoGroupUndo);
  SetOption(dlgHomeKeyJumpsToNearestStart, eoEnhanceHomeKey);
  SetOption(dlgHalfPageScroll, eoHalfPageScroll);
  SetOption(dlgKeepCaretX, eoKeepCaretX);
  SetOption(dlgPersistentCaret, eoPersistentCaret);
  SetOption(dlgRightMouseMovesCursor, eoRightMouseMovesCursor);
  // not for Preview: SetOption('NoSelectionCheckBox',eoNoSelection);
  SetOption(dlgScrollByOneLess, eoScrollByOneLess);
  SetOption(dlgScrollPastEndFile, eoScrollPastEoF);
  SetOption(dlgScrollPastEndLine, eoScrollPastEoL);
  SetOption(dlgShowScrollHint, eoShowScrollHint);
  SetOption(dlgSmartTabs, eoSmartTabs);
  SetOption(dlgTabsToSpaces, eoTabsToSpaces);
  SetOption(dlgTabIndent, eoTabIndent);
  SetOption(dlgTrimTrailingSpaces, eoTrimTrailingSpaces);
  
  SetOption2(dlgCaretSkipsSelection, eoCaretSkipsSelection);
  SetOption2(dlgAlwaysVisibleCaret, eoAlwaysVisibleCaret);

  for a := Low(PreviewEdits) to High(PreviewEdits) do
    if PreviewEdits[a] <> Nil then
    begin
      // general
      i:=EditorOptionsGroupBox.Items.IndexOf(dlgUseSyntaxHighlight);
      if EditorOptionsGroupBox.Checked[i] then
        PreviewEdits[a].Highlighter := PreviewSyn
      else
        PreviewEdits[a].Highlighter := Nil;
      // display
      if (a in [1, 2]) then
        PreviewEdits[a].Gutter.Visible := VisibleGutterCheckBox.Checked;
      PreviewEdits[a].Gutter.ShowLineNumbers := ShowLineNumbersCheckBox.Checked;
      PreviewEdits[a].RightEdgeColor:=RightMarginColorButton.ButtonColor;
      if VisibleRightMarginCheckBox.Checked then
        PreviewEdits[a].RightEdge:=StrToIntDef(RightMarginComboBox.Text,80)
      else
        PreviewEdits[a].RightEdge:=0;
    end;
  if CurHighlightElement <> Nil then
  begin
    if Sender = ForeGroundUseDefaultCheckBox then
      if UpdatingColor = False then
      begin
        UpdatingColor := True;
        if not ForeGroundUseDefaultCheckBox.Checked then
          NewColor := ForeGroundColorButton.ButtonColor
        else
          NewColor := clNone;
        ForeGroundColorButton.Visible := not
          ForeGroundUseDefaultCheckBox.Checked;
        if NewColor <> CurHighlightElement.Foreground then
        begin
          CurHighlightElement.Foreground := NewColor;
          InvalidatePreviews;
        end;
        UpdatingColor := False;
      end;
    if Sender = BackGroundUseDefaultCheckBox then
      if UpdatingColor = False then
      begin
        if not BackGroundUseDefaultCheckBox.Checked then
          NewColor := BackGroundColorButton.ButtonColor
        else
          NewColor := clNone;
        BackGroundColorButton.Visible := not
          BackGroundUseDefaultCheckBox.Checked;
        if NewColor <> CurHighlightElement.Background then
        begin
          CurHighlightElement.Background := NewColor;
          InvalidatePreviews;
        end;
      end;
    if Sender = TextBoldCheckBox then
      if TextBoldCheckBox.Checked xor (fsBold in CurHighlightElement.Style) then
      begin
        if TextBoldCheckBox.Checked then
          CurHighlightElement.Style := CurHighlightElement.Style + [fsBold]
        else
          CurHighlightElement.Style := CurHighlightElement.Style - [fsBold];
        InvalidatePreviews;
      end;
    if Sender = TextItalicCheckBox then
      if TextItalicCheckBox.Checked then
      begin
        if not (fsItalic in CurHighlightElement.Style) then
        begin
          CurHighlightElement.Style := CurHighlightElement.Style + [fsItalic];
          InvalidatePreviews;
        end;
      end
      else
      if (fsItalic in CurHighlightElement.Style) then
      begin
        CurHighlightElement.Style := CurHighlightElement.Style - [fsItalic];
        InvalidatePreviews;
      end;
    if Sender = TextUnderlineCheckBox then
      if TextUnderlineCheckBox.Checked then
      begin
        if not (fsUnderline in CurHighlightElement.Style) then
        begin
          CurHighlightElement.Style := CurHighlightElement.Style + [fsUnderline];
          InvalidatePreviews;
        end;
      end
      else
      if (fsUnderline in CurHighlightElement.Style) then
      begin
        CurHighlightElement.Style := CurHighlightElement.Style - [fsUnderline];
        InvalidatePreviews;
      end;
  end;
end;

procedure TEditorOptionsFormNew.chkCodeFoldingEnabledChange(Sender: TObject);
begin
  lblDividerDrawLevel.Enabled := chkCodeFoldingEnabled.Checked;
  edDividerDrawLevel.Enabled  := chkCodeFoldingEnabled.Checked;
end;

procedure TEditorOptionsFormNew.EditorFontComboBoxEditingDone(Sender: TObject);
var
  i: Integer;
begin
  for i := Low(PreviewEdits) to High(PreviewEdits) do
    if PreviewEdits[i] <> Nil then
      PreviewEdits[i].Font.Name:=EditorFontComboBox.Text;
end;

procedure TEditorOptionsFormNew.ColorButtonColorChanged(Sender: TObject);
var
  a: Integer;
begin
  if FormCreating then
    exit;
  if Sender = ForeGroundColorButton then
  begin
    if (CurHighlightElement = Nil) or UpdatingColor then
      exit;
    if not ForeGroundUseDefaultCheckBox.Checked then
    begin
      CurHighlightElement.Foreground := ForeGroundColorButton.ButtonColor;
      InvalidatePreviews;
    end;
  end;
  if Sender = BackGroundColorButton then
  begin
    if (CurHighlightElement = Nil) or UpdatingColor then
      exit;
    if not BackGroundUseDefaultCheckBox.Checked then
    begin
      CurHighlightElement.Background := BackGroundColorButton.ButtonColor;
      InvalidatePreviews;
    end;
  end;
  if Sender = GutterColorButton then
    for a := Low(PreviewEdits) to High(PreviewEdits) do
      if PreviewEdits[a] <> Nil then
      begin
        PreviewEdits[a].Gutter.Color := GutterColorButton.ButtonColor;
        PreviewEdits[a].Invalidate;
      end;
end;

procedure TEditorOptionsFormNew.FontDialogNameToFont(FontDialogName: String;
  AFont: TFont);
var
  TmpFont: TFont;
  p, p2, index: Integer;
  s: shortstring;
begin
  TmpFont := TFont.Create;
  TmpFont.Assign(AFont);
  try
    p := 1;
    p2 := 0;
    index := 1;
    while (p <= length(FontDialogName)) do
    begin
      if (FontDialogName[p] = '-') then
      begin
        s := copy(FontDialogName, p2 + 1, p - p2 - 1);
        p2 := p;
        case Index of
          3:
            TmpFont.Name := s;
          //8:TmpFont.Height:=StrToIntDef(s,TmpFont.Height);
        end;
        inc(Index);
      end;
      inc(p);
    end;
    AFont.Assign(TmpFont);
  finally
    TmpFont.Free;
  end;
end;

procedure TEditorOptionsFormNew.FontDialogApplyClicked(Sender: TObject);
var
  a: Integer;
begin
  for a := Low(PreviewEdits) to High(PreviewEdits) do
    if PreviewEdits[a] <> Nil then
      PreviewEdits[a].Font.Assign(TFontDialog(Sender).Font);
  EditorFontComboBox.Text := DisplayPreview.Font.Name;
  SetComboBoxText(EditorFontHeightComboBox,
                  IntToStr(DisplayPreview.Font.Height));
end;

procedure TEditorOptionsFormNew.EditorFontButtonClick(Sender: TObject);
var
  FontDialog: TFontDialog;
begin
  FontDialog := TFontDialog.Create(Nil);
  try
    with FontDialog do
    begin
      Font.Name   := EditorFontComboBox.Text;
      Font.Height := StrToIntDef(EditorFontHeightComboBox.Text, PreviewEdits[1].Font.Height);
      Options := Options + [fdApplyButton];
      OnApplyClicked := @FontDialogApplyClicked;
      if Execute then
        FontDialogApplyClicked(FontDialog);
    end;
  finally
    FontDialog.Free;
  end;
end;

procedure TEditorOptionsFormNew.KeyMappingChooseSchemeButtonClick(
  Sender: TObject);
var
  NewScheme: String;
begin
  if ShowChooseKeySchemeDialog(NewScheme) <> mrOk then
    exit;
  EditingKeyMap.LoadScheme(NewScheme);
  FillKeyMappingTreeView;
end;

procedure TEditorOptionsFormNew.ComboBoxOnExit(Sender: TObject);
var
  NewVal, a: Integer;
  Box: TComboBox;
begin
  if FormCreating then
    exit;
  Box := TComboBox(Sender);
  if PreviewEdits[1] <> Nil then
    if Sender = BlockIndentComboBox then
    begin
      NewVal := StrToIntDef(BlockIndentComboBox.Text,
        PreviewEdits[1].BlockIndent);
      SetComboBoxText(BlockIndentComboBox, IntToStr(NewVal));
      for a := Low(PreviewEdits) to High(PreviewEdits) do
        if PreviewEdits[a] <> Nil then
          PreviewEdits[a].BlockIndent := NewVal;
    end
    else
    if Sender = TabWidthsComboBox then
    begin
      NewVal := StrToIntDef(TabWidthsComboBox.Text,
        PreviewEdits[1].TabWidth);
      SetComboBoxText(TabWidthsComboBox, IntToStr(NewVal));
      for a := Low(PreviewEdits) to High(PreviewEdits) do
        if PreviewEdits[a] <> Nil then
          PreviewEdits[a].TabWidth := NewVal;
    end
    // display
    else
    if Sender = EditorFontHeightComboBox then
    begin
      NewVal := StrToIntDef(EditorFontHeightComboBox.Text,
        PreviewEdits[1].Font.Height);
      if (NewVal < 0) then
        if (NewVal > -6) then
          NewVal := -6;
      if (NewVal >= 0) then
        if (NewVal < 6) then
          NewVal := 6;
      if (NewVal > 40) then
        NewVal := 40;
      if (NewVal < -40) then
        NewVal := -40;
      SetComboBoxText(EditorFontHeightComboBox, IntToStr(NewVal));
      for a := Low(PreviewEdits) to High(PreviewEdits) do
        if PreviewEdits[a] <> Nil then
          PreviewEdits[a].Font.Height := NewVal;
    end
    else
    if Sender = ExtraLineSpacingComboBox then
    begin
      NewVal := StrToIntDef(ExtraLineSpacingComboBox.Text,
        PreviewEdits[1].ExtraLineSpacing);
      SetComboBoxText(ExtraLineSpacingComboBox, IntToStr(NewVal));
      for a := Low(PreviewEdits) to High(PreviewEdits) do
        if PreviewEdits[a] <> Nil then
          PreviewEdits[a].ExtraLineSpacing := NewVal;
    end
    else
    if Sender = GutterWidthComboBox then
    begin
      NewVal := StrToIntDef(GutterWidthComboBox.Text,
        PreviewEdits[1].Gutter.Width);
      SetComboBoxText(GutterWidthComboBox, IntToStr(NewVal));
      for a := Low(PreviewEdits) to High(PreviewEdits) do
        if PreviewEdits[a] <> Nil then
          PreviewEdits[a].Gutter.Width := NewVal;
    end
    else
    if Sender = RightMarginComboBox then
    begin
      NewVal := StrToIntDef(RightMarginComboBox.Text,PreviewEdits[1].RightEdge);
      SetComboBoxText(RightMarginComboBox, IntToStr(NewVal));
      for a := Low(PreviewEdits) to High(PreviewEdits) do
        if PreviewEdits[a] <> Nil then begin
          if VisibleRightMarginCheckBox.Checked then
            PreviewEdits[a].RightEdge := NewVal
          else
            PreviewEdits[a].RightEdge := 0;
        end;
    end
    // color
    else
    if Sender = ColorSchemeComboBox then
    begin
      if Box.Items.IndexOf(Box.Text) < 0 then
        SetComboBoxText(Box, GetCurColorScheme(PreviewSyn.LanguageName))
        // unknown color scheme -> switch back
      else
      if Box.Text <> GetCurColorScheme(PreviewSyn.LanguageName) then
      begin
        SetCurColorScheme(PreviewSyn.LanguageName, Box.Text);
        SetComboBoxText(Box, Box.Text);
        PreviewSyn := GetHighlighter(TCustomSynClass(PreviewSyn.ClassType),
          Box.Text, True);
        SetPreviewSynInAllPreviews;
        FillColorElementListBox;
        FindCurHighlightElement;
      end// change the colorscheme
      ;
    end
    else
    if Sender = FileExtensionsComboBox then
    begin
      //DebugLn(['TEditorOptionsFormNew.ComboBoxOnExit Box.Text="',Box.Text,'" Old="',GetCurFileExtensions(PreviewSyn.LanguageName),'" PreviewSyn.LanguageName=',PreviewSyn.LanguageName]);
      if Box.Text <> GetCurFileExtensions(PreviewSyn.LanguageName) then
      begin
        SetCurFileExtensions(PreviewSyn.LanguageName, Box.Text);
        SetComboBoxText(Box, Box.Text);
      end;
      //DebugLn(['TEditorOptionsFormNew.ComboBoxOnExit Box.Text="',Box.Text,'" Now="',GetCurFileExtensions(PreviewSyn.LanguageName),'" PreviewSyn.LanguageName=',PreviewSyn.LanguageName]);
    end
    else
    if Sender = LanguageComboBox then
      if Box.Items.IndexOf(Box.Text) < 0 then
        SetComboBoxText(Box, PreviewSyn.LanguageName)// unknown language -> switch back
      else
      if Box.Text <> PreviewSyn.LanguageName then
      begin
        NewVal := EditorOpts.HighlighterList.FindByName(Box.Text);
        if NewVal >= 0 then
        begin
          SetComboBoxText(Box, Box.Text);
          CurLanguageID := NewVal;
          PreviewSyn    := GetHighlighter(
            EditorOpts.HighlighterList[CurLanguageID].SynClass,
            GetCurColorScheme(
            EditorOpts.HighlighterList[
            CurLanguageID].SynClass.GetLanguageName)
            , True);
          SetComboBoxText(ColorSchemeComboBox,
            GetCurColorScheme(PreviewSyn.LanguageName));
          SetComboBoxText(FileExtensionsComboBox,
            GetCurFileExtensions(PreviewSyn.LanguageName));
          for a := Low(PreviewEdits) to High(PreviewEdits) do
            if a <> 3 then
              PreviewEdits[a].Lines.Text :=
                EditorOpts.HighlighterList[CurLanguageID].SampleSource;
          SetPreviewSynInAllPreviews;
          FillColorElementListBox;
          FindCurHighlightElement;
        end;
      end// change language
    // general
  ;
end;

procedure TEditorOptionsFormNew.ComboBoxOnKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key = VK_S) then
    ComboBoxOnExit(Sender);
end;

procedure TEditorOptionsFormNew.ComboBoxOnChange(Sender: TObject);
var
  ComboBox: TComboBox;
begin
  ComboBox := TComboBox(Sender);
  if ComboBox.Items.IndexOf(ComboBox.Text) >= 0 then
    ComboBoxOnExit(Sender);
end;

procedure TEditorOptionsFormNew.FindCurHighlightElement;
var
  a, i: Integer;
  Old:  TSynHighlightElement;
begin
  Old := CurHighlightElement;
  CurHighlightElement := Nil;
  a   := ColorElementListBox.ItemIndex;
  if (a >= 0) then
  begin
    i := PreviewSyn.AttrCount - 1;
    while (i >= 0) do
    begin
      if ColorElementListBox.Items[a] = PreviewSyn.Attribute[i].Name then
      begin
        CurHighlightElement := PreviewSyn.Attribute[i];
        break;
      end;
      dec(i);
    end;
  end;
  if Old <> CurHighlightElement then
    ShowCurAttribute;
end;

procedure TEditorOptionsFormNew.InvalidatePreviews;
var
  a: Integer;
begin
  for a := Low(PreviewEdits) to High(PreviewEdits) do
    if PreviewEdits[a] <> Nil then
      PreviewEdits[a].Invalidate;
end;

procedure TEditorOptionsFormNew.SetPreviewSynInAllPreviews;
var
  a: Integer;
begin
  for a := Low(PreviewEdits) to High(PreviewEdits) do
    if PreviewEdits[a] <> Nil then
      if EditorOpts.UseSyntaxHighlight then
        PreviewEdits[a].Highlighter := PreviewSyn
      else
        PreviewEdits[a].Highlighter := Nil;
end;

procedure TEditorOptionsFormNew.ShowCurAttribute;
begin
  if (CurHighlightElement = Nil) or UpdatingColor then
    exit;
  UpdatingColor := True;
  TextBoldCheckBox.Checked := fsBold in CurHighlightElement.Style;
  TextItalicCheckBox.Checked := fsItalic in CurHighlightElement.Style;
  TextUnderlineCheckBox.Checked := fsUnderline in CurHighlightElement.Style;
  if CurHighlightElement.Foreground = clNone then
    ForeGroundUseDefaultCheckBox.Checked := True
  else
  begin
    ForeGroundUseDefaultCheckBox.Checked := False;
    ForeGroundColorButton.ButtonColor    := CurHighlightElement.Foreground;
  end;
  ForeGroundColorButton.Visible := not ForeGroundUseDefaultCheckBox.Checked;
  if CurHighlightElement.Background = clNone then
    BackGroundUseDefaultCheckBox.Checked := True
  else
  begin
    BackGroundUseDefaultCheckBox.Checked := False;
    BackGroundColorButton.ButtonColor    := CurHighlightElement.Background;
  end;
  BackGroundColorButton.Visible := not BackGroundUseDefaultCheckBox.Checked;
  UpdatingColor := False;
end;

procedure TEditorOptionsFormNew.KeyMappingTreeViewMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
  ARelation: TKeyCommandRelation;
  ANode: TTreeNode;
begin
  ANode := KeyMappingTreeView.GetNodeAt(X, Y);
  if (ANode <> Nil) and (ANode.Data <> Nil) and
    (TObject(ANode.Data) is TKeyCommandRelation) then
  begin
    ARelation := TKeyCommandRelation(ANode.Data);
    i := EditingKeyMap.IndexOf(ARelation);
    if (i >= 0) and (ShowKeyMappingEditForm(i, EditingKeyMap) = mrOk) then
    begin
      FillKeyMappingTreeView;
      for i := Low(PreviewEdits) to High(PreviewEdits) do
        if PreviewEdits[i] <> Nil then
          EditingKeyMap.AssignTo(PreviewEdits[i].KeyStrokes,
            TSourceEditorWindowInterface);
    end;
  end;
end;

type
  TKeyMapErrorsForm = class(TForm)
    ListBox: TListBox;
    BackButton: TButton;
    procedure BackButtonClick(Sender: TObject);
  public
    constructor Create(AnOwner: TComponent); override;
  end;

constructor TKeyMapErrorsForm.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  if LazarusResources.Find(ClassName) = Nil then
  begin
    SetBounds((Screen.Width - 410) div 2, (Screen.Height - 260) div 2, 400, 250);
    Caption := dlgKeyMappingErrors;

    ListBox := TListBox.Create(Self);
    with ListBox do
    begin
      Name := 'ListBox';
      Parent := Self;
      Left := 0;
      Top  := 0;
      Width := Self.ClientWidth - 4;
      Height := Self.ClientHeight - 50;
    end;

    BackButton := TButton.Create(Self);
    with BackButton do
    begin
      Name := 'BackButton';
      Parent := Self;
      Width := 60;
      Height := 25;
      Caption := dlgEdBack;
      Left := ((Self.ClientWidth - 4) - Width) div 2;
      Top  := Self.ClientHeight - 38;
      OnClick := @BackButtonClick;
    end;
  end;
end;

procedure TKeyMapErrorsForm.BackButtonClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TEditorOptionsFormNew.KeyMappingConsistencyCheckButtonClick(
  Sender: TObject);
var
  Protocol: TStringList;
  ErrorCount, Index1, Index2: Integer;
  ACaption, AText: String;
  KeyMapErrorsForm: TKeyMapErrorsForm;
begin
  Protocol := TStringList.Create;
  try
    ErrorCount := FindKeymapConflicts(EditingKeyMap, Protocol, Index1, Index2);
    if ErrorCount > 0 then
    begin
      KeyMapErrorsForm := TKeyMapErrorsForm.Create(Nil);
      try
        KeyMapErrorsForm.ListBox.Items.Assign(Protocol);
        KeyMapErrorsForm.ShowModal;
      finally
        KeyMapErrorsForm.Free;
      end;
    end
    else
    begin
      ACaption := dlgReport;
      AText    := dlgEdNoErr;
      MessageDlg(ACaption, AText, mtInformation, [mbOk], 0);
    end;
  finally
    Protocol.Free;
  end;
end;

procedure TEditorOptionsFormNew.ColorElementListBoxSelectionChange(
  Sender: TObject;
  User: Boolean);
begin
  FindCurHighlightElement;
end;

procedure TEditorOptionsFormNew.FillColorElementListBox;
var
  i: Integer;
begin
  with ColorElementListBox.Items do
  begin
    BeginUpdate;
    Clear;

    for i := 0 to PreviewSyn.AttrCount - 1 do
      if PreviewSyn.Attribute[i].Name <> '' then
        Add(PreviewSyn.Attribute[i].Name);
    EndUpdate;
  end;

  CurHighlightElement := Nil;
  if ColorElementListBox.Items.Count > 0 then
    ColorElementListBox.Selected[0] := True;
  FindCurHighlightElement;
end;

procedure TEditorOptionsFormNew.ColorPreviewMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  NewIndex: Integer;
  Token:    String;
  Attri:    TSynHighlightElement;
  MouseXY, XY: TPoint;
  AddAttr:  TAdditionalHilightAttribute;
begin
  MouseXY := Point(X, Y);
  XY      := ColorPreview.PixelsToRowColumn(MouseXY);
  NewIndex := -1;
  if CurLanguageID >= 0 then
  begin
    AddAttr := EditorOpts.HighlighterList[
      CurLanguageID].SampleLineToAddAttr(XY.Y);
    if AddAttr <> ahaNone then
      NewIndex := ColorElementListBox.Items.IndexOf(
        AdditionalHighlightAttributes[AddAttr]);
  end;
  if NewIndex < 0 then
  begin
    ColorPreview.GetHighlighterAttriAtRowCol(XY, Token, Attri);
    if Attri = Nil then
      Attri := PreviewSyn.WhitespaceAttribute;
    if Attri <> Nil then
      NewIndex := ColorElementListBox.Items.IndexOf(Attri.Name);
  end;
  if NewIndex >= 0 then
  begin
    ColorElementListBox.ItemIndex := NewIndex;
    FindCurHighlightElement;
  end;
end;

procedure TEditorOptionsFormNew.OnSpecialLineColors(Sender: TObject;
  Line: Integer; var Special: Boolean; var FG, BG: TColor);
var
  e: TSynHighlightElement;
  AddAttr: TAdditionalHilightAttribute;
  i: Integer;
begin
  if CurLanguageID >= 0 then
  begin
    AddAttr := EditorOpts.HighlighterList[CurLanguageID].SampleLineToAddAttr(Line);
    if AddAttr <> ahaNone then
    begin
      i := PreviewSyn.AttrCount - 1;
      while (i >= 0) do
      begin
        e := PreviewSyn.Attribute[i];
        if e.Name = '' then
          continue;
        if e.Name = AdditionalHighlightAttributes[AddAttr] then
        begin
          Special := (e.ForeGround <> clNone) or (e.BackGround <> clNone);
          if e.ForeGround <> clNone then
            FG := e.ForeGround;
          if e.BackGround <> clNone then
            BG := e.BackGround;
          exit;
        end;
        dec(i);
      end;
    end;
  end;
end;

procedure TEditorOptionsFormNew.RightMarginColorButtonColorChanged(Sender: TObject
  );
var
  a: Integer;
begin
  for a := Low(PreviewEdits) to High(PreviewEdits) do
    if PreviewEdits[a] <> Nil then
      PreviewEdits[a].RightEdgeColor:=RightMarginColorButton.ButtonColor;
end;

procedure TEditorOptionsFormNew.SetAttributeToDefaultButtonClick(Sender: TObject);
begin
  SetColorElementsToDefaults(True);
end;

procedure TEditorOptionsFormNew.SetAllAttributesToDefaultButtonClick(
  Sender: TObject);
begin
  SetColorElementsToDefaults(False);
end;

procedure TEditorOptionsFormNew.SetColorElementsToDefaults(OnlySelected: Boolean);
var
  DefaultSyn: TCustomSyn;
  PascalSyn: TPreviewPasSyn;
  i, j: Integer;
  CurSynClass: TCustomSynClass;
begin
  PascalSyn := TPreviewPasSyn(GetHighlighter(TPreviewPasSyn,
    ColorSchemeComboBox.Text, True));
  CurSynClass := TCustomSynClass(PreviewSyn.ClassType);
  DefaultSyn := CurSynClass.Create(Nil);
  try
    EditorOpts.AddSpecialHilightAttribsToHighlighter(DefaultSyn);
    EditorOpts.ReadDefaultsForHighlighterSettings(DefaultSyn,
      ColorSchemeComboBox.Text, PascalSyn);
    for i := 0 to DefaultSyn.AttrCount - 1 do
    begin
      if DefaultSyn.Attribute[i].Name = '' then
        continue;
      if OnlySelected then
      begin
        if (DefaultSyn.Attribute[i].Name = CurHighlightElement.Name) then
          CopyHiLightAttributeValues(DefaultSyn.Attribute[i],
            CurHighlightElement);
      end
      else
        for j := 0 to PreviewSyn.AttrCount - 1 do
          if PreviewSyn.Attribute[j].Name = DefaultSyn.Attribute[i].Name then
            CopyHiLightAttributeValues(DefaultSyn.Attribute[i],
              PreviewSyn.Attribute[j]);
    end;
  finally
    DefaultSyn.Free;
  end;
  ShowCurAttribute;
end;

function TEditorOptionsFormNew.GetCurColorScheme(
  const LanguageName: String): String;
begin
  if fColorSchemes = Nil then
    Result := ''
  else
    Result := fColorSchemes.Values[LanguageName];
  if Result = '' then
    Result := EditorOpts.ReadColorScheme(LanguageName);
end;

procedure TEditorOptionsFormNew.SetCurColorScheme(
  const LanguageName, ColorScheme: String);
begin
  if fColorSchemes = Nil then
    fColorSchemes := TStringList.Create;
  fColorSchemes.Values[LanguageName] := ColorScheme;
end;

procedure TEditorOptionsFormNew.SaveAllColorSchemes;
var
  i: Integer;
begin
  if fColorSchemes = Nil then
    exit;
  for i := 0 to fColorSchemes.Count - 1 do
    EditorOpts.WriteColorScheme(fColorSchemes.Names[i],
      fColorSchemes.Values[fColorSchemes.Names[i]]);
end;

function TEditorOptionsFormNew.GetCurFileExtensions(
  const LanguageName: String): String;
var
  i: Integer;
begin
  if fFileExtensions = Nil then
    Result := ''
  else
    Result := fFileExtensions.Values[LanguageName];
  if Result = '' then
  begin
    i := EditorOpts.HighlighterList.FindByName(LanguageName);
    if i >= 0 then
      Result := EditorOpts.HighlighterList[i].FileExtensions;
  end;
end;

procedure TEditorOptionsFormNew.SetCurFileExtensions(
  const LanguageName, FileExtensions: String);
begin
  if fFileExtensions = Nil then
    fFileExtensions := TStringList.Create;
  fFileExtensions.Values[LanguageName] := FileExtensions;
  //DebugLn(['TEditorOptionsFormNew.SetCurFileExtensions ',LanguageName,'=',FileExtensions]);
end;

procedure TEditorOptionsFormNew.SaveAllFileExtensions;
var
  i, j: Integer;
begin
  if fFileExtensions = Nil then
    exit;
  for i := 0 to fFileExtensions.Count - 1 do
  begin
    j := EditorOpts.HighlighterList.FindByName(fFileExtensions.Names[i]);
    if j >= 0 then begin
      EditorOpts.HighlighterList[j].FileExtensions :=
        fFileExtensions.ValueFromIndex[i];
      //DebugLn(['TEditorOptionsFormNew.SaveAllFileExtensions ',fFileExtensions.Names[i],'=',fFileExtensions.ValueFromIndex[i],' -> ',EditorOpts.HighlighterList[j].FileExtensions]);
    end;
  end;
end;

function TEditorOptionsFormNew.GetHighlighter(SynClass: TCustomSynClass;
  const ColorScheme: String; CreateIfNotExists: Boolean): TCustomSyn;
var
  i: Integer;
begin
  if fHighlighterList = Nil then
    fHighlighterList := TStringList.Create;
  for i := 0 to fHighlighterList.Count - 1 do
    if (fHighlighterList[i] = ColorScheme) and
      (TCustomSynClass(TCustomSyn(fHighlighterList.Objects[i]).ClassType) =
      SynClass) then
    begin
      Result := TCustomSyn(fHighlighterList.Objects[i]);
      exit;
    end;
  if CreateIfNotExists then
  begin
    Result := SynClass.Create(Nil);
    EditorOpts.AddSpecialHilightAttribsToHighlighter(Result);
    fHighlighterList.AddObject(ColorScheme, Result);
    EditorOpts.ReadHighlighterSettings(Result, ColorScheme);
  end;
end;

procedure TEditorOptionsFormNew.ClearHighlighters;
var
  i: Integer;
begin
  if fHighlighterList = Nil then
    exit;
  for i := 0 to fHighlighterList.Count - 1 do
    TCustomSyn(fHighlighterList.Objects[i]).Free;
  fHighlighterList.Free;
end;

procedure TEditorOptionsFormNew.SaveAllHighlighters;
var
  i: Integer;
  Syn: TCustomSyn;
begin
  if fHighlighterList = Nil then
    exit;
  for i := 0 to fHighlighterList.Count - 1 do
  begin
    Syn := TCustomSyn(fHighlighterList.Objects[i]);
    EditorOpts.WriteHighlighterSettings(Syn, fHighlighterList[i]);
  end;
end;

// keymapping ------------------------------------------------------------------

function TEditorOptionsFormNew.KeyMappingRelationToString(Index: Integer): String;
begin
  Result := KeyMappingRelationToString(EditingKeyMap.Relations[Index]);
end;

function TEditorOptionsFormNew.KeyMappingRelationToString(
  KeyRelation: TKeyCommandRelation): String;
var
  s: String;
begin
  with KeyRelation do
  begin
    Result := copy(LocalizedName, 1, 40);
    if length(Result) < 40 then
    begin
      SetLength(s, (40 - length(Result)));
      FillChar(s[1], length(s), ' ');
    end
    else
      s := '';
    Result := Result + s;
    if (ShortcutA.Key1 = VK_UNKNOWN) and (ShortcutB.Key1 = VK_UNKNOWN) then
      Result := Result + lisNone2
    else
    if (ShortcutA.Key1 = VK_UNKNOWN) then
      Result := Result + KeyAndShiftStateToEditorKeyString(ShortcutB)
    else
    if (ShortcutB.Key1 = VK_UNKNOWN) then
      Result := Result + KeyAndShiftStateToEditorKeyString(ShortcutA)
    else
      Result := Result + KeyAndShiftStateToEditorKeyString(ShortcutA)
                       + '  '+lisOr+'  ' +
                         KeyAndShiftStateToEditorKeyString(ShortcutB);
  end;
end;

procedure TEditorOptionsFormNew.FillKeyMappingTreeView;
var
  i, j: Integer;
  NewCategoryNode, NewKeyNode: TTreeNode;
  CurCategory: TIDECommandCategory;
  CurKeyRelation: TKeyCommandRelation;
begin
  with KeyMappingTreeView do
  begin
    BeginUpdate;
    for i := 0 to EditingKeyMap.CategoryCount - 1 do
    begin
      CurCategory := EditingKeyMap.Categories[i];
      if Items.TopLvlCount > i then
      begin
        NewCategoryNode := Items.TopLvlItems[i];
        NewCategoryNode.Text := CurCategory.Description;
        NewCategoryNode.Data := CurCategory;
      end
      else
        NewCategoryNode := Items.AddObject(Nil, CurCategory.Description, CurCategory);
      NewCategoryNode.ImageIndex := 0;
      NewCategoryNode.SelectedIndex := NewCategoryNode.ImageIndex;
      for j := 0 to CurCategory.Count - 1 do
      begin
        CurKeyRelation := TKeyCommandRelation(CurCategory[j]);
        if NewCategoryNode.Count > j then
        begin
          NewKeyNode := NewCategoryNode.Items[j];
          NewKeyNode.Text := KeyMappingRelationToString(CurKeyRelation);
          NewKeyNode.Data := CurKeyRelation;
        end
        else
          NewKeyNode := Items.AddChildObject(NewCategoryNode,
            KeyMappingRelationToString(CurKeyRelation), CurKeyRelation);
        NewKeyNode.ImageIndex := 1;
        NewKeyNode.SelectedIndex := NewKeyNode.ImageIndex;
      end;
      while NewCategoryNode.Count > CurCategory.Count do
        NewCategoryNode[NewCategoryNode.Count - 1].Delete;
    end;
    while Items.TopLvlCount > EditingKeyMap.CategoryCount do
      Items.TopLvlItems[Items.TopLvlCount - 1].Delete;
    EndUpdate;
  end;
end;

// useful functions

procedure TEditorOptionsFormNew.SetComboBoxText(AComboBox: TComboBox;
  const AText: String);
var
  a: Integer;
begin
  a := AComboBox.Items.IndexOf(AText);
  if a >= 0 then begin
    AComboBox.ItemIndex := a;
  end else
  begin
    AComboBox.Items.Add(AText);
    AComboBox.ItemIndex := AComboBox.Items.IndexOf(AText);
  end;
end;

procedure TEditorOptionsFormNew.SetupGeneralPage(Page: Integer);
begin
  NoteBook.Page[Page].Caption := lisMenuInsertGeneral;

  EditorOptionsGroupBox.Caption := lismenueditoroptions;

  with EditorOptionsGroupBox do
  begin
    // selections
    Items.Add(dlgAltSetClMode);
    Items.Add(dlgAutoIdent);
    // visual effects
    Items.Add(dlgBracHighlight);
    Items.Add(dlgShowGutterHints);
    Items.Add(dlgShowScrollHint);
    Items.Add(dlgUseSyntaxHighlight);
    // drag&drop
    Items.Add(dlgDragDropEd);
    Items.Add(dlgDropFiles);
    // caret + scrolling + key navigation
    Items.Add(dlgHalfPageScroll);
    Items.Add(dlgKeepCaretX);
    Items.Add(dlgPersistentCaret);
    Items.Add(dlgCaretSkipsSelection);
    Items.Add(dlgRightMouseMovesCursor);
    Items.Add(dlgScrollByOneLess);
    Items.Add(dlgScrollPastEndFile);
    Items.Add(dlgScrollPastEndLine);
    Items.Add(dlgHomeKeyJumpsToNearestStart);
    Items.Add(dlgAlwaysVisibleCaret);
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

    Checked[Items.IndexOf(dlgAltSetClMode)] := eoAltSetsColumnMode in
                                                      EditorOpts.SynEditOptions;
    Checked[Items.IndexOf(dlgAutoIdent)]    := eoAutoIndent in EditorOpts.SynEditOptions;
    Checked[Items.IndexOf(dlgBracHighlight)] :=
                                eoBracketHighlight in EditorOpts.SynEditOptions;
    Checked[Items.IndexOf(dlgDragDropEd)]   :=
                                 eoDragDropEditing in EditorOpts.SynEditOptions;
    Checked[Items.IndexOf(dlgDropFiles)]    := eoDropFiles in EditorOpts.SynEditOptions;
    //TODO CheckEnabledByName[dlgDropFiles] := False;
    Checked[Items.IndexOf(dlgGroupUndo)] := eoGroupUndo in EditorOpts.SynEditOptions;
    Checked[Items.IndexOf(dlgHalfPageScroll)] :=
                                  eoHalfPageScroll in EditorOpts.SynEditOptions;
    Checked[Items.IndexOf(dlgKeepCaretX)]   := eoKeepCaretX in EditorOpts.SynEditOptions;
    Checked[Items.IndexOf(dlgPersistentCaret)] :=
                                 eoPersistentCaret in EditorOpts.SynEditOptions;
    Checked[Items.IndexOf(dlgRightMouseMovesCursor)] :=
                           eoRightMouseMovesCursor in EditorOpts.SynEditOptions;
    Checked[Items.IndexOf(dlgScrollByOneLess)] :=
                                 eoScrollByOneLess in EditorOpts.SynEditOptions;
    Checked[Items.IndexOf(dlgScrollPastEndFile)] :=
                                   eoScrollPastEoF in EditorOpts.SynEditOptions;
    Checked[Items.IndexOf(dlgMouseLinks)]   := EditorOpts.CtrlMouseLinks;
    Checked[Items.IndexOf(dlgShowGutterHints)] := EditorOpts.ShowGutterHints;
    Checked[Items.IndexOf(dlgScrollPastEndLine)] :=
                                   eoScrollPastEoL in EditorOpts.SynEditOptions;
    Checked[Items.IndexOf(dlgCloseButtonsNotebook)] := EditorOpts.ShowTabCloseButtons;
    Checked[Items.IndexOf(dlgShowScrollHint)] :=
                                  eoShowScrollHint in EditorOpts.SynEditOptions;
    Checked[Items.IndexOf(dlgSmartTabs)] := eoSmartTabs in EditorOpts.SynEditOptions;
    //DebugLn(['TEditorOptionsFormNew.SetupGeneralPage ',Checked[Items.IndexOf(dlgSmartTabs)],' ',Items.IndexOf(dlgSmartTabs),' ',eoSmartTabs in EditorOpts.SynEditOptions]);
    Checked[Items.IndexOf(dlgTabsToSpaces)] :=
                                    eoTabsToSpaces in EditorOpts.SynEditOptions;
    Checked[Items.IndexOf(dlgTabIndent)]    := eoTabIndent in EditorOpts.SynEditOptions;
    Checked[Items.IndexOf(dlgTrimTrailingSpaces)] :=
                              eoTrimTrailingSpaces in EditorOpts.SynEditOptions;
    Checked[Items.IndexOf(dlgUndoAfterSave)] := EditorOpts.UndoAfterSave;
    Checked[Items.IndexOf(dlgDoubleClickLine)] :=
                          eoDoubleClickSelectsLine in EditorOpts.SynEditOptions;
    Checked[Items.IndexOf(dlgFindTextatCursor)] := EditorOpts.FindTextAtCursor;
    Checked[Items.IndexOf(dlgUseSyntaxHighlight)] := EditorOpts.UseSyntaxHighlight;
    Checked[Items.IndexOf(dlgCopyWordAtCursorOnCopyNone)] :=
                                          EditorOpts.CopyWordAtCursorOnCopyNone;
    Checked[Items.IndexOf(dlgHomeKeyJumpsToNearestStart)] :=
                                  eoEnhanceHomeKey in EditorOpts.SynEditOptions;
    Checked[Items.IndexOf(dlgCaretSkipsSelection)] :=
                            eoCaretSkipsSelection in EditorOpts.SynEditOptions2;
    Checked[Items.IndexOf(dlgAlwaysVisibleCaret)] :=
                             eoAlwaysVisibleCaret in EditorOpts.SynEditOptions2;
  end;

  with BlockIndentComboBox do
    SetComboBoxText(BlockIndentComboBox, IntToStr(EditorOpts.BlockIndent));

  BlockIndentLabel.Caption := dlgBlockIndent;

  with UndoLimitComboBox do
    SetComboBoxText(UndoLimitComboBox, IntToStr(EditorOpts.UndoLimit));

  UndoLimitLabel.Caption := dlgUndoLimit;

  with TabWidthsComboBox do
    SetComboBoxText(TabWidthsComboBox, IntToStr(EditorOpts.TabWidth));

  TabWidthsLabel.Caption := dlgTabWidths;
end;

procedure TEditorOptionsFormNew.SetupDisplayPage(Page: Integer);
begin
  NoteBook.Page[Page].Caption := dlgEdDisplay;

  MarginAndGutterGroupBox.Caption := dlgMarginGutter;

  with VisibleRightMarginCheckBox do
  begin
    Caption := dlgVisibleRightMargin;
    Checked := EditorOpts.VisibleRightMargin;
  end;

  with VisibleGutterCheckBox do
  begin
    Caption := dlgVisibleGutter;
    Checked := EditorOpts.VisibleGutter;
  end;

  with ShowLineNumbersCheckBox do
  begin
    Caption := dlgShowLineNumbers;
    Checked := EditorOpts.ShowLineNumbers;
  end;

  RightMarginLabel.Caption := dlgRightMargin;

  VisibleRightMarginCheckBox.Checked:=EditorOpts.VisibleRightMargin;
  with RightMarginComboBox do
    SetComboBoxText(RightMarginComboBox, IntToStr(EditorOpts.RightMargin));

  RightMarginColorLabel.Caption := dlgRightMarginColor;

  RightMarginColorButton.ButtonColor := EditorOpts.RightMarginColor;

  GutterWidthLabel.Caption := dlgGutterWidth;

  with GutterWidthComboBox do
    SetComboBoxText(GutterWidthComboBox, IntToStr(EditorOpts.GutterWidth));

  GutterColorLabel.Caption := dlgGutterColor;

  GutterColorButton.ButtonColor := EditorOpts.GutterColor;

  EditorFontGroupBox.Caption := dlgDefaultEditorFont;

  with EditorFontComboBox do
    SetComboBoxText(EditorFontComboBox, EditorOpts.EditorFont);

  EditorFontLabel.Caption := dlgEditorFont;

  with EditorFontHeightComboBox do
    SetComboBoxText(EditorFontHeightComboBox
      , IntToStr(EditorOpts.EditorFontHeight));

  EditorFontHeightLabel.Caption := dlgEditorFontHeight;

  with ExtraLineSpacingComboBox do
    SetComboBoxText(ExtraLineSpacingComboBox
      , IntToStr(EditorOpts.ExtraLineSpacing));

  ExtraLineSpacingLabel.Caption := dlgExtraLineSpacing;
end;

procedure TEditorOptionsFormNew.SetupKeyMappingsPage(Page: Integer);
begin
  NoteBook.Page[Page].Caption := dlgKeyMapping;

  KeyMappingChooseSchemeButton.Caption := lisEdOptsChooseScheme;

  KeyMappingConsistencyCheckButton.Caption := dlgCheckConsistency;

  KeyMappingHelpLabel.Caption := dlgEdHintCommand;
end;

procedure TEditorOptionsFormNew.SetupColorPage(Page: Integer);
var
  a: Integer;
begin
  NoteBook.Page[Page].Caption := dlgEdColor;

  LanguageLabel.Caption := dlgLang;

  with LanguageComboBox do
    with Items do
    begin
      BeginUpdate;
      for a := 0 to EditorOpts.HighlighterList.Count - 1 do
        Add(EditorOpts.HighlighterList[a].SynClass.GetLanguageName);
      //for a:=0 to EditorOpts.HighlighterList.Count-1 do
      //  writeln('TEditorOptionsFormNew.SetupColorPage ',a,' ',EditorOpts.HighlighterList[a].SynClass.GetLanguageName
      //  ,' ',EditorOpts.HighlighterList[a].SynClass.ClassName);
      EndUpdate;
    end;

  ColorSchemeLabel.Caption := dlgClrScheme;

  with ColorSchemeComboBox do
  begin
    with Items do
    begin
      BeginUpdate;
      // ToDo: fill also with custom color schemes
      Add(DefaultColorScheme);
      Add('Delphi');
      Add('Pascal Classic');
      Add('Twilight');
      Add('Ocean');
      EndUpdate;
    end;
    Text := DefaultColorScheme;
  end;

  FileExtensionsLabel.Caption := dlgFileExts;

  with FileExtensionsComboBox do
    if CurLanguageID >= 0 then
      SetComboBoxText(FileExtensionsComboBox,
        EditorOpts.HighlighterList[CurLanguageID].FileExtensions);

  ColorElementLabel.Caption := dlgEdElement;

  SetAttributeToDefaultButton.Caption := dlgSetElementDefault;

  SetAllAttributesToDefaultButton.Caption := dlgSetAllElementDefault;

  ForeGroundLabel.Caption := dlgForecolor;

  ForeGroundUseDefaultCheckBox.Caption := dlgEdUseDefColor;

  BackGroundLabel.Caption := dlgBackColor;

  BackgroundColorButton.Color := clBlue;

  BackGroundUseDefaultCheckBox.Caption := dlgEdUseDefColor;

  TextAttributesGroupBox.Caption := dlgTextAttributes;

  TextBoldCheckBox.Caption := dlgEdBold;

  TextItalicCheckBox.Caption := dlgEdItal;

  TextUnderlineCheckBox.Caption := dlgEdUnder;
end;

procedure TEditorOptionsFormNew.SetupCodeToolsPage(Page: Integer);
begin
  NoteBook.Page[Page].Caption := dlgCodeToolsTab;

  AutomaticFeaturesGroupBox.Caption := lisAutomaticFeatures;

  with AutoIdentifierCompletionCheckBox do
  begin
    Caption := dlgEdIdComlet;
    Checked := EditorOpts.AutoIdentifierCompletion;
  end;

  with AutoCodeParametersCheckBox do
  begin
    Caption := dlgEdCodeParams;
    Checked := EditorOpts.AutoCodeParameters;
  end;

  with AutoToolTipExprEvalCheckBox do
  begin
    Caption := dlgTooltipEval;
    Checked := EditorOpts.AutoToolTipExprEval;
  end;

  with AutoToolTipSymbToolsCheckBox do
  begin
    Caption := dlgTooltipTools;
    Checked := EditorOpts.AutoToolTipSymbTools;
  end;

  AutoDelayLabel.Caption := dlgEdDelay;
  AutoDelayTrackBar.Position := EditorOpts.AutoDelayInMSec;
  AutoDelayMinLabel.Caption := '0.5 ' + DlgTimeSecondUnit;
  AutoDelayMaxLabel.Caption := '4.0 ' + dlgTimeSecondUnit;
end;

procedure TEditorOptionsFormNew.SetupCodeFoldingPage(Page: integer);
begin
  NoteBook.Page[Page].Caption := dlgUseCodeFolding;
  chkCodeFoldingEnabled.Caption   := dlgUseCodeFolding;
  lblDividerDrawLevel.Caption     := dlgCFDividerDrawLevel + ':';
  
  chkCodeFoldingEnabled.Checked   := EditorOpts.UseCodeFolding;
  edDividerDrawLevel.Value        := EditorOpts.CFDividerDrawLevel;
end;

procedure TEditorOptionsFormNew.SetupButtonBar;
begin
  btnOk.Caption := lisOk;
  btnCancel.Caption := dlgCancel;

  CancelControl := btnCancel;
end;

procedure TEditorOptionsFormNew.OkButtonClick(Sender: TObject);
var
  SynOptions: TSynEditorOptions;
  i: Integer;
begin
  IDEDialogLayoutList.SaveLayout(Self);

  // save all values
  EditorOpts.KeyMap.Assign(EditingKeyMap);
  SynOptions := PreviewEdits[1].Options - [eoNoSelection, eoNoCaret];
  if CheckGroupItemChecked(EditorOptionsGroupBox,dlgBracHighlight) then
    Include(SynOptions, eoBracketHighlight)
  else
    Exclude(SynOptions, eoBracketHighlight);
  PreviewEdits[1].Options := SynOptions;
  EditorOpts.SetSynEditSettings(PreviewEdits[1]);
  PreviewEdits[1].Options :=
    SynOptions - [eoBracketHighlight] +
    [eoNoCaret, eoNoSelection];

  // general
  EditorOpts.ShowTabCloseButtons :=
    CheckGroupItemChecked(EditorOptionsGroupBox,dlgCloseButtonsNotebook);
  EditorOpts.UndoAfterSave :=
    CheckGroupItemChecked(EditorOptionsGroupBox,dlgUndoAfterSave);
  EditorOpts.CopyWordAtCursorOnCopyNone :=
    CheckGroupItemChecked(EditorOptionsGroupBox,dlgCopyWordAtCursorOnCopyNone);
  EditorOpts.ShowGutterHints :=
    CheckGroupItemChecked(EditorOptionsGroupBox,dlgShowGutterHints);
  EditorOpts.FindTextAtCursor :=
    CheckGroupItemChecked(EditorOptionsGroupBox,dlgFindTextatCursor);
  EditorOpts.UseSyntaxHighlight :=
    CheckGroupItemChecked(EditorOptionsGroupBox,dlgUseSyntaxHighlight);
  EditorOpts.CtrlMouseLinks :=
    CheckGroupItemChecked(EditorOptionsGroupBox,dlgMouseLinks);
  i := StrToIntDef(UndoLimitComboBox.Text, 32767);
  if i < 1 then
    i := 1;
  if i > 32767 then
    i := 32767;
  EditorOpts.UndoLimit := i;
  i := StrToIntDef(TabWidthsComboBox.Text, 2);
  if i < 1 then
    i := 1;
  if i > 20 then
    i := 20;
  EditorOpts.TabWidth := i;
  i := StrToIntDef(BlockIndentComboBox.Text, 2);
  if i < 1 then
    i := 1;
  if i > 20 then
    i := 20;
  EditorOpts.BlockIndent := i;


  // color
  SaveAllFileExtensions;
  SaveAllColorSchemes;
  SaveAllHighlighters;

  // code Tools
  EditorOpts.AutoIdentifierCompletion :=
    AutoIdentifierCompletionCheckBox.Checked;
  EditorOpts.AutoCodeParameters := AutoCodeParametersCheckBox.Checked;
  EditorOpts.AutoToolTipExprEval := AutoToolTipExprEvalCheckBox.Checked;
  EditorOpts.AutoToolTipSymbTools := AutoToolTipSymbToolsCheckBox.Checked;
  EditorOpts.AutoDelayInMSec    := AutoDelayTrackBar.Position;

  // Code Folding
  EditorOpts.UseCodeFolding       := chkCodeFoldingEnabled.Checked;
  EditorOpts.CFDividerDrawLevel   := edDividerDrawLevel.Value;
  
  EditorOpts.Save;
  ModalResult := mrOk;
end;

procedure TEditorOptionsFormNew.CancelButtonClick(Sender: TObject);
begin
  IDEDialogLayoutList.SaveLayout(Self);
  EditorOpts.Load;
  ModalResult := mrCancel;
end;

//=============================================================================

initialization
  {$I editoroptions_new.lrs}
  {$I lazarus_dci.lrs}

end.
