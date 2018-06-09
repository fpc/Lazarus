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
}
unit editor_markup_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils,
  // LCL
  LCLType, CheckLst, Controls, StdCtrls, ComCtrls, Graphics, Spin,
  // LazControls
  DividerBevel,
  // IDEIntf
  EditorSyntaxHighlighterDef, IDEOptionsIntf, IDEOptEditorIntf,
  // SynEdit
  SynEdit, SynHighlighterPas, SynEditKeyCmds, SynEditHighlighterFoldBase,
  // IDE
  EditorOptions, LazarusIDEStrConsts, SourceMarks, SynEditMarkupBracket,
  editor_color_options, editor_general_options, editor_keymapping_options,
  editor_codefolding_options;

type
  { TEditorMarkupOptionsFrame }

  TEditorMarkupOptionsFrame = class(TAbstractIDEOptionsEditor)
    BracketCombo: TComboBox;
    BracketLabel: TLabel;
    BracketLink: TLabel;
    cbMarkup: TCheckBox;
    cbOutline: TCheckBox;
    cbMarkupWordBracket: TCheckBox;
    cbMarkupOutline: TCheckBox;
    chkKWGroups: TCheckListBox;
    chkExtPasKeywords: TCheckBox;
    divKeyWordGroups: TDividerBevel;
    dropPasStringKeywords: TComboBox;
    divKeywords: TDividerBevel;
    divMatchingBrackets: TDividerBevel;
    divWordGroup: TDividerBevel;
    LanguageComboBox: TComboBox;
    LanguageLabel: TLabel;
    lblPasStringKeywords: TLabel;
    MarkupColorLink: TLabel;
    MarkupKeyLink: TLabel;
    MarkupWordDelayLabel: TLabel;
    MarkupWordNoTimerCheckBox: TCheckBox;
    MarkupWordFullLenSpin: TSpinEdit;
    MarkupWordFullLenLabel: TLabel;
    MarkupWordNoKeyword: TCheckBox;
    MarkupWordTrim: TCheckBox;
    MarkupWordTimeTrackBar: TTrackBar;
    procedure AutoDelayTrackBarChange(Sender: TObject);
    procedure BracketComboChange(Sender: TObject);
    procedure BracketComboExit(Sender: TObject);
    procedure BracketComboKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure BracketLinkClick(Sender: TObject);
    procedure BracketLinkMouseEnter(Sender: TObject);
    procedure BracketLinkMouseLeave(Sender: TObject);
    procedure cbMarkupChange(Sender: TObject);
    procedure cbMarkupOutlineChange(Sender: TObject);
    procedure cbMarkupWordBracketChange(Sender: TObject);
    procedure chkExtPasKeywordsChange(Sender: TObject);
    procedure chkKWGroupsClick(Sender: TObject);
    procedure chkKWGroupsClickCheck(Sender: TObject);
    procedure chkKWGroupsKeyUp(Sender: TObject; var {%H-}Key: Word; {%H-}Shift: TShiftState);
    procedure dropPasStringKeywordsChange(Sender: TObject);
    function GeneralPage: TEditorGeneralOptionsFrame; inline;
    function FoldPage: TEditorCodefoldingOptionsFrame; inline;
    procedure LanguageComboBoxChange(Sender: TObject);
    procedure LanguageComboBoxExit(Sender: TObject);
    procedure LanguageComboBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MarkupColorLinkClick(Sender: TObject);
    procedure MarkupKeyLinkClick(Sender: TObject);
  private
    { private declarations }
    FDialog: TAbstractOptionsEditorDialog;
    FModeLock: Boolean;
    FCurHighlighter: TSrcIDEHighlighter;
    FCurFoldInfo: TEditorOptionsFoldRecord;
    FUseMarkupWordBracket: Boolean;
    FUseMarkupOutline: Boolean;
    function GetHighlighter(SynType: TLazSyntaxHighlighter;
      CreateIfNotExists: Boolean): TSrcIDEHighlighter;
    procedure UpdateMarkupCheckBoxes;

  public
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TEditorMarkupOptionsFrame }

procedure TEditorMarkupOptionsFrame.BracketComboChange(Sender: TObject);
begin
  if BracketCombo.Items.IndexOf(BracketCombo.Text) >= 0 then
    BracketComboExit(Sender);
end;

procedure TEditorMarkupOptionsFrame.AutoDelayTrackBarChange(Sender: TObject);
begin
  MarkupWordDelayLabel.Caption :=
    Format(dlgEdDelayInSec, [FormatFloat('0.00', MarkupWordTimeTrackBar.Position/1000)]);
end;

procedure TEditorMarkupOptionsFrame.BracketComboExit(Sender: TObject);
var
  a: Integer;
begin
  with GeneralPage do
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

procedure TEditorMarkupOptionsFrame.BracketComboKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key = VK_S) then
    BracketComboExit(Sender);
end;

procedure TEditorMarkupOptionsFrame.BracketLinkClick(Sender: TObject);
var
  col: TEditorColorOptionsFrame;
begin
  col := TEditorColorOptionsFrame(FDialog.FindEditor(TEditorColorOptionsFrame));
  if col = nil then exit;
  FDialog.OpenEditor(TEditorColorOptionsFrame);
  col.SelectAhaColor(ahaBracketMatch);
end;

procedure TEditorMarkupOptionsFrame.BracketLinkMouseEnter(Sender: TObject);
begin
  (Sender as TLabel).Font.Underline := True;
  (Sender as TLabel).Font.Color := clRed;
end;

procedure TEditorMarkupOptionsFrame.BracketLinkMouseLeave(Sender: TObject);
begin
  (Sender as TLabel).Font.Underline := False;
  (Sender as TLabel).Font.Color := clBlue;
end;

procedure TEditorMarkupOptionsFrame.cbMarkupChange(Sender: TObject);
var
  Hl: TSynCustomFoldHighlighter;
  Modes: TSynCustomFoldConfigModes;
  i: LongInt;
begin
  if FModeLock then exit;
  if not (assigned(FCurHighlighter) and
         (FCurHighlighter is TSynCustomFoldHighlighter)) then exit;

  i := chkKWGroups.ItemIndex;
  if i < 0 then exit;
  i := PtrUInt(chkKWGroups.Items.Objects[i]);
  i := FCurFoldInfo.Info^[i].Index;
  Hl := TSynCustomFoldHighlighter(FCurHighlighter);

  Modes := [fmMarkup];
  if Sender = cbOutline then
    Modes := [fmOutline];

  if TCheckBox(Sender).Checked then
    Hl.FoldConfig[i].Modes := Hl.FoldConfig[i].Modes + Modes
  else
    Hl.FoldConfig[i].Modes := Hl.FoldConfig[i].Modes - Modes;

  chkKWGroups.Checked[chkKWGroups.ItemIndex] := cbMarkup.Checked or cbOutline.Checked;
end;

procedure TEditorMarkupOptionsFrame.cbMarkupOutlineChange(Sender: TObject);
begin
  FUseMarkupOutline := cbMarkupOutline.Checked;
  LanguageComboBoxExit(nil);
end;

procedure TEditorMarkupOptionsFrame.cbMarkupWordBracketChange(Sender: TObject);
begin
  FUseMarkupWordBracket := cbMarkupWordBracket.Checked;
  LanguageComboBoxExit(nil);
end;

procedure TEditorMarkupOptionsFrame.chkExtPasKeywordsChange(Sender: TObject);
begin
  GeneralPage.PasExtendedKeywordsMode := chkExtPasKeywords.Checked;
  GeneralPage.UpdatePrevieEdits;
end;

procedure TEditorMarkupOptionsFrame.chkKWGroupsClick(Sender: TObject);
begin
  UpdateMarkupCheckBoxes;
end;

procedure TEditorMarkupOptionsFrame.chkKWGroupsClickCheck(Sender: TObject);
var
  i, j, idx, i1: Integer;
  Hl: TSynCustomFoldHighlighter;
  FMask: TSynCustomFoldConfigModes;
begin
  if not (assigned(FCurHighlighter) and
         (FCurHighlighter is TSynCustomFoldHighlighter)) then exit;
  Hl := TSynCustomFoldHighlighter(FCurHighlighter);
  FMask := [fmMarkup, fmOutline];
  if not FUseMarkupWordBracket then FMask := FMask - [fmMarkup];
  if not FUseMarkupOutline then FMask := FMask - [fmOutline];

  j := 0;
  for i1 := 0 to chkKWGroups.Count - 1 do begin
    i := PtrUInt(chkKWGroups.Items.Objects[i1]);
    idx := FCurFoldInfo.Info^[i].Index;
    if Hl.FoldConfig[idx].SupportedModes * [fmMarkup, fmOutline] <> [] then begin
      if not chkKWGroups.Checked[j] then
        Hl.FoldConfig[idx].Modes := Hl.FoldConfig[idx].Modes - FMask
      else
      if Hl.FoldConfig[idx].Modes * FMask = [] then
        Hl.FoldConfig[idx].Modes := Hl.FoldConfig[idx].Modes + FMask;
      inc(j);
    end;
  end;

  UpdateMarkupCheckBoxes;
end;

procedure TEditorMarkupOptionsFrame.chkKWGroupsKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  chkKWGroupsClickCheck(nil);
end;

procedure TEditorMarkupOptionsFrame.dropPasStringKeywordsChange(Sender: TObject);
begin
  GeneralPage.PasStringKeywordMode := TSynPasStringMode(dropPasStringKeywords.ItemIndex);
  GeneralPage.UpdatePrevieEdits;
end;

function TEditorMarkupOptionsFrame.GeneralPage: TEditorGeneralOptionsFrame; inline;
begin
  Result := TEditorGeneralOptionsFrame(FDialog.FindEditor(TEditorGeneralOptionsFrame));
end;

function TEditorMarkupOptionsFrame.FoldPage: TEditorCodefoldingOptionsFrame;
begin
  Result := TEditorCodefoldingOptionsFrame(FDialog.FindEditor(TEditorCodefoldingOptionsFrame));
end;

procedure TEditorMarkupOptionsFrame.LanguageComboBoxChange(Sender: TObject);
var
  ComboBox: TComboBox absolute Sender;
begin
  if ComboBox.Items.IndexOf(ComboBox.Text) >= 0 then
    LanguageComboBoxExit(Sender);
end;

procedure TEditorMarkupOptionsFrame.LanguageComboBoxExit(Sender: TObject);
var
  tp: TLazSyntaxHighlighter;
  i, j: Integer;
  Hl: TSynCustomFoldHighlighter;
  FMask: TSynCustomFoldConfigModes;
begin
  tp := EditorOpts.HighlighterList
          [EditorOpts.HighlighterList.FindByName(LanguageComboBox.Text)].TheType;
  FCurHighlighter := GetHighlighter(tp, True);
  FCurFoldInfo := EditorOptionsFoldDefaults[tp];

  chkKWGroups.Clear;
  if not (assigned(FCurHighlighter) and
         (FCurHighlighter is TSynCustomFoldHighlighter)) then exit;
  Hl := TSynCustomFoldHighlighter(FCurHighlighter);

  FMask := [fmMarkup, fmOutline];
  if not FUseMarkupWordBracket then FMask := FMask - [fmMarkup];
  if not FUseMarkupOutline then FMask := FMask - [fmOutline];
  for i := 0 to FCurFoldInfo.Count - 1 do begin
    if Hl.FoldConfig[FCurFoldInfo.Info^[i].Index].SupportedModes * FMask <> [] then begin
      j := chkKWGroups.Items.Add(FCurFoldInfo.Info^[i].Name);
      chkKWGroups.Checked[j] :=
        (Hl.FoldConfig[FCurFoldInfo.Info^[i].Index].Modes * FMask <> []);
      chkKWGroups.Items.Objects[j] := TObject({%H-}Pointer(PtrUInt(i)));
    end;
  end;
  UpdateMarkupCheckBoxes;
end;

procedure TEditorMarkupOptionsFrame.LanguageComboBoxKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key = VK_S) then
    LanguageComboBoxExit(Sender);
end;

procedure TEditorMarkupOptionsFrame.MarkupColorLinkClick(Sender: TObject);
var
  col: TEditorColorOptionsFrame;
begin
  col := TEditorColorOptionsFrame(FDialog.FindEditor(TEditorColorOptionsFrame));
  if col = nil then exit;
  FDialog.OpenEditor(TEditorColorOptionsFrame);
  col.SelectAhaColor(ahaHighlightWord);
end;

procedure TEditorMarkupOptionsFrame.MarkupKeyLinkClick(Sender: TObject);
var
  col: TEditorKeymappingOptionsFrame;
begin
  col := TEditorKeymappingOptionsFrame(FDialog.FindEditor(TEditorKeymappingOptionsFrame));
  if col = nil then exit;
  FDialog.OpenEditor(TEditorKeymappingOptionsFrame);
  col.SelectByIdeCommand(EcToggleMarkupWord);
end;

function TEditorMarkupOptionsFrame.GetHighlighter(
  SynType: TLazSyntaxHighlighter; CreateIfNotExists: Boolean
  ): TSrcIDEHighlighter;
begin
  Result := FoldPage.GetHighlighter(SynType, CreateIfNotExists);
end;

procedure TEditorMarkupOptionsFrame.UpdateMarkupCheckBoxes;
var
  i: LongInt;
  FMask: TSynCustomFoldConfigModes;
  Hl: TSynCustomFoldHighlighter;
begin
  if not (assigned(FCurHighlighter) and
         (FCurHighlighter is TSynCustomFoldHighlighter)) then exit;
  Hl := TSynCustomFoldHighlighter(FCurHighlighter);
  FModeLock := True;
  i := chkKWGroups.ItemIndex;
  if i >= 0 then
    i := PtrUInt(chkKWGroups.Items.Objects[i]);

  if i >= 0 then begin
    i := FCurFoldInfo.Info^[i].Index;
    FMask := [fmMarkup, fmOutline];
    if not FUseMarkupWordBracket then FMask := FMask - [fmMarkup];
    if not FUseMarkupOutline then FMask := FMask - [fmOutline];

    cbMarkup.Enabled := fmMarkup in Hl.FoldConfig[i].SupportedModes * FMask;
    cbMarkup.Checked := fmMarkup in Hl.FoldConfig[i].Modes * FMask;
    cbOutline.Enabled := fmOutline in Hl.FoldConfig[i].SupportedModes * FMask;
    cbOutline.Checked := fmOutline in Hl.FoldConfig[i].Modes * FMask;
  end else
  begin
    cbMarkup.Enabled  := false;
    cbMarkup.Checked  := false;
    cbOutline.Enabled := false;
    cbOutline.Checked := false;
  end;
  FModeLock := False;
end;

function TEditorMarkupOptionsFrame.GetTitle: String;
begin
  Result := lisAutoMarkup;
end;

procedure TEditorMarkupOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
var
  i: Integer;
  rf: TEditorOptionsFoldRecord;
begin
  FDialog := ADialog;

  divWordGroup.Caption := dlgMarkupGroup;
  MarkupWordFullLenLabel.Caption := dlgMarkupWordFullLen;
  MarkupWordNoKeyword.Caption := dlgMarkupWordNoKeyword;
  MarkupWordTrim.Caption := dlgMarkupWordTrim;
  MarkupWordNoTimerCheckBox.Caption := dlgMarkupWordNoTimer;
  MarkupColorLink.Caption := dlgColorLink;
  MarkupKeyLink.Caption := dlgKeyLink;

  divMatchingBrackets.Caption := dlgBracketMatchGroup;
  BracketLabel.Caption := dlgBracketHighlight;
  BracketLink.Caption := dlgColorLink;
  BracketCombo.Items.Add(dlgNoBracketHighlight);
  BracketCombo.Items.Add(dlgHighlightLeftOfCursor);
  BracketCombo.Items.Add(dlgHighlightRightOfCursor);
  BracketCombo.Items.Add(gldHighlightBothSidesOfCursor);

  divKeywords.Caption := dlgPasExtKeywordsGroup;
  chkExtPasKeywords.Caption := dlgPasExtKeywords;
  lblPasStringKeywords.Caption := dlgPasStringKeywords;
  dropPasStringKeywords.Items.Add(dlgPasStringKeywordsOptDefault);
  dropPasStringKeywords.Items.Add(dlgPasStringKeywordsOptString);
  dropPasStringKeywords.Items.Add(dlgPasStringKeywordsOptNone);

  LanguageLabel.Caption := dlgLang;
  divKeyWordGroups.Caption := dlgPasKeywordsMatches;

  cbMarkup.Caption := dlgPasKeywordsMarkup;
  cbOutline.Caption := dlgPasKeywordsOutline;
  cbMarkupWordBracket.Caption := dlgMarkupWordBracket;
  cbMarkupOutline.Caption := dlgMarkupOutline;

  with LanguageComboBox.Items do begin
    BeginUpdate;
    for i := 0 to EditorOpts.HighlighterList.Count - 1 do begin
      rf := EditorOptionsFoldDefaults[EditorOpts.HighlighterList[i].TheType];
      if (rf.Count > 0) and (rf.HasMarkup) then
        Add(EditorOpts.HighlighterList[i].SynClass.GetLanguageName);
    end;
    EndUpdate;
  end;
end;

procedure TEditorMarkupOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEditorOptions do
  begin
    MarkupWordTimeTrackBar.Position := MarkupCurWordTime;
    MarkupWordFullLenSpin. Value := MarkupCurWordFullLen;
    MarkupWordNoKeyword.Checked := MarkupCurWordNoKeyword;
    MarkupWordTrim.Checked := MarkupCurWordTrim;
    MarkupWordNoTimerCheckBox.Checked := MarkupCurWordNoTimer;

    if eoBracketHighlight in SynEditOptions then
      BracketCombo.ItemIndex := Ord(BracketHighlightStyle) + 1
    else
      BracketCombo.ItemIndex := 0;

    chkExtPasKeywords.Checked := PasExtendedKeywordsMode;
    dropPasStringKeywords.ItemIndex := ord(PasStringKeywordMode);

    FUseMarkupWordBracket := UseMarkupWordBracket;
    FUseMarkupOutline := UseMarkupOutline;
  end;
  AutoDelayTrackBarChange(nil);

  LanguageComboBox.ItemIndex := 0;
  LanguageComboBoxExit(LanguageComboBox);
  cbMarkupOutline.Checked := FUseMarkupOutline;
  cbMarkupWordBracket.Checked := FUseMarkupWordBracket;
  UpdateMarkupCheckBoxes;
end;

procedure TEditorMarkupOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEditorOptions do
  begin
    MarkupCurWordTime := MarkupWordTimeTrackBar.Position;
    MarkupCurWordFullLen := MarkupWordFullLenSpin.Value;
    MarkupCurWordNoKeyword := MarkupWordNoKeyword.Checked;
    MarkupCurWordTrim := MarkupWordTrim.Checked;
    MarkupCurWordNoTimer := MarkupWordNoTimerCheckBox.Checked;

    if BracketCombo.ItemIndex = 0 then
      SynEditOptions := SynEditOptions - [eoBracketHighlight]
    else
    begin
      SynEditOptions := SynEditOptions + [eoBracketHighlight];
      BracketHighlightStyle := TSynEditBracketHighlightStyle(BracketCombo.ItemIndex - 1);
    end;

    PasExtendedKeywordsMode := chkExtPasKeywords.Checked;
    PasStringKeywordMode := TSynPasStringMode(dropPasStringKeywords.ItemIndex);

    UseMarkupWordBracket := FUseMarkupWordBracket;
    UseMarkupOutline := FUseMarkupOutline;
  end;
end;

class function TEditorMarkupOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEditorOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupEditor, TEditorMarkupOptionsFrame,
    EdtOptionsMarkup, EdtOptionsDisplay);
end.

