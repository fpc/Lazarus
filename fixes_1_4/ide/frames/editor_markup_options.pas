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
unit editor_markup_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, StdCtrls, ComCtrls, Graphics, sysutils, EditorOptions, LazarusIDEStrConsts,
  SourceMarks, IDEOptionsIntf, Spin, ExtCtrls, SynEditMarkupBracket, editor_color_options,
  editor_general_options, editor_keymapping_options, editor_codefolding_options, SynEdit,
  SynCompletion, SynHighlighterPas, SynEditKeyCmds, SynEditHighlighterFoldBase, DividerBevel,
  LCLType, CheckLst, Controls;

type
  { TEditorMarkupOptionsFrame }

  TEditorMarkupOptionsFrame = class(TAbstractIDEOptionsEditor)
    BracketCombo: TComboBox;
    BracketLabel: TLabel;
    BracketLink: TLabel;
    chkKWGroups: TCheckListBox;
    chkExtPasKeywords: TCheckBox;
    divKeyWordGroups: TDividerBevel;
    dropPasStringKeywords: TComboBox;
    divKeywords: TDividerBevel;
    divMatchingBrackets: TDividerBevel;
    divWordGroup: TDividerBevel;
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
    procedure chkExtPasKeywordsChange(Sender: TObject);
    procedure chkKWGroupsClickCheck(Sender: TObject);
    procedure chkKWGroupsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure dropPasStringKeywordsChange(Sender: TObject);
    function GeneralPage: TEditorGeneralOptionsFrame; inline;
    function FoldPage: TEditorCodefoldingOptionsFrame; inline;
    procedure MarkupColorLinkClick(Sender: TObject);
    procedure MarkupKeyLinkClick(Sender: TObject);
  private
    { private declarations }
    FDialog: TAbstractOptionsEditorDialog;
    FKWMathHighLighter: TSynCustomFoldHighlighter;
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

procedure TEditorMarkupOptionsFrame.chkExtPasKeywordsChange(Sender: TObject);
begin
  GeneralPage.PasExtendedKeywordsMode := chkExtPasKeywords.Checked;
  GeneralPage.UpdatePrevieEdits;
end;

procedure TEditorMarkupOptionsFrame.chkKWGroupsClickCheck(Sender: TObject);
var
  i, j, idx: Integer;
  CurFoldInfo: TEditorOptionsFoldRecord;
begin
  j := 0;
  CurFoldInfo := EditorOptionsFoldDefaults[lshFreePascal];
  for i := 0 to CurFoldInfo.Count - 1 do begin
    idx := CurFoldInfo.Info^[i].Index;
    if fmMarkup in FKWMathHighLighter.FoldConfig[idx].SupportedModes then begin
      if chkKWGroups.Checked[j] then
        FKWMathHighLighter.FoldConfig[idx].Modes := FKWMathHighLighter.FoldConfig[idx].Modes + [fmMarkup]
      else
        FKWMathHighLighter.FoldConfig[idx].Modes := FKWMathHighLighter.FoldConfig[idx].Modes - [fmMarkup];
      inc(j);
    end;
  end;
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

function TEditorMarkupOptionsFrame.GetTitle: String;
begin
  Result := lisAutoMarkup;
end;

procedure TEditorMarkupOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
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

  divKeyWordGroups.Caption := dlgPasKeywordsMatches;
end;

procedure TEditorMarkupOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  CurFoldInfo: TEditorOptionsFoldRecord;
  i, j, idx: Integer;
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
  end;
  AutoDelayTrackBarChange(nil);

  FKWMathHighLighter := TSynCustomFoldHighlighter(FoldPage.GetHighlighter(lshFreePascal, True));
  CurFoldInfo := EditorOptionsFoldDefaults[lshFreePascal];
  for i := 0 to CurFoldInfo.Count - 1 do begin
    idx := CurFoldInfo.Info^[i].Index;
    if fmMarkup in FKWMathHighLighter.FoldConfig[idx].SupportedModes then begin
      j := chkKWGroups.Items.Add(CurFoldInfo.Info^[i].Name);
      chkKWGroups.Checked[j] := fmMarkup in FKWMathHighLighter.FoldConfig[idx].Modes;
    end;
  end;

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

