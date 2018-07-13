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
unit editor_indent_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  LCLProc, LCLType, StdCtrls, Controls, ExtCtrls, Graphics, ComCtrls, Spin,
  // LazControls
  DividerBevel,
  // SynEdit
  SynEdit, SynBeautifier, SynHighlighterPas, SynEditKeyCmds,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, IDEUtils, SrcEditorIntf,
  // IDE
  EditorOptions, LazarusIDEStrConsts, IDEProcs, KeyMapping,
  editor_keymapping_options, editor_general_options;

type
  TPreviewEditor = TSynEdit;
  { TEditorIndentOptionsFrame }

  TEditorIndentOptionsFrame = class(TAbstractIDEOptionsEditor)
    BlockIndentLink: TLabel;
    BlockIndentComboBox: TComboBox;
    BlockTabIndentComboBox: TComboBox;
    BlockTabIndentLabel: TLabel;
    BlockIndentTypeComboBox: TComboBox;
    BlockIndentLabel: TLabel;
    AutoIndentCheckBox: TCheckBox;
    AutoIndentTypeLabel: TLabel;
    cbSlashExtend: TComboBox;
    CenterLabel1: TLabel;
    cbStringEnableAutoContinue: TCheckBox;
    CommentsGroupDivider: TDividerBevel;
    edStringAutoAppend: TEdit;
    edStringAutoPrefix: TEdit;
    lbStringAutoAppend: TLabel;
    lbStringAutoPrefix: TLabel;
    lblBlockIndentShortcut: TLabel;

    cbAnsiEnableAutoContinue: TCheckBox;
    edAnsiMatch: TEdit;
    edAnsiPrefix: TEdit;
    lbAnsiMatch: TLabel;
    lbAnsiPrefix: TLabel;
    cbAnsiMatchMode: TComboBox;
    cbAnsiIndentMode: TComboBox;
    lbAnsiAlignMax: TLabel;
    edAnsiAlignMax: TSpinEdit;


    cbCurlyEnableAutoContinue: TCheckBox;
    edCurlyMatch: TEdit;
    edCurlyPrefix: TEdit;
    lbCurlyMatch: TLabel;
    lbCurlyPrefix: TLabel;
    cbCurlyMatchMode: TComboBox;
    cbCurlyIndentMode: TComboBox;
    lbCurlyAlignMax: TLabel;
    edCurlyAlignMax: TSpinEdit;

    cbSlashEnableAutoContinue: TCheckBox;
    edSlashMatch: TEdit;
    edSlashPrefix: TEdit;
    lbSlashMatch: TLabel;
    lbSlashPrefix: TLabel;
    cbSlashMatchMode: TComboBox;
    cbSlashIndentMode: TComboBox;
    lbSlashAlignMax: TLabel;
    edSlashAlignMax: TSpinEdit;

    CommentsPageControl: TPageControl;
    TabsGroupDivider: TDividerBevel;
    AutoIndentLink: TLabel;
    CenterLabel:TLabel;
    IndentsGroupDivider: TDividerBevel;
    lblBlockIndentKeys: TLabel;
    TabIndentBlocksCheckBox: TCheckBox;
    SmartTabsCheckBox: TCheckBox;
    tbAnsi: TTabSheet;
    tbCurly: TTabSheet;
    tbShlash: TTabSheet;
    tbString: TTabSheet;
    TabsToSpacesCheckBox: TCheckBox;
    TabWidthsComboBox: TComboBox;
    TabWidthsLabel: TLabel;
    procedure AutoIndentCheckBoxChange(Sender: TObject);
    procedure AutoIndentLinkClick(Sender: TObject);
    procedure AutoIndentLinkMouseEnter(Sender: TObject);
    procedure AutoIndentLinkMouseLeave(Sender: TObject);
    procedure BlockIndentLinkClick(Sender: TObject);
    procedure cbAnsiEnableAutoContinueChange(Sender: TObject);
    procedure cbAnsiIndentModeChange(Sender: TObject);
    procedure cbCurlyEnableAutoContinueChange(Sender: TObject);
    procedure cbCurlyIndentModeChange(Sender: TObject);
    procedure cbSlashEnableAutoContinueChange(Sender: TObject);
    procedure cbSlashIndentModeChange(Sender: TObject);
    procedure cbStringEnableAutoContinueChange(Sender: TObject);
    procedure ComboboxOnChange(Sender: TObject);
    procedure ComboboxOnKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ComboBoxOnExit(Sender: TObject);
    procedure SmartTabsCheckBoxChange(Sender: TObject);
    procedure TabIndentBlocksCheckBoxChange(Sender: TObject);
    procedure TabsToSpacesCheckBoxChange(Sender: TObject);
  private
    FDefaultBookmarkImages: TImageList;
    FDialog: TAbstractOptionsEditorDialog;
    FPasExtendedKeywordsMode: Boolean;
    FPasStringKeywordMode: TSynPasStringMode;
    function DefaultBookmarkImages: TImageList;
    procedure SetExtendedKeywordsMode(const AValue: Boolean);
    procedure SetStringKeywordMode(const AValue: TSynPasStringMode);
    function GeneralPage: TEditorGeneralOptionsFrame; inline;
  public
    procedure SetPreviewOption(AValue: Boolean; AnOption: TSynEditorOption); overload;
    procedure SetPreviewOption(AValue: Boolean; AnOption: TSynEditorOption2); overload;
    procedure UpdatePrevieEdits;

    constructor Create(AOwner: TComponent); override;
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
    // current previewmode
    property PasExtendedKeywordsMode: Boolean
             read FPasExtendedKeywordsMode write SetExtendedKeywordsMode default False;
    property PasStringKeywordMode: TSynPasStringMode
             read FPasStringKeywordMode write SetStringKeywordMode default spsmDefault;
  end;

implementation

{$R *.lfm}

{ TEditorIndentOptionsFrame }

function TEditorIndentOptionsFrame.GetTitle: String;
begin
  Result := dlgEdTabIndent;
end;

procedure TEditorIndentOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  FDialog := ADialog;

  // tabs
  TabsGroupDivider.Caption := dlgIndentsTabsGroupOptions;
  TabsToSpacesCheckBox.Caption := dlgTabsToSpaces;
  TabWidthsLabel.Caption := dlgTabWidths;
  SmartTabsCheckBox.Caption := dlgSmartTabs;

  // indents
  IndentsGroupDivider.Caption := dlgIndentsIndentGroupOptions;
  AutoIndentCheckBox.Caption := dlgAutoIndent;
  AutoIndentTypeLabel.Caption := dlgAutoIndentType;

  lblBlockIndentKeys.Caption := dlgBlockIndentKeys;
  lblBlockIndentShortcut.Caption := '';
  BlockIndentLink.Caption := dlgBlockIndentLink;
  BlockIndentLabel.Caption := dlgBlockIndent;
  BlockTabIndentLabel.Caption := dlgBlockTabIndent;

  BlockIndentTypeComboBox.Items.Add(dlgBlockIndentTypeSpace);
  BlockIndentTypeComboBox.Items.Add(dlgBlockIndentTypeCopy);
  BlockIndentTypeComboBox.Items.Add(dlgBlockIndentTypePos);
  BlockIndentTypeComboBox.Items.Add(dlgBlockIndentTypeTabSpace);
  BlockIndentTypeComboBox.Items.Add(dlgBlockIndentTypeTabOnly);

  TabIndentBlocksCheckBox.Caption := dlgTabIndent;
  AutoIndentLink.Caption := dlgAutoIndentLink;

  // Comments
  CommentsGroupDivider.Caption := dlgCommentIndentGroupOptions;
  tbAnsi.Caption := dlgAnsiCommentTab;
  tbCurly.Caption := dlgCurlyCommentTab;
  tbShlash.Caption := dlgSlashCommentTab;
  tbString.Caption := dlgStringBreakIndentTab;

  cbAnsiEnableAutoContinue.Caption := dlgCommentContinue;
  lbAnsiMatch.Caption := dlgCommentContinueMatch;
  lbAnsiPrefix.Caption := dlgCommentContinuePrefix;
  lbAnsiAlignMax.Caption := dlgCommentAlignMaxToken;

  cbAnsiMatchMode.Items.Clear;
  cbAnsiMatchMode.Items.Add(Format(dlgCommentContinueMatchText, ['(*']));
  cbAnsiMatchMode.Items.Add(Format(dlgCommentContinueMatchToken, ['(*']));
  cbAnsiMatchMode.Items.Add(Format(dlgCommentContinueMatchLine, ['(*']));
  cbAnsiMatchMode.Items.Add(dlgCommentContinueMatchAsterisk);

  cbAnsiIndentMode.Items.Clear;
  cbAnsiIndentMode.Items.Add(dlgCommentContinuePrefixIndDefault);
  cbAnsiIndentMode.Items.Add(dlgCommentContinuePrefixIndMatch);
  cbAnsiIndentMode.Items.Add(dlgCommentContinuePrefixIndNone);


  cbCurlyEnableAutoContinue.Caption := dlgCommentContinue;
  lbCurlyMatch.Caption := dlgCommentContinueMatch;
  lbCurlyPrefix.Caption := dlgCommentContinuePrefix;
  lbCurlyAlignMax.Caption := dlgCommentAlignMaxToken;

  cbCurlyMatchMode.Items.Clear;
  cbCurlyMatchMode.Items.Add(Format(dlgCommentContinueMatchText, ['{']));
  cbCurlyMatchMode.Items.Add(Format(dlgCommentContinueMatchToken, ['{']));
  cbCurlyMatchMode.Items.Add(Format(dlgCommentContinueMatchLine, ['{']));

  cbCurlyIndentMode.Items.Clear;
  cbCurlyIndentMode.Items.Add(dlgCommentContinuePrefixIndDefault);
  cbCurlyIndentMode.Items.Add(dlgCommentContinuePrefixIndMatch);
  cbCurlyIndentMode.Items.Add(dlgCommentContinuePrefixIndNone);


  cbSlashEnableAutoContinue.Caption := dlgCommentContinue;
  lbSlashMatch.Caption := dlgCommentContinueMatch;
  lbSlashPrefix.Caption := dlgCommentContinuePrefix;
  lbSlashAlignMax.Caption := dlgCommentAlignMaxToken;

  cbSlashMatchMode.Items.Clear;
  cbSlashMatchMode.Items.Add(Format(dlgCommentContinueMatchText, ['//']));
  cbSlashMatchMode.Items.Add(Format(dlgCommentContinueMatchToken, ['//']));
  cbSlashMatchMode.Items.Add(Format(dlgCommentContinueMatchLine, ['//']));

  cbSlashIndentMode.Items.Clear;
  cbSlashIndentMode.Items.Add(dlgCommentContinuePrefixIndDefault);
  cbSlashIndentMode.Items.Add(dlgCommentContinuePrefixIndMatch);
  cbSlashIndentMode.Items.Add(dlgCommentContinuePrefixIndNone);

  cbSlashExtend.Items.Clear;
  cbSlashExtend.Items.Add(dlgCommentShlashExtendMatch);
  cbSlashExtend.Items.Add(dlgCommentShlashExtendMatchSplit);
  cbSlashExtend.Items.Add(dlgCommentShlashExtendAlways);
  cbSlashExtend.Items.Add(dlgCommentShlashExtendAlwaysSplit);

  cbStringEnableAutoContinue.Caption := dlgStringEnableAutoContinue;
  lbStringAutoAppend.Caption := dlgStringAutoAppend;
  lbStringAutoPrefix.Caption := dlgStringAutoPrefix;

end;

procedure TEditorIndentOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  K: TKeyCommandRelation;
const
  MatchModeToIdx: array [TSynCommentMatchMode] of integer = (0,1,2,3);
  ExtendModeToIdx: array [TSynCommentExtendMode] of integer = (3,2,3,0,1);
begin
  with AOptions as TEditorOptions do
  begin
    SetComboBoxText(BlockIndentComboBox, IntToStr(BlockIndent), cstCaseInsensitive);
    SetComboBoxText(BlockTabIndentComboBox, IntToStr(BlockTabIndent), cstCaseInsensitive);
    SetComboBoxText(TabWidthsComboBox, IntToStr(TabWidth), cstCaseInsensitive);
    BlockIndentTypeComboBox.ItemIndex := ord(BlockIndentType);

    // tabs, indents
    AutoIndentCheckBox.Checked := eoAutoIndent in SynEditOptions;
    TabIndentBlocksCheckBox.Checked := eoTabIndent in SynEditOptions;
    SmartTabsCheckBox.Checked := eoSmartTabs in SynEditOptions;
    TabsToSpacesCheckBox.Checked := eoTabsToSpaces in SynEditOptions;

    lblBlockIndentShortcut.Caption := '';
    K := KeyMap.FindByCommand(ecBlockIndent);
    if k <> nil then
      lblBlockIndentShortcut.Caption := lblBlockIndentShortcut.Caption +
        KeyAndShiftStateToEditorKeyString(k.ShortcutA)+ ' / ';
    K := KeyMap.FindByCommand(ecBlockUnindent);
    if k <> nil then
      lblBlockIndentShortcut.Caption := lblBlockIndentShortcut.Caption +
        KeyAndShiftStateToEditorKeyString(k.ShortcutA);


    cbAnsiEnableAutoContinue.Checked := AnsiCommentContinueEnabled;
    edAnsiMatch.Text := AnsiCommentMatch;
    edAnsiPrefix.Text := AnsiCommentPrefix;
    cbAnsiMatchMode.ItemIndex := MatchModeToIdx[AnsiCommentMatchMode];
    edAnsiAlignMax.Value := AnsiIndentAlignMax;
    if sciNone in AnsiIndentMode
    then cbAnsiIndentMode.ItemIndex := 2
    else if sciAlignOpen in AnsiIndentMode
    then cbAnsiIndentMode.ItemIndex := 1
    else cbAnsiIndentMode.ItemIndex := 0;
    cbAnsiEnableAutoContinueChange(nil);
    cbAnsiIndentModeChange(nil);

    cbCurlyEnableAutoContinue.Checked := CurlyCommentContinueEnabled;
    edCurlyMatch.Text := CurlyCommentMatch;
    edCurlyPrefix.Text := CurlyCommentPrefix;
    cbCurlyMatchMode.ItemIndex := MatchModeToIdx[CurlyCommentMatchMode];
    edCurlyAlignMax.Value := CurlyIndentAlignMax;
    if sciNone in CurlyIndentMode
    then cbCurlyIndentMode.ItemIndex := 2
    else if sciAlignOpen in CurlyIndentMode
    then cbCurlyIndentMode.ItemIndex := 1
    else cbCurlyIndentMode.ItemIndex := 0;
    cbCurlyEnableAutoContinueChange(nil);
    cbCurlyIndentModeChange(nil);


    cbSlashEnableAutoContinue.Checked := SlashCommentContinueEnabled;
    edSlashMatch.Text := SlashCommentMatch;
    edSlashPrefix.Text := SlashCommentPrefix;
    cbSlashMatchMode.ItemIndex := MatchModeToIdx[SlashCommentMatchMode];
    edSlashAlignMax.Value := SlashIndentAlignMax;
    if sciNone in SlashIndentMode
    then cbSlashIndentMode.ItemIndex := 2
    else if sciAlignOpen in SlashIndentMode
    then cbSlashIndentMode.ItemIndex := 1
    else cbSlashIndentMode.ItemIndex := 0;

    cbSlashExtend.ItemIndex := ExtendModeToIdx[SlashCommentExtend];
    cbSlashEnableAutoContinueChange(nil);
    cbSlashIndentModeChange(nil);

    cbStringEnableAutoContinue.Checked := StringBreakEnabled;
    edStringAutoAppend.Text := StringBreakAppend;
    edStringAutoPrefix.Text := StringBreakPrefix;
    cbStringEnableAutoContinueChange(nil);

  end;
end;

procedure TEditorIndentOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);

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
const
  IdxToMatchMode: array [0..3] of TSynCommentMatchMode = (scmMatchAfterOpening,scmMatchOpening,scmMatchWholeLine,scmMatchAtAsterisk);
  IdxToExtendMode: array [0..3] of TSynCommentExtendMode= (sceMatching,sceMatchingSplitLine,sceAlways,sceSplitLine);
begin
  with AOptions as TEditorOptions do
  begin
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
    if i < 0 then
      i := 0;
    if i > 20 then
      i := 20;
    BlockIndent := i;

    i := StrToIntDef(BlockTabIndentComboBox.Text, 0);
    if i < 0 then
      i := 0;
    if i > 20 then
      i := 20;
    BlockTabIndent := i;

    BlockIndentType := TSynBeautifierIndentType(BlockIndentTypeComboBox.ItemIndex);

    AnsiCommentContinueEnabled := cbAnsiEnableAutoContinue.Checked;
    AnsiCommentMatch := edAnsiMatch.Text;
    AnsiCommentPrefix := edAnsiPrefix.Text;
    AnsiCommentMatchMode := IdxToMatchMode[cbAnsiMatchMode.ItemIndex];
    AnsiIndentAlignMax := edAnsiAlignMax.Value;
    case cbAnsiIndentMode.ItemIndex of
      0: AnsiIndentMode := [sciAddTokenLen, sciAddPastTokenIndent,
                            sciAlignOnlyTokenLen, sciAlignOnlyPastTokenIndent,
                            sciMatchOnlyPastTokenIndent
                           ];
      1: AnsiIndentMode := [sciAlignOpen, sciAddTokenLen, sciAddPastTokenIndent,
                            sciAlignOnlyTokenLen, sciAlignOnlyPastTokenIndent,
                            sciMatchOnlyPastTokenIndent
                           ];
      2: AnsiIndentMode := [sciNone];
    end;

    CurlyCommentContinueEnabled := cbCurlyEnableAutoContinue.Checked;
    CurlyCommentMatch := edCurlyMatch.Text;
    CurlyCommentPrefix := edCurlyPrefix.Text;
    CurlyCommentMatchMode := IdxToMatchMode[cbCurlyMatchMode.ItemIndex];
    CurlyIndentAlignMax := edCurlyAlignMax.Value;
    case cbCurlyIndentMode.ItemIndex of
      0: CurlyIndentMode := [sciAddTokenLen, sciAddPastTokenIndent,
                             sciAlignOnlyTokenLen, sciAlignOnlyPastTokenIndent,
                             sciMatchOnlyPastTokenIndent
                            ];
      1: CurlyIndentMode := [sciAlignOpen, sciAddTokenLen, sciAddPastTokenIndent];
      2: CurlyIndentMode := [sciNone];
    end;

    SlashCommentContinueEnabled := cbSlashEnableAutoContinue.Checked;
    SlashCommentMatch := edSlashMatch.Text;
    SlashCommentPrefix := edSlashPrefix.Text;
    SlashCommentMatchMode := IdxToMatchMode[cbSlashMatchMode.ItemIndex];
    SlashIndentAlignMax := edSlashAlignMax.Value;
    case cbSlashIndentMode.ItemIndex of
      0: SlashIndentMode := [sciAddTokenLen, sciAddPastTokenIndent,
                             sciAlignOnlyTokenLen, sciAlignOnlyPastTokenIndent,
                             sciMatchOnlyPastTokenIndent
                            ];
      1: SlashIndentMode := [sciAlignOpen, sciAddTokenLen, sciAddPastTokenIndent];
      2: SlashIndentMode := [sciNone];
    end;

    SlashCommentExtend := IdxToExtendMode[cbSlashExtend.ItemIndex];

    StringBreakEnabled := cbStringEnableAutoContinue.Checked;
    StringBreakAppend := edStringAutoAppend.Text;
    StringBreakPrefix := edStringAutoPrefix.Text;

  end;
end;

class function TEditorIndentOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEditorOptions;
end;

procedure TEditorIndentOptionsFrame.SetPreviewOption(AValue: Boolean; AnOption: TSynEditorOption);
var
  a: Integer;
begin
  with GeneralPage do
    for a := Low(PreviewEdits) to High(PreviewEdits) do
    begin
      if PreviewEdits[a] <> nil then
        if AValue then
          PreviewEdits[a].Options := PreviewEdits[a].Options + [AnOption]
        else
          PreviewEdits[a].Options := PreviewEdits[a].Options - [AnOption];
    end;
end;

procedure TEditorIndentOptionsFrame.SetPreviewOption(AValue: Boolean; AnOption: TSynEditorOption2);
var
  a: Integer;
begin
  with GeneralPage do
    for a := Low(PreviewEdits) to High(PreviewEdits) do
    begin
      if PreviewEdits[a] <> nil then
        if AValue then
          PreviewEdits[a].Options2 := PreviewEdits[a].Options2 + [AnOption]
        else
          PreviewEdits[a].Options2 := PreviewEdits[a].Options2 - [AnOption];
    end;
end;

procedure TEditorIndentOptionsFrame.UpdatePrevieEdits;
var
  a: Integer;
begin
  with GeneralPage do
    for a := Low(PreviewEdits) to High(PreviewEdits) do
      if PreviewEdits[a].Highlighter is TSynPasSyn then begin
        TSynPasSyn(PreviewEdits[a].Highlighter).ExtendedKeywordsMode := PasExtendedKeywordsMode;
        TSynPasSyn(PreviewEdits[a].Highlighter).StringKeywordMode := PasStringKeywordMode;
      end;
end;

procedure TEditorIndentOptionsFrame.ComboboxOnChange(Sender: TObject);
var
  ComboBox: TComboBox absolute Sender;
begin
  if ComboBox.Items.IndexOf(ComboBox.Text) >= 0 then
    ComboBoxOnExit(Sender);
end;

procedure TEditorIndentOptionsFrame.AutoIndentCheckBoxChange(Sender: TObject);
begin
  SetPreviewOption(AutoIndentCheckBox.Checked, eoAutoIndent);
end;

procedure TEditorIndentOptionsFrame.AutoIndentLinkClick(Sender: TObject);
begin
  FDialog.OpenEditor(GroupCodetools,CdtOptionsGeneral);
end;

procedure TEditorIndentOptionsFrame.AutoIndentLinkMouseEnter(Sender: TObject);
begin
  (Sender as TLabel).Font.Underline := True;
  (Sender as TLabel).Font.Color := clRed;
end;

procedure TEditorIndentOptionsFrame.AutoIndentLinkMouseLeave(Sender: TObject);
begin
  (Sender as TLabel).Font.Underline := False;
  (Sender as TLabel).Font.Color := clBlue;
end;

procedure TEditorIndentOptionsFrame.BlockIndentLinkClick(Sender: TObject);
var
  col: TEditorKeymappingOptionsFrame;
begin
  col := TEditorKeymappingOptionsFrame(FDialog.FindEditor(TEditorKeymappingOptionsFrame));
  if col = nil then exit;
  FDialog.OpenEditor(TEditorKeymappingOptionsFrame);
  col.SelectByIdeCommand(ecBlockIndent);
end;

procedure TEditorIndentOptionsFrame.cbAnsiEnableAutoContinueChange(Sender: TObject);
begin
  edAnsiMatch.Enabled := cbAnsiEnableAutoContinue.Checked;
  edAnsiPrefix.Enabled := cbAnsiEnableAutoContinue.Checked;
  cbAnsiMatchMode.Enabled := cbAnsiEnableAutoContinue.Checked;
  cbAnsiIndentMode.Enabled := cbAnsiEnableAutoContinue.Checked;
  edAnsiAlignMax.Enabled := cbAnsiEnableAutoContinue.Checked;
end;

procedure TEditorIndentOptionsFrame.cbAnsiIndentModeChange(Sender: TObject);
begin
  case cbAnsiIndentMode.ItemIndex of
    1:   lbAnsiAlignMax.Caption := dlgCommentAlignMaxToken;
    else lbAnsiAlignMax.Caption := dlgCommentAlignMaxDefault;
  end;
end;

procedure TEditorIndentOptionsFrame.cbCurlyEnableAutoContinueChange(Sender: TObject);
begin
  edCurlyMatch.Enabled := cbCurlyEnableAutoContinue.Checked;
  edCurlyPrefix.Enabled := cbCurlyEnableAutoContinue.Checked;
  cbCurlyMatchMode.Enabled := cbCurlyEnableAutoContinue.Checked;
  cbCurlyIndentMode.Enabled := cbCurlyEnableAutoContinue.Checked;
  edCurlyAlignMax.Enabled := cbCurlyEnableAutoContinue.Checked;
end;

procedure TEditorIndentOptionsFrame.cbCurlyIndentModeChange(Sender: TObject);
begin
  case cbCurlyIndentMode.ItemIndex of
    1:   lbCurlyAlignMax.Caption := dlgCommentAlignMaxToken;
    else lbCurlyAlignMax.Caption := dlgCommentAlignMaxDefault;
  end;
end;

procedure TEditorIndentOptionsFrame.cbSlashEnableAutoContinueChange(Sender: TObject);
begin
  edSlashMatch.Enabled := cbSlashEnableAutoContinue.Checked;
  edSlashPrefix.Enabled := cbSlashEnableAutoContinue.Checked;
  cbSlashMatchMode.Enabled := cbSlashEnableAutoContinue.Checked;
  cbSlashIndentMode.Enabled := cbSlashEnableAutoContinue.Checked;
  cbSlashExtend.Enabled := cbSlashEnableAutoContinue.Checked;
  edSlashAlignMax.Enabled := cbSlashEnableAutoContinue.Checked;
end;

procedure TEditorIndentOptionsFrame.cbSlashIndentModeChange(Sender: TObject);
begin
  case cbSlashIndentMode.ItemIndex of
    1:   lbSlashAlignMax.Caption := dlgCommentAlignMaxToken;
    else lbSlashAlignMax.Caption := dlgCommentAlignMaxDefault;
  end;
end;

procedure TEditorIndentOptionsFrame.cbStringEnableAutoContinueChange(Sender: TObject);
begin
  edStringAutoAppend.Enabled := cbStringEnableAutoContinue.Checked;
  edStringAutoPrefix.Enabled := cbStringEnableAutoContinue.Checked;
end;

procedure TEditorIndentOptionsFrame.ComboboxOnKeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key = VK_S) then
    ComboBoxOnExit(Sender);
end;

procedure TEditorIndentOptionsFrame.ComboBoxOnExit(Sender: TObject);
var
  NewVal, a: Integer;
begin
  if Sender = BlockIndentComboBox then
  begin
    NewVal := StrToIntDef(BlockIndentComboBox.Text, GeneralPage.PreviewEdits[1].BlockIndent);
    // Todo: min/max
    SetComboBoxText(BlockIndentComboBox, IntToStr(NewVal), cstCaseInsensitive);
    for a := Low(GeneralPage.PreviewEdits) to High(GeneralPage.PreviewEdits) do
      if GeneralPage.PreviewEdits[a] <> nil then
        GeneralPage.PreviewEdits[a].BlockIndent := NewVal;
  end
  else
  if Sender = BlockTabIndentComboBox then
  begin
    NewVal := StrToIntDef(BlockTabIndentComboBox.Text, GeneralPage.PreviewEdits[1].BlockTabIndent);
    // Todo: min/max
    SetComboBoxText(BlockTabIndentComboBox, IntToStr(NewVal), cstCaseInsensitive);
    for a := Low(GeneralPage.PreviewEdits) to High(GeneralPage.PreviewEdits) do
      if GeneralPage.PreviewEdits[a] <> nil then
        GeneralPage.PreviewEdits[a].BlockTabIndent := NewVal;
  end
  else
  if Sender = TabWidthsComboBox then
  begin
    NewVal := StrToIntDef(TabWidthsComboBox.Text, GeneralPage.PreviewEdits[1].TabWidth);
    SetComboBoxText(TabWidthsComboBox, IntToStr(NewVal), cstCaseInsensitive);
    for a := Low(GeneralPage.PreviewEdits) to High(GeneralPage.PreviewEdits) do
      if GeneralPage.PreviewEdits[a] <> nil then
        GeneralPage.PreviewEdits[a].TabWidth := NewVal;
  end
end;

procedure TEditorIndentOptionsFrame.SmartTabsCheckBoxChange(Sender: TObject);
begin
  SetPreviewOption(SmartTabsCheckBox.Checked, eoSmartTabs);
end;

procedure TEditorIndentOptionsFrame.TabIndentBlocksCheckBoxChange(
  Sender: TObject);
begin
  SetPreviewOption(TabIndentBlocksCheckBox.Checked, eoTabIndent);
end;

procedure TEditorIndentOptionsFrame.TabsToSpacesCheckBoxChange(Sender: TObject);
begin
  SetPreviewOption(TabsToSpacesCheckBox.Checked, eoTabsToSpaces);
end;

function TEditorIndentOptionsFrame.DefaultBookmarkImages: TImageList;
var
  i: integer;
begin
  if FDefaultBookmarkImages = nil then
  begin
    FDefaultBookmarkImages := TImageList.Create(Self);
    FDefaultBookmarkImages.Width := 11;
    FDefaultBookmarkImages.Height := 11;
    for i in TBookmarkNumRange do
      FDefaultBookmarkImages.AddResourceName(HInstance, 'bookmark' + IntToStr(i));
  end;
  Result := FDefaultBookmarkImages;
end;

procedure TEditorIndentOptionsFrame.SetExtendedKeywordsMode(const AValue: Boolean);
begin
  if FPasExtendedKeywordsMode = AValue then exit;
  FPasExtendedKeywordsMode := AValue;
  UpdatePrevieEdits;
end;

procedure TEditorIndentOptionsFrame.SetStringKeywordMode(const AValue: TSynPasStringMode);
begin
  if FPasStringKeywordMode = AValue then exit;
  FPasStringKeywordMode := AValue;
  UpdatePrevieEdits;
end;

function TEditorIndentOptionsFrame.GeneralPage: TEditorGeneralOptionsFrame;
begin
  Result := TEditorGeneralOptionsFrame(FDialog.FindEditor(TEditorGeneralOptionsFrame));
end;

constructor TEditorIndentOptionsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if EditorOpts <> nil then begin
    FPasExtendedKeywordsMode := EditorOpts.PasExtendedKeywordsMode;
    FPasStringKeywordMode := EditorOpts.PasStringKeywordMode;
  end;
end;

initialization
  RegisterIDEOptionsEditor(GroupEditor, TEditorIndentOptionsFrame, EdtOptionsIndent, EdtOptionsGeneral);
end.

