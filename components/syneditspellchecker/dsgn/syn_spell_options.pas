{
 *****************************************************************************
  This file is part of the SynEditSpellCheckerDsgn package from the Lazarus IDE.

  This content of this file is licensed: Modified LGPL-2
  Or at the users choice: Modified LGPL-3
  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  Alternatively, the contents of this file may be used under the terms of the
  Mozilla Public License Version 1.1 or 2.0 http://www.mozilla.org/MPL/

  A copy used under either License can have the other Licenses removed from this
  header. A note should be added that the original file is available with the
  above choice of License.
  Used units may have to be amended according to the choice.
 *****************************************************************************
}
unit syn_spell_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, EditBtn, ExtCtrls, Spin, IDEOptEditorIntf,
  LazIDEIntf, IDEOptionsIntf, LazFileUtils, SynSpellCheckDsgnStrings, SynSpellCheckDsgnOptions,
  SynSpellDictionary, DividerBevel, ShiftStateSelector, SynASpellDef, SynSpellCheckWordBreaker,
  SynEditMouseCmds;

type

  { TSynSpellOptionsFrame }

  TSynSpellOptionsFrame = class(TAbstractIDEOptionsEditor)
    anchTIC3: TLabel;
    anchTIC4: TLabel;
    anchTIC5: TLabel;
    anchTIC6: TLabel;
    anchUpChars1: TLabel;
    anchWMT3: TLabel;
    anchWMT4: TLabel;
    anchWMT5: TLabel;
    anchWMT6: TLabel;
    anchWXC10: TLabel;
    anchWXC11: TLabel;
    anchWXC12: TLabel;
    anchWXC13: TLabel;
    anchWXC14: TLabel;
    anchWXC3: TLabel;
    anchWXC4: TLabel;
    anchWXC5: TLabel;
    anchWXC6: TLabel;
    anchWXC7: TLabel;
    anchWXC8: TLabel;
    anchWXC9: TLabel;
    bvDictionary: TDividerBevel;
    bvMouse: TDividerBevel;
    bvPartLeadMix: TDividerBevel;
    bvPartUpLow: TDividerBevel;
    bvPrefix: TDividerBevel;
    bvRules: TDividerBevel;
    bvToken: TDividerBevel;
    cbIgnoreUpPrefixCase: TCheckBox;
    cbIgnoreLowPrefixCase: TCheckBox;
    cbMouseSuggest: TCheckBox;
    cbTokenIgnoreXCaps: TCheckBox;
    cbTokenAllowXCaps: TCheckBox;
    cbWordMultiToken: TCheckBox;
    cbIgnoreUpArticle: TCheckBox;
    cbIgnoreLowArticle: TCheckBox;
    cbWordXCaps: TCheckBox;
    cbWordIgnoreXCaps: TCheckBox;
    cbIgnoreUpArticleCase: TCheckBox;
    cbIgnoreLowArticleCase: TCheckBox;
    dropMouseSuggest: TComboBox;
    EdDictPath: TDirectoryEdit;
    lbLowTokenIgnoreLen: TLabel;
    edUpChars: TEdit;
    edLowChars: TEdit;
    edLang: TEdit;
    EdLibPath: TFileNameEdit;
    edIgnorePrefix: TEdit;
    EdUserDictPath: TFileNameEdit;
    anchUpChars: TLabel;
    anchLowChars: TLabel;
    anchTIC2: TLabel;
    anchWIC1: TLabel;
    anchWIC2: TLabel;
    anchWXC1: TLabel;
    anchWXC2: TLabel;
    anchWMT1: TLabel;
    anchWMT2: TLabel;
    anchTIC1: TLabel;
    lbHeadPartL1: TLabel;
    lbHeadPartL2: TLabel;
    lbHeadPartL3: TLabel;
    lbHeadPartL4: TLabel;
    lbHeadPartL5: TLabel;
    lbHeadPartR1: TLabel;
    lbHeadPartR2: TLabel;
    lbHeadPartR3: TLabel;
    lbHeadPartR4: TLabel;
    lbHeadPartR5: TLabel;
    lbHeadTokenL: TLabel;
    lbHeadPartL: TLabel;
    lbHeadTokenL1: TLabel;
    lbHeadTokenR: TLabel;
    lbHeadPartR: TLabel;
    lbHeadTokenR1: TLabel;
    lbPrefixRemain: TLabel;
    lbUpChars: TLabel;
    lbLowChars: TLabel;
    lbIgnorePrefix: TLabel;
    lbWordIgnoreLen: TLabel;
    lbTokenIgnoreLen: TLabel;
    lbUsrDict: TLabel;
    lbUserDictPath: TLabel;
    lbLoadedWarn: TLabel;
    lbLibraryPath: TLabel;
    lbDictionaryPath: TLabel;
    lbLanguage: TLabel;
    lbLP: TLabel;
    lbDP: TLabel;
    lbL: TLabel;
    ErrTxt: TMemo;
    lbPartLeadMin: TLabel;
    lbPartLowIgnoreLen: TLabel;
    lbPartUpIgnoreRemain: TLabel;
    lbPartLowIgnoreRemain: TLabel;
    lbPartMixLen: TLabel;
    lbPartLeadIgnoreLen: TLabel;
    lbPartMixIgnoreLen: TLabel;
    lbPartLeadIgnoreRemain: TLabel;
    lbPartMixIgnoreRemain: TLabel;
    lbPartUpLen: TLabel;
    lbPartLowLen: TLabel;
    lbPartUpIgnoreLen: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    shMouseSuggest: TShiftStateSelector;
    spinPrefixRemain: TSpinEdit;
    spinLowTokenIgnoreLen: TSpinEdit;
    spinWordIgnoreLen: TSpinEdit;
    spinTokenIgnoreLen: TSpinEdit;
    spinPartLeadLen: TSpinEdit;
    spinPartLowIgnoreLen: TSpinEdit;
    spinPartUpIgnoreRemain: TSpinEdit;
    spinPartLowIgnoreRemain: TSpinEdit;
    spinPartMixLen: TSpinEdit;
    spinPartLeadIgnoreLen: TSpinEdit;
    spinPartMixIgnoreLen: TSpinEdit;
    spinPartLeadIgnoreRemain: TSpinEdit;
    spinPartMixIgnoreRemain: TSpinEdit;
    spinPartUpLen: TSpinEdit;
    spinPartLowLen: TSpinEdit;
    spinPartUpIgnoreLen: TSpinEdit;
    procedure cbMouseSuggestChange(Sender: TObject);
    procedure EdASpellOptsEditingDone(Sender: TObject);
  private
    FASpellOpts: TSynSpellDictionaryASpell;
    procedure UpdateWarnings(ALastErr: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

  procedure Register;

implementation

{$R *.lfm}

procedure Register;
begin
  SynSpellOptions.Load;
  RegisterIDEOptionsEditor(GroupEditor, TSynSpellOptionsFrame, 9999);
end;

{ TSynSpellOptionsFrame }

procedure TSynSpellOptionsFrame.EdASpellOptsEditingDone(Sender: TObject);
begin
  FASpellOpts.LibraryName       := EdLibPath.FileName;
  FASpellOpts.DictionaryDir     := EdDictPath.Directory;
  FASpellOpts.Language          := edLang.Text;
  FASpellOpts.PersonalWordsFile := EdUserDictPath.Text;

  FASpellOpts.Load;
  UpdateWarnings(FASpellOpts.LastError);
end;

procedure TSynSpellOptionsFrame.cbMouseSuggestChange(Sender: TObject);
begin
  dropMouseSuggest.Enabled := cbMouseSuggest.Checked;
  shMouseSuggest.Enabled := cbMouseSuggest.Checked;
end;

procedure TSynSpellOptionsFrame.UpdateWarnings(ALastErr: String);
begin
  ErrTxt.Text := ALastErr;
  ErrTxt.Visible := ALastErr <> '';

  lbLoadedWarn.Caption := Format(SynSpellOptCurrentLoadedLibraryIsSRe, [LibLoadedPath]);
  lbLoadedWarn.Visible := LibLoadedPath <> FASpellOpts.LibraryName;
end;

constructor TSynSpellOptionsFrame.Create(AOwner: TComponent);
begin
  FASpellOpts := TSynSpellDictionaryASpell.Create;
  inherited Create(AOwner);
end;

destructor TSynSpellOptionsFrame.Destroy;
begin
  inherited Destroy;
  FASpellOpts.ReleaseReference;
end;

function TSynSpellOptionsFrame.GetTitle: String;
begin
  Result := SynSpellOptSpellChecking;
end;

procedure TSynSpellOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  bvDictionary.Caption := SynSpellOptDictionary;
  lbLibraryPath.Caption := SynSpellOptLibraryNameAndPath;
  lbDictionaryPath.Caption := SynSpellOptDictionaryPath;
  lbLanguage.Caption := SynSpellOptLanguage;
  lbUserDictPath.Caption := SynSpellOptPersonalDictionaryFile;

  bvMouse.Caption := SynSpellOptMouse;
  cbMouseSuggest.Caption := SynSpellOptSuggestionAndAddToPersona;
  dropMouseSuggest.Items.Clear;
  dropMouseSuggest.Items.Add(SynSpellOptLeft);
  dropMouseSuggest.Items.Add(SynSpellOptRight);
  dropMouseSuggest.Items.Add(SynSpellOptMiddle);

  bvRules.Caption := SynSpellOptSyntaxRules;
  bvToken.Caption := SynSpellOptWordsAndWordPartsSplitAtC;

  lbUpChars.Caption := SynSpellOptUnicodeUpperChars;
  lbUpChars.Hint := SynSpellOptUpperCharsOtherThanAZUsed;
  lbLowChars.Caption := SynSpellOptUnicodeLowerChars;
  lbLowChars.Hint := SynSpellOptLowerCharsOtherThanAZUsed;

  cbWordMultiToken.Caption := SynSpellOptAlsoCheckTheWholeWordEven;
  cbWordXCaps.Caption := SynSpellOptCheckWordsWithSeveralLead;
  lbWordIgnoreLen.Caption := SynSpellOptIgnoreWordsUpToChars;
  cbWordIgnoreXCaps.Caption := SynSpellOptAlsoIgnoreWordsWithSevera;
  lbTokenIgnoreLen.Caption := SynSpellOptIgnoreWordPartsUpToChars;
  cbTokenAllowXCaps.Caption := SynSpellOptAlsoCheckWordPartsWithSev;
  cbTokenIgnoreXCaps.Caption := SynSpellOptAlsoIgnoreWordPartsWithSe;
  lbLowTokenIgnoreLen.Caption := SynSpellOptIgnoreLeadingLowercaseCha;


  bvPartLeadMix.Caption := SynSpellOptSubPartsSOMEThingUpperLea;
  bvPartLeadMix.Hint := SynSpellOptTryToFindValidWordsBySpli;

  lbPartLeadMin.Caption := SynSpellOptRequireMinimumLenForLEAD;
  lbPartLeadIgnoreLen.Caption := SynSpellOptIgnoreLEADUpToLen;
  lbPartLeadIgnoreRemain.Caption := SynSpellOptOnlyIgnoreLEADIfCapitaliz;
  lbPartMixLen.Caption := SynSpellOptRequireMinimumLenForCapit;
  lbPartMixIgnoreLen.Caption := SynSpellOptIgnoreCapitalizedUpToLen;
  lbPartMixIgnoreRemain.Caption := SynSpellOptOnlyIgnoreCapitalizedIfLE;

  bvPartUpLow.Caption := SynSpellOptSubPartsUPPERlower;
  bvPartUpLow.Hint := SynSpellOptTryToFindValidWordsBySpli2;

  lbPartUpLen.Caption := SynSpellOptRequireMinimumLenForUPPER;
  lbPartUpIgnoreLen.Caption := SynSpellOptIgnoreUPPERUpToLen;
  lbPartUpIgnoreRemain.Caption := SynSpellOptOnlyIgnoreUPPERIfLowerHas;
  lbPartLowLen.Caption := SynSpellOptRequireMinimumLenForLower;
  lbPartLowIgnoreLen.Caption := SynSpellOptIgnoreLowerUpToLen;
  lbPartLowIgnoreRemain.Caption := SynSpellOptOnlyIgnoreLowerIfUPPERHas;


  bvPrefix.Caption := SynSpellOptStripPrefix;

  cbIgnoreUpArticle.Caption := SynSpellOptIgnoreArticleAAnIfStartin;
  cbIgnoreUpArticleCase.Caption := SynSpellOptOnlyIgnoreUpperArticlesIf;
  cbIgnoreLowArticle.Caption := SynSpellOptIgnoreArticleAAnIfStartin2;
  cbIgnoreLowArticleCase.Caption := SynSpellOptOnlyIgnoreLowerArticlesIf;
  lbIgnorePrefix.Caption := SynSpellOptListOfCharsPrefixToIgnore;
  cbIgnoreUpPrefixCase.Caption := SynSpellOptOnlyIgnoreUpperPrefixChar;
  cbIgnoreLowPrefixCase.Caption := SynSpellOptOnlyIgnoreLowerPrefixChar;
  lbPrefixRemain.Caption := SynSpellOptOnlyRemovePrefixArticleIf;


end;

procedure TSynSpellOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  co: TSynSpellWordCheckerSourceCode;
begin
  FASpellOpts.Assign(SynSpellOptions.ASpellOpts);
  EdLibPath.FileName  := FASpellOpts.LibraryName;
  EdDictPath.Directory := FASpellOpts.DictionaryDir;
  edLang.Text         := FASpellOpts.Language;
  EdUserDictPath.Text := FASpellOpts.PersonalWordsFile;

  cbMouseSuggest.Checked := SynSpellOptions.EnablePopupMenu;
  dropMouseSuggest.ItemIndex := ord(SynSpellOptions.MouseButton);
  shMouseSuggest.ShiftSelection := SynSpellOptions.MouseShift;
  shMouseSuggest.ShiftMask := SynSpellOptions.MouseShiftMask;

  FASpellOpts.Load;
  UpdateWarnings(FASpellOpts.LastError);

  co := SynSpellOptions.CheckerOpts;

  edUpChars.Text                 := co.SpecialUpperLetters;
  edLowChars.Text                := co.SpecialLowerLetters;
  cbWordMultiToken.Checked       := coAllowMultiTokenWord in co.Options;
  cbWordXCaps.Checked            := coAllowWordWithXCaps in co.Options;
  spinWordIgnoreLen.Value        := co.IgnoreShortWord;
  cbWordIgnoreXCaps.Checked      := coAllowIgnoreWordWithXCaps in co.Options;
  spinTokenIgnoreLen.Value       := co.IgnoreShortToken;
  cbTokenAllowXCaps.Checked      := coAllowTokenWithXCaps in co.Options;
  cbTokenIgnoreXCaps.Checked     := coAllowIgnoreTokenWithXCaps in co.Options;
  spinLowTokenIgnoreLen.Value    := co.IgnoreLowerStart;

  spinPartLeadLen.Value          := co.PartLeadConstraints.MinLen;
  spinPartLeadIgnoreLen.Value    := co.PartLeadConstraints.IgnoreLen;
  spinPartLeadIgnoreRemain.Value := co.PartLeadConstraints.IgnoreRemainderLen;
  spinPartMixLen.Value           := co.PartMixedConstraints.MinLen;
  spinPartMixIgnoreLen.Value     := co.PartMixedConstraints.IgnoreLen;
  spinPartMixIgnoreRemain.Value  := co.PartMixedConstraints.IgnoreRemainderLen;
  spinPartUpLen.Value            := co.PartUpperConstraints.MinLen;
  spinPartUpIgnoreLen.Value      := co.PartUpperConstraints.IgnoreLen;
  spinPartUpIgnoreRemain.Value   := co.PartUpperConstraints.IgnoreRemainderLen;
  spinPartLowLen.Value           := co.PartLowerConstraints.MinLen;
  spinPartLowIgnoreLen.Value     := co.PartLowerConstraints.IgnoreLen;
  spinPartLowIgnoreRemain.Value  := co.PartLowerConstraints.IgnoreRemainderLen;

  cbIgnoreUpArticle.Checked      := coIgnoreUpperArticle in co.Options;
  cbIgnoreUpArticleCase.Checked  := coIgnoreUpperArticleMustMatchCase in co.Options;
  cbIgnoreLowArticle.Checked     := coIgnoreLowerArticle in co.Options;
  cbIgnoreLowArticleCase.Checked := coIgnoreLowerArticleMustMatchCase in co.Options;
  edIgnorePrefix.Text            := co.IgnoreLettersAtStart;
  cbIgnoreUpPrefixCase.Checked   := coIgnoredUpperStartMustMatchCase in co.Options;
  cbIgnoreLowPrefixCase.Checked  := coIgnoredLowerStartMustMatchCase in co.Options;
  spinPrefixRemain.Value         := co.IgnoreAtStartMinRemainderLen;
end;

procedure TSynSpellOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  co: TSynSpellWordCheckerSourceCode;
begin
  SynSpellOptions.EnablePopupMenu := cbMouseSuggest.Checked;
  SynSpellOptions.MouseButton := TSynMouseButton(dropMouseSuggest.ItemIndex);
  SynSpellOptions.MouseShift := shMouseSuggest.ShiftSelection;
  SynSpellOptions.MouseShiftMask := shMouseSuggest.ShiftMask;

  FASpellOpts.LibraryName       := EdLibPath.FileName;
  FASpellOpts.DictionaryDir     := EdDictPath.Directory;
  FASpellOpts.Language          := edLang.Text;
  FASpellOpts.PersonalWordsFile := EdUserDictPath.Text;

  SynSpellOptions.ASpellOpts.Assign(FASpellOpts);

  co := SynSpellOptions.CheckerOpts;

  co.SpecialUpperLetters := edUpChars.Text;
  co.SpecialLowerLetters := edLowChars.Text;
  if cbWordMultiToken.Checked
  then co.Options := co.Options + [coAllowMultiTokenWord]
  else co.Options := co.Options - [coAllowMultiTokenWord];
  if cbWordXCaps.Checked
  then co.Options := co.Options + [coAllowWordWithXCaps]
  else co.Options := co.Options - [coAllowWordWithXCaps];
  co.IgnoreShortWord := spinWordIgnoreLen.Value;
  if cbWordIgnoreXCaps.Checked
  then co.Options := co.Options + [coAllowIgnoreWordWithXCaps]
  else co.Options := co.Options - [coAllowIgnoreWordWithXCaps];
  co.IgnoreShortToken := spinTokenIgnoreLen.Value;
  if cbTokenAllowXCaps.Checked
  then co.Options := co.Options + [coAllowTokenWithXCaps]
  else co.Options := co.Options - [coAllowTokenWithXCaps];
  if cbTokenIgnoreXCaps.Checked
  then co.Options := co.Options + [coAllowIgnoreTokenWithXCaps]
  else co.Options := co.Options - [coAllowIgnoreTokenWithXCaps];
  co.IgnoreLowerStart := spinLowTokenIgnoreLen.Value;

  co.PartLeadConstraints.MinLen := spinPartLeadLen.Value;
  co.PartLeadConstraints.IgnoreLen := spinPartLeadIgnoreLen.Value;
  co.PartLeadConstraints.IgnoreRemainderLen := spinPartLeadIgnoreRemain.Value;
  co.PartMixedConstraints.MinLen := spinPartMixLen.Value;
  co.PartMixedConstraints.IgnoreLen := spinPartMixIgnoreLen.Value;
  co.PartMixedConstraints.IgnoreRemainderLen := spinPartMixIgnoreRemain.Value;
  co.PartUpperConstraints.MinLen := spinPartUpLen.Value;
  co.PartUpperConstraints.IgnoreLen := spinPartUpIgnoreLen.Value;
  co.PartUpperConstraints.IgnoreRemainderLen := spinPartUpIgnoreRemain.Value;
  co.PartLowerConstraints.MinLen := spinPartLowLen.Value;
  co.PartLowerConstraints.IgnoreLen := spinPartLowIgnoreLen.Value;
  co.PartLowerConstraints.IgnoreRemainderLen := spinPartLowIgnoreRemain.Value;

  if cbIgnoreUpArticle.Checked
  then co.Options := co.Options + [coIgnoreUpperArticle]
  else co.Options := co.Options - [coIgnoreUpperArticle];
  if cbIgnoreUpArticleCase.Checked
  then co.Options := co.Options + [coIgnoreUpperArticleMustMatchCase]
  else co.Options := co.Options - [coIgnoreUpperArticleMustMatchCase];
  if cbIgnoreLowArticle.Checked
  then co.Options := co.Options + [coIgnoreLowerArticle]
  else co.Options := co.Options - [coIgnoreLowerArticle];
  if cbIgnoreLowArticleCase.Checked
  then co.Options := co.Options + [coIgnoreLowerArticleMustMatchCase]
  else co.Options := co.Options - [coIgnoreLowerArticleMustMatchCase];
  co.IgnoreLettersAtStart := edIgnorePrefix.Text;
  if cbIgnoreUpPrefixCase.Checked
  then co.Options := co.Options + [coIgnoredUpperStartMustMatchCase]
  else co.Options := co.Options - [coIgnoredUpperStartMustMatchCase];
  if cbIgnoreLowPrefixCase.Checked
  then co.Options := co.Options + [coIgnoredLowerStartMustMatchCase] else co.Options := co.Options - [coIgnoredLowerStartMustMatchCase];
  co.IgnoreAtStartMinRemainderLen := spinPrefixRemain.Value;


  SynSpellOptions.Save;
end;

class function TSynSpellOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=IDEEditorGroups.GetByIndex(GroupEditor)^.GroupClass;
end;

end.

