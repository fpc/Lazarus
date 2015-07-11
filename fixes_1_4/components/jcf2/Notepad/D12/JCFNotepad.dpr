program JCFNotepad;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is JCFNotepad, released May 2003.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 1999-2008 Anthony Steele.
All Rights Reserved. 
Contributor(s): Anthony Steele. 

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations 
under the License.

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

uses
  Forms,
  frmJcfNotepad in '..\frmJcfNotepad.pas' {fmJCFNotepad},
  Converter in '..\..\ReadWrite\Converter.pas',
  ConvertTypes in '..\..\ReadWrite\ConvertTypes.pas',
  BuildParseTree in '..\..\Parse\BuildParseTree.pas',
  BuildTokenList in '..\..\Parse\BuildTokenList.pas',
  ParseError in '..\..\Parse\ParseError.pas',
  ParseTreeNode in '..\..\Parse\ParseTreeNode.pas',
  ParseTreeNodeType in '..\..\Parse\ParseTreeNodeType.pas',
  SourceToken in '..\..\Parse\SourceToken.pas',
  SourceTokenList in '..\..\Parse\SourceTokenList.pas',
  VisitSetXY in '..\..\Process\VisitSetXY.pas',
  BaseVisitor in '..\..\Process\BaseVisitor.pas',
  JcfMiscFunctions in '..\..\Utils\JcfMiscFunctions.pas',
  JcfLog in '..\..\Utils\JcfLog.pas',
  fShowParseTree in '..\..\Parse\UI\fShowParseTree.pas' {frmShowParseTree},
  SetUses in '..\..\Settings\SetUses.pas',
  JCFSetBase in '..\..\Settings\JCFSetBase.pas',
  JCFSettings in '..\..\Settings\JCFSettings.pas',
  SetAlign in '..\..\Settings\SetAlign.pas',
  SetCaps in '..\..\Settings\SetCaps.pas',
  SetClarify in '..\..\Settings\SetClarify.pas',
  SetFile in '..\..\Settings\SetFile.pas',
  SetIndent in '..\..\Settings\SetIndent.pas',
  SetObfuscate in '..\..\Settings\SetObfuscate.pas',
  SetReplace in '..\..\Settings\SetReplace.pas',
  SetReturns in '..\..\Settings\SetReturns.pas',
  SetSpaces in '..\..\Settings\SetSpaces.pas',
  SettingsStream in '..\..\Settings\Streams\SettingsStream.pas',
  RegistrySettings in '..\..\Settings\Streams\RegistrySettings.pas',
  RemoveUnneededWhiteSpace in '..\..\Process\Obfuscate\RemoveUnneededWhiteSpace.pas',
  FixCase in '..\..\Process\Obfuscate\FixCase.pas',
  RebreakLines in '..\..\Process\Obfuscate\RebreakLines.pas',
  ReduceWhiteSpace in '..\..\Process\Obfuscate\ReduceWhiteSpace.pas',
  RemoveComment in '..\..\Process\Obfuscate\RemoveComment.pas',
  RemoveConsecutiveWhiteSpace in '..\..\Process\Obfuscate\RemoveConsecutiveWhiteSpace.pas',
  RemoveReturn in '..\..\Process\Obfuscate\RemoveReturn.pas',
  WarnRealType in '..\..\Process\Warnings\WarnRealType.pas',
  WarnAssignToFunctionName in '..\..\Process\Warnings\WarnAssignToFunctionName.pas',
  WarnCaseNoElse in '..\..\Process\Warnings\WarnCaseNoElse.pas',
  WarnDestroy in '..\..\Process\Warnings\WarnDestroy.pas',
  WarnEmptyBlock in '..\..\Process\Warnings\WarnEmptyBlock.pas',
  Warning in '..\..\Process\Warnings\Warning.pas',
  JcfVersionConsts in '..\..\JcfVersionConsts.pas',
  JcfRegistrySettings in '..\..\Settings\JcfRegistrySettings.pas',
  TokenUtils in '..\..\Parse\TokenUtils.pas',
  NoSpaceBefore in '..\..\Process\Spacing\NoSpaceBefore.pas',
  NoSpaceAfter in '..\..\Process\Spacing\NoSpaceAfter.pas',
  SingleSpaceAfter in '..\..\Process\Spacing\SingleSpaceAfter.pas',
  SingleSpaceBefore in '..\..\Process\Spacing\SingleSpaceBefore.pas',
  ReturnAfter in '..\..\Process\Returns\ReturnAfter.pas',
  Nesting in '..\..\Process\Nesting.pas',
  VisitSetNesting in '..\..\Process\VisitSetNesting.pas',
  ReturnBefore in '..\..\Process\Returns\ReturnBefore.pas',
  NoReturnAfter in '..\..\Process\Returns\NoReturnAfter.pas',
  NoReturnBefore in '..\..\Process\Returns\NoReturnBefore.pas',
  AllProcesses in '..\..\Process\AllProcesses.pas',
  RemoveBlankLine in '..\..\Process\Obfuscate\RemoveBlankLine.pas',
  BlockStyles in '..\..\Process\Returns\BlockStyles.pas',
  SwitchableVisitor in '..\..\Process\SwitchableVisitor.pas',
  FormatFlags in '..\..\Process\FormatFlags.pas',
  TabToSpace in '..\..\Process\Spacing\TabToSpace.pas',
  SpaceToTab in '..\..\Process\Spacing\SpaceToTab.pas',
  SpecificWordCaps in '..\..\Process\Capitalisation\SpecificWordCaps.pas',
  Capitalisation in '..\..\Process\Capitalisation\Capitalisation.pas',
  Indenter in '..\..\Process\Indent\Indenter.pas',
  PropertyOnOneLine in '..\..\Process\Returns\PropertyOnOneLine.pas',
  SpaceBeforeColon in '..\..\Process\Spacing\SpaceBeforeColon.pas',
  VisitStripEmptySpace in '..\..\Process\VisitStripEmptySpace.pas',
  RemoveBlankLinesAfterProcHeader in '..\..\Process\Returns\RemoveBlankLinesAfterProcHeader.pas',
  RemoveBlankLinesInVars in '..\..\Process\Returns\RemoveBlankLinesInVars.pas',
  ReturnChars in '..\..\Process\Returns\ReturnChars.pas',
  RemoveReturnsBeforeEnd in '..\..\Process\Returns\RemoveReturnsBeforeEnd.pas',
  RemoveReturnsAfterBegin in '..\..\Process\Returns\RemoveReturnsAfterBegin.pas',
  LongLineBreaker in '..\..\Process\Returns\LongLineBreaker.pas',
  IntList in '..\..\Utils\IntList.pas',
  BasicStats in '..\..\Process\Info\BasicStats.pas',
  AlignConst in '..\..\Process\Align\AlignConst.pas',
  AlignBase in '..\..\Process\Align\AlignBase.pas',
  AlignAssign in '..\..\Process\Align\AlignAssign.pas',
  AlignVars in '..\..\Process\Align\AlignVars.pas',
  AlignTypedef in '..\..\Process\Align\AlignTypedef.pas',
  AlignComment in '..\..\Process\Align\AlignComment.pas',
  JCFDropTarget in '..\..\Utils\DragDrop\JCFDropTarget.pas',
  frDrop in '..\..\Utils\DragDrop\frDrop.pas' {FrameDrop: TFrame},
  JCFHelp in '..\..\Utils\JCFHelp.pas',
  fAbout in '..\..\Ui\fAbout.pas' {frmAboutBox},
  frmBaseSettingsFrame in '..\..\Ui\Settings\frmBaseSettingsFrame.pas' {frSettingsFrame: TFrame},
  fAllSettings in '..\..\Ui\fAllSettings.pas' {FormAllSettings},
  frFiles in '..\..\Ui\Settings\frFiles.pas' {fFiles: TFrame},
  frObfuscateSettings in '..\..\Ui\Settings\frObfuscateSettings.pas' {fObfuscateSettings: TFrame},
  frUses in '..\..\Ui\Settings\frUses.pas' {fUses: TFrame},
  frNotIdentifierCapsSettings in '..\..\Ui\Settings\frNotIdentifierCapsSettings.pas' {fNotIdentifierCapsSettings: TFrame},
  frBasicSettings in '..\..\Ui\Settings\frBasicSettings.pas' {frBasic: TFrame},
  frClarify in '..\..\Ui\Settings\frClarify.pas' {fClarify: TFrame},
  frClarifyAlign in '..\..\Ui\Settings\frClarifyAlign.pas' {fClarifyAlign: TFrame},
  frClarifyCaseBlocks in '..\..\Ui\Settings\frClarifyCaseBlocks.pas' {fClarifyCaseBlocks: TFrame},
  frClarifyIndent in '..\..\Ui\Settings\frClarifyIndent.pas' {fClarifyIndent: TFrame},
  frClarifyReturns in '..\..\Ui\Settings\frClarifyReturns.pas' {fClarifyReturns: TFrame},
  frClarifySpaces in '..\..\Ui\Settings\frClarifySpaces.pas' {fClarifySpaces: TFrame},
  frReplace in '..\..\Ui\Settings\frReplace.pas' {fReplace: TFrame},
  frReservedCapsSettings in '..\..\Ui\Settings\frReservedCapsSettings.pas' {frReservedCapsSettings: TFrame},
  frClarifyLongLineBreaker in '..\..\Ui\Settings\frClarifyLongLineBreaker.pas' {fClarifyLongLineBreaker: TFrame},
  fRegistrySettings in '..\..\Ui\fRegistrySettings.pas' {fmRegistrySettings},
  Tokens in '..\..\Parse\Tokens.pas',
  SettingsTypes in '..\..\Settings\SettingsTypes.pas',
  SetWordList in '..\..\Settings\SetWordList.pas',
  UnitNameCaps in '..\..\Process\Capitalisation\UnitNameCaps.pas',
  PreProcessorExpressionTokens in '..\..\Parse\PreProcessor\PreProcessorExpressionTokens.pas',
  PreProcessorExpressionParser in '..\..\Parse\PreProcessor\PreProcessorExpressionParser.pas',
  PreProcessorExpressionTokenise in '..\..\Parse\PreProcessor\PreProcessorExpressionTokenise.pas',
  frPreProcessor in '..\..\Ui\Settings\frPreProcessor.pas' {fPreProcessor: TFrame},
  SetPreProcessor in '..\..\Settings\SetPreProcessor.pas',
  RemoveSpaceAtLineEnd in '..\..\Process\Spacing\RemoveSpaceAtLineEnd.pas',
  frUnitCaps in '..\..\Ui\Settings\frUnitCaps.pas' {frUnitNameCaps: TFrame},
  FindReplace in '..\..\Process\Transform\FindReplace.pas',
  fJcfErrorDisplay in '..\..\Ui\fJcfErrorDisplay.pas' {ExceptionDialog},
  ReturnsAfterFinalEnd in '..\..\Process\Returns\ReturnsAfterFinalEnd.pas',
  UsesClauseInsert in '..\..\Process\Transform\UsesClauseInsert.pas',
  UsesClauseRemove in '..\..\Process\Transform\UsesClauseRemove.pas',
  UsesClauseFindReplace in '..\..\Process\Transform\UsesClauseFindReplace.pas',
  PreProcessorParseTree in '..\..\Parse\PreProcessor\PreProcessorParseTree.pas',
  RemoveEmptyComment in '..\..\Process\RemoveEmptyComment.pas',
  SetComments in '..\..\Settings\SetComments.pas',
  frWarnings in '..\..\Ui\Settings\frWarnings.pas' {fWarnings: TFrame},
  RemoveConsecutiveReturns in '..\..\Process\Returns\RemoveConsecutiveReturns.pas',
  frBlankLines in '..\..\Ui\Settings\frBlankLines.pas' {fBlankLines: TFrame},
  MaxSpaces in '..\..\Process\Spacing\MaxSpaces.pas',
  TreeWalker in '..\..\Process\TreeWalker.pas',
  AddBlockEndSemicolon in '..\..\Process\Transform\AddBlockEndSemicolon.pas',
  AddBeginEnd in '..\..\Process\Transform\AddBeginEnd.pas',
  SetTransform in '..\..\Settings\SetTransform.pas',
  frTransform in '..\..\Ui\Settings\frTransform.pas' {fTransform: TFrame},
  AlignField in '..\..\Process\Align\AlignField.pas',
  frClarifyBlocks in '..\..\Ui\Settings\frClarifyBlocks.pas' {fClarifyBlocks: TFrame},
  SortUses in '..\..\Process\Transform\SortUses.pas',
  SortUsesData in '..\..\Process\Transform\SortUsesData.pas',
  frCompilerDirectReturns in '..\..\Ui\Settings\frCompilerDirectReturns.pas' {fCompilerDirectReturns: TFrame},
  WarnUnusedParam in '..\..\Process\Warnings\WarnUnusedParam.pas',
  IdentifierCaps in '..\..\Process\Capitalisation\IdentifierCaps.pas',
  frIdentifierCapsSettings in '..\..\Ui\Settings\frIdentifierCapsSettings.pas' {fIdentifierCapsSettings: TFrame},
  frAnyCapsSettings in '..\..\Ui\Settings\frAnyCapsSettings.pas',
  frComments in '..\..\Ui\Settings\frComments.pas' {fComments: TFrame},
  JcfFontSetFunctions in '..\..\Utils\JcfFontSetFunctions.pas',
  SetAsm in '..\..\Settings\SetAsm.pas',
  frAsm in '..\..\Ui\Settings\frAsm.pas' {fAsm: TFrame},
  RemoveReturnsAfter in '..\..\Process\Returns\RemoveReturnsAfter.pas',
  IndentAsmParam in '..\..\Process\Indent\IndentAsmParam.pas',
  AsmKeywords in '..\..\Parse\AsmKeywords.pas',
  JcfUnicode in '..\..\Utils\JcfUnicode.pas',
  JcfUnicodeFiles in '..\..\Utils\JcfUnicodeFiles.pas',
  JcfStringUtils in '..\..\Utils\JcfStringUtils.pas',
  MoveSpaceToBeforeColon in '..\..\Process\Spacing\MoveSpaceToBeforeColon.pas',
  JcfSystemUtils in '..\..\Utils\JcfSystemUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'JCF Notepad';
  Application.HelpFile := 'CodeFormat.chm';
  Application.CreateForm(TfmJCFNotepad, fmJCFNotepad);
  Application.Run;
end.
