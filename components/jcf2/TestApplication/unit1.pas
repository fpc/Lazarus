{   Quick and dirty app for testing and debugging porpouses.

}

unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn,
  Menus, SynEdit, SynHighlighterPas, ConvertTypes, SynEditTypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    btnClearAndPaste: TButton;
    cbShowTree: TCheckBox;
    edFileName: TFileNameEdit;
    lbPos: TLabel;
    Memo3: TMemo;
    Memo1: TSynEdit;
    Memo2: TSynEdit;
    miCut: TMenuItem;
    miCopy: TMenuItem;
    miPaste: TMenuItem;
    miSelectAll: TMenuItem;
    PopupMenu1: TPopupMenu;
    SynPasSyn1: TSynPasSyn;
    procedure btnClearAndPasteClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure edFileNameAcceptFileName(Sender: TObject; var Value: string);
    procedure Memo1StatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure miCopyClick(Sender: TObject);
    procedure miCutClick(Sender: TObject);
    procedure miPasteClick(Sender: TObject);
    procedure miSelectAllClick(Sender: TObject);
  private

  public
    procedure LogIDEMessage(const psFile, psMessage: string; const peMessageType: TStatusMessageType; const piY, piX: integer);
  end;


var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

uses
  JcfStringUtils in '..\..\Utils\JcfStringUtils.pas',
  //JcfFileUtils in '..\..\Utils\JcfFileUtils.pas',
  JcfSystemUtils in '..\..\Utils\JcfSystemUtils.pas',
  Converter in '..\..\ReadWrite\Converter.pas',
  FileConverter in '..\..\ReadWrite\FileConverter.pas',
  //  ConvertTypes in '..\..\ReadWrite\ConvertTypes.pas',
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
  //FileUtils in '..\..\Utils\FileUtils.pas',
  JcfLog in '..\..\Utils\JcfLog.pas',
  //fShowParseTree in '..\..\Parse\UI\fShowParseTree.pas' {frmShowParseTree},
  SetUses in '..\..\Settings\SetUses.pas',
  JcfSetBase in '..\..\Settings\JcfSetBase.pas',
  JcfSettings,
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
  Tokens in '..\..\Parse\Tokens.pas',
  SetWordList in '..\..\Settings\SetWordList.pas',
  PreProcessorExpressionTokens in '..\..\Parse\PreProcessor\PreProcessorExpressionTokens.pas',
  PreProcessorExpressionParser in '..\..\Parse\PreProcessor\PreProcessorExpressionParser.pas',
  PreProcessorExpressionTokenise in '..\..\Parse\PreProcessor\PreProcessorExpressionTokenise.pas',
  JcfHelp in '..\..\Utils\JcfHelp.pas',
  SettingsTypes in '..\..\Settings\SettingsTypes.pas',
  SetPreProcessor in '..\..\Settings\SetPreProcessor.pas',
  UnitNameCaps in '..\..\Process\Capitalisation\UnitNameCaps.pas',
  RemoveSpaceAtLineEnd in '..\..\Process\Spacing\RemoveSpaceAtLineEnd.pas',
  FindReplace in '..\..\Process\Transform\FindReplace.pas',
  //fJcfErrorDisplay in '..\..\Ui\fJcfErrorDisplay.pas' {ExceptionDialog},
  ReturnsAfterFinalEnd in '..\..\Process\Returns\ReturnsAfterFinalEnd.pas',
  PreProcessorParseTree in '..\..\Parse\PreProcessor\PreProcessorParseTree.pas',
  RemoveEmptyComment in '..\..\Process\RemoveEmptyComment.pas',
  RemoveConsecutiveReturns in '..\..\Process\Returns\RemoveConsecutiveReturns.pas',
  UsesClauseFindReplace in '..\..\Process\Transform\UsesClauseFindReplace.pas',
  UsesClauseInsert in '..\..\Process\Transform\UsesClauseInsert.pas',
  UsesClauseRemove in '..\..\Process\Transform\UsesClauseRemove.pas',
  MaxSpaces in '..\..\Process\Spacing\MaxSpaces.pas',
  SetComments in '..\..\Settings\SetComments.pas',
  TreeWalker in '..\..\Process\TreeWalker.pas',
  AddBlockEndSemicolon in '..\..\Process\Transform\AddBlockEndSemicolon.pas',
  AddBeginEnd in '..\..\Process\Transform\AddBeginEnd.pas',
  SetTransform in '..\..\Settings\SetTransform.pas',
  AlignField in '..\..\Process\Align\AlignField.pas',
  SortUses in '..\..\Process\Transform\SortUses.pas',
  SortUsesData in '..\..\Process\Transform\SortUsesData.pas',
  IdentifierCaps in '..\..\Process\Capitalisation\IdentifierCaps.pas',
  WarnUnusedParam in '..\..\Process\Warnings\WarnUnusedParam.pas',
  //JcfFontSetFunctions in '..\..\Utils\JcfFontSetFunctions.pas',
  SetAsm in '..\..\Settings\SetAsm.pas',
  RemoveReturnsAfter in '..\..\Process\Returns\RemoveReturnsAfter.pas',
  IndentAsmParam in '..\..\Process\Indent\IndentAsmParam.pas',
  AsmKeywords in '..\..\Parse\AsmKeywords.pas';

procedure TForm1.Button1Click(Sender: TObject);
var
  fcConverter: TConverter;
begin
  Memo3.Lines.Clear;
  fcConverter := TConverter.Create;
  try
    fcConverter.OnStatusMessage := @LogIDEMessage;
    fcConverter.InputCode := Memo1.Text;
    fcConverter.GuiMessages := True;
    fcConverter.ShowTree:=cbShowTree.Checked;
    fcConverter.Convert;
    if not fcConverter.ConvertError then
    begin
      Memo2.Text := fcConverter.OutputCode;
    end;
  finally
    fcConverter.Free;
  end;
end;

procedure TForm1.btnClearAndPasteClick(Sender: TObject);
begin
  Memo1.Lines.Clear;
  Memo1.PasteFromClipboard;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if Trim(edFileName.Text)<>'' then
    Memo1.Lines.LoadFromFile(edFileName.Text);
end;

procedure TForm1.edFileNameAcceptFileName(Sender: TObject; var Value: string);
begin
  Memo1.Lines.LoadFromFile(Value);
end;

procedure TForm1.Memo1StatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
  lbPos.Caption := 'Line: ' + IntToStr(Memo1.CaretY) + ' Col: ' + IntToStr(Memo1.CaretX);
end;

procedure TForm1.miCopyClick(Sender: TObject);
begin
  Memo1.CopyToClipboard;
end;

procedure TForm1.miCutClick(Sender: TObject);
begin
  Memo1.CutToClipboard;
end;

procedure TForm1.miPasteClick(Sender: TObject);
begin
  Memo1.PasteFromClipboard;
end;

procedure TForm1.miSelectAllClick(Sender: TObject);
begin
  Memo1.SelectAll;
end;

procedure TForm1.LogIDEMessage(const psFile, psMessage: string; const peMessageType: TStatusMessageType; const piY, piX: integer);
//var
//  Urgency: TMessageLineUrgency;
begin
  { no empty lines in this log }
  if psMessage = '' then
    exit;
  //case peMessageType of
  //mtException,mtInputError,mtParseError: Urgency:=mluError;
  //mtCodeWarning: Urgency:=mluWarning;
  //mtFinalSummary: Urgency:=mluImportant;
  //mtProgress: Urgency:=mluProgress;
  //end;
  Memo3.Lines.Add(format('%d,%d : %s ', [piY, piX, psMessage]));
  //lazMessages.AddCustomMessage(Urgency,psMessage, psFile, piY, piX, 'JCF')
end;

end.


