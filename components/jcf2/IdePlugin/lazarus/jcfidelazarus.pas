{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit jcfidelazarus; 

interface

uses
  JcfIdeMain, JcfIdeRegister, AsmKeywords, BuildParseTree, BuildTokenList, 
  ParseError, ParseTreeNode, ParseTreeNodeType, PreProcessorExpressionParser, 
  PreProcessorExpressionTokenise, PreProcessorExpressionTokens, 
  PreProcessorParseTree, SourceToken, SourceTokenList, Tokens, TokenUtils, 
  fShowParseTree, AlignAssign, AlignBase, AlignComment, AlignConst, 
  AlignField, AlignTypedef, AlignVars, AllProcesses, BaseVisitor, 
  Capitalisation, IdentifierCaps, SpecificWordCaps, UnitNameCaps, FormatFlags, 
  IndentAsmParam, Indenter, BasicStats, Nesting, FixCase, RebreakLines, 
  ReduceWhiteSpace, RemoveBlankLine, RemoveComment, 
  RemoveConsecutiveWhiteSpace, RemoveReturn, RemoveUnneededWhiteSpace, 
  MozComment, RemoveEmptyComment, BlockStyles, LongLineBreaker, NoReturnAfter, 
  NoReturnBefore, PropertyOnOneLine, RemoveBlankLinesAfterProcHeader, 
  RemoveBlankLinesInVars, RemoveConsecutiveReturns, RemoveReturnsAfter, 
  RemoveReturnsAfterBegin, RemoveReturnsBeforeEnd, ReturnAfter, ReturnBefore, 
  ReturnChars, ReturnsAfterFinalEnd, MaxSpaces, NoSpaceAfter, NoSpaceBefore, 
  RemoveSpaceAtLineEnd, SingleSpaceAfter, SingleSpaceBefore, SpaceBeforeColon, 
  SpaceToTab, TabToSpace, SwitchableVisitor, AddBeginEnd, 
  AddBlockEndSemicolon, FindReplace, SortUses, SortUsesData, 
  UsesClauseFindReplace, UsesClauseInsert, UsesClauseRemove, TreeWalker, 
  VisitSetNesting, VisitSetXY, VisitStripEmptySpace, WarnAssignToFunctionName, 
  WarnCaseNoElse, WarnDestroy, WarnEmptyBlock, Warning, WarnRealType, 
  WarnUnusedParam, Converter, ConvertTypes, EditorConverter, FileConverter, 
  JcfRegistrySettings, JcfSetBase, JcfSettings, SetAlign, SetAsm, SetCaps, 
  SetClarify, SetComments, SetIndent, SetObfuscate, SetPreProcessor, 
  SetReplace, SetReturns, SetSpaces, SettingsTypes, SetTransform, SetUses, 
  SetWordList, SettingsStream, fJcfErrorDisplay, Delay, fileutils, IntList, 
  JcfFontSetFunctions, JcfHelp, JcfLog, JcfMiscFunctions, fAbout, 
  JcfVersionConsts, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('JcfIdeRegister', @JcfIdeRegister.Register); 
end; 

initialization
  RegisterPackage('jcfidelazarus', @Register); 
end.
