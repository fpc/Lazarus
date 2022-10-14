program TestJCF;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX}
  cthreads, {$ENDIF} {$IFDEF HASAMIGA}
  athreads, {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, unit1, JcfSettings, BuildParseTree, PreProcessorExpressionParser,
  PreProcessorExpressionTokenise, PreProcessorExpressionTokens,
  PreProcessorParseTree, fShowParseTree, Converter, JcfUiTools, JcfStringUtils,
  SettingsStream, ReturnAfter, NoReturnBefore, RemoveReturnsBeforeEnd,
  PropertyOnOneLine, NoReturnAfter, LongLineBreaker, BlockStyles,
  RemoveBlankLinesAfterProcHeader, RemoveBlankLinesInVars,
  RemoveConsecutiveReturns, RemoveReturnsAfter, RemoveReturnsAfterBegin,
  ReturnBefore, ReturnChars, ReturnsAfterFinalEnd, SingleSpaceBefore,
  NoSpaceAfter, NoSpaceBefore, MaxSpaces, SingleSpaceAfter, SpaceToTab,
  TabToSpace, MoveSpaceToBeforeColon, SpaceBeforeColon, RemoveSpaceAtLineEnd,
  IndentAsmParam, Indenter, AlignAssign, AlignBase, AlignComment, AlignConst,
  AlignField, AlignTypedef, AlignVars, Capitalisation, IdentifierCaps,
  SpecificWordCaps, UnitNameCaps, AddBeginEnd, AddBlockEndSemicolon,
  FindReplace, SortUses, SortUsesData, UsesClauseFindReplace, UsesClauseInsert,
  UsesClauseRemove, BasicStats, FixCase, RebreakLines, ReduceWhiteSpace,
  RemoveBlankLine, RemoveComment, RemoveConsecutiveWhiteSpace, RemoveReturn,
  RemoveUnneededWhiteSpace, WarnAssignToFunctionName, WarnCaseNoElse,
  WarnDestroy, WarnEmptyBlock, WarnImbalancedComment, Warning, WarnRealType,
  WarnUnusedParam, AllProcesses, BaseVisitor, FormatFlags, Nesting,
  RemoveEmptyComment, SwitchableVisitor, TreeWalker, VisitSetNesting,
  VisitSetXY, VisitStripEmptySpace, SysUtils;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

