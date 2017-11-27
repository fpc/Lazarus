{
 Test with:
   ./testcodetools --suite=TTestFindDeclaration
   ./testcodetools --suite=TestFindDeclaration_Basic
   ./testcodetools --suite=TestFindDeclaration_ClassOf
   ./testcodetools --suite=TestFindDeclaration_With
   ./testcodetools --suite=TestFindDeclaration_NestedClasses
   ./testcodetools --suite=TestFindDeclaration_ClassHelper
   ./testcodetools --suite=TestFindDeclaration_TypeHelper
   ./testcodetools --suite=TestFindDeclaration_ObjCClass
   ./testcodetools --suite=TestFindDeclaration_ObjCCategory
   ./testcodetools --suite=TestFindDeclaration_Generics
   ./testcodetools --suite=TestFindDeclaration_FileAtCursor

 FPC tests:
   ./testcodetools --suite=TestFindDeclaration_FPCTests
   ./testcodetools --suite=TestFindDeclaration_FPCTests --filemask=t*.pp
   ./testcodetools --suite=TestFindDeclaration_FPCTests --filemask=tchlp41.pp
 Laz tests:
   ./testcodetools --suite=TestFindDeclaration_LazTests
   ./testcodetools --suite=TestFindDeclaration_LazTests --filemask=t*.pp
   ./testcodetools --suite=TestFindDeclaration_LazTests --filemask=tdefaultproperty1.pp
}
unit TestIdentCompletion;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, FileProcs, LazFileUtils, LazLogger,
  CodeToolManager, ExprEval, CustomCodeTool, FindDeclarationTool,
  KeywordFuncLists, CodeCache, IdentCompletionTool, CodeTree,
  TestFindDeclaration;

type

  { TTestIdentCompletion }

  TTestIdentCompletion = class(TCustomTestFindDeclaration)
  private
    procedure CheckCodeContext(Context: TCodeContextInfoItem; MarkerName: string
      );
  published
    procedure Test_GetValuesOfCaseVariable_Enum;
    procedure Test_FindCodeContext_ProcParams;
    procedure Test_FindCodeContext_ProcParams_NoClosingBracket;
    procedure Test_FindCodeContext_ProcTypeParams;
    procedure Test_FindCodeContext_AttributeParams;
  end;

implementation

{ TTestIdentCompletion }

procedure TTestIdentCompletion.CheckCodeContext(Context: TCodeContextInfoItem; MarkerName: string);
var
  Marker: TFDMarker;
begin
  AssertNotNull('CheckCodeContext: missing context for #'+MarkerName,Context);
  AssertEquals('CheckCodeContext: Context.Expr.Desc for #'+MarkerName,
    ExpressionTypeDescNames[xtContext],ExpressionTypeDescNames[Context.Expr.Desc]);
  if MainTool<>Context.Expr.Context.Tool then
    Fail('CheckCodeContext: Context.Expr.Context.Tool for #'+MarkerName+' expected "'+MainTool.MainFilename+'", but found "'+Context.Expr.Context.Tool.MainFilename+'"');
  Marker:=FindMarker(MarkerName,'#');
  AssertNotNull('CheckCodeContext: missing marker #'+MarkerName,Marker);
  AssertEquals('CheckCodeContext: Context.Expr.Context.Node.StartPos for #'+MarkerName,
    Marker.CleanPos,Context.Expr.Context.Node.StartPos);
end;

procedure TTestIdentCompletion.Test_GetValuesOfCaseVariable_Enum;
var
  List: TStrings;
begin
  List:=TStringList.Create;
  try
    Code.Source:=
    'type TEnum = (red,green);'+LineEnding
    +'var e: TEnum;'+LineEnding
    +'begin'+LineEnding
    +'  case e of'+LineEnding
    +'end.';
    List.Clear;
    if not CodeToolBoss.GetValuesOfCaseVariable(Code,8,4,List) then begin
      Fail('GetValuesOfCaseVariable failed on case enum');
    end;
    //writeln('TTestIdentCompletion.Test_GetValuesOfCaseVariable_Enum ',List.Text);
    AssertEquals('case enum count',2,List.Count);
    AssertEquals('case enum[0]','red',List[0]);
    AssertEquals('case enum[1]','green',List[1]);
  finally
    List.Free;
  end;
end;

procedure TTestIdentCompletion.Test_FindCodeContext_ProcParams;
var
  SrcMark: TFDMarker;
  CursorPos: TCodeXYPosition;
  CodeContexts: TCodeContextInfo;
begin
  StartProgram;
  Add([
  '{#a}procedure DoIt(i, j: longint);',
  'begin',
  'end;',
  '{#b}procedure DoIt(s, h: string);',
  'begin',
  'end;',
  'begin',
  '  DoIt(3,{#c}4);',
  'end.']);
  ParseSimpleMarkers(Code);
  SrcMark:=FindMarker('c','#');
  AssertNotNull('missing src marker #c',SrcMark);
  MainTool.CleanPosToCaret(SrcMark.CleanPos,CursorPos);
  CodeContexts:=nil;
  try
    if not CodeToolBoss.FindCodeContext(Code,CursorPos.X,CursorPos.Y,CodeContexts)
    then begin
      WriteSource(CursorPos);
      Fail('CodeToolBoss.FindCodeContext');
    end;
    AssertEquals('CodeContexts.Count',2,CodeContexts.Count);
    //for i:=0 to CodeContexts.Count-1 do
    //  debugln(['TTestIdentCompletion.Test_FindCodeContext_ProcParams ',i,' ',CodeContexts[i].AsDebugString(true)]);
    CheckCodeContext(CodeContexts[0],'b');
    CheckCodeContext(CodeContexts[1],'a');
  finally
    CodeContexts.Free;
  end;
end;

procedure TTestIdentCompletion.Test_FindCodeContext_ProcParams_NoClosingBracket;
var
  SrcMark: TFDMarker;
  CursorPos: TCodeXYPosition;
  CodeContexts: TCodeContextInfo;
begin
  StartProgram;
  Add([
  '{#a}procedure DoIt(i, j: longint);',
  'begin',
  'end;',
  'begin',
  '  DoIt({#c}',
  'end.']);
  ParseSimpleMarkers(Code);
  SrcMark:=FindMarker('c','#');
  AssertNotNull('missing src marker #c',SrcMark);
  MainTool.CleanPosToCaret(SrcMark.CleanPos,CursorPos);
  CodeContexts:=nil;
  try
    if not CodeToolBoss.FindCodeContext(Code,CursorPos.X,CursorPos.Y,CodeContexts)
    then begin
      WriteSource(CursorPos);
      Fail('CodeToolBoss.FindCodeContext');
    end;
    AssertEquals('CodeContexts.Count',1,CodeContexts.Count);
    //for i:=0 to CodeContexts.Count-1 do
    //  debugln(['TTestIdentCompletion.Test_FindCodeContext_ProcParams ',i,' ',CodeContexts[i].AsDebugString(true)]);
    CheckCodeContext(CodeContexts[0],'a');
  finally
    CodeContexts.Free;
  end;
end;

procedure TTestIdentCompletion.Test_FindCodeContext_ProcTypeParams;
var
  SrcMark: TFDMarker;
  CursorPos: TCodeXYPosition;
  CodeContexts: TCodeContextInfo;
begin
  StartProgram;
  Add([
  'type',
  '  TProc = procedure(i,j: longint);',
  'var {#p}p: TProc;',
  'begin',
  '  p(3,{#c}4);',
  'end.']);
  ParseSimpleMarkers(Code);
  SrcMark:=FindMarker('c','#');
  AssertNotNull('missing src marker #c',SrcMark);
  MainTool.CleanPosToCaret(SrcMark.CleanPos,CursorPos);
  CodeContexts:=nil;
  try
    if not CodeToolBoss.FindCodeContext(Code,CursorPos.X,CursorPos.Y,CodeContexts)
    then begin
      WriteSource(CursorPos);
      Fail('CodeToolBoss.FindCodeContext');
    end;
    AssertEquals('CodeContexts.Count',1,CodeContexts.Count);
    //for i:=0 to CodeContexts.Count-1 do
    //  debugln(['TTestIdentCompletion.Test_FindCodeContext_ProcParams ',i,' ',CodeContexts[i].AsDebugString(true)]);
    CheckCodeContext(CodeContexts[0],'p');
  finally
    CodeContexts.Free;
  end;
end;

procedure TTestIdentCompletion.Test_FindCodeContext_AttributeParams;
var
  SrcMark: TFDMarker;
  CursorPos: TCodeXYPosition;
  CodeContexts: TCodeContextInfo;
begin
  StartProgram;
  Add([
  '{$modeswitch prefixedattributes}',
  'type',
  '  BirdAttribute = class',
  '    {#a}constructor Create; overload;',
  '    {#b}constructor Create(i,j: longint); overload;',
  '  end;',
  '  [Bird({#c})]',
  '  TColor = 1..3;',
  'begin',
  'end.']);
  ParseSimpleMarkers(Code);
  SrcMark:=FindMarker('c','#');
  AssertNotNull('missing src marker #c',SrcMark);
  MainTool.CleanPosToCaret(SrcMark.CleanPos,CursorPos);
  CodeContexts:=nil;
  try
    if not CodeToolBoss.FindCodeContext(Code,CursorPos.X,CursorPos.Y,CodeContexts)
    then begin
      WriteSource(CursorPos);
      Fail('CodeToolBoss.FindCodeContext');
    end;
    //for i:=0 to CodeContexts.Count-1 do
    //  debugln(['TTestIdentCompletion.Test_FindCodeContext_ProcParams ',i,' ',CodeContexts[i].AsDebugString(true)]);
    AssertEquals('CodeContexts.Count',3,CodeContexts.Count);
    CheckCodeContext(CodeContexts[0],'b');
    CheckCodeContext(CodeContexts[1],'a');
    // last entry is the default TObject.Create in unit objpas
  finally
    CodeContexts.Free;
  end;
end;

initialization
  RegisterTests([TTestIdentCompletion]);
end.

