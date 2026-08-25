{
 Test with:
   ./testcodetools --suite=TTestIdentCompletion
   ./testcodetools --suite=Test_CreateDeclarationPathAt_Basic
}
unit TestIdentCompletion;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // FPCUnit
  TestRegistry,
  // LazUtils
  LazLoggerBase, LazFileUtils,
  // CodeTools
  CodeToolManager, ExprEval, CustomCodeTool, CodeCache, CodeTree, FileProcs,
  FindDeclarationTool, KeywordFuncLists, IdentCompletionTool,
  // (project)
  TestFindDeclaration, TestGlobals;

type

  { TTestIdentCompletion }

  TTestIdentCompletion = class(TCustomTestFindDeclaration)
  private
    procedure CheckCodeContext(Context: TCodeContextInfoItem; MarkerName: string);
    procedure CheckCreateDeclarationPath(const MarkerName, TargetPath,
      ExpectedPath: string; const TargetMarkerName: string = '');
  published
    procedure Test_GetValuesOfCaseVariable_Enum;
    procedure Test_GetValuesOfCaseVariable_Enum_Amp;
    procedure Test_FindCodeContext_ProcParams;
    procedure Test_FindCodeContext_ProcParams_Amp;
    procedure Test_FindCodeContext_ProcParams_NoClosingBracket;
    procedure Test_FindCodeContext_ProcTypeParams;
    procedure Test_FindCodeContext_AttributeParams;
    procedure Test_GatherIdentifiers_ProcParams_String1;
    procedure Test_GatherIdentifiers_ProcParams_String2;
    procedure Test_GatherIdentifiers_ProcParams_String3;
    procedure Test_GatherIdentifiers_DereferencedProperty;

    // CreateDeclarationPathAt
    procedure Test_CreateDeclarationPathAt_Basic;
    procedure Test_CreateDeclarationPathAt_Shadowed;
    procedure Test_CreateDeclarationPathAt_Generics;
    procedure Test_CreateDeclarationPathAt_GenericsObjFPC;
    procedure Test_CreateDeclarationPathAt_UnitPrefix;
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

procedure TTestIdentCompletion.CheckCreateDeclarationPath(const MarkerName,
  TargetPath, ExpectedPath: string; const TargetMarkerName: string);
// Test TIdentCompletionTool.CreateDeclarationPathAt.
// The path is created at the node of the marker {#MarkerName}.
// TargetPath gives the target declaration. It is resolved with
// FindDeclarationPathAt at the node of the marker {#TargetMarkerName}, or at
// {#MarkerName} if TargetMarkerName is empty. It must be found, so use an
// unambiguous path.
// ExpectedPath is the expected shortest path, '' means: expect not found.
// Note: call ParseSimpleMarkers before.
var
  Marker, TargetMarker: TFDMarker;
  StartNode, TargetStartNode: TCodeTreeNode;
  Target: TFindContext;
  FoundPath: String;
begin
  Marker:=FindMarker(MarkerName,'#');
  if Marker=nil then begin
    WriteSource(1,MainTool);
    Fail('Marker {'+MarkDecl+MarkerName+'} missing');
  end;
  StartNode:=MainTool.BuildSubTreeAndFindDeepestNodeAtPos(Marker.CleanPos,true);
  if TargetMarkerName='' then
    TargetStartNode:=StartNode
  else begin
    TargetMarker:=FindMarker(TargetMarkerName,'#');
    if TargetMarker=nil then begin
      WriteSource(1,MainTool);
      Fail('Marker {'+MarkDecl+TargetMarkerName+'} missing');
    end;
    TargetStartNode:=MainTool.BuildSubTreeAndFindDeepestNodeAtPos(
                                              TargetMarker.CleanPos,true);
  end;
  Target:=MainTool.FindDeclarationPathAt(TargetStartNode,TargetPath,[]);
  if Target.Node=nil then begin
    WriteSource(Marker.CleanPos,MainTool);
    Fail('CreateDeclarationPathAt at '+MainTool.CleanPosToStr(Marker.CleanPos,true)
      +' target "'+TargetPath+'" not found');
  end;
  FoundPath:=MainTool.CreateDeclarationPathAt(StartNode,Target.Tool,Target.Node);
  if LowerCase(FoundPath)<>LowerCase(ExpectedPath) then begin
    WriteSource(Marker.CleanPos,MainTool);
    Fail('CreateDeclarationPathAt at '+MainTool.CleanPosToStr(Marker.CleanPos,true)
      +' target "'+TargetPath+'" expected "'+ExpectedPath+'", but found "'+FoundPath+'"');
  end;
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
    //debugln('TTestIdentCompletion.Test_GetValuesOfCaseVariable_Enum ',List.Text);
    AssertEquals('case enum count',2,List.Count);
    AssertEquals('case enum[0]','red',List[0]);
    AssertEquals('case enum[1]','green',List[1]);
  finally
    List.Free;
  end;
end;

procedure TTestIdentCompletion.Test_GetValuesOfCaseVariable_Enum_Amp;
var
  List: TStrings;
begin
  List:=TStringList.Create;
  try
    Code.Source:=
    'type &array = (abc, &array, amp, &aaa);'+LineEnding
    +'var e: &array;'+LineEnding
    +'begin'+LineEnding
    +'  case e of'+LineEnding
    +'end.';
    List.Clear;
    if not CodeToolBoss.GetValuesOfCaseVariable(Code,8,4,List) then begin
      Fail('GetValuesOfCaseVariable failed on case enum');
    end;
    //debugln('TTestIdentCompletion.Test_GetValuesOfCaseVariable_Enum ',List.Text);
    AssertEquals('case enum count',4,List.Count);
    AssertEquals('case enum[0]','abc',List[0]);
    AssertEquals('case enum[1]','&array',List[1]);
    AssertEquals('case enum[2]','amp',List[2]);
    AssertEquals('case enum[3]','&aaa',List[3]);
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

procedure TTestIdentCompletion.Test_FindCodeContext_ProcParams_Amp;
var
  SrcMark: TFDMarker;
  CursorPos: TCodeXYPosition;
  CodeContexts: TCodeContextInfo;
begin
  StartProgram;
  Add([
  'type &array = byte;',
  '{#a}procedure &procedure(&type, j: &array);',
  'begin',
  'end;',
  '{#b}procedure &procedure(s, &h: string);',
  'begin',
  'end;',
  'begin',
  '  &procedure(3,{#c}4);',
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

procedure TTestIdentCompletion.Test_GatherIdentifiers_DereferencedProperty;
var
  SrcMark: TFDMarker;
  CursorPos: TCodeXYPosition;
  CodeContexts: TCodeContextInfo;
begin
  StartProgram;
  Add([
    '{$ModeSwitch AUTODEREF+}',
    'type',
    '  TRec = record',
    '    one: Boolean;',
    '  end;',
    '  PRec = ^TRec;',
    '  TTest = class',
    '    function GetMyRec(const aIndex: Integer): PRec;',
    '    property MyRec[const aIndex: Integer]: PRec read GetMyRec;',
    '  end;',
    'var t: TTest;',
    'begin',
    '  p := t.MyRec[1].{#c};',
    'end.']);
  ParseSimpleMarkers(Code);
  SrcMark:=FindMarker('c','#');
  AssertNotNull('missing src marker #c',SrcMark);
  MainTool.CleanPosToCaret(SrcMark.CleanPos,CursorPos);
  CodeContexts:=nil;
  try
    CodeToolBoss.GatherIdentifiers(Code,CursorPos.X,CursorPos.Y);
    AssertTrue('CodeToolBoss.GatherIdentifiers: '+CodeToolBoss.ErrorMessage,CodeToolBoss.ErrorId=0);
    AssertEquals(CodeToolBoss.IdentifierList.GetFilteredCount, 1);
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

procedure TTestIdentCompletion.Test_GatherIdentifiers_ProcParams_String1;
var
  SrcMark: TFDMarker;
  CursorPos: TCodeXYPosition;
  CodeContexts: TCodeContextInfo;
begin
  StartProgram;
  Add([
  'begin',
  '  writeln({#c}'''');',
  'end.']);
  ParseSimpleMarkers(Code);
  SrcMark:=FindMarker('c','#');
  AssertNotNull('missing src marker #c',SrcMark);
  MainTool.CleanPosToCaret(SrcMark.CleanPos,CursorPos);
  CodeContexts:=nil;
  try
    CodeToolBoss.GatherIdentifiers(Code,CursorPos.X,CursorPos.Y);
    AssertTrue('CodeToolBoss.GatherIdentifiers: '+CodeToolBoss.ErrorMessage,CodeToolBoss.ErrorId=0);
  finally
    CodeContexts.Free;
  end;
end;

procedure TTestIdentCompletion.Test_GatherIdentifiers_ProcParams_String2;
var
  SrcMark: TFDMarker;
  CursorPos: TCodeXYPosition;
  CodeContexts: TCodeContextInfo;
begin
  StartProgram;
  Add([
  'begin',
  '  writeln({#c}'''');',
  'end.']);
  ParseSimpleMarkers(Code);
  SrcMark:=FindMarker('c','#');
  AssertNotNull('missing src marker #c',SrcMark);
  MainTool.CleanPosToCaret(SrcMark.CleanPos+1,CursorPos);
  CodeContexts:=nil;
  try
    CodeToolBoss.GatherIdentifiers(Code,CursorPos.X,CursorPos.Y);
    AssertTrue('CodeToolBoss.GatherIdentifiers: '+CodeToolBoss.ErrorMessage,CodeToolBoss.ErrorId=0);
  finally
    CodeContexts.Free;
  end;
end;

procedure TTestIdentCompletion.Test_GatherIdentifiers_ProcParams_String3;
var
  SrcMark: TFDMarker;
  CursorPos: TCodeXYPosition;
  CodeContexts: TCodeContextInfo;
begin
  StartProgram;
  Add([
  'begin',
  '  writeln(''''{#c});',
  'end.']);
  ParseSimpleMarkers(Code);
  SrcMark:=FindMarker('c','#');
  AssertNotNull('missing src marker #c',SrcMark);
  MainTool.CleanPosToCaret(SrcMark.CleanPos,CursorPos);
  CodeContexts:=nil;
  try
    CodeToolBoss.GatherIdentifiers(Code,CursorPos.X,CursorPos.Y);
    AssertTrue('CodeToolBoss.GatherIdentifiers: '+CodeToolBoss.ErrorMessage,CodeToolBoss.ErrorId=0);
  finally
    CodeContexts.Free;
  end;
end;

procedure TTestIdentCompletion.Test_CreateDeclarationPathAt_Basic;
begin
  StartProgram;
  Add([
  'type',
  '  TColor = (clRed, clGreen);',
  '  TWing = record',
  '    Size: word;',
  '  end;',
  '  TBird = class',
  '  public',
  '    type',
  '      TFeather = record',
  '        Len: word;',
  '      end;',
  '    var',
  '    Wing: TWing;',
  '    procedure Fly;',
  '  end;',
  'procedure TBird.Fly;',
  'begin',
  'end;',
  'var b: TBird;',
  'begin',
  '  b.Fly;{#start}',
  'end.',
  '']);
  ParseSimpleMarkers(Code);

  // a top level declaration is reachable with its identifier
  CheckCreateDeclarationPath('start','TBird','TBird');
  CheckCreateDeclarationPath('start','TWing','TWing');
  CheckCreateDeclarationPath('start','TColor','TColor');
  CheckCreateDeclarationPath('start','b','b');

  // a member needs the type
  CheckCreateDeclarationPath('start','TBird.Fly','TBird.Fly');
  CheckCreateDeclarationPath('start','TBird.Wing','TBird.Wing');
  CheckCreateDeclarationPath('start','TWing.Size','TWing.Size');

  // a nested type needs the parent type
  CheckCreateDeclarationPath('start','TBird.TFeather','TBird.TFeather');
  CheckCreateDeclarationPath('start','TBird.TFeather.Len','TBird.TFeather.Len');

  // an enum value is visible without the enumeration type
  CheckCreateDeclarationPath('start','TColor.clRed','clRed');
end;

procedure TTestIdentCompletion.Test_CreateDeclarationPathAt_Shadowed;
begin
  StartProgram;
  Add([
  'type',
  '  TBird = class',
  '  end;',
  'procedure Run;',
  'var TBird: word;',
  'begin',
  '  {#shadow}',
  'end;',
  'procedure Other;',
  'begin',
  '  {#other}',
  'end;',
  'begin',
  '  {#start}',
  'end.',
  '']);
  ParseSimpleMarkers(Code);

  // the local variable hides the type, so the type needs the unit name
  CheckCreateDeclarationPath('shadow','test1.TBird','test1.TBird');
  // the local variable itself is reachable with its identifier
  CheckCreateDeclarationPath('shadow','TBird','TBird');
  // a local variable of another procedure can not be qualified
  CheckCreateDeclarationPath('other','TBird','','shadow');
  CheckCreateDeclarationPath('start','TBird','','shadow');
  // without the local variable the type needs no unit name
  CheckCreateDeclarationPath('other','TBird','TBird');
end;

procedure TTestIdentCompletion.Test_CreateDeclarationPathAt_Generics;
begin
  StartProgram;
  Add([
  '{$mode delphi}',
  'type',
  '  TFoo = class',
  '    A: word;',
  '  end;',
  '  TFoo<T> = class',
  '    B: word;',
  '  end;',
  '  TFoo<T,U> = class',
  '    C: word;',
  '  end;',
  '  TCat = class',
  '    procedure DoIt; overload;',
  '    procedure DoIt<T>; overload;',
  '    procedure DoIt<T,U>; overload;',
  '  end;',
  'begin',
  '  {#start}',
  'end.',
  '']);
  ParseSimpleMarkers(Code);

  // in mode delphi the number of generic parameters must match, the types are
  // irrelevant, so 'T' is used for every parameter
  CheckCreateDeclarationPath('start','TFoo','TFoo');
  CheckCreateDeclarationPath('start','TFoo<T>','TFoo<T>');
  CheckCreateDeclarationPath('start','TFoo<T,U>','TFoo<T,T>');
  // the members tell the three TFoo apart
  CheckCreateDeclarationPath('start','TFoo.A','TFoo.A');
  CheckCreateDeclarationPath('start','TFoo<T>.B','TFoo<T>.B');
  CheckCreateDeclarationPath('start','TFoo<T,U>.C','TFoo<T,T>.C');

  // generic methods
  CheckCreateDeclarationPath('start','TCat.DoIt','TCat.DoIt');
  CheckCreateDeclarationPath('start','TCat.DoIt<T>','TCat.DoIt<T>');
  CheckCreateDeclarationPath('start','TCat.DoIt<T,U>','TCat.DoIt<T,T>');
end;

procedure TTestIdentCompletion.Test_CreateDeclarationPathAt_GenericsObjFPC;
begin
  StartProgram;
  Add([
  'type',
  '  generic TFoo<T> = class',
  '    B: word;',
  '  end;',
  'begin',
  '  {#start}',
  'end.',
  '']);
  ParseSimpleMarkers(Code);

  // outside mode delphi a part without ''<>'' matches any number of parameters
  CheckCreateDeclarationPath('start','TFoo','TFoo');
  CheckCreateDeclarationPath('start','TFoo.B','TFoo.B');
end;

procedure TTestIdentCompletion.Test_CreateDeclarationPathAt_UnitPrefix;
var
  Unit2, NSUnit: TCodeBuffer;
begin
  Unit2:=CodeToolBoss.CreateFile('unit2.pp');
  NSUnit:=CodeToolBoss.CreateFile('red.green.pp');
  try
    Unit2.Source:=LinesToStr([
      'unit unit2;',
      '{$mode objfpc}{$H+}',
      'interface',
      'type',
      '  TBird = class',
      '    procedure Fly;',
      '  end;',
      '  TFish = class',
      '  end;',
      'implementation',
      'procedure TBird.Fly;',
      'begin',
      'end;',
      'end.']);
    NSUnit.Source:=LinesToStr([
      'unit Red.Green;',
      'interface',
      'var Two: word;',
      'implementation',
      'end.']);

    Add([
    'unit test1;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses unit2, Red.Green;',
    'type',
    '  TBird = class',
    '  end;',
    '  TIntfType = word;{#intf}',
    'implementation',
    'procedure Run;',
    'begin',
    '  {#impl}',
    'end;',
    'end.',
    '']);
    ParseSimpleMarkers(Code);

    // a type of a used unit, that is not hidden, needs no prefix
    CheckCreateDeclarationPath('impl','unit2.TFish','TFish');
    // the own TBird hides the TBird of unit2
    CheckCreateDeclarationPath('impl','unit2.TBird','unit2.TBird');
    CheckCreateDeclarationPath('impl','TBird','TBird');
    // a member of a hidden type
    CheckCreateDeclarationPath('impl','unit2.TBird.Fly','unit2.TBird.Fly');
    // a namespaced unit
    CheckCreateDeclarationPath('impl','Red.Green.Two','Two');
    // the own interface
    CheckCreateDeclarationPath('impl','TIntfType','TIntfType');
    CheckCreateDeclarationPath('intf','TIntfType','TIntfType');
  finally
    Unit2.IsDeleted:=true;
    NSUnit.IsDeleted:=true;
  end;
end;

initialization
  RegisterTests([TTestIdentCompletion]);
end.

