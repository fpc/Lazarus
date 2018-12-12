{
 Test all with:
   ./runtests --format=plain --suite=TTestPascalParser

 Test specific with:
   ./runtests --format=plain --suite=TestRecord_ClassOperators
}
unit TestPascalParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, CodeToolManager, CodeCache, CodeAtom,
  LazLogger, fpcunit, testregistry, TestGlobals;

type

  { TCustomTestPascalParser }

  TCustomTestPascalParser = class(TTestCase)
  private
    FCode: TCodeBuffer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure DoParseModule(aCode: TCodeBuffer; out Tool: TCodeTool);
  public
    procedure Add(const s: string);
    procedure Add(Args: array of const);
    procedure StartUnit;
    procedure StartProgram;
    procedure ParseModule;
    procedure CheckParseError(const CursorPos: TCodeXYPosition; Msg: string);
    procedure WriteSource(CleanPos: integer; Tool: TCodeTool);
    procedure WriteSource(const CursorPos: TCodeXYPosition);
    property Code: TCodeBuffer read FCode;
  end;

  { TTestPascalParser }

  TTestPascalParser = class(TCustomTestPascalParser)
  published
    procedure TestAtomRing;
    procedure TestRecord_ClassOperators;
    procedure TestDeprecated;
    procedure TestMissingGenericKeywordObjFPCFail;
    procedure TestParseGenericsDelphi;
    procedure TestParseExternalConcat;
    procedure TestParseExternalConst;
    procedure TestParseModeTP;
    procedure TestParseProcAnoAssign;
    procedure TestParseProcAnoArg;
  end;

implementation

{ TCustomTestPascalParser }

procedure TCustomTestPascalParser.SetUp;
begin
  inherited SetUp;
  FCode:=CodeToolBoss.CreateFile('test1.pas');
end;

procedure TCustomTestPascalParser.TearDown;
begin
  inherited TearDown;
end;

procedure TCustomTestPascalParser.DoParseModule(aCode: TCodeBuffer; out
  Tool: TCodeTool);
var
  i: Integer;
  Line: String;
begin
  if not CodeToolBoss.Explore(aCode,Tool,true) then begin
    debugln(aCode.Filename+'------------------------------------------');
    for i:=1 to aCode.LineCount do begin
      Line:=aCode.GetLine(i-1,false);
      if i=CodeToolBoss.ErrorLine then
        System.Insert('|',Line,CodeToolBoss.ErrorColumn);
      debugln(Format('%:4d: ',[i]),Line);
    end;
    debugln('Error: '+CodeToolBoss.ErrorDbgMsg);
    Fail('PascalParser failed: '+CodeToolBoss.ErrorMessage);
  end;
end;

procedure TCustomTestPascalParser.Add(const s: string);
begin
  FCode.Source:=FCode.Source+s+LineEnding;
end;

procedure TCustomTestPascalParser.Add(Args: array of const);
begin
  FCode.Source:=FCode.Source+LinesToStr(Args);
end;

procedure TCustomTestPascalParser.StartUnit;
begin
  Add('unit test1;');
  Add('');
  Add('{$mode objfpc}{$H+}');
  Add('');
  Add('interface');
  Add('');
end;

procedure TCustomTestPascalParser.StartProgram;
begin
  Add('program test1;');
  Add('');
  Add('{$mode objfpc}{$H+}');
  Add('');
end;

procedure TCustomTestPascalParser.ParseModule;
var
  Tool: TCodeTool;
begin
  Add('end.');
  DoParseModule(Code,Tool);
end;

procedure TCustomTestPascalParser.CheckParseError(
  const CursorPos: TCodeXYPosition; Msg: string);
var
  Tool: TCodeTool;
begin
  if CodeToolBoss.Explore(Code,Tool,true) then begin
    WriteSource(CursorPos);
    Fail('missing parser error "'+Msg+'"');
  end;
  if Tool=nil then begin
    WriteSource(CursorPos);
    Fail('missing Tool, Msg="'+Msg+'"');
  end;
  if CursorPos.Code<>CodeToolBoss.ErrorCode then begin
    WriteSource(CursorPos);
    Fail('expected parser error "'+Msg+'" in "'+CursorPos.Code.Filename+'", not in "'+CodeToolBoss.ErrorCode.Filename+'"');
  end;
  if (CursorPos.Y<>CodeToolBoss.ErrorLine) or (CursorPos.X<>CodeToolBoss.ErrorColumn) then begin
    WriteSource(CursorPos);
    Fail('expected parser error "'+Msg+'" at line='+IntToStr(CursorPos.Y)+' col='+IntToStr(CursorPos.X)+', but got line='+IntToStr(CodeToolBoss.ErrorLine)+' col='+IntToStr(CodeToolBoss.ErrorColumn));
  end;
  if (Msg<>CodeToolBoss.ErrorMessage) then begin
    WriteSource(CursorPos);
    Fail('expected parser error "'+Msg+'" instead of "'+CodeToolBoss.ErrorMessage+'"');
  end;
end;

procedure TCustomTestPascalParser.WriteSource(CleanPos: integer; Tool: TCodeTool
  );
var
  Caret: TCodeXYPosition;
begin
  if Tool=nil then
    Fail('TCustomTestPascalParser.WriteSource: missing Tool');
  if not Tool.CleanPosToCaret(CleanPos,Caret) then
    Fail('TCustomTestPascalParser.WriteSource: invalid cleanpos '+IntToStr(CleanPos)+' Tool='+Tool.MainFilename);
  WriteSource(Caret);
end;

procedure TCustomTestPascalParser.WriteSource(const CursorPos: TCodeXYPosition);
var
  CurCode: TCodeBuffer;
  i: Integer;
  Line: String;
begin
  CurCode:=CursorPos.Code;
  if CurCode=nil then
    Fail('TCustomTestPascalParser.WriteSource CurCode=nil');
  for i:=1 to CurCode.LineCount do begin
    Line:=CurCode.GetLine(i-1,false);
    if (i=CursorPos.Y) then begin
      write('*');
      Line:=LeftStr(Line,CursorPos.X-1)+'|'+copy(Line,CursorPos.X,length(Line));
    end;
    writeln(Format('%:4d: ',[i]),Line);
  end;
end;

{ TTestPascalParser }

procedure TTestPascalParser.TestAtomRing;

  procedure CheckAtom(Msg: String; const Expected, Actual: TAtomPosition);
  begin
    AssertEquals(Msg+' StartPos',Expected.StartPos,Actual.StartPos);
    AssertEquals(Msg+' EndPos',Expected.EndPos,Actual.EndPos);
    if Expected.Flag<>Actual.Flag then
      Fail(Msg+' Flag Expected='+CommonAtomFlagNames[Expected.Flag]+' but found '+CommonAtomFlagNames[Actual.Flag]);
  end;

  procedure CheckIndexOf(Msg: string; R: TAtomRing);
  var
    i, Actual: Integer;
    P: TAtomPosition;
  begin
    for i:=1-R.PriorCount to R.NextCount do begin
      P:=R.GetAtomAt(i);
      if not R.IndexOf(P.StartPos,Actual) then
        Fail(Msg+' CheckIndexOf i='+IntToStr(i)+' IndexOf failed');
      AssertEquals(Msg+' CheckIndexOf',i,Actual);
    end;
  end;

var
  R: TAtomRing;
  P, P1, P2: TAtomPosition;
  i: Integer;
begin
  R:=TAtomRing.Create;
  try
    R.Size:=4;
    AssertEquals('1-empty count',0,R.PriorCount);
    AssertEquals('1-empty nextcount',0,R.NextCount);

    P1:=AtomPosition(1,2,cafWord);
    R.Add(P1);
    AssertEquals('2-first atom count',1,R.PriorCount);
    AssertEquals('2-first atom nextcount',0,R.NextCount);
    P:=R.GetAtomAt(0);
    CheckAtom('2-first atom',P1,P);

    CheckIndexOf('2-first atom',R);

    R.UndoLastAdd;
    //R.WriteDebugReport;
    AssertEquals('3-empty after undo count',0,R.PriorCount);
    AssertEquals('3-empty after undo nextcount',0,R.NextCount);

    P1:=AtomPosition(1,2,cafWord);
    R.Add(P1);
    //R.WriteDebugReport;
    AssertEquals('4-first atom count',1,R.PriorCount);
    AssertEquals('4-first atom nextcount',0,R.NextCount);
    P:=R.GetAtomAt(0);
    CheckAtom('4-first atom',P1,P);
    CheckIndexOf('4-first atom',R);

    P2:=AtomPosition(3,4,cafWord);
    R.Add(P2);
    //R.WriteDebugReport;

    AssertEquals('5-second atom count',2,R.PriorCount);
    AssertEquals('5-second atom nextcount',0,R.NextCount);
    P:=R.GetAtomAt(0);
    CheckAtom('5-second atom 0',P2,P);
    P:=R.GetAtomAt(-1);
    CheckAtom('5-second atom -1',P1,P);
    CheckIndexOf('5-second atom',R);

    R.UndoLastAdd;
    //R.WriteDebugReport;
    AssertEquals('6-undo after add two: count',1,R.PriorCount);
    AssertEquals('6-undo after add two: nextcount',1,R.NextCount);
    P:=R.GetAtomAt(0);
    CheckAtom('6-undo after add two: atom 0',P1,P);
    P:=R.GetAtomAt(1);
    CheckAtom('6-undo after add two: atom +1',P2,P);
    CheckIndexOf('6-undo after add two',R);

    P2:=AtomPosition(5,6,cafWord);
    R.Add(P2);
    //R.WriteDebugReport;

    AssertEquals('7-second atom count',2,R.PriorCount);
    AssertEquals('7-second atom nextcount',0,R.NextCount);
    P:=R.GetAtomAt(0);
    CheckAtom('7-second atom 0',P2,P);
    P:=R.GetAtomAt(-1);
    CheckAtom('7-second atom -1',P1,P);
    CheckIndexOf('7-second atom',R);

    R.Clear;
    //R.WriteDebugReport;
    for i:=1 to 5 do begin
      // add first
      P1:=AtomPosition(i*4,i*4+1,cafWord);
      R.Add(P1);
      //R.WriteDebugReport;
      AssertEquals('8-Added first: '+IntToStr(i)+' count',Min(i,R.Size),R.PriorCount);
      AssertEquals('8-Added first: '+IntToStr(i)+' nextcount',0,R.NextCount);
      P:=R.GetAtomAt(0);
      CheckAtom('8-Added first: atom 0',P1,P);
      CheckIndexOf('8-Added first',R);

      // add two
      P2:=AtomPosition(i*4+2,i*4+3,cafWord);
      R.Add(P2);
      //R.WriteDebugReport;
      AssertEquals('9-Added second: '+IntToStr(i)+' count',Min(i+1,R.Size),R.PriorCount);
      AssertEquals('9-Added second: '+IntToStr(i)+' nextcount',0,R.NextCount);
      P:=R.GetAtomAt(0);
      CheckAtom('9-Added second: '+IntToStr(i)+' atom 0',P2,P);
      P:=R.GetAtomAt(-1);
      CheckAtom('9-Added second: '+IntToStr(i)+' atom -1',P1,P);
      CheckIndexOf('9-Added second',R);

      // undo one
      R.UndoLastAdd;
      //R.WriteDebugReport;
      AssertEquals('10-Undo: '+IntToStr(i)+' count',Min(i,R.Size-1),R.PriorCount);
      AssertEquals('10-Undo: '+IntToStr(i)+' nextcount',1,R.NextCount);
      P:=R.GetAtomAt(0);
      CheckAtom('10-Undo: '+IntToStr(i)+' atom 0',P1,P);
      P:=R.GetAtomAt(1);
      CheckAtom('10-Undo: '+IntToStr(i)+' atom +1',P2,P);
      CheckIndexOf('10-Undo',R);
    end;

    FreeAndNil(R);
  finally
    if R<>nil then begin
      R.WriteDebugReport;
      R.Free;
    end;
  end;
end;

procedure TTestPascalParser.TestRecord_ClassOperators;
begin
  StartProgram;
  Add([
    'type',
    '  TFlag = (flag1);',
    '{$Define FPC_HAS_MANAGEMENT_OPERATORS}',
    '  TMyRecord = record',
    '    class operator Implicit(t: TMyRecord): TMyRecord;',
    '    class operator Explicit(t: TMyRecord): TMyRecord;',
    '    class operator Negative(t: TMyRecord): TMyRecord;',
    '    class operator Positive(t: TMyRecord): TMyRecord;',
    '    class operator Inc(t: TMyRecord): TMyRecord;',
    '    class operator Dec(t: TMyRecord): TMyRecord;',
    '    class operator LogicalNot(t: TMyRecord): TMyRecord;',
    '    class operator Trunc(t: TMyRecord): TMyRecord;',
    '    class operator Round(t: TMyRecord): TMyRecord;',
    '    class operator In(f: TFlag; t: TMyRecord): boolean;',
    '    class operator Equal(t1, t2: TMyRecord): boolean;',
    '    class operator NotEqual(t1, t2: TMyRecord): boolean;',
    '    class operator GreaterThan(t1, t2: TMyRecord): boolean;',
    '    class operator GreaterThanOrEqual(t1, t2: TMyRecord): boolean;',
    '    class operator LessThan(t1, t2: TMyRecord): boolean;',
    '    class operator LessThanOrEqual(t1, t2: TMyRecord): boolean;',
    '    class operator Add(t1, t2: TMyRecord): TMyRecord;',
    '    class operator Subtract(t1, t2: TMyRecord): TMyRecord;',
    '    class operator Multiply(t1, t2: TMyRecord): TMyRecord;',
    '    class operator Divide(t1, t2: TMyRecord): TMyRecord;',
    '    class operator IntDivide(t1, t2: TMyRecord): TMyRecord;',
    '    class operator Modulus(t1, t2: TMyRecord): TMyRecord;',
    '    class operator LeftShift(t1, t2: TMyRecord): TMyRecord;',
    '    class operator RightShift(t1, t2: TMyRecord): TMyRecord;',
    '    class operator LogicalAnd(b: boolean; t: TMyRecord): TMyRecord;',
    '    class operator LogicalOr(b: boolean; t: TMyRecord): TMyRecord;',
    '    class operator LogicalXor(b: boolean; t: TMyRecord): TMyRecord;',
    '    class operator BitwiseAnd(t1, t2: TMyRecord): TMyRecord;',
    '    class operator BitwiseOr(t1, t2: TMyRecord): TMyRecord;',
    '    class operator BitwiseXor(t1, t2: TMyRecord): TMyRecord;',
    '    // only IFDEF FPC_HAS_MANAGEMENT_OPERATORS',
    '    class operator Initialize(var t: TMyRecord);',
    '    class operator Finalize(var t: TMyRecord);',
    '    class operator Copy(var t: TMyRecord);',
    '    class operator AddRef(constref t1: TMyRecord ; var t2: TMyRecord);',
    '  end;',
    '',
    'class operator TMyRecord.Implicit(t: TMyRecord): TMyRecord;',
    'begin end;',
    '',
    '// only IFDEF FPC_HAS_MANAGEMENT_OPERATORS',
    'class operator TMyRecord.Initialize(var t: TMyRecord);',
    'begin end;',
    '',
    'begin'
    ]);
  ParseModule;
end;

procedure TTestPascalParser.TestDeprecated;
begin
  StartProgram;
  Add([
  'type',
  '  t = string deprecated ''t'';',
  '  TBird = class',
  '    FA: longint deprecated;',
  '    Deprecated: longint;',
  '    procedure SetA; deprecated;',
  '    property A: longint read FA; deprecated;',
  '    Platform: longint;',
  '  end deprecated ''tbird'';',
  'var',
  '  c: char deprecated;',
  '  b: boolean deprecated ''b'';',
  '  deprecated: boolean;',
  'procedure DoIt; deprecated;',
  'begin end;',
  'begin']);
  ParseModule;
end;

procedure TTestPascalParser.TestMissingGenericKeywordObjFPCFail;
begin
  Add([
  'program test1;',
  '{$mode objfpc}',
  'type',
  '  TList<T> = class end;',
  'begin']);
  CheckParseError(CodeXYPosition(8,4,Code),'expected =, but < found');
end;

procedure TTestPascalParser.TestParseGenericsDelphi;
begin
  Add([
  'program test1;',
  '{$mode delphi}',
  'type',
  '  TRec = record',
  '    procedure Proc<T>;', // generic proc inside normal record
  '  end;',
  '  TBird<B> = class(TAnimal<B>)',
  '    procedure DoIt;', // normal proc inside generic class
  '    procedure DoSome<T>;', // generic proc inside generic class
  '    generic class procedure DoGen<P>(i: P);',
  '  end;',
  'procedure TRec.Proc<T>;', // generic proc inside normal record
  'begin',
  'end;',
  'procedure TBird<B>.DoIt;', // normal proc inside generic class
  'begin',
  'end;',
  'procedure TBird<B>.DoSome<T>;', // generic proc inside generic class
  'begin',
  'end;',
  'generic class procedure TBird<B>.DoGen<P>(i: P);',
  'begin',
  'end;',
  'begin']);
  ParseModule;
end;

procedure TTestPascalParser.TestParseExternalConcat;
begin
  Add([
  'program test1;',
  '{$mode objfpc}',
  'procedure foo; cdecl; external name concat(''foo'', ''bar'');',
  'begin']);
  ParseModule;
end;

procedure TTestPascalParser.TestParseExternalConst;
begin
  Add([
  'program test1;',
  'const NaN: double; external;',
  'const nan: double; external name ''NaN'';',
  '{$modeswitch externalclass}',
  'type',
  '  TExtA = class external name ''ExtA''',
  '    const id;',
  '  end;',
  'begin']);
  ParseModule;
end;

procedure TTestPascalParser.TestParseModeTP;
begin
  Add([
  'program test1;',
  '{$mode tp}',
  '{ {}',
  'begin']);
  ParseModule;
end;

procedure TTestPascalParser.TestParseProcAnoAssign;
begin
  Add([
  'program test1;',
  '{$modeswitch closures}',
  'procedure DoIt;',
  'begin',
  '  p:=procedure begin end;',
  '  p:=procedure(w: word) begin end;',
  '  p:=procedure assembler asm end;',
  '  p:=procedure var w:word; begin end;',
  '  p:=procedure const c=3; begin end;',
  '  p:=procedure type p=procedure; begin end;',
  '  p:=procedure begin p:=function:word begin end end;',
  '  p:=procedure begin p:=procedure(w:word) begin end; end;',
  'end;',
  'begin',
  '  p:=procedure begin end;',
  '  p:=procedure begin p:=procedure(w:word) begin end; end;',
  '']);
  ParseModule;
end;

procedure TTestPascalParser.TestParseProcAnoArg;
begin
  Add([
  'program test1;',
  '{$mode objfpc}',
  '{$modeswitch closures}',
  'procedure DoIt;',
  'begin',
  '  DoIt(procedure begin end);',
  '  DoIt(procedure(var v: word; const c: word; out o: word) begin end);',
  '  DoIt(procedure assembler asm end);',
  '  DoIt(procedure var w:word; begin end);',
  '  DoIt(procedure const c=3; begin end);',
  '  DoIt(procedure type p=procedure; begin end);',
  '  DoIt(procedure begin p:=function:word begin end end);',
  '  DoIt(procedure begin p:=procedure(w:word) begin end; end);',
  'end;',
  'begin',
  '  DoIt(procedure begin end);',
  '  DoIt(procedure begin p:=procedure(w:word) begin end; end);',
  '']);
  ParseModule;
end;

initialization
  RegisterTest(TTestPascalParser);

end.

