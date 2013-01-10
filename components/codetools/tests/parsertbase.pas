{
 Test with:
     ./parsertest --format=plain --suite=TTestParseFPCTestUnits
     ./parsertest --format=plain --suite=TestParse_ugenconstraints
}
unit parsertbase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CodeToolManager, ExprEval, CodeCache, LazFileUtils,
  LazLogger, fpcunit, testregistry;

type

  { TTestParseFPCTestUnits }

  TTestParseFPCTestUnits = class(TTestCase)
  private
  published
    procedure TestParse_ugenconstraints;
  end;

var
  BugsTestSuite: TTestSuite;
  ParserTestSuite: TTestSuite;

procedure AddToBugsTestSuite(ATest: TTest);
procedure AddToParserTestSuite(ATestClass: TClass);

implementation

procedure AddToBugsTestSuite(ATest: TTest);
begin
  BugsTestSuite.AddTest(ATest);
end;

procedure AddToParserTestSuite(ATestClass: TClass);
begin
  ParserTestSuite.AddTestSuiteFromClass(ATestClass);
end;

{ TTestParseFPCTestUnits }

procedure TTestParseFPCTestUnits.TestParse_ugenconstraints;
var
  FPCDir: String;
  Filename: String;
  Code: TCodeBuffer;
  Tool: TCodeTool;
begin
  FPCDir:=TrimFilename(CodeToolBoss.GlobalValues.Variables[ExternalMacroStart+'FPCSrcDir']);
  if not DirPathExists(FPCDir) then
    raise Exception.Create('FPCDIR not found: '+FPCDir);
  Filename:=AppendPathDelim(FPCDir)+'tests/test/ugenconstraints.pas';
  //debugln(['TTestParseFPCTestUnits.TestParse_ugenconstraints ',Filename]);
  Code:=CodeToolBoss.LoadFile(Filename,true,false);
  if Code=nil then
    raise Exception.Create('unable to load '+Filename);
  if not CodeToolBoss.Explore(Code,Tool,true) then begin
    debugln(['TTestParseFPCTestUnits.TestParse_ugenconstraints ',CodeToolBoss.ErrorMessage]);
  end;
end;

initialization
  GetTestRegistry.TestName := 'All tests';
  BugsTestSuite := TTestSuite.Create('Bugs');
  GetTestRegistry.AddTest(BugsTestSuite);
  ParserTestSuite := TTestSuite.Create('Parser');
  GetTestRegistry.AddTest(ParserTestSuite);

  AddToParserTestSuite(TTestParseFPCTestUnits);
end.

