{
 Test with:
     ./parsertest --format=plain --suite=TTestParseFPCTestUnits
     ./parsertest --format=plain --suite=TestParse_ugenconstraints
     ./parsertest --format=plain --suite=TestParse_PT_Files
}
unit ParserTBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CodeToolManager, ExprEval, CodeCache, LazFileUtils,
  LazLogger, fpcunit, testregistry;

type

  { TTestParseFPCTestUnits }

  TTestParseFPCTestUnits = class(TTestCase)
  private
    procedure TestParseFile(aFilename: string);
  published
    procedure TestParse_ugenconstraints;
    procedure TestParse_PT_Files;
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

procedure TTestParseFPCTestUnits.TestParseFile(aFilename: string);
var
  Code: TCodeBuffer;
  Tool: TCodeTool;
  Src: String;
  ShouldFail: Boolean;
  FailPos: SizeInt;
begin
  Code:=CodeToolBoss.LoadFile(aFilename,true,false);
  if Code=nil then begin
    AssertEquals('unable to read file "'+aFilename+'"',true,false);
    exit;
  end;
  ShouldFail:=false;
  Src:=Code.Source;
  FailPos:=0;
  if copy(Src,1,6)='{fail:' then begin
    ShouldFail:=true;
    FailPos:=Pos('{fail}',Src);
    if FailPos>0 then FailPos+=6;
  end;

  if CodeToolBoss.Explore(Code,Tool,true) then begin
    if ShouldFail then
      AssertEquals('parser skipped error file "'+aFilename+'"',true,false);
  end else begin
    if ShouldFail then begin
      if FailPos>0 then
        AssertEquals('wrong parser pos in file "'+aFilename+'"',Tool.CleanPosToStr(FailPos),Tool.CodeXYToStr(Tool.ErrorPosition));
    end else begin
      AssertEquals('unable to parse file "'+aFilename+'"',true,false);
    end;
  end;
end;

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

procedure TTestParseFPCTestUnits.TestParse_PT_Files;
var
  Info: TSearchRec;
  Filename: TFilename;
  Dir: String;
begin
  Dir:=CleanAndExpandDirectory(GetCurrentDirUTF8);
  if FindFirstUTF8(Dir+'pt_*.pas',faAnyFile,Info)=0 then begin
    repeat
      Filename:=Dir+Info.Name;
      TestParseFile(Filename);
    until FindNextUTF8(Info)<>0;
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

