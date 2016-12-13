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
  Classes, SysUtils, CodeToolManager, CodeCache,
  LazLogger, fpcunit, testregistry, TestGlobals;

type

  { TTestPascalParser }

  TTestPascalParser = class(TTestCase)
  private
    FCode: TCodeBuffer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
    procedure Add(const s: string);
    procedure Add(Args: array of const);
    procedure StartUnit;
    procedure StartProgram;
    procedure ParseModule;
    property Code: TCodeBuffer read FCode;
  published
    procedure TestRecord_ClassOperators;
  end;

implementation

{ TTestPascalParser }

procedure TTestPascalParser.SetUp;
begin
  inherited SetUp;
  FCode:=CodeToolBoss.CreateFile('test1.pas');
end;

procedure TTestPascalParser.TearDown;
begin
  inherited TearDown;
end;

procedure TTestPascalParser.Add(const s: string);
begin
  FCode.Source:=FCode.Source+s+LineEnding;
end;

procedure TTestPascalParser.Add(Args: array of const);
begin
  FCode.Source:=FCode.Source+LinesToStr(Args);
end;

procedure TTestPascalParser.StartUnit;
begin
  Add('unit test1;');
  Add('');
  Add('{$mode objfpc}{$H+}');
  Add('');
  Add('interface');
  Add('');
end;

procedure TTestPascalParser.StartProgram;
begin
  Add('program test1;');
  Add('');
  Add('{$mode objfpc}{$H+}');
  Add('');
end;

procedure TTestPascalParser.ParseModule;
var
  Tool: TCodeTool;
  Cnt, i: Integer;
begin
  Add('end.');
  if not CodeToolBoss.Explore(Code,Tool,true) then begin
    debugln(Code.Filename+'------------------------------------------');
    if CodeToolBoss.ErrorLine>0 then
      Cnt:=CodeToolBoss.ErrorLine
    else
      Cnt:=Code.LineCount;
    for i:=1 to Cnt do
      debugln(Format('%:4d: ',[i]),Code.GetLine(i-1,false));
    debugln('Error: '+CodeToolBoss.ErrorDbgMsg);
    Fail('PascalParser failed: '+CodeToolBoss.ErrorMessage);
  end;
end;

procedure TTestPascalParser.TestRecord_ClassOperators;
begin
  StartProgram;
  Add([
    'type',
    '  TMyRecord = record',
    '    class operator Implicit(t: TMyRecord): TMyRecord;',
    '  end;',
    '',
    'class operator TMyRecord.Implicit(t: TMyRecord): TMyRecord;',
    'begin end;',
    '',
    'begin'
    ]);
  ParseModule;
end;

initialization
  AddToPascalTestSuite(TTestPascalParser);

end.

