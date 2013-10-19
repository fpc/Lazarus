unit TestTypeInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FpPascalParser, FpDbgDwarf, FpDbgClasses, FpDbgLoader, FileUtil,
  LazLoggerBase, fpcunit, testutils, testregistry;

const
  TESTPROG1_FUNC_BAR_LINE = 25;

type

  { TTestPascalExpression }

  TTestPascalExpression = class(TFpPascalExpression)
  private
  protected
    function GetDbgTyeForIdentifier(AnIdent: String): TDbgSymbol; override;
  public
  end;

  { TTestTypInfo }

  TTestTypInfo = class(TTestCase)
  protected
    procedure LoadDwarf(AFileName: String);
    procedure UnLoadDwarf;
    function GetTestAppDir: String;
  published
    procedure Test1;
  end;

implementation
var
  FImageLoader: TDbgImageLoader;
  FDwarfInfo: TDbgDwarf;
  Location: TDBGPtr;

{ TTestPascalExpression }

function TTestPascalExpression.GetDbgTyeForIdentifier(AnIdent: String): TDbgSymbol;
var
  Loc: TDBGPtr;
begin
  Result := nil;
  if (FDwarfInfo <> nil) and (AnIdent <> '') then
    Result := FDwarfInfo.FindIdentifier(Location, AnIdent);
end;

procedure TTestTypInfo.LoadDwarf(AFileName: String);
begin
  UnLoadDwarf;
  if not FileExistsUTF8(AFileName) then exit;
  FImageLoader := TDbgImageLoader.Create(AFileName);
  if not FImageLoader.IsValid then begin
    FreeAndNil(FImageLoader);
    exit;
  end;;
  FDwarfInfo := TDbgDwarf.Create(FImageLoader);
  FDwarfInfo.LoadCompilationUnits;
end;

procedure TTestTypInfo.UnLoadDwarf;
begin
  FreeAndNil(FDwarfInfo);
  FreeAndNil(FImageLoader);
end;

function TTestTypInfo.GetTestAppDir: String;
begin
  Result :=  ProgramDirectory;
  while Result <> '' do begin
    if DirectoryExistsUTF8(AppendPathDelim(Result) + 'testapps') then begin
      Result := AppendPathDelim(Result) + 'testapps';
      exit;
    end;
    while (Result <> '') and (Result[Length(Result)] <> DirectorySeparator) do
      SetLength(Result, Length(Result)-1);
    while (Result <> '') and (Result[Length(Result)] = DirectorySeparator) do
      SetLength(Result, Length(Result)-1);
  end;
end;

procedure TTestTypInfo.Test1;
var
  Expr: TTestPascalExpression;
  TestText: String;

  procedure DoTest(ADbgSym: TDbgSymbol; AKind: TDbgSymbolKind; ATypeName: String = '');
  begin
    AssertTrue(TestText+' not nil', ADbgSym <> nil);
    if ATypeName <> '' then
      AssertEquals(TestText+' type-name', LowerCase(ATypeName), LowerCase(ADbgSym.Name));
    AssertEquals(TestText+' kind', dbgs(AKind), dbgs(ADbgSym.Kind));
  end;

  procedure DoTest(AExprText: String; AKind: TDbgSymbolKind; ATypeName: String = '');
  begin
    FreeAndNil(Expr);
    TestText := AExprText;
    Expr := TTestPascalExpression.Create(AExprText);
DebugLn(Expr.DebugDump);

    AssertTrue(TestText+' is valid', Expr.Valid);
    AssertTrue(TestText+' has dbginfo', Expr.ResultType <> nil);

debugln(['### ', TestText, ' ## ', dbgs(Expr.ResultType.Kind), ' # ',Expr.ResultType.Name]);
    if ATypeName <> '' then
      AssertEquals(TestText+' type-name', LowerCase(ATypeName), LowerCase(Expr.ResultType.Name));
    AssertEquals(TestText+' kind', dbgs(AKind), dbgs(Expr.ResultType.Kind));
  end;

  procedure DoTestInvalid(AExprText: String);
  begin
    FreeAndNil(Expr);
    TestText := AExprText;
    Expr := TTestPascalExpression.Create(AExprText);

    AssertFalse(TestText+' has NO dbginfo', Expr.ResultType <> nil);
  end;

var
  LineInfo: PDWarfLineMap;
begin
  LoadDwarf(AppendPathDelim(GetTestAppDir) + 'testprog1.exe');
  AssertTrue('Loaded dwarf', FDwarfInfo <> nil);

  LineInfo := FDwarfInfo.GetLineAddressMap('testprog1.pas');
  Location := LineInfo^.GetAddressForLine(TESTPROG1_FUNC_BAR_LINE);

  DoTest('int1', skInteger);
  DoTest('b1', skCardinal);

  DoTest('pint1', skPointer);
  DoTest(Expr.ResultType.PointedToType, skInteger);

  DoTest('@int1', skPointer);
  DoTest(Expr.ResultType.PointedToType, skInteger);

  DoTest('pint1^', skInteger);
  DoTest('@int1^', skInteger);

  DoTestInvalid('int1^');
  DoTestInvalid('pint1^^');
  DoTestInvalid('@int1^^');

  DoTest('bool1', skBoolean);

  DoTest('test.FWord', skCardinal);
  DoTest('test.FBool', skBoolean);
  DoTest('test.FTest.FWord', skCardinal);
  DoTest('test.FTest.FBool', skBoolean);


  DoTest('longint(bool1)', skInteger);
  DoTest('^longint(bool1)', skPointer);
  DoTest('^longint(bool1)^', skInteger);

  DoTestInvalid('^int1');
  DoTestInvalid('^int1(int2)');

  FreeAndNil(expr);

  UnLoadDwarf;
end;



initialization

  RegisterTest(TTestTypInfo);
end.

