unit TestTypeInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FpPascalParser, FpDbgDwarf, FpDbgInfo, FpDbgLoader, FpPascalBuilder,
  FpDbgUtil, FileUtil, LazLoggerBase, LazUTF8, fpcunit, testutils, testregistry;

const
  TESTPROG1_FUNC_BAR_LINE = 185;

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
    procedure X;
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
  Ctx: TDbgInfoAddressContext;
begin
  Result := nil;
  if (FDwarfInfo <> nil) and (AnIdent <> '') then begin
    Ctx := FDwarfInfo.FindContext(Location);
    if Ctx <> nil then
      Result := Ctx.FindSymbol(AnIdent);
    Ctx.ReleaseReference;
  end;
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
  LineInfo: PDWarfLineMap;

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

  procedure TestTypeName(AExpName: String; AFlags: TTypeNameFlags = []);
  var
    s: String;
  begin
    AssertEquals(TestText + ' GetTypeName bool ', AExpName <> '', GetTypeName(s, Expr.ResultType, AFlags));
    if AExpName <> '' then
      AssertEquals(TestText + ' GetTypeName result ', LowerCase(AExpName), LowerCase(s));
  end;

  procedure DoTestInvalid(AExprText: String);
  begin
    FreeAndNil(Expr);
    TestText := AExprText;
    Expr := TTestPascalExpression.Create(AExprText);

    AssertFalse(TestText+' has NO dbginfo', Expr.ResultType <> nil);
  end;

  procedure DoRun;
  var
    s: String;
  begin
    Expr := nil;
    LineInfo := FDwarfInfo.GetLineAddressMap('testprog1.pas');
    Location := LineInfo^.GetAddressForLine(TESTPROG1_FUNC_BAR_LINE);

    DoTest('int1', skInteger);
    DoTest('b1', skCardinal);
GetTypeAsDeclaration(s, Expr.ResultType); DebugLn(s);

    DoTest('pint1', skPointer);
    DoTest(Expr.ResultType.TypeInfo, skInteger);

    DoTest('@int1', skPointer);
    DoTest(Expr.ResultType.TypeInfo, skInteger);
GetTypeAsDeclaration(s, Expr.ResultType); DebugLn(s);

    DoTest('pint1^', skInteger);
    DoTest('@int1^', skInteger);

    DoTestInvalid('int1^');
    DoTestInvalid('pint1^^');
    DoTestInvalid('@int1^^');

    DoTest('bool1', skBoolean);

    DoTest('testC.FWord', skCardinal);
    DoTest('testC.FBool', skBoolean);
    DoTest('testC.FTest.FWord', skCardinal);
    DoTest('testC.FTest.FBool', skBoolean);


    DoTest('longint(bool1)', skInteger);
    DoTest('^longint(bool1)', skPointer);
    DoTest('^longint(bool1)^', skInteger);

    DoTestInvalid('^int1');
    DoTestInvalid('^int1(int2)');

    DoTest('ppi', skPointer, 'ppint');
    TestTypeName('^pint', [tnfIncludeOneRef]);

    DoTest('@int1', skPointer, '');
    TestTypeName('^longint', []);
    TestTypeName('^longint', [tnfIncludeOneRef]);
    TestTypeName('', [tnfOnlyDeclared]);

    DoTest('testC', skClass);
GetTypeAsDeclaration(s, Expr.ResultType); DebugLn(s);
    DoTest('testC2', skClass);
GetTypeAsDeclaration(s, Expr.ResultType); DebugLn(s);

    DoTest('enum1', skEnum);
GetTypeAsDeclaration(s, Expr.ResultType); DebugLn(s);

    DoTest('subr', skCardinal);
GetTypeAsDeclaration(s, Expr.ResultType); DebugLn(s);
    DoTest('subr2', skInteger);
GetTypeAsDeclaration(s, Expr.ResultType); DebugLn(s);
    DoTest('subr3', skChar);
GetTypeAsDeclaration(s, Expr.ResultType); DebugLn(s);

    DoTest('set1', skSet);
GetTypeAsDeclaration(s, Expr.ResultType); DebugLn(s);
    DoTest('set2', skSet);
GetTypeAsDeclaration(s, Expr.ResultType); DebugLn(s);
    DoTest('set3', skSet);
GetTypeAsDeclaration(s, Expr.ResultType); DebugLn(s);


    DoTest('a1', skArray);
    AssertTrue(TestText + ' Flag: ', sfStatArray in Expr.ResultType.Flags);
GetTypeAsDeclaration(s, Expr.ResultType); DebugLn(s);

    DoTest('a2', skArray);
    AssertTrue(TestText + ' Flag: ', sfDynArray in Expr.ResultType.Flags);
GetTypeAsDeclaration(s, Expr.ResultType); DebugLn(s);

    DoTest('a1b', skArray);
    AssertTrue(TestText + ' Flag: ', sfStatArray in Expr.ResultType.Flags);
GetTypeAsDeclaration(s, Expr.ResultType); DebugLn(s);

    DoTest('a2b', skArray);
    AssertTrue(TestText + ' Flag: ', sfDynArray in Expr.ResultType.Flags);
GetTypeAsDeclaration(s, Expr.ResultType); DebugLn(s);

    DoTest('a1p', skPointer);
    //AssertTrue(TestText + ' Flag: ', sfStatArray in Expr.ResultType.Flags);
GetTypeAsDeclaration(s, Expr.ResultType); DebugLn(s);

    DoTest('a2p', skPointer);
    //AssertTrue(TestText + ' Flag: ', sfDynArray in Expr.ResultType.Flags);
GetTypeAsDeclaration(s, Expr.ResultType); DebugLn(s);

    DoTest('pdarg', skPointer);
    //AssertTrue(TestText + ' Flag: ', sfDynArray in Expr.ResultType.Flags);
GetTypeAsDeclaration(s, Expr.ResultType); DebugLn(s);

    DoTest('a3', skArray);
    AssertTrue(TestText + ' Flag: ', sfStatArray in Expr.ResultType.Flags);
GetTypeAsDeclaration(s, Expr.ResultType); DebugLn(s);

    DoTest('a4', skArray);
    AssertTrue(TestText + ' Flag: ', sfDynArray in Expr.ResultType.Flags);
GetTypeAsDeclaration(s, Expr.ResultType); DebugLn(s);




    DoTest('testc2.a1', skArray);
    AssertTrue(TestText + ' Flag: ', sfStatArray in Expr.ResultType.Flags);
GetTypeAsDeclaration(s, Expr.ResultType); DebugLn(s);

    DoTest('testc2.a2', skArray);
    AssertTrue(TestText + ' Flag: ', sfDynArray in Expr.ResultType.Flags);
GetTypeAsDeclaration(s, Expr.ResultType); DebugLn(s);

    DoTest('testc2.a3', skArray);
    AssertTrue(TestText + ' Flag: ', sfStatArray in Expr.ResultType.Flags);
GetTypeAsDeclaration(s, Expr.ResultType); DebugLn(s);

    DoTest('testc2.a4', skArray);
    AssertTrue(TestText + ' Flag: ', sfDynArray in Expr.ResultType.Flags);
GetTypeAsDeclaration(s, Expr.ResultType); DebugLn(s);

    DoTest('testc2.a5', skArray);
    AssertTrue(TestText + ' Flag: ', sfStatArray in Expr.ResultType.Flags);
GetTypeAsDeclaration(s, Expr.ResultType); DebugLn(s);
GetTypeAsDeclaration(s, Expr.ResultType, [tdfSkipClassBody, tdfSkipRecordBody]); DebugLn(s);


    DoTest('testc2.a6', skArray);
    AssertTrue(TestText + ' Flag: ', sfDynArray in Expr.ResultType.Flags);
GetTypeAsDeclaration(s, Expr.ResultType); DebugLn(s);

    DoTest('testc2.a7', skArray);
    AssertTrue(TestText + ' Flag: ', sfStatArray in Expr.ResultType.Flags);
GetTypeAsDeclaration(s, Expr.ResultType); DebugLn(s);



    DoTest('a1[1]', skBoolean);
GetTypeAsDeclaration(s, Expr.ResultType); DebugLn(s);

    DoTest('a1[3]', skBoolean);
GetTypeAsDeclaration(s, Expr.ResultType); DebugLn(s);


    DoTest('a1b[1]', skArray);
GetTypeAsDeclaration(s, Expr.ResultType); DebugLn(s);

    DoTest('a1b[3]', skArray);
GetTypeAsDeclaration(s, Expr.ResultType); DebugLn(s);


    DoTest('a1b[1][3]', skBoolean);
GetTypeAsDeclaration(s, Expr.ResultType); DebugLn(s);

    DoTest('a1b[3][4]', skBoolean);
GetTypeAsDeclaration(s, Expr.ResultType); DebugLn(s);


    DoTest('TTestClass(0)', skClass);
GetTypeAsDeclaration(s, Expr.ResultType); DebugLn(s);

    DoTest('TTestClass(0).FWord', skCardinal);
GetTypeAsDeclaration(s, Expr.ResultType); DebugLn(s);


    DoTest('TTestClass2(0).enum4', skPointer);
    DoTest('TTestClass2(0).enum4^', skEnum);

    FreeAndNil(expr);
  end;

begin
  LoadDwarf(AppendPathDelim(GetTestAppDir) + 'testprog1_262.exe');
  AssertTrue('Loaded dwarf', FDwarfInfo <> nil);
  DoRun;
  UnLoadDwarf;

  LoadDwarf(AppendPathDelim(GetTestAppDir) + 'testprog1_271.exe');
  AssertTrue('Loaded dwarf', FDwarfInfo <> nil);
  DoRun;
  UnLoadDwarf;
end;

procedure TTestTypInfo.X;
var
  s1, s2,s3: String;
begin
  s1 := '_vptr$TOBJECT';
  s2 := UTF8UpperCase( '_vptr$TOBJECT');
  s3 := UTF8LowerCase( '_vptr$TOBJECT');
  DebugLn (dbgs(  CompareUtf8BothCase(@s2[1],@s3[1],@s1[1]) ));
end;



initialization

  RegisterTest(TTestTypInfo);
  DebugLogger.FindOrRegisterLogGroup('FPDBG_DWARF_SEARCH' {$IFDEF FPDBG_DWARF_SEARCH} , True {$ENDIF} )^.Enabled := True;
end.

