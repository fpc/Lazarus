unit TestTypeInfo;

{$mode objfpc}{$H+}

interface

uses
  FpPascalParser, FpDbgDwarf, FpDbgInfo,
  FpDbgUtil, FpDbgDwarfConst, LazLoggerBase, LazUTF8, sysutils, fpcunit,
  testregistry, TestHelperClasses, TestDwarfSetup1;


type

  { TTestPascalExpression }

  TTestPascalExpression = class(TFpPascalExpression)
  private
    FContext: TDbgInfoAddressContext;
  protected
    function GetDbgSymbolForIdentifier(AnIdent: String): TDbgSymbol; override;
  public
    constructor Create(ATextExpression: String; AContext: TDbgInfoAddressContext);
  end;

  { TTestTypInfo }

  TTestTypInfo = class(TTestCase)
  protected
    FDwarfInfo: TDbgDwarf;
  published
    Procedure New1;
    procedure X;
  end;

implementation

{ TTestPascalExpression }

function TTestPascalExpression.GetDbgSymbolForIdentifier(AnIdent: String): TDbgSymbol;
begin
  Result := nil;
  if (AnIdent <> '') and (FContext <> nil) then
    Result := FContext.FindSymbol(AnIdent);
end;

constructor TTestPascalExpression.Create(ATextExpression: String;
  AContext: TDbgInfoAddressContext);
begin
  FContext := AContext;
  inherited Create(ATextExpression);
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

procedure TTestTypInfo.New1;
type
  TTestFlag = (ttHasType, ttNotHasType, ttHasSymbol, ttHasValSymbol, ttHasTypeSymbol);
  TTestFlags = set of TTestFlag;
var
  CurrentTestName: String;
  Ctx: TDbgInfoAddressContext;
  Expression: TTestPascalExpression;

  procedure InitTest(Expr: String; ExtraName: String = '');
  begin
    if ExtraName <> '' then ExtraName := ' (' + ExtraName + ')';
    CurrentTestName := Expr + ExtraName + ': ';
    Expression.Free;
    Expression := TTestPascalExpression.Create(Expr, Ctx);
  end;
  procedure StartTest(Expr: String; TestFlags: TTestFlags = []; ExtraName: String = '');
  var
    i: TTestFlag;
  begin
    InitTest(Expr, ExtraName);
    AssertTrue(CurrentTestName + 'valid', Expression.Valid);
    AssertTrue(CurrentTestName + 'has ResVal', Expression.ResultValue <> nil);
    for i := low(TTestFlags) to high(TTestFlags) do
      if i in TestFlags then
        case i of
          ttHasType:      AssertTrue('hastype', Expression.ResultValue.TypeInfo <> nil);
          ttNotHasType:   AssertTrue('has not type', Expression.ResultValue.TypeInfo = nil);
          ttHasSymbol:    AssertTrue('hassymbol', Expression.ResultValue.DbgSymbol <> nil);
          ttHasValSymbol: AssertTrue('hassymbol', (Expression.ResultValue.DbgSymbol <> nil) and
                                                  (Expression.ResultValue.DbgSymbol.SymbolType = stValue));
          ttHasTypeSymbol: AssertTrue('hassymbol', (Expression.ResultValue.DbgSymbol <> nil) and
                                                   (Expression.ResultValue.DbgSymbol.SymbolType = stType));
        end;
  end;
  procedure StartTest(Expr: String; ExpKind: TDbgSymbolKind; TestFlags: TTestFlags = []; ExtraName: String = '');
  var
    s: String;
  begin
    StartTest(Expr, TestFlags, ExtraName);
    WriteStr(s, 'Kind exected ', ExpKind, ' but was ', Expression.ResultValue.Kind);
    AssertTrue(s, Expression.ResultValue.Kind = ExpKind);
  end;
  procedure StartInvalTest(Expr: String; ExpError: String; ExtraName: String = '');
  begin
    InitTest(Expr, ExtraName);
    Expression.ResultValue;
    AssertTrue(CurrentTestName + 'invalid', (not Expression.Valid) or (Expression.ResultValue = nil));
    //AssertTrue(CurrentTestName + 'invalid', (not Expression.Valid));
    //ExpError
  end;

  procedure ExpFlags(ExpFlags: TDbgSymbolValueFieldFlags; ExpNotFlags: TDbgSymbolValueFieldFlags = []);
  var
    i: TDbgSymbolValueFieldFlag;
    s: string;
    f: TDbgSymbolValueFieldFlags;
  begin
    AssertTrue(CurrentTestName + 'has ResVal', Expression.ResultValue <> nil);
    f := Expression.ResultValue.FieldFlags;
    For i := low(TDbgSymbolValueFieldFlag) to High(TDbgSymbolValueFieldFlag) do
      if i in ExpFlags then begin
        WriteStr(s, i);
        AssertTrue('Has flag' + s, i in f);
      end;
    For i := low(TDbgSymbolValueFieldFlag) to High(TDbgSymbolValueFieldFlag) do
      if i in ExpNotFlags then begin
        WriteStr(s, i);
        AssertTrue('Has NOT flag + s', not (i in f));
      end;
  end;
  procedure ExpResult(Field: TDbgSymbolValueFieldFlag; ExpValue: QWord);
    procedure AssertEqualsQW(const AMessage: string; Expected, Actual: QWord);
    begin
      AssertTrue(AMessage + ComparisonMsg(IntToStr(Expected), IntToStr(Actual)), Expected = Actual);
    end;
  var
    s: string;
  begin
    ExpFlags([Field]);
    WriteStr(s, Field);
    case Field of
      svfAddress:           AssertEqualsQW('VAlue for '+s, ExpValue, Expression.ResultValue.Address);
      svfSize:              AssertEqualsQW('VAlue for '+s, ExpValue, Expression.ResultValue.Size);
      svfDataAddress:       AssertEqualsQW('VAlue for '+s, ExpValue, Expression.ResultValue.DataAddress);
      svfDataSize:          AssertEqualsQW('VAlue for '+s, ExpValue, Expression.ResultValue.DataSize);
      svfInteger:           AssertEqualsQW('VAlue for '+s, ExpValue, Expression.ResultValue.AsInteger);
      svfCardinal:          AssertEqualsQW('VAlue for '+s, ExpValue, Expression.ResultValue.AsCardinal);
      svfOrdinal:           AssertEqualsQW('VAlue for '+s, ExpValue, Expression.ResultValue.AsCardinal);
      else                  AssertTrue('No test method avail', False);
    end;
  end;
  procedure ExpResult(Field: TDbgSymbolValueFieldFlag; ExpValue: Int64);
  var
    s: string;
  begin
    ExpFlags([Field]);
    WriteStr(s, Field);
    case Field of
      svfAddress:           AssertEquals('VAlue for '+s, ExpValue, Expression.ResultValue.Address);
      svfSize:              AssertEquals('VAlue for '+s, ExpValue, Expression.ResultValue.Size);
      svfDataAddress:       AssertEquals('VAlue for '+s, ExpValue, Expression.ResultValue.DataAddress);
      svfDataSize:          AssertEquals('VAlue for '+s, ExpValue, Expression.ResultValue.DataSize);
      svfInteger:           AssertEquals('VAlue for '+s, ExpValue, Expression.ResultValue.AsInteger);
      svfCardinal:          AssertEquals('VAlue for '+s, ExpValue, Expression.ResultValue.AsCardinal);
      svfOrdinal:           AssertEquals('VAlue for '+s, ExpValue, Expression.ResultValue.AsCardinal);
      else                  AssertTrue('No test method avail', False);
    end;
  end;
  procedure ExpResult(Field: TDbgSymbolValueFieldFlag; ExpValue: Boolean);
  var
    s: string;
  begin
    ExpFlags([Field]);
    WriteStr(s, Field);
    case Field of
      svfBoolean:           AssertEquals('VAlue for '+s, ExpValue, Expression.ResultValue.AsBool);
      else                  AssertTrue('No test method avail', False);
    end;
  end;
  procedure ExpResult(Field: TDbgSymbolValueFieldFlag; ExpValue: String);
  var
    s: string;
  begin
    ExpFlags([Field]);
    WriteStr(s, Field);
    case Field of
      svfString:            AssertEquals('VAlue for '+s, ExpValue, Expression.ResultValue.AsString);
      else                  AssertTrue('No test method avail', False);
    end;
  end;
  procedure ExpResult(Field: TDbgSymbolValueFieldFlag; ExpValue: WideString);
  var
    s: string;
  begin
    ExpFlags([Field]);
    WriteStr(s, Field);
    case Field of
      svfWideString:        AssertEquals('VAlue for '+s, ExpValue, Expression.ResultValue.AsWideString);
      else                  AssertTrue('No test method avail', False);
    end;
  end;

var
  ImageLoader: TTestLoaderSetup1;
  MemReader: TTestMemReader;
  sym: TDbgSymbol;

  obj1: TTestSetup1Class;
  vobj1: TTestSetup1Object;
begin
  ImageLoader := TTestLoaderSetup1.Create;

  obj1 := TTestSetup1Class.Create;
  ImageLoader.TestStackFrame.Int1 := -299;
  ImageLoader.TestStackFrame.Rec1.FWord := 1021;
  ImageLoader.TestStackFrame.VParamTestSetup1Object := @vobj1;
  ImageLoader.TestStackFrame.Obj1 := obj1;


  MemReader := TTestMemReader.Create;
  MemReader.RegisterValues[5] := TDbgPtr(@ImageLoader.TestStackFrame.EndPoint);

  Ctx := nil;
  Expression := nil;
  FDwarfInfo := TDbgDwarf.Create(ImageLoader);
  try
    FDwarfInfo.LoadCompilationUnits;
    FDwarfInfo.MemReader := MemReader;
    //////////////////////////////////////////////////////////

    Ctx := FDwarfInfo.FindContext($00401010);
    AssertTrue('got ctx', Ctx <> nil);

    sym := Ctx.FindSymbol('Int1');
    AssertTrue('got sym',  sym <> nil);
    sym.ReleaseReference();

    // Not existing
    StartInvalTest('NotExisting1399', 'xxx');

    // Not Existing Typecast
    StartInvalTest('TNotExisting1399(Int1)', 'xxx');



    StartTest('Int1', skInteger, [ttHasType]);
    ExpResult(svfInteger, -299);
    ExpFlags([svfInteger, svfOrdinal, svfAddress]); // svfSize;

    StartTest('@Int1');
// TODO: dataAddr
    ExpResult(svfCardinal, PtrUInt(@ImageLoader.TestStackFrame.Int1));
    ExpFlags([svfCardinal, svfOrdinal], [svfAddress]);

    StartInvalTest('@Int1^', 'xxx');

    StartTest('(@Int1)^');
    ExpResult(svfInteger, -299);
    ExpFlags([svfInteger, svfOrdinal, svfAddress]); // svfSize;

    StartTest('Word(Int1)');
    ExpResult(svfCardinal, $FED5);
    ExpFlags([svfCardinal, svfOrdinal, svfAddress], [svfInteger]); // svfSize;

    StartTest('LongInt(Obj1)');
    ExpResult(svfOrdinal, PtrUInt(ImageLoader.TestStackFrame.Obj1));
    ExpResult(svfInteger, PtrInt(ImageLoader.TestStackFrame.Obj1));
    ExpFlags([svfInteger, svfOrdinal, svfAddress], [svfCardinal]); // svfSize;

    // Class/Object
    StartTest('Obj1');
    ExpFlags([svfMembers, svfOrdinal, svfAddress]); // svfSize;

    Obj1.FWord := 1019;
    StartTest('Obj1.FWord');
    ExpResult(svfCardinal, 1019);
    ExpFlags([svfCardinal, svfOrdinal, svfAddress]); // svfSize;

    StartTest('TTestSetup1Class(Obj1).FWord');
    ExpResult(svfCardinal, 1019);
    ExpFlags([svfCardinal, svfOrdinal, svfAddress]); // svfSize;

    StartTest('Obj1.FTest');
    ExpFlags([svfMembers, svfOrdinal, svfAddress]); // svfSize;

    StartTest('TTestSetup1Class(Obj1).FTest');
    ExpFlags([svfMembers, svfOrdinal, svfAddress]); // svfSize;

    // cast int to object
    StartTest('TTestSetup1Class('+IntToStr(PtrUInt(obj1))+').FWord');
    ExpResult(svfCardinal, 1019);
//todo: address
    //ExpFlags([svfCardinal, svfOrdinal, svfAddress]); // svfSize;
    ExpFlags([svfCardinal, svfOrdinal]); // svfSize;

    //TODO 64 bit
    ImageLoader.TestStackFrame.Int1 := PtrInt(obj1);
    StartTest('TTestSetup1Class(Int1).FWord');
    ExpResult(svfCardinal, 1019);
    ExpFlags([svfCardinal, svfOrdinal, svfAddress]); // svfSize;

    obj1.FTest := obj1;
    StartTest('Obj1.FTest.FWord');
    ExpResult(svfCardinal, 1019);
    ExpFlags([svfCardinal, svfOrdinal, svfAddress]); // svfSize;

    StartTest('TTestSetup1Class(Obj1).FTest.FWord');
    ExpResult(svfCardinal, 1019);
    ExpFlags([svfCardinal, svfOrdinal, svfAddress]); // svfSize;

    StartTest('TTestSetup1Class(TTestSetup1Class(Obj1).FTest).FWord');
    ExpResult(svfCardinal, 1019);
    ExpFlags([svfCardinal, svfOrdinal, svfAddress]); // svfSize;

    StartInvalTest('Obj1.NotExisting', 'xxx');

    StartInvalTest('TObject(Obj1).FWord', 'xxx');

    // @
    StartTest('@Obj1');
    ExpResult(svfCardinal, PtrUint(@ImageLoader.TestStackFrame.Obj1));
    ExpFlags([svfCardinal, svfOrdinal], [svfAddress]);

    StartTest('@Obj1.FWord');
    ExpResult(svfCardinal, PtrUint(@ImageLoader.TestStackFrame.Obj1.FWord));
    ExpFlags([svfCardinal, svfOrdinal], [svfAddress]);

    StartTest('@Obj1.FTest');
    ExpResult(svfCardinal, PtrUint(@ImageLoader.TestStackFrame.Obj1.FTest));
    ExpFlags([svfCardinal, svfOrdinal], [svfAddress]);

    // TODO: NOT valid (^ operates before @
    StartInvalTest('@Obj1.FWord^', 'xxx');
////debugln( Expression.DebugDump);

    StartTest('(@Obj1)^.FWord');
    ExpResult(svfCardinal, 1019);
    ExpFlags([svfCardinal, svfOrdinal, svfAddress]); // svfSize; //

    StartTest('(@Obj1.FWord)^');
    ExpResult(svfCardinal, 1019);
    ExpFlags([svfCardinal, svfOrdinal, svfAddress]); // svfSize;

    // Record
    StartTest('Rec1');

    StartTest('Rec1.FWord');
    ExpResult(svfCardinal, 1021);
    ExpFlags([svfCardinal, svfOrdinal, svfAddress]);

    // var param // old style object
    vobj1.FWord := 1122;
    vobj1.FInt := -122;

    StartTest('VParamTestSetup1Object');

    StartTest('VParamTestSetup1Object.FWord');
    ExpResult(svfCardinal, 1122);
    ExpFlags([svfCardinal, svfOrdinal, svfAddress]);

    StartTest('VParamTestSetup1Object.FInt');
    ExpResult(svfInteger, -122);
    ExpFlags([svfInteger, svfOrdinal, svfAddress]);


    // pointer
    ImageLoader.TestStackFrame.Int1 := -299;
    ImageLoader.TestStackFrame.pi := @ImageLoader.TestStackFrame.int1;
    GlobTestSetup1Pointer := @ImageLoader.TestStackFrame.int1;
    GlobTestSetup1QWord := QWord(@ImageLoader.TestStackFrame.int1);

    StartTest('pi', skPointer, [ttHasType]);
    ExpResult(svfOrdinal, QWord(@ImageLoader.TestStackFrame.int1));
    ExpResult(svfAddress, QWord(@ImageLoader.TestStackFrame.pi));
    ExpResult(svfDataAddress, QWord(@ImageLoader.TestStackFrame.int1));
    ExpFlags([svfOrdinal, svfAddress, svfDataAddress, svfSizeOfPointer]);

    StartTest('GlobTestSetup1Pointer', skPointer, [ttHasType]);
    ExpResult(svfOrdinal, QWord(@ImageLoader.TestStackFrame.int1));
    ExpResult(svfAddress, QWord(@GlobTestSetup1Pointer));
    ExpResult(svfDataAddress, QWord(@ImageLoader.TestStackFrame.int1));
    ExpFlags([svfOrdinal, svfAddress, svfDataAddress, svfSizeOfPointer]);

    StartTest('pointer(pi)', skPointer, [ttHasType]);
    ExpResult(svfOrdinal, QWord(@ImageLoader.TestStackFrame.int1));
    ExpResult(svfAddress, QWord(@ImageLoader.TestStackFrame.pi));
    ExpResult(svfDataAddress, QWord(@ImageLoader.TestStackFrame.int1));
    ExpFlags([svfOrdinal, svfAddress, svfDataAddress, svfSizeOfPointer]);

    StartTest('PInt(pi)', skPointer, [ttHasType]);
    ExpResult(svfOrdinal, QWord(@ImageLoader.TestStackFrame.int1));
    ExpResult(svfAddress, QWord(@ImageLoader.TestStackFrame.pi));
    ExpResult(svfDataAddress, QWord(@ImageLoader.TestStackFrame.int1));
    ExpFlags([svfOrdinal, svfAddress, svfDataAddress, svfSizeOfPointer]);

    StartTest('PTestSetup1Class(pi)', skPointer, [ttHasType]);
    ExpResult(svfOrdinal, QWord(@ImageLoader.TestStackFrame.int1));
    ExpResult(svfAddress, QWord(@ImageLoader.TestStackFrame.pi));
    ExpResult(svfDataAddress, QWord(@ImageLoader.TestStackFrame.int1));
    ExpFlags([svfOrdinal, svfAddress, svfDataAddress, svfSizeOfPointer]);

    StartTest('pointer(GlobTestSetup1Pointer)', skPointer, [ttHasType]);
    ExpResult(svfOrdinal, QWord(@ImageLoader.TestStackFrame.int1));
    ExpResult(svfAddress, QWord(@GlobTestSetup1Pointer));
    ExpResult(svfDataAddress, QWord(@ImageLoader.TestStackFrame.int1));
    ExpFlags([svfOrdinal, svfAddress, svfDataAddress, svfSizeOfPointer]);

    StartTest('pint(GlobTestSetup1Pointer)', skPointer, [ttHasType]);
    ExpResult(svfOrdinal, QWord(@ImageLoader.TestStackFrame.int1));
    ExpResult(svfAddress, QWord(@GlobTestSetup1Pointer));
    ExpResult(svfDataAddress, QWord(@ImageLoader.TestStackFrame.int1));
    ExpFlags([svfOrdinal, svfAddress, svfDataAddress, svfSizeOfPointer]);

    StartTest('pi^', skInteger, [ttHasType]);
    ExpResult(svfInteger, -299);
    ExpFlags([svfInteger, svfOrdinal, svfAddress], [svfDataAddress]); // svfSize;

    StartTest('PInt(pi)^', skInteger, [ttHasType]);
    ExpResult(svfInteger, -299);
    ExpFlags([svfInteger, svfOrdinal, svfAddress], [svfDataAddress]); // svfSize;

    StartTest('PInt(GlobTestSetup1Pointer)^', skInteger, [ttHasType]);
    ExpResult(svfInteger, -299);
    ExpFlags([svfInteger, svfOrdinal, svfAddress], [svfDataAddress]); // svfSize;

    StartTest('LongInt(pointer(pi)^)', skInteger, [ttHasType]);
    ExpResult(svfInteger, -299);
    ExpFlags([svfInteger, svfOrdinal, svfAddress], [svfDataAddress]); // svfSize;

    StartTest('LongInt(GlobTestSetup1Pointer^)', skInteger, [ttHasType]);
    ExpResult(svfInteger, -299);
    ExpFlags([svfInteger, svfOrdinal, svfAddress], [svfDataAddress]); // svfSize;


  ///////////////////////////
  finally
    Ctx.ReleaseReference;
    FDwarfInfo.Free;
    ImageLoader.Free;
    MemReader.Free;
    obj1.Free;
    Expression.Free;
  end;
end;



initialization

  RegisterTest(TTestTypInfo);
  DebugLogger.FindOrRegisterLogGroup('FPDBG_DWARF_SEARCH' {$IFDEF FPDBG_DWARF_SEARCH} , True {$ENDIF} )^.Enabled := True;
end.

