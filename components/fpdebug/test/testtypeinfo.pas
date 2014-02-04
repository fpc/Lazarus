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
var
  ImageLoader: TTestLoaderSetup1;
  MemReader: TTestMemReader;
  Ctx: TDbgInfoAddressContext;
  sym: TDbgSymbol;

  obj1: TTestSetup1Class;
  vobj1: TTestSetup1Object;

  Expression: TTestPascalExpression;
begin
  ImageLoader := TTestLoaderSetup1.Create;

  obj1 := TTestSetup1Class.Create;
  ImageLoader.TestStackFrame.Int1 := -299;
  ImageLoader.TestStackFrame.Rec1.FWord := 1021;
  ImageLoader.TestStackFrame.VObj1 := @vobj1;
  ImageLoader.TestStackFrame.Obj1 := obj1;


  MemReader := TTestMemReader.Create;
  MemReader.RegisterValues[5] := TDbgPtr(@ImageLoader.TestStackFrame.EndPoint);

  Ctx := nil;
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

    Expression := TTestPascalExpression.Create('Int1', Ctx);
    AssertTrue('Int1: valid', Expression.Valid);
    AssertTrue('Int1: has ResVal', Expression.ResultValue <> nil);
    AssertEquals('Int1: Value', -299, Expression.ResultValue.AsInteger);
    Expression.Free;

    Expression := TTestPascalExpression.Create('@Int1', Ctx);
    AssertTrue('@Int1: valid', Expression.Valid);
    AssertTrue('@Int1: has ResVal', Expression.ResultValue <> nil);
    AssertEquals('@Int1: Value', PtrInt(@ImageLoader.TestStackFrame.Int1), Expression.ResultValue.AsInteger);
    Expression.Free;

    // TODO, invalid
    Expression := TTestPascalExpression.Create('@Int1^', Ctx);
    //AssertTrue('@Int1^: valid', Expression.Valid);
    //AssertTrue('@Int1^: has ResVal', Expression.ResultValue <> nil);
    ////AssertEquals('@Int1^: Value', -299, Expression.ResultValue.AsInteger);
    Expression.Free;

    Expression := TTestPascalExpression.Create('(@Int1)^', Ctx);
    AssertTrue('(@Int1)^: valid', Expression.Valid);
    AssertTrue('(@Int1)^: has ResVal', Expression.ResultValue <> nil);
    AssertEquals('(@Int1)^: Value', -299, Expression.ResultValue.AsInteger);
    Expression.Free;

    Expression := TTestPascalExpression.Create('Word(Int1)', Ctx);
    AssertTrue('Word(Int1): valid', Expression.Valid);
    AssertTrue('Word(Int1): has ResVal', Expression.ResultValue <> nil);
    AssertEquals('Word(Int1): Value', $FED5, Expression.ResultValue.AsCardinal);
    AssertTrue('Word(Int1): svfCardinal', svfCardinal in  Expression.ResultValue.FieldFlags);
    Expression.Free;

    Expression := TTestPascalExpression.Create('LongInt(Obj1)', Ctx);
    AssertTrue('LongInt(Obj1): valid', Expression.Valid);
    AssertTrue('LongInt(Obj1): has ResVal', Expression.ResultValue <> nil);
    AssertEquals('LongInt(Obj1): Value', PtrUInt(ImageLoader.TestStackFrame.Obj1), Expression.ResultValue.AsCardinal);
    AssertTrue('LongInt(Obj1): svfInteger', svfInteger in  Expression.ResultValue.FieldFlags);
    Expression.Free;

    // Class/Object
    Expression := TTestPascalExpression.Create('Obj1', Ctx);
    AssertTrue('Obj1: valid', Expression.Valid);
    Expression.ResultValue; // just access it
    Expression.Free;

    Obj1.FWord := 1019;
    Expression := TTestPascalExpression.Create('Obj1.FWord', Ctx);
    AssertTrue('Obj1.FWord: valid', Expression.Valid);
    AssertTrue('Obj1.FWord: has ResVal', Expression.ResultValue <> nil);
    AssertEquals('Obj1.FWord: Value', 1019, Expression.ResultValue.AsCardinal);
    Expression.Free;

    Expression := TTestPascalExpression.Create('TTestSetup1Class(Obj1).FWord', Ctx);
    AssertTrue('TTestSetup1Class(Obj1).FWord: valid', Expression.Valid);
    AssertTrue('TTestSetup1Class(Obj1).FWord: has ResVal', Expression.ResultValue <> nil);
    AssertEquals('TTestSetup1Class(Obj1).FWord: Value', 1019, Expression.ResultValue.AsCardinal);
    Expression.Free;

    Expression := TTestPascalExpression.Create('Obj1.FTest', Ctx);
    AssertTrue('Obj1.FTest: valid', Expression.Valid);
    AssertTrue('Obj1.FTest: has ResVal', Expression.ResultValue <> nil);
    Expression.Free;

    Expression := TTestPascalExpression.Create('TTestSetup1Class(Obj1).FTest', Ctx);
    AssertTrue('TTestSetup1Class(Obj1).FTest: valid', Expression.Valid);
    AssertTrue('TTestSetup1Class(Obj1).FTest: has ResVal', Expression.ResultValue <> nil);
    Expression.Free;

    // cast int to object
    Expression := TTestPascalExpression.Create('TTestSetup1Class('+IntToStr(PtrUInt(obj1))+').FWord', Ctx);
    AssertTrue('TTestSetup1Class('+IntToStr(PtrUInt(@obj1))+').FWord: valid', Expression.Valid);
    AssertTrue('TTestSetup1Class('+IntToStr(PtrUInt(@obj1))+').FWord: has ResVal', Expression.ResultValue <> nil);
    AssertEquals('TTestSetup1Class('+IntToStr(PtrUInt(@obj1))+').FWord: Value', 1019, Expression.ResultValue.AsCardinal);
    Expression.Free;

    //TODO 64 bit
    ImageLoader.TestStackFrame.Int1 := PtrInt(obj1);
    Expression := TTestPascalExpression.Create('TTestSetup1Class(Int1).FWord', Ctx);
    AssertTrue('TTestSetup1Class(Int1).FWord: valid', Expression.Valid);
    AssertTrue('TTestSetup1Class(Int1).FWord: has ResVal', Expression.ResultValue <> nil);
    AssertEquals('TTestSetup1Class(Int1).FWord: Value', 1019, Expression.ResultValue.AsCardinal);
    Expression.Free;

    obj1.FTest := obj1;
    Expression := TTestPascalExpression.Create('Obj1.FTest.FWord', Ctx);
    AssertTrue('Obj1.FTest.FWord: valid', Expression.Valid);
    AssertTrue('Obj1.FTest.FWord: has ResVal', Expression.ResultValue <> nil);
    AssertEquals('Obj1.FTest.FWord: Value', 1019, Expression.ResultValue.AsCardinal);
    Expression.Free;

    Expression := TTestPascalExpression.Create('TTestSetup1Class(Obj1).FTest.FWord', Ctx);
    AssertTrue('TTestSetup1Class(Obj1).FTest.FWord: valid', Expression.Valid);
    AssertTrue('TTestSetup1Class(Obj1).FTest.FWord: has ResVal', Expression.ResultValue <> nil);
    AssertEquals('TTestSetup1Class(Obj1).FTest.FWord: Value', 1019, Expression.ResultValue.AsCardinal);
    Expression.Free;

    Expression := TTestPascalExpression.Create('TTestSetup1Class(TTestSetup1Class(Obj1).FTest).FWord', Ctx);
    AssertTrue('TTestSetup1Class(TTestSetup1Class(Obj1).FTest).FWord: valid', Expression.Valid);
    AssertTrue('TTestSetup1Class(TTestSetup1Class(Obj1).FTest).FWord: has ResVal', Expression.ResultValue <> nil);
    AssertEquals('TTestSetup1Class(TTestSetup1Class(Obj1).FTest).FWord: Value', 1019, Expression.ResultValue.AsCardinal);
    Expression.Free;


    Expression := TTestPascalExpression.Create('Obj1.NotExisting', Ctx);
    if Expression.Valid then;
    Expression.ResultValue; // just access it
    Expression.Free;

    Expression := TTestPascalExpression.Create('TObject(Obj1).FWord', Ctx);
    if Expression.Valid then;
    Expression.ResultValue; // just access it
    Expression.Free;

    // @
    Expression := TTestPascalExpression.Create('@Obj1', Ctx);
    AssertTrue('@Obj1: valid', Expression.Valid);
    AssertTrue('@Obj1: has ResVal', Expression.ResultValue <> nil);
    AssertEquals('@Obj1: Value', PtrUint(@ImageLoader.TestStackFrame.Obj1), Expression.ResultValue.AsCardinal);
    Expression.Free;

    Expression := TTestPascalExpression.Create('@Obj1.FWord', Ctx);
    AssertTrue('@Obj1.FWord: valid', Expression.Valid);
    AssertTrue('@Obj1.FWord: has ResVal', Expression.ResultValue <> nil);
    AssertEquals('@Obj1.FWord: Value', PtrUint(@obj1.FWord), Expression.ResultValue.AsCardinal);
    Expression.Free;

    Expression := TTestPascalExpression.Create('@Obj1.FTest', Ctx);
    AssertTrue('@Obj1.FTest: valid', Expression.Valid);
    AssertTrue('@Obj1.FTest: has ResVal', Expression.ResultValue <> nil);
    AssertEquals('@Obj1.FTest: Value', PtrUint(@obj1.FTest), Expression.ResultValue.AsCardinal);
    Expression.Free;

    // TODO: NOT valid (^ operates before @
    Expression := TTestPascalExpression.Create('@Obj1.FWord^', Ctx);
//debugln( Expression.DebugDump);
    //AssertTrue(not '@Obj1.FWord^: valid', Expression.Valid);
    //AssertTrue('@Obj1.FWord^: has ResVal', Expression.ResultValue <> nil);
    //AssertEquals('@Obj1.FWord^: Value', 1019, Expression.ResultValue.AsCardinal);
    Expression.Free;

    Expression := TTestPascalExpression.Create('(@Obj1)^.FWord', Ctx);
    AssertTrue('(@Obj1)^.FWord: valid', Expression.Valid);
    AssertTrue('(@Obj1)^.FWord: has ResVal', Expression.ResultValue <> nil);
    AssertEquals('(@Obj1)^.FWord: Value', 1019, Expression.ResultValue.AsCardinal);
    Expression.Free;

    Expression := TTestPascalExpression.Create('(@Obj1.FWord)^', Ctx);
    AssertTrue('(@Obj1.FWord)^: valid', Expression.Valid);
    AssertTrue('(@Obj1.FWord)^: has ResVal', Expression.ResultValue <> nil);
    AssertEquals('(@Obj1.FWord)^: Value', 1019, Expression.ResultValue.AsCardinal);
    Expression.Free;

    // Record
    Expression := TTestPascalExpression.Create('Rec1', Ctx);
    AssertTrue('Rec1: valid', Expression.Valid);
    Expression.ResultValue; // just access it
    Expression.Free;

    Expression := TTestPascalExpression.Create('Rec1.FWord', Ctx);
    AssertTrue('Rec1.FWord: valid', Expression.Valid);
    AssertTrue('Rec1.FWord: has ResVal', Expression.ResultValue <> nil);
    AssertEquals('Rec1.FWord: Value', 1021, Expression.ResultValue.AsInteger);
    Expression.Free;

    // var param // old style object
    vobj1.FWord := 1122;
    vobj1.FInt := -122;
    Expression := TTestPascalExpression.Create('vobj1', Ctx);
    AssertTrue('vobj1: valid', Expression.Valid);
    Expression.ResultValue; // just access it
    Expression.Free;

    Expression := TTestPascalExpression.Create('vobj1.FWord', Ctx);
    AssertTrue('vobj1.FWord: valid', Expression.Valid);
    AssertTrue('vobj1.FWord: has ResVal', Expression.ResultValue <> nil);
    AssertEquals('vobj1.FWord: Value', 1122, Expression.ResultValue.AsCardinal);
    Expression.Free;

    Expression := TTestPascalExpression.Create('vobj1.FInt', Ctx);
    AssertTrue('vobj1.FInt: valid', Expression.Valid);
    AssertTrue('vobj1.FInt: has ResVal', Expression.ResultValue <> nil);
    AssertEquals('vobj1.FInt: Value', -122, Expression.ResultValue.AsInteger);
    Expression.Free;



    // Not existing
    Expression := TTestPascalExpression.Create('NotExisting1399', Ctx);
    //AssertTrue(Expression.Valid);
    if Expression.Valid then;
    Expression.ResultValue; // just access it
    Expression.Free;

    // Not Existing Typecast
    Expression := TTestPascalExpression.Create('TNotExisting1399(Int1)', Ctx);
    //AssertTrue(Expression.Valid);
    if Expression.Valid then;
    Expression.ResultValue; // just access it
    Expression.Free;



  ///////////////////////////
  finally
    Ctx.ReleaseReference;
    FDwarfInfo.Free;
    ImageLoader.Free;
    MemReader.Free;
    obj1.Free;
  end;
end;



initialization

  RegisterTest(TTestTypInfo);
  DebugLogger.FindOrRegisterLogGroup('FPDBG_DWARF_SEARCH' {$IFDEF FPDBG_DWARF_SEARCH} , True {$ENDIF} )^.Enabled := True;
end.

