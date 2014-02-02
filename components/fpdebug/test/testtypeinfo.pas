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
    AssertTrue(Expression.Valid);
    AssertTrue(Expression.ResultValue <> nil);
    AssertEquals(-299, Expression.ResultValue.AsInteger);
    Expression.Free;

    Expression := TTestPascalExpression.Create('@Int1', Ctx);
    AssertTrue(Expression.Valid);
    AssertTrue(Expression.ResultValue <> nil);
    AssertEquals(PtrInt(@ImageLoader.TestStackFrame.Int1), Expression.ResultValue.AsInteger);
    Expression.Free;

    Expression := TTestPascalExpression.Create('@Int1^', Ctx);
    AssertTrue(Expression.Valid);
    AssertTrue(Expression.ResultValue <> nil);
    AssertEquals(-299, Expression.ResultValue.AsInteger);
    Expression.Free;

    Expression := TTestPascalExpression.Create('(@Int1)^', Ctx);
    AssertTrue(Expression.Valid);
    AssertTrue(Expression.ResultValue <> nil);
    AssertEquals(-299, Expression.ResultValue.AsInteger);
    Expression.Free;

    // Class/Object
    Expression := TTestPascalExpression.Create('Obj1', Ctx);
    AssertTrue(Expression.Valid);
    Expression.ResultValue; // just access it
    Expression.Free;

    Obj1.FWord := 1019;
    Expression := TTestPascalExpression.Create('Obj1.FWord', Ctx);
    AssertTrue(Expression.Valid);
    AssertTrue(Expression.ResultValue <> nil);
    AssertEquals(1019, Expression.ResultValue.AsCardinal);
    Expression.Free;

    Expression := TTestPascalExpression.Create('TTestSetup1Class(Obj1).FWord', Ctx);
    AssertTrue(Expression.Valid);
    AssertTrue(Expression.ResultValue <> nil);
    AssertEquals(1019, Expression.ResultValue.AsCardinal);
    Expression.Free;

    Expression := TTestPascalExpression.Create('Obj1.FTest', Ctx);
    AssertTrue(Expression.Valid);
    AssertTrue(Expression.ResultValue <> nil);
    Expression.Free;

    Expression := TTestPascalExpression.Create('TTestSetup1Class(Obj1).FTest', Ctx);
    AssertTrue(Expression.Valid);
    AssertTrue(Expression.ResultValue <> nil);
    Expression.Free;

    obj1.FTest := obj1;
    Expression := TTestPascalExpression.Create('Obj1.FTest.FWord', Ctx);
    AssertTrue(Expression.Valid);
    AssertTrue(Expression.ResultValue <> nil);
    AssertEquals(1019, Expression.ResultValue.AsCardinal);
    Expression.Free;

    Expression := TTestPascalExpression.Create('TTestSetup1Class(Obj1).FTest.FWord', Ctx);
    AssertTrue(Expression.Valid);
    AssertTrue(Expression.ResultValue <> nil);
    AssertEquals(1019, Expression.ResultValue.AsCardinal);
    Expression.Free;

    Expression := TTestPascalExpression.Create('TTestSetup1Class(TTestSetup1Class(Obj1).FTest).FWord', Ctx);
    AssertTrue(Expression.Valid);
    AssertTrue(Expression.ResultValue <> nil);
    AssertEquals(1019, Expression.ResultValue.AsCardinal);
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
    AssertTrue(Expression.Valid);
    AssertTrue(Expression.ResultValue <> nil);
//TODO
    //AssertEquals(PtrUint(@obj1), Expression.ResultValue.AsCardinal);
    Expression.Free;

    Expression := TTestPascalExpression.Create('@Obj1.FWord', Ctx);
    AssertTrue(Expression.Valid);
    AssertTrue(Expression.ResultValue <> nil);
    AssertEquals(PtrUint(@obj1.FWord), Expression.ResultValue.AsCardinal);
    Expression.Free;

    Expression := TTestPascalExpression.Create('@Obj1.FTest', Ctx);
    AssertTrue(Expression.Valid);
    AssertTrue(Expression.ResultValue <> nil);
//TODO
    //AssertEquals(PtrUint(@obj1), Expression.ResultValue.AsCardinal);
    Expression.Free;

    Expression := TTestPascalExpression.Create('@Obj1.FWord^', Ctx);
    AssertTrue(Expression.Valid);
    AssertTrue(Expression.ResultValue <> nil);
    AssertEquals(1019, Expression.ResultValue.AsCardinal);
    Expression.Free;

    Expression := TTestPascalExpression.Create('(@Obj1^).FWord', Ctx);
    AssertTrue(Expression.Valid);
    AssertTrue(Expression.ResultValue <> nil);
    AssertEquals(1019, Expression.ResultValue.AsCardinal);
    Expression.Free;

    Expression := TTestPascalExpression.Create('@(Obj1.FWord)^', Ctx);
    AssertTrue(Expression.Valid);
    AssertTrue(Expression.ResultValue <> nil);
    AssertEquals(1019, Expression.ResultValue.AsCardinal);
    Expression.Free;

    // Record
    Expression := TTestPascalExpression.Create('Rec1', Ctx);
    AssertTrue(Expression.Valid);
    Expression.ResultValue; // just access it
    Expression.Free;

    Expression := TTestPascalExpression.Create('Rec1.FWord', Ctx);
    AssertTrue(Expression.Valid);
    AssertTrue(Expression.ResultValue <> nil);
    AssertEquals(1021, Expression.ResultValue.AsInteger);
    Expression.Free;

    // var param // old style object
    vobj1.FWord := 1122;
    vobj1.FInt := -122;
    Expression := TTestPascalExpression.Create('vobj1', Ctx);
    AssertTrue(Expression.Valid);
    Expression.ResultValue; // just access it
    Expression.Free;

    Expression := TTestPascalExpression.Create('vobj1.FWord', Ctx);
    AssertTrue(Expression.Valid);
    AssertTrue(Expression.ResultValue <> nil);
    AssertEquals(1122, Expression.ResultValue.AsCardinal);
    Expression.Free;

    Expression := TTestPascalExpression.Create('vobj1.FInt', Ctx);
    AssertTrue(Expression.Valid);
    AssertTrue(Expression.ResultValue <> nil);
    AssertEquals(-122, Expression.ResultValue.AsInteger);
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

