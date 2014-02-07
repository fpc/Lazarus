unit TestTypeInfo;

{$mode objfpc}{$H+}

interface

uses
  FpPascalParser, FpDbgDwarf, FpDbgInfo,
  FpDbgUtil, FpDbgDwarfConst, LazLoggerBase, LazUTF8, sysutils, fpcunit,
  testregistry, TestHelperClasses, TestDwarfSetup1;


type

  { TTestTypInfo }

  TTestTypInfo = class(TTestCase)
  protected
    FDwarfInfo: TDbgDwarf;
  published
    Procedure TestExpressions;
    procedure TestCompareUtf8BothCase;
  end;

implementation

procedure TTestTypInfo.TestCompareUtf8BothCase;
var
  s1, s2,s3: String;
begin
  s2 := UTF8UpperCase( '_vptr$TOBJECT');
  s3 := UTF8LowerCase( '_vptr$TOBJECT');

  s1 := '_vptr$TOBJECT';
  AssertTrue( CompareUtf8BothCase(@s2[1],@s3[1],@s1[1]) );
  s1 := '_Vptr$TOBJECT';
  AssertTrue( CompareUtf8BothCase(@s2[1],@s3[1],@s1[1]) );
  s1 := '_vPtR$TOBJECT';
  AssertTrue( CompareUtf8BothCase(@s2[1],@s3[1],@s1[1]) );
  s1 := '_Vvptr$TOBJECT';
  AssertFalse( CompareUtf8BothCase(@s2[1],@s3[1],@s1[1]) );
end;

procedure TTestTypInfo.TestExpressions;
type
  TTestFlag = (ttHasType, ttNotHasType, ttHasSymbol, ttHasValSymbol, ttHasTypeSymbol);
  TTestFlags = set of TTestFlag;
var
  CurrentTestName: String;
  Ctx: TDbgInfoAddressContext;
  Expression: TFpPascalExpression;

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
        AssertTrue(CurrentTestName + 'Has flag' + s, i in f);
      end;
    For i := low(TDbgSymbolValueFieldFlag) to High(TDbgSymbolValueFieldFlag) do
      if i in ExpNotFlags then begin
        WriteStr(s, i);
        AssertTrue(CurrentTestName + 'Has NOT flag' + s, not (i in f));
      end;
  end;

  procedure InitTest(Expr: String; ExtraName: String = '');
  begin
    if ExtraName <> '' then ExtraName := ' (' + ExtraName + ')';
    CurrentTestName := Expr + ExtraName + ': ';
    Expression.Free;
    Expression := TFpPascalExpression.Create(Expr, Ctx);
//debugln(Expression.DebugDump);
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
          ttHasType:      AssertTrue(CurrentTestName + 'hastype', Expression.ResultValue.TypeInfo <> nil);
          ttNotHasType:   AssertTrue(CurrentTestName + 'has not type', Expression.ResultValue.TypeInfo = nil);
          ttHasSymbol:    AssertTrue(CurrentTestName + 'hassymbol', Expression.ResultValue.DbgSymbol <> nil);
          ttHasValSymbol: AssertTrue(CurrentTestName + 'hassymbol', (Expression.ResultValue.DbgSymbol <> nil) and
                                                  (Expression.ResultValue.DbgSymbol.SymbolType = stValue));
          ttHasTypeSymbol: AssertTrue(CurrentTestName + 'hassymbol', (Expression.ResultValue.DbgSymbol <> nil) and
                                                   (Expression.ResultValue.DbgSymbol.SymbolType = stType));
        end;
  end;
  procedure StartTest(Expr: String; ExpKind: TDbgSymbolKind; TestFlags: TTestFlags = []; ExtraName: String = '');
  var
    s: String;
  begin
    StartTest(Expr, TestFlags, ExtraName);
    WriteStr(s, CurrentTestName, 'Kind exected ', ExpKind, ' but was ', Expression.ResultValue.Kind);
    AssertTrue(s, Expression.ResultValue.Kind = ExpKind);
    if (ttHasType in TestFlags) and (ExpKind <> skNone) then begin
      WriteStr(s, CurrentTestName, 'typeinfo.Kind exected ', ExpKind, ' but was ', Expression.ResultValue.TypeInfo.Kind);
      AssertTrue(s, Expression.ResultValue.TypeInfo.Kind = ExpKind);
    end;
    // some general assumptions
    s := CurrentTestName;
    WriteStr(CurrentTestName, s, ' Expecting for kind:', ExpKind);
    case ExpKind of
      skInstance: ;
      skUnit: ;
      skRecord:  ExpFlags([svfMembers], [svfOrdinal, svfInteger, svfCardinal, svfDataAddress, svfDataSize]);
      skObject:  ExpFlags([svfMembers], [svfOrdinal, svfInteger, svfCardinal, svfDataAddress, svfDataSize]);
      // skClass does NOT have svfSize (maybe svfSizeOfPointer ?);
      skClass:   ExpFlags([svfOrdinal, svfMembers, svfDataAddress, svfDataSize], [svfSize, svfInteger, svfCardinal]);
      skInterface: ;
      skProcedure: ;
      skFunction: ;
      skArray: ;
      // skPointer: svfOrdinal, svfCardinal, svfDataAddress are all the same value
      skPointer:  ExpFlags([svfOrdinal, svfCardinal, svfDataAddress, svfSizeOfPointer], [svfMembers]);
      skInteger:  ExpFlags([svfOrdinal, svfInteger], [svfDataAddress, svfDataSize, svfMembers]);
      skCardinal: ExpFlags([svfOrdinal, svfCardinal], [svfDataAddress, svfDataSize, svfMembers]);
      skBoolean: ;
      skChar: ;
      skFloat: ;
      skString: ;
      skAnsiString: ;
      skCurrency: ;
      skVariant: ;
      skWideString: ;
      skEnum: ;
      skEnumValue: ;
      skSet: ;
      skRegister: ;
    end;
    CurrentTestName := s;
  end;

  procedure StartInvalTest(Expr: String; ExpError: String; ExtraName: String = '');
  begin
    InitTest(Expr, ExtraName);
    Expression.ResultValue;
    AssertTrue(CurrentTestName + 'invalid', (not Expression.Valid) or (Expression.ResultValue = nil));
    //AssertTrue(CurrentTestName + 'invalid', (not Expression.Valid));
    //ExpError
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
    WriteStr(s, CurrentTestName, Field);
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
    WriteStr(s, CurrentTestName, Field);
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
    WriteStr(s, CurrentTestName, Field);
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
    WriteStr(s, CurrentTestName, Field);
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
    WriteStr(s, CurrentTestName, Field);
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
  i, j: Integer;
  FieldsExp: TDbgSymbolValueFieldFlags;
  AddrExp: TDbgPtr;
  s, s2: String;
begin
  ImageLoader := TTestLoaderSetup1.Create;

  obj1 := TTestSetup1Class.Create;
  ImageLoader.TestStackFrame.Int1 := -299;
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

    StartInvalTest('@99', 'xxx');
    StartInvalTest('99^', 'xxx');
    StartInvalTest('longint(@99)', 'xxx');
    StartInvalTest('^longint(@99)', 'xxx');
    StartInvalTest('PInt(@99)', 'xxx');
    StartInvalTest('@Int1^', 'xxx');
    StartInvalTest('^(longint(Int1))', 'xxx'); // no ( allowed between ^ and type


    StartTest('LongInt', [ttHasSymbol, ttNotHasType]);
    StartTest('^LongInt', [ttHasSymbol, ttNotHasType]);
    StartTest('TObject', [ttHasSymbol, ttNotHasType]);
    StartTest('^TObject', [ttHasSymbol, ttNotHasType]);


    // TODO: maybe treat numbers as integer?
    StartTest('244', skCardinal, []);
    ExpFlags([svfCardinal, svfOrdinal], [svfAddress, svfSize, svfSizeOfPointer, svfDataAddress, svfDataSize]);
    ExpResult(svfCardinal, 244);
    ExpResult(svfOrdinal, QWord(244));

    ImageLoader.TestStackFrame.pint1 := @ImageLoader.TestStackFrame.Int1;
    ImageLoader.GlobTestSetup1.VarQWord := PtrInt(@ImageLoader.TestStackFrame.pint1);
    ImageLoader.GlobTestSetup1.VarPointer := @ImageLoader.TestStackFrame.pint1;
    for i := 0 to 22 do begin
      case i of
         0: s := 'Int1';
         1: s := 'longint(Int1)';
         2: s := 'int64(Int1)';
         3: s := '(@Int1)^';
         4: s := 'PInt(@Int1)^';
         5: s := '^longint(@Int1)^';
         6: s := '^longint(pointer(@Int1))^';
         7: s := 'longint(^longint(@Int1)^)';
         8: s := 'int64(^longint(@Int1)^)';
         9: s := 'PInt('+IntToStr((PtrUInt(@ImageLoader.TestStackFrame.Int1)))+')^';
        10: s := '^longint('+IntToStr((PtrUInt(@ImageLoader.TestStackFrame.Int1)))+')^';
        11: s := 'LongInt(Pointer('+IntToStr((PtrUInt(@ImageLoader.TestStackFrame.Int1)))+')^)';
        12: s := '^^longint('+IntToStr((PtrUInt(@ImageLoader.TestStackFrame.PInt1)))+')^^';
        13: s := '^^longint(GlobTestSetup1Pointer)^^';
        14: s := '^^^longint(@GlobTestSetup1Pointer)^^^';
        15: s := '^^longint(GlobTestSetup1QWord)^^';
        16: s := '^^^longint(@GlobTestSetup1QWord)^^^';
        17: s := '^^^longint('+IntToStr((PtrUInt(@ImageLoader.GlobTestSetup1.VarPointer)))+')^^^';
        18: s := '(^^^longint('+IntToStr((PtrUInt(@ImageLoader.GlobTestSetup1.VarPointer)))+')^^)^';
        19: s := '(^^^longint('+IntToStr((PtrUInt(@ImageLoader.GlobTestSetup1.VarPointer)))+')^)^^';
        20: s := '((^^^longint('+IntToStr((PtrUInt(@ImageLoader.GlobTestSetup1.VarPointer)))+')^)^)^';
        21: s := '^^PInt('+IntToStr((PtrUInt(@ImageLoader.GlobTestSetup1.VarPointer)))+')^^^';
        22: s := '(^^PInt('+IntToStr((PtrUInt(@ImageLoader.GlobTestSetup1.VarPointer)))+')^^)^';
      end;

      StartTest(s, skInteger, [ttHasType]);
      ExpFlags([svfInteger, svfOrdinal, svfAddress], [svfCardinal, svfDataAddress]); // svfSize;
      ExpResult(svfInteger, -299);
      ExpResult(svfOrdinal, QWord(-299));
      ExpResult(svfAddress, TDbgPtr(PtrUInt(@ImageLoader.TestStackFrame.Int1)));
      case i of
        0,1,3..7,10..22: ExpResult(svfSize, 4); // integer
        2,8: ExpResult(svfSize, 8); // int64
      end;
    end;

    for i := 0 to 19 do begin
      case i of
         0: s := '@Int1';
         1: s := 'PInt(@Int1)';
         2: s := 'PInt(Pointer(@Int1))';
         3: s := '^longint(@Int1)';
         4: s := '^word(@Int1)';
         5: s := '^int64(@Int1)';
         6: s := '@longint(Int1)';
         7: s := '@(longint(Int1))';
         8: s := '@word(Int1)';
         9: s := '@int64(Int1)';
        10: s := '^longint('+IntToStr((PtrUInt(@ImageLoader.TestStackFrame.Int1)))+')';
        11: s := 'PInt('+IntToStr((PtrUInt(@ImageLoader.TestStackFrame.Int1)))+')';
        12: s := '@PInt(@Int1)^';
        13: s := '@^longint(@Int1)^';
        14: s := '^^longint('+IntToStr((PtrUInt(@ImageLoader.TestStackFrame.PInt1)))+')^';
        15: s := '^^longint(GlobTestSetup1Pointer)^';
        16: s := '^^^longint(@GlobTestSetup1Pointer)^^';
        17: s := '^^longint(GlobTestSetup1QWord)^';
        18: s := '^^^longint(@GlobTestSetup1QWord)^^';
        19: s := '^^^longint('+IntToStr((PtrUInt(@ImageLoader.GlobTestSetup1.VarPointer)))+')^^';
      end;

      StartTest(s, skPointer, [ttHasType]);
      ExpFlags([svfCardinal, svfOrdinal, svfDataAddress], [svfAddress]);
      ExpResult(svfOrdinal, PtrUInt(@ImageLoader.TestStackFrame.Int1));
      ExpResult(svfDataAddress, TDbgPtr(PtrUInt(@ImageLoader.TestStackFrame.Int1)));
    end;

    for i := 0 to 5 do begin
      case i of
         0: s := '^^longint('+IntToStr((PtrUInt(@ImageLoader.TestStackFrame.PInt1)))+')';
         1: s := '^^longint(GlobTestSetup1Pointer)';
         2: s := '^^^longint(@GlobTestSetup1Pointer)^';
         3: s := '^^longint(GlobTestSetup1QWord)';
         4: s := '^^^longint(@GlobTestSetup1QWord)^';
         5: s := '^^^longint('+IntToStr((PtrUInt(@ImageLoader.GlobTestSetup1.VarPointer)))+')^';
      end;

      StartTest(s, skPointer, [ttHasType]);
      ExpFlags([svfCardinal, svfOrdinal, svfDataAddress], [svfAddress]);
      ExpResult(svfOrdinal, PtrUInt(@ImageLoader.TestStackFrame.PInt1));
      ExpResult(svfDataAddress, TDbgPtr(PtrUInt(@ImageLoader.TestStackFrame.PInt1)));
    end;


    // intentionally read more mem
    StartTest('^int64(@Int1)^', skInteger, [ttHasType]);
    ExpFlags([svfInteger, svfOrdinal, svfAddress], [svfCardinal, svfDataAddress]); // svfSize;
    ExpResult(svfAddress, TDbgPtr(PtrUInt(@ImageLoader.TestStackFrame.Int1)));
    DebugLn([Expression.ResultValue.AsInteger, '  ', IntToHex(Expression.ResultValue.AsInteger,16)]);
    AssertTrue(CurrentTestName+'unknown result', -299 <> Expression.ResultValue.AsInteger);
    AssertTrue(CurrentTestName+'result and mask',
               (Integer((Expression.ResultValue.AsInteger shr 32) and int64($ffffffff))  = -299) or
               (Integer((Expression.ResultValue.AsInteger)        and int64($ffffffff))  = -299) );

    StartTest('Word(Int1)');
    ExpFlags([svfCardinal, svfOrdinal, svfAddress], [svfInteger, svfDataAddress]); // svfSize;
    ExpResult(svfCardinal, $FED5);
    ExpResult(svfAddress, TDbgPtr(PtrUInt(@ImageLoader.TestStackFrame.Int1)));

    StartTest('LongInt(Obj1)');
    ExpResult(svfOrdinal, PtrUInt(ImageLoader.TestStackFrame.Obj1));
    ExpResult(svfInteger, PtrInt(ImageLoader.TestStackFrame.Obj1));
    ExpFlags([svfInteger, svfOrdinal, svfAddress], [svfCardinal]); // svfSize;

    // Class/Object
    ImageLoader.GlobTestSetup1.VarQWord := PtrInt(obj1);
    ImageLoader.GlobTestSetup1.VarPointer := @ImageLoader.TestStackFrame.Obj1;
    ImageLoader.TestStackFrame.PObj1 := @ImageLoader.TestStackFrame.Obj1;
    ImageLoader.TestStackFrame.VParamTestSetup1Class := @ImageLoader.TestStackFrame.Obj1;
    ImageLoader.TestStackFrame.VParamTestSetup1ClassP := @ImageLoader.TestStackFrame.PObj1;
    Obj1.FWord := 1019;
    Obj1.FWordL := QWord($9aa99aa97bb7b77b); // Make sure there is data, if other fields read to much

    for i := 0 to 23 do begin
       case i of
         11..13, 23: ImageLoader.GlobTestSetup1.VarPointer := @ImageLoader.TestStackFrame.Obj1;
         14:   ImageLoader.GlobTestSetup1.VarPointer := Pointer(ImageLoader.TestStackFrame.Obj1);
       end;
      // Different ways to access an object
      case i of
         0: s := 'Obj1';
         1: s := 'TTestSetup1Class(Obj1)';
         2: s := 'TTestSetup1Class(TObject(Obj1))';
         3: s := 'TTestSetup1Class(Pointer(Obj1))';
         4: s := 'TTestSetup1Class(TTestSetup1Class(Obj1))';
         // Downcast
         5: s := 'TObject(Obj1)';
         6: s := 'TObject(TTestSetup1Class(Obj1))';
         // object from const address (ord value of obj variable / pointer to obj-data)
         7: s := 'TTestSetup1Class('+IntToStr(PtrUInt(obj1))+')'; // no address
         8: s := 'TTestSetup1Class(Pointer('+IntToStr(PtrUInt(obj1))+'))'; // no address
         9: s := 'TTestSetup1Class(QWord('+IntToStr(PtrUInt(obj1))+'))'; // no address
         // object stored in QWord
        10: s := 'TTestSetup1Class(GlobTestSetup1QWord)';
         // pointer to object
        11: s := 'TTestSetup1Class(GlobTestSetup1Pointer^)';
        12: s := 'PTestSetup1Class(GlobTestSetup1Pointer)^';
         // object from const address of object var (pointer to obj)
        13: s := 'PTestSetup1Class('+IntToStr(PtrUInt(ImageLoader.GlobTestSetup1.VarPointer))+')^';
         // object stored in pointer (typecasted / NOT pointer to object)
        14: s := 'TTestSetup1Class(GlobTestSetup1Pointer)'; // no deref
        15: s := '(@Obj1)^';
        16: s := 'TTestSetup1Class(@(PInt(Obj1)^))';
         //
        17: s := 'PObj1^';
        18: s := 'TTestSetup1Class(PObj1^)';
        19: s := 'PTestSetup1Class(PObj1)^';
        20: s := '(@PObj1)^^';
        21: s := 'VParamTestSetup1Class';
        22: s := 'VParamTestSetup1ClassP^';
        23: s := '^TTestSetup1Class(GlobTestSetup1Pointer)^';
      end;
      FieldsExp := [svfMembers, svfOrdinal, svfAddress, svfDataAddress]; // svfSize dataSize;
      AddrExp   := TDbgPtr(@ImageLoader.TestStackFrame.Obj1);
      if i in [7..9, 16] then FieldsExp := FieldsExp - [svfAddress];
      if i in [10] then AddrExp := TDbgPtr(@ImageLoader.GlobTestSetup1.VarQWord);
      if i in [14] then AddrExp := TDbgPtr(@ImageLoader.GlobTestSetup1.VarPointer);

      // Check result for object
      StartTest(s, skClass, [ttHasType]);
      ExpFlags(FieldsExp);
      if i in [7..9, 16] then
        ExpFlags([], [svfAddress]);
      if svfAddress in FieldsExp then
        ExpResult(svfAddress, AddrExp);
      ExpResult(svfDataAddress, TDbgPtr(PtrUInt(ImageLoader.TestStackFrame.Obj1)));
      ExpResult(svfOrdinal, PtrUInt (ImageLoader.TestStackFrame.Obj1));
      case i of
        5,6: ExpResult(svfDataSize, TObject.InstanceSize);
        else ExpResult(svfDataSize, ImageLoader.TestStackFrame.Obj1.InstanceSize);
      end;


      // Check result for @object
      if svfAddress in FieldsExp then begin
        StartTest('@('+s+')', skPointer, [ttHasType]);
        ExpFlags([svfCardinal, svfOrdinal, svfDataAddress], [svfAddress]);
        ExpResult(svfCardinal, QWord(AddrExp));
        ExpResult(svfDataAddress, AddrExp);

        if not (i in [12, 13, 15, 17, 19, 20]) then begin
          StartTest('@'+s, skPointer, [ttHasType]);
          ExpFlags([svfCardinal, svfOrdinal, svfDataAddress], [svfAddress]);
          ExpResult(svfCardinal, QWord(AddrExp));
          ExpResult(svfDataAddress, AddrExp);
        end;
      end;


      if not (i in [5,6]) then begin

        // Object.FWord
        for j := 0 to 2 do begin
          case j of
            0: s2 := s+'.FWord';
            1: s2 := 'Word('+s+'.FWord)';
            2: s2 := '(@('+s+'.FWord))^';
          end;
          StartTest(s2, skCardinal, [ttHasType]);
          ExpFlags([svfCardinal, svfOrdinal, svfAddress], [svfDataAddress]); // svfSize;
          ExpResult(svfCardinal, 1019);
          ExpResult(svfOrdinal, 1019);
          ExpResult(svfAddress, TDbgPtr(@ImageLoader.TestStackFrame.Obj1.FWord));
        end;

        // @Object.FWord
        StartTest('@('+s2+')');
        ExpFlags([svfCardinal, svfOrdinal, svfDataAddress], [svfAddress]);
        ExpResult(svfCardinal, QWord(TDbgPtr(@ImageLoader.TestStackFrame.Obj1.FWord)));
        ExpResult(svfDataAddress, TDbgPtr(@ImageLoader.TestStackFrame.Obj1.FWord));

        if not (j in [2]) then begin
          StartTest('@'+s2);
          ExpFlags([svfCardinal, svfOrdinal, svfDataAddress], [svfAddress]);
          ExpResult(svfCardinal, QWord(TDbgPtr(@ImageLoader.TestStackFrame.Obj1.FWord)));
          ExpResult(svfDataAddress, TDbgPtr(@ImageLoader.TestStackFrame.Obj1.FWord));
        end;


        // Object.FTest
        ImageLoader.TestStackFrame.Obj1.FTest := nil;
        StartTest(s+'.FTest', skClass, [ttHasType]);
        ExpFlags([svfMembers, svfOrdinal, svfAddress, svfDataAddress]); // svfSize;
        ExpResult(svfAddress, TDbgPtr(@ImageLoader.TestStackFrame.Obj1.FTest));
        ExpResult(svfDataAddress, TDbgPtr(PtrUInt(ImageLoader.TestStackFrame.Obj1.FTest)));
        ExpResult(svfOrdinal, QWord(PtrUInt(ImageLoader.TestStackFrame.Obj1.FTest)));


        ImageLoader.TestStackFrame.Obj1.FTest := obj1;
        StartTest(s+'.FTest.FWord', skCardinal, [ttHasSymbol]);
        ExpResult(svfCardinal, 1019);
        ExpFlags([svfCardinal, svfOrdinal, svfAddress], [svfDataAddress]); // svfSize;

      end; // not (i in [4,5])

    end;


    StartTest('@Obj1.FTest');
    ExpResult(svfCardinal, PtrUint(@ImageLoader.TestStackFrame.Obj1.FTest));
    ExpFlags([svfCardinal, svfOrdinal], [svfAddress]);

    // TODO: NOT valid (^ operates before @
    StartInvalTest('@Obj1.FWord^', 'xxx');

    StartTest('(@Obj1.FWord)^');
    ExpResult(svfCardinal, 1019);
    ExpFlags([svfCardinal, svfOrdinal, svfAddress]); // svfSize;


    StartInvalTest('Obj1.@FWord', 'xxx');
    StartInvalTest('Obj1.@Int1', 'xxx');
    StartInvalTest('Obj1.@Obj1', 'xxx');
    StartInvalTest('Obj1.Obj1', 'xxx');
    StartInvalTest('Int1.FWord', 'xxx');
    StartInvalTest('Obj1.NotExisting', 'xxx');
    StartInvalTest('Obj1.123', 'xxx');
    StartInvalTest('123.NotExisting', 'xxx');

    StartInvalTest('TObject(Obj1).FWord', 'xxx');

    // Record
    // VParamTestRecord mis-named
    ImageLoader.TestStackFrame.Rec1.FWord := $9ab7;
    ImageLoader.TestStackFrame.PRec1 := @ImageLoader.TestStackFrame.Rec1;
    ImageLoader.TestStackFrame.VParamTestSetup1Record := @ImageLoader.TestStackFrame.Rec1;
    ImageLoader.TestStackFrame.VParamTestRecord := @ImageLoader.TestStackFrame.PRec1;

    StartTest('PRec1', skPointer, [ttHasType]);
    ExpFlags([svfCardinal, svfOrdinal, svfAddress, svfDataAddress]); // svfSize;

    for i := 0 to 5 do begin
      case i of
         0: s := 'Rec1';
         1: s := 'PRec1^';
         2: s := '(@Rec1)^';
         3: s := '(@PRec1)^^';
         4: s := 'VParamTestSetup1Record';
         5: s := 'VParamTestRecord^';
      end;

      StartTest(s, skRecord, [ttHasType]);
      // svfSize;
      ExpFlags([svfMembers, svfAddress], [svfOrdinal, svfCardinal, svfInteger, svfDataAddress]);
      ExpResult(svfAddress, TDbgPtr(PtrUInt(@ImageLoader.TestStackFrame.Rec1)));
      ExpResult(svfSize, SizeOf(ImageLoader.TestStackFrame.Rec1));


      StartTest(s+'.FWord', skCardinal, [ttHasType]);
      ExpFlags([svfCardinal, svfOrdinal, svfAddress], [svfDataAddress]); // svfSize;
      ExpResult(svfCardinal, $9ab7);
      ExpResult(svfAddress, TDbgPtr(PtrUInt(@ImageLoader.TestStackFrame.Rec1.FWord)));

      StartTest('@'+s+'.FWord', skPointer, [ttHasType]);
      ExpFlags([svfCardinal, svfOrdinal, svfDataAddress], [svfAddress]); // svfSize;
      ExpResult(svfCardinal, QWord(PtrUInt(@ImageLoader.TestStackFrame.Rec1.FWord)));
      ExpResult(svfDataAddress, TDbgPtr(PtrUInt(@ImageLoader.TestStackFrame.Rec1.FWord)));


      ImageLoader.TestStackFrame.Rec1.FBool := False;
      StartTest(s+'.FBool', skBoolean, [ttHasType]);
      ExpFlags([svfBoolean, svfOrdinal, svfAddress], [svfDataAddress]); // svfSize;
      ExpResult(svfBoolean, False);
      ExpResult(svfAddress, TDbgPtr(PtrUInt(@ImageLoader.TestStackFrame.Rec1.FBool)));

      ImageLoader.TestStackFrame.Rec1.FBool := True;
      StartTest(s+'.FBool', skBoolean, [ttHasType]);
      ExpResult(svfBoolean, True);

    end;

    // Record
    ImageLoader.TestStackFrame.Rec1.FWord := 1021;
    StartTest('Rec1');

    StartTest('Rec1.FWord');
    ExpResult(svfCardinal, 1021);
    ExpFlags([svfCardinal, svfOrdinal, svfAddress]);


    // type = Object ... end;
    StartTest('OldObj1', skObject, [ttHasType]);
    ExpFlags([svfMembers, svfAddress], [svfOrdinal, svfCardinal, svfInteger, svfDataAddress]);
    ExpResult(svfAddress, TDbgPtr(PtrUInt(@ImageLoader.TestStackFrame.OldObj1)));

    // var param // old style object
    vobj1.FWord := 1122;
    vobj1.FInt := -122;
    ImageLoader.TestStackFrame.OldObj1 := vobj1;
    ImageLoader.TestStackFrame.POldObj1 := @vobj1;
    ImageLoader.TestStackFrame.VParamTestSetup1Object := @vobj1;
    ImageLoader.TestStackFrame.VParamTestSetup1ObjectP := @ImageLoader.TestStackFrame.POldObj1;

    for i := 0 to 5 do begin
      case i of
         2,4: AddrExp := TDbgPtr(PtrUInt(@ImageLoader.TestStackFrame.OldObj1));
         else AddrExp := TDbgPtr(PtrUInt(@vobj1));
      end;
      case i of
         0: s := 'VParamTestSetup1Object';
         1: s := 'VParamTestSetup1ObjectP^';
         2: s := 'OldObj1';
         3: s := 'POldObj1^';
         4: s := '(@OldObj1)^';
         5: s := '(@POldObj1)^^';
      end;

      StartTest(s, skObject, [ttHasType]);
      ExpFlags([svfMembers, svfAddress], [svfOrdinal, svfCardinal, svfInteger, svfDataAddress]);
      ExpResult(svfAddress, AddrExp);
      ExpResult(svfSize, SizeOf(ImageLoader.TestStackFrame.OldObj1));

      case i of
         2,4: AddrExp := TDbgPtr(PtrUInt(@ImageLoader.TestStackFrame.OldObj1.FWord));
         else AddrExp := TDbgPtr(PtrUInt(@vobj1.FWord));
      end;
      StartTest(s+'.FWord');
      ExpFlags([svfCardinal, svfOrdinal, svfAddress]);
      ExpResult(svfCardinal, 1122);
      ExpResult(svfAddress, AddrExp);

      StartTest(s+'.FInt');
      ExpResult(svfInteger, -122);
      ExpFlags([svfInteger, svfOrdinal, svfAddress]);

    end;


    // pointer
    ImageLoader.TestStackFrame.Int1 := -299;
    ImageLoader.TestStackFrame.pi := @ImageLoader.TestStackFrame.int1;
    ImageLoader.GlobTestSetup1.VarPointer := @ImageLoader.TestStackFrame.int1;
    ImageLoader.GlobTestSetup1.VarQWord := QWord(@ImageLoader.TestStackFrame.int1);

    StartTest('pi', skPointer, [ttHasType]);
    ExpResult(svfOrdinal, QWord(@ImageLoader.TestStackFrame.int1));
    ExpResult(svfAddress, QWord(@ImageLoader.TestStackFrame.pi));
    ExpResult(svfDataAddress, QWord(@ImageLoader.TestStackFrame.int1));
    ExpFlags([svfOrdinal, svfAddress, svfDataAddress, svfSizeOfPointer]);

    StartTest('GlobTestSetup1Pointer', skPointer, [ttHasType]);
    ExpResult(svfOrdinal, QWord(@ImageLoader.TestStackFrame.int1));
    ExpResult(svfAddress, QWord(@ImageLoader.GlobTestSetup1.VarPointer));
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
    ExpResult(svfAddress, QWord(@ImageLoader.GlobTestSetup1.VarPointer));
    ExpResult(svfDataAddress, QWord(@ImageLoader.TestStackFrame.int1));
    ExpFlags([svfOrdinal, svfAddress, svfDataAddress, svfSizeOfPointer]);

    StartTest('pint(GlobTestSetup1Pointer)', skPointer, [ttHasType]);
    ExpResult(svfOrdinal, QWord(@ImageLoader.TestStackFrame.int1));
    ExpResult(svfAddress, QWord(@ImageLoader.GlobTestSetup1.VarPointer));
    ExpResult(svfDataAddress, QWord(@ImageLoader.TestStackFrame.int1));
    ExpFlags([svfOrdinal, svfAddress, svfDataAddress, svfSizeOfPointer]);

    StartTest('pi^', skInteger, [ttHasType]);
    ExpResult(svfInteger, -299);
    ExpFlags([svfInteger, svfOrdinal, svfAddress], [svfDataAddress]); // svfSize;

    StartTest('PInt(pi)^', skInteger, [ttHasType]);
    ExpResult(svfInteger, -299);
    ExpFlags([svfInteger, svfOrdinal, svfAddress], [svfDataAddress]); // svfSize;

    StartTest('LongInt(pointer(pi)^)', skInteger, [ttHasType]);
    ExpResult(svfInteger, -299);
    ExpFlags([svfInteger, svfOrdinal, svfAddress], [svfDataAddress]); // svfSize;

    StartTest('PInt(GlobTestSetup1Pointer)^', skInteger, [ttHasType]);
    ExpResult(svfInteger, -299);
    ExpFlags([svfInteger, svfOrdinal, svfAddress], [svfDataAddress]); // svfSize;

    StartTest('LongInt(GlobTestSetup1Pointer^)', skInteger, [ttHasType]);
    ExpResult(svfInteger, -299);
    ExpFlags([svfInteger, svfOrdinal, svfAddress], [svfDataAddress]); // svfSize;

    StartTest('PInt(GlobTestSetup1QWord)^', skInteger, [ttHasType]);
    ExpResult(svfInteger, -299);
    ExpFlags([svfInteger, svfOrdinal, svfAddress], [svfDataAddress]); // svfSize;

    StartInvalTest('LongInt(GlobTestSetup1QWord^)', '');

    //StartTest('LongInt(GlobTestSetup1QWord^)', skInteger, [ttHasType]);
    //ExpResult(svfInteger, -299);
    //ExpFlags([svfInteger, svfOrdinal, svfAddress], [svfDataAddress]); // svfSize;


    ImageLoader.TestStackFrame.pi := @ImageLoader.TestStackFrame.Obj1;
    ImageLoader.GlobTestSetup1.VarPointer := @ImageLoader.TestStackFrame.Obj1;
    ImageLoader.GlobTestSetup1.VarQWord := QWord(@ImageLoader.TestStackFrame.Obj1);

    StartTest('TTestSetup1Class(pi^).FWord', skCardinal, [ttHasType]);
    ExpResult(svfCardinal, 1019);
    ExpFlags([svfCardinal, svfOrdinal, svfAddress]); // svfSize; //

    StartTest('PTestSetup1Class(pi)^.FWord', skCardinal, [ttHasType]);
    ExpResult(svfCardinal, 1019);
    ExpFlags([svfCardinal, svfOrdinal, svfAddress]); // svfSize; //

    StartTest('TTestSetup1Class(GlobTestSetup1Pointer^).FWord', skCardinal, [ttHasType]);
    ExpResult(svfCardinal, 1019);
    ExpFlags([svfCardinal, svfOrdinal, svfAddress]); // svfSize; //

    StartTest('PTestSetup1Class(GlobTestSetup1Pointer)^.FWord', skCardinal, [ttHasType]);
    ExpResult(svfCardinal, 1019);
    ExpFlags([svfCardinal, svfOrdinal, svfAddress]); // svfSize; //

    StartTest('PTestSetup1Class(GlobTestSetup1QWord)^.FWord', skCardinal, [ttHasType]);
    ExpResult(svfCardinal, 1019);
    ExpFlags([svfCardinal, svfOrdinal, svfAddress]); // svfSize; //

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

