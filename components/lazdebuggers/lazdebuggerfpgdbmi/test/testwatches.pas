unit TestWatches;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils, fpcunit, testutils, testregistry, TestGDBMIControl,
  DbgIntfBaseTypes, DbgIntfDebuggerBase, TestBase, FpGdbmiDebugger, LCLProc, SynRegExpr,
  TestWatchUtils, GDBMIDebugger;

const
  BREAK_COUNT_TestWatchesUnitSimple = 17;
  BL_TW1  = 373; // Test1Sub
  BL_TW2  = 378; // Simple_NoneNested
  BL_TW3  = 611; // Test1Method
  BL_TW4  = 507; // Test1Method Nested1
  BL_TW5  = 641; // Test2Method
  BL_TW6  = 622; // Test2Method Nested2a
  BL_TW7  = 628; // Test2Method Nested2b
  BL_TW8  = 632; // Test2Method Nested2c
  BL_TW9  = 389; // Test0Method
  BL_TW10 = 386; // Test0Method Nested0
  BREAK_LINE_TestWatchesUnitSimple: array [1..BREAK_COUNT_TestWatchesUnitSimple] of Integer =
    ( BL_TW1,  // 1:  Test1Sub
      BL_TW3,  // 2:  Class1.Test1Method
      BL_TW4,  // 3:  Class1.Test1Method > Nested1
      BL_TW2,  // 4:  Class1.Test1Method > Nested1 > Simple_NoneNested
      BL_TW2,  // 5:  Class1.Test1Method > Simple_NoneNested
      BL_TW9,  // 6:  Class1.Test1Method > Test0Method
      BL_TW10, // 7:  Class1.Test1Method > Test0Method > Nested0
      BL_TW5,  // 8:  Class2.Test2Method
      BL_TW6,  // 9:  Class2.Test2Method > Nested2a
      BL_TW7,  // 10: Class2.Test2Method > Nested2b
      BL_TW7,  // 11: Class2.Test2Method > Nested2b > Nested2b
      BL_TW8,  // 12: Class2.Test2Method > Nested2b > Nested2b > Nested2c
      BL_TW6,  // 13: Class2.Test2Method > Nested2b > Nested2b > Nested2c > Nested2a
      BL_TW9,  // 14: Class2.Test0Method

      BL_TW10, // 15: Class2.Test0Method > Nested0
      BL_TW9,  // 16: Class0.Test0Method
      BL_TW10  // 17: Class0.Test0Method > Nested0
    );

  BREAK_LINE_TestWatchesUnitArray = 842;

type

  { TTestWatches }

  TTestWatches = class(TTestWatchesBase)
  private
    FWatches: TWatches;

    ExpectBreakSimple: array [1..BREAK_COUNT_TestWatchesUnitSimple] of TWatchExpectationArray;

    ExpectBreakArray_1: TWatchExpectationArray;

    FCurrentExpect: PWatchExpectationArray; // currently added to

    FDbgOutPut: String;
    FDbgOutPutEnable: Boolean;

    procedure DoDbgOutput(Sender: TObject; const AText: String); override;
    procedure ClearAllTestArrays;
    function  HasTestArraysData: Boolean;

    function Add(AnExpr:  string; AFmt: TWatchDisplayFormat; AMtch: string;
                 AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags): PWatchExpectation;
    function Add(AnExpr:  string; AFmt: TWatchDisplayFormat; AEvalFlags: TDBGEvaluateFlags; AMtch: string;
                 AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags): PWatchExpectation;
    function AddFmtDef        (AnExpr, AMtch: string; AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags=[]): PWatchExpectation;
    function AddFmtDef        (AnExpr: String; AEvalFlags: TDBGEvaluateFlags; AMtch: string; AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags=[]): PWatchExpectation;

    function AddSimpleInt(AnExpr: string; AMtch: Int64; ATpNm: string): PWatchExpectation;
    function AddSimpleUInt(AnExpr: string; AMtch: QWord; ATpNm: string): PWatchExpectation;

    procedure AdjustExpectToAddress(AWatchExp: PWatchExpectation); // only ExpectBreakSimple_1
    procedure AdjustArrayExpectToAddress(AWatchExp: PWatchExpectation); // only ExpectBreakSimple_1

    procedure AddExpectSimple;
    procedure AddExpectArray_1;
    procedure RunTestWatches(NamePreFix: String;
                             TestExeName, ExtraOpts: String;
                             UsedUnits: array of TUsesDir
                            );
  published
    procedure TestWatches;
  end;

implementation

const
  RNoPreQuote  = '(^|[^''])'; // No open qoute (Either at start, or other char)
  RNoPostQuote = '($|[^''])'; // No close qoute (Either at end, or other char)
  Match_Pointer = '\$[0-9A-F]+';
  M_Int = 'Integer|LongInt';

  {%region    * Classes * }
  // _vptr$TOBJECt on older gdb e.g. mac 6.3.50
  Match_ArgTFoo = '<TFoo> = \{.*?(<|vptr\$)TObject>?.+ValueInt = -11';
  Match_ArgTFoo1 = '<TFoo> = \{.*?(<|vptr\$)TObject>?.+ValueInt = 31';
  {%ebdregion    * Classes * }
  // Todo: Dwarf fails with dereferenced var pointer types

function MatchPointer(TypeName: String=''): String;
begin
  if TypeName = ''
  then Result := '\$[0-9A-F]+'
  else Result := TypeName+'\(\$[0-9A-F]+';
end;

function MatchRecord(TypeName: String; AContent: String = ''): String;
begin
  Result := 'record '+TypeName+' .+'+AContent;
end;
function MatchRecord(TypeName: String; AValInt: integer; AValFoo: String = ''): String;
begin
  Result := 'record '+TypeName+' .+ valint = '+IntToStr(AValInt);
  If AValFoo <> '' then Result := Result + ',.*? valfoo = '+AValFoo;
end;

function MatchClass(TypeName: String; AContent: String = ''): String;
begin
  Result := '<'+TypeName+'> = \{.*?(vptr\$|<TObject>).+'+AContent;
end;

function MatchClassNil(TypeName: String): String;
begin
  Result := '<'+TypeName+'> = nil';
end;


{ TTestWatches }

procedure TTestWatches.AdjustExpectToAddress(AWatchExp: PWatchExpectation);
var
  OtherWatchExp: TWatchExpectation;
  s: String;
  st: TSymbolType;
  OtherList: PWatchExpectationArray;
begin
  OtherList := PWatchExpectationArray(AWatchExp^.UserData2);
  OtherWatchExp := OtherList^[PtrUInt(AWatchExp^.UserData)];
  if OtherWatchExp.TheWatch = nil then begin
    debugln(['SKIPPING watch update']);
    exit;
  end;
  s := OtherWatchExp.TheWatch.Values[1,0].Value;
  delete(s, 1, pos('$', s) - 1); delete(s, pos(')', s), 99);
  for st := low(TSymbolType) to high(TSymbolType) do
    AWatchExp^.Result[st].ExpMatch := '\'+s;
end;

procedure TTestWatches.AdjustArrayExpectToAddress(AWatchExp: PWatchExpectation);
var
  OtherWatchExp: TWatchExpectation;
  s: String;
  st: TSymbolType;
  OtherList: PWatchExpectationArray;
begin
  OtherList := PWatchExpectationArray(AWatchExp^.UserData2);
  //OtherWatchExp := OtherList^[PtrUInt(AWatchExp^.UserData)];
  OtherWatchExp := ExpectBreakArray_1[PtrUInt(AWatchExp^.UserData)];
  if OtherWatchExp.TheWatch = nil then begin
    debugln(['SKIPPING watch update']);
    exit;
  end;
  s := OtherWatchExp.TheWatch.Values[1,0].Value;
  delete(s, 1, pos('$', s) - 1); delete(s, pos(')', s), 99);
  for st := low(TSymbolType) to high(TSymbolType) do begin
    AWatchExp^.Result[st].ExpMatch := AnsiReplaceText(AWatchExp^.Result[st].ExpMatch, 'REPLACEME', s);
  end;
  AWatchExp^.TestName := AnsiReplaceText(AWatchExp^.TestName, 'REPLACEME', 'REPLACEME='+s);
  AWatchExp^.Expression := AnsiReplaceText(AWatchExp^.Expression, 'REPLACEME', s);
  AWatchExp^.TheWatch.Expression := AWatchExp^.Expression;
end;

procedure TTestWatches.DoDbgOutput(Sender: TObject; const AText: String);
begin
  inherited DoDbgOutput(Sender, AText);
  if FDbgOutPutEnable then
    FDbgOutPut := FDbgOutPut + AText;
end;

procedure TTestWatches.ClearAllTestArrays;
var
  i: Integer;
begin
  for i := 1 to BREAK_COUNT_TestWatchesUnitSimple do
    SetLength(ExpectBreakSimple[i], 0);
  SetLength(ExpectBreakArray_1, 0);
end;

function TTestWatches.HasTestArraysData: Boolean;
var
  i: Integer;
begin
  Result :=
    (Length(ExpectBreakArray_1) > 0);
  for i := 1 to BREAK_COUNT_TestWatchesUnitSimple do
    Result := Result or (Length(ExpectBreakSimple[i]) > 0);
end;

function TTestWatches.Add(AnExpr: string; AFmt: TWatchDisplayFormat; AMtch: string;
  AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags): PWatchExpectation;
begin
  Result := AddWatchExp(FCurrentExpect^, AnExpr, AFmt, AMtch, AKind, ATpNm, AFlgs );
end;

function TTestWatches.Add(AnExpr: string; AFmt: TWatchDisplayFormat;
  AEvalFlags: TDBGEvaluateFlags; AMtch: string; AKind: TDBGSymbolKind; ATpNm: string;
  AFlgs: TWatchExpectationFlags): PWatchExpectation;
begin
  Result := AddWatchExp(FCurrentExpect^, AnExpr, AFmt, AEvalFlags, AMtch, AKind, ATpNm, AFlgs );
end;

function TTestWatches.AddFmtDef(AnExpr, AMtch: string; AKind: TDBGSymbolKind; ATpNm: string;
  AFlgs: TWatchExpectationFlags): PWatchExpectation;
begin
  Result := Add(AnExpr, wdfDefault, AMtch, AKind, ATpNm, AFlgs );
end;

function TTestWatches.AddFmtDef(AnExpr: String; AEvalFlags: TDBGEvaluateFlags; AMtch: string;
  AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags): PWatchExpectation;
begin
  Result := Add(AnExpr, wdfDefault, AEvalFlags, AMtch, AKind, ATpNm, AFlgs );
end;

function TTestWatches.AddSimpleInt(AnExpr: string; AMtch: Int64;
  ATpNm: string): PWatchExpectation;
begin
  Result := AddFmtDef(AnExpr, '^'+IntToStr(AMtch), skSimple, ATpNm, [fTpMtch]);
end;

function TTestWatches.AddSimpleUInt(AnExpr: string; AMtch: QWord;
  ATpNm: string): PWatchExpectation;
begin
  Result := AddFmtDef(AnExpr, '^'+IntToStr(AMtch), skSimple, ATpNm, [fTpMtch]);
end;

procedure TTestWatches.AddExpectSimple;
  procedure AddExpectSimple_Test1Sub;
  var
    i, j: Integer;
    s, s2, s2def: String;
    r: PWatchExpectation;
  begin
    {%region Int/Cardinal types}
    for i := 0 to 3 do begin
      s2def := '';
      case i of
        0: s := '%s';
        1: s := '(@%s)^';
        2: s := 'Int64(%s)';
        3: s := 'QWord(%s)';
      end;
      case i of
        2: s2def := 'Int64';
        3: s2def := 'QWord';
      end;


      s2 := s2def;
      if s2 = '' then s2 := 'ShortInt';
      if not(i in [3]) then begin
        AddSimpleInt(Format(s, ['SimpleArg_Short1']),   -92, s2);
        AddSimpleInt(Format(s, ['SimpleVArg_Short1']),  -91, s2);
      end else begin
        AddSimpleUInt(Format(s, ['SimpleArg_Short1']),   QWord(-92), s2);
        AddSimpleUInt(Format(s, ['SimpleVArg_Short1']),  QWord(-91), s2);
      end;
      AddSimpleInt(Format(s, ['Local_Short1']), 39, s2);
      AddSimpleInt(Format(s, ['SimpleGlob_Short1 ']), 29, s2);
      AddSimpleInt(Format(s, ['SimpleGlob_Short2']),  0, s2);
      if not(i in [3]) then begin
        AddSimpleInt(Format(s, ['SimpleGlob_Short3']),  -1, s2);
      end;
      AddSimpleInt(Format(s, ['SimpleGlob_Short4']),  high(ShortInt), s2);
      if not(i in [3]) then begin
        AddSimpleInt(Format(s, ['SimpleGlob_Short5']),  low(ShortInt), s2);
      end;

      s2 := s2def;
      if s2 = '' then s2 := 'SmallInt';
      if not(i in [3]) then begin
        AddSimpleInt(Format(s, ['SimpleArg_Small1']),    -192, s2);
        AddSimpleInt(Format(s, ['SimpleVArg_Small1']),   -191, s2);
      end;
      AddSimpleInt(Format(s, ['Local_Small1']), 391, s2);
      AddSimpleInt(Format(s, ['SimpleGlob_Small1 ']), 291, s2);
      AddSimpleInt(Format(s, ['SimpleGlob_Small2']), 0, s2);
      if not(i in [3]) then begin
        AddSimpleInt(Format(s, ['SimpleGlob_Small3']), -1, s2);
      end;
      AddSimpleInt(Format(s, ['SimpleGlob_Small4']), high(SmallInt), s2);
      if not(i in [3]) then begin
        AddSimpleInt(Format(s, ['SimpleGlob_Small5']), low(SmallInt), s2);
      end;

      s2 := s2def;
      if s2 = '' then s2 := M_Int;
      if not(i in [3]) then begin
        AddSimpleInt(Format(s, ['SimpleArg_Int1']),    -1902, s2);
        AddSimpleInt(Format(s, ['SimpleVArg_Int1']),   -1901, s2);
      end;
      AddSimpleInt(Format(s, ['Local_Int1']),  3901, s2);
      AddSimpleInt(Format(s, ['SimpleGlob_Int1']),   2901, s2);
      AddSimpleInt(Format(s, ['SimpleGlob_Int2']),   0, s2);
      if not(i in [3]) then begin
        AddSimpleInt(Format(s, ['SimpleGlob_Int3']),   -1, s2);
      end;
      AddSimpleInt(Format(s, ['SimpleGlob_Int4']),   2147483647, s2);
      if not(i in [3]) then begin
        AddSimpleInt(Format(s, ['SimpleGlob_Int5']),   -2147483648, s2);
      end;

      s2 := s2def;
      if s2 = '' then s2 := 'Int64';
      if not(i in [3]) then begin
        AddSimpleInt(Format(s, ['SimpleArg_QInt1']),    -190000000000002, s2);
        AddSimpleInt(Format(s, ['SimpleVArg_QInt1']),   -190000000000001, s2);
      end;
      AddSimpleInt(Format(s, ['Local_QInt1']), 39001, s2);
      AddSimpleInt(Format(s, ['SimpleGlob_QInt1 ']), 29001, s2);
      AddSimpleInt(Format(s, ['SimpleGlob_QInt2']), 0, s2);
      if not(i in [3]) then begin
        AddSimpleInt(Format(s, ['SimpleGlob_QInt3']), -1, s2);
      end;
      AddSimpleInt(Format(s, ['SimpleGlob_QInt4']), high(Int64), s2);
      if not(i in [3]) then begin
        AddSimpleInt(Format(s, ['SimpleGlob_QInt5']), low(Int64), s2);
      end;

      s2 := s2def;
      if s2 = '' then s2 := 'Byte';
      AddSimpleUInt(Format(s, ['Local_Byte1']), 59, s2);
      AddSimpleUInt(Format(s, ['SimpleGlob_Byte1 ']), 49, s2);
      AddSimpleUInt(Format(s, ['SimpleGlob_Byte2']), $7f, s2);
      AddSimpleUInt(Format(s, ['SimpleGlob_Byte3']), $80, s2);
      AddSimpleUInt(Format(s, ['SimpleGlob_Byte4']), high(Byte), s2);
      AddSimpleUInt(Format(s, ['SimpleGlob_Byte5']), low(Byte), s2);

      s2 := s2def;
      if s2 = '' then s2 := 'Word';
      AddSimpleUInt(Format(s, ['Local_Word1']), 591, s2);
      AddSimpleUInt(Format(s, ['SimpleGlob_Word1 ']), 491, s2);
      AddSimpleUInt(Format(s, ['SimpleGlob_Word2']), $7fff, s2);
      AddSimpleUInt(Format(s, ['SimpleGlob_Word3']), $8000, s2);
      AddSimpleUInt(Format(s, ['SimpleGlob_Word4']), high(Word), s2);
      AddSimpleUInt(Format(s, ['SimpleGlob_Word5']), low(Word), s2);

      s2 := s2def;
      if s2 = '' then s2 := 'LongWord';
      AddSimpleUInt(Format(s, ['Local_DWord1']), 5901, s2);
      AddSimpleUInt(Format(s, ['SimpleGlob_DWord1 ']), 4901, s2);
      AddSimpleUInt(Format(s, ['SimpleGlob_DWord2']), $7fffffff, s2);
      AddSimpleUInt(Format(s, ['SimpleGlob_DWord3']), $80000000, s2);
      AddSimpleUInt(Format(s, ['SimpleGlob_DWord4']), high(LongWord), s2);
      AddSimpleUInt(Format(s, ['SimpleGlob_DWord5']), low(LongWord), s2);

      s2 := s2def;
      if s2 = '' then s2 := 'QWord';
      if not(i in [2]) then begin
        AddSimpleUInt(Format(s, ['Local_QWord1']), 59001, s2);
        AddSimpleUInt(Format(s, ['SimpleGlob_QWord1 ']), 49001, s2);
        AddSimpleUInt(Format(s, ['SimpleGlob_QWord2']), $7fffffffffffffff, s2);
        AddSimpleUInt(Format(s, ['SimpleGlob_QWord3']), qword($8000000000000000), s2);
        AddSimpleUInt(Format(s, ['SimpleGlob_QWord4']), high(QWord), s2);
        AddSimpleUInt(Format(s, ['SimpleGlob_QWord5']), low(QWord), s2);
      end;

    end;


    s2 := 'Byte';
    s := 'Byte(%s)';
    AddSimpleInt(Format(s, ['SimpleArg_Int1']),    Byte(-1902), s2);
    AddSimpleInt(Format(s, ['SimpleVArg_Int1']),   Byte(-1901), s2);
    AddSimpleInt(Format(s, ['Local_Int1']),  Byte(3901), s2);
    AddSimpleInt(Format(s, ['SimpleGlob_Int1']),   Byte(2901), s2);
    AddSimpleInt(Format(s, ['SimpleGlob_Int2']),   Byte(0), s2);
    AddSimpleInt(Format(s, ['SimpleGlob_Int3']),   Byte(-1), s2);
    AddSimpleInt(Format(s, ['SimpleGlob_Int4']),   Byte(2147483647), s2);
    AddSimpleInt(Format(s, ['SimpleGlob_Int5']),   Byte(-2147483648), s2);


    for i := 0 to 3 do begin
      case i of
        0: s := 'SimpleArg_Class1.%s';
        1: s := 'SimpleVArg_Class1.%s';
        2: s := 'SimpleArg_Class2.%s';
        3: s := 'SimpleVArg_Class2.%s';
      end;
      j := 0;
      if i in [2,3] then j := 100;

      AddSimpleInt(Format(s, ['Field_Short1']),   j+11, 'ShortInt');
      AddSimpleInt(Format(s, ['Field_Small1']),   j+12, 'SmallInt');
      AddSimpleInt(Format(s, ['Field_Int1']),     j+13, 'LongInt');
      AddSimpleInt(Format(s, ['Field_QInt1']),    j+14, 'Int64');

      AddSimpleInt(Format(s, ['Field_Byte1']),     j+15, 'Byte');
      AddSimpleInt(Format(s, ['Field_Word1']),     j+16, 'Word');
      AddSimpleInt(Format(s, ['Field_DWord1']),    j+17, 'LongWord');
      AddSimpleInt(Format(s, ['Field_QWord1']),    j+18, 'QWord');
    end;
    {%endregion}

    s := '%s';
    AddFmtDef(Format(s, ['SimpleGlob_Single1']),   '^99\.(2|19)',  skSimple, '', [fTpMtch]);
    AddFmtDef(Format(s, ['SimpleGlob_Double1']),   '^199\.(3|29)', skSimple, '', [fTpMtch]);
    AddFmtDef(Format(s, ['SimpleGlob_Ext1']),      '^299\.(4|39)', skSimple, '', [fTpMtch]);
    AddSimpleInt(Format(s, ['SimpleGlob_Comp1']),    -2, '');

    {%region AddressOf / Var param, hidden pointer}
      //SimplePArg_Int1, SimplePVArg_Int1, SimplePLocal_Int1, SimplePGlob_Int1: PLongInt;
      r := AddFmtDef('@SimpleArg_Int1',    'replaceme', skPointer, '');
      r^.OnBeforeTest := @AdjustExpectToAddress;
      r^.UserData := pointer(ptruint(Length(FCurrentExpect^)));
      r^.UserData2 := FCurrentExpect;
      AddFmtDef('SimplePArg_Int1',    '\$[0-9A-F]', skPointer, '');

      r := AddFmtDef('@SimpleVArg_Int1',    'replaceme', skPointer, '');
      UpdResMinFpc(r, stSymAll, 020600);
      r^.OnBeforeTest := @AdjustExpectToAddress;
      r^.UserData := pointer(PtrUInt(Length(FCurrentExpect^)));
      r^.UserData2 := FCurrentExpect;
      AddFmtDef('SimplePVArg_Int1',    '\$[0-9A-F]', skPointer, '');

      r := AddFmtDef('@Local_Int1',    'replaceme', skPointer, '');
      r^.OnBeforeTest := @AdjustExpectToAddress;
      r^.UserData := pointer(PtrUInt(Length(FCurrentExpect^)));
      r^.UserData2 := FCurrentExpect;
      AddFmtDef('SimplePLocal_Int1',    '\$[0-9A-F]', skPointer, '');

      r := AddFmtDef('@SimpleGlob_Int1',    'replaceme', skPointer, '');
      r^.OnBeforeTest := @AdjustExpectToAddress;
      r^.UserData := pointer(PtrUInt(Length(FCurrentExpect^)));
      r^.UserData2 := FCurrentExpect;
      AddFmtDef('SimplePGlob_Int1',    '\$[0-9A-F]', skPointer, '');

    {%endregion}
  end;
var
  BrkIdx, i, i2, j: Integer;
  s, s2, s2def: String;
  r: PWatchExpectation;
begin
  for BrkIdx := 1 to BREAK_COUNT_TestWatchesUnitSimple do begin
    FCurrentExpect := @ExpectBreakSimple[BrkIdx];

    if BrkIdx in [1] then begin // Test1Sub
      AddExpectSimple_Test1Sub;
      continue;
    end;

    {%region  Fields / Glob / ... }
    for i := 0 to 5 do begin
      case BrkIdx of
        // Simple_NoneNested
        4, 5: if not (i in [1]) then continue; // Only Global /// TODO: test for error
        // Test2Method
        8..13:  if not (i in [0, 1, 5, 6]) then continue;
        // Class[12].Test0Method
        6..7, 14..15: if not (i in [1]) then continue; // Only Global /// TODO: test for error
        // Class0.Test0Method
        16..17: if not (i in [1]) then continue; // Only Global /// TODO: test for error
      end;

      case i of
        0: s := 'Field_';
        1: s := 'SimpleGlob_';
        2: s := 'Arg_';
        3: s := 'VArg_';
        4: s := 'Local_';
        5: s := 'self.Field_';
        //6: s := 'TSimpleClass1(self).Field_';
        //7: s := 'TSimpleClass2(self).Field_';
        //8: s := 'TSimpleClass0(self).Field_';
        // 6: passed in object / var arg object
        // unit.glob
      end;

      {%region  address of }
      //r := AddFmtDef(Format('@%sDynInt1', [s]), '\REPLACEME', skPointer, '', [fTpMtch]);
      //if i = 3 then UpdResMinFpc(r, stSymAll, 020600);
      //r^.OnBeforeTest := @AdjustArrayExpectToAddress;
      //r^.UserData := pointer(ptruint(Length(FCurrentExpect^)));
      //r^.UserData2 := FCurrentExpect;
      //AddFmtDef(Format('%sPDynInt1', [s]), '\$[0-9A-F]', skPointer, '', [fTpMtch]);

      {%endregion  address of }


      for i2 := 0 to 1 do begin
        s2 := '';
        if i2 = 1 then begin
          s := s + 'P';
          s2 := '^';
        end;

        AddSimpleInt(Format('%sShort1%1:s', [s,s2]),   11, 'ShortInt');
        AddSimpleInt(Format('%sSmall1%1:s', [s,s2]),   112, 'SmallInt');
        AddSimpleInt(Format('%sInt1%1:s', [s,s2]),     1123, 'LongInt');
        AddSimpleInt(Format('%sQInt1%1:s', [s,s2]),    11234, 'Int64');

        AddSimpleInt(Format('%sByte1%1:s', [s,s2]),    22, 'Byte');
        AddSimpleInt(Format('%sWord1%1:s', [s,s2]),    223, 'Word');
        AddSimpleInt(Format('%sDWord1%1:s', [s,s2]),   2234, 'LongWord');
        AddSimpleInt(Format('%sQWord1%1:s', [s,s2]),   22345, 'QWord');

        AddFmtDef(Format('%sSingle1%1:s', [s,s2]),  '^0.(5|49)',  skSimple, '', [fTpMtch]);
        AddFmtDef(Format('%sDouble1%1:s', [s,s2]),  '^0.2(5|49)',  skSimple, '', [fTpMtch]);
        AddFmtDef(Format('%sExt1%1:s', [s,s2]),     '^0.7(5|49)',  skSimple, '', [fTpMtch]);
        AddFmtDef(Format('%sComp1%1:s', [s,s2]),    '^-9',  skSimple, '', [fTpMtch]);

        AddFmtDef(Format('%sBool1%1:s', [s,s2]),     '^True', skSimple, '', [fTpMtch]);
        AddFmtDef(Format('%sBool2%1:s', [s,s2]),     '^False', skSimple, '', [fTpMtch]);
        AddFmtDef(Format('%sEnum1%1:s', [s,s2]),    '^eval2', skSimple, '', [fTpMtch]);
        AddFmtDef(Format('%sEnum2%1:s', [s,s2]),    '^eval1', skSimple, '', [fTpMtch]);
        r := AddFmtDef(Format('%sSet1%1:s', [s,s2]),     '^\[eval2\]', skSimple, '', [fTpMtch]);
        UpdExpRes(r, stDwarf, '', skSimple); // no sets in dwarf2
        r := AddFmtDef(Format('%sSet2%1:s', [s,s2]),     '^\[\]', skSimple, '', [fTpMtch]);
        UpdExpRes(r, stDwarf, '', skSimple); // no sets in dwarf2

      end; // i2

    end; // i
    {%endregion  Fields }


  end;
end;

procedure TTestWatches.AddExpectArray_1;
var
  i, i2: Integer;
  s,s2: String;
  r: PWatchExpectation;
begin
  FCurrentExpect := @ExpectBreakArray_1;


  {%region  Fields / Glob / ... }
  for i := 0 to 5 do begin
    case i of
      0: s := 'Field_';
      1: s := 'ArrayGlob_';
      2: s := 'Arg_';
      3: s := 'VArg_';
      4: s := 'Local_';
      5: s := 'self.Field_';
      // 6: passed in object / var arg object
    end;

    {%region  address of }
    r := AddFmtDef(Format('@%sDynInt1', [s]), '\REPLACEME', skPointer, '', [fTpMtch]);
    if i = 3 then UpdResMinFpc(r, stSymAll, 020600);
    r^.OnBeforeTest := @AdjustArrayExpectToAddress;
    r^.UserData := pointer(ptruint(Length(FCurrentExpect^)));
    r^.UserData2 := FCurrentExpect;
    AddFmtDef(Format('%sPDynInt1', [s]), '\$[0-9A-F]', skPointer, '', [fTpMtch]);

    r := AddFmtDef(Format('@%sStatInt1', [s]), '\REPLACEME', skPointer, '', [fTpMtch]);
    if i = 3 then UpdResMinFpc(r, stSymAll, 020600);
    r^.OnBeforeTest := @AdjustArrayExpectToAddress;
    r^.UserData := pointer(ptruint(Length(FCurrentExpect^)));
    r^.UserData2 := FCurrentExpect;
    AddFmtDef(Format('%sPStatInt1', [s]), '\$[0-9A-F]', skPointer, '', [fTpMtch]);
    {%endregion  address of }


    for i2 := 0 to 1 do begin
      s2 := '';
      if i2 = 1 then begin
        s := s + 'P';
        s2 := '^';
      end;

if not (i in [2,3]) then
      AddFmtDef(Format('%sDynAInt1%1:s', [s,s2]), '^[\(L].*?100, 101, 102', skArray, '', [fTpMtch]);
if (not (i in [2,3])) or (i2=0) then begin // open array / not valid, pointer is pointer to dyn array
      AddSimpleInt(Format('%sDynAInt1%1:s[0]', [s,s2]),    100, M_Int);
      AddSimpleInt(Format('%sDynAInt1%1:s[1]', [s,s2]),    101, M_Int);
      AddFmtDef(Format('%sDynAInt1%1:s[0][0]', [s,s2]), 'Error', skNone, '', [fTpMtch, IgnKind, fTExpectError]); // ERROR
end;


      AddFmtDef(Format('%sStatAInt1%1:s', [s,s2]), '^[\(L].*?6600, 6601, 6602',
                skArray, '', [fTpMtch]);
      AddSimpleInt(Format('%sStatAInt1%1:s[4]', [s,s2]),    6600, M_Int);
      AddSimpleInt(Format('%sStatAInt1%1:s[9]', [s,s2]),    6699, M_Int);
      AddFmtDef(Format('%sStatAInt1%1:s[3]', [s,s2]), '', skSimple, M_Int, [fTpMtch]); // Just do not crash
      AddFmtDef(Format('%sStatAInt1%1:s[10]', [s,s2]), '', skSimple, M_Int, [fTpMtch]); // Just do not crash
      AddFmtDef(Format('%sStatAInt1%1:s[-1]', [s,s2]), '', skSimple, M_Int, [fTpMtch]); // Just do not crash


      AddFmtDef(Format('%sStatAInt2%1:s', [s,s2]), '^[\(L].*?3300, 3301, 3302',
                skArray, '', [fTpMtch]);
      AddSimpleInt(Format('%sStatAInt2%1:s[-4]', [s,s2]),    3300, M_Int);
      AddSimpleInt(Format('%sStatAInt2%1:s[0]', [s,s2]),    3304, M_Int);


      AddFmtDef(Format('%sDynInt1%1:s', [s,s2]), '^[\(L].*?5511, 5512, 5513, 5514, -5511',
                skArray, '', [fTpMtch]);
      AddSimpleInt(Format('%sDynInt1%1:s[0]', [s,s2]),    5511, M_Int);
      AddSimpleInt(Format('%sDynInt1%1:s[19]', [s,s2]),    5500, M_Int);
      // typecast for dynarray
      r := AddFmtDef(Format('TArrayDynInt(%sDynInt1%1:s)', [s,s2]), '^[\(L].*?5511, 5512, 5513, 5514, -5511',
                skArray, '', [fTpMtch]);
      if i in [3] then UpdResMinFpc(r, stSymAll, 020600);
      r := AddSimpleInt(Format('TArrayDynInt(%sDynInt1%1:s)[0]', [s,s2]),    5511, M_Int);
      if i in [3] then UpdResMinFpc(r, stSymAll, 020600);
      // typecast for dynarray
      r := AddFmtDef(Format('TArrayDynInt(Pointer(%sDynInt1%1:s))', [s,s2]), '^[\(L].*?5511, 5512, 5513, 5514, -5511',
                skArray, '', [fTpMtch]);
      if i in [3] then UpdResMinFpc(r, stSymAll, 020600);
      r := AddSimpleInt(Format('TArrayDynInt(Pointer(%sDynInt1%1:s))[0]', [s,s2]),    5511, M_Int);
      if i in [3] then UpdResMinFpc(r, stSymAll, 020600);
      // typecast for dynarray
      r := AddFmtDef(Format('TArrayDynInt(QWord(%sDynInt1%1:s))', [s,s2]), '^[\(L].*?5511, 5512, 5513, 5514, -5511',
                skArray, '', [fTpMtch]);
      if i in [3] then UpdResMinFpc(r, stSymAll, 020600);
      r := AddSimpleInt(Format('TArrayDynInt(QWord(%sDynInt1%1:s))[0]', [s,s2]),    5511, M_Int);
      if i in [3] then UpdResMinFpc(r, stSymAll, 020600);
      // typecast for dynarray
      if (i2 = 0) and (i <> 3) then begin
        r := AddFmtDef(Format('TArrayDynInt(REPLACEME)', [s,s2]), '^[\(L].*?5511, 5512, 5513, 5514, -5511',
                  skArray, '', [fTpMtch]);
        //      if i in [3] then UpdResMinFpc(r, stSymAll, 020600);
        r^.OnBeforeTest := @AdjustArrayExpectToAddress;
        r^.UserData := pointer(ptruint(Length(FCurrentExpect^)));
        r^.UserData2 := FCurrentExpect;
        AddFmtDef(Format('Pointer(%sDynInt1%1:s)', [s,s2]), '\$[0-9A-F]', skPointer, '', [fTpMtch]);
        r := AddSimpleInt(Format('TArrayDynInt(REPLACEME)[0]', [s,s2]),    5511, M_Int);
        //if i in [3] then UpdResMinFpc(r, stSymAll, 020600);
        r^.OnBeforeTest := @AdjustArrayExpectToAddress;
        r^.UserData := pointer(ptruint(Length(FCurrentExpect^)));
        r^.UserData2 := FCurrentExpect;
        AddFmtDef(Format('Pointer(%sDynInt1%1:s)', [s,s2]), '\$[0-9A-F]', skPointer, '', [fTpMtch]);
      end;


      AddFmtDef(Format('%sDynInt2%1:s', [s,s2]), '^nil', skArray, '', [fTpMtch]);
      r := AddFmtDef(Format('TArrayDynInt(%sDynInt2%1:s)', [s,s2]), '^nil', skArray, '', [fTpMtch]);
      if i in [3] then UpdResMinFpc(r, stSymAll, 020600);
      r := AddSimpleInt(Format('QWord(%sDynInt2%1:s)', [s,s2]),   0, 'QWord');
      if i in [3] then UpdResMinFpc(r, stSymAll, 020600);


      AddFmtDef(Format('%sDynClass1%1:s', [s,s2]), '^[\(L].*?'+
        '\(.*?Field_INT1 = 98700;.*?Field_INT2 = 98701;.*?Field_DYNAINT1 = \(9900, 9901\);.*?\), ' +
        '\(.*?Field_INT1 = 88700;.*?Field_INT2 = 88701;.*?Field_DYNAINT1 = \(8900, 8901\);.*?\), ' +
        '\(.*?Field_INT1 = 78700;.*?Field_INT2 = 78701;.*?Field_DYNAINT1 = \(7900, 7901, 7902\);.*?\)',
      skArray, '', [fTpMtch]);
      AddFmtDef(Format('%sDynClass1%1:s[0]', [s,s2]),
        '\(.*?Field_INT1 = 98700;.*?Field_INT2 = 98701;.*?Field_DYNAINT1 = \(9900, 9901\);.*?\), ',
      skClass, 'TArrayClass1', [fTpMtch]);
      AddFmtDef(Format('%sDynClass1%1:s[1]', [s,s2]),
        '\(.*?Field_INT1 = 88700;.*?Field_INT2 = 88701;.*?Field_DYNAINT1 = \(8900, 8901\);.*?\), ',
      skClass, 'TArrayClass1', [fTpMtch]);
      AddSimpleInt(Format('%sDynClass1%1:s[0].Field_INT1', [s,s2]),  98700, M_Int);


      AddFmtDef(Format('%sDynRec1%1:s', [s,s2]), '^[\(L].*?'+
        '\(.*?FieldINT1 = 200;.*?FieldINT2 = 201;.*?\), ' +
        '\(.*?FieldINT1 = 210;.*?FieldINT2 = 211;.*?\), ' +
        '\(.*?FieldINT1 = 220;.*?FieldINT2 = 221;.*?\)',
      skArray, '', [fTpMtch]);

if not (i in [2,3]) then // open array / TODO
      AddFmtDef(Format('%sDynRec2%1:s', [s,s2]), '^[\(L].*?'+
        '\(.*?FieldByte1 = 200;.*?FieldByte2 = 201;.*?\), ' +
        '\(.*?FieldByte1 = 210;.*?FieldByte2 = 211;.*?\), ' +
        '\(.*?FieldByte1 = 220;.*?FieldByte2 = 221;.*?\)',
      skArray, '', [fTpMtch]);


      AddFmtDef(Format('%sDynDynInt1%1:s', [s,s2]), '^[\(L].*?'+
        '\(1000, 1001, 1002\), ' +    '\(1010, 1011, 1012\), ' +    '\(1020, 1021, 1022\)',
      skArray, '', [fTpMtch]);
      AddSimpleInt(Format('%sDynDynInt1%1:s[0][0]', [s,s2]),    1000, M_Int);
      AddSimpleInt(Format('%sDynDynInt1%1:s[0,0]', [s,s2]),    1000, M_Int);
      AddSimpleInt(Format('%0:sDynDynInt1%1:s[%0:sDynDynInt1%1:s[3,0], %0:sDynDynInt1%1:s[3,1]]', [s,s2]),    1012, M_Int);
      AddSimpleInt(Format('%0:sDynDynInt1%1:s[%0:sDynDynInt1%1:s[3,0]][%0:sDynDynInt1%1:s[3,1]]', [s,s2]),    1012, M_Int);

      //AddFmtDef(Format('%0:sDynDynInt1%1:s[%0:sDynDynInt1%1:s[3,0].NoMember, %0:sDynDynInt1%1:s[3,1]]', [s,s2]), 'Error', skNone, '', [fTpMtch, IgnKind, fTExpectError]); // ERROR


      AddFmtDef(Format('%sDynDynClass1%1:s', [s,s2]), '^[^\(G]*?\('+ // not GDB:
        '\(\(.*?Field_INT1 = 5000;.*?\), \(.*?Field_INT1 = 5001;.*?\)\), ' +
        '\(nil, \(.*?Field_INT1 = 5011;.*?\)\), ' +
        '\(nil, nil\)',
      skArray, '', [fTpMtch]);


      ////
      AddFmtDef(Format('%sStatStatInt1%1:s', [s,s2]), '^[\(L].*?'+
        '\(4091, 4092, 4093\), ' +    '\(4081, 4082, 4083\), ' +    '\(4071, 4072, 4073\), ',
      skArray, '', [fTpMtch]);
      AddSimpleInt(Format('%sStatStatInt1%1:s[-9,1]', [s,s2]),    4091, M_Int);
      r := AddFmtDef(Format('TArrayStatStatInt(%sStatStatInt1%1:s)', [s,s2]), '^[\(L].*?'+
        '\(4091, 4092, 4093\), ' +    '\(4081, 4082, 4083\), ' +    '\(4071, 4072, 4073\), ',
      skArray, '', [fTpMtch]);
      if i in [3] then UpdResMinFpc(r, stSymAll, 020600);
      r := AddSimpleInt(Format('TArrayStatStatInt(%sStatStatInt1%1:s)[-9,1]', [s,s2]),    4091, M_Int);
      if i in [3] then UpdResMinFpc(r, stSymAll, 020600);

    end; // i2

  end; // i
  {%endregion  Fields }

//TODO
  AddSimpleInt('PInteger(Field_DynInt1)[0]',  5511, M_Int);
  AddSimpleInt('PInteger(Field_DynInt1)[1]',  5512, M_Int);
  AddSimpleInt('PInteger(Field_DynInt1)[2]',  5513, M_Int);

  AddSimpleInt('^LongInt(Field_DynInt1)[0]',  5511, M_Int);
  AddSimpleInt('^LongInt(Field_DynInt1)[1]',  5512, M_Int);
  AddSimpleInt('^LongInt(Field_DynInt1)[2]',  5513, M_Int);

end;

procedure TTestWatches.RunTestWatches(NamePreFix: String; TestExeName, ExtraOpts: String;
  UsedUnits: array of TUsesDir);

var
  dbg: TGDBMIDebugger;
  Only: Integer;
  OnlyName, OnlyNamePart: String;

  procedure SetBreak(AFileName: String; ALineNum: Integer);
  begin
    with dbg.BreakPoints.Add(AFileName, ALineNum) do begin
      InitialEnabled := True;
      Enabled := True;
    end;
  end;

var
  i: Integer;
  st: TSymbolType;
  s: String;

begin
  TestBaseName := NamePreFix;
  if not HasTestArraysData then exit;
  Only := StrToIntDef(TestControlForm.EdOnlyWatch.Text, -1);
  OnlyNamePart := '';OnlyName := '';
  if Only < 0
  then begin
    OnlyName := TestControlForm.EdOnlyWatch.Text;
    if (OnlyName <> '') and (OnlyName[1]='*') then begin
      OnlyNamePart := copy(OnlyName, 2, length(OnlyName));
      OnlyName := '';
    end;
  end;


  try
    TestCompile(AppDir + 'TestWatchesProg.pas', TestExeName, UsedUnits, '', ExtraOpts);
  except
    on e: Exception do begin
      TestTrue('Compile error: ' + e.Message, False);
      exit;
    end;
  end;

  try
    dbg := StartGDB(AppDir, TestExeName);
    FWatches := Watches.Watches;

    for i := 1 to BREAK_COUNT_TestWatchesUnitSimple do
      SetBreak('TestWatchesUnitSimple.pas', BREAK_LINE_TestWatchesUnitSimple[i]);
    SetBreak('TestWatchesUnitArray.pas', BREAK_LINE_TestWatchesUnitArray);

    if dbg.State = dsError then
      Fail(' Failed Init');

    for i := 1 to BREAK_COUNT_TestWatchesUnitSimple do
      AddWatches(ExpectBreakSimple[i], FWatches, Only, OnlyName, OnlyNamePart);
    AddWatches(ExpectBreakArray_1,  FWatches, Only, OnlyName, OnlyNamePart);

    (* Start debugging *)
    dbg.ShowConsole := True;
    dbg.Run;


    for i := 1 to BREAK_COUNT_TestWatchesUnitSimple do begin
      if not TestTrue('State=Pause', dbg.State = dsPause) then begin
        TestTrue('Hit BREAK_LINE_TestWatchesUnitSimple_'+IntToStr(i), False);
        exit;
      end;
      AssertTrue('in simple', pos('simple', LowerCase(dbg.GetLocation.SrcFile)) > 0);
      (* Hit first breakpoint:  *)
      TestWatchList('Simple'+IntToStr(i), ExpectBreakSimple[i], dbg, Only, OnlyName, OnlyNamePart);

      dbg.Run;
    end;


    // array


   if not TestTrue('State=Pause', dbg.State = dsPause) then begin
      TestTrue('Hit BREAK_LINE_TestWatchesUnitArray', False);
      exit;
    end;
    AssertTrue('reached array', pos('array', LowerCase(dbg.GetLocation.SrcFile)) > 0);

    (* Hit 11st Array breakpoint: *)
    TestWatchList('Array1',ExpectBreakArray_1, dbg, Only, OnlyName, OnlyNamePart);

//    dbg.Run;





    dbg.Stop;
  except
    on e: Exception do begin
      TestTrue('Error: ' + e.Message, False);
      exit;
    end;
  end;
  dbg.Done;
  CleanGdb;
  dbg.Free;
end;

procedure TTestWatches.TestWatches;
var
  TestExeName: string;
begin
  if SkipTest then exit;
  if not TestControlForm.CheckListBox1.Checked[TestControlForm.CheckListBox1.Items.IndexOf('TTestWatches')] then exit;

  ClearTestErrors;

  ClearAllTestArrays;
  AddExpectSimple;
  AddExpectArray_1;

  RunTestWatches('', TestExeName,  '', []);

  AssertTestErrors;
end;

initialization

  RegisterDbgTest(TTestWatches);
  RegisterTestSelectors(['TTestWatches'
                        ]);

end.

