unit TestWatches;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils, fpcunit, testutils, testregistry, RegExpr,
  DbgIntfBaseTypes, DbgIntfDebuggerBase, TestBase, FpGdbmiDebugger, LCLProc,
  TestWatchUtils, GDBMIDebugger, FpErrorMessages, TestDbgControl, TestDbgConfig,
  TTestDbgExecuteables, TestDbgTestSuites;

const
  BREAK_COUNT_TestWatchesUnitSimple = 17;
  BL_TW1  = 413; // Test1Sub
  BL_TW2  = 418; // Simple_NoneNested
  BL_TW3  = 708; // Test1Method
  BL_TW4  = 576; // Test1Method Nested1
  BL_TW5  = 738; // Test2Method
  BL_TW6  = 719; // Test2Method Nested2a
  BL_TW7  = 729; // Test2Method Nested2b
  BL_TW8  = 725; // Test2Method Nested2c
  BL_TW9  = 429; // Test0Method
  BL_TW10 = 426; // Test0Method Nested0
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

  BREAK_LINE_TestWatchesUnitArray = 931;

type

  { TTestWatches }

  TTestWatches = class(TTestWatchesBase)
  private
    FWatches: TWatches;

    ExpectBreakSimple: array [1..BREAK_COUNT_TestWatchesUnitSimple] of TWatchExpectationArray;

    ExpectBreakArray_1: TWatchExpectationArray;

    FCurrentExpect: PWatchExpectationArray; // currently added to
    FLastAddedExp: PWatchExpectation;

    FDbgOutPut: String;
    FDbgOutPutEnable: Boolean;

    procedure DoDbgOutput(Sender: TObject; const AText: String); override;
    procedure ClearAllTestArrays;
    function  HasTestArraysData: Boolean;

    // Add to FLastAddedExp
    function Add(AnExpr:  string; AFmt: TWatchDisplayFormat; AMtch: string;
                 AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags): PWatchExpectation;
    function Add(AnExpr:  string; AFmt: TWatchDisplayFormat; AEvalFlags: TDBGEvaluateFlags; AMtch: string;
                 AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags): PWatchExpectation;
    //
    function AddFmtDef        (AnExpr: string;
                               AMtch: string; AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags=[]): PWatchExpectation;
    function AddFmtDef        (AnExpr: string; AnFormatParam: array of const;
                               AMtch: string; AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags=[]): PWatchExpectation;
    //function AddFmtDef        (AnExpr: String; AEvalFlags: TDBGEvaluateFlags;
    //                           AMtch: string; AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags=[]): PWatchExpectation;
    function AddFmtDef        (AnExpr: String; AnFormatParam: array of const; AEvalFlags: TDBGEvaluateFlags;
                               AMtch: string; AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags=[]): PWatchExpectation;

    procedure JoinExpectsForHexReplace; // Join the lat 2 additions
    procedure LastExpectsToPtrReplace(AReplacePtrExpr: String);
    function Sp2RegEx(s: String): String;

    function AddError         (AnExpr: String; AnErrorCode: Integer = 0; AMtch: string = ''): PWatchExpectation;
    function AddError         (AnExpr: String; AnFormatParam: array of const; AnErrorCode: Integer = 0; AMtch: string = ''): PWatchExpectation;
    function AddExpInt(AnExpr: string; AMtch: Int64; ATpNm: string): PWatchExpectation;
    function AddExpInt(AnExpr: string; AnFormatParam: array of const; AMtch: Int64; ATpNm: string): PWatchExpectation;
    function AddExpUInt(AnExpr: string; AMtch: QWord; ATpNm: string): PWatchExpectation;
    function AddExpUInt(AnExpr: string; AnFormatParam: array of const; AMtch: QWord; ATpNm: string): PWatchExpectation;
    function AddExpPtr(AnExpr: string): PWatchExpectation;
    function AddExpPtr(AnExpr: string; AnFormatParam: array of const): PWatchExpectation;

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

  { TTestFpErrorHandler }

  TTestFpErrorHandler = class(TFpErrorHandler)
  public
    function ErrorAsString(AnErrorCode: TFpErrorCode; AData: array of const): string; override;
  end;

implementation

var
  ControlTTestWatches: Pointer;

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

{ TTestFpErrorHandler }

function TTestFpErrorHandler.ErrorAsString(AnErrorCode: TFpErrorCode;
  AData: array of const): string;
begin
  Result := inherited ErrorAsString(AnErrorCode, AData);
  Result := 'Error ' + IntToStr(AnErrorCode) + ': ' + Result;
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
  FLastAddedExp := Result;
end;

function TTestWatches.Add(AnExpr: string; AFmt: TWatchDisplayFormat;
  AEvalFlags: TDBGEvaluateFlags; AMtch: string; AKind: TDBGSymbolKind; ATpNm: string;
  AFlgs: TWatchExpectationFlags): PWatchExpectation;
begin
  Result := AddWatchExp(FCurrentExpect^, AnExpr, AFmt, AEvalFlags, AMtch, AKind, ATpNm, AFlgs );
  FLastAddedExp := Result;
end;

function TTestWatches.AddFmtDef(AnExpr: string; AMtch: string; AKind: TDBGSymbolKind;
  ATpNm: string; AFlgs: TWatchExpectationFlags): PWatchExpectation;
begin
  Result := Add(AnExpr, wdfDefault, AMtch, AKind, ATpNm, AFlgs );
end;

function TTestWatches.AddFmtDef(AnExpr: string; AnFormatParam: array of const; AMtch: string;
  AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags): PWatchExpectation;
begin
  Result := AddFmtDef(Format(AnExpr, AnFormatParam), AMtch, AKind, ATpNm, AFlgs);
end;

//function TTestWatches.AddFmtDef(AnExpr: String; AEvalFlags: TDBGEvaluateFlags; AMtch: string;
//  AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags): PWatchExpectation;
//begin
//  Result := Add(AnExpr, wdfDefault, AEvalFlags, AMtch, AKind, ATpNm, AFlgs );
//end;

function TTestWatches.AddFmtDef(AnExpr: String; AnFormatParam: array of const;
  AEvalFlags: TDBGEvaluateFlags; AMtch: string; AKind: TDBGSymbolKind; ATpNm: string;
  AFlgs: TWatchExpectationFlags): PWatchExpectation;
begin
  Result := Add(Format(AnExpr, AnFormatParam), wdfDefault, AEvalFlags, AMtch, AKind, ATpNm, AFlgs );
end;

procedure TTestWatches.JoinExpectsForHexReplace;
var
  i: Integer;
begin
  assert(Length(FCurrentExpect^) >= 2, 'TTestWatches.JoinExpectsForHexReplace: Length(FCurrentExpect) >= 2');
  i := Length(FCurrentExpect^) - 1;

  FCurrentExpect^[i-1].OnBeforeTest := @AdjustArrayExpectToAddress;
  FCurrentExpect^[i-1].UserData := pointer(ptruint(i));
  FCurrentExpect^[i-1].UserData2 := FCurrentExpect;
end;

procedure TTestWatches.LastExpectsToPtrReplace(AReplacePtrExpr: String);
begin
  AddExpPtr(AReplacePtrExpr);
  JoinExpectsForHexReplace;
end;

function TTestWatches.Sp2RegEx(s: String): String;
begin
  Result := AnsiReplaceText(s, ' ', '[ \r\n]');
end;

function TTestWatches.AddError(AnExpr: String; AnErrorCode: Integer;
  AMtch: string): PWatchExpectation;
var
  s: String;
begin
  if AnErrorCode > 0 then
    s := 'Error '+IntToStr(AnErrorCode) + ': ' + AMtch
  else
    s := 'Error ' + AMtch;
  AddFmtDef(AnExpr, s, skNone, '', [fTpMtch, IgnKind, fTExpectError]); // ERROR
end;

function TTestWatches.AddError(AnExpr: String; AnFormatParam: array of const;
  AnErrorCode: Integer; AMtch: string): PWatchExpectation;
begin
  Result := AddError(Format(AnExpr, AnFormatParam), AnErrorCode, AMtch);
end;

function TTestWatches.AddExpInt(AnExpr: string; AMtch: Int64;
  ATpNm: string): PWatchExpectation;
begin
  Result := AddFmtDef(AnExpr, '^'+IntToStr(AMtch), skSimple, ATpNm, [fTpMtch]);
end;

function TTestWatches.AddExpInt(AnExpr: string; AnFormatParam: array of const; AMtch: Int64;
  ATpNm: string): PWatchExpectation;
begin
  Result := AddExpInt(Format(AnExpr, AnFormatParam), AMtch, ATpNm);
end;

function TTestWatches.AddExpUInt(AnExpr: string; AMtch: QWord;
  ATpNm: string): PWatchExpectation;
begin
  Result := AddFmtDef(AnExpr, '^'+IntToStr(AMtch), skSimple, ATpNm, [fTpMtch]);
end;

function TTestWatches.AddExpUInt(AnExpr: string; AnFormatParam: array of const; AMtch: QWord;
  ATpNm: string): PWatchExpectation;
begin
  Result := AddExpUInt(Format(AnExpr, AnFormatParam), AMtch, ATpNm);
end;

function TTestWatches.AddExpPtr(AnExpr: string): PWatchExpectation;
begin
  Result := AddFmtDef(AnExpr, '\$[0-9A-F]', skPointer, '', [fTpMtch]);
end;

function TTestWatches.AddExpPtr(AnExpr: string;
  AnFormatParam: array of const): PWatchExpectation;
begin
  Result := AddExpPtr(Format(AnExpr, AnFormatParam));
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
        AddExpInt(s, ['SimpleArg_Short1'],   -92, s2);
        AddExpInt(s, ['SimpleVArg_Short1'],  -91, s2);
      end else begin
        AddExpUInt(s, ['SimpleArg_Short1'],   QWord(-92), s2);
        AddExpUInt(s, ['SimpleVArg_Short1'],  QWord(-91), s2);
      end;
      AddExpInt(s, ['Local_Short1'], 39, s2);
      AddExpInt(s, ['SimpleGlob_Short1 '], 29, s2);
      AddExpInt(s, ['SimpleGlob_Short2'],  0, s2);
      if not(i in [3]) then begin
        AddExpInt(s, ['SimpleGlob_Short3'],  -1, s2);
      end;
      AddExpInt(s, ['SimpleGlob_Short4'],  high(ShortInt), s2);
      if not(i in [3]) then begin
        AddExpInt(s, ['SimpleGlob_Short5'],  low(ShortInt), s2);
      end;

      s2 := s2def;
      if s2 = '' then s2 := 'SmallInt';
      if not(i in [3]) then begin
        AddExpInt(s, ['SimpleArg_Small1'],    -192, s2);
        AddExpInt(s, ['SimpleVArg_Small1'],   -191, s2);
      end;
      AddExpInt(s, ['Local_Small1'], 391, s2);
      AddExpInt(s, ['SimpleGlob_Small1 '], 291, s2);
      AddExpInt(s, ['SimpleGlob_Small2'], 0, s2);
      if not(i in [3]) then begin
        AddExpInt(s, ['SimpleGlob_Small3'], -1, s2);
      end;
      AddExpInt(s, ['SimpleGlob_Small4'], high(SmallInt), s2);
      if not(i in [3]) then begin
        AddExpInt(s, ['SimpleGlob_Small5'], low(SmallInt), s2);
      end;

      s2 := s2def;
      if s2 = '' then s2 := M_Int;
      if not(i in [3]) then begin
        AddExpInt(s, ['SimpleArg_Int1'],    -1902, s2);
        AddExpInt(s, ['SimpleVArg_Int1'],   -1901, s2);
      end;
      AddExpInt(s, ['Local_Int1'],  3901, s2);
      AddExpInt(s, ['SimpleGlob_Int1'],   2901, s2);
      AddExpInt(s, ['SimpleGlob_Int2'],   0, s2);
      if not(i in [3]) then begin
        AddExpInt(s, ['SimpleGlob_Int3'],   -1, s2);
      end;
      AddExpInt(s, ['SimpleGlob_Int4'],   2147483647, s2);
      if not(i in [3]) then begin
        AddExpInt(s, ['SimpleGlob_Int5'],   -2147483648, s2);
      end;

      s2 := s2def;
      if s2 = '' then s2 := 'Int64';
      if not(i in [3]) then begin
        AddExpInt(s, ['SimpleArg_QInt1'],    -190000000000002, s2);
        AddExpInt(s, ['SimpleVArg_QInt1'],   -190000000000001, s2);
      end;
      AddExpInt(s, ['Local_QInt1'], 39001, s2);
      AddExpInt(s, ['SimpleGlob_QInt1 '], 29001, s2);
      AddExpInt(s, ['SimpleGlob_QInt2'], 0, s2);
      if not(i in [3]) then begin
        AddExpInt(s, ['SimpleGlob_QInt3'], -1, s2);
      end;
      AddExpInt(s, ['SimpleGlob_QInt4'], high(Int64), s2);
      if not(i in [3]) then begin
        AddExpInt(s, ['SimpleGlob_QInt5'], low(Int64), s2);
      end;

      s2 := s2def;
      if s2 = '' then s2 := 'Byte';
      AddExpUInt(s, ['Local_Byte1'], 59, s2);
      AddExpUInt(s, ['SimpleGlob_Byte1 '], 49, s2);
      AddExpUInt(s, ['SimpleGlob_Byte2'], $7f, s2);
      AddExpUInt(s, ['SimpleGlob_Byte3'], $80, s2);
      AddExpUInt(s, ['SimpleGlob_Byte4'], high(Byte), s2);
      AddExpUInt(s, ['SimpleGlob_Byte5'], low(Byte), s2);

      s2 := s2def;
      if s2 = '' then s2 := 'Word';
      AddExpUInt(s, ['Local_Word1'], 591, s2);
      AddExpUInt(s, ['SimpleGlob_Word1 '], 491, s2);
      AddExpUInt(s, ['SimpleGlob_Word2'], $7fff, s2);
      AddExpUInt(s, ['SimpleGlob_Word3'], $8000, s2);
      AddExpUInt(s, ['SimpleGlob_Word4'], high(Word), s2);
      AddExpUInt(s, ['SimpleGlob_Word5'], low(Word), s2);

      s2 := s2def;
      if s2 = '' then s2 := 'LongWord';
      AddExpUInt(s, ['Local_DWord1'], 5901, s2);
      AddExpUInt(s, ['SimpleGlob_DWord1 '], 4901, s2);
      AddExpUInt(s, ['SimpleGlob_DWord2'], $7fffffff, s2);
      AddExpUInt(s, ['SimpleGlob_DWord3'], $80000000, s2);
      AddExpUInt(s, ['SimpleGlob_DWord4'], high(LongWord), s2);
      AddExpUInt(s, ['SimpleGlob_DWord5'], low(LongWord), s2);

      s2 := s2def;
      if s2 = '' then s2 := 'QWord';
      if not(i in [2]) then begin
        AddExpUInt(s, ['Local_QWord1'], 59001, s2);
        AddExpUInt(s, ['SimpleGlob_QWord1 '], 49001, s2);
        AddExpUInt(s, ['SimpleGlob_QWord2'], $7fffffffffffffff, s2);
        AddExpUInt(s, ['SimpleGlob_QWord3'], qword($8000000000000000), s2);
        AddExpUInt(s, ['SimpleGlob_QWord4'], high(QWord), s2);
        AddExpUInt(s, ['SimpleGlob_QWord5'], low(QWord), s2);
      end;

    end;


    s2 := 'Byte';
    s := 'Byte(%s)';
    AddExpInt(s, ['SimpleArg_Int1'],    Byte(-1902), s2);
    AddExpInt(s, ['SimpleVArg_Int1'],   Byte(-1901), s2);
    AddExpInt(s, ['Local_Int1'],  Byte(3901), s2);
    AddExpInt(s, ['SimpleGlob_Int1'],   Byte(2901), s2);
    AddExpInt(s, ['SimpleGlob_Int2'],   Byte(0), s2);
    AddExpInt(s, ['SimpleGlob_Int3'],   Byte(-1), s2);
    AddExpInt(s, ['SimpleGlob_Int4'],   Byte(2147483647), s2);
    AddExpInt(s, ['SimpleGlob_Int5'],   Byte(-2147483648), s2);


    for i := 0 to 3 do begin
      case i of
        0: s := 'SimpleArg_Class1.%s';
        1: s := 'SimpleVArg_Class1.%s';
        2: s := 'SimpleArg_Class2.%s';
        3: s := 'SimpleVArg_Class2.%s';
      end;
      j := 0;
      if i in [2,3] then j := 100;

      AddExpInt(s, ['Field_Short1'],   j+11, 'ShortInt');
      AddExpInt(s, ['Field_Small1'],   j+12, 'SmallInt');
      AddExpInt(s, ['Field_Int1'],     j+13, 'LongInt');
      AddExpInt(s, ['Field_QInt1'],    j+14, 'Int64');

      AddExpInt(s, ['Field_Byte1'],     j+15, 'Byte');
      AddExpInt(s, ['Field_Word1'],     j+16, 'Word');
      AddExpInt(s, ['Field_DWord1'],    j+17, 'LongWord');
      AddExpInt(s, ['Field_QWord1'],    j+18, 'QWord');
    end;
    {%endregion}

    s := '%s';
    AddFmtDef(s, ['SimpleGlob_Single1'],   '^99\.(2|19)',  skSimple, '', [fTpMtch]);
    AddFmtDef(s, ['SimpleGlob_Double1'],   '^199\.(3|29)', skSimple, '', [fTpMtch]);
    AddFmtDef(s, ['SimpleGlob_Ext1'],      '^299\.(4|39)', skSimple, '', [fTpMtch]);
    AddExpInt(s, ['SimpleGlob_Comp1'],    -2, '');

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
  s, s1, s2, s2def: String;
  r: PWatchExpectation;
  ErrCodeNotFound: TFpErrorCode;
begin
  for BrkIdx := 1 to BREAK_COUNT_TestWatchesUnitSimple do begin
    FCurrentExpect := @ExpectBreakSimple[BrkIdx];

    if BrkIdx in [1] then begin // Test1Sub
      AddExpectSimple_Test1Sub;
      continue;
    end;

    {%region  Fields / Glob / ... }
    for i := 0 to 7 do begin

      case i of
        0: s := 'Field_';
        1: s := 'SimpleGlob_';
        2: s := 'Arg_';
        3: s := 'VArg_';
        4: s := 'Local_';
        5: s := 'self.Field_';
        6: s := 'TSimpleClass1(self).Field_';
        7: s := 'TSimpleClass2(self).Field_';
        //8: s := 'TSimpleClass0(self).Field_';
        // 6: passed in object / var arg object
        // unit.glob
      end;
      if (i in [5..7]) and not(BrkIdx in [4, 5]) then
        ErrCodeNotFound := fpErrNoMemberWithName
      else
        ErrCodeNotFound := fpErrSymbolNotFound;


      if i in [0, 5] then begin
        // Some Expressions that will always fail
        s1 := s;
        s2 := '';
        for j := 0 to 9 do begin
          case j of
            0: s1 := s;
            1: s2 := '[1]';
            2: s2 := '[-1]';
            3: s2 := '[0]';
            4: s2 := '[$7fffffff]';
            5: s2 := '^';
            6: s1 := '@' + s;
            7: s2 := '()';
            8: s2 := '(1)';
            9: begin s1 := 'TSimpleClass1(' + s; s2 := ')'; end;
          end;

          AddError(Format('%sIDoNotExist%1:s', [s1,s2]), ErrCodeNotFound); // ERROR
          // procedures
          AddError(Format('%sInitFields%1:s', [s1,s2]),  ErrCodeNotFound); // ERROR
          AddError(Format('%sSimple_NoneNested%1:s', [s1,s2]),  ErrCodeNotFound); // ERROR
          // type
          AddError(Format('%sTSimpleClass1%1:s', [s1,s2]),  ErrCodeNotFound); // ERROR
          // unit
          AddError(Format('%sTestWatchesUnitSimple%1:s', [s1,s2]),  ErrCodeNotFound); // ERROR
        end;
      end;

      if ( (BrkIdx in [6, 7]) and (i in [7]) ) or          // Class1.Test0Method
         ( (BrkIdx in [16,17]) and (i in [6,7]) )          // Class0.Test0Method
      then
        continue; // typecast to unrelated class. May fail or succeed
        // TODO check there is no crash

      if
        ( (BrkIdx in [4, 5]) and             // Class1:Simple_NoneNested
          ( not (i in [1]) ) ) or
        ( (BrkIdx in [6..7]) and             // Class1.Test0Method
          ( not (i in [1, 6]) ) ) or
        ( (BrkIdx in [8..13]) and            // Class2.Test2Method
          ( not (i in [0..1, 5..8]) ) ) or
        ( (BrkIdx in [14..15]) and           // Class2.Test0Method
          ( not (i in [1, 6..7]) ) ) or
        ( (BrkIdx in [16..17]) and           // Class0.Test0Method
          ( not (i in [1]) ) )
      then begin
        // Not Found
        for i2 := 0 to 1 do begin
          s2 := '';
          if i2 = 1 then begin
            s := s + 'P';
            s2 := '^';
          end;

          AddError(Format('%sShort1%1:s', [s,s2]), ErrCodeNotFound); // ERROR
          AddError(Format('%sInt1%1:s',   [s,s2]), ErrCodeNotFound); // ERROR
        end;

        continue;
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

        AddExpInt(Format('%sShort1%1:s', [s,s2]),   11, 'ShortInt');
        AddExpInt(Format('%sSmall1%1:s', [s,s2]),   112, 'SmallInt');
        AddExpInt(Format('%sInt1%1:s', [s,s2]),     1123, 'LongInt');
        AddExpInt(Format('%sQInt1%1:s', [s,s2]),    11234, 'Int64');

        AddExpInt(Format('%sByte1%1:s', [s,s2]),    22, 'Byte');
        AddExpInt(Format('%sWord1%1:s', [s,s2]),    223, 'Word');
        AddExpInt(Format('%sDWord1%1:s', [s,s2]),   2234, 'LongWord');
        AddExpInt(Format('%sQWord1%1:s', [s,s2]),   22345, 'QWord');

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

        AddError(Format('%sByte1%1:s^', [s,s2]), fpErrCannotDereferenceType); // ERROR
    end; // i2

    end; // i
    {%endregion  Fields }


  end;
end;

procedure TTestWatches.AddExpectArray_1;
var
  i, i2: Integer;
  s, s2: String;
  SkipParamArgs: Boolean;
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
    AddFmtDef('@%sDynInt1', [s], '\REPLACEME', skPointer, '', [fTpMtch]);
    if i = 3 then UpdResMinFpc(FLastAddedExp, stSymAll, 020600);
    LastExpectsToPtrReplace(Format('%sPDynInt1', [s]));

    AddFmtDef('@%sStatInt1', [s], '\REPLACEME', skPointer, '', [fTpMtch]);
    if i = 3 then UpdResMinFpc(FLastAddedExp, stSymAll, 020600);
    LastExpectsToPtrReplace(Format('%sPStatInt1', [s]));
    {%endregion  address of }


    for i2 := 0 to 1 do begin
      s2 := '';
      if i2 = 1 then begin
        s := s + 'P';
        s2 := '^';
      end;

      SkipParamArgs := (i in [2,3]);

      // Test: Simple Array of int
      if not SkipParamArgs then begin
        AddFmtDef('%sDynAInt1%1:s', [s,s2], '^[\(L].*?100, 101, 102, 103, 104\)', skArray, '', [fTpMtch]);
        // RepeatCount
        AddFmtDef('%sDynAInt1%1:s', [s,s2], '^[\(L].*?100, 101, 102, 103, 104,', skArray, '', [fTpMtch]);
        FLastAddedExp^.RepeatCount := 6;
      end;
      if (not SkipParamArgs) or (i2=0) then begin // open array / not valid, pointer is pointer to dyn array
        AddExpInt('%sDynAInt1%1:s[0]', [s,s2],    100, M_Int);
        AddExpInt('%sDynAInt1%1:s[1]', [s,s2],    101, M_Int);
        AddError ('%sDynAInt1%1:s[0][0]', [s,s2], fpErrTypeHasNoIndex); // ERROR
      end;


      AddFmtDef('%sStatAInt1%1:s', [s,s2], '^[\(L].*?6600, 6601, 6602, 6603, 6604, 6699\)', skArray, '', [fTpMtch]);
      // RepeatCount
      AddFmtDef('%sStatAInt1%1:s', [s,s2], '^[\(L].*?6600, 6601, 6602, 6603, 6604, 6699,', skArray, '', [fTpMtch]);
      FLastAddedExp^.RepeatCount := 7;
      AddExpInt('%sStatAInt1%1:s[4]', [s,s2],    6600, M_Int);
      AddExpInt('%sStatAInt1%1:s[9]', [s,s2],    6699, M_Int);
      AddFmtDef('%sStatAInt1%1:s[3]', [s,s2], '', skSimple, M_Int, [fTpMtch]); // Just do not crash
      AddFmtDef('%sStatAInt1%1:s[10]', [s,s2], '', skSimple, M_Int, [fTpMtch]); // Just do not crash
      AddFmtDef('%sStatAInt1%1:s[-1]', [s,s2], '', skSimple, M_Int, [fTpMtch]); // Just do not crash
      AddError ('%sStatAInt1%1:s[0][0]', [s,s2], fpErrTypeHasNoIndex); // ERROR


      AddFmtDef('%sStatAInt2%1:s', [s,s2], '^[\(L].*?3300, 3301, 3302', skArray, '', [fTpMtch]);
      AddExpInt('%sStatAInt2%1:s[-4]', [s,s2],    3300, M_Int);
      AddExpInt('%sStatAInt2%1:s[0]', [s,s2],    3304, M_Int);


      AddFmtDef('%sDynInt1%1:s', [s,s2], '^[\(L].*?5511, 5512, 5513, 5514, -5511', skArray, '', [fTpMtch]);
      AddExpInt('%sDynInt1%1:s[0]', [s,s2],    5511, M_Int);
      AddExpInt('%sDynInt1%1:s[19]', [s,s2],    5500, M_Int);

      // Test: wdfMemDump
//TODO: DynArray.Size / check for end after last element.
      if not SkipParamArgs then begin
        // 5511 = $1587 / -5511=$FFFFEA79 / 5500=$157c
        AddFmtDef('%sDynInt1%1:s', [s,s2], Sp2RegEx('^\$?[0-9A-F]+: *(00 00 )?((15 87)|(87 15)) +00 00 +([0-9A-F][0-9A-F] +)*00 ((15 7C)|(7C 15))'), skArray, '', [fTpMtch]);
        FLastAddedExp^.DspFormat := wdfMemDump;
        AddFmtDef('%sDynInt1%1:s[1]', [s,s2], Sp2RegEx('^\$?[0-9A-F]+: *((00 00 15 88)|(88 15 00 00)) *$'), skArray, '', [fTpMtch]);
        FLastAddedExp^.DspFormat := wdfMemDump;
        AddFmtDef('%sDynInt1%1:s[1]', [s,s2], Sp2RegEx('^\$?[0-9A-F]+: *((00 00 15 88)|(88 15 00 00)) +[0-9A-F][0-9A-F]'), skArray, '', [fTpMtch]);
        FLastAddedExp^.DspFormat := wdfMemDump;
        FLastAddedExp^.RepeatCount := 5;
      end;

      // 6600 = $19C8 / 6699=$1A2B
      AddFmtDef('%sStatAInt1%1:s', [s,s2], Sp2RegEx('^\$?[0-9A-F]+: *(00 00 )?((19 C8)|(C8 19)) +00 00 +([0-9A-F][0-9A-F] +)*00 ((1A 2B)|(2B 1A))'), skArray, '', [fTpMtch]);
      FLastAddedExp^.DspFormat := wdfMemDump;
      AddFmtDef('%sStatAInt1%1:s[5]', [s,s2], Sp2RegEx('^\$?[0-9A-F]+: *((00 00 19 C9)|(C9 19 00 00)) *$'), skArray, '', [fTpMtch]);
      FLastAddedExp^.DspFormat := wdfMemDump;
      AddFmtDef('%sStatAInt1%1:s[5]', [s,s2], Sp2RegEx('^\$?[0-9A-F]+: *((00 00 19 C9)|(C9 19 00 00)) +[0-9A-F][0-9A-F]'), skArray, '', [fTpMtch]);
      FLastAddedExp^.DspFormat := wdfMemDump;
      FLastAddedExp^.RepeatCount := 5;

      // Test: typecast dynarray to dynarray
      AddFmtDef('TArrayDynInt(%sDynInt1%1:s)', [s,s2], '^[\(L].*?5511, 5512, 5513, 5514, -5511', skArray, '', [fTpMtch]);
      if i in [3] then UpdResMinFpc(FLastAddedExp, stSymAll, 020600);
      AddExpInt('TArrayDynInt(%sDynInt1%1:s)[0]', [s,s2],    5511, M_Int);
      if i in [3] then UpdResMinFpc(FLastAddedExp, stSymAll, 020600);
      // typecast for dynarray
      AddFmtDef('TArrayDynInt(Pointer(%sDynInt1%1:s))', [s,s2], '^[\(L].*?5511, 5512, 5513, 5514, -5511',
                skArray, '', [fTpMtch]);
      if i in [3] then UpdResMinFpc(FLastAddedExp, stSymAll, 020600);
      AddExpInt('TArrayDynInt(Pointer(%sDynInt1%1:s))[0]', [s,s2],    5511, M_Int);
      if i in [3] then UpdResMinFpc(FLastAddedExp, stSymAll, 020600);
      // typecast for dynarray
      AddFmtDef('TArrayDynInt(QWord(%sDynInt1%1:s))', [s,s2], '^[\(L].*?5511, 5512, 5513, 5514, -5511',
                skArray, '', [fTpMtch]);
      if i in [3] then UpdResMinFpc(FLastAddedExp, stSymAll, 020600);
      AddExpInt('TArrayDynInt(QWord(%sDynInt1%1:s))[0]', [s,s2],    5511, M_Int);
      if i in [3] then UpdResMinFpc(FLastAddedExp, stSymAll, 020600);

      // Test: typecast Address to array
      if (i2 = 0) and (i <> 3) then begin
        AddFmtDef('TArrayDynInt(REPLACEME)', [s,s2], '^[\(L].*?5511, 5512, 5513, 5514, -5511',
                  skArray, '', [fTpMtch]);
        //      if i in [3] then UpdResMinFpc(FLastAddedExp, stSymAll, 020600);
        LastExpectsToPtrReplace(Format('Pointer(%sDynInt1%1:s)', [s,s2]));

        AddExpInt('TArrayDynInt(REPLACEME)[0]', [s,s2],    5511, M_Int);
        //if i in [3] then UpdResMinFpc(FLastAddedExp, stSymAll, 020600);
        LastExpectsToPtrReplace(Format('Pointer(%sDynInt1%1:s)', [s,s2]));

        //AddError('TArrayStatInt(REPLACEME)', [s,s2]);
        //LastExpectsToPtrReplace(Format('Pointer(@%sStatInt1%1:s[-2])', [s,s2]));
        //AddError('TArrayStatInt(REPLACEME)[0]', [s,s2]);
        //LastExpectsToPtrReplace(Format('Pointer(@%sStatInt1%1:s[-2])', [s,s2]));
      end;


      // Test: Empty DynArray = nil
      AddFmtDef('%sDynInt2%1:s', [s,s2], '^nil', skArray, '', [fTpMtch]);
      AddFmtDef('TArrayDynInt(%sDynInt2%1:s)', [s,s2], '^nil', skArray, '', [fTpMtch]);
      if i in [3] then UpdResMinFpc(FLastAddedExp, stSymAll, 020600);
      AddExpInt('QWord(%sDynInt2%1:s)', [s,s2],   0, 'QWord');
      if i in [3] then UpdResMinFpc(FLastAddedExp, stSymAll, 020600);


      // Test: Array of structure // nested array
      AddFmtDef('%sDynClass1%1:s', [s,s2], '^[\(L].*?'+
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
      AddExpInt('%sDynClass1%1:s[0].Field_INT1', [s,s2],  98700, M_Int);


      AddFmtDef('%sDynRec1%1:s', [s,s2], '^[\(L].*?'+
        '\(.*?FieldINT1 = 200;.*?FieldINT2 = 201;.*?\), ' +
        '\(.*?FieldINT1 = 210;.*?FieldINT2 = 211;.*?\), ' +
        '\(.*?FieldINT1 = 220;.*?FieldINT2 = 221;.*?\)',
      skArray, '', [fTpMtch]);

      if not SkipParamArgs then // open array / TODO
        AddFmtDef('%sDynRec2%1:s', [s,s2], '^[\(L].*?'+
          '\(.*?FieldByte1 = 200;.*?FieldByte2 = 201;.*?\), ' +
          '\(.*?FieldByte1 = 210;.*?FieldByte2 = 211;.*?\), ' +
          '\(.*?FieldByte1 = 220;.*?FieldByte2 = 221;.*?\)',
        skArray, '', [fTpMtch]);

      // Test: Nested Array
      // 1st nested has diff len
      AddFmtDef('%sDynDynInt1%1:s', [s,s2], '^[\(L].*?'+
        '\(1000, 1001\), ' +    '\(1010, 1011, 1012\), ' +    '\(1020, 1021, 1022\)',
      skArray, '', [fTpMtch]);
      AddExpInt('%sDynDynInt1%1:s[0][0]', [s,s2],    1000, M_Int);
      AddExpInt('%sDynDynInt1%1:s[0,0]', [s,s2],    1000, M_Int);
      AddExpInt('%0:sDynDynInt1%1:s[%0:sDynDynInt1%1:s[3,0], %0:sDynDynInt1%1:s[3,1]]', [s,s2],    1012, M_Int);
      AddExpInt('%0:sDynDynInt1%1:s[%0:sDynDynInt1%1:s[3,0]][%0:sDynDynInt1%1:s[3,1]]', [s,s2],    1012, M_Int);

      AddError('%0:sDynDynInt1%1:s[FooNoExistFoo, %0:sDynDynInt1%1:s[3,1]]', [s,s2], fpErrSymbolNotFound); // ERROR
      AddError('%0:sDynDynInt1%1:s[%0:sDynDynInt1%1:s[3,0].NoMember, %0:sDynDynInt1%1:s[3,1]]', [s,s2], fpErrorNotAStructure); // ERROR


      AddFmtDef('%sDynDynClass1%1:s', [s,s2], '^[^\(G]*?\('+ // not GDB:
        '\(\(.*?Field_INT1 = 5000;.*?\), \(.*?Field_INT1 = 5001;.*?\)\), ' +
        '\(nil, \(.*?Field_INT1 = 5011;.*?\)\), ' +
        '\(nil, nil\)',
      skArray, '', [fTpMtch]);


      ////
      AddFmtDef('%sStatStatInt1%1:s', [s,s2], '^[\(L].*?'+
        '\(4091, 4092, 4093\), ' +    '\(4081, 4082, 4083\), ' +    '\(4071, 4072, 4073\), ',
      skArray, '', [fTpMtch]);
      AddExpInt('%sStatStatInt1%1:s[-9,1]', [s,s2],    4091, M_Int);
      AddFmtDef('TArrayStatStatInt(%sStatStatInt1%1:s)', [s,s2], '^[\(L].*?'+
        '\(4091, 4092, 4093\), ' +    '\(4081, 4082, 4083\), ' +    '\(4071, 4072, 4073\), ',
      skArray, '', [fTpMtch]);
      if i in [3] then UpdResMinFpc(FLastAddedExp, stSymAll, 020600);
      AddExpInt('TArrayStatStatInt(%sStatStatInt1%1:s)[-9,1]', [s,s2],    4091, M_Int);
      if i in [3] then UpdResMinFpc(FLastAddedExp, stSymAll, 020600);

      {%region  Pointer }
      AddExpInt('(%sIntPointer%1:s-2)^',    [s,s2],    5511, M_Int);
      AddExpInt('(%sIntPointer%1:s+(-2))^', [s,s2],    5511, M_Int);
      AddExpInt('(%sIntPointer%1:s+ArraySub2)^', [s,s2],    5511, M_Int);
      AddExpInt('(%sIntPointer%1:s-ArrayAdd2)^', [s,s2],    5511, M_Int);
      AddExpInt('(ArraySub2+%sIntPointer%1:s)^', [s,s2],    5511, M_Int);
      AddExpInt('(-2+%sIntPointer%1:s)^',   [s,s2],    5511, M_Int);
      AddExpInt('(-1+%sIntPointer%1:s-1)^', [s,s2],    5511, M_Int);

      AddExpInt('(%sIntPointer%1:s-1)^',    [s,s2],    5512, M_Int);
      AddExpInt('(%sIntPointer%1:s+(-1))^', [s,s2],    5512, M_Int);
      AddExpInt('%sIntPointer%1:s^',        [s,s2],    5513, M_Int);
      AddExpInt('(%sIntPointer%1:s)^',      [s,s2],    5513, M_Int);
      AddExpInt('(%sIntPointer%1:s+0)^',    [s,s2],    5513, M_Int);
      AddExpInt('(%sIntPointer%1:s-0)^',    [s,s2],    5513, M_Int);
      AddExpInt('(%sIntPointer%1:s+1)^',    [s,s2],    5514, M_Int);
      AddExpInt('(%sIntPointer%1:s+2)^',    [s,s2],   -5511, M_Int);

      AddExpInt('%sIntPointer%1:s[-2]', [s,s2],    5511, M_Int);
      AddExpInt('%sIntPointer%1:s[ArraySub2]', [s,s2],    5511, M_Int);
      AddExpInt('%sIntPointer%1:s[-ArrayAdd2]', [s,s2],    5511, M_Int);
      AddExpInt('%sIntPointer%1:s[-1]', [s,s2],    5512, M_Int);
      AddExpInt('%sIntPointer%1:s[0]',  [s,s2],    5513, M_Int);
      AddExpInt('%sIntPointer%1:s[1]',  [s,s2],    5514, M_Int);
      AddExpInt('%sIntPointer%1:s[2]',  [s,s2],   -5511, M_Int);

      AddExpInt('(PInteger(@%sIntPointer%1:s[-1])-1)^', [s,s2],    5511, M_Int);
      AddExpInt('(%sIntPointer%1:s-1)[-1]',    [s,s2],    5511, M_Int);
      AddExpInt('(PInteger(@Pointer(%sIntPointer%1:s)[-4])-1)^', [s,s2],    5511, M_Int);
      AddExpInt('(PInteger(@Pointer(%sIntPointer%1:s)[-2]-6))^', [s,s2],    5511, M_Int);

      // add 2 each for word pointer
      AddExpInt('PInteger(%sWordPointer%1:s)^',     [s,s2],    5513, M_Int);
      AddExpInt('PInteger(%sWordPointer%1:s+0)^',   [s,s2],    5513, M_Int);
      AddExpInt('PInteger(%sWordPointer%1:s+2)^',   [s,s2],    5514, M_Int);
      AddExpInt('PInteger(%sWordPointer%1:s+ArrayAdd2)^',   [s,s2],    5514, M_Int);
      AddExpInt('PInteger(%sWordPointer%1:s-ArraySub2)^',   [s,s2],    5514, M_Int);

      // add 4 each for pointer
      AddExpInt('PInteger(%sPointer%1:s)^',     [s,s2],    5513, M_Int);
      AddExpInt('PInteger(%sPointer%1:s+0)^',   [s,s2],    5513, M_Int);
      AddExpInt('PInteger(%sPointer%1:s+4)^',   [s,s2],    5514, M_Int);

      // deref generic: error
      //AddFmtDef('%sPointer%1:s^', [s,s2], 'GDB|Error', skNone, '', [fTpMtch, IgnKind, fTExpectError]); // ERROR
      //AddFmtDef('(%sPointer%1:s)^', [s,s2], 'GDB|Error', skNone, '', [fTpMtch, IgnKind, fTExpectError]); // ERROR
      //AddFmtDef('(%sPointer%1:s+0)^', [s,s2], 'GDB|Error', skNone, '', [fTpMtch, IgnKind, fTExpectError]); // ERROR
      //AddFmtDef('(%sPointer%1:s+4)^', [s,s2], 'GDB|Error', skNone, '', [fTpMtch, IgnKind, fTExpectError]); // ERROR
      //AddFmtDef('(%sPointer%1:s-4)^', [s,s2], 'GDB|Error', skNone, '', [fTpMtch, IgnKind, fTExpectError]); // ERROR


      if i2 = 0 then begin
        {%region  address of }
        AddFmtDef('%sIntPointer', [s], '\REPLACEME', skPointer, '', [fTpMtch]);
        if i = 3 then UpdResMinFpc(FLastAddedExp, stSymAll, 020600);
        LastExpectsToPtrReplace(Format('@%sDynInt1[2]', [s]));

        AddFmtDef('(%sIntPointer+0)', [s], '\REPLACEME', skPointer, '', [fTpMtch]);
        if i = 3 then UpdResMinFpc(FLastAddedExp, stSymAll, 020600);
        LastExpectsToPtrReplace(Format('@%sDynInt1[2]', [s]));

        AddFmtDef('@(%sIntPointer^)', [s], '\REPLACEME', skPointer, '', [fTpMtch]);
        if i = 3 then UpdResMinFpc(FLastAddedExp, stSymAll, 020600);
        LastExpectsToPtrReplace(Format('@%sDynInt1[2]', [s]));

        AddFmtDef('(%sIntPointer+1)', [s], '\REPLACEME', skPointer, '', [fTpMtch]);
        if i = 3 then UpdResMinFpc(FLastAddedExp, stSymAll, 020600);
        LastExpectsToPtrReplace(Format('@%sDynInt1[3]', [s]));

        AddFmtDef('(%sIntPointer-1)', [s], '\REPLACEME', skPointer, '', [fTpMtch]);
        if i = 3 then UpdResMinFpc(FLastAddedExp, stSymAll, 020600);
        LastExpectsToPtrReplace(Format('@%sDynInt1[1]', [s]));

        // Add/SUb to word pointer
        AddFmtDef('(%sWordPointer+2)', [s], '\REPLACEME', skPointer, '', [fTpMtch]);
        if i = 3 then UpdResMinFpc(FLastAddedExp, stSymAll, 020600);
        LastExpectsToPtrReplace(Format('@%sDynInt1[3]', [s]));

        // Add/SUb to generic pointer
        AddFmtDef('%sPointer', [s], '\REPLACEME', skPointer, '', [fTpMtch]);
        if i = 3 then UpdResMinFpc(FLastAddedExp, stSymAll, 020600);
        LastExpectsToPtrReplace(Format('@%sDynInt1[2]', [s]));

        AddFmtDef('%sPointer+4', [s], '\REPLACEME', skPointer, '', [fTpMtch]);
        if i = 3 then UpdResMinFpc(FLastAddedExp, stSymAll, 020600);
        LastExpectsToPtrReplace(Format('@%sDynInt1[3]', [s]));

        AddFmtDef('%sPointer-4', [s], '\REPLACEME', skPointer, '', [fTpMtch]);
        if i = 3 then UpdResMinFpc(FLastAddedExp, stSymAll, 020600);
        LastExpectsToPtrReplace(Format('@%sDynInt1[1]', [s]));

        {%endregion  address of }
      end;

      {%endregion  Pointer }
    end; // i2

  end; // i
  {%endregion  Fields }

//TODO
  AddExpInt('PInteger(Field_DynInt1)[0]',  5511, M_Int);
  AddExpInt('PInteger(Field_DynInt1)[1]',  5512, M_Int);
  AddExpInt('PInteger(Field_DynInt1)[2]',  5513, M_Int);

  AddExpInt('^LongInt(Field_DynInt1)[0]',  5511, M_Int);
  AddExpInt('^LongInt(Field_DynInt1)[1]',  5512, M_Int);
  AddExpInt('^LongInt(Field_DynInt1)[2]',  5513, M_Int);

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
  Only := StrToIntDef(TestControlGetTestPattern, -1);
  OnlyNamePart := '';OnlyName := '';
  if Only < 0
  then begin
    OnlyName := TestControlGetTestPattern;
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
  if not TestControlCanTest(ControlTTestWatches) then exit;

  ClearTestErrors;

  // Floating point tests assume that decimal separator is a '.' so make sure that's what we get.
  FormatSettings.DecimalSeparator := '.';
  ClearAllTestArrays;
  AddExpectSimple;
  AddExpectArray_1;

  RunTestWatches('', TestExeName,  '', []);

  AssertTestErrors;
end;

initialization

  ErrorHandler := TTestFpErrorHandler.Create;
  RegisterDbgTest(TTestWatches);
  ControlTTestWatches := TestControlRegisterTest('TTestWatches');

end.

