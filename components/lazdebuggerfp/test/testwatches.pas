unit TestWatches;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, TestGDBMIControl, DbgIntfBaseTypes,
  DbgIntfDebuggerBase, TestBase, FpGdbmiDebugger, LCLProc, SynRegExpr, TestWatchUtils,
  GDBMIDebugger;

const
  BREAK_LINE_TestWatchesUnitSimple_1 = 182;
  BREAK_LINE_TestWatchesUnitSimple_2 = 189;
  BREAK_LINE_TestWatchesUnitSimple_3 = 196;

  BREAK_LINE_TestWatchesUnitArray = 176;

type

  { TTestWatches }

  TTestWatches = class(TTestWatchesBase)
  private
    FWatches: TWatches;

    ExpectBreakSimple_1: TWatchExpectationArray;
    ExpectBreakSimple_2: TWatchExpectationArray;
    ExpectBreakSimple_3: TWatchExpectationArray;

    ExpectBreakArray_1: TWatchExpectationArray;

    FCurrentExpect: ^TWatchExpectationArray; // currently added to

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

    procedure AdjustExpectToAddress(AWatchExp: PWatchExpectation);

    procedure AddExpectSimple_1;
    procedure AddExpectSimple_2;
    procedure AddExpectSimple_3;
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
  Match_ArgTFoo = '<TFoo> = \{.*(<|vptr\$)TObject>?.+ValueInt = -11';
  Match_ArgTFoo1 = '<TFoo> = \{.*(<|vptr\$)TObject>?.+ValueInt = 31';
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
  If AValFoo <> '' then Result := Result + ',.* valfoo = '+AValFoo;
end;

function MatchClass(TypeName: String; AContent: String = ''): String;
begin
  Result := '<'+TypeName+'> = \{.*(vptr\$|<TObject>).+'+AContent;
end;

function MatchClassNil(TypeName: String): String;
begin
  Result := '<'+TypeName+'> = nil';
end;


{ TTestWatches }

procedure TTestWatches.AdjustExpectToAddress(AWatchExp: PWatchExpectation);
var
  OtherWatchExp: PWatchExpectation;
  s: String;
  st: TSymbolType;
begin
  OtherWatchExp := PWatchExpectation(AWatchExp^.UserData);
  if OtherWatchExp = nil then exit;;
  s := OtherWatchExp^.TheWatch.Values[1,0].Value;
  delete(s, 1, pos('$', s) - 1); delete(s, pos(')', s), 99);
  for st := low(TSymbolType) to high(TSymbolType) do
    AWatchExp^.Result[st].ExpMatch := '\'+s;
end;

procedure TTestWatches.DoDbgOutput(Sender: TObject; const AText: String);
begin
  inherited DoDbgOutput(Sender, AText);
  if FDbgOutPutEnable then
    FDbgOutPut := FDbgOutPut + AText;
end;

procedure TTestWatches.ClearAllTestArrays;
begin
  //SetLength(ExpectBreakFooGdb, 0);
  //SetLength(ExpectBreakSubFoo, 0);
end;

function TTestWatches.HasTestArraysData: Boolean;
begin
  //Result := (Length(ExpectBreakFooGdb) > 0) or
  //          (Length(ExpectBreakSubFoo) > 0) or
  //          (Length(ExpectBreakFoo) > 0) or
  //          (Length(ExpectBreakFooArray) >0 );
  //

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
  AddFmtDef(AnExpr, '^'+IntToStr(AMtch), skSimple, ATpNm, [fTpMtch]);
end;

function TTestWatches.AddSimpleUInt(AnExpr: string; AMtch: QWord;
  ATpNm: string): PWatchExpectation;
begin
  AddFmtDef(AnExpr, '^'+IntToStr(AMtch), skSimple, ATpNm, [fTpMtch]);
end;

procedure TTestWatches.AddExpectSimple_1;
var
  i: Integer;
  s, s2, s2def: String;
  j: Integer;
  r: PWatchExpectation;
begin
  FCurrentExpect := @ExpectBreakSimple_1;

  {%region Int?Cardinal types}
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
    AddSimpleInt(Format(s, ['SimpleLocal_Short1']), 39, s2);
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
    AddSimpleInt(Format(s, ['SimpleLocal_Small1']), 391, s2);
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
    AddSimpleInt(Format(s, ['SimpleLocal_Int1']),  3901, s2);
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
    AddSimpleInt(Format(s, ['SimpleLocal_QInt1']), 39001, s2);
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
    AddSimpleUInt(Format(s, ['SimpleLocal_Byte1']), 59, s2);
    AddSimpleUInt(Format(s, ['SimpleGlob_Byte1 ']), 49, s2);
    AddSimpleUInt(Format(s, ['SimpleGlob_Byte2']), $7f, s2);
    AddSimpleUInt(Format(s, ['SimpleGlob_Byte3']), $80, s2);
    AddSimpleUInt(Format(s, ['SimpleGlob_Byte4']), high(Byte), s2);
    AddSimpleUInt(Format(s, ['SimpleGlob_Byte5']), low(Byte), s2);

    s2 := s2def;
    if s2 = '' then s2 := 'Word';
    AddSimpleUInt(Format(s, ['SimpleLocal_Word1']), 591, s2);
    AddSimpleUInt(Format(s, ['SimpleGlob_Word1 ']), 491, s2);
    AddSimpleUInt(Format(s, ['SimpleGlob_Word2']), $7fff, s2);
    AddSimpleUInt(Format(s, ['SimpleGlob_Word3']), $8000, s2);
    AddSimpleUInt(Format(s, ['SimpleGlob_Word4']), high(Word), s2);
    AddSimpleUInt(Format(s, ['SimpleGlob_Word5']), low(Word), s2);

    s2 := s2def;
    if s2 = '' then s2 := 'LongWord';
    AddSimpleUInt(Format(s, ['SimpleLocal_DWord1']), 5901, s2);
    AddSimpleUInt(Format(s, ['SimpleGlob_DWord1 ']), 4901, s2);
    AddSimpleUInt(Format(s, ['SimpleGlob_DWord2']), $7fffffff, s2);
    AddSimpleUInt(Format(s, ['SimpleGlob_DWord3']), $80000000, s2);
    AddSimpleUInt(Format(s, ['SimpleGlob_DWord4']), high(LongWord), s2);
    AddSimpleUInt(Format(s, ['SimpleGlob_DWord5']), low(LongWord), s2);

    s2 := s2def;
    if s2 = '' then s2 := 'QWord';
    if not(i in [2]) then begin
      AddSimpleUInt(Format(s, ['SimpleLocal_QWord1']), 59001, s2);
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
  AddSimpleInt(Format(s, ['SimpleLocal_Int1']),  Byte(3901), s2);
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

    AddSimpleInt(Format(s, ['SimpleField_Short1']),   j+11, 'ShortInt');
    AddSimpleInt(Format(s, ['SimpleField_Small1']),   j+12, 'SmallInt');
    AddSimpleInt(Format(s, ['SimpleField_Int1']),     j+13, 'LongInt');
    AddSimpleInt(Format(s, ['SimpleField_QInt1']),    j+14, 'Int64');

    AddSimpleInt(Format(s, ['SimpleField_Byte1']),     j+15, 'Byte');
    AddSimpleInt(Format(s, ['SimpleField_Word1']),     j+16, 'Word');
    AddSimpleInt(Format(s, ['SimpleField_DWord1']),    j+17, 'LongWord');
    AddSimpleInt(Format(s, ['SimpleField_QWord1']),    j+18, 'QWord');
  end;
  {%region}

  s := '%s';
  AddFmtDef(Format(s, ['SimpleGlob_Single1']),   '^99\.(2|19)',  skSimple, '', [fTpMtch]);
  AddFmtDef(Format(s, ['SimpleGlob_Double1']),   '^199\.(3|29)', skSimple, '', [fTpMtch]);
  AddFmtDef(Format(s, ['SimpleGlob_Ext1']),      '^299\.(4|39)', skSimple, '', [fTpMtch]);
  AddSimpleInt(Format(s, ['SimpleGlob_Comp1']),    -2, '');

  {%region AddressOf / Var param, hidden pointer}
    //SimplePArg_Int1, SimplePVArg_Int1, SimplePLocal_Int1, SimplePGlob_Int1: PLongInt;
    r := AddFmtDef('@SimpleArg_Int1',    'replaceme', skPointer, '');
    r^.OnBeforeTest := @AdjustExpectToAddress;
    r^.UserData := AddFmtDef('SimplePArg_Int1',    '\$[0-9A-F]', skPointer, '');

    r := AddFmtDef('@SimpleVArg_Int1',    'replaceme', skPointer, '');
    r^.OnBeforeTest := @AdjustExpectToAddress;
    r^.UserData := AddFmtDef('SimplePVArg_Int1',    '\$[0-9A-F]', skPointer, '');
    UpdResMinFpc(r, stSymAll, 020600);

    r := AddFmtDef('@SimpleLocal_Int1',    'replaceme', skPointer, '');
    r^.OnBeforeTest := @AdjustExpectToAddress;
    r^.UserData := AddFmtDef('SimplePLocal_Int1',    '\$[0-9A-F]', skPointer, '');

    r := AddFmtDef('@SimpleGlob_Int1',    'replaceme', skPointer, '');
    r^.OnBeforeTest := @AdjustExpectToAddress;
    r^.UserData := AddFmtDef('SimplePGlob_Int1',    '\$[0-9A-F]', skPointer, '');

  {%region}

end;

procedure TTestWatches.AddExpectSimple_2;
var
  s: String;
begin
  FCurrentExpect := @ExpectBreakSimple_2;

  s := '%s';
    AddSimpleInt(Format(s, ['SimpleField_Short1']),   11, 'ShortInt');
    AddSimpleInt(Format(s, ['SimpleField_Small1']),   12, 'SmallInt');
    AddSimpleInt(Format(s, ['SimpleField_Int1']),     13, 'LongInt');
    AddSimpleInt(Format(s, ['SimpleField_QInt1']),    14, 'Int64');

    AddSimpleInt(Format(s, ['SimpleField_Byte1']),     15, 'Byte');
    AddSimpleInt(Format(s, ['SimpleField_Word1']),     16, 'Word');
    AddSimpleInt(Format(s, ['SimpleField_DWord1']),    17, 'LongWord');
    AddSimpleInt(Format(s, ['SimpleField_QWord1']),    18, 'QWord');

  s := 'self.%s';
    AddSimpleInt(Format(s, ['SimpleField_Short1']),   11, 'ShortInt');
    AddSimpleInt(Format(s, ['SimpleField_Small1']),   12, 'SmallInt');
    AddSimpleInt(Format(s, ['SimpleField_Int1']),     13, 'LongInt');
    AddSimpleInt(Format(s, ['SimpleField_QInt1']),    14, 'Int64');

    AddSimpleInt(Format(s, ['SimpleField_Byte1']),     15, 'Byte');
    AddSimpleInt(Format(s, ['SimpleField_Word1']),     16, 'Word');
    AddSimpleInt(Format(s, ['SimpleField_DWord1']),    17, 'LongWord');
    AddSimpleInt(Format(s, ['SimpleField_QWord1']),    18, 'QWord');

end;

procedure TTestWatches.AddExpectSimple_3;
var
  s: String;
begin
  FCurrentExpect := @ExpectBreakSimple_3;

  s := '%s';
    AddSimpleInt(Format(s, ['SimpleField_Short1']),   111, 'ShortInt');
    AddSimpleInt(Format(s, ['SimpleField_Small1']),   112, 'SmallInt');
    AddSimpleInt(Format(s, ['SimpleField_Int1']),     113, 'LongInt');
    AddSimpleInt(Format(s, ['SimpleField_QInt1']),    114, 'Int64');

    AddSimpleInt(Format(s, ['SimpleField_Byte1']),     115, 'Byte');
    AddSimpleInt(Format(s, ['SimpleField_Word1']),     116, 'Word');
    AddSimpleInt(Format(s, ['SimpleField_DWord1']),    117, 'LongWord');
    AddSimpleInt(Format(s, ['SimpleField_QWord1']),    118, 'QWord');

  s := 'self.%s';
    AddSimpleInt(Format(s, ['SimpleField_Short1']),   111, 'ShortInt');
    AddSimpleInt(Format(s, ['SimpleField_Small1']),   112, 'SmallInt');
    AddSimpleInt(Format(s, ['SimpleField_Int1']),     113, 'LongInt');
    AddSimpleInt(Format(s, ['SimpleField_QInt1']),    114, 'Int64');

    AddSimpleInt(Format(s, ['SimpleField_Byte1']),     115, 'Byte');
    AddSimpleInt(Format(s, ['SimpleField_Word1']),     116, 'Word');
    AddSimpleInt(Format(s, ['SimpleField_DWord1']),    117, 'LongWord');
    AddSimpleInt(Format(s, ['SimpleField_QWord1']),    118, 'QWord');

end;

procedure TTestWatches.AddExpectArray_1;
begin
  FCurrentExpect := @ExpectBreakArray_1;

  AddFmtDef('ArrayGlob_DynInt2', '^nil', skArray, '', [fTpMtch]);
  AddSimpleInt('QWord(ArrayGlob_DynInt2)',   0, 'QWord');

  // TODO var param
  //UpdResMinFpc(r, stSymAll, 020600);

  AddFmtDef('ArrayGlob_DynInt1', '^[\(L].*5511, 5512, 5513, 5514, -5511',
            skArray, '', [fTpMtch]);
  AddSimpleInt('ArrayGlob_DynInt1[0]',    5511, M_Int);
  AddSimpleInt('ArrayGlob_DynInt1[19]',    5500, M_Int);


  AddFmtDef('ArrayGlob_StatInt1', '^[\(L].*6600, 6601, 6602',
            skArray, '', [fTpMtch]);
  AddSimpleInt('ArrayGlob_StatInt1[4]',    6600, M_Int);
  AddSimpleInt('ArrayGlob_StatInt1[9]',    6699, M_Int);
  AddFmtDef('ArrayGlob_StatInt1[3]', '', skSimple, M_Int, [fTpMtch]); // Just do not crash
  AddFmtDef('ArrayGlob_StatInt1[10]', '', skSimple, M_Int, [fTpMtch]); // Just do not crash
  AddFmtDef('ArrayGlob_StatInt1[-1]', '', skSimple, M_Int, [fTpMtch]); // Just do not crash


  AddFmtDef('ArrayGlob_StatInt2', '^[\(L].*3300, 3301, 3302',
            skArray, '', [fTpMtch]);
  AddSimpleInt('ArrayGlob_StatInt2[-4]',    3300, M_Int);
  AddSimpleInt('ArrayGlob_StatInt2[0]',    3304, M_Int);


  AddFmtDef('FieldDynInt1', '^[\(L].*100, 101, 102', skArray, '', [fTpMtch]);


  AddFmtDef('FieldDynClass1', '^[\(L].*?'+
    '\(.*FIELDINT1 = 98700;.*FIELDINT2 = 98701;.*FIELDDYNAINT1 = \(9900, 9901\);.*\), ' +
    '\(.*FIELDINT1 = 88700;.*FIELDINT2 = 88701;.*FIELDDYNAINT1 = \(8900, 8901\);.*\), ' +
    '\(.*FIELDINT1 = 78700;.*FIELDINT2 = 78701;.*FIELDDYNAINT1 = \(7900, 7901, 7902\);.*\)',
  skArray, '', [fTpMtch]);


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

    SetBreak('TestWatchesUnitSimple.pas', BREAK_LINE_TestWatchesUnitSimple_1);
    SetBreak('TestWatchesUnitSimple.pas', BREAK_LINE_TestWatchesUnitSimple_2);
    SetBreak('TestWatchesUnitSimple.pas', BREAK_LINE_TestWatchesUnitSimple_3);
    SetBreak('TestWatchesUnitArray.pas', BREAK_LINE_TestWatchesUnitArray);

    if dbg.State = dsError then
      Fail(' Failed Init');

    AddWatches(ExpectBreakSimple_1, FWatches, Only, OnlyName, OnlyNamePart);
    AddWatches(ExpectBreakSimple_2, FWatches, Only, OnlyName, OnlyNamePart);
    AddWatches(ExpectBreakSimple_3, FWatches, Only, OnlyName, OnlyNamePart);
    AddWatches(ExpectBreakArray_1,  FWatches, Only, OnlyName, OnlyNamePart);

    (* Start debugging *)
    dbg.ShowConsole := True;


    dbg.Run;
    if not TestTrue('State=Pause', dbg.State = dsPause) then begin
      TestTrue('Hit BREAK_LINE_TestWatchesUnitSimple_1', False);
      exit;
    end;
    (* Hit first breakpoint:  *)
    TestWatchList('Simple1',ExpectBreakSimple_1, dbg, Only, OnlyName, OnlyNamePart);


    dbg.Run;
    if not TestTrue('State=Pause', dbg.State = dsPause) then begin
      TestTrue('Hit BREAK_LINE_TestWatchesUnitSimple', False);
      exit;
    end;
    (* Hit 2nd Simple breakpoint: *)
    TestWatchList('Simple2',ExpectBreakSimple_2, dbg, Only, OnlyName, OnlyNamePart);

    dbg.Run;
    if not TestTrue('State=Pause', dbg.State = dsPause) then begin
      TestTrue('Hit BREAK_LINE_TestWatchesUnitSimple', False);
      exit;
    end;
    (* Hit 3rd Simlpe breakpoint: *)
    TestWatchList('Simple3',ExpectBreakSimple_3, dbg, Only, OnlyName, OnlyNamePart);


    // array


    dbg.Run;
   if not TestTrue('State=Pause', dbg.State = dsPause) then begin
      TestTrue('Hit BREAK_LINE_TestWatchesUnitArray', False);
      exit;
    end;

    (* Hit 11st Array breakpoint: *)
    TestWatchList('Array1',ExpectBreakArray_1, dbg, Only, OnlyName, OnlyNamePart);

    dbg.Run;





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
  AddExpectSimple_1;
  AddExpectSimple_2;
  AddExpectSimple_3;
  AddExpectArray_1;

  RunTestWatches('', TestExeName,  '', []);

  AssertTestErrors;
end;

initialization

  RegisterDbgTest(TTestWatches);
  RegisterTestSelectors(['TTestWatches'
                        ]);

end.

