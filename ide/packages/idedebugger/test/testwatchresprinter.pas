unit TestWatchResPrinter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IdeDebuggerWatchResPrinter, IdeDebuggerWatchResult, IdeDebuggerWatchValueIntf,
  LazDebuggerIntf, TestWatchResult, fpcunit, testutils, testregistry, RegExpr;

type

  { TTestWatchResPrinter }

  TTestWatchResPrinter = class(TTestBaseIdeDebuggerWatchResult)
  private
    // benchmark
    BenchMarkIdeRes: array [0..3] of TWatchResultData;

  private
    FWatchResPrinter: TWatchResultPrinter;
    FWatchResWrapper: TTestWatchResWrapper;

    function GetNumRes(ANumValue: QWord; ASigned: Boolean; AByteSize: Integer = 0): TWatchResultData; virtual;
    function TestPrint(AResValue: TWatchResultData; const ADispFormat: TWatchDisplayFormat): String;
    Procedure AssertPrint(AExp: String; AResValue: TWatchResultData; const ADispFormat: TWatchDisplayFormat);
    Procedure AssertPrint(AName: String; AExp: String; AResValue: TWatchResultData; const ADispFormat: TWatchDisplayFormat);
    Procedure AssertPrintNoMatch(AName: String; AMatch: String; AResValue: TWatchResultData; const ADispFormat: TWatchDisplayFormat);
    Procedure AssertPrintMatch(AName: String; AMatch: String; AResValue: TWatchResultData; const ADispFormat: TWatchDisplayFormat);

  private const
      MLEN1 = '(?is)Len=';
      MLEN2 = ':[\s\r\n]*\(';
      MLEN2s = ':[\s]*\(';
      MLEN2m = ':[\s]*\r\n?\s*\(';
      MNOT  = '(?:[^L]|L[^e]|Le[^n])*';
  private
    function RxLen1(Len: integer):String;
    function RxLen1S(Len: integer):String;
    function RxLen1M(Len: integer):String;
    function RxLen2(const Len: array of integer):String;
    function RxLen2S(const Len: array of integer):String;
    function Rep(s: string; cnt: integer): string;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestPrintNumber;
    procedure TestPrintNumberSignFormat;
    procedure TestMultiLine;
    procedure TestPrintArrayLen;
  public // do not run below by default
    procedure TestTime;
  end;

implementation

function TTestWatchResPrinter.GetNumRes(ANumValue: QWord; ASigned: Boolean; AByteSize: Integer
  ): TWatchResultData;
begin
  FWatchResWrapper.Done;
  FWatchResWrapper.ResIntf.CreateNumValue(ANumValue, ASigned, AByteSize);
  Result := FWatchResWrapper.IdeRes;
end;

function TTestWatchResPrinter.TestPrint(AResValue: TWatchResultData;
  const ADispFormat: TWatchDisplayFormat): String;
begin
  Result := FWatchResPrinter.PrintWatchValue(AResValue, ADispFormat, '');
end;

procedure TTestWatchResPrinter.AssertPrint(AExp: String; AResValue: TWatchResultData;
  const ADispFormat: TWatchDisplayFormat);
begin
  AssertPrint('', AExp, AResValue, ADispFormat);
end;

procedure TTestWatchResPrinter.AssertPrint(AName: String; AExp: String;
  AResValue: TWatchResultData; const ADispFormat: TWatchDisplayFormat);
begin
  AssertEquals(AName, AExp, TestPrint(AResValue, ADispFormat));
end;

procedure TTestWatchResPrinter.AssertPrintNoMatch(AName: String; AMatch: String;
  AResValue: TWatchResultData; const ADispFormat: TWatchDisplayFormat);
var
  s: String;
begin
  s := TestPrint(AResValue, ADispFormat);
  AssertFalse(AName+' '+AMatch+' ~ '+s, ExecRegExpr(AMatch, s));
end;

procedure TTestWatchResPrinter.AssertPrintMatch(AName: String; AMatch: String;
  AResValue: TWatchResultData; const ADispFormat: TWatchDisplayFormat);
var
  s: String;
begin
  s := TestPrint(AResValue, ADispFormat);
  AssertTrue(AName+' '+AMatch+' ~ '+s, ExecRegExpr(AMatch, s));
end;

function TTestWatchResPrinter.RxLen1(Len: integer): String;
begin
  Result := MLEN1+IntToStr(len)+MLEN2;
end;

function TTestWatchResPrinter.RxLen1S(Len: integer): String;
begin
  Result := MLEN1+IntToStr(len)+MLEN2s;
end;

function TTestWatchResPrinter.RxLen1M(Len: integer): String;
begin
  Result := MLEN1+IntToStr(len)+MLEN2m;
end;

function TTestWatchResPrinter.RxLen2(const Len: array of integer): String;
var
  s: String;
  i: Integer;
begin
  s := '';
  for i := 0 to Length(Len)-2 do s := s + IntToStr(Len[i]) + '\s*[,;]\s*';
  s := s + IntToStr(Len[Length(Len)-1]);
  Result := MLEN1+'\s*[\[(]\s*'+s+'\s*[\])]'+MLEN2;
end;

function TTestWatchResPrinter.RxLen2S(const Len: array of integer): String;
var
  s: String;
  i: Integer;
begin
  s := '';
  for i := 0 to Length(Len)-2 do s := s + IntToStr(Len[i]) + '\s*[,;]\s*';
  s := s + IntToStr(Len[Length(Len)-1]);
  Result := MLEN1+'\s*[\[(]\s*'+s+'\s*[\])]'+MLEN2s;
end;

function TTestWatchResPrinter.Rep(s: string; cnt: integer): string;
var
  i: Integer;
begin
  Result := s;
  for i := 2 to cnt do Result := Result + s;
end;

procedure TTestWatchResPrinter.SetUp;
begin
  inherited SetUp;
  FWatchResWrapper.Init;
  FWatchResPrinter := TWatchResultPrinter.Create;
end;

procedure TTestWatchResPrinter.TearDown;
begin
  FWatchResWrapper.Done;
  FreeAndNil(FWatchResPrinter);
  inherited TearDown;
end;

procedure TTestWatchResPrinter.TestPrintNumber;
var
  Res: TWatchResultData;
  d: TWatchDisplayFormat;
begin
  DefaultFormatSettings.ThousandSeparator := '.';


  d := DefaultWatchDisplayFormat;
  d.MakeAllOverrides;
  d.Num.BaseFormat                := vdfBaseDecimal;
  d.Num.SignFormat                := vdfSignAuto;
  d.Num.MinDigits[vdfBaseDecimal] := 0;
  d.Num.SeparatorDec              := False;
  d.Num.SeparatorHexBin           := vdfhsNone;

  Res := GetNumRes(QWord(201), True, 2);

  // Pad with lead 0
  d.Num.MinDigits[vdfBaseDecimal] := 0;
  AssertEquals('Pad 201', '201', FWatchResPrinter.PrintWatchValue(Res, d, ''));
  d.Num.MinDigits[vdfBaseDecimal] := 1;
  AssertEquals('Pad 201', '201', FWatchResPrinter.PrintWatchValue(Res, d, ''));
  d.Num.MinDigits[vdfBaseDecimal] := 2;
  AssertEquals('Pad 201', '201', FWatchResPrinter.PrintWatchValue(Res, d, ''));
  d.Num.MinDigits[vdfBaseDecimal] := 4;
  AssertEquals('Pad 0201', '0201', FWatchResPrinter.PrintWatchValue(Res, d, ''));
  d.Num.MinDigits[vdfBaseDecimal] := 7;
  AssertEquals('Pad 0201', '0000201', FWatchResPrinter.PrintWatchValue(Res, d, ''));
  d.Num.MinDigits[vdfBaseDecimal] := -1;
  AssertEquals('Pad 201', '00201', FWatchResPrinter.PrintWatchValue(Res, d, ''));

  // Pad with lead 0  and thousand-dot
  d.Num.SeparatorDec := True;
  d.Num.MinDigits[vdfBaseDecimal] := 0;
  AssertEquals('Pad/Sep 201', '201', FWatchResPrinter.PrintWatchValue(Res, d, ''));
  d.Num.MinDigits[vdfBaseDecimal] := 1;
  AssertEquals('Pad/Sep 201', '201', FWatchResPrinter.PrintWatchValue(Res, d, ''));
  d.Num.MinDigits[vdfBaseDecimal] := 2;
  AssertEquals('Pad/Sep 201', '201', FWatchResPrinter.PrintWatchValue(Res, d, ''));
  d.Num.MinDigits[vdfBaseDecimal] := 4;
  AssertEquals('Pad/Sep 0201', '0.201', FWatchResPrinter.PrintWatchValue(Res, d, ''));
  d.Num.MinDigits[vdfBaseDecimal] := 5;
  AssertEquals('Pad 0201', '00.201', FWatchResPrinter.PrintWatchValue(Res, d, ''));
  d.Num.MinDigits[vdfBaseDecimal] := 6;
  AssertEquals('Pad 0201', '000.201', FWatchResPrinter.PrintWatchValue(Res, d, ''));
  d.Num.MinDigits[vdfBaseDecimal] := 7;
  AssertEquals('Pad 0201', '0.000.201', FWatchResPrinter.PrintWatchValue(Res, d, ''));
  d.Num.MinDigits[vdfBaseDecimal] := 8;
  AssertEquals('Pad 0201', '00.000.201', FWatchResPrinter.PrintWatchValue(Res, d, ''));
  d.Num.MinDigits[vdfBaseDecimal] := 9;
  AssertEquals('Pad 0201', '000.000.201', FWatchResPrinter.PrintWatchValue(Res, d, ''));
  d.Num.MinDigits[vdfBaseDecimal] := -1;
  AssertEquals('Pad/Sep 201', '00.201', FWatchResPrinter.PrintWatchValue(Res, d, ''));

  // thousand-dot
  Res := GetNumRes(QWord(1234567), True, 4);
  d.Num.MinDigits[vdfBaseDecimal] := 0;
  d.Num.SeparatorDec := False;
  AssertEquals('Sep 1234567', '1234567', FWatchResPrinter.PrintWatchValue(Res, d, ''));
  d.Num.SeparatorDec := True;
  AssertEquals('Sep 1234567', '1.234.567', FWatchResPrinter.PrintWatchValue(Res, d, ''));

  d.Num.SeparatorDec := True;
  Res := GetNumRes(QWord(123456), True, 4);
  AssertEquals('Sep 123456', '123.456', FWatchResPrinter.PrintWatchValue(Res, d, ''));
  Res := GetNumRes(QWord(23456), True, 4);
  AssertEquals('Sep 23456', '23.456', FWatchResPrinter.PrintWatchValue(Res, d, ''));


  Res := GetNumRes(QWord(-1201), True, 2);
  d.Num.SeparatorDec := False;
  d.Num.MinDigits[vdfBaseDecimal] := 0;
  AssertEquals('', '-1201', FWatchResPrinter.PrintWatchValue(Res, d, ''));

  d.Num.MinDigits[vdfBaseDecimal] := 7;
  AssertEquals('', '-0001201', FWatchResPrinter.PrintWatchValue(Res, d, ''));

  d.Num.MinDigits[vdfBaseDecimal] := -1;
  AssertEquals('', '-01201', FWatchResPrinter.PrintWatchValue(Res, d, ''));

  d.Num.MinDigits[vdfBaseDecimal] := 0;
  d.Num.SeparatorDec              := True;
  AssertEquals('', '-1.201', FWatchResPrinter.PrintWatchValue(Res, d, ''));

  // hex full digit
  Res := GetNumRes(QWord($123456789A), False, 8);
  d.Num.BaseFormat                := vdfBaseHex;
  d.Num.MinDigits[vdfBaseDecimal] := 0;
  AssertEquals('Hex grp', '$000000123456789A', FWatchResPrinter.PrintWatchValue(Res, d, ''));
  d.Num.MinDigits[vdfBaseDecimal] := -1;
  AssertEquals('Hex grp', '$000000123456789A', FWatchResPrinter.PrintWatchValue(Res, d, ''));

  // hex spacing
  d.Num.MinDigits[vdfBaseHex] := 1;
  AssertEquals('Hex grp', '$123456789A', FWatchResPrinter.PrintWatchValue(Res, d, ''));
  d.Num.SeparatorHexBin := vdfhsByte;
  AssertEquals('Hex grp', '$12 34 56 78 9A', FWatchResPrinter.PrintWatchValue(Res, d, ''));
  d.Num.SeparatorHexBin := vdfhsWord;
  AssertEquals('Hex grp', '$12 3456 789A', FWatchResPrinter.PrintWatchValue(Res, d, ''));
  d.Num.SeparatorHexBin := vdfhsLong;
  AssertEquals('Hex grp', '$12 3456789A', FWatchResPrinter.PrintWatchValue(Res, d, ''));


  // digit count oct/bin
  Res := GetNumRes(QWord(201), True, 2);
  d.Num.BaseFormat                := vdfBaseOct;
  AssertEquals('Hex grp', '&311', FWatchResPrinter.PrintWatchValue(Res, d, ''));

  d.Num.MinDigits[vdfBaseOct] := -1;
  AssertEquals('Hex grp', '&000311', FWatchResPrinter.PrintWatchValue(Res, d, ''));

  d.Num.BaseFormat                := vdfBaseBin;
  AssertEquals('Hex grp', '%11001001', FWatchResPrinter.PrintWatchValue(Res, d, ''));

  d.Num.MinDigits[vdfBaseBin] := 12;
  AssertEquals('Hex grp', '%000011001001', FWatchResPrinter.PrintWatchValue(Res, d, ''));
end;

procedure TTestWatchResPrinter.TestPrintNumberSignFormat;
var
  d: TWatchDisplayFormat;
begin
  DefaultFormatSettings.ThousandSeparator := '.';

  d := DefaultWatchDisplayFormat;
  d.MakeAllOverrides;
  d.Num.BaseFormat                := vdfBaseDecimal;
  d.Num.SignFormat                := vdfSignAuto;
  d.Num.MinDigits[vdfBaseDecimal] := 0;
  d.Num.SeparatorDec              := False;
  d.Num.SeparatorHexBin           := vdfhsNone;

  AssertPrint('auto, int64:  1',                   '1',                    GetNumRes(QWord(1), True, 8), d);
  AssertPrint('auto, int64: -1',                   '-1',                   GetNumRes(QWord(-1), True, 8), d);
  AssertPrint('auto, int64: 9223372036854775807',  '9223372036854775807',  GetNumRes(QWord(9223372036854775807), True, 8), d);
  AssertPrint('auto, int64: -9223372036854775807', '-9223372036854775807', GetNumRes(QWord(-9223372036854775807), True, 8), d);
  AssertPrint('auto, int64: -9223372036854775808', '-9223372036854775808', GetNumRes(QWord($8000000000000000), True, 8), d);
  AssertPrint('auto, QWord:  1',                   '1',                    GetNumRes(QWord(1), False, 8), d);
  AssertPrint('auto, QWord: 18446744073709551615', '18446744073709551615', GetNumRes(QWord(-1), False, 8), d);
  AssertPrint('auto, QWord: 9223372036854775807',  '9223372036854775807',  GetNumRes(QWord(9223372036854775807), False, 8), d);
  AssertPrint('auto, QWord: 9223372036854775808',  '9223372036854775808',  GetNumRes(QWord($8000000000000000), False, 8), d);

  d.Num.SignFormat                := vdfSignSigned;
  AssertPrint('auto, int64:  1',                   '1',                    GetNumRes(QWord(1), True, 8), d);
  AssertPrint('auto, int64: -1',                   '-1',                   GetNumRes(QWord(-1), True, 8), d);
  AssertPrint('auto, int64: 9223372036854775807',  '9223372036854775807',  GetNumRes(QWord(9223372036854775807), True, 8), d);
  AssertPrint('auto, int64: -9223372036854775807', '-9223372036854775807', GetNumRes(QWord(-9223372036854775807), True, 8), d);
  AssertPrint('auto, int64: -9223372036854775808', '-9223372036854775808', GetNumRes(QWord($8000000000000000), True, 8), d);
  AssertPrint('auto, QWord:  1',                   '1',                    GetNumRes(QWord(1), False, 8), d);
  AssertPrint('auto, QWord: 18446744073709551615', '-1',                   GetNumRes(QWord(-1), False, 8), d);
  AssertPrint('auto, QWord: 9223372036854775807',  '9223372036854775807',  GetNumRes(QWord(9223372036854775807), False, 8), d);
  AssertPrint('auto, QWord: 9223372036854775808',  '-9223372036854775808', GetNumRes(QWord($8000000000000000), False, 8), d);

  d.Num.SignFormat                := vdfSignUnsigned;
  AssertPrint('auto, int64:  1',                   '1',                    GetNumRes(QWord(1), True, 8), d);
  AssertPrint('auto, int64: -1',                   '18446744073709551615', GetNumRes(QWord(-1), True, 8), d);
  AssertPrint('auto, int64: 9223372036854775807',  '9223372036854775807',  GetNumRes(QWord(9223372036854775807), True, 8), d);
  AssertPrint('auto, int64: -9223372036854775807', '9223372036854775809',  GetNumRes(QWord(-9223372036854775807), True, 8), d);
  AssertPrint('auto, int64: -9223372036854775808', '9223372036854775808',  GetNumRes(QWord($8000000000000000), True, 8), d);
  AssertPrint('auto, QWord:  1',                   '1',                    GetNumRes(QWord(1), False, 8), d);
  AssertPrint('auto, QWord: 18446744073709551615', '18446744073709551615', GetNumRes(QWord(-1), False, 8), d);
  AssertPrint('auto, QWord: 9223372036854775807',  '9223372036854775807',  GetNumRes(QWord(9223372036854775807), False, 8), d);
  AssertPrint('auto, QWord: 9223372036854775808',  '9223372036854775808',  GetNumRes(QWord($8000000000000000), False, 8), d);

end;

procedure TTestWatchResPrinter.TestMultiLine;
const
  RSingle = '[^\r\n]*?';
  RBreak  = '[^\r\n]*\r\n?[^\r\n]*?';
  RBreaks  = '[^\r\n]*\r.*?';
  RNxtFldS  = '[^:\r\n]*?(?:qq:[^:\r\n]*?)?'; // allow most-inner field
  RNxtFldM  = '[^:\r\n]*(?:qq:[^:\r\n]*)?\r\n?[^:\r\n]*?'; // allow most-inner field

  Procedure CheckData(AName: String;
    IdeRes: TWatchResultData;
    AMaxMultiLineDepth: integer; // Deeper than this will not be line-wrapped
    AForceSingleLine: Boolean;
    AForceSingleLineThresholdStructFld: integer; // fields in the struct itself
    AForceSingleLineThresholdArrayLen: integer;  // len of array
    AForceSingleLineReverseDepth: integer;       // amount of array/struct levels that can be excluded
    AForceSingleLineThresholdEach: integer;      // max nested elements in each field
    AForceSingleLineThresholdLen: integer;       // max printed len of each field (name + content)
    AMatch: String
  );
  var
    d2: TWatchDisplayFormat;
  begin
    d2 := DefaultWatchDisplayFormat;
    d2.MakeAllOverrides;
    d2.ArrayLen.HideLen := True;
    d2.ArrayLen.HideLenThresholdCnt  := 1;
    d2.ArrayLen.HideLenKeepDepth     := 0;
    d2.ArrayLen.HideLenThresholdEach := 0;
    d2.Multiline.MaxMultiLineDepth                 := AMaxMultiLineDepth;
    d2.Multiline.ForceSingleLine                   := AForceSingleLine;
    d2.Multiline.ForceSingleLineThresholdStructFld := AForceSingleLineThresholdStructFld;
    d2.Multiline.ForceSingleLineThresholdArrayLen  := AForceSingleLineThresholdArrayLen;
    d2.Multiline.ForceSingleLineReverseDepth       := AForceSingleLineReverseDepth;
    d2.Multiline.ForceSingleLineThresholdEach      := AForceSingleLineThresholdEach;
    d2.Multiline.ForceSingleLineThresholdLen       := AForceSingleLineThresholdLen;
    writeln;writeln(AMatch);writeln(TestPrint(IdeRes, d2));
    AssertPrintMatch(AName, AMatch, IdeRes, d2);
  end;

var
  IdeRes: TWatchResultData;
  MatchZero, MatchOne, MatchTwo, MatchThree: String;
  SecondLoop, MostInnerLoop, i: Integer;
  Second, MostInner: TBuildInfo;
begin
  MatchZero := '^[^\r\n]*$';
  FWatchResPrinter.FormatFlags := [rpfMultiLine, rpfIndent];

  for SecondLoop := 0 to 1 do // The single value
  for MostInnerLoop := 0 to 2 do begin  // The single value
    case SecondLoop of
      0: begin
        Second := b(datDynArray, 3);
        MatchOne := RxLen1M(4)+Rep(RxLen1S(3)+RBreak, 4) + '\)'; //  ONE level multiline
        MatchTwo := RxLen1M(4)+Rep(  // TWO levels with multiline
           RxLen1M(3)+RSingle+ Rep('a:'+RNxtFldS+'b:'+RNxtFldS+'c:'+RBreaks, 3),
           4);
        MatchThree := RxLen1M(4)+Rep(
           RxLen1M(3)+RSingle+ Rep('a:'+RNxtFldM+'b:'+RNxtFldM+'c:'+RBreaks, 3),
           4);
      end;
      else {1:} begin
        Second := b(dstRecord, ['x','y','z']);
        MatchOne := RxLen1M(4)+RNxtFldS+Rep('x:'+RSingle+'y:'+RSingle+'z:'+ RBreak, 4) + '\)'; //  ONE level multiline
        MatchTwo := RxLen1M(4)+Rep(  // TWO levels with multiline
           RSingle+ Rep('[xyz]:'+RNxtFldS+  'a:'+RNxtFldS+'b:'+RNxtFldS+'c:'+RBreaks, 3),
           4);
        MatchThree := RxLen1M(4)+Rep(
           RSingle+ Rep('[xyz]:'+RNxtFldS+  'a:'+RNxtFldM+'b:'+RNxtFldM+'c:'+RBreaks, 3),
           4);
      end;
    end;
    case MostInnerLoop of
      0: MostInner := b(rdkSignedNumVal,123456);
      1: MostInner := b(datDynArray, 1);  // single element in array // number will be auto-generated
      else {2:} MostInner := b(dstObject, ['qq']);  // single element in struct // number will be auto-generated
    end;


    IdeRes := CreateNestedData([
      b(datDynArray, 4),
      Second,
      b(dstRecord, ['a','b','c','d']),
      MostInner
    ]);

    CheckData('', IdeRes, 0,  False,   0, 0,   0,   0, 0,  MatchZero );
    CheckData('', IdeRes, 1,  False,   0, 0,   0,   0, 0,  MatchOne );
    CheckData('', IdeRes, 2,  False,   0, 0,   0,   0, 0,  MatchTwo );
    CheckData('', IdeRes, 3,  False,   0, 0,   0,   0, 0,  MatchThree );

    if MostInnerLoop = 0 then begin
      // force single
      for i := 0 to 2 do begin // nest EACH count does not matter // nothnig nested
        CheckData('', IdeRes, 3,  True,   3, 0,   2,   i, 0,  MatchThree );  // field cnt - not allowed
        CheckData('', IdeRes, 3,  True,   4, 0,   2,   i, 0,  MatchTwo );    // field cnt - allowed
        CheckData('', IdeRes, 3,  True,   4, 0,   2,   i,99,  MatchTwo );    // field cnt - allowed
        CheckData('', IdeRes, 3,  True,   4, 0,   2,   i, 1,  MatchThree );    // length not allowed
      end;
    end
    else begin
      // force single
      CheckData('', IdeRes, 3,  True,   3, 0,   3,   0, 0,  MatchThree );  // field cnt - not allowed
      CheckData('', IdeRes, 3,  True,   4, 0,   3,   0, 0,  MatchThree );  // nested not simple
      for i := 1 to 2 do begin // nest EACH count does not matter // nothnig nested
        CheckData('', IdeRes, 3,  True,   3, 0,   3,   i, 0,  MatchThree );  // field cnt - not allowed
        CheckData('', IdeRes, 3,  True,   4, 0,   3,   i, 0,  MatchTwo );    // field cnt - allowed
        CheckData('', IdeRes, 3,  True,   4, 0,   3,   i,99,  MatchTwo );    // field cnt - allowed
        CheckData('', IdeRes, 3,  True,   4, 0,   3,   i, 1,  MatchThree );    // length not allowed
      end;
    end;


    if MostInnerLoop = 0 then begin
      // force 2 levels to single line
      CheckData('', IdeRes, 3,  True,   4, 3,   2,   4, 0,  MatchOne );
      CheckData('', IdeRes, 9,  True,   4, 3,   2,   4, 0,  MatchOne );  // 2 levels from MaxMultiLineDepth;
      CheckData('', IdeRes, 2,  True,  99,99,   2, 999, 0,  MatchZero ); // 2 levels from MaxMultiLineDepth;
      CheckData('', IdeRes, 2,  True,  99,99,   1, 999, 0,  MatchOne );  // 1 levels from MaxMultiLineDepth;

      CheckData('', IdeRes, 3,  True,  99,99,   2, 999, 0,  MatchOne ); // 2 levels
      CheckData('', IdeRes, 3,  True,  99,99,   2, 999,30,  MatchTwo ); // Only 1 level, due to length
      CheckData('', IdeRes, 3,  True,  99,99,   2, 999, 3,  MatchThree ); // Only 0 level, due to length

      CheckData('', IdeRes, 3,  True,  99,99,   3,   3, 0,  MatchTwo );  // 1 levels, due to nested EACH
      CheckData('', IdeRes, 3,  True,  99,99,   3,   4, 0,  MatchOne );  // 2 levels, due to nested EACH
      CheckData('', IdeRes, 3,  True,  99,99,   3,  11, 0,  MatchOne );  // 2 levels, due to nested EACH
      CheckData('', IdeRes, 3,  True,  99,99,   3,  12, 0,  MatchZero ); // 3 levels, due to nested EACH
    end;

    IdeRes.Free;
  end;
end;

procedure TTestWatchResPrinter.TestPrintArrayLen;
  Procedure CheckData(AName: String; AMatch: String;
    IdeRes: TWatchResultData; const ADispFormat: TWatchDisplayFormat;
    AShowLenPrefix:         boolean;
    AShowLenPrefixEmbedded: boolean; // arary in struct in array
    ALenPrefixMaxNest:      integer; // In directly nested arrays, show for n nested levels too
    ALenPrefixCombine:      TValueDisplayFormatArrayType;
    AHideLen:               boolean;
    AHideLenThresholdCnt:   integer; // maximum "len" value to ommit
    AHideLenKeepDepth:      integer; // Min-nest level, before considering to hide
    AHideLenThresholdEach:  integer; // If each entry has less than n nested entries (0 = only simple values allowed)
    AHideLenThresholdLen:   integer // max printed len of each entry
   );
  var
    d2: TWatchDisplayFormat;
  begin
    d2 := ADispFormat;
    d2.ArrayLen.ShowLenPrefix        := AShowLenPrefix;
    d2.ArrayLen.ShowLenPrefixEmbedded:= AShowLenPrefixEmbedded;
    d2.ArrayLen.LenPrefixMaxNest     := ALenPrefixMaxNest;
    d2.ArrayLen.LenPrefixCombine     := ALenPrefixCombine;
    d2.ArrayLen.HideLen              := AHideLen;
    d2.ArrayLen.HideLenThresholdCnt  := AHideLenThresholdCnt;
    d2.ArrayLen.HideLenKeepDepth     := AHideLenKeepDepth;
    d2.ArrayLen.HideLenThresholdEach := AHideLenThresholdEach;
    d2.ArrayLen.HideLenThresholdLen  := AHideLenThresholdLen;
    AssertPrintMatch(AName, AMatch, IdeRes, d2);
  end;
var
  d: TWatchDisplayFormat;
  IdeRes: TWatchResultData;
  OuterIntf: IDbgWatchDataIntf;
  i: Integer;
begin
  d := DefaultWatchDisplayFormat;
  d.MakeAllOverrides;

  IdeRes := CreateNestedData([b(datDynArray, 10), b(rdkSignedNumVal,123456)]);
  CheckData('', RxLen1(10), IdeRes, d,   True, True, 0,   vdfatNone,  False, 0, 0, 0, 0);
  CheckData('', RxLen1(10), IdeRes, d,   True, True, 0,   vdfatDyn,   False, 0, 0, 0, 0);
  CheckData('', RxLen1(10), IdeRes, d,   True, True, 0,   vdfatStat,  False, 0, 0, 0, 0);
  CheckData('', RxLen1(10), IdeRes, d,   True, True, 0,   vdfatAll,   False, 0, 0, 0, 0);
  IdeRes.Free;


  IdeRes := CreateNestedData([b(datDynArray, 10), b(datDynArray, 9), b(rdkSignedNumVal,123456)]);
  CheckData('', RxLen1(10),                IdeRes, d,   True, True, 0,   vdfatNone,  False, 0, 0, 0, 0);
  CheckData('', RxLen1(10),                IdeRes, d,   True, True, 0,   vdfatStat,  False, 0, 0, 0, 0);
  CheckData('', RxLen1(10)+'.*'+RxLen1(9), IdeRes, d,   True, True, 1,   vdfatNone,  False, 0, 0, 0, 0);
  CheckData('', RxLen1(10)+'.*'+RxLen1(9), IdeRes, d,   True, True, 1,   vdfatStat,  False, 0, 0, 0, 0);

  CheckData('', RxLen2([10,9]),            IdeRes, d,   True, True, 0,   vdfatDyn,  False, 0, 0, 0, 0);
  CheckData('', RxLen2([10,9]),            IdeRes, d,   True, True, 0,   vdfatAll,  False, 0, 0, 0, 0);
  CheckData('', RxLen2([10,9]),            IdeRes, d,   True, True, 1,   vdfatDyn,  False, 0, 0, 0, 0);
  CheckData('', RxLen2([10,9]),            IdeRes, d,   True, True, 1,   vdfatAll,  False, 0, 0, 0, 0);
  IdeRes.Free;


  IdeRes := CreateNestedData([b(datStatArray, 10), b(datStatArray, 9), b(rdkSignedNumVal,123456)]);
  CheckData('', RxLen1(10),                IdeRes, d,   True, True, 0,   vdfatNone,  False, 0, 0, 0, 0);
  CheckData('', RxLen1(10),                IdeRes, d,   True, True, 0,   vdfatDyn,  False, 0, 0, 0, 0);
  CheckData('', RxLen1(10)+'.*'+RxLen1(9), IdeRes, d,   True, True, 1,   vdfatNone,  False, 0, 0, 0, 0);
  CheckData('', RxLen1(10)+'.*'+RxLen1(9), IdeRes, d,   True, True, 1,   vdfatDyn,  False, 0, 0, 0, 0);

  CheckData('', RxLen2([10,9]),            IdeRes, d,   True, True, 0,   vdfatStat,  False, 0, 0, 0, 0);
  CheckData('', RxLen2([10,9]),            IdeRes, d,   True, True, 0,   vdfatAll,  False, 0, 0, 0, 0);
  CheckData('', RxLen2([10,9]),            IdeRes, d,   True, True, 1,   vdfatStat,  False, 0, 0, 0, 0);
  CheckData('', RxLen2([10,9]),            IdeRes, d,   True, True, 1,   vdfatAll,  False, 0, 0, 0, 0);
  IdeRes.Free;


  IdeRes := CreateNestedData([b(datStatArray, 10), b(datDynArray, 9), b(rdkSignedNumVal,123456)]);
  CheckData('', RxLen1(10),                IdeRes, d,   True, True, 0,   vdfatNone,  False, 0, 0, 0, 0);
  CheckData('', RxLen1(10),                IdeRes, d,   True, True, 0,   vdfatDyn,  False, 0, 0, 0, 0);
  CheckData('', RxLen1(10),                IdeRes, d,   True, True, 0,   vdfatStat,  False, 0, 0, 0, 0);
  CheckData('', RxLen1(10)+'.*'+RxLen1(9), IdeRes, d,   True, True, 1,   vdfatNone,  False, 0, 0, 0, 0);
  CheckData('', RxLen1(10)+'.*'+RxLen1(9), IdeRes, d,   True, True, 1,   vdfatDyn,  False, 0, 0, 0, 0);
  CheckData('', RxLen1(10)+'.*'+RxLen1(9), IdeRes, d,   True, True, 1,   vdfatStat,  False, 0, 0, 0, 0);

  CheckData('', RxLen2([10,9]),            IdeRes, d,   True, True, 0,   vdfatAll,  False, 0, 0, 0, 0);
  CheckData('', RxLen2([10,9]),            IdeRes, d,   True, True, 1,   vdfatAll,  False, 0, 0, 0, 0);
  IdeRes.Free;


  IdeRes := CreateNestedData([b(datDynArray, 10), b(datStatArray, 9), b(rdkSignedNumVal,123456)]);
  CheckData('', RxLen1(10),                IdeRes, d,   True, True, 0,   vdfatNone,  False, 0, 0, 0, 0);
  CheckData('', RxLen1(10),                IdeRes, d,   True, True, 0,   vdfatDyn,  False, 0, 0, 0, 0);
  CheckData('', RxLen1(10),                IdeRes, d,   True, True, 0,   vdfatStat,  False, 0, 0, 0, 0);
  CheckData('', RxLen1(10)+'.*'+RxLen1(9), IdeRes, d,   True, True, 1,   vdfatNone,  False, 0, 0, 0, 0);
  CheckData('', RxLen1(10)+'.*'+RxLen1(9), IdeRes, d,   True, True, 1,   vdfatDyn,  False, 0, 0, 0, 0);
  CheckData('', RxLen1(10)+'.*'+RxLen1(9), IdeRes, d,   True, True, 1,   vdfatStat,  False, 0, 0, 0, 0);

  CheckData('', RxLen2([10,9]),            IdeRes, d,   True, True, 0,   vdfatAll,  False, 0, 0, 0, 0);
  CheckData('', RxLen2([10,9]),            IdeRes, d,   True, True, 1,   vdfatAll,  False, 0, 0, 0, 0);
  IdeRes.Free;


  (* *** check NEST depth / combine is not limited *** *)
  IdeRes := CreateNestedData([b(datDynArray, 10), b(datDynArray, 9, 8), b(rdkSignedNumVal,123456)]);
  CheckData('', RxLen1(10),                IdeRes, d,   True, True, 0,   vdfatNone,  False, 0, 0, 0, 0);
  CheckData('', RxLen1(10)+'.*'+RxLen1(9), IdeRes, d,   True, True, 1,   vdfatNone,  False, 0, 0, 0, 0);
  CheckData('', RxLen2([10,9]),            IdeRes, d,   True, True, 0,   vdfatAll,  False, 0, 0, 0, 0);
  IdeRes.Free;
  IdeRes := CreateNestedData([b(datDynArray, 10,9), b(datDynArray, 9), b(rdkSignedNumVal,123456)]);
  CheckData('', RxLen1(10),                IdeRes, d,   True, True, 0,   vdfatNone,  False, 0, 0, 0, 0);
  CheckData('', RxLen1(10)+'.*'+RxLen1(9), IdeRes, d,   True, True, 1,   vdfatNone,  False, 0, 0, 0, 0);
  CheckData('', RxLen1(10)+'.*'+RxLen1(9), IdeRes, d,   True, True, 1,   vdfatAll,  False, 0, 0, 0, 0);
  IdeRes.Free;

  IdeRes := CreateNestedData([b(datStatArray, 10), b(datStatArray, 9, 8), b(rdkSignedNumVal,123456)]);
  CheckData('', RxLen1(10),                IdeRes, d,   True, True, 0,   vdfatNone,  False, 0, 0, 0, 0);
  CheckData('', RxLen1(10)+'.*'+RxLen1(9), IdeRes, d,   True, True, 1,   vdfatNone,  False, 0, 0, 0, 0);
  CheckData('', RxLen2([10,9]),            IdeRes, d,   True, True, 0,   vdfatAll,  False, 0, 0, 0, 0);
  IdeRes.Free;
  IdeRes := CreateNestedData([b(datStatArray, 10,9), b(datStatArray, 9), b(rdkSignedNumVal,123456)]);
  CheckData('', RxLen1(10),                IdeRes, d,   True, True, 0,   vdfatNone,  False, 0, 0, 0, 0);
  CheckData('', RxLen1(10)+'.*'+RxLen1(9), IdeRes, d,   True, True, 1,   vdfatNone,  False, 0, 0, 0, 0);
  CheckData('', RxLen2([10,9]),            IdeRes, d,   True, True, 0,   vdfatAll,  False, 0, 0, 0, 0);
  IdeRes.Free;


  (* *** check combine adheres to stat vs dyn array / mixed array *** *)
  IdeRes := CreateNestedData([b(datStatArray, 10), b(datStatArray, 9), b(datDynArray, 8), b(rdkSignedNumVal,123456)]);
  CheckData('', RxLen2([10,9])+MNOT+'$',    IdeRes, d,   True, True, 0,   vdfatStat,  False, 0, 0, 0, 0);
  CheckData('', RxLen2([10,9])+MNOT+'$',    IdeRes, d,   True, True, 1,   vdfatStat,  False, 0, 0, 0, 0);
  CheckData('', RxLen2([10,9])+'.*'+RxLen1(8),    IdeRes, d,   True, True, 2,   vdfatStat,  False, 0, 0, 0, 0);
  IdeRes.Free;


  (* *** check 2nd multi dim run  *** *)
  IdeRes := CreateNestedData([b(datDynArray, 10),
         b(dstRecord, ['Foo']),
         b(datDynArray, 20), b(rdkSignedNumVal,9123456)
       ]);
  CheckData('', RxLen1(10)+MNOT+'$', IdeRes, d,   True, False, 0,   vdfatNone,  False, 0, 0, 0, 0);
  CheckData('', RxLen1(10)+'.*'+RxLen1(20), IdeRes, d,   True, True, 0,   vdfatNone,  False, 0, 0, 0, 0);
  IdeRes.Free;


  (* *** HIDE *** *)
  (* *** Check Min-Len *** *)
  IdeRes := CreateNestedData([b(datDynArray, 10), b(datDynArray, 5), b(datDynArray, 4), b(rdkSignedNumVal,123456)]);
  CheckData('', RxLen1(10)+'.*'+RxLen1(5)+'.*'+RxLen1(4),
            IdeRes, d,   True, True, 9,   vdfatNone,  False, 0, 0, 0, 0);
  CheckData('', RxLen1(10)+'.*'+RxLen1(5)+'.*'+RxLen1(4),
            IdeRes, d,   True, True, 9,   vdfatNone,  True, 3, 1, 99, 0);
  CheckData('', RxLen1(10)+'.*'+RxLen1(5)+MNOT+'$',
            IdeRes, d,   True, True, 9,   vdfatNone,  True, 4, 1, 99, 0);
  CheckData('', RxLen1(10)+MNOT+'$',
            IdeRes, d,   True, True, 9,   vdfatNone,  True, 5, 1, 99, 0);
  CheckData('', RxLen1(10)+MNOT+'$',
            IdeRes, d,   True, True, 9,   vdfatNone,  True, 15, 1, 99, 0); // keep 1 level

  // nested elem
  CheckData('', RxLen1(10)+'.*'+RxLen1(5)+MNOT+'$',
            IdeRes, d,   True, True, 9,   vdfatNone,  True, 5, 1, 3, 0);
  CheckData('', RxLen1(10)+MNOT+'$',
            IdeRes, d,   True, True, 9,   vdfatNone,  True, 5, 1, 4, 0);
  IdeRes.Free;

  IdeRes := CreateNestedData([b(datDynArray, 10), b(datDynArray, 5), b(dstRecord, ['a','b','c','d']), b(rdkSignedNumVal,123456)]);
  CheckData('', RxLen1(10)+'.*'+RxLen1(5)+MNOT+'$',
            IdeRes, d,   True, True, 9,   vdfatNone,  False, 0, 0, 0, 0);
  // nested elem
  CheckData('', RxLen1(10)+'.*'+RxLen1(5)+MNOT+'$',
            IdeRes, d,   True, True, 9,   vdfatNone,  True, 5, 1, 3, 0);
  CheckData('', RxLen1(10)+MNOT+'$',
            IdeRes, d,   True, True, 9,   vdfatNone,  True, 5, 1, 4, 0);
  IdeRes.Free;

end;

procedure TTestWatchResPrinter.TestTime;
var
  s: String;
  tt: QWord;
  i: Integer;
  d: TWatchDisplayFormat;
begin
  if BenchMarkIdeRes[0] = nil then begin
    BenchMarkIdeRes[0]:= CreateNestedData([
      b(datDynArray, 1000),
      b(dstRecord, ['Foo','LongFieldName123456'*dfvProtected,'LongFieldName123457','LongFieldName123458','a','b','c']),
      b(dstClass, ['Foo','LongFieldName123456','LongFieldName123457'*dfvPrivate,'LongFieldName123458','a','b']),
      b(rdkSignedNumVal,123456)
    ]);

    BenchMarkIdeRes[1]:= CreateNestedData([
      b(datDynArray, 50),
      b(dstRecord, ['Foo','LongFieldName123456'*dfvPrivate,'LongFieldName123457','LongFieldName123458'*dfvProtected,'a','b']),
      b(dstRecord, ['Foo','LongFieldName123456','LongFieldName123457','LongFieldName123458','a','b']),
      b(dstRecord, ['Foo','LongFieldName123456','LongFieldName123457'*dfvProtected,'LongFieldName123458','a','b']),
      b(dstRecord, ['Foo','LongFieldName123456','LongFieldName123457','LongFieldName123458','a','b']),
      b(rdkSignedNumVal,123456)
    ]);

    BenchMarkIdeRes[2] := CreateNestedData([
      b(datDynArray, 4),
      b(dstRecord, ['Foo','LongFieldName123456','LongFieldName123457','LongFieldName123458','a','b']),
      b(datDynArray, 5),
      b(dstRecord, ['Foo','LongFieldName123456','LongFieldName123457','LongFieldName123458','a','b']),
      b(datDynArray, 20),
      b(dstRecord, ['Foo','LongFieldName123456','LongFieldName123457','LongFieldName123458','a','b']),
      b(dstRecord, ['Foo','LongFieldName123456','LongFieldName123457','LongFieldName123458','a','b']),
      b(rdkSignedNumVal,123456)
    ]);

    BenchMarkIdeRes[3] := CreateNestedData([
      b(datDynArray, 200),
      b(datDynArray, 50),
      b(datDynArray, 40),
      b(rdkSignedNumVal,123456)
    ]);
  end;

  d := DefaultWatchDisplayFormat;
  d.Struct.UseInherited := False;
  d.Struct.DataFormat := vdfStructValOnly;
  for i := 0 to 5 do begin
    tt := GetTickCount64; s := TestPrint(BenchMarkIdeRes[0], d); Write(GetTickCount64-tt, ' ## ');
  end;
  WriteLn( ' ## ', Length(s));

  d := DefaultWatchDisplayFormat;
  d.Struct.UseInherited := False;
  d.Struct.DataFormat := vdfStructFields;
  for i := 0 to 5 do begin
    tt := GetTickCount64; s := TestPrint(BenchMarkIdeRes[0], d); Write(GetTickCount64-tt, ' ## ');
  end;
  WriteLn( ' ## ', Length(s));

  d := DefaultWatchDisplayFormat;
  d.Struct.UseInherited := False;
  d.Struct.DataFormat := vdfStructFull;
  for i := 0 to 5 do begin
    tt := GetTickCount64; s := TestPrint(BenchMarkIdeRes[0], d); Write(GetTickCount64-tt, ' ## ');
  end;
  WriteLn( ' ## ', Length(s));

  d := DefaultWatchDisplayFormat;
  d.Struct.UseInherited := False;
  d.Struct.DataFormat := vdfStructValOnly;
  d.Struct.ShowPointerFormat := vdfStructPointerOn;
  for i := 0 to 5 do begin
    tt := GetTickCount64; s := TestPrint(BenchMarkIdeRes[0], d); Write(GetTickCount64-tt, ' ## ');
  end;
  WriteLn( ' ## ', Length(s));


  for i := 0 to 5 do begin
    tt := GetTickCount64; s := TestPrint(BenchMarkIdeRes[1], DefaultWatchDisplayFormat); Write(GetTickCount64-tt, ' ## ');
  end;
  WriteLn( ' ## ', Length(s));


  for i := 0 to 5 do begin
    tt := GetTickCount64; s := TestPrint(BenchMarkIdeRes[2], DefaultWatchDisplayFormat); Write(GetTickCount64-tt, ' ## ');
  end;
  WriteLn( ' ## ', Length(s));


  for i := 0 to 5 do begin
    tt := GetTickCount64; s := TestPrint(BenchMarkIdeRes[3], DefaultWatchDisplayFormat); Write(GetTickCount64-tt, ' ## ');
  end;
  WriteLn( ' ## ', Length(s));

end;



initialization

  RegisterTest(TTestWatchResPrinter);
end.

