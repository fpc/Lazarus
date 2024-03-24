unit TestWatchResPrinter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IdeDebuggerWatchResPrinter, IdeDebuggerWatchResult, IdeDebuggerWatchValueIntf,
  TestWatchResult, fpcunit, testutils, testregistry;

type

  { TTestWatchResPrinter }

  TTestWatchResPrinter = class(TTestCase)
  private
    FWatchResPrinter: TWatchResultPrinter;
    FWatchResWrapper: TTestWatchResWrapper;

    function GetNumRes(ANumValue: QWord; ASigned: Boolean; AByteSize: Integer = 0): TWatchResultData; virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestPrintNumber;
  end;

implementation

function TTestWatchResPrinter.GetNumRes(ANumValue: QWord; ASigned: Boolean; AByteSize: Integer
  ): TWatchResultData;
begin
  FWatchResWrapper.Done;
  FWatchResWrapper.ResIntf.CreateNumValue(ANumValue, ASigned, AByteSize);
  Result := FWatchResWrapper.IdeRes;
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
  AssertEquals('Pad 201', '201', FWatchResPrinter.PrintWatchValue(Res, d));
  d.Num.MinDigits[vdfBaseDecimal] := 1;
  AssertEquals('Pad 201', '201', FWatchResPrinter.PrintWatchValue(Res, d));
  d.Num.MinDigits[vdfBaseDecimal] := 2;
  AssertEquals('Pad 201', '201', FWatchResPrinter.PrintWatchValue(Res, d));
  d.Num.MinDigits[vdfBaseDecimal] := 4;
  AssertEquals('Pad 0201', '0201', FWatchResPrinter.PrintWatchValue(Res, d));
  d.Num.MinDigits[vdfBaseDecimal] := 7;
  AssertEquals('Pad 0201', '0000201', FWatchResPrinter.PrintWatchValue(Res, d));
  d.Num.MinDigits[vdfBaseDecimal] := -1;
  AssertEquals('Pad 201', '00201', FWatchResPrinter.PrintWatchValue(Res, d));

  // Pad with lead 0  and thousand-dot
  d.Num.SeparatorDec := True;
  d.Num.MinDigits[vdfBaseDecimal] := 0;
  AssertEquals('Pad/Sep 201', '201', FWatchResPrinter.PrintWatchValue(Res, d));
  d.Num.MinDigits[vdfBaseDecimal] := 1;
  AssertEquals('Pad/Sep 201', '201', FWatchResPrinter.PrintWatchValue(Res, d));
  d.Num.MinDigits[vdfBaseDecimal] := 2;
  AssertEquals('Pad/Sep 201', '201', FWatchResPrinter.PrintWatchValue(Res, d));
  d.Num.MinDigits[vdfBaseDecimal] := 4;
  AssertEquals('Pad/Sep 0201', '0.201', FWatchResPrinter.PrintWatchValue(Res, d));
  d.Num.MinDigits[vdfBaseDecimal] := 5;
  AssertEquals('Pad 0201', '00.201', FWatchResPrinter.PrintWatchValue(Res, d));
  d.Num.MinDigits[vdfBaseDecimal] := 6;
  AssertEquals('Pad 0201', '000.201', FWatchResPrinter.PrintWatchValue(Res, d));
  d.Num.MinDigits[vdfBaseDecimal] := 7;
  AssertEquals('Pad 0201', '0.000.201', FWatchResPrinter.PrintWatchValue(Res, d));
  d.Num.MinDigits[vdfBaseDecimal] := 8;
  AssertEquals('Pad 0201', '00.000.201', FWatchResPrinter.PrintWatchValue(Res, d));
  d.Num.MinDigits[vdfBaseDecimal] := 9;
  AssertEquals('Pad 0201', '000.000.201', FWatchResPrinter.PrintWatchValue(Res, d));
  d.Num.MinDigits[vdfBaseDecimal] := -1;
  AssertEquals('Pad/Sep 201', '00.201', FWatchResPrinter.PrintWatchValue(Res, d));

  // thousand-dot
  Res := GetNumRes(QWord(1234567), True, 4);
  d.Num.MinDigits[vdfBaseDecimal] := 0;
  d.Num.SeparatorDec := False;
  AssertEquals('Sep 1234567', '1234567', FWatchResPrinter.PrintWatchValue(Res, d));
  d.Num.SeparatorDec := True;
  AssertEquals('Sep 1234567', '1.234.567', FWatchResPrinter.PrintWatchValue(Res, d));

  d.Num.SeparatorDec := True;
  Res := GetNumRes(QWord(123456), True, 4);
  AssertEquals('Sep 123456', '123.456', FWatchResPrinter.PrintWatchValue(Res, d));
  Res := GetNumRes(QWord(23456), True, 4);
  AssertEquals('Sep 23456', '23.456', FWatchResPrinter.PrintWatchValue(Res, d));


  Res := GetNumRes(QWord(-1201), True, 2);
  d.Num.SeparatorDec := False;
  d.Num.MinDigits[vdfBaseDecimal] := 0;
  AssertEquals('', '-1201', FWatchResPrinter.PrintWatchValue(Res, d));

  d.Num.MinDigits[vdfBaseDecimal] := 7;
  AssertEquals('', '-0001201', FWatchResPrinter.PrintWatchValue(Res, d));

  d.Num.MinDigits[vdfBaseDecimal] := -1;
  AssertEquals('', '-01201', FWatchResPrinter.PrintWatchValue(Res, d));

  d.Num.MinDigits[vdfBaseDecimal] := 0;
  d.Num.SeparatorDec              := True;
  AssertEquals('', '-1.201', FWatchResPrinter.PrintWatchValue(Res, d));

  // hex full digit
  Res := GetNumRes(QWord($123456789A), False, 8);
  d.Num.BaseFormat                := vdfBaseHex;
  d.Num.MinDigits[vdfBaseDecimal] := 0;
  AssertEquals('Hex grp', '$000000123456789A', FWatchResPrinter.PrintWatchValue(Res, d));
  d.Num.MinDigits[vdfBaseDecimal] := -1;
  AssertEquals('Hex grp', '$000000123456789A', FWatchResPrinter.PrintWatchValue(Res, d));

  // hex spacing
  d.Num.MinDigits[vdfBaseHex] := 1;
  AssertEquals('Hex grp', '$123456789A', FWatchResPrinter.PrintWatchValue(Res, d));
  d.Num.SeparatorHexBin := vdfhsByte;
  AssertEquals('Hex grp', '$12 34 56 78 9A', FWatchResPrinter.PrintWatchValue(Res, d));
  d.Num.SeparatorHexBin := vdfhsWord;
  AssertEquals('Hex grp', '$12 3456 789A', FWatchResPrinter.PrintWatchValue(Res, d));
  d.Num.SeparatorHexBin := vdfhsLong;
  AssertEquals('Hex grp', '$12 3456789A', FWatchResPrinter.PrintWatchValue(Res, d));


  // digit count oct/bin
  Res := GetNumRes(QWord(201), True, 2);
  d.Num.BaseFormat                := vdfBaseOct;
  AssertEquals('Hex grp', '&311', FWatchResPrinter.PrintWatchValue(Res, d));

  d.Num.MinDigits[vdfBaseOct] := -1;
  AssertEquals('Hex grp', '&000311', FWatchResPrinter.PrintWatchValue(Res, d));

  d.Num.BaseFormat                := vdfBaseBin;
  AssertEquals('Hex grp', '%11001001', FWatchResPrinter.PrintWatchValue(Res, d));

  d.Num.MinDigits[vdfBaseBin] := 12;
  AssertEquals('Hex grp', '%000011001001', FWatchResPrinter.PrintWatchValue(Res, d));
end;



initialization

  RegisterTest(TTestWatchResPrinter);
end.

