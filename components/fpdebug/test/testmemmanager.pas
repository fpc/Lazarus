unit TestMemManager;

{$mode objfpc}{$H+}

interface

uses
  FpDbgDwarf, FpDbgUtil, FpdMemoryTools, TestHelperClasses, LazLoggerBase, LazUTF8,
  DbgIntfBaseTypes, sysutils, fpcunit, testregistry;

type

  { TestDwarfVarious }

  { TTestMemManager }

  TTestMemManager = class(TTestCase)
  protected
    FCurrentTestName: String;
    FMemReader: TTestMemReader;
    FMemConvTarget: TFpDbgMemConvertorLittleEndian;
    FMemConvSelf: TFpDbgMemConvertorLittleEndian;
    FMemManager: TFpDbgMemManager;

    procedure InitMemMgr;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestMemMgr;
  end;

implementation

procedure TTestMemManager.InitMemMgr;
begin
  FMemReader     := TTestMemReader.Create;
  FMemConvTarget := TFpDbgMemConvertorLittleEndian.Create;
  FMemConvSelf   := TFpDbgMemConvertorLittleEndian.Create;
  FMemManager    := TFpDbgMemManager.Create(FMemReader, FMemConvTarget, FMemConvSelf);
end;

procedure TTestMemManager.SetUp;
begin
  inherited SetUp;
  FMemReader     := nil;
  FMemConvTarget := nil;
  FMemConvSelf   := nil;
  FMemManager    := nil;
end;

procedure TTestMemManager.TearDown;
begin
  inherited TearDown;
  FreeAndNil(FMemReader);
  FreeAndNil(FMemConvTarget);
  FreeAndNil(FMemConvSelf);
  FreeAndNil(FMemManager);
end;

procedure TTestMemManager.TestMemMgr;
var
  i, j: Integer;
  TestBaseName: String;
  Data: QWord;
  DataExt: Extended;
  DataDouble: Double;
  DataSingle: Single;
  MemValue: QWord;
  GotRes: Boolean;
  GotInt: Int64;
  GotUInt: QWord;
  GotAddr: TFpDbgMemLocation;
  GotExt: Extended;


  procedure SetData(Aval:  QWord);
  begin
    Data := Aval;
    FMemReader.RegisterValues[2] := Aval;
    FCurrentTestName := TestBaseName + ' ' + IntToHex(Aval, 16) + ': ';
  end;
  procedure CheckIntRes(AName: String; AExp: int64);
  begin
    AssertTrue(FCurrentTestName + AName + 'Read OK', GotRes);
    AssertEquals(FCurrentTestName + AName + 'Val', AExp, GotInt);
  end;
  procedure CheckUIntRes(AName: String; AExp: int64);
  begin
    AssertTrue(FCurrentTestName + AName + 'Read OK', GotRes);
    AssertEquals(FCurrentTestName + AName + 'Val', AExp, GotUInt);
  end;
  procedure CheckAddrRes(AName: String; AExp: int64);
  begin
    AssertTrue(FCurrentTestName + AName + 'Read OK', GotRes);
    AssertTrue(FCurrentTestName + AName + 'Valid', IsValidLoc(GotAddr));
    AssertEquals(FCurrentTestName + AName + 'Val', AExp, GotAddr.Address);
  end;

  Procedure DoSignedIntTests(ReadSize: Cardinal; ExpIntVal: Int64);
  var
    a: Cardinal;
  begin
    GotRes := FMemManager.ReadSignedInt(TargetLoc(TDbgPtr(@Data)),    ReadSize, GotInt);
    CheckIntRes('signed target', ExpIntVal);

    GotRes := FMemManager.ReadSignedInt(SelfLoc(@Data),               ReadSize, GotInt);
    CheckIntRes('signed self',   ExpIntVal);

    FMemReader.RegisterSizes[2] := ReadSize;
    GotRes := FMemManager.ReadSignedInt(RegisterLoc(2),               ReadSize, GotInt);
    CheckIntRes('signed Reg ',    ExpIntVal);

    for a := ReadSize+1 to 8 do begin
      // expanded
      FMemReader.RegisterSizes[2] := ReadSize;
      GotRes := FMemManager.ReadSignedInt(RegisterLoc(2),             a, GotInt);
      CheckIntRes('signed Reg  readsize='+IntToStr(a),    ExpIntVal);

      FMemReader.RegisterSizes[2] := a;
      GotRes := FMemManager.ReadSignedInt(RegisterLoc(2),             ReadSize, GotInt);
      CheckIntRes('signed Reg  regsize'+IntToStr(a),    ExpIntVal);
    end;

    GotRes := FMemManager.ReadSignedInt(ConstLoc(QWord(ExpIntVal)), ReadSize, GotInt);
    CheckIntRes('signed const (pre-expanded)', ExpIntVal);
  end;

  Procedure DoUnsignedIntTests(ReadSize: Cardinal; ExpIntVal: QWord);
  var
    a: Cardinal;
  begin
    GotRes := FMemManager.ReadUnsignedInt(TargetLoc(TDbgPtr(@Data)),    ReadSize, GotUInt);
    CheckUIntRes('unsigned target', ExpIntVal);

    GotRes := FMemManager.ReadUnsignedInt(SelfLoc(@Data),               ReadSize, GotUInt);
    CheckUIntRes('unsigned self',   ExpIntVal);

    FMemReader.RegisterSizes[2] := ReadSize;
    GotRes := FMemManager.ReadUnsignedInt(RegisterLoc(2),               ReadSize, GotUInt);
    CheckUIntRes('unsigned Reg ',    ExpIntVal);

    for a := ReadSize+1 to 8 do begin
      // expanded
      FMemReader.RegisterSizes[2] := ReadSize;
      GotRes := FMemManager.ReadUnsignedInt(RegisterLoc(2),             a, GotUInt);
      CheckUIntRes('unsigned Reg  readsize='+IntToStr(a),    ExpIntVal);

      FMemReader.RegisterSizes[2] := a;
      GotRes := FMemManager.ReadUnsignedInt(RegisterLoc(2),             ReadSize, GotUInt);
      CheckUIntRes('unsigned Reg  regsize'+IntToStr(a),    ExpIntVal);
    end;

    GotRes := FMemManager.ReadUnsignedInt(ConstLoc(QWord(ExpIntVal)), ReadSize, GotUInt);
    CheckUIntRes('unsigned const (pre-expanded)', ExpIntVal);

    //////
    // Address
    GotRes := FMemManager.ReadAddress(TargetLoc(TDbgPtr(@Data)),    ReadSize, GotAddr);
    CheckAddrRes('addr target', ExpIntVal);

    GotRes := FMemManager.ReadAddress(SelfLoc(@Data),               ReadSize, GotAddr);
    CheckAddrRes('addr self',   ExpIntVal);

    FMemReader.RegisterSizes[2] := ReadSize;
    GotRes := FMemManager.ReadAddress(RegisterLoc(2),               ReadSize, GotAddr);
    CheckAddrRes('addr Reg ',    ExpIntVal);

    for a := ReadSize+1 to 8 do begin
      // expanded
      FMemReader.RegisterSizes[2] := ReadSize;
      GotRes := FMemManager.ReadAddress(RegisterLoc(2),             a, GotAddr);
      CheckAddrRes('addr Reg  readsize='+IntToStr(a),    ExpIntVal);

      FMemReader.RegisterSizes[2] := a;
      GotRes := FMemManager.ReadAddress(RegisterLoc(2),             ReadSize, GotAddr);
      CheckAddrRes('addr Reg  regsize'+IntToStr(a),    ExpIntVal);
    end;

    GotRes := FMemManager.ReadAddress(ConstLoc(QWord(ExpIntVal)), ReadSize, GotAddr);
    CheckAddrRes('addr const (pre-expanded)', ExpIntVal);

    //////
    // Address
    GotAddr := FMemManager.ReadAddress(TargetLoc(TDbgPtr(@Data)),    ReadSize);
    GotRes := isValidLoc(GotAddr);
    CheckAddrRes('addr target', ExpIntVal);

    GotAddr := FMemManager.ReadAddress(SelfLoc(@Data),               ReadSize);
    GotRes := isValidLoc(GotAddr);
    CheckAddrRes('addr self',   ExpIntVal);

    FMemReader.RegisterSizes[2] := ReadSize;
    GotAddr := FMemManager.ReadAddress(RegisterLoc(2),               ReadSize);
    GotRes := isValidLoc(GotAddr);
    CheckAddrRes('addr Reg ',    ExpIntVal);

    for a := ReadSize+1 to 8 do begin
      // expanded
      FMemReader.RegisterSizes[2] := ReadSize;
      GotAddr := FMemManager.ReadAddress(RegisterLoc(2),             a);
      GotRes := isValidLoc(GotAddr);
      CheckAddrRes('addr Reg  readsize='+IntToStr(a),    ExpIntVal);

      FMemReader.RegisterSizes[2] := a;
      GotAddr := FMemManager.ReadAddress(RegisterLoc(2),             ReadSize);
      GotRes := isValidLoc(GotAddr);
      CheckAddrRes('addr Reg  regsize'+IntToStr(a),    ExpIntVal);
    end;

    GotAddr := FMemManager.ReadAddress(ConstLoc(QWord(ExpIntVal)), ReadSize);
    GotRes := isValidLoc(GotAddr);
    CheckAddrRes('addr const (pre-expanded)', ExpIntVal);

  end;
begin
  InitMemMgr;

  TestBaseName := 'size 1';
  SetData($00);  DoSignedIntTests(1,   0);
  SetData($08);  DoSignedIntTests(1,   8);
  SetData($7f);  DoSignedIntTests(1, 127);
  SetData($FB);  DoSignedIntTests(1,  -5);
  SetData($80);  DoSignedIntTests(1,-128);
  SetData($FF);  DoSignedIntTests(1,  -1);
  SetData($0108);  DoSignedIntTests(1,   8);

  TestBaseName := 'size 2';
  SetData($0000);  DoSignedIntTests(2,   0);
  SetData($0108);  DoSignedIntTests(2, 264);
  SetData($00FB);  DoSignedIntTests(2, 251);
  SetData($FFFB);  DoSignedIntTests(2,  -5);
  SetData($010208);  DoSignedIntTests(2, 520);

  TestBaseName := 'size 8';
  SetData($7FAAFFBBFFCCFFDD);  DoSignedIntTests(8, $7FAAFFBBFFCCFFDD);
  SetData(QWord(-3));  DoSignedIntTests(8,  -3);


  TestBaseName := 'size 1';
  SetData($00);  DoUnsignedIntTests(1,   0);
  SetData($08);  DoUnsignedIntTests(1,   8);
  SetData($7f);  DoUnsignedIntTests(1, 127);
  SetData($FB);  DoUnsignedIntTests(1, 251);
  SetData($80);  DoUnsignedIntTests(1, 128);
  SetData($FF);  DoUnsignedIntTests(1, 255);
  SetData($0108);  DoSignedIntTests(1,   8);

  FCurrentTestName := 'Extended';
  DataExt := 1.7722;
  GotRes := FMemManager.ReadFloat(TargetLoc(TDbgPtr(@DataExt)), SizeOf(Extended), GotExt);
  AssertTrue(FCurrentTestName +  'Read OK', GotRes);
  AssertEquals(FCurrentTestName + 'target not changed', 1.7722, DataExt);
  AssertEquals(FCurrentTestName + 'Val', DataExt, GotExt);

  FCurrentTestName := 'Double';
  DataDouble := 1.7722;
  GotRes := FMemManager.ReadFloat(TargetLoc(TDbgPtr(@DataDouble)), SizeOf(Double), GotExt);
  AssertTrue(FCurrentTestName +  'Read OK', GotRes);
  AssertEquals(FCurrentTestName + 'target not changed', 1.7722, DataDouble);
  AssertEquals(FCurrentTestName + 'Val', DataDouble, GotExt);

  FCurrentTestName := 'Single';
  DataSingle := 1.7722;
  GotRes := FMemManager.ReadFloat(TargetLoc(TDbgPtr(@DataSingle)), SizeOf(Single), GotExt);
  AssertTrue(FCurrentTestName +  'Read OK', GotRes);
  AssertEquals(FCurrentTestName + 'target not changed', 1.7722, DataSingle);
  AssertEquals(FCurrentTestName + 'Val', DataSingle, GotExt);


end;

initialization

  RegisterTest(TTestMemManager);
end.

