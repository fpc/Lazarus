unit TestWatchResult;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Debugger,
  fpcunit, testutils, testregistry,
  IdeDebuggerWatchResult, LazDebuggerIntf, Laz2_XMLCfg, LazLogger;

const
  SkipSubTestCopy = False; // Skip CreateCopy in Assert....
  SkipSubTestSave = False; // Skip SaveLoad in Assert....
  DebuglnXmlStream = False;

type

  { TTestWatchResWrapper }

  TTestWatchResWrapper = object
  private
    FCurrentRes: TCurrentResData;
    FIdeRes: TWatchResultData;
    function GetIdeRes: TWatchResultData;
    function GetResIntf: TCurrentResData;
  public
    procedure Init;
    procedure Done;
    property ResIntf: TCurrentResData read GetResIntf;
    property IdeRes: TWatchResultData read GetIdeRes;
  end;

  { TTestIdeDebuggerWatchResult }

  TTestIdeDebuggerWatchResult = class(TTestCase)
  private type
    TTwoResRecord = record
      NestPtr, NestNum: TLzDbgWatchDataIntf;
    end;
    TTestCreateDataKind = (
      cdErrNum,
      cdErrPre,
      cdPtr_ErrNum, cdErrPtr_Num,
      cdPtr_Ptr_ErrNum, cdPtr_ErrPtr_Num, cdErrPtr_Ptr_Num,
      cdErrArr_Num, cdArr_EmptyNum, cdArr_ErrNum,
      cdPtr_ErrArr_Num,
      cdErrStruct, cdStruct_ErrField,
      cdStruct_Nil,   // Instead of error, create struct, but no fields / no field-data
      cdStruct_ErrEmptyFields
      //cdStruct_EmptyFields // Add fields, but instead of error, do not create data for the fields
    );
  const
    SecondType: array [TTestCreateDataKind] of array [0..1] of TTestCreateDataKind = (
      (cdErrNum,         cdErrNum),    // cdErrNum,
      (cdErrPre,         cdErrPre),    // cdErrNum,
      (cdPtr_ErrNum,     cdErrPtr_Num),    // cdPtr_ErrNum,
      (cdPtr_ErrNum,     cdErrPtr_Num),    // cdErrPtr_Num,
      (cdPtr_Ptr_ErrNum, cdErrPtr_Ptr_Num),    // cdPtr_Ptr_ErrNum,
      (cdPtr_Ptr_ErrNum, cdErrPtr_Ptr_Num),    // cdPtr_ErrPtr_Num,
      (cdPtr_Ptr_ErrNum, cdErrPtr_Ptr_Num),    // cdErrPtr_Ptr_Num
      (cdErrArr_Num,     cdArr_ErrNum),        // cdErrArr_Num
      (cdErrArr_Num,     cdArr_ErrNum),        // cdArr_EmptyNum
      (cdErrArr_Num,     cdArr_ErrNum),        // cdArr_ErrNum
      (cdPtr_ErrArr_Num, cdPtr_ErrArr_Num),    // cdPtr_ErrArr_Num
      (cdErrStruct,      cdStruct_Nil),        // cdErrStruct
      (cdErrStruct,      cdStruct_Nil),        // cdStruct_ErrField
      (cdErrStruct,      cdStruct_Nil),        // cdStruct_Nil
      (cdStruct_ErrEmptyFields,cdStruct_ErrEmptyFields) //cdStruct_ErrEmptyFields
      //(cdStruct_EmptyFields, cdStruct_EmptyFields)  // cdStruct_EmptyFields
    );
  protected
    class procedure AssertEquals(const AMessage: string; Expected, Actual: TWatchResultDataKind); overload;

    procedure AssertValKind(const AMessage: string; IdeRes: TWatchResultData;
      ExpKind: TWatchResultDataKind);
    procedure AssertTypeName(const AMessage: string; IdeRes: TWatchResultData;
      ExpTypeName: String = #1
    );


    procedure AssertNumData(const AMessage: string; IdeRes: TWatchResultData;
      ExpKind: TWatchResultDataKind; ExpInt64: Int64; ExpQW: QWord;
      ExpStr: String;
      ExpNumByte: Integer;
      ExpTypeName: String = #1;
      ASaveLoad: Boolean = True; ACreateCopy: Boolean = True
    );
    procedure AssertNumDataSigned(const AMessage: string; IdeRes: TWatchResultData;
      ExpInt64: Int64; ExpNumByte: Integer;
      ExpTypeName: String = #1;
      ASaveLoad: Boolean = True; ACreateCopy: Boolean = True
    );
    procedure AssertErrData(const AMessage: string; IdeRes: TWatchResultData;
      ExpErr: String;
      //ExpTypeName: String = #1;
      ASaveLoad: Boolean = True; ACreateCopy: Boolean = True
    );
    procedure AssertPrePrintData(const AMessage: string; IdeRes: TWatchResultData;
      ExpStr: String;
      ExpTypeName: String = #1;
      ASaveLoad: Boolean = True; ACreateCopy: Boolean = True
    );

    procedure AssertPointerData(const AMessage: string; IdeRes: TWatchResultData;
      ExpAddr: QWord;
      ExpHasDeref: Boolean;
      ExpTypeName: String = #1;
      ASaveLoad: Boolean = True; ACreateCopy: Boolean = True
    );
    procedure AssertPointerToPrePrintData(const AMessage: string; IdeRes: TWatchResultData;
      ExpAddr: QWord;
      ExpStr: String;
      ExpTypeName: String = #1;
      ExpStrTypeName: String = #1;
      ASaveLoad: Boolean = True; ACreateCopy: Boolean = True
    );
    procedure AssertPointerToErrData(const AMessage: string; IdeRes: TWatchResultData;
      ExpAddr: QWord;
      ExpErr: String;
      ExpTypeName: String = #1;
      ASaveLoad: Boolean = True; ACreateCopy: Boolean = True
    );
    procedure AssertPointerToSignedNumData(const AMessage: string; IdeRes: TWatchResultData;
      ExpAddr: QWord;
      ExpInt64: Int64; ExpNumByte: Integer;
      ExpTypeName: String = #1;
      ExpNumTypeName: String = #1;
      ASaveLoad: Boolean = True; ACreateCopy: Boolean = True
    );
    procedure AssertPtrPointerToErrData(const AMessage: string; IdeRes: TWatchResultData;
      ExpAddr, ExpNestAddr: QWord;
      ExpErr: String;
      ExpTypeName: String = #1; ExpNestTypeName: String = #1;
      ASaveLoad: Boolean = True; ACreateCopy: Boolean = True
    );
    procedure AssertPtrPointerToSignedNumData(const AMessage: string; IdeRes: TWatchResultData;
      ExpAddr, ExpNestAddr: QWord;
      ExpInt64: Int64; ExpNumByte: Integer;
      ExpTypeName: String = #1; ExpNestTypeName: String = #1;
      ExpNumTypeName: String = #1;
      ASaveLoad: Boolean = True; ACreateCopy: Boolean = True
    );
    procedure AssertArrayOfNumData(const AMessage: string; IdeRes: TWatchResultData;
      ExpNum: Int64;
      ExpTypeName: String = #1;
      ASaveLoad: Boolean = True; ACreateCopy: Boolean = True
    );
    procedure AssertArrayOfErrData(const AMessage: string; IdeRes: TWatchResultData;
      ExpAnErr: String;
      ASaveLoad: Boolean = True; ACreateCopy: Boolean = True
    );
    procedure AssertEmptyArrayOfNumData(const AMessage: string; IdeRes: TWatchResultData;
      ExpNum: Int64;
      ExpTypeName: String = #1;
      ASaveLoad: Boolean = True; ACreateCopy: Boolean = True
    );
    procedure AssertPtrArrayOfNumData(const AMessage: string; IdeRes: TWatchResultData;
      ExpNum: Int64;
      ExpTypeName: String = #1;
      ASaveLoad: Boolean = True; ACreateCopy: Boolean = True
    );

    procedure AssertArrayData(const AMessage: string; IdeRes: TWatchResultData;
      ExpArrayType: TLzDbgArrayType;
      ExpLength: Int64;
      ExpLowIdxOrAddr: QWord;
      ExpTypeName: String = #1;
      ASaveLoad: Boolean = True; ACreateCopy: Boolean = True
    );


    procedure AssertStructField(const AMessage: string; IdeRes: TWatchResultData;
      TestFieldNum: Integer;
      ExpName: String;
      ExpVisibilty: TLzDbgFieldVisibility;
      ExpFlags: TLzDbgFieldFlags;
      ExpAnchTypeName: String = #1;
      ExpTypeName: String = #1;
      ExpKind: TWatchResultDataKind = rdkUnknown;
      ASaveLoad: Boolean = True; ACreateCopy: Boolean = True;
      aOnlyFieldData: Boolean = False
    );

    procedure AssertStructData(const AMessage: string; IdeRes: TWatchResultData;
      ExpType: TLzDbgStructType;
      ExpAddr: QWord;
      ExpFieldCnt: Integer;
      ExpTypeName: String = #1;
      ASaveLoad: Boolean = True; ACreateCopy: Boolean = True
    );
    procedure AssertStructAnch(const AMessage: string; IdeRes: TWatchResultData;
      ExpType: TLzDbgStructType;
      ExpDirectFieldCnt: Integer;
      ExpTypeName: String = #1;
      ASaveLoad: Boolean = True; ACreateCopy: Boolean = True
    );

    procedure AssertStruct(const AMessage: string; IdeRes: TWatchResultData;
                          StrctTyp: TLzDbgStructType;
                          WithFld: Boolean;
                          WithAnch: Integer;
                          WithAnch1Fld, WithAnch2Fld: Boolean;
                          aEntryType1, aEntryType2: TTestCreateDataKind;
                          aErr1, aErr2: Boolean;
                          aNil: Boolean = False;
                          ExpTypeName: String = '';
                          aOnlyFieldData: Boolean = False
                         );

    function SaveLoad(ARes: TWatchResultData): TWatchResultData;

    function CreatePointer(ResIntf: TLzDbgWatchDataIntf; AnAddr: QWord; ATypeName: String=''; ASkipSetDeref: boolean = False): TLzDbgWatchDataIntf;

    function CreatePtrPrePrint(ResIntf: TLzDbgWatchDataIntf; AnAddr: QWord; AStr: String): TLzDbgWatchDataIntf;
    function CreatePtrNum(ResIntf: TLzDbgWatchDataIntf; AnAddr: QWord; ANum: Int64; AByteSize: integer = 4): TLzDbgWatchDataIntf;
    function CreatePtrErr(ResIntf: TLzDbgWatchDataIntf; AnAddr: QWord; AnErr: String): TLzDbgWatchDataIntf;
    function CreatePtrPtrNum(ResIntf: TLzDbgWatchDataIntf; AnAddr: QWord; ANum: Int64; AByteSize: integer = 2): TTwoResRecord;
    function CreatePtrPtrErr(ResIntf: TLzDbgWatchDataIntf; AnAddr: QWord; AnErr: String): TTwoResRecord;
    function CreateArrayOfNum(ResIntf: TLzDbgWatchDataIntf; ANum: Int64; AByteSize: integer = 2): TTwoResRecord;
    function CreateArrayOfErr(ResIntf: TLzDbgWatchDataIntf; AnErr: String): TTwoResRecord;
    function CreateEmptyArrayOfNum(ResIntf: TLzDbgWatchDataIntf; ANum: Int64; AByteSize: integer = 2): TTwoResRecord;
    function CreatePtrArrayOfNum(ResIntf: TLzDbgWatchDataIntf; ANum: Int64; AByteSize: integer = 2): TTwoResRecord;
    procedure CreateStruct(ResIntf: TLzDbgWatchDataIntf;
                          StrctTyp: TLzDbgStructType;
                          WithFld: Boolean;
                          WithAnch: Integer;
                          WithAnch1Fld, WithAnch2Fld: Boolean;
                          aEntryType1, aEntryType2: TTestCreateDataKind;
                          aErr1, aErr2: Boolean;
                          aNil: Boolean = False;
                          aOnlyFieldData: Boolean = False
                         );

    function  CreateData(ResIntf: TLzDbgWatchDataIntf;
                         AKind: TTestCreateDataKind; AnErr: Boolean;
                         ATypeName: String = '';
                         ANumVal: Int64 = 200;
                         AnAddr: QWord = 990;
                         AnErrPreFix: String = ''
                        ): TTwoResRecord;
    procedure AssertData(const AMessage: string; IdeRes: TWatchResultData;
                         AKind: TTestCreateDataKind; AnErr: Boolean;
                         ATypeName: String = '';
                         ANumVal: Int64 = 200;
                         AnAddr: QWord = 990;
                         AnErrPreFix: String = ''
                        );

  published
    procedure TestWatchResError;
    procedure TestPrePrint;
    procedure TestWatchResNum;
    procedure TestWatchPointer;
    procedure TestWatchResPCharOrString;
    procedure TestWatchResPCharOrStringWithPtr;
    procedure TestWatchResPCharOrStringWithPtrPtr;
    procedure TestWatchResPCharOrStringWithArray;
    procedure TestWatchArray;
    procedure TestWatchArrayNested;
    procedure TestWatchStuct;
    procedure TestWatchStuctNested;
    procedure TestWatchArrayStuct;
    procedure TestWatchArrayStuctArrayStuct;

  end;

implementation

{ TTestWatchResWrapper }

function TTestWatchResWrapper.GetIdeRes: TWatchResultData;
begin
  if FIdeRes = nil then begin
    FCurrentRes := FCurrentRes.RootResultData;
//FCurrentRes.DebugPrint('BEFORE');
    FCurrentRes.Done;
//FCurrentRes.DebugPrint('AFTER');
    FIdeRes := FCurrentRes.NewResultData;
    FreeAndNil(FCurrentRes);
  end;
  Result := FIdeRes;
end;

function TTestWatchResWrapper.GetResIntf: TCurrentResData;
begin
  if FCurrentRes = nil then
    FCurrentRes := TCurrentResData.Create;
  Result := FCurrentRes;
end;

procedure TTestWatchResWrapper.Init;
begin
  FCurrentRes := nil;
  FIdeRes := Nil;
end;

procedure TTestWatchResWrapper.Done;
begin
  FreeAndNil(FCurrentRes);
  FreeAndNil(FIdeRes);
end;

{ TTestIdeDebuggerWatchResult }

class procedure TTestIdeDebuggerWatchResult.AssertEquals(
  const AMessage: string; Expected, Actual: TWatchResultDataKind);
begin
  AssertEquals(AMessage, dbgs(Expected), dbgs(Actual));
end;

procedure TTestIdeDebuggerWatchResult.AssertValKind(const AMessage: string;
  IdeRes: TWatchResultData; ExpKind: TWatchResultDataKind);
begin
  AssertTrue(AMessage+': not nil', IdeRes <> nil);
  AssertEquals(AMessage, ExpKind, IdeRes.ValueKind);
end;

procedure TTestIdeDebuggerWatchResult.AssertTypeName(const AMessage: string;
  IdeRes: TWatchResultData; ExpTypeName: String);
begin
  AssertTrue(AMessage+': not nil', IdeRes <> nil);
  if ExpTypeName <> #1 then
    AssertEquals(AMessage, ExpTypeName, IdeRes.TypeName);
end;

procedure TTestIdeDebuggerWatchResult.AssertNumData(const AMessage: string;
  IdeRes: TWatchResultData; ExpKind: TWatchResultDataKind; ExpInt64: Int64;
  ExpQW: QWord; ExpStr: String; ExpNumByte: Integer; ExpTypeName: String;
  ASaveLoad: Boolean; ACreateCopy: Boolean);
var
  t: TWatchResultData;
begin
  AssertTrue(AMessage+': not nil', IdeRes <> nil);
  AssertEquals(AMessage + ': VKind',  ExpKind,    IdeRes.ValueKind);
  AssertEquals(AMessage + ': Int64',  ExpInt64,   IdeRes.AsInt64);
  AssertEquals(AMessage + ': QWord',  ExpQW,      IdeRes.AsQWord);
  AssertEquals(AMessage + ': String', ExpStr,     IdeRes.AsString);
  AssertEquals(AMessage + ': BSize',  ExpNumByte, IdeRes.ByteSize);

  AssertTypeName(AMessage + ': TypeName',  IdeRes, ExpTypeName);

  if ASaveLoad and not SkipSubTestSave then begin
    t := SaveLoad(IdeRes);
    AssertNumData(AMessage, IdeRes, ExpKind, ExpInt64, ExpQW, ExpStr, ExpNumByte, ExpTypeName, False, False);
    t.Free;
  end;
  if ACreateCopy and not SkipSubTestCopy then begin
    t := IdeRes.CreateCopy;
    AssertNumData(AMessage, IdeRes, ExpKind, ExpInt64, ExpQW, ExpStr, ExpNumByte, ExpTypeName, False, False);
    t.Free;
  end;
end;

procedure TTestIdeDebuggerWatchResult.AssertNumDataSigned(
  const AMessage: string; IdeRes: TWatchResultData; ExpInt64: Int64;
  ExpNumByte: Integer; ExpTypeName: String; ASaveLoad: Boolean;
  ACreateCopy: Boolean);
begin
  AssertNumData(AMessage, IdeRes, rdkSignedNumVal, ExpInt64, QWord(ExpInt64), IntToStr(ExpInt64), ExpNumByte, ExpTypeName, ASaveLoad, ACreateCopy);
end;

procedure TTestIdeDebuggerWatchResult.AssertErrData(const AMessage: string;
  IdeRes: TWatchResultData; ExpErr: String; ASaveLoad: Boolean;
  ACreateCopy: Boolean);
var
  t: TWatchResultData;
begin
  AssertTrue(AMessage+': not nil', IdeRes <> nil);
  AssertEquals(AMessage + ': VKind', rdkError, IdeRes.ValueKind);
  AssertEquals(AMessage + ': Err',   ExpErr,   IdeRes.AsString);

  if ASaveLoad and not SkipSubTestSave then begin
    t := SaveLoad(IdeRes);
    AssertErrData(AMessage, IdeRes, ExpErr, False, False);
    t.Free;
  end;
  if ACreateCopy and not SkipSubTestCopy then begin
    t := IdeRes.CreateCopy;
    AssertErrData(AMessage, IdeRes, ExpErr, False, False);
    t.Free;
  end;
end;

procedure TTestIdeDebuggerWatchResult.AssertPrePrintData(
  const AMessage: string; IdeRes: TWatchResultData; ExpStr: String;
  ExpTypeName: String; ASaveLoad: Boolean; ACreateCopy: Boolean);
var
  t: TWatchResultData;
begin
  AssertTrue(AMessage+': not nil', IdeRes <> nil);
  AssertEquals(AMessage + ': VKind',  rdkPrePrinted, IdeRes.ValueKind);
  AssertEquals(AMessage + ': String', ExpStr,        IdeRes.AsString);

  AssertTypeName(AMessage + ': TypeName',  IdeRes, ExpTypeName);

  if ASaveLoad and not SkipSubTestSave then begin
    t := SaveLoad(IdeRes);
    AssertPrePrintData(AMessage, IdeRes, ExpStr, ExpTypeName, False, False);
    t.Free;
  end;
  if ACreateCopy and not SkipSubTestCopy then begin
    t := IdeRes.CreateCopy;
    AssertPrePrintData(AMessage, IdeRes, ExpStr, ExpTypeName, False, False);
    t.Free;
  end;
end;

procedure TTestIdeDebuggerWatchResult.AssertPointerData(const AMessage: string;
  IdeRes: TWatchResultData; ExpAddr: QWord; ExpHasDeref: Boolean;
  ExpTypeName: String; ASaveLoad: Boolean; ACreateCopy: Boolean);
var
  t: TWatchResultData;
begin
  AssertTrue(AMessage+': not nil', IdeRes <> nil);
  AssertEquals(AMessage + ': VKind',    rdkPointerVal, IdeRes.ValueKind);
  AssertEquals(AMessage + ': Addr',     ExpAddr,       IdeRes.AsQWord);
  AssertEquals(AMessage + ': HasDeref', ExpHasDeref,   IdeRes.DerefData<>nil);

  AssertTypeName(AMessage + ': TypeName',  IdeRes, ExpTypeName);

  if ASaveLoad and not SkipSubTestSave then begin
    t := SaveLoad(IdeRes);
    AssertPointerData(AMessage, IdeRes, ExpAddr, ExpHasDeref, ExpTypeName, False, False);
    t.Free;
  end;
  if ACreateCopy and not SkipSubTestCopy then begin
    t := IdeRes.CreateCopy;
    AssertPointerData(AMessage, IdeRes, ExpAddr, ExpHasDeref, ExpTypeName, False, False);
    t.Free;
  end;
end;

procedure TTestIdeDebuggerWatchResult.AssertPointerToPrePrintData(
  const AMessage: string; IdeRes: TWatchResultData; ExpAddr: QWord;
  ExpStr: String; ExpTypeName: String; ExpStrTypeName: String;
  ASaveLoad: Boolean; ACreateCopy: Boolean);
var
  t: TWatchResultData;
begin
  AssertTrue(AMessage+': not nil', IdeRes <> nil);
  AssertEquals(AMessage + ': VKind',    rdkPointerVal, IdeRes.ValueKind);
  AssertEquals(AMessage + ': Addr',     ExpAddr,       IdeRes.AsQWord);
  AssertEquals(AMessage + ': HasDeref', True,          IdeRes.DerefData<>nil);

  AssertTypeName(AMessage + ': TypeName',  IdeRes, ExpTypeName);

  AssertPrePrintData(AMessage + ': Deref', IdeRes.DerefData, ExpStr, ExpStrTypeName, ASaveLoad, ACreateCopy);

  if ASaveLoad and not SkipSubTestSave then begin
    t := SaveLoad(IdeRes);
    AssertPointerToPrePrintData(AMessage, IdeRes, ExpAddr, ExpStr, ExpTypeName, ExpStrTypeName, False, False);
    t.Free;
  end;
  if ACreateCopy and not SkipSubTestCopy then begin
    t := IdeRes.CreateCopy;
    AssertPointerToPrePrintData(AMessage, IdeRes, ExpAddr, ExpStr, ExpTypeName, ExpStrTypeName, False, False);
    t.Free;
  end;
end;

procedure TTestIdeDebuggerWatchResult.AssertPointerToErrData(
  const AMessage: string; IdeRes: TWatchResultData; ExpAddr: QWord;
  ExpErr: String; ExpTypeName: String; ASaveLoad: Boolean; ACreateCopy: Boolean
  );
var
  t: TWatchResultData;
begin
  AssertTrue(AMessage+': not nil', IdeRes <> nil);
  AssertEquals(AMessage + ': VKind',    rdkPointerVal, IdeRes.ValueKind);
  AssertEquals(AMessage + ': Addr',     ExpAddr,       IdeRes.AsQWord);
  AssertEquals(AMessage + ': HasDeref', True,          IdeRes.DerefData<>nil);

  AssertTypeName(AMessage + ': TypeName',  IdeRes, ExpTypeName);

  AssertErrData(AMessage + ': Deref', IdeRes.DerefData, ExpErr, ASaveLoad, ACreateCopy);

  if ASaveLoad and not SkipSubTestSave then begin
    t := SaveLoad(IdeRes);
    AssertPointerToErrData(AMessage, IdeRes, ExpAddr, ExpErr, ExpTypeName, False, False);
    t.Free;
  end;
  if ACreateCopy and not SkipSubTestCopy then begin
    t := IdeRes.CreateCopy;
    AssertPointerToErrData(AMessage, IdeRes, ExpAddr, ExpErr, ExpTypeName, False, False);
    t.Free;
  end;
end;

procedure TTestIdeDebuggerWatchResult.AssertPointerToSignedNumData(
  const AMessage: string; IdeRes: TWatchResultData; ExpAddr: QWord;
  ExpInt64: Int64; ExpNumByte: Integer; ExpTypeName: String;
  ExpNumTypeName: String; ASaveLoad: Boolean; ACreateCopy: Boolean);
var
  t: TWatchResultData;
begin
  AssertTrue(AMessage+': not nil', IdeRes <> nil);
  AssertEquals(AMessage + ': VKind',    rdkPointerVal, IdeRes.ValueKind);
  AssertEquals(AMessage + ': Addr',     ExpAddr,       IdeRes.AsQWord);
  AssertEquals(AMessage + ': HasDeref', True,          IdeRes.DerefData<>nil);

  AssertTypeName(AMessage + ': TypeName',  IdeRes, ExpTypeName);

  AssertNumDataSigned(AMessage + ': Deref', IdeRes.DerefData, ExpInt64, ExpNumByte, ExpNumTypeName, ASaveLoad, ACreateCopy);

  if ASaveLoad and not SkipSubTestSave then begin
    t := SaveLoad(IdeRes);
    AssertPointerToSignedNumData(AMessage, IdeRes, ExpAddr, ExpInt64, ExpNumByte, ExpTypeName, ExpNumTypeName, False, False);
    t.Free;
  end;
  if ACreateCopy and not SkipSubTestCopy then begin
    t := IdeRes.CreateCopy;
    AssertPointerToSignedNumData(AMessage, IdeRes, ExpAddr, ExpInt64, ExpNumByte, ExpTypeName, ExpNumTypeName, False, False);
    t.Free;
  end;
end;

procedure TTestIdeDebuggerWatchResult.AssertPtrPointerToErrData(
  const AMessage: string; IdeRes: TWatchResultData; ExpAddr,
  ExpNestAddr: QWord; ExpErr: String; ExpTypeName: String;
  ExpNestTypeName: String; ASaveLoad: Boolean; ACreateCopy: Boolean);
var
  t: TWatchResultData;
begin
  AssertTrue(AMessage+': not nil', IdeRes <> nil);
  AssertEquals(AMessage + ': VKind',    rdkPointerVal, IdeRes.ValueKind);
  AssertEquals(AMessage + ': Addr',     ExpAddr,       IdeRes.AsQWord);
  AssertEquals(AMessage + ': HasDeref', True,          IdeRes.DerefData<>nil);

  AssertTypeName(AMessage + ': TypeName',  IdeRes, ExpTypeName);

  AssertPointerToErrData(AMessage + ': Deref', IdeRes.DerefData, ExpNestAddr, ExpErr, ExpNestTypeName, ASaveLoad, ACreateCopy);

  if ASaveLoad and not SkipSubTestSave then begin
    t := SaveLoad(IdeRes);
    AssertPtrPointerToErrData(AMessage, IdeRes, ExpAddr, ExpNestAddr, ExpErr, ExpTypeName, ExpNestTypeName, False, False);
    t.Free;
  end;
  if ACreateCopy and not SkipSubTestCopy then begin
    t := IdeRes.CreateCopy;
    AssertPtrPointerToErrData(AMessage, IdeRes, ExpAddr, ExpNestAddr, ExpErr, ExpTypeName, ExpNestTypeName, False, False);
    t.Free;
  end;
end;

procedure TTestIdeDebuggerWatchResult.AssertPtrPointerToSignedNumData(
  const AMessage: string; IdeRes: TWatchResultData; ExpAddr,
  ExpNestAddr: QWord; ExpInt64: Int64; ExpNumByte: Integer;
  ExpTypeName: String; ExpNestTypeName: String; ExpNumTypeName: String;
  ASaveLoad: Boolean; ACreateCopy: Boolean);
var
  t: TWatchResultData;
begin
  AssertTrue(AMessage+': not nil', IdeRes <> nil);
  AssertEquals(AMessage + ': VKind',    rdkPointerVal, IdeRes.ValueKind);
  AssertEquals(AMessage + ': Addr',     ExpAddr,       IdeRes.AsQWord);
  AssertEquals(AMessage + ': HasDeref', True,          IdeRes.DerefData<>nil);

  AssertTypeName(AMessage + ': TypeName',  IdeRes, ExpTypeName);

  AssertPointerToSignedNumData(AMessage + ': Deref', IdeRes.DerefData, ExpNestAddr, ExpInt64, ExpNumByte, ExpNestTypeName, ExpNumTypeName, ASaveLoad, ACreateCopy);

  if ASaveLoad and not SkipSubTestSave then begin
    t := SaveLoad(IdeRes);
    AssertPtrPointerToSignedNumData(AMessage, IdeRes, ExpAddr, ExpNestAddr, ExpInt64, ExpNumByte, ExpTypeName, ExpNestTypeName, ExpNumTypeName, False, False);
    t.Free;
  end;
  if ACreateCopy and not SkipSubTestCopy then begin
    t := IdeRes.CreateCopy;
    AssertPtrPointerToSignedNumData(AMessage, IdeRes, ExpAddr, ExpNestAddr, ExpInt64, ExpNumByte, ExpTypeName, ExpNestTypeName, ExpNumTypeName, False, False);
    t.Free;
  end;
end;

procedure TTestIdeDebuggerWatchResult.AssertArrayOfNumData(
  const AMessage: string; IdeRes: TWatchResultData; ExpNum: Int64;
  ExpTypeName: String; ASaveLoad: Boolean; ACreateCopy: Boolean);
var
  t: TWatchResultData;
begin
  AssertArrayData(AMessage, IdeRes, datDynArray, 2, 0, ExpTypeName);
  IdeRes.SetSelectedIndex(0);
  AssertNumDataSigned (AMessage, IdeRes.SelectedEntry, ExpNum, 2);
  IdeRes.SetSelectedIndex(1);
  AssertNumDataSigned (AMessage, IdeRes.SelectedEntry, ExpNum+7, 2);

  if ASaveLoad and not SkipSubTestSave then begin
    t := SaveLoad(IdeRes);
    AssertArrayOfNumData(AMessage, IdeRes, ExpNum, ExpTypeName, False, False);
    t.Free;
  end;
  if ACreateCopy and not SkipSubTestCopy then begin
    t := IdeRes.CreateCopy;
    AssertArrayOfNumData(AMessage, IdeRes, ExpNum, ExpTypeName, False, False);
    t.Free;
  end;
end;

procedure TTestIdeDebuggerWatchResult.AssertArrayOfErrData(
  const AMessage: string; IdeRes: TWatchResultData; ExpAnErr: String;
  ASaveLoad: Boolean; ACreateCopy: Boolean);
var
  t: TWatchResultData;
begin
  AssertArrayData(AMessage, IdeRes, datDynArray, 1, 0);
  IdeRes.SetSelectedIndex(0);
  AssertErrData(AMessage, IdeRes.SelectedEntry, ExpAnErr);

  if ASaveLoad and not SkipSubTestSave then begin
    t := SaveLoad(IdeRes);
    AssertArrayOfErrData(AMessage, IdeRes, ExpAnErr, False, False);
    t.Free;
  end;
  if ACreateCopy and not SkipSubTestCopy then begin
    t := IdeRes.CreateCopy;
    AssertArrayOfErrData(AMessage, IdeRes, ExpAnErr, False, False);
    t.Free;
  end;
end;

procedure TTestIdeDebuggerWatchResult.AssertEmptyArrayOfNumData(
  const AMessage: string; IdeRes: TWatchResultData; ExpNum: Int64;
  ExpTypeName: String; ASaveLoad: Boolean; ACreateCopy: Boolean);
var
  t: TWatchResultData;
begin
  AssertArrayData(AMessage, IdeRes, datDynArray, 0, 0, ExpTypeName);

  if ASaveLoad and not SkipSubTestSave then begin
    t := SaveLoad(IdeRes);
    AssertEmptyArrayOfNumData(AMessage, IdeRes, ExpNum, ExpTypeName, False, False);
    t.Free;
  end;
  if ACreateCopy and not SkipSubTestCopy then begin
    t := IdeRes.CreateCopy;
    AssertEmptyArrayOfNumData(AMessage, IdeRes, ExpNum, ExpTypeName, False, False);
    t.Free;
  end;
end;

procedure TTestIdeDebuggerWatchResult.AssertPtrArrayOfNumData(
  const AMessage: string; IdeRes: TWatchResultData; ExpNum: Int64;
  ExpTypeName: String; ASaveLoad: Boolean; ACreateCopy: Boolean);
var
  t: TWatchResultData;
begin
  AssertTrue(AMessage+': not nil', IdeRes <> nil);
  AssertEquals(AMessage + ': VKind',    rdkPointerVal, IdeRes.ValueKind);
  AssertEquals(AMessage + ': Addr',     1500,       IdeRes.AsQWord);
  AssertEquals(AMessage + ': HasDeref', True,          IdeRes.DerefData<>nil);

  AssertTypeName(AMessage + ': TypeName',  IdeRes, ExpTypeName);

  AssertArrayOfNumData(AMessage, IdeRes.DerefData, ExpNum, #1, False, False);

  if ASaveLoad and not SkipSubTestSave then begin
    t := SaveLoad(IdeRes);
    AssertPtrArrayOfNumData(AMessage, IdeRes, ExpNum, ExpTypeName, False, False);
    t.Free;
  end;
  if ACreateCopy and not SkipSubTestCopy then begin
    t := IdeRes.CreateCopy;
    AssertPtrArrayOfNumData(AMessage, IdeRes, ExpNum, ExpTypeName, False, False);
    t.Free;
  end;
end;

procedure TTestIdeDebuggerWatchResult.AssertArrayData(
  const AMessage: string; IdeRes: TWatchResultData;
  ExpArrayType: TLzDbgArrayType;
  ExpLength: Int64;
  ExpLowIdxOrAddr: QWord;
  ExpTypeName: String;
  ASaveLoad: Boolean; ACreateCopy: Boolean);
var
  t: TWatchResultData;
begin
  AssertTrue(AMessage+': not nil', IdeRes <> nil);
  AssertEquals(AMessage + ': VKind',    rdkArray, IdeRes.ValueKind);
  AssertEquals(AMessage + ': Type',     ord(ExpArrayType),  ord(IdeRes.ArrayType));
  AssertEquals(AMessage + ': Len',      ExpLength,          IdeRes.Count);
  case ExpArrayType of
    datUnknown: ;
    datDynArray:
      AssertEquals(AMessage + ': Addr', ExpLowIdxOrAddr,  IdeRes.DataAddress);
    datStatArray:
      AssertEquals(AMessage + ': Low', int64(ExpLowIdxOrAddr),  IdeRes.LowBound);
  end;

  AssertTypeName(AMessage + ': TypeName',  IdeRes, ExpTypeName);

  if ASaveLoad and not SkipSubTestSave then begin
    t := SaveLoad(IdeRes);
    AssertArrayData(AMessage, IdeRes, ExpArrayType, ExpLength, ExpLowIdxOrAddr, ExpTypeName, False, False);
    t.Free;
  end;
  if ACreateCopy and not SkipSubTestCopy then begin
    t := IdeRes.CreateCopy;
    AssertArrayData(AMessage, IdeRes, ExpArrayType, ExpLength, ExpLowIdxOrAddr, ExpTypeName, False, False);
    t.Free;
  end;
end;

procedure TTestIdeDebuggerWatchResult.AssertStructField(const AMessage: string;
  IdeRes: TWatchResultData; TestFieldNum: Integer; ExpName: String;
  ExpVisibilty: TLzDbgFieldVisibility; ExpFlags: TLzDbgFieldFlags;
  ExpAnchTypeName: String; ExpTypeName: String; ExpKind: TWatchResultDataKind;
  ASaveLoad: Boolean; ACreateCopy: Boolean; aOnlyFieldData: Boolean);
var
  t: TWatchResultData;
  FldData: TWatchResultDataFieldInfo;
begin
  AssertTrue(AMessage+': not nil', IdeRes <> nil);
  AssertTrue(AMessage+': in range', IdeRes.FieldCount > TestFieldNum);

  FldData := IdeRes.Fields[TestFieldNum];
  if not aOnlyFieldData then
    AssertTrue(AMessage+': Field not nil', FldData.Field <> nil);
  AssertTrue(AMessage+': Owner not nil', FldData.Owner <> nil);

  AssertEquals(AMessage+': Field Name',       ExpName, FldData.FieldName);
  AssertEquals(AMessage+': Field Visibility', ord(ExpVisibilty), ord(FldData.FieldVisibility));
  AssertTrue  (AMessage+': Field Flags',      ExpFlags = FldData.FieldFlags);

  if (ExpKind <> rdkUnknown) and
     (not aOnlyFieldData)
  then
    AssertEquals(AMessage + ': VKind',    ExpKind, FldData.Field.ValueKind);

  AssertTypeName(AMessage + ': Anch TypeName',   FldData.Owner, ExpAnchTypeName);
  if (not aOnlyFieldData) and
     (FldData.Field.ValueKind <> rdkError)
  then
    AssertTypeName(AMessage + ': Field TypeName',  FldData.Field, ExpTypeName);


  if ASaveLoad and not SkipSubTestSave then begin
    t := SaveLoad(IdeRes);
    AssertStructField(AMessage, IdeRes, TestFieldNum, ExpName, ExpVisibilty,
      ExpFlags, ExpAnchTypeName, ExpTypeName, ExpKind, False, False);
    t.Free;
  end;
  if ACreateCopy and not SkipSubTestCopy then begin
    t := IdeRes.CreateCopy;
    AssertStructField(AMessage, IdeRes, TestFieldNum, ExpName, ExpVisibilty,
      ExpFlags, ExpAnchTypeName, ExpTypeName, ExpKind, False, False);
    t.Free;
  end;
end;

procedure TTestIdeDebuggerWatchResult.AssertStructData(const AMessage: string;
  IdeRes: TWatchResultData; ExpType: TLzDbgStructType; ExpAddr: QWord;
  ExpFieldCnt: Integer; ExpTypeName: String; ASaveLoad: Boolean;
  ACreateCopy: Boolean);
var
  t: TWatchResultData;
begin
  AssertTrue(AMessage+': not nil', IdeRes <> nil);
  AssertEquals(AMessage + ': VKind',    rdkStruct, IdeRes.ValueKind);
  AssertEquals(AMessage + ': Type',     ord(ExpType),  ord(IdeRes.StructType));

  if ExpType in [dstClass, dstInterface] then
    AssertEquals(AMessage + ': Addr',     ExpAddr,          IdeRes.DataAddress);

  if ExpFieldCnt >= 0 then
    AssertEquals(AMessage + ': Cnt',      ExpFieldCnt,      IdeRes.FieldCount);

  AssertTypeName(AMessage + ': TypeName',  IdeRes, ExpTypeName);

  if ASaveLoad and not SkipSubTestSave then begin
    t := SaveLoad(IdeRes);
    AssertStructData(AMessage, IdeRes, ExpType, ExpAddr, ExpFieldCnt,
      ExpTypeName, False, False);
    t.Free;
  end;
  if ACreateCopy and not SkipSubTestCopy then begin
    t := IdeRes.CreateCopy;
    AssertStructData(AMessage, IdeRes, ExpType, ExpAddr, ExpFieldCnt,
      ExpTypeName, False, False);
    t.Free;
  end;
end;

procedure TTestIdeDebuggerWatchResult.AssertStructAnch(const AMessage: string;
  IdeRes: TWatchResultData; ExpType: TLzDbgStructType;
  ExpDirectFieldCnt: Integer; ExpTypeName: String; ASaveLoad: Boolean;
  ACreateCopy: Boolean);
var
  t: TWatchResultData;
begin
  AssertTrue(AMessage+': not nil', IdeRes <> nil);
  AssertEquals(AMessage + ': VKind',    rdkStruct, IdeRes.ValueKind);
  AssertEquals(AMessage + ': Type',     ord(ExpType),  ord(IdeRes.StructType));

  AssertEquals(AMessage + ': Cnt',      ExpDirectFieldCnt,      IdeRes.DirectFieldCount);

  AssertTypeName(AMessage + ': TypeName',  IdeRes, ExpTypeName);

  if ASaveLoad and not SkipSubTestSave then begin
    t := SaveLoad(IdeRes);
    AssertStructAnch(AMessage, IdeRes, ExpType, ExpDirectFieldCnt,
      ExpTypeName, False, False);
    t.Free;
  end;
  if ACreateCopy and not SkipSubTestCopy then begin
    t := IdeRes.CreateCopy;
    AssertStructAnch(AMessage, IdeRes, ExpType, ExpDirectFieldCnt,
      ExpTypeName, False, False);
    t.Free;
  end;
end;

procedure TTestIdeDebuggerWatchResult.AssertStruct(const AMessage: string;
  IdeRes: TWatchResultData; StrctTyp: TLzDbgStructType; WithFld: Boolean;
  WithAnch: Integer; WithAnch1Fld, WithAnch2Fld: Boolean; aEntryType1,
  aEntryType2: TTestCreateDataKind; aErr1, aErr2: Boolean; aNil: Boolean;
  ExpTypeName: String; aOnlyFieldData: Boolean);
var
  ExpCnt, ExpOffs: Integer;
begin
  if ExpTypeName = '' then
    ExpTypeName := 'TMyStruct';
  if aNil then begin
    AssertStructData('', IdeRes, StrctTyp, 0, -1, ExpTypeName);
    exit;
  end;

  ExpCnt := 0;
  if WithFld then ExpCnt := ExpCnt + 3;
  if WithAnch1Fld and (WithAnch >= 1) then ExpCnt := ExpCnt + 2;
  if WithAnch2Fld and (WithAnch >= 2) then ExpCnt := ExpCnt + 1;

  AssertStructData('', IdeRes, StrctTyp, 700, ExpCnt, ExpTypeName);
  ExpOffs := 0;
  if WithFld then begin
    if aErr1 and not aErr2 then begin
      if not aOnlyFieldData then
        AssertErrData('', IdeRes.Fields[ExpOffs+0].Field, 'bad');
      end
    else begin
      AssertStructField('', IdeRes, ExpOffs+0, 'Foo', dfvProtected, [], ExpTypeName, 'TMyFoo',
        rdkUnknown, True, True, aOnlyFieldData);
    end;

    AssertStructField('', IdeRes, ExpOffs+1, 'Abc', dfvPublic,    [], ExpTypeName, #1, rdkError,
      True, True, aOnlyFieldData);
    if not aOnlyFieldData then
      AssertErrData('', IdeRes.Fields[ExpOffs+1].Field, 'ouch');

    AssertStructField('', IdeRes, ExpOffs+2, 'Bar', dfvPublic,    [], ExpTypeName, 'TMyBar',
      rdkUnknown, True, True, aOnlyFieldData);
    if not aOnlyFieldData then
      AssertData('', IdeRes.Fields[ExpOffs+2].Field, aEntryType2, aErr2, 'TMyBar', 301, 1201);

    ExpOffs := ExpOffs + 3;
  end;

  if WithAnch1Fld and (WithAnch >= 1) then begin
    AssertStructField('', IdeRes, ExpOffs+0, 'P1Abc', dfvPrivate,   [], 'TAnch1', #1, rdkError,
      True, True, aOnlyFieldData);
    if not aOnlyFieldData then
      AssertErrData('', IdeRes.Fields[ExpOffs+0].Field, 'bad');

    AssertStructField('', IdeRes, ExpOffs+1, 'P1Foo', dfvProtected, [], 'TAnch1', 'TMyFoo',
      rdkUnknown, True, True, aOnlyFieldData);
    if not aOnlyFieldData then
      AssertData('', IdeRes.Fields[ExpOffs+1].Field, aEntryType1, False, 'TMyFoo', 310, 1210);
    ExpOffs := ExpOffs + 2;
   end;

  if WithAnch2Fld and (WithAnch >= 2) then begin
    AssertStructField('', IdeRes, ExpOffs+0, 'P2Foo', dfvProtected, [], 'TAnch2', 'TMyXyz',
      rdkUnknown, True, True, aOnlyFieldData);
    if not aOnlyFieldData then
      AssertData('', IdeRes.Fields[ExpOffs+0].Field, aEntryType2, not aErr2, 'TMyXyz', 320, 1220);
    ExpOffs := ExpOffs + 1;
  end;

  if WithAnch >= 1 then
    if WithAnch1Fld then
      AssertStructAnch('', IdeRes.Anchestor, StrctTyp, 2, 'TAnch1')
    else
      AssertStructAnch('', IdeRes.Anchestor, StrctTyp, 0, 'TAnch1');

  if WithAnch >= 2 then
    if WithAnch2Fld then
      AssertStructAnch('', IdeRes.Anchestors[1], StrctTyp, 1, 'TAnch2')
    else
      AssertStructAnch('', IdeRes.Anchestors[1], StrctTyp, 0, 'TAnch2');
end;

function TTestIdeDebuggerWatchResult.SaveLoad(ARes: TWatchResultData
  ): TWatchResultData;
var
  AXmlConf: TXMLConfig;
  s: TStringStream;
begin
  AXmlConf := TXMLConfig.CreateClean('');
  ARes.SaveDataToXMLConfig(AXmlConf, 'foo/');
  if DebuglnXmlStream then begin
    s:= TStringStream.Create();
    AXmlConf.WriteToStream(s);
    DebugLn(s.DataString);
    s.free;
  end;
  Result := TWatchResultData.CreateFromXMLConfig(AXmlConf, 'foo/');
  AXmlConf.Free;
end;

function TTestIdeDebuggerWatchResult.CreatePointer(
  ResIntf: TLzDbgWatchDataIntf; AnAddr: QWord; ATypeName: String;
  ASkipSetDeref: boolean): TLzDbgWatchDataIntf;
begin
  Result := nil;
  ResIntf.CreatePointerValue(AnAddr);
  if ATypeName <> '' then
    ResIntf.SetTypeName(ATypeName);
  if not ASkipSetDeref then
    Result := ResIntf.SetDerefData;
end;

function TTestIdeDebuggerWatchResult.CreatePtrPrePrint(
  ResIntf: TLzDbgWatchDataIntf; AnAddr: QWord; AStr: String
  ): TLzDbgWatchDataIntf;
begin
  ResIntf.CreatePointerValue(AnAddr);
  ResIntf.SetTypeName('TMyPtr');
  Result := ResIntf.SetDerefData;
  Result.CreatePrePrinted(AStr);
  Result.SetTypeName('TMyText');
end;

function TTestIdeDebuggerWatchResult.CreatePtrNum(ResIntf: TLzDbgWatchDataIntf;
  AnAddr: QWord; ANum: Int64; AByteSize: integer): TLzDbgWatchDataIntf;
begin
  ResIntf.CreatePointerValue(AnAddr);
  ResIntf.SetTypeName('TMyPtr');
  Result := ResIntf.SetDerefData;
  Result.CreateNumValue(ANum, True, AByteSize);
  Result.SetTypeName('TMyNum');
end;

function TTestIdeDebuggerWatchResult.CreatePtrErr(ResIntf: TLzDbgWatchDataIntf;
  AnAddr: QWord; AnErr: String): TLzDbgWatchDataIntf;
begin
  ResIntf.CreatePointerValue(AnAddr);
  ResIntf.SetTypeName('TMyPtr');
  Result := ResIntf.SetDerefData;
  Result.CreateError(AnErr);
end;

function TTestIdeDebuggerWatchResult.CreatePtrPtrNum(
  ResIntf: TLzDbgWatchDataIntf; AnAddr: QWord; ANum: Int64; AByteSize: integer
  ): TTwoResRecord;
begin
  ResIntf.CreatePointerValue(AnAddr);
  ResIntf.SetTypeName('TMyPtr');

  Result.NestPtr := ResIntf.SetDerefData;
  Result.NestPtr.CreatePointerValue(AnAddr+1);
  Result.NestPtr.SetTypeName('TFooPtr');

  Result.NestNum := Result.NestPtr.SetDerefData;
  Result.NestNum.CreateNumValue(ANum, True, AByteSize);
  Result.NestNum.SetTypeName('TFooNum');
end;

function TTestIdeDebuggerWatchResult.CreatePtrPtrErr(
  ResIntf: TLzDbgWatchDataIntf; AnAddr: QWord; AnErr: String): TTwoResRecord;
begin
  ResIntf.CreatePointerValue(AnAddr);
  ResIntf.SetTypeName('TMyPtr');

  Result.NestPtr := ResIntf.SetDerefData;
  Result.NestPtr.CreatePointerValue(AnAddr+1);
  Result.NestPtr.SetTypeName('TFooPtr');

  Result.NestNum := Result.NestPtr.SetDerefData;
  Result.NestNum.CreateError(AnErr);
end;

function TTestIdeDebuggerWatchResult.CreateArrayOfNum(ResIntf: TLzDbgWatchDataIntf;
  ANum: Int64; AByteSize: integer): TTwoResRecord;
var
  dat: TLzDbgWatchDataIntf;
begin
  Result.NestPtr := ResIntf.CreateArrayValue(datDynArray, 2);

  Result.NestNum := ResIntf.SetNextArrayData;
  Result.NestNum.CreateNumValue(ANum, True, 2);

  dat := ResIntf.SetNextArrayData;
  dat.CreateNumValue(ANum+7, True, 2);
end;

function TTestIdeDebuggerWatchResult.CreateArrayOfErr(
  ResIntf: TLzDbgWatchDataIntf; AnErr: String): TTwoResRecord;
var
  dat: TLzDbgWatchDataIntf;
begin
  Result.NestPtr := ResIntf.CreateArrayValue(datDynArray, 2);

  Result.NestNum := ResIntf.SetNextArrayData;
  Result.NestNum.CreateError(AnErr);
end;

function TTestIdeDebuggerWatchResult.CreateEmptyArrayOfNum(
  ResIntf: TLzDbgWatchDataIntf; ANum: Int64; AByteSize: integer): TTwoResRecord;
var
  p, dat: TLzDbgWatchDataIntf;
begin
  Result.NestNum := ResIntf.CreateArrayValue(datDynArray, 2);
//  Result.NestNum.CreateNumValue(ANum, True, 2); // proto
end;

function TTestIdeDebuggerWatchResult.CreatePtrArrayOfNum(
  ResIntf: TLzDbgWatchDataIntf; ANum: Int64; AByteSize: integer): TTwoResRecord;
begin
  ResIntf.CreatePointerValue(1500);
  Result.NestPtr := ResIntf.SetDerefData;
  Result.NestNum :=
  CreateArrayOfNum(Result.NestPtr, ANum, AByteSize).NestNum;
end;

procedure TTestIdeDebuggerWatchResult.CreateStruct(
  ResIntf: TLzDbgWatchDataIntf; StrctTyp: TLzDbgStructType; WithFld: Boolean;
  WithAnch: Integer; WithAnch1Fld, WithAnch2Fld: Boolean; aEntryType1,
  aEntryType2: TTestCreateDataKind; aErr1, aErr2: Boolean; aNil: Boolean;
  aOnlyFieldData: Boolean);
var
  ExpCnt: Integer;
  FldIntf, Anch1Intf: TLzDbgWatchDataIntf;
begin
  if aNil then begin
    if StrctTyp in [dstClass, dstInterface] then
      ResIntf.CreateStructure(StrctTyp, 0)
    else
      ResIntf.CreateStructure(StrctTyp);
    ResIntf.SetTypeName('TMyStruct');
    exit;
  end;

  if StrctTyp in [dstClass, dstInterface] then
    ResIntf.CreateStructure(StrctTyp, 700)
  else
    ResIntf.CreateStructure(StrctTyp);
  ResIntf.SetTypeName('TMyStruct');

  ExpCnt := 0;
  if WithFld then begin
    FldIntf := ResIntf.AddField('Foo', dfvProtected, []);
    if not aOnlyFieldData then
      CreateData(FldIntf, aEntryType1, aErr1, 'TMyFoo', 300, 1200);
    //if aErr3 then
    if aErr1 and not aErr2 then
      FldIntf.CreateError('bad');

    FldIntf := ResIntf.AddField('Abc', dfvPublic, []);
    FldIntf.CreateError('ouch');

    FldIntf := ResIntf.AddField('Bar', dfvPublic, []);
    if not aOnlyFieldData then
      CreateData(FldIntf, aEntryType2, aErr2, 'TMyBar', 301, 1201);
    ExpCnt := ExpCnt + 3;
  end;

  if WithAnch > 0 then begin
    Anch1Intf := ResIntf.SetAnchestor('TAnch1');
    if WithAnch1Fld then begin
      FldIntf := Anch1Intf.AddField('P1Abc', dfvPrivate, []);
      FldIntf.CreateError('bad');

      FldIntf := Anch1Intf.AddField('P1Foo', dfvProtected, []);
      if not aOnlyFieldData then
        CreateData(FldIntf, aEntryType1, False, 'TMyFoo', 310, 1210);
      ExpCnt := ExpCnt + 2;
    end;

    if WithAnch = 2 then begin
      Anch1Intf := Anch1Intf.SetAnchestor('TAnch2');

      if WithAnch2Fld then begin
        FldIntf := Anch1Intf.AddField('P2Foo', dfvProtected, []);
        if not aOnlyFieldData then
          CreateData(FldIntf, aEntryType2, not aErr2, 'TMyXyz', 320, 1220);
        ExpCnt := ExpCnt + 1;
      end;
    end;
  end;
end;

function TTestIdeDebuggerWatchResult.CreateData(ResIntf: TLzDbgWatchDataIntf;
  AKind: TTestCreateDataKind; AnErr: Boolean; ATypeName: String;
  ANumVal: Int64; AnAddr: QWord; AnErrPreFix: String): TTwoResRecord;
begin
  Result.NestPtr := ResIntf;
  Result.NestNum := ResIntf;
  if AnErr then
    case AKind of
      cdErrNum:         ResIntf.CreateError(AnErrPreFix+'ouch');
      cdErrPre:         ResIntf.CreateError(AnErrPreFix+'ouch');

      cdPtr_ErrNum:     Result.NestNum :=
                        CreatePtrErr   (ResIntf, AnAddr, AnErrPreFix+'out of cheese');
      cdErrPtr_Num:     ResIntf.CreateError(AnErrPreFix+'ouch');

      cdPtr_Ptr_ErrNum: Result :=
                        CreatePtrPtrErr(ResIntf, AnAddr, AnErrPreFix+'gone wrong');
      cdPtr_ErrPtr_Num: Result.NestNum :=
                        CreatePtrErr   (ResIntf, AnAddr, AnErrPreFix+'out of cheese');
      cdErrPtr_Ptr_Num: ResIntf.CreateError(AnErrPreFix+'ouch');
      cdErrArr_Num:     ResIntf.CreateError(AnErrPreFix+'ouch');
      cdArr_EmptyNum:   Result :=
                        CreateEmptyArrayOfNum(ResIntf, ANumVal);
      cdArr_ErrNum:     Result :=
                        CreateArrayOfErr(ResIntf, AnErrPreFix+'no');
      cdPtr_ErrArr_Num: Result.NestNum :=
                        CreatePtrErr   (ResIntf, AnAddr, AnErrPreFix+'argh');
      cdErrStruct:      ResIntf.CreateError(AnErrPreFix+'bad-obj');
      cdStruct_ErrField:CreateStruct(ResIntf, dstObject, True, 0, False, False, cdErrNum, cdErrNum, True, True);
      cdStruct_Nil:     CreateStruct(ResIntf, dstObject, False, 0, False, False, cdErrNum, cdErrNum, False, False);
      cdStruct_ErrEmptyFields:ResIntf.CreateError(AnErrPreFix+'f-ouch');
      //cdStruct_EmptyFields:CreateStruct(ResIntf, dstObject, True, 0, False, False, cdErrNum, cdErrNum, False, False,False, True);
    end
  else
    case AKind of
      cdErrNum:         ResIntf.CreateNumValue(ANumVal, True, 2);
      cdErrPre:         ResIntf.CreatePrePrinted(IntToStr(ANumVal));
      cdPtr_ErrNum..
      cdErrPtr_Num:     Result.NestNum :=
                        CreatePtrNum(ResIntf, AnAddr, ANumVal, 2);
      cdPtr_Ptr_ErrNum..
      cdErrPtr_Ptr_Num: Result :=
                        CreatePtrPtrNum(ResIntf, AnAddr, ANumVal, 2);
      cdErrArr_Num:     Result :=
                        CreateArrayOfNum(ResIntf, ANumVal);
      cdArr_EmptyNum,
      cdArr_ErrNum:     Result :=
                        CreateArrayOfNum(ResIntf, ANumVal);
      cdPtr_ErrArr_Num: Result :=
                        CreatePtrArrayOfNum(ResIntf, ANumVal);
      cdErrStruct,
      cdStruct_ErrField,
      cdStruct_Nil,
      cdStruct_ErrEmptyFields:CreateStruct(ResIntf, dstObject, True, 0, False, False, cdErrNum, cdErrNum, False, False);
      //cdStruct_EmptyFields:CreateStruct(ResIntf, dstObject, True, 0, False, False, cdErrNum, cdErrNum, False, False);
    end;

  if ATypeName <> '' then
    ResIntf.SetTypeName(ATypeName); // incl. on error
end;

procedure TTestIdeDebuggerWatchResult.AssertData(const AMessage: string;
  IdeRes: TWatchResultData; AKind: TTestCreateDataKind; AnErr: Boolean;
  ATypeName: String; ANumVal: Int64; AnAddr: QWord; AnErrPreFix: String);
begin
      if AnErr then
        case AKind of
          cdErrNum:         AssertErrData            (AMessage, IdeRes, AnErrPreFix+'ouch');
          cdErrPre:         AssertErrData            (AMessage, IdeRes, AnErrPreFix+'ouch');
          cdPtr_ErrNum:     AssertPointerToErrData   (AMessage, IdeRes, AnAddr, AnErrPreFix+'out of cheese');
          cdErrPtr_Num:     AssertErrData            (AMessage, IdeRes, AnErrPreFix+'ouch');
          cdPtr_Ptr_ErrNum: AssertPtrPointerToErrData(AMessage, IdeRes, AnAddr, AnAddr+1, AnErrPreFix+'gone wrong');
          cdPtr_ErrPtr_Num: AssertPointerToErrData   (AMessage, IdeRes, AnAddr, AnErrPreFix+'out of cheese');
          cdErrPtr_Ptr_Num: AssertErrData            (AMessage, IdeRes, AnErrPreFix+'ouch');
          cdErrArr_Num:     AssertErrData            (AMessage, IdeRes, AnErrPreFix+'ouch');
          cdArr_EmptyNum:   AssertEmptyArrayOfNumData(AMessage, IdeRes, ANumVal, ATypeName);
          cdArr_ErrNum:     AssertArrayOfErrData     (AMessage, IdeRes, AnErrPreFix+'no');
          cdPtr_ErrArr_Num: AssertPointerToErrData   (AMessage, IdeRes, AnAddr, AnErrPreFix+'argh');
          cdErrStruct:      AssertErrData            (AMessage, IdeRes, AnErrPreFix+'bad-obj');
          cdStruct_ErrField:AssertStruct(AMessage, IdeRes, dstObject, True, 0, False, False, cdErrNum, cdErrNum, True, True, False, ATypeName);
          cdStruct_Nil:     AssertStruct(AMessage, IdeRes, dstObject, False, 0, False, False, cdErrNum, cdErrNum, False, False, True, ATypeName);
          cdStruct_ErrEmptyFields:AssertErrData      (AMessage, IdeRes, AnErrPreFix+'f-ouch');
          //cdStruct_EmptyFields:AssertStruct(AMessage, IdeRes, dstObject, True, 0, False, False, cdErrNum, cdErrNum, False, False, False, ATypeName, True);
        end
      else
        case AKind of
          cdErrNum:         AssertNumDataSigned (AMessage, IdeRes, ANumVal, 2, ATypeName);
          cdErrPre:         AssertPrePrintData (AMessage, IdeRes, IntToStr(ANumVal), ATypeName);
          cdPtr_ErrNum..
          cdErrPtr_Num:     AssertPointerToSignedNumData(AMessage, IdeRes, AnAddr, ANumVal, 2, ATypeName, 'TMyNum');
          cdPtr_Ptr_ErrNum..
          cdErrPtr_Ptr_Num: AssertPtrPointerToSignedNumData(AMessage, IdeRes, AnAddr, AnAddr+1, ANumVal, 2, ATypeName, 'TFooPtr', 'TFooNum');
          cdErrArr_Num,
          cdArr_EmptyNum,
          cdArr_ErrNum:     AssertArrayOfNumData(AMessage, IdeRes, ANumVal, ATypeName);
          cdPtr_ErrArr_Num: AssertPtrArrayOfNumData(AMessage, IdeRes, ANumVal, ATypeName);
          cdErrStruct,
          cdStruct_ErrField,
          cdStruct_Nil,
          cdStruct_ErrEmptyFields:AssertStruct(AMessage, IdeRes, dstObject, True, 0, False, False, cdErrNum, cdErrNum, False, False, False, ATypeName);
          //cdStruct_EmptyFields:AssertStruct(AMessage, IdeRes, dstObject, True, 0, False, False, cdErrNum, cdErrNum, False, False, False, ATypeName);
        end;
end;

procedure TTestIdeDebuggerWatchResult.TestWatchResError;
var
  t: TTestWatchResWrapper;
begin
  t.Init;
  t.ResIntf.CreateError('out of cheese');
  AssertErrData('', t.IdeRes, 'out of cheese');
  t.Done;
end;

procedure TTestIdeDebuggerWatchResult.TestPrePrint;
var
  t: TTestWatchResWrapper;
begin
  t.Init;
  t.ResIntf.CreatePrePrinted('Hello World');
  t.ResIntf.SetTypeName('TMyString');
  AssertPrePrintData('', t.IdeRes, 'Hello World', 'TMyString');
  t.Done;
end;

procedure TTestIdeDebuggerWatchResult.TestWatchResNum;
var
  t: TTestWatchResWrapper;
  IdeRes2: TWatchResultData;
begin
  (* Create / CreateCopy *)
  t.Init;
  t.ResIntf.CreateNumValue(123, True, 2);
  t.ResIntf.SetTypeName('TMyNum');
  AssertNumData('', t.IdeRes, rdkSignedNumVal, 123, 123, '123', 2, 'TMyNum');

  IdeRes2 := t.IdeRes.CreateCopy;
  AssertNumData('', IdeRes2, rdkSignedNumVal, 123, 123, '123', 2, 'TMyNum');

  t.Done;
  AssertNumData('', IdeRes2, rdkSignedNumVal, 123, 123, '123', 2, 'TMyNum');
  IdeRes2.Free;

  (* Sign Conversion *)
  t.Init;
  t.ResIntf.CreateNumValue(123, True, 1);
  AssertNumData('', t.IdeRes, rdkSignedNumVal, 123, 123, '123', 1);
  t.Done;

  t.Init;
  t.ResIntf.CreateNumValue(qword(-2), True, 1);
  AssertNumData('', t.IdeRes, rdkSignedNumVal, -2, 254, '-2', 1);
  t.Done;

  t.Init;
  t.ResIntf.CreateNumValue(123, False, 1);
  AssertNumData('', t.IdeRes, rdkUnsignedNumVal, 123, 123, '123', 1);
  t.Done;

  t.Init;
  t.ResIntf.CreateNumValue(254, False, 1);
  AssertNumData('', t.IdeRes, rdkUnsignedNumVal, -2, 254, '254', 1);
  t.Done;

  (* Replace with error *)
  t.Init;
  t.ResIntf.CreateNumValue(123, True, 1);
  t.ResIntf.CreateError('ouch');
  AssertErrData('', t.IdeRes, 'ouch');
  t.Done;
end;

procedure TTestIdeDebuggerWatchResult.TestWatchPointer;
var
  t: TTestWatchResWrapper;
  ResIntfPtr, ResIntfPtr2: TLzDbgWatchDataIntf;
begin
  t.Init;
  t.ResIntf.CreatePointerValue(0);
  t.ResIntf.SetTypeName('TMyPtr');
  AssertPointerData('', t.IdeRes, 0, False, 'TMyPtr');
  t.Done;

  t.Init;
  t.ResIntf.CreatePointerValue(110);
  AssertPointerData('', t.IdeRes, 110, False, '');
  t.Done;

  t.Init;
  t.ResIntf.CreatePointerValue(110);
  t.ResIntf.SetTypeName('TMyPtr');
  ResIntfPtr := t.ResIntf.SetDerefData;
  ResIntfPtr.CreateNumValue(121, True, 1);
  ResIntfPtr.SetTypeName('TMyNum');
  AssertPointerToSignedNumData('', t.IdeRes, 110, 121, 1, 'TMyPtr', 'TMyNum');
  t.Done;

  t.Init;
  t.ResIntf.CreatePointerValue(110);
  t.ResIntf.SetTypeName('TMyPtr');
  ResIntfPtr := t.ResIntf.SetDerefData;
  ResIntfPtr.CreatePointerValue(120);
  ResIntfPtr.SetTypeName('TMyNestPtr');
  ResIntfPtr2 := ResIntfPtr.SetDerefData;
  ResIntfPtr2.CreateNumValue(121, True, 1);
  ResIntfPtr2.SetTypeName('TMyNum');
  AssertPtrPointerToSignedNumData('', t.IdeRes, 110, 120, 121, 1, 'TMyPtr', 'TMyNestPtr', 'TMyNum');
  t.Done;
end;

procedure TTestIdeDebuggerWatchResult.TestWatchResPCharOrString;
var
  t: TTestWatchResWrapper;
  ResIntfStr: TLzDbgWatchDataIntf;
  i0, i1, x: Integer;
  Res: TWatchResultData;
begin
  (* Test 2 values and alternating access *)
  t.Init;
  t.ResIntf.CreateNumValue(123, True, 1);
  t.ResIntf.SetTypeName('TMyType');
  ResIntfStr := t.ResIntf.SetPCharShouldBeStringValue;
  ResIntfStr.CreateNumValue(121, True, 1);

  AssertValKind('', t.IdeRes, rdkPCharOrString);

  t.IdeRes.SetSelectedIndex(0);
  AssertNumDataSigned('i=0', t.IdeRes.SelectedEntry, 123, 1, 'TMyType');
  t.IdeRes.SetSelectedIndex(1);
  AssertNumDataSigned('i=1', t.IdeRes.SelectedEntry, 121, 1, 'TMyType');

  // Switch between elements
  t.IdeRes.SetSelectedIndex(1);
  AssertNumDataSigned('i=1', t.IdeRes.SelectedEntry, 121, 1, 'TMyType');
  t.IdeRes.SetSelectedIndex(0);
  AssertNumDataSigned('i=0', t.IdeRes.SelectedEntry, 123, 1, 'TMyType');
  t.IdeRes.SetSelectedIndex(0);
  AssertNumDataSigned('i=0', t.IdeRes.SelectedEntry, 123, 1, 'TMyType');
  t.IdeRes.SetSelectedIndex(1);
  AssertNumDataSigned('i=1', t.IdeRes.SelectedEntry, 121, 1, 'TMyType');

  t.Done;

  (* Test with error *)
  (* Test with error for idx=1 / after val *)

  // 0: no error
  // 1: set value, then error
  // 2: set error direct
  // -1: idx=0: set value, then PCharOrStr, then error
  for i0 := -1 to 2 do  // idx=0
  for i1 :=  0 to 2 do  // idx=1
  for  x :=  0 to 2 do
  begin
    t.Init;

    if i0 <= 1 then begin
      t.ResIntf.CreateNumValue(123, True, 1);
      t.ResIntf.SetTypeName('TMyType');
    end;
    if i0 >= 1 then t.ResIntf.CreateError('ouch');

    ResIntfStr := t.ResIntf.SetPCharShouldBeStringValue;

    if i0 = -1 then t.ResIntf.CreateError('ouch');

    if i1 <= 1 then begin
      ResIntfStr.CreateNumValue(121, True, 1);
      if (i0 <> 0) then
        ResIntfStr.SetTypeName('TMyType');
    end;
    if i1 >= 1 then ResIntfStr.CreateError('out of cheese');

    Res := t.IdeRes;
    case x of
      1: Res := SaveLoad(Res);
      2: Res := Res.CreateCopy;
    end;

    Res.SetSelectedIndex(0);
    case i0 of
      -1: AssertErrData('i=0', Res.SelectedEntry, 'ouch');
       0: AssertNumDataSigned('i=0', Res.SelectedEntry, 123, 1, 'TMyType');
       1: AssertErrData('i=0', Res.SelectedEntry, 'ouch');
       2: AssertErrData('i=0', Res.SelectedEntry, 'ouch');
    end;

    Res.SetSelectedIndex(1);
    case i1 of
       0: AssertNumDataSigned('i=1', Res.SelectedEntry, 121, 1, 'TMyType');
       1: AssertErrData('i=1', Res.SelectedEntry, 'out of cheese');
       2: AssertErrData('i=1', Res.SelectedEntry, 'out of cheese');
    end;

    Res.SetSelectedIndex(0);
    case i0 of
      -1: AssertErrData('i=0', Res.SelectedEntry, 'ouch');
       0: AssertNumDataSigned('i=0', Res.SelectedEntry, 123, 1, 'TMyType');
       1: AssertErrData('i=0', Res.SelectedEntry, 'ouch');
       2: AssertErrData('i=0', Res.SelectedEntry, 'ouch');
    end;

    if x > 0 then
      Res.Free;
    t.Done;
  end;


// pchar error on subtype
// pointer to nil
// error with empty string
end;

procedure TTestIdeDebuggerWatchResult.TestWatchResPCharOrStringWithPtr;
var
  t: TTestWatchResWrapper;
  ResIntfStr, n1: TLzDbgWatchDataIntf;
  i0, i1, x: Integer;
  Res: TWatchResultData;
begin
// TODO: repeat all, but with empty string as error

  //-3: only idx=0: Ptr->Num  then MarkAsPchar...  then  Ptr->Err
  //-2: only idx=0: Ptr->Err  then MarkAsPchar...  then  Err
  //-1: only idx=0: Ptr->Num  then MarkAsPchar...  then  Err
  // 0: Ptr->Num   (no error)
  // 1: Ptr->Num  then  Err
  // 2: Err
  // 3: Ptr->Err
  // 4: Ptr->Num  then  Ptr->Err
  // 5: Ptr->Err  then  Err
  for i0 := -3 to 5 do  // idx=0
  for i1 :=  0 to 5 do  // idx=1
  for  x :=  0 to 2 do
  begin
    t.Init;

    case i0 of
      -3: n1 := CreatePtrNum(t.ResIntf, 900, 125);
      -2: CreatePtrErr(t.ResIntf, 900, 'no good');
      -1: CreatePtrNum(t.ResIntf, 900, 125);
       0: CreatePtrNum(t.ResIntf, 900, 125);
       1: begin
          CreatePtrNum(t.ResIntf, 900, 125);       t.ResIntf.CreateError('ouch');
          end;
       2: t.ResIntf.CreateError('ouch');
       3: CreatePtrErr(t.ResIntf, 900, 'ouch');
       4: CreatePtrNum(t.ResIntf, 900, 125)        .CreateError('ouch');
       5: begin
          CreatePtrErr(t.ResIntf, 900, 'no good'); t.ResIntf.CreateError('ouch');
          end;
    end;

    ResIntfStr := t.ResIntf.SetPCharShouldBeStringValue;

    // update "pchar" data
    case i0 of
      -3: n1.CreateError('ouch');
      -2: t.ResIntf.CreateError('ouch');
      -1: t.ResIntf.CreateError('ouch');
    end;

    case i1 of
       0: CreatePtrNum(ResIntfStr, 800, 115);
       1: begin
          CreatePtrNum(ResIntfStr, 800, 115);          ResIntfStr.CreateError('out of cheese');
          end;
       2: ResIntfStr.CreateError('out of cheese');
       3: CreatePtrErr(ResIntfStr, 800, 'out of cheese');
       4: CreatePtrNum(ResIntfStr, 800, 115)           .CreateError('out of cheese');
       5: begin
          CreatePtrErr(ResIntfStr, 800, 'gone wrong'); ResIntfStr.CreateError('out of cheese');
          end;
    end;


    Res := t.IdeRes;
    case x of
      1: Res := SaveLoad(Res);
      2: Res := Res.CreateCopy;
    end;

    Res.SetSelectedIndex(0);
    case i0 of
      -3: AssertPointerToErrData('i=0', Res.SelectedEntry, 900, 'ouch');
      -2: AssertErrData('i=0', Res.SelectedEntry, 'ouch');
      -1: AssertErrData('i=0', Res.SelectedEntry, 'ouch');
       0: AssertPointerToSignedNumData('i=0', Res.SelectedEntry, 900, 125, 4, 'TMyPtr', 'TMyNum');
       1: AssertErrData('i=0', Res.SelectedEntry, 'ouch');
       2: AssertErrData('i=0', Res.SelectedEntry, 'ouch');
       3: AssertPointerToErrData('i=0', Res.SelectedEntry, 900, 'ouch');
       4: AssertPointerToErrData('i=0', Res.SelectedEntry, 900, 'ouch');
       5: AssertErrData('i=0', Res.SelectedEntry, 'ouch');
    end;

    Res.SetSelectedIndex(1);
    case i1 of
       0: AssertPointerToSignedNumData('i=1', Res.SelectedEntry, 800, 115, 4, 'TMyPtr', 'TMyNum');
       1: AssertErrData('i=1', Res.SelectedEntry, 'out of cheese');
       2: AssertErrData('i=1', Res.SelectedEntry, 'out of cheese');
       3: AssertPointerToErrData('i=1', Res.SelectedEntry, 800, 'out of cheese');
       4: AssertPointerToErrData('i=1', Res.SelectedEntry, 800, 'out of cheese');
       5: AssertErrData('i=1', Res.SelectedEntry, 'out of cheese');
    end;

    Res.SetSelectedIndex(0);
    case i0 of
      -3: AssertPointerToErrData('i=0', Res.SelectedEntry, 900, 'ouch');
      -2: AssertErrData('i=0', Res.SelectedEntry, 'ouch');
      -1: AssertErrData('i=0', Res.SelectedEntry, 'ouch');
       0: AssertPointerToSignedNumData('i=0', Res.SelectedEntry, 900, 125, 4, 'TMyPtr', 'TMyNum');
       1: AssertErrData('i=0', Res.SelectedEntry, 'ouch');
       2: AssertErrData('i=0', Res.SelectedEntry, 'ouch');
       3: AssertPointerToErrData('i=0', Res.SelectedEntry, 900, 'ouch');
       4: AssertPointerToErrData('i=0', Res.SelectedEntry, 900, 'ouch');
       5: AssertErrData('i=0', Res.SelectedEntry, 'ouch');
    end;

    if x > 0 then
      Res.Free;
    t.Done;
  end;
end;

procedure TTestIdeDebuggerWatchResult.TestWatchResPCharOrStringWithPtrPtr;
var
  t: TTestWatchResWrapper;
  x: Integer;
  aEntryType, aEntryTypeSecond: TTestCreateDataKind;
  ASetError, ASetErrorSecond: Integer;
  ResIntfStr: TLzDbgWatchDataIntf;
  Res: TWatchResultData;
begin
  for  x :=  0 to 2 do
  for aEntryType := low(TTestCreateDataKind) to high(TTestCreateDataKind) do
  for aEntryTypeSecond := SecondType[aEntryType][0] to SecondType[aEntryType][1] do
  for ASetError := 0 to 1 do
  for ASetErrorSecond := 0 to 1 do
  begin

    t.Init;
    CreateData(t.ResIntf, aEntryType, ASetError = 1, 'TMyProto', 200, 990);

    ResIntfStr := t.ResIntf.SetPCharShouldBeStringValue;
    CreateData(ResIntfStr, aEntryTypeSecond, ASetErrorSecond = 1, 'TMyProto', 100, 890);

    Res := t.GetIdeRes;
    case x of
      1: Res := SaveLoad(Res);
      2: Res := Res.CreateCopy;
    end;
    //if x > 0 then
    //  t.Done;


    AssertValKind('', Res, rdkPCharOrString);

    Res.SetSelectedIndex(0);
    AssertData('', Res.SelectedEntry, aEntryType, ASetError=1, 'TMyProto', 200, 990);

    Res.SetSelectedIndex(1);
    AssertData('', Res.SelectedEntry, aEntryTypeSecond, ASetErrorSecond=1, 'TMyProto', 100, 890);

    Res.SetSelectedIndex(0);
    AssertData('', Res.SelectedEntry, aEntryType, ASetError=1, 'TMyProto', 200, 990);

    if x > 0 then
      Res.Free;
    t.Done;
  end;
end;

procedure TTestIdeDebuggerWatchResult.TestWatchResPCharOrStringWithArray;
var
  t: TTestWatchResWrapper;
  x: Integer;
  aEntryType: TTestCreateDataKind;
  aLen, aLenSecond: Integer;
  AErrIdx, AErrIdxSecond, i: Integer;
  ResIntfStr, ProtoIntf: TLzDbgWatchDataIntf;
  Res, ResArray: TWatchResultData;
begin
  (*
  - PCharOrString
    0: Array[aLen]  OR  Err (AErrIdx=-2)
       [0] Data-or-Err      (aErrIdx=0)
       [1] Data-or-Err      (aErrIdx=1)
    1: Array[aLen]  OR  Err (AErrIdxSecond=-2)
       [0] Data-or-Err      (AErrIdxSecond=0)
       [1] Data-or-Err      (AErrIdxSecond=1)

  *)
  for  x :=  0 to 2 do
  for aEntryType := low(TTestCreateDataKind) to high(TTestCreateDataKind) do
  for aLen          := 0 to 2 do
  for aLenSecond    := 0 to 2 do
  for AErrIdx       := -2 to aLen-1 do
  for AErrIdxSecond := -2 to aLenSecond-1 do
  begin
    if (AErrIdx = -2) and (aLen > 0) then Continue; // aLen not used
    if (AErrIdxSecond = -2) and (aLenSecond > 0) then Continue; // aLenSecond not used
    t.Init;

    if AErrIdx = -2 then begin
      t.ResIntf.CreateError('failed');
    end
    else begin
      ProtoIntf := t.ResIntf.CreateArrayValue(datUnknown, Max(0, aLen), 0);
      t.ResIntf.SetTypeName('TMyArray');

      //if aSetProto then begin
      //  CreateData(ProtoIntf, aEntryType, False, 'TMyProto', 987, 87, 'ZZ'); // value part should be ignored
      //end;

      for i := 0 to aLen-1 do begin
        ProtoIntf := t.ResIntf.SetNextArrayData;
        CreateData(ProtoIntf, aEntryType, i = aErrIdx, 'TMyProto', 200+i, 990);
      end;
    end;


    ResIntfStr := t.ResIntf.SetPCharShouldBeStringValue;

    if AErrIdxSecond = -2 then begin
      ResIntfStr.CreateError('failed 2nd');
    end
    else begin
      ProtoIntf := ResIntfStr.CreateArrayValue(datUnknown, Max(0, aLenSecond), 0);
      ResIntfStr.SetTypeName('TMyArray');

      //if aSetProto then begin
      //  CreateData(ProtoIntf, aEntryType, False, 'TMyProto', 987, 87, 'ZZ'); // value part should be ignored
      //end;

      for i := 0 to aLenSecond-1 do begin
        ProtoIntf := ResIntfStr.SetNextArrayData;
        CreateData(ProtoIntf, aEntryType, i = AErrIdxSecond, 'TMyProto', 100+i, 890, 'E2');
      end;
    end;


    Res := t.GetIdeRes;
    case x of
      1: Res := SaveLoad(Res);
      2: Res := Res.CreateCopy;
    end;
    if x > 0 then
      t.Done;

    AssertValKind('', Res, rdkPCharOrString);

    Res.SetSelectedIndex(0);
    ResArray := Res.SelectedEntry;
    if AErrIdx = -2 then begin
      AssertErrData('', ResArray, 'failed');
    end
    else begin
      AssertValKind('0:', ResArray, rdkArray);
      AssertArrayData('0:', ResArray, datUnknown, max(0, aLen), 0, 'TMyArray');
      for i := 0 to aLen-1 do begin
        ResArray.SetSelectedIndex(i);
        AssertData('0:'+IntToStr(i), ResArray.SelectedEntry, aEntryType, (i = aErrIdx), 'TMyProto', 200+i, 990);
      end;
    end;


    Res.SetSelectedIndex(1);
    ResArray := Res.SelectedEntry;
    if AErrIdxSecond = -2 then begin
      AssertErrData('', ResArray, 'failed 2nd');
    end
    else begin
      AssertValKind('0:', ResArray, rdkArray);
      AssertArrayData('0:', ResArray, datUnknown, max(0, aLenSecond), 0, 'TMyArray');
      for i := 0 to aLenSecond-1 do begin
        ResArray.SetSelectedIndex(i);
        AssertData('0:'+IntToStr(i), ResArray.SelectedEntry, aEntryType, (i = AErrIdxSecond), 'TMyProto', 100+i, 890, 'E2');
      end;
    end;


    Res.SetSelectedIndex(0);
    ResArray := Res.SelectedEntry;
    if AErrIdx = -2 then begin
      AssertErrData('', ResArray, 'failed');
    end
    else begin
      AssertValKind('0:', ResArray, rdkArray);
      AssertArrayData('0:', ResArray, datUnknown, max(0, aLen), 0, 'TMyArray');
      for i := 0 to aLen-1 do begin
        ResArray.SetSelectedIndex(i);
        AssertData('0:'+IntToStr(i), ResArray.SelectedEntry, aEntryType, (i = aErrIdx), 'TMyProto', 200+i, 990);
      end;
    end;

    if x > 0 then
      Res.Free
    else
      t.Done;
  end;
end;

procedure TTestIdeDebuggerWatchResult.TestWatchArray;
var
  t: TTestWatchResWrapper;
  ProtoIntf: TLzDbgWatchDataIntf;
  Res: TWatchResultData;
  i, x: Integer;
  aSetProto: Boolean;
  aEntryType: TTestCreateDataKind;
  aLen, aErrIdx, aErrIdx2: Integer;
  aArrType: TLzDbgArrayType;
begin
  for x := 0 to 2 do
  for aSetProto := low(Boolean) to high(Boolean) do
  for aEntryType := low(TTestCreateDataKind) to high(TTestCreateDataKind) do
  for aArrType := low(TLzDbgArrayType) to high(TLzDbgArrayType) do
  for aLen := 0 to 4 do
  for aErrIdx := -1 to aLen-1 do
  for aErrIdx2 := Min(aErrIdx-1, -1) to aErrIdx - 1 do
  begin

    t.Init;
    ProtoIntf := t.ResIntf.CreateArrayValue(aArrType, Max(0, aLen), 0);
    t.ResIntf.SetTypeName('TMyArray');

    if aSetProto then begin
      CreateData(ProtoIntf, aEntryType, False, 'TMyProto', 987, 87, 'ZZ'); // value part should be ignored
    end;

    for i := 0 to aLen-1 do begin
      ProtoIntf := t.ResIntf.SetNextArrayData;
      CreateData(ProtoIntf, aEntryType, (i = aErrIdx) or (i = aErrIdx2), 'TMyProto', 200+i, 990);
    end;

    Res := t.GetIdeRes;
    case x of
      1: Res := SaveLoad(Res);
      2: Res := Res.CreateCopy;
    end;
    if x > 0 then
      t.Done;

    AssertValKind('', Res, rdkArray);
    AssertArrayData('', Res, aArrType, max(0, aLen), 0, 'TMyArray');

    for i := 0 to aLen-1 do begin
      Res.SetSelectedIndex(i);
      AssertData(''+IntToStr(i), Res.SelectedEntry, aEntryType, (i = aErrIdx) or (i = aErrIdx2), 'TMyProto', 200+i, 990);
    end;


    if x > 0 then
      Res.Free
    else
      t.Done;
  end;
end;

procedure TTestIdeDebuggerWatchResult.TestWatchArrayNested;
var
  t: TTestWatchResWrapper;
  ProtoIntf, InnerIntf, ExtraIntf, ExtraEntryIntf: TLzDbgWatchDataIntf;
  Res, InnerRes: TWatchResultData;
  i, j, x: Integer;
  iLen, IErrIx: integer;
  aSetProto, aUsePtr: Boolean;
  aEntryType: TTestCreateDataKind;
  aLen, aExtraArray, aInnerLen, aInnerLen2, aErrIdx, aErrIdx2: Integer;
begin
  for x := 0 to 2 do
  for aEntryType := low(TTestCreateDataKind) to high(TTestCreateDataKind) do
  for aSetProto := low(Boolean) to high(Boolean) do
  for aLen := 1 to 2 do
  for aUsePtr := low(Boolean) to high(Boolean) do
  for aExtraArray := 0 to 3 do
  for aInnerLen := 0 to 2 do
  for aInnerLen2 := 0 to 2 do
  for aErrIdx := -1 to aInnerLen-1 do
  for aErrIdx2 := -1 to aInnerLen2-1 do
  begin
    t.Init;
    ProtoIntf := t.ResIntf.CreateArrayValue(datDynArray, aLen, 0);
    t.ResIntf.SetTypeName('TMyArray');

    if aSetProto then begin
      if aUsePtr then begin
        ProtoIntf := CreatePointer(ProtoIntf, 1900, 'TMyPtr', True);
      end
      else
      if aExtraArray <> 0 then begin
        ProtoIntf.CreateArrayValue(datStatArray, Min(aExtraArray, 2), 0);
        ProtoIntf.SetTypeName('TMyStat');
      end
      else begin
        ProtoIntf.CreateArrayValue(datDynArray, 0, 0);
        ProtoIntf.SetTypeName('TMyInner');
      end;
    end;

    for i := 0 to aLen-1 do begin
      ProtoIntf := t.ResIntf.SetNextArrayData;

      iLen := aInnerLen;
      IErrIx := aErrIdx;
      if i = 1 then begin
        iLen:= aInnerLen2;
        IErrIx := aErrIdx2;
      end;

      if aUsePtr then
        ProtoIntf := CreatePointer(ProtoIntf, 900+i, 'TMyPtr');

      if aExtraArray <> 0 then begin
        ExtraIntf := ProtoIntf;
        ExtraIntf.CreateArrayValue(datStatArray, Min(aExtraArray, 2), 0);
        ExtraIntf.SetTypeName('TMyStat');

        ExtraEntryIntf := ExtraIntf.SetNextArrayData;
        if aExtraArray = 2 then begin
          ExtraEntryIntf.CreateArrayValue(datDynArray, 0, 0);
          ExtraEntryIntf.SetTypeName('TMyInner');
          ExtraEntryIntf := ExtraIntf.SetNextArrayData;
        end;
        ProtoIntf := ExtraEntryIntf;
      end;

      InnerIntf := ProtoIntf.CreateArrayValue(datDynArray, iLen, 0);
      ProtoIntf.SetTypeName('TMyInner');

      if aSetProto then begin
        CreateData(InnerIntf, aEntryType, False, 'TMyProto', 987, 87, 'ZZ'); // value part should be ignored
      end;

      for j := 0 to iLen - 1 do begin
        InnerIntf := ProtoIntf.SetNextArrayData;
        CreateData(InnerIntf, aEntryType, j = IErrIx, 'TMyProto', 200+j, 990);
      end;

      if aExtraArray =3 then begin
        ExtraEntryIntf := ExtraIntf.SetNextArrayData;
        ExtraEntryIntf.CreateArrayValue(datDynArray, 0, 0);
      end;
    end;


    Res := t.GetIdeRes;
    case x of
      1: Res := SaveLoad(Res);
      2: Res := Res.CreateCopy;
    end;
    if x > 0 then
      t.Done;

    AssertValKind('', Res, rdkArray);
    AssertArrayData('', Res, datDynArray, aLen, 0, 'TMyArray');

    for i := 0 to aLen-1 do begin
      Res.SetSelectedIndex(i);
      InnerRes := Res.SelectedEntry;

      iLen := aInnerLen;
      IErrIx := aErrIdx;
      if i = 1 then begin
        iLen:= aInnerLen2;
        IErrIx := aErrIdx2;
      end;

      if aUsePtr then begin
        AssertPointerData('', InnerRes, 900+i, True, 'TMyPtr');
        InnerRes := InnerRes.DerefData;
      end;

      if aExtraArray <> 0 then begin
        AssertValKind('', InnerRes, rdkArray);
        AssertArrayData('', InnerRes, datStatArray, Min(aExtraArray, 2), 0, 'TMyStat');
        if aExtraArray = 2 then
          InnerRes.SetSelectedIndex(1)
        else
          InnerRes.SetSelectedIndex(0);
        InnerRes := InnerRes.SelectedEntry;
      end;

      AssertValKind('', InnerRes, rdkArray);
      AssertArrayData('', InnerRes, datDynArray, iLen, 0, 'TMyInner');

      for j := 0 to iLen - 1 do begin
        InnerRes.SetSelectedIndex(j);
        AssertData(''+IntToStr(i), InnerRes.SelectedEntry, aEntryType, j = IErrIx, 'TMyProto', 200+j, 990);
      end;
    end;



    if x > 0 then
      Res.Free
    else
      t.Done;
  end;
end;

procedure TTestIdeDebuggerWatchResult.TestWatchStuct;
var
  t: TTestWatchResWrapper;
  x, WithAnch: Integer;
  Res: TWatchResultData;
  StrctTyp: TLzDbgStructType;
  WithFld, WithAnch1Fld, WithAnch2Fld: Boolean;
  aEntryType: TTestCreateDataKind;
begin
  for x :=  0 to 2 do
  for StrctTyp := low(TLzDbgStructType) to high(TLzDbgStructType) do
  for WithFld := low(Boolean) to high(Boolean) do
  for aEntryType := low(TTestCreateDataKind) to high(TTestCreateDataKind) do
  for WithAnch := 0 to 2 do
  for WithAnch1Fld := low(Boolean) to high(Boolean) do
  for WithAnch2Fld := low(Boolean) to high(Boolean) do
  begin

    if (not WithFld) and (aEntryType > low(TTestCreateDataKind)) then
      Continue;
    if (WithAnch = 0) and (WithAnch1Fld or WithAnch2Fld) then
      Continue;
    if (WithAnch = 1) and WithAnch2Fld then
      Continue;

    t.Init;

    CreateStruct(t.ResIntf, StrctTyp, WithFld, WithAnch, WithAnch1Fld, WithAnch2Fld,
      aEntryType, aEntryType, False, False);


    Res := t.GetIdeRes;
    case x of
      1: Res := SaveLoad(Res);
      2: Res := Res.CreateCopy;
    end;
    if x > 0 then
      t.Done;

    AssertValKind('', Res, rdkStruct);

    AssertStruct('', Res, StrctTyp, WithFld, WithAnch, WithAnch1Fld, WithAnch2Fld,
      aEntryType, aEntryType, False, False);

    if x > 0 then
      Res.Free
    else
      t.Done;
  end;
end;

procedure TTestIdeDebuggerWatchResult.TestWatchStuctNested;
var
  t: TTestWatchResWrapper;
  x, WithAnch, aPos: Integer;
  Res, InnerRes: TWatchResultData;
  StrctTyp: TLzDbgStructType;
  WithFld, aErr, aUsePtr, aOuterErr, aNil: Boolean;
  aEntryType: TTestCreateDataKind;
  ProtoIntf, AnchIntf: TLzDbgWatchDataIntf;
begin
  StrctTyp := dstClass;
  for x :=  0 to 2 do
  //for StrctTyp := low(TLzDbgStructType) to high(TLzDbgStructType) do
  for aPos := 0 to 2 do
  for WithFld := low(Boolean) to high(Boolean) do
  for aEntryType := low(TTestCreateDataKind) to high(TTestCreateDataKind) do
  for WithAnch := 0 to 2 do
  for aUsePtr := low(Boolean) to high(Boolean) do
  for aOuterErr := low(Boolean) to high(Boolean) do
  for aErr := low(Boolean) to high(Boolean) do
  for aNil := low(Boolean) to high(Boolean) do
  begin
//if x <> 0 then continue;
//if aOuterErr <> true then continue;

    if (not WithFld) and (aEntryType > low(TTestCreateDataKind)) then
      Continue;

    t.Init;

    t.ResIntf.CreateStructure(StrctTyp);
    t.ResIntf.SetTypeName('TExtraStruct');
    case aPos of
    0: begin
        ProtoIntf := t.ResIntf.AddField('FObj', dfvProtected, []);
      end;
    1: begin
        t.ResIntf.AddField('FObj', dfvProtected, []).CreatePrePrinted('dummy2');
        ProtoIntf := t.ResIntf.AddField('FDat', dfvProtected, []);
      end;
    2: begin
        t.ResIntf.AddField('FObj', dfvProtected, []).CreatePrePrinted('dummy2');
        t.ResIntf.AddField('FDat', dfvProtected, []).CreatePrePrinted('dummy1');
        AnchIntf := t.ResIntf.SetAnchestor('TAnch');
        ProtoIntf := AnchIntf.AddField('FBar', dfvProtected, []);
      end;
    end;

    if aUsePtr then begin
      ProtoIntf := CreatePointer(ProtoIntf, 1900, 'TMyPtr');
    end;

    CreateStruct(ProtoIntf, StrctTyp, WithFld, WithAnch, WithFld, WithFld,
      aEntryType, aEntryType, aErr, not aErr, aNil);

    case aPos of
    0: begin
        t.ResIntf.AddField('FDat', dfvProtected, []).CreatePrePrinted('dummy1');
      end;
    1: begin
      end;
    end;

    if aOuterErr then
      t.ResIntf.CreateError('boom');


    Res := t.GetIdeRes;
    case x of
      1: Res := SaveLoad(Res);
      2: Res := Res.CreateCopy;
    end;
    if x > 0 then
      t.Done;

    if aOuterErr then begin
      AssertErrData('outer err', Res, 'boom');
    end
    else begin
      AssertValKind('', Res, rdkStruct);

      if aPos = 2 then
        InnerRes := Res.Anchestor.Fields[0].Field
      else
        InnerRes := Res.Fields[aPos].Field;
      AssertTrue('field <> nil', InnerRes <> nil);
      if InnerRes <> nil then begin

        if aUsePtr then begin
          AssertPointerData('', InnerRes, 1900, True, 'TMyPtr');
          InnerRes := InnerRes.DerefData;
        end;

        AssertValKind('', InnerRes, rdkStruct);

        AssertStruct('', InnerRes, StrctTyp, WithFld, WithAnch, WithFld, WithFld,
          aEntryType, aEntryType, aErr, not aErr, aNil);
      end;
    end;

    if x > 0 then
      Res.Free
    else
      t.Done;
  end;
end;

procedure TTestIdeDebuggerWatchResult.TestWatchArrayStuct;
var
  t: TTestWatchResWrapper;
  ProtoIntf, FldIntf, OuterIntf: TLzDbgWatchDataIntf;
  x, WithAnch, i: Integer;
  StrctTyp: TLzDbgStructType;
  aSetProto, aUsePtr, aUseExtraStuct, WithFld, aErr1, aErr2, aErr1b, aErr2b,
    aNil, aNilb, aOuterErr: Boolean;
  aEntryType1, aEntryType2: TTestCreateDataKind;
  Res, InnerRes: TWatchResultData;
begin
  StrctTyp := dstClass;

  for x :=  0 to 2 do
  for aSetProto := low(Boolean) to high(Boolean) do
  //for StrctTyp := low(TLzDbgStructType) to high(TLzDbgStructType) do
  for aUsePtr := low(Boolean) to high(Boolean) do
  for aUseExtraStuct := low(Boolean) to high(Boolean) do
  for WithFld := low(Boolean) to high(Boolean) do
  for aOuterErr := low(Boolean) to high(Boolean) do
  for aEntryType1 := low(TTestCreateDataKind) to high(TTestCreateDataKind) do
  for aEntryType2 := low(TTestCreateDataKind) to high(TTestCreateDataKind) do
  for WithAnch := 0 to 2 do
  for aNil := low(Boolean) to high(Boolean) do
  for aNilb := low(Boolean) to high(Boolean) do
  for aErr1 := low(Boolean) to high(Boolean) do
  for aErr2 := low(Boolean) to high(Boolean) do
  for aErr1b := low(Boolean) to high(Boolean) do
  for aErr2b := low(Boolean) to high(Boolean) do
  begin
//if x <> 0 then continue;
//if aSetProto <> False then continue;
//if aUsePtr <> False then continue;
//if aUseExtraStuct <> False then continue;
//if WithFld <> True then continue;
//if aOuterErr<> True then continue;
//if aEntryType1 <> cdPtr_ErrNum then continue;
//if aEntryType2 <> cdErrNum then continue;
//if WithAnch <> 0 then continue;
//if aErr1<> False then continue;
//if aErr2<> False then continue;
//if aErr1b<> True then continue;
//if aErr2b<> False then continue;
//if aNil <> False then continue;
//if aNilb <> False then continue;

    if (aNil and aNilb) and
       ( (not aSetProto) or aUsePtr) and  // main struct is NOT proto-type
       ( WithFld or (WithAnch>0) )
    then
      Continue;
    if ( (not WithFld) or (aSetProto and (aUsePtr or aUseExtraStuct)) or aOuterErr ) and
       ( (aEntryType1 > low(TTestCreateDataKind)) or
         (aEntryType2 > low(TTestCreateDataKind)) or
         aErr1 or aErr2 or aErr1b or aErr2b
       )
    then
      Continue;
    if aOuterErr and not(aUsePtr or aUseExtraStuct) then
      Continue;

    if abs(ord(aEntryType1) - ord(aEntryType2)) > 1 then
      Continue;

    t.Init;
    ProtoIntf := t.ResIntf.CreateArrayValue(datDynArray, 2, 0);
    t.ResIntf.SetTypeName('TMyArray');

    if aSetProto then begin
      if aUsePtr then begin
        CreatePointer(ProtoIntf, 1900, 'TMyPtr', True);
      end
      else if aUseExtraStuct then begin
        ProtoIntf.CreateStructure(StrctTyp);
        ProtoIntf.SetTypeName('TExtraStruct');
        FldIntf := ProtoIntf.AddField('FObj', dfvProtected, []);
        CreateStruct(FldIntf, StrctTyp, WithFld, WithAnch, WithFld, WithFld,
          aEntryType1, aEntryType2, aErr1b, aErr2b);
      end
      else begin
        CreateStruct(ProtoIntf, StrctTyp, WithFld, WithAnch, WithFld, WithFld,
          aEntryType1, aEntryType2, aErr1b, aErr2b);
      end;
    end;

    for i := 0 to 1 do begin
      ProtoIntf := t.ResIntf.SetNextArrayData;
      OuterIntf := ProtoIntf;
      if aUsePtr then begin
        ProtoIntf := CreatePointer(ProtoIntf, 1900, 'TMyPtr');
      end;
      if aUseExtraStuct then begin
        ProtoIntf.CreateStructure(StrctTyp);
        ProtoIntf.SetTypeName('TExtraStruct');
        ProtoIntf := ProtoIntf.AddField('FObj', dfvProtected, []);
      end;

      if i = 0 then begin
        // Index 0
        CreateStruct(ProtoIntf, StrctTyp, WithFld, WithAnch, WithFld, WithFld,
          aEntryType1, aEntryType2, aErr1, aErr2, aNil);
        if aOuterErr then begin
          OuterIntf.CreateError('boom');
        end;
      end
      else begin
        // Index 1
        CreateStruct(ProtoIntf, StrctTyp, WithFld, WithAnch, WithFld, WithFld,
          aEntryType1, aEntryType2, aErr1b, aErr2b, aNilb);
      end;
    end;


    Res := t.GetIdeRes;
    case x of
      1: Res := SaveLoad(Res);
      2: Res := Res.CreateCopy;
    end;
    if x > 0 then
      t.Done;


    AssertValKind('', Res, rdkArray);
    AssertArrayData('', Res, datDynArray, 2, 0, 'TMyArray');

    for i := 0 to 1 do begin
      Res.SetSelectedIndex(i);

      if (i = 0) and aOuterErr then begin
        AssertErrData('outer err', Res.SelectedEntry, 'boom');
        continue;
      end;

      InnerRes := Res.SelectedEntry;
      if aUsePtr then begin
        AssertPointerData('', InnerRes, 1900, True, 'TMyPtr');
        InnerRes := InnerRes.DerefData;
      end;
      if aUseExtraStuct then begin
        AssertValKind('', InnerRes, rdkStruct);
        AssertStructField('', InnerRes, 0, 'FObj', dfvProtected, []);
        InnerRes := InnerRes.Fields[0].Field;
      end;

      if i = 0 then begin
        // Index 0
        AssertValKind('', InnerRes, rdkStruct);
        AssertStruct('', InnerRes, StrctTyp, WithFld, WithAnch, WithFld, WithFld,
          aEntryType1, aEntryType2, aErr1, aErr2, aNil);
      end
      else begin
        // Index 1
        AssertValKind('', InnerRes, rdkStruct);
        AssertStruct('', InnerRes, StrctTyp, WithFld, WithAnch, WithFld, WithFld,
          aEntryType1, aEntryType2, aErr1b, aErr2b, aNilb);
      end;
    end;


    if x > 0 then
      Res.Free
    else
      t.Done;
  end;
end;

procedure TTestIdeDebuggerWatchResult.TestWatchArrayStuctArrayStuct;
var
  t: TTestWatchResWrapper;
  Res, EntryRes, FieldRes, InnerEntryRes, InnerFieldRes: TWatchResultData;
  x, InnerEntryCnt_0, InnerEntryCnt_1: Integer;
  aEntryType1, aEntryType1Second: TTestCreateDataKind;
  EntryIntf, FieldIntf, InnerEntryIntf, InnerFieldIntf: TLzDbgWatchDataIntf;
  aErr_0_Fi1, aErr_1_Fi1: Boolean;
begin
  for x :=  0 to 2 do
  for InnerEntryCnt_0 := 0 to 2 do
  for InnerEntryCnt_1 := 0 to 2 do
  for aEntryType1 := low(TTestCreateDataKind) to high(TTestCreateDataKind) do
  for aEntryType1Second := SecondType[aEntryType1][0] to SecondType[aEntryType1][1] do
  for aErr_0_Fi1 := low(Boolean) to high(Boolean) do
  for aErr_1_Fi1 := low(Boolean) to high(Boolean) do
  begin
//if x <> 1 then continue;
//if InnerEntryCnt_0 <> 1 then continue;
//if InnerEntryCnt_1 <> 2 then continue;
//if aEntryType1 <> cdErrNum then continue;
//if aEntryType1Second <> cdErrNum then continue;
//if aErr_0_Fi1 <> True then continue;
//if aErr_1_Fi1 <> False then continue;

    if (InnerEntryCnt_0 = 1) and
       ( aErr_1_Fi1 or (aEntryType1Second <> aEntryType1) )
    then
      Continue;

    t.Init;

    t.ResIntf.CreateArrayValue(datDynArray, 2, 0);
    t.ResIntf.SetTypeName('TMyArray');

      //[0]
      EntryIntf := t.ResIntf.SetNextArrayData;
      EntryIntf.CreateStructure(dstRecord);
      EntryIntf.SetTypeName('TMyStruct');

        // [0].F1
        FieldIntf := EntryIntf.AddField('F1', dfvProtected, []);
        CreateData(FieldIntf, aEntryType1, False, 'TF1');

        // [0].F2
        FieldIntf := EntryIntf.AddField('F2', dfvProtected, []);
        FieldIntf.CreateArrayValue(datDynArray, 2, 0);
        FieldIntf.SetTypeName('TMyInnerArray');

        if InnerEntryCnt_0 > 0 then begin
          // [0].F2[0]
          InnerEntryIntf := FieldIntf.SetNextArrayData;
          InnerEntryIntf.CreateStructure(dstClass, 1100);
          InnerEntryIntf.SetTypeName('TMyInnerStruct');
          // [0].F2[0].Fi1
          InnerFieldIntf := InnerEntryIntf.AddField('Fi1', dfvProtected, []);
          CreateData(InnerFieldIntf, aEntryType1, aErr_0_Fi1, 'TFi1', 200, 990, 'EFi1');
          // [0].F2[0].Fi2
          InnerFieldIntf := InnerEntryIntf.AddField('Fi2', dfvProtected, []);
          CreateData(InnerFieldIntf, aEntryType1, False, 'TFi2', 202, 992, 'EFi2');


          if InnerEntryCnt_0 > 1 then begin
            // [0].F2[1]
            InnerEntryIntf := FieldIntf.SetNextArrayData;
            InnerEntryIntf.CreateStructure(dstClass, 1200);
            // [0].F2[1].Fi1
            InnerFieldIntf := InnerEntryIntf.AddField('Fi1', dfvProtected, []);
            CreateData(InnerFieldIntf, aEntryType1Second, aErr_1_Fi1, 'TFi1', 210, 910, 'EFi1');
            // [0].F2[1].Fi2
            InnerFieldIntf := InnerEntryIntf.AddField('Fi2', dfvProtected, []);
            CreateData(InnerFieldIntf, aEntryType1, False, 'TFi2', 212, 912, 'EFi2');
          end;
        end;


      //[1]
      EntryIntf := t.ResIntf.SetNextArrayData;
      EntryIntf.CreateStructure(dstRecord);
      EntryIntf.SetTypeName('TMyStruct');

        // [1].F1
        FieldIntf := EntryIntf.AddField('F1', dfvProtected, []);
        CreateData(FieldIntf, aEntryType1, False);

        // [1].F2
        FieldIntf := EntryIntf.AddField('F2', dfvProtected, []);
        FieldIntf.CreateArrayValue(datDynArray, 3, 0);
        FieldIntf.SetTypeName('TMyInnerArray');

        if InnerEntryCnt_1 > 0 then begin
          // [1].F2[0]
          InnerEntryIntf := FieldIntf.SetNextArrayData;
          InnerEntryIntf.CreateStructure(dstClass, 2100);
          InnerEntryIntf.SetTypeName('TMyInnerStruct');
          // [1].F2[0].Fi1
          InnerFieldIntf := InnerEntryIntf.AddField('Fi1', dfvProtected, []);
          CreateData(InnerFieldIntf, aEntryType1, aErr_0_Fi1, 'TFi1', 2200, 2990, 'E1Fi1');
          // [1].F2[0].Fi2
          InnerFieldIntf := InnerEntryIntf.AddField('Fi2', dfvProtected, []);
          CreateData(InnerFieldIntf, aEntryType1, False, 'TFi2', 2202, 2992, 'E1Fi2');


          if InnerEntryCnt_1 > 1 then begin
            // [1].F2[1]
            InnerEntryIntf := FieldIntf.SetNextArrayData;
            InnerEntryIntf.CreateStructure(dstClass, 2200);
            // [1].F2[1].Fi1
            InnerFieldIntf := InnerEntryIntf.AddField('Fi1', dfvProtected, []);
            CreateData(InnerFieldIntf, aEntryType1Second, aErr_1_Fi1, 'TFi1', 2210, 2910, 'E1Fi1');
            // [1].F2[1].Fi2
            InnerFieldIntf := InnerEntryIntf.AddField('Fi2', dfvProtected, []);
            CreateData(InnerFieldIntf, aEntryType1, False, 'TFi2', 2212, 2912, 'E1Fi2');
          end;
        end;


    Res := t.GetIdeRes;
    case x of
      1: Res := SaveLoad(Res);
      2: Res := Res.CreateCopy;
    end;
    if x > 0 then
      t.Done;


    AssertValKind('', Res, rdkArray);
    AssertArrayData('', Res, datDynArray, 2, 0, 'TMyArray');

      // [0]
      res.SetSelectedIndex(0);
      EntryRes := Res.SelectedEntry;

      AssertStructData('[0]', EntryRes, dstRecord, 0, 2, 'TMyStruct');

        // [0].F1
        AssertStructField('[0].F1', EntryRes, 0, 'F1', dfvProtected, []);
        FieldRes := EntryRes.Fields[0].Field;
        AssertData('[0].F1',FieldRes, aEntryType1, False, 'TF1');

        // [0].F2
        AssertStructField('[0].F2', EntryRes, 1, 'F2', dfvProtected, []);
        FieldRes := EntryRes.Fields[1].Field;
        /// len =1 as only one is set
        AssertArrayData('[0].F2', FieldRes, datDynArray, InnerEntryCnt_0, 0, 'TMyInnerArray');

        if InnerEntryCnt_0 > 0 then begin
          // [0].F2[0]
          FieldRes.SetSelectedIndex(0);
          InnerEntryRes := FieldRes.SelectedEntry;
          AssertStructData('[0].F2[0]', InnerEntryRes, dstClass, 1100, 2, 'TMyInnerStruct');
          // [0].F2[0].Fi1
          AssertStructField('[0].F2[0].Fi1', InnerEntryRes, 0, 'Fi1', dfvProtected, []);
          InnerFieldRes := InnerEntryRes.Fields[0].Field;
          AssertData('[0].F2[0].Fi1',InnerFieldRes, aEntryType1, aErr_0_Fi1, 'TFi1', 200, 990, 'EFi1');
          // [0].F2[0].Fi2
          AssertStructField('[0].F2[0].Fi2', InnerEntryRes, 1, 'Fi2', dfvProtected, []);
          InnerFieldRes := InnerEntryRes.Fields[1].Field;
          AssertData('[0].F2[0].Fi2',InnerFieldRes, aEntryType1, False, 'TFi2', 202, 992, 'EFi2');

          // [0].F2[1]
          if InnerEntryCnt_0 > 1 then begin
            FieldRes.SetSelectedIndex(1);
            InnerEntryRes := FieldRes.SelectedEntry;
            AssertStructData('[0].F2[1]', InnerEntryRes, dstClass, 1200, 2, 'TMyInnerStruct');
            // [0].F2[1].Fi1
            AssertStructField('[0].F2[1].Fi1', InnerEntryRes, 0, 'Fi1', dfvProtected, []);
            InnerFieldRes := InnerEntryRes.Fields[0].Field;
            AssertData('[0].F2[1].Fi1',InnerFieldRes, aEntryType1Second, aErr_1_Fi1, 'TFi1', 210, 910, 'EFi1');
            // [0].F2[1].Fi2
            AssertStructField('[0].F2[1].Fi2', InnerEntryRes, 1, 'Fi2', dfvProtected, []);
            InnerFieldRes := InnerEntryRes.Fields[1].Field;
            AssertData('[0].F2[1].Fi2',InnerFieldRes, aEntryType1, False, 'TFi2', 212, 912, 'EFi2');
          end;
        end;


      // [1]
      res.SetSelectedIndex(1);
      EntryRes := Res.SelectedEntry;

      AssertStructData('[0]', EntryRes, dstRecord, 0, 2, 'TMyStruct');

        // [1].F1
        AssertStructField('[1].F1', EntryRes, 0, 'F1', dfvProtected, []);
        FieldRes := EntryRes.Fields[0].Field;
        AssertData('[1].F1',FieldRes, aEntryType1, False, 'TF1');

        // [1].F2
        AssertStructField('[1].F2', EntryRes, 1, 'F2', dfvProtected, []);
        FieldRes := EntryRes.Fields[1].Field;
        /// len =1 as only one is set
        AssertArrayData('[1].F2', FieldRes, datDynArray, InnerEntryCnt_1, 0, 'TMyInnerArray');

        if InnerEntryCnt_1 > 0 then begin
         // [1].F2[0]
          FieldRes.SetSelectedIndex(0);
          InnerEntryRes := FieldRes.SelectedEntry;
          AssertStructData('[1].F2[0]', InnerEntryRes, dstClass, 2100, 2, 'TMyInnerStruct');
          // [1].F2[0].Fi1
          AssertStructField('[1].F2[0].Fi1', InnerEntryRes, 0, 'Fi1', dfvProtected, []);
          InnerFieldRes := InnerEntryRes.Fields[0].Field;
          AssertData('[1].F2[0].Fi1',InnerFieldRes, aEntryType1, aErr_0_Fi1, 'TFi1', 2200, 2990, 'E1Fi1');
          // [1].F2[0].Fi2
          AssertStructField('[1].F2[0].Fi2', InnerEntryRes, 1, 'Fi2', dfvProtected, []);
          InnerFieldRes := InnerEntryRes.Fields[1].Field;
          AssertData('[1].F2[0].Fi2',InnerFieldRes, aEntryType1, False, 'TFi2', 2202, 2992, 'E1Fi2');

          // [1].F2[1]
          if InnerEntryCnt_1 > 1 then begin
            FieldRes.SetSelectedIndex(1);
            InnerEntryRes := FieldRes.SelectedEntry;
            AssertStructData('[1].F2[1]', InnerEntryRes, dstClass, 2200, 2, 'TMyInnerStruct');
            // [1].F2[1].Fi1
            AssertStructField('[1].F2[1].Fi1', InnerEntryRes, 0, 'Fi1', dfvProtected, []);
            InnerFieldRes := InnerEntryRes.Fields[0].Field;
            AssertData('[1].F2[1].Fi1',InnerFieldRes, aEntryType1Second, aErr_1_Fi1, 'TFi1', 2210, 2910, 'E1Fi1');
            // [1].F2[1].Fi2
            AssertStructField('[1].F2[1].Fi2', InnerEntryRes, 1, 'Fi2', dfvProtected, []);
            InnerFieldRes := InnerEntryRes.Fields[1].Field;
            AssertData('[1].F2[1].Fi2',InnerFieldRes, aEntryType1, False, 'TFi2', 2212, 2912, 'E1Fi2');
          end;
        end;


    if x > 0 then
      Res.Free
    else
      t.Done;
  end;
end;



initialization

  RegisterTest(TTestIdeDebuggerWatchResult);
end.

