unit TTestWatchUtilities;

{$mode objfpc}{$H+}
{$modeswitch AdvancedRecords}
{$modeswitch TypeHelpers}
{$WARN 5093 off : function result variable of a managed type does not seem to be initialized}

interface

uses
  Classes, SysUtils, math, DbgIntfBaseTypes, DbgIntfDebuggerBase,
  FpPascalBuilder, LazLoggerBase, IdeDebuggerWatchValueIntf, Forms,
  IdeDebuggerBase, IdeDebuggerUtils, IdeDebuggerWatchResult,
  IdeDebuggerWatchResPrinter, RegExpr, TestDbgTestSuites, TTestDebuggerClasses,
  TTestDbgExecuteables, TestDbgConfig, LazDebuggerIntf,
  LazDebuggerIntfBaseTypes, TestOutputLogger;

const
  SIZE_1 = 1;
  SIZE_2 = 2;
  SIZE_4 = 4;
  SIZE_8 = 8;

type
  TWatchExpectationResultKind = (
    rkMatch, rkInteger, rkCardinal, rkFloat, rkBool, rkSizedBool, rkEnum, rkSet,
    rkChar, rkAnsiString, rkShortString, rkWideString, rkPointer, rkPointerAddr,
    rkClass, rkObject, rkRecord, rkInterface, rkField,
    rkArray, rkStatArray, rkDynArray
  );

  TWatchExpErrorHandlingFlag =
    (ehIgnAll,             // ignore error for all
     ehTestSkip,       // Do not run test

     ehIgnData,           // Ignore the data part
     ehIgnPointerDerefData,  // Ignore if a pointer has deref data or not
     ehIgnKind,           // Ignore skSimple, ....
     ehIgnKindPtr,        // Ignore skSimple, ONLY if got kind=skPointer
     ehIgnKindArrayType,  // Ignore dyn vs stat array
     ehIgnTypeName,       // Ignore the typename
     ehIgnTypeNameInData, // Ignore any appearance of typename in data
     ehMatchTypeName,     // The typename is a regex
     ehNoTypeInfo,

     ehNoCharQuoting,     // unprintable chars are already escaped/quoted
     ehCharFromIndex,     // Debugger is allowed Pchar: 'x' String 'y'
     ehChrIdxExpString,   // Use "String" *IF* Debugger returns Pchar: 'x' String 'y'
     ehChrIdxExpPChar,    // Use "PChar" *IF* Debugger returns Pchar: 'x' String 'y'
     ehChrIdxSkip,        // SKIP checks " *IF* Debugger returns Pchar: 'x' String 'y'
     //ehChrIdxOnly,        // SKIP checks " *UNLESS* Debugger returns Pchar: 'x' String 'y'
     ehNoFieldOrder,  // structure: fields can be in any order
     ehMissingFields, // structure: fields may have gaps

     ehExpectNotFound,
     ehExpectError,       // watch is invalid (less specific, than not found / maybe invalid expression ?)
     ehExpectErrorText,   // watch is invalid // still test for Expected test

     ehNotImplemented,     // The debugger is known to fail this test // same as ehIgnAll
     ehNotImplementedKind, // skSimple...
     ehNotImplementedType, // typename
     ehNotImplementedData
    );
  TWatchExpErrorHandlingFlags = set of TWatchExpErrorHandlingFlag;


  { TWatchExpectationResult }

  TWatchExpectationResult = record
    //ResultKind: TWatchExpectationResultKind;

    ExpTextData: string; // depends on ResultKind
    ExpSymKind: TDbgSymbolKind; // skSimple, skInteger...
    ExpTypeName: string; // AnsiString, Integer, TObject...
    ExpErrorHandlingFlags: Array [TSymbolType] of TWatchExpErrorHandlingFlags;

    ExpSubResults: Array of TWatchExpectationResult;
//    MinDbg, MinFpc: Integer;
    //FullTypesExpect: TFullTypeMemberExpectationResultArray;

    ExpSetData: array of string; // rkSet

    ExpFieldName: string; // member in structure

    function AddFlag(AFlag: TWatchExpErrorHandlingFlag; ASymTypes: TSymbolTypes = []): TWatchExpectationResult;
    function AddFlag(AFlags: TWatchExpErrorHandlingFlags; ASymTypes: TSymbolTypes = []): TWatchExpectationResult;

    function N(AFieldName: String): TWatchExpectationResult; // FieldName

    function Skip(ASymTypes: TSymbolTypes = []): TWatchExpectationResult;
    function SkipIf(ACond: Boolean; ASymTypes: TSymbolTypes = []): TWatchExpectationResult;

    function IgnAll(ASymTypes: TSymbolTypes = []): TWatchExpectationResult;
    function IgnData(ASymTypes: TSymbolTypes = []): TWatchExpectationResult;
    function IgnKind(ASymTypes: TSymbolTypes = []): TWatchExpectationResult;
    function IgnKindPtr(ASymTypes: TSymbolTypes = []): TWatchExpectationResult;
    function IgnKindArrayType(ASymTypes: TSymbolTypes = []): TWatchExpectationResult;
    function IgnTypeName(ASymTypes: TSymbolTypes = []): TWatchExpectationResult;
    function MatchTypeName(ASymTypes: TSymbolTypes = []): TWatchExpectationResult;

    function CharFromIndex(ASymTypes: TSymbolTypes = []): TWatchExpectationResult;
    function ChrIdxExpString(ASymTypes: TSymbolTypes = []): TWatchExpectationResult;
    function ChrIdxExpPChar(ASymTypes: TSymbolTypes = []): TWatchExpectationResult;
    function ChrIdxSkip(ASymTypes: TSymbolTypes = []): TWatchExpectationResult;

    function ExpectNotFound(ASymTypes: TSymbolTypes = []): TWatchExpectationResult;
    function ExpectError(ASymTypes: TSymbolTypes = []): TWatchExpectationResult;

    function NotImplemented(ASymTypes: TSymbolTypes = []): TWatchExpectationResult;
    function NotImplementedData(ASymTypes: TSymbolTypes = []): TWatchExpectationResult;

    procedure MakeCopy;


    case ExpResultKind: TWatchExpectationResultKind of
      rkMatch: ();
      rkInteger: (
        ExpIntValue: Int64;
        ExpIntSize: Integer; // Byte=1, Word=2, ...
      );
      rkCardinal: (
        ExpCardinalValue: QWord;
        ExpCardinalSize: Integer; // Byte=1, Word=2, ...
      );
      rkFloat: (
        ExpFloatValue: Extended;
      );
      rkBool, rkSizedBool: (
        ExpBoolValue: Boolean;
      );
      rkPointerAddr: (
        ExpPointerValue: Pointer;
      );
      rkDynArray: (
        ExpFullArrayLen: integer;
      );
  end;

  TWatchExpectationResultArray = array of TWatchExpectationResult;

  (* Do *NOT* start any other identifiers with "Tst...".
     There are a lot of
       with TWatchExpectation do ...
     blocks
  *)
  PWatchExpectation = ^TWatchExpectation;

  { TWatchExpectation }

  TWatchExpectation = record
    TstTestName: String;
    TstWatch: TTestWatch;

    SkipEvalCall: Boolean;
    EvalCallTestFlags: TWatcheEvaluateFlags;
    EvalCallResReceived: Boolean;
    EvalCallResSuccess: Boolean;
    EvalCallResText: String;
    EvalCallResDBGType: TDBGType;

    //TstRepeatCount: Integer;
    //TstEvaluateFlags: TWatcheEvaluateFlags;
    TstStackFrame: Integer;
    TstMinDbg, TstMinFpc: Integer;

    TstExpected: TWatchExpectationResult;
    //TstExpected: Array [TSymbolType] of TWatchExpectationResult;

    //TstUserData, TstUserData2: Pointer;
    //TstOnBeforeTest: TWatchExpectOnBeforeTest;

    function AddFlag(AFlag: TWatchExpErrorHandlingFlag; ASymTypes: TSymbolTypes = []): PWatchExpectation;
    function AddFlag(AFlags: TWatchExpErrorHandlingFlags; ASymTypes: TSymbolTypes = []): PWatchExpectation;
  end;

  { TWatchExpectationHelper }

  TWatchExpectationHelper = type helper for PWatchExpectation
    function AddFlag(AFlag: TWatchExpErrorHandlingFlag; ASymTypes: TSymbolTypes = []; ACond: Boolean = True): PWatchExpectation;
    function AddFlag(AFlags: TWatchExpErrorHandlingFlags; ASymTypes: TSymbolTypes = []; ACond: Boolean = True): PWatchExpectation;
    function AddFlag(AFlag: TWatchExpErrorHandlingFlag; ACond: Boolean): PWatchExpectation;
    function AddFlag(AFlags: TWatchExpErrorHandlingFlags; ACond: Boolean): PWatchExpectation;

    function AddEvalFlag(AFlags: TWatcheEvaluateFlags; ACond: Boolean = True): PWatchExpectation;

    function Skip(ASymTypes: TSymbolTypes = []): PWatchExpectation;
    function SkipIf(ACond: Boolean; ASymTypes: TSymbolTypes = []): PWatchExpectation;
    function SkipEval: PWatchExpectation;

    function IgnAll(ASymTypes: TSymbolTypes = []; ACond: Boolean = True): PWatchExpectation;
    function IgnData(ASymTypes: TSymbolTypes = []; ACond: Boolean = True): PWatchExpectation;
    function IgnKind(ASymTypes: TSymbolTypes = []; ACond: Boolean = True): PWatchExpectation;
    function IgnKindPtr(ASymTypes: TSymbolTypes = []; ACond: Boolean = True): PWatchExpectation;
    function IgnTypeName(ASymTypes: TSymbolTypes = []; ACond: Boolean = True): PWatchExpectation;
    function MatchTypeName(ASymTypes: TSymbolTypes = []; ACond: Boolean = True): PWatchExpectation;

    function NoCharQuoting(ASymTypes: TSymbolTypes = []; ACond: Boolean = True): PWatchExpectation;
    function CharFromIndex(ASymTypes: TSymbolTypes = []; ACond: Boolean = True): PWatchExpectation;
    function ChrIdxExpString(ASymTypes: TSymbolTypes = []; ACond: Boolean = True): PWatchExpectation;
    function ChrIdxExpPChar(ASymTypes: TSymbolTypes = []; ACond: Boolean = True): PWatchExpectation;
    function ChrIdxSkip(ASymTypes: TSymbolTypes = []; ACond: Boolean = True): PWatchExpectation;

    function ExpectNotFound(ASymTypes: TSymbolTypes = []; ACond: Boolean = True): PWatchExpectation;
    function ExpectError(ASymTypes: TSymbolTypes = []; ACond: Boolean = True): PWatchExpectation;

    function NotImplemented(ASymTypes: TSymbolTypes = []; ACond: Boolean = True): PWatchExpectation;
    function NotImplementedData(ASymTypes: TSymbolTypes = []; ACond: Boolean = True): PWatchExpectation;
  end;

  TWatchExpectationResultArrayHelper = type helper for TWatchExpectationResultArray
  end;

  TWatchExpTestCurrentData = record
    WatchExp: TWatchExpectation;
    WatchRes: TWatchResultData;
    WatchTpInf: TDBGType; //deprecated;
    Expectation: TWatchExpectationResult;
    HasTypeInfo: Boolean;
    IsNested: Boolean;
  end;

  { TWatchExpectationList }
  TDbgSymbolKinds = set of TDbgSymbolKind;

  TWatchExpectationList = class
  private
    FWatchResultPrinter: TWatchResultPrinter;
    FAcceptSkSimple: TDbgSymbolKinds;
    FTest: TDBGTestCase;
    FList: array of TWatchExpectation;
    FTypeNameAliases: TStringList;

    FCurEvalCallWatchExp: PWatchExpectation;
    procedure EvalCallback(Sender: TObject; ASuccess: Boolean;
      ResultText: String; ResultDBGType: TDBGType);
    function GetCompiler: TTestDbgCompiler;
    function GetDebugger: TTestDbgDebugger;
    function GetLazDebugger: TDebuggerIntf;
    function GetTests(Index: Integer): PWatchExpectation;
    function ParseCommaList(AVal: String; out AFoundCount: Integer;
      AMaxLen: Integer = -1; AComma: char = ','): TStringArray;
    function GetExpTypeNameAsRegEx(AnExpect: TWatchExpectationResult): String;
  protected
    function EvaluateWatch(AWatchExp: PWatchExpectation; AThreadId: Integer; constref CurLoc: TDBGLocationRec; AWaitForEval: Boolean = True): Boolean; virtual;
    function EvaluateExprCmd(AWatchExp: PWatchExpectation; AThreadId: Integer; constref CurLoc: TDBGLocationRec; AWaitForEval: Boolean = True): Boolean; virtual;
    procedure WaitWhileEval; virtual;

    function TestMatches(Name: string; Expected, Got: string; AContext: TWatchExpTestCurrentData; AIgnoreReason: String): Boolean;
    function TestMatches(Name: string; Expected, Got: string; ACaseSense: Boolean; AContext: TWatchExpTestCurrentData; AIgnoreReason: String): Boolean;
    function TestEquals(Name: string; Expected, Got: string; AContext: TWatchExpTestCurrentData; AIgnoreReason: String): Boolean;
    function TestEquals(Name: string; Expected, Got: string; ACaseSense: Boolean; AContext: TWatchExpTestCurrentData; AIgnoreReason: String): Boolean;
    function TestEquals(Name: string; Expected, Got: int64; AContext: TWatchExpTestCurrentData; AIgnoreReason: String): Boolean;
    function TestEquals(Name: string; Expected, Got: QWord; AContext: TWatchExpTestCurrentData; AIgnoreReason: String): Boolean;
    function TestTrue(Name: string; Got: Boolean; AContext: TWatchExpTestCurrentData; AIgnoreReason: String): Boolean;
    function TestFalse(Name: string; Got: Boolean; AContext: TWatchExpTestCurrentData; AIgnoreReason: String): Boolean;

    function CheckResult(AnWatchExp: TWatchExpectation): Boolean;
    function CheckData(AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String; AnIsNested: Boolean = True; ASkipErrCheck: Boolean = False): Boolean; virtual;
    function VerifyDebuggerState: Boolean; virtual;
    function VerifySymType(AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean; virtual;
    function VerifyTypeName(AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean; virtual;

    function CheckResultMatch(AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean; virtual;
    function CheckResultNum(AContext: TWatchExpTestCurrentData; IsCardinal: Boolean; AnIgnoreRsn: String): Boolean; virtual;
    function CheckResultBool(AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean; virtual;
    function CheckResultFloat(AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean; virtual;
    function CheckResultEnum(AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean; virtual;
    function CheckResultSet(AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean; virtual;
    function CheckResultChar(AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean; virtual;
    function CheckResultAnsiStr(AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean; virtual;
    function CheckResultShortStr(AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean; virtual;
    function CheckResultPointer(AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean; virtual;
    function CheckResultPointerAddr(AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean; virtual;
    function CheckResultArray(AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean; virtual;
    function CheckStructureFields(const AnIgnoreRsn: String; var AContext: TWatchExpTestCurrentData): Boolean; virtual;
    function CheckResultRecord(AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean; virtual;
    function CheckResultClass(AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean; virtual;
    function CheckResultObject(AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean; virtual;
    function CheckResultInterface(AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean; virtual;

    property Compiler: TTestDbgCompiler read GetCompiler;
    property Debugger: TTestDbgDebugger read GetDebugger;
    property LazDebugger: TDebuggerIntf read GetLazDebugger;
  public
    constructor Create(ATest: TDBGTestCase);
    destructor Destroy; override;

    function AddWithoutExpect(ATestName: String;
      AnExpr:  string;
      AStackFrame: Integer = 0; AMinFpc: Integer = 0; AMinDbg: Integer = 0
    ): PWatchExpectation;

    function Add(ATestName: String;
      AnExpr:  string; // AEvaluateFlags: TWatcheEvaluateFlags;
      AnExpect: TWatchExpectationResult;
      AStackFrame: Integer = 0; AMinFpc: Integer = 0; AMinDbg: Integer = 0
      // ASpecialFlags: ... // Ignore this or that // maybe per result
    ): PWatchExpectation;

    function Add(
      AnExpr:  string; // AEvaluateFlags: TWatcheEvaluateFlags;
      AnExpect: TWatchExpectationResult;
      AStackFrame: Integer = 0; AMinFpc: Integer = 0; AMinDbg: Integer = 0
    ): PWatchExpectation;

    procedure AddIndexFromPrevious(IndexNames: array of string;
      ValueIndex: array of integer; AnPreviousOffset: Integer = 0);
    procedure AddMemberFromPrevious(AnPreviousOffset: Integer = 0);


    procedure Clear;
    function Count: Integer;
    procedure EvaluateWatches;
    procedure CheckResults;
    procedure EvalAndCheck;

    procedure AddTypeNameAlias(ATypeName, AnAliases: String);
    property AcceptSkSimple: TDbgSymbolKinds read FAcceptSkSimple write FAcceptSkSimple ; // skSimple for skInteger,skChar,...
    property Tests[Index: Integer]: PWatchExpectation read GetTests;
  end;


function weMatch(AExpVal: String; ASymKind: TDBGSymbolKind; ATypeName: String=''): TWatchExpectationResult;
function weMatchErr(AExpVal: String): TWatchExpectationResult;
function weMatchFpErr(AExpVal: String): TWatchExpectationResult; // take stirng for FORMAT()

function weInteger(AExpVal: Int64; ATypeName: String=#1; ASize: Integer = 4): TWatchExpectationResult;
function weCardinal(AExpVal: QWord; ATypeName: String=#1; ASize: Integer = 4): TWatchExpectationResult;
function weSingle(AExpVal: Extended; ATypeName: String=#1): TWatchExpectationResult;
function weDouble(AExpVal: Extended; ATypeName: String=#1): TWatchExpectationResult;
function weFloat(AExpVal: Extended; ATypeName: String=''): TWatchExpectationResult;

function weBool(AExpVal: Boolean; ATypeName: String=#1): TWatchExpectationResult;
function weSizedBool(AExpVal: Boolean; ATypeName: String=''): TWatchExpectationResult; // Display as True(255) etc
function weEnum(AExpVal: string; ATypeName: String=#1): TWatchExpectationResult;
function weSet(const AExpVal: Array of string; ATypeName: String=#1): TWatchExpectationResult;

function weChar(AExpVal: char; ATypeName: String=#1): TWatchExpectationResult;
function weWideChar(AExpVal: char; ATypeName: String=#1): TWatchExpectationResult;
function weAnsiStr(AExpVal: string; ATypeName: String=#1): TWatchExpectationResult;
function weShortStr(AExpVal: string; ATypeName: String=#1): TWatchExpectationResult;
function weWideStr(AExpVal: string; ATypeName: String=#1): TWatchExpectationResult;
function weUniStr(AExpVal: string; ATypeName: String=#1): TWatchExpectationResult;

function wePointer(ATypeName: String=''): TWatchExpectationResult;
function wePointer(AExpVal: TWatchExpectationResult; ATypeName: String=''): TWatchExpectationResult;
function wePointerAddr(AExpVal: Pointer; ATypeName: String=''): TWatchExpectationResult;
function wePointerAddr(AExpVal: Pointer; AExpSubVal: TWatchExpectationResult; ATypeName: String=''): TWatchExpectationResult;

function weArray(const AExpVal: Array of TWatchExpectationResult; AExpFullLen: Integer; ATypeName: String=''): TWatchExpectationResult; overload;
function weStatArray(const AExpVal: Array of TWatchExpectationResult; ATypeName: String=''): TWatchExpectationResult; overload;
function weStatArray(const AExpVal: Array of TWatchExpectationResult; AExpFullLen: Integer; ATypeName: String=''): TWatchExpectationResult; overload;
function weDynArray(const AExpVal: Array of TWatchExpectationResult; ATypeName: String=''): TWatchExpectationResult; overload;
function weDynArray(const AExpVal: Array of TWatchExpectationResult; AExpFullLen: Integer; ATypeName: String=''): TWatchExpectationResult; overload;

// common arrays: weTttArray(weChar([...]))
function weChar(const AExpVal: array of char; ATypeName: String=#1): TWatchExpectationResultArray;
function weWideChar(const AExpVal: array of char; ATypeName: String=#1): TWatchExpectationResultArray;
function weInteger(const AExpVal: array of Int64; ATypeName: String=#1; ASize: Integer = 4): TWatchExpectationResultArray;
function weCardinal(const AExpVal: array of QWord; ATypeName: String=#1; ASize: Integer = 4): TWatchExpectationResultArray;
function weAnsiStr(const AExpVal: array of string; ATypeName: String=#1): TWatchExpectationResultArray;
function weShortStr(const AExpVal: array of string; ATypeName: String=#1): TWatchExpectationResultArray;
function weBool(const AExpVal: array of Boolean; ATypeName: String=#1): TWatchExpectationResultArray;
function weEnum(const AExpVal: array of string; ATypeName: String=#1): TWatchExpectationResultArray;



function weRecord(AExpFields: array of TWatchExpectationResult; ATypeName: String=#1): TWatchExpectationResult;
function weClass(AExpFields: array of TWatchExpectationResult; ATypeName: String=#1): TWatchExpectationResult;
function weObject(AExpFields: array of TWatchExpectationResult; ATypeName: String=#1): TWatchExpectationResult;
function weInterface(AExpFields: array of TWatchExpectationResult; ATypeName: String=#1): TWatchExpectationResult;


operator := (a:string): TWatchExpectationResult;
operator := (a:integer): TWatchExpectationResult;
operator := (a:pointer): TWatchExpectationResult;



implementation

var
  TestWatchDisplayFormat: TWatchDisplayFormat;

operator := (a:string): TWatchExpectationResult;
begin
  Result := weAnsiStr(a);
end;

operator := (a:integer): TWatchExpectationResult;
begin
  Result := weInteger(a);
end;

operator := (a: pointer): TWatchExpectationResult;
begin
  Result := wePointerAddr(a);
end;

type

  { TStringArrayHelper }

  TStringArrayHelper = type helper for TStringArray
    function IndexOfFieldName(AName: String; ALength: Integer = Maxint; Sep: char = '='): Integer;
    function ValueOfFieldName(AnIndex: Integer; Sep: char = '='): String;
    procedure delete(AStart, ACnt: Integer);
  end;

{ TStringArrayHelper }

function TStringArrayHelper.IndexOfFieldName(AName: String; ALength: Integer;
  Sep: char): Integer;
var
  p: SizeInt;
begin
  ALength := Min(ALength, Length(Self));
  Result := 0;
  while Result < ALength do begin
    p := pos(Sep, Self[Result]);
    if (p >= 0) and (CompareText(trim(Copy(Self[Result], 1, p-1)), AName) = 0) then
      exit;
    inc(Result);
  end;
  if Result >= ALength then
    Result := -1;
end;

function TStringArrayHelper.ValueOfFieldName(AnIndex: Integer; Sep: char): String;
begin
  Result := trim(copy(Self[AnIndex], pos(Sep, Self[AnIndex])+1, MaxInt));
end;

procedure TStringArrayHelper.delete(AStart, ACnt: Integer);
var
  i: Integer;
begin
  if ACnt <= 0 then exit;
  for i := AStart to Length(Self)-1 - ACnt do
    Self[i] := Self[i+ACnt];
end;

function weMatch(AExpVal: String; ASymKind: TDBGSymbolKind; ATypeName: String
  ): TWatchExpectationResult;
begin
  Result := Default(TWatchExpectationResult);
  Result.ExpResultKind := rkMatch;
  Result.ExpSymKind := ASymKind;
  Result.ExpTextData := AExpVal;
end;

function weMatchErr(AExpVal: String): TWatchExpectationResult;
begin
  Result := Default(TWatchExpectationResult);
  Result.ExpResultKind := rkMatch;
  Result.ExpSymKind := skNone;
  Result.ExpTextData := AExpVal;
  Result.AddFlag(ehExpectErrorText);
end;

function weMatchFpErr(AExpVal: String): TWatchExpectationResult;
var
  i, j: integer;
begin
  AExpVal := QuoteRegExprMetaChars(AExpVal);
  i := pos('%', AExpVal);
  while (i > 0) and (i < Length(AExpVal)) do begin
    j := i + 1;
    while (j <= Length(AExpVal)) and not (AExpVal[j] in ['a'..'z', 'A'..'Z']) do inc(j);
    AExpVal[i] := '.';
    AExpVal[i+1] := '*';
    delete(AExpVal, i+2, j-i-1);
    i := pos('%', AExpVal);
  end;
  Result := Default(TWatchExpectationResult);
  Result.ExpResultKind := rkMatch;
  Result.ExpSymKind := skNone;
  Result.ExpTextData := AExpVal;
  Result.AddFlag(ehExpectErrorText);
end;

function weInteger(AExpVal: Int64; ATypeName: String; ASize: Integer
  ): TWatchExpectationResult;
begin
  Result := Default(TWatchExpectationResult);
  if ATypeName = #1 then ATypeName := 'Integer';
  Result.ExpResultKind := rkInteger;
  Result.ExpSymKind := skInteger;
  Result.ExpTypeName := ATypeName;
  Result.expIntValue := AExpVal;
  Result.expIntSize  := ASize;
end;

function weCardinal(AExpVal: QWord; ATypeName: String; ASize: Integer
  ): TWatchExpectationResult;
begin
  Result := Default(TWatchExpectationResult);
  if ATypeName = #1 then ATypeName := 'Cardinal';
  Result.ExpResultKind := rkCardinal;
  Result.ExpSymKind := skCardinal;
  Result.ExpTypeName := ATypeName;
  Result.expCardinalValue := AExpVal;
  Result.expCardinalSize  := ASize;
end;

function weSingle(AExpVal: Extended; ATypeName: String
  ): TWatchExpectationResult;
begin
  if ATypeName = #1 then ATypeName := 'Single';
  Result := weFloat(AExpVal, ATypeName);
end;

function weDouble(AExpVal: Extended; ATypeName: String
  ): TWatchExpectationResult;
begin
  if ATypeName = #1 then ATypeName := 'Double';
  Result := weFloat(AExpVal, ATypeName);
end;

function weFloat(AExpVal: Extended; ATypeName: String): TWatchExpectationResult;
begin
  Result := Default(TWatchExpectationResult);
  Result.ExpResultKind := rkFloat;
  Result.ExpSymKind := skFloat;
  Result.ExpTypeName := ATypeName;
  Result.ExpFloatValue := AExpVal;
end;

function weBool(AExpVal: Boolean; ATypeName: String): TWatchExpectationResult;
begin
  Result := Default(TWatchExpectationResult);
  if ATypeName = #1 then ATypeName := 'Boolean';
  Result.ExpResultKind := rkBool;
  Result.ExpSymKind := skBoolean;
  Result.ExpTypeName := ATypeName;
  Result.ExpBoolValue := AExpVal;
end;

function weSizedBool(AExpVal: Boolean; ATypeName: String
  ): TWatchExpectationResult;
begin
  Result := Default(TWatchExpectationResult);
  Result.ExpResultKind := rkSizedBool;
  Result.ExpSymKind := skBoolean;
  Result.ExpTypeName := ATypeName;
  Result.ExpBoolValue := AExpVal;
end;

function weEnum(AExpVal: string; ATypeName: String): TWatchExpectationResult;
begin
  Result := Default(TWatchExpectationResult);
  if ATypeName = #1 then ATypeName := '';
  Result.ExpResultKind := rkEnum;
  Result.ExpSymKind := skEnum;
  Result.ExpTypeName := ATypeName;
  Result.ExpTextData := AExpVal;
end;

function weSet(const AExpVal: array of string; ATypeName: String
  ): TWatchExpectationResult;
var
  i: Integer;
begin
  Result := Default(TWatchExpectationResult);
  if ATypeName = #1 then ATypeName := '';
  Result.ExpResultKind := rkSet;
  Result.ExpSymKind := skSet;
  Result.ExpTypeName := ATypeName;
  SetLength(Result.ExpSetData, Length(AExpVal));
  for i := 0 to Length(AExpVal) - 1 do
    Result.ExpSetData[i] := AExpVal[i];
end;

function weChar(AExpVal: char; ATypeName: String): TWatchExpectationResult;
begin
  Result := Default(TWatchExpectationResult);
  if ATypeName = #1 then ATypeName := 'Char';
  Result.ExpResultKind := rkChar;
  Result.ExpSymKind := skChar;
  Result.ExpTypeName := ATypeName;
  Result.ExpTextData := AExpVal;
end;

function weWideChar(AExpVal: char; ATypeName: String): TWatchExpectationResult;
begin
  Result := Default(TWatchExpectationResult);
  if ATypeName = #1 then ATypeName := 'WideChar';
  Result.ExpResultKind := rkChar;
  Result.ExpSymKind := skChar;
  Result.ExpTypeName := ATypeName;
  Result.ExpTextData := AExpVal;
end;

function weAnsiStr(AExpVal: string; ATypeName: String): TWatchExpectationResult;
begin
  Result := Default(TWatchExpectationResult);
  if ATypeName = #1 then ATypeName := 'AnsiString';
  Result.ExpResultKind := rkAnsiString;
  Result.ExpSymKind := skAnsiString;
  Result.ExpTypeName := ATypeName;
  Result.ExpTextData := AExpVal;
end;

function weShortStr(AExpVal: string; ATypeName: String
  ): TWatchExpectationResult;
begin
  Result := Default(TWatchExpectationResult);
  if ATypeName = #1 then ATypeName := 'ShortString';
  Result.ExpResultKind := rkShortString;
  Result.ExpSymKind := skString;
  Result.ExpTypeName := ATypeName;
  Result.ExpTextData := AExpVal;
end;

function weWideStr(AExpVal: string; ATypeName: String): TWatchExpectationResult;
begin
  Result := Default(TWatchExpectationResult);
  if ATypeName = #1 then ATypeName := 'WideString';
  Result.ExpResultKind := rkWideString;
  Result.ExpSymKind := skWideString;
  Result.ExpTypeName := ATypeName;
  Result.ExpTextData := AExpVal;
end;

function weUniStr(AExpVal: string; ATypeName: String): TWatchExpectationResult;
begin
  Result := Default(TWatchExpectationResult);
  if ATypeName = #1 then ATypeName := 'UnicodeString';
  Result.ExpResultKind := rkWideString;
  Result.ExpSymKind := skWideString;
  Result.ExpTypeName := ATypeName;
  Result.ExpTextData := AExpVal;
end;

function wePointer(ATypeName: String): TWatchExpectationResult;
begin
  Result := Default(TWatchExpectationResult);
  Result.ExpResultKind := rkPointer;
  Result.ExpSymKind := skPointer;
  Result.ExpTypeName := ATypeName;
end;

function wePointer(AExpVal: TWatchExpectationResult; ATypeName: String
  ): TWatchExpectationResult;
begin
  Result := Default(TWatchExpectationResult);
  Result.ExpResultKind := rkPointer;
  Result.ExpSymKind := skPointer;
  Result.ExpTypeName := ATypeName;
  SetLength(Result.ExpSubResults, 1);
  Result.ExpSubResults[0] := AExpVal;
end;

function wePointerAddr(AExpVal: Pointer; ATypeName: String
  ): TWatchExpectationResult;
begin
  Result := Default(TWatchExpectationResult);
  Result.ExpResultKind := rkPointerAddr;
  Result.ExpSymKind := skPointer;
  Result.ExpTypeName := ATypeName;
  Result.ExpPointerValue := AExpVal;
end;

function wePointerAddr(AExpVal: Pointer; AExpSubVal: TWatchExpectationResult;
  ATypeName: String): TWatchExpectationResult;
begin
  Result := Default(TWatchExpectationResult);
  Result.ExpResultKind := rkPointerAddr;
  Result.ExpSymKind := skPointer;
  Result.ExpTypeName := ATypeName;
  Result.ExpPointerValue := AExpVal;
  SetLength(Result.ExpSubResults, 1);
  Result.ExpSubResults[0] := AExpSubVal;
end;

function weArray(const AExpVal: array of TWatchExpectationResult; AExpFullLen: Integer;
  ATypeName: String): TWatchExpectationResult;
var
  i: Integer;
begin
  Result := Default(TWatchExpectationResult);
  Result.ExpResultKind := rkArray;
  Result.ExpSymKind := skArray;
  Result.ExpTypeName := ATypeName;
  Result.ExpFullArrayLen := AExpFullLen;
  SetLength(Result.ExpSubResults, Length(AExpVal));
  for i := 0 to high(AExpVal) do
    Result.ExpSubResults[i] := AExpVal[i];
end;

function weStatArray(const AExpVal: array of TWatchExpectationResult;
  ATypeName: String): TWatchExpectationResult;
var
  i: Integer;
begin
  Result := Default(TWatchExpectationResult);
  Result.ExpResultKind := rkStatArray;
  Result.ExpSymKind := skArray;
  Result.ExpTypeName := ATypeName;
  Result.ExpFullArrayLen := Length(AExpVal);
  SetLength(Result.ExpSubResults, Length(AExpVal));
  for i := 0 to high(AExpVal) do
    Result.ExpSubResults[i] := AExpVal[i];
end;

function weStatArray(const AExpVal: array of TWatchExpectationResult;
  AExpFullLen: Integer; ATypeName: String): TWatchExpectationResult;
var
  i: Integer;
begin
  Result := Default(TWatchExpectationResult);
  Result.ExpResultKind := rkStatArray;
  Result.ExpSymKind := skArray;
  Result.ExpTypeName := ATypeName;
  Result.ExpFullArrayLen := AExpFullLen;
  SetLength(Result.ExpSubResults, Length(AExpVal));
  for i := 0 to high(AExpVal) do
    Result.ExpSubResults[i] := AExpVal[i];
end;

function weDynArray(const AExpVal: array of TWatchExpectationResult;
  ATypeName: String): TWatchExpectationResult;
var
  i: Integer;
begin
  Result := Default(TWatchExpectationResult);
  Result.ExpResultKind := rkDynArray;
  Result.ExpSymKind := skArray;
  Result.ExpTypeName := ATypeName;
  Result.ExpFullArrayLen := Length(AExpVal);
  SetLength(Result.ExpSubResults, Length(AExpVal));
  for i := 0 to high(AExpVal) do
    Result.ExpSubResults[i] := AExpVal[i];
end;

function weDynArray(const AExpVal: array of TWatchExpectationResult;
  AExpFullLen: Integer; ATypeName: String): TWatchExpectationResult;
var
  i: Integer;
begin
  Result := Default(TWatchExpectationResult);
  Result.ExpResultKind := rkDynArray;
  Result.ExpSymKind := skArray;
  Result.ExpTypeName := ATypeName;
  Result.ExpFullArrayLen := AExpFullLen;
  SetLength(Result.ExpSubResults, Length(AExpVal));
  for i := 0 to high(AExpVal) do
    Result.ExpSubResults[i] := AExpVal[i];
end;

function weChar(const AExpVal: array of char; ATypeName: String
  ): TWatchExpectationResultArray;
var
  i: Integer;
begin
  SetLength(Result, Length(AExpVal));
  for i := 0 to Length(AExpVal) - 1 do
    Result[i] := weChar(AExpVal[i], ATypeName);
end;

function weWideChar(const AExpVal: array of char; ATypeName: String
  ): TWatchExpectationResultArray;
var
  i: Integer;
begin
  SetLength(Result, Length(AExpVal));
  for i := 0 to Length(AExpVal) - 1 do
    Result[i] := weWideChar(AExpVal[i], ATypeName);
end;

function weInteger(const AExpVal: array of Int64; ATypeName: String;
  ASize: Integer): TWatchExpectationResultArray;
var
  i: Integer;
begin
  SetLength(Result, Length(AExpVal));
  for i := 0 to Length(AExpVal) - 1 do
    Result[i] := weInteger(AExpVal[i], ATypeName, ASize);
end;

function weCardinal(const AExpVal: array of QWord; ATypeName: String;
  ASize: Integer): TWatchExpectationResultArray;
var
  i: Integer;
begin
  SetLength(Result, Length(AExpVal));
  for i := 0 to Length(AExpVal) - 1 do
    Result[i] := weCardinal(AExpVal[i], ATypeName, ASize);
end;

function weAnsiStr(const AExpVal: array of string; ATypeName: String
  ): TWatchExpectationResultArray;
var
  i: Integer;
begin
  SetLength(Result, Length(AExpVal));
  for i := 0 to Length(AExpVal) - 1 do
    Result[i] := weAnsiStr(AExpVal[i], ATypeName);
end;

function weShortStr(const AExpVal: array of string; ATypeName: String
  ): TWatchExpectationResultArray;
var
  i: Integer;
begin
  SetLength(Result, Length(AExpVal));
  for i := 0 to Length(AExpVal) - 1 do
    Result[i] := weShortStr(AExpVal[i], ATypeName);
end;

function weBool(const AExpVal: array of Boolean; ATypeName: String
  ): TWatchExpectationResultArray;
var
  i: Integer;
begin
  SetLength(Result, Length(AExpVal));
  for i := 0 to Length(AExpVal) - 1 do
    Result[i] := weBool(AExpVal[i], ATypeName);
end;

function weEnum(const AExpVal: array of string; ATypeName: String
  ): TWatchExpectationResultArray;
var
  i: Integer;
begin
  SetLength(Result, Length(AExpVal));
  for i := 0 to Length(AExpVal) - 1 do
    Result[i] := weEnum(AExpVal[i], ATypeName);
end;

function weRecord(AExpFields: array of TWatchExpectationResult;
  ATypeName: String): TWatchExpectationResult;
var
  i: Integer;
begin
  Result := Default(TWatchExpectationResult);
  Result.ExpResultKind := rkRecord;
  Result.ExpSymKind := skRecord;
  Result.ExpTypeName := ATypeName;
  Result.ExpFullArrayLen := Length(AExpFields);
  SetLength(Result.ExpSubResults, Length(AExpFields));
  for i := 0 to high(AExpFields) do
    Result.ExpSubResults[i] := AExpFields[i];
end;

function weClass(AExpFields: array of TWatchExpectationResult; ATypeName: String
  ): TWatchExpectationResult;
var
  i: Integer;
begin
  Result := Default(TWatchExpectationResult);
  Result.ExpResultKind := rkClass;
  Result.ExpSymKind := skClass;
  Result.ExpTypeName := ATypeName;
  Result.ExpFullArrayLen := Length(AExpFields);
  SetLength(Result.ExpSubResults, Length(AExpFields));
  for i := 0 to high(AExpFields) do
    Result.ExpSubResults[i] := AExpFields[i];
end;

function weObject(AExpFields: array of TWatchExpectationResult;
  ATypeName: String): TWatchExpectationResult;
var
  i: Integer;
begin
  Result := Default(TWatchExpectationResult);
  Result.ExpResultKind := rkObject;
  Result.ExpSymKind := skObject;
  Result.ExpTypeName := ATypeName;
  Result.ExpFullArrayLen := Length(AExpFields);
  SetLength(Result.ExpSubResults, Length(AExpFields));
  for i := 0 to high(AExpFields) do
    Result.ExpSubResults[i] := AExpFields[i];
end;

function weInterface(AExpFields: array of TWatchExpectationResult;
  ATypeName: String): TWatchExpectationResult;
var
  i: Integer;
begin
  Result := Default(TWatchExpectationResult);
  Result.ExpResultKind := rkInterface;
  Result.ExpSymKind := skInterface;
  Result.ExpTypeName := ATypeName;
  Result.ExpFullArrayLen := Length(AExpFields);
  SetLength(Result.ExpSubResults, Length(AExpFields));
  for i := 0 to high(AExpFields) do
    Result.ExpSubResults[i] := AExpFields[i];
end;

{ TWatchExpectationResult }

function TWatchExpectationResult.AddFlag(AFlag: TWatchExpErrorHandlingFlag;
  ASymTypes: TSymbolTypes): TWatchExpectationResult;
var
  i: TSymbolType;
begin
  if ASymTypes = [] then ASymTypes := [low(ASymTypes)..high(ASymTypes)];
  for i := low(ASymTypes) to high(ASymTypes) do
    if i in ASymTypes then
      ExpErrorHandlingFlags[i] := ExpErrorHandlingFlags[i] + [AFlag];
  Result := Self;
end;

function TWatchExpectationResult.AddFlag(AFlags: TWatchExpErrorHandlingFlags;
  ASymTypes: TSymbolTypes): TWatchExpectationResult;
var
  i: TSymbolType;
begin
  if ASymTypes = [] then ASymTypes := [low(ASymTypes)..high(ASymTypes)];
  for i := low(ASymTypes) to high(ASymTypes) do
    if i in ASymTypes then
      ExpErrorHandlingFlags[i] := ExpErrorHandlingFlags[i] + AFlags;
  Result := Self;
end;

function TWatchExpectationResult.N(AFieldName: String): TWatchExpectationResult;
begin
  Self.ExpFieldName := AFieldName;
  Result := Self;
end;

function TWatchExpectationResult.Skip(ASymTypes: TSymbolTypes
  ): TWatchExpectationResult;
begin
  Result := Self.AddFlag(ehTestSkip, ASymTypes);
end;

function TWatchExpectationResult.SkipIf(ACond: Boolean; ASymTypes: TSymbolTypes
  ): TWatchExpectationResult;
begin
  if ACond then
    Result := Self.AddFlag(ehTestSkip, ASymTypes)
  else
    Result := Self;
end;

function TWatchExpectationResult.IgnAll(ASymTypes: TSymbolTypes
  ): TWatchExpectationResult;
begin
  Result := Self.AddFlag(ehIgnAll, ASymTypes);
end;

function TWatchExpectationResult.IgnData(ASymTypes: TSymbolTypes
  ): TWatchExpectationResult;
begin
  Result := Self.AddFlag(ehIgnData, ASymTypes);
end;

function TWatchExpectationResult.IgnKind(ASymTypes: TSymbolTypes
  ): TWatchExpectationResult;
begin
  Result := Self.AddFlag(ehIgnKind, ASymTypes);
end;

function TWatchExpectationResult.IgnKindPtr(ASymTypes: TSymbolTypes
  ): TWatchExpectationResult;
begin
  Result := Self.AddFlag(ehIgnKindPtr, ASymTypes);
end;

function TWatchExpectationResult.IgnKindArrayType(ASymTypes: TSymbolTypes
  ): TWatchExpectationResult;
begin
  Result := Self.AddFlag(ehIgnKindArrayType, ASymTypes);
end;

function TWatchExpectationResult.IgnTypeName(ASymTypes: TSymbolTypes
  ): TWatchExpectationResult;
begin
  Result := Self.AddFlag(ehIgnTypeName, ASymTypes);
end;

function TWatchExpectationResult.MatchTypeName(ASymTypes: TSymbolTypes
  ): TWatchExpectationResult;
begin
  Result := Self.AddFlag(ehMatchTypeName, ASymTypes);
end;

function TWatchExpectationResult.CharFromIndex(ASymTypes: TSymbolTypes
  ): TWatchExpectationResult;
begin
  Result := Self.AddFlag(ehCharFromIndex, ASymTypes);
end;

function TWatchExpectationResult.ChrIdxExpString(ASymTypes: TSymbolTypes
  ): TWatchExpectationResult;
begin
  Result := Self.AddFlag(ehChrIdxExpString, ASymTypes);
end;

function TWatchExpectationResult.ChrIdxExpPChar(ASymTypes: TSymbolTypes
  ): TWatchExpectationResult;
begin
  Result := Self.AddFlag(ehChrIdxExpPChar, ASymTypes);
end;

function TWatchExpectationResult.ChrIdxSkip(ASymTypes: TSymbolTypes
  ): TWatchExpectationResult;
begin
  Result := Self.AddFlag(ehChrIdxSkip, ASymTypes);
end;

function TWatchExpectationResult.ExpectNotFound(ASymTypes: TSymbolTypes
  ): TWatchExpectationResult;
begin
  Result := Self.AddFlag(ehExpectNotFound, ASymTypes);
end;

function TWatchExpectationResult.ExpectError(ASymTypes: TSymbolTypes
  ): TWatchExpectationResult;
begin
  Result := Self.AddFlag(ehExpectError, ASymTypes);
end;

function TWatchExpectationResult.NotImplemented(ASymTypes: TSymbolTypes
  ): TWatchExpectationResult;
begin
  Result := Self.AddFlag(ehNotImplemented, ASymTypes);
end;

function TWatchExpectationResult.NotImplementedData(ASymTypes: TSymbolTypes
  ): TWatchExpectationResult;
begin
  Result := Self.AddFlag(ehNotImplementedData, ASymTypes);
end;

procedure TWatchExpectationResult.MakeCopy;
var
  i: Integer;
begin
  ExpSubResults := copy(ExpSubResults, low(ExpSubResults), high(ExpSubResults)-low(ExpSubResults)+1);
  ExpSetData := copy(ExpSetData, low(ExpSetData), high(ExpSetData)-low(ExpSetData)+1);
  for i := low(ExpSubResults) to high(ExpSubResults) do
    ExpSubResults[i].MakeCopy;
end;

{ TWatchExpectationHelper }

function TWatchExpectationHelper.AddFlag(AFlag: TWatchExpErrorHandlingFlag;
  ASymTypes: TSymbolTypes; ACond: Boolean): PWatchExpectation;
begin
  if ACond then
    Result := Self^.AddFlag(AFlag, ASymTypes)
  else
    Result := Self;
end;

function TWatchExpectationHelper.AddFlag(AFlags: TWatchExpErrorHandlingFlags;
  ASymTypes: TSymbolTypes; ACond: Boolean): PWatchExpectation;
begin
  if ACond then
    Result := Self^.AddFlag(AFlags, ASymTypes)
  else
    Result := Self;
end;

function TWatchExpectationHelper.AddFlag(AFlag: TWatchExpErrorHandlingFlag;
  ACond: Boolean): PWatchExpectation;
begin
  if ACond then
    Result := Self^.AddFlag(AFlag, [])
  else
    Result := Self;
end;

function TWatchExpectationHelper.AddFlag(AFlags: TWatchExpErrorHandlingFlags;
  ACond: Boolean): PWatchExpectation;
begin
  if ACond then
    Result := Self^.AddFlag(AFlags, [])
  else
    Result := Self;
end;

function TWatchExpectationHelper.AddEvalFlag(AFlags: TWatcheEvaluateFlags;
  ACond: Boolean): PWatchExpectation;
begin
  Result := Self;
  if not ACond then
    exit;

  Self^.TstWatch.EvaluateFlags := Self^.TstWatch.EvaluateFlags + AFlags;
  Self^.EvalCallTestFlags := Self^.EvalCallTestFlags + AFlags;
end;

function TWatchExpectationHelper.Skip(ASymTypes: TSymbolTypes
  ): PWatchExpectation;
begin
  Result := Self^.AddFlag(ehTestSkip, ASymTypes);
end;

function TWatchExpectationHelper.SkipIf(ACond: Boolean; ASymTypes: TSymbolTypes
  ): PWatchExpectation;
begin
  if ACond then
    Result := Self^.AddFlag(ehTestSkip, ASymTypes)
  else
    Result := Self;
end;

function TWatchExpectationHelper.SkipEval: PWatchExpectation;
begin
  Self^.SkipEvalCall := True;
  Result := Self;
end;

function TWatchExpectationHelper.IgnAll(ASymTypes: TSymbolTypes; ACond: Boolean
  ): PWatchExpectation;
begin
  if not ACond then exit(Self);
  Result := Self^.AddFlag(ehIgnAll, ASymTypes);
end;

function TWatchExpectationHelper.IgnData(ASymTypes: TSymbolTypes; ACond: Boolean): PWatchExpectation;
begin
  if not ACond then exit(Self);
  Result := Self^.AddFlag(ehIgnData, ASymTypes);
end;

function TWatchExpectationHelper.IgnKind(ASymTypes: TSymbolTypes; ACond: Boolean): PWatchExpectation;
begin
  if not ACond then exit(Self);
  Result := Self^.AddFlag(ehIgnKind, ASymTypes);
end;

function TWatchExpectationHelper.IgnKindPtr(ASymTypes: TSymbolTypes; ACond: Boolean): PWatchExpectation;
begin
  if not ACond then exit(Self);
  Result := Self^.AddFlag(ehIgnKindPtr, ASymTypes);
end;

function TWatchExpectationHelper.IgnTypeName(ASymTypes: TSymbolTypes; ACond: Boolean): PWatchExpectation;
begin
  if not ACond then exit(Self);
  Result := Self^.AddFlag(ehIgnTypeName, ASymTypes);
end;

function TWatchExpectationHelper.MatchTypeName(ASymTypes: TSymbolTypes; ACond: Boolean): PWatchExpectation;
begin
  if not ACond then exit(Self);
  Result := Self^.AddFlag(ehMatchTypeName, ASymTypes);
end;

function TWatchExpectationHelper.NoCharQuoting(ASymTypes: TSymbolTypes;
  ACond: Boolean): PWatchExpectation;
begin
  if not ACond then exit(Self);
  Result := Self^.AddFlag(ehNoCharQuoting, ASymTypes);
end;

function TWatchExpectationHelper.CharFromIndex(ASymTypes: TSymbolTypes; ACond: Boolean): PWatchExpectation;
begin
  if not ACond then exit(Self);
  Result := Self^.AddFlag(ehCharFromIndex, ASymTypes);
end;

function TWatchExpectationHelper.ChrIdxExpString(ASymTypes: TSymbolTypes;
  ACond: Boolean): PWatchExpectation;
begin
  if not ACond then exit(Self);
  Result := Self^.AddFlag(ehChrIdxExpString, ASymTypes);
end;

function TWatchExpectationHelper.ChrIdxExpPChar(ASymTypes: TSymbolTypes;
  ACond: Boolean): PWatchExpectation;
begin
  if not ACond then exit(Self);
  Result := Self^.AddFlag(ehChrIdxExpPChar, ASymTypes);
end;

function TWatchExpectationHelper.ChrIdxSkip(ASymTypes: TSymbolTypes;
  ACond: Boolean): PWatchExpectation;
begin
  if not ACond then exit(Self);
  Result := Self^.AddFlag(ehChrIdxSkip, ASymTypes);
end;

function TWatchExpectationHelper.ExpectNotFound(ASymTypes: TSymbolTypes; ACond: Boolean): PWatchExpectation;
begin
  if not ACond then exit(Self);
  Result := Self^.AddFlag(ehExpectNotFound, ASymTypes);
end;

function TWatchExpectationHelper.ExpectError(ASymTypes: TSymbolTypes; ACond: Boolean): PWatchExpectation;
begin
  if not ACond then exit(Self);
  Result := Self^.AddFlag(ehExpectError, ASymTypes);
end;

function TWatchExpectationHelper.NotImplemented(ASymTypes: TSymbolTypes; ACond: Boolean): PWatchExpectation;
begin
  if not ACond then exit(Self);
  Result := Self^.AddFlag(ehNotImplemented, ASymTypes);
end;

function TWatchExpectationHelper.NotImplementedData(ASymTypes: TSymbolTypes; ACond: Boolean): PWatchExpectation;
begin
  if not ACond then exit(Self);
  Result := Self^.AddFlag(ehNotImplementedData, ASymTypes);
end;

{ TWatchExpectation }

function TWatchExpectation.AddFlag(AFlag: TWatchExpErrorHandlingFlag;
  ASymTypes: TSymbolTypes): PWatchExpectation;
var
  i: TSymbolType;
begin
  if ASymTypes = [] then ASymTypes := [low(ASymTypes)..high(ASymTypes)];
  for i := low(ASymTypes) to high(ASymTypes) do
    if i in ASymTypes then
      TstExpected.ExpErrorHandlingFlags[i] := TstExpected.ExpErrorHandlingFlags[i] + [AFlag];
  Result := @Self;
end;

function TWatchExpectation.AddFlag(AFlags: TWatchExpErrorHandlingFlags;
  ASymTypes: TSymbolTypes): PWatchExpectation;
var
  i: TSymbolType;
begin
  if ASymTypes = [] then ASymTypes := [low(ASymTypes)..high(ASymTypes)];
  for i := low(ASymTypes) to high(ASymTypes) do
    if i in ASymTypes then
      TstExpected.ExpErrorHandlingFlags[i] := TstExpected.ExpErrorHandlingFlags[i] + AFlags;
  Result := @Self;
end;

{ TWatchExpectationList }

function TWatchExpectationList.GetDebugger: TTestDbgDebugger;
begin
  Result := FTest.Debugger;
end;

function TWatchExpectationList.GetCompiler: TTestDbgCompiler;
begin
  Result := FTest.Compiler;
end;

function TWatchExpectationList.GetLazDebugger: TDebuggerIntf;
begin
  Result := Debugger.LazDebugger;
end;

function TWatchExpectationList.GetTests(Index: Integer): PWatchExpectation;
begin
  if Index < 0 then
    Index := Index + Count;
  Result := @FList[Index];
end;

function TWatchExpectationList.ParseCommaList(AVal: String; out
  AFoundCount: Integer; AMaxLen: Integer; AComma: char): TStringArray;
var
  i, BracketLvl, SquareLvl: Integer;
  InQuote: Boolean;
begin
  if AMaxLen < 0 then
    SetLength(Result, Length(AVal) div 2)
  else
    SetLength(Result, AMaxLen);

  i := 1;
  AFoundCount := 0;
  BracketLvl := 0;
  SquareLvl := 0;
  InQuote := false;
  while length(AVal) > 0 do begin
    while (i <= length(AVal)) and (
        (AVal[i] <> AComma) or (BracketLvl > 0) or (SquareLvl > 0) or InQuote
      )
    do begin
      case AVal[i] of
        '(': if not InQuote then inc(BracketLvl);
        ')': if not InQuote then dec(BracketLvl);
        '[': if not InQuote then inc(SquareLvl);
        ']': if not InQuote then dec(SquareLvl);
        '''':
          if InQuote and (i < length(AVal)) and (AVal[i+1] = '''') then inc(i)
          else InQuote := not InQuote;
      end;
      inc(i);
    end;

    //if (AMaxLen >=0) and (AFoundCount >= AMaxLen) then begin
    //  AFoundCount := -1; // more than expected
    //  exit;
    //end;

    if AFoundCount < Length(Result) then
      Result[AFoundCount] := copy(AVal, 1, i-1);
    inc(AFoundCount);
    while (i < length(AVal)) and (AVal[i+1] in [#1..#32]) do
      inc(i);
    delete(AVal, 1, i);
    i := 1;
    //if (length(AVal) > 0) and (AVal[1] = ' ') then
    //  delete(AVal, 1, 1);
  end;
end;

function TWatchExpectationList.GetExpTypeNameAsRegEx(
  AnExpect: TWatchExpectationResult): String;
begin
  if AnExpect.ExpTypeName = #1 then
    Result := ''
  else
    Result := QuoteRegExprMetaChars(AnExpect.ExpTypeName);

  if ehIgnTypeNameInData in AnExpect.ExpErrorHandlingFlags[Compiler.SymbolType] then
    Result := '[a-zA-Z0-9_]*';
end;

procedure TWatchExpectationList.EvalCallback(Sender: TObject;
  ASuccess: Boolean; ResultText: String; ResultDBGType: TDBGType);
begin
  if FCurEvalCallWatchExp = nil then begin
    DebugLn('???????? Late result');
    exit;
  end;
  FCurEvalCallWatchExp^.EvalCallResSuccess := ASuccess;
  FCurEvalCallWatchExp^.EvalCallResText    := ResultText;
  FCurEvalCallWatchExp^.EvalCallResDBGType := ResultDBGType;
  FCurEvalCallWatchExp^.EvalCallResReceived := True;
end;

function TWatchExpectationList.EvaluateWatch(AWatchExp: PWatchExpectation;
  AThreadId: Integer; constref CurLoc: TDBGLocationRec; AWaitForEval: Boolean
  ): Boolean;
var
  i: Integer;
begin
  //with CurLoc do
  //  FTest.LogText('###### ' + AWatchExp^.TstTestName + ' // ' + AWatchExp^.TstWatch.Expression +
  //    ' (AT '+ SrcFile + ':' + IntToStr(SrcLine) +')' +
  //    '###### '+LineEnding);
  AWatchExp^.TstWatch.Values[AThreadId, AWatchExp^.TstStackFrame].Value;
  if not AWaitForEval then
    exit(True);

  for i := 1 to 50 do begin
    Application.Idle(False);
    Result := AWatchExp^.TstWatch.Values[AThreadId, AWatchExp^.TstStackFrame].Validity <> ddsRequested;
    if Result then break;
    WaitWhileEval;
  end;
  //FTest.LogText('<<<<< ' + dbgs(AWatchExp^.TstWatch.Values[AThreadId, AWatchExp^.TstStackFrame].Validity) + ': ' +
  //  AWatchExp^.TstWatch.Values[AThreadId, AWatchExp^.TstStackFrame].Value );
end;

function TWatchExpectationList.EvaluateExprCmd(AWatchExp: PWatchExpectation;
  AThreadId: Integer; constref CurLoc: TDBGLocationRec; AWaitForEval: Boolean
  ): Boolean;
var
  i: Integer;
begin
  Result := true;
  if AWatchExp^.SkipEvalCall then
    exit;

  if AWatchExp^.EvalCallTestFlags <> [] then begin
    with CurLoc do
      FTest.LogText('###### ' + AWatchExp^.TstTestName + ' // ' + AWatchExp^.TstWatch.Expression +
        ' (AT '+ SrcFile + ':' + IntToStr(SrcLine) +')' +
        '###### '+LineEnding);
      // TODO: set thread/stack
    FCurEvalCallWatchExp := AWatchExp;
    AWatchExp^.EvalCallResReceived := False;

    LazDebugger.Evaluate(AWatchExp^.TstWatch.Expression, @EvalCallback, AWatchExp^.EvalCallTestFlags);

    for i := 1 to 5 do begin
      Application.Idle(False);
      Result := AWatchExp^.EvalCallResReceived;
      if Result then break;
      WaitWhileEval;
    end;

    FTest.LogText('<<<<< CB:'+ dbgs(AWatchExp^.EvalCallResReceived)+ ' Res'+ dbgs(AWatchExp^.EvalCallResSuccess)+
      ' Tp:'+dbgs(AWatchExp^.EvalCallResDBGType <> nil)+ ' '+ AWatchExp^.EvalCallResText  );
  end;

  FCurEvalCallWatchExp := nil;
end;

procedure TWatchExpectationList.WaitWhileEval;
begin
  FTest.Debugger.WaitForFinishRun(25, True);
end;

function TWatchExpectationList.TestMatches(Name: string; Expected, Got: string;
  AContext: TWatchExpTestCurrentData; AIgnoreReason: String): Boolean;
begin
  Result := FTest.TestMatches(Name, Expected, Got, AContext.WatchExp.TstMinDbg, AContext.WatchExp.TstMinFpc, AIgnoreReason);
end;

function TWatchExpectationList.TestMatches(Name: string; Expected, Got: string;
  ACaseSense: Boolean; AContext: TWatchExpTestCurrentData; AIgnoreReason: String
  ): Boolean;
begin
  Result := FTest.TestMatches(Name, Expected, Got, ACaseSense, AContext.WatchExp.TstMinDbg, AContext.WatchExp.TstMinFpc, AIgnoreReason);
end;

function TWatchExpectationList.TestEquals(Name: string; Expected, Got: string;
  AContext: TWatchExpTestCurrentData; AIgnoreReason: String): Boolean;
begin
  Result := FTest.TestEquals(Name, Expected, Got, AContext.WatchExp.TstMinDbg, AContext.WatchExp.TstMinFpc, AIgnoreReason);
end;

function TWatchExpectationList.TestEquals(Name: string; Expected, Got: string;
  ACaseSense: Boolean; AContext: TWatchExpTestCurrentData; AIgnoreReason: String
  ): Boolean;
begin
  Result := FTest.TestEquals(Name, Expected, Got, ACaseSense, AContext.WatchExp.TstMinDbg, AContext.WatchExp.TstMinFpc, AIgnoreReason);
end;

function TWatchExpectationList.TestEquals(Name: string; Expected, Got: int64;
  AContext: TWatchExpTestCurrentData; AIgnoreReason: String): Boolean;
begin
  Result := FTest.TestEquals(Name, Expected, Got, AContext.WatchExp.TstMinDbg, AContext.WatchExp.TstMinFpc, AIgnoreReason);
end;

function TWatchExpectationList.TestEquals(Name: string; Expected, Got: QWord;
  AContext: TWatchExpTestCurrentData; AIgnoreReason: String): Boolean;
begin
  Result := FTest.TestEquals(Name, Expected, Got, AContext.WatchExp.TstMinDbg, AContext.WatchExp.TstMinFpc, AIgnoreReason);
end;

function TWatchExpectationList.TestTrue(Name: string; Got: Boolean;
  AContext: TWatchExpTestCurrentData; AIgnoreReason: String): Boolean;
begin
  Result := FTest.TestTrue(Name, Got, AContext.WatchExp.TstMinDbg, AContext.WatchExp.TstMinFpc, AIgnoreReason);
end;

function TWatchExpectationList.TestFalse(Name: string; Got: Boolean;
  AContext: TWatchExpTestCurrentData; AIgnoreReason: String): Boolean;
begin
  Result := FTest.TestFalse(Name, Got, AContext.WatchExp.TstMinDbg, AContext.WatchExp.TstMinFpc, AIgnoreReason);
end;

function TWatchExpectationList.CheckResult(AnWatchExp: TWatchExpectation
  ): Boolean;
var
  Thread, Stack: Integer;
  CurBaseName, AnIgnoreRsn: String;
  WatchVal: TWatchValue;
  Context: TWatchExpTestCurrentData;
  ehf: TWatchExpErrorHandlingFlags;
begin
  Result := False;
  CurBaseName := FTest.TestBaseName;
  Context.WatchExp := AnWatchExp;
  Context.Expectation := AnWatchExp.TstExpected;
  Context.HasTypeInfo := False;
  ehf := Context.Expectation.ExpErrorHandlingFlags[Compiler.SymbolType];
  if ehTestSkip in ehf then
    exit(True);

  with AnWatchExp do begin
    try
      if Debugger.LazDebugger.State in [dsPause, dsInternalPause] then
        with Debugger.CurLocation do
          FTest.TestBaseName := FTest.TestBaseName + ' ' + TstTestName + ' WATCH: '+TstWatch.Expression+' AT '+ SrcFile + ':' + IntToStr(SrcLine) +')';
      if TstStackFrame > 0 then
        FTest.TestBaseName := FTest.TestBaseName + ' (Stack: ' + IntToStr(TstStackFrame) + ') ';
      if not VerifyDebuggerState then
        exit;
      FTest.LogText('###### ' + TstTestName + ' // ' + TstWatch.Expression + '###### '+LineEnding);

      AnIgnoreRsn := '';
      if ehIgnAll in ehf then
        AnIgnoreRsn := AnIgnoreRsn + 'All ignored';
      if ehNotImplemented in ehf then
        AnIgnoreRsn := AnIgnoreRsn + 'Not implemented';

      Thread := LazDebugger.Threads.CurrentThreads.CurrentThreadId;
      Stack  := TstStackFrame;
      WatchVal := TstWatch.Values[Thread, Stack];
      Context.WatchRes := WatchVal.ResultData;

      //debugln([' ## ## ',  WatchVal.Expression]);
      //debugln([FWatchResultPrinter.PrintWatchValue(WatchVal.ResultData,TestWatchDisplayFormat)]); debugln;

      if (Context.WatchRes <> nil) and
         (Context.WatchRes.ValueKind = rdkPCharOrString)
      then begin
        if ehChrIdxExpPChar in ehf then begin
          Context.WatchRes.SetSelectedIndex(0);
          Context.WatchRes := Context.WatchRes.SelectedEntry;
        end
        else
        if ehChrIdxExpString in ehf then begin
          Context.WatchRes.SetSelectedIndex(1);
          Context.WatchRes := Context.WatchRes.SelectedEntry;
        end;
        if ehChrIdxSkip in ehf then
          exit;
      end;

      if (Context.WatchRes <> nil) and
         (Context.WatchRes.ValueKind = rdkConvertRes) and
         (Context.WatchRes.FieldCount > 0)
      then
        Context.WatchRes := Context.WatchRes.Fields[0].Field;

      Context.WatchTpInf := WatchVal.TypeInfo;

      if not VerifyDebuggerState then
        exit;

      if ehExpectError in ehf then begin
//TODO
        if Context.WatchRes.ValueKind = rdkError then
          Result := TestTrue('IsErr', Context.WatchRes.ValueKind = rdkError, Context, AnIgnoreRsn)
        else
          Result := TestTrue('TstWatch.value is NOT valid', WatchVal.Validity in [ddsError, ddsInvalid], Context, AnIgnoreRsn);
        exit;
      end;
      if ehExpectErrorText in ehf then begin
        if Context.WatchRes.ValueKind = rdkError then
          Result := TestTrue('IsErr', Context.WatchRes.ValueKind = rdkError, Context, AnIgnoreRsn)
        else
          Result := TestTrue('TstWatch.value is NOT valid', WatchVal.Validity in [ddsError, ddsInvalid], Context, AnIgnoreRsn);
        Result := CheckData(Context, AnIgnoreRsn, True, True);
        exit;
      end;
      if ehExpectNotFound in ehf then begin
        Result := TestMatches('TstWatch.value NOT found', 'not found', WatchVal.Value, Context, AnIgnoreRsn);
        Result := TestTrue('TstWatch.value NOT found', WatchVal.Validity in [ddsError, ddsInvalid], Context, AnIgnoreRsn);
        exit;
      end;
      if not TestTrue('TstWatch.value is valid', WatchVal.Validity = ddsValid, Context, AnIgnoreRsn) then
        exit;

      if (not (ehNoTypeInfo in ehf)) then begin
        if not(Context.WatchRes.ValueKind in [rdkUnknown, rdkPrePrinted, rdkError])
        then
          Context.HasTypeInfo := True
        else
        if TestTrue('Has TypeInfo', Context.WatchTpInf <> nil, Context, AnIgnoreRsn)
        then
          Context.HasTypeInfo := True;
      end;

      if (EvalCallTestFlags <> []) and not SkipEvalCall then begin
        TestTrue('Got eval res', EvalCallResReceived, Context, AnIgnoreRsn);
        TestTrue('Got eval success', EvalCallResSuccess, Context, AnIgnoreRsn);
//        if (Context.WatchRes.ValueKind in [rdkUnknown, rdkPrePrinted, rdkError]) then
        TestTrue('Got eval type', EvalCallResDBGType <> nil, Context, AnIgnoreRsn);
      end;

      VerifySymType(Context, AnIgnoreRsn);
      VerifyTypeName(Context, AnIgnoreRsn);

      if ehIgnData in ehf then
        AnIgnoreRsn := AnIgnoreRsn + 'Test ignored (Data)';
      if ehNotImplementedData in ehf then
        AnIgnoreRsn := AnIgnoreRsn + 'Not implemented (Data)';

      Result := CheckData(Context, AnIgnoreRsn, False);
    finally
      FTest.TestBaseName := CurBaseName;
    end;
  end;
end;

function TWatchExpectationList.CheckData(AContext: TWatchExpTestCurrentData;
  AnIgnoreRsn: String; AnIsNested: Boolean; ASkipErrCheck: Boolean): Boolean;
var
  ehf: TWatchExpErrorHandlingFlags;
begin
  ehf := AContext.Expectation.ExpErrorHandlingFlags[Compiler.SymbolType];
  AContext.IsNested := AnIsNested;

  if (AContext.WatchRes.ValueKind = rdkVariant) and (AContext.WatchRes.DerefData <> nil) then
    AContext.WatchRes := AContext.WatchRes.DerefData;

  // TODO: can be pre-printed for an error
  if not ASkipErrCheck then begin
    if ehExpectError in ehf then begin
      Result := TestTrue('IsErr', AContext.WatchRes.ValueKind in [rdkError, rdkPrePrinted], AContext, AnIgnoreRsn);
      exit;
    end;
    if ehExpectErrorText in ehf then begin
      Result := TestTrue('IsErr', AContext.WatchRes.ValueKind in [rdkError, rdkPrePrinted], AContext, AnIgnoreRsn);
      Result := CheckResultMatch(AContext, AnIgnoreRsn);
      exit;
    end;
  end;

  case AContext.Expectation.ExpResultKind of
    rkMatch:       Result := CheckResultMatch(AContext, AnIgnoreRsn);
    rkInteger:     Result := CheckResultNum(AContext, False, AnIgnoreRsn);
    rkCardinal:    Result := CheckResultNum(AContext, True, AnIgnoreRsn);
    rkBool, rkSizedBool: Result := CheckResultBool(AContext, AnIgnoreRsn);
    rkFloat:       Result := CheckResultFloat(AContext, AnIgnoreRsn);
    rkEnum:        Result := CheckResultEnum(AContext, AnIgnoreRsn);
    rkSet:         Result := CheckResultSet(AContext, AnIgnoreRsn);
    rkChar:        Result := CheckResultChar(AContext, AnIgnoreRsn);
    rkWideString,
    rkAnsiString:  Result := CheckResultAnsiStr(AContext, AnIgnoreRsn);
    rkShortString: Result := CheckResultShortStr(AContext, AnIgnoreRsn);
    rkPointer:     Result := CheckResultPointer(AContext, AnIgnoreRsn);
    rkPointerAddr: Result := CheckResultPointerAddr(AContext, AnIgnoreRsn);
    rkClass:       Result := CheckResultClass(AContext, AnIgnoreRsn);
    rkObject:      Result := CheckResultObject(AContext, AnIgnoreRsn);
    rkRecord:      Result := CheckResultRecord(AContext, AnIgnoreRsn);
    rkInterface:   Result := CheckResultInterface(AContext, AnIgnoreRsn);
    rkArray:       Result := CheckResultArray(AContext, AnIgnoreRsn);
    rkStatArray:   Result := CheckResultArray(AContext, AnIgnoreRsn);
    rkDynArray:    Result := CheckResultArray(AContext, AnIgnoreRsn);
  end;
end;

function TWatchExpectationList.VerifyDebuggerState: Boolean;
begin
  Result := FTest.TestTrue('Dbg State is paused: '+dbgs(LazDebugger.State), LazDebugger.State in [dsPause, dsInternalPause]);
end;

function TWatchExpectationList.VerifySymType(
  AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean;
var
  Expect: TWatchExpectationResult;
  ehf: TWatchExpErrorHandlingFlags;
  t: TDbgSymbolKind;
  s1, s2, n: string;
begin
  with AContext.WatchExp do begin
    Result := True;
    Expect := AContext.Expectation;

    if (not AContext.HasTypeInfo) then
      exit;

    if (AContext.WatchTpInf = nil) then begin
      TestTrue('ResultValue instead of typeinfo',
        not (AContext.WatchRes.ValueKind in [rdkUnknown, rdkPrePrinted, rdkError]),
        AContext, AnIgnoreRsn
      );
      exit;
    end;

    t := AContext.WatchTpInf.Kind;
    WriteStr(s1, t);
    WriteStr(s2, Expect.ExpSymKind);

    ehf := Expect.ExpErrorHandlingFlags[Compiler.SymbolType];
    if ehIgnKind in ehf then
      AnIgnoreRsn := AnIgnoreRsn + 'Test ignored'
    else
    if (ehIgnKindPtr in ehf) and (t = skPointer) then
      AnIgnoreRsn := 'Ignored by flag (Kind may be Ptr)';
    if ehNotImplementedKind in ehf then
      AnIgnoreRsn := AnIgnoreRsn + 'Not implemented (symkind)';

    n := '';
    if (t = skSimple) and (Expect.ExpSymKind in AcceptSkSimple) then begin
      n := ' (skSimple for '+s2+')';
      s2 := 'skSimple';
    end;

    //if (t in [skRecord, skClass]) and (Expect.ExpSymKind = skObject) then begin
    //  n := ' (skObject for '+s1+')';
    //  s1 := 'skObject';
    //end;
    //if (t in [skClass]) and (Expect.ExpSymKind = skInterface) then begin
    //  n := ' (skInterface for '+s1+')';
    //  s1 := 'skInterface';
    //end;

    Result := TestEquals('SymbolType'+n, s2, s1, AContext, AnIgnoreRsn);
    //if ((s2='skClass') and (s = 'skRecord')) or ((s='skClass') and (s2 = 'skRecord')) then begin
    //  TotalClassVsRecord := TotalClassVsRecord + 1;
  end;
end;

function TWatchExpectationList.VerifyTypeName(
  AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean;
var
  ehf: TWatchExpErrorHandlingFlags;
  Expect: TWatchExpectationResult;
  WtchTpName, ExpTpName, s, n, n2: String;
  i: SizeInt;
begin
  with AContext.WatchExp do begin
    Result := True;
    Expect := AContext.Expectation;

    if (Expect.ExpTypeName = '') or (Expect.ExpTypeName = #1) or (not AContext.HasTypeInfo) then
      exit;

    ehf := Expect.ExpErrorHandlingFlags[Compiler.SymbolType];
    if ehIgnTypeName in ehf then
      AnIgnoreRsn := AnIgnoreRsn + 'Test ignored';
    if ehNotImplementedType in ehf then
      AnIgnoreRsn := AnIgnoreRsn + 'Not implemented (typename)';

    //if vtTypeName in AContext.WatchVal.ValidTypes then
    WtchTpName := AContext.WatchRes.TypeName;
    if (AContext.WatchTpInf <> nil) then begin
      if (WtchTpName = '') then
        WtchTpName := AContext.WatchTpInf.TypeName
      else
        TestEquals('same typename in type-info', WtchTpName, AContext.WatchTpInf.TypeName, False, AContext, AnIgnoreRsn);
    end;

    if ehMatchTypeName in ehf then
      Result := TestMatches('TypeName', Expect.ExpTypeName, WtchTpName, AContext, AnIgnoreRsn)
    else begin
      n := '';
      ExpTpName := Expect.ExpTypeName;
      n2 := FTypeNameAliases.Values[UpperCase(ExpTpName)];
      if n2 <> '' then begin
        n := ' using alias "' + n2 + '" for "' + ExpTpName + '"';
        ExpTpName := n2;
      end;

      i := pos('|', ExpTpName);
      if i > 1 then begin
        n := n + ' in "' + ExpTpName + '"';
        while i > 1 do begin
          s := copy(ExpTpName, 1, i-1);
          delete(ExpTpName, i, i);
          if UpperCase(s) = UpperCase(WtchTpName) then begin
            Result := TestEquals('TypeName'+n, s, WtchTpName, EqIgnoreCase, AContext, AnIgnoreRsn);
            exit;
          end;
          i := pos('|', ExpTpName);
        end;
        if (ExpTpName <> '') and (UpperCase(ExpTpName) = UpperCase(WtchTpName)) then begin
          Result := TestEquals('TypeName'+n, ExpTpName, WtchTpName, EqIgnoreCase, AContext, AnIgnoreRsn);
          exit;
        end;
        Result := TestTrue('TypeName "' + WtchTpName + '"' + n, True, AContext, AnIgnoreRsn);
      end

      else begin
        Result := TestEquals('TypeName'+n, ExpTpName, WtchTpName, EqIgnoreCase, AContext, AnIgnoreRsn);
      end;
    end;
  end;
end;

function TWatchExpectationList.CheckResultMatch(
  AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean;
var
  Expect: TWatchExpectationResult;
begin
  with AContext.WatchExp do begin
    Result := True;
    Expect := AContext.Expectation;

    Result := TestMatches('Data', Expect.ExpTextData, FWatchResultPrinter.PrintWatchValue(AContext.WatchRes, TestWatchDisplayFormat, ''), AContext, AnIgnoreRsn);
  end;
end;

function TWatchExpectationList.CheckResultNum(
  AContext: TWatchExpTestCurrentData; IsCardinal: Boolean; AnIgnoreRsn: String
  ): Boolean;
var
  Expect: TWatchExpectationResult;
  s: String;
begin
  with AContext.WatchExp do begin
    Result := True;
    Expect := AContext.Expectation;
    //AContext.Expectation.ExpSymKind ???

    if AContext.WatchRes.ValueKind = rdkPrePrinted then begin
      if IsCardinal then
        s := IntToStr(Expect.expCardinalValue)
      else
        s := IntToStr(Expect.expIntValue);

      Result := TestEquals('Data', s, FWatchResultPrinter.PrintWatchValue(AContext.WatchRes, TestWatchDisplayFormat, ''), AContext, AnIgnoreRsn);
    end
    else begin
      if IsCardinal then begin
        Result := TestTrue('ValKind', AContext.WatchRes.ValueKind = rdkUnsignedNumVal, AContext, AnIgnoreRsn);
        Result := TestEquals('num Data', Expect.ExpCardinalValue, AContext.WatchRes.AsQWord, AContext, AnIgnoreRsn);

        if (Expect.ExpCardinalSize > 0) then
          TestEquals('DataSize', Expect.ExpCardinalSize, AContext.WatchRes.ByteSize, AContext, AnIgnoreRsn);
      end
      else begin
        Result := TestTrue('ValKind', AContext.WatchRes.ValueKind = rdkSignedNumVal, AContext, AnIgnoreRsn);
        Result := TestEquals('num Data', Expect.ExpIntValue, AContext.WatchRes.AsInt64, AContext, AnIgnoreRsn);

        if (Expect.ExpIntSize > 0) then
          TestEquals('DataSize', Expect.ExpIntSize, AContext.WatchRes.ByteSize, AContext, AnIgnoreRsn);
      end;
    end;
  end;
end;

function TWatchExpectationList.CheckResultBool(
  AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean;
var
  Expect: TWatchExpectationResult;
  s, v: String;
  i: SizeInt;
begin
  with AContext.WatchExp do begin
    Result := True;
    Expect := AContext.Expectation;

    WriteStr(s, Expect.ExpBoolValue);
    v := FWatchResultPrinter.PrintWatchValue(AContext.WatchRes, TestWatchDisplayFormat, '');
    if AContext.Expectation.ExpResultKind = rkSizedBool then begin
      i := pos('(', v);
      if i > 1 then
        delete(v, i, 99); // remove the int value in brackets
    end;
    Result := TestEquals('Data', s, v, False, AContext, AnIgnoreRsn);

  end;
end;

function TWatchExpectationList.CheckResultFloat(
  AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean;
var
  Expect: TWatchExpectationResult;
begin
  with AContext.WatchExp do begin
    Result := True;
    Expect := AContext.Expectation;

    if AContext.WatchRes.ValueKind = rdkPrePrinted then begin
      Result := TestEquals('Data', FloatToStr(Expect.ExpFloatValue), FWatchResultPrinter.PrintWatchValue(AContext.WatchRes, TestWatchDisplayFormat, ''), EqIgnoreCase, AContext, AnIgnoreRsn);
    end
    else begin
      Result := TestTrue('ValKind', AContext.WatchRes.ValueKind = rdkFloatVal, AContext, AnIgnoreRsn);
      Result := TestTrue('Data', CompareValue(AContext.WatchRes.AsFloat, Expect.ExpFloatValue, 0.000001)=0,AContext, AnIgnoreRsn);
    end;
  end;
end;

function TWatchExpectationList.CheckResultEnum(
  AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean;
var
  Expect: TWatchExpectationResult;
begin
  with AContext.WatchExp do begin
    Result := True;
    Expect := AContext.Expectation;

    TestWatchDisplayFormat.Enum.UseInherited := True;
    TestWatchDisplayFormat.EnumVal.UseInherited := True;
    TestWatchDisplayFormat.Enum.MainFormat := vdfEnumName;
    TestWatchDisplayFormat.EnumVal.MainFormat := vdfEnumName;
    if pos('=',  Expect.ExpTextData) > 0 then begin
      TestWatchDisplayFormat.Enum.UseInherited := False;
      TestWatchDisplayFormat.EnumVal.UseInherited := False;
      TestWatchDisplayFormat.Enum.MainFormat := vdfEnumNameAndOrd;
      TestWatchDisplayFormat.EnumVal.MainFormat := vdfEnumNameAndOrd;
    end;

    Result := TestEquals('Data', Expect.ExpTextData, FWatchResultPrinter.PrintWatchValue(AContext.WatchRes, TestWatchDisplayFormat, ''), not(Compiler.SymbolType in stDwarf2), AContext, AnIgnoreRsn);
  end;
end;

function TWatchExpectationList.CheckResultSet(
  AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean;
var
  Expect: TWatchExpectationResult;
  v: String;
  parsed: TStringArray;
  e, i: Integer;
begin
  with AContext.WatchExp do begin
    Result := True;
    Expect := AContext.Expectation;

    v := FWatchResultPrinter.PrintWatchValue(AContext.WatchRes, TestWatchDisplayFormat, '');

    if (v='') or (v[1] <> '[') or (v[length(v)] <> ']') then begin
      Result := TestTrue('elements are in [...]', False, AContext, AnIgnoreRsn);
      exit;
    end;
    delete(v, 1, 1);
    delete(v, length(v), 1);

    parsed := ParseCommaList(v, e, Length(Expect.ExpSetData));
    TestTrue('FieldParser len', e <= Length(parsed), AContext, AnIgnoreRsn);

    Result := TestEquals('Length', Length(Expect.ExpSetData), e, AContext, AnIgnoreRsn);

    e := min(e, Length(parsed));
    for i := 0 to min(e, length(Expect.ExpSetData)) - 1 do
      TestEquals('element'+IntToStr(i), Expect.ExpSetData[i], parsed[i], not(Compiler.SymbolType in stDwarf2), AContext, AnIgnoreRsn);

  end;
end;

function TWatchExpectationList.CheckResultChar(
  AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean;
var
  Expect: TWatchExpectationResult;
  ehf: TWatchExpErrorHandlingFlags;
  e, v: String;
begin
  with AContext.WatchExp do begin
    Result := True;
    Expect := AContext.Expectation;
    ehf := Expect.ExpErrorHandlingFlags[Compiler.SymbolType];

    if ehNoCharQuoting in ehf then
      e := Expect.ExpTextData
    else
      e := QuoteText(Expect.ExpTextData);

    v := FWatchResultPrinter.PrintWatchValue(AContext.WatchRes, TestWatchDisplayFormat, '');
    if ehCharFromIndex in ehf then begin
      if v <> e then begin
//AnIgnoreRsn := AnIgnoreRsn + 'char from index not implemented';
        Result := TestMatches('Data (pchar/string)', '([Pp][Cc]har|[Ss]tring):? *'+e,
          v, EqMatchCase, AContext, AnIgnoreRsn);
        exit;
      end
      else
      if AnIgnoreRsn = '' then
        TestTrue('Expect from Index, yet got only one value', False, AContext, 'Success, better than expected');
    end;

    Result := TestEquals('Data', e, v, AContext, AnIgnoreRsn);
  end;
end;

function TWatchExpectationList.CheckResultAnsiStr(
  AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean;
var
  Expect: TWatchExpectationResult;
  v, e, tn: String;
  ehf: TWatchExpErrorHandlingFlags;
begin
  with AContext.WatchExp do begin
    Result := True;
    Expect := AContext.Expectation;

    if AContext.WatchRes.ValueKind = rdkPrePrinted then begin
      v := FWatchResultPrinter.PrintWatchValue(AContext.WatchRes, TestWatchDisplayFormat, '');
      ehf := Expect.ExpErrorHandlingFlags[Compiler.SymbolType];

      // in dwarf 2 ansistring are pchar
      // widestring are always pwidechar
      if (Compiler.SymbolType in stDwarf2) or (AContext.Expectation.ExpResultKind = rkWideString) then begin
        tn := GetExpTypeNameAsRegEx(Expect);
        if (tn <> '') then begin
          if (Expect.ExpTextData = '') and
             FTest.Matches('^'+tn+'\(nil\)', v) or
             FTest.Matches('^nil$', v)  // new format, no typename
          then
            v := ''''''
          else
          if FTest.Matches('^'+tn+'\(\$[0-9a-fA-F]+\) ', v) then
            delete(v, 1, pos(') ', v)+1)
          else
          if FTest.Matches('^\$[0-9a-fA-F]+(\^:)? ', v) then
            delete(v, 1, pos(' ', v));
        end
        else begin
          if (Expect.ExpTextData = '') and (v = 'nil')
          then
            v := ''''''
          else
          if FTest.Matches('^\$[0-9a-fA-F]+ ', v) then
            delete(v, 1, pos(' ', v));
        end;
      end;

      if ehNoCharQuoting in ehf then
        e := Expect.ExpTextData
      else
        e := QuoteText(Expect.ExpTextData);
      Result := TestEquals('Data', e, v, AContext, AnIgnoreRsn);
    end
    else begin
      if ((Compiler.SymbolType in stDwarf2) or (AContext.Expectation.ExpResultKind = rkWideString)) and
         (AContext.WatchRes.ValueKind = rdkPointerVal)
      then begin
        // got pchar
        if (Expect.ExpTextData = '') and (AContext.WatchRes.DerefData = nil) then // valid nil pointer
          exit;
        Result := TestTrue('Has Deref', AContext.WatchRes.DerefData <> nil, AContext, AnIgnoreRsn);
        if AContext.WatchRes.DerefData = nil then
          exit;

        Result := TestTrue('Deref is string', AContext.WatchRes.DerefData.ValueKind in [rdkString, rdkWideString], AContext, AnIgnoreRsn);
        if not Result then
          exit;
        v := AContext.WatchRes.DerefData.AsString;
      end
      else
      if (AContext.WatchRes.ValueKind = rdkWideString) then begin
        v := AContext.WatchRes.AsWideString;
      end
      else
      if (AContext.WatchRes.ValueKind = rdkString) then begin
        v := AContext.WatchRes.AsString;
      end
      else begin
        TestEquals('got correct type', 'rdk...string', dbgs(AContext.WatchRes.ValueKind), AContext, AnIgnoreRsn);
        exit;
      end;
      Result := TestEquals('Data', Expect.ExpTextData, v, AContext, AnIgnoreRsn);
    end;
  end;
end;

function TWatchExpectationList.CheckResultShortStr(
  AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean;
var
  Expect: TWatchExpectationResult;
  e: String;
  ehf: TWatchExpErrorHandlingFlags;
begin
  with AContext.WatchExp do begin
    Result := True;
    Expect := AContext.Expectation;
    ehf := Expect.ExpErrorHandlingFlags[Compiler.SymbolType];

    if AContext.WatchRes.ValueKind = rdkPrePrinted then begin
      if ehNoCharQuoting in ehf then
        e := Expect.ExpTextData
      else
        e := QuoteText(Expect.ExpTextData);

      Result := TestEquals('Data', e, FWatchResultPrinter.PrintWatchValue(AContext.WatchRes, TestWatchDisplayFormat, ''), AContext, AnIgnoreRsn);
    end
    else begin
      Result := TestTrue('ValueKind', AContext.WatchRes.ValueKind = rdkString, AContext, AnIgnoreRsn);
      Result := TestEquals('Data', Expect.ExpTextData, AContext.WatchRes.AsString, AContext, AnIgnoreRsn);
    end;
  end;
end;

function TWatchExpectationList.CheckResultPointer(
  AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean;
var
  Expect: TWatchExpectationResult;
  g, e, n, tn: String;
  i: SizeInt;
  SubContext: TWatchExpTestCurrentData;
  ehf: TWatchExpErrorHandlingFlags;
begin
  with AContext.WatchExp do begin
    Result := True;
    Expect := AContext.Expectation;
    ehf := Expect.ExpErrorHandlingFlags[Compiler.SymbolType];

    if AContext.WatchRes.ValueKind = rdkPrePrinted then begin
      g := FWatchResultPrinter.PrintWatchValue(AContext.WatchRes, TestWatchDisplayFormat, '');

      e := '(\$[0-9a-fA-F]*\^?|nil)';
      tn := GetExpTypeNameAsRegEx(Expect);
      if (tn <> '') and
         (Length(Expect.ExpSubResults) = 1) and
         (Expect.ExpSubResults[0].ExpResultKind in [rkChar, rkAnsiString, rkWideString, rkShortString]) and
         (not FTest.Matches(tn+'\(', g))
      then
        tn := ''; // char pointer to not (always?) include the type
      if tn <> '' then
        e := tn+'\('+e+'\)';
      e := '^'+e+'\^?([:\s=]|$)';

      Result := TestMatches('Data', e, g, AContext, AnIgnoreRsn);

      if ehIgnPointerDerefData in ehf then
        exit;

      i := pos(' ', g);
      if i > 1 then
        delete(g, 1, i)
      else
      if not((Length(Expect.ExpSubResults) = 1) and (Expect.ExpSubResults[0].ExpResultKind in [rkChar, rkAnsiString, rkWideString, rkShortString]) ) then begin
        TestTrue('nil pointer, but expecting data / internal test correctness', False, AContext, AnIgnoreRsn);
        exit;
      end
      else
      if pos('nil', g) > 0 then
        g := '''''' // only pchar, ... / simulate empty string for nil pointer
      else begin
        TestTrue('pointer has data', False, AContext, AnIgnoreRsn);
        exit;
      end;

      if Length(Expect.ExpSubResults) = 0 then begin
        TestTrue('pointer has expectation / internal test correctness', False, AContext, AnIgnoreRsn);
        exit;
      end;


      n := FTest.TestBaseName;
      SubContext := AContext;
      SubContext.WatchRes := TWatchResultDataPrePrinted.Create(g);
      SubContext.Expectation := Expect.ExpSubResults[0];
      FTest.TestBaseName := n + ' / deref value ';

      Result := CheckData(SubContext, AnIgnoreRsn);

      FreeAndNil(SubContext.WatchRes);
      FTest.TestBaseName := n;
    end
    else begin
      Result := TestTrue('ValKind', AContext.WatchRes.ValueKind = rdkPointerVal, AContext, AnIgnoreRsn);
      //TODO: FLAG for nil/non-nil pointer
      //Result := TestTrue('Not nil', AContext.WatchRes.AsQWord <> 0, AContext, AnIgnoreRsn);

      if (Length(Expect.ExpSubResults) = 1) then begin
        if (Expect.ExpSubResults[0].ExpResultKind in [rkChar, rkAnsiString, rkWideString, rkShortString]) and
           (Expect.ExpSubResults[0].ExpTextData = '')
        then begin
          //TODO: check pointed-to type
          if AContext.WatchRes.DerefData = nil then  // valid nil pointer
            exit;
        end;

        Result := TestTrue('Has Deref', AContext.WatchRes.DerefData <> nil, AContext, AnIgnoreRsn);
        if AContext.WatchRes.DerefData <> nil then begin
          n := FTest.TestBaseName;
          SubContext := AContext;
          SubContext.WatchRes := AContext.WatchRes.DerefData;
          SubContext.Expectation := Expect.ExpSubResults[0];
          FTest.TestBaseName := n + ' / deref value ';

          Result := VerifyTypeName(SubContext, AnIgnoreRsn);
          // TODO: check type specifics, like ValueKind, ByteSize, ...  // ehIgnData
          if not (ehIgnPointerDerefData in ehf) then
            Result := CheckData(SubContext, AnIgnoreRsn);

          FTest.TestBaseName := n;
        end;
      end
      else begin
        TestTrue('SelfTest: ehIgnPointerDerefData for pointer without sub-expect', ehIgnPointerDerefData in ehf, AContext, AnIgnoreRsn);
      end;
    end;
  end;
end;

function TWatchExpectationList.CheckResultPointerAddr(
  AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean;
var
  Expect: TWatchExpectationResult;
  e, tn: String;
  ehf: TWatchExpErrorHandlingFlags;
begin
  with AContext.WatchExp do begin
    Result := True;
    Expect := AContext.Expectation;
    ehf := Expect.ExpErrorHandlingFlags[Compiler.SymbolType];

    if AContext.WatchRes.ValueKind = rdkPrePrinted then begin
      tn := GetExpTypeNameAsRegEx(Expect);

      if Expect.ExpPointerValue = nil then
        e := 'nil'
      else
        e := '\$0*'+IntToHex(PtrUInt(Expect.ExpPointerValue), 8);
      if tn <> '' then
        e := tn+'\('+e+'\)';
      e := '^'+e;

      Result := TestMatches('Data', e, AContext.WatchRes.AsString, AContext, AnIgnoreRsn);
    end
    else begin
      Result := TestTrue('ValKind', AContext.WatchRes.ValueKind = rdkPointerVal, AContext, AnIgnoreRsn);
      Result := TestEquals('Ptr-Addr', TDBGPtr(Expect.ExpPointerValue), AContext.WatchRes.DataAddress, AContext, AnIgnoreRsn);
    end;
  end;
  if (Length(Expect.ExpSubResults) = 1) then
    Result := CheckResultPointer(AContext, AnIgnoreRsn);
end;

function TWatchExpectationList.CheckResultArray(
  AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean;
var
  Expect: TWatchExpectationResult;
  SubContext: TWatchExpTestCurrentData;
  v, n: String;
  parsed: array of String;
  i, e: Integer;
  ehf: TWatchExpErrorHandlingFlags;
  PrePrint: Boolean;
begin
  with AContext.WatchExp do begin
    Result := True;
    Expect := AContext.Expectation;
    ehf := AContext.Expectation.ExpErrorHandlingFlags[Compiler.SymbolType];

    PrePrint := AContext.WatchRes.ValueKind = rdkPrePrinted;

    if (not (ehIgnKindArrayType in ehf)) and
       (not (PrePrint and AContext.IsNested))
    then begin
      TestTrue('ValueKind is array', AContext.WatchRes.ValueKind = rdkArray, AContext, AnIgnoreRsn);
      case AContext.Expectation.ExpResultKind of
        rkStatArray: TestTrue('Is a static array', AContext.WatchRes.ArrayType = datStatArray, AContext, AnIgnoreRsn);
        rkDynArray:  TestTrue('Is a dynamic array', AContext.WatchRes.ArrayType = datDynArray, AContext, AnIgnoreRsn);
      end;
    end;

    v := FWatchResultPrinter.PrintWatchValue(AContext.WatchRes, TestWatchDisplayFormat, '');
debugln([' expect ',Expect.ExpFullArrayLen,'  got "',v,'"' ]);

    if CompareText(v, 'nil') = 0 then begin
      Result := TestEquals('Length/nil', Expect.ExpFullArrayLen, 0, AContext, AnIgnoreRsn);
      exit;
    end;

    if (v='') or (v[1] <> '(') or (v[length(v)] <> ')') then begin
      Result := TestTrue('elements are in (...)', False, AContext, AnIgnoreRsn);
      exit;
    end;
    delete(v, 1, 1);
    delete(v, length(v), 1);

    parsed := ParseCommaList(v, e, Expect.ExpFullArrayLen);
    TestTrue('FieldParser len', e <= Length(parsed), AContext, AnIgnoreRsn);

    if Expect.ExpFullArrayLen >= 0 then
      Result := TestEquals('Length', Expect.ExpFullArrayLen, e, AContext, AnIgnoreRsn);

    e := min(e, Length(parsed));
    n := FTest.TestBaseName;
    SubContext := AContext;
    for i := 0 to min(e, length(Expect.ExpSubResults)) - 1 do begin
      FTest.TestBaseName := n + ' Idx='+IntToStr(i)+' ';
      AContext.WatchRes.SetSelectedIndex(i);

      if PrePrint then
        SubContext.WatchRes := TWatchResultDataPrePrinted.Create(parsed[i])
      else
        SubContext.WatchRes := AContext.WatchRes.SelectedEntry;
      SubContext.Expectation := Expect.ExpSubResults[i];

      // if "not preprint" do treat this as non-nested
      Result := CheckData(SubContext, AnIgnoreRsn, PrePrint);

      if PrePrint then
        FreeAndNil(SubContext.WatchRes);
    end;
    FTest.TestBaseName := n;
  end;

end;

function TWatchExpectationList.CheckStructureFields(const AnIgnoreRsn: String;
  var AContext: TWatchExpTestCurrentData): Boolean;
var
  Expect: TWatchExpectationResult;
  ehf: TWatchExpErrorHandlingFlags;
  i, j, a: Integer;
  SubContext: TWatchExpTestCurrentData;
  sr: TWatchExpectationResult;
  n, v: String;
  lastidx: integer;
begin
  Result := True;
  with AContext.WatchExp do begin
    Expect := AContext.Expectation;
    ehf := Expect.ExpErrorHandlingFlags[Compiler.SymbolType];

    v := Trim(FWatchResultPrinter.PrintWatchValue(AContext.WatchRes, TestWatchDisplayFormat, ''));
    delete(v, 1, pos('(', v));
    delete(v, length(v), 1);

    n := FTest.TestBaseName;
    SubContext := AContext;
    lastidx := -1;
    for i := 0 to length(Expect.ExpSubResults) - 1 do begin
      sr := Expect.ExpSubResults[i];
      if not TestTrue('field name ' + IntToStr(i), sr.ExpFieldName<>'', AContext, AnIgnoreRsn) then
        Continue;

      if AContext.WatchTpInf <> nil then begin
        a := AContext.WatchTpInf.Fields.Count -1;
        while (a >= 0)
        and (CompareText(AContext.WatchTpInf.Fields[a].Name, sr.ExpFieldName) <> 0) do
          dec(a);
        TestTrue('typeinfo has field '+sr.ExpFieldName, a >= 0, AContext, AnIgnoreRsn);
      end;

      if EvalCallResDBGType <> nil then begin
        a := EvalCallResDBGType.Fields.Count -1;
        while (a >= 0)
        and (CompareText(EvalCallResDBGType.Fields[a].Name, sr.ExpFieldName) <> 0) do
          dec(a);
        TestTrue('EvalCallResDBGType has field '+sr.ExpFieldName, a >= 0, AContext, AnIgnoreRsn);
      end;


      v := LowerCase(sr.ExpFieldName);
      j := AContext.WatchRes.FieldCount-1;
      while (j >= 0) and (LowerCase(AContext.WatchRes.Fields[j].FieldName) <> v) do
        dec(j);

      TestTrue('found field '+v, j >= 0, AContext, AnIgnoreRsn);
(*
      if not(ehNoFieldOrder in ehf) then begin
        if ehMissingFields in ehf then begin
          TestTrue('field in order ' + IntToStr(lastidx) + ' ' + IntToStr(j), j > lastidx, AContext, AnIgnoreRsn);
          lastidx := j;
        end
        else begin
          inc(lastidx);
          TestTrue('field in order ' + IntToStr(lastidx) + ' ' + IntToStr(j), j = lastidx, AContext, AnIgnoreRsn);
        end;
      end;
*)

      if j >= 0 then begin
        SubContext.WatchRes := TWatchResultDataPrePrinted.Create(
          FWatchResultPrinter.PrintWatchValue(AContext.WatchRes.Fields[j].Field, TestWatchDisplayFormat, '')
        );
        FTest.TestBaseName := n + ' Idx=' + IntToStr(i)+' ';
        SubContext.Expectation := sr;
        Result := CheckData(SubContext, AnIgnoreRsn);

        FreeAndNil(SubContext.WatchRes);
      end;

    end;
    FTest.TestBaseName := n;
  end;
end;

function TWatchExpectationList.CheckResultRecord(
  AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean;
var
  v: String;
begin
  with AContext.WatchExp do begin
    Result := True;

    v := Trim(FWatchResultPrinter.PrintWatchValue(AContext.WatchRes, TestWatchDisplayFormat, ''));
    TestTrue('TODO / some text', v <> '', AContext, AnIgnoreRsn);

    TestTrue('rdkStruct', AContext.WatchRes.ValueKind = rdkStruct, AContext, AnIgnoreRsn);
    TestTrue('dstRecord', AContext.WatchRes.StructType = dstRecord, AContext, AnIgnoreRsn);

    Result := CheckStructureFields(AnIgnoreRsn, AContext);
  end;
end;

function TWatchExpectationList.CheckResultClass(
  AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean;
var
  v: String;
begin
  with AContext.WatchExp do begin
    Result := True;

    v := Trim(FWatchResultPrinter.PrintWatchValue(AContext.WatchRes, TestWatchDisplayFormat, ''));
    TestTrue('TODO / some text', v <> '', AContext, AnIgnoreRsn);

    TestTrue('rdkStruct', AContext.WatchRes.ValueKind = rdkStruct, AContext, AnIgnoreRsn);
    TestTrue('dstClass', AContext.WatchRes.StructType = dstClass, AContext, AnIgnoreRsn);

    Result := CheckStructureFields(AnIgnoreRsn, AContext);
  end;
end;

function TWatchExpectationList.CheckResultObject(
  AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean;
var
  v: String;
begin
  with AContext.WatchExp do begin
    Result := True;

    v := Trim(FWatchResultPrinter.PrintWatchValue(AContext.WatchRes, TestWatchDisplayFormat, ''));
    TestTrue('TODO / some text', v <> '', AContext, AnIgnoreRsn);

    TestTrue('rdkStruct', AContext.WatchRes.ValueKind = rdkStruct, AContext, AnIgnoreRsn);
    if AContext.WatchRes.StructType in [dstClass, dstRecord] then
      TestTrue('dstObject', AContext.WatchRes.StructType = dstObject, AContext, 'Ignored')
    else
      TestTrue('dstObject', AContext.WatchRes.StructType = dstObject, AContext, AnIgnoreRsn);

    Result := CheckStructureFields(AnIgnoreRsn, AContext);
  end;
end;

function TWatchExpectationList.CheckResultInterface(
  AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean;
var
  v: String;
begin
  with AContext.WatchExp do begin
    Result := True;

    v := Trim(FWatchResultPrinter.PrintWatchValue(AContext.WatchRes, TestWatchDisplayFormat, ''));
    TestTrue('TODO / some text', v <> '', AContext, AnIgnoreRsn);

    TestTrue('rdkStruct', AContext.WatchRes.ValueKind = rdkStruct, AContext, AnIgnoreRsn);
    TestTrue('dstInterface', AContext.WatchRes.StructType = dstInterface, AContext, AnIgnoreRsn);

    Result := CheckStructureFields(AnIgnoreRsn, AContext);
  end;
end;

constructor TWatchExpectationList.Create(ATest: TDBGTestCase);
begin
  FWatchResultPrinter := TWatchResultPrinter.Create;
  FWatchResultPrinter.FormatFlags := [];
  FTest := ATest;
  FTypeNameAliases := TStringList.Create;
  inherited Create;
end;

destructor TWatchExpectationList.Destroy;
begin
  Clear;
  FTypeNameAliases.Free;
  FWatchResultPrinter.Free;
  inherited Destroy;
end;

function TWatchExpectationList.AddWithoutExpect(ATestName: String;
  AnExpr: string; AStackFrame: Integer; AMinFpc: Integer; AMinDbg: Integer
  ): PWatchExpectation;
var
  i: Integer;
  w: TTestWatch;
begin
  i := Length(FList);
  SetLength(FList, i+1);

  w := TTestWatch.Create(Debugger.Watches.Watches);
  w.Expression := AnExpr;
  w.Enabled := True;

  FList[i].TstTestName := ATestName;
  FList[i].TstWatch := w;
  FList[i].TstStackFrame := AStackFrame;
  FList[i].TstMinFpc   := AMinFpc;
  FList[i].TstMinDbg   := AMinDbg;

  Result := @FList[i];
end;

function TWatchExpectationList.Add(ATestName: String; AnExpr: string;
  AnExpect: TWatchExpectationResult; AStackFrame: Integer; AMinFpc: Integer;
  AMinDbg: Integer): PWatchExpectation;
var
  i: Integer;
  w: TTestWatch;
begin
  i := Length(FList);
  SetLength(FList, i+1);

  w := TTestWatch.Create(Debugger.Watches.Watches);
  w.Expression := AnExpr;
  w.Enabled := True;

  FList[i].TstTestName := ATestName;
  FList[i].TstWatch := w;
  FList[i].TstExpected := AnExpect;
  FList[i].TstStackFrame := AStackFrame;
  FList[i].TstMinFpc   := AMinFpc;
  FList[i].TstMinDbg   := AMinDbg;

  Result := @FList[i];
end;

function TWatchExpectationList.Add(AnExpr: string;
  AnExpect: TWatchExpectationResult; AStackFrame: Integer; AMinFpc: Integer;
  AMinDbg: Integer): PWatchExpectation;
begin
  Result := Add('', AnExpr, AnExpect, AStackFrame, AMinFpc, AMinDbg);
end;

procedure TWatchExpectationList.AddIndexFromPrevious(
  IndexNames: array of string; ValueIndex: array of integer;
  AnPreviousOffset: Integer);
var
  prev: TWatchExpectation;
  i: Integer;
  t: PWatchExpectation;
  st: TSymbolType;
begin
  prev := FList[Count-1-AnPreviousOffset];
  for i := 0 to high(IndexNames) do begin
    t := Add(Prev.TstTestName + ' ['+IndexNames[i]+']',
      prev.TstWatch.Expression+ '['+IndexNames[i]+']',
      prev.TstExpected.ExpSubResults[ValueIndex[i]],
      prev.TstStackFrame, prev.TstMinFpc, prev.TstMinDbg
    );
    t^.TstExpected.MakeCopy;
    // copy flags from expectation for whole array
    for st := low(TSymbolTypes) to high(TSymbolTypes) do
      t^.AddFlag(prev.TstExpected.ExpErrorHandlingFlags[st], [st]);
  end;
end;

procedure TWatchExpectationList.AddMemberFromPrevious(AnPreviousOffset: Integer
  );
var
  prev: TWatchExpectation;
  i: Integer;
  t: PWatchExpectation;
  st: TSymbolType;
  psub: TWatchExpectationResult;
begin
  prev := FList[Count-1-AnPreviousOffset];

  for i := 0 to Length(prev.TstExpected.ExpSubResults) -1 do begin
    psub := prev.TstExpected.ExpSubResults[i];
    t := Add(Prev.TstTestName + '.'+psub.ExpFieldName,
      prev.TstWatch.Expression+ '.'+psub.ExpFieldName,
      psub,
      prev.TstStackFrame, prev.TstMinFpc, prev.TstMinDbg
    );

    t^.TstExpected.MakeCopy;
    t^.TstExpected.ExpFieldName := '';
    // copy flags from expectation for whole array
    for st := low(TSymbolTypes) to high(TSymbolTypes) do
      t^.AddFlag(prev.TstExpected.ExpErrorHandlingFlags[st], [st]);
  end;
end;

procedure TWatchExpectationList.AddTypeNameAlias(ATypeName, AnAliases: String);
begin
  ATypeName := UpperCase(ATypeName);
  if FTypeNameAliases.Values[ATypeName] <> '' then
    AnAliases := FTypeNameAliases.Values[ATypeName] + '|' + FTypeNameAliases.Values[ATypeName];
  FTypeNameAliases.Values[ATypeName] := AnAliases;
end;

procedure TWatchExpectationList.Clear;
var
  i: Integer;
begin
  for i := 0 to Length(FList)-1 do begin
    FList[i].TstWatch.Free;
    FList[i].EvalCallResDBGType.Free;
  end;
  FList := nil;
end;

function TWatchExpectationList.Count: Integer;
begin
  Result := Length(FList);
end;

procedure TWatchExpectationList.EvaluateWatches;
var
  i, t: Integer;
  l: TDBGLocationRec;
begin
  t := LazDebugger.Threads.CurrentThreads.CurrentThreadId;
  l := Debugger.CurLocation;

  // Fire up all watches / so the debugger does not need to wait for the testcase
  for i := 0 to Length(FList)-1 do
    EvaluateWatch(@FList[i], t, l, False);

  // Wait for the results
  for i := 0 to Length(FList)-1 do begin
    EvaluateWatch(@FList[i], t, l);
    if (i mod 16) = 0 then TestLogger.DbgOut('.');
  end;

  // Eval by ReqCommand
  for i := 0 to Length(FList)-1 do begin
    EvaluateExprCmd(@FList[i], t, l);
    if (i mod 16) = 0 then TestLogger.DbgOut('.');
  end;

  TestLogger.DebugLn('');
end;

procedure TWatchExpectationList.CheckResults;
var
  i: Integer;
begin
  for i := 0 to Length(FList)-1 do
    CheckResult(FList[i]);
end;

procedure TWatchExpectationList.EvalAndCheck;
begin
  EvaluateWatches;
  CheckResults;
end;

initialization
  TestWatchDisplayFormat := DefaultWatchDisplayFormat;
  TestWatchDisplayFormat.ArrayLen.UseInherited  := False;
  TestWatchDisplayFormat.ArrayLen.ShowLenPrefix := False;

end.

