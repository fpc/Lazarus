unit TTestWatchUtilities;

{$mode objfpc}{$H+}
{$modeswitch AdvancedRecords}
{$modeswitch TypeHelpers}

interface

uses
  Classes, SysUtils, math, DbgIntfBaseTypes, DbgIntfDebuggerBase,
  FpPascalBuilder, LazLoggerBase, RegExpr, TestDbgTestSuites,
  TTestDebuggerClasses, TTestDbgExecuteables, TestDbgConfig, TestOutputLogger;

type
  TWatchExpectationResultKind = (
    rkMatch, rkInteger, rkCardinal, rkFloat, rkEnum, rkSet,
    rkChar, rkAnsiString, rkShortString, rkWideString, rkPointer, rkPointerAddr,
    rkClass, rkObject, rkRecord, rkField,
    rkStatArray, rkDynArray
  );

  TWatchExpErrorHandlingFlag =
    (ehIgnAll,             // ignore error for all
     ehTestSkip,       // Do not run test

     ehIgnData,           // Ignore the data part
     ehIgnKind,           // Ignore skSimple, ....
     ehIgnKindPtr,        // Ignore skSimple, ONLY if got kind=skPointer
     ehIgnTypeName,       // Ignore the typename
     ehMatchTypeName,     // The typename is a regex

     ehCharFromIndex,     // Debugger is allowed Pchar: 'x' String 'y'

     ehExpectNotFound,
     ehExpectError,       // watch is invalid (less specific, than not found / maybe invalid expression ?)

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

    function AddFlag(AFlag: TWatchExpErrorHandlingFlag; ASymTypes: TSymbolTypes = []): TWatchExpectationResult;
    function AddFlag(AFlags: TWatchExpErrorHandlingFlags; ASymTypes: TSymbolTypes = []): TWatchExpectationResult;

    function Skip(ASymTypes: TSymbolTypes = []): TWatchExpectationResult;
    function SkipIf(ACond: Boolean; ASymTypes: TSymbolTypes = []): TWatchExpectationResult;

    function IgnAll(ASymTypes: TSymbolTypes = []): TWatchExpectationResult;
    function IgnData(ASymTypes: TSymbolTypes = []): TWatchExpectationResult;
    function IgnKind(ASymTypes: TSymbolTypes = []): TWatchExpectationResult;
    function IgnKindPtr(ASymTypes: TSymbolTypes = []): TWatchExpectationResult;
    function IgnTypeName(ASymTypes: TSymbolTypes = []): TWatchExpectationResult;
    function MatchTypeName(ASymTypes: TSymbolTypes = []): TWatchExpectationResult;

    function CharFromIndex(ASymTypes: TSymbolTypes = []): TWatchExpectationResult;

    function ExpectNotFound(ASymTypes: TSymbolTypes = []): TWatchExpectationResult;
    function ExpectError(ASymTypes: TSymbolTypes = []): TWatchExpectationResult;

    function NotImplemented(ASymTypes: TSymbolTypes = []): TWatchExpectationResult;
    function NotImplementedData(ASymTypes: TSymbolTypes = []): TWatchExpectationResult;

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

    //TstDspFormat: TWatchDisplayFormat;
    //TstRepeatCount: Integer;
    //TstEvaluateFlags: TDBGEvaluateFlags;
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
    function AddFlag(AFlag: TWatchExpErrorHandlingFlag; ASymTypes: TSymbolTypes = []): PWatchExpectation;
    function AddFlag(AFlags: TWatchExpErrorHandlingFlags; ASymTypes: TSymbolTypes = []): PWatchExpectation;

    function Skip(ASymTypes: TSymbolTypes = []): PWatchExpectation;
    function SkipIf(ACond: Boolean; ASymTypes: TSymbolTypes = []): PWatchExpectation;

    function IgnAll(ASymTypes: TSymbolTypes = []): PWatchExpectation;
    function IgnData(ASymTypes: TSymbolTypes = []): PWatchExpectation;
    function IgnKind(ASymTypes: TSymbolTypes = []): PWatchExpectation;
    function IgnKindPtr(ASymTypes: TSymbolTypes = []): PWatchExpectation;
    function IgnTypeName(ASymTypes: TSymbolTypes = []): PWatchExpectation;
    function MatchTypeName(ASymTypes: TSymbolTypes = []): PWatchExpectation;

    function CharFromIndex(ASymTypes: TSymbolTypes = []): PWatchExpectation;

    function ExpectNotFound(ASymTypes: TSymbolTypes = []): PWatchExpectation;
    function ExpectError(ASymTypes: TSymbolTypes = []): PWatchExpectation;

    function NotImplemented(ASymTypes: TSymbolTypes = []): PWatchExpectation;
    function NotImplementedData(ASymTypes: TSymbolTypes = []): PWatchExpectation;
  end;

  //TWatchExpectationResultHelper = type helper for TWatchExpectationResult
  //end;

  TWatchExpectationResultArrayHelper = type helper for TWatchExpectationResultArray
  end;

  TWatchExpTestCurrentData = record
    WatchExp: TWatchExpectation;
    WatchVal: TWatchValue;
    Expectation: TWatchExpectationResult;
    HasTypeInfo: Boolean;
  end;

  { TWatchExpectationList }
  TDbgSymbolKinds = set of TDbgSymbolKind;

  TWatchExpectationList = class
  private
    FAcceptSkSimple: TDbgSymbolKinds;
    FTest: TDBGTestCase;
    FList: array of TWatchExpectation;
    FTypeNameAliases: TStringList;
    function GetCompiler: TTestDbgCompiler;
    function GetDebugger: TTestDbgDebugger;
    function GetLazDebugger: TDebuggerIntf;
    function ParseCommaList(AVal: String; out AFoundCount: Integer;
      AMaxLen: Integer = -1): TStringArray;
  protected
    function EvaluateWatch(AWatchExp: TWatchExpectation; AThreadId: Integer): Boolean; virtual;
    procedure WaitWhileEval; virtual;

    function TestMatches(Name: string; Expected, Got: string; AContext: TWatchExpTestCurrentData; AIgnoreReason: String): Boolean;
    function TestMatches(Name: string; Expected, Got: string; ACaseSense: Boolean; AContext: TWatchExpTestCurrentData; AIgnoreReason: String): Boolean;
    function TestEquals(Name: string; Expected, Got: string; AContext: TWatchExpTestCurrentData; AIgnoreReason: String): Boolean;
    function TestEquals(Name: string; Expected, Got: string; ACaseSense: Boolean; AContext: TWatchExpTestCurrentData; AIgnoreReason: String): Boolean;
    function TestEquals(Name: string; Expected, Got: integer; AContext: TWatchExpTestCurrentData; AIgnoreReason: String): Boolean;
    function TestTrue(Name: string; Got: Boolean; AContext: TWatchExpTestCurrentData; AIgnoreReason: String): Boolean;
    function TestFalse(Name: string; Got: Boolean; AContext: TWatchExpTestCurrentData; AIgnoreReason: String): Boolean;

    function CheckResult(AnWatchExp: TWatchExpectation): Boolean;
    function CheckData(AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean; virtual;
    function VerifyDebuggerState: Boolean; virtual;
    function VerifySymType(AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean; virtual;
    function VerifyTypeName(AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean; virtual;

    function CheckResultMatch(AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean; virtual;
    function CheckResultNum(AContext: TWatchExpTestCurrentData; IsCardinal: Boolean; AnIgnoreRsn: String): Boolean; virtual;
    function CheckResultFloat(AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean; virtual;
    function CheckResultEnum(AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean; virtual;
    function CheckResultSet(AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean; virtual;
    function CheckResultChar(AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean; virtual;
    function CheckResultAnsiStr(AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean; virtual;
    function CheckResultShortStr(AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean; virtual;
    function CheckResultPointer(AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean; virtual;
    function CheckResultPointerAddr(AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean; virtual;
    function CheckResultArray(AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean; virtual;

    property Compiler: TTestDbgCompiler read GetCompiler;
    property Debugger: TTestDbgDebugger read GetDebugger;
    property LazDebugger: TDebuggerIntf read GetLazDebugger;
  public
    constructor Create(ATest: TDBGTestCase);
    destructor Destroy; override;

    function Add(ATestName: String;
      AnExpr:  string; // AEvaluateFlags: TDBGEvaluateFlags; // AFmt: TWatchDisplayFormat;
      AnExpect: TWatchExpectationResult;
      AStackFrame: Integer = 0; AMinFpc: Integer = 0; AMinDbg: Integer = 0
      // ASpecialFlags: ... // Ignore this or that // maybe per result
    ): PWatchExpectation;

    function Add(
      AnExpr:  string; // AEvaluateFlags: TDBGEvaluateFlags; // AFmt: TWatchDisplayFormat;
      AnExpect: TWatchExpectationResult;
      AStackFrame: Integer = 0; AMinFpc: Integer = 0; AMinDbg: Integer = 0
    ): PWatchExpectation;

    procedure Clear;
    function EvaluateWatches: Boolean;
    procedure CheckResults;

    procedure AddTypeNameAlias(ATypeName, AnAliases: String);
    property AcceptSkSimple: TDbgSymbolKinds read FAcceptSkSimple write FAcceptSkSimple ; // skSimple for skInteger,skChar,...
  end;


function weMatch(AExpVal: String; ASymKind: TDBGSymbolKind; ATypeName: String=''): TWatchExpectationResult;

function weInteger(AExpVal: Int64; ATypeName: String=#1; ASize: Integer = 4): TWatchExpectationResult;
function weCardinal(AExpVal: QWord; ATypeName: String=#1; ASize: Integer = 4): TWatchExpectationResult;
function weSingle(AExpVal: Extended; ATypeName: String=#1): TWatchExpectationResult;
function weDouble(AExpVal: Extended; ATypeName: String=#1): TWatchExpectationResult;
function weFloat(AExpVal: Extended; ATypeName: String=''): TWatchExpectationResult;

function weEnum(AExpVal: string; ATypeName: String=#1): TWatchExpectationResult;
function weSet(const AExpVal: Array of string; ATypeName: String=#1): TWatchExpectationResult;

function weChar(AExpVal: char; ATypeName: String=#1): TWatchExpectationResult;
function weWideChar(AExpVal: char; ATypeName: String=#1): TWatchExpectationResult;
function weAnsiStr(AExpVal: string; ATypeName: String=#1): TWatchExpectationResult;
function weShortStr(AExpVal: string; ATypeName: String=#1): TWatchExpectationResult;
function weWideStr(AExpVal: string; ATypeName: String=#1): TWatchExpectationResult;

function wePointer(AExpVal: TWatchExpectationResult; ATypeName: String=''): TWatchExpectationResult;
function wePointerAddr(AExpVal: Pointer; ATypeName: String=''): TWatchExpectationResult;

function weStatArray(const AExpVal: Array of TWatchExpectationResult; ATypeName: String=''): TWatchExpectationResult; overload;
function weStatArray(const AExpVal: Array of TWatchExpectationResult; AExpFullLen: Integer; ATypeName: String=''): TWatchExpectationResult; overload;
function weDynArray(const AExpVal: Array of TWatchExpectationResult; ATypeName: String=''): TWatchExpectationResult; overload;
function weDynArray(const AExpVal: Array of TWatchExpectationResult; AExpFullLen: Integer; ATypeName: String=''): TWatchExpectationResult; overload;

// common arrays: weTttArray(weChar([...]))
function weChar(const AExpVal: array of char; ATypeName: String=#1): TWatchExpectationResultArray;
function weInteger(const AExpVal: array of Int64; ATypeName: String=#1; ASize: Integer = 4): TWatchExpectationResultArray;
function weAnsiStr(const AExpVal: array of string; ATypeName: String=#1): TWatchExpectationResultArray;
function weShortStr(const AExpVal: array of string; ATypeName: String=#1): TWatchExpectationResultArray;



function weClass(AExpClass: String; AExpFields: array of TWatchExpectationResult; ATypeName: String=#1): TWatchExpectationResult;
function weField(AExpName: String; AExpVal: TWatchExpectationResult; ATypeName: String=#1): TWatchExpectationResult;

operator := (a:string): TWatchExpectationResult;
operator := (a:integer): TWatchExpectationResult;
operator := (a:pointer): TWatchExpectationResult;



implementation

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

function weMatch(AExpVal: String; ASymKind: TDBGSymbolKind; ATypeName: String
  ): TWatchExpectationResult;
begin
  Result := Default(TWatchExpectationResult);
  Result.ExpResultKind := rkMatch;
  Result.ExpSymKind := ASymKind;
  Result.ExpTextData := AExpVal;
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
  Result.ExpSymKind := skAnsiString; // TODO
  Result.ExpTypeName := ATypeName;
  Result.ExpTextData := AExpVal;
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

function weInteger(const AExpVal: array of Int64; ATypeName: String;
  ASize: Integer): TWatchExpectationResultArray;
var
  i: Integer;
begin
  SetLength(Result, Length(AExpVal));
  for i := 0 to Length(AExpVal) - 1 do
    Result[i] := weInteger(AExpVal[i], ATypeName, ASize);
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

function weClass(AExpClass: String;
  AExpFields: array of TWatchExpectationResult; ATypeName: String
  ): TWatchExpectationResult;
begin
  Result := Default(TWatchExpectationResult);

end;

function weField(AExpName: String; AExpVal: TWatchExpectationResult;
  ATypeName: String): TWatchExpectationResult;
begin
  Result := Default(TWatchExpectationResult);

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

{ TWatchExpectationHelper }

function TWatchExpectationHelper.AddFlag(AFlag: TWatchExpErrorHandlingFlag;
  ASymTypes: TSymbolTypes): PWatchExpectation;
begin
  Result := Self^.AddFlag(AFlag, ASymTypes);
end;

function TWatchExpectationHelper.AddFlag(AFlags: TWatchExpErrorHandlingFlags;
  ASymTypes: TSymbolTypes): PWatchExpectation;
begin
  Result := Self^.AddFlag(AFlags, ASymTypes);
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

function TWatchExpectationHelper.IgnAll(ASymTypes: TSymbolTypes
  ): PWatchExpectation;
begin
  Result := Self^.AddFlag(ehIgnAll, ASymTypes);
end;

function TWatchExpectationHelper.IgnData(ASymTypes: TSymbolTypes
  ): PWatchExpectation;
begin
  Result := Self^.AddFlag(ehIgnData, ASymTypes);
end;

function TWatchExpectationHelper.IgnKind(ASymTypes: TSymbolTypes
  ): PWatchExpectation;
begin
  Result := Self^.AddFlag(ehIgnKind, ASymTypes);
end;

function TWatchExpectationHelper.IgnKindPtr(ASymTypes: TSymbolTypes
  ): PWatchExpectation;
begin
  Result := Self^.AddFlag(ehIgnKindPtr, ASymTypes);
end;

function TWatchExpectationHelper.IgnTypeName(ASymTypes: TSymbolTypes
  ): PWatchExpectation;
begin
  Result := Self^.AddFlag(ehIgnTypeName, ASymTypes);
end;

function TWatchExpectationHelper.MatchTypeName(ASymTypes: TSymbolTypes
  ): PWatchExpectation;
begin
  Result := Self^.AddFlag(ehMatchTypeName, ASymTypes);
end;

function TWatchExpectationHelper.CharFromIndex(ASymTypes: TSymbolTypes
  ): PWatchExpectation;
begin
  Result := Self^.AddFlag(ehCharFromIndex, ASymTypes);
end;

function TWatchExpectationHelper.ExpectNotFound(ASymTypes: TSymbolTypes
  ): PWatchExpectation;
begin
  Result := Self^.AddFlag(ehExpectNotFound, ASymTypes);
end;

function TWatchExpectationHelper.ExpectError(ASymTypes: TSymbolTypes
  ): PWatchExpectation;
begin
  Result := Self^.AddFlag(ehExpectError, ASymTypes);
end;

function TWatchExpectationHelper.NotImplemented(ASymTypes: TSymbolTypes
  ): PWatchExpectation;
begin
  Result := Self^.AddFlag(ehNotImplemented, ASymTypes);
end;

function TWatchExpectationHelper.NotImplementedData(ASymTypes: TSymbolTypes
  ): PWatchExpectation;
begin
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

function TWatchExpectationList.ParseCommaList(AVal: String; out AFoundCount: Integer; AMaxLen: Integer = -1): TStringArray;
var
  i, BracketLvl: Integer;
  InQuote: Boolean;
begin
  if AMaxLen < 0 then
    SetLength(Result, Length(AVal) div 2)
  else
    SetLength(Result, AMaxLen);

  i := 1;
  AFoundCount := 0;
  BracketLvl := 0;
  InQuote := false;
  while length(AVal) > 0 do begin
    while (i <= length(AVal)) and (
        (AVal[i] <> ',') or (BracketLvl > 0) or InQuote
      )
    do begin
      case AVal[i] of
        '(': if not InQuote then inc(BracketLvl);
        ')': if not InQuote then dec(BracketLvl);
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
    delete(AVal, 1, i);
    i := 1;
    if (length(AVal) > 0) and (AVal[1] = ' ') then
      delete(AVal, 1, 1);
  end;
end;

function TWatchExpectationList.EvaluateWatch(AWatchExp: TWatchExpectation;
  AThreadId: Integer): Boolean;
var
  i: Integer;
begin
  FTest.LogText('###### ' + AWatchExp.TstTestName + ' // ' + AWatchExp.TstWatch.Expression +
    ' (AT '+ LazDebugger.GetLocation.SrcFile + ':' + IntToStr(LazDebugger.GetLocation.SrcLine) +')' +
    '###### '+LineEnding);
  AWatchExp.TstWatch.Values[AThreadId, AWatchExp.TstStackFrame].Value;

  for i := 1 to 5 do begin
    WaitWhileEval;
    Result := AWatchExp.TstWatch.Values[AThreadId, AWatchExp.TstStackFrame].Validity <> ddsRequested;
    if Result then break;
  end;
  FTest.LogText('<<<<< ' + dbgs(AWatchExp.TstWatch.Values[AThreadId, AWatchExp.TstStackFrame].Validity) + ': ' +
    AWatchExp.TstWatch.Values[AThreadId, AWatchExp.TstStackFrame].Value );
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

function TWatchExpectationList.TestEquals(Name: string; Expected, Got: integer;
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
      FTest.TestBaseName := FTest.TestBaseName + ' ' + TstTestName + ' ('+TstWatch.Expression+' AT '+ LazDebugger.GetLocation.SrcFile + ':' + IntToStr(LazDebugger.GetLocation.SrcLine) +')';
      if TstStackFrame > 0 then
        FTest.TestBaseName := FTest.TestBaseName + ' (Stack: ' + IntToStr(TstStackFrame) + ')';
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
      Context.WatchVal := WatchVal;

      if not VerifyDebuggerState then
        exit;

      if ehExpectError in ehf then begin
//TODO
        Result := TestTrue('TstWatch.value is NOT valid', WatchVal.Validity in [ddsError, ddsInvalid], Context, AnIgnoreRsn);
        exit;
      end;
      if ehExpectNotFound in ehf then begin
        Result := TestMatches('TstWatch.value NOT found', 'not found', WatchVal.Value, Context, AnIgnoreRsn);
        Result := TestTrue('TstWatch.value NOT found', WatchVal.Validity in [ddsError, ddsInvalid], Context, AnIgnoreRsn);
        exit;
      end;
      if not TestTrue('TstWatch.value is valid', WatchVal.Validity = ddsValid, Context, AnIgnoreRsn) then
        exit;

      if TestTrue('Has TypeInfo', Context.WatchVal.TypeInfo <> nil, Context, AnIgnoreRsn) then
        Context.HasTypeInfo := True;

      VerifySymType(Context, AnIgnoreRsn);
      VerifyTypeName(Context, AnIgnoreRsn);

      if ehIgnData in ehf then
        AnIgnoreRsn := AnIgnoreRsn + 'Test ignored (Data)';
      if ehNotImplementedData in ehf then
        AnIgnoreRsn := AnIgnoreRsn + 'Not implemented (Data)';

      Result := CheckData(Context, AnIgnoreRsn);
    finally
      FTest.TestBaseName := CurBaseName;
    end;
  end;
end;

function TWatchExpectationList.CheckData(AContext: TWatchExpTestCurrentData;
  AnIgnoreRsn: String): Boolean;
begin
  case AContext.Expectation.ExpResultKind of
    rkMatch:       Result := CheckResultMatch(AContext, AnIgnoreRsn);
    rkInteger:     Result := CheckResultNum(AContext, False, AnIgnoreRsn);
    rkCardinal:    Result := CheckResultNum(AContext, True, AnIgnoreRsn);
    rkFloat:       Result := CheckResultFloat(AContext, AnIgnoreRsn);
    rkEnum:        Result := CheckResultEnum(AContext, AnIgnoreRsn);
    rkSet:         Result := CheckResultSet(AContext, AnIgnoreRsn);
    rkChar:        Result := CheckResultChar(AContext, AnIgnoreRsn);
    rkWideString,
    rkAnsiString:  Result := CheckResultAnsiStr(AContext, AnIgnoreRsn);
    rkShortString: Result := CheckResultShortStr(AContext, AnIgnoreRsn);
    rkPointer:     Result := CheckResultPointer(AContext, AnIgnoreRsn);
    rkPointerAddr: Result := CheckResultPointerAddr(AContext, AnIgnoreRsn);
    rkClass: ;
    rkObject: ;
    rkRecord: ;
    rkField: ;
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

    t := AContext.WatchVal.TypeInfo.Kind;
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

    if (Expect.ExpTypeName = '') or (not AContext.HasTypeInfo) then
      exit;

    ehf := Expect.ExpErrorHandlingFlags[Compiler.SymbolType];
    if ehIgnTypeName in ehf then
      AnIgnoreRsn := AnIgnoreRsn + 'Test ignored';
    if ehNotImplementedType in ehf then
      AnIgnoreRsn := AnIgnoreRsn + 'Not implemented (typename)';

    WtchTpName := AContext.WatchVal.TypeInfo.TypeName;

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
  ehf: TWatchExpErrorHandlingFlags;
begin
  with AContext.WatchExp do begin
    Result := True;
    Expect := AContext.Expectation;

    Result := TestMatches('Data', Expect.ExpTextData, AContext.WatchVal.Value, AContext, AnIgnoreRsn);
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

    if IsCardinal then
      s := IntToStr(Expect.expCardinalValue)
    else
      s := IntToStr(Expect.expIntValue);

    Result := TestEquals('Data', s, AContext.WatchVal.Value, AContext, AnIgnoreRsn);

    //if not TestEquals('DataSize', Expect.ExpIntSize, AContext.WatchVal.TypeInfo.Len, AContext, AnIgnoreRsn) then
    //  Result := False;
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

    Result := TestEquals('Data', FloatToStr(Expect.ExpFloatValue), AContext.WatchVal.Value, EqIgnoreCase, AContext, AnIgnoreRsn);
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

    Result := TestEquals('Data', Expect.ExpTextData, AContext.WatchVal.Value, not(Compiler.SymbolType in stDwarf2), AContext, AnIgnoreRsn);
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

    v := AContext.WatchVal.Value;

    if (v='') or (v[1] <> '[') or (v[length(v)] <> ']') then begin
      Result := TestTrue('elements are in [...]', False, AContext, AnIgnoreRsn);
      exit;
    end;
    delete(v, 1, 1);
    delete(v, length(v), 1);

    parsed := ParseCommaList(v, e, Length(Expect.ExpSetData));

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
  e: String;
begin
  with AContext.WatchExp do begin
    Result := True;
    Expect := AContext.Expectation;

    e := QuoteText(Expect.ExpTextData);

    ehf := Expect.ExpErrorHandlingFlags[Compiler.SymbolType];
    if ehCharFromIndex in ehf then begin
      if AContext.WatchVal.Value <> e then begin
//AnIgnoreRsn := AnIgnoreRsn + 'char from index not implemented';
        Result := TestMatches('Data (pchar/string)', '([Pp][Cc]har|[Ss]tring):? *'+e,
          AContext.WatchVal.Value, EqMatchCase, AContext, AnIgnoreRsn);
        exit;
      end
      else
      if AnIgnoreRsn = '' then
        TestTrue('Expect from Index, yet got only one value', False, AContext, 'Success, better than expected');
    end;

    Result := TestEquals('Data', e, AContext.WatchVal.Value, AContext, AnIgnoreRsn);
  end;
end;

function TWatchExpectationList.CheckResultAnsiStr(
  AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean;
var
  Expect: TWatchExpectationResult;
  v, e: String;
begin
  with AContext.WatchExp do begin
    Result := True;
    Expect := AContext.Expectation;

    v := AContext.WatchVal.Value;

    // in dwarf 2 ansistring are pchar
    // widestring are always pwidechar
    if (Compiler.SymbolType in stDwarf2) or (AContext.Expectation.ExpResultKind = rkWideString) then begin
      if (Expect.ExpTypeName <> '') then begin
        if (Expect.ExpTextData = '') and
           FTest.Matches('^'+Expect.ExpTypeName+'\(nil\)', v)
        then
          v := ''''''
        else
        if FTest.Matches('^'+Expect.ExpTypeName+'\(\$[0-9a-fA-F]+\) ', v) then
          delete(v, 1, pos(') ', v)+1)
        else
        if FTest.Matches('^\$[0-9a-fA-F]+ ', v) then
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

    e := QuoteText(Expect.ExpTextData);
    Result := TestEquals('Data', e, v, AContext, AnIgnoreRsn);
  end;
end;

function TWatchExpectationList.CheckResultShortStr(
  AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean;
var
  Expect: TWatchExpectationResult;
  e: String;
begin
  with AContext.WatchExp do begin
    Result := True;
    Expect := AContext.Expectation;
    e := QuoteText(Expect.ExpTextData);

    Result := TestEquals('Data', e, AContext.WatchVal.Value, AContext, AnIgnoreRsn);
  end;
end;

function TWatchExpectationList.CheckResultPointer(
  AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean;
var
  Expect: TWatchExpectationResult;
  g, e, n: String;
  i, i2: SizeInt;
  SubContext: TWatchExpTestCurrentData;
begin
  with AContext.WatchExp do begin
    Result := True;
    Expect := AContext.Expectation;
DebugLn(['test pointer got: ', AContext.WatchVal.Value, ' // want: ',Expect.ExpSubResults[0].ExpTextData]);

    e := '(\$[0-9a-fA-F]*|nil)';
    if Expect.ExpTypeName <> '' then
      e := Expect.ExpTypeName+'\('+e+'\)';
    e := '^'+e;

    Result := TestMatches('Data', e, AContext.WatchVal.Value, AContext, AnIgnoreRsn);

    g := AContext.WatchVal.Value;
    i := pos(' ', g);
    if i > 1 then
      delete(g, 1, i)
    else
    if not (Expect.ExpSubResults[0].ExpResultKind in [rkChar, rkAnsiString, rkWideString, rkShortString]) then begin
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
    SubContext.WatchVal.Value := g;
    FTest.TestBaseName := n + ' / deref value';

    //SubContext.WatchExp.TstExpected := Expect.ExpSubResults[0];
    SubContext.Expectation := Expect.ExpSubResults[0];
    Result := CheckData(SubContext, AnIgnoreRsn);

    FTest.TestBaseName := n;
    //AContext.WatchVal.Value := v;

  end;
end;

function TWatchExpectationList.CheckResultPointerAddr(
  AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean;
var
  Expect: TWatchExpectationResult;
  e: String;
begin
  with AContext.WatchExp do begin
    Result := True;
    Expect := AContext.Expectation;

    e := '\$0*'+IntToHex(PtrUInt(Expect.ExpPointerValue), 8);
    if Expect.ExpTypeName <> '' then
      e := Expect.ExpTypeName+'\('+e+'\)';
    e := '^'+e;

    Result := TestMatches('Data', e, AContext.WatchVal.Value, AContext, AnIgnoreRsn);
  end;
end;

function TWatchExpectationList.CheckResultArray(
  AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean;
var
  Expect: TWatchExpectationResult;
  SubContext: TWatchExpTestCurrentData;
  v, n: String;
  parsed: array of String;
  i, e, b: Integer;
  q: Boolean;
begin
  with AContext.WatchExp do begin
    Result := True;
    Expect := AContext.Expectation;

    v := AContext.WatchVal.Value;
debugln([' expect ',Expect.ExpFullArrayLen,'  got "',v,'"' ]);

    if (LowerCase(v) = 'nil') then begin
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

    if Expect.ExpFullArrayLen >= 0 then
      Result := TestEquals('Length', Expect.ExpFullArrayLen, e, AContext, AnIgnoreRsn);

    e := min(e, Length(parsed));
    n := FTest.TestBaseName;
    SubContext := AContext;
    for i := 0 to min(e, length(Expect.ExpSubResults)) - 1 do begin
      SubContext.WatchVal.Value := parsed[i];
      FTest.TestBaseName := n + ' Idx='+IntToStr(i);

      //SubContext.WatchExp.TstExpected := Expect.ExpSubResults[i];
      SubContext.Expectation := Expect.ExpSubResults[i];
      Result := CheckData(SubContext, AnIgnoreRsn);
    end;

    FTest.TestBaseName := n;
    AContext.WatchVal.Value := v;
  end;

end;

constructor TWatchExpectationList.Create(ATest: TDBGTestCase);
begin
  FTest := ATest;
  FTypeNameAliases := TStringList.Create;
  inherited Create;
end;

destructor TWatchExpectationList.Destroy;
begin
  Clear;
  FTypeNameAliases.Free;
  inherited Destroy;
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
  for i := 0 to Length(FList)-1 do
    FList[i].TstWatch.Free;
  FList := nil;
end;

function TWatchExpectationList.EvaluateWatches: Boolean;
var
  i, t, c: Integer;
begin
  t := LazDebugger.Threads.CurrentThreads.CurrentThreadId;
  for i := 0 to Length(FList)-1 do begin
    EvaluateWatch(FList[i], t);
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

end.

