unit TTestWatchUtilities;

{$mode objfpc}{$H+}
{$modeswitch AdvancedRecords}

interface

uses
  Classes, SysUtils, DbgIntfBaseTypes, DbgIntfDebuggerBase, RegExpr,
  TestDbgTestSuites, TTestDebuggerClasses, TTestDbgExecuteables, TestDbgConfig,
  TestOutputLogger;

type
  TWatchExpectationResultKind = (
    rkMatch, rkInteger, rkCardinal,
    rkChar, rkAnsiString, rkShortString,
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
     ehExpectError
    );
  TWatchExpErrorHandlingFlags = set of TWatchExpErrorHandlingFlag;


  TWatchExpectationResult = record
    //ResultKind: TWatchExpectationResultKind;

    ExpTextData: string; // depends on ResultKind
    ExpSymKind: TDbgSymbolKind; // skSimple, skInteger...
    ExpTypeName: string; // AnsiString, Integer, TObject...
    ExpErrorHandlingFlags: Array [TSymbolType] of TWatchExpErrorHandlingFlags;

    ExpSubResults: Array of TWatchExpectationResult;
//    MinDbg, MinFpc: Integer;
    //FullTypesExpect: TFullTypeMemberExpectationResultArray;

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
  end;

  (* Do *NOT* start any other identifiers with "Tst...".
     There are a lot of
       with TWatchExpectation do ...
     blocks
  *)
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
  end;
  PWatchExpectation = ^TWatchExpectation;

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
  protected
    procedure WaitWhileEval; virtual;

    function TestMatches(Name: string; Expected, Got: string; AContext: TWatchExpTestCurrentData; AIgnoreReason: String): Boolean;
    function TestMatches(Name: string; Expected, Got: string; ACaseSense: Boolean; AContext: TWatchExpTestCurrentData; AIgnoreReason: String): Boolean;
    function TestEquals(Name: string; Expected, Got: string; AContext: TWatchExpTestCurrentData; AIgnoreReason: String): Boolean;
    function TestEquals(Name: string; Expected, Got: string; ACaseSense: Boolean; AContext: TWatchExpTestCurrentData; AIgnoreReason: String): Boolean;
    function TestEquals(Name: string; Expected, Got: integer; AContext: TWatchExpTestCurrentData; AIgnoreReason: String): Boolean;
    function TestTrue(Name: string; Got: Boolean; AContext: TWatchExpTestCurrentData; AIgnoreReason: String): Boolean;
    function TestFalse(Name: string; Got: Boolean; AContext: TWatchExpTestCurrentData; AIgnoreReason: String): Boolean;

    function CheckResult(AnWatchExp: TWatchExpectation): Boolean;
    function VerifyDebuggerState: Boolean; virtual;
    function VerifySymType(AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean; virtual;
    function VerifyTypeName(AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean; virtual;

    function CheckResultMatch(AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean; virtual;
    function CheckResultNum(AContext: TWatchExpTestCurrentData; IsCardinal: Boolean; AnIgnoreRsn: String): Boolean; virtual;
    function CheckResultChar(AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean; virtual;
    function CheckResultAnsiStr(AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean; virtual;
    function CheckResultShortStr(AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean; virtual;

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

function weInteger(AExpVal: Integer; ATypeName: String=#1; ASize: Integer = 4): TWatchExpectationResult;
function weCardinal(AExpVal: Integer; ATypeName: String=#1; ASize: Integer = 4): TWatchExpectationResult;

function weChar(AExpVal: char; ATypeName: String=#1): TWatchExpectationResult;
function weAnsiStr(AExpVal: string; ATypeName: String=#1): TWatchExpectationResult;
function weShortStr(AExpVal: string; ATypeName: String=#1): TWatchExpectationResult;

function weClass(AExpClass: String; AExpFields: array of TWatchExpectationResult; ATypeName: String=#1): TWatchExpectationResult;
function weField(AExpName: String; AExpVal: TWatchExpectationResult; ATypeName: String=#1): TWatchExpectationResult;

operator := (a:string): TWatchExpectationResult;
operator := (a:integer): TWatchExpectationResult;



implementation

operator := (a:string): TWatchExpectationResult;
begin
  Result := weAnsiStr(a);
end;

operator := (a:integer): TWatchExpectationResult;
begin
  Result := weInteger(a);
end;

function weMatch(AExpVal: String; ASymKind: TDBGSymbolKind; ATypeName: String
  ): TWatchExpectationResult;
begin
  Result.ExpResultKind := rkMatch;
  Result.ExpSymKind := ASymKind;
  Result.ExpTextData := AExpVal;
end;

function weInteger(AExpVal: Integer; ATypeName: String; ASize: Integer
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

function weCardinal(AExpVal: Integer; ATypeName: String; ASize: Integer
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

function weChar(AExpVal: char; ATypeName: String): TWatchExpectationResult;
begin
  Result := Default(TWatchExpectationResult);
  if ATypeName = #1 then ATypeName := 'Char';
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
  Result.ExpSymKind := skSimple; // skShortString does not (yet) exist
  Result.ExpTypeName := ATypeName;
  Result.ExpTextData := AExpVal;
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
  with AnWatchExp do begin
    try
      FTest.TestBaseName := FTest.TestBaseName + ' ' + TstTestName + ' (AT '+ LazDebugger.GetLocation.SrcFile + ':' + IntToStr(LazDebugger.GetLocation.SrcLine) +')';
      if not VerifyDebuggerState then
        exit;
      FTest.LogText('###### ' + TstTestName + ' // ' + TstWatch.Expression + '###### '+LineEnding);

      AnIgnoreRsn := '';
      ehf := Context.Expectation.ExpErrorHandlingFlags[Compiler.SymbolType];
      if ehIgnAll in ehf then
        AnIgnoreRsn := AnIgnoreRsn + 'All ignored';

      Thread := LazDebugger.Threads.CurrentThreads.CurrentThreadId;
      Stack  := TstStackFrame;
      WatchVal := TstWatch.Values[Thread, Stack];
      Context.WatchVal := WatchVal;
TestLogger.DebugLn(['>>> ', dbgs(WatchVal.Validity), ' / ',WatchVal.Value]);

      if not VerifyDebuggerState then
        exit;

      if ehExpectError in ehf then begin
        Result := TestTrue('TstWatch.value is NOT valid', WatchVal.Validity = ddsInvalid, Context, AnIgnoreRsn);
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
      case TstExpected.ExpResultKind of
        rkMatch:       Result := CheckResultMatch(Context, AnIgnoreRsn);
        rkInteger:     Result := CheckResultNum(Context, False, AnIgnoreRsn);
        rkCardinal:    Result := CheckResultNum(Context, True, AnIgnoreRsn);
        rkChar:        Result := CheckResultChar(Context, AnIgnoreRsn);
        rkAnsiString:  Result := CheckResultAnsiStr(Context, AnIgnoreRsn);
        rkShortString: Result := CheckResultShortStr(Context, AnIgnoreRsn);
        rkClass: ;
        rkObject: ;
        rkRecord: ;
        rkField: ;
        rkStatArray: ;
        rkDynArray: ;
      end;

    finally
      FTest.TestBaseName := CurBaseName;
    end;
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
  end;
end;

function TWatchExpectationList.CheckResultChar(
  AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean;
var
  Expect: TWatchExpectationResult;
  ehf: TWatchExpErrorHandlingFlags;
begin
  with AContext.WatchExp do begin
    Result := True;
    Expect := AContext.Expectation;

    ehf := Expect.ExpErrorHandlingFlags[Compiler.SymbolType];
    if ehCharFromIndex in ehf then begin
      Result := TestMatches('Data', '([Pp][Cc]har|[Ss]tring):? *'''+Expect.ExpTextData+'''',
        AContext.WatchVal.Value, EqMatchCase, AContext, AnIgnoreRsn);
      exit;
    end;

    Result := TestEquals('Data', Expect.ExpTextData, AContext.WatchVal.Value, AContext, AnIgnoreRsn);
  end;
end;

function TWatchExpectationList.CheckResultAnsiStr(
  AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean;
var
  Expect: TWatchExpectationResult;
begin
  with AContext.WatchExp do begin
    Result := True;
    Expect := AContext.Expectation;

    Result := TestEquals('Data', Expect.ExpTextData, AContext.WatchVal.Value, AContext, AnIgnoreRsn);
  end;
end;

function TWatchExpectationList.CheckResultShortStr(
  AContext: TWatchExpTestCurrentData; AnIgnoreRsn: String): Boolean;
var
  Expect: TWatchExpectationResult;
begin
  with AContext.WatchExp do begin
    Result := True;
    Expect := AContext.Expectation;

    Result := TestEquals('Data', Expect.ExpTextData, AContext.WatchVal.Value, AContext, AnIgnoreRsn);
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
  Result := Add(AnExpr, AnExpr, AnExpect, AStackFrame, AMinFpc, AMinDbg);
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
  for i := 0 to Length(FList)-1 do
    FList[i].TstWatch.Values[t, FList[i].TstStackFrame].Value;
  i := Length(FList)-1;
  c := Length(FList);
  while c > 0 do begin
    WaitWhileEval;
    Result := FList[i].TstWatch.Values[t, FList[i].TstStackFrame].Validity <> ddsRequested;
    if Result then break;
    dec(c);
  end;
end;

procedure TWatchExpectationList.CheckResults;
var
  i: Integer;
begin
  for i := 0 to Length(FList)-1 do
    CheckResult(FList[i]);
end;

end.

