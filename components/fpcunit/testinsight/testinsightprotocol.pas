unit testinsightprotocol;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpJSON;

Type
  ETestInsight = class(Exception);

  TTestResultType = (rtUnknown, rtPassed, rtFailed, rtError, rtWarning, rtSkipped, rtRunning);
  TTestPhase = (tpSetUp, tpRunTest, spTearDown, tpNothing);


  { TTestInsightResult }

  TTestInsightResult = Class
  Public
    TestResult : TTestResultType;
    TestName: string;
    TestDuration: Cardinal;
    TestUnitName: string;
    TestClassName: string;
    TestMethodName: string;
    // Status
    TestStatus: string;
    TestExceptionMessage: string;
    TestExceptionClass: string;
    TestIsIgnored : Boolean;
    TestPhase : TTestPhase;
    // Failure info
    FailureUnitName: string;
    FailureMethodName: string;
    FailureLineNumber: Integer;
    FailureSourceUnitName : String; // Exception
    FailureLocationInfo : String;
    Constructor Create; virtual;
    procedure FromJSON(const aJSON : TJSONStringType); overload;
    procedure FromJSON(const aJSON : TJSONObject); overload;
    function ToJSON: string;
    Procedure ToJSON(aJSON : TJSONObject);
  end;
  TTestInsightResultClass = Class of TTestInsightResult;
  TTestInsightResultArray = Array of TTestInsightResult;

  { TTestInsightOptions }

  TTestInsightOptions = Class (TPersistent)
  Public
    ExecuteTests: Boolean;
    ShowProgress: Boolean;
    TestSuite : String;
    Constructor Create; virtual;
    Procedure Assign(Source : TPersistent); override;
    procedure FromJSON(const aJSON : TJSONStringType); overload;
    procedure FromJSON(const aJSON : TJSONObject); overload;
    function ToJSON: string;
    Procedure ToJSON(aJSON : TJSONObject);
  end;
  TTestInsightOptionsClass = Class of TTestInsightOptions;

Const
  // Used in URLS
  DefaultUrl = 'http://localhost:8081/tests';

  pathTests = '/tests';
  pathStarted = 'started';
  pathFinished = 'finished';
  pathResults = 'results';
  pathOptions = 'options';
  qryTotalCount = 'totalcount';

  // Used in TTestInsightResult JSON encoding.
  keyTestName = 'testname';
  KeyTestUnitName = 'testunitname';
  keyTestClassName = 'testclassname';
  keyTestMethodName = 'testmethodname';
  keyTestDuration = 'testduration';

  keyResultType = 'testresulttype';
  keyExceptionMessage = 'exceptionmessage';
  KeyExceptionClass = 'exceptionclass';

  keyfailureMethodName = 'failuremethodname';
  keyFailureUnitName = 'failureunitname';
  keyFailureLineNumber = 'failurelinenumber';
  KeyFailureSourceUnitName = 'failuresourceunitname';
  KeyFailureLocationInfo = 'location';

  keyStatus = 'status';
  keyIsIgnored = 'ignored';
  KeyTestPhase = 'phase';

  // Config file settings
  SConfig = 'Config';
  KeyBaseURL = 'BaseUrl';

  // Used in TTestInsightOptions JSON encoding and Ini file
  keyExecuteTests = 'ExecuteTests';
  keyShowProgress = 'ShowProgress';
  KeySuite = 'Suite';

  ResultTypeNames: array[TTestResultType] of string
    = ('?','Passed', 'Failed', 'Error', 'Warning', 'Skipped', 'Running');
  TestPhaseNames: array[TTestPhase] of string
    = ('Setup', 'Run', 'TearDown','');

Function StringToResultType(aValue : string) : TTestResultType;
Function ResultTypeToString(aValue : TTestResultType) : string;

Function StringToTestPhase(aValue : string) : TTestPhase;
Function TestPhaseToString(aValue : TTestPhase) : string;


// Convert CR/LF separated list name to expected JSON format
Function TestStringsToJSON(aContent : String): TJSONObject;


implementation

Function ResultTypeToString(aValue : TTestResultType) : string;
begin
  Result:=ResultTypeNames[aValue];
end;

function StringToTestPhase(aValue: string): TTestPhase;
Var
  T : TTestPhase;

begin
  Result:=tpNothing;
  For T in TTestPhase do
    if SameText(TestPhaseNames[T],aValue) then
      Exit(T);
end;

function TestPhaseToString(aValue: TTestPhase): string;
begin
  Result:=TestPhaseNames[aValue];
end;

Function StringToResultType(aValue : string) : TTestResultType;

Var
  T : TTestResultType;

begin
  Result:=rtFailed;
  For T in TTestResultType do
    if SameText(ResultTypeNames[T],aValue) then
      Exit(T);
end;

Procedure AddTests(aParent : TJSONObject; aList : TStrings; aPath : String; var aIdx : Integer);

Var
  P : integer;
  Up : Boolean;
  Curr,Sub : String;
  Obj : TJSONObject;

begin
  Repeat
    Curr:=aList[aIdx];
    Up:=(aPath<>'') and Not SameText(Copy(Curr,1,Length(aPath)),aPath);
    if not Up then
      begin
      Delete(Curr,1,Length(aPath));
      if Curr[1]='.' then delete(Curr,1,1);
      P:=Pos('.',Curr);
      if P<>0 then
        begin
        Sub:=Copy(Curr,1,P-1);
        Obj:=TJSONObject.Create;
        aParent.Add(Sub,Obj);
        if aPath<>'' then
          AddTests(Obj,aList,APath+'.'+Sub,aIdx)
        else
          AddTests(Obj,aList,Sub,aIdx)
        end
      else
        begin
        aParent.Add(Curr,TJSONNull.Create);
        Inc(aIdx);
        end;
      end
  Until (aIdx>=aList.Count) or Up;
end;


Function TestStringsToJSON(aContent : String): TJSONObject;

Var
  L : TStringList;
  aPath : String;
  aIdx : Integer;

begin
  Result:=nil;
  L:=TStringList.Create;
  try
    L.Text:=aContent;
    L.Sort;
    Result:=TJSONObject.Create;
    aPath:='';
    aIdx:=0;
    AddTests(Result,L,aPath,aIdx);
  finally
    L.Free;
  end;
end;

{ TTestInsightOptions }

constructor TTestInsightOptions.Create;
begin
  ExecuteTests:=True;
  ShowProgress:=True;
end;

procedure TTestInsightOptions.Assign(Source: TPersistent);

var
  Src : TTestInsightOptions absolute source;

begin
  if Source is TTestInsightOptions then
    begin
    ExecuteTests:=Src.ExecuteTests;
    ShowProgress:=Src.ShowProgress;
    TestSuite:=Src.TestSuite;
    end
  else
    inherited Assign(Source);
end;

procedure TTestInsightOptions.FromJSON(const aJSON: TJSONStringType);
Var
  D : TJSONData;

begin
  D:=GetJSON(aJSON);
  try
    if D is TJSONObject then
      FromJSON(D as TJSONObject);
  finally
    D.Free;
  end;
end;

procedure TTestInsightOptions.FromJSON(const aJSON: TJSONObject);

begin
  with aJSON do
    begin
    ExecuteTests:=Get(KeyExecuteTests,ExecuteTests);
    ShowProgress:=Get(KeyShowProgress,ShowProgress);
    TestSuite:=Get(KeySuite,TestSuite);
    end;
end;

function TTestInsightOptions.ToJSON: string;

Var
  Obj : TJSONObject;

begin
  Obj:=TJSONObject.Create;
  try
    ToJSON(Obj);
    Result := Obj.AsJSON;
  finally
    Obj.Free;
  end;
end;

procedure TTestInsightOptions.ToJSON(aJSON: TJSONObject);

begin
  with aJSON do
    begin
    Add(KeyExecuteTests,ExecuteTests);
    Add(KeyShowProgress,ShowProgress);
    Add(KeySuite,TestSuite);
    end;
end;


{ TTestInsightResult }

constructor TTestInsightResult.Create;
begin
  TestResult:=rtUnknown;
end;

procedure TTestInsightResult.FromJSON(const aJSON: TJSONStringType);

Var
  D : TJSONData;

begin
  D:=GetJSON(aJSON);
  try
    if D is TJSONObject then
      FromJSON(D as TJSONObject);
  finally
    D.Free;
  end;
end;

procedure TTestInsightResult.FromJSON(const aJSON: TJSONObject);

begin
  With aJSON Do
    begin
    TestName:=Get(KeyTestName, TestName);
    TestUnitName:=Get(KeyTestUnitName, TestUnitName);
    TestClassName:=Get(KeyTestClassName, TestClassName);
    TestMethodName:=Get(KeyTestMethodName, TestMethodName);

    TestResult:=StringToResultType(Get(KeyResultType, ResultTypeNames[TestResult]));
    TestDuration:=Get(KeyTestDuration, TestDuration);
    TestStatus:=Get(Keystatus, TestStatus);
    TestIsIgnored:=Get(keyIsIgnored,TestIsIgnored);
    TestPhase:=StringToTestPhase(Get(KeyTestPhase,TestPhaseToString(TestPhase)));

    TestExceptionMessage:=Get(KeyExceptionMessage, TestExceptionMessage);
    TestExceptionClass:=Get(KeyExceptionClass, TestExceptionClass);

    FailureUnitName:=Get(KeyFailureUnitName, FailureUnitName);
    FailureMethodName:=Get(KeyFailureMethodName, FailureMethodName);
    FailureLineNumber:=Get(KeyFailureLineNumber, FailureLineNumber);
    FailureSourceUnitName:=Get(KeyFailureSourceUnitName,FailureSourceUnitName);
    FailureLocationInfo:=Get(KeyFailureLocationInfo,FailureLocationInfo);
    end;
end;

function TTestInsightResult.ToJSON: string;

Var
  Obj : TJSONObject;

begin
  Obj:=TJSONObject.Create;
  try
    ToJSON(Obj);
    Result := Obj.AsJSON;
  finally
    Free;
  end;
end;

procedure TTestInsightResult.ToJSON(aJSON: TJSONObject);
begin
  With aJSON do
    begin
    Add(KeyTestname, TestName);
    Add(KeyTestUnitName, TestUnitName);
    Add(KeyTestClassName, TestClassName);
    Add(KeyTestMethodName,TestMethodName);

    Add(KeyResultType, ResultTypeNames[TestResult]);
    Add(KeyTestDuration, TestDuration);
    Add(KeyStatus, TestStatus);
    Add(keyIsIgnored,TestIsIgnored);
    Add(KeyTestPhase,TestPhaseToString(TestPhase));

    Add(KeyExceptionMessage, TestExceptionMessage);
    Add(KeyExceptionClass, TestExceptionClass);

    Add(KeyFailureUnitName, FailureUnitName);
    Add(KeyFailureMethodName, FailureMethodName);
    Add(KeyFailureLinenumber, FailureLineNumber);
    Add(KeyFailureSourceUnitName,FailureSourceUnitName);
    Add(KeyFailureLocationInfo,FailureLocationInfo);
    end;
end;


end.

