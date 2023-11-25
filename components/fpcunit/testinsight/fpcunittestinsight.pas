unit FpcUnitTestInsight;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, types, DateUtils, fpjson, fpcunit, testregistry,
  TestInsightProtocol, TestInsightClient;

type

  { TFPCUnitTestInsightHelper }

  TFPCUnitTestInsightHelper = Class helper for TTestInsightResult
    Procedure FromTestFailure(ATest: TTest; aFailure: TTestFailure);
  end;

  { TTestInsightListener }

  TTestInsightListener = class(TInterfacedObject, ITestListener)
  private
    fClient: TAbstractTestInsightClient;
    fSelectedTests: TStringDynArray;
    FLastError : TTest;
    FStart : TDateTime;
    FRootTest : TTest;
  Protected
    procedure AddFailure(ATest: TTest; aFailure: TTestFailure); virtual;
    procedure AddError(ATest: TTest; aError: TTestFailure); virtual;
    procedure StartTest({%H-}ATest: TTest); virtual;
    procedure EndTest(ATest: TTest); virtual;
    procedure StartTestSuite(ATestSuite: TTestSuite); virtual;
    procedure EndTestSuite(ATestSuite: TTestSuite); virtual;
    procedure SendTestSuite; virtual;
  public
    constructor Create(const aClient : TAbstractTestInsightClient; aRoot: TTest);
    Property RootTest : TTest Read FRootTest;
  end;

procedure RunRegisteredTests(const aConfig : String = ''; const baseUrl: string = DefaultUrl);
function IsTestinsightListening(const aConfig : String = ''; const baseUrl: string = DefaultUrl) : Boolean;
Function DefaultTestConfigFileName : String;
Function TestSuiteToJSON(aSuite : TTest) : TJSONObject;
Procedure TestSuiteToJSON(aSuite : TTest; aJSON : TJSONObject);

implementation

Function DefaultTestConfigFileName : String;
begin
  Result:=ExtractFilePath(Paramstr(0));
  Result:=Result+'TestInsightSettings.ini';
end;

function TestSuiteToJSON(aSuite: TTest): TJSONObject;
begin
  Result:=TJSONObject.Create;
  try
   TestSuiteToJSOn(aSuite,Result);
  except
    Result.Free;
    Raise;
  end;
end;

procedure TestSuiteToJSON(aSuite: TTest; aJSON: TJSONObject);

Var
  T : TTest;
  I : Integer;
  j : TJSONObject;

begin
  For I:=0 to aSuite.GetChildTestCount-1 do
    begin
    T:=aSuite.GetChildTest(I);
    if T is TTestSuite then
      begin
      J:=TJSONObject.Create;
      aJSON.Add(T.TestName,J);
      TestSuiteToJSON(T as TTestSuite,J);
      end
    else
      aJSON.Add(T.TestName);
    end;
end;


Function CreateClient(const aConfig : String = ''; const baseUrl: string = DefaultUrl) : TAbstractTestInsightClient;

Var
  aURL,Cfg : String;

begin
  Cfg:=aConfig;
  if (Cfg='') then
    Cfg:=DefaultTestConfigFileName;
  aURL:=baseURL;
  if aURL='' then
    aURL:=DefaultURL;
  Result:=TTestInsightHTTPClient.Create(aURL);
  Result.LoadConfig(cfg);
end;

function IsTestinsightListening(const aConfig : String = ''; const baseUrl: string = DefaultUrl) : Boolean;

begin
  With CreateClient(aConfig,BaseURL) do
    try
      GetServerOptions;
      Result:=Not HasError;
    finally
      Free
    end;
end;

Procedure AddSkips (aResult : TTestResult; aSuite : TTest; aAllowed : TTest);

Var
  I : Integer;
  T : TTest;

begin
  if (aSuite=aAllowed) then exit;
  for I:=0 to aSuite.GetChildTestCount-1 do
    begin
    T:=aSuite.GetChildTest(I);
    if T is TTestCase then
      aResult.AddToSkipList(T as TTestCase)
    else
      AddSkips(aResult,T,aAllowed)
    end;
end;

procedure RunRegisteredTests(const aConfig : String = ''; const baseUrl: string = DefaultUrl);

var
  Suite: TTest;
  TestResult: TTestResult;
  Listener: TTestInsightListener;
  Client : TAbstractTestInsightClient;

begin
  Suite := GetTestRegistry;
  if not Assigned(Suite) then
    Exit;
  Client:=Nil;
  TestResult:=Nil;
  Listener:=Nil;
  try
    Client:=CreateClient(aConfig,BaseURL);
    Listener := TTestInsightListener.Create(Client, Suite);
    if Client.Options.ExecuteTests then
      begin
      TestResult := TTestResult.Create;
      if Client.Options.TestSuite<>'' then
        AddSkips(TestResult,Suite,Suite.FindTest(Client.Options.TestSuite));
      TestResult.AddListener(Listener as ITestListener);
      Suite.Run(TestResult);
      end
    else
      FreeAndNil(Listener);
  finally
    TestResult.Free;
    Client.Free;
  end;
end;

{ TFPCUnitTestInsightHelper }

procedure TFPCUnitTestInsightHelper.FromTestFailure(ATest: TTest; aFailure: TTestFailure);

Const
  TestStepToPhase : Array[TTestStep] of TTestPhase
    = (tpSetUp, tpRunTest, spTearDown, tpNothing);


begin
  TestName:=aTest.TestSuiteName+'.'+aTest.TestName;
  TestClassName:=aTest.ClassName;
  TestUnitName:=aTest.UnitName;
  TestMethodName := aTest.TestName;
  if not Assigned(aFailure) then
    exit;
  TestExceptionMessage := aFailure.ExceptionMessage;
  TestExceptionClass:= aFailure.ExceptionClassName;
  TestIsIgnored:=aFailure.IsIgnoredTest;
  if aFailure.IsFailure then
    TestResult:=rtFailed
  else
    TestResult:=rtError;
  TestPhase:=TestStepToPhase[aFailure.TestLastStep];
  FailureLineNumber:=aFailure.LineNumber;
  FailureUnitName:=aFailure.UnitName;
  FailureMethodName:=aFailure.FailedMethodName;
  FailureSourceUnitName:=aFailure.SourceUnitName;
  FailureLocationInfo:=aFailure.LocationInfo;
end;

{ TTestInsightTestListener }

constructor TTestInsightListener.Create(const aClient : TAbstractTestInsightClient; aRoot : TTest);

begin
  inherited Create;
  fClient := aClient;
  fSelectedTests := fClient.GetTests;
  FRootTest:=aRoot;
  SendTestSuite;
end;

procedure TTestInsightListener.AddError(ATest: TTest; aError: TTestFailure);

var
  testResult: TTestInsightResult;
begin
  testResult := TTestInsightResult.Create;
  testResult.FromTestFailure(aTest,aError);
  testResult.TestResult := rtError;
  fClient.PostResult(testResult,false);
end;

procedure TTestInsightListener.AddFailure(ATest: TTest; aFailure: TTestFailure);
var
  testResult: TTestInsightResult;
begin
  testResult := TTestInsightResult.Create;
  testResult.FromTestFailure(aTest,aFailure);
  if aFailure.ExceptionMessage = SAssertNotCalled then
    testResult.TestResult := rtWarning
  else
    testResult.TestResult := rtFailed;
  fClient.PostResult(testResult,False);
  FLastError:=aTest;
end;


procedure TTestInsightListener.EndTestSuite(ATestSuite: TTestSuite);
begin
  if (aTestSuite=FRootTest) then
    fClient.FinishedTesting;
end;

procedure TTestInsightListener.StartTestSuite(ATestSuite: TTestSuite);
begin
  if (aTestSuite=FRootTest) then
    fClient.StartedTesting(FRootTest.CountTestCases);
end;

procedure TTestInsightListener.StartTest(ATest: TTest);

begin
  FStart:=Now;
end;

procedure TTestInsightListener.EndTest(ATest: TTest);

var
  testResult: TTestInsightResult;

begin
  if Not ({IsTestMethod(aTest) and} (fLastError <> Atest)) then
    exit;
  testResult := TTestInsightResult.Create;
  TestResult.TestName:=aTest.TestSuiteName+'.'+aTest.TestName;
  TestResult.TestResult:=rtPassed;
  testResult.TestDuration := MilliSecondsBetween(Now,FStart);
  testResult.TestUnitName := aTest.UnitName;
  testResult.TestClassName := ATest.ClassName;
  testResult.TestMethodName := aTest.TestName;
  fClient.PostResult(testResult,False);
end;


procedure TTestInsightListener.SendTestSuite;

Var
  aJSON : TJSONObject;

begin
  aJSON:=TestSuiteToJSON(FRootTest);
  try
    fClient.SetTestNames(aJSON);
  finally
    aJSON.Free;
  end;
end;

end.

