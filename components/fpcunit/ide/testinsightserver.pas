unit TestInsightServer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, types, httpdefs, syncobjs, fphttpserver, fpJSON, testinsightprotocol;

{$IF DECLARED(TCORSSUPPORT)}
{$DEFINE USECORS}
{$ENDIF}


Type
  TTestItem = Class;

  { TTestItem }

  TTestItem = Class
  Public
    TestParent : TTestItem;
    TestName : String;
    Tests : Array of TTestItem;
    TestResult : TTestInsightResult;
    Constructor Create(aParent : TTestItem; const aTestName : String);
    Destructor Destroy; override;
    Procedure FromJSON(aJSON : TJSONObject);
    Function TestPath : String;
    Function CountTestCases : Integer;
    Function IsTestSuite : Boolean;
    Function IsTestCase : Boolean;
  end;

   (*
     // Incoming
     {
       "name" : null
       "suite" : {
         "name1" : null,
         "name2" : null
       }
     }
     // Internal

      {
        "name" : { "$Result" : { }, "$Path" : "name" }
        "suite" : {
          "$path'
          name1 : { $Result : {} , "$Path" : "suite.name1" }
          name1 : { $Result : {} , "$Path" : "suite.name2"}
        }
      }
   *)


   { TTestInsightServer }

   // Object Receiver frees received object.
   TTestNamesEvent = Procedure(Sender : TObject; Tests : TTestItem) of object;
   TSelectedTestsEvent = Procedure(Sender : TObject; var Tests: TStringDynArray) of object;
   TTestResultEvent = Procedure(Sender : TObject; aResult : TTestInsightResultArray) of object;
   TTestsStartedEvent = Procedure(Sender : TObject; aCount : Integer) of object;
   TTestsOptionsEvent = Procedure(Sender : TObject; aOptions : TTestInsightOptions) of object;
   TInsightMessageType = (imtInfo,imtError);
   TTestInsightLogEvent = Procedure(Sender : TObject; const aType : TInsightMessageType; const aMessage : String) of object;

   TTestInsightServer = class(TComponent)
   private
     FBasePath: String;
     FOnClearTests: TNotifyEvent;
     FOnLog: TTestInsightLogEvent;
     FOnTestsFinished: TNotifyEvent;
     FOnTestsStarted: TTestsStartedEvent;
     FServer : TFPHttpServer;
     FOnSelectedTests: TSelectedTestsEvent;
     FOnSetTestNames: TTestNamesEvent;
     FOnTestResult: TtestResultEvent;
     FSelectedTests : String;
     FTestInsightResultClass: TTestInsightResultClass;
     FTestSuite : TTestItem;
     FTestCount : Integer;
     FInsightOptions : TTestInsightOptions;
     FOnGetOptions: TTestsOptionsEvent;
     FThread: TThread;
     FServerPort : Word;
     FServerActive : Boolean;
{$IFDEF USECORS}
     FCorsSupport : TCORSSupport;
{$ENDIF}
     procedure CreateServer;
     function ExtractResults(anArray: TJSONArray): TTestInsightResultArray;
     procedure FreeResults(Results: TTestInsightResultArray);
     function GetPort: Word;
     procedure HandleStartThreadTerminate(Sender: TObject);
     procedure SetBasePath(AValue: String);
{$IFDEF USECORS}
     procedure SetCorsSupport(AValue: TCORSSupport);
{$ENDIF}
     procedure SetPort(AValue: Word);
   Protected
     Procedure DoLog(const aType : TInsightMessageType; const aMessage : String);
     Procedure DoLog(const aType : TInsightMessageType; const Fmt : String; Args : Array of const);
     // Override if you want to create a descendent.
     function CreateTestInsightOptions: TTestInsightOptions; virtual;
     // these are called in the main thread
     procedure DoGetSelectedTests; virtual;
     procedure DoSetTestNames; virtual;
     procedure DoTestsStarted; virtual;
     procedure DoTestsFinished; virtual;
     procedure DoClearTests; virtual;
     procedure DoOptions; virtual;
     procedure Send400(aResponse: TFPHTTPConnectionResponse; aText: String);
     Procedure Send404(aResponse : TFPHTTPConnectionResponse);
     Procedure Send405(aResponse : TFPHTTPConnectionResponse);
     Procedure Send200(aResponse : TFPHTTPConnectionResponse);
     procedure SetTestList(ARequest: TFPHTTPConnectionRequest; aResponse: TFPHTTPConnectionResponse);
     procedure DoTestResults(ARequest: TFPHTTPConnectionRequest;  aResponse: TFPHTTPConnectionResponse);
     procedure ClearTestList(aResponse: TFPHTTPConnectionResponse); virtual;
     procedure DoFinishTests(aResponse: TFPHTTPConnectionResponse); virtual;
     procedure DoStartTests(aResponse: TFPHTTPConnectionResponse; aCount: Integer); virtual;
     procedure SendSelectedTestList(aResponse: TFPHTTPConnectionResponse); virtual;
     procedure DoGetOptions(aResponse: TFPHTTPConnectionResponse); virtual;
     procedure DoRequest(Sender: TObject;
       var ARequest: TFPHTTPConnectionRequest;
       var AResponse: TFPHTTPConnectionResponse);
   Public
     constructor create(aOwner : TComponent); override;
     destructor destroy; override;
     procedure StartServer;
     procedure StopServer;
     Property Active : Boolean Read FServerActive;
     // TestResultClass
     Property TestInsightResultClass : TTestInsightResultClass Read FTestInsightResultClass Write FTestInsightResultClass;
     // Options sent to the client.
     property InsightOptions : TTestInsightOptions Read FInsightOptions;
     // Port on which to listen
     property Port: Word Read GetPort Write SetPort;
     // First part of URL. By default: /tests
     Property BasePath : String Read FBasePath Write SetBasePath;
     // CORS Support ?
{$IFDEF USECORS}
     Property CorsSupport : TCORSSupport Read FCorsSupport Write SetCorsSupport;
{$ENDIF}
     // Set the list of tests. Event handler must free JSON object.
     Property OnSetTestNames : TTestNamesEvent Read FOnSetTestNames Write FOnSetTestNames;
     // Get the list of selected tests. The server will free the received object.
     Property OnGetSelectedTests : TSelectedTestsEvent Read FOnSelectedTests Write FOnSelectedTests;
     // Results of run tests. Event handler must free result array.
     Property OnTestResult : TTestResultEvent Read FOnTestResult Write FOnTestResult;
     // Tests started.
     Property OnTestsStarted : TTestsStartedEvent  Read FOnTestsStarted Write FOnTestsStarted;
     // Tests finished.
     Property OnTestsFinished : TNotifyEvent  Read FOnTestsFinished Write FOnTestsFinished;
     // Clear tests.
     Property OnClearTests : TNotifyEvent  Read FOnClearTests Write FOnClearTests;
     // Last change to set options before sending to the client. If not, Options are sent as-is.
     Property OnGetOptions : TTestsOptionsEvent Read FOnGetOptions Write FOnGetOptions;
     // Diagnostic messages
     Property OnLog : TTestInsightLogEvent Read FOnLog Write FOnLog;
   end;


implementation

Uses ssockets;

Type

  { TStartServerThread }

  TStartServerThread = Class(TThread)
  Private
    FServer : TFPHttpServer;
    FStartErrorClass: String;
    FStartErrorMessage: String;
  Public
    constructor Create(aServer: TFPHttpServer; aOnTerminate: TNotifyEvent);
    Procedure Execute; override;
    Property StartErrorMessage : String Read FStartErrorMessage;
    Property StartErrorClass : String Read FStartErrorClass;
  end;

{ TTestItem }

constructor TTestItem.Create(aParent: TTestItem; const aTestName: String);
begin
  TestParent:=aParent;
  TestName:=aTestName;
end;

destructor TTestItem.Destroy;

Var
  I : Integer;

begin
  For I:=0 to Length(Tests)-1 do
    FreeAndNil(Tests[i]);
  SetLength(Tests,0);
  FreeAndNil(TestResult);
  Inherited;
end;

procedure TTestItem.FromJSON(aJSON: TJSONObject);

Var
  I : Integer;

begin
  SetLength(Tests,aJSON.Count);
  For I:=0 to aJSON.Count-1 do
    begin
    Tests[i]:=TTestItem.Create(Self, aJSON.Names[i]);
    if aJSON.Items[i].JSONType=jtObject then
      Tests[i].FromJSON(TJSONObject(aJSON.Items[i]));
    end;
end;

function TTestItem.TestPath: String;

Var
  P : TTestItem;

begin
  if Self=Nil then exit('');
  Result:=TestName;
  P:=TestParent;
  While (P<>Nil) do
    begin
    if P.TestName<>'' then
      Result:=P.TestName+'.'+Result;
    P:=P.TestParent;
    end;
end;

function TTestItem.CountTestCases: Integer;

Var
  I : integer;

begin
  if Self=Nil then exit(0);
  Result:=0;
  for I:=0 to Length(Tests)-1 do
    Result:=Result+Tests[i].CountTestCases;
  If (Result=0) and (TestName<>'') then
    Inc(Result);
end;

function TTestItem.IsTestSuite: Boolean;
begin
  if Self=Nil then exit(False);
  Result:=Length(Tests)>0;
end;

function TTestItem.IsTestCase: Boolean;
begin
  if Self=Nil then exit(False);
  Result:=Length(Tests)=0;
end;

{ TStartServerThread }

constructor TStartServerThread.Create(aServer: TFPHttpServer; aOnTerminate : TNotifyEvent);
begin
  FServer:=aServer;
  FreeOnTerminate:=True;
  OnTerminate:=aOnTerminate;
  Inherited Create(False);
end;

procedure TStartServerThread.Execute;
begin
  try
    FServer.Active:=true;
  except
    On E : Exception do
      begin
      FStartErrorClass:=E.ClassName;
      FStartErrorMessage:=E.Message;
      end;
  end;
end;

{ TTestInsightServer }

Type

  { TTransferTestResult }

  TTransferTestResult = Class
  Private
    FEvent: TTestResultEvent;
    FSender : TObject;
    FResult : TTestInsightResultArray;
  Public
    constructor create (aEvent: TTestResultEvent; aSender : TObject; aResult : TTestInsightResultArray);
    procedure DoResultEvent;
  end;

constructor TTransferTestResult.create(aEvent: TTestResultEvent; aSender: TObject; aResult: TTestInsightResultArray);
begin
  FEvent:=aEvent;
  FSender:=aSender;
  FResult:=aResult;
end;

procedure TTransferTestResult.DoResultEvent;

begin
  if Assigned(FEvent) then
    FEvent(FSender,FResult);
end;

procedure TTestInsightServer.DoSetTestNames;
begin
  if Assigned(FOnSetTestNames) then
    FOnSetTestNames(Self,FTestSuite);
end;

procedure TTestInsightServer.DoTestsStarted;
begin
  if Assigned(FOnTestsStarted) then
    FOnTestsStarted(Self,Self.FTestCount);
end;

procedure TTestInsightServer.DoTestsFinished;
begin
  if Assigned(FOnTestsFinished) then
    FOnTestsFinished(Self);
end;

procedure TTestInsightServer.DoClearTests;
begin
  if Assigned(FOnClearTests) then
    FOnClearTests(Self);
end;

procedure TTestInsightServer.DoOptions;
begin
  if Assigned(FOnGetOptions) then
    FOnGetOptions(Self,FInsightOptions);
end;


procedure TTestInsightServer.SetTestList(ARequest: TFPHTTPConnectionRequest; aResponse: TFPHTTPConnectionResponse);

Var
  D : TJSONData;

begin
  D:=Nil;
  try
    if SameText(aRequest.ContentType,'application/json') then
      D:=GetJSON(aRequest.Content)
    else if SameText(aRequest.ContentType,'text/text') then
      D:=TestStringsToJSON(aRequest.Content)
    else
      Send400(aResponse,'Invalid content type. Need application/json or text/text');
    if (D<>Nil) and not (D is TJSONObject) then
      begin
      Send400(aResponse,'Invalid JSON, need object');
      FreeAndNil(D);
      end;
  except
    on E : exception do
      begin
      Send400(aResponse,E.Message);
      Raise;
      end;
  end;
  if D=nil then
    Exit;
  // Here we have a valid test list
  Send200(aResponse);
  if Assigned(D) then
    begin
    FTestSuite:=TtestItem.Create(Nil,'');
    FTestSuite.FromJSON(D as TJSONObject);
    if Assigned(OnSetTestNames) then
      TThread.Synchronize(TThread.CurrentThread,@DoSetTestNames)
    else
      FTestSuite.Free;
    D.Free;
    end;
end;

Function TTestInsightServer.ExtractResults(anArray : TJSONArray) : TTestInsightResultArray;

Var
  i,aLen : Integer;
  Res: TTestInsightResult;


begin
  aLen:=0;
  SetLength(Result,anArray.Count);
  For I:=0 to anArray.Count-1 do
    begin
    if anArray.Types[i]=jtObject then
      begin
      Res:=TestInsightResultClass.Create;
      try
        Res.FromJSOn(anArray.Objects[i]);
      except
        on E : Exception do
          begin
          FreeAndNil(res);
          DoLog(imtError,'Error %s extracting test result: %s',[E.ClassName, E.Message]);
          end;
      end;
      if Assigned(Res) then
        begin
        Result[aLen]:=Res;
        Inc(aLen);
        end;
      end;
    end;
end;

procedure TTestInsightServer.DoTestResults(ARequest: TFPHTTPConnectionRequest; aResponse: TFPHTTPConnectionResponse);

Var
  D : TJSONData;
  Results : TTestInsightResultArray;
  Trans : TTransferTestResult;

begin
  Results:=Nil;
  try
    D:=GetJSON(aRequest.Content);
  except
    on E : exception do
      begin
      Send400(aResponse,E.Message);
      Raise;
      end;
  end;
  try
    if D is TJSONArray then
      begin
      Send200(aResponse);
      Results:=ExtractResults(D as TJSONArray);
      end
    else if (D is TJSONObject) and (D.Count=1) and (D.Items[0] is TJSONArray) then
      begin
      Send200(aResponse);
      Results:=ExtractResults(TJSONObject(D).Extract(0) as TJSONArray);
      end
    else
      Send400(aResponse,'Bad JSON message');
  finally
    D.Free;
  end;
  if not (Assigned(Results) and Assigned(OnTestResult)) then
    Exit;
  Trans:=TTransferTestResult.create(OnTestResult,Self,Results);
  try
    TThread.Synchronize(TThread.CurrentThread,@Trans.DoResultEvent);
    FreeResults(Results);
  finally
    Trans.Free;
  end;
end;

procedure TTestInsightServer.FreeResults(Results : TTestInsightResultArray);

Var
  Res : TTestInsightResult;

begin
  For Res in Results do
    Res.Free;
  SetLength(Results,0);
end;


procedure TTestInsightServer.DoGetSelectedTests;

Var
  aTests : TStringDynArray;
  D : TJSONObject;
  A : TJSONArray;
  S : String;

begin
  aTests:=Nil;
  if Assigned(FOnSelectedTests) then
    FOnselectedTests(Self,aTests);
  A:=TJSONArray.Create;
  try
    D:=TJSONObject.Create(['tests',A]);
    For S in aTests do
      A.Add(S);
    FSelectedTests:=D.asJSON;
  Finally
    D.Free;
  end;
end;

procedure TTestInsightServer.SendSelectedTestList(aResponse: TFPHTTPConnectionResponse);
begin
  FSelectedTests:='';
  if Assigned(FOnSelectedTests) then
    TThread.Synchronize(TThread.CurrentThread,@DoGetSelectedTests);
  if FSelectedTests='' then
    FSelectedTests:='{ "tests" : [] }';
  aResponse.Content:=FSelectedTests;
  aResponse.ContentType:='application/json';
  send200(aResponse);
end;

procedure TTestInsightServer.DoGetOptions(aResponse: TFPHTTPConnectionResponse);
begin
  if Assigned(FONGetOptions) then
    TThread.Synchronize(TThread.CurrentThread,@DoOptions);
  aResponse.Content:=FInsightOptions.ToJSON;
  aResponse.ContentType:='application/json';
  send200(aResponse);
end;

procedure TTestInsightServer.ClearTestList(aResponse: TFPHTTPConnectionResponse);

begin
  Send200(aResponse);
  if Assigned(FOnClearTests) then
    TThread.Synchronize(TThread.CurrentThread,@DoClearTests);
end;

procedure TTestInsightServer.DoStartTests(aResponse: TFPHTTPConnectionResponse; aCount : Integer);

begin
  Send200(aResponse);
  FTestCount:=aCount;
  if Assigned(FOnTestsStarted) then
    TThread.Synchronize(TThread.CurrentThread,@DoTestsStarted);
end;

procedure TTestInsightServer.DoFinishTests(aResponse: TFPHTTPConnectionResponse);

begin
  Send200(aResponse);
  if Assigned(FonTestsFinished) then
    TThread.Synchronize(TThread.CurrentThread,@DoTestsFinished);
end;

procedure TTestInsightServer.DoRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);

  function CheckMethod(aMethod : String) : Boolean;

  begin
    Result:=SameText(aMethod,aRequest.Method);
    if not Result then
      Send405(aResponse);
  end;

Var
  aPath : String;

begin
  aPath:=aRequest.PathInfo;
  DoLog(imtInfo,'Handling request %s %s',[aRequest.Method,aPath]);
  if not SameText(Copy(aPath,1,Length(BasePath)),BasePath) then
    Send404(aResponse)
  else
    begin
{$IFDEF USECORS}
    if FCorsSupport.HandleRequest(aRequest,aResponse,[hcDetect, hcsend]) then
      exit;
{$ENDIF}
    Delete(aPath,1,Length(BasePath));
    if (aPath='') then // '/tests'
      begin
      if SameText(aRequest.Method,'POST') then
        SetTestList(aRequest,aResponse)
      else if SameText(aRequest.Method,'get') then
        SendSelectedTestList(aResponse)
      else if SameText(aRequest.Method,'delete') then
        ClearTestList(aResponse)
      else
        Send405(aResponse)
      end
    else
      begin
      If aPath[1]='/' then
        delete(aPath,1,1);
      if SameText(aPath,pathStarted) then
        begin
        if CheckMethod('POST') then
          DoStartTests(aResponse,StrToIntDef(aRequest.QueryFields.Values[qryTotalCount],-1));
        end
      else if SameText(aPath,pathFinished) then
        begin
        if CheckMethod('POST') then
          DoFinishTests(aResponse);
        end
      else if SameText(aPath,PathResults) then
        begin
        if CheckMethod('POST') then
          DoTestResults(aRequest,aResponse);
        end
      else if SameText(aPath,PathOptions) then
        begin
        if CheckMethod('GET') then
          DoGetOptions(aResponse);
        end
      else
        Send404(aResponse);
      end;
    end;
end;

function TTestInsightServer.GetPort: Word;
begin
  if Assigned(FServer) then
    Result:=FServer.Port
  else
    Result:=FServerPort;
end;

procedure TTestInsightServer.HandleStartThreadTerminate(Sender: TObject);

var
  SThread : TStartServerThread ;
  ErrClass,ErrMsg : String;

begin
  If Sender<>FThread then exit;
  SThread:=Sender as TStartServerThread;
  ErrClass:=SThread.StartErrorClass;
  ErrMsg:=SThread.StartErrorMessage;
  if ErrClass<>'' then
    DoLog(imtError,'Error %s starting server: %s',[ErrClass,ErrMsg]);
  FThread:=Nil;
  FServerActive:=False;

end;


procedure TTestInsightServer.SetBasePath(AValue: String);

var
  len : integer;

begin
  // Make sure it starts with /
  if (AValue<>'') and (AValue[1]<>'/') then
    AValue:='/'+AValue;
  // Make sure it does not end with /
  Len:=Length(AValue);
  if (Len>1) and (AValue[Len]='/') then
    AValue:=Copy(AValue,1,Len-1)
  else if Len<2 then
    Raise ETestInsight.Create('Basepath cannot be empty');
  if FBasePath=AValue then Exit;
  FBasePath:=AValue;
end;

{$IFDEF USECORS}
procedure TTestInsightServer.SetCorsSupport(AValue: TCORSSupport);
begin
  if FCorsSupport=AValue then Exit;
  FCorsSupport.Assign(AValue);
end;
{$ENDIF}

procedure TTestInsightServer.SetPort(AValue: Word);
begin
  FServer.Port:=aValue;
  FServerPort:=aValue;
end;


procedure TTestInsightServer.DoLog(const aType : TInsightMessageType;const aMessage: String);
begin
  If Assigned(FOnLog) then
    FOnLog(Self,aType,aMessage);
end;

procedure TTestInsightServer.DoLog(const aType : TInsightMessageType;const Fmt: String; Args: array of const);
begin
  DoLog(aType,Format(Fmt,Args));
end;

procedure TTestInsightServer.Send400(aResponse: TFPHTTPConnectionResponse; aText : String);
begin
  aResponse.Code:=400;
  aResponse.CodeText:='400 BAD REQUEST';
  if aText<>'' then
    aResponse.Content:=aText
  else
    aResponse.Content:='The requested URL is invalid';
  aResponse.ContentType:='text/text';
  aResponse.SendResponse;
end;

procedure TTestInsightServer.Send404(aResponse: TFPHTTPConnectionResponse);
begin
  aResponse.Code:=404;
  aResponse.CodeText:='NOT FOUND';
  aResponse.Content:='The requested URL is invalid';
  aResponse.ContentType:='text/text';
  aResponse.SendResponse;
end;

procedure TTestInsightServer.Send405(aResponse: TFPHTTPConnectionResponse);
begin
  aResponse.Code:=405;
  aResponse.CodeText:='METHOD NOT ALLOWED';
  aResponse.Content:='The requested HTTP method is invalid';
  aResponse.ContentType:='text/text';
  aResponse.SendResponse;
end;

procedure TTestInsightServer.Send200(aResponse: TFPHTTPConnectionResponse);
begin
  aResponse.Code:=200;
  aResponse.CodeText:='OK';
  aResponse.ContentType:='application/json';
  aResponse.SendResponse;
end;

constructor TTestInsightServer.create(aOwner: TComponent);
begin
  inherited create(aOwner);
  CreateServer;
  BasePath:=pathTests;
  FTestInsightResultClass:=TTestInsightResult;
  FInsightOptions:=CreateTestInsightOptions;
{$IFDEF USECORS}
  FCorsSupport:=TCORSSupport.Create;
  FCorsSupport.Enabled:=True;
{$ENDIF}
end;


destructor TTestInsightServer.destroy;
begin
  StopServer;
  FreeAndNil(FServer);
  FreeAndNil(FInsightOptions);
{$IFDEF USECORS}
  FreeAndNil(FCorsSupport);
{$ENDIF}
  inherited destroy;
end;

procedure TTestInsightServer.CreateServer;

begin
  FServer:=TFPHttpServer.Create(Self);
  FServer.Threaded:=True;
  if FServerPort>0 then
    FServer.Port:=FServerPort;
end;

function TTestInsightServer.CreateTestInsightOptions: TTestInsightOptions;

begin
  Result:=TTestInsightOptions.Create;
end;

procedure TTestInsightServer.StartServer;
begin
  if FServerActive then
    exit;
  FServer.OnRequest:=@DoRequest;
  FServerActive:=True;
  FThread:=TStartServerThread.Create(FServer,@HandleStartThreadTerminate);
  DoLog(imtInfo,'Starting test insight server on port %d',[Port]);
end;

procedure TTestInsightServer.StopServer;
begin

  if not Assigned(FServer) then
    exit;
  if not FServer.Active then
    exit;
  FServer.OnRequest:=Nil;
  FServerActive:=False;
  DoLog(imtInfo,'Deactivating server');
  FServer.Active:=False;
  DoLog(imtInfo,'Fake request');
  Try
    TInetSocket.Create('localhost',FServer.Port,10,Nil).Free;
  except
    on E  : Exception do
     DoLog(imtError,'Fake request resulted in %s: %s',[E.ClassName,E.Message]);
  end;
  DoLog(imtInfo,'Waiting for server thread to stop');
  If Assigned(FThread) then
    FThread.WaitFor;
  DoLog(imtInfo,'Server thread stopped');
end;



end.

