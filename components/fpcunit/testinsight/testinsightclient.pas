unit TestInsightClient;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, types, fphttpclient, fpjson, contnrs, inifiles,
  TestInsightProtocol;

Type

  { TAbstractTestInsightClient }

  TAbstractTestInsightClient = class (TObject)
  private
    FOptions : TTestInsightOptions;
    FPendingResultCount: Integer;
    FResults : TFPObjectList;
    FBaseURL : String;
    procedure SetOptions(const value: TTestInsightOptions);
  protected
    function GetHasError: Boolean; virtual; abstract;
    function GetLastErrorMessage: String; virtual; abstract;
    function JSONToTests(const aJSON: string): TStringDynArray;
    // URL is Relative to base URL
    procedure ServerPost(aURL: string; const aContent: string = ''); virtual; abstract;
    Function ServerGet(aURL: string) : string; virtual; abstract;
    Procedure ServerDelete(aURL: string) ; virtual; abstract;
    Function ConcatURL(aURL : String) : String; virtual;
  public
    constructor Create(const aBaseURL : String); virtual;
    destructor Destroy; override;
    procedure LoadConfig(const aConfigFileName : String; aSection : String = '');
    procedure LoadConfig(aIni : TCustomIniFile; aSection : String);
    procedure GetServerOptions;
    // The client will free the result.
    procedure PostResult(const testResult: TTestInsightResult; forceSend: Boolean);
    // The client will free the results.
    procedure PostResults(const testResults: array of TTestInsightResult; forceSend: Boolean);
    procedure StartedTesting(const totalCount: Integer);
    procedure FinishedTesting;
    procedure ClearTests;
    Procedure SetTestNames(aJSON : TJSONObject);
    function GetTests: TStringDynArray;
    property Options: TTestInsightOptions read FOptions write SetOptions;
    Property HasError : Boolean Read GetHasError;
    Property LastErrorMessage : String Read GetLastErrorMessage;
    Property BaseURL : String Read FBaseURL;
    Property PendingResultCount : Integer Read FPendingResultCount;
  end;

  { TTestInsightHTTPClient }

  TTestInsightHTTPClient = class(TAbstractTestInsightClient)
  private
    FHttp: TFPHTTPClient;
    FLastError : String;
    FInError : Boolean;
    Procedure SetError(E : Exception);
  protected
    function GetHasError: Boolean; override;
    function GetLastErrorMessage: string; override;
    procedure ServerPost(aURL: string; const aContent: string = ''); override;
    Function ServerGet(aURL: string) : string; override;
    Procedure ServerDelete(aURL: string); override;
  public
    Constructor Create(Const aBaseURL : String); override;
    Destructor Destroy; override;
  end;


implementation

{ TTestInsightHTTPClient }

procedure TTestInsightHTTPClient.ServerPost(aURL: string; const aContent: string = '');

Var
  Body,Res : TStringStream;

begin
  Body:=Nil;
  Res:=TStringStream.Create('');
  try
    // Writeln('Sending content to; ',ConcatURL(aURL),':');
    // Writeln(aContent);
    if aContent<>'' then
      Body:=TStringStream.Create(aContent);
    FHTTP.RequestBody:=Body;
    try
      FHTTP.AddHeader('Content-Type','application/json');
      FHTTP.Post(ConcatURL(aURL),Res);
      SetError(Nil);
    except
      on E : Exception do
        SetError(E);
    end;
  finally
    FHTTP.RequestBody:=Nil;
    Res.Free;
    Body.Free;
  end;
end;

function TTestInsightHTTPClient.ServerGet(aURL: string): string;
Var
  Res : TStringStream;

begin
  Res:=TStringStream.Create('');
  try
    try
      // Writeln('Get URL ',ConcatURL(aURL),':');
      FHTTP.Get(ConcatURL(aURL),Res);
      Result:=Res.DataString;
      // Writeln(Result);
      SetError(Nil);
    except
      on E : Exception do
        SetError(E);
    end;
  finally
    Res.Free;
  end;
end;

procedure TTestInsightHTTPClient.ServerDelete(aURL: string);
begin
  try
    FHTTP.Delete(ConcatURL(aURL));
    SetError(Nil);
  except
    on E : Exception do
      SetError(E);
  end;
end;

constructor TTestInsightHTTPClient.Create(const aBaseURL: String);
begin
  inherited Create(aBaseURL);
  FHTTP:=TFPHTTPClient.Create(Nil);
//  FHTTP.ConnectTimeout:=100;
//  FHTTP.IOTimeout:=1000;
end;

destructor TTestInsightHTTPClient.Destroy;
begin
  FreeAndNil(FHTTP);
  inherited Destroy;
end;

procedure TTestInsightHTTPClient.SetError(E: Exception);
begin
  FInError:=Assigned(E);
  if Assigned(E) then
    FLastError:=E.ClassName+': '+E.Message
  else
    FLastError:='';
end;

function TTestInsightHTTPClient.GetHasError: Boolean;
begin
  Result:=FInError;
end;

function TTestInsightHTTPClient.GetLastErrorMessage: string;
begin
  Result:=FLastError;
end;

{ TAbstractTestInsightClient }

procedure TAbstractTestInsightClient.SetOptions(const value: TTestInsightOptions
  );
begin
  FOptions.Assign(Value);
end;


function TAbstractTestInsightClient.JSONToTests(const aJSON: string): TStringDynArray;

Var
  D : TJSONData;
  A : TJSONArray;
  I : Integer;

begin
  Result:=[];
  D:=GetJSON(aJSON);
  try
    if D=Nil then exit;
    if D.JSONType=jtArray then
      A:=D as TJSONArray
    else if (D.Count=1) and (D.Items[0].JSONType=jtArray) then
      A:=D.Items[0] as TJSONArray
    else
      A:=nil;
    if A<>Nil then
      begin
      SetLength(Result,a.Count);
      For I:=0 to Length(Result)-1 do
        Result[i]:=A.Strings[i];
      end;
  finally
    D.Free;
  end;
end;

function TAbstractTestInsightClient.ConcatURL(aURL: String): String;
begin
  Result:=fBaseURL;
  if (Result<>'') and (aURL<>'') and (Result[Length(Result)]<>'/') then
    Result:=Result+'/';
  Result:=Result+aURL;
end;

constructor TAbstractTestInsightClient.Create(const aBaseURL: String);
begin
  FBaseURL:=aBaseURL;
  FOptions:=TTestInsightOptions.Create;
  FResults:=TFPObjectList.Create(False);
end;

destructor TAbstractTestInsightClient.Destroy;
begin
  FResults.Clear;
  FreeAndNil(FResults);
  FreeAndNil(FOptions);
  inherited Destroy;
end;

procedure TAbstractTestInsightClient.LoadConfig(const aConfigFileName: String; aSection : String = '');

Var
  aIni: TCustomIniFile;

begin
  aIni:=TMemIniFile.Create(aConfigFileName);
  try
    if aSection='' then
      aSection:=SConfig;
    LoadConfig(aIni,aSection);
  finally
    aIni.Free;
  end;
end;

procedure TAbstractTestInsightClient.LoadConfig(aIni: TCustomIniFile;aSection : String);
begin
  FBaseURL:=aIni.ReadString(aSection,KeyBaseURL,BaseURL);
  Options.ShowProgress:=aIni.ReadBool(aSection,keyShowProgress,Self.Options.ShowProgress);
  Options.ExecuteTests:=aIni.ReadBool(aSection,keyExecuteTests,Self.Options.ExecuteTests);
  Options.TestSuite:=aIni.ReadString(aSection,keySuite,Self.Options.TestSuite);
end;

procedure TAbstractTestInsightClient.GetServerOptions;

begin
  Options.FromJSON(ServerGet(pathOptions))
end;

procedure TAbstractTestInsightClient.PostResult(
  const testResult: TTestInsightResult; forceSend: Boolean);
begin
  PostResults([testResult],forceSend);
end;

procedure TAbstractTestInsightClient.PostResults(
  const testResults: array of TTestInsightResult; forceSend: Boolean);

Var
  Res : TTestInsightResult;
  J : TJSONArray;
  O : TJSONOBject;


begin
  if ForceSend or (Options.ShowProgress and Options.ExecuteTests) then
    begin
    J:=TJSONArray.Create;
    try
      For Res in testResults do
        begin
        O:=TJSONObject.Create;
        J.Add(O);
        Res.ToJSON(O);
        Res.Free;
        end;
      ServerPost(pathResults,J.AsJSON);
    finally
      J.Free;
    end;
    end
  else
    For Res in TestResults do
      FResults.Add(res);
end;

procedure TAbstractTestInsightClient.StartedTesting(const totalCount: Integer);
begin
  ServerPost(Format('%s?%s=%d', [pathStarted,qryTotalCount,Totalcount]),'');
end;

procedure TAbstractTestInsightClient.FinishedTesting;

Var
  A : Array of TTestInsightResult;
  Len,I : Integer;

begin
  A:=[];
  Len:=FResults.Count;
  if (Len>0) then
    begin
    Setlength(A,Len);
    For I:=0 to Len-1 do
      A[I]:=TTestInsightResult(FResults[i]);
    try
      PostResults(A,True);
    finally
      FResults.Clear;
    end;
    end;
  ServerPost(pathFinished,'');
end;

procedure TAbstractTestInsightClient.ClearTests;
begin
  ServerDelete(pathResults);
end;

procedure TAbstractTestInsightClient.SetTestNames(aJSON : TJSONObject);

begin
  ServerPost('',aJSON.AsJSON);
end;

function TAbstractTestInsightClient.GetTests: TStringDynArray;
begin
  Result:=JSONToTests(ServerGet(''));
end;

end.

