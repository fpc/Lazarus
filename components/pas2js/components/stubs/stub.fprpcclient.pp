unit stub.fprpcclient;

{$mode ObjFPC}
{$modeswitch advancedrecords}
{$WARN 5024 off : Parameter "$1" not used}
interface

uses
  Classes, SysUtils, stub.JS;

Const
  DefaultJSONRPCversion = '2.0';

Type
  ERPCClient = Class(Exception);

  { TRPCRequestParamsBuilder }

  TRPCRequestParamsBuilder = class
  Protected
    Procedure DoAddArg(const aName : String; aValue : JSValue); virtual; abstract;
    Function DoGetArgs : JSValue; virtual; abstract;
  Public
    Procedure AddArg(const aName : string; aValue : NativeInt);
    Procedure AddArg(const aName : string; aValue : String);
    Procedure AddArg(const aName : string; aValue : Boolean);
    Procedure AddArg(const aName : string; aValue : Double);
    Procedure AddArg(const aName : string; aValue : TJSArray);
    Procedure AddArg(const aName : string; aValue : TJSObject);
  end;

  { TRPCArrayRequestParamsBuilder }

  TRPCArrayRequestParamsBuilder = class (TRPCRequestParamsBuilder)
  private
    FParams: TJSArray;
  Protected
    Function DoGetArgs : JSValue; override;
    Procedure DoAddArg(const aName : String; aValue : JSValue); override;
  Public
    Constructor Create(aParams : TJSArray);
    Property Params : TJSArray Read FParams;
  end;

  { TRPCObjectRequestParamsBuilder }

  TRPCObjectRequestParamsBuilder = class (TRPCRequestParamsBuilder)
  private
    FParams: TJSObject;
  Protected
    Procedure DoAddArg(const aName : String; aValue : JSValue); override;
    Function DoGetArgs : JSValue; override;
  Public
    Constructor Create(aParams : TJSObject);
    Property Params : TJSObject Read FParams;
  end;

  { TRPCError }

  TRPCError = record
    ID : NativeInt;
    Code : NativeInt;
    Message : String;
    ErrorClass : String;
    Procedure FromValue(Err : JSValue);
  end;

  { TRPCResponse }

  TRPCResponse = Record
    isOK : Boolean;
    ID : NativeInt;
    Error : TRPCError;
    HasError : Boolean;
    Result : JSValue;
    Version : String;
    Procedure FromObject(Obj : TJSObject);
  end;

  TRPCFailureCallBack = Procedure (Sender : TObject; const aError : TRPCError);
  TRPCResultCallBack = Procedure (Sender : TObject; const aResult : JSValue);
  TRPCUnexpectedErrorCallback = Procedure (Sender : TObject; Const aStage : String; E : Exception) of object;

  TRPCOption = (roParamsAsObject,roFullMethodName,roUseBatch,roAutoBatch,roForceArray);
  TRPCOptions = Set of TRPCOption;

  TRPCRequest = Record
    IsNotification : Boolean;
    ClassName : String;
    MethodName : String;
    ID : NativeInt;
    Params : JSValue;
    OnFailure : TRPCFailureCallBack;
    OnSuccess : TRPCResultCallBack;
  end;

  { TRPCBatch }

  TRPCBatch = Record
    Requests : Array of TRPCRequest;
    ID : NativeInt;
    Function GetRequest(aID : NativeInt; DoRemove : Boolean) : TRPCRequest;
  end;

  TRPCConfigRequest = procedure (sender : TObject; aConfig : TJSObject) of object;
  TRPCHeadersRequest = procedure (sender : TObject; aHeaders : TStrings) of object;

  { TRPCClient }
  TRPCClient = Class(TComponent)
  private
    FBatchTimeout: Integer;
    FCustomHeaders: TStrings;
    FJSONRPCversion: String;
    FOnConfigRequest: TRPCConfigRequest;
    FOnCustomHeaders: TRPCHeadersRequest;
    FOnUnexpectedError: TRPCUnexpectedErrorCallback;
    FOptions: TRPCoptions;
    FPendingBatches : TJSObject;
    FURL: String;
    procedure SetCustomHeaders(AValue: TStrings);
    procedure SetOptions(AValue: TRPCoptions);
    procedure SetURL(AValue: String);
  Protected
    // For use in service
    Function DoExecuteRequest(const aClassName,aMethodName : String; aParams : JSValue; aOnSuccess : TRPCResultCallBack = Nil; aOnFailure: TRPCFailureCallBack = nil) : NativeInt;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    // you are responsible for freeing the request params builder.
    Function CreateRequestParamsBuilder : TRPCRequestParamsBuilder;
    // Execute a request. Params can be passed as object or array
    Function ExecuteRequest(const aClassName,aMethodName : String; aParams : TJSArray; aOnSuccess : TRPCResultCallBack = Nil; aOnFailure: TRPCFailureCallBack = nil) : NativeInt;
    Function ExecuteRequest(const aClassName,aMethodName : String; aParams : TJSObject; aOnSuccess : TRPCResultCallBack = Nil; aOnFailure: TRPCFailureCallBack = nil) : NativeInt;
    // Close current batch
    Procedure CloseBatch;
  Published
    // URL for RPC server.
    Property URL : String Read FURL Write SetURL;
    // Options.
    Property Options : TRPCoptions Read FOptions Write SetOptions;
    // If roAutoBatch is in options, this is the timeout between first request and the time the batch is created and sent. Default 100 ms.
    Property BatchTimeout: Integer Read FBatchTimeout Write FBatchTimeout;
    // JSON RPC version to send, default: 2.0
    Property JSONRPCversion : String Read FJSONRPCversion Write FJSONRPCversion;
    // Custom headers to be sent with each request. NameValueSeparator is colon (:) so add Name:value
    Property CustomHeaders : TStrings Read FCustomHeaders Write SetCustomHeaders;
    // Called when configuring a FETCH request
    Property OnConfigRequest : TRPCConfigRequest Read FOnConfigRequest Write FOnConfigRequest;
    // Called when collecting headers for a request.
    Property OnCustomHeaders : TRPCHeadersRequest Read FOnCustomHeaders Write FOnCustomHeaders;
    // Called when an unexpected error occurs during success/failure callbacks
    Property OnUnexpectedError : TRPCUnexpectedErrorCallback Read FOnUnexpectedError Write FOnUnexpectedError;
  end;

  TPas2jsRPCClient = class(TRPCClient);

  { TRPCCustomService }

  // Result callback types for all supported types
  TEmptyResultHandler = procedure;
  TBooleanResultHandler = procedure (aResult : Boolean);
  TNativeIntResultHandler = procedure (aResult : NativeInt);
  TDoubleResultHandler = procedure (aResult : Double);
  TStringResultHandler = procedure (aResult : String);
  TArrayResultHandler = procedure (aResult : TJSArray);
  TObjectResultHandler = procedure (aResult : TJSObject);
  TJSValueResultHandler = procedure (aResult : JSValue);

  TRPCCustomService = class(TComponent)
  private
    FClient: TRPCClient;
    FParamBuilder: TRPCRequestParamsBuilder;
    procedure SetClient(AValue: TRPCClient);
  protected
    Procedure AddParam(const aName : string; aValue : NativeInt);
    Procedure AddParam(const aName : string; aValue : String);
    Procedure AddParam(const aName : string; aValue : Boolean);
    Procedure AddParam(const aName : string; aValue : Double);
    Procedure AddParam(const aName : string; aValue : TJSArray);
    Procedure AddParam(const aName : string; aValue : TJSObject);
    Procedure StartParams;
    Function EndParams : JSValue;
    Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    Function RPCClassName : String ; virtual;
    Function ExecuteRequest(const aClassName,aMethodName : String; aParams : JSValue; aOnSuccess : TRPCResultCallBack = Nil; aOnFailure: TRPCFailureCallBack = nil) : NativeInt;
    Property ParamBuilder : TRPCRequestParamsBuilder Read FParamBuilder;
  Published
    Property RPCClient : TRPCClient Read FClient Write SetClient;
  end;

implementation

uses stub.web;

{ TRPCCustomService }

procedure TRPCCustomService.SetClient(AValue: TRPCClient);
begin
  if FClient=AValue then Exit;
  if Assigned(FClient) then
    FClient.RemoveFreeNotification(Self);
  FClient:=AValue;
  if Assigned(FClient) then
    FClient.FreeNotification(Self);
end;

procedure TRPCCustomService.AddParam(const aName: string; aValue: NativeInt);
begin
  ParamBuilder.AddArg(aName,aValue);
end;

procedure TRPCCustomService.AddParam(const aName: string; aValue: String);
begin
  ParamBuilder.AddArg(aName,aValue);
end;

procedure TRPCCustomService.AddParam(const aName: string; aValue: Boolean);
begin
  ParamBuilder.AddArg(aName,aValue);
end;

procedure TRPCCustomService.AddParam(const aName: string; aValue: Double);
begin
  ParamBuilder.AddArg(aName,aValue);
end;

procedure TRPCCustomService.AddParam(const aName: string; aValue: TJSArray);
begin
  ParamBuilder.AddArg(aName,aValue);
end;

procedure TRPCCustomService.AddParam(const aName: string; aValue: TJSObject);
begin
  ParamBuilder.AddArg(aName,aValue);
end;

procedure TRPCCustomService.StartParams;
begin
  if Assigned(FParamBuilder) then
    Raise ERPCClient.Create('Parameter building already in progress');
  if Not Assigned(RPCClient) then
    Raise ERPCClient.Create('Parameter building cannot be started without RPCClient');
  FParamBuilder:=RPCClient.CreateRequestParamsBuilder;
end;

function TRPCCustomService.EndParams: JSValue;
begin
  if not Assigned(FParamBuilder) then
    Raise ERPCClient.Create('No parameter builder was started. Call StartParams first');
  Result:=ParamBuilder.DoGetArgs;
  FreeAndNil(FParamBuilder);
end;

procedure TRPCCustomService.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then
    if AComponent=FClient then
      FClient:=Nil;
end;

function TRPCCustomService.RPCClassName: String;
begin
  Result:='';
end;

function TRPCCustomService.ExecuteRequest(const aClassName,
  aMethodName: String; aParams: JSValue; aOnSuccess: TRPCResultCallBack;
  aOnFailure: TRPCFailureCallBack): NativeInt;
begin
  if Not Assigned(RPCClient) then
    Raise ERPCClient.Create('ExecuteRequest cannot be called without RPCClient');
  Result:=RPCClient.DoExecuteRequest(aClassName,aMethodName,aParams,aOnSuccess,aOnFailure);
end;

{ TRPCBatch }

function TRPCBatch.GetRequest(aID: NativeInt; DoRemove: Boolean): TRPCRequest;

Var
  Len,Idx : Integer;

begin
  Idx:=0;
  Len:=Length(Requests);
  While (Idx<Len) and (Requests[Idx].ID<>aID) do
    Inc(Idx);
  if (Idx<Len) then
    begin
    Result:=Requests[Idx];
    if DoRemove then
      Delete(Requests,Idx,1);
    end
  else
    Result:=Default(TRPCRequest);
end;

{ TRPCError }

procedure TRPCError.FromValue(Err: JSValue);

begin
end;

{ TRPCResponse }

procedure TRPCResponse.FromObject(Obj: TJSObject);


begin
end;

{ TRPCRequestParamsBuilder }


procedure TRPCRequestParamsBuilder.AddArg(const aName: string; aValue: NativeInt);
begin

end;

procedure TRPCRequestParamsBuilder.AddArg(const aName: string; aValue: String);
begin

end;

procedure TRPCRequestParamsBuilder.AddArg(const aName: string; aValue: Boolean);
begin

end;

procedure TRPCRequestParamsBuilder.AddArg(const aName: string; aValue: Double);
begin

end;

procedure TRPCRequestParamsBuilder.AddArg(const aName: string; aValue: TJSArray);
begin

end;

procedure TRPCRequestParamsBuilder.AddArg(const aName: string; aValue: TJSObject);
begin

end;

{ TRPCObjectRequestParamsBuilder }

procedure TRPCObjectRequestParamsBuilder.DoAddArg(const aName: String; aValue: JSValue
  );
begin

end;

function TRPCObjectRequestParamsBuilder.DoGetArgs: JSValue;
begin
  Result:=Null;
end;

constructor TRPCObjectRequestParamsBuilder.Create(aParams: TJSObject);
begin

end;

{ TRPCArrayRequestParamsBuilder }

function TRPCArrayRequestParamsBuilder.DoGetArgs: JSValue;
begin
  Result:=Null;
end;

procedure TRPCArrayRequestParamsBuilder.DoAddArg(const aName: String; aValue: JSValue
  );
begin
end;

constructor TRPCArrayRequestParamsBuilder.Create(aParams: TJSArray);
begin

end;

{ TRPCClient }

procedure TRPCClient.SetOptions(AValue: TRPCoptions);

begin
  if FOptions=AValue then Exit;
  FOptions:=AValue;
end;

procedure TRPCClient.SetURL(AValue: String);
begin
  if FURL=AValue then Exit;
  FURL:=AValue;
end;


function TRPCClient.DoExecuteRequest(const aClassName, aMethodName: String;
  aParams: JSValue; aOnSuccess: TRPCResultCallBack;
  aOnFailure: TRPCFailureCallBack): NativeInt;
begin
  Result:=0;
end;


procedure TRPCClient.SetCustomHeaders(AValue: TStrings);
begin
  if FCustomHeaders=AValue then Exit;
  FCustomHeaders.Assign(AValue);
end;



constructor TRPCClient.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FBatchTimeOut:=100;
  JSONRPCVersion:=DefaultJSONRPCversion;
  FCustomHeaders:=TStringList.Create;
  FCustomHeaders.NameValueSeparator:=':';
end;

destructor TRPCClient.Destroy;
begin
  FreeAndNil(FCustomHeaders);
  FPendingBatches:=Nil;
  inherited Destroy;
end;

function TRPCClient.CreateRequestParamsBuilder: TRPCRequestParamsBuilder;
begin
  Result:=Nil;
end;

function TRPCClient.ExecuteRequest(const aClassName, aMethodName: String;
  aParams: TJSArray; aOnSuccess: TRPCResultCallBack; aOnFailure: TRPCFailureCallBack): NativeInt;

begin
  Result:=-1;
end;

function TRPCClient.ExecuteRequest(const aClassName, aMethodName: String;
  aParams: TJSObject; aOnSuccess: TRPCResultCallBack;
  aOnFailure: TRPCFailureCallBack): NativeInt;

begin
  Result:=-1;
end;

procedure TRPCClient.CloseBatch;
begin
  //
end;

end.

