{
    This file is part of the Free Component Library
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    Open API service - client proxy parent

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit fpopenapiclient;
{$ENDIF FPC_DOTTEDUNITS}

{$mode ObjFPC}
{$H+}
{$IFNDEF VER3_2}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}
{$ENDIF}
{$modeswitch advancedrecords}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, FpWeb.Client;
  {$ELSE}
  Classes, SysUtils, fpwebclient;
  {$ENDIF}

Type
  EOpenAPIClient = class(Exception);
  TServiceRequestID = string;

  TServiceResponse = Record
    RequestID : TServiceRequestID;
    StatusCode : Integer;
    StatusText : String;
    Content : String;
    ContentStream : TStream;
  end;

  { TServiceResult }

  generic TServiceResult<T> = record
    RequestID : TServiceRequestID;
    ErrorCode : Integer;
    ErrorText : String;
    Value : T;
    constructor create(aServiceResponse : TServiceResponse);
    function Success : Boolean;
  end;

  TVoidServiceResult = specialize TServiceResult<Boolean>;

  {$IFNDEF VER3_2}
  TServiceResponseCallback = reference to procedure (aResult: TServiceResponse);
  TVoidServiceResultCallBack = reference to procedure (aResult: TVoidServiceResult);
  {$ELSE}
  TServiceResponseCallback = procedure (aResult: TServiceResponse) of object;
  TVoidServiceResultCallBack = procedure (aResult: TVoidServiceResult) of object;
  {$ENDIF}

  { TFPOpenAPIServiceClient }
  TAPIServicePrepareRequestEvent = procedure(aSender : TObject; aRequest : TWebClientRequest) of object;
  TAPIServiceProcessResponseEvent = procedure(aSender : TObject; aResponse : TWebClientResponse) of object;

  TFPOpenAPIServiceClient = Class(TComponent)
  private
    FBaseURL: String;
    FOnPrepareRequest: TAPIServicePrepareRequestEvent;
    FOnProcessResponse: TAPIServiceProcessResponseEvent;
    FRequestHeaders: TStrings;
    FWebClient: TAbstractWebClient;
    procedure SetBaseURL(AValue: String);
    procedure SetRequestHeaders(const aValue: TStrings);
    procedure SetWebClient(AValue: TAbstractWebClient);
  protected
    function StreamToString(aStream : TStream) : string;
    procedure PrepareRequest(aRequest: TWebClientRequest); virtual;
    procedure ProcessResponse(aResponse: TWebClientResponse); virtual;
    procedure ProcessServiceException(aReq : TWebClientRequest; aError : Exception);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function BuildEndPointURL(const aEndPoint : string) : string; virtual;
    function ReplacePathParam (const aPath : String; const aParamName : string; const aParamValue : String) : String; virtual;
    function ConcatRestParam(const aQueryParam: string; const aParamName: string; const aParamValue: string): string; virtual;
    function ExecuteRequest(const aMethod,aURL,aBody : String; aRequestID : TServiceRequestID = '') : TServiceResponse; virtual;
    function ExecuteRequest(const aMethod,aURL,aBody : String; aResponseBody : TStream; aRequestID : TServiceRequestID = '') : TServiceResponse; virtual;
    {$IFNDEF VER3_2}
    function ExecuteRequest(const aMethod,aURL: String; aBody : TStream; aRequestID : TServiceRequestID = '') : TServiceResponse; virtual;
    function ExecuteRequest(const aMethod,aURL: String; aBody,aResponseBody : TStream; aRequestID : TServiceRequestID = '') : TServiceResponse; virtual;
    function ExecuteRequest(const aMethod,aURL,aBody : String; aCallback : TServiceResponseCallback; aRequestID : TServiceRequestID = '') : TServiceRequestID;virtual;
    {$ENDIF}
  Public
    constructor create(aOwner : TComponent); override;
    destructor destroy; override;
    procedure AddRequestHeader(const aName, aValue: string);
  Published
    Property WebClient : TAbstractWebClient Read FWebClient Write SetWebClient;
    Property BaseURL : String Read FBaseURL Write SetBaseURL;
    Property RequestHeaders : TStrings Read FRequestHeaders Write SetRequestHeaders;
    Property OnPrepareRequest : TAPIServicePrepareRequestEvent Read FOnPrepareRequest Write FOnPrepareRequest;
    Property OnProcessResponse : TAPIServiceProcessResponseEvent Read FOnProcessResponse Write FOnProcessResponse;
  end;

const
  cRestBooleans : Array[Boolean] of string = ('false','true');

function cRestFormatSettings: TFormatSettings;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses FpWeb.Http.Protocol;
{$ELSE}
uses httpprotocol;
{$ENDIF}

var
  _FormatSettings : TFormatSettings;

function cRestFormatSettings: TFormatSettings;

begin
  if _FormatSettings.DateSeparator=#0 then
    _FormatSettings:=DefaultFormatSettings;
  Result:=_FormatSettings;
end;

{ TServiceResult }

constructor TServiceResult.create(aServiceResponse: TServiceResponse);

begin
  Value:=Default(T);
  if (aServiceResponse.StatusCode div 100)<>2 then
    begin
    ErrorCode:=aServiceResponse.StatusCode;
    ErrorText:=aServiceResponse.StatusText;
    end
  else
    begin
    ErrorCode:=0;
    ErrorText:='';
    end;
end;

function TServiceResult.Success: Boolean;

begin
  Result:=ErrorCode=0;
end;

{ TFPOpenAPIClient }

procedure TFPOpenAPIServiceClient.SetBaseURL(AValue: String);

begin
  if FBaseURL=AValue then Exit;
  FBaseURL:=AValue;
end;

procedure TFPOpenAPIServiceClient.SetRequestHeaders(const aValue: TStrings);
begin
  if FRequestHeaders=aValue then Exit;
  FRequestHeaders.Assign(aValue);
end;


procedure TFPOpenAPIServiceClient.SetWebClient(AValue: TAbstractWebClient);

begin
  if FWebClient=AValue then Exit;
  if Assigned(FWebClient) then
    FWebClient.RemoveFreeNotification(Self);
  FWebClient:=AValue;
  if Assigned(FWebClient) then
    FWebClient.FreeNotification(Self);
end;


procedure TFPOpenAPIServiceClient.PrepareRequest(aRequest: TWebClientRequest);
var
  I : integer;
  N,V : String;

begin
  aRequest.Headers.Values['Content-Type']:='application/json';
  aRequest.Headers.Values['Accept']:='application/json';
  For I:=0 to FRequestHeaders.Count-1 do
    begin
    FRequestHeaders.GetNameValue(i,N,V);
    ARequest.Headers.Values[N]:=V;
    end;
  if assigned(OnPrepareRequest) then
    OnPrepareRequest(Self,aRequest);
end;


procedure TFPOpenAPIServiceClient.ProcessResponse(aResponse: TWebClientResponse);

begin
  if Assigned(FOnProcessResponse) then
    FOnProcessResponse(Self,aResponse);
end;


procedure TFPOpenAPIServiceClient.ProcessServiceException(aReq: TWebClientRequest; aError: Exception);

begin
  // Do nothing
end;


procedure TFPOpenAPIServiceClient.Notification(AComponent: TComponent; Operation: TOperation);

begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) then
    if aComponent=FWebClient then
      FWebClient:=Nil;
end;


function TFPOpenAPIServiceClient.BuildEndPointURL(const aEndPoint: string): string;

var
  lEndPoint : String;

begin
  Result:=BaseURL;
  if (Result<>'') and (Result[Length(Result)]<>'/') then
    Result:=Result+'/';
  lEndPoint:=aEndPoint;
  if (aEndPoint<>'') and (aEndPoint[1]='/') then
    lEndPoint:=Copy(lEndPoint,2);
  Result:=Result+lEndPoint;
end;


function TFPOpenAPIServiceClient.ReplacePathParam(const aPath: String; const aParamName: string; const aParamValue: String): String;

var
  lEncoded : String;

begin
  lEncoded:=HTTPEncode(aParamValue);
  Result:=StringReplace(aPath,'{'+aParamName+'}',lEncoded,[rfReplaceAll]);
end;


function TFPOpenAPIServiceClient.ConcatRestParam(const aQueryParam: string; const aParamName: string; const aParamValue: string): string;

begin
  Result := aQueryParam;
  if (aParamValue = '') then
    exit;
  if Result='' then
    Result:=Result+'?'
  else
    Result:=Result+'&';
  Result:=Result+aParamName;
  Result:=Result+'='+HTTPEncode(aParamValue);
end;

function TFPOpenAPIServiceClient.StreamToString(aStream : TStream) : string;
  
begin
  Result:='';
  SetLength(Result,aStream.Size);
  aStream.Position:=0;
  if (aStream.Size>0) then
    aStream.ReadBuffer(Result[1],aStream.Size);
end;



{$IFNDEF VER3_2}
function TFPOpenAPIServiceClient.ExecuteRequest(const aMethod,aURL: String; aBody,aResponseBody : TStream; aRequestID : TServiceRequestID = '') : TServiceResponse; 

var
  lReq : TWebClientRequest;
  lResponse : TWebClientResponse;

begin
  Result:=Default(TServiceResponse);
  if Not Assigned(WebClient) then
    Raise EOpenAPIClient.Create('No webclient assigned');
  try
    lReq:=WebClient.CreateRequest(False,aRequestID);
    LReq.ResponseContent:=aResponseBody;
    Result.RequestID:=lReq.RequestID;
    lReq.Content:=aBody;
    lReq.OwnsStream:=False;
    try
      PrepareRequest(lReq);
      lResponse:=WebClient.ExecuteRequest(aMethod,aURL,lReq);
      ProcessResponse(lResponse);
      Result.StatusCode:=lResponse.StatusCode;
      Result.StatusText:=lResponse.StatusText;
      Result.ContentStream:=aResponseBody;
    except
      on E : Exception do
        begin
        ProcessServiceException(lReq,E);
        Result.StatusCode:=999;
        Result.StatusText:=Format('%s: %s',[E.ClassName,E.Message]);
        end;
    end;
  finally
    lReq.Free;
    lResponse.Free;
  end;
end;

function TFPOpenAPIServiceClient.ExecuteRequest(const aMethod,aURL: String; aBody : TStream; aRequestID : TServiceRequestID = '') : TServiceResponse; 
var
  lResponse : TStream;
begin
  lResponse:=TMemoryStream.Create;
  try
    Result:=ExecuteRequest(aMethod,aURL,aBody,lResponse,aRequestID);
    Result.ContentStream:=Nil;
    Result.Content:=StreamToString(lResponse);
  finally
    lResponse.Free;
  end;  
end;


function TFPOpenAPIServiceClient.ExecuteRequest(const aMethod, aURL, aBody: String; aResponseBody : TStream; aRequestID: TServiceRequestID): TServiceResponse;

var
  lBody : TStringStream;
  
begin
  lBody:=TStringStream.Create(aBody);
  try
    Result:=ExecuteRequest(aMethod,aURL,lBody,aResponseBody,aRequestID);
  finally
    lBody.Free;
  end;  
end;


function TFPOpenAPIServiceClient.ExecuteRequest(const aMethod, aURL, aBody: String; aRequestID: TServiceRequestID): TServiceResponse;

var
  lResponse : TMemoryStream;
begin
  lResponse:=TStringStream.Create(aBody);
  try
    Result:=ExecuteRequest(aMethod,aURL,aBody,lResponse,aRequestID);
    Result.ContentStream:=Nil;
    Result.Content:=StreamToString(lResponse);
  finally
    lResponse.Free;
  end;  
end;

function TFPOpenAPIServiceClient.ExecuteRequest(const aMethod,aURL,aBody : String; aCallback : TServiceResponseCallback; aRequestID : TServiceRequestID = '') : TServiceRequestID;

var
  lReq : TWebClientRequest;
  lResponse : TWebClientResponse;
  lExResponse : TServiceResponse;

begin
  Result:=aRequestID;
  if Not Assigned(WebClient) then
    Raise EOpenAPIClient.Create('No webclient assigned');
  try
    lReq:=WebClient.CreateRequest(True,aRequestID);
    Result:=lReq.RequestID;
    if aBody<>'' then
      lReq.SetContentFromString(aBody);
    try
      PrepareRequest(lReq);
      WebClient.ExecuteRequest(aMethod,aURL,lReq,procedure(aResponse : TWebClientResponseResult)
         var
           aResult : TServiceResponse;
         begin
           if not aResponse.Success then
             begin
             ProcessServiceException(lReq,aResponse.Error);
             With aResponse.Error do
               begin
               aResult.StatusText:=Format('%s : %s',[ClassName,Message]);
               aResult.StatusCode:=999;
               aResult.Content:='';
               end
             end
           else
             begin
             ProcessResponse(aResponse.Response);
             aResult.StatusCode:=aResponse.Response.StatusCode;
             aResult.StatusText:=aResponse.Response.StatusText;
             aResult.Content:=aResponse.Response.GetContentAsString;
             end;
           aCallBack(aResult);
         end);
      lReq:=Nil;
      lResponse:=Nil;
    except
      on E : Exception do
        begin
        ProcessServiceException(lReq,E);
        lExResponse.RequestID:=lReq.RequestID;
        lExResponse.StatusCode:=999;
        lExResponse.StatusText:=Format('%s: %s',[E.ClassName,E.Message]);
        aCallBack(lExResponse);
        end;
    end;
  finally
    lReq.Free;
    lResponse.Free;
  end;
end;

{$ELSE}

function TFPOpenAPIServiceClient.ExecuteRequest(const aMethod,aURL,aBody : String; aResponseBody : TStream; aRequestID : TServiceRequestID = '') : TServiceResponse; 


var
  I : Integer;
  N,V : String;
  lReq : TWebClientRequest;
  lResponse : TWebClientResponse;

begin
  Result:=Default(TServiceResponse);
  if Not Assigned(WebClient) then
    Raise EOpenAPIClient.Create('No webclient assigned');
  try
    Result.RequestID:=aRequestID;
    lReq:=WebClient.CreateRequest;
    lReq.ResponseContent:=aResponseBody;
    if aBody<>'' then
      lReq.SetContentFromString(aBody);
    try
      PrepareRequest(lReq);
      lResponse:=WebClient.ExecuteRequest(aMethod,aURL,lReq);
      ProcessResponse(lResponse);
      Result.StatusCode:=lResponse.StatusCode;
      Result.StatusText:=lResponse.StatusText;
      Result.ContentStream:=lResponse.Content;
    except
      on E : Exception do
        begin
        ProcessServiceException(lReq,E);
        Result.StatusCode:=999;
        Result.StatusText:=Format('%s: %s',[E.ClassName,E.Message]);
        end;
    end;
  finally
    lReq.Free;
    lResponse.Free;
  end;
end;

function TFPOpenAPIServiceClient.ExecuteRequest(const aMethod, aURL, aBody: String; aRequestID: TServiceRequestID): TServiceResponse;

var
  lResponse : TStream;
begin
  lResponse:=TMemoryStream.Create;
  try
    Result:=ExecuteRequest(aMethod,aURL,aBody,lResponse,aRequestID);
    Result.ContentStream:=Nil;
    SetLength(Result.Content,lResponse.Size);
    lResponse.Position:=0;
    if lResponse.Size>0 then
      lResponse.ReadBuffer(Result.Content[1],lResponse.Size);
  finally
    lResponse.Free;
  end;
end;  
{$ENDIF}

constructor TFPOpenAPIServiceClient.create(aOwner: TComponent);
begin
  inherited create(aOwner);
  FRequestHeaders:=TStringList.Create;
  FRequestHeaders.NameValueSeparator:=':';
end;

destructor TFPOpenAPIServiceClient.destroy;
begin
  FreeAndNil(FRequestHeaders);
  inherited destroy;
end;

procedure TFPOpenAPIServiceClient.AddRequestHeader(const aName, aValue : string);
begin
  RequestHeaders.Values[aName]:=aValue;
end;



end.

