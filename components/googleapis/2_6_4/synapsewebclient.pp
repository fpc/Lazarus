unit synapsewebclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpwebclient, httpsend;

Type

  { TSynapseRequest }

  TSynapseRequest = Class(TWebClientRequest)
  Private
    FHTTP : THTTPSend;
  Protected
    function GetHeaders: TStrings;override;
    function GetStream: TStream;override;
  Public
    Constructor Create(AHTTP : THTTPSend);
    Destructor Destroy; override;
  end;

  { TSynapseResponse }

  TSynapseResponse = Class(TWebClientResponse)
  Private
    FHTTP : THTTPSend;
  Protected
    function GetHeaders: TStrings;override;
    function GetStream: TStream;override;
    Function GetStatusCode : Integer; override;
    Function GetStatusText : String; override;
  Public
    Constructor Create(ARequest : TWebClientRequest);  override;
    Destructor Destroy; override;
  end;

  { TSynapseWebClient }

  TSynapseWebClient = Class(TAbstractWebClient)
  Protected
    Function DoCreateRequest: TWebClientRequest; override;
    Function DoHTTPMethod(Const AMethod,AURL : String; ARequest : TWebClientRequest) : TWebClientResponse; override;
  end;

implementation

{ TSynapseRequest }

function TSynapseRequest.GetHeaders: TStrings;
begin
  if Assigned(FHTTP) then
    Result:=FHTTP.Headers
  else
    Result:=Inherited GetHeaders;
end;

function TSynapseRequest.GetStream: TStream;
begin
  if Assigned(FHTTP) then
    Result:=FHTTP.Document
  else
    Result:=Inherited GetStream;
end;

Constructor TSynapseRequest.Create(AHTTP: THTTPSend);
begin
  FHTTP:=AHTTP;
end;

Destructor TSynapseRequest.Destroy;
begin
  FreeAndNil(FHTTP);
  inherited Destroy;
end;

{ TSynapseResponse }

function TSynapseResponse.GetHeaders: TStrings;
begin
  if Assigned(FHTTP) then
    Result:=FHTTP.Headers
  else
    Result:=Inherited GetHeaders;
end;

function TSynapseResponse.GetStream: TStream;
begin
  if Assigned(FHTTP) then
    Result:=FHTTP.Document
  else
    Result:=Inherited GetStream;
end;

Function TSynapseResponse.GetStatusCode: Integer;
begin
  if Assigned(FHTTP) then
    Result:=FHTTP.ResultCode
  else
    Result:=0;
end;

Function TSynapseResponse.GetStatusText: String;
begin
  if Assigned(FHTTP) then
    Result:=FHTTP.ResultString
  else
    Result:='';
end;

Constructor TSynapseResponse.Create(ARequest : TWebClientRequest);
begin
  Inherited Create(ARequest);
  FHTTP:=(ARequest as TSynapseRequest).FHTTP;
end;

Destructor TSynapseResponse.Destroy;
begin
  FreeAndNil(FHTTP);
  inherited Destroy;
end;

{ TSynapseWebClient }

Function TSynapseWebClient.DoCreateRequest: TWebClientRequest;
begin
  Result:=TSynapseRequest.Create(THTTPSend.Create);
end;

Function TSynapseWebClient.DoHTTPMethod(Const AMethod, AURL: String;
  ARequest: TWebClientRequest): TWebClientResponse;

Var
  U,S : String;
  I : Integer;
  h : THTTPSend;

begin
  U:=AURL;
  H:=TSynapseRequest(ARequest).FHTTP;
  S:=ARequest.ParamsAsQuery;
  if (S<>'') then
    begin
    if Pos('?',U)=0 then
      U:=U+'?';
    U:=U+S;
    end;
  I:=H.Headers.IndexOfName('Content-type');
  if I<>-1 then
    begin
    H.MimeType:=H.Headers.Values['Content-type'];
    H.Headers.Delete(I);
    end;
  if Not H.HTTPMethod(AMethod,U) then
    begin
    H.Document.Clear;
    H.Headers.Clear;
    H.Cookies.Clear;
    With H.Sock do
      Raise EFPWebClient.CreateFmt('HTTP Request failed (%d : %s)',[LastError,LastErrorDesc]);
    end
  else
    begin
    Result:=TSynapseResponse.Create(ARequest);
    if Assigned(ARequest.ResponseContent) then
      ARequest.ResponseContent.CopyFrom(ARequest.Content,0);
    TSynapseRequest(ARequest).FHTTP:=Nil;
    end;
end;

end.

