{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2024 by Michael Van Canneyt

    ASync HTTP request execution

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpasyncwebclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpwebclient;

type
  TAsyncRequestContext = record
    Method,URL : String;
    Request : TWebClientRequest;
    Client : TAbstractWebClient;
    UserData : TObject;
  end;

  TWebResponseEvent = procedure (aResponse : TWebClientResponse; aUserData : TObject) of object;
  TWebRequestErrorEvent = procedure (aException : Exception; aContext : TAsyncRequestContext) of object;

  { TAsyncRequestData }

  TAsyncRequestData = class
    FContext : TAsyncRequestContext;
    OnResponse : TWebResponseEvent;
    OnError : TWebRequestErrorEvent;
    constructor create(const aContext : TAsyncRequestContext);
    destructor destroy; override;
    procedure ExecuteRequest;
  end;

  { THTTPRequestThread }

  THTTPRequestThread = class(TThread)
  Private
    FData : TAsyncRequestData;
  protected
    Procedure Execute; override;
  public
    // Thread will destroy aData and itself.
    constructor create(aData : TAsyncRequestData); reintroduce;
  end;


implementation


{ THTTPRequestThread }

constructor THTTPRequestThread.Create(aData : TAsyncRequestData);

begin
  FData:=AData;
  FreeOnTerminate:=True;
  Inherited Create(False);
end;

procedure THTTPRequestThread.Execute;

begin
  try
    FData.ExecuteRequest;
  finally
    FData.Free;
  end;
end;


{ TAsyncRequestData }

constructor TAsyncRequestData.create(const aContext: TAsyncRequestContext);
begin
  FContext:=aContext;
end;

destructor TAsyncRequestData.destroy;
begin
  FContext.Request.Free;
end;

procedure TAsyncRequestData.ExecuteRequest;

var
  Res : TWebClientResponse;

begin
  With FContext do
    try
      begin
      Res:=Client.ExecuteRequest(Method,Url,Request);
      if Assigned(OnResponse) then
        OnResponse(Res,UserData);
      end;
    except
      On E : Exception do
        If Assigned(OnError) then
          OnError(E,FContext);
    end;
end;


end.

