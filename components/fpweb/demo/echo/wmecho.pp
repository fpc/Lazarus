unit wmecho;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, HTTPDefs, websession, fpHTTP,
  fpWeb;

type

  { TEchoModule }

  TEchoModule = class(TFPWebModule)
    procedure EchoModuleRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  EchoModule: TEchoModule;

implementation

uses webutil;

{ TEchoModule }

procedure TEchoModule.EchoModuleRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
  
Var
  S : TStrings;
  
begin
  S:=TStringList.Create;
  try
    // Analyze request.
    DumpRequest(ARequest,S);
    // Optional, because default.
    AResponse.ContentType:='text/html';
    AResponse.Contents:=S;
    Handled:=True;
  finally
    S.Free;
  end;
end;

initialization
  {$I wmecho.lrs}

  RegisterHTTPModule('echo', TEchoModule);
end.

