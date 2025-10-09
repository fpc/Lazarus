unit llm.ollama;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, llm.client, fpopenapiclient, ollama.Service.Impl;

Type
  
  { TOLLamaProtocol }

  TOLLamaProtocol = class(TLLMProtocol)
  protected
    procedure ConfigProxy(aProxy : TFPOpenAPIServiceClient);
  public
    function GetModels: TGetModelsResult; override;
    function SendPrompt(aPrompt: TPromptArray; aMaxLen: Integer): TSendPromptResult; override;
    class function ProtocolName : string; override;
    class function DefaultURL : String; override;
    class function DefaultAPIKeyVariable : String; override;
  end;

implementation

uses
  ollama.Service.Intf,
  ollama.Dto;

{ TOLLamaProtocol }

procedure TOLLamaProtocol.ConfigProxy(aProxy : TFPOpenAPIServiceClient);

begin
  aProxy.WebClient:=ResolveWebClient;
  aProxy.BaseURL:=ResolveBaseURL;
end;

function TOLLamaProtocol.GetModels: TGetModelsResult;
var
  lModelService: TModelServiceProxy;
  lModelsResponse: Tapi_ListResponseServiceResult;
  lModels : TModelDataArray;
  i : Integer;
begin
  Result:=Default(TGetModelsResult);
  lModels:=[];
  lModelsResponse:=Default(Tapi_ListResponseServiceResult);
  lModelService:=TModelServiceProxy.Create(Nil);
  try
    ConfigProxy(lModelService);
    // Exceptions will be handled by caller.
    lModelsResponse:=lModelService.List();
    SetLength(lModels,Length(lModelsResponse.Value.models));
    for I:=0 to Length(lModels)-1 do
      begin
      lModels[i].ID:=lModelsResponse.Value.models[i].model;
      lModels[i].Name:=lModelsResponse.Value.models[i].name;
      end;
    Result.SetOKValue(lModels);
  finally
    lModelsResponse.Value.Free;
    lModelService.Free;
  end;
end;

function TOLLamaProtocol.SendPrompt(aPrompt: TPromptArray; aMaxLen: Integer): TSendPromptResult;
var
  lChatService: TChatServiceProxy;
  lRequest: Tapi_ChatRequest;
  i : Integer;
  lResponses : TPromptResponseArray;
  lResult: Tapi_ChatResponseServiceResult;
  lMessage: Tapi_Message;
begin
  Result:=Default(TSendPromptResult);
  lResponses:=[];
  lRequest:=nil;
  lResult:=Default(Tapi_ChatResponseServiceResult);
  lChatService:=TChatServiceProxy.Create(Nil);
  try
    ConfigProxy(lChatService);
    lRequest:=Tapi_ChatRequest.Create;
    lRequest.model := ResolveModel;
    lRequest.keep_alive:=300;
    lRequest.think:=false;
    lRequest.stream := False;  // Non-streaming response
    SetLength(lRequest.messages,Length(aPrompt));
    for I:=0 to Length(aPrompt)-1 do
      begin
      lMessage:=Tapi_Message.Create;
      lMessage.role:=aPrompt[i].Role;
      lMessage.content:=aPrompt[i].Text;
      lRequest.messages[i] := lMessage;
      end;
    lResult:=lChatService.Complete(lRequest);
    SetLength(lResponses,1);
    lResponses[0].role:=lResult.Value.message.Role;
    lResponses[0].text:=lResult.Value.message.content;
    Result.SetOKValue(lResponses);
  finally
    lRequest.Free;
    lResult.Value.Free;
  end;
end;

class function TOLLamaProtocol.ProtocolName: string;
begin
  Result:='ollama';
end;

class function TOLLamaProtocol.DefaultURL: String;
begin
  Result:='http://localhost:11434/';
end;

class function TOLLamaProtocol.DefaultAPIKeyVariable: String;
begin
  Result:='';
end;

initialization
  TOLLamaProtocol.Register;
end.

