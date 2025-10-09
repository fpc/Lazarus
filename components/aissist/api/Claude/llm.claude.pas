unit llm.claude;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, llm.client, fpopenapiclient, claude.Service.Intf, claude.Service.Impl;

Type

  { TClaudeProtocol }

  TClaudeProtocol = class(TLLMProtocol)
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
  claude.Dto;

{ TClaudeProtocol }

procedure TClaudeProtocol.ConfigProxy(aProxy : TFPOpenAPIServiceClient);

begin
  aProxy.WebClient:=ResolveWebClient;
  aProxy.BaseURL:=ResolveBaseURL;
  // Set Claude API authentication header
  aProxy.RequestHeaders.Add('x-api-key='+ResolveAuthorizationKey);
  aProxy.RequestHeaders.Add('anthropic-version=2023-06-01');
end;

function TClaudeProtocol.GetModels: TGetModelsResult;
var
  lModelService: TModelsServiceProxy;
  lModelsResponse: TModelsResponseServiceResult;
  lModels : TModelDataArray;
  i : Integer;
begin
  Result:=Default(TGetModelsResult);
  lModels:=[];
  lModelsResponse:=Default(TModelsResponseServiceResult);
  lModelService:=TModelsServiceProxy.Create(Nil);
  try
    ConfigProxy(lModelService);
    // Exceptions will be handled by caller.
    lModelsResponse:=lModelService.List();
    SetLength(lModels,Length(lModelsResponse.Value.data));
    for I:=0 to Length(lModels)-1 do
      begin
      lModels[i].ID:=lModelsResponse.Value.data[i].id;
      lModels[i].Name:=lModelsResponse.Value.data[i].display_name;
      if lModels[i].Name = '' then
        lModels[i].Name:=lModelsResponse.Value.data[i].id;
      end;
    Result.SetOKValue(lModels);
  finally
    lModelsResponse.Value.Free;
    lModelService.Free;
  end;
end;

function TClaudeProtocol.SendPrompt(aPrompt: TPromptArray; aMaxLen: Integer): TSendPromptResult;
var
  lMessageService: TMessagesServiceProxy;
  lRequest: TMessageRequest;
  i : Integer;
  lResponses : TPromptResponseArray;
  lResult: TMessageResponseServiceResult;
  lMessage: TMessage;
  lContent: TMessageContent;
begin
  Result:=Default(TSendPromptResult);
  lResponses:=[];
  lRequest:=nil;
  lResult:=Default(TMessageResponseServiceResult);
  lMessageService:=TMessagesServiceProxy.Create(Nil);
  try
    ConfigProxy(lMessageService);
    lRequest:=TMessageRequest.Create;
    lRequest.model := ResolveModel;
    lRequest.max_tokens := aMaxLen;
    if lRequest.max_tokens = 0 then
      lRequest.max_tokens := 4096; // Default max tokens for Claude
    lRequest.stream := False;  // Non-streaming response
    SetLength(lRequest.messages,Length(aPrompt));

    for I:=0 to Length(aPrompt)-1 do
      begin
      lMessage:=TMessage.Create;
      lMessage.role:=aPrompt[i].Role;
      SetLength(lMessage.content, 1);
      lContent:=TMessageContent.Create;
      lContent.type_:='text';
      lContent.text:=aPrompt[i].Text;
      lMessage.content[0] := lContent;
      lRequest.messages[i] := lMessage;
      end;

    lResult:=lMessageService.CreateMessage(lRequest);
    SetLength(lResponses,1);
    lResponses[0].role:=lResult.Value.role;
    // Extract text from first content item
    if (Length(lResult.Value.content) > 0) and (lResult.Value.content[0].type_ = 'text') then
      lResponses[0].text:=lResult.Value.content[0].text
    else
      lResponses[0].text:='';
    Result.SetOKValue(lResponses);
  finally
    lRequest.Free;
    lResult.Value.Free;
    lMessageService.Free;
  end;
end;

class function TClaudeProtocol.ProtocolName: string;
begin
  Result:='claude';
end;

class function TClaudeProtocol.DefaultURL: String;
begin
  Result:='https://api.anthropic.com/';
end;

class function TClaudeProtocol.DefaultAPIKeyVariable: String;
begin
  Result:='ANTHROPIC_API_KEY';
end;

initialization
  TClaudeProtocol.Register;
end.