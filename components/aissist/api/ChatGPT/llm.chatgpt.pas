unit llm.chatgpt;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, llm.client, fpopenapiclient, chatgpt.Service.Intf, chatgpt.Service.Impl;

Type

  { TChatGPTProtocol }

  TChatGPTProtocol = class(TLLMProtocol)
  protected
    procedure ConfigProxy(aProxy : TFPOpenAPIServiceClient);
    function ConvertPromptArrayToString(aPrompt: TPromptArray): string;
  public
    function GetModels: TGetModelsResult; override;
    function SendPrompt(aPrompt: TPromptArray; aMaxLen: Integer): TSendPromptResult; override;
    class function ProtocolName : string; override;
    class function DefaultURL : String; override;
    class function DefaultAPIKeyVariable : String; override;
  end;

implementation

uses
  chatgpt.Dto;

{ TChatGPTProtocol }

procedure TChatGPTProtocol.ConfigProxy(aProxy : TFPOpenAPIServiceClient);

begin
  aProxy.WebClient:=ResolveWebClient;
  aProxy.BaseURL:=ResolveBaseURL;
  // Set OpenAI API authentication header
  aProxy.RequestHeaders.Add('Authorization=Bearer '+ResolveAuthorizationKey);
end;

function TChatGPTProtocol.ConvertPromptArrayToString(aPrompt: TPromptArray): string;
var
  i: Integer;
  lPromptText: string;
begin
  lPromptText := '';
  for i := 0 to Length(aPrompt) - 1 do
    begin
    if aPrompt[i].Role = 'user' then
      lPromptText := lPromptText + 'Human: ' + aPrompt[i].Text + #10
    else if aPrompt[i].Role = 'assistant' then
      lPromptText := lPromptText + 'Assistant: ' + aPrompt[i].Text + #10
    else if aPrompt[i].Role = 'system' then
      lPromptText := lPromptText + aPrompt[i].Text + #10
    else
      lPromptText := lPromptText + aPrompt[i].Text + #10;
    end;

  // Add prompt for assistant response if the last role wasn't assistant
  if (Length(aPrompt) > 0) and (aPrompt[Length(aPrompt)-1].Role <> 'assistant') then
    lPromptText := lPromptText + 'Assistant:';

  Result := lPromptText;
end;

function TChatGPTProtocol.GetModels: TGetModelsResult;
var
  lModelService: TModelsServiceProxy;
  lModelsResponse: TModelsServiceResult;
  lModels : TModelDataArray;
  i : Integer;
begin
  Result:=Default(TGetModelsResult);
  lModels:=[];
  lModelsResponse:=Default(TModelsServiceResult);
  lModelService:=TModelsServiceProxy.Create(Nil);
  try
    ConfigProxy(lModelService);
    // Exceptions will be handled by caller.
    lModelsResponse:=lModelService.Get_models();
    SetLength(lModels,Length(lModelsResponse.Value.data));
    for I:=0 to Length(lModels)-1 do
      begin
      lModels[i].ID:=lModelsResponse.Value.data[i].id;
      lModels[i].Name:=lModelsResponse.Value.data[i].id; // Use ID as name since no display name field
      end;
    Result.SetOKValue(lModels);
  finally
    lModelsResponse.Value.Free;
    lModelService.Free;
  end;
end;

function TChatGPTProtocol.SendPrompt(aPrompt: TPromptArray; aMaxLen: Integer): TSendPromptResult;
var
  lCompletionService: TCompletionsServiceProxy;
  lRequest: TCompletionRequest;
  lResponses : TPromptResponseArray;
  lResult: TCompletionResponseServiceResult;
begin
  Result:=Default(TSendPromptResult);
  lResponses:=[];
  lRequest:=nil;
  lResult:=Default(TCompletionResponseServiceResult);
  lCompletionService:=TCompletionsServiceProxy.Create(Nil);
  try
    ConfigProxy(lCompletionService);
    lRequest:=TCompletionRequest.Create;

    lRequest.model := ResolveModel;
    lRequest.prompt := ConvertPromptArrayToString(aPrompt);
    lRequest.max_tokens := aMaxLen;
    if lRequest.max_tokens = 0 then
      lRequest.max_tokens := 2048; // Default max tokens for ChatGPT
    lRequest.temperature := 0.7; // Default temperature
    lRequest.top_p := 1.0; // Default top_p
    lRequest.n := 1; // Number of completions
    lRequest.stream := False; // Non-streaming response

    lResult:=lCompletionService.Post_completions(lRequest);

    // Extract response from first choice
    SetLength(lResponses,1);
    if (Length(lResult.Value.choices) > 0) then
      begin
      lResponses[0].role := 'assistant';
      lResponses[0].text := lResult.Value.choices[0].text;
      end
    else
      begin
      lResponses[0].role := 'assistant';
      lResponses[0].text := '';
      end;

    Result.SetOKValue(lResponses);
  finally
    lRequest.Free;
    lResult.Value.Free;
    lCompletionService.Free;
  end;
end;

class function TChatGPTProtocol.ProtocolName: string;
begin
  Result:='chatgpt';
end;

class function TChatGPTProtocol.DefaultURL: String;
begin
  Result:='https://api.openai.com/v1/';
end;

class function TChatGPTProtocol.DefaultAPIKeyVariable: String;
begin
  Result:='OPENAI_API_KEY';
end;

initialization
  TChatGPTProtocol.Register;
end.