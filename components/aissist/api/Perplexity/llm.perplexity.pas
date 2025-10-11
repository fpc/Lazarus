unit llm.perplexity;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, llm.client, fpopenapiclient, perplexity.Service.Intf, perplexity.Service.Impl;

Type

  { TPerplexityProtocol }

  TPerplexityProtocol = class(TLLMProtocol)
  protected
    procedure ConfigProxy(aProxy : TFPOpenAPIServiceClient);
    function ConvertPromptArrayToString(aPrompt: TPromptArray): string;
  public
    function GetModels: TGetModelsResult; override;
    function SendPrompt(aPrompt: TPromptArray; aMaxLen: Integer): TSendPromptResult; override;
    class function ProtocolName : string; override;
    class function DefaultURL : String; override;
    class function DefaultAPIKeyVariable : String; override;

    //not sure for this override
    class function APIKeyVariable : String; override;
  end;

implementation

uses
  perplexity.Dto;

{ TPerplexityProtocol }

procedure TPerplexityProtocol.ConfigProxy(aProxy : TFPOpenAPIServiceClient);

begin
     aProxy.WebClient:=ResolveWebClient;
     aProxy.BaseURL:=ResolveBaseURL;
     // Set OpenAI API authentication header
     //aProxy.RequestHeaders.Add('Authorization=Bearer '+ResolveAuthorizationKey);
     aProxy.RequestHeaders.Values['Authorization']:='Bearer '+ResolveAuthorizationKey;
end;

function TPerplexityProtocol.ConvertPromptArrayToString(aPrompt: TPromptArray): string;
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

function TPerplexityProtocol.GetModels: TGetModelsResult;
var
   lModels: TModelDataArray
   =
    (
    (ID: 'sonar'              ; Name: 'sonar'              ),
    (ID: 'sonar-pro'          ; Name: 'sonar-pro'          ),
    (ID: 'sonar-deep-research'; Name: 'sonar-deep-research'),
    (ID: 'sonar-reasoning'    ; Name: 'sonar-reasoning'    ),
    (ID: 'sonar-reasoning-pro'; Name: 'sonar-reasoning-pro')
    );
begin
     Result:=Default(TGetModelsResult);
     Result.SetOKValue(lModels);
end;

function TPerplexityProtocol.SendPrompt(aPrompt: TPromptArray; aMaxLen: Integer): TSendPromptResult;
var
   lCompletionService: TCompletionsServiceProxy;
   lRequest: TCompletionRequest;
   lResponses : TPromptResponseArray;
   lResult: TCompletionResponseServiceResult;
   procedure Process_Prompt;
   var
      p: TPrompt;
   begin
        for p in aPrompt
        do
          lRequest.Add_Message( p.Role, p.Text);
   end;
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
        Process_Prompt;
        lRequest.max_tokens := aMaxLen;
        if lRequest.max_tokens = 0
        then
            lRequest.max_tokens := 100; // Default max tokens for Perplexity
        lRequest.temperature := 0.2; // Default temperature
        lRequest.top_p := 0.9; // Default top_p
        lRequest.stream := False; // Non-streaming response

        lResult:=lCompletionService.Post_completions(lRequest);

        // Extract response from first choice
        SetLength(lResponses,1);
        if     Assigned( lResult.Value)
           and (Length(lResult.Value.choices) > 0)
        then
            begin
            lResponses[0].role := lResult.Value.choices[0].message.role;
            lResponses[0].text := lResult.Value.choices[0].message.content;
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

class function TPerplexityProtocol.ProtocolName: string;
begin
  Result:='perplexity';
end;

class function TPerplexityProtocol.DefaultURL: String;
begin
     Result:='https://api.perplexity.ai/';
end;

class function TPerplexityProtocol.DefaultAPIKeyVariable: String;
begin
     Result:='PERPLEXITY_API_KEY';
end;

//not sure for this override
class function TPerplexityProtocol.APIKeyVariable: String;
begin
     Result:= DefaultAPIKeyVariable;
end;

initialization
  TPerplexityProtocol.Register;
end.
