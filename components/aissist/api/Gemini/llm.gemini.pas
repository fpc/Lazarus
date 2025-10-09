unit llm.gemini;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, llm.client, fpopenapiclient, Gemini.Service.Intf, Gemini.Service.Impl;

Type

  { TGeminiProtocol }

  TGeminiProtocol = class(TLLMProtocol)
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
  Gemini.Dto;

{ TGeminiProtocol }

procedure TGeminiProtocol.ConfigProxy(aProxy : TFPOpenAPIServiceClient);

begin
  aProxy.WebClient:=ResolveWebClient;
  aProxy.BaseURL:=ResolveBaseURL;
  // Set Gemini API authentication header
  aProxy.RequestHeaders.Add('x-goog-api-key='+ResolveAuthorizationKey);
end;

function TGeminiProtocol.GetModels: TGetModelsResult;
var
  lModelService: TModelsServiceProxy;
  lModelsResponse: TGeminiListModelsResponseServiceResult;
  lModels : TModelDataArray;
  i : Integer;
  lModelName: string;
begin
  Result:=Default(TGetModelsResult);
  lModels:=[];
  lModelsResponse:=Default(TGeminiListModelsResponseServiceResult);
  lModelService:=TModelsServiceProxy.Create(Nil);
  try
    ConfigProxy(lModelService);
    // Exceptions will be handled by caller.
    lModelsResponse:=lModelService.List();
    SetLength(lModels,Length(lModelsResponse.Value.models));
    for I:=0 to Length(lModels)-1 do
      begin
      lModelName := lModelsResponse.Value.models[i].name;
      // Remove 'models/' prefix if present for cleaner ID
      if lModelName.StartsWith('models/') then
        lModels[i].ID := Copy(lModelName, 8, Length(lModelName))
      else
        lModels[i].ID := lModelName;
      lModels[i].Name := lModelsResponse.Value.models[i].display_name;
      if lModels[i].Name = '' then
        lModels[i].Name := lModels[i].ID;
      end;
    Result.SetOKValue(lModels);
  finally
    lModelsResponse.Value.Free;
    lModelService.Free;
  end;
end;

function TGeminiProtocol.SendPrompt(aPrompt: TPromptArray; aMaxLen: Integer): TSendPromptResult;
var
  lContentService: TGenerateContentServiceProxy;
  lRequest: TGeminiGenerateContentRequest;
  i : Integer;
  lResponses : TPromptResponseArray;
  lResult: TGeminiGenerateContentResponseServiceResult;
  lContent: TGeminiContent;
  lPart: TGeminiPart;
  lResponseText: string;
begin
  Result:=Default(TSendPromptResult);
  lResponses:=[];
  lRequest:=nil;
  lResult:=Default(TGeminiGenerateContentResponseServiceResult);
  lContentService:=TGenerateContentServiceProxy.Create(Nil);
  try
    ConfigProxy(lContentService);
    lRequest:=TGeminiGenerateContentRequest.Create;

    // Set generation configuration
    lRequest.generation_config := TGeminiGenerationConfig.Create;
    lRequest.generation_config.max_output_tokens := aMaxLen;
    if lRequest.generation_config.max_output_tokens = 0 then
      lRequest.generation_config.max_output_tokens := 8192; // Default max tokens for Gemini
    lRequest.generation_config.candidate_count := 1;

    // Convert prompt array to Gemini content format
    SetLength(lRequest.contents, Length(aPrompt));
    for I:=0 to Length(aPrompt)-1 do
      begin
      lContent:=TGeminiContent.Create;
      lContent.role := aPrompt[i].Role;
      SetLength(lContent.parts, 1);
      lPart := TGeminiPart.Create;
      lPart.text := aPrompt[i].Text;
      lContent.parts[0] := lPart;
      lRequest.contents[i] := lContent;
      end;

    lResult:=lContentService.GenerateContent(ResolveModel, lRequest);

    // Extract response from first candidate
    SetLength(lResponses,1);
    lResponseText := '';
    if (Length(lResult.Value.candidates) > 0) then
      begin
      lResponses[0].role := lResult.Value.candidates[0].content.role;
      // Concatenate all text parts from the response
      if (Length(lResult.Value.candidates[0].content.parts) > 0) then
        begin
        for i := 0 to Length(lResult.Value.candidates[0].content.parts) - 1 do
          lResponseText := lResponseText + lResult.Value.candidates[0].content.parts[i].text;
        end;
      lResponses[0].text := lResponseText;
      end
    else
      begin
      lResponses[0].role := 'model';
      lResponses[0].text := '';
      end;

    Result.SetOKValue(lResponses);
  finally
    lRequest.Free;
    lResult.Value.Free;
    lContentService.Free;
  end;
end;

class function TGeminiProtocol.ProtocolName: string;
begin
  Result:='gemini';
end;

class function TGeminiProtocol.DefaultURL: String;
begin
  Result:='https://generativelanguage.googleapis.com/v1beta/';
end;

class function TGeminiProtocol.DefaultAPIKeyVariable: String;
begin
  Result:='GEMINI_API_KEY';
end;

initialization
  TGeminiProtocol.Register;
end.