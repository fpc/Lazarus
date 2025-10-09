program gemini_demo;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils,
  fpWebClient,
  fphttpwebclient,
  jsonparser,
  opensslsockets,
  Gemini.Service.Intf,
  Gemini.Service.Impl,
  Gemini.Dto,
  Gemini.Persister;

var
  GenerateService: TGenerateContentServiceProxy;
  ModelsService: TModelsServiceProxy;
  Request: TGeminiGenerateContentRequest;
  Content: TGeminiContent;
  Part: TGeminiPart;
  Response: TGeminiGenerateContentResponseServiceResult;
  ModelsResponse: TGeminiListModelsResponseServiceResult;
  WebClient : TFPHTTPWebClient;
  lOwner : TComponent;
  lAPIKey : string;
  i,lCount : Integer;

begin
  WriteLn('Google Gemini API Demo');
  WriteLn('======================');
  lOwner:=TComponent.create(Nil);
  WebClient:=TFPHTTPWebClient.Create(lOwner);
  lAPIKey:=GetEnvironmentVariable('GEMINI_API_KEY');
  if lAPIKey='' then
    Raise Exception.Create('No api key set. Please modify the source or set the GEMINI_API_KEY environment variable');

  try
    WriteLn('Testing Models Service...');
    // Create the models service proxy
    ModelsService := TModelsServiceProxy.Create(lOwner);
    try
      ModelsService.WebClient:=WebClient;
      ModelsService.BaseURL := 'https://generativelanguage.googleapis.com';
      ModelsService.AddRequestHeader('x-goog-api-key',lApiKey);

      WriteLn('Requesting available models...');
      ModelsResponse := ModelsService.List();

      if ModelsResponse.Success then
        begin
        WriteLn('Models retrieved successfully!');
        WriteLn('Next page token: ', ModelsResponse.Value.next_page_token);
        lCount:=Length(ModelsResponse.Value.models);
        Writeln('Retrieved ',lCount,' models');
        if lCount > 0 then
          begin
          For i:=0 to lCount-1 do
            WriteLn('model[',i,']: ', ModelsResponse.Value.models[i].name)
          end
        else
          WriteLn('No models found in response');
        end
      else
        begin
        WriteLn('Error retrieving models:');
        WriteLn('Error message: ', ModelsResponse.ErrorText);
        WriteLn('Error code: ', ModelsResponse.ErrorCode);
        end;

    finally
      ModelsResponse.Value.Free;
      ModelsService.Free;
    end;

    WriteLn;
    WriteLn('Testing Generate Content Service...');

    // Create the content generation service proxy
    GenerateService := TGenerateContentServiceProxy.Create(lOwner);
    try
      GenerateService.WebClient:=WebClient;
      GenerateService.BaseURL := 'https://generativelanguage.googleapis.com';
      GenerateService.RequestHeaders.Values['x-goog-api-key']:=lApiKey;

      Request := TGeminiGenerateContentRequest.Create;
      try
        Part := TGeminiPart.Create;
        Part.text := 'What is the capital of France?';
        Content := TGeminiContent.Create;
        Content.role := 'user';
        SetLength(Content.parts, 1);
        Content.parts[0] := Part;
        SetLength(Request.contents, 1);
        Request.contents[0] := Content;

        // Configure generation settings (optional)
        Request.generation_config := TGeminiGenerationConfig.Create;
        Request.generation_config.max_output_tokens := 100;
        Request.generation_config.temperature := 0.7;
        Request.generation_config.top_p := 1.0;
        Request.generation_config.top_k := 40;

        WriteLn('Sending content generation request...');
        WriteLn('Model: gemini-2.0-flash');
        WriteLn('Prompt: ', Part.text);
        WriteLn('Max tokens: ', Request.generation_config.max_output_tokens);
        WriteLn;

        // Make the API call
        Response := GenerateService.GenerateContent('gemini-2.0-flash', Request);

        // Check if the request was successful
        if Response.Success then
          begin
          WriteLn('Response received successfully!');
          if Length(Response.Value.candidates) > 0 then
            begin
            if Length(Response.Value.candidates[0].content.parts) > 0 then
              begin
              WriteLn('Generated text: ', Response.Value.candidates[0].content.parts[0].text);
              WriteLn('Finish reason: ', Response.Value.candidates[0].finish_reason);
              end
            else
              WriteLn('No content parts in response');
            end
          else
            WriteLn('No candidates in response');

          if Assigned(Response.Value.usage_metadata) then
            begin
            WriteLn('Token usage:');
            WriteLn('  Prompt tokens: ', Response.Value.usage_metadata.prompt_token_count);
            WriteLn('  Candidate tokens: ', Response.Value.usage_metadata.candidates_token_count);
            WriteLn('  Total tokens: ', Response.Value.usage_metadata.total_token_count);
            end;
          end
        else
          begin
          WriteLn('Error occurred:');
          WriteLn('Error message: ', Response.ErrorText);
          WriteLn('Error code: ', Response.ErrorCode);
          end;

      finally
        Response.Value.Free;
        Request.Free;
      end;

    finally
      GenerateService.Free;
    end;

  except
    on E: Exception do
    begin
      WriteLn('Exception occurred: ', E.ClassName, ': ', E.Message);
    end;
  end;
  lOwner.Free;
end.
