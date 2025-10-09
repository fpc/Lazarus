program chatgpt_demo;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpwebclient, fphttpwebclient, opensslsockets, jsonparser,
  chatgpt.Service.Intf,
  chatgpt.Service.Impl,
  chatgpt.Dto,
  chatgpt.Serializer;

var
  CompletionsService: TCompletionsServiceProxy;
  Request: TCompletionRequest;
  Response: TCompletionResponseServiceResult;
  lOwner : TComponent;
  lWebClient : TAbstractWebClient;
  lAPIKey : String;

begin
  WriteLn('ChatGPT API Demo - Completions Request');
  WriteLn('======================================');
  lAPIKey:=GetEnvironmentVariable('OPENAI_API_KEY');
  if lAPIKey='' then
    Raise Exception.Create('No api key set. Please modify the source or set the OPENAI_API_KEY environment variable.');
  Request:=Nil;
  Response:=Default(TCompletionResponseServiceResult);
  lOwner:=TComponent.Create(Nil);
  try
    lWebClient:=TFPHTTPWebClient.Create(lOwner);
    // Create the service proxy instance
    CompletionsService := TCompletionsServiceProxy.Create(lOwner);
    CompletionsService.WebClient:=lWebClient;
    CompletionsService.BaseURL := 'https://api.openai.com/v1';
    CompletionsService.RequestHeaders.Values['Authorization']:='Bearer '+lAPIKey;
    // Create and configure the completion request
    Request := TCompletionRequest.Create;
    // Configure the request parameters
    Request.model := 'gpt-3.5-turbo-instruct';  // Use the text completion model
    Request.prompt := 'What is the capital of France? Answer in one sentence.';
    Request.max_tokens := 100;
    Request.temperature := 0.7;
    Request.top_p := 1.0;
    Request.n := 1;  // Number of completions to generate
    Request.echo := False;  // Don't echo the prompt in response
    Request.stream := False;  // Non-streaming response
    Request.frequency_penalty := 0.0;
    Request.presence_penalty := 0.0;
    Request.best_of:=1;

    WriteLn('Sending request to ChatGPT API...');
    WriteLn('Model: ', Request.model);
    WriteLn('Prompt: ', Request.prompt);
    WriteLn('Max tokens: ', Request.max_tokens);
    WriteLn('Temperature: ', Request.temperature:0:1);
    WriteLn;

    // Make the API call
    Response := CompletionsService.Post_completions(Request);
    if Response.Success then
      begin
      if (Length(Response.Value.choices)>0) and assigned(Response.Value.choices[0]) then
        WriteLn('Model response: ', Response.Value.choices[0].text);
      end
    else
      begin
      WriteLn('Error occurred:');
      WriteLn('Error message: ', Response.ErrorText);
      WriteLn('Error code: ', Response.ErrorCode);
      end;
  except
    on E: Exception do
      WriteLn('Exception occurred: ', E.ClassName, ': ', E.Message);
  end;
  CompletionsService.Free;
  Request.Free;
  Response.Value.Free;
  lOwner.Free;
end.
