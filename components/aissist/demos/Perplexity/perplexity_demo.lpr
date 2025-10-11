program perplexity_demo;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpwebclient, fphttpwebclient, opensslsockets, jsonparser,
  perplexity.Service.Intf,
  perplexity.Service.Impl,
  perplexity.Dto,
  perplexity.Serializer;

var
  CompletionsService: TCompletionsServiceProxy;
  Request: TCompletionRequest;
  Response: TCompletionResponseServiceResult;
  lOwner : TComponent;
  lWebClient : TAbstractWebClient;
  lAPIKey : String;

begin
  WriteLn('Perplexity API Demo - Completions Request');
  WriteLn('======================================');
  lAPIKey:=GetEnvironmentVariable('PERPLEXITY_API_KEY');
  if lAPIKey='' then
    Raise Exception.Create('No api key set. Please modify the source or set the PERPLEXITY_API_KEY environment variable.');
  Request:=Nil;
  Response:=Default(TCompletionResponseServiceResult);
  lOwner:=TComponent.Create(Nil);
  try
    lWebClient:=TFPHTTPWebClient.Create(lOwner);
    // Create the service proxy instance
    CompletionsService := TCompletionsServiceProxy.Create(lOwner);
    CompletionsService.WebClient:=lWebClient;
    CompletionsService.BaseURL := 'https://api.perplexity.ai/';
    CompletionsService.RequestHeaders.Values['Authorization']:='Bearer '+lAPIKey;
    // Create and configure the completion request
    Request := TCompletionRequest.Create;
    // Configure the request parameters
    //Request.model := 'mistral-7b';  // Use the text completion model
    Request.model := 'sonar';  // Use the text completion model
    Request.Add_Message( 'user', 'What is the capital of France? Answer in one sentence.');
    Request.max_tokens := 100;
    Request.temperature := 0.7;
    Request.top_p := 1.0;
    Request.stream := False;  // Non-streaming response
    Request.frequency_penalty := 0.0;
    Request.presence_penalty := 0.0;

    WriteLn('Sending request to Perplexity API...');
    WriteLn('Model: ', Request.model);
    WriteLn('Messages: ', Request.List_Messages);
    WriteLn('Max tokens: ', Request.max_tokens);
    WriteLn('Temperature: ', Request.temperature:0:1);
    WriteLn;

    // Make the API call
    Response := CompletionsService.Post_completions(Request);
    if Response.Success
    then
        begin
        if     (Length(Response.Value.choices)>0)
           and assigned(Response.Value.choices[0])
        then
            begin
            if assigned(Response.Value.choices[0].message)
            then
                WriteLn('Model response: ', Response.Value.choices[0].message.content);
            WriteLn( Response.Value.List_Citations);
            WriteLn('Hit enter to continue ...');
            Readln;
            WriteLn( Response.Value.List_Search_Results);
            end;
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
  WriteLn('Hit enter to terminate ...');
  Readln;
end.
