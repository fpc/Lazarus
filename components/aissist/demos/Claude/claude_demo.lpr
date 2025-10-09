program claude_demo;

{
  Your Antrhopic API key should be set in the ANTHROPIC_API_KEY environment variable.
  Alternatively, set it directly in the sources below.
}

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpwebclient, fphttpwebclient, opensslsockets, jsonparser,
  claude.Service.Intf,
  claude.Service.Impl,
  claude.Dto;

var
  ModelsService: TModelsServiceProxy;
  MessagesService: TMessagesServiceProxy;
  ModelsResponse: TModelsResponseServiceResult;
  Request: TMessageRequest;
  Response: TMessageResponseServiceResult;
  lModel : TModel;
  lMsg : TMessage;
  lOwner : TComponent;
  lWebClient : TAbstractWebClient;
  i: Integer;

  lAPIKey : String;

begin
  WriteLn('Claude API Demo - Models List and Completion Request');
  WriteLn('===================================================');
  lAPIKey:=GetEnvironmentVariable('ANTHROPIC_API_KEY');
  lOwner:=TComponent.Create(Nil);
  try
    lWebClient:=TFPHTTPWebClient.Create(lOwner);

    // First, list available models
    ModelsService := TModelsServiceProxy.Create(lOwner);
    try
      ModelsService.WebClient:=lWebClient;
      ModelsService.BaseURL := 'https://api.anthropic.com';
      ModelsService.RequestHeaders.Values['x-api-key']:=lAPIKey;
      ModelsService.RequestHeaders.Values['anthropic-version']:='2023-06-01';

      WriteLn('Fetching available models...');
      WriteLn;

      ModelsResponse := ModelsService.List;

      if ModelsResponse.Success then
        begin
        WriteLn('Available Models:');
        WriteLn('================');
        for i := 0 to Length(ModelsResponse.Value.data) - 1 do
          begin
          lModel:=ModelsResponse.Value.data[i];
          Writeln('Model[',i,'] ID: ', lModel.id,' Name',lModel.display_name);
          end;
        WriteLn;
        end
      else
        begin
        WriteLn('Error fetching models:');
        WriteLn('Error message: ', ModelsResponse.ErrorText);
        WriteLn('Error code: ', ModelsResponse.ErrorCode);
        WriteLn;
        end;
    finally
      ModelsService.Free;
    end;

    // Now proceed with message service
    MessagesService := TMessagesServiceProxy.Create(lOwner);
    try
      MessagesService.WebClient:=lWebClient;
      MessagesService.BaseURL := 'https://api.anthropic.com';
      MessagesService.RequestHeaders.Values['x-api-key']:=lAPIKey;
      MessagesService.RequestHeaders.Values['anthropic-version']:='2023-06-01';

      Request := TMessageRequest.Create;
      try
        Request.model := 'claude-3-haiku-20240307';  // Use Claude 3 Haiku model
        SetLength(Request.Messages,1);
        lMsg:=TMessage.Create;
        lMsg.role:='user';
        Request.Messages[0]:=lMsg;
        SetLength(lMsg.content,1);
        lMsg.content[0]:=TMessageContent.Create;
        lMsg.content[0].text:='What is the capital of France?';
        lMsg.content[0].type_:='text';
        Request.max_tokens:=100;
        Request.temperature := 0.7;
        Request.top_p := 1.0;
        Request.top_k := -1;  // No top-k filtering
        Request.stream := False;  // Non-streaming response

        WriteLn('Sending message request to Claude API...');
        WriteLn('Model: ', Request.model);
        Writeln('Message : ',lMsg.content[0].text);
        WriteLn;

        Response := MessagesService.CreateMessage(Request);

        if Response.Success then
          begin
          WriteLn('Response received successfully!');
          WriteLn('Completion: ', Response.Value.content[0].text);
          WriteLn('Stop reason: ', Response.Value.stop_reason);
          end
        else
          begin
          WriteLn('Error occurred:');
          WriteLn('Error message: ', Response.ErrorText);
          WriteLn('Error code: ', Response.ErrorCode);
          end;
      finally
        Request.Free;
      end;
    finally
      MessagesService.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('Exception occurred: ', E.ClassName, ': ', E.Message);
    end;
  end;
  lOwner.Free;
end.
