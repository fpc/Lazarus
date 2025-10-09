program ollama_demo;

{$mode objfpc}{$H+}

{ You may need to adjust the model, host and port where ollama is listening
  If no model is specified, the first model in the listed models will be used.
}

uses
  Classes, SysUtils, fpwebclient, fphttpwebclient, opensslsockets, jsonparser,
  ollama.Service.Intf,
  ollama.Service.Impl,
  ollama.Dto;

var
  ModelService: TModelServiceProxy;
  ChatService: TChatServiceProxy;
  ModelsResponse: Tapi_ListResponseServiceResult;
  Request: Tapi_ChatRequest;
  Response: Tapi_ChatResponseServiceResult;
  lMessage: Tapi_Message;
  lOwner: TComponent;
  lWebClient: TAbstractWebClient;
  i: Integer;
  lBaseURL,lModel,lHost : string;
  lPort : Integer;

begin
  // Default Ollama URL parameters
  lHost:='localhost';
  lPort:=11434;
  lModel:='';
  lBaseURL:=Format('http://%s:%d',[lHost,lPort]);
  WriteLn('Ollama API Demo - Models List and Chat Completion Request');
  WriteLn('========================================================');

  lOwner := TComponent.Create(Nil);
  try
    lWebClient := TFPHTTPWebClient.Create(lOwner);

    // First, list available models
    ModelService := TModelServiceProxy.Create(lOwner as TComponent);
    try
      ModelService.WebClient := lWebClient;
      ModelService.BaseURL := lBaseURL;

      WriteLn('Fetching available models...');
      WriteLn;

      ModelsResponse := ModelService.List;

      if ModelsResponse.Success then
        begin
        WriteLn('Available Models:');
        WriteLn('================');
        for i := 0 to Length(ModelsResponse.Value.models) - 1 do
          begin
          if lModel='' then
            lModel:=ModelsResponse.Value.models[i].name;
          WriteLn('Model[', i, '] Name: ', ModelsResponse.Value.models[i].name);
          WriteLn('  Size: ', ModelsResponse.Value.models[i].size);
          WriteLn('  Modified: ', ModelsResponse.Value.models[i].modified_at);
          WriteLn;
          end;
        end
      else
        begin
        WriteLn('Error fetching models:');
        WriteLn('Error message: ', ModelsResponse.ErrorText);
        WriteLn('Error code: ', ModelsResponse.ErrorCode);
        WriteLn;
        end;
    finally
      ModelService.Free;
    end;
    if lModel='' then
      Request.model := 'gemma3:latest';  // Use a common Ollama model

    // Now proceed with chat service
    ChatService := TChatServiceProxy.Create(lOwner);
    try
      ChatService.WebClient := lWebClient;
      ChatService.BaseURL := lBaseURL;

      Request := Tapi_ChatRequest.Create;
      try
        Request.model := lModel;
        // Request.format:='json';
        Request.keep_alive:=300;
        Request.think:=false;
        SetLength(Request.messages, 1);
        lMessage := Tapi_Message.Create;
        lMessage.role := 'user';
        lMessage.content := 'What is the capital of France?';
        Request.messages[0] := lMessage;
        Request.stream := False;  // Non-streaming response

        WriteLn('Sending chat request to Ollama API...');
        WriteLn('Model: ', Request.model);
        WriteLn('Message: ', lMessage.content);
        WriteLn;

        Response := ChatService.Complete(Request);

        if Response.Success then
          begin
          WriteLn('Response received successfully!');
          WriteLn('Completion: ', Response.Value.message.content);
          WriteLn('Done: ', BoolToStr(Response.Value.done, True));
          WriteLn('Model: ', Response.Value.model);
          if Response.Value.eval_count > 0 then
            WriteLn('Tokens evaluated: ', Response.Value.eval_count);
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
      ChatService.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('Exception occurred: ', E.ClassName, ': ', E.Message);
    end;
  end;
  lOwner.Free;
end.
