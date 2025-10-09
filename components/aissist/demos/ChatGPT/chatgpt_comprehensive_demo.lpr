program chatgpt_comprehensive_demo;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpwebclient, fphttpwebclient, opensslsockets, jsonparser, fpopenapiclient,
  chatgpt.Service.Intf,
  chatgpt.Service.Impl,
  chatgpt.Dto,
  chatgpt.Serializer;

var
  // Will be initialized from environment variable: OPENAI_API_KEY
  // Alternatively, set the value here.
  lAPIKey : string;

procedure SetupService(aService : TFPOpenAPIServiceClient; aWebClient : TAbstractWebClient);
begin
  aService.WebClient:=aWebClient;
  aService.BaseURL := 'https://api.openai.com/v1';
  aService.RequestHeaders.Values['Authorization']:='Bearer '+lAPIKey;
end;

procedure DoCompletions(aOwner : TComponent; aWebClient : TAbstractWebClient);

var
  CompletionsService: TCompletionsServiceProxy;
  CompletionResponse: TCompletionResponseServiceResult;
  CompletionRequest: TCompletionRequest;

begin
  CompletionRequest:=Nil;
  CompletionResponse:=Default(TCompletionResponseServiceResult);
  try
    CompletionsService := TCompletionsServiceProxy.Create(aOwner);
    SetupService(CompletionsService,aWebClient);

    CompletionRequest := TCompletionRequest.Create;
    CompletionRequest.model := 'gpt-3.5-turbo-instruct';
    CompletionRequest.prompt := 'Explain artificial intelligence in simple terms.';
    CompletionRequest.max_tokens := 150;
    CompletionRequest.temperature := 0.8;
    CompletionRequest.top_p := 1.0;
    CompletionRequest.n := 1;
    CompletionRequest.frequency_penalty := 0.0;
    CompletionRequest.presence_penalty := 0.0;
    CompletionRequest.best_of:=1;

    WriteLn('Configuration:');
    WriteLn('  Model: ', CompletionRequest.model);
    WriteLn('  Prompt: ', CompletionRequest.prompt);
    WriteLn('  Max Tokens: ', CompletionRequest.max_tokens);
    WriteLn('  Temperature: ', CompletionRequest.temperature:0:1);
    WriteLn;

    CompletionResponse := CompletionsService.Post_completions(CompletionRequest);

    if CompletionResponse.Success then
      begin
      WriteLn('Completion request successful :)');
      if Assigned(CompletionResponse.Value) and (Length(CompletionResponse.Value.choices) > 0) then
        begin
        WriteLn('Completion text: ', CompletionResponse.Value.choices[0].text);
        WriteLn('Model: ', CompletionResponse.Value.model);
        WriteLn('Finish reason: ', CompletionResponse.Value.choices[0].finish_reason);
        if Assigned(CompletionResponse.Value.usage) then
          begin
          WriteLn('Token usage:');
          WriteLn('  Prompt tokens: ', CompletionResponse.Value.usage.prompt_tokens);
          WriteLn('  Completion tokens: ', CompletionResponse.Value.usage.completion_tokens);
          WriteLn('  Total tokens: ', CompletionResponse.Value.usage.total_tokens);
          end;
        end
      else
        WriteLn('No completion data received');
      end
    else
      begin
      WriteLn('Completion request failed :(');
      WriteLn('Error: ', CompletionResponse.ErrorText);
      WriteLn('Code: ', CompletionResponse.ErrorCode);
    end;
  finally
    CompletionRequest.Free;
    CompletionResponse.Value.Free;
  end;
end;

procedure DoEmbeddings(aOwner : TComponent; aWebClient : TAbstractWebClient);
var
  EmbeddingsService: TEmbeddingsServiceProxy;
  EmbeddingRequest: TEmbeddingsRequest;
  EmbeddingResponse: TEmbeddingsResponseServiceResult;
begin
  EmbeddingRequest:=Nil;
  EmbeddingResponse:=Default(TEmbeddingsResponseServiceResult);
  try
    EmbeddingsService := TEmbeddingsServiceProxy.Create(aOwner);
    SetupService(EmbeddingsService,aWebClient);
    EmbeddingRequest := TEmbeddingsRequest.Create;
    EmbeddingRequest.model := 'text-embedding-ada-002';
    SetLength(EmbeddingRequest.input, 1);
    EmbeddingRequest.input[0] := 'Hello, world! This is a test string for embedding.';

    WriteLn('Configuration:');
    WriteLn('  Model: ', EmbeddingRequest.model);
    WriteLn('  Input: ', EmbeddingRequest.input[0]);
    WriteLn;

    EmbeddingResponse := EmbeddingsService.Post_embeddings(EmbeddingRequest);
    if EmbeddingResponse.Success then
      begin
      WriteLn('Embedding request successful :)');
      if Assigned(EmbeddingResponse.Value) and (Length(EmbeddingResponse.Value.data) > 0) then
        begin
        WriteLn('Model: ', EmbeddingResponse.Value.model);
        WriteLn('Object type: ', EmbeddingResponse.Value.object_);
        WriteLn('Number of embeddings: ', Length(EmbeddingResponse.Value.data));
        if Assigned(EmbeddingResponse.Value.usage) then
          begin
          WriteLn('Token usage:');
          WriteLn('  Prompt tokens: ', EmbeddingResponse.Value.usage.prompt_tokens);
          WriteLn('  Total tokens: ', EmbeddingResponse.Value.usage.total_tokens);
          end;
        end
      else
        WriteLn('No embedding data received');
      end
    else
      begin
      WriteLn('Embedding request failed :(');
      WriteLn('Error: ', EmbeddingResponse.ErrorText);
      WriteLn('Code: ', EmbeddingResponse.ErrorCode);
      end;
  finally
    EmbeddingResponse.Value.Free;
    EmbeddingRequest.Free;
  end;
end;

procedure DoFiles(aOwner : TComponent; aWebClient : TAbstractWebClient);
var
  I : Integer;
  FilesService: TFilesServiceProxy;
  FilesResponse: TFileResponseServiceResult;

begin
  FilesResponse:=Default(TFileResponseServiceResult);
  try
    FilesService := TFilesServiceProxy.Create(aOwner);
    SetupService(FilesService,aWebClient);
    WriteLn('Testing file listing endpoint...');
    FilesResponse := FilesService.Get_files();
    if FilesResponse.Success then
      begin
      WriteLn('Files list request successful :)');
      if Assigned(FilesResponse.Value) and (Length(FilesResponse.Value.data) > 0) then
        begin
        WriteLn('Object type: ', FilesResponse.Value.object_);
        WriteLn('Number of files: ', Length(FilesResponse.Value.data));
        WriteLn('Files:');
        For I:=0 to Length(FilesResponse.Value.data) do
          Writeln('File[',i,']: ',FilesResponse.Value.data[i].url);
        end
      else
        WriteLn('No files found or no data received');
      end
    else
      begin
      WriteLn('Files list request failed :(');
      WriteLn('Error: ', FilesResponse.ErrorText);
      WriteLn('Code: ', FilesResponse.ErrorCode);
      end;
  finally
    FilesResponse.Value.Free;
  end;
end;

procedure ShowSeparator(const Title: string);
begin
  Writeln;
  WriteLn(Title);
  WriteLn(StringOfChar('=',Length(Title)));
  Writeln;
end;

var
  lWebClient : TAbstractWebClient;
  lOwner : TComponent;


begin
  WriteLn('ChatGPT API Comprehensive Demo');
  WriteLn('==============================');

  if lAPIKey='' then
    lAPIKey:=GetEnvironmentVariable('OPENAI_API_KEY');

  if lAPIKey='' then
    Raise Exception.Create('No api key set. Please modify the source or set the OPENAI_API_KEY environment variable.');

  lOwner:=TComponent.Create(Nil);
  try
    lWebClient:=TFPHTTPWebClient.Create(lOwner);

    ShowSeparator('1. Text Completions Service');
    DoCompletions(lOwner,lWebClient);

    ShowSeparator('2. Embeddings Service');
    DoEmbeddings(lOwner,lWebClient);

    ShowSeparator('3. Files Service');
    DoFiles(lOwner,lWebClient);
  except
    on E: Exception do
      begin
      WriteLn;
      WriteLn('EXCEPTION OCCURRED:');
      WriteLn('Class: ', E.ClassName);
      WriteLn('Message: ', E.Message);
      end;
  end;
  lOwner.Free;
end.
