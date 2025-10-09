{
    This file is part of the Free Component Library
    Copyright (c) 2025 by Michael Van Canneyt michael@freepascal.org

    OLLama Rest API - Service interface implementations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit ollama.Service.Impl;

{$mode objfpc}
{$h+}

interface

uses
  classes, fpopenapiclient
  , ollama.Service.Intf                     // Service definition 
  , ollama.Dto;

Type
  // Service IBlobsService
  
  TBlobsServiceProxy = Class (TFPOpenAPIServiceClient,IBlobsService)
    Function Check(aDigest : string; aResponseStream : TStream) : TStreamServiceResult;
    Function CreateBlob(aDigest : string; aRequest : TStream; aResponseStream : TStream) : TStreamServiceResult;
  end;
  
  // Service IChatService
  
  TChatServiceProxy = Class (TFPOpenAPIServiceClient,IChatService)
    Function Complete(aRequest : Tapi_ChatRequest) : Tapi_ChatResponseServiceResult;
  end;
  
  // Service IChatServiceV1
  
  TChatServiceV1Proxy = Class (TFPOpenAPIServiceClient,IChatServiceV1)
    Function Complete(aRequest : Tapi_ChatRequest) : Tapi_ChatResponseServiceResult;
  end;
  
  // Service IEmbeddingsService
  
  TEmbeddingsServiceProxy = Class (TFPOpenAPIServiceClient,IEmbeddingsService)
    Function Generate(aRequest : Tapi_EmbedRequest) : Tapi_EmbedResponseServiceResult;
    Function GenerateCompatibility(aRequest : Tapi_EmbeddingRequest) : Tapi_EmbeddingResponseServiceResult;
  end;
  
  // Service IEmbeddingsServiceV1
  
  TEmbeddingsServiceV1Proxy = Class (TFPOpenAPIServiceClient,IEmbeddingsServiceV1)
    Function Generate(aRequest : Tapi_EmbedRequest) : Tapi_EmbedResponseServiceResult;
  end;
  
  // Service IGenerationService
  
  TGenerationServiceProxy = Class (TFPOpenAPIServiceClient,IGenerationService)
    Function Completion(aRequest : Tapi_GenerateRequest) : stringServiceResult;
    Function Post_v1_completions(aRequest : Tapi_GenerateRequest) : stringServiceResult;
  end;
  
  // Service IModelService
  
  TModelServiceProxy = Class (TFPOpenAPIServiceClient,IModelService)
    Function Copy(aRequest : Tapi_CopyRequest) : stringServiceResult;
    Function CreateModel(aRequest : Tapi_CreateRequest) : Tapi_ProgressResponseServiceResult;
    Function Delete(aRequest : Tapi_DeleteRequest) : stringServiceResult;
    Function List() : Tapi_ListResponseServiceResult;
    Function Pull(aRequest : Tapi_PullRequest) : Tapi_ProgressResponseServiceResult;
    Function Push(aRequest : Tapi_PushRequest) : Tapi_ProgressResponseServiceResult;
    Function Running() : Tapi_ProcessResponseServiceResult;
    Function Show(aRequest : Tapi_ShowRequest) : Tapi_ShowResponseServiceResult;
  end;
  
  // Service IModelServiceV1
  
  TModelServiceV1Proxy = Class (TFPOpenAPIServiceClient,IModelServiceV1)
    Function Get(aModel : string; aRequest : Tapi_ShowRequest) : Tapi_ShowResponseServiceResult;
    Function List() : Tapi_ListResponseServiceResult;
  end;
  
  // Service ISystemService
  
  TSystemServiceProxy = Class (TFPOpenAPIServiceClient,ISystemService)
    Function Version() : stringServiceResult;
    Function Version2() : stringServiceResult;
  end;
  

implementation

uses
  SysUtils
  , ollama.Serializer;

Function TBlobsServiceProxy.Check(aDigest : string; aResponseStream : TStream) : TStreamServiceResult;

const
  lMethodURL = '/api/blobs/{digest}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TStreamServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'digest',aDigest);
  lResponse:=ExecuteRequest('head',lURL,'',aResponseStream);
  Result:=TStreamServiceResult.Create(lResponse);
  Result.Value:=aResponseStream;
end;

Function TBlobsServiceProxy.CreateBlob(aDigest : string; aRequest : TStream; aResponseStream : TStream) : TStreamServiceResult;

const
  lMethodURL = '/api/blobs/{digest}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TStreamServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'digest',aDigest);
  {$IFNDEF VER3_2}
  lResponse:=ExecuteRequest('post',lURL,aRequest,aResponseStream);
  {$ENDIF}
  Result:=TStreamServiceResult.Create(lResponse);
  Result.Value:=aResponseStream;
end;

Function TChatServiceProxy.Complete(aRequest : Tapi_ChatRequest) : Tapi_ChatResponseServiceResult;

const
  lMethodURL = '/api/chat';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(Tapi_ChatResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=Tapi_ChatResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=Tapi_ChatResponse.Deserialize(lResponse.Content);
end;

Function TChatServiceV1Proxy.Complete(aRequest : Tapi_ChatRequest) : Tapi_ChatResponseServiceResult;

const
  lMethodURL = '/v1/chat/completions';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(Tapi_ChatResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=Tapi_ChatResponseServiceResult.Create(lResponse);
  Writeln('----------------- Got : ',lResponse.Content);
  if Result.Success then
    Result.Value:=Tapi_ChatResponse.Deserialize(lResponse.Content);
end;

Function TEmbeddingsServiceProxy.Generate(aRequest : Tapi_EmbedRequest) : Tapi_EmbedResponseServiceResult;

const
  lMethodURL = '/api/embed';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(Tapi_EmbedResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=Tapi_EmbedResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=Tapi_EmbedResponse.Deserialize(lResponse.Content);
end;

Function TEmbeddingsServiceProxy.GenerateCompatibility(aRequest : Tapi_EmbeddingRequest) : Tapi_EmbeddingResponseServiceResult;

const
  lMethodURL = '/api/embeddings';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(Tapi_EmbeddingResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=Tapi_EmbeddingResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=Tapi_EmbeddingResponse.Deserialize(lResponse.Content);
end;

Function TEmbeddingsServiceV1Proxy.Generate(aRequest : Tapi_EmbedRequest) : Tapi_EmbedResponseServiceResult;

const
  lMethodURL = '/v1/embeddings';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(Tapi_EmbedResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=Tapi_EmbedResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=Tapi_EmbedResponse.Deserialize(lResponse.Content);
end;

Function TGenerationServiceProxy.Completion(aRequest : Tapi_GenerateRequest) : stringServiceResult;

const
  lMethodURL = '/api/generate';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(stringServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=stringServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=lResponse.Content;
end;

Function TGenerationServiceProxy.Post_v1_completions(aRequest : Tapi_GenerateRequest) : stringServiceResult;

const
  lMethodURL = '/v1/completions';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(stringServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=stringServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=lResponse.Content;
end;

Function TModelServiceProxy.Copy(aRequest : Tapi_CopyRequest) : stringServiceResult;

const
  lMethodURL = '/api/copy';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(stringServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=stringServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=lResponse.Content;
end;

Function TModelServiceProxy.CreateModel(aRequest : Tapi_CreateRequest) : Tapi_ProgressResponseServiceResult;

const
  lMethodURL = '/api/create';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(Tapi_ProgressResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=Tapi_ProgressResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=Tapi_ProgressResponse.Deserialize(lResponse.Content);
end;

Function TModelServiceProxy.Delete(aRequest : Tapi_DeleteRequest) : stringServiceResult;

const
  lMethodURL = '/api/delete';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(stringServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('delete',lURL,aRequest.Serialize);
  Result:=stringServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=lResponse.Content;
end;

Function TModelServiceProxy.List() : Tapi_ListResponseServiceResult;

const
  lMethodURL = '/api/tags';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(Tapi_ListResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=Tapi_ListResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=Tapi_ListResponse.Deserialize(lResponse.Content);
end;

Function TModelServiceProxy.Pull(aRequest : Tapi_PullRequest) : Tapi_ProgressResponseServiceResult;

const
  lMethodURL = '/api/pull';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(Tapi_ProgressResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=Tapi_ProgressResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=Tapi_ProgressResponse.Deserialize(lResponse.Content);
end;

Function TModelServiceProxy.Push(aRequest : Tapi_PushRequest) : Tapi_ProgressResponseServiceResult;

const
  lMethodURL = '/api/push';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(Tapi_ProgressResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=Tapi_ProgressResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=Tapi_ProgressResponse.Deserialize(lResponse.Content);
end;

Function TModelServiceProxy.Running() : Tapi_ProcessResponseServiceResult;

const
  lMethodURL = '/api/ps';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(Tapi_ProcessResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=Tapi_ProcessResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=Tapi_ProcessResponse.Deserialize(lResponse.Content);
end;

Function TModelServiceProxy.Show(aRequest : Tapi_ShowRequest) : Tapi_ShowResponseServiceResult;

const
  lMethodURL = '/api/show';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(Tapi_ShowResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=Tapi_ShowResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=Tapi_ShowResponse.Deserialize(lResponse.Content);
end;

Function TModelServiceV1Proxy.Get(aModel : string; aRequest : Tapi_ShowRequest) : Tapi_ShowResponseServiceResult;

const
  lMethodURL = '/v1/models/{model}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(Tapi_ShowResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'model',aModel);
  lResponse:=ExecuteRequest('get',lURL,aRequest.Serialize);
  Result:=Tapi_ShowResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=Tapi_ShowResponse.Deserialize(lResponse.Content);
end;

Function TModelServiceV1Proxy.List() : Tapi_ListResponseServiceResult;

const
  lMethodURL = '/v1/models';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(Tapi_ListResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=Tapi_ListResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=Tapi_ListResponse.Deserialize(lResponse.Content);
end;

Function TSystemServiceProxy.Version() : stringServiceResult;

const
  lMethodURL = '/api/version';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(stringServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=stringServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=lResponse.Content;
end;

Function TSystemServiceProxy.Version2() : stringServiceResult;

const
  lMethodURL = '/api/version';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(stringServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('head',lURL,'');
  Result:=stringServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=lResponse.Content;
end;


end.
