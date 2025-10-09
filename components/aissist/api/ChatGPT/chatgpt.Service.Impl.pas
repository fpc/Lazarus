{
    This file is part of the Free Component Library
    Copyright (c) 2025 by Michael Van Canneyt michael@freepascal.org

    ChatGPT Rest API - Service interface implementations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit chatgpt.Service.Impl;

{$mode objfpc}
{$h+}

interface

uses
  classes, fpopenapiclient
  , chatgpt.Service.Intf                     // Service definition 
  , chatgpt.Dto;

Type
  // Service ICompletionsService
  
  TCompletionsServiceProxy = Class (TFPOpenAPIServiceClient,ICompletionsService)
    Function Post_completions(aRequest : TCompletionRequest) : TCompletionResponseServiceResult;
  end;
  
  // Service IEditsService
  
  TEditsServiceProxy = Class (TFPOpenAPIServiceClient,IEditsService)
    Function Post_edits(aRequest : TEditRequest) : TEditResponseServiceResult;
  end;
  
  // Service IEmbeddingsService
  
  TEmbeddingsServiceProxy = Class (TFPOpenAPIServiceClient,IEmbeddingsService)
    Function Post_embeddings(aRequest : TEmbeddingsRequest) : TEmbeddingsResponseServiceResult;
  end;
  
  // Service IFilesService
  
  TFilesServiceProxy = Class (TFPOpenAPIServiceClient,IFilesService)
    Function Delete_files_file_id(aFile_id : string) : TDeleteFileResponseServiceResult;
    Function Get_files() : TFileResponseServiceResult;
    Function Get_files_file_id(aFile_id : string) : TRetrieveFileResponseServiceResult;
    Function Get_files_file_id_content(aFile_id : string) : stringServiceResult;
    Function Post_files(aRequest : TUploadFileRequest) : TUploadFileResponseServiceResult;
  end;
  
  // Service IFine_tunesService
  
  TFine_tunesServiceProxy = Class (TFPOpenAPIServiceClient,IFine_tunesService)
    Function Delete_models_model(aModel : string) : TDeleteFineTuneResponseServiceResult;
    Function Get_fine_tunes() : TGetFineTunesResponseServiceResult;
    Function Get_fine_tunes_fine_tune_id(aFine_tune_id : string) : TGetFineTuneResponseServiceResult;
    Function Get_fine_tunes_fine_tune_id_events(aFine_tune_id : string; aStream : boolean) : TFineTuneEventsResponseServiceResult;
    Function Post_fine_tunes(aRequest : TFineTunesRequest) : TFineTunesResponseServiceResult;
    Function Post_fine_tunes_fine_tune_id_cancel(aFine_tune_id : string) : TFineTunesResponseServiceResult;
  end;
  
  // Service IImagesService
  
  TImagesServiceProxy = Class (TFPOpenAPIServiceClient,IImagesService)
    Function Post_images_edits(aRequest : TImageEditRequest) : TImagesResponseServiceResult;
    Function Post_images_generations(aRequest : TCreateImagesRequest) : TImagesResponseServiceResult;
    Function Post_images_variations(aRequest : TImageValidationRequest) : TImagesResponseServiceResult;
  end;
  
  // Service IModelsService
  
  TModelsServiceProxy = Class (TFPOpenAPIServiceClient,IModelsService)
    Function Get_models() : TModelsServiceResult;
    Function Get_models_model(aModel : string) : TModelServiceResult;
  end;
  
  // Service IModerationsService
  
  TModerationsServiceProxy = Class (TFPOpenAPIServiceClient,IModerationsService)
    Function Post_moderations(aRequest : TModerationRequest) : TModerationResponseServiceResult;
  end;
  

implementation

uses
  SysUtils
  , chatgpt.Serializer;

Function TCompletionsServiceProxy.Post_completions(aRequest : TCompletionRequest) : TCompletionResponseServiceResult;

const
  lMethodURL = '/completions';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TCompletionResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TCompletionResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TCompletionResponse.Deserialize(lResponse.Content);
end;

Function TEditsServiceProxy.Post_edits(aRequest : TEditRequest) : TEditResponseServiceResult;

const
  lMethodURL = '/edits';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TEditResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TEditResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TEditResponse.Deserialize(lResponse.Content);
end;

Function TEmbeddingsServiceProxy.Post_embeddings(aRequest : TEmbeddingsRequest) : TEmbeddingsResponseServiceResult;

const
  lMethodURL = '/embeddings';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TEmbeddingsResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TEmbeddingsResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TEmbeddingsResponse.Deserialize(lResponse.Content);
end;

Function TFilesServiceProxy.Delete_files_file_id(aFile_id : string) : TDeleteFileResponseServiceResult;

const
  lMethodURL = '/files/{file_id}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TDeleteFileResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'file_id',aFile_id);
  lResponse:=ExecuteRequest('delete',lURL,'');
  Result:=TDeleteFileResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TDeleteFileResponse.Deserialize(lResponse.Content);
end;

Function TFilesServiceProxy.Get_files() : TFileResponseServiceResult;

const
  lMethodURL = '/files';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TFileResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TFileResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TFileResponse.Deserialize(lResponse.Content);
end;

Function TFilesServiceProxy.Get_files_file_id(aFile_id : string) : TRetrieveFileResponseServiceResult;

const
  lMethodURL = '/files/{file_id}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TRetrieveFileResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'file_id',aFile_id);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TRetrieveFileResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TRetrieveFileResponse.Deserialize(lResponse.Content);
end;

Function TFilesServiceProxy.Get_files_file_id_content(aFile_id : string) : stringServiceResult;

const
  lMethodURL = '/files/{file_id}/content';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(stringServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'file_id',aFile_id);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=stringServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=lResponse.Content;
end;

Function TFilesServiceProxy.Post_files(aRequest : TUploadFileRequest) : TUploadFileResponseServiceResult;

const
  lMethodURL = '/files';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TUploadFileResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TUploadFileResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TUploadFileResponse.Deserialize(lResponse.Content);
end;

Function TFine_tunesServiceProxy.Delete_models_model(aModel : string) : TDeleteFineTuneResponseServiceResult;

const
  lMethodURL = '/models/{model}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TDeleteFineTuneResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'model',aModel);
  lResponse:=ExecuteRequest('delete',lURL,'');
  Result:=TDeleteFineTuneResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TDeleteFineTuneResponse.Deserialize(lResponse.Content);
end;

Function TFine_tunesServiceProxy.Get_fine_tunes() : TGetFineTunesResponseServiceResult;

const
  lMethodURL = '/fine-tunes';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TGetFineTunesResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TGetFineTunesResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TGetFineTunesResponse.Deserialize(lResponse.Content);
end;

Function TFine_tunesServiceProxy.Get_fine_tunes_fine_tune_id(aFine_tune_id : string) : TGetFineTuneResponseServiceResult;

const
  lMethodURL = '/fine-tunes/{fine_tune_id}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TGetFineTuneResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'fine_tune_id',aFine_tune_id);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TGetFineTuneResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TGetFineTuneResponse.Deserialize(lResponse.Content);
end;

Function TFine_tunesServiceProxy.Get_fine_tunes_fine_tune_id_events(aFine_tune_id : string; aStream : boolean) : TFineTuneEventsResponseServiceResult;

const
  lMethodURL = '/fine-tunes/{fine_tune_id}/events';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TFineTuneEventsResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'fine_tune_id',aFine_tune_id);
  lQuery:=ConcatRestParam(lQuery,'stream',cRESTBooleans[aStream]);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TFineTuneEventsResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TFineTuneEventsResponse.Deserialize(lResponse.Content);
end;

Function TFine_tunesServiceProxy.Post_fine_tunes(aRequest : TFineTunesRequest) : TFineTunesResponseServiceResult;

const
  lMethodURL = '/fine-tunes';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TFineTunesResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TFineTunesResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TFineTunesResponse.Deserialize(lResponse.Content);
end;

Function TFine_tunesServiceProxy.Post_fine_tunes_fine_tune_id_cancel(aFine_tune_id : string) : TFineTunesResponseServiceResult;

const
  lMethodURL = '/fine-tunes/{fine_tune_id}/cancel';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TFineTunesResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'fine_tune_id',aFine_tune_id);
  lResponse:=ExecuteRequest('post',lURL,'');
  Result:=TFineTunesResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TFineTunesResponse.Deserialize(lResponse.Content);
end;

Function TImagesServiceProxy.Post_images_edits(aRequest : TImageEditRequest) : TImagesResponseServiceResult;

const
  lMethodURL = '/images/edits';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TImagesResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TImagesResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TImagesResponse.Deserialize(lResponse.Content);
end;

Function TImagesServiceProxy.Post_images_generations(aRequest : TCreateImagesRequest) : TImagesResponseServiceResult;

const
  lMethodURL = '/images/generations';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TImagesResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TImagesResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TImagesResponse.Deserialize(lResponse.Content);
end;

Function TImagesServiceProxy.Post_images_variations(aRequest : TImageValidationRequest) : TImagesResponseServiceResult;

const
  lMethodURL = '/images/variations';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TImagesResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TImagesResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TImagesResponse.Deserialize(lResponse.Content);
end;

Function TModelsServiceProxy.Get_models() : TModelsServiceResult;

const
  lMethodURL = '/models';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TModelsServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TModelsServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TModels.Deserialize(lResponse.Content);
end;

Function TModelsServiceProxy.Get_models_model(aModel : string) : TModelServiceResult;

const
  lMethodURL = '/models/{model}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TModelServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'model',aModel);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TModelServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TModel.Deserialize(lResponse.Content);
end;

Function TModerationsServiceProxy.Post_moderations(aRequest : TModerationRequest) : TModerationResponseServiceResult;

const
  lMethodURL = '/moderations';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TModerationResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TModerationResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TModerationResponse.Deserialize(lResponse.Content);
end;


end.
