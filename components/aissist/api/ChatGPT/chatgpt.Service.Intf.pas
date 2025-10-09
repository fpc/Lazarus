{
    This file is part of the Free Component Library
    Copyright (c) 2025 by Michael Van Canneyt michael@freepascal.org

    ChatGPT Rest API - Service interface definitions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit chatgpt.Service.Intf;

{$mode objfpc}
{$h+}

interface

uses
   classes, fpopenapiclient, chatgpt.Dto;

Type
  // Service result types
  stringServiceResult = specialize TServiceResult<string>;
  TCompletionResponseServiceResult = specialize TServiceResult<TCompletionResponse>;
  TEditResponseServiceResult = specialize TServiceResult<TEditResponse>;
  TEmbeddingsResponseServiceResult = specialize TServiceResult<TEmbeddingsResponse>;
  TDeleteFileResponseServiceResult = specialize TServiceResult<TDeleteFileResponse>;
  TFileResponseServiceResult = specialize TServiceResult<TFileResponse>;
  TRetrieveFileResponseServiceResult = specialize TServiceResult<TRetrieveFileResponse>;
  TUploadFileResponseServiceResult = specialize TServiceResult<TUploadFileResponse>;
  TDeleteFineTuneResponseServiceResult = specialize TServiceResult<TDeleteFineTuneResponse>;
  TGetFineTunesResponseServiceResult = specialize TServiceResult<TGetFineTunesResponse>;
  TGetFineTuneResponseServiceResult = specialize TServiceResult<TGetFineTuneResponse>;
  TFineTuneEventsResponseServiceResult = specialize TServiceResult<TFineTuneEventsResponse>;
  TFineTunesResponseServiceResult = specialize TServiceResult<TFineTunesResponse>;
  TImagesResponseServiceResult = specialize TServiceResult<TImagesResponse>;
  TModelsServiceResult = specialize TServiceResult<TModels>;
  TModelServiceResult = specialize TServiceResult<TModel>;
  TModerationResponseServiceResult = specialize TServiceResult<TModerationResponse>;
  
  // Service ICompletionsService
  
  ICompletionsService = interface  ['{8E9CB642-AC6F-4940-8461-DE159479BB2A}']
    Function Post_completions(aRequest : TCompletionRequest) : TCompletionResponseServiceResult;
  end;
  
  // Service IEditsService
  
  IEditsService = interface  ['{F22A7330-221D-4DEF-A5E7-394780D11C8B}']
    Function Post_edits(aRequest : TEditRequest) : TEditResponseServiceResult;
  end;
  
  // Service IEmbeddingsService
  
  IEmbeddingsService = interface  ['{8FF9C34A-5AAD-4318-9F8A-77189C6984CD}']
    Function Post_embeddings(aRequest : TEmbeddingsRequest) : TEmbeddingsResponseServiceResult;
  end;
  
  // Service IFilesService
  
  IFilesService = interface  ['{2FEF020A-8DF1-4F2A-958D-7F332298DDC2}']
    Function Delete_files_file_id(aFile_id : string) : TDeleteFileResponseServiceResult;
    Function Get_files() : TFileResponseServiceResult;
    Function Get_files_file_id(aFile_id : string) : TRetrieveFileResponseServiceResult;
    Function Get_files_file_id_content(aFile_id : string) : stringServiceResult;
    Function Post_files(aRequest : TUploadFileRequest) : TUploadFileResponseServiceResult;
  end;
  
  // Service IFine_tunesService
  
  IFine_tunesService = interface  ['{5BBF9613-4799-4667-8492-DDBD5A2B2F7A}']
    Function Delete_models_model(aModel : string) : TDeleteFineTuneResponseServiceResult;
    Function Get_fine_tunes() : TGetFineTunesResponseServiceResult;
    Function Get_fine_tunes_fine_tune_id(aFine_tune_id : string) : TGetFineTuneResponseServiceResult;
    Function Get_fine_tunes_fine_tune_id_events(aFine_tune_id : string; aStream : boolean) : TFineTuneEventsResponseServiceResult;
    Function Post_fine_tunes(aRequest : TFineTunesRequest) : TFineTunesResponseServiceResult;
    Function Post_fine_tunes_fine_tune_id_cancel(aFine_tune_id : string) : TFineTunesResponseServiceResult;
  end;
  
  // Service IImagesService
  
  IImagesService = interface  ['{C21F2BC2-D92B-478F-8869-03200CE59B12}']
    Function Post_images_edits(aRequest : TImageEditRequest) : TImagesResponseServiceResult;
    Function Post_images_generations(aRequest : TCreateImagesRequest) : TImagesResponseServiceResult;
    Function Post_images_variations(aRequest : TImageValidationRequest) : TImagesResponseServiceResult;
  end;
  
  // Service IModelsService
  
  IModelsService = interface  ['{399FDC09-1AE0-4E90-A723-533D771F40AF}']
    Function Get_models() : TModelsServiceResult;
    Function Get_models_model(aModel : string) : TModelServiceResult;
  end;
  
  // Service IModerationsService
  
  IModerationsService = interface  ['{CA2C83CC-9B0C-4C40-BB57-E76461097114}']
    Function Post_moderations(aRequest : TModerationRequest) : TModerationResponseServiceResult;
  end;
  

implementation

end.
