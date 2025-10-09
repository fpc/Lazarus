{
    This file is part of the Free Component Library
    Copyright (c) 2025 by Michael Van Canneyt michael@freepascal.org

    Gemini Rest API - Service interface definitions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit ollama.Service.Intf;

{$mode objfpc}
{$h+}

interface

uses
   classes, fpopenapiclient, ollama.Dto;

Type
  // Service result types
  stringServiceResult = specialize TServiceResult<string>;
  Tapi_ChatResponseServiceResult = specialize TServiceResult<Tapi_ChatResponse>;
  Tapi_EmbeddingResponseServiceResult = specialize TServiceResult<Tapi_EmbeddingResponse>;
  Tapi_EmbedResponseServiceResult = specialize TServiceResult<Tapi_EmbedResponse>;
  Tapi_ListResponseServiceResult = specialize TServiceResult<Tapi_ListResponse>;
  Tapi_ProcessResponseServiceResult = specialize TServiceResult<Tapi_ProcessResponse>;
  Tapi_ProgressResponseServiceResult = specialize TServiceResult<Tapi_ProgressResponse>;
  Tapi_ShowResponseServiceResult = specialize TServiceResult<Tapi_ShowResponse>;
  TStreamServiceResult = specialize TServiceResult<TStream>;
  
  // Service IBlobsService
  
  IBlobsService = interface  ['{CD92ED4C-CDD4-4579-B6FD-F673929D04EC}']
    Function Check(aDigest : string; aResponseStream : TStream) : TStreamServiceResult;
    Function CreateBlob(aDigest : string; aRequest : TStream; aResponseStream : TStream) : TStreamServiceResult;
  end;
  
  // Service IChatService
  
  IChatService = interface  ['{E1BA040F-DCF9-4FE4-BD69-54F538C9795B}']
    Function Complete(aRequest : Tapi_ChatRequest) : Tapi_ChatResponseServiceResult;
  end;
  
  // Service IChatServiceV1
  
  IChatServiceV1 = interface  ['{F2A37CF9-A3BD-4458-AA6D-2186B27D3DAA}']
    Function Complete(aRequest : Tapi_ChatRequest) : Tapi_ChatResponseServiceResult;
  end;
  
  // Service IEmbeddingsService
  
  IEmbeddingsService = interface  ['{525B917D-B55A-494B-876E-EEA04B46C505}']
    Function Generate(aRequest : Tapi_EmbedRequest) : Tapi_EmbedResponseServiceResult;
    Function GenerateCompatibility(aRequest : Tapi_EmbeddingRequest) : Tapi_EmbeddingResponseServiceResult;
  end;
  
  // Service IEmbeddingsServiceV1
  
  IEmbeddingsServiceV1 = interface  ['{B34460BF-D6D1-445C-834B-A984BE5AE7B3}']
    Function Generate(aRequest : Tapi_EmbedRequest) : Tapi_EmbedResponseServiceResult;
  end;
  
  // Service IGenerationService
  
  IGenerationService = interface  ['{F218329B-41C8-46A0-BE2B-7A6D2D8F7BBF}']
    Function Completion(aRequest : Tapi_GenerateRequest) : stringServiceResult;
    Function Post_v1_completions(aRequest : Tapi_GenerateRequest) : stringServiceResult;
  end;
  
  // Service IModelService
  
  IModelService = interface  ['{E841C7A5-D150-4C22-BA2B-52760E4309F6}']
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
  
  IModelServiceV1 = interface  ['{7354ABFD-0F3A-4967-BB62-2311ECF68A8D}']
    Function Get(aModel : string; aRequest : Tapi_ShowRequest) : Tapi_ShowResponseServiceResult;
    Function List() : Tapi_ListResponseServiceResult;
  end;
  
  // Service ISystemService
  
  ISystemService = interface  ['{F8D11985-500C-4C09-B05C-B14E8B2500FC}']
    Function Version() : stringServiceResult;
    Function Version2() : stringServiceResult;
  end;
  

implementation

end.
