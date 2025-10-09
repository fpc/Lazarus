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
{ -----------------------------------------------------------------------
  Google Gemini API Service Interfaces

  This file contains the service interface definitions for Google Gemini API.
  Services include content generation and model listing functionality.

  API Endpoints:
  - POST /v1beta/models/{model}:generateContent
  - GET /v1beta/models
  - GET /v1beta/models/{name}
  -----------------------------------------------------------------------}
unit Gemini.Service.Intf;

{$mode objfpc}
{$h+}

interface

uses
   fpopenapiclient, Gemini.Dto;

Type
  // Service result types
  TGeminiGenerateContentResponseServiceResult = specialize TServiceResult<TGeminiGenerateContentResponse>;
  TGeminiListModelsResponseServiceResult = specialize TServiceResult<TGeminiListModelsResponse>;
  TGeminiModelServiceResult = specialize TServiceResult<TGeminiModel>;

  IGenerateContentService = interface  ['{A1B2C3D4-5E6F-7A8B-9C0D-1E2F3A4B5C6D}']
    Function GenerateContent(aModel : String; aBody : TGeminiGenerateContentRequest) : TGeminiGenerateContentResponseServiceResult;
  end;

  IModelsService = interface  ['{B2C3D4E5-6F7A-8B9C-0D1E-2F3A4B5C6D7E}']
    Function List(aPageSize : Integer = 0; aPageToken : String = '') : TGeminiListModelsResponseServiceResult;
    Function Get(aName : String) : TGeminiModelServiceResult;
  end;

implementation

end.
