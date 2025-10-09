{
    This file is part of the Free Component Library
    Copyright (c) 2025 by Michael Van Canneyt michael@freepascal.org

    Gemini Rest API - Service interface implementations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Gemini.Service.Impl;

{$mode objfpc}
{$h+}

interface

uses
  fpopenapiclient
  , Gemini.Service.Intf                     // Service definition
  , Gemini.Dto
  , Gemini.Persister                        // Serialization helpers
  , fpwebclient                             // Web client for HTTP requests
  , Classes, SysUtils;

Type
  TGenerateContentServiceProxy = Class (TFPOpenAPIServiceClient,IGenerateContentService)
  public
    Function GenerateContent(aModel : String; aBody : TGeminiGenerateContentRequest) : TGeminiGenerateContentResponseServiceResult;
  end;

  TModelsServiceProxy = Class (TFPOpenAPIServiceClient,IModelsService)
  public
    Function List(aPageSize : Integer = 0; aPageToken : String = '') : TGeminiListModelsResponseServiceResult;
    Function Get(aName : String) : TGeminiModelServiceResult;
  end;

implementation

{ TGenerateContentServiceProxy }

Function TGenerateContentServiceProxy.GenerateContent(aModel : String; aBody : TGeminiGenerateContentRequest) : TGeminiGenerateContentResponseServiceResult;
var
  URL: string;
  RequestBody: string;
  ServiceResponse: TServiceResponse;
begin
  URL := BuildEndPointURL('/v1beta/models/' + aModel + ':generateContent');
  RequestBody := aBody.Serialize;
  ServiceResponse := ExecuteRequest('POST', URL, RequestBody);
  Result := TGeminiGenerateContentResponseServiceResult.Create(ServiceResponse);
  if Result.Success then
    begin
    try
      Result.Value := TGeminiGenerateContentResponse.Deserialize(ServiceResponse.Content);
    except
      on E: Exception do
        begin
        Result.ErrorCode := -1;
        Result.ErrorText := 'Failed to deserialize response: ' + E.Message;
        end;
    end;
    end;
end;

{ TModelsServiceProxy }

Function TModelsServiceProxy.List(aPageSize : Integer = 0; aPageToken : String = '') : TGeminiListModelsResponseServiceResult;
var
  URL: string;
  QueryParams: string;
  ServiceResponse: TServiceResponse;
begin
  URL := BuildEndPointURL('/v1beta/models');
  QueryParams := '';
  if aPageSize > 0 then
    QueryParams := ConcatRestParam(QueryParams, 'pageSize', IntToStr(aPageSize));
  if aPageToken <> '' then
    QueryParams := ConcatRestParam(QueryParams, 'pageToken', aPageToken);
  if QueryParams <> '' then
    URL := URL + '?' + QueryParams;
  ServiceResponse := ExecuteRequest('GET', URL, '');
  Result := TGeminiListModelsResponseServiceResult.Create(ServiceResponse);
    begin
    try
      Result.Value := TGeminiListModelsResponse.Deserialize(ServiceResponse.Content);
    except
      on E: Exception do
        begin
        Result.ErrorCode := -1;
        Result.ErrorText := 'Failed to deserialize response: ' + E.Message;
        end;
    end;
    end;
end;

Function TModelsServiceProxy.Get(aName : String) : TGeminiModelServiceResult;
var
  URL: string;
  ServiceResponse: TServiceResponse;
begin
  if Pos('models/', aName) = 1 then
    URL := BuildEndPointURL('/v1beta/' + aName)
  else
    URL := BuildEndPointURL('/v1beta/models/' + aName);
  ServiceResponse := ExecuteRequest('GET', URL, '');
  Result := TGeminiModelServiceResult.Create(ServiceResponse);
  if Result.Success then
    begin
    try
      Result.Value := TGeminiModel.Deserialize(ServiceResponse.Content);
    except
      on E: Exception do
        begin
        Result.ErrorCode := -1;
        Result.ErrorText := 'Failed to deserialize response: ' + E.Message;
        end;
    end;
    end;
end;

end.
