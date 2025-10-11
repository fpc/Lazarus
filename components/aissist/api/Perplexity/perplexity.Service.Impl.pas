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
unit perplexity.Service.Impl;

{$mode objfpc}
{$h+}

interface

uses
  classes, fpopenapiclient
  , perplexity.Service.Intf                     // Service definition
  , perplexity.Dto;

Type
  // Service ICompletionsService
  
  TCompletionsServiceProxy = Class (TFPOpenAPIServiceClient,ICompletionsService)
    Function Post_completions(aRequest : TCompletionRequest) : TCompletionResponseServiceResult;
  end;
  
implementation

uses
  SysUtils
  , perplexity.Serializer;

Function TCompletionsServiceProxy.Post_completions(aRequest : TCompletionRequest) : TCompletionResponseServiceResult;

const
  lMethodURL = '/chat/completions';

var
  lURL : String;
  lBody: String;
  lResponse : TServiceResponse;
begin
  Result:=Default(TCompletionResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lBody:= aRequest.Serialize;
  //WriteLn('lBody:', lBody);
  lResponse:=ExecuteRequest('post',lURL,lBody);
  //WriteLn('lResponse:', lResponse.Content);
  Result:=TCompletionResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TCompletionResponse.Deserialize(lResponse.Content);
end;


end.
