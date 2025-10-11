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
unit perplexity.Service.Intf;

{$mode objfpc}
{$h+}

interface

uses
   classes, fpopenapiclient, perplexity.Dto;

Type
  // Service result types
  stringServiceResult = specialize TServiceResult<string>;
  TCompletionResponseServiceResult = specialize TServiceResult<TCompletionResponse>;

  // Service ICompletionsService
  
  ICompletionsService = interface  ['{027C8810-5828-476A-B9D0-CBBCDC500AD0}']
    Function Post_completions(aRequest : TCompletionRequest) : TCompletionResponseServiceResult;
  end;
  

implementation

end.
