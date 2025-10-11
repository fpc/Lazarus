{
    This file is part of the Free Component Library
    Copyright (c) 2025 by Michael Van Canneyt michael@freepascal.org

    ChatGPT Rest API -  Data transfer objects

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit perplexity.Dto;

{$mode objfpc}
{$h+}


interface

uses types;

Type

  stringArray = Array of string;
  
  TNested_messages_items
  =
   Class(TObject)
     role   : string;
     content: string;
   end;
  TNested_messages_items_array= array of TNested_messages_items;

  { TCompletionRequest }

  TCompletionRequest = Class(TObject)
    model : string;
    messages: TNested_messages_items_array;
    frequency_penalty : double;
    max_tokens : integer;
    presence_penalty : double;
    stream : boolean;
    temperature : double;
    top_p : double;
    destructor Destroy; override;
    procedure Add_Message( _role, _content: String);
    function List_Messages: String;
  end;
  
  TNested_choices_items = Class(TObject)
    message: TNested_messages_items;
    delta: TNested_messages_items;
    index : integer;
    logprobs : string;
    finish_reason : string;
  end;
  TNested_choices_itemsArray = Array of TNested_choices_items;

  TCompletionResponse_usage_cost
  =
   class(TObject)
     input_tokens_cost : double;
     output_tokens_cost: double;
     request_cost      : double;
     total_cost        : double;
   end;

  TCompletionResponse_usage
  =
   class(TObject)
     prompt_tokens : integer;
     completion_tokens : integer;
     total_tokens : integer;
     search_context_size: string;
     cost: TCompletionResponse_usage_cost;
   end;

  { TCompletionResponse_search_results_item }

  TCompletionResponse_search_results_item
  =
   class(TObject)
     title: String;
     url: String;
     date: string;
     last_updated: string;
     snippet: String;
     source: String;
     function List: String;
   end;

  TCompletionResponse_search_results_item_array
  =
   array of TCompletionResponse_search_results_item;

  { TCompletionResponse }

  TCompletionResponse = Class(TObject)
    id : string;
    model : string;
    created : integer;
    usage : TCompletionResponse_usage;
    citations: stringArray;
    search_results: TCompletionResponse_search_results_item_array;
    object_ : string;
    choices : TNested_choices_itemsArray;
    destructor destroy; override;
    function List_Citations: String;
    function List_Search_Results: String;
  end;

  TError_error = Class(TObject)
    message : string;
    type_ : string;
    param : string;
    code : string;
  end;
  
  { TError }

  TError = Class(TObject)
    error : TError_error;
    destructor destroy; override;
  end;
  
  TNested_permission_items = Class(TObject)
    id : string;
    object_ : string;
    created : double;
    allow_create_engine : boolean;
    allow_sampling : boolean;
    allow_logprobs : boolean;
    allow_search_indices : boolean;
    allow_view : boolean;
    allow_fine_tuning : boolean;
    organization : string;
    group : string;
    is_blocking : boolean;
  end;
  TNested_permission_itemsArray = Array of TNested_permission_items;

implementation

uses sysutils;

{ TCompletionRequest }

destructor TCompletionRequest.Destroy;
var
   message: TNested_messages_items;
begin
     for message in messages do message.Free;
     SetLength( messages, 0);

     inherited Destroy;
end;

procedure TCompletionRequest.Add_Message( _role, _content: String);
var
   message: TNested_messages_items;
begin
     message:= TNested_messages_items.Create;
     try
        message.role   := _role   ;
        message.content:= _content;
     except
           message.Free;
           raise
           end;
     SetLength( messages, Length(messages)+1);
     messages[High(messages)]:= message;
end;

function TCompletionRequest.List_Messages: String;
var
   message: TNested_messages_items;
begin
     Result:= '';
     for message in messages
     do
       Result:= Result+'role:'+message.role+', content:'+message.content+LineEnding;
end;

{ TCompletionResponse_search_results_item }

function TCompletionResponse_search_results_item.List: String;
begin
     Result
     :=
       'title       :'+title       +LineEnding+
       'url         :'+url         +LineEnding+
       'date        :'+date        +LineEnding+
       'last_updated:'+last_updated+LineEnding+
       'snippet     :'+snippet     +LineEnding+
       'source      :'+source      +LineEnding;
end;

{ TCompletionResponse }

destructor TCompletionResponse.destroy;
var
  i : Integer;
begin
  for I:=0 to Length(Choices)-1 do
    FreeAndNil(Choices[i]);
  FreeAndNil(usage);
  inherited destroy;
end;

function TCompletionResponse.List_Citations: String;
var
   i: Integer;
   s: String;
begin
     Result:= 'Citations:'+LineEnding;
     for i:= Low( citations) to High( citations)
     do
       Result:= Result + '['+IntToStr(i+1)+'] '+citations[i]+LineEnding;
end;

function TCompletionResponse.List_Search_Results: String;
var
   sr: TCompletionResponse_search_results_item;
begin
     Result:= 'Search results:'+LineEnding;
     for sr in search_results
     do
       Result:= Result + sr.List+LineEnding;
end;

{ TError }

destructor TError.destroy;
begin
  FreeAndNil(error);
  inherited destroy;
end;

end.
