{
    This file is part of the Free Component Library
    Copyright (c) 2025 by Michael Van Canneyt michael@freepascal.org

    ChatGPT Rest API - Data transfer object serializers

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit perplexity.Serializer;

interface

{$mode objfpc}
{$h+}
{$modeswitch typehelpers}


uses
  Types,
  fpJSON,
  perplexity.Dto;

Type

  { TNested_messages_itemsSerializer }

  TNested_messages_itemsSerializer = class helper for TNested_messages_items
    class function Deserialize(aJSON : TJSONObject) : TNested_messages_items; overload; static;
    class function Deserialize(aJSON : String) : TNested_messages_items; overload; static;
  end;

  TCompletionRequestSerializer = class helper for TCompletionRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
  end;
  
  TNested_choices_itemsSerializer = class helper for TNested_choices_items
    class function Deserialize(aJSON : TJSONObject) : TNested_choices_items; overload; static;
    class function Deserialize(aJSON : String) : TNested_choices_items; overload; static;
  end;
  
  { TCompletionResponse_usage_costSerializer }

  TCompletionResponse_usage_costSerializer = class helper for TCompletionResponse_usage_cost
    class function Deserialize(aJSON : TJSONObject) : TCompletionResponse_usage_cost; overload; static;
    class function Deserialize(aJSON : String) : TCompletionResponse_usage_cost; overload; static;
  end;
  
  TCompletionResponse_usageSerializer = class helper for TCompletionResponse_usage
    class function Deserialize(aJSON : TJSONObject) : TCompletionResponse_usage; overload; static;
    class function Deserialize(aJSON : String) : TCompletionResponse_usage; overload; static;
  end;

  { TCompletionResponse_search_results_itemSerializer }

  TCompletionResponse_search_results_itemSerializer = class helper for TCompletionResponse_search_results_item
    class function Deserialize(aJSON : TJSONObject) : TCompletionResponse_search_results_item; overload; static;
    class function Deserialize(aJSON : String) : TCompletionResponse_search_results_item; overload; static;
  end;

  TCompletionResponseSerializer = class helper for TCompletionResponse
    class function Deserialize(aJSON : TJSONObject) : TCompletionResponse; overload; static;
    class function Deserialize(aJSON : String) : TCompletionResponse; overload; static;
  end;
  
  TError_errorSerializer = class helper for TError_error
    class function Deserialize(aJSON : TJSONObject) : TError_error; overload; static;
    class function Deserialize(aJSON : String) : TError_error; overload; static;
  end;
  
  TErrorSerializer = class helper for TError
    class function Deserialize(aJSON : TJSONObject) : TError; overload; static;
    class function Deserialize(aJSON : String) : TError; overload; static;
  end;
  
  TNested_permission_itemsSerializer = class helper for TNested_permission_items
    class function Deserialize(aJSON : TJSONObject) : TNested_permission_items; overload; static;
    class function Deserialize(aJSON : String) : TNested_permission_items; overload; static;
  end;
  
implementation

uses Generics.Collections, SysUtils, DateUtils, StrUtils;

function ISO8601ToDateDef(S: String; aDefault : TDateTime; aConvertUTC: Boolean = True) : TDateTime;

begin
  if (S='') then
    Exit(aDefault);
  try
    Result:=ISO8601ToDate(S,aConvertUTC);
  except
    Result:=aDefault;
  end;
end;

{ TNested_messages_itemsSerializer }

class function TNested_messages_itemsSerializer.Deserialize( aJSON: TJSONObject): TNested_messages_items;
begin
     Result := TNested_messages_items.Create;
     If (aJSON=Nil) then exit;

     Result.role   :=aJSON.Get('role'   ,'');
     Result.content:=aJSON.Get('content','');
end;

class function TNested_messages_itemsSerializer.Deserialize( aJSON: String): TNested_messages_items;
var
  lObj : TJSONObject;
begin
     Result := Default(TNested_messages_items);
     if (aJSON='') then exit;

     lObj := GetJSON(aJSON) as TJSONObject;
     if (lObj = nil) then exit;

     try
        Result:=Deserialize(lObj);
     finally
            lObj.Free
            end;
end;


function TCompletionRequestSerializer.SerializeObject : TJSONObject;
   procedure Add_messages;
   var
      json_messages: TJSONArray;
      message: TNested_messages_items;
      json_message: TJSONObject;
   begin
       json_messages:= TJSONArray.Create;
       for message in messages
       do
         begin
         json_message:= TJSONObject.Create;
         json_message.Add('role'   , message.role   );
         json_message.Add('content', message.content);
         json_messages.Add( json_message);
         end;
       Result.Arrays['messages']:= json_messages;
   end;
begin
     Result:=TJSONObject.Create;
     try
        Result.Add('model',model);

        Add_messages;

        Result.Add('frequency_penalty',frequency_penalty);
        Result.Add('max_tokens',max_tokens);
        Result.Add('presence_penalty',presence_penalty);
        Result.Add('stream',stream);
        Result.Add('temperature',temperature);
        Result.Add('top_p',top_p);
     except
           Result.Free;
           raise;
     end;
end;

function TCompletionRequestSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TError_errorSerializer.Deserialize(aJSON : TJSONObject) : TError_error;

begin
  Result := TError_error.Create;
  If (aJSON=Nil) then
    exit;
  Result.message:=aJSON.Get('message','');
  Result.type_:=aJSON.Get('type','');
  Result.param:=aJSON.Get('param','');
  Result.code:=aJSON.Get('code','');
end;

class function TError_errorSerializer.Deserialize(aJSON : String) : TError_error;

var
  lObj : TJSONObject;
begin
  Result := Default(TError_error);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

class function TErrorSerializer.Deserialize(aJSON : TJSONObject) : TError;

begin
  Result := TError.Create;
  If (aJSON=Nil) then
    exit;
  Result.error:=TError_error.Deserialize(aJSON.Get('error',TJSONObject(Nil)));
end;

class function TErrorSerializer.Deserialize(aJSON : String) : TError;

var
  lObj : TJSONObject;
begin
  Result := Default(TError);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

class function TNested_choices_itemsSerializer.Deserialize(aJSON : TJSONObject) : TNested_choices_items;
begin
  Result := TNested_choices_items.Create;
  If (aJSON=Nil) then
    exit;
  Result.message:= TNested_messages_items.Deserialize( aJSON.Get('message',TJSONObject(Nil)));
  Result.delta:= TNested_messages_items.Deserialize( aJSON.Get('delta',TJSONObject(Nil)));
  Result.index:=aJSON.Get('index',0);
  Result.logprobs:=aJSON.Get('logprobs','');
  Result.finish_reason:=aJSON.Get('finish_reason','');
end;

class function TNested_choices_itemsSerializer.Deserialize(aJSON : String) : TNested_choices_items;
var
  lObj : TJSONObject;
begin
  Result := Default(TNested_choices_items);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

{ TCompletionResponse_usage_costSerializer }

class function TCompletionResponse_usage_costSerializer.Deserialize( aJSON: TJSONObject): TCompletionResponse_usage_cost;
begin
     Result := TCompletionResponse_usage_cost.Create;
     If (aJSON=Nil) then exit;

     Result.input_tokens_cost := aJSON.Get('input_tokens_cost' ,0);
     Result.output_tokens_cost:= aJSON.Get('output_tokens_cost',0);
     Result.request_cost      := aJSON.Get('request_cost'      ,0);
     Result.total_cost        := aJSON.Get('total_cost'        ,0);
end;

class function TCompletionResponse_usage_costSerializer.Deserialize( aJSON: String): TCompletionResponse_usage_cost;
var
   lObj : TJSONObject;
begin
     Result := Default(TCompletionResponse_usage_cost);
     if (aJSON='') then exit;

     lObj := GetJSON(aJSON) as TJSONObject;
     if (lObj = nil) then exit;

     try
        Result:=Deserialize(lObj);
     finally
            lObj.Free
            end;
end;

class function TCompletionResponse_usageSerializer.Deserialize(aJSON : TJSONObject) : TCompletionResponse_usage;
begin
     Result := TCompletionResponse_usage.Create;
     If (aJSON=Nil) then exit;

     Result.prompt_tokens      :=aJSON.Get('prompt_tokens',0);
     Result.completion_tokens  :=aJSON.Get('completion_tokens',0);
     Result.total_tokens       :=aJSON.Get('total_tokens',0);
     Result.search_context_size:=aJSON.Get('search_context_size','');
     Result.cost:= TCompletionResponse_usage_cost.Deserialize( aJSON.Get('cost',TJSONObject(Nil)));
end;

class function TCompletionResponse_usageSerializer.Deserialize(aJSON : String) : TCompletionResponse_usage;
var
   lObj : TJSONObject;
begin
     Result := Default(TCompletionResponse_usage);
     if (aJSON='') then exit;

     lObj := GetJSON(aJSON) as TJSONObject;
     if (lObj = nil) then exit;

     try
        Result:=Deserialize(lObj);
     finally
            lObj.Free
            end;
end;

{ TCompletionResponse_search_results_itemSerializer }

class function TCompletionResponse_search_results_itemSerializer.Deserialize(
 aJSON: TJSONObject): TCompletionResponse_search_results_item;
begin
     Result := TCompletionResponse_search_results_item.Create;
     If (aJSON=Nil) then exit;

     Result.title       := aJSON.Get( 'title'       ,'');
     Result.url         := aJSON.Get( 'url'         ,'');
     Result.date        := aJSON.Get( 'date'        ,'');
     Result.last_updated:= aJSON.Get( 'last_updated','');
     Result.snippet     := aJSON.Get( 'snippet'     ,'');
     Result.source      := aJSON.Get( 'source'      ,'');
end;

class function TCompletionResponse_search_results_itemSerializer.Deserialize(
 aJSON: String): TCompletionResponse_search_results_item;
var
  lObj : TJSONObject;
begin
  Result := Default(TCompletionResponse_search_results_item);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

class function TCompletionResponseSerializer.Deserialize(aJSON : TJSONObject) : TCompletionResponse;
   procedure Process_Citations;
   var
      lCitations: TJSONArray;
      i: Integer;
   begin
        lCitations := aJSON.Get('citations', TJSONArray(nil));
        if nil = lCitations then exit;

        SetLength(Result.citations, lCitations.Count);
        for i := 0 to lCitations.Count - 1
        do
          Result.citations[i]:= lCitations.Strings[i];
   end;
   procedure Process_Search_Results;
   var
      lSearch_Results: TJSONArray;
      i: Integer;
   begin
        lSearch_Results := aJSON.Get('search_results', TJSONArray(nil));
        if nil = lSearch_Results then exit;

        SetLength(Result.search_results, lSearch_Results.Count);
        for i := 0 to lSearch_Results.Count - 1
        do
          Result.search_results[i]:= TCompletionResponse_search_results_item.Deserialize(lSearch_Results.Objects[i]);
   end;
   procedure Process_Choices;
   var
      lChoices: TJSONArray;
      i: Integer;
   begin
        lChoices := aJSON.Get('choices', TJSONArray(nil));
        if nil = lChoices then exit;

        SetLength(Result.choices, lChoices.Count);
        for i := 0 to lChoices.Count - 1
        do
          Result.choices[i] := TNested_choices_items.Deserialize(lChoices.Objects[i]);
   end;
begin
     Result := TCompletionResponse.Create;
     If (aJSON=Nil) then exit;

     Result.id     :=aJSON.Get('id','');
     Result.model  :=aJSON.Get('model','');
     Result.created:=aJSON.Get('created',0);
     Result.usage  :=TCompletionResponse_usage.Deserialize(aJSON.Get('usage',TJSONObject(Nil)));
     Process_Citations;
     Process_Search_Results;
     Result.object_:=aJSON.Get('object','');
     Process_Choices;
end;

class function TCompletionResponseSerializer.Deserialize(aJSON : String) : TCompletionResponse;
var
  lObj : TJSONObject;
begin
  Result := Default(TCompletionResponse);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

class function TNested_permission_itemsSerializer.Deserialize(aJSON : TJSONObject) : TNested_permission_items;
begin
  Result := TNested_permission_items.Create;
  If (aJSON=Nil) then
    exit;
  Result.id:=aJSON.Get('id','');
  Result.object_:=aJSON.Get('object','');
  Result.created:=aJSON.Get('created',0.0);
  Result.allow_create_engine:=aJSON.Get('allow_create_engine',false);
  Result.allow_sampling:=aJSON.Get('allow_sampling',false);
  Result.allow_logprobs:=aJSON.Get('allow_logprobs',false);
  Result.allow_search_indices:=aJSON.Get('allow_search_indices',false);
  Result.allow_view:=aJSON.Get('allow_view',false);
  Result.allow_fine_tuning:=aJSON.Get('allow_fine_tuning',false);
  Result.organization:=aJSON.Get('organization','');
  Result.group:=aJSON.Get('group','');
  Result.is_blocking:=aJSON.Get('is_blocking',false);
end;

class function TNested_permission_itemsSerializer.Deserialize(aJSON : String) : TNested_permission_items;
var
  lObj : TJSONObject;
begin
  Result := Default(TNested_permission_items);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

end.
