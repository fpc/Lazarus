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
unit chatgpt.Serializer;

interface

{$mode objfpc}
{$h+}
{$modeswitch typehelpers}


uses
  Types,
  fpJSON,
  chatgpt.Dto;

Type
  TCompletionRequestSerializer = class helper for TCompletionRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
  end;
  
  TNested_choices_itemsSerializer = class helper for TNested_choices_items
    class function Deserialize(aJSON : TJSONObject) : TNested_choices_items; overload; static;
    class function Deserialize(aJSON : String) : TNested_choices_items; overload; static;
  end;
  
  TCompletionResponse_usageSerializer = class helper for TCompletionResponse_usage
    class function Deserialize(aJSON : TJSONObject) : TCompletionResponse_usage; overload; static;
    class function Deserialize(aJSON : String) : TCompletionResponse_usage; overload; static;
  end;
  
  TCompletionResponseSerializer = class helper for TCompletionResponse
    class function Deserialize(aJSON : TJSONObject) : TCompletionResponse; overload; static;
    class function Deserialize(aJSON : String) : TCompletionResponse; overload; static;
  end;
  
  TCreateImagesRequestSerializer = class helper for TCreateImagesRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
  end;
  
  TDeleteFileResponseSerializer = class helper for TDeleteFileResponse
    class function Deserialize(aJSON : TJSONObject) : TDeleteFileResponse; overload; static;
    class function Deserialize(aJSON : String) : TDeleteFileResponse; overload; static;
  end;
  
  TDeleteFineTuneResponseSerializer = class helper for TDeleteFineTuneResponse
    class function Deserialize(aJSON : TJSONObject) : TDeleteFineTuneResponse; overload; static;
    class function Deserialize(aJSON : String) : TDeleteFineTuneResponse; overload; static;
  end;
  
  TEditRequestSerializer = class helper for TEditRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
  end;
  
  TEditResponse_usageSerializer = class helper for TEditResponse_usage
    class function Deserialize(aJSON : TJSONObject) : TEditResponse_usage; overload; static;
    class function Deserialize(aJSON : String) : TEditResponse_usage; overload; static;
  end;
  
  TEditResponseSerializer = class helper for TEditResponse
    class function Deserialize(aJSON : TJSONObject) : TEditResponse; overload; static;
    class function Deserialize(aJSON : String) : TEditResponse; overload; static;
  end;
  
  TEmbeddingsRequestSerializer = class helper for TEmbeddingsRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
  end;
  
  TNested_data_itemsSerializer = class helper for TNested_data_items
    class function Deserialize(aJSON : TJSONObject) : TNested_data_items; overload; static;
    class function Deserialize(aJSON : String) : TNested_data_items; overload; static;
  end;
  
  TEmbeddingsResponse_usageSerializer = class helper for TEmbeddingsResponse_usage
    class function Deserialize(aJSON : TJSONObject) : TEmbeddingsResponse_usage; overload; static;
    class function Deserialize(aJSON : String) : TEmbeddingsResponse_usage; overload; static;
  end;
  
  TEmbeddingsResponseSerializer = class helper for TEmbeddingsResponse
    class function Deserialize(aJSON : TJSONObject) : TEmbeddingsResponse; overload; static;
    class function Deserialize(aJSON : String) : TEmbeddingsResponse; overload; static;
  end;
  
  TError_errorSerializer = class helper for TError_error
    class function Deserialize(aJSON : TJSONObject) : TError_error; overload; static;
    class function Deserialize(aJSON : String) : TError_error; overload; static;
  end;
  
  TErrorSerializer = class helper for TError
    class function Deserialize(aJSON : TJSONObject) : TError; overload; static;
    class function Deserialize(aJSON : String) : TError; overload; static;
  end;
  
  TFileResponseSerializer = class helper for TFileResponse
    class function Deserialize(aJSON : TJSONObject) : TFileResponse; overload; static;
    class function Deserialize(aJSON : String) : TFileResponse; overload; static;
  end;
  
  TFineTuneEventsResponseSerializer = class helper for TFineTuneEventsResponse
    class function Deserialize(aJSON : TJSONObject) : TFineTuneEventsResponse; overload; static;
    class function Deserialize(aJSON : String) : TFineTuneEventsResponse; overload; static;
  end;
  
  TFineTunesRequestSerializer = class helper for TFineTunesRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
  end;
  
  TNested_events_itemsSerializer = class helper for TNested_events_items
    class function Deserialize(aJSON : TJSONObject) : TNested_events_items; overload; static;
    class function Deserialize(aJSON : String) : TNested_events_items; overload; static;
  end;
  
  TFineTunesResponse_hyperparamsSerializer = class helper for TFineTunesResponse_hyperparams
    class function Deserialize(aJSON : TJSONObject) : TFineTunesResponse_hyperparams; overload; static;
    class function Deserialize(aJSON : String) : TFineTunesResponse_hyperparams; overload; static;
  end;
  
  TNested_training_files_itemsSerializer = class helper for TNested_training_files_items
    class function Deserialize(aJSON : TJSONObject) : TNested_training_files_items; overload; static;
    class function Deserialize(aJSON : String) : TNested_training_files_items; overload; static;
  end;
  
  TFineTunesResponseSerializer = class helper for TFineTunesResponse
    class function Deserialize(aJSON : TJSONObject) : TFineTunesResponse; overload; static;
    class function Deserialize(aJSON : String) : TFineTunesResponse; overload; static;
  end;
  
  TGetFineTuneResponse_hyperparamsSerializer = class helper for TGetFineTuneResponse_hyperparams
    class function Deserialize(aJSON : TJSONObject) : TGetFineTuneResponse_hyperparams; overload; static;
    class function Deserialize(aJSON : String) : TGetFineTuneResponse_hyperparams; overload; static;
  end;
  
  TNested_result_files_itemsSerializer = class helper for TNested_result_files_items
    class function Deserialize(aJSON : TJSONObject) : TNested_result_files_items; overload; static;
    class function Deserialize(aJSON : String) : TNested_result_files_items; overload; static;
  end;
  
  TGetFineTuneResponseSerializer = class helper for TGetFineTuneResponse
    class function Deserialize(aJSON : TJSONObject) : TGetFineTuneResponse; overload; static;
    class function Deserialize(aJSON : String) : TGetFineTuneResponse; overload; static;
  end;
  
  TGetFineTunesResponseSerializer = class helper for TGetFineTunesResponse
    class function Deserialize(aJSON : TJSONObject) : TGetFineTunesResponse; overload; static;
    class function Deserialize(aJSON : String) : TGetFineTunesResponse; overload; static;
  end;
  
  TImageEditRequestSerializer = class helper for TImageEditRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
  end;
  
  TImagesResponseSerializer = class helper for TImagesResponse
    class function Deserialize(aJSON : TJSONObject) : TImagesResponse; overload; static;
    class function Deserialize(aJSON : String) : TImagesResponse; overload; static;
  end;
  
  TImageValidationRequestSerializer = class helper for TImageValidationRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
  end;
  
  TNested_permission_itemsSerializer = class helper for TNested_permission_items
    class function Deserialize(aJSON : TJSONObject) : TNested_permission_items; overload; static;
    class function Deserialize(aJSON : String) : TNested_permission_items; overload; static;
  end;
  
  TModelSerializer = class helper for TModel
    class function Deserialize(aJSON : TJSONObject) : TModel; overload; static;
    class function Deserialize(aJSON : String) : TModel; overload; static;
  end;
  
  TModelsSerializer = class helper for TModels
    class function Deserialize(aJSON : TJSONObject) : TModels; overload; static;
    class function Deserialize(aJSON : String) : TModels; overload; static;
  end;
  
  TModerationRequestSerializer = class helper for TModerationRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
  end;
  
  T_Nested_results_items__categoriesSerializer = class helper for T_Nested_results_items__categories
    class function Deserialize(aJSON : TJSONObject) : T_Nested_results_items__categories; overload; static;
    class function Deserialize(aJSON : String) : T_Nested_results_items__categories; overload; static;
  end;
  
  T_Nested_results_items__category_scoresSerializer = class helper for T_Nested_results_items__category_scores
    class function Deserialize(aJSON : TJSONObject) : T_Nested_results_items__category_scores; overload; static;
    class function Deserialize(aJSON : String) : T_Nested_results_items__category_scores; overload; static;
  end;
  
  TNested_results_itemsSerializer = class helper for TNested_results_items
    class function Deserialize(aJSON : TJSONObject) : TNested_results_items; overload; static;
    class function Deserialize(aJSON : String) : TNested_results_items; overload; static;
  end;
  
  TModerationResponseSerializer = class helper for TModerationResponse
    class function Deserialize(aJSON : TJSONObject) : TModerationResponse; overload; static;
    class function Deserialize(aJSON : String) : TModerationResponse; overload; static;
  end;
  
  TRetrieveFileResponseSerializer = class helper for TRetrieveFileResponse
    class function Deserialize(aJSON : TJSONObject) : TRetrieveFileResponse; overload; static;
    class function Deserialize(aJSON : String) : TRetrieveFileResponse; overload; static;
  end;
  
  TUploadFileRequestSerializer = class helper for TUploadFileRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
  end;
  
  TUploadFileResponseSerializer = class helper for TUploadFileResponse
    class function Deserialize(aJSON : TJSONObject) : TUploadFileResponse; overload; static;
    class function Deserialize(aJSON : String) : TUploadFileResponse; overload; static;
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

function TCompletionRequestSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('best_of',best_of);
    Result.Add('echo',echo);
    Result.Add('frequency_penalty',frequency_penalty);
    if (logit_bias<>'') then
      Result.Add('logit_bias',GetJSON(logit_bias));
    Result.Add('logprobs',logprobs);
    Result.Add('max_tokens',max_tokens);
    Result.Add('model',model);
    Result.Add('n',n);
    Result.Add('presence_penalty',presence_penalty);
    Result.Add('prompt',prompt);
    Result.Add('stop',stop);
    Result.Add('stream',stream);
    Result.Add('suffix',suffix);
    Result.Add('temperature',temperature);
    Result.Add('top_p',top_p);
    Result.Add('user',user);
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

function TCreateImagesRequestSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('n',n);
    Result.Add('prompt',prompt);
    Result.Add('response_format',response_format);
    Result.Add('size',size);
    Result.Add('user',user);
  except
    Result.Free;
    raise;
  end;
end;

function TCreateImagesRequestSerializer.Serialize : String;
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

function TEditRequestSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('input',input);
    Result.Add('instruction',instruction);
    Result.Add('model',model);
    Result.Add('n',n);
    Result.Add('temperature',temperature);
    Result.Add('top_p',top_p);
  except
    Result.Free;
    raise;
  end;
end;

function TEditRequestSerializer.Serialize : String;
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

function TEmbeddingsRequestSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('input',Arr);
    For I:=0 to Length(input)-1 do
      Arr.Add(input[i]);
    Result.Add('model',model);
    Result.Add('user',user);
  except
    Result.Free;
    raise;
  end;
end;

function TEmbeddingsRequestSerializer.Serialize : String;
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

function TFineTunesRequestSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('batch_size',batch_size);
    Arr:=TJSONArray.Create;
    Result.Add('classification_betas',Arr);
    For I:=0 to Length(classification_betas)-1 do
      Arr.Add(classification_betas[i]);
    Result.Add('classification_n_classes',classification_n_classes);
    Result.Add('classification_positive_class',classification_positive_class);
    Result.Add('compute_classification_metrics',compute_classification_metrics);
    Result.Add('learning_rate_multiplier',learning_rate_multiplier);
    Result.Add('model',model);
    Result.Add('n_epochs',n_epochs);
    Result.Add('prompt_loss_weight',prompt_loss_weight);
    Result.Add('suffix',suffix);
    Result.Add('training_file',training_file);
    Result.Add('validation_file',validation_file);
  except
    Result.Free;
    raise;
  end;
end;

function TFineTunesRequestSerializer.Serialize : String;
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

function TImageEditRequestSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('image',image);
    Result.Add('mask',mask);
    Result.Add('n',n);
    Result.Add('prompt',prompt);
    Result.Add('response_format',response_format);
    Result.Add('size',size);
    Result.Add('user',user);
  except
    Result.Free;
    raise;
  end;
end;

function TImageEditRequestSerializer.Serialize : String;
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

function TImageValidationRequestSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('image',image);
    Result.Add('n',n);
    Result.Add('response_format',response_format);
    Result.Add('size',size);
    Result.Add('user',user);
  except
    Result.Free;
    raise;
  end;
end;

function TImageValidationRequestSerializer.Serialize : String;
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

function TModerationRequestSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('input',input);
    Result.Add('model',model);
  except
    Result.Free;
    raise;
  end;
end;

function TModerationRequestSerializer.Serialize : String;
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

function TUploadFileRequestSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('file',file_);
    Result.Add('purpose',purpose);
  except
    Result.Free;
    raise;
  end;
end;

function TUploadFileRequestSerializer.Serialize : String;
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

class function TNested_choices_itemsSerializer.Deserialize(aJSON : TJSONObject) : TNested_choices_items;
begin
  Result := TNested_choices_items.Create;
  If (aJSON=Nil) then
    exit;
  Result.text:=aJSON.Get('text','');
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

class function TCompletionResponse_usageSerializer.Deserialize(aJSON : TJSONObject) : TCompletionResponse_usage;
begin
  Result := TCompletionResponse_usage.Create;
  If (aJSON=Nil) then
    exit;
  Result.prompt_tokens:=aJSON.Get('prompt_tokens',0);
  Result.completion_tokens:=aJSON.Get('completion_tokens',0);
  Result.total_tokens:=aJSON.Get('total_tokens',0);
end;

class function TCompletionResponse_usageSerializer.Deserialize(aJSON : String) : TCompletionResponse_usage;
var
  lObj : TJSONObject;
begin
  Result := Default(TCompletionResponse_usage);
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
var
  lChoices: TJSONArray;
  i: Integer;
begin
  Result := TCompletionResponse.Create;
  If (aJSON=Nil) then
    exit;
  lChoices := aJSON.Get('choices', TJSONArray(nil));
  if lChoices <> nil then
  begin
    SetLength(Result.choices, lChoices.Count);
    for i := 0 to lChoices.Count - 1 do
      Result.choices[i] := TNested_choices_items.Deserialize(lChoices.Objects[i]);
  end;
  Result.created:=aJSON.Get('created',0);
  Result.id:=aJSON.Get('id','');
  Result.model:=aJSON.Get('model','');
  Result.object_:=aJSON.Get('object','');
  Result.usage:=TCompletionResponse_usage.Deserialize(aJSON.Get('usage',TJSONObject(Nil)));
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

class function TDeleteFileResponseSerializer.Deserialize(aJSON : TJSONObject) : TDeleteFileResponse;
begin
  Result := TDeleteFileResponse.Create;
  If (aJSON=Nil) then
    exit;
  Result.deleted:=aJSON.Get('deleted',false);
  Result.id:=aJSON.Get('id','');
  Result.object_:=aJSON.Get('object','');
end;

class function TDeleteFileResponseSerializer.Deserialize(aJSON : String) : TDeleteFileResponse;
var
  lObj : TJSONObject;
begin
  Result := Default(TDeleteFileResponse);
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

class function TDeleteFineTuneResponseSerializer.Deserialize(aJSON : TJSONObject) : TDeleteFineTuneResponse;
begin
  Result := TDeleteFineTuneResponse.Create;
  If (aJSON=Nil) then
    exit;
  Result.deleted:=aJSON.Get('deleted',false);
  Result.id:=aJSON.Get('id','');
  Result.object_:=aJSON.Get('object','');
end;

class function TDeleteFineTuneResponseSerializer.Deserialize(aJSON : String) : TDeleteFineTuneResponse;
var
  lObj : TJSONObject;
begin
  Result := Default(TDeleteFineTuneResponse);
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

class function TEditResponse_usageSerializer.Deserialize(aJSON : TJSONObject) : TEditResponse_usage;
begin
  Result := TEditResponse_usage.Create;
  If (aJSON=Nil) then
    exit;
  Result.prompt_tokens:=aJSON.Get('prompt_tokens',0);
  Result.completion_tokens:=aJSON.Get('completion_tokens',0);
  Result.total_tokens:=aJSON.Get('total_tokens',0);
end;

class function TEditResponse_usageSerializer.Deserialize(aJSON : String) : TEditResponse_usage;
var
  lObj : TJSONObject;
begin
  Result := Default(TEditResponse_usage);
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

class function TEditResponseSerializer.Deserialize(aJSON : TJSONObject) : TEditResponse;
var
  lChoices: TJSONArray;
  i: Integer;
begin
  Result := TEditResponse.Create;
  If (aJSON=Nil) then
    exit;
  lChoices := aJSON.Get('choices', TJSONArray(nil));
  if lChoices <> nil then
  begin
    SetLength(Result.choices, lChoices.Count);
    for i := 0 to lChoices.Count - 1 do
      Result.choices[i] := TNested_choices_items.Deserialize(lChoices.Objects[i]);
  end;
  Result.created:=aJSON.Get('created',0);
  Result.object_:=aJSON.Get('object','');
  Result.usage:=TEditResponse_usage.Deserialize(aJSON.Get('usage',TJSONObject(Nil)));
end;

class function TEditResponseSerializer.Deserialize(aJSON : String) : TEditResponse;
var
  lObj : TJSONObject;
begin
  Result := Default(TEditResponse);
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

class function TNested_data_itemsSerializer.Deserialize(aJSON : TJSONObject) : TNested_data_items;
begin
  Result := TNested_data_items.Create;
  If (aJSON=Nil) then
    exit;
  Result.url:=aJSON.Get('url','');
end;

class function TNested_data_itemsSerializer.Deserialize(aJSON : String) : TNested_data_items;
var
  lObj : TJSONObject;
begin
  Result := Default(TNested_data_items);
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

class function TEmbeddingsResponse_usageSerializer.Deserialize(aJSON : TJSONObject) : TEmbeddingsResponse_usage;
begin
  Result := TEmbeddingsResponse_usage.Create;
  If (aJSON=Nil) then
    exit;
  Result.prompt_tokens:=aJSON.Get('prompt_tokens',0);
  Result.total_tokens:=aJSON.Get('total_tokens',0);
end;

class function TEmbeddingsResponse_usageSerializer.Deserialize(aJSON : String) : TEmbeddingsResponse_usage;
var
  lObj : TJSONObject;
begin
  Result := Default(TEmbeddingsResponse_usage);
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

class function TEmbeddingsResponseSerializer.Deserialize(aJSON : TJSONObject) : TEmbeddingsResponse;
var
  lData: TJSONArray;
  i: Integer;
begin
  Result := TEmbeddingsResponse.Create;
  If (aJSON=Nil) then
    exit;
  lData := aJSON.Get('data', TJSONArray(nil));
  if lData <> nil then
  begin
    SetLength(Result.data, lData.Count);
    for i := 0 to lData.Count - 1 do
      Result.data[i] := TNested_data_items.Deserialize(lData.Objects[i]);
  end;
  Result.model:=aJSON.Get('model','');
  Result.object_:=aJSON.Get('object','');
  Result.usage:=TEmbeddingsResponse_usage.Deserialize(aJSON.Get('usage',TJSONObject(Nil)));
end;

class function TEmbeddingsResponseSerializer.Deserialize(aJSON : String) : TEmbeddingsResponse;
var
  lObj : TJSONObject;
begin
  Result := Default(TEmbeddingsResponse);
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

class function TFileResponseSerializer.Deserialize(aJSON : TJSONObject) : TFileResponse;
var
  lData: TJSONArray;
  i: Integer;
begin
  Result := TFileResponse.Create;
  If (aJSON=Nil) then
    exit;
  lData := aJSON.Get('data', TJSONArray(nil));
  if lData <> nil then
  begin
    SetLength(Result.data, lData.Count);
    for i := 0 to lData.Count - 1 do
      Result.data[i] := TNested_data_items.Deserialize(lData.Objects[i]);
  end;
  Result.object_:=aJSON.Get('object','');
end;

class function TFileResponseSerializer.Deserialize(aJSON : String) : TFileResponse;
var
  lObj : TJSONObject;
begin
  Result := Default(TFileResponse);
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

class function TImagesResponseSerializer.Deserialize(aJSON : TJSONObject) : TImagesResponse;
var
  lData: TJSONArray;
  i: Integer;
begin
  Result := TImagesResponse.Create;
  If (aJSON=Nil) then
    exit;
  lData := aJSON.Get('data', TJSONArray(nil));
  if lData <> nil then
  begin
    SetLength(Result.data, lData.Count);
    for i := 0 to lData.Count - 1 do
      Result.data[i] := TNested_data_items.Deserialize(lData.Objects[i]);
  end;
  Result.created:=aJSON.Get('created',0);
end;

class function TImagesResponseSerializer.Deserialize(aJSON : String) : TImagesResponse;
var
  lObj : TJSONObject;
begin
  Result := Default(TImagesResponse);
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

class function TModelSerializer.Deserialize(aJSON : TJSONObject) : TModel;
var
  lPermissions: TJSONArray;
  i: Integer;
begin
  Result := TModel.Create;
  If (aJSON=Nil) then
    exit;
  Result.created:=aJSON.Get('created',0.0);
  Result.id:=aJSON.Get('id','');
  Result.object_:=aJSON.Get('object','');
  Result.owned_by:=aJSON.Get('owned_by','');
  Result.parent:=aJSON.Get('parent','');
  lPermissions := aJSON.Get('permission', TJSONArray(nil));
  if lPermissions <> nil then
  begin
    SetLength(Result.permission, lPermissions.Count);
    for i := 0 to lPermissions.Count - 1 do
      Result.permission[i] := TNested_permission_items.Deserialize(lPermissions.Objects[i]);
  end;
  Result.root:=aJSON.Get('root','');
end;

class function TModelSerializer.Deserialize(aJSON : String) : TModel;
var
  lObj : TJSONObject;
begin
  Result := Default(TModel);
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

class function TModelsSerializer.Deserialize(aJSON : TJSONObject) : TModels;
var
  lData: TJSONArray;
  i: Integer;
begin
  Result := TModels.Create;
  If (aJSON=Nil) then
    exit;
  lData := aJSON.Get('data', TJSONArray(nil));
  if lData <> nil then
  begin
    SetLength(Result.data, lData.Count);
    for i := 0 to lData.Count - 1 do
      Result.data[i] := TModel.Deserialize(lData.Objects[i]);
  end;
  Result.object_:=aJSON.Get('object','');
end;

class function TModelsSerializer.Deserialize(aJSON : String) : TModels;
var
  lObj : TJSONObject;
begin
  Result := Default(TModels);
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

class function T_Nested_results_items__categoriesSerializer.Deserialize(aJSON : TJSONObject) : T_Nested_results_items__categories;
begin
  Result := T_Nested_results_items__categories.Create;
  If (aJSON=Nil) then
    exit;
  Result.hate:=aJSON.Get('hate',false);
  Result.hate_threatening:=aJSON.Get('hate/threatening',false);
  Result.self_harm:=aJSON.Get('self-harm',false);
  Result.sexual:=aJSON.Get('sexual',false);
  Result.sexual_minors:=aJSON.Get('sexual/minors',false);
  Result.violence:=aJSON.Get('violence',false);
  Result.violence_graphic:=aJSON.Get('violence/graphic',false);
end;

class function T_Nested_results_items__categoriesSerializer.Deserialize(aJSON : String) : T_Nested_results_items__categories;
var
  lObj : TJSONObject;
begin
  Result := Default(T_Nested_results_items__categories);
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

class function T_Nested_results_items__category_scoresSerializer.Deserialize(aJSON : TJSONObject) : T_Nested_results_items__category_scores;
begin
  Result := T_Nested_results_items__category_scores.Create;
  If (aJSON=Nil) then
    exit;
  Result.hate:=aJSON.Get('hate',0.0);
  Result.hate_threatening:=aJSON.Get('hate/threatening',0.0);
  Result.self_harm:=aJSON.Get('self-harm',0.0);
  Result.sexual:=aJSON.Get('sexual',0.0);
  Result.sexual_minors:=aJSON.Get('sexual/minors',0.0);
  Result.violence:=aJSON.Get('violence',0.0);
  Result.violence_graphic:=aJSON.Get('violence/graphic',0.0);
end;

class function T_Nested_results_items__category_scoresSerializer.Deserialize(aJSON : String) : T_Nested_results_items__category_scores;
var
  lObj : TJSONObject;
begin
  Result := Default(T_Nested_results_items__category_scores);
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

class function TNested_results_itemsSerializer.Deserialize(aJSON : TJSONObject) : TNested_results_items;
begin
  Result := TNested_results_items.Create;
  If (aJSON=Nil) then
    exit;
  Result.categories:=T_Nested_results_items__categories.Deserialize(aJSON.Get('categories',TJSONObject(Nil)));
  Result.category_scores:=T_Nested_results_items__category_scores.Deserialize(aJSON.Get('category_scores',TJSONObject(Nil)));
  Result.flagged:=aJSON.Get('flagged',false);
end;

class function TNested_results_itemsSerializer.Deserialize(aJSON : String) : TNested_results_items;
var
  lObj : TJSONObject;
begin
  Result := Default(TNested_results_items);
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

class function TModerationResponseSerializer.Deserialize(aJSON : TJSONObject) : TModerationResponse;
var
  lResults: TJSONArray;
  i: Integer;
begin
  Result := TModerationResponse.Create;
  If (aJSON=Nil) then
    exit;
  Result.id:=aJSON.Get('id','');
  Result.model:=aJSON.Get('model','');
  lResults := aJSON.Get('results', TJSONArray(nil));
  if lResults <> nil then
  begin
    SetLength(Result.results, lResults.Count);
    for i := 0 to lResults.Count - 1 do
      Result.results[i] := TNested_results_items.Deserialize(lResults.Objects[i]);
  end;
end;

class function TModerationResponseSerializer.Deserialize(aJSON : String) : TModerationResponse;
var
  lObj : TJSONObject;
begin
  Result := Default(TModerationResponse);
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

class function TRetrieveFileResponseSerializer.Deserialize(aJSON : TJSONObject) : TRetrieveFileResponse;
begin
  Result := TRetrieveFileResponse.Create;
  If (aJSON=Nil) then
    exit;
  Result.bytes:=aJSON.Get('bytes',0);
  Result.created_at:=aJSON.Get('created_at',0);
  Result.filename:=aJSON.Get('filename','');
  Result.id:=aJSON.Get('id','');
  Result.object_:=aJSON.Get('object','');
  Result.purpose:=aJSON.Get('purpose','');
end;

class function TRetrieveFileResponseSerializer.Deserialize(aJSON : String) : TRetrieveFileResponse;
var
  lObj : TJSONObject;
begin
  Result := Default(TRetrieveFileResponse);
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

class function TUploadFileResponseSerializer.Deserialize(aJSON : TJSONObject) : TUploadFileResponse;
begin
  Result := TUploadFileResponse.Create;
  If (aJSON=Nil) then
    exit;
  Result.bytes:=aJSON.Get('bytes',0);
  Result.created_at:=aJSON.Get('created_at',0);
  Result.filename:=aJSON.Get('filename','');
  Result.id:=aJSON.Get('id','');
  Result.object_:=aJSON.Get('object','');
  Result.purpose:=aJSON.Get('purpose','');
end;

class function TUploadFileResponseSerializer.Deserialize(aJSON : String) : TUploadFileResponse;
var
  lObj : TJSONObject;
begin
  Result := Default(TUploadFileResponse);
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

class function TFineTuneEventsResponseSerializer.Deserialize(aJSON : TJSONObject) : TFineTuneEventsResponse;
var
  lData: TJSONArray;
  i: Integer;
begin
  Result := TFineTuneEventsResponse.Create;
  If (aJSON=Nil) then
    exit;
  lData := aJSON.Get('data', TJSONArray(nil));
  if lData <> nil then
  begin
    SetLength(Result.data, lData.Count);
    for i := 0 to lData.Count - 1 do
      Result.data[i] := TNested_data_items.Deserialize(lData.Objects[i]);
  end;
  Result.object_:=aJSON.Get('object','');
end;

class function TFineTuneEventsResponseSerializer.Deserialize(aJSON : String) : TFineTuneEventsResponse;
var
  lObj : TJSONObject;
begin
  Result := Default(TFineTuneEventsResponse);
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

class function TNested_events_itemsSerializer.Deserialize(aJSON : TJSONObject) : TNested_events_items;
begin
  Result := TNested_events_items.Create;
  If (aJSON=Nil) then
    exit;
  Result.object_:=aJSON.Get('object','');
  Result.created_at:=aJSON.Get('created_at',0);
  Result.level:=aJSON.Get('level','');
  Result.message:=aJSON.Get('message','');
end;

class function TNested_events_itemsSerializer.Deserialize(aJSON : String) : TNested_events_items;
var
  lObj : TJSONObject;
begin
  Result := Default(TNested_events_items);
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

class function TFineTunesResponse_hyperparamsSerializer.Deserialize(aJSON : TJSONObject) : TFineTunesResponse_hyperparams;
begin
  Result := TFineTunesResponse_hyperparams.Create;
  If (aJSON=Nil) then
    exit;
  Result.batch_size:=aJSON.Get('batch_size',0);
  Result.learning_rate_multiplier:=aJSON.Get('learning_rate_multiplier',0.0);
  Result.n_epochs:=aJSON.Get('n_epochs',0);
  Result.prompt_loss_weight:=aJSON.Get('prompt_loss_weight',0.0);
end;

class function TFineTunesResponse_hyperparamsSerializer.Deserialize(aJSON : String) : TFineTunesResponse_hyperparams;
var
  lObj : TJSONObject;
begin
  Result := Default(TFineTunesResponse_hyperparams);
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

class function TNested_training_files_itemsSerializer.Deserialize(aJSON : TJSONObject) : TNested_training_files_items;
begin
  Result := TNested_training_files_items.Create;
  If (aJSON=Nil) then
    exit;
  Result.id:=aJSON.Get('id','');
  Result.object_:=aJSON.Get('object','');
  Result.bytes:=aJSON.Get('bytes',0);
  Result.created_at:=aJSON.Get('created_at',0);
  Result.filename:=aJSON.Get('filename','');
  Result.purpose:=aJSON.Get('purpose','');
end;

class function TNested_training_files_itemsSerializer.Deserialize(aJSON : String) : TNested_training_files_items;
var
  lObj : TJSONObject;
begin
  Result := Default(TNested_training_files_items);
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

class function TFineTunesResponseSerializer.Deserialize(aJSON : TJSONObject) : TFineTunesResponse;
var
  lEvents: TJSONArray;
  lTrainingFiles: TJSONArray;
  lResultFiles: TJSONArray;
  lValidationFiles: TJSONArray;
  i: Integer;
begin
  Result := TFineTunesResponse.Create;
  If (aJSON=Nil) then
    exit;
  Result.created_at:=aJSON.Get('created_at',0);
  lEvents := aJSON.Get('events', TJSONArray(nil));
  if lEvents <> nil then
  begin
    SetLength(Result.events, lEvents.Count);
    for i := 0 to lEvents.Count - 1 do
      Result.events[i] := TNested_events_items.Deserialize(lEvents.Objects[i]);
  end;
  Result.fine_tuned_model:=aJSON.Get('fine_tuned_model','');
  Result.hyperparams:=TFineTunesResponse_hyperparams.Deserialize(aJSON.Get('hyperparams',TJSONObject(Nil)));
  Result.id:=aJSON.Get('id','');
  Result.model:=aJSON.Get('model','');
  Result.object_:=aJSON.Get('object','');
  Result.organization_id:=aJSON.Get('organization_id','');
  lResultFiles := aJSON.Get('result_files', TJSONArray(nil));
  if lResultFiles <> nil then
  begin
    SetLength(Result.result_files, lResultFiles.Count);
    for i := 0 to lResultFiles.Count - 1 do
      Result.result_files[i] := lResultFiles.Strings[i];
  end;
  Result.status:=aJSON.Get('status','');
  lTrainingFiles := aJSON.Get('training_files', TJSONArray(nil));
  if lTrainingFiles <> nil then
  begin
    SetLength(Result.training_files, lTrainingFiles.Count);
    for i := 0 to lTrainingFiles.Count - 1 do
      Result.training_files[i] := TNested_training_files_items.Deserialize(lTrainingFiles.Objects[i]);
  end;
  Result.updated_at:=aJSON.Get('updated_at',0);
  lValidationFiles := aJSON.Get('validation_files', TJSONArray(nil));
  if lValidationFiles <> nil then
  begin
    SetLength(Result.validation_files, lValidationFiles.Count);
    for i := 0 to lValidationFiles.Count - 1 do
      Result.validation_files[i] := lValidationFiles.Strings[i];
  end;
end;

class function TFineTunesResponseSerializer.Deserialize(aJSON : String) : TFineTunesResponse;
var
  lObj : TJSONObject;
begin
  Result := Default(TFineTunesResponse);
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

class function TGetFineTuneResponse_hyperparamsSerializer.Deserialize(aJSON : TJSONObject) : TGetFineTuneResponse_hyperparams;
begin
  Result := TGetFineTuneResponse_hyperparams.Create;
  If (aJSON=Nil) then
    exit;
  Result.batch_size:=aJSON.Get('batch_size',0);
  Result.learning_rate_multiplier:=aJSON.Get('learning_rate_multiplier',0.0);
  Result.n_epochs:=aJSON.Get('n_epochs',0);
  Result.prompt_loss_weight:=aJSON.Get('prompt_loss_weight',0.0);
end;

class function TGetFineTuneResponse_hyperparamsSerializer.Deserialize(aJSON : String) : TGetFineTuneResponse_hyperparams;
var
  lObj : TJSONObject;
begin
  Result := Default(TGetFineTuneResponse_hyperparams);
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

class function TNested_result_files_itemsSerializer.Deserialize(aJSON : TJSONObject) : TNested_result_files_items;
begin
  Result := TNested_result_files_items.Create;
  If (aJSON=Nil) then
    exit;
  Result.id:=aJSON.Get('id','');
  Result.object_:=aJSON.Get('object','');
  Result.bytes:=aJSON.Get('bytes',0);
  Result.created_at:=aJSON.Get('created_at',0);
  Result.filename:=aJSON.Get('filename','');
  Result.purpose:=aJSON.Get('purpose','');
end;

class function TNested_result_files_itemsSerializer.Deserialize(aJSON : String) : TNested_result_files_items;
var
  lObj : TJSONObject;
begin
  Result := Default(TNested_result_files_items);
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

class function TGetFineTuneResponseSerializer.Deserialize(aJSON : TJSONObject) : TGetFineTuneResponse;
var
  lEvents: TJSONArray;
  lTrainingFiles: TJSONArray;
  lResultFiles: TJSONArray;
  lValidationFiles: TJSONArray;
  i: Integer;
begin
  Result := TGetFineTuneResponse.Create;
  If (aJSON=Nil) then
    exit;
  Result.created_at:=aJSON.Get('created_at',0);
  lEvents := aJSON.Get('events', TJSONArray(nil));
  if lEvents <> nil then
  begin
    SetLength(Result.events, lEvents.Count);
    for i := 0 to lEvents.Count - 1 do
      Result.events[i] := TNested_events_items.Deserialize(lEvents.Objects[i]);
  end;
  Result.fine_tuned_model:=aJSON.Get('fine_tuned_model','');
  Result.hyperparams:=TGetFineTuneResponse_hyperparams.Deserialize(aJSON.Get('hyperparams',TJSONObject(Nil)));
  Result.id:=aJSON.Get('id','');
  Result.model:=aJSON.Get('model','');
  Result.object_:=aJSON.Get('object','');
  Result.organization_id:=aJSON.Get('organization_id','');
  lResultFiles := aJSON.Get('result_files', TJSONArray(nil));
  if lResultFiles <> nil then
  begin
    SetLength(Result.result_files, lResultFiles.Count);
    for i := 0 to lResultFiles.Count - 1 do
      Result.result_files[i] := TNested_result_files_items.Deserialize(lResultFiles.Objects[i]);
  end;
  Result.status:=aJSON.Get('status','');
  lTrainingFiles := aJSON.Get('training_files', TJSONArray(nil));
  if lTrainingFiles <> nil then
  begin
    SetLength(Result.training_files, lTrainingFiles.Count);
    for i := 0 to lTrainingFiles.Count - 1 do
      Result.training_files[i] := TNested_training_files_items.Deserialize(lTrainingFiles.Objects[i]);
  end;
  Result.updated_at:=aJSON.Get('updated_at',0);
  lValidationFiles := aJSON.Get('validation_files', TJSONArray(nil));
  if lValidationFiles <> nil then
  begin
    SetLength(Result.validation_files, lValidationFiles.Count);
    for i := 0 to lValidationFiles.Count - 1 do
      Result.validation_files[i] := lValidationFiles.Strings[i];
  end;
end;

class function TGetFineTuneResponseSerializer.Deserialize(aJSON : String) : TGetFineTuneResponse;
var
  lObj : TJSONObject;
begin
  Result := Default(TGetFineTuneResponse);
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

class function TGetFineTunesResponseSerializer.Deserialize(aJSON : TJSONObject) : TGetFineTunesResponse;
var
  lData: TJSONArray;
  i: Integer;
begin
  Result := TGetFineTunesResponse.Create;
  If (aJSON=Nil) then
    exit;
  lData := aJSON.Get('data', TJSONArray(nil));
  if lData <> nil then
  begin
    SetLength(Result.data, lData.Count);
    for i := 0 to lData.Count - 1 do
      Result.data[i] := TNested_data_items.Deserialize(lData.Objects[i]);
  end;
  Result.object_:=aJSON.Get('object','');
end;

class function TGetFineTunesResponseSerializer.Deserialize(aJSON : String) : TGetFineTunesResponse;
var
  lObj : TJSONObject;
begin
  Result := Default(TGetFineTunesResponse);
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
