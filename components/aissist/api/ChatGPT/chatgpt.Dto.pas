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
unit chatgpt.Dto;

{$mode objfpc}
{$h+}


interface

uses types;

Type

  stringArray = Array of string;
  
  TCompletionRequest = Class(TObject)
    best_of : integer;
    echo : boolean;
    frequency_penalty : double;
    logit_bias : string;
    logprobs : integer;
    max_tokens : integer;
    model : string;
    n : integer;
    presence_penalty : double;
    prompt : string;
    stop : string;
    stream : boolean;
    suffix : string;
    temperature : double;
    top_p : double;
    user : string;
  end;
  
  TNested_choices_items = Class(TObject)
    text : string;
    index : integer;
    logprobs : string;
    finish_reason : string;
  end;
  TNested_choices_itemsArray = Array of TNested_choices_items;

  TCompletionResponse_usage = Class(TObject)
    prompt_tokens : integer;
    completion_tokens : integer;
    total_tokens : integer;
  end;
  
  { TCompletionResponse }

  TCompletionResponse = Class(TObject)
    choices : TNested_choices_itemsArray;
    created : integer;
    id : string;
    model : string;
    object_ : string;
    usage : TCompletionResponse_usage;
    destructor destroy; override;
  end;
  
  TCreateImagesRequest = Class(TObject)
    n : integer;
    prompt : string;
    response_format : string;
    size : string;
    user : string;
  end;
  
  TDeleteFileResponse = Class(TObject)
    deleted : boolean;
    id : string;
    object_ : string;
  end;
  
  TDeleteFineTuneResponse = Class(TObject)
    deleted : boolean;
    id : string;
    object_ : string;
  end;
  
  TEditRequest = Class(TObject)
    input : string;
    instruction : string;
    model : string;
    n : integer;
    temperature : double;
    top_p : double;
  end;
  
  TEditResponse_usage = Class(TObject)
    prompt_tokens : integer;
    completion_tokens : integer;
    total_tokens : integer;
  end;
  
  { TEditResponse }

  TEditResponse = Class(TObject)
    choices : TNested_choices_itemsArray;
    created : integer;
    object_ : string;
    usage : TEditResponse_usage;
    destructor destroy; override;
  end;
  
  TEmbeddingsRequest = Class(TObject)
    input : TStringDynArray;
    model : string;
    user : string;
  end;
  
  TNested_data_items = Class(TObject)
    url : string;
  end;
  TNested_data_itemsArray = Array of TNested_data_items;

  TEmbeddingsResponse_usage = Class(TObject)
    prompt_tokens : integer;
    total_tokens : integer;
  end;
  
  { TEmbeddingsResponse }

  TEmbeddingsResponse = Class(TObject)
    data : TNested_data_itemsArray;
    model : string;
    object_ : string;
    usage : TEmbeddingsResponse_usage;
    destructor destroy; override;
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
  
  { TFileResponse }

  TFileResponse = Class(TObject)
    data : TNested_data_itemsArray;
    object_ : string;
    destructor Destroy; override;
  end;
  
  { TFineTuneEventsResponse }

  TFineTuneEventsResponse = Class(TObject)
    data : TNested_data_itemsArray;
    object_ : string;
    destructor Destroy; override;
  end;
  
  TFineTunesRequest = Class(TObject)
    batch_size : integer;
    classification_betas : TDoubleDynArray;
    classification_n_classes : integer;
    classification_positive_class : string;
    compute_classification_metrics : boolean;
    learning_rate_multiplier : double;
    model : string;
    n_epochs : integer;
    prompt_loss_weight : double;
    suffix : string;
    training_file : string;
    validation_file : string;
  end;
  
  TNested_events_items = Class(TObject)
    object_ : string;
    created_at : integer;
    level : string;
    message : string;
  end;
  
  TNested_events_itemsArray = Array of TNested_events_items;
  TFineTunesResponse_hyperparams = Class(TObject)
    batch_size : integer;
    learning_rate_multiplier : double;
    n_epochs : integer;
    prompt_loss_weight : double;
  end;
  
  TNested_training_files_items = Class(TObject)
    id : string;
    object_ : string;
    bytes : integer;
    created_at : integer;
    filename : string;
    purpose : string;
  end;
  TNested_training_files_itemsArray = Array of TNested_training_files_items;

  { TFineTunesResponse }

  TFineTunesResponse = Class(TObject)
    created_at : integer;
    events : TNested_events_itemsArray;
    fine_tuned_model : string;
    hyperparams : TFineTunesResponse_hyperparams;
    id : string;
    model : string;
    object_ : string;
    organization_id : string;
    result_files : stringArray;
    status : string;
    training_files : TNested_training_files_itemsArray;
    updated_at : integer;
    validation_files : stringArray;
    destructor destroy; override;
  end;
  
  TGetFineTuneResponse_hyperparams = Class(TObject)
    batch_size : integer;
    learning_rate_multiplier : double;
    n_epochs : integer;
    prompt_loss_weight : double;
  end;
  
  TNested_result_files_items = Class(TObject)
    id : string;
    object_ : string;
    bytes : integer;
    created_at : integer;
    filename : string;
    purpose : string;
  end;
  TNested_result_files_itemsArray = Array of TNested_result_files_items;

  { TGetFineTuneResponse }

  TGetFineTuneResponse = Class(TObject)
    created_at : integer;
    events : TNested_events_itemsArray;
    fine_tuned_model : string;
    hyperparams : TGetFineTuneResponse_hyperparams;
    id : string;
    model : string;
    object_ : string;
    organization_id : string;
    result_files : TNested_result_files_itemsArray;
    status : string;
    training_files : TNested_training_files_itemsArray;
    updated_at : integer;
    validation_files : stringArray;
    destructor destroy; override;
  end;
  
  { TGetFineTunesResponse }

  TGetFineTunesResponse = Class(TObject)
    data : TNested_data_itemsArray;
    object_ : string;
    destructor destroy; override;
  end;
  
  TImageEditRequest = Class(TObject)
    image : string;
    mask : string;
    n : integer;
    prompt : string;
    response_format : string;
    size : string;
    user : string;
  end;
  
  { TImagesResponse }

  TImagesResponse = Class(TObject)
    created : integer;
    data : TNested_data_itemsArray;
    destructor destroy; override;
  end;
  
  TImageValidationRequest = Class(TObject)
    image : string;
    n : integer;
    response_format : string;
    size : string;
    user : string;
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

  { TModel }

  TModel = Class(TObject)
    created : double;
    id : string;
    object_ : string;
    owned_by : string;
    parent : string;
    permission : TNested_permission_itemsArray;
    root : string;
    destructor Destroy; override;
  end;
  TModelArray = Array of TModel;

  { TModels }

  TModels = Class(TObject)
    data : TModelArray;
    object_ : string;
    destructor Destroy; override;
  end;
  
  TModerationRequest = Class(TObject)
    input : string;
    model : string;
  end;
  
  T_Nested_results_items__categories = Class(TObject)
    hate : boolean;
    hate_threatening : boolean;
    self_harm : boolean;
    sexual : boolean;
    sexual_minors : boolean;
    violence : boolean;
    violence_graphic : boolean;
  end;
  
  T_Nested_results_items__category_scores = Class(TObject)
    hate : double;
    hate_threatening : double;
    self_harm : double;
    sexual : double;
    sexual_minors : double;
    violence : double;
    violence_graphic : double;
  end;
  
  { TNested_results_items }

  TNested_results_items = Class(TObject)
    categories : T_Nested_results_items__categories;
    category_scores : T_Nested_results_items__category_scores;
    flagged : boolean;
    destructor destroy; override;
  end;
  
  TNested_results_itemsArray = Array of TNested_results_items;

  { TModerationResponse }

  TModerationResponse = Class(TObject)
    id : string;
    model : string;
    results : TNested_results_itemsArray;
    destructor destroy; override;
  end;
  
  TRetrieveFileResponse = Class(TObject)
    bytes : integer;
    created_at : integer;
    filename : string;
    id : string;
    object_ : string;
    purpose : string;
  end;
  
  TUploadFileRequest = Class(TObject)
    file_ : string;
    purpose : string;
  end;
  
  TUploadFileResponse = Class(TObject)
    bytes : integer;
    created_at : integer;
    filename : string;
    id : string;
    object_ : string;
    purpose : string;
  end;
  
implementation

uses sysutils;

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

{ TEditResponse }

destructor TEditResponse.destroy;
var
  i : Integer;
begin
  for I:=0 to Length(Choices)-1 do
    FreeAndNil(Choices[i]);
  FreeAndNil(usage);
  inherited destroy;
end;

{ TEmbeddingsResponse }

destructor TEmbeddingsResponse.destroy;
var
  i : Integer;
begin
  for I:=0 to Length(data)-1 do
    FreeAndNil(data[i]);
  FreeAndNil(usage);
  inherited destroy;
end;

{ TError }

destructor TError.destroy;
begin
  FreeAndNil(error);
  inherited destroy;
end;

{ TFileResponse }

destructor TFileResponse.Destroy;
var
  i : Integer;
begin
  for I:=0 to Length(data)-1 do
    FreeAndNil(data[i]);
  inherited Destroy;
end;

{ TFineTuneEventsResponse }

destructor TFineTuneEventsResponse.Destroy;
var
  i : Integer;
begin
  for I:=0 to Length(data)-1 do
    FreeAndNil(data[i]);
  inherited Destroy;
end;

{ TFineTunesResponse }

destructor TFineTunesResponse.destroy;
var
  i : Integer;
begin
  for I:=0 to Length(events)-1 do
    FreeAndNil(events[i]);
  FreeAndNil(hyperparams);
  for I:=0 to Length(training_files)-1 do
    FreeAndNil(training_files[i]);
  inherited destroy;
end;

{ TGetFineTuneResponse }

destructor TGetFineTuneResponse.destroy;
var
  i : Integer;
begin
  for I:=0 to Length(events)-1 do
    FreeAndNil(events[i]);
  FreeAndNil(hyperparams);
  for I:=0 to Length(result_files)-1 do
    FreeAndNil(result_files[i]);
  for I:=0 to Length(training_files)-1 do
    FreeAndNil(training_files[i]);
  inherited destroy;
end;

{ TGetFineTunesResponse }

destructor TGetFineTunesResponse.destroy;
var
  i : Integer;
begin
  for I:=0 to Length(data)-1 do
    FreeAndNil(data[i]);
  inherited destroy;
end;

{ TImagesResponse }

destructor TImagesResponse.destroy;
var
  i : Integer;
begin
  for I:=0 to Length(data)-1 do
    FreeAndNil(data[i]);
  inherited destroy;
end;

{ TModel }

destructor TModel.Destroy;
var
  i : Integer;
begin
  for I:=0 to Length(permission)-1 do
    FreeAndNil(permission[i]);
  inherited Destroy;
end;

{ TModels }

destructor TModels.Destroy;
var
  i : Integer;
begin
  for I:=0 to Length(data)-1 do
    FreeAndNil(data[i]);
  inherited Destroy;
end;

{ TNested_results_items }

destructor TNested_results_items.destroy;
begin
  FreeAndNil(categories);
  FreeAndNil(category_scores);

  inherited destroy;
end;

{ TModerationResponse }

destructor TModerationResponse.destroy;
var
  i : Integer;
begin
  for I:=0 to Length(Results)-1 do
    FreeAndNil(Results[i]);
  Inherited;
end;

end.
