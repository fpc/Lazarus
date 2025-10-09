{
    This file is part of the Free Component Library
    Copyright (c) 2025 by Michael Van Canneyt michael@freepascal.org

    OLLama Rest API -  Data transfer objects

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit ollama.Dto;

{$mode objfpc}
{$h+}


interface

uses types, fpjson;

Type

  Tgithub_com_ollama_ollama_types_model_Capability = string;
  Tgithub_com_ollama_ollama_types_model_CapabilityArray = Array of Tgithub_com_ollama_ollama_types_model_Capability;
  
  TIntegerDynArrayArray = Array of TIntegerDynArray;
  TDoubleDynArrayArray = Array of TDoubleDynArray;
  
  Tapi_ToolCallFunctionArguments = Class(TObject)
  end;
  
  { Tapi_ToolCallFunction }

  Tapi_ToolCallFunction = Class(TObject)
    arguments : Tapi_ToolCallFunctionArguments;
    index : integer;
    name : string;
    constructor CreateWithMembers;
    destructor destroy; override;
  end;
  
  Tapi_ToolCall = Class(TObject)
    function_ : Tapi_ToolCallFunction;
    constructor CreateWithMembers;
    destructor destroy; override;
  end;
  
  Tapi_ToolCallArray = Array of Tapi_ToolCall;

  { Tapi_Message }

  Tapi_Message = Class(TObject)
    content : string;
    images : TIntegerDynArrayArray;
    role : string;
    thinking : string;
    tool_calls : Tapi_ToolCallArray;
    destructor Destroy; override;
  end;
  
  Tapi_MessageArray = Array of Tapi_Message;
  Tapi_ToolFunction_parameters = Class(TObject)
    _defs : string;
    items : string;
    properties : string;
    required : TStringDynArray;
    type_ : string;
  end;
  
  { Tapi_ToolFunction }

  Tapi_ToolFunction = Class(TObject)
    description : string;
    name : string;
    parameters : Tapi_ToolFunction_parameters;
    destructor Destroy; override;
  end;
  
  Tapi_Tool = Class(TObject)
    function_ : Tapi_ToolFunction;
    items : string;
    type_ : string;
    constructor CreateWithMembers;
    destructor Destroy; override;
  end;
  
  Tapi_ToolArray = Array of Tapi_Tool;

  { Tapi_ChatRequest }

  Tapi_ChatRequest = Class(TObject)
    format : string;
    keep_alive : integer;
    messages : Tapi_MessageArray;
    model : string;
    options : TJSONObject;
    stream : boolean;
    think : boolean;
    tools : Tapi_ToolArray;
    destructor Destroy; override;
  end;
  
  { Tapi_ChatResponse }

  Tapi_ChatResponse = Class(TObject)
    created_at : string;
    done : boolean;
    done_reason : string;
    eval_count : integer;
    eval_duration : integer;
    load_duration : integer;
    message : Tapi_Message;
    model : string;
    prompt_eval_count : integer;
    prompt_eval_duration : integer;
    total_duration : integer;
    constructor CreateWithMembers;
    destructor Destroy; override;
  end;
  
  Tapi_CopyRequest = Class(TObject)
    destination : string;
    source : string;
  end;
  
  { Tapi_CreateRequest }

  Tapi_CreateRequest = Class(TObject)
    adapters : string;
    files : string;
    from : string;
    license : string;
    messages : Tapi_MessageArray;
    model : string;
    name : string;
    parameters : string;
    quantization : string;
    quantize : string;
    stream : boolean;
    system : string;
    template : string;
    destructor Destroy; override;
  end;
  
  Tapi_DeleteRequest = Class(TObject)
    model : string;
    name : string;
  end;
  
  Tapi_EmbeddingRequest = Class(TObject)
    keep_alive : string;
    model : string;
    options : string;
    prompt : string;
  end;
  
  Tapi_EmbeddingResponse = Class(TObject)
    embedding : TDoubleDynArray;
  end;
  
  Tapi_EmbedRequest = Class(TObject)
    input : string;
    keep_alive : string;
    model : string;
    options : string;
    truncate : boolean;
  end;
  
  Tapi_EmbedResponse = Class(TObject)
    embeddings : TDoubleDynArrayArray;
    load_duration : integer;
    model : string;
    prompt_eval_count : integer;
    total_duration : integer;
  end;
  
  Tapi_GenerateRequest = Class(TObject)
    context : TIntegerDynArray;
    format : string;
    images : TIntegerDynArrayArray;
    keep_alive : string;
    model : string;
    options : string;
    prompt : string;
    raw : boolean;
    stream : boolean;
    suffix : string;
    system : string;
    template : string;
    think : boolean;
  end;
  
  Tapi_GenerateResponse = Class(TObject)
    context : TIntegerDynArray;
    created_at : string;
    done : boolean;
    done_reason : string;
    eval_count : integer;
    eval_duration : integer;
    load_duration : integer;
    model : string;
    prompt_eval_count : integer;
    prompt_eval_duration : integer;
    response : string;
    thinking : string;
    total_duration : integer;
  end;
  
  Tapi_ModelDetails = Class(TObject)
    families : TStringDynArray;
    family : string;
    format : string;
    parameter_size : string;
    parent_model : string;
    quantization_level : string;
  end;
  
  { Tapi_ListModelResponse }

  Tapi_ListModelResponse = Class(TObject)
    details : Tapi_ModelDetails;
    digest : string;
    model : string;
    modified_at : string;
    name : string;
    size : integer;
    constructor CreateWithMembers;
    destructor Destroy; override;
  end;
  
  Tapi_ListModelResponseArray = Array of Tapi_ListModelResponse;

  { Tapi_ListResponse }

  Tapi_ListResponse = Class(TObject)
    models : Tapi_ListModelResponseArray;
    destructor Destroy; override;
  end;
  
  { Tapi_ProcessModelResponse }

  Tapi_ProcessModelResponse = Class(TObject)
    details : Tapi_ModelDetails;
    digest : string;
    expires_at : string;
    model : string;
    name : string;
    size : integer;
    size_vram : integer;
    constructor CreateWithMembers;
    destructor Destroy; override;
  end;
  
  Tapi_ProcessModelResponseArray = Array of Tapi_ProcessModelResponse;

  { Tapi_ProcessResponse }

  Tapi_ProcessResponse = Class(TObject)
    models : Tapi_ProcessModelResponseArray;
    destructor Destroy; override;
  end;
  
  Tapi_ProgressResponse = Class(TObject)
    completed : integer;
    digest : string;
    status : string;
    total : integer;
  end;
  
  Tapi_PullRequest = Class(TObject)
    insecure : boolean;
    model : string;
    name : string;
    password : string;
    stream : boolean;
    username : string;
  end;
  
  Tapi_PushRequest = Class(TObject)
    insecure : boolean;
    model : string;
    name : string;
    password : string;
    stream : boolean;
    username : string;
  end;
  
  Tapi_ShowRequest = Class(TObject)
    model : string;
    name : string;
    options : string;
    system : string;
    template : string;
    verbose : boolean;
  end;
  
  Tapi_Tensor = Class(TObject)
    name : string;
    shape : TIntegerDynArray;
    type_ : string;
  end;
  
  Tapi_TensorArray = Array of Tapi_Tensor;

  { Tapi_ShowResponse }

  Tapi_ShowResponse = Class(TObject)
    capabilities : Tgithub_com_ollama_ollama_types_model_CapabilityArray;
    details : Tapi_ModelDetails;
    license : string;
    messages : Tapi_MessageArray;
    modelfile : string;
    model_info : string;
    modified_at : string;
    parameters : string;
    projector_info : string;
    system : string;
    template : string;
    tensors : Tapi_TensorArray;
    constructor CreateWithMembers;
    destructor Destroy; override;
  end;
  
implementation

uses sysutils;

constructor Tapi_ToolCallFunction.CreateWithMembers;

begin
  arguments := Tapi_ToolCallFunctionArguments.Create;
end;

destructor Tapi_ToolCallFunction.destroy;
begin
  FreeAndNil(arguments);
  inherited destroy;
end;

constructor Tapi_ToolCall.CreateWithMembers;

begin
  function_ := Tapi_ToolCallFunction.CreateWithMembers;
end;

destructor Tapi_ToolCall.destroy;
begin
  FreeAndNil(function_);
  inherited destroy;
end;

{ Tapi_Message }

destructor Tapi_Message.Destroy;
var
  I : integer;
begin
  for I:=0 to length(tool_calls)-1 do
    FreeAndNil(tool_calls[I]);
  inherited Destroy;
end;

{ Tapi_ToolFunction }

destructor Tapi_ToolFunction.Destroy;
begin
  FreeAndNil(parameters);
  inherited Destroy;
end;

constructor Tapi_Tool.CreateWithMembers;

begin
  function_ := Tapi_ToolFunction.Create;
end;

destructor Tapi_Tool.Destroy;
begin
  FreeAndNil(function_);
  inherited Destroy;
end;

{ Tapi_ChatRequest }

destructor Tapi_ChatRequest.Destroy;
var
  I : integer;
begin
  for I:=0 to length(messages)-1 do
    FreeAndNil(messages[I]);
  for I:=0 to length(tools)-1 do
    FreeAndNil(tools[I]);
  FreeAndNil(options);
  inherited Destroy;
end;

constructor Tapi_ChatResponse.CreateWithMembers;

begin
  message := Tapi_Message.Create;
end;

destructor Tapi_ChatResponse.Destroy;
begin
  FreeAndNil(message);
  inherited Destroy;
end;

{ Tapi_CreateRequest }

destructor Tapi_CreateRequest.Destroy;
var
  I : integer;
begin
  for I:=0 to length(messages)-1 do
    FreeAndNil(messages[I]);
  inherited Destroy;
end;

constructor Tapi_ListModelResponse.CreateWithMembers;

begin
  details := Tapi_ModelDetails.Create;
end;

destructor Tapi_ListModelResponse.Destroy;
begin
  FreeAndNil(details);
  inherited Destroy;
end;

{ Tapi_ListResponse }

destructor Tapi_ListResponse.Destroy;
var
  I : integer;
begin
  for I:=0 to length(models)-1 do
    FreeAndNil(models[I]);
  inherited Destroy;
end;

constructor Tapi_ProcessModelResponse.CreateWithMembers;

begin
  details := Tapi_ModelDetails.Create;
end;

destructor Tapi_ProcessModelResponse.Destroy;
begin
  FreeAndNil(details);
  inherited Destroy;
end;

{ Tapi_ProcessResponse }

destructor Tapi_ProcessResponse.Destroy;
var
  I : integer;
begin
  for I:=0 to length(models)-1 do
    FreeAndNil(models[I]);
  inherited Destroy;
end;

constructor Tapi_ShowResponse.CreateWithMembers;

begin
  details := Tapi_ModelDetails.Create;
end;

destructor Tapi_ShowResponse.Destroy;
var
  I : integer;
begin
  for I:=0 to length(messages)-1 do
    FreeAndNil(messages[I]);
  for I:=0 to length(tensors)-1 do
    FreeAndNil(tensors[I]);
  FreeAndNil(details);
  inherited Destroy;
end;

end.
