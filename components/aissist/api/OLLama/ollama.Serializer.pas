{ -----------------------------------------------------------------------
  Do not edit !
  
  This file was automatically generated on 2025-10-04 16:13.
  Used command-line parameters:
     -i ollama.json -r -o ollama
  Source OpenAPI document data:
    Title: Ollama API
    Version: 1.0
  -----------------------------------------------------------------------}
unit ollama.Serializer;

interface

{$mode objfpc}
{$h+}
{$modeswitch typehelpers}


uses
  Types,
  fpJSON,
  ollama.Dto;

Type
  Tapi_ToolCallFunctionArgumentsSerializer = class helper for Tapi_ToolCallFunctionArguments
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : Tapi_ToolCallFunctionArguments; overload; static;
    class function Deserialize(aJSON : String) : Tapi_ToolCallFunctionArguments; overload; static;
  end;
  
  Tapi_ToolCallFunctionSerializer = class helper for Tapi_ToolCallFunction
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : Tapi_ToolCallFunction; overload; static;
    class function Deserialize(aJSON : String) : Tapi_ToolCallFunction; overload; static;
  end;
  
  Tapi_ToolCallSerializer = class helper for Tapi_ToolCall
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : Tapi_ToolCall; overload; static;
    class function Deserialize(aJSON : String) : Tapi_ToolCall; overload; static;
  end;
  
  Tapi_MessageSerializer = class helper for Tapi_Message
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : Tapi_Message; overload; static;
    class function Deserialize(aJSON : String) : Tapi_Message; overload; static;
  end;
  
  Tapi_ToolFunction_parametersSerializer = class helper for Tapi_ToolFunction_parameters
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : Tapi_ToolFunction_parameters; overload; static;
    class function Deserialize(aJSON : String) : Tapi_ToolFunction_parameters; overload; static;
  end;
  
  Tapi_ToolFunctionSerializer = class helper for Tapi_ToolFunction
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : Tapi_ToolFunction; overload; static;
    class function Deserialize(aJSON : String) : Tapi_ToolFunction; overload; static;
  end;
  
  Tapi_ToolSerializer = class helper for Tapi_Tool
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : Tapi_Tool; overload; static;
    class function Deserialize(aJSON : String) : Tapi_Tool; overload; static;
  end;
  
  Tapi_ChatRequestSerializer = class helper for Tapi_ChatRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : Tapi_ChatRequest; overload; static;
    class function Deserialize(aJSON : String) : Tapi_ChatRequest; overload; static;
  end;
  
  Tapi_ChatResponseSerializer = class helper for Tapi_ChatResponse
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : Tapi_ChatResponse; overload; static;
    class function Deserialize(aJSON : String) : Tapi_ChatResponse; overload; static;
  end;
  
  Tapi_CopyRequestSerializer = class helper for Tapi_CopyRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : Tapi_CopyRequest; overload; static;
    class function Deserialize(aJSON : String) : Tapi_CopyRequest; overload; static;
  end;
  
  Tapi_CreateRequestSerializer = class helper for Tapi_CreateRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : Tapi_CreateRequest; overload; static;
    class function Deserialize(aJSON : String) : Tapi_CreateRequest; overload; static;
  end;
  
  Tapi_DeleteRequestSerializer = class helper for Tapi_DeleteRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : Tapi_DeleteRequest; overload; static;
    class function Deserialize(aJSON : String) : Tapi_DeleteRequest; overload; static;
  end;
  
  Tapi_EmbeddingRequestSerializer = class helper for Tapi_EmbeddingRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : Tapi_EmbeddingRequest; overload; static;
    class function Deserialize(aJSON : String) : Tapi_EmbeddingRequest; overload; static;
  end;
  
  Tapi_EmbeddingResponseSerializer = class helper for Tapi_EmbeddingResponse
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : Tapi_EmbeddingResponse; overload; static;
    class function Deserialize(aJSON : String) : Tapi_EmbeddingResponse; overload; static;
  end;
  
  Tapi_EmbedRequestSerializer = class helper for Tapi_EmbedRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : Tapi_EmbedRequest; overload; static;
    class function Deserialize(aJSON : String) : Tapi_EmbedRequest; overload; static;
  end;
  
  Tapi_EmbedResponseSerializer = class helper for Tapi_EmbedResponse
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : Tapi_EmbedResponse; overload; static;
    class function Deserialize(aJSON : String) : Tapi_EmbedResponse; overload; static;
  end;
  
  Tapi_GenerateRequestSerializer = class helper for Tapi_GenerateRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : Tapi_GenerateRequest; overload; static;
    class function Deserialize(aJSON : String) : Tapi_GenerateRequest; overload; static;
  end;
  
  Tapi_GenerateResponseSerializer = class helper for Tapi_GenerateResponse
  end;
  
  Tapi_ModelDetailsSerializer = class helper for Tapi_ModelDetails
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : Tapi_ModelDetails; overload; static;
    class function Deserialize(aJSON : String) : Tapi_ModelDetails; overload; static;
  end;
  
  Tapi_ListModelResponseSerializer = class helper for Tapi_ListModelResponse
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : Tapi_ListModelResponse; overload; static;
    class function Deserialize(aJSON : String) : Tapi_ListModelResponse; overload; static;
  end;
  
  Tapi_ListResponseSerializer = class helper for Tapi_ListResponse
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : Tapi_ListResponse; overload; static;
    class function Deserialize(aJSON : String) : Tapi_ListResponse; overload; static;
  end;
  
  Tapi_ProcessModelResponseSerializer = class helper for Tapi_ProcessModelResponse
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : Tapi_ProcessModelResponse; overload; static;
    class function Deserialize(aJSON : String) : Tapi_ProcessModelResponse; overload; static;
  end;
  
  Tapi_ProcessResponseSerializer = class helper for Tapi_ProcessResponse
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : Tapi_ProcessResponse; overload; static;
    class function Deserialize(aJSON : String) : Tapi_ProcessResponse; overload; static;
  end;
  
  Tapi_ProgressResponseSerializer = class helper for Tapi_ProgressResponse
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : Tapi_ProgressResponse; overload; static;
    class function Deserialize(aJSON : String) : Tapi_ProgressResponse; overload; static;
  end;
  
  Tapi_PullRequestSerializer = class helper for Tapi_PullRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : Tapi_PullRequest; overload; static;
    class function Deserialize(aJSON : String) : Tapi_PullRequest; overload; static;
  end;
  
  Tapi_PushRequestSerializer = class helper for Tapi_PushRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : Tapi_PushRequest; overload; static;
    class function Deserialize(aJSON : String) : Tapi_PushRequest; overload; static;
  end;
  
  Tapi_ShowRequestSerializer = class helper for Tapi_ShowRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : Tapi_ShowRequest; overload; static;
    class function Deserialize(aJSON : String) : Tapi_ShowRequest; overload; static;
  end;
  
  Tapi_TensorSerializer = class helper for Tapi_Tensor
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : Tapi_Tensor; overload; static;
    class function Deserialize(aJSON : String) : Tapi_Tensor; overload; static;
  end;
  
  Tapi_ShowResponseSerializer = class helper for Tapi_ShowResponse
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : Tapi_ShowResponse; overload; static;
    class function Deserialize(aJSON : String) : Tapi_ShowResponse; overload; static;
  end;
  
  TIntegerDynArraySerializer = type helper for TIntegerDynArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TIntegerDynArray; overload; static;
    class function Deserialize(aJSON : String) : TIntegerDynArray; overload; static;
  end;
  TDoubleDynArraySerializer = type helper for TDoubleDynArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TDoubleDynArray; overload; static;
    class function Deserialize(aJSON : String) : TDoubleDynArray; overload; static;
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

function Tapi_ToolCallFunctionArgumentsSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
  except
    Result.Free;
    raise;
  end;
end;

function Tapi_ToolCallFunctionArgumentsSerializer.Serialize : String;
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

class function Tapi_ToolCallFunctionArgumentsSerializer.Deserialize(aJSON : TJSONObject) : Tapi_ToolCallFunctionArguments;

begin
  Result := Tapi_ToolCallFunctionArguments.Create;
  If (aJSON=Nil) then
    exit;
end;

class function Tapi_ToolCallFunctionArgumentsSerializer.Deserialize(aJSON : String) : Tapi_ToolCallFunctionArguments;

var
  lObj : TJSONObject;
begin
  Result := Default(Tapi_ToolCallFunctionArguments);
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

function Tapi_ToolCallFunctionSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    if Assigned(arguments) then
      Result.Add('arguments',arguments.SerializeObject);
    Result.Add('index',index);
    Result.Add('name',name);
  except
    Result.Free;
    raise;
  end;
end;

function Tapi_ToolCallFunctionSerializer.Serialize : String;
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

class function Tapi_ToolCallFunctionSerializer.Deserialize(aJSON : TJSONObject) : Tapi_ToolCallFunction;

begin
  Result := Tapi_ToolCallFunction.Create;
  If (aJSON=Nil) then
    exit;
  Result.arguments:=Tapi_ToolCallFunctionArguments.Deserialize(aJSON.Get('arguments',TJSONObject(Nil)));
  Result.index:=aJSON.Get('index',0);
  Result.name:=aJSON.Get('name','');
end;

class function Tapi_ToolCallFunctionSerializer.Deserialize(aJSON : String) : Tapi_ToolCallFunction;

var
  lObj : TJSONObject;
begin
  Result := Default(Tapi_ToolCallFunction);
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

function Tapi_ToolCallSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    if Assigned(function_) then
      Result.Add('function',function_.SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function Tapi_ToolCallSerializer.Serialize : String;
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

class function Tapi_ToolCallSerializer.Deserialize(aJSON : TJSONObject) : Tapi_ToolCall;

begin
  Result := Tapi_ToolCall.Create;
  If (aJSON=Nil) then
    exit;
  Result.function_:=Tapi_ToolCallFunction.Deserialize(aJSON.Get('function',TJSONObject(Nil)));
end;

class function Tapi_ToolCallSerializer.Deserialize(aJSON : String) : Tapi_ToolCall;

var
  lObj : TJSONObject;
begin
  Result := Default(Tapi_ToolCall);
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

function Tapi_MessageSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('content',content);
    Arr:=TJSONArray.Create;
    Result.Add('images',Arr);
    For I:=0 to Length(images)-1 do
      Arr.Add(images[i].SerializeArray);
    Result.Add('role',role);
    Result.Add('thinking',thinking);
    Arr:=TJSONArray.Create;
    Result.Add('tool_calls',Arr);
    For I:=0 to Length(tool_calls)-1 do
      Arr.Add(tool_calls[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function Tapi_MessageSerializer.Serialize : String;
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

class function Tapi_MessageSerializer.Deserialize(aJSON : TJSONObject) : Tapi_Message;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := Tapi_Message.Create;
  If (aJSON=Nil) then
    exit;
  Result.content:=aJSON.Get('content','');
  lArr:=aJSON.Get('images',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.images,lArr.Count);
    For I:=0 to Length(Result.images)-1 do
      Result.images[i]:=TIntegerDynArray.Deserialize(lArr[i] as TJSONArray);
    end;
  Result.role:=aJSON.Get('role','');
  Result.thinking:=aJSON.Get('thinking','');
  lArr:=aJSON.Get('tool_calls',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.tool_calls,lArr.Count);
    For I:=0 to Length(Result.tool_calls)-1 do
      Result.tool_calls[i]:=Tapi_ToolCall.Deserialize(lArr[i] as TJSONObject);
    end;
end;

class function Tapi_MessageSerializer.Deserialize(aJSON : String) : Tapi_Message;

var
  lObj : TJSONObject;
begin
  Result := Default(Tapi_Message);
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

function Tapi_ToolFunction_parametersSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    if (_defs<>'') then
      Result.Add('$defs',GetJSON(_defs));
    if (items<>'') then
      Result.Add('items',GetJSON(items));
    if (properties<>'') then
      Result.Add('properties',GetJSON(properties));
    Arr:=TJSONArray.Create;
    Result.Add('required',Arr);
    For I:=0 to Length(required)-1 do
      Arr.Add(required[i]);
    Result.Add('type',type_);
  except
    Result.Free;
    raise;
  end;
end;

function Tapi_ToolFunction_parametersSerializer.Serialize : String;
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

class function Tapi_ToolFunction_parametersSerializer.Deserialize(aJSON : TJSONObject) : Tapi_ToolFunction_parameters;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := Tapi_ToolFunction_parameters.Create;
  If (aJSON=Nil) then
    exit;
  Result._defs:=aJSON.Get('$defs',TJSONObject(Nil)).AsJSON;
  Result.items:=aJSON.Get('items',TJSONObject(Nil)).AsJSON;
  Result.properties:=aJSON.Get('properties',TJSONObject(Nil)).AsJSON;
  lArr:=aJSON.Get('required',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.required,lArr.Count);
    For I:=0 to Length(Result.required)-1 do
      Result.required[i]:=lArr[i].Asstring;
    end;
  Result.type_:=aJSON.Get('type','');
end;

class function Tapi_ToolFunction_parametersSerializer.Deserialize(aJSON : String) : Tapi_ToolFunction_parameters;

var
  lObj : TJSONObject;
begin
  Result := Default(Tapi_ToolFunction_parameters);
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

function Tapi_ToolFunctionSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('description',description);
    Result.Add('name',name);
    if Assigned(parameters) then
      Result.Add('parameters',parameters.SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function Tapi_ToolFunctionSerializer.Serialize : String;
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

class function Tapi_ToolFunctionSerializer.Deserialize(aJSON : TJSONObject) : Tapi_ToolFunction;

begin
  Result := Tapi_ToolFunction.Create;
  If (aJSON=Nil) then
    exit;
  Result.description:=aJSON.Get('description','');
  Result.name:=aJSON.Get('name','');
  Result.parameters:=Tapi_ToolFunction_parameters.Deserialize(aJSON.Get('parameters',TJSONObject(Nil)));
end;

class function Tapi_ToolFunctionSerializer.Deserialize(aJSON : String) : Tapi_ToolFunction;

var
  lObj : TJSONObject;
begin
  Result := Default(Tapi_ToolFunction);
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

function Tapi_ToolSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    if Assigned(function_) then
      Result.Add('function',function_.SerializeObject);
    if (items<>'') then
      Result.Add('items',GetJSON(items));
    Result.Add('type',type_);
  except
    Result.Free;
    raise;
  end;
end;

function Tapi_ToolSerializer.Serialize : String;
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

class function Tapi_ToolSerializer.Deserialize(aJSON : TJSONObject) : Tapi_Tool;

begin
  Result := Tapi_Tool.Create;
  If (aJSON=Nil) then
    exit;
  Result.function_:=Tapi_ToolFunction.Deserialize(aJSON.Get('function',TJSONObject(Nil)));
  Result.items:=aJSON.Get('items',TJSONObject(Nil)).AsJSON;
  Result.type_:=aJSON.Get('type','');
end;

class function Tapi_ToolSerializer.Deserialize(aJSON : String) : Tapi_Tool;

var
  lObj : TJSONObject;
begin
  Result := Default(Tapi_Tool);
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

function Tapi_ChatRequestSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('format',format);
    Result.Add('keep_alive',keep_alive);
    Arr:=TJSONArray.Create;
    Result.Add('messages',Arr);
    For I:=0 to Length(messages)-1 do
      Arr.Add(messages[i].SerializeObject);
    Result.Add('model',model);
    if (options<>nil) then
      Result.Add('options',options.clone);
    Result.Add('stream',stream);
    Result.Add('think',think);
    Arr:=TJSONArray.Create;
    Result.Add('tools',Arr);
    For I:=0 to Length(tools)-1 do
      Arr.Add(tools[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function Tapi_ChatRequestSerializer.Serialize : String;
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

class function Tapi_ChatRequestSerializer.Deserialize(aJSON : TJSONObject) : Tapi_ChatRequest;

var
  lArr : TJSONArray;
  i : Integer;
  lObj : TJSONObject;
begin
  Result := Tapi_ChatRequest.Create;
  If (aJSON=Nil) then
    exit;
  Result.format:=aJSON.Get('format','');
  Result.keep_alive:=aJSON.Get('keep_alive',0);
  lArr:=aJSON.Get('messages',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.messages,lArr.Count);
    For I:=0 to Length(Result.messages)-1 do
      Result.messages[i]:=Tapi_Message.Deserialize(lArr[i] as TJSONObject);
    end;
  Result.model:=aJSON.Get('model','');
  lObj:=aJSON.Get('options',TJSONObject(Nil));
  if Assigned(lObj) then
    Result.options:=lObj.Clone as TJSONObject;
  Result.stream:=aJSON.Get('stream',False);
  Result.think:=aJSON.Get('think',False);
  lArr:=aJSON.Get('tools',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.tools,lArr.Count);
    For I:=0 to Length(Result.tools)-1 do
      Result.tools[i]:=Tapi_Tool.Deserialize(lArr[i] as TJSONObject);
    end;
end;

class function Tapi_ChatRequestSerializer.Deserialize(aJSON : String) : Tapi_ChatRequest;

var
  lObj : TJSONObject;
begin
  Result := Default(Tapi_ChatRequest);
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

function Tapi_ChatResponseSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('created_at',created_at);
    Result.Add('done',done);
    Result.Add('done_reason',done_reason);
    Result.Add('eval_count',eval_count);
    Result.Add('eval_duration',eval_duration);
    Result.Add('load_duration',load_duration);
    if Assigned(message) then
      Result.Add('message',message.SerializeObject);
    Result.Add('model',model);
    Result.Add('prompt_eval_count',prompt_eval_count);
    Result.Add('prompt_eval_duration',prompt_eval_duration);
    Result.Add('total_duration',total_duration);
  except
    Result.Free;
    raise;
  end;
end;

function Tapi_ChatResponseSerializer.Serialize : String;
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

class function Tapi_ChatResponseSerializer.Deserialize(aJSON : TJSONObject) : Tapi_ChatResponse;

begin
  Result := Tapi_ChatResponse.Create;
  If (aJSON=Nil) then
    exit;
  Result.created_at:=aJSON.Get('created_at','');
  Result.done:=aJSON.Get('done',False);
  Result.done_reason:=aJSON.Get('done_reason','');
  Result.eval_count:=aJSON.Get('eval_count',0);
  Result.eval_duration:=aJSON.Get('eval_duration',0);
  Result.load_duration:=aJSON.Get('load_duration',0);
  Result.message:=Tapi_Message.Deserialize(aJSON.Get('message',TJSONObject(Nil)));
  Result.model:=aJSON.Get('model','');
  Result.prompt_eval_count:=aJSON.Get('prompt_eval_count',0);
  Result.prompt_eval_duration:=aJSON.Get('prompt_eval_duration',0);
  Result.total_duration:=aJSON.Get('total_duration',0);
end;

class function Tapi_ChatResponseSerializer.Deserialize(aJSON : String) : Tapi_ChatResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(Tapi_ChatResponse);
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

function Tapi_CopyRequestSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('destination',destination);
    Result.Add('source',source);
  except
    Result.Free;
    raise;
  end;
end;

function Tapi_CopyRequestSerializer.Serialize : String;
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

class function Tapi_CopyRequestSerializer.Deserialize(aJSON : TJSONObject) : Tapi_CopyRequest;

begin
  Result := Tapi_CopyRequest.Create;
  If (aJSON=Nil) then
    exit;
  Result.destination:=aJSON.Get('destination','');
  Result.source:=aJSON.Get('source','');
end;

class function Tapi_CopyRequestSerializer.Deserialize(aJSON : String) : Tapi_CopyRequest;

var
  lObj : TJSONObject;
begin
  Result := Default(Tapi_CopyRequest);
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

function Tapi_CreateRequestSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    if (adapters<>'') then
      Result.Add('adapters',GetJSON(adapters));
    if (files<>'') then
      Result.Add('files',GetJSON(files));
    Result.Add('from',from);
    if (license<>'') then
      Result.Add('license',GetJSON(license));
    Arr:=TJSONArray.Create;
    Result.Add('messages',Arr);
    For I:=0 to Length(messages)-1 do
      Arr.Add(messages[i].SerializeObject);
    Result.Add('model',model);
    Result.Add('name',name);
    if (parameters<>'') then
      Result.Add('parameters',GetJSON(parameters));
    Result.Add('quantization',quantization);
    Result.Add('quantize',quantize);
    Result.Add('stream',stream);
    Result.Add('system',system);
    Result.Add('template',template);
  except
    Result.Free;
    raise;
  end;
end;

function Tapi_CreateRequestSerializer.Serialize : String;
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

class function Tapi_CreateRequestSerializer.Deserialize(aJSON : TJSONObject) : Tapi_CreateRequest;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := Tapi_CreateRequest.Create;
  If (aJSON=Nil) then
    exit;
  Result.adapters:=aJSON.Get('adapters',TJSONObject(Nil)).AsJSON;
  Result.files:=aJSON.Get('files',TJSONObject(Nil)).AsJSON;
  Result.from:=aJSON.Get('from','');
  Result.license:=aJSON.Get('license',TJSONObject(Nil)).AsJSON;
  lArr:=aJSON.Get('messages',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.messages,lArr.Count);
    For I:=0 to Length(Result.messages)-1 do
      Result.messages[i]:=Tapi_Message.Deserialize(lArr[i] as TJSONObject);
    end;
  Result.model:=aJSON.Get('model','');
  Result.name:=aJSON.Get('name','');
  Result.parameters:=aJSON.Get('parameters',TJSONObject(Nil)).AsJSON;
  Result.quantization:=aJSON.Get('quantization','');
  Result.quantize:=aJSON.Get('quantize','');
  Result.stream:=aJSON.Get('stream',False);
  Result.system:=aJSON.Get('system','');
  Result.template:=aJSON.Get('template','');
end;

class function Tapi_CreateRequestSerializer.Deserialize(aJSON : String) : Tapi_CreateRequest;

var
  lObj : TJSONObject;
begin
  Result := Default(Tapi_CreateRequest);
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

function Tapi_DeleteRequestSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('model',model);
    Result.Add('name',name);
  except
    Result.Free;
    raise;
  end;
end;

function Tapi_DeleteRequestSerializer.Serialize : String;
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

class function Tapi_DeleteRequestSerializer.Deserialize(aJSON : TJSONObject) : Tapi_DeleteRequest;

begin
  Result := Tapi_DeleteRequest.Create;
  If (aJSON=Nil) then
    exit;
  Result.model:=aJSON.Get('model','');
  Result.name:=aJSON.Get('name','');
end;

class function Tapi_DeleteRequestSerializer.Deserialize(aJSON : String) : Tapi_DeleteRequest;

var
  lObj : TJSONObject;
begin
  Result := Default(Tapi_DeleteRequest);
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

function Tapi_EmbeddingRequestSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('keep_alive',keep_alive);
    Result.Add('model',model);
    if (options<>'') then
      Result.Add('options',GetJSON(options));
    Result.Add('prompt',prompt);
  except
    Result.Free;
    raise;
  end;
end;

function Tapi_EmbeddingRequestSerializer.Serialize : String;
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

class function Tapi_EmbeddingRequestSerializer.Deserialize(aJSON : TJSONObject) : Tapi_EmbeddingRequest;

begin
  Result := Tapi_EmbeddingRequest.Create;
  If (aJSON=Nil) then
    exit;
  Result.keep_alive:=aJSON.Get('keep_alive','');
  Result.model:=aJSON.Get('model','');
  Result.options:=aJSON.Get('options',TJSONObject(Nil)).AsJSON;
  Result.prompt:=aJSON.Get('prompt','');
end;

class function Tapi_EmbeddingRequestSerializer.Deserialize(aJSON : String) : Tapi_EmbeddingRequest;

var
  lObj : TJSONObject;
begin
  Result := Default(Tapi_EmbeddingRequest);
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

function Tapi_EmbeddingResponseSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('embedding',Arr);
    For I:=0 to Length(embedding)-1 do
      Arr.Add(embedding[i]);
  except
    Result.Free;
    raise;
  end;
end;

function Tapi_EmbeddingResponseSerializer.Serialize : String;
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

class function Tapi_EmbeddingResponseSerializer.Deserialize(aJSON : TJSONObject) : Tapi_EmbeddingResponse;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := Tapi_EmbeddingResponse.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('embedding',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.embedding,lArr.Count);
    For I:=0 to Length(Result.embedding)-1 do
      Result.embedding[i]:=lArr[i].AsFloat;
    end;
end;

class function Tapi_EmbeddingResponseSerializer.Deserialize(aJSON : String) : Tapi_EmbeddingResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(Tapi_EmbeddingResponse);
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

function Tapi_EmbedRequestSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    if (input<>'') then
      Result.Add('input',GetJSON(input));
    Result.Add('keep_alive',keep_alive);
    Result.Add('model',model);
    if (options<>'') then
      Result.Add('options',GetJSON(options));
    Result.Add('truncate',truncate);
  except
    Result.Free;
    raise;
  end;
end;

function Tapi_EmbedRequestSerializer.Serialize : String;
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

class function Tapi_EmbedRequestSerializer.Deserialize(aJSON : TJSONObject) : Tapi_EmbedRequest;

begin
  Result := Tapi_EmbedRequest.Create;
  If (aJSON=Nil) then
    exit;
  Result.input:=aJSON.Get('input',TJSONObject(Nil)).AsJSON;
  Result.keep_alive:=aJSON.Get('keep_alive','');
  Result.model:=aJSON.Get('model','');
  Result.options:=aJSON.Get('options',TJSONObject(Nil)).AsJSON;
  Result.truncate:=aJSON.Get('truncate',False);
end;

class function Tapi_EmbedRequestSerializer.Deserialize(aJSON : String) : Tapi_EmbedRequest;

var
  lObj : TJSONObject;
begin
  Result := Default(Tapi_EmbedRequest);
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

function Tapi_EmbedResponseSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('embeddings',Arr);
    For I:=0 to Length(embeddings)-1 do
      Arr.Add(embeddings[i].SerializeArray);
    Result.Add('load_duration',load_duration);
    Result.Add('model',model);
    Result.Add('prompt_eval_count',prompt_eval_count);
    Result.Add('total_duration',total_duration);
  except
    Result.Free;
    raise;
  end;
end;

function Tapi_EmbedResponseSerializer.Serialize : String;
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

class function Tapi_EmbedResponseSerializer.Deserialize(aJSON : TJSONObject) : Tapi_EmbedResponse;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := Tapi_EmbedResponse.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('embeddings',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.embeddings,lArr.Count);
    For I:=0 to Length(Result.embeddings)-1 do
      Result.embeddings[i]:=TDoubleDynArray.Deserialize(lArr[i] as TJSONArray);
    end;
  Result.load_duration:=aJSON.Get('load_duration',0);
  Result.model:=aJSON.Get('model','');
  Result.prompt_eval_count:=aJSON.Get('prompt_eval_count',0);
  Result.total_duration:=aJSON.Get('total_duration',0);
end;

class function Tapi_EmbedResponseSerializer.Deserialize(aJSON : String) : Tapi_EmbedResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(Tapi_EmbedResponse);
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

function Tapi_GenerateRequestSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('context',Arr);
    For I:=0 to Length(context)-1 do
      Arr.Add(context[i]);
    Result.Add('format',format);
    Arr:=TJSONArray.Create;
    Result.Add('images',Arr);
    For I:=0 to Length(images)-1 do
      Arr.Add(images[i].SerializeArray);
    Result.Add('keep_alive',keep_alive);
    Result.Add('model',model);
    if (options<>'') then
      Result.Add('options',GetJSON(options));
    Result.Add('prompt',prompt);
    Result.Add('raw',raw);
    Result.Add('stream',stream);
    Result.Add('suffix',suffix);
    Result.Add('system',system);
    Result.Add('template',template);
    Result.Add('think',think);
  except
    Result.Free;
    raise;
  end;
end;

function Tapi_GenerateRequestSerializer.Serialize : String;
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

class function Tapi_GenerateRequestSerializer.Deserialize(aJSON : TJSONObject) : Tapi_GenerateRequest;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := Tapi_GenerateRequest.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('context',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.context,lArr.Count);
    For I:=0 to Length(Result.context)-1 do
      Result.context[i]:=lArr[i].AsInteger;
    end;
  Result.format:=aJSON.Get('format','');
  lArr:=aJSON.Get('images',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.images,lArr.Count);
    For I:=0 to Length(Result.images)-1 do
      Result.images[i]:=TIntegerDynArray.Deserialize(lArr[i] as TJSONArray);
    end;
  Result.keep_alive:=aJSON.Get('keep_alive','');
  Result.model:=aJSON.Get('model','');
  Result.options:=aJSON.Get('options',TJSONObject(Nil)).AsJSON;
  Result.prompt:=aJSON.Get('prompt','');
  Result.raw:=aJSON.Get('raw',False);
  Result.stream:=aJSON.Get('stream',False);
  Result.suffix:=aJSON.Get('suffix','');
  Result.system:=aJSON.Get('system','');
  Result.template:=aJSON.Get('template','');
  Result.think:=aJSON.Get('think',False);
end;

class function Tapi_GenerateRequestSerializer.Deserialize(aJSON : String) : Tapi_GenerateRequest;

var
  lObj : TJSONObject;
begin
  Result := Default(Tapi_GenerateRequest);
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

function Tapi_ModelDetailsSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('families',Arr);
    For I:=0 to Length(families)-1 do
      Arr.Add(families[i]);
    Result.Add('family',family);
    Result.Add('format',format);
    Result.Add('parameter_size',parameter_size);
    Result.Add('parent_model',parent_model);
    Result.Add('quantization_level',quantization_level);
  except
    Result.Free;
    raise;
  end;
end;

function Tapi_ModelDetailsSerializer.Serialize : String;
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

class function Tapi_ModelDetailsSerializer.Deserialize(aJSON : TJSONObject) : Tapi_ModelDetails;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := Tapi_ModelDetails.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('families',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.families,lArr.Count);
    For I:=0 to Length(Result.families)-1 do
      Result.families[i]:=lArr[i].Asstring;
    end;
  Result.family:=aJSON.Get('family','');
  Result.format:=aJSON.Get('format','');
  Result.parameter_size:=aJSON.Get('parameter_size','');
  Result.parent_model:=aJSON.Get('parent_model','');
  Result.quantization_level:=aJSON.Get('quantization_level','');
end;

class function Tapi_ModelDetailsSerializer.Deserialize(aJSON : String) : Tapi_ModelDetails;

var
  lObj : TJSONObject;
begin
  Result := Default(Tapi_ModelDetails);
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

function Tapi_ListModelResponseSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    if Assigned(details) then
      Result.Add('details',details.SerializeObject);
    Result.Add('digest',digest);
    Result.Add('model',model);
    Result.Add('modified_at',modified_at);
    Result.Add('name',name);
    Result.Add('size',size);
  except
    Result.Free;
    raise;
  end;
end;

function Tapi_ListModelResponseSerializer.Serialize : String;
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

class function Tapi_ListModelResponseSerializer.Deserialize(aJSON : TJSONObject) : Tapi_ListModelResponse;

begin
  Result := Tapi_ListModelResponse.Create;
  If (aJSON=Nil) then
    exit;
  Result.details:=Tapi_ModelDetails.Deserialize(aJSON.Get('details',TJSONObject(Nil)));
  Result.digest:=aJSON.Get('digest','');
  Result.model:=aJSON.Get('model','');
  Result.modified_at:=aJSON.Get('modified_at','');
  Result.name:=aJSON.Get('name','');
  Result.size:=aJSON.Get('size',0);
end;

class function Tapi_ListModelResponseSerializer.Deserialize(aJSON : String) : Tapi_ListModelResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(Tapi_ListModelResponse);
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

function Tapi_ListResponseSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('models',Arr);
    For I:=0 to Length(models)-1 do
      Arr.Add(models[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function Tapi_ListResponseSerializer.Serialize : String;
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

class function Tapi_ListResponseSerializer.Deserialize(aJSON : TJSONObject) : Tapi_ListResponse;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := Tapi_ListResponse.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('models',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.models,lArr.Count);
    For I:=0 to Length(Result.models)-1 do
      Result.models[i]:=Tapi_ListModelResponse.Deserialize(lArr[i] as TJSONObject);
    end;
end;

class function Tapi_ListResponseSerializer.Deserialize(aJSON : String) : Tapi_ListResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(Tapi_ListResponse);
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

function Tapi_ProcessModelResponseSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    if Assigned(details) then
      Result.Add('details',details.SerializeObject);
    Result.Add('digest',digest);
    Result.Add('expires_at',expires_at);
    Result.Add('model',model);
    Result.Add('name',name);
    Result.Add('size',size);
    Result.Add('size_vram',size_vram);
  except
    Result.Free;
    raise;
  end;
end;

function Tapi_ProcessModelResponseSerializer.Serialize : String;
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

class function Tapi_ProcessModelResponseSerializer.Deserialize(aJSON : TJSONObject) : Tapi_ProcessModelResponse;

begin
  Result := Tapi_ProcessModelResponse.Create;
  If (aJSON=Nil) then
    exit;
  Result.details:=Tapi_ModelDetails.Deserialize(aJSON.Get('details',TJSONObject(Nil)));
  Result.digest:=aJSON.Get('digest','');
  Result.expires_at:=aJSON.Get('expires_at','');
  Result.model:=aJSON.Get('model','');
  Result.name:=aJSON.Get('name','');
  Result.size:=aJSON.Get('size',0);
  Result.size_vram:=aJSON.Get('size_vram',0);
end;

class function Tapi_ProcessModelResponseSerializer.Deserialize(aJSON : String) : Tapi_ProcessModelResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(Tapi_ProcessModelResponse);
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

function Tapi_ProcessResponseSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('models',Arr);
    For I:=0 to Length(models)-1 do
      Arr.Add(models[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function Tapi_ProcessResponseSerializer.Serialize : String;
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

class function Tapi_ProcessResponseSerializer.Deserialize(aJSON : TJSONObject) : Tapi_ProcessResponse;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := Tapi_ProcessResponse.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('models',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.models,lArr.Count);
    For I:=0 to Length(Result.models)-1 do
      Result.models[i]:=Tapi_ProcessModelResponse.Deserialize(lArr[i] as TJSONObject);
    end;
end;

class function Tapi_ProcessResponseSerializer.Deserialize(aJSON : String) : Tapi_ProcessResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(Tapi_ProcessResponse);
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

function Tapi_ProgressResponseSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('completed',completed);
    Result.Add('digest',digest);
    Result.Add('status',status);
    Result.Add('total',total);
  except
    Result.Free;
    raise;
  end;
end;

function Tapi_ProgressResponseSerializer.Serialize : String;
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

class function Tapi_ProgressResponseSerializer.Deserialize(aJSON : TJSONObject) : Tapi_ProgressResponse;

begin
  Result := Tapi_ProgressResponse.Create;
  If (aJSON=Nil) then
    exit;
  Result.completed:=aJSON.Get('completed',0);
  Result.digest:=aJSON.Get('digest','');
  Result.status:=aJSON.Get('status','');
  Result.total:=aJSON.Get('total',0);
end;

class function Tapi_ProgressResponseSerializer.Deserialize(aJSON : String) : Tapi_ProgressResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(Tapi_ProgressResponse);
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

function Tapi_PullRequestSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('insecure',insecure);
    Result.Add('model',model);
    Result.Add('name',name);
    Result.Add('password',password);
    Result.Add('stream',stream);
    Result.Add('username',username);
  except
    Result.Free;
    raise;
  end;
end;

function Tapi_PullRequestSerializer.Serialize : String;
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

class function Tapi_PullRequestSerializer.Deserialize(aJSON : TJSONObject) : Tapi_PullRequest;

begin
  Result := Tapi_PullRequest.Create;
  If (aJSON=Nil) then
    exit;
  Result.insecure:=aJSON.Get('insecure',False);
  Result.model:=aJSON.Get('model','');
  Result.name:=aJSON.Get('name','');
  Result.password:=aJSON.Get('password','');
  Result.stream:=aJSON.Get('stream',False);
  Result.username:=aJSON.Get('username','');
end;

class function Tapi_PullRequestSerializer.Deserialize(aJSON : String) : Tapi_PullRequest;

var
  lObj : TJSONObject;
begin
  Result := Default(Tapi_PullRequest);
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

function Tapi_PushRequestSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('insecure',insecure);
    Result.Add('model',model);
    Result.Add('name',name);
    Result.Add('password',password);
    Result.Add('stream',stream);
    Result.Add('username',username);
  except
    Result.Free;
    raise;
  end;
end;

function Tapi_PushRequestSerializer.Serialize : String;
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

class function Tapi_PushRequestSerializer.Deserialize(aJSON : TJSONObject) : Tapi_PushRequest;

begin
  Result := Tapi_PushRequest.Create;
  If (aJSON=Nil) then
    exit;
  Result.insecure:=aJSON.Get('insecure',False);
  Result.model:=aJSON.Get('model','');
  Result.name:=aJSON.Get('name','');
  Result.password:=aJSON.Get('password','');
  Result.stream:=aJSON.Get('stream',False);
  Result.username:=aJSON.Get('username','');
end;

class function Tapi_PushRequestSerializer.Deserialize(aJSON : String) : Tapi_PushRequest;

var
  lObj : TJSONObject;
begin
  Result := Default(Tapi_PushRequest);
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

function Tapi_ShowRequestSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('model',model);
    Result.Add('name',name);
    if (options<>'') then
      Result.Add('options',GetJSON(options));
    Result.Add('system',system);
    Result.Add('template',template);
    Result.Add('verbose',verbose);
  except
    Result.Free;
    raise;
  end;
end;

function Tapi_ShowRequestSerializer.Serialize : String;
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

class function Tapi_ShowRequestSerializer.Deserialize(aJSON : TJSONObject) : Tapi_ShowRequest;

begin
  Result := Tapi_ShowRequest.Create;
  If (aJSON=Nil) then
    exit;
  Result.model:=aJSON.Get('model','');
  Result.name:=aJSON.Get('name','');
  Result.options:=aJSON.Get('options',TJSONObject(Nil)).AsJSON;
  Result.system:=aJSON.Get('system','');
  Result.template:=aJSON.Get('template','');
  Result.verbose:=aJSON.Get('verbose',False);
end;

class function Tapi_ShowRequestSerializer.Deserialize(aJSON : String) : Tapi_ShowRequest;

var
  lObj : TJSONObject;
begin
  Result := Default(Tapi_ShowRequest);
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

function Tapi_TensorSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('name',name);
    Arr:=TJSONArray.Create;
    Result.Add('shape',Arr);
    For I:=0 to Length(shape)-1 do
      Arr.Add(shape[i]);
    Result.Add('type',type_);
  except
    Result.Free;
    raise;
  end;
end;

function Tapi_TensorSerializer.Serialize : String;
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

class function Tapi_TensorSerializer.Deserialize(aJSON : TJSONObject) : Tapi_Tensor;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := Tapi_Tensor.Create;
  If (aJSON=Nil) then
    exit;
  Result.name:=aJSON.Get('name','');
  lArr:=aJSON.Get('shape',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.shape,lArr.Count);
    For I:=0 to Length(Result.shape)-1 do
      Result.shape[i]:=lArr[i].AsInteger;
    end;
  Result.type_:=aJSON.Get('type','');
end;

class function Tapi_TensorSerializer.Deserialize(aJSON : String) : Tapi_Tensor;

var
  lObj : TJSONObject;
begin
  Result := Default(Tapi_Tensor);
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

function Tapi_ShowResponseSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('capabilities',Arr);
    For I:=0 to Length(capabilities)-1 do
      Arr.Add(capabilities[i]);
    if Assigned(details) then
      Result.Add('details',details.SerializeObject);
    Result.Add('license',license);
    Arr:=TJSONArray.Create;
    Result.Add('messages',Arr);
    For I:=0 to Length(messages)-1 do
      Arr.Add(messages[i].SerializeObject);
    Result.Add('modelfile',modelfile);
    if (model_info<>'') then
      Result.Add('model_info',GetJSON(model_info));
    Result.Add('modified_at',modified_at);
    Result.Add('parameters',parameters);
    if (projector_info<>'') then
      Result.Add('projector_info',GetJSON(projector_info));
    Result.Add('system',system);
    Result.Add('template',template);
    Arr:=TJSONArray.Create;
    Result.Add('tensors',Arr);
    For I:=0 to Length(tensors)-1 do
      Arr.Add(tensors[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function Tapi_ShowResponseSerializer.Serialize : String;
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

class function Tapi_ShowResponseSerializer.Deserialize(aJSON : TJSONObject) : Tapi_ShowResponse;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := Tapi_ShowResponse.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('capabilities',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.capabilities,lArr.Count);
    For I:=0 to Length(Result.capabilities)-1 do
      Result.capabilities[i]:=lArr[i].Asstring;
    end;
  Result.details:=Tapi_ModelDetails.Deserialize(aJSON.Get('details',TJSONObject(Nil)));
  Result.license:=aJSON.Get('license','');
  lArr:=aJSON.Get('messages',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.messages,lArr.Count);
    For I:=0 to Length(Result.messages)-1 do
      Result.messages[i]:=Tapi_Message.Deserialize(lArr[i] as TJSONObject);
    end;
  Result.modelfile:=aJSON.Get('modelfile','');
  Result.model_info:=aJSON.Get('model_info',TJSONObject(Nil)).AsJSON;
  Result.modified_at:=aJSON.Get('modified_at','');
  Result.parameters:=aJSON.Get('parameters','');
  Result.projector_info:=aJSON.Get('projector_info',TJSONObject(Nil)).AsJSON;
  Result.system:=aJSON.Get('system','');
  Result.template:=aJSON.Get('template','');
  lArr:=aJSON.Get('tensors',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.tensors,lArr.Count);
    For I:=0 to Length(Result.tensors)-1 do
      Result.tensors[i]:=Tapi_Tensor.Deserialize(lArr[i] as TJSONObject);
    end;
end;

class function Tapi_ShowResponseSerializer.Deserialize(aJSON : String) : Tapi_ShowResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(Tapi_ShowResponse);
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


function TIntegerDynArraySerializer.SerializeArray : TJSONArray;
var
  I : Integer;
begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to length(Self)-1 do
      Result.Add(self[i]);
  except
    Result.Free;
    raise;
  end;
end;


function TIntegerDynArraySerializer.Serialize : String;
var
  lObj : TJSONArray;
begin
  lObj:=SerializeArray;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TIntegerDynArraySerializer.Deserialize(aJSON : TJSONArray) : TIntegerDynArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=aJSON[i].AsInteger;
end;

class function TIntegerDynArraySerializer.Deserialize(aJSON : String) : TIntegerDynArray; 

var
  lObj : TJSONData;
  lArr : TJSONArray absolute lobj;
begin
  lObj:=GetJSON(aJSON);
  try
    Result:=DeSerialize(lArr);
  finally
    lObj.Free;
  end;
end;


function TDoubleDynArraySerializer.SerializeArray : TJSONArray;
var
  I : Integer;
begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to length(Self)-1 do
      Result.Add(self[i]);
  except
    Result.Free;
    raise;
  end;
end;


function TDoubleDynArraySerializer.Serialize : String;
var
  lObj : TJSONArray;
begin
  lObj:=SerializeArray;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TDoubleDynArraySerializer.Deserialize(aJSON : TJSONArray) : TDoubleDynArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=aJSON[i].AsFloat;
end;

class function TDoubleDynArraySerializer.Deserialize(aJSON : String) : TDoubleDynArray; 

var
  lObj : TJSONData;
  lArr : TJSONArray absolute lobj;
begin
  lObj:=GetJSON(aJSON);
  try
    Result:=DeSerialize(lArr);
  finally
    lObj.Free;
  end;
end;


end.
