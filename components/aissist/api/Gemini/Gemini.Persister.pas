{
    This file is part of the Free Component Library
    Copyright (c) 2025 by Michael Van Canneyt michael@freepascal.org

    Gemini Rest API - Data transfer object serializers

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Gemini.Persister;

{$mode objfpc}
{$h+}
{$modeswitch typehelpers}

interface

uses
  fpJSON, Classes, SysUtils,
  Gemini.Dto;

Type
  // Request serializers
  TGeminiTextPartSerializer = class helper for TGeminiTextPart
    function SerializeObject : TJSONObject;
    function Serialize : String;
  end;

  TGeminiInlineDataPartSerializer = class helper for TGeminiInlineDataPart
    function SerializeObject : TJSONObject;
    function Serialize : String;
  end;

  TGeminiFileDataPartSerializer = class helper for TGeminiFileDataPart
    function SerializeObject : TJSONObject;
    function Serialize : String;
  end;

  TGeminiPartSerializer = class helper for TGeminiPart
    function SerializeObject : TJSONObject;
    function Serialize : String;
  end;

  TGeminiContentSerializer = class helper for TGeminiContent
    function SerializeObject : TJSONObject;
    function Serialize : String;
  end;

  TGeminiSafetySettingSerializer = class helper for TGeminiSafetySetting
    function SerializeObject : TJSONObject;
    function Serialize : String;
  end;

  TGeminiGenerationConfigSerializer = class helper for TGeminiGenerationConfig
    function SerializeObject : TJSONObject;
    function Serialize : String;
  end;

  TGeminiFunctionDeclarationSerializer = class helper for TGeminiFunctionDeclaration
    function SerializeObject : TJSONObject;
    function Serialize : String;
  end;

  TGeminiToolSerializer = class helper for TGeminiTool
    function SerializeObject : TJSONObject;
    function Serialize : String;
  end;

  TGeminiToolConfigSerializer = class helper for TGeminiToolConfig
    function SerializeObject : TJSONObject;
    function Serialize : String;
  end;

  TGeminiGenerateContentRequestSerializer = class helper for TGeminiGenerateContentRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
  end;

  // Response deserializers
  TGeminiSafetyRatingSerializer = class helper for TGeminiSafetyRating
    class function Deserialize(aJSON : TJSONObject) : TGeminiSafetyRating; overload; static;
    class function Deserialize(aJSON : String) : TGeminiSafetyRating; overload; static;
  end;

  { TGeminiCitationSourceSerializer }

  TGeminiCitationSourceSerializer = class helper for TGeminiCitationSource
    class function Deserialize(aJSON : TJSONObject) : TGeminiCitationSource; overload; static;
    class function Deserialize(aJSON : String) : TGeminiCitationSource; overload; static;
  end;

  TGeminiCitationMetadataSerializer = class helper for TGeminiCitationMetadata
    class function Deserialize(aJSON : TJSONObject) : TGeminiCitationMetadata; overload; static;
    class function Deserialize(aJSON : String) : TGeminiCitationMetadata; overload; static;
  end;

  TGeminiFunctionCallSerializer = class helper for TGeminiFunctionCall
    class function Deserialize(aJSON : TJSONObject) : TGeminiFunctionCall; overload; static;
    class function Deserialize(aJSON : String) : TGeminiFunctionCall; overload; static;
  end;

  TGeminiFunctionResponseSerializer = class helper for TGeminiFunctionResponse
    class function Deserialize(aJSON : TJSONObject) : TGeminiFunctionResponse; overload; static;
    class function Deserialize(aJSON : String) : TGeminiFunctionResponse; overload; static;
  end;

  TGeminiResponsePartSerializer = class helper for TGeminiResponsePart
    class function Deserialize(aJSON : TJSONObject) : TGeminiResponsePart; overload; static;
    class function Deserialize(aJSON : String) : TGeminiResponsePart; overload; static;
  end;

  TGeminiResponseContentSerializer = class helper for TGeminiResponseContent
    class function Deserialize(aJSON : TJSONObject) : TGeminiResponseContent; overload; static;
    class function Deserialize(aJSON : String) : TGeminiResponseContent; overload; static;
  end;

  TGeminiUsageMetadataSerializer = class helper for TGeminiUsageMetadata
    class function Deserialize(aJSON : TJSONObject) : TGeminiUsageMetadata; overload; static;
    class function Deserialize(aJSON : String) : TGeminiUsageMetadata; overload; static;
  end;

  TGeminiCandidateSerializer = class helper for TGeminiCandidate
    class function Deserialize(aJSON : TJSONObject) : TGeminiCandidate; overload; static;
    class function Deserialize(aJSON : String) : TGeminiCandidate; overload; static;
  end;

  TGeminiPromptFeedbackSerializer = class helper for TGeminiPromptFeedback
    class function Deserialize(aJSON : TJSONObject) : TGeminiPromptFeedback; overload; static;
    class function Deserialize(aJSON : String) : TGeminiPromptFeedback; overload; static;
  end;

  TGeminiGenerateContentResponseSerializer = class helper for TGeminiGenerateContentResponse
    class function  Deserialize(aJSON : TJSONObject) : TGeminiGenerateContentResponse; overload; static;
    class function Deserialize(aJSON : String) : TGeminiGenerateContentResponse; overload; static;
  end;

  TGeminiModelSerializer = class helper for TGeminiModel
    class function Deserialize(aJSON : TJSONObject) : TGeminiModel; overload; static;
    class function Deserialize(aJSON : String) : TGeminiModel; overload; static;
  end;

  TGeminiListModelsResponseSerializer = class helper for TGeminiListModelsResponse
    class function Deserialize(aJSON : TJSONObject) : TGeminiListModelsResponse; overload; static;
    class function Deserialize(aJSON : String) : TGeminiListModelsResponse; overload; static;
  end;

implementation

// Helper functions
function SafeStringValue(obj: TJSONObject; const key: string): string;
begin
  Result:=Obj.Get(key,'');
end;

function SafeIntegerValue(obj: TJSONObject; const key: string): integer;
begin
  Result:=Obj.Get(key,0);
end;

function SafeFloatValue(obj: TJSONObject; const key: string): double;
begin
  Result:=Obj.Get(key,0.0);
end;

function SafeBooleanValue(obj: TJSONObject; const key: string): boolean;
begin
  Result:=Obj.Get(key,False);
end;

// Request serializers implementation
function TGeminiTextPartSerializer.SerializeObject: TJSONObject;
begin
  Result := TJSONObject.Create;
  if text <> '' then
    Result.Add('text', text);
end;

function TGeminiTextPartSerializer.Serialize: String;
var
  obj: TJSONObject;
begin
  obj := SerializeObject;
  try
    Result := obj.AsJSON;
  finally
    obj.Free;
  end;
end;

function TGeminiInlineDataPartSerializer.SerializeObject: TJSONObject;
begin
  Result := TJSONObject.Create;
  if mime_type <> '' then
    Result.Add('mimeType', mime_type);
  if data <> '' then
    Result.Add('data', data);
end;

function TGeminiInlineDataPartSerializer.Serialize: String;
var
  obj: TJSONObject;
begin
  obj := SerializeObject;
  try
    Result := obj.AsJSON;
  finally
    obj.Free;
  end;
end;

function TGeminiFileDataPartSerializer.SerializeObject: TJSONObject;
begin
  Result := TJSONObject.Create;
  if mime_type <> '' then
    Result.Add('mimeType', mime_type);
  if file_uri <> '' then
    Result.Add('fileUri', file_uri);
end;

function TGeminiFileDataPartSerializer.Serialize: String;
var
  obj: TJSONObject;
begin
  obj := SerializeObject;
  try
    Result := obj.AsJSON;
  finally
    obj.Free;
  end;
end;

function TGeminiPartSerializer.SerializeObject: TJSONObject;
var
  inlineDataObj: TJSONObject;
  fileDataObj: TJSONObject;
begin
  Result := TJSONObject.Create;

  if text <> '' then
    Result.Add('text', text)
  else if Assigned(inline_data) then
  begin
    inlineDataObj := inline_data.SerializeObject;
    Result.Add('inlineData', inlineDataObj);
  end
  else if Assigned(file_data) then
  begin
    fileDataObj := file_data.SerializeObject;
    Result.Add('fileData', fileDataObj);
  end;
end;

function TGeminiPartSerializer.Serialize: String;
var
  obj: TJSONObject;
begin
  obj := SerializeObject;
  try
    Result := obj.AsJSON;
  finally
    obj.Free;
  end;
end;

function TGeminiContentSerializer.SerializeObject: TJSONObject;
var
  partsArray: TJSONArray;
  i: integer;
begin
  Result := TJSONObject.Create;

  if Length(parts) > 0 then
  begin
    partsArray := TJSONArray.Create;
    for i := 0 to High(parts) do
      partsArray.Add(parts[i].SerializeObject);
    Result.Add('parts', partsArray);
  end;

  if role <> '' then
    Result.Add('role', role);
end;

function TGeminiContentSerializer.Serialize: String;
var
  obj: TJSONObject;
begin
  obj := SerializeObject;
  try
    Result := obj.AsJSON;
  finally
    obj.Free;
  end;
end;

function TGeminiSafetySettingSerializer.SerializeObject: TJSONObject;
begin
  Result := TJSONObject.Create;
  if category <> '' then
    Result.Add('category', category);
  if threshold <> '' then
    Result.Add('threshold', threshold);
end;

function TGeminiSafetySettingSerializer.Serialize: String;
var
  obj: TJSONObject;
begin
  obj := SerializeObject;
  try
    Result := obj.AsJSON;
  finally
    obj.Free;
  end;
end;

function TGeminiGenerationConfigSerializer.SerializeObject: TJSONObject;
var
  stopSeqArray: TJSONArray;
  i: integer;
begin
  Result := TJSONObject.Create;

  if Length(stop_sequences) > 0 then
  begin
    stopSeqArray := TJSONArray.Create;
    for i := 0 to High(stop_sequences) do
      stopSeqArray.Add(stop_sequences[i]);
    Result.Add('stopSequences', stopSeqArray);
  end;

  if response_mime_type <> '' then
    Result.Add('responseMimeType', response_mime_type);
  if response_schema <> '' then
    Result.Add('responseSchema', TJSONObject.Create); // Parse JSON schema string
  if candidate_count > 0 then
    Result.Add('candidateCount', candidate_count);
  if max_output_tokens > 0 then
    Result.Add('maxOutputTokens', max_output_tokens);
  if temperature > 0 then
    Result.Add('temperature', temperature);
  if top_p > 0 then
    Result.Add('topP', top_p);
  if top_k > 0 then
    Result.Add('topK', top_k);
end;

function TGeminiGenerationConfigSerializer.Serialize: String;
var
  obj: TJSONObject;
begin
  obj := SerializeObject;
  try
    Result := obj.AsJSON;
  finally
    obj.Free;
  end;
end;

function TGeminiFunctionDeclarationSerializer.SerializeObject: TJSONObject;
begin
  Result := TJSONObject.Create;
  if name <> '' then
    Result.Add('name', name);
  if description <> '' then
    Result.Add('description', description);
  if parameters <> '' then
    Result.Add('parameters', TJSONObject.Create); // Parse JSON schema string
end;

function TGeminiFunctionDeclarationSerializer.Serialize: String;
var
  obj: TJSONObject;
begin
  obj := SerializeObject;
  try
    Result := obj.AsJSON;
  finally
    obj.Free;
  end;
end;

function TGeminiToolSerializer.SerializeObject: TJSONObject;
var
  funcArray: TJSONArray;
  i: integer;
begin
  Result := TJSONObject.Create;

  if Length(function_declarations) > 0 then
  begin
    funcArray := TJSONArray.Create;
    for i := 0 to High(function_declarations) do
      funcArray.Add(function_declarations[i].SerializeObject);
    Result.Add('functionDeclarations', funcArray);
  end;
end;

function TGeminiToolSerializer.Serialize: String;
var
  obj: TJSONObject;
begin
  obj := SerializeObject;
  try
    Result := obj.AsJSON;
  finally
    obj.Free;
  end;
end;

function TGeminiToolConfigSerializer.SerializeObject: TJSONObject;
begin
  Result := TJSONObject.Create;
  if function_calling_config <> '' then
    Result.Add('functionCallingConfig', TJSONObject.Create); // Parse JSON config string
end;

function TGeminiToolConfigSerializer.Serialize: String;
var
  obj: TJSONObject;
begin
  obj := SerializeObject;
  try
    Result := obj.AsJSON;
  finally
    obj.Free;
  end;
end;

function TGeminiGenerateContentRequestSerializer.SerializeObject: TJSONObject;
var
  contentsArray, toolsArray, safetyArray: TJSONArray;
  i: integer;
begin
  Result := TJSONObject.Create;

  if Length(contents) > 0 then
  begin
    contentsArray := TJSONArray.Create;
    for i := 0 to High(contents) do
      contentsArray.Add(contents[i].SerializeObject);
    Result.Add('contents', contentsArray);
  end;

  if Length(tools) > 0 then
  begin
    toolsArray := TJSONArray.Create;
    for i := 0 to High(tools) do
      toolsArray.Add(tools[i].SerializeObject);
    Result.Add('tools', toolsArray);
  end;

  if Assigned(tool_config) then
    Result.Add('toolConfig', tool_config.SerializeObject);

  if Length(safety_settings) > 0 then
  begin
    safetyArray := TJSONArray.Create;
    for i := 0 to High(safety_settings) do
      safetyArray.Add(safety_settings[i].SerializeObject);
    Result.Add('safetySettings', safetyArray);
  end;

  if Assigned(system_instruction) then
    Result.Add('systemInstruction', system_instruction.SerializeObject);

  if Assigned(generation_config) then
    Result.Add('generationConfig', generation_config.SerializeObject);

  if cached_content <> '' then
    Result.Add('cachedContent', cached_content);
end;

function TGeminiGenerateContentRequestSerializer.Serialize: String;
var
  obj: TJSONObject;
begin
  obj := SerializeObject;
  try
    Result := obj.AsJSON;
  finally
    obj.Free;
  end;
end;

// Response deserializers implementation (basic implementations)
class function TGeminiGenerateContentResponseSerializer.Deserialize(aJSON: TJSONObject): TGeminiGenerateContentResponse;

var
  lUsageMetaData : TJSONObject;
  lCandidates : TJSONArray;
  i : integer;
begin
  Result := TGeminiGenerateContentResponse.Create;
  lCandidates:=aJSON.Get('candidates',TJSONArray(Nil));
  if assigned(lCandidates) then
    begin
    SetLength(Result.Candidates,lCandidates.Count);
    For I:=0 to lCandidates.Count-1 do
      Result.Candidates[i]:=TGeminiCandidate.Deserialize(lCandidates.Objects[i]);
    end;
  lUsageMetaData:=aJSON.get('usageMetadata',TJSONObject(Nil));
  if assigned(lUsageMetaData) then
    Result.usage_metadata:=TGeminiUsageMetadata.Deserialize(lUsageMetaData);
end;

class function TGeminiGenerateContentResponseSerializer.Deserialize(aJSON: String): TGeminiGenerateContentResponse;
var
  json: TJSONData;
begin
  json := GetJSON(aJSON);
  try
    Result := Deserialize(json as TJSONObject);
  finally
    json.Free;
  end;
end;

// Additional deserializer implementations would follow the same pattern...
// For brevity, showing the basic structure. Full implementation would handle all types.

class function TGeminiCandidateSerializer.Deserialize(aJSON: TJSONObject): TGeminiCandidate;
var
  lObj : TJSONObject;
begin
  Result := TGeminiCandidate.Create;
  Result.finish_reason := SafeStringValue(aJSON, 'finishReason');
  Result.index := SafeIntegerValue(aJSON, 'index');
  Result.token_count := SafeIntegerValue(aJSON, 'tokenCount');
  lObj:= aJSON.Get('content',TJSONObject(Nil));
  if assigned(lObj) then
    Result.content:=TGeminiResponseContent.Deserialize(lObj);

end;

class function TGeminiCandidateSerializer.Deserialize(aJSON: String): TGeminiCandidate;
var
  json: TJSONData;
begin
  json := GetJSON(aJSON);
  try
    Result := Deserialize(json as TJSONObject);
  finally
    json.Free;
  end;
end;

class function TGeminiModelSerializer.Deserialize(aJSON: TJSONObject): TGeminiModel;
var
  lArr : TJSONArray;
  I : integer;
begin
  Result := TGeminiModel.Create;
  Result.name := SafeStringValue(aJSON, 'name');
  Result.base_model_id := SafeStringValue(aJSON, 'baseModelId');
  Result.version := SafeStringValue(aJSON, 'version');
  Result.display_name := SafeStringValue(aJSON, 'displayName');
  Result.description := SafeStringValue(aJSON, 'description');
  lArr:=aJSON.get('supportedGenerationMethods',TJSONArray(Nil));
  SetLength(Result.supported_generation_methods,Larr.Count);
  for I:=0 to lArr.Count-1 do
    Result.supported_generation_methods[i]:=lArr.Strings[i];
  Result.input_token_limit := SafeIntegerValue(aJSON, 'inputTokenLimit');
  Result.output_token_limit := SafeIntegerValue(aJSON, 'outputTokenLimit');
  Result.temperature := SafeFloatValue(aJSON, 'temperature');
  Result.max_temperature := SafeFloatValue(aJSON, 'maxTemperature');
  Result.top_p := SafeFloatValue(aJSON, 'topP');
  Result.top_k := SafeIntegerValue(aJSON, 'topK');
end;

class function TGeminiModelSerializer.Deserialize(aJSON: String): TGeminiModel;
var
  json: TJSONData;
begin
  json := GetJSON(aJSON);
  try
    Result := Deserialize(json as TJSONObject);
  finally
    json.Free;
  end;
end;

class function TGeminiListModelsResponseSerializer.Deserialize(aJSON: TJSONObject): TGeminiListModelsResponse;
var
  I : integer;
  lArr : TJSONArray;
begin
  Result := TGeminiListModelsResponse.Create;
  Result.next_page_token := SafeStringValue(aJSON, 'nextPageToken');
  lArr:=aJSON.Get('models',TJSONArray(Nil));
  if assigned(lArr) then
    begin
    SetLength(Result.models,lArr.Count);
    For I:=0 to lArr.Count-1 do
      Result.Models[i]:=TGeminiModel.Deserialize(lArr.Objects[i]);
    end;
end;

class function TGeminiListModelsResponseSerializer.Deserialize(aJSON: String): TGeminiListModelsResponse;
var
  json: TJSONData;
begin
  json := GetJSON(aJSON);
  try
    Result := Deserialize(json as TJSONObject);
  finally
    json.Free;
  end;
end;

// Placeholder implementations for other deserializers
class function TGeminiSafetyRatingSerializer.Deserialize(aJSON: TJSONObject): TGeminiSafetyRating;
begin
  Result := TGeminiSafetyRating.Create;
  Result.category := SafeStringValue(aJSON, 'category');
  Result.probability := SafeStringValue(aJSON, 'probability');
  Result.blocked := SafeBooleanValue(aJSON, 'blocked');
end;

class function TGeminiSafetyRatingSerializer.Deserialize(aJSON: String): TGeminiSafetyRating;
var
  json: TJSONData;
begin
  json := GetJSON(aJSON);
  try
    Result := Deserialize(json as TJSONObject);
  finally
    json.Free;
  end;
end;

{ TGeminiCitationSourceSerializer }

class function TGeminiCitationSourceSerializer.Deserialize(aJSON: TJSONObject): TGeminiCitationSource;
var
  json: TJSONData;
begin
  Result:=TGeminiCitationSource.Create;
  Result.startIndex:=aJSON.Get('startIndex',0);
  Result.endIndex:=aJSON.Get('endIndex',0);
  Result.uri:=aJSON.Get('uri','');
  Result.license:=aJSON.Get('license','');
end;

class function TGeminiCitationSourceSerializer.Deserialize(aJSON: String): TGeminiCitationSource;
var
  json: TJSONData;
begin
  json := GetJSON(aJSON);
  try
    Result := Deserialize(json as TJSONObject);
  finally
    json.Free;
  end;
end;

class function TGeminiCitationMetadataSerializer.Deserialize(aJSON: TJSONObject): TGeminiCitationMetadata;
var
  lArr : TJSONArray;
  I : integer;
begin
  Result := TGeminiCitationMetadata.Create;
  lArr:=aJSON.get('citationSources',TJSONArray(Nil));
  SetLength(Result.citation_sources,lArr.Count);
  for I:=0 to lArr.Count-1 do
    Result.citation_sources[i]:=TGeminiCitationSource.Deserialize(lArr.Objects[i]);
end;

class function TGeminiCitationMetadataSerializer.Deserialize(aJSON: String): TGeminiCitationMetadata;
var
  json: TJSONData;
begin
  json := GetJSON(aJSON);
  try
    Result := Deserialize(json as TJSONObject);
  finally
    json.Free;
  end;
end;

class function TGeminiFunctionCallSerializer.Deserialize(aJSON: TJSONObject): TGeminiFunctionCall;
begin
  Result := TGeminiFunctionCall.Create;
  Result.name := SafeStringValue(aJSON, 'name');
  Result.args := SafeStringValue(aJSON, 'args');
end;

class function TGeminiFunctionCallSerializer.Deserialize(aJSON: String): TGeminiFunctionCall;
var
  json: TJSONData;
begin
  json := GetJSON(aJSON);
  try
    Result := Deserialize(json as TJSONObject);
  finally
    json.Free;
  end;
end;

class function TGeminiFunctionResponseSerializer.Deserialize(aJSON: TJSONObject): TGeminiFunctionResponse;
begin
  Result := TGeminiFunctionResponse.Create;
  Result.name := SafeStringValue(aJSON, 'name');
  Result.response := SafeStringValue(aJSON, 'response');
end;

class function TGeminiFunctionResponseSerializer.Deserialize(aJSON: String): TGeminiFunctionResponse;
var
  json: TJSONData;
begin
  json := GetJSON(aJSON);
  try
    Result := Deserialize(json as TJSONObject);
  finally
    json.Free;
  end;
end;

class function TGeminiResponsePartSerializer.Deserialize(aJSON: TJSONObject): TGeminiResponsePart;
begin
  Result := TGeminiResponsePart.Create;
  Result.text := SafeStringValue(aJSON, 'text');
  // Need more data types
end;

class function TGeminiResponsePartSerializer.Deserialize(aJSON: String): TGeminiResponsePart;
var
  json: TJSONData;
begin
  json := GetJSON(aJSON);
  try
    Result := Deserialize(json as TJSONObject);
  finally
    json.Free;
  end;
end;

class function TGeminiResponseContentSerializer.Deserialize(aJSON: TJSONObject): TGeminiResponseContent;
var
  lArray : TJSONArray;
  I : Integer;
begin
  Result := TGeminiResponseContent.Create;
  Result.role := SafeStringValue(aJSON, 'role');
  lArray:=aJSON.Get('parts',TJSONArray(nil));
  if Assigned(lArray) then
    begin
    SetLength(Result.parts,lArray.Count);
    for I:=0 to lArray.Count-1 do
      Result.parts[i]:=TGeminiResponsePart.Deserialize(lArray.Objects[i]);
    end;
end;

class function TGeminiResponseContentSerializer.Deserialize(aJSON: String): TGeminiResponseContent;
var
  json: TJSONData;
begin
  json := GetJSON(aJSON);
  try
    Result := Deserialize(json as TJSONObject);
  finally
    json.Free;
  end;
end;

class function TGeminiUsageMetadataSerializer.Deserialize(aJSON: TJSONObject): TGeminiUsageMetadata;
begin
  Result := TGeminiUsageMetadata.Create;
  Result.prompt_token_count := SafeIntegerValue(aJSON, 'promptTokenCount');
  Result.candidates_token_count := SafeIntegerValue(aJSON, 'candidatesTokenCount');
  Result.total_token_count := SafeIntegerValue(aJSON, 'totalTokenCount');
  Result.cached_content_token_count := SafeIntegerValue(aJSON, 'cachedContentTokenCount');
end;

class function TGeminiUsageMetadataSerializer.Deserialize(aJSON: String): TGeminiUsageMetadata;
var
  json: TJSONData;
begin
  json := GetJSON(aJSON);
  try
    Result := Deserialize(json as TJSONObject);
  finally
    json.Free;
  end;
end;

class function TGeminiPromptFeedbackSerializer.Deserialize(aJSON: TJSONObject): TGeminiPromptFeedback;
begin
  Result := TGeminiPromptFeedback.Create;
  Result.block_reason := SafeStringValue(aJSON, 'blockReason');
  // Safety ratings array parsing would be implemented here
end;

class function TGeminiPromptFeedbackSerializer.Deserialize(aJSON: String): TGeminiPromptFeedback;
var
  json: TJSONData;
begin
  json := GetJSON(aJSON);
  try
    Result := Deserialize(json as TJSONObject);
  finally
    json.Free;
  end;
end;

end.
