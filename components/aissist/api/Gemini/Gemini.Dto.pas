{
    This file is part of the Free Component Library
    Copyright (c) 2025 by Michael Van Canneyt michael@freepascal.org

    Google Gemini Rest API -  Data transfer objects

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Gemini.Dto;

{$mode objfpc}
{$h+}

interface

uses types;

Type

  // Content part types
  TGeminiTextPart = Class(TObject)
    text : string;
  end;

  TGeminiInlineDataPart = Class(TObject)
    mime_type : string;
    data : string;  // Base64 encoded data
  end;

  TGeminiFileDataPart = Class(TObject)
    mime_type : string;
    file_uri : string;
  end;

  // Content part (union type - only one field should be set)

  { TGeminiPart }

  TGeminiPart = Class(TObject)
    text : string;                    // For text content
    inline_data : TGeminiInlineDataPart; // For inline binary data
    file_data : TGeminiFileDataPart;     // For file references
    destructor destroy; override;
  end;

  // Content with role and parts

  { TGeminiContent }

  TGeminiContent = Class(TObject)
    parts : array of TGeminiPart;
    role : string;  // "user", "model", or "function"
    destructor destroy; override;
  end;

  // Safety settings
  TGeminiSafetySetting = Class(TObject)
    category : string;  // HARM_CATEGORY_*
    threshold : string; // BLOCK_* threshold
  end;

  // Generation configuration
  TGeminiGenerationConfig = Class(TObject)
    stop_sequences : TStringDynArray;
    response_mime_type : string;
    response_schema : string;  // JSON schema as string
    candidate_count : integer;
    max_output_tokens : integer;
    temperature : double;
    top_p : double;
    top_k : integer;
  end;

  // Function declaration for function calling
  TGeminiFunctionDeclaration = Class(TObject)
    name : string;
    description : string;
    parameters : string;  // JSON schema as string
  end;

  // Tool definition

  { TGeminiTool }

  TGeminiTool = Class(TObject)
    function_declarations : array of TGeminiFunctionDeclaration;
    destructor destroy; override;
  end;

  // Tool configuration
  TGeminiToolConfig = Class(TObject)
    function_calling_config : string;  // JSON config as string
  end;

  // Generate content request

  { TGeminiGenerateContentRequest }

  TGeminiGenerateContentRequest = Class(TObject)
    contents : array of TGeminiContent;
    tools : array of TGeminiTool;
    tool_config : TGeminiToolConfig;
    safety_settings : array of TGeminiSafetySetting;
    system_instruction : TGeminiContent;
    generation_config : TGeminiGenerationConfig;
    cached_content : string;  // Name of cached content to use
    destructor destroy; override;
  end;

  // Safety rating
  TGeminiSafetyRating = Class(TObject)
    category : string;
    probability : string;  // NEGLIGIBLE, LOW, MEDIUM, HIGH
    blocked : boolean;
  end;

  TGeminiCitationSource = Class(TObject)
    startIndex : Integer;
    endIndex : Integer;
    uri : String;
    license : string;
  end;

  // Citation metadata

  { TGeminiCitationMetadata }

  TGeminiCitationMetadata = Class(TObject)
    citation_sources : array of TGeminiCitationSource;  // Simplified - actual structure is more complex
    destructor destroy; override;
  end;

  // Function call
  TGeminiFunctionCall = Class(TObject)
    name : string;
    args : string;  // JSON object as string
  end;

  // Function response
  TGeminiFunctionResponse = Class(TObject)
    name : string;
    response : string;  // JSON object as string
  end;

  // Extended content part for responses

  { TGeminiResponsePart }

  TGeminiResponsePart = Class(TObject)
    text : string;
    inline_data : TGeminiInlineDataPart;
    file_data : TGeminiFileDataPart;
    function_call : TGeminiFunctionCall;
    function_response : TGeminiFunctionResponse;
    destructor Destroy; override;
  end;

  // Response content

  { TGeminiResponseContent }

  TGeminiResponseContent = Class(TObject)
    parts : array of TGeminiResponsePart;
    role : string;
    destructor Destroy; override;
  end;

  // Usage metadata
  TGeminiUsageMetadata = Class(TObject)
    prompt_token_count : integer;
    candidates_token_count : integer;
    total_token_count : integer;
    cached_content_token_count : integer;
  end;

  // Candidate response

  { TGeminiCandidate }

  TGeminiCandidate = Class(TObject)
    content : TGeminiResponseContent;
    finish_reason : string;  // FINISH_REASON_*
    safety_ratings : array of TGeminiSafetyRating;
    citation_metadata : TGeminiCitationMetadata;
    token_count : integer;
    grounding_attributions : array of string;  // Simplified
    index : integer;
    destructor Destroy; override;
  end;

  // Prompt feedback

  { TGeminiPromptFeedback }

  TGeminiPromptFeedback = Class(TObject)
    block_reason : string;  // BLOCK_REASON_*
    safety_ratings : array of TGeminiSafetyRating;
    destructor destroy; override;
  end;

  // Generate content response

  { TGeminiGenerateContentResponse }

  TGeminiGenerateContentResponse = Class(TObject)
    candidates : array of TGeminiCandidate;
    prompt_feedback : TGeminiPromptFeedback;
    usage_metadata : TGeminiUsageMetadata;
    destructor destroy; override;
  end;

  // Models list response types
  TGeminiModel = Class(TObject)
    name : string;
    base_model_id : string;
    version : string;
    display_name : string;
    description : string;
    input_token_limit : integer;
    output_token_limit : integer;
    supported_generation_methods : TStringDynArray;
    temperature : double;
    max_temperature : double;
    top_p : double;
    top_k : integer;
  end;

  { TGeminiListModelsResponse }

  TGeminiListModelsResponse = Class(TObject)
    models : array of TGeminiModel;
    next_page_token : string;
    destructor destroy; override;
  end;

implementation

uses sysutils;

{ TGeminiPart }

destructor TGeminiPart.destroy;
begin
  FreeAndNil(inline_data);
  FreeAndNil(file_data);
  inherited destroy;
end;

{ TGeminiContent }

destructor TGeminiContent.destroy;
var
  i : integer;
begin
  For I:=0 to Length(parts)-1 do
    FreeAndNil(parts[i]);
  inherited destroy;
end;

{ TGeminiTool }

destructor TGeminiTool.destroy;
var
  i : integer;
begin
  For I:=0 to Length(function_declarations)-1 do
    FreeAndNil(function_declarations[i]);
  inherited destroy;
end;

{ TGeminiGenerateContentRequest }

destructor TGeminiGenerateContentRequest.destroy;
var
  i : integer;
begin
  For I:=0 to Length(contents)-1 do
    FreeAndNil(contents[i]);
  For I:=0 to Length(tools)-1 do
    FreeAndNil(tools[i]);
  FreeAndNil(tool_config);
  For I:=0 to Length(safety_settings)-1 do
    FreeAndNil(safety_settings[i]);
  FreeAndNil(system_instruction);
  FreeAndNil(generation_config);
  inherited destroy;
end;

{ TGeminiCitationMetadata }

destructor TGeminiCitationMetadata.destroy;
var
  i : integer;
begin
  For I:=0 to Length(citation_sources)-1 do
    FreeAndNil(citation_sources[i]);
  inherited destroy;
end;

{ TGeminiResponsePart }

destructor TGeminiResponsePart.Destroy;
begin
  FreeAndNil(inline_data);
  FreeAndNil(file_data);
  FreeAndNil(function_call);
  FreeAndNil(function_response);
  inherited Destroy;
end;

{ TGeminiResponseContent }

destructor TGeminiResponseContent.Destroy;
var
  i : integer;
begin
  For I:=0 to Length(parts)-1 do
    FreeAndNil(Parts[i]);
  inherited Destroy;
end;

{ TGeminiCandidate }

destructor TGeminiCandidate.Destroy;
var
  i : integer;
begin
  FreeAndNil(content);
  For I:=0 to Length(safety_ratings)-1 do
    FreeAndNil(safety_ratings[i]);
  FreeAndNil(citation_metadata);
  inherited Destroy;
end;

{ TGeminiPromptFeedback }

destructor TGeminiPromptFeedback.destroy;
var
  i : integer;
begin
  For I:=0 to Length(safety_ratings)-1 do
    FreeAndNil(safety_ratings[i]);
  inherited destroy;
end;

{ TGeminiGenerateContentResponse }

destructor TGeminiGenerateContentResponse.destroy;
var
  i : integer;
begin
  For I:=0 to Length(candidates)-1 do
    FreeAndNil(candidates[i]);
  FreeAndNil(prompt_feedback);
  FreeAndNil(usage_metadata);
  inherited destroy;
end;

{ TGeminiListModelsResponse }

destructor TGeminiListModelsResponse.destroy;
var
  i : integer;
begin
  For I:=0 to Length(Models)-1 do
    FreeAndNil(Models[i]);
  inherited destroy;
end;

end.
