{
    This file is part of the Free Component Library
    Copyright (c) 2025 by Michael Van Canneyt michael@freepascal.org

    Claude Rest API -  Data transfer objects

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit claude.Dto;

{$mode objfpc}
{$h+}


interface

uses types;

Type

  TCompleteRequest = Class(TObject)
    max_tokens_to_sample : integer;
    metadata : string;
    model : string;
    prompt : string;
    stop_sequences : TStringDynArray;
    stream : boolean;
    temperature : double;
    top_k : integer;
    top_p : double;
  end;
  
  TCompleteResponse = Class(TObject)
    completion : string;
    stop_reason : string;
  end;

  TModel = Class(TObject)
    id : string;
    type_ : string;
    display_name : string;
    created_at : string;
  end;

  TModelsResponse = Class(TObject)
    data : array of TModel;
    has_more : boolean;
    first_id : string;
    last_id : string;
  end;

  TFileUploadRequest = Class(TObject)
    file_path : string;
    purpose : string;
  end;

  TFile = Class(TObject)
    id : string;
    type_ : string;
    filename : string;
    purpose : string;
    size_bytes : integer;
    created_at : string;
  end;

  { TFilesResponse }

  TFilesResponse = Class(TObject)
    data : array of TFile;
    has_more : boolean;
    first_id : string;
    last_id : string;
    Destructor destroy; override;
  end;

  TFileDeleteResponse = Class(TObject)
    id : string;
    deleted : boolean;
    type_ : string;
  end;

  TBatchRequest = Class(TObject)
    custom_id : string;
    method : string;
    url : string;
    body : string;
  end;

  { TMessageBatchCreateRequest }

  TMessageBatchCreateRequest = Class(TObject)
    requests : array of TBatchRequest;
    destructor destroy; override;
  end;

  TBatchRequestCounts = Class(TObject)
    processing : integer;
    succeeded : integer;
    errored : integer;
    canceled : integer;
    expired : integer;
  end;

  TBatchResult = Class(TObject)
    custom_id : string;
    result : string;
  end;

  { TMessageBatch }

  TMessageBatch = Class(TObject)
    id : string;
    type_ : string;
    processing_status : string;
    request_counts : TBatchRequestCounts;
    ended_at : string;
    created_at : string;
    expires_at : string;
    archived_at : string;
    cancel_initiated_at : string;
    results_url : string;
    destructor destroy; override;
  end;

  { TMessageBatchesResponse }

  TMessageBatchesResponse = Class(TObject)
    data : array of TMessageBatch;
    has_more : boolean;
    first_id : string;
    last_id : string;
    destructor destroy; override;
  end;

  TOrganization = Class(TObject)
    id : string;
    type_ : string;
    display_name : string;
    created_at : string;
  end;

  TOrganizationMemberUpdateRequest = Class(TObject)
    role : string;
  end;

  TOrganizationMember = Class(TObject)
    id : string;
    type_ : string;
    role : string;
    email : string;
    display_name : string;
    created_at : string;
    updated_at : string;
  end;

  { TOrganizationMembersResponse }

  TOrganizationMembersResponse = Class(TObject)
    data : array of TOrganizationMember;
    has_more : boolean;
    first_id : string;
    last_id : string;
    destructor destroy; override;
  end;

  TOrganizationMemberDeleteResponse = Class(TObject)
    id : string;
    deleted : boolean;
    type_ : string;
  end;

  TOrganizationInviteCreateRequest = Class(TObject)
    email : string;
    role : string;
  end;

  TOrganizationInvite = Class(TObject)
    id : string;
    type_ : string;
    email : string;
    role : string;
    status : string;
    created_at : string;
    expires_at : string;
  end;

  { TOrganizationInvitesResponse }

  TOrganizationInvitesResponse = Class(TObject)
    data : array of TOrganizationInvite;
    has_more : boolean;
    first_id : string;
    last_id : string;
    destructor destroy; override;
  end;

  TOrganizationInviteDeleteResponse = Class(TObject)
    id : string;
    deleted : boolean;
    type_ : string;
  end;

  TWorkspaceCreateRequest = Class(TObject)
    name : string;
    description : string;
  end;

  TWorkspaceUpdateRequest = Class(TObject)
    name : string;
    description : string;
  end;

  TWorkspace = Class(TObject)
    id : string;
    type_ : string;
    name : string;
    description : string;
    status : string;
    created_at : string;
    updated_at : string;
    archived_at : string;
  end;

  TWorkspacesResponse = Class(TObject)
    data : array of TWorkspace;
    has_more : boolean;
    first_id : string;
    last_id : string;
  end;

  TWorkspaceMemberAddRequest = Class(TObject)
    user_id : string;
    workspace_role : string;
  end;

  TWorkspaceMemberUpdateRequest = Class(TObject)
    workspace_role : string;
  end;

  TWorkspaceMember = Class(TObject)
    id : string;
    type_ : string;
    user_id : string;
    workspace_role : string;
    created_at : string;
    updated_at : string;
  end;

  { TWorkspaceMembersResponse }

  TWorkspaceMembersResponse = Class(TObject)
    data : array of TWorkspaceMember;
    has_more : boolean;
    first_id : string;
    last_id : string;
    destructor destroy; override;
  end;

  TWorkspaceMemberDeleteResponse = Class(TObject)
    id : string;
    deleted : boolean;
    type_ : string;
  end;

  TApiKeyCreateRequest = Class(TObject)
    name : string;
    description : string;
  end;

  TApiKey = Class(TObject)
    id : string;
    type_ : string;
    name : string;
    description : string;
    partial_key : string;
    created_at : string;
    updated_at : string;
  end;

  { TApiKeysResponse }

  TApiKeysResponse = Class(TObject)
    data : array of TApiKey;
    has_more : boolean;
    first_id : string;
    last_id : string;
    destructor destroy; override;
  end;

  TApiKeyDeleteResponse = Class(TObject)
    id : string;
    deleted : boolean;
    type_ : string;
  end;

  TCostReportRequest = Class(TObject)
    starting_at : string;
    ending_at : string;
    group_by : TStringDynArray;
    workspace_id : string;
    api_key_id : string;
    description : string;
  end;

  TCostData = Class(TObject)
    workspace_id : string;
    api_key_id : string;
    description : string;
    cost_usd : string;
  end;

  { TCostReportResponse }

  TCostReportResponse = Class(TObject)
    data : array of TCostData;
    destructor destroy; override;
  end;

  TUsageReportRequest = Class(TObject)
    starting_at : string;
    ending_at : string;
    bucket_width : string;
    group_by : TStringDynArray;
    workspace_id : string;
    api_key_id : string;
    model : string;
    service_tier : string;
    context_window : string;
  end;

  TTokenUsage = Class(TObject)
    uncached_input_tokens : integer;
    cached_input_tokens : integer;
    cache_creation_input_tokens : integer;
    output_tokens : integer;
  end;

  TUsageData = Class(TObject)
    start_time : string;
    end_time : string;
    workspace_id : string;
    api_key_id : string;
    model : string;
    service_tier : string;
    context_window : string;
    token_usage : TTokenUsage;
    web_search_count : integer;
    code_execution_count : integer;
  end;

  TUsageReportResponse = Class(TObject)
    data : array of TUsageData;
  end;

  TClaudeCodeAnalyticsRequest = Class(TObject)
    starting_at : string;
    ending_at : string;
    bucket_width : string;
    group_by : TStringDynArray;
    workspace_id : string;
  end;

  TClaudeCodeUsage = Class(TObject)
    start_time : string;
    end_time : string;
    workspace_id : string;
    active_users : integer;
    commands_executed : integer;
    conversations_started : integer;
    tokens_consumed : integer;
    cost_usd : string;
  end;

  { TClaudeCodeAnalyticsResponse }

  TClaudeCodeAnalyticsResponse = Class(TObject)
    data : array of TClaudeCodeUsage;
    destructor destroy; override;
  end;

  TMessageContent = Class(TObject)
    type_ : string;
    text : string;
  end;

  TMessage = Class(TObject)
    role : string;
    content : array of TMessageContent;
    destructor destroy; override;
  end;

  TToolParam = Class(TObject)
    type_ : string;
    description : string;
  end;

  TToolInputSchema = Class(TObject)
    type_ : string;
    properties : string;
    required : TStringDynArray;
  end;

  TTool = Class(TObject)
    name : string;
    description : string;
    input_schema : TToolInputSchema;
    destructor destroy; override;
  end;

  { TMessageRequest }

  TMessageRequest = Class(TObject)
    model : string;
    max_tokens : integer;
    messages : array of TMessage;
    system : string;
    temperature : double;
    top_k : integer;
    top_p : double;
    stop_sequences : TStringDynArray;
    stream : boolean;
    tools : array of TTool;
    tool_choice : string;
    metadata : string;
    destructor destroy; override;
  end;

  TUsage = Class(TObject)
    input_tokens : integer;
    output_tokens : integer;
    cache_creation_input_tokens : integer;
    cache_read_input_tokens : integer;
  end;

  TToolUse = Class(TObject)
    id : string;
    name : string;
    input : string;
  end;

  TMessageResponseContent = Class(TObject)
    type_ : string;
    text : string;
    id : string;
    name : string;
    input : string;
  end;

  { TMessageResponse }

  TMessageResponse = Class(TObject)
    id : string;
    type_ : string;
    role : string;
    content : array of TMessageResponseContent;
    model : string;
    stop_reason : string;
    stop_sequence : string;
    usage : TUsage;
    destructor destroy; override;
  end;

implementation

uses SysUtils;

{ TFilesResponse }

destructor TFilesResponse.destroy;
var
  i : integer;
begin
  for I:=0 to Length(data)-1 do
    FreeAndNil(Data[i]);
  inherited destroy;
end;

{ TMessageBatchCreateRequest }

destructor TMessageBatchCreateRequest.destroy;
var
  i : integer;
begin
  for I:=0 to Length(requests)-1 do
    FreeAndNil(requests[i]);
  inherited destroy;
end;

{ TMessageBatch }

destructor TMessageBatch.destroy;
begin
  FreeAndNil(request_counts);
  inherited destroy;
end;

{ TMessageBatchesResponse }

destructor TMessageBatchesResponse.destroy;
var
  i : integer;
begin
  for I:=0 to Length(data)-1 do
    FreeAndNil(data[i]);
  inherited destroy;
end;

{ TOrganizationMembersResponse }

destructor TOrganizationMembersResponse.destroy;
var
  i : integer;
begin
  for I:=0 to Length(data)-1 do
    FreeAndNil(data[i]);
  inherited destroy;
end;

{ TOrganizationInvitesResponse }

destructor TOrganizationInvitesResponse.destroy;
var
  i : integer;
begin
  for I:=0 to Length(data)-1 do
    FreeAndNil(data[i]);
  inherited destroy;
end;

{ TWorkspaceMembersResponse }

destructor TWorkspaceMembersResponse.destroy;
var
  i : integer;
begin
  for I:=0 to Length(data)-1 do
    FreeAndNil(data[i]);
  inherited destroy;
end;

{ TApiKeysResponse }

destructor TApiKeysResponse.destroy;
var
  i : integer;
begin
  for I:=0 to Length(data)-1 do
    FreeAndNil(data[i]);
  inherited destroy;
end;

{ TCostReportResponse }

destructor TCostReportResponse.destroy;
var
  i : integer;
begin
  for I:=0 to Length(data)-1 do
    FreeAndNil(data[i]);
  inherited destroy;
end;

{ TClaudeCodeAnalyticsResponse }

destructor TClaudeCodeAnalyticsResponse.destroy;
var
  i : integer;
begin
  for I:=0 to Length(data)-1 do
    FreeAndNil(data[i]);
  inherited destroy;
end;

{ TMessage }

destructor TMessage.destroy;
var
  i : integer;
begin
  for I:=0 to Length(content)-1 do
    FreeAndNil(content[i]);
  inherited destroy;
end;

{ TTool }

destructor TTool.destroy;
begin
  FreeAndNil(input_schema);
  inherited destroy;
end;

{ TMessageRequest }

destructor TMessageRequest.destroy;
var
  i : integer;
begin
  for I:=0 to Length(messages)-1 do
    FreeAndNil(messages[i]);
  for I:=0 to Length(tools)-1 do
    FreeAndNil(tools[i]);
  inherited destroy;
end;

{ TMessageResponse }

destructor TMessageResponse.destroy;
var
  i : integer;
begin
  for I:=0 to Length(content)-1 do
    FreeAndNil(content[i]);
  FreeAndNil(usage);
  inherited destroy;
end;

end.
