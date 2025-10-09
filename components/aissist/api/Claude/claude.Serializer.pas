{
    This file is part of the Free Component Library
    Copyright (c) 2025 by Michael Van Canneyt michael@freepascal.org

    Claude Rest API - Data transfer object serializers

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit claude.Serializer;

interface

{$mode objfpc}
{$h+}
{$modeswitch typehelpers}


uses
  fpJSON,
  claude.Dto;

Type
  TCompleteRequestSerializer = class helper for TCompleteRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
  end;
  
  TCompleteResponseSerializer = class helper for TCompleteResponse
    class function Deserialize(aJSON : TJSONObject) : TCompleteResponse; overload; static;
    class function Deserialize(aJSON : String) : TCompleteResponse; overload; static;
  end;

  TModelSerializer = class helper for TModel
    class function Deserialize(aJSON : TJSONObject) : TModel; overload; static;
    class function Deserialize(aJSON : String) : TModel; overload; static;
  end;

  TModelsResponseSerializer = class helper for TModelsResponse
    class function Deserialize(aJSON : TJSONObject) : TModelsResponse; overload; static;
    class function Deserialize(aJSON : String) : TModelsResponse; overload; static;
  end;

  TFileUploadRequestSerializer = class helper for TFileUploadRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
  end;

  TFileSerializer = class helper for TFile
    class function Deserialize(aJSON : TJSONObject) : TFile; overload; static;
    class function Deserialize(aJSON : String) : TFile; overload; static;
  end;

  TFilesResponseSerializer = class helper for TFilesResponse
    class function Deserialize(aJSON : TJSONObject) : TFilesResponse; overload; static;
    class function Deserialize(aJSON : String) : TFilesResponse; overload; static;
  end;

  TFileDeleteResponseSerializer = class helper for TFileDeleteResponse
    class function Deserialize(aJSON : TJSONObject) : TFileDeleteResponse; overload; static;
    class function Deserialize(aJSON : String) : TFileDeleteResponse; overload; static;
  end;

  TMessageContentSerializer = class helper for TMessageContent
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TMessageContent; overload; static;
    class function Deserialize(aJSON : String) : TMessageContent; overload; static;
  end;

  TMessageSerializer = class helper for TMessage
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TMessage; overload; static;
    class function Deserialize(aJSON : String) : TMessage; overload; static;
  end;

  TToolInputSchemaSerializer = class helper for TToolInputSchema
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TToolInputSchema; overload; static;
    class function Deserialize(aJSON : String) : TToolInputSchema; overload; static;
  end;

  TToolSerializer = class helper for TTool
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TTool; overload; static;
    class function Deserialize(aJSON : String) : TTool; overload; static;
  end;

  TMessageRequestSerializer = class helper for TMessageRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
  end;

  TUsageSerializer = class helper for TUsage
    class function Deserialize(aJSON : TJSONObject) : TUsage; overload; static;
    class function Deserialize(aJSON : String) : TUsage; overload; static;
  end;

  TMessageResponseContentSerializer = class helper for TMessageResponseContent
    class function Deserialize(aJSON : TJSONObject) : TMessageResponseContent; overload; static;
    class function Deserialize(aJSON : String) : TMessageResponseContent; overload; static;
  end;

  TMessageResponseSerializer = class helper for TMessageResponse
    class function Deserialize(aJSON : TJSONObject) : TMessageResponse; overload; static;
    class function Deserialize(aJSON : String) : TMessageResponse; overload; static;
  end;

  TBatchRequestSerializer = class helper for TBatchRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
  end;

  TMessageBatchCreateRequestSerializer = class helper for TMessageBatchCreateRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
  end;

  TBatchRequestCountsSerializer = class helper for TBatchRequestCounts
    class function Deserialize(aJSON : TJSONObject) : TBatchRequestCounts; overload; static;
    class function Deserialize(aJSON : String) : TBatchRequestCounts; overload; static;
  end;

  TMessageBatchSerializer = class helper for TMessageBatch
    class function Deserialize(aJSON : TJSONObject) : TMessageBatch; overload; static;
    class function Deserialize(aJSON : String) : TMessageBatch; overload; static;
  end;

  TMessageBatchesResponseSerializer = class helper for TMessageBatchesResponse
    class function Deserialize(aJSON : TJSONObject) : TMessageBatchesResponse; overload; static;
    class function Deserialize(aJSON : String) : TMessageBatchesResponse; overload; static;
  end;

  TOrganizationSerializer = class helper for TOrganization
    class function Deserialize(aJSON : TJSONObject) : TOrganization; overload; static;
    class function Deserialize(aJSON : String) : TOrganization; overload; static;
  end;

  TOrganizationMemberUpdateRequestSerializer = class helper for TOrganizationMemberUpdateRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
  end;

  TOrganizationMemberSerializer = class helper for TOrganizationMember
    class function Deserialize(aJSON : TJSONObject) : TOrganizationMember; overload; static;
    class function Deserialize(aJSON : String) : TOrganizationMember; overload; static;
  end;

  TOrganizationMembersResponseSerializer = class helper for TOrganizationMembersResponse
    class function Deserialize(aJSON : TJSONObject) : TOrganizationMembersResponse; overload; static;
    class function Deserialize(aJSON : String) : TOrganizationMembersResponse; overload; static;
  end;

  TOrganizationMemberDeleteResponseSerializer = class helper for TOrganizationMemberDeleteResponse
    class function Deserialize(aJSON : TJSONObject) : TOrganizationMemberDeleteResponse; overload; static;
    class function Deserialize(aJSON : String) : TOrganizationMemberDeleteResponse; overload; static;
  end;

  TOrganizationInviteCreateRequestSerializer = class helper for TOrganizationInviteCreateRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
  end;

  TOrganizationInviteSerializer = class helper for TOrganizationInvite
    class function Deserialize(aJSON : TJSONObject) : TOrganizationInvite; overload; static;
    class function Deserialize(aJSON : String) : TOrganizationInvite; overload; static;
  end;

  TOrganizationInvitesResponseSerializer = class helper for TOrganizationInvitesResponse
    class function Deserialize(aJSON : TJSONObject) : TOrganizationInvitesResponse; overload; static;
    class function Deserialize(aJSON : String) : TOrganizationInvitesResponse; overload; static;
  end;

  TOrganizationInviteDeleteResponseSerializer = class helper for TOrganizationInviteDeleteResponse
    class function Deserialize(aJSON : TJSONObject) : TOrganizationInviteDeleteResponse; overload; static;
    class function Deserialize(aJSON : String) : TOrganizationInviteDeleteResponse; overload; static;
  end;

  TWorkspaceCreateRequestSerializer = class helper for TWorkspaceCreateRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
  end;

  TWorkspaceUpdateRequestSerializer = class helper for TWorkspaceUpdateRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
  end;

  TWorkspaceSerializer = class helper for TWorkspace
    class function Deserialize(aJSON : TJSONObject) : TWorkspace; overload; static;
    class function Deserialize(aJSON : String) : TWorkspace; overload; static;
  end;

  TWorkspacesResponseSerializer = class helper for TWorkspacesResponse
    class function Deserialize(aJSON : TJSONObject) : TWorkspacesResponse; overload; static;
    class function Deserialize(aJSON : String) : TWorkspacesResponse; overload; static;
  end;

  TWorkspaceMemberAddRequestSerializer = class helper for TWorkspaceMemberAddRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
  end;

  TWorkspaceMemberUpdateRequestSerializer = class helper for TWorkspaceMemberUpdateRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
  end;

  TWorkspaceMemberSerializer = class helper for TWorkspaceMember
    class function Deserialize(aJSON : TJSONObject) : TWorkspaceMember; overload; static;
    class function Deserialize(aJSON : String) : TWorkspaceMember; overload; static;
  end;

  TWorkspaceMembersResponseSerializer = class helper for TWorkspaceMembersResponse
    class function Deserialize(aJSON : TJSONObject) : TWorkspaceMembersResponse; overload; static;
    class function Deserialize(aJSON : String) : TWorkspaceMembersResponse; overload; static;
  end;

  TWorkspaceMemberDeleteResponseSerializer = class helper for TWorkspaceMemberDeleteResponse
    class function Deserialize(aJSON : TJSONObject) : TWorkspaceMemberDeleteResponse; overload; static;
    class function Deserialize(aJSON : String) : TWorkspaceMemberDeleteResponse; overload; static;
  end;

  TApiKeyCreateRequestSerializer = class helper for TApiKeyCreateRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
  end;

  TApiKeySerializer = class helper for TApiKey
    class function Deserialize(aJSON : TJSONObject) : TApiKey; overload; static;
    class function Deserialize(aJSON : String) : TApiKey; overload; static;
  end;

  TApiKeysResponseSerializer = class helper for TApiKeysResponse
    class function Deserialize(aJSON : TJSONObject) : TApiKeysResponse; overload; static;
    class function Deserialize(aJSON : String) : TApiKeysResponse; overload; static;
  end;

  TApiKeyDeleteResponseSerializer = class helper for TApiKeyDeleteResponse
    class function Deserialize(aJSON : TJSONObject) : TApiKeyDeleteResponse; overload; static;
    class function Deserialize(aJSON : String) : TApiKeyDeleteResponse; overload; static;
  end;

  TCostReportRequestSerializer = class helper for TCostReportRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
  end;

  TCostDataSerializer = class helper for TCostData
    class function Deserialize(aJSON : TJSONObject) : TCostData; overload; static;
    class function Deserialize(aJSON : String) : TCostData; overload; static;
  end;

  TCostReportResponseSerializer = class helper for TCostReportResponse
    class function Deserialize(aJSON : TJSONObject) : TCostReportResponse; overload; static;
    class function Deserialize(aJSON : String) : TCostReportResponse; overload; static;
  end;

  TUsageReportRequestSerializer = class helper for TUsageReportRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
  end;

  TTokenUsageSerializer = class helper for TTokenUsage
    class function Deserialize(aJSON : TJSONObject) : TTokenUsage; overload; static;
    class function Deserialize(aJSON : String) : TTokenUsage; overload; static;
  end;

  TUsageDataSerializer = class helper for TUsageData
    class function Deserialize(aJSON : TJSONObject) : TUsageData; overload; static;
    class function Deserialize(aJSON : String) : TUsageData; overload; static;
  end;

  TUsageReportResponseSerializer = class helper for TUsageReportResponse
    class function Deserialize(aJSON : TJSONObject) : TUsageReportResponse; overload; static;
    class function Deserialize(aJSON : String) : TUsageReportResponse; overload; static;
  end;

  TClaudeCodeAnalyticsRequestSerializer = class helper for TClaudeCodeAnalyticsRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
  end;

  TClaudeCodeUsageSerializer = class helper for TClaudeCodeUsage
    class function Deserialize(aJSON : TJSONObject) : TClaudeCodeUsage; overload; static;
    class function Deserialize(aJSON : String) : TClaudeCodeUsage; overload; static;
  end;

  TClaudeCodeAnalyticsResponseSerializer = class helper for TClaudeCodeAnalyticsResponse
    class function Deserialize(aJSON : TJSONObject) : TClaudeCodeAnalyticsResponse; overload; static;
    class function Deserialize(aJSON : String) : TClaudeCodeAnalyticsResponse; overload; static;
  end;

implementation

uses Generics.Collections, SysUtils, Types, DateUtils, StrUtils;

function ISO8601ToDateDef(S: String; aDefault : TDateTime) : TDateTime;

begin
  if (S='') then
    Exit(aDefault);
  try
    Result:=ISO8601ToDate(S);
  except
    Result:=aDefault;
  end;
end;

function TCompleteRequestSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('max_tokens_to_sample',max_tokens_to_sample);
    if (metadata<>'') then
      Result.Add('metadata',GetJSON(metadata));
    Result.Add('model',model);
    Result.Add('prompt',prompt);
    Arr:=TJSONArray.Create;
    Result.Add('stop_sequences',Arr);
    For I:=0 to Length(stop_sequences)-1 do
      Arr.Add(stop_sequences[i]);
    Result.Add('stream',stream);
    Result.Add('temperature',temperature);
    Result.Add('top_k',top_k);
    Result.Add('top_p',top_p);
  except
    Result.Free;
    raise;
  end;
end;

function TCompleteRequestSerializer.Serialize : String;
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

class function TCompleteResponseSerializer.Deserialize(aJSON : TJSONObject) : TCompleteResponse;

begin
  Result := TCompleteResponse.Create;
  If (aJSON=Nil) then
    exit;
  Result.completion:=aJSON.Get('completion','');
  Result.stop_reason:=aJSON.Get('stop_reason','');
end;

class function TCompleteResponseSerializer.Deserialize(aJSON : String) : TCompleteResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(TCompleteResponse);
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

begin
  Result := TModel.Create;
  If (aJSON=Nil) then
    exit;
  Result.id:=aJSON.Get('id','');
  Result.type_:=aJSON.Get('type','');
  Result.display_name:=aJSON.Get('display_name','');
  Result.created_at:=aJSON.Get('created_at','');
end;

class function TModelSerializer.Deserialize(aJSON : String) : TModel;

var
  lObj : TJSONObject;
begin
  Result := Default(TModel);
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

class function TModelsResponseSerializer.Deserialize(aJSON : TJSONObject) : TModelsResponse;

var
  lArr : TJSONArray;
  i : integer;

begin
  Result := TModelsResponse.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('data',TJSONArray(nil));
  if Assigned(lArr) then
  begin
    SetLength(Result.data,lArr.Count);
    For I:=0 to lArr.Count-1 do
      Result.data[i]:=TModel.Deserialize(lArr.Objects[i]);
  end;
  Result.has_more:=aJSON.Get('has_more',false);
  Result.first_id:=aJSON.Get('first_id','');
  Result.last_id:=aJSON.Get('last_id','');
end;

class function TModelsResponseSerializer.Deserialize(aJSON : String) : TModelsResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(TModelsResponse);
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TFileUploadRequestSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('file_path',file_path);
    Result.Add('purpose',purpose);
  except
    Result.Free;
    raise;
  end;
end;

function TFileUploadRequestSerializer.Serialize : String;
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

class function TFileSerializer.Deserialize(aJSON : TJSONObject) : TFile;

begin
  Result := TFile.Create;
  If (aJSON=Nil) then
    exit;
  Result.id:=aJSON.Get('id','');
  Result.type_:=aJSON.Get('type','');
  Result.filename:=aJSON.Get('filename','');
  Result.purpose:=aJSON.Get('purpose','');
  Result.size_bytes:=aJSON.Get('size_bytes',0);
  Result.created_at:=aJSON.Get('created_at','');
end;

class function TFileSerializer.Deserialize(aJSON : String) : TFile;

var
  lObj : TJSONObject;
begin
  Result := Default(TFile);
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

class function TFilesResponseSerializer.Deserialize(aJSON : TJSONObject) : TFilesResponse;

var
  lArr : TJSONArray;
  i : integer;

begin
  Result := TFilesResponse.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('data',TJSONArray(nil));
  if Assigned(lArr) then
  begin
    SetLength(Result.data,lArr.Count);
    For I:=0 to lArr.Count-1 do
      Result.data[i]:=TFile.Deserialize(lArr.Objects[i]);
  end;
  Result.has_more:=aJSON.Get('has_more',false);
  Result.first_id:=aJSON.Get('first_id','');
  Result.last_id:=aJSON.Get('last_id','');
end;

class function TFilesResponseSerializer.Deserialize(aJSON : String) : TFilesResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(TFilesResponse);
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

class function TFileDeleteResponseSerializer.Deserialize(aJSON : TJSONObject) : TFileDeleteResponse;

begin
  Result := TFileDeleteResponse.Create;
  If (aJSON=Nil) then
    exit;
  Result.id:=aJSON.Get('id','');
  Result.deleted:=aJSON.Get('deleted',false);
  Result.type_:=aJSON.Get('type','');
end;

class function TFileDeleteResponseSerializer.Deserialize(aJSON : String) : TFileDeleteResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(TFileDeleteResponse);
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TMessageContentSerializer.SerializeObject : TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('type',type_);
  if text <> '' then
    Result.Add('text',text);
end;

function TMessageContentSerializer.Serialize : String;
var
  lJSONObject : TJSONObject;
begin
  lJSONObject:=SerializeObject;
  try
    Result:=lJSONObject.AsJSON;
  finally
    lJSONObject.Free;
  end;
end;

class function TMessageContentSerializer.Deserialize(aJSON : TJSONObject) : TMessageContent;
begin
  Result := TMessageContent.Create;
  If (aJSON=Nil) then
    exit;
  Result.type_:=aJSON.Get('type','');
  Result.text:=aJSON.Get('text','');
end;

class function TMessageContentSerializer.Deserialize(aJSON : String) : TMessageContent;
var
  lObj : TJSONObject;
begin
  Result := Default(TMessageContent);
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TMessageSerializer.SerializeObject : TJSONObject;
var
  lJSONArray : TJSONArray;
  I : Integer;
begin
  Result := TJSONObject.Create;
  Result.Add('role',role);
  if Length(content) > 0 then
  begin
    lJSONArray:=TJSONArray.Create;
    for I:=0 to Length(content)-1 do
      lJSONArray.Add(content[I].SerializeObject);
    Result.Add('content',lJSONArray);
  end;
end;

function TMessageSerializer.Serialize : String;
var
  lJSONObject : TJSONObject;
begin
  lJSONObject:=SerializeObject;
  try
    Result:=lJSONObject.AsJSON;
  finally
    lJSONObject.Free;
  end;
end;

class function TMessageSerializer.Deserialize(aJSON : TJSONObject) : TMessage;
var
  lJSONArray : TJSONArray;
  I : Integer;
begin
  Result := TMessage.Create;
  If (aJSON=Nil) then
    exit;
  Result.role:=aJSON.Get('role','');
  lJSONArray:=aJSON.Get('content',TJSONArray(nil));
  if Assigned(lJSONArray) then
  begin
    SetLength(Result.content,lJSONArray.Count);
    for I:=0 to lJSONArray.Count-1 do
      Result.content[I]:=TMessageContent.Deserialize(lJSONArray.Objects[I]);
  end;
end;

class function TMessageSerializer.Deserialize(aJSON : String) : TMessage;
var
  lObj : TJSONObject;
begin
  Result := Default(TMessage);
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TToolInputSchemaSerializer.SerializeObject : TJSONObject;
var
  lJSONArray : TJSONArray;
  I : Integer;
begin
  Result := TJSONObject.Create;
  Result.Add('type',type_);
  if properties <> '' then
    Result.Add('properties',properties);
  if Length(required) > 0 then
  begin
    lJSONArray:=TJSONArray.Create;
    for I:=0 to Length(required)-1 do
      lJSONArray.Add(required[I]);
    Result.Add('required',lJSONArray);
  end;
end;

function TToolInputSchemaSerializer.Serialize : String;
var
  lJSONObject : TJSONObject;
begin
  lJSONObject:=SerializeObject;
  try
    Result:=lJSONObject.AsJSON;
  finally
    lJSONObject.Free;
  end;
end;

class function TToolInputSchemaSerializer.Deserialize(aJSON : TJSONObject) : TToolInputSchema;
var
  lJSONArray : TJSONArray;
  I : Integer;
begin
  Result := TToolInputSchema.Create;
  If (aJSON=Nil) then
    exit;
  Result.type_:=aJSON.Get('type','');
  Result.properties:=aJSON.Get('properties','');
  lJSONArray:=aJSON.Get('required',TJSONArray(nil));
  if Assigned(lJSONArray) then
  begin
    SetLength(Result.required,lJSONArray.Count);
    for I:=0 to lJSONArray.Count-1 do
      Result.required[I]:=lJSONArray.Strings[I];
  end;
end;

class function TToolInputSchemaSerializer.Deserialize(aJSON : String) : TToolInputSchema;
var
  lObj : TJSONObject;
begin
  Result := Default(TToolInputSchema);
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TToolSerializer.SerializeObject : TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('name',name);
  if description <> '' then
    Result.Add('description',description);
  if Assigned(input_schema) then
    Result.Add('input_schema',input_schema.SerializeObject);
end;

function TToolSerializer.Serialize : String;
var
  lJSONObject : TJSONObject;
begin
  lJSONObject:=SerializeObject;
  try
    Result:=lJSONObject.AsJSON;
  finally
    lJSONObject.Free;
  end;
end;

class function TToolSerializer.Deserialize(aJSON : TJSONObject) : TTool;
var
  lSchema : TJSONObject;
begin
  Result := TTool.Create;
  If (aJSON=Nil) then
    exit;
  Result.name:=aJSON.Get('name','');
  Result.description:=aJSON.Get('description','');
  lSchema:=aJSON.Get('input_schema',TJSONObject(nil));
  if Assigned(lSchema) then
    Result.input_schema:=TToolInputSchema.Deserialize(lSchema);
end;

class function TToolSerializer.Deserialize(aJSON : String) : TTool;
var
  lObj : TJSONObject;
begin
  Result := Default(TTool);
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TMessageRequestSerializer.SerializeObject : TJSONObject;
var
  lJSONArray : TJSONArray;
  I : Integer;
begin
  Result := TJSONObject.Create;
  Result.Add('model',model);
  Result.Add('max_tokens',max_tokens);

  if Length(messages) > 0 then
  begin
    lJSONArray:=TJSONArray.Create;
    for I:=0 to Length(messages)-1 do
      lJSONArray.Add(messages[I].SerializeObject);
    Result.Add('messages',lJSONArray);
  end;

  if system <> '' then
    Result.Add('system',system);
  if temperature <> 0 then
    Result.Add('temperature',temperature);
  if top_k <> 0 then
    Result.Add('top_k',top_k);
  if top_p <> 0 then
    Result.Add('top_p',top_p);

  if Length(stop_sequences) > 0 then
  begin
    lJSONArray:=TJSONArray.Create;
    for I:=0 to Length(stop_sequences)-1 do
      lJSONArray.Add(stop_sequences[I]);
    Result.Add('stop_sequences',lJSONArray);
  end;

  if stream then
    Result.Add('stream',stream);

  if Length(tools) > 0 then
  begin
    lJSONArray:=TJSONArray.Create;
    for I:=0 to Length(tools)-1 do
      lJSONArray.Add(tools[I].SerializeObject);
    Result.Add('tools',lJSONArray);
  end;

  if tool_choice <> '' then
    Result.Add('tool_choice',tool_choice);
  if metadata <> '' then
    Result.Add('metadata',metadata);
end;

function TMessageRequestSerializer.Serialize : String;
var
  lJSONObject : TJSONObject;
begin
  lJSONObject:=SerializeObject;
  try
    Result:=lJSONObject.AsJSON;
  finally
    lJSONObject.Free;
  end;
end;

class function TUsageSerializer.Deserialize(aJSON : TJSONObject) : TUsage;
begin
  Result := TUsage.Create;
  If (aJSON=Nil) then
    exit;
  Result.input_tokens:=aJSON.Get('input_tokens',0);
  Result.output_tokens:=aJSON.Get('output_tokens',0);
  Result.cache_creation_input_tokens:=aJSON.Get('cache_creation_input_tokens',0);
  Result.cache_read_input_tokens:=aJSON.Get('cache_read_input_tokens',0);
end;

class function TUsageSerializer.Deserialize(aJSON : String) : TUsage;
var
  lObj : TJSONObject;
begin
  Result := Default(TUsage);
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

class function TMessageResponseContentSerializer.Deserialize(aJSON : TJSONObject) : TMessageResponseContent;
begin
  Result := TMessageResponseContent.Create;
  If (aJSON=Nil) then
    exit;
  Result.type_:=aJSON.Get('type','');
  Result.text:=aJSON.Get('text','');
  Result.id:=aJSON.Get('id','');
  Result.name:=aJSON.Get('name','');
  Result.input:=aJSON.Get('input','');
end;

class function TMessageResponseContentSerializer.Deserialize(aJSON : String) : TMessageResponseContent;
var
  lObj : TJSONObject;
begin
  Result := Default(TMessageResponseContent);
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

class function TMessageResponseSerializer.Deserialize(aJSON : TJSONObject) : TMessageResponse;
var
  lJSONArray : TJSONArray;
  lUsage : TJSONObject;
  I : Integer;
begin
  Result := TMessageResponse.Create;
  If (aJSON=Nil) then
    exit;
  Result.id:=aJSON.Get('id','');
  Result.type_:=aJSON.Get('type','');
  Result.role:=aJSON.Get('role','');
  Result.model:=aJSON.Get('model','');
  Result.stop_reason:=aJSON.Get('stop_reason','');
  Result.stop_sequence:=aJSON.Get('stop_sequence','');

  lJSONArray:=aJSON.Get('content',TJSONArray(nil));
  if Assigned(lJSONArray) then
  begin
    SetLength(Result.content,lJSONArray.Count);
    for I:=0 to lJSONArray.Count-1 do
      Result.content[I]:=TMessageResponseContent.Deserialize(lJSONArray.Objects[I]);
  end;

  lUsage:=aJSON.Get('usage',TJSONObject(nil));
  if Assigned(lUsage) then
    Result.usage:=TUsage.Deserialize(lUsage);
end;

class function TMessageResponseSerializer.Deserialize(aJSON : String) : TMessageResponse;
var
  lObj : TJSONObject;
begin
  Result := Default(TMessageResponse);
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TBatchRequestSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('custom_id',custom_id);
    Result.Add('method',method);
    Result.Add('url',url);
    if (body<>'') then
      Result.Add('body',GetJSON(body));
  except
    Result.Free;
    raise;
  end;
end;

function TBatchRequestSerializer.Serialize : String;
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

function TMessageBatchCreateRequestSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('requests',Arr);
    For I:=0 to Length(requests)-1 do
      Arr.Add(requests[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function TMessageBatchCreateRequestSerializer.Serialize : String;
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

class function TBatchRequestCountsSerializer.Deserialize(aJSON : TJSONObject) : TBatchRequestCounts;

begin
  Result := TBatchRequestCounts.Create;
  If (aJSON=Nil) then
    exit;
  Result.processing:=aJSON.Get('processing',0);
  Result.succeeded:=aJSON.Get('succeeded',0);
  Result.errored:=aJSON.Get('errored',0);
  Result.canceled:=aJSON.Get('canceled',0);
  Result.expired:=aJSON.Get('expired',0);
end;

class function TBatchRequestCountsSerializer.Deserialize(aJSON : String) : TBatchRequestCounts;

var
  lObj : TJSONObject;
begin
  Result := Default(TBatchRequestCounts);
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

class function TMessageBatchSerializer.Deserialize(aJSON : TJSONObject) : TMessageBatch;

var
  lRequestCounts : TJSONObject;

begin
  Result := TMessageBatch.Create;
  If (aJSON=Nil) then
    exit;
  Result.id:=aJSON.Get('id','');
  Result.type_:=aJSON.Get('type','');
  Result.processing_status:=aJSON.Get('processing_status','');
  lRequestCounts:=aJSON.Get('request_counts',TJSONObject(nil));
  if Assigned(lRequestCounts) then
    Result.request_counts:=TBatchRequestCounts.Deserialize(lRequestCounts);
  Result.ended_at:=aJSON.Get('ended_at','');
  Result.created_at:=aJSON.Get('created_at','');
  Result.expires_at:=aJSON.Get('expires_at','');
  Result.archived_at:=aJSON.Get('archived_at','');
  Result.cancel_initiated_at:=aJSON.Get('cancel_initiated_at','');
  Result.results_url:=aJSON.Get('results_url','');
end;

class function TMessageBatchSerializer.Deserialize(aJSON : String) : TMessageBatch;

var
  lObj : TJSONObject;
begin
  Result := Default(TMessageBatch);
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

class function TMessageBatchesResponseSerializer.Deserialize(aJSON : TJSONObject) : TMessageBatchesResponse;

var
  lArr : TJSONArray;
  i : integer;

begin
  Result := TMessageBatchesResponse.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('data',TJSONArray(nil));
  if Assigned(lArr) then
  begin
    SetLength(Result.data,lArr.Count);
    For I:=0 to lArr.Count-1 do
      Result.data[i]:=TMessageBatch.Deserialize(lArr.Objects[i]);
  end;
  Result.has_more:=aJSON.Get('has_more',false);
  Result.first_id:=aJSON.Get('first_id','');
  Result.last_id:=aJSON.Get('last_id','');
end;

class function TMessageBatchesResponseSerializer.Deserialize(aJSON : String) : TMessageBatchesResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(TMessageBatchesResponse);
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

class function TOrganizationSerializer.Deserialize(aJSON : TJSONObject) : TOrganization;

begin
  Result := TOrganization.Create;
  If (aJSON=Nil) then
    exit;
  Result.id:=aJSON.Get('id','');
  Result.type_:=aJSON.Get('type','');
  Result.display_name:=aJSON.Get('display_name','');
  Result.created_at:=aJSON.Get('created_at','');
end;

class function TOrganizationSerializer.Deserialize(aJSON : String) : TOrganization;

var
  lObj : TJSONObject;
begin
  Result := Default(TOrganization);
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TOrganizationMemberUpdateRequestSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('role',role);
  except
    Result.Free;
    raise;
  end;
end;

function TOrganizationMemberUpdateRequestSerializer.Serialize : String;
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

class function TOrganizationMemberSerializer.Deserialize(aJSON : TJSONObject) : TOrganizationMember;

begin
  Result := TOrganizationMember.Create;
  If (aJSON=Nil) then
    exit;
  Result.id:=aJSON.Get('id','');
  Result.type_:=aJSON.Get('type','');
  Result.role:=aJSON.Get('role','');
  Result.email:=aJSON.Get('email','');
  Result.display_name:=aJSON.Get('display_name','');
  Result.created_at:=aJSON.Get('created_at','');
  Result.updated_at:=aJSON.Get('updated_at','');
end;

class function TOrganizationMemberSerializer.Deserialize(aJSON : String) : TOrganizationMember;

var
  lObj : TJSONObject;
begin
  Result := Default(TOrganizationMember);
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

class function TOrganizationMembersResponseSerializer.Deserialize(aJSON : TJSONObject) : TOrganizationMembersResponse;

var
  lArr : TJSONArray;
  i : integer;

begin
  Result := TOrganizationMembersResponse.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('data',TJSONArray(nil));
  if Assigned(lArr) then
  begin
    SetLength(Result.data,lArr.Count);
    For I:=0 to lArr.Count-1 do
      Result.data[i]:=TOrganizationMember.Deserialize(lArr.Objects[i]);
  end;
  Result.has_more:=aJSON.Get('has_more',false);
  Result.first_id:=aJSON.Get('first_id','');
  Result.last_id:=aJSON.Get('last_id','');
end;

class function TOrganizationMembersResponseSerializer.Deserialize(aJSON : String) : TOrganizationMembersResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(TOrganizationMembersResponse);
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

class function TOrganizationMemberDeleteResponseSerializer.Deserialize(aJSON : TJSONObject) : TOrganizationMemberDeleteResponse;

begin
  Result := TOrganizationMemberDeleteResponse.Create;
  If (aJSON=Nil) then
    exit;
  Result.id:=aJSON.Get('id','');
  Result.deleted:=aJSON.Get('deleted',false);
  Result.type_:=aJSON.Get('type','');
end;

class function TOrganizationMemberDeleteResponseSerializer.Deserialize(aJSON : String) : TOrganizationMemberDeleteResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(TOrganizationMemberDeleteResponse);
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TOrganizationInviteCreateRequestSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('email',email);
    Result.Add('role',role);
  except
    Result.Free;
    raise;
  end;
end;

function TOrganizationInviteCreateRequestSerializer.Serialize : String;
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

class function TOrganizationInviteSerializer.Deserialize(aJSON : TJSONObject) : TOrganizationInvite;

begin
  Result := TOrganizationInvite.Create;
  If (aJSON=Nil) then
    exit;
  Result.id:=aJSON.Get('id','');
  Result.type_:=aJSON.Get('type','');
  Result.email:=aJSON.Get('email','');
  Result.role:=aJSON.Get('role','');
  Result.status:=aJSON.Get('status','');
  Result.created_at:=aJSON.Get('created_at','');
  Result.expires_at:=aJSON.Get('expires_at','');
end;

class function TOrganizationInviteSerializer.Deserialize(aJSON : String) : TOrganizationInvite;

var
  lObj : TJSONObject;
begin
  Result := Default(TOrganizationInvite);
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

class function TOrganizationInvitesResponseSerializer.Deserialize(aJSON : TJSONObject) : TOrganizationInvitesResponse;

var
  lArr : TJSONArray;
  i : integer;

begin
  Result := TOrganizationInvitesResponse.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('data',TJSONArray(nil));
  if Assigned(lArr) then
  begin
    SetLength(Result.data,lArr.Count);
    For I:=0 to lArr.Count-1 do
      Result.data[i]:=TOrganizationInvite.Deserialize(lArr.Objects[i]);
  end;
  Result.has_more:=aJSON.Get('has_more',false);
  Result.first_id:=aJSON.Get('first_id','');
  Result.last_id:=aJSON.Get('last_id','');
end;

class function TOrganizationInvitesResponseSerializer.Deserialize(aJSON : String) : TOrganizationInvitesResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(TOrganizationInvitesResponse);
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

class function TOrganizationInviteDeleteResponseSerializer.Deserialize(aJSON : TJSONObject) : TOrganizationInviteDeleteResponse;

begin
  Result := TOrganizationInviteDeleteResponse.Create;
  If (aJSON=Nil) then
    exit;
  Result.id:=aJSON.Get('id','');
  Result.deleted:=aJSON.Get('deleted',false);
  Result.type_:=aJSON.Get('type','');
end;

class function TOrganizationInviteDeleteResponseSerializer.Deserialize(aJSON : String) : TOrganizationInviteDeleteResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(TOrganizationInviteDeleteResponse);
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TWorkspaceCreateRequestSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('name',name);
    if (description<>'') then
      Result.Add('description',description);
  except
    Result.Free;
    raise;
  end;
end;

function TWorkspaceCreateRequestSerializer.Serialize : String;
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

function TWorkspaceUpdateRequestSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    if (name<>'') then
      Result.Add('name',name);
    if (description<>'') then
      Result.Add('description',description);
  except
    Result.Free;
    raise;
  end;
end;

function TWorkspaceUpdateRequestSerializer.Serialize : String;
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

class function TWorkspaceSerializer.Deserialize(aJSON : TJSONObject) : TWorkspace;

begin
  Result := TWorkspace.Create;
  If (aJSON=Nil) then
    exit;
  Result.id:=aJSON.Get('id','');
  Result.type_:=aJSON.Get('type','');
  Result.name:=aJSON.Get('name','');
  Result.description:=aJSON.Get('description','');
  Result.status:=aJSON.Get('status','');
  Result.created_at:=aJSON.Get('created_at','');
  Result.updated_at:=aJSON.Get('updated_at','');
  Result.archived_at:=aJSON.Get('archived_at','');
end;

class function TWorkspaceSerializer.Deserialize(aJSON : String) : TWorkspace;

var
  lObj : TJSONObject;
begin
  Result := Default(TWorkspace);
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

class function TWorkspacesResponseSerializer.Deserialize(aJSON : TJSONObject) : TWorkspacesResponse;

var
  lArr : TJSONArray;
  i : integer;

begin
  Result := TWorkspacesResponse.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('data',TJSONArray(nil));
  if Assigned(lArr) then
  begin
    SetLength(Result.data,lArr.Count);
    For I:=0 to lArr.Count-1 do
      Result.data[i]:=TWorkspace.Deserialize(lArr.Objects[i]);
  end;
  Result.has_more:=aJSON.Get('has_more',false);
  Result.first_id:=aJSON.Get('first_id','');
  Result.last_id:=aJSON.Get('last_id','');
end;

class function TWorkspacesResponseSerializer.Deserialize(aJSON : String) : TWorkspacesResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(TWorkspacesResponse);
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TWorkspaceMemberAddRequestSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('user_id',user_id);
    Result.Add('workspace_role',workspace_role);
  except
    Result.Free;
    raise;
  end;
end;

function TWorkspaceMemberAddRequestSerializer.Serialize : String;
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

function TWorkspaceMemberUpdateRequestSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('workspace_role',workspace_role);
  except
    Result.Free;
    raise;
  end;
end;

function TWorkspaceMemberUpdateRequestSerializer.Serialize : String;
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

class function TWorkspaceMemberSerializer.Deserialize(aJSON : TJSONObject) : TWorkspaceMember;

begin
  Result := TWorkspaceMember.Create;
  If (aJSON=Nil) then
    exit;
  Result.id:=aJSON.Get('id','');
  Result.type_:=aJSON.Get('type','');
  Result.user_id:=aJSON.Get('user_id','');
  Result.workspace_role:=aJSON.Get('workspace_role','');
  Result.created_at:=aJSON.Get('created_at','');
  Result.updated_at:=aJSON.Get('updated_at','');
end;

class function TWorkspaceMemberSerializer.Deserialize(aJSON : String) : TWorkspaceMember;

var
  lObj : TJSONObject;
begin
  Result := Default(TWorkspaceMember);
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

class function TWorkspaceMembersResponseSerializer.Deserialize(aJSON : TJSONObject) : TWorkspaceMembersResponse;

var
  lArr : TJSONArray;
  i : integer;

begin
  Result := TWorkspaceMembersResponse.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('data',TJSONArray(nil));
  if Assigned(lArr) then
  begin
    SetLength(Result.data,lArr.Count);
    For I:=0 to lArr.Count-1 do
      Result.data[i]:=TWorkspaceMember.Deserialize(lArr.Objects[i]);
  end;
  Result.has_more:=aJSON.Get('has_more',false);
  Result.first_id:=aJSON.Get('first_id','');
  Result.last_id:=aJSON.Get('last_id','');
end;

class function TWorkspaceMembersResponseSerializer.Deserialize(aJSON : String) : TWorkspaceMembersResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(TWorkspaceMembersResponse);
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

class function TWorkspaceMemberDeleteResponseSerializer.Deserialize(aJSON : TJSONObject) : TWorkspaceMemberDeleteResponse;

begin
  Result := TWorkspaceMemberDeleteResponse.Create;
  If (aJSON=Nil) then
    exit;
  Result.id:=aJSON.Get('id','');
  Result.deleted:=aJSON.Get('deleted',false);
  Result.type_:=aJSON.Get('type','');
end;

class function TWorkspaceMemberDeleteResponseSerializer.Deserialize(aJSON : String) : TWorkspaceMemberDeleteResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(TWorkspaceMemberDeleteResponse);
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TApiKeyCreateRequestSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('name',name);
    if (description<>'') then
      Result.Add('description',description);
  except
    Result.Free;
    raise;
  end;
end;

function TApiKeyCreateRequestSerializer.Serialize : String;
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

class function TApiKeySerializer.Deserialize(aJSON : TJSONObject) : TApiKey;

begin
  Result := TApiKey.Create;
  If (aJSON=Nil) then
    exit;
  Result.id:=aJSON.Get('id','');
  Result.type_:=aJSON.Get('type','');
  Result.name:=aJSON.Get('name','');
  Result.description:=aJSON.Get('description','');
  Result.partial_key:=aJSON.Get('partial_key','');
  Result.created_at:=aJSON.Get('created_at','');
  Result.updated_at:=aJSON.Get('updated_at','');
end;

class function TApiKeySerializer.Deserialize(aJSON : String) : TApiKey;

var
  lObj : TJSONObject;
begin
  Result := Default(TApiKey);
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

class function TApiKeysResponseSerializer.Deserialize(aJSON : TJSONObject) : TApiKeysResponse;

var
  lArr : TJSONArray;
  i : integer;

begin
  Result := TApiKeysResponse.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('data',TJSONArray(nil));
  if Assigned(lArr) then
  begin
    SetLength(Result.data,lArr.Count);
    For I:=0 to lArr.Count-1 do
      Result.data[i]:=TApiKey.Deserialize(lArr.Objects[i]);
  end;
  Result.has_more:=aJSON.Get('has_more',false);
  Result.first_id:=aJSON.Get('first_id','');
  Result.last_id:=aJSON.Get('last_id','');
end;

class function TApiKeysResponseSerializer.Deserialize(aJSON : String) : TApiKeysResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(TApiKeysResponse);
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

class function TApiKeyDeleteResponseSerializer.Deserialize(aJSON : TJSONObject) : TApiKeyDeleteResponse;

begin
  Result := TApiKeyDeleteResponse.Create;
  If (aJSON=Nil) then
    exit;
  Result.id:=aJSON.Get('id','');
  Result.deleted:=aJSON.Get('deleted',false);
  Result.type_:=aJSON.Get('type','');
end;

class function TApiKeyDeleteResponseSerializer.Deserialize(aJSON : String) : TApiKeyDeleteResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(TApiKeyDeleteResponse);
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TCostReportRequestSerializer.SerializeObject : TJSONObject;

var
  lArr : TJSONArray;
  i : integer;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('starting_at',starting_at);
    Result.Add('ending_at',ending_at);
    if Length(group_by) > 0 then
    begin
      lArr := TJSONArray.Create;
      For i := 0 to High(group_by) do
        lArr.Add(group_by[i]);
      Result.Add('group_by',lArr);
    end;
    if (workspace_id<>'') then
      Result.Add('workspace_id',workspace_id);
    if (api_key_id<>'') then
      Result.Add('api_key_id',api_key_id);
    if (description<>'') then
      Result.Add('description',description);
  except
    Result.Free;
    raise;
  end;
end;

function TCostReportRequestSerializer.Serialize : String;
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

class function TCostDataSerializer.Deserialize(aJSON : TJSONObject) : TCostData;

begin
  Result := TCostData.Create;
  If (aJSON=Nil) then
    exit;
  Result.workspace_id:=aJSON.Get('workspace_id','');
  Result.api_key_id:=aJSON.Get('api_key_id','');
  Result.description:=aJSON.Get('description','');
  Result.cost_usd:=aJSON.Get('cost_usd','');
end;

class function TCostDataSerializer.Deserialize(aJSON : String) : TCostData;

var
  lObj : TJSONObject;
begin
  Result := Default(TCostData);
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

class function TCostReportResponseSerializer.Deserialize(aJSON : TJSONObject) : TCostReportResponse;

var
  lArr : TJSONArray;
  i : integer;

begin
  Result := TCostReportResponse.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('data',TJSONArray(nil));
  if Assigned(lArr) then
  begin
    SetLength(Result.data,lArr.Count);
    For I:=0 to lArr.Count-1 do
      Result.data[i]:=TCostData.Deserialize(lArr.Objects[i]);
  end;
end;

class function TCostReportResponseSerializer.Deserialize(aJSON : String) : TCostReportResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(TCostReportResponse);
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TUsageReportRequestSerializer.SerializeObject : TJSONObject;

var
  lArr : TJSONArray;
  i : integer;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('starting_at',starting_at);
    Result.Add('ending_at',ending_at);
    if (bucket_width<>'') then
      Result.Add('bucket_width',bucket_width);
    if Length(group_by) > 0 then
    begin
      lArr := TJSONArray.Create;
      For i := 0 to High(group_by) do
        lArr.Add(group_by[i]);
      Result.Add('group_by',lArr);
    end;
    if (workspace_id<>'') then
      Result.Add('workspace_id',workspace_id);
    if (api_key_id<>'') then
      Result.Add('api_key_id',api_key_id);
    if (model<>'') then
      Result.Add('model',model);
    if (service_tier<>'') then
      Result.Add('service_tier',service_tier);
    if (context_window<>'') then
      Result.Add('context_window',context_window);
  except
    Result.Free;
    raise;
  end;
end;

function TUsageReportRequestSerializer.Serialize : String;
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

class function TTokenUsageSerializer.Deserialize(aJSON : TJSONObject) : TTokenUsage;

begin
  Result := TTokenUsage.Create;
  If (aJSON=Nil) then
    exit;
  Result.uncached_input_tokens:=aJSON.Get('uncached_input_tokens',0);
  Result.cached_input_tokens:=aJSON.Get('cached_input_tokens',0);
  Result.cache_creation_input_tokens:=aJSON.Get('cache_creation_input_tokens',0);
  Result.output_tokens:=aJSON.Get('output_tokens',0);
end;

class function TTokenUsageSerializer.Deserialize(aJSON : String) : TTokenUsage;

var
  lObj : TJSONObject;
begin
  Result := Default(TTokenUsage);
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

class function TUsageDataSerializer.Deserialize(aJSON : TJSONObject) : TUsageData;

var
  lTokenUsageObj : TJSONObject;

begin
  Result := TUsageData.Create;
  If (aJSON=Nil) then
    exit;
  Result.start_time:=aJSON.Get('start_time','');
  Result.end_time:=aJSON.Get('end_time','');
  Result.workspace_id:=aJSON.Get('workspace_id','');
  Result.api_key_id:=aJSON.Get('api_key_id','');
  Result.model:=aJSON.Get('model','');
  Result.service_tier:=aJSON.Get('service_tier','');
  Result.context_window:=aJSON.Get('context_window','');
  lTokenUsageObj:=aJSON.Get('token_usage',TJSONObject(nil));
  if Assigned(lTokenUsageObj) then
    Result.token_usage:=TTokenUsage.Deserialize(lTokenUsageObj)
  else
    Result.token_usage:=TTokenUsage.Create;
  Result.web_search_count:=aJSON.Get('web_search_count',0);
  Result.code_execution_count:=aJSON.Get('code_execution_count',0);
end;

class function TUsageDataSerializer.Deserialize(aJSON : String) : TUsageData;

var
  lObj : TJSONObject;
begin
  Result := Default(TUsageData);
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

class function TUsageReportResponseSerializer.Deserialize(aJSON : TJSONObject) : TUsageReportResponse;

var
  lArr : TJSONArray;
  i : integer;

begin
  Result := TUsageReportResponse.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('data',TJSONArray(nil));
  if Assigned(lArr) then
  begin
    SetLength(Result.data,lArr.Count);
    For I:=0 to lArr.Count-1 do
      Result.data[i]:=TUsageData.Deserialize(lArr.Objects[i]);
  end;
end;

class function TUsageReportResponseSerializer.Deserialize(aJSON : String) : TUsageReportResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(TUsageReportResponse);
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TClaudeCodeAnalyticsRequestSerializer.SerializeObject : TJSONObject;

var
  lArr : TJSONArray;
  i : integer;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('starting_at',starting_at);
    Result.Add('ending_at',ending_at);
    if (bucket_width<>'') then
      Result.Add('bucket_width',bucket_width);
    if Length(group_by) > 0 then
    begin
      lArr := TJSONArray.Create;
      For i := 0 to High(group_by) do
        lArr.Add(group_by[i]);
      Result.Add('group_by',lArr);
    end;
    if (workspace_id<>'') then
      Result.Add('workspace_id',workspace_id);
  except
    Result.Free;
    raise;
  end;
end;

function TClaudeCodeAnalyticsRequestSerializer.Serialize : String;
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

class function TClaudeCodeUsageSerializer.Deserialize(aJSON : TJSONObject) : TClaudeCodeUsage;

begin
  Result := TClaudeCodeUsage.Create;
  If (aJSON=Nil) then
    exit;
  Result.start_time:=aJSON.Get('start_time','');
  Result.end_time:=aJSON.Get('end_time','');
  Result.workspace_id:=aJSON.Get('workspace_id','');
  Result.active_users:=aJSON.Get('active_users',0);
  Result.commands_executed:=aJSON.Get('commands_executed',0);
  Result.conversations_started:=aJSON.Get('conversations_started',0);
  Result.tokens_consumed:=aJSON.Get('tokens_consumed',0);
  Result.cost_usd:=aJSON.Get('cost_usd','');
end;

class function TClaudeCodeUsageSerializer.Deserialize(aJSON : String) : TClaudeCodeUsage;

var
  lObj : TJSONObject;
begin
  Result := Default(TClaudeCodeUsage);
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

class function TClaudeCodeAnalyticsResponseSerializer.Deserialize(aJSON : TJSONObject) : TClaudeCodeAnalyticsResponse;

var
  lArr : TJSONArray;
  i : integer;

begin
  Result := TClaudeCodeAnalyticsResponse.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('data',TJSONArray(nil));
  if Assigned(lArr) then
  begin
    SetLength(Result.data,lArr.Count);
    For I:=0 to lArr.Count-1 do
      Result.data[i]:=TClaudeCodeUsage.Deserialize(lArr.Objects[i]);
  end;
end;

class function TClaudeCodeAnalyticsResponseSerializer.Deserialize(aJSON : String) : TClaudeCodeAnalyticsResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(TClaudeCodeAnalyticsResponse);
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
