{
    This file is part of the Free Component Library
    Copyright (c) 2025 by Michael Van Canneyt michael@freepascal.org

    Claude Rest API - Service interface implementations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit claude.Service.Impl;

{$mode objfpc}
{$h+}

interface

uses
  fpopenapiclient
  , claude.Service.Intf                     // Service definition 
  , claude.Dto;

Type
  // Service ICompleteService

  TCompleteServiceProxy = Class (TFPOpenAPIServiceClient,ICompleteService)
    Function Post(aBody : TCompleteRequest) : TCompleteResponseServiceResult;
  end;

  // Service IModelsService

  TModelsServiceProxy = Class (TFPOpenAPIServiceClient,IModelsService)
    Function List : TModelsResponseServiceResult;
    Function Get(aModelId : String) : TModelServiceResult;
  end;

  // Service IFilesService

  TFilesServiceProxy = Class (TFPOpenAPIServiceClient,IFilesService)
    Function Upload(aBody : TFileUploadRequest) : TFileServiceResult;
    Function List : TFilesResponseServiceResult;
    Function Get(aFileId : String) : TFileServiceResult;
    Function Delete(aFileId : String) : TFileDeleteResponseServiceResult;
  end;

  // Service IMessagesService

  TMessagesServiceProxy = Class (TFPOpenAPIServiceClient,IMessagesService)
    Function CreateMessage(aBody : TMessageRequest) : TMessageResponseServiceResult;
  end;

  // Service IMessageBatchesService

  TMessageBatchesServiceProxy = Class (TFPOpenAPIServiceClient,IMessageBatchesService)
    Function CreateBatch(aBody : TMessageBatchCreateRequest) : TMessageBatchServiceResult;
    Function List : TMessageBatchesResponseServiceResult;
    Function Get(aBatchId : String) : TMessageBatchServiceResult;
    Function Cancel(aBatchId : String) : TMessageBatchServiceResult;
  end;

  // Service IOrganizationsService

  TOrganizationsServiceProxy = Class (TFPOpenAPIServiceClient,IOrganizationsService)
    Function Get(aOrganizationId : String) : TOrganizationServiceResult;
  end;

  // Service IOrganizationMembersService

  TOrganizationMembersServiceProxy = Class (TFPOpenAPIServiceClient,IOrganizationMembersService)
    Function List(aOrganizationId : String) : TOrganizationMembersResponseServiceResult;
    Function Get(aOrganizationId, aMemberId : String) : TOrganizationMemberServiceResult;
    Function Update(aOrganizationId, aMemberId : String; aBody : TOrganizationMemberUpdateRequest) : TOrganizationMemberServiceResult;
    Function Delete(aOrganizationId, aMemberId : String) : TOrganizationMemberDeleteResponseServiceResult;
  end;

  // Service IOrganizationInvitesService

  TOrganizationInvitesServiceProxy = Class (TFPOpenAPIServiceClient,IOrganizationInvitesService)
    Function CreateInvite(aOrganizationId : String; aBody : TOrganizationInviteCreateRequest) : TOrganizationInviteServiceResult;
    Function List(aOrganizationId : String) : TOrganizationInvitesResponseServiceResult;
    Function Get(aOrganizationId, aInviteId : String) : TOrganizationInviteServiceResult;
    Function Delete(aOrganizationId, aInviteId : String) : TOrganizationInviteDeleteResponseServiceResult;
  end;

  // Service IWorkspacesService

  TWorkspacesServiceProxy = Class (TFPOpenAPIServiceClient,IWorkspacesService)
    Function List(aOrganizationId : String) : TWorkspacesResponseServiceResult;
    Function CreateWorkspace(aOrganizationId : String; aBody : TWorkspaceCreateRequest) : TWorkspaceServiceResult;
    Function Get(aOrganizationId, aWorkspaceId : String) : TWorkspaceServiceResult;
    Function Update(aOrganizationId, aWorkspaceId : String; aBody : TWorkspaceUpdateRequest) : TWorkspaceServiceResult;
    Function Archive(aOrganizationId, aWorkspaceId : String) : TWorkspaceServiceResult;
  end;

  // Service IWorkspaceMembersService

  TWorkspaceMembersServiceProxy = Class (TFPOpenAPIServiceClient,IWorkspaceMembersService)
    Function List(aOrganizationId, aWorkspaceId : String) : TWorkspaceMembersResponseServiceResult;
    Function Add(aOrganizationId, aWorkspaceId : String; aBody : TWorkspaceMemberAddRequest) : TWorkspaceMemberServiceResult;
    Function Update(aOrganizationId, aWorkspaceId, aMemberId : String; aBody : TWorkspaceMemberUpdateRequest) : TWorkspaceMemberServiceResult;
    Function Remove(aOrganizationId, aWorkspaceId, aMemberId : String) : TWorkspaceMemberDeleteResponseServiceResult;
  end;

  // Service IApiKeysService

  TApiKeysServiceProxy = Class (TFPOpenAPIServiceClient,IApiKeysService)
    Function List(aOrganizationId : String) : TApiKeysResponseServiceResult;
    Function CreateKey(aOrganizationId : String; aBody : TApiKeyCreateRequest) : TApiKeyServiceResult;
    Function Get(aOrganizationId, aApiKeyId : String) : TApiKeyServiceResult;
    Function Delete(aOrganizationId, aApiKeyId : String) : TApiKeyDeleteResponseServiceResult;
  end;

  // Service IUsageCostService

  TUsageCostServiceProxy = Class (TFPOpenAPIServiceClient,IUsageCostService)
    Function GetCostReport(aBody : TCostReportRequest) : TCostReportResponseServiceResult;
    Function GetUsageReport(aBody : TUsageReportRequest) : TUsageReportResponseServiceResult;
    Function GetClaudeCodeAnalytics(aBody : TClaudeCodeAnalyticsRequest) : TClaudeCodeAnalyticsResponseServiceResult;
  end;


implementation

uses
  SysUtils
  , claude.Serializer;

Function TCompleteServiceProxy.Post(aBody : TCompleteRequest) : TCompleteResponseServiceResult;

const
  lMethodURL = '/v1/complete';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TCompleteResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('post',lURL,aBody.Serialize);
  Result:=TCompleteResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TCompleteResponse.Deserialize(lResponse.Content);
end;

Function TModelsServiceProxy.List : TModelsResponseServiceResult;

const
  lMethodURL = '/v1/models';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TModelsResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TModelsResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TModelsResponse.Deserialize(lResponse.Content);
end;

Function TModelsServiceProxy.Get(aModelId : String) : TModelServiceResult;

const
  lMethodURL = '/v1/models/';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TModelServiceResult);
  lURL:=BuildEndPointURL(lMethodURL + aModelId);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TModelServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TModel.Deserialize(lResponse.Content);
end;

Function TFilesServiceProxy.Upload(aBody : TFileUploadRequest) : TFileServiceResult;

const
  lMethodURL = '/v1/files';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TFileServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('post',lURL,aBody.Serialize);
  Result:=TFileServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TFile.Deserialize(lResponse.Content);
end;

Function TFilesServiceProxy.List : TFilesResponseServiceResult;

const
  lMethodURL = '/v1/files';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TFilesResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TFilesResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TFilesResponse.Deserialize(lResponse.Content);
end;

Function TFilesServiceProxy.Get(aFileId : String) : TFileServiceResult;

const
  lMethodURL = '/v1/files/';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TFileServiceResult);
  lURL:=BuildEndPointURL(lMethodURL + aFileId);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TFileServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TFile.Deserialize(lResponse.Content);
end;

Function TFilesServiceProxy.Delete(aFileId : String) : TFileDeleteResponseServiceResult;

const
  lMethodURL = '/v1/files/';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TFileDeleteResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL + aFileId);
  lResponse:=ExecuteRequest('delete',lURL,'');
  Result:=TFileDeleteResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TFileDeleteResponse.Deserialize(lResponse.Content);
end;

Function TMessagesServiceProxy.CreateMessage(aBody : TMessageRequest) : TMessageResponseServiceResult;

const
  lMethodURL = '/v1/messages';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TMessageResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('post',lURL,aBody.Serialize);
  Result:=TMessageResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TMessageResponse.Deserialize(lResponse.Content);
end;

Function TMessageBatchesServiceProxy.CreateBatch(aBody : TMessageBatchCreateRequest) : TMessageBatchServiceResult;

const
  lMethodURL = '/v1/messages/batches';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TMessageBatchServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('post',lURL,aBody.Serialize);
  Result:=TMessageBatchServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TMessageBatch.Deserialize(lResponse.Content);
end;

Function TMessageBatchesServiceProxy.List : TMessageBatchesResponseServiceResult;

const
  lMethodURL = '/v1/messages/batches';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TMessageBatchesResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TMessageBatchesResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TMessageBatchesResponse.Deserialize(lResponse.Content);
end;

Function TMessageBatchesServiceProxy.Get(aBatchId : String) : TMessageBatchServiceResult;

const
  lMethodURL = '/v1/messages/batches/';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TMessageBatchServiceResult);
  lURL:=BuildEndPointURL(lMethodURL + aBatchId);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TMessageBatchServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TMessageBatch.Deserialize(lResponse.Content);
end;

Function TMessageBatchesServiceProxy.Cancel(aBatchId : String) : TMessageBatchServiceResult;

const
  lMethodURL = '/v1/messages/batches/';
  lCancelSuffix = '/cancel';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TMessageBatchServiceResult);
  lURL:=BuildEndPointURL(lMethodURL + aBatchId + lCancelSuffix);
  lResponse:=ExecuteRequest('post',lURL,'');
  Result:=TMessageBatchServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TMessageBatch.Deserialize(lResponse.Content);
end;

Function TOrganizationsServiceProxy.Get(aOrganizationId : String) : TOrganizationServiceResult;

const
  lMethodURL = '/v1/organizations/';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TOrganizationServiceResult);
  lURL:=BuildEndPointURL(lMethodURL + aOrganizationId);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TOrganizationServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TOrganization.Deserialize(lResponse.Content);
end;

Function TOrganizationMembersServiceProxy.List(aOrganizationId : String) : TOrganizationMembersResponseServiceResult;

const
  lMethodURL = '/v1/organizations/';
  lMembersSuffix = '/members';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TOrganizationMembersResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL + aOrganizationId + lMembersSuffix);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TOrganizationMembersResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TOrganizationMembersResponse.Deserialize(lResponse.Content);
end;

Function TOrganizationMembersServiceProxy.Get(aOrganizationId, aMemberId : String) : TOrganizationMemberServiceResult;

const
  lMethodURL = '/v1/organizations/';
  lMembersSuffix = '/members/';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TOrganizationMemberServiceResult);
  lURL:=BuildEndPointURL(lMethodURL + aOrganizationId + lMembersSuffix + aMemberId);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TOrganizationMemberServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TOrganizationMember.Deserialize(lResponse.Content);
end;

Function TOrganizationMembersServiceProxy.Update(aOrganizationId, aMemberId : String; aBody : TOrganizationMemberUpdateRequest) : TOrganizationMemberServiceResult;

const
  lMethodURL = '/v1/organizations/';
  lMembersSuffix = '/members/';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TOrganizationMemberServiceResult);
  lURL:=BuildEndPointURL(lMethodURL + aOrganizationId + lMembersSuffix + aMemberId);
  lResponse:=ExecuteRequest('patch',lURL,aBody.Serialize);
  Result:=TOrganizationMemberServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TOrganizationMember.Deserialize(lResponse.Content);
end;

Function TOrganizationMembersServiceProxy.Delete(aOrganizationId, aMemberId : String) : TOrganizationMemberDeleteResponseServiceResult;

const
  lMethodURL = '/v1/organizations/';
  lMembersSuffix = '/members/';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TOrganizationMemberDeleteResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL + aOrganizationId + lMembersSuffix + aMemberId);
  lResponse:=ExecuteRequest('delete',lURL,'');
  Result:=TOrganizationMemberDeleteResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TOrganizationMemberDeleteResponse.Deserialize(lResponse.Content);
end;

Function TOrganizationInvitesServiceProxy.CreateInvite(aOrganizationId : String; aBody : TOrganizationInviteCreateRequest) : TOrganizationInviteServiceResult;

const
  lMethodURL = '/v1/organizations/';
  lInvitesSuffix = '/invites';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TOrganizationInviteServiceResult);
  lURL:=BuildEndPointURL(lMethodURL + aOrganizationId + lInvitesSuffix);
  lResponse:=ExecuteRequest('post',lURL,aBody.Serialize);
  Result:=TOrganizationInviteServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TOrganizationInvite.Deserialize(lResponse.Content);
end;

Function TOrganizationInvitesServiceProxy.List(aOrganizationId : String) : TOrganizationInvitesResponseServiceResult;

const
  lMethodURL = '/v1/organizations/';
  lInvitesSuffix = '/invites';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TOrganizationInvitesResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL + aOrganizationId + lInvitesSuffix);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TOrganizationInvitesResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TOrganizationInvitesResponse.Deserialize(lResponse.Content);
end;

Function TOrganizationInvitesServiceProxy.Get(aOrganizationId, aInviteId : String) : TOrganizationInviteServiceResult;

const
  lMethodURL = '/v1/organizations/';
  lInvitesSuffix = '/invites/';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TOrganizationInviteServiceResult);
  lURL:=BuildEndPointURL(lMethodURL + aOrganizationId + lInvitesSuffix + aInviteId);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TOrganizationInviteServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TOrganizationInvite.Deserialize(lResponse.Content);
end;

Function TOrganizationInvitesServiceProxy.Delete(aOrganizationId, aInviteId : String) : TOrganizationInviteDeleteResponseServiceResult;

const
  lMethodURL = '/v1/organizations/';
  lInvitesSuffix = '/invites/';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TOrganizationInviteDeleteResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL + aOrganizationId + lInvitesSuffix + aInviteId);
  lResponse:=ExecuteRequest('delete',lURL,'');
  Result:=TOrganizationInviteDeleteResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TOrganizationInviteDeleteResponse.Deserialize(lResponse.Content);
end;

Function TWorkspacesServiceProxy.List(aOrganizationId : String) : TWorkspacesResponseServiceResult;

const
  lMethodURL = '/v1/organizations/';
  lWorkspacesSuffix = '/workspaces';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TWorkspacesResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL + aOrganizationId + lWorkspacesSuffix);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TWorkspacesResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TWorkspacesResponse.Deserialize(lResponse.Content);
end;

Function TWorkspacesServiceProxy.CreateWorkspace(aOrganizationId : String; aBody : TWorkspaceCreateRequest) : TWorkspaceServiceResult;

const
  lMethodURL = '/v1/organizations/';
  lWorkspacesSuffix = '/workspaces';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TWorkspaceServiceResult);
  lURL:=BuildEndPointURL(lMethodURL + aOrganizationId + lWorkspacesSuffix);
  lResponse:=ExecuteRequest('post',lURL,aBody.Serialize);
  Result:=TWorkspaceServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TWorkspace.Deserialize(lResponse.Content);
end;

Function TWorkspacesServiceProxy.Get(aOrganizationId, aWorkspaceId : String) : TWorkspaceServiceResult;

const
  lMethodURL = '/v1/organizations/';
  lWorkspacesSuffix = '/workspaces/';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TWorkspaceServiceResult);
  lURL:=BuildEndPointURL(lMethodURL + aOrganizationId + lWorkspacesSuffix + aWorkspaceId);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TWorkspaceServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TWorkspace.Deserialize(lResponse.Content);
end;

Function TWorkspacesServiceProxy.Update(aOrganizationId, aWorkspaceId : String; aBody : TWorkspaceUpdateRequest) : TWorkspaceServiceResult;

const
  lMethodURL = '/v1/organizations/';
  lWorkspacesSuffix = '/workspaces/';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TWorkspaceServiceResult);
  lURL:=BuildEndPointURL(lMethodURL + aOrganizationId + lWorkspacesSuffix + aWorkspaceId);
  lResponse:=ExecuteRequest('patch',lURL,aBody.Serialize);
  Result:=TWorkspaceServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TWorkspace.Deserialize(lResponse.Content);
end;

Function TWorkspacesServiceProxy.Archive(aOrganizationId, aWorkspaceId : String) : TWorkspaceServiceResult;

const
  lMethodURL = '/v1/organizations/';
  lWorkspacesSuffix = '/workspaces/';
  lArchiveSuffix = '/archive';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TWorkspaceServiceResult);
  lURL:=BuildEndPointURL(lMethodURL + aOrganizationId + lWorkspacesSuffix + aWorkspaceId + lArchiveSuffix);
  lResponse:=ExecuteRequest('post',lURL,'');
  Result:=TWorkspaceServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TWorkspace.Deserialize(lResponse.Content);
end;

Function TWorkspaceMembersServiceProxy.List(aOrganizationId, aWorkspaceId : String) : TWorkspaceMembersResponseServiceResult;

const
  lMethodURL = '/v1/organizations/';
  lWorkspacesSuffix = '/workspaces/';
  lMembersSuffix = '/members';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TWorkspaceMembersResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL + aOrganizationId + lWorkspacesSuffix + aWorkspaceId + lMembersSuffix);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TWorkspaceMembersResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TWorkspaceMembersResponse.Deserialize(lResponse.Content);
end;

Function TWorkspaceMembersServiceProxy.Add(aOrganizationId, aWorkspaceId : String; aBody : TWorkspaceMemberAddRequest) : TWorkspaceMemberServiceResult;

const
  lMethodURL = '/v1/organizations/';
  lWorkspacesSuffix = '/workspaces/';
  lMembersSuffix = '/members';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TWorkspaceMemberServiceResult);
  lURL:=BuildEndPointURL(lMethodURL + aOrganizationId + lWorkspacesSuffix + aWorkspaceId + lMembersSuffix);
  lResponse:=ExecuteRequest('post',lURL,aBody.Serialize);
  Result:=TWorkspaceMemberServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TWorkspaceMember.Deserialize(lResponse.Content);
end;

Function TWorkspaceMembersServiceProxy.Update(aOrganizationId, aWorkspaceId, aMemberId : String; aBody : TWorkspaceMemberUpdateRequest) : TWorkspaceMemberServiceResult;

const
  lMethodURL = '/v1/organizations/';
  lWorkspacesSuffix = '/workspaces/';
  lMembersSuffix = '/members/';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TWorkspaceMemberServiceResult);
  lURL:=BuildEndPointURL(lMethodURL + aOrganizationId + lWorkspacesSuffix + aWorkspaceId + lMembersSuffix + aMemberId);
  lResponse:=ExecuteRequest('patch',lURL,aBody.Serialize);
  Result:=TWorkspaceMemberServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TWorkspaceMember.Deserialize(lResponse.Content);
end;

Function TWorkspaceMembersServiceProxy.Remove(aOrganizationId, aWorkspaceId, aMemberId : String) : TWorkspaceMemberDeleteResponseServiceResult;

const
  lMethodURL = '/v1/organizations/';
  lWorkspacesSuffix = '/workspaces/';
  lMembersSuffix = '/members/';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TWorkspaceMemberDeleteResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL + aOrganizationId + lWorkspacesSuffix + aWorkspaceId + lMembersSuffix + aMemberId);
  lResponse:=ExecuteRequest('delete',lURL,'');
  Result:=TWorkspaceMemberDeleteResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TWorkspaceMemberDeleteResponse.Deserialize(lResponse.Content);
end;

Function TApiKeysServiceProxy.List(aOrganizationId : String) : TApiKeysResponseServiceResult;

const
  lMethodURL = '/v1/organizations/';
  lApiKeysSuffix = '/api_keys';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TApiKeysResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL + aOrganizationId + lApiKeysSuffix);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TApiKeysResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TApiKeysResponse.Deserialize(lResponse.Content);
end;

Function TApiKeysServiceProxy.CreateKey(aOrganizationId : String; aBody : TApiKeyCreateRequest) : TApiKeyServiceResult;

const
  lMethodURL = '/v1/organizations/';
  lApiKeysSuffix = '/api_keys';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TApiKeyServiceResult);
  lURL:=BuildEndPointURL(lMethodURL + aOrganizationId + lApiKeysSuffix);
  lResponse:=ExecuteRequest('post',lURL,aBody.Serialize);
  Result:=TApiKeyServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TApiKey.Deserialize(lResponse.Content);
end;

Function TApiKeysServiceProxy.Get(aOrganizationId, aApiKeyId : String) : TApiKeyServiceResult;

const
  lMethodURL = '/v1/organizations/';
  lApiKeysSuffix = '/api_keys/';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TApiKeyServiceResult);
  lURL:=BuildEndPointURL(lMethodURL + aOrganizationId + lApiKeysSuffix + aApiKeyId);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TApiKeyServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TApiKey.Deserialize(lResponse.Content);
end;

Function TApiKeysServiceProxy.Delete(aOrganizationId, aApiKeyId : String) : TApiKeyDeleteResponseServiceResult;

const
  lMethodURL = '/v1/organizations/';
  lApiKeysSuffix = '/api_keys/';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TApiKeyDeleteResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL + aOrganizationId + lApiKeysSuffix + aApiKeyId);
  lResponse:=ExecuteRequest('delete',lURL,'');
  Result:=TApiKeyDeleteResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TApiKeyDeleteResponse.Deserialize(lResponse.Content);
end;

Function TUsageCostServiceProxy.GetCostReport(aBody : TCostReportRequest) : TCostReportResponseServiceResult;

const
  lMethodURL = '/v1/organizations/cost_report';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TCostReportResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('get',lURL,aBody.Serialize);
  Result:=TCostReportResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TCostReportResponse.Deserialize(lResponse.Content);
end;

Function TUsageCostServiceProxy.GetUsageReport(aBody : TUsageReportRequest) : TUsageReportResponseServiceResult;

const
  lMethodURL = '/v1/organizations/usage_report/messages';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TUsageReportResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('get',lURL,aBody.Serialize);
  Result:=TUsageReportResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TUsageReportResponse.Deserialize(lResponse.Content);
end;

Function TUsageCostServiceProxy.GetClaudeCodeAnalytics(aBody : TClaudeCodeAnalyticsRequest) : TClaudeCodeAnalyticsResponseServiceResult;

const
  lMethodURL = '/v1/organizations/usage_report/claude_code';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TClaudeCodeAnalyticsResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('get',lURL,aBody.Serialize);
  Result:=TClaudeCodeAnalyticsResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TClaudeCodeAnalyticsResponse.Deserialize(lResponse.Content);
end;


end.
