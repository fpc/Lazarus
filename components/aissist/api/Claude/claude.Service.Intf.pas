{
    This file is part of the Free Component Library
    Copyright (c) 2025 by Michael Van Canneyt michael@freepascal.org

    Claude Rest API - Service interface definitions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit claude.Service.Intf;

{$mode objfpc}
{$h+}

interface

uses
   fpopenapiclient, claude.Dto;

Type
  // Service result types
  TCompleteResponseServiceResult = specialize TServiceResult<TCompleteResponse>;
  TModelsResponseServiceResult = specialize TServiceResult<TModelsResponse>;
  TModelServiceResult = specialize TServiceResult<TModel>;
  TFileServiceResult = specialize TServiceResult<TFile>;
  TFilesResponseServiceResult = specialize TServiceResult<TFilesResponse>;
  TFileDeleteResponseServiceResult = specialize TServiceResult<TFileDeleteResponse>;
  TMessageResponseServiceResult = specialize TServiceResult<TMessageResponse>;
  TMessageBatchServiceResult = specialize TServiceResult<TMessageBatch>;
  TMessageBatchesResponseServiceResult = specialize TServiceResult<TMessageBatchesResponse>;
  TOrganizationServiceResult = specialize TServiceResult<TOrganization>;
  TOrganizationMemberServiceResult = specialize TServiceResult<TOrganizationMember>;
  TOrganizationMembersResponseServiceResult = specialize TServiceResult<TOrganizationMembersResponse>;
  TOrganizationMemberDeleteResponseServiceResult = specialize TServiceResult<TOrganizationMemberDeleteResponse>;
  TOrganizationInviteServiceResult = specialize TServiceResult<TOrganizationInvite>;
  TOrganizationInvitesResponseServiceResult = specialize TServiceResult<TOrganizationInvitesResponse>;
  TOrganizationInviteDeleteResponseServiceResult = specialize TServiceResult<TOrganizationInviteDeleteResponse>;
  TWorkspaceServiceResult = specialize TServiceResult<TWorkspace>;
  TWorkspacesResponseServiceResult = specialize TServiceResult<TWorkspacesResponse>;
  TWorkspaceMemberServiceResult = specialize TServiceResult<TWorkspaceMember>;
  TWorkspaceMembersResponseServiceResult = specialize TServiceResult<TWorkspaceMembersResponse>;
  TWorkspaceMemberDeleteResponseServiceResult = specialize TServiceResult<TWorkspaceMemberDeleteResponse>;
  TApiKeyServiceResult = specialize TServiceResult<TApiKey>;
  TApiKeysResponseServiceResult = specialize TServiceResult<TApiKeysResponse>;
  TApiKeyDeleteResponseServiceResult = specialize TServiceResult<TApiKeyDeleteResponse>;
  TCostReportResponseServiceResult = specialize TServiceResult<TCostReportResponse>;
  TUsageReportResponseServiceResult = specialize TServiceResult<TUsageReportResponse>;
  TClaudeCodeAnalyticsResponseServiceResult = specialize TServiceResult<TClaudeCodeAnalyticsResponse>;

  // Service ICompleteService

  ICompleteService = interface  ['{301B9D3B-1C09-4E56-8E5C-66864EF98545}']
    Function Post(aBody : TCompleteRequest) : TCompleteResponseServiceResult;
  end;

  // Service IModelsService

  IModelsService = interface  ['{4F2A8E1D-5B3C-4A67-9E8F-12345678ABCD}']
    Function List : TModelsResponseServiceResult;
    Function Get(aModelId : String) : TModelServiceResult;
  end;

  // Service IFilesService

  IFilesService = interface  ['{8A7B6C5D-4E3F-2A1B-9C8D-7E6F5A4B3C2D}']
    Function Upload(aBody : TFileUploadRequest) : TFileServiceResult;
    Function List : TFilesResponseServiceResult;
    Function Get(aFileId : String) : TFileServiceResult;
    Function Delete(aFileId : String) : TFileDeleteResponseServiceResult;
  end;

  // Service IMessagesService

  IMessagesService = interface  ['{1A2B3C4D-5E6F-7A8B-9C0D-1E2F3A4B5C6F}']
    Function CreateMessage(aBody : TMessageRequest) : TMessageResponseServiceResult;
  end;

  // Service IMessageBatchesService

  IMessageBatchesService = interface  ['{9D8C7B6A-5E4F-3A2B-1C9D-8E7F6A5B4C3D}']
    Function CreateBatch(aBody : TMessageBatchCreateRequest) : TMessageBatchServiceResult;
    Function List : TMessageBatchesResponseServiceResult;
    Function Get(aBatchId : String) : TMessageBatchServiceResult;
    Function Cancel(aBatchId : String) : TMessageBatchServiceResult;
  end;

  // Service IOrganizationsService

  IOrganizationsService = interface  ['{A1B2C3D4-5E6F-7A8B-9C0D-1E2F3A4B5C6D}']
    Function Get(aOrganizationId : String) : TOrganizationServiceResult;
  end;

  // Service IOrganizationMembersService

  IOrganizationMembersService = interface  ['{B2C3D4E5-6F7A-8B9C-0D1E-2F3A4B5C6D7E}']
    Function List(aOrganizationId : String) : TOrganizationMembersResponseServiceResult;
    Function Get(aOrganizationId, aMemberId : String) : TOrganizationMemberServiceResult;
    Function Update(aOrganizationId, aMemberId : String; aBody : TOrganizationMemberUpdateRequest) : TOrganizationMemberServiceResult;
    Function Delete(aOrganizationId, aMemberId : String) : TOrganizationMemberDeleteResponseServiceResult;
  end;

  // Service IOrganizationInvitesService

  IOrganizationInvitesService = interface  ['{C3D4E5F6-7A8B-9C0D-1E2F-3A4B5C6D7E8F}']
    Function CreateInvite(aOrganizationId : String; aBody : TOrganizationInviteCreateRequest) : TOrganizationInviteServiceResult;
    Function List(aOrganizationId : String) : TOrganizationInvitesResponseServiceResult;
    Function Get(aOrganizationId, aInviteId : String) : TOrganizationInviteServiceResult;
    Function Delete(aOrganizationId, aInviteId : String) : TOrganizationInviteDeleteResponseServiceResult;
  end;

  // Service IWorkspacesService

  IWorkspacesService = interface  ['{D4E5F6A7-8B9C-0D1E-2F3A-4B5C6D7E8F9A}']
    Function List(aOrganizationId : String) : TWorkspacesResponseServiceResult;
    Function CreateWorkspace(aOrganizationId : String; aBody : TWorkspaceCreateRequest) : TWorkspaceServiceResult;
    Function Get(aOrganizationId, aWorkspaceId : String) : TWorkspaceServiceResult;
    Function Update(aOrganizationId, aWorkspaceId : String; aBody : TWorkspaceUpdateRequest) : TWorkspaceServiceResult;
    Function Archive(aOrganizationId, aWorkspaceId : String) : TWorkspaceServiceResult;
  end;

  // Service IWorkspaceMembersService

  IWorkspaceMembersService = interface  ['{E5F6A7B8-9C0D-1E2F-3A4B-5C6D7E8F9A0B}']
    Function List(aOrganizationId, aWorkspaceId : String) : TWorkspaceMembersResponseServiceResult;
    Function Add(aOrganizationId, aWorkspaceId : String; aBody : TWorkspaceMemberAddRequest) : TWorkspaceMemberServiceResult;
    Function Update(aOrganizationId, aWorkspaceId, aMemberId : String; aBody : TWorkspaceMemberUpdateRequest) : TWorkspaceMemberServiceResult;
    Function Remove(aOrganizationId, aWorkspaceId, aMemberId : String) : TWorkspaceMemberDeleteResponseServiceResult;
  end;

  // Service IApiKeysService

  IApiKeysService = interface  ['{F6A7B8C9-0D1E-2F3A-4B5C-6D7E8F9A0B1C}']
    Function List(aOrganizationId : String) : TApiKeysResponseServiceResult;
    Function CreateKey(aOrganizationId : String; aBody : TApiKeyCreateRequest) : TApiKeyServiceResult;
    Function Get(aOrganizationId, aApiKeyId : String) : TApiKeyServiceResult;
    Function Delete(aOrganizationId, aApiKeyId : String) : TApiKeyDeleteResponseServiceResult;
  end;

  // Service IUsageCostService

  IUsageCostService = interface  ['{A1B2C3D4-5E6F-7A8B-9C0D-1E2F3A4B5C6E}']
    Function GetCostReport(aBody : TCostReportRequest) : TCostReportResponseServiceResult;
    Function GetUsageReport(aBody : TUsageReportRequest) : TUsageReportResponseServiceResult;
    Function GetClaudeCodeAnalytics(aBody : TClaudeCodeAnalyticsRequest) : TClaudeCodeAnalyticsResponseServiceResult;
  end;


implementation

end.
