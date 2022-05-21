{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019 by Michael Van Canneyt, member of the
    Free Pascal development team

    Simple SQLDBRESTBridge JSON dataset component and connection stubs.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit stub.restdataset;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, fpJSONDataset;

Type

  { TSQLDBRestConnection }
  // We don't need the actual objects in the IDE.

  TDataRequest = Class(TObject);
  TRecordUpdateDescriptor = Class(TObject);

  TRestGetURLEvent = Procedure (Sender : TComponent; aRequest : TDataRequest; Var aURL : String) of Object;
  TRestUpdateURLEvent = Procedure (Sender : TComponent; aRequest : TRecordUpdateDescriptor; Var aURL : String) of Object;

  TSQLDBRestConnection = Class(TComponent)
  private
    FBaseURL: String;
    FConnectionsResourceName: String;
    FCustomViewResourceName: String;
    FDataProperty: String;
    FmetaDataProperty: String;
    FMetaDataResourceName: String;
    FonGetResources: TNotifyEvent;
    FOnGetURL: TRestGetURLEvent;
    FOnUpdateURL: TRestUpdateURLEvent;
    FPageParam: String;
    FPassword: String;
    FUserName: String;
    function DoStoreDataProp: Boolean;
    function DoStoreMetadata: Boolean;
    function DoStoreMetadataProp: Boolean;
  Public
    Constructor Create(AOwner: TComponent); override;
  Published
    // Property name used to indicate metadata
    Property metaDataProperty : String read FmetaDataProperty Write FmetaDataProperty Stored DoStoreMetadataProp;
    // Property name for data array
    Property DataProperty : String read FDataProperty Write FDataProperty Stored DoStoreDataProp;
    // Name of the MetaData resource
    Property MetaDataResourceName : String Read FMetaDataResourceName Write FMetaDataResourceName Stored DoStoreMetadata;
    // Username for Basic authentication
    Property UserName : String Read FUserName Write FUserName;
    // Password for Basic authentication
    Property Password : String Read FPassword Write FPassword;
    // Name of the Connections resource
    Property ConnectionsResourceName : String Read FConnectionsResourceName Write FConnectionsResourceName;
    // Name of the CustomView resource
    Property CustomViewResourceName : String Read FCustomViewResourceName Write FCustomViewResourceName;
    // Name of Callback when resources are retrieved
    Property OnGetResources : TNotifyEvent Read FonGetResources Write FOnGetResources;
    // Base URL for the REST resources
    Property BaseURL : String Read FBaseURL Write FBaseURL;
    // Name of the paging parameter
    Property PageParam : String Read FPageParam Write FPageParam;
    // callback to get the REST GET url
    Property OnGetURL : TRestGetURLEvent Read FOnGetURL Write FOnGetURL;
    // callback to get the REST Update/Post url
    Property OnUpdateURL : TRestUpdateURLEvent Read FOnUpdateURL Write FOnUpdateURL;
  end;


  { TQueryParam }

  TQueryParam = class(TParam)
  private
    FEnabled: Boolean;
  Public
    Procedure Assign(Source : TPersistent); override;
  Published
    Property Enabled : Boolean Read FEnabled Write FEnabled;
  end;

  { TQueryParams }

  TQueryParams = Class(TParams)
  private
    function GetP(aIndex : Integer): TQueryParam;
    procedure SetP(aIndex : Integer; AValue: TQueryParam);
  Public
    Property Params[aIndex : Integer] : TQueryParam Read GetP Write SetP; default;
  end;

  TGetQueryParamsEvent = Procedure (Sender : TDataset; IsReadURL : Boolean; var QueryString : String) of object;

  { TSQLDBRestDataset }

  TSQLDBRestDataset = Class(TJSONDataset)
  private
    FConnection: TSQLDBRestConnection;
    FDatabaseConnection: String;
    FOnGetQueryParams: TGetQueryParamsEvent;
    FParams: TQueryParams;
    FResourceName: String;
    FSQL: TStrings;
    function CreateQueryParams: TQueryParams;
    procedure SetConnection(AValue: TSQLDBRestConnection);
    procedure SetParams(AValue: TQueryParams);
    procedure SetSQL(AValue: TStrings);
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
  Published
    // Connection to use
    Property Connection: TSQLDBRestConnection Read FConnection Write SetConnection;
    // Resource we want to get
    Property ResourceName : String Read FResourceName Write FResourceName;
    // SQL to send when using the CustomView resource
    Property SQL : TStrings Read FSQL Write SetSQL;
    // Database connection name. Will be prepended to REST resource name.
    property DatabaseConnection : String Read FDatabaseConnection Write FDatabaseConnection;
    // Parameters to be used in request
    Property Params : TQueryParams Read FParams Write SetParams;
    // Event to adapt parameters when issuing a request
    Property OnGetQueryParams : TGetQueryParamsEvent Read FOnGetQueryParams Write FOnGetQueryParams;
  end;


implementation

Const
  DataProp = 'data';
  MetaDataProp = 'metaData';
  MetaDataResource = 'metadata';

{ TQueryParams }

function TQueryParams.GetP(aIndex: Integer): TQueryParam;
begin
  Result:=TQueryParam(Items[aIndex]);
end;

procedure TQueryParams.SetP(aIndex: Integer; AValue: TQueryParam);
begin
  Items[aIndex]:=aValue;
end;

{ TQueryParam }

procedure TQueryParam.Assign(Source: TPersistent);
Var
  P : TQueryParam absolute Source;

begin
  if Source is TQueryParam then
    begin
    FEnabled:=P.Enabled;
    end;
  inherited Assign(Source);
end;

{ TSQLDBRestDataset }

procedure TSQLDBRestDataset.SetConnection(AValue: TSQLDBRestConnection);
begin
  if FConnection=AValue then Exit;
  if Assigned(FConnection) then
    FConnection.RemoveFreeNotification(Self);
  FConnection:=AValue;
  if Assigned(FConnection) then
    FConnection.FreeNotification(Self);
end;

procedure TSQLDBRestDataset.SetParams(AValue: TQueryParams);
begin
  if FParams=AValue then Exit;
  FParams.Assign(AValue);
end;


procedure TSQLDBRestDataset.SetSQL(AValue: TStrings);
begin
  if FSQL=AValue then Exit;
  FSQL.Assign(AValue);
end;

constructor TSQLDBRestDataset.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FSQL:=TStringList.Create;
  FParams:=CreateQueryParams;
end;

function TSQLDBRestDataset.CreateQueryParams: TQueryParams;
begin
  Result:=TQueryParams.Create(Self,TQueryParam);
end;


destructor TSQLDBRestDataset.Destroy;
begin
  FreeAndNil(FParams);
  FreeAndNil(FSQL);
  inherited Destroy;
end;

{ TSQLDBRestConnection }

function TSQLDBRestConnection.DoStoreDataProp: Boolean;
begin
  Result:=FDataProperty<>DataProp;
end;

function TSQLDBRestConnection.DoStoreMetadata: Boolean;
begin
  Result:=FmetaDataProperty<>MetaDataProp;
end;

function TSQLDBRestConnection.DoStoreMetadataProp: Boolean;
begin
  Result:=FMetaDataResourceName<>MetaDataResource;
end;

constructor TSQLDBRestConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMetaDataResourceName:=MetaDataResource;
  FmetaDataProperty:=MetaDataProp;
  FDataProperty:=DataProp;
end;

end.

