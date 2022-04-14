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
  Classes, SysUtils, DB, fpJSONDataset;

Type

  { TSQLDBRestConnection }
  // We don't need the actual objects in the IDE.

  TDataRequest = Class(TObject);
  TRecordUpdateDescriptor = Class(TObject);

  TRestGetURLEvent = Procedure (Sender : TComponent; aRequest : TDataRequest; Var aURL : String) of Object;
  TRestUpdateURLEvent = Procedure (Sender : TComponent; aRequest : TRecordUpdateDescriptor; Var aURL : String) of Object;

  TSQLDBRestConnection = Class(TComponent)
  private
    FConnectionsResourceName: String;
    FCustomViewResourceName: String;
    FDataProperty: String;
    FmetaDataProperty: String;
    FMetaDataResourceName: String;
    FonGetResources: TNotifyEvent;
    FPassword: String;
    FUserName: String;
    function DoStoreDataProp: Boolean;
    function DoStoreMetadata: Boolean;
    function DoStoreMetadataProp: Boolean;
  Public
    Constructor Create(AOwner: TComponent); override;
  Published
    Property metaDataProperty : String read FmetaDataProperty Write FmetaDataProperty Stored DoStoreMetadataProp;
    Property DataProperty : String read FDataProperty Write FDataProperty Stored DoStoreDataProp;
    Property MetaDataResourceName : String Read FMetaDataResourceName Write FMetaDataResourceName Stored DoStoreMetadata;
    Property UserName : String Read FUserName Write FUserName;
    Property Password : String Read FPassword Write FPassword;
    Property ConnectionsResourceName : String Read FConnectionsResourceName Write FConnectionsResourceName;
    Property CustomViewResourceName : String Read FCustomViewResourceName Write FCustomViewResourceName;
    Property OnGetResources : TNotifyEvent Read FonGetResources Write FOnGetResources;
  end;

  { TSQLDBRestDataset }

  TSQLDBRestDataset = Class(TJSONDataset)
  private
    FConnection: TSQLDBRestConnection;
    FDatabaseConnection: String;
    FResourceName: String;
    FSQL: TStrings;
    procedure SetConnection(AValue: TSQLDBRestConnection);
    procedure SetSQL(AValue: TStrings);
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
  Published

    Property Connection: TSQLDBRestConnection Read FConnection Write SetConnection;
    Property ResourceName : String Read FResourceName Write FResourceName;
    Property SQL : TStrings Read FSQL Write SetSQL;
    property DatabaseConnection : String Read FDatabaseConnection Write FDatabaseConnection;
  end;


implementation

Const
  DataProp = 'data';
  MetaDataProp = 'metaData';
  MetaDataResource = 'metadata';

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


procedure TSQLDBRestDataset.SetSQL(AValue: TStrings);
begin
  if FSQL=AValue then Exit;
  FSQL.Assign(AValue);
end;

constructor TSQLDBRestDataset.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FSQL:=TStringList.Create;
end;

destructor TSQLDBRestDataset.Destroy;
begin
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

