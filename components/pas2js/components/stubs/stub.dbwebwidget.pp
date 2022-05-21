{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019-Now by Michael Van Canneyt, member of the
    Free Pascal development team

    WEB Widget Set - DB-Aware widget stubs

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit stub.dbwebwidget;


{$mode objfpc}

interface

uses
  Sysutils, Classes, stub.webwidget, db;

Type

  { TCustomDBLoopTemplateWidget }
  TDBFieldValueData = Class(TLoopTemplateValue)
    Field : TField;
  end;
  TGetFieldValueEvent = Procedure (Sender : TObject; aData : TDBFieldValueData) of object;

  { TDBLoopTemplateGroup }

  TDBLoopTemplateGroup = Class(TLoopTemplateGroup)
  private
    FFieldList: String;
  Public
    Procedure Assign(Source : TPersistent); override;
  Published
    Property FieldList : String Read FFieldList Write FFieldList;
  end;

  TDBLoopTemplateValueEvent = Procedure (Sender : TObject; aDataset : TDataset; aValue : TLoopTemplateValue) of object;
  TLoopDatalink = class(TDataLink);

  TCustomDBLoopTemplateWidget = Class(TCustomLoopTemplateWidget)
  private
    FLink : TLoopDatalink;
    FOnFormatField: TGetFieldValueEvent;
    FOnGetGroupValue: TDBLoopTemplateValueEvent;
    function GetDataset: TDataset;
    function GetDatasource: TDatasource;
    procedure SetDatasource(AValue: TDatasource);
  Protected
    Class Function CreateGroups(aOwner : TComponent) : TLoopTemplateGroupList; override;
  Protected
    Property Datasource : TDatasource Read GetDatasource Write SetDatasource;
    Property OnFormatField : TGetFieldValueEvent Read FOnFormatField Write FOnFormatField;
  Public
    Constructor create(aOwner : TComponent); override;
    Destructor Destroy; override;
    Property Dataset : TDataset Read GetDataset;
    Property OnGetGroupValue : TDBLoopTemplateValueEvent Read FOnGetGroupValue Write FOnGetGroupValue;
  end;


  TDBLoopTemplateWidget = Class(TCustomDBLoopTemplateWidget)
  Published
    Property Groups;
    Property HeaderTemplate;
    Property ItemTemplate;
    Property FooterTemplate;
    Property OnGetValue;
    Property References;
    Property Datasource;
    Property OnFormatField;
    Property OnGetGroupValue;
  end;

implementation

{ TCustomDBLoopTemplateWidget }

function TCustomDBLoopTemplateWidget.GetDataset: TDataset;
begin
  if Assigned(Datasource) then
    Result:=Datasource.Dataset
  else
    Result:=Nil;
end;

function TCustomDBLoopTemplateWidget.GetDatasource: TDatasource;
begin
  Result:=FLink.Datasource;
end;

procedure TCustomDBLoopTemplateWidget.SetDatasource(AValue: TDatasource);
begin
  FLink.Datasource:=aValue;
end;

class function TCustomDBLoopTemplateWidget.CreateGroups(aOwner: TComponent
  ): TLoopTemplateGroupList;
begin
  Result:=TLoopTemplateGroupList.Create(aOwner,TDBLoopTemplateGroup);
end;


constructor TCustomDBLoopTemplateWidget.create(aOwner: TComponent);
begin
  inherited create(aOwner);
  FLink:=TLoopDataLink.Create;
end;

destructor TCustomDBLoopTemplateWidget.Destroy;
begin
  FreeAndNil(FLink);
  inherited Destroy;
end;

{ TDBLoopTemplateGroup }


procedure TDBLoopTemplateGroup.Assign(Source: TPersistent);

Var
  G : TDBLoopTemplateGroup absolute Source;

begin
  if Source is TDBLoopTemplateGroup then
    FieldList:=G.FieldList;
  inherited Assign(Source);
end;

end.

