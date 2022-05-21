{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019-Now by Michael Van Canneyt, member of the
    Free Pascal development team

    WEB Widget Set : DB-AWare bare HTML Widgets

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit stub.dbhtmlwidgets;

{$mode objfpc}
{$h+}

interface

uses
  Classes, SysUtils, db, stub.htmlwidgets;

Type

  { TCustomDBTableWidget }

  { TDBTableColumn }

  TDBTableColumn = class(TCustomTableColumn)
  private
    FFieldName: String;
    FTemplate: String;
    procedure SetFieldName(AValue: String);
  Public
    Procedure Assign(Source : TPersistent); override;
  Published
    Property FieldName : String Read FFieldName Write SetFieldName;
    Property Template : String Read FTemplate Write FTemplate;
  end;

  { TDBTableColumns }

  TDBTableColumns = Class(TCustomTableColumns)
  private
    function GetCol(Index : Integer): TDBTableColumn;
    procedure SetCol(Index : Integer; AValue: TDBTableColumn);
  Public
    Function AddField(F : TField) : TDBTableColumn;
    Function AddField(const AFieldName,aCaption : String) : TDBTableColumn;
    Property DBColumns[Index : Integer] : TDBTableColumn Read GetCol Write SetCol; default;
  end;

   { TDBTableRowEnumerator }

  TCustomDBTableWidget = Class(TCustomTableWidget)
  private
    FDatasource: TDatasource;
    FRowKeyField: String;
    function GetColumns: TDBTableColumns;
    procedure SetColumns(AValue: TDBTableColumns);
    procedure SetDatasource(AValue: TDatasource);
    procedure SetRowKeyField(AValue: String);
  Protected
    function CreateColumns: TCustomTableColumns; override;
    Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetDataset: TDataset;
    Property Datasource : TDatasource Read FDatasource write SetDatasource;
    Property RowKeyField : String Read FRowKeyField Write SetRowKeyField;
  Public
    Property Dataset : TDataset Read GetDataset;
    Property Columns : TDBTableColumns Read GetColumns Write SetColumns;
  end;

  TDBTableWidget = class(TCustomDBTableWidget)
  Published
    Property Classes;
    Property TableOptions;
    Property ParentID ;
    Property Datasource;
    Property Columns;
    Property OnGetCellData;
    Property OnCellClick;
    Property OnHeaderCellClick;
    Property OnFooterCellClick;
    Property OnRowClick;
    Property OnHeaderRowClick;
    Property OnFooterRowClick;
    Property RowKeyField;
  end;

  // Select that gets the values from a dataset.

  TCustomDBSelectWidget = class;
  
  { TCustomDBSelectWidget }
  TSelectLink = class(TDatalink)
  Private
    FWidget : TCustomDBSelectWidget;
  Public
    Constructor Create(aWidget : TCustomDBSelectWidget);
    Property Widget: TCustomDBSelectWidget Read FWidget;
  end;
  
  TCustomDBSelectWidget = class(TCustomSelectWidget)
  Private
    FLink : TSelectLink;
    FItemField: String;
    FNullIsNotValue: Boolean;
    FValue: string;
    FValueField: String;
    procedure SetDatasource(AValue: TDatasource);
    function GetDatasource: TDatasource;
    procedure SetItemField(AValue: String);
    procedure SetNullIsNotValue(AValue: Boolean);
    procedure SetValueField(AValue: String);
  Protected
    Property Datasource : TDatasource Read GetDatasource write SetDatasource;
    Property ItemField : String Read FItemField Write SetItemField;
    Property ValueField : String Read FValueField Write SetValueField;
    Property NullIsNotValue : Boolean Read FNullIsNotValue Write SetNullIsNotValue;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    Property Value : string read FValue Write FValue;
  end;

  { TDBSelectWidget }

  TDBSelectWidget = Class(TCustomDBSelectWidget)
  Published
    Property Datasource;
    Property ItemField;
    Property ValueField;
    Property NullIsNotValue;
    property SelectedIndex;
    Property Multiple;
    Property Value;
  end;


implementation

{ TSelectLink }

Constructor TSelectLink.Create(aWidget : TCustomDBSelectWidget);

begin
  FWidget:=aWidget;
  Inherited create;
end;

{ TCustomDBSelectWidget }


function TCustomDBSelectWidget.GetDatasource: TDatasource;
begin
  Result:=FLink.Datasource;
end;

procedure TCustomDBSelectWidget.SetDatasource(AValue: TDatasource);
begin
  FLink.Datasource:=aValue;
end;


procedure TCustomDBSelectWidget.SetItemField(AValue: String);
begin
  if FItemField=AValue then Exit;
  FItemField:=AValue;
end;

procedure TCustomDBSelectWidget.SetNullIsNotValue(AValue: Boolean);
begin
  if FNullIsNotValue=AValue then Exit;
  FNullIsNotValue:=AValue;
end;

procedure TCustomDBSelectWidget.SetValueField(AValue: String);
begin
  if FValueField=AValue then Exit;
  FValueField:=AValue;
end;

constructor TCustomDBSelectWidget.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FLink:=TSelectLink.Create(Self);
end;

destructor TCustomDBSelectWidget.Destroy;
begin
  FreeAndNil(FLink);
  inherited Destroy;
end;

{ TDBTableColumn }

procedure TDBTableColumn.SetFieldName(AValue: String);
begin
  if FFieldName=AValue then Exit;
  FFieldName:=AValue;
end;



procedure TDBTableColumn.Assign(Source: TPersistent);
begin
  if Source is TDBTableColumn then
    FieldName:=TDBTableColumn(Source).FieldName;
  inherited Assign(Source);
end;

{ TDBTableColumns }

function TDBTableColumns.GetCol(Index : Integer): TDBTableColumn;
begin
  Result:=TDBTableColumn(Items[Index])
end;

procedure TDBTableColumns.SetCol(Index : Integer; AValue: TDBTableColumn);
begin
  Items[Index]:=AValue;
end;

function TDBTableColumns.AddField(F: TField): TDBTableColumn;
begin
  Result:=AddField(F.FieldName,F.DisplayLabel);
end;

function TDBTableColumns.AddField(const AFieldName, aCaption : String): TDBTableColumn;
begin
  Result:=(Add as TDBtableColumn);
  Result.FieldName:=aFieldName;
  Result.Caption:=aCaption;
end;
{ TCustomDBTableWidget }

procedure TCustomDBTableWidget.SetDatasource(AValue: TDatasource);
begin
  if FDatasource=AValue then Exit;
  if Assigned(FDatasource) then
    FDatasource.RemoveFreeNotification(Self);
  FDatasource:=AValue;
  if Assigned(FDatasource) then
    FDatasource.FreeNotification(Self);
end;

procedure TCustomDBTableWidget.SetRowKeyField(AValue: String);
begin
  if FRowKeyField=AValue then Exit;
  FRowKeyField:=AValue;
end;

procedure TCustomDBTableWidget.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (AComponent=FDatasource) then
    FDataSource:=Nil;
end;


function TCustomDBTableWidget.GetColumns: TDBTableColumns;
begin
  Result:=CustomColumns as TDBTableColumns;
end;

procedure TCustomDBTableWidget.SetColumns(AValue: TDBTableColumns);
begin
  Customcolumns.Assign(AValue);
end;


function TCustomDBTableWidget.CreateColumns: TCustomTableColumns;
begin
  Result:=TDBTableColumns.Create(TDBTableColumn);
end;

function TCustomDBTableWidget.GetDataset: TDataset;
begin
  Result:=Datasource.Dataset;
end;


end.

