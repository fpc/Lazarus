unit lrTDbfData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LR_Class, LR_DBComponent, DB, dbf;

type
  TlrTDbfData = class(TComponent)
  end;

type

  { TLRDbf }

  TLRDbf = class(TLRDataSetControl)
  private
    function GetFilePath: string;
    function GetIndexName: string;
    function GetShowDeleted: Boolean;
    function GetTableName: string;
    procedure SetFilePath(AValue: string);
    procedure SetIndexName(AValue: string);
    procedure SetShowDeleted(AValue: Boolean);
    procedure SetTableName(AValue: string);
  protected
    procedure SetDataSource(AValue: string); override;
    procedure AfterLoad;override;
  public
    constructor Create(AOwnerPage:TfrPage); override;

    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
  published
    property ShowDeleted: Boolean read GetShowDeleted write SetShowDeleted;
    property TableName: string read GetTableName write SetTableName;
    property IndexName: string read GetIndexName write SetIndexName;
    property FilePath: string read GetFilePath write SetFilePath;
  end;

procedure Register;

implementation
{$R lrtdbfdata_img.res}
uses LR_Utils;

var
  lrBMP_TDbf:TBitmap = nil;

procedure InitLRComp;
begin
  if not assigned(lrBMP_TDbf) then
  begin
    lrBMP_TDbf := TbitMap.Create;
    lrBMP_TDbf.LoadFromResourceName(HInstance, 'TLRDbf');
    frRegisterObject(TLRDbf, lrBMP_TDbf, 'TLRDbf', nil, otlUIControl, nil);
  end;
end;

procedure Register;
begin
  RegisterComponents('LazReport',[TlrTDbfData]);
end;

{ TLRDbf }

procedure TLRDbf.SetIndexName(AValue: string);
begin
  TDbf(DataSet).IndexName:=AValue
end;

function TLRDbf.GetFilePath: string;
begin
  Result:=TDbf(DataSet).FilePath;
end;

function TLRDbf.GetIndexName: string;
begin
  Result:=TDbf(DataSet).IndexName;
end;

function TLRDbf.GetShowDeleted: Boolean;
begin
  Result:=TDbf(DataSet).ShowDeleted;
end;

function TLRDbf.GetTableName: string;
begin
  Result:=TDbf(DataSet).TableName;
end;

procedure TLRDbf.SetFilePath(AValue: string);
begin
  TDbf(DataSet).FilePath:=AValue
end;

procedure TLRDbf.SetShowDeleted(AValue: Boolean);
begin
  TDbf(DataSet).ShowDeleted:=AValue
end;

procedure TLRDbf.SetTableName(AValue: string);
begin
  TDbf(DataSet).TableName:=AValue
end;

//TDBF not used Master-detail
procedure TLRDbf.SetDataSource(AValue: string);
begin
end;

procedure TLRDbf.AfterLoad;
begin
  try
    if FileExists(TDbf(DataSet).FilePathFull + TDbf(DataSet).TableName) then
      DataSet.Active:=FActive;
  finally
  end;
end;

constructor TLRDbf.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName := 'lrDbf';
  DataSet:=TDbf.Create(OwnerForm);
end;

procedure TLRDbf.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
  TableName  := XML.GetValue(Path+'TableName/Value'{%H-}, '');
  IndexName  := XML.GetValue(Path+'IndexName/Value'{%H-}, '');
  ShowDeleted:= XML.GetValue(Path+'ShowDeleted/Value'{%H-}, false);
  FilePath:= XML.GetValue(Path+'FilePath/Value'{%H-}, '');
end;

procedure TLRDbf.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path+'TableName/Value'{%H-}, TableName);
  XML.SetValue(Path+'IndexName/Value'{%H-}, IndexName);
  XML.SetValue(Path+'ShowDeleted/Value'{%H-}, ShowDeleted);
  XML.SetValue(Path+'FilePath/Value'{%H-}, FilePath);
end;

initialization
  InitLRComp;

{  RegisterPropertyEditor(TypeInfo(string), TLRZQuery, 'Database', TLRZQueryDataBaseProperty);
  RegisterPropertyEditor(TypeInfo(string), TLRZConnection, 'Protocol', TLRZConnectionProtocolProperty);
}
finalization
  if Assigned(lrBMP_TDbf) then
    FreeAndNil(lrBMP_TDbf);
end.

