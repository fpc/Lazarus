unit LR_DBComponent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, LR_Class, LR_DBSet;

type
  { TLRDataSetControl }

  TLRDataSetControl = class(TfrNonVisualControl)
  private
    FFilter: string;
    FlrDBDataSet:TfrDBDataSet;
    FlrDataSource:TDataSource;
    FDS:TDataSet;
    FDataSource: string;
    function GetFieldCount: integer;
    function GetActive: boolean;
    function GetEOF: boolean;
    function GetRecordCount: integer;
    procedure SetActive(AValue: boolean);
    procedure SetDataSet(AValue: TDataSet);
    procedure SetFilter(AValue: string);
  protected
    FActive:boolean;
    procedure SetName(const AValue: string); override;
    procedure SetDataSource(AValue: string); virtual;
    procedure AfterLoad;override;
    function ExecMetod(const AName: String; p1, p2, p3: Variant; var Val: Variant):boolean;override;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    destructor Destroy; override;

    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
    property DataSet:TDataSet read FDS write SetDataSet;
    property lrDBDataSet:TfrDBDataSet read FlrDBDataSet;
    property lrDataSource:TDataSource read FlrDataSource;
  published
    property Active:boolean read GetActive write SetActive;
    property EOF:boolean read GetEOF;
    property RecordCount:integer read GetRecordCount;
    property FieldCount:integer read GetFieldCount;
    property Filter:string read FFilter write SetFilter;
    property DataSource:string read FDataSource write SetDataSource;
  end;

implementation
uses DBPropEdits, PropEdits, LazarusPackageIntf, types, LR_Utils;

{ TLRDataSetControl }

function TLRDataSetControl.GetFieldCount: integer;
begin
  if FDS.Active then
    Result:=FDS.RecordCount
  else
    Result:=0;
end;

function TLRDataSetControl.GetActive: boolean;
begin
  Result:=FDS.Active
end;

function TLRDataSetControl.GetEOF: boolean;
begin
  if FDS.Active then
    Result:=FDS.EOF
  else
    Result:=true;
end;

function TLRDataSetControl.GetRecordCount: integer;
begin
  if FDS.Active then
    Result:=FDS.RecordCount
  else
    Result:=0;
end;

procedure TLRDataSetControl.SetActive(AValue: boolean);
begin
{  FActive:=AValue;
  if Assigned(FDS.Connection) then}
  FDS.Active:=AValue;
end;

procedure TLRDataSetControl.SetDataSet(AValue: TDataSet);
begin
  if FDS=AValue then Exit;
  FDS:=AValue;
  FlrDBDataSet.DataSet:=FDS;
  FlrDataSource.DataSet:=FDS;
end;

procedure TLRDataSetControl.SetDataSource(AValue: string);
begin
  if FDataSource=AValue then Exit;
  FDataSource:=AValue;
end;

procedure TLRDataSetControl.SetFilter(AValue: string);
begin
  if FFilter=AValue then Exit;
  FFilter:=AValue;
end;

procedure TLRDataSetControl.SetName(const AValue: string);
begin
  inherited SetName(AValue);
  FDS.Name:=Name;
  FlrDBDataSet.Name:='_'+Name;
  FlrDataSource.Name:='ds'+Name;
end;

procedure TLRDataSetControl.AfterLoad;
begin
  inherited AfterLoad;
  DataSet.Active:=FActive;
end;

function TLRDataSetControl.ExecMetod(const AName: String; p1, p2, p3: Variant;
  var Val: Variant): boolean;
begin
  Result:=inherited ExecMetod(AName, p1, p2, p3, Val);
  if Result then exit;

  if AName = 'NEXT' then
    FDS.Next
  else
  if AName = 'FIRST' then
    FDS.First
  else
  if AName = 'LAST' then
    FDS.Last
  else
  if AName = 'PRIOR' then
    FDS.Prior
  else
  if AName = 'OPEN' then
    FDS.Open
  else
  if AName = 'CLOSE' then
    FDS.Close
  else
    exit;
  Result:=true;
end;

constructor TLRDataSetControl.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  FDesignOptions:=FDesignOptions + [doUndoDisable];
  FlrDBDataSet:=TfrDBDataSet.Create(OwnerForm);
  FlrDataSource:=TDataSource.Create(OwnerForm);
end;

destructor TLRDataSetControl.Destroy;
begin
  FreeAndNil(FDS);
  FreeAndNil(FlrDBDataSet);
  FreeAndNil(FlrDataSource);
  inherited Destroy;
end;

procedure TLRDataSetControl.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
  FActive  := XML.GetValue(Path + 'Active/Value'{%H-}, false);
  FDataSource  := XML.GetValue(Path + 'DataSource/Value'{%H-}, '');
end;

procedure TLRDataSetControl.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path+'Active/Value', Active);
  XML.SetValue(Path + 'DataSource/Value'{%H-}, FDataSource);
end;

type

  { TLRDataSetControlDataSourceProperty }

  TLRDataSetControlDataSourceProperty = class(TFieldProperty)
  public
    procedure FillValues(const Values: TStringList); override;
  end;

{ TLRDataSetControlDataSourceProperty }

procedure TLRDataSetControlDataSourceProperty.FillValues(
  const Values: TStringList);
begin
  if (GetComponent(0) is TLRDataSetControl) then
    frGetComponents(nil, TDataSource, Values, nil);
end;

initialization
  RegisterPropertyEditor(TypeInfo(string), TLRDataSetControl, 'DataSource', TLRDataSetControlDataSourceProperty);

finalization
end.


