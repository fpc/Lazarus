{

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

 Authors: Alexander Klenin

}
unit TADbSource;

{$H+}

interface

uses
  Classes, Db, TAChartUtils, TACustomSource;

type

  { TDbChartSource }

  TDbChartSource = class(TCustomChartSource)
  strict private
    FBookmark: TBookmark;
    FCurItem: TChartDataItem;
    FDataLink: TDataLink;
    FFieldColor: String;
    FFieldText: String;
    FFieldX: String;
    FFieldY: String;
    FFieldYList: TStringList;

    function GetDataSource: TDataSource;
    procedure SetDataSource(AValue: TDataSource);
    procedure SetFieldColor(const AValue: String);
    procedure SetFieldText(const AValue: String);
    procedure SetFieldX(const AValue: String);
    procedure SetFieldY(const AValue: String);
  protected
    function GetCount: Integer; override;
    function GetItem(AIndex: Integer): PChartDataItem; override;
    procedure SetYCount(AValue: Cardinal); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure AfterDraw; override;
    procedure BeforeDraw; override;
    procedure Reset;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property FieldColor: String read FFieldColor write SetFieldColor;
    property FieldText: String read FFieldText write SetFieldText;
    property FieldX: String read FFieldX write SetFieldX;
    property FieldY: String read FFieldY write SetFieldY;
  end;

procedure Register;

implementation

uses
  Math, SysUtils;

type

  { TDbChartSourceDataLink }

  TDbChartSourceDataLink = class(TDataLink)
  strict private
    FChartSrc: TDbChartSource;
  protected
    procedure DataSetChanged; override;
    procedure DataSetScrolled(ADistance: Integer); override;
    procedure UpdateData; override;
  public
    constructor Create(ASrc: TDbChartSource);
  end;

// FIXME: This is a workaround for issue #19887.
// Remove when dataset gains the capability to turn data events off.
var
  VLockedDatasets: TFPList;

{ TDbChartSourceDataLink }

constructor TDbChartSourceDataLink.Create(ASrc: TDbChartSource);
begin
  FChartSrc := ASrc;
  VisualControl := true;
end;

procedure TDbChartSourceDataLink.DataSetChanged;
begin
  inherited DataSetChanged;
  if DataSet.State = dsBrowse then
    FChartSrc.Reset;
end;

procedure TDbChartSourceDataLink.DataSetScrolled(ADistance: Integer);
begin
  Unused(ADistance); // No need to react on scrolling.
end;

procedure TDbChartSourceDataLink.UpdateData;
begin
  inherited UpdateData;
  FChartSrc.Reset;
end;

procedure Register;
begin
  RegisterComponents(CHART_COMPONENT_IDE_PAGE, [TDbChartSource]);
end;

{ TDbChartSource }

procedure TDbChartSource.AfterDraw;
begin
  inherited AfterDraw;
  try
    if not FDataLink.Active or (FBookmark = nil) then exit;
    FDataLink.DataSet.GotoBookmark(FBookmark);
    FDataLink.DataSet.FreeBookmark(FBookmark);
  finally
    FBookmark := nil;
    VLockedDatasets.Remove(FDataLink.DataSet);
  end;
end;

procedure TDbChartSource.BeforeDraw;
begin
  inherited BeforeDraw;
  VLockedDatasets.Add(FDataLink.DataSet);
  if FDataLink.Active then
    FBookmark := FDataLink.DataSet.GetBookmark;
end;

constructor TDbChartSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TDbChartSourceDataLink.Create(Self);
  FFieldYList := TStringList.Create;
  FYCount := 0; // Set to 1 by inherited.
end;

destructor TDbChartSource.Destroy;
begin
  FreeAndNil(FDataLink);
  FreeAndNil(FFieldYList);
  inherited;
end;

function TDbChartSource.GetCount: Integer;
begin
  if FDataLink.Active then
    Result := DataSource.DataSet.RecordCount
  else
    Result := 0;
end;

function TDbChartSource.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TDbChartSource.GetItem(AIndex: Integer): PChartDataItem;
var
  ds: TDataSet;
  i: Integer;
begin
  Result := @FCurItem;
  SetDataItemDefaults(FCurItem);
  if not FDataLink.Active then exit;

  Inc(AIndex); // RecNo is counted from 1
  ds := FDataLink.DataSet;
  if ds.IsUniDirectional then begin
    if ds.RecNo < AIndex then
      ds.First;
  end
  else begin
    if AIndex > ds.RecNo - AIndex then
      while (ds.RecNo > AIndex) and not ds.BOF do
        ds.Prior
    else
      ds.First;
  end;
  while (ds.RecNo < AIndex) and not ds.EOF do
    ds.Next;
  if ds.RecNo <> AIndex then exit;
  if FieldX <> '' then
    FCurItem.X := ds.FieldByName(FieldX).AsFloat
  else
    FCurItem.X := ds.RecNo;
  if FYCount > 0 then begin
    FCurItem.Y := ds.FieldByName(FFieldYList[0]).AsFloat;
    for i := 0 to High(FCurItem.YList) do
      FCurItem.YList[i] := ds.FieldByName(FFieldYList[i + 1]).AsFloat;
  end;
  if FieldColor <> '' then
    FCurItem.Color := ds.FieldByName(FieldColor).AsInteger;
  if FieldText <> '' then
    FCurItem.Text := ds.FieldByName(FieldText).AsString;
end;

procedure TDbChartSource.Reset;
begin
  if VLockedDatasets.IndexOf(FDataLink.DataSet) >= 0 then exit;
  InvalidateCaches;
  Notify;
end;

procedure TDbChartSource.SetDataSource(AValue: TDataSource);
begin
  if DataSource = AValue then exit;
  FDataLink.DataSource := AValue;
end;

procedure TDbChartSource.SetFieldColor(const AValue: String);
begin
  if FFieldColor = AValue then exit;
  FFieldColor := AValue;
  Reset;
end;

procedure TDbChartSource.SetFieldText(const AValue: String);
begin
  if FFieldText = AValue then exit;
  FFieldText := AValue;
  Reset;
end;

procedure TDbChartSource.SetFieldX(const AValue: String);
begin
  if FFieldX = AValue then exit;
  FFieldX := AValue;
  Reset;
end;

procedure TDbChartSource.SetFieldY(const AValue: String);
begin
  if FFieldY = AValue then exit;
  FFieldY := AValue;
  if FFieldY = '' then
    FFieldYList.Clear
  else
    FFieldYList.CommaText := FFieldY;
  FYCount := FFieldYList.Count;
  SetLength(FCurItem.YList, Max(FYCount - 1, 0));
  Reset;
end;

procedure TDbChartSource.SetYCount(AValue: Cardinal);
begin
  Unused(AValue);
  raise EYCountError.Create('Set FieldY instead');
end;

initialization
  VLockedDatasets := TFPList.Create;

finalization
  FreeAndNil(VLockedDatasets);

end.

