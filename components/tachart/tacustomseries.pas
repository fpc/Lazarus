{

 Basic types for TAChart series.

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
unit TACustomSeries;

{$H+}

interface

uses
  Classes, Graphics, SysUtils,
  TAChartUtils, TAGraph, TASources, TATypes;

type
  { TCustomChartSeries }

  TCustomChartSeries = class(TBasicChartSeries)
  protected
    procedure SetActive(AValue: Boolean); override;
    procedure SetDepth(AValue: TChartDistance); override;
    procedure SetShowInLegend(AValue: Boolean); override;
    procedure SetZPosition(AValue: TChartDistance); override;
    procedure StyleChanged(Sender: TObject);
    procedure UpdateParentChart;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TChartGetMarkEvent = procedure (
    out AFormattedMark: String; AIndex: Integer) of object;

  { TChartSeries }

  TChartSeries = class(TCustomChartSeries)
  private
    FBuiltinSource: TCustomChartSource;
    FListener: TListener;
    FMarks: TChartMarks;
    FOnGetMark: TChartGetMarkEvent;
    FSource: TCustomChartSource;

    function GetSource: TCustomChartSource;
    function IsSourceStored: boolean;
    procedure SetMarks(const AValue: TChartMarks);
    procedure SetOnGetMark(const AValue: TChartGetMarkEvent);
    procedure SetSource(AValue: TCustomChartSource);
  protected
    procedure AfterAdd; override;
    procedure AfterDraw; override;
    procedure BeforeDraw; override;
    function ColorOrDefault(AColor: TColor; ADefault: TColor = clTAColor): TColor;
    function GetGraphPoint(AIndex: Integer): TDoublePoint;
    function GetGraphPointX(AIndex: Integer): Double;
    function GetSeriesColor: TColor; virtual;
    function GetXMaxVal: Integer;
    procedure GetBounds(out ABounds: TDoubleRect); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  public
    function  GetColor(AIndex: Integer): TColor;
    procedure GetMax(out X, Y: Double);
    procedure GetMin(out X, Y: Double);
    function  GetXImgValue(AIndex: Integer): Integer;
    function  GetXMax: Double;
    function  GetXMin: Double;
    function  GetXValue(AIndex: Integer): Double;
    function  GetYImgValue(AIndex: Integer): Integer;
    function  GetYMax: Double;
    function  GetYMin: Double;
    function  GetYValue(AIndex: Integer): Double;
    procedure SetColor(AIndex: Integer; AColor: TColor);
    procedure SetXValue(AIndex: Integer; AValue: Double); inline;
    procedure SetYValue(AIndex: Integer; AValue: Double); inline;
  public
    function Add(AValue: Double; XLabel: String; Color: TColor): Integer; inline;
    function AddXY(X, Y: Double; XLabel: String; Color: TColor): Integer; virtual; overload;
    function AddXY(X, Y: Double): Integer; overload; inline;
    procedure Clear; inline;
    function Count: Integer; inline;
    function DefaultFormattedMark(AIndex: integer): String;
    procedure Delete(AIndex: Integer); virtual;
    function Extent: TDoubleRect; virtual;
    function FormattedMark(AIndex: integer): String;
    function IsEmpty: Boolean; override;
    function ListSource: TListChartSource;
    property Source: TCustomChartSource
      read GetSource write SetSource stored IsSourceStored;
  published
    property Active default true;
    property Marks: TChartMarks read FMarks write SetMarks;
    property ShowInLegend;
    property Title;
    property ZPosition;
  published
    property OnGetMark: TChartGetMarkEvent read FOnGetMark write SetOnGetMark;
  end;

implementation

type

  { TChartSeriesListener }

  TChartSeriesListener = class(TListener)
  private
    FSeries: TChartSeries;
  public
    constructor Create(ASeries: TChartSeries);
    procedure Forget; override;
    procedure Notify; override;
  end;

{ TChartSeriesListener }

constructor TChartSeriesListener.Create(ASeries: TChartSeries);
begin
  FSeries := ASeries;
end;

procedure TChartSeriesListener.Forget;
begin
  inherited Forget;
  FSeries.FSource := nil;
end;

procedure TChartSeriesListener.Notify;
begin
  FSeries.UpdateParentChart;
end;

{ TCustomChartSeries }

constructor TCustomChartSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := true;
  FShowInLegend := true;
end;

procedure TCustomChartSeries.SetActive(AValue: Boolean);
begin
  if FActive = AValue then exit;
  FActive := AValue;
  UpdateParentChart;
end;

procedure TCustomChartSeries.SetDepth(AValue: TChartDistance);
begin
  if FDepth = AValue then exit;
  FDepth := AValue;
  UpdateParentChart;
end;

procedure TCustomChartSeries.SetShowInLegend(AValue: Boolean);
begin
  if FShowInLegend = AValue then exit;
  FShowInLegend := AValue;
  UpdateParentChart;
end;

procedure TCustomChartSeries.SetZPosition(AValue: TChartDistance);
begin
  if FZPosition = AValue then exit;
  FZPosition := AValue;
  UpdateParentChart;
end;

procedure TCustomChartSeries.StyleChanged(Sender: TObject);
begin
  UpdateParentChart;
end;

procedure TCustomChartSeries.UpdateParentChart;
begin
  if ParentChart <> nil then ParentChart.Invalidate;
end;

{ TChartSeries }

function TChartSeries.Add(AValue: Double; XLabel: String; Color: TColor): Integer;
begin
  Result := AddXY(GetXMaxVal + 1, AValue, XLabel, Color);
end;

function TChartSeries.AddXY(X, Y: Double; XLabel: String; Color: TColor): Integer;
begin
  Result := ListSource.Add(X, Y, XLabel, Color);
end;

function TChartSeries.AddXY(X, Y: Double): Integer;
begin
  Result := AddXY(X, Y, '', clTAColor);
end;

procedure TChartSeries.AfterAdd;
begin
  FMarks.SetOwner(FChart);
end;

procedure TChartSeries.AfterDraw;
begin
  Source.AfterDraw;
end;

procedure TChartSeries.BeforeDraw;
begin
  Source.BeforeDraw;
end;

procedure TChartSeries.Clear;
begin
  ListSource.Clear;
end;

function TChartSeries.ColorOrDefault(AColor: TColor; ADefault: TColor): TColor;
begin
  Result := AColor;
  if Result <> clTAColor then exit;
  Result := ADefault;
  if Result <> clTAColor then exit;
  Result := GetSeriesColor;
end;

function TChartSeries.Count: Integer;
begin
  Result := Source.Count;
end;

constructor TChartSeries.Create(AOwner: TComponent);
const
  BUILTIN_SOURCE_NAME = 'Builtin';
begin
  inherited Create(AOwner);

  FListener := TChartSeriesListener.Create(Self);
  FBuiltinSource := TListChartSource.Create(Self);
  FBuiltinSource.Name := BUILTIN_SOURCE_NAME;
  FBuiltinSource.Subscribe(FListener);
  FMarks := TChartMarks.Create(FChart);
end;

function TChartSeries.DefaultFormattedMark(AIndex: integer): String;
const
  TO_PERCENT = 100;
var
  total, percent: Double;
begin
  total := Source.ValuesTotal;
  with Source[AIndex]^ do begin
    if total = 0 then
      percent := 0
    else
      percent := Y / total * TO_PERCENT;
    Result := Format(FMarks.Format, [y, percent, Text, total, X]);
  end;
end;

procedure TChartSeries.Delete(AIndex: Integer);
begin
  ListSource.Delete(AIndex);
end;

destructor TChartSeries.Destroy;
begin
  if FListener.IsListening then
    Source.Unsubscribe(FListener);
  FBuiltinSource.Free;
  FMarks.Free;
  FListener.Free;

  inherited Destroy;
end;

function TChartSeries.Extent: TDoubleRect;
begin
  Result := Source.Extent;
end;

function TChartSeries.FormattedMark(AIndex: integer): String;
begin
  if Assigned(FOnGetMark) then
    FOnGetMark(Result, AIndex)
  else
    Result := DefaultFormattedMark(AIndex);
end;

procedure TChartSeries.GetBounds(out ABounds: TDoubleRect);
begin
  if not Active or (Count = 0) then exit;
  ABounds := Extent;
end;

function TChartSeries.GetColor(AIndex: Integer): TColor;
begin
  Result := ColorOrDefault(Source[AIndex]^.Color);
end;

function TChartSeries.GetGraphPoint(AIndex: Integer): TDoublePoint;
begin
  with Source[AIndex]^ do begin
    Result.X := AxisToGraphX(X);
    Result.Y := AxisToGraphY(Y);
  end;
end;

function TChartSeries.GetGraphPointX(AIndex: Integer): Double;
begin
  Result := AxisToGraphX(Source[AIndex]^.X);
end;

procedure TChartSeries.GetMax(out X, Y: Double);
begin
  X := Source.XOfMax;
  Y := Extent.b.Y;
end;

procedure TChartSeries.GetMin(out X, Y: Double);
begin
  X := Source.XOfMin;
  Y := Extent.a.Y;
end;

function TChartSeries.GetSeriesColor: TColor;
begin
  Result := clTAColor;
end;

function TChartSeries.GetSource: TCustomChartSource;
begin
  if Assigned(FSource) then
    Result := FSource
  else
    Result := FBuiltinSource;
end;

function TChartSeries.GetXImgValue(AIndex: Integer): Integer;
begin
  Result := ParentChart.XGraphToImage(Source[AIndex]^.X);
end;

function TChartSeries.GetXMax: Double;
begin
  Result := Extent.b.X;
end;

function TChartSeries.GetXMaxVal: Integer;
begin
  if Count > 0 then
    Result := Round(Source[Count - 1]^.X)
  else
    Result := 0;
end;

function TChartSeries.GetXMin: Double;
begin
  Result := Extent.a.X;
end;

function TChartSeries.GetXValue(AIndex: Integer): Double;
begin
  Result := Source[AIndex]^.X;
end;

function TChartSeries.GetYImgValue(AIndex: Integer): Integer;
begin
  Result := ParentChart.YGraphToImage(Source[AIndex]^.Y);
end;

function TChartSeries.GetYMax: Double;
begin
  Result := Extent.b.Y;
end;

function TChartSeries.GetYMin: Double;
begin
  Result := Extent.a.Y;
end;

function TChartSeries.GetYValue(AIndex: Integer): Double;
begin
  Result := Source[AIndex]^.Y;
end;

function TChartSeries.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TChartSeries.IsSourceStored: boolean;
begin
  Result := FSource <> nil;
end;

function TChartSeries.ListSource: TListChartSource;
begin
  if not (Source is TListChartSource) then
    raise EEditableSourceRequired.Create('Editable chart source required');
  Result := Source as TListChartSource;
end;

procedure TChartSeries.SetColor(AIndex: Integer; AColor: TColor);
begin
  Source[AIndex]^.Color := AColor;
end;

procedure TChartSeries.SetMarks(const AValue: TChartMarks);
begin
  if FMarks = AValue then exit;
  FMarks.Assign(AValue);
end;

procedure TChartSeries.SetOnGetMark(const AValue: TChartGetMarkEvent);
begin
  if FOnGetMark = AValue then exit;
  FOnGetMark := AValue;
  UpdateParentChart;
end;

procedure TChartSeries.SetSource(AValue: TCustomChartSource);
begin
  if FSource = AValue then exit;
  if FListener.IsListening then
    Source.Unsubscribe(FListener);
  FSource := AValue;
  Source.Subscribe(FListener);
  UpdateParentChart;
end;

procedure TChartSeries.SetXValue(AIndex: Integer; AValue: Double); inline;
begin
  ListSource.SetXValue(AIndex, AValue);
end;

procedure TChartSeries.SetYValue(AIndex: Integer; AValue: Double); inline;
begin
  ListSource.SetYValue(AIndex, AValue);
end;

end.

