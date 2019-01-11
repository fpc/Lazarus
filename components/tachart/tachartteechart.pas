{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  TeeChart compatibility.

  Authors: Alexander Klenin

}
unit TAChartTeeChart;

{$H+}

interface

uses
  Classes, SysUtils,
  TAGraph, TAChartAxis, TAChartAxisUtils, TAChartUtils, TACustomSeries,
  TASeries, TATransformations;

type
  TChartTeeChart = class helper for TChart
  strict private
    // Workaround for issue #21809.
    function GetAxisByAlign1(AIndex: TChartAxisAlignment): TChartAxis; inline;
    function GetMargin(AIndex: Integer): Integer; inline;
    procedure SetMargin(AIndex: Integer; AValue: TChartDistance); inline;
  public
    procedure UndoZoom;
    procedure ZoomPercent(const PercentZoom: Double);
    property RightAxis: TChartAxis index calRight read GetAxisByAlign1;
    property TopAxis: TChartAxis index calTop read GetAxisByAlign1;
  public
    property MarginBottom: TChartDistance index 4 read GetMargin write SetMargin;
    property MarginLeft: TChartDistance index 1 read GetMargin write SetMargin;
    property MarginRight: TChartDistance index 3 read GetMargin write SetMargin;
    property MarginTop: TChartDistance index 2 read GetMargin write SetMargin;
  end;

  TPointSeries = class(TLineSeries)
  published
    property LineType default ltNone;
    property ShowPoints default true;
  end;

  THorizBarSeries = class(TBarSeries)
  public
    // Swap X and Y as TeeChart does.
    function AddXY(
      AX, AY: Double; AXLabel: String = '';
      AColor: TChartColor = clTAColor): Integer; overload; inline;
    constructor Create(AOwner: TComponent); override;
  published
    property AxisIndexX default 0;
    property AxisIndexY default 1;
  end;

  TChartSeriesStyle = set of (
    tssIsTemplate, tssDenyChangeType, tssDenyDelete, tssDenyClone,
    tssIsPersistent, tssHideDataSource);

  TCustomChartSeriesTeeChart = class helper for TCustomChartSeries
  strict private
    function GetStyle: TChartSeriesStyle; inline;
    procedure SetStyle(AValue: TChartSeriesStyle); inline;
  published
    // Does not do anything, just avoid IFDEFs in client code.
    property Style: TChartSeriesStyle read GetStyle write SetStyle default [];
  end;

  TChartAxisTeeChart = class helper for TChartAxis
  strict private
    function GetLogarithmic: Boolean;
    procedure SetLogarithmic(AValue: Boolean);
  published
    property Logarithmic: Boolean
      read GetLogarithmic write SetLogarithmic default false;
  end;

implementation

uses
  Math, TAGeometry;

type
  TLogTransformEnumerator = class(TAxisTransformEnumerator)
    function GetCurrent: TLogarithmAxisTransform;
    function MoveNext: Boolean;
    property Current: TLogarithmAxisTransform read GetCurrent;
    function GetEnumerator: TLogTransformEnumerator;
  end;

var
  VLogTransforms: array of TChartAxisTransformations;

function AddLogTransforms: TChartAxisTransformations;
begin
  Result := TChartAxisTransformations.Create(nil);
  TLogarithmAxisTransform.Create(nil).Transformations := Result;
  SetLength(VLogTransforms, Length(VLogTransforms) + 1);
  VLogTransforms[High(VLogTransforms)] := Result;
end;

procedure FreeLogTransforms;
var
  t: TChartAxisTransformations;
begin
  for t in VLogTransforms do
    t.Free;
  VLogTransforms := nil;
end;

{ TLogTransformEnumerator }

function TLogTransformEnumerator.GetCurrent: TLogarithmAxisTransform;
begin
  Result := inherited GetCurrent as TLogarithmAxisTransform;
end;

function TLogTransformEnumerator.GetEnumerator: TLogTransformEnumerator;
begin
  Result := Self;
end;

function TLogTransformEnumerator.MoveNext: Boolean;
begin
  repeat
    Result := inherited MoveNext;
  until Result and (inherited GetCurrent is TLogarithmAxisTransform);
end;

{ TChartAxisTeeChart }

function TChartAxisTeeChart.GetLogarithmic: Boolean;
var
  t: TLogarithmAxisTransform;
begin
  if Transformations <> nil then
    for t in TLogTransformEnumerator.Create(Transformations.List) do
      if t.Enabled then
        exit(true);
  Result := false;
end;

procedure TChartAxisTeeChart.SetLogarithmic(AValue: Boolean);
var
  t: TLogarithmAxisTransform;
begin
  Intervals.Tolerance := IfThen(AValue, 2, 0);
  if Transformations <> nil then
    for t in TLogTransformEnumerator.Create(Transformations.List) do
      t.Enabled := AValue
  else if AValue then
    Transformations := AddLogTransforms;
end;

{ TCustomChartSeriesTeeChart }

function TCustomChartSeriesTeeChart.GetStyle: TChartSeriesStyle;
begin
  Result := [];
end;

procedure TCustomChartSeriesTeeChart.SetStyle(AValue: TChartSeriesStyle);
begin
  Unused(AValue);
end;

{ THorizBarSeries }

function THorizBarSeries.AddXY(
  AX, AY: Double; AXLabel: String; AColor: TChartColor): Integer;
begin
  Result := inherited AddXY(AY, AX, AXLabel, AColor);
end{%H-};  // to silence the compiler warning of impossible inline of inherited method

constructor THorizBarSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetPropDefaults(Self, ['AxisIndexX', 'AxisIndexY']);
end;

{ TChartTeeChart }

function TChartTeeChart.GetAxisByAlign1(AIndex: TChartAxisAlignment): TChartAxis;
begin
  // Using "inherited" here results in a crash, probably due to FPC bug.
  Result := GetAxisByAlign(AIndex);
end;

function TChartTeeChart.GetMargin(AIndex: Integer): Integer;
begin
  Result := Margins.GetValue(AIndex);
end;

procedure TChartTeeChart.SetMargin(AIndex: Integer; AValue: TChartDistance);
begin
  Margins.SetValue(AIndex, AValue);
end;

procedure TChartTeeChart.UndoZoom;
begin
  ZoomFull;
end;

procedure TChartTeeChart.ZoomPercent(const PercentZoom: Double);
var
  ext: TDoubleRect;
  factor: Double;
  dx, dy: Double;
begin
  if PercentZoom <= 0 then exit;
  factor := (1.0 - PercentZoom * 0.01) * 0.5;
  ext := LogicalExtent;
  dx := (ext.b.x - ext.a.x) * factor;
  dy := (ext.b.y - ext.a.y) * factor;
  LogicalExtent := DoubleRect(ext.a.x - dx, ext.a.y - dy, ext.b.x + dx, ext.b.y + dy);
end;

procedure Dummy;
begin
  // Workaround for issue #21808.
end;

initialization
  Dummy;

finalization
  FreeLogTransforms;

end.

