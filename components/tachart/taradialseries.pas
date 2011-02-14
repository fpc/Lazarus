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

unit TARadialSeries;

{$H+}

interface

uses
  Classes, Graphics, SysUtils, Types,
  TACustomSeries, TALegend, TAChartUtils;

type
  TLabelParams = record
    FAttachment: TPoint;
    FCenter: TPoint;
    FText: String;
  end;

  TPieSlice = record
    FAngle: Double;
    FBase: TPoint;
    FLabel: TLabelParams;
  end;

  { TCustomPieSeries }

  TCustomPieSeries = class(TChartSeries)
  private
    FCenter: TPoint;
    FExploded: Boolean;
    FFixedRadius: TChartDistance;
    FRadius: Integer;
    FRotateLabels: Boolean;
    FSlices: array of TPieSlice;
    procedure Measure(ACanvas: TCanvas);
    procedure SetExploded(AValue: Boolean);
    procedure SetFixedRadius(AValue: TChartDistance);
    procedure SetRotateLabels(AValue: Boolean);
    function SliceColor(AIndex: Integer): TColor;
    function TryRadius(ACanvas: TCanvas): TRect;
  protected
    procedure AfterAdd; override;
    procedure GetLegendItems(AItems: TChartLegendItems); override;
  public
    function AddPie(Value: Double; Text: String; Color: TColor): Longint;
    procedure Draw(ACanvas: TCanvas); override;
    function FindContainingSlice(const APoint: TPoint): Integer;

    // Offset slices away from center based on X value.
    property Exploded: Boolean read FExploded write SetExploded default false;
    property FixedRadius: TChartDistance
      read FFixedRadius write SetFixedRadius default 0;
    property RotateLabels: Boolean
      read FRotateLabels write SetRotateLabels default false;
    property Source;
  end;

implementation

uses
  Math,
  TADrawUtils, TACustomSource;

{ TCustomPieSeries }

function TCustomPieSeries.AddPie(
  Value: Double; Text: String; Color: TColor): Longint;
begin
  Result := AddXY(GetXMaxVal + 1, Value, Text, Color);
end;

procedure TCustomPieSeries.AfterAdd;
begin
  inherited;
  // disable axis when we have TPie series
  ParentChart.LeftAxis.Visible := false;
  ParentChart.BottomAxis.Visible := false;
end;

procedure TCustomPieSeries.Draw(ACanvas: TCanvas);
var
  i: Integer;
  prevAngle: Double = 0;
  prevLabelPoly: TPointArray = nil;
begin
  if IsEmpty then exit;

  Marks.SetAdditionalAngle(0);
  Measure(ACanvas);

  ACanvas.Pen.Color := clBlack;
  ACanvas.Pen.Style := psSolid;
  ACanvas.Brush.Style := bsSolid;
  for i := 0 to Count - 1 do begin
    ACanvas.Brush.Color := SliceColor(i);
    with FSlices[i] do begin
      ACanvas.RadialPie(
        FBase.X - FRadius, FBase.Y - FRadius,
        FBase.X + FRadius, FBase.Y + FRadius,
        RadToDeg16(prevAngle), RadToDeg16(FAngle));
      prevAngle += FAngle;
    end;
  end;
  if not Marks.IsMarkLabelsVisible then exit;
  prevAngle := 0;
  for i := 0 to Count - 1 do
    with FSlices[i].FLabel do begin
      if FText <> '' then begin
        if RotateLabels then
          Marks.SetAdditionalAngle(prevAngle + FSlices[i].FAngle / 2);
        Marks.DrawLabel(ACanvas, FAttachment, FCenter, FText, prevLabelPoly);
      end;
      prevAngle += FSlices[i].FAngle;
    end;
end;

function TCustomPieSeries.FindContainingSlice(const APoint: TPoint): Integer;
var
  prevAngle: Double = 0;
  c: TPoint;
  pointAngle: Double;
begin
  if IsEmpty then exit(-1);

  for Result := 0 to Count - 1 do
    with FSlices[Result] do begin
      c := APoint - FBase;
      pointAngle := ArcTan2(-c.Y, c.X);
      if pointAngle < 0 then
        pointAngle += 2 * Pi;
      if
        InRange(pointAngle - prevAngle, 0, FAngle) and
        (Sqr(c.X) + Sqr(c.Y) <= Sqr(FRadius))
      then
        exit;
      prevAngle += FAngle;
    end;
  Result := -1;
end;

procedure TCustomPieSeries.GetLegendItems(AItems: TChartLegendItems);
var
  i: Integer;
  br: TLegendItemBrushRect;
  ps: TLegendItemPieSlice;
begin
  case Legend.Multiplicity of
    lmSingle: begin
      br := TLegendItemBrushRect.Create(nil, Title);
      br.Color := SliceColor(0);
      AItems.Add(br);
    end;
    lmPoint:
      for i := 0 to Count - 1 do begin
        ps := TLegendItemPieSlice.Create(FormattedMark(i));
        ps.Color := SliceColor(i);
        AItems.Add(ps);
      end;
  end;
end;

procedure TCustomPieSeries.Measure(ACanvas: TCanvas);
const
  MIN_RADIUS = 5;
var
  a, b: Integer;
begin
  FCenter := CenterPoint(ParentChart.ClipRect);
  if FixedRadius = 0 then begin
    // Use binary search to find maximum radius fitting into the parent chart.
    a := MIN_RADIUS;
    with Size(ParentChart.ClipRect) do
      b := Max(cx div 2, cy div 2);
    repeat
      FRadius := (a + b) div 2;
      if IsRectInRect(TryRadius(ACanvas), ParentChart.ClipRect) then
        a := FRadius
      else
        b := FRadius - 1;
    until a >= b - 1;
  end
  else begin
    FRadius := FixedRadius;
    TryRadius(ACanvas);
  end;
end;

procedure TCustomPieSeries.SetExploded(AValue: Boolean);
begin
  if FExploded = AValue then exit;
  FExploded := AValue;
  UpdateParentChart;
end;

procedure TCustomPieSeries.SetFixedRadius(AValue: TChartDistance);
begin
  if FFixedRadius = AValue then exit;
  FFixedRadius := AValue;
  UpdateParentChart;
end;

procedure TCustomPieSeries.SetRotateLabels(AValue: Boolean);
begin
  if FRotateLabels = AValue then exit;
  FRotateLabels := AValue;
  UpdateParentChart;
end;

function TCustomPieSeries.SliceColor(AIndex: Integer): TColor;
begin
  Result :=
    ColorOrDefault(Source[AIndex]^.Color, Colors[AIndex mod High(Colors) + 1]);
end;

function TCustomPieSeries.TryRadius(ACanvas: TCanvas): TRect;

  function EndPoint(AAngle, ARadius: Double): TPoint;
  begin
    Result := RotatePoint(Point(Round(ARadius), 0), -AAngle);
  end;

  function LabelExtraDist(APoly: TPointArray; AAngle: Double): Double;
  const
    ALMOST_INF = 1e100;
  var
    sa, ca: Extended;
    denom, t, tmin: Double;
    a, b, d: TPoint;
    i: Integer;
  begin
    // x = t * ca; y = t * sa
    // (t * ca - a.x) * dy = (t * sa - a.y) * dx
    // t * (ca * dy - sa * dx) = a.x * dy - a.y * dx
    SinCos(-Pi - AAngle, sa, ca);
    b := APoly[High(APoly)];
    tmin := ALMOST_INF;
    for i := 0 to High(APoly) do begin
      a := APoly[i];
      d := b - a;
      denom := ca * d.Y - sa * d.X;
      if denom <> 0 then begin
        t := (a.X * d.Y - a.Y * d.X) / denom;
        if t > 0 then
          tmin := Min(tmin, t);
      end;
      b := a;
    end;
    Result := Norm([tmin * ca, tmin * sa]);
  end;

  procedure PrepareLabel(
    var ALabel: TLabelParams; AIndex: Integer; AAngle: Double);
  var
    i: Integer;
    p: TPointArray;
  begin
    with ALabel do begin
      FCenter := FAttachment;
      if not Marks.IsMarkLabelsVisible then exit;
        FText := FormattedMark(AIndex);
      if FText = '' then exit;
      if RotateLabels then
        Marks.SetAdditionalAngle(AAngle);
      p := Marks.GetLabelPolygon(ACanvas.TextExtent(FText));
      FCenter += EndPoint(AAngle, Marks.Distance + LabelExtraDist(p, AAngle));
      for i := 0 to High(p) do
        ExpandRect(Result, p[i] + FCenter);
    end;
  end;

const
  MARGIN = 4;
var
  i: Integer;
  di: PChartDataItem;
  prevAngle: Double = 0;
  a: Double;
begin
  Result.TopLeft := FCenter;
  Result.BottomRight := FCenter;
  SetLength(FSlices, Count);
  for i := 0 to Count - 1 do begin
    di := Source[i];
    with FSlices[i] do begin
      FAngle := CycleToRad(di^.Y / Source.ValuesTotal);
      FBase := FCenter;
      a := prevAngle + FAngle / 2;
      if Exploded and (di^.X > 0) then
        FBase += EndPoint(a, FRadius * di^.X);
      ExpandRect(Result, FBase, FRadius, - prevAngle, - prevAngle - FAngle);
      FLabel.FAttachment := EndPoint(a, FRadius) + FBase;
      PrepareLabel(FLabel, i, a);
      prevAngle += FAngle;
    end;
  end;
  InflateRect(Result, MARGIN, MARGIN);
end;

end.

