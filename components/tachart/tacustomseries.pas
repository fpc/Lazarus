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

const
  DEF_AXIS_INDEX = -1;

type
  { TCustomChartSeries }

  TCustomChartSeries = class(TBasicChartSeries)
  private
    FAxisIndexX: Integer;
    FAxisIndexY: Integer;
    procedure SetAxisIndexX(AValue: Integer);
    procedure SetAxisIndexY(AValue: Integer);

  protected
    procedure GetGraphBounds(var ABounds: TDoubleRect); override;
    procedure SetActive(AValue: Boolean); override;
    procedure SetDepth(AValue: TChartDistance); override;
    procedure SetShowInLegend(AValue: Boolean); override;
    procedure SetZPosition(AValue: TChartDistance); override;
    procedure StyleChanged(Sender: TObject);
    procedure UpdateParentChart;

  protected
    procedure ReadState(Reader: TReader); override;
    procedure SetParentComponent(AParent: TComponent); override;

  protected
    function GetIndex: Integer; override;
    procedure SetIndex(AValue: Integer); override;

  protected
    function AxisToGraphX(AX: Double): Double; override;
    function AxisToGraphY(AY: Double): Double; override;
    function GraphToAxisX(AX: Double): Double; override;
    function GraphToAxisY(AY: Double): Double; override;

    function IsRotated: Boolean;

  public
    constructor Create(AOwner: TComponent); override;
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;

    property AxisIndexX: Integer read FAxisIndexX write SetAxisIndexX default DEF_AXIS_INDEX;
    property AxisIndexY: Integer read FAxisIndexY write SetAxisIndexY default DEF_AXIS_INDEX;
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
    procedure GetBounds(var ABounds: TDoubleRect); override;
    function GetGraphPoint(AIndex: Integer): TDoublePoint;
    function GetGraphPointX(AIndex: Integer): Double; inline;
    function GetGraphPointY(AIndex: Integer): Double; inline;
    function GetSeriesColor: TColor; virtual;
    function GetXMaxVal: Integer;
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
    procedure Delete(AIndex: Integer); virtual;
    function Extent: TDoubleRect; virtual;
    function FormattedMark(AIndex: Integer): String;
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

uses
  Math, TAChartAxis;

{ TCustomChartSeries }

function TCustomChartSeries.AxisToGraphX(AX: Double): Double;
begin
  Result := TransformByAxis(FChart.AxisList, AxisIndexX).AxisToGraph(AX)
end;

function TCustomChartSeries.AxisToGraphY(AY: Double): Double;
begin
  Result := TransformByAxis(FChart.AxisList, AxisIndexY).AxisToGraph(AY)
end;

constructor TCustomChartSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := true;
  FShowInLegend := true;
  FAxisIndexX := DEF_AXIS_INDEX;
  FAxisIndexY := DEF_AXIS_INDEX;
end;

procedure TCustomChartSeries.GetGraphBounds(var ABounds: TDoubleRect);
begin
  GetBounds(ABounds);
  with ABounds do begin
    if not IsInfinite(a.X) then
      a.X := AxisToGraphX(a.X);
    if not IsInfinite(a.Y) then
      a.Y := AxisToGraphY(a.Y);
    if not IsInfinite(b.X) then
      b.X := AxisToGraphX(b.X);
    if not IsInfinite(b.Y) then
      b.Y := AxisToGraphY(b.Y);
    if IsRotated then begin
      Exchange(a.X, a.Y);
      Exchange(b.X, b.Y);
    end;
  end;
end;

function TCustomChartSeries.GetIndex: Integer;
begin
  Result := FChart.Series.List.IndexOf(Self);
end;

function TCustomChartSeries.GetParentComponent: TComponent;
begin
  Result := FChart;
end;

function TCustomChartSeries.GraphToAxisX(AX: Double): Double;
begin
  Result := TransformByAxis(FChart.AxisList, AxisIndexX).GraphToAxis(AX)
end;

function TCustomChartSeries.GraphToAxisY(AY: Double): Double;
begin
  Result := TransformByAxis(FChart.AxisList, AxisIndexY).GraphToAxis(AY)
end;

function TCustomChartSeries.HasParent: Boolean;
begin
  Result := true;
end;

function TCustomChartSeries.IsRotated: Boolean;
begin
  Result :=
    (AxisIndexX >= 0) and FChart.AxisList[AxisIndexX].IsVertical and
    (AxisIndexY >= 0) and not FChart.AxisList[AxisIndexY].IsVertical;
end;

procedure TCustomChartSeries.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  if Reader.Parent is TChart then
    (Reader.Parent as TChart).AddSeries(Self);
end;

procedure TCustomChartSeries.SetActive(AValue: Boolean);
begin
  if FActive = AValue then exit;
  FActive := AValue;
  UpdateParentChart;
end;

procedure TCustomChartSeries.SetAxisIndexX(AValue: Integer);
begin
  if FAxisIndexX = AValue then exit;
  FAxisIndexX := AValue;
  UpdateParentChart;
end;

procedure TCustomChartSeries.SetAxisIndexY(AValue: Integer);
begin
  if FAxisIndexY = AValue then exit;
  FAxisIndexY := AValue;
  UpdateParentChart;
end;

procedure TCustomChartSeries.SetDepth(AValue: TChartDistance);
begin
  if FDepth = AValue then exit;
  FDepth := AValue;
  UpdateParentChart;
end;

procedure TCustomChartSeries.SetIndex(AValue: Integer);
begin
  with FChart.Series.List do
    Move(Index, EnsureRange(AValue, 0, Count - 1));
end;

procedure TCustomChartSeries.SetParentComponent(AParent: TComponent);
begin
  if not (csLoading in ComponentState) then
    (AParent as TChart).AddSeries(Self);
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

  FListener := TListener.Create(@FSource,  @StyleChanged);
  FBuiltinSource := TListChartSource.Create(Self);
  FBuiltinSource.Name := BUILTIN_SOURCE_NAME;
  FBuiltinSource.Broadcaster.Subscribe(FListener);
  FMarks := TChartMarks.Create(FChart);
end;

procedure TChartSeries.Delete(AIndex: Integer);
begin
  ListSource.Delete(AIndex);
end;

destructor TChartSeries.Destroy;
begin
  FListener.Free;
  FBuiltinSource.Free;
  FMarks.Free;

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
    Result := Source.FormatItem(Marks.Format, AIndex);
end;

procedure TChartSeries.GetBounds(var ABounds: TDoubleRect);
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
  Result.X := GetGraphPointX(AIndex);
  Result.Y := GetGraphPointY(AIndex);;
  if IsRotated then
    Exchange(Result.X, Result.Y);
end;

function TChartSeries.GetGraphPointX(AIndex: Integer): Double;
begin
  Result := AxisToGraphX(Source[AIndex]^.X);
end;

function TChartSeries.GetGraphPointY(AIndex: Integer): Double;
begin
  Result := AxisToGraphY(Source[AIndex]^.Y);
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
    Source.Broadcaster.Unsubscribe(FListener);
  FSource := AValue;
  Source.Broadcaster.Subscribe(FListener);
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

