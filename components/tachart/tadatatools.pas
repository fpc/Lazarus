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

unit TADataTools;

{$H+}

interface

uses
  Classes, TAChartUtils, TAGraph, TATools, TATypes;

type
  TDataPointDistanceTool = class;

  TDataPointDistanceToolMeasureEvent =
    procedure (ASender: TDataPointDistanceTool) of object;
  TDataPointGetDistanceTextEvent =
    procedure (ASender: TDataPointDistanceTool; var AText: String) of object;

  TDataPointDistanceToolPointer = class(TSeriesPointer)
  published
    property Style default psVertBar;
  end;

  TDataPointDistanceToolMarks = class(TCustomChartMarks)
  public
    procedure Assign(ASource: TPersistent); override;
    constructor Create(AOwner: TCustomChart);
  published
    property Distance default DEF_MARKS_DISTANCE;
    property Format;
    property Frame;
    property LabelBrush;
    property LinkPen;
  end;


  TDataPointDistanceTool = class(TDataPointDrawTool)
  published
  type
    TOptions = set of (dpdoLockToData);

  strict private
    // Workaround for FPC 2.6 bug. Remove after migration to 2.8.
    FAnchors: array of TObject;
    FMarks: TDataPointDistanceToolMarks;
    FMeasureMode: TChartDistanceMode;
    FOnGetDistanceText: TDataPointGetDistanceTextEvent;
    FOnMeasure: TDataPointDistanceToolMeasureEvent;
    FOptions: TOptions;
    FPointerEnd: TDataPointDistanceToolPointer;
    FPointerStart: TDataPointDistanceToolPointer;
    function GetPointEnd: TDataPointTool.TPointRef; inline;
    function GetPointStart: TDataPointTool.TPointRef;
    procedure SetMarks(AValue: TDataPointDistanceToolMarks);
    procedure SetOptions(AValue: TOptions);
    procedure SetPointerEnd(AValue: TDataPointDistanceToolPointer);
    procedure SetPointerStart(AValue: TDataPointDistanceToolPointer);

  strict protected
    procedure DoDraw; override;
    function FindRef(APoint: TPoint; ADest: TDataPointTool.TPointRef): Boolean;
    function GetDistanceText: String;
    function SameTransformations(ASeries1, ASeries2: TBasicChartSeries): Boolean;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Distance(AUnits: TChartUnits = cuGraph): Double;
    procedure KeyDown(APoint: TPoint); override;
    procedure KeyUp(APoint: TPoint); override;
    procedure MouseDown(APoint: TPoint); override;
    procedure MouseMove(APoint: TPoint); override;
    procedure MouseUp(APoint: TPoint); override;
    property PointEnd: TDataPointTool.TPointRef read GetPointEnd;
    property PointStart: TDataPointTool.TPointRef read GetPointStart;

  published
    property DrawingMode;
    property GrabRadius default 20;
    property LinePen: TChartPen read FPen write SetPen;
    property Marks: TDataPointDistanceToolMarks read FMarks write SetMarks;
    property MeasureMode: TChartDistanceMode
      read FMeasureMode write FMeasureMode default cdmXY;
    property Options: TOptions read FOptions write SetOptions default [];
    property PointerEnd: TDataPointDistanceToolPointer
      read FPointerEnd write SetPointerEnd;
    property PointerStart: TDataPointDistanceToolPointer
      read FPointerStart write SetPointerStart;
  published
    property OnGetDistanceText: TDataPointGetDistanceTextEvent
      read FOnGetDistanceText write FOnGetDistanceText;
    property OnMeasure: TDataPointDistanceToolMeasureEvent
      read FOnMeasure write FOnMeasure;
  end;


implementation

uses
  FPCanvas, Graphics, GraphMath, Math, SysUtils, Types,
  TAChartAxis, TACustomSeries, TADrawerCanvas, TAGeometry;

const
  DEF_DISTANCE_FORMAT = '%0:.9g';

{ TDataPointDistanceToolMarks }

procedure TDataPointDistanceToolMarks.Assign(ASource: TPersistent);
begin
  if ASource is TChartMarks then
    with TDataPointDistanceToolMarks(ASource) do begin
      Self.FLabelBrush.Assign(FLabelBrush);
      Self.FLabelFont.Assign(FLabelFont);
      Self.FLinkPen.Assign(FLinkPen);
    end;
  inherited Assign(ASource);
end;

constructor TDataPointDistanceToolMarks.Create(AOwner: TCustomChart);
begin
  inherited Create(AOwner);
  FDistance := DEF_MARKS_DISTANCE;
  SetPropDefaults(FLabelBrush, ['Color']);
  Format := DEF_DISTANCE_FORMAT;
end;

{ TDataPointDistanceTool }

constructor TDataPointDistanceTool.Create(AOwner: TComponent);
begin
  inherited;
  SetLength(FAnchors, 2);
  FAnchors[0] := TDataPointTool.TPointRef.Create;
  FAnchors[1] := TDataPointTool.TPointRef.Create;
  FMarks := TDataPointDistanceToolMarks.Create(nil);
  FPointerEnd := TDataPointDistanceToolPointer.Create(nil);
  FPointerStart := TDataPointDistanceToolPointer.Create(nil);
end;

destructor TDataPointDistanceTool.Destroy;
begin
  FAnchors[0].Free;
  FAnchors[1].Free;
  FreeAndNil(FMarks);
  FreeAndNil(FPointerEnd);
  FreeAndNil(FPointerStart);
  inherited;
end;

function TDataPointDistanceTool.Distance(AUnits: TChartUnits): Double;
var
  p1, p2: TDoublePoint;
begin
  case AUnits of
    cuPercent: exit(0); // Not implemented.
    cuAxis: begin
      p1 := PointStart.AxisPos;
      p2 := PointEnd.AxisPos;
    end;
    cuGraph: begin
      p1 := PointStart.GraphPos;
      p2 := PointEnd.GraphPos;
    end;
    cuPixel: begin
      with FChart.GraphToImage(PointStart.GraphPos) do
        p1 := DoublePoint(X, Y);
      with FChart.GraphToImage(PointEnd.GraphPos) do
        p2 := DoublePoint(X, Y);
    end;
  end;
  case MeasureMode of
    cdmOnlyX: Result := Abs(p1.X - p2.X);
    cdmOnlyY: Result := Abs(p1.Y - p2.Y);
    // The user is responsible for ensuring that both axes have
    // the same physical dimensions: the xy distance is not
    // meaningful, for example, if x is in days and y in Dollars.
    cdmXY: Result := Norm([p1.X - p2.X, p1.Y - p2.Y]);
  end;
end;

procedure TDataPointDistanceTool.DoDraw;
var
  a: Double;

  procedure DrawPointer(APointer: TDataPointDistanceToolPointer; APos: TPoint);
  var
    oldMode: TFPPenMode;
    oldColor: TColor;
    oldStyle: TFPBrushStyle;
  begin
    with APointer do begin
      if not Visible then exit;
      if EffectiveDrawingMode = tdmXor then begin
        oldMode := Pen.Mode;
        oldColor := Pen.Color;
        oldStyle := Brush.Style;
        Pen.Mode := pmXor;
        Pen.Color := clWhite;
        Brush.Style := bsClear;
      end;
      try
        DrawSize(FChart.Drawer, APos, Point(HorizSize, VertSize), clTAColor, a);
      finally
        if EffectiveDrawingMode = tdmXor then begin
          Pen.Mode := oldMode;
          Pen.Color := oldColor;
          Brush.Style := oldStyle;
        end;
      end;
    end;
  end;

var
  p1, p2: TPoint;
  dummy: TPointArray = nil;
begin
  p1 := FChart.GraphToImage(PointStart.GraphPos);
  p2 := FChart.GraphToImage(PointEnd.GraphPos);
  case MeasureMode of
    cdmOnlyX: p2.Y := p1.Y;
    cdmOnlyY: p2.X := p1.X;
  end;
  if p1 = p2 then exit;
  if LinePen.Visible then
    FChart.Drawer.Line(p1, p2);
  a := ArcTan2(p2.Y - p1.Y, p2.X - p1.X);
  DrawPointer(PointerStart, p1);
  DrawPointer(PointerEnd, p2);
  if Marks.Visible then begin
    p1 := (p1 + p2) div 2;
    if EffectiveDrawingMode = tdmNormal then
      Marks.DrawLabel(FChart.Drawer, p1, p1, GetDistanceText, dummy)
    else
      DrawXorText(FChart.Canvas, p1, GetDistanceText);
  end;
  inherited;
end;

function TDataPointDistanceTool.FindRef(
  APoint: TPoint; ADest: TDataPointTool.TPointRef): Boolean;
begin
  if dpdoLockToData in Options then begin
    FindNearestPoint(APoint);
    if FSeries = nil then exit(false);
    with ADest do begin
      FGraphPos := FNearestGraphPoint;
      FIndex := PointIndex;
      FSeries := Self.FSeries;
    end;
  end
  else
    ADest.SetGraphPos(FChart.ImageToGraph(APoint));
  Result := true;
end;

// Use Marks.Format and/or OnGetDistanceText event handler to create the text
// to be displayed along the connecting line between PointStart and PointEnd.
// OnGetDistanceText is useful for converting the distance, for example, to a
// datetime string.
function TDataPointDistanceTool.GetDistanceText: String;
begin
  Result := Format(Marks.Format, [Distance]);
  if Assigned(OnGetDistanceText) then
    OnGetDistanceText(Self, Result);
end;

function TDataPointDistanceTool.GetPointEnd: TDataPointTool.TPointRef;
begin
  Result := TDataPointTool.TPointRef(FAnchors[High(FAnchors)]);
end;

function TDataPointDistanceTool.GetPointStart: TDataPointTool.TPointRef;
begin
  Result := TDataPointTool.TPointRef(FAnchors[0]);
end;

procedure TDataPointDistanceTool.KeyDown(APoint: TPoint);
begin
  MouseDown(APoint);
end;

procedure TDataPointDistanceTool.KeyUp(APoint: TPoint);
begin
  MouseUp(APoint);
end;

procedure TDataPointDistanceTool.MouseDown(APoint: TPoint);
begin
  DoHide;
  if not FindRef(APoint, PointStart) then exit;
  Activate;
  PointEnd.Assign(PointStart);
  Handled;
end;

procedure TDataPointDistanceTool.MouseMove(APoint: TPoint);
var
  newEnd: TPointRef;
begin
  if not IsActive then exit;
  DoHide;
  newEnd := TPointRef.Create;
  try
    if
      FindRef(APoint, newEnd) and
      SameTransformations(PointStart.Series, newEnd.Series)
    then
      PointEnd.Assign(newEnd);
  finally
    FreeAndNil(newEnd);
  end;
  if EffectiveDrawingMode = tdmXor then begin
    FChart.Drawer.SetXorPen(FPen);
    DoDraw;
  end;
  Handled;
end;

procedure TDataPointDistanceTool.MouseUp(APoint: TPoint);
begin
  MouseMove(APoint);
  if Assigned(OnMeasure) and (PointStart.GraphPos <> PointEnd.GraphPos) then
    OnMeasure(Self);
  Deactivate;
end;

function TDataPointDistanceTool.SameTransformations(
  ASeries1, ASeries2: TBasicChartSeries): Boolean;

  function CheckAxis(AAxisIndex1, AAxisIndex2: Integer): Boolean; inline;
  begin
    Result :=
      TransformByAxis(FChart.AxisList, AAxisIndex1) =
      TransformByAxis(FChart.AxisList, AAxisIndex2);
  end;

var
  s1: TCustomChartSeries absolute ASeries1;
  s2: TCustomChartSeries absolute ASeries2;
begin
  Result :=
    (ASeries1 = ASeries2) or
    (ASeries1 is TCustomChartSeries) and
    (ASeries2 is TCustomChartSeries) and
    ((MeasureMode = cdmOnlyY) or CheckAxis(s1.AxisIndexX, s2.AxisIndexX)) and
    ((MeasureMode = cdmOnlyX) or CheckAxis(s1.AxisIndexY, s2.AxisIndexY));
end;

procedure TDataPointDistanceTool.SetMarks(AValue: TDataPointDistanceToolMarks);
begin
  if FMarks = AValue then exit;
  FMarks.Assign(AValue);
end;

procedure TDataPointDistanceTool.SetOptions(AValue: TOptions);
begin
  if FOptions = AValue then exit;
  FOptions := AValue;
end;

procedure TDataPointDistanceTool.SetPointerEnd(
  AValue: TDataPointDistanceToolPointer);
begin
  if FPointerEnd = AValue then exit;
  FPointerEnd.Assign(AValue);
end;

procedure TDataPointDistanceTool.SetPointerStart(
  AValue: TDataPointDistanceToolPointer);
begin
  if FPointerStart = AValue then exit;
  FPointerStart.Assign(AValue);
end;

initialization

  RegisterChartToolClass(TDataPointDistanceTool, 'Distance measurement');

end.

