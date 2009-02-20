{
 /***************************************************************************
                               TASeries.pas
                               ------------
                Component Library Standard Graph Series


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

Authors: Luís Rodrigues, Philippe Martinole, Alexander Klenin

}

unit TASeries;

{$H+}

interface

uses
  Classes, Dialogs, Graphics, sysutils, TAGraph, TAChartUtils;

type

  //not completetly implemented (only TPieSeries - not all)
  TSeriesMarksStyle = (
    smsValue,          { 1234 }
    smsPercent,        { 12 % }
    smsLabel,          { Cars }
    smsLabelPercent,   { Cars 12 % }
    smsLabelValue,     { Cars 1234 }
    smsLegend,         { ? }
    smsPercentTotal,   { 12 % of 1234 }
    smsLabelPercentTotal, { Cars 12 % of 1234 }
    smsXValue);        { 21/6/1996 }

  ChartCoord = record
    x, y: Double;
    Color: TColor;
    Text: String;
  end;
  PChartCoord = ^ChartCoord;

  TSeriesPointerStyle = (
    psRectangle, psCircle, psCross, psDiagCross, psStar,
    psLowBracket, psHighBracket);

  BarException = class(Exception);

  { TChartSeries }

  TChartSeries = class(TBasicChartSeries)
  private
    // Graph = coordinates in the graph
    FXGraphMin, FYGraphMin: Double;                // Max Graph value of points
    FXGraphMax, FYGraphMax: Double;
    FCoordList: TList;
    FActive: Boolean;
    FMarks: TSeriesMarksStyle;
    FShowInLegend: Boolean;

    procedure SetActive(Value: Boolean);
    procedure SetMarks(Value: TSeriesMarksStyle);
    function GetXMinVal: Integer;
    procedure SetShowInLegend(Value: Boolean);
    procedure InitBounds(out XMin, YMin, XMax, YMax: Integer);
  protected
    procedure StyleChanged(Sender: TObject);
    property Coord: TList read FCoordList;
    procedure DrawLegend(ACanvas: TCanvas; const ARect: TRect); override;
    function GetLegendWidth(ACanvas: TCanvas): Integer; override;
    function GetLegendCount: Integer; override;
    function IsInLegend: Boolean; override;
    procedure UpdateBounds(
      var ANumPoints: Integer; var AXMin, AYMin, AXMax, AYMax: Double); override;
    procedure UpdateParentChart;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property XGraphMin: Double read FXGraphMin write FXGraphMin;
    property YGraphMin: Double read FYGraphMin write FYGraphMin;
    property XGraphMax: Double read FXGraphMax write FXGraphMax;
    property YGraphMax: Double read FYGraphMax write FYGraphMax;

    function Count: Integer; override;
    procedure Draw(ACanvas: TCanvas); virtual; abstract;
    procedure DrawIfActive(ACanvas: TCanvas); override;
    function AddXY(X, Y: Double; XLabel: String; Color: TColor): Longint; virtual;
    function Add(AValue: Double; XLabel: String; Color: TColor): Longint; virtual;
    procedure Delete(AIndex: Integer); virtual;
    procedure Clear;

  published
    property Active: Boolean read FActive write SetActive default true;
    property MarksStyle: TSeriesMarksStyle
      read FMarks write SetMarks default smsLabel; //this should be an object
    property ShowInLegend: Boolean
      read FShowInLegend write SetShowInLegend default true;
    property Title;
  end;

  TSeriesPointer = class(TPersistent)
  private
    FHorizSize, FVertSize: Integer;
    FStyle: TSeriesPointerStyle;
    FPen: TChartPen;
    FBrush: TBrush;
    FVisible: Boolean;
    FOwner: TChartSeries;
    FChanged: TNotifyEvent;

    procedure SetVisible(Value: Boolean);
    procedure SetStyle(Value: TSeriesPointerStyle);
    procedure SetPen(Value: TChartPen);
    procedure SetBrush(Value: TBrush);
    procedure SetHorizSize(Value: Integer);
    procedure SetVertSize(Value: Integer);
  protected
  public
    constructor Create(AOwner: TChartSeries);
    destructor Destroy; override;
    procedure Draw(ACanvas: TCanvas; px, py: Integer; AColor: TColor = clTAColor);

    property ParentSeries: TChartSeries read FOwner;
    procedure Assign(Source: TPersistent); override;
  published
    property Brush: TBrush read FBrush write SetBrush;
    property HorizSize: Integer read FHorizSize write SetHorizSize default 4;
    property OnChange: TNotifyEvent read FChanged write FChanged;
    property Pen: TChartPen read FPen write SetPen;
    property Style: TSeriesPointerStyle read FStyle write SetStyle default psRectangle;
    property VertSize: Integer read FVertSize write SetVertSize default 4;
    property Visible: Boolean read FVisible write SetVisible;
  end;

  { TBarSeries }

  TBarSeries = class(TChartSeries)
  private
    FBarBrush: TBrush;
    FBarPen: TPen;
    FBarWidthPercent: Integer;

    procedure SetBarWidthPercent(Value: Integer);
    procedure SetBarBrush(Value: TBrush);
    procedure SetBarPen(Value: TPen);
    procedure ExamineAllBarSeries(out ATotalNumber, AMyPos: Integer);
  protected
    procedure DrawLegend(ACanvas: TCanvas; const ARect: TRect); override;
    function GetSeriesColor: TColor; override;
    procedure SetSeriesColor(const AValue: TColor); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Draw(ACanvas: TCanvas); override;
    function AddXY(X, Y: Double; XLabel: String; Color: TColor): Longint; override;
  published
    property BarBrush: TBrush read FBarBrush write SetBarBrush;
    property BarPen: TPen read FBarPen write SetBarPen;
    property BarWidthPercent: Integer
      read FBarWidthPercent write SetBarWidthPercent default 70;
    property SeriesColor;
  end;

  { TPieSeries }

  TPieSeries = class(TChartSeries)
  private
    ColorIndex: Integer;
    FMiscColors: array [1..3] of TColor;
    function GetMiscColor(AIndex: integer): TColor;
    procedure SetMiscColor(AIndex: integer; const AValue: TColor);
  protected
    procedure DrawLegend(ACanvas: TCanvas; const ARect: TRect); override;
    function GetLegendCount: Integer; override;
    function GetLegendWidth(ACanvas: TCanvas): Integer; override;
    procedure AfterAdd; override;
    function GetSeriesColor: TColor; override;
    procedure SetSeriesColor(const AValue: TColor); override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Draw(ACanvas: TCanvas); override;
    function AddXY(X, Y: Double; XLabel: String; Color: TColor): Longint; override;
    function AddPie(Value: Double; Text: String; Color: TColor): Longint;
  published
    property LabelTextColor: TColor index 1
      read GetMiscColor write SetMiscColor default clBlack;
    property LabelBackgroundColor: TColor index 2
      read GetMiscColor write SetMiscColor default clYellow;
    property LabelToPieLinkColor: TColor index 3
      read GetMiscColor write SetMiscColor default clWhite;
  end;

  { TAreaSeries }

  TAreaSeries = class(TChartSeries)
  private
    FAreaLinesPen: TChartPen;
    FAreaBrush: TBrush;
    FStairs: Boolean;
    FInvertedStairs: Boolean;

    procedure SetAreaBrush(Value: TBrush);
    procedure SetStairs(Value: Boolean);
    procedure SetInvertedStairs(Value: Boolean);
  protected
    procedure DrawLegend(ACanvas: TCanvas; const ARect: TRect); override;
    function GetSeriesColor: TColor; override;
    procedure SetSeriesColor(const AValue: TColor); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure Draw(ACanvas: TCanvas); override;
    function AddXY(X, Y: Double; XLabel: String; Color: TColor): Longint; override;
  published
    property AreaLinesPen: TChartPen read FAreaLinesPen write FAreaLinesPen;
    property AreaBrush: TBrush read FAreaBrush write SetAreaBrush;
    property InvertedStairs: Boolean read FInvertedStairs write SetInvertedStairs;
    property SeriesColor;
    property Stairs: Boolean read FStairs write SetStairs;
  end;

  { TBasicLineSeries }

  TBasicLineSeries  = class(TChartSeries)
  protected
    procedure DrawLegend(ACanvas: TCanvas; const ARect: TRect); override;
  end;

  { TSerie }

  TSerie = class(TBasicLineSeries)
  private
    FPointer: TSeriesPointer;
    FStyle: TPenStyle;
    FSeriesColor: TColor;

    XOfYGraphMin, XOfYGraphMax: Double;          // X max value of points
    FShowPoints: Boolean;
    FShowLines: Boolean;
    UpdateInProgress: Boolean;

    procedure SetShowPoints(Value: Boolean);
    procedure SetShowLines(Value: Boolean);
    procedure SetPointer(Value: TSeriesPointer);
  protected
    function GetNearestPoint(
      ADistFunc: TPointDistFunc; const APoint: TPoint;
      out AIndex: Integer; out AImg: TPoint; out AValue: TDoublePoint): Boolean;
      override;
    function GetSeriesColor: TColor; override;
    procedure SetSeriesColor(const AValue: TColor); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure Draw(ACanvas: TCanvas); override;
    function  AddXY(X, Y: Double; XLabel: String; Color: TColor): Longint; override;
    function  GetXValue(AIndex: Integer): Double;
    function  GetYValue(AIndex: Integer): Double;
    procedure SetXValue(AIndex: Integer; Value: Double);
    procedure SetYValue(AIndex: Integer; Value: Double);
    function  GetXImgValue(AIndex: Integer): Integer;
    function  GetYImgValue(AIndex: Integer): Integer;
    procedure GetMin(var X, Y: Double);
    procedure GetMax(var X, Y: Double);
    function  GetXMin: Double;
    function  GetXMax: Double;
    function  GetYMin: Double;
    function  GetYMax: Double;
    procedure SetColor(AIndex: Integer; AColor: TColor);
    function  GetColor(AIndex: Integer): TColor;

    procedure BeginUpdate;
    procedure EndUpdate;

    property XGraphMin;
    property YGraphMin;
    property XGraphMax;
    property YGraphMax;
  published
    property Pointer: TSeriesPointer read FPointer write SetPointer;
    property SeriesColor;
    property ShowLines: Boolean read FShowLines write SetShowLines default true;
    property ShowPoints: Boolean read FShowPoints write SetShowPoints;
  end;

  TLineStyle = (lsVertical, lsHorizontal);

  { TLine }

  TLine = class(TBasicLineSeries)
  private
    FPen: TPen;
    FPosGraph: Double;                      // Graph coordinates of line
    FStyle: TLineStyle;

    procedure SetPen(AValue: TPen);
    procedure SetPos(AValue: Double);
    procedure SetStyle(AValue: TLineStyle);
    procedure Changed;
  protected
    function GetSeriesColor: TColor; override;
    procedure SetSeriesColor(const AValue: TColor); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure Draw(ACanvas: TCanvas); override;

  published
    property LineStyle: TLineStyle read FStyle write SetStyle default lsHorizontal;
    property Pen: TPen read FPen write SetPen;
    property Position: Double read FPosGraph write SetPos;
    property SeriesColor;
  end;

implementation

uses
  GraphMath, Math, Types;

constructor TChartSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  XGraphMin := MaxDouble;
  YGraphMin := MaxDouble;
  XGraphMax := MinDouble;
  YGraphMax := MinDouble;

  FActive := true;
  FShowInLegend := true;
  FMarks := smsLabel;
  FCoordList := TList.Create;
end;

destructor TChartSeries.Destroy;
var
  i: Integer;
begin
  for i := 0 to FCoordList.Count - 1 do
    Dispose(PChartCoord(FCoordList.Items[i]));
  FCoordList.Free;
  UpdateParentChart;

  inherited Destroy;
end;

procedure TChartSeries.DrawIfActive(ACanvas: TCanvas);
begin
  if Active then
    Draw(ACanvas);
end;

procedure TChartSeries.DrawLegend(ACanvas: TCanvas; const ARect: TRect);
begin
  ACanvas.TextOut(ARect.Right + 3, ARect.Top, Title);
end;

function TChartSeries.GetLegendCount: Integer;
begin
  Result := 1;
end;

function TChartSeries.GetLegendWidth(ACanvas: TCanvas): Integer;
begin
  Result := ACanvas.TextWidth(Title);
end;

function TChartSeries.GetXMinVal: Integer;
begin
  if Count > 0 then
    Result := Round(PChartCoord(FCoordList[FCoordList.Count-1])^.x)
  else
    Result := 0;
end;

procedure TChartSeries.InitBounds(out XMin, YMin, XMax, YMax: Integer);
begin
  with ParentChart do begin
    XMin := XImageMin;
    XMax := XImageMax;
    YMin := YImageMin;
    YMax := YImageMax;
  end;

  if XMin > XMax then
    Exchange(XMin, XMax);
  if YMin > YMax then
    Exchange(YMin, YMax);
end;

function TChartSeries.IsInLegend: Boolean;
begin
  Result := Active and ShowInLegend;
end;

function TChartSeries.AddXY(X, Y: Double; XLabel: String; Color: TColor): Longint;
var
  pcc: PChartCoord;
begin
  New(pcc);
  pcc^.x := X;
  pcc^.y := Y;
  pcc^.Color := Color;
  pcc^.Text := XLabel;

  // We keep FCoordList ordered by X coordinate.
  // Note that this leads to O(N^2) time except
  // for the case of adding already ordered points.
  // So, is the user wants to add many (>10000) points to a graph,
  // he should pre-sort them to avoid performance penalty.
  Result := FCoordList.Count;
  while (Result > 0) and (PChartCoord(FCoordList.Items[Result - 1])^.x > X) do
    Dec(Result);
  FCoordList.Insert(Result, pcc);
end;

function TChartSeries.Add(AValue: Double; XLabel: String; Color: TColor): Longint;
var
  XVal: Integer;
begin
  if FCoordList.Count = 0 then
    XVal := 0
  else
    XVal := Round(PChartCoord(FCoordList.Items[FCoordList.Count - 1])^.x);
  Result := AddXY(XVal + 1, AValue, XLabel, Color);
end;


procedure TChartSeries.Delete(AIndex:Integer);
begin
  Dispose(PChartCoord(FCoordList.Items[AIndex]));
  FCoordList.Delete(AIndex);
  UpdateParentChart;
end;

procedure TChartSeries.Clear;
begin
  FCoordList.Clear;

  XGraphMin := MaxDouble;
  YGraphMin := MaxDouble;
  XGraphMax := MinDouble;
  YGraphMax := MinDouble;

  UpdateParentChart;
end;

function TChartSeries.Count:Integer;
begin
  Result := FCoordList.Count;
end;

procedure TChartSeries.SetActive(Value: Boolean);
begin
  FActive := Value;
  UpdateParentChart;
end;

procedure TChartSeries.SetShowInLegend(Value: Boolean);
begin
  FShowInLegend := Value;
  UpdateParentChart;
end;

procedure TChartSeries.StyleChanged(Sender: TObject);
begin
  UpdateParentChart;
end;

procedure TChartSeries.UpdateBounds(
  var ANumPoints: Integer; var AXMin, AYMin, AXMax, AYMax: Double);
begin
  if not Active or (Count = 0) then exit;
  ANumPoints += Count;
  if XGraphMin < AXMin then AXMin := XGraphMin;
  if YGraphMin < AYMin then AYMin := YGraphMin;
  if XGraphMax > AXMax then AXMax := XGraphMax;
  if YGraphMax > AYMax then AYMax := YGraphMax;
end;

procedure TChartSeries.UpdateParentChart;
begin
  if ParentChart <> nil then ParentChart.Invalidate;
end;

procedure TChartSeries.SetMarks(Value: TSeriesMarksStyle);
begin
  FMarks := Value;
  UpdateParentChart;
end;

{ TSeriesPointer }

procedure TSeriesPointer.SetVisible(Value: Boolean);
begin
  FVisible := Value;
  if Assigned(FChanged) then FChanged(Self);
end;

procedure TSeriesPointer.SetStyle(Value: TSeriesPointerStyle);
begin
  FStyle := Value;
  if Assigned(FChanged) then FChanged(Self);
end;

procedure TSeriesPointer.SetPen(Value: TChartPen);
begin
  FPen.Assign(Value);
  if Assigned(FChanged) then FChanged(Self);
end;

procedure TSeriesPointer.SetBrush(Value: TBrush);
begin
  FBrush.Assign(Value);
  if Assigned(FChanged) then FChanged(Self);
end;

procedure TSeriesPointer.SetHorizSize(Value: Integer);
begin
  FHorizSize := Value;
  if Assigned(FChanged) then FChanged(Self);
end;

procedure TSeriesPointer.SetVertSize(Value: Integer);
begin
  FVertSize := Value;
  if Assigned(FChanged) then FChanged(Self);
end;

constructor TSeriesPointer.Create(AOwner: TChartSeries);
begin
  FBrush := TBrush.Create;
  FBrush.Color := clLime;
  FPen := TChartPen.Create;
  FOwner := AOwner;

  FHorizSize := 4;
  FVertSize  := 4;
end;

destructor TSeriesPointer.Destroy;
begin
  FBrush.Free;
  FPen.Free;
  inherited Destroy;
end;

procedure TSeriesPointer.Draw(ACanvas: TCanvas; px, py: Integer; AColor: TColor);
begin
  with FOwner do begin
    if AColor = clTAColor then
      AColor := SeriesColor;
    ACanvas.Brush.Assign(FBrush);
    ACanvas.Pen.Assign(FPen);

    case FStyle of
      psRectangle: begin
        ACanvas.Brush.Color := AColor;
        ACanvas.Rectangle(px-FHorizSize,py-FVertSize,px+FHorizSize+1,py+FVertSize+1);
      end;
      psCross: begin
        ACanvas.Pen.Color := AColor;
        ACanvas.MoveTo(px-FHorizSize,py);
        ACanvas.LineTo(px+FHorizSize+1,py);
        ACanvas.MoveTo(px,py-FVertSize);
        ACanvas.LineTo(px,py+FVertSize+1);
      end;
      psDiagCross: begin
        ACanvas.Pen.Color := AColor;
        ACanvas.MoveTo(px-FHorizSize,py-FVertSize);
        ACanvas.LineTo(px+FHorizSize+1,py+FVertSize+1);
        ACanvas.MoveTo(px-FHorizSize,py+FVertSize+1);
        ACanvas.LineTo(px+FHorizSize+1,py-FVertSize);
      end;
      psStar: begin
        ACanvas.Pen.Color := AColor;
        ACanvas.MoveTo(px-FHorizSize,py);
        ACanvas.LineTo(px+FHorizSize+1,py);
        ACanvas.MoveTo(px,py-FVertSize);
        ACanvas.LineTo(px,py+FVertSize+1);

        ACanvas.MoveTo(px-FHorizSize,py-FVertSize);
        ACanvas.LineTo(px+FHorizSize+1,py+FVertSize+1);
        ACanvas.MoveTo(px-FHorizSize,py+FVertSize+1);
        ACanvas.LineTo(px+FHorizSize+1,py-FVertSize);
      end;
      psCircle: begin
        ACanvas.Brush.Color := AColor;
        ACanvas.Ellipse(px-FHorizSize,py-FVertSize,px+FHorizSize+1,py+FVertSize+1);
      end;
      psLowBracket: begin
        ACanvas.Pen.Color := AColor;
        ACanvas.MoveTo(px-FHorizSize,py);
        ACanvas.LineTo(px-FHorizSize,py+FVertSize+1);
        ACanvas.LineTo(px+FHorizSize+1,py+FVertSize+1);
        ACanvas.LineTo(px+FHorizSize+1,py-1);
      end;
      psHighBracket: begin
        ACanvas.Pen.Color := AColor;
        ACanvas.MoveTo(px-FHorizSize,py);
        ACanvas.LineTo(px-FHorizSize,py-FVertSize);
        ACanvas.LineTo(px+FHorizSize+1,py-FVertSize);
        ACanvas.LineTo(px+FHorizSize+1,py+1);
      end;
    end;
  end;
end;

procedure TSeriesPointer.Assign(Source: TPersistent);
begin
  if Source is TSeriesPointer then
    with TSeriesPointer(Source) do begin
      FBrush.Assign(Brush);
      FPen.Assign(Pen);
      FStyle := Style;
      FVisible := Visible;
    end;
  inherited Assign(Source);
end;

{ TSerie }

constructor TSerie.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPointer := TSeriesPointer.Create(Self);
  FPointer.FStyle := psCross;
  FPointer.OnChange := @StyleChanged;

  FStyle := psSolid;

  ShowPoints := false;
  ShowLines := true;

  UpdateInProgress := false;
end;

destructor TSerie.Destroy;
begin
  FPointer.Free;
  inherited Destroy;
end;

procedure TSerie.SetPointer(Value: TSeriesPointer);
begin
  FPointer.Assign(Value);
  UpdateParentChart;
end;

procedure TSerie.SetSeriesColor(const AValue: TColor);
begin
  FSeriesColor := AValue;
end;

procedure TSerie.Draw(ACanvas: TCanvas);
var
  xi1, yi1, xi2, yi2: Integer;
  xg1, yg1, xg2, yg2: Double;
  XMin, XMax, YMin, YMax: Integer;
  chartCoord: PChartCoord;

  function PrepareLine: Boolean;
  begin
    Result := false;
    if not FShowLines then exit;

    with ParentChart do
      if // line is totally outside the viewport
        (xg1 < XGraphMin) and (xg2 < XGraphMin) or
        (xg1 > XGraphMax) and (xg2 > XGraphMax) or
        (yg1 < YGraphMin) and (yg2 < YGraphMin) or
        (yg1 > YGraphMax) and (yg2 > YGraphMax)
      then
        exit;

    Result := true;
    with ParentChart do
      if // line is totally inside the viewport
        InRange(xg1, XGraphMin, XGraphMax) and
        InRange(xg2, XGraphMin, XGraphMax) and
        InRange(yg1, YGraphMin, YGraphMax) and
        InRange(yg2, YGraphMin, YGraphMax)
      then
        exit;

    if yg1 > yg2 then begin
      Exchange(xg1, xg2);
      Exchange(yg1, yg2);
    end;

    if yg1 = yg2 then begin
      if xg1 > xg2 then begin
        Exchange(xg1, xg2);
        Exchange(yg1, yg2);
      end;
      if xg1 < ParentChart.XGraphMin then xi1 := ParentChart.XImageMin;
      if xg2 > ParentChart.XGraphMax then xi2 := ParentChart.XImageMax;
    end
    else if xg1 = xg2 then begin
      if yg1 < ParentChart.YGraphMin then yi1 := ParentChart.YImageMin;
      if yg2 > ParentChart.YGraphMax then yi2 := ParentChart.YImageMax;
    end
    else if ParentChart.LineInViewPort(xg1, yg1, xg2, yg2) then begin
      ParentChart.GraphToImage(xg1, yg1, xi1, yi1);
      ParentChart.GraphToImage(xg2, yg2, xi2, yi2);
    end
    else
      Result := false;
  end;

  procedure DrawPoint;
  begin
    if
      FShowPoints and InRange(yi1, YMin, YMax) and InRange(xi1, XMin, XMax)
    then
      FPointer.Draw(ACanvas, xi1, yi1, SeriesColor);
  end;

  procedure GetCoords(
    AIndex: Integer; out AX, AY: Double; out AXScr, AYScr: Integer);
  begin
    chartCoord := FCoordList.Items[AIndex];
    AX := chartCoord^.x;
    AY := chartCoord^.y;
    ParentChart.GraphToImage(AX, AY, AXScr, AYScr);
  end;

var
  i: Integer;
begin
  if Count = 0 then exit;

  InitBounds(XMin, YMin, XMax, YMax);
  ACanvas.Pen.Mode := pmCopy;
  ACanvas.Pen.Width := 1;

  for i := 0 to Count - 2 do begin
    GetCoords(i, xg1, yg1, xi1, yi1);
    GetCoords(i + 1, xg2, yg2, xi2, yi2);

    if PrepareLine then begin
      ACanvas.Pen.Style := FStyle;
      ACanvas.Pen.Color := chartCoord^.Color;
      ACanvas.MoveTo(xi1, yi1);
      ACanvas.LineTo(xi2, yi2);
    end;
    DrawPoint;
  end;

  // Draw last point
  GetCoords(Count - 1, xg1, yg1, xi1, yi1);
  DrawPoint;
end;


function TSerie.AddXY(X, Y: Double; XLabel: String; Color: TColor): Longint;
begin
  if Color = clTAColor then
    Color := SeriesColor;

  Result := inherited AddXY(X, Y, XLabel, Color);

  // Update max
  if X > XGraphMax then XGraphMax := X;
  if X < XGraphMin then XGraphMin := X;
  if Y > YGraphMax then begin
    YGraphMax := Y;
    XOfYGraphMax := X;
  end;
  if Y < YGraphMin then begin
    YGraphMin := Y;
    XOfYGraphMin := X;
  end;

  UpdateParentChart;
end;

function TSerie.GetXValue(AIndex: Integer): Double;
begin
  Result := PChartCoord(FCoordList.Items[AIndex])^.x;
end;

function TSerie.GetYValue(AIndex: Integer): Double;
begin
  Result := PChartCoord(FCoordList.Items[AIndex])^.y;
end;

procedure TSerie.SetXValue(AIndex: Integer; Value: Double);
var
  i: Integer;
  Val: Double;
begin
  if not UpdateInProgress then begin
     if Value < XGraphMin then XGraphMin := Value
     else if Value > XGraphMax then XGraphMax := Value
     else begin
       if PChartCoord(FCoordList.Items[AIndex])^.x = XGraphMax then begin
         PChartCoord(FCoordList.Items[AIndex])^.x := Value;
         if Value < XGraphMax then begin
           XGraphMax := MinDouble;
           for i := 0 to FCoordList.Count - 1 do begin
             Val := PChartCoord(FCoordList.Items[AIndex])^.x;
             if Val > XGraphMax then XGraphMax := Val;
           end;
         end;
       end
       else if PChartCoord(FCoordList.Items[AIndex])^.x = XGraphMin then begin
         PChartCoord(FCoordList.Items[AIndex])^.x := Value;
         if Value > XGraphMin then begin
           XGraphMin := MaxDouble;
           for i := 0 to FCoordList.Count - 1 do begin
             Val := PChartCoord(FCoordList.Items[AIndex])^.x;
             if Val < XGraphMin then XGraphMin := Val;
           end;
         end;
       end;
     end;
  end;

  PChartCoord(FCoordList.Items[AIndex])^.x := Value;

  UpdateParentChart;
end;

procedure TSerie.SetYValue(AIndex: Integer; Value: Double);
var
  i: Integer;
  Val: Double;
begin
  if not UpdateInProgress then begin
    if Value<YGraphMin then YGraphMin:=Value
    else if Value>YGraphMax then YGraphMax:=Value
    else begin
      if PChartCoord(FCoordList.Items[AIndex])^.y=YGraphMax then begin
        PChartCoord(FCoordList.Items[AIndex])^.y:=Value;
        if Value<YGraphMax then begin
          YGraphMax:=MinDouble;
          for i:=0 to FCoordList.Count-1 do begin
            Val:=PChartCoord(FCoordList.Items[AIndex])^.y;
            if Val>YGraphMax then YGraphMax:=Val;
          end;
        end;
      end
      else if PChartCoord(FCoordList.Items[AIndex])^.y=YGraphMin then begin
        PChartCoord(FCoordList.Items[AIndex])^.y:=Value;
        if Value>YGraphMin then begin
          YGraphMin:=MaxDouble;
          for i:=0 to FCoordList.Count-1 do begin
            Val:=PChartCoord(FCoordList.Items[AIndex])^.y;
            if Val<YGraphMin then YGraphMin:=Val;
          end;
        end;
      end;
    end;
  end;

  PChartCoord(FCoordList.Items[AIndex])^.y := Value;

  UpdateParentChart;
end;

function TSerie.GetXImgValue(AIndex: Integer): Integer;
begin
  ParentChart.XGraphToImage(PChartCoord(FCoordList.Items[AIndex])^.x, Result);
end;

function TSerie.GetYImgValue(AIndex: Integer): Integer;
begin
  ParentChart.YGraphToImage(PChartCoord(FCoordList.Items[AIndex])^.y, Result);
end;

function TSerie.GetXMin: Double;
begin
  Result := XGraphMin;
end;

function TSerie.GetXMax: Double;
begin
  Result := XGraphMax;
end;

function TSerie.GetYMin: Double;
begin
  Result := YGraphMin;
end;

function TSerie.GetYMax: Double;
begin
  Result := YGraphMax;
end;

procedure TSerie.GetMax(var X, Y: Double);
begin
  X := XOfYGraphMax;
  Y := YGraphMax;
end;

procedure TSerie.GetMin(var X, Y: Double);
begin
  X := XOfYGraphMin;
  Y := YGraphMin;
end;

function TSerie.GetNearestPoint(
  ADistFunc: TPointDistFunc; const APoint: TPoint;
  out AIndex: Integer; out AImg: TPoint; out AValue: TDoublePoint): Boolean;
var
  dist, minDist, i: Integer;
  pt: TPoint;
begin
  Result := Count > 0;
  minDist := MaxInt;
  for i := 0 to Count - 1 do begin
    pt := Point(GetXImgValue(i), GetYImgValue(i));
    dist := ADistFunc(APoint, pt);
    if dist >= minDist then
      Continue;
    minDist := dist;
    AIndex := i;
    AImg := pt;
    AValue.X := GetXValue(i);
    AValue.Y := GetYValue(i);
  end;
end;

function TSerie.GetSeriesColor: TColor;
begin
  Result := FSeriesColor;
end;

procedure TSerie.SetColor(AIndex: Integer; AColor: TColor);
begin
  PChartCoord(FCoordList.items[AIndex])^.Color := AColor;
end;

function TSerie.GetColor(AIndex: Integer): TColor;
begin
  Result := PChartCoord(FCoordList.items[AIndex])^.Color;
end;

procedure TSerie.SetShowPoints(Value: Boolean);
begin
  FShowPoints := Value;
  UpdateParentChart;
end;

procedure TSerie.SetShowLines(Value: Boolean);
begin
  FShowLines := Value;
  UpdateParentChart;
end;

procedure TSerie.BeginUpdate;
begin
  UpdateInProgress := true;
end;

procedure TSerie.EndUpdate;
var
  i: Integer;
  Val: Double;
begin
  UpdateInProgress := false;

  XGraphMax := MinDouble;
  XGraphMin := MaxDouble;
  for i := 0 to Count - 1 do begin
    Val := PChartCoord(FCoordList.Items[i])^.x;
    if Val > XGraphMax then XGraphMax := Val;
    if Val < XGraphMin then XGraphMin := Val;
  end;

  YGraphMax := MinDouble;
  YGraphMin := MaxDouble;
  for i:=0 to Count-1 do begin
    Val := PChartCoord(FCoordList.Items[i])^.y;
    if Val > YGraphMax then YGraphMax := Val;
    if Val < YGraphMin then YGraphMin := Val;
  end;

  UpdateParentChart;
end;

{ TLine }

procedure TLine.Changed;
begin
  //FIXME: not the best way of doing this
  {if Visible then begin
     NBPointsMax:=NBPointsMax+1;
     case LineStyle of
        lsHorizontal:
           begin
           if Position<YMinSeries then YMinSeries:=Position;
           if Position>YMaxSeries then YMaxSeries:=Position;
           end;
        lsVertical:
           begin
           if Position<XMinSeries then XMinSeries:=Position;
           if Position>XMaxSeries then XMaxSeries:=Position;
           end;
        end;
     end;
  end;}
  case LineStyle of
    lsHorizontal: begin YGraphMin := FPosGraph; YGraphMax := FPosGraph; end;
    lsVertical: begin XGraphMin := FPosGraph; XGraphMax := FPosGraph; end;
  end;
  UpdateParentChart;
end;

constructor TLine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPen := TPen.Create;
  FPen.OnChange := @StyleChanged;
  LineStyle := lsHorizontal;
end;

destructor TLine.Destroy;
begin
  inherited Destroy;
  FPen.Free;
end;

procedure TLine.SetPen(AValue: TPen);
begin
  FPen.Assign(AValue);
end;

procedure TLine.SetStyle(AValue: TLineStyle);
begin
  if FStyle = AValue then exit;
  FStyle := AValue;
  Changed;
end;

procedure TLine.SetPos(AValue: Double);
begin
  if FPosGraph = AValue then exit;
  FPosGraph := AValue;
  Changed;
end;

procedure TLine.SetSeriesColor(const AValue: TColor);
begin
  FPen.Color := AValue;
end;

procedure TLine.Draw(ACanvas: TCanvas);
var
  xmin, xmax, ymin, ymax, posImage: Integer;
begin
  InitBounds(xmin, ymin, xmax, ymax);

  ACanvas.Pen.Assign(FPen);

  case LineStyle of
    lsHorizontal:
      if InRange(FPosGraph, ParentChart.XGraphMin, ParentChart.XGraphMax) then begin
        ParentChart.YGraphToImage(FPosGraph, posImage);
        ACanvas.MoveTo(xmin, posImage);
        ACanvas.LineTo(xmax, posImage);
      end;
    lsVertical:
      if InRange(FPosGraph, ParentChart.YGraphMin, ParentChart.YGraphMax) then begin
        ParentChart.XGraphToImage(FPosGraph, posImage);
        ACanvas.MoveTo(posImage, ymin);
        ACanvas.LineTo(posImage, ymax);
      end;
  end;
end;

function TLine.GetSeriesColor: TColor;
begin
  Result := FPen.Color;
end;

{ TBarSeries }

constructor TBarSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBarWidthPercent := 70; //70%

  FBarBrush := TBrush.Create;
  FBarBrush.OnChange := @StyleChanged;

  FBarPen := TPen.Create;
  FBarPen.OnChange := @StyleChanged;
  FBarPen.Mode := pmCopy;
  FBarPen.Style := psSolid;
  FBarPen.Width := 1;
  FBarPen.Color := clBlack;
  FBarBrush.Color := clRed;
end;

destructor TBarSeries.Destroy;
begin
  FBarPen.Free;
  FBarBrush.Free;
  inherited Destroy;
end;

procedure TBarSeries.SetBarBrush(Value: TBrush);
begin
  FBarBrush.Assign(Value);
end;

procedure TBarSeries.SetBarPen(Value:TPen);
begin
  FBarPen.Assign(Value);
end;

procedure TBarSeries.SetBarWidthPercent(Value: Integer);
begin
  if (Value < 1) or (Value > 100) then
    raise BarException.Create('Wrong BarWidth Percent')
  else
    FBarWidthPercent := Value;
end;

procedure TBarSeries.SetSeriesColor(const AValue: TColor);
begin
  FBarBrush.Color := AValue;
end;

function TBarSeries.AddXY(X, Y: Double; XLabel: String; Color: TColor): Longint;
begin
  if Color = clTAColor then Color := SeriesColor;

  Result := inherited AddXY(X, Y, XLabel, Color);

  //update the interval - the 0.6 is a hack to allow the bars to have some space apart
  if X > XGraphMax - 0.6 then XGraphMax := X + 0.6;
  if X < XGraphMin + 0.6 then XGraphMin := X - 0.6;
  //check if the bar is abouve 0 or not
  if Y >= 0 then begin
    if Y > YGraphMax then YGraphMax := Y;
    if YGraphMin > 0 then YGraphMin := 0;
  end else begin
    if Y < YGraphMin then YGraphMin := Y;
    if YGraphMax < 0 then YGraphMax := 0;
  end;

  UpdateParentChart;
end;

procedure TBarSeries.Draw(ACanvas: TCanvas);
var
  XMin, XMax: Integer;
  i: Integer;
  graphCoordTop: ChartCoord;
  graphCoordBottom: ChartCoord;
  topX, topY, bottomY: Integer;
  barWidth, totalbarWidth, totalBarSeries, myPos: Integer;
  bx1, by1, bx2, by2: Integer;

  function BarInViewPort(cTop, cBottom: ChartCoord): Boolean;
  begin //FIXME make cleaner?
    Result :=
      ((cTop.x >= ParentChart.XGraphMin) and (cTop.x <= ParentChart.XGraphMax)) and
      ( ((cTop.y > ParentChart.YGraphMax) and (cBottom.y < ParentChart.YGraphMin))
        or ( (cTop.y < ParentChart.YGraphMax) and (cTop.y > ParentChart.YGraphMin))
        or ( (cBottom.y < ParentChart.YGraphMax) and (cBottom.y > ParentChart.YGraphMin))
      );
  end;

begin
  //no elements to draw
  if FCoordList.Count = 0 then exit;

  //get the limits (for the zoom) ??
  XMin := ParentChart.XImageMin;
  XMax := ParentChart.XImageMax;
  if XMin > XMax then
    Exchange(XMin, XMax);

  // Draw the bars
  ACanvas.Pen.Assign(FBarPen);
  ACanvas.Brush.Assign(FBarBrush);

  //calc the single bar width
  totalbarWidth :=
    Round((FBarWidthPercent * 0.01) * ParentChart.ChartWidth / FCoordList.Count);
  ExamineAllBarSeries(totalBarSeries, myPos);
  barWidth := totalbarWidth div totalBarSeries;

  for i := 0 to FCoordList.Count - 1 do begin
    //get the top and bottom points
    if ChartCoord(FCoordList.Items[i]^).y >= 0 then begin
      graphCoordTop := ChartCoord(FCoordList.Items[i]^);
      graphCoordBottom.x := graphCoordTop.x;
      graphCoordBottom.y := 0;
    end else begin
      graphCoordBottom := ChartCoord(FCoordList.Items[i]^);
      graphCoordTop.x := graphCoordBottom.x;
      graphCoordTop.y := 0;
    end;

    //check if bar in view port
    if BarInViewPort(graphCoordTop, graphCoordBottom) then begin
      //only draw to the limits
      if graphCoordTop.y > ParentChart.YGraphMax then graphCoordTop.y := ParentChart.YGraphMax;
      if graphCoordBottom.y < ParentChart.YGraphMin then graphCoordBottom.y := ParentChart.YGraphMin;
      //convert from graph to imgs coords
      ParentChart.GraphToImage(graphCoordTop.x, graphCoordTop.y, topX, topY);
      ParentChart.YGraphToImage(graphCoordBottom.y, bottomY);

      //calc coords for bar
{     bx1 := topX-(barWidth div 2);
      by1 := topY;
      bx2 := topX+(barWidth div 2);
      by2 := bottomY;
}
      bx1 := topX - (totalbarWidth div 2) + myPos * barWidth;
      by1 := topY;
      bx2 := topX - (totalbarWidth div 2) + myPos * barWidth + barWidth;
      by2 := bottomY;

      //FIXME only draw if bar inside image coord (get a better way of doing this)
      if (bx1 >= XMin) and (bx2 <= XMax) then
        if by1 = by2 then begin //draw a line when y=0 FIXME (clean)
          ACanvas.Pen.Color := FBarBrush.Color;
          ACanvas.MoveTo(bx1, by1);
          ACanvas.LineTo(bx2, by2);
          ACanvas.Pen.Assign(FBarPen);
        end else
          ACanvas.Rectangle( bx1, by1, bx2, by2);
    end;
  end; // for
end;

procedure TBarSeries.DrawLegend(ACanvas: TCanvas; const ARect: TRect);
begin
  inherited DrawLegend(ACanvas, ARect);
  ACanvas.Pen.Color := clBlack;
  ACanvas.Brush.Assign(BarBrush);
  ACanvas.Rectangle(ARect);
end;

procedure TBarSeries.ExamineAllBarSeries(out ATotalNumber, AMyPos: Integer);
var
  i: Integer;
begin
  ATotalNumber := 0;
  AMyPos := -1;
  for i := 0 to ParentChart.SeriesCount - 1 do begin
    if ParentChart.Series[i] = Self then
      AMyPos := ATotalNumber;
    if ParentChart.Series[i] is TBarSeries then
      Inc(ATotalNumber);
  end;
  Assert(AMyPos >= 0);
end;

function TBarSeries.GetSeriesColor: TColor;
begin
  Result := FBarBrush.Color;
end;

{ TPieSeries }

function TPieSeries.AddPie(Value: Double; Text: String; Color: TColor): Longint;
begin
  Result := AddXY(getXMinVal + 1, Value, Text, Color);
end;

function TPieSeries.AddXY(X, Y: Double; XLabel: String; Color: TColor): Longint;
begin
  if Color = clTAColor then Color := Colors[ColorIndex];
  Inc(ColorIndex);
  if ColorIndex > MaxColor then ColorIndex := 1;

  Result := inherited AddXY(X, Y, XLabel, Color);

  UpdateParentChart;
end;

procedure TPieSeries.AfterAdd;
begin
  // disable axis when we have TPie series
  ParentChart.LeftAxis.Visible := false;
  ParentChart.BottomAxis.Visible := false;
end;

constructor TPieSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ColorIndex := 1;
  LabelBackgroundColor := clYellow;
  LabelToPieLinkColor := clWhite;
end;

procedure TPieSeries.Draw(ACanvas: TCanvas);
var
  i, radius: Integer;
  yTotal, prevAngle, angleStep: Double;
  graphCoord: PChartCoord;
  labelWidths, labelHeights: TIntegerDynArray;
  labelTexts: TStringDynArray;
  a, b, center: TPoint;
const
  MARGIN = 8;
  MARKS_DIST = 32;
  MarkYMargin = 2;
  MarkXMargin = 4;
begin
  if FCoordList.Count = 0 then exit;

  yTotal := 0;
  for i := 0 to FCoordList.Count - 1 do
    yTotal += PChartCoord(FCoordList[i])^.y;

  SetLength(labelWidths, FCoordList.Count);
  SetLength(labelHeights, FCoordList.Count);
  SetLength(labelTexts, FCoordList.Count);
  for i := 0 to FCoordList.Count - 1 do
    with PChartCoord(FCoordList[i])^ do begin
      case MarksStyle of
        smsLabel:
          labelTexts[i] := Text;
        smsLabelPercent:
          labelTexts[i] := Text + Format(' %1.3g%%', [y / yTotal * 100]);
      end;
      with ACanvas.TextExtent(labelTexts[i]) do begin
        labelWidths[i] := cx;
        labelHeights[i] := cy;
      end;
    end;

  with ParentChart do begin
    center.x := (XImageMin + XImageMax) div 2;
    center.y := (YImageMin + YImageMax) div 2;
    // Reserve space for labels.
    radius := Min(
      XImageMax - center.x - MaxIntValue(labelWidths),
      YImageMin - center.y - MaxIntValue(labelHeights));
  end;
  radius := Max(radius - MARKS_DIST - MARGIN, 0);

  prevAngle := 0;
  for i := 0 to FCoordList.Count - 1 do begin
    // if y < 0 then y := -y;
    // if y = 0 then y := 0.1; // just to simulate tchart when y=0

    graphCoord := FCoordList[i];
    angleStep := graphCoord^.y / yTotal * 360 * 16;
    ACanvas.Brush.Color := graphCoord^.Color;

    ACanvas.RadialPie(
      center.x - radius, center.y - radius,
      center.x + radius, center.y + radius, round(prevAngle), round(angleStep));

    a := LineEndPoint(center, prevAngle + angleStep / 2, radius);
    b := LineEndPoint(center, prevAngle + angleStep / 2, radius + MARKS_DIST);

    // line from mark to pie
    ACanvas.Pen.Color := LabelToPieLinkColor;
    ACanvas.MoveTo(a.x, a.y);
    ACanvas.LineTo(b.x, b.y);

    if b.x < center.x then
      b.x -= labelWidths[i];
    if b.y < center.y then
      b.y -= labelHeights[i];

    ACanvas.Pen.Color := LabelTextColor;
    ACanvas.Brush.Color := LabelBackgroundColor;
    ACanvas.Rectangle(
      b.x - MarkXMargin, b.y - MarkYMargin,
      b.x + labelWidths[i] + MarkXMargin, b.y + labelHeights[i] + MarkYMargin);
    ACanvas.TextOut(b.x, b.y, labelTexts[i]);

    prevAngle += angleStep;
  end;
end;

procedure TPieSeries.DrawLegend(ACanvas: TCanvas; const ARect: TRect);
var
  i: Integer;
  pc, bc: TColor;
  r: TRect;
begin
  r := ARect;
  pc := ACanvas.Pen.Color;
  bc := ACanvas.Brush.Color;
  for i := 0 to Count - 1 do begin
    ACanvas.Pen.Color := pc;
    ACanvas.Brush.Color := bc;
    with PChartCoord(Coord.Items[i])^ do begin
      ACanvas.TextOut(r.Right + 3, r.Top, Format('%1.2g %s', [y, Text]));
      ACanvas.Pen.Color := clBlack;
      ACanvas.Brush.Color := Color;
    end;
    ACanvas.Rectangle(r);
    OffsetRect(r, 0, r.Bottom - r.Top + LEGEND_SPACING);
  end;
end;

function TPieSeries.GetLegendCount: Integer;
begin
  Result := Count;
end;

function TPieSeries.GetLegendWidth(ACanvas: TCanvas): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    with PChartCoord(Coord.Items[i])^ do
      Result := Max(ACanvas.TextWidth(Format('%1.2g %s', [y, Text])), Result);
end;

function TPieSeries.GetMiscColor(AIndex: integer): TColor;
begin
  Result := FMiscColors[AIndex];
end;

function TPieSeries.GetSeriesColor: TColor;
begin
  Result := clBlack; // SeriesColor is meaningless for PieSeries
end;

procedure TPieSeries.SetMiscColor(AIndex: integer; const AValue: TColor);
begin
  if FMiscColors[AIndex] = AValue then exit;
  FMiscColors[AIndex] := AValue;
  UpdateParentChart;
end;


procedure TPieSeries.SetSeriesColor(const AValue: TColor);
begin
  // SeriesColor is meaningless for PieSeries
end;

{ TAreaSeries }

constructor TAreaSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAreaLinesPen := TChartPen.Create;
  FAreaBrush := TBrush.Create;
end;

destructor TAreaSeries.Destroy;
begin
  FAreaLinesPen.Free;
  FAreaBrush.Free;
  inherited Destroy;
end;

function TAreaSeries.AddXY(X, Y: Double; XLabel: String; Color: TColor): Longint;
begin
  if Color = clTAColor then Color := SeriesColor;

  Result := inherited AddXY(X, Y, XLabel, Color);

  // Update max
  if X > XGraphMax then XGraphMax := X;
  if X < XGraphMin then XGraphMin := X;
  if Y > YGraphMax then YGraphMax := Y;
  if Y < YGraphMin then YGraphMin := Y;

  UpdateParentChart;
end;

procedure TAreaSeries.SetAreaBrush(Value: TBrush);
begin
  FAreaBrush.Assign(Value);
  UpdateParentChart;
end;

procedure TAreaSeries.SetStairs(Value: Boolean);
begin
  FStairs := Value;
  UpdateParentChart;
end;

procedure TAreaSeries.SetInvertedStairs(Value: Boolean);
begin
  FInvertedStairs := Value;
  UpdateParentChart;
end;

procedure TAreaSeries.SetSeriesColor(const AValue: TColor);
begin
  FAreaBrush.Color := AValue;
end;

procedure TAreaSeries.Draw(ACanvas: TCanvas);
var
  i: Integer;
  xi1, yi1, xi2, yi2, xi2a: Integer;
  xg1, yg1, xg2, yg2: Double;
  XMin, XMax, YMin, YMax: Integer;
  graphCoord: PChartCoord;
  iy_min: Integer;

  procedure DrawPart;
  begin
    ACanvas.Polygon([
      Point(xi1, iy_min), Point(xi1, yi1), Point(xi2, yi2), Point(xi2, iy_min)]);
  end;

begin
  if Count = 0 then exit;

  InitBounds(XMin, YMin, XMax, YMax);

  with ParentChart do begin
    Canvas.Pen.Mode := pmCopy;
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Width := 1;
  end;

  for i := 0 to Count - 2 do begin
    graphCoord := FCoordList.Items[i];
    xg1 := graphCoord^.x;
    yg1 := graphCoord^.y;
    ParentChart.GraphToImage(xg1, yg1, xi1, yi1);
    graphCoord := FCoordList.Items[i + 1];
    xg2 := graphCoord^.x;
    yg2 := graphCoord^.y;
    ParentChart.GraphToImage(xg2, yg2, xi2, yi2);

    ParentChart.YGraphToImage(ParentChart.YGraphMin, iy_min);
    ACanvas.Pen.Color:= clBlack;
    ACanvas.Brush.Color:= graphCoord^.Color;

    with ParentChart do
      if // top line is totally inside the viewport
        InRange(xg1, XGraphMin, XGraphMax) and
        InRange(xg2, XGraphMin, XGraphMax) and
        InRange(yg1, YGraphMin, YGraphMax) and
        InRange(yg2, YGraphMin, YGraphMax)
    then begin
      if FStairs then begin
        if FInvertedStairs then
          ACanvas.Polygon([
            Point(xi1, iy_min), Point(xi1, yi2), Point(xi2, yi2), Point(xi2, iy_min)])
        else
          ACanvas.Polygon([
            Point(xi1, iy_min), Point(xi1, yi1), Point(xi2, yi1), Point(xi2, iy_min)])
      end else
        DrawPart;
      continue;
    end;

    with ParentChart do
      if // top line is totally outside the viewport
        (xg1 < XGraphMin) and (xg2 < XGraphMin) or
        (xg1 > XGraphMax) and (xg2 > XGraphMax) or
        (yg1 < YGraphMin) and (yg2 < YGraphMin)
      then
        continue;

    if yg1 > yg2 then begin
      Exchange(xg1, xg2); Exchange(yg1, yg2);
      Exchange(xi1, xi2); Exchange(yi1, yi2);
    end;

    if yg1 = yg2 then begin
      if xg1 > xg2 then begin
        Exchange(xg1, xg2);
        Exchange(yg1, yg2);
      end;
      if xg1 < ParentChart.XGraphMin then xi1 := ParentChart.XImageMin;
      if xg2 > ParentChart.XGraphMax then xi2 := ParentChart.XImageMax;
    end
    else if xg1 = xg2 then begin
      if yg1 < ParentChart.YGraphMin then yi1 := ParentChart.YImageMin;
      if yg2 > ParentChart.YGraphMax then yi2 := ParentChart.YImageMax;
    end
    else if ParentChart.LineInViewPort(xg1, yg1, xg2, yg2) then begin
      xi2a := xi2;
      ParentChart.GraphToImage(xg1, yg1, xi1, yi1);
      ParentChart.GraphToImage(xg2, yg2, xi2, yi2);
      if yi2 <= YMin then begin
        ACanvas.Polygon([
          Point(xi1, iy_min), Point(xi1, yi1), Point(xi2, yi2),
          Point(xi2a, YMin), Point(xi2a, iy_min)]);
        continue;
      end;
    end
    else if yg2 >= ParentChart.YGraphMax then begin
      yi1 := YMin;
      yi2 := YMin;
      xi1 := EnsureRange(xi1, XMin, XMax);
      xi2 := EnsureRange(xi2, XMin, XMax);
    end;
    DrawPart;
  end;
end;

procedure TAreaSeries.DrawLegend(ACanvas: TCanvas; const ARect: TRect);
begin
  inherited DrawLegend(ACanvas, ARect);
  ACanvas.Pen.Color := clBlack;
  ACanvas.Brush.Color := SeriesColor;
  ACanvas.Rectangle(ARect);
end;

function TAreaSeries.GetSeriesColor: TColor;
begin
  Result := FAreaBrush.Color;
end;

{ TBasicLineSeries }

procedure TBasicLineSeries.DrawLegend(ACanvas: TCanvas; const ARect: TRect);
var
  y: Integer;
begin
  inherited DrawLegend(ACanvas, ARect);
  ACanvas.Pen.Color := SeriesColor;
  y := (ARect.Top + ARect.Bottom) div 2;
  ACanvas.MoveTo(ARect.Left, y);
  ACanvas.LineTo(ARect.Right, y);
end;

initialization
  RegisterSeriesClass(TSerie, 'Line series');
  RegisterSeriesClass(TAreaSeries, 'Area series');
  RegisterSeriesClass(TBarSeries, 'Bar series');
  RegisterSeriesClass(TPieSeries, 'Pie series');
  RegisterSeriesClass(TLine, 'Line');

end.
