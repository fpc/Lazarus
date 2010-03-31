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

unit TATools;

interface

{$H+}

uses
  Classes,
  TAGraph;

type

  TChartToolset = class;

  { TChartTool }

  TChartTool = class(TBasicChartTool)
  private
    FEnabled: Boolean;
    FShift: TShiftState;
    FToolset: TChartToolset;
    procedure SetToolset(const AValue: TChartToolset);
  protected
    procedure ReadState(Reader: TReader); override;
    procedure SetParentComponent(AParent: TComponent); override;
  protected
    procedure Dispatch(
      AChart: TChart; AEventId: TChartToolEventId; APoint: TPoint);
    function Index: Integer; override;
    function IsActive: Boolean;
    procedure MouseDown(APoint: TPoint); virtual;
    procedure MouseMove(APoint: TPoint); virtual;
    procedure MouseUp(APoint: TPoint); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure Assign(Source: TPersistent); override;
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;

    property Toolset: TChartToolset read FToolset write SetToolset;
  published
    property Enabled: Boolean read FEnabled write FEnabled default true;
    property Shift: TShiftState read FShift write FShift;
  end;

  TChartToolClass = class of TChartTool;

  TChartTools = class(TFPList)
  end;

  { TChartToolset }

  TChartToolset = class(TBasicChartToolset)
  private
    FTools: TChartTools;
    function GetItem(AIndex: Integer): TChartTool;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
  public
    function Dispatch(
      AChart: TChart; AEventId: TChartToolEventId;
      AShift: TShiftState; APoint: TPoint): Boolean; override;
    property Item[AIndex: Integer]: TChartTool read GetItem; default;
  published
    property Tools: TChartTools read FTools;
  end;

  { TChartZoomDragTool }

  TChartZoomDragTool = class(TChartTool)
  private
    FSelectionRect: TRect;
  public
    procedure MouseDown(APoint: TPoint); override;
    procedure MouseMove(APoint: TPoint); override;
    procedure MouseUp(APoint: TPoint); override;
  end;

  { TChartReticuleTool }

  TChartReticuleTool = class(TChartTool)
  public
    procedure MouseMove(APoint: TPoint); override;
  end;

  procedure Register;
  procedure RegisterChartToolClass(
    AToolClass: TChartToolClass; const ACaption: String);

implementation

uses
  GraphMath, Math, Types,
  TAChartUtils;

var
  ToolsClassRegistry: TStringList;

function InitBuitlinTools(AChart: TChart): TBasicChartToolset;
var
  ts: TChartToolset;
begin
  ts := TChartToolset.Create(AChart);
  Result := ts;
  with TChartZoomDragTool.Create(AChart) do begin
    Shift := [ssLeft];
    Toolset := ts;
  end;
  TChartReticuleTool.Create(AChart).Toolset := ts;
end;

procedure Register;
begin
  RegisterComponents(CHART_COMPONENT_IDE_PAGE, [TChartToolset]);
end;

procedure RegisterChartToolClass(
  AToolClass: TChartToolClass; const ACaption: String);
begin
  ToolsClassRegistry.AddObject(ACaption, TObject(AToolClass));
end;

procedure TChartTool.Assign(Source: TPersistent);
begin
  if Source is TChartTool then
    with TChartTool(Source) do begin
      Self.FEnabled := Enabled;
      Self.FShift := Shift;
    end
  else
    inherited Assign(Source);
end;

{ TChartTool }

constructor TChartTool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := true;
end;

destructor TChartTool.Destroy;
begin
  Toolset := nil;
  inherited;
end;

procedure TChartTool.Dispatch(
  AChart: TChart; AEventId: TChartToolEventId; APoint: TPoint);
begin
  if (FChart <> nil) and (FChart <> AChart) then exit;
  FChart := AChart;
  try
    case AEventId of
      evidMouseDown: MouseDown(APoint);
      evidMouseMove: MouseMove(APoint);
      evidMouseUp  : MouseUp  (APoint);
    end;
  finally
    if not IsActive then
      FChart := nil;
  end;
end;

function TChartTool.GetParentComponent: TComponent;
begin
  Result := FToolset;
end;

function TChartTool.HasParent: Boolean;
begin
  Result := true;
end;

function TChartTool.Index: Integer;
begin
  if FToolset = nil then
    Result := -1
  else
    Result := FToolset.Tools.IndexOf(Self);
end;

function TChartTool.IsActive: Boolean;
begin
  Result := FChart.ActiveToolIndex = Index;
end;

procedure TChartTool.MouseDown(APoint: TPoint);
begin
  Unused(APoint);
end;

procedure TChartTool.MouseMove(APoint: TPoint);
begin
  Unused(APoint);
end;

procedure TChartTool.MouseUp(APoint: TPoint);
begin
  Unused(APoint);
end;

procedure TChartTool.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  if Reader.Parent is TChartToolset then
    Toolset := Reader.Parent as TChartToolset;
end;

procedure TChartTool.SetParentComponent(AParent: TComponent);
begin
  if not (csLoading in ComponentState) then
    Toolset := AParent as TChartToolset;
end;

procedure TChartTool.SetToolset(const AValue: TChartToolset);
begin
  if FToolset = AValue then exit;
  if FToolset <> nil then
    FToolset.FTools.Remove(Self);
  FToolset := AValue;
  if FToolset <> nil then
    FToolset.FTools.Add(Self);
end;

{ TChartToolset }

constructor TChartToolset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTools := TChartTools.Create;
end;

destructor TChartToolset.Destroy;
begin
  while Tools.Count > 0 do
    Item[Tools.Count - 1].Free;
  FTools.Free;
  inherited Destroy;
end;

function TChartToolset.Dispatch(
  AChart: TChart; AEventId: TChartToolEventId;
  AShift: TShiftState; APoint: TPoint): Boolean;
var
  i: Integer;
begin
  i := AChart.ActiveToolIndex;
  if InRange(i, 0, Tools.Count - 1) then begin
    Item[i].Dispatch(AChart, AEventId, APoint);
    exit(true);
  end;
  for i := 0 to Tools.Count - 1 do
    with Item[i] do
      if Enabled and (Shift = AShift) then begin
        Dispatch(AChart, AEventId, APoint);
        exit(true);
      end;
  Result := false;
end;

procedure TChartToolset.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i: Integer;
  t: TChartTool;
begin
  for i := 0 to Tools.Count - 1 do begin
    t := Item[i];
    if t.Owner = Root then
      Proc(t);
  end;
end;

function TChartToolset.GetItem(AIndex: Integer): TChartTool;
begin
  Result := TChartTool(Tools.Items[AIndex]);
end;

procedure TChartToolset.SetChildOrder(Child: TComponent; Order: Integer);
var
  i: Integer;
begin
  i := Tools.IndexOf(Child);
  if i >= 0 then
    Tools.Move(i, Order);
end;

{ TChartZoomDragTool }

procedure TChartZoomDragTool.MouseDown(APoint: TPoint);
begin
  if not FChart.AllowZoom then exit;
  Activate;
  with APoint do
    FSelectionRect := Rect(X, Y, X, Y);
end;

procedure TChartZoomDragTool.MouseMove(APoint: TPoint);
begin
  if not IsActive then exit;
  PrepareXorPen(FChart.Canvas);
  FChart.Canvas.Rectangle(FSelectionRect);
  FSelectionRect.BottomRight := APoint;
  FChart.Canvas.Rectangle(FSelectionRect);
end;

procedure TChartZoomDragTool.MouseUp(APoint: TPoint);
begin
  Unused(APoint);
  Deactivate;
  with FChart do begin
    PrepareXorPen(Canvas);
    Canvas.Rectangle(FSelectionRect);
    ZoomToRect(FSelectionRect);
  end;
end;

{ TChartReticuleTool }

procedure TChartReticuleTool.MouseMove(APoint: TPoint);
const
  DIST_FUNCS: array [TReticuleMode] of TPointDistFunc = (
    nil, @PointDistX, @PointDistY, @PointDist);
var
  i, pointIndex, bestSeries: Integer;
  value: TDoublePoint;
  newRetPos, bestRetPos: TPoint;
  d, minDist: Double;
  df: TPointDistFunc;
begin
  if FChart.ReticuleMode = rmNone then exit;
  minDist := Infinity;
  df := DIST_FUNCS[FChart.ReticuleMode];
  for i := 0 to FChart.SeriesCount - 1 do
    if
      FChart.Series[i].GetNearestPoint(df, APoint, pointIndex, newRetPos, value) and
      PtInRect(FChart.ClipRect, newRetPos)
    then begin
       d := df(APoint, newRetPos);
       if d < minDist then begin
         bestRetPos := newRetPos;
         bestSeries := i;
         minDist := d;
       end;
    end;
  if (minDist < Infinity) and (bestRetPos <> FChart.ReticulePos) then begin
    FChart.ReticulePos := bestRetPos;
    if Assigned(FChart.OnDrawReticule) then
      FChart.OnDrawReticule(FChart, bestSeries, pointIndex, value);
  end;
end;

initialization

  ToolsClassRegistry := TStringList.Create;
  OnInitBuiltinTools := @InitBuitlinTools;
  RegisterChartToolClass(TChartZoomDragTool, 'Zoom drag tool');
  RegisterChartToolClass(TChartReticuleTool, 'Reticule tool');

finalization

  ToolsClassRegistry.Free;

end.

