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
  Classes, Controls, Types,
  TAGraph;

type

  TChartToolset = class;
  TChartTool = class;

  TChartToolMouseEvent = procedure (ATool: TChartTool; APoint: TPoint) of object;

  { TChartTool }

  TChartTool = class(TBasicChartTool)
  private
    FActiveCursor: TCursor;
    FEnabled: Boolean;
    FMouseEvents: array [0..5] of TChartToolMouseEvent;
    FShift: TShiftState;
    FToolset: TChartToolset;
    procedure SetActiveCursor(const AValue: TCursor);
    procedure SetToolset(const AValue: TChartToolset);
  private
    FOldCursor: TCursor;
    function GetMouseEvent(AIndex: Integer): TChartToolMouseEvent;
    procedure RestoreCursor;
    procedure SetCursor;
    procedure SetMouseEvent(AIndex: Integer; AValue: TChartToolMouseEvent);
  protected
    procedure ReadState(Reader: TReader); override;
    procedure SetParentComponent(AParent: TComponent); override;
  protected
    procedure Activate; override;
    procedure Deactivate; override;
    procedure Dispatch(
      AChart: TChart; AEventId: TChartToolEventId; APoint: TPoint);
    function GetIndex: Integer; override;
    function IsActive: Boolean;
    procedure MouseDown(APoint: TPoint); virtual;
    procedure MouseMove(APoint: TPoint); virtual;
    procedure MouseUp(APoint: TPoint); virtual;
    procedure SetIndex(AValue: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure Assign(Source: TPersistent); override;
    function GetParentComponent: TComponent; override;
    procedure Handled;
    function HasParent: Boolean; override;

    property ActiveCursor: TCursor
      read FActiveCursor write SetActiveCursor default crDefault;
    property Toolset: TChartToolset read FToolset write SetToolset;
  published
    property Enabled: Boolean read FEnabled write FEnabled default true;
    property Shift: TShiftState read FShift write FShift;
  published
    property OnAfterMouseDown: TChartToolMouseEvent
      index 0 read GetMouseEvent write SetMouseEvent;
    property OnAfterMouseMove: TChartToolMouseEvent
      index 1 read GetMouseEvent write SetMouseEvent;
    property OnAfterMouseUp: TChartToolMouseEvent
      index 2 read GetMouseEvent write SetMouseEvent;
    property OnBeforeMouseDown: TChartToolMouseEvent
      index 3 read GetMouseEvent write SetMouseEvent;
    property OnBeforeMouseMove: TChartToolMouseEvent
      index 4 read GetMouseEvent write SetMouseEvent;
    property OnBeforeMouseUp: TChartToolMouseEvent
      index 5 read GetMouseEvent write SetMouseEvent;
  end;

  TChartToolClass = class of TChartTool;

  TChartTools = class(TFPList)
  end;

  { TChartToolset }

  TChartToolset = class(TBasicChartToolset)
  private
    FIsHandled: Boolean;
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

  { TZoomDragTool }

  TZoomDragTool = class(TChartTool)
  private
    FProportional: Boolean;
    FSelectionRect: TRect;
  public
    procedure MouseDown(APoint: TPoint); override;
    procedure MouseMove(APoint: TPoint); override;
    procedure MouseUp(APoint: TPoint); override;
  published
    property Proportional: Boolean read FProportional write FProportional default false;
  end;

  { TZoomClickTool }

  TZoomClickTool = class(TChartTool)
  private
    FZoomFactor: Double;
    function ZoomFactorIsStored: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure MouseDown(APoint: TPoint); override;
  published
    property ZoomFactor: Double
      read FZoomFactor write FZoomFactor stored ZoomFactorIsStored;
  end;

  TPanDirection = (pdLeft, pdUp, pdRight, pdDown);
  TPanDirectionSet = set of TPanDirection;

const
  PAN_DIRECTIONS_ALL = [Low(TPanDirection) .. High(TPanDirection)];

type
  { TPanDragTool }

  TPanDragTool = class(TChartTool)
  private
    FDirections: TPanDirectionSet;
    FOrigin: TPoint;
  public
    constructor Create(AOwner: TComponent); override;
    procedure MouseDown(APoint: TPoint); override;
    procedure MouseMove(APoint: TPoint); override;
    procedure MouseUp(APoint: TPoint); override;
  published
    property ActiveCursor default crSizeAll;
    property Directions: TPanDirectionSet
      read FDirections write FDirections default PAN_DIRECTIONS_ALL;
  end;

const
  DEF_GRAB_RADIUS = 4;

type
  { TDataPointDragTool }

  TDataPointDragTool = class(TChartTool)
  private
    FAffectedSeries: String;
    FGrabRadius: Integer;
    FPointIndex: Integer;
    FSeries: TBasicChartSeries;
    function ParseAffectedSeries: TBooleanDynArray;
  public
    constructor Create(AOwner: TComponent); override;
    procedure MouseDown(APoint: TPoint); override;
    procedure MouseMove(APoint: TPoint); override;
    procedure MouseUp(APoint: TPoint); override;
  public
    property PointIndex: Integer read FPointIndex;
    property Series: TBasicChartSeries read FSeries;
  published
    property ActiveCursor default crSizeAll;
    property AffectedSeries: String
      read FAffectedSeries write FAffectedSeries;
    property GrabRadius: Integer
      read FGrabRadius write FGrabRadius default DEF_GRAB_RADIUS;
  end;

  { TReticuleTool }

  TReticuleTool = class(TChartTool)
  public
    procedure MouseMove(APoint: TPoint); override;
  end;

  procedure Register;
  procedure RegisterChartToolClass(
    AToolClass: TChartToolClass; const ACaption: String);

resourcestring
  tasToolsEditorTitle = 'Edit tools';

implementation

uses
  ComponentEditors, Forms, GraphMath, Math, PropEdits, SysUtils,
  TAChartUtils, TASubcomponentsEditor;

{$IFOPT R+}{$DEFINE RangeChecking}{$ELSE}{$UNDEF RangeChecking}{$ENDIF}
{$IFOPT Q+}{$DEFINE OverflowChecking}{$ELSE}{$UNDEF OverflowChecking}{$ENDIF}

type
  { TToolsComponentEditor }

  TToolsComponentEditor = class(TSubComponentListEditor)
  protected
    function MakeEditorForm: TForm; override;
  public
    function GetVerb(Index: Integer): string; override;
  end;

  { TToolsPropertyEditor }

  TToolsPropertyEditor = class(TComponentListPropertyEditor)
  protected
    function GetChildrenCount: Integer; override;
    function MakeEditorForm: TForm; override;
  end;

  { TToolsEditorForm }

  TToolsEditorForm = class(TComponentListEditorForm)
  protected
    procedure AddSubcomponent(AParent, AChild: TComponent); override;
    procedure BuildCaption; override;
    function ChildClass: TComponentClass; override;
    procedure EnumerateSubcomponentClasses; override;
    function GetChildrenList: TFPList; override;
    function MakeSubcomponent(
      AOwner: TComponent; ATag: Integer): TComponent; override;
  end;

var
  ToolsClassRegistry: TStringList;

function InitBuitlinTools(AChart: TChart): TBasicChartToolset;
var
  ts: TChartToolset;
begin
  ts := TChartToolset.Create(AChart);
  Result := ts;
  with TZoomDragTool.Create(AChart) do begin
    Shift := [ssLeft];
    Toolset := ts;
  end;
  TReticuleTool.Create(AChart).Toolset := ts;
end;

procedure Register;
var
  i: Integer;
begin
  for i := 0 to ToolsClassRegistry.Count - 1 do
    RegisterNoIcon([TChartToolClass(ToolsClassRegistry.Objects[i])]);
  RegisterComponents(CHART_COMPONENT_IDE_PAGE, [TChartToolset]);
  RegisterPropertyEditor(
    TypeInfo(TChartTools), TChartToolset, 'Tools', TToolsPropertyEditor);
  RegisterComponentEditor(TChartToolset, TToolsComponentEditor);
end;

procedure RegisterChartToolClass(
  AToolClass: TChartToolClass; const ACaption: String);
begin
  RegisterClass(AToolClass);
  ToolsClassRegistry.AddObject(ACaption, TObject(AToolClass));
end;

{ TToolsComponentEditor }

function TToolsComponentEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := tasToolsEditorTitle
  else
    Result := '';
end;

function TToolsComponentEditor.MakeEditorForm: TForm;
begin
  Result := TToolsEditorForm.Create(Application, GetComponent, Self, nil);
end;

{ TToolsPropertyEditor }

function TToolsPropertyEditor.GetChildrenCount: Integer;
begin
  Result := (GetObjectValue as TChartTools).Count;
end;

function TToolsPropertyEditor.MakeEditorForm: TForm;
begin
  with TToolsEditorForm do
    Result := Create(Application, GetComponent(0) as TComponent, nil, Self);
end;

{ TToolsEditorForm }

procedure TToolsEditorForm.AddSubcomponent(AParent, AChild: TComponent);
begin
  (AChild as TChartTool).Toolset := (AParent as TChartToolset);
end;

procedure TToolsEditorForm.BuildCaption;
begin
  Caption := tasToolsEditorTitle + ' - ' + Parent.Name;
end;

function TToolsEditorForm.ChildClass: TComponentClass;
begin
  Result := TChartTool;
end;

procedure TToolsEditorForm.EnumerateSubcomponentClasses;
var
  i: Integer;
begin
  for i := 0 to ToolsClassRegistry.Count - 1 do
    AddSubcomponentClass(ToolsClassRegistry[i], i);
end;

function TToolsEditorForm.GetChildrenList: TFPList;
begin
  Result := (Parent as TChartToolset).Tools;
end;

function TToolsEditorForm.MakeSubcomponent(
  AOwner: TComponent; ATag: Integer): TComponent;
begin
  Result := TChartToolClass(ToolsClassRegistry.Objects[ATag]).Create(AOwner);
end;

procedure TChartTool.Activate;
begin
  inherited Activate;
  SetCursor;
end;

{ TChartTool }

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

constructor TChartTool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := true;
  FActiveCursor := crDefault;
end;

procedure TChartTool.Deactivate;
begin
  RestoreCursor;
  inherited Deactivate;
end;

destructor TChartTool.Destroy;
begin
  Toolset := nil;
  inherited;
end;

procedure TChartTool.Dispatch(
  AChart: TChart; AEventId: TChartToolEventId; APoint: TPoint);
var
  ev: TChartToolMouseEvent;
begin
  if not Enabled or (FChart <> nil) and (FChart <> AChart) then exit;
  FChart := AChart;
  try
    ev := FMouseEvents[Ord(AEventId) + Ord(High(AEventId)) + 1];
    if Assigned(ev) then begin
      ev(Self, APoint);
      if Toolset.FIsHandled then exit;
    end;
    case AEventId of
      evidMouseDown: MouseDown(APoint);
      evidMouseMove: MouseMove(APoint);
      evidMouseUp  : MouseUp  (APoint);
    end;
    ev := FMouseEvents[Ord(AEventId)];
    if Assigned(ev) then
      ev(Self, APoint);
  finally
    if not IsActive then
      FChart := nil;
  end;
end;

function TChartTool.GetIndex: Integer;
begin
  if Toolset = nil then
    Result := -1
  else
    Result := Toolset.Tools.IndexOf(Self);
end;

function TChartTool.GetMouseEvent(AIndex: Integer): TChartToolMouseEvent;
begin
  Result := FMouseEvents[AIndex];
end;

function TChartTool.GetParentComponent: TComponent;
begin
  Result := FToolset;
end;

procedure TChartTool.Handled;
begin
  Toolset.FIsHandled := true;
end;

function TChartTool.HasParent: Boolean;
begin
  Result := true;
end;

function TChartTool.IsActive: Boolean;
begin
  Result := (FChart <> nil) and (FChart.ActiveToolIndex = Index);
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

procedure TChartTool.RestoreCursor;
begin
  if ActiveCursor = crDefault then exit;
  FChart.Cursor := FOldCursor;
end;

procedure TChartTool.SetActiveCursor(const AValue: TCursor);
begin
  if FActiveCursor = AValue then exit;
  if IsActive then
    RestoreCursor;
  FActiveCursor := AValue;
  if IsActive then
    SetCursor;
end;

procedure TChartTool.SetCursor;
begin
  if ActiveCursor = crDefault then exit;
  FOldCursor := FChart.Cursor;
  FChart.Cursor := ActiveCursor;
end;

procedure TChartTool.SetIndex(AValue: Integer);
begin
  Toolset.Tools.Move(Index, EnsureRange(AValue, 0, Toolset.Tools.Count - 1));
end;

procedure TChartTool.SetMouseEvent(
  AIndex: Integer; AValue: TChartToolMouseEvent);
begin
  FMouseEvents[AIndex] := AValue;
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
  FreeAndNil(FTools);
  inherited;
end;

function TChartToolset.Dispatch(
  AChart: TChart; AEventId: TChartToolEventId;
  AShift: TShiftState; APoint: TPoint): Boolean;
var
  candidates: array of TChartTool;
  candidateCount: Integer;

  procedure AddCandidate(AIndex: Integer);
  begin
    candidates[candidateCount] := Item[AIndex];
    candidateCount += 1;
  end;

var
  i, ai: Integer;
begin
  if Tools.Count = 0 then exit(false);

  SetLength(candidates, Tools.Count);
  candidateCount := 0;

  ai := AChart.ActiveToolIndex;
  if InRange(ai, 0, Tools.Count - 1) then
    AddCandidate(ai);
  for i := 0 to Tools.Count - 1 do
    if (i <> ai) and (Item[i].Shift = AShift) then
      AddCandidate(i);

  FIsHandled := false;
  for i := 0 to candidateCount - 1 do begin
    candidates[i].Dispatch(AChart, AEventId, APoint);
    if FIsHandled then exit(true);
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

{ TZoomDragTool }

procedure TZoomDragTool.MouseDown(APoint: TPoint);
begin
  if not FChart.AllowZoom then exit;
  Activate;
  with APoint do
    FSelectionRect := Rect(X, Y, X, Y);
  Handled;
end;

procedure TZoomDragTool.MouseMove(APoint: TPoint);
begin
  if not IsActive then exit;
  PrepareXorPen(FChart.Canvas);
  FChart.Canvas.Rectangle(FSelectionRect);
  FSelectionRect.BottomRight := APoint;
  FChart.Canvas.Rectangle(FSelectionRect);
  Handled;
end;

procedure TZoomDragTool.MouseUp(APoint: TPoint);
var
  ext: TDoubleRect;

  procedure CheckProportions;
  var
    newSize, oldSize: TDoublePoint;
    coeff: Double;
  begin
    if not Proportional then exit;
    newSize := ext.b - ext.a;
    oldSize := FChart.LogicalExtent.b - FChart.LogicalExtent.a;
    coeff := newSize.Y * oldSize.X;
    if coeff = 0 then exit;
    coeff := newSize.X * oldSize.Y / coeff;
    if coeff = 0 then exit;
    if coeff > 1 then
      ExpandRange(ext.a.Y, ext.b.Y, (coeff - 1) / 2)
    else
      ExpandRange(ext.a.X, ext.b.X, (1 / coeff  - 1) / 2);
  end;

begin
  Unused(APoint);
  Deactivate;

  PrepareXorPen(FChart.Canvas);
  FChart.Canvas.Rectangle(FSelectionRect);
  with FSelectionRect do begin
    if (Left >= Right) or (Top >= Bottom) then begin
      FChart.ZoomFull;
      exit;
    end;
    ext.a := FChart.ImageToGraph(TopLeft);
    ext.b := FChart.ImageToGraph(BottomRight);
  end;
  with ext do begin
    if a.X > b.X then
      Exchange(a.X, b.X);
    if a.Y > b.Y then
      Exchange(a.Y, b.Y);
  end;
  CheckProportions;
  FChart.LogicalExtent := ext;
  Handled;
end;

{ TReticuleTool }

procedure TReticuleTool.MouseMove(APoint: TPoint);
const
  DIST_FUNCS: array [TReticuleMode] of TPointDistFunc = (
    nil, @PointDistX, @PointDistY, @PointDist);
var
  cur, best: record
    pointIndex: Integer;
    retPos: TPoint;
    value: TDoublePoint;
  end;
  i, bestSeries: Integer;
  d, minDist: Double;
  df: TPointDistFunc;
begin
  if FChart.ReticuleMode = rmNone then exit;
  {$R-}{$Q-}
  minDist := Infinity;
  df := DIST_FUNCS[FChart.ReticuleMode];
  for i := 0 to FChart.SeriesCount - 1 do
    if
      FChart.Series[i].GetNearestPoint(
        df, APoint, cur.pointIndex, cur.retPos, cur.value) and
      PtInRect(FChart.ClipRect, cur.retPos)
    then begin
       d := df(APoint, cur.retPos);
       if d < minDist then begin
         bestSeries := i;
         best := cur;
         minDist := d;
       end;
    end;
  if (minDist < Infinity) and (best.retPos <> FChart.ReticulePos) then begin
    FChart.ReticulePos := best.retPos;
    if Assigned(FChart.OnDrawReticule) then
      FChart.OnDrawReticule(FChart, bestSeries, best.pointIndex, best.value);
  end;
  {$ifdef OverflowChecking}{$Q+}{$endif}{$ifdef RangeChecking}{$R+}{$endif}
end;

{ TZoomClickTool }

constructor TZoomClickTool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FZoomFactor := 1.0;
end;

procedure TZoomClickTool.MouseDown(APoint: TPoint);
var
  sz, center: TDoublePoint;
  ext: TDoubleRect;
begin
  if ZoomFactor <= 0 then exit;
  ext := FChart.LogicalExtent;
  sz := ext.b - ext.a;
  sz.X /= ZoomFactor * 2;
  sz.Y /= ZoomFactor * 2;
  center := FChart.ImageToGraph(APoint);
  ext.a := center - sz;
  ext.b := center + sz;
  FChart.LogicalExtent := ext;
  Handled;
end;

function TZoomClickTool.ZoomFactorIsStored: boolean;
begin
  Result := FZoomFactor <> 1.0;
end;

{ TPanDragTool }

constructor TPanDragTool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActiveCursor := crSizeAll;
  FDirections := PAN_DIRECTIONS_ALL;
end;

procedure TPanDragTool.MouseDown(APoint: TPoint);
begin
  Activate;
  FOrigin := APoint;
  Handled;
end;

procedure TPanDragTool.MouseMove(APoint: TPoint);
var
  d: TPoint;
  dd: TDoublePoint;
  ext: TDoubleRect;
begin
  d := FOrigin - APoint;
  FOrigin := APoint;

  if not (pdLeft in Directions) then d.X := Max(d.X, 0);
  if not (pdRight in Directions) then d.X := Min(d.X, 0);
  if not (pdUp in Directions) then d.Y := Max(d.Y, 0);
  if not (pdDown in Directions) then d.Y := Min(d.Y, 0);

  dd := FChart.ImageToGraph(d) - FChart.ImageToGraph(Point(0, 0));
  ext := FChart.LogicalExtent;
  ext.a += dd;
  ext.b += dd;
  FChart.LogicalExtent := ext;
  Handled;
end;

procedure TPanDragTool.MouseUp(APoint: TPoint);
begin
  Unused(APoint);
  Deactivate;
  Handled;
end;

{ TDataPointDragTool }

constructor TDataPointDragTool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActiveCursor := crSizeAll;
  FGrabRadius := DEF_GRAB_RADIUS;
  FPointIndex := -1;
end;

procedure TDataPointDragTool.MouseDown(APoint: TPoint);
var
  i, d, bestd, idx: Integer;
  s, bests: TBasicChartSeries;
  affected: TBooleanDynArray;
  dummy: TDoublePoint;
  nearest: TPoint;
begin
  bestd := MaxInt;
  bests := nil;
  affected := ParseAffectedSeries;
  for i := 0 to FChart.SeriesCount - 1 do begin
    if not affected[i] then continue;
    s := FChart.Series[i];
    if not s.GetNearestPoint(@PointDist, APoint, idx, nearest, dummy) then continue;
    d := PointDist(APoint, nearest);
    if d < bestd then begin
      bestd := d;
      bests := s;
      FPointIndex := idx;
    end;
  end;
  if (bests = nil) or (bestd > Sqr(GrabRadius)) then exit;
  FSeries := bests;
  Activate;
  Handled;
end;

procedure TDataPointDragTool.MouseMove(APoint: TPoint);
begin
  if FSeries <> nil then
    FSeries.MovePoint(FPointIndex, APoint);
end;

procedure TDataPointDragTool.MouseUp(APoint: TPoint);
begin
  Unused(APoint);
  FSeries := nil;
  Deactivate;
  Handled;
end;

function TDataPointDragTool.ParseAffectedSeries: TBooleanDynArray;
var
  s: TStringList;
  i, p: Integer;
begin
  SetLength(Result, FChart.SeriesCount);
  if AffectedSeries = '' then begin
    FillChar(Result[0], Length(Result), true);
    exit;
  end;
  s := TStringList.Create;
  try
    s.CommaText := AffectedSeries;
    FillChar(Result[0], Length(Result), false);
    for i := 0 to s.Count - 1 do begin
      p := StrToIntDef(s[i], -1);
      if InRange(p, 0, High(Result)) then
        Result[p] := true;
    end;
  finally
    s.Free;
  end;
end;

initialization

  ToolsClassRegistry := TStringList.Create;
  OnInitBuiltinTools := @InitBuitlinTools;
  RegisterChartToolClass(TZoomDragTool, 'Zoom drag tool');
  RegisterChartToolClass(TZoomClickTool, 'Zoom click tool');
  RegisterChartToolClass(TPanDragTool, 'Panning drag tool');
  RegisterChartToolClass(TReticuleTool, 'Reticule tool');
  RegisterChartToolClass(TDataPointDragTool, 'Data point drag tool');

finalization

  FreeAndNil(ToolsClassRegistry);

end.

