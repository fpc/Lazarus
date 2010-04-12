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
  Classes, Controls,
  TAGraph;

type

  TChartToolset = class;

  { TChartTool }

  TChartTool = class(TBasicChartTool)
  private
    FActiveCursor: TCursor;
    FEnabled: Boolean;
    FShift: TShiftState;
    FToolset: TChartToolset;
    procedure SetActiveCursor(const AValue: TCursor);
    procedure SetToolset(const AValue: TChartToolset);
  private
    FOldCursor: TCursor;
    procedure RestoreCursor;
    procedure SetCursor;
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
    function HasParent: Boolean; override;

    property ActiveCursor: TCursor
      read FActiveCursor write SetActiveCursor default crDefault;
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
  ComponentEditors, Forms, GraphMath, Math, PropEdits, SysUtils, Types,
  TAChartUtils, TASubcomponentsEditor;

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

function TChartTool.GetIndex: Integer;
begin
  if Toolset = nil then
    Result := -1
  else
    Result := Toolset.Tools.IndexOf(Self);
end;

function TChartTool.GetParentComponent: TComponent;
begin
  Result := FToolset;
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

{ TZoomDragTool }

procedure TZoomDragTool.MouseDown(APoint: TPoint);
begin
  if not FChart.AllowZoom then exit;
  Activate;
  with APoint do
    FSelectionRect := Rect(X, Y, X, Y);
end;

procedure TZoomDragTool.MouseMove(APoint: TPoint);
begin
  if not IsActive then exit;
  PrepareXorPen(FChart.Canvas);
  FChart.Canvas.Rectangle(FSelectionRect);
  FSelectionRect.BottomRight := APoint;
  FChart.Canvas.Rectangle(FSelectionRect);
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
end;

{ TReticuleTool }

procedure TReticuleTool.MouseMove(APoint: TPoint);
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
end;

procedure TPanDragTool.MouseMove(APoint: TPoint);
var
  d: TPoint;
  dd: TDoublePoint;
  ext: TDoubleRect;
begin
  d := APoint - FOrigin;
  FOrigin := APoint;

  if not (pdLeft in Directions) then d.X := Max(d.X, 0);
  if not (pdRight in Directions) then d.X := Min(d.X, 0);
  if not (pdUp in Directions) then d.Y := Max(d.Y, 0);
  if not (pdDown in Directions) then d.Y := Min(d.Y, 0);

  dd := FChart.ImageToGraph(d) -  FChart.ImageToGraph(Point(0, 0));
  ext := FChart.LogicalExtent;
  ext.a += dd;
  ext.b += dd;
  FChart.LogicalExtent := ext;
end;

procedure TPanDragTool.MouseUp(APoint: TPoint);
begin
  Unused(APoint);
  Deactivate;
end;

initialization

  ToolsClassRegistry := TStringList.Create;
  OnInitBuiltinTools := @InitBuitlinTools;
  RegisterChartToolClass(TZoomDragTool, 'Zoom drag tool');
  RegisterChartToolClass(TZoomClickTool, 'Zoom click tool');
  RegisterChartToolClass(TPanDragTool, 'Panning drag tool');
  RegisterChartToolClass(TReticuleTool, 'Reticule tool');

finalization

  FreeAndNil(ToolsClassRegistry);

end.

