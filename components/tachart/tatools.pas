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

  { TChartTool }

  TChartTool = class(TBasicChartTool)
  private
    FEnabled: Boolean;
    FShift: TShiftState;
  protected
    procedure Dispatch(
      AChart: TChart; AEventId: TChartToolEventId; APoint: TPoint);
    function IsActive: Boolean;
    procedure MouseDown(APoint: TPoint); virtual;
    procedure MouseMove(APoint: TPoint); virtual;
    procedure MouseUp(APoint: TPoint); virtual;
  public
    constructor Create(ACollection: TCollection); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled default true;
    property Shift: TShiftState read FShift write FShift;
  end;

  TChartToolClass = class of TChartTool;

  { TChartToolset }

  TChartToolset = class(TBasicChartToolset)
  private
    FTools: TCollection;
    function GetItem(AIndex: Integer): TChartTool;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Dispatch(
      AChart: TChart; AEventId: TChartToolEventId;
      AShift: TShiftState; APoint: TPoint): Boolean; override;
    property Item[AIndex: Integer]: TChartTool read GetItem; default;
  published
    property Tools: TCollection read FTools;
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
  CollectionPropEditForm, Forms, GraphMath, Math, Menus, PropEdits, Types,
  TAChartUtils;

type

  { TChartToolsEditor }

  TChartToolsEditor = class(TCollectionPropertyEditor)
  public
    class function ShowCollectionEditor(
      ACollection: TCollection; OwnerPersistent: TPersistent;
      const PropName: String): TCustomForm; override;
  end;

  { TChartToolsEditorForm }

  TChartToolsEditorForm = class(TCollectionPropertyEditorForm)
  private
    procedure OnAddToolClick(ASender: TObject);
  end;

var
  ToolsClassRegistry: TStringList;
  ChartToolsForm: TChartToolsEditorForm;

function InitBuitlinTools(AChart: TChart): TBasicChartToolset;
var
  ts: TChartToolset;
begin
  ts := TChartToolset.Create(AChart);
  Result := ts;
  TChartZoomDragTool.Create(ts.Tools).Shift := [ssLeft];
  TChartReticuleTool.Create(ts.Tools);
end;

procedure InitChartToolsForm;
var
  m: TPopupMenu;
  mi: TMenuItem;
  i: Integer;
begin
  if ChartToolsForm <> nil then exit;
  ChartToolsForm := TChartToolsEditorForm.Create(Application);
  m := TPopupMenu.Create(ChartToolsForm);
  for i := 0 to ToolsClassRegistry.Count - 1 do begin
    mi := TMenuItem.Create(ChartToolsForm);
    mi.Caption := ToolsClassRegistry[i];
    mi.Tag := i;
    mi.OnClick := @ChartToolsForm.OnAddToolClick;
    m.Items.Add(mi);
  end;
  ChartToolsForm.AddButton.DropdownMenu := m;
  ChartToolsForm.AddButton.OnClick := nil;
end;

procedure Register;
begin
  RegisterComponents(CHART_COMPONENT_IDE_PAGE, [TChartToolset]);
  RegisterPropertyEditor(
    TypeInfo(TCollection), TChartToolset, 'Tools', TChartToolsEditor);
end;

procedure RegisterChartToolClass(
  AToolClass: TChartToolClass; const ACaption: String);
begin
  ToolsClassRegistry.AddObject(ACaption, TObject(AToolClass));
end;

{ TChartToolsEditor }

class function TChartToolsEditor.ShowCollectionEditor(
  ACollection: TCollection; OwnerPersistent: TPersistent;
  const PropName: String): TCustomForm;
begin
  InitChartToolsForm;
  ChartToolsForm.SetCollection(ACollection, OwnerPersistent, PropName);
  ChartToolsForm.EnsureVisible;
  Result := ChartToolsForm;
end;

{ TChartToolsEditorForm }

procedure TChartToolsEditorForm.OnAddToolClick(ASender: TObject);
begin
  if Collection = nil then exit;
  with ASender as TMenuItem do
    TChartToolClass(ToolsClassRegistry.Objects[Tag]).Create(Collection);

  FillCollectionListBox;
  if CollectionListBox.Items.Count > 0 then
    CollectionListBox.ItemIndex := CollectionListBox.Items.Count - 1;
  SelectInObjectInspector(True, False);
  UpdateButtons;
  UpdateCaption;
  Modified;
end;


{ TChartTool }

constructor TChartTool.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FEnabled := true;
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

{ TChartToolset }

constructor TChartToolset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTools := TCollection.Create(TChartTool);
end;

destructor TChartToolset.Destroy;
begin
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

function TChartToolset.GetItem(AIndex: Integer): TChartTool;
begin
  Result := Tools.Items[AIndex] as TChartTool;
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

