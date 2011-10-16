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
  Classes, Controls, CustomTimer, Types,
  TAChartUtils, TADrawUtils, TAGraph, TATypes;

type

  TChartToolset = class;
  TChartTool = class;

  TChartToolEvent = procedure (ATool: TChartTool; APoint: TPoint) of object;

  TChartToolDrawingMode = (tdmDefault, tdmNormal, tdmXor);
  TChartToolEffectiveDrawingMode = tdmNormal .. tdmXor;

  { TChartTool }

  TChartTool = class(TBasicChartTool)
  strict private
    FActiveCursor: TCursor;
    FDrawingMode: TChartToolDrawingMode;
    FEnabled: Boolean;
    FEventsAfter: array [TChartToolEventId] of TChartToolEvent;
    FEventsBefore: array [TChartToolEventId] of TChartToolEvent;
    FOldCursor: TCursor;
    FShift: TShiftState;
    FToolset: TChartToolset;
    function GetAfterEvent(AIndex: Integer): TChartToolEvent;
    function GetBeforeEvent(AIndex: Integer): TChartToolEvent;
    procedure SetActiveCursor(AValue: TCursor);
    procedure SetAfterEvent(AIndex: Integer; AValue: TChartToolEvent);
    procedure SetBeforeEvent(AIndex: Integer; AValue: TChartToolEvent);
    procedure SetDrawingMode(AValue: TChartToolDrawingMode);
    procedure SetToolset(AValue: TChartToolset);
  protected
    procedure ReadState(Reader: TReader); override;
    procedure SetParentComponent(AParent: TComponent); override;
    property DrawingMode: TChartToolDrawingMode
      read FDrawingMode write SetDrawingMode default tdmDefault;
  strict protected
    procedure Activate; override;
    procedure Deactivate; override;
    function EffectiveDrawingMode: TChartToolEffectiveDrawingMode;
    function GetIndex: Integer; override;
    function IsActive: Boolean;
    procedure KeyDown(APoint: TPoint); virtual;
    procedure KeyUp(APoint: TPoint); virtual;
    procedure MouseDown(APoint: TPoint); virtual;
    procedure MouseMove(APoint: TPoint); virtual;
    procedure MouseUp(APoint: TPoint); virtual;
    procedure MouseWheelDown(APoint: TPoint); virtual;
    procedure MouseWheelUp(APoint: TPoint); virtual;
    procedure RestoreCursor;
    procedure SetCursor;
    procedure SetIndex(AValue: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;
  public
    procedure AfterDraw(AChart: TChart; ADrawer: IChartDrawer); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure Dispatch(
      AChart: TChart; AEventId: TChartToolEventId; APoint: TPoint); overload;
    procedure Draw(AChart: TChart; ADrawer: IChartDrawer); virtual;
    procedure Handled;

    property ActiveCursor: TCursor
      read FActiveCursor write SetActiveCursor default crDefault;
    property Toolset: TChartToolset read FToolset write SetToolset;
  published
    property Enabled: Boolean read FEnabled write FEnabled default true;
    property Shift: TShiftState read FShift write FShift default [];
  published
    property OnAfterKeyDown: TChartToolEvent
      index 0 read GetAfterEvent write SetAfterEvent;
    property OnAfterKeyUp: TChartToolEvent
      index 1 read GetAfterEvent write SetAfterEvent;
    property OnAfterMouseDown: TChartToolEvent
      index 2 read GetAfterEvent write SetAfterEvent;
    property OnAfterMouseMove: TChartToolEvent
      index 3 read GetAfterEvent write SetAfterEvent;
    property OnAfterMouseUp: TChartToolEvent
      index 4 read GetAfterEvent write SetAfterEvent;
    property OnAfterMouseWheelDown: TChartToolEvent
      index 5 read GetAfterEvent write SetAfterEvent;
    property OnAfterMouseWheelUp: TChartToolEvent
      index 6 read GetAfterEvent write SetAfterEvent;

    property OnBeforeKeyDown: TChartToolEvent
      index 0 read GetBeforeEvent write SetBeforeEvent;
    property OnBeforeKeyUp: TChartToolEvent
      index 1 read GetBeforeEvent write SetBeforeEvent;
    property OnBeforeMouseDown: TChartToolEvent
      index 2 read GetBeforeEvent write SetBeforeEvent;
    property OnBeforeMouseMove: TChartToolEvent
      index 3 read GetBeforeEvent write SetBeforeEvent;
    property OnBeforeMouseUp: TChartToolEvent
      index 4 read GetBeforeEvent write SetBeforeEvent;
    property OnBeforeMouseWheelDown: TChartToolEvent
      index 5 read GetBeforeEvent write SetBeforeEvent;
    property OnBeforeMouseWheelUp: TChartToolEvent
      index 6 read GetBeforeEvent write SetBeforeEvent;
  end;

  {$IFNDEF fpdoc} // Workaround for issue #18549.
  TChartToolsEnumerator = specialize TTypedFPListEnumerator<TChartTool>;
  {$ENDIF}

  TChartToolClass = class of TChartTool;

  TChartTools = class(TIndexedComponentList)
  public
    function GetEnumerator: TChartToolsEnumerator;
  end;

  { TChartToolset }

  TChartToolset = class(TBasicChartToolset)
  strict private
    FTools: TChartTools;
    function GetItem(AIndex: Integer): TChartTool;
  private
    FIsHandled: Boolean;
  protected
    procedure SetName(const AValue: TComponentName); override;
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
    procedure Draw(AChart: TChart; ADrawer: IChartDrawer); override;
    property Item[AIndex: Integer]: TChartTool read GetItem; default;
  published
    property Tools: TChartTools read FTools;
  end;

  TUserDefinedTool = class(TChartTool)
  end;

  { TBasicZoomTool }

  TBasicZoomTool = class(TChartTool)
  strict private
    FAnimationInterval: Cardinal;
    FAnimationSteps: Cardinal;
    FCurrentStep: Cardinal;
    FExtDst: TDoubleRect;
    FExtSrc: TDoubleRect;
    FFullZoom: Boolean;
    FTimer: TCustomTimer;

    procedure OnTimer(ASender: TObject);
  protected
    procedure DoZoom(const ANewExtent: TDoubleRect; AFull: Boolean);
    function IsAnimating: Boolean; inline;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AnimationInterval: Cardinal
      read FAnimationInterval write FAnimationInterval default 0;
    property AnimationSteps: Cardinal
      read FAnimationSteps write FAnimationSteps default 0;
  end;

  TZoomRatioLimit = (zrlNone, zrlProportional, zrlFixedX, zrlFixedY);

  { TZoomDragTool }

  TZoomDragTool = class(TBasicZoomTool)
  strict private
    FFrame: TChartPen;
    FRatioLimit: TZoomRatioLimit;
    FSelectionRect: TRect;
    function GetProportional: Boolean;
    procedure SetFrame(AValue: TChartPen);
    procedure SetProportional(AValue: Boolean);
  public
    procedure MouseDown(APoint: TPoint); override;
    procedure MouseMove(APoint: TPoint); override;
    procedure MouseUp(APoint: TPoint); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Draw(AChart: TChart; ADrawer: IChartDrawer); override;
  published
    property DrawingMode;
    property Frame: TChartPen read FFrame write SetFrame;
    property Proportional: Boolean
      read GetProportional write SetProportional stored false default false;
      deprecated;
    property RatioLimit: TZoomRatioLimit
      read FRatioLimit write FRatioLimit default zrlNone;
  end;

  TBasicZoomStepTool = class(TBasicZoomTool)
  strict private
    FFixedPoint: Boolean;
    FZoomFactor: Double;
    FZoomRatio: Double;
    function ZoomFactorIsStored: boolean;
    function ZoomRatioIsStored: boolean;
  strict protected
    procedure DoZoomStep(const APoint: TPoint; const AFactor: TDoublePoint);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property FixedPoint: Boolean read FFixedPoint write FFixedPoint default true;
    property ZoomFactor: Double
      read FZoomFactor write FZoomFactor stored ZoomFactorIsStored;
    property ZoomRatio: Double
      read FZoomRatio write FZoomRatio stored ZoomRatioIsStored;
  end;

  TZoomClickTool = class(TBasicZoomStepTool)
  public
    procedure MouseDown(APoint: TPoint); override;
  end;

  TZoomMouseWheelTool = class(TBasicZoomStepTool)
  public
    procedure MouseWheelDown(APoint: TPoint); override;
    procedure MouseWheelUp(APoint: TPoint); override;
  end;

  TPanDirection = (pdLeft, pdUp, pdRight, pdDown);
  TPanDirectionSet = set of TPanDirection;

const
  PAN_DIRECTIONS_ALL = [Low(TPanDirection) .. High(TPanDirection)];

type

  { TBasicPanTool }

  TBasicPanTool = class(TChartTool)
  strict private
    FLimitToExtent: TPanDirectionSet;
  strict protected
    procedure PanBy(AOffset: TPoint);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property LimitToExtent: TPanDirectionSet
      read FLimitToExtent write FLimitToExtent default [];
  end;

  { TPanDragTool }

  TPanDragTool = class(TBasicPanTool)
  strict private
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

  { TPanClickTool }

  TPanClickTool = class(TBasicPanTool)
  strict private
    FInterval: Cardinal;
    FMargins: TChartMargins;
    FOffset: TPoint;
    FTimer: TCustomTimer;

    function GetOffset(APoint: TPoint): TPoint;
    procedure OnTimer(ASender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MouseDown(APoint: TPoint); override;
    procedure MouseMove(APoint: TPoint); override;
    procedure MouseUp(APoint: TPoint); override;
  published
    property ActiveCursor default crSizeAll;
    property Interval: Cardinal read FInterval write FInterval default 0;
    property Margins: TChartMargins read FMargins write FMargins;
  end;

  { TPanMouseWheelTool }

  TPanMouseWheelTool = class(TBasicPanTool)
  strict private
    FStep: Cardinal;
    FWheelUpDirection: TPanDirection;

    procedure DoPan(AStep: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    procedure MouseWheelDown(APoint: TPoint); override;
    procedure MouseWheelUp(APoint: TPoint); override;
  published
    property Step: Cardinal read FStep write FStep default 10;
    property WheelUpDirection: TPanDirection
      read FWheelUpDirection write FWheelUpDirection default pdUp;
  end;

  TChartDistanceMode = (cdmXY, cdmOnlyX, cdmOnlyY);

  { TDataPointTool }

  TDataPointTool = class(TChartTool)
  strict private
    FAffectedSeries: TPublishedIntegerSet;
    FDistanceMode: TChartDistanceMode;
    FGrabRadius: Integer;
    function GetAffectedSeries: String; inline;
    function GetIsSeriesAffected(AIndex: Integer): Boolean; inline;
    procedure SetAffectedSeries(AValue: String); inline;
    procedure SetIsSeriesAffected(AIndex: Integer; AValue: Boolean); inline;
  strict protected
    FNearestGraphPoint: TDoublePoint;
    FPointIndex: Integer;
    FSeries: TBasicChartSeries;
    procedure FindNearestPoint(APoint: TPoint);
  public
    constructor Create(AOwner: TComponent); override;
  public
    property IsSeriesAffected[AIndex: Integer]: Boolean
      read GetIsSeriesAffected write SetIsSeriesAffected;
    property NearestGraphPoint: TDoublePoint read FNearestGraphPoint;
    property PointIndex: Integer read FPointIndex;
    property Series: TBasicChartSeries read FSeries;
  published
    property AffectedSeries: String
      read GetAffectedSeries write SetAffectedSeries;
    property DistanceMode: TChartDistanceMode
      read FDistanceMode write FDistanceMode default cdmXY;
    property GrabRadius: Integer read FGrabRadius write FGrabRadius default 4;
  end;

  { TDataPointDragTool }

  TDataPointDragTool = class(TDataPointTool)
  public
    constructor Create(AOwner: TComponent); override;
    procedure MouseDown(APoint: TPoint); override;
    procedure MouseMove(APoint: TPoint); override;
    procedure MouseUp(APoint: TPoint); override;
  published
    property ActiveCursor default crSizeAll;
  end;

  { TDataPointClickTool }

  TDataPointClickTool = class(TDataPointTool)
  strict private
    FMouseDownPoint: TPoint;
    FOnPointClick: TChartToolEvent;
  public
    procedure MouseDown(APoint: TPoint); override;
    procedure MouseUp(APoint: TPoint); override;
  published
    property ActiveCursor;
    property OnPointClick: TChartToolEvent
      read FOnPointClick write FOnPointClick;
  end;

  TDataPointHintTool = class;

  TChartToolHintEvent = procedure (
    ATool: TDataPointHintTool; const APoint: TPoint; var AHint: String) of object;

  { TDataPointHintTool }

  TDataPointHintTool = class(TDataPointTool)
  strict private
    FOnHint: TChartToolHintEvent;
    FPrevPointIndex: Integer;
    FPrevSeries: TBasicChartSeries;
    FUseDefaultHintText: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure MouseMove(APoint: TPoint); override;
  published
    property ActiveCursor;
    property OnHint: TChartToolHintEvent read FOnHint write FOnHint;
    property UseDefaultHintText: Boolean
      read FUseDefaultHintText write FUseDefaultHintText default true;
  end;

  TDataPointCrosshairTool = class;

  TChartCrosshairDrawEvent = procedure (
    ASender: TDataPointCrosshairTool) of object;

  TChartCrosshairShape = (ccsNone, ccsVertical, ccsHorizontal, ccsCross);

  TDataPointCrosshairTool = class(TDataPointTool)
  strict private
    FCrosshairPen: TChartPen;
    FOnDraw: TChartCrosshairDrawEvent;
    FPosition: TDoublePoint;
    FShape: TChartCrosshairShape;
    FSize: Integer;
    procedure DoDraw;
    procedure DoHide;
    procedure SetCrosshairPen(AValue: TChartPen);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Draw(AChart: TChart; ADrawer: IChartDrawer); override;
    procedure Hide;
    procedure KeyDown(APoint: TPoint); override;
    procedure MouseMove(APoint: TPoint); override;
    property Position: TDoublePoint read FPosition;
  published
    property CrosshairPen: TChartPen read FCrosshairPen write SetCrosshairPen;
    property DrawingMode;
    property GrabRadius default 20;
    property OnDraw: TChartCrosshairDrawEvent read FOnDraw write FOnDraw;
    property Shape: TChartCrosshairShape
      read FShape write FShape default ccsCross;
    property Size: Integer read FSize write FSize default -1;
  end;

  { TReticuleTool }

  TReticuleTool = class(TChartTool)
  public
    procedure MouseMove(APoint: TPoint); override;
  end;

  procedure Register;
  procedure RegisterChartToolClass(
    AToolClass: TChartToolClass; const ACaption: String);

var
  ToolsClassRegistry: TStringList;

implementation

uses
  Forms, GraphMath, InterfaceBase, Math, SysUtils,
  TACustomSeries, TADrawerCanvas, TAEnumerators, TAGeometry, TAMath;

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
end;

procedure RegisterChartToolClass(
  AToolClass: TChartToolClass; const ACaption: String);
begin
  RegisterClass(AToolClass);
  ToolsClassRegistry.AddObject(ACaption, TObject(AToolClass));
end;

{ TChartTool }

procedure TChartTool.Activate;
begin
  inherited Activate;
  SetCursor;
end;

procedure TChartTool.AfterDraw(AChart: TChart; ADrawer: IChartDrawer);
begin
  Unused(AChart, ADrawer);
  if not IsActive then
    FChart := nil;
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
  ev: TChartToolEvent;
begin
  if not Enabled or (FChart <> nil) and (FChart <> AChart) then exit;
  FChart := AChart;
  ev := FEventsBefore[AEventId];
  if Assigned(ev) then begin
    ev(Self, APoint);
    if Toolset.FIsHandled then exit;
  end;
  case AEventId of
    evidKeyDown       : KeyDown       (APoint);
    evidKeyUp         : KeyUp         (APoint);
    evidMouseDown     : MouseDown     (APoint);
    evidMouseMove     : MouseMove     (APoint);
    evidMouseUp       : MouseUp       (APoint);
    evidMouseWheelDown: MouseWheelDown(APoint);
    evidMouseWheelUp  : MouseWheelUp  (APoint);
  end;
  ev := FEventsAfter[AEventId];
  if Assigned(ev) then
    ev(Self, APoint);
  if not IsActive then
    FChart := nil;
end;

procedure TChartTool.Draw(AChart: TChart; ADrawer: IChartDrawer);
begin
  Unused(ADrawer);
  FChart := AChart;
end;

function TChartTool.EffectiveDrawingMode: TChartToolEffectiveDrawingMode;
begin
  if DrawingMode <> tdmDefault then
    Result := DrawingMode
  else if WidgetSet.LCLPlatform in [lpGtk, lpGtk2, lpWin32] then
    Result := tdmXor
  else
    Result := tdmNormal;
end;

function TChartTool.GetAfterEvent(AIndex: Integer): TChartToolEvent;
begin
  Result := FEventsAfter[TChartToolEventId(AIndex)];
end;

function TChartTool.GetBeforeEvent(AIndex: Integer): TChartToolEvent;
begin
  Result := FEventsBefore[TChartToolEventId(AIndex)];
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

procedure TChartTool.KeyDown(APoint: TPoint);
begin
  Unused(APoint);
end;

procedure TChartTool.KeyUp(APoint: TPoint);
begin
  Unused(APoint);
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

procedure TChartTool.MouseWheelDown(APoint: TPoint);
begin
  Unused(APoint);
end;

procedure TChartTool.MouseWheelUp(APoint: TPoint);
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

procedure TChartTool.SetActiveCursor(AValue: TCursor);
begin
  if FActiveCursor = AValue then exit;
  if IsActive then
    RestoreCursor;
  FActiveCursor := AValue;
  if IsActive then
    SetCursor;
end;

procedure TChartTool.SetAfterEvent(AIndex: Integer; AValue: TChartToolEvent);
begin
  FEventsAfter[TChartToolEventId(AIndex)] := AValue;
end;

procedure TChartTool.SetBeforeEvent(AIndex: Integer; AValue: TChartToolEvent);
begin
  FEventsBefore[TChartToolEventId(AIndex)] := AValue;
end;

procedure TChartTool.SetCursor;
begin
  if ActiveCursor = crDefault then exit;
  FOldCursor := FChart.Cursor;
  FChart.Cursor := ActiveCursor;
end;

procedure TChartTool.SetDrawingMode(AValue: TChartToolDrawingMode);
begin
  if FDrawingMode = AValue then exit;
  FDrawingMode := AValue;
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

procedure TChartTool.SetToolset(AValue: TChartToolset);
begin
  if FToolset = AValue then exit;
  if FToolset <> nil then
    FToolset.Tools.Remove(Self);
  FToolset := AValue;
  if FToolset <> nil then
    FToolset.Tools.Add(Self);
end;

{ TChartTools }

function TChartTools.GetEnumerator: TChartToolsEnumerator;
begin
  Result := TChartToolsEnumerator.Create(Self);
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

procedure TChartToolset.Draw(AChart: TChart; ADrawer: IChartDrawer);
var
  t: TChartTool;
begin
  for t in Tools do begin
    t.Draw(AChart, ADrawer);
    t.AfterDraw(AChart, ADrawer);
  end;
end;

procedure TChartToolset.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  t: TChartTool;
begin
  for t in Tools do
    if t.Owner = Root then
      Proc(t);
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

procedure TChartToolset.SetName(const AValue: TComponentName);
var
  oldName: String;
begin
  if Name = AValue then exit;
  oldName := Name;
  inherited SetName(AValue);
  if csDesigning in ComponentState then
    Tools.ChangeNamePrefix(oldName, AValue);
end;

{ TBasicZoomTool }

constructor TBasicZoomTool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimer := TCustomTimer.Create(nil);
  FTimer.Enabled := false;
  FTimer.OnTimer := @OnTimer;
end;

destructor TBasicZoomTool.Destroy;
begin
  FreeAndNil(FTimer);
  inherited Destroy;
end;

procedure TBasicZoomTool.DoZoom(const ANewExtent: TDoubleRect; AFull: Boolean);
begin
  if (AnimationInterval = 0) or (AnimationSteps = 0) then begin
    if AFull then
      FChart.ZoomFull
    else
      FChart.LogicalExtent := ANewExtent;
    if IsActive then
      Deactivate;
    exit;
  end;
  if not IsActive then
    Activate;
  FExtSrc := FChart.LogicalExtent;
  FExtDst := ANewExtent;
  FFullZoom := AFull;
  FCurrentStep := 0;
  FTimer.Interval := AnimationInterval;
  FTimer.Enabled := true;
end;

function TBasicZoomTool.IsAnimating: Boolean;
begin
  Result := FTimer.Enabled;
end;

procedure TBasicZoomTool.OnTimer(ASender: TObject);
var
  ext: TDoubleRect;
  t: Double;
  i: Integer;
begin
  Unused(ASender);
  FCurrentStep += 1;
  FTimer.Enabled := FCurrentStep < AnimationSteps;
  if FFullZoom and not IsAnimating then
    FChart.ZoomFull
  else begin
    t := FCurrentStep / AnimationSteps;
    for i := Low(ext.coords) to High(ext.coords) do
      ext.coords[i] := WeightedAverage(FExtSrc.coords[i], FExtDst.coords[i], t);
    NormalizeRect(ext);
    FChart.LogicalExtent := ext;
  end;
  if not IsAnimating then
    Deactivate;
end;

{ TZoomDragTool }

constructor TZoomDragTool.Create(AOwner: TComponent);
begin
  inherited;
  FFrame := TChartPen.Create;
end;

destructor TZoomDragTool.Destroy;
begin
  FreeAndNil(FFrame);
  inherited;
end;

procedure TZoomDragTool.Draw(AChart: TChart; ADrawer: IChartDrawer);
begin
  if not IsActive or IsAnimating then exit;
  inherited;
  case EffectiveDrawingMode of
    tdmXor:
      PrepareXorPen(FChart.Canvas);
    tdmNormal:
      FChart.Drawer.Pen := Frame;
  end;
  FChart.Drawer.Rectangle(FSelectionRect);
end;

function TZoomDragTool.GetProportional: Boolean;
begin
  Result := RatioLimit = zrlProportional;
end;

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
  if not IsActive or IsAnimating then exit;
  case EffectiveDrawingMode of
    tdmXor: begin
      PrepareXorPen(FChart.Canvas);
      FChart.Canvas.Rectangle(FSelectionRect);
      FSelectionRect.BottomRight := APoint;
      FChart.Canvas.Rectangle(FSelectionRect);
    end;
    tdmNormal: begin
      FSelectionRect.BottomRight := APoint;
      FChart.StyleChanged(Self);
    end;
  end;
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
    case RatioLimit of
      zrlNone: exit;
      zrlProportional: begin
        newSize := ext.b - ext.a;
        oldSize := FChart.LogicalExtent.b - FChart.LogicalExtent.a;
        coeff := newSize.Y * oldSize.X;
        if coeff = 0 then exit;
        coeff := newSize.X * oldSize.Y / coeff;
        if coeff = 0 then exit;
        if coeff > 1 then
          ExpandRange(ext.a.Y, ext.b.Y, (coeff - 1) / 2)
        else
          ExpandRange(ext.a.X, ext.b.X, (1 / coeff - 1) / 2);
      end;
      zrlFixedX:
        with FChart.GetFullExtent do begin
          ext.a.X := a.X;
          ext.b.X := b.X;
        end;
      zrlFixedY:
        with FChart.GetFullExtent do begin
          ext.a.Y := a.Y;
          ext.b.Y := b.Y;
        end;
    end;
  end;

begin
  Unused(APoint);

  PrepareXorPen(FChart.Canvas);
  FChart.Canvas.Rectangle(FSelectionRect);
  with FSelectionRect do begin
    if (Left >= Right) or (Top >= Bottom) then begin
      DoZoom(FChart.GetFullExtent, true);
      exit;
    end;
    ext.a := FChart.ImageToGraph(TopLeft);
    ext.b := FChart.ImageToGraph(BottomRight);
  end;
  NormalizeRect(ext);
  CheckProportions;
  DoZoom(ext, false);
  Handled;
end;

procedure TZoomDragTool.SetFrame(AValue: TChartPen);
begin
  FFrame.Assign(AValue);
end;

procedure TZoomDragTool.SetProportional(AValue: Boolean);
begin
  if AValue then
    RatioLimit := zrlProportional
  else
    RatioLimit := zrlNone;
end;

{ TReticuleTool }

procedure TReticuleTool.MouseMove(APoint: TPoint);
const
  DIST_FUNCS: array [TReticuleMode] of TPointDistFunc = (
    nil, @PointDistX, @PointDistY, @PointDist);
var
  cur, best: TNearestPointResults;
  p: TNearestPointParams;
  s, bestS: TCustomChartSeries;
begin
  if FChart.ReticuleMode = rmNone then exit;
  best.FDist := MaxInt;
  p.FDistFunc := DIST_FUNCS[FChart.ReticuleMode];
  p.FPoint := APoint;
  p.FRadius := Trunc(Sqrt(MaxInt));
  for s in CustomSeries(FChart) do
    if
      (not (s is TBasicPointSeries) or TBasicPointSeries(s).UseReticule) and
      s.GetNearestPoint(p, cur) and PtInRect(FChart.ClipRect, cur.FImg) and
      (cur.FDist < best.FDist)
    then begin
      bestS := s;
      best := cur;
    end;
  if (best.FDist = MaxInt) or (best.FImg = FChart.ReticulePos) then exit;
  FChart.ReticulePos := best.FImg;
  if Assigned(FChart.OnDrawReticule) then
    FChart.OnDrawReticule(FChart, bestS.Index, best.FIndex, best.FValue);
end;

{ TBasicZoomStepTool }

constructor TBasicZoomStepTool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFixedPoint := true;
  FZoomFactor := 1.0;
  FZoomRatio := 1.0;
end;

procedure TBasicZoomStepTool.DoZoomStep(
  const APoint: TPoint; const AFactor: TDoublePoint);
var
  sz, center, ratio: TDoublePoint;
  ext: TDoubleRect;
begin
  ext := FChart.LogicalExtent;
  center := FChart.ImageToGraph(APoint);
  sz := ext.b - ext.a;
  if FixedPoint then
    ratio := (center - ext.a) / sz
  else
    ratio := DoublePoint(0.5, 0.5);
  ext.a := center - sz * ratio / AFactor;
  ext.b := center + sz * (DoublePoint(1, 1) - ratio) / AFactor;
  DoZoom(ext, false);
  Handled;
end;

function TBasicZoomStepTool.ZoomFactorIsStored: boolean;
begin
  Result := FZoomFactor <> 1.0;
end;

function TBasicZoomStepTool.ZoomRatioIsStored: boolean;
begin
  Result := FZoomRatio <> 1.0;
end;

{ TZoomClickTool }

procedure TZoomClickTool.MouseDown(APoint: TPoint);
begin
  if (ZoomFactor <= 0) or (ZoomRatio <= 0) then exit;
  DoZoomStep(APoint, DoublePoint(ZoomFactor, ZoomFactor * ZoomRatio));
end;

{ TZoomMouseWheelTool }

procedure TZoomMouseWheelTool.MouseWheelDown(APoint: TPoint);
begin
  if (ZoomFactor <= 0) or (ZoomRatio <= 0) then exit;
  DoZoomStep(APoint, DoublePoint(ZoomFactor, ZoomFactor * ZoomRatio));
end;

procedure TZoomMouseWheelTool.MouseWheelUp(APoint: TPoint);
begin
  if (ZoomFactor <= 0) or (ZoomRatio <= 0) then exit;
  DoZoomStep(APoint, DoublePoint(1 / ZoomFactor, 1 / ZoomFactor / ZoomRatio));
end;

{ TBasicPanTool }

constructor TBasicPanTool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActiveCursor := crSizeAll;
end;

procedure TBasicPanTool.PanBy(AOffset: TPoint);
var
  dd: TDoublePoint;
  ext, fullExt: TDoubleRect;
begin
  dd := FChart.ImageToGraph(AOffset) - FChart.ImageToGraph(Point(0, 0));
  ext := FChart.LogicalExtent;
  if LimitToExtent <> [] then begin
    fullExt := FChart.GetFullExtent;
    if (pdRight in LimitToExtent) and (ext.a.X + dd.X < fullExt.a.X) then
      dd.X := fullExt.a.X - ext.a.X;
    if (pdUp in LimitToExtent) and (ext.a.Y + dd.Y < fullExt.a.Y) then
      dd.Y := fullExt.a.Y - ext.a.Y;
    if (pdLeft in LimitToExtent) and (ext.b.X + dd.X > fullExt.b.X) then
      dd.X := fullExt.b.X - ext.b.X;
    if (pdDown in LimitToExtent) and (ext.b.Y + dd.Y > fullExt.b.Y) then
      dd.Y := fullExt.b.Y - ext.b.Y;
  end;
  ext.a += dd;
  ext.b += dd;
  FChart.LogicalExtent := ext;
end;

{ TPanDragTool }

constructor TPanDragTool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
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
begin
  d := FOrigin - APoint;
  FOrigin := APoint;

  if not (pdLeft in Directions) then d.X := Max(d.X, 0);
  if not (pdRight in Directions) then d.X := Min(d.X, 0);
  if not (pdUp in Directions) then d.Y := Max(d.Y, 0);
  if not (pdDown in Directions) then d.Y := Min(d.Y, 0);

  PanBy(d);
  Handled;
end;

procedure TPanDragTool.MouseUp(APoint: TPoint);
begin
  Unused(APoint);
  Deactivate;
  Handled;
end;

{ TPanClickTool }

constructor TPanClickTool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMargins := TChartMargins.Create(nil);
  FTimer := TCustomTimer.Create(nil);
  FTimer.Enabled := false;
  FTimer.OnTimer := @OnTimer;
end;

destructor TPanClickTool.Destroy;
begin
  FreeAndNil(FMargins);
  FreeAndNil(FTimer);
  inherited Destroy;
end;

function TPanClickTool.GetOffset(APoint: TPoint): TPoint;
var
  r: TRect;
begin
  Result := Point(0, 0);
  r := FChart.ClipRect;
  if not PtInRect(r, APoint) then exit;
  with Size(r) do
    if
      (Margins.Left + Margins.Right >= cx) or
      (Margins.Top + Margins.Bottom >= cy)
    then
      exit;
  Result.X := Min(APoint.X - r.Left - Margins.Left, 0);
  if Result.X = 0 then
    Result.X := Max(Margins.Right - r.Right + APoint.X, 0);
  Result.Y := Min(APoint.Y - r.Top - Margins.Top, 0);
  if Result.Y = 0 then
    Result.Y := Max(Margins.Bottom - r.Bottom + APoint.Y, 0);
end;

procedure TPanClickTool.MouseDown(APoint: TPoint);
begin
  FOffset := GetOffset(APoint);
  if FOffset = Point(0, 0) then exit;
  PanBy(FOffset);
  if Interval > 0 then begin
    Activate;
    FTimer.Interval := Interval;
    FTimer.Enabled := true;
  end;
  Handled;
end;

procedure TPanClickTool.MouseMove(APoint: TPoint);
begin
  if not IsActive then exit;
  FOffset := GetOffset(APoint);
  FTimer.Enabled := FOffset <> Point(0, 0);
end;

procedure TPanClickTool.MouseUp(APoint: TPoint);
begin
  Unused(APoint);
  FTimer.Enabled := false;
  Deactivate;
  Handled;
end;

procedure TPanClickTool.OnTimer(ASender: TObject);
begin
  Unused(ASender);
  if FOffset <> Point(0, 0) then
    PanBy(FOffset);
end;

{ TPanMouseWheelTool }

constructor TPanMouseWheelTool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetPropDefaults(Self, ['Step', 'WheelUpDirection']);
end;

procedure TPanMouseWheelTool.DoPan(AStep: Integer);
const
  DIR_TO_OFFSET: array [TPanDirection] of TPoint =
    // pdLeft, pdUp, pdRight, pdDown
    ((X: -1; Y: 0), (X: 0; Y: -1), (X: 1; Y: 0), (X: 0; Y: 1));
begin
  PanBy(DIR_TO_OFFSET[WheelUpDirection] * AStep);
end;

procedure TPanMouseWheelTool.MouseWheelDown(APoint: TPoint);
begin
  Unused(APoint);
  DoPan(-Step);
end;

procedure TPanMouseWheelTool.MouseWheelUp(APoint: TPoint);
begin
  Unused(APoint);
  DoPan(Step);
end;

{ TDataPointTool }

constructor TDataPointTool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAffectedSeries.Init;
  SetPropDefaults(Self, ['GrabRadius']);
  FPointIndex := -1;
end;

procedure TDataPointTool.FindNearestPoint(APoint: TPoint);
const
  DIST_FUNCS: array [TChartDistanceMode] of TPointDistFunc = (
    @PointDist, @PointDistX, @PointDistY);
var
  s, bestS: TCustomChartSeries;
  p: TNearestPointParams;
  cur, best: TNearestPointResults;
begin
  p.FDistFunc := DIST_FUNCS[DistanceMode];
  p.FPoint := APoint;
  p.FRadius := GrabRadius;
  best.FDist := MaxInt;
  for s in CustomSeries(FChart, FAffectedSeries.AsBooleans(FChart.SeriesCount)) do
    if
      s.GetNearestPoint(p, cur) and PtInRect(FChart.ClipRect, cur.FImg) and
      (cur.FDist < best.FDist)
    then begin
      bestS := s;
      best := cur;
    end;
  if best.FDist = MaxInt then exit;
  FSeries := bestS;
  FPointIndex := best.FIndex;
  FNearestGraphPoint := FChart.ImageToGraph(best.FImg);
end;

function TDataPointTool.GetAffectedSeries: String;
begin
  Result := FAffectedSeries.AsString;
end;

function TDataPointTool.GetIsSeriesAffected(AIndex: Integer): Boolean;
begin
  Result := FAffectedSeries.IsSet[AIndex];
end;

procedure TDataPointTool.SetAffectedSeries(AValue: String);
begin
  FAffectedSeries.AsString := AValue;
end;

procedure TDataPointTool.SetIsSeriesAffected(AIndex: Integer; AValue: Boolean);
begin
  FAffectedSeries.IsSet[AIndex] := AValue;
end;

{ TDataPointDragTool }

constructor TDataPointDragTool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActiveCursor := crSizeAll;
end;

procedure TDataPointDragTool.MouseDown(APoint: TPoint);
begin
  FindNearestPoint(APoint);
  if FSeries = nil then exit;
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

{ TDataPointClickTool }

procedure TDataPointClickTool.MouseDown(APoint: TPoint);
begin
  FindNearestPoint(APoint);
  if FSeries = nil then exit;
  FMouseDownPoint := APoint;
  Activate;
  Handled;
end;

procedure TDataPointClickTool.MouseUp(APoint: TPoint);
begin
  if
    Assigned(OnPointClick) and (FSeries <> nil) and
    (PointDist(APoint, FMouseDownPoint) <= Sqr(GrabRadius))
  then
    OnPointClick(Self, FMouseDownPoint);
  FSeries := nil;
  Deactivate;
  Handled;
end;

{ TDataPointHintTool }

constructor TDataPointHintTool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUseDefaultHintText := true;
end;

procedure TDataPointHintTool.MouseMove(APoint: TPoint);

  function GetHintText: String;
  begin
    if UseDefaultHintText then begin
      if Series is TChartSeries then
        Result := (Series as TChartSeries).FormattedMark(PointIndex)
      else
        Result := Format('%s: %d', [Series.Title, PointIndex]);
    end;
    if Assigned(OnHint) then
      OnHint(Self, APoint, Result);
  end;

begin
  FSeries := nil;
  FindNearestPoint(APoint);
  if Series = nil then begin
    FChart.ShowHint := false;
    Application.CancelHint;
    RestoreCursor;
    FPrevSeries := nil;
    exit;
  end;
  if (FPrevSeries = Series) and (FPrevPointIndex = PointIndex) then
    exit;
  if FPrevSeries = nil then
    SetCursor;
  FPrevSeries := Series;
  FPrevPointIndex := PointIndex;
  FChart.Hint := GetHintText;
  FChart.ShowHint := FChart.Hint <> '';
  if not FChart.ShowHint then exit;
  Application.HintPause := 0;
  Application.ActivateHint(FChart.ClientToScreen(APoint));
end;

{ TDataPointCrosshairTool }

constructor TDataPointCrosshairTool.Create(AOwner: TComponent);
begin
  inherited;
  SetPropDefaults(Self, ['Shape', 'Size']);
  FCrosshairPen := TChartPen.Create;
end;

destructor TDataPointCrosshairTool.Destroy;
begin
  FreeAndNil(FCrosshairPen);
  inherited;
end;

procedure TDataPointCrosshairTool.DoDraw;
var
  p: TPoint;
begin
  p := FChart.GraphToImage(Position);
  if Shape in [ccsVertical, ccsCross] then
    if Size < 0 then
      FChart.DrawLineVert(FChart.Drawer, p.X)
    else
      FChart.Drawer.Line(p - Point(0, Size), p + Point(0, Size));
  if Shape in [ccsHorizontal, ccsCross] then
    if Size < 0 then
      FChart.DrawLineHoriz(FChart.Drawer, p.Y)
    else
      FChart.Drawer.Line(p - Point(Size, 0), p + Point(Size, 0));
  if Assigned(OnDraw) then
    OnDraw(Self);
end;

procedure TDataPointCrosshairTool.DoHide;
begin
  if FSeries = nil then exit;
  FSeries := nil;
  case EffectiveDrawingMode of
    tdmXor: begin
      PrepareXorPen(FChart.Canvas);
      DoDraw;
    end;
    tdmNormal:
      FChart.StyleChanged(Self);
  end;
end;

procedure TDataPointCrosshairTool.Draw(AChart: TChart; ADrawer: IChartDrawer);
begin
  if FSeries = nil then exit;
  inherited;
  case EffectiveDrawingMode of
    tdmXor:
      PrepareXorPen(FChart.Canvas);
    tdmNormal:
      FChart.Drawer.Pen := CrosshairPen;
  end;
  DoDraw;
end;

procedure TDataPointCrosshairTool.Hide;
begin
  DoHide;
  FChart := nil;
end;

procedure TDataPointCrosshairTool.KeyDown(APoint: TPoint);
begin
  MouseMove(APoint);
end;

procedure TDataPointCrosshairTool.MouseMove(APoint: TPoint);
begin
  DoHide;
  FindNearestPoint(APoint);
  if FSeries = nil then exit;
  FPosition := FNearestGraphPoint;
  if EffectiveDrawingMode = tdmXor then begin
    PrepareXorPen(FChart.Canvas);
    DoDraw;
  end;
end;

procedure TDataPointCrosshairTool.SetCrosshairPen(AValue: TChartPen);
begin
  FCrosshairPen.Assign(AValue);
end;

initialization

  ToolsClassRegistry := TStringList.Create;
  OnInitBuiltinTools := @InitBuitlinTools;
  RegisterChartToolClass(TZoomDragTool, 'Zoom by drag');
  RegisterChartToolClass(TZoomClickTool, 'Zoom by click');
  RegisterChartToolClass(TZoomMouseWheelTool, 'Zoom by mouse wheel');
  RegisterChartToolClass(TPanDragTool, 'Panning by drag');
  RegisterChartToolClass(TPanClickTool, 'Panning by click');
  RegisterChartToolClass(TPanMouseWheelTool, 'Panning by mouse wheel');
  RegisterChartToolClass(TReticuleTool, 'Reticule');
  RegisterChartToolClass(TDataPointClickTool, 'Data point click');
  RegisterChartToolClass(TDataPointDragTool, 'Data point drag');
  RegisterChartToolClass(TDataPointHintTool, 'Data point hint');
  RegisterChartToolClass(TDataPointCrosshairTool, 'Data point crosshair');
  RegisterChartToolClass(TUserDefinedTool, 'User-defined');

finalization

  FreeAndNil(ToolsClassRegistry);

end.

