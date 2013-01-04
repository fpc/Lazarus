unit ADLayoutViewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, math, Controls, Graphics, ComCtrls,
  AnchorDockStorage, LazLogger;

type
  TADLTVMonitor = class
  public
    Monitor: integer;
    WorkArea: TRect;
    Bounds: TRect; // hull of all windows on this monitor
    X, Y: integer;
  end;

  { TADCustomLayoutTreeView }

  TADCustomLayoutTreeView = class(TCustomControl)
  private
    FLayout: TAnchorDockLayoutTree;
    FScaleMax: double;
    FScaleMin: double;
    FScale: double;
    FBounds: TRect;
    FScaledBounds: TRect;
    FScaledScroll: TPoint;
    FMonitors: array of TADLTVMonitor;
    FZoomTrackbar: TCustomTrackBar;
    FOldZoombarOnChange: TNotifyEvent;
    FIgnoreZoomTrackbarChange: boolean;
    function GetLayoutMaxX: integer;
    function GetLayoutMaxY: integer;
    function GetLayoutMinX: integer;
    function GetLayoutMinY: integer;
    function GetMonitors(Index: integer): TADLTVMonitor;
    function GetScaledMaxX: integer;
    function GetScaledMaxY: integer;
    function GetScaledMinX: integer;
    function GetScaledMinY: integer;
    function GetScaledOffsetX: integer;
    function GetScaledOffsetY: integer;
    procedure SetScaleMax(AValue: double);
    procedure SetScaleMin(AValue: double);
    procedure SetScale(AValue: double);
    procedure SetScaledOffsetX(AValue: integer);
    procedure SetScaledOffsetY(AValue: integer);
    procedure ComputeLayout;
    procedure ClearMonitors;
    function FindMonitor(Monitor: integer): TADLTVMonitor;
    procedure SetZoomTrackbar(AValue: TCustomTrackBar);
    procedure UpdateZoomTrackBar;
    procedure ZoomTrackbarChange(Sender: TObject);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    function ScaleRect(const r: TRect): TRect;
    procedure LayoutChanged; virtual;
    property Layout: TAnchorDockLayoutTree read FLayout;
    property Scale: double read FScale write SetScale;
    property ScaledOffsetX: integer read GetScaledOffsetX write SetScaledOffsetX;
    property ScaledOffsetY: integer read GetScaledOffsetY write SetScaledOffsetY;
    property LayoutMinX: integer read GetLayoutMinX;
    property LayoutMinY: integer read GetLayoutMinY;
    property LayoutMaxX: integer read GetLayoutMaxX;
    property LayoutMaxY: integer read GetLayoutMaxY;
    property ScaledMinX: integer read GetScaledMinX;
    property ScaledMinY: integer read GetScaledMinY;
    property ScaledMaxX: integer read GetScaledMaxX;
    property ScaledMaxY: integer read GetScaledMaxY;
    function MonitorCount: integer;
    property Monitors[Index: integer]: TADLTVMonitor read GetMonitors;
    property ZoomTrackbar: TCustomTrackBar read FZoomTrackbar write SetZoomTrackbar;
    function ScaleToZoomTrackBarPos(aScale: double): integer;
    function ZoomTrackBarPosToScale(p: integer): double;
    property ScaleMin: double read FScaleMin write SetScaleMin;
    property ScaleMax: double read FScaleMax write SetScaleMax;
  end;

  TADLayoutTreeView = class(TADCustomLayoutTreeView)
  published
    property Scale: double read FScale write SetScale;
    property ScaledOffsetX: integer read GetScaledOffsetX write SetScaledOffsetX;
    property ScaledOffsetY: integer read GetScaledOffsetY write SetScaledOffsetY;
    property ZoomTrackbar: TCustomTrackBar read FZoomTrackbar write SetZoomTrackbar;
    property ScaleMin: double read FScaleMin write SetScaleMin;
    property ScaleMax: double read FScaleMax write SetScaleMax;
  end;

implementation

{ TADCustomLayoutTreeView }

procedure TADCustomLayoutTreeView.SetScale(AValue: double);
begin
  AValue:=Min(ScaleMax,Max(AValue,ScaleMin));
  if FScale=AValue then Exit;
  FScale:=AValue;
  FScaledBounds:=ScaleRect(FBounds);
  UpdateZoomTrackBar;
  Invalidate;
end;

procedure TADCustomLayoutTreeView.ZoomTrackbarChange(Sender: TObject);
begin
  if not FIgnoreZoomTrackbarChange then
    Scale:=ZoomTrackBarPosToScale(ZoomTrackbar.Position);
  if Assigned(FOldZoombarOnChange) then
    FOldZoombarOnChange(Sender);
end;

function TADCustomLayoutTreeView.GetLayoutMaxX: integer;
begin
  Result:=FBounds.Right;
end;

function TADCustomLayoutTreeView.GetLayoutMaxY: integer;
begin
  Result:=FBounds.Bottom;
end;

function TADCustomLayoutTreeView.GetLayoutMinX: integer;
begin
  Result:=FBounds.Left;
end;

function TADCustomLayoutTreeView.GetLayoutMinY: integer;
begin
  Result:=FBounds.Top;
end;

function TADCustomLayoutTreeView.GetMonitors(Index: integer): TADLTVMonitor;
begin
  Result:=FMonitors[Index];
end;

function TADCustomLayoutTreeView.GetScaledMaxX: integer;
begin
  Result:=FScaledBounds.Right;
end;

function TADCustomLayoutTreeView.GetScaledMaxY: integer;
begin
  Result:=FScaledBounds.Bottom;
end;

function TADCustomLayoutTreeView.GetScaledMinX: integer;
begin
  Result:=FScaledBounds.Left;
end;

function TADCustomLayoutTreeView.GetScaledMinY: integer;
begin
  Result:=FScaledBounds.Top;
end;

function TADCustomLayoutTreeView.GetScaledOffsetX: integer;
begin
  Result:=FScaledScroll.X;
end;

function TADCustomLayoutTreeView.GetScaledOffsetY: integer;
begin
  Result:=FScaledScroll.Y;
end;

procedure TADCustomLayoutTreeView.SetScaleMax(AValue: double);
// must be >=1.0
begin
  AValue:=Max(AValue,1.0);
  if FScaleMax=AValue then Exit;
  FScaleMax:=AValue;
  Scale:=Min(ScaleMax,Max(Scale,ScaleMin));
  UpdateZoomTrackBar;
end;

procedure TADCustomLayoutTreeView.SetScaleMin(AValue: double);
// must be between 0.00001 and 1.0
begin
  AValue:=Min(1.0,Max(AValue,0.00001));
  if FScaleMin=AValue then Exit;
  FScaleMin:=AValue;
  Scale:=Min(ScaleMax,Max(Scale,ScaleMin));
  UpdateZoomTrackBar;
end;

procedure TADCustomLayoutTreeView.SetScaledOffsetX(AValue: integer);
begin
  if FScaledScroll.X=AValue then Exit;
  FScaledScroll.X:=AValue;
  Invalidate;
end;

procedure TADCustomLayoutTreeView.SetScaledOffsetY(AValue: integer);
begin
  if FScaledScroll.Y=AValue then Exit;
  FScaledScroll.Y:=AValue;
  Invalidate;
end;

procedure TADCustomLayoutTreeView.ComputeLayout;

  procedure ComputeMonitors(Node: TAnchorDockLayoutTreeNode);
  var
    i: Integer;
    Monitor: TADLTVMonitor;
    r: TRect;
  begin
    if Node=nil then exit;
    if Node.NodeType in [adltnLayout,adltnCustomSite] then begin
      // top level window
      Monitor:=FindMonitor(Node.Monitor);
      if Monitor=nil then begin
        // first window on this monitor
        Monitor:=TADLTVMonitor.Create;
        Monitor.Monitor:=Node.Monitor;
        Monitor.WorkArea:=Node.WorkAreaRect;
        Monitor.Bounds:=Node.BoundsRect;
        SetLength(FMonitors,length(FMonitors)+1);
        FMonitors[length(FMonitors)-1]:=Monitor;
      end else begin
        // another window on this monitor
        r:=Rect(0,0,0,0);
        UnionRect(r,Monitor.Bounds,Node.BoundsRect);
        Monitor.Bounds:=r;
      end;
    end else begin
      for i:=0 to Node.Count-1 do
        ComputeMonitors(Node[i]);
    end;
  end;

var
  i: Integer;
  Monitor: TADLTVMonitor;
  TileSize: TPoint;
  TileCols: Integer;
  LeftMost: Integer;
  TopMost: Integer;
  TileCol: Integer;
  TileRow: Integer;
  TileLeft: Integer;
  TileTop: Integer;
  r: TRect;
begin
  if FLayout=nil then exit;
  // clear computed values
  FBounds:=Rect(0,0,0,0);
  FScaledBounds:=Rect(0,0,0,0);
  ClearMonitors;
  // collect all monitor bounds
  ComputeMonitors(FLayout.Root);
  if length(FMonitors)=0 then exit;
  // get biggest monitor
  TileSize:=Point(0,0);
  for i:=0 to length(FMonitors)-1 do begin
    Monitor:=FMonitors[i];
    Monitor.Bounds.Right:=Max(Monitor.Bounds.Left,Monitor.Bounds.Right);
    Monitor.Bounds.Bottom:=Max(Monitor.Bounds.Top,Monitor.Bounds.Bottom);
    TileSize.X:=Max(TileSize.X,Monitor.Bounds.Right-Monitor.Bounds.Left);
    TileSize.X:=Max(TileSize.X,Monitor.WorkArea.Right-Monitor.WorkArea.Left);
    TileSize.Y:=Max(TileSize.Y,Monitor.Bounds.Bottom-Monitor.Bounds.Top);
    TileSize.Y:=Max(TileSize.Y,Monitor.WorkArea.Bottom-Monitor.WorkArea.Top);
  end;
  // put monitors into tiles of same size in a quadratic layout
  TileCols:=ceil(SQRT(length(FMonitors)));
  for i:=0 to length(FMonitors)-1 do begin
    Monitor:=FMonitors[i];
    LeftMost:=Min(Monitor.WorkArea.Left,Monitor.Bounds.Left);
    TopMost:=Min(Monitor.WorkArea.Top,Monitor.Bounds.Top);
    TileCol:=i mod TileCols;
    TileRow:=i div TileCols;
    TileLeft:=TileCol*TileSize.X;
    TileTop:=TileRow*TileSize.Y;
    // move left/topmost to left,top of tile
    Monitor.X:=TileLeft-LeftMost;
    Monitor.Y:=TileTop-TopMost;
    // compute total bounds
    r.Left:=Monitor.X+Min(Monitor.WorkArea.Left,Monitor.Bounds.Left);
    r.Top:=Monitor.Y+Min(Monitor.WorkArea.Top,Monitor.Bounds.Top);
    r.Right:=Monitor.X+Min(Monitor.WorkArea.Right,Monitor.Bounds.Right);
    r.Bottom:=Monitor.Y+Min(Monitor.WorkArea.Bottom,Monitor.Bounds.Bottom);
    if i=0 then
      FBounds:=r
    else begin
      FBounds.Left:=Min(FBounds.Left,r.Left);
      FBounds.Right:=Max(FBounds.Right,r.Right);
      FBounds.Top:=Min(FBounds.Top,r.Top);
      FBounds.Bottom:=Max(FBounds.Bottom,r.Bottom);
    end;
    DebugLn(['TADLayoutTreeView.ComputeLayout ',i,'/',length(FMonitors),' WorkArea=',dbgs(Monitor.WorkArea),' Bounds=',dbgs(Monitor.Bounds),' X=',Monitor.X,' Y=',Monitor.Y]);
  end;
  FScaledBounds:=ScaleRect(FBounds);
end;

procedure TADCustomLayoutTreeView.ClearMonitors;
var
  i: Integer;
begin
  for i:=0 to length(FMonitors)-1 do FreeAndNil(FMonitors[i]);
  SetLength(FMonitors,0);
end;

function TADCustomLayoutTreeView.FindMonitor(Monitor: integer): TADLTVMonitor;
var
  i: Integer;
begin
  for i:=0 to length(FMonitors)-1 do begin
    Result:=FMonitors[i];
    if Result.Monitor=Monitor then exit;
  end;
  Result:=nil;
end;

procedure TADCustomLayoutTreeView.SetZoomTrackbar(AValue: TCustomTrackBar);
begin
  if FZoomTrackbar=AValue then Exit;
  if ZoomTrackbar<>nil then begin
    ZoomTrackbar.OnChange:=FOldZoombarOnChange;
  end;
  FZoomTrackbar:=AValue;
  if ZoomTrackbar<>nil then begin
    FreeNotification(ZoomTrackbar);
    fOldZoombarOnChange:=ZoomTrackbar.OnChange;
    ZoomTrackbar.OnChange:=@ZoomTrackbarChange;
    UpdateZoomTrackBar;
  end;
end;

procedure TADCustomLayoutTreeView.UpdateZoomTrackBar;
var
  OldChange: Boolean;
begin
  OldChange:=FIgnoreZoomTrackbarChange;
  try
    FIgnoreZoomTrackbarChange:=true;
    //debugln(['TADCustomLayoutTreeView.UpdateZoomTrackBar Scale=',Scale,' Zoom=',ScaleToZoomTrackBarPos(Scale)]);
    ZoomTrackbar.Position:=ScaleToZoomTrackBarPos(Scale);
  finally
    FIgnoreZoomTrackbarChange:=OldChange;
  end;
  ZoomTrackbar.Caption:='';
end;

procedure TADCustomLayoutTreeView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then begin
    if AComponent=ZoomTrackbar then
      ZoomTrackbar:=nil;
  end;
end;

constructor TADCustomLayoutTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLayout:=TAnchorDockLayoutTree.Create;
  FScale:=0.25;
  FScaleMin:=1/20;
  FScaleMax:=5;
end;

destructor TADCustomLayoutTreeView.Destroy;
begin
  FreeAndNil(FLayout);
  ClearMonitors;
  inherited Destroy;
end;

procedure TADCustomLayoutTreeView.Paint;

  procedure DrawContent(Node: TAnchorDockLayoutTreeNode;
    OriginX, OriginY: integer);
  var
    r: TRect;
    i: Integer;
  begin
    r:=Node.BoundsRect;
    OffsetRect(r,OriginX,OriginY);
    case Node.NodeType of
    adltnLayout,adltnCustomSite:
      begin
        Canvas.Brush.Color:=clWhite;
        Canvas.Pen.Color:=clBlue;
      end;
    adltnControl:
      begin
        Canvas.Brush.Color:=clGray;
        Canvas.Pen.Color:=clBlack;
      end;
    adltnSplitterHorizontal:
      begin
        Canvas.Brush.Color:=clGreen;
        Canvas.Pen.Color:=clGreen;
      end;
    adltnSplitterVertical:
      begin
        Canvas.Brush.Color:=clLime;
        Canvas.Pen.Color:=clLime;
      end;
    adltnPages:
      begin
        Canvas.Brush.Color:=clYellow;
        Canvas.Pen.Color:=clYellow;
      end;
    else
      exit;
    end;
    Canvas.Rectangle(ScaleRect(r));
    if Node.NodeType in [adltnLayout] then begin
      for i:=0 to Node.Count-1 do
        DrawContent(Node[i],r.Left,r.Top);
    end;
  end;

  procedure DrawWindows(Node: TAnchorDockLayoutTreeNode);
  var
    i: Integer;
    Monitor: TADLTVMonitor;
    r: TRect;
  begin
    if Node.NodeType in [adltnCustomSite,adltnLayout] then begin
      // top level window
      Monitor:=FindMonitor(Node.Monitor);
      if Monitor=nil then exit;
      r:=Node.BoundsRect;
      OffsetRect(r,Monitor.X,Monitor.Y);
      Canvas.Brush.Color:=clWhite;
      Canvas.Pen.Color:=clRed;
      Canvas.Rectangle(ScaleRect(r));
      for i:=0 to Node.Count-1 do
        DrawContent(Node[i],r.Left,r.Top);
    end else begin
      for i:=0 to Node.Count-1 do
        DrawWindows(Node[i]);
    end;
  end;

var
  i: Integer;
  Monitor: TADLTVMonitor;
  r: TRect;
begin
  Canvas.Brush.Color:=clGray;
  Canvas.FillRect(0,0,ClientWidth,ClientHeight);

  // draw monitor workareas
  Canvas.Pen.Color:=clBlue;
  Canvas.Brush.Color:=RGBToColor(128,128,255);
  for i:=0 to MonitorCount-1 do begin
    Monitor:=fMonitors[i];
    r:=ScaleRect(Monitor.WorkArea);
    Canvas.Rectangle(r);
  end;

  DrawWindows(Layout.Root);

  // call event
  inherited Paint;
end;

function TADCustomLayoutTreeView.ScaleRect(const r: TRect): TRect;
begin
  Result.Left:=floor(r.Left*Scale)+FScaledScroll.X;
  Result.Top:=floor(r.Top*Scale)+FScaledScroll.Y;
  Result.Right:=ceil(r.Right*Scale)+FScaledScroll.X;
  Result.Bottom:=ceil(r.Bottom*Scale)+FScaledScroll.Y;
end;

procedure TADCustomLayoutTreeView.LayoutChanged;
begin
  ComputeLayout;
end;

function TADCustomLayoutTreeView.MonitorCount: integer;
begin
  Result:=length(FMonitors);
end;

function TADCustomLayoutTreeView.ScaleToZoomTrackBarPos(aScale: double
  ): integer;
var
  lnMinPos, lnMaxPos, lnPos, Percent: Double;
begin
  // ZoomTrackbar.Min corresponds to ln(ScaleMin)
  // ZoomTrackbar.Max corresponds to ln(ScaleMax)
  lnMinPos:=ln(ScaleMin);
  lnMaxPos:=ln(ScaleMax);
  lnPos:=ln(aScale);
  Percent:=(lnPos-lnMinPos)/(lnMaxPos-lnMinPos);
  Result:=ZoomTrackbar.Min+round(Percent*(ZoomTrackbar.Max-ZoomTrackbar.Min));
  //debugln(['TADCustomLayoutTreeView.ScaleToZoomTrackBarPos ScaleMin=',ScaleMin,' ScaleMax=',ScaleMax,' lnMinPos=',lnMinPos,' lnMaxPos=',lnMaxPos,' lnPos=',lnPos,' Percent=',Percent,' TrackBar=Min=',ZoomTrackbar.Min,',Max=',ZoomTrackbar.Max,' Result=',Result]);
  // avoid out of bounds due to rounding errors
  Result:=Min(ZoomTrackbar.Max,Max(ZoomTrackbar.Min,Result));
end;

function TADCustomLayoutTreeView.ZoomTrackBarPosToScale(p: integer): double;
var
  lnMinPos, lnMaxPos, lnPos, Percent: Double;
begin
  // ZoomTrackbar.Min corresponds to ln(ScaleMin)
  // ZoomTrackbar.Max corresponds to ln(ScaleMax)
  lnMinPos:=ln(ScaleMin);
  lnMaxPos:=ln(ScaleMax);
  Percent:=(p-ZoomTrackbar.Min)/(ZoomTrackbar.Max-ZoomTrackbar.Min);
  lnPos:=lnMinPos+Percent*(lnMaxPos-lnMinPos);
  Result:=exp(lnPos);
  //debugln(['TADCustomLayoutTreeView.ZoomTrackBarPosToScale ScaleMin=',ScaleMin,' ScaleMax=',ScaleMax,' lnMinPos=',lnMinPos,' lnMaxPos=',lnMaxPos,' TrackBar=Min=',ZoomTrackbar.Min,',Max=',ZoomTrackbar.Max,',Position=',ZoomTrackbar.Position,' Percent=',Percent,' lnPos=',lnPos,' Result=',Result]);
  // avoid out of bounds due to rounding errors
  Result:=Max(ScaleMin,Min(ScaleMax,Result));
end;

end.

