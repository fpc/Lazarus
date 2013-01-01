unit ADLayoutViewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, math, Controls, Graphics, AnchorDockStorage;

type
  TADLTVMonitor = class
  public
    Monitor: integer;
    WorkArea: TRect;
    Bounds: TRect; // hull of all windows on this monitor
    X, Y: integer;
  end;

  { TADLayoutTreeView }

  TADLayoutTreeView = class(TCustomControl)
  private
    FLayout: TAnchorDockLayoutTree;
    FScale: double;
    FBounds: TRect;
    FScaledBounds: TRect;
    FScaledScroll: TPoint;
    FMonitors: array of TADLTVMonitor;
    function GetLayoutMaxX: integer;
    function GetLayoutMaxY: integer;
    function GetLayoutMinX: integer;
    function GetLayoutMinY: integer;
    function GetScaledMaxX: integer;
    function GetScaledMaxY: integer;
    function GetScaledMinX: integer;
    function GetScaledMinY: integer;
    function GetScaledOffsetX: integer;
    function GetScaledOffsetY: integer;
    procedure SetScale(AValue: double);
    procedure SetScaledOffsetX(AValue: integer);
    procedure SetScaledOffsetY(AValue: integer);
    procedure ComputeLayout;
    procedure ClearMonitors;
    function FindMonitor(Monitor: integer): TADLTVMonitor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
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
  end;

implementation

{ TADLayoutTreeView }

procedure TADLayoutTreeView.SetScale(AValue: double);
begin
  if FScale=AValue then Exit;
  FScale:=AValue;
end;

function TADLayoutTreeView.GetLayoutMaxX: integer;
begin
  Result:=FBounds.Right;
end;

function TADLayoutTreeView.GetLayoutMaxY: integer;
begin
  Result:=FBounds.Bottom;
end;

function TADLayoutTreeView.GetLayoutMinX: integer;
begin
  Result:=FBounds.Left;
end;

function TADLayoutTreeView.GetLayoutMinY: integer;
begin
  Result:=FBounds.Top;
end;

function TADLayoutTreeView.GetScaledMaxX: integer;
begin
  Result:=FScaledBounds.Right;
end;

function TADLayoutTreeView.GetScaledMaxY: integer;
begin
  Result:=FScaledBounds.Bottom;
end;

function TADLayoutTreeView.GetScaledMinX: integer;
begin
  Result:=FScaledBounds.Left;
end;

function TADLayoutTreeView.GetScaledMinY: integer;
begin
  Result:=FScaledBounds.Top;
end;

function TADLayoutTreeView.GetScaledOffsetX: integer;
begin
  Result:=FScaledScroll.X;
end;

function TADLayoutTreeView.GetScaledOffsetY: integer;
begin
  Result:=FScaledScroll.Y;
end;

procedure TADLayoutTreeView.SetScaledOffsetX(AValue: integer);
begin
  if FScaledScroll.X=AValue then Exit;
  FScaledScroll.X:=AValue;
end;

procedure TADLayoutTreeView.SetScaledOffsetY(AValue: integer);
begin
  if FScaledScroll.Y=AValue then Exit;
  FScaledScroll.Y:=AValue;
end;

procedure TADLayoutTreeView.ComputeLayout;

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
  end;
end;

procedure TADLayoutTreeView.ClearMonitors;
var
  i: Integer;
begin
  for i:=0 to length(FMonitors)-1 do FreeAndNil(FMonitors[i]);
  SetLength(FMonitors,0);
end;

function TADLayoutTreeView.FindMonitor(Monitor: integer): TADLTVMonitor;
var
  i: Integer;
begin
  for i:=0 to length(FMonitors)-1 do begin
    Result:=FMonitors[i];
    if Result.Monitor=Monitor then exit;
  end;
  Result:=nil;
end;

constructor TADLayoutTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLayout:=TAnchorDockLayoutTree.Create;
end;

destructor TADLayoutTreeView.Destroy;
begin
  FreeAndNil(FLayout);
  ClearMonitors;
  inherited Destroy;
end;

procedure TADLayoutTreeView.Paint;
begin
  Canvas.Brush.Color:=clWhite;
  Canvas.FillRect(0,0,ClientWidth,ClientHeight);

  // call event
  inherited Paint;
end;

procedure TADLayoutTreeView.LayoutChanged;
begin
  ComputeLayout;
end;

end.

