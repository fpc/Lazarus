unit ADLayoutViewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, AnchorDockStorage;

type

  { TADLayoutTreeView }

  TADLayoutTreeView = class(TCustomControl)
  private
    FLayout: TAnchorDockLayoutTree;
    FLayoutMaxX: integer;
    FLayoutMaxY: integer;
    FLayoutMinX: integer;
    FLayoutMinY: integer;
    FScale: double;
    FScaledMaxX: integer;
    FScaledMaxY: integer;
    FScaledMinX: integer;
    FScaledMinY: integer;
    FScaledOffsetX: integer;
    FScaledOffsetY: integer;
    procedure SetScale(AValue: double);
    procedure SetScaledOffsetX(AValue: integer);
    procedure SetScaledOffsetY(AValue: integer);
    procedure ComputeLayout;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure LayoutChanged; virtual;
    property Layout: TAnchorDockLayoutTree read FLayout;
    property Scale: double read FScale write SetScale;
    property ScaledOffsetX: integer read FScaledOffsetX write SetScaledOffsetX;
    property ScaledOffsetY: integer read FScaledOffsetY write SetScaledOffsetY;
    property LayoutMinX: integer read FLayoutMinX;
    property LayoutMinY: integer read FLayoutMinY;
    property LayoutMaxX: integer read FLayoutMaxX;
    property LayoutMaxY: integer read FLayoutMaxY;
    property ScaledMinX: integer read FScaledMinX;
    property ScaledMinY: integer read FScaledMinY;
    property ScaledMaxX: integer read FScaledMaxX;
    property ScaledMaxY: integer read FScaledMaxY;
  end;

implementation

{ TADLayoutTreeView }

procedure TADLayoutTreeView.SetScale(AValue: double);
begin
  if FScale=AValue then Exit;
  FScale:=AValue;
end;

procedure TADLayoutTreeView.SetScaledOffsetX(AValue: integer);
begin
  if FScaledOffsetX=AValue then Exit;
  FScaledOffsetX:=AValue;
end;

procedure TADLayoutTreeView.SetScaledOffsetY(AValue: integer);
begin
  if FScaledOffsetY=AValue then Exit;
  FScaledOffsetY:=AValue;
end;

procedure TADLayoutTreeView.ComputeLayout;
begin
  if FLayout=nil then exit;

end;

constructor TADLayoutTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLayout:=TAnchorDockLayoutTree.Create;
end;

destructor TADLayoutTreeView.Destroy;
begin
  FreeAndNil(FLayout);
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

