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

unit TANavigation;

{$H+}

interface

uses
  Classes, StdCtrls, TAChartUtils, TAGraph;

type

  { TChartNavScrollBar }

  TChartNavScrollBar = class (TCustomScrollBar)
  private
    FAutoPageSize: Boolean;
    FChart: TChart;
    FListener: TListener;
    procedure ChartExtentChanged(ASender: TObject);
    procedure SetAutoPageSize(AValue: Boolean);
    procedure SetChart(AValue: TChart);
  protected
    procedure Scroll(
      AScrollCode: TScrollCode; var AScrollPos: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AutoPageSize: Boolean
      read FAutoPageSize write SetAutoPageSize default false;
    property Chart: TChart read FChart write SetChart;
  published
    property Align;
    property Anchors;
    property BidiMode;
    property BorderSpacing;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Kind;
    property LargeChange;
    property Max;
    property Min;
    property PageSize;
    property ParentBidiMode;
    property ParentShowHint;
    property PopupMenu;
    property Position;
    property ShowHint;
    property SmallChange;
    property TabOrder;
    property TabStop;
    property Visible;
  published
    property OnChange;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnScroll;
    property OnStartDrag;
    property OnUTF8KeyPress;
  end;

procedure Register;

implementation

uses
  Forms, SysUtils;

procedure Register;
begin
  RegisterComponents(CHART_COMPONENT_IDE_PAGE, [TChartNavScrollBar]);
end;

{ TChartNavScrollBar }

procedure TChartNavScrollBar.ChartExtentChanged(ASender: TObject);
var
  fe, le: TDoubleRect;
  fw, lw: Double;
begin
  Unused(ASender);
  if FChart = nil then exit;
  fe := FChart.GetFullExtent;
  le := FChart.LogicalExtent;
  case Kind of
    sbHorizontal: begin
      fw := fe.b.X - fe.a.X;
      if fw <= 0 then
        Position := 0
      else
        Position := Round(WeightedAverage(Min, Max, (le.a.X - fe.a.X) / fw));
      lw := le.b.X - le.a.X;
    end;
    sbVertical: begin
      fw := fe.b.Y - fe.a.Y;
      if fw <= 0 then
        Position := 0
      else
        Position := Round(WeightedAverage(Max, Min, (le.a.Y - fe.a.Y) / fw));
      lw := le.b.Y - le.a.Y;
    end;
  end;
  if AutoPageSize and not (csDesigning in ComponentState) then
    PageSize := Round(lw / fw * (Max - Min));
end;

constructor TChartNavScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FListener := TListener.Create(@FChart, @ChartExtentChanged);
end;

destructor TChartNavScrollBar.Destroy;
begin
  FreeAndNil(FListener);
  inherited Destroy;
end;

procedure TChartNavScrollBar.Scroll(
  AScrollCode: TScrollCode; var AScrollPos: Integer);
var
  fe, le: TDoubleRect;
  d, w: Double;
begin
  inherited Scroll(AScrollCode, AScrollPos);
  if FChart = nil then exit;
  w := Max - Min;
  if w = 0 then exit;
  fe := FChart.GetFullExtent;
  le := FChart.LogicalExtent;
  case Kind of
    sbHorizontal: begin
      d := WeightedAverage(fe.a.X, fe.b.X, Position / w);
      le.b.X += d - le.a.X;
      le.a.X := d;
    end;
    sbVertical: begin
      d := WeightedAverage(fe.b.Y, fe.a.Y, Position / w);
      le.b.Y += d - le.a.Y;
      le.a.Y := d;
    end;
  end;
  FChart.LogicalExtent := le;
  // Focused ScrollBar is glitchy under Win32, especially after PageSize change.
  FChart.SetFocus;
end;

procedure TChartNavScrollBar.SetAutoPageSize(AValue: Boolean);
begin
  if FAutoPageSize = AValue then exit;
  FAutoPageSize := AValue;
  ChartExtentChanged(Self);
end;

procedure TChartNavScrollBar.SetChart(AValue: TChart);
begin
  if FChart = AValue then exit;

  if FListener.IsListening then
    FChart.ExtentBroadcaster.Unsubscribe(FListener);
  FChart := AValue;
  if FChart <> nil then
    FChart.ExtentBroadcaster.Subscribe(FListener);
  ChartExtentChanged(Self);
end;

end.

