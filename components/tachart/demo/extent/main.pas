unit main;

{$mode objfpc}{$H+}

interface

uses
  ExtCtrls, Spin, StdCtrls, Forms, TAGraph, TASeries, TASources, TATools,
  TATypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnPrevExtent: TButton;
    cgUseBounds: TCheckGroup;
    Chart1: TChart;
    ChartToolset1: TChartToolset;
    ChartToolset1PanDragTool1: TPanDragTool;
    ChartToolset1ZoomDragTool1: TZoomDragTool;
    clRight: TConstantLine;
    clLeft: TConstantLine;
    clTop: TConstantLine;
    clBottom: TConstantLine;
    Chart1LineSeries: TLineSeries;
    fseBounds: TFloatSpinEdit;
    lblHistory: TLabel;
    lblBoundValue: TLabel;
    Panel1: TPanel;
    RandomChartSource1: TRandomChartSource;
    procedure btnPrevExtentClick(Sender: TObject);
    procedure cgUseBoundsItemClick(Sender: TObject; AIndex: integer);
    procedure Chart1ExtentChanged(ASender: TChart);
    procedure FormCreate(Sender: TObject);
    procedure fseBoundsChange(Sender: TObject);
  private
    FHistory: TChartExtentHistory;
    FIsRemembering: Boolean;
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

uses
  SysUtils, TAChartUtils;

{ TForm1 }

procedure TForm1.btnPrevExtentClick(Sender: TObject);
begin
  FIsRemembering := FHistory.Count > 0;
  if FIsRemembering then
    Chart1.LogicalExtent := FHistory.Pop;
end;

procedure TForm1.cgUseBoundsItemClick(Sender: TObject; AIndex: integer);
begin
  Unused(AIndex);
  with Chart1.Extent do begin
    UseXMin := cgUseBounds.Checked[0];
    UseXMax := cgUseBounds.Checked[1];
    UseYMin := cgUseBounds.Checked[2];
    UseYMax := cgUseBounds.Checked[3];
  end;
end;

procedure TForm1.Chart1ExtentChanged(ASender: TChart);
begin
  Unused(ASender);
  if not FIsRemembering then
    FHistory.Add(Chart1.PrevLogicalExtent);
  FIsRemembering := false;
  btnPrevExtent.Enabled := FHistory.Count > 0;
  lblHistory.Caption := Format(
    'History: %d of %d', [FHistory.Count, FHistory.Capacity]);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  fseBoundsChange(nil);
  FHistory := TChartExtentHistory.Create;
  FHistory.Capacity := 8;
  FIsRemembering := true;
end;

procedure TForm1.fseBoundsChange(Sender: TObject);
begin
  clRight.Position := fseBounds.Value;
  clLeft.Position := -fseBounds.Value;
  clTop.Position := fseBounds.Value;
  clBottom.Position := -fseBounds.Value;
  with Chart1.Extent do begin
    XMin := -fseBounds.Value;
    XMax := fseBounds.Value;
    YMin := -fseBounds.Value;
    YMax := fseBounds.Value;
  end;
end;

end.

