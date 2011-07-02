unit main;

{$mode objfpc}{$H+}

interface

uses
  ExtCtrls, Spin, StdCtrls, Forms, TAGraph, TASeries, TASources, Classes,
  TALegend, TAFuncSeries, Graphics;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1AreaSeries1: TAreaSeries;
    Chart1FuncSeries1: TFuncSeries;
    Chart1LineSeries1: TLineSeries;
    Chart1PieSeries1: TPieSeries;
    cbUseSidebar: TCheckBox;
    lblSpacing: TLabel;
    lblMarginX: TLabel;
    lblSymbolWidth: TLabel;
    lblMarginY: TLabel;
    ListChartSource1: TListChartSource;
    ListChartSource2: TListChartSource;
    pnControls: TPanel;
    rgAlignment: TRadioGroup;
    RandomChartSource1: TRandomChartSource;
    seSpacing: TSpinEdit;
    seMarginX: TSpinEdit;
    seSymbolWidth: TSpinEdit;
    seMarginY: TSpinEdit;
    procedure cbUseSidebarChange(Sender: TObject);
    procedure Chart1FuncSeries1Calculate(const AX: Double; out AY: Double);
    procedure Chart1FuncSeries1DrawLegend(
      ACanvas: TCanvas; const ARect: TRect; AIndex: Integer; var AText: String);
    procedure rgAlignmentClick(Sender: TObject);
    procedure seMarginXChange(Sender: TObject);
    procedure seMarginYChange(Sender: TObject);
    procedure seSpacingChange(Sender: TObject);
    procedure seSymbolWidthChange(Sender: TObject);
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

uses
  SysUtils;

{ TForm1 }

procedure TForm1.cbUseSidebarChange(Sender: TObject);
begin
  Chart1.Legend.UseSidebar := cbUseSidebar.Checked;
end;

procedure TForm1.Chart1FuncSeries1Calculate(const AX: Double; out AY: Double);
begin
  AY := Sin(AX * 2) + 7;
end;

procedure TForm1.Chart1FuncSeries1DrawLegend(
  ACanvas: TCanvas; const ARect: TRect; AIndex: Integer; var AText: String);
var
  x, y0, w: Integer;
begin
  AText := 'Function ' + IntToStr(AIndex);
  ACanvas.Pen := Chart1FuncSeries1.Pen;
  y0 := (ARect.Top + ARect.Bottom) div 2;
  ACanvas.MoveTo(ARect.Left, y0);
  w := ARect.Right - ARect.Left;
  for x := 0 to w do
    ACanvas.LineTo(
      ARect.Left + x,
      Round(Sin(x / w * 2 * Pi) * (ARect.Bottom - ARect.Top) / 2) + y0);
end;

procedure TForm1.rgAlignmentClick(Sender: TObject);
begin
  with Chart1.Legend do
    case rgAlignment.ItemIndex of
      0: Alignment := laTopLeft;
      1: Alignment := laBottomLeft;
      2: Alignment := laTopRight;
      3: Alignment := laBottomRight;
    end;
end;

procedure TForm1.seMarginXChange(Sender: TObject);
begin
  Chart1.Legend.MarginX := seMarginX.Value;
end;

procedure TForm1.seMarginYChange(Sender: TObject);
begin
  Chart1.Legend.MarginY := seMarginY.Value;
end;

procedure TForm1.seSpacingChange(Sender: TObject);
begin
  Chart1.Legend.Spacing := seSpacing.Value;
end;

procedure TForm1.seSymbolWidthChange(Sender: TObject);
begin
  Chart1.Legend.SymbolWidth := seSymbolWidth.Value;
end;

end.

