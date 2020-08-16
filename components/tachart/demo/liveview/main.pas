unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Spin, TAGraph, TASeries, TANavigation, TAChartLiveView;

type

  { TMainFrom }

  TMainFrom = class(TForm)
    btnAddDataPoint: TButton;
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    ChartLiveView1: TChartLiveView;
    ChartNavScrollBar1: TChartNavScrollBar;
    cbLiveMode: TCheckBox;
    cbFixedExtent: TCheckBox;
    cbExtentY: TComboBox;
    lblExtentY: TLabel;
    seViewportSize: TFloatSpinEdit;
    lblViewportSize: TLabel;
    Panel1: TPanel;
    procedure btnAddDataPointClick(Sender: TObject);
    procedure cbLiveModeChange(Sender: TObject);
    procedure cbFixedExtentChange(Sender: TObject);
    procedure cbExtentYChange(Sender: TObject);
    procedure seViewportSizeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  MainFrom: TMainFrom;

implementation

{$R *.lfm}

uses
  Math, TAChartUtils;

{ TMainFrom }

procedure TMainFrom.btnAddDataPointClick(Sender: TObject);
const
  TWO_PI = 2.0 * pi;
var
  x, y: Double;
begin
  x := Chart1LineSeries1.Count;
  y := sin(x * TWO_PI / 10) + randg(0, 0.1);
  Chart1Lineseries1.AddXY(x, y);
end;

procedure TMainFrom.cbFixedExtentChange(Sender: TObject);
begin
  Chart1.Extent.YMin := -1.5;
  Chart1.Extent.YMax := 1.5;
  Chart1.Extent.UseYMin := cbFixedExtent.Checked;
  Chart1.Extent.UseYMax := cbFixedExtent.Checked;
end;

procedure TMainFrom.cbLiveModeChange(Sender: TObject);
begin
  ChartLiveView1.Active := cbLiveMode.Checked;
  seViewportSize.Visible := cbLiveMode.Checked;
  lblViewportSize.Visible := cbLiveMode.Checked;
  cbExtentY.Visible := cbLiveMode.Checked;
  lblExtentY.Visible := cbLiveMode.Checked;
end;

procedure TMainFrom.cbExtentYChange(Sender: TObject);
begin
  ChartLiveView1.ExtentY := TChartLiveViewExtentY(cbExtentY.ItemIndex);
end;

procedure TMainFrom.FormCreate(Sender: TObject);
begin
  // Add three data points to start with
  btnAddDataPointClick(nil);
  btnAddDataPointClick(nil);
  btnAddDataPointClick(nil);

  // Initialize live view controls
  cbLiveMode.Checked := ChartLiveView1.Active;
  seViewportSize.Value := ChartLiveView1.ViewportSize;
  cbExtentY.ItemIndex := ord(ChartLiveView1.ExtentY);
end;

procedure TMainFrom.seViewportSizeChange(Sender: TObject);
begin
  ChartLiveView1.ViewportSize := seViewportSize.Value;
end;


end.

