unit unit1;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Graphics, ExtCtrls,
  TAGraph, TASeries, TATools, Buttons, StdCtrls, Spin, Classes;

type
  { TForm1 }

  TForm1 = class(TForm)
    btnClearArea: TButton;
    btnClearBar: TButton;
    btnClearLine: TButton;
    btnClearAll: TButton;
    btnClearPie: TButton;
    btnAddBar: TButton;
    btnAddPie: TButton;
    btnAddLine: TButton;
    btnAddArea: TButton;
    cbShowAxisTitles: TCheckBox;
    Chart1: TChart;
    cbBottomAxis: TCheckBox;
    cbLeftAxis: TCheckBox;
    cbTitle: TCheckBox;
    cbFooter: TCheckBox;
    cbInverted: TCheckBox;
    cbLegend: TCheckBox;
    cbShowGridCheckBox: TCheckBox;
    Chart1LineHor: TConstantLine;
    Chart1LineVert: TConstantLine;
    cbMarkStyle: TComboBox;
    ChartToolset1: TChartToolset;
    DataPointCrosshairTool: TDataPointCrosshairTool;
    cbCrosshairTool: TComboBox;
    lblCrossHairTool: TLabel;
    lblAddCount: TLabel;
    lblAdd: TLabel;
    lblMarkStyle: TLabel;
    lblClear: TLabel;
    Panel1: TPanel;
    edAddCount: TSpinEdit;
    procedure btnClearAllClick(Sender: TObject);
    procedure btnClearAreaClick(Sender: TObject);
    procedure btnClearBarClick(Sender: TObject);
    procedure btnClearLineClick(Sender: TObject);
    procedure btnClearPieClick(Sender: TObject);
    procedure cbCrosshairToolChange(Sender: TObject);
    procedure cbInvertedChange(Sender: TObject);
    procedure btnAddAreaClick(Sender: TObject);
    procedure btnAddBarClick(Sender: TObject);
    procedure btnAddLineClick(Sender: TObject);
    procedure btnAddPieClick(Sender: TObject);
    procedure cbBottomAxisChange(Sender: TObject);
    procedure cbLeftAxisChange(Sender: TObject);
    procedure cbShowAxisTitlesChange(Sender: TObject);
    procedure cbTitleChange(Sender: TObject);
    procedure cbFooterChange(Sender: TObject);
    procedure cbLegendChange(Sender: TObject);
    procedure cbShowGridCheckBoxChange(Sender: TObject);
  private
    FArea: TAreaSeries;
    FBar: TBarSeries;
    FLine: TLineSeries;
    FPie: TPieSeries;
    x, y, x1, y1, x3, y3: Double;
    procedure InitBar;
    procedure InitLine;
    procedure InitPie;
    procedure InitArea;
    procedure BringToFront(ASeries: TBasicChartSeries);
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

uses
  Math, TAChartUtils, TATextElements, TATypes;

{ TForm1 }

procedure TForm1.BringToFront(ASeries: TBasicChartSeries);
var
  s: TBasicChartSeries;
begin
  for s in Chart1.Series do
    s.ZPosition := Ord(s = ASeries);
end;

procedure TForm1.btnAddAreaClick(Sender: TObject);
var
  i: integer;
begin
  if FArea = nil then InitArea;
  BringToFront(FArea);
  FArea.Marks.Style := TSeriesMarksStyle(cbMarkStyle.ItemIndex);
  for i := 1 to edAddCount.Value do begin
    X3 := X3 + 1;
    if random(2) >= 0.7 then Y3 := Y3 + random(5)
    else if random(2) >= 0.7 then Y3 := 0
    else Y3 := Y3 - random(5);
    FArea.AddXY(x3, y3, '', clTAColor);
  end;
end;

procedure TForm1.btnAddBarClick(Sender: TObject);
var
  i: integer;
begin
  if FBar = nil then InitBar;
  BringToFront(FBar);
  FBar.Marks.Style := TSeriesMarksStyle(cbMarkStyle.ItemIndex);
  for i := 1 to edAddCount.Value do begin
    FBar.AddXY(x, y, '', clBlue * (i mod 2) + clTAColor * (1 - i mod 2));
    X := X + 1;
    if random(2) >= 0.7 then Y := Y + random(5)
    else if random(2) >= 0.7 then Y := 0
    else Y := Y - random(5);
  end;
end;

procedure TForm1.btnAddLineClick(Sender: TObject);
var
  i: integer;
begin
  if FLine = nil then InitLine;
  BringToFront(FLine);
  FLine.Marks.Style := TSeriesMarksStyle(cbMarkStyle.ItemIndex);
  for i := 1 to edAddCount.Value do begin
    FLine.AddXY(x1, y1, '', IfThen(i mod 2 = 0, clGreen, clBlue));
    X1 := X1 + 1.5;
    if random(2) >= 0.5 then Y1 := Y1 + random(10)
    else Y1 := Y1 - random(5);
  end;
end;

procedure TForm1.btnAddPieClick(Sender: TObject);
var
  i: integer;
begin
  if FPie = nil then InitPie;
  BringToFront(FPie);
  FPie.Marks.Style := TSeriesMarksStyle(cbMarkStyle.ItemIndex);
  for i := 1 to edAddCount.Value do begin
    FPie.AddPie(3.4234235235, 'sde21312', clTAColor);
    FPie.AddPie(0.2323, 'adassssssdddddd', clTAColor);
    FPie.AddPie(30, 'filipe romao', clTAColor);
    FPie.AddPie(40, '234eds sa', clTAColor);
  end;
end;

procedure TForm1.btnClearAllClick(Sender: TObject);
begin
  Chart1.ClearSeries;
  FArea := nil;
  FBar := nil;
  FLine := nil;
  FPie := nil;
end;

procedure TForm1.btnClearAreaClick(Sender: TObject);
begin
  FreeAndNil(FArea);
end;

procedure TForm1.btnClearBarClick(Sender: TObject);
begin
  FreeAndNil(FBar);
end;

procedure TForm1.btnClearLineClick(Sender: TObject);
begin
  FreeAndNil(FLine);
end;

procedure TForm1.btnClearPieClick(Sender: TObject);
begin
  FreeAndNil(FPie);
end;

procedure TForm1.cbCrosshairToolChange(Sender: TObject);
begin
  DataPointCrosshairTool.Enabled := CbCrosshairTool.ItemIndex > 0;
  case cbCrosshairTool.ItemIndex of
    1: begin
         DatapointCrosshairTool.Shape := ccsVertical;
         DatapointCrosshairTool.DistanceMode := cdmOnlyX;
       end;
    2: begin
         DatapointCrosshairTool.Shape := ccsHorizontal;
         DatapointCrosshairTool.DistanceMode := cdmOnlyY;
       end;
    3: begin
         DatapointCrosshairTool.Shape := ccsCross;
         DatapointCrosshairTool.DistanceMode := cdmXY;
       end;
  end;
end;

procedure TForm1.cbBottomAxisChange(Sender: TObject);
begin
  Chart1.BottomAxis.Visible := cbBottomAxis.Checked;
end;

procedure TForm1.cbFooterChange(Sender: TObject);
begin
  Chart1.Foot.Visible := cbFooter.Checked;
end;

procedure TForm1.cbInvertedChange(Sender: TObject);
begin
  Chart1.BottomAxis.Inverted := cbInverted.Checked;
  Chart1.LeftAxis.Inverted := cbInverted.Checked;
end;

procedure TForm1.cbLegendChange(Sender: TObject);
begin
  Chart1.Legend.Visible := cbLegend.Checked;
end;

procedure TForm1.cbShowAxisTitlesChange(Sender: TObject);
begin
  with Chart1.BottomAxis.Title do
    if cbShowAxisTitles.Checked then Caption := 'X axis' else Caption := '';
  with Chart1.LeftAxis.Title do
    if cbShowAxisTitles.Checked then Caption := 'Y axis' else Caption := '';
end;

procedure TForm1.cbShowGridCheckBoxChange(Sender: TObject);
begin
  Chart1.LeftAxis.Grid.Visible := cbShowGridCheckBox.Checked;
  Chart1.BottomAxis.Grid.Visible := cbShowGridCheckBox.Checked;
end;

procedure TForm1.cbLeftAxisChange(Sender: TObject);
begin
  Chart1.LeftAxis.Visible := cbLeftAxis.Checked;
end;

procedure TForm1.cbTitleChange(Sender: TObject);
begin
  Chart1.Title.Visible := cbTitle.Checked;
end;

procedure TForm1.InitArea;
begin
  FArea := TAreaSeries.Create(Chart1);
  FArea.SeriesColor := clFuchsia;
  FArea.Title := 'area';
  Chart1.AddSeries(FArea);
end;

procedure TForm1.InitBar;
begin
  FBar := TBarSeries.Create(Chart1);
  FBar.Title := 'bars';
  FBar.SeriesColor := clGreen;
  Chart1.AddSeries(FBar);
end;

procedure TForm1.InitLine;
begin
  FLine := TLineSeries.Create(Chart1);
  FLine.ShowLines := true;
  FLine.ShowPoints := true;
  FLine.Pointer.Style := psRectangle;
  FLine.Pointer.Brush.Color := clRed;
  FLine.Title := 'line';
  FLine.SeriesColor := clRed;
  Chart1.AddSeries(FLine);
end;

procedure TForm1.InitPie;
begin
  FPie := TPieSeries.Create(Chart1);
  FPie.Title := 'pie';
  FPie.Marks.LabelBrush.Color := $80FFFF;
  FPie.Marks.LinkPen.Width := 2;
  FPie.Marks.OverlapPolicy := opHideNeighbour;
  Chart1.AddSeries(FPie);
end;

end.

