unit unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  TAGraph, TASeries, Buttons, StdCtrls, Spin;

type
  { TForm1 }

  TForm1 = class(TForm)
    btnClearArea: TButton;
    btnClearBar: TButton;
    btnClearLine: TButton;
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
    edShowGridCheckBox: TCheckBox;
    lblAddCount: TLabel;
    lblAdd: TLabel;
    lblClear: TLabel;
    Panel1: TPanel;
    edAddCount: TSpinEdit;
    procedure btnClearAreaClick(Sender: TObject);
    procedure btnClearBarClick(Sender: TObject);
    procedure btnClearLineClick(Sender: TObject);
    procedure btnClearPieClick(Sender: TObject);
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
    procedure edShowGridCheckBoxChange(Sender: TObject);
  private
    FArea: TAreaSeries;
    FBar: TBarSeries;
    FLine: TSerie;
    FPie: TPieSeries;
    x, y, x1, y1, x3, y3: Double;
    procedure InitBar;
    procedure InitLine;
    procedure InitPie;
    procedure InitArea;
  end;

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.btnAddAreaClick(Sender: TObject);
var
  i: integer;
begin
  if FArea = nil then InitArea;

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
  for i := 1 to edAddCount.Value do begin
    FBar.AddXY(x, y, '', clRed);
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
  for i := 1 to edAddCount.Value do begin
    FLine.AddXY(x1, y1, '', clGreen);
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
  for i := 1 to edAddCount.Value do begin
    FPie.AddPie(3.4234235235, 'sde21312', clTAColor);
    FPie.AddPie(0.2323, 'adassssssdddddd', clTAColor);
    FPie.AddPie(30, 'filipe romao', clTAColor);
    FPie.AddPie(40, '234eds sa', clTAColor);
  end;
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

procedure TForm1.cbLeftAxisChange(Sender: TObject);
begin
  Chart1.LeftAxis.Visible := cbLeftAxis.Checked;
end;

procedure TForm1.cbTitleChange(Sender: TObject);
begin
  Chart1.Title.Visible := cbTitle.Checked;
end;

procedure TForm1.edShowGridCheckBoxChange(Sender: TObject);
begin
  Chart1.LeftAxis.Grid.Visible := edShowGridCheckBox.Checked;
  Chart1.BottomAxis.Grid.Visible := edShowGridCheckBox.Checked;
end;

procedure TForm1.InitArea;
begin
  FArea := TAreaSeries.Create(Chart1);
  Chart1.AddSerie(FArea);
  FArea.SeriesColor := clFuchsia;
  //FArea.Stairs := true;
  FArea.InvertedStairs := false;
end;

procedure TForm1.InitBar;
begin
  FBar := TBarSeries.Create(Chart1);
  Chart1.AddSerie(FBar);
  FBar.Title := 'bars';
  FBar.SeriesColor := clRed;
end;

procedure TForm1.InitLine;
begin
  FLine := TSerie.Create(Chart1);
  FLine.ShowLines := true;
  FLine.ShowPoints := true;
  FLine.Pointer.Style := psRectangle;
  FLine.Title := 'line';
  FLine.SeriesColor := clRed;
  Chart1.AddSerie(FLine);
end;

procedure TForm1.InitPie;
begin
  FPie := TPieSeries.Create(Chart1);
  Chart1.AddSerie(FPie);
  FPie.Title := 'pie';
  FPie.SeriesColor := clRed;
  FPie.MarksStyle := smsLabelPercent;
end;

initialization
  {$I unit1.lrs}

end.

