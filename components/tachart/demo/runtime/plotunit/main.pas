unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Types, Forms, Controls,
  Graphics, Dialogs, StdCtrls, ExtCtrls,
  TAGraph, TACustomSeries, TASeries, TAFuncSeries;

type

  { TMainForm }

  TMainForm = class(TForm)
    Bevel1: TBevel;
    BtnAdd: TButton;
    BtnClear: TButton;
    Chart: TChart;
    CbPlotTypes: TComboBox;
    procedure BtnAddClick(Sender: TObject);
    procedure BtnClearClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  TATypes, uplot;

const
  PALETTE: array[0..7] of TColor = (
    clRed, clGreen, clBlue, clOlive, clFuchsia, clAqua, clBlack, clLime);

  SYMBOLS: array[0..5] of TSeriesPointerStyle = (
    psCircle, psRectangle, psTriangle, psDiamond, psDownTriangle, psFullStar);

  LINESTYLES: array[0..4] of TPenStyle = (
    psSolid, psDot, psDash, psDashDot, psDashDotDot);


{ TMainForm }

procedure CreateData(ACount: Integer; var x, y: TDoubleDynArray);
const
  MIN = -10;
  MAX = 10;
var
  i: Integer;
begin
  SetLength(x, ACount);
  SetLength(y, ACount);
  for i:=0 to ACount-1 do
  begin
    x[i] := MIN + (MAX - MIN) * i / (ACount - 1);
    y[i] := random;
  end;
end;

procedure TMainForm.BtnAddClick(Sender: TObject);
var
  x, y: TDoubleDynArray;
  colorIndex: Integer;
  symbolIndex: Integer;
  linestyleIndex: Integer;
  plottype: Integer;
begin
  if Chart.SeriesCount = 0 then
    PrepareChart(Chart, 'x axis', 'y axis', 'TAChart Plot Demo', 'Written by Lazarus');
  CreateData(Random(20)+3, x, y);
  colorIndex := Chart.SeriesCount mod Length(PALETTE);
  symbolIndex := Chart.SeriesCount mod Length(SYMBOLS);
  lineStyleIndex := Chart.SeriesCount mod Length(LINESTYLES);
  plottype := PtrInt(CbPlotTypes.Items.Objects[CbPlotTypes.ItemIndex]);
  Plot(Chart,
    x, y,
    'Series #'+IntToStr(Chart.SeriesCount+1),
    PALETTE[colorIndex],
    SYMBOLS[symbolIndex],
    LINESTYLES[linestyleIndex],
    TPlotType(abs(plottype)),
    plottype < 0                // with symbols if value is negative
  );
end;

procedure TMainForm.BtnClearClick(Sender: TObject);
begin
  Chart.ClearSeries;
  Chart.Legend.Visible := false;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Chart.Visible := false;

  CbPlotTypes.Items.Clear;

  // Encode plot types into the object of the CbPlotTypes combobox:
  // - The ord value of the plot type is cast to an object if the series is
  //   without symbols.
  // - The negative ord value of the plot type is cast if an object if the
  //   series is with symbols.
  CbPlotTypes.Items.AddObject('Symbols only', TObject(PtrInt(ptSymbolsOnly)));
  CbPlotTypes.Items.AddObject('Line segments w/ symbols', TObject(-PtrInt(ptSegments)));
  CbPlotTypes.Items.AddObject('Line segments w/o symbols', TObject(PtrInt(ptSegments)));
  CbPlotTypes.Items.AddObject('Cubic spline w/ symbols', TObject(-PtrInt(ptCubicSpline)));
  CbPlotTypes.Items.AddObject('Cubic spline w/o symbols', TObject(PtrInt(ptCubicSpline)));
  CbPlotTypes.Items.AddObject('B-Spline w/ symbols', TObject(-PtrInt(ptBSpline)));
  CbPlotTypes.Items.AddObject('B-Spline w/o symbols', TObject(PtrInt(ptBSpline)));
  CbPlotTypes.Items.AddObject('Linear fit w/ symbols', TObject(-PtrInt(ptLinearfit)));
  CbPlotTypes.Items.AddObject('Linear fit w/o symbols', TObject(PtrInt(ptLinearFit)));
  CbPlotTypes.Items.AddObject('Parabolic fit w/symbols', TObject(-PtrInt(ptSquareFit)));
  CbPlotTypes.Items.AddObject('Parabolic fit w/o symbols)', TObject(PtrInt(ptSquareFit)));
  CbPlotTypes.Items.AddObject('Area (no symbols)', TObject(PtrInt(ptArea)));
  CbPlotTypes.Items.AddObject('Bars (horizontal)', TObject(PtrInt(ptBars)));
  CbPlotTypes.Items.AddObject('Columns (vertical)', TObject(PtrInt(ptColumns)));
  CbPlotTypes.ItemIndex := 1;
end;

end.

