unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, Spin, StdCtrls, SysUtils, FileUtil, Forms, Controls,
  Graphics, Dialogs, TAGraph, TASeries, TASources, TACustomSource;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnGenerate: TButton;
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    Chart1ManhattanSeries1: TManhattanSeries;
    lblTime: TLabel;
    pnlControls: TPanel;
    seCount: TFloatSpinEdit;
    UserDefinedChartSource1: TUserDefinedChartSource;
    procedure btnGenerateClick(Sender: TObject);
    procedure Chart1AfterDrawBackground(
      ASender: TChart; ACanvas: TCanvas; const ARect: TRect);
    procedure Chart1AfterPaint(ASender: TChart);
    procedure UserDefinedChartSource1GetChartDataItem(
      ASource: TUserDefinedChartSource; AIndex: Integer;
      var AItem: TChartDataItem);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  LCLIntf, LCLType, Math,
  TAChartUtils;

var
  VData: array of record X, Y: Double; Color: TColor; end;
  t0: Int64;

{ TForm1 }

procedure TForm1.btnGenerateClick(Sender: TObject);
const
  COLORS: array [1..5] of TColor = (clYellow, clBlue, clGreen, clMaroon, clFuchsia);
var
  i: DWord;
begin
  SetLength(VData, Round(seCount.Value));
  for i := 0 to High(VData) do
    with VData[i] do begin
      X := Random(1000);
      Y := Abs(RandG(0.0, 1.0));
      Color := COLORS[Trunc(X / 1000 * Length(COLORS)) + 1];
    end;
  UserDefinedChartSource1.PointsNumber := Round(seCount.Value);
  UserDefinedChartSource1.Reset;
end;

procedure TForm1.Chart1AfterDrawBackground(
  ASender: TChart; ACanvas: TCanvas; const ARect: TRect);
begin
  Unused(ASender);
  Unused(ACanvas, ARect);
  t0 := GetTickCount64;
end;

procedure TForm1.Chart1AfterPaint(ASender: TChart);
begin
  Unused(ASender);
  lblTime.Caption := Format('Time:'+LineEnding+'%d ms', [GetTickCount64 - t0]);
end;

procedure TForm1.UserDefinedChartSource1GetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
begin
  Unused(ASource);
  with VData[AIndex] do begin
    AItem.X := X;
    AItem.Y := Y;
    AItem.Color := Color;
  end;
end;

end.

