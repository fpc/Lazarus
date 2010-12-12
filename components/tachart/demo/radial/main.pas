unit main; 

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, ExtCtrls, Spin, StdCtrls, SysUtils, FileUtil, Forms,
  Controls, Graphics, Dialogs, TAGraph, TASeries, TASources, TATools;

type

  { TForm1 }

  TForm1 = class(TForm)
    ChartPie: TChart;
    ChartPiePieSeries1: TPieSeries;
    ChartToolset1: TChartToolset;
    cbRotate: TCheckBox;
    lblWords: TLabel;
    lblLabelAngle: TLabel;
    ListChartSource1: TListChartSource;
    PageControl1: TPageControl;
    Panel1: TPanel;
    seWords: TSpinEdit;
    seLabelAngle: TSpinEdit;
    tsPie: TTabSheet;
    procedure cbRotateChange(Sender: TObject);
    procedure ChartPieMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure seWordsChange(Sender: TObject);
    procedure seLabelAngleChange(Sender: TObject);
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.cbRotateChange(Sender: TObject);
begin
  ChartPiePieSeries1.RotateLabels := cbRotate.Checked;
end;

procedure TForm1.ChartPieMouseDown(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  i := ChartPiePieSeries1.FindContainingSlice(Point(X, Y));
  if i < 0 then exit;
  ListChartSource1.SetXValue(i, 0.2 - ListChartSource1[i]^.X);
  ChartPie.Invalidate;
end;

procedure TForm1.seWordsChange(Sender: TObject);
var
  r: TMWCRandomGenerator;

  function RandWord: String;
  var
    i: Integer;
  begin
    SetLength(Result, r.GetInRange(1, 5));
    for i := 1 to Length(Result) do
      Result[i] := Chr(r.GetInRange(Ord('a'), Ord('z')));
  end;

var
  i, j: Integer;
begin
  r := TMWCRandomGenerator.Create;
  try
    r.Seed := 9823743;
    for i := 0 to ListChartSource1.Count - 1 do
      ListChartSource1[i]^.Text := '';
    for j := 1 to seWords.Value do
      for i := 0 to ListChartSource1.Count - 1 do
        with ListChartSource1[i]^ do begin
          if Text <> '' then
            Text := Text + ' ';
          Text := Text + RandWord;
        end;
  finally
    r.Free;
  end;
  ChartPie.Invalidate;
end;

procedure TForm1.seLabelAngleChange(Sender: TObject);
begin
  ChartPiePieSeries1.Marks.LabelFont.Orientation := seLabelAngle.Value * 10;
end;

end.

