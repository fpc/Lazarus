unit main; 

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, ExtCtrls, Spin, StdCtrls, SysUtils, FileUtil, Forms,
  Controls, Graphics, Dialogs, TAGraph, TARadialSeries, TASeries, TASources,
  TATools;

type

  { TForm1 }

  TForm1 = class(TForm)
    ChartPolar: TChart;
    ChartPolarSeries1: TPolarSeries;
    ChartPolarSeries2: TPolarSeries;
    ChartPie: TChart;
    ChartPiePieSeries1: TPieSeries;
    ChartToolset1: TChartToolset;
    cbRotate: TCheckBox;
    cbMarkPositions: TComboBox;
    cbCloseCircle: TCheckBox;
    cbShowPoints: TCheckBox;
    cbFilled: TCheckBox;
    lblTransparency: TLabel;
    lblWords: TLabel;
    lblLabelAngle: TLabel;
    ListChartSource1: TListChartSource;
    PageControl1: TPageControl;
    Panel1: TPanel;
    pnlPolar: TPanel;
    RandomChartSource1: TRandomChartSource;
    sbTransparency: TScrollBar;
    seWords: TSpinEdit;
    seLabelAngle: TSpinEdit;
    tsPolar: TTabSheet;
    tsPie: TTabSheet;
    procedure cbCloseCircleChange(Sender: TObject);
    procedure cbFilledChange(Sender: TObject);
    procedure cbMarkPositionsChange(Sender: TObject);
    procedure cbRotateChange(Sender: TObject);
    procedure ChartPieMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cbShowPointsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure sbTransparencyChange(Sender: TObject);
    procedure seWordsChange(Sender: TObject);
    procedure seLabelAngleChange(Sender: TObject);
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.cbCloseCircleChange(Sender: TObject);
begin
  ChartPolarSeries1.CloseCircle := cbCloseCircle.Checked;
  ChartPolarSeries2.CloseCircle := cbCloseCircle.Checked;
end;

procedure TForm1.cbFilledChange(Sender: TObject);
begin
  ChartPolarSeries1.Filled := cbFilled.Checked;
  ChartPolarSeries2.Filled := cbFilled.Checked;
end;

procedure TForm1.cbMarkPositionsChange(Sender: TObject);
begin
  ChartPiePieSeries1.MarkPositions :=
    TPieMarkPositions(cbMarkPositions.ItemIndex);
end;

procedure TForm1.cbRotateChange(Sender: TObject);
begin
  ChartPiePieSeries1.RotateLabels := cbRotate.Checked;
end;

procedure TForm1.cbShowPointsChange(Sender: TObject);
begin
  ChartPolarSeries1.ShowPoints := cbShowPoints.Checked;
  ChartPolarSeries2.ShowPoints := cbShowPoints.Checked;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  seWordsChange(nil);
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

procedure TForm1.sbTransparencyChange(Sender: TObject);
begin
  ChartPolarSeries1.Transparency := sbTransparency.Position;
  ChartPolarSeries2.Transparency := sbTransparency.Position;
  lblTransparency.Caption := 'Transparency (' + IntToStr(sbTransparency.Position) + ')';
end;

procedure TForm1.seLabelAngleChange(Sender: TObject);
begin
  ChartPiePieSeries1.Marks.LabelFont.Orientation := seLabelAngle.Value * 10;
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

end.

