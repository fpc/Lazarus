unit frmDerivative;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes, Math,
  Forms, Controls, StdCtrls, ExtCtrls, Spin,
  TAGraph, TASeries, TASources;

type
  TDerivativeFrame = class(TFrame)
    cbAccDirDerivative: TComboBox;
    cbSmooth: TCheckBox;
    ccsDerivative: TCalculatedChartSource;
    chDerivative: TChart;
    chDerivativeLineDeriv: TLineSeries;
    chDerivativeLineOrig: TLineSeries;
    lblAccumulationRange: TLabel;
    lcsDerivative: TListChartSource;
    ParamsPanel: TPanel;
    rgDataShape: TRadioGroup;
    seAccumulationRange: TSpinEdit;
    procedure cbAccDirDerivativeChange(Sender: TObject);
    procedure cbSmoothChange(Sender: TObject);
    procedure rgDataShapeClick(Sender: TObject);
    procedure seAccumulationRangeChange(Sender: TObject);
  private
    procedure CreateDataForDerivative;

  public
    constructor Create(AOwner: TComponent); override;

  end;

implementation

{$R *.lfm}

constructor TDerivativeFrame.Create(AOwner: TComponent);
begin
  inherited;
  CreateDataForDerivative;
end;

procedure TDerivativeFrame.cbAccDirDerivativeChange(Sender: TObject);
begin
  ccsDerivative.AccumulationDirection := TChartAccumulationDirection(cbAccDirDerivative.ItemIndex);
end;

procedure TDerivativeFrame.cbSmoothChange(Sender: TObject);
begin
  if cbSmooth.Checked then
    ccsDerivative.AccumulationMethod := camSmoothDerivative
  else
    ccsDerivative.AccumulationMethod := camDerivative;
end;

procedure TDerivativeFrame.CreateDataForDerivative;
const
  N = 100;
  MIN_X = -10;
  MAX_X = 10;
  EPS = 1e-6;
var
  i: Integer;
  x, y: Double;
begin
  lcsDerivative.Clear;
  if rgDataShape.ItemIndex = 6 then
    for i := 0 to 9 do
      lcsDerivative.Add(i - IfThen(i > 6, 1, 0), i)
  else
    for i := 0 to N - 1 do begin
      x := MIN_X + (MAX_X - MIN_X) / (N - 1) * i;
      if SameValue(x, 0.0, EPS) then x := 0;
      case rgDataShape.ItemIndex of
        0: y := x;
        1: y := Sin(x);
        2: if x = 0 then y := 1 else y := Sin(x) / x;
        3: y := Exp(-x / 3);
        4: y := Exp(-Sqr((x - 2.5) / 2.5));
        5: y := Exp(-Sqr((x - 2.5) / 2.5)) + 0.05 * (Random - 0.5);
      end;
      lcsDerivative.Add(x, y);
    end;
end;

procedure TDerivativeFrame.rgDataShapeClick(Sender: TObject);
begin
  CreateDataForDerivative;
end;

procedure TDerivativeFrame.seAccumulationRangeChange(Sender: TObject);
begin
  ccsDerivative.AccumulationRange := seAccumulationRange.Value;
end;

end.

