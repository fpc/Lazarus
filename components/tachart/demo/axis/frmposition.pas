unit frmPosition;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes, Forms, Controls, StdCtrls, ExtCtrls, Spin,
  TAGraph, TAChartUtils, TAChartAxisUtils, TAFuncSeries;

type
  TPositionFrame = class(TFrame)
    cbStaticX: TCheckBox;
    cbStaticY: TCheckBox;
    cbUnitsX: TComboBox;
    cbUnitsY: TComboBox;
    ChartPosition: TChart;
    FuncSeries: TFuncSeries;
    gbPositionX: TGroupBox;
    gbPositionY: TGroupBox;
    lblPositionX: TLabel;
    lblPositionY: TLabel;
    lblUnitsX: TLabel;
    lblUnitsY: TLabel;
    pnlPosition: TPanel;
    rbPositionBottom: TRadioButton;
    rbPositionLeft: TRadioButton;
    rbPositionRight: TRadioButton;
    rbPositionTop: TRadioButton;
    seXPosition: TSpinEdit;
    seYPosition: TSpinEdit;
    procedure cbStaticXChange(Sender: TObject);
    procedure cbStaticYChange(Sender: TObject);
    procedure cbUnitsXChange(Sender: TObject);
    procedure cbUnitsYChange(Sender: TObject);
    procedure FuncSeriesCalculate(const AX: Double; out AY: Double);
    procedure rbPositionBottomChange(Sender: TObject);
    procedure rbPositionLeftChange(Sender: TObject);
    procedure seXPositionChange(Sender: TObject);
    procedure seYPositionChange(Sender: TObject);
  private

  public

  end;

implementation

{$R *.lfm}

procedure TPositionFrame.cbStaticXChange(Sender: TObject);
begin
  ChartPosition.AxisList[3].Visible := cbStaticX.Checked;
end;

procedure TPositionFrame.cbStaticYChange(Sender: TObject);
begin
  ChartPosition.AxisList[2].Visible := cbStaticY.Checked;
end;

procedure TPositionFrame.cbUnitsXChange(Sender: TObject);
begin
  ChartPosition.AxisList[1].PositionUnits := TChartUnits(cbUnitsX.ItemIndex);
end;

procedure TPositionFrame.cbUnitsYChange(Sender: TObject);
begin
  ChartPosition.AxisList[0].PositionUnits := TChartUnits(cbUnitsY.ItemIndex);
end;

procedure TPositionFrame.FuncSeriesCalculate(const AX: Double;
  out AY: Double);
begin
  AY := Sin(AX / 30) * 10 + Cos(AX / 10) * 20;
end;

procedure TPositionFrame.rbPositionBottomChange(Sender: TObject);
begin
  with ChartPosition.AxisList[1] do
    if rbPositionBottom.Checked then
      Alignment := calBottom
    else
      Alignment := calTop;
end;

procedure TPositionFrame.rbPositionLeftChange(Sender: TObject);
begin
  with ChartPosition.AxisList[0] do
    if rbPositionLeft.Checked then
      Alignment := calLeft
    else
      Alignment := calRight;
end;

procedure TPositionFrame.seXPositionChange(Sender: TObject);
begin
  ChartPosition.AxisList[1].Position := seXPosition.Value;
end;

procedure TPositionFrame.seYPositionChange(Sender: TObject);
begin
  ChartPosition.AxisList[0].Position := seYPosition.Value;
end;


end.

