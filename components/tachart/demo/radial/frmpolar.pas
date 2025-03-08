unit frmPolar;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes,
  Forms, Controls, StdCtrls, ExtCtrls,
  TAGraph, TASources, TASeries, TARadialSeries;

type
  TPolarFrame = class(TFrame)
    cbCloseCircle: TCheckBox;
    cbFilled: TCheckBox;
    cbShowPoints: TCheckBox;
    Chart: TChart;
    ChartSeries1: TPolarSeries;
    ChartSeries2: TPolarSeries;
    lblTransparency: TLabel;
    pnlPolar: TPanel;
    RandomChartSource: TRandomChartSource;
    sbTransparency: TScrollBar;
    procedure cbCloseCircleChange(Sender: TObject);
    procedure cbFilledChange(Sender: TObject);
    procedure cbShowPointsChange(Sender: TObject);
    procedure sbTransparencyChange(Sender: TObject);
  private

  public

  end;

implementation

{$R *.lfm}

procedure TPolarFrame.cbCloseCircleChange(Sender: TObject);
begin
  ChartSeries1.CloseCircle := cbCloseCircle.Checked;
  ChartSeries2.CloseCircle := cbCloseCircle.Checked;
end;

procedure TPolarFrame.cbFilledChange(Sender: TObject);
begin
  ChartSeries1.Filled := cbFilled.Checked;
  ChartSeries2.Filled := cbFilled.Checked;
end;

procedure TPolarFrame.cbShowPointsChange(Sender: TObject);
begin
  ChartSeries1.ShowPoints := cbShowPoints.Checked;
  ChartSeries2.ShowPoints := cbShowPoints.Checked;
end;

procedure TPolarFrame.sbTransparencyChange(Sender: TObject);
begin
  ChartSeries1.Transparency := sbTransparency.Position;
  ChartSeries2.Transparency := sbTransparency.Position;
  lblTransparency.Caption := Format('Transparency (%d)', [sbTransparency.Position]);
end;

end.

