unit frmAutoYExtent;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes,
  Forms, Controls, StdCtrls, ExtCtrls,
  TAGraph, TAFuncSeries;

type
  TAutoYExtentFrame = class(TFrame)
    cbAutoExtentY: TCheckBox;
    Chart: TChart;
    ChartFuncSeries1: TFuncSeries;
    pnlAutoExtentY: TPanel;
    Timer1: TTimer;
    procedure cbAutoExtentYChange(Sender: TObject);
    procedure ChartFuncSeries1Calculate(const AX: Double; out AY: Double);
    procedure Timer1Timer(Sender: TObject);
  private

  public

  end;

implementation

{$R *.lfm}

procedure TAutoYExtentFrame.ChartFuncSeries1Calculate(const AX: Double;
  out AY: Double);
begin
  AY := Sin(AX * 2) + 3 * Cos(AX * 3) + 2 * Cos(AX * AX * 5);
end;

procedure TAutoYExtentFrame.Timer1Timer(Sender: TObject);
begin
  with ChartFuncSeries1.Extent do begin
    XMin := XMin + 0.05;
    XMax := XMax + 0.05;
  end;
end;

procedure TAutoYExtentFrame.cbAutoExtentYChange(Sender: TObject);
begin
  ChartFuncSeries1.ExtentAutoY := cbAutoExtentY.Checked;
end;

end.

