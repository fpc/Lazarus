unit frmColorEach;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  Forms, Graphics, Controls, StdCtrls, ExtCtrls,
  TAGraph, TAChartUtils, TASeries;

type
  TColorEachDatapointFrame = class(TFrame)
    cmbColorEach: TComboBox;
    cmbColorEachLineType: TComboBox;
    cbColorEachRotated: TCheckBox;
    Chart: TChart;
    LineSeries: TLineSeries;
    pnlColorEach: TPanel;
    procedure cmbColorEachChange(Sender: TObject);
    procedure cmbColorEachLineTypeChange(Sender: TObject);
    procedure cbColorEachRotatedChange(Sender: TObject);
  private

  public
    constructor Create(AOwner: TComponent); override;

  end;

implementation

{$R *.lfm}

constructor TColorEachDatapointFrame.Create(AOwner: TComponent);
var
  i: Integer;
  x, y: Double;
  clr: TColor;
begin
  inherited Create(AOwner);

  // Populate series for the "ColorEach" demo
  for i := 0 to 20 do begin
    x := i/2;
    y := sin(x);
    clr := InterpolateRGB(clRed, clYellow, (y+1)/2);
    LineSeries.AddXY(x, y, '', clr);
  end;
  cmbColorEach.ItemIndex := ord(LineSeries.ColorEach);
end;

procedure TColorEachDatapointFrame.cbColorEachRotatedChange(Sender: TObject);
begin
  LineSeries.AxisIndexY := Ord(cbColorEachRotated.Checked);
  LineSeries.AxisIndexX := 1 - LineSeries.AxisIndexY;
end;

procedure TColorEachDatapointFrame.cmbColorEachChange(Sender: TObject);
begin
  LineSeries.ColorEach := TColorEachMode(cmbColorEach.ItemIndex);
end;

procedure TColorEachDatapointFrame.cmbColorEachLineTypeChange(Sender: TObject);
begin
  LineSeries.LineType := TLineType(cmbColorEachLineType.ItemIndex);
end;

end.

