unit frmField;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes,
  Forms, Controls, StdCtrls, ExtCtrls, Spin,
  TAGraph, TAChartUtils, TAGeometry, TAMultiSeries;

type
  TFieldFrame = class(TFrame)
    Chart: TChart;
    FieldSeries: TFieldSeries;
    edMaxVectorLength: TFloatSpinEdit;
    Label1: TLabel;
    Panel1: TPanel;
    rbRadial: TRadioButton;
    rbTangential: TRadioButton;
    procedure edMaxVectorLengthChange(Sender: TObject);
    procedure rbRadialChange(Sender: TObject);
  private
    procedure CreateFieldSeriesData;

  public
    constructor Create(AOwner: TComponent); override;

  end;

implementation

{$R *.lfm}

constructor TFieldFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateFieldSeriesData;
end;

procedure TFieldFrame.CreateFieldSeriesData;
const
  NX = 21;
  NY = 21;
  MIN = -5.0;
  MAX = +5.0;
var
  i, j: Integer;
  x, y, r: Double;
  v: TDoublePoint;
begin
  v := DoublePoint(2.0, 2.0);
  r := sqrt(sqr(v.x) + sqr(v.y));
  FieldSeries.Clear;
  for j := 0 to NY - 1 do begin
    y := MIN + (MAX - MIN) / (NY - 1) * j;
    for i := 0 to NX - 1 do begin
      x := MIN + (MAX - MIN) / (NX - 1) * i;
      r := sqr(x) + sqr(y);
      if r > 0.1 then begin
        if rbRadial.Checked then
          v := DoublePoint(x/r, y/r)    // radial vector
        else
        if rbTangential.Checked then
          v := DoublePoint(y/r, -x/r);  // tangential vector
        FieldSeries.AddVector(x, y, v.x, v.y);
      end;
    end;
  end;
  // Since the data points, in this example, have a distance of 0.5 units we
  // can avoid overlapping of vectors if they are scaled to a length of 0.5
  // units as well.
  FieldSeries.NormalizeVectors(0.5);
end;

procedure TFieldFrame.edMaxVectorLengthChange(Sender: TObject);
begin
  FieldSeries.NormalizeVectors(EdMaxVectorLength.Value);
  Chart.Invalidate;
end;

procedure TFieldFrame.rbRadialChange(Sender: TObject);
begin
  CreateFieldSeriesData;
end;

end.

