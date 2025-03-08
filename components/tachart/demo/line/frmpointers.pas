unit frmPointers;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes,
  Forms, Graphics, Controls, StdCtrls, ExtCtrls, Spin,
  TAGraph, TAEnumerators, TAChartUtils, TATypes, TASeries;

type
  TPointersFrame = class(TFrame)
    Chart: TChart;
    lblPointerSize: TLabel;
    pnlPointers: TPanel;
    sePointerSize: TSpinEdit;
    procedure sePointerSizeChange(Sender: TObject);
  private

  public
    constructor Create(AOwner: TComponent); override;

  end;

implementation

{$R *.lfm}

type
  TLineSeriesEnum =
    specialize TFilteredChartSeriesEnumeratorFactory<TLineSeries>;

constructor TPointersFrame.Create(AOwner: TComponent);
var
  st: TSeriesPointerStyle;
  ls: TLineSeries;
  s: ShortString;
begin
  inherited Create(AOwner);

  // Populate the series for the PointerStyle demo
  for st in TSeriesPointerStyle do begin
    ls := TLineSeries.Create(Self);
    ls.LinePen.Color := clGreen;
    ls.ShowPoints := true;
    ls.Pointer.Pen.Color := clRed;
    ls.Pointer.Style := st;
    ls.AddXY(1, Ord(st));
    Str(st, s);
    ls.AddXY(10, Ord(st), s, clGreen);
    ls.AddXY(19, Ord(st));
    ls.Marks.Visible := true;
    ls.Marks.Style := smsLabel;
    ls.Marks.Distance := 4;
    Chart.AddSeries(ls);
  end;
end;

procedure TPointersFrame.sePointerSizeChange(Sender: TObject);
var
  ls: TLineSeries;
begin
  for ls in TLineSeriesEnum.Create(Chart) do
    with ls.Pointer do begin
      HorizSize := sePointerSize.Value;
      VertSize := sePointerSize.Value;
    end;
end;

end.

