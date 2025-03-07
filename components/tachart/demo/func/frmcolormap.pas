unit frmColorMap;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes,
  Forms, Controls, StdCtrls, ExtCtrls, Spin,
  TAGraph, TALegend, TALegendPanel, TAFuncSeries, TASources, TATools;

type
  TColorMapFrame = class(TFrame)
    cbInterpolate: TCheckBox;
    cbMultLegend: TCheckBox;
    cbNiceLegend: TCheckBox;
    Chart: TChart;
    ColorMapSeries: TColorMapSeries;
    ChartLegendPanel1: TChartLegendPanel;
    chtsColorMap: TChartToolset;
    ColorSource: TListChartSource;
    PanDragTool: TPanDragTool;
    Splitter1: TSplitter;
    ZoomDragTool: TZoomDragTool;
    cmbPalette: TComboBox;
    lblPalette: TLabel;
    lblStep: TLabel;
    Panel1: TPanel;
    seStep: TSpinEdit;
    procedure cbInterpolateChange(Sender: TObject);
    procedure cbMultLegendChange(Sender: TObject);
    procedure cbNiceLegendChange(Sender: TObject);
    procedure ColorMapSeriesCalculate(const AX, AY: Double; out AZ: Double);
    procedure cmbPaletteChange(Sender: TObject);
    procedure seStepChange(Sender: TObject);
  private

  public
    constructor Create(AOwner: TComponent); override;

  end;

implementation

{$R *.lfm}

constructor TColorMapFrame.Create(AOwner: TComponent);
begin
  inherited;
  seStep.Value := ColorMapSeries.StepX;
end;

procedure TColorMapFrame.cbInterpolateChange(Sender: TObject);
begin
  ColorMapSeries.Interpolate := cbInterpolate.Checked;
end;

procedure TColorMapFrame.cbMultLegendChange(Sender: TObject);
begin
  with ColorMapSeries.Legend do
    if cbMultLegend.Checked then
      Multiplicity := lmPoint
    else
    begin
      Multiplicity := lmSingle;
      Format := '';
    end;
end;

procedure TColorMapFrame.cbNiceLegendChange(Sender: TObject);
begin
  if cbMultLegend.Checked and cbNiceLegend.Checked then
    ColorMapSeries.Legend.Format := 'z ≤ %1:.3f|%.3f < z ≤ %.3f|%.3f < z'
  else
    ColorMapSeries.Legend.Format := '';
end;

procedure TColorMapFrame.cmbPaletteChange(Sender: TObject);
begin
  if cmbPalette.ItemIndex < cmbPalette.Items.Count-1 then begin
    ColorMapSeries.ColorSource := nil;
    ColorMapSeries.BuiltinPalette := TColorMapPalette(cmbPalette.ItemIndex);
  end else
    ColorMapSeries.ColorSource := ColorSource;
end;

procedure TColorMapFrame.ColorMapSeriesCalculate(const AX, AY:
  Double; out AZ: Double);
begin
  AZ := Sin(10 * Sqr(AX) + 17 * Sqr(AY));
end;

procedure TColorMapFrame.seStepChange(Sender: TObject);
begin
  ColorMapSeries.StepX := seStep.Value;
  ColorMapSeries.StepY := seStep.Value;
end;

end.

