unit frmGeneral;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math,
  Graphics, Forms, Controls, StdCtrls, ExtCtrls, Spin,
  TAGraph, TAChartUtils, TASeries, TATextElements, TAStyles;

type
  TGeneralFrame = class(TFrame)
    BtnNewData: TButton;
    Cb3D: TCheckBox;
    CbBanded: TCheckBox;
    CbCentered: TCheckBox;
    CmbLabelsAt: TComboBox;
    CbRotated: TCheckBox;
    CbShowDataLabels: TCheckBox;
    CbShowDropLines: TCheckBox;
    CbShowLegend: TCheckBox;
    CbStacked: TCheckBox;
    CbUseZeroLevel: TCheckBox;
    Chart: TChart;
    AreaSeries: TAreaSeries;
    ChartStyles: TChartStyles;
    EdCount: TSpinEdit;
    EdDepthBrightnessDelta: TSpinEdit;
    EdYCount: TSpinEdit;
    EdZeroLevel: TFloatSpinEdit;
    LblCount: TLabel;
    LblDepthBrightnessDelta: TLabel;
    LblYCount: TLabel;
    ParamsPanel: TPanel;
    procedure BtnNewDataClick(Sender: TObject);
    procedure Cb3DChange(Sender: TObject);
    procedure CbBandedChange(Sender: TObject);
    procedure CbCenteredChange(Sender: TObject);
    procedure CmbLabelsAtChange(Sender: TObject);
    procedure CbRotatedChange(Sender: TObject);
    procedure CbShowDataLabelsChange(Sender: TObject);
    procedure CbShowDropLinesChange(Sender: TObject);
    procedure CbShowLegendChange(Sender: TObject);
    procedure CbStackedChange(Sender: TObject);
    procedure CbUseZeroLevelChange(Sender: TObject);
    procedure EdCountChange(Sender: TObject);
    procedure EdDepthBrightnessDeltaChange(Sender: TObject);
    procedure EdYCountChange(Sender: TObject);
    procedure EdZeroLevelChange(Sender: TObject);
  private
    procedure PopulateSeries(N, NY: Integer);
    procedure UpdateStyles(Count: Integer);

  public
    constructor Create(AOwner: TComponent); override;

  end;

implementation

{$R *.lfm}

constructor TGeneralFrame.Create(AOwner: TComponent);
begin
  inherited;
  PopulateSeries(EdCount.Value, EdYCount.Value);
end;

procedure TGeneralFrame.BtnNewDataClick(Sender: TObject);
begin
  PopulateSeries(EdCount.Value, EdYCount.Value);
end;

procedure TGeneralFrame.Cb3DChange(Sender: TObject);
const
  DEPTH = 20;
begin
  AreaSeries.Depth := IfThen(Cb3D.Checked, DEPTH, 0);
  Chart.Margins.Right := 4 + AreaSeries.Depth;
  Chart.Margins.Top := 4 + AreaSeries.Depth;
  EdDepthBrightnessDelta.Enabled := Cb3D.Checked;
  lblDepthBrightnessDelta.Enabled := Cb3D.Checked;
end;

procedure TGeneralFrame.CbBandedChange(Sender: TObject);
begin
  AreaSeries.Banded := CbBanded.Checked;
  if AreaSeries.Banded then
    UpdateStyles(AreaSeries.ListSource.YCount-1)
  else
    UpdateStyles(AreaSeries.ListSource.YCount);
end;

procedure TGeneralFrame.CbCenteredChange(Sender: TObject);
begin
  AreaSeries.MarkPositionCentered := CbCentered.Checked;
  if CbCentered.Checked then begin
    AreaSeries.Marks.Distance := 0;
    AreaSeries.Marks.Attachment := maCenter;
  end else begin
    AreaSeries.Marks.Distance := 20;
    AreaSeries.Marks.Attachment := maDefault;
  end;
end;

procedure TGeneralFrame.CmbLabelsAtChange(Sender: TObject);
begin
  if CmbLabelsAt.ItemIndex = 0 then
    AreaSeries.Marks.YIndex := - 1
  else
    AreaSeries.Marks.YIndex := CmbLabelsAt.ItemIndex - 1;
end;

procedure TGeneralFrame.CbRotatedChange(Sender: TObject);
var
  tmp: Integer;
begin
  tmp := AreaSeries.AxisIndexX;
  AreaSeries.AxisIndexX := AreaSeries.AxisIndexY;
  AreaSeries.AxisIndexY := tmp;
end;

procedure TGeneralFrame.CbShowDataLabelsChange(Sender: TObject);
begin
  if CbShowDataLabels.Checked then begin
    AreaSeries.Marks.Style := smsLabel;
    AreaSeries.Marks.Format := '%.2f';
  end else
    AreaSeries.Marks.Style := smsNone;
  CbCentered.Enabled := CbShowDataLabels.Checked;
  CmbLabelsAt.Enabled := CbShowDataLabels.Checked;
end;

procedure TGeneralFrame.CbShowDropLinesChange(Sender: TObject);
begin
  if CbShowDropLines.Checked then
    AreaSeries.AreaLinesPen.Style := psSolid
  else
    AreaSeries.AreaLinesPen.Style := psClear;
end;

procedure TGeneralFrame.CbShowLegendChange(Sender: TObject);
begin
  Chart.Legend.Visible := CbShowLegend.Checked;
end;

procedure TGeneralFrame.CbStackedChange(Sender: TObject);
begin
  AreaSeries.Stacked := CbStacked.Checked;
end;

procedure TGeneralFrame.CbUseZeroLevelChange(Sender: TObject);
begin
  AreaSeries.UseZeroLevel := CbUseZeroLevel.Checked;
end;

procedure TGeneralFrame.EdCountChange(Sender: TObject);
begin
  PopulateSeries(EdCount.Value, EdYCount.Value);
end;

procedure TGeneralFrame.EdDepthBrightnessDeltaChange(Sender: TObject);
begin
  AreaSeries.DepthBrightnessDelta := EdDepthBrightnessDelta.Value;
end;

procedure TGeneralFrame.EdYCountChange(Sender: TObject);
begin
  PopulateSeries(EdCount.Value, EdYCount.Value);
end;

procedure TGeneralFrame.EdZeroLevelChange(Sender: TObject);
begin
  AreaSeries.ZeroLevel := EdZeroLevel.Value;
end;

procedure TGeneralFrame.PopulateSeries(N, NY: Integer);
var
  i: Integer;
  idx: Integer;
begin
  AreaSeries.Clear;
  AreaSeries.ListSource.YCount := NY;
  for i:=1 to N do
    AreaSeries.AddXY(i, Random, [0.1 + Random, 0.2 + Random, 0.3 + Random, 0.2 + Random]);

  UpdateStyles(AreaSeries.ListSource.YCount);

  idx := CmbLabelsAt.ItemIndex;
  CmbLabelsAt.Clear;
  CmbLabelsAt.Items.Add('all');
  for i:= 0 to NY - 1 do
    CmbLabelsAt.Items.Add('y index ' + IntToStr(i));
  if (idx > -1) and (idx < CmbLabelsAt.Items.Count) then
    CmbLabelsAt.ItemIndex := idx
  else
    CmbLabelsAt.ItemIndex := 0;
end;

procedure TGeneralFrame.UpdateStyles(Count: Integer);
const
  COLORS: array[0..4] of TColor = (clRed, clBlue, clYellow, clGreen, clFuchsia);
var
  i: Integer;
begin
  ChartStyles.Styles.Clear;
  for i := 1 to Count do
    with ChartStyles.Add do begin
      Brush.Color := COLORS[i-1];
      Pen.Width := 3;
      Text := 'Curve ' + IntToStr(i);
    end;
end;


end.

