unit frmPie;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes,
  Forms, Controls, StdCtrls, ExtCtrls, Spin,
  TAGraph, TAChartUtils, TATextElements, TASeries, TARadialSeries, TASources, TATools;

type
  TPieFrame = class(TFrame)
    cb3D: TCheckBox;
    cmbMarkAttachment: TComboBox;
    cmbMarkPositions: TComboBox;
    cbMarkPositionsCentered: TCheckBox;
    cbRotate: TCheckBox;
    cbShowLabels: TCheckBox;
    Chart: TChart;
    PieSeries: TPieSeries;
    ChartToolset: TChartToolset;
    cmbOrientation: TComboBox;
    lblAngleRange: TLabel;
    lblDepth: TLabel;
    lblDepthBrightnessDelta: TLabel;
    lblDistance: TLabel;
    lblInnerRadius: TLabel;
    lblLabelAngle: TLabel;
    lblStartAngle: TLabel;
    lblViewAngle: TLabel;
    lblWords: TLabel;
    ListChartSource: TListChartSource;
    Panel1: TPanel;
    seAngleRange: TSpinEdit;
    seDepth: TSpinEdit;
    seDepthBrightnessDelta: TSpinEdit;
    seDistance: TSpinEdit;
    seInnerRadius: TSpinEdit;
    seLabelAngle: TSpinEdit;
    seStartAngle: TSpinEdit;
    seViewAngle: TSpinEdit;
    seWords: TSpinEdit;
    procedure cb3DChange(Sender: TObject);
    procedure cbMarkPositionsCenteredChange(Sender: TObject);
    procedure ChartMouseDown(Sender: TObject; Button: TMouseButton; Shift:
      TShiftState; X, Y: Integer);
    procedure cmbMarkAttachmentChange(Sender: TObject);
    procedure cbRotateChange(Sender: TObject);
    procedure cbShowLabelsChange(Sender: TObject);
    procedure cmbMarkPositionsChange(Sender: TObject);
    procedure cmbOrientationChange(Sender: TObject);
    procedure seAngleRangeChange(Sender: TObject);
    procedure seDepthBrightnessDeltaChange(Sender: TObject);
    procedure seDepthChange(Sender: TObject);
    procedure seDistanceChange(Sender: TObject);
    procedure seInnerRadiusChange(Sender: TObject);
    procedure seLabelAngleChange(Sender: TObject);
    procedure seStartAngleChange(Sender: TObject);
    procedure seViewAngleChange(Sender: TObject);
    procedure seWordsChange(Sender: TObject);
  private

  public
    constructor Create(AOwner: TComponent); override;

  end;

implementation

{$R *.lfm}

constructor TPieFrame.Create(AOwner: TComponent);
begin
  inherited;
  seWordsChange(nil);
end;

procedure TPieFrame.cbMarkPositionsCenteredChange(Sender: TObject);
begin
  PieSeries.MarkPositionCentered := cbMarkPositionsCentered.Checked;
end;

procedure TPieFrame.ChartMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  i := PieSeries.FindContainingSlice(Point(X, Y));
  if i < 0 then exit;
  ListChartSource.SetXValue(i, 0.2 - ListChartSource[i]^.X);
  Chart.Invalidate;
end;

procedure TPieFrame.cb3DChange(Sender: TObject);
begin
  if cb3D.Checked then
    PieSeries.Depth := seDepth.Value
  else
    PieSeries.Depth := 0;
  seDepth.Enabled := cb3D.Checked;
  lblDepth.Enabled := cb3D.Checked;
  seDepthBrightnessDelta.Enabled := cb3D.Checked;
  lblDepthBrightnessDelta.Enabled := cb3D.Checked;
  lblViewAngle.Enabled := cb3D.Checked;
  seViewAngle.Enabled := cb3D.Checked;
  cmbOrientation.Enabled := cb3D.Checked;
end;

procedure TPieFrame.cbRotateChange(Sender: TObject);
begin
  PieSeries.RotateLabels := cbRotate.Checked;
end;

procedure TPieFrame.cbShowLabelsChange(Sender: TObject);
begin
  if cbShowLabels.Checked then
    PieSeries.Marks.Style := smsLabel
  else
    PieSeries.Marks.Style := smsNone;
  seWords.Enabled := cbShowLabels.Checked;
  lblWords.Enabled := cbShowLabels.Checked;
  seLabelAngle.Enabled := cbShowLabels.Checked;
  lblLabelAngle.Enabled := cbShowLabels.Checked;
  cmbMarkPositions.Enabled := cbShowLabels.Checked;
  cmbMarkAttachment.Enabled := cbShowlabels.Checked;
  cbMarkPositionsCentered.Enabled := cbShowLabels.Checked;
  cbRotate.Enabled := cbShowLabels.Checked;
end;

procedure TPieFrame.cmbMarkAttachmentChange(Sender: TObject);
begin
  PieSeries.Marks.Attachment := TChartMarkAttachment(cmbMarkAttachment.ItemIndex);
end;

procedure TPieFrame.cmbMarkPositionsChange(Sender: TObject);
begin
  PieSeries.MarkPositions := TPieMarkPositions(cmbMarkPositions.ItemIndex);
end;

procedure TPieFrame.cmbOrientationChange(Sender: TObject);
begin
  PieSeries.Orientation := TPieOrientation(cmbOrientation.ItemIndex);
end;

procedure TPieFrame.seAngleRangeChange(Sender: TObject);
begin
  PieSeries.AngleRange := seAngleRange.Value;
end;

procedure TPieFrame.seDepthBrightnessDeltaChange(Sender: TObject);
begin
  PieSeries.DepthBrightnessDelta := seDepthBrightnessDelta.Value;
end;

procedure TPieFrame.seDepthChange(Sender: TObject);
begin
  PieSeries.Depth := seDepth.Value;
end;

procedure TPieFrame.seDistanceChange(Sender: TObject);
begin
  PieSeries.Marks.Distance := seDistance.Value;
end;

procedure TPieFrame.seInnerRadiusChange(Sender: TObject);
begin
  PieSeries.InnerRadiusPercent := seInnerRadius.Value;
end;

procedure TPieFrame.seLabelAngleChange(Sender: TObject);
begin
  PieSeries.Marks.LabelFont.Orientation := seLabelAngle.Value * 10;
end;

procedure TPieFrame.seStartAngleChange(Sender: TObject);
begin
  PieSeries.StartAngle := seStartAngle.Value;
end;

procedure TPieFrame.seViewAngleChange(Sender: TObject);
begin
  PieSeries.ViewAngle := seViewAngle.Value;
end;

procedure TPieFrame.seWordsChange(Sender: TObject);
var
  r: TMWCRandomGenerator;

  function RandWord: String;
  var
    i: Integer;
  begin
    Result := '';
    SetLength(Result, r.GetInRange(1, 5));
    for i := 1 to Length(Result) do
      Result[i] := Chr(r.GetInRange(Ord('a'), Ord('z')));
  end;

var
  i, j: Integer;
begin
  if seWords.Value = 0 then begin
    PieSeries.Marks.Style := smsValue;
    exit;
  end;

  PieSeries.Marks.Style := smsLabel;
  r := TMWCRandomGenerator.Create;
  try
    r.Seed := 9823743;
    for i := 0 to ListChartSource.Count - 1 do
      ListChartSource[i]^.Text := '';
    for j := 1 to seWords.Value do
      for i := 0 to ListChartSource.Count - 1 do
        with ListChartSource[i]^ do begin
          if Text <> '' then
            Text := Text + ' ';
          Text := Text + RandWord;
        end;
  finally
    r.Free;
  end;
  Chart.Invalidate;
end;

end.

