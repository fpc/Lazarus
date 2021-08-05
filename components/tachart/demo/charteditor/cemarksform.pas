unit ceMarksForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, StdCtrls,
  ExtCtrls, Spin,
  TAGraph, TATextElements, TACustomSeries,
  ceFontFrame, ceShapeBrushPenMarginsFrame, ceSimplePenFrame, ceArrowFrame;

type

  { TMarksForm }

  TMarksForm = class(TForm)
    ButtonPanel: TButtonPanel;
    cbMarksCentered: TCheckBox;
    cmbMarkPositions: TComboBox;
    gbArrow: TGroupBox;
    gbShapeBrushPenMargins: TGroupBox;
    gbLinkPen: TGroupBox;
    gbLabelFont: TGroupBox;
    gbPosition: TGroupBox;
    lblPosition: TLabel;
    lblDistance: TLabel;
    Panel1: TPanel;
    seDistance: TSpinEdit;
    procedure cbMarksCenteredChange(Sender: TObject);
    procedure cmbMarkPositionsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure seDistanceChange(Sender: TObject);
  private
    FSeries: TChartSeries;
    FShapeBrushPenMarginsFrame: TChartShapeBrushPenMarginsFrame;
    FLinkPenFrame: TSimpleChartPenFrame;
    FFontFrame: TChartFontFrame;
    FArrowFrame: TChartArrowFrame;
    function GetChart: TChart;
    procedure ShapeChangedHandler(AShape: TChartLabelShape);
  public
    procedure Prepare(ASeries: TChartSeries);
  end;

var
  MarksForm: TMarksForm;

implementation

{$R *.lfm}

uses
  TASeries,
  ceUtils;


{ TMarksForm }

procedure TMarksForm.cbMarksCenteredChange(Sender: TObject);
begin
  if (FSeries is TBarSeries) then
    TBarSeries(FSeries).MarkPositionCentered := cbMarksCentered.Checked;
end;

procedure TMarksForm.cmbMarkPositionsChange(Sender: TObject);
begin
  if (FSeries is TLineSeries) then
    TLineSeries(FSeries).MarkPositions := TLinearMarkPositions(cmbMarkPositions.ItemIndex)
  else if (FSeries is TAreaSeries) then
    TAreaSeries(FSeries).MarkPositions := TLinearMarkPositions(cmbMarkPositions.ItemIndex)
  else if (FSeries is TBarSeries) then
    TBarSeries(FSeries).MarkPositions := TLinearMarkPositions(cmbMarkPositions.ItemIndex)
  else
    raise Exception.Create('Series type not supported.');
end;

procedure TMarksForm.FormCreate(Sender: TObject);
begin
  FFontFrame := TChartFontFrame.Create(self);
  FFontFrame.Name := '';
  FFontFrame.Align := alClient;
  FFontFrame.BorderSpacing.Around := 8;
  FFontFrame.AutoSize := true;
  FFontFrame.Parent := gbLabelFont;
  gbLabelFont.AutoSize := true;
  gbLabelFont.Caption := 'Marks font';

  FShapeBrushPenMarginsFrame := TChartShapeBrushPenMarginsFrame.Create(self);
  FShapeBrushPenMarginsFrame.Name := '';
  FShapeBrushPenMarginsFrame.Align := alClient;
  FShapeBrushPenMarginsFrame.BorderSpacing.Around := 8;
  FShapeBrushPenMarginsFrame.OnShapeChange := @ShapeChangedHandler;
  FShapeBrushPenMarginsFrame.AutoSize := true;
  FShapeBrushPenMarginsFrame.Parent := gbShapeBrushPenMargins;
  gbShapeBrushPenMargins.AutoSize := true;
  gbShapeBrushPenMargins.Caption := 'Marks shape and background';

  FLinkPenFrame := TSimpleChartPenFrame.Create(self);
  FLinkPenFrame.Name := '';
  FLinkPenFrame.Align := alClient;
  FLinkPenFrame.BorderSpacing.Around := 8;
  FLinkPenFrame.AutoSize := true;
  FLinkPenFrame.Parent := gbLinkPen;
  gbLinkPen.AutoSize := true;
  gbLinkPen.Caption := 'Link line';

  FArrowFrame := TChartArrowFrame.Create(self);
  FArrowFrame.Name := '';
  FArrowFrame.Align := alClient;
  FArrowFrame.BorderSpacing.Around := 8;
  FArrowFrame.AutoSize := true;
  FArrowFrame.Parent := gbArrow;
  gbArrow.AutoSize := true;
  gbArrow.Caption := 'Arrow';

  AutoSize := true;

  BoldHeaders(self);
end;

procedure TMarksForm.FormShow(Sender: TObject);
var
  wf: Integer = 0;
  hf: Integer = 0;
  ws: Integer = 0;
  hs: Integer = 0;
begin
  FFontFrame.GetPreferredsize(wf, hf);
  inc(wf, 2 * FLinkPenFrame.BorderSpacing.Around);
  inc(hf, 2 * FLinkPenFrame.BorderSpacing.Around);
//  inc(wf, FLinkPenFrame.BorderSpacing.Left + FLinkPenFrame.BorderSpacing.Right);
//  inc(hf, FLinkPenFrame.BorderSpacing.Top + FLinkPenFrame.BorderSpacing.Bottom);

  FShapeBrushPenMarginsFrame.GetPreferredSize(ws, hs);
  inc(ws, 2 * FShapeBrushPenMarginsFrame.BorderSpacing.Around);
  inc(hs, 2 * FShapeBrushPenMarginsFrame.BorderSpacing.Around);
//  inc(ws, FShapeBrushPenMarginsFrame.BorderSpacing.Left + FShapeBrushPenMarginsFrame.BorderSpacing.Right);
//  inc(hs, FShapeBrushPenMarginsFrame.BorderSpacing.Top + FShapeBrushPenMarginsFrame.BorderSpacing.Bottom);

  if wf > ws then begin
    gbShapeBrushPenMargins.AnchorSideRight.Control := gbLabelFont;
    gbShapeBrushPenMargins.AnchorSideRight.Side := asrRight;
    gbShapeBrushPenMargins.Anchors := gbShapeBrushPenMargins.Anchors + [akRight];
  end else
  begin
    gbLabelFont.AnchorSideRight.Control := gbShapeBrushPenMargins;
    gbLabelFont.AnchorSideRight.Side := asrRight;
    gbLabelFont.Anchors := gbLabelFont.Anchors + [akRight];
  end;
end;

function TMarksForm.GetChart: TChart;
begin
  Result := FSeries.ParentChart;
end;

procedure TMarksForm.Prepare(ASeries: TChartSeries);
begin
  FSeries := ASeries;

  with ASeries.Marks do
  begin
    FFontFrame.Prepare(LabelFont, true);
    FShapeBrushPenMarginsFrame.Prepare(Shape, LabelBrush, Frame, Margins);
    FLinkPenFrame.Prepare(LinkPen);
    FArrowFrame.Prepare(Arrow);
  end;

  seDistance.Value := ASeries.Marks.Distance;
  if ASeries is TLineSeries then
  begin
    cmbMarkPositions.ItemIndex := ord(TLineSeries(ASeries).MarkPositions);
    cbMarksCentered.Hide;
  end else
  if ASeries is TBarSeries then
  begin
    cmbMarkPositions.ItemIndex := ord(TBarSeries(ASeries).MarkPositions);
    cbMarksCentered.Checked := TBarSeries(ASeries).MarkPositionCentered;
  end else
  if ASeries is TAreaSeries then
  begin
    cmbMarkPositions.ItemIndex := ord(TAreaSeries(ASeries).MarkPositions);
    cbMarksCentered.Hide;
  end else
  begin
    cmbMarkPositions.Hide;
    lblPosition.Hide;
    cbMarksCentered.Hide;
  end;

  FLinkPenFrame.WidthLeft := FArrowFrame.seArrowBaseLength.Left;
  seDistance.BorderSpacing.Left := FArrowFrame.Left +
    FArrowFrame.seArrowBaseLength.Left - lblDistance.Width;
end;

procedure TMarksForm.seDistanceChange(Sender: TObject);
begin
  FSeries.Marks.Distance := seDistance.Value;
end;

procedure TMarksForm.ShapeChangedHandler(AShape: TChartLabelShape);
begin
  FSeries.Marks.Shape := AShape;
end;

end.

