unit ceSeriesFrame;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Forms, Controls, ExtCtrls, StdCtrls,
  TAGraph, TACustomSeries, TASeries,
  ceBrushFrame, cePenFrame, cePointerFrame;

type

  { TChartSeriesFrame }

  TChartSeriesFrame = class(TFrame)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    btnMore: TButton;
    cbAreaShowContourLines: TCheckBox;
    cbAreaShowDropLines: TCheckBox;
    cbBarShape: TComboBox;
    cmbLegendMultiplicity: TComboBox;
    cbLineSeriesShowLines: TCheckBox;
    cbLineSeriesShowPoints: TCheckBox;
    cbMarksStyle: TComboBox;
    cbShowInLegend: TCheckBox;
    cbShowMarks: TCheckBox;
    cbShowSeries: TCheckBox;
    edMarksFormat: TEdit;
    edSeriesTitle: TEdit;
    gbAreaContourPen: TGroupBox;
    gbAreaDropLinePen: TGroupBox;
    gbAreaSeriesBrush: TGroupBox;
    gbBarSeriesBorder: TGroupBox;
    gbBarSeriesBrush: TGroupBox;
    gbBarShape: TGroupBox;
    gbLegendText: TGroupBox;
    gbLineSeriesLineStyle: TGroupBox;
    gbLineSeriesPointer: TGroupBox;
    gbMarks: TGroupBox;
    lblLegendItems: TLabel;
    Label2: TLabel;
    lblSeriesMarksStyle: TLabel;
    nbSeriesTypes: TNotebook;
    PanelTop: TPanel;
    pgAreaSeries: TPage;
    pgBarSeries: TPage;
    pgLineSeries: TPage;
    procedure btnMoreClick(Sender: TObject);
    procedure cbAreaShowContourLinesChange(Sender: TObject);
    procedure cbAreaShowDropLinesChange(Sender: TObject);
    procedure cbBarShapeChange(Sender: TObject);
    procedure cmbLegendMultiplicityChange(Sender: TObject);
    procedure cbLineSeriesShowLinesChange(Sender: TObject);
    procedure cbLineSeriesShowPointsChange(Sender: TObject);
    procedure cbMarksStyleChange(Sender: TObject);
    procedure cbShowInLegendChange(Sender: TObject);
    procedure cbShowMarksChange(Sender: TObject);
    procedure cbShowSeriesChange(Sender: TObject);
    procedure edMarksFormatEditingDone(Sender: TObject);
    procedure edSeriesTitleChange(Sender: TObject);
  private
    FSeries: TBasicChartSeries;
    FLineSeriesPenFrame: TChartPenFrame;
    FLineSeriesPointerFrame: TChartPointerFrame;
    FBarSeriesBrushFrame: TChartBrushFrame;
    FBarSeriesPenFrame: TChartPenFrame;
    FAreaSeriesBrushFrame: TChartBrushFrame;
    FAreaSeriesContourPenFrame: TChartPenFrame;
    FAreaSeriesDropLinesPenFrame: TChartPenFrame;
    FOnChanged: TNotifyEvent;
    procedure ChangedHandler(Sender: TObject);
  protected
    function GetChart: TChart;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Prepare(ASeries: TBasicChartSeries);
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;

  end;

implementation

{$R *.lfm}

uses
  TAChartUtils, TALegend,
  ceUtils, ceMarksForm;

constructor TChartSeriesFrame.Create(AOwner: TComponent);
begin
  inherited;

  FLineSeriesPenFrame := TChartPenFrame.Create(self);
  FLineSeriesPenFrame.Parent := gbLineSeriesLineStyle;
  FLineSeriesPenFrame.Name := '';
  FLineSeriesPenFrame.Align := alClient;
  FLineSeriesPenFrame.BorderSpacing.Around := 8;
  FLineSeriesPenFrame.OnChange := @ChangedHandler;
  gbLineSeriesLineStyle.Caption := 'Connecting lines';
  gbLineSeriesLineStyle.AutoSize := true;

  FLineSeriesPointerFrame := TChartPointerFrame.Create(self);
  FLineSeriesPointerFrame.Parent := gbLineSeriesPointer;
  FLineSeriesPointerFrame.Name := '';
  FLineSeriesPointerFrame.AutoSize := true;
  FLineSeriesPointerFrame.Align := alClient;
  FLineSeriesPointerFrame.BorderSpacing.Around := 8;
  FLineSeriesPointerFrame.OnChange := @ChangedHandler;
  gbLineSeriesPointer.Caption := 'Series pointer';
  gbLineSeriesPointer.AutoSize := true;

  { Bar series page }
  FBarSeriesBrushFrame := TChartBrushFrame.Create(self);
  FBarSeriesBrushFrame.Parent := gbBarSeriesBrush;
  FBarSeriesBrushFrame.Name := '';
  FBarSeriesBrushFrame.Align := alClient;
  FBarSeriesBrushFrame.BorderSpacing.Around := 8;
  FBarSeriesBrushFrame.OnChange := @ChangedHandler;
  gbBarSeriesBrush.Caption := 'Bar brush';
  gbBarSeriesBrush.AutoSize := true;

  FBarSeriesPenFrame := TChartPenFrame.Create(self);
  FBarSeriesPenFrame.Parent := gbBarSeriesBorder;
  FBarSeriesPenFrame.Name := '';
  FBarSeriesPenFrame.Align := alClient;
  FBarSeriesPenFrame.BorderSpacing.Around := 8;
  FBarSeriesPenFrame.OnChange := @ChangedHandler;
  gbBarSeriesBorder.Caption := 'Bar borders';
  gbBarSeriesBorder.AutoSize := true;

  cbBarShape.DropdownCount := DEFAULT_DROPDOWN_COUNT;

  { Area series page }
  FAreaSeriesBrushFrame := TChartBrushFrame.Create(self);
  FAreaSeriesBrushFrame.Parent := gbAreaSeriesBrush;
  FAreaSeriesBrushFrame.Name := '';
  FAreaSeriesBrushFrame.Align := alClient;
  FAreaSeriesBrushFrame.BorderSpacing.Around := 8;
  FAreaSeriesBrushFrame.OnChange := @ChangedHandler;
  gbAreaSeriesBrush.Caption := 'Area fill';
  gbAreaSeriesBrush.AutoSize := true;

  FAreaSeriesContourPenFrame := TChartPenFrame.Create(self);
  FAreaSeriesContourPenFrame.Parent := gbAreaContourPen;
  FAreaSeriesContourPenFrame.Name := '';
  FAreaSeriesContourPenFrame.Align := alClient;
  FAreaSeriesContourPenFrame.BorderSpacing.Around := 8;
  FAreaSeriesContourPenFrame.OnChange := @ChangedHandler;
  gbAreaContourPen.Caption := 'Border';
  gbAreaContourPen.AutoSize := true;

  FAreaSeriesDropLinesPenFrame := TChartPenFrame.Create(self);
  FAreaSeriesDropLinesPenFrame.Parent := gbAreaDropLinePen;
  FAreaSeriesDropLinesPenFrame.Name := '';
  FAreaSeriesDropLinesPenFrame.Align := alClient;
  FAreaSeriesDropLinesPenFrame.BorderSpacing.Around := 8;
  FAreaSeriesDropLinesPenFrame.OnChange := @ChangedHandler;
  gbAreaDropLinePen.Caption := 'Drop lines';
  gbAreaDropLinePen.AutoSize := true;

  { for all }
  BoldHeaders(self);

  cmbLegendMultiplicity.DropdownCount := DEFAULT_DROPDOWN_COUNT;
  cbMarksStyle.DropdownCount := DEFAULT_DROPDOWN_COUNT;
end;

procedure TChartSeriesFrame.cbAreaShowContourLinesChange(Sender: TObject);
begin
  if FSeries is TAreaSeries then begin
    if cbAreaShowContourLines.Checked then
      TAreaSeries(FSeries).AreaContourPen.Style := FAreaSeriesContourPenFrame.cbPenStyle.PenStyle
    else
      TAreaSeries(FSeries).AreaContourPen.Style := psClear;
    FAreaSeriesContourPenFrame.Enabled := cbAreaShowContourLines.Checked;
  end;
end;

procedure TChartSeriesFrame.btnMoreClick(Sender: TObject);
var
  F: TMarksForm;
  ser: TBasicChartSeries;
begin
  if not (FSeries is TChartSeries) then
    exit;

  F := TMarksForm.Create(GetParentForm(self));
  try
    ser := TSeriesClass(FSeries.ClassType).Create(nil);
    ser.Assign(FSeries);

    F.Prepare(TChartSeries(FSeries));
    F.Position := poOwnerFormCenter;
    if F.ShowModal <> mrOK then
      FSeries.Assign(ser);
  finally
    ser.Free;
    F.Free;
  end;
end;

procedure TChartSeriesFrame.cbAreaShowDropLinesChange(Sender: TObject);
begin
  if FSeries is TAreaSeries then begin
    if cbAreaShowDropLines.Checked then
      TAreaSeries(FSeries).AreaLinesPen.Style := FAreaSeriesDropLinesPenFrame.cbPenStyle.PenStyle
    else
      TAreaSeries(FSeries).AreaLinesPen.Style := psClear;
    FAreaSeriesDropLinesPenFrame.Enabled := cbAreaShowDropLines.Checked;
  end;
end;

procedure TChartSeriesFrame.cbBarShapeChange(Sender: TObject);
begin
  if FSeries is TBarSeries then
    TBarSeries(FSeries).BarShape := TBarShape(cbBarShape.ItemIndex);
end;

procedure TChartSeriesFrame.cmbLegendMultiplicityChange(Sender: TObject);
begin
  (FSeries as TCustomChartSeries).Legend.Multiplicity := TLegendMultiplicity(cmbLegendMultiplicity.ItemIndex);
end;

procedure TChartSeriesFrame.cbLineSeriesShowLinesChange(Sender: TObject);
begin
  if FSeries is TLineSeries then
  begin
    TLineSeries(FSeries).ShowLines := cbLineSeriesShowLines.Checked;
    FLineSeriesPenFrame.Enabled := cbLineSeriesShowLines.Checked;
  end;
end;

procedure TChartSeriesFrame.cbLineSeriesShowPointsChange(Sender: TObject);
begin
  if FSeries is TLineSeries then
    TLineSeries(FSeries).ShowPoints := cbLineSeriesShowPoints.Checked;
  FLineSeriesPointerFrame.Enabled := cbLineSeriesShowPoints.Checked;
end;

procedure TChartSeriesFrame.cbMarksStyleChange(Sender: TObject);
var
  series: TChartSeries;
begin
  if (FSeries is TChartSeries) then begin
    series := TChartSeries(FSeries);
    series.Marks.Style := TSeriesMarksStyle(cbMarksStyle.ItemIndex);
    edMarksFormat.Text := series.Marks.Format;
    btnMore.Enabled := (series.Marks.Style <> smsNone) and cbShowMarks.Checked;
  end;
end;

procedure TChartSeriesFrame.cbShowInLegendChange(Sender: TObject);
begin
  (FSeries as TCustomChartSeries).Legend.Visible := cbShowInLegend.Checked;
  lblLegendItems.Enabled := cbShowInLegend.Checked;
  cmbLegendMultiplicity.Enabled := cbShowInLegend.Checked;
end;

procedure TChartSeriesFrame.cbShowMarksChange(Sender: TObject);
begin
  if (FSeries is TChartSeries) then
  begin
    TChartSeries(FSeries).Marks.Visible := cbShowMarks.Checked;
    btnMore.Enabled := (TChartSeries(FSeries).Marks.Style <> smsNone) and cbShowMarks.Checked;
  end;
end;

procedure TChartSeriesFrame.cbShowSeriesChange(Sender: TObject);
begin
  FSeries.Active := cbShowSeries.Checked;
  gbLegendText.Visible := cbShowSeries.Checked;
  gbMarks.Visible := cbShowSeries.Checked;
  nbSeriesTypes.Visible := cbShowSeries.Checked;
end;

procedure TChartSeriesFrame.ChangedHandler(Sender: TObject);
begin
  GetChart.Invalidate;
  if Assigned(FOnChanged) then FOnChanged(self);
end;

procedure TChartSeriesFrame.edMarksFormatEditingDone(Sender: TObject);
begin
  if (FSeries is TChartSeries) then
    try
      TChartSeries(FSeries).Marks.Format := edMarksFormat.Text;
    except
    end;
end;

procedure TChartSeriesFrame.edSeriesTitleChange(Sender: TObject);
begin
  (FSeries as TCustomChartSeries).Title := edSeriesTitle.Text;
end;

function TChartSeriesFrame.GetChart: TChart;
begin
  Result := FSeries.ParentChart;
end;

procedure TChartSeriesFrame.Prepare(ASeries: TBasicChartSeries);
var
  series: TCustomChartSeries;
begin
  FSeries := ASeries;
  series := TCustomChartSeries(FSeries);

  cbShowSeries.Checked := series.Active;
  cbShowInLegend.Checked := series.Legend.Visible;
  edSeriesTitle.Text := series.Title;
  cmbLegendMultiplicity.ItemIndex := ord(series.Legend.Multiplicity);

  gbMarks.Visible := (FSeries is TChartSeries);
  if (FSeries is TChartSeries) then begin
    cbMarksStyle.ItemIndex := ord(TChartSeries(FSeries).Marks.Style);
    edMarksFormat.Text := TChartSeries(FSeries).Marks.Format;
    cbShowMarks.Checked := TChartSeries(FSeries).Marks.Visible;
  end;

  if ASeries is TLineSeries then begin
    nbSeriesTypes.PageIndex := 0;
    cbLineSeriesShowLines.Checked := TLineSeries(ASeries).ShowLines;
    cbLineSeriesShowPoints.Checked := TLineSeries(ASeries).ShowPoints;
    FLineSeriesPenFrame.Prepare(TLineSeries(ASeries).LinePen);
    FLineSeriesPointerFrame.Prepare(TLineSeries(ASeries).Pointer);
  end;

  if ASeries is TBarSeries then begin
    nbSeriesTypes.PageIndex := 1;
    FBarSeriesPenFrame.Prepare(TBarSeries(ASeries).BarPen);
    FBarSeriesBrushFrame.Prepare(TBarSeries(ASeries).BarBrush);
    cbBarShape.ItemIndex := ord(TBarSeries(ASeries).BarShape);
  end;

  if ASeries is TAreaSeries then begin
    nbSeriesTypes.PageIndex := 2;
    cbAreaShowContourLines.Checked := TAreaSeries(ASeries).AreaContourPen.Style <> psClear;
    cbAreaShowDropLines.Checked := TAreaSeries(ASeries).AreaLinesPen.Style <> psClear;
    FAreaSeriesBrushFrame.Prepare(TAreaSeries(ASeries).AreaBrush);
    FAreaSeriesContourPenFrame.Prepare(TAreaSeries(ASeries).AreaContourPen);
    FAreaSeriesDropLinesPenFrame.Prepare(TAreaSeries(ASeries).AreaLinesPen);
  end;

end;

end.

