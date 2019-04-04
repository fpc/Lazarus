unit ceSeriesDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, ExtCtrls,
  StdCtrls,
  TACustomSeries, TASeries, TAGraph, TAChartCombos,
  cePenFrame, ceBrushFrame, cePointerFrame;

type

  { TSeriesEditor }

  TSeriesEditor = class(TForm)
    AreaSeriesDropLinesPenFrame: TPenFrame;
    Bevel1: TBevel;
    BarSeriesBrushFrame: TBrushFrame;
    Bevel2: TBevel;
    Bevel3: TBevel;
    AreaSeriesBrushFrame: TBrushFrame;
    Bevel4: TBevel;
    ButtonPanel: TButtonPanel;
    cbAreaShowDropLines: TCheckBox;
    cbLineSeriesShowPoints: TCheckBox;
    cbPenColor: TColorButton;
    cbPenStyle: TChartComboBox;
    cbPenWidth: TChartComboBox;
    cbShowSeries: TCheckBox;
    cbLineSeriesShowLines: TCheckBox;
    cbBarShape: TComboBox;
    cbAreaShowContourLines: TCheckBox;
    cbShowInLegend: TCheckBox;
    cbLegendMultiplicity: TComboBox;
    cbMarksStyle: TComboBox;
    cbShowMarks: TCheckBox;
    edMarksFormat: TEdit;
    edSeriesTitle: TEdit;
    gbAreaDropLinePen: TGroupBox;
    gbLineSeriesLineStyle: TGroupBox;
    gbBarSeriesBorder: TGroupBox;
    gbBarSeriesBrush: TGroupBox;
    gbBarShape: TGroupBox;
    gbLineSeriesPointer: TGroupBox;
    gbAreaSeriesBrush: TGroupBox;
    gbAreaContourPen: TGroupBox;
    gbLegendText: TGroupBox;
    gbMarks: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    lblSeriesMarksStyle: TLabel;
    lblPenStyle: TLabel;
    lblPenWidth: TLabel;
    BarSeriesPenFrame: TPenFrame;
    nbSeriesTypes: TNotebook;
    LineSeriesPenFrame: TPenFrame;
    AreaSeriesContourPenFrame: TPenFrame;
    pgAreaSeries: TPage;
    pgBarSeries: TPage;
    pgLineSeries: TPage;
    PanelTop: TPanel;
    LineSeriesPointerFrame: TPointerFrame;
    procedure cbAreaShowContourLinesChange(Sender: TObject);
    procedure cbAreaShowDropLinesChange(Sender: TObject);
    procedure cbBarShapeChange(Sender: TObject);
    procedure cbLegendMultiplicityChange(Sender: TObject);
    procedure cbLineSeriesShowLinesChange(Sender: TObject);
    procedure cbLineSeriesShowPointsChange(Sender: TObject);
    procedure cbMarksStyleChange(Sender: TObject);
    procedure cbShowInLegendChange(Sender: TObject);
    procedure cbShowMarksChange(Sender: TObject);
    procedure cbShowSeriesChange(Sender: TObject);
    procedure edMarksFormatEditingDone(Sender: TObject);
    procedure edSeriesTitleChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    FSeries: TBasicChartSeries;
    FSavedSeries: TBasicChartSeries;
    FSavedActive: Boolean;
    FOKClicked: Boolean;
    procedure ChangedHandler(Sender: TObject);
  protected
    function GetChart: TChart;

  public
    procedure Prepare(ASeries: TBasicChartSeries; ACaptionMask: String);

  end;

var
  SeriesEditor: TSeriesEditor;

implementation

{$R *.lfm}

uses
  TAChartUtils, TATextElements, TALegend,
  ceUtils;

procedure TSeriesEditor.cbShowSeriesChange(Sender: TObject);
begin
  FSeries.Active := cbShowSeries.Checked;
end;

procedure TSeriesEditor.edMarksFormatEditingDone(Sender: TObject);
begin
  if (FSeries is TChartSeries) then
    try
      TChartSeries(FSeries).Marks.Format := edMarksFormat.Text;
    except
    end;
end;

procedure TSeriesEditor.edSeriesTitleChange(Sender: TObject);
begin
  (FSeries as TCustomChartSeries).Title := edSeriesTitle.Text;
end;

procedure TSeriesEditor.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if not CanClose then exit;
  if not FOKClicked then begin
    FSeries.Assign(FSavedSeries);
    FSeries.Active := FSavedActive;
    GetChart.Invalidate;
  end;
end;

procedure TSeriesEditor.FormCreate(Sender: TObject);
begin
  BoldHeaders(self);

  LineSeriesPenFrame.OnChange := @ChangedHandler;
  BarSeriesPenFrame.OnChange := @ChangedHandler;
  BarSeriesBrushFrame.OnChange := @ChangedHandler;
end;

procedure TSeriesEditor.FormDestroy(Sender: TObject);
begin
  FSavedSeries.Free;
end;

procedure TSeriesEditor.OKButtonClick(Sender: TObject);
begin
  FOKClicked := true;
end;

procedure TSeriesEditor.cbBarShapeChange(Sender: TObject);
begin
  if FSeries is TBarSeries then
    TBarSeries(FSeries).BarShape := TBarShape(cbBarShape.ItemIndex);
end;

procedure TSeriesEditor.cbLegendMultiplicityChange(Sender: TObject);
begin
  (FSeries as TCustomChartSeries).Legend.Multiplicity := TLegendMultiplicity(cbLegendMultiplicity.ItemIndex);
end;

procedure TSeriesEditor.cbAreaShowContourLinesChange(Sender: TObject);
begin
  if FSeries is TAreaSeries then begin
    if cbAreaShowContourLines.Checked then
      TAreaSeries(FSeries).AreaContourPen.Style := AreaSeriesContourPenFrame.cbPenStyle.PenStyle
    else
      TAreaSeries(FSeries).AreaContourPen.Style := psClear;
  end;
end;

procedure TSeriesEditor.cbAreaShowDropLinesChange(Sender: TObject);
begin
  if FSeries is TAreaSeries then begin
    if cbAreaShowDropLines.Checked then
      TAreaSeries(FSeries).AreaLinesPen.Style := AreaSeriesDropLinesPenFrame.cbPenStyle.PenStyle
    else
      TAreaSeries(FSeries).AreaLinesPen.Style := psClear;
  end;
end;

procedure TSeriesEditor.cbLineSeriesShowLinesChange(Sender: TObject);
begin
  if FSeries is TLineSeries then
    TLineSeries(FSeries).ShowLines := cbLineSeriesShowLines.Checked;
end;

procedure TSeriesEditor.cbLineSeriesShowPointsChange(Sender: TObject);
begin
  if FSeries is TLineSeries then
    TLineSeries(FSeries).ShowPoints := cbLineSeriesShowPoints.Checked;
end;

procedure TSeriesEditor.cbMarksStyleChange(Sender: TObject);
var
  series: TChartSeries;
begin
  if (FSeries is TChartSeries) then begin
    series := TChartSeries(FSeries);
    series.Marks.Style := TSeriesMarksStyle(cbMarksStyle.ItemIndex);
    edMarksFormat.Text := series.Marks.Format;
  end;
end;

procedure TSeriesEditor.cbShowInLegendChange(Sender: TObject);
begin
  (FSeries as TCustomChartSeries).Legend.Visible := cbShowInLegend.Checked;
end;

procedure TSeriesEditor.cbShowMarksChange(Sender: TObject);
begin
  if (FSeries is TChartSeries) then
    TChartSeries(FSeries).Marks.Visible := cbShowMarks.Checked;
end;

procedure TSeriesEditor.ChangedHandler(Sender: TObject);
begin
  GetChart.Invalidate;
end;

function TSeriesEditor.GetChart: TChart;
begin
  Result := FSeries.ParentChart;
end;

procedure TSeriesEditor.Prepare(ASeries: TBasicChartSeries; ACaptionMask: String);
var
  seriesClass: TSeriesClass;
  series: TCustomChartSeries;
begin
  FSeries := ASeries;
  FSavedSeries.Free;
  seriesClass := TSeriesClass(FSeries.ClassType);
  FSavedSeries := seriesClass.Create(FSeries.Owner);
  FSavedSeries.Assign(FSeries);
  FSavedActive := FSeries.Active;
  FOKClicked := false;

  if FSeries is TCustomChartSeries then
    Caption := Format(ACaptionMask, [TCustomChartSeries(FSeries).Title])
  else begin
    Caption := 'SERIES NOT SUPPORTED';
    exit;
  end;
  series := TCustomChartSeries(FSeries);

  cbShowSeries.Checked := series.Active;
  cbShowInLegend.Checked := series.Legend.Visible;
  edSeriesTitle.Text := series.Title;
  cbLegendMultiplicity.ItemIndex := ord(series.Legend.Multiplicity);

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
    LineSeriesPenFrame.Prepare(TLineSeries(ASeries).LinePen);
    LineSeriesPointerFrame.Prepare(TLineSeries(ASeries).Pointer);
  end;

  if ASeries is TBarSeries then begin
    nbSeriesTypes.PageIndex := 1;
    BarSeriesPenFrame.Prepare(TBarSeries(ASeries).BarPen);
    BarSeriesBrushFrame.Prepare(TBarSeries(ASeries).BarBrush);
    cbBarShape.ItemIndex := ord(TBarSeries(ASeries).BarShape);
  end;

  if ASeries is TAreaSeries then begin
    nbSeriesTypes.PageIndex := 2;
    cbAreaShowContourLines.Checked := TAreaSeries(ASeries).AreaContourPen.Style <> psClear;
    cbAreaShowDropLines.Checked := TAreaSeries(ASeries).AreaLinesPen.Style <> psClear;
    AreaSeriesBrushFrame.Prepare(TAreaSeries(ASeries).AreaBrush);
    AreaSeriesContourPenFrame.Prepare(TAreaSeries(ASeries).AreaContourPen);
    AreaSeriesDropLinesPenFrame.Prepare(TAreaSeries(ASeries).AreaLinesPen);
  end;

end;

end.

