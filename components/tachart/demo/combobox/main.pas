unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TAChartListbox, TAChartCombos, TASeries,
  TASources, TATools, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ColorBox, Spin, Types;

type

  { TForm1 }

  TForm1 = class(TForm)
    Bevel2: TBevel;
    CbAreaSerBrushColor: TColorBox;
    CbAreaSerBrushStyle: TChartComboBox;
    CbAreaSerContourColor: TColorBox;
    CbAreaSerLinesColor: TColorBox;
    CbAreaSerLinesStyle: TChartComboBox;
    CbAreaSerLinesWidth: TChartComboBox;
    CbBarSerPenStyle: TChartComboBox;
    CbAreaSerContourStyle: TChartComboBox;
    CbBarSerPenWidth: TChartComboBox;
    CbBarSerPenColor: TColorBox;
    CbAreaSerContourWidth: TChartComboBox;
    CbLineSerPointerBorderColor: TColorBox;
    CbLineSerPointerBrushColor: TColorBox;
    CbBarSerBrushColor: TColorBox;
    CbShowLines: TCheckBox;
    Chart1: TChart;
    CbLineSerLinePenStyle: TChartComboBox;
    CbLineSerLinePenWidth: TChartComboBox;
    CbLineSerPointerStyle: TChartComboBox;
    Chart1AreaSeries1: TAreaSeries;
    Chart1BarSeries1: TBarSeries;
    Chart1LineSeries1: TLineSeries;
    CbBarSerBrushStyle: TChartComboBox;
    ChartListbox1: TChartListbox;
    CbShow: TCheckBox;
    CbLineSerPenColor: TColorBox;
    ChartToolset1: TChartToolset;
    ChartToolset1DataPointClickTool1: TDataPointClickTool;
    CbShowPoints: TCheckBox;
    GbBarSerBrush1: TGroupBox;
    GbBarSerPen1: TGroupBox;
    GbBarSerPen2: TGroupBox;
    GbLineSerLines: TGroupBox;
    GbBarSerPen: TGroupBox;
    GbLineSerPointer: TGroupBox;
    GbBarSerBrush: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LblSeries: TLabel;
    Notebook1: TNotebook;
    Page1: TPage;
    Page2: TPage;
    Page3: TPage;
    Panel1: TPanel;
    RandomChartSource1: TRandomChartSource;
    RandomChartSource2: TRandomChartSource;
    RandomChartSource3: TRandomChartSource;
    EdLineSerPointerSize: TSpinEdit;
    Splitter1: TSplitter;
    procedure CbAreaSerBrushColorChange(Sender: TObject);
    procedure CbAreaSerBrushStyleChange(Sender: TObject);
    procedure CbAreaSerContourColorChange(Sender: TObject);
    procedure CbAreaSerContourStyleChange(Sender: TObject);
    procedure CbAreaSerContourWidthChange(Sender: TObject);
    procedure CbAreaSerLinesColorChange(Sender: TObject);
    procedure CbAreaSerLinesStyleChange(Sender: TObject);
    procedure CbAreaSerLinesWidthChange(Sender: TObject);
    procedure CbBarSerBrushColorChange(Sender: TObject);
    procedure CbBarSerBrushStyleChange(Sender: TObject);
    procedure CbBarSerPenStyleChange(Sender: TObject);
    procedure CbBarSerPenWidthChange(Sender: TObject);
    procedure CbBarSerPenColorChange(Sender: TObject);
    procedure CbLineSerPointerBorderColorChange(Sender: TObject);
    procedure CbLineSerLinePenStyleChange(Sender: TObject);
    procedure CbLineSerLinePenWidthChange(Sender: TObject);
    procedure CbLineSerPenColorChange(Sender: TObject);
    procedure CbLineSerPointerBrushColorChange(Sender: TObject);
    procedure CbLineSerPointerStyleChange(Sender: TObject);
    procedure CbShowChange(Sender: TObject);
    procedure CbShowLinesChange(Sender: TObject);
    procedure CbShowPointsChange(Sender: TObject);
    procedure ChartListbox1Click(Sender: TObject);
    procedure ChartToolset1DataPointClickTool1PointClick(ATool: TChartTool;
      APoint: TPoint);
    procedure EdLineSerPointerSizeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  TACustomSeries;


{ TAreaSeries }

procedure TForm1.CbAreaSerBrushColorChange(Sender: TObject);
var
  ser: TAreaSeries;
begin
  ser := ChartListbox1.Series[ChartListbox1.ItemIndex] as TAreaSeries;
  ser.AreaBrush.Color := CbAreaSerBrushColor.Selected;
end;

procedure TForm1.CbAreaSerBrushStyleChange(Sender: TObject);
var
  ser: TAreaSeries;
begin
  ser := ChartListbox1.Series[ChartListbox1.ItemIndex] as TAreaSeries;
  ser.AreaBrush.Style := CbAreaSerBrushStyle.BrushStyle;
end;

procedure TForm1.CbAreaSerContourColorChange(Sender: TObject);
var
  ser: TAreaSeries;
begin
  ser := ChartListbox1.Series[ChartListbox1.ItemIndex] as TAreaSeries;
  ser.AreaContourPen.Color := CbAreaSerContourColor.Selected;
end;

procedure TForm1.CbAreaSerContourStyleChange(Sender: TObject);
var
  ser: TAreaSeries;
begin
  ser := ChartListbox1.Series[ChartListbox1.ItemIndex] as TAreaSeries;
  ser.AreaContourPen.Style := CbAreaSerContourStyle.PenStyle;
end;

procedure TForm1.CbAreaSerContourWidthChange(Sender: TObject);
var
  ser: TAreaSeries;
begin
  ser := ChartListbox1.Series[ChartListbox1.ItemIndex] as TAreaSeries;
  ser.AreaContourPen.Width := CbAreaSerContourWidth.PenWidth;
end;

procedure TForm1.CbAreaSerLinesColorChange(Sender: TObject);
var
  ser: TAreaSeries;
begin
  ser := ChartListbox1.Series[ChartListbox1.ItemIndex] as TAreaSeries;
  ser.AreaLinesPen.Color := CbAreaSerLinesColor.Selected;
end;

procedure TForm1.CbAreaSerLinesStyleChange(Sender: TObject);
var
  ser: TAreaSeries;
begin
  ser := ChartListbox1.Series[ChartListbox1.ItemIndex] as TAreaSeries;
  ser.AreaLinesPen.Style := CbAreaSerLinesStyle.PenStyle;
end;

procedure TForm1.CbAreaSerLinesWidthChange(Sender: TObject);
var
  ser: TAreaSeries;
begin
  ser := ChartListbox1.Series[ChartListbox1.ItemIndex] as TAreaSeries;
  ser.AreaLinesPen.Width := CbAreaSerLinesWidth.PenWidth;
end;


{ TBarSeries }

procedure TForm1.CbBarSerBrushColorChange(Sender: TObject);
var
  ser: TBarSeries;
begin
  ser := ChartListbox1.Series[ChartListbox1.ItemIndex] as TBarSeries;
  ser.BarBrush.Color := CbBarSerBrushColor.Selected;
end;

procedure TForm1.CbBarSerBrushStyleChange(Sender: TObject);
var
  ser: TBarSeries;
begin
  ser := ChartListbox1.Series[ChartListbox1.ItemIndex] as TBarSeries;
  ser.BarBrush.Style := CbBarSerBrushStyle.BrushStyle;
end;

procedure TForm1.CbBarSerPenStyleChange(Sender: TObject);
var
  ser: TBarSeries;
begin
  ser := ChartListbox1.Series[ChartListbox1.ItemIndex] as TBarSeries;
  ser.BarPen.Style := CbBarSerPenStyle.PenStyle;
end;

procedure TForm1.CbBarSerPenWidthChange(Sender: TObject);
var
  ser: TBarSeries;
begin
  ser := ChartListbox1.Series[ChartListbox1.ItemIndex] as TBarSeries;
  ser.BarPen.Width := CbBarSerPenWidth.PenWidth;
end;

{ Line series }

procedure TForm1.CbLineSerLinePenStyleChange(Sender: TObject);
var
  ser: TLineSeries;
begin
  ser := ChartListbox1.Series[ChartListbox1.ItemIndex] as TLineSeries;
  CbLineSerLinePenStyle.SetPenPattern(ser.LinePen);
  ser.LinePen.Cosmetic := CbLineSerLinePenStyle.Cosmetic;
  ser.LinePen.Style := CbLineSerLinePenStyle.PenStyle;
end;

procedure TForm1.CbLineSerLinePenWidthChange(Sender: TObject);
var
  ser: TLineSeries;
begin
  ser := ChartListbox1.Series[ChartListbox1.ItemIndex] as TLineSeries;
  ser.LinePen.Width := CbLineSerLinePenWidth.PenWidth;
end;

procedure TForm1.CbBarSerPenColorChange(Sender: TObject);
var
  ser: TBarSeries;
begin
  ser := ChartListbox1.Series[ChartListbox1.ItemIndex] as TBarSeries;
  ser.BarPen.Color := CbBarSerPenColor.Selected;
end;

procedure TForm1.CbLineSerPenColorChange(Sender: TObject);
var
  ser: TLineSeries;
begin
  ser := ChartListbox1.Series[ChartListbox1.ItemIndex] as TLineSeries;
  ser.LinePen.Color := CbLineSerPenColor.Selected;
end;

procedure TForm1.CbLineSerPointerBorderColorChange(Sender: TObject);
var
  ser: TLineSeries;
begin
  ser := ChartListbox1.Series[ChartListbox1.ItemIndex] as TLineSeries;
  ser.Pointer.Pen.Color := CbLineSerPointerBorderColor.Selected;
end;

procedure TForm1.CbLineSerPointerBrushColorChange(Sender: TObject);
var
  ser: TLineSeries;
begin
  ser := ChartListbox1.Series[ChartListbox1.ItemIndex] as TLineSeries;
  ser.Pointer.Brush.Color := CbLineSerPointerBrushColor.Selected;
end;

procedure TForm1.CbLineSerPointerStyleChange(Sender: TObject);
var
  ser: TLineSeries;
begin
  ser := ChartListbox1.Series[ChartListbox1.ItemIndex] as TLineSeries;
  ser.Pointer.Style := CbLineSerPointerStyle.PointerStyle;
end;


{ Other events }

procedure TForm1.CbShowChange(Sender: TObject);
var
  ser: TCustomChartSeries;
begin
  ser := ChartListbox1.Series[ChartListbox1.ItemIndex];
  ser.Active := CbShow.Checked;
end;

procedure TForm1.CbShowLinesChange(Sender: TObject);
var
  ser: TLineSeries;
begin
  ser := ChartListbox1.Series[ChartListbox1.ItemIndex] as TLineSeries;
  ser.ShowLines := CbShowLines.Checked;
end;

procedure TForm1.CbShowPointsChange(Sender: TObject);
var
  ser: TLineSeries;
begin
  ser := ChartListbox1.Series[ChartListbox1.ItemIndex] as TLineSeries;
  ser.ShowPoints := CbShowPoints.Checked;
end;

procedure TForm1.ChartListbox1Click(Sender: TObject);
var
  ser: TCustomChartSeries;
begin
  ser := ChartListbox1.Series[ChartListbox1.ItemIndex];
  if ser is TLineSeries then begin
    Notebook1.PageIndex := 0;
    CbLineSerLinePenStyle.SetPenPattern(TLineSeries(ser).LinePen);
    CbLineSerLinePenstyle.Cosmetic := TLineSeries(ser).LinePen.Cosmetic;
    CbLineSerLinePenStyle.PenStyle := TLineSeries(ser).LinePen.Style;
    CbLineSerLinePenWidth.PenWidth := TLineSeries(ser).LinePen.Width;
    CbLineSerPenColor.Selected := TLineSeries(ser).LinePen.Color;
    CbLineSerPointerStyle.PointerStyle := TLineSeries(ser).Pointer.Style;
    CbLineSerPointerBorderColor.Selected := TLineSeries(ser).Pointer.Pen.Color;
    CblineSerPointerBrushColor.Selected := TLineSeries(ser).Pointer.Brush.Color;
    EdLineSerPointerSize.Value := TLineSeries(ser).Pointer.HorizSize;
  end
  else if ser is TBarSeries then begin
    Notebook1.PageIndex := 1;
    CbBarSerPenStyle.PenStyle := TBarSeries(ser).BarPen.Style;
    CbBarSerPenWidth.PenWidth := TBarseries(ser).BarPen.Width;
    CbBarSerPenColor.Selected := TBarSeries(ser).BarPen.Color;
    CbBarSerBrushColor.Selected := TBarSeries(ser).BarBrush.Color;
    CbBarSerBrushStyle.BrushStyle := TBarSeries(ser).BarBrush.Style;
  end else if ser is TAreaSeries then begin
    Notebook1.PageIndex := 2;
    CbAreaSerBrushColor.Selected := TAreaseries(ser).AreaBrush.Color;
    CbAreaSerBrushStyle.BrushStyle := TAreaSeries(ser).AreaBrush.Style;
    CbAreaSerContourStyle.PenStyle := TAreaSeries(ser).AreaContourPen.Style;
    CbAreaSerContourWidth.PenWidth := TAreaSeries(ser).AreaContourPen.Width;
    CbAreaSerContourColor.Selected := TAreaSeries(ser).AreaContourPen.Color;
    CbAreaSerLinesStyle.PenStyle := TAreaSeries(ser).AreaLinesPen.Style;
    CbAreaSerLinesWidth.PenWidth := TAreaSeries(ser).AreaLinesPen.Width;
    CbAreaSerLinesColor.Selected := TAreaSeries(ser).AreaLinesPen.Color;
  end;
  LblSeries.Caption := ser.Title;
  CbShow.Checked := ser.Active;
end;

procedure TForm1.ChartToolset1DataPointClickTool1PointClick(ATool: TChartTool;
  APoint: TPoint);
var
  ser: TChartSeries;
begin
  ser := TChartSeries(TDataPointClickTool(ATool).Series);
  if ser <> nil then begin
    ChartListbox1.ItemIndex := ChartListbox1.FindSeriesIndex(ser);
    ChartListbox1Click(nil);
  end;
end;

procedure TForm1.EdLineSerPointerSizeChange(Sender: TObject);
var
  ser: TLineSeries;
begin
  ser := ChartListbox1.Series[ChartListbox1.ItemIndex] as TLineSeries;
  ser.Pointer.HorizSize := EdLineSerPointerSize.Value;
  ser.Pointer.VertSize := EdLineSerPointerSize.Value;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  ChartListbox1.ItemIndex := 0;
  ChartListbox1Click(nil);
end;

end.

