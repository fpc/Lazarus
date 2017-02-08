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
    CbLineSerLinePenColor: TColorBox;
    ChartToolset1: TChartToolset;
    ChartToolset1DataPointClickTool1: TDataPointClickTool;
    CbShowPoints: TCheckBox;
    GbAreaSerBrush: TGroupBox;
    GbAreaSerContour: TGroupBox;
    GbAreaSerLines: TGroupBox;
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
    procedure AreaBrushChange(Sender: TObject);
    procedure AreaContourChange(Sender: TObject);
    procedure AreaLinesChange(Sender: TObject);
    procedure BarBrushChange(Sender: TObject);
    procedure BarPenChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LinePenChange(Sender: TObject);
    procedure LinePointerChange(Sender: TObject);
    procedure CbShowChange(Sender: TObject);
    procedure ChartListbox1Click(Sender: TObject);
    procedure ChartToolset1DataPointClickTool1PointClick(ATool: TChartTool;
      APoint: TPoint);
    procedure FormShow(Sender: TObject);
  private
    FLockChanges: Integer;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  FPCanvas, TAChartUtils, TACustomSeries;


{ TAreaSeries }

procedure TForm1.AreaBrushChange(Sender: TObject);
var
  ser: TAreaSeries;
begin
  if FLockChanges > 0 then
    exit;
  ser := ChartListbox1.Series[ChartListbox1.ItemIndex] as TAreaSeries;
  ser.AreaBrush.Color := CbAreaSerBrushColor.Selected;
  ser.AreaBrush.Style := CbAreaserBrushStyle.BrushStyle;
  if CbAreaSerBrushStyle.BrushStyle in [bsImage, bsPattern] then
    // Must be AFTER assigning brush style because that sets Brush.Bitmap to nil
    ser.AreaBrush.Bitmap := CbAreaSerBrushStyle.BrushBitmap;
end;

procedure TForm1.AreaContourChange(Sender: TObject);
var
  ser: TAreaSeries;
begin
  if FLockChanges > 0 then
    exit;
  ser := ChartListbox1.Series[ChartListbox1.ItemIndex] as TAreaSeries;
  ser.AreaContourPen.Color := CbAreaSerContourColor.Selected;
  ser.AreaContourPen.Width := CbAreaSerContourWidth.PenWidth;
  ser.AreaContourPen.Style := CbAreaSerContourStyle.PenStyle;
  if ser.AreaContourPen.Style = psPattern then
    CbAreaSerContourStyle.SetPenPattern(ser.AreaContourPen);
end;

procedure TForm1.AreaLinesChange(Sender: TObject);
var
  ser: TAreaSeries;
begin
  if FLockChanges > 0 then
    exit;
  ser := ChartListbox1.Series[ChartListbox1.ItemIndex] as TAreaSeries;
  ser.AreaLinesPen.Color := CbAreaSerLinesColor.Selected;
  ser := ChartListbox1.Series[ChartListbox1.ItemIndex] as TAreaSeries;
  ser.AreaLinesPen.Width := CbAreaSerLinesWidth.PenWidth;
  ser.AreaLinesPen.Style := CbAreaSerLinesStyle.PenStyle;
  if ser.AreaLinesPen.Style = psPattern then
    CbAreaSerLinesStyle.SetPenPattern(ser.AreaLinesPen);
end;


{ TBarSeries }

procedure TForm1.BarBrushChange(Sender: TObject);
var
  ser: TbarSeries;
begin
  if FLockChanges > 0 then
    exit;
  ser := ChartListbox1.Series[ChartListbox1.ItemIndex] as TBarSeries;
  ser.BarBrush.Color := CbBarSerBrushColor.Selected;
  ser.BarBrush.Style := CbBarSerBrushStyle.BrushStyle;
  if CbBarSerBrushStyle.BrushStyle in [bsImage, bsPattern] then
    // Must be AFTER assigning brush style
    ser.BarBrush.Bitmap := CbBarSerBrushStyle.BrushBitmap;
end;

procedure TForm1.BarPenChange(Sender: TObject);
var
  ser: TBarSeries;
begin
  if FLockChanges > 0 then
    exit;
  ser := ChartListbox1.Series[ChartListbox1.ItemIndex] as TBarSeries;
  ser.BarPen.Color := CbBarSerPenColor.Selected;
  ser.BarPen.Width := CbBarSerPenWidth.PenWidth;
  ser.BarPen.Style := CbBarSerPenStyle.PenStyle;
  if ser.BarPen.Style = psPattern then
    CbBarSerPenStyle.SetPenPattern(ser.BarPen);
end;

{ Line series }

procedure TForm1.LinePenChange(Sender: TObject);
var
  ser: TLineSeries;
begin
  if FLockChanges > 0 then
    exit;
  ser := ChartListbox1.Series[ChartListbox1.ItemIndex] as TLineSeries;
  ser.LinePen.Color := CbLineSerLinePenColor.Selected;
  ser.LinePen.Width := CbLineSerLinePenWidth.PenWidth;
  ser.LinePen.Style := CbLineSerLinePenStyle.PenStyle;
  if ser.LinePen.Style = psPattern then
    CbLineSerLinePenStyle.SetPenPattern(ser.LinePen);
  ser.ShowLines := cbShowLines.Checked;
end;

procedure TForm1.LinePointerChange(Sender: TObject);
var
  ser: TLineSeries;
begin
  if FLockChanges > 0 then
    exit;
  ser := ChartListbox1.Series[ChartListbox1.ItemIndex] as TLineSeries;
  ser.Pointer.Pen.Color := CbLineSerPointerBorderColor.Selected;
  ser.Pointer.Brush.Color := CbLineSerPointerBrushColor.Selected;
  ser.Pointer.Style := CbLineSerPointerStyle.PointerStyle;
  ser.Pointer.HorizSize := EdLineSerPointerSize.Value;
  ser.Pointer.VertSize := EdLineSerPointerSize.Value;
  ser.ShowPoints := CbShowPoints.Checked;
end;


{ Other events }

procedure TForm1.CbShowChange(Sender: TObject);
var
  ser: TCustomChartSeries;
begin
  ser := ChartListbox1.Series[ChartListbox1.ItemIndex];
  ser.Active := CbShow.Checked;
end;

procedure TForm1.ChartListbox1Click(Sender: TObject);
var
  ser: TCustomChartSeries;
begin
  ser := ChartListbox1.Series[ChartListbox1.ItemIndex];
  if ser is TLineSeries then begin
    Notebook1.PageIndex := 0;
    CbShowLines.Checked := TLineSeries(ser).ShowLines;
    CbLineSerLinePenStyle.SetPenPattern(TLineSeries(ser).LinePen);
    CbLineSerLinePenStyle.Cosmetic := TLineSeries(ser).LinePen.Cosmetic;
    CbLineSerLinePenStyle.PenStyle := TLineSeries(ser).LinePen.Style;
    CbLineSerLinePenWidth.PenWidth := TLineSeries(ser).LinePen.Width;
    CbLineSerLinePenColor.Selected := TLineSeries(ser).LinePen.Color;

    inc(FLockChanges);
    CbShowPoints.Checked := TLineSeries(ser).ShowPoints;
    EdLineSerPointerSize.Value := TLineSeries(ser).Pointer.HorizSize;
    CbLineSerPointerStyle.PointerStyle := TLineSeries(ser).Pointer.Style;
    CbLineSerPointerBorderColor.Selected := TLineSeries(ser).Pointer.Pen.Color;
    CblineSerPointerBrushColor.Selected := TLineSeries(ser).Pointer.Brush.Color;
    dec(FLockChanges);  // Not clear why this is necessary - should work without it
 //   LinePointerChange(nil);
  end
  else
  if ser is TBarSeries then begin
    Notebook1.PageIndex := 1;
    CbBarSerPenStyle.PenStyle := TBarSeries(ser).BarPen.Style;
    CbBarSerPenWidth.PenWidth := TBarseries(ser).BarPen.Width;
    CbBarSerPenColor.Selected := TBarSeries(ser).BarPen.Color;
    CbBarSerBrushColor.Selected := TBarSeries(ser).BarBrush.Color;
    CbBarSerBrushStyle.BrushStyle := TBarSeries(ser).BarBrush.Style;
  end
  else if ser is TAreaSeries then begin
    Notebook1.PageIndex := 2;
    CbAreaSerBrushColor.Selected := TAreaSeries(ser).AreaBrush.Color;
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
  Unused(APoint);
  ser := TChartSeries(TDataPointClickTool(ATool).Series);
  if ser <> nil then begin
    ChartListbox1.ItemIndex := ChartListbox1.FindSeriesIndex(ser);
    ChartListbox1Click(nil);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to ComponentCount - 1 do
    if Components[i] is TCustomCombobox then
      TCustomCombobox(Components[i]).DropDownCount := DEFAULT_DROPDOWN_COUNT;
end;

procedure TForm1.FormShow(Sender: TObject);
const
  DEFAULT_PATTERN = '2|1|1|1|1|1|1|1';
var
  bmp: TBitmap;
begin
  // Prepare the application's icon as a brush fill picture
  bmp := TBitmap.Create;
  try
    bmp.SetSize(Application.Icon.Width, Application.Icon.Height);
    bmp.Canvas.FillRect(0, 0, bmp.Width, bmp.Height);
    bmp.Transparent := true;
    bmp.canvas.Draw(0, 0, Application.Icon);
    CbBarSerBrushStyle.BrushBitmap.Assign(bmp);
    CbAreaSerBrushStyle.BrushBitmap.Assign(bmp);
  finally
    bmp.Free;
  end;
  {
  bmp := TBitmap.Create;
  try
    bmp.SetSize(2, 2);
    bmp.Canvas.Pixels[0, 0] := clWhite;
    bmp.Canvas.Pixels[1, 0] := clBlack;
    bmp.Canvas.Pixels[0, 1] := clBlack;
    bmp.Canvas.Pixels[1, 1] := clWhite;
  finally
    CbBarSerBrushStyle.BrushBitmap.Assign(bmp);
    CbAreaserBrushStyle.BrushBitmap.Assign(bmp);
  end;
  }

  // Prepare user-defined line pattern
  CbLineSerLinePenStyle.PenPattern := DEFAULT_PATTERN;
  CbBarSerPenStyle.PenPattern := DEFAULT_PATTERN;
  CbAreaSerLinesStyle.PenPattern := DEFAULT_PATTERN;
  CbAreaSerContourStyle.PenPattern := DEFAULT_PATTERN;

  ChartListbox1.ItemIndex := 0;
  ChartListbox1Click(nil);
end;

end.

