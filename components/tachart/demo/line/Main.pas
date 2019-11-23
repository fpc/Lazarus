unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, ExtCtrls, Spin, StdCtrls, SysUtils, FileUtil, Forms,
  Controls, Graphics, Dialogs, TAGraph, TASeries, TASources, TATools,
  TATransformations, TACustomSeries, Types, TADrawUtils, TATypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnAddSeries: TButton;
    btnRefresh: TButton;
    catOscillatorLinearAxisTransform1: TLinearAxisTransform;
    cb3D: TCheckBox;
    cbLineType: TComboBox;
    cbColorEach: TComboBox;
    cbRotated: TCheckBox;
    cbSorted: TCheckBox;
    catOscillator: TChartAxisTransformations;
    Chart_CustomDrawPointer: TChart;
    ChartGetPointerStyleEvent: TChart;
    Chart_ColorEach: TChart;
    lsColorEach: TLineSeries;
    lsGetPointerStyle: TLineSeries;
    pnlColorEach: TPanel;
    PointerImage: TImage;
    lsCustomDrawPointer: TLineSeries;
    cbBitmapPointer: TCheckBox;
    cbDrawEveryNthPointer: TCheckBox;
    chOscillator: TChart;
    chOscillatorLineSeries1: TLineSeries;
    chPointers: TChart;
    chFast: TChart;
    chFastConstantLine1: TConstantLine;
    chFastLineSeries1: TLineSeries;
    ChartToolset1: TChartToolset;
    ChartToolset1DataPointDragTool1: TDataPointDragTool;
    ChartToolset1PanDragTool1: TPanDragTool;
    ChartToolset1ZoomDragTool1: TZoomDragTool;
    edTime: TEdit;
    lblPointerSize: TLabel;
    lblPointsCount: TLabel;
    lcsOscillator: TListChartSource;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    pnlPointers: TPanel;
    RandomChartSource1: TRandomChartSource;
    sePointerSize: TSpinEdit;
    edEveryNth: TSpinEdit;
    tsColorEach: TTabSheet;
    tsCustomDrawPointer: TTabSheet;
    tsGetPointerStyle: TTabSheet;
    tsOwnerDrawnPointer: TTabSheet;
    timOscilloscope: TTimer;
    tsOscilloscope: TTabSheet;
    tsPointers: TTabSheet;
    tsFast: TTabSheet;
    procedure btnAddSeriesClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure cb3DChange(Sender: TObject);
    procedure cbBitmapPointerChange(Sender: TObject);
    procedure cbColorEachChange(Sender: TObject);
    procedure cbLineTypeChange(Sender: TObject);
    procedure cbRotatedChange(Sender: TObject);
    procedure cbSortedChange(Sender: TObject);
    procedure edEveryNthChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lsGetPointerStyleGetPointerStyle(ASender: TChartSeries;
      AValueIndex: Integer; var AStyle: TSeriesPointerStyle);
    procedure lsCustomDrawPointerCustomDrawPointer(ASender: TChartSeries;
      ADrawer: IChartDrawer; AIndex: Integer; ACenter: TPoint);
    procedure PageControl1Change(Sender: TObject);
    procedure sePointerSizeChange(Sender: TObject);
    procedure timOscilloscopeTimer(Sender: TObject);
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

uses
  LCLIntf, IntfGraphics, TAChartUtils, TAEnumerators;

type
  TLineSeriesEnum =
    specialize TFilteredChartSeriesEnumeratorFactory<TLineSeries>;

{ TForm1 }

procedure TForm1.btnAddSeriesClick(Sender: TObject);
const
  POINTS_PER_SERIE = 50000;
var
  s: TLineSeries;
  i, j: Integer;
begin
  for i := 1 to 10 do begin
    s := TLineSeries.Create(chFast);
    s.SeriesColor := clRed;
    if cb3D.Checked then
      s.Depth := 15 - s.Depth;
    s.AxisIndexY := Ord(cbRotated.Checked);
    s.AxisIndexX := 1 - s.AxisIndexY;
    s.ListSource.Sorted := cbSorted.Checked;
    for j := 1 to POINTS_PER_SERIE do
      s.AddXY(j, Random * 5 + chFast.SeriesCount * 10);
    chFast.AddSeries(s);
  end;
  lblPointsCount.Caption :=
    Format('Points: %.0n', [chFast.SeriesCount * POINTS_PER_SERIE * 1.0]);
end;

procedure TForm1.btnRefreshClick(Sender: TObject);
var
  t: TDateTime;
begin
  t := Now;
  chFast.Refresh;
  edTime.Text := FormatDateTime('s.zzz', Now - t) + ' s';
end;

procedure TForm1.cb3DChange(Sender: TObject);
var
  ls: TLineSeries;
begin
  for ls in TLineSeriesEnum.Create(chFast) do
    ls.Depth := 15 - ls.Depth;
end;

procedure TForm1.cbBitmapPointerChange(Sender: TObject);
begin
  if cbBitmapPointer.Checked or cbDrawEveryNthPointer.Checked then
    lsCustomDrawPointer.OnCustomDrawPointer := @lsCustomDrawPointerCustomDrawPointer
  else
    lsCustomDrawPointer.OnCustomDrawPointer := nil;
  Chart_CustomDrawPointer.Invalidate;
end;

procedure TForm1.cbColorEachChange(Sender: TObject);
begin
  lsColorEach.ColorEach := TColorEachMode(cbColorEach.ItemIndex);
end;

procedure TForm1.cbLineTypeChange(Sender: TObject);
var
  ls: TLineSeries;
begin
  for ls in TLineSeriesEnum.Create(chFast) do
    ls.LineType := TLineType(cbLineType.ItemIndex);
end;

procedure TForm1.cbRotatedChange(Sender: TObject);
var
  ls: TLineSeries;
begin
  for ls in TLineSeriesEnum.Create(chFast) do begin
    ls.AxisIndexY := Ord(cbRotated.Checked);
    ls.AxisIndexX := 1 - ls.AxisIndexY;
  end;
  chFastConstantLine1.LineStyle := TLineStyle(cbRotated.Checked);
  chFastConstantLine1.AxisIndex := Ord(cbRotated.Checked);
end;

procedure TForm1.cbSortedChange(Sender: TObject);
var
  ls: TLineSeries;
begin
  for ls in TLineSeriesEnum.Create(chFast) do
    if ls.Source is TListChartSource then
      ls.ListSource.Sorted := cbSorted.Checked;
end;

procedure TForm1.edEveryNthChange(Sender: TObject);
begin
  Chart_CustomDrawPointer.Invalidate;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  st: TSeriesPointerStyle;
  ls: TLineSeries;
  s: ShortString;
  i: Integer;
  x, y: Double;
  clr: TColor;
begin
  lblPointsCount.Caption := '';

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
    chPointers.AddSeries(ls);
  end;

  // Populate series for the "ColorEach" demo
  for i:=0 to 20 do begin
    x := i/2;
    y := sin(x);
    clr := InterpolateRGB(clRed, clYellow, (y+1)/2);
    lsColorEach.AddXY(x, y, '', clr);
  end;
  cbColorEach.ItemIndex := ord(lsColorEach.ColorEach);
end;

procedure TForm1.lsGetPointerStyleGetPointerStyle(ASender: TChartSeries;
  AValueIndex: Integer; var AStyle: TSeriesPointerStyle);
begin
  AStyle := TSeriesPointerStyle(AValueIndex mod (ord(High(TSeriespointerStyle))+1));
end;

procedure TForm1.lsCustomDrawPointerCustomDrawPointer(ASender: TChartSeries;
  ADrawer: IChartDrawer; AIndex: Integer; ACenter: TPoint);

  procedure DoDrawPointer;
  var
    img: TLazIntfImage;
    bmp: TBitmap;
    ser: TLineSeries;
  begin
    if cbBitmapPointer.Checked then begin
      img := TLazIntfImage.Create(0,0);
      try
        bmp := PointerImage.Picture.Bitmap;
        img.LoadFromBitmap(bmp.Handle, bmp.MaskHandle);
        ADrawer.PutImage(
          ACenter.X - bmp.Width div 2,
          ACenter.Y - bmp.Height div 2,
          img
        )
      finally
        img.Free;
      end;
    end else begin
      ser := TLineseries(ASender);
      ser.Pointer.Draw(ADrawer, ACenter, ser.Pointer.Brush.Color);
    end;
  end;

begin
  if not cbDrawEveryNthPointer.Checked or (AIndex mod edEveryNth.Value = 0) then
    DoDrawPointer;
end;

procedure TForm1.PageControl1Change(Sender: TObject);
begin
  timOscilloscope.Enabled := PageControl1.ActivePage = tsOscilloscope;
end;

procedure TForm1.sePointerSizeChange(Sender: TObject);
var
  ls: TLineSeries;
begin
  for ls in TLineSeriesEnum.Create(chPointers) do
    with ls.Pointer do begin
      HorizSize := sePointerSize.Value;
      VertSize := sePointerSize.Value;
    end;
end;

procedure TForm1.timOscilloscopeTimer(Sender: TObject);
var
  rp: TChartRenderingParams;
begin
  rp := chOscillator.RenderingParams;
  with chOscillatorLineSeries1 do begin
    Add(Sin(GetXMax / 20) + Random - 0.5);
    if Count > 20 then
      ListSource.Delete(0);
    // Allow to zoom into various parts of the chart
    // while preserving "oscilloscope" behaviour.
    catOscillatorLinearAxisTransform1.Offset := -GetXMin;
  end;
  // Transformation change resets logical extent.
  // We know the old extent is safe to keep, so restore it.
  chOscillator.RenderingParams := rp;
end;

end.

