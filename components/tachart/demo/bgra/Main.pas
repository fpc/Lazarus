unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, ExtCtrls, Spin, StdCtrls, SysUtils, FileUtil, Forms,
  Controls, Graphics, Dialogs,
  TAGraph, TASeries, TASources, TAAnimatedSource, TACustomSource,
  BGRASliceScaling;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnStartStop: TButton;
    cbAntialiasing: TCheckBox;
    cbPie: TCheckBox;
    chSimple: TChart;
    chSimpleAreaSeries1: TAreaSeries;
    chSimpleBarSeries1: TBarSeries;
    chSimpleLineSeries1: TLineSeries;
    chSimplePieSeries1: TPieSeries;
    chSliceScaling: TChart;
    chSliceScalingBarSeries1: TBarSeries;
    Image1: TImage;
    lblSkipped: TLabel;
    ListChartSource1: TListChartSource;
    PageControl1: TPageControl;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    RandomChartSource1: TRandomChartSource;
    rgMethod: TRadioGroup;
    Splitter1: TSplitter;
    tsSimple: TTabSheet;
    tsSliceScaling: TTabSheet;
    procedure btnStartStopClick(Sender: TObject);
    procedure cbAntialiasingChange(Sender: TObject);
    procedure cbPieChange(Sender: TObject);
    procedure chSimpleAfterPaint(ASender: TChart);
    procedure chSliceScalingBarSeries1BeforeDrawBar(ASender: TBarSeries;
      ACanvas: TCanvas; const ARect: TRect; APointIndex, AStackIndex: Integer;
      var ADoDefaultDrawing: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure rgMethodClick(Sender: TObject);
  private
    FAnimatedSource: TCustomAnimatedChartSource;
    procedure OnGetItem(
      ASource: TCustomAnimatedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
    procedure OnStop(ASource: TCustomAnimatedChartSource);
  public
    sliceScaling: TBGRASliceScaling;
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

uses
  Math, BGRABitmap, BGRABitmapTypes,
  TAChartUtils, TADrawerBGRA, TADrawerCanvas, TADrawUtils, TAGeometry;

{ TForm1 }

procedure TForm1.btnStartStopClick(Sender: TObject);
begin
  if FAnimatedSource.IsAnimating then
    FAnimatedSource.Stop
  else
    FAnimatedSource.Start;
end;

procedure TForm1.cbAntialiasingChange(Sender: TObject);
begin
  if cbAntialiasing.Checked then
    chSimple.AntialiasingMode := amOn
  else
    chSimple.AntialiasingMode := amOff;
end;

procedure TForm1.cbPieChange(Sender: TObject);
begin
  chSimplePieSeries1.Active := cbPie.Checked;
end;

procedure TForm1.chSimpleAfterPaint(ASender: TChart);
begin
  Unused(ASender);
  PaintBox1.Invalidate;
end;

procedure TForm1.chSliceScalingBarSeries1BeforeDrawBar(ASender: TBarSeries;
  ACanvas: TCanvas; const ARect: TRect; APointIndex, AStackIndex: Integer;
  var ADoDefaultDrawing: Boolean);
var
  temp: TBGRABitmap;
  sz: TPoint;
begin
  sz := ARect.BottomRight - ARect.TopLeft;
  temp := TBGRABitmap.Create(
    sliceScaling.BitmapWidth, Round(sliceScaling.BitmapWidth * sz.Y / sz.X));
  try
    sliceScaling.Draw(temp, 0, 0, temp.Width, temp.Height);
    temp.Draw(ACanvas, ARect);
  finally
    temp.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FAnimatedSource := TCustomAnimatedChartSource.Create(Self);
  FAnimatedSource.Origin := ListChartSource1;
  FAnimatedSource.AnimationInterval := 30;
  FAnimatedSource.AnimationTime := 1000;
  FAnimatedSource.OnGetItem := @OnGetItem;
  FAnimatedSource.OnStop := @OnStop;
  chSliceScalingBarSeries1.Source := FAnimatedSource;

  sliceScaling := TBGRASliceScaling.Create(Image1.Picture.Bitmap, 70, 0, 35, 0);
  sliceScaling.AutodetectRepeat;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  sliceScaling.Free;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
var
  bmp: TBGRABitmap;
  id: IChartDrawer;
  rp: TChartRenderingParams;
begin
  bmp := TBGRABitmap.Create(PaintBox1.Width, PaintBox1.Height);
  chSimple.DisableRedrawing;
  try
    chSimple.Title.Text.Text := 'BGRABitmap';
    id := TBGRABitmapDrawer.Create(bmp);
    id.DoGetFontOrientation := @CanvasGetFontOrientationFunc;
    rp := chSimple.RenderingParams;
    chSimple.Draw(id, Rect(0, 0, PaintBox1.Width, PaintBox1.Height));
    chSimple.RenderingParams := rp;
    bmp.Draw(PaintBox1.Canvas, 0, 0);
    chSimple.Title.Text.Text := 'Standard';
  finally
    chSimple.EnableRedrawing;
    bmp.Free;
  end;
end;

procedure TForm1.OnGetItem(
  ASource: TCustomAnimatedChartSource;
  AIndex: Integer; var AItem: TChartDataItem);
begin
  case rgMethod.ItemIndex of
  0: AItem.Y *= ASource.Progress;
  1:
    if ASource.Count * ASource.Progress < AIndex then
      AItem.Y := 0;
  2:
    case Sign(Trunc(ASource.Count * ASource.Progress) - AIndex) of
      0: AItem.Y *= Frac(ASource.Count * ASource.Progress);
      -1: AItem.Y := 0;
    end;
  end;
end;

procedure TForm1.OnStop(ASource: TCustomAnimatedChartSource);
begin
  lblSkipped.Caption := Format('Skipped frames: %d', [ASource.SkippedFramesCount]);
end;

procedure TForm1.rgMethodClick(Sender: TObject);
begin
  FAnimatedSource.Start;
end;

end.

