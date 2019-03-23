unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, ExtCtrls, StdCtrls, SysUtils, FileUtil, Forms, Controls,
  Graphics, Dialogs, TAGraph, TAGUIConnectorBGRA, TASeries, TASources,
  TAAnimatedSource, TACustomSource, BGRASliceScaling, Types, TADrawUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnStartStop: TButton;
    cbAntialiasing: TCheckBox;
    cbPie: TCheckBox;
    ChartGUIConnectorBGRA1: TChartGUIConnectorBGRA;
    cbUseConnector: TCheckBox;
    chSimple: TChart;
    chSimpleAreaSeries1: TAreaSeries;
    chSimpleBarSeries1: TBarSeries;
    chSimpleLineSeries1: TLineSeries;
    chSimplePieSeries1: TPieSeries;
    chBarEffects: TChart;
    chBarEffectsBarSeries1: TBarSeries;
    Image1: TImage;
    ListChartSource1: TListChartSource;
    PageControl1: TPageControl;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    RandomChartSource1: TRandomChartSource;
    rgAnimation: TRadioGroup;
    rgStyle: TRadioGroup;
    Splitter1: TSplitter;
    tsSimple: TTabSheet;
    tsBarEffects: TTabSheet;
    procedure btnStartStopClick(Sender: TObject);
    procedure cbAntialiasingChange(Sender: TObject);
    procedure cbPieChange(Sender: TObject);
    procedure cbUseConnectorChange(Sender: TObject);
    procedure chBarEffectsBarSeries1CustomDrawBar(ASeries: TBarSeries;
      ADrawer: IChartDrawer; const ARect: TREct; APointIndex,
      AStackIndex: Integer);
    procedure chSimpleAfterPaint(ASender: TChart);
    procedure chBarEffectsBarSeries1BeforeDrawBar(ASender: TBarSeries;
      ACanvas: TCanvas; const ARect: TRect; APointIndex, AStackIndex: Integer;
      var ADoDefaultDrawing: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure rgAnimationClick(Sender: TObject);
    procedure rgStyleClick(Sender: TObject);
  private
    FAnimatedSource: TCustomAnimatedChartSource;
    FSliceScaling: TBGRASliceScaling;
    procedure OnGetItem(
      ASource: TCustomAnimatedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

uses
  Math, BGRABitmap, BGRABitmapTypes, BGRAGradients,
  TABGRAUtils, TAChartUtils, TADrawerBGRA, TADrawerCanvas,
  TAGeometry;

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

procedure TForm1.cbUseConnectorChange(Sender: TObject);
begin
  if cbUseConnector.Checked then
    chSimple.GUIConnector := ChartGUIConnectorBGRA1
  else
    chSimple.GUIConnector := nil;
end;

procedure TForm1.chSimpleAfterPaint(ASender: TChart);
begin
  Unused(ASender);
  PaintBox1.Invalidate;
end;

procedure TForm1.chBarEffectsBarSeries1BeforeDrawBar(ASender: TBarSeries;
  ACanvas: TCanvas; const ARect: TRect; APointIndex, AStackIndex: Integer;
  var ADoDefaultDrawing: Boolean);
var
  temp, stretched: TBGRABitmap;
  sz: TPoint;
  lightPos: TPoint;
begin
  Unused(ASender);
  Unused(APointIndex, AStackIndex);
  ADoDefaultDrawing := false;
  sz := ARect.BottomRight - ARect.TopLeft;
  case rgStyle.ItemIndex of
    0: begin
      temp := TBGRABitmap.Create(
        FSliceScaling.BitmapWidth,
        Round(FSliceScaling.BitmapWidth * sz.Y / sz.X));
      stretched := nil;
      try
        FSliceScaling.Draw(temp, 0, 0, temp.Width, temp.Height);
        temp.ResampleFilter := rfLinear;
        stretched := temp.Resample(sz.x, sz.Y, rmFineResample) as TBGRABitmap;
        stretched.Draw(ACanvas, ARect, False);
      finally
        temp.Free;
        stretched.Free;
      end;
    end;
    1: DrawChocolateBar(ASender, ACanvas, ARect, APointIndex, false);
    2: DrawPhong3DBar(ASender, ACanvas, ARect, APointIndex);
  end;
end;

procedure TForm1.chBarEffectsBarSeries1CustomDrawBar(ASeries: TBarSeries;
  ADrawer: IChartDrawer; const ARect: TRect; APointIndex, AStackIndex: Integer);
var
  temp, stretched: TBGRABitmap;
  sz: TPoint;
  ic: IChartTCanvasDrawer;
begin
  Unused(ASeries);
  Unused(APointIndex, AStackIndex);

  if not Supports(ADrawer, IChartTCanvasDrawer, ic) then
    raise Exception.Create('This program require a canvas drawer.');

  sz := ARect.BottomRight - ARect.TopLeft;
  case rgStyle.ItemIndex of
    0: begin
      temp := TBGRABitmap.Create(
        FSliceScaling.BitmapWidth,
        Round(FSliceScaling.BitmapWidth * sz.Y / sz.X));
      stretched := nil;
      try
        FSliceScaling.Draw(temp, 0, 0, temp.Width, temp.Height);
        temp.ResampleFilter := rfLinear;
        stretched := temp.Resample(sz.x, sz.Y, rmFineResample) as TBGRABitmap;
        stretched.Draw(ic.Canvas, ARect, False);
      finally
        temp.Free;
        stretched.Free;
      end;
    end;
    1: DrawChocolateBar(ASeries, ic.Canvas, ARect, APointIndex, false);
    2: DrawPhong3DBar(ASeries, ic.Canvas, ARect, APointIndex);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FAnimatedSource := TCustomAnimatedChartSource.Create(Self);
  FAnimatedSource.Origin := ListChartSource1;
  FAnimatedSource.AnimationInterval := 30;
  FAnimatedSource.AnimationTime := 1000;
  FAnimatedSource.OnGetItem := @OnGetItem;

  chBarEffectsBarSeries1.Source := FAnimatedSource;
  chBarEffects.BackColor:= BGRAToColor(CSSDarkSlateBlue);
  chSimple.BackColor:= BGRAToColor(CSSYellowGreen);
  chSimple.Color:= BGRAToColor(CSSYellowGreen);
  chSimple.BackColor := BGRAToColor(CSSBeige);

  FSliceScaling := TBGRASliceScaling.Create(Image1.Picture.Bitmap, 70, 0, 35, 0);
  FSliceScaling.AutodetectRepeat;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FSliceScaling.Free;
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
  case rgAnimation.ItemIndex of
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

procedure TForm1.rgAnimationClick(Sender: TObject);
begin
  FAnimatedSource.Start;
end;

procedure TForm1.rgStyleClick(Sender: TObject);
var
  d: Integer;
begin
  d := IfThen(rgStyle.ItemIndex = 2, 10, 0);
  chBarEffects.Depth := d;
  chBarEffectsBarSeries1.Depth := d;
  chBarEffectsBarSeries1.ZPosition := d;
  chBarEffects.BottomAxis.ZPosition := d;
  FAnimatedSource.Start;
end;

end.

