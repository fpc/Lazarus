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
    procedure chSimpleAfterPaint(ASender: TChart);
    procedure chBarEffectsBarSeries1BeforeDrawBar(ASender: TBarSeries;
      ACanvas: TCanvas; const ARect: TRect; APointIndex, AStackIndex: Integer;
      var ADoDefaultDrawing: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure rgAnimationClick(Sender: TObject);
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
  TAChartUtils, TADrawerBGRA, TADrawerCanvas, TADrawUtils, TAGeometry;

function CreateChocolateTexture(ATx, ATy: integer): TBGRABitmap;
var
  square, map: TBGRABitmap;
  phong: TPhongShading;
  margin: Integer;
begin
  margin := ATx div 20;
  map := TBGRABitmap.Create(ATx, ATy, BGRABlack);
  try
    square := CreateRectangleMap(ATx - 2 * margin, ATy - 2 * margin, ATx div 8);
    try
      map.PutImage(margin, margin, square, dmDrawWithTransparency);
      BGRAReplace(map, map.FilterBlurRadial(ATx div 40, rbFast));
    finally
      square.free;
    end;

    Result := TBGRABitmap.Create(ATx, ATy);
    phong := TPhongShading.Create;
    try
      phong.LightSourceDistanceFactor := 0;
      phong.LightDestFactor := 0;
      phong.LightSourceIntensity := 200;
      phong.AmbientFactor := 0.5;
      phong.LightPosition := Point(-50, -100);
      phong.LightPositionZ := 80;
      phong.Draw(Result, map, 20, 0, 0, BGRA(86, 41, 38));
    finally
      phong.Free;
    end;
  finally
    map.Free;
  end;
end;

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

procedure TForm1.chBarEffectsBarSeries1BeforeDrawBar(ASender: TBarSeries;
  ACanvas: TCanvas; const ARect: TRect; APointIndex, AStackIndex: Integer;
  var ADoDefaultDrawing: Boolean);
var
  temp, chocolate: TBGRABitmap;
  sz: TPoint;
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
      try
        FSliceScaling.Draw(temp, 0, 0, temp.Width, temp.Height);
        temp.Draw(ACanvas, ARect);
      finally
        temp.Free;
      end;
    end;
    1: begin
      chocolate := CreateChocolateTexture(sz.X, sz.X);
      temp := TBGRABitmap.Create(sz.X, sz.Y);
      try
        temp.FillRect(0, 0, sz.X, sz.Y, chocolate, dmSet);
        temp.Draw(ACanvas, ARect);
      finally
        temp.Free;
        chocolate.Free;
      end;
    end;
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

end.

