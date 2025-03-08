unit frmCustomDrawPointer;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes,
  Forms, IntfGraphics, Graphics, Controls, StdCtrls, ExtCtrls, Spin,
  TAGraph, TASources, TADrawUtils, TACustomSeries, TASeries;

type
  TCustomDrawPointerFrame = class(TFrame)
    cbBitmapPointer: TCheckBox;
    cbDrawEveryNthPointer: TCheckBox;
    Chart: TChart;
    edEveryNth: TSpinEdit;
    LineSeries: TLineSeries;
    Panel2: TPanel;
    PointerImage: TImage;
    RandomChartSource: TRandomChartSource;
    procedure cbBitmapPointerChange(Sender: TObject);
    procedure cbDrawEveryNthPointerChange(Sender: TObject);
    procedure edEveryNthChange(Sender: TObject);
    procedure LineSeriesCustomDrawPointer(ASender: TChartSeries;
      ADrawer: IChartDrawer; AIndex: Integer; ACenter: TPoint);
  private

  public

  end;

implementation

{$R *.lfm}

procedure TCustomDrawPointerFrame.cbBitmapPointerChange(Sender: TObject);
begin
  if cbBitmapPointer.Checked or cbDrawEveryNthPointer.Checked then
    LineSeries.OnCustomDrawPointer := @LineSeriesCustomDrawPointer
  else
    LineSeries.OnCustomDrawPointer := nil;
  Chart.Invalidate;
end;

procedure TCustomDrawPointerFrame.cbDrawEveryNthPointerChange(Sender: TObject);
begin
  if cbBitmapPointer.Checked or cbDrawEveryNthPointer.Checked then
    LineSeries.OnCustomDrawPointer := @LineSeriesCustomDrawPointer
  else
    LineSeries.OnCustomDrawPointer := nil;
  Chart.Invalidate;
end;

procedure TCustomDrawPointerFrame.edEveryNthChange(Sender: TObject);
begin
  Chart.Invalidate;
end;

procedure TCustomDrawPointerFrame.LineSeriesCustomDrawPointer(ASender:
  TChartSeries; ADrawer: IChartDrawer; AIndex: Integer; ACenter: TPoint);

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

end.

