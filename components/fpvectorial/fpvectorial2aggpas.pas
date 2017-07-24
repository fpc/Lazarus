{
Implements support for drawing to the LCL TCanvas via AggPas

License: The same modified LGPL as the Free Pascal RTL
         See the file COPYING.modifiedLGPL for more details

AUTHORS: Felipe Monteiro de Carvalho
}
unit fpvectorial2aggpas;

{$ifdef fpc}
  {$mode objfpc}{$h+}
{$endif}

{$define USE_CANVAS_CLIP_REGION}
{.$define DEBUG_CANVAS_CLIP_REGION}

{.$define FPVECTORIAL_DEBUG_DIMENSIONS}
{.$define FPVECTORIAL_TOCANVAS_DEBUG}
{.$define FPVECTORIAL_DEBUG_BLOCKS}
{.$define FPVECTORIAL_AUTOFIT_DEBUG}
{.$define FPVECTORIAL_SUPPORT_LAZARUS_1_6}
// visual debugs
{.$define FPVECTORIAL_TOCANVAS_ELLIPSE_VISUALDEBUG}
{.$define FPVECTORIAL_RENDERINFO_VISUALDEBUG}

interface

uses
  Classes, SysUtils, Math, TypInfo, contnrs, types,
  // FCL-Image
  fpcanvas, fpimage, fpwritebmp,
  // lazutils
  laz2_dom,
  // LCL
  lazutf8, lazregions,
  Graphics, LCLIntf, LCLType, intfgraphics, graphtype, interfacebase,
  // AggPas
  agg_fpimage, Agg_LCL,
  // fpvectorial
  fpvutils, fpvectorial;

type

  { TFPVAggPasRenderer }

  TFPVAggPasRenderer = class(TvRenderer)
  private
    Bitmap: TBitmap;
    AggLCLCanvas: TAggLCLCanvas;
  public
    procedure BeginRender(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean); override;
    procedure EndRender(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean); override;
    // TPath
    procedure TPath_Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean; APath: TPath); override;
  end;

implementation


{ TFPVAggPasRenderer }

procedure TFPVAggPasRenderer.BeginRender(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean);
var
  HasFont: Boolean;
  FontFilename: String;
begin
  Bitmap := TBitmap.Create;
  AggLCLCanvas:=TAggLCLCanvas.Create;
  AggLCLCanvas.Image.PixelFormat:=afpimRGBA32;
  AggLCLCanvas.Image.SetSize(2000, 2000);

  {$IFDEF LCLGtk2}
  {HasFont:=true;
  FontFilename:=SetDirSeparators('../../verdana.ttf');
  if not FileExistsUTF8(FontFilename) then begin
    ShowMessage('file not found: '+FontFilename+' CurDir='+GetCurrentDirUTF8);
    HasFont:=false;
  end; }
  // paint to agg canvas
  {with AggLCLCanvas do begin
    if HasFont then begin
      Font.LoadFromFile(FontFilename);
      Font.Size:=10;
      Font.Color:=clBlack;
    end;}
  {$ELSE}
  //HasFont:=false;
  {$ENDIF}

  // solid white background
  AggLCLCanvas.Brush.Color:=clWhite;
  AggLCLCanvas.FillRect(0, 0, AggLCLCanvas.Width, AggLCLCanvas.Height);
end;

procedure TFPVAggPasRenderer.EndRender(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean);
begin
  // convert to LCL native pixel format
  Bitmap.LoadFromIntfImage(AggLCLCanvas.Image.IntfImg);
  TCanvas(ARenderInfo.Canvas).Draw(0, 0, Bitmap);

  AggLCLCanvas.Free;
  Bitmap.Free;
end;

procedure TFPVAggPasRenderer.TPath_Render(var ARenderInfo: TvRenderInfo;
  ADoDraw: Boolean; APath: TPath);
var
  ADest: TFPCustomCanvas absolute ARenderInfo.Canvas;
  ADestX: Integer absolute ARenderInfo.DestX;
  ADestY: Integer absolute ARenderInfo.DestY;
  AMulX: Double absolute ARenderInfo.MulX;
  AMulY: Double absolute ARenderInfo.MulY;
  //
  i, j, curPt: Integer;
  coordX, coordY: Integer;
  curSegment: TPathSegment;
  cur2DSegment: T2DSegment absolute curSegment;
  pts: TPointsArray;
begin
  if not ADoDraw then Exit;

  AggLCLCanvas.Pen.Style := APath.Pen.Style;
  AggLCLCanvas.Pen.Width := APath.Pen.Width;
  AggLCLCanvas.Pen.FPColor := APath.Pen.Color;
  AggLCLCanvas.Brush.Style := APath.Brush.Style;
  AggLCLCanvas.Brush.FPColor := APath.Brush.Color;
  AggLCLCanvas.Brush.AggFillEvenOdd := APath.ClipMode = vcmEvenOddRule;

  AggLCLCanvas.AggResetPath;
  APath.PrepareForSequentialReading;
  for j := 0 to APath.Len - 1 do
  begin
    curSegment := TPathSegment(APath.Next);
    case curSegment.SegmentType of
      stMoveTo:
        begin
          inc(i);
          coordX := CoordToCanvasX(cur2DSegment.X, ADestX, AMulX);
          coordY := CoordToCanvasY(cur2DSegment.Y, ADestY, AMulY);
          AggLCLCanvas.AggMoveTo(coordX, coordY);
        end;
      st2DLineWithPen, st2DLine, st3DLine:
        begin
          coordX := CoordToCanvasX(cur2DSegment.X, ADestX, AMulX);
          coordY := CoordToCanvasY(cur2DSegment.Y, ADestY, AMulY);
          if curSegment.SegmentType = st2DLineWithPen then
          begin
            AggLCLCanvas.Pen.FPColor := APath.AdjustColorToBackground(T2DSegmentWithPen(Cur2DSegment).Pen.Color, ARenderInfo);
            AggLCLCanvas.Pen.Width := T2DSegmentWithPen(cur2DSegment).Pen.Width;
            AggLCLCanvas.Pen.Style := T2DSegmentWithPen(cur2DSegment).Pen.Style;
            AggLCLCanvas.AggLineTo(coordX, coordY);
            AggLCLCanvas.Pen.Style := APath.Pen.Style;
            AggLCLCanvas.Pen.Width := APath.Pen.Width;
            AggLCLCanvas.Pen.FPColor := APath.Pen.Color;
          end
          else
            AggLCLCanvas.AggLineTo(coordX, coordY);
        end;
      st2DBezier, st3DBezier, st2DEllipticalArc:
        begin
          coordX := CoordToCanvasX(T2DSegment(curSegment.Previous).X, ADestX, AMulX);
          coordY := CoordToCanvasY(T2DSegment(curSegment.Previous).Y, ADestY, AMulY);

          SetLength(pts, 1);
          pts[0] := Point(coordX, coordY);
          curSegment.AddToPoints(ADestX, ADestY, AMulX, AMulY, pts);
          for curPt := 0 to Length(pts)-1 do
          begin
            AggLCLCanvas.AggLineTo(pts[curPt].X, pts[curPt].Y);
          end;
          AggLCLCanvas.AggMoveTo(pts[High(pts)].X, pts[High(pts)].Y);
        end;
    end;
  end;
  if APath.Len > 0 then
  begin
    AggLCLCanvas.AggClosePolygon;
    AggLCLCanvas.AggDrawPath(AGG_FillAndStroke, False);
  end;
end;

end.

