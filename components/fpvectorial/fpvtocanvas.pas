unit fpvtocanvas;

{$mode objfpc}{$H+}

interface

{$define USE_LCL_CANVAS}
{$ifdef USE_LCL_CANVAS}
  {$define USE_CANVAS_CLIP_REGION}
  {.$define DEBUG_CANVAS_CLIP_REGION}
{$endif}
{$ifndef Windows}
{.$define FPVECTORIAL_TOCANVAS_DEBUG}
{$endif}

uses
  Classes, SysUtils, Math,
  {$ifdef USE_LCL_CANVAS}
  Graphics, LCLIntf, LCLType,
  {$endif}
  fpcanvas,
  fpimage,
  fpvectorial, fpvutils;

procedure DrawFPVectorialToCanvas(ASource: TvVectorialPage;
  ADest: TFPCustomCanvas;
  ADestX: Integer = 0; ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0);
procedure DrawFPVPathToCanvas(ASource: TvVectorialPage; CurPath: TPath;
  ADest: TFPCustomCanvas;
  ADestX: Integer = 0; ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0);
procedure DrawFPVEntityToCanvas(ASource: TvVectorialPage; CurEntity: TvEntity;
  ADest: TFPCustomCanvas;
  ADestX: Integer = 0; ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0);
procedure DrawFPVTextToCanvas(ASource: TvVectorialPage; CurText: TvText;
  ADest: TFPCustomCanvas;
  ADestX: Integer = 0; ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0);

implementation

{@@
  This function draws a FPVectorial vectorial image to a TFPCustomCanvas
  descendent, such as TCanvas from the LCL.

  Be careful that by default this routine does not execute coordinate transformations,
  and that FPVectorial works with a start point in the bottom-left corner, with
  the X growing to the right and the Y growing to the top. This will result in
  an image in TFPCustomCanvas mirrored in the Y axis in relation with the document
  as seen in a PDF viewer, for example. This can be easily changed with the
  provided parameters. To have the standard view of an image viewer one could
  use this function like this:

  DrawFPVectorialToCanvas(ASource, ADest, 0, ASource.Height, 1.0, -1.0);
}
procedure DrawFPVectorialToCanvas(ASource: TvVectorialPage;
  ADest: TFPCustomCanvas;
  ADestX: Integer = 0; ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0);
var
  i: Integer;
  CurEntity: TvEntity;
begin
  {$ifdef FPVECTORIAL_TOCANVAS_DEBUG}
  WriteLn(':>DrawFPVectorialToCanvas');
  {$endif}

  for i := 0 to ASource.GetEntitiesCount - 1 do
  begin
    {$ifdef FPVECTORIAL_TOCANVAS_DEBUG}
    Write(Format('[Path] ID=%d', [i]));
    {$endif}

    CurEntity := ASource.GetEntity(i);

    if CurEntity is TPath then DrawFPVPathToCanvas(ASource, TPath(CurEntity), ADest, ADestX, ADestY, AMulX, AMulY)
    else if CurEntity is TvText then DrawFPVTextToCanvas(ASource, TvText(CurEntity), ADest, ADestX, ADestY, AMulX, AMulY)
    else DrawFPVEntityToCanvas(ASource, CurEntity, ADest, ADestX, ADestY, AMulX, AMulY);
  end;

  {$ifdef FPVECTORIAL_TOCANVAS_DEBUG}
  WriteLn(':<DrawFPVectorialToCanvas');
  {$endif}
end;

procedure DrawFPVPathToCanvas(ASource: TvVectorialPage; CurPath: TPath;
  ADest: TFPCustomCanvas;
  ADestX: Integer = 0; ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0);

  function CoordToCanvasX(ACoord: Double): Integer;
  begin
    Result := Round(ADestX + AmulX * ACoord);
  end;

  function CoordToCanvasY(ACoord: Double): Integer;
  begin
    Result := Round(ADestY + AmulY * ACoord);
  end;

var
  j, k: Integer;
  PosX, PosY: Double; // Not modified by ADestX, etc
  CoordX, CoordY: Integer;
  CurSegment: TPathSegment;
  Cur2DSegment: T2DSegment absolute CurSegment;
  Cur2DBSegment: T2DBezierSegment absolute CurSegment;
  // For bezier
  CurX, CurY: Integer; // Not modified by ADestX, etc
  CoordX2, CoordY2, CoordX3, CoordY3, CoordX4, CoordY4: Integer;
  CurveLength: Integer;
  t: Double;
  // For polygons
  Points: array of TPoint;
  // Clipping Region
  {$ifdef USE_LCL_CANVAS}
  ClipRegion, OldClipRegion: HRGN;
  ACanvas: TCanvas absolute ADest;
  {$endif}
begin
  PosX := 0;
  PosY := 0;
  ADest.Brush.Style := bsClear;

  ADest.MoveTo(ADestX, ADestY);

  // Set the path Pen and Brush options
  ADest.Pen.Style := CurPath.Pen.Style;
  ADest.Pen.Width := Round(CurPath.Pen.Width * AMulX);
  if ADest.Pen.Width < 1 then ADest.Pen.Width := 1;
  if (CurPath.Pen.Width <= 2) and (ADest.Pen.Width > 2) then ADest.Pen.Width := 2;
  if (CurPath.Pen.Width <= 5) and (ADest.Pen.Width > 5) then ADest.Pen.Width := 5;
  ADest.Pen.FPColor := CurPath.Pen.Color;
  ADest.Brush.FPColor := CurPath.Brush.Color;

  // Prepare the Clipping Region, if any
  {$ifdef USE_CANVAS_CLIP_REGION}
  if CurPath.ClipPath <> nil then
  begin
    OldClipRegion := LCLIntf.CreateEmptyRegion();
    GetClipRgn(ACanvas.Handle, OldClipRegion);
    ClipRegion := ConvertPathToRegion(CurPath.ClipPath, ADestX, ADestY, AMulX, AMulY);
    SelectClipRgn(ACanvas.Handle, ClipRegion);
    DeleteObject(ClipRegion);
    // debug info
    {$ifdef DEBUG_CANVAS_CLIP_REGION}
    ConvertPathToPoints(CurPath.ClipPath, ADestX, ADestY, AMulX, AMulY, Points);
    ACanvas.Polygon(Points);
    {$endif}
  end;
  {$endif}

  //
  // For solid paths, draw a polygon for the main internal area
  //
  if CurPath.Brush.Style <> bsClear then
  begin
    CurPath.PrepareForSequentialReading;

    {$ifdef FPVECTORIAL_TOCANVAS_DEBUG}
    Write(' Solid Path Internal Area');
    {$endif}
    ADest.Brush.Style := CurPath.Brush.Style;

    SetLength(Points, CurPath.Len);

    for j := 0 to CurPath.Len - 1 do
    begin
      //WriteLn('j = ', j);
      CurSegment := TPathSegment(CurPath.Next());

      CoordX := CoordToCanvasX(Cur2DSegment.X);
      CoordY := CoordToCanvasY(Cur2DSegment.Y);

      Points[j].X := CoordX;
      Points[j].Y := CoordY;

      {$ifdef FPVECTORIAL_TOCANVAS_DEBUG}
      Write(Format(' P%d,%d', [CoordY, CoordY]));
      {$endif}
    end;

    ADest.Polygon(Points);

    {$ifdef FPVECTORIAL_TOCANVAS_DEBUG}
    Write(' Now the details ');
    {$endif}
  end;

  //
  // For other paths, draw more carefully
  //
  CurPath.PrepareForSequentialReading;

  for j := 0 to CurPath.Len - 1 do
  begin
    //WriteLn('j = ', j);
    CurSegment := TPathSegment(CurPath.Next());

    case CurSegment.SegmentType of
    stMoveTo:
    begin
      CoordX := CoordToCanvasX(Cur2DSegment.X);
      CoordY := CoordToCanvasY(Cur2DSegment.Y);
      ADest.MoveTo(CoordX, CoordY);
      PosX := Cur2DSegment.X;
      PosY := Cur2DSegment.Y;
      {$ifdef FPVECTORIAL_TOCANVAS_DEBUG}
      Write(Format(' M%d,%d', [CoordY, CoordY]));
      {$endif}
    end;
    // This element can override temporarely the Pen
    st2DLineWithPen:
    begin
      ADest.Pen.FPColor := T2DSegmentWithPen(Cur2DSegment).Pen.Color;

      CoordX := CoordToCanvasX(PosX);
      CoordY := CoordToCanvasY(PosY);
      CoordX2 := CoordToCanvasX(Cur2DSegment.X);
      CoordY2 := CoordToCanvasY(Cur2DSegment.Y);
      ADest.Line(CoordX, CoordY, CoordX2, CoordY2);

      PosX := Cur2DSegment.X;
      PosY := Cur2DSegment.Y;

      ADest.Pen.FPColor := CurPath.Pen.Color;

      {$ifdef FPVECTORIAL_TOCANVAS_DEBUG}
      Write(Format(' L%d,%d', [CoordToCanvasX(Cur2DSegment.X), CoordToCanvasY(Cur2DSegment.Y)]));
      {$endif}
    end;
    st2DLine, st3DLine:
    begin
      CoordX := CoordToCanvasX(PosX);
      CoordY := CoordToCanvasY(PosY);
      CoordX2 := CoordToCanvasX(Cur2DSegment.X);
      CoordY2 := CoordToCanvasY(Cur2DSegment.Y);
      ADest.Line(CoordX, CoordY, CoordX2, CoordY2);
      PosX := Cur2DSegment.X;
      PosY := Cur2DSegment.Y;
      {$ifdef FPVECTORIAL_TOCANVAS_DEBUG}
      Write(Format(' L%d,%d', [CoordX, CoordY]));
      {$endif}
    end;
    { To draw a bezier we need to divide the interval in parts and make
      lines between this parts }
    st2DBezier, st3DBezier:
    begin
      CoordX := CoordToCanvasX(PosX);
      CoordY := CoordToCanvasY(PosY);
      CoordX2 := CoordToCanvasX(Cur2DBSegment.X2);
      CoordY2 := CoordToCanvasY(Cur2DBSegment.Y2);
      CoordX3 := CoordToCanvasX(Cur2DBSegment.X3);
      CoordY3 := CoordToCanvasY(Cur2DBSegment.Y3);
      CoordX4 := CoordToCanvasX(Cur2DBSegment.X);
      CoordY4 := CoordToCanvasY(Cur2DBSegment.Y);
      SetLength(Points, 0);
      AddBezierToPoints(
        Make2DPoint(CoordX, CoordY),
        Make2DPoint(CoordX2, CoordY2),
        Make2DPoint(CoordX3, CoordY3),
        Make2DPoint(CoordX4, CoordY4),
        Points
      );

      ADest.Brush.Style := CurPath.Brush.Style;
      if Length(Points) >= 3 then
        ADest.Polygon(Points);

      PosX := Cur2DSegment.X;
      PosY := Cur2DSegment.Y;

      {$ifdef FPVECTORIAL_TOCANVAS_DEBUG}
      Write(Format(' ***C%d,%d %d,%d %d,%d %d,%d',
        [CoordToCanvasX(PosX), CoordToCanvasY(PosY),
         CoordToCanvasX(Cur2DBSegment.X2), CoordToCanvasY(Cur2DBSegment.Y2),
         CoordToCanvasX(Cur2DBSegment.X3), CoordToCanvasY(Cur2DBSegment.Y3),
         CoordToCanvasX(Cur2DBSegment.X), CoordToCanvasY(Cur2DBSegment.Y)]));
      {$endif}
    end;
    end;
  end;
  {$ifdef FPVECTORIAL_TOCANVAS_DEBUG}
  WriteLn('');
  {$endif}

  // Restores the previous Clip Region
  {$ifdef USE_CANVAS_CLIP_REGION}
  if CurPath.ClipPath <> nil then
  begin
    SelectClipRgn(ACanvas.Handle, OldClipRegion); //Using OldClipRegion crashes in Qt
  end;
  {$endif}
end;

procedure DrawFPVEntityToCanvas(ASource: TvVectorialPage; CurEntity: TvEntity;
  ADest: TFPCustomCanvas;
  ADestX: Integer = 0; ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0);
begin
  if CurEntity is TvEntityWithPenAndBrush then
    TvEntityWithPenAndBrush(CurEntity).ApplyBrushToCanvas(ADest);
  if CurEntity is TvEntityWithPen then
    TvEntityWithPen(CurEntity).ApplyPenToCanvas(ADest);

  CurEntity.Render(ADest, ADestX, ADestY, AMulX, AMulY);
end;

procedure DrawFPVTextToCanvas(ASource: TvVectorialPage; CurText: TvText;
  ADest: TFPCustomCanvas;
  ADestX: Integer = 0; ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0);
begin
  CurText.Render(ADest, ADestX, ADestY, AMulX, AMulY);
end;

end.

