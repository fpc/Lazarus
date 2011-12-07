{
 /***************************************************************************
                              lazcanvas.pas
                              ---------------

 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Felipe Monteiro de Carvalho

  Abstract:
    Classes and functions for extending TFPImageCanvas to support more stretching
    filters and to support all features from the LCL TCanvas
}
unit lazcanvas;

{$mode objfpc}{$H+}

interface

uses
  // RTL
  Classes, SysUtils, contnrs, Math,
  // FCL-Image
  fpimgcanv, fpcanvas, fpimage, clipping, pixtools, fppixlcanv,
  // regions
  lazregions;

type

  { TFPSharpInterpolation }

  // This does a very sharp and square interpolation for stretching,
  // similar to StretchBlt from the Windows API
  TFPSharpInterpolation = class (TFPCustomInterpolation)
  protected
    procedure Execute (x,y,w,h : integer); override;
  end;

  { TLazCanvasState }

  TLazCanvasState = class
  public
    Brush: TFPCustomBrush;
    Pen: TFPCustomPen;
    BaseWindowOrg: TPoint;
    WindowOrg: TPoint;
    destructor Destroy; override;
  end;

  { TLazCanvas }

  TLazCanvas = class(TFPImageCanvas)
  private
    FAssignedBrush: TFPCustomBrush;
    FAssignedPen: TFPCustomPen;
    FBaseWindowOrg: TPoint;
    {$if defined(ver2_4) or defined(ver2_5) or defined(ver2_6)}
    FLazClipRegion: TLazRegion;
    {$endif}
    FWindowOrg: TPoint; // already in absolute coords with BaseWindowOrg summed up
    GraphicStateStack: TObjectStack; // TLazCanvasState
    function GetAssignedBrush: TFPCustomBrush;
    function GetAssignedPen: TFPCustomPen;
    function GetWindowOrg: TPoint;
    procedure SetWindowOrg(AValue: TPoint);
  protected
    procedure SetColor (x,y:integer; const AValue:TFPColor); override;
    // Routines broken/unimplemented in FPC
    procedure DoPolygonFill (const points:array of TPoint); override;
    // Routines which don't work with out extended clipping in TFPImageCanvas
    procedure DoLine (x1,y1,x2,y2:integer); override;
  public
    HasNoImage: Boolean;
    NativeDC: PtrInt; // Utilized by LCL-CustomDrawn
    constructor create (AnImage : TFPCustomImage);
    destructor destroy; override;
    procedure SetLazClipRegion(ARegion: TLazRegion);
    // Canvas states stack
    procedure SaveState;
    procedure RestoreState;
    // A simple operation to bring the Canvas in the default LCL TCanvas state
    procedure ResetCanvasState;
    // Alpha blending operations
    procedure AlphaBlend(ASource: TLazCanvas;
      const ADestX, ADestY, ASourceX, ASourceY, ASourceWidth, ASourceHeight: Integer);
    procedure CanvasCopyRect(ASource: TLazCanvas;
      const ADestX, ADestY, ASourceX, ASourceY, ASourceWidth, ASourceHeight: Integer);
    // Compatibility with older FPC versions
    {$if defined(ver2_4) or defined(ver2_5)}
    procedure FillRect(const ARect: TRect);
    procedure FillRect(X1,Y1,X2,Y2: Integer);
    {$endif}
    // Utilized by LCLIntf.SelectObject and by RestoreState
    // This needed to be added because Pen/Brush.Assign raises exceptions
    procedure AssignPenData(APen: TFPCustomPen);
    procedure AssignBrushData(ABrush: TFPCustomBrush);
    // These properties are utilized to implement LCLIntf.SelectObject
    // to keep track of which brush handle was assigned to this canvas
    // They are not utilized by TLazCanvas itself
    property AssignedPen: TFPCustomPen read GetAssignedPen write FAssignedPen;
    property AssignedBrush: TFPCustomBrush read GetAssignedBrush write FAssignedBrush;
    //
    // SetWindowOrg operations will be relative to BaseWindowOrg,
    // This is very useful for implementing the non-native wincontrol,
    // because operations of SetWindowOrg inside a non-native wincontrol will be
    // based upon the BaseWindowOrg which is set relative to the Form canvas
    property BaseWindowOrg: TPoint read FBaseWindowOrg write FBaseWindowOrg;
    {$if defined(ver2_4) or defined(ver2_5) or defined(ver2_6)}
    property ClipRegion: TLazRegion read FLazClipRegion write FLazClipRegion;
    {$endif}
    property WindowOrg: TPoint read GetWindowOrg write SetWindowOrg;
  end;

implementation

{ TLazCanvasState }

destructor TLazCanvasState.Destroy;
begin
  if Brush <> nil then Brush.Free;
  if Pen <> nil then Pen.Free;
  inherited Destroy;
end;

{ TLazCanvas }

function TLazCanvas.GetAssignedBrush: TFPCustomBrush;
begin
  if FAssignedBrush = nil then
    Result := TFPEmptyBrush.Create
  else
    Result := FAssignedBrush;
end;

function TLazCanvas.GetAssignedPen: TFPCustomPen;
begin
  if FAssignedPen = nil then
    Result := TFPEmptyPen.Create
  else
    Result := FAssignedPen;
end;

function TLazCanvas.GetWindowOrg: TPoint;
begin
  Result := Point(FWindowOrg.X-FBaseWindowOrg.X, FWindowOrg.Y-FBaseWindowOrg.Y)
end;

procedure TLazCanvas.SetWindowOrg(AValue: TPoint);
begin
  FWindowOrg.X := AValue.X+FBaseWindowOrg.X;
  FWindowOrg.Y := AValue.Y+FBaseWindowOrg.Y;
end;

procedure TLazCanvas.SetColor(x, y: integer; const AValue: TFPColor);
var
  lx, ly: Integer;
begin
  lx := x + FWindowOrg.X;
  ly := y + FWindowOrg.Y;
  {$if defined(ver2_4) or defined(ver2_5) or defined(ver2_6)}
  if Clipping and (not FLazClipRegion.IsPointInRegion(lx, ly)) then
    Exit;
  if (lx >= 0) and (lx < width) and (ly >= 0) and (ly < height) then
      Image.Colors[lx,ly] := AValue;
  {$else}
  if Clipping and (not FClipRegion.IsPointInRegion(lx, ly)) then
    Exit;
  if (lx >= 0) and (lx < width) and (ly >= 0) and (ly < height) then
      FImage.Colors[lx,ly] := AValue;
  {$endif}
end;

procedure TLazCanvas.DoPolygonFill(const points: array of TPoint);
var
  lBoundingBox: TRect;
  x, y, i: integer;
begin
  // Find the Bounding Box of the Polygon
  lBoundingBox := Rect(0, 0, 0, 0);
  for i := low(Points) to High(Points) do
  begin
    lBoundingBox.Left := Min(Points[i].X, lBoundingBox.Left);
    lBoundingBox.Top := Min(Points[i].Y, lBoundingBox.Top);
    lBoundingBox.Right := Max(Points[i].X, lBoundingBox.Right);
    lBoundingBox.Bottom := Max(Points[i].Y, lBoundingBox.Bottom);
  end;

  // Now scan all points using IsPointInPolygon
  for x := lBoundingBox.Left to lBoundingBox.Right do
    for y := lBoundingBox.Top to lBoundingBox.Bottom do
    begin
      if IsPointInPolygon(X, Y, Points) then SetColor(X, Y, Brush.FPColor);
    end;
end;

procedure TLazCanvas.DoLine(x1, y1, x2, y2: integer);
  procedure DrawOneLine (xx1,yy1, xx2,yy2:integer);
  begin
    if Clipping then
      CheckLineClipping (ClipRect, xx1,yy1, xx2,yy2);
    DrawSolidLine (self, xx1,yy1, xx2,yy2, Pen.FPColor);
  end;

  procedure SolidThickLine;
  var w1, w2, r : integer;
      MoreHor : boolean;
  begin
    // determine lines above and under
    w1 := pen.width div 2;
    w2 := w1;
    if w1+w2 = pen.width then
      dec (w1);
    // determine slanting
    MoreHor := (abs(x2-x1) < abs(y2-y1));
    if MoreHor then
      begin  // add lines left/right
      for r := 1 to w1 do
        DrawOneLine (x1-r,y1, x2-r,y2);
      for r := 1 to w2 do
        DrawOneLine (x1+r,y1, x2+r,y2);
      end
    else
      begin  // add lines above/under
      for r := 1 to w1 do
        DrawOneLine (x1,y1-r, x2,y2-r);
      for r := 1 to w2 do
        DrawOneLine (x1,y1+r, x2,y2+r);
      end;
  end;

begin
{ We can are not clip here because we clip in each drawn pixel
  or introduce a more complex algorithm to take into account lazregions
  if Clipping then
    CheckLineClipping (ClipRect, x1,y1, x2,y2);}
  case Pen.style of
    psSolid :
      begin
      DrawSolidLine (self, x1,y1, x2,y2, Pen.FPColor);
      if pen.width > 1 then
        SolidThickLine;
      end;
    psPattern:
      DrawPatternLine (self, x1,y1, x2,y2, pen.pattern);
      // Patterned lines have width always at 1
    psDash, psDot, psDashDot, psDashDotDot :
      DrawPatternLine (self, x1,y1, x2,y2, PenPatterns[Pen.Style]);
  end;
end;

constructor TLazCanvas.create(AnImage: TFPCustomImage);
begin
  inherited Create(AnImage);
  GraphicStateStack := TObjectStack.Create;
  HasNoImage := AnImage = nil;
end;

destructor TLazCanvas.destroy;
begin
  GraphicStateStack.Free;
  if FAssignedBrush <> nil then FAssignedBrush.Free;
  if FAssignedPen <> nil then FAssignedPen.Free;
  inherited destroy;
end;

procedure TLazCanvas.SetLazClipRegion(ARegion: TLazRegion);
begin
  if ARegion.IsSimpleRectRegion then
  begin
    Clipping := True;
    ClipRect := TLazRegionRect(ARegion.Parts.Items[0]).Rect;
    FLazClipRegion := ARegion;
  end;
end;

procedure TLazCanvas.SaveState;
var
  lState: TLazCanvasState;
begin
  lState := TLazCanvasState.Create;

  lState.Brush := Brush.CopyBrush;
  lState.Pen := Pen.CopyPen;

  GraphicStateStack.Push(lState);
end;

procedure TLazCanvas.RestoreState;
var
  lState: TLazCanvasState;
begin
  lState := TLazCanvasState(GraphicStateStack.Pop());
  if lState = nil then Exit;

  AssignPenData(lState.Pen);
  AssignBrushData(lState.Brush);

  lState.Free;
end;

procedure TLazCanvas.ResetCanvasState;
begin
  Pen.FPColor := colBlack;
  Pen.Style := psSolid;

  Brush.FPColor := colWhite;
  Brush.Style := bsSolid;
end;

procedure TLazCanvas.AlphaBlend(ASource: TLazCanvas;
  const ADestX, ADestY, ASourceX, ASourceY, ASourceWidth, ASourceHeight: Integer);
var
  x, y, CurX, CurY: Integer;
  MaskValue, InvMaskValue: Word;
  CurColor: TFPColor;
  lDrawWidth, lDrawHeight: Integer;
begin
  // Take care not to draw outside the destination area
  lDrawWidth := Min(Self.Width - ADestX, ASource.Width);
  lDrawHeight := Min(Self.Height - ADestY, ASource.Height);
  for y := 0 to lDrawHeight - 1 do
  begin
    for x := 0 to lDrawWidth - 1 do
    begin
      CurX := ADestX + x;
      CurY := ADestY + y;

      // Never draw outside the destination
      if (CurX < 0) or (CurY < 0) then Continue;

      MaskValue := ASource.Colors[x, y].alpha;
      InvMaskValue := $FFFF - MaskValue;

      if MaskValue = $FFFF then
      begin
        Self.Colors[CurX, CurY] := ASource.Colors[x, y];
      end
      else if MaskValue > $00 then
      begin
        CurColor := Self.Colors[CurX, CurY];

        CurColor.Red := Round(
          CurColor.Red * InvMaskValue / $FFFF +
          ASource.Colors[x, y].Red * MaskValue / $FFFF);

        CurColor.Green := Round(
          CurColor.Green * InvMaskValue / $FFFF +
          ASource.Colors[x, y].Green * MaskValue / $FFFF);

        CurColor.Blue := Round(
          CurColor.Blue * InvMaskValue / $FFFF +
          ASource.Colors[x, y].Blue * MaskValue / $FFFF);

        Self.Colors[CurX, CurY] := CurColor;
      end;
    end;
  end;
end;

procedure TLazCanvas.CanvasCopyRect(ASource: TLazCanvas; const ADestX, ADestY,
  ASourceX, ASourceY, ASourceWidth, ASourceHeight: Integer);
var
  x, y, CurDestX, CurDestY, CurSrcX, CurSrcY: Integer;
  MaskValue, InvMaskValue: Word;
  CurColor: TFPColor;
  lDrawWidth, lDrawHeight: Integer;
begin
  // Take care not to draw outside the source and also not outside the destination area
  lDrawWidth := Min(Self.Width - ADestX, ASource.Width - ASourceX);
  lDrawHeight := Min(Self.Height - ADestY, ASource.Height - ASourceY);
  lDrawWidth := Min(lDrawWidth, ASourceWidth);
  lDrawHeight := Min(lDrawHeight, ASourceHeight);

  for y := 0 to lDrawHeight - 1 do
  begin
    for x := 0 to lDrawWidth - 1 do
    begin
      CurDestX := ADestX + x;
      CurDestY := ADestY + y;
      CurSrcX := ASourceX + x;
      CurSrcY := ASourceY + y;

      // Never draw outside the destination
      if (CurDestX < 0) or (CurDestY < 0) then Continue;

      Self.Colors[CurDestX, CurDestY] := ASource.Colors[CurSrcX, CurSrcY];
    end;
  end;
end;

{$if defined(ver2_4) or defined(ver2_5)}
procedure TLazCanvas.FillRect(const ARect: TRect);
begin
  if (Brush.style <> bsClear) then
    begin
    //if not (brush is TFPCustomDrawBrush) then
      DoRectangleFill (ARect)
    //else
    //  with ARect do
    //    TFPCustomDrawBrush(Brush).Rectangle (left,top,right,bottom);
    end;
end;

procedure TLazCanvas.FillRect(X1, Y1, X2, Y2: Integer);
begin
  FillRect (Rect(X1,Y1,X2,Y2));
end;
{$endif}

procedure TLazCanvas.AssignPenData(APen: TFPCustomPen);
begin
  if APen = nil then Exit;
  Pen.FPColor := APen.FPColor;
  Pen.Style := APen.Style;
  Pen.Width := APen.Width;
end;

procedure TLazCanvas.AssignBrushData(ABrush: TFPCustomBrush);
begin
  if ABrush = nil then Exit;
  Brush.FPColor := ABrush.FPColor;
  Brush.Style := ABrush.Style;
end;

{ TFPWindowsSharpInterpolation }

procedure TFPSharpInterpolation.Execute(x, y, w, h: integer);
// paint Image on Canvas at x,y,w*h
var
  srcx, srcy: Integer; // current coordinates in the source image
  dx, dy: Integer; // current coordinates in the destination canvas
  lWidth, lHeight: Integer; // Image size
  lColor: TFPColor;
begin
  if (w<=0) or (h<=0) or (image.Width=0) or (image.Height=0) then
    exit;

  lWidth := Image.Width-1;
  lHeight := Image.Height-1;

  for dx := 0 to w-1 do
   for dy := 0 to h-1 do
   begin
     srcx := Round((dx / w) * lWidth);
     srcy := Round((dy / h) * lHeight);
     lColor := Image.Colors[srcx, srcy];
     Canvas.Colors[dx+x, dy+y] := lColor;
   end;
end;

end.

