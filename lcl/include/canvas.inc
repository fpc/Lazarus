{%MainUnit ../graphics.pp}
{******************************************************************************
                                     TCANVAS
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

const
  csAllValid = [csHandleValid..csBrushValid];

{-----------------------------------------------}
{--  TCanvas.BrushCopy --}
{-----------------------------------------------}
Procedure TCanvas.BrushCopy(Dest: TRect; InternalImages: TBitmap; Src: TRect;
  TransparentColor :TColor);
Begin
  //TODO:TCANVAS.BRUSHCOPY
end;

{-----------------------------------------------}
{--  TCanvas.Draw --}
{-----------------------------------------------}
Procedure TCanvas.Draw(X, Y: Integer; SrcGraphic: TGraphic);
var
  ARect: TRect;
begin
  if not Assigned(SrcGraphic) then exit;
  ARect:=Bounds(X,Y,SrcGraphic.Width,SrcGraphic.Height);
  StretchDraw(ARect,SrcGraphic);
end;

{-----------------------------------------------}
{--  TCanvas.StretchDraw --}
{-----------------------------------------------}
procedure TCanvas.StretchDraw(const DestRect: TRect; SrcGraphic: TGraphic);
begin
  if not Assigned(SrcGraphic) then exit;
  Changing;
  RequiredState([csHandleValid]);
  SrcGraphic.Draw(Self, DestRect);
  Changed;
end;

{-----------------------------------------------}
{--  TCanvas.GetClipRect --}
{-----------------------------------------------}
function TCanvas.GetClipRect: TRect;
begin
  If GetClipBox(FHandle, @Result) = ERROR then
    Result := Rect(0,0,2000,2000);{Just in Case}
end;


{-----------------------------------------------}
{--  TCanvas.CopyRect --}
{-----------------------------------------------}
Procedure TCanvas.CopyRect(const Dest: TRect; SrcCanvas: TCanvas;
  const Source: TRect);
var
  SH, SW, DH, DW: Integer;
Begin
  if SrcCanvas= nil then exit;

  SH := Source.Bottom - Source.Top;
  SW := Source.Right - Source.Left;
  if (SH=0) or (SW=0) then exit;
  DH := Dest.Bottom - Dest.Top;
  DW := Dest.Right - Dest.Left;
  if (Dh=0) or (DW=0) then exit;

  SrcCanvas.RequiredState([csHandleValid]);
  Changing;
  RequiredState([csHandleValid]);

  //DebugLn('TCanvas.CopyRect ',ClassName,' SrcCanvas=',SrcCanvas.ClassName,' ',
  //  ' Src=',Source.Left,',',Source.Top,',',SW,',',SH,
  //  ' Dest=',Dest.Left,',',Dest.Top,',',DW,',',DH);
  StretchBlt(FHandle, Dest.Left, Dest.Top, DW, DH,
    SrcCanvas.FHandle, Source.Left, Source.Top, SW, SH, CopyMode);
  Changed;
end;
{-----------------------------------------------}
{--  TCanvas.GetPixel --}
{-----------------------------------------------}
function TCanvas.GetPixel(X, Y: Integer): TColor;
begin
  Result := InterfaceObject.DCGetPixel(Self.Handle, X, Y);
end;

{-----------------------------------------------}
{--  TCanvas.SetPixel --}
{-----------------------------------------------}
procedure TCanvas.SetPixel(X, Y: Integer; Value: TColor);
begin
  InterfaceObject.DCSetPixel(Self.Handle, X, Y, Value);
end;

{------------------------------------------------------------------------------
  procedure TCanvas.RealizeAutoRedraw;
 ------------------------------------------------------------------------------}
procedure TCanvas.RealizeAutoRedraw;
begin
  if FAutoRedraw and HandleAllocated then
    InterfaceObject.DCRedraw(Handle);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.CreateBrush
  Params:   None
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.CreateBrush;
var OldHandle: HBRUSH;
begin
//DebugLn('[TCanvas.CreateBrush] ',Classname,'  Self=',DbgS(Self)
// ,'  Brush=',DbgS(Brush));
  OldHandle:=SelectObject(FHandle, Brush.Handle);
  //debugln('TCanvas.CreateBrush ',ClassName,' Self=',DbgS(Self),' OldHandle=',DbgS(OldHandle),8),' NewHandle=',DbgS(Brush.Handle),' FSavedBrushHandle=',DbgS(Cardinal(FSavedBrushHandle));
  if (OldHandle<>Brush.Handle) and (FSavedBrushHandle=0) then
    FSavedBrushHandle:=OldHandle;
  Include(FState, csBrushValid);
  SetBkColor(FHandle, Brush.Color);
  SetBkMode(FHandle, TRANSPARENT);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.CreatePen
  Params:   None
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.CreatePen;
var OldHandle: HPEN;
const PenModes:Array[TPenMode] of Integer =
  ( R2_BLACK, R2_WHITE, R2_NOP, R2_NOT, R2_COPYPEN, R2_NOTCOPYPEN, R2_MERGEPENNOT,
    R2_MASKPENNOT, R2_MERGENOTPEN, R2_MASKNOTPEN, R2_MERGEPEN, R2_NOTMERGEPEN,
    R2_MASKPEN, R2_NOTMASKPEN, R2_XORPEN, R2_NOTXORPEN );
{
  TPenMode = (pmBlack, pmWhite, pmNop, pmNot, pmCopy, pmNotCopy, pmMergePenNot,
              pmMaskPenNot, pmMergeNotPen, pmMaskNotPen, pmMerge,pmNotMerge,
              pmMask, pmNotMask, pmXor, pmNotXor
}
begin
//DebugLn('[TCanvas.CreatePen] ',Classname,'  Self=',DbgS(Self)
// ,'  Pen=',DbgS(Pen));
  OldHandle:=SelectObject(FHandle, Pen.Handle);
  if (OldHandle<>Pen.Handle) and (FSavedPenHandle=0) then
    FSavedPenHandle:=OldHandle;
  MoveTo(PenPos.X,PenPos.Y);
  Include(FState, csPenValid);
  SetROP2(FHandle, PenModes[Pen.Mode]);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.CreateFont
  Params:   None
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.CreateFont;
var OldHandle: HFONT;
begin
  OldHandle:=SelectObject(FHandle, Font.Handle);
  if (OldHandle<>Font.Handle) and (FSavedFontHandle=0) then
    FSavedFontHandle:=OldHandle;
  Include(FState, csFontValid);
  SetTextColor(FHandle, Font.Color);
end;

{------------------------------------------------------------------------------
  Procedure TCanvas.CreateRegion;
 ------------------------------------------------------------------------------}
Procedure TCanvas.CreateRegion;
var OldHandle: HRGN;
begin
  OldHandle:=SelectObject(FHandle, Region.Handle);
  if (OldHandle<>Region.Handle) and (FSavedRegionHandle=0) then
    FSavedRegionHandle:=OldHandle;
  Include(FState, csRegionValid);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.SetAutoReDraw
  Params:   Value
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.SetAutoRedraw(Value : Boolean);
begin
  if FAutoRedraw=Value then exit;
  FAutoRedraw := Value;
  RealizeAutoRedraw;
end;

{------------------------------------------------------------------------------
  procedure TCanvas.SetInternalPenPos(const Value: TPoint);
 ------------------------------------------------------------------------------}
procedure TCanvas.SetInternalPenPos(const Value: TPoint);
begin
  {$IFDEF UseFPCanvas}
  inherited SetPenPos(Value);
  {$ELSE}
  FPenPos:=Value;
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.SetLazBrush
  Params:   Value
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.SetLazBrush(Value : TBrush);
begin
  FBrush.Assign(Value);
end;

procedure TCanvas.SetPenPos(const AValue: TPoint);
begin
  MoveTo(AValue.X,AValue.Y);
  // fpcanvas TODO
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.SetLazFont
  Params:   Value
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.SetLazFont(Value : TFont);
begin
  FFont.Assign(Value);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.SetLazPen
  Params:   Value
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.SetLazPen(Value : TPen);
begin
  FPen.Assign(Value);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.SetRegion
  Params:   Value
  Returns:  Nothing

 ------------------------------------------------------------------------------}
Procedure TCanvas.SetRegion(value : TRegion);
begin
  FRegion.Assign(Value);
end;

{$IFDEF UseFPCanvas}
function TCanvas.DoCreateDefaultFont: TFPCustomFont;
begin
  Result:=TFont.Create;
end;

function TCanvas.DoCreateDefaultPen: TFPCustomPen;
begin
  Result:=TPen.Create;
end;

function TCanvas.DoCreateDefaultBrush: TFPCustomBrush;
begin
  Result:=TBrush.Create;
end;

procedure TCanvas.SetColor(x, y: integer; const Value: TFPColor);
begin
  Pixels[x,y]:=FPColorToTColor(Value);
end;

function TCanvas.GetColor(x, y: integer): TFPColor;
begin
  Result:=TColorToFPColor(Pixels[x,y]);
end;

procedure TCanvas.SetHeight(AValue: integer);
begin
  RaiseGDBException('TCanvas.SetHeight not allowed for LCL canvas');
end;

function TCanvas.GetHeight: integer;
var
  w: Integer;
begin
  if HandleAllocated then
    GetWindowSize(Handle,w,Result)
  else
    Result:=0;
end;

procedure TCanvas.SetWidth(AValue: integer);
begin
  RaiseGDBException('TCanvas.SetWidth not allowed for LCL canvas');
end;

function TCanvas.GetWidth: integer;
var
  h: Integer;
begin
  if HandleAllocated then
    GetWindowSize(Handle,Result,h)
  else
    Result:=0;
end;

procedure TCanvas.DoLockCanvas;
begin
  if FLock=0 then InitializeCriticalSection(FLock);
  EnterCriticalSection(FLock);
  inherited DoLockCanvas;
end;

procedure TCanvas.DoUnlockCanvas;
begin
  LeaveCriticalSection(FLock);
  inherited DoUnlockCanvas;
end;

procedure TCanvas.DoTextOut(x, y: integer; Text: string);
begin
  TextOut(X,Y,Text);
end;

procedure TCanvas.DoGetTextSize(Text: string; var w, h: integer);
var
  TxtSize: tagSIZE;
begin
  TxtSize:=TextExtent(Text);
  w:=TxtSize.cx;
  h:=TxtSize.cy;
end;

function TCanvas.DoGetTextHeight(Text: string): integer;
begin
  Result:=TextHeight(Text);
end;

function TCanvas.DoGetTextWidth(Text: string): integer;
begin
  Result:=TextWidth(Text);
end;

procedure TCanvas.DoRectangle(const Bounds: TRect);
begin
  Frame(Bounds);
end;

procedure TCanvas.DoRectangleFill(const Bounds: TRect);
begin
  FillRect(Bounds);
end;

procedure TCanvas.DoRectangleAndFill(const Bounds: TRect);
begin
  Rectangle(Bounds);
end;

procedure TCanvas.DoEllipse(const Bounds: TRect);
var
  x: Integer;
  y: Integer;
  w: Integer;
  h: Integer;
begin
  if Bounds.Left<Bounds.Right then x:=Bounds.Left else x:=Bounds.Right;
  if Bounds.Top<Bounds.Bottom then y:=Bounds.Top else y:=Bounds.Bottom;
  w:=Abs(Bounds.Right-Bounds.Left);
  h:=Abs(Bounds.Bottom-Bounds.Top);
  Arc(x,y,w,h,0,360*16);
end;

procedure TCanvas.DoEllipseFill(const Bounds: TRect);
begin
  Ellipse(Bounds);
end;

procedure TCanvas.DoEllipseAndFill(const Bounds: TRect);
begin
  inherited DoEllipseAndFill(Bounds);
end;

procedure TCanvas.DoPolygon(const Points: array of TPoint);
begin
  Polyline(Points);
end;

procedure TCanvas.DoPolygonFill(const Points: array of TPoint);
begin
  Polygon(Points);
end;

procedure TCanvas.DoPolygonAndFill(const Points: array of TPoint);
begin
  inherited DoPolygonAndFill(Points);
end;

procedure TCanvas.DoPolyline(const Points: array of TPoint);
begin
  Polyline(Points);
end;

procedure TCanvas.DoFloodFill(x, y: integer);
begin
  FloodFill(x,y,Color,fsSurface);
end;

procedure TCanvas.DoMoveTo(x, y: integer);
begin
  MoveTo(X,Y);
end;

procedure TCanvas.DoLineTo(x, y: integer);
begin
  LineTo(X,Y);
end;

procedure TCanvas.DoLine(x1, y1, x2, y2: integer);
begin
  Line(x1,y1,x2,y2);
end;

procedure TCanvas.DoCopyRect(x, y: integer; SrcCanvas: TFPCustomCanvas;
  const SourceRect: TRect);
  
  Procedure WarnNotSupported;
  begin
    debugln('WARNING: TCanvas.DoCopyRect from ',DbgSName(SrcCanvas));
  end;
  
var
  SH: Integer;
  SW: Integer;
Begin
  if SrcCanvas=nil then exit;
  if SrcCanvas is TCanvas then begin
    SW := SourceRect.Right - SourceRect.Left;
    SH := SourceRect.Bottom - SourceRect.Top;
    if (SH=0) or (SW=0) then exit;
    CopyRect(Rect(x,y,x+SW,y+SH),TCanvas(SrcCanvas),SourceRect);
  end else begin
    WarnNotSupported;
  end;
end;

procedure TCanvas.DoDraw(x, y: integer; const Image: TFPCustomImage);
var
  LazImg: TLazIntfImage;
  BitmapHnd, MaskHnd: HBitmap;
begin
  if Image=nil then exit;
  LazImg:=TLazIntfImage(Image);
  BitmapHnd:=0;
  MaskHnd:=0;
  try
    if not (LazImg is TLazIntfImage) then begin
      LazImg:=TLazIntfImage.Create(0,0);
      RequiredState([csHandleValid]);
      LazImg.GetDescriptionFromDevice(Handle);
      LazImg.Assign(Image);
    end;
    LazImg.CreateBitmap(BitmapHnd,MaskHnd,false);
    if BitmapHnd=0 then exit;

    Changing;
    RequiredState([csHandleValid]);
    StretchBlt(FHandle,x,y,LazImg.Width,LazImg.Height,
      BitmapHnd, 0,0,LazImg.Width,LazImg.Height, CopyMode);
    Changed;
  finally
    if Image<>LazImg then LazImg.Free;
    if BitmapHnd<>0 then DeleteDC(BitmapHnd);
    if MaskHnd<>0 then DeleteDC(MaskHnd);
  end;
end;

procedure TCanvas.CheckHelper(AHelper: TFPCanvasHelper);
begin
  debugln('TCanvas.CheckHelper ignored for ',DbgSName(AHelper));
end;
{$ENDIF}

{------------------------------------------------------------------------------
  Method:   TCanvas.Arc
  Params:   Left,Top,AWidth,AHeight,angle1,angle2
  Returns:  Nothing

  Use Arc to draw an elliptically curved line with the current Pen.
  The angles angle1 and angle2 are 1/16th of a degree. For example, a full
  circle equals 5760 (16*360). Positive values of Angle and AngleLength mean
  counter-clockwise while negative values mean clockwise direction.
  Zero degrees is at the 3'o clock position.

 ------------------------------------------------------------------------------}
procedure TCanvas.Arc(Left,Top,AWidth,AHeight,angle1,angle2 : Integer);
begin
  Changing;
  RequiredState([csHandleValid, csPenValid]);
  LCLIntf.Arc(FHandle,Left,Top,AWidth,AHeight,angle1,angle2);
  Changed;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Arc
  Params:   DC,Left,Top,AWidth,AHeight,sx,sy,ex,ey
  Returns:  Nothing

  Use Arc to draw an elliptically curved line with the current Pen. The
  values sx,sy, and ex,ey represent the starting and ending radial-points
  between which the Arc is drawn.

------------------------------------------------------------------------------}
procedure TCanvas.Arc(Left,Top,AWidth,AHeight,sx,sy,ex,ey : Integer);
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  LCLIntf.RadialArc(FHandle,Left,Top,AWidth,AHeight,sx,sy,ex,ey);
  Changed;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.RadialPie
  Params:   x,y, AWidth, AHeight, StartAngle16Deg, EndAngle16Deg: Integer
  Returns:  Nothing

  Use Pie to draw a filled pie-shaped wedge on the canvas.
  The angles StartAngle16Deg and EndAngle16Deg are 1/16th of a degree.
  For example, a full circle equals 5760 (16*360).
  Positive values of Angle and AngleLength mean
  counter-clockwise while negative values mean clockwise direction.
  Zero degrees is at the 3'o clock position.

 ------------------------------------------------------------------------------}
procedure TCanvas.RadialPie(x, y, AWidth, AHeight,
  StartAngle16Deg, EndAngle16Deg: Integer);
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  LCLIntf.RadialPieWithAngles(FHandle,x,y,AWidth,AHeight,
                              StartAngle16Deg,EndAngle16Deg);
  Changed;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.RadialPie
  Params:   x,y, AWidth, AHeight, sx,sy,ex,ey
  Returns:  Nothing

  Use Pie to draw a filled Pie-shaped wedge on the canvas. The values sx,sy,
  and ex,ey represent the starting and ending radial-points between which
  the Bounding-Arc is drawn.

------------------------------------------------------------------------------}
procedure TCanvas.RadialPie(x,y, AWidth, AHeight, sx,sy,ex,ey: Integer);
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  LCLIntf.RadialPie(FHandle,x,y,AWidth,AHeight,sx,sy,ex,ey);
  Changed;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Pie
  Params:   EllipseX1, EllipseY1, EllipseX2, EllipseY2,
            StartX, StartY, EndX, EndY
  Returns:  Nothing

  Use Pie to draw a filled Pie-shaped wedge on the canvas. The pie is part of
  an ellipse between the points EllipseX1, EllipseY1, EllipseX2, EllipseY2.
  The values StartX, StartY and EndX, EndY represent the starting and ending
  radial-points between which the Bounding-Arc is drawn.

------------------------------------------------------------------------------}
procedure TCanvas.Pie(EllipseX1, EllipseY1, EllipseX2, EllipseY2,
  StartX, StartY, EndX, EndY: Integer);
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  LCLIntf.Pie(FHandle,EllipseX1,EllipseY1,EllipseX2,EllipseY2,
               StartX,StartY,EndX,EndY);
  Changed;
end;

{------------------------------------------------------------------------------
  Method:  TCanvas.PolyBezier
  Params:  Points, Filled, Continous
  Returns: Boolean

  Use Polybezier to draw cubic Bézier curves. The first curve is drawn from the
  first point to the fourth point with the second and third points being the
  control points. If the Continuous flag is TRUE then each subsequent curve
  requires three more points, using the end-point of the previous Curve as its
  starting point, the first and second points being used as its control points,
  and the third point its end-point. If the continous flag is set to FALSE,
  then each subsequent Curve requires 4 additional points, which are used
  excatly as in the first curve. Any additonal points which do not add up to
  a full bezier(4 for Continuous, 3 otherwise) are ingored. There must be at
  least 4 points for an drawing to occur. If the Filled Flag is set to TRUE
  then the resulting Poly-Bézier will be drawn as a Polygon.

 ------------------------------------------------------------------------------}
procedure TCanvas.PolyBezier(const Points: array of TPoint;
  Filled: boolean{$IFNDEF VER1_0} = False{$ENDIF};
  Continuous: boolean{$IFNDEF VER1_0} = False{$ENDIF});
var NPoints, i: integer;
  PointArray: ^TPoint;
begin
  NPoints:=High(Points)-Low(Points)+1;
  if NPoints<=0 then exit;
  GetMem(PointArray,SizeOf(TPoint)*NPoints);
  for i:=0 to NPoints-1 do
    PointArray[i]:=Points[i+Low(Points)];
  PolyBezier(PointArray, NPoints, Filled, Continuous);
  FreeMem(PointArray);
end;

procedure TCanvas.PolyBezier(Points: PPoint; NumPts: Integer;
  Filled: boolean{$IFNDEF VER1_0} = False{$ENDIF};
  Continuous: boolean{$IFNDEF VER1_0} = False{$ENDIF});
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  LCLIntf.PolyBezier(FHandle,Points,NumPts,Filled, Continuous);
  Changed;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.PolyBezier
  Params:   Points
  Returns:  Nothing

 ------------------------------------------------------------------------------}
{$ifdef VER1_0} 
procedure TCanvas.PolyBezier(const Points: array of TPoint);
begin
  PolyBezier(Points, False, True);
end;
{$endif}

{------------------------------------------------------------------------------
  Method:   TCanvas.Polygon
  Params:   Points: array of TPoint; Winding: Boolean = False;
            StartIndex: Integer = 0; NumPts: Integer = -1
  Returns:  Nothing

  Use Polygon to draw a closed, many-sided shape on the canvas, using the value
  of Pen. After drawing the complete shape, Polygon fills the shape using the
  value of Brush.
  The Points parameter is an array of points that give the vertices of the
  polygon.
  Winding determines how the polygon is filled. When Winding is True, Polygon
  fills the shape using the Winding fill algorithm. When Winding is False,
  Polygon uses the even-odd (alternative) fill algorithm.
  StartIndex gives the index of the first point in the array to use. All points
  before this are ignored.
  NumPts indicates the number of points to use, starting at StartIndex.
  If NumPts is -1 (the default), Polygon uses all points from StartIndex to the
  end of the array.
  The first point is always connected to the last point.
  To draw a polygon on the canvas, without filling it, use the Polyline method,
  specifying the first point a second time at the end.
}
procedure TCanvas.Polygon(const Points: array of TPoint; Winding: Boolean;
  StartIndex: Integer; NumPts: Integer);
var
  NPoints: integer;
begin
  if NumPts<0 then
    NPoints:=High(Points)-StartIndex+1
  else
    NPoints:=NumPts;
  if NPoints<=0 then exit;
  Polygon(@Points[StartIndex],NPoints,Winding);
end;

procedure TCanvas.Polygon(Points: PPoint; NumPts: Integer;
  Winding: boolean{$IFNDEF VER1_0} = False{$ENDIF});
begin
  if NumPts<=0 then exit;
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  LCLIntf.Polygon(FHandle,Points,NumPts,Winding);
  Changed;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Polygon
  Params:   Points
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.Polygon(const Points: array of TPoint);
begin
  Polygon(Points, True, Low(Points), High(Points) - Low(Points) + 1);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Polyline
  Params:   Points: array of TPoint;
            StartIndex: Integer = 0; NumPts: Integer = -1
  Returns:  Nothing

  Use Polyline to connect a set of points on the canvas. If you specify only two
  points, Polyline draws a single line.
  The Points parameter is an array of points to be connected.
  StartIndex identifies the first point in the array to use.
  NumPts indicates the number of points to use. If NumPts is -1 (the default),
  PolyLine uses all the points from StartIndex to the end of the array.
  Calling the MoveTo function with the value of the first point, and then
  repeatedly calling LineTo with all subsequent points will draw the same image
  on the canvas. However, unlike LineTo, Polyline does not change the value of
  PenPos.
}
procedure TCanvas.Polyline(const Points: array of TPoint; StartIndex: Integer;
  NumPts: Integer);
var
  NPoints : integer;
begin
  if NumPts<0 then
    NPoints:=High(Points)-StartIndex+1
  else
    NPoints:=NumPts;
  if NPoints<=0 then exit;
  Polyline(@Points[StartIndex], NPoints);
end;

procedure TCanvas.Polyline(Points: PPoint; NumPts: Integer);
begin
  Changing;
  RequiredState([csHandleValid, csPenValid]);
  LCLIntf.Polyline(FHandle,Points,NumPts);
  Changed;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Polyline
  Params:   Points
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.Polyline(const Points: array of TPoint);
begin
  Polyline(Points, Low(Points), High(Points) - Low(Points) + 1);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Ellipse
  Params:   X1, Y1, X2, Y2
  Returns:  Nothing

  Use Ellipse to draw a filled circle or ellipse on the canvas.

 ------------------------------------------------------------------------------}
procedure TCanvas.Ellipse(x1, y1, x2, y2: Integer);
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  LCLIntf.Ellipse(FHandle,x1,y1,x2,y2);
  Changed;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Ellipse
  Params:   ARect: TRect
  Returns:  Nothing

  Use Ellipse to draw a filled circle or ellipse on the canvas.

 ------------------------------------------------------------------------------}
procedure TCanvas.Ellipse(const ARect: TRect);
begin
  Ellipse(ARect.Left,ARect.Top,ARect.Right,ARect.Bottom);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.FillRect
  Params:   ARect
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.FillRect(const ARect : TRect);
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid]);
  LCLIntf.FillRect(FHandle, ARect, Brush.Handle);
  Changed;
end;

{------------------------------------------------------------------------------
  procedure TCanvas.FillRect(X1,Y1,X2,Y2 : Integer);
 ------------------------------------------------------------------------------}
procedure TCanvas.FillRect(X1,Y1,X2,Y2 : Integer);
begin
  FillRect(Rect(X1,Y1,X2,Y2));
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.FillRect
  Params:   X, Y: Integer; Color: TColor; FillStyle: TFillStyle
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.FloodFill(X, Y: Integer; FillColor: TColor; FillStyle: TFillStyle
  );
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid]);
  LCLIntf.FloodFill(FHandle, X, Y, FillColor, FillStyle, Brush.Handle);
  Changed;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Frame3d
  Params:   Rect
  Returns:  the inflated rectangle (the inner rectangle without the frame)

 ------------------------------------------------------------------------------}
procedure TCanvas.Frame3d(var ARect: TRect; const FrameWidth : integer;
  const Style : TGraphicsBevelCut);
begin
  Changing;
  RequiredState([csHandleValid,csBrushValid,csPenValid]);
  LCLIntf.Frame3d(FHandle, ARect, FrameWidth, Style);
  Changed;
end;

{------------------------------------------------------------------------------
  procedure TCanvas.Frame(const ARect: TRect);

  Drawing the border of a rectangle with the current pen
 ------------------------------------------------------------------------------}
procedure TCanvas.Frame(const ARect: TRect);
begin
  Changing;
  RequiredState([csHandleValid, csPenValid]);
  LCLIntf.Frame(FHandle, ARect);
  Changed;
end;

{------------------------------------------------------------------------------
  procedure TCanvas.Frame(const ARect: TRect);

  Drawing the border of a rectangle with the current pen
 ------------------------------------------------------------------------------}
procedure TCanvas.Frame(X1, Y1, X2, Y2: Integer);
begin
  Frame(Rect(X1, Y1, X2, Y2));
end;

{------------------------------------------------------------------------------
  procedure TCanvas.FrameRect(const ARect: TRect);

  Drawing the border of a rectangle with the current brush
 ------------------------------------------------------------------------------}
procedure TCanvas.FrameRect(const ARect: TRect);
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid]);
  LCLIntf.FrameRect(FHandle, ARect, Brush.GetHandle);
  Changed;
end;

{------------------------------------------------------------------------------
  procedure TCanvas.FrameRect(const ARect: TRect);

  Drawing the border of a rectangle with the current brush
 ------------------------------------------------------------------------------}
procedure TCanvas.FrameRect(X1, Y1, X2, Y2: Integer);
begin
  FrameRect(Rect(X1, Y1, X2, Y2));
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Rectangle
  Params:   X1,Y1,X2,Y2
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.Rectangle(X1,Y1,X2,Y2 : Integer);
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  LCLIntf.Rectangle(FHandle, X1, Y1, X2, Y2);
  Changed;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Rectangle
  Params:   Rect
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.Rectangle(const ARect: TRect);
begin
  Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.RoundRect
  Params:   X1, Y1, X2, Y2, RX, RY
  Returns:  Nothing

 ------------------------------------------------------------------------------}
Procedure TCanvas.RoundRect(X1, Y1, X2, Y2: Integer; RX,RY : Integer);
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  LCLIntf.RoundRect(FHandle, X1, Y1, X2, Y2, RX, RY);
  Changed;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.RoundRect
  Params:   Rect, RX, RY
  Returns:  Nothing

 ------------------------------------------------------------------------------}
Procedure TCanvas.RoundRect(const Rect : TRect; RX,RY : Integer);
begin
  RoundRect(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, RX, RY);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.TextRect
  Params:   ARect, X, Y, Text
  Returns:  Nothing

 ------------------------------------------------------------------------------}
Procedure TCanvas.TextRect(const ARect: TRect; X,Y : Integer;
  const Text : String);
begin
  TextRect(ARect,X,Y,Text,TextStyle);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.TextRect
  Params:   ARect, X, Y, Text, Style
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.TextRect(ARect: TRect; X, Y : Integer; const Text : String;
  const Style : TTextStyle);
var
  Options : Longint;
  fRect : TRect;
begin
  //debugln('TCanvas.TextRect A Text="',Text,'" ',dbgs(ARect));
  Changing;
  Options := 0;
  case Style.Alignment of
    taRightJustify : Options := DT_RIGHT;
    taCenter : Options := DT_Center;
  end;
  case Style.Layout of
    tlCenter : Options := Options or DT_VCENTER;
    tlBottom : Options := Options or DT_BOTTOM;
  end;
  If Style.WordBreak then
    Options := Options or DT_WORDBREAK
  else
    If Style.SingleLine then
      Options := Options or DT_SINGLELINE;

  If not Style.Clipping then
    Options := Options or DT_NOCLIP;

  If not Style.ShowPrefix then
    Options := Options or DT_NOPREFIX;

  If Style.SystemFont then begin
    Options := Options or DT_INTERNAL;
    RequiredState([csHandleValid]);
    SelectObject(Self.Handle, GetStockObject(DEFAULT_GUI_FONT));
  end
  else
    RequiredState([csHandleValid, csFontValid]);

  // calculate text rectangle
  fRect := ARect;
  if Style.Alignment = taLeftJustify then
    fRect.Left := X;
  if Style.Layout = tlTop then
    fRect.Top := Y;
  if (Style.Alignment in [taRightJustify,taCenter])
  or (Style.Layout in [tlCenter,tlBottom]) then begin
    DrawText(Self.Handle,pChar(Text),Length(Text),fRect,DT_CALCRECT or Options);
    case Style.Alignment of
      taRightJustify : OffsetRect(fRect, ARect.Right - fRect.Right, 0);
      taCenter : OffsetRect(fRect, (ARect.Right - fRect.Right) div 2, 0);
    end;
    case Style.Layout of
      tlCenter : OffsetRect(fRect, 0,
               ((ARect.Bottom - ARect.Top) - (fRect.Bottom - fRect.Top)) div 2);
      tlBottom : OffsetRect(fRect, 0, ARect.Bottom - fRect.Bottom);
    end;
  end;
  If Style.Opaque then begin
    RequiredState([csHandleValid, csBrushValid]);
    FillRect(fRect);
  end;
  If Style.SystemFont then
    SetTextColor(Self.Handle, Font.Color);
  //debugln('TCanvas.TextRect DRAW Text="',Text,'" ',dbgs(fRect));
  DrawText(Self.Handle, pChar(Text), Length(Text), fRect, Options);
  Changed;
end;


{------------------------------------------------------------------------------
  Method:   TCanvas.TextOut
  Params:   X,Y,Text
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.TextOut(X,Y: Integer; const Text: String);
var
  Flags : Cardinal;
begin
  Changing;
  RequiredState([csHandleValid, csFontValid, csBrushValid]);
  Flags := 0;
  If TextStyle.Opaque then
    Flags := ETO_Opaque;
  ExtUTF8Out(FHandle, X, Y, Flags, nil, PChar(Text), Length(Text), nil);
  MoveTo(X + TextWidth(Text), Y);
  Changed;
end;

{------------------------------------------------------------------------------
  function TCanvas.HandleAllocated: boolean;
 ------------------------------------------------------------------------------}
function TCanvas.HandleAllocated: boolean;
begin
  Result:=(FHandle<>0);
end;

{------------------------------------------------------------------------------
  function TCanvas.GetUpdatedHandle(ReqState: TCanvasState): HDC;
 ------------------------------------------------------------------------------}
function TCanvas.GetUpdatedHandle(ReqState: TCanvasState): HDC;
begin
  RequiredState(ReqState+[csHandleValid]);
  Result:=FHandle;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.MoveTo
  Params:   X1,Y1
  Returns:  Nothing

 ------------------------------------------------------------------------------}
Procedure TCanvas.MoveTo(X1, Y1: Integer);
begin
  RequiredState([csHandleValid]);
  if LCLIntf.MoveToEx(FHandle, X1, Y1, nil) then begin
    SetInternalPenPos(Point(X1, Y1));
  end;
End;

{------------------------------------------------------------------------------
  Method:   TCanvas.LineTo
  Params:   X1,Y1
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.LineTo(X1, Y1 : Integer);
begin
  Changing;
  RequiredState([csHandleValid, csPenValid]);
  if LCLIntf.LineTo(FHandle, X1, Y1) then
    SetInternalPenPos(Point(X1, Y1));
  Changed;
end;


{------------------------------------------------------------------------------
  Method:   TCanvas.Line
  Params:   X1,Y1,X2,Y2
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.Line(X1,Y1,X2,Y2 : Integer);
begin
  MoveTo(X1, Y1);
  LineTo(X2, Y2);
end;


{------------------------------------------------------------------------------
  Function: TCanvas.GetColor
  Params:   None
  Returns:

 ------------------------------------------------------------------------------}
function TCanvas.GetColor:TColor;
begin
  Result:=Brush.Color;
end;


{------------------------------------------------------------------------------
  Method:   TCanvas.SetColor
  Params:   None
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.SetColor(c:TColor);
begin
  Brush.Color:=c;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.BrushChanged
  Params:   ABrush: The changed brush
  Returns:  Nothing

  Notify proc for a brush change
 ------------------------------------------------------------------------------}
procedure TCanvas.BrushChanged(ABrush: TObject);
begin
  if csBrushValid in FState then begin
    Exclude(FState, csBrushValid);
  end;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.FontChanged
  Params:   AFont: the changed font
  Returns:  Nothing

  Notify proc for a font change
 ------------------------------------------------------------------------------}
procedure TCanvas.FontChanged(AFont: TObject);
begin
  if csFontValid in FState then begin
    Exclude(FState, csFontValid);
  end;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.PenChanging
  Params:   APen: The changing pen
  Returns:  Nothing

  Notify proc for a pen change
 ------------------------------------------------------------------------------}
procedure TCanvas.PenChanging(APen: TObject);
begin
  if [csPenValid, csHandleValid] * FState = [csPenValid, csHandleValid] then
  begin
    Exclude(FState, csPenValid);
    SelectObject(FHandle, FSavedPenHandle);
    FSavedPenHandle := 0;
  end;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.PenChanged
  Params:   APen: The changed pen
  Returns:  Nothing

  Notify proc for a pen change
 ------------------------------------------------------------------------------}
procedure TCanvas.PenChanged(APen: TObject);
begin
  if csPenValid in FState
  then begin
    Exclude(FState, csPenValid);
  end;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.RegionChanged
  Params:   ARegion: The changed Region
  Returns:  Nothing

  Notify proc for a region change
 ------------------------------------------------------------------------------}
procedure TCanvas.RegionChanged(ARegion: TObject);
begin
  if csRegionValid in FState
  then begin
    Exclude(FState, csRegionValid);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCanvas.Create
  Params:  none
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TCanvas.Create;
begin
  FHandle := 0;
  {$IFDEF UseFPCanvas}
  ManageResources := true;
  {$ENDIF}
  inherited Create;
  {$IFDEF UseFPCanvas}
  FFont := TFont(inherited Font);
  FPen := TPen(inherited Pen);
  FBrush := TBrush(inherited Brush);
  {$ELSE}
  FFont := TFont.Create;
  FPen := TPen.Create;
  FBrush := TBrush.Create;
  FPenPos := Point(0, 0);
  FLockCount := 0;
  {$ENDIF}
  FFont.OnChange := @FontChanged;
  FSavedFontHandle := 0;
  FPen.OnChanging := @PenChanging;
  FPen.OnChange := @PenChanged;
  FSavedPenHandle := 0;
  FBrush.OnChange := @BrushChanged;
  FSavedBrushHandle := 0;
  FRegion := TRegion.Create;
  FRegion.OnChange := @RegionChanged;
  FSavedRegionHandle := 0;
  FCopyMode := cmSrcCopy;
  // FLock will be initialized on demand, because most canvas don't use it
  With FTextStyle do begin
    Alignment := taLeftJustify;
    Layout := tlTop;
    WordBreak := True;
    SingleLine := False;
    Clipping := True;
    ShowPrefix := True;
    Opaque := False;
  end;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Chord
  Params:   x,y, AWidth, AHeight, StartAngle16Deg, EndAngle16Deg
  Returns:  Nothing

  Use Chord to draw a filled Chord-shape on the canvas. The angles angle1 and
  angle2 are 1/16th of a degree. For example, a full circle equals 5760(16*360).
  Positive values of Angle and AngleLength mean counter-clockwise while negative
  values mean clockwise direction. Zero degrees is at the 3'o clock position.

------------------------------------------------------------------------------}
procedure TCanvas.Chord(x, y, AWidth, AHeight,
  StartAngle16Deg, EndAngle16Deg: Integer);
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  LCLIntf.AngleChord(FHandle,x,y,AWidth,AHeight,StartAngle16Deg,EndAngle16Deg);
  Changed;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Chord
  Params:   x,y,AWidth, AHeight, sx,sy,ex,ey
  Returns:  Nothing

  Use Chord to draw a filled Chord-shape on the canvas. The values sx,sy,
  and ex,ey represent a starting and ending radial-points between which
  the Arc is draw.

------------------------------------------------------------------------------}
procedure TCanvas.Chord(x,y,AWidth, AHeight, sx,sy,ex,ey : Integer);
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  LCLIntf.RadialChord(FHandle,x,y,AWidth,AHeight,sx,sy,ex,ey);
  Changed;
end;

{------------------------------------------------------------------------------
  Method: TCanvas.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TCanvas.Destroy;
begin
//DebugLn('[TCanvas.Destroy] ',ClassName,'  Self=',DbgS(Self));
  Handle := 0;
  {$IFNDEF UseFPCanvas}
  FreeThenNil(FFont);
  FreeThenNil(FPen);
  FreeThenNil(FBrush);
  {$ENDIF}
  FreeThenNil(FRegion);
  if FLock <> 0 then
    DeleteCriticalSection(FLock);
  inherited Destroy;
  {$IFDEF UseFPCanvas}
  // set resources to nil, so that dangling pointers are spotted early
  FFont:=nil;
  FPen:=nil;
  FBrush:=nil;
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Function: TCanvas.GetHandle
  Params:   None
  Returns:  A handle to the GUI object

  Checks if a handle is allocated, otherwise create it
 ------------------------------------------------------------------------------}
function TCanvas.GetHandle : HDC;
begin
  //DebugLn('[TCanvas.GetHandle] ',ClassName);
  RequiredState(csAllValid);
  Result := FHandle;
end;

{------------------------------------------------------------------------------
  Method:  TCanvas.SetHandle
  Params:  NewHandle - the new device context
  Returns: nothing

  Deselect sub handles and sets the Handle
 ------------------------------------------------------------------------------}
procedure TCanvas.SetHandle(NewHandle: HDC);
begin
  if FHandle<>NewHandle then begin
    //DebugLn('[TCanvas.SetHandle] Self=',DbgS(Self),' Old=',DbgS(FHandle,8),' New=',DbgS(NewHandle,8));
    if FHandle <> 0 then
    begin
      DeselectHandles;
      FHandle := 0;
      Exclude(FState, csHandleValid);
    end;
    if NewHandle <> 0 then
    begin
      Include(FState, csHandleValid);
      FHandle := NewHandle;
    end;
    //DebugLn('[TCanvas.SetHandle] END Self=',DbgS(Self),' Handle=',DbgS(FHandle,8));
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCanvas.DeselectHandles
  Params:  none
  Returns: nothing

  Deselect all subhandles in the current device context
 ------------------------------------------------------------------------------}
procedure TCanvas.DeselectHandles;
begin
  //debugln('TCanvas.DeselectHandles ',ClassName,' Self=',DbgS(Self),' Handle=',DbgS(FHandle),8),' FSavedBrushHandle=',DbgS(Cardinal(FSavedBrushHandle));
  if (FHandle<>0) then begin
    // select default sub handles in the device context without deleting owns
    if FSavedBrushHandle<>0 then begin
      SelectObject(FHandle,FSavedBrushHandle);
      FSavedBrushHandle:=0;
    end;
    if FSavedPenHandle<>0 then begin
      SelectObject(FHandle,FSavedPenHandle);
      FSavedPenHandle:=0;
    end;
    if FSavedFontHandle<>0 then begin
      SelectObject(FHandle,FSavedFontHandle);
      FSavedFontHandle:=0;
    end;
    FState := FState - [csPenValid, csBrushValid, csFontValid];
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCanvas.CreateHandle
  Params:  None
  Returns: Nothing

  Creates the handle ( = object).
 ------------------------------------------------------------------------------}
procedure TCanvas.CreateHandle;
begin
// Plain canvas does nothing
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.RequiredState
  Params:   ReqState: The required state
  Returns:  Nothing

  Ensures that all handles needed are valid;
 ------------------------------------------------------------------------------}
procedure TCanvas.RequiredState(ReqState: TCanvasState);
var
  Needed: TCanvasState;
begin
  Needed := ReqState - FState;
  //DebugLn('[TCanvas.RequiredState] ',ClassName,' ',csHandleValid in ReqState,' ',csHandleValid in FState,' Needed=',Needed<>[]);
  if Needed <> [] then
  begin
    //DebugLn('[TCanvas.RequiredState] B ',ClassName,' ',csHandleValid in Needed,',',csFontValid in Needed,',',csPenValid in Needed,',',csBrushValid in Needed);
    if csHandleValid in Needed then
    begin
      CreateHandle;
      if FHandle = 0 then
        raise EInvalidOperation.Create(rsCanvasDoesNotAllowDrawing);
      Include(FState, csHandleValid);
    end;
    if csFontValid in Needed then CreateFont;
    if csPenValid in Needed then
    begin
      CreatePen;
      if Pen.Style in [psDash, psDot, psDashDot, psDashDotDot]
      then Include(Needed, csBrushValid);
    end;
    if csBrushValid in Needed then CreateBrush;
  end;
end;

procedure TCanvas.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TCanvas.Changing;
begin
  if Assigned(FOnChanging) then FOnChanging(Self);
end;

{------------------------------------------------------------------------------
  Function: TCanvas.TextExtent
  Params:   Text: The text to measure
  Returns:  The size

  Gets the width and height of a text
 ------------------------------------------------------------------------------}
function TCanvas.TextExtent(const Text: string): TSize;
begin
  Result.cX := 0;
  Result.cY := 0;
  if Text='' then exit;
  RequiredState([csHandleValid, csFontValid]);
  GetTextExtentPoint(FHandle, PChar(Text), Length(Text), Result);
end;

{------------------------------------------------------------------------------
  Function: TCanvas.TextWidth
  Params:   Text: The text to measure
  Returns:  The width

  Gets the width of a text
 ------------------------------------------------------------------------------}
function TCanvas.TextWidth(const Text: string): Integer;
begin
  Result := TextExtent(Text).cX;
end;

{------------------------------------------------------------------------------
  Function: TCanvas.TextHeight
  Params:   Text: The text to measure
  Returns:  A handle to the GUI object

  Gets the height of a text
 ------------------------------------------------------------------------------}
function TCanvas.TextHeight(const Text: string): Integer;
begin
  Result := TextExtent(Text).cY;
end;

{------------------------------------------------------------------------------
  Function: TCanvas.Lock
  Params:   none
  Returns:  nothing
 ------------------------------------------------------------------------------}
procedure TCanvas.Lock;
begin
  {$IFDEF UseFPCanvas}
  LockCanvas;
  {$ELSE}
  if FLockCount=0 then begin
    if FLock=0 then InitializeCriticalSection(FLock);
    EnterCriticalSection(FLock);
  end;
  Inc(FLockCount);
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Function: TCanvas.Unlock
  Params:   none
  Returns:  nothing
 ------------------------------------------------------------------------------}
procedure TCanvas.Unlock;

  procedure RaiseTooManyUnlock;
  begin
    raise Exception.Create(
                         'TCanvas.Unlock '+DbgSName(Self)+': too many unlocks');
  end;

begin
  {$IFDEF UseFPCanvas}
  UnlockCanvas;
  {$ELSE}
  Dec(FLockCount);
  if FLockCount=0 then begin
    LeaveCriticalSection(FLock);
  end else if FLockCount<0 then
    RaiseTooManyUnlock;
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  procedure TCanvas.Refresh;
 ------------------------------------------------------------------------------}
procedure TCanvas.Refresh;
begin
  DeselectHandles;
end;

{ =============================================================================

  $Log$
  Revision 1.92  2005/03/07 21:59:44  vincents
  changed hexstr(cardinal()) for pointers to dbgs() and other 64-bits fixes   from Peter Vreman

  Revision 1.91  2005/03/04 13:50:08  mattias
  fixed Arc and changed x,y to Left,Top to make meaning more clear

  Revision 1.90  2005/02/04 01:04:40  mattias
  fixed gtk intf Arc

  Revision 1.89  2005/01/15 11:56:13  mattias
  fixed locking TCanvas

  Revision 1.88  2005/01/10 18:44:44  mattias
  implemented the fpCanvas support for the LCL - Compile with -dUseFPCanvas

  Revision 1.87  2005/01/08 15:06:06  mattias
  fixed TabOrder dialog for new TabOrder

  Revision 1.86  2005/01/08 11:03:18  mattias
  implemented TPen.Mode=pmXor  from Jesus

  Revision 1.85  2005/01/01 20:05:08  mattias
  accelerated code completion box in IDE and replaced ExtTextOut with ExtUTF8Out in TCanvas.TextOut

  Revision 1.84  2004/11/07 17:21:08  micha
  fix text position to be at X, Y; if alignment/layout indicates so

  Revision 1.83  2004/10/01 13:16:44  mattias
  fixed unselecting TCanvas objects

  Revision 1.82  2004/09/29 15:18:27  mattias
  fixed TBitmap.Canvas.Frame3d

  Revision 1.81  2004/09/24 13:45:32  mattias
  fixed TCanvas.TextRect Delphi compatible Rect and added TBarChart from Michael VC

  Revision 1.80  2004/09/20 23:13:46  mattias
  moved remaining TCanvas handle dependent methods to protected

  Revision 1.79  2004/09/14 18:02:44  mattias
  made TCanvas methods virtual

  Revision 1.78  2004/09/14 10:06:26  micha
  convert LM_REDRAW message to interface method (in twidgetset)

  Revision 1.77  2004/09/13 15:33:07  micha
  rename AutoReDraw to AutoRedraw, as "Re" is not a word

  Revision 1.76  2004/09/12 13:11:50  micha
  convert LM_GETPIXEL and LM_SETPIXEL to interface methods (of twidgetset, DCGetPixel and DCSetPixel)

  Revision 1.75  2004/05/11 12:16:47  mattias
  replaced writeln by debugln

  Revision 1.74  2004/04/10 17:58:56  mattias
  implemented mainunit hints for include files

  Revision 1.73  2004/03/28 12:49:22  mattias
  implemented mask merge and extraction for raw images

  Revision 1.72  2004/03/06 17:12:19  mattias
  fixed CreateBrushIndirect

  Revision 1.71  2004/03/06 15:37:43  mattias
  fixed FreeDC

  Revision 1.70  2004/03/03 23:35:55  mattias
  accelerated TActionList editor  from Radek

  Revision 1.69  2004/02/28 00:34:35  mattias
  fixed CreateComponent for buttons, implemented basic Drag And Drop

  Revision 1.68  2004/02/23 08:19:04  micha
  revert intf split

  Revision 1.66  2004/02/21 15:17:43  micha
  fixed: compiler doesn't know what function to call using ver 1.9.2

  Revision 1.65  2004/02/10 00:05:03  mattias
  TSpeedButton now uses MaskBlt

  Revision 1.64  2004/02/03 08:54:09  mattias
  Frame3D rect now var again

  Revision 1.63  2004/02/02 15:46:19  mattias
  implemented basic TSplitter, still many ToDos

  Revision 1.62  2004/01/31 15:29:55  mattias
  added icons for synhighlighter

  Revision 1.61  2004/01/05 01:18:15  mattias
  implemented Double Buffering for synedit and deactivated multi buffering in TGTKObject.ExtTextOut

  Revision 1.60  2004/01/03 23:14:59  mattias
  default font can now change height and fixed gtk crash

  Revision 1.59  2003/12/30 22:24:47  micha
  fix number of points in polygon (form vincent)

  Revision 1.58  2003/12/26 10:16:54  mattias
  changed TColorRef from longword to longint

  Revision 1.57  2003/12/23 11:16:41  mattias
  started key combinations, fixed some range check errors

  Revision 1.56  2003/12/02 12:25:17  micha
  try: gdi memory leak fix for pen

  Revision 1.55  2003/11/22 17:22:15  mattias
  moved TBevelCut to controls.pp

  Revision 1.54  2003/11/03 16:57:47  peter
    * change $ifdef ver1_1 to $ifndef ver1_0 so it works also with
      fpc 1.9.x

  Revision 1.53  2003/09/18 09:21:03  mattias
  renamed LCLLinux to LCLIntf

  Revision 1.52  2003/08/27 08:14:37  mattias
  fixed system fonts for win32 intf

  Revision 1.51  2003/08/18 19:24:18  mattias
  fixed TCanvas.Pie

  Revision 1.50  2003/07/04 08:54:53  mattias
  implemented 16bit rawimages for gtk

  Revision 1.49  2003/06/30 10:09:46  mattias
  fixed Get/SetPixel for DC without widget

  Revision 1.48  2003/06/25 10:38:28  mattias
  implemented saving original stream of TBitmap

  Revision 1.47  2002/08/18 16:50:09  mattias
  fixes for debugging

  Revision 1.46  2002/08/18 04:57:01  mattias
  fixed csDashDot

  Revision 1.45  2003/06/13 21:08:53  mattias
  moved TColorButton to dialogs.pp

  Revision 1.44  2003/06/13 10:37:20  mattias
  fixed AV on StretchDraw 0x0

  Revision 1.43  2003/04/02 13:23:23  mattias
  fixed default font

  Revision 1.42  2003/03/12 14:39:29  mattias
  fixed clipping origin in stretchblt

  Revision 1.41  2003/03/11 07:46:43  mattias
  more localization for gtk- and win32-interface and lcl

  Revision 1.40  2003/02/26 12:44:52  mattias
  readonly flag is now only saved if user set

  Revision 1.39  2003/02/06 06:39:02  mattias
  implemented TCanvas.Refresh

  Revision 1.38  2003/01/28 17:04:34  mattias
  renamed one Rect

  Revision 1.37  2003/01/27 13:49:16  mattias
  reduced speedbutton invalidates, added TCanvas.Frame

  Revision 1.36  2002/12/01 22:00:34  mattias
  fixed DeleteCriticalSection

  Revision 1.35  2002/11/29 15:14:47  mattias
  replaced many invalidates by invalidaterect

  Revision 1.34  2002/10/31 17:31:10  lazarus
  MG: fixed return polygon point

  Revision 1.33  2002/10/27 11:51:35  lazarus
  MG: fixed memleaks

  Revision 1.32  2002/10/25 10:42:08  lazarus
  MG: broke minor circles

  Revision 1.31  2002/10/14 14:29:50  lazarus
  AJ: Improvements to TUpDown; Added TStaticText & GNOME DrawText

  Revision 1.30  2002/10/04 14:24:14  lazarus
  MG: added DrawItem to TComboBox/TListBox

  Revision 1.29  2002/09/27 20:52:22  lazarus
  MWE: Applied patch from "Andrew Johnson" <aj_genius@hotmail.com>

  Here is the run down of what it includes -

   -Vasily Volchenko's Updated Russian Localizations

   -improvements to GTK Styles/SysColors
   -initial GTK Palette code - (untested, and for now useless)

   -Hint Windows and Modal dialogs now try to stay transient to
    the main program form, aka they stay on top of the main form
    and usually minimize/maximize with it.

   -fixes to Form BorderStyle code(tool windows needed a border)

   -fixes DrawFrameControl DFCS_BUTTONPUSH to match Win32 better
    when flat

   -fixes DrawFrameControl DFCS_BUTTONCHECK to match Win32 better
    and to match GTK theme better. It works most of the time now,
    but some themes, noteably Default, don't work.

   -fixes bug in Bitmap code which broke compiling in NoGDKPixbuf
    mode.

   -misc other cleanups/ fixes in gtk interface

   -speedbutton's should now draw correctly when flat in Win32

   -I have included an experimental new CheckBox(disabled by
    default) which has initial support for cbGrayed(Tri-State),
    and WordWrap, and misc other improvements. It is not done, it
    is mostly a quick hack to test DrawFrameControl
    DFCS_BUTTONCHECK, however it offers many improvements which
    can be seen in cbsCheck/cbsCrissCross (aka non-themed) state.

   -fixes Message Dialogs to more accurately determine
    button Spacing/Size, and Label Spacing/Size based on current
    System font.
   -fixes MessageDlgPos, & ShowMessagePos in Dialogs
   -adds InputQuery & InputBox to Dialogs

   -re-arranges & somewhat re-designs Control Tabbing, it now
    partially works - wrapping around doesn't work, and
    subcontrols(Panels & Children, etc) don't work. TabOrder now
    works to an extent. I am not sure what is wrong with my code,
    based on my other tests at least wrapping and TabOrder SHOULD
    work properly, but.. Anyone want to try and fix?

   -SynEdit(Code Editor) now changes mouse cursor to match
    position(aka over scrollbar/gutter vs over text edit)

   -adds a TRegion property to Graphics.pp, and Canvas. Once I
    figure out how to handle complex regions(aka polygons) data
    properly I will add Region functions to the canvas itself
    (SetClipRect, intersectClipRect etc.)

   -BitBtn now has a Stored flag on Glyph so it doesn't store to
    lfm/lrs if Glyph is Empty, or if Glyph is not bkCustom(aka
    bkOk, bkCancel, etc.) This should fix most crashes with older
    GDKPixbuf libs.

  Revision 1.28  2002/09/19 19:56:14  lazarus
  MG: accelerated designer drawings

  Revision 1.27  2002/09/18 17:07:24  lazarus
  MG: added patch from Andrew

  Revision 1.26  2002/09/12 15:35:57  lazarus
  MG: small bugfixes

  Revision 1.25  2002/09/03 08:07:19  lazarus
  MG: image support, TScrollBox, and many other things from Andrew

  Revision 1.24  2002/08/30 13:43:37  lazarus
  MG: fixed drawing of non visual components in designer

  Revision 1.23  2002/08/19 20:34:47  lazarus
  MG: improved Clipping, TextOut, Polygon functions

  Revision 1.22  2002/08/15 15:46:48  lazarus
  MG: added changes from Andrew (Clipping)

  Revision 1.21  2002/08/13 07:08:24  lazarus
  MG: added gdkpixbuf.pp and changes from Andrew Johnson

  Revision 1.20  2002/08/08 18:05:46  lazarus
  MG: added graphics extensions from Andrew Johnson

  Revision 1.19  2002/06/04 15:17:22  lazarus
  MG: improved TFont for XLFD font names

  Revision 1.18  2002/05/10 06:05:51  lazarus
  MG: changed license to LGPL

  Revision 1.17  2002/03/14 23:25:51  lazarus
  MG: fixed TBevel.Create and TListView.Destroy

  Revision 1.16  2002/03/08 16:16:55  lazarus
  MG: fixed parser of end blocks in initialization section added label sections

  Revision 1.15  2002/02/03 00:24:01  lazarus
  TPanel implemented.
  Basic graphic primitives split into GraphType package, so that we can
  reference it from interface (GTK, Win32) units.
  New Frame3d canvas method that uses native (themed) drawing (GTK only).
  New overloaded Canvas.TextRect method.
  LCLIntf and Graphics was split, so a bunch of files had to be modified.

  Revision 1.14  2002/01/02 15:24:58  lazarus
  MG: added TCanvas.Polygon and TCanvas.Polyline

  Revision 1.13  2001/12/28 11:41:51  lazarus
  MG: added TCanvas.Ellipse, TCanvas.Pie

  Revision 1.12  2001/12/27 16:31:28  lazarus
  MG: implemented TCanvas.Arc

  Revision 1.11  2001/11/09 19:14:23  lazarus
  HintWindow changes
  Shane

  Revision 1.10  2001/10/07 07:28:33  lazarus
  MG: fixed setpixel and TCustomForm.OnResize event

  Revision 1.9  2001/09/30 08:34:49  lazarus
  MG: fixed mem leaks and fixed range check errors

  Revision 1.8  2001/03/24 18:05:58  lazarus
  MG: canvas size enlarged

  Revision 1.4  2001/03/19 14:00:50  lazarus
  MG: fixed many unreleased DC and GDIObj bugs

  Revision 1.3  2001/02/04 18:24:41  lazarus
  Code cleanup
  Shane

  Revision 1.2  2000/08/10 18:56:24  lazarus
  Added some winapi calls.
  Most don't have code yet.
  SetTextCharacterExtra
  CharLowerBuff
  IsCharAlphaNumeric
  Shane

  Revision 1.1  2000/07/13 10:28:24  michael
  + Initial import

  Revision 1.6  2000/07/09 20:18:56  lazarus
  MWE:
    + added new controlselection
    + some fixes
    ~ some cleanup

  Revision 1.5  2000/05/10 22:52:57  lazarus
  MWE:
    = Moved some global api stuf to gtkobject

  Revision 1.4  2000/05/09 02:07:40  lazarus
  Replaced writelns with Asserts.                          CAW

  Revision 1.3  2000/05/08 15:56:58  lazarus
  MWE:
    + Added support for mwedit92 in Makefiles
    * Fixed bug # and #5 (Fillrect)
    * Fixed labelsize in ApiWizz
    + Added a call to the resize event in WMWindowPosChanged

  Revision 1.2  2000/05/08 12:54:19  lazarus
  Removed some writeln's
  Added alignment for the TLabel.  Isn't working quite right.
  Added the shell code for WindowFromPoint and GetParent.
  Added FindLCLWindow
  Shane

  Revision 1.1  2000/04/02 20:49:55  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.27  2000/03/30 18:07:53  lazarus
  Added some drag and drop code
  Added code to change the unit name when it's saved as a different name.  Not perfect yet because if you are in a comment it fails.

  Shane

  Revision 1.26  2000/03/21 18:53:28  lazarus
  Added code for TBitBtn. Not finished but looks like mostly working.
  Shane

  Revision 1.25  2000/03/06 00:05:05  lazarus
  MWE: Added changes from Peter Dyson <peter@skel.demon.co.uk> for a new
    release of mwEdit (0.92)

  Revision 1.24  2000/01/26 19:16:24  lazarus
  Implemented TPen.Style properly for GTK. Done SelectObject for pen objects.
  Misc bug fixes.
  Corrected GDK declaration for gdk_gc_set_slashes.

  Revision 1.23  2000/01/18 21:47:00  lazarus
  Added OffSetRec

  Revision 1.22  1999/12/21 00:07:06  lazarus
  MWE:
    Some fixes
    Completed a bit of DraWEdge

  Revision 1.21  1999/12/07 01:19:25  lazarus
  MWE:
    Removed some double events
    Changed location of SetCallBack
    Added call to remove signals
    Restructured somethings
    Started to add default handlers in TWinControl
    Made some parts of TControl and TWinControl more delphi compatible
    ... and lots more ...

  Revision 1.20  1999/12/06 16:56:30  lazarus
  Modifications made to help me debug the error during SETTEXT.
  Shane

  Revision 1.19  1999/12/02 19:00:59  lazarus
  MWE:
    Added (GDI)Pen
    Changed (GDI)Brush
    Changed (GDI)Font (color)
    Changed Canvas to use/create pen/brush/font
    Hacked mwedit to allow setting the number of chars (till it get a WM/LM_SIZE event)
    The editor shows a line !


}
