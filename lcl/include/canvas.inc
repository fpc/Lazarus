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
Procedure TCanvas.BrushCopy(Dest : TRect; InternalImages: TBitmap; Src : TRect;
  TransparentColor :TColor);
Begin
//TODO:TCANVAS.BRUSHCOPY
end;

{-----------------------------------------------}
{--  TCanvas.Draw --}
{-----------------------------------------------}
Procedure TCanvas.Draw(X,Y : Integer; Graphic : TGraphic);
begin

end;

{-----------------------------------------------}
{--  TCanvas.GetCanvasClipRect --}
{-----------------------------------------------}
function TCanvas.GetCanvasClipRect: TRect;
begin
  If GetClipBox(FHandle, @Result) = ERROR then
    Result := Rect(0,0,2000,2000);{Just in Case}
end;


{-----------------------------------------------}
{--  TCanvas.CopyRect --}
{-----------------------------------------------}
Procedure TCanvas.CopyRect(const Dest : TRect; Canvas : TCanvas; const Source : TRect);
var
  SH, SW, DH, DW: Integer;
Begin
//this SHOULD stretch the image to the new canvas, but it doesn't yet.....
  Assert(False, Format('Trace:==> [TCanvas.CopyRect] ', []));
  if Canvas <> nil
  then begin
    Canvas.RequiredState([csHandleValid, csBrushValid]);
    RequiredState([csHandleValid, csFontValid, csBrushValid]);

    SH := Source.Bottom - Source.Top;
    SW := Source.Right - Source.Left;
    DH := Dest.Bottom - Dest.Top;
    DW := Dest.Right - Dest.Left;
    StretchBlt(FHandle, Dest.Left, Dest.Top, DW, DH,
      Canvas.FHandle, Source.Left, Source.Top, SW, SH, CopyMode);
  end;

  Assert(False, Format('Trace:<== [TCanvas.CopyRect] ', []));
end;
{-----------------------------------------------}
{--  TCanvas.GetPixel --}
{-----------------------------------------------}
Function TCanvas.GetPixel(X,Y : Integer) : TColor;
var
Msg : TLMSetGetPixel;
{TLMSetGetPixel = record
  X,Y : Integer;
  PixColor : TColor;
end;
}
Begin
  msg.X := x;
  msg.Y := Y;
  CNSendMessage(LM_GetPixel, Self, @msg);
  Result := msg.PixColor;
end;

{-----------------------------------------------}
{--  TCanvas.SetPixel --}
{-----------------------------------------------}
Procedure TCanvas.SetPixel(X,Y: Integer; Value : TColor);
var
  Msg : TLMSetGetPixel;
Begin
  Msg.X := X;
  msg.Y := Y;
  MSg.PixColor := ColorToRGB(Value);
  CNSendMessage(LM_SetPixel, Self, @msg);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.CreateBrush
  Params:   None
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.CreateBrush;
var OldHandle: HBRUSH;
begin
//writeln('[TCanvas.CreateBrush] ',Classname,'  Self=',HexStr(Cardinal(Pointer(Self)),8)
// ,'  Brush=',HexStr(Cardinal(Pointer(Brush)),8));
  OldHandle:=SelectObject(FHandle, Brush.Handle);
  if (OldHandle<>Brush.Handle) and (FSavedBrushHandle=0) then
    FSavedBrushHandle:=OldHandle;
  Include(FState, csBrushValid);
  SetBkColor(FHandle, ColorToRGB(Brush.Color));
  SetBkMode(FHandle, TRANSPARENT);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.CreatePen
  Params:   None
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.CreatePen;
var OldHandle: HPEN;
begin
//writeln('[TCanvas.CreatePen] ',Classname,'  Self=',HexStr(Cardinal(Pointer(Self)),8)
// ,'  Pen=',HexStr(Cardinal(Pointer(Pen)),8));
  OldHandle:=SelectObject(FHandle, Pen.Handle);
  if (OldHandle<>Pen.Handle) and (FSavedPenHandle=0) then
    FSavedPenHandle:=OldHandle;
  Include(FState, csPenValid);
//  SetROP2(FHandle, PenModes[Pen.Mode]);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.CreateFont
  Params:   None
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.CreateFont;
var OldHandle: HPEN;
begin
  OldHandle:=SelectObject(FHandle, Font.Handle);
  if (OldHandle<>Font.Handle) and (FSavedFontHandle=0) then
    FSavedFontHandle:=OldHandle;
  Include(FState, csFontValid);
  SetTextColor(FHandle, ColorToRGB(Font.Color));
end;

{------------------------------------------------------------------------------
  Function: TCanvas.GetPenPos
  Params:   None
  Returns:  PenPos

 ------------------------------------------------------------------------------}
function TCanvas.GetPenPos: TPoint;
begin
  Result := FPenPos;
end;


{------------------------------------------------------------------------------
  Method:   TCanvas.SetAutoReDraw
  Params:   Value
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.SetAutoReDraw(Value : Boolean);
begin
  FAutoRedraw := Value;
  If FAutoReDraw then
    CNSendMessage(LM_ReDraw, Self, nil);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.SetPenPos
  Params:   Value
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.SetPenPos(Value : TPoint);
begin
  MoveTo(Value.X, Value.Y);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.SetBrush
  Params:   Value
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.SetBrush(Value : TBrush);
begin
  FBrush.Assign(Value);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.SetFont
  Params:   Value
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.SetFont(Value : TFont);
begin
  FFont.Assign(Value);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.SetPen
  Params:   Value
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.SetPen(Value : TPen);
begin
  FPen.Assign(Value);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Arc
  Params:   x,y,width,height,angle1,angle2
  Returns:  Nothing

  Use Arc to draw an elliptically curved line with the current Pen.
  The angles angle1 and angle2 are 1/16th of a degree. For example, a full
  circle equals 5760 (16*360). Positive values of Angle and AngleLength mean
  counter-clockwise while negative values mean clockwise direction.
  Zero degrees is at the 3'o clock position. 

 ------------------------------------------------------------------------------}
procedure TCanvas.Arc(x,y,width,height,angle1,angle2 : Integer);
begin
  RequiredState([csHandleValid, csPenValid]);
  LCLLinux.Arc(FHandle,x,y,width,height,angle1,angle2);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Arc
  Params:   DC,x,y,width,height,sx,sy,ex,ey 
  Returns:  Nothing

  Use Arc to draw an elliptically curved line with the current Pen. The 
  values sx,sy, and ex,ey represent the starting and ending radial-points 
  between which the Arc is drawn.

------------------------------------------------------------------------------}
procedure TCanvas.Arc(x,y,width,height,sx,sy,ex,ey : Integer);
begin
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  LCLLinux.RadialArc(FHandle,x,y,width,height,sx,sy,ex,ey);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Pie
  Params:   x,y,width,height,angle1,angle2
  Returns:  Nothing

  Use Pie to draw a filled pie-shaped wedge on the canvas.
  The angles angle1 and angle2 are 1/16th of a degree. For example, a full
  circle equals 5760 (16*360). Positive values of Angle and AngleLength mean
  counter-clockwise while negative values mean clockwise direction.
  Zero degrees is at the 3'o clock position. 

 ------------------------------------------------------------------------------}
procedure TCanvas.Pie(x,y,width,height,angle1,angle2 : Integer);
begin
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  LCLLinux.Pie(FHandle,x,y,width,height,angle1,angle2);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Pie
  Params:   x,y,width,height,sx,sy,ex,ey
  Returns:  Nothing

  Use Pie to draw a filled Pie-shaped wedge on the canvas. The values sx,sy,
  and ex,ey represent the starting and ending radial-points between which
  the Bounding-Arc is drawn.

------------------------------------------------------------------------------}
procedure TCanvas.Pie(x,y,width,height,sx,sy,ex,ey : Integer);
begin
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  LCLLinux.RadialPie(FHandle,x,y,width,height,sx,sy,ex,ey);
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
  Filled: boolean{$IFDEF VER1_1} = False{$ENDIF};
  Continuous: boolean{$IFDEF VER1_1} = False{$ENDIF});
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
  Filled: boolean{$IFDEF VER1_1} = False{$ENDIF};
  Continuous: boolean{$IFDEF VER1_1} = False{$ENDIF});
begin
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  LCLLinux.PolyBezier(FHandle,Points,NumPts,Filled, Continuous);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.PolyBezier
  Params:   Points
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.PolyBezier(const Points: array of TPoint);
begin
  PolyBezier(Points, False, True);
end;

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
var NPoints, i: integer;
  PointArray: ^TPoint;
begin
  if NumPts<0 then
    NPoints:=High(Points)-StartIndex+1
  else
    NPoints:=NumPts;
  if NPoints<=0 then exit;
  GetMem(PointArray,SizeOf(TPoint)*NPoints);
  for i:=0 to NPoints-1 do
    PointArray[i]:=Points[i+StartIndex];
  Polygon(PointArray,NPoints,Winding);
  FreeMem(PointArray);
end;

procedure TCanvas.Polygon(Points: PPoint; NumPts: Integer;
  Winding: boolean{$IFDEF VER1_1} = False{$ENDIF});
begin
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  LCLLinux.Polygon(FHandle,Points,NumPts,Winding);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Polygon
  Params:   Points
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.Polygon(const Points: array of TPoint);
begin
  Polygon(Points, True, Low(Points), High(Points) - Low(Points));
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
var NPoints, i: integer;
  PointArray: ^TPoint;
begin
  if NumPts<0 then
    NPoints:=High(Points)-StartIndex+1
  else
    NPoints:=NumPts;
  if NPoints<=0 then exit;
  GetMem(PointArray,SizeOf(TPoint)*NPoints);
  for i:=0 to NPoints-1 do
    PointArray[i]:=Points[i+StartIndex];
  Polyline(PointArray,NPoints);
  FreeMem(PointArray);
end;

procedure TCanvas.Polyline(Points: PPoint; NumPts: Integer);
begin
  RequiredState([csHandleValid, csPenValid]);
  LCLLinux.Polyline(FHandle,Points,NumPts);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Polyline
  Params:   Points
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.Polyline(const Points: array of TPoint);
begin
  Polyline(Points, Low(Points), High(Points) - Low(Points));
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Ellipse
  Params:   X1, Y1, X2, Y2
  Returns:  Nothing

  Use Ellipse to draw a filled circle or ellipse on the canvas.

 ------------------------------------------------------------------------------}
procedure TCanvas.Ellipse(x1, y1, x2, y2: Integer);
begin
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  LCLLinux.Ellipse(FHandle,x1,y1,x2,y2);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Ellipse
  Params:   X1, Y1, X2, Y2
  Returns:  Nothing

  Use Ellipse to draw a filled circle or ellipse on the canvas.

 ------------------------------------------------------------------------------}
procedure TCanvas.Ellipse(const Rect: TRect);
begin
  Ellipse(Rect.Left,Rect.Top,Rect.Right,Rect.Bottom);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.FillRect
  Params:   Rect
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.FillRect(const Rect : TRect);
begin
  RequiredState([csHandleValid, csBrushValid]);
  LCLLinux.FillRect(FHandle, Rect, Brush.Handle);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.FillRect
  Params:   X, Y: Integer; Color: TColor; FillStyle: TFillStyle
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.FloodFill(X, Y: Integer; FillColor: TColor; FillStyle: TFillStyle
  );
begin
  RequiredState([csHandleValid, csBrushValid]);
  LCLLinux.FloodFill(FHandle, X, Y, FillColor, FillStyle, Brush.Handle);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Frame3d
  Params:   Rect
, AWidth, AHeight
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.Frame3d(var Rect : TRect; const FrameWidth : integer; const Style : TBevelCut);
begin
  RequiredState([csHandleValid]);
  LCLLinux.Frame3d(FHandle, Rect, FrameWidth, Style);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Rectangle
  Params:   X1,Y1,X2,Y2
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.Rectangle(X1,Y1,X2,Y2 : Integer);
begin
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  LCLLinux.Rectangle(FHandle, X1, Y1, X2, Y2);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Rectangle
  Params:   Rect
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.Rectangle(const Rect: TRect);
begin
 Rectangle(Rect.Left, REct.Top, Rect.RIght, REct.Bottom);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.TextRect
  Params:   Rect, X, Y, Text
  Returns:  Nothing

 ------------------------------------------------------------------------------}
Procedure TCanvas.TextRect(Rect: TRect; X,Y : Integer; const Text : String);
begin
  TextRect(Rect,X,Y,Text,TextStyle);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.TextRect
  Params:   Rect, X, Y, Text, Style
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.TextRect(Rect: TRect; X, Y : Integer; const Text : String; const Style : TTextStyle);
var
  Options : Longint;
  fRect : TRect;
begin
  Rect.Left := Rect.Left + X;
  Rect.Top := Rect.Top + Y;
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

  fRect := Rect;
  RequiredState([csHandleValid, csFontValid]);
  DrawText(Self.Handle, pChar(Text), Length(Text), fRect, DT_CALCRECT or Options);
  case Style.Alignment of
    taRightJustify : OffsetRect(fRect, Rect.Right - fRect.Right, 0);
    taCenter : OffsetRect(fRect, (Rect.Right - fRect.Right) div 2, 0);
  end;
  case Style.Layout of
    tlCenter : OffsetRect(fRect, 0, (Rect.Bottom - Rect.Top) div 2 - (fRect.Bottom - fRect.Top) div 2);
    tlBottom : OffsetRect(fRect, 0, Rect.Bottom - fRect.Bottom);
  end;
  If Style.Opaque then begin
    RequiredState([csHandleValid, csBrushValid]);
    FillRect(fRect);
  end;
  RequiredState([csHandleValid, csFontValid]);
  DrawText(Self.Handle, pChar(Text), Length(Text), fRect, Options)
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
  RequiredState([csHandleValid, csFontValid, csBrushValid]);
  Flags := 0;
  If TextStyle.Opaque then
    Flags := ETO_Opaque;
  ExtTextOut(FHandle, X, Y, Flags, nil,
             PChar(Text), Length(Text), nil);
  MoveTo(X + TextWidth(Text), Y);
end;

function TCanvas.HandleAllocated: boolean;
begin
  Result:=(FHandle<>0);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.MoveTo
  Params:   X1,Y1
  Returns:  Nothing

 ------------------------------------------------------------------------------}
Procedure TCanvas.MoveTo(X1, Y1 : Integer);
begin
  RequiredState([csHandleValid]);
  if LCLLinux.MoveToEx(FHandle, X1, Y1, nil) then FPenPos:= Point(X1, Y1);
End;

{------------------------------------------------------------------------------
  Method:   TCanvas.LineTo
  Params:   X1,Y1
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.LineTo(X1, Y1 : Integer);
begin
  RequiredState([csHandleValid, csPenValid]);
  if LCLLinux.LineTo(FHandle, X1, Y1) then FPenPos:= Point(X1, Y1);
end;


{------------------------------------------------------------------------------
  Method:   TCanvas.Line
  Params:   X1,Y1,X2,Y2
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.Line(X1,Y1,X2,Y2 : Integer);
begin
  //?? Additional function ??
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
  if csBrushValid in FState
  then begin
    Exclude(FState, csBrushValid);
    //TODO: Select stock object;
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
  if csFontValid in FState
  then begin
    Exclude(FState, csFontValid);
    //TODO: Select stock object;
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
    //TODO: Select stock object;
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
  inherited Create;
  FFont := TFont.Create;
  FFont.OnChange := @FontChanged;
  FSavedFontHandle := 0;
  FPen := TPen.Create;
  FPen.OnChange := @PenChanged;
  FSavedPenHandle := 0;
  FBrush := TBrush.Create;
  FBrush.OnChange := @BrushChanged;
  FSavedBrushHandle := 0;
  FCopyMode := cmSrcCopy;
  FPenPos := Point(0, 0);
  FLockCount := 0;
  InitializeCriticalSection(FLock);
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
  Params:   x,y,width,height,angle1,angle2
  Returns:  Nothing

  Use Chord to draw a filled Chord-shape on the canvas. The angles angle1 and
  angle2 are 1/16th of a degree. For example, a full circle equals 5760(16*360).
  Positive values of Angle and AngleLength mean counter-clockwise while negative
  values mean clockwise direction. Zero degrees is at the 3'o clock position.

------------------------------------------------------------------------------}
procedure TCanvas.Chord(x,y,width,height,angle1,angle2 : Integer);
begin
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  LCLLinux.AngleChord(FHandle,x,y,width,height,angle1,angle2);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Chord
  Params:   x,y,width,height,sx,sy,ex,ey
  Returns:  Nothing

  Use Chord to draw a filled Chord-shape on the canvas. The values sx,sy,
  and ex,ey represent a starting and ending radial-points between which
  the Arc is draw.

------------------------------------------------------------------------------}
procedure TCanvas.Chord(x,y,width,height,sx,sy,ex,ey : Integer);
begin
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  LCLLinux.RadialChord(FHandle,x,y,width,height,sx,sy,ex,ey);
end;

{------------------------------------------------------------------------------
  Method: TCanvas.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TCanvas.Destroy;
begin
//writeln('[TCanvas.Destroy] ',ClassName,'  Self=',HexStr(Cardinal(Pointer(Self)),8));
  Handle := 0;
  FFont.Free;
  FPen.Free;
  FBrush.Free;
  if FLockCount <> 0 then
     DeleteCriticalSection(FLock);
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TCanvas.GetHandle
  Params:   None
  Returns:  A handle to the GUI object

  Checks if a handle is allocated, otherwise create it
 ------------------------------------------------------------------------------}
function TCanvas.GetHandle : HDC;
begin
//writeln('[TCanvas.GetHandle] ',ClassName);
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
//writeln('[TCanvas.SetHandle] Old=',HexStr(FHandle,8),' New=',HexStr(NewHandle,8));
    if FHandle <> 0 then
    begin
      DeselectHandles;
      FPenPos := GetPenPos;
      FHandle := 0;
      Exclude(FState, csHandleValid);
    end;
    if NewHandle <> 0 then
    begin
      Include(FState, csHandleValid);
      FHandle := NewHandle;
      SetPenPos(FPenPos);
    end;
//writeln('[TCanvas.SetHandle] END Handle=',HexStr(FHandle,8));
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
  if (FHandle<>0) 
  and (FState - [csPenValid, csBrushValid, csFontValid] <> FState) then begin
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
//writeln('[TCanvas.RequiredState] ',csHandleValid in ReqState,' ',csHandleValid in FState);
  Needed := ReqState - FState;
  if Needed <> [] then
  begin
    if csHandleValid in Needed then
    begin
      CreateHandle;
      if FHandle = 0
      then raise EInvalidOperation.Create('Canvas does not allow drawing');
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

{------------------------------------------------------------------------------
  Function: TCanvas.TextExtent
  Params:   Text: The text to measure
  Returns:  The size

  Gets the width and height of a text
 ------------------------------------------------------------------------------}
function TCanvas.TextExtent(const Text: string): TSize;
var
  pStr: PChar;
begin
  Result.cX := 0;
  Result.cY := 0;
  RequiredState([csHandleValid, csFontValid]);

  pStr := StrAlloc(Length(Text)+1);
  try
    StrPCopy(pStr, Text);
    GetTextExtentPoint(FHandle, pStr, Length(Text), Result);
  finally
    StrDispose(PStr);
  end;
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
  EnterCriticalSection(FLock);
  Inc(FLockCount);
end;

{------------------------------------------------------------------------------
  Function: TCanvas.Unlock
  Params:   none
  Returns:  nothing
 ------------------------------------------------------------------------------}
procedure TCanvas.Unlock;
begin
  LeaveCriticalSection(FLock);
  Dec(FLockCount);
end;

{ =============================================================================

  $Log$
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
  LCLLinux and Graphics was split, so a bunch of files had to be modified.

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
