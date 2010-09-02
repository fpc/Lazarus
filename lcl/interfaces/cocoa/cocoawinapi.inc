{%MainUnit cocoaint.pp}
{ $Id: cocoawinapi.inc 15525 2008-06-23 06:39:58Z paul $ }
{******************************************************************************
  All Cocoa Winapi implementations.
  This are the implementations of the overrides of the Cocoa Interface for the
  methods defined in the
  lcl/include/winapi.inc


  !! Keep alphabetical !!


 ******************************************************************************
 Implementation
 ******************************************************************************

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
}

//##apiwiz##sps##   // Do not remove, no wizard declaration before this line

{------------------------------------------------------------------------------
  function ShowWindow(hWnd: HWND; nCmdShow: Integer): Boolean;

  nCmdShow:
    SW_SHOWNORMAL, SW_MINIMIZE, SW_SHOWMAXIMIZED
------------------------------------------------------------------------------}
function TCocoaWidgetSet.ShowWindow(hWnd: HWND; nCmdShow: Integer): Boolean;
begin
  {$ifdef VerboseCocoaWinAPI}
    DebugLn('TCocoaWidgetSet.ShowWindow');
  {$endif}

  case nCmdShow of
    SW_SHOW, SW_SHOWNORMAL:
      NSWindow(hwnd).orderFront(nil);
    SW_HIDE:
      NSWindow(hwnd).orderOut(nil);
    SW_MINIMIZE:
      NSWindow(hwnd).miniaturize(nil);
  end;
  Result:=true;
end;

{------------------------------------------------------------------------------
  Method:  GetWindowRect
  Params:  Handle - Handle of window
           Rect   - Record for window coordinates
  Returns: if the function succeeds, the return value is nonzero; if the
           function fails, the return value is zero

  Retrieves the screen bounding rectangle of the specified window
 ------------------------------------------------------------------------------}
function TCocoaWidgetSet.GetWindowRect(Handle: hwnd; var ARect: TRect): Integer;
begin
  Result:=0;
  if Handle=0 then Exit;

  if NSObject(Handle).isKindOfClass_(NSWindow) then
    GetNSWindowFrame(NSWindow(Handle), ARect)
  else
    {todo: GetNSViewFrame(NSView(Handle), ARect)};
end;

{--------------------------------- DRAWING ------------------------------------}


type
  TPointArray = array [word] of TPoint;
  PPointArray = ^TPointArray;

function TCocoaWidgetSet.LineTo(DC: HDC; X, Y: Integer): Boolean;
var
  ctx : TCocoaContext;
begin
  ctx:=CheckDC(DC);
  Result:=Assigned(ctx);
  if not Result then Exit;

  ctx.LineTo(x,y);
end;

function TCocoaWidgetSet.MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint): Boolean;
var
  ctx : TCocoaContext;
begin
  ctx:=CheckDC(DC);
  Result:=Assigned(ctx);
  if not Result then Exit;

  if Assigned(OldPoint) then OldPoint^:=ctx.PenPos;
  ctx.MoveTo(x,y);
end;

function TCocoaWidgetSet.Polygon(DC: HDC; Points: PPoint; NumPts: Integer; Winding: boolean): boolean;
var
  ctx : TCocoaContext;
begin
  ctx:=CheckDC(DC);
  Result:=Assigned(ctx) and Assigned(Points) and (NumPts>=2);
  if not Result then Exit;

  ctx.Polygon(PPointArray(Points)^, NumPts, Winding);
  Result:=True;
end;

function TCocoaWidgetSet.Polyline(DC: HDC; Points: PPoint; NumPts: Integer): boolean;
var
  ctx : TCocoaContext;
begin
  ctx:=CheckDC(DC);
  Result:=Assigned(ctx) and Assigned(Points) and (NumPts>0);
  if not Result then Exit;

  ctx.Polyline(PPointArray(Points)^, NumPts);
  Result:=True;
end;

function TCocoaWidgetSet.Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean;
var
  ctx : TCocoaContext;
begin
  ctx:=CheckDC(DC);
  Result:=Assigned(ctx);
  if not Result then Exit;
  ctx.Rectangle(X1, Y1, X2, Y2, False, nil);
  Result:=True;
end;

function TCocoaWidgetSet.FillRect(DC: HDC; const Rect: TRect; Brush: HBRUSH): Boolean;
var
  ctx : TCocoaContext;
  br  : TCocoaGDIObject;
begin
  ctx:=CheckDC(DC);
  br:=CheckGDIOBJ(Brush);
  Result:=Assigned(ctx) and (not Assigned(br) or (br is TCocoaBrush));
  if not Result then Exit;
  with Rect do
    ctx.Rectangle(Left, Top, Right, Bottom, True, TCocoaBrush(br));
end;

function TCocoaWidgetSet.Ellipse(DC: HDC; x1, y1, x2, y2: Integer): Boolean;
var
  ctx : TCocoaContext;
begin
  ctx:=CheckDC(DC);
  Result:=Assigned(ctx);
  if not Result then Exit;
  ctx.Ellipse(x1, y1, x2, y2);
end;

{----------------------------------- REGION -----------------------------------}

{------------------------------------------------------------------------------
  Method:  CreatePolygonRgn
  Params:  Points   - Pointer to array of polygon points
           NumPts   - Number of points passed
           FillMode - Filling mode
  Returns: The new polygonal region

  Creates a new polygonal region from the specified points
 ------------------------------------------------------------------------------}
function TCocoaWidgetSet.CreatePolygonRgn(Points: PPoint; NumPts: Integer;
  FillMode: integer): HRGN;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCocoaWidgetSet.CreatePolygonRgn NumPts: ' + DbgS(NumPts) +
      ' FillMode: ' + DbgS(FillMode));
  {$ENDIF}

  Result := HRGN(TCocoaRegion.Create(Points, NumPts, FillMode=ALTERNATE));
end;

function CocoaCombineMode(fnCombineMode: Integer): TCocoaCombine;
begin
  case fnCombineMode of
    RGN_AND:  Result:=cc_And;
    RGN_OR:   Result:=cc_Or;
    RGN_XOR:  Result:=cc_Xor;
    RGN_DIFF: Result:=cc_Diff;
  else
    Result:=cc_Copy;
  end;
end;

function TCocoaWidgetSet.CombineRgn(Dest, Src1, Src2: HRGN;
  fnCombineMode: Longint): Longint;
begin
  Result := LCLType.Error;
  if (Dest = 0) or (Src1 = 0) or (fnCombineMode<RGN_AND) or (fnCombineMode>RGN_COPY) then Exit;
  if (fnCombineMode <> RGN_COPY) and (Src2 = 0) then Exit;

  TCocoaRegion(Dest).CombineWith(TCocoaRegion(Src1), cc_Copy);

  if fnCombineMode <> RGN_COPY then
    TCocoaRegion(Dest). CombineWith(TCocoaRegion(Src2), CocoaCombineMode(fnCombineMode));
end;

function TCocoaWidgetSet.CreateRectRgn(X1, Y1, X2, Y2: Integer): HRGN;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCocoaWidgetSet.CreateRectRgn R: ' + DbgS(Classes.Rect(X1, Y1, X2, Y2)));
  {$ENDIF}

  Result := HRGN(TCocoaRegion.Create(X1, Y1, X2, Y2));
end;

//todo:
//function TCocoaWidgetSet.CreateEllipticRgn(p1, p2, p3, p4: Integer): HRGN;
//begin
//end;

{---------------------------- PEN AND BRUSH -----------------------------------}

function TCocoaWidgetSet.CreatePenIndirect(const LogPen: TLogPen): HPEN;
var
  p   : TCocoaPen;
  cl  : DWORD;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCocoaWidgetSet.CreatePenIndirect');
  {$ENDIF}
  p:=TCocoaPen.Create;
  if LogPen.lopnWidth.x>0 then p.Width:=LogPen.lopnWidth.x;
  p.Style:=LogPen.lopnStyle;
  if LogPen.lopnColor and $8000000 > 0 then cl:=GetSysColor(LogPen.lopnColor)
  else cl:=LogPen.lopnColor;
  //todo:!
  p.R:=(cl and $FF) / $FF;
  p.G:=((cl shr 8) and $FF) / $FF;
  p.B:=((cl shr 16) and $FF) / $FF;
  Result := HPEN(p);//TCocoaPen.Create(LogPen));
end;

{------------------------------- DEVICE CONTEXT -------------------------------}

function TCocoaWidgetSet.SelectObject(ADC: HDC; GDIObj: HGDIOBJ): HGDIOBJ;
var
  dc: TCocoaContext;
  gdi: TCocoaGDIObject;
const
  SName = 'TCarbonWidgetSet.SelectObject';
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCocoaWidgetSet.SelectObject DC: ' + DbgS(DC) + ' GDIObj: ' +  DbgS(GDIObj));
  {$ENDIF}
  Result := 0;

  dc:=CheckDC(ADC);
  gdi:=CheckGDIOBJ(GDIObj);
  if not Assigned(dc) then Exit;

  if gdi is TCocoaBrush then begin // select brush
    Result := HBRUSH(dc.Brush);
    dc.Brush := TCocoaBrush(gdi);
  end else if gdi is TCocoaPen then begin // select pen
    Result := HPEN(dc.Pen);
    dc.Pen := TCocoaPen(gdi);
  end else if gdi is TCocoaFont then begin // select font
    Result := HFONT(dc.Font);
    dc.Font := TCocoaFont(gdi);
  end else if gdi is TCocoaRegion then begin // select region
    Result := HBRUSH(dc.Region);
    dc.Region := TCocoaRegion(gdi);
  end else if gdi is TCocoaBitmap then begin // select bitmap
    {if not (ADC is TCarbonBitmapContext) then
    begin
      DebugLn(SName + ' Error - The specified device context is not bitmap context!');
      Exit;
    end;}
    Result := HBITMAP(dc.Bitmap);
    dc.Bitmap:=TCocoaBitmap(gdi);
    //TCarbonBitmapContext(ADC).Bitmap := TCarbonBitmap(GDIObj);
  end;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCocoaWidgetSet.SelectObject Result: ' + DbgS(Result));
  {$ENDIF}
end;

function TCocoaWidgetSet.RectVisible(DC: HDC; const ARect: TRect): Boolean;
var
  ClipBox: CGRect;
  ctx : TCocoaContext;
  R: TRect;
begin
  Result := False;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.RectVisible DC: ' + DbgS(DC) + ' R: ' + DbgS(ARect));
  {$ENDIF}

  ctx:=CheckDC(DC);
  if not Assigned(ctx) or (ARect.Right <= ARect.Left) or (ARect.Bottom <= ARect.Top) then Exit;

  // In Quartz 2D there is no direct access to clipping path of CGContext,
  // therefore we can only test bounding box of the clipping path.

  ClipBox := CGContextGetClipBoundingBox(ctx.CGContext);
  Result := IntersectRect(R, ARect, CGRectToRect(ClipBox));

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.RectVisible Result: ' + DbgS(Result) + ' Clip: ' + DbgS(CGRectToRect(ClipBox)));
  {$ENDIF}
end;

{------------------------------------ TEXT ------------------------------------}

function TCocoaWidgetSet.ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
var
  ctx : TCocoaContext;
begin
  ctx:=CheckDC(DC);
  Result:=Assigned(ctx);
  if not Assigned(ctx) then Exit;

  ctx.TextOut(X,Y, Str, Count, Dx);
end;

function TCocoaWidgetSet.TextOut(DC: HDC; X,Y : Integer; Str : Pchar; Count: Integer) : Boolean;
begin
  Result:=ExtTextOut(DC, X, Y, 0, nil, Str, Count, nil);
end;


//##apiwiz##eps##   // Do not remove, no wizard declaration after this line
