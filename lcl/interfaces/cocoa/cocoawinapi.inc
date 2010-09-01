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
    WriteLn('[WinAPI ShowWindow]');
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

function TCocoaWidgetSet.LineTo(DC: HDC; X, Y: Integer): Boolean;
var
  ctx : TCocoaContext;
begin
  ctx:=CheckDC(DC);
  Result:=Assigned(ctx);
  if not Result then Exit;

  ctx.MoveTo(x,y);
end;

function TCocoaWidgetSet.MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint): Boolean;
var
  ctx : TCocoaContext;
begin
  ctx:=CheckDC(DC);
  Result:=Assigned(ctx);
  if not Result then Exit;

  if Assigned(OldPoint) then OldPoint^:=ctx.PenPos;
  ctx.LineTo(x,y);
end;

{-------------------------- REGION ROUTINES -----------------------------------}

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
    DebugLn('TCarbonWidgetSet.CreatePolygonRgn NumPts: ' + DbgS(NumPts) +
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
    DebugLn('TCarbonWidgetSet.CreateRectRgn R: ' + DbgS(Classes.Rect(X1, Y1, X2, Y2)));
  {$ENDIF}

  Result := HRGN(TCocoaRegion.Create(X1, Y1, X2, Y2));
end;

//todo:
//function TCocoaWidgetSet.CreateEllipticRgn(p1, p2, p3, p4: Integer): HRGN;
//begin
//end;


//##apiwiz##eps##   // Do not remove, no wizard declaration after this line
