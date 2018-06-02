{%MainUnit muiint.pp}

{******************************************************************************
  All MUI Winapi implementations.
  This are the implementations of the overrides of the MUI Interface for the
  methods defined in the
  lcl/include/winapi.inc


  !! Keep alphabetical !!


 ******************************************************************************
 Implementation
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{.$define VERBOSEAROS}


const
  {$ifdef AROS}
  DEFALPHAVALUE = 255;
  {$endif}
  {$ifdef MorphOS}
  DEFALPHAVALUE = $FFFFFFFF;
  {$endif}
  {$ifdef Amiga}
  DEFALPHAVALUE = $FFFFFFFF;
  {$endif}

//##apiwiz##sps##   // Do not remove, no wizard declaration before this line

{------------------------------------------------------------------------------
  Function: BeginPaint
  Params:
  Returns:

  This function is Called:
  - Once on every OnPaint event
 ------------------------------------------------------------------------------}
function TMUIWidgetSet.BeginPaint(Handle: hWnd; Var PS : TPaintStruct): hdc;
(*var
  PrivateWidget: TFPGUIPrivateWidget absolute Handle;
  DC: TFpGuiDeviceContext;*)
begin
  {$ifdef VERBOSEAROS}
  writeln('begin paint');
  {$endif}
  Result := 0;
(*  {$ifdef VerboseFPGUIWinAPI}
    WriteLn('Trace:> [WinAPI BeginPaint] Handle=', dbghex(Handle));
  {$endif}
  {$WARNING TMUIWidgetSet.BeginPaint Temporary Fix to prevent Crashing}
  try
  if PrivateWidget <> nil then
    DC := TFpGuiDeviceContext.Create(PrivateWidget)
  else
    DC := TFpGuiDeviceContext.Create(nil);
    {$ifdef VerboseFPGUIWinAPI}
    if PrivateWidget <> nil then
    WriteLn(PrivateWidget.ClassName);
    {$endif}
  except
    DC := TFpGuiDeviceContext.Create(nil);
  end;
  PS.hdc := HDC(DC);

  Result := PS.hdc;

  {$ifdef VerboseFPGUIWinAPI}
    WriteLn('Trace:< [WinAPI BeginPaint] Result=', dbghex(Result));
  {$endif}*)
end;

function TMUIWidgetSet.BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD): Boolean;
var
  Dest: TMUICanvas absolute DestDC;
  Src: TMUICanvas absolute SrcDC;
begin
  {$ifdef VERBOSEAROS}
  writeln('BitBlt $', HexStr(Pointer(DestDC)), ', $',HexStr(Pointer(SrcDC)),', $',HexStr(Pointer(Mask)));
  {$endif}
  if Assigned(Dest) and Assigned(Src) and Assigned(Src.Bitmap) and Assigned(Src.Bitmap.FImage) then
  begin
    Dest.Drawn := True;
    if Src.Drawn then // means something was drawn on -> no alpha anymore :(
    begin
      ClipBlit(Src.RastPort, xSrc, YSrc, Dest.RastPort, Dest.GetOffset.X + x, Dest.GetOffset.Y + y, width, height, $c0);
    end else
    begin
      {$ifndef AMIGA68k}
      WritePixelArrayAlpha(Src.Bitmap.FImage, XSrc, YSrc, Src.Bitmap.FWidth * SizeOf(LongWord), Dest.RastPort, Dest.GetOffset.X + x, Dest.GetOffset.Y + y, width, height, DEFALPHAVALUE);
      {$else}
      if Assigned(CyberGfxBase) then
        Cybergraphics.WritePixelArray(Src.Bitmap.FImage, XSrc, YSrc, Src.Bitmap.FWidth * SizeOf(LongWord), Dest.RastPort, Dest.GetOffset.X + x, Dest.GetOffset.Y + y, width, height, RECTFMT_ARGB);
      {$endif}
    end;
    //WritePixelArray(Src.Bitmap.FImage, XSrc, YSrc, Src.Bitmap.FWidth * SizeOf(LongWord), Dest.RastPort, Dest.GetOffset.X + x, Dest.GetOffset.Y + y, width, height, RECTFMT_ARGB)
  end;
  Result := True;
end;

function TMUIWidgetSet.ClientToScreen(Handle: HWND; var P: TPoint): Boolean;
var
  Widget: TMUIObject absolute Handle;
begin
  {$ifdef VERBOSEAROS}
  writeln('client to screen start ', P.X ,' ', HexStr(Widget));
  {$endif}
  if Assigned(Widget) then
  begin
    repeat
      P.X := P.X + Widget.Left;
      P.Y := P.Y + Widget.Top;
      Widget := Widget.Parent;
    until not Assigned(Widget) or (Widget is TMUIApplication);
    //TODO: get real left and top border from Widget
    P.X := P.X + 15;
    P.Y := P.Y + 30;
  end;
  {$ifdef VERBOSEAROS}
  writeln('client to screen end ', P.X);
  {$endif}
  Result := True;
end;

function TMUIWidgetSet.CombineRgn(Dest, Src1, Src2: HRGN;
  fnCombineMode: Longint): Longint;
var
  R1: TMUIBasicRegion absolute Src1;
  R2: TMUIBasicRegion absolute Src2;
  DR: TMUIBasicRegion absolute Dest;
  Combine: TMUIRegionCombine;
begin
  {$ifdef VERBOSEAROS}
  writeln('combine region $', HexStr(Pointer(src1)), ' + $', HexStr(Pointer(src2)), ' to $', HexStr(Pointer(Dest)));
  writeln('src1: ', R1.Debugout);
  writeln('src2: ', R2.Debugout);
  {$endif}
  Result := 0;
  case fnCombineMode of
    RGN_AND:  Combine:=eRegionCombineAnd;
    RGN_COPY: Combine:=eRegionCombineCopy;
    RGN_DIFF: Combine:=eRegionCombineDiff;
    RGN_OR:   Combine:=eRegionCombineOr;
    RGN_XOR:  Combine:=eRegionCombineXor;
  end;
  if DR<>nil then DR.Free;
  DR:=R1.CombineWithRegion(R2,Combine);
  Case dr.RegionType of
    eRegionNULL: Result:=NullRegion;
    eRegionSimple: Result:=SimpleRegion ;
    eRegionComplex: Result:=ComplexRegion;
    eRegionNotCombinableOrError: Result:=Region_Error;
  end;
  {$ifdef VERBOSEAROS}
  writeln('Dest: ', DR.Debugout);
  {$endif}
end;

function TMUIWidgetSet.CreateBitmap(Width, Height: Integer; Planes,
  BitCount: Longint; BitmapBits: Pointer): HBITMAP;
//var
//  img: TFPGUIWinAPIBitmap;
begin
  {$ifdef VERBOSEAROS}
  writeln('Create Bitmap');
  {$endif}
(*  if BitCount>0 then begin
    img:=TFPGUIWinAPIBitmap.Create(BitCount,Width,Height);
    Result:=HBITMAP(img);
  end else begin
    Result:=0;
  end;*)
  Result := 0;
end;

function TMUIWidgetSet.CreateBrushIndirect(const LogBrush: TLogBrush
  ): HBRUSH;
begin
  Result:=HBRUSH(TMUIBrushObj.Create(LogBrush));
end;

function TMUIWidgetSet.CreateCaret(Handle : HWND; Bitmap : hBitmap; Width, Height : Integer) : Boolean;
var
  Widget: TMUIObject absolute Handle;
begin
  {$ifdef VERBOSEAROS}
  writeln('Create Caret ', Width, ', ', Height);
  {$endif}
  Result := False;
  if Assigned(Widget) then
  begin
    Widget.Caret := TMUICaret.Create;
    Widget.Caret.Left := 0;
    Widget.Caret.Top := 0;
    Widget.Caret.Width := Width;
    Widget.Caret.Height := Height;
    Widget.Caret.Shown := False;
    Result := True;
  end;
end;

function TMUIWidgetSet.CreateCompatibleBitmap(DC: HDC; Width, Height: Integer): HBITMAP;
//var
//  img: TFPGUIWinAPIBitmap;
begin
  {$ifdef VERBOSEAROS}
  writeln('Create Compatible Bitmap');
  {$endif}
  //img:=TFPGUIWinAPIBitmap.Create(32,Width,Height);
  //Result:=HBITMAP(img);
  Result := 0;
end;

function TMUIWidgetSet.CreateCompatibleDC(DC: HDC): HDC;
var
  ADC: TMUICanvas absolute DC;
  NewDC: TMUICanvas;
begin
  {$ifdef VERBOSEAROS}
  writeln('-->CreateCompatibleDC ', HexStr(ADC));
  {$endif}
  Result := 0;
  //if DC <> 0 then
  begin
    NewDC := TMUICanvas.Create;
    NewDC.RastPort := nil;
    if ADC <> nil then
    begin
      NewDC.RastPort := CloneRastPortA(ADC.RastPort);
      NewDC.DrawRect := ADC.DrawRect;
    end else
    begin
      NewDC.RastPort := CreateRastPortA;
      NewDC.RastPort^.Layer := nil;
      NewDC.RastPort^.Bitmap := AllocBitMap(IntuitionBase^.ActiveScreen^.Width, IntuitionBase^.ActiveScreen^.Height, 32, BMF_CLEAR or BMF_MINPLANES or BMF_DISPLAYABLE, IntuitionBase^.ActiveScreen^.RastPort.Bitmap);
      NewDC.DrawRect := Rect(0, 0, IntuitionBase^.ActiveScreen^.Width, IntuitionBase^.ActiveScreen^.Height);
    end;
    NewDC.InitCanvas;
    NewDC.RenderInfo := nil;
    if DC <> 0 then
      NewDC.RenderInfo := ADC.RenderInfo;
    ClipBlit(@(IntuitionBase^.ActiveScreen^.RastPort), 0, 0, NewDC.RastPort, 0, 0, IntuitionBase^.ActiveScreen^.Width, IntuitionBase^.ActiveScreen^.Height, $00C0);
    NewDC.Drawn := True;
    Result := HDC(NewDC);
  end;
  {$ifdef VERBOSEAROS}
  writeln('<--CreateCompatibleDC ' , HexStr(Pointer(Result)));
  {$endif}
end;

function TMUIWidgetSet.CreateFontIndirect(const LogFont: TLogFont): HFONT;
var
  FontObj: TMUIFontObj;
begin
  {$ifdef VERBOSEAROS}
  writeln('CreateFontIndirect');
  {$endif}
  FontObj := TMUIFontObj.Create(LogFont);
  if Assigned(FontObj.TextFont) then
  begin
    Result := HFont(FontObj);
  end else
  begin
    FontObj.Free;
    Result := 0;
  end;
end;

function TMUIWidgetSet.CreateFontIndirectEx(const LogFont: TLogFont; const LongFontName: string): HFONT;
var
  FontObj: TMUIFontObj;
begin
  {$ifdef VERBOSEAROS}
  writeln('CreateFontIndirectEx ', LongFontName);
  {$endif}
  FontObj := TMUIFontObj.Create(LogFont, LongFontName);
  if Assigned(FontObj.TextFont) then
  begin
    Result := HFont(FontObj);
  end else
  begin
    FontObj.Free;
    Result := 0;
  end;
end;

function TMUIWidgetSet.CreatePenIndirect(const LogPen: TLogPen): HPEN;
begin
  Result := HPEN(TMUIPenObj.Create(LogPen));
end;

function TMUIWidgetSet.CreateRectRgn(X1, Y1, X2, Y2: Integer): HRGN;
var
  Reg: TMUIBasicRegion;
begin
  Reg:=TMUIBasicRegion.Create(Rect(X1,Y1,X2,Y2));
  Result:=HRGN(Reg);
  {$ifdef VERBOSEAROS}
  writeln('Create Rect Region ', x1,', ', y1, ', ', x2, ', ', y2,' $',HexStr(Reg));
  {$endif}
end;

procedure TMUIWidgetSet.DeleteCriticalSection(var CritSection: TCriticalSection);
var
  ACritSec: System.PRTLCriticalSection;
begin
  ACritSec:=System.PRTLCriticalSection(CritSection);
  System.DoneCriticalsection(ACritSec^);
  Dispose(ACritSec);
  CritSection:=0;
end;

function TMUIWidgetSet.DeleteDC(hDC: HDC): Boolean;
var
  ADC: TMUICanvas absolute hDC;
begin
  {$ifdef VERBOSEAROS}
  writeln('Delete DC $', HexStr(Pointer(hdc)));
  {$endif}
  Result := True;
  ADC.Free;
end;

function TMUIWidgetSet.DeleteObject(GDIObject: HGDIOBJ): Boolean;
var
  Obj: TObject absolute GDIObject;
begin
  {$ifdef VERBOSEAROS}
  writeln(obj.classname, ' DeleteObject $', HexStr(Pointer(GDIObject)));
  {$endif}
  if (GDIObject > $100) and Assigned(Obj) then
  begin
   if Obj is TMUIWinAPIObject then
     Obj.Free;
  end;
  Result:=True;
end;

function TMUIWidgetSet.DestroyCaret(Handle : HWND): Boolean;
var
  Widget: TMUIObject absolute Handle;
begin
  {$ifdef VERBOSEAROS}
  writeln('Destroy Caret');
  {$endif}
  Result := False;
  if Assigned(Widget) then
  begin
    Widget.Caret.Free;
    Widget.Caret := nil;
  end;
end;

function TMUIWidgetSet.DrawFocusRect(DC: HDC; const Rect: TRect): boolean;
//var
//  ADC: TFpGuiDeviceContext absolute DC;
//  r: TfpgRect;
begin
  //writeln('DrawFocusRect');
  //ADC.fpgCanvas.DrawFocusRect(ADC.PrepareRectOffsets(Rect));
  Result:=true;
end;

function TMUIWidgetSet.DrawEdge(DC: HDC; var Rect: TRect; edge: Cardinal; grfFlags: Cardinal): Boolean;
begin
  //writeln('DrawEdge');
  Frame3d(DC, Rect, 1, bvRaised);
  Result := True;
end;

function TMUIWidgetSet.DrawText(DC: HDC; Str: PChar; Count: Integer;
  var ARect: TRect; Flags: Cardinal): Integer;
var
  ADC: TMUICanvas absolute DC;
begin
  {$ifdef VERBOSEAROS}
  writeln('Draw Text ', str, ' DC: ', HexStr(Pointer(DC)) , ' to ', ARect.Left, ',  ', ARect.Top, '; ', ARect.Right, ', ', ARect.Bottom, ',',  Assigned(ADC.RastPort));
  {$endif}
  Result := 0;
  if Assigned(ADC) then
  begin
    if (Flags and DT_CALCRECT) <> 0 then
    begin
      ARect.Right := ARect.Left + ADC.TextWidth(Str, Count);
      ARect.Bottom := ARect.Top + ADC.TextHeight(Str, Count);
      Result := ADC.TextHeight(Str, Count);
      Exit;
    end;
    if Assigned(ADC.RastPort) then
    begin
      {$ifdef VERBOSEAROS}
      if (Flags and DT_BOTTOM) <> 0 then
      begin
        writeln('Bottom');
      end;
      if (Flags and DT_Right) <> 0 then
      begin
        writeln('Right');
      end;
      if (Flags and DT_Center) <> 0 then
      begin
        writeln('Center');
      end;
      if (Flags and DT_Left) <> 0 then
      begin
        writeln('Left');
      end;
      if (Flags and DT_Top) <> 0 then
      begin
        writeln('Top');
      end;
      if (Flags and DT_VCenter) <> 0 then
      begin
        writeln('VCenter');
      end;
      {$endif}
      SetDrMd(ADC.RastPort, JAM1);
      ADC.MoveTo(ARect.Left, ARect.Top{ + ADC.TextHeight(str, Count) div 2});
      ADC.WriteText(str, Count);
      Result := ADC.TextHeight(Str, Count);
      ADC.ResetPenBrushFont;
    end;
  end;
end;

function TMUIWidgetSet.Ellipse(DC: HDC; x1, y1, x2, y2: Integer): Boolean;
var
  ADC: TMUICanvas absolute DC;
begin
  {$ifdef VERBOSEAROS}
  writeln('Ellipse ', X1, ', ', Y1, ' - ', X2, ', ', Y2);
  {$endif}
  Result := False;
  if Assigned(ADC) then
  begin
    ADC.ellipse(X1, Y1, X2, Y2);
    Result := True;
  end;

end;

function TMUIWidgetSet.EnableWindow(hWnd: HWND; bEnable: Boolean): Boolean;
var
  Widget: TMUIObject absolute hWnd;
begin
  Widget.Enabled:=bEnable;
  Result:=true;
end;

function TMUIWidgetSet.EndPaint(Handle: hwnd; var PS: TPaintStruct): Integer;
//var
//  DC: TFpGuiDeviceContext;
begin
  {$ifdef VERBOSEAROS}
   writeln('end paint');
  {$endif}
//  DC := TFpGuiDeviceContext(PS.hdc);
//  DC.Free;
//  Result:=1; //Any non zero value.
  Result := 0;
end;

procedure TMUIWidgetSet.EnterCriticalSection(var CritSection: TCriticalSection);
var
  ACritSec: System.PRTLCriticalSection;
begin
  ACritSec:=System.PRTLCriticalSection(CritSection);
  System.EnterCriticalsection(ACritSec^);
end;

function TMUIWidgetSet.EnumDisplayMonitors({%H-}hdc: HDC; {%H-}lprcClip: PRect; lpfnEnum: MonitorEnumProc; dwData: LPARAM): LongBool;
var
  i: integer;
begin
  Result := True;
  for i := 0 to 0 do
  begin
    Result := Result and lpfnEnum(i + 1, 0, nil, dwData);
    if not Result then break;
  end;
end;

{
function TMUIWidgetSet.ExtSelectClipRGN(dc: hdc; rgn : hrgn; Mode : Longint) : Integer;
var
  Region: TMUIBasicRegion absolute RGN;
begin
  writeln('ExtSelectClip ', Region.Debugout, ': ', Mode);
end; }

function TMUIWidgetSet.ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint;
  Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
var
  ADC: TMUICanvas absolute DC;
begin
  {$ifdef VERBOSEAROS}
  write('Ext Text out "', str,'" - ', Options, ' - ');
  {$endif}
  if Assigned(ADC) and Assigned(ADC.RastPort) then
  begin
    if Assigned(Rect) and ((Options and ETO_OPAQUE) <> 0) then
    begin
      ADC.SetBKToRP(True);
      ADC.FillRect(Rect^.Left, Rect^.Top, Rect^.Right, Rect^.Bottom);
      ADC.SetPenToRP;
    end;
    if ADC.BKMode = Opaque then
      ADC.SetBrushToRP(False)
    else
      SetDrMd(ADC.RastPort, JAM1);
    //
    ADC.MoveTo(X, Y);
    ADC.WriteText(Str, Count);
  end;
  Result := True;
end;

function TMUIWidgetSet.FillRect(DC: HDC; const Rect: TRect; Brush: HBRUSH): Boolean;
var
  ADC: TMUICanvas absolute DC;
  MUIBrush: TMUIWinAPIElement absolute Brush;
  OBrush: TMUIWinAPIElement;
begin
  if Assigned(MUIBrush) then
  begin
    OBrush := ADC.SelectObject(MUIBrush);
  end;
  ADC.SetBrushToRP(True);
  ADC.FillRect(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
  ADC.SetPenToRP;
  if Assigned(MUIBrush) then
  begin
    MUIBrush := ADC.SelectObject(OBrush);
  end;
  Result:=False;
end;

function TMUIWidgetSet.FloodFill(DC: HDC; X: Integer; Y: Integer; Color: TGraphicsColor; FillStyle: TGraphicsFillStyle; Brush: HBRUSH):Boolean;
var
  ADC: TMUICanvas absolute DC;
begin
  REsult := False;
  if Assigned(ADC) then
  begin
    ADC.FloodFill(X,Y,Color);
    Result := True;
  end;
end;

function TMUIWidgetSet.Frame3d(DC: HDC; var ARect: TRect; const FrameWidth : integer; const Style : TBevelCut): Boolean;
var
  ADC: TMUICanvas absolute DC;
begin
  Result := False;
  inherited;
  if Assigned(ADC) and Assigned(ADC.RastPort) then
  begin
    //writeln('Frame3D ', ARect.Left, ', ', ARect.Right, ' ; ', ARect.Top, ' ', ARect.Bottom,' w:', FrameWidth, ' style: ', Ord(Style));
    if Style = bvRaised then
    begin
      ADC.SetAMUIPen(MPEN_SHINE);
      ADC.MoveTo(ARect.Left, ARect.Bottom - 1);
      ADC.LineTo(ARect.Left, ARect.Top, True);
      ADC.LineTo(ARect.Right - 1, ARect.Top, True);
      ADC.SetAMUIPen(MPEN_SHADOW);
      ADC.MoveTo(ARect.Right - 1, ARect.Top);
      ADC.LineTo(ARect.Right - 1, ARect.Bottom - 1, True);
      ADC.LineTo(ARect.Left, ARect.Bottom - 1, True);
    end;
    if Style = bvLowered then
    begin
      ADC.SetAMUIPen(MPEN_SHADOW);
      ADC.MoveTo(ARect.Right - 1, ARect.Top);
      ADC.LineTo(ARect.Left, ARect.Top, True);
      ADC.LineTo(ARect.Left, ARect.Bottom - 1, True);
      ADC.SetAMUIPen(MPEN_SHINE);
      ADC.MoveTo(ARect.Left, ARect.Bottom - 1);
      ADC.LineTo(ARect.Right - 1, ARect.Bottom - 1, True);
      ADC.LineTo(ARect.Right - 1, ARect.Top, True);
    end;
    Inc(ARect.Left);
    Inc(ARect.Top);
    Dec(ARect.Right);
    Dec(ARect.Bottom);
    ADC.ResetPenBrushFont;
    Result := True;
  end;
  ADC.SetPenToRP();
end;

function TMUIWidgetSet.FrameRect(DC: HDC; const ARect: TRect; hBr: HBRUSH): Integer;
var
  ADC: TMUICanvas absolute DC;
begin
  ////writeln('FrameRect');
  ADC.Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  Result := 0;
end;

function TMUIWidgetSet.GetBitmapBits(Bitmap: HBITMAP; Count: Longint;  Bits: Pointer): Longint;
begin
  {$ifdef VERBOSEAROS}
  writeln('GetBitmapBits');
  {$endif}
  Result := 0;
end;

function TMUIWidgetSet.GetCapture: HWND;
begin
  Result := HWND(CaptureObj);
end;

function TMUIWidgetSet.GetClientBounds(handle : HWND; var ARect : TRect) : Boolean;
var
  Widget: TMUIObject absolute handle;
begin
  ARect.Left := Widget.Left;
  ARect.Right := Widget.Left + Widget.Width;
  ARect.Top := Widget.Top;
  ARect.Bottom := Widget.Top + Widget.Height;
  //writeln(Widget.classname, '################Get Clientbounds ', ARect.Left, ', ', ARect.Right);
  Result := True;
end;

function TMUIWidgetSet.GetClientRect(handle: HWND; var ARect: TRect
  ): Boolean;
var
  Widget: TMUIObject absolute handle;
begin
  ARect := Widget.GetClientRect;
  {$ifdef VERBOSEAROS}
  writeln(Widget.classname, '################Get ClientRect ', ARect.Left, ', ', ARect.Right);
  {$endif}
  Result:=True;
end;

function TMUIWidgetSet.GetClipBox(DC : hDC; lpRect : PRect) : Longint;
var
  ADC: TMUICanvas absolute DC;
begin
  // set default values
  Result := SIMPLEREGION;
  if lpRect <> nil then
    lpRect^ := Rect(0,0,0,0);

  if not Assigned(ADC) then
  begin
    Result := ERROR;
    Exit;
  end;
  lpRect^ := ADC.DrawRect;
end;

function TMUIWidgetSet.GetClipRGN(DC: hDC; RGN: hRGN): Longint;
var
  ADC: TMUICanvas absolute DC;
  Region: TMUIBasicRegion absolute RGN;
begin
  if Region=nil Then
    Exit;                   //
  //Region.CreateRectRegion(Rect(ADC.DrawRect.Left,ADC.DrawRect.Top,ADC.DrawRect.Right,ADC.DrawRect.Bottom));
  Region.CreateRectRegion(Rect(0,0,ADC.DrawRect.Right,ADC.DrawRect.Bottom));
  {$ifdef VERBOSEAROS}
  writeln('Get Clip region ', HexStr(Pointer(Rgn)) , Region.Debugout);
  {$endif}
  if Region.RegionType=eRegionNULL then begin
    Result:=0;
  end else if Region.RegionType=eRegionNotCombinableOrError then begin
    Result:=-1;
  end else begin
    Result:=1;
  end;
end;

function TMUIWidgetSet.GetCursorPos(var lpPoint: TPoint): Boolean;
begin
  lpPoint.X := IntuitionBase^.ActiveScreen^.MouseX;
  lpPoint.Y := IntuitionBase^.ActiveScreen^.MouseY;
  Result := True;
end;

function TMUIWidgetSet.GetDC(hWnd: HWND): HDC;
var
  PrivateWidget: TMUIObject absolute hWnd;
  ri: PMUI_RenderInfo;
begin
  Result := 0;
  if Assigned(PrivateWidget) then
  begin
    {$ifdef VERBOSEAROS}
    writeln('Get DC ', PrivateWidget.classname,' ', Assigned(PrivateWidget.MuiCanvas.RastPort));
    {$endif}
    if not Assigned(PrivateWidget.MuiCanvas.RastPort) then
    begin
      if PRivateWidget is TMuiWindow then
      begin
        ri := MUIRenderInfo(TMuiWindow(PrivateWidget).Grpobj);
        if Assigned(ri) then
        begin
          PrivateWidget.MUICanvas.RastPort := ri^.mri_RastPort;
          PrivateWidget.MUICanvas.DrawRect := Rect(0,0,0,0);
        end;
      end;
    end;
    Result := THandle(PrivateWidget.MuiCanvas);
  end else
    Result := CreateCompatibleDC(0);
end;

function TMUIWidgetSet.GetDeviceCaps(DC: HDC; Index: Integer): Integer;
begin
  //Desktop device caps
  { TODO : Create real data for GetDeviceCaps }
  Result := 0;
  Case Index of
    LOGPIXELSX: Result:=96; //Hardcoded by now
    BITSPIXEL : Result:=32; //Hardcoded by now
    else
        WriteLn(Self.ClassName,'.GetDeviceCaps Index ',Index,' Desktop');
  end;
end;

function TMUIWidgetSet.GetDeviceSize(DC: HDC; var P: TPoint): Boolean;
var
  ADC: TMUICanvas absolute DC;
begin
  Result := False;
  if Assigned(ADC) then
  begin
    P.X := ADC.DrawRect.Right;
    P.Y := ADC.DrawRect.Bottom;
  end;
end;

function TMUIWidgetSet.GetDIBits(DC: HDC; Bitmap: HBitmap; StartScan, NumScans: UINT; Bits: Pointer; var BitInfo: BitmapInfo; Usage: UINT): Integer;
begin
  {$ifdef VERBOSEAROS}
  writeln('GetDIBits');
  {$endif}
  Result := 0;
end;

function TMUIWidgetSet.GetFocus: HWND;
begin
  Result := FocusWidget;
end;

function TMUIWidgetSet.GetKeyState(nVirtKey: Integer): Smallint;
const
  StateDown: SmallInt = SmallInt($FF80);
var
  RShift, LShift, Shift, Control, LAlt, RAlt, Alt, RMeta, LMeta: Boolean;
  LMouse, MMouse, RMouse: Boolean;
begin
  Result := 0;
  //writeln('Keystate: ', HexStr(Pointer(KeyState)));
  RShift := KeyState and IEQUALIFIER_RSHIFT <> 0;
  LShift := KeyState and IEQUALIFIER_LSHIFT <> 0;
  Shift := RShift or LShift;
  Control := KeyState and IEQUALIFIER_CONTROL <> 0;
  //
  LAlt := keyState and IEQUALIFIER_LALT <> 0;
  RAlt := keyState and IEQUALIFIER_RALT <> 0;
  Alt := RAlt or LAlt;
  //
  LMeta := keyState and IEQUALIFIER_LCOMMAND <> 0;
  RMeta := keyState and IEQUALIFIER_RCOMMAND <> 0;
  //
  LMouse := KeyState and IEQUALIFIER_LEFTBUTTON <> 0;
  MMouse := KeyState and IEQUALIFIER_MIDBUTTON <> 0;
  RMouse := keyState and IEQUALIFIER_RBUTTON <> 0;
  case nVirtKey of
    VK_LShift: Result := ifthen(LShift, StateDown, 0);
    VK_RShift: Result := ifthen(RShift, StateDown, 0);
    VK_Shift: Result := ifthen(Shift, StateDown, 0);
    VK_Control: Result := ifthen(Control, StateDown, 0);
    VK_LControl: Result := ifthen(Control, StateDown, 0);
    VK_LMENU: Result := ifthen(LAlt, StateDown, 0);
    VK_RMENU: Result := ifthen(RAlt, StateDown, 0);
    VK_MENU: Result := ifthen(Alt, StateDown, 0);
    VK_LWIN: Result := ifthen(LMeta, StateDown, 0);
    VK_RWIN: Result := ifthen(RMeta, StateDown, 0);
    VK_LBUTTON: Result := ifthen(LMouse, StateDown, 0);
    VK_MBUTTON: Result := ifthen(MMouse, StateDown, 0);
    VK_RBUTTON: Result := ifthen(RMouse, StateDown, 0);
  end;
end;

function TMUIWidgetSet.GetMonitorInfo(Monitor: HMONITOR; lpmi: PMonitorInfo): Boolean;
begin
  Result := False;
  if (lpmi = nil) or (lpmi^.cbSize < SizeOf(TMonitorInfo)) then
    Exit;
  Result := True;
  lpmi^.rcMonitor := Bounds(0, 0, IntuitionBase^.ActiveScreen^.Width, IntuitionBase^.ActiveScreen^.Height);
  lpmi^.rcWork := lpmi^.rcMonitor;
  lpmi^.dwFlags := MONITORINFOF_PRIMARY;
end;

function TMUIWidgetSet.GetProp(Handle: hwnd; Str: PChar): Pointer;
var
    Widget: TMUIObject absolute Handle;
begin
  if Str = 'WinControl' then
  begin
    Result := Widget.PasObject;
  end else
  begin
    {.$ifdef VerboseFPGUIWinAPI}
      WriteLn('Trace:Unknown Window property: ',Str);
    {.$endif}
    Result:=nil;
  end;
end;

function TMUIWidgetSet.GetScrollBarSize(Handle: HWND; BarKind: Integer): integer;
var
  Widget: TMUIObject absolute Handle;
  SC: TMUIScrollbar;
begin
  Result := 0;
  if Assigned(Widget) then
  begin
    SC := nil;
    case BarKind of
      SB_VERT: SC := TMUIScrollbar(Widget.VScroll);
      SB_Horz: SC := TMUIScrollbar(Widget.HScroll);
    end;
    if not Assigned(SC) then
      Exit;
    //????  what it wants to know?
  end;
end;

function TMUIWidgetSet.GetScrollbarVisible(Handle: HWND; SBStyle: Integer): boolean;
var
  Widget: TMUIObject absolute Handle;
  SC: TMUIScrollbar;
begin
  Result := False;
  if Assigned(Widget) then
  begin
    SC := nil;
    case SBStyle of
      SB_VERT: SC := TMUIScrollbar(Widget.VScroll);
      SB_Horz: SC := TMUIScrollbar(Widget.HScroll);
    end;
    if not Assigned(SC) then
      Exit;
    Result := SC.Visible;
  end;
end;

function TMUIWidgetSet.GetScrollInfo(Handle: HWND; BarFlag: Integer; Var ScrollInfo: TScrollInfo): Boolean;
var
  Widget: TMUIObject absolute Handle;
  SC: TMUIScrollbar;
begin
  Result := False;
  //writeln('Get Scroll info');
  //Exit;

  if Assigned(Widget) then
  begin
    if not Widget.Visible then
      Exit;
    SC := nil;
    if TObject(Widget) is TMUIScrollbar then
    begin
      SC := TMUIScrollbar(Widget)
    end
    else
    begin
      if BarFlag = SB_VERT then
      begin
        SC := TMUIScrollbar(Widget.VScroll)
      end
      else
      begin
        if BarFlag = SB_Horz then
          SC := TMUIScrollbar(Widget.HScroll);
      end;
    end;
    if not Assigned(SC) then
      Exit;
    ScrollInfo.nMin := SC.MinValue;
    ScrollInfo.nMax := SC.MaxValue;
    ScrollInfo.nPage := SC.PageSize;
    ScrollInfo.nPos := SC.Position;
    Result := True;
  end; //}
end;

const
  SysColorMap: array [0..MAX_SYS_COLORS] of DWORD = (
    $C0C0C0,     {COLOR_SCROLLBAR}
    $808000,     {COLOR_BACKGROUND}
    $800000,     {COLOR_ACTIVECAPTION}
    $808080,     {COLOR_INACTIVECAPTION}
    $C0C0C0,     {COLOR_MENU}
    $FFFFFF,     {COLOR_WINDOW}
    $000000,     {COLOR_WINDOWFRAME}
    $000000,     {COLOR_MENUTEXT}
    $000000,     {COLOR_WINDOWTEXT}
    $FFFFFF,     {COLOR_CAPTIONTEXT}
    $C0C0C0,     {COLOR_ACTIVEBORDER}
    $C0C0C0,     {COLOR_INACTIVEBORDER}
    $808080,     {COLOR_APPWORKSPACE}
    $800000,     {COLOR_HIGHLIGHT}
    $FFFFFF,     {COLOR_HIGHLIGHTTEXT}
    $D0D0D0,     {COLOR_BTNFACE}
    $808080,     {COLOR_BTNSHADOW}
    $808080,     {COLOR_GRAYTEXT}
    $000000,     {COLOR_BTNTEXT}
    $C0C0C0,     {COLOR_INACTIVECAPTIONTEXT}
    $F0F0F0,     {COLOR_BTNHIGHLIGHT}
    $000000,     {COLOR_3DDKSHADOW}
    $C0C0C0,     {COLOR_3DLIGHT}
    $000000,     {COLOR_INFOTEXT}
    $AEF3F3,     {COLOR_INFOBK}
    $000000,     {unassigned}
    $000000,     {COLOR_HOTLIGHT}
    $800000,     {COLOR_GRADIENTACTIVECAPTION}
    $808080,     {COLOR_GRADIENTINACTIVECAPTION}
    $800000,     {COLOR_MENUHILIGHT}
    $D0D0D0,     {COLOR_MENUBAR}
    $D0D0D0      {COLOR_FORM}
  ); {end _SysColors}


function TMUIWidgetSet.GetSysColor(nIndex: Integer): DWORD;
begin
  if (nIndex < 0) or (nIndex > MAX_SYS_COLORS) then
  begin
    Result := 0;
    DumpStack;
    DebugLn(SysUtils.Format('ERROR: [TMUIWidgetSet.GetSysColor] Bad Value: %d. Valid Range between 0 and %d', [nIndex, MAX_SYS_COLORS]));
  end
  else
    Result := SysColorMap[nIndex];
end;

function TMUIWidgetSet.GetSystemMetrics(nIndex: Integer): Integer;
var
  Sc: PScreen;
begin
  //writeln('get system metrics ', nIndex);
  Sc := LockPubscreen('Workbench');
  if Assigned(Sc) then
  begin
    case nIndex of
      //Current screen size
      SM_CXSCREEN: Result:= Sc^.Width;
      SM_CYSCREEN: Result:= Sc^.Height;
      //Desktop size
      SM_CXVIRTUALSCREEN: Result:=Sc^.Width;
      SM_CYVIRTUALSCREEN: Result:=Sc^.Height;
    end;
    UnlockPubScreen('Workbench', Sc);
  end;
end;

function TMUIWidgetSet.GetTextExtentPoint(DC: HDC; Str: PChar;
  Count: Integer; var Size: TSize): Boolean;
var
  ADC: TMUICanvas absolute DC;
begin
  Result := False;
  inherited;
  Size.cy := 18;
  Size.cx := Count * 11;
  if Assigned(ADC) then
  begin
    //if Assigned(ADC.RastPort) then
    begin
      Size.cx := ADC.TextWidth(Str, Count);
      Size.cy := ADC.TextHeight(Str, Count);
      Result := True;
    end;
  end;
end;

function TMUIWidgetSet.GetTextMetrics(DC: HDC; var TM: TTextMetric): Boolean;
var
  ADC: TMUICanvas absolute DC;
begin
  {$ifdef VERBOSEAROS}
  writeln('Get TextMetric');
  {$endif}
  FillByte(TM, SizeOf(TM), 0);
  TM.tmAscent := 2;
  TM.tmDescent := 2;
  TM.tmAveCharWidth := 8;
  TM.tmHeight := 11;
  //Defined usually in MSDN as the average of 'x' char.
  if Assigned(ADC) then
  begin
    TM.tmAveCharWidth := ADC.TextWidth('x', 1);
    TM.tmHeight := ADC.TextHeight('X', 1);
  end;
  Result := True;
end;

function TMUIWidgetSet.GetWindowOrgEx(dc: hdc; P: PPoint): Integer;
var
  ADC: TMUICanvas absolute DC;
begin
  {$ifdef VERBOSEAROS}
  writeln('Get Window org Ex');
  {$endif}
  if Assigned(P) then
  begin
    P^.X := ADC.Offset.X;//ADC.Left;
    P^.Y := ADC.Offset.Y;//ADC.Top;
  end;
  Result:=1;
end;

function TMUIWidgetSet.GetWindowRect(Handle: hwnd; var ARect: TRect
  ): Integer;
var
  PrivateWidget: TMUIObject absolute Handle;
begin
  {$ifdef VERBOSEAROS}
  writeln('Get Window rect');
  {$endif}
  ARect:=Rect(PrivateWidget.Left, PrivateWidget.Top, PrivateWidget.Width, PrivateWidget.Height);
  Result:=1;
end;

function TMUIWidgetSet.GetWindowSize(Handle: hwnd; var Width, Height: Integer
  ): boolean;
var
  Widget: TMUIObject absolute Handle;
begin
  {$ifdef VERBOSEAROS}
  writeln('Get Window Size');
  {$endif}
  Width := Widget.Width;
  Height := Widget.Height;
  Result := True;
end;

function TMUIWidgetSet.GradientFill(DC: HDC; Vertices: PTriVertex; NumVertices : Longint; Meshes: Pointer; NumMeshes : Longint; Mode : Longint): Boolean;
begin
  //writeln('GradientFill');
  Result := True;
end;

function TMUIWidgetSet.HideCaret(hWnd: HWND): Boolean;
var
  Widget: TMUIObject absolute hWnd;
begin
  {$ifdef VERBOSEAROS}
  writeln('Hide Caret');
  {$endif}
  Result := False;
  if Assigned(Widget) then
  begin
    if Assigned(Widget.Caret) then
      Widget.Caret.Shown := False;
  end;
end;

function TMUIWidgetSet.InvalidateRect(aHandle: HWND; Rect: pRect;
  bErase: Boolean): Boolean;
var
  PrivateWidget: TMUIObject absolute aHandle;
begin
  {$ifdef VERBOSEAROS}
  writeln('-->invalidate ', bErase, ' ', HexStr(PrivateWidget));
  writeln('  ', PrivateWidget.classname);
  if Assigned(Rect) then
  begin
    writeln('Rect: ', Rect^.Left, ', ', Rect^.Top, ' - ', Rect^.Right,', ', Rect^.Bottom);
  end else
    writeln('Rect = nil;');
  {$endif}
  Result := False;
  if (Rect^.Right - Rect^.Left = 0) or (Rect^.Bottom - Rect^.Top = 0) then
  begin
    {$ifdef VERBOSEAROS}
    writeln('<<<< Exit Invalidate');
    {$endif}
    Exit;
  end;
  if Assigned(PrivateWidget) then
  begin
    MUIApp.AddInvalidatedObject(PrivateWidget);
    Result := True;
  end;
  {$ifdef VERBOSEAROS}
  writeln('<--invalidate ', bErase);
  {$endif}
end;

procedure TMUIWidgetSet.InitializeCriticalSection(var CritSection: TCriticalSection);
var
  ACritSec: System.PRTLCriticalSection;
begin
  New(ACritSec);
  System.InitCriticalSection(ACritSec^);
  CritSection:=TCriticalSection(ACritSec);
end;

procedure TMUIWidgetSet.LeaveCriticalSection(var CritSection: TCriticalSection);
var
  ACritSec: System.PRTLCriticalSection;
begin
  ACritSec:=System.PRTLCriticalSection(CritSection);
  System.LeaveCriticalsection(ACritSec^);
end;

function TMUIWidgetSet.LineTo(DC: HDC; X, Y: Integer): Boolean;
var
  ADC: TMUICanvas absolute DC;
begin
  {$ifdef VERBOSEAROS}
  writeln('-->LineTo ', x, ', ', y ,', ', HexStr(ADC), ' RastPort: ', HexStr(ADC.RastPort));
  {$endif}
  Result := False;
  inherited;
  if Assigned(ADC) and Assigned(ADC.RastPort) then
  begin
    ADC.LineTo(X, Y);
    Result := True;
  end;
end;

{ Most of the functionality is implemented. As described in MSDN:
  http://msdn.microsoft.com/en-us/library/windows/desktop/ms645505%28v=vs.85%29.aspx }
function TMUIWidgetSet.MessageBox(hWnd: HWND; lpText, lpCaption: PChar;
  uType: Cardinal): integer;
var
  ES: PEasyStruct;
  Buttons: string;
  Res: LongInt;
  BtnType: LongWord;
begin
  New(ES);
  ES^.es_StructSize := SizeOf(TEasyStruct);
  ES^.es_Flags := 0;
  ES^.es_Title := PChar(lpCaption);
  ES^.es_TextFormat := PChar(lpText);
  BtnType := (uType and $0000000F);

  case BtnType of
    MB_OKCANCEL: Buttons := 'OK|Cancel';
    MB_ABORTRETRYIGNORE: Buttons := 'Abort|Retry|Ignore';
    MB_YESNOCANCEL: Buttons := 'Yes|No|Cancel';
    MB_YESNO: Buttons := 'Yes|No';
    MB_RETRYCANCEL: Buttons := 'Retry|Cancel';
    MB_CANCELTRYCONTINUE: Buttons := 'Abort|Retry|Ignore';
  else
    Buttons := 'OK';
  end;
  ES^.es_GadgetFormat := PChar(Buttons);
  //
  Res := EasyRequestArgs(nil, ES, nil, nil);
  Result := mrCancel;
  case BtnType of
    MB_OKCANCEL: begin
      if Res = 0 then
        Result := mrOK
      else
        Result := mrCancel;
    end;
    MB_ABORTRETRYIGNORE: begin
      if Res = 0 then
        Result := mrAbort;
      if Res = 1 then
        Result := mrRetry;
      if Res = 2 then
        Result := mrIgnore;
    end;
    MB_YESNOCANCEL: begin
      if Res = 0 then
        Result := mrYes;
      if Res = 1 then
        Result := mrNo;
      if Res = 2 then
        Result := mrCancel;
    end;
    MB_YESNO:begin
      if Res = 0 then
        Result := mrYes;
      if Res = 1 then
        Result := mrNo;
    end;
    MB_RETRYCANCEL:begin
      if Res = 0 then
        Result := mrRetry;
      if Res = 1 then
        Result := mrCancel;
    end;
    MB_CANCELTRYCONTINUE: begin
      if Res = 0 then
        Result := mrCancel;
      if Res = 1 then
        Result := mrRetry;
      if Res = 2 then
        Result := mrIgnore;
    end;
  end;
  Dispose(ES);
end;



(*var
  Str: AnsiString;
  TitleStr: AnsiString;
  Buttons : TfpgMsgDlgButtons;
  BtnType: Cardinal;
  DlgType: Cardinal;*)
//begin
//  Result := 0;
(*  BtnType := (uType and $0000000F); { mask the button type }

  if (BtnType = MB_OKCANCEL) then
    Buttons := mbOKCancel
  else
  if (BtnType = MB_ABORTRETRYIGNORE) then
    Buttons := mbAbortRetryIgnore
  else
  if (BtnType = MB_YESNOCANCEL) then
    Buttons := mbYesNoCancel
  else
  if (BtnType = MB_YESNO) then
    Buttons := mbYesNo
  else
  if (BtnType = MB_RETRYCANCEL) then
    Buttons := [mbRetry, mbCancel]
  else
  if (BtnType = MB_CANCELTRYCONTINUE) then
    Buttons := mbAbortRetryIgnore
  else
    Buttons := [mbOK];

  { shoud we had a Help button too? - again as per MSDN }
  if (uType and MB_HELP) = MB_HELP then
    Include(Buttons, mbHelp);

  Str := lpText;
  TitleStr := lpCaption;
  if lpCaption = nil then
    TitleStr := 'Error'; // as per MSDN

  DlgType := (uType and $000000F0); { mask the dialog type }

  if (DlgType and MB_ICONINFORMATION) = MB_ICONINFORMATION then
    TfpgMessageDialog.Information(TitleStr, Str, Buttons)
  else
  if (DlgType and MB_ICONWARNING) = MB_ICONWARNING then
    TfpgMessageDialog.Warning(TitleStr, Str, Buttons)
  else
  if (DlgType and MB_ICONQUESTION) = MB_ICONQUESTION then
    TfpgMessageDialog.Question(TitleStr, Str, Buttons)
  else
  if (DlgType and MB_ICONERROR) = MB_ICONERROR then
    TfpgMessageDialog.Critical(TitleStr, Str, Buttons)
  else
    TfpgMessageDialog.Information(TitleStr, Str, Buttons);*)
//end;

function TMUIWidgetSet.MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint): Boolean;
var
  ADC: TMUICanvas absolute DC;
begin
  Result := False;
  {$ifdef VERBOSEAROS}
  writeln('Move to : ', x, ', ', y);
  {$endif}
  inherited;
  if Assigned(ADC) and Assigned(ADC.RastPort) then
  begin
    if Assigned(OldPoint) then
    begin
      OldPoint^.X := ADC.Position.X;
      OldPoint^.Y := ADC.Position.Y;
    end;
    ADC.MoveTo(X, Y);
    Result := True;
  end;
end;

function TMUIWidgetSet.Polygon(DC: HDC; Points: PPoint; NumPts: Integer; Winding: boolean): boolean;
var
  ADC: TMUICanvas absolute DC;
begin
  Result := False;
  {$ifdef VERBOSEAROS}
  writeln('Polygon ', IntToStr(NumPts));
  {$endif}
  if Assigned(ADC) and (NumPts > 0) and Assigned(Points) then
  begin
    ADC.Polygon(Points, NumPts);
    Result := True;
  end;
end;

function TMUIWidgetSet.Polyline(DC: HDC; Points: PPoint; NumPts: Integer): boolean;
var
  CurPoint: PPoint;
  i: Integer;
  ADC: TMUICanvas absolute DC;
begin
  {$ifdef VERBOSEAROS}
  writeln('Polyline ', IntToStr(NumPts));
  {$endif}
  Result := False;
  if Assigned(ADC) and Assigned(Points) and (NumPts > 0) then
  begin
    Result := True;
    CurPoint := Points;
    ADC.MoveTo(CurPoint^.X, CurPoint^.Y);
    for i := 1 to NumPts - 1 do
    begin
      Inc(CurPoint);
      ADC.LineTo(CurPoint^.X, CurPoint^.Y);
    end;
  end;
end;


function TMUIWidgetSet.Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean;
var
  ADC: TMUICanvas absolute DC;
begin
  {$ifdef VERBOSEAROS}
  writeln('Rectangle ', X1, ', ', Y1, ' - ', X2, ', ', Y2);
  {$endif}
  Result := False;
  if Assigned(ADC) then
  begin
    ADC.Rectangle(X1, Y1, X2, Y2);
    Result := True;
  end;

end;

function TMUIWidgetSet.RectVisible(dc : hdc; const ARect: TRect) : Boolean;
begin
  {$ifdef VERBOSEAROS}
  writeln('Rect Visible ', ARect.Left, ',', ARect.Right);
  {$endif}
  Result := Boolean(1);
end;

function TMUIWidgetSet.ReleaseCapture: Boolean;
begin
  Result := True;
end;

function TMUIWidgetSet.ReleaseDC(hWnd: HWND; DC: HDC): Integer;
var
  Widget: TMUIObject absolute hWnd;
  ADC: TMUICanvas absolute DC;
begin
  {$ifdef VERBOSEAROS}
  writeln('release dc ', HexStr(Widget), ' ',HexStr(ADC));
  {$endif}
  Result := 0;
  if not Assigned(Widget) then // Only Release if not attached to a Widget
  begin
    ADC.Free;
    Result := 1;
  end;
end;

function TMUIWidgetSet.RestoreDC(DC: HDC; SavedDC: Integer): Boolean;
//var
//  ADC: TFPGUIDeviceContext absolute DC;
begin
  //Result:=ADC.RestoreDC(SavedDC);
  Result := False;
end;

function TMUIWidgetSet.RoundRect(DC : hDC; X1, Y1, X2, Y2: Integer; RX,RY : Integer): Boolean;
begin
  Result := True;
  Rectangle(DC, x1, y1, x2, y2);
end;

function TMUIWidgetSet.SaveDC(DC: HDC): Integer;
//var
//  ADC: TFPGUIDeviceContext absolute DC;
begin
  //Result:=ADC.SaveDC;
  Result := 0;
end;

function TMUIWidgetSet.ScreenToClient(Handle : HWND; var P : TPoint) : Integer;
var
  Widget: TMUIObject absolute Handle;
begin
  {$ifdef VERBOSEAROS}
  writeln('screen to client start ', P.X ,' ', HexStr(Widget));
  {$endif}
  if Assigned(Widget) then
  begin
    //TODO: get real left and top border from Widget
    P.X := P.X - 15;
    P.Y := P.Y - 30;
    repeat
      P.X := P.X - Widget.Left;
      P.Y := P.Y - Widget.Top;
      Widget := Widget.Parent;
    until not Assigned(Widget) or (Widget is TMUIApplication);
  end;
  {$ifdef VERBOSEAROS}
  writeln('screen to client end ', P.X);
  {$endif}
  Result := 1;
end;

function TMUIWidgetSet.SelectClipRGN(DC: hDC; RGN: HRGN): Longint;
var
  ADC: TMUICanvas absolute DC;
  Reg: TMUIBasicRegion absolute RGN;
begin
  {$ifdef VERBOSEAROS}
  writeln('select Clip Rgn $', HexStr(Pointer(RGN)));
  {$endif}
  if Assigned(ADC) then
  begin
    ADC.SetClipping(Reg)
  end;
  Result:=SimpleRegion;
end;

function TMUIWidgetSet.SelectObject(DC: HDC; GDIObj: HGDIOBJ): HGDIOBJ;
var
  MyDC: TMUICanvas absolute DC;
  GDI: TMUIWinAPIObject absolute GDIObj;
begin
  {$ifdef VERBOSEAROS}
  writeln('  select object ', HexStr(Pointer(GDIObj)), ' ', GDI.classname,' DC ' , HexStr(MyDC));
  {$endif}
  Result := 0;
  if not Assigned(WinObjList) then
    Exit;
  if WinObjList.IndexOf(GDI) < 0 then
    Exit;
  if Assigned(MyDC) and Assigned(GDI) and (GDIObj > $100) then
  begin
    if (TObject(GDIObj) is TMUIWinAPIObject) then
      Result := HGDIOBJ(MyDC.SelectObject(GDI));
  end;
end;

function TMUIWidgetSet.SetBKColor(DC: HDC; Color: TColorRef): TColorRef;
var
  ADC: TMUICanvas absolute DC;
begin
  Result := 0;
  if Assigned(ADC) then
  begin
    Result := ADC.BKColor;
    ADC.BKColor := Color;
  end;
end;

function TMUIWidgetSet.SetBkMode(DC: HDC; bkMode : Integer) : Integer;
var
  ADC: TMUICanvas absolute DC;
begin
  Result := 0;
  if Assigned(ADC) then
  begin
    Result := ADC.BKMode;
    ADC.BKMode := bkMode;
  end;
end;

function TMUIWidgetSet.SetCapture(AHandle: HWND): HWND;
var
  Widget: TMUIObject absolute AHandle;
begin
  Result := HWND(CaptureObj);
  CaptureObj := Widget;
  if CaptureObj<>nil then
    SendMessage(HWnd({%H-}PtrUInt(CaptureObj.PasObject)), LM_CAPTURECHANGED, 0, Result);
end;

function TMUIWidgetSet.SetFocus(hWnd: HWND): HWND;
var
  Widget: TMUIObject absolute hWnd;
  Obj: TMUIObject;
  Win: TMUIWindow;
begin
  FocusWidget := hwnd;
  Result := 0;
  if Assigned(Widget) then
  begin
    Win := nil;
    Obj := Widget;
    while Assigned(Obj) do
    begin
      if Obj is TMUIWindow then
      begin
        Win := TMUIWindow(Obj);
        Break;
      end;
      Obj := Obj.Parent;
    end;
    if Assigned(Win) then
    begin
      if Assigned(Win.FocusedControl) then
        LCLSendKillFocusMsg(Win.FocusedControl.PasObject);
      Result := LCLType.HWND(Win.FocusedControl);
      Win.FocusedControl := Widget;
      LCLSendSetFocusMsg(Widget.PasObject);
    end;
  end;
end;

function TMUIWidgetSet.SetParent(hWndChild: HWND; hWndParent: HWND): HWND;
//var
//  PrivateWidgetParent: TFPGUIPrivateWidget absolute hWndParent;
//  PrivateWidgetChild: TFPGUIPrivateWidget absolute hWndChild;
begin
//  PrivateWidgetChild.Widget.Parent:=PrivateWidgetParent.Widget;
  Result:=0; //???
end;

function TMUIWidgetSet.SetScrollInfo(Handle : HWND; SBStyle : Integer; ScrollInfo: TScrollInfo; bRedraw : Boolean): Integer;
var
  Widget: TMUIObject absolute Handle;
  Sc: TMUIScrollbar;
begin
  //writeln('-->SetScrollInfo');
  Result := 0;
  //writeln(' Set ScrollInfo ',SBStyle,' m:', ScrollInfo.NMax, ' page:', ScrollInfo.nPage, ' pos:', ScrollInfo.nPos);
  if (ScrollInfo.NMax = 0) and (ScrollInfo.NPage = 0) and (ScrollInfo.NMin = 0) then
    Exit;
  if Assigned(Widget) then
  begin
    //if not Assigned(Widget.VScroll) or not Assigned(Widget.HScroll) then
    //  Widget.CreateScrollbars;
    //writeln('SetScrollInfo ', Widget.classname,' ', SBStyle,' ', HexStr(Pointer(ScrollInfo.fMask)));
    if not Widget.Visible then
      Exit;
    Sc := nil;
    if SBStyle = SB_CTL then
    begin
      SC := TMUIScrollbar(Widget);
    end
    else
    if SBStyle = SB_Vert then
    begin
      SC := TMUIScrollbar(Widget.VScroll);
      if ((SIF_POS and ScrollInfo.fMask) <> 0) and (Widget.VScrollPos <> ScrollInfo.nPos) then
        Widget.VScrollPos := ScrollInfo.nPos;
    end
    else
    begin
      if SBStyle = SB_HORZ then
      begin
        SC := TMUIScrollbar(Widget.HScroll);
        if ((SIF_POS and ScrollInfo.fMask) <> 0) and (Widget.HScrollPos <> ScrollInfo.nPos) then
          Widget.HScrollPos := ScrollInfo.nPos;
      end;
    end;
    if not Assigned(SC) then
      Exit;
    //
    if ((SIF_PAGE and ScrollInfo.fMask) <> 0) and (SC.PageSize <> ScrollInfo.nPage) and (ScrollInfo.nPage <> 0) then
    begin
      //writeln('Set Page ', ScrollInfo.nPage);
      SC.PageSize := ScrollInfo.nPage;
    end;
    if (SIF_RANGE and ScrollInfo.fMask) <> 0 then
    begin
      //writeln('->Set min max ', ScrollInfo.nMin, ' max: ', ScrollInfo.nMax);
      if SC.MinValue <> ScrollInfo.nMin then
        SC.MinValue := ScrollInfo.nMin;
      if SC.MaxValue <> ScrollInfo.nMax then
        SC.MaxValue := ScrollInfo.nMax;
      //writeln('<-Set min max ', ScrollInfo.nMin, ' max: ', ScrollInfo.nMax);
    end;
    if ((SIF_POS and ScrollInfo.fMask) <> 0) and (SC.Position <> ScrollInfo.nPos) then
    begin
      SC.Position := ScrollInfo.nPos;
    end;
    if not SC.Visible then
      SC.Visible := True;
    Result := SC.Position;
  end;
  //writeln('<--SetScrollInfo');
end;

function TMUIWidgetSet.SetTextColor(DC: HDC; Color: TColorRef): TColorRef;
var
  ADC: TMUICanvas absolute DC;
begin
  if Assigned(ADC) then
  begin
    Result := ADC.TextColor;
    ADC.TextColor := Color;
  end;
end;

function TMUIWidgetSet.SetWindowOrgEx(DC: HDC; NewX, NewY: Integer;
  OldPoint: PPoint): Boolean;
var
  ADC: TMUICanvas absolute DC;
begin
  {$ifdef VERBOSEAROS}
  writeln('################set window org ex ', NewX, ', ', NewY);
  {$endif}
  if Assigned(OldPoint) then
  begin
    OldPoint^.X := ADC.Offset.X;
    OldPoint^.Y := ADC.Offset.Y;
  end;
  ADC.Offset.X := ADC.Offset.X - NewX;
  ADC.Offset.Y := ADC.Offset.Y - NewY;
  Result:=True;
end;

function TMUIWidgetSet.ShowCaret(hWnd: HWND): Boolean;
var
  Widget: TMUIObject absolute hWnd;
begin
  {$ifdef VERBOSEAROS}
  writeln('Show Caret');
  {$endif}
  Result := False;
  if Assigned(Widget) then
  begin
    if Assigned(Widget.Caret) then
      Widget.Caret.Shown := True;
  end;
end;

function TMUIWidgetSet.ShowScrollBar(Handle: HWND; wBar: Integer; bShow: Boolean): Boolean;
var
  Widget: TMUIObject absolute Handle;
  ReDo: Boolean;
  Created: Boolean;
begin
  //writeln('Show ScrollBar ', HexStr(Widget),' ', wBar, ' Show: ', bShow);
  Result := False;
  ReDo := False;
  Created := False;
  if Assigned(Widget) then
  begin
    Result := True;
    if bShow and (not Assigned(Widget.VScroll) or not Assigned(Widget.HScroll)) then
    begin
      Widget.CreateScrollbars;
      Created := True;
    end;
    if wBar = SB_Vert then
    begin
      if Assigned(Widget.VScroll) then
      begin
        if Widget.VScroll.Visible <> bShow then
        begin
          Widget.VScroll.Visible := bShow;
          ReDo := True;
        end;
      end;
    end;
    if wBar = SB_Horz then
    begin
      if Assigned(Widget.HScroll) then
      begin
        Widget.HScroll.Visible := bShow;
        ReDo := True;
      end;
    end;
    if (Widget.PasObject is TWinControl) and ReDo and not Created then
      TWinControl(Widget.PasObject).InvalidateClientRectCache(False);
  end;
end;


function TMUIWidgetSet.SetCaretPos(X, Y: Integer): Boolean;
begin
  Result := False;
end;

function TMUIWidgetSet.SetCaretPosEx(Handle: HWnd; X, Y: Integer): Boolean;
var
  Widget: TMUIObject absolute Handle;
begin
  Result := False;
  //writeln('SetCarePosEx');
  if Assigned(Widget) then
    MUIApp.AddInvalidatedObject(Widget);
end;

function TMUIWidgetSet.ShowWindow(hWnd: HWND; nCmdShow: Integer): Boolean;
var
  Widget: TMUIObject absolute hWnd;
begin
  Result := Widget.Visible;
  Widget.Visible := True;
end;

type
  PARGBColor = ^TARGBColor;
  TARGBColor = packed record
    A: Byte;
    R: Byte;
    G: Byte;
    B: Byte;
  end;

function TMUIWidgetSet.StretchBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; ROp: Cardinal
  ): Boolean;
var
  Dest: TMUICanvas absolute DestDC;
  Src: TMUICanvas absolute SrcDC;
  i,xs,ys: Integer;
  NImage: Pointer;
  DB,SB: PLongWord;
  FX,FY: Double;
  NX, NY: Integer;
  LineStart: PLongWord;
  ScaledBitmap: AGraphics.PBitmap;
  Bsa: TBitScaleArgs;
  {$ifdef Amiga68k}
  OldCol: LongWord;
  a1, a2: LongInt;
  {$endif}
begin
  {$ifdef VERBOSEAROS}
  writeln('StretchBlt $', HexStr(Pointer(DestDC)), ', $',HexStr(Pointer(SrcDC)),', $',HexStr(Pointer(Mask)));
  {$endif}
  if Assigned(Dest) and Assigned(Src) and Assigned(Src.Bitmap) then
  begin
    Dest.Drawn := True;
    {$ifdef Amiga68k}
    if (SrcWidth = Width) and (SrcHeight = height) and (Src.Drawn and UseAmigaAlpha) then
    {$else}
    if (SrcWidth = Width) and (SrcHeight = height) then
    {$endif}
    begin
      if Src.Drawn then // means something was drawn on -> no alpha anymore :(
      begin
        ClipBlit(Src.RastPort, xSrc, YSrc, Dest.RastPort, Dest.GetOffset.X + x, Dest.GetOffset.Y + y, width, height, $c0);
      end else
      begin
        {$ifndef AMIGA68k}
        WritePixelArrayAlpha(Src.Bitmap.FImage, XSrc, YSrc, Src.Bitmap.FWidth * SizeOf(LongWord), Dest.RastPort, Dest.GetOffset.X + x, Dest.GetOffset.Y + y, width, height, DEFALPHAVALUE)
        {$else}
        if Assigned(CyberGfxBase) then
          Cybergraphics.WritePixelArray(Src.Bitmap.FImage, XSrc, YSrc, Src.Bitmap.FWidth * SizeOf(LongWord), Dest.RastPort, Dest.GetOffset.X + x, Dest.GetOffset.Y + y, width, height, RECTFMT_ARGB)
        {$endif}
      end;
    end else
    begin
      if Src.Drawn then
      begin
        ScaledBitmap := AllocBitMap(Width, Height, 32, BMF_MINPLANES or BMF_DISPLAYABLE, IntuitionBase^.ActiveScreen^.RastPort.Bitmap);
        with bsa do
        begin
          bsa_SrcX := XSrc;
          bsa_SrcY := YSrc;
          bsa_SrcWidth := SrcWidth;
          bsa_SrcHeight := SrcHeight;
          bsa_XSrcFactor := SrcWidth;
          bsa_YSrcFactor := SrcHeight;
          bsa_DestX := 0;
          bsa_DestY := 0;
          bsa_DestWidth := Width;
          bsa_DestHeight := Height;
          bsa_XDestFactor := Width;
          bsa_YDestFactor := Height;
          bsa_SrcBitmap := Src.RastPort^.Bitmap;
          bsa_DestBitmap := ScaledBitmap;
          bsa_Flags := 0;
          bsa_XDDA := 0;
          bsa_YDDA := 0;
          bsa_Reserved1 := 0;
          bsa_Reserved2 := 0;
        end;
        BitmapScale(@bsa);
        BltBitMapRastPort(ScaledBitmap, 0, 0, Dest.RastPort, Dest.GetOffset.X + x, Dest.GetOffset.Y + y, width, height, $c0);
        FreeBitmap(ScaledBitmap);
      end else
      begin
        //writeln('StretchBlt Width: ', Width, ' Height: ', Height);
        NImage := System.AllocMem(Width * (Height + 10) * SizeOf(LongWord));
        DB := NImage;
        FX := 1;
        FY := 1;
        if SrcWidth > 0 then
          FX := SrcWidth/Width;
        if SrcHeight > 0 then
          FY := SrcHeight/Height;
        for ys := 0 to Height - 1 do
        begin
          NY := Min(Src.Bitmap.FHeight - 1, Round(ys * FY));
          i := NY * SrcWidth;
          LineStart := Src.Bitmap.FImage;
          Inc(LineStart, i);
          for xs := 0 to Width - 1 do
          begin
            NX := Min(Src.Bitmap.FWidth - 1, Round(xs * FX));
            SB := LineStart;
            Inc(SB, NX);
            {$ifdef Amiga68k}
            if PARGBColor(SB)^.A = 0 then
            begin
              DB^ := ReadRGBPixel(Dest.RastPort, NX, NY);
            end
            else
            begin
              if PARGBColor(SB)^.A = 255 then
              begin
                DB^ := SB^;
              end
              else
              begin
                OldCol := ReadRGBPixel(Dest.RastPort, NX, NY);
                a1 := PARGBColor(SB)^.A;
                a2 := 255 - a1;
                PARGBColor(DB)^.A := 255;
                PARGBColor(DB)^.R := Min(255, ((TARGBColor(OldCol).R * a2) + (PARGBColor(SB)^.R * a1)) div 255);
                PARGBColor(DB)^.G := Min(255, ((TARGBColor(OldCol).G * a2) + (PARGBColor(SB)^.G * a1)) div 255);
                PARGBColor(DB)^.B := Min(255, ((TARGBColor(OldCol).B * a2) + (PARGBColor(SB)^.B * a1)) div 255);
              end;
            end;
            {$else}
            DB^ := SB^;
            {$endif}
            Inc(DB);
          end;
        end;
        if Assigned(CyberGfxBase) then
          Cybergraphics.WritePixelArray(NImage, XSrc, YSrc, Width * SizeOf(LongWord), Dest.RastPort, Dest.GetOffset.X + x, Dest.GetOffset.Y + y, width, height, RECTFMT_ARGB);
        System.FreeMem(NImage);
      end;
    end;
    //ScalePixelArray(Src.Bitmap.FImage, SrcWidth, SrcHeight, SrcWidth * SizeOf(LongWord), Dest.RastPort, Dest.GetOffset.X + x, Dest.GetOffset.Y + y, width, height, RECTFMT_RGBA);
    //writeln('wrote: ', i, ', ', Dest.GetOffset.X + x, ', ', Dest.GetOffset.Y + Y, ', ', Width, ', ', Height,' - ', SrcWidth,',', SrcHeight);
  end;
  Result := True;
end;

function TMUIWidgetSet.StretchMaskBlt(DestDC: HDC; X, Y, Width,
  Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer;
  Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean;
var
  Dest: TMUICanvas absolute DestDC;
  Src: TMUICanvas absolute SrcDC;
  i,xs,ys: Integer;
  NImage: Pointer;
  DB,SB: PLongWord;
  FX,FY: Double;
  NX, NY: Integer;
  LineStart: PLongWord;
  //Sptr, Dptr: PByte;
  ScaledBitmap: AGraphics.PBitmap;
  Bsa: TBitScaleArgs;
  {$ifdef Amiga68k}
  OldCol: LongWord;
  a1, a2: LongInt;
  {$endif}
begin
  {$ifdef VERBOSEAROS}
  writeln('StretchMaskBlt $', HexStr(Pointer(DestDC)), ', $',HexStr(Pointer(SrcDC)),', $',HexStr(Pointer(Mask)));

  writeln('   SRC  ', Assigned(Src),',',Assigned(Src.Bitmap),',',Assigned(Src.RastPort),',',assigned(Src.MUIObject));
  writeln('   DEST ', Assigned(Dest),',',Assigned(Dest.Bitmap),',',Assigned(Dest.RastPort),',',assigned(Dest.MUIObject));
  {$endif}
  if Assigned(Dest) and Assigned(Src) and Assigned(Src.Bitmap) and Assigned(Dest.RastPort) and assigned(Src.Bitmap.FImage) then
  begin
    Dest.Drawn := True;
    {$ifdef Amiga68k}
    if (SrcWidth = Width) and (SrcHeight = height) and (Src.Drawn and UseAmigaAlpha) then
    {$else}
    if (SrcWidth = Width) and (SrcHeight = height) then
    {$endif}
    begin
      //WritePixelArray(Src.Bitmap.FImage, XSrc, YSrc, Src.Bitmap.FWidth * SizeOf(LongWord), Dest.RastPort, Dest.GetOffset.X + x, Dest.GetOffset.Y + y, width, height, PIXFMT_0RGB32);
      {$ifdef VERBOSEAROS}
      writeln('   wrote: ', Dest.GetOffset.X + x, ', ', Dest.GetOffset.Y + Y, ', ', Width, ', ', Height,' - ', SrcWidth,',', SrcHeight, ', ', Src.Bitmap.FWidth);
      writeln('   Draw Bitmap: ', HexStr(Src.Bitmap), ' width: ', Src.Bitmap.FWidth, ' ', Src.Drawn);
      {$endif}
      if Src.Drawn then // means something was drawn on -> no alpha anymore :(
      begin
        ClipBlit(Src.RastPort, xSrc, YSrc, Dest.RastPort, Dest.GetOffset.X + x, Dest.GetOffset.Y + y, width, height, $c0);
      end else
      begin
        //WritePixelArray(Src.Bitmap.FImage, XSrc, YSrc, Src.Bitmap.FWidth * SizeOf(LongWord), Dest.RastPort, Dest.GetOffset.X + x, Dest.GetOffset.Y + y, width, height, PIXFMT_ARGB32);
        {$ifndef AMIGA68k}
        WritePixelArrayAlpha(Src.Bitmap.FImage, XSrc, YSrc, Src.Bitmap.FWidth * SizeOf(LongWord), Dest.RastPort, Dest.GetOffset.X + x, Dest.GetOffset.Y + y, width, height, DEFALPHAVALUE);
        {$else}
        if Assigned(CyberGfxBase) then
          cybergraphics.WritePixelArray(Src.Bitmap.FImage, XSrc, YSrc, Src.Bitmap.FWidth * SizeOf(LongWord), Dest.RastPort, Dest.GetOffset.X + x, Dest.GetOffset.Y + y, width, height, RECTFMT_ARGB);
        {$endif}
      end;
    end else
    begin
      if Src.Drawn then
      begin
        ScaledBitmap := AllocBitMap(Width, Height, 32, BMF_MINPLANES or BMF_DISPLAYABLE, IntuitionBase^.ActiveScreen^.RastPort.Bitmap);
        with bsa do
        begin
          bsa_SrcX := XSrc;
          bsa_SrcY := YSrc;
          bsa_SrcWidth := SrcWidth;
          bsa_SrcHeight := SrcHeight;
          bsa_XSrcFactor := SrcWidth;
          bsa_YSrcFactor := SrcHeight;
          bsa_DestX := 0;
          bsa_DestY := 0;
          bsa_DestWidth := Width;
          bsa_DestHeight := Height;
          bsa_XDestFactor := Width;
          bsa_YDestFactor := Height;
          bsa_SrcBitmap := Src.RastPort^.Bitmap;
          bsa_DestBitmap := ScaledBitmap;
          bsa_Flags := 0;
          bsa_XDDA := 0;
          bsa_YDDA := 0;
          bsa_Reserved1 := 0;
          bsa_Reserved2 := 0;
        end;
        BitmapScale(@bsa);
        BltBitMapRastPort(ScaledBitmap, 0, 0, Dest.RastPort, Dest.GetOffset.X + x, Dest.GetOffset.Y + y, width, height, $c0);
        FreeBitmap(ScaledBitmap);
      end else
      begin
        //writeln('StretchMaskBlt Width: ', Width, ' Height: ', Height);
        NImage := System.AllocMem(Width * (Height + 10) * SizeOf(LongWord));
        DB := NImage;
        FX := 1;
        FY := 1;
        if SrcWidth > 0 then
          FX := SrcWidth/Width;
        if SrcHeight > 0 then
          FY := SrcHeight/Height;
        for ys := 0 to Height - 1 do
        begin
          NY := Min(Src.Bitmap.FHeight - 1, Round(ys * FY));
          i := NY * SrcWidth;
          LineStart := Src.Bitmap.FImage;
          Inc(LineStart, i);
          for xs := 0 to Width - 1 do
          begin
            NX := Min(Src.Bitmap.FWidth - 1, Round(xs * FX));
            SB := LineStart;
            Inc(SB, NX);
            {$ifdef Amiga68k}
            if PARGBColor(SB)^.A = 0 then
            begin
              DB^ := ReadRGBPixel(Dest.RastPort, NX, NY);
            end
            else
            begin
              if PARGBColor(SB)^.A = 255 then
              begin
                DB^ := SB^;
              end
              else
              begin
                OldCol := ReadRGBPixel(Dest.RastPort, NX, NY);
                a1 := PARGBColor(SB)^.A;
                a2 := 255 - a1;
                PARGBColor(DB)^.A := 255;
                PARGBColor(DB)^.R := Min(255, ((TARGBColor(OldCol).R * a2) + (PARGBColor(SB)^.R * a1)) div 255);
                PARGBColor(DB)^.G := Min(255, ((TARGBColor(OldCol).G * a2) + (PARGBColor(SB)^.G * a1)) div 255);
                PARGBColor(DB)^.B := Min(255, ((TARGBColor(OldCol).B * a2) + (PARGBColor(SB)^.B * a1)) div 255);
              end;
            end;
            {$else}
            DB^ := SB^;
            {$endif}
            Inc(DB);
          end;
        end;
        {$ifndef AMIGA68k}
        WritePixelArrayAlpha(NImage, XSrc, YSrc, Width * SizeOf(LongWord), Dest.RastPort, Dest.GetOffset.X + x, Dest.GetOffset.Y + y, width, height, DEFALPHAVALUE);
        {$else}
        if Assigned(CyberGfxBase) then
          Cybergraphics.WritePixelArray(NImage, XSrc, YSrc, Width * SizeOf(LongWord), Dest.RastPort, Dest.GetOffset.X + x, Dest.GetOffset.Y + y, width, height, RECTFMT_ARGB);
        {$endif}
        System.FreeMem(NImage);
      end;
      //writeln('stretchblt ok');
    end;
    //ScalePixelArray(Src.Bitmap.FImage, SrcWidth, SrcHeight, SrcWidth * SizeOf(LongWord), Dest.RastPort, Dest.GetOffset.X + x, Dest.GetOffset.Y + y, width, height, RECTFMT_RGBA);
    //writeln('wrote: ', i, ', ', Dest.GetOffset.X + x, ', ', Dest.GetOffset.Y + Y, ', ', Width, ', ', Height,' - ', SrcWidth,',', SrcHeight);
  end;
  Result := True;
  //writeln('end StretchMaskBlt');
end;

function TMUIWidgetSet.WindowFromPoint(Point: TPoint): HWND;
begin
  { TODO : Temporal hack while not real WindowFromPoint implementation }
  //Result:=HWND(GlobalMouseCursorPosWidget);
  Result := 0;
end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line

