{%MainUnit fpguiint.pp}

{******************************************************************************
  All FPGUI Winapi implementations.
  This are the implementations of the overrides of the FPGUI Interface for the
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

//##apiwiz##sps##   // Do not remove, no wizard declaration before this line

{------------------------------------------------------------------------------
  Function: BeginPaint
  Params:
  Returns:

  This function is Called:
  - Once on every OnPaint event
 ------------------------------------------------------------------------------}
function TFpGuiWidgetSet.BeginPaint(Handle: hWnd; Var PS : TPaintStruct): hdc;
var
  PrivateWidget: TFPGUIPrivateWidget absolute Handle;
  DC: TFpGuiDeviceContext;
begin
  {$ifdef VerboseFPGUIWinAPI}
    WriteLn('Trace:> [WinAPI BeginPaint] Handle=', dbghex(Handle));
  {$endif}
  {$WARNING TFpGuiWidgetSet.BeginPaint Temporary Fix to prevent Crashing}
  (*
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
  *)

  ps.hdc:=GetDC(Handle);

  {$ifdef VerboseFPGUIWinAPI}
    WriteLn('Trace:< [WinAPI BeginPaint] Result=', dbghex(Result));
  {$endif}
end;

function TFpGuiWidgetSet.ClientToScreen(Handle: HWND; var P: TPoint): Boolean;
var
  O: TFPGUIPrivateWidget absolute Handle;
begin
  p:=TfpgWindowBase(o.Widget).WindowToScreen(TfpgWindowBase(o.Widget),p);
  Result:=true;
end;

function TFpGuiWidgetSet.CombineRgn(Dest, Src1, Src2: HRGN;
  fnCombineMode: Longint): Longint;
var
  R1: TFPGUIBasicRegion absolute Src1;
  R2: TFPGUIBasicRegion absolute Src2;
  DR: TFPGUIBasicRegion absolute Dest;
  Combine: TFPGUIRegionCombine;
begin
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
end;

function TFpGuiWidgetSet.CreateBitmap(Width, Height: Integer; Planes,
  BitCount: Longint; BitmapBits: Pointer): HBITMAP;
var
  img: TFPGUIWinAPIBitmap;
begin
  if BitCount>0 then begin
    img:=TFPGUIWinAPIBitmap.Create(BitCount,Width,Height);
    Result:=HBITMAP(img);
  end else begin
    Result:=0;
  end;
end;

function TFpGuiWidgetSet.CreateBrushIndirect(const LogBrush: TLogBrush
  ): HBRUSH;
begin
  Result:=HBRUSH(TFPGUIWinAPIBrush.Create(LogBrush));
end;

function TFpGuiWidgetSet.CreateCompatibleBitmap(DC: HDC; Width, Height: Integer
  ): HBITMAP;
var
  img: TFPGUIWinAPIBitmap;
begin
  img:=TFPGUIWinAPIBitmap.Create(32,Width,Height);
  Result:=HBITMAP(img);
end;

function TFpGuiWidgetSet.CreateCompatibleDC(DC: HDC): HDC;
var
  ADC: TFpGuiDeviceContext absolute DC;
begin
  {$ifdef VerboseFPGUIWinAPI}
    if DC=0 then begin
      WriteLn(Self.ClassName,'.CreateCompatibleDC ','NULL');
    end else begin
      if ADC.FPrivateWidget<>nil then begin
        WriteLn(Self.ClassName,'.CreateCompatibleDC ',ADC.FPrivateWidget.LCLObject.Name);
      end else begin
        WriteLn(Self.ClassName,'.CreateCompatibleDC ','Desktop');
      end;
    end;
  {$endif}
  if DC=0 then begin
    //Create DC desktop compatible, or retrieve the destop one to avoid memory leask.
    Result:=HDC(FPGUIGetDesktopDC());
  end else begin
    //Create DC widget compatible
    Result:=HDC(TFpGuiDeviceContext.Create(ADC.FPrivateWidget));
  end;
end;

function TFpGuiWidgetSet.CreateFontIndirect(const LogFont: TLogFont): HFONT;
begin
  Result:=HFONT(TFPGUIWinAPIFont.Create(LogFont));
end;

function TFpGuiWidgetSet.CreateFontIndirectEx(const LogFont: TLogFont;
  const LongFontName: string): HFONT;
begin
  Result:=HFONT(TFPGUIWinAPIFont.Create(LogFont,LongFontName));
end;

function TFpGuiWidgetSet.CreatePenIndirect(const LogPen: TLogPen): HPEN;
begin
  Result:=HPEN(TFPGUIWinAPIPen.Create(LogPen));
end;

function TFpGuiWidgetSet.CreateRectRgn(X1, Y1, X2, Y2: Integer): HRGN;
var
  Reg: TFPGUIBasicRegion;
begin
  Reg:=TFPGUIBasicRegion.Create(Rect(X1,Y1,X2,Y2));
  Result:=HRGN(Reg);
end;

function TFpGuiWidgetSet.DeleteObject(GDIObject: HGDIOBJ): Boolean;
begin
  if IsValidGDIObject(GDIObject) then begin
    TObject(GDIObject).Free;
    Result:=true;
  end else begin
    Result:=false;
  end;
end;

function TFpGuiWidgetSet.DrawFocusRect(DC: HDC; const Rect: TRect): boolean;
var
  ADC: TFpGuiDeviceContext absolute DC;
  r: TfpgRect;
begin
  ADC.fpgCanvas.DrawFocusRect(ADC.PrepareRectOffsets(Rect));
  Result:=true;
end;

function TFpGuiWidgetSet.DrawText(DC: HDC; Str: PChar; Count: Integer;
  var ARect: TRect; Flags: Cardinal): Integer;
var
  ADC: TFpGuiDeviceContext absolute DC;
begin
  ADC.fpgCanvas.Font:=ADC.FFont.fpguiFont;
  Result:=inherited DrawText(DC, Str, Count, ARect, Flags);
  if (Flags and DT_CALCRECT)=0 then begin
    ADC.fpgCanvas.TextColor:=ADC.FTextColor;
    ADC.fpgCanvas.DrawText(ADC.PrepareRectOffsets(ARect),Str,[],0);
  end;
  Result:=ARect.Bottom-ARect.Top; //The height of the text.
end;

function TFpGuiWidgetSet.Ellipse(DC: HDC; x1, y1, x2, y2: Integer): Boolean;
var
  ADC: TFpGuiDeviceContext;// absolute DC;
  j: integer;
  OldColor: TfpgColor;
  r: TfpgRect;
  RR: TRect;
begin
  ADC:=TFPGUIDeviceContext(DC);
  r:=ADC.PrepareRectOffsets(Rect(x1,y1,x2,y2));
  TfpgRectToRect(r,RR);
  if not ADC.UseBrush then begin
    if ADC.UsePen then begin
      ADC.fpgCanvas.DrawArc(rr.Left,rr.Top,x2-x1,y2-y1,0,360);
    end;
  end else begin
    ADC.fpgCanvas.FillArc(rr.Left,rr.Top,x2-x1,y2-y1,0,360);
    if ADC.UsePen then begin
      ADC.fpgCanvas.DrawArc(rr.Left,rr.Top,x2-x1,y2-y1,0,360);
    end;
  end;
  Result:=true;
end;

function TFpGuiWidgetSet.EnableWindow(hWnd: HWND; bEnable: Boolean): Boolean;
var
  Widget: TFPGUIPrivateWidget absolute hWnd;
begin
  Result:=not Widget.Enabled;
  Widget.Enabled:=bEnable;
end;

function TFpGuiWidgetSet.EndPaint(Handle: hwnd; var PS: TPaintStruct): Integer;
var
  DC: TFpGuiDeviceContext;
begin
//  DC := TFpGuiDeviceContext(PS.hdc);
  ReleaseDC(Handle,PS.hdc);
  Result:=1; //Any non zero value.
end;

function TFpGuiWidgetSet.ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint;
  Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
var
  ADC: TFpGuiDeviceContext absolute DC;
  AStr: string;
  r: TfpgRect;
  rClip,OldClip: TfpgRect;
  RestoreClip: Boolean;
begin
  SetLength(AStr,Count);
  move(Str[0],AStr[1],Count);
  r:=ADC.PrepareRectOffsets(classes.Rect(X,Y,0,0));
  RestoreClip:=false;
  if Rect<>nil then begin
    rClip:=fpgRect(Rect^.Left,Rect^.Top,Rect^.Right-Rect^.Left,Rect^.Bottom-Rect^.Top);
    if (ETO_CLIPPED or ETO_OPAQUE) and Options <> 0 then begin
      OldClip:=ADC.fpgCanvas.GetClipRect;
      ADC.fpgCanvas.SetClipRect(rClip);
      RestoreClip:=true;
    end;
    if ETO_OPAQUE and Options = ETO_OPAQUE then begin
      ADC.ClearRectangle(rClip);
    end;
  end;

  ADC.fpgCanvas.TextColor:=ADC.FTextColor;
  ADC.fpgCanvas.Font:=ADC.FFont.fpguiFont;
  ADC.fpgCanvas.DrawText(r.Left, r.Top, AStr);

  if RestoreClip then begin
    ADC.fpgCanvas.SetClipRect(OldClip);
  end;
  Result:=true;
end;

function TFpGuiWidgetSet.FillRect(DC: HDC; const Rect: TRect; Brush: HBRUSH
  ): Boolean;
var
  ADC: TFpGuiDeviceContext;
  NewBrush: TFPGUIWinAPIBrush;
  TheRect: TfpgRect;
begin
  ADC:=TFPGUIDeviceContext(DC);
  NewBrush:=TFPGUIWinAPIBrush(Brush);
  TheRect:=ADC.PrepareRectOffsets(Rect);
  ADC.fpgCanvas.Color:=NewBrush.Color;
  ADC.fpgCanvas.FillRectangle(TheRect);
  Result:=true;
end;

function TFpGuiWidgetSet.GetCapture: HWND;
begin
  Result:=HWND(GlobalMouseCapturedPrivateWidget);
end;

function TFpGuiWidgetSet.GetClientRect(handle: HWND; var ARect: TRect
  ): Boolean;
var
  fpguiPrivate: TFPGUIPrivateWidget absolute handle;
begin
  fpguiPrivate.GetClientRect(ARect);
  Result:=true;
end;

function TFpGuiWidgetSet.GetClipBox(DC: hDC; lpRect: PRect): Longint;
var
  ADC: TFpGuiDeviceContext absolute DC;
  Clip: TfpgRect;
begin
  ADC.fpgCanvas.GetWinRect(Clip); { TODO : Should be a clip region, but use this by now }
  Result:=SimpleRegion;
  TfpgRectToRect(Clip,lpRect^);
end;

function TFpGuiWidgetSet.GetClipRGN(DC: hDC; RGN: hRGN): Longint;
var
  ADC: TFpGuiDeviceContext absolute DC;
  Clip: TfpgRect;
  Region: TFPGUIBasicRegion absolute RGN;
begin
  ADC.fpgCanvas.GetWinRect(Clip); { TODO : Should be a clip region, but use this by now }
  if Region<>nil Then FreeAndNil(Region);
  Region:=TFPGUIBasicRegion.Create(Rect(Clip.Left,Clip.Top,Clip.Right,Clip.Bottom));
  RGN:=HRGN(Region);
  if Region.RegionType=eRegionNULL then begin
    Result:=0;
  end else if Region.RegionType=eRegionNotCombinableOrError then begin
    Result:=-1;
  end else begin
    Result:=1;
  end;
end;

function TFpGuiWidgetSet.GetCursorPos(var lpPoint: TPoint): Boolean;
var
  O: TFPGUIPrivateWidget;
  alPoint: TPoint;
begin
  // TODO: Fix it cross platform style
  Result:=fpguicrosshelpers.GetCursorPos(lpPoint);
end;

function TFpGuiWidgetSet.GetDC(hWnd: HWND): HDC;
var
  PrivateWidget: TFPGUIPrivateWidget;
begin
  //Create a new DC
  PrivateWidget:=TFPGUIPrivateWidget(hWnd);
  if Assigned(PrivateWidget) then begin
    Result:=HDC(TFpGuiDeviceContext.Create(PrivateWidget));
  end else begin
    Result:=HDC(FPGUIGetDesktopDC());
  end;
end;

function TFpGuiWidgetSet.GetDeviceCaps(DC: HDC; Index: Integer): Integer;
var
  ADC: TFpGuiDeviceContext absolute DC;
begin
  if ADC.FPrivateWidget=nil then begin
    //Desktop device caps
    { TODO : Create real data for GetDeviceCaps }
    Case Index of
      LOGPIXELSX: Result:=96; //Hardcoded by now
      BITSPIXEL : Result:=32; //Hardcoded by now
      else begin
        {$ifdef VerboseFPGUIWinAPI}
          WriteLn(Self.ClassName,'.GetDeviceCaps Index ',Index,' Desktop');
        {$endif}
      end;
    end;
  end else begin
    //other
    {$ifdef VerboseFPGUIWinAPI}
      WriteLn(Self.ClassName,'.GetDeviceCaps Index ',Index,ADC.FPrivateWidget.LCLObject.Name);
    {$endif}
  end;
end;

function TFpGuiWidgetSet.GetFocus: HWND;
begin
  Result:=HWND(GlobalFocusedPrivateWidget);
end;

function TFpGuiWidgetSet.GetProp(Handle: hwnd; Str: PChar): Pointer;
var
  PrivateWidget: TFPGUIPrivateWidget absolute Handle;
begin
  if Assigned(PrivateWidget) and (Str='WinControl') then begin
    Result:=PrivateWidget.LCLObject;
  end else begin
    {$ifdef VerboseFPGUIWinAPI}
      WriteLn('Trace:Unknown Window property: ',Str);
    {$endif}
    Result:=nil;
  end;
end;

function TFpGuiWidgetSet.GetSysColor(nIndex: Integer): DWORD;
begin
  if (nIndex < 0) or (nIndex > MAX_SYS_COLORS) then
  begin
    {$ifdef VerboseFPGUIWinAPI}
      WriteLn('Trace:Unknown lcl system color: [TFpGuiWidgetSet.GetSysColor]');
    {$endif}
    Result:=clRed;
    exit;
  end;
  Result:=GetSysColorRGB(nIndex);
end;

function TFpGuiWidgetSet.GetSystemMetrics(nIndex: Integer): Integer;
begin
  case nIndex of
    //Current screen size
    SM_CXSCREEN: Result:=fpgApplication.ScreenWidth;
    SM_CYSCREEN: Result:=fpgApplication.ScreenHeight;
    //Desktop size
    SM_CXVIRTUALSCREEN: Result:=fpgApplication.ScreenWidth;
    SM_CYVIRTUALSCREEN: Result:=fpgApplication.ScreenHeight;
  end;
end;

function TFpGuiWidgetSet.GetTextColor(DC: HDC): TColorRef;
var
  ADC: TFpGuiDeviceContext absolute DC;
begin
  Result:=ADC.GetTextColor;
end;

function TFpGuiWidgetSet.GetTextExtentPoint(DC: HDC; Str: PChar;
  Count: Integer; var Size: TSize): Boolean;
var
  ADC: TFpGuiDeviceContext absolute DC;
begin
  Size.cx:=ADC.FFont.fpguiFont.TextWidth(Str);
  Size.cy:=ADC.FFont.fpguiFont.Height;
  Result:=true;
end;

function TFpGuiWidgetSet.GetTextMetrics(DC: HDC; var TM: TTextMetric): Boolean;
var
  ADC: TFpGuiDeviceContext absolute DC;
begin
  FillByte(TM,sizeof(TM),0);
  TM.tmAscent:=ADC.FFont.fpguiFont.Ascent;
  TM.tmDescent:=ADC.FFont.fpguiFont.Descent;
  //Defined usually in MSDN as the average of 'x' char.
  TM.tmAveCharWidth:=ADC.FFont.fpguiFont.TextWidth('x');
  TM.tmHeight:=ADC.FFont.fpguiFont.Height;
  Result:=true;
end;

function TFpGuiWidgetSet.GetWindowOrgEx(dc: hdc; P: PPoint): Integer;
var
  ADC: TFpGuiDeviceContext absolute DC;
begin
  P^:=ADC.FOrg;
  Result:=1;
end;

function TFpGuiWidgetSet.GetWindowRect(Handle: hwnd; var ARect: TRect
  ): Integer;
var
  PrivateWidget: TFPGUIPrivateWidget absolute Handle;
begin
  PrivateWidget.GetWindowRect(ARect);
  Result:=1;
end;

function TFpGuiWidgetSet.GetWindowSize(Handle: hwnd; var Width, Height: Integer
  ): boolean;
var
  PrivateWidget: TFPGUIPrivateBin;
begin
  PrivateWidget:=TFPGUIPrivateBin(Handle);
  Width:=PrivateWidget.Widget.Width;
  Height:=PrivateWidget.Widget.Height;
  Result:=true;
end;

function TFpGuiWidgetSet.InvalidateRect(aHandle: HWND; Rect: pRect;
  bErase: Boolean): Boolean;
var
  PrivateWidget: TFPGUIPrivateWidget absolute aHandle;
begin
//  PrivateWidget.Widget.Canvas.BeginDraw(true);
  PrivateWidget.Invalidate;
//  PrivateWidget.Widget.Canvas.EndDraw;
  Result:=true;
end;

function TFpGuiWidgetSet.MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint): Boolean;
var
  ADC: TFpGuiDeviceContext;// absolute DC;
  r: TPoint;
begin
  ADC:=TFPGUIDeviceContext(DC);
  r:=ADC.PreparePointOffsets(Point(x,y));
  if OldPoint<>nil then OldPoint^:=ADC.UnPreparePointOffsets(ADC.FDrawXY);
  ADC.FDrawXY:=r;
end;

function TFpGuiWidgetSet.LineTo(DC: HDC; X, Y: Integer): Boolean;
var
  ADC: TFpGuiDeviceContext;// absolute DC;
  p: TPoint;
begin
  ADC:=TFPGUIDeviceContext(DC);
  p:=ADC.PreparePointOffsets(Point(X,Y));
  if ADC.UsePen then begin
    ADC.fpgCanvas.DrawLine(ADC.FDrawXY.x,ADC.FDrawXY.y,p.x,p.y);
  end;
  ADC.FDrawXY:=p;
  Result:=true;
end;

{ Most of the functionality is implemented. As described in MSDN:
  http://msdn.microsoft.com/en-us/library/windows/desktop/ms645505%28v=vs.85%29.aspx }
function TFpGuiWidgetSet.MessageBox(hWnd: HWND; lpText, lpCaption: PChar;
  uType: Cardinal): integer;
var
  Str: AnsiString;
  TitleStr: AnsiString;
  Buttons : TfpgMsgDlgButtons;
  BtnType: Cardinal;
  DlgType: Cardinal;
begin
  BtnType := (uType and $0000000F); { mask the button type }

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
    TfpgMessageDialog.Information(TitleStr, Str, Buttons);
end;

function TFpGuiWidgetSet.Polygon(DC: HDC; Points: PPoint; NumPts: Integer;
  Winding: boolean): boolean;
var
  ADC: TFpGuiDeviceContext;// absolute DC;
  lPoints: array of TPoint;
  j: integer;
begin
  ADC:=TFPGUIDeviceContext(DC);
  SetLength(lPoints,NumPts+1);
  for j := 0 to Pred(NumPts) do
  begin
    lPoints[j]:=ADC.PreparePointOffsets(Points[j]);
  end;
  lPoints[NumPts]:=lPoints[0];
  if not ADC.UseBrush then begin
    if ADC.UsePen then begin
      ADC.fpgCanvas.DrawPolyLine(lPoints);
    end;
  end else begin
    ADC.fpgCanvas.DrawPolygon(@lPoints[0],NumPts,Winding);
    if ADC.UsePen then begin
      ADC.fpgCanvas.DrawPolyLine(lPoints);
    end;
  end;
  Result:=true;
end;

function TFpGuiWidgetSet.Polyline(DC: HDC; Points: PPoint; NumPts: Integer
  ): boolean;
var
  ADC: TFpGuiDeviceContext;// absolute DC;
  lPoints: array of TPoint;
  j: integer;
begin
  ADC:=TFPGUIDeviceContext(DC);
  SetLength(lPoints,NumPts);
  for j := 0 to Pred(NumPts) do
  begin
    lPoints[j]:=ADC.PreparePointOffsets(Points[j]);
  end;
  if ADC.UsePen then begin
    ADC.fpgCanvas.DrawPolyLine(lPoints);
  end;
  Result:=true;
end;

function TFpGuiWidgetSet.SendMessage(HandleWnd: HWND; Msg: Cardinal;
  WParam: WParam; LParam: LParam): LResult;
var
  lMessageParams: TfpgMessageParams;
  lNewMsg: integer;
begin
  FillByte(lMessageParams,Sizeof(lMessageParams),0);
  case msg of
    LM_USER..LM_USER+$7FFF: begin
      lNewMsg:=FPGM_USER;
      lMessageParams.user.Param1:=(msg-LM_USER);
      lMessageParams.user.Param2:=wParam;
      lMessageParams.user.Param3:=lParam;
    end;
  otherwise
    // This needs a lot of conversion between Windows messages and fpGUI messages.
    Result:=0;
    exit;
  end;
  fpgSendMessage(nil,TFPGUIPrivateWidget(HandleWnd),lNewMsg,lMessageParams);
  Result:=0;
end;


function TFpGuiWidgetSet.PostMessage(Handle: HWND; Msg: Cardinal;
  wParam: WParam; lParam: LParam): Boolean;
var
  lMessageParams: TfpgMessageParams;
  lNewMsg: integer;
begin
  FillByte(lMessageParams,Sizeof(lMessageParams),0);
  case msg of
    LM_USER..LM_USER+$7FFF: begin
      lNewMsg:=FPGM_USER;
      lMessageParams.user.Param1:=(msg-LM_USER);
      lMessageParams.user.Param2:=wParam;
      lMessageParams.user.Param3:=lParam;
    end;
  otherwise
    // This needs a lot of conversion between Windows messages and fpGUI messages.
    Result:=false;
    exit;
  end;
  fpgPostMessage(nil,TFPGUIPrivateWidget(Handle),lNewMsg,lMessageParams);
  fpgDeliverMessages; // Force message loop to deliver messages as
                      // the post message seems to not wakeup the queue.
  Result:=true;
end;

function TFpGuiWidgetSet.Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean;
var
  ADC: TFpGuiDeviceContext;// absolute DC;
  r: TfpgRect;
begin
  ADC:=TFPGUIDeviceContext(DC);
  r:=ADC.PrepareRectOffsets(Rect(X1,Y1,X2,Y2));

  if not ADC.UseBrush then begin
    if ADC.UsePen then begin
      ADC.fpgCanvas.DrawRectangle(r);
    end;
  end else begin
    ADC.fpgCanvas.FillRectangle(r);
    if ADC.UsePen then begin
      ADC.fpgCanvas.DrawRectangle(r);
    end;
  end;
  Result:=true;
end;

function TFpGuiWidgetSet.ReleaseCapture: Boolean;
begin
  if Assigned(GlobalMouseCapturedPrivateWidget) then begin
    GlobalMouseCapturedPrivateWidget.Widget.ReleaseMouse;
    GlobalMouseCapturedPrivateWidget:=nil;
    Result:=true;
  end else begin
    Result:=false;
  end;
end;

function TFpGuiWidgetSet.ReleaseDC(hWnd: HWND; DC: HDC): Integer;
var
  MyDC: TFpGuiDeviceContext;
  MyPrivateWidget: TFPGUIPrivateWidget;
begin
  MyDC:=TFPGUIDeviceContext(DC);
  if MyDC<>FPGUIGetDesktopDC then begin //DesktopDC can not be freed
    MyPrivateWidget:=TFPGUIPrivateWidget(hWnd);
    FreeAndNil(MyDC);
  end;
  Result:=1;
end;

function TFpGuiWidgetSet.RestoreDC(DC: HDC; SavedDC: Integer): Boolean;
var
  ADC: TFPGUIDeviceContext absolute DC;
begin
  Result:=ADC.RestoreDC(SavedDC);
end;

function TFpGuiWidgetSet.SaveDC(DC: HDC): Integer;
var
  ADC: TFPGUIDeviceContext absolute DC;
begin
  Result:=ADC.SaveDC;
end;

function TFpGuiWidgetSet.SelectClipRGN(DC: hDC; RGN: HRGN): Longint;
var
  ADC: TFPGUIDeviceContext absolute DC;
  Reg: TFPGUIBasicRegion absolute RGN;
begin
  if Reg.RegionType=eRegionSimple then begin
    ADC.SelectObject(HGDIObj(Reg));
    Result:=SimpleRegion;
  end else begin
    Result:=NullRegion;
  end;
end;

function TFpGuiWidgetSet.SelectObject(DC: HDC; GDIObj: HGDIOBJ): HGDIOBJ;
var
  MyDC: TFpGuiDeviceContext absolute DC;
begin
  Result:=MyDC.SelectObject(GDIObj);
end;

function TFpGuiWidgetSet.SetCapture(AHandle: HWND): HWND;
var
  PrivateWidget: TFPGUIPrivateWidget absolute AHandle;
begin
  Result:=HWND(GlobalMouseCapturedPrivateWidget);
  if Assigned(GlobalMouseCapturedPrivateWidget) then begin
    ReleaseCapture;
  end;
  if Assigned(PrivateWidget) then begin
    PrivateWidget.Widget.CaptureMouse;
  end;
  GlobalMouseCapturedPrivateWidget:=PrivateWidget;
end;

function TFpGuiWidgetSet.SetFocus(hWnd: HWND): HWND;
var
  PrivateWidget: TFPGUIPrivateWidget absolute hWnd;
begin
  Result:=LCLType.HWND(PrivateWidget.Widget.ActiveWidget);
  PrivateWidget.SetFocus;
end;

function TFpGuiWidgetSet.SetParent(hWndChild: HWND; hWndParent: HWND): HWND;
var
  PrivateWidgetParent: TFPGUIPrivateWidget absolute hWndParent;
  PrivateWidgetChild: TFPGUIPrivateWidget absolute hWndChild;
begin
  Result:=HWND(PrivateWidgetChild.Widget.Parent);
  PrivateWidgetChild.Widget.Parent:=PrivateWidgetParent.Widget;
end;

function TFpGuiWidgetSet.SetScrollInfo(Handle: HWND; SBStyle: Integer;
  ScrollInfo: TScrollInfo; bRedraw: Boolean): Integer;
var
  PrivateWidget: TFPGUIPrivateWidget absolute Handle;
  ScrollWin: TFPGUIPrivateScrollingWinControl;
begin
  Result:=0;
  if PrivateWidget is TFPGUIPrivateScrollingWinControl then begin
    ScrollWin:=TFPGUIPrivateScrollingWinControl(PrivateWidget);
    if ScrollInfo.fMask and SIF_RANGE = SIF_RANGE then begin
      if SBStyle=1 then begin
        ScrollWin.ScrollFrame.ContentFrame.Height:=ScrollInfo.nMax-ScrollInfo.nMin;
        Result:=-ScrollWin.ScrollFrame.ContentFrame.Left;
      end;
      if SBStyle=0 then begin
        ScrollWin.ScrollFrame.ContentFrame.Width:=ScrollInfo.nMax-ScrollInfo.nMin;
        Result:=-ScrollWin.ScrollFrame.ContentFrame.Top;
      end;
    end;
  end;
end;

function TFpGuiWidgetSet.SetTextColor(DC: HDC; Color: TColorRef): TColorRef;
var
  ADC: TFpGuiDeviceContext;// absolute DC;
begin
  ADC:=Tfpguidevicecontext(DC);
  Result:=ADC.SetTextColor(Color);
end;

function TFpGuiWidgetSet.SetWindowOrgEx(DC: HDC; NewX, NewY: Integer;
  OldPoint: PPoint): Boolean;
var
  ADC: TFpGuiDeviceContext absolute DC;
begin
  OldPoint^:=ADC.FOrg;
  ADC.SetOrigin(NewX,NewY);
  Result:=true;
end;

function TFpGuiWidgetSet.ShowWindow(hWnd: HWND; nCmdShow: Integer): Boolean;
var
  Widget: TFPGUIPrivateWidget absolute hWnd;
begin
  Result:=Widget.Visible;
  Widget.Visible:=true;{ TODO -oJose Mejuto : Process showwindow mode }
end;

function TFpGuiWidgetSet.StretchBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; ROp: Cardinal
  ): Boolean;
var
  SDC: TFPGUIDeviceContext absolute SrcDC;
  TDC: TFPGUIDeviceContext absolute DestDC;
begin
  Result:=false;
end;

function TFpGuiWidgetSet.StretchMaskBlt(DestDC: HDC; X, Y, Width,
  Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer;
  Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean;
var
  SDC: TFPGUIDeviceContext;
  TDC: TFPGUIDeviceContext;
  pTarget: TPoint;
begin
  // Incomplete, partial source, masks and ROP not implemented.
  SDC:=TFPGUIDeviceContext(SrcDC);
  TDC:=TFPGUIDeviceContext(DestDC);
  pTarget:=Point(x,y);
  pTarget:=TDC.PreparePointOffsets(pTarget);
  TDC.ClearDC;
  if Assigned(SDC.FBitmap) then begin
    TDC.fpgCanvas.StretchDraw(pTarget.x,pTarget.y,Width,Height,SDC.FBitmap.Image);
  end;
  Result:=true;
end;

function TFpGuiWidgetSet.SystemParametersInfo(uiAction: DWord; uiParam: DWord;
  pvParam: Pointer; fWinIni: DWord): LongBool;
begin
  case uiAction of
    SPI_GETWORKAREA: begin
      PRect(pvPAram)^.Left:=0;
      PRect(pvPAram)^.Top:=0;
      PRect(pvPAram)^.Right:=fpgApplication.ScreenWidth;
      PRect(pvPAram)^.Bottom:=fpgApplication.ScreenHeight;
      Result := True;
    end;
  otherwise
    Result:=inherited SystemParametersInfo(uiAction, uiParam, pvParam, fWinIni);
  end;
end;

function TFpGuiWidgetSet.WindowFromPoint(Point: TPoint): HWND;
begin
  { TODO : Temporal hack while not real WindowFromPoint implementation }
  Result:=HWND(GlobalMouseCursorPosPrivateWidget);
end;

function TFpGuiWidgetSet.CreateStandardCursor(ACursor: SmallInt): hCursor;
var
  CursorValue: Integer;
begin
  Result := 0;
  if ACursor < crLow then Exit;
  if ACursor > crHigh then Exit;

  case TCursor(ACursor) of
    crDefault:  CursorValue := integer(mcDefault);
    crArrow:    CursorValue := integer(mcArrow);
    crCross:    CursorValue := integer(mcCross);
    crIBeam:    CursorValue := integer(mcIBeam);
    crSizeNESW: CursorValue := integer(mcSizeNESW);
    crSizeNS:   CursorValue := integer(mcSizeNS);
    crSizeNWSE: CursorValue := integer(mcSizeNWSE);
    crSizeWE:   CursorValue := integer(mcSizeEW);
    crSizeNW:   CursorValue := integer(mcSizeNWSE);
    crSizeN:    CursorValue := integer(mcSizeNS);
    crSizeNE:   CursorValue := integer(mcSizeNESW);
    crSizeW:    CursorValue := integer(mcSizeEW);
    crSizeE:    CursorValue := integer(mcSizeEW);
    crSizeSW:   CursorValue := integer(mcSizeSWNE);
    crSizeS:    CursorValue := integer(mcSizeNS);
    crSizeSE:   CursorValue := integer(mcSizeSENW);
    crUpArrow:  CursorValue := integer(mcArrow);
    crHourGlass:CursorValue := integer(mcHourGlass);
    crHSplit:   CursorValue := integer(mcSizeEW);
    crVSplit:   CursorValue := integer(mcSizeNS);
//    crAppStart: CursorValue := integer(mcAppStart);
//    crHelp:     CursorValue := integer(mcHelp);
    crHandPoint:CursorValue := integer(mcHand);
    crSizeAll:  CursorValue := integer(mcSizeNESW);
  otherwise
    CursorValue:=-1;
  end;
  if CursorValue<>-1 then begin
    Result := hCursor(CursorValue);
  end;
end;
//##apiwiz##eps##   // Do not remove, no wizard declaration after this line

