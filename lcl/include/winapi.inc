(******************************************************************************
  All Winapi related stuff goes here.
  This file is used by LCLLinux
  If a procedure is platform dependent then it should call:
    InterfaceObjec.MyDependentProc

  If a procedure insn't platform dependent, it is no part of InterfaseBase has
  to be implementerd here

  !! Keep this alphabetical !!
 ******************************************************************************)

{******************************************************************************
  Platform specific stuff

 ******************************************************************************}
//##apiwiz##sps##   // Do not remove

function BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD): Boolean; 
begin
  Result := InterfaceObject.BitBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, Rop);
end;

function CallNextHookEx(hhk : HHOOK; ncode : Integer; wParam, lParam : Longint) : Integer; 
begin
  Result := InterfaceObject.CallNExtHookEx(hhk,ncode,wparam,lParam);
end;

Function ClienttoScreen(Handle : HWND; var P : TPoint) : Boolean;
Begin
  Result := InterfaceObject.ClientToScreen(Handle, P);
end;

function CreateBitmap(Width, Height: Integer; Planes, BitCount: Longint; BitmapBits: Pointer): HBITMAP;
begin
  Result := InterfaceObject.CreateBitmap(Width, Height, Planes, BitCount, BitmapBits);
end;

function CreateBrushIndirect(const LogBrush: TLogBrush): HBRUSH;
begin
  Result := InterfaceObject.CreateBrushIndirect(LogBrush);
end;

Function CreateCaret(Handle : HWND; Bitmap : hBitmap; width, Height : Integer) : Boolean;
Begin
  Result := InterfaceObject.CreateCaret(Handle, Bitmap, width, Height);
end;

function CreateCompatibleBitmap(DC: HDC; Width, Height: Integer): HBITMAP;
begin
  Result := InterfaceObject.CreateCompatibleBitmap(DC, Width, Height);
end;

function CreateCompatibleDC(DC: HDC): HDC;
begin
  Result := InterfaceObject.CreateCompatibleDC(DC);
end;

function CreateFontIndirect(const LogFont: TLogFont): HFONT;
begin
  Result := InterfaceObject.CreateFontIndirect(LogFont);
end;

function CreatePenIndirect(const LogPen: TLogPen): HPEN;
begin
  Result := InterfaceObject.CreatePenIndirect(LogPen);
end;

function CreatePixmapIndirect(const Data: Pointer; const TransColor: Longint): HBITMAP;
begin
  Result := InterfaceObject.CreatePixmapIndirect(Data, TransColor);
end;

function CreateRectRgn(X1,Y1,X2,Y2 : Integer): HRGN;
Begin
  Result := InterfaceObject.CreateRectRgn(X1,Y1,X2,Y2);
end;

function DeleteDC(hDC: HDC): Boolean;
begin
  Result := InterfaceObject.DeleteDC(hDC);
end;

function DeleteObject(GDIObject: HGDIOBJ): Boolean;
begin
  Result := InterfaceObject.DeleteObject(GDIObject);
end;

function DestroyCaret: Boolean;
Begin
  Result := InterfaceObject.DestroyCaret;
end;

Function DrawFrameControl(DC: HDC; var Rect : TRect; uType, uState : Cardinal) : Boolean;
Begin
  Result := InterfaceObject.DrawFrameControl(DC, Rect, uType, uState);
end;

function DrawEdge(DC: HDC; var Rect: TRect; edge: Cardinal; grfFlags: Cardinal): Boolean;
Begin
  Result := InterfaceObject.DrawEdge(DC, Rect, edge, grfFlags);
end;

Function EmptyClipBoard : Boolean;
begin
  Result := InterfaceObject.EmptyClipBoard;
end;

function EnableMenuItem(hMenu: HMENU; uIDEnableItem: Integer; bEnable: Boolean): Boolean;
begin
  Result := InterfaceObject.EnableMenuItem(hMenu, uIDEnableItem, bEnable);
end;

function EnableScrollBar(Wnd: HWND; wSBflags, wArrows: Cardinal): Boolean;
begin
  Result := InterfaceObject.EnableScrollBar(Wnd, wSBflags, wArrows);
end;

function EnableWindow(hWnd: HWND; bEnable: Boolean): Boolean;
begin
  Result := InterfaceObject.EnableWindow(hWnd, bEnable);
end;

function ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
begin
  Result := InterfaceObject.ExtTextOut(DC, X, Y, Options, Rect, Str, Count, Dx);
end;

function FillRect(DC: HDC; const Rect: TRect; Brush: HBRUSH): Boolean;
begin
  Result := InterfaceObject.FillRect(DC, Rect, Brush);
end;

Function GetCapture: HWND;
Begin
  Result := InterfaceObject.GetCapture;
end;

function GetCaretPos(var lpPoint: TPoint): Boolean;
begin
  Result := InterfaceObject.GetCaretPos(lpPoint);
end;

Function GetCursorPos(var lpPoint:TPoint): Boolean;
Begin
  Result := InterfaceObject.GetCaretPos(lpPoint);
end;

function GetCharABCWidths(DC: HDC; p2, p3: UINT; const ABCStructs): Boolean; //pbd
begin
  Result := InterfaceObject.GetCharABCWidths(DC, p2, p3, ABCStructs);
end;

function GetDC(hWnd: HWND): HDC;
begin
  Result := InterfaceObject.GetDC(hWnd);
end;

function GetFocus: HWND;
begin
  Result := InterfaceObject.GetFocus;
end;

function GetKeyState(nVirtKey: Integer): Smallint;
begin
  Result := InterfaceObject.GetKeyState(nVirtKey);
end;

function GetObject(GDIObject: HGDIOBJ; BufSize: Integer; Buf: Pointer): Integer;
begin
  Result := InterfaceObject.GetObject(GDIObject, BufSize, Buf);
end;

Function GetParent(Handle : HWND): HWND;
begin
  Result := InterfaceObject.GetParent(Handle);
end;

Function GetProp(Handle : hwnd; Str : PChar): Pointer;
Begin
  Result := InterfaceObject.GetProp(Handle,Str);
end;

function GetScrollInfo(Handle: HWND; BarFlag: Integer; var ScrollInfo: TScrollInfo): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
begin
  Result := InterfaceObject.GetScrollInfo(Handle, BarFlag, ScrollInfo);
end;

function GetStockObject(Value : Integer): Longint;
begin
  Result := InterfaceObject.GetStockObject(Value);
end;

function GetSysColor(nIndex: Integer): DWORD;
begin
  Result := InterfaceObject.GetSysColor(nIndex);
end;

function GetSystemMetrics(nIndex: Integer): Integer;
begin
  Result := InterfaceObject.GetSystemMetrics(nIndex);
end;

function GetTextExtentPoint(DC: HDC; Str: PChar; Count: Integer; var Size: TSize): Boolean;
begin
  Result := InterfaceObject.GetTextExtentPoint(DC, Str, Count, Size);
end;

function GetTextMetrics(DC: HDC; var TM: TTextMetric): Boolean;
begin
  Result := InterfaceObject.GetTextMetrics(DC, TM);
end;

function GetWindowLong(Handle : hwnd; int : Integer): Longint;
begin
  Result := InterfaceObject.GetWindowLong(Handle, int);
end;

Function GetWindowRect(Handle : hwnd; Rect : PRect): Integer;
begin
  Result := InterfaceObject.GetWindowRect(Handle, Rect);
end;

Function GetWindowOrgEx(dc : hdc; var P : TPoint): Integer;
begin
 Result := InterfaceObject.GetWindowOrgEx(dc,P);
end;

function HideCaret(hWnd: HWND): Boolean;
begin
  Result := InterfaceObject.HideCaret(hWnd);
end;

Function InvalidateRect(aHandle : HWND; Rect : pRect; bErase : Boolean) : Boolean;
begin
  Result := InterfaceObject.InvalidateRect(aHandle, Rect, bErase);
end;

function LineTo(DC: HDC; X, Y: Integer): Boolean;
begin
  Result := InterfaceObject.LineTo(DC, X, Y);
end;

function MaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean; 
begin
  Result := InterfaceObject.MaskBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, Mask, XMask, YMask, Rop);
end;

function MessageBox(hWnd: HWND; lpText, lpCaption: PChar; uType : Cardinal): integer;
begin
  Result:= InterfaceObject.MessageBox(hWnd, lpText, lpCaption, uType);
end;

function MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint): Boolean;
begin
  Result := InterfaceObject.MoveToEx(DC, X, Y, OldPoint);
end;

function PeekMessage(var lpMsg : TMsg; Handle : HWND; wMsgFilterMin, wMsgFilterMax,wRemoveMsg : UINT): Boolean;
begin
  Result := InterfaceObject.PeekMessage(lpMsg,Handle,wMsgFilterMin,wMsgFilterMax,wRemoveMsg);
end;

function PostMessage(hWnd: HWND; Msg: Cardinal; wParam: LongInt; lParam: LongInt): Boolean;
begin
  Result := InterfaceObject.PostMessage(hWnd, Msg, wParam, lParam);
end;

function RealizePalette(DC: HDC): Cardinal;
begin
  Result := InterfaceObject.RealizePalette(DC);
end;

function Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean;
begin
  Result := InterfaceObject.Rectangle(DC, X1, Y1, X2, Y2);
end;

function ReleaseCapture:  Boolean;
begin
  Result := InterfaceObject.ReleaseCapture;
end;

function ReleaseDC(hWnd: HWND; DC: HDC): Integer;
begin
  Result := InterfaceObject.ReleaseDC(hWnd, DC);
end;

function RestoreDC(DC: HDC; SavedDC: Integer): Boolean; 
begin
  Result := InterfaceObject.RestoreDC(DC, SavedDC)  
end;

function SaveDC(DC: HDC) : Integer; 
begin
  Result := InterfaceObject.SaveDC(DC)  
end;

function ScrollWindowEx(hWnd: HWND; dx, dy: Integer; prcScroll, prcClip: PRect; hrgnUpdate: HRGN; prcUpdate: PRect; flags: UINT): Boolean;
begin
  Result := InterfaceObject.ScrollWindowEx(hWnd, dx, dy, prcScroll, prcClip, hrgnUpdate, prcUpdate, flags);
end;

function SendMessage(hWnd: HWND; Msg: Cardinal; wParam: LongInt; lParam: LongInt): Integer;
begin
  Result := InterfaceObject.SendMessage(hWnd, Msg, wParam, lParam);
end;

function SetBkColor(DC: HDC; Color: TColorRef): TColorRef;  //pbd
begin
  Result := InterfaceObject.SetBkColor(DC, Color);
end;

Function SetBkMode(DC: HDC; bkMode : Integer) : Integer;
begin
  Result := InterfaceObject.SetBkMode(DC, bkMode);
end;

Function SetCapture(value : Longint): Longint;
Begin
  Result := InterfaceObject.SetCapture(Value);
end;

function SetCaretPos(X, Y: Integer): Boolean;
begin
  Result := InterfaceObject.SetCaretPos(X, Y);
end;

Function SetProp(Handle: hwnd; Str : PChar; Data : Pointer) : Boolean;
Begin
  Result := InterfaceObject.SetProp(Handle,Str,Data);
end;

function SelectObject(DC: HDC; GDIObj: HGDIOBJ): HGDIOBJ;
begin
  Result := InterfaceObject.SelectObject(DC, GDIObj);
end;

function SelectPalette(DC: HDC; Palette: HPALETTE; ForceBackground: Boolean): HPALETTE;
begin
  Result := InterfaceObject.SelectPalette(DC, Palette, ForceBackground);
end;

function SetFocus(hWnd: HWND): HWND;
begin
  Result := InterfaceObject.SetFocus(hWnd);
end;

function SetScrollInfo(Handle : HWND; SBStyle : Integer; ScrollInfo: TScrollInfo; Redraw : Boolean): Integer;
begin
  Result := InterfaceObject.SetSCrollInfo(Handle, SBStyle, ScrollInfo, Redraw)
end;

function SetSysColors(cElements: Integer; const lpaElements; const lpaRgbValues): Boolean; 
begin
  Result := InterfaceObject.SetSysColors(cElements, lpaElements, lpaRgbValues);
end;

Function SetTextCharacterExtra(_hdc : hdc; nCharExtra : Integer):Integer;
begin
  Result := InterfaceObject.SetTextCharacterExtra(_hdc, nCharExtra);
end;

function SetTextColor(DC: HDC; Color: TColorRef): TColorRef;
begin
  Result := InterfaceObject.SetTextColor(DC, Color);
end;

function SetWindowLong(Handle: HWND; Idx: Integer; NewLong : Longint): LongInt;
begin
  Result := InterfaceObject.SetWindowLong(handle, Idx, NewLong);
end;

Function SetWindowOrgEx(dc : hdc; NewX, NewY : Integer; Var lpPoint : TPoint) : Boolean;
begin
  Result := InterfaceObject.SetWindowOrgEx(dc,NewX,NewY,lpPoint);
end;

function ShowCaret(hWnd: HWND): Boolean;
begin
  Result := InterfaceObject.ShowCaret(hWnd)
end;

function ShowScrollBar(Handle: HWND; wBar: Integer; bShow: Boolean): Boolean;
begin
  Result := InterfaceObject.ShowScrollBar(Handle, wBar, bShow);
end;

function StretchBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Rop: Cardinal): Boolean;
begin
  Result := InterfaceObject.StretchBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, SrcWidth, SrcHeight, Rop)
end;

function StretchMaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean; 
begin
  Result := InterfaceObject.StretchMaskBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, SrcWidth, SrcHeight, Mask, XMask, YMask, Rop);
end;

function TextOut(DC: HDC; X,Y : Integer; Str : Pchar; Count: Integer) : Boolean;
begin
  Result := InterfaceObject.TextOut(DC, X, Y, Str, Count);
end;

function WindowFromPoint(Point : TPoint) : HWND;
begin
  Result := InterfaceObject.WindowFromPoint(Point);
end;

//##apiwiz##eps##   // Do not remove

{******************************************************************************
  Platform independent stuff
 ******************************************************************************}

//##apiwiz##spi##   // Do not remove
{------------------------------------------------------------------------------
  Function: AdjustWindowRectEx
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function AdjustWindowRectEx( Var Rect: TRect; Style1: Word; MenuExist : Boolean; Style2 : Word) : Boolean;
begin
  Result := true;
  try
    if MenuExist 
    then Rect.Top := Rect.Top + 25;
  except
    Result := False;
  end;
end;

{------------------------------------------------------------------------------
  Function: BeginPaint
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function BeginPaint(Handle : hwnd; Var PS : TPaintStruct) : hdc;
begin
  Assert(False, Format('Trace:> [BeginPaint] HWND: 0x%x', [Handle]));
  
  //TODO: Finish this.  BEGINPAINT
  // Move to platform dependent ??
  Result := Getdc(Handle);

  Assert(False, Format('Trace:< [BeginPaint] HWND: 0x%x --> 0x%x', [Handle, Result]));
end;

{------------------------------------------------------------------------------
  Function: CharLowerBuff
  Params: pStr:
          Len:
  Returns:

 ------------------------------------------------------------------------------}
function CharLowerBuff(pStr : PChar; Len : Integer): Integer;
begin
  // your code here
  //TODO:WINAPI call CHARLOWERBUFF
  Writeln('TODO: WINAPI call CHARLOWERBUFF');
end;


{------------------------------------------------------------------------------
  Function: CopyRect  pbd
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function CopyRect(var lprcDst: TRect; const lprcSrc: TRect): Boolean;
begin
  Move(lprcSrc, lprcDst, SizeOf(TRect));
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: CreateFont
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function CreateFont(Height, Width, Escapement, Orientation, Weight: Integer;
  Italic, Underline, StrikeOut, CharSet, OutputPrecision, ClipPrecision,
  Quality, PitchAndFamily: Cardinal; FaceName: PChar): HFONT;
var
  LogFont: TLogFont;
begin
  with LogFont do
  begin
    lfHeight := Height;
    lfWidth := Width;
    lfEscapement := Escapement;
    lfOrientation := Orientation;
    lfWeight := Weight;
    lfItalic := Italic;
    lfUnderline := Underline;
    lfStrikeOut := StrikeOut;
    lfCharSet := CharSet;
    lfOutPrecision := OutputPrecision;
    lfClipPrecision := ClipPrecision;
    lfQuality := Quality;
    lfPitchAndFamily := PitchAndFamily;
    StrLCopy(@lfFaceName, FaceName, SizeOf(lfFaceName));
  end;

  Result := CreateFontIndirect(LogFont);
end;

{------------------------------------------------------------------------------
  Function: CreatePen
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function CreatePen(Style, Width: Integer; Color: TColorRef): HPEN;
var
  LogPen: TLogPen;
begin
  with LogPen do
  begin
    lopnStyle := Style;
    lopnWidth.X := Width;
    lopnColor := Color;
  end;

  Result := CreatePenIndirect(LogPen);
end;


{------------------------------------------------------------------------------
  Function: ExcludeClipRect
  Params:
-------------------------------------------------------------------------------}
Function ExcludeClipRect(dc: hdc; LEft,Top, Right, Bottom : Integer) : Integer;
begin
//TODO: Finish ExcludeClipRect
Result := SimpleRegion;
end;

{------------------------------------------------------------------------------
  Function: EndPaint
  Params:
-------------------------------------------------------------------------------}
Function EndPaint(Handle : hwnd; var PS : TPaintStruct): Integer;
Begin
  Assert(False, Format('Trace:> [EndPaint] HWND: 0x%x', [Handle]));

  //TODO: Finish EndPaint in winapi.inc
  Result := 1;
  Assert(False, Format('Trace:< [EndPaint] HWND: 0x%x --> 0x%x', [Handle, Result]));
end;


{------------------------------------------------------------------------------
  Function: EqualRect
  Params:   Rect to Compare
  Returns:
  Quicker with a comparemem? whats the fpc equiv?
 ------------------------------------------------------------------------------}
function EqualRect(const lprc1, lprc2: TRect): Boolean;
begin
  //Result := CompareMem(@lprc1, @lprc2, SizeOf(TRect);
  Result := (lprc1.Left = lprc2.Left) And (lprc1.Right = lprc2.Right) And
            (lprc1.Top = lprc2.Top) And (lprc1.Bottom = lprc2.Bottom);
end;


{------------------------------------------------------------------------------
  Function: GetScrollPos
  Params:   Handle, nBar
  Returns:

 ------------------------------------------------------------------------------}
function GetScrollPos(Handle: HWND; nBar: Integer): Integer;
var
  Info: TScrollInfo;
begin
  GetScrollInfo(Handle, nBar, Info);
  Result := Info.nPos;
end;

{------------------------------------------------------------------------------
  Function: GetScrollRange
  Params:   Handle, nBar, lpMinPos, lpMaxPos
  Returns:

 ------------------------------------------------------------------------------}
function GetScrollRange(Handle: HWND; nBar: Integer; var lpMinPos, lpMaxPos: Integer): Boolean;
var
  Info: TScrollInfo;
begin
  Result := GetScrollInfo(Handle, nBar, Info);
  lpMinPos := Info.nMin;
  lpMaxPos := Info.nMax;
end;

{------------------------------------------------------------------------------
  Function: InflateRect
  Params:   Rect: points to structure that increases or decreases in size.
            dx  : amount to increase or decrease the rectangle width.
            dy  : amount to increase or decrease the rectangle height.
  Returns:  True if succesful

  Increases or decreases the width and height of the specified rectangle.
 ------------------------------------------------------------------------------}
function InflateRect(var Rect: TRect; dx, dy: Integer): Boolean;
begin
  with Rect do
  begin
    Dec(Left, dx);
    Inc(Right, dx);
    Dec(Top, dy);
    Inc(Bottom, dy);
  end;

  Result := True;
end;

{------------------------------------------------------------------------------
  Function: IntersectClipRect
  Params:
  Returns:
 ------------------------------------------------------------------------------}
function IntersectClipRect(dc: hdc; Leftrect, Toprect, RightREct,Bottomrect : Integer): Integer;
Begin
//TODO: FInish me IntersectClipRect in winapi.inc
result := Error;
end;

{------------------------------------------------------------------------------
  Function: IntersectRect
  Params:
  Returns:
 ------------------------------------------------------------------------------}
function IntersectRect(var lprcDst: TRect; const lprcSrc1, lprcSrc2: TRect): Boolean;
var
  URect: TRect;
  h1,h2,v1,v2: Integer;
begin
  Result := False;

  // Sanity Checks
  if IsRectEmpty(lprcSrc1) or IsRectEmpty(lprcSrc1) then 
  begin
    SetRectEmpty(lprcDst);
    Exit;
  end;

   // next test for no intersection at all.
   with lprcSrc2 do
     if ((Left > lprcSrc1.Left  ) and (Left >= lprcSrc1.Right  )) 
     or ((Right <= lprcSrc1.Left) and (Right < lprcSrc1.Right  )) 
     or ((Top > lprcSrc1.Top    ) and (Top >= lprcSrc1.Bottom  )) 
     or ((Bottom <= lprcSrc1.Top) and (Bottom < lprcSrc1.Bottom)) 
     then begin
       SetRectEmpty(lprcDst);
       Exit;
     end;

   // we got this far, ok intersecting rects
   UnionRect(URect, lprcSrc1, lprcSrc2);
   
   with URect do
   begin
     if lprcSrc1.Top = lprcSrc2.Top 
     then h1 := Top 
     else if lprcSrc1.Top = Top 
       then h1 := lprcSrc2.Top 
       else h1 := lprcSrc1.Top;

     if lprcSrc2.Bottom = lprcSrc1.Bottom 
     then h2 := Bottom 
     else if lprcSrc2.Bottom = Bottom 
       then h2 := lprcSrc1.Bottom 
       else h2 := lprcSrc2.Bottom;

     if lprcSrc1.Left = lprcSrc2.Left 
     then v1 := Left 
     else if lprcSrc1.Left = Left 
       then v1 := lprcSrc2.Left 
       else v1 := lprcSrc1.Left;

     if lprcSrc2.Right = lprcSrc1.Right 
     then v2 := Right 
     else if lprcSrc2.Right = Right 
     then v2 := lprcSrc1.Right 
     else v2 := lprcSrc2.Right;
   end;

   with lprcDst do
   begin
     Left := Min(v1, v2);
     Right := Max(v1, v2);
     Top := Min(h1, h2);
     Bottom := Max(h1, h2);
   end;

   Result := True;
end;

{------------------------------------------------------------------------------
  Function: IsCharAlphaNumeric
  Params: c:
  Returns:

 ------------------------------------------------------------------------------}
Function IsCharAlphaNumeric(c : Char) : Boolean;
begin
  // your code here
  Result := False;
  Result :=  ((ord(c) >= 65) and (ord(c) <=90) ) or ((ord(c) >= 97) and (ord(c) <=122) );
end;

{------------------------------------------------------------------------------
  Function: IsRectEmpty
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function IsRectEmpty(const lprc: TRect): Boolean; 
begin
  with lprc do
    Result := (Left = 0) and (Top = 0) and (Right = 0) and (Bottom = 0);
end;

{------------------------------------------------------------------------------
  Function: MakeLParam
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function MakeLParam(l, h: Word): LPARAM;
begin
  Result := MakeLong(l, h);
end;

{------------------------------------------------------------------------------
  Function: MakeLResult
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function MakeLResult(l, h: Word): LRESULT;
begin
  Result := MakeLong(l, h);
end;

{------------------------------------------------------------------------------
  Function: MakeWParam
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function MakeWParam(l, h: Word): WPARAM;
begin
  Result := MakeLong(l, h);
end;

{------------------------------------------------------------------------------
  Function: OffSetRect
  Params:   Rect: points to structure that moves.
            dx  : amount to move the rect to left or right.  Must be negative to move to left.
            dy  : amount to move the rect up or down.  Mmust be negative to move up.
  Returns:  True if succesful

  Moves the rectangle up or down, left or right.
 ------------------------------------------------------------------------------}
function OffSetRect(var Rect: TRect; dx,dy: Integer): Boolean;
Begin
  with Rect do
  begin
    Left := Left + dx;
    Right := Right + dx;
    Top := Top + dy;
    bottom := bottom + dy;
  end;
  if (rect.Left >= 0) and (Rect.Top >= 0) then
  Result := True
  else
  result := False;

end;

{------------------------------------------------------------------------------
  Function: PointtoSmallPoint
  Params:
  Returns:

 ------------------------------------------------------------------------------}
Function PointtoSmallPoint(const P : TPoint) : TSmallPoint;
Begin
  Result.X := P.X;
  Result.Y := P.Y;
end;

{------------------------------------------------------------------------------
  Function: PtInRect
  Params:  Rect
           Point
  Returns: True if point is in rect

  Determines if the POINT is within the rect
  It is considered inside if it lies on the left top, or within the rectangle.
  It is outside ther rect if it's on the bottom or right.
 ------------------------------------------------------------------------------}
Function PtInRect(Rect : TRect; Point : TPoint) : Boolean;
Begin
  Result := not (
    (Point.X < Rect.Left) or
    (Point.X >= Rect.Right) or
    (Point.Y < Rect.Top) or
    (Point.Y >= Rect.Bottom)
  );
end;

{------------------------------------------------------------------------------
  Function: RectVisible
-------------------------------------------------------------------------------}
Function RectVisible(dc : hdc; Rect: TRect) : Boolean;
Begin
//TODO: Finish me!  RectVisible in winapi.inc
result := True;
end;

{------------------------------------------------------------------------------
  Function: ScrollWindow   In progress pbd
  Params:   Handle
            XAmount +scroll down -scroll up 
            Rect: Rect to move
            ClipRect: Boundaries at which the pixels go to /dev/nul
  Returns:  More than a simple boolean but for compatibilty bool will do

  scrolls a window or portion of a window
 ------------------------------------------------------------------------------}
function ScrollWindow(hWnd: HWND; XAmount, YAmount: Integer; Rect, ClipRect: PRect): Boolean;
begin
  Result := ScrollWindowEx(hWnd, XAmount, YAmount, Rect, ClipRect, 0, 0, 0);
end;

{------------------------------------------------------------------------------
  Function: SetRect
  Params:
  Returns: 
 ------------------------------------------------------------------------------}
Function SetRect(var Rect : TRect; xLeft,yTop,xRight,yBottom : Integer) : Boolean;
Begin
  Result := True;
  try
   with Rect do
   begin
     Left := xLeft;
     Top := yTop;
     Right := xRight;
     Bottom := yBottom;
   end;
  except
   Result := False;
  End;
End;

{------------------------------------------------------------------------------
  Function: SetRectEmpty
  Params: Rect to clear 
  Returns: essentially nothing
 ------------------------------------------------------------------------------}
function SetRectEmpty(var lprc: TRect): Boolean;
begin
  FillChar(lprc, SizeOf(TRect), 0);
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: SetScrollPos
  Params:   Handle, nBar, nPos, bRedraw
  Returns:  The old position

  The SetScrollPos function sets the position of the scroll box (thumb) in the
  specified scroll bar and, if requested, redraws the scroll bar to reflect
  the new position of the scroll box.
 ------------------------------------------------------------------------------}
function SetScrollPos(Handle: HWND; nBar, nPos: Integer; bRedraw: Boolean): Integer;
var
  Info: TScrollInfo;
begin
  Info.fMask := SIF_POS;
  Info.nPos := nPos;
  Result := SetScrollInfo(Handle, nBar, Info, bRedraw);
end;

{------------------------------------------------------------------------------
  Function: SetScrollRange
  Params:   Handle, nBar, nMinPos, nMaxPos, bRedraw
  Returns:  True is succesful

  The SetScrollRange function sets the minimum and maximum position values
  for the specified scroll bar.
 ------------------------------------------------------------------------------}
function SetScrollRange(Handle: HWND; nBar, nMinPos, nMaxPos: Integer; bRedraw: Boolean): Boolean;
var
  Info: TScrollInfo;
begin
  Info.fMask := SIF_RANGE;
  Info.nMin := nMinPos;
  Info.nMAx := nMaxPos;
  SetScrollInfo(Handle, nBar, Info, bRedraw);
  Result := True;
end;

{------------------------------------------------------------------------------
  Function:
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function SmallPointtoPoint(const P : TSmallPoint) : Tpoint;
Begin
  Result.X := P.X;
  Result.Y := P.Y;
end;

{------------------------------------------------------------------------------
  Function: UnionRect          pbd
  Params: lprcDst: TRect Result  INTENT OUT
          lprcSrc1, lprcSrc2 TRects to Union INTENT IN
  Returns: Boolean 0 on failure

 ------------------------------------------------------------------------------}
function UnionRect(var lprcDst: TRect; const lprcSrc1, lprcSrc2: TRect): Boolean;
begin
  Result := True;
  try
   lprcDst.Left := Min(lprcSrc1.Left, lprcSrc2.Left);
   lprcDst.Top := Min(lprcSrc1.Top, lprcSrc2.Top);
   lprcDst.Right := Max(lprcSrc1.Right, lprcSrc2.Right);
   lprcDst.Bottom := Max(lprcSrc1.Bottom, lprcSrc2.Bottom);
  except
   Result := False;
  end;

end;

//##apiwiz##epi##   // Do not remove
{ =============================================================================

  $Log$
  Revision 1.7  2001/01/23 18:42:10  lazarus
  Added InvalidateRect to gtkwinapi.inc
  Shane

  Revision 1.6  2000/09/10 23:08:30  lazarus
  MWE:
    + Added CreateCompatibeleBitamp function
    + Updated TWinControl.WMPaint
    + Added some checks to avoid gtk/gdk errors
    - Removed no fixed warning from GetDC
    - Removed some output

  Revision 1.5  2000/08/14 12:31:12  lazarus
  Minor modifications for SynEdit .
  Shane

  Revision 1.4  2000/08/11 14:59:09  lazarus
  Adding all the Synedit files.
  Changed the GDK_KEY_PRESS and GDK_KEY_RELEASE stuff to fix the problem in the editor with the shift key being ignored.
  Shane

  Revision 1.3  2000/08/10 18:56:24  lazarus
  Added some winapi calls.
  Most don't have code yet.
  SetTextCharacterExtra
  CharLowerBuff
  IsCharAlphaNumeric
  Shane

  Revision 1.2  2000/07/30 21:48:32  lazarus
  MWE:
    = Moved ObjectToGTKObject to GTKProc unit
    * Fixed array checking in LoadPixmap
    = Moved LM_SETENABLED to API func EnableWindow and EnableMenuItem
    ~ Some cleanup

  Revision 1.1  2000/07/13 10:28:28  michael
  + Initial import

  Revision 1.6  2000/05/11 22:04:15  lazarus
  MWE:
    + Added messagequeue
    * Recoded SendMessage and Peekmessage
    + Added postmessage
    + added DeliverPostMessage

  Revision 1.5  2000/05/08 12:54:19  lazarus
  Removed some writeln's
  Added alignment for the TLabel.  Isn't working quite right.
  Added the shell code for WindowFromPoint and GetParent.
  Added FindLCLWindow
  Shane

  Revision 1.4  2000/04/10 22:22:18  lazarus
  MWE:
    + Added fpcdir to makefiles

  Revision 1.3  2000/04/10 14:03:07  lazarus
  Added SetProp and GetProp winapi calls.
  Added ONChange to the TEdit's published property list.
  Shane

  Revision 1.2  2000/04/07 16:59:55  lazarus
  Implemented GETCAPTURE and SETCAPTURE along with RELEASECAPTURE.
  Shane

  Revision 1.1  2000/04/02 20:49:57  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.32  2000/03/31 18:41:03  lazarus
  Implemented MessageBox / Application.MessageBox calls. No icons yet, though...

  Revision 1.31  2000/03/30 18:07:55  lazarus
  Added some drag and drop code
  Added code to change the unit name when it's saved as a different name.  Not perfect yet because if you are in a comment it fails.

  Shane

  Revision 1.30  2000/03/28 22:47:50  lazarus
  MWE:
    Started with the blt function family

  Revision 1.29  2000/03/19 23:01:43  lazarus
  MWE:
    = Changed splashscreen loading/colordepth
    = Chenged Save/RestoreDC to platform  dependent, since they are
      relative to a DC

  Revision 1.28  2000/03/16 23:58:46  lazarus
  MWE:
    Added TPixmap for XPM support

  Revision 1.27  2000/03/14 19:49:05  lazarus
  Modified the painting process for TWincontrol.  Now it runs throug it's FCONTROLS list and paints all them
  Shane

  Revision 1.26  2000/03/10 18:31:11  lazarus
  Added TSpeedbutton code
  Shane

  Revision 1.25  2000/03/09 23:48:04  lazarus
  MWE:
    * Fixed colorcache
    * Fixed black window in new editor
    ~ Did some cosmetic stuff

  From Peter Dyson <peter@skel.demon.co.uk>:
    + Added Rect api support functions
    + Added the start of ScrollWindowEx

  Revision 1.24  2000/03/08 23:57:39  lazarus
  MWE:
    Added SetSysColors
    Fixed TEdit text bug (thanks to hans-joachim ott <hjott@compuserve.com>)
    Finished GetKeyState
    Added changes from Peter Dyson <peter@skel.demon.co.uk>
    - a new GetSysColor
    - some improvements on ExTextOut

  Revision 1.23  2000/03/06 00:05:05  lazarus
  MWE: Added changes from Peter Dyson <peter@skel.demon.co.uk> for a new
    release of mwEdit (0.92)

  Revision 1.22  2000/03/03 22:58:26  lazarus
  MWE:
    Fixed focussing problem.
      LM-FOCUS was bound to the wrong signal
    Added GetKeyState api func.
      Now LCL knows if shift/trl/alt is pressed (might be handy for keyboard
      selections ;-)

  Revision 1.21  2000/02/24 21:15:31  lazarus
  Added TCustomForm.GetClientRect and RequestAlign to try and get the controls to align correctly when a MENU is present.  Not Complete yet.

  Fixed the bug in TEdit that caused it not to update it's text property.  I will have to
  look at TMemo to see if anything there was affected.

  Added SetRect to WinAPI calls
  Added AdjustWindowRectEx to WINAPI calls.
  Shane

  Revision 1.20  2000/02/22 23:26:13  lazarus
  MWE: Fixed cursor movement in editor
       Started on focus problem

  Revision 1.19  2000/01/31 20:00:22  lazarus
  Added code for Application.ProcessMessages.  Needs work.
  Added TScreen.Width and TScreen.Height.  Added the code into
  GetSystemMetrics for these two properties.
  Shane

  Revision 1.18  2000/01/25 00:38:25  lazarus
  MWE:
    Added GetFocus

  Revision 1.17  2000/01/18 21:47:00  lazarus
  Added OffSetRec

  Revision 1.16  2000/01/16 23:23:07  lazarus
  MWE:
    Added/completed scrollbar API funcs

  Revision 1.15  1999/12/21 21:35:54  lazarus
  committed the latest toolbar code.  Currently it doesn't appear anywhere and I have to get it to add buttons correctly through (I think) setstyle.  I think I'll implement the LM_TOOLBARINSERTBUTTON call there.
  Shane

  Revision 1.14  1999/12/21 00:07:06  lazarus
  MWE:
    Some fixes
    Completed a bit of DraWEdge

  Revision 1.13  1999/12/20 21:01:14  lazarus
  Added a few things for compatability with Delphi and TToolbar
  Shane

  Revision 1.12  1999/12/18 18:27:32  lazarus
  MWE:
    Rearranged some events to get a LM_SIZE, LM_MOVE and LM_WINDOWPOSCHANGED
    Initialized the TextMetricstruct to zeros to clear unset values
    Get mwEdit to show more than one line
    Fixed some errors in earlier commits

  Revision 1.11  1999/12/14 21:07:12  lazarus
  Added more stuff for TToolbar
  Shane

  Revision 1.10  1999/12/02 19:00:59  lazarus
  MWE:
    Added (GDI)Pen
    Changed (GDI)Brush
    Changed (GDI)Font (color)
    Changed Canvas to use/create pen/brush/font
    Hacked mwedit to allow setting the number of chars (till it get a WM/LM_SIZE event)
    The editor shows a line !

  Revision 1.9  1999/11/29 00:46:47  lazarus
  MWE:
    Added TBrush as gdiobject
    commented out some more mwedit MWE_FPC ifdefs

  Revision 1.8  1999/11/25 23:45:08  lazarus
  MWE:
    Added font as GDIobject
    Added some API testcode to testform
    Commented out some more IFDEFs in mwCustomEdit

  Revision 1.7  1999/11/19 01:09:43  lazarus
  MWE:
    implemented TCanvas.CopyRect
    Added StretchBlt
    Enabled creation of TCustomControl.Canvas
    Added a temp hack in TWinControl.Repaint to get a LM_PAINT

  Revision 1.6  1999/11/18 00:13:08  lazarus
  MWE:
    Partly Implemented SelectObject
    Added  ExTextOut
    Added  GetTextExtentPoint
    Added  TCanvas.TextExtent/TextWidth/TextHeight
    Added  TSize and HPEN

  Revision 1.5  1999/11/17 01:16:40  lazarus
  MWE:
    Added some more API stuff
    Added an initial TBitmapCanvas
    Added some DC stuff
    Changed and commented out, original gtk linedraw/rectangle code. This
      is now called through the winapi wrapper.

  Revision 1.4  1999/11/16 01:32:22  lazarus
  MWE:
    Added some more DC functionality

}
