{ $Id$ }
{
                      ------------------------------------
                      win32debug.pp  -  graphic dump utils 
                      ------------------------------------
 
 @created(Fri Jun 1th WET 2007)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)                       

 This unit contains utility functions to show the contents of graphics
 
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

unit Win32Debug;

{$mode objfpc}{$H+}

interface 

uses
  windows, ctypes, sysutils, win32Extra;

procedure DbgDumpBitmap(ABitmap: HBITMAP; ATitle: String = ''; AUseBitBlt: Boolean = False; AWidth: Integer = -1; AHeight: Integer = -1);
procedure DbgDumpDC(ADC: HDC; ATitle: String = ''; AUseBitBlt: Boolean = False; AWidth: Integer = -1; AHeight: Integer = -1);

implementation

type
  PDbgDumpInfo = ^TDbgDumpInfo;
  TDbgDumpInfo = record                            
    Width, Height: Integer;
    OrgWidth, OrgHeight: Integer;
    Bitmap: HBITMAP;
    ColorIdx: Byte;
    UseAlphaBlend: Boolean;
  end;

function DbgWindowProc(Wnd: HWnd; Msg: UINT; WParam: WPAram; LParam: LPARAM): LRESULT; stdcall;
  function GetInfo: Pointer;
  begin
    // grrr.... this function isn't mapped to GetWindowLong
    {$ifdef CPU64}
    Result := Pointer(GetWindowLongPtr(wnd, GWL_USERDATA));
    {$else}
    Result := Pointer(GetWindowLong(wnd, GWL_USERDATA));
    {$endif}
  end;
const
  COLORS: array[0..7] of COLORREF = (
    $00000000,
    $000000FF,
    $0000FF00,
    $0000FFFF,
    $00FF0000,
    $00FF00FF,
    $00FFFF00,
    $00FFFFFF
  );
var
  Info: PDbgDumpInfo;
  PS: TPaintStruct;
  DC: HDC;
  OldBmp: HBITMAP;
  Blend: TBlendFunction;
  br: HBRUSH;
begin
  Result := 0;
  case Msg of
    WM_PAINT: begin
      Info := GetInfo;
      BeginPaint(Wnd, PS);

      br := CreateSolidBrush(COLORS[Info^.ColorIdx and $7]);
      FillRect(PS.hDC, PS.rcPaint, br);
      DeleteObject(br);

      DC := CreateCompatibleDC(PS.hdc);
      OldBmp := SelectObject(DC, Info^.Bitmap);
      
      if Info^.UseAlphaBlend
      then begin
        Blend.BlendOp := AC_SRC_OVER;
        Blend.BlendFlags := 0;
        Blend.SourceConstantAlpha := 255;
        Blend.AlphaFormat := AC_SRC_ALPHA;

        Win32Extra.AlphaBlend(PS.hDC, 0, 0, Info^.Width, Info^.Height, DC, 0,0, Info^.OrgWidth, Info^.OrgHeight, Blend);
      end
      else begin
        BitBlt(PS.hDC, 0, 0, Info^.Width, Info^.Height, DC, 0,0, SRCCOPY);
      end;
      
      SelectObject(DC, OldBmp);
      DeleteDC(DC);
      EndPaint(Wnd, PS);
    end;
    WM_DESTROY: begin
      Info := GetInfo;
      DeleteObject(Info^.Bitmap);
      Dispose(Info);
    end;
    WM_LBUTTONUP: begin
      Info := GetInfo;
      Info^.ColorIdx := (Info^.ColorIdx + 1) and $7;
      InvalidateRect(Wnd, nil, False);
    end;
  else
    Result := DefWindowProc(wnd, Msg, WParam, LParam);
  end;
end;

var
  MDbgClassCreated: Boolean = False;

procedure DbgCreateClass;
var
  wc: TWndClass;
begin
  if MDbgClassCreated then Exit;

  FillByte(wc, SizeOf(wc), 0);
  wc.style := CS_HREDRAW or CS_VREDRAW;
  wc.lpfnWndProc := @DbgWindowProc;
  wc.hInstance := hinstance;
  wc.hbrBackground := GetStockObject(BLACK_BRUSH);
  wc.lpszClassName := 'LazDbgWindow';
  RegisterClass(wc);

  MDbgClassCreated := True;
end;

procedure DbgCreateWindow(AInfo: PDbgDumpInfo; const ATitle: String);
var
  window: HWND;  
  w, h: Integer;
begin
  DbgCreateClass;
  if AInfo^.Width < 50 then W := 50 else w := AInfo^.Width;
  if AInfo^.Height < 25 then H := 25 else H := AInfo^.Height;
  window := CreateWindowEx(WS_EX_TOOLWINDOW, 'LazDbgWindow', PChar(ATitle), WS_OVERLAPPEDWINDOW, CW_USEDEFAULT, CW_USEDEFAULT, W + 8, H+ 25, 0, 0, HINSTANCE, nil);
  {$ifdef CPU64}
  SetWindowLongPtr(window, GWL_USERDATA, PtrInt(AInfo));
  {$else}
  SetWindowLong(window, GWL_USERDATA, PtrInt(AInfo));
  {$endif}

  ShowWindow(window, SW_SHOWNOACTIVATE); 
end;

procedure InternalDumpBitmap(ABitmap: HBITMAP; ADesc, ATitle: String; AWidth: Integer; AHeight: Integer; AUseBitBlt: Boolean);
var
  Info: PDbgDumpInfo;
  h,w,d: Integer;
  WinBmp: Windows.TBitmap;
begin
  New(Info);
  if (ABitmap = 0)
  or (Windows.GetObject(ABitmap, SizeOf(WinBmp), @WinBmp) = 0)
  then begin
    w := 0; h:= 0; d := 0;
    Info^.Bitmap := 0;
    if AWidth = -1 then AWidth := 0;
    if AHeight = -1 then AHeight := 0;
  end
  else begin
    w := WinBmp.bmWidth;
    h := WinBmp.bmHeight;
    d := WinBmp.bmBitsPixel;
    if AWidth = -1 then AWidth := W;
    if AHeight = -1 then AHeight := H;
    Info^.Bitmap := CopyImage(ABitmap, IMAGE_BITMAP, AWidth, AHeight, 0);
  end;
  
  Info^.Width := AWidth;
  Info^.Height := AHeight;
  Info^.OrgWidth := w;
  Info^.OrgHeight := h;
  Info^.UseAlphaBlend := (d > 24) and not AUseBitBlt;

  ATitle := ATitle + Format(' (%s W:%d H:%d D:%d)', [ADesc, w, h, d]);
  DbgCreateWindow(Info, ATitle);
end;

procedure DbgDumpBitmap(ABitmap: HBITMAP; ATitle: String; AUseBitBlt: Boolean; AWidth, AHeight: Integer);
begin
  InternalDumpBitmap(ABitmap, Format('Bitmap:$%x', [ABitmap]), ATitle, AWidth, AHeight, AUseBitBlt);
end;

procedure DbgDumpDC(ADC: HDC; ATitle: String; AUseBitBlt: Boolean; AWidth, AHeight: Integer);
var
  bmp: HBITMAP;
begin
  bmp := CreateBitmap(1,1,1,1,nil);
  // select dummy to get selected bitmap
  bmp := SelectObject(ADC, bmp);
  InternalDumpBitmap(bmp, Format('DC:$%x', [ADC]), ATitle, AWidth, AHeight, AUseBitBlt);
  // restore bitmap and delete dummy
  DeleteObject(SelectObject(ADC, bmp));
end;

end.
