{
 *****************************************************************************
 *                            WinCEWinApiEmu.pp                              *
 *                            -----------------                              *
 * Extra WinCE code that's not in the RTL or present on all WinCE versions.  *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Roozbeh GHolizadeh
          Marc Weustink

  Abstract:
    Missing and useful windows api are defined and emulated here.
    Not all functionalities are present,but only those neccessary for lcl to function.
}
unit WinCEExtra;

{$mode objfpc}{$H+}

interface

uses
  LCLType, LCLIntf, {keep both before windows}
  Windows, Classes, SysUtils, GraphType;

type
  DrawStateProc = function(
    dc:HDC;         // handle to device context
    ldata: LPARAM;  // image information
    wData: WPARAM;  // more image information
    cx: integer;    // image width
    cy: integer     // image height  
  ) : boolean;

const
  { State type }
  DSS_NORMAL    = $0000;
  DSS_UNION     = $0010;  { Gray string appearance }
  DSS_DISABLED  = $0020;
  DSS_MONO      = $0080;
  DSS_RIGHT     = $8000;
  DSS_DEFAULT   = $0040;  { Make it bold }
  DSS_HIDEPREFIX= $0200;
  DSS_PREFIXONLY= $0400;
  
  { missing progress bar styles }
  PBS_SMOOTH=01;
  PBS_VERTICAL=04;
  PBM_SETRANGE32=WM_USER+6;
  
  { missing listview styles}
  LVS_EX_LABELTIP         = $00004000;
  
  { missing messages }

  WM_HIBERNATE = $03FF;


function DrawState(dc:HDC ; hbr : HBRUSH ; func: DRAWSTATEPROC ; lp:LPARAM; wp:WPARAM;x,y,cx,cy:integer;flags:UINT) : boolean;
function GetTopWindow(hWnd:HWND):HWND;

// missing imagelist macros and constants

const
// image list copy flags
  ILCF_MOVE = $00000000;
  ILCF_SWAP = $00000001;

{$ifdef win32}
function ImageList_Copy(himlDst: HIMAGELIST; iDst: longint; himlSrc: HIMAGELIST; Src: longint; uFlags: UINT): BOOL; stdcall; external 'comctl32';
{$else}
function ImageList_Copy(himlDst: HIMAGELIST; iDst: longint; himlSrc: HIMAGELIST; Src: longint; uFlags: UINT): BOOL; cdecl; external KernelDLL;
{$endif}

const
  // BlendOp flags
  AC_SRC_OVER = $00;
  // AlphaFormat flags
  AC_SRC_ALPHA = $01;

var
  // AlphaBlend is only defined for CE5 and up
  // load dynamic and use ownfunction if not defined
  AlphaBlend: function(hdcDest: HDC; nXOriginDest, nYOriginDest, nWidthDest, nHeightDest: Integer; hdcSrc: HDC; nXOriginSrc, nYOriginSrc, nWidthSrc, nHeightSrc: Integer; blendFunction: TBlendFunction): BOOL; cdecl;

  // SHSendBackToFocusWindow should be available since a long time, but in practice some
  // devices don't have it in their aygshell.dll library
  // see:
  SHSendBackToFocusWindow: procedure(uMsg: UINT; wp: WPARAM; lp: LPARAM); cdecl;
  // For reference the previous static loading:
  //{$ifdef wince}
  //procedure SHSendBackToFocusWindow(uMsg: UINT; wp: WPARAM; lp: LPARAM); cdecl; external 'aygshell' index 97;
  //{$endif}

implementation

uses
  WinCEProc;
  
const
  wPattern55AA: array[1..8] of word = ($5555, $aaaa, $5555, $aaaa, $5555, $aaaa, $5555, $aaaa);

function GetTopWindow(hWnd:HWND):HWND;
begin
  Result := GetWindow(hWnd,GW_CHILD);
end;


{ Wine sources - www.winehq.com - mostly used for emulating DrawState functions }
function DrawStateJam(dc:HDC; opcode:UINT; func: DrawStateProc; lp:LPARAM; wp: WPARAM; rc:LPRECT; dtflags: UINT): boolean;
var
  memdc: HDC;
  hbmsave: HBITMAP;
  cx,cy: integer;
begin
  cx := rc^.Right - rc^.left;
  cy := rc^.bottom - rc^.top;
  
  case opcode of
    DST_TEXT, DST_PREFIXTEXT:
      Result := DrawTextW(dc, PWideChar(lp), wp, rc, dtflags) <> 0;
  
    DST_ICON:
      Result := DrawIcon(dc, rc^.left, rc^.top, lp);
  
    DST_BITMAP: begin
      memdc := CreateCompatibleDC(dc);
      if memdc = 0 then Exit(False);
      
      hbmsave := SelectObject(memdc, lp);
      if hbmsave = 0 then
      begin
        DeleteDC(memdc);
        Exit(False);
      end;
      
      Result := BitBlt(dc, rc^.left, rc^.top, cx, cy, memdc, 0, 0, SRCCOPY);
      SelectObject(memdc, hbmsave);
      DeleteDC(memdc);
    end;
  
    DST_COMPLEX: begin
      if func <> nil then
      begin
        { DRAWSTATEPROC assumes that it draws at the center of coordinates  }
        //OffsetViewportOrgEx(dc, rc^.left, rc^.top, nil);
        Result := func(dc, lp, wp, cx, cy);
        
        { Restore origin }
        //OffsetViewportOrgEx(dc, -rc^.left, -rc^.top, nil);
      end 
      else Result := False;
    end;        
  else  
    Result := False;
  end;
end;


{$goto on} // TODO: remove goto

function DrawState(dc: HDC; hbr: HBRUSH; func: DRAWSTATEPROC; lp:LPARAM; wp:WPARAM; x, y, cx, cy: integer; flags: UINT): boolean;
label
  cleanup;
var
  hbm,hbmsave :HBITMAP;
  hfsave : HFONT;
  hbsave,hbrtmp : HBRUSH;
  memdc : HDC;
  rc:TRECT;
  dtflags:UINT;
  opcode:UINT;
  len:integer;
  tmp : boolean;
  sz:SIZE;
  //ici:^CURSORICONINFO;
  bm:BITMAP;
  fg, bg : COLORREF;
  
  h55AABrush : HBRUSH;
  h55AABitmap: HBITMAP;
  
begin
  Result := False; 
  hbrtmp := 0;
  dtflags := DT_NOCLIP;
  opcode := flags and $f;
  len := wp;

  if  ((opcode = DST_TEXT) or (opcode = DST_PREFIXTEXT)) and (len=0) then
    len := Length(WideString(PWideChar(lp))); // The string is '\0' terminated

  { Find out what size the image has if not given by caller }
  if (cx=0) or (cy=0) then
  begin
    case opcode of
      DST_TEXT, DST_PREFIXTEXT:
        begin
          if not GetTextExtentPoint32W(dc, PWideChar(lp), len, sz) then Exit;
        end;

      {DST_ICON:
        begin
          ici = (CURSORICONINFO *)GlobalLock16((HGLOBAL16)lp);
          if(!ici) then return false;
          s.cx = ici->nWidth;
          s.cy = ici->nHeight;
          GlobalUnlock16((HGLOBAL16)lp);
        end;}

      DST_BITMAP:
        begin
          if GetObject(lp, sizeof(bm), @bm) = 0 
          then Exit;
          sz.cx := bm.bmWidth;
          sz.cy := bm.bmHeight;
        end;

      DST_COMPLEX: {/* cx and cy must be set in this mode */}
        Exit;
    end;

    if cx = 0 then cx := sz.cx;
    if cy = 0 then cy := sz.cy;
  end;

  rc.left   := x;
  rc.top    := y;
  rc.right  := x + cx;
  rc.bottom := y + cy;

  if (flags and DSS_RIGHT) <> 0 then { This one is not documented in the win32.hlp file }
    dtflags := dtflags or DT_RIGHT;
  
  if opcode = DST_TEXT then
    dtflags := dtflags or DT_NOPREFIX;

  { For DSS_NORMAL we just jam in the image and return }
  if (flags and $7ff0) = DSS_NORMAL then
    Exit(DrawStateJam(dc, opcode, func, lp, len, @rc, dtflags));

  { For all other states we need to convert the image to B/W in a local bitmap
    before it is displayed }
  fg := SetTextColor(dc, RGB(0, 0, 0));
  bg := SetBkColor(dc, RGB(255, 255, 255));
  hbm := 0;
  hbmsave := 0;
  memdc := 0;
  hbsave := 0;

  { From here on we must use "goto cleanup" when something goes wrong }
  // MWE: you can also use an exception block for this.

  hbm := CreateBitmap(cx, cy, 1, 1, nil);
  if hbm = 0 then goto cleanup;
  
  memdc := CreateCompatibleDC(dc);
  if memdc = 0 then goto cleanup;
  
  hbmsave := SelectObject(memdc, hbm);
  if hbmsave = 0 then goto cleanup;
  
  rc.top := 0;
  rc.left := 0;
  rc.right := cx;
  rc.bottom := cy;
  if FillRect(memdc, rc, GetStockObject(WHITE_BRUSH)) = 0 then goto cleanup;
  
  SetBkColor(memdc, RGB(255, 255, 255));
  SetTextColor(memdc, RGB(0, 0, 0));
  hfsave := SelectObject(memdc, GetCurrentObject(dc, OBJ_FONT));

  { DST_COMPLEX may draw text as well,
    so we must be sure that correct font is selected }
  if (hfsave = 0) and (opcode <= DST_PREFIXTEXT) then goto cleanup;
  tmp := DrawStateJam(memdc, opcode, func, lp, len, @rc, dtflags);
  if hfsave <> 0 then SelectObject(memdc, hfsave);
  if not tmp then goto cleanup;

  { This state cause the image to be dithered }
  if (flags and DSS_UNION) <> 0 then
  begin
    h55AABitmap := CreateBitmap( 8, 8, 1, 1, @wPattern55AA);
    h55AABrush := CreatePatternBrush(h55AABitmap);
    hbsave := SelectObject(memdc, h55AABrush);
    if hbsave = 0 
    then begin
      DeleteObject(h55AABrush);
      DeleteObject(h55AABitmap);
      goto cleanup;
    end;
    
    tmp := PatBlt(memdc, 0, 0, cx, cy, $00FA0089);
    SelectObject(memdc, hbsave);
    DeleteObject(h55AABrush);
    DeleteObject(h55AABitmap);
    if not tmp then goto cleanup;
  end;

  if (flags and DSS_DISABLED) <> 0 
  then
     hbrtmp := CreateSolidBrush(LCLIntf.GetSysColor(COLOR_3DHILIGHT))
  else if (flags and DSS_DEFAULT) <> 0 
  then
     hbrtmp := CreateSolidBrush(LCLIntf.GetSysColor(COLOR_3DSHADOW));

  { Draw light or dark shadow }
  if (flags and (DSS_DISABLED or DSS_DEFAULT)) <> 0 then
  begin
    if hbrtmp = 0 then goto cleanup;
    hbsave := SelectObject(dc, hbrtmp);
    if hbsave = 0 then goto cleanup;
    if not BitBlt(dc, x+1, y+1, cx, cy, memdc, 0, 0, $00B8074A)  then goto cleanup;
    SelectObject(dc, hbsave);
    DeleteObject(hbrtmp);
    hbrtmp := 0;
  end;

  if (flags and DSS_DISABLED) <> 0 then
  begin
    hbrtmp := CreateSolidBrush(LCLIntf.GetSysColor(COLOR_3DSHADOW));
    hbr := hbrtmp;
    if hbrtmp = 0 then goto cleanup;
  end
  else begin
    if hbr = 0 
    then hbr := GetStockObject(BLACK_BRUSH);
  end;

  hbsave := SelectObject(dc, hbr);

  if not BitBlt(dc, x, y, cx, cy, memdc, 0, 0, $00B8074A) then goto cleanup;

  Result := True;

cleanup:
  SetTextColor(dc, fg);
  SetBkColor(dc, bg);

  if(hbsave<>0)  then SelectObject(dc, hbsave);
  if(hbmsave<>0) then SelectObject(memdc, hbmsave);
  if(hbrtmp<>0)  then DeleteObject(hbrtmp);
  if(hbm<>0)     then DeleteObject(hbm);
  if(memdc<>0)   then DeleteDC(memdc);
end;

function _AlphaBlend(hdcDest: HDC; nXOriginDest, nYOriginDest, nWidthDest, nHeightDest: Integer; hdcSrc: HDC; nXOriginSrc, nYOriginSrc, nWidthSrc, nHeightSrc: Integer; blendFunction: TBlendFunction): BOOL; cdecl;
var
  SCA: Byte absolute blendFunction.SourceConstantAlpha;
  
  R: TRect;
  DC, TmpDC: HDC;
  OldBmp, OldTmpBmp, SrcBmp, DstBmp, TmpBmp, AlphaBmp: HBITMAP;
  StretchSrc: Boolean;
  SrcSection, DstSection: TDIBSection;
  Info: record
    Header: TBitmapInfoHeader;
    Colors: array[0..3] of Cardinal; // reserve extra color for colormasks
  end;

  SrcBytesPtr, DstBytesPtr, TmpBytesPtr, AlphaBytesPtr: Pointer;
  SrcLinePtr, DstLinePtr: PByte;
  CleanupSrc, CleanupSrcPtr, CleanupDst, CleanupAlpha: Boolean;
  SrcSize: Cardinal;
  SrcPixelBytes, DstPixelBytes: Byte;
  SrcRowStride, DstRowStride: Integer;

  X, Y: Integer;
  SrcRGBA, TmpRGBA, DstRGBA: PRGBAQuad;
  SrcAlpha: PByte;
begin
  if nXOriginSrc < 0 then Exit(False);
  if nYOriginSrc < 0 then Exit(False);
  if nWidthSrc < 0 then Exit(False);
  if nHeightSrc < 0 then Exit(False);
  if nWidthDest < 0 then Exit(False);
  if nHeightDest < 0 then Exit(False);
  
  if blendFunction.SourceConstantAlpha = 0
  then Exit(True); // nothing to do

  if (blendFunction.AlphaFormat = 0)
  and (blendFunction.SourceConstantAlpha = 255)
  then begin
    // simple strechblt
    Result := StretchBlt(hdcDest, nXOriginDest, nYOriginDest, nWidthDest, nHeightDest, hdcSrc, nXOriginSrc, nYOriginSrc, nWidthSrc, nHeightSrc, SRCCOPY);
    Exit;
  end;

  // get source info, atleast bitmap, section if available
  SrcBmp := GetCurrentObject(hdcSrc, OBJ_BITMAP);
  if GetObject(SrcBmp, SizeOf(SrcSection), @SrcSection) = 0 then Exit(False);
  if nXOriginSrc + nWidthSrc > SrcSection.dsBm.bmWidth then Exit(False);
  if nYOriginSrc + nHeightSrc > SrcSection.dsBm.bmHeight then Exit(False);
  
  if (blendFunction.AlphaFormat = AC_SRC_ALPHA) and (SrcSection.dsBm.bmBitsPixel <> 32) then Exit(False); // invalid

  // get destination info, atleast bitmap, section if available
  DstBmp := GetCurrentObject(hdcDest, OBJ_BITMAP);
  if (DstBmp = 0) or (GetObject(DstBmp, SizeOf(DstSection), @DstSection) = 0)
  then begin
    // GetCurrentObject can only be used on memory devices,
    // so fill in some values manually
    DstSection.dsBm.bmWidth := GetDeviceCaps(hdcDest, HORZRES);
    DstSection.dsBm.bmHeight := GetDeviceCaps(hdcDest, VERTRES);
    DstSection.dsBm.bmBitsPixel := GetDeviceCaps(hdcDest, BITSPIXEL);
  end;

  // docs doesn't require dest retangle inside dest.
  // however if dest rect is outside the destination, we're done here
  if nXOriginDest + nWidthDest < 0 then Exit(True);
  if nYOriginDest + nHeightDest < 0 then Exit(True);
  if nXOriginDest >= DstSection.dsBm.bmWidth then Exit(True);
  if nYOriginDest >= DstSection.dsBm.bmHeight then Exit(True);
  
  // setup info shared by alpha, source and destination bytes
  FillChar(Info, sizeof(Info), 0);
  Info.Header.biSize := sizeof(Windows.TBitmapInfoHeader);
  Info.Header.biWidth := nWidthDest;
  Info.Header.biHeight := -nHeightDest; // top down
  Info.Header.biPlanes := 1;
  Info.Header.biBitCount := 32;
  Info.Header.biSizeImage := nWidthDest * nHeightDest * 4;
  Info.Header.biCompression := BI_BITFIELDS;
  // when 24bpp, CE only supports B8G8R8 encoding
  Info.Colors[0] := $FF0000; {le-red}
  Info.Colors[1] := $00FF00; {le-green}
  Info.Colors[2] := $0000FF; {le-blue}

  StretchSrc := (nWidthDest <> nWidthSrc) or (nHeightDest <> nHeightSrc);
  if StretchSrc
  then begin
    // we need to strech the source
    
    // create alphabmp
    if blendFunction.AlphaFormat = AC_SRC_ALPHA
    then begin
      // create alpha source data
      R := Rect(nXOriginSrc, nYOriginSrc, nXOriginSrc + nWidthSrc, nYOriginSrc + nHeightSrc);
      if not GetBitmapBytes(SrcBmp, R, rileDWordBoundary, SrcBytesPtr, SrcSize) then Exit(False);

      // set info to source size
      Info.Header.biWidth := nWidthSrc;
      Info.Header.biHeight := -nHeightSrc; // top down
      Info.Header.biSizeImage := nWidthSrc * nHeightSrc * 4;

      // create temp bitmap to store orginal grayscale alpha
      TmpBmp := CreateDIBSection(hdcSrc, PBitmapInfo(@Info)^, DIB_RGB_COLORS, TmpBytesPtr, 0, 0);
      if TmpBmp = 0 then Exit(False);
      if TmpBytesPtr = nil
      then begin
        FreeMem(SrcBytesPtr);
        DeleteObject(TmpBmp);
        Exit(False);
      end;
    
      // create grayscale image from alpha
      TmpRGBA := TmpBytesPtr;
      SrcRGBA := SrcBytesPtr;
      while SrcSize > 0 do
      begin
        TmpRGBA^.Blue := SrcRGBA^.Alpha;
        TmpRGBA^.Green := SrcRGBA^.Alpha;
        TmpRGBA^.Red := SrcRGBA^.Alpha;
        TmpRGBA^.Alpha := 255;
        Inc(SrcRGBA);
        Inc(TmpRGBA);
        Dec(SrcSize, 4);
      end;

      // restore to destination size
      Info.Header.biWidth := nWidthDest;
      Info.Header.biHeight := -nHeightDest; // top down
      Info.Header.biSizeImage := nWidthDest * nHeightDest * 4;

      // create bitmap to store stretched grayscale alpha
      AlphaBmp := CreateDIBSection(hdcSrc, PBitmapInfo(@Info)^, DIB_RGB_COLORS, AlphaBytesPtr, 0, 0);
      if (AlphaBmp = 0) or (AlphaBytesPtr = nil)
      then begin
        FreeMem(SrcBytesPtr);
        DeleteObject(TmpBmp);
        DeleteObject(AlphaBmp);
        Exit(False);
      end;
      
      // stretch grayscale alpha bitmap
      DC := CreateCompatibleDC(hdcSrc);
      OldBmp := SelectObject(DC, AlphaBmp);
      TmpDC := CreateCompatibleDC(hdcSrc);
      OldTmpBmp := SelectObject(TmpDC, TmpBmp);
      StretchBlt(DC, 0, 0, nWidthDest, nHeightDest, TmpDC, 0, 0, nWidthSrc, nHeightSrc, SRCCOPY);
      SelectObject(DC, OldBmp);
      DeleteDC(DC);
      SelectObject(TmpDC, OldTmpBmp);
      DeleteDC(TmpDC);
      DeleteObject(TmpBmp);
      FreeMem(SrcBytesPtr);
      
      // as long as AlphaBmp exists, AlphaBytesPtr is valid.
      CleanupAlpha := True;
    end
    else begin
      CleanupAlpha := False;
    end;

    // create new srcbmp
    SrcBmp := CreateDIBSection(hdcSrc, PBitmapInfo(@Info)^, DIB_RGB_COLORS, SrcBytesPtr, 0, 0);
    if (SrcBmp = 0) or (SrcBytesPtr = nil)
    then begin
      DeleteObject(AlphaBmp);
      DeleteObject(SrcBmp);
      Exit(False);
    end;
    SrcSize := Info.Header.biSizeImage;
    CleanupSrc := True;
    CleanupSrcPtr := False;
    SrcPixelBytes := 4;
    SrcRowStride := nWidthDest * SrcPixelBytes;

    DC := CreateCompatibleDC(hdcSrc);
    OldBmp := SelectObject(DC, SrcBmp);
    StretchBlt(DC, 0, 0, nWidthDest, nHeightDest, hdcSrc, nXOriginSrc, nYOriginSrc, nWidthSrc, nHeightSrc, SRCCOPY);
    SelectObject(DC, OldBmp);
    DeleteDC(DC);

    // adjust source size
    nWidthSrc := nWidthDest;
    nHeightSrc := nHeightDest;
    nXOriginSrc := 0;
    nYOriginSrc := 0;
  end
  else begin
    // only get source data
    SrcPixelBytes := SrcSection.dsBm.bmBitsPixel shr 3;
    if SrcSection.dsBm.bmBits <> nil
    then begin
      // source is a dibsection :)
      SrcBytesPtr := SrcSection.dsBm.bmBits;
      SrcRowStride := SrcSection.dsBm.bmWidthBytes;
      CleanupSrc := False;
      CleanupSrcPtr := False;
    end
    else begin
      R := Rect(nXOriginSrc, nYOriginSrc, nXOriginSrc + nWidthSrc, nYOriginSrc + nHeightSrc);
      if not GetBitmapBytes(SrcBmp, R, rileDWordBoundary, SrcBytesPtr, SrcSize) then Exit;
      SrcRowStride := nWidthSrc * SrcPixelBytes;
      CleanupSrc := False;
      CleanupSrcPtr := True;
      nXOriginSrc := 0;
      nYOriginSrc := 0;
    end;
    AlphaBytesPtr := nil;
    CleanupAlpha := False;
  end;

  // if a palette destination or destination isn't a section, create a temp DIB
  if (DstSection.dsBm.bmBitsPixel < 24)
  or (DstSection.dsBm.bmBits = nil)
  or (DstSection.dsBmih.biCompression <> BI_RGB)
  then begin
    // create temp dib
    DstBmp := CreateDIBSection(hdcSrc, PBitmapInfo(@Info)^, DIB_RGB_COLORS, DstBytesPtr, 0, 0);
    // copy destination
    DC := CreateCompatibleDC(hdcDest);
    OldBmp := SelectObject(DC, DstBmp);
    BitBlt(DC, 0, 0, nWidthDest, nHeightDest, hdcDest, nXOriginDest, nYOriginDest, SRCCOPY);
    SelectObject(DC, OldBmp);
    DeleteDC(DC);
    DstPixelBytes := 4;
    DstRowStride := nWidthDest * DstPixelBytes;
    CleanupDst := True;
  end
  else begin
    DstBytesPtr := DstSection.dsBm.bmBits;
    DstPixelBytes := DstSection.dsBm.bmBitsPixel shr 3;
    DstRowStride := DstSection.dsBm.bmWidthBytes;
    Inc(PByte(DstBytesPtr), nXOriginDest + nYOriginDest * DstRowStride);
    CleanupDst := False;
  end;


  // blend image

  SrcLinePtr := SrcBytesPtr;
  Inc(SrcLinePtr, nXOriginSrc + nYOriginSrc * SrcRowStride);
  DstLinePtr := DstBytesPtr;

  if blendFunction.AlphaFormat = AC_SRC_ALPHA
  then begin
    if AlphaBytesPtr <> nil
    then SrcAlpha := AlphaBytesPtr;
    
    if SCA {blendFunction.SourceConstantAlpha} = 255
    then begin
      for y := 1 to nHeightDest do
      begin
        SrcRGBA := Pointer(SrcLinePtr);
        if AlphaBytesPtr = nil
        then SrcAlpha := @SrcRGBA^.Alpha;
        DstRGBA := Pointer(DstLinePtr);
        for x := 1 to nWidthDest do
        begin
          if SrcAlpha^ <> 0
          then begin
            DstRGBA^.Red   := SrcRgba^.Red   + (DstRGBA^.Red   * (255 - SrcAlpha^)) div 255;
            DstRGBA^.Green := SrcRgba^.Green + (DstRGBA^.Green * (255 - SrcAlpha^)) div 255;
            DstRGBA^.Blue  := SrcRgba^.Blue  + (DstRGBA^.Blue  * (255 - SrcAlpha^)) div 255;
            if DstPixelBytes = 4
            then DstRGBA^.Alpha := SrcAlpha^ + (DstRGBA^.Alpha * (255 - SrcAlpha^)) div 255;
          end;
          Inc(SrcRGBA);
          Inc(SrcAlpha, 4);
          Inc(PByte(DstRGBA), DstPixelBytes);
        end;
        Inc(SrcLinePtr, SrcRowStride);
        Inc(DstLinePtr, DstRowStride);
      end;
    end
    else begin
      for y := 1 to nHeightDest do
      begin
        SrcRGBA := Pointer(SrcLinePtr);
        if AlphaBytesPtr = nil
        then SrcAlpha := @SrcRGBA^.Alpha;
        DstRGBA := Pointer(DstLinePtr);
        for x := 1 to nWidthDest do
        begin
          if SrcAlpha^ <> 0
          then begin
            DstRGBA^.Red   := (SrcRgba^.Red   * SCA) div 255 + (DstRGBA^.Red   * (255 - SrcAlpha^)) div 255;
            DstRGBA^.Green := (SrcRgba^.Green * SCA) div 255 + (DstRGBA^.Green * (255 - SrcAlpha^)) div 255;
            DstRGBA^.Blue  := (SrcRgba^.Blue  * SCA) div 255 + (DstRGBA^.Blue  * (255 - SrcAlpha^)) div 255;
            if DstPixelBytes = 4
            then DstRGBA^.Alpha := (SrcAlpha^ * SCA) div 255 + (DstRGBA^.Alpha * (255 - SrcAlpha^)) div 255;
          end;
          Inc(SrcRGBA);
          Inc(SrcAlpha, 4);
          Inc(PByte(DstRGBA), DstPixelBytes);
        end;
        Inc(SrcLinePtr, SrcRowStride);
        Inc(DstLinePtr, DstRowStride);
      end;
    end;
  end
  else begin
    for y := 1 to nHeightDest do
    begin
      SrcRGBA := Pointer(SrcLinePtr);
      if AlphaBytesPtr = nil
      then SrcAlpha := @SrcRGBA^.Alpha;
      DstRGBA := Pointer(DstLinePtr);
      for x := 1 to nWidthDest do
      begin
        DstRGBA^.Red :=   (SrcRGBA^.Red   * SCA) div 255 + (DstRGBA^.Red   * (255 - SCA)) div 255;
        DstRGBA^.Green := (SrcRGBA^.Green * SCA) div 255 + (DstRGBA^.Green * (255 - SCA)) div 255;
        DstRGBA^.Blue :=  (SrcRGBA^.Blue  * SCA) div 255 + (DstRGBA^.Blue  * (255 - SCA)) div 255;
        if (DstPixelBytes = 4) and (SrcPixelBytes = 4)
        then DstRGBA^.Alpha := (SrcAlpha^ * SCA) div 255 + (DstRGBA^.Alpha * (255 - SCA)) div 255;
        Inc(PByte(SrcRGBA), SrcPixelBytes);
        Inc(PByte(DstRGBA), DstPixelBytes);
        Inc(SrcAlpha, 4);
      end;
      Inc(SrcLinePtr, SrcRowStride);
      Inc(DstLinePtr, DstRowStride);
    end;
  end;
  
  // Replace destination if needed and do cleanup
  if CleanupDst
  then begin
    DC := CreateCompatibleDC(hdcDest);
    OldBmp := SelectObject(DC, DstBmp);
    BitBlt(hdcDest, nXOriginDest, nYOriginDest, nWidthDest, nHeightDest, DC, 0, 0, SRCCOPY);
    SelectObject(DC, OldBmp);
    DeleteDC(DC);
    DeleteObject(DstBmp);
  end;
  if CleanupSrc
  then DeleteObject(SrcBmp);
  if CleanupSrcPtr
  then FreeMem(SrcBytesPtr);
  if CleanupAlpha
  then DeleteObject(AlphaBmp);
end;

procedure _SHSendBackToFocusWindow_(uMsg: UINT; wp: WPARAM; lp: LPARAM); cdecl;
begin
  {$ifdef VerboseWinCE}
  DebugLn('Calling _SHSendBackToFocusWindow_, this routine is called when the real one fails dynamic loading ');
  {$endif}
end;

var
  kerneldllhandle: THandle = 0;
  aygshelldllhandle: THandle = 0;
  p: Pointer;

initialization

  // AlphaBlend initialization
  AlphaBlend := @_AlphaBlend;
  {$ifndef win32}
  kerneldllhandle := LoadLibrary(KernelDLL);
  if kerneldllhandle <> 0 then
  begin
    p := GetProcAddress(kerneldllhandle, 'AlphaBlend');
    if p <> nil then Pointer(AlphaBlend) := p;
  end;
  {$endif}


  // SHSendBackToFocusWindow
  {$ifndef win32}
  aygshelldllhandle := LoadLibrary('aygshell');
  if aygshelldllhandle <> 0 then
  begin
    // p := GetProcAddress(aygshelldllhandle, 'SHSendBackToFocusWindow'); <<-- This code doesn't work because the function is only exported by number
    p := GetProcAddress(aygshelldllhandle, PWideChar(PtrInt(97)));
    if p <> nil then Pointer(SHSendBackToFocusWindow) := p
    else SHSendBackToFocusWindow := @_SHSendBackToFocusWindow_;
  end
  else
    SHSendBackToFocusWindow := @_SHSendBackToFocusWindow_;
  {$endif}


finalization

  // AlphaBlend finalization
  AlphaBlend := @_AlphaBlend;
  if kerneldllhandle <> 0 then FreeLibrary(kerneldllhandle);
  kerneldllhandle := 0;

  // SHSendBackToFocusWindow
  SHSendBackToFocusWindow := @_SHSendBackToFocusWindow_;
  if aygshelldllhandle <> 0 then FreeLibrary(aygshelldllhandle);
  aygshelldllhandle := 0;

end.


