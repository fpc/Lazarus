{
 *****************************************************************************
 *                            WinCEWinApiEmu.pp                              *
 *                            -----------------                              *
 *                        Windows API Emulation unit                         *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Roozbeh GHolizadeh

  Abstract:
    Missing and usefull windows api are defined and emulated here.
    Not all functionalities are present,but only those neccessary for lcl to function.
}
unit WinCEWinApiEmu;

{$mode objfpc}{$H+}
{$goto on}
interface

uses
  Windows,Classes, SysUtils;

type
DrawStateProc = function(
  dc:HDC ;       // handle to device context
  ldata : LPARAM;  // image information
  wData : WPARAM;  // more image information
  cx :integer;        // image width
  cy : integer         // image height
) : boolean;

const
{ State type }
DSS_NORMAL    = $0000;
DSS_UNION     = $0010;  { Gray string appearance }
DSS_DISABLED  = $0020;
DSS_MONO      = $0080;
DSS_RIGHT     = $8000;
DSS_DEFAULT   =$0040;  { Make it bold }
DSS_HIDEPREFIX=$0200;
DSS_PREFIXONLY=$0400;

{ missing progress bar styles }
PBS_SMOOTH=01;
PBS_VERTICAL=04;
PBM_SETRANGE32=WM_USER+6;

var
SYSCOLOR_55AABrush : HBRUSH;

function DrawState(dc:HDC ; hbr : HBRUSH ; func: DRAWSTATEPROC ; lp:LPARAM; wp:WPARAM;x,y,cx,cy:integer;flags:UINT) : boolean;
function GetTopWindow(hWnd:HWND):HWND;

function SetProp(_hWnd:HWND; {lpString:LPCSTR;} hData:HANDLE):WINBOOL;
function GetProp(_hWnd:HWND{; lpString:LPCSTR}):HANDLE;
function RemoveProp(_hWnd:HWND{; lpString:LPCSTR}):HANDLE;
function EnumProps(_hWnd:HWND;lpEnumFunc:PROPENUMPROC) : integer;

implementation

type
  PTPropertyListWindows = ^TPropertyListWindows;
  TPropertyListWindows = record
   WindowHWND : HWND;
   WindowInfo : HANDLE;//if you want to make it just like windows this also should be an array!
   NextPropertyListWindows : PTPropertyListWindows;
  end;

Const
wPattern55AA :  array[1..8] of word = ( $5555, $aaaa, $5555, $aaaa, $5555, $aaaa, $5555, $aaaa );

var
h55AABitmap : HBITMAP;//when free it?!
ThePropertyLists : PTPropertyListWindows;



function SetProp(_hWnd:HWND; {lpString:LPCSTR;} hData:HANDLE):WINBOOL;
var
  pPrevPropertyLists,pPropertyLists : PTPropertyListWindows;
begin
  Result := true;
  if ThePropertyLists = nil then
  begin
    New(ThePropertyLists);
    ThePropertyLists^.WindowInfo := 0;
    ThePropertyLists^.WindowHWND := 0;
    ThePropertyLists^.NextPropertyListWindows := nil;
  end;
  pPropertyLists := ThePropertyLists;
  pPrevPropertyLists := nil;
  //traverse through previously created ones and then change it.
  repeat
    if (pPropertyLists^.WindowHWND = _hWnd) or (pPropertyLists^.WindowHWND = 0) then
    begin
      pPropertyLists^.WindowInfo := hData;
      pPropertyLists^.WindowHWND := _hWnd;//if it was 0 then make it hwnd
      break;
    end;
    pPrevPropertyLists := pPropertyLists;
    pPropertyLists := pPropertyLists^.NextPropertyListWindows;
  until pPropertyLists = nil;

  //not found in previously created ones so create a new node
  if pPropertyLists = nil then
  begin
    New(pPrevPropertyLists^.NextPropertyListWindows);
    pPropertyLists := pPrevPropertyLists^.NextPropertyListWindows;

    pPropertyLists^.NextPropertyListWindows := nil;
    pPropertyLists^.WindowHWND := _hWnd;
    pPropertyLists^.WindowInfo := hData;
  end;
end;


function GetProp(_hWnd:HWND{; lpString:LPCSTR}):HANDLE;
var
  pPropertyLists : PTPropertyListWindows;
begin
  Result := 0;
  pPropertyLists := ThePropertyLists;
  if pPropertyLists = nil then
  begin
    exit;
  end;
  repeat
    if (pPropertyLists^.WindowHWND = _hWnd) then
    begin
      result := pPropertyLists^.WindowInfo;
      break;
    end;
    pPropertyLists := pPropertyLists^.NextPropertyListWindows;
  until pPropertyLists = nil;
end;


function RemoveProp(_hWnd:HWND{; lpString:LPCSTR}):HANDLE;
var
  pPrevPropertyLists,pPropertyLists : PTPropertyListWindows;
begin
  Result := 0;
  pPropertyLists := ThePropertyLists;
  pPrevPropertyLists := nil;
  if pPropertyLists = nil then exit;
  repeat
    if (pPropertyLists^.WindowHWND = _hWnd) then
    begin
      result := pPropertyLists^.WindowInfo;
      if pPrevPropertyLists <> nil then
      begin
        pPrevPropertyLists^.NextPropertyListWindows := pPropertyLists^.NextPropertyListWindows;
        Dispose(pPropertyLists);
      end
      else
      begin//now the list contain nothing
        Dispose(pPropertyLists);
        ThePropertyLists := nil;
      end;
    break;
    end;
    pPrevPropertyLists := pPropertyLists;
    pPropertyLists := pPropertyLists^.NextPropertyListWindows;
  until pPropertyLists = nil;
end;

//well we only have one property for each window handle so just find and call that
// return -1 if none found!
function EnumProps(_hWnd:HWND;lpEnumFunc:PROPENUMPROC) : integer;
var
h:HANDLE;
begin
Result := -1;
h:=GetProp(_hWnd);
if h<>0 then
Result := integer(lpEnumFunc(_hWnd,'',h));
end;


procedure FreePropList;
var
  pPropertyLists,pNextPropertyList : PTPropertyListWindows;
begin
  pPropertyLists := ThePropertyLists;
  pNextPropertyList := nil;
  if pPropertyLists = nil then exit;

  repeat
    pNextPropertyList := pPropertyLists^.NextPropertyListWindows;
    Dispose(pPropertyLists);
    pPropertyLists := pNextPropertyList;
  until pPropertyLists = nil;
  
 ThePropertyLists := nil;
end;


function GetTopWindow(hWnd:HWND):HWND;
begin
  Result := GetWindow(hWnd,GW_CHILD);
end;


{ Wine sources - www.winehq.com - mostly used for emulating DrawState functions }
function DrawStateJam(dc:HDC;opcode:UINT;func: DrawStateProc; lp:LPARAM ;wp: WPARAM ;
                                  rc:LPRECT ; dtflags : UINT) : boolean;
var
memdc:HDC;
hbmsave:HBITMAP;
retval:boolean;
cx,cy:integer;
bRet : boolean;
begin
    cx := rc^.Right - rc^.left;
    cy := rc^.bottom - rc^.top;

    case opcode of

    DST_TEXT,DST_PREFIXTEXT:
      exit(boolean(DrawText(dc, PWideChar(lp), wp, rc, dtflags)));

    DST_ICON:
        exit(boolean(DrawIcon(dc, rc^.left, rc^.top, lp)));

    DST_BITMAP:
        begin
        memdc := CreateCompatibleDC(dc);
        if(memdc=0) then exit(false);
        hbmsave := SelectObject(memdc, lp);
        if(hbmsave=0) then
        begin
            DeleteDC(memdc);
            exit(false);
        end;
        retval := BitBlt(dc, rc^.left, rc^.top, cx, cy, memdc, 0, 0, SRCCOPY);
        SelectObject(memdc, hbmsave);
        DeleteDC(memdc);
        exit(retval);
        end;

    DST_COMPLEX:
        if (func<>nil) then
        begin
            { DRAWSTATEPROC assumes that it draws at the center of coordinates  }
            //OffsetViewportOrgEx(dc, rc^.left, rc^.top, nil);
            bRet := func(dc, lp, wp, cx, cy);
            { Restore origin }
            //OffsetViewportOrgEx(dc, -rc^.left, -rc^.top, nil);
            exit(bRet);
        end else
            exit(false);
    end;
    exit(false);
end;



function DrawState(dc:HDC ; hbr : HBRUSH ; func: DRAWSTATEPROC ; lp:LPARAM; wp:WPARAM;x,y,cx,cy:integer;flags:UINT) : boolean;
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
retval,tmp : boolean;
s:SIZE;
//ici:^CURSORICONINFO;
bm:BITMAP;
fg, bg : COLORREF;
begin
    hbrtmp := 0;
    dtflags := DT_NOCLIP;
    opcode := flags and $f;
    len := wp;

    if ((opcode = DST_TEXT) or (opcode = DST_PREFIXTEXT)) and (len=0) then   { The string is '\0' terminated }
       len := length(widestring(PWideChar(lp)));

    { Find out what size the image has if not given by caller }
    if(cx=0) or (cy=0) then
    begin
        case opcode of
          DST_TEXT,DST_PREFIXTEXT:
            begin
              retval := GetTextExtentPoint32(dc, pwidechar(lp), len, @s);
              if(retval=false) then exit(false);
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
            if (GetObject(lp, sizeof(bm), @bm)=0) then exit(false);
            s.cx := bm.bmWidth;
            s.cy := bm.bmHeight;
            end;

          DST_COMPLEX: {/* cx and cy must be set in this mode */}
            exit(false);
        end;

        if(cx=0) then cx := s.cx;
        if(cy=0) then cy := s.cy;
    end;

    rc.left   := x;
    rc.top    := y;
    rc.right  := x + cx;
    rc.bottom := y + cy;

    if (flags and DSS_RIGHT<>0) then   { This one is not documented in the win32.hlp file }
        dtflags := dtflags or DT_RIGHT;
    if (opcode = DST_TEXT) then
        dtflags := dtflags or DT_NOPREFIX;

    { For DSS_NORMAL we just jam in the image and return }
    if (flags and $7ff0) = DSS_NORMAL then
        exit(DrawStateJam(dc, opcode, func, lp, len, @rc, dtflags));

    { For all other states we need to convert the image to B/W in a local bitmap
      before it is displayed }
    fg := SetTextColor(dc, RGB(0, 0, 0));
    bg := SetBkColor(dc, RGB(255, 255, 255));
    hbm := 0;
    hbmsave := 0;
    memdc := 0;
    hbsave := 0;
    retval := false; { assume failure }

    { From here on we must use "goto cleanup" when something goes wrong }
    hbm := CreateBitmap(cx, cy, 1, 1, nil);
    if(hbm=0) then goto cleanup;
    memdc   := CreateCompatibleDC(dc);
    if(memdc=0) then goto cleanup;
    hbmsave := SelectObject(memdc, hbm);
    if(hbmsave=0) then goto cleanup;
    rc.top := 0;
    rc.left := 0;
    rc.right := cx;
    rc.bottom := cy;
    if (FillRect(memdc, rc, GetStockObject(WHITE_BRUSH))=0) then goto cleanup;
    SetBkColor(memdc, RGB(255, 255, 255));
    SetTextColor(memdc, RGB(0, 0, 0));
    hfsave  := SelectObject(memdc, GetCurrentObject(dc, OBJ_FONT));

    { DST_COMPLEX may draw text as well,
      so we must be sure that correct font is selected }
    if(hfsave=0) and (opcode <= DST_PREFIXTEXT) then goto cleanup;
    tmp := DrawStateJam(memdc, opcode, func, lp, len, @rc, dtflags);

    if (hfsave<>0) then SelectObject(memdc, hfsave);
    if(tmp=false) then
      goto cleanup;

    { This state cause the image to be dithered }
    if (flags and DSS_UNION<>0) then
    begin
        h55AABitmap := CreateBitmap( 8, 8, 1, 1, @wPattern55AA );
        SYSCOLOR_55AABrush := CreatePatternBrush( h55AABitmap );
        hbsave := SelectObject(memdc, SYSCOLOR_55AABrush);
        if(hbsave=0) then goto cleanup;
        tmp := PatBlt(memdc, 0, 0, cx, cy, $00FA0089);
        SelectObject(memdc, hbsave);
        if (tmp=false) then goto cleanup;
    end;

    if (flags and DSS_DISABLED<>0) then
       hbrtmp := CreateSolidBrush(GetSysColor(COLOR_3DHILIGHT))
    else if (flags and DSS_DEFAULT<>0) then
       hbrtmp := CreateSolidBrush(GetSysColor(COLOR_3DSHADOW));

    { Draw light or dark shadow }
    if (flags and (DSS_DISABLED or DSS_DEFAULT)<>0) then
    begin
       if(hbrtmp=0) then goto cleanup;
       hbsave := SelectObject(dc, hbrtmp);
       if (hbsave=0) then goto cleanup;
       if(BitBlt(dc, x+1, y+1, cx, cy, memdc, 0, 0, $00B8074A)=false) then goto cleanup;
       SelectObject(dc, hbsave);
       DeleteObject(hbrtmp);
       hbrtmp := 0;
    end;

    if (flags and DSS_DISABLED<>0) then
    begin
       hbrtmp := CreateSolidBrush(GetSysColor(COLOR_3DSHADOW));
       hbr := hbrtmp;
       if(hbrtmp=0) then goto cleanup;
    end
    else if (hbr=0) then
    begin
       hbr := GetStockObject(BLACK_BRUSH);
    end;

    hbsave := SelectObject(dc, hbr);

    if(BitBlt(dc, x, y, cx, cy, memdc, 0, 0, $00B8074A)=false) then goto cleanup;

    retval := TRUE; { We succeeded }

cleanup:
    SetTextColor(dc, fg);
    SetBkColor(dc, bg);

    if(hbsave<>0)  then SelectObject(dc, hbsave);
    if(hbmsave<>0) then SelectObject(memdc, hbmsave);
    if(hbrtmp<>0)  then DeleteObject(hbrtmp);
    if(hbm<>0)     then DeleteObject(hbm);
    if(memdc<>0)   then DeleteDC(memdc);

    result:=retval;
end;




initialization
  New(ThePropertyLists);
  ThePropertyLists^.WindowInfo := 0;
  ThePropertyLists^.WindowHWND := 0;
  ThePropertyLists^.NextPropertyListWindows := nil;

finalization

//  FreePropList;//roozbeh:best place to call?!
end.

