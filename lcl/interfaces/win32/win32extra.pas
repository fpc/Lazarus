{
  Extra Win32 code that's not in the RTL.
  Copyright (C) 2001, 2002 Keith Bowes. 
  Modified by Marc Weustink

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
}

unit Win32Extra;

{$mode objfpc}{$H+}

{$IFDEF TRACE}
  {$ASSERTIONS ON}
{$ENDIF}

{$PACKRECORDS C}
{$SMARTLINK ON}

interface

uses 
  InterfaceBase, Classes, Windows, GraphType;

{ Win32 API records not included in windows.pp }
type
  TNMCustomDraw = record
    hdr        : NMHDR;
    dwDrawStage: DWORD;
    hdc        : HDC;
    rc         : TRECT;
    dwItemSpec : DWORD;
    uItemState : UINT;
    lItemlParam: LPARAM;
  end;
  PNMCustomDraw=^TNMCustomDraw;

  TNMLVCustomDraw = Record
                   hdr          : NMHDR;
                   dwDrawStage  : DWORD;
                   hdc          : HDC;
                   rc           : TRECT;
                   dwItemSpec   : DWORD;
                   uItemState   : UINT;
                   lItemlParam  : longint;
		   clrText,clrTextBk:COLORREF;
                   iSubItem     :longint;
                END;
  PNMLVCustomDraw=^TNMLVCustomDraw;

{ Win32 API constants not included in windows.pp }
const
  { Recommended modal-dialog style }
  DSC_MODAL = WS_POPUP Or WS_SYSMENU Or WS_CAPTION Or DS_MODALFRAME;
  { Recommended modeless-dialog style }
  DSC_MODELESS = WS_POPUP Or WS_CAPTION Or WS_BORDER Or WS_SYSMENU;
  { The window's direct parent window }
  GA_PARENT = 1;
  { The window's root window }
  GA_ROOT = 2;
  { The window's owner }
  GA_ROOTOWNER = 3;
  { Application starting cursor }
  IDC_APPSTARTING = 32650;
  { Hand cursor }
  IDC_HAND = 32649;
  { Get the progress bar range }
  PBM_GETRANGE = 1031;
  { Smooth progrss bar }
  PBS_SMOOTH = 1;
  { Vertical progress bar }
  PBS_VERTICAL = 4;
  { Left-to-right reading text }
  WS_EX_LTRLEADING = 0;

  { Tab Control Styles}
  TCS_RIGHT = $0002;
  TCS_BOTTOM = $0002;
  TCS_VERTICAL = $0080;
  TCS_MULTILINE = $0200;
  
  { Open File Dialog}
  OFN_ENABLESIZING = $800000;
  
  { BrowseForFolder dialog}
  BIF_RETURNONLYFSDIRS = 1;

  BFFM_INITIALIZED = 1;
  BFFM_SELCHANGED = 2;

  BFFM_SETSELECTION = WM_USER + 102;
  
  {SpinEdit 32 bit messages}
  UDM_GETPOS32 = 1138;
  UDM_GETRANGE32 = 1136;
  UDM_SETPOS32 = 1137;
  UDM_SETRANGE32 = 1135;

  // Listview constants
  LVCFMT_JUSTIFYMASK = LVCFMT_LEFT or LVCFMT_RIGHT or LVCFMT_CENTER;
  LVCFMT_IMAGE            = $0800;
  LVCFMT_BITMAP_ON_RIGHT  = $1000;
  LVCFMT_COL_HAS_IMAGES   = $8000;

  LVCF_IMAGE = $0010;
  LVCF_ORDER = $0020;

  LVM_FIRST                    = $1000;
  LVM_GETHEADER                = LVM_FIRST + 31;
  LVM_SETEXTENDEDLISTVIEWSTYLE = LVM_FIRST + 54;
  LVM_GETEXTENDEDLISTVIEWSTYLE = LVM_FIRST + 55;
  LVM_GETSUBITEMRECT           = LVM_FIRST + 56;
  LVM_SETHOVERTIME             = LVM_FIRST + 71;
  LVM_GETHOVERTIME             = LVM_FIRST + 72;

  LVS_TYPEMASK = LVS_ICON  or LVS_SMALLICON or LVS_LIST or LVS_REPORT;

  // Comctl32 version:
  // 4.70
  LVS_EX_GRIDLINES        = $00000001;
  LVS_EX_SUBITEMIMAGES    = $00000002;
  LVS_EX_CHECKBOXES       = $00000004;
  LVS_EX_TRACKSELECT      = $00000008;
  LVS_EX_HEADERDRAGDROP   = $00000010;
  LVS_EX_FULLROWSELECT    = $00000020;
  LVS_EX_ONECLICKACTIVATE = $00000040;
  LVS_EX_TWOCLICKACTIVATE = $00000080;
  // 4.71
  LVS_EX_FLATSB           = $00000100;
  LVS_EX_REGIONAL         = $00000200;
  LVS_EX_INFOTIP          = $00000400;
  LVS_EX_UNDERLINEHOT     = $00000800;
  LVS_EX_UNDERLINECOLD    = $00001000;
  LVS_EX_MULTIWORKAREAS   = $00002000;
  // 5.80
  LVS_EX_LABELTIP         = $00004000;
  // 4.71
  LVS_EX_BORDERSELECT     = $00008000;
  // 6
  LVS_EX_DOUBLEBUFFER     = $00010000;   // TODO: investigate
                                         // this may be a valid (ex) style message for other controls as well
                                         // atleast the same value is used for controls on the .net framework
                                         // coincidence ??
  LVS_EX_HIDELABELS       = $00020000;
  LVS_EX_SINGLEROW        = $00040000;
  LVS_EX_SNAPTOGRID       = $00080000;
  LVS_EX_SIMPLESELECT     = $00100000;
  
  //state information for common control items (used for listview)
  CDIS_SELECTED           = $001;
  CDIS_GRAYED             = $002;
  CDIS_DISABLED           = $004;
  CDIS_CHECKED            = $008;
  CDIS_FOCUS              = $010;
  CDIS_DEFAULT            = $020;
  CDIS_HOT                = $040;
  CDIS_MARKED             = $080;
  CDIS_INDETERMINATE      = $100;
  
  //custom draw event stage information
  CDDS_PREPAINT      = $00001;
  CDDS_POSTPAINT     = $00002;
  CDDS_PREERASE      = $00003;
  CDDS_POSTERASE     = $00004;
  
  CDDS_ITEM          = $10000;
  CDDS_ITEMPREPAINT  = $10001;
  CDDS_ITEMPOSTPAINT = $10002;
  CDDS_ITEMPREERASE  = $10003;
  CDDS_ITEMPOSTERASE = $10004;

  CDDS_SUBITEM       = $20000;
  
  //values returned by an custom draw event
  CDRF_DODEFAULT         = $00;
  CDRF_SKIPDEFAULT       = $04;
  CDRF_NOTIFYPOSTPAINT   = $10;
  CDRF_NOTIFYITEMDRAW    = $20; 
  CDRF_NOTIFYSUBITEMDRAW = $20; // flags are the same, we can distinguish by context
  CDRF_NOTIFYPOSTERASE   = $40;
  CDRF_NOTIFYITEMERASE   = $80; 

  // trackbar customdraw
  TBCD_TICS          = $01;
  TBCD_THUMB         = $02;
  TBCD_CHANNEL       = $03;
  
// for calendar control
  MCN_FIRST               = (0-750);          // monthcal
  MCN_SELCHANGE           = (MCN_FIRST + 1);

// for GetRandomRgn
  SYSRGN                  = 4;

{not used anymore
// missing listview macros
function ListView_GetHeader(hwndLV: HWND): HWND;
function ListView_GetExtendedListViewStyle(hwndLV: HWND): DWORD;
function ListView_SetExtendedListViewStyle(hwndLV: HWND; dw: DWORD): BOOL;
function ListView_GetHoverTime(hwndLV: HWND): DWORD;
function ListView_SetHoverTime(hwndLV: HWND; dwHoverTimeMs: DWORD): DWORD;
}

// missing imagelist macros and constants

const
  ILCF_MOVE = $00000000;
  ILCF_SWAP = $00000001;

function ImageList_Copy(himlDst: HIMAGELIST; iDst: longint; himlSrc: HIMAGELIST; Src: longint; uFlags: UINT): BOOL; stdcall; external 'comctl32';


{ Win32 API functions not included in windows.pp }
{ Get the ancestor at level Flag of window HWnd }
function GetAncestor(Const HWnd: HWND; Const Flag: UINT): HWND; StdCall; External 'user32';
{ Get information about combo box hwndCombo and place in pcbi }
function GetRandomRgn(aHDC: HDC; aHRGN: HRGN; iNum: longint): longint; stdcall; external 'gdi32';

{ Functions allocate and dealocate memory used in ole32 functions
  e.g. BrowseForFolder dialog functions}
function CoTaskMemAlloc(cb : ULONG) : PVOID; stdcall; external 'ole32.dll' name 'CoTaskMemAlloc';
procedure CoTaskMemFree(pv : PVOID); stdcall; external 'ole32.dll' name 'CoTaskMemFree';

const
  // BlendOp flags
  AC_SRC_OVER = $00;
  // AlphaFormat flags
  AC_SRC_ALPHA = $01;

// AlphaBlend is only defined for win98&2k and up 
// load dynamic and use ownfunction if not defined
var
  AlphaBlend: function(hdcDest: HDC; nXOriginDest, nYOriginDest, nWidthDest, nHeightDest: Integer; hdcSrc: HDC; nXOriginSrc, nYOriginSrc, nWidthSrc, nHeightSrc: Integer; blendFunction: TBlendFunction): BOOL; stdcall;


{$ifdef VER2_0}
function GET_X_LPARAM(lp : Windows.LParam) : longint;
function GET_Y_LPARAM(lp : Windows.LParam) : longint;

// the declaration in the windows unit doesn't have the FlagsEx field
type
     OPENFILENAME = record
          lStructSize : DWORD;
          hwndOwner : HWND;
          hInstance : HINST;
          lpstrFilter : LPCTSTR;
          lpstrCustomFilter : LPTSTR;
          nMaxCustFilter : DWORD;
          nFilterIndex : DWORD;
          lpstrFile : LPTSTR;
          nMaxFile : DWORD;
          lpstrFileTitle : LPTSTR;
          nMaxFileTitle : DWORD;
          lpstrInitialDir : LPCTSTR;
          lpstrTitle : LPCTSTR;
          Flags : DWORD;
          nFileOffset : WORD;
          nFileExtension : WORD;
          lpstrDefExt : LPCTSTR;
          lCustData : LPARAM;
          lpfnHook : LPOFNHOOKPROC;
          lpTemplateName : LPCTSTR;
          pvReserved: pointer;
          dwReserved: DWORD;
          FlagsEx: DWORD;
       end;
     LPOPENFILENAME = ^OPENFILENAME;
     TOPENFILENAME = OPENFILENAME;
     POPENFILENAME = ^OPENFILENAME;
     
     OPENFILENAME_NT4 = Windows.OPENFILENAME;

// these functions are declared, because they need to have Win32Extra.LPOPENFILENAME parameter
function GetOpenFileName(_para1:LPOPENFILENAME):WINBOOL; stdcall; external 'comdlg32' name 'GetOpenFileNameA';
function GetSaveFileName(_para1:LPOPENFILENAME):WINBOOL; stdcall; external 'comdlg32' name 'GetSaveFileNameA';
{$endif}


Implementation

Uses SysUtils;

{$PACKRECORDS NORMAL}

{$ifdef VER2_0}
function GET_X_LPARAM(lp : Windows.LParam) : longint;
  begin
    result:=smallint(LOWORD(lp));
  end;


function GET_Y_LPARAM(lp : Windows.LParam) : longint;
  begin
    result:=smallint(HIWORD(lp));
  end;
{$endif VER2_0}

function _AlphaBlend(hdcDest: HDC; nXOriginDest, nYOriginDest, nWidthDest, nHeightDest: Integer; hdcSrc: HDC; nXOriginSrc, nYOriginSrc, nWidthSrc, nHeightSrc: Integer; blendFunction: TBlendFunction): BOOL; stdcall;
var
  R: TRect;
  SrcImage, DstImage: TRawImage;
  SrcDC: HDC;
  bmp: HBITMAP;
  X, Y: Integer;
begin
  if blendFunction.AlphaFormat = 0
  then begin
    Result := True;
    case blendFunction.SourceConstantAlpha of
      0: begin
        Exit;
      end;
      255: begin
        // simple strechblt
        StretchBlt(hdcDest, nXOriginDest, nYOriginDest, nWidthDest, nHeightDest, hdcSrc, nXOriginSrc, nYOriginSrc, nWidthSrc, nHeightSrc, SRCCOPY);
      end;
    end;
  end;

  // TODO: implement someday for win95 and NT

(*
  // get source by replacing it with a dummy
  R := Rect(nXOriginSrc, nYOriginSrc, nXOriginSrc + nWidthSrc, nYOriginSrc + nHeightSrc);
  bmp := CreateBitmap(1,1,1,1,nil);
  bmp := SelectObject(hdcSrc, bmp);
  Result := Widgetset.RawImage_FromBitmap(SrcImage, bmp, 0, R);
  // Restore source
  bmp := SelectObject(hdcSrc, bmp);

  // Get destination
  bmp := SelectObject(hdcDest, bmp);
  // check if destination is 32bit, copy to temp 32bit if not so
  // ...
  // create dstimage
  Result := Widgetset.RawImage_FromBitmap(DstImage, bmp, 0, R);
  // Restore destination
  bmp := SelectObject(hdcDest, bmp);

  // check if resized
  if (nWidthDest <> nWidthSrc) or (nHeightDest <> nHeightSrc)
  then begin
    // separate image and alpha, scale the resulting image and recreate Src Rawimage

  end;
  
  // loop through pixels
  
  // create and bitblt destination bitmap
  
  // cleanup
  
*)
end;


const 
  msimg32lib = 'msimg32.dll';
 
var
  msimg32handle: THandle = 0;

procedure Initialize;
var
  p: Pointer;
begin                
  AlphaBlend := @_AlphaBlend;
  msimg32handle := LoadLibrary(msimg32lib);
  if msimg32handle <> 0
  then begin 
    p := GetProcAddress(msimg32handle, 'AlphaBlend');
    if p <> nil
    then Pointer(AlphaBlend) := p;
  end;
end;

procedure Finalize;
begin
  AlphaBlend := @_AlphaBlend;
  if msimg32handle <> 0
  then FreeLibrary(msimg32handle);
  msimg32handle := 0;
end;

initialization
  Initialize;

finalization
  Finalize;

End.
