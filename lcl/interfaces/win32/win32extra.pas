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
  InterfaceBase, Classes, LCLType, Windows, GraphType, SysUtils;

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

  TNMLVCustomDraw = record
    nmcd         : TNMCustomDraw;
    clrText      : COLORREF;
    clrTextBk    : COLORREF;
    iSubItem     : LongInt;
  end;
  PNMLVCustomDraw=^TNMLVCustomDraw;

  tagCOMBOBOXINFO = record
    cbSize: DWORD;
    rcItem: TRect;
    rcButton: TRect;
    stateButton: DWORD;
    hwndCombo: HWND;
    hwndItem: HWND;
    hwndList: HWND;
  end;
  TComboboxInfo = tagCOMBOBOXINFO;
  PComboboxInfo = ^TComboboxInfo;
  
  tagIMAGELISTDRAWPARAMS = record
    cbSize: DWORD;
    himl: HIMAGELIST;
    i: integer;
    hdcDst: HDC;
    x: integer;
    y: integer;
    cx: integer;
    cy: integer;
    xBitmap: integer;
    yBitmap: integer;
    rgbBk: COLORREF;
    rgbFg: COLORREF;
    fStyle: UINT;
    dwRop: DWORD;
    fState: DWORD;
    Frame: DWORD;
    crEffect: DWORD;
  end;
  TImageListDrawParams = tagIMAGELISTDRAWPARAMS;
  PImageListDrawParams = ^TImageListDrawParams;
  
  {$ifdef ver2_2_0}
  _browseinfoW = record
    hwndOwner : HWND;
    pidlRoot : LPCITEMIDLIST;
    pszDisplayName : LPWSTR;    { Return display name of item selected. }
    lpszTitle : LPCWSTR;        { text to go in the banner over the tree. }
    ulFlags : UINT;             { Flags that control the return stuff }
    lpfn : BFFCALLBACK;
    lParam : LPARAM;            { extra info that's passed back in callbacks }
    iImage : longint;           { output var: where to return the Image index. }
  end;
  BROWSEINFOW = _browseinfoW;
  PBROWSEINFOW = ^BROWSEINFOW;
  PPBROWSEINFOW = ^PBROWSEINFOW;
  LPBROWSEINFOW = PbrowseinfoW;
  PLPBROWSEINFOW = ^LPBROWSEINFOW;
  TBROWSEINFOW = BROWSEINFOW;
  {$endif}



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
  
  { month picker, date picker, time picker, updown }
  ICC_DATE_CLASSES       = $00000100;


 // OEM Resource Ordinal Numbers
  OBM_CLOSED         = System.MakeIntResource(32731);
  OBM_TRTYPE         = System.MakeIntResource(32732);
  OBM_LFARROWI       = System.MakeIntResource(32734);
  OBM_RGARROWI       = System.MakeIntResource(32735);
  OBM_DNARROWI       = System.MakeIntResource(32736);
  OBM_UPARROWI       = System.MakeIntResource(32737);
  OBM_COMBO          = System.MakeIntResource(32738);
  OBM_MNARROW        = System.MakeIntResource(32739);
  OBM_LFARROWD       = System.MakeIntResource(32740);
  OBM_RGARROWD       = System.MakeIntResource(32741);
  OBM_DNARROWD       = System.MakeIntResource(32742);
  OBM_UPARROWD       = System.MakeIntResource(32743);
  OBM_RESTORED       = System.MakeIntResource(32744);
  OBM_ZOOMD          = System.MakeIntResource(32745);
  OBM_REDUCED        = System.MakeIntResource(32746);
  OBM_RESTORE        = System.MakeIntResource(32747);
  OBM_ZOOM           = System.MakeIntResource(32748);
  OBM_REDUCE         = System.MakeIntResource(32749);
  OBM_LFARROW        = System.MakeIntResource(32750);
  OBM_RGARROW        = System.MakeIntResource(32751);
  OBM_DNARROW        = System.MakeIntResource(32752);
  OBM_UPARROW        = System.MakeIntResource(32753);
  OBM_CLOSE          = System.MakeIntResource(32754);
  OBM_OLD_RESTORE    = System.MakeIntResource(32755);
  OBM_OLD_ZOOM       = System.MakeIntResource(32756);
  OBM_OLD_REDUCE     = System.MakeIntResource(32757);
  OBM_BTNCORNERS     = System.MakeIntResource(32758);
  OBM_CHECKBOXES     = System.MakeIntResource(32759);
  OBM_CHECK          = System.MakeIntResource(32760);
  OBM_BTSIZE         = System.MakeIntResource(32761);
  OBM_OLD_LFARROW    = System.MakeIntResource(32762);
  OBM_OLD_RGARROW    = System.MakeIntResource(32763);
  OBM_OLD_DNARROW    = System.MakeIntResource(32764);
  OBM_OLD_UPARROW    = System.MakeIntResource(32765);
  OBM_SIZE           = System.MakeIntResource(32766);
  OBM_OLD_CLOSE      = System.MakeIntResource(32767);

  OCR_NORMAL         = System.MakeIntResource(32512);
  OCR_IBEAM          = System.MakeIntResource(32513);
  OCR_WAIT           = System.MakeIntResource(32514);
  OCR_CROSS          = System.MakeIntResource(32515);
  OCR_UP             = System.MakeIntResource(32516);
  OCR_SIZE           = System.MakeIntResource(32640);
  OCR_ICON           = System.MakeIntResource(32641);
  OCR_SIZENWSE       = System.MakeIntResource(32642);
  OCR_SIZENESW       = System.MakeIntResource(32643);
  OCR_SIZEWE         = System.MakeIntResource(32644);
  OCR_SIZENS         = System.MakeIntResource(32645);
  OCR_SIZEALL        = System.MakeIntResource(32646);
  OCR_ICOCUR         = System.MakeIntResource(32647);
  OCR_NO             = System.MakeIntResource(32648);
  OCR_HAND           = System.MakeIntResource(32649);
  OCR_APPSTARTING    = System.MakeIntResource(32650);
  OCR_HELP           = System.MakeIntResource(32651);

  OIC_SAMPLE         = System.MakeIntResource(32512);
  OIC_HAND           = System.MakeIntResource(32513);
  OIC_QUES           = System.MakeIntResource(32514);
  OIC_BANG           = System.MakeIntResource(32515);
  OIC_NOTE           = System.MakeIntResource(32516);
  OIC_WINLOGO        = System.MakeIntResource(32517);
  OIC_WARNING        = OIC_BANG;
  OIC_ERROR          = OIC_HAND;
  OIC_INFORMATION    = OIC_NOTE;

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
  BIF_NEWDIALOGSTYLE = $40;
    //Version 5.0. Use the new user interface. Setting this flag provides the
    //user with a larger dialog box that can be resized. The dialog box has
    //several new capabilities, including: drag-and-drop capability within the
    //dialog box, reordering, shortcut menus, new folders, delete, and other
    //shortcut menu commands. To use this flag, you must call OleInitialize or
    //CoInitialize before calling SHBrowseForFolder.

  {$ifdef ver2_2_0}
  BFFM_INITIALIZED = 1;
  BFFM_SELCHANGED = 2;

  BFFM_SETSELECTION = WM_USER + 102;
  BFFM_SETSELECTIONW = (WM_USER + 103);
  {$endif}

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

// for SetWindowPos

SWP_DEFERERASE     = $2000;
SWP_ASYNCWINDOWPOS = $4000;
SWP_STATECHANGED   = $8000; // used by windows but not documented (used even in wine)

// addition XP messages
WM_THEMECHANGED = $31A;
  
// missing imagelist macros and constants

const
// image list copy flags
  ILCF_MOVE = $00000000;
  ILCF_SWAP = $00000001;
// image list states
  ILS_NORMAL   = $00000000;
  ILS_GLOW     = $00000001;
  ILS_SHADOW   = $00000002;
  ILS_SATURATE = $00000004;
  ILS_ALPHA    = $00000008;


function ImageList_Copy(himlDst: HIMAGELIST; iDst: longint; himlSrc: HIMAGELIST; Src: longint; uFlags: UINT): BOOL; stdcall; external 'comctl32';
// only with IExplorer 3.0 or later
function ImageList_DrawIndirect(pimldp: PIMAGELISTDRAWPARAMS): BOOL; stdcall; external 'comctl32';

{ Win32 API functions not included in windows.pp }
{ Get the ancestor at level Flag of window HWnd }
function GetAncestor(Const HWnd: HWND; Const Flag: UINT): HWND; StdCall; External 'user32';
{ Get information about combo box hwndCombo and place in pcbi }
function GetRandomRgn(aHDC: HDC; aHRGN: HRGN; iNum: longint): longint; stdcall; external 'gdi32';

{ Functions allocate and dealocate memory used in ole32 functions
  e.g. BrowseForFolder dialog functions}
function CoTaskMemAlloc(cb : ULONG) : PVOID; stdcall; external 'ole32.dll' name 'CoTaskMemAlloc';
procedure CoTaskMemFree(pv : PVOID); stdcall; external 'ole32.dll' name 'CoTaskMemFree';

{$ifdef ver2_2_0}
function SHGetPathFromIDListW(pidl:LPCITEMIDLIST; pszPath:LPWStr):BOOL;StdCall;external 'shell32' name 'SHGetPathFromIDListW';
function SHBrowseForFolderW(lpbi:LPBROWSEINFOW):LPITEMIDLIST;StdCall;external 'shell32' name 'SHBrowseForFolderW';
{$endif}

const
  // BlendOp flags
  AC_SRC_OVER = $00;
  // AlphaFormat flags
  AC_SRC_ALPHA = $01;

// AlphaBlend is only defined for win98&2k and up 
// load dynamic and use ownfunction if not defined
var
  AlphaBlend: function(hdcDest: HDC; nXOriginDest, nYOriginDest, nWidthDest, nHeightDest: Integer; hdcSrc: HDC; nXOriginSrc, nYOriginSrc, nWidthSrc, nHeightSrc: Integer; blendFunction: TBlendFunction): BOOL; stdcall;
  GetComboBoxInfo: function(hwndCombo: HWND; pcbi: PComboboxInfo): BOOL; stdcall;

const
  // ComCtlVersions
  ComCtlVersionIE3   = $00040046;
  ComCtlVersionIE4   = $00040047;
  ComCtlVersionIE401 = $00040048;
  ComCtlVersionIE5   = $00050050;
  ComCtlVersionIE501 = $00050051;
  ComCtlVersionIE6   = $00060000;


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

function GetFileVersion(FileName: string): dword;

{$endif}

implementation

uses
  Win32Proc;

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

function GetFileVersion(FileName: string): dword;
var
  buf: pointer;
  lenBuf: dword;
  fixedInfo: ^VS_FIXEDFILEINFO;
begin
  Result := $FFFFFFFF;
  lenBuf := GetFileVersionInfoSize(PChar(FileName), lenBuf);
  if lenBuf > 0 then
  begin
    GetMem(buf, lenBuf);
    if GetFileVersionInfo(PChar(FileName), 0, lenBuf, buf) then
    begin
      VerQueryValue(buf, '\', pointer(fixedInfo), lenBuf);
      Result := fixedInfo^.dwFileVersionMS;
    end;
    FreeMem(buf);
  end;
end;

{$endif VER2_0}

function _AlphaBlend(hdcDest: HDC; nXOriginDest, nYOriginDest, nWidthDest, nHeightDest: Integer; hdcSrc: HDC; nXOriginSrc, nYOriginSrc, nWidthSrc, nHeightSrc: Integer; blendFunction: TBlendFunction): BOOL; stdcall;
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
  SrcSize: PtrUInt;
  SrcPixelBytes, DstPixelBytes: Byte;
  SrcRowStride, DstRowStride: Integer;
  SrcLineOrder: TRawImageLineOrder;

  X, Y: Integer;
  SrcRGBA, TmpRGBA, DstRGBA: PRGBAQuad;
  SrcAlpha: PByte;
  NotAlpha: Byte;
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

  // get source info, atleast bitmap, if possible also section
  if GetObjectType(hdcSrc) <> OBJ_MEMDC then Exit(False);
  SrcBmp := GetCurrentObject(hdcSrc, OBJ_BITMAP);
  if GetObject(SrcBmp, SizeOf(SrcSection), @SrcSection) = 0 then Exit(False);
  if nXOriginSrc + nWidthSrc > SrcSection.dsBm.bmWidth then Exit(False);
  if nYOriginSrc + nHeightSrc > SrcSection.dsBm.bmHeight then Exit(False);

  if (blendFunction.AlphaFormat = AC_SRC_ALPHA) and (SrcSection.dsBm.bmBitsPixel <> 32) then Exit(False); // invalid

  // get destination info, atleast bitmap, if possible also section
  if WindowsVersion in [wv95, wv98]
  then begin
    // under windows 98 GetObjectType() sometimes produce AV inside and
    // as result our debugger stopes and show exception
    // lazarus is not alone application with such problem under windows 98
    // here is workaround for windows 9x
    DstBmp := GetCurrentObject(hdcDest, OBJ_BITMAP);
    DstSection.dsBm.bmBits := nil;
    if (DstBmp <> 0)
    and ((GetObject(DstBmp, SizeOf(DstSection), @DstSection) < SizeOf(TDIBSection)) or (DstSection.dsBm.bmBits = nil))
    then DstBmp := 0;
  end
  else begin
    if GetObjectType(hdcDest) = OBJ_MEMDC
    then DstBmp := GetCurrentObject(hdcDest, OBJ_BITMAP)
    else DstBmp := 0;
    if (DstBmp <> 0) and (GetObject(DstBmp, SizeOf(DstSection), @DstSection) = 0)
    then DstBmp := 0;
  end;

  if (DstBmp = 0)
  then begin
    // GetCurrentObject can only be used on memory devices,
    // so fill in some values manually
    DstSection.dsBm.bmWidth := GetDeviceCaps(hdcDest, HORZRES);
    DstSection.dsBm.bmHeight := GetDeviceCaps(hdcDest, VERTRES);
    DstSection.dsBm.bmBitsPixel := GetDeviceCaps(hdcDest, BITSPIXEL);
    DstSection.dsBm.bmBits := nil;
  end;

  // docs doesn't require dest retangle inside dest.
  // however if dest rect is outside the destination, we're done here
  if nXOriginDest + nWidthDest < 0 then Exit(True);
  if nYOriginDest + nHeightDest < 0 then Exit(True);
  if nXOriginDest >= DstSection.dsBm.bmWidth then Exit(True);
  if nYOriginDest >= DstSection.dsBm.bmHeight then Exit(True);
  
  // get lineorder of source so we use the right direction
  SrcLineOrder := GetBitmapOrder(SrcSection.dsBm, SrcBmp);

  // setup info shared by alpha, source and destination bytes
  FillChar(Info, sizeof(Info), 0);
  Info.Header.biSize := sizeof(Windows.TBitmapInfoHeader);
  Info.Header.biWidth := nWidthDest;
  if SrcLineOrder = riloBottomToTop
  then Info.Header.biHeight := nHeightDest
  else Info.Header.biHeight := -nHeightDest;
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
      R := Classes.Rect(nXOriginSrc, nYOriginSrc, nXOriginSrc + nWidthSrc, nYOriginSrc + nHeightSrc);
      if not GetBitmapBytes(SrcSection.dsBm, SrcBmp, R, rileDWordBoundary, SrcLineOrder, SrcBytesPtr, SrcSize) then Exit(False);

      // set info to source size
      Info.Header.biWidth := nWidthSrc;
      if SrcLineOrder = riloBottomToTop
      then Info.Header.biHeight := nHeightSrc
      else Info.Header.biHeight := -nHeightSrc;
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
      if SrcLineOrder = riloBottomToTop
      then Info.Header.biHeight := nHeightDest
      else Info.Header.biHeight := -nHeightDest;
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
      R := Classes.Rect(nXOriginSrc, nYOriginSrc, nXOriginSrc + nWidthSrc, nYOriginSrc + nHeightSrc);
      if not GetBitmapBytes(SrcSection.dsBm, SrcBmp, R, rileDWordBoundary, SrcLineOrder, SrcBytesPtr, SrcSize) then Exit;
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
  Inc(SrcLinePtr, nXOriginSrc * SrcPixelBytes + nYOriginSrc * SrcRowStride);
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
            NotAlpha := not SrcAlpha^;
            DstRGBA^.Red   := SrcRgba^.Red   + (DstRGBA^.Red   * NotAlpha) div 255;
            DstRGBA^.Green := SrcRgba^.Green + (DstRGBA^.Green * NotAlpha) div 255;
            DstRGBA^.Blue  := SrcRgba^.Blue  + (DstRGBA^.Blue  * NotAlpha) div 255;
            if DstPixelBytes = 4
            then DstRGBA^.Alpha := SrcAlpha^ + (DstRGBA^.Alpha * NotAlpha) div 255;
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
            NotAlpha := not SrcAlpha^;
            DstRGBA^.Red   := (SrcRgba^.Red   * SCA + DstRGBA^.Red   * NotAlpha) div 255;
            DstRGBA^.Green := (SrcRgba^.Green * SCA + DstRGBA^.Green * NotAlpha) div 255;
            DstRGBA^.Blue  := (SrcRgba^.Blue  * SCA + DstRGBA^.Blue  * NotAlpha) div 255;
            if DstPixelBytes = 4
            then DstRGBA^.Alpha := (SrcAlpha^ * SCA + DstRGBA^.Alpha * NotAlpha) div 255;
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
    // no source alpha
    NotAlpha := not SCA;
    for y := 1 to nHeightDest do
    begin
      SrcRGBA := Pointer(SrcLinePtr);
      if AlphaBytesPtr = nil
      then SrcAlpha := @SrcRGBA^.Alpha;
      DstRGBA := Pointer(DstLinePtr);
      for x := 1 to nWidthDest do
      begin
        DstRGBA^.Red :=   (SrcRGBA^.Red   * SCA + DstRGBA^.Red   * NotAlpha) div 255;
        DstRGBA^.Green := (SrcRGBA^.Green * SCA + DstRGBA^.Green * NotAlpha) div 255;
        DstRGBA^.Blue :=  (SrcRGBA^.Blue  * SCA + DstRGBA^.Blue  * NotAlpha) div 255;
        if (DstPixelBytes = 4) and (SrcPixelBytes = 4)
        then DstRGBA^.Alpha := (SrcAlpha^ * SCA + DstRGBA^.Alpha * NotAlpha) div 255;
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

// win98 only supports dibsections, so if not a dib section,
// we draw ourselves
{var
  AlphaBlend98: function(hdcDest: HDC; nXOriginDest, nYOriginDest, nWidthDest, nHeightDest: Integer; hdcSrc: HDC; nXOriginSrc, nYOriginSrc, nWidthSrc, nHeightSrc: Integer; blendFunction: TBlendFunction): BOOL; stdcall;
}

function _AlphaBlend98(hdcDest: HDC; nXOriginDest, nYOriginDest, nWidthDest, nHeightDest: Integer; hdcSrc: HDC; nXOriginSrc, nYOriginSrc, nWidthSrc, nHeightSrc: Integer; blendFunction: TBlendFunction): BOOL; stdcall;
begin
  // we can check the bitmaptypes here and call AlphaBlend98, but for now, just call own implementation
  Result := _AlphaBlend(hdcDest, nXOriginDest, nYOriginDest, nWidthDest, nHeightDest, hdcSrc, nXOriginSrc, nYOriginSrc, nWidthSrc, nHeightSrc, blendFunction);
end;

function _GetComboboxInfo(hwndCombo: HWND; pcbi: PComboboxInfo): BOOL; stdcall;
begin
  Result := (pcbi <> nil) and (pcbi^.cbSize = SizeOf(TComboboxInfo));
  if Result then
  begin
    pcbi^.hwndCombo := hwndCombo;
    if (GetWindowLong(hwndCombo, GWL_STYLE) and CBS_SIMPLE) <> 0 then
    begin
      pcbi^.hwndList := GetTopWindow(hwndCombo);
      pcbi^.hwndItem := GetWindow(pcbi^.hwndList, GW_HWNDNEXT);
    end
    else
    begin
      pcbi^.hwndItem := GetTopWindow(hwndCombo);
      pcbi^.hwndList := 0;
    end;
  end;
end;


const 
  msimg32lib = 'msimg32.dll';
  user32lib = 'user32.dll';

var
  msimg32handle: THandle = 0;
  user32handle: THandle = 0;

procedure Initialize;
var
  p: Pointer;
begin
  GetComboBoxInfo := nil;

  AlphaBlend := @_AlphaBlend;

  msimg32handle := LoadLibrary(msimg32lib);
  if msimg32handle <> 0
  then begin 
    p := GetProcAddress(msimg32handle, 'AlphaBlend');
    if p <> nil
    then begin
      // Detect win98 since aplhablend doesn't support all bitmap types
      if WindowsVersion = wv98
      then begin
        // windows 98
        // Pointer(AlphaBlend98) := p;
        AlphaBlend := @_AlphaBlend98;
      end
      else begin
        // other
        Pointer(AlphaBlend) := p;
      end;
    end;
  end;
  
  user32handle := LoadLibrary(user32lib);
  if user32handle <> 0 then
  begin
    p := GetProcAddress(user32handle, 'GetComboBoxInfo');
    if p <> nil then
      Pointer(GetComboboxInfo) := p
    else
      Pointer(GetComboboxInfo) := @_GetComboboxInfo;
  end;
end;

procedure Finalize;
begin
  AlphaBlend := @_AlphaBlend;
  GetComboboxInfo := nil;

  if msimg32handle <> 0
  then FreeLibrary(msimg32handle);
  msimg32handle := 0;
  
  if user32handle <> 0 then
    FreeLibrary(user32handle);
  user32handle := 0;
end;

initialization
  Initialize;

finalization
  Finalize;

end.
