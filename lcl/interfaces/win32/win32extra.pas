{
  Extra Win32 code that's not in the RTL.
  Copyright (C) 2001, 2002 Keith Bowes. 
  Modified by Marc Weustink

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

unit Win32Extra;

{$mode objfpc}{$H+}
{$I win32defines.inc}

{$IFDEF TRACE}
  {$ASSERTIONS ON}
{$ENDIF}

{$PACKRECORDS C}
{$SMARTLINK ON}

interface

uses 
  Classes, LCLType, Windows, GraphType, SysUtils, ActiveX, ShlObj, CommCtrl;

type
  tagMENUBARINFO = record
    cbSize: DWORD;
    rcBar: TRect;
    hMenu: HMENU;
    hwndMenu: HWND;
    Flags: DWORD;
  end;
  MENUBARINFO = tagMENUBARINFO;
  PMENUBARINFO = ^tagMENUBARINFO;

  // Window information snapshot
  tagWINDOWINFO = record
    cbSize: DWORD;
    rcWindow: TRect;
    rcClient: TRect;
    dwStyle: DWORD;
    dwExStyle: DWORD;
    dwWindowStatus: DWORD;
    cxWindowBorders: UINT;
    cyWindowBorders: UINT;
    atomWindowType: ATOM;
    wCreatorVersion: WORD;
  end;
  WINDOWINFO = tagWINDOWINFO;
  PWINDOWINFO = ^tagWINDOWINFO;

// ===================== Task Dialog =========================

//Not yet defined in fpc 3.2.2
const
  TD_QUESTION_ICON        = MAKEINTRESOURCEW(Word(32514));  //source: https://www.vbarchiv.net/tipps/tipp_2224-vista-taskdialogindirect.html

// ==================== End TaskDialog =======================

// File dialogs
type
  GETPROPERTYSTOREFLAGS = DWord;
  SIATTRIBFLAGS = DWord;
  CDCONTROLSTATEF = DWord;

  _tagpropertykey = packed record
      fmtid: TGUID;
      pid: DWORD;
  end;
  PROPERTYKEY = _tagpropertykey;
  REFPROPERTYKEY = ^PROPERTYKEY;
  REFPROPVARIANT = ^TPROPVARIANT;
  IEnumShellItems = interface(IUnknown)
    ['{70629033-e363-4a28-a567-0db78006e6d7}']
    function Next(celt: ULONG; out rgelt: IShellItem; var pceltFetched: ULONG): HResult; stdcall;
    function Skip(celt: ULONG): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out ppenum: IEnumShellItems): HResult; stdcall;
  end;

  IShellItemArray = interface(IUnknown)
    ['{b63ea76d-1f85-456f-a19c-48159efa858b}']
    function BindToHandler(pbc: IBindCtx; const bhid: TGUID; const riid: REFIID; out ppvOut): HResult; stdcall;
    function GetPropertyStore(flags: GETPROPERTYSTOREFLAGS; const riid: REFIID; out ppv): HResult; stdcall;
    function GetPropertyDescriptionList(keyType: REFPROPERTYKEY; const riid: REFIID; out ppv): HResult; stdcall;
    function GetAttributes(AttribFlags: SIATTRIBFLAGS; sfgaoMask: SFGAOF; var psfgaoAttribs: SFGAOF): HResult; stdcall;
    function GetCount(var pdwNumItems: DWORD): HResult; stdcall;
    function GetItemAt(dwIndex: DWORD; var ppsi: IShellItem): HResult; stdcall;
    function EnumItems(var ppenumShellItems: IEnumShellItems): HResult; stdcall;
  end;

  IPropertyStore = interface(IUnknown)
    ['{886d8eeb-8cf2-4446-8d02-cdba1dbdcf99}']
    function GetCount(out cProps: DWORD): HResult; stdcall;
    function GetAt(iProp: DWORD; out pkey: PROPERTYKEY): HResult; stdcall;
    function GetValue(key: REFPROPERTYKEY; out pv: PROPVARIANT): HResult; stdcall;
    function SetValue(key: REFPROPERTYKEY; propvar: REFPROPVARIANT): HResult; stdcall;
    function Commit: HResult; stdcall;
  end;

  IPropertyDescriptionList = interface(IUnknown)
    ['{1f9fc1d0-c39b-4b26-817f-011967d3440e}']
    function GetCount(out pcElem: UINT): HResult; stdcall;
    function GetAt(iElem: UINT; const riid: REFIID; out ppv): HResult; stdcall;
  end;

  IFileOperationProgressSink = interface(IUnknown)
    ['{04b0f1a7-9490-44bc-96e1-4296a31252e2}']
    function StartOperations: HResult; stdcall;
    function FinishOperations(hrResult: HResult): HResult; stdcall;
    function PreRenameItem(dwFlags: DWORD; psiItem: IShellItem; pszNewName: LPCWSTR): HResult; stdcall;
    function PostRenameItem(dwFlags: DWORD; psiItem: IShellItem; pszNewName: LPCWSTR; hrRename: HRESULT; psiNewlyCreated: IShellItem): HResult; stdcall;
    function PreMoveItem(dwFlags: DWORD; psiItem: IShellItem; psiDestinationFolder: IShellItem; pszNewName: LPCWSTR): HResult; stdcall;
    function PostMoveItem(dwFlags: DWORD; psiItem: IShellItem; psiDestinationFolder: IShellItem; pszNewName: LPCWSTR; hrMove: HRESULT; psiNewlyCreated: IShellItem): HResult; stdcall;
    function PreCopyItem(dwFlags: DWORD; psiItem: IShellItem; psiDestinationFolder: IShellItem; pszNewName: LPCWSTR): HResult; stdcall;
    function PostCopyItem(dwFlags: DWORD; psiItem: IShellItem; psiDestinationFolder: IShellItem; pszNewName: LPCWSTR; hrCopy: HRESULT; psiNewlyCreated: IShellItem): HResult; stdcall;
    function PreDeleteItem(dwFlags: DWORD; psiItem: IShellItem): HResult; stdcall;
    function PostDeleteItem(dwFlags: DWORD; psiItem: IShellItem; hrDelete: HRESULT; psiNewlyCreated: IShellItem): HResult; stdcall;
    function PreNewItem(dwFlags: DWORD; psiDestinationFolder: IShellItem; pszNewName: LPCWSTR): HResult; stdcall;
    function PostNewItem(dwFlags: DWORD; psiDestinationFolder: IShellItem; pszNewName: LPCWSTR; pszTemplateName: LPCWSTR; dwFileAttributes: DWORD; hrNew: HRESULT; psiNewItem: IShellItem): HResult; stdcall;
    function UpdateProgress(iWorkTotal: UINT; iWorkSoFar: UINT): HResult; stdcall;
    function ResetTimer: HResult; stdcall;
    function PauseTimer: HResult; stdcall;
    function ResumeTimer: HResult; stdcall;
  end;

  IFileDialogCustomize = interface(IUnknown)
    ['{e6fdd21a-163f-4975-9c8c-a69f1ba37034}']
    function EnableOpenDropDown(dwIDCtl: DWORD): HResult; stdcall;
    function AddMenu(dwIDCtl: DWORD; pszLabel: LPCWSTR): HResult; stdcall;
    function AddPushButton(dwIDCtl: DWORD; pszLabel: LPCWSTR): HResult; stdcall;
    function AddComboBox(dwIDCtl: DWORD): HResult; stdcall;
    function AddRadioButtonList(dwIDCtl: DWORD): HResult; stdcall;
    function AddCheckButton(dwIDCtl: DWORD; pszLabel: LPCWSTR; bChecked: BOOL): HResult; stdcall;
    function AddEditBox(dwIDCtl: DWORD; pszText: LPCWSTR): HResult; stdcall;
    function AddSeparator(dwIDCtl: DWORD): HResult; stdcall;
    function AddText(dwIDCtl: DWORD; pszText: LPCWSTR): HResult; stdcall;
    function SetControlLabel(dwIDCtl: DWORD; pszLabel: LPCWSTR): HResult; stdcall;
    function GetControlState(dwIDCtl: DWORD; out pdwState: CDCONTROLSTATEF): HResult; stdcall;
    function SetControlState(dwIDCtl: DWORD; dwState: CDCONTROLSTATEF): HResult; stdcall;
    function GetEditBoxText(dwIDCtl: DWORD; out ppszText: WCHAR): HResult; stdcall;
    function SetEditBoxText(dwIDCtl: DWORD; pszText: LPCWSTR): HResult; stdcall;
    function GetCheckButtonState(dwIDCtl: DWORD; out pbChecked: BOOL): HResult; stdcall;
    function SetCheckButtonState(dwIDCtl: DWORD; bChecked: BOOL): HResult; stdcall;
    function AddControlItem(dwIDCtl: DWORD; dwIDItem: DWORD; pszLabel: LPCWSTR): HResult; stdcall;
    function RemoveControlItem(dwIDCtl: DWORD; dwIDItem: DWORD): HResult; stdcall;
    function RemoveAllControlItems(dwIDCtl: DWORD): HResult; stdcall;
    function GetControlItemState(dwIDCtl: DWORD; dwIDItem: DWORD; out pdwState: CDCONTROLSTATEF): HResult; stdcall;
    function SetControlItemState(dwIDCtl: DWORD; dwIDItem: DWORD; dwState: CDCONTROLSTATEF): HResult; stdcall;
    function GetSelectedControlItem(dwIDCtl: DWORD; out pdwIDItem: DWORD): HResult; stdcall;
    function SetSelectedControlItem(dwIDCtl: DWORD; dwIDItem: DWORD): HResult; stdcall;
    function StartVisualGroup(dwIDCtl: DWORD; pszLabel: LPCWSTR): HResult; stdcall;
    function EndVisualGroup: HResult; stdcall;
    function MakeProminent(dwIDCtl: DWORD): HResult; stdcall;
    function SetControlItemText(dwIDCtl: DWORD; dwIDItem: DWORD; pszLabel: LPCWSTR): HResult; stdcall;
  end;

  IFileDialogControlEvents = interface(IUnknown)
    ['{36116642-D713-4b97-9B83-7484A9D00433}']
    function OnItemSelected(pfdc: IFileDialogCustomize; dwIDCtl: DWORD; dwIDItem: DWORD): HResult; stdcall;
    function OnButtonClicked(pfdc: IFileDialogCustomize; dwIDCtl: DWORD): HResult; stdcall;
    function OnCheckButtonToggled(pfdc: IFileDialogCustomize; dwIDCtl: DWORD; bChecked: BOOL): HResult; stdcall;
    function OnControlActivating(pfdc: IFileDialogCustomize; dwIDCtl: DWORD): HResult; stdcall;
  end;

  IFileOpenDialog = interface(IFileDialog)
    ['{d57c7288-d4ad-4768-be02-9d969532d960}']
    function GetResults(var ppenum: IShellItemArray): HResult; stdcall;
    function GetSelectedItems(var ppsai: IShellItemArray): HResult; stdcall;
  end;

  IFileSaveDialog = interface(IFileDialog)
    ['{84bccd23-5fde-4cdb-aea4-af64b83d78ab}']
    function SetSaveAsItem(psi: IShellItem): HResult; stdcall;
    function SetProperties(pStore: IPropertyStore): HResult; stdcall;
    function SetCollectedProperties(pList: IPropertyDescriptionList; fAppendDefault: BOOL): HResult; stdcall;
    function GetProperties(var ppStore: IPropertyStore): HResult; stdcall;
    function ApplyProperties(psi: IShellItem; pStore: IPropertyStore; hwnd: HWND; pSink: IFileOperationProgressSink): HResult; stdcall;
  end;

// AlphaBlend is only defined for win98&2k and up
// load dynamic and use ownfunction if not defined
var
  AlphaBlend: function(hdcDest: HDC; nXOriginDest, nYOriginDest, nWidthDest, nHeightDest: Integer; hdcSrc: HDC; nXOriginSrc, nYOriginSrc, nWidthSrc, nHeightSrc: Integer; blendFunction: TBlendFunction): BOOL; stdcall;
  GradientFill: function(DC: HDC; p2: PTriVertex; p3: ULONG; p4: Pointer; p5, p6: ULONG): BOOL; stdcall;
  GetComboBoxInfo: function(hwndCombo: HWND; pcbi: PComboboxInfo): BOOL; stdcall;
  GetMenuBarInfo: function(hwnd: HWND; idObject: LONG; idItem: LONG; pmbi: PMENUBARINFO): BOOL; stdcall;
  GetWindowInfo: function(hwnd: HWND; pwi: PWINDOWINFO): BOOL; stdcall;
  SetLayout: function(dc: HDC; l: DWord): DWord; stdcall;
  SetLayeredWindowAttributes: function (HWND: hwnd; crKey: COLORREF; bAlpha: byte; dwFlags: DWORD): BOOL; stdcall;
  UpdateLayeredWindow: function(hWnd: HWND; hdcDst: HDC; pptDst: PPoint; psize: PSize;
      hdcSrc: HDC; pptSrc: PPoint; crKey: COLORREF; pblend: PBlendFunction; dwFlags: DWORD): BOOL; stdcall;
  IsProcessDPIAware: function: BOOL; stdcall;
  TaskDialogIndirect: function(const pTaskConfig: PTASKDIALOGCONFIG; pnButton: PInteger; pnRadioButton: PInteger; pfVerificationFlagChecked: PBOOL): HRESULT; stdcall;
  TaskDialog: function(hwndParent: HWND; hInstance: HINST; pszWindowTitle: PCWSTR; pszMainInstruction: PCWSTR; pszContent: PCWSTR;
      dwCommonButtons: TASKDIALOG_COMMON_BUTTON_FLAGS; pszIcon: PCWSTR; pnButton: PInteger): HRESULT; stdcall;
  SHCreateItemFromParsingName: function(pszPath: PCWSTR; pbc: IBindCtx; const riid: REFIID; out ppv): HResult; stdcall;

const
  // ComCtlVersions
  ComCtlVersionIE3   = $00040046;
  ComCtlVersionIE4   = $00040047;
  ComCtlVersionIE401 = $00040048;
  ComCtlVersionIE5   = $00050050;
  ComCtlVersionIE501 = $00050051;
  ComCtlVersionIE6   = $00060000;

type
  SHSTOCKICONINFO = record
    cbSize: DWORD;
    hIcon: HICON;
    iSysImageIndex: integer;
    iIcon: integer;
    szPath: array[0..MAX_PATH - 1] of WCHAR;
  end;
  TSHSTOCKICONINFO = SHSTOCKICONINFO;
  PSHSTOCKICONINFO = ^SHSTOCKICONINFO;

var
  SHGetStockIconInfo: function(siid: integer; uFlags: UINT; psii: PSHSTOCKICONINFO): HResult; stdcall;

const
  SIID_SHIELD = 77;
  SHGFI_SMALLICON = $000000001;
  SHGFI_LARGEICON = $000000000;
  SHGFI_ICON      = $000000100;

type
  //64bit safe Timer functions and callback
  //todo: remove as soon the last supported fpc version has updated header (rev 22526)
  TIMERPROC = procedure (hWnd: HWND; uMsg: UINT; idEvent: UINT_PTR; dwTime: DWORD); stdcall;

  function SetTimer(hWnd:HWND; nIDEvent:UINT_PTR; uElapse:UINT; lpTimerFunc: TIMERPROC): UINT_PTR; stdcall; external 'user32' name 'SetTimer';
  function KillTimer(hWnd:HWND; uIDEvent:UINT_PTR):WINBOOL; stdcall; external 'user32' name 'KillTimer';
  function HasManifest: Boolean;
  function PrintWindow(HWND:HWND;hdcBlt:HDC;nFlags:DWORD):WINBOOL; stdcall; external 'user32.dll';

implementation

uses
  Win32Proc;

{$PACKRECORDS NORMAL}

function _AlphaBlend(hdcDest: HDC; nXOriginDest, nYOriginDest, nWidthDest, nHeightDest: Integer; hdcSrc: HDC; nXOriginSrc, nYOriginSrc, nWidthSrc, nHeightSrc: Integer; blendFunction: TBlendFunction): BOOL; stdcall;
var
  SCA: Byte absolute blendFunction.SourceConstantAlpha;

  R: TRect;
  DC, TmpDC: HDC;
  OldBmp, OldTmpBmp, SrcBmp, DstBmp, TmpBmp, AlphaBmp: HBITMAP;
  StretchSrc: Boolean;
  SrcSection, DstSection: TDIBSection;
type
  TLocalInfo = record
    Header: TBitmapInfoHeader;
    Colors: array[0..3] of Cardinal; // reserve extra color for colormasks
  end;
var
  Info: TLocalInfo;
  SrcBytesPtr: Pointer = nil;
  DstBytesPtr: Pointer = nil;
  AlphaBytesPtr: Pointer = nil;
  TmpBytesPtr: Pointer = nil;
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
  Info := Default(TLocalInfo);
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

  Result := False;
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
      AlphaBmp := INVALID_HANDLE_VALUE;
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

  SrcAlpha := nil;
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

function _GradientFill(DC: HDC; p2: PTriVertex; p3: ULONG; p4: Pointer; p5, p6: ULONG): BOOL;
begin
  Result := False;
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

function _GetMenuBarInfo(hwnd: HWND; idObject: LONG; idItem: LONG; pmbi: PMENUBARINFO): BOOL; stdcall;
begin
  Result := False;
end;

function _GetWindowInfo(hwnd: HWND; pwi: PWINDOWINFO): BOOL; stdcall;
begin
  Result := False;
end;

function _SHGetStockIconInfo(siid: integer; uFlags: UINT; psii: PSHSTOCKICONINFO): HResult; stdcall;
begin
  Result := E_NOTIMPL;
end;

function _SetLayout(dc: HDC; l: DWord): DWord; stdcall;
begin
  Result := GDI_ERROR;
end;

function _SetLayeredWindowAttributes(HWND: hwnd; crKey: COLORREF; bAlpha: byte; dwFlags: DWORD): BOOL; stdcall;
begin
  Result := False;
end;

function _UpdateLayeredWindow(hWnd: HWND; hdcDst: HDC; pptDst: PPoint; psize: PSize;
      hdcSrc: HDC; pptSrc: PPoint; crKey: COLORREF; pblend: PBlendFunction; dwFlags: DWORD): BOOL; stdcall;
begin
  Result := False;
end;

function _IsProcessDPIAware: BOOL; stdcall;
begin
  Result := False;
end;

function _TaskDialogIndirect(const pTaskConfig: PTASKDIALOGCONFIG; pnButton: PInteger; pnRadioButton: PInteger; pfVerificationFlagChecked: PBOOL): HRESULT; stdcall;
begin
  Result := E_NOTIMPL;
end;

function _TaskDialog(hwndParent: HWND; hInstance: HINST; pszWindowTitle: PCWSTR; pszMainInstruction: PCWSTR; pszContent: PCWSTR;
    dwCommonButtons: TASKDIALOG_COMMON_BUTTON_FLAGS; pszIcon: PCWSTR; pnButton: PInteger): HRESULT; stdcall;
begin
  Result := E_NOTIMPL;
end;

function _SHCreateItemFromParsingName(pszPath: PCWSTR; pbc: IBindCtx; const riid: REFIID; out ppv): HResult; stdcall;
begin
  Result := E_NOTIMPL;
end;

function GetComCtlVersion: Cardinal;
begin
  if (ComCtlVersion <> 0) then
    Result := ComCtlVersion
  else
    Result := GetFileVersion(comctl32);
end;

function HasManifest: Boolean;
begin
  Result := (GetComCtlVersion >= ComCtlVersionIE6);
end;

const
  msimg32lib = 'msimg32.dll';
  user32lib = 'user32.dll';
  shell32lib = 'shell32.dll';
  gdi32lib = 'gdi32.dll';
  comctl32lib = 'comctl32.dll';

var
  msimg32handle: THandle = 0;
  user32handle: THandle = 0;
  shell32handle: THandle = 0;
  gdi32handle: THandle = 0;
  comctl32handle: THandle = 0;

procedure Initialize;
var
  p: Pointer;
begin
  if WindowsVersion = wvUnknown then
    UpdateWindowsVersion;
  ComCtlVersion := GetComCtlVersion;

  GetComboBoxInfo := nil;
  GetMenuBarInfo := nil;
  GetWindowInfo := nil;
  
  // defaults
  Pointer(GradientFill) := @_GradientFill;
  // Detect win98 since aplhablend doesn't support all bitmap types
  if WindowsVersion = wv98
  then Pointer(AlphaBlend) := @_AlphaBlend98
  else Pointer(AlphaBlend) := @_AlphaBlend;


  msimg32handle := LoadLibrary(msimg32lib);
  if msimg32handle <> 0
  then begin 
    if WindowsVersion <> wv98
    then begin
      p := GetProcAddress(msimg32handle, 'AlphaBlend');
      if p <> nil 
      then Pointer(AlphaBlend) := p;
    end;

    p := GetProcAddress(msimg32handle, 'GradientFill');
    if p <> nil 
    then Pointer(GradientFill) := p;
  end;
  
  // Defaults
  Pointer(GetComboboxInfo) := @_GetComboboxInfo;
  Pointer(GetMenuBarInfo) := @_GetMenuBarInfo;
  Pointer(GetWindowInfo) := @_GetWindowInfo;
  Pointer(SetLayeredWindowAttributes) := @_SetLayeredWindowAttributes;
  Pointer(UpdateLayeredWindow) := @_UpdateLayeredWindow;
  Pointer(IsProcessDPIAware) := @_IsProcessDPIAware;

  user32handle := LoadLibrary(user32lib);
  if user32handle <> 0 then
  begin
    p := GetProcAddress(user32handle, 'GetComboBoxInfo');
    if p <> nil 
    then Pointer(GetComboboxInfo) := p;

    p := GetProcAddress(user32handle, 'GetMenuBarInfo');
    if p <> nil 
    then Pointer(GetMenuBarInfo) := p;
    
    p := GetProcAddress(user32handle, 'GetWindowInfo');
    if p <> nil 
    then Pointer(GetWindowInfo) := p;

    p := GetProcAddress(user32handle, 'SetLayeredWindowAttributes');
    if p <> nil
    then Pointer(SetLayeredWindowAttributes) := p;

    p := GetProcAddress(user32handle, 'UpdateLayeredWindow');
    if p <> nil
    then Pointer(UpdateLayeredWindow) := p;

    p := GetProcAddress(user32handle, 'IsProcessDPIAware');
    if p <> nil
    then Pointer(IsProcessDPIAware) := p;
  end;

  // Defaults
  Pointer(SHGetStockIconInfo) := @_SHGetStockIconInfo;
  Pointer(SHCreateItemFromParsingName) := @_SHCreateItemFromParsingName;

  shell32handle := LoadLibrary(shell32lib);
  if shell32handle <> 0 then
  begin
    p := GetProcAddress(shell32handle, 'SHGetStockIconInfo');
    if p <> nil 
    then Pointer(SHGetStockIconInfo) := p;

    p := GetProcAddress(shell32handle, 'SHCreateItemFromParsingName');
    if p <> nil
    then Pointer(SHCreateItemFromParsingName) := p;
  end;

  // Defaults
  Pointer(SetLayout) := @_SetLayout;
  
  gdi32handle := LoadLibrary(gdi32lib);
  if gdi32handle <> 0 then
  begin
    p := GetProcAddress(gdi32handle, 'SetLayout');
    if p <> nil 
    then Pointer(SetLayout) := p;
  end;

  // Defaults
  Pointer(TaskDialogIndirect) := @_TaskDialogIndirect;
  Pointer(TaskDialog) := @_TaskDialog;

  comctl32handle := LoadLibrary(comctl32lib);
  if comctl32handle <> 0 then
  begin
    p := GetProcAddress(comctl32handle, 'TaskDialogIndirect');
    if p <> nil
    then Pointer(TaskDialogIndirect) := p;

    p := GetProcAddress(comctl32handle, 'TaskDialog');
    if p <> nil
    then Pointer(TaskDialog) := p;
  end;
end;

procedure Finalize;
begin
  AlphaBlend := @_AlphaBlend;
  GetComboboxInfo := nil;
  GetMenuBarInfo := nil;

  if msimg32handle <> 0
  then FreeLibrary(msimg32handle);
  msimg32handle := 0;
  
  if user32handle <> 0 then
    FreeLibrary(user32handle);
  user32handle := 0;

  if shell32handle <> 0 then
    FreeLibrary(shell32handle);
  shell32handle := 0;

  if gdi32handle <> 0 then
    FreeLibrary(gdi32handle);
  gdi32handle := 0;

  if comctl32handle <> 0 then
    FreeLibrary(comctl32handle);
  comctl32handle := 0;
end;


initialization
  Initialize;

finalization
  Finalize;

end.
