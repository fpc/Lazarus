{
  Extra Win32 code that's not in the RTL.
  Copyright (C) 2001, 2002 Keith Bowes.

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

Unit WinExt;

{$mode objfpc}{$H+}

{$IFDEF TRACE}
  {$ASSERTIONS ON}
{$ENDIF}

{$PACKRECORDS C}
{$SMARTLINK ON}

Interface

Uses Classes, Windows;

//roozbeh : remove those things include in our windows.pp!
{ Win32 API records not included in windows.pp }
Type
  { Record for the @link(GetComboBoxInfo) function }
  COMBOBOXINFO = Record
    cbSize, stateButton: DWORD;
    rcItem, rcButton: RECT;
    hwndCombo, hwndItem, hwndList: HWND;
  End;
  { Pointer to @link(COMBOBOXINFO) }
  PComboBoxInfo = ^COMBOBOXINFO;

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
Const
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
  { Get the progress bar range }
  PBM_GETRANGE = 1031;
  { Smooth progrss bar }
  PBS_SMOOTH = 1;
  { Vertical progress bar }
  PBS_VERTICAL = 4;
  { Mouse-hovering message }
  WM_MOUSEHOVER = $02A1;
  { Mouse-leaving message }
  WM_MOUSELEAVE = $02A3;
  { Mouse-wheel message }
  WM_MOUSEWHEEL = $020A;
  { Left-to-right reading text }
  WS_EX_LTRLEADING = 0;

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
  
// progressbar
  PBM_SETRANGE32          = 1030;

// for calendar control
  MCN_FIRST               = (0-750);          // monthcal
  MCN_SELCHANGE           = (MCN_FIRST + 1);

// for GetRandomRgn
  SYSRGN                  = 4;
  
// missing listview macros
function ListView_GetHeader(hwndLV: HWND): HWND;
function ListView_GetExtendedListViewStyle(hwndLV: HWND): DWORD;
function ListView_SetExtendedListViewStyle(hwndLV: HWND; dw: DWORD): BOOL;
function ListView_GetHoverTime(hwndLV: HWND): DWORD;
function ListView_SetHoverTime(hwndLV: HWND; dwHoverTimeMs: DWORD): DWORD;




{ Miscellaneous functions }
{ Convert string Str to a PChar }
function StrToPChar(Const Str: String): PChar;

{ Replace OrigStr with ReplStr in Str }
function Replace(Const Str, OrigStr, ReplStr: String; Const Global: Boolean): String;

{ Creates a string list limited to Count (-1 for no limit) entries by splitting
  Str into substrings around SplitStr }
function Split(Const Str: String; SplitStr: String; Count: Integer; Const CaseSensitive: Boolean): TStringList;

//{$ifdef VER2_0}
function GET_X_LPARAM(lp : Windows.LParam) : longint;
function GET_Y_LPARAM(lp : Windows.LParam) : longint;
//{$endif VER2_0}

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

{$ifndef win32}
type
  // See: http://msdn.microsoft.com/en-us/library/aa453954.aspx
  // Available in Windows CE 5.0+
  TSetWindowOrgEx = function(
    dc:HDC;
    x:longint;
    y:longint;
    point:LPPOINT):WINBOOL; cdecl;
  TGetWindowOrgEx = function(DC: HDC; Point: LPPoint): BOOL; cdecl;

var
  SetWindowOrgEx: TSetWindowOrgEx;
  GetWindowOrgEx: TGetWindowOrgEx;
  lModule: HINST;
{$endif}

Implementation

Uses SysUtils;

{$PACKRECORDS NORMAL}

function ListView_GetHeader(hwndLV: HWND): HWND;
begin
  Result := SendMessage(hwndLV, LVM_GETHEADER, 0, 0);
end;

function ListView_GetExtendedListViewStyle(hwndLV: HWND): DWORD;
begin
  Result := SendMessage(hwndLV, LVM_GETEXTENDEDLISTVIEWSTYLE, 0, 0);
end;

function ListView_SetExtendedListViewStyle(hwndLV: HWND; dw: DWORD): BOOL;
begin
  Result := BOOL(SendMessage(hwndLV, LVM_SETEXTENDEDLISTVIEWSTYLE, 0, dw));
end;

function ListView_GetHoverTime(hwndLV: HWND): DWORD;
begin
  Result := SendMessage(hwndLV, LVM_GETHOVERTIME, 0, 0);
end;

function ListView_SetHoverTime(hwndLV: HWND; dwHoverTimeMs: DWORD): DWORD;
begin
  Result := SendMessage(hwndLV, LVM_SETHOVERTIME, 0, dwHoverTimeMs);
end;

procedure ListView_SetCheckState(hwndLV: HWND; iIndex: UINT;fCheck:BOOL);
begin
end;


Var
  TmpStr: PChar;

function StrToPChar(Const Str: String): PChar;
Begin
  TmpStr := PChar(Str);
  Result := TmpStr;
End;

function Replace(Const Str, OrigStr, ReplStr: String; Const Global: Boolean): String;
Var
  InsPt: Integer;
Begin
  Result := Str;
  Repeat
    InsPt := Pos(OrigStr, Result);
    If InsPt <> 0 Then
    Begin
      Delete(Result, InsPt, Length(OrigStr));
      Insert(ReplStr, Result, InsPt);
    End;

    If Not Global Then
      Break;
  Until InsPt = 0;
End;

function Split(Const Str: String; SplitStr: String; Count: Integer;
  Const CaseSensitive: Boolean): TStringList;
Var
  LastP, P: Integer;
  OrigCt: Integer;
  S: String;
Begin
  Result := TStringList.Create;
  OrigCt := Count;
  If Not CaseSensitive Then
  Begin
    S := LowerCase(Str);
    SplitStr := LowerCase(SplitStr);
  End
  Else
    S := Str;
  P := Pos(SplitStr, Str);
  LastP:=0;
  Repeat
    S := Copy(S, P + 1, Length(S));
    Result.Capacity := Result.Count;
    Result.Add(Copy(Str, LastP + 1, P - 1));
    P := Pos(SplitStr, S);
    LastP := P;
    If Count > 0 Then
      Dec(Count)
  Until (P = 0) Or (Count = 0);
  If OrigCt <> 0 Then
  Begin
    Result.Capacity := Result.Count;
    Result.Add(Copy(Str, (Length(Str) - Length(S)) + 1, Pos(SplitStr, Str) - 1));
  End;
End;

//{$ifdef VER2_0}
function GET_X_LPARAM(lp : Windows.LParam) : longint;
  begin
    result:=smallint(LOWORD(lp));
  end;


function GET_Y_LPARAM(lp : Windows.LParam) : longint;
  begin
    result:=smallint(HIWORD(lp));
  end;
//{$endif VER2_0}

Initialization

TmpStr := StrNew('');

{$ifndef Win32}
  lModule := LoadLibrary(KernelDLL);
  GetWindowOrgEx := TGetWindowOrgEx(GetProcAddress(lModule, 'GetWindowOrgEx'));
  SetWindowOrgEx := TSetWindowOrgEx(GetProcAddress(lModule, 'SetWindowOrgEx'));
{$endif}

Finalization

Try
  StrDispose(TmpStr);
  TmpStr := Nil;
Except
  On E: Exception Do
    Assert(False, Format('Trace:Could not deallocate string --> %S', [E.Message]));
End;

End.
