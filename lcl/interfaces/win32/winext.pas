{ 
  Extra Win32 code that's not in the RTL.
  Copyright (C) 2001 Keith Bowes.

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.
}

Unit WinExt;

{$PACKRECORDS C}
{$SMARTLINK ON}

Interface

Uses Windows;

{ Types not included in system.pp }
Type
  { Pointer to TObject }
  PObject = ^TObject;

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

{ Win32 API functions not included in windows.pp }
{ Get the ancestor at level Flag of window HWnd }
Function GetAncestor(Const HWnd: HWND; Const Flag: UINT): HWND; StdCall; External 'user32';
{ Get information about combo box hwndCombo and place in pcbi }
Function GetComboBoxInfo(Const hwndCombo: HWND; pcbi: PCOMBOBOXINFO): BOOL; StdCall; External 'user32';

{ Miscellaneous functions }
{ Convert string Str to a PChar }
Function StrToPChar(Const Str: String): PChar;

Implementation

Uses SysUtils;

{$PACKRECORDS NORMAL}

Var
  TmpStr: PChar;

Function StrToPChar(Const Str: String): PChar;
Begin
  TmpStr := PChar(Str);
  Result := TmpStr;
End;

Initialization

TmpStr := StrNew('');

Finalization

StrDispose(TmpStr);
TmpStr := Nil;

End.