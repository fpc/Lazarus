Unit WinExt;

{ winext.pas: Extra Win32 code that's not in the RTL. }
{ Copyright (C) 2001 Keith Bowes. }
{ This unit is licensed under the GNU LGPL.
  See http://www.gnu.org/copyleft/lesser.html for details. }

{$LONGSTRINGS ON}
{$MODE OBJFPC}
{$PACKRECORDS C}
{$SMARTLINK ON}
{$TYPEDADDRESS ON}

Interface

Uses SysUtils, Windows;

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
  { The windows' direct parent window }
  GA_PARENT = 1;
  { The windows' root window }
  GA_ROOT = 2;
  { The windows' owner }
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

{$PACKRECORDS NORMAL}

Type
  TStrArray = Array[1..2] Of Char;
  PStrArray = ^TStrArray;

Var
  ArLen: Cardinal;
  StrArray: PStrArray;

{ Function StrToPChar: Converts a String to a PChar without using a
  buffer.
  Parameters:
    * Str: String to convert.
  Returns: A PChar equivalent of the input string.
}
Function StrToPChar(Const Str: String): PChar;
Var
  I: Cardinal;
Begin
  StrArray := Nil;
  ArLen := SizeOf(Str) * Length(Str);
  GetMem(StrArray, ArLen);

  For I := 1 To Length(Str) Do
    StrArray^[I] := Str[I];

  Result := PChar(StrArray);
End;

Initialization

ArLen := 0;
StrArray := Nil;

Finalization

If ArLen <> 0 Then
  FreeMem(StrArray, ArLen);

StrArray := Nil;

End.