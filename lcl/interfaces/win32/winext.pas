{ 
  Extra Win32 code that's not in the RTL.
  Copyright (C) 2001, 2002 Keith Bowes.

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

Unit WinExt;

{$IFDEF TRACE}
  {$ASSERTIONS ON}
{$ENDIF}

{$PACKRECORDS C}
{$SMARTLINK ON}

Interface

Uses Classes, Windows;

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
  { Mouse-hovering message }
  WM_MOUSEHOVER = $02A1;
  { Mouse-leaving message }
  WM_MOUSELEAVE = $02A3;
  { Mouse-wheel message }
  WM_MOUSEWHEEL = $020A;
  { Left-to-right reading text }
  WS_EX_LTRLEADING = 0;

  { Tab Control Styles}
  TCS_RIGHT = $0002;
  TCS_BOTTOM = $0002;
  TCS_VERTICAL = $0080;
  TCS_MULTILINE = $0200;

{ Win32 API functions not included in windows.pp }
{ Get the ancestor at level Flag of window HWnd }
Function GetAncestor(Const HWnd: HWND; Const Flag: UINT): HWND; StdCall; External 'user32';
{ Get information about combo box hwndCombo and place in pcbi }
Function GetComboBoxInfo(Const hwndCombo: HWND; pcbi: PCOMBOBOXINFO): BOOL; StdCall; External 'user32';

{ Miscellaneous functions }
{ Convert string Str to a PChar }
Function StrToPChar(Const Str: String): PChar;

{ Replace OrigStr with ReplStr in Str }
Function Replace(Const Str, OrigStr, ReplStr: String; Const Global: Boolean): String;

{ Creates a string list limited to Count (-1 for no limit) entries by splitting
  Str into substrings around SplitStr }
Function Split(Const Str: String; SplitStr: String; Count: Integer; Const CaseSensitive: Boolean): TStringList;

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

Function Replace(Const Str, OrigStr, ReplStr: String; Const Global: Boolean): String;
Var
  InsPt: Byte;
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

Function Split(Const Str: String; SplitStr: String; Count: Integer; Const CaseSensitive: Boolean): TStringList;
Var
  LastP, P: Byte;
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

Initialization

TmpStr := StrNew('');

Finalization

Try
  StrDispose(TmpStr);
  TmpStr := Nil;
Except
  On E: Exception Do
    Assert(False, Format('Trace:Could not deallocate string --> %S', [E.Message]));
End;

End.
