{ 
  Extra Win32 code that's not in the RTL.
  Copyright (C) 2001, 2002 Keith Bowes.

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

{$IFDEF TRACE}
  {$ASSERTIONS ON}
{$ENDIF}

{$PACKRECORDS C}
{$SMARTLINK ON}

Interface

Uses Classes, RegExpr, Windows;

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

{ Creates a string list limited to Count (-1 for no limit) entries by splitting
  Str into substrings around SplitStr }
Function Split(Const Str: String; SplitStr: String; Count: Integer; Const CaseSensitive: Boolean): TStringList;

{ Creates a string list limited to Count (-1 for no limit) entries by splitting
  Str into substrings around any character or string that matches the pattern
  of SplitStr }
Function Split(Const Str: PChar; SplitStr: TRegExprEngine; Count: Integer; Const CaseSensitive: Boolean): TStringList;

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

Function Split(Const Str: PChar; SplitStr: TRegExprEngine; Count: Integer; Const CaseSensitive: Boolean): TStringList;
Var
  Index, Index2, Len, Len2: Integer;
  LastIndex: Byte;
  OrigCt: Integer;
  S, S2: String;
Begin
  Result := TStringList.Create;
  OrigCt := Count;
  S := String(Str);
  RegExprPos(SplitStr, Str, Index, Len);
  Repeat
    If OrigCt = 0 Then
      Break;
    S := Copy(S, Index + 1, Length(S));
    Result.Capacity := Result.Count;
    S2 := Copy(S, Index + 1, Length(S));
    RegExprPos(SplitStr, PChar(S2), Index2, Len2);
    Result.Add(Copy(S, Index + Len, (Index2 - Index) + 1));
    RegExprPos(SplitStr, PChar(S), Index, Len);
    If Index > 0 Then
      LastIndex := Index;
    If Count > -1 Then
      Dec(Count)
  Until (Index < 1) Or (Count = 0);
  Result.Capacity := Result.Count;
  Result.Insert(0, Copy(Str, Length(String(Str)) - Length(S), Index + 1));
  If Count <> 0 Then
  Begin
    Result.Capacity := Result.Count;
    Result.Add(Copy(S, LastIndex + Len + (Index2 - Index), Length(S)));
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