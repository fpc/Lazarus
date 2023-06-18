unit DelphiCompat;

{ Delphi Compatibility Unit

  Copyright (C) 2007 Luiz Américo Pereira Câmara
  pascalive@bol.com.br

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

}

{$mode objfpc}{$H+}
{.$define DEBUG_DELPHICOMPAT}

interface

uses
  LMessages, Types, Classes, LCLType, LCLVersion;

const
  //Messages
  WM_GETDLGCODE = LM_GETDLGCODE;
  WM_ERASEBKGND = LM_ERASEBKGND;
  WM_VSCROLL = LM_VSCROLL;
  WM_HSCROLL = LM_HSCROLL;
  WM_CHAR = LM_CHAR;
  WM_KEYDOWN = LM_KEYDOWN;
  WM_KEYUP = LM_KEYUP;
  WM_KILLFOCUS = LM_KILLFOCUS;
  WM_SIZE = LM_SIZE;
  WM_LBUTTONDBLCLK = LM_LBUTTONDBLCLK;
  WM_LBUTTONDOWN = LM_LBUTTONDOWN;

  LOCALE_IMEASURE = 13;
  LOCALE_USER_DEFAULT = $400;

type
  //TWM* types
  TMessage = TLMessage;
  TWMHScroll = TLMHScroll;
  TWMVScroll = TLMVScroll;
  TWMChar = TLMChar;
  TWMKeyDown = TLMKeyDown;
  TWMKeyUp = TLMKeyUp;
  TWMKillFocus = TLMKillFocus;
  TWMSize = TLMSize;
  TWMLButtonDblClk = TLMLButtonDblClk;
  TWMMeasureItem = TLMMeasureItem;
  TWMDrawItem = TLMDrawItems;
  
  //timer
  TTimerNotify = procedure (TimerId: PtrUInt) of object;

function BeginDeferWindowPos({%H-}nNumWindows: LongInt):THandle;
function BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD): Boolean;

function CF_UNICODETEXT: TClipboardFormat;
function CopyImage({%H-}hImage: THandle; {%H-}uType:LongWord; {%H-}cxDesired, {%H-}cyDesired: LongInt; {%H-}fuFlags:LongWord):THandle;

function DeferWindowPos({%H-}hWinPosInfo, {%H-}hWnd, {%H-}hWndInsertAfter:THandle; {%H-}x, {%H-}y, {%H-}cx, {%H-}cy:longint; {%H-}uFlags:LongWord):THandle;
function DrawTextW(hDC: HDC; lpString: PWideChar; nCount: Integer; var lpRect: TRect; uFormat: LongWord): Integer;

function EndDeferWindowPos({%H-}hWinPosInfo:THandle):Boolean;
function ExtTextOutW(DC: HDC; X, Y: Integer; Options: LongInt; Rect: PRect;
  Str: PWideChar; Count: LongInt; Dx: PInteger): Boolean;

function GdiFlush: Boolean;
function GetACP:LongWord;
function GetBkColor(DC:HDC):COLORREF;
function GetCurrentObject(hdc: HDC; uObjectType: UINT): HGDIOBJ;
function GetDCEx({%H-}hWnd:THandle; {%H-}hrgnClip:HRGN; {%H-}flags:DWORD):HDC;
function GetDoubleClickTime: UINT;
function GetKeyboardLayout({%H-}dwLayout:DWORD):THandle;
function GetKeyboardState({%H-}lpKeyState:PBYTE):BOOLEAN;
function GetLocaleInfo({%H-}Locale, {%H-}LCType:LongWord; {%H-}lpLCData:PChar; {%H-}cchData:longint):longint;
function GetRandomRgn({%H-}DC: HDC; {%H-}Rgn: HRGN; {%H-}iNum: Integer): Integer; stdcall;
function GetTextAlign({%H-}hDC:HDC): LongWord;
function GetTextExtentExPoint(DC: HDC; Str: PChar;
  Count, {%H-}MaxWidth: Integer; {%H-}MaxCount, PartialWidths: PInteger;
  var Size: TSize): BOOL;
function GetTextExtentExPointW(DC: HDC; Str: PWideChar; Count, MaxWidth: Integer;
  MaxCount, PartialWidths: PInteger; var Size: TSize): BOOL;
function GetTextExtentPoint32W(DC: HDC; Str: PWideChar; Count: Integer; out Size: TSize): Boolean;
function GetTextExtentPointW(DC: HDC; Str: PWideChar; Count: Integer; out Size: TSize): Boolean;
function GetWindowDC({%H-}hWnd:THandle):HDC;

function ImageList_DragShowNolock({%H-}fShow: Boolean): Boolean;
function InvertRect(DC: HDC; const lprc: TRECT): Boolean;

function KillTimer(hWnd:THandle; nIDEvent:UINT_PTR):Boolean;

function MapWindowPoints(hWndFrom, hWndTo: HWND; var lpPoints; cPoints: UINT): Integer;
function MultiByteToWideChar({%H-}CodePage, {%H-}dwFlags:DWORD; {%H-}lpMultiByteStr:PChar; {%H-}cchMultiByte:longint; {%H-}lpWideCharStr:PWideChar;{%H-}cchWideChar:longint):longint;

function OffsetRgn({%H-}hrgn:HRGN; {%H-}nxOffset, {%H-}nYOffset:longint):longint;

function RedrawWindow(hWnd:THandle; lprcUpdate:PRECT; {%H-}hrgnUpdate:HRGN; flags:LongWord): Boolean;

function ScrollDC({%H-}DC:HDC; {%H-}dx:longint; {%H-}dy:longint; var {%H-}lprcScroll:TRECT; var {%H-}lprcClip:TRECT;{%H-}hrgnUpdate:HRGN; {%H-}lprcUpdate:PRECT):Boolean;
function ScrollWindow(hWnd:THandle; XAmount, YAmount:longint;{%H-}lpRect:PRECT; {%H-}lpClipRect:PRECT):Boolean;
function SetBrushOrgEx({%H-}DC:HDC; {%H-}nXOrg, {%H-}nYOrg:longint; {%H-}lppt:PPOINT):Boolean;
function SetTimer(hWnd:THandle; nIDEvent:UINT_PTR; uElapse:LongWord; lpTimerFunc:TTimerNotify):UINT_PTR;
function SubtractRect(var {%H-}lprcDst: TRect; const {%H-}lprcSrc1, {%H-}lprcSrc2: TRect): Boolean;

function TextOutW(DC: HDC; X,Y : Integer; Str : PWideChar; Count: Integer) : Boolean;
function ToAscii({%H-}uVirtKey, {%H-}uScanCode:LongWord; {%H-}lpKeyState: PByte; {%H-}lpChar: PWord; {%H-}uFlags:LongWord): LongInt;

function UpdateWindow({%H-}Handle: HWND): Boolean;

implementation


uses
{$i uses.inc}
  maps, LazUTF8, LCLProc, LCLMessageGlue, Controls
  {$ifdef DEBUG_DELPHICOMPAT}
  ,multiloglcl, filechannel
  {$endif}
  ;

{$ifdef DEBUG_DELPHICOMPAT}
const
  //Logger  classes
  lcInfo = 0;
  lcStack = 1;

var
  Logger: TLCLLogger;
{$endif}


{$i delphicompat.inc}

initialization
  FTimerList := TTimerList.Create;
  {$ifdef DEBUG_DELPHICOMPAT}
  Logger := TLCLLogger.Create;
  Logger.Channels.Add(TFileChannel.Create('delphicompat.log'));
  Logger.ActivateClasses := [lcInfo,lcStack];
  Logger.MaxStackCount := 3;
  {$endif}

finalization
  FTimerList.Free;
  {$ifdef DEBUG_DELPHICOMPAT}
  Logger.Free;
  {$endif}
end.
