
{  $Id$  }
{
 /***************************************************************************
                                LCLIntf.pas
                                -----------
                             Component Library Windows Controls
                   Initial Revision  : Fri Jul 23 20:00:00 PDT 1999


 ***************************************************************************/

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

{
@author(Curtis White <cwhite@aracnet.com>)
@created(17-Oct-1999)
@lastmod(17-Oct-1999)

This unit is being created specifically for compatibility with Delphi. It
should only be used for constants and type definitions that are included in
the Delphi Windows unit. This is only done for compatibility.
}

unit LCLIntf;

{$mode objfpc}{$H+}

interface

uses
  Math, Classes, SysUtils, LCLType, LCLProc, GraphType, InterfaceBase,
  LResources;

{$ifdef Trace}
  {$ASSERTIONS ON}
{$endif}
{$DEFINE ClientRectBugFix}

// All winapi related stuff (Delphi compatible)
{$I winapih.inc}
// All interface communication (Our additions)
{$I lclintfh.inc}


function MakeLong(A,B : Word) : LongInt;
function MakeWord(A,B : Byte) : Word;

function PredefinedClipboardFormat(
  AFormat: TPredefinedClipboardFormat): TClipboardFormat;

function CharLower(c: char): char;
function CharUpper(c: char): char;

function MsgKeyDataToShiftState(KeyData: Longint): TShiftState;


{$IFDEF win32}
function GetTickCount: DWord; stdcall; external 'kernel32.dll' name 'GetTickCount';
{$ELSE}
function GetTickCount: DWord;
{$ENDIF}

{$IFDEF DebugLCL}
function GetTickStep: DWord;
{$ENDIF}


implementation

{$IFNDEF Win32}
uses
  {$IFDEF Ver1_0}Linux{$ELSE}Unix{$ENDIF};
{$ENDIF}

var
  FPredefinedClipboardFormats:
    array[TPredefinedClipboardFormat] of TClipboardFormat;
  LowerCaseChars: array[char] of char;
  UpperCaseChars: array[char] of char;

{$IFNDEF Win32}
function GetTickCount: DWord;
begin
  Result := DWord(Trunc(Now * 24 * 60 * 60 * 1000));
end;
{$ENDIF}

{$IFDEF DebugLCL}
var
  LastTickValid: boolean;
  LastTick: DWord;

function GetTickStep: DWord;
var
  CurTick: DWord;
begin
  CurTick:=GetTickCount;
  if LastTickValid then begin
    if LastTick<=CurTick then
      Result:=CurTick-LastTick
    else begin
      // tick counter has restarted
      Result:=CurTick+(DWord($FFFFFFFF)-LastTick+1);
    end;
  end else begin
    Result:=0;
  end;
  LastTickValid:=true;
  LastTick:=CurTick;
end;
{$ENDIF}

function MakeLong(A,B : Word) : LongInt;
begin
  Result := A or B shl 16;
end;

function MakeWord(A,B : Byte) : Word;
Begin
  Result := A or B shl 8;
end;

function PredefinedClipboardFormat(AFormat: TPredefinedClipboardFormat
  ): TClipboardFormat;
begin
  if FPredefinedClipboardFormats[AFormat]=0 then
    FPredefinedClipboardFormats[AFormat]:=
      ClipboardRegisterFormat(PredefinedClipboardMimeTypes[AFormat]);
  Result:=FPredefinedClipboardFormats[AFormat];
end;

function CharLower(c: char): char;
begin
  Result:=LowerCaseChars[c];
end;

function CharUpper(c: char): char;
begin
  Result:=UpperCaseChars[c];
end;

function MsgKeyDataToShiftState(KeyData: Longint): TShiftState;
begin
  Result := [];

  if GetKeyState(VK_SHIFT) < 0 then Include(Result, ssShift);
  if GetKeyState(VK_CONTROL) < 0 then Include(Result, ssCtrl);
  if KeyData and $20000000 <> 0 then Include(Result, ssAlt);
end;

{$I winapi.inc}
{$I lclintf.inc}

procedure InternalInit;
var
  AClipboardFormat: TPredefinedClipboardFormat;
  c: char;
  s: string;
begin
  for AClipboardFormat:=Low(TPredefinedClipboardFormat) to
    High(TPredefinedClipboardFormat) do
      FPredefinedClipboardFormats[AClipboardFormat]:=0;
  for c:=Low(char) to High(char) do begin
    s:=lowercase(c);
    LowerCaseChars[c]:=s[1];
    UpperCaseChars[c]:=upcase(c);
  end;
  {$IFDEF DebugLCL}
  LastTickValid:=false;
  {$ENDIF}
end;

initialization
  {$I lclicons.lrs}
  InternalInit;

end.

{
  $Log$
  Revision 1.19  2005/03/11 14:40:37  mattias
  moved CM_ message constants from crontrols.pp to lmessages.pp to break circles and clean up controls.pp

  Revision 1.18  2004/11/10 15:25:32  mattias
  updated memcheck.pas from heaptrc.pp

  Revision 1.17  2004/11/03 22:59:58  marc
  * fixed GetTickCount

  Revision 1.16  2004/08/18 09:31:21  mattias
  removed obsolete unit vclglobals

  Revision 1.15  2004/05/02 12:01:15  mattias
  removed unneeded units in uses sections

  Revision 1.14  2004/03/06 21:57:14  mattias
  fixed compilation under fpc 1.9.3

  Revision 1.13  2004/02/23 08:19:04  micha
  revert intf split

  Revision 1.11  2004/02/19 09:57:03  mattias
  moved GetTickStep to IFDEF DebugLCL

  Revision 1.10  2004/02/19 05:07:16  mattias
  CreateBitmapFromRawImage now creates mask only if needed

  Revision 1.9  2004/02/18 08:50:42  mattias
  moved GetTickStep to non win32

  Revision 1.8  2004/02/17 22:17:40  mattias
  accelerated conversion from data to lrs

  Revision 1.7  2004/02/02 12:44:45  mattias
  implemented interface constraints

  Revision 1.6  2003/11/27 23:02:30  mattias
  removed menutype.pas

  Revision 1.5  2003/11/26 21:30:19  mattias
  reduced unit circles, fixed fpImage streaming

  Revision 1.4  2003/11/25 14:21:28  micha
  new api lclenable,checkmenuitem according to list

  Revision 1.3  2003/11/24 11:03:07  marc
  * Splitted winapi*.inc into a winapi and a lcl interface communication part

  Revision 1.2  2003/09/19 16:10:32  mattias
  started TDBNavigator

  Revision 1.1  2003/09/18 09:21:03  mattias
  renamed LCLLinux to LCLIntf

  Revision 1.25  2002/12/16 12:12:50  mattias
  fixes for fpc 1.1

  Revision 1.24  2002/12/12 17:47:45  mattias
  new constants for compatibility

  Revision 1.23  2002/11/23 09:34:12  mattias
  fixed compiling errors for synregexpr.pas

  Revision 1.22  2002/10/26 15:15:46  lazarus
  MG: broke LCL<->interface circles

  Revision 1.21  2002/10/25 10:42:08  lazarus
  MG: broke minor circles

  Revision 1.20  2002/10/24 10:05:51  lazarus
  MG: broke graphics.pp <-> clipbrd.pp circle

  Revision 1.19  2002/06/04 15:17:21  lazarus
  MG: improved TFont for XLFD font names

  Revision 1.18  2002/05/20 14:19:03  lazarus
  MG: activated the clientrect bugfixes

  Revision 1.17  2002/05/10 06:05:50  lazarus
  MG: changed license to LGPL

  Revision 1.16  2002/03/08 16:16:55  lazarus
  MG: fixed parser of end blocks in initialization section added label sections

  Revision 1.15  2002/02/03 00:24:00  lazarus
  TPanel implemented.
  Basic graphic primitives split into GraphType package, so that we can
  reference it from interface (GTK, Win32) units.
  New Frame3d canvas method that uses native (themed) drawing (GTK only).
  New overloaded Canvas.TextRect method.
  LCLLinux and Graphics was split, so a bunch of files had to be modified.

  Revision 1.14  2002/01/02 15:24:58  lazarus
  MG: added TCanvas.Polygon and TCanvas.Polyline

  Revision 1.13  2001/11/12 16:56:07  lazarus
  MG: CLIPBOARD

  Revision 1.12  2001/11/01 18:48:52  lazarus
  Changed Application.Messagebox to use TMessageBox class.
  Added icon images for mtError and mtConfirmation
  Shane

  Revision 1.11  2001/10/31 21:43:28  lazarus
  Added code for TApplication to get it ready to accept exceptions.
  Shane

  Revision 1.10  2001/09/30 08:34:49  lazarus
  MG: fixed mem leaks and fixed range check errors

  Revision 1.9  2001/06/20 13:35:51  lazarus
  MG: added VK_IRREGULAR and key grabbing

  Revision 1.8  2001/06/15 10:31:06  lazarus
  MG: set longstrings as default

  Revision 1.7  2001/04/06 22:28:09  lazarus
  * TTimer uses winapi interface now instead of sendmessage interface, stoppok

  Revision 1.6  2001/03/26 14:58:31  lazarus
  MG: setwindowpos + bugfixes

  Revision 1.5  2001/02/01 16:45:19  lazarus
  Started the code completion.
  Shane

  Revision 1.4  2000/09/10 23:08:30  lazarus
  MWE:
    + Added CreateCompatibeleBitamp function
    + Updated TWinControl.WMPaint
    + Added some checks to avoid gtk/gdk errors
    - Removed no fixed warning from GetDC
    - Removed some output

  Revision 1.3  2000/08/11 14:59:09  lazarus
  Adding all the Synedit files.
  Changed the GDK_KEY_PRESS and GDK_KEY_RELEASE stuff to fix the problem in the editor with the shift key being ignored.
  Shane

  Revision 1.1  2000/07/13 10:28:24  michael
  + Initial import
}
