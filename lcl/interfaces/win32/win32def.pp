{ $Id$
                         ------------------------------
                         gtkdef.pp  -  Type definitions
                         ------------------------------

 @created(Wed Jan 24st WET 2001)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)

 This unit contains type definitions needed in the Windows <-> LCL interface

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

Unit Win32Def;

{$mode objfpc}{$H+}

Interface

Uses
  Windows, Classes, LCLType;

Const
  // Used by TCalendar
  MCM_FIRST = $1000;
  MCM_GETCURSEL = MCM_FIRST + 1;
  MCM_SETCURSEL =  MCM_FIRST + 2;

Type
  TGDIType = (gdiBitmap, gdiBrush, gdiFont, gdiPen, gdiRegion);
  TGDIBitmapType = (gbBitmap, gbPixmap, gbImage);

  PGDIRGB = ^TGDIRGB;
  TGDIRGB = Record
    Red,
    Green,
    Blue: Byte;
  End;

  PGDIRawImage = ^TGDIRawImage;
  TGDIRawImage = Record
    Height,
    Width: Integer;
    Depth: Byte;
    Data: Array[0..0] Of TGDIRGB;
  End;

  PGDIObject = ^TGDIObject;
  TGDIObject = Record
    Case GDIType: TGDIType Of
      gdiBitmap:
      (
        GDIBitmapMaskObject: HICON;
        Case GDIBitmapType: TGDIBitmapType Of
          gbBitmap: (GDIBitmapObject: HBITMAP);
          gbPixmap: (GDIPixmapObject: HICON);
          gbImage : (GDIRawImageObject: PGDIRawImage);
      );
      gdiBrush:
      (
        GDIBrushColor: COLORREF;
        GDIBrushFill: COLORREF;
        GDIBrushPixMap: HICON;
      );
      gdiFont:
      (
        GDIFontObject: HFONT;
        LogFont: TLogFont;
      );
      gdiPen:
      (
        GDIPenColor: COLORREF;
        GDIPenWidth: Integer;
        GDIPenStyle: Word;
      );
      gdiRegion:
      (
      );
  End;

  // move to class ??
  PDeviceContext = ^TDeviceContext;
  { Stored record of Device contexts and related GDI information }
  TDeviceContext = Record
    hWnd: HWND;
    GC: HDC;
    Drawable: PHANDLE;
    PenPos: TPoint;
    CurrentBitmap: PGdiObject;
    CurrentFont: PGdiObject;
    CurrentPen: PGdiObject;
    CurrentBrush: PGdiObject;
    CurrentTextColor: COLORREF;
    CurrentBackColor: COLORREF;
    SavedContext: PDeviceContext; // linked list of saved DCs
  End;

  PWinControlInfo = ^TWinControlInfo;
  { Info needed by the API of a HWND }
  TWinControlInfo = Record
    ImplementationControl: HWND; // used to be "fixed" or "core-child"
    UpdateRect: TRect; // used by LM_Paint, beginpaint etc
    WndProc: Integer; // window data
    Style: Integer;
    ExStyle: Integer;
    UserData: Integer;
  End;

  { lazarus win32 Interface definition for additional timer data needed to find the callback}
  PWin32TimerInfo = ^TWin32Timerinfo;
  TWin32TimerInfo = record
    TimerID: UINT;         // the windows timer ID for this timer
    TimerFunc: TFNTimerProc; // owner function to handle timer
  end;

  // In the way that ScrollWindow is implemented at Windows unit
  // It's not possible to pass a pointer as argument
  // which prevents the use of nil
  function ScrollWindow(hWnd:HWND; XAmount:longint; YAmount:longint;lpRect,lpClipRect:LPRECT):WINBOOL; external 'user32' name 'ScrollWindow';
var
  // FTimerData contains the currently running timers
  FTimerData : TList;   // list of PWin32Timerinfo

Implementation

End.

{
  $Log$
  Revision 1.11  2004/08/18 09:31:21  mattias
  removed obsolete unit vclglobals

  Revision 1.10  2003/12/29 14:22:22  micha
  fix a lot of range check errors win32

  Revision 1.9  2003/10/02 11:18:09  mattias
  clean ups from Karl

  Revision 1.8  2003/09/27 09:52:44  mattias
  TScrollBox for win32 intf from Karl

  Revision 1.7  2003/09/08 12:21:48  mattias
  added fpImage reader/writer hooks to TBitmap

  Revision 1.6  2003/08/17 12:26:00  mattias
  fixed parts of the win32 intf size system

  Revision 1.5  2002/11/23 13:48:48  mattias
  added Timer patch from Vincent Snijders

  Revision 1.4  2002/05/10 07:43:48  lazarus
  MG: updated licenses

  Revision 1.3  2002/01/05 13:16:09  lazarus
  MG: win32 interface update from Keith Bowes

  Revision 1.2  2001/11/01 22:40:13  lazarus
  MG: applied Keith Bowes win32 interface updates

  Revision 1.1  2001/08/02 12:58:35  lazarus
  MG: win32 interface patch from Keith Bowes

}
