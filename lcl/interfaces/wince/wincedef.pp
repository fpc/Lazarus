{ $Id: wincedef.pp 9243 2006-05-05 05:52:08Z mattias $
                         ------------------------------
                         gtkdef.pp  -  Type definitions
                         ------------------------------

 @created(Wed Jan 24st WET 2001)
 @lastmod($Date: 2006-05-05 09:22:08 +0330 (Fri, 05 May 2006) $)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)

 This unit contains type definitions needed in the Windows <-> LCL interface

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

Unit WinCEDef;

{$mode objfpc}{$H+}

Interface

Uses
  Windows, Classes, LCLType;

Const
  // Used by TCalendar
  MCM_FIRST = $1000;
  MCM_GETCURSEL = MCM_FIRST + 1;
  MCM_SETCURSEL =  MCM_FIRST + 2;
  MCM_GETMINREQRECT = MCM_FIRST + 9;

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
  PWinCETimerInfo = ^TWinCETimerinfo;
  TWinCETimerInfo = record
    TimerID: UINT;         // the windows timer ID for this timer
    TimerFunc: TFNTimerProc; // owner function to handle timer
  end;

var
  // FTimerData contains the currently running timers
  FTimerData : TList;   // list of PWin32Timerinfo

Implementation

End.
