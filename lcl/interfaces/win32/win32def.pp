{ $Id$
                         ------------------------------ 
                         gtkdef.pp  -  Type definitions
                         ------------------------------ 

 @created(Wed Jan 24st WET 2001)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)                       

 This unit contains type definitions needed in the Windows <-> LCL interface

/*************************************************************************** 
 *                                                                         * 
 *   This program is free software; you can redistribute it and/or modify  * 
 *   it under the terms of the GNU General Public License as published by  * 
 *   the Free Software Foundation; either version 2 of the License, or     * 
 *   (at your option) any later version.                                   * 
 *                                                                         * 
 ***************************************************************************/ 
 }

Unit Win32Def;

{$MODE OBJFPC}
{$LONGSTRINGS ON}

Interface 

Uses
  Windows, VCLGlobals, Classes;

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

Implementation 

End.

{ =============================================================================

  $Log$
  Revision 1.3  2002/01/05 13:16:09  lazarus
  MG: win32 interface update from Keith Bowes

  Revision 1.2  2001/11/01 22:40:13  lazarus
  MG: applied Keith Bowes win32 interface updates

  Revision 1.1  2001/08/02 12:58:35  lazarus
  MG: win32 interface patch from Keith Bowes

}
