{ $Id$
                         ------------------------------ 
                         gtkdef.pp  -  Type definitions
                         ------------------------------ 
 
 @created(Wed Jan 24st WET 2001)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)                       

 This unit contains type definitions needed in the GTK <-> LCL interface
 
/*************************************************************************** 
 *                                                                         * 
 *   This program is free software; you can redistribute it and/or modify  * 
 *   it under the terms of the GNU General Public License as published by  * 
 *   the Free Software Foundation; either version 2 of the License, or     * 
 *   (at your option) any later version.                                   * 
 *                                                                         * 
 ***************************************************************************/ 
 } 


unit win32def;
 
{$mode objfpc} 
{$LONGSTRINGS ON}


interface

uses
  Windows, VclGlobals, Classes;

type
  TGDIType = (gdiBitmap, gdiBrush, gdiFont, gdiPen, gdiRegion);
  TGDIBitmapType = (gbBitmap, gbPixmap, gbImage);
  
  PGDIRGB = ^TGDIRGB;
  TGDIRGB = record
    Red,
    Green,
    Blue: Byte;
  end;

  PGDIRawImage = ^TGDIRawImage;
  TGDIRawImage = record
    Height,
    Width: Integer;
    Depth: Byte;
    Data: array[0..0] of TGDIRGB;
  end;
  
  PGDIObject = ^TGDIObject;
  TGDIObject = record
    case GDIType: TGDIType of
      gdiBitmap: (
        GDIBitmapMaskObject: HICON {PGdkPixmap};
        case GDIBitmapType: TGDIBitmapType of
          gbBitmap: (GDIBitmapObject: HBITMAP {PGdkBitmap}); 
          gbPixmap: (GDIPixmapObject: HICON {PGdkPixmap});
          gbImage : (GDIRawImageObject: PGDIRawImage);
      );
      gdiBrush: ( 
        GDIBrushColor: COLORREF {TGdkColor};
        GDIBrushFill: COLORREF {TGdkFill};
        GDIBrushPixMap: HICON {PGdkPixmap};
      ); 
      gdiFont: (
        GDIFontObject: HFONT {PGdkFont};
        LogFont: TLogFont;  // for now font info is stored as well, later query font params
      ); 
      gdiPen: (
        GDIPenColor: COLORREF {TGdkColor};
        GDIPenWidth: Integer;
        GDIPenStyle: Word;
      ); 
      gdiRegion: (
      ); 
  end;


  // move to class ??
  PDeviceContext = ^TDeviceContext;
  TDeviceContext = record
    hWnd: HWND;
    GC: HDC {pgdkGC}; // Not sure of Win32 equiv.
    Drawable: PHANDLE {PGDKDrawable}; // Not sure of Win32 equiv.
    PenPos: TPoint;
    CurrentBitmap: PGdiObject;
    CurrentFont: PGdiObject;
    CurrentPen: PGdiObject;
    CurrentBrush: PGdiObject;
    CurrentTextColor: COLORREF {TGdkColor};
    CurrentBackColor: COLORREF {TGdkColor};
    SavedContext: PDeviceContext; // linked list of saved DCs
  end;

  // Info needed by the API of a HWND (=Widget) 
  PWinControlInfo = ^TWinControlInfo;
  TWinControlInfo = record
    ImplementationWidget: HWND; // used to be "fixed" or "core-child"
    UpdateRect: TRect;                // used by LM_Paint, beginpaint etc
    WndProc: Integer;                 // window data 
    Style: Integer;                   
    ExStyle: Integer;
    UserData: Integer;
  end;
  

implementation

end.

{ =============================================================================

  $Log$
  Revision 1.2  2001/11/01 22:40:13  lazarus
  MG: applied Keith Bowes win32 interface updates

  Revision 1.1  2001/08/02 12:58:35  lazarus
  MG: win32 interface patch from Keith Bowes

  Revision 1.3  2001/03/27 21:12:54  lazarus
  MWE:
    + Turned on longstrings
    + modified memotest to add lines

  Revision 1.2  2001/01/25 21:38:57  lazarus
  MWE:
    * fixed lil bug I commetted yesterday (listbox crash)

  Revision 1.1  2001/01/24 23:26:40  lazarus
  MWE:
    = moved some types to gtkdef
    + added WinWidgetInfo
    + added some initialization to Application.Create

}
