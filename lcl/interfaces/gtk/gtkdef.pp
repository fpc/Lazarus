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


unit gtkdef;
 
{$mode objfpc} 

interface

uses
  gtk, gdk, LCLLinux, VclGlobals, Classes;

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
        GDIBitmapMaskObject: PGdkPixmap;
        case GDIBitmapType: TGDIBitmapType of
          gbBitmap: (GDIBitmapObject: PGdkBitmap); 
          gbPixmap: (GDIPixmapObject: PGdkPixmap);
          gbImage : (GDIRawImageObject: PGDIRawImage);
      );
      gdiBrush: ( 
        GDIBrushColor: TGdkColor;
        GDIBrushFill: TGdkFill;
        GDIBrushPixMap: PGdkPixmap;
      ); 
      gdiFont: (
        GDIFontObject: PGdkFont;
        LogFont: TLogFont;  // for now font info is stored as well, later query font params
      ); 
      gdiPen: (
        GDIPenColor: TGdkColor;
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
    GC: pgdkGC;
    Drawable: PGDKDrawable;
    PenPos: TPoint;
    CurrentBitmap: PGdiObject;
    CurrentFont: PGdiObject;
    CurrentPen: PGdiObject;
    CurrentBrush: PGdiObject;
    CurrentTextColor: TGdkColor;
    CurrentBackColor: TGdkColor;
    SavedContext: PDeviceContext; // linked list of saved DCs
  end;

  // Info needed by the API of a HWND (=Widget) 
  PWinWidgetInfo = ^TWinWidgetInfo;
  TWinWidgetInfo = record
    ImplementationWidget: PGTKWidget; // used to be "fixed" or "core-child"
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
  Revision 1.2  2001/01/25 21:38:57  lazarus
  MWE:
    * fixed lil bug I commetted yesterday (listbox crash)

  Revision 1.1  2001/01/24 23:26:40  lazarus
  MWE:
    = moved some types to gtkdef
    + added WinWidgetInfo
    + added some initialization to Application.Create

}
