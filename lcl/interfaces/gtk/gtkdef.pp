{ $Id$
                         ------------------------------ 
                         gtkdef.pp  -  Type definitions
                         ------------------------------ 
 
 @created(Wed Jan 24st WET 2001)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)                       

 This unit contains type definitions needed in the GTK <-> LCL interface
 
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


unit GtkDef;
 
{$mode objfpc} 
{$LONGSTRINGS ON}


interface

uses
  gtk, gdk, LCLLinux, LCLType, VclGlobals, Classes, LCLMemManager;

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
    Next: PGDIObject; // 'Next' is used by the internal mem manager
    case GDIType: TGDIType of
      gdiBitmap: (
        GDIBitmapMaskObject: PGdkPixmap;
        Visual : PGDKVisual;
        Colormap : PGDKColormap;
        case GDIBitmapType: TGDIBitmapType of
          gbBitmap: (GDIBitmapObject: PGdkBitmap); // pixmap with depth 1
          gbPixmap: (GDIPixmapObject: PGdkPixmap); // normal pixmap
          gbImage : (GDIRawImageObject: PGDIRawImage);
      );
      gdiBrush: ( 
        // ToDo: add bitmap mask
        GDIBrushColor: TGdkColor;
        GDIBrushFill: TGdkFill;
        GDIBrushPixMap: PGdkPixmap;
      );
      gdiFont: (
        GDIFontObject: PGdkFont;
        LogFont: TLogFont;// for now font info is stored as well, later query font params
      ); 
      gdiPen: (
        GDIPenColor: TGdkColor;
        GDIPenWidth: Integer;
        GDIPenStyle: Word;
      ); 
      gdiRegion: (
        GDIRegionObject: PGdkRegion;
      );
  end;


  // move to class ??
  PDeviceContext = ^TDeviceContext;
  TDeviceContext = record
    hWnd: HWND; 
    GC: pgdkGC;
    Drawable: PGDKDrawable;
    Origin: TPoint;
    SpecialOrigin: boolean;
    PenPos: TPoint;
    CurrentBitmap: PGdiObject;
    CurrentFont: PGdiObject;
    CurrentPen: PGdiObject;
    CurrentBrush: PGdiObject;
    CurrentTextColor: TGdkColor;
    CurrentBackColor: TGdkColor;
    ClipRegion : hRGN;
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
    EventMask: TGdkEventMask;
  end;
  
// clipboard
type
  TClipboardEventData = record
    TimeID: Cardinal;
    Data: TGtkSelectionData;
  end;
  PClipboardEventData = ^TClipboardEventData;
  
  TGtkClipboardFormat = (
    gfCLASS, gfCOMPOUND_TEXT, gfDELETE, gfFILE_NAME, gfHOST_NAME, gfLENGTH,
    gfMULTIPLE, gfNAME, gfOWNER_OS, gfPROCESS, gfSTRING, gfTARGETS, gfTEXT,
    gfTIMESTAMP, gfUSER);
    
  TGtkClipboardFormats = set of TGtkClipboardFormat;
    
const
  GtkClipboardFormatName: array[TGtkClipboardFormat] of string = (
      'CLASS', 'COMPOUND_TEXT', 'DELETE', 'FILE_NAME', 'HOST_NAME', 'LENGTH',
      'MULTIPLE', 'NAME', 'OWNER_OS', 'PROCESS', 'STRING', 'TARGETS', 'TEXT',
      'TIMESTAMP', 'USER'
    );
  

function NewPGDIObject: PGDIObject;
procedure DisposePGDIObject(GDIObject: PGdiObject);

function NewPDeviceContext: PDeviceContext;
procedure DisposePDeviceContext(DeviceContext: PDeviceContext);


implementation


{$IFOpt R+}{$Define RangeChecksOn}{$Endif}

// memory system for PGDIObject(s) ---------------------------------------------
type
  TGDIObjectMemManager = class(TLCLMemManager)
  protected
    procedure FreeFirstItem; override;
  public
    procedure DisposeGDIObject(AGDIObject: PGDIObject);
    function NewGDIObject: PGDIObject;
  end;
  
const
  GDIObjectMemManager: TGDIObjectMemManager = nil;

function NewPGDIObject: PGDIObject;
begin
  if GDIObjectMemManager=nil then begin
    GDIObjectMemManager:=TGDIObjectMemManager.Create;
    GDIObjectMemManager.MinimumFreeCount:=1000;
  end;
  Result:=GDIObjectMemManager.NewGDIObject;
end;

procedure DisposePGDIObject(GDIObject: PGdiObject);
begin
  GDIObjectMemManager.DisposeGDIObject(GDIObject);
end;

{ TGDIObjectMemManager }

procedure TGDIObjectMemManager.FreeFirstItem;
var AGDIObject: PGDIObject;
begin
  AGDIObject:=PGDIObject(FFirstFree);
  PGDIObject(FFirstFree):=AGDIObject^.Next;
  Dispose(AGDIObject);
  //writeln('TGDIObjectMemManager.DisposeGDIObject A FFreedCount=',FFreedCount);
  {$R-}
  inc(FFreedCount);
  {$IfDef RangeChecksOn}{$R+}{$Endif}
end;

procedure TGDIObjectMemManager.DisposeGDIObject(AGDIObject: PGDIObject);
begin
  if (FFreeCount<FMinFree) or (FFreeCount<((FCount shr 3)*FMaxFreeRatio)) then
  begin
    // add AGDIObject to Free list
    AGDIObject^.Next:=PGDIObject(FFirstFree);
    PGDIObject(FFirstFree):=AGDIObject;
    inc(FFreeCount);
  end else begin
    // free list full -> free the ANode
    Dispose(AGDIObject);
    //writeln('TGDIObjectMemManager.DisposeGDIObject B FFreedCount=',FFreedCount);
    {$R-}
    inc(FFreedCount);
    {$IfDef RangeChecksOn}{$R+}{$Endif}
  end;
  dec(FCount);
end;

function TGDIObjectMemManager.NewGDIObject: PGDIObject;
begin
  if FFirstFree<>nil then begin
    // take from free list
    Result:=PGDIObject(FFirstFree);
    PGDIObject(FFirstFree):=Result^.Next;
    dec(FFreeCount);
  end else begin
    // free list empty -> create new node
    New(Result);
    // writeln('TGDIObjectMemManager.NewGDIObject FAllocatedCount=',FAllocatedCount);
    {$R-}
    inc(FAllocatedCount);
    {$IfDef RangeChecksOn}{$R+}{$Endif}
  end;
  FillChar(Result^, SizeOf(TGDIObject), 0);
  inc(FCount);
end;


// memory system for PDeviceContext(s) ---------------------------------------------
type
  TDeviceContextMemManager = class(TLCLMemManager)
  protected
    procedure FreeFirstItem; override;
  public
    procedure DisposeDeviceContext(ADeviceContext: PDeviceContext);
    function NewDeviceContext: PDeviceContext;
  end;

const
  DeviceContextMemManager: TDeviceContextMemManager = nil;

function NewPDeviceContext: PDeviceContext;
begin
  if DeviceContextMemManager=nil then begin
    DeviceContextMemManager:=TDeviceContextMemManager.Create;
    DeviceContextMemManager.MinimumFreeCount:=1000;
  end;
  Result:=DeviceContextMemManager.NewDeviceContext;
end;

procedure DisposePDeviceContext(DeviceContext: PDeviceContext);
begin
  DeviceContextMemManager.DisposeDeviceContext(DeviceContext);
end;

{ TDeviceContextMemManager }

procedure TDeviceContextMemManager.FreeFirstItem;
var ADeviceContext: PDeviceContext;
begin
  ADeviceContext:=PDeviceContext(FFirstFree);
  PDeviceContext(FFirstFree):=ADeviceContext^.SavedContext;
  //writeln('TDeviceContextMemManager.FreeFirstItem FFreedCount=',FFreedCount);
  Dispose(ADeviceContext);
  {$R-}
  inc(FFreedCount);
  {$IfDef RangeChecksOn}{$R+}{$Endif}
end;

procedure TDeviceContextMemManager.DisposeDeviceContext(
  ADeviceContext: PDeviceContext);
begin
  if (FFreeCount<FMinFree) or (FFreeCount<((FCount shr 3)*FMaxFreeRatio)) then
  begin
    // add ADeviceContext to Free list
    ADeviceContext^.SavedContext:=PDeviceContext(FFirstFree);
    PDeviceContext(FFirstFree):=ADeviceContext;
    inc(FFreeCount);
  end else begin
    // free list full -> free the ANode
    //writeln('TDeviceContextMemManager.DisposeDeviceContext FFreedCount=',FFreedCount);
    Dispose(ADeviceContext);
    {$R-}
    inc(FFreedCount);
    {$IfDef RangeChecksOn}{$R+}{$Endif}
  end;
  dec(FCount);
end;

function TDeviceContextMemManager.NewDeviceContext: PDeviceContext;
begin
  if FFirstFree<>nil then begin
    // take from free list
    Result:=PDeviceContext(FFirstFree);
    PDeviceContext(FFirstFree):=Result^.SavedContext;
    dec(FFreeCount);
  end else begin
    // free list empty -> create new node
    New(Result);
    //writeln('TDeviceContextMemManager.NewDeviceContext FAllocatedCount=',FAllocatedCount);
    {$R-}
    inc(FAllocatedCount);
    {$IfDef RangeChecksOn}{$R+}{$Endif}
  end;
  FillChar(Result^, SizeOf(TDeviceContext), 0);
  inc(FCount);
end;


//------------------------------------------------------------------------------
finalization
  GDIObjectMemManager.Free;
  GDIObjectMemManager:=nil;
  DeviceContextMemManager.Free;
  DeviceContextMemManager:=nil;

end.

{ =============================================================================

  $Log$
  Revision 1.17  2002/09/10 06:49:20  lazarus
  MG: scrollingwincontrol from Andrew

  Revision 1.16  2002/09/06 16:38:25  lazarus
  MG: added GetDCOffset

  Revision 1.15  2002/08/30 12:32:22  lazarus
  MG: MoveWindowOrgEx, Splitted FWinControls/FControls, TControl drawing, Better DesignerDrawing, ...

  Revision 1.14  2002/08/21 14:44:18  lazarus
  MG: accelerated synedit

  Revision 1.13  2002/08/21 14:06:40  lazarus
  MG: added TDeviceContextMemManager

  Revision 1.12  2002/08/21 10:46:37  lazarus
  MG: fixed unreleased gdiRegions

  Revision 1.11  2002/08/21 08:13:37  lazarus
  MG: accelerated new/dispose of GdiObjects

  Revision 1.10  2002/08/15 15:46:49  lazarus
  MG: added changes from Andrew (Clipping)

  Revision 1.9  2002/06/09 14:00:41  lazarus
  MG: fixed persistent caret and implemented Form.BorderStyle=bsNone

  Revision 1.8  2002/06/04 15:17:23  lazarus
  MG: improved TFont for XLFD font names

  Revision 1.7  2002/06/01 08:41:28  lazarus
  MG: DrawFramControl now uses gtk style, transparent STrechBlt

  Revision 1.6  2002/05/10 06:05:56  lazarus
  MG: changed license to LGPL

  Revision 1.5  2002/02/03 00:24:01  lazarus
  TPanel implemented.
  Basic graphic primitives split into GraphType package, so that we can
  reference it from interface (GTK, Win32) units.
  New Frame3d canvas method that uses native (themed) drawing (GTK only).
  New overloaded Canvas.TextRect method.
  LCLLinux and Graphics was split, so a bunch of files had to be modified.

  Revision 1.4  2001/11/12 16:56:08  lazarus
  MG: CLIPBOARD

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
