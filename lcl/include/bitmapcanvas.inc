{%MainUnit ../graphics.pp}

{******************************************************************************
                                  TBitmapCanvas
 ******************************************************************************

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
{------------------------------------------------------------------------------
  Method: TBitmapCanvas.Create
  Params:  ABitMap: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TBitmapCanvas.Create(ABitmap : TBitMap);
begin
  inherited Create;
  FBitmap := ABitmap;
end;

{------------------------------------------------------------------------------
  Method:  TBitmapCanvas.CreateHandle
  Params:  None
  Returns: Nothing

  Creates the handle.
 ------------------------------------------------------------------------------}
procedure TBitmapCanvas.CreateHandle;
var
  DC: HDC;
begin
  if HandleAllocated then exit;
  if FBitmap = nil then exit;
  FBitmap.HandleNeeded;
  FBitmap.PaletteNeeded;
  DC := CreateCompatibleDC(0);

  FOldBitmap := 0;
  FOldBitmapValid:=false;
  if FBitmap.HandleAllocated then begin
    FOldBitmap := SelectObject(DC, FBitmap.Handle);
    FOldBitmapValid:=true;
  end;

  FOldPalette:=0;
  FOldPaletteValid:=false;
  if FBitmap.PaletteAllocated then begin
    FOldPalette := SelectPalette(DC, FBitmap.FPalette, True);
    FOldPaletteValid:=true;
    RealizePalette(DC);
  end;

  Handle := DC;
  //DebugLn('TBitmapCanvas.CreateHandle END Self=',DbgS(Self),' DC=',DbgS(DC),
  //  ' Handle=',DbgS(GetUpdatedHandle([csHandleValid])));
end;

procedure TBitmapCanvas.DeselectHandles;
begin
  if HandleAllocated then begin
    if FOldBitmapValid then begin
      SelectObject(FHandle, FOldBitmap);
      FOldBitmap:=0;
      FOldBitmapValid:=false;
    end;
    if FOldPaletteValid then begin
      SelectPalette(FHandle, FOldPalette, True);
      FOldPalette:=0;
      FOldPaletteValid:=false;
    end;
  end;
  inherited DeselectHandles;
end;

{------------------------------------------------------------------------------
  Method: TBitmapCanvas.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TBitmapCanvas.Destroy;
begin
  FreeDC;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method:  TControlCanvas.FreeDC
  Params:  None
  Returns: Nothing

  Frees the device context
 ------------------------------------------------------------------------------}
procedure TBitmapCanvas.FreeDC;
var
  OldHandle: HBITMAP;
begin
  if not HandleAllocated then exit;
  //DebugLn('TBitmapCanvas.FreeDC START Self=',DbgS(Self),' FBitmap=',DbgS(FBitmap));
  if FBitmap<>nil then begin
    OldHandle := FHandle;
    Handle := 0;
    DeleteDC(OldHandle);
  end else begin
    Handle:=0;
  end;
end;

// included by graphics.pp

{ =============================================================================

  $Log$
  Revision 1.14  2005/03/07 21:59:44  vincents
  changed hexstr(cardinal()) for pointers to dbgs() and other 64-bits fixes   from Peter Vreman

  Revision 1.13  2004/10/01 13:16:44  mattias
  fixed unselecting TCanvas objects

  Revision 1.12  2004/05/11 12:16:47  mattias
  replaced writeln by debugln

  Revision 1.11  2004/04/10 17:58:56  mattias
  implemented mainunit hints for include files

  Revision 1.10  2004/03/06 15:37:43  mattias
  fixed FreeDC

  Revision 1.9  2004/03/02 22:37:36  mattias
  clean up for TBitmapImage sharing

  Revision 1.8  2004/02/05 16:28:38  mattias
  fixed unsharing TBitmap

  Revision 1.7  2003/06/30 10:09:46  mattias
  fixed Get/SetPixel for DC without widget

  Revision 1.6  2002/09/13 16:58:27  lazarus
  MG: removed the 1x1 bitmap from TBitBtn

  Revision 1.5  2002/06/01 08:41:28  lazarus
  MG: DrawFramControl now uses gtk style, transparent STrechBlt

  Revision 1.4  2002/05/10 06:05:51  lazarus
  MG: changed license to LGPL

  Revision 1.3  2001/03/19 14:00:50  lazarus
  MG: fixed many unreleased DC and GDIObj bugs

  Revision 1.2  2000/09/10 23:08:30  lazarus
  MWE:
    + Added CreateCompatibeleBitamp function
    + Updated TWinControl.WMPaint
    + Added some checks to avoid gtk/gdk errors
    - Removed no fixed warning from GetDC
    - Removed some output

  Revision 1.1  2000/07/13 10:28:24  michael
  + Initial import

  Revision 1.2  2000/07/09 20:18:56  lazarus
  MWE:
    + added new controlselection
    + some fixes
    ~ some cleanup

  Revision 1.1  2000/04/02 20:49:55  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.5  2000/03/21 23:47:33  lazarus
  MWE:
    + Added TBitmap.MaskHandle & TGraphic.Draw & TBitmap.Draw

  Revision 1.4  2000/03/06 00:05:05  lazarus
  MWE: Added changes from Peter Dyson <peter@skel.demon.co.uk> for a new
    release of mwEdit (0.92)

  Revision 1.3  1999/11/19 01:09:43  lazarus
  MWE:
    implemented TCanvas.CopyRect
    Added StretchBlt
    Enabled creation of TCustomControl.Canvas
    Added a temp hack in TWinControl.Repaint to get a LM_PAINT

  Revision 1.2  1999/11/17 01:16:39  lazarus
  MWE:
    Added some more API stuff
    Added an initial TBitmapCanvas
    Added some DC stuff
    Changed and commented out, original gtk linedraw/rectangle code. This
      is now called through the winapi wrapper.

  Revision 1.1  1999/08/13 19:51:07  lazarus
  Minor changes for compatability made.


}

