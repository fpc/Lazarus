// included by graphics.pp

{******************************************************************************
                                  TBITMAPCANVAS
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
  Method: TBitMapCanvas.Create
  Params:  ABitMap: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TBitMapCanvas.Create(ABitmap : TBitMap);
begin
  inherited Create;
  FBitmap := ABitmap;
end;

{------------------------------------------------------------------------------
  Method:  TBitMapCanvas.CreateHandle
  Params:  None
  Returns: Nothing

  Creates the handle ( = object).
 ------------------------------------------------------------------------------}
procedure TBitMapCanvas.CreateHandle;
var
  DC: HDC;
begin
  if FBitmap <> nil then
  begin
    FBitmap.HandleNeeded;
    FreeDC;
    FBitmap.PaletteNeeded;
    DC := CreateCompatibleDC(0);

    Assert(False, Format('trace:[TBitmapCanvas.CreateHandle] Got Handle 0x%x', [FBitmap.Handle]));

    if FBitmap.Handle = 0 then
      FOldBitmap := 0
    else
      FOldBitmap := SelectObject(DC, FBitmap.Handle);

    if FBitmap.FPalette = 0 then
      FOldPalette := 0
    else begin
      FOldPalette := SelectPalette(DC, FBitmap.FPalette, True);
      RealizePalette(DC);
    end;

    Handle := DC;
  end;
end;

{------------------------------------------------------------------------------
  Method: TBitMapCanvas.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TBitMapCanvas.Destroy;
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
  hBmp: HBITMAP;
begin
  if FHandle <> 0 then
  begin
    if FOldBitmap <> 0 then begin
      SelectObject(FHandle, FOldBitmap);
      FOldBitmap:=0;
    end;
    if FOldPalette <> 0 then begin
      SelectPalette(FHandle, FOldPalette, True);
      FOldPalette:=0;
    end;
    hBmp := FHandle;
    Handle := 0;
    DeleteDC(hBmp);
  end;
end;

// included by graphics.pp

{ =============================================================================

  $Log$
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

