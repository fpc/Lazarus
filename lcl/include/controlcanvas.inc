{******************************************************************************
                                     TControlCanvas
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
  Method:  TControlCanvas.SetControl
  Params:  AControl: The control this canvas belongs to
  Returns: Nothing

  Sets the owner of this canvas
 ------------------------------------------------------------------------------}
Procedure TControlCanvas.SetControl(AControl: TControl);
begin
  if FControl <> AControl then
  begin
    FreeHandle;
    FControl := AControl;
  end;
end;

{------------------------------------------------------------------------------
  procedure TControlCanvas.CreateFont;
 ------------------------------------------------------------------------------}
procedure TControlCanvas.CreateFont;
begin
  inherited CreateFont;
  //writeln('TControlCanvas.CreateFont A ',ClassName,' Control=',Control.Name,':',Control.ClassName,' ',Font.Name,' ',Font.Height);
  //if Control.ClassName='TSpeedButton' then RaiseGDBException('ARRGH');
end;

{------------------------------------------------------------------------------
  Method: TControlCanvas.Create
  Params:  none
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TControlCanvas.Create;
begin
  inherited Create;
  FDeviceContext := 0;
  FControl := nil;
  FWindowHandle := 0;
end;

{------------------------------------------------------------------------------
  Method: TControlCanvas.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TControlCanvas.Destroy;
begin
  FreeHandle;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method:  TControlCanvas.CreateHandle
  Params:  None
  Returns: Nothing

  Creates the handle ( = object).
 ------------------------------------------------------------------------------}
procedure TControlCanvas.CreateHandle;
begin
//writeln('[TControlCanvas.CreateHandle] ',FControl<>nil,' DC=',HexStr(FDeviceContext,8),' WinHandle=',HexStr(FWindowHandle,8));
  if FControl = nil
  then inherited CreateHandle
  else begin
    if FDeviceContext = 0 then
    begin
      // store the handle locally since  we need it to check (and dont
      // want to fire creation events)
      FDeviceContext := FControl.GetDeviceContext(FWindowHandle);
    end;
    Handle := FDeviceContext;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TControlCanvas.FreeHandle
  Params:  None
  Returns: Nothing

  Frees the handle
 ------------------------------------------------------------------------------}
procedure TControlCanvas.FreeHandle;
begin
  if FDeviceContext <> 0 then
  begin
    Handle := 0;
    ReleaseDC(FWindowHandle, FDeviceContext);
    FDeviceContext := 0;
  end;
end;

{ =============================================================================

  $Log$
  Revision 1.5  2003/04/02 13:23:23  mattias
  fixed default font

  Revision 1.4  2002/05/10 06:05:51  lazarus
  MG: changed license to LGPL

  Revision 1.3  2001/10/07 07:28:33  lazarus
  MG: fixed setpixel and TCustomForm.OnResize event

  Revision 1.2  2001/03/19 14:00:50  lazarus
  MG: fixed many unreleased DC and GDIObj bugs

  Revision 1.1  2000/07/13 10:28:25  michael
  + Initial import

  Revision 1.1  2000/04/02 20:49:56  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.8  2000/03/06 00:05:05  lazarus
  MWE: Added changes from Peter Dyson <peter@skel.demon.co.uk> for a new
    release of mwEdit (0.92)

  Revision 1.7  1999/11/17 01:16:39  lazarus
  MWE:
    Added some more API stuff
    Added an initial TBitmapCanvas
    Added some DC stuff
    Changed and commented out, original gtk linedraw/rectangle code. This
      is now called through the winapi wrapper.

  Revision 1.6  1999/11/01 01:28:29  lazarus
  MWE: Implemented HandleNeeded/CreateHandle/CreateWND
       Now controls are created on demand. A call to CreateComponent shouldn't
       be needed. It is now part of CreateWnd

  Revision 1.5  1999/10/28 17:17:41  lazarus
  Removed references to FCOmponent.
  Shane

  Revision 1.4  1999/08/07 17:59:13  lazarus

        buttons.pp   the DoLeave and DoEnter were connected to the wrong
                     event.

        The rest were modified to use the new SendMessage function.   MAH

  Revision 1.3  1999/07/31 06:39:18  lazarus

       Modified the IntSendMessage3 to include a data variable. It isn't used
       yet but will help in merging the Message2 and Message3 features.

       Adjusted TColor routines to match Delphi color format

       Added a TGdkColorToTColor routine in gtkproc.inc

       Finished the TColorDialog added to comDialog example.        MAH

 }
