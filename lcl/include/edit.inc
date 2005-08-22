{%MainUnit ../stdctrls.pp}

{******************************************************************************
                                     TEdit
                                base found in stdctrls.pp
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

// included by stdctrls.pp

{
  $Log$
  Revision 1.4  2004/04/10 17:58:57  mattias
  implemented mainunit hints for include files

  Revision 1.3  2002/05/10 06:05:52  lazarus
  MG: changed license to LGPL

  Revision 1.2  2002/04/18 08:09:03  lazarus
  MG: added include comments

  Revision 1.1  2000/07/13 10:28:25  michael
  + Initial import

  Revision 1.1  2000/04/02 20:49:56  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.7  2000/02/24 21:15:30  lazarus
  Added TCustomForm.GetClientRect and RequestAlign to try and get the controls to align correctly when a MENU is present.  Not Complete yet.

  Fixed the bug in TEdit that caused it not to update it's text property.  I will have to
  look at TMemo to see if anything there was affected.

  Added SetRect to WinAPI calls
  Added AdjustWindowRectEx to WINAPI calls.
  Shane

  Revision 1.6  2000/02/21 17:38:04  lazarus
  Added modalresult to TCustomForm
  Added a View Units dialog box
  Added a View Forms dialog box
  Added a New Unit menu selection
  Added a New Form menu selection
  Shane

  Revision 1.5  2000/02/18 19:38:52  lazarus
  Implemented TCustomForm.Position
  Better implemented border styles. Still needs some tweaks.
  Changed TComboBox and TListBox to work again, at least partially.
  Minor cleanups.

  Revision 1.4  1999/12/07 01:19:25  lazarus
  MWE:
    Removed some double events
    Changed location of SetCallBack
    Added call to remove signals
    Restructured somethings
    Started to add default handlers in TWinControl
    Made some parts of TControl and TWinControl more delphi compatible
    ... and lots more ...


}
