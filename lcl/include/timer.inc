// included by extctrls.pp

{******************************************************************************
                                  TTimer
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

// included by extctrls.pp

{
  $Log$
  Revision 1.9  2002/11/04 11:48:48  lazarus
  MG: implemented TIdleTimer and fixed small bugs

  Revision 1.8  2002/10/24 10:27:52  lazarus
  MG: broke extctrls.pp <-> forms.pp circle

  Revision 1.7  2002/10/15 16:01:36  lazarus
  MG: fixed timers

  Revision 1.6  2002/10/14 19:00:49  lazarus
  MG: fixed zombie timers

  Revision 1.5  2002/05/10 06:05:55  lazarus
  MG: changed license to LGPL

  Revision 1.4  2001/11/19 21:48:54  lazarus
  MG: fixed splash timer AV, incomplete project loading, application save as

  Revision 1.3  2001/09/30 08:34:50  lazarus
  MG: fixed mem leaks and fixed range check errors

  Revision 1.2  2001/04/06 22:25:14  lazarus
  * TTimer uses winapi-interface now instead of sendmessage-interface, stoppok

  Revision 1.1  2000/07/13 10:28:28  michael
  + Initial import

  Revision 1.2  2000/05/09 02:07:40  lazarus
  Replaced writelns with Asserts.                          CAW

  Revision 1.1  2000/04/02 20:49:57  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.6  2000/03/30 18:07:55  lazarus
  Added some drag and drop code
  Added code to change the unit name when it's saved as a different name.  Not perfect yet because if you are in a comment it fails.

  Shane

  Revision 1.5  2000/03/06 00:05:05  lazarus
  MWE: Added changes from Peter Dyson <peter@skel.demon.co.uk> for a new
    release of mwEdit (0.92)

  Revision 1.4  1999/09/16 21:14:27  lazarus
    Some cleanups to the timer class. (moved some comments to timer.inc,
    added more comments and changed TTimer.Timer from function to procedure)
      Stoppok

  Revision 1.3  1999/08/07 17:59:24  lazarus

        buttons.pp   the DoLeave and DoEnter were connected to the wrong
                     event.

        The rest were modified to use the new CNSendMessage function.   MAH

  Revision 1.2  1999/07/31 06:39:31  lazarus

       Modified the IntCNSendMessage3 to include a data variable. It isn't used
       yet but will help in merging the Message2 and Message3 features.

       Adjusted TColor routines to match Delphi color format

       Added a TGdkColorToTColor routine in gtkproc.inc

       Finished the TColorDialog added to comDialog example.        MAH

 }
