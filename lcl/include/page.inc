{%MainUnit ../extctrls.pp}
{******************************************************************************
                                     TPage
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

{ TPage }



// included by extctrls.pp
{
  $Log$
  Revision 1.22  2004/04/10 17:58:57  mattias
  implemented mainunit hints for include files

  Revision 1.21  2003/09/20 13:27:49  mattias
  varois improvements for ParentColor from Micha

  Revision 1.20  2003/09/20 09:16:07  mattias
  added TDBGrid from Jesus

  Revision 1.19  2003/09/17 15:26:41  mattias
  fixed removing TPage

  Revision 1.18  2003/08/26 14:33:40  mattias
  implemented component tree for OI

  Revision 1.17  2003/08/14 15:31:42  mattias
  started TTabSheet and TPageControl

  Revision 1.16  2003/07/24 06:54:32  mattias
  fixed anti circle mechnism for aligned controls

  Revision 1.15  2002/08/19 15:15:23  mattias
  implemented TPairSplitter

  Revision 1.14  2003/06/11 22:29:42  mattias
  fixed realizing bounds after loading form

  Revision 1.13  2003/06/10 17:23:35  mattias
  implemented tabstop

  Revision 1.12  2002/12/22 22:42:55  mattias
  custom controls now support child wincontrols

  Revision 1.11  2002/09/05 12:11:43  lazarus
  MG: TNotebook is now streamable

  Revision 1.10  2002/09/02 19:10:28  lazarus
  MG: TNoteBook now starts with no Page and TPage has no auto names

  Revision 1.9  2002/06/08 17:16:02  lazarus
  MG: added close buttons and images to TNoteBook and close buttons to source editor

  Revision 1.8  2002/05/10 06:05:55  lazarus
  MG: changed license to LGPL

  Revision 1.7  2002/03/31 23:20:38  lazarus
  MG: fixed initial size of TPage

  Revision 1.6  2002/03/25 17:59:20  lazarus
  GTK Cleanup
  Shane

  Revision 1.5  2002/03/13 22:48:16  lazarus
  Constraints implementation (first cut) and sizig - moving system rework to
  better match Delphi/Kylix way of doing things (the existing implementation
  worked by acident IMHO :-)

  Revision 1.4  2002/01/01 15:50:14  lazarus
  MG: fixed initial component aligning

  Revision 1.3  2001/06/26 21:44:32  lazarus
  MG: reduced paint messages

  Revision 1.2  2001/01/12 18:27:32  lazarus
  Streaming additions by MAttias
  Shane

  Revision 1.1  2000/07/13 10:28:27  michael
  + Initial import

  Revision 1.2  2000/05/09 02:07:40  lazarus
  Replaced writelns with Asserts.                          CAW

  Revision 1.1  2000/04/02 20:49:56  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.13  2000/02/26 23:31:50  lazarus
  MWE:
    Fixed notebook crash on insert
    Fixed loadfont problem for win32 (tleast now a fontname is required)

  Revision 1.12  2000/02/25 19:28:34  lazarus
  Played with TNotebook to see why it crashes when I add a tab and the tnotebook is showing.  Havn't figured it out
  Shane

  Revision 1.11  2000/02/20 20:13:47  lazarus
  On my way to make alignments and stuff work :-)

  Revision 1.10  2000/01/06 01:10:36  lazarus
  Stoppok:
     - changed ReadState to match current definition in fcl
       (affects TPage & TCustomNotebook)
     - added callback FItems.OnChanging to TCustomRadiogroup

  Revision 1.9  1999/11/01 01:28:30  lazarus
  MWE: Implemented HandleNeeded/CreateHandle/CreateWND
       Now controls are created on demand. A call to CreateComponent shouldn't
       be needed. It is now part of CreateWnd

  Revision 1.8  1999/10/05 02:40:22  lazarus
  Cleaned up the code.       CAW

  Revision 1.7  1999/09/30 21:59:03  lazarus
  MWE: Fixed TNoteBook problems
       Modifications: A few
       - Removed some debug messages
       + Added some others
       * changed fixed widged of TPage. Code is still broken.
       + TWinControls are also added to the Controls collection
       + Added TControl.Controls[] property

  Revision 1.6  1999/09/22 20:07:16  lazarus
  *** empty log message ***

  Revision 1.4  1999/09/21 23:46:55  lazarus
  *** empty log message ***

  Revision 1.3  1999/08/03 06:34:26  lazarus
  Added cvs logging to bottom of unit

}

