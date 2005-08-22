{%MainUnit ../interfacebase.pp}
{ $Id$
******************************************************************************
                                  TWidgetSet

                                   WinApi stuff


  !! Keep this alphabetical !!
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

constructor TWidgetSet.Create;
begin
  inherited Create;
end;

destructor TWidgetSet.Destroy;
begin
  inherited Destroy;
end;

procedure TWidgetSet.PassCmdLineOptions;
begin
end;

{
  $Log$
  Revision 1.117  2004/09/24 21:34:14  micha
  convert LM_CREATE message to interface methods
  remove SendMsgToInterface, CNSendMessage and related methods
  remove TWidgetSet.IntSendMessage3; all LCL to interface messages have been converted

  Revision 1.116  2004/04/10 17:58:57  mattias
  implemented mainunit hints for include files

  Revision 1.115  2004/03/05 00:14:02  marc
  * Renamed TInterfaceBase to TWidgetSet

  Revision 1.114  2004/02/23 08:19:04  micha
  revert intf split

  Revision 1.112  2003/11/24 11:03:07  marc
  * Splitted winapi*.inc into a winapi and a lcl interface communication part

  Revision 1.111  2003/11/14 21:17:59  micha
  new maskblt method for transparency blitting

  Revision 1.110  2003/11/10 16:15:31  micha
  cleanups; win32 fpimage support

  Revision 1.109  2003/11/07 22:50:44  mattias
  fixed finding sysutilh.inc

  Revision 1.108  2003/11/07 18:48:52  micha
  symmetry getdesignerdc, releasedesignerdc

  Revision 1.107  2003/11/03 22:37:41  mattias
  fixed vert scrollbar, implemented GetDesignerDC

  Revision 1.106  2003/10/28 14:25:37  mattias
  fixed unit circle

  Revision 1.105  2003/10/26 17:34:41  micha
  new interface method to attach a menu to window

  Revision 1.104  2003/10/16 23:54:27  marc
  Implemented new gtk keyevent handling

  Revision 1.103  2003/08/31 17:30:49  mattias
  fixed TControl painting for win32

  Revision 1.102  2003/08/26 08:12:33  mattias
  applied listbox/combobox patch from Karl

  Revision 1.101  2003/08/25 16:43:32  mattias
  moved many graphics types form graphtype.pp to graphics.pp

  Revision 1.100  2003/08/19 12:23:23  mattias
  moved types from graphtype.pp back to graphics.pp

  Revision 1.99  2003/08/18 19:24:18  mattias
  fixed TCanvas.Pie

  Revision 1.98  2003/07/20 06:39:03  mattias
  added comments

  Revision 1.97  2003/07/20 06:27:19  mattias
  fixed GetWindowRelativePosition

  Revision 1.96  2003/07/06 20:40:34  mattias
  TWinControl.WmSize/Move now updates interface messages smarter

  Revision 1.95  2003/07/04 10:12:16  mattias
  added default message handler to win32 interface

  Revision 1.94  2003/07/02 15:56:15  mattias
  fixed win32 painting and started creating bitmaps from rawimages

  Revision 1.93  2003/07/02 10:02:51  mattias
  fixed TPaintStruct

  Revision 1.92  2003/07/01 14:06:45  mattias
  made Begin/EndPaint interface dependent

  Revision 1.91  2003/07/01 09:29:51  mattias
  attaching menuitems topdown

  Revision 1.90  2002/08/19 15:15:23  mattias
  implemented TPairSplitter

  Revision 1.89  2002/08/18 00:03:45  mattias
  fixed bitbtn image for NoToAll

  Revision 1.88  2002/08/17 23:41:34  mattias
  many clipping fixes

  Revision 1.87  2003/04/11 17:10:20  mattias
  added but not implemented ComboBoxDropDown

  Revision 1.86  2003/03/29 17:20:05  mattias
  added TMemoScrollBar

  Revision 1.85  2003/03/17 20:53:16  mattias
  removed SetRadioButtonGroupMode

  Revision 1.84  2003/03/17 20:50:30  mattias
  fixed TRadioGroup.ItemIndex=-1

  Revision 1.83  2003/03/17 08:51:09  mattias
  added IsWindowVisible

  Revision 1.82  2003/02/28 19:54:05  mattias
  added ShowWindow

  Revision 1.81  2003/02/28 10:14:29  mattias
  started package system (packager)

  Revision 1.80  2003/02/26 12:44:52  mattias
  readonly flag is now only saved if user set

  Revision 1.79  2003/01/27 13:49:16  mattias
  reduced speedbutton invalidates, added TCanvas.Frame

  Revision 1.78  2003/01/19 14:44:28  mattias
  started make resource string

  Revision 1.77  2002/12/30 17:24:08  mattias
  added history to identifier completion

  Revision 1.76  2002/12/27 17:12:38  mattias
  added more Delphi win32 compatibility functions

  Revision 1.75  2002/12/26 11:00:14  mattias
  added included by to unitinfo and a few win32 functions

  Revision 1.74  2002/12/25 13:30:36  mattias
  added more windows funcs and fixed jump to compiler error end of file

  Revision 1.73  2002/12/25 10:21:05  mattias
  made Form.Close more Delphish, added some windows compatibility functions

  Revision 1.72  2002/02/09 02:30:56  mattias
  added patch from Jeroen van Idekinge

  Revision 1.71  2002/02/09 01:48:23  mattias
  renamed TinterfaceObject.Init to AppInit and TWinControls can now contain childs in gtk

  Revision 1.70  2002/12/05 22:16:29  mattias
  double byte char font started

  Revision 1.69  2002/11/30 11:22:53  mattias
  statusbar now uses invalidaterect

  Revision 1.68  2002/11/23 13:48:44  mattias
  added Timer patch from Vincent Snijders

  Revision 1.67  2002/11/12 10:16:16  lazarus
  MG: fixed TMainMenu creation

  Revision 1.66  2002/11/09 15:02:07  lazarus
  MG: fixed LM_LVChangedItem, OnShowHint, small bugs

  Revision 1.65  2002/10/31 21:29:47  lazarus
  MG: implemented TControlScrollBar.Size

  Revision 1.64  2002/10/31 04:27:59  lazarus
  AJ: added TShape

  Revision 1.63  2002/10/26 15:15:48  lazarus
  MG: broke LCL<->interface circles

  Revision 1.62  2002/10/25 10:10:40  lazarus
  MG: reduced output

  Revision 1.61  2002/10/25 10:06:34  lazarus
  MG: broke interfacebase uses circles

  Revision 1.60  2002/10/25 09:47:38  lazarus
  MG: added inputdialog.inc

  Revision 1.59  2002/10/23 20:47:26  lazarus
  AJ: Started Form Scrolling
      Started StaticText FocusControl
      Fixed Misc Dialog Problems
      Added TApplication.Title

  Revision 1.58  2002/10/23 14:36:52  lazarus
  AJ:Fixes to PromptUser;Switched ShowMessage* to use NotifyUser*;
     fixed TGraphicPropertyEditor for when Property is nil.

  Revision 1.57  2002/10/16 16:58:22  lazarus
  MG: moved SendCachedLCLMessages

  Revision 1.56  2002/10/14 18:36:56  lazarus
  AJ: Improvements/Fixes to new PromptUser API

  Revision 1.55  2002/10/12 16:36:39  lazarus
  AJ: added new QueryUser/NotifyUser

  Revision 1.54  2002/10/11 16:00:39  lazarus
  AJ: made InputQuery Interface Dependant

  Revision 1.53  2002/10/10 13:29:08  lazarus
  AJ: added LoadStockPixmap routine & minor fixes to/for GNOMEInt

  Revision 1.52  2002/10/08 02:52:58  lazarus
  AJ: fixed bug in drawtext, switched hintwindow's to use Canvas.TextRect

  Revision 1.51  2002/10/03 14:47:31  lazarus
  MG: added TComboBox.OnPopup+OnCloseUp+ItemWidth

  Revision 1.50  2002/09/27 20:52:23  lazarus
  MWE: Applied patch from "Andrew Johnson" <aj_genius@hotmail.com>

  Here is the run down of what it includes -

   -Vasily Volchenko's Updated Russian Localizations

   -improvements to GTK Styles/SysColors
   -initial GTK Palette code - (untested, and for now useless)

   -Hint Windows and Modal dialogs now try to stay transient to
    the main program form, aka they stay on top of the main form
    and usually minimize/maximize with it.

   -fixes to Form BorderStyle code(tool windows needed a border)

   -fixes DrawFrameControl DFCS_BUTTONPUSH to match Win32 better
    when flat

   -fixes DrawFrameControl DFCS_BUTTONCHECK to match Win32 better
    and to match GTK theme better. It works most of the time now,
    but some themes, noteably Default, don't work.

   -fixes bug in Bitmap code which broke compiling in NoGDKPixbuf
    mode.

   -misc other cleanups/ fixes in gtk interface

   -speedbutton's should now draw correctly when flat in Win32

   -I have included an experimental new CheckBox(disabled by
    default) which has initial support for cbGrayed(Tri-State),
    and WordWrap, and misc other improvements. It is not done, it
    is mostly a quick hack to test DrawFrameControl
    DFCS_BUTTONCHECK, however it offers many improvements which
    can be seen in cbsCheck/cbsCrissCross (aka non-themed) state.

   -fixes Message Dialogs to more accurately determine
    button Spacing/Size, and Label Spacing/Size based on current
    System font.
   -fixes MessageDlgPos, & ShowMessagePos in Dialogs
   -adds InputQuery & InputBox to Dialogs

   -re-arranges & somewhat re-designs Control Tabbing, it now
    partially works - wrapping around doesn't work, and
    subcontrols(Panels & Children, etc) don't work. TabOrder now
    works to an extent. I am not sure what is wrong with my code,
    based on my other tests at least wrapping and TabOrder SHOULD
    work properly, but.. Anyone want to try and fix?

   -SynEdit(Code Editor) now changes mouse cursor to match
    position(aka over scrollbar/gutter vs over text edit)

   -adds a TRegion property to Graphics.pp, and Canvas. Once I
    figure out how to handle complex regions(aka polygons) data
    properly I will add Region functions to the canvas itself
    (SetClipRect, intersectClipRect etc.)

   -BitBtn now has a Stored flag on Glyph so it doesn't store to
    lfm/lrs if Glyph is Empty, or if Glyph is not bkCustom(aka
    bkOk, bkCancel, etc.) This should fix most crashes with older
    GDKPixbuf libs.

  Revision 1.49  2002/09/19 19:56:14  lazarus
  MG: accelerated designer drawings

  Revision 1.48  2002/09/18 17:07:24  lazarus
  MG: added patch from Andrew

  Revision 1.47  2002/09/12 05:56:15  lazarus
  MG: gradient fill, minor issues from Andrew

  Revision 1.46  2002/09/10 06:49:19  lazarus
  MG: scrollingwincontrol from Andrew

  Revision 1.45  2002/09/09 14:01:05  lazarus
  MG: improved TScreen and ShowModal

  Revision 1.44  2002/08/30 12:32:20  lazarus
  MG: MoveWindowOrgEx, Splitted FWinControls/FControls, TControl drawing, Better DesignerDrawing, ...

  Revision 1.43  2002/08/28 09:40:49  lazarus
  MG: reduced paint messages and DC getting/releasing

  Revision 1.42  2002/08/25 13:31:35  lazarus
  MG: replaced C-style operators

  Revision 1.41  2002/08/21 10:46:37  lazarus
  MG: fixed unreleased gdiRegions

  Revision 1.40  2002/08/19 20:34:47  lazarus
  MG: improved Clipping, TextOut, Polygon functions

  Revision 1.39  2002/08/15 15:46:48  lazarus
  MG: added changes from Andrew (Clipping)

  Revision 1.38  2002/08/13 07:08:24  lazarus
  MG: added gdkpixbuf.pp and changes from Andrew Johnson

  Revision 1.37  2002/08/08 18:05:46  lazarus
  MG: added graphics extensions from Andrew Johnson

  Revision 1.36  2002/08/08 17:26:37  lazarus
  MG: added property TMenuItems.RightJustify

  Revision 1.35  2002/08/08 09:07:06  lazarus
  MG: TMenuItem can now be created/destroyed/moved at any time

  Revision 1.34  2002/08/07 09:55:30  lazarus
  MG: codecompletion now checks for filebreaks, savefile now checks for filedate

  Revision 1.33  2002/08/05 10:45:02  lazarus
  MG: TMenuItem.Caption can now be set after creation

  Revision 1.32  2002/06/21 15:41:56  lazarus
  MG: moved RectVisible, ExcludeClipRect and IntersectClipRect to interface dependent functions

  Revision 1.31  2002/06/04 15:17:22  lazarus
  MG: improved TFont for XLFD font names

  Revision 1.30  2002/05/27 17:58:41  lazarus
  MG: added command line help

  Revision 1.29  2002/05/24 07:16:31  lazarus
  MG: started mouse bugfix and completed Makefile.fpc

  Revision 1.28  2002/05/10 06:05:52  lazarus
  MG: changed license to LGPL

  Revision 1.27  2002/05/09 12:41:28  lazarus
  MG: further clientrect bugfixes

  Revision 1.26  2002/03/08 16:16:55  lazarus
  MG: fixed parser of end blocks in initialization section added label sections

  Revision 1.25  2002/02/03 00:24:01  lazarus
  TPanel implemented.
  Basic graphic primitives split into GraphType package, so that we can
  reference it from interface (GTK, Win32) units.
  New Frame3d canvas method that uses native (themed) drawing (GTK only).
  New overloaded Canvas.TextRect method.
  LCLLinux and Graphics was split, so a bunch of files had to be modified.

  Revision 1.24  2002/01/02 15:24:58  lazarus
  MG: added TCanvas.Polygon and TCanvas.Polyline

  Revision 1.23  2001/12/28 11:41:51  lazarus
  MG: added TCanvas.Ellipse, TCanvas.Pie

  Revision 1.22  2001/12/27 16:31:28  lazarus
  MG: implemented TCanvas.Arc

  Revision 1.21  2001/12/12 14:23:17  lazarus
  MG: implemented DestroyCaret

  Revision 1.20  2001/11/14 19:10:03  lazarus
  MG: fixes for parser and linkscanner and small cleanups

  Revision 1.19  2001/11/14 17:46:58  lazarus
  Changes to make toggling between form and unit work.
  Added BringWindowToTop
  Shane

  Revision 1.18  2001/11/12 16:56:07  lazarus
  MG: CLIPBOARD

  Revision 1.17  2001/10/10 17:55:04  lazarus
  MG: fixed caret lost, gtk cleanup, bracket lvls, bookmark saving

  Revision 1.16  2001/06/26 21:44:32  lazarus
  MG: reduced paint messages

  Revision 1.15  2001/06/14 23:13:30  lazarus
  MWE:
    * Fixed some syntax errors for the latest 1.0.5 compiler

  Revision 1.14  2001/04/06 22:25:14  lazarus
  * TTimer uses winapi-interface now instead of sendmessage-interface, stoppok

  Revision 1.13  2001/03/26 14:58:31  lazarus
  MG: setwindowpos + bugfixes

  Revision 1.12  2001/03/12 12:17:01  lazarus
  MG: fixed random function results

  Revision 1.11  2001/02/16 19:13:30  lazarus
  Added some functions
  Shane

  Revision 1.10  2001/01/23 19:13:57  lazarus
  Fixxed the errors I commited with Unionrect
  Shane

  Revision 1.9  2001/01/23 19:01:10  lazarus
  Fixxed bug in RestoreDC
  Shane

  Revision 1.8  2001/01/23 18:42:10  lazarus
  Added InvalidateRect to gtkwinapi.inc
  Shane

  Revision 1.7  2000/12/06 14:54:38  lazarus
  Set some defaults in interfacebase.inc
  Shane

  Revision 1.6  2000/09/10 19:58:47  lazarus
  MWE:
    * Updated makefiles for FPC release 1.0  binary units
    * Changed creation, now LCL unit distributions are possible
    * Moved interfaces.pp from LCL to interface dirs

  Revision 1.5  2000/08/14 12:31:12  lazarus
  Minor modifications for SynEdit .
  Shane

  Revision 1.4  2000/08/11 14:59:09  lazarus
  Adding all the Synedit files.
  Changed the GDK_KEY_PRESS and GDK_KEY_RELEASE stuff to fix the problem in the editor with the shift key being ignored.
  Shane

  Revision 1.3  2000/08/10 18:56:24  lazarus
  Added some winapi calls.
  Most don't have code yet.
  SetTextCharacterExtra
  CharLowerBuff
  IsCharAlphaNumeric
  Shane

  Revision 1.2  2000/07/30 21:48:32  lazarus
  MWE:
    = Moved ObjectToGTKObject to GTKProc unit
    * Fixed array checking in LoadPixmap
    = Moved LM_SETENABLED to API func EnableWindow and EnableMenuItem
    ~ Some cleanup

  Revision 1.1  2000/07/13 10:28:26  michael
  + Initial import

  Revision 1.6  2000/05/11 22:04:15  lazarus
  MWE:
    + Added messagequeue
    * Recoded SendMessage and Peekmessage
    + Added postmessage
    + added DeliverPostMessage

  Revision 1.5  2000/05/08 12:54:19  lazarus
  Removed some writeln's
  Added alignment for the TLabel.  Isn't working quite right.
  Added the shell code for WindowFromPoint and GetParent.
  Added FindLCLWindow
  Shane

  Revision 1.4  2000/05/03 00:27:05  lazarus
  MWE:
    + First rollout of the API wizzard.

  Revision 1.3  2000/04/10 14:03:07  lazarus
  Added SetProp and GetProp winapi calls.
  Added ONChange to the TEdit's published property list.
  Shane

  Revision 1.2  2000/04/07 16:59:55  lazarus
  Implemented GETCAPTURE and SETCAPTURE along with RELEASECAPTURE.
  Shane

  Revision 1.1  2000/04/02 20:49:56  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.30  2000/03/31 18:41:03  lazarus
  Implemented MessageBox / Application.MessageBox calls. No icons yet, though...

  Revision 1.29  2000/03/30 18:07:54  lazarus
  Added some drag and drop code
  Added code to change the unit name when it's saved as a different name.  Not perfect yet because if you are in a comment it fails.

  Shane

  Revision 1.28  2000/03/28 22:47:50  lazarus
  MWE:
    Started with the blt function family

  Revision 1.27  2000/03/19 23:01:43  lazarus
  MWE:
    = Changed splashscreen loading/colordepth
    = Chenged Save/RestoreDC to platform  dependent, since they are
      relative to a DC

  Revision 1.26  2000/03/16 23:58:46  lazarus
  MWE:
    Added TPixmap for XPM support

  Revision 1.25  2000/03/14 19:49:05  lazarus
  Modified the painting process for TWincontrol.  Now it runs throug it's FCONTROLS list and paints all them
  Shane

  Revision 1.24  2000/03/10 18:31:10  lazarus
  Added TSpeedbutton code
  Shane

  Revision 1.23  2000/03/09 23:48:02  lazarus
  MWE:
    * Fixed colorcache
    * Fixed black window in new editor
    ~ Did some cosmetic stuff

  From Peter Dyson <peter@skel.demon.co.uk>:
    + Added Rect api support functions
    + Added the start of ScrollWindowEx

  Revision 1.22  2000/03/08 23:57:39  lazarus
  MWE:
    Added SetSysColors
    Fixed TEdit text bug (thanks to hans-joachim ott <hjott@compuserve.com>)
    Finished GetKeyState
    Added changes from Peter Dyson <peter@skel.demon.co.uk>
    - a new GetSysColor
    - some improvements on ExTextOut

  Revision 1.21  2000/03/06 00:05:05  lazarus
  MWE: Added changes from Peter Dyson <peter@skel.demon.co.uk> for a new
    release of mwEdit (0.92)

  Revision 1.20  2000/03/03 22:58:26  lazarus
  MWE:
    Fixed focussing problem.
      LM-FOCUS was bound to the wrong signal
    Added GetKeyState api func.
      Now LCL knows if shift/trl/alt is pressed (might be handy for keyboard
      selections ;-)

  Revision 1.19  2000/02/22 23:26:13  lazarus
  MWE: Fixed cursor movement in editor
       Started on focus problem

  Revision 1.18  2000/01/31 20:00:22  lazarus
  Added code for Application.ProcessMessages.  Needs work.
  Added TScreen.Width and TScreen.Height.  Added the code into
  GetSystemMetrics for these two properties.
  Shane

  Revision 1.17  2000/01/25 00:38:25  lazarus
  MWE:
    Added GetFocus

  Revision 1.16  2000/01/16 23:23:07  lazarus
  MWE:
    Added/completed scrollbar API funcs

  Revision 1.15  1999/12/21 21:35:54  lazarus
  committed the latest toolbar code.  Currently it doesn't appear anywhere and I have to get it to add buttons correctly through (I think) setstyle.  I think I'll implement the LM_TOOLBARINSERTBUTTON call there.
  Shane

  Revision 1.14  1999/12/21 00:07:06  lazarus
  MWE:
    Some fixes
    Completed a bit of DraWEdge

  Revision 1.13  1999/12/20 21:01:14  lazarus
  Added a few things for compatability with Delphi and TToolbar
  Shane

  Revision 1.12  1999/12/18 18:27:32  lazarus
  MWE:
    Rearranged some events to get a LM_SIZE, LM_MOVE and LM_WINDOWPOSCHANGED
    Initialized the TextMetricstruct to zeros to clear unset values
    Get mwEdit to show more than one line
    Fixed some errors in earlier commits

  Revision 1.11  1999/12/14 21:07:12  lazarus
  Added more stuff for TToolbar
  Shane

  Revision 1.10  1999/12/02 19:00:59  lazarus
  MWE:
    Added (GDI)Pen
    Changed (GDI)Brush
    Changed (GDI)Font (color)
    Changed Canvas to use/create pen/brush/font
    Hacked mwedit to allow setting the number of chars (till it get a WM/LM_SIZE event)
    The editor shows a line !

  Revision 1.9  1999/11/29 00:46:47  lazarus
  MWE:
    Added TBrush as gdiobject
    commented out some more mwedit MWE_FPC ifdefs

  Revision 1.8  1999/11/25 23:45:08  lazarus
  MWE:
    Added font as GDIobject
    Added some API testcode to testform
    Commented out some more IFDEFs in mwCustomEdit

  Revision 1.7  1999/11/19 01:09:43  lazarus
  MWE:
    implemented TCanvas.CopyRect
    Added StretchBlt
    Enabled creation of TCustomControl.Canvas
    Added a temp hack in TWinControl.Repaint to get a LM_PAINT

  Revision 1.6  1999/11/18 00:13:08  lazarus
  MWE:
    Partly Implemented SelectObject
    Added  ExTextOut
    Added  GetTextExtentPoint
    Added  TCanvas.TextExtent/TextWidth/TextHeight
    Added  TSize and HPEN

  Revision 1.5  1999/11/17 01:16:40  lazarus
  MWE:
    Added some more API stuff
    Added an initial TBitmapCanvas
    Added some DC stuff
    Changed and commented out, original gtk linedraw/rectangle code. This
      is now called through the winapi wrapper.

  Revision 1.4  1999/11/16 01:32:22  lazarus
  MWE:
    Added some more DC functionality

}


