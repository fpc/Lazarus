{  $Id$  }
{
 /***************************************************************************
                                graphtype.pp
                                ------------
                    Graphic related platform independent types
		    and utility functions.
                   Initial Revision  : Sat Feb 02 0:02:58 2002

 ***************************************************************************/

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
unit GraphType;

{$mode objfpc}{$H+}

interface

uses
  Classes, LCLType;

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}

type
  PColor = ^TColor;
  // don't define TColor as longint, or else the RTTI can't distinguish them
  TColor = -$7FFFFFFF-1..$7FFFFFFF;

  TFontPitch = (fpDefault, fpVariable, fpFixed);
  TFontName = string;
  TFontCharSet = 0..255;
  TFontDataName = string[LF_FACESIZE -1];
  TFontStyle = (fsBold, fsItalic, fsStrikeOut, fsUnderline);
  TFontStyles = set of TFontStyle;
  TFontStylesbase = set of TFontStyle;

  TFontData = record
    Handle : HFont;
    Height : Integer;
    Pitch : TFontPitch;
    Style : TFontStylesBase;
    CharSet : TFontCharSet;
    Name : TFontDataName;
  end;
  
  { Reflects text style when drawn in a rectangle }
  
  TTextLayout = (tlTop, tlCenter, tlBottom);
  TTextStyle = packed record
    Alignment  : TAlignment;  // TextRect Only : horizontal alignment

    Layout     : TTextLayout; // TextRect Only : vertical alignment

    SingleLine : boolean;     // If WordBreak is false then process #13, #10 as
                              // standard chars and perform no Line breaking.

    Clipping   : boolean;     // TextRect Only : Clip Text to passed Rectangle

    ExpandTabs : boolean;     // ignored

    ShowPrefix : boolean;     // TextRect Only : Process first single '&' per
                              //    line as an underscore and draw '&&' as '&'

    Wordbreak  : boolean;     // TextRect Only : If line of text is too long
                              //    too fit between left and right boundaries
                              //    try to break into multiple lines between
                              //    words

    Opaque     : boolean;     // TextRect : Fills background with current Brush
                              // TextOut  : Fills background with current
                              //            foreground color

    SystemFont : Boolean;     // Use the system font instead of Canvas Font
  end;

  TPenStyle = (psSolid, psDash, psDot, psDashDot, psDashDotDot, psClear, psInsideframe);
  TPenMode = (pmBlack, pmWhite, pmNop, pmNot, pmCopy, pmNotCopy, pmMergePenNot,
              pmMaskPenNot, pmMergeNotPen, pmMaskNotPen, pmMerge,pmNotMerge, pmMask,
              pmNotMask, pmXor, pmNotXor
             );

  TPenData = record
    Handle : HPen;
    Color : TColor;
    Width : Integer;
    Style : TPenStyle;
  end;

  TBrushStyle = (bsSolid, bsClear, bsHorizontal, bsVertical, bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross);

  TFillStyle = (fsSurface, fsBorder);
  TFillMode = (fmAlternate, fmWinding);

  TCopymode = longint;

  TCanvasStates = (csHandleValid, csFontValid, csPenvalid, csBrushValid, csRegionValid);
  TCanvasState = set of TCanvasStates;
  TCanvasOrientation = (csLefttoRight, coRighttoLeft);

  { TProgressEvent is a generic progress notification event which may be
        used by TGraphic classes with computationally intensive (slow)
        operations, such as loading, storing, or transforming image data.
    Event params:
      Stage - Indicates whether this call to the OnProgress event is to
        prepare for, process, or clean up after a graphic operation.  If
        OnProgress is called at all, the first call for a graphic operation
        will be with Stage = psStarting, to allow the OnProgress event handler
        to allocate whatever resources it needs to process subsequent progress
        notifications.  After Stage = psStarting, you are guaranteed that
        OnProgress will be called again with Stage = psEnding to allow you
        to free those resources, even if the graphic operation is aborted by
        an exception.  Zero or more calls to OnProgress with Stage = psRunning
        may occur between the psStarting and psEnding calls.
      PercentDone - The ratio of work done to work remaining, on a scale of
        0 to 100.  Values may repeat or even regress (get smaller) in
        successive calls.  PercentDone is usually only a guess, and the
        guess may be dramatically altered as new information is discovered
        in decoding the image.
      RedrawNow - Indicates whether the graphic can be/should be redrawn
        immediately.  Useful for showing successive approximations of
        an image as data is available instead of waiting for all the data
        to arrive before drawing anything.  Since there is no message loop
        activity during graphic operations, you should call Update to force
        a control to be redrawn immediately in the OnProgress event handler.
        Redrawing a graphic when RedrawNow = False could corrupt the image
        and/or cause exceptions.
      Rect - Area of image that has changed and needs to be redrawn.
      Msg - Optional text describing in one or two words what the graphic
        class is currently working on.  Ex:  "Loading" "Storing"
        "Reducing colors".  The Msg string can also be empty.
        Msg strings should be resourced for translation,  should not
        contain trailing periods, and should be used only for
        display purposes.  (do not: if Msg = 'Loading' then...)
  }
  TProgressStage = (psStarting, psRunning, psEnding);
  TProgressEvent = procedure (Sender: TObject; Stage: TProgressStage;
                          PercentDone: Byte; RedrawNow: Boolean; const R: TRect;
                          const Msg: string) of object;
  
  TBevelCut = (bvNone, bvLowered, bvRaised);

  TPixelFormat = (pfDevice, pf1bit, pf4bit, pf8bit, pf15bit, pf16bit, pf24bit,
                  pf32bit, pfCustom);

implementation

end.

{ =============================================================================

  $Log$
  Revision 1.7  2002/09/27 20:52:21  lazarus
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

  Revision 1.6  2002/09/03 08:07:19  lazarus
  MG: image support, TScrollBox, and many other things from Andrew

  Revision 1.5  2002/08/06 09:32:48  lazarus
  MG: moved TColor definition to graphtype.pp and registered TColor names

  Revision 1.4  2002/06/04 15:17:21  lazarus
  MG: improved TFont for XLFD font names

  Revision 1.3  2002/05/10 06:05:50  lazarus
  MG: changed license to LGPL

  Revision 1.2  2002/03/08 16:16:55  lazarus
  MG: fixed parser of end blocks in initialization section added label sections

  Revision 1.1  2002/02/03 00:24:00  lazarus
  TPanel implemented.
  Basic graphic primitives split into GraphType package, so that we can
  reference it from interface (GTK, Win32) units.
  New Frame3d canvas method that uses native (themed) drawing (GTK only).
  New overloaded Canvas.TextRect method.
  LCLLinux and Graphics was split, so a bunch of files had to be modified.

  Revision 1.21  2002/01/02 15:24:58  lazarus
  MG: added TCanvas.Polygon and TCanvas.Polyline

  Revision 1.20  2002/01/02 12:10:01  lazarus
  MG: fixed typo

  Revision 1.19  2001/12/28 11:41:50  lazarus
  MG: added TCanvas.Ellipse, TCanvas.Pie

  Revision 1.18  2001/12/21 18:16:59  lazarus
  Added TImage class
  Shane

  Revision 1.17  2001/11/12 22:12:57  lazarus
  MG: fixed parser: multiple brackets, nil, string[]

  Revision 1.16  2001/11/09 19:14:23  lazarus
  HintWindow changes
  Shane

  Revision 1.15  2001/10/25 19:02:18  lazarus
  MG: fixed parsing constants with OR, AND, XOR, MOD, DIV, SHL, SHR

  Revision 1.14  2001/10/24 00:35:55  lazarus
  MG: fixes for fpc 1.1: range check errors

  Revision 1.13  2001/09/30 08:34:49  lazarus
  MG: fixed mem leaks and fixed range check errors

  Revision 1.12  2001/08/05 10:14:50  lazarus
  MG: removed double props in OI, small bugfixes

  Revision 1.11  2001/06/26 00:08:35  lazarus
  MG: added code for form icons from Rene E. Beszon

  Revision 1.10  2001/06/04 09:32:17  lazarus
  MG: fixed bugs and cleaned up messages

  Revision 1.9  2001/03/21 00:20:29  lazarus
  MG: fixed memory leaks

  Revision 1.7  2001/03/19 14:00:50  lazarus
  MG: fixed many unreleased DC and GDIObj bugs

  Revision 1.6  2001/03/05 14:20:04  lazarus
  added streaming to tgraphic, added tpicture

  Revision 1.5  2001/02/04 19:23:26  lazarus
  Goto dialog added
  Shane

  Revision 1.4  2001/02/04 18:24:41  lazarus
  Code cleanup
  Shane

  Revision 1.3  2001/01/31 21:16:45  lazarus
  Changed to TCOmboBox focusing.
  Shane

  Revision 1.2  2000/08/10 18:56:23  lazarus
  Added some winapi calls.
  Most don't have code yet.
  SetTextCharacterExtra
  CharLowerBuff
  IsCharAlphaNumeric
  Shane

  Revision 1.1  2000/07/13 10:28:23  michael
  + Initial import

  Revision 1.46  2000/05/08 15:56:58  lazarus
  MWE:
    + Added support for mwedit92 in Makefiles
    * Fixed bug # and #5 (Fillrect)
    * Fixed labelsize in ApiWizz
    + Added a call to the resize event in WMWindowPosChanged

  Revision 1.45  2000/03/30 18:07:53  lazarus
  Added some drag and drop code
  Added code to change the unit name when it's saved as a different name.  Not perfect yet because if you are in a comment it fails.

  Shane

  Revision 1.44  2000/03/21 23:47:33  lazarus
  MWE:
    + Added TBitmap.MaskHandle & TGraphic.Draw & TBitmap.Draw

  Revision 1.43  2000/03/16 23:58:46  lazarus
  MWE:
    Added TPixmap for XPM support

  Revision 1.42  2000/03/15 20:15:31  lazarus
  MOdified TBitmap but couldn't get it to work
  Shane

  Revision 1.41  2000/03/10 13:13:37  lazarus
  *** empty log message ***

  Revision 1.40  2000/03/09 23:44:03  lazarus
  MWE:
    * Fixed colorcache
    * Fixed black window in new editor
    ~ Did some cosmetic stuff

  From Peter Dyson <peter@skel.demon.co.uk>:
    + Added Rect api support functions
    + Added the start of ScrollWindowEx

  Revision 1.39  2000/03/08 23:57:38  lazarus
  MWE:
    Added SetSysColors
    Fixed TEdit text bug (thanks to hans-joachim ott <hjott@compuserve.com>)
    Finished GetKeyState
    Added changes from Peter Dyson <peter@skel.demon.co.uk>
    - a new GetSysColor
    - some improvements on ExTextOut

  Revision 1.38  2000/03/06 00:05:05  lazarus
  MWE: Added changes from Peter Dyson <peter@skel.demon.co.uk> for a new
    release of mwEdit (0.92)

  Revision 1.37  2000/01/26 19:16:24  lazarus
  Implemented TPen.Style properly for GTK. Done SelectObject for pen objects.
  Misc bug fixes.
  Corrected GDK declaration for gdk_gc_set_slashes.

  Revision 1.36  2000/01/17 20:36:25  lazarus
  Fixed Makefile again.
  Made implementation of TScreen and screen info saner.
  Began to implemented DeleteObject in GTKWinAPI.
  Fixed a bug in GDI allocation which in turn fixed A LOT of other bugs :-)

  Revision 1.35  1999/12/14 22:05:37  lazarus
  More changes for TToolbar
  Shane

  Revision 1.34  1999/12/02 19:00:59  lazarus
  MWE:
    Added (GDI)Pen
    Changed (GDI)Brush
    Changed (GDI)Font (color)
    Changed Canvas to use/create pen/brush/font
    Hacked mwedit to allow setting the number of chars (till it get a WM/LM_SIZE event)
    The editor shows a line !

  Revision 1.33  1999/11/29 00:46:47  lazarus
  MWE:
    Added TBrush as gdiobject
    commented out some more mwedit MWE_FPC ifdefs

  Revision 1.32  1999/11/25 23:45:08  lazarus
  MWE:
    Added font as GDIobject
    Added some API testcode to testform
    Commented out some more IFDEFs in mwCustomEdit

  Revision 1.31  1999/11/19 01:09:43  lazarus
  MWE:
    implemented TCanvas.CopyRect
    Added StretchBlt
    Enabled creation of TCustomControl.Canvas
    Added a temp hack in TWinControl.Repaint to get a LM_PAINT

  Revision 1.30  1999/11/18 00:13:08  lazarus
  MWE:
    Partly Implemented SelectObject
    Added  ExTextOut
    Added  GetTextExtentPoint
    Added  TCanvas.TextExtent/TextWidth/TextHeight
    Added  TSize and HPEN

  Revision 1.29  1999/11/17 01:16:39  lazarus
  MWE:
    Added some more API stuff
    Added an initial TBitmapCanvas
    Added some DC stuff
    Changed and commented out, original gtk linedraw/rectangle code. This
      is now called through the winapi wrapper.

  Revision 1.28  1999/11/09 17:19:54  lazarus
  added the property PITCH to TFONT.
  Shane

  Revision 1.26  1999/11/05 17:48:17  lazarus
  Added a mwedit1 component to lazarus (MAIN.PP)
  It crashes on create.
  Shane

  Revision 1.25  1999/11/01 01:28:29  lazarus
  MWE: Implemented HandleNeeded/CreateHandle/CreateWND
       Now controls are created on demand. A call to CreateComponent shouldn't
       be needed. It is now part of CreateWnd

  Revision 1.24  1999/10/28 17:17:42  lazarus
  Removed references to FCOmponent.
  Shane

  Revision 1.23  1999/10/25 17:38:52  lazarus
  More stuff added for compatability.  Most stuff added was put in the windows.pp file.  CONST scroll bar messages and such.  2 functions were also added to that unit that needs to be completed.
  Shane

  Revision 1.22  1999/10/22 21:01:51  lazarus

        Removed calls to InterfaceObjects except for controls.pp. Commented
        out any gtk depend lines of code.     MAH

  Revision 1.21  1999/10/19 21:16:23  lazarus
  TColor added to graphics.pp

  Revision 1.20  1999/10/18 07:32:42  lazarus
  Added definitions for Load methods in the TBitmap class. The
  methods have not been implemented yet. They need to be implemented.   CAW

  Revision 1.19  1999/09/26 16:58:01  lazarus
  MWE: Added TBitMap.Mask method

  Revision 1.18  1999/08/26 23:36:02  peter
    + paintbox
    + generic keydefinitions and gtk conversion
    * gtk state -> shiftstate conversion

  Revision 1.17  1999/08/25 18:53:02  lazarus
  Added Canvas.pixel property which allows
  the user to get/set the pixel color.  This will be used in the editor
  to create the illusion of the cursor by XORing the pixel with black.

  Shane

  Revision 1.16  1999/08/20 15:44:37  lazarus
  TImageList changes added from Marc Weustink

  Revision 1.15  1999/08/17 16:46:25  lazarus
  Slight modification to Editor.pp
  Shane

  Revision 1.14  1999/08/16 20:48:03  lazarus
  Added a changed event for TFOnt and code to get the average size of the font.  Doesn't seem to work very well yet.
  The "average size" code is found in gtkobject.inc.

  Revision 1.13  1999/08/16 15:48:49  lazarus
  Changes by file:
       Control: TCOntrol-Function GetRect added
                         ClientRect property added
                TImageList - Added Count
                TWinControl- Function Focused added.
      Graphics: TCanvas - CopyRect added - nothing finished on it though
                          Draw added - nothing finiushed on it though
                clbtnhighlight and clbtnshadow added.  Actual color values not right.
               IMGLIST.PP and IMGLIST.INC files added.

   A few other minor changes for compatability added.

    Shane

  Revision 1.12  1999/08/13 19:55:47  lazarus
  TCanvas.MoveTo added for compatability.

  Revision 1.11  1999/08/13 19:51:07  lazarus
  Minor changes for compatability made.

  Revision 1.10  1999/08/11 20:41:33  lazarus

  Minor changes and additions made.  Lazarus may not compile due to these changes

  Revision 1.9  1999/08/02 01:13:33  lazarus
  Added new colors and corrected BTNFACE
  Need the TSCrollbar class to go further with the editor.
  Mouse doesn't seem to be working correctly yet when I click on the editor window

  Revision 1.8  1999/08/01 21:46:26  lazarus
  Modified the GETWIDTH and GETHEIGHT of TFOnt so you can use it to calculate the length in Pixels of a string.  This is now used in the editor.

  Shane

  Revision 1.7  1999/07/31 06:39:26  lazarus

       Modified the IntCNSendMessage3 to include a data variable. It isn't used
       yet but will help in merging the Message2 and Message3 features.

       Adjusted TColor routines to match Delphi color format

       Added a TGdkColorToTColor routine in gtkproc.inc

       Finished the TColorDialog added to comDialog example.        MAH

 }
