{******************************************************************************}
{                                                                              }
{ Visual Styles (Themes) API interface Unit for Object Pascal                  }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: uxtheme.h, released June 2001. The original Pascal     }
{ code is: UxTheme.pas, released July 2001. The initial developer of the       }
{ Pascal code is Marcel van Brakel (brakelm@chello.nl).                        }
{                                                                              }
{ Portions created by Marcel van Brakel are Copyright (C) 1999-2001            }
{ Marcel van Brakel. All Rights Reserved.                                      }
{                                                                              }
{ Portions created by Mike Lischke are Copyright (C) 1999-2002                 }
{ Mike Lischke. All Rights Reserved.                                           }
{                                                                              }
{ Potions created by Paul Ishenin are Copyright (C) 2009                       }
{ Paul Ishenin. All Rights Reserved                                            }
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{                                                                              }
{ You may retrieve the latest version of this file at the Project JEDI home    }
{ page, located at http://delphi-jedi.org or my personal homepage located at   }
{ http://members.chello.nl/m.vanbrakel2                                        }
{                                                                              }
{ The contents of this file are used with permission, subject to the Mozilla   }
{ Public License Version 1.1 (the "License"); you may not use this file except }
{ in compliance with the License. You may obtain a copy of the License at      }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ Alternatively, the contents of this file may be used under the terms of the  }
{ GNU Lesser General Public License (the  "LGPL License"), in which case the   }
{ provisions of the LGPL License are applicable instead of those above.        }
{ If you wish to allow use of your version of this file only under the terms   }
{ of the LGPL License and not to allow others to use your version of this file }
{ under the MPL, indicate your decision by deleting  the provisions above and  }
{ replace  them with the notice and other provisions required by the LGPL      }
{ License.  If you do not delete the provisions above, a recipient may use     }
{ your version of this file under either the MPL or the LGPL License.          }
{                                                                              }
{ For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html }
{                                                                              }
{******************************************************************************}

unit Win32UxTheme;

{$mode objfpc}{$H+}

{$HPPEMIT ''}
{$HPPEMIT '#include "uxtheme.h"'}
{$HPPEMIT ''}

interface

uses
  Windows;

procedure FreeThemeLibrary;
function InitThemeLibrary: Boolean;
function UseThemes: Boolean;

const
  WM_THEMECHANGED = $031A;
  
type
  HTHEME = THANDLE;     // handle to a section of theme data for class
  {$EXTERNALSYM HTHEME}

const
//if (_WIN32_WINNT >= 0x0600)
  MAX_THEMECOLOR  = 64;
  {$EXTERNALSYM MAX_THEMECOLOR}
  MAX_THEMESIZE   = 64;
  {$EXTERNALSYM MAX_THEMESIZE}
//endif

//if (NTDDI_VERSION>= NTDDI_WIN7)
//---------------------------------------------------------------------------
// BeginPanningFeedback - Visual feedback init function related to pan gesture
//   - internally called by DefaultGestureHandler
//   - called by application
//
//  HWND hwnd - The handle to the Target window that will receive feedback
//
//---------------------------------------------------------------------------

var
  BeginPanningFeedback: function(hwnd: HWND): BOOL; stdcall;
{$EXTERNALSYM BeginPanningFeedback}

//---------------------------------------------------------------------------
// UpdatePanningFeedback : Visual feedback function related to pan gesture
// Can Be called only after a BeginPanningFeedback call
//   - internally called by DefaultGestureHandler
//   - called by application
//
// HWND hwnd                 - The handle to the Target window that will receive feedback
//                             For the method to succeed this must be the same hwnd as provided in
//                             BeginPanningFeedback
//
// LONG lTotalOverpanOffsetX - The Total displacement that the window has moved in the horizontal direction
//                             since the end of scrollable region was reached. The API would move the window by the distance specified
//                             A maximum displacement of 30 pixels is allowed
//
// LONG lTotalOverpanOffsetY - The Total displacement that the window has moved in the horizontal direction
//                             since the end of scrollable
//                             region was reached. The API would move the window by the distance specified
//                             A maximum displacement of 30 pixels is allowed
//
// BOOL fInInertia           - Flag dictating whether the Application is handling a WM_GESTURE message with the
//                             GF_INERTIA FLAG set
//
//   Incremental calls to UpdatePanningFeedback should make sure they always pass
//   the sum of the increments and not just the increment themselves
//   Eg : If the initial displacement is 10 pixels and the next displacement 10 pixels
//        the second call would be with the parameter as 20 pixels as opposed to 10
//   Eg : UpdatePanningFeedback(hwnd, 10, 10, TRUE)
//

var
  UpdatePanningFeedback: function(hwnd: HWND; lTotalOverpanOffsetX: LONG; lTotalOverpanOffsetY: LONG;
    fInInertia: BOOL): BOOL; stdcall;
{$EXTERNALSYM UpdatePanningFeedback}

//---------------------------------------------------------------------------
//
// EndPanningFeedback :Visual feedback reset function related to pan gesture
//   - internally called by DefaultGestureHandler
//   - called by application
//   Terminates any existing animation that was in process or set up by BeginPanningFeedback and UpdatePanningFeedback
//   The EndPanningFeedBack needs to be called Prior to calling any BeginPanningFeedBack if we have already
//   called a BeginPanningFeedBack followed by one/ more UpdatePanningFeedback calls
//
//  HWND hwnd         - The handle to the Target window that will receive feedback
//
//  BOOL fAnimateBack - Flag to indicate whether you wish the displaced window to move back
//                      to the original position via animation or a direct jump.
//                      Either ways the method will try to restore the moved window.
//                      The latter case exists for compatibility with legacy apps.
//

var
  EndPanningFeedback: function(hwnd: HWND; fAnimateBack: BOOL): BOOL; stdcall;
{$EXTERNALSYM EndPanningFeedback}
//endif

//----------------------------------------------------------------------------------------------------------------------
// NOTE: PartId's and StateId's used in the theme API are defined in the
//       hdr file <tmschema.h> using the TM_PART and TM_STATE macros.  For
//       example, "TM_PART(BP, PUSHBUTTON)" defines the PartId "BP_PUSHBUTTON".
//----------------------------------------------------------------------------------------------------------------------
//  OpenThemeData()     - Open the theme data for the specified HWND and
//                        semi-colon separated list of class names.
//
//                        OpenThemeData() will try each class name, one at
//                        a time, and use the first matching theme info
//                        found.  If a match is found, a theme handle
//                        to the data is returned.  If no match is found,
//                        a "NULL" handle is returned.
//
//                        When the window is destroyed or a WM_THEMECHANGED
//                        msg is received, "CloseThemeData()" should be
//                        called to close the theme handle.
//
//  hwnd                - window handle of the control/window to be themed
//
//  pszClassList        - class name (or list of names) to match to theme data
//                        section.  if the list contains more than one name,
//                        the names are tested one at a time for a match.
//                        If a match is found, OpenThemeData() returns a
//                        theme handle associated with the matching class.
//                        This param is a list (instead of just a single
//                        class name) to provide the class an opportunity
//                        to get the "best" match between the class and
//                        the current theme.  For example, a button might
//                        pass L"OkButton, Button" if its ID=ID_OK.  If
//                        the current theme has an entry for OkButton,
//                        that will be used.  Otherwise, we fall back on
//                        the normal Button entry.
//----------------------------------------------------------------------------------------------------------------------

var
  OpenThemeData: function(hwnd: HWND; pszClassList: LPCWSTR): HTHEME; stdcall;
{$EXTERNALSYM OpenThemeData}

const
  OTD_FORCE_RECT_SIZING = $00000001;          // make all parts size to rect
{$EXTERNALSYM OTD_FORCE_RECT_SIZING}
  OTD_NONCLIENT         = $00000002;          // set if hTheme to be used for nonclient area
{$EXTERNALSYM OTD_NONCLIENT}
  OTD_VALIDBITS         = (OTD_FORCE_RECT_SIZING or OTD_NONCLIENT);
{$EXTERNALSYM OTD_VALIDBITS}

//---------------------------------------------------------------------------
//  OpenThemeDataEx     - Open the theme data for the specified HWND and
//                        semi-colon separated list of class names.
//
//                        OpenThemeData() will try each class name, one at
//                        a time, and use the first matching theme info
//                        found.  If a match is found, a theme handle
//                        to the data is returned.  If no match is found,
//                        a "NULL" handle is returned.
//
//                        When the window is destroyed or a WM_THEMECHANGED
//                        msg is received, "CloseThemeData()" should be
//                        called to close the theme handle.
//
//  hwnd                - window handle of the control/window to be themed
//
//  pszClassList        - class name (or list of names) to match to theme data
//                        section.  if the list contains more than one name,
//                        the names are tested one at a time for a match.
//                        If a match is found, OpenThemeData() returns a
//                        theme handle associated with the matching class.
//                        This param is a list (instead of just a single
//                        class name) to provide the class an opportunity
//                        to get the "best" match between the class and
//                        the current theme.  For example, a button might
//                        pass L"OkButton, Button" if its ID=ID_OK.  If
//                        the current theme has an entry for OkButton,
//                        that will be used.  Otherwise, we fall back on
//                        the normal Button entry.
//
//  dwFlags              - allows certain overrides of std features
//                         (see OTD_XXX defines above)
//---------------------------------------------------------------------------

var
  OpenThemeDataEx: function(hwnd: HWND; pszClassList: LPCWSTR; dwFlags: DWORD): HTHEME; stdcall;
{$EXTERNALSYM OpenThemeDataEx}

//----------------------------------------------------------------------------------------------------------------------
//  CloseTHemeData()    - closes the theme data handle.  This should be done
//                        when the window being themed is destroyed or
//                        whenever a WM_THEMECHANGED msg is received
//                        (followed by an attempt to create a new Theme data
//                        handle).
//
//  hTheme              - open theme data handle (returned from prior call
//                        to OpenThemeData() API).
//----------------------------------------------------------------------------------------------------------------------

var
  CloseThemeData: function(hTheme: HTHEME): HRESULT; stdcall;
{$EXTERNALSYM CloseThemeData}

//----------------------------------------------------------------------------------------------------------------------
//    functions for basic drawing support
//----------------------------------------------------------------------------------------------------------------------
// The following methods are the theme-aware drawing services.
// Controls/Windows are defined in drawable "parts" by their author: a
// parent part and 0 or more child parts.  Each of the parts can be
// described in "states" (ex: disabled, hot, pressed).
//----------------------------------------------------------------------------------------------------------------------
// For the list of all themed classes and the definition of all
// parts and states, see the file "tmschmea.h".
//----------------------------------------------------------------------------------------------------------------------
// Each of the below methods takes a "iPartId" param to specify the
// part and a "iStateId" to specify the state of the part.
// "iStateId=0" refers to the root part.  "iPartId" = "0" refers to
// the root class.
//----------------------------------------------------------------------------------------------------------------------
// Note: draw operations are always scaled to fit (and not to exceed)
// the specified "Rect".
//----------------------------------------------------------------------------------------------------------------------

//----------------------------------------------------------------------------------------------------------------------
//  DrawThemeBackground()
//                      - draws the theme-specified border and fill for
//                        the "iPartId" and "iStateId".  This could be
//                        based on a bitmap file, a border and fill, or
//                        other image description.
//
//  hTheme              - theme data handle
//  hdc                 - HDC to draw into
//  iPartId             - part number to draw
//  iStateId            - state number (of the part) to draw
//  pRect               - defines the size/location of the part
//  pClipRect           - optional clipping rect (don't draw outside it)
//----------------------------------------------------------------------------------------------------------------------

var
  DrawThemeBackground: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; const pRect: TRect;
    pClipRect: PRECT): HRESULT; stdcall;
{$EXTERNALSYM DrawThemeBackground}

//------------------------------------------------------------------------
//---- bits used in dwFlags of DTBGOPTS ----
const
  DTBG_CLIPRECT        = $00000001;  // rcClip has been specified
{$EXTERNALSYM DTBG_CLIPRECT}
  DTBG_DRAWSOLID       = $00000002;  // DEPRECATED: draw transparent/alpha images as solid
{$EXTERNALSYM DTBG_DRAWSOLID}
  DTBG_OMITBORDER      = $00000004;  // don't draw border of part
{$EXTERNALSYM DTBG_OMITBORDER}
  DTBG_OMITCONTENT     = $00000008;  // don't draw content area of part
{$EXTERNALSYM DTBG_OMITCONTENT}
  DTBG_COMPUTINGREGION = $00000010;  // TRUE if calling to compute region
{$EXTERNALSYM DTBG_COMPUTINGREGION}
  DTBG_MIRRORDC        = $00000020;  // assume the hdc is mirrorred and
                                            // flip images as appropriate (currently
                                            // only supported for bgtype=imagefile)
{$EXTERNALSYM DTBG_MIRRORDC}
  DTBG_NOMIRROR        = $00000040;  // don't mirror the output, overrides everything else
{$EXTERNALSYM DTBG_NOMIRROR}
  DTBG_VALIDBITS       = (DTBG_CLIPRECT or
                          DTBG_DRAWSOLID or
                          DTBG_OMITBORDER or
                          DTBG_OMITCONTENT or
                          DTBG_COMPUTINGREGION or
                          DTBG_MIRRORDC or
                          DTBG_NOMIRROR);
{$EXTERNALSYM DTBG_VALIDBITS}

type
  _DTBGOPTS = record
    dwSize: DWORD;           // size of the struct
    dwFlags: DWORD;          // which options have been specified
    rcClip: TRect;            // clipping rectangle
  end;
{$EXTERNALSYM _DTBGOPTS}
  DTBGOPTS = _DTBGOPTS;
{$EXTERNALSYM DTBGOPTS}
  PDTBGOPTS = ^_DTBGOPTS;
{$EXTERNALSYM PDTBGOPTS}
  TDTBgOpts = DTBGOPTS;

//------------------------------------------------------------------------
//  DrawThemeBackgroundEx()
//                      - draws the theme-specified border and fill for
//                        the "iPartId" and "iStateId".  This could be
//                        based on a bitmap file, a border and fill, or
//                        other image description.  NOTE: This will be
//                        merged back into DrawThemeBackground() after
//                        BETA 2.
//
//  hTheme              - theme data handle
//  hdc                 - HDC to draw into
//  iPartId             - part number to draw
//  iStateId            - state number (of the part) to draw
//  pRect               - defines the size/location of the part
//  pOptions            - ptr to optional params
//------------------------------------------------------------------------

var
  DrawThemeBackgroundEx: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; const pRect: TRect;
    pOptions: PDTBGOPTS): HRESULT; stdcall;
{$EXTERNALSYM DrawThemeBackgroundEx}

//----------------------------------------------------------------------------------------------------------------------
//----- DrawThemeText() flags ----

const
  DTT_GRAYED = $1;         // draw a grayed-out string
  {$EXTERNALSYM DTT_GRAYED}

//----------------------------------------------------------------------------------------------------------------------
//  DrawThemeText()     - draws the text using the theme-specified
//                        color and font for the "iPartId" and
//                        "iStateId".
//
//  hTheme              - theme data handle
//  hdc                 - HDC to draw into
//  iPartId             - part number to draw
//  iStateId            - state number (of the part) to draw
//  pszText             - actual text to draw
//  dwCharCount         - number of chars to draw (-1 for all)
//  dwTextFlags         - same as DrawText() "uFormat" param
//  dwTextFlags2        - additional drawing options
//  pRect               - defines the size/location of the part
//----------------------------------------------------------------------------------------------------------------------

var
  DrawThemeText: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; pszText: LPCWSTR; iCharCount: Integer;
    dwTextFlags, dwTextFlags2: DWORD; const pRect: TRect): HRESULT; stdcall;
{$EXTERNALSYM DrawThemeText}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeBackgroundContentRect()
//                      - gets the size of the content for the theme-defined
//                        background.  This is usually the area inside
//                        the borders or Margins.
//
//      hTheme          - theme data handle
//      hdc             - (optional) device content to be used for drawing
//      iPartId         - part number to draw
//      iStateId        - state number (of the part) to draw
//      pBoundingRect   - the outer RECT of the part being drawn
//      pContentRect    - RECT to receive the content area
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeBackgroundContentRect: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer;
    const pBoundingRect: TRect; pContentRect: PRECT): HRESULT; stdcall;
{$EXTERNALSYM GetThemeBackgroundContentRect}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeBackgroundExtent() - calculates the size/location of the theme-
//                               specified background based on the
//                               "pContentRect".
//
//      hTheme          - theme data handle
//      hdc             - (optional) device content to be used for drawing
//      iPartId         - part number to draw
//      iStateId        - state number (of the part) to draw
//      pContentRect    - RECT that defines the content area
//      pBoundingRect   - RECT to receive the overall size/location of part
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeBackgroundExtent: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; const pContentRect: TRect;
    var pExtentRect: TRect): HRESULT; stdcall;
{$EXTERNALSYM GetThemeBackgroundExtent}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeBackgroundRegion()
//                      - computes the region for a regular or partially
//                        transparent theme-specified background that is
//                        bound by the specified "pRect".
//                        If the rectangle is empty, sets the HRGN to NULL
//                        and return S_FALSE.
//
//  hTheme              - theme data handle
//  hdc                 - optional HDC to draw into (DPI scaling)
//  iPartId             - part number to draw
//  iStateId            - state number (of the part)
//  pRect               - the RECT used to draw the part
//  pRegion             - receives handle to calculated region
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeBackgroundRegion: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; const pRect: TRect;
    var pRegion: HRGN): HRESULT; stdcall;
{$EXTERNALSYM GetThemeBackgroundRegion}

//----------------------------------------------------------------------------------------------------------------------

type
  THEMESIZE = (
    TS_MIN,             // minimum size
    TS_TRUE,            // size without stretching
    TS_DRAW             // size that theme mgr will use to draw part
  );
  {$EXTERNALSYM THEMESIZE}
  TThemeSize = THEMESIZE;

//----------------------------------------------------------------------------------------------------------------------
//  GetThemePartSize() - returns the specified size of the theme part
//
//  hTheme              - theme data handle
//  hdc                 - HDC to select font into & measure against
//  iPartId             - part number to retrieve size for
//  iStateId            - state number (of the part)
//  prc                 - (optional) rect for part drawing destination
//  eSize               - the type of size to be retreived
//  psz                 - receives the specified size of the part
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemePartSize: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; prc: PRECT; eSize: THEMESIZE;
    var psz: TSize): HRESULT; stdcall;
{$EXTERNALSYM GetThemePartSize}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeTextExtent() - calculates the size/location of the specified
//                         text when rendered in the Theme Font.
//
//  hTheme              - theme data handle
//  hdc                 - HDC to select font & measure into
//  iPartId             - part number to draw
//  iStateId            - state number (of the part)
//  pszText             - the text to be measured
//  dwCharCount         - number of chars to draw (-1 for all)
//  dwTextFlags         - same as DrawText() "uFormat" param
//  pszBoundingRect     - optional: to control layout of text
//  pszExtentRect       - receives the RECT for text size/location
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeTextExtent: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; pszText: LPCWSTR;
    iCharCount: Integer; dwTextFlags: DWORD; pBoundingRect: PRECT; var pExtentRect: TRect): HRESULT; stdcall;
{$EXTERNALSYM GetThemeTextExtent}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeTextMetrics()
//                      - returns info about the theme-specified font
//                        for the part/state passed in.
//
//  hTheme              - theme data handle
//  hdc                 - optional: HDC for screen context
//  iPartId             - part number to draw
//  iStateId            - state number (of the part)
//  ptm                 - receives the font info
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeTextMetrics: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer;
    var ptm: TEXTMETRIC): HRESULT; stdcall;
{$EXTERNALSYM GetThemeTextMetrics}

//----------------------------------------------------------------------------------------------------------------------
//----- HitTestThemeBackground, HitTestThemeBackgroundRegion flags ----

//  Theme background segment hit test flag (default). possible return values are:
//  HTCLIENT: hit test succeeded in the middle background segment
//  HTTOP, HTLEFT, HTTOPLEFT, etc:  // hit test succeeded in the the respective theme background segment.

const
  HTTB_BACKGROUNDSEG         = $0000;
  {$EXTERNALSYM HTTB_BACKGROUNDSEG}

//  Fixed border hit test option.  possible return values are:
//  HTCLIENT: hit test succeeded in the middle background segment
//  HTBORDER: hit test succeeded in any other background segment

  HTTB_FIXEDBORDER           = $0002;  // Return code may be either HTCLIENT or HTBORDER.
  {$EXTERNALSYM HTTB_FIXEDBORDER}

//  Caption hit test option.  Possible return values are:
//  HTCAPTION: hit test succeeded in the top, top left, or top right background segments
//  HTNOWHERE or another return code, depending on absence or presence of accompanying flags, resp.

  HTTB_CAPTION               = $0004;
  {$EXTERNALSYM HTTB_CAPTION}

//  Resizing border hit test flags.  Possible return values are:
//  HTCLIENT: hit test succeeded in middle background segment
//  HTTOP, HTTOPLEFT, HTLEFT, HTRIGHT, etc:    hit test succeeded in the respective system resizing zone
//  HTBORDER: hit test failed in middle segment and resizing zones, but succeeded in a background border segment

  HTTB_RESIZINGBORDER_LEFT   = $0010; // Hit test left resizing border,
  {$EXTERNALSYM HTTB_RESIZINGBORDER_LEFT}
  HTTB_RESIZINGBORDER_TOP    = $0020; // Hit test top resizing border
  {$EXTERNALSYM HTTB_RESIZINGBORDER_TOP}
  HTTB_RESIZINGBORDER_RIGHT  = $0040; // Hit test right resizing border
  {$EXTERNALSYM HTTB_RESIZINGBORDER_RIGHT}
  HTTB_RESIZINGBORDER_BOTTOM = $0080; // Hit test bottom resizing border
  {$EXTERNALSYM HTTB_RESIZINGBORDER_BOTTOM}

  HTTB_RESIZINGBORDER        = (HTTB_RESIZINGBORDER_LEFT or HTTB_RESIZINGBORDER_TOP or
                                HTTB_RESIZINGBORDER_RIGHT or HTTB_RESIZINGBORDER_BOTTOM);
  {$EXTERNALSYM HTTB_RESIZINGBORDER}

// Resizing border is specified as a template, not just window edges.
// This option is mutually exclusive with HTTB_SYSTEMSIZINGWIDTH; HTTB_SIZINGTEMPLATE takes precedence

  HTTB_SIZINGTEMPLATE        = $0100;
  {$EXTERNALSYM HTTB_SIZINGTEMPLATE}

// Use system resizing border width rather than theme content margins.
// This option is mutually exclusive with HTTB_SIZINGTEMPLATE, which takes precedence.

  HTTB_SYSTEMSIZINGMARGINS   = $0200;
  {$EXTERNALSYM HTTB_SYSTEMSIZINGMARGINS}

//----------------------------------------------------------------------------------------------------------------------
//  HitTestThemeBackground()
//                      - returns a HitTestCode (a subset of the values
//                        returned by WM_NCHITTEST) for the point "ptTest"
//                        within the theme-specified background
//                        (bound by pRect).  "pRect" and "ptTest" should
//                        both be in the same coordinate system
//                        (client, screen, etc).
//
//      hTheme          - theme data handle
//      hdc             - HDC to draw into
//      iPartId         - part number to test against
//      iStateId        - state number (of the part)
//      pRect           - the RECT used to draw the part
//      hrgn            - optional region to use; must be in same coordinates as
//                      -    pRect and pTest.
//      ptTest          - the hit point to be tested
//      dwOptions       - HTTB_xxx constants
//      pwHitTestCode   - receives the returned hit test code - one of:
//
//                        HTNOWHERE, HTLEFT, HTTOPLEFT, HTBOTTOMLEFT,
//                        HTRIGHT, HTTOPRIGHT, HTBOTTOMRIGHT,
//                        HTTOP, HTBOTTOM, HTCLIENT
//----------------------------------------------------------------------------------------------------------------------

var
  HitTestThemeBackground: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; dwOptions: DWORD;
    const pRect: TRect; hrgn: HRGN; ptTest: TPoint; var pwHitTestCode: WORD): HRESULT; stdcall;
{$EXTERNALSYM HitTestThemeBackground}

//----------------------------------------------------------------------------------------------------------------------
//  DrawThemeEdge()     - Similar to the DrawEdge() API, but uses part colors
//                        and is high-DPI aware
//  hTheme              - theme data handle
//  hdc                 - HDC to draw into
//  iPartId             - part number to draw
//  iStateId            - state number of part
//  pDestRect           - the RECT used to draw the line(s)
//  uEdge               - Same as DrawEdge() API
//  uFlags              - Same as DrawEdge() API
//  pContentRect        - Receives the interior rect if (uFlags & BF_ADJUST)
//----------------------------------------------------------------------------------------------------------------------

var
  DrawThemeEdge: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; const pDestRect: TRect; uEdge,
    uFlags: UINT; pContentRect: PRECT): HRESULT; stdcall;
{$EXTERNALSYM DrawThemeEdge}

//----------------------------------------------------------------------------------------------------------------------
//  DrawThemeIcon()     - draws an image within an imagelist based on
//                        a (possible) theme-defined effect.
//
//  hTheme              - theme data handle
//  hdc                 - HDC to draw into
//  iPartId             - part number to draw
//  iStateId            - state number of part
//  pRect               - the RECT to draw the image within
//  himl                - handle to IMAGELIST
//  iImageIndex         - index into IMAGELIST (which icon to draw)
//----------------------------------------------------------------------------------------------------------------------

var
  DrawThemeIcon: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; const pRect: TRect; himl: HIMAGELIST;
    iImageIndex: Integer): HRESULT; stdcall;
{$EXTERNALSYM DrawThemeIcon}

//----------------------------------------------------------------------------------------------------------------------
//  IsThemePartDefined() - returns TRUE if the theme has defined parameters
//                         for the specified "iPartId" and "iStateId".
//
//  hTheme              - theme data handle
//  iPartId             - part number to find definition for
//  iStateId            - state number of part
//----------------------------------------------------------------------------------------------------------------------

var
  IsThemePartDefined: function(hTheme: HTHEME; iPartId, iStateId: Integer): BOOL; stdcall;
{$EXTERNALSYM IsThemePartDefined}

//----------------------------------------------------------------------------------------------------------------------
//  IsThemeBackgroundPartiallyTransparent()
//                      - returns TRUE if the theme specified background for
//                        the part/state has transparent pieces or
//                        alpha-blended pieces.
//
//  hTheme              - theme data handle
//  iPartId             - part number
//  iStateId            - state number of part
//----------------------------------------------------------------------------------------------------------------------

var
  IsThemeBackgroundPartiallyTransparent: function(hTheme: HTHEME; iPartId, iStateId: Integer): BOOL; stdcall;
{$EXTERNALSYM IsThemeBackgroundPartiallyTransparent}

//----------------------------------------------------------------------------------------------------------------------
//    lower-level theme information services
//----------------------------------------------------------------------------------------------------------------------
// The following methods are getter routines for each of the Theme Data types.
// Controls/Windows are defined in drawable "parts" by their author: a
// parent part and 0 or more child parts.  Each of the parts can be
// described in "states" (ex: disabled, hot, pressed).
//----------------------------------------------------------------------------------------------------------------------
// Each of the below methods takes a "iPartId" param to specify the
// part and a "iStateId" to specify the state of the part.
// "iStateId=0" refers to the root part.  "iPartId" = "0" refers to
// the root class.
//----------------------------------------------------------------------------------------------------------------------
// Each method also take a "iPropId" param because multiple instances of
// the same primitive type can be defined in the theme schema.
//----------------------------------------------------------------------------------------------------------------------


//----------------------------------------------------------------------------------------------------------------------
//  GetThemeColor()     - Get the value for the specified COLOR property
//
//  hTheme              - theme data handle
//  iPartId             - part number
//  iStateId            - state number of part
//  iPropId             - the property number to get the value for
//  pColor              - receives the value of the property
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeColor: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer; var pColor: COLORREF): HRESULT; stdcall;
{$EXTERNALSYM GetThemeColor}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeMetric()    - Get the value for the specified metric/size
//                        property
//
//  hTheme              - theme data handle
//  hdc                 - (optional) hdc to be drawn into (DPI scaling)
//  iPartId             - part number
//  iStateId            - state number of part
//  iPropId             - the property number to get the value for
//  piVal               - receives the value of the property
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeMetric: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId, iPropId: Integer;
    var piVal: Integer): HRESULT; stdcall;
{$EXTERNALSYM GetThemeMetric}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeString()    - Get the value for the specified string property
//
//  hTheme              - theme data handle
//  iPartId             - part number
//  iStateId            - state number of part
//  iPropId             - the property number to get the value for
//  pszBuff             - receives the string property value
//  cchMaxBuffChars     - max. number of chars allowed in pszBuff
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeString: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer; pszBuff: LPWSTR;
    cchMaxBuffChars: Integer): HRESULT; stdcall;
{$EXTERNALSYM GetThemeString}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeBool()      - Get the value for the specified BOOL property
//
//  hTheme              - theme data handle
//  iPartId             - part number
//  iStateId            - state number of part
//  iPropId             - the property number to get the value for
//  pfVal               - receives the value of the property
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeBool: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer; var pfVal: BOOL): HRESULT; stdcall;
{$EXTERNALSYM GetThemeBool}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeInt()       - Get the value for the specified int property
//
//  hTheme              - theme data handle
//  iPartId             - part number
//  iStateId            - state number of part
//  iPropId             - the property number to get the value for
//  piVal               - receives the value of the property
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeInt: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer; var piVal: Integer): HRESULT; stdcall;
{$EXTERNALSYM GetThemeInt}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeEnumValue() - Get the value for the specified ENUM property
//
//  hTheme              - theme data handle
//  iPartId             - part number
//  iStateId            - state number of part
//  iPropId             - the property number to get the value for
//  piVal               - receives the value of the enum (cast to int*)
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeEnumValue: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer; var piVal: Integer): HRESULT; stdcall;
{$EXTERNALSYM GetThemeEnumValue}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemePosition()  - Get the value for the specified position
//                        property
//
//  hTheme              - theme data handle
//  iPartId             - part number
//  iStateId            - state number of part
//  iPropId             - the property number to get the value for
//  pPoint              - receives the value of the position property
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemePosition: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;var pPoint: TPoint): HRESULT; stdcall;
{$EXTERNALSYM GetThemePosition}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeFont()      - Get the value for the specified font property
//
//  hTheme              - theme data handle
//  hdc                 - (optional) hdc to be drawn to (DPI scaling)
//  iPartId             - part number
//  iStateId            - state number of part
//  iPropId             - the property number to get the value for
//  pFont               - receives the value of the LOGFONT property
//                        (scaled for the current logical screen dpi)
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeFont: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId, iPropId: Integer;
    var pFont: LOGFONT): HRESULT; stdcall;
{$EXTERNALSYM GetThemeFont}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeRect()      - Get the value for the specified RECT property
//
//  hTheme              - theme data handle
//  iPartId             - part number
//  iStateId            - state number of part
//  iPropId             - the property number to get the value for
//  pRect               - receives the value of the RECT property
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeRect: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer; var pRect: TRect): HRESULT; stdcall;
{$EXTERNALSYM GetThemeRect}

//----------------------------------------------------------------------------------------------------------------------

type
  _MARGINS = record
    cxLeftWidth: Integer;      // width of left border that retains its size
    cxRightWidth: Integer;     // width of right border that retains its size
    cyTopHeight: Integer;      // height of top border that retains its size
    cyBottomHeight: Integer;   // height of bottom border that retains its size
  end;
  {$EXTERNALSYM _MARGINS}
  MARGINS = _MARGINS;
  {$EXTERNALSYM MARGINS}
  PMARGINS = ^MARGINS;
  {$EXTERNALSYM PMARGINS}
  TMargins = MARGINS;

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeMargins()   - Get the value for the specified MARGINS property
//
//      hTheme          - theme data handle
//      hdc             - (optional) hdc to be used for drawing
//      iPartId         - part number
//      iStateId        - state number of part
//      iPropId         - the property number to get the value for
//      prc             - RECT for area to be drawn into
//      pMargins        - receives the value of the MARGINS property
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeMargins: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId, iPropId: Integer; prc: PRECT;
    var pMargins: MARGINS): HRESULT; stdcall;
{$EXTERNALSYM GetThemeMargins}

//----------------------------------------------------------------------------------------------------------------------

(* how to translate?
{$IF _WIN32_WINNT >= 0x0600}
  MAX_INTLIST_COUNT = 402;
{$ELSE}
  MAX_INTLIST_COUNT = 10;
{$ENDIF}
*)
const
  MAX_INTLIST_COUNT = 10;
  {$EXTERNALSYM MAX_INTLIST_COUNT}

type
  _INTLIST = record
    iValueCount: Integer;      // number of values in iValues
    iValues: array [0..MAX_INTLIST_COUNT - 1] of Integer;
  end;
  {$EXTERNALSYM _INTLIST}
  INTLIST = _INTLIST;
  {$EXTERNALSYM INTLIST}
  PINTLIST = ^INTLIST;
  {$EXTERNALSYM PINTLIST}
  TIntList = INTLIST;

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeIntList()   - Get the value for the specified INTLIST struct
//
//      hTheme          - theme data handle
//      iPartId         - part number
//      iStateId        - state number of part
//      iPropId         - the property number to get the value for
//      pIntList        - receives the value of the INTLIST property
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeIntList: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer; var pIntList: INTLIST): HRESULT; stdcall;
{$EXTERNALSYM GetThemeIntList}

//----------------------------------------------------------------------------------------------------------------------

type
  PROPERTYORIGIN = (
    PO_STATE,           // property was found in the state section
    PO_PART,            // property was found in the part section
    PO_CLASS,           // property was found in the class section
    PO_GLOBAL,          // property was found in [globals] section
    PO_NOTFOUND);       // property was not found
  {$EXTERNALSYM PROPERTYORIGIN}
  TPropertyOrigin = PROPERTYORIGIN;

//----------------------------------------------------------------------------------------------------------------------
//  GetThemePropertyOrigin()
//                      - searches for the specified theme property
//                        and sets "pOrigin" to indicate where it was
//                        found (or not found)
//
//  hTheme              - theme data handle
//  iPartId             - part number
//  iStateId            - state number of part
//  iPropId             - the property number to search for
//  pOrigin             - receives the value of the property origin
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemePropertyOrigin: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
    var pOrigin: PROPERTYORIGIN): HRESULT; stdcall;
{$EXTERNALSYM GetThemePropertyOrigin}

//----------------------------------------------------------------------------------------------------------------------
//  SetWindowTheme()
//                      - redirects an existing Window to use a different
//                        section of the current theme information than its
//                        class normally asks for.
//
//  hwnd                - the handle of the window (cannot be NULL)
//
//  pszSubAppName       - app (group) name to use in place of the calling
//                        app's name.  If NULL, the actual calling app
//                        name will be used.
//
//  pszSubIdList        - semicolon separated list of class Id names to
//                        use in place of actual list passed by the
//                        window's class.  if NULL, the id list from the
//                        calling class is used.
//----------------------------------------------------------------------------------------------------------------------
// The Theme Manager will remember the "pszSubAppName" and the
// "pszSubIdList" associations thru the lifetime of the window (even
// if themes are subsequently changed).  The window is sent a
// "WM_THEMECHANGED" msg at the end of this call, so that the new
// theme can be found and applied.
//----------------------------------------------------------------------------------------------------------------------
// When "pszSubAppName" or "pszSubIdList" are NULL, the Theme Manager
// removes the previously remember association.  To turn off theme-ing for
// the specified window, you can pass an empty string (L"") so it
// won't match any section entries.
//----------------------------------------------------------------------------------------------------------------------

var
  SetWindowTheme: function(hwnd: HWND; pszSubAppName: LPCWSTR; pszSubIdList: LPCWSTR): HRESULT; stdcall;
{$EXTERNALSYM SetWindowTheme}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeFilename()  - Get the value for the specified FILENAME property.
//
//  hTheme              - theme data handle
//  iPartId             - part number
//  iStateId            - state number of part
//  iPropId             - the property number to search for
//  pszThemeFileName    - output buffer to receive the filename
//  cchMaxBuffChars     - the size of the return buffer, in chars
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeFilename: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer; pszThemeFileName: LPWSTR;
    cchMaxBuffChars: Integer): HRESULT; stdcall;
{$EXTERNALSYM GetThemeFilename}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeSysColor()  - Get the value of the specified System color.
//
//  hTheme              - the theme data handle.  if non-NULL, will return
//                        color from [SysMetrics] section of theme.
//                        if NULL, will return the global system color.
//
//  iColorId            - the system color index defined in winuser.h
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeSysColor: function(hTheme: HTHEME; iColorId: Integer): COLORREF; stdcall;
{$EXTERNALSYM GetThemeSysColor}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeSysColorBrush()
//                      - Get the brush for the specified System color.
//
//  hTheme              - the theme data handle.  if non-NULL, will return
//                        brush matching color from [SysMetrics] section of
//                        theme.  if NULL, will return the brush matching
//                        global system color.
//
//  iColorId            - the system color index defined in winuser.h
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeSysColorBrush: function(hTheme: HTHEME; iColorId: Integer): HBRUSH; stdcall;
{$EXTERNALSYM GetThemeSysColorBrush}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeSysBool()   - Get the boolean value of specified System metric.
//
//  hTheme              - the theme data handle.  if non-NULL, will return
//                        BOOL from [SysMetrics] section of theme.
//                        if NULL, will return the specified system boolean.
//
//  iBoolId             - the TMT_XXX BOOL number (first BOOL
//                        is TMT_FLATMENUS)
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeSysBool: function(hTheme: HTHEME; iBoolId: Integer): BOOL; stdcall;
{$EXTERNALSYM GetThemeSysBool}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeSysSize()   - Get the value of the specified System size metric.
//                        (scaled for the current logical screen dpi)
//
//  hTheme              - the theme data handle.  if non-NULL, will return
//                        size from [SysMetrics] section of theme.
//                        if NULL, will return the global system metric.
//
//  iSizeId             - the following values are supported when
//                        hTheme is non-NULL:
//
//                          SM_CXBORDER   (border width)
//                          SM_CXVSCROLL  (scrollbar width)
//                          SM_CYHSCROLL  (scrollbar height)
//                          SM_CXSIZE     (caption width)
//                          SM_CYSIZE     (caption height)
//                          SM_CXSMSIZE   (small caption width)
//                          SM_CYSMSIZE   (small caption height)
//                          SM_CXMENUSIZE (menubar width)
//                          SM_CYMENUSIZE (menubar height)
//
//                        when hTheme is NULL, iSizeId is passed directly
//                        to the GetSystemMetrics() function
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeSysSize: function(hTheme: HTHEME; iSizeId: Integer): Integer; stdcall;
{$EXTERNALSYM GetThemeSysSize}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeSysFont()   - Get the LOGFONT for the specified System font.
//
//  hTheme              - the theme data handle.  if non-NULL, will return
//                        font from [SysMetrics] section of theme.
//                        if NULL, will return the specified system font.
//
//  iFontId             - the TMT_XXX font number (first font
//                        is TMT_CAPTIONFONT)
//
//  plf                 - ptr to LOGFONT to receive the font value.
//                        (scaled for the current logical screen dpi)
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeSysFont: function(hTheme: HTHEME; iFontId: Integer; var plf: LOGFONT): HRESULT; stdcall;
{$EXTERNALSYM GetThemeSysFont}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeSysString() - Get the value of specified System string metric.
//
//  hTheme              - the theme data handle (required)
//
//  iStringId           - must be one of the following values:
//
//                          TMT_CSSNAME
//                          TMT_XMLNAME
//
//  pszStringBuff       - the buffer to receive the string value
//
//  cchMaxStringChars   - max. number of chars that pszStringBuff can hold
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeSysString: function(hTheme: HTHEME; iStringId: Integer; pszStringBuff: LPWSTR;
    cchMaxStringChars: Integer): HRESULT; stdcall;
{$EXTERNALSYM GetThemeSysString}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeSysInt() - Get the value of specified System int.
//
//  hTheme              - the theme data handle (required)
//
//  iIntId              - must be one of the following values:
//
//                          TMT_DPIX
//                          TMT_DPIY
//                          TMT_MINCOLORDEPTH
//
//  piValue             - ptr to int to receive value
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeSysInt: function(hTheme: HTHEME; iIntId: Integer; var piValue: Integer): HRESULT; stdcall;
{$EXTERNALSYM GetThemeSysInt}

//----------------------------------------------------------------------------------------------------------------------
//  IsThemeActive()     - can be used to test if a system theme is active
//                        for the current user session.
//
//                        use the API "IsAppThemed()" to test if a theme is
//                        active for the calling process.
//----------------------------------------------------------------------------------------------------------------------

var
  IsThemeActive: function: BOOL; stdcall;
{$EXTERNALSYM IsThemeActive}

//----------------------------------------------------------------------------------------------------------------------
//  IsAppThemed()       - returns TRUE if a theme is active and available to
//                        the current process
//----------------------------------------------------------------------------------------------------------------------

var
  IsAppThemed: function: BOOL; stdcall;
{$EXTERNALSYM IsAppThemed}

//----------------------------------------------------------------------------------------------------------------------
//  GetWindowTheme()    - if window is themed, returns its most recent
//                        HTHEME from OpenThemeData() - otherwise, returns
//                        NULL.
//
//      hwnd            - the window to get the HTHEME of
//----------------------------------------------------------------------------------------------------------------------

var
  GetWindowTheme: function(hwnd: HWND): HTHEME; stdcall;
{$EXTERNALSYM GetWindowTheme}

//----------------------------------------------------------------------------------------------------------------------
//  EnableThemeDialogTexture()
//
//  - Enables/disables dialog background theme.  This method can be used to
//    tailor dialog compatibility with child windows and controls that
//    may or may not coordinate the rendering of their client area backgrounds
//    with that of their parent dialog in a manner that supports seamless
//    background texturing.
//
//      hdlg         - the window handle of the target dialog
//      dwFlags      - ETDT_ENABLE to enable the theme-defined dialog background texturing,
//                     ETDT_DISABLE to disable background texturing,
//                     ETDT_ENABLETAB to enable the theme-defined background
//                          texturing using the Tab texture
//----------------------------------------------------------------------------------------------------------------------

const
  ETDT_DISABLE       = $00000001;
  {$EXTERNALSYM ETDT_DISABLE}
  ETDT_ENABLE        = $00000002;
  {$EXTERNALSYM ETDT_ENABLE}
  ETDT_USETABTEXTURE = $00000004;
  {$EXTERNALSYM ETDT_USETABTEXTURE}
  ETDT_ENABLETAB     = (ETDT_ENABLE or ETDT_USETABTEXTURE);
  {$EXTERNALSYM ETDT_ENABLETAB}

var
  EnableThemeDialogTexture: function(hwnd: HWND; dwFlags: DWORD): HRESULT; stdcall;
{$EXTERNALSYM EnableThemeDialogTexture}

//----------------------------------------------------------------------------------------------------------------------
//  IsThemeDialogTextureEnabled()
//
//  - Reports whether the dialog supports background texturing.
//
//      hdlg         - the window handle of the target dialog
//----------------------------------------------------------------------------------------------------------------------

var
  IsThemeDialogTextureEnabled: function(hwnd: HWND): BOOL; stdcall;
{$EXTERNALSYM IsThemeDialogTextureEnabled}

//----------------------------------------------------------------------------------------------------------------------
//---- flags to control theming within an app ----

const
  STAP_ALLOW_NONCLIENT   = (1 shl 0);
  {$EXTERNALSYM STAP_ALLOW_NONCLIENT}
  STAP_ALLOW_CONTROLS    = (1 shl 1);
  {$EXTERNALSYM STAP_ALLOW_CONTROLS}
  STAP_ALLOW_WEBCONTENT  = (1 shl 2);
  {$EXTERNALSYM STAP_ALLOW_WEBCONTENT}
  STAP_VALIDBITS         = (STAP_ALLOW_NONCLIENT or STAP_ALLOW_CONTROLS or STAP_ALLOW_WEBCONTENT);
  {$EXTERNALSYM STAP_VALIDBITS}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeAppProperties()
//                      - returns the app property flags that control theming
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeAppProperties: function: DWORD; stdcall;
{$EXTERNALSYM GetThemeAppProperties}

//----------------------------------------------------------------------------------------------------------------------
//  SetThemeAppProperties()
//                      - sets the flags that control theming within the app
//
//      dwFlags         - the flag values to be set
//----------------------------------------------------------------------------------------------------------------------

var
  SetThemeAppProperties: procedure(dwFlags: DWORD); stdcall;
{$EXTERNALSYM SetThemeAppProperties}

//----------------------------------------------------------------------------------------------------------------------
//  GetCurrentThemeName()
//                      - Get the name of the current theme in-use.
//                        Optionally, return the ColorScheme name and the
//                        Size name of the theme.
//
//  pszThemeFileName    - receives the theme path & filename
//  cchMaxNameChars     - max chars allowed in pszNameBuff
//
//  pszColorBuff        - (optional) receives the canonical color scheme name
//                        (not the display name)
//  cchMaxColorChars    - max chars allowed in pszColorBuff
//
//  pszSizeBuff         - (optional) receives the canonical size name
//                        (not the display name)
//  cchMaxSizeChars     - max chars allowed in pszSizeBuff
//----------------------------------------------------------------------------------------------------------------------

var
  GetCurrentThemeName: function(pszThemeFileName: LPWSTR; cchMaxNameChars: Integer; pszColorBuff: LPWSTR;
    cchMaxColorChars: Integer; pszSizeBuff: LPWSTR; cchMaxSizeChars: Integer): HRESULT; stdcall;
{$EXTERNALSYM GetCurrentThemeName}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeDocumentationProperty()
//                      - Get the value for the specified property name from
//                        the [documentation] section of the themes.ini file
//                        for the specified theme.  If the property has been
//                        localized in the theme files string table, the
//                        localized version of the property value is returned.
//
//  pszThemeFileName    - filename of the theme file to query
//  pszPropertyName     - name of the string property to retreive a value for
//  pszValueBuff        - receives the property string value
//  cchMaxValChars      - max chars allowed in pszValueBuff
//----------------------------------------------------------------------------------------------------------------------

const
  SZ_THDOCPROP_DISPLAYNAME               = WideString('DisplayName');
  {$EXTERNALSYM SZ_THDOCPROP_DISPLAYNAME}
  SZ_THDOCPROP_CANONICALNAME             = WideString('ThemeName');
  {$EXTERNALSYM SZ_THDOCPROP_CANONICALNAME}
  SZ_THDOCPROP_TOOLTIP                   = WideString('ToolTip');
  {$EXTERNALSYM SZ_THDOCPROP_TOOLTIP}
  SZ_THDOCPROP_AUTHOR                    = WideString('author');
  {$EXTERNALSYM SZ_THDOCPROP_AUTHOR}

var
  GetThemeDocumentationProperty: function(pszThemeName, pszPropertyName: LPCWSTR; pszValueBuff: LPWSTR;
    cchMaxValChars: Integer): HRESULT; stdcall;
{$EXTERNALSYM GetThemeDocumentationProperty}

//----------------------------------------------------------------------------------------------------------------------
//  Theme API Error Handling
//
//      All functions in the Theme API not returning an HRESULT (THEMEAPI_)
//      use the WIN32 function "SetLastError()" to record any call failures.
//
//      To retreive the error code of the last failure on the
//      current thread for these type of API's, use the WIN32 function
//      "GetLastError()".
//
//      All Theme API error codes (HRESULT's and GetLastError() values)
//      should be normal win32 errors which can be formatted into
//      strings using the Win32 API FormatMessage().
//----------------------------------------------------------------------------------------------------------------------

//----------------------------------------------------------------------------------------------------------------------
// DrawThemeParentBackground()
//                      - used by partially-transparent or alpha-blended
//                        child controls to draw the part of their parent
//                        that they appear in front of.
//
//  hwnd                - handle of the child control
//  hdc                 - hdc of the child control
//  prc                 - (optional) rect that defines the area to be
//                        drawn (CHILD coordinates)
//----------------------------------------------------------------------------------------------------------------------

var
  DrawThemeParentBackground: function(hwnd: HWND; hdc: HDC; prc: PRECT): HRESULT; stdcall;
{$EXTERNALSYM DrawThemeParentBackground}

//----------------------------------------------------------------------------------------------------------------------
//  EnableTheming()     - enables or disables themeing for the current user
//                        in the current and future sessions.
//
//  fEnable             - if FALSE, disable theming & turn themes off.
//                      - if TRUE, enable themeing and, if user previously
//                        had a theme active, make it active now.
//----------------------------------------------------------------------------------------------------------------------

var
  EnableTheming: function(fEnable: BOOL): HRESULT; stdcall;
{$EXTERNALSYM EnableTheming}

const
  GBF_DIRECT    = $00000001;      // direct dereferencing.
{$EXTERNALSYM GBF_DIRECT}
  GBF_COPY      = $00000002;      // create a copy of the bitmap
{$EXTERNALSYM GBF_COPY}
  GBF_VALIDBITS = (GBF_DIRECT or GBF_COPY);
{$EXTERNALSYM GBF_VALIDBITS}

// if (_WIN32_WINNT >= 0x0600)
const
  DTPB_WINDOWDC          = $00000001;
{$EXTERNALSYM DTPB_WINDOWDC}
  DTPB_USECTLCOLORSTATIC = $00000002;
{$EXTERNALSYM DTPB_USECTLCOLORSTATIC}
  DTPB_USEERASEBKGND     = $00000004;
{$EXTERNALSYM DTPB_USEERASEBKGND}

//---------------------------------------------------------------------------
// DrawThemeParentBackgroundEx()
//                      - used by partially-transparent or alpha-blended
//                        child controls to draw the part of their parent
//                        that they appear in front of.
//                        Sends a WM_ERASEBKGND message followed by a WM_PRINTCLIENT.
//
//  hwnd                - handle of the child control
//
//  hdc                 - hdc of the child control
//
//  dwFlags             - if 0, only returns S_OK if the parent handled
//                        WM_PRINTCLIENT.
//                      - if DTPB_WINDOWDC is set, hdc is assumed to be a window DC,
//                        not a client DC.
//                      - if DTPB_USEERASEBKGND is set, the function will return S_OK
//                        without sending a WM_CTLCOLORSTATIC message if the parent
//                        actually painted on WM_ERASEBKGND.
//                      - if DTPB_CTLCOLORSTATIC is set, the function will send
//                        a WM_CTLCOLORSTATIC message to the parent and use the
//                        brush if one is provided, else COLOR_BTNFACE.
//
//  prc                 - (optional) rect that defines the area to be
//                        drawn (CHILD coordinates)
//
//  Return value        - S_OK if something was painted, S_FALSE if not.
//---------------------------------------------------------------------------

var
  DrawThemeParentBackgroundEx: function(hwnd: HWND; hdc: HDC; dwFlags: DWORD; prc: PRect): HRESULT; stdcall;
{$EXTERNALSYM DrawThemeParentBackgroundEx}

const
  WTA_NONCLIENT = 1;
{$EXTERNALSYM WTA_NONCLIENT}

type
  _WTA_OPTIONS = record
    dwFlags: DWORD;          // values for each style option specified in the bitmask
    dwMask: DWORD;           // bitmask for flags that are changing
                             // valid options are: WTNCA_NODRAWCAPTION, WTNCA_NODRAWICON, WTNCA_NOSYSMENU
  end;
{$EXTERNALSYM _WTA_OPTIONS}
  WTA_OPTIONS = _WTA_OPTIONS;
{$EXTERNALSYM WTA_OPTIONS}
  PWTA_OPTIONS = ^_WTA_OPTIONS;
{$EXTERNALSYM PWTA_OPTIONS}
  TWTA_Options = WTA_OPTIONS;

const
  WTNCA_NODRAWCAPTION = $00000001;    // don't draw the window caption
{$EXTERNALSYM WTNCA_NODRAWCAPTION}
  WTNCA_NODRAWICON    = $00000002;    // don't draw the system icon
{$EXTERNALSYM WTNCA_NODRAWICON}
  WTNCA_NOSYSMENU     = $00000004;    // don't expose the system menu icon functionality
{$EXTERNALSYM WTNCA_NOSYSMENU}
  WTNCA_NOMIRRORHELP  = $00000008;    // don't mirror the question mark, even in RTL layout
{$EXTERNALSYM WTNCA_NOMIRRORHELP}
  WTNCA_VALIDBITS     = (WTNCA_NODRAWCAPTION or WTNCA_NODRAWICON or WTNCA_NOSYSMENU or WTNCA_NOMIRRORHELP);
{$EXTERNALSYM WTNCA_VALIDBITS}

var
  SetWindowThemeAttribute: function(hwnd: HWND; eAttribute: LongWord; pvAttribute: Pointer; cbAttribute: DWORD): HRESULT; stdcall;
{$EXTERNALSYM SetWindowThemeAttribute}

function SetWindowThemeNonClientAttributes(hwnd: HWND; dwMask: DWORD; dwAttributes: DWORD): HRESULT;
{$EXTERNALSYM SetWindowThemeNonClientAttributes}

// endif // if (_WIN32_WINNT >= 0x0600)

//---------------------------------------------------------------------------
//
// DrawThemeTextEx
//
// Note: DrawThemeTextEx only exists on Windows Vista and higher, but the
// following declarations are provided to enable declaring its prototype when
// compiling for all platforms.

// Callback function used by DrawThemeTextEx, instead of DrawText
type
  DTT_CALLBACK_PROC = function(hdc: HDC; pszText: LPCWSTR; cchText: Integer; prc: PRect;
    dwFlags: UINT; lParam: LPARAM): Integer; stdcall;
{$EXTERNALSYM DTT_CALLBACK_PROC}

//---- bits used in dwFlags of DTTOPTS ----
const
  DTT_TEXTCOLOR    = (1 shl 0);      // crText has been specified
{$EXTERNALSYM DTT_TEXTCOLOR}
  DTT_BORDERCOLOR  = (1 shl 1);      // crBorder has been specified
{$EXTERNALSYM DTT_BORDERCOLOR}
  DTT_SHADOWCOLOR  = (1 shl 2);      // crShadow has been specified
{$EXTERNALSYM DTT_SHADOWCOLOR}
  DTT_SHADOWTYPE   = (1 shl 3);      // iTextShadowType has been specified
{$EXTERNALSYM DTT_SHADOWTYPE}
  DTT_SHADOWOFFSET = (1 shl 4);      // ptShadowOffset has been specified
{$EXTERNALSYM DTT_SHADOWOFFSET}
  DTT_BORDERSIZE   = (1 shl 5);      // iBorderSize has been specified
{$EXTERNALSYM DTT_BORDERSIZE}
  DTT_FONTPROP     = (1 shl 6);      // iFontPropId has been specified
{$EXTERNALSYM DTT_FONTPROP}
  DTT_COLORPROP    = (1 shl 7);      // iColorPropId has been specified
{$EXTERNALSYM DTT_COLORPROP}
  DTT_STATEID      = (1 shl 8);      // IStateId has been specified
{$EXTERNALSYM DTT_STATEID}
  DTT_CALCRECT     = (1 shl 9);      // Use pRect as and in/out parameter
{$EXTERNALSYM DTT_CALCRECT}
  DTT_APPLYOVERLAY = (1 shl 10);     // fApplyOverlay has been specified
{$EXTERNALSYM DTT_APPLYOVERLAY}
  DTT_GLOWSIZE     = (1 shl 11);     // iGlowSize has been specified
{$EXTERNALSYM DTT_GLOWSIZE}
  DTT_CALLBACK     = (1 shl 12);     // pfnDrawTextCallback has been specified
{$EXTERNALSYM DTT_CALLBACK}
  DTT_COMPOSITED   = (1 shl 13);     // Draws text with antialiased alpha (needs a DIB section)
{$EXTERNALSYM DTT_COMPOSITED}
  DTT_VALIDBITS    = (DTT_TEXTCOLOR or DTT_BORDERCOLOR or DTT_SHADOWCOLOR or
                      DTT_SHADOWTYPE or DTT_SHADOWOFFSET or DTT_BORDERSIZE or
                      DTT_FONTPROP or DTT_COLORPROP or DTT_STATEID or
                      DTT_CALCRECT or DTT_APPLYOVERLAY or DTT_GLOWSIZE or
                      DTT_COMPOSITED);
{$EXTERNALSYM DTT_VALIDBITS}

type
  _DTTOPTS = record
    dwSize: DWORD;                          // size of the struct
    dwFlags: DWORD;                         // which options have been specified
    crText: COLORREF;                       // color to use for text fill
    crBorder: COLORREF;                     // color to use for text outline
    crShadow: COLORREF;                     // color to use for text shadow
    iTextShadowType: Integer;               // TST_SINGLE or TST_CONTINUOUS
    ptShadowOffset: TPoint;                 // where shadow is drawn (relative to text)
    iBorderSize: Integer;                   // Border radius around text
    iFontPropId: Integer;                   // Font property to use for the text instead of TMT_FONT
    iColorPropId: Integer;                  // Color property to use for the text instead of TMT_TEXTCOLOR
    iStateId: Integer;                      // Alternate state id
    fApplyOverlay: BOOL;                    // Overlay text on top of any text effect?
    iGlowSize: Integer;                     // Glow radious around text
    pfnDrawTextCallback: DTT_CALLBACK_PROC; // Callback for DrawText
    lParam: LPARAM;                         // Parameter for callback
  end;
{$EXTERNALSYM _DTTOPTS}
  DTTOPTS = _DTTOPTS;
{$EXTERNALSYM DTTOPTS}
  PDTTOPTS = ^_DTTOPTS;
{$EXTERNALSYM PDTTOPTS}
  TDTTOpts = DTTOPTS;

// if (_WIN32_WINNT >= 0x0600)

var
  DrawThemeTextEx: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; pszText: LPCWSTR;
    cchText: Integer; dwTextFlags: DWORD; pRect: PRect; pOptions: PDTTOPTS): HRESULT; stdcall;
{$EXTERNALSYM DrawThemeTextEx}

//-----------------------------------------------------------------------
//  GetThemeStream() - Get the value for the specified STREAM property
//
//      hTheme      - theme data handle
//      iPartId     - part number
//      iStateId    - state number of part
//      iPropId     - the property number to get the value for
//      ppvStream   - if non-null receives the value of the STREAM property (not to be freed)
//      pcbStream   - if non-null receives the size of the STREAM property
//      hInst       - NULL when iPropId==TMT_STREAM, HINSTANCE of a loaded msstyles
//                    file when iPropId==TMT_DISKSTREAM (use GetCurrentThemeName
//                    and LoadLibraryEx(LOAD_LIBRARY_AS_DATAFILE)
//-----------------------------------------------------------------------

var
  GetThemeBitmap: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer; dwFlags: ULONG;
    var phBitmap: HBITMAP): HRESULT; stdcall;
{$EXTERNALSYM GetThemeBitmap}

//-----------------------------------------------------------------------
//  GetThemeStream() - Get the value for the specified STREAM property
//
//      hTheme      - theme data handle
//      iPartId     - part number
//      iStateId    - state number of part
//      iPropId     - the property number to get the value for
//      ppvStream   - if non-null receives the value of the STREAM property (not to be freed)
//      pcbStream   - if non-null receives the size of the STREAM property
//      hInst       - NULL when iPropId==TMT_STREAM, HINSTANCE of a loaded msstyles
//                    file when iPropId==TMT_DISKSTREAM (use GetCurrentThemeName
//                    and LoadLibraryEx(LOAD_LIBRARY_AS_DATAFILE)
//-----------------------------------------------------------------------

var
  GetThemeStream: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer; ppvStream: PPointer;
    pcbStream: PDWORD; hInst: HINST): HRESULT; stdcall;
{$EXTERNALSYM GetThemeStream}

//------------------------------------------------------------------------
//  BufferedPaintInit() - Initialize the Buffered Paint API.
//                        Should be called prior to BeginBufferedPaint,
//                        and should have a matching BufferedPaintUnInit.
//------------------------------------------------------------------------
var
  BufferedPaintInit: function: HRESULT; stdcall;
{$EXTERNALSYM BufferedPaintInit}

//------------------------------------------------------------------------
//  BufferedPaintUnInit() - Uninitialize the Buffered Paint API.
//                          Should be called once for each call to BufferedPaintInit,
//                          when calls to BeginBufferedPaint are no longer needed.
//------------------------------------------------------------------------

var
  BufferedPaintUnInit: function: HRESULT; stdcall;
{$EXTERNALSYM BufferedPaintUnInit}

//------------------------------------------------------------------------
//  BeginBufferedPaint() - Begins a buffered paint operation.
//
//    hdcTarget          - Target DC on which the buffer will be painted
//    rcTarget           - Rectangle specifying the area of the target DC to paint to
//    dwFormat           - Format of the buffer (see BP_BUFFERFORMAT)
//    pPaintParams       - Paint operation parameters (see BP_PAINTPARAMS)
//    phBufferedPaint    - Pointer to receive handle to new buffered paint context
//------------------------------------------------------------------------

// HPAINTBUFFER
type
  HPAINTBUFFER = HANDLE;  // handle to a buffered paint context
{$EXTERNALSYM HPAINTBUFFER}

// BP_BUFFERFORMAT
const
  BPBF_COMPATIBLEBITMAP = 0;    // Compatible bitmap
{$EXTERNALSYM BPBF_COMPATIBLEBITMAP}
  BPBF_DIB              = 1;    // Device-independent bitmap
{$EXTERNALSYM BPBF_DIB}
  BPBF_TOPDOWNDIB       = 2;    // Top-down device-independent bitmap
{$EXTERNALSYM BPBF_TOPDOWNDIB}
  BPBF_TOPDOWNMONODIB   = 3;    // Top-down monochrome device-independent bitmap
{$EXTERNALSYM BPBF_TOPDOWNMONODIB}
  BPBF_COMPOSITED       = BPBF_TOPDOWNDIB;
{$EXTERNALSYM BPBF_COMPOSITED}


// BP_ANIMATIONSTYLE
const
  BPAS_NONE   = 0;              // No animation
{$EXTERNALSYM BPAS_NONE}
  BPAS_LINEAR = 1;              // Linear fade animation
{$EXTERNALSYM BPAS_LINEAR}
  BPAS_CUBIC  = 2;              // Cubic fade animation
{$EXTERNALSYM BPAS_CUBIC}
  BPAS_SINE   = 3;              // Sinusoid fade animation
{$EXTERNALSYM BPAS_SINE}

// BP_ANIMATIONPARAMS
type
   _BP_ANIMATIONPARAMS = record
    cbSize: DWORD;
    dwFlags: DWORD;           // BPAF_ flags
    style: LongWord;          // BP_ANIMATIONSTYLE
    dwDuration: DWORD;
  end;
{$EXTERNALSYM _BP_ANIMATIONPARAMS}
  BP_ANIMATIONPARAMS = _BP_ANIMATIONPARAMS;
{$EXTERNALSYM BP_ANIMATIONPARAMS}
  PBP_ANIMATIONPARAMS = ^_BP_ANIMATIONPARAMS;
{$EXTERNALSYM BP_ANIMATIONPARAMS}
  TBP_AnimationParams = _BP_ANIMATIONPARAMS;

const
  BPPF_ERASE     = $0001; // Empty the buffer during BeginBufferedPaint()
{$EXTERNALSYM BPPF_ERASE}
  BPPF_NOCLIP    = $0002; // Don't apply the target DC's clip region to the double buffer
{$EXTERNALSYM BPPF_NOCLIP}
  BPPF_NONCLIENT = $0004; // Using a non-client DC
{$EXTERNALSYM BPPF_NONCLIENT}

// BP_PAINTPARAMS
type
  _BP_PAINTPARAMS = record
    cbSize: DWORD;
    dwFlags: DWORD;                // BPPF_ flags
    prcExclude: PRect;
    pBlendFunction: PBLENDFUNCTION;
  end;
{$EXTERNALSYM _BP_PAINTPARAMS}
  BP_PAINTPARAMS = _BP_PAINTPARAMS;
{$EXTERNALSYM BP_PAINTPARAMS}
  PBP_PAINTPARAMS = ^_BP_PAINTPARAMS;
{$EXTERNALSYM PBP_PAINTPARAMS}
  TBP_PaintParams = _BP_PAINTPARAMS;

var
  BeginBufferedPaint: function(hdcTarget: HDC; prcTarget: PRECT; dwFormat: LongWord;
    pPaintParams: PBP_PAINTPARAMS; var phdc: HDC): HPAINTBUFFER; stdcall;
{$EXTERNALSYM BeginBufferedPaint}

//------------------------------------------------------------------------
//  EndBufferedPaint() - Ends a buffered paint operation.
//
//    hBufferedPaint   - handle to buffered paint context
//    fUpdateTarget    - update target DC
//------------------------------------------------------------------------

var
  EndBufferedPaint: function(hBufferedPaint: HPAINTBUFFER; fUpdateTarget: BOOL): HRESULT; stdcall;
{$EXTERNALSYM EndBufferedPaint}

//------------------------------------------------------------------------
//  GetBufferedPaintTargetRect() - Returns the target rectangle specified during BeginBufferedPaint
//
//    hBufferedPaint             - handle to buffered paint context
//    prc                        - pointer to receive target rectangle
//------------------------------------------------------------------------

var
  GetBufferedPaintTargetRect: function(hBufferedPaint: HPAINTBUFFER; var prc: TRect): HRESULT; stdcall;
{$EXTERNALSYM GetBufferedPaintTargetRect}

//------------------------------------------------------------------------
//  GetBufferedPaintTargetDC() - Returns the target DC specified during BeginBufferedPaint
//
//    hBufferedPaint           - handle to buffered paint context
//------------------------------------------------------------------------

var
  GetBufferedPaintTargetDC: function(hBufferedPaint: HPAINTBUFFER): HDC; stdcall;
{$EXTERNALSYM GetBufferedPaintTargetDC}

//------------------------------------------------------------------------
//  GetBufferedPaintDC() - Returns the same paint DC returned by BeginBufferedPaint
//
//    hBufferedPaint     - handle to buffered paint context
//------------------------------------------------------------------------

var
  GetBufferedPaintDC: function(hBufferedPaint: HPAINTBUFFER): HDC; stdcall;
{$EXTERNALSYM GetBufferedPaintDC}

//------------------------------------------------------------------------
//  GetBufferedPaintBits() - Obtains a pointer to the buffer bitmap, if the buffer is a DIB
//
//    hBufferedPaint       - handle to buffered paint context
//    ppbBuffer            - pointer to receive pointer to buffer bitmap pixels
//    pcxRow               - pointer to receive width of buffer bitmap, in pixels;
//                           this value may not necessarily be equal to the buffer width
//------------------------------------------------------------------------

var
  GetBufferedPaintBits: function(hBufferedPaint: HPAINTBUFFER; var ppbBuffer: PRGBQUAD;
    var pcxRow: Integer): HRESULT; stdcall;
{$EXTERNALSYM GetBufferedPaintBits}

//------------------------------------------------------------------------
//  BufferedPaintClear() - Clears given rectangle to ARGB = {0, 0, 0, 0}
//
//    hBufferedPaint     - handle to buffered paint context
//    prc                - rectangle to clear; NULL specifies entire buffer
//------------------------------------------------------------------------

var
  BufferedPaintClear: function(hBufferedPaint: HPAINTBUFFER; prc: PRect): HRESULT; stdcall;
{$EXTERNALSYM BufferedPaintClear}

//------------------------------------------------------------------------
//  BufferedPaintSetAlpha() - Set alpha to given value in given rectangle
//
//    hBufferedPaint        - handle to buffered paint context
//    prc                   - rectangle to set alpha in; NULL specifies entire buffer
//    alpha                 - alpha value to set in the given rectangle
//------------------------------------------------------------------------

var
  BufferedPaintSetAlpha: function(hBufferedPaint: HPAINTBUFFER; prc: PRect; alpha: Byte): HRESULT; stdcall;
{$EXTERNALSYM BufferedPaintSetAlpha}

// Macro for setting the buffer to opaque (alpha = 255)
function BufferedPaintMakeOpaque(hBufferedPaint: HPAINTBUFFER; prc: PRect): HRESULT;
{$EXTERNALSYM BufferedPaintMakeOpaque}

//------------------------------------------------------------------------
//  BufferedPaintStopAllAnimations() - Stop all buffer animations for the given window
//
//    hwnd                           - window on which to stop all animations
//------------------------------------------------------------------------

var
  BufferedPaintStopAllAnimations: function(hwnd: HWND): HRESULT; stdcall;
{$EXTERNALSYM BufferedPaintStopAllAnimations}

type
  HANIMATIONBUFFER = HANDLE;  // handle to a buffered paint animation
{$EXTERNALSYM HANIMATIONBUFFER}

var
  BeginBufferedAnimation: function(hwnd: HWND; hdcTarget: HDC; var prcTarget: TRect;
    dwFormat: LongWord; pPaintParams: PBP_PAINTPARAMS; pAnimationParams: PBP_ANIMATIONPARAMS;
    var phdcFrom: HDC; var phdcTo: HDC): HANIMATIONBUFFER; stdcall;
{$EXTERNALSYM BeginBufferedAnimation}

var
  EndBufferedAnimation: function(hbpAnimation: HANIMATIONBUFFER; fUpdateTarget: BOOL): HRESULT; stdcall;
{$EXTERNALSYM EndBufferedAnimation}

var
  BufferedPaintRenderAnimation: function(hwnd: HWND; hdcTarget: HDC): BOOL; stdcall;
{$EXTERNALSYM BufferedPaintRenderAnimation}

//----------------------------------------------------------------------------
// Tells if the DWM is running, and composition effects are possible for this
// process (themes are active).
// Roughly equivalent to "DwmIsCompositionEnabled() && IsAppthemed()"
//----------------------------------------------------------------------------
var
  IsCompositionActive: function: BOOL; stdcall;
{$EXTERNALSYM IsCompositionActive}

//------------------------------------------------------------------------
//  GetThemeTransitionDuration()
//                      - Gets the duration for the specified transition
//
//  hTheme              - theme data handle
//  iPartId             - part number
//  iStateIdFrom        - starting state number of part
//  iStateIdTo          - ending state number of part
//  iPropId             - property id
//  pdwDuration         - receives the transition duration
//------------------------------------------------------------------------

var
  GetThemeTransitionDuration: function(hTheme: HTHEME; iPartId, iStateIdFrom, iStateIdTo, iPropId: Integer;
    var pdwDuration: DWORD): HRESULT;
{$EXTERNALSYM GetThemeTransitionDuration}

// endif // if (_WIN32_WINNT >= 0x0600)

implementation

uses
  SyncObjs;

//----------------------------------------------------------------------------------------------------------------------

const
  themelib = 'uxtheme.dll';

var
  ThemeLibrary: THandle;
  ReferenceCount: Integer;  // We have to keep track of several load/unload calls.
  Lock: TCriticalSection;

procedure FreeThemeLibrary;
begin
  Lock.Enter;
  try
    if ReferenceCount > 0 then
      Dec(ReferenceCount);

    if (ThemeLibrary <> 0) and (ReferenceCount = 0) then
    begin
      FreeLibrary(ThemeLibrary);
      ThemeLibrary := 0;

      OpenThemeData := nil;
      CloseThemeData := nil;
      DrawThemeBackground := nil;
      DrawThemeText := nil;
      GetThemeBackgroundContentRect := nil;
      GetThemeBackgroundExtent := nil;
      GetThemePartSize := nil;
      GetThemeTextExtent := nil;
      GetThemeTextMetrics := nil;
      GetThemeBackgroundRegion := nil;
      HitTestThemeBackground := nil;
      DrawThemeEdge := nil;
      DrawThemeIcon := nil;
      IsThemePartDefined := nil;
      IsThemeBackgroundPartiallyTransparent := nil;
      GetThemeColor := nil;
      GetThemeMetric := nil;
      GetThemeString := nil;
      GetThemeBool := nil;
      GetThemeInt := nil;
      GetThemeEnumValue := nil;
      GetThemePosition := nil;
      GetThemeFont := nil;
      GetThemeRect := nil;
      GetThemeMargins := nil;
      GetThemeIntList := nil;
      GetThemePropertyOrigin := nil;
      SetWindowTheme := nil;
      GetThemeFilename := nil;
      GetThemeSysColor := nil;
      GetThemeSysColorBrush := nil;
      GetThemeSysBool := nil;
      GetThemeSysSize := nil;
      GetThemeSysFont := nil;
      GetThemeSysString := nil;
      GetThemeSysInt := nil;
      IsThemeActive := nil;
      IsAppThemed := nil;
      GetWindowTheme := nil;
      EnableThemeDialogTexture := nil;
      IsThemeDialogTextureEnabled := nil;
      GetThemeAppProperties := nil;
      SetThemeAppProperties := nil;
      GetCurrentThemeName := nil;
      GetThemeDocumentationProperty := nil;
      DrawThemeParentBackground := nil;
      EnableTheming := nil;
      OpenThemeDataEx := nil;
      DrawThemeBackgroundEx := nil;
      DrawThemeParentBackgroundEx := nil;
      SetWindowThemeAttribute := nil;
      DrawThemeTextEx := nil;
      GetThemeBitmap := nil;
      GetThemeStream := nil;
      BufferedPaintInit := nil;
      BufferedPaintUnInit := nil;
      BeginBufferedPaint := nil;
      EndBufferedPaint := nil;
      GetBufferedPaintTargetRect := nil;
      GetBufferedPaintTargetDC := nil;
      GetBufferedPaintDC := nil;
      GetBufferedPaintBits := nil;
      BufferedPaintClear := nil;
      BufferedPaintSetAlpha := nil;
      BufferedPaintStopAllAnimations := nil;
      BeginBufferedAnimation := nil;
      EndBufferedAnimation := nil;
      BufferedPaintRenderAnimation := nil;
      IsCompositionActive := nil;
      GetThemeTransitionDuration := nil;
      BeginPanningFeedback := nil;
      UpdatePanningFeedback := nil;
      EndPanningFeedback := nil;
    end;
  finally
    Lock.Leave;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function InitThemeLibrary: Boolean;

begin
  Lock.Enter;
  try
    Inc(ReferenceCount);

    if ThemeLibrary = 0 then
    begin
      ThemeLibrary := LoadLibrary(themelib);
      if ThemeLibrary > 0 then
      begin
        // windows XP
        Pointer(OpenThemeData) := GetProcAddress(ThemeLibrary, 'OpenThemeData');
        Pointer(CloseThemeData) := GetProcAddress(ThemeLibrary, 'CloseThemeData');
        Pointer(DrawThemeBackground) := GetProcAddress(ThemeLibrary, 'DrawThemeBackground');
        Pointer(DrawThemeText) := GetProcAddress(ThemeLibrary, 'DrawThemeText');
        Pointer(GetThemeBackgroundContentRect) := GetProcAddress(ThemeLibrary, 'GetThemeBackgroundContentRect');
        Pointer(GetThemeBackgroundExtent) := GetProcAddress(ThemeLibrary, 'GetThemeBackgroundExtent');
        Pointer(GetThemePartSize) := GetProcAddress(ThemeLibrary, 'GetThemePartSize');
        Pointer(GetThemeTextExtent) := GetProcAddress(ThemeLibrary, 'GetThemeTextExtent');
        Pointer(GetThemeTextMetrics) := GetProcAddress(ThemeLibrary, 'GetThemeTextMetrics');
        Pointer(GetThemeBackgroundRegion) := GetProcAddress(ThemeLibrary, 'GetThemeBackgroundRegion');
        Pointer(HitTestThemeBackground) := GetProcAddress(ThemeLibrary, 'HitTestThemeBackground');
        Pointer(DrawThemeEdge) := GetProcAddress(ThemeLibrary, 'DrawThemeEdge');
        Pointer(DrawThemeIcon) := GetProcAddress(ThemeLibrary, 'DrawThemeIcon');
        Pointer(IsThemePartDefined) := GetProcAddress(ThemeLibrary, 'IsThemePartDefined');
        Pointer(IsThemeBackgroundPartiallyTransparent) := GetProcAddress(ThemeLibrary, 'IsThemeBackgroundPartiallyTransparent');
        Pointer(GetThemeColor) := GetProcAddress(ThemeLibrary, 'GetThemeColor');
        Pointer(GetThemeMetric) := GetProcAddress(ThemeLibrary, 'GetThemeMetric');
        Pointer(GetThemeString) := GetProcAddress(ThemeLibrary, 'GetThemeString');
        Pointer(GetThemeBool) := GetProcAddress(ThemeLibrary, 'GetThemeBool');
        Pointer(GetThemeInt) := GetProcAddress(ThemeLibrary, 'GetThemeInt');
        Pointer(GetThemeEnumValue) := GetProcAddress(ThemeLibrary, 'GetThemeEnumValue');
        Pointer(GetThemePosition) := GetProcAddress(ThemeLibrary, 'GetThemePosition');
        Pointer(GetThemeFont) := GetProcAddress(ThemeLibrary, 'GetThemeFont');
        Pointer(GetThemeRect) := GetProcAddress(ThemeLibrary, 'GetThemeRect');
        Pointer(GetThemeMargins) := GetProcAddress(ThemeLibrary, 'GetThemeMargins');
        Pointer(GetThemeIntList) := GetProcAddress(ThemeLibrary, 'GetThemeIntList');
        Pointer(GetThemePropertyOrigin) := GetProcAddress(ThemeLibrary, 'GetThemePropertyOrigin');
        Pointer(SetWindowTheme) := GetProcAddress(ThemeLibrary, 'SetWindowTheme');
        Pointer(GetThemeFilename) := GetProcAddress(ThemeLibrary, 'GetThemeFilename');
        Pointer(GetThemeSysColor) := GetProcAddress(ThemeLibrary, 'GetThemeSysColor');
        Pointer(GetThemeSysColorBrush) := GetProcAddress(ThemeLibrary, 'GetThemeSysColorBrush');
        Pointer(GetThemeSysBool) := GetProcAddress(ThemeLibrary, 'GetThemeSysBool');
        Pointer(GetThemeSysSize) := GetProcAddress(ThemeLibrary, 'GetThemeSysSize');
        Pointer(GetThemeSysFont) := GetProcAddress(ThemeLibrary, 'GetThemeSysFont');
        Pointer(GetThemeSysString) := GetProcAddress(ThemeLibrary, 'GetThemeSysString');
        Pointer(GetThemeSysInt) := GetProcAddress(ThemeLibrary, 'GetThemeSysInt');
        Pointer(IsThemeActive) := GetProcAddress(ThemeLibrary, 'IsThemeActive');
        Pointer(IsAppThemed) := GetProcAddress(ThemeLibrary, 'IsAppThemed');
        Pointer(GetWindowTheme) := GetProcAddress(ThemeLibrary, 'GetWindowTheme');
        Pointer(EnableThemeDialogTexture) := GetProcAddress(ThemeLibrary, 'EnableThemeDialogTexture');
        Pointer(IsThemeDialogTextureEnabled) := GetProcAddress(ThemeLibrary, 'IsThemeDialogTextureEnabled');
        Pointer(GetThemeAppProperties) := GetProcAddress(ThemeLibrary, 'GetThemeAppProperties');
        Pointer(SetThemeAppProperties) := GetProcAddress(ThemeLibrary, 'SetThemeAppProperties');
        Pointer(GetCurrentThemeName) := GetProcAddress(ThemeLibrary, 'GetCurrentThemeName');
        Pointer(GetThemeDocumentationProperty) := GetProcAddress(ThemeLibrary, 'GetThemeDocumentationProperty');
        Pointer(DrawThemeParentBackground) := GetProcAddress(ThemeLibrary, 'DrawThemeParentBackground');
        Pointer(EnableTheming) := GetProcAddress(ThemeLibrary, 'EnableTheming');
        // windows Vista
        Pointer(OpenThemeDataEx) := GetProcAddress(ThemeLibrary, 'OpenThemeDataEx');
        Pointer(DrawThemeBackgroundEx) := GetProcAddress(ThemeLibrary, 'DrawThemeBackgroundEx');
        Pointer(DrawThemeParentBackgroundEx) := GetProcAddress(ThemeLibrary, 'DrawThemeParentBackgroundEx');
        Pointer(SetWindowThemeAttribute) := GetProcAddress(ThemeLibrary, 'SetWindowThemeAttribute');
        Pointer(DrawThemeTextEx) := GetProcAddress(ThemeLibrary, 'DrawThemeTextEx');
        Pointer(GetThemeBitmap) := GetProcAddress(ThemeLibrary, 'GetThemeBitmap');
        Pointer(GetThemeStream) := GetProcAddress(ThemeLibrary, 'GetThemeStream');
        Pointer(BufferedPaintInit) := GetProcAddress(ThemeLibrary, 'BufferedPaintInit');
        Pointer(BufferedPaintUnInit) := GetProcAddress(ThemeLibrary, 'BufferedPaintUnInit');
        Pointer(BeginBufferedPaint) := GetProcAddress(ThemeLibrary, 'BeginBufferedPaint');
        Pointer(EndBufferedPaint) := GetProcAddress(ThemeLibrary, 'EndBufferedPaint');
        Pointer(GetBufferedPaintTargetRect) := GetProcAddress(ThemeLibrary, 'GetBufferedPaintTargetRect');
        Pointer(GetBufferedPaintTargetDC) := GetProcAddress(ThemeLibrary, 'GetBufferedPaintTargetDC');
        Pointer(GetBufferedPaintDC) := GetProcAddress(ThemeLibrary, 'GetBufferedPaintDC');
        Pointer(GetBufferedPaintBits) := GetProcAddress(ThemeLibrary, 'GetBufferedPaintBits');
        Pointer(BufferedPaintClear) := GetProcAddress(ThemeLibrary, 'BufferedPaintClear');
        Pointer(BufferedPaintSetAlpha) := GetProcAddress(ThemeLibrary, 'BufferedPaintSetAlpha');
        Pointer(BufferedPaintStopAllAnimations) := GetProcAddress(ThemeLibrary, 'BufferedPaintStopAllAnimations');
        Pointer(BeginBufferedAnimation) := GetProcAddress(ThemeLibrary, 'BeginBufferedAnimation');
        Pointer(EndBufferedAnimation) := GetProcAddress(ThemeLibrary, 'EndBufferedAnimation');
        Pointer(BufferedPaintRenderAnimation) := GetProcAddress(ThemeLibrary, 'BufferedPaintRenderAnimation');
        Pointer(IsCompositionActive) := GetProcAddress(ThemeLibrary, 'IsCompositionActive');
        Pointer(GetThemeTransitionDuration) := GetProcAddress(ThemeLibrary, 'GetThemeTransitionDuration');
        // windows 7
        Pointer(BeginPanningFeedback) := GetProcAddress(ThemeLibrary, 'BeginPanningFeedback');
        Pointer(UpdatePanningFeedback) := GetProcAddress(ThemeLibrary, 'UpdatePanningFeedback');
        Pointer(EndPanningFeedback) := GetProcAddress(ThemeLibrary, 'EndPanningFeedback');
      end;
    end;
    Result := ThemeLibrary > 0;
  finally
    Lock.Leave;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function UseThemes: Boolean;
begin
  Result := ThemeLibrary > 0;
  if Result then
    Result := IsAppThemed() and IsThemeActive();
end;

//----------------------------------------------------------------------------------------------------------------------

function SetWindowThemeNonClientAttributes(hwnd: HWND; dwMask: DWORD; dwAttributes: DWORD): HRESULT;
var
  wta: WTA_OPTIONS;
begin
  wta.dwFlags := dwAttributes;
  wta.dwMask := dwMask;
  Result := SetWindowThemeAttribute(hwnd, WTA_NONCLIENT, @wta, sizeof(wta));
end;

//----------------------------------------------------------------------------------------------------------------------
function BufferedPaintMakeOpaque(hBufferedPaint: HPAINTBUFFER; prc: PRect): HRESULT;
begin
  Result := BufferedPaintSetAlpha(hBufferedPaint, prc, 255);
end;
//----------------------------------------------------------------------------------------------------------------------

initialization
  ReferenceCount := 0;
  Lock := TCriticalSection.Create;
finalization
  while ReferenceCount > 0 do
    FreeThemeLibrary;
  Lock.Free;
end.

