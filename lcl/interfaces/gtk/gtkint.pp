{ 
 /*************************************************************************** 
                         GTKINT.pp  -  GTKInterface Object
                             ------------------- 
 
                   Initial Revision  : Thu July 1st CST 1999 
 
 
 ***************************************************************************/ 
 
/*************************************************************************** 
 *                                                                         * 
 *   This program is free software; you can redistribute it and/or modify  * 
 *   it under the terms of the GNU General Public License as published by  * 
 *   the Free Software Foundation; either version 2 of the License, or     * 
 *   (at your option) any later version.                                   * 
 *                                                                         * 
 ***************************************************************************/ 
 } 
 
unit gtkint;
 
{$mode objfpc} 

interface

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}
 
uses 
  InterfaceBase, gtk, gdk, glib, sysutils, lmessages, Classes, Controls,
  extctrls, forms,dialogs, VclGlobals,stdctrls, comctrls, LCLLinux;
 


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
    case GDIType: TGDIType of
      gdiBitmap: (
        GDIBitmapMaskObject: PGdkPixmap;
        case GDIBitmapType: TGDIBitmapType of
          gbBitmap: (GDIBitmapObject: PGdkBitmap); 
          gbPixmap: (GDIPixmapObject: PGdkPixmap);
          gbImage : (GDIRawImageObject: PGDIRawImage);
      );
      gdiBrush: ( 
        GDIBrushColor: TGdkColor;
        GDIBrushFill: TGdkFill;
        GDIBrushPixMap: PGdkPixmap;
      ); 
      gdiFont: (
        GDIFontObject: PGdkFont;
        LogFont: TLogFont;  // for now font info is stored as well, later query font params
      ); 
      gdiPen: (
        GDIPenColor: TGdkColor;
        GDIPenWidth: Integer;
        GDIPenStyle: Word;
      ); 
      gdiRegion: (
      ); 
  end;


  // move to class ??
  PDeviceContext = ^TDeviceContext;
  TDeviceContext = record
    hWnd: HWND; 
    GC: pgdkGC;
    Drawable: PGDKDrawable;
    PenPos: TPoint;
    CurrentBitmap: PGdiObject;
    CurrentFont: PGdiObject;
    CurrentPen: PGdiObject;
    CurrentBrush: PGdiObject;
    CurrentTextColor: TGdkColor;
    CurrentBackColor: TGdkColor;
    SavedContext: PDeviceContext; // linked list of saved DCs
  end;
  
type

   TgtkObject = class(TInterfaceBase)
   private
      FCaptureHandle: HWND;
      FKeyStateList: TList; // Keeps track of which keys are pressed
      FDeviceContexts: TList;
      FGDIObjects: TList;
      FMessageQueue: TList;
      FGTKToolTips: PGtkToolTips;
      FAccelGroup: PgtkAccelGroup;

      procedure CreateComponent(Sender : TObject);
      procedure AddChild(Parent,Child : Pointer; Left,Top: Integer);
      procedure ResizeChild(Sender : TObject; Left,Top,Width,Height : Integer);
      function  GetLabel(CompStyle: Integer; P : Pointer) : String;
      procedure AssignSelf(Child ,Data : Pointer);
      procedure ReDraw(Child : Pointer);
      Procedure SetCursor(Sender : TObject);
      
      function IsValidDC(const DC: HDC): Boolean;
      function IsValidGDIObject(const GDIObject: HGDIOBJ): Boolean;
      function IsValidGDIObjectType(const GDIObject: HGDIOBJ; const GDIType: TGDIType): Boolean;
      function NewGDIObject(const GDIType: TGDIType): PGdiObject;
      function NewDC: PDeviceContext;
      function CreateDefaultBrush: PGdiObject;
      function CreateDefaultFont: PGdiObject;
      function CreateDefaultPen: PGdiObject;

      procedure ShowHide(Sender : TObject);
      procedure AddNBPage(Parent,Child: TObject; Index: Integer);
      procedure RemoveNBPage(Parent: TObject; Index: Integer);
      procedure SetText(Child,Data : Pointer);
      procedure GetFontinfo(Sender : TObject; Data : Pointer);
      procedure FontSetName(Sender : TObject);
      procedure SetColor(Sender : TObject);
      Procedure SetPixel(Sender : TObject; Data : Pointer);
      Procedure GetPixel(Sender : TObject; Data : Pointer);
      function GetValue (Sender : TObject; Data : pointer) : integer;
      function SetValue (Sender : TObject; Data : pointer) : integer;
      function SetProperties (Sender: TObject) : integer;
      procedure AttachMenu(Sender: TObject);
   protected
      Cursor_Watch : pGDKCursor;
      Cursor_Arrow : pGDKCursor;
      Cursor_Cross : pGDKCursor;
      Cursor_Hand1 : pGDKCursor;
      Cursor_XTerm : pGDKCursor;
   public
      constructor Create; 
      destructor Destroy; override;
      function GetText(Sender: TControl; var Text: String): Boolean; override;
      procedure SetLabel(Sender : TObject; Data : Pointer);
      function  IntSendMessage3(LM_Message : Integer; Sender : TObject; data : pointer) : integer; override;
      procedure SetCallback(Msg : LongInt; Sender : TObject); override;
      procedure RemoveCallbacks(Sender : TObject); override;
      procedure DoEvents; override;
      procedure HandleEvents; override;
      procedure AppTerminate; override;
      procedure Init; override;
      function UpdateHint(Sender: TObject): Integer; override;
      
      {$I gtkwinapih.inc}

   end;


type

   TEventProc = record
      Name : String[25];
      CallBack : Procedure(Data : TObject);
      Data : Pointer;
   End;

   CallbackProcedure = Procedure (Data : Pointer);

   pTRect = ^TRect;


procedure EventTrace(message : string; data : pointer);

const
   TargetEntrys = 3;
var
  target_table : Array[0..TargetEntrys-1] of TgtkTargetEntry;

  //drag icons
  TrashCan_Open : PgdkPixmap;
  TrashCan_Open_Mask : PGdkPixmap;
  Trashcan_closed : PGdkPixmap;
  Trashcan_closed_mask : PGdkPixmap;
  Drag_Icon : PgdkPixmap;
  Drag_Mask : PgdkPixmap;

  Dragging : Boolean;

{$I gtklistslh.inc}



Implementation

uses Graphics, buttons, Menus, GTKWinApiWindow, CListBox;

{$I gtklistsl.inc}


const

  KEYMAP_VKUNKNOWN = $10000;
  KEYMAP_TOGGLE    = $20000;
  KEYMAP_EXTENDED  = $40000;

// PDB: note this is a hack. Windows maintains a system wide
//      system color table we will have to have our own
//      to be able to do the translations required from
//      window manager to window manager this means every
//      application will carry its own color table
//      we set the defaults here to reduce the initial
//      processing of creating a default table
// MWE: Naaaaah, not a hack, just something temporary
const
  SysColorMap: array [0..MAX_SYS_COLORS] of DWORD = (
    $C0C0C0,     {COLOR_SCROLLBAR}
    $808000,     {COLOR_BACKGROUND}
    $800000,     {COLOR_ACTIVECAPTION}
    $808080,     {COLOR_INACTIVECAPTION}
    $C0C0C0,     {COLOR_MENU}
    $FFFFFF,     {COLOR_WINDOW}
    $000000,     {COLOR_WINDOWFRAME}
    $000000,     {COLOR_MENUTEXT}
    $000000,     {COLOR_WINDOWTEXT}
    $FFFFFF,     {COLOR_CAPTIONTEXT}
    $C0C0C0,     {COLOR_ACTIVEBORDER}
    $C0C0C0,     {COLOR_INACTIVEBORDER}
    $808080,     {COLOR_APPWORKSPACE}
    $800000,     {COLOR_HIGHLIGHT}
    $FFFFFF,     {COLOR_HIGHLIGHTTEXT}
    $D0D0D0,     {COLOR_BTNFACE}
    $808080,     {COLOR_BTNSHADOW}
    $808080,     {COLOR_GRAYTEXT}
    $000000,     {COLOR_BTNTEXT}
    $C0C0C0,     {COLOR_INACTIVECAPTIONTEXT}
    $F0F0F0,     {COLOR_BTNHIGHLIGHT}
    $000000,     {COLOR_3DDKSHADOW}
    $C0C0C0,     {COLOR_3DLIGHT}
    $000000,     {COLOR_INFOTEXT}
    $E1FFFF,     {COLOR_INFOBK}
    $000000,     {unasigned}
    $000000,     {COLOR_HOTLIGHT}
    $000000,     {COLOR_GRADIENTACTIVECAPTION}
    $000000      {COLOR_GRADIENTINACTIVECAPTION}
  ); {end _SysColors}

type

  //Defined in gtksignal.c
  PGtkHandler = ^TGtkHandler;
  TGtkHandler = record
    id:               guint;           
    next:             PGtkHandler;      
    prev:             PGtkHandler;     
    flags:            guint; // -->  blocked : 20, object_signal : 1, after : 1, no_marshal : 1          
    ref_count:        guint16;         
    signal_id:        guint16;         
    func:             TGtkSignalFunc;   
    func_data:        gpointer;        
    destroy_func:     TGtkSignalDestroy;
  end;

var
  Event : TGDKEVENTCONFIGURE;
  gtk_handler_quark: TGQuark;



const
  TARGET_STRING = 1;
  TARGET_ROOTWIN = 2;


{$I dragicons.inc}
{$I gtkproc.inc}
{$I gtkcallback.inc}
{$I gtkobject.inc}
{$I gtkwinapi.inc}



var
  n: Integer;


initialization
  

  gtk_handler_quark := g_quark_from_static_string('gtk-signal-handlers');

  Target_Table[0].Target := 'STRING';
  Target_Table[0].Flags := 0;
  Target_Table[0].Info := TARGET_STRING;
  Target_Table[1].Target := 'text/plain';
  Target_Table[1].Flags := 0;
  Target_Table[1].Info := TARGET_STRING;
  Target_Table[2].Target := 'application/x-rootwin-drop';
  Target_Table[2].Flags := 0;
  Target_Table[2].Info := TARGET_ROOTWIN;



end.

{ =============================================================================

  $Log$
  Revision 1.1  2000/07/13 10:28:29  michael
  + Initial import

  Revision 1.10  2000/06/14 21:51:26  lazarus
  MWE:
    + Added menu accelerators. Not finished

  Revision 1.9  2000/06/09 11:35:22  lazarus
  More shortcut work.
  Shane

  Revision 1.8  2000/06/04 10:00:33  lazarus
  MWE:
    * Fixed bug #6.

  Revision 1.7  2000/05/27 22:20:56  lazarus
  MWE & VRS:
    + Added new hint code

  Revision 1.6  2000/05/11 22:04:15  lazarus
  MWE:
    + Added messagequeue
    * Recoded SendMessage and Peekmessage
    + Added postmessage
    + added DeliverPostMessage

  Revision 1.5  2000/05/10 22:52:58  lazarus
  MWE:
    = Moved some global api stuf to gtkobject

  Revision 1.4  2000/05/09 02:05:08  lazarus
  Replaced writelns with Asserts.                        CAW

  Revision 1.3  2000/04/13 21:25:16  lazarus
  MWE:
    ~ Added some docu and did some cleanup.
  Hans-Joachim Ott <hjott@compuserve.com>:
    * TMemo.Lines works now.
    + TMemo has now a property Scrollbar.
    = TControl.GetTextBuf revised :-)
    + Implementation for CListBox columns added
    * Bug in TGtkCListStringList.Assign corrected.

  Revision 1.2  2000/04/07 16:59:55  lazarus
  Implemented GETCAPTURE and SETCAPTURE along with RELEASECAPTURE.
  Shane

  Revision 1.1  2000/03/30 22:51:42  lazarus
  MWE:
    Moved from ../../lcl

  Revision 1.56  2000/03/30 21:57:44  lazarus
  MWE:
    + Added some general functions to Get/Set the Main/Fixed/CoreChild
      widget
    + Started with graphic scalig/depth stuff. This is way from finished

  Hans-Joachim Ott <hjott@compuserve.com>:
    + Added some improvements for TMEMO

  Revision 1.55  2000/03/23 22:48:56  lazarus
  MWE & Hans-Joachim Ott <hjott@compuserve.com>:
    + added replacement for LM_GetText

  Revision 1.54  2000/03/23 20:40:03  lazarus
  Added some drag code
  Shane

  Revision 1.53  2000/03/22 17:09:29  lazarus
  *** empty log message ***

  Revision 1.52  2000/03/19 23:01:42  lazarus
  MWE:
    = Changed splashscreen loading/colordepth
    = Chenged Save/RestoreDC to platform  dependent, since they are
      relative to a DC

  Revision 1.51  2000/03/10 18:31:09  lazarus
  Added TSpeedbutton code
  Shane

  Revision 1.50  2000/03/08 23:57:38  lazarus
  MWE:
    Added SetSysColors
    Fixed TEdit text bug (thanks to hans-joachim ott <hjott@compuserve.com>)
    Finished GetKeyState
    Added changes from Peter Dyson <peter@skel.demon.co.uk>
    - a new GetSysColor
    - some improvements on ExTextOut

  Revision 1.49  2000/03/03 22:58:26  lazarus
  MWE:
    Fixed focussing problem.
      LM-FOCUS was bound to the wrong signal
    Added GetKeyState api func.
      Now LCL knows if shift/trl/alt is pressed (might be handy for keyboard
      selections ;-)

  Revision 1.48  2000/02/24 21:15:30  lazarus
  Added TCustomForm.GetClientRect and RequestAlign to try and get the controls to align correctly when a MENU is present.  Not Complete yet.

  Fixed the bug in TEdit that caused it not to update it's text property.  I will have to
  look at TMemo to see if anything there was affected.

  Added SetRect to WinAPI calls
  Added AdjustWindowRectEx to WINAPI calls.
  Shane

  Revision 1.47  2000/02/21 17:38:04  lazarus
  Added modalresult to TCustomForm
  Added a View Units dialog box
  Added a View Forms dialog box
  Added a New Unit menu selection
  Added a New Form menu selection
  Shane

  Revision 1.46  2000/01/26 19:16:24  lazarus
  Implemented TPen.Style properly for GTK. Done SelectObject for pen objects.
  Misc bug fixes.
  Corrected GDK declaration for gdk_gc_set_slashes.

  Revision 1.45  2000/01/25 22:04:27  lazarus
  MWE:
    The first primitive Caret functions are getting visible

  Revision 1.44  2000/01/25 00:38:24  lazarus
  MWE:
    Added GetFocus

  Revision 1.43  2000/01/14 15:01:15  lazarus
  Changed SETCURSOR so the cursor's were created in the gtkObject.Init and destroyed in GTkObject.AppTerminate
  Shane

  Revision 1.42  2000/01/11 20:50:32  lazarus
  Added some code for SETCURSOR.  Doesn't work perfect yet but getting there.
  Shane

  Revision 1.41  2000/01/10 00:07:12  lazarus
  MWE:
    Added more scrollbar support for TWinControl
    Most signals for TWinContorl are jet connected to the wrong widget
      (now scrolling window, should be fixed)
    Added some cvs entries

  Revision 1.40  2000/01/04 19:16:09  lazarus
  Stoppok:
     - new messages LM_GETVALUE, LM_SETVALUE, LM_SETPROPERTIES
     - changed trackbar, progressbar, checkbox to use above messages
     - some more published properties for above components
       (all properties derived from TWinControl)
     - new functions SetValue, GetValue, SetProperties in gtk-interface

  Revision 1.39  2000/01/03 00:19:21  lazarus
  MWE:
    Added keyup and buttonup events
    Added LM_MOUSEMOVE callback
    Started with scrollbars in editor

  Revision 1.38  1999/12/27 22:32:15  lazarus
  MWE:
    Fixed triple chars in editor. Events where fired more than once. Now
      it is checked if there already exists a callback.

  Revision 1.37  1999/12/23 19:50:53  lazarus
  Working on the toolbar again.  Haven't been able to get it to display at all yet.

  gtkobject.inc - removed Intsendmessage and Intsendmessage2
  WinControl.inc - addded code to InsertControl so when a control is added to a parent's control list, a CMCONTROLCHANGED message is sent.  This way the parent can react to the addition.

  Shane

  Revision 1.36  1999/12/08 00:56:07  lazarus
  MWE:
    Fixed menus. Events aren't enabled yet (dumps --> invalid typecast ??)

  Revision 1.35  1999/12/03 00:26:47  lazarus
  MWE:
    fixed control location
    added gdiobject reference counter

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

  Revision 1.32  1999/11/25 23:55:22  lazarus
  MWE:
    Removed test unit

  Revision 1.31  1999/11/25 23:45:08  lazarus
  MWE:
    Added font as GDIobject
    Added some API testcode to testform
    Commented out some more IFDEFs in mwCustomEdit

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

  Revision 1.28  1999/11/16 01:32:22  lazarus
  MWE:
    Added some more DC functionality

  Revision 1.27  1999/11/15 00:40:22  lazarus
  MWE:
    Added GetDC, ReleaseDC, Rectangle functions

  Revision 1.26  1999/11/13 13:03:34  lazarus
  MWE:
    Started to implement some platform dependent WINAPI stuff
    Added a baseclass for InterfaceObject
    Started messing around with canvasses

  Revision 1.25  1999/11/05 00:34:10  lazarus
  MWE: Menu structure updated, events and visible code not added yet

  Revision 1.24  1999/10/28 17:17:42  lazarus
  Removed references to FCOmponent.
  Shane

  Revision 1.23  1999/10/19 19:16:51  lazarus
  renamed stdcontrols.pp stdctrls.pp
  Shane

  Revision 1.22  1999/09/26 13:30:15  lazarus

     Implemented OnEnter & OnExit events for TTrackbar. These properties
     and handler functions have been added to TWincontrol, two new
     callbacks have been added to gtkcallback.
      stoppok

  Revision 1.21  1999/09/13 03:24:00  lazarus
  Changed definitions of AddNBPage and RemoveNBPage functions.       caw

  Revision 1.20  1999/08/25 18:53:03  lazarus
  Added Canvas.pixel property which allows
  the user to get/set the pixel color.  This will be used in the editor
  to create the illusion of the cursor by XORing the pixel with black.

  Shane

  Revision 1.19  1999/08/24 20:18:00  lazarus
  *** empty log message ***

  Revision 1.18  1999/08/19 18:40:55  lazarus
  Added stuff for TProgressBar

    stoppok  Aug. 19 1999

  Revision 1.17  1999/08/16 18:45:41  lazarus
  Added a TFont Dialog plus minor additions.

  Shane Aug 16th 1999  14:07 CST

  Revision 1.16  1999/08/14 10:05:53  lazarus
  Added TListBox ItemIndex property. Made ItemIndex public for TComboBox and TListBox.

  Revision 1.15  1999/08/07 17:59:20  lazarus

        buttons.pp   the DoLeave and DoEnter were connected to the wrong
                     event.

        The rest were modified to use the new SendMessage function.   MAH

  Revision 1.14  1999/08/06 23:55:28  lazarus
  Patched some files with a patch from Michal Bukovjan for
  TComboBox and TListBox.

  Revision 1.13  1999/08/03 06:31:13  lazarus
  Moved all TNotebook GTK code to gtkint units

  Revision 1.12  1999/08/01 21:46:27  lazarus
  Modified the GETWIDTH and GETHEIGHT of TFOnt so you can use it to calculate the length in Pixels of a string.  This is now used in the editor.

  Shane

  Revision 1.11  1999/07/31 06:39:28  lazarus

       Modified the IntSendMessage3 to include a data variable. It isn't used
       yet but will help in merging the Message2 and Message3 features.

       Adjusted TColor routines to match Delphi color format

       Added a TGdkColorToTColor routine in gtkproc.inc

       Finished the TColorDialog added to comDialog example.        MAH

 }
