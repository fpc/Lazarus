{
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
unit GTKGlobals;

{$mode objfpc}{$H+}

interface

uses
  InterfaceBase, gtk, gdk, glib, SysUtils, LMessages, Classes, Controls, Forms,
  VclGlobals, LCLLinux, LCLType, GTKDef, DynHashArray, LazQueue;

{$I dragicons.inc}

var
//  target_table : array[0..TARGET_ENTRYS - 1] of TgtkTargetEntry;

  //drag icons
  //TrashCan_Open : PgdkPixmap;
  //TrashCan_Open_Mask : PGdkPixmap;
  //TrashCan_Closed : PGdkPixmap;
  //TrashCan_Closed_Mask : PGdkPixmap;
  Drag_Icon : PgdkPixmap;
  Drag_Mask : PgdkPixmap;

  //Dragging : Boolean;

  MCaptureHandle: HWND;

const
  DblClickTime = 250;// 250 miliseconds or less between clicks is a double click
  DblClickThreshold = 3;// max Movement between two clicks of a DblClick

type
  TLastMouseClick = record
    Down: boolean;
    TheTime: TDateTime; // last Down time
    ClickCount: integer;
    Component: TComponent;
    Window: PGdkWindow;
    WindowPoint: TPoint;
  end;

const
  EmptyLastMouseClick: TLastMouseClick =
    (Down: false; TheTime: -1; ClickCount: 0; Component: nil;
     Window: nil; WindowPoint: (X: 0; Y: 0));

var
  LastLeft, LastMiddle, LastRight: TLastMouseClick;
  LastFileSelectRow : gint;


  // mouse cursors
  Cursor_Watch    : pGDKCursor;
  Cursor_Arrow    : pGDKCursor;
  Cursor_Cross    : pGDKCursor;
  Cursor_Hand1    : pGDKCursor;
  Cursor_XTerm    : pGDKCursor;
  Cursor_StdArrow : pGDKCursor;
  Cursor_HSplit   : pGDKCursor;
  Cursor_VSplit   : pGDKCursor;
  Cursor_SizeNWSE : pGDKCursor;
  Cursor_SizeNS   : pGDKCursor;
  Cursor_SizeNESW : pGDKCursor;
  Cursor_SizeWE   : pGDKCursor;
  Cursor_SizeAll  : pGDKCursor;
  Cursor_Help     : pGDKCursor;

  Styles : TStrings;

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

const
  FormStyleMap : array[TFormBorderStyle] of integer = (
    GTK_WINDOW_DIALOG,  // bsNone
    GTK_WINDOW_TOPLEVEL,// bsSingle
    GTK_WINDOW_TOPLEVEL,// bsSizeable
    GTK_WINDOW_DIALOG,  // bsDialog
    GTK_WINDOW_DIALOG,  // bsToolWindow
    GTK_WINDOW_DIALOG   // bsSizeToolWin
  );
  FormResizableMap : array[TFormBorderStyle] of gint = (
    0, // bsNone
    1, // bsSingle
    1, // bsSizeable
    0, // bsDialog
    0, // bsToolWindow
    1  // bsSizeToolWin
  );
//unused:  FormBorderWidth : array[TFormBorderStyle] of gint = (0, 1, 2, 1, 1, 2);

type

  //Defined in gtksignal.c
  PGtkHandler = ^TGtkHandler;
  TGtkHandler = record
    id:               guint;
    next:             PGtkHandler;
    prev:             PGtkHandler;
    flags:            guint; // -->  blocked : 20 bits,
                             //      object_signal : 1 bit,
                             //      after : 1 bit,
                             //      no_marshal : 1 bit
    ref_count:        guint16;
    signal_id:        guint16;
    func:             TGtkSignalFunc;
    func_data:        gpointer;
    destroy_func:     TGtkSignalDestroy;
  end;

const
  bmSignalAfter = $00200000;

type
  { lazarus GtkInterface definition for additional timer data, not in gtk }
  PGtkITimerInfo = ^TGtkITimerinfo;
  TGtkITimerInfo = record
    Handle     : hWND;     // owner handle
    IDEvent    : Cardinal; // owner ID for this timer
    TimerFunc  : TFNTimerProc; // owner function to handle timer
    TimerHandle: guint;    // the gtk handle for this timer
  end;

var
  // FTimerData contains the currently running timers
  // and FOldTimerData contains the old and to be disposed timers
  FTimerData : TList;   // list of PGtkITimerinfo
  FOldTimerData: TList; // list of PGtkITimerinfo

var
  gtk_handler_quark: TGQuark;


// Internal Paint message:
const
  LM_GTKPaint         = LM_INTERFACEFIRST + 0;
  
  GtkPaint_LCLWidget = 1;
  GtkPaint_GtkWidget = 2;

type
  TLMGtkPaint = packed record
    Msg: Cardinal;
    Widget: PGtkWidget;
    State: integer; // see GtkPaint_xxx
    Unused: integer;
  end;


const
  TARGET_STRING = 1;
  TARGET_ROOTWIN = 2;

{off $DEFINE DEBUG_CLIPBOARD}
var
  // All clipboard events are handled by only one widget - the ClipboardWidget
  // This widget is typically the main form
  ClipboardWidget: PGtkWidget;
  // each selection has an gtk identifier (an atom)
  ClipboardTypeAtoms: array[TClipboardType] of cardinal;
  // each active request will procduce an TClipboardEventData stored in this list
  ClipboardSelectionData: TList; // list of PClipboardEventData
  // each selection can have an user defined handler (normally set by the lcl)
  ClipboardHandler: array[TClipboardType] of TClipboardRequestEvent;
  // boolean array, telling what gtk format is automatically supported by
  // gtk interface and not by the program (the lcl)
  ClipboardExtraGtkFormats: array[TClipboardType,TGtkClipboardFormat] of boolean;
  // lists of supported targets
  ClipboardTargetEntries: array[TClipboardType] of PGtkTargetEntry;
  ClipboardTargetEntryCnt: array[TClipboardType] of integer;

  // each main widget that was resized by the gtk is stored here
  // (hasharray of PGtkWidget)
  FWidgetsResized: TDynHashArray;
  // each fixed widget that was resized by the gtk is stored here
  // (hasharray of PGtkWidget)
  FFixWidgetsResized: TDynHashArray;

const
  aGtkJustification: array[TAlignment] of TGTKJustification =
    (GTK_JUSTIFY_LEFT,GTK_JUSTIFY_RIGHT,GTK_JUSTIFY_CENTER);

  aGtkSelectionMode: Array[Boolean] of TGtkSelectionMode =
    (GTK_SELECTION_SINGLE,GTk_SELECTION_EXTENDED);

  { file dialog }

type
  PFileSelHistoryEntry = ^TFileSelHistoryEntry;
  TFileSelHistoryEntry = record
      Filename: PChar;
      MenuItem: PGtkWidget;
    end;

  PFileSelFilterEntry = ^TFileSelFilterEntry;
  TFileSelFilterEntry = record
      Description: PChar;
      Mask: PChar;
      FilterIndex: integer;
      MenuItem: PGtkWidget;
    end;

  { Menu }

type
  TCheckMenuItemDrawProc =
    procedure (check_menu_item:PGtkCheckMenuItem; area:PGdkRectangle); cdecl;

  TMenuSizeRequestProc =
    procedure (widget:PGtkWidget; requisition:PGtkRequisition); cdecl;

const
  OldCheckMenuItemDrawProc: TCheckMenuItemDrawProc = nil;
  OldMenuSizeRequestProc: TMenuSizeRequestProc = nil;
  OldCheckMenuItemToggleSize: integer = 0;


implementation

end.

