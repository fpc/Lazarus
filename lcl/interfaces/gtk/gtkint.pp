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
{$LONGSTRINGS ON}

interface

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}
 
uses 
  InterfaceBase, gtk, gdk, glib, SysUtils, LMessages, Classes, Controls,
  ExtCtrls,Forms,Dialogs, VclGlobals, StdCtrls, ComCtrls, LCLLinux, LCLType,
  gtkDef, DynHashArray, LazQueue, Calendar, Arrow, GraphType,Spin, CommCtrl;

type
  TgtkObject = class(TInterfaceBase)
  private
    FKeyStateList: TList; // Keeps track of which keys are pressed
    FDeviceContexts: TDynHashArray;// hasharray of HDC
    FGDIObjects: TDynHashArray;    // hasharray of PGdiObject
    FMessageQueue: TLazQueue;      // queue of PMsg
    FPaintMessages: TDynHashArray; // hasharray of PLazQueueItem 
    FGTKToolTips: PGtkToolTips;
    FAccelGroup: PgtkAccelGroup;
    FTimerData : TList;       // keeps track of timer event structures
    FDefaultFont : PGdkFont;

    FStockNullBrush: HBRUSH;
    FStockBlackBrush: HBRUSH;
    FStockLtGrayBrush: HBRUSH;
    FStockGrayBrush: HBRUSH;
    FStockDkGrayBrush: HBRUSH;
    FStockWhiteBrush: HBRUSH;
    Procedure HookSignals(Sender : TObject);  //hooks all signals for controls
    procedure CreateComponent(Sender : TObject);
    procedure AddChild(Parent,Child : Pointer; Left,Top: Integer);
    procedure ResizeChild(Sender : TObject; Left,Top,Width,Height : Integer);
    function  GetLabel(CompStyle: Integer; P : Pointer) : String;
    procedure AssignSelf(Child ,Data : Pointer);
    procedure ReDraw(Child : Pointer);
    Procedure SetCursor(Sender : TObject);
    procedure SetClipboardWidget(TargetWidget: PGtkWidget);
    
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
    procedure SetColor(Sender : TObject);
    Procedure SetPixel(Sender : TObject; Data : Pointer);
    Procedure GetPixel(Sender : TObject; Data : Pointer);
    function  GetValue(Sender : TObject; Data : pointer) : integer;
    function  SetValue(Sender : TObject; Data : pointer) : integer;
    function  SetProperties (Sender: TObject) : integer;
    procedure AttachMenu(Sender: TObject);
    procedure SetColorDialogColor(ColorSelection: PGtkColorSelection;
      Color: TColor);
    
    function HashPaintMessage(p: pointer): integer;
    function FindPaintMessage(HandleWnd: HWnd): PLazQueueItem;
    procedure SetCallback(Msg : LongInt; Sender : TObject); override;
    procedure RemoveCallbacks(Sender : TObject); override;
  protected
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

  public
    constructor Create; 
    destructor Destroy; override;
    function  GetText(Sender: TControl; var Text: String): Boolean; override;
    procedure SetLabel(Sender : TObject; Data : Pointer);
    function  IntSendMessage3(LM_Message : Integer; Sender : TObject; data : pointer) : integer; override;
    procedure DoEvents; override;
    procedure HandleEvents; override;
    procedure WaitMessage; override;
    procedure AppTerminate; override;
    procedure Init; override;
    function  UpdateHint(Sender: TObject): Integer; override;
    function  RecreateWnd(Sender: TObject): Integer; override;

    {$I gtkwinapih.inc}
  end;

{$I gtklistslh.inc}

implementation

uses 
  Graphics, Buttons, Menus, GTKWinApiWindow, CListBox, KeyMap;

{$I gtklistsl.inc}


{const
  TARGET_ENTRYS = 3;
}

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

  { lazarus GtkInterface definition for additional timer data, not in gtk }
  PGtkITimerInfo = ^TGtkITimerinfo;
  TGtkITimerInfo = record
    Handle   : hWND;
    IDEvent  : Cardinal;
    TimerFunc: TFNTimerProc;
  end;
  
  
var
  //Event : TGDKEVENTCONFIGURE;
  gtk_handler_quark: TGQuark;
  FOldTimerData: TList; // list of PGtkITimerinfo


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
  //ClipboardIgnoreLossCount: array[TClipboardType] of integer;


{$I dragicons.inc}
{$I gtkproc.inc}
{$I gtkcallback.inc}
{$I gtkobject.inc}
{$I gtkwinapi.inc}

procedure InternalInit;
var c: TClipboardType;
begin
  gtk_handler_quark := g_quark_from_static_string('gtk-signal-handlers');
{
  Target_Table[0].Target := 'STRING';
  Target_Table[0].Flags := 0;
  Target_Table[0].Info := TARGET_STRING;
  Target_Table[1].Target := 'text/plain';
  Target_Table[1].Flags := 0;
  Target_Table[1].Info := TARGET_STRING;
  Target_Table[2].Target := 'application/x-rootwin-drop';
  Target_Table[2].Flags := 0;
  Target_Table[2].Info := TARGET_ROOTWIN;
}
  MCaptureHandle := 0;
  FOldTimerData:=TList.Create;
  
  // clipboard
  ClipboardSelectionData:=TList.Create;
  for c:=Low(TClipboardType) to High(TClipboardType) do begin
    ClipboardTypeAtoms[c]:=0;
    ClipboardHandler[c]:=nil;
    //ClipboardIgnoreLossCount[c]:=0;
    ClipboardTargetEntries[c]:=nil;
    ClipboardTargetEntryCnt[c]:=0;
  end;
end;

procedure InternalFinal;
var i: integer;
  t: PGtkITimerinfo;
  ced: PClipboardEventData;
  c: TClipboardType;
begin
  for i:=0 to FOldTimerData.Count-1 do begin
    t:=PGtkITimerinfo(FOldTimerData[i]);
    dispose(t);
  end;
  FOldTimerData.Free;
  FOldTimerData:=nil;
  
  // clipboard
  for i:=0 to ClipboardSelectionData.Count-1 do begin
    ced:=PClipboardEventData(ClipboardSelectionData[i]);
    if ced^.Data.Data<>nil then FreeMem(ced^.Data.Data);
    Dispose(ced);
  end;
  for c:=Low(TClipboardType) to High(TClipboardType) do 
    FreeClipboardTargetEntries(c);
  ClipboardSelectionData.Free;
  ClipboardSelectionData:=nil;
end;


initialization
//writeln('gtkint.pp - initialization');
  InternalInit;

finalization
  InternalFinal;

end.

{ =============================================================================

  $Log$
  Revision 1.33  2002/03/27 00:33:54  lazarus
  MWE:
    * Cleanup in lmessages
    * Added Listview selection and notification events
    + introduced commctrl

  Revision 1.32  2002/03/25 17:59:20  lazarus
  GTK Cleanup
  Shane

  Revision 1.31  2002/03/14 20:28:49  lazarus
  Bug fix for Mattias.
  Fixed spinedit so you can now get the value and set the value.
  Shane

  Revision 1.30  2002/02/25 18:46:06  lazarus
  MG: fixed nested record parsing

  Revision 1.29  2002/02/25 16:48:13  lazarus
  MG: new IDE window layout system

  Revision 1.28  2002/02/20 19:11:48  lazarus
  Minor tweaks, default font caching.

  Revision 1.27  2002/02/18 22:46:11  lazarus
  Implented TMenuItem.ShortCut (not much tested).

  Revision 1.26  2002/02/03 00:24:01  lazarus
  TPanel implemented.
  Basic graphic primitives split into GraphType package, so that we can
  reference it from interface (GTK, Win32) units.
  New Frame3d canvas method that uses native (themed) drawing (GTK only).
  New overloaded Canvas.TextRect method.
  LCLLinux and Graphics was split, so a bunch of files had to be modified.

  Revision 1.25  2001/12/06 13:39:36  lazarus
  Added TArrow component
  Shane

  Revision 1.24  2001/12/05 17:23:45  lazarus
  Added Calendar component
  Shane

  Revision 1.23  2001/11/13 18:50:10  lazarus
  Changes to facilitate the toggle between form and unit
  Shane

  Revision 1.22  2001/11/12 16:56:08  lazarus
  MG: CLIPBOARD

  Revision 1.21  2001/10/08 08:05:08  lazarus
  MG: fixed TColorDialog set color

  Revision 1.20  2001/10/07 07:28:33  lazarus
  MG: fixed setpixel and TCustomForm.OnResize event

  Revision 1.19  2001/09/30 08:34:51  lazarus
  MG: fixed mem leaks and fixed range check errors

  Revision 1.18  2001/07/01 23:33:13  lazarus
  MG: added WaitMessage and HandleEvents is now non blocking

  Revision 1.17  2001/06/26 21:44:32  lazarus
  MG: reduced paint messages

  Revision 1.15  2001/06/14 14:57:59  lazarus
  MG: small bugfixes and less notes

  Revision 1.14  2001/06/04 07:50:42  lazarus
  MG: close application object in gtkint.pp

  Revision 1.13  2001/04/06 22:25:14  lazarus
  * TTimer uses winapi-interface now instead of sendmessage-interface, stoppok

  Revision 1.12  2001/03/27 21:12:54  lazarus
  MWE:
    + Turned on longstrings
    + modified memotest to add lines

  Revision 1.11  2001/03/19 18:51:57  lazarus
  MG: added dynhasharray and renamed tsynautocompletion

  Revision 1.10  2001/03/19 14:44:22  lazarus
  MG: fixed many unreleased DC and GDIObj bugs

  Revision 1.7  2001/02/20 16:53:27  lazarus
  Changes for wordcompletion and many other things from Mattias.
  Shane

  Revision 1.6  2001/02/06 18:19:38  lazarus
  Shane

  Revision 1.5  2001/02/01 19:34:50  lazarus
  TScrollbar created and a lot of code added.

  It's cose to working.
  Shane

  Revision 1.4  2001/01/24 23:26:40  lazarus
  MWE:
    = moved some types to gtkdef
    + added WinWidgetInfo
    + added some initialization to Application.Create

  Revision 1.3  2001/01/23 23:33:55  lazarus
  MWE:
    - Removed old LM_InvalidateRect
    - did some cleanup in old  code
    + added some comments  on gtkobject data (gtkproc)

  Revision 1.2  2000/07/30 21:48:33  lazarus
  MWE:
    = Moved ObjectToGTKObject to GTKProc unit
    * Fixed array checking in LoadPixmap
    = Moved LM_SETENABLED to API func EnableWindow and EnableMenuItem
    ~ Some cleanup

  Revision 1.1  2000/07/13 10:28:29  michael
  + Initial import

}
