{  $Id$  }
{
 /***************************************************************************
                             GTKWinapiWindow.pp
                             -------------------
                       gtkimplementation for basic window
                   Initial Revision  : Sun Jan 9 16:00:00 GMT+1 2000


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
{
@abstract(A GTK widget to support controls derived from a window)
@author(TGTKWinapiWindow - Marc Weustink <weus@quicknet.nl>)
@created(2000)
@lastmod(2000)
}
unit GTKWinapiWindow;
{$mode objfpc}{$H+}

interface

uses
  glib,gdk,gtk;

type
  PGTKAPIWidget = ^TGTKAPIWidget;
  TGTKAPIWidget = record
    // ! the ScrolledWindow must be the first attribute of this record !
    ScrolledWindow: TGTKScrolledWindow;
    Client: PGTKWidget;
  end;
  
  PGTKAPIWidgetClass = ^TGTKAPIWidgetClass;
  TGTKAPIWidgetClass = record
    parent_class: TGTKScrolledWindowClass;
  end;

function GTKAPIWidget_GetType : guint;
function GTKAPIWidget_New : PGTKWidget;
procedure GTKAPIWidget_CreateCaret(APIWidget: PGTKAPIWidget;
                                 AWidth, AHeight: Integer; ABitmap: PGDKPixmap); 
procedure GTKAPIWidget_HideCaret(APIWidget: PGTKAPIWidget); 
procedure GTKAPIWidget_ShowCaret(APIWidget: PGTKAPIWidget); 
procedure GTKAPIWidget_SetCaretPos(APIWidget: PGTKAPIWidget; X, Y: Integer); 
procedure GTKAPIWidget_GetCaretPos(APIWidget: PGTKAPIWidget; var X, Y: Integer); 

implementation

uses
  sysutils;

//---------------------------------------------------------------------------
// gtk_winapiwindow_internal
//---------------------------------------------------------------------------
type
  TCaretInfo = record
    X: Integer;
    Y: Integer;
    Width: Integer;
    Height: Integer; 	    
    Visible: Boolean;  // Caret is on
    IsDrawn: Boolean;  // Caret is visible at the moment
    Blinking: Boolean; // Caret should blink
    Pixmap: PGDKPixMap;
    BackPixmap: PGDKPixMap;
    Timer: guint;
  end;

  PGTKAPIWidgetClient = ^TGTKAPIWidgetClient;
  TGTKAPIWidgetClient = record
    // ! the Widget must be the first attribute of the record !
    Widget: TGTKWidget;
    OtherWindow: PGDKWindow;
    Caret: TCaretInfo;
  end;
  
  PGTKAPIWidgetClientClass = ^TGTKAPIWidgetClientClass;
  TGTKAPIWidgetClientClass = record
    ParentClass: TGTKWidgetClass;
    set_scroll_adjustments: procedure(Widget: PGTKWidget;
                               HAdjustment, VAdjustment: PGTKAdjustment); cdecl;
  end;

procedure GTKAPIWidgetClient_DrawCaret(Client: PGTKAPIWidgetClient); forward;

function GTKAPIWidgetClient_Timer(Client: Pointer): gint; cdecl;
// returning 0 would stop the timer, 1 will restart it
begin
  if PGTKAPIWidgetClient(Client)^.Caret.Timer=0 then begin
    Result := 0;
    exit;
  end;
  GTKAPIWidgetClient_DrawCaret(Client);
  if PGTKAPIWidgetClient(Client)^.Caret.Timer<>0 then
    Result := 1
  else
    Result := 0;
end;

procedure GTKAPIWidgetClient_Realize(Widget: PGTKWidget); cdecl;
var
  Attributes: TGdkWindowAttr;
  AttributesMask: gint;

  Client: PGTKAPIWidgetClient;
begin
//  Assert(False, 'Trace:[GTKAPIWidgetClient_Realize]');
   
  Client := PGTKAPIWidgetClient(Widget); 
  
  gtk_widget_set_flags(Widget, GTK_REALIZED);

  with Attributes do
  begin
    Window_type := gdk_window_child;
    X := Widget^.allocation.x;
    Y := Widget^.allocation.y;
    Width := Widget^.allocation.width;
    Height := Widget^.allocation.height;
    WClass := GDK_INPUT_OUTPUT;
    Visual := gtk_widget_get_visual(Widget);
    Colormap := gtk_widget_get_colormap(Widget);
    Event_mask := gtk_widget_get_events(Widget) 
      or GDK_EXPOSURE_MASK or GDK_BUTTON_PRESS_MASK or GDK_BUTTON_RELEASE_MASK 
      or GDK_BUTTON_MOTION_MASK or GDK_ENTER_NOTIFY_MASK or GDK_LEAVE_NOTIFY_MASK 
      or GDK_KEY_PRESS_MASK or GDK_KEY_RELEASE_MASK;
  end;
  AttributesMask := GDK_WA_X or GDK_WA_Y or GDK_WA_VISUAL or GDK_WA_COLORMAP;

  Widget^.Window := gdk_window_new(gtk_widget_get_parent_window(Widget),
                                   @Attributes, AttributesMask);

  gdk_window_set_user_data(Widget^.Window, Client);

  with Attributes do
  begin
    X := PGTKStyle(Widget^.theStyle)^.Klass^.XThickness;
    Y := PGTKStyle(Widget^.theStyle)^.Klass^.YThickness;
    Width := Widget^.Allocation.Width - Attributes.X * 2;
    Height := Widget^.Allocation.Height - Attributes.Y * 2;
//    Cursor := gdk_cursor_new(GDK_XTERM);
  end;
//  AttributesMask := AttributesMask or GDK_WA_CURSOR;

  Client^.OtherWindow := gdk_window_new(Widget^.Window, @Attributes, AttributesMask);
  gdk_window_set_user_data (Client^.OtherWindow, Client);

  Widget^.theStyle := gtk_style_attach(Widget^.theStyle, Widget^.Window);

  gtk_style_set_background (Widget^.theStyle, Widget^.Window, GTK_STATE_NORMAL);
//  gdk_window_set_background(Widget^.Window, @PGTKStyle(Widget^.theStyle)^.Base[gtk_widget_state(Widget)]);
//  gdk_window_set_background (Client^.OtherWindow, @PGTKStyle(Widget^.theStyle)^.Base[gtk_widget_state(Widget)]);
end;

procedure GTKAPIWidgetClient_UnRealize(Widget: PGTKWidget); cdecl;
begin
  with PGTKAPIWidgetClient(Widget)^.Caret do 
    if Timer <> 0 then begin
      gtk_timeout_remove(Timer);
      Timer:=0;
    end;
end;
  
function GTKAPIWidgetClient_KeyPress(Widget: PGTKWidget;
  Event: PGDKEventKey): gint; cdecl;
begin
//  Assert(False, 'Trace:[GTKAPIWidgetClient_KeyPress]');
  // supress further processing
  Result := gtk_True;
end;

function GTKAPIWidgetClient_ButtonPress(Widget: PGTKWidget;
  Event: PGDKEventButton): gint; cdecl;
begin
  if not gtk_widget_has_focus(Widget)
  then gtk_widget_grab_focus(Widget);
  
  Result := gtk_False;
end;

function GTKAPIWidgetClient_FocusIn(Widget: PGTKWidget;
  Event: PGdkEventFocus): gint; cdecl;
begin
//  Assert(False, 'Trace:[GTKAPIWidgetClient_FocusIn]');
  gtk_widget_set_flags(Widget, GTK_HAS_FOCUS);
  GTKAPIWidgetClient_DrawCaret(PGTKAPIWidgetClient(Widget));
  Result := gtk_False;
end;

function GTKAPIWidgetClient_FocusOut(Widget: PGTKWidget;
  Event: PGdkEventFocus): gint; cdecl;
begin
//  Assert(False, 'Trace:[GTKAPIWidgetClient_FocusOut]');
  gtk_widget_unset_flags(Widget, GTK_HAS_FOCUS);
  GTKAPIWidgetClient_DrawCaret(PGTKAPIWidgetClient(Widget));
  Result := gtk_False;
end;

procedure GTKAPIWidgetClient_ClassInit(theClass: Pointer);cdecl;
// theClass: PGTKAPIWidgetClientClass
var 
  ObjectClass: PGTKObjectClass;
  WidgetClass: PGTKWidgetClass;
  SignalID: Guint;
begin
  ObjectClass := PGTKObjectClass(theClass);
  WidgetClass := PGTKWidgetClass(theClass);
  
  SignalID := gtk_signal_new(
    'set_scroll_adjustments',
    GTK_RUN_FIRST,
    ObjectClass^.thetype,
    (@PGTKAPIWidgetClientClass(theClass)^.set_scroll_adjustments - Pointer(theClass)),
    @gtk_marshal_NONE__POINTER_POINTER,
    GTK_TYPE_NONE,
    2, 
    [gtk_adjustment_get_type, gtk_adjustment_get_type]
  );

  with WidgetClass^ do
  begin
    set_scroll_adjustments_signal := SignalID;
    Realize := @GTKAPIWidgetClient_Realize;
    Button_Press_Event := @GTKAPIWidgetClient_ButtonPress;
    Key_Press_Event := @GTKAPIWidgetClient_KeyPress;
    focus_in_event := @GTKAPIWidgetClient_FocusIn;
    focus_out_event := @GTKAPIWidgetClient_FocusOut;
  end;

  PGTKAPIWidgetClientClass(theClass)^.set_scroll_adjustments := nil;
end;

procedure GTKAPIWidgetClient_Init(Client, theClass: Pointer); cdecl;
// Client: PGTKAPIWidgetClient
// theClass: PGTKAPIWidgetClientClass
begin
  gtk_widget_set_flags(PGTKWidget(Client), GTK_CAN_FOCUS);

  with PGTKAPIWidgetClient(Client)^.Caret do 
  begin
    Visible := False;
    IsDrawn := False;
    Blinking := True;
    X := 0;
    Y := 0;
    Width := 1;
    Height := 10;
    Pixmap := nil;
    BackPixmap := nil;
    Timer := 0;
  end;
end;

function GTKAPIWidgetClient_GetType: Guint;
const 
  TheType: Guint = 0;
  Info: TGTKTypeInfo = (
    type_name: 'LCLWinapiClient';
    object_size: SizeOf(TGTKAPIWidgetClient)+100;
    class_size: SizeOf(TGTKAPIWidgetClientClass)+100;
    class_init_func: @GTKAPIWidgetClient_ClassInit;
    object_init_func : @GTKAPIWidgetClient_Init;
  );
begin
  if (TheType = 0) 
  then TheType := gtk_type_unique(gtk_widget_get_type, @Info);

  Result := TheType;
end;

function GTKAPIWidgetClient_New: PGTKWidget;
begin
  Result := PGTKWidget(gtk_type_new(GTKAPIWidgetClient_GetType()));
end;

procedure GTKAPIWidgetClient_HideCaret(Client: PGTKAPIWidgetClient); 
begin
  if Client = nil
  then begin
    WriteLn('WARNING: [GTKAPIWidgetClient_HideCaret] Got nil client');
    Exit;
  end;
  Client^.Caret.Visible := False;
  GTKAPIWidgetClient_DrawCaret(Client);
end;

procedure GTKAPIWidgetClient_DrawCaret(Client: PGTKAPIWidgetClient); 
const
  GC_STATE: array[Boolean] of TGtkStateType =
                                 (GTK_STATE_INSENSITIVE, GTK_STATE_NORMAL);
var
  Widget: PGTKWidget;
begin
  if Client = nil then begin
    WriteLn('WARNING: [GTKAPIWidgetClient_DrawCaret] Got nil client');
    Exit;
  end;
  Widget := PGTKWidget(Client);
  
  with Client^.Caret do begin
    if (Timer <> 0) and ((not Blinking) or (not Visible)) then begin
      gtk_timeout_remove(Timer);
      Timer := 0;
    end;
    
    if IsDrawn and ((not Visible) or Blinking) then begin
      // hide caret
      if (BackPixmap <> nil) and (Widget<>nil) and (Widget^.theStyle<>nil)
      then gdk_draw_pixmap(
        Widget^.Window, 
        PGTKStyle(Widget^.theStyle)^.bg_gc[GTK_STATE_NORMAL], 
        BackPixmap, 0, 0, X, Y, Width, Height
      );
      IsDrawn := False;
    end
    else
    if Visible
    and gtk_widget_has_focus(Widget)
    and (not IsDrawn)
    then begin
      if Pixmap <> nil then
        Assert(False, 'Trace:TODO: [GTKAPIWidgetClient_ShowCaret] Implement bitmap');
      
      //Create backbitmap if needed
      if (BackPixmap = nil) and (Widget^.Window<>nil)
      then BackPixmap := gdk_pixmap_new(Widget^.Window, Width, Height, -1);
      
      if (BackPixmap <> nil) and (Widget<>nil) and ((Widget^.theStyle)<>nil)
      then gdk_draw_pixmap(
        BackPixmap, 
        PGTKStyle(Widget^.theStyle)^.bg_gc[GTK_STATE_NORMAL], 
        Widget^.Window, X, Y, 0, 0, Width, Height
      );
      
      // draw caret
      if PGTKStyle(PGTKWidget(Client)^.theStyle)<>nil then
        gdk_draw_rectangle(
          PGTKWidget(Client)^.Window, 
          PGTKStyle(PGTKWidget(Client)^.theStyle)^.fg_gc[GC_STATE[Integer(Pixmap) <> 1]],
          1, X, Y, Width, Height
        );
      IsDrawn := True;
    end;
    
    if Visible and Blinking and (Timer = 0) and gtk_widget_has_focus(Widget)
    then
      Timer := gtk_timeout_add(500, @GTKAPIWidgetClient_Timer, Client);
  end;
end;

procedure GTKAPIWidgetClient_ShowCaret(Client: PGTKAPIWidgetClient); 
begin
  if Client = nil 
  then begin
    WriteLn('WARNING: [GTKAPIWidgetClient_ShowCaret] Got nil client');
    Exit;
  end;
  
  Client^.Caret.Visible := True;
  GTKAPIWidgetClient_DrawCaret(Client);
end;

procedure GTKAPIWidgetClient_CreateCaret(Client: PGTKAPIWidgetClient;
  AWidth, AHeight: Integer; ABitmap: PGDKPixmap); 
var
  IsVisible: Boolean;
begin
  if Client = nil 
  then begin
    WriteLn('WARNING: [GTKAPIWidgetClient_HideCaret] Got nil client');
    Exit;
  end;

  with Client^.Caret do
  begin
    IsVisible := Visible;
    if IsVisible then GTKAPIWidgetClient_HideCaret(Client);
    
    if (Width <> AWidth) or (Height <> AHeight) 
    then begin
      if BackPixmap <> nil then gdk_pixmap_unref(BackPixmap);
      BackPixmap := nil;
      Width := AWidth;
      Height := AHeight;
    end;
    
    Pixmap := ABitmap;

    if IsVisible then GTKAPIWidgetClient_ShowCaret(Client);
  end;
end;

procedure GTKAPIWidgetClient_SetCaretPos(Client: PGTKAPIWidgetClient;
  AX, AY: Integer);
var
  IsVisible: Boolean;
begin
//Writeln('[GTKAPIWIDGETCLIENT] SetCaretPos '+inttostr(ax)+','+Inttostr(ay));

  if Client = nil 
  then begin
    WriteLn('WARNING: [GTKAPIWidgetClient_SetCaretPos] Got nil client');
    Exit;
  end;
  
  with Client^.Caret do
  begin
    IsVisible := Visible;
    if IsVisible then GTKAPIWidgetClient_HideCaret(Client);
    X := AX;
    Y := AY;
    if IsVisible then GTKAPIWidgetClient_ShowCaret(Client);
  end;
end;

procedure GTKAPIWidgetClient_GetCaretPos(Client: PGTKAPIWidgetClient;
  var X, Y: Integer); 
begin
  if Client = nil 
  then begin
    WriteLn('WARNING: [GTKAPIWidgetClient_GetCaretPos] Got nil client');
    Exit;
  end;
  
  X := Client^.Caret.X;
  Y := Client^.Caret.Y;
end;

//---------------------------------------------------------------------------
// GTKAPIWidget
//---------------------------------------------------------------------------

function GTKAPIWidget_FocusIn(Widget: PGTKWidget; Event: PGdkEventFocus): gint; cdecl;
var
  TopLevel: PGTKWidget;
begin
  Assert(False, 'Trace:[GTKAPIWidget_FocusIn]');

  TopLevel := gtk_widget_get_toplevel(Widget);
  if gtk_type_is_a(gtk_object_type(PGTKObject(TopLevel)), gtk_window_get_type) 
  then gtk_window_set_focus(PGTKWindow(TopLevel), PGTKAPIWidget(Widget)^.Client);
  
  Result := -1;
end;

function GTKAPIWidget_FocusOut(Widget: PGTKWidget; Event: PGdkEventFocus): gint; cdecl;
begin
  Assert(False, 'Trace:[GTKAPIWidget_FocusOut]');
  Result := -1;
end;


procedure GTKAPIWidget_ClassInit(wawClass: Pointer); cdecl;
//wawClass: PGTKAPIWidgetClass
var 
  WidgetClass: PGTKWidgetClass;
begin
  WidgetClass := PGTKWidgetClass(wawClass);
  
  WidgetClass^.focus_in_event := @GTKAPIWidget_FocusIn;
  WidgetClass^.focus_out_event := @GTKAPIWidget_FocusOut;
end;

procedure GTKAPIWidget_Init(waw, theClass: Pointer); cdecl;
// waw: PGTKAPIWidget; 
// theClass: PGTKAPIWidgetClass
var
  Widget: PGTKWidget;
  APIWidget: PGTKAPIWidget;
begin
  Widget := PGTKWidget(waw);
  APIWidget := PGTKAPIWidget(waw);
  
  gtk_widget_set_flags(Widget, GTK_CAN_FOCUS);

  APIWidget^.Client := GTKAPIWidgetClient_New;
  gtk_object_set_data(PGTKObject(Widget), 'Fixed', APIWidget^.Client);
  gtk_object_set_data(PGTKObject(APIWidget^.Client), 'Main', Widget);
  gtk_widget_show(APIWidget^.Client);
  
writeln('(gtkwinapiwindow.pp) GTKAPIWidget_Init B  check this:');
// MG: range check GTK-Critical warnings results possibly from
// function GTKAPIwidget_new.
// ToDo: check nil parameters
  gtk_container_add(PGTKContainer(Widget), APIWidget^.Client);
writeln('GTKAPIWidget_Init END');
end;

function GTKAPIWidget_GetType: Guint;
const 
  wawType: Guint = 0;
  wawInfo: TGTKTypeInfo = (
    type_name: 'LCLWinapiWidget';
    object_size: SizeOf(TGTKAPIWidget)+100;
    class_size: SizeOf(TGTKAPIWidgetClass)+100;
    class_init_func: @GTKAPIWidget_ClassInit;
    object_init_func : @GTKAPIWidget_Init;
  );
begin
  if (wawType = 0) 
  then wawType := gtk_type_unique(gtk_scrolled_window_get_type, @wawInfo);
  Result := wawType;
end;

function GTKAPIWidget_new: PGTKWidget;
begin
writeln('(gtkwinapiwindow.pp) GTKAPIWidget_new, ToDo: check parameters, gtk-Critical');
// ToDo: check these nil parameters
  Result := gtk_widget_new(
    GTKAPIWidget_GetType,
    'hadjustment', [nil,  // what are these nils?
    'vadjustment', nil,
    nil]
  );
writeln('GTKAPIWidget_new END');
end;

procedure GTKAPIWidget_CreateCaret(APIWidget: PGTKAPIWidget;
  AWidth, AHeight: Integer; ABitmap: PGDKPixmap); 
begin
  if APIWidget = nil 
  then begin
    WriteLn('WARNING: [GTKAPIWidget_CreateCaret] Got nil client');
    Exit;
  end;
  GTKAPIWidgetClient_CreateCaret(PGTKAPIWidgetClient(APIWidget^.Client),
    AWidth, AHeight, ABitmap);
end;

procedure GTKAPIWidget_HideCaret(APIWidget: PGTKAPIWidget); 
begin
  if APIWidget = nil 
  then begin
    WriteLn('WARNING: [GTKAPIWidget_HideCaret] Got nil client');
    Exit;
  end;
  GTKAPIWidgetClient_HideCaret(PGTKAPIWidgetClient(APIWidget^.Client));
end;

procedure GTKAPIWidget_ShowCaret(APIWidget: PGTKAPIWidget); 
begin
  if APIWidget = nil 
  then begin
    WriteLn('WARNING: [GTKAPIWidget_ShowCaret] Got nil client');
    Exit;
  end;
  GTKAPIWidgetClient_ShowCaret(PGTKAPIWidgetClient(APIWidget^.Client));
end;

procedure GTKAPIWidget_SetCaretPos(APIWidget: PGTKAPIWidget; X, Y: Integer);
begin
  if APIWidget = nil 
  then begin
    WriteLn('WARNING: [GTKAPIWidget_SetCaretPos] Got nil client');
    Exit;
  end;
  GTKAPIWidgetClient_SetCaretPos(PGTKAPIWidgetClient(APIWidget^.Client), X, Y);
end;

procedure GTKAPIWidget_GetCaretPos(APIWidget: PGTKAPIWidget; var X, Y: Integer); 
begin
  if APIWidget = nil 
  then begin
    WriteLn('WARNING: [GTKAPIWidget_GetCaretPos] Got nil client');
    Exit;
  end;
  GTKAPIWidgetClient_GetCaretPos(PGTKAPIWidgetClient(APIWidget^.Client), X, Y);
end;

end.

{ =============================================================================

  $Log$
  Revision 1.11  2001/08/07 11:05:51  lazarus
  MG: small bugfixes

  Revision 1.10  2001/07/02 15:17:24  lazarus
  MG: fixed wordcompletion and carettimer nonfocus bug

  Revision 1.9  2001/06/14 23:13:30  lazarus
  MWE:
    * Fixed some syntax errors for the latest 1.0.5 compiler

  Revision 1.8  2001/06/12 18:31:01  lazarus
  MG: small bugfixes

  Revision 1.7  2001/06/04 09:32:17  lazarus
  MG: fixed bugs and cleaned up messages

  Revision 1.6  2001/03/27 11:11:13  lazarus
  MG: fixed mouse msg, added filedialog initialdir

  Revision 1.5  2001/03/26 14:58:32  lazarus
  MG: setwindowpos + bugfixes

  Revision 1.4  2001/03/13 15:02:14  lazarus
  MG: activated GetWindowOrgEx

  Revision 1.3  2001/02/16 19:13:31  lazarus
  Added some functions
  Shane

  Revision 1.2  2001/02/06 18:19:38  lazarus
  Shane

  Revision 1.1  2000/07/13 10:28:30  michael
  + Initial import

  Revision 1.2  2000/05/10 01:45:13  lazarus
  Replaced writelns with Asserts.
  Put ERROR and WARNING messages back to writelns.            CAW

  Revision 1.1  2000/03/30 22:51:43  lazarus
  MWE:
    Moved from ../../lcl

  Revision 1.9  2000/03/13 23:17:34  lazarus
  MWE:
    + finished hide caret
    + added blinking caret

  Revision 1.8  2000/03/03 22:58:26  lazarus
  MWE:
    Fixed focussing problem.
      LM-FOCUS was bound to the wrong signal
    Added GetKeyState api func.
      Now LCL knows if shift/trl/alt is pressed (might be handy for keyboard
      selections ;-)

  Revision 1.7  2000/02/22 23:26:13  lazarus
  MWE: Fixed cursor movement in editor
       Started on focus problem

  Revision 1.6  2000/01/25 23:51:14  lazarus
  MWE:
    Added more Caret functionality.
    Removed old ifdef stuff from the editor

  Revision 1.5  2000/01/25 22:04:27  lazarus
  MWE:
    The first primitive Caret functions are getting visible

  Revision 1.4  2000/01/25 00:38:25  lazarus
  MWE:
    Added GetFocus

  Revision 1.3  2000/01/14 00:33:31  lazarus
  MWE:
    Added Scrollbar messages

  Revision 1.2  2000/01/13 22:44:05  lazarus
  MWE:
    Created/updated net gtkwidget for TWinControl decendants
      also improved foccusing on such a control

  Revision 1.1  2000/01/10 00:07:13  lazarus
  MWE:
    Added more scrollbar support for TWinControl
    Most signals for TWinContorl are jet connected to the wrong widget
      (now scrolling window, should be fixed)
    Added some cvs entries


}
