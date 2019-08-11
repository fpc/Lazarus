{
 /***************************************************************************
                             Gtk2WinapiWindow.pp
                             -------------------
                       gtkimplementation for basic window
                   Initial Revision  : Sun Jan 9 16:00:00 GMT+1 2000


 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
{
@abstract(A GTK widget to support controls derived from a wincontrol)
@author(TGTK2WinapiWindow - Marc Weustink <marc@@freepascal.org>)
@created(2000)
@lastmod(2004)
}
unit Gtk2WinapiWindow;

{$mode objfpc}{$H+}

interface

uses
  // RTL
  SysUtils, glib2, gdk2, gtk2,
  // LCL
  LCLProc, Gtk2Extra, Controls, Gtk2Proc, Gtk2Def;

{ $Define VerboseCaret}
// the gtk has a function to draw the cursor, but it does not support xor
// so it does not work with synedit and twilight hightlighter settings
{off $DEFINE Has_gtk_draw_insertion_cursor}

type
  PGTKAPIWidget = ^TGTKAPIWidget;
  TGTKAPIWidget = record
    // ! the ScrolledWindow must be the first attribute of this record !
    ScrolledWindow: TGTKScrolledWindow;
    Reserved1: Word; // workaround gtk2 win32 fpc bug: SizeOf(TGTKScrolledWindow) is less than in real
    Frame: PGtkFrame;
    Client: PGtkWidget;
  end;
  
  PGTKAPIWidgetClass = ^TGTKAPIWidgetClass;
  TGTKAPIWidgetClass = record
    ParentClass: TGTKScrolledWindowClass;
  end;

procedure HideCaretOfWidgetGroup(ChildWidget: PGtkWidget;
  var MainWidget: PGtkWidget; var CaretWasVisible: boolean);

function GTKAPIWidget_GetType: GType;
function GTKAPIWidget_New: PGTKWidget;
procedure GTKAPIWidget_CreateCaret(APIWidget: PGTKAPIWidget;
                                 AWidth, AHeight: Integer; ABitmap: PGDKPixmap); 
procedure GTKAPIWidget_DestroyCaret(APIWidget: PGTKAPIWidget); 
procedure GTKAPIWidget_InvalidateCaret(APIWidget: PGTKAPIWidget);
procedure GTKAPIWidget_HideCaret(APIWidget: PGTKAPIWidget; var OldVisible: boolean);
procedure GTKAPIWidget_ShowCaret(APIWidget: PGTKAPIWidget); 
procedure GTKAPIWidget_SetCaretPos(APIWidget: PGTKAPIWidget; X, Y: Integer); 
procedure GTKAPIWidget_GetCaretPos(APIWidget: PGTKAPIWidget; var X, Y: Integer); 
procedure GTKAPIWidget_SetCaretRespondToFocus(APIWidget: PGTKAPIWidget;
  ShowHideOnFocus: boolean); 
procedure GTKAPIWidget_GetCaretRespondToFocus(APIWidget: PGTKAPIWidget;
  var ShowHideOnFocus: boolean);

procedure GTKAPIWidget_SetShadowType(APIWidget: PGTKAPIWidget; AShadowType: TGtkShadowType);

function GTK_APIWIDGETCLIENT_TYPE: GType;

implementation

const
  CURSOR_ON_MULTIPLIER    = 2;
  CURSOR_OFF_MULTIPLIER   = 1;
  CURSOR_DIVIDER          = 3;

//---------------------------------------------------------------------------
// gtk_winapiwindow_internal
//---------------------------------------------------------------------------
type
  TCaretInfo = record
    X: Integer;
    Y: Integer;
    Width: Integer;
    Height: Integer;
    Visible: Boolean;   // Caret is on (can be visible/invisible due to Blinking)
    IsDrawn: Boolean;   // Caret is visible at the moment
    Blinking: Boolean;  // Caret should blink
    BlinkTime: Integer; // Blink time = show + hide time
    BlinkTimeout: Integer; // Time after which if there is no user interaction happened blinking must finish
    BlinkHide: boolean; // current blinking phase is Hide
    Pixmap: PGDKPixMap;
    BackPixmap: PGDKPixMap;
    Timer: guint;
    ShowHideOnFocus: boolean; // true = hide on loose focus, show on get focus
    Invalidated: boolean;
  end;

  PGTKAPIWidgetClient = ^TGTKAPIWidgetClient;
  TGTKAPIWidgetClient = record
    // ! the Widget must be the first attribute of the record !
    Widget: TGtkFixed;
    Caret: TCaretInfo;
  end;
  
  PGTKAPIWidgetClientClass = ^TGTKAPIWidgetClientClass;
  TGTKAPIWidgetClientClass = record
    ParentClass: TGTKFixedClass;
//    ParentClass: TGTKWidgetClass;
   
    set_scroll_adjustments: procedure(Widget: PGTKWidget;
                               HAdjustment, VAdjustment: PGTKAdjustment); cdecl;
  end;

{------------------------------------------------------------------------------
  procedure HideCaretOfWidgetGroup(ChildWidget: PGtkWidget;
    var MainWidget: PGtkWidget; var CaretWasVisible: boolean);

  Find main widget and if it is a API widget, hide caret.
 ------------------------------------------------------------------------------}
procedure HideCaretOfWidgetGroup(ChildWidget: PGtkWidget;
  var MainWidget: PGtkWidget; var CaretWasVisible: boolean);
var
  LCLObject: TObject;
  IsAPIWidget: Boolean;
begin
  MainWidget:=ChildWidget;
  LCLObject:=GetNearestLCLObject(ChildWidget);
  if (LCLObject is TWinControl) then
    MainWidget:={%H-}PGtkWidget(TWinControl(LCLObject).Handle);
  IsAPIWidget:=GtkWidgetIsA(MainWidget,GTKAPIWidget_GetType);
  CaretWasVisible:=false;
  if IsAPIWidget then
    GTKAPIWidget_HideCaret(PGTKAPIWidget(MainWidget),CaretWasVisible);
end;

////////////////////////////////////////////////////
// TEMP solution until gtkmarshal.inc is implemeted
//      to get this compiled
////////////////////////////////////////////////////                                                                            
procedure gtk_marshal_VOID__POINTER_POINTER (closure: PGClosure; 
                                             return_value: PGValue;
                                             n_param_values: guint;
                                             param_values: PGValue;
                                             invocation_hint: gpointer;
                                             marshal_data: gpointer); cdecl; external gtklib;
////////////////////////////////////////////////////                                                                            

type
  GTKEventResult = gboolean;

var
  MParentClass: PGtkFixedClass;

function GTKAPIWidgetClient_Timer(Client: Pointer): GTKEventResult; cdecl; forward;
procedure GTKAPIWidgetClient_Realize(AWidget: PGTKWidget); cdecl; forward;
procedure GTKAPIWidgetClient_UnRealize(AWidget: PGTKWidget); cdecl; forward;
procedure GTKAPIWidgetClient_SizeAllocate (AWidget: PGTKWidget; AAllocation: PGtkAllocation); cdecl; forward;


function GTKAPIWidgetClient_KeyPress(Widget: PGTKWidget; 
  Event: PGDKEventKey): GTKEventResult; cdecl; forward;
function GTKAPIWidgetClient_ButtonPress({%H-}Widget: PGTKWidget;
  Event: PGDKEventButton): GTKEventResult; cdecl; forward;
function GTKAPIWidgetClient_FocusIn(AWidget: PGTKWidget;
  {%H-}Event: PGdkEventFocus): GTKEventResult; cdecl; forward;
function GTKAPIWidgetClient_FocusOut(AWidget: PGTKWidget;
  {%H-}Event: PGdkEventFocus): GTKEventResult; cdecl; forward;

procedure GTKAPIWidgetClient_ClassInit(theClass: Pointer);cdecl; forward;
procedure GTKAPIWidgetClient_Init(Client:PGTypeInstance; theClass: Pointer); cdecl; forward;
function GTKAPIWidgetClient_GetType: GType; forward;
function GTKAPIWidgetClient_New: PGTKWidget; forward;

procedure GTKAPIWidgetClient_HideCaret(Client: PGTKAPIWidgetClient;
                                       out OldVisible: boolean); forward;
procedure GTKAPIWidgetClient_ShowCaret(Client: PGTKAPIWidgetClient); forward;
procedure GTKAPIWidgetClient_DrawCaret(Client: PGTKAPIWidgetClient; {%H-}CalledByTimer: boolean); forward;
procedure GTKAPIWidgetClient_CreateCaret(Client: PGTKAPIWidgetClient;
  AWidth, AHeight: Integer; ABitmap: PGDKPixmap); forward;
procedure GTKAPIWidgetClient_DestroyCaret(Client: PGTKAPIWidgetClient); forward;
procedure GTKAPIWidgetClient_InvalidateCaret(Client: PGTKAPIWidgetClient); forward;
function GTKAPIWidgetClient_IsPainting(Client: PGTKAPIWidgetClient): boolean; forward;
procedure GTKAPIWidgetClient_SetCaretPos(Client: PGTKAPIWidgetClient;
  AX, AY: Integer); forward;
procedure GTKAPIWidgetClient_GetCaretPos(Client: PGTKAPIWidgetClient;
  var X, Y: Integer); forward;
procedure GTKAPIWidgetClient_SetCaretRespondToFocus(Client: PGTKAPIWidgetClient;
  ShowHideOnFocus: boolean); forward;
procedure GTKAPIWidgetClient_GetCaretRespondToFocus(Client: PGTKAPIWidgetClient;
  var ShowHideOnFocus: boolean); forward;

function GTKAPIWidgetClient_GetCursorBlink(Client: PGTKAPIWidgetClient): gboolean; forward;
function GTKAPIWidgetClient_GetCursorBlinkTime(Client: PGTKAPIWidgetClient): gint; forward;
function GTKAPIWidgetClient_GetCursorBlinkTimeout(Client: PGTKAPIWidgetClient): gint; forward;
//-----------------------------

procedure GTKAPIWidget_SetShadowType(APIWidget: PGTKAPIWidget;
  AShadowType: TGtkShadowType);
begin
  if (APIWidget^.Frame <> nil) then
    gtk_frame_set_shadow_type(APIWidget^.Frame, AShadowType)
  else
    gtk_scrolled_window_set_shadow_type(PGtkScrolledWindow(APIWidget), AShadowType);
end;

function GTK_APIWIDGETCLIENT_TYPE: GType;
begin
  GTK_APIWIDGETCLIENT_TYPE := GTKAPIWidgetClient_GetType;
end;


function GTKAPIWidgetClient_GetType: GType;
const 
  TYPE_NAME = 'LCLWinapiClient';
  TheType: GType = 0;
  Info: TGTKTypeInfo = (
    type_name: TYPE_NAME;
    object_size: SizeOf(TGTKAPIWidgetClient){+100};
    class_size:  SizeOf(TGTKAPIWidgetClientClass){+100};
    class_init_func:  @GTKAPIWidgetClient_ClassInit;
    object_init_func: @GTKAPIWidgetClient_Init;
    reserved_1: nil;
    reserved_2: nil;
    base_class_init_func: nil;
  );
begin
  if (TheType = 0)
  then begin
    TheType := gtk_type_from_name(TYPE_NAME);
    if TheType = 0 then TheType := gtk_type_unique(GTK_TYPE_FIXED, @Info);
  end;
  Result := TheType;
end;

procedure GTKAPIWidgetClient_ClassInit(theClass: Pointer);cdecl;
// theClass: PGTKAPIWidgetClientClass
var
  ObjectClass: PGTKObjectClass;
  WidgetClass: PGTKWidgetClass;
  ClientClass: PGTKAPIWidgetClientClass;
  SignalID: Guint;
begin
  ObjectClass := PGTKObjectClass(theClass);
  WidgetClass := PGTKWidgetClass(theClass);
  ClientClass := PGTKAPIWidgetClientClass(theClass);
  
  MParentClass := gtk_type_class(gtk_fixed_get_type);
  SignalID := gtk_signal_new(
    'set_scroll_adjustments',
    GTK_RUN_FIRST,
    gtk_class_type(ObjectClass),
    (@ClientClass^.set_scroll_adjustments - Pointer(theClass)),
    @gtk_marshal_VOID__POINTER_POINTER,
    GTK_TYPE_NONE,
    2, 
    [gtk_adjustment_get_type, gtk_adjustment_get_type]
  );
  
  ClientClass^.set_scroll_adjustments := nil;

  with WidgetClass^ do
  begin
    set_scroll_adjustments_signal := SignalID;
    realize := @GTKAPIWidgetClient_Realize;
    unrealize := @GTKAPIWidgetClient_UnRealize;
    size_allocate := @GTKAPIWidgetClient_SizeAllocate;
    button_press_event := @GTKAPIWidgetClient_ButtonPress;
    key_press_event := @GTKAPIWidgetClient_KeyPress;
    focus_in_event := @GTKAPIWidgetClient_FocusIn;
    focus_out_event := @GTKAPIWidgetClient_FocusOut;
  end;
end;

procedure GTKAPIWidgetClient_Init(Client:PGTypeInstance; theClass: Pointer); cdecl;
// Client: PGTKAPIWidgetClient
// theClass: PGTKAPIWidgetClientClass
begin
  if theClass=nil then ;
  gtk_widget_set_flags(PGTKWidget(Client), GTK_CAN_FOCUS);
  gtk_widget_set_flags(PGTKWidget(Client), GTK_CAN_DEFAULT);
  gtk_widget_unset_flags(PGTKWidget(Client), GTK_NO_WINDOW);
  with PGTKAPIWidgetClient(Client)^.Caret do
  begin
    Visible := False;
    IsDrawn := False;
    Blinking := GTKAPIWidgetClient_GetCursorBlink(PGTKAPIWidgetClient(Client));
    BlinkTime := GTKAPIWidgetClient_GetCursorBlinkTime(PGTKAPIWidgetClient(Client));
    BlinkTimeout := GTKAPIWidgetClient_GetCursorBlinkTimeout(PGTKAPIWidgetClient(Client));
    if (gtk_major_version = 2) and (gtk_minor_version >= 18) then
    begin
      if not Blinking and (BlinkTimeOut > 0) then
        Blinking := True;
    end;
    X := 0;
    Y := 0;
    Width := 1;
    Height := 10;
    Pixmap := nil;
    BackPixmap := nil;
    Timer := 0;
    ShowHideOnFocus := true;
  end;

  {$IFNDEF NoStyle}
  gtk_widget_set_app_paintable(PGTKWidget(Client),true);
  {$ENDIF}
end;

function GTKAPIWidgetClient_New: PGTKWidget;
begin
  Result := PGTKWidget(gtk_type_new(GTKAPIWidgetClient_GetType()));
end;

function GTKAPIWidgetClient_Timer(Client: Pointer): GTKEventResult; cdecl;
// returning 0 would stop the timer, 1 will restart it
var
  WClient: PGTKAPIWidgetClient;
begin
  Result := gtk_false;
  WClient := PGTKAPIWidgetClient(Client);
  if WClient^.Caret.Timer <> 0 then
  begin
    WClient^.Caret.BlinkHide := not WClient^.Caret.BlinkHide;
    GTKAPIWidgetClient_DrawCaret(Client,true);
    if WClient^.Caret.Timer <> 0 then
      Result := gtk_true;
  end;
end;

procedure GTKAPIWidgetClient_Realize(AWidget: PGTKWidget); cdecl;

// All //@ marked lines are already set by the inherited realize
// we only have to (re)set the event mask

var
  Info: PWidgetInfo;
//@  Attributes: TGdkWindowAttr;
//@  AttributesMask: gint;
begin
  PGTKWidgetClass(MParentClass)^.realize(AWidget);
  
//@  gtk_widget_set_flags(AWidget, GTK_REALIZED);
  
  gtk_widget_set_double_buffered(AWidget, True); // True bites caret => ToDo
  gtk_widget_set_redraw_on_allocate(AWidget, False);

//@  with Attributes do
//@  begin
//@    Window_type := gdk_window_child;
//@    X := AWidget^.allocation.x;
//@    Y := AWidget^.allocation.y;
//@    Width := AWidget^.allocation.width;
//@    Height := AWidget^.allocation.height;
//@    WClass := GDK_INPUT_OUTPUT;
//@    Visual := gtk_widget_get_visual(AWidget);
//@    Colormap := gtk_widget_get_colormap(AWidget);
//@    Event_mask := gtk_widget_get_events(AWidget)
//@      or GDK_EXPOSURE_MASK or GDK_BUTTON_PRESS_MASK or GDK_BUTTON_RELEASE_MASK
//@      or GDK_BUTTON_MOTION_MASK or GDK_ENTER_NOTIFY_MASK or GDK_LEAVE_NOTIFY_MASK
//@      or GDK_KEY_PRESS_MASK or GDK_KEY_RELEASE_MASK;
//@  end;
//@  AttributesMask := GDK_WA_X or GDK_WA_Y or GDK_WA_VISUAL or GDK_WA_COLORMAP;
//@
//@  AWidget^.Window := gdk_window_new(gtk_widget_get_parent_window(AWidget),
//@                                    @Attributes, AttributesMask);
//@
//@  gdk_window_set_user_data(AWidget^.Window, AWidget);

  gdk_window_set_events(AWidget^.Window, gdk_window_get_events(AWidget^.Window)
    or GDK_EXPOSURE_MASK or GDK_BUTTON_PRESS_MASK or GDK_BUTTON_RELEASE_MASK
    or GDK_BUTTON_MOTION_MASK or GDK_ENTER_NOTIFY_MASK or GDK_LEAVE_NOTIFY_MASK
    or GDK_KEY_PRESS_MASK or GDK_KEY_RELEASE_MASK);

//@  AWidget^.Style := gtk_style_attach(AWidget^.Style, AWidget^.Window);
//@  gtk_style_set_background(AWidget^.Style, AWidget^.Window, GTK_STATE_NORMAL);
  Info := GetWidgetInfo(AWidget);
  if (Info = nil) or ([wwiNoEraseBkgnd] * Info^.Flags = []) then begin
    gdk_window_set_back_pixmap(AWidget^.Window, nil, GdkFalse);
    if AWidget^.parent<>nil then begin
      // clear parent window background pixmap too, just in case AWidget is a nowindow
      // see note of gtk docs about gtk_widget_modify_bg
      gdk_window_set_back_pixmap(AWidget^.parent^.Window, nil, GdkFalse);
    end;
  end;
end;

procedure GTKAPIWidgetClient_UnRealize(AWidget: PGTKWidget); cdecl;
begin
  with PGTKAPIWidgetClient(AWidget)^.Caret do 
  begin
    if Timer <> 0 
    then begin
      gtk_timeout_remove(Timer);
      Timer := 0;
    end;
  end;
    
  PGTKWidgetClass(MParentClass)^.unrealize(AWidget);
end;                        

procedure GTKAPIWidgetClient_SizeAllocate(AWidget: PGTKWidget; 
  AAllocation: PGtkAllocation); cdecl; 
begin
  PGTKWidgetClass(MParentClass)^.size_allocate(AWidget, AAllocation);

end;

  
function GTKAPIWidgetClient_KeyPress(Widget: PGTKWidget;
  Event: PGDKEventKey): GTKEventResult; cdecl;
begin
  if (Widget=nil) or (Event=nil) then ;
  // DO NOT supress further processing. The next one who changes that please do the debugging too.
  // just do not.
  Result := gtk_False;
end;

function GTKAPIWidgetClient_ButtonPress(Widget: PGTKWidget;
  Event: PGDKEventButton): GTKEventResult; cdecl;
begin
  {$IFDEF VerboseFocus}
  DebugLn('GTKAPIWidgetClient_ButtonPress ',DbgS(Widget));
  {$ENDIF}
  if Event=nil then ;
  Result := gtk_False;
end;

function GTKAPIWidgetClient_FocusIn(AWidget: PGTKWidget;
  Event: PGdkEventFocus): GTKEventResult; cdecl;
begin
  {$IFDEF VerboseFocus}
  DebugLn('GTKAPIWidgetClient_FocusIn ',DbgS(AWidget),' ',dbgs(event^._in));
  {$ENDIF}
  
  gtk_widget_set_flags(AWidget, GTK_HAS_FOCUS);
  GTKAPIWidgetClient_DrawCaret(PGTKAPIWidgetClient(AWidget), False);
  gtk_widget_queue_draw(AWidget);
  Result := gtk_False;
end;

function GTKAPIWidgetClient_FocusOut(AWidget: PGTKWidget;
  Event: PGdkEventFocus): GTKEventResult; cdecl;
begin
  {$IFDEF VerboseFocus}
  DebugLn('GTKAPIWidgetClient_FocusOut ',DbgS(AWidget),' ',dbgs(event^._in));
  {$ENDIF}

  gtk_widget_unset_flags(AWidget, GTK_HAS_FOCUS);
  GTKAPIWidgetClient_DrawCaret(PGTKAPIWidgetClient(AWidget), False);
  gtk_widget_queue_draw(AWidget);
  Result := gtk_False;
end;

procedure GTKAPIWidgetClient_HideCaret(Client: PGTKAPIWidgetClient;
  out OldVisible: boolean);
begin
  if Client = nil
  then begin
    OldVisible:=false;
    DebugLn('WARNING: [GTKAPIWidgetClient_HideCaret] Got nil client');
    Exit;
  end;
  {$IFDEF VerboseCaret}
  DebugLn(['GTKAPIWidgetClient_HideCaret ',DbgS(Client),' ShowHideOnFocus=',Client^.Caret.ShowHideOnFocus]);
  {$ENDIF}
  OldVisible:=Client^.Caret.Visible;
  Client^.Caret.Visible := False;
  GTKAPIWidgetClient_DrawCaret(Client,false);

  {if (Client^.Caret.IsDrawn) then begin
    with Client^.Caret do begin
      DebugLn('GTKAPIWidgetClient_ShowCaret IsDrawn=',dbgs(IsDrawn),' Visible=',dbgs(Visible),
        ' Blinking='+dbgs(Blinking),' HasFocus=',dbgs(gtk_widget_has_focus(PGtkWidget(Client))),
        ' ShowHideOnFocus='+dbgs(ShowHideOnFocus),
        ' Window='+dbgs(PGtkWidget(Client)^.Window<>nil),
        ' Style='+dbgs(gtk_widget_get_style(PGtkWidget(Client))<>nil));
    end;
  end;}
end;

function GTKAPIWidgetClient_GetCursorBlink(Client: PGTKAPIWidgetClient): gboolean;
var
  settings: PGtkSettings;
begin
  settings := gtk_widget_get_settings(PGtkWidget(Client));
  g_object_get(settings, 'gtk-cursor-blink', @Result, nil);
end;

function GTKAPIWidgetClient_GetCursorBlinkTime(Client: PGTKAPIWidgetClient): gint;
var
  settings: PGtkSettings;
begin
  settings := gtk_widget_get_settings(PGtkWidget(Client));
  Result := 1200; // default value of gtk-cursor-blink-timeout
  // property valid since 2.12
  if (gtk_major_version = 2) and (gtk_minor_version > 10) then
    g_object_get(settings, 'gtk-cursor-blink-time', @Result, nil);
end;

function GTKAPIWidgetClient_GetCursorBlinkTimeout(Client: PGTKAPIWidgetClient): gint;
var
  settings: PGtkSettings;
begin
  settings := gtk_widget_get_settings(PGtkWidget(Client));
  Result := MaxInt; // default value of gtk-cursor-blink-timeout
  // property valid since 2.12
  if (gtk_major_version = 2) and (gtk_minor_version > 10) then
    g_object_get(settings, 'gtk-cursor-blink-timeout', @Result, nil);
end;

procedure GTKAPIWidgetClient_DrawCaret(Client: PGTKAPIWidgetClient; CalledByTimer: boolean);
{ ShowCaret/HideCaret are used in winapi like:
   ShowCaret (paint xor)
   Blinking (restore)
   StartPaintEvent
   HideCaret
   Painting
   ShowCaret
   EndPaintEvent
   Blinking

   Moving a caret works like this: HideCaret, move, ShowCaret

   The gtk2 uses double buffering with clipping.
   This means, during a paint event you can only paint in the clipping area,
   which does not need to be rectangular.
   => If the caret would be painted outside the paint event, then we can not hide
   it if the clipping area does not completely contain the old position.
   => Therefore we can only paint either inside or outside the paint event.
   Painting outside the paint event means that between painting and showing
   caret there are other events, so continuus painting will hardly show the
   caret. It appears to be almost invisible.
   => Therefore we must paint only inside the paint event
   Algorithm:
     InvalidateRect automatically invalidates the caret
     Hide
       outside paint event: invalidate and IsDrawn:=false
       inside paint event: IsDrawn:=false
     Show
       outside paint event: invalidate
       inside paint event: draw and IsDrawn:=true

     Blinking makes it more complicated, because a Hide triggers an OnPaint,
     which triggers in synedit code HideCaret+ShowCaret.
}
var
  Widget: PGTKWidget;
  WidgetStyle: PGTKStyle;
  HasFocus: boolean;
  WidgetIsPainting: Boolean;
{$IFDEF Has_gtk_draw_insertion_cursor}
  location: TGdkRectangle;
{$ENDIF}

  procedure DrawCursor(Pixmap: PGdkPixmap; X, Y, Width, Height: Integer);
  const
    GC_STATE: array[Boolean] of TGtkStateType =
   (
     GTK_STATE_INSENSITIVE,
     GTK_STATE_NORMAL
   );
  var
    ForeGroundGC: PGdkGC;
  begin
    // set draw function to xor
    ForeGroundGC := WidgetStyle^.fg_gc[GC_STATE[{%H-}PtrUInt(Pixmap) <> 1]];
    //gdk_gc_get_values(ForeGroundGC,@ForeGroundGCValues);
    //OldGdkFunction:=ForeGroundGCValues.thefunction;
    {$IFDEF VerboseCaret}
    DebugLn(['GTKAPIWidgetClient_DrawCaret Real Draw ',X,',',Y]);
    {$ENDIF}
    gdk_gc_set_function(ForeGroundGC,GDK_invert);
    try
      // draw the caret
      //DebugLn('DRAWING');
      gdk_draw_rectangle(
        Widget^.Window,
        ForeGroundGC,
        1,
        X, Y-1,  // Y-1 for Delphi compatibility
        Width, Height
      );
    finally
      // restore draw function
      gdk_gc_set_function(ForeGroundGC, GDK_COPY);
    end;
  end;

begin
  if Client = nil then
  begin
    DebugLn('WARNING: [GTKAPIWidgetClient_DrawCaret] Got nil client');
    Exit;
  end;

  Widget := PGTKWidget(Client);
  WidgetStyle := gtk_widget_get_style(Widget);
  WidgetIsPainting := GTKAPIWidgetClient_IsPainting(Client);

  with Client^.Caret do 
  begin
    HasFocus := gtk_widget_has_focus(Widget);
    if WidgetIsPainting then
      Invalidated := false;

    {$IFDEF VerboseCaret}
    DebugLn(['GTKAPIWidgetClient_DrawCaret START Client=',DbgS(Client),' Timer=',Timer,' Blink=',Blinking,' BlinkHide=',BlinkHide,' Visible=',Visible,' ShowHideOnFocus=',ShowHideOnFocus,' Focus=',gtk_widget_has_focus(Widget),' IsDrawn=',IsDrawn,' W=',Width,' H=',Height,' WidgetIsPainting=',WidgetIsPainting]);
    {$ENDIF}

    if IsDrawn and
       (
         (not Visible) or
         (Blinking and BlinkHide)
       ) then
    begin
      // hide caret (restore background)
      if WidgetIsPainting then
      begin
        if (BackPixmap <> nil) and (Widget<>nil) and (WidgetStyle<>nil) then
        begin
          gdk_draw_pixmap(
            Widget^.Window,
            WidgetStyle^.bg_gc[GTK_STATE_NORMAL],
            BackPixmap, 0, 0,
            X, Y-1, // Y-1 for Delphi compatibility
            Width, Height
          );
          {$IFDEF VerboseCaret}
          DebugLn(['GTKAPIWidgetClient_DrawCaret Real Hide ',X,',',Y]);
          {$ENDIF}
        end;
        IsDrawn := False;
        Invalidated:=false;
      end else
      begin
        // paint only during painting, otherwise invalidate
        {$IFDEF VerboseCaret}
        DebugLn(['GTKAPIWidgetClient_DrawCaret Invalidate Hide ',X,',',Y]);
        {$ENDIF}
        GTKAPIWidgetClient_InvalidateCaret(Client);
        IsDrawn := false;
      end;
    end
    else
    if Visible
    and (HasFocus or (not ShowHideOnFocus))
    and (not IsDrawn)
    and (not (Blinking and BlinkHide))
    and (Widget^.Window<>nil)
    and (WidgetStyle<>nil)
    then begin
      if WidgetIsPainting then
      begin
        //Create backbitmap if needed
        if (BackPixmap = nil)
        and (Widget^.Window<>nil)
        and (Width>0)
        and (Height>0)
        then
          BackPixmap := gdk_pixmap_new(Widget^.Window, Width, Height, -1);

        // store background
        if (BackPixmap <> nil)
        and (Widget<>nil)
        and (WidgetStyle<>nil)
        and (Width>0) and (Height>0)
        then begin
          {$IFDEF VerboseCaret}
          DebugLn(['GTKAPIWidgetClient_DrawCaret Store ',X,',',Y]);
          {$ENDIF}
          gdk_draw_pixmap(
            BackPixmap,
            WidgetStyle^.bg_gc[GTK_STATE_NORMAL],
            Widget^.Window,
              X, Y-1, // Y-1 for Delphi compatibility
              0, 0,
              Width, Height
          );
        end;

        // draw caret
        {$IFDEF VerboseCaret}
        DebugLn(['GTKAPIWidgetClient_DrawCaret SHOWING Client=',DbgS(Client)
        ,' ',cardinal(WidgetStyle)
        ,' ',cardinal(Widget^.Window)
        ,' X=',X,' Y=',Y
        ,' W=',Width
        ,' H=',Height
        ]);
        {$ENDIF}
        if (WidgetStyle<>nil)
        and (Widget^.Window<>nil)
        and (Width>0)
        and (Height>0)
        then begin
          {$IFDEF Has_gtk_draw_insertion_cursor}
          if Width <= 3 then
          begin
            location.x := X;
            location.y := Y - 1;
            location.width := 0;
            location.height := Height;
            gtk_draw_insertion_cursor(Widget, Widget^.Window, nil, @location, PtrUInt(Pixmap) <> 1,
               GTK_TEXT_DIR_LTR, false);
          end
          else
          {$ENDIF}
            DrawCursor(Pixmap, X, Y, Width, Height);
        end else
          DebugLn('***: Draw Caret failed: Client=',DbgS(Client),
            ' X='+dbgs(X)+' Y='+dbgs(Y)+' W='+dbgs(Width)+' H='+dbgs(Height),
            ' ',dbgs(Pixmap<>nil),',',dbgs(Widget^.Window),',',dbgs(WidgetStyle));
        IsDrawn := True;
        Invalidated:=false;
      end else begin
        // not in a paint event => use only invalidate
        {$IFDEF VerboseCaret}
        DebugLn(['GTKAPIWidgetClient_DrawCaret Invalidate Show']);
        {$ENDIF}
        GTKAPIWidgetClient_InvalidateCaret(Client);
      end;
    end;

    // stop, start timer
    if Visible and Blinking and ((not ShowHideOnFocus) or HasFocus) then
    begin
      if Timer = 0 then
        if IsDrawn then
          Timer := gtk_timeout_add(BlinkTime * CURSOR_ON_MULTIPLIER div CURSOR_DIVIDER,
            @GTKAPIWidgetClient_Timer, Client)
        else
          Timer := gtk_timeout_add(BlinkTime * CURSOR_OFF_MULTIPLIER div CURSOR_DIVIDER,
            @GTKAPIWidgetClient_Timer, Client)
    end else
    begin
      if Timer <> 0 then
      begin
        gtk_timeout_remove(Timer);
        Timer := 0;
      end;
    end;

    {$IFDEF VerboseCaret}
    DebugLn(['GTKAPIWidgetClient_DrawCaret END Client=',DbgS(Client),' Timer=',Timer,' Blink=',Blinking,' BlinkHide=',BlinkHide,' Visible=',Visible,' ShowHideOnFocus=',ShowHideOnFocus,' Focus=',gtk_widget_has_focus(Widget),' IsDrawn=',IsDrawn,' W=',Width,' H=',Height,' WidgetIsPainting=',WidgetIsPainting]);
    {$ENDIF}
  end;
end;

procedure GTKAPIWidgetClient_ShowCaret(Client: PGTKAPIWidgetClient); 
begin
  //DebugLn('[GTKAPIWidgetClient_ShowCaret] A Client=',DbgS(Client));
  if Client = nil 
  then begin
    DebugLn('WARNING: [GTKAPIWidgetClient_ShowCaret] Got nil client');
    Exit;
  end;
  
  {$IFDEF VerboseCaret}
  DebugLn('GTKAPIWidgetClient_ShowCaret ',DbgS(Client));
  {$ENDIF}

  Client^.Caret.Visible := True;
  GTKAPIWidgetClient_DrawCaret(Client,false);
end;

procedure GTKAPIWidgetClient_CreateCaret(Client: PGTKAPIWidgetClient;
  AWidth, AHeight: Integer; ABitmap: PGDKPixmap); 
var
  IsVisible: Boolean;
  WasVisible: boolean;
begin
  {$IFDEF VerboseCaret}
  DebugLn(['********** [GTKAPIWidgetClient_CreateCaret] A Client=',DbgS(Client),' Width=',AWidth,' Height=',AHeight,' Bitmap=',ABitmap<>nil]);
  {$ENDIF}
  if Client = nil 
  then begin
    DebugLn('WARNING: [GTKAPIWidgetClient_CreateCaret] Got nil client');
    Exit;
  end;

  with Client^.Caret do
  begin
    IsVisible := Visible;
    if IsVisible then GTKAPIWidgetClient_HideCaret(Client,WasVisible);
    
    if (Width <> AWidth) or (Height <> AHeight) 
    then begin
      if BackPixmap <> nil then gdk_pixmap_unref(BackPixmap);
      BackPixmap := nil;
      Width := AWidth;
      Height := AHeight;
    end;
    
    Pixmap := ABitmap;
    BlinkHide:=false;// start show phase
    Invalidated:=false;

    if IsVisible then GTKAPIWidgetClient_ShowCaret(Client);
  end;
end;

procedure GTKAPIWidgetClient_DestroyCaret(Client: PGTKAPIWidgetClient); 
var
  WasVisible: boolean;
begin
  {$IFDEF VerboseCaret}
  DebugLn('********** [GTKAPIWidgetClient_DestroyCaret] A Client=',DbgS(Client));
  {$ENDIF}
  if Client = nil 
  then begin
    DebugLn('WARNING: [GTKAPIWidgetClient_DestroyCaret] Got nil client');
    Exit;
  end;

  with Client^.Caret do begin
    if Visible then begin
      Visible:=false;
      GTKAPIWidgetClient_HideCaret(Client,WasVisible);
    end;

    if Timer<>0 then begin
      gtk_timeout_remove(Timer);
      Timer:=0;
    end;
    
    if BackPixmap <> nil then begin
      gdk_pixmap_unref(BackPixmap);
      BackPixmap := nil;
    end;
    Pixmap := nil;
  end;
  {$IFDEF VerboseCaret}
  DebugLn('********** B[GTKAPIWidgetClient_DestroyCaret] A Client=',DbgS(Client));
  {$ENDIF}
end;

procedure GTKAPIWidgetClient_InvalidateCaret(Client: PGTKAPIWidgetClient);
begin
  {$IFDEF VerboseCaret}
  DebugLn('********** [GTKAPIWidgetClient_InvalidateCaret] A Client=',DbgS(Client));
  {$ENDIF}
  with Client^.Caret do begin
    if not Invalidated then begin
      {$IFDEF VerboseCaret}
      DebugLn(['GTKAPIWidgetClient_InvalidateCaret invalidate caret: X=',X,' Y=',Y-1,' ',Width,'x',Height]);
      {$ENDIF}
      gtk_widget_queue_draw_area(PGtkWidget(Client),
          X, Y-1, // Y-1 for Delphi compatibility
          Width,Height);
      Invalidated:=true;
    end;
  end;
  {$IFDEF VerboseCaret}
  DebugLn('********** B[GTKAPIWidgetClient_InvalidateCaret] A Client=',DbgS(Client));
  {$ENDIF}
end;

function GTKAPIWidgetClient_IsPainting(Client: PGTKAPIWidgetClient): boolean;
var
  Info: PWidgetInfo;
begin
  Info:=GetWidgetInfo(Client);
  Result:=(Info<>nil) and (Info^.PaintDepth>0);
end;

procedure GTKAPIWidgetClient_SetCaretPos(Client: PGTKAPIWidgetClient;
  AX, AY: Integer);
var
  IsVisible, WasVisible: Boolean;
begin
  {$IFDEF VerboseCaret}
  DebugLn('[GTKAPIWIDGETCLIENT] SetCaretPos '+inttostr(ax)+','+Inttostr(ay));
  {$ENDIF}

  if Client = nil 
  then begin
    DebugLn('WARNING: [GTKAPIWidgetClient_SetCaretPos] Got nil client');
    Exit;
  end;
  
  with Client^.Caret do
  begin
    if (X=AX) and (Y=AY) then exit;
    IsVisible := Visible;
    if IsVisible then GTKAPIWidgetClient_HideCaret(Client,WasVisible);
    X := AX;
    Y := AY;
    BlinkHide:=false;// start show phase
    Invalidated:=false;
    if Timer<>0 then begin
      // reset timer
      gtk_timeout_remove(Timer);
      Timer:=0;
    end;
    if IsVisible then GTKAPIWidgetClient_ShowCaret(Client);
  end;
end;

procedure GTKAPIWidgetClient_GetCaretPos(Client: PGTKAPIWidgetClient;
  var X, Y: Integer); 
begin
  if Client = nil 
  then begin
    DebugLn('WARNING: [GTKAPIWidgetClient_GetCaretPos] Got nil client');
    Exit;
  end;
  
  X := Client^.Caret.X;
  Y := Client^.Caret.Y;
end;

procedure GTKAPIWidgetClient_SetCaretRespondToFocus(Client: PGTKAPIWidgetClient;
  ShowHideOnFocus: boolean);
begin
  {$IFDEF VerboseCaret}
  DebugLn(['[GTKAPIWidgetClient_SetCaretRespondToFocus] A ',ShowHideOnFocus]);
  {$ENDIF}
  if Client = nil
  then begin
    DebugLn(
      'WARNING: [GTKAPIWidgetClient_SetCaretRespondToFocus] Got nil client');
    Exit;
  end;
  
  Client^.Caret.ShowHideOnFocus:=ShowHideOnFocus;
end;

procedure GTKAPIWidgetClient_GetCaretRespondToFocus(Client: PGTKAPIWidgetClient;
  var ShowHideOnFocus: boolean);
begin
  if Client = nil
  then begin
    DebugLn(
      'WARNING: [GTKAPIWidgetClient_GetCaretRespondToFocus] Got nil client');
    Exit;
  end;

  ShowHideOnFocus:=Client^.Caret.ShowHideOnFocus;
end;

//---------------------------------------------------------------------------
// GTKAPIWidget
//---------------------------------------------------------------------------

function GTKAPIWidget_FocusIn(Widget: PGTKWidget;
  {%H-}Event: PGdkEventFocus): GTKEventResult; cdecl;
var
  TopLevel, FocusWidget: PGTKWidget;
begin
  TopLevel := gtk_widget_get_toplevel(Widget);
  if gtk_type_is_a(gtk_object_type(PGTKObject(TopLevel)), gtk_window_get_type) 
  then begin
    if GTK_WIDGET_CAN_FOCUS(PGTKAPIWidget(Widget)^.Client) then
      FocusWidget:=PGTKAPIWidget(Widget)^.Client
    else
      FocusWidget:=Widget;
    {debugln(['GTKAPIWidget_FocusIn ',
      ' Widget=',GetWidgetDebugReport(Widget),
      ' Client=',GetWidgetDebugReport(PGTKAPIWidget(Widget)^.Client),
      ' GTK_WIDGET_CAN_FOCUS(Widget)=',GTK_WIDGET_CAN_FOCUS(Widget),
      ' GTK_WIDGET_CAN_FOCUS(Client)=',GTK_WIDGET_CAN_FOCUS(PGTKAPIWidget(Widget)^.Client),
      '']);}
    gtk_window_set_focus(PGTKWindow(TopLevel), FocusWidget);
  end;

  Result := gtk_True;
end;

function GTKAPIWidget_FocusOut({%H-}Widget: PGTKWidget;
  {%H-}Event: PGdkEventFocus): GTKEventResult; cdecl;
begin
  Result := gtk_True;
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

procedure GTKAPIWidget_Init(waw:PGTypeInstance; theClass: Pointer); cdecl;
// waw: PGTKAPIWidget;
// theClass: PGTKAPIWidgetClass
var
  Widget: PGTKWidget;
begin
  if theClass=nil then ;
  Widget := PGTKWidget(waw);
  gtk_widget_set_flags(Widget, GTK_CAN_FOCUS);
end;

function GTKAPIWidget_GetType: GType;
const 
  WAW_NAME = 'LCLWinapiWidget';
  wawInfo: TGTKTypeInfo = (
    type_name: WAW_NAME;
    object_size: SizeOf(TGTKAPIWidget)+100; // a TGTKScrolledWindow
    class_size: SizeOf(TGTKAPIWidgetClass)+100;
    class_init_func: @GTKAPIWidget_ClassInit;
    object_init_func : @GTKAPIWidget_Init;
    reserved_1: nil;
    reserved_2: nil;
    base_class_init_func: nil;
  );
begin
  if (GTKAPIWidget_Type = 0)
  then begin
    GTKAPIWidget_Type := gtk_type_from_name(WAW_NAME);
    if GTKAPIWidget_Type = 0
    then GTKAPIWidget_Type := gtk_type_unique(gtk_scrolled_window_get_type, @wawInfo);
  end;
  Result := GTKAPIWidget_Type;
end;

function GTKAPIWidget_new: PGTKWidget;
var
  APIWidget: PGTKAPIWidget;
begin
  Result := gtk_widget_new(GTKAPIWidget_GetType, nil, []);

  APIWidget := PGTKAPIWidget(Result);
  gtk_container_set_border_width(PGTKContainer(APIWidget),0);

  // create client widget
  APIWidget^.Client := GTKAPIWidgetClient_New;
  g_object_set_data(PGObject(Result), 'Fixed', APIWidget^.Client);
  g_object_set_data(PGObject(APIWidget^.Client), 'Main', Result);
  gtk_widget_show(APIWidget^.Client);
  gtk_container_add(PGTKContainer(APIWidget), APIWidget^.Client);
end;

procedure GTKAPIWidget_CreateCaret(APIWidget: PGTKAPIWidget;
  AWidth, AHeight: Integer; ABitmap: PGDKPixmap); 
begin
  if APIWidget = nil 
  then begin
    DebugLn('WARNING: [GTKAPIWidget_CreateCaret] Got nil client');
    Exit;
  end;
  GTKAPIWidgetClient_CreateCaret(PGTKAPIWidgetClient(APIWidget^.Client),
    AWidth, AHeight, ABitmap);
end;

procedure GTKAPIWidget_DestroyCaret(APIWidget: PGTKAPIWidget); 
begin
  if APIWidget = nil 
  then begin
    DebugLn('WARNING: [GTKAPIWidget_DestroyCaret] Got nil client');
    Exit;
  end;
  GTKAPIWidgetClient_DestroyCaret(PGTKAPIWidgetClient(APIWidget^.Client));
end;

procedure GTKAPIWidget_InvalidateCaret(APIWidget: PGTKAPIWidget);
begin
  {$IFDEF VerboseCaret}
  DebugLn('[GTKAPIWidget_InvalidateCaret] A');
  {$ENDIF}
  if APIWidget = nil
  then begin
    DebugLn('WARNING: [GTKAPIWidget_InvalidateCaret] Got nil client');
    Exit;
  end;
  GTKAPIWidgetClient_InvalidateCaret(PGTKAPIWidgetClient(APIWidget^.Client));
end;

procedure GTKAPIWidget_HideCaret(APIWidget: PGTKAPIWidget;
  var OldVisible: boolean);
begin
  {$IFDEF VerboseCaret}
  DebugLn('[GTKAPIWidget_HideCaret] A');
  {$ENDIF}
  if APIWidget = nil 
  then begin
    DebugLn('WARNING: [GTKAPIWidget_HideCaret] Got nil client');
    Exit;
  end;
  GTKAPIWidgetClient_HideCaret(PGTKAPIWidgetClient(APIWidget^.Client),OldVisible);
end;

procedure GTKAPIWidget_ShowCaret(APIWidget: PGTKAPIWidget); 
begin
  {$IFDEF VerboseCaret}
  DebugLn('[GTKAPIWidget_ShowCaret] A');
  {$ENDIF}
  if APIWidget = nil
  then begin
    DebugLn('WARNING: [GTKAPIWidget_ShowCaret] Got nil client');
    Exit;
  end;
  GTKAPIWidgetClient_ShowCaret(PGTKAPIWidgetClient(APIWidget^.Client));
end;

procedure GTKAPIWidget_SetCaretPos(APIWidget: PGTKAPIWidget; X, Y: Integer);
begin
  {$IFDEF VerboseCaret}
  DebugLn('[GTKAPIWidget_SetCaretPos] A');
  {$ENDIF}
  if APIWidget = nil
  then begin
    DebugLn('WARNING: [GTKAPIWidget_SetCaretPos] Got nil client');
    Exit;
  end;
  GTKAPIWidgetClient_SetCaretPos(PGTKAPIWidgetClient(APIWidget^.Client), X, Y);
end;

procedure GTKAPIWidget_GetCaretPos(APIWidget: PGTKAPIWidget; var X, Y: Integer); 
begin
  if APIWidget = nil 
  then begin
    DebugLn('WARNING: [GTKAPIWidget_GetCaretPos] Got nil client');
    Exit;
  end;
  GTKAPIWidgetClient_GetCaretPos(PGTKAPIWidgetClient(APIWidget^.Client), X, Y);
end;

procedure GTKAPIWidget_SetCaretRespondToFocus(APIWidget: PGTKAPIWidget;
  ShowHideOnFocus: boolean); 
begin
  {$IFDEF VerboseCaret}
  DebugLn(['[GTKAPIWidget_SetCaretRespondToFocus] A ',ShowHideOnFocus]);
  {$ENDIF}
  if APIWidget = nil
  then begin
    DebugLn('WARNING: [GTKAPIWidget_SetCaretRespondToFocus] Got nil client');
    Exit;
  end;
  GTKAPIWidgetClient_SetCaretRespondToFocus(
    PGTKAPIWidgetClient(APIWidget^.Client), ShowHideOnFocus);
end;

procedure GTKAPIWidget_GetCaretRespondToFocus(APIWidget: PGTKAPIWidget;
  var ShowHideOnFocus: boolean);
begin
  if APIWidget = nil
  then begin
    DebugLn('WARNING: [GTKAPIWidget_GetCaretRespondToFocus] Got nil client');
    Exit;
  end;
  GTKAPIWidgetClient_GetCaretRespondToFocus(
    PGTKAPIWidgetClient(APIWidget^.Client), ShowHideOnFocus);
end;

initialization
  MParentClass := nil;

end.


