{  $Id$  }
{
 /***************************************************************************
                             GTKWinapiWindow.pp
                             -------------------
                       gtkimplementation for basic window
                   Initial Revision  : Sun Jan 9 16:00:00 GMT+1 2000


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
{
@abstract(A GTK widget to support controls derived from a wincontrol)
@author(TGTKWinapiWindow - Marc Weustink <weus@quicknet.nl>)
@created(2000)
@lastmod(2000)
}
unit GTKWinapiWindow;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF gtk2}
  glib2, gdk2pixbuf, gdk2, gtk2,
  {$ELSE}
  glib, gdk, gtk, {$Ifndef NoGdkPixbufLib}gdkpixbuf,{$EndIf}
  {$ENDIF}
  SysUtils;

{ $Define VerboseCaret}

type
  PGTKAPIWidget = ^TGTKAPIWidget;
  TGTKAPIWidget = record
    // ! the ScrolledWindow must be the first attribute of this record !
    ScrolledWindow: TGTKScrolledWindow;
    Client: PGtkWidget;
  end;
  
  PGTKAPIWidgetClass = ^TGTKAPIWidgetClass;
  TGTKAPIWidgetClass = record
    parent_class: TGTKScrolledWindowClass;
  end;

function GTKAPIWidget_GetType: guint;
function GTKAPIWidget_New: PGTKWidget;
procedure GTKAPIWidget_CreateCaret(APIWidget: PGTKAPIWidget;
                                 AWidth, AHeight: Integer; ABitmap: PGDKPixmap); 
procedure GTKAPIWidget_DestroyCaret(APIWidget: PGTKAPIWidget); 
procedure GTKAPIWidget_HideCaret(APIWidget: PGTKAPIWidget); 
procedure GTKAPIWidget_ShowCaret(APIWidget: PGTKAPIWidget); 
procedure GTKAPIWidget_SetCaretPos(APIWidget: PGTKAPIWidget; X, Y: Integer); 
procedure GTKAPIWidget_GetCaretPos(APIWidget: PGTKAPIWidget; var X, Y: Integer); 
procedure GTKAPIWidget_SetCaretRespondToFocus(APIWidget: PGTKAPIWidget;
  ShowHideOnFocus: boolean); 
procedure GTKAPIWidget_GetCaretRespondToFocus(APIWidget: PGTKAPIWidget;
  var ShowHideOnFocus: boolean);

implementation

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
    ShowHideOnFocus: boolean; // true = hide on loose focus, show on get focus
  end;

  PGTKAPIWidgetClient = ^TGTKAPIWidgetClient;
  TGTKAPIWidgetClient = record
    // ! the Widget must be the first attribute of the record !
    Widget: TGtkFixed;
    Caret: TCaretInfo;
  end;
  
  PGTKAPIWidgetClientClass = ^TGTKAPIWidgetClientClass;
  TGTKAPIWidgetClientClass = record
    ParentClass: TGTKWidgetClass;
    set_scroll_adjustments: procedure(Widget: PGTKWidget;
                               HAdjustment, VAdjustment: PGTKAdjustment); cdecl;
  end;

function GTKAPIWidgetClient_Timer(Client: Pointer): gint; cdecl; forward;
procedure GTKAPIWidgetClient_Realize(Widget: PGTKWidget); cdecl; forward;
procedure GTKAPIWidgetClient_UnRealize(Widget: PGTKWidget); cdecl; forward;
function GTKAPIWidgetClient_KeyPress(Widget: PGTKWidget;
  Event: PGDKEventKey): gint; cdecl; forward;
function GTKAPIWidgetClient_ButtonPress(Widget: PGTKWidget;
  Event: PGDKEventButton): gint; cdecl; forward;
function GTKAPIWidgetClient_FocusIn(Widget: PGTKWidget;
  Event: PGdkEventFocus): gint; cdecl; forward;
function GTKAPIWidgetClient_FocusOut(Widget: PGTKWidget;
  Event: PGdkEventFocus): gint; cdecl; forward;
  
procedure GTKAPIWidgetClient_ClassInit(theClass: Pointer);cdecl; forward;
procedure GTKAPIWidgetClient_Init(Client, theClass: Pointer); cdecl; forward;
function GTKAPIWidgetClient_GetType: Guint; forward;
function GTKAPIWidgetClient_New: PGTKWidget; forward;

procedure GTKAPIWidgetClient_HideCaret(Client: PGTKAPIWidgetClient); forward;
procedure GTKAPIWidgetClient_DrawCaret(Client: PGTKAPIWidgetClient); forward;
procedure GTKAPIWidgetClient_ShowCaret(Client: PGTKAPIWidgetClient); forward;
procedure GTKAPIWidgetClient_CreateCaret(Client: PGTKAPIWidgetClient;
  AWidth, AHeight: Integer; ABitmap: PGDKPixmap); forward;
procedure GTKAPIWidgetClient_DestroyCaret(Client: PGTKAPIWidgetClient); forward;
procedure GTKAPIWidgetClient_SetCaretPos(Client: PGTKAPIWidgetClient;
  AX, AY: Integer); forward;
procedure GTKAPIWidgetClient_GetCaretPos(Client: PGTKAPIWidgetClient;
  var X, Y: Integer); forward;
procedure GTKAPIWidgetClient_SetCaretRespondToFocus(Client: PGTKAPIWidgetClient;
  ShowHideOnFocus: boolean); forward;
procedure GTKAPIWidgetClient_GetCaretRespondToFocus(Client: PGTKAPIWidgetClient;
  var ShowHideOnFocus: boolean); forward;


function GTKAPIWidgetClient_Timer(Client: Pointer): gint; cdecl;
// returning 0 would stop the timer, 1 will restart it
begin
  if PGTKAPIWidgetClient(Client)^.Caret.Timer<=0 then begin
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
begin
//  Assert(False, 'Trace:[GTKAPIWidgetClient_Realize]');
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

  gdk_window_set_user_data(Widget^.Window, Widget);

  with Attributes do
  begin
    X := PGTKStyle(Widget^.theStyle)^.Klass^.XThickness;
    Y := PGTKStyle(Widget^.theStyle)^.Klass^.YThickness;
    Width := Widget^.Allocation.Width - Attributes.X * 2;
    Height := Widget^.Allocation.Height - Attributes.Y * 2;
//    Cursor := gdk_cursor_new(GDK_XTERM);
  end;
//  AttributesMask := AttributesMask or GDK_WA_CURSOR;

  Widget^.theStyle := gtk_style_attach(Widget^.theStyle, Widget^.Window);

  gtk_style_set_background (Widget^.theStyle, Widget^.Window, GTK_STATE_NORMAL);
//  gdk_window_set_background(Widget^.Window, @PGTKStyle(Widget^.theStyle)^.Base[gtk_widget_state(Widget)]);
//  gdk_window_set_background (Client^.OtherWindow, @PGTKStyle(Widget^.theStyle)^.Base[gtk_widget_state(Widget)]);
  gdk_window_set_back_pixmap(Widget^.Window,nil,0);
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
  // supress further processing
  Result := gtk_True;
end;

function GTKAPIWidgetClient_ButtonPress(Widget: PGTKWidget;
  Event: PGDKEventButton): gint; cdecl;
begin
  {$IFDEF VerboseFocus}
  writeln('GTKAPIWidgetClient_ButtonPress ',HexStr(Cardinal(Widget),8));
  {$ENDIF}
  if not gtk_widget_has_focus(Widget) then
    gtk_widget_grab_focus(Widget);
  
  Result := gtk_False;
end;

function GTKAPIWidgetClient_FocusIn(Widget: PGTKWidget;
  Event: PGdkEventFocus): gint; cdecl;
begin
  {$IFDEF VerboseFocus}
  writeln('GTKAPIWidgetClient_FocusIn ',HexStr(Cardinal(Widget),8),' ',event^.thein);
  {$ENDIF}
  gtk_widget_set_flags(Widget, GTK_HAS_FOCUS);
  GTKAPIWidgetClient_DrawCaret(PGTKAPIWidgetClient(Widget));
  Result := gtk_False;
end;

function GTKAPIWidgetClient_FocusOut(Widget: PGTKWidget;
  Event: PGdkEventFocus): gint; cdecl;
begin
  {$IFDEF VerboseFocus}
  writeln('GTKAPIWidgetClient_FocusOut ',HexStr(Cardinal(Widget),8),' ',event^.thein);
  {$ENDIF}
  gtk_widget_unset_flags(Widget, GTK_HAS_FOCUS);
  GTKAPIWidgetClient_DrawCaret(PGTKAPIWidgetClient(Widget));
  Result := gtk_False;
end;

procedure GTKAPIWidgetClient_ClassInit(theClass: Pointer);cdecl;
// theClass: PGTKAPIWidgetClientClass
{$IFDEF VER1_1}
type
  TAdjustParams = packed record
    Param1, Param2: TGtkType;
  end;
{$ENDIF}
var 
  ObjectClass: PGTKObjectClass;
  WidgetClass: PGTKWidgetClass;
  ClientClass: PGTKAPIWidgetClientClass;
  SignalID: Guint;
{$IFDEF VER1_1}
  AdjustParams: TAdjustParams;
{$ENDIF}
begin
  ObjectClass := PGTKObjectClass(theClass);
  WidgetClass := PGTKWidgetClass(theClass);
  ClientClass := PGTKAPIWidgetClientClass(theClass);
  
  {$IFDEF VER1_1}
  AdjustParams.Param1 := gtk_adjustment_get_type;
  AdjustParams.Param2 := AdjustParams.Param1;
  SignalID := gtk_signal_newv(
  {$ELSE}
  SignalID := gtk_signal_new(
  {$ENDIF}
    'set_scroll_adjustments',
    GTK_RUN_FIRST,
    ObjectClass^.thetype,
    (@ClientClass^.set_scroll_adjustments - Pointer(theClass)),
    @gtk_marshal_NONE__POINTER_POINTER,
    GTK_TYPE_NONE,
    2, 
    {$IFDEF VER1_1}
    @AdjustParams
    {$ELSE}
    [gtk_adjustment_get_type, gtk_adjustment_get_type]
    {$ENDIF}
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

  ClientClass^.set_scroll_adjustments := nil;
end;

procedure GTKAPIWidgetClient_Init(Client, theClass: Pointer); cdecl;
// Client: PGTKAPIWidgetClient
// theClass: PGTKAPIWidgetClientClass
begin
  gtk_widget_set_flags(PGTKWidget(Client), GTK_CAN_FOCUS);
  gtk_widget_set_flags(PGTKWidget(Client), GTK_CAN_DEFAULT);

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
    ShowHideOnFocus := true;
  end;

  gtk_widget_set_app_paintable(PGTKWidget(Client),true);
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
    reserved_1: nil;
    reserved_2: nil;
    base_class_init_func: nil;
  );
begin
  if (TheType = 0) then
    TheType := gtk_type_unique(gtk_fixed_type,@Info);
  Result := TheType;
end;

function GTKAPIWidgetClient_New: PGTKWidget;
begin
  Result := PGTKWidget(gtk_type_new(GTKAPIWidgetClient_GetType()));
end;

procedure GTKAPIWidgetClient_HideCaret(Client: PGTKAPIWidgetClient); 
begin
  //writeln('[GTKAPIWidgetClient_HideCaret] A Client=',HexStr(Cardinal(Client),8));
  if Client = nil
  then begin
    WriteLn('WARNING: [GTKAPIWidgetClient_HideCaret] Got nil client');
    Exit;
  end;
  {$IFDEF VerboseCaret}
  writeln('GTKAPIWidgetClient_HideCaret ',HexStr(Cardinal(Client),8),' ShowHideOnFocus=',Client^.Caret.ShowHideOnFocus);
  {$ENDIF}
  Client^.Caret.Visible := False;
  GTKAPIWidgetClient_DrawCaret(Client);
end;

procedure GTKAPIWidgetClient_DrawCaret(Client: PGTKAPIWidgetClient); 
const
  GC_STATE: array[Boolean] of TGtkStateType =
                                 (GTK_STATE_INSENSITIVE, GTK_STATE_NORMAL);
var
  Widget: PGTKWidget;
  HasFocus: boolean;
  ForeGroundGC: PGdkGC;
  //OldGdkFunction: TGdkFunction;
  //ForeGroundGCValues: TGdkGCValues;
begin
  if Client = nil then begin
    WriteLn('WARNING: [GTKAPIWidgetClient_DrawCaret] Got nil client');
    Exit;
  end;
  Widget := PGTKWidget(Client);
  
  with Client^.Caret do begin
    HasFocus:=gtk_widget_has_focus(Widget);
  
    if (Timer <> 0) and
      ((not Blinking) or (not Visible)
      or (ShowHideOnFocus and (not HasFocus))) 
    then begin
      gtk_timeout_remove(Timer);
      Timer := 0;
    end;
    if IsDrawn and ((not Visible) or Blinking) then begin
      {$IFDEF VerboseCaret}
      writeln('GTKAPIWidgetClient_DrawCaret ',HexStr(Cardinal(Client),8),
        ' Hiding Caret IsDrawn=',IsDrawn,' Visible=',Visible,' Blinking=',Blinking);
      {$ENDIF}
      // hide caret
      if (BackPixmap <> nil) and (Widget<>nil) and (Widget^.theStyle<>nil)
      then gdk_draw_pixmap(
        Widget^.Window, 
        PGTKStyle(Widget^.theStyle)^.bg_gc[GTK_STATE_NORMAL], 
        BackPixmap, 0, 0,
        X, Y-1, // Y-1 for Delphi compatibility
        Width, Height
      );
      IsDrawn := False;
    end
    else
    if Visible
    and (gtk_widget_has_focus(Widget) or not ShowHideOnFocus)
    and (not IsDrawn) and (Widget^.Window<>nil) and (Widget^.theStyle<>nil)
    then begin
      if Pixmap <> nil then
        Assert(False, 'Trace:TODO: [GTKAPIWidgetClient_DrawCaret] Implement bitmap');
      
      //Create backbitmap if needed
      if (BackPixmap = nil) and (Widget^.Window<>nil) and (Width>0)
      and (Height>0)
      then
        BackPixmap := gdk_pixmap_new(Widget^.Window, Width, Height, -1);

      // undraw old caret
      if (BackPixmap <> nil) and (Widget<>nil) and (Widget^.theStyle<>nil)
      and (Width>0) and (Height>0)
      then gdk_draw_pixmap(
        BackPixmap, 
        PGTKStyle(Widget^.theStyle)^.bg_gc[GTK_STATE_NORMAL], 
        Widget^.Window,
          X, Y-1, // Y-1 for Delphi compatibility
          0, 0,
          Width, Height
      );

      // draw caret
      {$IFDEF VerboseCaret}
      writeln('GTKAPIWidgetClient_DrawCaret B Client=',HexStr(Cardinal(Client),8)
      ,' ',cardinal(PGTKWidget(Client)^.theStyle)
      ,' ',cardinal(PGTKWidget(Client)^.Window)
      ,' ',Width
      ,' ',Height
      );
      {$ENDIF}
      if (PGTKWidget(Client)^.theStyle<>nil) 
      and (PGTKWidget(Client)^.Window<>nil)
      and (Width>0) and (Height>0) then begin
        // set draw function to xor
        ForeGroundGC:=PGTKStyle(
           PGTKWidget(Client)^.theStyle)^.fg_gc[GC_STATE[Integer(Pixmap) <> 1]];
        //gdk_gc_get_values(ForeGroundGC,@ForeGroundGCValues);
        //OldGdkFunction:=ForeGroundGCValues.thefunction;
        {$IFDEF VerboseCaret}
        writeln('GTKAPIWidgetClient_DrawCaret ',HexStr(Cardinal(Client),8),' Real Drawing Caret ');
        {$ENDIF}
        gdk_gc_set_function(ForeGroundGC,GDK_invert);
        try
          // draw the caret
          //writeln('DRAWING');
          gdk_draw_rectangle(
            PGTKWidget(Client)^.Window,
            ForeGroundGC,
            1,
            X, Y-1,  // Y-1 for Delphi compatibility
            Width, Height
          );
        finally
          // restore draw function
          gdk_gc_set_function(ForeGroundGC,GDK_COPY);
        end;
      end else
        writeln('***: Draw Caret failed: Client=',HexStr(Cardinal(Client),8),' X=',X,' Y=',Y,' W=',Width,' H=',Height,' ',Pixmap<>nil,',',PGTKWidget(Client)^.Window<>nil,',',PGTKWidget(Client)^.theStyle<>nil);
      IsDrawn := True;
    end;
    
    //writeln('GTKAPIWidgetClient_DrawCaret A Client=',HexStr(Cardinal(Client),8),' Timer=',Timer,' Blink=',Blinking,' Visible=',Visible,' ShowHideOnFocus=',ShowHideOnFocus,' Focus=',gtk_widget_has_focus(Widget),' IsDrawn=',IsDrawn,' W=',Width,' H=',Height);
    if Visible and Blinking and (Timer = 0) 
    and (not ShowHideOnFocus or HasFocus)
    then
      Timer := gtk_timeout_add(500, @GTKAPIWidgetClient_Timer, Client);
  end;
end;

procedure GTKAPIWidgetClient_ShowCaret(Client: PGTKAPIWidgetClient); 
begin
  //writeln('[GTKAPIWidgetClient_ShowCaret] A Client=',HexStr(Cardinal(Client),8));
  if Client = nil 
  then begin
    WriteLn('WARNING: [GTKAPIWidgetClient_ShowCaret] Got nil client');
    Exit;
  end;
  
  {$IFDEF VerboseCaret}
  writeln('GTKAPIWidgetClient_ShowCaret ',HexStr(Cardinal(Client),8));
  {$ENDIF}
  Client^.Caret.Visible := True;
  GTKAPIWidgetClient_DrawCaret(Client);
end;

procedure GTKAPIWidgetClient_CreateCaret(Client: PGTKAPIWidgetClient;
  AWidth, AHeight: Integer; ABitmap: PGDKPixmap); 
var
  IsVisible: Boolean;
begin
  {$IFDEF VerboseCaret}
  writeln('********** [GTKAPIWidgetClient_CreateCaret] A Client=',HexStr(Cardinal(Client),8),' Width=',AWidth,' Height=',AHeight,' Bitmap=',ABitmap<>nil);
  {$ENDIF}
  if Client = nil 
  then begin
    WriteLn('WARNING: [GTKAPIWidgetClient_CreateCaret] Got nil client');
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

procedure GTKAPIWidgetClient_DestroyCaret(Client: PGTKAPIWidgetClient); 
begin
  {$IFDEF VerboseCaret}
  writeln('********** [GTKAPIWidgetClient_DestroyCaret] A Client=',HexStr(Cardinal(Client),8));
  {$ENDIF}
  if Client = nil 
  then begin
    WriteLn('WARNING: [GTKAPIWidgetClient_DestroyCaret] Got nil client');
    Exit;
  end;

  with Client^.Caret do begin
    if Visible then GTKAPIWidgetClient_HideCaret(Client);
    
    if BackPixmap <> nil then begin
      gdk_pixmap_unref(BackPixmap);
      BackPixmap := nil;
    end;
    Pixmap := nil;
  end;
  {$IFDEF VerboseCaret}
  writeln('********** B[GTKAPIWidgetClient_DestroyCaret] A Client=',HexStr(Cardinal(Client),8));
  {$ENDIF}
end;

procedure GTKAPIWidgetClient_SetCaretPos(Client: PGTKAPIWidgetClient;
  AX, AY: Integer);
var
  IsVisible: Boolean;
begin
  {$IFDEF VerboseCaret}
  Writeln('[GTKAPIWIDGETCLIENT] SetCaretPos '+inttostr(ax)+','+Inttostr(ay));
  {$ENDIF}

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

procedure GTKAPIWidgetClient_SetCaretRespondToFocus(Client: PGTKAPIWidgetClient;
  ShowHideOnFocus: boolean);
begin
  {$IFDEF VerboseCaret}
  writeln('[GTKAPIWidgetClient_SetCaretRespondToFocus] A ',ShowHideOnFocus);
  {$ENDIF}
  if Client = nil
  then begin
    WriteLn(
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
    WriteLn(
      'WARNING: [GTKAPIWidgetClient_GetCaretRespondToFocus] Got nil client');
    Exit;
  end;

  ShowHideOnFocus:=Client^.Caret.ShowHideOnFocus;
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
begin
  Widget := PGTKWidget(waw);
  gtk_widget_set_flags(Widget, GTK_CAN_FOCUS);
end;

function GTKAPIWidget_GetType: Guint;
const 
  wawType: Guint = 0;
  wawInfo: TGTKTypeInfo = (
    type_name: 'LCLWinapiWidget';
    object_size: SizeOf(TGTKAPIWidget)+100; // a TGTKScrolledWindow
    class_size: SizeOf(TGTKAPIWidgetClass)+100;
    class_init_func: @GTKAPIWidget_ClassInit;
    object_init_func : @GTKAPIWidget_Init;
    reserved_1: nil;
    reserved_2: nil;
    base_class_init_func: nil;
  );
begin
  if (wawType = 0) 
  then wawType := gtk_type_unique(gtk_scrolled_window_get_type, @wawInfo);
  Result := wawType;
end;

function GTKAPIWidget_new: PGTKWidget;
var
  APIWidget: PGTKAPIWidget;
const
  Args : array[0..1] of TGTKArg =
    ((
      thetype : GTK_TYPE_OBJECT;
      name : 'hadjustment';
      d:(object_data : nil)
    ),
    (
      thetype : GTK_TYPE_OBJECT;
      name : 'vadjustment';
      d:(object_data : nil)
    ));
begin
  Args[0].thetype := GTK_ADJUSTMENT_TYPE;
  Args[1].thetype := GTK_ADJUSTMENT_TYPE;

  Result := gtk_widget_newv(GTKAPIWidget_GetType, 2, @Args[0]);

  // create client widget
  APIWidget := PGTKAPIWidget(Result);
  APIWidget^.Client := GTKAPIWidgetClient_New;
  gtk_object_set_data(PGTKObject(Result), 'Fixed', APIWidget^.Client);
  gtk_object_set_data(PGTKObject(APIWidget^.Client), 'Main', Result);
  gtk_widget_show(APIWidget^.Client);
  
  gtk_container_add(PGTKContainer(Result), APIWidget^.Client);
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

procedure GTKAPIWidget_DestroyCaret(APIWidget: PGTKAPIWidget); 
begin
  if APIWidget = nil 
  then begin
    WriteLn('WARNING: [GTKAPIWidget_DestroyCaret] Got nil client');
    Exit;
  end;
  GTKAPIWidgetClient_DestroyCaret(PGTKAPIWidgetClient(APIWidget^.Client));
end;

procedure GTKAPIWidget_HideCaret(APIWidget: PGTKAPIWidget); 
begin
  {$IFDEF VerboseCaret}
  writeln('[GTKAPIWidget_HideCaret] A');
  {$ENDIF}
  if APIWidget = nil 
  then begin
    WriteLn('WARNING: [GTKAPIWidget_HideCaret] Got nil client');
    Exit;
  end;
  GTKAPIWidgetClient_HideCaret(PGTKAPIWidgetClient(APIWidget^.Client));
end;

procedure GTKAPIWidget_ShowCaret(APIWidget: PGTKAPIWidget); 
begin
  {$IFDEF VerboseCaret}
  writeln('[GTKAPIWidget_ShowCaret] A');
  {$ENDIF}
  if APIWidget = nil
  then begin
    WriteLn('WARNING: [GTKAPIWidget_ShowCaret] Got nil client');
    Exit;
  end;
  GTKAPIWidgetClient_ShowCaret(PGTKAPIWidgetClient(APIWidget^.Client));
end;

procedure GTKAPIWidget_SetCaretPos(APIWidget: PGTKAPIWidget; X, Y: Integer);
begin
  {$IFDEF VerboseCaret}
  writeln('[GTKAPIWidget_SetCaretPos] A');
  {$ENDIF}
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

procedure GTKAPIWidget_SetCaretRespondToFocus(APIWidget: PGTKAPIWidget;
  ShowHideOnFocus: boolean); 
begin
  {$IFDEF VerboseCaret}
  writeln('[GTKAPIWidget_SetCaretRespondToFocus] A ',ShowHideOnFocus);
  {$ENDIF}
  if APIWidget = nil
  then begin
    WriteLn('WARNING: [GTKAPIWidget_SetCaretRespondToFocus] Got nil client');
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
    WriteLn('WARNING: [GTKAPIWidget_GetCaretRespondToFocus] Got nil client');
    Exit;
  end;
  GTKAPIWidgetClient_GetCaretRespondToFocus(
    PGTKAPIWidgetClient(APIWidget^.Client), ShowHideOnFocus);
end;


end.

{ =============================================================================

  $Log$
  Revision 1.43  2002/12/30 17:24:08  mattias
  added history to identifier completion

  Revision 1.42  2002/12/23 10:28:02  mattias
  fixed setting background

  Revision 1.41  2002/12/23 10:23:45  mattias
  fix for fpc 1.0.7

  Revision 1.40  2002/12/23 10:12:40  mattias
  added symlink test and unit types

  Revision 1.39  2002/12/22 22:42:55  mattias
  custom controls now support child wincontrols

  Revision 1.38  2002/12/15 11:52:28  mattias
  started gtk2 interface

  Revision 1.37  2002/02/09 02:13:38  mattias
  undid the TWinControls can now contain childs in gtk, due to slowness

  Revision 1.36  2002/02/09 01:48:23  mattias
  renamed TinterfaceObject.Init to AppInit and TWinControls can now contain childs in gtk

  Revision 1.35  2002/10/21 22:12:49  lazarus
  MG: fixed frmactivate

  Revision 1.34  2002/10/21 00:17:33  lazarus
  AJ:fix for FPC 1.0

  Revision 1.33  2002/10/20 22:57:18  lazarus
  AJ:switched to gtk_widget_newv to work around array of const

  Revision 1.32  2002/06/12 12:35:45  lazarus
  MG: fixed apiwidget warnings/criticals

  Revision 1.31  2002/06/11 13:41:11  lazarus
  MG: fixed mouse coords and fixed mouse clicked thru bug

  Revision 1.30  2002/06/09 14:00:42  lazarus
  MG: fixed persistent caret and implemented Form.BorderStyle=bsNone

  Revision 1.29  2002/06/06 14:41:29  lazarus
  MG: if completion form visible it will now get all synedit keys

  Revision 1.28  2002/06/05 17:02:31  lazarus
  MG: fixed finddeclaration Result in with statement

  Revision 1.27  2002/06/05 12:34:00  lazarus
  MG: fixed fonts in XLFD format and styles

  Revision 1.26  2002/06/04 19:28:18  lazarus
  MG: cursor is now inverted and can be used with twilight color scheme

  Revision 1.25  2002/05/10 06:05:58  lazarus
  MG: changed license to LGPL

  Revision 1.24  2002/04/27 15:35:51  lazarus
  MG: fixed window shrinking

  Revision 1.23  2002/03/15 13:15:24  lazarus
  Removed FOCUSIN messages
  Removed Bitbtn created message
  Shane

  Revision 1.22  2002/03/14 20:28:50  lazarus
  Bug fix for Mattias.
  Fixed spinedit so you can now get the value and set the value.
  Shane

  Revision 1.21  2002/01/27 21:08:40  lazarus
  MWE: Removed ^M

  Revision 1.20  2002/01/21 14:17:47  lazarus
  MG: added find-block-start and renamed find-block-other-end

  Revision 1.19  2001/12/17 12:14:40  lazarus
  MG: tried to xor caret, but failed :(

  Revision 1.18  2001/12/12 14:39:26  lazarus
  MG: carets will now be auto destroyed on widget destroy

  Revision 1.17  2001/12/12 14:23:19  lazarus
  MG: implemented DestroyCaret

  Revision 1.16  2001/11/13 18:50:10  lazarus
  Changes to facilitate the toggle between form and unit
  Shane

  Revision 1.15  2001/10/25 13:21:06  lazarus
  Added an IFDEF for VER1_1
  Shane

  Revision 1.14  2001/10/24 09:28:03  lazarus
  MG: workaround for fpc1.1 in GTKAPIWidgetClient_ClassInit

  Revision 1.13  2001/10/24 00:35:55  lazarus
  MG: fixes for fpc 1.1: range check errors

  Revision 1.12  2001/10/10 17:55:06  lazarus
  MG: fixed caret lost, gtk cleanup, bracket lvls, bookmark saving

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

