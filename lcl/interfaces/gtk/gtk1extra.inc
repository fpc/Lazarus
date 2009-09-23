{%MainUnit gtkproc.pp}{%MainUnit gtkint.pp}
{ $Id$ }
{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

function G_OBJECT(p: Pointer): PGtkObject;
begin
  Result:=PGtkObject(p);
end;

function G_CALLBACK(p: Pointer): TGTKSignalFunc;
begin
  Result:=TGTKSignalFunc(p);
end;

function GDK_GET_CURRENT_DESKTOP(): gint;
var
  XDisplay: PDisplay;
  XScreen: PScreen;
  XWindow: TWindow;
  AtomType: x.TAtom;
  Format: gint;
  nitems: gulong;
  bytes_after: gulong;
  current_desktop: pguint;
begin
  Result := -1;

  xdisplay := gdk_display;
  xscreen := XDefaultScreenOfDisplay(xdisplay);
  xwindow := XRootWindowOfScreen(xscreen);

  XGetWindowProperty (xdisplay, xwindow,
             XInternAtom(xdisplay, '_NET_CURRENT_DESKTOP', false),
             0, MaxInt, False, XA_CARDINAL, @atomtype, @format, @nitems,
             @bytes_after, gpointer(@current_desktop));

  if (atomtype = XA_CARDINAL) and (format = 32) and  (nitems > 0) then
    Result := current_desktop[0];
  if current_desktop <> nil then
    XFree (current_desktop);
end;


function GDK_WINDOW_GET_DESKTOP(Window: PGdkWindowPrivate): gint;
var
  xdisplay: PDisplay;
  xwindow: TWindow;

  atomtype: x.TAtom;
  format: gint;
  nitems: gulong;
  bytes_after: gulong;
  current_desktop: pguint;
begin
  Result := -1;
  XWindow := GDK_WINDOW_XWINDOW (Window);
  XDisplay := GDK_WINDOW_XDISPLAY (Window);
  XGetWindowProperty (xdisplay, xwindow,
             XInternAtom(xdisplay, '_NET_WM_DESKTOP', false),
             0, MaxInt, False, XA_CARDINAL, @atomtype, @format, @nitems,
             @bytes_after, gpointer(@current_desktop));

  if (atomtype = XA_CARDINAL) and (format = 32) and  (nitems > 0) then
    Result := current_desktop[0];
  if current_desktop <> nil then
    XFree (current_desktop);
end;

function GDK_WINDOW_SET_DESKTOP(Window: PGdkWindowPrivate; Desktop: gint): gint;
var
  XDisplay: PDisplay;
  XScreen: PScreen;
  XRootWindow,
  XWindow: TWindow;
  XEvent: TXClientMessageEvent;
  _NET_WM_DESKTOP: Integer;
begin

  Result := -1;

  XDisplay := GDK_WINDOW_XDISPLAY (Window);
  XScreen := XDefaultScreenOfDisplay(xdisplay);
  XRootWindow := XRootWindowOfScreen(xscreen);
  XWindow := GDK_WINDOW_XWINDOW (Window);

  _NET_WM_DESKTOP := XInternAtom(xdisplay, '_NET_WM_DESKTOP', false);

  XEvent._type := ClientMessage;
  XEvent.window := XWindow;
  XEvent.message_type := _NET_WM_DESKTOP;
  XEvent.format := 32;
  XEvent.data.l[0] := Desktop;

  XSendEvent(XDisplay, XRootWindow, False, SubstructureNotifyMask, PXEvent(@XEvent));
end;


procedure GDK_WINDOW_ACTIVATE(Window: PGdkWindowPrivate);
var
  XDisplay: PDisplay;
  XScreen: PScreen;
  aXRootWindow,
  XWindow: x.TWindow;
  XEvent: xlib.TXClientMessageEvent;
  _NET_ACTIVE_WINDOW: Integer;
begin
  if (Window=nil) or (gdk.destroyed(Window^)<>0) then exit;

  XDisplay := GDK_WINDOW_XDISPLAY (Window);
  if XDisplay=nil then exit;
  XScreen := XDefaultScreenOfDisplay(xdisplay);
  if XScreen=nil then exit;
  aXRootWindow := XRootWindowOfScreen(xscreen);
  if aXRootWindow=0 then exit;
  XWindow := GDK_WINDOW_XWINDOW (Window);
  if XWindow=0 then exit;

  _NET_ACTIVE_WINDOW := XInternAtom(xdisplay, '_NET_ACTIVE_WINDOW', false);

  XEvent._type := ClientMessage;
  XEvent.window := XWindow;
  XEvent.message_type := _NET_ACTIVE_WINDOW;
  XEvent.format := 32;
  XEvent.data.l[0] := 1; //Message is from program
  XEvent.data.l[1] := CurrentTime;
  XEvent.data.l[2] := 0; // Applications current active window

  XSendEvent(XDisplay, aXRootWindow, False, SubstructureNotifyMask, PXEvent(@XEvent));
end;

procedure GDK_WINDOW_MAXIMIZE(Window: PGdkWindowPrivate);
const
  _NET_WM_STATE_REMOVE    =    0;   // remove/unset property
  _NET_WM_STATE_ADD       =    1;   // add/set property
  _NET_WM_STATE_TOGGLE    =    2;   // toggle property
var
  XDisplay: PDisplay;
  XScreen: PScreen;
  aXRootWindow,
  XWindow: TWindow;
  XEvent: TXClientMessageEvent;
  _NET_WM_STATE,
  _NET_WM_STATE_MAXIMIZED_VERT,
  _NET_WM_STATE_MAXIMIZED_HORZ: Integer;

begin
  XDisplay := GDK_WINDOW_XDISPLAY (Window);
  XScreen := XDefaultScreenOfDisplay(xdisplay);
  aXRootWindow := XRootWindowOfScreen(xscreen);
  XWindow := GDK_WINDOW_XWINDOW (Window);

  _NET_WM_STATE := XInternAtom(xdisplay, '_NET_WM_STATE', false);
  _NET_WM_STATE_MAXIMIZED_VERT := XInternAtom(xdisplay, '_NET_WM_STATE_MAXIMIZED_VERT', false);
  _NET_WM_STATE_MAXIMIZED_HORZ := XInternAtom(xdisplay, '_NET_WM_STATE_MAXIMIZED_HORZ', false);

  XEvent._type := ClientMessage;
  XEvent.window := XWindow;
  XEvent.message_type := _NET_WM_STATE;
  XEvent.format := 32;
  XEvent.data.l[0] := _NET_WM_STATE_ADD;
  XEvent.data.l[1] := _NET_WM_STATE_MAXIMIZED_HORZ;
  XEvent.data.l[2] := _NET_WM_STATE_MAXIMIZED_VERT;

  XSendEvent(XDisplay, aXRootWindow, False, SubstructureNotifyMask, PXEvent(@XEvent));
end;

function GDK_WINDOW_GET_MINIMIZED(Window: PGdkWindowPrivate): gboolean;
const
  _NET_WM_STATE_REMOVE    =    0;   // remove/unset property
  _NET_WM_STATE_ADD       =    1;   // add/set property
  _NET_WM_STATE_TOGGLE    =    2;   // toggle property
var
  XDisplay: PDisplay;
  XWindow: x.TWindow;
  _NET_WM_STATE,
  _NET_WM_STATE_HIDDEN: Integer;
  atomtype: x.TAtom;
  format: gint;
  nitems: gulong;
  bytes_after: gulong;
  state_array: pguint;
  X: Integer;
begin
  Result := False;
  XDisplay := GDK_WINDOW_XDISPLAY(Window);
  XWindow := GDK_WINDOW_XWINDOW(Window);

  _NET_WM_STATE := XInternAtom(XDisplay, '_NET_WM_STATE', False);
  _NET_WM_STATE_HIDDEN := XInternAtom(XDisplay, '_NET_WM_STATE_HIDDEN', False);

  XGetWindowProperty(XDisplay, XWindow, _NET_WM_STATE,
             0, MaxInt, False, XA_ATOM, @atomtype, @format, @nitems,
             @bytes_after, @state_array);
  if (atomtype = XA_ATOM) and (format = 32) and  (nitems > 0) then
  begin
    // Check to see if the window is already minimized...
    for X := 0 to nitems - 1 do
    begin
      if (state_array[X] = _NET_WM_STATE_HIDDEN) then
      begin
        XFree(state_array);
        Exit(True);
      end;
    end;
  end;
  if state_array <> nil then
    XFree(state_array);
end;

procedure GDK_WINDOW_MINIMIZE(Window: PGdkWindowPrivate);
var
  XDisplay: PDisplay;
  XScreen: PScreen;
  XWindow: x.TWindow;
begin
  if GDK_WINDOW_GET_MINIMIZED(Window) then
    Exit;

  XDisplay := GDK_WINDOW_XDISPLAY(Window);
  XScreen := XDefaultScreenOfDisplay(XDisplay);
  XWindow := GDK_WINDOW_XWINDOW(Window);

  XIconifyWindow(XDisplay, XWindow, XScreenNumberOfScreen(XScreen));
end;

function GDK_WINDOW_GET_MAXIMIZED(Window: PGdkWindowPrivate): gboolean;
var
  xdisplay: PDisplay;
  xwindow: TWindow;

  atomtype: x.TAtom;
  format: gint;
  nitems: gulong;
  bytes_after: gulong;
  state_array: pguint;
  _NET_WM_STATE,
  _NET_WM_STATE_MAXIMIZED_VERT,
  _NET_WM_STATE_MAXIMIZED_HORZ: x.TAtom;
  X: Integer;
  maximized_horz, maximized_vert: Boolean;
begin
  Result := False;
  XWindow := GDK_WINDOW_XWINDOW(Window);
  XDisplay := GDK_WINDOW_XDISPLAY(Window);

  _NET_WM_STATE := XInternAtom(XDisplay, '_NET_WM_STATE', False);
  _NET_WM_STATE_MAXIMIZED_VERT := XInternAtom(XDisplay, '_NET_WM_STATE_MAXIMIZED_VERT', False);
  _NET_WM_STATE_MAXIMIZED_HORZ := XInternAtom(XDisplay, '_NET_WM_STATE_MAXIMIZED_HORZ', False);

  XGetWindowProperty (xdisplay, xwindow,
             _NET_WM_STATE,
             0, MaxInt, False, XA_ATOM, @atomtype, @format, @nitems,
             @bytes_after, gpointer(@state_array));

  if (atomtype = XA_ATOM) and (format = 32) and  (nitems > 0) then
  begin
    maximized_horz := False;
    maximized_vert := False;
    for X := 0 to nitems - 1 do
    begin
      if (state_array[X] = _NET_WM_STATE_MAXIMIZED_VERT) then maximized_vert := True;
      if (state_array[X] = _NET_WM_STATE_MAXIMIZED_HORZ) then maximized_horz := True;
      Result := (maximized_horz and maximized_vert);

      if Result then Break;
    end;
  end;
  if state_array <> nil then
    XFree(state_array);
end;

procedure GDK_WINDOW_SHOW_IN_TASKBAR(Window: PGdkWindowPrivate; Show: Boolean);
// this is a try to hide windows from the taskbar.
// Unpleasantly, some windowmanagers like metacity also hides from the Alt-Tab
// cycle.
// This feature is therefore disabled on default.
{$IFDEF EnableHideFromTaskBar}
var
  XDisplay: PDisplay;
  XScreen: PScreen;
  XRootWindow,
  XWindow: TWindow;
  XEvent: TXClientMessageEvent;
  _NET_WM_STATE,
  _NET_WM_STATE_SKIP_TASKBAR: clong;
{$ENDIF}
begin
  {$IFDEF EnableHideFromTaskBar}
  // GTK1: reshowing does not work, so a modal form will hide the whole application
  // GTK

  XDisplay := GDK_WINDOW_XDISPLAY (Window);
  XScreen := XDefaultScreenOfDisplay(xdisplay);
  XRootWindow := XRootWindowOfScreen(xscreen);
  XWindow := GDK_WINDOW_XWINDOW (Window);

  _NET_WM_STATE := XInternAtom(xdisplay, '_NET_WM_STATE', false);
  _NET_WM_STATE_SKIP_TASKBAR := XInternAtom(xdisplay, '_NET_WM_STATE_SKIP_TASKBAR', false);

  XEvent._type := ClientMessage;
  XEvent.window := XWindow;
  XEvent.message_type := _NET_WM_STATE;
  XEvent.format := 32;
  if Show then
    XEvent.data.l[0] := 1
  else
    XEvent.data.l[0] := 0;// 0=Remove 1=Add 2=Toggle
  XEvent.data.l[1] := _NET_WM_STATE_SKIP_TASKBAR;

  XSendEvent(XDisplay, XRootWindow, False, SubstructureNotifyMask, @XEvent);
  {$ENDIF}
end;

function gtk_class_get_type(aclass : Pointer) : TGtkType;
begin
  If (aclass <> nil) then
    result := PGtkTypeClass(aclass)^.thetype
  else
    result := 0;
end;

function gtk_object_get_class(anobject : Pointer) : Pointer;
begin
  If (anobject <> nil) then
    result := PGtkTypeObject(anobject)^.klass
  else
    result := nil;
end;

function gtk_window_get_modal(window:PGtkWindow):gboolean;
begin
  if assigned(Window) then
    result := (Window^.flag0 and bm_modal)<>0
  else
    result := False;
end;

function gtk_bin_get_child(bin : PGTKBin) : PGTKWidget;
begin
  if (bin <> nil) then
    result := bin^.Child
  else
    result := nil;
end;

procedure gtk_menu_item_set_right_justified(menu_item : PGtkMenuItem; right_justified : gboolean);
begin
  if right_justified then
    menu_item^.flag0:=menu_item^.flag0 or bm_right_justify
  else
    menu_item^.flag0:=menu_item^.flag0 and (not bm_right_justify);
end;

function gtk_check_menu_item_get_active(menu_item : PGtkCheckMenuItem) : gboolean;
begin
   Result:=(menu_item^.flag0 and bm_checkmenuitem_active <> 0);
end;

procedure gtk_menu_append(menu : PGTKWidget; Item : PGtkWidget);
begin
  gtk.gtk_menu_append(PGTKMenu(menu), Item);
end;

procedure gtk_menu_insert(menu : PGtkWidget; Item : PGTKWidget; Index : gint);
begin
  gtk.gtk_menu_insert(PGTKMenu(menu), Item, Index);
end;

procedure gtk_menu_bar_insert(menubar : PGtkWidget; Item : PGTKWidget; Index : gint);
begin
  gtk.gtk_menu_bar_insert(PGtkMenuBar(menubar), Item, Index);
end;

function gtk_image_new :PGTKWidget;
begin
  result := gtk.gtk_image_new(nil,nil);
end;

function gtk_toolbar_new : PGTKWidget;
begin
  result := gtk.gtk_toolbar_new(GTK_ORIENTATION_HORIZONTAL,GTK_TOOLBAR_BOTH);
end;

procedure gtk_color_selection_get_current_color(colorsel : PGTKColorSelection; Color : PGDKColor);
var
  colorArray : array[0..2] of double;
begin
  gtk_color_selection_get_color(colorsel, @colorArray[0]);
  Color^.pixel := 0;
  Color^.red := gushort(TruncToCardinal(colorArray[0] * $FFFF));
  Color^.green := gushort(TruncToCardinal(colorArray[1] * $FFFF));
  Color^.blue := gushort(TruncToCardinal(colorArray[2] * $FFFF));
  {$IFDEF VerboseColorDialog}
  DebugLn('gtk_color_selection_get_current_color ',
    ' Red=',DbgS(Color^.Red),
    ' Green=',DbgS(Color^.Green),
    ' Blue=',DbgS(Color^.Blue),
    '');
  {$ENDIF}
end;

procedure gtk_color_selection_set_current_color(colorsel : PGTKColorSelection;
  Color : PGDKColor);
var
  SelectionColor: PGDouble;
begin
  {$IFDEF VerboseColorDialog}
  DebugLn('gtk_color_selection_set_current_color ',
    ' Red=',DbgS(Color^.Red),
    ' Green=',DbgS(Color^.Green),
    ' Blue=',DbgS(Color^.Blue),
    '');
  {$ENDIF}
  GetMem(SelectionColor,4*SizeOf(GDouble));
  try
    SelectionColor[0]:=gdouble(Color^.Red)/65535;
    SelectionColor[1]:=gdouble(Color^.Green)/65535;
    SelectionColor[2]:=gdouble(Color^.Blue)/65535;
    SelectionColor[3]:=0.0;
    gtk_color_selection_set_color(colorSel,SelectionColor);
  finally
    FreeMem(SelectionColor);
  end;
end;

procedure gdk_image_unref(Image : PGdkImage);
begin
  gdk_window_unref(PGdkWindow(Image));
end;

procedure gdk_colormap_query_color(colormap : PGDKColormap; Pixel : gulong; Result : PGDKColor);
var
  GdkColorContext: PGdkColorContext;
begin
  if (Colormap = nil) or (Result = nil) then exit;
  GdkColorContext:= gdk_color_context_new(gdk_colormap_get_visual(colormap),colormap);
  Result^.Pixel := Pixel;
  gdk_color_context_query_color(GdkColorContext, Result);
  gdk_color_context_free(GdkColorContext);
end;

function gdk_region_intersect(source1:PGdkRegion; source2:PGdkRegion) : PGdkRegion;
begin
  result := gdk_regions_intersect(source1, source2);
end;

function gdk_region_union(source1:PGdkRegion; source2:PGdkRegion) : PGdkRegion;
begin
  result := gdk_regions_union(source1, source2);
end;

function gdk_region_subtract(source1:PGdkRegion; source2:PGdkRegion) : PGdkRegion;
begin
  result := gdk_regions_subtract(source1, source2);
end;

function gdk_region_xor(source1:PGdkRegion; source2:PGdkRegion) : PGdkRegion;
begin
  result := gdk_regions_xor(source1, source2);
end;

function gdk_region_copy(region: PGDKRegion): PGDKRegion;
var
  EmptyRegion: PGdkRegion;
begin
  EmptyRegion := gdk_region_new;
  Result := gdk_regions_union(region, EmptyRegion);
  gdk_region_destroy(EmptyRegion);
end;

function gdk_region_rectangle(rect: PGdkRectangle): PGDKRegion;
var
  EmptyRegion: PGdkRegion;
begin
  EmptyRegion := gdk_region_new;
  Result := gdk_region_union_with_rect(EmptyRegion,Rect);
  gdk_region_destroy(EmptyRegion);
end;

function gdk_pixmap_create_from_xpm_d (window : PGdkWindow; var mask : PGdkBitmap; transparent_color : PGdkColor; data : PPgchar) : PGdkPixmap;
begin
  result := gdk.gdk_pixmap_create_from_xpm_d(window, @mask, transparent_color, data)
end;

function gdk_pixmap_colormap_create_from_xpm_d (window : PGdkWindow; colormap: PGdkColormap; var mask : PGdkBitmap; transparent_color : PGdkColor; data : PPgchar) : PGdkPixmap;
begin
  result := gdk.gdk_pixmap_colormap_create_from_xpm_d(window, colormap, @mask, transparent_color, data)
end;

function gdk_pixmap_colormap_create_from_xpm (window : PGdkWindow; colormap: PGdkColormap; var mask : PGdkBitmap; transparent_color : PGdkColor; filename : Pgchar) : PGdkPixmap;
begin
  result := gdk.gdk_pixmap_colormap_create_from_xpm(window, colormap, @mask, transparent_color, filename)
end;

procedure gdk_pixbuf_render_pixmap_and_mask(pixbuf : PGdkPixbuf; var pixmap_return : PGdkPixmap; var mask_return : PGdkBitmap; alpha_threshold : gint);
begin
  gdkpixbuf.gdk_pixbuf_render_pixmap_and_mask(pixbuf, @pixmap_return, @mask_return, alpha_threshold);
end;

function gdk_pixbuf_new_subpixbuf(src_pixbuf: PGdkPixbuf; src_x: longint; src_y: longint; width: longint; height: longint): PGdkPixbuf;
var
  tmp: PGdkPixbuf;
  buf: PChar;
  rowstride: Integer;
begin
  buf := gdk_pixbuf_get_pixels(src_pixbuf);

  rowstride := gdk_pixbuf_get_rowstride(src_pixbuf);

  inc(buf, src_y * rowstride + src_x * gdk_pixbuf_get_n_channels(src_pixbuf));
  tmp := gdk_pixbuf_new_from_data(buf, gdk_pixbuf_get_colorspace(src_pixbuf),
    gdk_pixbuf_get_has_alpha(src_pixbuf), gdk_pixbuf_get_bits_per_sample(src_pixbuf),
    width, height, rowstride, nil, nil);
  Result := gdk_pixbuf_copy(tmp);
  gdk_pixbuf_unref(tmp);
end;

function gdk_drawable_get_depth(Drawable : PGDKDrawable) : gint;
begin
  gdk_window_get_geometry(Drawable, nil, nil, nil, nil, @result);
end;

procedure gdk_drawable_get_size(Drawable : PGDKDrawable; Width, Height : PGInt);
begin
  gdk_window_get_geometry(Drawable, nil, nil, Width, Height, nil);
end;

function gdk_drawable_get_image(Drawable : PGDKDrawable; x, y, width, height : gint) : PGdkImage;
begin
  result := gdk_image_get(Drawable, x, y, width, height);
end;

function gdk_drawable_get_colormap(Drawable : PGDKDrawable) : PGdkColormap;
begin
  result := gdk_window_get_colormap(Drawable);
end;

{$ifdef HasX}
function gdk_x11_image_get_ximage(image: PGdkImage): PXImage;
begin
  Result := PGdkImagePrivate(Image)^.ximage;
end;
{$endif}

