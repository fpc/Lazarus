{%MainUnit ../gdk2x.pas}

{$IFDEF read_forward_definitions}
   // remove this TXRenderColor, when it has been added to the X11 bindings of FPC
   TXRenderColor = record
     red: word;
     green: word;
     blue: word;
     alpha: word;
   end;
   
   PPXRectangle = ^PXRectangle;
{$ENDIF read_forward_definitions}

{$IFDEF read_interface_rest}

type
   PGdkGCX11 = ^TGdkGCX11;
   TGdkGCX11 = record
        parent_instance : TGdkGC;
        xgc : TGC;
        screen : PGdkScreen;
        clip_region : PGdkRegion;
        dirty_mask : guint;
        fg_picture : TPicture;
        fg_picture_color : TXRenderColor;
        fg_pixel : gulong;
     end;

   PGdkGCX11Class = ^TGdkGCX11Class;
   TGdkGCX11Class = record
        parent_class : TGdkGCClass;
     end;

   PGdkCursorPrivate = ^TGdkCursorPrivate;
   TGdkCursorPrivate = record
        cursor : TGdkCursor;
        xcursor : TCursor;
        screen : PGdkScreen;
     end;

   PGdkVisualPrivate = ^TGdkVisualPrivate;
   TGdkVisualPrivate = record
        visual : TGdkVisual;
        xvisual : PVisual;
        screen : PGdkScreen;
     end;

// Private function GDK_TYPE_GC_X11 : GType;
function GDK_GC_X11(obj : pointer) : PGdkGCX11;
function GDK_GC_X11_CLASS(klass : pointer) : PGdkGCX11Class;
// Private function GDK_IS_GC_X11(obj : pointer) : boolean;
// Private function GDK_IS_GC_X11_CLASS(klass : pointer) : boolean;
// Private function GDK_GC_X11_GET_CLASS(obj : pointer) : PGdkGCX11Class;


procedure _gdk_xid_table_insert(display:PGdkDisplay; xid:PXID; data:gpointer);cdecl;external;
procedure _gdk_xid_table_remove(display:PGdkDisplay; xid:TXID);cdecl;external;
function _gdk_send_xevent(display:PGdkDisplay; window:TWindow; propagate:gboolean; event_mask:glong; event_send:PXEvent):gint;cdecl;external;
// Private function _gdk_gc_x11_get_type:GType;cdecl;external;

{$ifdef HAVE_XFT}
function _gdk_x11_have_render(display:PGdkDisplay):gboolean;cdecl;external;
function _gdk_x11_gc_get_fg_picture(gc:PGdkGC):TPicture;cdecl;external;
{$endif HAVE_XFT}

function _gdk_x11_gc_new(drawable:PGdkDrawable; values:PGdkGCValues; values_mask:TGdkGCValuesMask):PGdkGC;cdecl;external;
function gdk_colormap_lookup(xcolormap:TColormap):PGdkColormap;cdecl;external;
function gdk_visual_lookup(xvisual:PVisual):PGdkVisual;cdecl;external;
procedure gdk_window_add_colormap_windows(window:PGdkWindow);cdecl;external;
function _gdk_x11_copy_to_image(drawable:PGdkDrawable; image:PGdkImage; src_x:gint; src_y:gint; dest_x:gint;
           dest_y:gint; width:gint; height:gint):PGdkImage;cdecl;external;
function _gdk_x11_image_get_shm_pixmap(image:PGdkImage):TPixmap;cdecl;external;

{ Routines from gdkgeometry-x11.c  }
procedure _gdk_window_init_position(window:PGdkWindow);cdecl;external;
procedure _gdk_window_move_resize_child(window:PGdkWindow; x:gint; y:gint; width:gint; height:gint);cdecl;external;
procedure _gdk_window_process_expose(window:PGdkWindow; serial:gulong; area:PGdkRectangle);cdecl;external;
procedure _gdk_selection_window_destroyed(window:PGdkWindow);cdecl;external;
function _gdk_selection_filter_clear_event(event:PXSelectionClearEvent):gboolean;cdecl;external;
procedure _gdk_region_get_xrectangles(region:PGdkRegion; x_offset:gint; y_offset:gint; rects:PPXRectangle; n_rects:Pgint);cdecl;external;
function _gdk_moveresize_handle_event(event:PXEvent):gboolean;cdecl;external;
function _gdk_moveresize_configure_done(display:PGdkDisplay; window:PGdkWindow):gboolean;cdecl;external;
procedure _gdk_keymap_state_changed(display:PGdkDisplay);cdecl;external;
function _gdk_x11_get_group_for_state(display:PGdkDisplay; state:TGdkModifierType):gint;cdecl;external;
//function _gdk_x11_gc_flush(gc:PGdkGC):TGC;cdecl;external;
procedure _gdk_x11_initialize_locale;cdecl;external;
procedure _gdk_xgrab_check_unmap(window:PGdkWindow; serial:gulong);cdecl;external;
procedure _gdk_xgrab_check_destroy(window:PGdkWindow);cdecl;external;
function _gdk_x11_display_is_root_window(display:PGdkDisplay; xroot_window:TWindow):gboolean;cdecl;external;
procedure _gdk_x11_events_init_screen(screen:PGdkScreen);cdecl;external;
procedure _gdk_events_init(display:PGdkDisplay);cdecl;external;
procedure _gdk_windowing_window_init(screen:PGdkScreen);cdecl;external;
procedure _gdk_visual_init(screen:PGdkScreen);cdecl;external;
procedure _gdk_dnd_init(display:PGdkDisplay);cdecl;external;
procedure _gdk_windowing_image_init(display:PGdkDisplay);cdecl;external;
procedure _gdk_input_init(display:PGdkDisplay);cdecl;external;


var
   _gdk_x11_drawable_class : TGdkDrawableClass;cvar;external;
   _gdk_use_xshm : gboolean;cvar;external;
(* Const before type ignored *)
   _gdk_nenvent_masks : longint;cvar;external;
(* Const before type ignored *)
   _gdk_event_mask_table : array of longint;cvar;external;
   _gdk_selection_property : TGdkAtom;cvar;external;
   _gdk_synchronize : gboolean;cvar;external;

function GDK_PIXMAP_SCREEN(pix : pointer) : PGdkScreen;

function GDK_PIXMAP_DISPLAY(pix : pointer) : PGdkDisplay;
function GDK_PIXMAP_XROOTWIN(pix : pointer) : TWindow;
function GDK_DRAWABLE_DISPLAY(win : PGdkDrawable) : PGdkDisplay;
function GDK_DRAWABLE_SCREEN(win : PGdkDrawable) : PGdkScreen;
function GDK_DRAWABLE_XROOTWIN(win : PGdkDrawable) : TWindow;
function GDK_SCREEN_DISPLAY(screen : PGdkScreen) : PGdkDisplay;
function GDK_SCREEN_XROOTWIN(screen : PGdkScreen) : TWindow;
function GDK_WINDOW_SCREEN(win : PGdkDrawable) : PGdkScreen;
function GDK_WINDOW_DISPLAY(win : PGdkDrawable) : PGdkDisplay;
function GDK_WINDOW_XROOTWIN(win : PGdkDrawable) : TWindow;
function GDK_GC_DISPLAY(gc : PGdkGC) : PGdkDisplay;

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}

{ Private
function GDK_TYPE_GC_X11 : GType;
begin
   GDK_TYPE_GC_X11:=_gdk_gc_x11_get_type;
end;}

function GDK_GC_X11(obj : pointer) : PGdkGCX11;
begin
   GDK_GC_X11:=PGdkGCX11(obj);
   //GDK_GC_X11:=PGdkGCX11(G_TYPE_CHECK_INSTANCE_CAST(obj,GDK_TYPE_GC_X11));
end;

function GDK_GC_X11_CLASS(klass : pointer) : PGdkGCX11Class;
begin
   GDK_GC_X11_CLASS:=PGdkGCX11Class(klass);
   //GDK_GC_X11_CLASS:=PGdkGCX11Class(G_TYPE_CHECK_CLASS_CAST(klass,GDK_TYPE_GC_X11));
end;

{ Private
function GDK_IS_GC_X11(obj : pointer) : boolean;
begin
   GDK_IS_GC_X11:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GDK_TYPE_GC_X11);
end;

function GDK_IS_GC_X11_CLASS(klass : pointer) : boolean;
begin
   GDK_IS_GC_X11_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GDK_TYPE_GC_X11);
end;

function GDK_GC_X11_GET_CLASS(obj : pointer) : PGdkGCX11Class;
begin
   GDK_GC_X11_GET_CLASS:=PGdkGCX11Class(G_TYPE_INSTANCE_GET_CLASS(obj,GDK_TYPE_GC_X11));
end;     }

function GDK_PIXMAP_SCREEN(pix : pointer) : PGdkScreen;
begin
   GDK_PIXMAP_SCREEN:=(GDK_DRAWABLE_IMPL_X11((PGdkPixmapObject(pix))^.impl))^.screen;
end;

function GDK_PIXMAP_DISPLAY(pix : pointer) : PGdkDisplay;
begin
   GDK_PIXMAP_DISPLAY:=(GDK_SCREEN_X11(GDK_PIXMAP_SCREEN(pix)))^.display;
end;

function GDK_PIXMAP_XROOTWIN(pix : pointer) : TWindow;
begin
   GDK_PIXMAP_XROOTWIN:=(GDK_SCREEN_X11(GDK_PIXMAP_SCREEN(pix)))^.xroot_window;
end;

function GDK_DRAWABLE_DISPLAY(win : PGdkDrawable) : PGdkDisplay;
var
   if_local1 : PGdkDisplay;
begin
   if GDK_IS_WINDOW(win) then
     if_local1:=GDK_WINDOW_DISPLAY(win)
   else
     if_local1:=GDK_PIXMAP_DISPLAY(win);
   GDK_DRAWABLE_DISPLAY:=if_local1;
end;

function GDK_DRAWABLE_SCREEN(win : PGdkDrawable) : PGdkScreen;
var
   if_local1 : PGdkScreen;
begin
   if GDK_IS_WINDOW(win) then
     if_local1:=GDK_WINDOW_SCREEN(win)
   else
     if_local1:=GDK_PIXMAP_SCREEN(win);
   GDK_DRAWABLE_SCREEN:=if_local1;
end;

function GDK_DRAWABLE_XROOTWIN(win : PGdkDrawable) : TWindow;
var
   if_local1 : TWindow;
begin
   if GDK_IS_WINDOW(win) then
     if_local1:=GDK_WINDOW_XROOTWIN(win)
   else
     if_local1:=GDK_PIXMAP_XROOTWIN(win);
   GDK_DRAWABLE_XROOTWIN:=if_local1;
end;

function GDK_SCREEN_DISPLAY(screen : PGdkScreen) : PGdkDisplay;
begin
   GDK_SCREEN_DISPLAY:=(GDK_SCREEN_X11(screen))^.display;
end;

function GDK_SCREEN_XROOTWIN(screen : PGdkScreen) : TWindow;
begin
   GDK_SCREEN_XROOTWIN:=(GDK_SCREEN_X11(screen))^.xroot_window;
end;

function GDK_WINDOW_SCREEN(win : PGdkDrawable) : PGdkScreen;
begin
   GDK_WINDOW_SCREEN:=(GDK_DRAWABLE_IMPL_X11((PGdkWindowObject(win))^.impl))^.screen;
end;

function GDK_WINDOW_DISPLAY(win : PGdkDrawable) : PGdkDisplay;
begin
   GDK_WINDOW_DISPLAY:=(GDK_SCREEN_X11(GDK_WINDOW_SCREEN(win)))^.display;
end;

function GDK_WINDOW_XROOTWIN(win : PGdkDrawable) : TWindow;
begin
   GDK_WINDOW_XROOTWIN:=(GDK_SCREEN_X11(GDK_WINDOW_SCREEN(win)))^.xroot_window;
end;

function GDK_GC_DISPLAY(gc : PGdkGC) : PGdkDisplay;
begin
   GDK_GC_DISPLAY:=GDK_SCREEN_DISPLAY((GDK_GC_X11(gc))^.screen);
end;

{$ENDIF read_implementation}

