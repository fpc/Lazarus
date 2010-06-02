{%MainUnit gtkextra.pp}
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

uses
  {$ifdef hasx}
    XAtom,
    X,
    XLib,
//    XUtil, //Font retrieval and Keyboard handling
  {$endif}


  glib, gdk, gdkpixbuf, gtk,
  ctypes, classes, LCLProc;


type
  PGtkOldEditable = PGtkEditable;
  GType = TGtkType;

function G_OBJECT(p: Pointer): PGtkObject;
function G_CALLBACK(p: Pointer): TGTKSignalFunc;

//routines to mimic GObject routines/behaviour-->
procedure g_signal_emit_by_name(anObject:PGtkObject; name:Pgchar;
         args: array of const);
         cdecl; overload; external gtkdll name 'gtk_signal_emit_by_name';
procedure g_signal_emit_by_name(anObject:PGtkObject; name:Pgchar);
         cdecl; overload; external gtkdll name 'gtk_signal_emit_by_name';

procedure g_signal_handlers_destroy(anObject: PGtkObject);
         cdecl; external gtkdll name 'gtk_signal_handlers_destroy';
procedure g_signal_stop_emission_by_name(anObject: PGtkObject;
         detailed_signal: Pgchar);
         cdecl; external gtkdll name 'gtk_signal_emit_stop_by_name';
function g_signal_connect(anObject: PGtkObject; name: Pgchar;
         func: TGtkSignalFunc; func_data: gpointer): guint;
         cdecl; external gtkdll name 'gtk_signal_connect';
function g_signal_connect_after(anObject: PGtkObject; name: Pgchar;
         func: TGtkSignalFunc; func_data: gpointer): guint;
         cdecl; external gtkdll name 'gtk_signal_connect_after';
function g_signal_lookup(name: Pgchar; anObject: TGTKType): guint;
         cdecl; external gtkdll name 'gtk_signal_lookup';
procedure _gtk_range_get_props(range: PGtkRange; slider_width, trough_border,
  stepper_size, stepper_spacing: pgint); cdecl; external gtkdll name '_gtk_range_get_props';

function GDK_GET_CURRENT_DESKTOP(): gint;
function GDK_WINDOW_GET_DESKTOP(Window: PGdkWindowPrivate): gint;
function GDK_WINDOW_SET_DESKTOP(Window: PGdkWindowPrivate; Desktop: gint): gint;
procedure GDK_WINDOW_ACTIVATE(Window: PGdkWindowPrivate);
procedure GDK_WINDOW_MAXIMIZE(Window: PGdkWindowPrivate);
procedure GDK_WINDOW_MINIMIZE(Window: PGdkWindowPrivate);
function GDK_WINDOW_GET_MINIMIZED(Window: PGdkWindowPrivate): gboolean;
function GDK_WINDOW_GET_MAXIMIZED(Window: PGdkWindowPrivate): gboolean;
procedure GDK_WINDOW_SHOW_IN_TASKBAR(Window: PGdkWindowPrivate; Show: Boolean);

//routines to mimic similar GDK2 routines/behaviour-->
procedure gdk_image_unref(Image: PGdkImage);
procedure gdk_colormap_query_color(colormap: PGDKColormap; Pixel: gulong;
                                   Result: PGDKColor);

//Wrapper around misnamed "regions" routines -->
function gdk_region_intersect(source1:PGdkRegion; source2:PGdkRegion): PGdkRegion;
function gdk_region_union(source1:PGdkRegion; source2:PGdkRegion): PGdkRegion;
function gdk_region_subtract(source1:PGdkRegion; source2:PGdkRegion): PGdkRegion;
function gdk_region_xor(source1:PGdkRegion; source2:PGdkRegion): PGdkRegion;
function gdk_region_copy(region: PGDKRegion): PGDKRegion;
function gdk_region_rectangle(rect: PGdkRectangle): PGDKRegion;

//routines to mimic similar GDK2 routines/behaviour-->
function gdk_pixmap_create_from_xpm_d (window: PGdkWindow;
                           var mask: PGdkBitmap; transparent_color: PGdkColor;
                           data: PPgchar): PGdkPixmap;
function gdk_pixmap_colormap_create_from_xpm_d (window: PGdkWindow;
                     colormap: PGdkColormap; var mask: PGdkBitmap;
                     transparent_color: PGdkColor; data: PPgchar): PGdkPixmap;
function gdk_pixmap_colormap_create_from_xpm (window: PGdkWindow;
                  colormap: PGdkColormap; var mask: PGdkBitmap;
                  transparent_color: PGdkColor; filename: Pgchar): PGdkPixmap;
procedure gdk_pixbuf_render_pixmap_and_mask(pixbuf: PGdkPixbuf;
  var pixmap_return: PGdkPixmap; var mask_return: PGdkBitmap;
  alpha_threshold: gint);
function gdk_pixbuf_new_subpixbuf(src_pixbuf: PGdkPixbuf; src_x: longint;
  src_y: longint; width: longint; height: longint): PGdkPixbuf;

//Wrapper around window functions like gtk2 -->
function gdk_drawable_get_depth(Drawable: PGDKDrawable): gint;
procedure gdk_drawable_get_size(Drawable: PGDKDrawable; Width, Height: PGInt);
function gdk_drawable_get_image(Drawable: PGDKDrawable;
                                x, y, width, height: gint): PGdkImage;
function gdk_drawable_get_colormap(Drawable: PGDKDrawable): PGdkColormap;


function  GTK_TYPE_WIDGET : TGTKType; cdecl; external gtkdll name 'gtk_widget_get_type';
function  GTK_TYPE_CONTAINER: TGTKType; cdecl; external gtkdll name 'gtk_container_get_type';
function  GTK_TYPE_BIN : TGTKType; cdecl; external gtkdll name 'gtk_bin_get_type';
function  GTK_TYPE_HBOX : TGTKType; cdecl; external gtkdll name 'gtk_hbox_get_type';
function  GTK_TYPE_SCROLLED_WINDOW: TGTKType; cdecl; external gtkdll name 'gtk_scrolled_window_get_type';
function  GTK_TYPE_COMBO : TGTKType; cdecl; external gtkdll name 'gtk_combo_get_type';
function  GTK_TYPE_WINDOW : TGTKType; cdecl; external gtkdll name 'gtk_window_get_type';
function  GTK_TYPE_MENU : TGTKType; cdecl; external gtkdll name 'gtk_menu_get_type';
function  GTK_TYPE_MENU_ITEM : TGTKType; cdecl; external gtkdll name 'gtk_menu_item_get_type';
function  GTK_TYPE_MENU_BAR : TGTKType; cdecl; external gtkdll name 'gtk_menu_bar_get_type';
function  GTK_TYPE_RADIO_MENU_ITEM : TGTKType; cdecl; external gtkdll name 'gtk_radio_menu_item_get_type';
function  GTK_TYPE_CHECK_MENU_ITEM : TGTKType; cdecl; external gtkdll name 'gtk_check_menu_item_get_type';
function  GTK_TYPE_TEXT : TGTKType; cdecl; external gtkdll name 'gtk_text_get_type';
function  GTK_TYPE_ENTRY : TGTKType; cdecl; external gtkdll name 'gtk_entry_get_type';
function  GTK_TYPE_RANGE : TGTKType; cdecl; external gtkdll name 'gtk_range_get_type';
function  GTK_TYPE_SCROLLBAR: TGTKType; cdecl; external gtkdll name 'gtk_scrollbar_get_type';
function  GTK_TYPE_HSCROLLBAR: TGTKType; cdecl; external gtkdll name 'gtk_hscrollbar_get_type';
function  GTK_TYPE_VSCROLLBAR: TGTKType; cdecl; external gtkdll name 'gtk_vscrollbar_get_type';
function  GTK_TYPE_LIST_ITEM: TGTKType; cdecl; external gtkdll name 'gtk_list_item_get_type';

function gtk_class_get_type(aclass: Pointer): TGtkType;

//routines to mimic similar GTK2 routines/behaviour-->
function gtk_object_get_class(anobject: Pointer): Pointer;
function gtk_window_get_modal(window:PGtkWindow):gboolean;
function gtk_bin_get_child(bin: PGTKBin): PGTKWidget;
procedure gtk_menu_item_set_right_justified(menu_item: PGtkMenuItem;
                                            right_justified: gboolean);
function gtk_check_menu_item_get_active(menu_item: PGtkCheckMenuItem): gboolean;
procedure gtk_menu_append(menu: PGTKWidget; Item: PGtkWidget);
procedure gtk_menu_insert(menu: PGtkWidget; Item: PGTKWidget; Index: gint);
procedure gtk_menu_bar_insert(menubar: PGtkWidget; Item: PGTKWidget; Index: gint);
function gtk_image_new: PGTKWidget;
function gtk_toolbar_new: PGTKWidget;
procedure gtk_color_selection_get_current_color(colorsel: PGTKColorSelection;
                                                Color: PGDKColor);
procedure gtk_color_selection_set_current_color(colorsel: PGTKColorSelection;
                                                Color: PGDKColor);

//routines to mimic similar GTK2 routines/behaviour-->
{$ifdef HasX}
function gdk_x11_image_get_ximage(image:PGdkImage): PXImage;
{$endif}


{$ifdef UseXinerama}
  // Xinerama
  function GetFirstScreen: Boolean;
{$endif}

var
  FirstScreen: TPoint;

procedure laz_gdk_gc_set_dashes(gc:PGdkGC; dash_offset:gint;
  dashlist:Pgint8; n:gint); cdecl; external gdkdll name 'gdk_gc_set_dashes';
  
