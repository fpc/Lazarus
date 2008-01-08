{%MainUnit gtkextra.pp}
{ $Id$ }
{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

uses
  {$ifdef HasX}
  xlib, x,
  {$endif}
  glib2, gdk2pixbuf, gdk2, gtk2,
  {$ifdef HasGdk2X}
  gdk2x,
  {$endif}
  Pango;


// still needed ??
{$ifdef windows}
procedure gdk_window_copy_area(Dest: PGDKWindow; GC: PGDKGC;
  DestX, DestY: Longint; SRC: PGDKWindow; XSRC, YSRC, Width, Height: Longint);
{$endif}


const
  gdkdll = gdklib;

// missing declarations in fpc 2.0
{$if defined(HasX) and not defined(HasGdk2X)}
var
  gdk_display: PDisplay; external gdklib name 'gdk_display';

function gdk_window_xwindow(win : PGdkDrawable): TXID; cdecl; external gdklib name 'gdk_x11_drawable_get_xid';
function gdk_x11_image_get_ximage(image:PGdkImage):PXImage;cdecl;external gdklib name 'gdk_x11_image_get_ximage';

{$endif}

procedure laz_gdk_gc_set_dashes(gc:PGdkGC; dash_offset:gint;
  dashlist:Pgint8; n:gint); cdecl; external gdkdll name 'gdk_gc_set_dashes';

type
  PPPangoAttrList = ^PPangoAttrList;
procedure gtk_im_context_get_preedit_string_laz(context:PGtkIMContext; str:PPgchar; attrs:PPPangoAttrList; cursor_pos:Pgint); cdecl; external gtklib name 'gtk_im_context_get_preedit_string';
function gdk_event_new(_type: TGdkEventType): PGdkEvent; cdecl; external gdklib;


// gtk 2.4
procedure gtk_toolbar_insert(toolbar: PGtkToolBar; item: PGtkToolItem; pos: gint); cdecl; external gtklib name 'gtk_toolbar_insert';
procedure gtk_event_box_set_visible_window(event_box: PGtkEventBox; visible_window: gboolean); cdecl; external gtklib name 'gtk_event_box_set_visible_window';
procedure gtk_event_box_set_above_child(event_box: PGtkEventBox; visible_window: gboolean); cdecl; external gtklib name 'gtk_event_box_set_above_child';

// gtk 2.6
function gtk_cell_view_get_displayed_row(cell_view: Pointer): PGtkTreePath; cdecl; external gtklib name 'gtk_cell_view_get_displayed_row';


// gdk 2.2
procedure gdk_display_get_pointer(display : PGdkDisplay; screen :PGdkScreen; x :Pgint; y : Pgint; mask : PGdkModifierType); cdecl; external gdklib;
function gdk_display_get_default:PGdkDisplay; cdecl; external gdklib;

procedure gdk_draw_pixbuf(drawable : PGdkDrawable; gc : PGdkGC; pixbuf : PGdkPixbuf; src_x, src_y, dest_x, dest_y, width, height : gint;
                                             dither : TGdkRgbDither; x_dither, y_dither : gint); cdecl; external gdklib;
function gdk_screen_get_default: PGdkScreen; cdecl; external gdklib;
function gdk_screen_get_rgb_colormap(screen: PGdkScreen): PGdkColormap; cdecl; external gdklib;

// gdk 2.4
function gdk_cursor_new_from_pixbuf(display: PGdkDisplay; pixbuf: PGdkPixbuf; x, y: gint): PGdkCursor; cdecl; external gdklib name 'gdk_cursor_new_from_pixbuf';
function gdk_display_get_default_cursor_size(display: PGdkDisplay): guint; cdecl; external gdklib;
procedure gdk_display_get_maximal_cursor_size(display: PGdkDisplay; w, h: pguint); cdecl; external gdklib;

{$ifdef GTK_2_8}
// gdk 2.8
procedure gdk_display_warp_pointer(display: PGdkDisplay; screen: PGdkScreen; x, y: gint); cdecl; external gdklib;
function gdk_screen_get_rgba_colormap(screen: PGdkScreen): PGdkColormap; cdecl; external gdklib;
{$endif}
{ for future use
function gdk_display_supports_cursor_alpha(display: PGdkDisplay): gBoolean; cdecl; external gdklib;
function gdk_display_supports_cursor_color(display: PGdkDisplay): gBoolean; cdecl; external gdklib;
}

// pango 1.6
type
   PPangoGravity = ^TPangoGravity;
   TPangoGravity = (
     PANGO_GRAVITY_SOUTH,
     PANGO_GRAVITY_EAST,
     PANGO_GRAVITY_NORTH,
     PANGO_GRAVITY_WEST,
     PANGO_GRAVITY_AUTO
   );
   
   PPangoMatrix = ^TPangoMatrix;
   TPangoMatrix = record
     xx: double;
     xy: double;
     yx: double;
     yy: double;
     x0: double;
     y0: double;
   end;

function pango_context_get_gravity(Context: PPangoContext): TPangoGravity; cdecl; external pangolib;
function pango_context_get_matrix(Context: PPangoContext): PPangoMatrix; cdecl; external pangolib;

// pango 1.16
function pango_version_check(required_major, required_minor, required_micro: integer): PChar; cdecl; external pangolib;

{$if defined(VER2_0) or defined(VER2_2_0)}
function pango_layout_get_font_description(layout:PPangoLayout):PPangoFontDescription; cdecl; external pangolib;
{$ifend}
