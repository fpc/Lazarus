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

  function gdk_window_xwindow(win : PGdkDrawable): TXID; external gdklib name 'gdk_x11_drawable_get_xid';

{$endif}

function gdk_screen_get_default: PGdkScreen; cdecl; external gdklib;

procedure laz_gdk_gc_set_dashes(gc:PGdkGC; dash_offset:gint;
  dashlist:Pgint8; n:gint); cdecl; external gdkdll name 'gdk_gc_set_dashes';

// gtk 2.2
procedure gdk_display_get_pointer(display : PGdkDisplay; screen :PGdkScreen; x :Pgint; y : Pgint; mask : PGdkModifierType); cdecl; external gdklib;
function gdk_display_get_default:PGdkDisplay; cdecl; external gdklib;

procedure gdk_draw_pixbuf(drawable : PGdkDrawable; gc : PGdkGC; pixbuf : PGdkPixbuf; src_x, src_y, dest_x, dest_y, width, height : gint;
                                             dither : TGdkRgbDither; x_dither, y_dither : gint); cdecl; external gdklib;
// gtk 2.4
function gdk_cursor_new_from_pixbuf(display: PGdkDisplay; pixbuf: PGdkPixbuf; x, y: gint): PGdkCursor; cdecl; external gdklib name 'gdk_cursor_new_from_pixbuf';

