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
{$ifndef HasGdk2X}
var
  gdk_display: PDisplay; external gdklib name 'gdk_display';

  function gdk_window_xwindow(win : PGdkDrawable): TXID; external gdklib name 'gdk_x11_drawable_get_xid';

{$endif}

function gdk_screen_get_default: PGdkScreen; cdecl; external gdklib;

procedure laz_gdk_gc_set_dashes(gc:PGdkGC; dash_offset:gint;
  dashlist:Pgint8; n:gint); cdecl; external gdkdll name 'gdk_gc_set_dashes';



