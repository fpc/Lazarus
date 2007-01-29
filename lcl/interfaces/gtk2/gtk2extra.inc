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

{$ifdef windows}
procedure gdk_window_copy_area(Dest : PGDKWindow; GC : PGDKGC; DestX,
  DestY : Longint; SRC : PGDKWindow; XSRC, YSRC, Width, Height : Longint);
begin
  gdk_draw_pixmap(Dest, GC, Src, XSrc, YSrc, DestX, DestY, Width, Height);
end;
{$endif}

