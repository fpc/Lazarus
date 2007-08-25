{ $Id$ }
{
                       ----------------------------------
                       gtkdebug.pp  -  graphic dump utils 
                       ----------------------------------
 
 @created(Wed May 10th WET 2007)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)                       

 This unit contains utility functions to show the contents of graphics
 
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

unit GtkDebug;

{$mode objfpc}{$H+}

interface 

uses
  {$IFDEF gtk2}
  glib2, gdk2pixbuf, gdk2, gtk2,
  {$ELSE}
  glib, gdk, gtk, gdkpixbuf,
  {$ENDIF}
  sysutils;

procedure DbgDumpBitmap(ABitmap: PGdkBitmap; ATitle: String = ''; AWidth: Integer = -1; AHeight: Integer = -1);
procedure DbgDumpPixmap(APixmap: PGdkPixmap; ATitle: String = ''; AWidth: Integer = -1; AHeight: Integer = -1);
procedure DbgDumpPixbuf(APixbuf: PGdkPixbuf; ATitle: String = ''; AWidth: Integer = -1; AHeight: Integer = -1);

implementation

type
  TDbgDumpType = (ddtBitmap, ddtPixmap, ddtPixbuf);

  PDbgDumpInfo = ^TDbgDumpInfo;
  TDbgDumpInfo = record                            
    Width, Height: Integer;
    case DumpType: TDbgDumpType of
      ddtBitmap: (Bitmap: PGdkBitmap);
      ddtPixmap: (Pixmap: PGdkPixmap);
      ddtPixbuf: (Pixbuf: PGdkPixbuf);
  end;
                    
procedure OnDbgWindowDestroy(widget: PGtkWidget; Data: Pointer); cdecl;
var
  Info: PDbgDumpInfo absolute Data;
begin
  case Info^.DumpType of
    ddtBitmap: if Info^.Bitmap <> nil then  gdk_pixmap_unref(Info^.Bitmap);
    ddtPixmap: if Info^.Pixmap <> nil then gdk_pixmap_unref(Info^.Pixmap);
    ddtPixbuf: if Info^.Pixbuf <> nil then gdk_pixbuf_unref(Info^.Pixbuf);
  end;
  Dispose(Info);
end;
                    
procedure OnDbgDrawAreaExpose(widget: PGtkWidget; event: PGdkEventExpose; Data: Pointer); cdecl;
var
  Info: PDbgDumpInfo absolute Data;
  gc: Pointer;
  color: TGdkColor;
begin
  gc := gdk_gc_new(widget^.window);

  case Info^.DumpType of
    ddtBitmap: begin
      if Info^.Bitmap = nil
      then color.pixel := $808080
      else color.pixel := 0;
      gdk_gc_set_foreground(gc, @color);
      gdk_draw_rectangle(widget^.window, gc, 1, 0, 0, Info^.Width, Info^.Height);
    
      if Info^.Bitmap <> nil
      then begin
        gdk_gc_set_clip_mask(gc, Info^.Bitmap);
        color.pixel := $FFFFFF;
        gdk_gc_set_foreground(gc, @color);
        gdk_draw_rectangle(widget^.window, gc, 1, 0, 0, Info^.Width, Info^.Height);
      end;
    end;
    ddtPixmap: begin
      if Info^.Pixmap <> nil
      then gdk_draw_pixmap(widget^.window, gc, Info^.Pixmap, 0, 0, 0, 0, Info^.Width, Info^.Height);
    end;
    ddtPixbuf: begin
      if Info^.Pixbuf <> nil
      then gdk_pixbuf_render_to_drawable_alpha(Info^.Pixbuf, widget^.window, 0, 0, 0, 0, Info^.Width, Info^.Height, GDK_PIXBUF_ALPHA_BILEVEL, $80, GDK_RGB_DITHER_NORMAL, 0, 0);
    end;
  end;
  
  gdk_gc_destroy(gc);
end;       

procedure DbgCreateWindow(AInfo: PDbgDumpInfo; const ATitle: String);
var
  window, darea: Pointer;
begin
  window := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(window, PChar(ATitle));
  gtk_window_set_default_size(window, AInfo^.Width, AInfo^.Height);
  
  darea := gtk_drawing_area_new;
  gtk_drawing_area_size (darea, AInfo^.Width, AInfo^.Height);
  gtk_container_add(window, darea);

  gtk_signal_connect(darea, 'expose-event', TGTKSignalFunc(@OnDbgDrawAreaExpose), AInfo);
  gtk_signal_connect(window, 'destroy', TGTKSignalFunc(@OnDbgWindowDestroy), AInfo);

  gtk_widget_show_all(window);
end;


procedure DbgDumpBitmap(ABitmap: PGdkBitmap; ATitle: String = ''; AWidth: Integer = -1; AHeight: Integer = -1);
var
  Info: PDbgDumpInfo;
  h,w,d: Integer;
begin
  New(Info);
  if ABitmap = nil
  then begin
    w := 0; h:= 0; d := 0;
  end
  else gdk_window_get_geometry(ABitmap, nil,nil,@w,@h,@d);
  
  if AWidth = -1 then AWidth := W;
  if AHeight = -1 then AHeight := H;
  Info^.Width := AWidth;
  Info^.Height := AHeight;
  if d = 1
  then begin
    Info^.DumpType := ddtBitmap;
    Info^.Bitmap := ABitmap;
  end
  else begin
    // got a pixmap as bitmap
    Info^.DumpType := ddtPixmap;
    Info^.Pixmap := ABitmap;
  end;
  gdk_pixmap_ref(ABitmap);

  ATitle := ATitle + Format(' (Bitmap:$%p W:%d H:%d D:%d)', [ABitmap, w, h, d]);
  DbgCreateWindow(Info, ATitle);
end;

procedure DbgDumpPixmap(APixmap: PGdkPixmap; ATitle: String = ''; AWidth: Integer = -1; AHeight: Integer = -1);
var
  Info: PDbgDumpInfo;
  h,w,d: Integer;
begin
  New(Info);
  if APixmap = nil
  then begin
    w := 0; h:= 0; d := 0;
  end
  else gdk_window_get_geometry(APixmap, nil,nil,@w,@h,@d);
  if AWidth = -1 then AWidth := W;
  if AHeight = -1 then AHeight := H;
  Info^.Width := AWidth;
  Info^.Height := AHeight;
  if d = 1
  then begin
    // got a bitmap as pixmap
    Info^.DumpType := ddtBitmap;
    Info^.Bitmap := APixmap;
  end
  else begin
    Info^.DumpType := ddtPixmap;
    Info^.Pixmap := APixmap;
  end;
  gdk_pixmap_ref(APixmap);

  ATitle := ATitle + Format(' (Pixmap:$%p W:%d H:%d D:%d)', [APixmap, w, h, d]);
  DbgCreateWindow(Info, ATitle);
end;

procedure DbgDumpPixbuf(APixbuf: PGdkPixbuf; ATitle: String = ''; AWidth: Integer = -1; AHeight: Integer = -1);
var
  Info: PDbgDumpInfo;
  h,w,c: Integer;
begin
  New(Info);
  c := gdk_pixbuf_get_n_channels(APixbuf);
  w := gdk_pixbuf_get_width(APixbuf);
  h := gdk_pixbuf_get_height(APixbuf);

  if AWidth = -1 then AWidth := W;
  if AHeight = -1 then AHeight := H;
  Info^.Width := AWidth;
  Info^.Height := AHeight;
  Info^.DumpType := ddtPixbuf;
  Info^.Pixbuf := APixbuf;
  gdk_pixbuf_ref(APixbuf);

  ATitle := ATitle + Format(' (Pixbuf:$%p W:%d H:%d C:%d)', [APixbuf, w, h, c]);
  DbgCreateWindow(Info, ATitle);
end;

end.
