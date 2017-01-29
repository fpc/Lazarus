{
    Pango
    pangocairo.h:
    
    Copyright (C) 1999, 2004 Red Hat, Inc.
    
    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
    Library General Public License for more details.
    
    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the
    Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
    Boston, MA 02110-1335, USA.
}

unit LazPangoCairo1;

{$mode objfpc}
{$H+}

interface
uses LazGlib2, LazPango1, LazCairo1, LazGObject2;

const
{$ifdef win32}
  {$define pangowin}
  pangocairolib = 'libpangocairo-1.0-0.dll';
  {$IFDEF FPC}
    {$ifndef NO_SMART_LINK}
      {$smartlink on}
    {$endif}
  {$ENDIF}
{$else}
  {$ifdef UseCustomLibs}
  pangocairolib = '';
  {$else}
  pangocairolib = 'libpangocairo-1.0.so.0';
  {$endif}
{$endif}

{$PACKRECORDS C}

Type
  PPangoCairoFont = pointer;   
  PPangoCairoFontMap = pointer;
  PangoCairoShapeRendererFunc = procedure (cr:Pcairo_t; attr:PPangoAttrShape; do_path:gboolean; data:gpointer);cdecl;
  GType = TGType;
  GDestroyNotify = TGDestroyNotify;

function PANGO_TYPE_CAIRO_FONT : GType;
function PANGO_CAIRO_FONT(objekt : pointer) : PPangoCairoFont;    
function PANGO_IS_CAIRO_FONT(objekt : pointer) : boolean;    

function PANGO_TYPE_CAIRO_FONT_MAP : GType;
function PANGO_CAIRO_FONT_MAP(objekt : pointer) : PPangoCairoFontMap;    
function PANGO_IS_CAIRO_FONT_MAP(objekt : pointer) : boolean;    

function pango_cairo_font_map_get_type():GType;cdecl;external pangocairolib;
function pango_cairo_font_map_new:PPangoFontMap;cdecl;external pangocairolib;
function pango_cairo_font_map_new_for_font_type(fonttype:cairo_font_type_t):PPangoFontMap;cdecl;external pangocairolib;
function pango_cairo_font_map_get_default:PPangoFontMap;cdecl;external pangocairolib;
function pango_cairo_font_map_get_font_type(fontmap:PPangoCairoFontMap):cairo_font_type_t;cdecl;external pangocairolib;
procedure pango_cairo_font_map_set_resolution(fontmap:PPangoCairoFontMap; dpi:double);cdecl;external pangocairolib;
function pango_cairo_font_map_get_resolution(fontmap:PPangoCairoFontMap):double;cdecl;external pangocairolib;

{$ifndef PANGO_DISABLE_DEPRECATED}
function pango_cairo_font_map_create_context(fontmap:PPangoCairoFontMap):PPangoContext;cdecl;external pangocairolib;
{$endif}
    
function pango_cairo_font_get_type():GType;cdecl;external pangocairolib;
function pango_cairo_font_get_scaled_font(font:PPangoCairoFont):Pcairo_scaled_font_t;cdecl;external pangocairolib;
procedure pango_cairo_update_context(cr:Pcairo_t; context:PPangoContext);cdecl;external pangocairolib;
procedure pango_cairo_context_set_font_options(context:PPangoContext; options:Pcairo_font_options_t);cdecl;external pangocairolib;
function pango_cairo_context_get_font_options(context:PPangoContext):Pcairo_font_options_t;cdecl;external pangocairolib;
procedure pango_cairo_context_set_resolution(context:PPangoContext; dpi:double);cdecl;external pangocairolib;
function pango_cairo_context_get_resolution(context:PPangoContext):double;cdecl;external pangocairolib;
procedure pango_cairo_context_set_shape_renderer(context:PPangoContext; func:PangoCairoShapeRendererFunc; data:gpointer; dnotify:GDestroyNotify);cdecl;external pangocairolib;
function pango_cairo_context_get_shape_renderer(context:PPangoContext; data:Pgpointer):PangoCairoShapeRendererFunc;cdecl;external pangocairolib;
function pango_cairo_create_layout(cr:Pcairo_t):PPangoLayout;cdecl;external pangocairolib;
procedure pango_cairo_update_layout(cr:Pcairo_t; layout:PPangoLayout);cdecl;external pangocairolib;
procedure pango_cairo_show_glyph_string(cr:Pcairo_t; font:PPangoFont; glyphs:PPangoGlyphString);cdecl;external pangocairolib;
procedure pango_cairo_show_layout_line(cr:Pcairo_t; line:PPangoLayoutLine);cdecl;external pangocairolib;
procedure pango_cairo_show_layout(cr:Pcairo_t; layout:PPangoLayout);cdecl;external pangocairolib;
procedure pango_cairo_show_error_underline(cr:Pcairo_t; x:double; y:double; width:double; height:double);cdecl;external pangocairolib;
procedure pango_cairo_glyph_string_path(cr:Pcairo_t; font:PPangoFont; glyphs:PPangoGlyphString);cdecl;external pangocairolib;
procedure pango_cairo_layout_line_path(cr:Pcairo_t; line:PPangoLayoutLine);cdecl;external pangocairolib;
procedure pango_cairo_layout_path(cr:Pcairo_t; layout:PPangoLayout);cdecl;external pangocairolib;
procedure pango_cairo_error_underline_path(cr:Pcairo_t; x:double; y:double; width:double; height:double);cdecl;external pangocairolib;

procedure pango_cairo_font_map_set_default(fontmap:PPangoCairoFontMap);cdecl;external pangocairolib;
function pango_cairo_create_context(cr:Pcairo_t):PPangoContext;cdecl;external pangocairolib;
procedure pango_cairo_show_glyph_item(cr:Pcairo_t; text:Pchar; glyph_item:PPangoGlyphItem);cdecl;external pangocairolib;


implementation

function PANGO_TYPE_CAIRO_FONT : GType;
begin
  PANGO_TYPE_CAIRO_FONT:=pango_cairo_font_get_type;
end;

function PANGO_CAIRO_FONT(objekt : pointer) : PPangoCairoFont;
begin
  if g_type_check_instance_is_a(PGTypeInstance(Objekt), pango_cairo_font_get_type) then
    Result := PPangoCairoFont(objekt)
  else
    Result := nil;
end;

function PANGO_IS_CAIRO_FONT(objekt : pointer) : boolean;
begin
  Result := g_type_check_instance_is_a(PGTypeInstance(Objekt), pango_cairo_font_get_type);
end;

function PANGO_TYPE_CAIRO_FONT_MAP : GType;
begin
  PANGO_TYPE_CAIRO_FONT_MAP:=pango_cairo_font_map_get_type;
end;

function PANGO_CAIRO_FONT_MAP(objekt : pointer) : PPangoCairoFontMap;
begin
  PANGO_CAIRO_FONT_MAP:=G_TYPE_CHECK_INSTANCE_CAST(objekt,PANGO_TYPE_CAIRO_FONT_MAP);
end;

function PANGO_IS_CAIRO_FONT_MAP(objekt : pointer) : boolean;
begin
  Result := g_type_check_instance_is_a(PGTypeInstance(Objekt), pango_cairo_font_map_get_type);
end;


end.
