unit LazCairo1;

(* cairo - a vector graphics library with display and print output
 *
 * Copyright Â© 2002 University of Southern California
 * Copyright Â© 2005 Red Hat, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it either under the terms of the GNU Lesser General Public
 * License version 2.1 as published by the Free Software Foundation
 * (the "LGPL") or, at your option, under the terms of the Mozilla
 * Public License Version 1.1 (the "MPL"). If you do not alter this
 * notice, a recipient may use your version of this file under either
 * the MPL or the LGPL.
 *
 * You should have received a copy of the LGPL along with this library
 * in the file COPYING-LGPL-2.1; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * You should have received a copy of the MPL along with this library
 * in the file COPYING-MPL-1.1
 *
 * The contents of this file are subject to the Mozilla Public License
 * Version 1.1 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * This software is distributed on an "AS IS" basis, WITHOUT WARRANTY
 * OF ANY KIND, either express or implied. See the LGPL or the MPL for
 * the specific language governing rights and limitations.
 *
 * The Original Code is the cairo graphics library.
 *
 * The Initial Developer of the Original Code is University of Southern
 * California.
 *
 * Contributor(s):
 *	Carl D. Worth <cworth@cworth.org>

 *  This FreePascal binding generated August 26, 2005 
 *  by Jeffrey Pohlmeyer <yetanothergeek@yahoo.com>

  - Updated to cairo version 1.4
  - Grouped OS specific fuctions in separated units
  - Organized the functions by group and ordered exactly as the c header
  - Cleared parameter list syntax according to pascal standard

  By Luiz Américo Pereira Câmara
  October 2007
*)

{$mode ObjFpc}

interface

Uses
  CTypes;

const
{$ifdef win32}
  LIB_CAIRO = 'libcairo-2.dll';
  {$IFDEF FPC}
    {$ifndef NO_SMART_LINK}
      {$smartlink on}
    {$endif}
  {$ENDIF}
{$else}
  {$ifdef darwin}
    LIB_CAIRO = 'cairo';
    {$linklib cairo}
  {$else}
    {$ifdef UseCustomLibs}
    LIB_CAIRO = '';
    {$else}
    LIB_CAIRO = 'libcairo.so.2';
    {$endif}
  {$endif}
{$endif}

{$IFDEF FPC}
  {$PACKRECORDS C}
{$ENDIF}

type
  cairo_status_t = (
    CAIRO_STATUS_SUCCESS = 0,
    CAIRO_STATUS_NO_MEMORY,
    CAIRO_STATUS_INVALID_RESTORE,
    CAIRO_STATUS_INVALID_POP_GROUP,
    CAIRO_STATUS_NO_CURRENT_POINT,
    CAIRO_STATUS_INVALID_MATRIX,
    CAIRO_STATUS_INVALID_STATUS,
    CAIRO_STATUS_NULL_POINTER,
    CAIRO_STATUS_INVALID_STRING,
    CAIRO_STATUS_INVALID_PATH_DATA,
    CAIRO_STATUS_READ_ERROR,
    CAIRO_STATUS_WRITE_ERROR,
    CAIRO_STATUS_SURFACE_FINISHED,
    CAIRO_STATUS_SURFACE_TYPE_MISMATCH,
    CAIRO_STATUS_PATTERN_TYPE_MISMATCH,
    CAIRO_STATUS_INVALID_CONTENT,
    CAIRO_STATUS_INVALID_FORMAT,
    CAIRO_STATUS_INVALID_VISUAL,
    CAIRO_STATUS_FILE_NOT_FOUND,
    CAIRO_STATUS_INVALID_DASH
  );

  cairo_operator_t = (
    CAIRO_OPERATOR_CLEAR,
    CAIRO_OPERATOR_SOURCE,
    CAIRO_OPERATOR_OVER,
    CAIRO_OPERATOR_IN,
    CAIRO_OPERATOR_OUT,
    CAIRO_OPERATOR_ATOP,
    CAIRO_OPERATOR_DEST,
    CAIRO_OPERATOR_DEST_OVER,
    CAIRO_OPERATOR_DEST_IN,
    CAIRO_OPERATOR_DEST_OUT,
    CAIRO_OPERATOR_DEST_ATOP,
    CAIRO_OPERATOR_XOR,
    CAIRO_OPERATOR_ADD,
    CAIRO_OPERATOR_SATURATE
  );

  cairo_antialias_t = (
    CAIRO_ANTIALIAS_DEFAULT,
    CAIRO_ANTIALIAS_NONE,
    CAIRO_ANTIALIAS_GRAY,
    CAIRO_ANTIALIAS_SUBPIXEL
  );

  cairo_fill_rule_t = (
    CAIRO_FILL_RULE_WINDING,
    CAIRO_FILL_RULE_EVEN_ODD
  );

  cairo_line_cap_t = (
    CAIRO_LINE_CAP_BUTT,
    CAIRO_LINE_CAP_ROUND,
    CAIRO_LINE_CAP_SQUARE
  );

  cairo_line_join_t = (
    CAIRO_LINE_JOIN_MITER,
    CAIRO_LINE_JOIN_ROUND,
    CAIRO_LINE_JOIN_BEVEL
  );

  cairo_font_slant_t = (
    CAIRO_FONT_SLANT_NORMAL,
    CAIRO_FONT_SLANT_ITALIC,
    CAIRO_FONT_SLANT_OBLIQUE
  );

  cairo_font_weight_t = (
    CAIRO_FONT_WEIGHT_NORMAL,
    CAIRO_FONT_WEIGHT_BOLD
  );

  cairo_subpixel_order_t = (
    CAIRO_SUBPIXEL_ORDER_DEFAULT,
    CAIRO_SUBPIXEL_ORDER_RGB,
    CAIRO_SUBPIXEL_ORDER_BGR,
    CAIRO_SUBPIXEL_ORDER_VRGB,
    CAIRO_SUBPIXEL_ORDER_VBGR
  );

  cairo_hint_style_t = (
    CAIRO_HINT_STYLE_DEFAULT,
    CAIRO_HINT_STYLE_NONE,
    CAIRO_HINT_STYLE_SLIGHT,
    CAIRO_HINT_STYLE_MEDIUM,
    CAIRO_HINT_STYLE_FULL
  );

  cairo_hint_metrics_t = (
    CAIRO_HINT_METRICS_DEFAULT,
    CAIRO_HINT_METRICS_OFF,
    CAIRO_HINT_METRICS_ON
  );

  cairo_path_data_type_t = (
    CAIRO_PATH_MOVE_TO,
    CAIRO_PATH_LINE_TO,
    CAIRO_PATH_CURVE_TO,
    CAIRO_PATH_CLOSE_PATH
  );

  cairo_content_t = (
    CAIRO_CONTENT_COLOR = $1000,
    CAIRO_CONTENT_ALPHA = $2000,
    CAIRO_CONTENT_COLOR_ALPHA = $3000
  );

  Tcairo_content_t = cairo_content_t;

  cairo_format_t = (
    // CAIRO_FORMAT_INVALID   = -1,
    CAIRO_FORMAT_ARGB32,
    CAIRO_FORMAT_RGB24,
    CAIRO_FORMAT_A8,
    CAIRO_FORMAT_A1
    // CAIRO_FORMAT_RGB16_565
    // CAIRO_FORMAT_RGB30
  );

  cairo_extend_t = (
    CAIRO_EXTEND_NONE,
    CAIRO_EXTEND_REPEAT,
    CAIRO_EXTEND_REFLECT,
    CAIRO_EXTEND_PAD
  );

  cairo_filter_t = (
    CAIRO_FILTER_FAST,
    CAIRO_FILTER_GOOD,
    CAIRO_FILTER_BEST,
    CAIRO_FILTER_NEAREST,
    CAIRO_FILTER_BILINEAR,
    CAIRO_FILTER_GAUSSIAN
  );

  cairo_font_type_t = (
    CAIRO_FONT_TYPE_TOY,
    CAIRO_FONT_TYPE_FT,
    CAIRO_FONT_TYPE_WIN32,
    CAIRO_FONT_TYPE_ATSUI
  );
  
  cairo_pattern_type_t = (
    CAIRO_PATTERN_TYPE_SOLID,
    CAIRO_PATTERN_TYPE_SURFACE,
    CAIRO_PATTERN_TYPE_LINEAR,
    CAIRO_PATTERN_TYPE_RADIAL,
    CAIRO_PATTERN_TYPE_MESH,
    CAIRO_PATTERN_TYPE_RASTER_SOURCE
  );
  
  cairo_surface_type_t = (
    CAIRO_SURFACE_TYPE_IMAGE,
    CAIRO_SURFACE_TYPE_PDF,
    CAIRO_SURFACE_TYPE_PS,
    CAIRO_SURFACE_TYPE_XLIB,
    CAIRO_SURFACE_TYPE_XCB,
    CAIRO_SURFACE_TYPE_GLITZ,
    CAIRO_SURFACE_TYPE_QUARTZ,
    CAIRO_SURFACE_TYPE_WIN32,
    CAIRO_SURFACE_TYPE_BEOS,
    CAIRO_SURFACE_TYPE_DIRECTFB,
    CAIRO_SURFACE_TYPE_SVG,
    CAIRO_SURFACE_TYPE_OS2,
    CAIRO_SURFACE_TYPE_WIN32_PRINTING,
    CAIRO_SURFACE_TYPE_QUARTZ_IMAGE,
    CAIRO_SURFACE_TYPE_SCRIPT,
    CAIRO_SURFACE_TYPE_QT,
    CAIRO_SURFACE_TYPE_RECORDING,
    CAIRO_SURFACE_TYPE_VG,
    CAIRO_SURFACE_TYPE_GL,
    CAIRO_SURFACE_TYPE_DRM,
    CAIRO_SURFACE_TYPE_TEE,
    CAIRO_SURFACE_TYPE_XML,
    CAIRO_SURFACE_TYPE_SKIA,
    CAIRO_SURFACE_TYPE_SUBSURFACE,
    CAIRO_SURFACE_TYPE_COGL
  );

  cairo_device_type_t = (
    CAIRO_DEVICE_TYPE_DRM,
    CAIRO_DEVICE_TYPE_GL,
    CAIRO_DEVICE_TYPE_SCRIPT,
    CAIRO_DEVICE_TYPE_XCB,
    CAIRO_DEVICE_TYPE_XLIB,
    CAIRO_DEVICE_TYPE_XML,
    CAIRO_DEVICE_TYPE_COGL,
    CAIRO_DEVICE_TYPE_WIN32,
    CAIRO_DEVICE_TYPE_INVALID = -1
  );
  
  cairo_svg_version_t = (
    CAIRO_SVG_VERSION_1_1,
    CAIRO_SVG_VERSION_1_2
  );

  cairo_region_overlap_t = (
      CAIRO_REGION_OVERLAP_IN,		// completely inside region
      CAIRO_REGION_OVERLAP_OUT,		// completely outside region
      CAIRO_REGION_OVERLAP_PART		// partly inside region
  );

  pcairo_svg_version_t = ^cairo_svg_version_t;
  ppcairo_svg_version_t = pcairo_svg_version_t;

  Pcairo_surface_t = ^cairo_surface_t;
  PPcairo_surface_t = ^Pcairo_surface_t;
  Pcairo_t = ^cairo_t;
  Pcairo_pattern_t = ^cairo_pattern_t;
  Pcairo_font_options_t = ^cairo_font_options_t;
  Pcairo_font_face_t = ^cairo_font_face_t;
  Pcairo_scaled_font_t = ^cairo_scaled_font_t;
  Pcairo_bool_t = ^cairo_bool_t;
  cairo_bool_t = LongInt;
  Pcairo_matrix_t = ^cairo_matrix_t;
  Pcairo_user_data_key_t = ^cairo_user_data_key_t;
  Pcairo_glyph_t = ^cairo_glyph_t;
  Pcairo_text_extents_t = ^cairo_text_extents_t;
  Pcairo_font_extents_t = ^cairo_font_extents_t;
  Pcairo_path_data_type_t = ^cairo_path_data_type_t;
  Pcairo_path_data_t = ^cairo_path_data_t;
  Pcairo_path_t = ^cairo_path_t;
  Pcairo_rectangle_t = ^cairo_rectangle_t;
  Pcairo_rectangle_list_t =^cairo_rectangle_list_t;

  cairo_destroy_func_t = procedure (data: Pointer); cdecl;
  cairo_write_func_t = function (closure: Pointer; data: PByte; length: LongWord): cairo_status_t; cdecl;
  cairo_read_func_t = function (closure: Pointer; data: PByte; length: LongWord): cairo_status_t; cdecl;

  cairo_t              = record {OPAQUE} end;
  cairo_surface_t      = record {OPAQUE} end;
  cairo_pattern_t      = record {OPAQUE} end;
  cairo_scaled_font_t  = record {OPAQUE} end;
  cairo_font_face_t    = record {OPAQUE} end;
  cairo_font_options_t = record {OPAQUE} end;

  cairo_matrix_t = record
    xx : Double;
    yx : Double;
    xy : Double;
    yy : Double;
    x0 : Double;
    y0 : Double;
  end;

  cairo_user_data_key_t = record
    unused : LongInt;
  end;

  cairo_glyph_t = record
    index : LongWord;
    x : Double;
    y : Double;
  end;

  cairo_text_extents_t = record
    x_bearing : Double;
    y_bearing : Double;
    width : Double;
    height : Double;
    x_advance : Double;
    y_advance : Double;
  end;

  cairo_font_extents_t = record
    ascent : Double;
    descent : Double;
    height : Double;
    max_x_advance : Double;
    max_y_advance : Double;
  end;

  cairo_path_data_t = record
    case LongInt of
      0 : ( header : record
            _type : cairo_path_data_type_t;
            length : LongInt;
          end );
      1 : ( point : record
            x : Double;
            y : Double;
          end );
  end;

  cairo_path_t = record
    status : cairo_status_t;
    data : Pcairo_path_data_t;
    num_data : LongInt;
  end;
  
  cairo_rectangle_t = record
    x, y, width, height: Double;
  end;

  PPcairo_pattern_t = ^Pcairo_pattern_t;

  PPcairo_region_t = ^Pcairo_region_t;
  Pcairo_region_t = ^Tcairo_region_t;
  Tcairo_region_t = record {OPAQUE} end;
  Pcairo_device_t = ^Tcairo_device_t;
  Tcairo_device_t = record {OPAQUE} end;

  PPcairo_rectangle_int_t = ^Pcairo_rectangle_int_t;
  Pcairo_rectangle_int_t = ^Tcairo_rectangle_int_t;

  Tcairo_rectangle_int_t = record
    x, y, width, height: cint;
  end;
  
  cairo_rectangle_list_t = record
    status: cairo_status_t;
    rectangles: Pcairo_rectangle_t;
    num_rectangles: LongInt;
  end;
  
function cairo_version: LongInt; cdecl; external LIB_CAIRO;
function cairo_version_string: PChar; cdecl; external LIB_CAIRO;
//Helper function to retrieve decoded version
procedure cairo_version(out major, minor, micro: LongInt);

(* Functions for manipulating state objects *)

function  cairo_create(target: Pcairo_surface_t): Pcairo_t; cdecl; external LIB_CAIRO;
function  cairo_reference(cr: Pcairo_t): Pcairo_t; cdecl; external LIB_CAIRO;
procedure cairo_destroy(cr: Pcairo_t); cdecl; external LIB_CAIRO;
function  cairo_get_reference_count(cr: Pcairo_t): LongWord; cdecl; external LIB_CAIRO;
function  cairo_get_user_data(cr: Pcairo_t; key: Pcairo_user_data_key_t): pointer; cdecl; external LIB_CAIRO;
function  cairo_set_user_data(cr:  PCairo_t; key:  Pcairo_user_data_key_t; user_data:  Pointer; destroy:  cairo_destroy_func_t):  cairo_status_t; cdecl; external LIB_CAIRO;
procedure cairo_save(cr: Pcairo_t); cdecl; external LIB_CAIRO;
procedure cairo_restore(cr: Pcairo_t); cdecl; external LIB_CAIRO;
procedure cairo_push_group (cr:  PCairo_t); cdecl; external LIB_CAIRO;
procedure cairo_push_group_with_content (cr:  PCairo_t; content:  cairo_content_t); cdecl; external LIB_CAIRO;
function  cairo_pop_group (cr:  PCairo_t):  Pcairo_pattern_t; cdecl; external LIB_CAIRO;
procedure cairo_pop_group_to_source (cr:  PCairo_t); cdecl; external LIB_CAIRO;

(* Modify state *)

procedure cairo_set_operator(cr: Pcairo_t; op: cairo_operator_t); cdecl; external LIB_CAIRO;
procedure cairo_set_source(cr: Pcairo_t; source: Pcairo_pattern_t); cdecl; external LIB_CAIRO;
procedure cairo_set_source_rgb(cr: Pcairo_t; red, green, blue: Double); cdecl; external LIB_CAIRO;
procedure cairo_set_source_rgba(cr: Pcairo_t; red, green, blue, alpha: Double); cdecl; external LIB_CAIRO;
procedure cairo_set_source_surface(cr: Pcairo_t; surface: Pcairo_surface_t; x, y: Double); cdecl; external LIB_CAIRO;
procedure cairo_set_tolerance(cr: Pcairo_t; tolerance: Double); cdecl; external LIB_CAIRO;
procedure cairo_set_antialias(cr: Pcairo_t; antialias: cairo_antialias_t); cdecl; external LIB_CAIRO;
procedure cairo_set_fill_rule(cr: Pcairo_t; fill_rule: cairo_fill_rule_t); cdecl; external LIB_CAIRO;
procedure cairo_set_line_width(cr: Pcairo_t; width: Double); cdecl; external LIB_CAIRO;
procedure cairo_set_line_cap(cr: Pcairo_t; line_cap: cairo_line_cap_t); cdecl; external LIB_CAIRO;
procedure cairo_set_line_join(cr: Pcairo_t; line_join: cairo_line_join_t); cdecl; external LIB_CAIRO;
procedure cairo_set_dash(cr: Pcairo_t; dashes: PDouble; num_dashes: LongInt; offset: Double); cdecl; external LIB_CAIRO;
procedure cairo_set_miter_limit(cr: Pcairo_t; limit: Double); cdecl; external LIB_CAIRO;
procedure cairo_translate(cr: Pcairo_t; tx, ty: Double); cdecl; external LIB_CAIRO;
procedure cairo_scale(cr: Pcairo_t; sx, sy: Double); cdecl; external LIB_CAIRO;
procedure cairo_rotate(cr: Pcairo_t; angle: Double); cdecl; external LIB_CAIRO;
procedure cairo_transform(cr: Pcairo_t; matrix: Pcairo_matrix_t); cdecl; external LIB_CAIRO;
procedure cairo_set_matrix(cr: Pcairo_t; matrix: Pcairo_matrix_t); cdecl; external LIB_CAIRO;
procedure cairo_identity_matrix(cr: Pcairo_t); cdecl; external LIB_CAIRO;
procedure cairo_user_to_device(cr: Pcairo_t; x, y: PDouble); cdecl; external LIB_CAIRO;
procedure cairo_user_to_device_distance(cr: Pcairo_t; dx, dy: PDouble); cdecl; external LIB_CAIRO;
procedure cairo_device_to_user(cr: Pcairo_t; x, y: PDouble); cdecl; external LIB_CAIRO;
procedure cairo_device_to_user_distance(cr: Pcairo_t; dx, dy: PDouble); cdecl; external LIB_CAIRO;

(* Path creation functions *)

procedure cairo_new_path(cr: Pcairo_t); cdecl; external LIB_CAIRO;
procedure cairo_move_to(cr: Pcairo_t; x, y: Double); cdecl; external LIB_CAIRO;
procedure cairo_new_sub_path(cr: Pcairo_t); cdecl; external LIB_CAIRO;
procedure cairo_line_to(cr: Pcairo_t; x, y: Double); cdecl; external LIB_CAIRO;
procedure cairo_curve_to(cr: Pcairo_t; x1, y1, x2, y2, x3, y3: Double); cdecl; external LIB_CAIRO;
procedure cairo_arc(cr: Pcairo_t; xc, yc, radius, angle1, angle2: Double); cdecl; external LIB_CAIRO;
procedure cairo_arc_negative(cr: Pcairo_t; xc, yc, radius, angle1, angle2: Double); cdecl; external LIB_CAIRO;
procedure cairo_rel_move_to(cr: Pcairo_t; dx, dy: Double); cdecl; external LIB_CAIRO;
procedure cairo_rel_line_to(cr: Pcairo_t; dx, dy: Double); cdecl; external LIB_CAIRO;
procedure cairo_rel_curve_to(cr: Pcairo_t; dx1, dy1, dx2, dy2, dx3, dy3: Double); cdecl; external LIB_CAIRO;
procedure cairo_rectangle(cr: Pcairo_t; x, y, width, height: Double); cdecl; external LIB_CAIRO;
procedure cairo_close_path(cr: Pcairo_t); cdecl; external LIB_CAIRO;

(* Painting functions *)

procedure cairo_paint(cr: Pcairo_t); cdecl; external LIB_CAIRO;
procedure cairo_paint_with_alpha(cr: Pcairo_t; alpha: Double); cdecl; external LIB_CAIRO;
procedure cairo_mask(cr: Pcairo_t; pattern: Pcairo_pattern_t); cdecl; external LIB_CAIRO;
procedure cairo_mask_surface(cr: Pcairo_t; surface: Pcairo_surface_t; surface_x, surface_y: Double); cdecl; external LIB_CAIRO;
procedure cairo_stroke(cr: Pcairo_t); cdecl; external LIB_CAIRO;
procedure cairo_stroke_preserve(cr: Pcairo_t); cdecl; external LIB_CAIRO;
procedure cairo_fill(cr: Pcairo_t); cdecl; external LIB_CAIRO;
procedure cairo_fill_preserve(cr: Pcairo_t); cdecl; external LIB_CAIRO;
procedure cairo_copy_page(cr: Pcairo_t); cdecl; external LIB_CAIRO;
procedure cairo_show_page(cr: Pcairo_t); cdecl; external LIB_CAIRO;

(* Insideness testing *)

function  cairo_in_stroke(cr: Pcairo_t; x, y: Double): cairo_bool_t; cdecl; external LIB_CAIRO;
function  cairo_in_fill(cr: Pcairo_t; x, y: Double): cairo_bool_t; cdecl; external LIB_CAIRO;

(* Rectangular extents *)

procedure cairo_stroke_extents(cr: Pcairo_t; x1, y1, x2, y2: PDouble); cdecl; external LIB_CAIRO;
procedure cairo_fill_extents(cr: Pcairo_t; x1, y1, x2, y2: PDouble); cdecl; external LIB_CAIRO;

(* Clipping *)

procedure cairo_reset_clip(cr: Pcairo_t); cdecl; external LIB_CAIRO;
procedure cairo_clip(cr: Pcairo_t); cdecl; external LIB_CAIRO;
procedure cairo_clip_preserve(cr: Pcairo_t); cdecl; external LIB_CAIRO;
procedure cairo_clip_extents(cr: Pcairo_t; x1, y1, x2, y2:  PDouble); cdecl; external LIB_CAIRO;
function  cairo_copy_clip_rectangle_list(cr: Pcairo_t): Pcairo_rectangle_list_t; cdecl; external LIB_CAIRO;
procedure cairo_rectangle_list_destroy(rectangle_list: Pcairo_rectangle_list_t); cdecl; external LIB_CAIRO;

(* Font/Text functions *)

function  cairo_font_options_create: Pcairo_font_options_t; cdecl; external LIB_CAIRO;
function  cairo_font_options_copy(original: Pcairo_font_options_t): Pcairo_font_options_t; cdecl; external LIB_CAIRO;
procedure cairo_font_options_destroy(options: Pcairo_font_options_t); cdecl; external LIB_CAIRO;
function  cairo_font_options_status(options: Pcairo_font_options_t): cairo_status_t; cdecl; external LIB_CAIRO;
procedure cairo_font_options_merge(options, other: Pcairo_font_options_t); cdecl; external LIB_CAIRO;
function  cairo_font_options_equal(options, other: Pcairo_font_options_t): cairo_bool_t; cdecl; external LIB_CAIRO;
function  cairo_font_options_hash(options: Pcairo_font_options_t): LongWord; cdecl; external LIB_CAIRO;
procedure cairo_font_options_set_antialias(options: Pcairo_font_options_t; antialias: cairo_antialias_t); cdecl; external LIB_CAIRO;
function  cairo_font_options_get_antialias(options: Pcairo_font_options_t): cairo_antialias_t; cdecl; external LIB_CAIRO;
procedure cairo_font_options_set_subpixel_order(options: Pcairo_font_options_t; subpixel_order: cairo_subpixel_order_t); cdecl; external LIB_CAIRO;
function  cairo_font_options_get_subpixel_order(options: Pcairo_font_options_t): cairo_subpixel_order_t; cdecl; external LIB_CAIRO;
procedure cairo_font_options_set_hint_style(options: Pcairo_font_options_t; hint_style: cairo_hint_style_t); cdecl; external LIB_CAIRO;
function  cairo_font_options_get_hint_style(options: Pcairo_font_options_t): cairo_hint_style_t; cdecl; external LIB_CAIRO;
procedure cairo_font_options_set_hint_metrics(options: Pcairo_font_options_t; hint_metrics: cairo_hint_metrics_t); cdecl; external LIB_CAIRO;
function  cairo_font_options_get_hint_metrics(options: Pcairo_font_options_t): cairo_hint_metrics_t; cdecl; external LIB_CAIRO;

(* This interface is for dealing with text as text, not caring about the
   font object inside the the cairo_t. *)

procedure cairo_select_font_face(cr: Pcairo_t; family: Pchar; slant: cairo_font_slant_t; weight: cairo_font_weight_t); cdecl; external LIB_CAIRO;
procedure cairo_set_font_size(cr: Pcairo_t; size: Double); cdecl; external LIB_CAIRO;
procedure cairo_set_font_matrix(cr: Pcairo_t; matrix: Pcairo_matrix_t); cdecl; external LIB_CAIRO;
procedure cairo_get_font_matrix(cr: Pcairo_t; matrix: Pcairo_matrix_t); cdecl; external LIB_CAIRO;
procedure cairo_set_font_options(cr: Pcairo_t; options: Pcairo_font_options_t); cdecl; external LIB_CAIRO;
procedure cairo_get_font_options(cr: Pcairo_t; options: Pcairo_font_options_t); cdecl; external LIB_CAIRO;
procedure cairo_set_font_face(cr: Pcairo_t; font_face: Pcairo_font_face_t); cdecl; external LIB_CAIRO;
function  cairo_get_font_face(cr: Pcairo_t): Pcairo_font_face_t; cdecl; external LIB_CAIRO;
procedure cairo_set_scaled_font(cr: PCairo_t; scaled_font:Pcairo_scaled_font_t); cdecl; external LIB_CAIRO;
function  cairo_get_scaled_font(cr: Pcairo_t): Pcairo_scaled_font_t; cdecl; external LIB_CAIRO;
procedure cairo_show_text(cr: Pcairo_t; utf8: Pchar); cdecl; external LIB_CAIRO;
procedure cairo_show_glyphs(cr: Pcairo_t; glyphs: Pcairo_glyph_t; num_glyphs: LongInt); cdecl; external LIB_CAIRO;
procedure cairo_text_path(cr: Pcairo_t; utf8: Pchar); cdecl; external LIB_CAIRO;
procedure cairo_glyph_path(cr: Pcairo_t; glyphs: Pcairo_glyph_t; num_glyphs: LongInt); cdecl; external LIB_CAIRO;
procedure cairo_text_extents(cr: Pcairo_t; utf8: Pchar; extents: Pcairo_text_extents_t); cdecl; external LIB_CAIRO;
procedure cairo_glyph_extents(cr: Pcairo_t; glyphs: Pcairo_glyph_t; num_glyphs: LongInt; extents: Pcairo_text_extents_t); cdecl; external LIB_CAIRO;
procedure cairo_font_extents(cr: Pcairo_t; extents: Pcairo_font_extents_t); cdecl; external LIB_CAIRO;

(* Generic identifier for a font style *)

function  cairo_font_face_reference(font_face: Pcairo_font_face_t): Pcairo_font_face_t; cdecl; external LIB_CAIRO;
procedure cairo_font_face_destroy(font_face: Pcairo_font_face_t); cdecl; external LIB_CAIRO;
function  cairo_font_face_get_reference_count (font_face: Pcairo_font_face_t):  LongWord; cdecl; external LIB_CAIRO;
function  cairo_font_face_status(font_face: Pcairo_font_face_t): cairo_status_t; cdecl; external LIB_CAIRO;
function  cairo_font_face_get_type(font_face: Pcairo_font_face_t): cairo_font_type_t; cdecl; external LIB_CAIRO;
function  cairo_font_face_get_user_data(font_face: Pcairo_font_face_t; key: Pcairo_user_data_key_t): pointer; cdecl; external LIB_CAIRO;
function  cairo_font_face_set_user_data(font_face: Pcairo_font_face_t; key: Pcairo_user_data_key_t; user_data: pointer; destroy: cairo_destroy_func_t): cairo_status_t; cdecl; external LIB_CAIRO;

(* Portable interface to general font features *)

function  cairo_scaled_font_create(font_face: Pcairo_font_face_t; font_matrix: Pcairo_matrix_t; ctm: Pcairo_matrix_t; options: Pcairo_font_options_t): Pcairo_scaled_font_t; cdecl; external LIB_CAIRO;
function  cairo_scaled_font_reference(scaled_font: Pcairo_scaled_font_t): Pcairo_scaled_font_t; cdecl; external LIB_CAIRO;
procedure cairo_scaled_font_destroy(scaled_font: Pcairo_scaled_font_t); cdecl; external LIB_CAIRO;
function  cairo_scaled_font_get_reference_count (scaled_font: Pcairo_scaled_font_t): LongWord; cdecl; external LIB_CAIRO;
function  cairo_scaled_font_status(scaled_font: Pcairo_scaled_font_t): cairo_status_t; cdecl; external LIB_CAIRO;
function  cairo_scaled_font_get_type(scaled_font: Pcairo_scaled_font_t): cairo_font_type_t; cdecl; external LIB_CAIRO;
function  cairo_scaled_font_get_user_data (scaled_font: Pcairo_scaled_font_t; key: Pcairo_user_data_key_t): Pointer; cdecl; external LIB_CAIRO;
function  cairo_scaled_font_set_user_data (scaled_font: Pcairo_scaled_font_t; key: Pcairo_user_data_key_t; user_data: Pointer; destroy: cairo_destroy_func_t): cairo_status_t; cdecl; external LIB_CAIRO;
procedure cairo_scaled_font_extents(scaled_font: Pcairo_scaled_font_t; extents: Pcairo_font_extents_t); cdecl; external LIB_CAIRO;
procedure cairo_scaled_font_text_extents(scaled_font: Pcairo_scaled_font_t; utf8: PChar; extents: Pcairo_text_extents_t); cdecl; external LIB_CAIRO;
procedure cairo_scaled_font_glyph_extents(scaled_font: Pcairo_scaled_font_t; glyphs: Pcairo_glyph_t; num_glyphs: LongInt; extents: Pcairo_text_extents_t); cdecl; external LIB_CAIRO;
function  cairo_scaled_font_get_font_face (scaled_font: Pcairo_scaled_font_t): Pcairo_font_face_t; cdecl; external LIB_CAIRO;
procedure cairo_scaled_font_get_font_matrix (scaled_font: Pcairo_scaled_font_t;	font_matrix: Pcairo_matrix_t); cdecl; external LIB_CAIRO;
procedure cairo_scaled_font_get_ctm (scaled_font: Pcairo_scaled_font_t;	ctm: Pcairo_matrix_t); cdecl; external LIB_CAIRO;
procedure cairo_scaled_font_get_font_options (scaled_font: Pcairo_scaled_font_t; options: Pcairo_font_options_t); cdecl; external LIB_CAIRO;

(* Query functions *)

function  cairo_get_operator(cr: Pcairo_t): cairo_operator_t; cdecl; external LIB_CAIRO;
function  cairo_get_source(cr: Pcairo_t): Pcairo_pattern_t; cdecl; external LIB_CAIRO;
function  cairo_get_tolerance(cr: Pcairo_t): Double; cdecl; external LIB_CAIRO;
function  cairo_get_antialias(cr: Pcairo_t): cairo_antialias_t; cdecl; external LIB_CAIRO;
procedure cairo_get_current_point(cr: Pcairo_t; x, y: PDouble); cdecl; external LIB_CAIRO;
function  cairo_get_fill_rule(cr: Pcairo_t): cairo_fill_rule_t; cdecl; external LIB_CAIRO;
function  cairo_get_line_width(cr: Pcairo_t): Double; cdecl; external LIB_CAIRO;
function  cairo_get_line_cap(cr: Pcairo_t): cairo_line_cap_t; cdecl; external LIB_CAIRO;
function  cairo_get_line_join(cr: Pcairo_t): cairo_line_join_t; cdecl; external LIB_CAIRO;
function  cairo_get_miter_limit(cr: Pcairo_t): Double; cdecl; external LIB_CAIRO;
function  cairo_get_dash_count(cr: Pcairo_t): LongInt; cdecl; external LIB_CAIRO;
procedure cairo_get_dash(cr: Pcairo_t; dashes, offset: PDouble); cdecl; external LIB_CAIRO;
procedure cairo_get_matrix(cr: Pcairo_t; matrix: Pcairo_matrix_t); cdecl; external LIB_CAIRO;
function  cairo_get_target(cr: Pcairo_t): Pcairo_surface_t; cdecl; external LIB_CAIRO;
function  cairo_get_group_target(cr: Pcairo_t): Pcairo_surface_t; cdecl; external LIB_CAIRO;

function  cairo_copy_path(cr: Pcairo_t): Pcairo_path_t; cdecl; external LIB_CAIRO;
function  cairo_copy_path_flat(cr: Pcairo_t): Pcairo_path_t; cdecl; external LIB_CAIRO;
procedure cairo_append_path(cr: Pcairo_t; path: Pcairo_path_t); cdecl; external LIB_CAIRO;
procedure cairo_path_destroy(path: Pcairo_path_t); cdecl; external LIB_CAIRO;

(* Error status queries *)

function  cairo_status(cr: Pcairo_t): cairo_status_t; cdecl; external LIB_CAIRO;
function  cairo_status_to_string(status: cairo_status_t): Pchar; cdecl; external LIB_CAIRO;

(* Surface manipulation *)

function cairo_surface_create_similar(other: Pcairo_surface_t; content: cairo_content_t; width, height: LongInt): Pcairo_surface_t; cdecl; external LIB_CAIRO;
function cairo_surface_create_for_rectangle(other: Pcairo_surface_t; x, y, width, height: double): Pcairo_surface_t; cdecl; external LIB_CAIRO;
function  cairo_surface_reference(surface: Pcairo_surface_t): Pcairo_surface_t; cdecl; external LIB_CAIRO;
procedure cairo_surface_finish(surface: Pcairo_surface_t); cdecl; external LIB_CAIRO;
procedure cairo_surface_destroy(surface: Pcairo_surface_t); cdecl; external LIB_CAIRO;
function  cairo_surface_get_reference_count(surface: Pcairo_surface_t): LongWord; cdecl; external LIB_CAIRO;
function  cairo_surface_status(surface: Pcairo_surface_t): cairo_status_t; cdecl; external LIB_CAIRO;
function  cairo_surface_get_type(surface: Pcairo_surface_t): cairo_surface_type_t; cdecl; external LIB_CAIRO;
function  cairo_surface_get_content(surface: Pcairo_surface_t): cairo_content_t; cdecl; external LIB_CAIRO;
function  cairo_surface_write_to_png(surface: Pcairo_surface_t; filename: Pchar): cairo_status_t; cdecl; external LIB_CAIRO;
function  cairo_surface_write_to_png_stream(surface: Pcairo_surface_t; write_func: cairo_write_func_t; closure: pointer): cairo_status_t; cdecl; external LIB_CAIRO;
function  cairo_surface_get_user_data(surface: Pcairo_surface_t; key: Pcairo_user_data_key_t): pointer; cdecl; external LIB_CAIRO;
function  cairo_surface_set_user_data(surface: Pcairo_surface_t; key: Pcairo_user_data_key_t; user_data: pointer; destroy: cairo_destroy_func_t): cairo_status_t; cdecl; external LIB_CAIRO;
procedure cairo_surface_get_font_options(surface: Pcairo_surface_t; options: Pcairo_font_options_t); cdecl; external LIB_CAIRO;
procedure cairo_surface_flush(surface: Pcairo_surface_t); cdecl; external LIB_CAIRO;
procedure cairo_surface_mark_dirty(surface: Pcairo_surface_t); cdecl; external LIB_CAIRO;
procedure cairo_surface_mark_dirty_rectangle(surface: Pcairo_surface_t; x, y, width, height: LongInt); cdecl; external LIB_CAIRO;
procedure cairo_surface_set_device_offset(surface: Pcairo_surface_t; x_offset, y_offset: Double); cdecl; external LIB_CAIRO;
procedure cairo_surface_get_device_offset(surface: Pcairo_surface_t; x_offset, y_offset: PDouble); cdecl; external LIB_CAIRO;
procedure cairo_surface_set_fallback_resolution(surface: Pcairo_surface_t; x_pixels_per_inch, y_pixels_per_inch: Double); cdecl; external LIB_CAIRO;
function cairo_surface_get_device(surface: Pcairo_surface_t): Pcairo_device_t; cdecl; external LIB_CAIRO;

{* cairo device *}
function cairo_device_get_type(device: PCairo_device_t): cairo_device_type_t; cdecl; external LIB_CAIRO;

(* Image-surface functions *)

function  cairo_image_surface_create(format: cairo_format_t; width, height: LongInt): Pcairo_surface_t; cdecl; external LIB_CAIRO;
function  cairo_image_surface_create_for_data(data: Pbyte; format: cairo_format_t; width, height, stride: LongInt): Pcairo_surface_t; cdecl; external LIB_CAIRO;
function  cairo_image_surface_get_data(surface: Pcairo_surface_t): PChar; cdecl; external LIB_CAIRO;
function  cairo_image_surface_get_format(surface: Pcairo_surface_t): cairo_format_t; cdecl; external LIB_CAIRO;
function  cairo_image_surface_get_width(surface: Pcairo_surface_t): LongInt; cdecl; external LIB_CAIRO;
function  cairo_image_surface_get_height(surface: Pcairo_surface_t): LongInt; cdecl; external LIB_CAIRO;
function  cairo_image_surface_get_stride(surface: Pcairo_surface_t): LongInt; cdecl; external LIB_CAIRO;
function  cairo_image_surface_create_from_png(filename: Pchar): Pcairo_surface_t; cdecl; external LIB_CAIRO;
function  cairo_image_surface_create_from_png_stream(read_func: cairo_read_func_t; closure: pointer): Pcairo_surface_t; cdecl; external LIB_CAIRO;

(* Pattern creation functions *)

function  cairo_pattern_create_rgb(red, green, blue: Double): Pcairo_pattern_t; cdecl; external LIB_CAIRO;
function  cairo_pattern_create_rgba(red, green, blue, alpha: Double): Pcairo_pattern_t; cdecl; external LIB_CAIRO;
function  cairo_pattern_create_for_surface(surface: Pcairo_surface_t): Pcairo_pattern_t; cdecl; external LIB_CAIRO;
function  cairo_pattern_create_linear(x0, y0, x1, y1: Double): Pcairo_pattern_t; cdecl; external LIB_CAIRO;
function  cairo_pattern_create_radial(cx0, cy0, radius0, cx1, cy1, radius1: Double): Pcairo_pattern_t; cdecl; external LIB_CAIRO;
function  cairo_pattern_reference(pattern: Pcairo_pattern_t): Pcairo_pattern_t; cdecl; external LIB_CAIRO;
procedure cairo_pattern_destroy(pattern: Pcairo_pattern_t); cdecl; external LIB_CAIRO;
function  cairo_pattern_get_reference_count (pattern: Pcairo_pattern_t): LongWord; cdecl; external LIB_CAIRO;
function  cairo_pattern_status(pattern: Pcairo_pattern_t): cairo_status_t; cdecl; external LIB_CAIRO;
function  cairo_pattern_get_user_data (pattern: Pcairo_pattern_t; key: Pcairo_user_data_key_t): Pointer; cdecl; external LIB_CAIRO;
function  cairo_pattern_set_user_data (pattern: Pcairo_pattern_t; key: Pcairo_user_data_key_t; user_data: Pointer; destroy: cairo_destroy_func_t): cairo_status_t; cdecl; external LIB_CAIRO;
function  cairo_pattern_get_type(pattern: Pcairo_pattern_t): cairo_pattern_type_t; cdecl; external LIB_CAIRO;
procedure cairo_pattern_add_color_stop_rgb(pattern: Pcairo_pattern_t; offset, red, green, blue: Double); cdecl; external LIB_CAIRO;
procedure cairo_pattern_add_color_stop_rgba(pattern: Pcairo_pattern_t; offset, red, green, blue, alpha: Double); cdecl; external LIB_CAIRO;
procedure cairo_pattern_set_matrix(pattern: Pcairo_pattern_t; matrix: Pcairo_matrix_t); cdecl; external LIB_CAIRO;
procedure cairo_pattern_get_matrix(pattern: Pcairo_pattern_t; matrix: Pcairo_matrix_t); cdecl; external LIB_CAIRO;
procedure cairo_pattern_set_extend(pattern: Pcairo_pattern_t; extend: cairo_extend_t); cdecl; external LIB_CAIRO;
function  cairo_pattern_get_extend(pattern: Pcairo_pattern_t): cairo_extend_t; cdecl; external LIB_CAIRO;
procedure cairo_pattern_set_filter(pattern: Pcairo_pattern_t; filter: cairo_filter_t); cdecl; external LIB_CAIRO;
function  cairo_pattern_get_filter(pattern: Pcairo_pattern_t): cairo_filter_t; cdecl; external LIB_CAIRO;
function  cairo_pattern_get_rgba (pattern: Pcairo_pattern_t; red, green, blue, alpha: PDouble): cairo_status_t; cdecl; external LIB_CAIRO;
function  cairo_pattern_get_surface (pattern: Pcairo_pattern_t; surface: PPcairo_surface_t): cairo_status_t; cdecl; external LIB_CAIRO;
function  cairo_pattern_get_color_stop_rgba(pattern: Pcairo_pattern_t; index: LongInt; offset, red, green, blue, alpha: PDouble):cairo_status_t; cdecl; external LIB_CAIRO;
function  cairo_pattern_get_color_stop_count(pattern: Pcairo_pattern_t; count: PLongInt):cairo_status_t; cdecl; external LIB_CAIRO;
function  cairo_pattern_get_linear_points (pattern: Pcairo_pattern_t; x0, y0, x1, y1: PDouble): cairo_status_t; cdecl; external LIB_CAIRO;
function  cairo_pattern_get_radial_circles (pattern: Pcairo_pattern_t; x0, y0, r0, x1, y1, r1:PDouble): cairo_status_t; cdecl; external LIB_CAIRO;

(* Matrix functions *)

procedure cairo_matrix_init(matrix: Pcairo_matrix_t; xx, yx, xy, yy, x0, y0: Double); cdecl; external LIB_CAIRO;
procedure cairo_matrix_init_identity(matrix: Pcairo_matrix_t); cdecl; external LIB_CAIRO;
procedure cairo_matrix_init_translate(matrix: Pcairo_matrix_t; tx, ty: Double); cdecl; external LIB_CAIRO;
procedure cairo_matrix_init_scale(matrix: Pcairo_matrix_t; sx, sy: Double); cdecl; external LIB_CAIRO;
procedure cairo_matrix_init_rotate(matrix: Pcairo_matrix_t; radians: Double); cdecl; external LIB_CAIRO;
procedure cairo_matrix_translate(matrix: Pcairo_matrix_t; tx, ty: Double); cdecl; external LIB_CAIRO;
procedure cairo_matrix_scale(matrix: Pcairo_matrix_t; sx, sy: Double); cdecl; external LIB_CAIRO;
procedure cairo_matrix_rotate(matrix: Pcairo_matrix_t; radians: Double); cdecl; external LIB_CAIRO;
function  cairo_matrix_invert(matrix: Pcairo_matrix_t): cairo_status_t; cdecl; external LIB_CAIRO;
procedure cairo_matrix_multiply(result, a, b: Pcairo_matrix_t); cdecl; external LIB_CAIRO;
procedure cairo_matrix_transform_distance(matrix: Pcairo_matrix_t; dx, dy: PDouble); cdecl; external LIB_CAIRO;
procedure cairo_matrix_transform_point(matrix: Pcairo_matrix_t; x, y: PDouble); cdecl; external LIB_CAIRO;

(* PDF functions *)

function  cairo_pdf_surface_create(filename: PChar; width_in_points, height_in_points: Double): Pcairo_surface_t; cdecl; external LIB_CAIRO;
function  cairo_pdf_surface_create_for_stream(write_func: cairo_write_func_t; closure: Pointer; width_in_points, height_in_points: Double): Pcairo_surface_t; cdecl; external LIB_CAIRO;
procedure cairo_pdf_surface_set_size(surface: Pcairo_surface_t; width_in_points, height_in_points: Double); cdecl; external LIB_CAIRO;

(* PS functions *)

function  cairo_ps_surface_create(filename: PChar; width_in_points, height_in_points: Double): Pcairo_surface_t; cdecl; external LIB_CAIRO;
function  cairo_ps_surface_create_for_stream(write_func: cairo_write_func_t; closure: Pointer; width_in_points, height_in_points: Double): Pcairo_surface_t; cdecl; external LIB_CAIRO;
procedure cairo_ps_surface_set_size(surface: Pcairo_surface_t; width_in_points, height_in_points: Double); cdecl; external LIB_CAIRO;
procedure cairo_ps_surface_dsc_comment(surface: Pcairo_surface_t; comment: PChar); cdecl; external LIB_CAIRO;
procedure cairo_ps_surface_dsc_begin_setup(surface: Pcairo_surface_t); cdecl; external LIB_CAIRO;
procedure cairo_ps_surface_dsc_begin_page_setup(surface: Pcairo_surface_t); cdecl; external LIB_CAIRO;

(* SVG functions *)

function  cairo_svg_surface_create(filename: PChar; width_in_points, height_in_points: Double): Pcairo_surface_t; cdecl; external LIB_CAIRO;
function  cairo_svg_surface_create_for_stream(write_func: cairo_write_func_t; closure: Pointer; width_in_points, height_in_points: Double): Pcairo_surface_t; cdecl; external LIB_CAIRO;
procedure cairo_svg_surface_restrict_to_version(surface: Pcairo_surface_t; version: cairo_svg_version_t); cdecl; external LIB_CAIRO;

procedure cairo_svg_get_versions(versions: ppcairo_svg_version_t;num_versions:pcint);cdecl; external LIB_CAIRO;
function  cairo_svg_version_to_string(version: cairo_svg_version_t): PChar; cdecl; external LIB_CAIRO;


(* Functions to be used while debugging (not intended for use in production code) *)

procedure cairo_debug_reset_static_data; cdecl; external LIB_CAIRO;

(* cairo_region addon *)

function cairo_region_create: Pcairo_region_t; cdecl; external LIB_CAIRO;
function cairo_region_create_rectangle(const ARect: Pcairo_rectangle_int_t): Pcairo_region_t; cdecl; external LIB_CAIRO;
function cairo_region_create_rectangles(const ARect: PPcairo_rectangle_int_t; ACount: cint): Pcairo_region_t; cdecl; external LIB_CAIRO;
function cairo_region_copy(ARegion: Pcairo_region_t): Pcairo_region_t; cdecl; external LIB_CAIRO;
function cairo_region_reference(ARegion: Pcairo_region_t): Pcairo_region_t; cdecl; external LIB_CAIRO;
procedure cairo_region_destroy(ARegion: Pcairo_region_t); cdecl; external LIB_CAIRO;
function cairo_region_status(ARegion: Pcairo_region_t): cairo_status_t; cdecl;  external LIB_CAIRO;
procedure cairo_region_get_extents(ARegion: Pcairo_region_t; AOutRect: Pcairo_rectangle_int_t); cdecl; external LIB_CAIRO;
function cairo_region_num_rectangles(ARegion: Pcairo_region_t): cint; cdecl;  external LIB_CAIRO;
procedure cairo_region_get_rectangle(ARegion: Pcairo_region_t; nth: cint; AOutRect: Pcairo_rectangle_int_t); cdecl; external LIB_CAIRO;
function cairo_region_is_empty(ARegion: Pcairo_region_t): boolean; cdecl;  external LIB_CAIRO;
function cairo_region_contains_point(ARegion: Pcairo_region_t; x,y: cint): boolean; cdecl;  external LIB_CAIRO;
function cairo_region_contains_rectangle(ARegion: Pcairo_region_t; ARect: Pcairo_rectangle_int_t): cairo_region_overlap_t; cdecl; external LIB_CAIRO;

// ops
function cairo_region_equal(ARegion1: Pcairo_region_t; ARegion2: Pcairo_region_t): boolean; cdecl; external LIB_CAIRO;
procedure cairo_region_translate(ADestRegion: Pcairo_region_t; dx: integer; dy: integer); cdecl; external LIB_CAIRO;
function cairo_region_intersect(ADestRegion: Pcairo_region_t; ASrcRegion: Pcairo_region_t): cairo_status_t; cdecl; external LIB_CAIRO;
function cairo_region_intersect_rectangle(ADestRegion: Pcairo_region_t; ASrcRect: Pcairo_rectangle_int_t): cairo_status_t; cdecl; external LIB_CAIRO;
function cairo_region_subtract(ADestRegion: Pcairo_region_t; ASrcRegion: Pcairo_region_t): cairo_status_t; cdecl; external LIB_CAIRO;
function cairo_region_subtract_rectangle(ADestRegion: Pcairo_region_t; ASrcRect: Pcairo_rectangle_int_t): cairo_status_t; cdecl; external LIB_CAIRO;
function cairo_region_union(ADestRegion: Pcairo_region_t; ASrcRegion: Pcairo_region_t): cairo_status_t; cdecl; external LIB_CAIRO;
function cairo_region_union_rectangle(ADestRegion: Pcairo_region_t; ASrcRect: Pcairo_rectangle_int_t): cairo_status_t; cdecl; external LIB_CAIRO;
function cairo_region_xor(ADestRegion: Pcairo_region_t; ASrcRegion: Pcairo_region_t): cairo_status_t; cdecl; external LIB_CAIRO;
function cairo_region_xor_rectangle(ADestRegion: Pcairo_region_t; ASrcRect: Pcairo_rectangle_int_t): cairo_status_t; cdecl; external LIB_CAIRO;


implementation

procedure cairo_version(out major, minor, micro: LongInt);
var
  version: LongInt;
begin
  version := cairo_version;
  major := version div 10000;
  minor := (version mod (major * 10000)) div 100;
  micro := (version mod ((major * 10000) + (minor * 100)));
end;

end.
