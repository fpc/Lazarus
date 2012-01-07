{%MainUnit Gtk2Extra.pp}
{ $Id$ }
{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

uses
  dynlibs,
  {$ifdef HasX}
  xlib, x,
  {$endif}
  glib2, gdk2pixbuf, gdk2, gtk2, gtk2ext,
  {$ifdef HasGdk2X}
  gdk2x,
  {$endif}
  Pango;

const
  { This is equired when people don't have -dev/-devel packages on linux.
    I'm not sure how Darwin is handled tho }
  {$ifdef windows}
  GLibNames: array[1..1] of string = (gliblib);
  {$else}
    {$ifdef darwin} // Mac/Darwin
    GLibNames: array[1..1] of string = (gliblib); // TODO: I don't know this one!
    {$else} // BSD, Solaris, Linux
    GLibNames: array[1..2] of string = (gliblib, gliblib + '.0');
    {$endif}
  {$endif}

const
  gdkdll = gdklib;

// missing declarations in fpc 2.2
const
   GTK_CELL_RENDERER_FOCUSED = 1 shl 4;

{$IFNDEF MSWINDOWS} // this is required for compilation under windows
{$if defined(HasX)} // This is required for compilation under Mac OS X
var
  gdk_display: PDisplay; external gdklib name 'gdk_display';

function gdk_window_xwindow(win : PGdkDrawable): TXID; cdecl; external gdklib name 'gdk_x11_drawable_get_xid';
function gdk_x11_image_get_ximage(image:PGdkImage):PXImage;cdecl;external gdklib name 'gdk_x11_image_get_ximage';
{$endif}
{$ENDIF}

procedure laz_gdk_gc_set_dashes(gc:PGdkGC; dash_offset:gint;
  dashlist:Pgint8; n:gint); cdecl; external gdkdll name 'gdk_gc_set_dashes';
// - - - - - - - - - pango extras - - - - - - - - -


function pango_unichar_direction(ch : gunichar) : TPangoDirection; cdecl; external pangolib;

// pango 1.4
function pango_find_base_dir(text : Pgchar; aLength : gint) : TPangoDirection; cdecl; external pangolib;

type
  PPPangoAttrList = ^PPangoAttrList;
// pango 1.16
  PPangoGravity = ^TPangoGravity;
  TPangoGravity = (
    PANGO_GRAVITY_SOUTH,
    PANGO_GRAVITY_EAST,
    PANGO_GRAVITY_NORTH,
    PANGO_GRAVITY_WEST,
    PANGO_GRAVITY_AUTO
  );

// ----------------- this pango symbols can be removed after fpc 2.2.4
// pango 1.6
  PPangoMatrix = ^TPangoMatrix;
  TPangoMatrix = record
    xx: double;
    xy: double;
    yx: double;
    yy: double;
    x0: double;
    y0: double;
  end;

// pango 1.8
  TPangoRenderer = pointer;
  PPangoRenderer = ^TPangoRenderer;
  TPangoRenderPart = (
    PANGO_RENDER_PART_FOREGROUND,
    PANGO_RENDER_PART_BACKGROUND,
    PANGO_RENDER_PART_UNDERLINE,
    PANGO_RENDER_PART_STRIKETHROUGH
  );

procedure pango_renderer_draw_layout(renderer: PPangoRenderer; layout: PPangoLayout; x, y: Integer); cdecl; external pangolib;

// ------------------end of symbold to remove --------------------------

function pango_context_get_gravity(Context: PPangoContext): TPangoGravity; cdecl; external pangolib;
procedure pango_context_set_base_gravity(Context: PPangoContext; gravity: TPangoGravity); cdecl; external pangolib;

function pango_context_get_matrix(Context: PPangoContext): PPangoMatrix; cdecl; external pangolib;
procedure pango_context_set_matrix(context: PPangoContext; const matrix: PPangoMatrix); cdecl; external pangolib;
procedure pango_matrix_translate(matrix: PPangoMatrix; tx, ty: double); cdecl; external pangolib;
procedure pango_matrix_rotate(matrix: PPangoMatrix; degrees: double); cdecl; external pangolib;

function pango_font_description_get_gravity(desc: PPangoFontDescription): TPangoGravity; cdecl; external pangolib;
procedure pango_font_description_set_gravity(desc: PPangoFontDescription; gravity: TPangoGravity); cdecl; external pangolib;

function pango_attr_gravity_new(gravity: TPangoGravity): PPangoAttribute; cdecl; external pangolib;

function pango_version_check(required_major, required_minor, required_micro: integer): PChar; cdecl; external pangolib;

{$if defined(VER2_2_0)}
function pango_layout_get_font_description(layout:PPangoLayout):PPangoFontDescription; cdecl; external pangolib;
{$ifend}

// - - - - - - - - - gtk extras - - - - - - - - -

// ----------------- this gtk symbols can be removed after fpc 2.2.4
// gtk 2.2
function gtk_tree_path_new_from_indices(first_index: gint): PGtkTreePath; cdecl; varargs; external gtklib name 'gtk_tree_path_new_from_indices';

// gtk 2.4
procedure gtk_toolbar_insert(toolbar: PGtkToolBar; item: PGtkToolItem; pos: gint); cdecl; external gtklib name 'gtk_toolbar_insert';
procedure gtk_event_box_set_visible_window(event_box: PGtkEventBox; visible_window: gboolean); cdecl; external gtklib name 'gtk_event_box_set_visible_window';
procedure gtk_event_box_set_above_child(event_box: PGtkEventBox; visible_window: gboolean); cdecl; external gtklib name 'gtk_event_box_set_above_child';
procedure gtk_draw_insertion_cursor(widget:PGtkWidget; drawable:PGdkDrawable; area:PGdkRectangle; location:PGdkRectangle; is_primary:gboolean;
            direction:TGtkTextDirection; draw_arrow:gboolean); cdecl; external gtklib;

// gtk 2.6
type
  PGtkCellView = ^TGtkCellView;
  TGtkCellView = record
    parent_instance: TGtkWidget;

    // private
    model: PGtkTreeModel;
    displayed_row: PGtkTreeRowReference;
    cell_list: PGList;
    spacing: gint;
    background: TGdkColor;
    background_set: gboolean;
  end;
function GTK_TYPE_CELL_VIEW : GType;
function GTK_CELL_VIEW(obj: pointer) : PGtkCellView;
//function GTK_CELL_VIEW_CLASS(klass: pointer) : PGtkCellViewClass;
function GTK_IS_CELL_VIEW(obj: pointer) : boolean;
function GTK_IS_CELL_VIEW_CLASS(klass: pointer) : boolean;
//function GTK_CELL_VIEW_GET_CLASS(obj: pointer) : PGtkCellViewClass;
function gtk_cell_view_get_type:TGtkType; cdecl; external gtklib;
function gtk_cell_view_get_model(cell_view: PGtkCellView): PGtkTreeModel;
function gtk_cell_view_get_displayed_row(cell_view: PGtkCellView): PGtkTreePath; cdecl; external gtklib name 'gtk_cell_view_get_displayed_row';
procedure gtk_file_chooser_set_show_hidden(chooser: PGtkFileChooser; show_hidden: gboolean); cdecl; external gtklib name 'gtk_file_chooser_set_show_hidden';
// ------------------end of symbold to remove --------------------------

procedure gtk_im_context_get_preedit_string_laz(context:PGtkIMContext; str:PPgchar; attrs:PPPangoAttrList; cursor_pos:Pgint); cdecl; external gtklib name 'gtk_im_context_get_preedit_string';

// - - - - - - - - - gdk extras - - - - - - - - -

// ----------------- this gdk symbols can be removed after fpc 2.2.4
type
  PGdkPangoRenderer = pointer;
// gdk 2.2
function gdk_event_new(_type: TGdkEventType): PGdkEvent; cdecl; external gdklib;

procedure gdk_display_get_pointer(display : PGdkDisplay; screen :PGdkScreen; x :Pgint; y : Pgint; mask : PGdkModifierType); cdecl; external gdklib;
function gdk_display_get_default:PGdkDisplay; cdecl; external gdklib;

procedure gdk_draw_pixbuf(drawable : PGdkDrawable; gc : PGdkGC; pixbuf : PGdkPixbuf; src_x, src_y, dest_x, dest_y, width, height : gint;
                                             dither : TGdkRgbDither; x_dither, y_dither : gint); cdecl; external gdklib;
function gdk_screen_get_default: PGdkScreen; cdecl; external gdklib;
function gdk_screen_get_rgb_colormap(screen: PGdkScreen): PGdkColormap; cdecl; external gdklib;
function gdk_x11_get_default_xdisplay: pointer; cdecl; external gdklib;
function gdk_x11_get_default_root_xwindow: guint; cdecl; external gdklib;

// gdk 2.4
function gdk_cursor_new_from_pixbuf(display: PGdkDisplay; pixbuf: PGdkPixbuf; x, y: gint): PGdkCursor; cdecl; external gdklib name 'gdk_cursor_new_from_pixbuf';
function gdk_display_get_default_cursor_size(display: PGdkDisplay): guint; cdecl; external gdklib;
procedure gdk_display_get_maximal_cursor_size(display: PGdkDisplay; w, h: pguint); cdecl; external gdklib;

// gdk 2.6
function gdk_pango_renderer_get_default(screen: PGdkScreen): PPangoRenderer; cdecl; external gdklib;
procedure gdk_pango_renderer_set_drawable(gdk_renderer: PGdkPangoRenderer; drawable: PGdkDrawable); cdecl; external gdklib;
procedure gdk_pango_renderer_set_gc(gdk_renderer: PGdkPangoRenderer; gc: PGdkGC); cdecl; external gdklib;
procedure gdk_pango_renderer_set_override_color(gdk_renderer: PGdkPangoRenderer; part: TPangoRenderPart; color: PGdkColor); cdecl; external gdklib;
// ------------------end of symbold to remove --------------------------

// gdk-pixbuf 2.6
function gdk_pixbuf_flip(src: PGdkPixbuf; horizontal: gboolean): PGdkPixbuf; cdecl; external gdkpixbuflib;

{$ifdef GTK_2_8}
// gdk 2.8
procedure gdk_display_warp_pointer(display: PGdkDisplay; screen: PGdkScreen; x, y: gint); cdecl; external gdklib;
function gdk_screen_get_rgba_colormap(screen: PGdkScreen): PGdkColormap; cdecl; external gdklib;
procedure gdk_window_move_region(window: PGdkWindow ; region: PGdkRegion; dx, dy: gint);cdecl; external gdklib;

type
  TGtkPackDirection = longint;
  
const
  GTK_PACK_DIRECTION_LTR = 0;
  GTK_PACK_DIRECTION_RTL = 1;
  GTK_PACK_DIRECTION_TTB = 2;
  GTK_PACK_DIRECTION_BTT = 3;

procedure gtk_menu_bar_set_pack_direction(menubar : PGtkMenuBar; pack_dir : TGtkPackDirection); cdecl; external gtklib;
procedure gtk_menu_bar_set_child_pack_direction(menubar : PGtkMenuBar; pack_dir : TGtkPackDirection); cdecl; external gtklib;
procedure gtk_file_chooser_set_do_overwrite_confirmation(chooser:PGtkFileChooser; do_overwrite_confirmation:gboolean); cdecl; external gtklib;
procedure gtk_icon_view_scroll_to_path(icon_view: PGtkIconView; path: PGtkTreePath; use_align: gboolean; row_align: gfloat; col_align: gfloat); cdecl; external gtklib;
function gtk_icon_view_get_cursor(icon_view: PGtkIconView; var path: PGtkTreePath; var cell: PGtkCellRenderer): gboolean; cdecl; external gtklib;
procedure gtk_icon_view_set_cursor(icon_view: PGtkIconView; path: PGtkTreePath; cell: PGtkCellRenderer; start_editing: gboolean); cdecl; external gtklib;
function gtk_tree_view_get_visible_range(tree_view: PGtkTreeView; out start_path, endpath: PGtkTreePath): gboolean; cdecl; external gtklib;
function gtk_icon_view_get_visible_range(tree_view: PGtkTreeView; out start_path, endpath: PGtkTreePath): gboolean; cdecl; external gtklib;
{$endif}
{$ifdef GTK_2_10}
function gdk_screen_is_composited(screen: PGdkScreen): gboolean; cdecl; external gdklib;
{$endif}

var
  gtk_window_set_opacity: procedure(window: PGtkWindow; opacity: gdouble); cdecl;
  g_object_ref_sink: function(anObject: PGObject): gpointer; cdecl;
  gdk_window_get_cursor: function(window: PGdkWindow): PGdkCursor; cdecl;

{$ifdef ver2_2}
{$ifdef darwin}
// this linklib statement is not present in gtk2.pas
{$linklib gthread-2.0}
{$endif}
{$endif}
