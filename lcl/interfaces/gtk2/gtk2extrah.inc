{%MainUnit Gtk2Extra.pp}
{ $Id$ }
{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

uses
  dynlibs,
  {$ifdef HasX}
  xlib, x,
  {$endif}
  glib2, gdk2, gtk2, gtk2ext,
  {$IF FPC_FULLVERSION<20600}
  gdk2pixbuf,
  {$ENDIF}
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
  // additional response codes for gtk2
  GTK_RESPONSE_LCL_ALL = GTK_RESPONSE_APPLY;
  GTK_RESPONSE_LCL_YESTOALL = GTK_RESPONSE_ACCEPT;
  GTK_RESPONSE_LCL_RETRY = -12;
  GTK_RESPONSE_LCL_IGNORE = -13;
  GTK_RESPONSE_LCL_NOTOALL = -14;

  GDK_KEY_ISO_Level5_Shift = $FE11;
  GDK_KEY_ISO_Level5_Latch = $FE12;
  GDK_KEY_ISO_Level5_Lock = $FE13;

{$IF FPC_FULLVERSION<20600}
// missing declarations in fpc 2.2
const
   GTK_CELL_RENDERER_FOCUSED = 1 shl 4;
{$ENDIF}

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

{$IF FPC_FULLVERSION<20600}
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
procedure pango_matrix_translate(matrix: PPangoMatrix; tx, ty: double); cdecl; external pangolib;
procedure pango_matrix_rotate(matrix: PPangoMatrix; degrees: double); cdecl; external pangolib;
{$ENDIF}

// ------------------end of symbold to remove --------------------------

function pango_context_get_gravity(Context: PPangoContext): TPangoGravity; cdecl; external pangolib;
procedure pango_context_set_base_gravity(Context: PPangoContext; gravity: TPangoGravity); cdecl; external pangolib;

function pango_context_get_matrix(Context: PPangoContext): PPangoMatrix; cdecl; external pangolib;
procedure pango_context_set_matrix(context: PPangoContext; const matrix: PPangoMatrix); cdecl; external pangolib;

function pango_font_description_get_gravity(desc: PPangoFontDescription): TPangoGravity; cdecl; external pangolib;
procedure pango_font_description_set_gravity(desc: PPangoFontDescription; gravity: TPangoGravity); cdecl; external pangolib;

function pango_attr_gravity_new(gravity: TPangoGravity): PPangoAttribute; cdecl; external pangolib;

function pango_version_check(required_major, required_minor, required_micro: integer): PChar; cdecl; external pangolib;

{$IF FPC_FULLVERSION<20501}
function pango_font_family_is_monospace(family:PPangoFontFamily):gboolean; cdecl; external pangolib;
{$ENDIF}


// - - - - - - - - - gtk extras - - - - - - - - -

// ----------------- this gtk symbols can be removed after fpc 2.2.4

{$IF FPC_FULLVERSION<20600}
// gtk 2.2
function gtk_tree_path_new_from_indices(first_index: gint): PGtkTreePath; cdecl; varargs; external gtklib name 'gtk_tree_path_new_from_indices';

// gtk 2.4
procedure gtk_toolbar_insert(toolbar: PGtkToolBar; item: PGtkToolItem; pos: gint); cdecl; external gtklib name 'gtk_toolbar_insert';
procedure gtk_event_box_set_visible_window(event_box: PGtkEventBox; visible_window: gboolean); cdecl; external gtklib name 'gtk_event_box_set_visible_window';
procedure gtk_event_box_set_above_child(event_box: PGtkEventBox; visible_window: gboolean); cdecl; external gtklib name 'gtk_event_box_set_above_child';
procedure gtk_draw_insertion_cursor(widget:PGtkWidget; drawable:PGdkDrawable; area:PGdkRectangle; location:PGdkRectangle; is_primary:gboolean;
            direction:TGtkTextDirection; draw_arrow:gboolean); cdecl; external gtklib;
{$ENDIF}

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

{$ifdef GTK_2_8}
// gdk 2.8
procedure gdk_display_warp_pointer(display: PGdkDisplay; screen: PGdkScreen; x, y: gint); cdecl; external gdklib;
function gdk_screen_get_rgba_colormap(screen: PGdkScreen): PGdkColormap; cdecl; external gdklib;
procedure gdk_window_move_region(window: PGdkWindow ; region: PGdkRegion; dx, dy: gint);cdecl; external gdklib;

type
  _PGtkIconViewPrivate = ^_GtkIconViewPrivate; {private struct of GtkIconView, used to fix #22778}
  _GtkIconViewPrivate = record
    Width: Gint;
    Height: Gint;
    Selection_Mode: PGtkSelectionMode;

    bin_window: PGdkWindow;
    children: PGList;
    Model: PGtkTreeModel;
    Items: PGList;

    hadjustment: PGtkAdjustment;
    vadjustment: PGtkAdjustment;

    layout_idle_id: guint;

    doing_rubberband: gboolean;
    rubberband_x1: gint;
    rubberband_y1: gint;
    rubberband_x2: gint;
    rubberband_y2: gint;
    scroll_timeout_id: guint;
    scroll_value_diff: gint;
    event_last_x: gint;
    event_last_y: gint;
    anchor_item: Pointer; // PGtkIconViewItem;
    cursor_item: Pointer; // PGtkIconViewItem;
    edited_item: Pointer; // PGtkIconViewItem;
    editable_item: Pointer; // PGtkCellEditable;
    last_single_clicked: Pointer; // PGtkIconViewItem;

    cell_list: PGList;
    n_cells: guint;
    cursor_cell: gint;


    orientation: TGtkOrientation;

    columns: gint;
    item_width: gint;
    spacing: gint;
    row_spacing: gint;
    column_spacing: gint;
    margin: gint;
    item_padding: gint;
    text_column: gint;
    markup_column: gint;
    pixbuf_column: gint;
    pixbuf_cell: gint;
    text_cell: gint;
    tooltip_column: gint;

    // * Drag-and-drop. */
    start_button_mask: TGdkModifierType;
    pressed_button: gint;
    press_start_x: gint;
    press_start_y: gint;

    source_actions: TGdkDragAction;
    dest_actions: TGdkDragAction;

    dest_item: PGtkTreeRowReference;
    dest_pos: guint; // TGtkIconViewDropPosition;

    // * scroll to */
    scroll_to_path: PGtkTreeRowReference;
    scroll_to_row_align: gfloat;
    scroll_to_col_align: gfloat;
    scroll_to_use_align: guint;

    source_set: guint;
    dest_set: guint;
    reorderable: guint;
    empty_view_drop: guint;

    ctrl_pressed: guint;
    shift_pressed: guint;

    draw_focus: guint;
  end;

  TGtkPackDirection = longint;
  
const
  GTK_PACK_DIRECTION_LTR = 0;
  GTK_PACK_DIRECTION_RTL = 1;
  GTK_PACK_DIRECTION_TTB = 2;
  GTK_PACK_DIRECTION_BTT = 3;

procedure gtk_menu_bar_set_pack_direction(menubar : PGtkMenuBar; pack_dir : TGtkPackDirection); cdecl; external gtklib;
procedure gtk_menu_bar_set_child_pack_direction(menubar : PGtkMenuBar; pack_dir : TGtkPackDirection); cdecl; external gtklib;
{$IF FPC_FULLVERSION<20600}
procedure gtk_file_chooser_set_do_overwrite_confirmation(chooser:PGtkFileChooser; do_overwrite_confirmation:gboolean); cdecl; external gtklib;
{$ENDIF}
procedure gtk_icon_view_scroll_to_path(icon_view: PGtkIconView; path: PGtkTreePath; use_align: gboolean; row_align: gfloat; col_align: gfloat); cdecl; external gtklib;
function gtk_icon_view_get_cursor(icon_view: PGtkIconView; var path: PGtkTreePath; var cell: PGtkCellRenderer): gboolean; cdecl; external gtklib;
procedure gtk_icon_view_set_cursor(icon_view: PGtkIconView; path: PGtkTreePath; cell: PGtkCellRenderer; start_editing: gboolean); cdecl; external gtklib;
function gtk_tree_view_get_visible_range(tree_view: PGtkTreeView; out start_path, endpath: PGtkTreePath): gboolean; cdecl; external gtklib;
function gtk_icon_view_get_visible_range(tree_view: PGtkTreeView; out start_path, endpath: PGtkTreePath): gboolean; cdecl; external gtklib;
function gtk_icon_view_get_item_at_pos(icon_view: PGtkIconView; x: gint; y: gint; var path: PGtkTreePath; var cell: PGtkCellRenderer): gboolean; cdecl; external gtklib;

procedure gtk_tree_view_column_queue_resize(tree_column: PGtkTreeViewColumn); cdecl; external gtklib;

{$endif}
const
  GDK_WINDOW_OFFSCREEN_lcl = 6; // ToDo: move to fpc
{$ifdef GTK_2_10}
function gdk_screen_is_composited(screen: PGdkScreen): gboolean; cdecl; external gdklib;
{$endif}

type
  TGtkTreeViewGridLines = cardinal;

const
  GTK_TREE_VIEW_GRID_LINES_NONE = 0;
  GTK_TREE_VIEW_GRID_LINES_HORIZONTAL = 1;
  GTK_TREE_VIEW_GRID_LINES_VERTICAL = 2;
  GTK_TREE_VIEW_GRID_LINES_BOTH = 3;

var
  gtk_window_set_opacity: procedure(window: PGtkWindow; opacity: gdouble); cdecl;
  g_object_ref_sink: function(anObject: PGObject): gpointer; cdecl;
  gdk_window_get_cursor: function(window: PGdkWindow): PGdkCursor; cdecl;
  // since 2.20
  gdk_screen_get_primary_monitor: function(screen: PGdkScreen): gint; cdecl;

  // since 2.10
  gtk_tree_view_set_grid_lines: procedure(tree_view: PGtkTreeView; grid_lines: TGtkTreeViewGridLines); cdecl;
  gtk_tree_view_get_grid_lines: function(tree_view: PGtkTreeView): TGtkTreeViewGridLines; cdecl;
  gtk_window_get_group: function(window:PGtkWindow): PGtkWindowGroup; cdecl;
  // since 2.14
  gtk_adjustment_configure: procedure(Adjustment: PGtkAdjustment; AValue: GDouble; ALower: GDouble; AUpper: GDouble;
    AStepIncrement: GDouble; APageIncrement: GDouble; APageSize: GDouble); cdecl;

