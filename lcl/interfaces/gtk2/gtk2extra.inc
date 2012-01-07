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

function GTK_TYPE_CELL_VIEW: GType;
begin
  GTK_TYPE_CELL_VIEW:=gtk_cell_view_get_type;
end;

function GTK_CELL_VIEW(obj: pointer): PGtkCellView;
begin
  GTK_CELL_VIEW:=PGtkCellView(GTK_CHECK_CAST(obj,GTK_TYPE_CELL_VIEW));
end;

function GTK_IS_CELL_VIEW(obj: pointer): boolean;
begin
  GTK_IS_CELL_VIEW:=GTK_CHECK_TYPE(obj,GTK_TYPE_CELL_VIEW);
end;

function GTK_IS_CELL_VIEW_CLASS(klass: pointer): boolean;
begin
  GTK_IS_CELL_VIEW_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_CELL_VIEW);
end;

function gtk_cell_view_get_model(cell_view: PGtkCellView): PGtkTreeModel;
var
  Value: TGvalue;
begin
  FillByte(Value,SizeOf(Value),0);
  g_value_init(@Value,GTK_TYPE_TREE_MODEL);
  g_object_get_property(PGObject(Cell_View),'model',@Value);
  Result:=PGtkTreeModel(g_value_get_object(@Value));
end;

{$ifdef windows}
procedure gdk_window_copy_area(Dest : PGDKWindow; GC : PGDKGC; DestX,
  DestY : Longint; SRC : PGDKWindow; XSRC, YSRC, Width, Height : Longint);
begin
  gdk_draw_pixmap(Dest, GC, Src, XSrc, YSrc, DestX, DestY, Width, Height);
end;
{$endif}

var
  gtkhandle: tlibhandle;
  glibhandle: tlibhandle;
  gdklibhandle: tlibhandle;
  libIter: Integer;

initialization
  for libIter := High(GtkLibNames) downto Low(GtkLibNames) do
  begin
    gtkhandle := LoadLibrary(GtkLibNames[libIter]);
    if gtkhandle <> 0 then
    begin
      pointer(gtk_window_set_opacity):=GetProcAddress(gtkhandle,'gtk_window_set_opacity');
      break;
    end;
  end;
  for libIter := High(GLibNames) downto Low(GLibNames) do
  begin
    glibhandle := LoadLibrary(GLibNames[libIter]);
    if glibhandle <> 0 then
    begin
      pointer(g_object_ref_sink):=GetProcAddress(glibhandle,'g_object_ref_sink');
      break;
    end;
  end;
  gdklibhandle := LoadLibrary(gdklib);
  if gdklibhandle <> 0
  then pointer(gdk_window_get_cursor):=GetProcAddress(gdklibhandle,'gdk_window_get_cursor');

finalization
  if gtkhandle <> 0 then
    FreeLibrary(gtkhandle);
  if glibhandle <> 0 then
    FreeLibrary(glibhandle);
  if gdklibhandle <> 0 then
    FreeLibrary(gdklibhandle);
