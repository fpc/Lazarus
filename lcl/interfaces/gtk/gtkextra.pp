{ $Id$ }
{
 ---------------------------------------------------------------------------
 gtkextra.pp  -  GTK(2) widgetset - additional gdk/gtk functions
 ---------------------------------------------------------------------------

 This unit contains missing gdk/gtk functions and defines for certain 
 versions of gtk or fpc.

 ---------------------------------------------------------------------------

 @created(Sun Jan 28th WET 2006)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.nl>)

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

unit GtkExtra;

{$mode objfpc}{$H+}

interface

{$I gtkdefines.inc}

{$ifdef gtk1}
{$I gtk1extrah.inc}
{$endif}

{$ifdef gtk2}
{$I gtk2extrah.inc}
{$endif}


implementation

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

{$ifdef gtk1}
{$I gtk1extra.inc}
{$endif}

{$ifdef gtk2}
{$I gtk2extra.inc}
{$endif}

end.
