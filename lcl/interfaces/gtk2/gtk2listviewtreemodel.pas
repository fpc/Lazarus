{ $Id$

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

unit Gtk2ListViewTreeModel;

{$mode objfpc}{$H+}

interface

uses
  // FPC
  Classes, SysUtils,
  // Gtk
  gtk2, glib2,
  // LCL
  ComCtrls;
const
  ELCLListViewModel = 'LCLListViewModel';
type

  { TLCLTreeModel }
  PLCLListViewModel = ^TLCLListViewModel;

  { TLCLListViewModel }

  TLCLListViewModel = object
    {%REGION GObject} // Add nothing in or before this region!!
    g_type_instance : TGTypeInstance;
    ref_count : guint;
    qdata : PGData;
    {%ENDREGION}
    {%REGION Interface methods in order!}
    procedure row_changed({%H-}path:PGtkTreePath; {%H-}iter:PGtkTreeIter); cdecl;
    procedure row_inserted({%H-}path:PGtkTreePath; {%H-}iter:PGtkTreeIter); cdecl;
    procedure row_has_child_toggled({%H-}path:PGtkTreePath; {%H-}iter:PGtkTreeIter); cdecl;
    procedure row_deleted({%H-}path:PGtkTreePath); cdecl;
    procedure rows_reordered({%H-}path:PGtkTreePath; {%H-}iter:PGtkTreeIter; {%H-}new_order:Pgint); cdecl;
    function  get_flags():TGtkTreeModelFlags; cdecl;
    function  get_n_columns():gint; cdecl;
    function  get_column_type({%H-}index:gint):GType; cdecl;
    function  get_iter(iter:PGtkTreeIter; path:PGtkTreePath):gboolean; cdecl;
    function  get_path(iter:PGtkTreeIter):PGtkTreePath; cdecl;
    procedure get_value(iter:PGtkTreeIter; {%H-}column:gint; value:PGValue); cdecl;
    function  iter_next(iter:PGtkTreeIter):gboolean; cdecl;
    function  iter_children({%H-}iter:PGtkTreeIter; {%H-}parent:PGtkTreeIter):gboolean; cdecl;
    function  iter_has_child({%H-}iter:PGtkTreeIter):gboolean; cdecl;
    function  iter_n_children(iter:PGtkTreeIter):gint; cdecl;
    function  iter_nth_child(iter:PGtkTreeIter; parent:PGtkTreeIter; n:gint):gboolean; cdecl;
    function  iter_parent({%H-}iter:PGtkTreeIter; {%H-}child:PGtkTreeIter):gboolean; cdecl;
    procedure ref_node({%H-}iter:PGtkTreeIter); cdecl;
    procedure unref_node({%H-}iter:PGtkTreeIter); cdecl;
    {%ENDREGION}
    procedure NotifyRowInserted(AIndex: PtrUInt);
    procedure NotifyRowDeleted(AIndex: PtrUInt);
    function TreeModel: PGtkTreeModel; inline;
  public
    ListView: TCustomListView;
  end;

  PMyTreeModelObjectClass = ^TLCLTreeModelClass;
  TLCLTreeModelClass = record
    parent_class: TGObjectClass;
  end;
  TLVItemHack = class(TListItem)
  end;


function LCLListViewModelNew(AListView: TCustomListView): PLCLListViewModel;
function LCLLISTVIEW_MODEL_TYPE: GType;


procedure LCLListViewModelClassInit({%H-}g_class:gpointer; {%H-}class_data:gpointer); cdecl;
procedure LCLListViewModelInit({%H-}instance:PGTypeInstance; {%H-}g_class:gpointer); cdecl;
procedure LCLListViewModelInterfaceInit(g_iface:PGtkTreeModelIface; {%H-}iface_data:gpointer); cdecl;


implementation

var
  _LCLLISTVIEW_MODEL_TYPE: gulong = 0;

function LCLListViewModelNew(AListView: TCustomListView): PLCLListViewModel;
begin
  Result := g_object_new(LCLLISTVIEW_MODEL_TYPE, nil);
  Result^.ListView := AListView;
end;

function LCLLISTVIEW_MODEL_TYPE: GType;
var
  TypeInfo: TGTypeInfo;
  INterfaceINfo: TGInterfaceInfo;
begin
  _LCLLISTVIEW_MODEL_TYPE := g_type_from_name(ELCLListViewModel);
  if _LCLLISTVIEW_MODEL_TYPE = 0 then
  begin
    with TypeInfo do
    begin
      class_size:= SizeOf(TLCLTreeModelClass);
      base_init := nil;//TGBaseInitFunc;
      base_finalize:= nil;// TGBaseFinalizeFunc;
      class_init := @LCLListViewModelClassInit;// TGClassInitFunc;
      class_finalize := nil;// TGClassFinalizeFunc;
      class_data := nil;// gconstpointer;
      instance_size := SizeOf(TLCLListViewModel);// guint16;
      n_preallocs := 0;// guint16;
      instance_init := @LCLListViewModelInit;// TGInstanceInitFunc;
      value_table := nil;// PGTypeValueTable;
    end;
    with InterfaceInfo do
    begin
      interface_init := TGInterfaceInitFunc(@LCLListViewModelInterfaceInit);// TGInterfaceInitFunc;
      interface_finalize := nil; //TGInterfaceFinalizeFunc;
      interface_data := nil;//gpointer;
    end;
    _LCLLISTVIEW_MODEL_TYPE := g_type_register_static(G_TYPE_OBJECT, ELCLListViewModel, @TypeInfo, 0);
    g_type_add_interface_static(_LCLLISTVIEW_MODEL_TYPE, GTK_TYPE_TREE_MODEL, @InterfaceInfo);
  end;
  Result := _LCLLISTVIEW_MODEL_TYPE;
end;

procedure LCLListViewModelClassInit(g_class:gpointer; class_data:gpointer); cdecl;
begin
end;

procedure LCLListViewModelInit(instance:PGTypeInstance; g_class:gpointer); cdecl;
begin
end;

procedure LCLListViewModelInterfaceInit(g_iface:PGtkTreeModelIface; iface_data:gpointer); cdecl;
var
  P: PPointer;
begin
  // Do not change the order here!!
  P := @g_iface^.row_changed;

  P^ := @TLCLListViewModel.row_changed;           Inc(P);
  P^ := @TLCLListViewModel.row_inserted;          Inc(P);
  P^ := @TLCLListViewModel.row_has_child_toggled; Inc(P);
  P^ := @TLCLListViewModel.row_deleted;           Inc(P);
  P^ := @TLCLListViewModel.rows_reordered;        Inc(P);
  P^ := @TLCLListViewModel.get_flags;             Inc(P);
  P^ := @TLCLListViewModel.get_n_columns;         Inc(P);
  P^ := @TLCLListViewModel.get_column_type;       Inc(P);
  P^ := @TLCLListViewModel.get_iter;              Inc(P);
  P^ := @TLCLListViewModel.get_path;              Inc(P);
  P^ := @TLCLListViewModel.get_value;             Inc(P);
  P^ := @TLCLListViewModel.iter_next;             Inc(P);
  P^ := @TLCLListViewModel.iter_children;         Inc(P);
  P^ := @TLCLListViewModel.iter_has_child;        Inc(P);
  P^ := @TLCLListViewModel.iter_n_children;       Inc(P);
  P^ := @TLCLListViewModel.iter_nth_child;        Inc(P);
  P^ := @TLCLListViewModel.iter_parent;           Inc(P);
  P^ := @TLCLListViewModel.ref_node;              Inc(P);
  P^ := @TLCLListViewModel.unref_node;            Inc(P);

end;

{ TLCLListViewModel }


procedure TLCLListViewModel.row_changed(path: PGtkTreePath; iter: PGtkTreeIter); cdecl;
begin
end;

procedure TLCLListViewModel.row_inserted(path: PGtkTreePath; iter: PGtkTreeIter); cdecl;
begin
end;

procedure TLCLListViewModel.row_has_child_toggled(path: PGtkTreePath;
  iter: PGtkTreeIter); cdecl;
begin
end;

procedure TLCLListViewModel.row_deleted(path: PGtkTreePath); cdecl;
begin
end;

procedure TLCLListViewModel.rows_reordered(path: PGtkTreePath;
  iter: PGtkTreeIter; new_order: Pgint); cdecl;
begin
end;

function TLCLListViewModel.get_flags(): TGtkTreeModelFlags; cdecl;
begin
  Result := GTK_TREE_MODEL_LIST_ONLY or 0;
end;

function TLCLListViewModel.get_n_columns(): gint; cdecl;
begin
  Result := 1;
end;

function TLCLListViewModel.get_column_type(index: gint): GType; cdecl;
begin
  Result := G_TYPE_POINTER;
end;

function TLCLListViewModel.get_iter(iter: PGtkTreeIter; path: PGtkTreePath): gboolean; cdecl;
var
  Index: PtrUInt;
begin
  Result := False;
  Index := gtk_tree_path_get_indices(path)[0];
  if Index < ListView.Items.Count then
  begin
    iter^.user_data:= {%H-}Pointer(Index);
    Exit(True);
  end;
end;

function TLCLListViewModel.get_path(iter: PGtkTreeIter): PGtkTreePath; cdecl;
var
  Index: PtrUint;
begin
  Result := nil;
  if iter = nil then
    Exit;
  Index := {%H-}PtrUint(Iter^.user_data);
  Result := gtk_tree_path_new_from_indices(Index, -1);
end;

procedure TLCLListViewModel.get_value(iter: PGtkTreeIter; column: gint;
  value: PGValue); cdecl;
var
  Index: Integer;
  Item: TLVItemHack;
  //ValueType: GType;
  //SubIndex: Integer;
begin
  Index := {%H-}PtrUint(Iter^.user_data);
  Item := TLVItemHack(ListView.Items.Item[Index]);

  g_value_init(value, G_TYPE_POINTER);
  g_value_set_pointer(value, Pointer(Item));


// We use custom renderers so the below is not needed and was never tested :)

{
  Listview Columns in the tree model are stored like so
  [ 0: Checked. 1: Pixbuf. 2: Caption. 3: Subitem(x) Pixbuf. 4: Subitem(x) Text etc
}

{  Case column of
    0:
       begin
         g_value_init(value, G_TYPE_BOOLEAN);
         g_value_set_boolean(value, Item.GetCheckedInternal);
       end;
    1:
       begin
         g_value_init(value, GTK_TYPE_PIXMAP);
         //g_value_set_pointer(ListView.im); !!
         g_value_set_pointer(value, nil);
       end;
    2:
       begin
         g_value_init(value, G_TYPE_STRING);
         g_value_set_static_string(value,PChar(Item.Caption));
       end;
  else
    SubIndex := (column - 3) div 2;
    if Column and 1 = 0 then // Picbuf
    begin
      g_value_init(value, GTK_TYPE_PIXMAP);
      //g_value_set_pointer(ListView.im); !!
      g_value_set_pointer(value, nil);
    end
    else // Text;
    begin
      g_value_init(value, G_TYPE_STRING);
      if SubIndex >= Item.SubItems.Count then
        g_value_set_static_string(value,PChar(''))
      else
        g_value_set_static_string(value,PChar(Item.SubItems.Strings[SubIndex]));
    end;
  end;
}
end;

function TLCLListViewModel.iter_next(iter: PGtkTreeIter): gboolean; cdecl;
begin
  Result := False;
  if ListView = nil then
    Exit;
  Inc({%H-}PtrUInt(Iter^.user_data));
  Result := {%H-}PtrUint(Iter^.user_data) < ListView.items.Count;
end;

function TLCLListViewModel.iter_children(iter: PGtkTreeIter; parent: PGtkTreeIter): gboolean; cdecl;
begin
  Result := False;
end;

function TLCLListViewModel.iter_has_child(iter: PGtkTreeIter): gboolean; cdecl;
begin
  Result := false;
end;

function TLCLListViewModel.iter_n_children(iter: PGtkTreeIter): gint; cdecl;
begin
  Result := 0;
  if (Iter = nil) and (ListView <> nil) then
    Result := ListView.Items.Count;
end;

function TLCLListViewModel.iter_nth_child(iter: PGtkTreeIter;
  parent: PGtkTreeIter; n: gint): gboolean; cdecl;
begin
  Result := False;
  if (ListView = nil) or (parent <> nil) then
    Exit;
  if (Iter <> nil) and (n < ListView.Items.Count) then
  begin
    {%H-}PtrUint(Iter^.user_data) := n;
    Result := True;
  end;
end;

function TLCLListViewModel.iter_parent(iter: PGtkTreeIter; child: PGtkTreeIter): gboolean; cdecl;
begin
  Result := False;
end;

procedure TLCLListViewModel.ref_node(iter: PGtkTreeIter); cdecl;
begin

end;

procedure TLCLListViewModel.unref_node(iter: PGtkTreeIter); cdecl;
begin

end;

procedure TLCLListViewModel.NotifyRowInserted(AIndex: PtrUInt);
var
  Path: PGtkTreePath;
  Iter: TGtkTreeIter;
begin
  Iter.user_data := {%H-}Pointer(AIndex);
  path := gtk_tree_path_new_from_indices(AIndex, -1);
  //emits a signal
  gtk_tree_model_row_inserted(TreeModel, path, @iter);
  gtk_tree_path_free(path);
end;

procedure TLCLListViewModel.NotifyRowDeleted(AIndex: PtrUInt);
var
  Path: PGtkTreePath;
begin
  path := gtk_tree_path_new_from_indices(AIndex, -1);
  //emits a signal
  gtk_tree_model_row_deleted(TreeModel, path);
  gtk_tree_path_free(path);
end;

function TLCLListViewModel.TreeModel: PGtkTreeModel; inline;
begin
  Result := PGtkTreeModel(@Self);
end;

end.
