{ $Id$}
{
 *****************************************************************************
 *                             Gtk2WSCheckLst.pp                             * 
 *                             -----------------                             * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

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
unit Gtk2WSCheckLst;

{$mode objfpc}{$H+}

interface

uses

Gtk2, GLib2, GtkDef,
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  CheckLst, Controls, LCLType, Classes, LMessages,
////////////////////////////////////////////////////
  WSCheckLst, WSLCLClasses,
  Gtk2WSStdCtrls;

type

  { TGtk2WSCheckListBox }

  TGtk2WSCustomCheckListBox = class(TWSCustomCheckListBox)
  private
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  protected
  public
    class function  GetChecked(const ACheckListBox: TCustomCheckListBox;
                                      const AIndex: integer): boolean; override;
    class procedure SetChecked(const ACheckListBox: TCustomCheckListBox;
                      const AIndex: integer; const AChecked: boolean); override;
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;


implementation

uses GtkWSControls, GtkProc;


{ TGtk2WSCheckListBox }

procedure Gtk2WS_CheckListBoxToggle(cellrenderertoggle : PGtkCellRendererToggle;
  arg1 : PGChar; WidgetInfo: PWidgetInfo); cdecl;
var
  aWidget : PGTKWidget;
  aTreeModel : PGtkTreeModel;
  aTreeIter : TGtkTreeIter;
  value : pgValue;
  Mess: TLMessage;
begin
  {$IFDEF EventTrace}
  EventTrace('Gtk2WS_CheckListBoxToggle', WidgetInfo^.LCLObject);
  {$ENDIF}
  aWidget := WidgetInfo^.CoreWidget;
  aTreeModel := gtk_tree_view_get_model (GTK_TREE_VIEW(aWidget));
  if (gtk_tree_model_get_iter_from_string (aTreeModel, @aTreeIter, arg1)) then begin
    // aTreeIter.stamp := GTK_LIST_STORE (aTreeModel)^.stamp; //strange hack
    value := g_new0(SizeOf(TgValue), 1);
    gtk_tree_model_get_value(aTreeModel, @aTreeIter, 0, value);

    g_value_set_boolean(value, not g_value_get_boolean(value));

    gtk_list_store_set_value (GTK_LIST_STORE (aTreeModel), @aTreeIter, 0, value);
    g_value_unset(value);
    g_free(value);
  end;
  Mess.Msg := LM_CHANGED;
  Mess.Result := 0;
  DeliverMessage(widgetInfo^.lclObject, Mess);
end;

procedure Gtk2WS_CheckListBoxRowActivate(treeview : PGtkTreeView;
  arg1 : PGtkTreePath; arg2 : PGtkTreeViewColumn; WidgetInfo: PWidgetInfo); cdecl;
var
  aTreeModel : PGtkTreeModel;
  aTreeIter : TGtkTreeIter;
  value : PGValue;
begin
  aTreeModel := gtk_tree_view_get_model (treeview);
  if (gtk_tree_model_get_iter (aTreeModel, @aTreeIter, arg1)) then begin
    // aTreeIter.stamp := GTK_LIST_STORE (aTreeModel)^.stamp; //strange hack
    value := g_new0(SizeOf(TgValue), 1);
    gtk_tree_model_get_value(aTreeModel, @aTreeIter, 0, value);

    g_value_set_boolean(value, not g_value_get_boolean(value));

    gtk_list_store_set_value (GTK_LIST_STORE (aTreeModel), @aTreeIter, 0, value);
    g_value_unset(value);
    g_free(value);
  end;
end;

class procedure TGtk2WSCustomCheckListBox.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
//var
//  Selection: PGtkTreeSelection;
begin
  TGtkWSBaseScrollingWinControl.SetCallbacks(AGtkWidget,AWidgetInfo);

  {Selection :=} gtk_tree_view_get_selection(PGtkTreeView(AWidgetInfo^.CoreWidget));
  //SignalConnect(PGtkWidget(Selection), 'changed', @Gtk2WS_ListBoxChange, AWidgetInfo);
end;

class function TGtk2WSCustomCheckListBox.GetChecked(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer): boolean;
var
  Iter : TGtkTreeIter;
  TreeView: PGtkTreeView;
  WidgetInfo: PWidgetInfo;
  ListStore: PGtkTreeModel;
begin
  Result:=False;
  WidgetInfo := GetWidgetInfo(PGtkWidget(ACheckListBox.Handle));
  
  TreeView := PGtkTreeView(WidgetInfo^.CoreWidget);
  ListStore := gtk_tree_view_get_model(TreeView);
  if gtk_tree_model_iter_nth_child(ListStore, @Iter, nil, AIndex) then
    gtk_tree_model_get(ListStore, @Iter, [0, @Result, -1]);
end;

class procedure TGtk2WSCustomCheckListBox.SetChecked(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer;
  const AChecked: boolean);
var
  Iter : TGtkTreeIter;
  TreeView: PGtkTreeView;
  WidgetInfo: PWidgetInfo;
  ListStore: PGtkTreeModel;
begin
  WidgetInfo := GetWidgetInfo(PGtkWidget(ACheckListBox.Handle));

  TreeView := PGtkTreeView(WidgetInfo^.CoreWidget);
  ListStore := gtk_tree_view_get_model(TreeView);
  if gtk_tree_model_iter_nth_child(ListStore, @Iter, nil, AIndex) then
    gtk_list_store_set(ListStore, @Iter, [0, AChecked, -1]);
end;

class function TGtk2WSCustomCheckListBox.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  TreeViewWidget: PGtkWidget;
  p: PGtkWidget;                 // ptr to the newly created GtkWidget
  liststore : PGtkListStore;
  Selection: PGtkTreeSelection;
  renderer : PGtkCellRenderer;
  column : PGtkTreeViewColumn;
  WidgetInfo: PWidgetInfo;
begin
  Result := TGtkWSBaseScrollingWinControl.CreateHandle(AWinControl,AParams);
  p:= PGtkWidget(Result);

  if Result = 0 then exit;

  WidgetInfo := GetWidgetInfo(p, False);

  GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(p)^.hscrollbar, GTK_CAN_FOCUS);
  GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(p)^.vscrollbar, GTK_CAN_FOCUS);
  gtk_scrolled_window_set_policy(PGtkScrolledWindow(p),
                                 GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_scrolled_window_set_shadow_type(PGtkScrolledWindow(p),GTK_SHADOW_IN);
  gtk_widget_show(p);

  liststore := gtk_list_store_new (3,
                          [G_TYPE_BOOLEAN, G_TYPE_STRING, G_TYPE_POINTER, nil]);
  TreeViewWidget:= gtk_tree_view_new_with_model (GTK_TREE_MODEL(liststore));
  g_object_unref (G_OBJECT (liststore));

  // Check Column
  renderer := gtk_cell_renderer_toggle_new();
  column := gtk_tree_view_column_new_with_attributes(
                                    'CHECKBTNS', renderer, ['active', 0,  nil]);
  gtk_cell_renderer_toggle_set_active(GTK_CELL_RENDERER_TOGGLE(renderer), True);
  gtk_tree_view_append_column (GTK_TREE_VIEW (TreeViewWidget), column);
  gtk_tree_view_column_set_clickable (GTK_TREE_VIEW_COLUMN (column), TRUE);

  SignalConnect(PGtkWidget(renderer), 'toggled', @Gtk2WS_CheckListBoxToggle, WidgetInfo);
  SignalConnect(TreeViewWidget, 'row_activated', @Gtk2WS_CheckListBoxRowActivate, WidgetInfo);

  //g_signal_connect (renderer, 'toggled', G_CALLBACK (@gtk_clb_toggle), AWinControl);
  //g_signal_connect (TreeViewWidget, 'row_activated', G_CALLBACK (@gtk_clb_toggle_row_activated), AWinControl);

  // Text Column
  renderer := gtk_cell_renderer_text_new();
  column := gtk_tree_view_column_new_with_attributes (
                                       'LISTITEMS', renderer, ['text', 1, nil]);
  gtk_tree_view_append_column (GTK_TREE_VIEW (TreeViewWidget), column);
  gtk_tree_view_column_set_clickable (GTK_TREE_VIEW_COLUMN (column), TRUE);

  gtk_tree_view_set_headers_visible(GTK_TREE_VIEW (TreeViewWidget), False);

  gtk_container_add(GTK_CONTAINER(p), TreeViewWidget);
  gtk_widget_show(TreeViewWidget);

  SetMainWidget(p, TreeViewWidget);
  GetWidgetInfo(p, True)^.CoreWidget := TreeViewWidget;

  Selection := gtk_tree_view_get_selection(PGtkTreeView(TreeViewWidget));

  case TCustomCheckListBox(AWinControl).MultiSelect of
    True : gtk_tree_selection_set_mode(Selection, GTK_SELECTION_MULTIPLE);
    False: gtk_tree_selection_set_mode(Selection, GTK_SELECTION_SINGLE);
  end;
  SetCallbacks(p, WidgetInfo);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCustomCheckListBox, TGtk2WSCustomCheckListBox);
////////////////////////////////////////////////////
end.
