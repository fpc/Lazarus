{ $Id$}
{
 *****************************************************************************
 *                             Gtk2WSCheckLst.pp                             * 
 *                             -----------------                             * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit Gtk2WSCheckLst;

{$mode objfpc}{$H+}

interface

uses
  // RTL
  SysUtils, Classes,
  Gtk2, GLib2, Gtk2Def,
  // LCL
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  CheckLst, StdCtrls, Controls, LCLType, LMessages, LCLProc,
////////////////////////////////////////////////////
  WSCheckLst, WSLCLClasses;

type

  { TGtk2WSCheckListBox }

  { TGtk2WSCustomCheckListBox }

  TGtk2WSCustomCheckListBox = class(TWSCustomCheckListBox)
  private
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class function GetItemEnabled(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer): Boolean; override;
    class function GetState(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer): TCheckBoxState; override;
    class procedure SetItemEnabled(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer; const AEnabled: Boolean); override;
    class procedure SetState(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer; const AState: TCheckBoxState); override;
  end;


implementation

uses
  Gtk2WSControls, Gtk2Proc, Gtk2CellRenderer;

const
  gtk2CLBState = 0; // byte
  gtk2CLBText = 1; // PGChar
  gtk2CLBDisabled = 3; // gboolean

{ TGtk2WSCheckListBox }

function Gtk2WS_CheckListBoxSelectionChanged({%H-}Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo): gboolean; cdecl;
var
  Mess: TLMessage;
begin
  Result := False;
  if WidgetInfo^.ChangeLock > 0 then
    Exit;
  FillChar(Mess{%H-},SizeOf(Mess),0);
  Mess.msg := LM_SELCHANGE;
  DeliverMessage(WidgetInfo^.LCLObject, Mess);
end;

procedure Gtk2WS_CheckListBoxDataFunc({%H-}tree_column: PGtkTreeViewColumn;
  cell: PGtkCellRenderer; tree_model: PGtkTreeModel; iter: PGtkTreeIter; {%H-}data: Pointer); cdecl;
var
  b: byte;
  ADisabled: gboolean;
  AValue: TCheckBoxState;
begin
  gtk_tree_model_get(tree_model, iter, [gtk2CLBState, @b, -1]);
  gtk_tree_model_get(tree_model, iter, [gtk2CLBDisabled, @ADisabled, -1]);
  AValue := TCheckBoxState(b); // TCheckBoxState is 4 byte
  g_object_set(cell, 'inconsistent', [gboolean(AValue = cbGrayed), nil]);
  if AValue <> cbGrayed then
    gtk_cell_renderer_toggle_set_active(PGtkCellRendererToggle(cell), AValue = cbChecked);

  g_object_set(cell, 'activatable', [gboolean(not ADisabled), nil]);
end;

procedure Gtk2WS_CheckListBoxToggle({%H-}cellrenderertoggle : PGtkCellRendererToggle;
  arg1 : PGChar; WidgetInfo: PWidgetInfo); cdecl;
var
  Mess: TLMessage;
  Param: PtrInt;
  Iter : TGtkTreeIter;
  TreeView: PGtkTreeView;
  ListStore: PGtkTreeModel;
  Path: PGtkTreePath;
  AState: TCheckBoxState;
begin
  {$IFDEF EventTrace}
  EventTrace('Gtk2WS_CheckListBoxToggle', WidgetInfo^.LCLObject);
  {$ENDIF}
  Val(arg1, Param);

  TreeView := PGtkTreeView(WidgetInfo^.CoreWidget);
  ListStore := gtk_tree_view_get_model(TreeView);

  if gtk_tree_model_iter_nth_child(ListStore, @Iter, nil, Param) then
    begin
      TCustomCheckListBox(WidgetInfo^.LCLObject).Toggle(Param);
      AState:=TCustomCheckListBox(WidgetInfo^.LCLObject).State[Param];

      gtk_list_store_set(ListStore, @Iter, [gtk2CLBState,
        Byte(AState), -1]);
    end;


  Path := gtk_tree_path_new_from_indices(Param, -1);
  if Path <> nil then
  begin
    if TreeView^.priv^.tree <> nil then
      gtk_tree_view_set_cursor(TreeView, Path, nil, False);
    gtk_tree_path_free(Path);
  end;

  FillChar(Mess{%H-}, SizeOf(Mess), #0);
  Mess.Msg := LM_CHANGED;

  Mess.Result := 0;
  Mess.WParam := Param;
  DeliverMessage(widgetInfo^.lclObject, Mess);

end;

class procedure TGtk2WSCustomCheckListBox.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
//var
//  Selection: PGtkTreeSelection;
begin
  TGtk2WSBaseScrollingWinControl.SetCallbacks(AGtkWidget,AWidgetInfo);

  {Selection :=} gtk_tree_view_get_selection(PGtkTreeView(AWidgetInfo^.CoreWidget));
  //SignalConnect(PGtkWidget(Selection), 'changed', @Gtk2WS_ListBoxChange, AWidgetInfo);
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
  Result := TGtk2WSBaseScrollingWinControl.CreateHandle(AWinControl,AParams);
  p := {%H-}PGtkWidget(Result);

  if Result = 0 then exit;

  WidgetInfo := GetWidgetInfo(p, False);

  GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(p)^.hscrollbar, GTK_CAN_FOCUS);
  GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(p)^.vscrollbar, GTK_CAN_FOCUS);
  gtk_scrolled_window_set_policy(PGtkScrolledWindow(p),
                                 GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_scrolled_window_set_shadow_type(PGtkScrolledWindow(p), GTK_SHADOW_IN);
  gtk_widget_show(p);

  liststore := gtk_list_store_new (4,
                          [G_TYPE_UCHAR, G_TYPE_STRING, G_TYPE_POINTER, G_TYPE_BOOLEAN, nil]);
  TreeViewWidget := gtk_tree_view_new_with_model(GTK_TREE_MODEL(liststore));
  g_object_unref(G_OBJECT(liststore));

  // Check Column
  renderer := gtk_cell_renderer_toggle_new();
  {$ifdef windows}
  // standard indicator size = 13 and its looks ugly under windows
  g_object_set(renderer, 'indicator-size', [14, nil]);
  {$endif}
  column := gtk_tree_view_column_new;
  gtk_tree_view_column_set_title(column, 'CHECKBTNS');
  gtk_tree_view_column_pack_start(column, renderer, True);
  gtk_tree_view_column_set_cell_data_func(column, renderer,
    @Gtk2WS_CheckListBoxDataFunc, WidgetInfo, nil);
  gtk_cell_renderer_toggle_set_active(GTK_CELL_RENDERER_TOGGLE(renderer), True);
  gtk_tree_view_append_column(GTK_TREE_VIEW(TreeViewWidget), column);
  gtk_tree_view_column_set_clickable(GTK_TREE_VIEW_COLUMN(column), True);

  SignalConnect(PGtkWidget(renderer), 'toggled', @Gtk2WS_CheckListBoxToggle, WidgetInfo);

  // Text Column
  renderer := LCLIntfCellRenderer_New; // gtk_cell_renderer_text_new();
  column := gtk_tree_view_column_new_with_attributes(
                             'LISTITEMS', renderer, ['text', gtk2CLBText, nil]);

  gtk_tree_view_column_set_cell_data_func(Column, renderer, TGtkTreeCellDataFunc(@LCLIntfCellRenderer_CellDataFunc), WidgetInfo, nil);
  gtk_tree_view_append_column(GTK_TREE_VIEW(TreeViewWidget), column);
  gtk_tree_view_column_set_clickable(GTK_TREE_VIEW_COLUMN(column), True);

  gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(TreeViewWidget), False);

  gtk_container_add(GTK_CONTAINER(p), TreeViewWidget);
  gtk_widget_show(TreeViewWidget);

  SetMainWidget(p, TreeViewWidget);
  GetWidgetInfo(p, True)^.CoreWidget := TreeViewWidget;

  Selection := gtk_tree_view_get_selection(PGtkTreeView(TreeViewWidget));

  case TCustomCheckListBox(AWinControl).MultiSelect of
    True : gtk_tree_selection_set_mode(Selection, GTK_SELECTION_MULTIPLE);
    False: gtk_tree_selection_set_mode(Selection, GTK_SELECTION_SINGLE);
  end;

  g_signal_connect_after(Selection, 'changed',
    G_CALLBACK(@Gtk2WS_CheckListBoxSelectionChanged), WidgetInfo);

  Set_RC_Name(AWinControl, P);
  if not AWinControl.HandleObjectShouldBeVisible and not (csDesigning in AWinControl.ComponentState) then
    gtk_widget_hide(p);
  SetCallbacks(p, WidgetInfo);
end;

class function TGtk2WSCustomCheckListBox.GetItemEnabled(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer): Boolean;
var
  Iter : TGtkTreeIter;
  TreeView: PGtkTreeView;
  WidgetInfo: PWidgetInfo;
  ListStore: PGtkTreeModel;
  Disabled: gboolean;
begin
  Result := True;
  WidgetInfo := GetWidgetInfo({%H-}PGtkWidget(ACheckListBox.Handle));

  TreeView := PGtkTreeView(WidgetInfo^.CoreWidget);
  ListStore := gtk_tree_view_get_model(TreeView);
  if gtk_tree_model_iter_nth_child(ListStore, @Iter, nil, AIndex) then
  begin
    gtk_tree_model_get(ListStore, @Iter, [gtk2CLBDisabled, @Disabled, -1]);
    Result := not Disabled;
  end;
end;

class function TGtk2WSCustomCheckListBox.GetState(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer
  ): TCheckBoxState;
var
  Iter : TGtkTreeIter;
  TreeView: PGtkTreeView;
  WidgetInfo: PWidgetInfo;
  ListStore: PGtkTreeModel;
  b: byte;
begin
  Result := cbUnchecked;
  WidgetInfo := GetWidgetInfo({%H-}PGtkWidget(ACheckListBox.Handle));

  TreeView := PGtkTreeView(WidgetInfo^.CoreWidget);
  ListStore := gtk_tree_view_get_model(TreeView);
  if gtk_tree_model_iter_nth_child(ListStore, @Iter, nil, AIndex) then
  begin
    gtk_tree_model_get(ListStore, @Iter, [gtk2CLBState, @b, -1]);
    Result := TCheckBoxState(b);
  end;
end;

class procedure TGtk2WSCustomCheckListBox.SetItemEnabled(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer;
  const AEnabled: Boolean);
var
  Iter : TGtkTreeIter;
  TreeView: PGtkTreeView;
  WidgetInfo: PWidgetInfo;
  ListStore: PGtkTreeModel;
  Disabled: gboolean;
begin
  WidgetInfo := GetWidgetInfo({%H-}PGtkWidget(ACheckListBox.Handle));

  TreeView := PGtkTreeView(WidgetInfo^.CoreWidget);
  ListStore := gtk_tree_view_get_model(TreeView);
  if gtk_tree_model_iter_nth_child(ListStore, @Iter, nil, AIndex) then begin
    Disabled:=not AEnabled;
    gtk_list_store_set(ListStore, @Iter, [gtk2CLBDisabled, Disabled, -1]);
  end;
end;

class procedure TGtk2WSCustomCheckListBox.SetState(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer;
  const AState: TCheckBoxState);
var
  Iter : TGtkTreeIter;
  TreeView: PGtkTreeView;
  WidgetInfo: PWidgetInfo;
  ListStore: PGtkTreeModel;
begin
  WidgetInfo := GetWidgetInfo({%H-}PGtkWidget(ACheckListBox.Handle));

  TreeView := PGtkTreeView(WidgetInfo^.CoreWidget);
  ListStore := gtk_tree_view_get_model(TreeView);
  if gtk_tree_model_iter_nth_child(ListStore, @Iter, nil, AIndex) then
  begin
    gtk_list_store_set(ListStore, @Iter, [gtk2CLBState, Byte(AState), -1]);
  end;
end;

end.
