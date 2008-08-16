{%MainUnit gtk2wscomctrls.pp}
{ $Id$

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

type
  TLVHack = class(TCustomListView)
  end;

////////////////////////////////////////
////   Event Code  /////////////////////
////////////////////////////////////////

procedure Gtk2_ItemCheckedChanged(renderer: PGtkCellRendererToggle; PathStr: Pgchar; WidgetInfo: PWidgetInfo);cdecl;
var
  Widgets: PTVWidgets;
  APath: PGtkTreePath;
  AValue: gboolean;
  Iter: TGtkTreeIter;
begin
  Widgets := PTVWidgets(WidgetInfo^.UserData);
  APath := gtk_tree_path_new_from_string(PathStr);
  with Widgets^ do
  begin
    if gtk_tree_model_get_iter(TreeModel, @Iter, APath) then
    begin
      gtk_tree_model_get(TreeModel, @Iter, [PGtkListStore(TreeModel)^.n_columns - 1,
        @AValue, -1]);
      AValue := not AValue;
      gtk_list_store_set(PGtkListStore(TreeModel), @Iter,
        [PGtkListStore(TreeModel)^.n_columns - 1, AValue, -1]);
    end;
  end;
end;

procedure Gtk2_ItemFocusChanged(treeview: PGtkTreeView; WidgetInfo: PWidgetInfo);cdecl;
var
  msg: TLMNotify;
  NM: TNMListView;
  path: PGtkTreePath;
  column: PGtkTreeViewColumn;
begin
  //DebugLn('Gtk2_ItemFocusChanged');
  // the defocus of the oldrow isn't send
  gtk_tree_view_get_cursor(treeview, path, column);
  if path = nil then exit;

  gtk_tree_path_free(path);

  msg.Msg := CN_NOTIFY;

  FillChar(NM, SizeOf(NM), 0);
  NM.hdr.hwndfrom := PtrUInt(WidgetInfo^.CoreWidget);
  NM.hdr.code := LVN_ITEMCHANGED;
  NM.iItem := StrToInt(gtk_tree_path_to_string(path));

  NM.iSubItem := 0;
  NM.uNewState := LVIS_FOCUSED;
  NM.uChanged := LVIF_STATE;
  msg.NMHdr := @NM.hdr;
  DeliverMessage(WidgetInfo^.LCLObject, msg);
end;

procedure Gtk2_ItemDeleted(model: PGtkTreeModel; path: PGtkTreePath; WidgetInfo: PWidgetInfo); cdecl;
var
  msg: TLMNotify;
  NM: TNMListView;
  ALV: TLVHack;
  Widgets: PTVWidgets;
begin
  //DebugLn('Gtk2_ItemDeleted');
  msg.Msg := CN_NOTIFY;

  FillChar(NM, SizeOf(NM), 0);
  NM.hdr.hwndfrom := PtrUInt(WidgetInfo^.CoreWidget);
  NM.hdr.code := LVN_DELETEITEM;
  NM.iItem := StrToInt(gtk_tree_path_to_string(path));
  msg.NMHdr := @NM.hdr;
  DeliverMessage(WidgetInfo^.LCLObject, msg);
  
  ALV := TLVHack(WidgetInfo^.LCLObject);
  
  if ALV.Items.Count = 0 then begin
    Widgets  := PTVWidgets(WidgetInfo^.UserData);
    Widgets^.ItemCache.Free;
    Widgets^.ItemCache := nil;
  end;
end;

procedure Gtk2_ItemInserted(model: PGtkTreeModel; path: PGtkTreePAth; Iter: PGtkTreeIter; WidgetInfo: PWidgetInfo); cdecl;
var
  msg: TLMNotify;
  NM: TNMListView;
begin
  //DebugLn('Gtk2_ItemInserted');
  msg.Msg := CN_NOTIFY;

  FillChar(NM, SizeOf(NM), 0);
  NM.hdr.hwndfrom := PtrUInt(WidgetInfo^.CoreWidget);
  NM.hdr.code := LVN_INSERTITEM;
  NM.iItem := StrToInt(gtk_tree_path_to_string(path));
  msg.NMHdr := @NM.hdr;
  DeliverMessage(WidgetInfo^.LCLObject, msg);
end;

//This is only for when the tree view sorts itself. Not needed since the LCL does the sorting.
//procedure Gtk2_ItemMoved(model: PGtkTreeModel; path: PGtkTreePAth; Iter: PGtkTreeIter; WidgetInfo: PWidgetInfo); cdecl;
//begin
//end;

procedure Gtk2_ItemChanged(model: PGtkTreeModel; path: PGtkTreePAth; Iter: PGtkTreeIter; WidgetInfo: PWidgetInfo); cdecl;
begin
 // OnChange Occurs immediately after an item in the list changes.
 // The Item parameter is the list item that just changed. The Change parameter
 // indicates the type of change that just occurred. Change is ctText if the
 // Caption property of the item changed. Change is ctImage if the ImageIndex
 // property of the item changed or the appropriate image list changed in the
 // list view. Change is ctState if the Cut, Focused, or Selected property of
 // the item changed.
 //DebugLn('Gtk2_ItemChanged');
end;


procedure Gtk2_ColumnClicked(column: PGtkTreeViewColumn; WidgetInfo: PWidgetInfo); cdecl;
var
  AColumn: TListColumn;
  msg: TLMNotify;
  NM: TNMListView;
begin
  AColumn := TListColumn(g_object_get_data(G_OBJECT(column), 'TListColumn'));

  msg.Msg := CN_NOTIFY;

  FillChar(NM, SizeOf(NM), 0);
  NM.hdr.hwndfrom := PtrUInt(WidgetInfo^.CoreWidget);
  NM.hdr.code := LVN_COLUMNCLICK;
  NM.iItem := -1;
  NM.iSubItem := AColumn.Index;
  msg.NMHdr := @NM.hdr;
  DeliverMessage(WidgetInfo^.LCLObject, msg);
end;

procedure Gtk2_ItemSelectionChanged(selection: PGtkTreeSelection; WidgetInfo: PWidgetInfo); cdecl;
var
  msg: TLMNotify;
  NM: TNMListView;
  Widgets: PTVWidgets;
  AIndex: String;
  i: Integer;
begin
  //DebugLn('Gtk2_ItemSelectionChanged');
  Widgets := PTVWidgets(WidgetInfo^.UserData);
  msg.Msg := CN_NOTIFY;
  if (widgets=nil) or (Widgets^.ItemCache=nil) or (Widgets^.ItemCache.Count=0) then
  begin
    debugln(' Gtk2_ItemSelectionChanged  ItemCache=nil ',tComponent(widgetInfo^.lclObject).name);
    exit;
  end;
  for i := 0 to Widgets^.ItemCache.Count -1 do
  begin
    FillChar(NM, SizeOf(NM), 0);
    NM.hdr.hwndfrom := PtrUInt(Widgets^.MainView);
    NM.hdr.code := LVN_ITEMCHANGED;
    AIndex := Widgets^.ItemCache.Strings[i];
    NM.iItem := StrToInt(AIndex);
    NM.iSubItem := 0;//AColumn;
    if Widgets^.ItemCache.Objects[i] <> nil then
      NM.uOldState := LVIS_SELECTED
    else
      NM.uNewState := LVIS_SELECTED;
    NM.uChanged := LVIF_STATE;
    msg.NMHdr := @NM.hdr;
    DeliverMessage(WidgetInfo^.LCLObject, msg);
  end;
  Widgets^.ItemCache.Clear;
end;

function Gtk2WSLV_ItemSelected(selection: PGtkTreeSelection; model: PGtkTreeModel;
           path: PGtkTreePath; path_is_currently_selected: GBoolean; WidgetInfo: PWidgetInfo): GBoolean; cdecl;
var
  Widgets: PTVWidgets;
  i: Integer;
  Str: String;
  msg: TLMNotify;
  NM: TNMListView;
begin
  //DebugLn('Gtk2_ItemSelected');
  // this function is called *before* the item is selected
  // The result should be True to allow the Item to change selection
  Result := True;

  Widgets := PTVWidgets(WidgetInfo^.UserData);
  Str := gtk_tree_path_to_string(path);
  
  
  FillChar(NM, SizeOf(NM), 0);
  NM.hdr.hwndfrom := PtrUInt(WidgetInfo^.CoreWidget);
  NM.hdr.code := LVN_ITEMCHANGING;
  NM.iItem := StrToInt(Str);
  NM.iSubItem := 0;//AColumn;
  NM.uNewState := LVIS_SELECTED;
  NM.uChanged := LVIF_STATE;
  msg.NMHdr := @NM.hdr;
  {Result := }DeliverMessage(WidgetInfo^.LCLObject, msg){ = 0};

  if not Result then Exit;

  // this stops a loop when you use the shift key to select multiple entries
  if gtk_tree_selection_get_mode(selection) = GTK_SELECTION_MULTIPLE then
  begin
    for i := 0 to Widgets^.ItemCache.Count-1 do
    begin
      if (Widgets^.ItemCache.Strings[i] = Str) and (Widgets^.ItemCache.Objects[i] = TObject(ptrint(Ord(path_is_currently_selected))))
      then begin
        Result := False;
        Exit;
      end;
    end;
  end;
  Widgets^.ItemCache.AddObject(Str,TObject(ptrint(Ord(path_is_currently_selected))));
end;

{ TGtk2WSCustomListView }

class function TGtk2WSCustomListView.IsIconView(const ALV: TCustomListView): Boolean;
begin
  Result := False;
  if TLVHack(ALV).ViewStyle in [vsIcon{, vsTile?}] then Result := True;
end;

class procedure TGtk2WSCustomListView.ReCreateListStore(const ALV: TCustomListView; const TVWidgets: PTVWidgets);
var
  GTypeArray: PGType;
  NewListStore: PGtkListStore;
  i, nColumns: Integer;
begin
  nColumns := (Max(1, TLVHack(ALV).Columns.Count) * 2) + 1; // +1 for checkboxes

  GetMem(GTypeArray, SizeOf(GType) * (nColumns + 1));
  
  for i := 0 to (nColumns div 2) - 1 do
  begin
    GTypeArray[i*2] := GDK_TYPE_PIXBUF;
    GTypeArray[(i*2)+1] := G_TYPE_STRING;
  end;

  GTypeArray[nColumns - 1] := G_TYPE_BOOLEAN; // checkboxes
  GTypeArray[nColumns] := 0;

  NewListStore := gtk_list_store_newv(nColumns, GTypeArray);
  FreeMem(GTypeArray);

  gtk_tree_view_set_model(PGtkTreeView(TVWidgets^.MainView), PGtkTreeModel(NewListStore));
  TVWidgets^.TreeModel := NewListStore;
end;

class procedure TGtk2WSCustomListView.ReCreateItems(const ALV: TCustomListView);
var
  Widgets: PTVWidgets;
  i,x: Integer;
  Item: TListItem;
begin
  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  
  // needed for when we are creating so items in the ListView are not added twice
  if gtk_tree_model_iter_n_children(GTK_TREE_MODEL(Widgets^.TreeModel), nil) = 0 then
    Exit;

  gtk_list_store_clear(PGtkListStore(Widgets^.TreeModel));
  for i := 0 to TLVHack(ALV).Items.Count-1 do
  begin
    Item := TLVHack(ALV).Items.Item[i];
    ItemInsert(ALV, i , Item);
    ItemSetText(ALV, i, Item, 0, Item.Caption);
    ItemSetChecked(ALV, i, Item, Item.Checked);
    for X := 0 to Min(Item.SubItems.Count-1, TLVHack(ALV).Columns.Count-2) do
      ItemSetText(ALV, i, Item, x+1, Item.SubItems.Strings[x]);
  end;
end;

class procedure TGtk2WSCustomListView.SetPropertyInternal(const ALV: TCustomListView;
  const Widgets: PTVWidgets; const AProp: TListViewProperty;
  const AIsSet: Boolean);
begin
  with Widgets^ do begin
    case AProp of
      lvpAutoArrange: begin
        // TODO: implement ??
      end;
      lvpCheckboxes:
        begin
          AddRemoveCheckboxRenderer(ALV, Widgets, AIsSet);
        end;
      lvpColumnClick: begin
        // allow only column modifications when in report mode
        if TLVHack(ALV).ViewStyle <> vsReport then Exit;
        gtk_tree_view_set_headers_clickable(PGtkTreeView(MainView), AIsSet);
      end;
      lvpFlatScrollBars: begin
        // TODO: implement ??
      end;
      lvpFullDrag: begin
        // TODO: implement ??
      end;
      lvpGridLines: begin
        // TODO: better implementation
        // maybe possible with some cellwidget hacking
        // this create rows with alternating colors
        gtk_tree_view_set_rules_hint(PGtkTreeView(MainView), AIsSet);
      end;
      lvpHideSelection: begin
        // TODO: implement
        // should be possible with some focus in/out events
      end;
      lvpHotTrack: begin
        // TODO: implement
        // should be possible with some mouse tracking
      end;
      lvpMultiSelect: begin
        if AIsSet
        then gtk_tree_selection_set_mode(TreeSelection, GTK_SELECTION_MULTIPLE)
        else gtk_tree_selection_set_mode(TreeSelection, GTK_SELECTION_SINGLE);
      end;
      lvpOwnerDraw: begin
        // TODO: implement
        // use custom images/widgets ?
      end;
      lvpReadOnly: begin
        // TODO: implement inline editor ?
      end;
      lvpRowSelect: begin
        // TODO: implement ???
        // how to do cell select
      end;
      lvpShowColumnHeaders: begin
        // allow only column modifications when in report mode
        if TLVHack(ALV).ViewStyle <> vsReport then Exit;
        gtk_tree_view_set_headers_visible(GTK_TREE_VIEW (MainView), AIsSet);
      end;
      lvpShowWorkAreas: begin
        // TODO: implement ???
      end;
      lvpWrapText: begin
        // TODO: implement ???
      end;
    end;
  end;
end;

class procedure TGtk2WSCustomListView.SetNeedDefaultColumn(
  const ALV: TCustomListView; const AValue: Boolean);
var
  Widgets: PTVWidgets;
  GtkColumn: PGtkTreeViewColumn;
  pixrenderer,
  textrenderer: PGtkCellRenderer;
begin
  if not WSCheckHandleAllocated(ALV, 'SetNeedDefaultColumn')
  then Exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  
  GtkColumn := g_object_get_data(G_OBJECT(Widgets^.MainView), 'LCL_DEFAULT_COLUMN');
  if AValue = True then
  begin
    if GtkColumn = nil then
    begin
      GtkColumn := gtk_tree_view_column_new();

      gtk_widget_unset_flags(PGtkWidget(GtkColumn), GTK_CAN_FOCUS);

      // add renderers
      pixrenderer := gtk_cell_renderer_pixbuf_new();
      textrenderer := gtk_cell_renderer_text_new();

      gtk_tree_view_column_pack_start(GtkColumn, pixrenderer, FALSE);
      gtk_tree_view_column_set_attributes(GtkColumn, pixrenderer,['pixbuf', 0, nil]);
      gtk_tree_view_column_pack_start(GtkColumn, textrenderer, True);
      gtk_tree_view_column_set_attributes(GtkColumn, textrenderer, ['text',1, nil]);

      // insert column
      gtk_tree_view_insert_column(GTK_TREE_VIEW(Widgets^.MainView), GtkColumn, 0);
      
      g_object_set_data(G_OBJECT(Widgets^.MainView), 'LCL_DEFAULT_COLUMN', GtkColumn);

    end;
  end
  else begin // No Column Needed
    if GtkColumn <> nil then
    begin
      gtk_tree_view_remove_column(PGtkTreeView(Widgets^.MainView), GtkColumn);
      g_object_set_data(G_OBJECT(Widgets^.MainView), 'LCL_DEFAULT_COLUMN', nil);
    end;
  end;
end;

class procedure TGtk2WSCustomListView.AddRemoveCheckboxRenderer(
  const ALV: TCustomListView; const Widgets: PTVWidgets; const Add: Boolean);
var
  togglerenderer,
  pixrenderer,
  textrenderer: PGtkCellRenderer;
  column: PGtkTreeViewColumn;
begin
  column := gtk_tree_view_get_column(PGtkTreeView(Widgets^.MainView), 0);

  if Add then
  begin
    togglerenderer := gtk_cell_renderer_toggle_new();
    // connect toggled signal
    g_signal_connect(togglerenderer, 'toggled', TGTKSignalFunc(@Gtk2_ItemCheckedChanged), GetWidgetInfo(PGtkWidget(ALV.Handle)));
    // add to column
    gtk_tree_view_column_pack_start(column, togglerenderer, FALSE);
    gtk_tree_view_column_set_attributes(column, togglerenderer, ['active', PGtkListStore(Widgets^.TreeModel)^.n_columns - 1, nil]);
    // move to begin
    gtk_cell_layout_reorder(GTK_CELL_LAYOUT(column), togglerenderer, 0);
  end
  else
  begin
    gtk_cell_layout_clear(GTK_CELL_LAYOUT(column));
    pixrenderer := gtk_cell_renderer_pixbuf_new();
    textrenderer := gtk_cell_renderer_text_new();

    gtk_tree_view_column_pack_start(column, pixrenderer, FALSE);
    gtk_tree_view_column_set_attributes(column, pixrenderer,['pixbuf', 0, nil]);
    gtk_tree_view_column_pack_start(column, textrenderer, True);
    gtk_tree_view_column_set_attributes(column, textrenderer, ['text',1, nil]);
  end;
end;

class procedure TGtk2WSCustomListView.SetCallbacks(const AScrollWidget: PGtkWidget;
  const Widgets: PTVWidgets; const AWidgetInfo: PWidgetInfo);
begin
  TGtkWSBaseScrollingWinControl.SetCallbacks(AScrollWidget, AWidgetInfo);
  TGtkWSWinControl.SetCallbacks(PGtkObject(Widgets^.MainView), TComponent(AWidgetInfo^.LCLObject));
  
  // the callbacks for OnColumnClick are set when the column is created in ColumnInsert
  
  gtk_tree_selection_set_select_function(Widgets^.TreeSelection,TGtkTreeSelectionFunc(@Gtk2WSLV_ItemSelected), gpointer(AWidgetInfo),nil);

  SignalConnect(PGtkWidget(Widgets^.TreeSelection), 'changed',           @Gtk2_ItemSelectionChanged,    AWidgetInfo);
  SignalConnect(PGtkWidget(Widgets^.TreeModel),     'row-changed',       @Gtk2_ItemChanged,             AWidgetInfo);
  SignalConnect(PGtkWidget(Widgets^.TreeModel),     'row-inserted',      @Gtk2_ItemInserted,            AWidgetInfo);
  SignalConnect(PGtkWidget(Widgets^.TreeModel),     'row-deleted',       @Gtk2_ItemDeleted,             AWidgetInfo);
  SignalConnect(PGtkWidget(Widgets^.MainView),      'toggle-cursor-row', @Gtk2_ItemFocusChanged,        AWidgetInfo);
end;

class procedure TGtk2WSCustomListView.ColumnDelete(const ALV: TCustomListView;
  const AIndex: Integer);
var
  Widgets: PTVWidgets;
  GtkColumn: PGtkTreeViewColumn;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnDelete')
  then Exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);

  ReCreateListStore(ALV, PTVWidgets(Widgets^.WidgetInfo^.UserData));
  ReCreateItems(ALV);

  with Widgets^ do
  begin
    GtkColumn := gtk_tree_view_get_column(PGtkTreeView(MainView), AIndex);
    if GtkColumn<>nil then
      gtk_tree_view_remove_column(PGtkTreeView(MainView), GtkColumn);
  end;
end;

class function TGtk2WSCustomListView.ColumnGetWidth(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn): Integer;
var
  Widgets: PTVWidgets;
  GtkColumn: PGtkTreeViewColumn;
  i: Integer;
begin
  Result := -1;

  if not WSCheckHandleAllocated(ALV, 'ColumnGetWidth')
  then Exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  with Widgets^ do begin
    i := AColumn.Index;
    GtkColumn := gtk_tree_view_get_column(PGtkTreeView(MainView), i);
    Result := gtk_tree_view_column_get_width(GtkColumn);
  end;
end;

class procedure TGtk2WSCustomListView.ColumnInsert(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn);
var
  Widgets: PTVWidgets;
  column: PGtkTreeViewColumn;
  pixrenderer,
  textrenderer: PGtkCellRenderer;
  RealIndex: Integer;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnInsert')
  then Exit;

  if AIndex < 0 then
    RealIndex := 0
  else
    RealIndex := AIndex * 2;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);

  if gtk_tree_model_get_n_columns(Widgets^.TreeModel) div 2 < TLVHack(ALV).Columns.Count then begin

    ReCreateListStore(ALV, PTVWidgets(Widgets^.WidgetInfo^.UserData));
    ReCreateItems(ALV);

    GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  end;

  column := gtk_tree_view_column_new();

  gtk_widget_unset_flags(PGtkWidget(column), GTK_CAN_FOCUS);

  // add renderers
  pixrenderer := gtk_cell_renderer_pixbuf_new();
  textrenderer := gtk_cell_renderer_text_new();

  gtk_tree_view_column_pack_start(column, pixrenderer, FALSE);
  gtk_tree_view_column_set_attributes(column, pixrenderer,['pixbuf', RealIndex, nil]);
  gtk_tree_view_column_pack_start(column, textrenderer, True);
  gtk_tree_view_column_set_attributes(column, textrenderer, ['text', RealIndex + 1, nil]);
  
  //store the TColumn in the column data for callbacks
  g_object_set_data(G_OBJECT(column), PChar('TListColumn'), gpointer(AColumn));
  
  // set callback for OnClick
  SignalConnect(PGtkWidget(column), 'clicked', @Gtk2_ColumnClicked, Widgets^.WidgetInfo);
  
  // insert column
  gtk_tree_view_insert_column(GTK_TREE_VIEW(Widgets^.MainView), Column, AIndex);

  //set clickable
  gtk_tree_view_column_set_clickable (GTK_TREE_VIEW_COLUMN (column), TRUE);
// dont set these here, it will be set by the lcl
(*
  // set title
  gtk_tree_view_column_set_title(column, PChar(AColumn.Caption));
  //set width
  gtk_tree_view_column_set_fixed_width(Column, AColumn.Width);
  // set Visible
  gtk_tree_view_column_set_visible(Column, AColumn.Visible);
  // set MinWidth
  if AColumn.MinWidth > 0 then
    gtk_tree_view_column_set_min_width(Column, AColumn.MinWidth);
  // set MaxWidth
  if AColumn.MaxWidth > 0 then
    gtk_tree_view_column_set_max_width(Column, AColumn.MaxWidth);

  //set resizable
  gtk_tree_view_column_set_resizable(GTK_TREE_VIEW_COLUMN (column), True);

*)
end;

class procedure TGtk2WSCustomListView.ColumnMove(const ALV: TCustomListView;
  const AOldIndex, ANewIndex: Integer; const AColumn: TListColumn);
var
  Widgets: PTVWidgets;
  Column: PGtkTreeViewColumn;
  PrevColumn: PGtkTreeViewColumn;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnMove')
  then Exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  with Widgets^ do begin
    Column := gtk_tree_view_get_column(PGtkTreeView(MainView), AOldIndex);
    if ANewIndex = 0 then  PrevColumn := nil
    else PrevColumn := gtk_tree_view_get_column(PGtkTreeView(MainView), ANewIndex-1);
    gtk_tree_view_move_column_after(PGtkTreeView(MainView), Column, PrevColumn);
  end;
end;

class procedure TGtk2WSCustomListView.ColumnSetAlignment(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn;
  const AAlignment: TAlignment);
var
  Widgets: PTVWidgets;
  GtkColumn: PGtkTreeViewColumn;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetAlignment')
  then Exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  with Widgets^ do begin
    GtkColumn := gtk_tree_view_get_column(PGtkTreeView(MainView), AIndex);

    gtk_tree_view_column_set_alignment(GtkColumn, AlignToGtkAlign(AAlignment));
  end;
  //DebugLn(['ColSetALignment AIndex=',AIndex,' (GtkColumn=nil)=', GtkColumn=nil]);

end;

class procedure TGtk2WSCustomListView.ColumnSetAutoSize(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AAutoSize: Boolean);
var
  Widgets: PTVWidgets;
  GtkColumn: PGtkTreeViewColumn;
  ColSizing: TGtkTreeViewColumnSizing;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetAutoSize')
  then Exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  with Widgets^ do begin
    GtkColumn := gtk_tree_view_get_column(PGtkTreeView(MainView), AIndex);
    if AAutoSize then
      ColSizing := GTK_TREE_VIEW_COLUMN_AUTOSIZE
    else
      ColSizing := GTK_TREE_VIEW_COLUMN_FIXED;

    gtk_tree_view_column_set_resizable(GTK_TREE_VIEW_COLUMN (GtkColumn), True);
    gtk_tree_view_column_set_sizing(GtkColumn, ColSizing);
  end;

end;

class procedure TGtk2WSCustomListView.ColumnSetCaption(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const ACaption: String);
var
  Widgets: PTVWidgets;
  GtkColumn: PGtkTreeViewColumn;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetCaption')
  then Exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  with Widgets^ do begin
    GtkColumn := gtk_tree_view_get_column(PGtkTreeView(MainView), AIndex);
    gtk_tree_view_column_set_title(GtkColumn, PChar(ACaption));
  end;
end;

class procedure TGtk2WSCustomListView.ColumnSetImage(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AImageIndex: Integer
  );
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetImage')
  then Exit;

  //DebugLn('TODO: Gtk2. TGtk2WSCustomListView.ColumnSetImage');
end;

class procedure TGtk2WSCustomListView.ColumnSetMaxWidth(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AMaxWidth: Integer);
var
  Widgets: PTVWidgets;
  GtkColumn: PGtkTreeViewColumn;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetMaxWidth')
  then Exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  with Widgets^ do begin
    GtkColumn := gtk_tree_view_get_column(PGtkTreeView(MainView), AIndex);
    gtk_tree_view_column_set_max_width(GtkColumn, AMaxWidth - Ord(AMaxWidth=0));
  end;
end;

class procedure TGtk2WSCustomListView.ColumnSetMinWidth(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AMinWidth: integer);
var
  Widgets: PTVWidgets;
  GtkColumn: PGtkTreeViewColumn;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetMinWidth')
  then Exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  with Widgets^ do begin
    GtkColumn := gtk_tree_view_get_column(PGtkTreeView(MainView), AIndex);
    gtk_tree_view_column_set_min_width(GtkColumn, AMinWidth - Ord(AMinWidth=0));
  end;
end;

class procedure TGtk2WSCustomListView.ColumnSetWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AWidth: Integer);
var
  Widgets: PTVWidgets;
  GtkColumn: PGtkTreeViewColumn;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetWidth')
  then Exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  with Widgets^ do begin
    GtkColumn := gtk_tree_view_get_column(PGtkTreeView(MainView), AIndex);
    gtk_tree_view_column_set_fixed_width(GtkColumn, AWidth + Ord(AWidth<1));
  end;
end;

class procedure TGtk2WSCustomListView.ColumnSetVisible(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AVisible: Boolean);
var
  Widgets: PTVWidgets;
  GtkColumn: PGtkTreeViewColumn;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetVisible')
  then Exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  with Widgets^ do begin
    GtkColumn := gtk_tree_view_get_column(PGtkTreeView(MainView), AIndex);
    if AVisible then
      g_object_set_data(G_OBJECT(GtkColumn), PChar('Visible'), gpointer(ptrint(1)))
    else
      g_object_set_data(G_OBJECT(GtkColumn), PChar('Visible'), gpointer(ptrint(0)));
    if TLVHack(ALV).ViewStyle = vsReport then begin
      gtk_tree_view_column_set_visible(GtkColumn, AVisible);
    end;
  end;
end;

class procedure TGtk2WSCustomListView.ItemDelete(const ALV: TCustomListView;
  const AIndex: Integer);
var
  Widgets: PTVWidgets;
  Iter: TGtkTreeIter;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemDelete')
  then Exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);

  with Widgets^ do begin
    if gtk_tree_model_iter_nth_child(TreeModel, @Iter, nil, AIndex) then
    begin
      gtk_list_store_remove(TreeModel, @Iter);
    end;
  end;
end;

class function TGtk2WSCustomListView.ItemGetChecked(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem): Boolean;
var
  Widgets: PTVWidgets;
  Iter: TGtkTreeIter;
  AValue: gboolean;
begin
  Result:=false;
  if not WSCheckHandleAllocated(ALV, 'ItemGetChecked')
  then Exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  with Widgets^ do
  begin
    gtk_tree_model_iter_nth_child(TreeModel, @Iter, nil, AIndex);
    gtk_tree_model_get(TreeModel, @Iter, [PGtkListStore(TreeModel)^.n_columns - 1,
      @AValue, -1]);
    Result := AValue;
  end;
end;

class function TGtk2WSCustomListView.ItemGetState(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const AState: TListItemState;
  out AIsSet: Boolean): Boolean;
var
  Widgets: PTVWidgets;
  Iter: TGtkTreeIter;
begin
  Result := False;

  if not WSCheckHandleAllocated(ALV, 'ItemGetState')
  then Exit;

  AIsSet := False;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);

  with Widgets^ do begin
    if gtk_tree_model_iter_nth_child(TreeModel, @Iter, nil, AIndex) then
    begin
      case AState of
        lisCut,
        lisDropTarget:
        begin
          //TODO: do something with the rowcolor ?
        end;

        lisFocused:
        begin
          AIsSet := gtk_tree_selection_iter_is_selected(TreeSelection, @Iter);//gtk2 iter has no focus??
          Result := True;
        end;

        lisSelected:
        begin
          AIsSet := gtk_tree_selection_iter_is_selected(TreeSelection, @Iter);
          Result := True;
        end;
      end;
    end;
  end;
end;

class procedure TGtk2WSCustomListView.ItemInsert(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem);
var
  Widgets: PTVWidgets;
  Iter: TGtkTreeIter;
//  BitImage: TBitmap;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemInsert')
  then Exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);

  with Widgets^ do
  begin
    if ItemCache = nil then
      ItemCache := TStringList.Create;
    if AIndex = -1 then
      gtk_list_store_append(PGtkListStore(TreeModel), @Iter)
    else
      gtk_list_store_insert(PGtkListStore(TreeModel), @Iter, AIndex);

{
    //Icon
    if  (TLVHack(ALV).SmallImages <> nil)
    and (AItem.ImageIndex > -1)
    then
    begin
      BitImage := TBitmap.Create;
      TLVHack(ALV).SmallImages.GetBitmap(AItem.ImageIndex,BitImage);

      gtk_list_store_set(TreeModel, @Iter,
            [0 ,PGdkPixmap(PGDIObject(BitImage.handle)^.GDIBitmapObject), -1]);
    end;
}
  end;
end;

class procedure TGtk2WSCustomListView.ItemSetChecked(
  const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem;
  const AChecked: Boolean);
var
  Widgets: PTVWidgets;
  Iter: TGtkTreeIter;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemSetChecked')
  then Exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  with Widgets^ do
  begin
    gtk_tree_model_iter_nth_child(TreeModel, @Iter, nil, AIndex);
    gtk_list_store_set(PGtkListStore(TreeModel), @Iter,
      [PGtkListStore(TreeModel)^.n_columns - 1, gboolean(Ord(AChecked)), -1]);
  end;
end;

class procedure TGtk2WSCustomListView.ItemSetImage(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const ASubIndex,
  AImageIndex: Integer);
var
  Widgets: PTVWidgets;
  Iter: TGtkTreeIter;
  BitImage: TBitmap;
  GPixBuf: PGDKPixBuf;
  Drawable: PGdkDrawable;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemSetImage')
  then Exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);

  //Icon
  if not gtk_tree_model_iter_nth_child(Widgets^.TreeModel, @Iter, nil, AIndex) then Exit;

  gpixbuf := nil;
  if  (TLVHack(ALV).SmallImages <> nil)
  and (AItem.ImageIndex > -1)
  then begin
    BitImage := TBitmap.Create;
    TLVHack(ALV).SmallImages.GetBitmap(AItem.ImageIndex, BitImage);
    case PGDIObject(BitImage.handle)^.GDIBitmapType of
      gbBitmap: Drawable := PGDIObject(BitImage.handle)^.GDIBitmapObject;
      gbPixmap: Drawable := PGDIObject(BitImage.handle)^.GDIPixmapObject.Image; {$IFDEF VerboseGtkToDos}{$note TODO add alpha mask}{$ENDIF}
    else
      Drawable := nil;
    end;
    
    if Drawable <> nil
    then GPixBuf := CreatePixbufFromDrawable(Drawable, nil, False, 0, 0, 0, 0, -1, -1);
  end;

  gtk_list_store_set(Widgets^.TreeModel, @Iter, [0, gpixbuf, -1]);
end;

class procedure TGtk2WSCustomListView.ItemSetState(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const AState: TListItemState;
  const AIsSet: Boolean);
var
  Widgets: PTVWidgets;
  Iter: TGtkTreeIter;
  Path: PGtkTreePath;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemSetState')
  then Exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);

  with Widgets^ do begin
    if gtk_tree_model_iter_nth_child(TreeModel, @Iter, nil, AIndex) then
    begin
      case AState of
        lisCut,
        lisDropTarget:
        begin
          //TODO: do something with the rowcolor ?
        end;

        lisFocused:
        begin
          //gtk2 iter has no focus??
          Path := gtk_tree_path_new_from_string(PChar(IntToStr(AIndex)));

          if AIsSet then begin
            gtk_tree_view_set_cursor(PGtkTreeView(MainView), Path, nil, False);
          end
          else begin
            gtk_tree_view_set_cursor(PGtkTreeView(MainView), nil, nil, False);
          end;
          gtk_tree_path_free(Path);
        end;

        lisSelected:
        begin
          if AIsSet then begin
            if not gtk_tree_selection_iter_is_selected(TreeSelection, @Iter) then
              gtk_tree_selection_select_iter(TreeSelection, @Iter);
          end
          else begin
            if gtk_tree_selection_iter_is_selected(TreeSelection, @Iter) then
              gtk_tree_selection_unselect_iter(TreeSelection, @Iter);
          end;
        end;
      end;
    end;
  end;
end;

class procedure TGtk2WSCustomListView.ItemSetText(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const ASubIndex: Integer;
  const AText: String);
var
  Widgets: PTVWidgets;
  Iter: TGtkTreeIter;
  Str: String;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemSetText')
  then Exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  with Widgets^ do
  begin
    gtk_tree_model_iter_nth_child(TreeModel, @Iter, nil, AIndex);
    if ASubIndex = 0 then
      Str := AItem.Caption
    else
    begin
      if AItem.Subitems.Count < ASubIndex then Exit;//
      Str := AItem.Subitems.Strings[ASubIndex-1];
    end;
    gtk_list_store_set(PGtkListStore(TreeModel), @Iter, [(ASubIndex*2)+1, PChar(Str), -1]);
  end;
end;

class procedure TGtk2WSCustomListView.ItemShow(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const PartialOK: Boolean);
var
  Widgets: PTVWidgets;
  Path: PGtkTreePath;
  StrIndex: String;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemShow')
  then Exit;

  //TODO check for partial visiblity. currently scrolls to the Item to make it fully visible
  StrIndex := IntToStr(AItem.Index);

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);

  with Widgets^ do begin
    Path := gtk_tree_path_new_from_string(PChar(StrIndex));
    gtk_tree_view_scroll_to_cell(PGtkTreeView(MainView),Path,nil,false,0,0);
    gtk_tree_path_free(Path);
  end;

end;

class function TGtk2WSCustomListView.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Widgets: PTVWidgets;
  ListView: TCustomListView;
  OrigScrollingData: PBaseScrollingWinControlData;
  //ListViewData: PCustomListViewData;
  //Allocation: TGTKAllocation;
  ScrollWidget: PGtkScrolledWindow;
  nColumns: Integer;
  GTypeArray: PGType;
  i: Integer;
begin
  ListView := TCustomListView(AWinControl as TCustomListView);

  Result := TGtkWSBaseScrollingWinControl.CreateHandle(AWinControl, AParams);
  if Result = 0 then Exit;

  ScrollWidget := PGtkScrolledWindow(Result);

  gtk_widget_unset_flags(ScrollWidget^.hscrollbar, GTK_CAN_FOCUS);
  gtk_widget_unset_flags(ScrollWidget^.vscrollbar, GTK_CAN_FOCUS);
  gtk_scrolled_window_set_policy(ScrollWidget, GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);

  gtk_widget_show(PGtkWidget(ScrollWidget));

  nColumns := (Max(1, TLVHack(ListView).Columns.Count) * 2) + 1;

  Widgets := nil;

  New(Widgets);
  with Widgets^ do
  begin
    ItemCache := nil;
    GetMem(GTypeArray, SizeOf(GType) * (nColumns + 1));
    for i := 0 to (nColumns div 2) - 1 do
    begin
      GTypeArray[i*2] := GDK_TYPE_PIXBUF;
      GTypeArray[(i*2)+1] := G_TYPE_STRING;
    end;
    GTypeArray[nColumns - 1] := G_TYPE_BOOLEAN;
    GTypeArray[nColumns] := 0; // -1;

    TreeModel := gtk_list_store_newv(nColumns, GTypeArray);
    ReAllocMem(GTypeArray, 0);

    MainView:=gtk_tree_view_new_with_model(TreeModel);
    //MainView:=gtk_tree_view_new;
    g_object_unref (G_OBJECT (TreeModel));

    TreeSelection := PGtkTreeSelection(gtk_tree_view_get_selection(PGtkTreeView(MainView)));

    gtk_container_add(GTK_CONTAINER(ScrollWidget),PGtkWidget(MainView));
  
    // create widget info
    // already created in TGtkWSBaseScrollingWinControl
    // Replace the ScrollingInfo with our info
    WidgetInfo := GetWidgetInfo(ScrollWidget);
    OrigScrollingData := WidgetInfo^.UserData;
    Widgets^.ScrollingData := OrigScrollingData^;

    WidgetInfo^.UserData := Widgets;
  
    Dispose(OrigScrollingData);
    WidgetInfo^.CoreWidget := PGtkWidget(MainView);
    gtk_object_set_data(Pointer(MainView), 'widgetinfo', WidgetInfo);
    //SetMainWidget(ScrollWidget, MainView);
    //GetWidgetInfo(ScrollWidget, True)^.CoreWidget := PGtkWidget(MainView);
    gtk_widget_show_all(PGtkWidget(MainView));
    
    SetCallbacks(PGtkWidget(ScrollWidget), Widgets, WidgetInfo);
   // SetCallbacks(PGtkWidget(MainView), Widgets^, WidgetInfo);
  end;

  (*
  New(Widgets);
  Widgets^.WidgetInfo := GetWidgetInfo(ScrollWidget);
  SetCallbacks(PGtkWidget(ScrollWidget), Widgets^, Widgets^.WidgetInfo);
  *)
end;

class procedure TGtk2WSCustomListView.DestroyHandle(
  const AWinControl: TWinControl);
var
  Widgets: PTVWidgets;
begin
  GetCommonTreeViewWidgets(PGtkWidget(AWinControl.Handle), Widgets);
  // on widget destroy we have no ItemDeleted notification and we must destroy ItemCache ourself
  // if things will change please remove this destroy
  Widgets^.ItemCache.Free;
  Widgets^.ItemCache := nil;
  TWSWinControlClass(ClassParent).DestroyHandle(AWinControl);
end;

class procedure TGtk2WSCustomListView.BeginUpdate(const ALV: TCustomListView);
var
  Widgets: PTVWidgets;
begin
  if not WSCheckHandleAllocated(ALV, 'BeginUpdate')
  then Exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  g_object_ref(Widgets^.TreeModel);

  gtk_tree_view_set_model(PGtkTreeView(Widgets^.MainView), nil);
end;

class procedure TGtk2WSCustomListView.EndUpdate(const ALV: TCustomListView);
var
  Widgets: PTVWidgets;
begin
  if not WSCheckHandleAllocated(ALV, 'EndUpdate')
  then Exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);

  gtk_tree_view_set_model(PGtkTreeView(Widgets^.MainView), Widgets^.TreeModel);
  g_object_unref(Widgets^.TreeModel);
end;

class function TGtk2WSCustomListView.GetBoundingRect(const ALV: TCustomListView
  ): TRect;
begin
  Result:=Rect(0,0,0,0);

  if not WSCheckHandleAllocated(ALV, 'GetBoundingRect')
  then Exit;

  //DebugLn('TODO: TGtk2WSCustomListView.GetBoundingRect');
end;

class function TGtk2WSCustomListView.GetDropTarget(const ALV: TCustomListView
  ): Integer;
var
  Widgets: PTVWidgets;
begin
  // TODO: implement
  Result := -1;

  if not WSCheckHandleAllocated(ALV, 'GetDropTarget')
  then Exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);

end;

class function TGtk2WSCustomListView.GetFocused(const ALV: TCustomListView): Integer;
var
  Widgets: PTVWidgets;
  Path: PGtkTreePath;
  Column: PGtkTreeViewColumn;
begin
  Result := -1;
  
  if not WSCheckHandleAllocated(ALV, 'GetFocused')
  then Exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);

  with Widgets^ do begin
    gtk_tree_view_get_cursor(PGtkTreeView(MainView), Path, Column);
    if Path <> nil then
    begin
      Result := StrToInt(PChar(Path));
      gtk_tree_path_free(Path);
    end;
  end;
end;

class function TGtk2WSCustomListView.GetHoverTime(const ALV: TCustomListView
  ): Integer;
var
  Widgets: PTVWidgets;
begin
  // TODO: implement
  Result := -1; // = default

  if not WSCheckHandleAllocated(ALV, 'GetHoverTime')
  then Exit;

  //DebugLn('TODO: TGtk2WSCustomListView.GetHoverTime');

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  with Widgets^ do begin

  end;

end;

class function TGtk2WSCustomListView.GetItemAt(const ALV: TCustomListView; x,
  y: integer): Integer;
var
  Widgets: PTVWidgets;
  ItemPath: PGtkTreePath;
  Column: PGtkTreeViewColumn;
begin
  Result := -1;
  if not WSCheckHandleAllocated(ALV, 'GetItemAt')
  then Exit;
  
  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  
  if gtk_tree_view_get_path_at_pos(PGtkTreeView(Widgets^.MainView), x, y, ItemPath, Column, nil, nil) then
  begin
    if ItemPath <> nil then
    begin
      Result := gtk_tree_path_get_indices(ItemPath)^;
      gtk_tree_path_free(ItemPath);
    end;
  end;
end;

class function TGtk2WSCustomListView.GetSelCount(const ALV: TCustomListView
  ): Integer;
var
  Widgets: PTVWidgets;
  AList: PGList;
begin
  Result := 0;

  if not WSCheckHandleAllocated(ALV, 'GetSelCount')
  then Exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  with Widgets^ do begin
    AList := gtk_tree_selection_get_selected_rows(TreeSelection, nil);
    Result := g_list_length(AList);
    g_list_free(AList);
  end;
end;

class function TGtk2WSCustomListView.GetSelection(const ALV: TCustomListView
  ): Integer;
var
  Widgets: PTVWidgets;
  Iter: TGtkTreeIter;
  Path: PGtkTreePath;
begin
  Result := -1;
  
  if not WSCheckHandleAllocated(ALV, 'GetSelection')
  then Exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);

  with Widgets^ do begin
    gtk_tree_selection_get_selected(TreeSelection, nil, @Iter);
    Path := gtk_tree_model_get_path(TreeModel, @Iter);
    Result := StrToInt(PChar(Path));
    gtk_tree_path_free(Path);
  end;
end;

class function TGtk2WSCustomListView.GetTopItem(const ALV: TCustomListView): Integer;
begin
  Result := -1;
  
  if not WSCheckHandleAllocated(ALV, 'GetTopItem')
  then Exit;
end;

class function TGtk2WSCustomListView.GetViewOrigin(const ALV: TCustomListView): TPoint;
var
  Widgets: PTVWidgets;
begin
  if not WSCheckHandleAllocated(ALV, 'GetViewOrigin')
  then begin
    Result := Point(0, 0);
    Exit;
  end;
  
  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  
  // not really needed to retrieve the adjustments, since the TVWidgets has this info also based on events
  Result := Point(Widgets^.ScrollingData.HValue, Widgets^.ScrollingData.VValue);
end;

class function TGtk2WSCustomListView.GetVisibleRowCount(const ALV: TCustomListView
  ): Integer;
begin
  Result := -1;

  if not WSCheckHandleAllocated(ALV, 'GetVisibleRowCount')
  then Exit;
end;

class procedure TGtk2WSCustomListView.SetAllocBy(const ALV: TCustomListView;
  const AValue: Integer);
begin
  if not WSCheckHandleAllocated(ALV, 'SetAllocBy')
  then Exit;
end;

class procedure TGtk2WSCustomListView.SetDefaultItemHeight(
  const ALV: TCustomListView; const AValue: Integer);
begin
  if not WSCheckHandleAllocated(ALV, 'SetDefaultItemHeight')
  then Exit;
end;

class procedure TGtk2WSCustomListView.SetHotTrackStyles(const ALV: TCustomListView;
  const AValue: TListHotTrackStyles);
begin
  if not WSCheckHandleAllocated(ALV, 'SetHotTrackStyles')
  then Exit;
end;

class procedure TGtk2WSCustomListView.SetHoverTime(const ALV: TCustomListView;
  const AValue: Integer);
begin
  if not WSCheckHandleAllocated(ALV, 'SetHoverTime')
  then Exit;
end;

class procedure TGtk2WSCustomListView.SetImageList(const ALV: TCustomListView;
  const AList: TListViewImageList; const AValue: TCustomImageList);
begin
  if not WSCheckHandleAllocated(ALV, 'SetImageList')
  then Exit;
end;

class procedure TGtk2WSCustomListView.SetProperty(const ALV: TCustomListView;
  const AProp: TListViewProperty; const AIsSet: Boolean);
var
  Widgets: PTVWidgets;
begin
  if not WSCheckHandleAllocated(ALV, 'SetProperty')
  then Exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);

  SetPropertyInternal(ALV, Widgets, AProp, AIsSet);
end;

class procedure TGtk2WSCustomListView.SetProperties(const ALV: TCustomListView;
  const AProps: TListViewProperties);
var
  Widgets: PTVWidgets;
  Prop: TListViewProperty;
begin
  if not WSCheckHandleAllocated(ALV, 'SetProperties')
  then Exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);

  for Prop := Low(Prop) to High(Prop) do
    SetPropertyInternal(ALV, Widgets, Prop, Prop in AProps);
end;

class procedure TGtk2WSCustomListView.SetScrollBars(const ALV: TCustomListView;
  const AValue: TScrollStyle);
var
  ScrollWidget: PGtkScrolledWindow;
  hPolicy, vPolicy: TGtkPolicyType;
begin
  if not WSCheckHandleAllocated(ALV, 'SetScrollBars')
  then Exit;

  ScrollWidget := PGtkScrolledWindow(ALV.Handle);

  case AValue of
    ssHorizontal, ssBoth: hPolicy := GTK_POLICY_ALWAYS;
    ssAutoHorizontal, ssAutoBoth: hPolicy := GTK_POLICY_AUTOMATIC;
  else
    hPolicy := GTK_POLICY_NEVER;
  end;

  case AValue of
    ssVertical, ssBoth: vPolicy := GTK_POLICY_ALWAYS;
    ssAutoVertical, ssAutoBoth: vPolicy := GTK_POLICY_AUTOMATIC;
  else
    vPolicy := GTK_POLICY_NEVER;
  end;

  gtk_scrolled_window_set_policy(ScrollWidget, hPolicy, vPolicy);
end;

class procedure TGtk2WSCustomListView.SetSort(const ALV: TCustomListView;
  const AType: TSortType; const AColumn: Integer);
begin
  if not WSCheckHandleAllocated(ALV, 'SetSort')
  then Exit;
//  inherited SetSort(ALV, AType, AColumn);
end;

class procedure TGtk2WSCustomListView.SetViewOrigin(const ALV: TCustomListView;
  const AValue: TPoint);
var
  Widgets: PTVWidgets;
begin
  if not WSCheckHandleAllocated(ALV, 'SetViewOrigin')
  then Exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  //DebugLn(['TGtk2WSCustomListView.SetViewOrigin ',GetWidgetDebugReport(Widgets^.MainView)]);
  if not GTK_WIDGET_REALIZED(Widgets^.MainView) then exit;
  gtk_tree_view_scroll_to_point(PGtkTreeView(Widgets^.MainView), AValue.X, AValue.Y);
end;

class procedure TGtk2WSCustomListView.SetViewStyle(const ALV: TCustomListView;
  const Avalue: TViewStyle);
  
  procedure ShowColumns(const Widgets: PTVWidgets; const Show: Boolean);
  var
    GtkColumn: PGtkTreeViewColumn;
    i: Integer;
  begin
    for i := 1 to TLVHack(ALV).Columns.Count-1 do begin
      GtkColumn := gtk_tree_view_get_column(PGtkTreeView(Widgets^.MainView), i);
      if GtkColumn=nil then continue;
      if not (Show)
      or (Show and (PtrUInt(g_object_get_data(G_OBJECT(GtkColumn),
                                             PChar('Visible')))<>0))
      then
        gtk_tree_view_column_set_visible(GtkColumn, Show);
    end;
  end;

var
  Widgets: PTVWidgets;
begin
  if not WSCheckHandleAllocated(ALV, 'SetViewStyle')
  then Exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  ShowColumns(Widgets, AValue = vsReport);
  with Widgets^ do begin

    case AValue of
      vsIcon, // TODO this can be a GtkIconView if someone implements it
      vsSmallIcon,
      vsList:
        begin
          SetNeedDefaultColumn(ALV, True);
          gtk_tree_view_set_headers_visible(GTK_TREE_VIEW (MainView), False);
        end;
      vsReport:
        begin
          SetNeedDefaultColumn(ALV, False);
          if TLVHack(ALV).ShowColumnHeaders = True then
            gtk_tree_view_set_headers_visible(GTK_TREE_VIEW (MainView), True);
        end;
    end;
  end;
//  inherited SetViewStyle(ALV, Avalue);
// this one is going to be fun because you have to free the GtkTreeView and Create GtkIconView etc depending on the new style
end;

{procedure TGtk2WSCustomListView.UpdateProperties(
  const ACustomListView: TCustomListView);
var
  Widgets: TTVWidgets;
  GtkColumn: PGtkTreeViewColumn;
  Column: TListColumn;
  Iter: TGtkTreeIter;
  Item: TListItem;
  X, Y: Integer;
  Count: Integer;
  BitImage: TBitmap;
begin
  if Not(ACustomListView.HandleAllocated) then exit;

  GetCommonTreeViewWidgets(PGtkWidget(ACustomListView.Handle), Widgets);
  with Widgets^ do begin
    // set up columns..
  for X := 0 to ACustomListView.Columns.Count-1 do begin;
    GtkColumn := gtk_tree_view_get_column(TreeView, X);
    Column := ACustomListView.Columns.Items[X];
    // set captions
    gtk_tree_view_column_set_title(GtkColumn, PChar(Column.Caption));
    // set column alignment
    gtk_tree_view_column_set_alignment(GtkColumn, AlignToGtkAlign(Column.Alignment));
    // set auto sizing
    case Column.AutoSize of
      // The gtk2 docs say that GTK_TREE_VIEW_COLUMN_AUTOSIZE is inefficient
      // for large views, so perhaps this should be
      // GTK_TREE_VIEW_COLUMN_GROW_ONLY
      True : gtk_tree_view_column_set_sizing(GtkColumn, GTK_TREE_VIEW_COLUMN_AUTOSIZE);
      //True : gtk_tree_view_column_set_sizing(GtkColumn, GTK_TREE_VIEW_COLUMN_GROW_ONLY);
      False: gtk_tree_view_column_set_sizing(GtkColumn, GTK_TREE_VIEW_COLUMN_FIXED);
    end;

    // set width
    gtk_tree_view_column_set_fixed_width(GtkColumn, Column.Width+Ord(Column.Width=0));
    // set Visible
    gtk_tree_view_column_set_visible(GtkColumn, Column.Visible);
    // set MinWidth
    gtk_tree_view_column_set_min_width(GtkColumn,
                                       Column.MinWidth-Ord(Column.MinWidth=0));
    // set MaxWidth
    gtk_tree_view_column_set_max_width(GtkColumn,
                                       Column.MaxWidth-Ord(Column.MaxWidth=0));
  end;

  // ViewStyle
  case ACustomListView.ViewStyle of
    vsReport:
      gtk_tree_view_set_headers_visible(TreeView, True);
    vsList:
      _set_headers_visible(TreeView, False);
  end;

  //sorting
  //TODO

  //multiselect
  case ACustomListView.MultiSelect of
    True : gtk_tree_selection_set_mode(TreeSelection, GTK_SELECTION_MULTIPLE);
    False: gtk_tree_selection_set_mode(TreeSelection, GTK_SELECTION_SINGLE);
    //GTK_SELECTION_NONE,
    //GTK_SELECTION_SINGLE,
    //GTK_SELECTION_BROWSE,
    //GTK_SELECTION_MULTIPLE
  end;

  //do items...

  for X := 0 to ACustomListView.Items.Count-1 do begin
    Item:= ACustomListView.Items.Item[X];
    if X = 0 then
      gtk_tree_model_get_iter_first(TreeModel, @Iter)
    else
      gtk_tree_model_iter_next(TreeModel, @Iter);

    //do image if one is assigned....
    if  (ACustomListView.SmallImages <> nil)
    and (Item.ImageIndex > -1)
    then
    begin
      BitImage := TBitmap.Create;
      ACustomListView.SmallImages.GetBitmap(Item.ImageIndex,BitImage);
      // this doesnt seem to be working :(
      gtk_list_store_set(TreeModel, @Iter,
            [0 ,PGdkPixmap(PGDIObject(BitImage.handle)^.GDIBitmapObject), -1]);
    end;

    //Item.Caption
    gtk_list_store_set(TreeModel, @Iter, [1, PChar(Item.Caption), -1]);
    Count := ACustomListView.Columns.Count;

    //Item.Subitems
    for Y := 2 to Count-1 do begin
      if (Y-2) >= Item.SubItems.Count then Break;
      gtk_list_store_set(TreeModel, @Iter, [Y, PChar(Item.SubItems.Strings[Y-2]), -1]);
    end;
  end;
  end;
end;}
