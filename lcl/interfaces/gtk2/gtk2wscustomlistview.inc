{%MainUnit gtk2wscomctrls.pp}
{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

type
  TLVHack = class(TCustomListView)
  end;
  TLVItemHack = class(TListItem)
  end;

////////////////////////////////////////
////   Event Code  /////////////////////
////////////////////////////////////////

function IsOldGtk2: Boolean;
begin
  Result := (gtk_major_version = 2) and (gtk_minor_version < 10);
end;

procedure Gtk2_PixBufFromBitmap(const BitImage:TBitmap; out pixbuf:PGdkPixbuf);
var
GDIObj: PGDIObject;
bitmap:PGdkBitmap;
Width, Height:gint;
pixmap: PGdkPixmap;
begin
GDIObj := {%H-}PGDIObject(BitImage.Handle);
  case GDIObj^.GDIBitmapType of
    gbBitmap:
      begin
        bitmap := GDIObj^.GDIBitmapObject;
        gdk_drawable_get_size(bitmap, @Width, @Height);
        pixbuf := CreatePixbufFromDrawable(bitmap, nil, False, 0, 0, 0, 0, Width, Height);
      end;
    gbPixmap:
      begin
        pixmap := GDIObj^.GDIPixmapObject.Image;
        if pixmap <> nil then
        begin
          gdk_drawable_get_size(pixmap, @Width, @Height);
          bitmap := CreateGdkMaskBitmap(BitImage.Handle, 0);
          pixbuf := CreatePixbufFromImageAndMask(pixmap, 0, 0, Width, Height, nil, Bitmap);
        end;
      end;
    gbPixbuf:
      begin
        pixbuf := gdk_pixbuf_copy(GDIObj^.GDIPixbufObject);
      end;
  end;
end;

procedure Gtk2_ItemCheckedChanged(renderer: PGtkCellRendererToggle; PathStr: Pgchar; WidgetInfo: PWidgetInfo);cdecl;
var
  LV: TLVHack;
  Index: Integer;
  ListItem: TLVItemHack;
  ARect: TGdkRectangle;
  R: TRect;
  x, y, cellw, cellh: gint;
begin
  LV := TLVHack(WidgetInfo^.LCLObject);
  Index := StrToInt(PathStr);
  ListItem := TLVItemHack(LV.Items.Item[Index]);
  if ListItem <> nil then
  begin
    ListItem.Checked := not ListItem.GetCheckedInternal;
    if Assigned(LV.OnItemChecked) then
      LV.OnItemChecked(TListView(WidgetInfo^.LCLObject), LV.Items.Item[Index]);

    // we must update renderer row, otherwise visually it looks different
    // if we change toggle state by keyboard (eg. pressing Space key)
    R := ListItem.DisplayRect(drBounds);
    ARect := GdkRectFromRect(R);
    gtk_cell_renderer_get_size(PGtkCellRenderer(renderer),
      WidgetInfo^.CoreWidget, @ARect, @x,@y, @cellw, @cellh);
    with R do
      gtk_widget_queue_draw_area(WidgetInfo^.CoreWidget, Left, Top, cellW, cellH);
  end;
end;

procedure Gtk2_ItemFocusChanged(Widget: PGtkWidget; WidgetInfo: PWidgetInfo);cdecl;
var
  msg: TLMNotify;
  NM: TNMListView;
  path: PGtkTreePath;
  pstr: PChar;
  column: PGtkTreeViewColumn;
  cell: PGtkCellRenderer;
begin
  // DebugLn('Gtk2_ItemFocusChanged');
  // the defocus of the oldrow isn't send
  if GTK_IS_TREE_VIEW(Widget) then begin
    path:=nil;
    column:=nil;
    gtk_tree_view_get_cursor(PGtkTreeView(Widget), path, column);
  end
  else
  if GTK_IS_ICON_VIEW(Widget) then begin
    path:=nil;
    cell:=nil;
    gtk_icon_view_get_cursor(PGtkIconView(Widget), path, cell);
  end
  else
    path := nil;

  if path = nil then
    Exit;

  gtk_tree_path_free(path);

  msg.Msg := CN_NOTIFY;

  FillChar(NM{%H-}, SizeOf(NM), 0);
  NM.hdr.hwndfrom := {%H-}PtrUInt(WidgetInfo^.CoreWidget);
  NM.hdr.code := LVN_ITEMCHANGED;
  pstr:=gtk_tree_path_to_string(path);
  NM.iItem := StrToInt(pstr);
  g_free(pstr);

  NM.iSubItem := 0;
  NM.uNewState := LVIS_FOCUSED;
  NM.uChanged := LVIF_STATE;
  msg.NMHdr := @NM.hdr;
  DeliverMessage(WidgetInfo^.LCLObject, msg);
end;

procedure Gtk2_ItemDeleted({%H-}model: PGtkTreeModel; path: PGtkTreePath; WidgetInfo: PWidgetInfo); cdecl;
var
  msg: TLMNotify;
  NM: TNMListView;
  pstr:PChar;
begin
  //DebugLn('Gtk2_ItemDeleted');
  msg.Msg := CN_NOTIFY;

  FillChar(NM{%H-}, SizeOf(NM), 0);
  NM.hdr.hwndfrom := {%H-}PtrUInt(WidgetInfo^.CoreWidget);
  NM.hdr.code := LVN_DELETEITEM;
  pstr := gtk_tree_path_to_string(path);
  NM.iItem := StrToInt(pstr);
  g_free(pstr);
  msg.NMHdr := @NM.hdr;
  DeliverMessage(WidgetInfo^.LCLObject, msg);
end;

procedure Gtk2_ItemInserted({%H-}model: PGtkTreeModel; path: PGtkTreePAth; {%H-}Iter: PGtkTreeIter; WidgetInfo: PWidgetInfo); cdecl;
var
  msg: TLMNotify;
  NM: TNMListView;
begin
  //DebugLn('Gtk2_ItemInserted');
  msg.Msg := CN_NOTIFY;

  FillChar(NM{%H-}, SizeOf(NM), 0);
  NM.hdr.hwndfrom := {%H-}PtrUInt(WidgetInfo^.CoreWidget);
  NM.hdr.code := LVN_INSERTITEM;
  NM.iItem := gtk_tree_path_get_indices(path)^;
  msg.NMHdr := @NM.hdr;
  DeliverMessage(WidgetInfo^.LCLObject, msg);
end;

//This is only for when the tree view sorts itself. Not needed since the LCL does the sorting.
//procedure Gtk2_ItemMoved(model: PGtkTreeModel; path: PGtkTreePAth; Iter: PGtkTreeIter; WidgetInfo: PWidgetInfo); cdecl;
//begin
//end;

procedure Gtk2_ItemChanged({%H-}model: PGtkTreeModel; {%H-}path: PGtkTreePAth; {%H-}Iter: PGtkTreeIter; {%H-}WidgetInfo: PWidgetInfo); cdecl;
begin
 // OnChange Occurs immediately after an item in the list changes.
 // The Item parameter is the list item that just changed. The Change parameter
 // indicates the type of change that just occurred. Change is ctText if the
 // Caption property of the item changed. Change is ctImage if the ImageIndex
 // property of the item changed or the appropriate image list changed in the
 // list view. Change is ctState if the Cut, Focused, or Selected property of
 // the item changed.
 // DebugLn('Gtk2_ItemChanged');
end;


procedure Gtk2_ColumnClicked(column: PGtkTreeViewColumn; WidgetInfo: PWidgetInfo); cdecl;
var
  AColumn: TListColumn;
  msg: TLMNotify;
  NM: TNMListView;
begin
  AColumn := TListColumn(g_object_get_data(G_OBJECT(column), 'TListColumn'));

  msg.Msg := CN_NOTIFY;

  FillChar(NM{%H-}, SizeOf(NM), 0);
  NM.hdr.hwndfrom := {%H-}PtrUInt(WidgetInfo^.CoreWidget);
  NM.hdr.code := LVN_COLUMNCLICK;
  NM.iItem := -1;
  NM.iSubItem := AColumn.Index;
  msg.NMHdr := @NM.hdr;
  DeliverMessage(WidgetInfo^.LCLObject, msg);
end;

procedure BroadcastListSelection(Target : Pointer; AHandle : HWND; AIndex : Integer;
  AState : Boolean);
var
  msg: TLMNotify;
  NM: TNMListView;
begin
  msg.Msg := CN_NOTIFY;

  FillChar(NM{%H-}, SizeOf(NM), 0);
  NM.hdr.hwndfrom := AHandle;
  NM.hdr.code := LVN_ITEMCHANGED;
  NM.iItem := AIndex;
  NM.iSubItem := 0;
  if AState then
    NM.uOldState := LVIS_SELECTED
  else
    NM.uNewState := LVIS_SELECTED;
  NM.uChanged := LVIF_STATE;
  msg.NMHdr := @NM.hdr;
  if g_object_get_data({%H-}PGObject(TWinControl(Target).Handle),'lcl_gtkwidget_in_update') = nil then
  begin
    DeliverMessage(Target, msg);
    if TLVHack(Target).Selected = TLVHack(Target).Items[AIndex] then
    begin
      NM.uOldState := 0;
      NM.uNewState := LVIS_FOCUSED;
      NM.uChanged := LVIF_STATE;
      msg.NMHdr := @NM.hdr;
      DeliverMessage(Target, msg);
    end else
    if TLVHack(Target).Selected = nil then
    begin
      NM.uOldState := LVIS_FOCUSED;
      NM.uNewState := 0;
      NM.uChanged := LVIF_STATE;
      msg.NMHdr := @NM.hdr;
      DeliverMessage(Target, msg);
    end;
  end;
end;

procedure Gtk2_ItemSelectionChanged(selection: PGtkTreeSelection; WidgetInfo: PWidgetInfo); cdecl;
var
  Widgets: PTVWidgets;
  AIndex: String;
  i: Integer;
  Indices: Integer;
  ListIndex: Integer;
  List: PgList;
  Path: PGtkTreePath;
begin
  // DebugLn('Gtk2_ItemSelectionChanged');
  Widgets := PTVWidgets(WidgetInfo^.UserData);

  if (widgets = nil) or (Widgets^.ItemCache = nil) or
    (Widgets^.ItemCache.Count=0) then
  begin
    // debugln(' Gtk2_ItemSelectionChanged  ItemCache=nil ',tComponent(widgetInfo^.lclObject).name);
    if IsOldGtk2 and (Widgets <> nil) and (Widgets^.ItemCache <> nil) then
    begin
      // debugLn('ItemsCache is valid ! count ',dbgs(Widgets^.ItemCache.Count));
      List := gtk_tree_selection_get_selected_rows(Selection, nil);
      if (List <> nil) then
      begin
        if Assigned(Widgets^.OldTreeSelection) then
        begin
          // we must iterate because of multiselections
          for i := 0 to g_list_length(Widgets^.OldTreeSelection) - 1 do
          begin
            Path := g_list_nth_data(Widgets^.OldTreeSelection, i);
            if Path <> nil then
            begin
              Indices := gtk_tree_path_get_indices(Path)^;
              ListIndex := Widgets^.ItemCache.IndexOf(IntToStr(Indices));
              if ListIndex = -1 then
                Widgets^.ItemCache.AddObject(IntToStr(Indices), TObject(1))
              else
                Widgets^.ItemCache.Objects[ListIndex] := TObject(1);
            end;
          end;
          g_list_free(Widgets^.OldTreeSelection);
          Widgets^.OldTreeSelection := g_list_alloc;
          // we must iterate because of multiselections
          for i := 0 to g_list_length(List) - 1 do
            g_list_append(Widgets^.OldTreeSelection, g_list_nth_data(List, i));
        end;
        // now compare new selection (add or set as selected)
        for i := 0 to g_list_length(List) - 1 do
        begin
          Path := g_list_nth_data(List, i);
          if Path <> nil then
          begin
            Indices := gtk_tree_path_get_indices(Path)^;
            ListIndex := Widgets^.ItemCache.IndexOf(IntToStr(Indices));
            if ListIndex = -1 then
              Widgets^.ItemCache.AddObject(IntToStr(Indices), TObject(0))
            else
              Widgets^.ItemCache.Objects[ListIndex] := TObject(0);
          end;
        end;
        g_list_free(List);
      end else
      begin
        // complete selection is clear !
        if Assigned(Widgets^.OldTreeSelection) then
        begin
          for i := 0 to g_list_length(Widgets^.OldTreeSelection) - 1 do
          begin
            Path := g_list_nth_data(Widgets^.OldTreeSelection, i);
            if Path <> nil then
            begin
              Indices := gtk_tree_path_get_indices(Path)^;
              ListIndex := Widgets^.ItemCache.IndexOf(IntToStr(Indices));
              if ListIndex = -1 then
                Widgets^.ItemCache.AddObject(IntToStr(Indices), TObject(1))
              else
                Widgets^.ItemCache.Objects[ListIndex] := TObject(1);
            end;
          end;
          g_list_free(Widgets^.OldTreeSelection);
          Widgets^.OldTreeSelection := g_list_alloc;
        end;
      end;
    end else
      Exit;
  end;

  // DebugLn('Gtk2_ItemSelectionChanged Trigger OnSelectItem ? ', dbgs(not (wwiInvalidEvent in Widgets^.WidgetInfo^.Flags)));
  // LCL sent selection !
  if wwiInvalidEvent in Widgets^.WidgetInfo^.Flags then
    exit;

  for i := 0 to Widgets^.ItemCache.Count -1 do
  begin
    AIndex := Widgets^.ItemCache.Strings[i];
    BroadcastListSelection(WidgetInfo^.LCLObject, {%H-}PtrUInt(Widgets^.MainView),
      StrToInt(AIndex), Widgets^.ItemCache.Objects[i] <> nil);
  end;
  Widgets^.ItemCache.Clear;
end;

procedure Gtk2_IconViewSelectionChanged(AIconView: PGtkIconView; WidgetInfo: PWidgetInfo); cdecl;
var
  Widgets: PTVWidgets;
  AIndex: String;
  i: Integer;
  List: PGList;
  Path: PGtkTreePath;
  pstr: PChar;
begin
  Widgets := PTVWidgets(WidgetInfo^.UserData);

  if (Widgets=nil) or (Widgets^.ItemCache=nil) or (Widgets^.ItemCache.Count=0) then
  begin
    if (Widgets^.ItemCache <> nil) and (Widgets^.ItemCache.Count = 0) then
    begin
      List := gtk_icon_view_get_selected_items(AIconView);
      if (List <> nil) then
      begin
        Path := PGtkTreePath(g_list_first(List)^.data);
        pstr := gtk_tree_path_to_string(path);
        Widgets^.ItemCache.Add(pstr);
        g_free(pstr);
        g_list_free(List);
      end else
        exit;
    end else
      Exit;
  end;

  // LCL already sent selection !
  if wwiInvalidEvent in Widgets^.WidgetInfo^.Flags then
    exit;

  for i := 0 to Widgets^.ItemCache.Count -1 do
  begin
    AIndex := Widgets^.ItemCache.Strings[i];
    BroadcastListSelection(WidgetInfo^.LCLObject, {%H-}PtrUInt(Widgets^.MainView),
      StrToInt(AIndex), Widgets^.ItemCache.Objects[i] <> nil);
  end;
  Widgets^.ItemCache.Clear;
end;

function Gtk2WSLV_ItemSelected({%H-}selection: PGtkTreeSelection; {%H-}model: PGtkTreeModel;
           path: PGtkTreePath; path_is_currently_selected: GBoolean; WidgetInfo: PWidgetInfo): GBoolean; cdecl;
var
  Widgets: PTVWidgets;
  i: Integer;
  Item: integer;
begin
  // DebugLn('Gtk2_ItemSelected ');
  // this function is called *before* the item is selected
  // The result should be True to allow the Item to change selection
  Result := True;

  Widgets := PTVWidgets(WidgetInfo^.UserData);
  Item := gtk_tree_path_get_indices(path)^;
  i := Widgets^.ItemCache.IndexOf(IntToStr(Item));
  if i = -1 Then
    Widgets^.ItemCache.AddObject(IntToStr(Item), TObject(PtrInt(Ord(path_is_currently_selected))))
  else
    Widgets^.ItemCache.Objects[i] := TObject(PtrInt(Ord(path_is_currently_selected)));
end;

procedure Gtk2WSLV_ListViewGetCheckedDataFunc({%H-}tree_column: PGtkTreeViewColumn;
  cell: PGtkCellRenderer; tree_model: PGtkTreeModel; iter: PGtkTreeIter; WidgetInfo: PWidgetInfo); cdecl;
var
  APath: PGtkTreePath;
  ListItem: TLVItemHack;
begin
  gtk_tree_model_get(tree_model, iter, [0, @ListItem, -1]);

  if (ListItem = nil) and TCustomListView(WidgetInfo^.LCLObject).OwnerData then
  begin
    APath := gtk_tree_model_get_path(tree_model,iter);
    ListItem := TLVItemHack(TCustomListView(WidgetInfo^.LCLObject).Items[gtk_tree_path_get_indices(APath)^]);
    gtk_tree_path_free(APath);
  end;

  if ListItem = nil then
    Exit;
  gtk_cell_renderer_toggle_set_active(PGtkCellRendererToggle(cell), ListItem.GetCheckedInternal);
end;


procedure Gtk2WSLV_ListViewGetPixbufDataFuncForColumn(tree_column: PGtkTreeViewColumn;
  cell: PGtkCellRenderer; tree_model: PGtkTreeModel; iter: PGtkTreeIter; WidgetInfo: PWidgetInfo); cdecl;
var
  ListItem: TListItem;
  Images: TList;
  Widgets: PTVWidgets;
  ListColumn: TListColumn;
  ImageIndex: Integer;
  ColumnIndex: Integer;
  APath: PGtkTreePath;
  ImageList: TCustomImageList;
  Bmp: TBitmap;
  pixbuf: PGdkPixbuf;
begin
  PGtkCellRendererPixbuf(cell)^.pixbuf := nil;
  Widgets := PTVWidgets(WidgetInfo^.UserData);
  gtk_tree_model_get(tree_model, iter, [0, @ListItem, -1]);

  ListColumn := TListColumn(g_object_get_data(G_OBJECT(tree_column), 'TListColumn'));
  if ListColumn = nil then
    Exit;
  ColumnIndex := ListColumn.Index;
  ImageList := nil;
  Images := Widgets^.Images;
  if TCustomListView(WidgetInfo^.LCLObject).OwnerData then
    ImageList := TLVHack(WidgetInfo^.LCLObject).SmallImages;
  if (Images = nil) and (ImageList = nil) then
  begin
    Exit;
  end;
  ImageIndex := -1;

  if (ListItem = nil) and TCustomListView(WidgetInfo^.LCLObject).OwnerData then
  begin
    APath := gtk_tree_model_get_path(tree_model,iter);
    ListItem := TCustomListView(WidgetInfo^.LCLObject).Items[gtk_tree_path_get_indices(APath)^];
    gtk_tree_path_free(APath);
  end;

  if ListItem = nil then
    Exit;

  if ColumnIndex = 0 then
    ImageIndex := ListItem.ImageIndex
  else
    if ColumnIndex -1 <= ListItem.SubItems.Count-1 then
      ImageIndex := ListItem.SubItemImages[ColumnIndex-1];

  if (ImageList <> nil) and
    (ImageIndex > -1) and (ImageIndex <= ImageList.Count-1) then
  begin
    Bmp := TBitmap.create;
    try
      pixbuf := nil;
      ImageList.GetBitmap(ImageIndex, Bmp);
      Gtk2_PixBufFromBitmap(Bmp,pixbuf);
      PGtkCellRendererPixbuf(cell)^.pixbuf :=pixbuf;
    finally
      Bmp.Free;
    end;
  end else
  if (ImageIndex > -1) and (ImageIndex <= Images.Count-1) then
    PGtkCellRendererPixbuf(cell)^.pixbuf := PGdkPixbuf(Images.Items[ImageIndex])
  else
    PGtkCellRendererPixbuf(cell)^.pixbuf := nil;
end;

procedure Gtk2WSLV_ListViewGetPixbufDataFuncForIconView({%H-}cell_layout:PGtkCellLayout;
  cell: PGtkCellRenderer; tree_model: PGtkTreeModel; iter: PGtkTreeIter; WidgetInfo: PWidgetInfo); cdecl;
var
  ListItem: TListItem;
  Images: TList;
  Widgets: PTVWidgets;
  ImageIndex: Integer;
  APath: PGtkTreePath;
begin
  PGtkCellRendererPixbuf(cell)^.pixbuf := nil;
  Widgets := PTVWidgets(WidgetInfo^.UserData);
  gtk_tree_model_get(tree_model, iter, [0, @ListItem, -1]);

  Images := Widgets^.Images;
  if Images = nil then
    Exit;
  ImageIndex := -1;

  if (ListItem = nil) and TCustomListView(WidgetInfo^.LCLObject).OwnerData then
  begin
    APath := gtk_tree_model_get_path(tree_model,iter);
    ListItem := TCustomListView(WidgetInfo^.LCLObject).Items[gtk_tree_path_get_indices(APath)^];
    gtk_tree_path_free(APath);
  end;

  if ListItem = nil then
    Exit;

  ImageIndex := ListItem.ImageIndex;

  if (ImageIndex > -1) and (ImageIndex <= Images.Count-1) then
    PGtkCellRendererPixbuf(cell)^.pixbuf := PGdkPixbuf(Images.Items[ImageIndex])
  else
    PGtkCellRendererPixbuf(cell)^.pixbuf := nil;
end;

{ TGtk2WSCustomListView }

class procedure TGtk2WSCustomListView.SetPropertyInternal(const ALV: TCustomListView;
  const Widgets: PTVWidgets; const AProp: TListViewProperty;
  const AIsSet: Boolean);
const
  BoolToSelectionMode: array[Boolean] of TGtkSelectionMode = (
    GTK_SELECTION_SINGLE,
    GTK_SELECTION_MULTIPLE
  );
begin
  with Widgets^ do begin
    case AProp of
      lvpAutoArrange: begin
        // TODO: implement ??
      end;
      lvpCheckboxes:
        begin
          if TLVHack(ALV).ViewStyle in [vsReport,vsList] then
            AddRemoveCheckboxRenderer(ALV, GetWidgetInfo(Widgets^.MainView), AIsSet);
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
        if GTK_IS_TREE_VIEW(MainView) then
        begin
          if gtk_tree_view_set_grid_lines <> nil then
          begin
            if AIsSet then
              gtk_tree_view_set_grid_lines(PGtkTreeView(MainView), GTK_TREE_VIEW_GRID_LINES_BOTH)
            else
              gtk_tree_view_set_grid_lines(PGtkTreeView(MainView), GTK_TREE_VIEW_GRID_LINES_NONE);

          end else
            gtk_tree_view_set_rules_hint(PGtkTreeView(MainView), AIsSet);
        end;
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
        if GTK_IS_TREE_VIEW(MainView) then
          gtk_tree_selection_set_mode(TreeSelection, BoolToSelectionMode[AIsSet])
        else
        if GTK_IS_ICON_VIEW(MainView) then
          gtk_icon_view_set_selection_mode(PGtkIconView(MainView), BoolToSelectionMode[AIsSet]);
      end;
      lvpOwnerDraw:
      begin
        // It must send CN_DRAWITEM with ItemID and proper rect of item
        // then LCL does all other stuff. Note that OwnerDraw should work only
        // in case of vsReport, according to embarcadero docs.
        // http://docwiki.embarcadero.com/Libraries/XE4/en/Vcl.ComCtrls.TCustomListView.OnDrawItem
        // NOTE: this is automatically handled by cell renderer (Gtk2CellRenderer).
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

class procedure TGtk2WSCustomListView.SetNeedDefaultColumn(const ALV: TCustomListView; const AValue: Boolean);
var
  Widgets: PTVWidgets;
  WidgetInfo: PWidgetInfo;
  GtkColumn: PGtkTreeViewColumn;
  pixrenderer,
  textrenderer: PGtkCellRenderer;
begin
  if not WSCheckHandleAllocated(ALV, 'SetNeedDefaultColumn')
  then Exit;

  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);
  WidgetInfo := GetWidgetInfo({%H-}PGtkWidget(ALV.Handle));
  
  GtkColumn := g_object_get_data(G_OBJECT(Widgets^.MainView), 'LCL_DEFAULT_COLUMN');
  if AValue then
  begin
    if GtkColumn = nil then
    begin
      GtkColumn := gtk_tree_view_column_new();

      gtk_widget_unset_flags(PGtkWidget(GtkColumn), GTK_CAN_FOCUS);

      // add renderers
      pixrenderer := gtk_cell_renderer_pixbuf_new();
      textrenderer := LCLIntfCellRenderer_New;

      if GTK_IS_TREE_VIEW(Widgets^.MainView) then
      begin
        gtk_tree_view_column_pack_start(GtkColumn, pixrenderer, FALSE);
        //gtk_tree_view_column_set_attributes(GtkColumn, pixrenderer,['pixbuf', 0, nil]);
        gtk_tree_view_column_pack_start(GtkColumn, textrenderer, True);
        //gtk_tree_view_column_set_attributes(GtkColumn, textrenderer, ['text',0, nil]);

        gtk_tree_view_column_set_cell_data_func(GtkColumn, pixrenderer,  TGtkTreeCellDataFunc(@Gtk2WSLV_ListViewGetPixbufDataFuncForColumn), WidgetInfo, nil);
        gtk_tree_view_column_set_cell_data_func(GtkColumn, textrenderer, TGtkTreeCellDataFunc(@LCLIntfCellRenderer_CellDataFunc), WidgetInfo, nil);

        // insert column
        gtk_tree_view_insert_column(GTK_TREE_VIEW(Widgets^.MainView), GtkColumn, 0);
      end
      else
      if GTK_IS_ICON_VIEW(Widgets^.MainView) then
      begin
        gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(Widgets^.MainView), pixrenderer, False);
        gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(Widgets^.MainView), textrenderer, True);
        gtk_cell_layout_set_cell_data_func(GTK_CELL_LAYOUT(Widgets^.MainView), pixrenderer, TGtkCellLayoutDataFunc(@Gtk2WSLV_ListViewGetPixbufDataFuncForIconView), WidgetInfo, nil);
        gtk_cell_layout_set_cell_data_func(GTK_CELL_LAYOUT(Widgets^.MainView), textrenderer, TGtkCellLayoutDataFunc(@LCLIntfCellRenderer_CellDataFunc), WidgetInfo, nil);
      end;
      g_object_set_data(G_OBJECT(Widgets^.MainView), 'LCL_DEFAULT_COLUMN', GtkColumn);
    end;
  end
  else begin // No Column Needed
    if GtkColumn <> nil then
    begin
      if GTK_IS_TREE_VIEW(Widgets^.MainView) and GTK_IS_TREE_VIEW_COLUMN(GtkColumn) then
        gtk_tree_view_remove_column(PGtkTreeView(Widgets^.MainView), GtkColumn)
      else
      if GTK_IS_TREE_VIEW_COLUMN(GtkColumn) and G_IS_OBJECT(GtkColumn) then
        g_object_unref(GtkColumn);
      g_object_set_data(G_OBJECT(Widgets^.MainView), 'LCL_DEFAULT_COLUMN', nil);
    end;
  end;
end;

class procedure TGtk2WSCustomListView.AddRemoveCheckboxRenderer(
  const ALV: TCustomListView; const WidgetInfo: PWidgetInfo; const Add: Boolean);
var
  togglerenderer,
  pixrenderer,
  textrenderer: PGtkCellRenderer;
  column: PGtkTreeViewColumn;
  renderers: PGList;
begin
  column := gtk_tree_view_get_column(PGtkTreeView(WidgetInfo^.CoreWidget), 0);

  if column = nil then
    Exit;

  renderers := gtk_tree_view_column_get_cell_renderers(column);
  textrenderer := PGtkCellRenderer(g_list_last(renderers)^.data);
  pixrenderer := PGtkCellRenderer(g_list_last(renderers)^.prev^.data);
  g_list_free(renderers);
  g_object_ref(G_OBJECT(pixrenderer));
  g_object_ref(G_OBJECT(textrenderer));

  if Add then
  begin
    gtk_cell_layout_clear(GTK_CELL_LAYOUT(column));
    togglerenderer := gtk_cell_renderer_toggle_new();

    gtk_tree_view_column_pack_start(column, togglerenderer, FALSE);
    gtk_tree_view_column_pack_start(column, pixrenderer, FALSE);
    gtk_tree_view_column_pack_start(column, textrenderer, True);
    gtk_tree_view_column_set_cell_data_func(column, togglerenderer, TGtkTreeCellDataFunc(@Gtk2WSLV_ListViewGetCheckedDataFunc), WidgetInfo, nil);
    gtk_tree_view_column_set_cell_data_func(column, pixrenderer,  TGtkTreeCellDataFunc(@Gtk2WSLV_ListViewGetPixbufDataFuncForColumn), WidgetInfo, nil);
    gtk_tree_view_column_set_cell_data_func(column, textrenderer, TGtkTreeCellDataFunc(@LCLIntfCellRenderer_CellDataFunc), WidgetInfo, nil);
    // connect toggled signal
    g_signal_connect(togglerenderer, 'toggled', TGTKSignalFunc(@Gtk2_ItemCheckedChanged), GetWidgetInfo({%H-}PGtkWidget(ALV.Handle)));

  end
  else
  begin
    gtk_cell_layout_clear(GTK_CELL_LAYOUT(column));

    gtk_tree_view_column_pack_start(column, pixrenderer, FALSE);
    gtk_tree_view_column_pack_start(column, textrenderer, True);
    gtk_tree_view_column_set_cell_data_func(column, pixrenderer,  TGtkTreeCellDataFunc(@Gtk2WSLV_ListViewGetPixbufDataFuncForColumn), WidgetInfo, nil);
    gtk_tree_view_column_set_cell_data_func(column, textrenderer, TGtkTreeCellDataFunc(@LCLIntfCellRenderer_CellDataFunc), WidgetInfo, nil);

  end;
  if G_IS_OBJECT(pixrenderer) then
    g_object_unref(G_OBJECT(pixrenderer));
  if G_IS_OBJECT(textrenderer) then
    g_object_unref(G_OBJECT(textrenderer));
end;

class function TGtk2WSCustomListView.GetViewModel(const AView: PGtkWidget): PGtkTreeModel;
begin
  if GTK_IS_TREE_VIEW(AView) then
    Result := gtk_tree_view_get_model(PGtkTreeView(AView))
  else
  if GTK_IS_ICON_VIEW(AView) then
    Result := gtk_icon_view_get_model(PGtkIconView(AView))
  else
    Result := nil;
end;

class procedure TGtk2WSCustomListView.SetListCallbacks(const AScrollWidget: PGtkWidget;
  const Widgets: PTVWidgets; const AWidgetInfo: PWidgetInfo);
begin
  TGtk2WSBaseScrollingWinControl.SetCallbacks(AScrollWidget, AWidgetInfo);
  TGtk2WSWinControl.SetCallbacks(PGtkObject(Widgets^.MainView), TComponent(AWidgetInfo^.LCLObject));
  
  // the callbacks for OnColumnClick are set when the column is created in ColumnInsert

  if GTK_IS_TREE_VIEW(Widgets^.MainView) then
  begin
    if IsOldGtk2 then
      // bug in 2.8, issue #19820
    else
      gtk_tree_selection_set_select_function(Widgets^.TreeSelection,TGtkTreeSelectionFunc(@Gtk2WSLV_ItemSelected), gpointer(AWidgetInfo),nil);
    SignalConnect(PGtkWidget(Widgets^.TreeSelection), 'changed',           @Gtk2_ItemSelectionChanged,    AWidgetInfo);
    SignalConnect(PGtkWidget(Widgets^.MainView),      'toggle-cursor-row', @Gtk2_ItemFocusChanged,        AWidgetInfo);
  end
  else
  if GTK_IS_ICON_VIEW(Widgets^.MainView) then
  begin
    SignalConnect(Widgets^.MainView, 'selection-changed',  @Gtk2_IconViewSelectionChanged,    AWidgetInfo);
    SignalConnect(Widgets^.MainView, 'toggle-cursor-item', @Gtk2_ItemFocusChanged,        AWidgetInfo);
  end;
  SignalConnect(PGtkWidget(Widgets^.TreeModel), 'row-changed',  @Gtk2_ItemChanged,  AWidgetInfo);
  SignalConnect(PGtkWidget(Widgets^.TreeModel), 'row-inserted', @Gtk2_ItemInserted, AWidgetInfo);
  SignalConnect(PGtkWidget(Widgets^.TreeModel), 'row-deleted',  @Gtk2_ItemDeleted,  AWidgetInfo);
end;

class procedure TGtk2WSCustomListView.ColumnDelete(const ALV: TCustomListView;
  const AIndex: Integer);
var
  Widgets: PTVWidgets;
  GtkColumn: PGtkTreeViewColumn;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnDelete')
  then Exit;

  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);

  if not GTK_IS_TREE_VIEW(Widgets^.MainView) then
    Exit;

  GtkColumn := gtk_tree_view_get_column(PGtkTreeView(Widgets^.MainView), AIndex);
  if (GtkColumn<>nil) and GTK_IS_TREE_VIEW_COLUMN(GtkColumn) then
    gtk_tree_view_remove_column(PGtkTreeView(Widgets^.MainView), GtkColumn);
end;

class function TGtk2WSCustomListView.ColumnGetWidth(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn): Integer;
var
  Widgets: PTVWidgets;
  GtkColumn: PGtkTreeViewColumn;
begin
  Result := -1;

  if not WSCheckHandleAllocated(ALV, 'ColumnGetWidth')
  then Exit;

  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);
  if not GTK_IS_TREE_VIEW(Widgets^.MainView) then
    Exit;
  GtkColumn := gtk_tree_view_get_column(PGtkTreeView(Widgets^.MainView), AIndex);
  if GtkColumn <> nil then
  begin
    Result := gtk_tree_view_column_get_width(GtkColumn);
    if Result = 0 then
      Result := gtk_tree_view_column_get_fixed_width(GtkColumn);
  end;
end;

class procedure TGtk2WSCustomListView.ColumnInsert(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn);
var
  Widgets: PTVWidgets;
  column: PGtkTreeViewColumn;
  pixrenderer,
  textrenderer: PGtkCellRenderer;
  WidgetInfo: PWidgetInfo;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnInsert')
  then Exit;

  WidgetInfo := GetWidgetInfo({%H-}PGtkWidget(ALV.Handle));
  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);

  if not GTK_IS_TREE_VIEW(Widgets^.MainView) then
    Exit;

  column := gtk_tree_view_column_new();

  gtk_widget_unset_flags(PGtkWidget(column), GTK_CAN_FOCUS);

  // add renderers
  pixrenderer := gtk_cell_renderer_pixbuf_new();
  textrenderer := LCLIntfCellRenderer_New;//gtk_cell_renderer_text_new();

  gtk_tree_view_column_pack_start(column, pixrenderer, False);
  //gtk_tree_view_column_set_attributes(column, pixrenderer,['pixbuf', RealIndex, nil]);
  gtk_tree_view_column_pack_start(column, textrenderer, True);
  //gtk_tree_view_column_set_attributes(column, textrenderer, ['text', 0, nil]);

  gtk_tree_view_column_set_cell_data_func(column, pixrenderer,  TGtkTreeCellDataFunc(@Gtk2WSLV_ListViewGetPixbufDataFuncForColumn), WidgetInfo, nil);
  gtk_tree_view_column_set_cell_data_func(column, textrenderer, TGtkTreeCellDataFunc(@LCLIntfCellRenderer_CellDataFunc), WidgetInfo, nil);
  //gtk_tree_view_column_set_cell_data_func(column, textrenderer, TGtkTreeCellDataFunc(@Gtk2WSLV_ListViewGetTextDataFunc), WidgetInfo, nil);

  
  //store the TColumn in the column data for callbacks
  g_object_set_data(G_OBJECT(column), PChar('TListColumn'), gpointer(AColumn));
  
  // set callback for OnClick
  SignalConnect(PGtkWidget(column), 'clicked', @Gtk2_ColumnClicked, Widgets^.WidgetInfo);
  
  // insert column
  gtk_tree_view_insert_column(GTK_TREE_VIEW(Widgets^.MainView), Column, AIndex);

  //set clickable
  gtk_tree_view_column_set_clickable (GTK_TREE_VIEW_COLUMN (column), TRUE);
// do not set these here, it will be set by the lcl
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
  if not WSCheckHandleAllocated(ALV, 'ColumnMove') then Exit;

  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);
  if not GTK_IS_TREE_VIEW(Widgets^.MainView) then Exit;

  Column := gtk_tree_view_get_column(PGtkTreeView(Widgets^.MainView), AOldIndex);
  if Column <> nil then
  begin
    if ANewIndex = 0 then
      PrevColumn := nil
    else
      PrevColumn := gtk_tree_view_get_column(PGtkTreeView(Widgets^.MainView), ANewIndex);
    gtk_tree_view_move_column_after(PGtkTreeView(Widgets^.MainView), Column, PrevColumn);
  end;
end;

class procedure TGtk2WSCustomListView.ColumnSetAlignment(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn;
  const AAlignment: TAlignment);
var
  Widgets: PTVWidgets;
  GtkColumn: PGtkTreeViewColumn;
  Alignment: gfloat;
  Value: TGValue;
  renderers: PGList;
  textrenderer: PGtkCellRenderer;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetAlignment')
  then Exit;

  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);

  if not GTK_IS_TREE_VIEW(Widgets^.MainView) then
    Exit;

  GtkColumn := gtk_tree_view_get_column(PGtkTreeView(Widgets^.MainView), AIndex);
  if GtkColumn <> nil then
  begin
    renderers := gtk_tree_view_column_get_cell_renderers(GtkColumn);
    textrenderer := PGtkCellRenderer(g_list_last(renderers)^.data);
    g_list_free(renderers);

    Alignment := AlignToGtkAlign(AAlignment);
    Value.g_type := G_TYPE_FLOAT;
    Value.data[0].v_float:= Alignment;
    g_object_set_property(G_OBJECT(textrenderer), PChar('xalign'), @Value);

    {now we call set alignment because it calls update over visible rows in col}
    gtk_tree_view_column_set_alignment(GtkColumn, Alignment);
  end;
end;

class procedure TGtk2WSCustomListView.ColumnSetAutoSize(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AAutoSize: Boolean);
const
  SizingMap: array[Boolean] of TGtkTreeViewColumnSizing = (
    GTK_TREE_VIEW_COLUMN_FIXED,
    GTK_TREE_VIEW_COLUMN_AUTOSIZE
  );
var
  Widgets: PTVWidgets;
  GtkColumn: PGtkTreeViewColumn;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetAutoSize')
  then Exit;

  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);

  if not GTK_IS_TREE_VIEW(Widgets^.MainView) then
    Exit;

  GtkColumn := gtk_tree_view_get_column(PGtkTreeView(Widgets^.MainView), AIndex);
  if GtkColumn <> nil then
  begin
    gtk_tree_view_column_set_resizable(GTK_TREE_VIEW_COLUMN(GtkColumn), True);
    gtk_tree_view_column_set_sizing(GtkColumn, SizingMap[AAutoSize]);
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

  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);

  if not GTK_IS_TREE_VIEW(Widgets^.MainView) then
    Exit;

  GtkColumn := gtk_tree_view_get_column(PGtkTreeView(Widgets^.MainView), AIndex);
  if GtkColumn <> nil then
    gtk_tree_view_column_set_title(GtkColumn, PChar(ACaption));
end;

class procedure TGtk2WSCustomListView.ColumnSetImage(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AImageIndex: Integer
  );
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetImage')
  then Exit;

  // ToDo: TGtk2WSCustomListView.ColumnSetImage
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

  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);
  if not GTK_IS_TREE_VIEW(Widgets^.MainView) then
    Exit;
  GtkColumn := gtk_tree_view_get_column(PGtkTreeView(Widgets^.MainView), AIndex);
  if GtkColumn <> nil then
    gtk_tree_view_column_set_max_width(GtkColumn, AMaxWidth - Ord(AMaxWidth=0));
end;

class procedure TGtk2WSCustomListView.ColumnSetMinWidth(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AMinWidth: integer);
var
  Widgets: PTVWidgets;
  GtkColumn: PGtkTreeViewColumn;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetMinWidth')
  then Exit;

  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);
  if not GTK_IS_TREE_VIEW(Widgets^.MainView) then
    Exit;
  GtkColumn := gtk_tree_view_get_column(PGtkTreeView(Widgets^.MainView), AIndex);
  if GtkColumn <> nil then
    gtk_tree_view_column_set_min_width(GtkColumn, AMinWidth - Ord(AMinWidth=0));
end;

class procedure TGtk2WSCustomListView.ColumnSetWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AWidth: Integer);
var
  Widgets: PTVWidgets;
  GtkColumn: PGtkTreeViewColumn;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetWidth')
  then Exit;

  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);
  if not GTK_IS_TREE_VIEW(Widgets^.MainView) then
    Exit;
  GtkColumn := gtk_tree_view_get_column(PGtkTreeView(Widgets^.MainView), AIndex);
  if GtkColumn <> nil then
  begin
    GtkColumn^.width := 0;
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

  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);
  if not GTK_IS_TREE_VIEW(Widgets^.MainView) then
    Exit;
  with Widgets^ do
  begin
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
  {$IFDEF USEORIGTREEMODEL}
  Iter: TGtkTreeIter;
  {$ENDIF}
begin
  if not WSCheckHandleAllocated(ALV, 'ItemDelete')
  then Exit;

  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);

  with Widgets^ do begin
    {$IFDEF USEORIGTREEMODEL}
    if gtk_tree_model_iter_nth_child(TreeModel, @Iter, nil, AIndex) then
    begin
      gtk_list_store_remove(TreeModel, @Iter);
    end;
    {$ELSE}
    PLCLListViewModel(TreeModel)^.NotifyRowDeleted(AIndex);
    {$ENDIF}
  end;
end;

class function TGtk2WSCustomListView.ItemDisplayRect(const ALV: TCustomListView; const AIndex, ASubItem: Integer;
  ACode: TDisplayCode): TRect;
var
  Widgets: PTVWidgets;
  ItemRect, IconRect: TGdkRectangle;
  Column: PGtkTreeViewColumn;
  Path: PGtkTreePath;
  X, Y, L, T, W, H: GInt;
  ARect: TGdkRectangle;
  R: TRect;
  ANewCell: PGtkCellRenderer;
  ANewPath: PGtkTreePath;
  APGList: PGList;
  pixrenderer: PGtkCellRenderer;
  AWidth: gint;
  AHeight: gint;
begin
  Result := Rect(0, 0, 0, 0);
  if not WSCheckHandleAllocated(ALV, 'ItemDisplayRect') then
    Exit;

  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);
  with Widgets^ do
  begin
    if gtk_widget_realized(MainView) = False then
      exit;
    Path := gtk_tree_path_new_from_indices(AIndex, -1);
    try
      if GTK_IS_TREE_VIEW(MainView) then
      begin
        Column := gtk_tree_view_get_column(PGtkTreeView(MainView), ASubItem);
        gtk_tree_view_get_cell_area(PGtkTreeView(MainView), Path, Column, @ItemRect);
        if gtk_tree_view_get_headers_visible(PGtkTreeView(MainView)) then
        begin
          gtk_tree_view_column_cell_get_size(gtk_tree_view_get_column(PGtkTreeView(MainView), 0),
            @ARect, @L, @T, @W, @H);
          inc(ItemRect.y, H);
        end;

        if (ACode in [drIcon, drLabel]) and not TLVHack(ALV).OwnerDraw then
        begin
          IconRect := ItemRect;
          APGList := gtk_tree_view_column_get_cell_renderers(gtk_tree_view_get_column(PGtkTreeView(MainView), 0));
          pixrenderer := PGtkCellRenderer(g_list_last(APGList)^.prev^.data);
          gtk_cell_renderer_get_fixed_size(pixrenderer, @AWidth, @AHeight);
          if AWidth > 0 then
            IconRect.Width := AWidth - 2;
          if AHeight > 0 then
            IconRect.Height := AHeight - 2;
          g_list_free(APGList);
          if ACode = drIcon then
            ItemRect := IconRect
          else
          begin
            ItemRect.x += AWidth + 2;
            ItemRect.y += 2; // offset
            ItemRect.Width -= AWidth + 2;
            ItemRect.Height -= 2; // offset
          end;
        end;

      end
      else
      if GTK_IS_ICON_VIEW(MainView) then
      begin
        ItemRect.x := 0;
        ItemRect.y := 0;
        ItemRect.width := gtk_icon_view_get_item_width(PGtkIconView(MainView));
        ItemRect.height := 0;
        if Path <> nil then
        begin
          ANewPath := nil;
          ANewCell := nil;
          R := ALV.ClientRect;
          l := 0;
          t := 0;
          Result := Rect(0, 0, 0, 0);
          while t < R.Bottom - 1 do
          begin
            l := 0;
            while l < R.Right - 1 do
            begin
              if gtk_icon_view_get_item_at_pos(PGtkIconView(MainView), l, t, ANewPath, ANewCell) then
              begin
                if (ANewPath <> nil) and (gtk_tree_path_compare(Path, ANewPath) = 0) then
                begin
                  gtk_cell_renderer_get_size(ANewCell, PGtkWidget(MainView), @ItemRect, @x,@y,@w,@h);
                  Result := Rect(l, t, w + l, h + t);
                  ItemRect := GdkRectFromRect(Result);
                  if ANewPath <> nil then
                    gtk_tree_path_free(ANewPath);
                  break;
                end;
                if ANewPath <> nil then
                  gtk_tree_path_free(ANewPath);
              end;
              inc(l, 1);
            end;

            inc(t, 1);

            if not IsRectEmpty(Result) then
              break;
          end;
        end;
      end;
    finally
      gtk_tree_path_free(Path);
    end;
    Result := RectFromGdkRect(ItemRect);
  end;
end;

class procedure TGtk2WSCustomListView.ItemExchange(const ALV: TCustomListView;
  AItem: TListItem; const AIndex1, AIndex2: Integer);
var
  Widgets: PTVWidgets;
  Path: PGtkTreePath;
  ItemRect: TGdkRectangle;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemExchange') then
    exit;
  // gtk2 needs only update
  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);
  if GetViewModel(Widgets^.MainView) = nil then
    exit;

  if not gtk_widget_realized(Widgets^.MainView) then
    exit;

  if GTK_IS_TREE_VIEW(Widgets^.MainView) then
  begin
    Path := gtk_tree_path_new_from_indices(AIndex1, -1);
    gtk_tree_view_get_cell_area(PGtkTreeView(Widgets^.MainView), Path, nil, @ItemRect);
    gtk_tree_path_free(Path);
    if ItemRect.height = 0 then
    begin
      Path := gtk_tree_path_new_from_indices(AIndex2, -1);
      gtk_tree_view_get_cell_area(PGtkTreeView(Widgets^.MainView), Path, nil, @ItemRect);
      gtk_tree_path_free(Path);
    end;
  end else
    ItemRect.height := 1; // force redraw

  if ItemRect.height <> 0 then // item is visible
    gtk_widget_queue_draw(Widgets^.MainView);
end;

class procedure TGtk2WSCustomListView.ItemMove(const ALV: TCustomListView;
  AItem: TListItem; const AFromIndex, AToIndex: Integer);
var
  Widgets: PTVWidgets;
  Path: PGtkTreePath;
  ItemRect: TGdkRectangle;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemMove') then
    exit;
  // gtk2 needs only update
  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);
  if GetViewModel(Widgets^.MainView) = nil then
    exit;

  if not gtk_widget_realized(Widgets^.MainView) then
    exit;

  if GTK_IS_TREE_VIEW(Widgets^.MainView) then
  begin
    Path := gtk_tree_path_new_from_indices(AFromIndex, -1);
    gtk_tree_view_get_cell_area(PGtkTreeView(Widgets^.MainView), Path, nil, @ItemRect);
    gtk_tree_path_free(Path);
    if ItemRect.height = 0 then
    begin
      Path := gtk_tree_path_new_from_indices(AToIndex, -1);
      gtk_tree_view_get_cell_area(PGtkTreeView(Widgets^.MainView), Path, nil, @ItemRect);
      gtk_tree_path_free(Path);
    end;
  end else
    ItemRect.height := 1; // force redraw

  if ItemRect.height <> 0 then // item is visible
    gtk_widget_queue_draw(Widgets^.MainView);
end;

class function TGtk2WSCustomListView.ItemGetChecked(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem): Boolean;
begin
  Result := TLVItemHack(AItem).GetCheckedInternal;
end;

class function TGtk2WSCustomListView.ItemGetState(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const AState: TListItemState;
  out AIsSet: Boolean): Boolean;
var
  Widgets: PTVWidgets;
  Path: PGtkTreePath;
  pstr: PChar;
  Column: PGtkTreeViewColumn;
  Cell: PGtkCellRenderer;
begin
  Result := False;

  if not WSCheckHandleAllocated(ALV, 'ItemGetState')
  then Exit;

  AIsSet := False;

  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);

  with Widgets^ do
  begin
    if GetViewModel(MainView) = nil then
      Exit; // we are in the midst of a begin update end update pair and the following will fail and cause gtk debug messages
    case AState of
      lisCut,
      lisDropTarget:
      begin
        //TODO: do something with the rowcolor ?
      end;

      lisFocused:
      begin
        if GTK_IS_TREE_VIEW(MainView) then begin
          Path:=nil;
          Column:=nil;
          gtk_tree_view_get_cursor(PGtkTreeView(MainView), Path, Column)
        end
        else
        if GTK_IS_ICON_VIEW(MainView) then
          gtk_icon_view_get_cursor(PGtkIconView(MainView), Path, Cell{%H-})
        else
          Path := nil;
        if (Path<>nil) then
        begin
          pstr := gtk_tree_path_to_string(path);
          AIsSet:=(StrToInt(pstr) = AIndex);
          g_free(pstr);
        end else
          AIsSet:=false;

        gtk_tree_path_free(Path);
        Result := True;
      end;

      lisSelected:
      begin
        Path := gtk_tree_path_new_from_string(PChar(IntToStr(AIndex)));
        if GTK_IS_TREE_VIEW(MainView) then
          AIsSet := gtk_tree_selection_path_is_selected(TreeSelection, Path)
        else
        if GTK_IS_ICON_VIEW(MainView) then
          AIsSet := gtk_icon_view_path_is_selected(PGtkIconView(MainView), Path)
        else
          AIsSet := False;
        gtk_tree_path_free(Path);
        Result := True;
      end;
    end;
  end;
end;

class procedure TGtk2WSCustomListView.ItemInsert(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem);
var
  Widgets: PTVWidgets;
  {$IFDEF USEORIGTREEMODEL}
  Iter: TGtkTreeIter;
  Index: Integer;
  {$ENDIF}
begin
  if not WSCheckHandleAllocated(ALV, 'ItemInsert')
  then Exit;


  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);

  with Widgets^ do
  begin
    {$IFDEF USEORIGTREEMODEL}
    if AIndex = -1 then
      Index := gtk_tree_model_iter_n_children(TreeModel, nil)
    else
      Index := AIndex;
    gtk_list_store_insert_with_values(PGtkListStore(TreeModel), @Iter, Index, 0, Pointer(AItem), -1);
    {$ELSE}
    if not (lisfWSItemsCreated in ALV.Items.Flags) then
      Exit;

    PLCLListViewModel(TreeModel)^.NotifyRowInserted(AIndex);
    {$ENDIF}
  end;
end;

class procedure TGtk2WSCustomListView.ItemSetChecked(
  const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem;
  const AChecked: Boolean);
begin
  if not WSCheckHandleAllocated(ALV, 'ItemSetChecked')
  then Exit;
  // nothing needed here
end;

class procedure TGtk2WSCustomListView.ItemSetImage(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const ASubIndex,
  AImageIndex: Integer);
var
  Widgets: PTVWidgets;
  Path: PGtkTreePath;
  ItemRect: TGdkRectangle;
  BitImage: TBitmap;
  pixbuf: PGDKPixBuf;
  i, ImgListWidth: Integer;
  ImgList: TCustomImageList;
  ImgListRes: TCustomImageListResolution;

begin
  if not WSCheckHandleAllocated(ALV, 'ItemSetImage')
  then Exit;
  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);
  with Widgets^ do
  begin
    if not gtk_widget_realized(MainView) then
    begin
      // DebugLn('WARNING: TGtk2WSCustomListView.ItemSetImage: MainView is not realized.');
      Exit;
    end;
    Path := gtk_tree_path_new_from_indices(AIndex, -1);
    if GTK_IS_TREE_VIEW(MainView) then
      gtk_tree_view_get_cell_area(PGtkTreeView(MainView), Path, nil, @ItemRect)
    else
      ItemRect.height := 1; // force redraw
    gtk_tree_path_free(Path);
    if ItemRect.height <> 0 then // item is visible
    begin
      if (TListView(ALV).ViewStyle in [vsSmallIcon, vsReport, vsList]) then
      begin
        ImgList := TListView(ALV).SmallImages;
        ImgListWidth := TListView(ALV).SmallImagesWidth;
      end
      else
      if (TListView(ALV).ViewStyle = vsIcon) then
      begin
        ImgList := TListView(ALV).LargeImages;
        ImgListWidth := TListView(ALV).LargeImagesWidth;
      end;

      if Assigned(ImgList) and (ImgList.Count > 0) and (AImageIndex >= 0) then
      begin
        ImgListRes := ImgList.ResolutionForPPI[ImgListWidth, ALV.Font.PixelsPerInch, ALV.GetCanvasScaleFactor].Resolution;
        if (ImgList.Count <> Widgets^.Images.Count) then
        begin
          if (TListView(ALV).ViewStyle in [vsSmallIcon, vsReport, vsList]) then
            SetImageList(ALV, lvilSmall, ImgListRes)
          else
            SetImageList(ALV, lvilLarge, ImgListRes);
          exit;
        end;

        if (Widgets^.Images <> nil) then
        begin
          for i := 0 to Widgets^.Images.Count-1 do
            if i = AImageIndex then
              gdk_pixbuf_unref(PGdkPixBuf(Widgets^.Images.Items[i]));

          pixbuf := nil;
          BitImage := TBitmap.Create;
          try
            ImgListRes.GetBitmap(AImageIndex, BitImage);
            Gtk2_PixBufFromBitmap(BitImage,pixbuf);
            Widgets^.Images.Items[AImageIndex] := pixbuf;
            if GTK_IS_TREE_VIEW(MainView) then
              gtk_tree_view_column_queue_resize(gtk_tree_view_get_column(PGtkTreeView(MainView), ASubIndex));
          finally
            BitImage.Free;
          end;
        end;
      end;
      gtk_widget_queue_draw(MainView);
    end;
  end;
end;

class procedure TGtk2WSCustomListView.ItemSetState(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const AState: TListItemState;
  const AIsSet: Boolean);
var
  Widgets: PTVWidgets;
  Path: PGtkTreePath;
  BroadcastMsg: Boolean;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemSetState')
  then Exit;

  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);
  // wwiInvalidEvent flag save us from infinite loop !
  // when this flag is included TreeSelection 'changed' won't
  // trigger - and it shouldn't LCL setted up selection.
  // fixes #16399
  Include(Widgets^.WidgetInfo^.Flags, wwiInvalidEvent);
  try
    BroadcastMsg := False;
    with Widgets^ do
    begin
      if GetViewModel(MainView) = nil then
        Exit; // we are in the midst of a begin update end update pair and the following will fail and cause gtk debug messages
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
          if GTK_IS_TREE_VIEW(MainView) then
            gtk_tree_view_set_cursor(PGtkTreeView(MainView), Path, nil, False)
          else
          if GTK_IS_ICON_VIEW(MainView) then
            gtk_icon_view_set_cursor(PGtkIconView(MainView), Path, nil, False);
          gtk_tree_path_free(Path);
        end;

        lisSelected:
        begin
          Path := gtk_tree_path_new_from_string(PChar(IntToStr(AIndex)));
          if GTK_IS_TREE_VIEW(MainView) then
          begin
            if AIsSet and not gtk_tree_selection_path_is_selected(TreeSelection, Path) then
            begin
              gtk_tree_selection_select_path(TreeSelection, Path);
              BroadcastMsg := True;
            end else
            if not AIsSet and gtk_tree_selection_path_is_selected(TreeSelection, Path) then
            begin
              gtk_tree_selection_unselect_path(TreeSelection, Path);
              BroadcastMsg := True;
            end;
          end
          else
          if GTK_IS_ICON_VIEW(MainView) then
          begin
            if AIsSet and not gtk_icon_view_path_is_selected(PGtkIconView(MainView), Path) then
            begin
              gtk_icon_view_select_path(PGtkIconView(MainView), Path);
              BroadCastMsg := True;
            end else
            if not AIsSet and gtk_icon_view_path_is_selected(PGtkIconView(MainView), Path) then
            begin
              gtk_icon_view_unselect_path(PGtkIconView(MainView), Path);
              BroadCastMsg := True;
            end;
          end;
          gtk_tree_path_free(Path);
          if BroadcastMsg then
            BroadCastListSelection(ALV, {%H-}PtrUInt(MainView), AIndex, not AIsSet);
        end;
      end;
    end;
  finally
    Exclude(Widgets^.WidgetInfo^.Flags, wwiInvalidEvent);
  end;
end;

class procedure TGtk2WSCustomListView.ItemSetText(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const ASubIndex: Integer;
  const AText: String);
var
  Widgets: PTVWidgets;
  Path: PGtkTreePath;
  ItemRect: TGdkRectangle;
begin
  // ToDo: TGtk2WSCustomListView.ItemSetText: this function queues a draw. Is this correct?

  if not WSCheckHandleAllocated(ALV, 'ItemSetText')
  then Exit;

  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);
  with Widgets^ do
  begin
    if not gtk_widget_realized(MainView) then
      Exit;

    if GTK_IS_TREE_VIEW(MainView) then
    begin
      Path := gtk_tree_path_new_from_indices(AIndex, -1);
      gtk_tree_view_get_cell_area(PGtkTreeView(MainView), Path, nil, @ItemRect);
      gtk_tree_path_free(Path);
    end
    else
      ItemRect.height := 1; // force redraw

    if ItemRect.height <> 0 then // item is visible
      gtk_widget_queue_draw(MainView);
  end;
end;

class procedure TGtk2WSCustomListView.ItemShow(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const PartialOK: Boolean);
var
  Widgets: PTVWidgets;
  Path: PGtkTreePath;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemShow')
  then Exit;

  // TODO: TGtk2WSCustomListView.ItemShow check for partial visiblity. currently scrolls to the Item to make it fully visible
  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);

  with Widgets^ do
  begin
    Path := gtk_tree_path_new_from_indices(AIndex, -1);
    if GTK_IS_TREE_VIEW(MainView) then
      gtk_tree_view_scroll_to_cell(PGtkTreeView(MainView), Path, nil, False, 0, 0)
    else
    if GTK_IS_ICON_VIEW(MainView) then
      gtk_icon_view_scroll_to_path(PGtkIconView(MainView), Path, False, 0, 0);
    gtk_tree_path_free(Path);
  end;
end;

class function TGtk2WSCustomListView.ItemGetPosition(const ALV: TCustomListView; const AIndex: Integer): TPoint;
var
  Widgets: PTVWidgets;
  Path: PGtkTreePath;
  ARect: TGdkRectangle;
  Column: PGtkTreeViewColumn;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemGetPosition')
  then Exit;

  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);

  Path := gtk_tree_path_new_from_indices(AIndex, -1);
  with Widgets^ do
  begin
    if GTK_IS_TREE_VIEW(MainView) then
    begin
      Column := gtk_tree_view_get_column(PGtkTreeView(MainView), 0);
      gtk_tree_view_get_cell_area(PGtkTreeView(MainView), Path, Column, @ARect);
      Result.X := ARect.x;
      Result.Y := Arect.y;
    end
    else
    if GTK_IS_ICON_VIEW(MainView) then
    begin
      // todo: gtk gives no way to get item rectangle, while internally it uses it
      Result.X := 0;
      Result.Y := 0;
    end;
  end;
  gtk_tree_path_free(Path);
end;

class procedure TGtk2WSCustomListView.ItemUpdate(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem);
{$IFDEF USEORIGTREEMODEL}
var
  Widgets: PTVWidgets;
  Iter: TGtkTreeIter;
  {$ENDIF}
begin
  if not WSCheckHandleAllocated(ALV, 'ItemUpdate')
  then Exit;
  {$IFDEF USEORIGTREEMODEL}
  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);

  with Widgets^ do
    if gtk_tree_model_iter_nth_child(TreeModel, @Iter, nil, AIndex) then
      gtk_list_store_set(PGtkListStore(TreeModel), @Iter, [0, Pointer(AItem), -1]);
  {$ENDIF}
end;

class function TGtk2WSCustomListView.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Widgets: PTVWidgets;
  OrigScrollingData: PBaseScrollingWinControlData;
  //ListViewData: PCustomListViewData;
  //Allocation: TGTKAllocation;
  ScrollWidget: PGtkScrolledWindow;
  {$IFDEF USEORIGTREEMODEL}
  PtrType: GType;
  {$ENDIF}
  SS: TPoint;
begin
  Result := TGtk2WSBaseScrollingWinControl.CreateHandle(AWinControl, AParams);
  if Result = 0 then Exit;

  ScrollWidget := {%H-}PGtkScrolledWindow(Result);

  gtk_widget_unset_flags(ScrollWidget^.hscrollbar, GTK_CAN_FOCUS);
  gtk_widget_unset_flags(ScrollWidget^.vscrollbar, GTK_CAN_FOCUS);

  SS := Gtk2TranslateScrollStyle(TListView(AWinControl).ScrollBars);
  gtk_scrolled_window_set_policy(ScrollWidget,SS.X, SS.Y);

  gtk_scrolled_window_set_shadow_type(ScrollWidget,
    BorderStyleShadowMap[TCustomListView(AWinControl).BorderStyle]);

  gtk_widget_show(PGtkWidget(ScrollWidget));

  Widgets := nil;

  New(Widgets);
  with Widgets^ do
  begin
    ItemCache := TStringList.Create;
    Images := nil;
    OldTreeSelection := nil;

    {$IFDEF USEORIGTREEMODEL}
    PtrType := G_TYPE_POINTER;
    TreeModel := gtk_list_store_newv(1, @PtrType);
    {$ELSE}
    TreeModel:= LCLListViewModelNew(TCustomListView(AWinControl));
    {$ENDIF}

    if TLVHack(AWinControl).ViewStyle in [vsIcon,vsSmallIcon] then
    begin
      MainView := gtk_icon_view_new_with_model(TreeModel);
      TreeSelection := nil;
      if TLVHack(AWinControl).IconOptions.Arrangement = iaTop then
        gtk_icon_view_set_columns(PGtkIconView(MainView), -1)
      else
        gtk_icon_view_set_columns(PGtkIconView(MainView), 1);
    end
    else
    begin
      if IsOldGtk2 then
        OldTreeSelection := g_list_alloc;
      MainView := gtk_tree_view_new_with_model(TreeModel);
      TreeSelection := PGtkTreeSelection(gtk_tree_view_get_selection(PGtkTreeView(MainView)));
    end;
    g_object_unref(G_OBJECT(TreeModel));

    // we added +1 because Ord(vsIcon) returns 0, so it's nil ptr
    g_object_set_data(PGObject(MainView),'lcllistviewstyle', {%H-}gpointer(PtrInt(Ord(TLVHack(AWinControl).ViewStyle) + 1)));

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
    g_object_set_data(Pointer(MainView), 'widgetinfo', WidgetInfo);
    gtk_widget_show_all(PGtkWidget(MainView));
    if not AWinControl.HandleObjectShouldBeVisible and not (csDesigning in AWinControl.ComponentState) then
      gtk_widget_hide(PGtkWidget(ScrollWidget));
    SetListCallbacks(PGtkWidget(ScrollWidget), Widgets, Widgets^.WidgetInfo);
  end;
end;

class procedure TGtk2WSCustomListView.DestroyHandle(const AWinControl: TWinControl);
var
  Widgets: PTVWidgets;
  i: Integer;
begin
  GetCommonTreeViewWidgets({%H-}PGtkWidget(AWinControl.Handle), Widgets);
  // on widget destroy we have no ItemDeleted notification and we must destroy ItemCache ourself
  // if things will change please remove this destroy
  if Widgets^.ItemCache <> nil then
    Widgets^.ItemCache.Free;
  Widgets^.ItemCache := nil;
  if Widgets^.OldTreeSelection <> nil then
  begin
    g_list_free(Widgets^.OldTreeSelection);
    Widgets^.OldTreeSelection := nil;
  end;
  if Widgets^.Images <> nil then
  begin
    for i := 0 to Widgets^.Images.Count-1 do
      if Widgets^.Images.Items[i] <> nil then
        gdk_pixbuf_unref(PGDKPixBuf(Widgets^.Images.Items[i]));
    FreeAndNil(Widgets^.Images);
  end;
  TWSWinControlClass(ClassParent).DestroyHandle(AWinControl);
end;

class procedure TGtk2WSCustomListView.BeginUpdate(const ALV: TCustomListView);
begin
  if not WSCheckHandleAllocated(ALV, 'BeginUpdate') then
    exit;
  g_object_set_data({%H-}PGObject(ALV.Handle),'lcl_gtkwidget_in_update', ALV);
end;

class procedure TGtk2WSCustomListView.EndUpdate(const ALV: TCustomListView);
begin
  if not WSCheckHandleAllocated(ALV, 'EndUpdate') then
    exit;
  g_object_set_data({%H-}PGObject(ALV.Handle),'lcl_gtkwidget_in_update', nil);
end;

class function TGtk2WSCustomListView.GetBoundingRect(const ALV: TCustomListView): TRect;
begin
  Result:=Rect(0,0,0,0);

  if not WSCheckHandleAllocated(ALV, 'GetBoundingRect')
  then Exit;

  //DebugLn('TODO: TGtk2WSCustomListView.GetBoundingRect');
end;

class function TGtk2WSCustomListView.GetDropTarget(const ALV: TCustomListView): Integer;
var
  Widgets: PTVWidgets;
begin
  // TODO: implement
  Result := -1;

  if not WSCheckHandleAllocated(ALV, 'GetDropTarget')
  then Exit;

  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);
end;

class function TGtk2WSCustomListView.GetFocused(const ALV: TCustomListView): Integer;
var
  Widgets: PTVWidgets;
  Path: PGtkTreePath;
  Column: PGtkTreeViewColumn;
  Cell: PGtkCellRenderer;
begin
  Result := -1;
  
  if not WSCheckHandleAllocated(ALV, 'GetFocused')
  then Exit;

  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);

  with Widgets^ do
  begin
    if GTK_IS_TREE_VIEW(MainView) then begin
      Path:=nil;
      Column:=nil;
      gtk_tree_view_get_cursor(PGtkTreeView(MainView), Path, Column);
    end
    else begin
      if GTK_IS_ICON_VIEW(MainView) then begin
        Cell:=nil;
        gtk_icon_view_get_cursor(PGtkIconView(MainView), Path, Cell);
      end
      else
        Path := nil;
    end;
    if Path <> nil then
    begin
      Result := StrToInt(PChar(Path));
      gtk_tree_path_free(Path);
    end;
  end;
end;

class function TGtk2WSCustomListView.GetHoverTime(const ALV: TCustomListView): Integer;
var
  Widgets: PTVWidgets;
begin
  // TODO: implement
  Result := -1; // = default

  if not WSCheckHandleAllocated(ALV, 'GetHoverTime')
  then Exit;

  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);
  with Widgets^ do
  begin

  end;
end;

class function TGtk2WSCustomListView.GetItemAt(const ALV: TCustomListView; x, y: integer): Integer;
var
  Widgets: PTVWidgets;
  ItemPath: PGtkTreePath;
  Column: PGtkTreeViewColumn;
  cx, cy: gint;
begin
  Result := -1;
  if not WSCheckHandleAllocated(ALV, 'GetItemAt')
  then Exit;
  
  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);

  if GTK_IS_TREE_VIEW(Widgets^.MainView) then
  begin
    // gtk2 >= 2.19 changed treeview api
    if gtk_minor_version >= 19 then
    begin
      gdk_window_get_position(gtk_tree_view_get_bin_window(PGtkTreeView(Widgets^.MainView)), @cx, @cy);
      Dec(x, cx);
      Dec(y, cy);
    end else
    begin
      // convert X, Y to bin window coords
      x := x + Round(PGtkTreeView(Widgets^.MainView)^.priv^.hadjustment^.value);
      if GTK_TREE_VIEW_FLAG_SET(PGtkTreeView(Widgets^.MainView), GTK_TREE_VIEW_HEADERS_VISIBLE) then
      begin
        gdk_window_get_size(PGtkTreeView(Widgets^.MainView)^.priv^.header_window, @cx, @cy);
        y := y - cy;
      end;
    end;
    ItemPath:=nil;
    Column:=nil;
    if gtk_tree_view_get_path_at_pos(PGtkTreeView(Widgets^.MainView), x, y, ItemPath, Column, nil, nil) then
    begin
      if ItemPath <> nil then
      begin
        Result := gtk_tree_path_get_indices(ItemPath)^;
        gtk_tree_path_free(ItemPath);
      end;
    end;
  end
  else
  if GTK_IS_ICON_VIEW(Widgets^.MainView) then
  begin
    ItemPath := gtk_icon_view_get_path_at_pos(PGtkIconView(Widgets^.MainView),
      x + Widgets^.ScrollingData.HValue, y + Widgets^.ScrollingData.VValue);
    if ItemPath <> nil then
    begin
      Result := gtk_tree_path_get_indices(ItemPath)^;
      gtk_tree_path_free(ItemPath);
    end;
  end;
end;

class function TGtk2WSCustomListView.GetSelCount(const ALV: TCustomListView): Integer;
var
  Widgets: PTVWidgets;
  AList: PGList;
begin
  Result := 0;

  if not WSCheckHandleAllocated(ALV, 'GetSelCount')
  then Exit;

  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);
  with Widgets^ do
  begin
    if GTK_IS_TREE_VIEW(MainView) then
      AList := gtk_tree_selection_get_selected_rows(TreeSelection, nil)
    else
    if GTK_IS_ICON_VIEW(MainView) then
      AList := gtk_icon_view_get_selected_items(PGtkIconView(MainView))
    else
      Exit;
    if AList <> nil then
    begin
      Result := g_list_length(AList);
      g_list_free(AList);
    end;
  end;
end;

class function TGtk2WSCustomListView.GetSelection(const ALV: TCustomListView): Integer;
var
  Widgets: PTVWidgets;
  Iter: TGtkTreeIter;
  Path: PGtkTreePath;
  AList: PGList;
begin
  Result := -1;
  
  if not WSCheckHandleAllocated(ALV, 'GetSelection')
  then Exit;

  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);

  with Widgets^ do
  begin
    if GTK_IS_TREE_VIEW(MainView) then
      gtk_tree_selection_get_selected(TreeSelection, nil, @Iter)
    else
    if GTK_IS_ICON_VIEW(MainView) then
    begin
      AList := gtk_icon_view_get_selected_items(PGtkIconView(MainView));
      if AList <> nil then
      begin
        Path := g_list_first(AList)^.data;
        g_list_free(AList);
      end else
        Path := nil;
    end;
    Path := gtk_tree_model_get_path(TreeModel, @Iter);
    Result := StrToInt(PChar(Path));
    gtk_tree_path_free(Path);
  end;
end;

class function TGtk2WSCustomListView.GetTopItem(const ALV: TCustomListView): Integer;
var
  Res: Boolean;
  s, e: PGtkTreePath;
  Widgets: PTVWidgets;
  Num: Pgint;
begin
  Result := -1;
  if not WSCheckHandleAllocated(ALV, 'GetTopItem') then
    exit;
  Res := false;
  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);
  with Widgets^ do
  begin
    if GTK_IS_TREE_VIEW(MainView) then
      Res := gtk_tree_view_get_visible_range(PGtkTreeView(Widgets^.MainView), s, e)
    else
      if GTK_IS_ICON_VIEW(MainView) then
        Res := gtk_icon_view_get_visible_range(PGtkTreeView(Widgets^.MainView), s, e)
      else
        Exit;
  end;
  if Res then
  begin
    Num := gtk_tree_path_get_indices(s);
    if Num <> nil then Result := Num^;
    gtk_tree_path_free(s);
    gtk_tree_path_free(e);
  end;
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
  
  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);
  
  // not really needed to retrieve the adjustments, since the TVWidgets has this info also based on events
  Result := Point(Widgets^.ScrollingData.HValue, Widgets^.ScrollingData.VValue);
end;

class function TGtk2WSCustomListView.GetVisibleRowCount(const ALV: TCustomListView): Integer;
var
  Res: Boolean;
  s, e: PGtkTreePath;
  Widgets: PTVWidgets;
  Num1,Num2: Pgint;
begin
  Result := -1;

  if not WSCheckHandleAllocated(ALV, 'GetVisibleRowCount') then
    exit;

   Result := 0;
   Res := false;
   GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);
   with Widgets^ do
   begin
     if GTK_IS_TREE_VIEW(MainView) then
       Res := gtk_tree_view_get_visible_range(PGtkTreeView(Widgets^.MainView), s, e)
     else
       if GTK_IS_ICON_VIEW(MainView) then
         Res := gtk_icon_view_get_visible_range(PGtkTreeView(Widgets^.MainView), s, e)
       else
         Exit;
   end;
   if Res then
   begin
     Num1 := gtk_tree_path_get_indices(s);
     Num2 := gtk_tree_path_get_indices(e);
     if (Num1 <> nil) and (Num2 <> nil) then Result := Num2^-Num1^+1;
     gtk_tree_path_free(s);
     gtk_tree_path_free(e);
   end;
end;

class procedure TGtk2WSCustomListView.SelectAll(const ALV: TCustomListView;
  const AIsSet: Boolean);
var
  Widgets: PTVWidgets;
begin
  if not WSCheckHandleAllocated(ALV, 'SelectAll') then
    exit;
  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);
  Include(Widgets^.WidgetInfo^.Flags, wwiInvalidEvent);
  try
    with Widgets^ do
    begin
      if GTK_IS_TREE_VIEW(MainView) then
      begin
        if AIsSet then
          gtk_tree_selection_select_all(gtk_tree_view_get_selection(PGtkTreeView(MainView)))
        else
          gtk_tree_selection_unselect_all(gtk_tree_view_get_selection(PGtkTreeView(MainView)));
      end else
      begin
        if GTK_IS_ICON_VIEW(MainView) then
        begin
          if AIsSet then
            gtk_icon_view_select_all(PGtkIconView(MainView))
          else
            gtk_icon_view_unselect_all(PGtkIconView(MainView));
        end;
      end;
    end;
  finally
    Exclude(Widgets^.WidgetInfo^.Flags, wwiInvalidEvent);
  end;
end;

class procedure TGtk2WSCustomListView.SetAllocBy(const ALV: TCustomListView;
  const AValue: Integer);
begin
  if not WSCheckHandleAllocated(ALV, 'SetAllocBy')
  then Exit;
end;

class procedure TGtk2WSCustomListView.SetColor(const AWinControl: TWinControl);
var
  AWidget: PGTKWidget;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetColor') then
    Exit;
  AWidget := {%H-}PGtkWidget(AWinControl.Handle);
  AWidget := GetWidgetInfo(AWidget, True)^.CoreWidget;
  Gtk2WidgetSet.SetWidgetColor(AWidget,
    AWinControl.Font.Color,
    AWinControl.Color,
    [GTK_STATE_NORMAL, GTK_STATE_ACTIVE, GTK_STATE_PRELIGHT, GTK_STYLE_BASE]);
end;

class procedure TGtk2WSCustomListView.SetDefaultItemHeight(
  const ALV: TCustomListView; const AValue: Integer);
begin
  if not WSCheckHandleAllocated(ALV, 'SetDefaultItemHeight')
  then Exit;
end;

class procedure TGtk2WSCustomListView.SetFont(const AWinControl: TWinControl;
  const AFont: TFont);
var
  Widget: PGtkWidget;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetFont') then
    Exit;
  Widget := {%H-}PGtkWidget(AWinControl.Handle);
  Widget := GetWidgetInfo(Widget, True)^.CoreWidget;
  Gtk2WidgetSet.SetWidgetFont(Widget, AFont);
  Gtk2WidgetSet.SetWidgetColor(Widget, AFont.Color, clNone,
                              [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,
                               GTK_STATE_PRELIGHT,GTK_STATE_SELECTED,
                               GTK_STYLE_TEXT]);
end;

class procedure TGtk2WSCustomListView.SetHotTrackStyles(const ALV: TCustomListView;
  const AValue: TListHotTrackStyles);
begin
  if not WSCheckHandleAllocated(ALV, 'SetHotTrackStyles')
  then Exit;
end;

class procedure TGtk2WSCustomListView.SetImageList(const ALV: TCustomListView;
  const AList: TListViewImageList; const AValue: TCustomImageListResolution);
var
  Widgets: PTVWidgets;
  BitImage: TBitmap;
  pixbuf: PGDKPixBuf;
  i: Integer;
  APGList: PGList;
  pixrenderer: PGtkCellRenderer;
begin
  if not WSCheckHandleAllocated(ALV, 'SetImageList')
  then Exit;
  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);
  gtk_widget_queue_draw(Widgets^.MainView);
  if ((AList = lvilLarge) and (TLVHack(ALV).ViewStyle = vsIcon)) or
     ((AList = lvilSmall) and (TLVHack(ALV).ViewStyle <> vsIcon)) then
  begin
    if Widgets^.Images <> nil then
    begin
      for i := 0 to Widgets^.Images.Count-1 do
        gdk_pixbuf_unref(PGdkPixBuf(Widgets^.Images.Items[i]));
      Widgets^.Images.Clear;
    end;
    if AValue = nil then
      Exit;
    if Widgets^.Images = nil then
      Widgets^.Images := TList.Create;

    if (AValue.Count = 0) and GTK_IS_TREE_VIEW(Widgets^.MainView) and (TLVHack(ALV).Columns.Count > 0) and
      not TLVHack(ALV).OwnerDraw then
    begin
      APGList := gtk_tree_view_column_get_cell_renderers(gtk_tree_view_get_column(PGtkTreeView(Widgets^.MainView), 0));
      pixrenderer := PGtkCellRenderer(g_list_last(APGList)^.prev^.data);
      gtk_cell_renderer_set_fixed_size(pixrenderer, AValue.Width + 2, AValue.Height + 2);
      g_list_free(APGList);
      gtk_tree_view_column_queue_resize(gtk_tree_view_get_column(PGtkTreeView(Widgets^.MainView), 0));
    end;

    for i := 0 to AValue.Count-1 do
    begin
      pixbuf := nil;
      BitImage := TBitmap.Create;
      try
        AValue.GetBitmap(i, BitImage);
        Gtk2_PixBufFromBitmap(BitImage, pixbuf);
        if GTK_IS_TREE_VIEW(Widgets^.MainView) and (TLVHack(ALV).Columns.Count > 0) and
          not TLVHack(ALV).OwnerDraw then
        begin
          APGList := gtk_tree_view_column_get_cell_renderers(gtk_tree_view_get_column(PGtkTreeView(Widgets^.MainView), 0));
          pixrenderer := PGtkCellRenderer(g_list_last(APGList)^.prev^.data);
          gtk_cell_renderer_set_fixed_size(pixrenderer, AValue.Width + 2, AValue.Height + 2);
          g_list_free(APGList);
          gtk_tree_view_column_queue_resize(gtk_tree_view_get_column(PGtkTreeView(Widgets^.MainView), 0));
        end;
        Widgets^.Images.Add(pixbuf);
      finally
        BitImage.Free;
      end;
    end;
  end;
end;

class procedure TGtk2WSCustomListView.SetItemsCount(const ALV: TCustomListView;
  const Avalue: Integer);
var
  Widgets: PTVWidgets;
  {$IFDEF USEORIGTREEMODEL}
  Iter: TGtkTreeIter;
  Index: Integer;
  {$ENDIF}
begin
  if not WSCheckHandleAllocated(ALV, 'SetItemsCount')
  then Exit;

  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);

  with Widgets^ do
  begin
    {$IFNDEF USEORIGTREEMODEL}
    g_object_ref(TreeModel);
    gtk_tree_view_set_model(PGtkTreeView(MainView), nil);
    gtk_tree_view_set_model(PGtkTreeView(MainView), TreeModel);
    g_object_unref(TreeModel);
    {$ELSE}
    gtk_list_store_clear(PGtkListStore(TreeModel));
    for Index := 0 to AValue - 1 do
      gtk_list_store_insert_with_values(PGtkListStore(TreeModel), @Iter, Index, 0, nil, -1);
    {$ENDIF}
  end;
end;

class procedure TGtk2WSCustomListView.SetProperty(const ALV: TCustomListView;
  const AProp: TListViewProperty; const AIsSet: Boolean);
var
  Widgets: PTVWidgets;
begin
  if not WSCheckHandleAllocated(ALV, 'SetProperty')
  then Exit;

  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);

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

  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);

  for Prop := Low(Prop) to High(Prop) do
    SetPropertyInternal(ALV, Widgets, Prop, Prop in AProps);
end;

class procedure TGtk2WSCustomListView.SetScrollBars(const ALV: TCustomListView;
  const AValue: TScrollStyle);
var
  SS:TPoint;
  ScrollWidget: PGtkScrolledWindow;
begin
  if not WSCheckHandleAllocated(ALV, 'SetScrollBars') then
    exit;

  ScrollWidget := {%H-}PGtkScrolledWindow(ALV.Handle);
  SS := Gtk2TranslateScrollStyle(AValue);
  gtk_scrolled_window_set_policy(ScrollWidget ,SS.X, SS.Y);
end;

class procedure TGtk2WSCustomListView.SetSort(const ALV: TCustomListView;
  const AType: TSortType; const AColumn: Integer;
  const ASortDirection: TSortDirection);
var
  Widgets: PTVWidgets;
begin
  if not WSCheckHandleAllocated(ALV, 'SetSort')
  then Exit;
  // gtk2 needs only update
  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets);
  if GetViewModel(Widgets^.MainView) = nil then
    exit;

  if not gtk_widget_realized(Widgets^.MainView) then
    exit;

  if GTK_IS_TREE_VIEW(Widgets^.MainView) then
    gtk_widget_queue_draw(Widgets^.MainView);
end;

class procedure TGtk2WSCustomListView.SetViewOrigin(const ALV: TCustomListView;
  const AValue: TPoint);
var
  Widgets: PTVWidgets;
begin
  if not WSCheckHandleAllocated(ALV, 'SetViewOrigin')
  then Exit;

  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets{%H-});
  //DebugLn(['TGtk2WSCustomListView.SetViewOrigin ',GetWidgetDebugReport(Widgets^.MainView)]);
  if not GTK_WIDGET_REALIZED(Widgets^.MainView) then exit;
  if GTK_IS_TREE_VIEW(Widgets^.MainView) then
    gtk_tree_view_scroll_to_point(PGtkTreeView(Widgets^.MainView), AValue.X, AValue.Y)
  else
  if GTK_IS_ICON_VIEW(Widgets^.MainView) then
  begin
    // TODO: iconview
  end;
end;

class procedure TGtk2WSCustomListView.SetViewStyle(const ALV: TCustomListView;
  const AValue: TViewStyle);

var
  APtrIntData: PtrInt;

  procedure ShowColumns(const Widgets: PTVWidgets; const Show: Boolean);
  var
    List: PGList;
    GtkColumn: PGtkTreeViewColumn;
    i: Integer;
  begin
    if not GTK_IS_TREE_VIEW(Widgets^.MainView) then
      Exit;
    List := gtk_tree_view_get_columns(PGtkTreeView(Widgets^.MainView));
    for i := 0 to g_list_length(List) - 1 do
    begin
      GtkColumn := g_list_nth_data(List, i);
      if GtkColumn = nil then
        Continue;

      if not Show or
        (Show and ({%H-}PtrUInt(g_object_get_data(G_OBJECT(GtkColumn),
                                             PChar('Visible'))) <> 0)) then
        gtk_tree_view_column_set_visible(GtkColumn, Show);
    end;
    g_list_free(List)
  end;

var
  Widgets: PTVWidgets;
begin
  if not WSCheckHandleAllocated(ALV, 'SetViewStyle')
  then Exit;

  GetCommonTreeViewWidgets({%H-}PGtkWidget(ALV.Handle), Widgets{%H-});
  if g_object_get_data(PGObject(Widgets^.MainView),'lcllistviewstyle') <> nil then
    APtrIntData := {%H-}PtrInt(g_object_get_data(PGObject(Widgets^.MainView),'lcllistviewstyle'))
  else
    APtrIntData := -1;
  if (APtrIntData <> -1) and (APtrIntData - 1 <> Ord(AValue)) then
  begin
    // we have to free the GtkTreeView and Create GtkIconView etc depending on the new style
    //RecreateMainView(ALV);
    // we actually need to recreate our ListView since not only the widget changes but also columns
    RecreateWnd(ALV);
    Exit;
  end;

  ShowColumns(Widgets, AValue = vsReport);
  with Widgets^ do
  begin
    case AValue of
      vsIcon,
      vsSmallIcon:
        begin
          SetNeedDefaultColumn(ALV, True);
        end;
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
end;
