{%MainUnit gtkwscomctrls.pp}
{ $Id$

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


{ TGtkWSCustomListView }

type
  TLVHack = class(TCustomListView)
  end;
  
  PCustomListViewData = ^TCustomListViewData;
  TCustomListViewData = record
    ScrollingData: TBaseScrollingWinControlData;
    ViewStyle: TViewStyle;
  end;
  
////////////////////////////////////////////////////////////////////////////////
// Event code
////////////////////////////////////////////////////////////////////////////////


  
//----------------------
//HDN_ENDTRACK
//HDN_TRACK
function GtkWSCustomListView_AbortColumnResize(AList: PGTKCList; AInfo: PWidgetInfo): GBoolean; cdecl;
begin
  //TODO: implement
  Result := False;
end;

//----------------------
//HDN_ENDTRACK
//HDN_TRACK
//HDN_ITEMCHANGED
//HDN_ITEMCHANGING
function GtkWSCustomListView_ResizeColumn(AList: PGTKCList; AColumn, AWidth: Integer; AInfo: PWidgetInfo): GBoolean; cdecl;
begin
  //TODO: implement
  Result := False;
end;

//----------------------
//HDN_ITEMCLICK
//LVN_COLUMNCLICK
function GtkWSCustomListView_ClickColumn(AList: PGTKCList; AColumn: Integer; AInfo: PWidgetInfo): GBoolean; cdecl;
var
  msg: TLMNotify;
  NM: TNMListView;
  ALV: TListView;
begin
  // this can happen when no columns are added which crashes the program
  ALV := TListView(AInfo^.LCLObject);
  if AColumn > ALV.Columns.Count-1 then
    Exit;
    
  msg.Msg := CN_NOTIFY;

  FillChar(NM, SizeOf(NM), 0);
  NM.hdr.hwndfrom := PtrUInt(AList);
  NM.hdr.code := LVN_COLUMNCLICK;
  NM.iItem := -1;
  NM.iSubItem := AColumn;
  msg.NMHdr := @NM.hdr;
  Result := DeliverMessage(AInfo^.LCLObject, msg) = 0;
end;

//----------------------
//LVN_DELETEITEM
//LVN_INSERTITEM
function GtkWSCustomListView_RowMove(AList: PGTKCList; AnOldIdx, ANewIdx: Integer; AInfo: PWidgetInfo): GBoolean; cdecl;
var
  msg: TLMNotify;
  NM: TNMListView;
  r: Boolean;
begin
  // Simulate move by remove and insert
  msg.Msg := CN_NOTIFY;

  FillChar(NM, SizeOf(NM), 0);
  NM.hdr.hwndfrom := PtrUInt(AList);
  NM.hdr.code := LVN_DELETEITEM;
  NM.iItem := AnOldIdx;
  msg.NMHdr := @NM.hdr;
  r := DeliverMessage(AInfo^.LCLObject, msg) = 0;

  NM.hdr.code := LVN_INSERTITEM;
  NM.iItem := ANewIdx;
  Result := (DeliverMessage(AInfo^.LCLObject, msg) = 0) and r;
end;

//----------------------
//LVN_ITEMCHANGED
//LVN_ITEMCHANGING
function GtkWSCustomListView_SelectRow(AList: PGTKCList; ARow, AColumn: Integer; AEvent: PGDKEventButton; AInfo: PWidgetInfo): GBoolean; cdecl;
var
  msg: TLMNotify;
  NM: TNMListView;
begin
  msg.Msg := CN_NOTIFY;

  FillChar(NM, SizeOf(NM), 0);
  NM.hdr.hwndfrom := PtrUInt(AList);
  NM.hdr.code := LVN_ITEMCHANGED;
  NM.iItem := ARow;
  NM.iSubItem := AColumn;
  NM.uNewState := LVIS_SELECTED;
  NM.uChanged := LVIF_STATE;
  msg.NMHdr := @NM.hdr;
  Result := DeliverMessage(AInfo^.LCLObject, msg) = 0;
end;

function GtkWSCustomListView_UnSelectRow(AList: PGTKCList; ARow, AColumn: Integer; AEvent: PGDKEventButton; AInfo: PWidgetInfo): GBoolean; cdecl;
var
  msg: TLMNotify;
  NM: TNMListView;
begin
  msg.Msg := CN_NOTIFY;

  FillChar(NM, SizeOf(NM), 0);
  NM.hdr.hwndfrom := PtrUInt(AList);
  NM.hdr.code := LVN_ITEMCHANGED;
  NM.iItem := ARow;
  NM.iSubItem := AColumn;
  NM.uOldState := LVIS_SELECTED;
  NM.uChanged := LVIF_STATE;
  msg.NMHdr := @NM.hdr;
  Result := DeliverMessage(AInfo^.LCLObject, msg) = 0;
end;

function GtkWSCustomListView_ToggleFocusRow(AList: PGTKCList; AInfo: PWidgetInfo): GBoolean; cdecl;
var
  msg: TLMNotify;
  NM: TNMListView;
begin
  // the defocus of the oldrow isn't send

  msg.Msg := CN_NOTIFY;

  FillChar(NM, SizeOf(NM), 0);
  NM.hdr.hwndfrom := PtrUInt(AList);
  NM.hdr.code := LVN_ITEMCHANGED;
  NM.iItem := AList^.focus_row;
  NM.iSubItem := 0;
  NM.uNewState := LVIS_FOCUSED;
  NM.uChanged := LVIF_STATE;
  msg.NMHdr := @NM.hdr;
  Result := DeliverMessage(AInfo^.LCLObject, msg) = 0;
end;

function GtkWSCustomListView_SelectAll(AList: PGTKCList; AInfo: PWidgetInfo): GBoolean; cdecl;
var
  msg: TLMNotify;
  NM: TNMListView;
  ListView: TListView;
  n: Integer;
begin
  msg.Msg := CN_NOTIFY;

  ListView := AInfo^.LCLObject as TListView;

  FillChar(NM, SizeOf(NM), 0);
  NM.hdr.hwndfrom := PtrUInt(AList);
  NM.hdr.code := LVN_ITEMCHANGED;
  for n := 0 to Listview.Items.Count - 1 do
  begin
    if ListView.Items[n].Selected
    then Continue;
    NM.iItem := n;
    NM.iSubItem := -1;
    NM.uNewState := LVIS_SELECTED;
    NM.uChanged := LVIF_STATE;
    msg.NMHdr := @NM.hdr;
    Result := DeliverMessage(AInfo^.LCLObject, msg) = 0;
  end;
end;

function GtkWSCustomListView_UnSelectAll(AList: PGTKCList; AInfo: PWidgetInfo): GBoolean; cdecl;
var
  msg: TLMNotify;
  NM: TNMListView;
  ListView: TListView;
  n: Integer;
begin
  msg.Msg := CN_NOTIFY;

  ListView := AInfo^.LCLObject as TListView;

  FillChar(NM, SizeOf(NM), 0);
  NM.hdr.hwndfrom := PtrUInt(AList);
  NM.hdr.code := LVN_ITEMCHANGED;
  for n := 0 to Listview.Items.Count - 1 do
  begin
    if not ListView.Items[n].Selected
    then Continue;
    NM.iItem := n;
    NM.iSubItem := -1;
    NM.uOldState := LVIS_SELECTED;
    NM.uChanged := LVIF_STATE;
    msg.NMHdr := @NM.hdr;
    Result := DeliverMessage(AInfo^.LCLObject, msg) = 0;
  end;
end;

function GtkWSCustomListView_EndSelection(AList: PGTKCList; AInfo: PWidgetInfo): GBoolean; cdecl;
begin
  Result:=true;
end;

////////////////////////////////////////////////////////////////////////////////
// Column code
////////////////////////////////////////////////////////////////////////////////

class procedure TGtkWSCustomListView.ColumnDelete(const ALV: TCustomListView; const AIndex: Integer);
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnDelete') then Exit;

  // allow only column modifications when in report mode
  if TLVHack(ALV).ViewStyle <> vsReport then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);
  if CListWidget^.columns = TLVHack(ALV).Columns.Count then Exit; // possible delayed update
  if AIndex >= CListWidget^.columns then Exit; // ???
  
  RecreateWnd(ALV);
end;

class function TGtkWSCustomListView.ColumnGetWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn): Integer;
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
  CListColumn: PGtkCListColumn;
begin
  Result := -1;
  if not WSCheckHandleAllocated(ALV, 'ColumnGetSize') then Exit;

  // allow only column modifications when in report mode
  if TLVHack(ALV).ViewStyle <> vsReport then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);

  // there is no get width function, so we need some internal hacking
  if AIndex >= CListWidget^.columns then Exit;
  CListColumn := CListWidget^.Column;
  Inc(CListColumn, AIndex);
  Result := CListColumn^.width;
end;

class procedure TGtkWSCustomListView.ColumnInsert(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn);
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnInsert') then Exit;

  // allow only column modifications when in report mode
  if TLVHack(ALV).ViewStyle <> vsReport then Exit;
  
  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);
  if CListWidget^.columns = TLVHack(ALV).Columns.Count then Exit; // possible delayed update

  RecreateWnd(ALV);
end;

class procedure TGtkWSCustomListView.ColumnMove(const ALV: TCustomListView; const AOldIndex, ANewIndex: Integer; const AColumn: TListColumn);
  procedure CopyColumn(const AList: PGtkCList; const AIndex: Integer; const ASrc: PGtkCListColumn);
  begin                                      
    gtk_clist_set_column_title(AList, AIndex, ASrc^.title);
    gtk_clist_set_column_min_width(AList, AIndex, ASrc^.min_width);
    gtk_clist_set_column_max_width(AList, AIndex, ASrc^.max_width);
    gtk_clist_set_column_width(AList, AIndex, ASrc^.width);
    gtk_clist_set_column_justification(AList, AIndex, ASrc^.justification);
    gtk_clist_set_column_visibility(AList, AIndex, (ASrc^.flag0 and bm_TGtkCListColumn_visible) <> 0);
    gtk_clist_set_column_resizeable(AList, AIndex, (ASrc^.flag0 and bm_TGtkCListColumn_resizeable) <> 0);
    gtk_clist_set_column_auto_resize(AList, AIndex, (ASrc^.flag0 and bm_TGtkCListColumn_auto_resize) <> 0);
    if (ASrc^.flag0 and bm_TGtkCListColumn_button_passive) <> 0
    then gtk_clist_column_title_passive(AList, AIndex)
    else gtk_clist_column_title_active(AList, AIndex);
  end;
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
  CListColumn: PGtkCListColumn;
  OldCListColumn: TGtkCListColumn;
  Count: Integer;      
  
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnMove') then Exit;

  // allow only column modifications when in report mode
  if TLVHack(ALV).ViewStyle <> vsReport then Exit;

  if AOldIndex = ANewIndex then Exit;
  if AOldIndex < 0 then Exit;
  if ANewIndex < 0 then Exit;
  
  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);
  
  if AOldIndex >= CListWidget^.columns then Exit;
  if ANewIndex >= CListWidget^.columns then Exit;
  
  Count := AOldIndex - ANewIndex;
  
  // Fetch old column values
  CListColumn := CListWidget^.Column;
  Inc(CListColumn, AOldIndex);
  OldCListColumn := CListColumn^; 
  // Create copy of the title
  OldCListColumn.title := StrNew(OldCListColumn.title);

  while Count <> 0 do
  begin
    // move to next source
    if Count < 0
    then Inc(CListColumn)
    else Dec(CListColumn);

    CopyColumn(CListWidget, ANewIndex + Count, CListColumn);

    if Count < 0
    then Inc(Count)
    else Dec(Count);
  end;
  // finally copy original data to new column
  CopyColumn(CListWidget, ANewIndex, @OldCListColumn);
  // dispose copy of the title
  StrDispose(OldCListColumn.title);
end;

class procedure TGtkWSCustomListView.ColumnSetAlignment(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AAlignment: TAlignment);
const
  JUSTIFICATION: array[TAlignment] of TGtkJustification = (
    GTK_JUSTIFY_LEFT,
    GTK_JUSTIFY_RIGHT,
    GTK_JUSTIFY_CENTER
  );
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetAlignment') then Exit;

  // allow only column modifications when in report mode
  if TLVHack(ALV).ViewStyle <> vsReport then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);

  gtk_clist_set_column_justification(CListWidget, AIndex, JUSTIFICATION[AAlignment]);
end;

class procedure TGtkWSCustomListView.ColumnSetAutoSize(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AAutoSize: Boolean);
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetAutoSize') then Exit;

  // allow only column modifications when in report mode
  if TLVHack(ALV).ViewStyle <> vsReport then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);

  gtk_clist_set_column_auto_resize(CListWidget, AIndex, AAutoSize);
end;

class procedure TGtkWSCustomListView.ColumnSetCaption(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const ACaption: String);
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetCaption') then Exit;

  // allow only column modifications when in report mode
  if TLVHack(ALV).ViewStyle <> vsReport then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);

  gtk_clist_set_column_title(CListWidget, AIndex, PChar(ACaption));
end;

class procedure TGtkWSCustomListView.ColumnSetImage(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AImageIndex: Integer);
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetImage') then Exit;

  // allow only column modifications when in report mode
  if TLVHack(ALV).ViewStyle <> vsReport then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);

  //TODO
  if CListWidget=nil then exit;
end;

class procedure TGtkWSCustomListView.ColumnSetMaxWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AMaxWidth: Integer);
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetMaxWidth') then Exit;

  // allow only column modifications when in report mode
  if TLVHack(ALV).ViewStyle <> vsReport then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);


  // TODO: ? -1 -2
  //LVSCW_AUTOSIZE              = -1;
  //LVSCW_AUTOSIZE_USEHEADER    = -2;

  if AMaxWidth <= 0 // unlimited
  then gtk_clist_set_column_max_width(CListWidget, AIndex, -1)
  else gtk_clist_set_column_max_width(CListWidget, AIndex, AMaxWidth);
end;

class procedure TGtkWSCustomListView.ColumnSetMinWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AMinWidth: integer);
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetMinWidth') then Exit;

  // allow only column modifications when in report mode
  if TLVHack(ALV).ViewStyle <> vsReport then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);

  gtk_clist_set_column_min_width(CListWidget, AIndex, AMinWidth);
end;

class procedure TGtkWSCustomListView.ColumnSetWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AWidth: Integer);
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetWidth') then Exit;

  // allow only column modifications when in report mode
  if TLVHack(ALV).ViewStyle <> vsReport then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);

  gtk_clist_set_column_width(CListWidget, AIndex, AWidth);
end;

class procedure TGtkWSCustomListView.ColumnSetVisible(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AVisible: Boolean);
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetVisible') then Exit;

  // allow only column modifications when in report mode
  if TLVHack(ALV).ViewStyle <> vsReport then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);

  gtk_clist_set_column_visibility(CListWidget, AIndex, AVisible);
end;

////////////////////////////////////////////////////////////////////////////////
// Item code
////////////////////////////////////////////////////////////////////////////////

class procedure TGtkWSCustomListView.ItemChangeInternal(const ACListWidget: PGtkCList; const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem);
var
  ImageBitmap: TBitmap;
  GDIObject: PGDIObject;
  Pixmap: PGdkPixmap;
  Mask: PGdkBitmap;
  n, Count: integer;
begin
  if  (TLVHack(ALV).SmallImages <> nil)
  and (AItem.ImageIndex >= 0)
  and (AItem.ImageIndex < TLVHack(ALV).SmallImages.Count)
  then begin
    // set image & caption
    ImageBitmap := TBitmap.Create;
    Mask := nil;
    try
      TLVHack(ALV).SmallImages.GetBitmap(AItem.ImageIndex, ImageBitmap);
      GDIObject := PGDIObject(ImageBitmap.Handle);
      case GDIObject^.GDIBitmapType of
        gbBitmap:
          begin
            Pixmap := GDIObject^.GDIBitmapObject;
            Mask := nil;
          end;
        gbPixmap:
          begin
            Pixmap := GDIObject^.GDIPixmapObject.Image;
            Mask := CreateGdkMaskBitmap(ImageBitmap.Handle, ImageBitmap.MaskHandle);
          end;
        gbPixbuf:
          begin
            Pixmap := nil;
            Mask := nil;
            gdk_pixbuf_render_pixmap_and_mask(GDIObject^.GDIPixbufObject, Pixmap, Mask, $80);
          end;
      end;
      gtk_clist_set_pixtext(ACListWidget, AIndex, 0, PChar(AItem.Caption), 3, Pixmap, Mask);
    finally
      if Mask <> nil then
        gdk_bitmap_unref(Mask);
      if Pixmap <> GDIObject^.GDIPixmapObject.Image then
        gdk_pixmap_unref(Pixmap);
      ImageBitmap.Free;
    end;
  end
  else begin
    // set caption alone
    gtk_clist_set_text(ACListWidget, AIndex, 0, PChar(AItem.Caption));
  end;

  // set the other column texts
  Count := AItem.SubItems.Count + 1;
  if Count > ACListWidget^.Columns
  then Count := ACListWidget^.Columns;
  // set the existing subitems
  for n := 1 to Count - 1 do
    gtk_clist_set_text(ACListWidget, AIndex, n, PChar(AItem.SubItems[n - 1]));
  // fill remaining
  for n := Count to ACListWidget^.Columns - 1 do
    gtk_clist_set_text(ACListWidget, AIndex, n, #0);
end;

class procedure TGtkWSCustomListView.ItemDelete(const ALV: TCustomListView;
  const AIndex: Integer);
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemDelete')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);

  gtk_clist_remove(CListWidget, AIndex);
end;

class function TGtkWSCustomListView.ItemGetState(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const AState: TListItemState;
  out AIsSet: Boolean): Boolean;
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  Result := False;
  
  if not WSCheckHandleAllocated(ALV, 'ItemGetState')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);
  if (AIndex < 0) or (AIndex >= CListWidget^.rows) 
  then begin
    DebugLN('[TGtkWSCustomListView.ItemGetState] Invalid row index: %d', [Aindex]);
    Exit;
  end;   
  
  case AState of
    lisCut, 
    lisDropTarget: begin
      //TODO: do something with the rowcolor ?
    end;

    lisFocused: begin
      AIsSet := CListWidget^.focus_row = AIndex;
      Result := True;
    end;

    lisSelected: begin 
      AIsSet := (CListWidget^.selection <> nil)
            and (g_list_find(CListWidget^.selection, Pointer(PtrInt(Aindex))) <> nil);
      Result := True;
    end;
  end;
  
end;
  
class procedure TGtkWSCustomListView.ItemInsert(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem);
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
  Titles: PPGChar;
  idx, Count: Integer;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemInsert')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);

  Count := CListWidget^.columns;

  if Count = 0
  then begin
    DebugLn('WARNING: TGtkWSCustomListView.ItemInsert  CListWidget^.columns = 0');
    Exit;
  end;

  GetMem(Titles, SizeOf(PGChar) * Count);
  FillChar(Titles^, SizeOf(PGChar) * Count, 0);
  Titles[0] := #0;

  idx := AIndex;
  if idx = -1
  then idx := gtk_clist_append(CListWidget, Titles)
  else gtk_clist_insert(CListWidget, idx, Titles);
  FreeMem(Titles);

  ItemChangeInternal(CListWidget, ALV, idx, AItem);
end;

class procedure TGtkWSCustomListView.ItemSetImage(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex, AImageIndex: Integer);
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
  ImageBitmap: TBitmap;
  Pixmap: PGdkPixmap;
  Mask: PGdkBitmap;
  Spacing: guint8;
  Text: PChar;
  Dummy1, Dummy2: PGdkBitmap;
  CellType: TGtkCellType;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemSetImage')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);

  Pixmap := nil;
  Mask := nil;

  if  (TLVHack(ALV).SmallImages <> nil)
  and (AImageIndex >= 0)
  and (AImageIndex < TLVHack(ALV).SmallImages.Count)
  then begin
    // set image & caption
    ImageBitmap := TBitmap.Create;
    try
      TLVHack(ALV).SmallImages.GetBitmap(AImageIndex, ImageBitmap);
      case PGDIObject(ImageBitmap.Handle)^.GDIBitmapType of
        gbBitmap:
          begin
            Pixmap := PGDIObject(ImageBitmap.Handle)^.GDIBitmapObject;
            gdk_pixmap_ref(Pixmap);
            Mask := nil;
          end;
        gbPixmap:
          begin
            Pixmap := PGDIObject(ImageBitmap.Handle)^.GDIPixmapObject.Image;
            Mask := CreateGdkMaskBitmap(ImageBitmap.Handle, ImageBitmap.MaskHandle);
            gdk_pixmap_ref(Pixmap);
          end;
        gbPixbuf:
          begin
            Pixmap := nil;
            Mask := nil;
            gdk_pixbuf_render_pixmap_and_mask(PGDIObject(ImageBitmap.Handle)^.GDIPixbufObject, pixmap, mask, $80);
          end;
      end;
    finally
      ImageBitmap.Free;
    end;
  end;

  CellType := gtk_clist_get_cell_type(CListWidget, AIndex, ASubIndex);
  // Sigh. 
  // gtk returns -1 for an invalid cell (which is not part of the enum)
  // so to handle it, we need a case based on integer
  case Ord(CellType) of
    Ord(GTK_CELL_TEXT),
    Ord(GTK_CELL_PIXTEXT),
    Ord(GTK_CELL_EMPTY),
    Ord(GTK_CELL_PIXMAP): begin
      if pixmap <> nil
      then begin
        case CellType of
          GTK_CELL_TEXT: begin
            // convert the cell
            Text := nil;
            gtk_clist_get_text(CListWidget, AIndex, ASubIndex, @Text);
            gtk_clist_set_pixtext(CListWidget, AIndex, ASubIndex, Text, DEFAULT_IMAGE_SPACING, Pixmap, Mask);
          end;
          GTK_CELL_PIXTEXT: begin
            if gtk_clist_get_pixtext(CListWidget, AIndex, ASubIndex, @Text, @Spacing, @Dummy2, @Dummy1) <> 0
            then gtk_clist_set_pixtext(CListWidget, AIndex, ASubIndex, Text, Spacing, Pixmap, Mask)
            else gtk_clist_set_pixmap(CListWidget, AIndex, ASubIndex, Pixmap, Mask);
          end;
          GTK_CELL_EMPTY,
          GTK_CELL_PIXMAP: begin
            gtk_clist_set_pixtext(CListWidget, AIndex, ASubIndex, '', DEFAULT_IMAGE_SPACING, Pixmap, Mask);
          end;
        end;
      end
      else begin
        case CellType of
          GTK_CELL_EMPTY,
          GTK_CELL_TEXT:; // nothing to do
          GTK_CELL_PIXTEXT: begin
            Text := nil;
            if gtk_clist_get_pixtext(CListWidget, AIndex, ASubIndex, @Text, @Spacing, @Dummy2, @Dummy1) <> 0
            then gtk_clist_set_text(CListWidget, AIndex, ASubIndex, Text)
            else gtk_clist_set_text(CListWidget, AIndex, ASubIndex, '');
          end;
          GTK_CELL_PIXMAP: begin
            gtk_clist_set_text(CListWidget, AIndex, ASubIndex, '');
          end;
        end;
      end;
    end;
    Ord(GTK_CELL_WIDGET): DebugLN('[TGtkWSCustomListView.ItemSetImage] Setting text of widget cell');
    -1: DebugLN('[TGtkWSCustomListView.ItemSetText] Cell (%d,%d) not created', [AIndex, ASubIndex]);
  else
    DebugLN('[TGtkWSCustomListView.ItemSetImage] Unknown celltype %d', [Ord(CellType)]);
  end;
  if Pixmap <> nil then
    gdk_pixmap_unref(Pixmap);
  if Mask <> nil then
    gdk_bitmap_unref(Mask);
end;

class procedure TGtkWSCustomListView.ItemSetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; const AIsSet: Boolean);
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemSetState')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);
  if (AIndex < 0) or (AIndex >= CListWidget^.rows) 
  then begin
    DebugLN('[TGtkWSCustomListView.ItemSetState] Invalid row index: %d', [Aindex]);
    Exit;
  end;   
  
  case AState of
    lisCut, 
    lisDropTarget: begin
      //TODO: do something with the rowcolor ?
    end;

    lisFocused: begin
      if AIsSet = (CListWidget^.focus_row = AIndex) then Exit;
      // reset old focus
      if (CListWidget^.focus_row <> -1)
      and (gtk_widget_has_focus(PGtkWidget(CListWidget)))
      then gtk_widget_draw_focus(PGtkWidget(CListWidget));
      
      if AIsSet
      then begin
        CListWidget^.focus_row := AIndex;
        if gtk_widget_has_focus(PGtkWidget(CListWidget))
        then gtk_widget_draw_focus(PGtkWidget(CListWidget));
      end
      else CListWidget^.focus_row := -1; 
    end;

    lisSelected: begin
      if AIsSet
      then begin
        if (CListWidget^.selection_mode = GTK_SELECTION_SINGLE)
        or (CListWidget^.selection_mode = GTK_SELECTION_BROWSE)
        then begin
          // check if the row is are already selected
          // since we are in singleselect, the first item is checked
          if (CListWidget^.selection <> nil)
          and (PtrInt(PtrUInt(CListWidget^.selection^.Data)) = AIndex)
          then Exit;
          gtk_clist_unselect_all(CListWidget);
        end;
        gtk_clist_select_row(CListWidget, AIndex, 0);
      end
      else gtk_clist_unselect_row(CListWidget, AIndex, 0);
    end;
  end;
end;

class procedure TGtkWSCustomListView.ItemSetText(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex: Integer; const AText: String);
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
  Pixmap: PGdkPixmap;
  Mask: PGdkBitmap;
  Spacing: guint8;
  Dummy: pgchar;
  CellType: TGtkCellType;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemSetText')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);

  CellType := gtk_clist_get_cell_type(CListWidget, AIndex, ASubIndex);
  // Sigh. 
  // gtk returns -1 for an invalid cell (which is not part of the enum)
  // so to handle it, we need a case based on integer
  case Ord(CellType) of
    Ord(GTK_CELL_EMPTY),
    Ord(GTK_CELL_TEXT): begin
      // simply set the text
      gtk_clist_set_text(CListWidget, AIndex, ASubIndex, PChar(AText));
    end;
    Ord(GTK_CELL_PIXTEXT): begin
      if gtk_clist_get_pixtext(CListWidget, AIndex, ASubIndex, @Dummy, @Spacing, @Pixmap, @Mask) <> 0
      then gtk_clist_set_pixtext(CListWidget, AIndex, ASubIndex, PChar(AText), Spacing, Pixmap, Mask)
      else gtk_clist_set_text(CListWidget, AIndex, ASubIndex, PChar(AText));
    end;
    Ord(GTK_CELL_PIXMAP): begin
      if gtk_clist_get_pixmap(CListWidget, AIndex, ASubIndex, @Pixmap, @Mask) <> 0
      then gtk_clist_set_pixtext(CListWidget, AIndex, ASubIndex, PChar(AText), DEFAULT_IMAGE_SPACING, Pixmap, Mask)
      else gtk_clist_set_text(CListWidget, AIndex, ASubIndex, PChar(AText));
    end;
    Ord(GTK_CELL_WIDGET): DebugLN('[TGtkWSCustomListView.ItemSetText] Setting text of widget cell');
    -1: DebugLN('[TGtkWSCustomListView.ItemSetText] Cell (%d,%d) not created', [AIndex, ASubIndex]);
  else
    DebugLN('[TGtkWSCustomListView.ItemSetText] Unknown celltype %d', [Ord(CellType)]);
  end;
end;

class procedure TGtkWSCustomListView.ItemShow(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const PartialOK: Boolean);
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
  RowTopY: Integer;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemShow')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);
  
  RowTopY := (CListWidget^.row_height * AIndex) + ((AIndex +1) *
                                     {CELL} 1 {SPACING} + CListWidget^.voffset);
                                     
                                                 // 0=NotVisible
                                                 // 1=PartiallyVisible
                                                 // 2=Fully Visible
                                                 //   |
  if gtk_clist_row_is_visible(CListWidget, AIndex) < (2 - Ord(PartialOK)) then begin
    if (RowTopY + CListWidget^.row_height > CListWidget^.clist_window_height) then begin
      gtk_clist_moveto (CListWidget, AIndex, -1, 1, 0);
        //                              |     |  |  |
        //                        The Row     |  |  |
        //                           The Column  |  |
        //                               Row Align  |
    end //                               Column Align
    else if (RowTopY < 0) then begin
      gtk_clist_moveto (CListWidget, AIndex, -1, 0, 0);
    end;//                                       |
  end;  //                                       |
        //                                       |
        //                      0 = your row will be at the top.
        //                      1 = it will be at the bottom.
end;

////////////////////////////////////////////////////////////////////////////////
// LV code
////////////////////////////////////////////////////////////////////////////////

class function TGtkWSCustomListView.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
var
  ListView: TLVHack; //TCustomListView
  WidgetInfo: PWidgetInfo;
  ScrollingData: PBaseScrollingWinControlData;
  ListViewData: PCustomListViewData;

  ScrollWidget: PGtkScrolledWindow;
  CListWidget: PGtkCList;
begin
  ListView := TLVHack(AWinControl as TCustomListView);

  Result := TGtkWSBaseScrollingWinControl.CreateHandle(AWinControl, AParams);
  if Result = 0 then Exit;
  ScrollWidget := PGtkScrolledWindow(Result);

  if ListView.ViewStyle = vsReport
  then begin
    // precreate colums since they cannot be added or removed
    CListWidget := PGtkCList(gtk_clist_new(Max(1, ListView.Columns.Count)));
    gtk_clist_column_titles_passive(CListWidget);
  end
  else begin
    CListWidget := PGtkCList(gtk_clist_new(1));
    gtk_clist_column_titles_hide(CListWidget);
    gtk_clist_set_column_auto_resize(CListWidget, 0, True);
  end;
  gtk_clist_set_shadow_type(CListWidget, GTK_SHADOW_IN);

  gtk_container_add(PGtkContainer(ScrollWidget), PGtkWidget(CListWidget));

  gtk_widget_unset_flags(ScrollWidget^.hscrollbar, GTK_CAN_FOCUS);
  gtk_widget_unset_flags(ScrollWidget^.vscrollbar, GTK_CAN_FOCUS);
  gtk_scrolled_window_set_policy(ScrollWidget, GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  // the next is not really needed since the container has only one child
  gtk_container_set_focus_vadjustment(PGtkContainer(CListWidget), gtk_scrolled_window_get_vadjustment(ScrollWidget));
  gtk_container_set_focus_hadjustment(PGtkContainer(CListWidget), gtk_scrolled_window_get_hadjustment(ScrollWidget));
  gtk_widget_show_all(PGtkWidget(CListWidget));

  // create widget info
  // already created in TGtkWSBaseScrollingWinControl
  // Replace the ScrollingInfo with our info
  WidgetInfo := GetWidgetInfo(ScrollWidget);
  WidgetInfo^.CoreWidget := PGtkWidget(CListWidget);
  ScrollingData := WidgetInfo^.UserData;
  New(ListViewData);
  ListViewData^.ScrollingData := ScrollingData^;
  ListViewData^.ViewStyle := ListView.ViewStyle;
  Dispose(ScrollingData);
  WidgetInfo^.UserData := ListViewData;
  //todo: obsolete
  // SetMainWidget(ScrollWidget, CListWidget);

  // set allocation
  // already created in TGtkWSBaseScrollingWinControl

  Set_RC_Name(AWinControl, PGtkWidget(ScrollWidget));
  SetCallbacks(PGtkWidget(ScrollWidget), PGtkWidget(CListWidget), WidgetInfo);
end;

class procedure TGtkWSCustomListView.SetCallbacks(const AScrollWidget, AListWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
begin
  TGtkWSBaseScrollingWinControl.SetCallbacks(AScrollWidget, AWidgetInfo);

  SignalConnect(AListWidget, 'click-column',        @GtkWSCustomListView_ClickColumn,       AWidgetInfo);
  SignalConnect(AListWidget, 'resize-column',       @GtkWSCustomListView_ResizeColumn,      AWidgetInfo);
  SignalConnect(AListWidget, 'abort-column-resize', @GtkWSCustomListView_AbortColumnResize, AWidgetInfo);
  SignalConnect(AListWidget, 'row-move',            @GtkWSCustomListView_RowMove,           AWidgetInfo);
  SignalConnect(AListWidget, 'select-row',          @GtkWSCustomListView_SelectRow,         AWidgetInfo);
  SignalConnect(AListWidget, 'unselect-row',        @GtkWSCustomListView_UnSelectRow,       AWidgetInfo);
  SignalConnect(AListWidget, 'toggle-focus-row',    @GtkWSCustomListView_ToggleFocusRow,    AWidgetInfo);
  SignalConnect(AListWidget, 'select-all',          @GtkWSCustomListView_SelectAll,         AWidgetInfo);
  SignalConnect(AListWidget, 'unselect-all',        @GtkWSCustomListView_UnSelectAll,       AWidgetInfo);
  SignalConnect(AListWidget, 'end-selection',       @GtkWSCustomListView_EndSelection,      AWidgetInfo);
end;

class procedure TGtkWSCustomListView.BeginUpdate(const ALV: TCustomListView);
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'BeginUpdate')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);
  
  gtk_clist_freeze(CListWidget);
end;

class procedure TGtkWSCustomListView.EndUpdate(const ALV: TCustomListView);
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'EndUpdate')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);
  
  gtk_clist_thaw(CListWidget);
end;

class function TGtkWSCustomListView.GetBoundingRect(const ALV: TCustomListView): TRect;
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  Result:=Rect(0,0,0,0);
  if not WSCheckHandleAllocated(ALV, 'GetBoundingRect')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);
  if CListWidget=nil then exit;
  
  
  if TLVHack(ALV).ViewStyle in [vsIcon, vsSmallIcon]
  then begin
    // TODO: implement
  end
  else begin
    Result := Rect(0,0,0,0);
  end;
end;

class function TGtkWSCustomListView.GetDropTarget(const ALV: TCustomListView): Integer;
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  Result:=0;
  if not WSCheckHandleAllocated(ALV, 'GetDropTarget')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);
  if CListWidget=nil then exit;
  
  // TODO: implement
  Result := -1;
end;

class function TGtkWSCustomListView.GetFocused(const ALV: TCustomListView): Integer;
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'GetFocused')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);
  
  Result := CListWidget^.focus_row;
end;

class function TGtkWSCustomListView.GetHoverTime(const ALV: TCustomListView): Integer;
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  Result := -1; // = default
  if not WSCheckHandleAllocated(ALV, 'GetHoverTime')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);
  if CListWidget=nil then exit;
  
  // TODO: implement
  Result := -1; // = default
end;

class function TGtkWSCustomListView.GetItemAt(const ALV: TCustomListView; x,
  y: integer): Integer;
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
  I, FirstRowY, LastRowY: Integer;
  ScrolledTop: Integer;
  RowHeight: Integer;
  
begin
  if not WSCheckHandleAllocated(ALV, 'GetItemAt')
  then Exit(-1);

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);
  
  ScrolledTop := Trunc(CListWidget^.vadjustment^.value);

  FirstRowY := ScrolledTop;
  
  // For some reason the actual row height is one pixel more than the size it says
  RowHeight := CListWidget^.row_height+1;

  Dec(y, CListWidget^.column_title_area.height);

  LastRowY := FirstRowY + CListWidget^.clist_window_height;
  
  Inc(y, ScrolledTop);
  
  for I := FirstRowY div RowHeight to LastRowY div RowHeight do
  begin
    if I > CListWidget^.rows-1 then
      Exit;
    if (I * RowHeight < y) and ((I+1) * RowHeight >= y-1) then
      Exit(I);
  end;
end;

class function TGtkWSCustomListView.GetSelCount(const ALV: TCustomListView): Integer;
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'GetSelCount')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);
  
  if CListWidget^.selection = nil
  then Result := 0
  else Result := g_list_length(CListWidget^.selection);
end;

class function TGtkWSCustomListView.GetSelection(const ALV: TCustomListView): Integer;
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'GetSelection')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);
  
  if CListWidget^.selection = nil
  then Result := -1
  else Result := PtrUInt(CListWidget^.selection^.data)
end;

class function TGtkWSCustomListView.GetTopItem(const ALV: TCustomListView): Integer;
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'GetTopItem')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);
  
  // Result := ROW_FROM_YPIXEL(0)
  // #define ROW_FROM_YPIXEL(clist, y)  (((y) - (clist)->voffset) / \
  //                                    ((clist)->row_height + CELL_SPACING))

  Result := -CListWidget^.voffset div (CListWidget^.row_height + 1);
end;

class function TGtkWSCustomListView.GetViewOrigin(const ALV: TCustomListView): TPoint;
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'GetViewOrigin')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);
  if CListWidget=nil then exit;

  Result.X := Round(CListWidget^.hAdjustment^.value);
  Result.Y := Round(CListWidget^.vAdjustment^.value);
end;

class function TGtkWSCustomListView.GetVisibleRowCount(const ALV: TCustomListView): Integer;
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'GetVisibleRowCount')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);
  
  Result := CListWidget^.clist_window_height div (CListWidget^.row_height + 1);
end;

class procedure TGtkWSCustomListView.SetAllocBy(const ALV: TCustomListView;
  const AValue: Integer);
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'SetAllocBy')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);
  if CListWidget=nil then exit;
  // TODO: implement ?
end;

class procedure TGtkWSCustomListView.SetDefaultItemHeight(const ALV: TCustomListView;
  const AValue: Integer);
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
const
  GTK_CLIST_ROW_HEIGHT_SET = 1 shl 1;
begin
  if not WSCheckHandleAllocated(ALV, 'SetDefaultItemHeight')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);

  if (AValue = 0) and (CListWidget^.flags and GTK_CLIST_ROW_HEIGHT_SET = 0) then
    exit;
  gtk_clist_set_row_height(CListWidget, AValue);
end;

class procedure TGtkWSCustomListView.SetHotTrackStyles(const ALV: TCustomListView;
  const AValue: TListHotTrackStyles);
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'SetHotTrackStyles')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);
  
  if CListWidget=nil then exit;
  // TODO: implement ?
end;

class procedure TGtkWSCustomListView.SetHoverTime(const ALV: TCustomListView;
  const AValue: Integer);
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'SetHoverTime')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);
  
  if CListWidget=nil then exit;
  // TODO: implement ?
end;

class procedure TGtkWSCustomListView.SetImageList(const ALV: TCustomListView;
  const AList: TListViewImageList; const AValue: TCustomImageList);
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'SetImageList')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);
  
  if CListWidget=nil then exit;
  // TODO: implement
end;

class procedure TGtkWSCustomListView.SetPropertyInternal(
  const ACListWidget: PGtkCList; const AInfo: PWidgetInfo;
  const AProp: TListViewProperty; const AIsSet: Boolean);
var
  ListViewData: PCustomListViewData;
begin
  case AProp of
    lvpAutoArrange: begin
      // TODO: implement ??
    end;
    lvpCheckboxes: begin
      // TODO: implement
    end;
    lvpColumnClick: begin
      // allow only column modifications when in report mode
      ListViewData := AInfo^.UserData;
      if ListViewData^.ViewStyle <> vsReport then Exit;

      if AIsSet
      then gtk_clist_column_titles_active(ACListWidget)
      else gtk_clist_column_titles_passive(ACListWidget);
    end;
    lvpFlatScrollBars: begin
      // TODO: implement ??
    end;
    lvpFullDrag: begin
      // TODO: implement ??
    end;
    lvpGridLines: begin
      // TODO: implement
      // maybe possible with some cellwidget hacking
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
      then gtk_clist_set_selection_mode(ACListWidget, GTK_SELECTION_EXTENDED)
      else gtk_clist_set_selection_mode(ACListWidget, GTK_SELECTION_BROWSE);
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
      ListViewData := AInfo^.UserData;
      if ListViewData^.ViewStyle <> vsReport then Exit;

      if AIsSet
      then gtk_clist_column_titles_show(ACListWidget)
      else gtk_clist_column_titles_hide(ACListWidget);
    end;
    lvpShowWorkAreas: begin
      // TODO: implement ???
    end;
    lvpWrapText: begin
      // TODO: implement ???
    end;
  end;
end;

class procedure TGtkWSCustomListView.SetProperty(const ALV: TCustomListView;
  const AProp: TListViewProperty; const AIsSet: Boolean);
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'SetProperty')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);
  
  SetPropertyInternal(CListWidget, WidgetInfo, AProp, AIsSet);
end;

class procedure TGtkWSCustomListView.SetProperties(const ALV: TCustomListView; const AProps: TListViewProperties);
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
  Prop: TListViewProperty;
begin
  if not WSCheckHandleAllocated(ALV, 'SetProperties')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);
  
  for Prop := Low(TListViewProperty) to High(TListViewProperty) do
    SetPropertyInternal(CListWidget, WidgetInfo, Prop, Prop in AProps);
end;

class procedure TGtkWSCustomListView.SetScrollBars(const ALV: TCustomListView; const AValue: TScrollStyle);
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

class procedure TGtkWSCustomListView.SetSort(const ALV: TCustomListView;
  const AType: TSortType; const AColumn: Integer);
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'SetSort')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);
  
  if CListWidget=nil then exit;
  // TODO implement
end;

class procedure TGtkWSCustomListView.SetViewOrigin(const ALV: TCustomListView; const AValue: TPoint);
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'SetViewOrigin')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);
  if CListWidget=nil then exit;

  gtk_adjustment_set_value(CListWidget^.hAdjustment, AValue.X);
  gtk_adjustment_set_value(CListWidget^.vAdjustment, AValue.Y);
end;

class procedure TGtkWSCustomListView.SetViewStyle(const ALV: TCustomListView; const Avalue: TViewStyle);
var
  WidgetInfo: PWidgetInfo;
  ListViewData: PCustomListViewData;
begin
  if not WSCheckHandleAllocated(ALV, 'SetViewStyle')
  then Exit;


  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  ListViewData := WidgetInfo^.UserData;
  if ListViewData^.ViewStyle = AValue then Exit; // nothing to do

  // We cannot change columns or viewstyle so we need to recreate
  RecreateWnd(ALV);
end;

