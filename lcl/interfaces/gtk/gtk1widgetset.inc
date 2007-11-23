{%MainUnit gtk1int.pp}

{******************************************************************************
                                   TGtkWidgetSet
 ******************************************************************************

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

{$IFOPT C-}
// Uncomment for local trace
//  {$C+}
//  {$DEFINE ASSERT_IS_ON}
{$ENDIF}

function TGTK1WidgetSet.CreateCListBox(Sender: TObject): PGtkWidget;
var
  ScrolledWnd: PGtkScrolledWindow absolute Result;
  CListBox: TCListBox absolute Sender;
  
  CList: Pointer;
  n: Integer;
begin
  Result := gtk_scrolled_window_new(nil, nil);
  GTK_WIDGET_UNSET_FLAGS(ScrolledWnd^.hscrollbar, GTK_CAN_FOCUS);
  GTK_WIDGET_UNSET_FLAGS(ScrolledWnd^.vscrollbar, GTK_CAN_FOCUS);
  gtk_scrolled_window_set_policy(ScrolledWnd,
                                 GTK_POLICY_AUTOMATIC,
                                 GTK_POLICY_AUTOMATIC);
  gtk_widget_show(Result);

  CList := gtk_clist_new(CListBox.ListColumns);
  for n := 0 to CListBox.ListColumns - 1 do
    gtk_clist_set_column_width(CList, n, (Max(0, CListBox.Width-10)) div CListBox.ListColumns);
    
  gtk_scrolled_window_add_with_viewport(ScrolledWnd, CList);
  gtk_container_set_focus_vadjustment(CList,
                gtk_scrolled_window_get_vadjustment(ScrolledWnd));
  gtk_container_set_focus_hadjustment(Clist,
                gtk_scrolled_window_get_hadjustment(ScrolledWnd));
  gtk_widget_show(CList);

  SetMainWidget(Result, CList);
  GetWidgetInfo(Result, True)^.CoreWidget := CList;
  SetSelectionMode(Sender, Result, CListBox.MultiSelect, CListBox.ExtendedSelect);
end;

{-------------------------------------------------------------------------------
  function TGtk1WidgetSet.CreateComboBox(ComboBoxObject: TObject): Pointer;
-------------------------------------------------------------------------------}
function TGtk1WidgetSet.CreateComboBox(ComboBoxObject: TObject): PGtkWidget;
var
  ComboBox: TComboBox absolute ComboBoxObject;
  Widget: PGtkCombo absolute Result;

  ItemList: TGtkListStringList;
  GtkList: PGtkList;
begin
  Result:= gtk_combo_new();

  SetMainWidget(Result, Widget^.entry);

  gtk_combo_disable_activate(Widget);
  gtk_combo_set_case_sensitive(Widget, GdkTrue);

  // Prevents the OnSelect event be fired after inserting the first item
  // or deleting the selected item
  GtkList:=PGtkList(Widget^.List);
  if GtkList^.selection=nil then
    gtk_list_set_selection_mode(GtkList,GTK_SELECTION_SINGLE)
  else
    gtk_list_set_selection_mode(GtkList,GTK_SELECTION_BROWSE);

  // Items
  ItemList:= TGtkListStringList.Create(GtkList,ComboBox,False);
  gtk_object_set_data(PGtkObject(Widget), GtkListItemLCLListTag, ItemList);
  ItemList.Assign(ComboBox.Items);
  ItemList.Sorted:= ComboBox.Sorted;

  // ItemIndex
  if ComboBox.ItemIndex >= 0 then
    gtk_list_select_item(GtkList, ComboBox.ItemIndex);

  // MaxLength
  gtk_entry_set_max_length(PGtkEntry(Widget^.entry),guint16(ComboBox.MaxLength));
end;

{-------------------------------------------------------------------------------
  function CreateComponentWidget
-------------------------------------------------------------------------------}

function TGTK1WidgetSet.CreateComponentWidget(Sender: TObject; ACompStyle: Integer; const ACaption: String): PGtkWidget;
begin
  case ACompstyle of
    csComboBox:
      Result := CreateComboBox(Sender);
      
    csMemo:
      Result := CreateMemo(Sender);
      
    csListBox, csCheckListBox:
      Result := CreateListBox(Sender);

    csCListBox:
      Result := CreateCListBox(Sender);

  else
    Result := inherited CreateComponentWidget(Sender, ACompStyle, ACaption);
  end;
end;

function TGTK1WidgetSet.CreateListBox(Sender: TObject): PGtkWidget;
var
  ScrolledWnd: PGtkScrolledWindow absolute Result;
  ListBox: TCustomListBox absolute Sender;

  List: Pointer;
begin
  Result := gtk_scrolled_window_new(nil, nil);
  GTK_WIDGET_UNSET_FLAGS(ScrolledWnd^.hscrollbar, GTK_CAN_FOCUS);
  GTK_WIDGET_UNSET_FLAGS(ScrolledWnd^.vscrollbar, GTK_CAN_FOCUS);
  gtk_scrolled_window_set_policy(ScrolledWnd, GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_widget_show(Result);

  List := gtk_list_new;
  gtk_scrolled_window_add_with_viewport(ScrolledWnd, List);
  gtk_container_set_focus_vadjustment(List,
               gtk_scrolled_window_get_vadjustment(ScrolledWnd));
  gtk_container_set_focus_hadjustment(PGtkContainer(List),
               gtk_scrolled_window_get_hadjustment(ScrolledWnd));
  gtk_widget_show(List);

  SetMainWidget(Result, List);
  GetWidgetInfo(Result, True)^.CoreWidget := List;

  SetSelectionMode(Sender, Result, ListBox.MultiSelect, ListBox.ExtendedSelect);
end;

function TGTK1WidgetSet.CreateMemo(Sender: TObject): PGtkWidget;
var
  p: Pointer absolute Result;
  Memo: TCustomMemo absolute Sender;
  
  textwidget: Pointer;
begin
  P := gtk_scrolled_window_new(nil, nil);
  textwidget := gtk_text_new(nil, nil);
  gtk_container_add(p, textwidget);

  GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(p)^.hscrollbar, GTK_CAN_FOCUS);
  GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(p)^.vscrollbar, GTK_CAN_FOCUS);
  gtk_scrolled_window_set_policy(p, GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_text_set_adjustments(textwidget,
    gtk_scrolled_window_get_hadjustment(p),
    gtk_scrolled_window_get_vadjustment(p));

  SetMainWidget(p, textwidget);
  GetWidgetInfo(p, True)^.CoreWidget := textwidget;

  gtk_text_set_editable(textwidget, not Memo.ReadOnly);
  if Memo.WordWrap then
    gtk_text_set_line_wrap(textwidget, GdkTrue)
  else
    gtk_text_set_line_wrap(textwidget, GdkFalse);
  gtk_text_set_word_wrap(textwidget, GdkTrue);

  gtk_widget_show_all(P);
  DebugLn(['TGtkWidgetSet.CreateComponent ',DbgSName(Sender),' ',GetWidgetDebugReport(p)]);
end;

function TGTK1WidgetSet.GetDeviceContextClass: TGtkDeviceContextClass;
begin
  Result := TGtk1DeviceContext;
end;

{------------------------------------------------------------------------------
  procedure SetWidgetFont
    AWidget : PGtkWidget; const AFont: TFont
 ------------------------------------------------------------------------------}
procedure TGtk1WidgetSet.SetWidgetFont(const AWidget: PGtkWidget; const AFont: TFont);
var
  WindowStyle: PGtkStyle;
  FontGdiObject: PGdiObject;

begin
  if GtkWidgetIsA(AWidget,GTKAPIWidget_GetType) then begin
    // the GTKAPIWidget is self drawn, so no use to change the widget style.
    exit;
  end;

  if (GTK_WIDGET_REALIZED(AWidget)) then begin
    WindowStyle := gtk_style_copy(gtk_widget_get_style (AWidget));
  end else begin
    WindowStyle := gtk_style_copy(gtk_rc_get_style (AWidget));
  end;
  if (Windowstyle = nil)  then begin
     Windowstyle := gtk_style_new ;
  end;

  FontGdiObject:=PGdiObject(AFont.Handle);
  windowstyle^.font:=pointer(FontGdiObject^.GdiFontObject);
  gtk_widget_set_style(aWidget,windowStyle);
end;
