{%MainUnit gtkwscomctrls.pp}
{
 *****************************************************************************
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

const
  GtkPositionTypeMap: array[TTabPosition] of TGtkPositionType =
  (
{ tpTop    } GTK_POS_TOP,
{ tpBottom } GTK_POS_BOTTOM,
{ tpLeft   } GTK_POS_LEFT,
{ tpRight  } GTK_POS_RIGHT
  );
  
  LCL_TabControlManualPageSwitchKey = 'lcl_manual_page_switch';

{ TGtkWSCustomPage }

class procedure TGtkWSCustomPage.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtkWSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));
end;

class function TGtkWSCustomPage.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
begin
  Widget := GtkWidgetset.CreateSimpleClientAreaWidget(AWinControl, True);
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget, dbgsName(AWinControl));
  {$ENDIF}
  Result := TLCLIntfHandle(PtrUInt(Widget));
  
  WidgetInfo := GetWidgetInfo(Widget);
  WidgetInfo^.LCLObject := AWinControl;
  WidgetInfo^.Style := AParams.Style;
  WidgetInfo^.ExStyle := AParams.ExStyle;
  WidgetInfo^.WndProc := PtrUInt(AParams.WindowClass.lpfnWndProc);
  
  Set_RC_Name(AWinControl, Widget);
  SetCallBacks(Widget, WidgetInfo);
end;

class procedure TGtkWSCustomPage.UpdateProperties(const ACustomPage: TCustomPage);
{$ifdef gtk2}
var
  NoteBook: PGtkWidget;
  PageWidget: PGtkWidget;
  TabWidget: PGtkWidget;
  TabImageWidget: PGtkWidget;
{$endif}
begin
  UpdateNotebookPageTab(nil, ACustomPage);
  {$ifdef gtk2}
  {we must update our icon (if exists) otherwise it will be updated only
  when our tab reach focus}
  if not (csDesigning in ACustomPage.ComponentState)
    and not ACustomPage.TabVisible
    or not ACustomPage.HandleAllocated
    or not Assigned(ACustomPage.Parent)
  then
    exit;

  PageWidget := PGtkWidget(ACustomPage.Handle);
  NoteBook := PGtkWidget(ACustomPage.Parent.Handle);
  if (NoteBook = nil) or not GTK_IS_NOTEBOOK(NoteBook) then
    exit;

  TabWidget := gtk_notebook_get_tab_label(PGtkNoteBook(Notebook), PageWidget);
  if (TabWidget = nil) or not GTK_WIDGET_VISIBLE(TabWidget) then
    exit;

  TabImageWidget := gtk_object_get_data(PGtkObject(TabWidget), 'TabImage');
  if TabImageWidget <> nil then
    gtk_widget_queue_draw(TabImageWidget);
  {$endif}
end;

class procedure TGtkWSCustomPage.SetBounds(const AWinControl: TWinControl;
  const ALeft, ATop, AWidth, AHeight: Integer);
begin
  // ignore resizes from the LCL
end;

class procedure TGtkWSCustomPage.ShowHide(const AWinControl: TWinControl);
begin
  {$ifdef gtk2}
  if (csDesigning in AWinControl.ComponentState) then
    TGtkWidgetSet(WidgetSet).SetVisible(AWinControl,
      AWinControl.HandleObjectShouldBeVisible)
  else
  {$endif}
    TGtkWidgetSet(WidgetSet).SetVisible(AWinControl,
      TCustomPage(AWinControl).TabVisible);
end;

{ TGtkWSCustomTabControl }

function TabControlPageRealToLCLIndex(const ATabControl: TCustomTabControl; AIndex: integer): integer;
var
  I: Integer;
begin
  Result := AIndex;
  if csDesigning in ATabControl.ComponentState then exit;
  I := 0;
  while (I < ATabControl.PageCount) and (I <= Result) do
  begin
    if not ATabControl.Page[I].TabVisible then Inc(Result);
    Inc(I);
  end;
end;


function GtkWSTabControl_SwitchPage(widget: PGtkWidget; page: Pgtkwidget; pagenum: integer; data: gPointer): GBoolean; cdecl;
var
  Mess: TLMNotify;
  NMHdr: tagNMHDR;
  IsManual: Boolean;
begin
  Result := CallBackDefaultReturn;
  EventTrace('switch-page', data);
  UpdateNotebookClientWidget(TObject(Data));

  // remove flag
  IsManual := gtk_object_get_data(PGtkObject(Widget), LCL_TabControlManualPageSwitchKey) <> nil;
  if IsManual then
    gtk_object_set_data(PGtkObject(Widget), LCL_TabControlManualPageSwitchKey, nil);
  if PGtkNotebook(Widget)^.cur_page = nil then // for windows compatibility
    Exit;

  // gtkswitchpage is called before the switch
  if not IsManual then
  begin
    // send first the TCN_SELCHANGING to ask if switch is allowed
    FillChar(Mess, SizeOf(Mess), 0);
    Mess.Msg := LM_NOTIFY;
    FillChar(NMHdr, SizeOf(NMHdr), 0);
    NMHdr.code := TCN_SELCHANGING;
    NMHdr.hwndFrom := PtrUInt(widget);
    NMHdr.idFrom := TabControlPageRealToLCLIndex(TCustomTabControl(Data), pagenum);  //use this to set pageindex to the correct page.
    Mess.NMHdr := @NMHdr;
    Mess.Result := 0;
    DeliverMessage(Data, Mess);
    if Mess.Result <> 0 then
    begin
      g_signal_stop_emission_by_name(PGtkObject(Widget), 'switch-page');
      Result := not CallBackDefaultReturn;
      Exit;
    end;
  end;

  // then send the new page
  FillChar(Mess, SizeOf(Mess), 0);
  Mess.Msg := LM_NOTIFY;
  FillChar(NMHdr, SizeOf(NMHdr), 0);
  NMHdr.code := TCN_SELCHANGE;
  NMHdr.hwndFrom := PtrUInt(widget);
  NMHdr.idFrom := TabControlPageRealToLCLIndex(TCustomTabControl(Data), pagenum);  //use this to set pageindex to the correct page.
  Mess.NMHdr := @NMHdr;
  DeliverMessage(Data, Mess);
end;

class procedure TGtkWSCustomTabControl.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtkWSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));
  ConnectSignal(PGtkObject(AGtkWidget), 'switch_page', @GtkWSTabControl_SwitchPage, AWidgetInfo^.LCLObject);
end;

class function TGtkWSCustomTabControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  AWidget: PGtkNotebook;
  WidgetInfo: PWidgetInfo;
begin
  AWidget := PGtkNotebook(gtk_notebook_new());
  WidgetInfo := CreateWidgetInfo(AWidget, AWinControl, AParams);
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Pointer(AWidget), dbgsName(AWinControl));
  {$ENDIF}
  gtk_notebook_set_scrollable(AWidget, True);

  if not (nboHidePageListPopup in TCustomTabControl(AWinControl).Options) then
    gtk_notebook_popup_enable(AWidget);

  if TCustomTabControl(AWinControl).PageCount=0 then
    // a gtk TabControl needs a page -> add dummy page
    GTKWidgetSet.AddDummyNotebookPage(AWidget);

  gtk_notebook_set_tab_pos(AWidget, GtkPositionTypeMap[TCustomTabControl(AWinControl).TabPosition]);
  Result := TLCLIntfHandle(PtrUInt(AWidget));
  Set_RC_Name(AWinControl, PGtkWidget(AWidget));
  SetCallBacks(PGtkWidget(AWidget), WidgetInfo);
end;

class procedure TGtkWSCustomTabControl.AddPage(const ATabControl: TCustomTabControl;
  const AChild: TCustomPage; const AIndex: integer);
{
  Inserts a new page to a TabControl at position Index. The ATabControl is a
  TCustomTabControl, the AChild one of its TCustomPage. Both handles must already
  be created. ATabControl Handle is a PGtkNotebook and APage handle is a
  PGtkHBox.
  This procedure creates a new tab with an optional image, the page caption and
  an optional close button. The image and the caption will also be added to the
  tab popup menu.
}
var
  TabControlWidget: PGtkWidget;  // the TabControl
  PageWidget: PGtkWidget;      // the page (content widget)
  TabWidget: PGtkWidget;       // the tab (hbox containing a pixmap, a label
                               //          and a close button)
  TabLabelWidget: PGtkWidget;  // the label in the tab
  MenuWidget: PGtkWidget;      // the popup menu (hbox containing a pixmap and
                               // a label)
  MenuLabelWidget: PGtkWidget; // the label in the popup menu item
begin
  {$IFDEF TabControl_DEBUG}
  DebugLn(['TGtkWSCustomTabControl.AddPage ',dbgsName(ATabControl),' ',ATabControl.HandleAllocated,' AChild=',dbgsName(AChild),' ',AChild.HandleAllocated,' Child.TabVisible=',AChild.TabVisible]);
  {$ENDIF}
  TabControlWidget := PGtkWidget(ATabControl.Handle);
  PageWidget := PGtkWidget(AChild.Handle);

  // set LCL size
  AChild.SetBounds(AChild.Left, AChild.Top, ATabControl.ClientWidth, ATabControl.ClientHeight);

  if AChild.TabVisible then
    gtk_widget_show(PageWidget);

  // Check if already created. if so just show it because it is invisible
  if gtk_notebook_get_tab_label(PGtkNotebook(TabControlWidget), PageWidget) <> nil
  then begin
    {$IFDEF TabControl_DEBUG}
    DebugLn(['TGtkWSCustomTabControl.AddPage already added']);
    {$ENDIF}
    exit;
  end;
  
  // create the tab (hbox container)
  TabWidget := gtk_hbox_new(false, 1);
  gtk_object_set_data(PGtkObject(TabWidget), 'TabImage', nil);
  gtk_object_set_data(PGtkObject(TabWidget), 'TabCloseBtn', nil);
  // put a label into the tab
  TabLabelWidget := gtk_label_new('');
  gtk_object_set_data(PGtkObject(TabWidget), 'TabLabel', TabLabelWidget);
  gtk_widget_show(TabLabelWidget);
  gtk_box_pack_start_defaults(PGtkBox(TabWidget), TabLabelWidget);

  if AChild.TabVisible then
    gtk_widget_show(TabWidget);

  // create popup menu item
  MenuWidget := gtk_hbox_new(false, 2);
  // set icon widget to nil
  gtk_object_set_data(PGtkObject(MenuWidget), 'TabImage', nil);
  // put a label into the menu
  MenuLabelWidget := gtk_label_new('');
  gtk_object_set_data(PGtkObject(MenuWidget), 'TabLabel', MenuLabelWidget);
  gtk_widget_show(MenuLabelWidget);
  gtk_box_pack_start_defaults(PGtkBox(MenuWidget), MenuLabelWidget);

  if AChild.TabVisible then
    gtk_widget_show(MenuWidget);

  // remove the dummy page (a gtk_notebook needs at least one page)
  RemoveDummyNotebookPage(PGtkNotebook(TabControlWidget));
  // insert the page
  gtk_notebook_insert_page_menu(PGtkNotebook(TabControlWidget), PageWidget,
    TabWidget, MenuWidget, AIndex);

  UpdateNotebookPageTab(ATabControl, AChild);
  UpdateNotebookClientWidget(ATabControl);
  
  // init the size of the page widget
  //DebugLn(['TGtkWSCustomTabControl.AddPage ',DbgSName(ATabControl),' ',dbgs(ATabControl.BoundsRect)]);
  {$IFDEF VerboseSizeMsg}
  DebugLn(['TGtkWSCustomTabControl.AddPage PageWidget^.allocation=',dbgs(PageWidget^.allocation),' TabControlWidget=',dbgs(TabControlWidget^.allocation)]);
  {$ENDIF}
end;

class procedure TGtkWSCustomTabControl.MovePage(const ATabControl: TCustomTabControl;
  const AChild: TCustomPage; const NewIndex: integer);
var
  TabControlWidget: PGtkNotebook;
begin
  TabControlWidget:=PGtkNotebook(ATabControl.Handle);
  gtk_notebook_reorder_child(TabControlWidget, PGtkWidget(AChild.Handle), NewIndex);
  UpdateNotebookClientWidget(ATabControl);
end;

class procedure TGtkWSCustomTabControl.RemovePage(const ATabControl: TCustomTabControl;
  const AIndex: integer);
var
  PageWidget: PGtkWidget;
  Page: TCustomPage;
begin
  // The gtk does not provide a function to remove a page without destroying it.
  // Luckily the LCL destroys the Handle, when a page is removed, so this
  // function is not needed.
  {$IFDEF TabControl_DEBUG}
  DebugLn(['TGtkWSCustomTabControl.RemovePage AIndex=',AIndex,' ',DbgSName(ATabControl.Page[AIndex])]);
  {$ENDIF}
  Page:=ATabControl.Page[AIndex];
  if not Page.HandleAllocated then exit;
  PageWidget := PGtkWidget(Page.Handle);
  gtk_widget_hide(PageWidget);
end;

class function TGtkWSCustomTabControl.GetCapabilities: TNotebookCapabilities;
begin
  Result:=[nbcPageListPopup, nbcShowCloseButtons];
end;

class function TGtkWSCustomTabControl.GetNotebookMinTabHeight(
  const AWinControl: TWinControl): integer;
var
  NBWidget: PGTKWidget;
  BorderWidth: Integer;
  {$IFDEF Gtk1}
  Requisition: TGtkRequisition;
  {$ENDIF}
  Page: PGtkNotebookPage;
begin
  Result:=inherited GetNotebookMinTabHeight(AWinControl);
  //debugln('TGtkWSCustomTabControl.GetNotebookMinTabHeight A ',dbgs(Result));
  exit;

  debugln('TGtkWSCustomTabControl.GetNotebookMinTabHeight A ',dbgs(AWinControl.HandleAllocated));
  if AWinControl.HandleAllocated then
    NBWidget:=PGTKWidget(AWinControl.Handle)
  else
    NBWidget:=GetStyleWidget(lgsNotebook);

  // ToDo: find out how to create a fully working hidden TabControl style widget

  if (NBWidget=nil) then begin
    Result:=TWSCustomTabControl.GetNotebookMinTabHeight(AWinControl);
    exit;
  end;
  debugln('TGtkWSCustomTabControl.GetNotebookMinTabHeight NBWidget: ',GetWidgetDebugReport(NBWidget),
   ' ',dbgs(NBWidget^.allocation.width),'x',dbgs(NBWidget^.allocation.height));
  
  BorderWidth:=(PGtkContainer(NBWidget)^.flag0 and bm_TGtkContainer_border_width)
               shr bp_TGtkContainer_border_width;
  if PGtkNotebook(NBWidget)^.first_tab<>nil then
    Page:=PGtkNotebook(NBWidget)^.cur_page;

  Result:=BorderWidth;
  {$IFDEF GTK2}
  if (Page<>nil) then begin
    debugln('TGtkWSCustomTabControl.RemovePage TODO');
  end;
  {$ELSE GTK2}
  if (NBWidget^.thestyle<>nil) and (PGtkStyle(NBWidget^.thestyle)^.klass<>nil) then
    inc(Result,PGtkStyle(NBWidget^.thestyle)^.klass^.ythickness);
  if (Page<>nil) and (Page^.child<>nil) then begin
    gtk_widget_size_request(Page^.Child, @Requisition);
    gtk_widget_map(Page^.child);
    debugln('TGtkWSCustomTabControl.GetNotebookMinTabHeight B ',dbgs(Page^.child^.allocation.height),
      ' ',GetWidgetDebugReport(Page^.child),' Requisition=',dbgs(Requisition.height));
    inc(Result,Page^.child^.allocation.height);
  end;
  {$ENDIF GTK2}
  debugln('TGtkWSCustomTabControl.GetNotebookMinTabHeight END ',dbgs(Result),' ',
    GetWidgetDebugReport(NBWidget));
end;

class function TGtkWSCustomTabControl.GetNotebookMinTabWidth(
  const AWinControl: TWinControl): integer;
begin
  Result:=TWSCustomTabControl.GetNotebookMinTabWidth(AWinControl);
end;

class function TGtkWSCustomTabControl.GetTabIndexAtPos(
  const ATabControl: TCustomTabControl; const AClientPos: TPoint): integer;
var
  TabControlWidget: PGtkNotebook;
  i: integer;
  TabWidget: PGtkWidget;
  PageWidget: PGtkWidget;
  TabControlPos: TPoint;
  {$IFDEF GTK2}
  Window: PGdkWindow;
  WindowOrg,ClientOrg: TPoint;
  {$ENDIF}
  Count: guint;
begin
  Result:=-1;
  TabControlWidget:=PGtkNotebook(ATabControl.Handle);
  if (TabControlWidget=nil) then exit;
  //DebugLn(['TGtkWSCustomTabControl.GetTabIndexAtPos ',GetWidgetDebugReport(PGtkWidget(TabControlWidget))]);
  {$IFDEF GTK2}
  Window := GetControlWindow(TabControlWidget);
  gdk_window_get_origin(Window,@WindowOrg.X,@WindowOrg.Y);
  ClientOrg:=GetWidgetClientOrigin(PGtkWidget(TabControlWidget));
  TabControlPos.X:= AClientPos.X + (ClientOrg.X-WindowOrg.X);
  TabControlPos.Y:= AClientPos.Y + (ClientOrg.Y-WindowOrg.Y);
  {$ELSE}
  TabControlPos:=AClientPos;
  {$ENDIF}
  // go through all tabs
  Count:=g_list_length(TabControlWidget^.Children);
  for i:=0 to Count-1 do
  begin
    PageWidget:=gtk_notebook_get_nth_page(TabControlWidget,i);
    if PageWidget<>nil then
    begin
      TabWidget:=gtk_notebook_get_tab_label(TabControlWidget, PageWidget);
      if (TabWidget<>nil) and GTK_WIDGET_MAPPED(TabWidget) then
      begin
        // test if position is in tabwidget
        if (TabWidget^.Allocation.X<=TabControlPos.X)
        and (TabWidget^.Allocation.Y<=TabControlPos.Y)
        and (TabWidget^.Allocation.X+TabWidget^.Allocation.Width>TabControlPos.X)
        and (TabWidget^.Allocation.Y+TabWidget^.Allocation.Height>TabControlPos.Y)
        then begin
          Result:=i;
          exit;
        end;
      end;
    end;
  end;
end;

class function TGtkWSCustomTabControl.GetTabRect(const ATabControl: TCustomTabControl;
  const AIndex: Integer): TRect;
var
  TabControlWidget: PGtkNotebook;
  TabWidget: PGtkWidget;
  PageWidget: PGtkWidget;
  {$IFDEF GTK2}
  Window: PGdkWindow;
  WindowOrg,ClientOrg: TPoint;
  {$ENDIF}
  XOffset, YOffset: Integer;
  Count: guint;
begin
  Result := inherited;
  TabControlWidget:=PGtkNotebook(ATabControl.Handle);
  if (TabControlWidget=nil) then exit;
  //DebugLn(['TGtkWSCustomTabControl.GetTabIndexAtPos ',GetWidgetDebugReport(PGtkWidget(TabControlWidget))]);
  {$IFDEF GTK2}
  Window := GetControlWindow(TabControlWidget);
  gdk_window_get_origin(Window,@WindowOrg.X,@WindowOrg.Y);
  ClientOrg:=GetWidgetClientOrigin(PGtkWidget(TabControlWidget));
  XOffset := (ClientOrg.X-WindowOrg.X);
  YOffset := (ClientOrg.Y-WindowOrg.Y);
  {$ELSE}
  XOffset := 0;
  YOffset := 0;
  {$ENDIF}
  // go through all tabs
  Count:=g_list_length(TabControlWidget^.Children);
  PageWidget:=gtk_notebook_get_nth_page(TabControlWidget, AIndex);
  if (PageWidget<>nil) and (AIndex < Count) then begin
    TabWidget:=gtk_notebook_get_tab_label(TabControlWidget, PageWidget);
    if TabWidget<>nil then begin
      Result.Top := TabWidget^.Allocation.Y - YOffset;
      Result.Bottom := TabWidget^.Allocation.Y - YOffset + TabWidget^.Allocation.Height;
      Result.Left := TabWidget^.Allocation.X - XOffset;
      Result.right := TabWidget^.Allocation.X - XOffset + TabWidget^.Allocation.Width;
      exit;
    end;
  end;
end;

class procedure TGtkWSCustomTabControl.SetPageIndex(
  const ATabControl: TCustomTabControl; const AIndex: integer);
var
  GtkTabControl: PGtkNotebook;
begin
  if not WSCheckHandleAllocated(ATabControl, 'SetPageIndex') then
    Exit;
    
  GtkTabControl := PGtkNotebook(ATabControl.Handle);
  if gtk_notebook_get_current_page(GtkTabControl) <> AIndex then
  begin
    gtk_object_set_data(PGtkObject(GtkTabControl), LCL_TabControlManualPageSwitchKey, ATabControl);
    gtk_notebook_set_page(GtkTabControl, AIndex);
  end;
  UpdateNotebookClientWidget(ATabControl);
end;

class procedure TGtkWSCustomTabControl.SetTabPosition(
  const ATabControl: TCustomTabControl; const ATabPosition: TTabPosition);
begin
  gtk_notebook_set_tab_pos(PGtkNotebook(ATabControl.Handle),
    GtkPositionTypeMap[ATabPosition]);
end;

class procedure TGtkWSCustomTabControl.ShowTabs(const ATabControl: TCustomTabControl;
  AShowTabs: boolean);
begin
  gtk_notebook_set_show_tabs(PGtkNotebook(ATabControl.Handle), AShowTabs);
end;

class procedure TGtkWSCustomTabControl.UpdateProperties(const ATabControl: TCustomTabControl);
begin
  if (nboHidePageListPopup in ATabControl.Options) then
    gtk_notebook_popup_disable(PGtkNotebook(ATabControl.Handle))
  else
    gtk_notebook_popup_enable(PGtkNotebook(ATabControl.Handle));
end;


