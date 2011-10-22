{%MainUnit gtk2wscomctrls.pp}
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

  LCL_NotebookManualPageSwitchKey = 'lcl_manual_page_switch';


type
  GtkNotebookPressEventProc = function (widget:PGtkWidget; event:PGdkEventButton):gboolean; cdecl;
  
var
  OldNoteBookButtonPress: GtkNotebookPressEventProc = nil;

// this was created as a workaround of a tnotebook eating rightclick of custom controls
function Notebook_Button_Press(widget:PGtkWidget; event:PGdkEventButton):gboolean; cdecl;
begin
  Result := True;
  if gtk_get_event_widget(PGdkEvent(event)) <> widget then exit;
  if OldNoteBookButtonPress = nil then exit;
  Result := OldNoteBookButtonPress(widget, event);
end;

procedure HookNoteBookClass;
var
  WidgetClass: PGtkWidgetClass;
begin
  WidgetClass := GTK_WIDGET_CLASS(gtk_type_class(gtk_notebook_get_type));

  OldNoteBookButtonPress := GtkNotebookPressEventProc(WidgetClass^.button_press_event);
  WidgetClass^.button_press_event := @Notebook_Button_Press;
end;

{ TGtk2WSCustomTabControl }


function NotebookPageRealToLCLIndex(const ATabControl: TCustomTabControl; AIndex: integer): integer;
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

function GtkWSNotebook_AfterSwitchPage(widget: PGtkWidget; page: Pgtkwidget; pagenum: integer; data: gPointer): GBoolean; cdecl;
var
  Mess: TLMNotify;
  NMHdr: tagNMHDR;
begin
  Result := CallBackDefaultReturn;
  // then send the new page
  FillChar(Mess, SizeOf(Mess), 0);
  Mess.Msg := LM_NOTIFY;
  FillChar(NMHdr, SizeOf(NMHdr), 0);
  NMHdr.code := TCN_SELCHANGE;
  NMHdr.hwndFrom := PtrUInt(widget);
  NMHdr.idFrom := NotebookPageRealToLCLIndex(TCustomTabControl(Data), pagenum);  //use this to set pageindex to the correct page.
  Mess.NMHdr := @NMHdr;
  DeliverMessage(Data, Mess);
end;

function GtkWSNotebook_SwitchPage(widget: PGtkWidget; page: Pgtkwidget; pagenum: integer; data: gPointer): GBoolean; cdecl;
var
  Mess: TLMNotify;
  NMHdr: tagNMHDR;
  IsManual: Boolean;
begin
  Result := CallBackDefaultReturn;
  EventTrace('switch-page', data);
  UpdateNoteBookClientWidget(TObject(Data));

  // remove flag
  IsManual := gtk_object_get_data(PGtkObject(Widget), LCL_NotebookManualPageSwitchKey) <> nil;
  if IsManual then
    gtk_object_set_data(PGtkObject(Widget), LCL_NotebookManualPageSwitchKey, nil);
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
    NMHdr.idFrom := NotebookPageRealToLCLIndex(TCustomTabControl(Data), pagenum);  //use this to set pageindex to the correct page.
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

end;

class procedure TGtk2WSCustomTabControl.SetCallbacks(
  const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
begin
  TGtk2WSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));
  ConnectSignal(PGtkObject(AGtkWidget), 'switch_page', @GtkWSNotebook_SwitchPage, AWidgetInfo^.LCLObject);
  ConnectSignalAfter(PGtkObject(AGtkWidget), 'switch_page', @GtkWSNotebook_AfterSwitchPage, AWidgetInfo^.LCLObject);
end;

class function TGtk2WSCustomTabControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  AWidget: PGtkNoteBook;
  WidgetInfo: PWidgetInfo;
begin
  if OldNoteBookButtonPress = nil then
    HookNoteBookClass;
  //DebugLn(['TGtk2WSCustomTabControl.CreateHandle ',DbgSName(AWinControl)]);

  AWidget := PGtkNoteBook(gtk_notebook_new());
  WidgetInfo := CreateWidgetInfo(AWidget, AWinControl, AParams);
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Pointer(AWidget), dbgsName(AWinControl));
  {$ENDIF}
  gtk_notebook_set_scrollable(AWidget, True);

  if not (nboHidePageListPopup in TCustomTabControl(AWinControl).Options) then
    gtk_notebook_popup_enable(AWidget);

  if TCustomTabControl(AWinControl).PageCount=0 then
    // a gtk notebook needs a page -> add dummy page
    Gtk2WidgetSet.AddDummyNoteBookPage(AWidget);

  gtk_notebook_set_tab_pos(AWidget, GtkPositionTypeMap[TCustomTabControl(AWinControl).TabPosition]);
  Result := HWND(TLCLIntfHandle(PtrUInt(AWidget)));
  Set_RC_Name(AWinControl, PGtkWidget(AWidget));
  SetCallBacks(PGtkWidget(AWidget), WidgetInfo);
end;

class function TGtk2WSCustomTabControl.GetDefaultClientRect(
  const AWinControl: TWinControl; const aLeft, aTop, aWidth, aHeight: integer;
  var aClientRect: TRect): boolean;
var
  FrameBorders: TRect;
begin
  Result:=false;
  //DebugLn(['TGtk2WSCustomTabControl.GetDefaultClientRect ',DbgSName(AWinControl),' ',aWidth,'x',aHeight]);
  if AWinControl.HandleAllocated then begin

  end else begin
    FrameBorders:=GetStyleNotebookFrameBorders;
    aClientRect:=Rect(0,0,
                 Max(0,aWidth-FrameBorders.Left-FrameBorders.Right),
                 Max(0,aHeight-FrameBorders.Top-FrameBorders.Bottom));
    Result:=true;
  end;
  {$IFDEF VerboseSizeMsg}
  if Result then DebugLn(['TGtk2WSCustomTabControl.GetDefaultClientRect END FrameBorders=',dbgs(FrameBorders),' aClientRect=',dbgs(aClientRect)]);
  {$ENDIF}
end;

class procedure TGtk2WSCustomTabControl.AddPage(const ATabControl: TCustomTabControl;
  const AChild: TCustomPage; const AIndex: integer);
{
  Inserts a new page to a notebook at position Index. The ATabControl is a
  TCustomTabControl, the AChild one of its TCustomPage. Both handles must already
  be created. ATabControl Handle is a PGtkNoteBook and APage handle is a
  PGtkHBox.
  This procedure creates a new tab with an optional image, the page caption and
  an optional close button. The image and the caption will also be added to the
  tab popup menu.
}
var
  NoteBookWidget: PGtkWidget;  // the notebook
  PageWidget: PGtkWidget;      // the page (content widget)
  TabWidget: PGtkWidget;       // the tab (hbox containing a pixmap, a label
                               //          and a close button)
  TabLabelWidget: PGtkWidget;  // the label in the tab
  MenuWidget: PGtkWidget;      // the popup menu (hbox containing a pixmap and
                               // a label)
  MenuLabelWidget: PGtkWidget; // the label in the popup menu item
begin
  {$IFDEF NOTEBOOK_DEBUG}
  DebugLn(['TGtkWSCustomTabControl.AddPage ',dbgsName(ATabControl),' ',ATabControl.HandleAllocated,' AChild=',dbgsName(AChild),' ',AChild.HandleAllocated,' Child.TabVisible=',AChild.TabVisible]);
  {$ENDIF}
  NoteBookWidget := PGtkWidget(ATabControl.Handle);
  PageWidget := PGtkWidget(AChild.Handle);

  // set LCL size
  AChild.SetBounds(AChild.Left, AChild.Top, ATabControl.ClientWidth, ATabControl.ClientHeight);

  if AChild.TabVisible then
    gtk_widget_show(PageWidget);

  // Check if already created. if so just show it because it is invisible
  if gtk_notebook_get_tab_label(PGtkNoteBook(NoteBookWidget), PageWidget) <> nil
  then begin
    {$IFDEF NOTEBOOK_DEBUG}
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
  RemoveDummyNoteBookPage(PGtkNotebook(NoteBookWidget));
  // insert the page
  gtk_notebook_insert_page_menu(PGtkNotebook(NotebookWidget), PageWidget,
    TabWidget, MenuWidget, AIndex);

  UpdateNotebookPageTab(ATabControl, AChild);
  UpdateNoteBookClientWidget(ATabControl);

  // init the size of the page widget
  //DebugLn(['TGtkWSCustomTabControl.AddPage ',DbgSName(ATabControl),' ',dbgs(ATabControl.BoundsRect)]);
  {$IFDEF VerboseSizeMsg}
  DebugLn(['TGtkWSCustomTabControl.AddPage PageWidget^.allocation=',dbgs(PageWidget^.allocation),' NotebookWidget=',dbgs(NotebookWidget^.allocation)]);
  {$ENDIF}
end;

class procedure TGtk2WSCustomTabControl.MovePage(const ATabControl: TCustomTabControl;
  const AChild: TCustomPage; const NewIndex: integer);
var
  NoteBookWidget: PGtkNotebook;
begin
  NoteBookWidget:=PGtkNotebook(ATabControl.Handle);
  gtk_notebook_reorder_child(NoteBookWidget, PGtkWidget(AChild.Handle), NewIndex);
  UpdateNoteBookClientWidget(ATabControl);
end;

class procedure TGtk2WSCustomTabControl.RemovePage(const ATabControl: TCustomTabControl;
  const AIndex: integer);
var
  PageWidget: PGtkWidget;
  Page: TCustomPage;
begin
  // The gtk does not provide a function to remove a page without destroying it.
  // Luckily the LCL destroys the Handle, when a page is removed, so this
  // function is not needed.
  {$IFDEF NOTEBOOK_DEBUG}
  DebugLn(['TGtkWSCustomTabControl.RemovePage AIndex=',AIndex,' ',DbgSName(ATabControl.Page[AIndex])]);
  {$ENDIF}
  Page:=ATabControl.Page[AIndex];
  if not Page.HandleAllocated then exit;
  PageWidget := PGtkWidget(Page.Handle);
  gtk_widget_hide(PageWidget);
end;

class function TGtk2WSCustomTabControl.GetCapabilities: TNoteBookCapabilities;
begin
  Result:=[nbcPageListPopup, nbcShowCloseButtons];
end;

class function TGtk2WSCustomTabControl.GetNotebookMinTabHeight(
  const AWinControl: TWinControl): integer;
var
  NBWidget: PGTKWidget;
  BorderWidth: Integer;
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

  // ToDo: find out how to create a fully working hidden Notebook style widget

  if (NBWidget=nil) then begin
    Result:=TWSCustomTabControl.GetNotebookMinTabHeight(AWinControl);
    exit;
  end;
  debugln('TGtkWSCustomTabControl.GetNotebookMinTabHeight NBWidget: ',GetWidgetDebugReport(NBWidget),
   ' ',dbgs(NBWidget^.allocation.width),'x',dbgs(NBWidget^.allocation.height));

  BorderWidth:=(PGtkContainer(NBWidget)^.flag0 and bm_TGtkContainer_border_width)
               shr bp_TGtkContainer_border_width;
  if PGtkNoteBook(NBWidget)^.first_tab<>nil then
    Page:=PGtkNoteBook(NBWidget)^.cur_page;

  Result:=BorderWidth;
  if (Page<>nil) then begin
    debugln('TGtkWSCustomTabControl.RemovePage TODO');
  end;
  debugln('TGtkWSCustomTabControl.GetNotebookMinTabHeight END ',dbgs(Result),' ',
    GetWidgetDebugReport(NBWidget));
end;

class function TGtk2WSCustomTabControl.GetNotebookMinTabWidth(
  const AWinControl: TWinControl): integer;
begin
  Result:=TWSCustomTabControl.GetNotebookMinTabWidth(AWinControl);
end;

class function TGtk2WSCustomTabControl.GetTabIndexAtPos(
  const ATabControl: TCustomTabControl; const AClientPos: TPoint): integer;
var
  NoteBookWidget: PGtkNotebook;
  i: integer;
  TabWidget: PGtkWidget;
  PageWidget: PGtkWidget;
  NotebookPos: TPoint;
  Window: PGdkWindow;
  WindowOrg,ClientOrg: TPoint;
  Count: guint;
begin
  Result:=-1;
  NoteBookWidget:=PGtkNotebook(ATabControl.Handle);
  if (NotebookWidget=nil) then exit;
  //DebugLn(['TGtkWSCustomTabControl.GetTabIndexAtPos ',GetWidgetDebugReport(PGtkWidget(NotebookWidget))]);
  Window := GetControlWindow(NoteBookWidget);
  gdk_window_get_origin(Window,@WindowOrg.X,@WindowOrg.Y);
  ClientOrg:=GetWidgetClientOrigin(PGtkWidget(NotebookWidget));
  NotebookPos.X:= AClientPos.X + (ClientOrg.X-WindowOrg.X);
  NotebookPos.Y:= AClientPos.Y + (ClientOrg.Y-WindowOrg.Y);
  // go through all tabs
  Count:=g_list_length(NoteBookWidget^.Children);
  for i:=0 to Count-1 do
  begin
    PageWidget:=gtk_notebook_get_nth_page(NoteBookWidget,i);
    if PageWidget<>nil then
    begin
      TabWidget:=gtk_notebook_get_tab_label(NoteBookWidget, PageWidget);
      if (TabWidget<>nil) and GTK_WIDGET_MAPPED(TabWidget) then
      begin
        // test if position is in tabwidget
        if (TabWidget^.Allocation.X<=NoteBookPos.X)
        and (TabWidget^.Allocation.Y<=NoteBookPos.Y)
        and (TabWidget^.Allocation.X+TabWidget^.Allocation.Width>NoteBookPos.X)
        and (TabWidget^.Allocation.Y+TabWidget^.Allocation.Height>NoteBookPos.Y)
        then begin
          Result:=i;
          exit;
        end;
      end;
    end;
  end;
end;

class function TGtk2WSCustomTabControl.GetTabRect(const ATabControl: TCustomTabControl;
  const AIndex: Integer): TRect;
var
  NoteBookWidget: PGtkNotebook;
  TabWidget: PGtkWidget;
  PageWidget: PGtkWidget;
  Count: guint;
begin
  Result := inherited;
  NoteBookWidget:=PGtkNotebook(ATabControl.Handle);
  if (NotebookWidget=nil) then exit;

  Count := g_list_length(NoteBookWidget^.Children);
  PageWidget := gtk_notebook_get_nth_page(NoteBookWidget, AIndex);
  if (PageWidget<>nil) and (AIndex < Count) then
  begin
    TabWidget := gtk_notebook_get_tab_label(NoteBookWidget, PageWidget);
    if TabWidget <> nil then
      Result := RectFromGdkRect(TabWidget^.allocation);
  end;
end;

class procedure TGtk2WSCustomTabControl.SetPageIndex(
  const ATabControl: TCustomTabControl; const AIndex: integer);
var
  GtkNotebook: PGtkNotebook;
begin
  if not WSCheckHandleAllocated(ATabControl, 'SetPageIndex') then
    Exit;

  GtkNotebook := PGtkNoteBook(ATabControl.Handle);
  if gtk_notebook_get_current_page(GtkNotebook) <> AIndex then
  begin
    gtk_object_set_data(PGtkObject(GtkNotebook), LCL_NotebookManualPageSwitchKey, ATabControl);
    gtk_notebook_set_page(GtkNotebook, AIndex);
  end;
  UpdateNoteBookClientWidget(ATabControl);
end;

class procedure TGtk2WSCustomTabControl.SetTabPosition(
  const ATabControl: TCustomTabControl; const ATabPosition: TTabPosition);
begin
  gtk_notebook_set_tab_pos(PGtkNotebook(ATabControl.Handle),
    GtkPositionTypeMap[ATabPosition]);
end;

class procedure TGtk2WSCustomTabControl.ShowTabs(const ATabControl: TCustomTabControl;
  AShowTabs: boolean);
begin
  gtk_notebook_set_show_tabs(PGtkNotebook(ATabControl.Handle), AShowTabs);
end;

class procedure TGtk2WSCustomTabControl.UpdateProperties(const ATabControl: TCustomTabControl);
begin
  if (nboHidePageListPopup in ATabControl.Options) then
    gtk_notebook_popup_disable(PGtkNotebook(ATabControl.Handle))
  else
    gtk_notebook_popup_enable(PGtkNotebook(ATabControl.Handle));
end;



{ TGtk2WSCustomPage }

class procedure TGtk2WSCustomPage.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtk2WSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));
end;

class function TGtk2WSCustomPage.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
begin
  Widget := Gtk2Widgetset.CreateSimpleClientAreaWidget(AWinControl, True);
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

class procedure TGtk2WSCustomPage.UpdateProperties(const ACustomPage: TCustomPage);
var
  NoteBook: PGtkWidget;
  PageWidget: PGtkWidget;
  TabWidget: PGtkWidget;
  TabImageWidget: PGtkWidget;
begin
  UpdateNotebookPageTab(nil, ACustomPage);
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
end;

class procedure TGtk2WSCustomPage.SetBounds(const AWinControl: TWinControl;
  const ALeft, ATop, AWidth, AHeight: Integer);
begin
  // ignore resizes from the LCL
end;

class procedure TGtk2WSCustomPage.ShowHide(const AWinControl: TWinControl);
begin
  if (csDesigning in AWinControl.ComponentState) then
    TGtk2WidgetSet(WidgetSet).SetVisible(AWinControl,
      AWinControl.HandleObjectShouldBeVisible)
  else
    TGtk2WidgetSet(WidgetSet).SetVisible(AWinControl,
      TCustomPage(AWinControl).TabVisible);
end;

class function TGtk2WSCustomPage.GetDefaultClientRect(
  const AWinControl: TWinControl; const aLeft, aTop, aWidth, aHeight: integer;
  var aClientRect: TRect): boolean;
begin
  Result:=false;
  if AWinControl.Parent=nil then exit;
  if AWinControl.HandleAllocated and AWinControl.Parent.HandleAllocated
  and (PGtkWidget(AWinControl.Handle)^.parent<>nil) then
  begin

  end else begin
    Result:=true;
    aClientRect:=AWinControl.Parent.ClientRect;
    //DebugLn(['TGtk2WSCustomPage.GetDefaultClientRect ',DbgSName(AWinControl),' Parent=',DbgSName(AWinControl.Parent),' ParentBounds=',dbgs(AWinControl.Parent.BoundsRect),' ParentClient=',dbgs(AWinControl.Parent.ClientRect)]);
  end;
  {$IFDEF VerboseSizeMsg}
  if Result then DebugLn(['TGtk2WSCustomPage.GetDefaultClientRect ',DbgSName(AWinControl),' aClientRect=',dbgs(aClientRect)]);
  {$ENDIF}
end;

