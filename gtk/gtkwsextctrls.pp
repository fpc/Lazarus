{ $Id$}
{
 *****************************************************************************
 *                             GtkWSExtCtrls.pp                              * 
 *                             ----------------                              * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

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
unit GtkWSExtCtrls;

{$mode objfpc}{$H+}

interface

uses
  LCLProc, LCLType, LCLIntf, LMessages,
{$IFDEF GTK2}
  gtk2, gdk2, gdk2PixBuf, glib2,
{$ELSE GTK2}
  gtk, gdk, glib, gtk1WSPrivate, graphics,
{$ENDIF GTK2}
  GtkExtra, GtkWsControls,
  GtkGlobals, GtkProc, GtkDef, GtkInt,
  SysUtils, Classes, Controls, ExtCtrls, Forms, Menus,
  WSExtCtrls, WSLCLClasses, WSProc, InterfaceBase;

type

  { TGtkWSCustomPage }

  TGtkWSCustomPage = class(TWSCustomPage)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure UpdateProperties(const ACustomPage: TCustomPage); override;
    class procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); override;
    class procedure ShowHide(const AWinControl: TWinControl); override;
  end;

  { TGtkWSCustomNotebook }

  TGtkWSCustomNotebook = class(TWSCustomNotebook)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure AddPage(const ANotebook: TCustomNotebook;
      const AChild: TCustomPage; const AIndex: integer); override;
    class procedure MovePage(const ANotebook: TCustomNotebook; 
      const AChild: TCustomPage; const NewIndex: integer); override;
    class procedure RemovePage(const ANotebook: TCustomNotebook; 
      const AIndex: integer); override;
    
    class function GetCapabilities: TNoteBookCapabilities; override;
    class function GetNotebookMinTabHeight(const AWinControl: TWinControl): integer; override;
    class function GetNotebookMinTabWidth(const AWinControl: TWinControl): integer; override;
    class function GetTabIndexAtPos(const ANotebook: TCustomNotebook; const AClientPos: TPoint): integer; override;
    class function GetTabRect(const ANotebook: TCustomNotebook; const AIndex: Integer): TRect; override;
    class procedure SetPageIndex(const ANotebook: TCustomNotebook; const AIndex: integer); override;
    class procedure SetTabPosition(const ANotebook: TCustomNotebook; const ATabPosition: TTabPosition); override;
    class procedure ShowTabs(const ANotebook: TCustomNotebook; AShowTabs: boolean); override;
    class procedure UpdateProperties(const ANotebook: TCustomNotebook); override;
  end;

  { TGtkWSPage }

  TGtkWSPage = class(TWSPage)
  published
  end;

  { TGtkWSNotebook }

  TGtkWSNotebook = class(TWSNotebook)
  published
  end;

  { TGtkWSShape }

  TGtkWSShape = class(TWSShape)
  published
  end;

  { TGtkWSCustomSplitter }

  TGtkWSCustomSplitter = class(TWSCustomSplitter)
  published
  end;

  { TGtkWSSplitter }

  TGtkWSSplitter = class(TWSSplitter)
  published
  end;

  { TGtkWSPaintBox }

  TGtkWSPaintBox = class(TWSPaintBox)
  published
  end;

  { TGtkWSCustomImage }

  TGtkWSCustomImage = class(TWSCustomImage)
  published
  end;

  { TGtkWSImage }

  TGtkWSImage = class(TWSImage)
  published
  end;

  { TGtkWSBevel }

  TGtkWSBevel = class(TWSBevel)
  published
  end;

  { TGtkWSCustomRadioGroup }

  TGtkWSCustomRadioGroup = class(TWSCustomRadioGroup)
  published
  end;

  { TGtkWSRadioGroup }

  TGtkWSRadioGroup = class(TWSRadioGroup)
  published
  end;

  { TGtkWSCustomCheckGroup }

  TGtkWSCustomCheckGroup = class(TWSCustomCheckGroup)
  published
  end;

  { TGtkWSCheckGroup }

  TGtkWSCheckGroup = class(TWSCheckGroup)
  published
  end;

  { TGtkWSCustomLabeledEdit }

  TGtkWSCustomLabeledEdit = class(TWSCustomLabeledEdit)
  published
  end;

  { TGtkWSLabeledEdit }

  TGtkWSLabeledEdit = class(TWSLabeledEdit)
  published
  end;

  { TGtkWSCustomPanel }

  TGtkWSCustomPanel = class(TWSCustomPanel)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetColor(const AWinControl: TWinControl); override;
  end;

  { TGtkWSPanel }

  TGtkWSPanel = class(TWSPanel)
  published
  end;

  { TGtkWSCustomTrayIcon }

{$IFDEF GTK1}
  TGtkWSCustomTrayIcon = class(TWSCustomTrayIcon)
  published
    class function Hide(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class function Show(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class procedure InternalUpdate(const ATrayIcon: TCustomTrayIcon); override;
    class function GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint; override;
    class function GetCanvas(const ATrayIcon: TCustomTrayIcon): TCanvas; override;
  end;
{$ENDIF}

implementation

{$IFDEF GTK1}
uses
  x, xlib, xutil;
{$ENDIF}

const
  GtkPositionTypeMap: array[TTabPosition] of TGtkPositionType =
  (
{ tpTop    } GTK_POS_TOP,
{ tpBottom } GTK_POS_BOTTOM,
{ tpLeft   } GTK_POS_LEFT,
{ tpRight  } GTK_POS_RIGHT
  );
  
  LCL_NotebookManualPageSwitchKey = 'lcl_manual_page_switch';

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

{ TGtkWSCustomNotebook }

function NotebookPageRealToLCLIndex(const ANotebook: TCustomNotebook; AIndex: integer): integer;
var
  I: Integer;
begin
  Result := AIndex;
  if csDesigning in ANotebook.ComponentState then exit;
  I := 0;
  while (I < ANotebook.PageCount) and (I <= Result) do
  begin
    if not ANotebook.Page[I].TabVisible then Inc(Result);
    Inc(I);
  end;
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
    NMHdr.idFrom := NotebookPageRealToLCLIndex(TCustomNotebook(Data), pagenum);  //use this to set pageindex to the correct page.
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
  NMHdr.idFrom := NotebookPageRealToLCLIndex(TCustomNotebook(Data), pagenum);  //use this to set pageindex to the correct page.
  Mess.NMHdr := @NMHdr;
  DeliverMessage(Data, Mess);
end;

class procedure TGtkWSCustomNotebook.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtkWSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));
  ConnectSignal(PGtkObject(AGtkWidget), 'switch_page', @GtkWSNotebook_SwitchPage, AWidgetInfo^.LCLObject);
end;

class function TGtkWSCustomNotebook.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  AWidget: PGtkNoteBook;
  WidgetInfo: PWidgetInfo;
begin
  AWidget := PGtkNoteBook(gtk_notebook_new());
  WidgetInfo := CreateWidgetInfo(AWidget, AWinControl, AParams);
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Pointer(AWidget), dbgsName(AWinControl));
  {$ENDIF}
  gtk_notebook_set_scrollable(AWidget, True);

  if not (nboHidePageListPopup in TCustomNotebook(AWinControl).Options) then
    gtk_notebook_popup_enable(AWidget);

  if TCustomNotebook(AWinControl).PageCount=0 then
    // a gtk notebook needs a page -> add dummy page
    GTKWidgetSet.AddDummyNoteBookPage(AWidget);

  gtk_notebook_set_tab_pos(AWidget, GtkPositionTypeMap[TCustomNotebook(AWinControl).TabPosition]);
  Result := TLCLIntfHandle(PtrUInt(AWidget));
  Set_RC_Name(AWinControl, PGtkWidget(AWidget));
  SetCallBacks(PGtkWidget(AWidget), WidgetInfo);
end;

class procedure TGtkWSCustomNotebook.AddPage(const ANotebook: TCustomNotebook;
  const AChild: TCustomPage; const AIndex: integer);
{
  Inserts a new page to a notebook at position Index. The ANotebook is a
  TCustomNoteBook, the AChild one of its TCustomPage. Both handles must already
  be created. ANoteBook Handle is a PGtkNoteBook and APage handle is a
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
  DebugLn(['TGtkWSCustomNotebook.AddPage ',dbgsName(ANoteBook),' ',ANotebook.HandleAllocated,' AChild=',dbgsName(AChild),' ',AChild.HandleAllocated,' Child.TabVisible=',AChild.TabVisible]);
  {$ENDIF}
  NoteBookWidget := PGtkWidget(ANoteBook.Handle);
  PageWidget := PGtkWidget(AChild.Handle);

  // set LCL size
  AChild.SetBounds(AChild.Left, AChild.Top, ANotebook.ClientWidth, ANotebook.ClientHeight);

  if AChild.TabVisible then
    gtk_widget_show(PageWidget);

  // Check if already created. if so just show it because it is invisible
  if gtk_notebook_get_tab_label(PGtkNoteBook(NoteBookWidget), PageWidget) <> nil
  then begin
    {$IFDEF NOTEBOOK_DEBUG}
    DebugLn(['TGtkWSCustomNotebook.AddPage already added']);
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

  UpdateNotebookPageTab(ANoteBook, AChild);
  UpdateNoteBookClientWidget(ANoteBook);
  
  // init the size of the page widget
  //DebugLn(['TGtkWSCustomNotebook.AddPage ',DbgSName(ANoteBook),' ',dbgs(ANoteBook.BoundsRect)]);
  {$IFDEF VerboseSizeMsg}
  DebugLn(['TGtkWSCustomNotebook.AddPage PageWidget^.allocation=',dbgs(PageWidget^.allocation),' NotebookWidget=',dbgs(NotebookWidget^.allocation)]);
  {$ENDIF}
end;

class procedure TGtkWSCustomNotebook.MovePage(const ANotebook: TCustomNotebook;
  const AChild: TCustomPage; const NewIndex: integer);
var
  NoteBookWidget: PGtkNotebook;
begin
  NoteBookWidget:=PGtkNotebook(ANoteBook.Handle);
  gtk_notebook_reorder_child(NoteBookWidget, PGtkWidget(AChild.Handle), NewIndex);
  UpdateNoteBookClientWidget(ANoteBook);
end;

class procedure TGtkWSCustomNotebook.RemovePage(const ANotebook: TCustomNotebook;
  const AIndex: integer);
var
  PageWidget: PGtkWidget;
  Page: TCustomPage;
begin
  // The gtk does not provide a function to remove a page without destroying it.
  // Luckily the LCL destroys the Handle, when a page is removed, so this
  // function is not needed.
  {$IFDEF NOTEBOOK_DEBUG}
  DebugLn(['TGtkWSCustomNotebook.RemovePage AIndex=',AIndex,' ',DbgSName(ANotebook.Page[AIndex])]);
  {$ENDIF}
  Page:=ANotebook.Page[AIndex];
  if not Page.HandleAllocated then exit;
  PageWidget := PGtkWidget(Page.Handle);
  gtk_widget_hide(PageWidget);
end;

class function TGtkWSCustomNotebook.GetCapabilities: TNoteBookCapabilities;
begin
  Result:=[nbcPageListPopup, nbcShowCloseButtons];
end;

class function TGtkWSCustomNotebook.GetNotebookMinTabHeight(
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
  //debugln('TGtkWSCustomNotebook.GetNotebookMinTabHeight A ',dbgs(Result));
  exit;

  debugln('TGtkWSCustomNotebook.GetNotebookMinTabHeight A ',dbgs(AWinControl.HandleAllocated));
  if AWinControl.HandleAllocated then
    NBWidget:=PGTKWidget(AWinControl.Handle)
  else
    NBWidget:=GetStyleWidget(lgsNotebook);

  // ToDo: find out how to create a fully working hidden Notebook style widget

  if (NBWidget=nil) then begin
    Result:=TWSCustomNotebook.GetNotebookMinTabHeight(AWinControl);
    exit;
  end;
  debugln('TGtkWSCustomNotebook.GetNotebookMinTabHeight NBWidget: ',GetWidgetDebugReport(NBWidget),
   ' ',dbgs(NBWidget^.allocation.width),'x',dbgs(NBWidget^.allocation.height));
  
  BorderWidth:=(PGtkContainer(NBWidget)^.flag0 and bm_TGtkContainer_border_width)
               shr bp_TGtkContainer_border_width;
  if PGtkNoteBook(NBWidget)^.first_tab<>nil then
    Page:=PGtkNoteBook(NBWidget)^.cur_page;

  Result:=BorderWidth;
  {$IFDEF GTK2}
  if (Page<>nil) then begin
    debugln('TGtkWSCustomNotebook.RemovePage TODO');
  end;
  {$ELSE GTK2}
  if (NBWidget^.thestyle<>nil) and (PGtkStyle(NBWidget^.thestyle)^.klass<>nil) then
    inc(Result,PGtkStyle(NBWidget^.thestyle)^.klass^.ythickness);
  if (Page<>nil) and (Page^.child<>nil) then begin
    gtk_widget_size_request(Page^.Child, @Requisition);
    gtk_widget_map(Page^.child);
    debugln('TGtkWSCustomNotebook.GetNotebookMinTabHeight B ',dbgs(Page^.child^.allocation.height),
      ' ',GetWidgetDebugReport(Page^.child),' Requisition=',dbgs(Requisition.height));
    inc(Result,Page^.child^.allocation.height);
  end;
  {$ENDIF GTK2}
  debugln('TGtkWSCustomNotebook.GetNotebookMinTabHeight END ',dbgs(Result),' ',
    GetWidgetDebugReport(NBWidget));
end;

class function TGtkWSCustomNotebook.GetNotebookMinTabWidth(
  const AWinControl: TWinControl): integer;
begin
  Result:=TWSCustomNotebook.GetNotebookMinTabWidth(AWinControl);
end;

class function TGtkWSCustomNotebook.GetTabIndexAtPos(
  const ANotebook: TCustomNotebook; const AClientPos: TPoint): integer;
var
  NoteBookWidget: PGtkNotebook;
  i: integer;
  TabWidget: PGtkWidget;
  PageWidget: PGtkWidget;
  NotebookPos: TPoint;
  {$IFDEF GTK2}
  Window: PGdkWindow;
  WindowOrg,ClientOrg: TPoint;
  {$ENDIF}
  Count: guint;
begin
  Result:=-1;
  NoteBookWidget:=PGtkNotebook(ANotebook.Handle);
  if (NotebookWidget=nil) then exit;
  //DebugLn(['TGtkWSCustomNotebook.GetTabIndexAtPos ',GetWidgetDebugReport(PGtkWidget(NotebookWidget))]);
  {$IFDEF GTK2}
  Window := GetControlWindow(NoteBookWidget);
  gdk_window_get_origin(Window,@WindowOrg.X,@WindowOrg.Y);
  ClientOrg:=GetWidgetClientOrigin(PGtkWidget(NotebookWidget));
  NotebookPos.X:= AClientPos.X + (ClientOrg.X-WindowOrg.X);
  NotebookPos.Y:= AClientPos.Y + (ClientOrg.Y-WindowOrg.Y);
  {$ELSE}
  NotebookPos:=AClientPos;
  {$ENDIF}
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

class function TGtkWSCustomNotebook.GetTabRect(const ANotebook: TCustomNotebook;
  const AIndex: Integer): TRect;
var
  NoteBookWidget: PGtkNotebook;
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
  NoteBookWidget:=PGtkNotebook(ANotebook.Handle);
  if (NotebookWidget=nil) then exit;
  //DebugLn(['TGtkWSCustomNotebook.GetTabIndexAtPos ',GetWidgetDebugReport(PGtkWidget(NotebookWidget))]);
  {$IFDEF GTK2}
  Window := GetControlWindow(NoteBookWidget);
  gdk_window_get_origin(Window,@WindowOrg.X,@WindowOrg.Y);
  ClientOrg:=GetWidgetClientOrigin(PGtkWidget(NotebookWidget));
  XOffset := (ClientOrg.X-WindowOrg.X);
  YOffset := (ClientOrg.Y-WindowOrg.Y);
  {$ELSE}
  XOffset := 0;
  YOffset := 0;
  {$ENDIF}
  // go through all tabs
  Count:=g_list_length(NoteBookWidget^.Children);
  PageWidget:=gtk_notebook_get_nth_page(NoteBookWidget, AIndex);
  if (PageWidget<>nil) and (AIndex < Count) then begin
    TabWidget:=gtk_notebook_get_tab_label(NoteBookWidget, PageWidget);
    if TabWidget<>nil then begin
      Result.Top := TabWidget^.Allocation.Y - YOffset;
      Result.Bottom := TabWidget^.Allocation.Y - YOffset + TabWidget^.Allocation.Height;
      Result.Left := TabWidget^.Allocation.X - XOffset;
      Result.right := TabWidget^.Allocation.X - XOffset + TabWidget^.Allocation.Width;
      exit;
    end;
  end;
end;

class procedure TGtkWSCustomNotebook.SetPageIndex(
  const ANotebook: TCustomNotebook; const AIndex: integer);
var
  GtkNotebook: PGtkNotebook;
begin
  if not WSCheckHandleAllocated(ANotebook, 'SetPageIndex') then
    Exit;
    
  GtkNotebook := PGtkNoteBook(ANotebook.Handle);
  if gtk_notebook_get_current_page(GtkNotebook) <> AIndex then
  begin
    gtk_object_set_data(PGtkObject(GtkNotebook), LCL_NotebookManualPageSwitchKey, ANotebook);
    gtk_notebook_set_page(GtkNotebook, AIndex);
  end;
  UpdateNoteBookClientWidget(ANotebook);
end;

class procedure TGtkWSCustomNotebook.SetTabPosition(
  const ANotebook: TCustomNotebook; const ATabPosition: TTabPosition);
begin
  gtk_notebook_set_tab_pos(PGtkNotebook(ANotebook.Handle),
    GtkPositionTypeMap[ATabPosition]);
end;

class procedure TGtkWSCustomNotebook.ShowTabs(const ANotebook: TCustomNotebook;
  AShowTabs: boolean);
begin
  gtk_notebook_set_show_tabs(PGtkNotebook(ANotebook.Handle), AShowTabs);
end;

class procedure TGtkWSCustomNotebook.UpdateProperties(const ANotebook: TCustomNotebook);
begin
  if (nboHidePageListPopup in ANoteBook.Options) then
    gtk_notebook_popup_disable(PGtkNotebook(ANoteBook.Handle))
  else
    gtk_notebook_popup_enable(PGtkNotebook(ANoteBook.Handle));
end;

class procedure TGtkWSCustomPanel.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtkWSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));
end;

class function TGtkWSCustomPanel.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  Allocation: TGTKAllocation;
begin
  Widget := TGtkWidgetset(Widgetset).CreateAPIWidget(AWinControl);
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget, dbgsName(AWinControl));
  {$ENDIF}

  Result := THandle(PtrUInt(Widget));
  if Result = 0 then Exit;

  WidgetInfo := GetWidgetInfo(Widget); // Widget info already created in CreateAPIWidget
  WidgetInfo^.Style := AParams.Style;
  WidgetInfo^.ExStyle := AParams.ExStyle;
  WidgetInfo^.WndProc := PtrUInt(AParams.WindowClass.lpfnWndProc);
  Include(WidgetInfo^.Flags, wwiNoEraseBkgnd);

  // set allocation
  Allocation.X := AParams.X;
  Allocation.Y := AParams.Y;
  Allocation.Width := AParams.Width;
  Allocation.Height := AParams.Height;
  gtk_widget_size_allocate(Widget, @Allocation);

  Set_RC_Name(AWinControl, Widget);
  SetCallbacks(Widget, WidgetInfo);
end;

class procedure TGtkWSCustomPanel.SetColor(const AWinControl: TWinControl);
var
  MainWidget: PGtkWidget;
begin
  if not AWinControl.HandleAllocated then exit;
  MainWidget:=GetFixedWidget(pGtkWidget(AWinControl.handle));
  if MainWidget<>nil then
  GtkWidgetSet.SetWidgetColor(MainWidget,
                              AWinControl.font.color, AWinControl.color,
                              [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,
                               GTK_STATE_PRELIGHT,GTK_STATE_SELECTED]);

  UpdateWidgetStyleOfControl(AWinControl);
end;

{$IFDEF GTK1}
  {$include gtk1trayicon.inc}
{$ENDIF}

end.
