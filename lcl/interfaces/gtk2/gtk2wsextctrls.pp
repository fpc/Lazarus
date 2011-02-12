{ $Id$}
{
 *****************************************************************************
 *                             Gtk2WSExtCtrls.pp                             * 
 *                             -----------------                             * 
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
unit Gtk2WSExtCtrls;

{$I gtk2defines.inc}
//{$define UseStatusIcon} // can be used only with fpc r13008


{$mode objfpc}{$H+}

interface

uses
  // libs
  Math, GLib2, Gtk2, Gdk2, Gdk2Pixbuf, Gtk2Int, Gtk2Def, {$ifdef UseStatusIcon}Gtk2Ext, {$endif}
  // LCL
  LCLProc, ExtCtrls, Classes, Controls, SysUtils, Graphics, LCLType, LMessages,
  // widgetset
  WSExtCtrls, WSLCLClasses, WSProc,
  Gtk2WSControls, Gtk2WSPrivate, Gtk2Proc, Gtk2Globals;

type

  { TGtk2WSCustomPage }

  TGtk2WSCustomPage = class(TWSCustomPage)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure UpdateProperties(const ACustomPage: TCustomPage); override;
    class procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); override;
    class procedure ShowHide(const AWinControl: TWinControl); override;
    class function GetDefaultClientRect(const AWinControl: TWinControl;
             const aLeft, aTop, aWidth, aHeight: integer; var aClientRect: TRect
             ): boolean; override;
  end;

  { TGtk2WSCustomNotebook }
  
  TGtk2WSCustomNotebook = class(TWSCustomNotebook)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function CreateHandle(const AWinControl: TWinControl;
                                const AParams: TCreateParams): HWND; override;
    class function GetDefaultClientRect(const AWinControl: TWinControl;
             const aLeft, aTop, aWidth, aHeight: integer; var aClientRect: TRect
             ): boolean; override;
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

  { TGtk2WSPage }

  TGtk2WSPage = class(TWSPage)
  published
  end;

  { TGtk2WSNotebook }

  TGtk2WSNotebook = class(TWSNotebook)
  published
  end;

  { TGtk2WSShape }

  TGtk2WSShape = class(TWSShape)
  published
  end;

  { TGtk2WSCustomSplitter }

  TGtk2WSCustomSplitter = class(TWSCustomSplitter)
  published
  end;

  { TGtk2WSSplitter }

  TGtk2WSSplitter = class(TWSSplitter)
  published
  end;

  { TGtk2WSPaintBox }

  TGtk2WSPaintBox = class(TWSPaintBox)
  published
  end;

  { TGtk2WSCustomImage }

  TGtk2WSCustomImage = class(TWSCustomImage)
  published
  end;

  { TGtk2WSImage }

  TGtk2WSImage = class(TWSImage)
  published
  end;

  { TGtk2WSBevel }

  TGtk2WSBevel = class(TWSBevel)
  published
  end;

  { TGtk2WSCustomRadioGroup }

  TGtk2WSCustomRadioGroup = class(TWSCustomRadioGroup)
  published
  end;

  { TGtk2WSRadioGroup }

  TGtk2WSRadioGroup = class(TWSRadioGroup)
  published
  end;

  { TGtk2WSCustomCheckGroup }

  TGtk2WSCustomCheckGroup = class(TWSCustomCheckGroup)
  published
  end;

  { TGtk2WSCheckGroup }

  TGtk2WSCheckGroup = class(TWSCheckGroup)
  published
  end;

  { TGtk2WSBoundLabel }

  {TGtk2WSBoundLabel = class(TWSBoundLabel)
  private
  protected
  public
  end;}

  { TGtk2WSCustomLabeledEdit }

  TGtk2WSCustomLabeledEdit = class(TWSCustomLabeledEdit)
  published
  end;

  { TGtk2WSLabeledEdit }

  TGtk2WSLabeledEdit = class(TWSLabeledEdit)
  published
  end;

  { TGtk2WSCustomPanel }

  TGtk2WSCustomPanel = class(TWSCustomPanel)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetColor(const AWinControl: TWinControl); override;
  end;

  { TGtk2WSPanel }

  TGtk2WSPanel = class(TWSPanel)
  published
  end;

  { TGtk2WSCustomTrayIcon }

  TGtk2WSCustomTrayIcon = class(TWSCustomTrayIcon)
  published
    class function Hide(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class function Show(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class procedure InternalUpdate(const ATrayIcon: TCustomTrayIcon); override;
    class function GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint; override;
  end;

implementation

uses
{$ifdef HasX}
  x, xlib, xutil,
{$endif}
//  gtk2, gdk2, glib2, gtk2def, gtk2proc,
{$ifdef HasGdk2X}
  gdk2x,
{$endif}
  interfacebase;

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

{ TGtk2WSCustomNotebook }


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

class procedure TGtk2WSCustomNotebook.SetCallbacks(
  const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
begin
  TGtk2WSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));
  ConnectSignal(PGtkObject(AGtkWidget), 'switch_page', @GtkWSNotebook_SwitchPage, AWidgetInfo^.LCLObject);
end;

class function TGtk2WSCustomNotebook.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  AWidget: PGtkNoteBook;
  WidgetInfo: PWidgetInfo;
begin
  if OldNoteBookButtonPress = nil then
    HookNoteBookClass;
  //DebugLn(['TGtk2WSCustomNotebook.CreateHandle ',DbgSName(AWinControl)]);

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
    Gtk2WidgetSet.AddDummyNoteBookPage(AWidget);

  gtk_notebook_set_tab_pos(AWidget, GtkPositionTypeMap[TCustomNotebook(AWinControl).TabPosition]);
  Result := HWND(TLCLIntfHandle(PtrUInt(AWidget)));
  Set_RC_Name(AWinControl, PGtkWidget(AWidget));
  SetCallBacks(PGtkWidget(AWidget), WidgetInfo);
end;

class function TGtk2WSCustomNotebook.GetDefaultClientRect(
  const AWinControl: TWinControl; const aLeft, aTop, aWidth, aHeight: integer;
  var aClientRect: TRect): boolean;
var
  FrameBorders: TRect;
begin
  Result:=false;
  //DebugLn(['TGtk2WSCustomNotebook.GetDefaultClientRect ',DbgSName(AWinControl),' ',aWidth,'x',aHeight]);
  if AWinControl.HandleAllocated then begin

  end else begin
    FrameBorders:=GetStyleNotebookFrameBorders;
    aClientRect:=Rect(0,0,
                 Max(0,aWidth-FrameBorders.Left-FrameBorders.Right),
                 Max(0,aHeight-FrameBorders.Top-FrameBorders.Bottom));
    Result:=true;
  end;
  {$IFDEF VerboseSizeMsg}
  if Result then DebugLn(['TGtk2WSCustomNotebook.GetDefaultClientRect END FrameBorders=',dbgs(FrameBorders),' aClientRect=',dbgs(aClientRect)]);
  {$ENDIF}
end;

class procedure TGtk2WSCustomNotebook.AddPage(const ANotebook: TCustomNotebook;
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

class procedure TGtk2WSCustomNotebook.MovePage(const ANotebook: TCustomNotebook;
  const AChild: TCustomPage; const NewIndex: integer);
var
  NoteBookWidget: PGtkNotebook;
begin
  NoteBookWidget:=PGtkNotebook(ANoteBook.Handle);
  gtk_notebook_reorder_child(NoteBookWidget, PGtkWidget(AChild.Handle), NewIndex);
  UpdateNoteBookClientWidget(ANoteBook);
end;

class procedure TGtk2WSCustomNotebook.RemovePage(const ANotebook: TCustomNotebook;
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

class function TGtk2WSCustomNotebook.GetCapabilities: TNoteBookCapabilities;
begin
  Result:=[nbcPageListPopup, nbcShowCloseButtons];
end;

class function TGtk2WSCustomNotebook.GetNotebookMinTabHeight(
  const AWinControl: TWinControl): integer;
var
  NBWidget: PGTKWidget;
  BorderWidth: Integer;
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
  if (Page<>nil) then begin
    debugln('TGtkWSCustomNotebook.RemovePage TODO');
  end;
  debugln('TGtkWSCustomNotebook.GetNotebookMinTabHeight END ',dbgs(Result),' ',
    GetWidgetDebugReport(NBWidget));
end;

class function TGtk2WSCustomNotebook.GetNotebookMinTabWidth(
  const AWinControl: TWinControl): integer;
begin
  Result:=TWSCustomNotebook.GetNotebookMinTabWidth(AWinControl);
end;

class function TGtk2WSCustomNotebook.GetTabIndexAtPos(
  const ANotebook: TCustomNotebook; const AClientPos: TPoint): integer;
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
  NoteBookWidget:=PGtkNotebook(ANotebook.Handle);
  if (NotebookWidget=nil) then exit;
  //DebugLn(['TGtkWSCustomNotebook.GetTabIndexAtPos ',GetWidgetDebugReport(PGtkWidget(NotebookWidget))]);
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

class function TGtk2WSCustomNotebook.GetTabRect(const ANotebook: TCustomNotebook;
  const AIndex: Integer): TRect;
var
  NoteBookWidget: PGtkNotebook;
  TabWidget: PGtkWidget;
  PageWidget: PGtkWidget;
  Count: guint;
begin
  Result := inherited;
  NoteBookWidget:=PGtkNotebook(ANotebook.Handle);
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

class procedure TGtk2WSCustomNotebook.SetPageIndex(
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

class procedure TGtk2WSCustomNotebook.SetTabPosition(
  const ANotebook: TCustomNotebook; const ATabPosition: TTabPosition);
begin
  gtk_notebook_set_tab_pos(PGtkNotebook(ANotebook.Handle),
    GtkPositionTypeMap[ATabPosition]);
end;

class procedure TGtk2WSCustomNotebook.ShowTabs(const ANotebook: TCustomNotebook;
  AShowTabs: boolean);
begin
  gtk_notebook_set_show_tabs(PGtkNotebook(ANotebook.Handle), AShowTabs);
end;

class procedure TGtk2WSCustomNotebook.UpdateProperties(const ANotebook: TCustomNotebook);
begin
  if (nboHidePageListPopup in ANoteBook.Options) then
    gtk_notebook_popup_disable(PGtkNotebook(ANoteBook.Handle))
  else
    gtk_notebook_popup_enable(PGtkNotebook(ANoteBook.Handle));
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

{ TGtk2WSCustomPanel }

class procedure TGtk2WSCustomPanel.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtk2WSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));
end;

class function TGtk2WSCustomPanel.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  Allocation: TGTKAllocation;
begin
  Widget := TGtk2Widgetset(Widgetset).CreateAPIWidget(AWinControl);
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget, dbgsName(AWinControl));
  {$ENDIF}

  Result := THandle(PtrUInt(Widget));
  if Result = 0 then Exit;

  WidgetInfo := GetWidgetInfo(Widget); // Widget info already created in CreateAPIWidget
  WidgetInfo^.Style := AParams.Style;
  WidgetInfo^.ExStyle := AParams.ExStyle;
  WidgetInfo^.WndProc := PtrUInt(AParams.WindowClass.lpfnWndProc);

  // set allocation
  Allocation.X := AParams.X;
  Allocation.Y := AParams.Y;
  Allocation.Width := AParams.Width;
  Allocation.Height := AParams.Height;
  gtk_widget_size_allocate(Widget, @Allocation);

  Set_RC_Name(AWinControl, Widget);
  SetCallbacks(Widget, WidgetInfo);
end;

class procedure TGtk2WSCustomPanel.SetColor(const AWinControl: TWinControl);
var
  MainWidget: PGtkWidget;
  FontColor, BGColor: TColor;
begin
  if not AWinControl.HandleAllocated then exit;
  MainWidget:=GetFixedWidget(pGtkWidget(AWinControl.handle));
  if MainWidget<>nil then
  Gtk2WidgetSet.SetWidgetColor(MainWidget,
                              AWinControl.Font.Color, AWinControl.Color,
                              [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,
                               GTK_STATE_PRELIGHT,GTK_STATE_SELECTED]);

  UpdateWidgetStyleOfControl(AWinControl);
end;


{$include gtk2trayicon.inc}

end.
