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
 *  See the file COPYING.LCL, included in this distribution,                 *
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
  LCLProc, Controls,
{$IFDEF GTK2}
  gtk2, gdk2,
{$ELSE GTK2}
  gtk, gdk,
{$ENDIF GTK2}
  GtkGlobals, GtkProc, ExtCtrls, Classes,
  WSExtCtrls, WSLCLClasses, gtkint, interfacebase;

type

  { TGtkWSCustomPage }

  TGtkWSCustomPage = class(TWSCustomPage)
  private
  protected
  public
    class procedure UpdateProperties(const ACustomPage: TCustomPage); override;
  end;

  { TGtkWSCustomNotebook }

  TGtkWSCustomNotebook = class(TWSCustomNotebook)
  private
  protected
  public
    class procedure AddPage(const ANotebook: TCustomNotebook; 
      const AChild: TCustomPage; const AIndex: integer); override;
    class procedure MovePage(const ANotebook: TCustomNotebook; 
      const AChild: TCustomPage; const NewIndex: integer); override;
    class procedure RemovePage(const ANotebook: TCustomNotebook; 
      const AIndex: integer); override;
    
    class function GetNotebookMinTabHeight(const AWinControl: TWinControl): integer; override;
    class function GetNotebookMinTabWidth(const AWinControl: TWinControl): integer; override;
    class procedure SetPageIndex(const ANotebook: TCustomNotebook; const AIndex: integer); override;
    class procedure SetTabPosition(const ANotebook: TCustomNotebook; const ATabPosition: TTabPosition); override;
    class procedure ShowTabs(const ANotebook: TCustomNotebook; AShowTabs: boolean); override;
  end;

  { TGtkWSPage }

  TGtkWSPage = class(TWSPage)
  private
  protected
  public
  end;

  { TGtkWSNotebook }

  TGtkWSNotebook = class(TWSNotebook)
  private
  protected
  public
  end;

  { TGtkWSShape }

  TGtkWSShape = class(TWSShape)
  private
  protected
  public
  end;

  { TGtkWSCustomSplitter }

  TGtkWSCustomSplitter = class(TWSCustomSplitter)
  private
  protected
  public
  end;

  { TGtkWSSplitter }

  TGtkWSSplitter = class(TWSSplitter)
  private
  protected
  public
  end;

  { TGtkWSPaintBox }

  TGtkWSPaintBox = class(TWSPaintBox)
  private
  protected
  public
  end;

  { TGtkWSCustomImage }

  TGtkWSCustomImage = class(TWSCustomImage)
  private
  protected
  public
  end;

  { TGtkWSImage }

  TGtkWSImage = class(TWSImage)
  private
  protected
  public
  end;

  { TGtkWSBevel }

  TGtkWSBevel = class(TWSBevel)
  private
  protected
  public
  end;

  { TGtkWSCustomRadioGroup }

  TGtkWSCustomRadioGroup = class(TWSCustomRadioGroup)
  private
  protected
  public
  end;

  { TGtkWSRadioGroup }

  TGtkWSRadioGroup = class(TWSRadioGroup)
  private
  protected
  public
  end;

  { TGtkWSCustomCheckGroup }

  TGtkWSCustomCheckGroup = class(TWSCustomCheckGroup)
  private
  protected
  public
  end;

  { TGtkWSCheckGroup }

  TGtkWSCheckGroup = class(TWSCheckGroup)
  private
  protected
  public
  end;

  { TGtkWSBoundLabel }

  TGtkWSBoundLabel = class(TWSBoundLabel)
  private
  protected
  public
  end;

  { TGtkWSCustomLabeledEdit }

  TGtkWSCustomLabeledEdit = class(TWSCustomLabeledEdit)
  private
  protected
  public
  end;

  { TGtkWSLabeledEdit }

  TGtkWSLabeledEdit = class(TWSLabeledEdit)
  private
  protected
  public
  end;

  { TGtkWSCustomPanel }

  TGtkWSCustomPanel = class(TWSCustomPanel)
  private
  protected
  public
  end;

  { TGtkWSPanel }

  TGtkWSPanel = class(TWSPanel)
  private
  protected
  public
  end;


implementation

var
  {$IFDef GTK1}
  NoteBookCloseBtnPixmapImg: PGdkPixmap = nil;
  NoteBookCloseBtnPixmapMask: PGdkPixmap = nil;
  {$Else}
  NoteBookCloseBtnPixbuf: PGdkPixbuf = nil;
  {$EndIf}

{-------------------------------------------------------------------------------
  procedure RemoveDummyNoteBookPage(NoteBookWidget: PGtkNotebook);

  Removes the dummy page.
  See also AddDummyNoteBookPage
-------------------------------------------------------------------------------}
procedure RemoveDummyNoteBookPage(NoteBookWidget: PGtkNotebook);
var
  DummyWidget: PGtkWidget;
begin
  DummyWidget:=GetGtkNoteBookDummyPage(NoteBookWidget);
  if DummyWidget=nil then exit;
  gtk_notebook_remove_page(NoteBookWidget,
                           gtk_notebook_page_num(NoteBookWidget,DummyWidget));
  DummyWidget:=nil;
  SetGtkNoteBookDummyPage(NoteBookWidget,DummyWidget);
end;

{-------------------------------------------------------------------------------
  method GetNoteBookCloseBtnImage
  Params:
  Result: none

  Loads the image for the close button in the tabs of the TCustomNoteBook(s).
-------------------------------------------------------------------------------}
{$IfDef GTK1}
procedure GetNoteBookCloseBtnImage(Window: PGdkWindow;
  var Img, Mask: PGdkPixmap);
begin
  if (NoteBookCloseBtnPixmapImg=nil)
  and (Window<>nil) then begin
    LoadXPMFromLazResource('tnotebook_close_tab',Window,
                        NoteBookCloseBtnPixmapImg,NoteBookCloseBtnPixmapMask);
  end;
  Img:=NoteBookCloseBtnPixmapImg;
  Mask:=NoteBookCloseBtnPixmapMask;
end;
{$Else}
procedure GetNoteBookCloseBtnImage(var Img: PGdkPixbuf);
begin
  if (NoteBookCloseBtnPixbuf=nil) then
    LoadPixbufFromLazResource('tnotebook_close_tab', NoteBookCloseBtnPixbuf);
  Img:=NoteBookCloseBtnPixbuf;
end;
{$EndIF}

{-------------------------------------------------------------------------------
  method UpdateNotebookPageTab
  Params: ANoteBook: TCustomNotebook; APage: TCustomPage
  Result: none

  Updates the tab of a page of a notebook. This contains the image to the left
  side, the label, the close button, the menu image and the menu label.
-------------------------------------------------------------------------------}
procedure UpdateNotebookPageTab(ANoteBook, APage: TObject);
var
  TheNoteBook: TCustomNotebook;
  ThePage: TCustomPage;

  NoteBookWidget: PGtkWidget;  // the notebook
  PageWidget: PGtkWidget;      // the page (content widget)
  TabWidget: PGtkWidget;       // the tab (hbox containing a pixmap, a label
                               //          and a close button)
  TabImageWidget: PGtkWidget;  // the icon widget in the tab (a fixed widget)
  TabLabelWidget: PGtkWidget;  // the label in the tab
  TabCloseBtnWidget: PGtkWidget;// the close button in the tab
  TabCloseBtnImageWidget: PGtkWidget; // the pixmap in the close button
  MenuWidget: PGtkWidget;      // the popup menu (hbox containing a pixmap and
                               // a label)
  MenuImageWidget: PGtkWidget; // the icon widget in the popup menu item (a fixed widget)
  MenuLabelWidget: PGtkWidget; // the label in the popup menu item

  procedure UpdateTabImage;
  var
    HasIcon: Boolean;
    IconSize: TPoint;
  begin
    HasIcon:=false;
    IconSize:=Point(0,0);
    if (TheNoteBook.Images<>nil)
    and (ThePage.ImageIndex>=0)
    and (ThePage.ImageIndex<TheNoteBook.Images.Count) then begin
      // page has valid image
      IconSize:=Point(TheNoteBook.Images.Width,TheNoteBook.Images.Height);
      HasIcon:=(IconSize.X>0) and (IconSize.Y>0);
    end;

    if HasIcon then begin
      // page has an image
      if TabImageWidget<>nil then begin
        // there is already an icon widget for the image in the tab
        // -> resize the icon widget
        gtk_widget_set_usize(TabImageWidget,IconSize.X,IconSize.Y);
      end else begin
        // there is no pixmap for the image in the tab
        // -> insert one ot the left side of the label
        TabImageWidget:= gtk_label_new(#0);
        g_signal_connect(PgtkObject(TabImageWidget), 'expose_event',
                           TGTKSignalFunc(@PageIconWidgetExposeAfter), ThePage);
        {$IFNDEF GTK2}
        g_signal_connect(PgtkObject(TabImageWidget), 'draw',
                             TGTKSignalFunc(@PageIconWidgetDrawAfter), ThePage);
        {$ENDIF}
        gtk_object_set_data(PGtkObject(TabWidget),'TabImage',TabImageWidget);
        gtk_widget_set_usize(TabImageWidget,IconSize.X,IconSize.Y);
        gtk_widget_show(TabImageWidget);
        gtk_box_pack_start_defaults(PGtkBox(TabWidget),TabImageWidget);
        gtk_box_reorder_child(PGtkBox(TabWidget),TabImageWidget,0);
      end;
      if MenuImageWidget<>nil then begin
        // there is already an icon widget for the image in the menu
        // -> resize the icon widget
        gtk_widget_set_usize(MenuImageWidget,IconSize.X,IconSize.Y);
      end else begin
        // there is no icon widget for the image in the menu
        // -> insert one at the left side of the label
        MenuImageWidget:=gtk_label_new(#0);
        g_signal_connect_after(PgtkObject(MenuImageWidget), 'expose_event',
                          TGTKSignalFunc(@PageIconWidgetExposeAfter), ThePage);
        {$IFNDEF GTK2}
        g_signal_connect_after(PgtkObject(MenuImageWidget), 'draw',
                             TGTKSignalFunc(@PageIconWidgetDrawAfter), ThePage);
        {$ENDIF}
        gtk_widget_set_usize(MenuImageWidget,IconSize.X,IconSize.Y);
        gtk_object_set_data(PGtkObject(MenuWidget),'TabImage',MenuImageWidget);
        gtk_widget_show(MenuImageWidget);
        gtk_box_pack_start_defaults(PGtkBox(MenuWidget),MenuImageWidget);
        gtk_box_reorder_child(PGtkBox(MenuWidget),MenuImageWidget,0);
      end;
    end else begin
      // page does not have an image
      if TabImageWidget<>nil then begin
        // there is a pixmap for an old image in the tab
        // -> remove the icon widget
        DestroyWidget(TabImageWidget);
        gtk_object_set_data(PGtkObject(TabWidget), 'TabImage', nil);
        TabImageWidget:=nil;
      end;
      if MenuImageWidget<>nil then begin
        // there is a pixmap for an old image in the menu
        // -> remove the icon widget
        DestroyWidget(MenuImageWidget);
        gtk_object_set_data(PGtkObject(MenuWidget), 'TabImage', nil);
        MenuImageWidget:=nil;
      end;
    end;
  end;

  procedure UpdateTabLabel;
  var TheCaption: PChar;
  begin
    TheCaption:=PChar(ThePage.Caption);
    if TheCaption=nil then
      TheCaption:=#0;
    gtk_label_set_text(PGtkLabel(TabLabelWidget),TheCaption);
    if MenuLabelWidget<>nil then
      gtk_label_set_text(PGtkLabel(MenuLabelWidget),TheCaption);
  end;

  procedure UpdateTabCloseBtn;
  var
    {$IfDef GTK1}
    Img: PGdkPixmap;
    Mask: PGdkBitmap;
    {$Else}
    Img: PGdkPixbuf;
    {$EndIf}
  begin
    {$IfDef GTK1}
    GetNoteBookCloseBtnImage(GetControlWindow(NoteBookWidget),Img,Mask);
    {$Else}
    GetNoteBookCloseBtnImage(Img);
    {$EndIf}
    if (nboShowCloseButtons in TheNotebook.Options) and (Img<>nil) then begin
      // close buttons enabled
      if TabCloseBtnWidget=nil then begin
        // there is no close button yet
        // -> add one to the right side of the label in the tab
        TabCloseBtnWidget:=gtk_button_new;
        gtk_object_set_data(PGtkObject(TabWidget), 'TabCloseBtn',
                            TabCloseBtnWidget);
        begin
          // put a pixmap into the button
         {$IfDef GTK1}
          TabCloseBtnImageWidget:=gtk_pixmap_new(Img,Mask);
         {$Else}
          TabCloseBtnImageWidget:=gtk_image_new_from_pixbuf(Img);
         {$EndIf}
          gtk_object_set_data(PGtkObject(TabCloseBtnWidget),'TabCloseBtnImage',
                              TabCloseBtnImageWidget);
          gtk_widget_show(TabCloseBtnImageWidget);
          gtk_container_add(PGtkContainer(TabCloseBtnWidget),
                            TabCloseBtnImageWidget);
        end;
        gtk_widget_show(TabCloseBtnWidget);
        g_signal_connect(PGtkObject(TabCloseBtnWidget), 'clicked',
          TGTKSignalFunc(@gtkNoteBookCloseBtnClicked), APage);
        gtk_box_pack_start_defaults(PGtkBox(TabWidget),TabCloseBtnWidget);
      end;
    end else begin
      // close buttons disabled
      if TabCloseBtnWidget<>nil then begin
        // there is a close button
        // -> remove it
        gtk_object_set_data(PGtkObject(TabWidget), 'TabCloseBtn',
                            nil);
        DestroyWidget(TabCloseBtnWidget);
        TabCloseBtnWidget:=nil;
      end;
    end;
  end;

begin
  ThePage:=TCustomPage(APage);
  TheNoteBook:=TCustomNotebook(ANoteBook);
  if (APage=nil) or (not ThePage.HandleAllocated) then exit;
  if TheNoteBook=nil then begin
    TheNoteBook:=TCustomNotebook(ThePage.Parent);
    if TheNoteBook=nil then exit;
  end;
  NoteBookWidget:=PGtkWidget(TWinControl(TheNoteBook).Handle);
  PageWidget:=PGtkWidget(TWinControl(ThePage).Handle);

  // get the tab container and the tab components: pixmap, label and closebtn
  TabWidget:=gtk_notebook_get_tab_label(PGtkNoteBook(NotebookWidget),
                                        PageWidget);
  if TabWidget<>nil then begin
    TabImageWidget:=gtk_object_get_data(PGtkObject(TabWidget), 'TabImage');
    TabLabelWidget:=gtk_object_get_data(PGtkObject(TabWidget), 'TabLabel');
    TabCloseBtnWidget:=gtk_object_get_data(PGtkObject(TabWidget),'TabCloseBtn');
  end else begin
    TabImageWidget:=nil;
    TabLabelWidget:=nil;
    TabCloseBtnWidget:=nil;
  end;

  // get the menu container and its components: pixmap and label
  MenuWidget:=gtk_notebook_get_menu_label(PGtkNoteBook(NotebookWidget),
                                          PageWidget);
  if MenuWidget<>nil then begin
    MenuImageWidget:=gtk_object_get_data(PGtkObject(MenuWidget), 'TabImage');
    MenuLabelWidget:=gtk_object_get_data(PGtkObject(MenuWidget), 'TabLabel');
  end else begin
    MenuImageWidget:=nil;
    MenuLabelWidget:=nil;
  end;

  UpdateTabImage;
  UpdateTabLabel;
  UpdateTabCloseBtn;
end;

{ TGtkWSCustomPage }

procedure TGtkWSCustomPage.UpdateProperties(const ACustomPage: TCustomPage);
begin
  UpdateNotebookPageTab(nil, ACustomPage);
end;

{ TGtkWSCustomNotebook }

procedure TGtkWSCustomNotebook.AddPage(const ANotebook: TCustomNotebook; 
  const AChild: TCustomPage; const AIndex: integer);
{
  Inserts a new page to a notebook at position Index. The ANotebook is a
  TCustomNoteBook, the AChild one of its TCustomPage. Both handles must already
  be created. ANoteBook Handle is a PGtkNoteBook and APage handle is a
  PGtkFixed.
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
  NoteBookWidget:=PGtkWidget(ANoteBook.Handle);
  PageWidget:=PGtkWidget(AChild.Handle);

  // create the tab (hbox container)
  TabWidget:=gtk_hbox_new(false,1);
  begin
    gtk_object_set_data(PGtkObject(TabWidget), 'TabImage', nil);
    gtk_object_set_data(PGtkObject(TabWidget), 'TabCloseBtn', nil);
    // put a label into the tab
    TabLabelWidget:=gtk_label_new('');
    gtk_object_set_data(PGtkObject(TabWidget), 'TabLabel', TabLabelWidget);
    gtk_widget_show(TabLabelWidget);
    gtk_box_pack_start_defaults(PGtkBox(TabWidget),TabLabelWidget);
  end;
  gtk_widget_show(TabWidget);

  // create popup menu
  MenuWidget:=gtk_hbox_new(false,2);
  begin
    // set icon widget to nil
    gtk_object_set_data(PGtkObject(MenuWidget), 'TabImage', nil);
    // put a label into the menu
    MenuLabelWidget:=gtk_label_new('');
    gtk_object_set_data(PGtkObject(MenuWidget), 'TabLabel', MenuLabelWidget);
    gtk_widget_show(MenuLabelWidget);
    gtk_box_pack_start_defaults(PGtkBox(MenuWidget),MenuLabelWidget);
  end;

  gtk_widget_show(MenuWidget);

  RemoveDummyNoteBookPage(PGtkNotebook(NoteBookWidget));
  gtk_notebook_insert_page_menu(GTK_NOTEBOOK(NotebookWidget), PageWidget,
    TabWidget, MenuWidget, AIndex);

  UpdateNotebookPageTab(ANoteBook, AChild);
  UpdateNoteBookClientWidget(ANoteBook);
end;

procedure TGtkWSCustomNotebook.MovePage(const ANotebook: TCustomNotebook; 
  const AChild: TCustomPage; const NewIndex: integer);
var
  NoteBookWidget: PGtkNotebook;
begin
  NoteBookWidget:=PGtkNotebook(ANoteBook.Handle);
  gtk_notebook_reorder_child(NoteBookWidget, PGtkWidget(AChild.Handle), NewIndex);
  UpdateNoteBookClientWidget(ANoteBook);
end;

procedure TGtkWSCustomNotebook.RemovePage(const ANotebook: TCustomNotebook; 
  const AIndex: integer);
begin
  // The gtk does not provide a function to remove a page without destroying it.
  // Luckily the LCL destroys the Handle, when a page is removed, so this
  // function is not needed.
end;

function TGtkWSCustomNotebook.GetNotebookMinTabHeight(
  const AWinControl: TWinControl): integer;
var
  NBWidget: PGTKWidget;
  BorderWidth: Integer;
  Requisition: TGtkRequisition;
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
    Result:=inherited GetNotebookMinTabHeight(AWinControl);
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
  {$WARNING TODO}
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
  debugln('TGtkWSCustomNotebook.GetNotebookMinTabHeight END ',dbgs(Result),' ',
    GetWidgetDebugReport(NBWidget));
{$ENDIF GTK2}
end;

function TGtkWSCustomNotebook.GetNotebookMinTabWidth(
  const AWinControl: TWinControl): integer;
begin
  Result:=inherited GetNotebookMinTabWidth(AWinControl);
end;

{ Code pasted from LM_GETITEMINDEX message implementation
     csNotebook:
       begin
         TLMNotebookEvent(Data^).Page :=
           gtk_notebook_get_current_page(PGtkNotebook(Handle));
         UpdateNoteBookClientWidget(ACustomListBox);
       end;
}

procedure TGtkWSCustomNotebook.SetPageIndex(const ANotebook: TCustomNotebook; const AIndex: integer);
begin
  gtk_notebook_set_page(PGtkNotebook(ANotebook.Handle), AIndex);
  UpdateNoteBookClientWidget(ANotebook);
end;

procedure TGtkWSCustomNotebook.SetTabPosition(const ANotebook: TCustomNotebook; const ATabPosition: TTabPosition);
var
  GtkNotebook: PGtkNotebook;
begin
  GtkNotebook := PGtkNotebook(ANotebook.Handle);
  case ATabPosition of
    tpTop   : gtk_notebook_set_tab_pos(GtkNotebook, GTK_POS_TOP);
    tpBottom: gtk_notebook_set_tab_pos(GtkNotebook, GTK_POS_BOTTOM);
    tpLeft  : gtk_notebook_set_tab_pos(GtkNotebook, GTK_POS_LEFT);
    tpRight : gtk_notebook_set_tab_pos(GtkNotebook, GTK_POS_RIGHT);
  end;
end;

procedure TGtkWSCustomNotebook.ShowTabs(const ANotebook: TCustomNotebook; AShowTabs: boolean);
begin
  gtk_notebook_set_show_tabs(PGtkNotebook(ANotebook.Handle), AShowTabs);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCustomPage, TGtkWSCustomPage);
  RegisterWSComponent(TCustomNotebook, TGtkWSCustomNotebook);
//  RegisterWSComponent(TPage, TGtkWSPage);
//  RegisterWSComponent(TNotebook, TGtkWSNotebook);
//  RegisterWSComponent(TShape, TGtkWSShape);
//  RegisterWSComponent(TCustomSplitter, TGtkWSCustomSplitter);
//  RegisterWSComponent(TSplitter, TGtkWSSplitter);
//  RegisterWSComponent(TPaintBox, TGtkWSPaintBox);
//  RegisterWSComponent(TCustomImage, TGtkWSCustomImage);
//  RegisterWSComponent(TImage, TGtkWSImage);
//  RegisterWSComponent(TBevel, TGtkWSBevel);
//  RegisterWSComponent(TCustomRadioGroup, TGtkWSCustomRadioGroup);
//  RegisterWSComponent(TRadioGroup, TGtkWSRadioGroup);
//  RegisterWSComponent(TCustomCheckGroup, TGtkWSCustomCheckGroup);
//  RegisterWSComponent(TCheckGroup, TGtkWSCheckGroup);
//  RegisterWSComponent(TBoundLabel, TGtkWSBoundLabel);
//  RegisterWSComponent(TCustomLabeledEdit, TGtkWSCustomLabeledEdit);
//  RegisterWSComponent(TLabeledEdit, TGtkWSLabeledEdit);
//  RegisterWSComponent(TCustomPanel, TGtkWSCustomPanel);
//  RegisterWSComponent(TPanel, TGtkWSPanel);
////////////////////////////////////////////////////
end.
