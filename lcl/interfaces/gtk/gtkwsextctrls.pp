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
  gtk2,
{$ELSE GTK2}
  gtk,
{$ENDIF GTK2}
  GtkGlobals, GtkProc, ExtCtrls,
  WSExtCtrls, WSLCLClasses, gtkint, interfacebase;

type

  { TGtkWSCustomPage }

  TGtkWSCustomPage = class(TWSCustomPage)
  private
  protected
  public
  end;

  { TGtkWSCustomNotebook }

  TGtkWSCustomNotebook = class(TWSCustomNotebook)
  private
  protected
  public
    class procedure AddPage(const ANotebook: TCustomNotebook; const AChild: TCustomPage; const AIndex: integer); override;
    class procedure RemovePage(const ANotebook: TCustomNotebook; const AIndex: integer); override;
    
    class function GetNotebookMinTabHeight(const AWinControl: TWinControl): integer; override;
    class function GetNotebookMinTabWidth(const AWinControl: TWinControl): integer; override;
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

  TGtkWidgetSet(InterfaceObject).UpdateNotebookPageTab(ANoteBook, AChild);
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

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TCustomPage, TGtkWSCustomPage);
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
