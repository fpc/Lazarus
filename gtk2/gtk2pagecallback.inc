// included by gtk2callback.inc

{
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

procedure DrawNotebookPageIcon(Page: TCustomPage; Widget: PGtkWidget);
var
  NoteBook: TCustomNotebook;
  NoteBookWidget: PGtkWidget;
  PageWidget: PGtkWidget;
  TabWidget: PGtkWidget;
  ImageIndex: Integer;
begin
  NoteBook := Page.Parent as TCustomNotebook;
  ImageIndex := NoteBook.GetImageIndex(Page.PageIndex);
  if (NoteBook.Images = nil) or (ImageIndex < 0)
  or (Page.ImageIndex >= NoteBook.Images.Count)
  or (not NoteBook.HandleAllocated)
  or (not Page.HandleAllocated)
  then exit;
  NoteBookWidget := PGtkWidget(NoteBook.Handle);
  PageWidget := PGtkWidget(Page.Handle);

  // get the tab container and the tab icon widget
  TabWidget := gtk_notebook_get_tab_label(PGtkNoteBook(NotebookWidget),
                                        PageWidget);
  if TabWidget = nil then exit;
  {$IFDEF VerboseGtkToDos}{$note reimplement}{$ENDIF}
  DrawImageListIconOnWidget(NoteBook.Images, ImageIndex, Widget);
end;

function PageIconWidgetExposeAfter(Widget: PGtkWidget; Event: PGDKEventExpose;
  Data: gPointer): GBoolean; cdecl;
var
  ThePage: TCustomPage;
begin
  Result := false;
  //DebugLn('PageIconWidgetExposeAfter ',DbgS(Widget));
  EventTrace('PageIconWidgetExposeAfter', Data);
  if (Event^.Count > 0) then exit;
  ThePage := TObject(Data) as TCustomPage;
  DrawNotebookPageIcon(ThePage, Widget);
end;


// included by gtk2callback.inc
