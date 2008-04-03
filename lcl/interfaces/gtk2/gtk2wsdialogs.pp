{ $Id$}
{
 *****************************************************************************
 *                             Gtk2WSDialogs.pp                              * 
 *                             ----------------                              * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

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
unit Gtk2WSDialogs;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Gtk2, Glib2, gdk2, pango,
  SysUtils, Classes,
  Controls, Dialogs, ExtDlgs,
  GtkWSDialogs, GtkInt, GtkGlobals, GtkDef, GtkProc,
////////////////////////////////////////////////////
  LCLType, WSDialogs, WSLCLClasses, FileUtil;
type
  { TGtk2WSCommonDialog }

  TGtk2WSCommonDialog = class(TGtkWSCommonDialog)
  private
  protected
  public
  end;

  { TGtk2WSFileDialog }

  TGtk2WSFileDialog = class(TGtkWSFileDialog)
  private
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); override;
  public
    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
  end;

  { TGtk2WSOpenDialog }

  TGtk2WSOpenDialog = class(TGtkWSOpenDialog)
  private
  protected
    class function CreateOpenDialogFilter(OpenDialog: TOpenDialog; SelWidget: PGtkWidget): string; override;
    class procedure CreatePreviewDialogControl(PreviewDialog: TPreviewFileDialog; SelWidget: PGtkWidget); override;
  public
    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
  end;

  { TGtk2WSSaveDialog }

  TGtk2WSSaveDialog = class(TGtkWSSaveDialog)
  private
  protected
  public
  end;

  { TGtk2WSSelectDirectoryDialog }

  TGtk2WSSelectDirectoryDialog = class(TGtkWSSelectDirectoryDialog)
  private
  protected
  public
  end;

  { TGtk2WSColorDialog }

  TGtk2WSColorDialog = class(TGtkWSColorDialog)
  private
  protected
  public
  end;

  { TGtk2WSColorButton }

  TGtk2WSColorButton = class(TGtkWSColorButton)
  private
  protected
  public
  end;

  { TGtk2WSFontDialog }

  TGtk2WSFontDialog = class(TGtkWSFontDialog)
  private
  protected
  public
  end;


implementation

// ---------------------- signals ----------------------------------------------

procedure gtkFileChooserSelectionChangedCB(Chooser: PGtkFileChooser;
  Data: Pointer); cdecl;
var
  cFilename: Pgchar;
  theDialog: TFileDialog;
begin
  //DebugLn(['gtkFileChooserSelectionChangedCB ']);
  cFilename := gtk_file_chooser_get_filename(Chooser);
  theDialog:=TFileDialog(Data);
  try
    if theDialog is TOpenDialog then
      UpdateDetailView(TOpenDialog(theDialog));
  finally
    if Assigned(cFilename) then
      g_free(cFilename);
  end;
end;

procedure Gtk2FileChooserResponseCB(widget: PGtkFileChooser; arg1: gint;
  data: gpointer); cdecl;

  procedure AddFile(List: TStrings; const NewFile: string);
  var
    i: Integer;
  begin
    for i := 0 to List.Count-1 do
      if List[i] = NewFile then
        Exit;
    List.Add(NewFile);
  end;

var
  TheDialog: TFileDialog;
  cFilename: PChar;
  cFilenames: PGSList;
  cFilenames1: PGSList;
  Files: TStringList;
begin
  //DebugLn(['Gtk2FileChooserResponseCB ']);
  theDialog := TFileDialog(data);

  if arg1 = GTK_RESPONSE_CANCEL then
  begin
    TheDialog.UserChoice := mrCancel;
    Exit;
  end;

  if theDialog is TOpenDialog then
  begin
    if ofAllowMultiSelect in TOpenDialog(theDialog).Options then
    begin
      TheDialog.FileName := '';
      Files := TStringList(TheDialog.Files);
      Files.Clear;
      cFilenames := gtk_file_chooser_get_filenames(widget);
      if Assigned(cFilenames) then
      begin
        cFilenames1 := cFilenames;
        while Assigned(cFilenames1) do
        begin
          cFilename := PChar(cFilenames1^.data);
          if Assigned(cFilename) then
          begin
            AddFile(Files, cFilename);
            g_free(cFilename);
          end;
          cFilenames1 := cFilenames1^.next;
        end;
        g_slist_free(cFilenames);
      end;
    end;
  end;

  cFilename := gtk_file_chooser_get_filename(widget);
  if Assigned(cFilename) then
  begin
    TheDialog.FileName := cFilename;
    g_free(cFilename);
  end;

  //?? StoreCommonDialogSetup(theDialog);
  theDialog.UserChoice := mrOK;
end;

procedure Gtk2FileChooserNotifyCB(dialog: PGObject; pspec: PGParamSpec;
  user_data: gpointer); cdecl;
var
  TheDialog: TFileDialog;
  GtkFilter: PGtkFileFilter;
  GtkFilterList: PGSList;
  NewFilterIndex: Integer;
begin
  //DebugLn(['Gtk2FileChooserNotifyCB ']);
  if pspec^.name = 'filter' then
  begin // filter changed
    theDialog := TFileDialog(user_data);
    GtkFilter := gtk_file_chooser_get_filter(dialog);
    GtkFilterList := gtk_file_chooser_list_filters(dialog);
    NewFilterIndex := g_slist_index(GtkFilterList, GtkFilter);
    theDialog.IntfFileTypeChanged(NewFilterIndex + 1);
    g_slist_free(GtkFilterList);
  end;
end;

// ---------------------- END OF signals ---------------------------------------

{ TGtk2WSOpenDialog }

class function TGtk2WSOpenDialog.CreateOpenDialogFilter(
  OpenDialog: TOpenDialog; SelWidget: PGtkWidget): string;
var
  FilterList: TFPList;
  i, j, k: integer;
  GtkFilter: PGtkFileFilter;
  MaskList: TStringList;
begin
  ExtractFilterList(OpenDialog.Filter, FilterList, false);
  if FilterList.Count > 0 then
  begin
    j := 1;
    MaskList := TStringList.Create;
    MaskList.Delimiter := ';';
    for i := 0 to FilterList.Count-1 do
    begin
      GtkFilter := gtk_file_filter_new();

      MaskList.DelimitedText := PFileSelFilterEntry(FilterList[i])^.Mask;

      for k := 0 to MaskList.Count-1 do
        gtk_file_filter_add_pattern(GtkFilter, PChar(MaskList.Strings[k]));

      gtk_file_filter_set_name(GtkFilter, PFileSelFilterEntry(FilterList[i])^.Description);

      gtk_file_chooser_add_filter(SelWidget, GtkFilter);

      if j = OpenDialog.FilterIndex then
        gtk_file_chooser_set_filter(SelWidget, GtkFilter);

      Inc(j);
      GtkFilter := nil;
    end;
    MaskList.Free;
  end;

  gtk_object_set_data(PGtkObject(SelWidget), 'LCLFilterList', FilterList);

  Result := 'hm'; { Don't use '' as null return as this is used for *.* }
end;

class procedure TGtk2WSOpenDialog.CreatePreviewDialogControl(
  PreviewDialog: TPreviewFileDialog; SelWidget: PGtkWidget);
var
  PreviewWidget: PGtkWidget;
  AControl: TPreviewFileControl;
  FileChooser: PGtkFileChooser;
begin
  AControl := PreviewDialog.PreviewFileControl;
  if AControl = nil then Exit;

  FileChooser := PGtkFileChooser(SelWidget);

  PreviewWidget := PGtkWidget(AControl.Handle);

  gtk_object_set_data(PGtkObject(PreviewWidget),'LCLPreviewFixed',
                      PreviewWidget);
  gtk_widget_set_size_request(PreviewWidget,AControl.Width,AControl.Height);

  gtk_file_chooser_set_preview_widget(FileChooser, PreviewWidget);

  gtk_widget_show(PreviewWidget);
end;

{
  Adds some functionality to a gtk file selection dialog.
  - multiselection
  - range selection
  - close on escape
  - file information
  - history pulldown
  - filter pulldown
  - preview control

  requires: gtk+ 2.6
}
class function TGtk2WSOpenDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;
var
  FileSelWidget: PGtkFileChooser;
  OpenDialog: TOpenDialog absolute ACommonDialog;
  HelpButton: PGtkWidget;
  InitialFilename: String;
  //FrameWidget: PGtkWidget;
  //HBox: PGtkWidget;
  //FileDetailLabel: PGtkWidget;
begin
  Result := TGtk2WSFileDialog.CreateHandle(ACommonDialog);
  FileSelWidget := PGtkFileChooser(Result);

  if OpenDialog.InheritsFrom(TSaveDialog) then
  begin
    if OpenDialog.InitialDir <> '' then
      gtk_file_chooser_set_current_folder(FileSelWidget, Pgchar(OpenDialog.InitialDir));
  end;
  
  // Help button
  if (ofShowHelp in OpenDialog.Options) then
  begin
    HelpButton := gtk_dialog_add_button(FileSelWidget, GTK_STOCK_HELP, GTK_RESPONSE_NONE);

    g_signal_connect(PGtkObject(HelpButton),
      'clicked', gtk_signal_func(@gtkDialogHelpclickedCB), OpenDialog);
  end;

  if ofAllowMultiSelect in OpenDialog.Options then
    gtk_file_chooser_set_select_multiple(FileSelWidget, gboolean(gtrue));

  // History List - a frame with an option menu
  CreateOpenDialogHistory(OpenDialog, FileSelWidget);

  // Filter
  CreateOpenDialogFilter(OpenDialog, FileSelWidget);

  // connect change event
  g_signal_connect(PGtkObject(FileSelWidget),
    'selection-changed', gtk_signal_func(@gtkFileChooserSelectionChangedCB),
    OpenDialog);

  (*  TODO
  // Details - a frame with a label
  if (ofViewDetail in OpenDialog.Options) then begin

    // create the frame around the information
    FrameWidget:=gtk_frame_new(PChar(rsFileInformation));
    //gtk_box_pack_start(GTK_BOX(FileSelWidget^.main_vbox),
    //                   FrameWidget,false,false,0);
    gtk_box_pack_start(GTK_BOX(gtk_file_chooser_get_extra_widget(
             PGtkFileChooser(SelWidget))), FrameWidget,false,false,0);
    gtk_widget_show(FrameWidget);
    // create a HBox, so that the information is left justified
    HBox:=gtk_hbox_new(false,0);
    gtk_container_add(GTK_CONTAINER(FrameWidget), HBox);
    // create the label for the file information
    FileDetailLabel:=gtk_label_new(PChar(rsDefaultFileInfoValue));
    gtk_box_pack_start(GTK_BOX(HBox),FileDetailLabel,false,false,5);
    gtk_widget_show_all(HBox);
  end else
    FileDetailLabel:=nil;
  gtk_object_set_data(PGtkObject(SelWidget), 'FileDetailLabel',
                      FileDetailLabel);
  *)
  // preview
  if (OpenDialog is TPreviewFileDialog) then
    CreatePreviewDialogControl(TPreviewFileDialog(OpenDialog), PGtkWidget(FileSelWidget));

  // set initial filename (gtk expects an absolute filename)
  InitialFilename := TrimFilename(OpenDialog.Filename);
  if InitialFilename <> '' then
  begin
    if not FilenameIsAbsolute(InitialFilename) and (OpenDialog.InitialDir <> '') then
      InitialFilename := TrimFilename(OpenDialog.InitialDir + PathDelim + InitialFilename);
    if not FilenameIsAbsolute(InitialFilename) then
      InitialFilename := CleanAndExpandFilename(InitialFilename);
    gtk_file_chooser_set_filename(FileSelWidget, PChar(InitialFilename));
  end;

  //if InitialFilter <> 'none' then
  //  PopulateFileAndDirectoryLists(FileSelWidget, InitialFilter);
end;

{ TGtk2WSFileDialog }

class procedure TGtk2WSFileDialog.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtkWSCommonDialog.SetCallbacks(AGtkWidget, AWidgetInfo);
  g_signal_connect(AGtkWidget, 'response', gtk_signal_func(@Gtk2FileChooserResponseCB), AWidgetInfo^.LCLObject);
  g_signal_connect(AGtkWidget, 'notify', gtk_signal_func(@Gtk2FileChooserNotifyCB), AWidgetInfo^.LCLObject);
end;

{
  Creates a new TFile/Open/SaveDialog
  requires: gtk+ 2.6
}
class function TGtk2WSFileDialog.CreateHandle(const ACommonDialog: TCommonDialog
  ): THandle;
var
  FileDialog: TFileDialog absolute ACommonDialog;
  Action: TGtkFileChooserAction;
  Button1: String;
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
begin
  Action := GTK_FILE_CHOOSER_ACTION_OPEN;
  Button1 := GTK_STOCK_OPEN;

  if (FileDialog is TSaveDialog) or (FileDialog is TSavePictureDialog) then
  begin
    Action := GTK_FILE_CHOOSER_ACTION_SAVE;
    Button1 := GTK_STOCK_SAVE;
  end
  else
  if FileDialog is TSelectDirectoryDialog then
  begin
    Action := GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER;
    Button1 := GTK_STOCK_OPEN;
  end;

  Widget := gtk_file_chooser_dialog_new(PChar(FileDialog.Title), nil, Action,
    PChar(GTK_STOCK_CANCEL), [GTK_RESPONSE_CANCEL, PChar(Button1), GTK_RESPONSE_OK, nil]);

(*gtk 2.8
  if FileDialog is TSaveDialog then
  begin
    gtk_file_chooser_set_do_overwrite_confirmation(Widget,
      ofOverwritePrompt in TOpenDialog(theDialog).Options);
  end;
*)
  if FileDialog.InitialDir <> '' then
    gtk_file_chooser_set_current_folder(Widget, Pgchar(FileDialog.InitialDir));

  if gtk_file_chooser_get_action(Widget) in
    [GTK_FILE_CHOOSER_ACTION_SAVE, GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER]
  then
    gtk_file_chooser_set_current_name(Widget, Pgchar(FileDialog.FileName));

  Result := THandle(PtrUInt(Widget));
  WidgetInfo := CreateWidgetInfo(Widget);
  WidgetInfo^.LCLObject := ACommonDialog;
  TGtkWSCommonDialog.SetSizes(Widget, WidgetInfo);
  SetCallbacks(Widget, WidgetInfo);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TCommonDialog, TGtk2WSCommonDialog);
  RegisterWSComponent(TFileDialog, TGtk2WSFileDialog);
  RegisterWSComponent(TOpenDialog, TGtk2WSOpenDialog);
//  RegisterWSComponent(TSaveDialog, TGtk2WSSaveDialog);
//  RegisterWSComponent(TSelectDirectoryDialog, TGtk2WSSelectDirectoryDialog);
//  RegisterWSComponent(TColorDialog, TGtk2WSColorDialog);
//  RegisterWSComponent(TColorButton, TGtk2WSColorButton);
//  RegisterWSComponent(TFontDialog, TGtk2WSFontDialog);
////////////////////////////////////////////////////
end.
