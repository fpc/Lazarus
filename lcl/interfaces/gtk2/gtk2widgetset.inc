{%MainUnit gtk2int.pas}
{******************************************************************************
                                   TGtk2WidgetSet
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

function GTK2FocusCB( widget: PGtkWidget; event:PGdkEventFocus;
  data: gPointer) : GBoolean; cdecl;
var
  Status : gBoolean;
begin
  Status := GTKFocusCB(Widget, Event, Data);

  if GtkWidgetIsA(Widget,GTK_APIWIDGETCLIENT_TYPE) then
    Result := Status
  else
    Result := False;
end;

function GTK2FocusCBAfter(widget: PGtkWidget; event:PGdkEventFocus;
  data: gPointer) : GBoolean; cdecl;
var
  Status : gBoolean;
begin
  Status := GTKFocusCBAfter(Widget, Event, Data);

  if GtkWidgetIsA(Widget,GTK_APIWIDGETCLIENT_TYPE) then
    Result := Status
  else
    Result := False;
end;

function gtk2HideCB( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
   Status : GBoolean;
begin
  Status := gtkHideCB(Widget, Data);

  if GtkWidgetIsA(Widget,GTK_APIWIDGETCLIENT_TYPE) then
    Result := Status
  else
    Result := False;
end;

function GTK2KeyDown(Widget: PGtkWidget; Event : pgdkeventkey;
  Data: gPointer) : GBoolean; cdecl;
begin
  //debugln('GTK2KeyDown ',DbgSName(TObject(Data)));
  Result := HandleGtkKeyUpDown(Widget, Event, Data, True, True);
end;

function GTK2KeyDownAfter(Widget: PGtkWidget; Event : pgdkeventkey;
  Data: gPointer) : GBoolean; cdecl;
begin
  //debugln('GTK2KeyDownAfter ',DbgSName(TObject(Data)));
  Result := HandleGtkKeyUpDown(Widget, Event, Data, False, True);
end;

function GTK2KeyUp(Widget: PGtkWidget; Event : pgdkeventkey;
  Data: gPointer) : GBoolean; cdecl;
begin
  //debugln('GTK2KeyUp ',DbgSName(TObject(Data)));
  Result := HandleGtkKeyUpDown(Widget, Event, Data, True, False);
end;

function GTK2KeyUpAfter(Widget: PGtkWidget; Event : pgdkeventkey;
  Data: gPointer) : GBoolean; cdecl;
begin
  //debugln('GTK2KeyUpAfter ',DbgSName(TObject(Data)));
  Result := HandleGtkKeyUpDown(Widget, Event, Data, False, False);
end;

function GTK2KillFocusCB(widget: PGtkWidget; event:PGdkEventFocus;
  data: gPointer) : GBoolean; cdecl;
var
  Status : gBoolean;
begin
  Status := GTKKillFocusCB(Widget, Event, Data);

  if GtkWidgetIsA(Widget,GTK_APIWIDGETCLIENT_TYPE) then
    Result := Status
  else
    Result := False;
end;

function GTK2KillFocusCBAfter(widget: PGtkWidget; event:PGdkEventFocus;
  data: gPointer) : GBoolean; cdecl;
var
  Status : gBoolean;
begin
  Status := GTKKillFocusCBAfter(Widget, Event, Data);

  if GtkWidgetIsA(Widget,GTK_APIWIDGETCLIENT_TYPE) then
    Result := Status
  else
    Result := False;
end;

function gtk2showCB( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
   Status : GBoolean;
begin
  Status := gtkshowCB(Widget, Data);

  if GtkWidgetIsA(Widget,GTK_APIWIDGETCLIENT_TYPE) then
    Result := Status
  else
    Result := False;
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

procedure Gtk2FileChooserResponseCB(widget: PGtkFileChooser; arg1: gint;
  data: gpointer); cdecl;

  procedure AddFile(List: TStrings; const NewFile: string);
  var
    i: Integer;
  begin
    for i:=0 to List.Count-1 do
      if List[i]=NewFile then exit;
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

  if arg1 = GTK_RESPONSE_CANCEL then begin  
    TheDialog.UserChoice := mrCancel;
    Exit;
  end;

  if theDialog is TOpenDialog then begin
    if ofAllowMultiSelect in TOpenDialog(theDialog).Options then begin
      TheDialog.FileName := '';
      Files := TStringList(TheDialog.Files);
      Files.Clear;

      cFilenames := gtk_file_chooser_get_filenames(widget);
      if Assigned(cFilenames) then begin

        cFilenames1 := cFilenames;
        while Assigned(cFilenames1) do begin
          cFilename := PChar(cFilenames1^.data);

          if Assigned(cFilename) then begin
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
  if Assigned(cFilename) then begin
    TheDialog.FileName := cFilename;
    g_free(cFilename);
  end;

  //?? StoreCommonDialogSetup(theDialog);
  theDialog.UserChoice := mrOK;
end;

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

Procedure gtk_clb_toggle(cellrenderertoggle : PGtkCellRendererToggle; arg1 : PGChar;
                         WinControl: TWinControl); cdecl;
var
  aWidget : PGTKWidget;
  aTreeModel : PGtkTreeModel;
  aTreeIter : TGtkTreeIter;
  value : pgValue;
begin
  aWidget := GetWidgetInfo(Pointer(WinControl.Handle), True)^.CoreWidget;
  aTreeModel := gtk_tree_view_get_model (GTK_TREE_VIEW(aWidget));
  if (gtk_tree_model_get_iter_from_string (aTreeModel, @aTreeIter, arg1)) then begin
    aTreeIter.stamp := GTK_LIST_STORE (aTreeModel)^.stamp; //strange hack
    value := g_new0(SizeOf(TgValue), 1);
    gtk_tree_model_get_value(aTreeModel, @aTreeIter, 0, value);

    g_value_set_boolean(value, not g_value_get_boolean(value));

    gtk_list_store_set_value (GTK_LIST_STORE (aTreeModel), @aTreeIter, 0, value);
    g_value_unset(value);
    g_free(value);
  end;
end;

Procedure gtk_clb_toggle_row_activated(treeview : PGtkTreeView; arg1 : PGtkTreePath;
                                  arg2 : PGtkTreeViewColumn; data : gpointer); cdecl;
var
  aTreeModel : PGtkTreeModel;
  aTreeIter : TGtkTreeIter;
  value : PGValue;
begin
  aTreeModel := gtk_tree_view_get_model (treeview);
  if (gtk_tree_model_get_iter (aTreeModel, @aTreeIter, arg1)) then begin
    aTreeIter.stamp := GTK_LIST_STORE (aTreeModel)^.stamp; //strange hack
    value := g_new0(SizeOf(TgValue), 1);
    gtk_tree_model_get_value(aTreeModel, @aTreeIter, 0, value);

    g_value_set_boolean(value, not g_value_get_boolean(value));

    gtk_list_store_set_value (GTK_LIST_STORE (aTreeModel), @aTreeIter, 0, value);
    g_value_unset(value);
    g_free(value);
  end;
end;

procedure gtk_commit_cb (context: PGtkIMContext; const Str: Pgchar;
  Data: Pointer); cdecl;
begin
  //DebugLn(['gtk_commit_cb ',dbgstr(Str),'="',Str,'"']);
  im_context_string:=Str;
end;

{$IfNDef GTK2_2}
Procedure gtkTreeSelectionCountSelectedRows(model : PGtkTreeModel; path : PGtkTreePath;
                                  iter : PGtkTreeIter; data : PGint); cdecl;
begin
  If Assigned(Data) then
    Inc(Data^);
end;

Type
  PPGList = ^PGList;
  
Procedure gtkTreeSelectionGetSelectedRows(model : PGtkTreeModel; path : PGtkTreePath;
                                  iter : PGtkTreeIter; data : PPGList); cdecl;
begin
  If Assigned(Data) then
    Data^ := g_list_append(Data^, gtk_tree_path_copy(path));
end;
{$EndIf}

{------------------------------------------------------------------------------
  procedure TGtk2WidgetSet.AppendText(Sender: TObject; Str: PChar);
 ------------------------------------------------------------------------------}
procedure TGtk2WidgetSet.AppendText(Sender: TObject; Str: PChar);
var
  Widget : PGtkWidget;
  aTextBuffer : PGtkTextBuffer;
  aTextIter1  : TGtkTextIter;
  aTextIter2  : TGtkTextIter;
begin
  if Str=nil then exit;

  if (Sender is TWinControl) then begin
    case TWinControl(Sender).fCompStyle of
      csMemo:
      begin
       Widget:= GetWidgetInfo(Pointer(TWinControl(Sender).Handle), True)^.CoreWidget;
        aTextBuffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(Widget));
        gtk_text_buffer_begin_user_action(aTextBuffer);
        gtk_text_buffer_get_bounds(aTextBuffer, @aTextIter1, @aTextIter2);
        gtk_text_buffer_insert(aTextBuffer, @aTextIter2, str, StrLen(str));
        gtk_text_buffer_end_user_action(aTextBuffer);
      end;
      {else
        inherited AppendText(Sender, Str);}
    end;
  end;
end;


function TGtk2WidgetSet.GetText(Sender: TComponent; var Text: String): Boolean;
var
  CS: PChar;
  Widget : PGtkWidget;
  aTextBuffer : PGtkTextBuffer;
  aTextIter1  : TGtkTextIter;
  aTextIter2  : TGtkTextIter;
begin
  Result := True;
  case TControl(Sender).fCompStyle of
   csEdit: begin
             Widget:= GTK_WIDGET(Pointer(TWinControl(Sender).Handle));
             CS := gtk_editable_get_chars(GTK_EDITABLE(Widget), 0, -1);
             Text := StrPas(CS);
             g_free(CS);
           end;

   csMemo    : begin
                 Widget:= GetWidgetInfo(Pointer(TWinControl(Sender).Handle), True)^.CoreWidget;
                  aTextBuffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(Widget));
                  gtk_text_buffer_get_bounds(aTextBuffer, @aTextIter1, @aTextIter2);
                  CS := gtk_text_buffer_get_text(aTextBuffer, @aTextIter1, @aTextIter2, True);
                  Text := StrPas(CS);
                  g_free(CS);
               end;
  {else
    Result := inherited GetText(Sender, Text);}
  end;
end;


procedure Tgtk2widgetset.HookSignals(const AGTKObject: PGTKObject; const ALCLObject: TObject);
var
  Info: PWidgetInfo;
begin
  if (ALCLObject is TWinControl) then
     Begin
       inherited HookSignals(AGTKObject,ALCLObject);
     End;

  if (ALCLObject is TControl) then begin
    Info:=GetWidgetInfo(AGTKObject,false);
  
    if GtkWidgetIsA(PGtkWidget(AGTKObject),GTK_TYPE_ENTRY) then begin
      // TEdit, TSpinEdit
      SetCallback(LM_CHANGED, AGTKObject, ALCLObject);
      SetCallback(LM_ACTIVATE, AGTKObject,ALCLObject);
      SetCallback(LM_CUTTOCLIP, AGTKObject,ALCLObject);
      SetCallback(LM_COPYTOCLIP, AGTKObject,ALCLObject);
      SetCallback(LM_PASTEFROMCLIP, AGTKObject,ALCLObject);
    end else if GtkWidgetIsA(Info^.CoreWidget,GTK_TYPE_TEXT_VIEW) then begin
      // TMemo
      // SetCallback(LM_CHANGED, AGTKObject,ALCLObject);
      //SetCallback(LM_ACTIVATE, AGTKObject,ALCLObject);
      SetCallback(LM_CUTTOCLIP, AGTKObject,ALCLObject);
      SetCallback(LM_COPYTOCLIP, AGTKObject,ALCLObject);
      SetCallback(LM_PASTEFROMCLIP, AGTKObject,ALCLObject);
      //SetCallback(LM_INSERTTEXT, AGTKObject,ALCLObject);
    end; //case
  end
  else
    If (ALCLObject is TMenuItem) then begin
      SetCallback(LM_ACTIVATE,AGTKObject,ALCLObject);
    end;
end;

{------------------------------------------------------------------------------
  Function: TGtk2WidgetSet.SetCallbackEx
  Params: Msg - message for which to set a callback
          sender - object to which callback will be send
  Returns:  nothing

  Applies a Message to the sender
 ------------------------------------------------------------------------------}
procedure TGTK2WidgetSet.SetCallbackEx(const AMsg: LongInt;
  const AGTKObject: PGTKObject; const ALCLObject: TObject; Direct: boolean);

  procedure ConnectSenderSignal(const AnObject:PGTKObject; const ASignal: PChar;
    const ACallBackProc: Pointer);
  begin
    ConnectSignal(AnObject,ASignal,ACallBackProc,TComponent(ALCLObject));
  end;

  procedure ConnectSenderSignalAfter(const AnObject:PGTKObject;
    const ASignal: PChar; const ACallBackProc: Pointer);
  begin
    ConnectSignalAfter(AnObject,ASignal,ACallBackProc,TComponent(ALCLObject));
  end;

  procedure ConnectSenderSignal(const AnObject:PGTKObject; const ASignal: PChar;
    const ACallBackProc: Pointer; const ReqSignalMask: TGdkEventMask);
  begin
    ConnectSignal(AnObject,ASignal,ACallBackProc,ALCLObject,
                  ReqSignalMask);
  end;

  procedure ConnectSenderSignalAfter(const AnObject:PGTKObject;
    const ASignal: PChar; const ACallBackProc: Pointer;
    const ReqSignalMask: TGdkEventMask);
  begin
    ConnectSignalAfter(AnObject,ASignal,ACallBackProc,ALCLObject,
                       ReqSignalMask);
  end;

  procedure ConnectFocusEvents(const AnObject: PGTKObject);
  begin
    //DebugLn(['ConnectFocusEvents ',GetWidgetDebugReport(PGtkWidget(AnObject))]);
    ConnectSenderSignal(AnObject, 'focus-in-event', @gtk2FocusCB);
    ConnectSenderSignalAfter(AnObject, 'focus-in-event', @gtk2FocusCBAfter);
    ConnectSenderSignal(AnObject, 'focus-out-event', @gtk2KillFocusCB);
    ConnectSenderSignalAfter(AnObject, 'focus-out-event', @gtk2KillFocusCBAfter);
  end;

  procedure ConnectKeyPressReleaseEvents(const AnObject: PGTKObject);
  begin
    //debugln('gtk2object ConnectKeyPressReleaseEvents A ALCLObject=',DbgSName(ALCLObject));
    ConnectSenderSignal(AnObject,
      'key-press-event', @GTK2KeyDown, GDK_KEY_PRESS_MASK);
    ConnectSenderSignalAfter(AnObject,
      'key-press-event', @GTK2KeyDownAfter, GDK_KEY_PRESS_MASK);
    ConnectSenderSignal(AnObject,
      'key-release-event', @GTK2KeyUp, GDK_KEY_RELEASE_MASK);
    ConnectSenderSignalAfter(AnObject,
      'key-release-event', @GTK2KeyUpAfter, GDK_KEY_RELEASE_MASK);
  end;

var
  gObject, gFixed, gCore: PGTKObject;
begin
  //debugln('gtk2object.inc TGtkWidgetSet.SetCallback A ALCLObject=',DbgSName(ALCLObject),' AMsg=',dbgs(AMsg));
  if Direct then begin
    gObject := AGTKObject;
    gFixed := AGTKObject;
    gCore := AGTKObject;
  end else begin
    gObject := AGTKObject;
    if gObject = nil then Exit;

    // gFixed is the widget with the client area (e.g. TGroupBox, TForm have this)
    gFixed := PGTKObject(GetFixedWidget(gObject));
    if gFixed = nil then gFixed := gObject;

    // gCore is the main widget (e.g. TListView has this)
    gCore:= PGtkObject(GetWidgetInfo(gObject, True)^.CoreWidget);
  end;

  case AMsg of
    LM_FOCUS :
    begin
      ConnectFocusEvents(gCore);
    end;

    LM_CHAR,
    LM_KEYDOWN,
    LM_KEYUP,
    
    LM_SYSCHAR,
    LM_SYSKEYDOWN,
    LM_SYSKEYUP:
    begin
      if (ALCLObject is TCustomComboBox) then begin
        ConnectKeyPressReleaseEvents(PgtkObject(PgtkCombo(gObject)^.entry));
      end
      else if (ALCLObject is TCustomForm) then begin
        ConnectKeyPressReleaseEvents(gObject);
      end;
      ConnectKeyPressReleaseEvents(gCore);
    end;

    LM_SHOWWINDOW :
    begin
      ConnectSenderSignal(gObject, 'show', @gtk2showCB);
      ConnectSenderSignal(gObject, 'hide', @gtk2hideCB);
    end;

  else
    Inherited SetCallbackEx(AMsg, AGTKObject, ALCLObject, Direct);
  end;
end;

function TGtk2WidgetSet.LoadStockPixmap(StockID: longint; var Mask: HBitmap) : HBitmap;
var
  Pixmap : PGDIObject;
  StockName : PChar;
  IconSet : PGtkIconSet;
  Pixbuf : PGDKPixbuf;
begin
  Mask := 0;
  case StockID Of
    idButtonOk : StockName := GTK_STOCK_OK;
    idButtonCancel : StockName := GTK_STOCK_CANCEL;
    idButtonYes : StockName := GTK_STOCK_YES;
    idButtonNo : StockName := GTK_STOCK_NO;
    idButtonHelp : StockName := GTK_STOCK_HELP;
    idButtonAbort : StockName := GTK_STOCK_CANCEL;
    idButtonClose : StockName := GTK_STOCK_QUIT;

    idDialogWarning : StockName := GTK_STOCK_DIALOG_WARNING;
    idDialogError : StockName := GTK_STOCK_DIALOG_ERROR;
    idDialogInfo : StockName := GTK_STOCK_DIALOG_INFO;
    idDialogConfirm : StockName := GTK_STOCK_DIALOG_QUESTION;
   else begin
      Result := inherited LoadStockPixmap(StockID, Mask);
      exit;
    end;
  end;

  if (StockID >= idButtonBase) and (StockID <= idDialogBase) then
    IconSet := gtk_style_lookup_icon_set(GetStyle(lgsButton), StockName)
  else
    IconSet := gtk_style_lookup_icon_set(GetStyle(lgsWindow), StockName);

  if (IconSet = nil) then
  begin
    Result := inherited LoadStockPixmap(StockID,Mask);
    exit;
  end;

  if (StockID >= idButtonBase) and (StockID <= idDialogBase) then
    pixbuf := gtk_icon_set_render_icon(IconSet, GetStyle(lgsbutton),
      GTK_TEXT_DIR_NONE, GTK_STATE_NORMAL, GTK_ICON_SIZE_BUTTON, GetStyleWidget(lgsbutton), nil)
  else
    pixbuf := gtk_icon_set_render_icon(IconSet, GetStyle(lgswindow),
      GTK_TEXT_DIR_NONE, GTK_STATE_NORMAL, GTK_ICON_SIZE_DIALOG, GetStyleWidget(lgswindow), nil);

  Pixmap := NewGDIObject(gdiBitmap);
  with Pixmap^ do
  begin
    GDIBitmapType := gbPixmap;
    visual := gdk_visual_get_system();
    gdk_visual_ref(visual);
    colormap := gdk_colormap_get_system();
    gdk_colormap_ref(colormap);
    gdk_pixbuf_render_pixmap_and_mask(pixbuf, GDIPixmapObject.Image,
      GDIPixmapObject.Mask, 128);
  end;

  gdk_pixbuf_unref(pixbuf);
  Result := HBitmap(PtrUInt(Pixmap));
end;

{------------------------------------------------------------------------------
  procedure TGtk2WidgetSet.SetSelectionMode(Sender: TObject; Widget: PGtkWidget;
    MultiSelect, ExtendedSelect: boolean);
------------------------------------------------------------------------------}
procedure TGtk2WidgetSet.SetSelectionMode(Sender: TObject; Widget: PGtkWidget;
  MultiSelect, ExtendedSelect: boolean);
var
  AControl: TWinControl;
  SelectionMode: TGtkSelectionMode;
  Selection : PGtkTreeSelection;
begin
  AControl:=TWinControl(Sender);
  if (AControl is TWinControl) and
    (AControl.fCompStyle in [csListBox, csCheckListBox, csCListBox]) then
  begin
    if MultiSelect then
      SelectionMode:= GTK_SELECTION_MULTIPLE
    else
      SelectionMode:= GTK_SELECTION_SINGLE;
      
    case AControl.fCompStyle of

    csListBox, csCheckListBox:
      begin
        Selection := gtk_tree_view_get_selection(GTK_TREE_VIEW(
           GetWidgetInfo(Widget, True)^.CoreWidget));
        gtk_tree_selection_set_mode(Selection, SelectionMode);
      end;
     else
       inherited SetSelectionMode(Sender, Widget, MultiSelect, ExtendedSelect);
    end;
  end;
end;

procedure TGtk2WidgetSet.SetWidgetFont(const AWidget: PGtkWidget; const AFONT: tFont);
var
  FontDesc: PPangoFontDescription;
  UseFont: PPangoLayout;
begin
  if GtkWidgetIsA(AWidget,GTKAPIWidget_GetType) then begin
    // the GTKAPIWidget is self drawn, so no use to change the widget style.
    exit;
  end;


  UseFont := PGdiObject(AFont.Handle)^.GDIFontObject;
  FontDesc := pango_layout_get_font_description(UseFont);
  gtk_widget_modify_font(AWidget, FontDesc);
end;

(*
{------------------------------------------------------------------------------
  function TGtk2WidgetSet.SetTopIndex(Sender: TObject; NewTopIndex: integer
    ): integer;
------------------------------------------------------------------------------}
function TGtk2WidgetSet.SetTopIndex(Sender: TObject; NewTopIndex: integer
  ): integer;
var
  aTreeView: PGtkTreeView;
  aTreeModel  : PGtkTreeModel;
  aTreeColumn  : PGtkTreeViewColumn;
  aTreeIter   : TGtkTreeIter;
  aTreePath   : PGtkTreePath;
  Count : Integer;
begin
  Result:=0;
  if not (Sender is TWinControl) then exit;
  case TWinControl(Sender).fCompStyle of

  csListBox, csCheckListBox:
    begin
      aTreeView := GTK_TREE_VIEW(GetWidgetInfo(Pointer(TWinControl(Sender).Handle), True)^.CoreWidget);
      aTreeModel := gtk_tree_view_get_model(aTreeView);

      If NewTopIndex < 0 then
        NewTopIndex := 0
      else begin
        Count := gtk_tree_model_iter_n_children(aTreeModel,nil);

        If NewTopIndex >= Count then
          NewTopIndex := Count - 1;
      end;
      
      if gtk_tree_model_iter_nth_child(aTreeModel,@aTreeIter, nil, NewTopIndex) then begin
        aTreePath := gtk_tree_model_get_path(aTreeModel, @aTreeIter);
        aTreeColumn := gtk_tree_view_get_column(aTreeView, 0);
        gtk_tree_view_scroll_to_cell(aTreeView, aTreePath, aTreeColumn, False, 0.0, 0.0);
        gtk_tree_path_free(aTreePath);
      end;
    end;
  end;
end;
*)

{------------------------------------------------------------------------------
  Function: TGtk2WidgetSet.InitializeOpenDialog
  Params: OpenDialog: TOpenDialog; SelWidget: PGtkWidget
  Returns: -

  Adds some functionality to a gtk file selection dialog.
  - multiselection
  - range selection
  - close on escape
  - file information
  - history pulldown
  - filter pulldown
  - preview control

  requires: gtk+ 2.6
 ------------------------------------------------------------------------------}
procedure TGtk2WidgetSet.InitializeOpenDialog(OpenDialog: TOpenDialog;
  SelWidget: PGtkWidget);
var
  FileSelWidget: PGtkFileChooser;
  HelpButton: PGtkWidget;
  InitialFilename: String;
  //FrameWidget: PGtkWidget;
  //HBox: PGtkWidget;
  //FileDetailLabel: PGtkWidget;
begin
  FileSelWidget := GTK_FILE_CHOOSER(SelWidget);

  // Help button
  if (ofShowHelp in OpenDialog.Options) then begin
    HelpButton := gtk_dialog_add_button(FileSelWidget, GTK_STOCK_HELP, GTK_RESPONSE_NONE);

    g_signal_connect( gtk_object(HelpButton),
      'clicked', gtk_signal_func(@gtkDialogHelpclickedCB), OpenDialog);
  end;

  if ofAllowMultiSelect in OpenDialog.Options then
    gtk_file_chooser_set_select_multiple(FileSelWidget, True);

  // History List - a frame with an option menu
  CreateOpenDialogHistory(OpenDialog, SelWidget);
  
  // Filter
  CreateOpenDialogFilter(OpenDialog,SelWidget);

  // connect change event
  g_signal_connect( gtk_object(FileSelWidget),
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
    CreatePreviewDialogControl(TPreviewFileDialog(OpenDialog), SelWidget);

  // set initial filename (gtk expects an absolute filename)
  InitialFilename:=TrimFilename(OpenDialog.Filename);
  if InitialFilename<>'' then begin
    if (not FilenameIsAbsolute(InitialFilename))
    and (OpenDialog.InitialDir<>'') then
      InitialFilename:=TrimFilename(OpenDialog.InitialDir+PathDelim+InitialFilename);
    if not FilenameIsAbsolute(InitialFilename) then
      InitialFilename:=CleanAndExpandFilename(InitialFilename);
    gtk_file_chooser_set_filename(FileSelWidget,PChar(InitialFilename));
  end;

  //if InitialFilter <> 'none' then
  //  PopulateFileAndDirectoryLists(FileSelWidget, InitialFilter);
end;

{------------------------------------------------------------------------------
  Function: TGtk2WidgetSet.InitializeFileDialog
  Params: FileDialog: TFileDialog; var SelWidget: PGtkWidget
  Returns: -

  Creates a new TFile/Open/SaveDialog
  requires: gtk+ 2.6
 ------------------------------------------------------------------------------}
procedure TGtk2WidgetSet.InitializeFileDialog(FileDialog: TFileDialog;
  var SelWidget: PGtkWidget; Title: PChar);
var
  Action: TGtkFileChooserAction; 
  Button1: String;
begin
  Action := GTK_FILE_CHOOSER_ACTION_OPEN;
  Button1 := GTK_STOCK_OPEN;

  if (FileDialog is TSaveDialog) or (FileDialog is TSavePictureDialog) then begin
    Action := GTK_FILE_CHOOSER_ACTION_SAVE;
    Button1 := GTK_STOCK_SAVE;
  end
  else if FileDialog is TSelectDirectoryDialog then begin
    Action := GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER;
    Button1 := GTK_STOCK_OPEN;
  end;

  SelWidget := gtk_file_chooser_dialog_new(Title, nil, Action,
    PChar(GTK_STOCK_CANCEL), [GTK_RESPONSE_CANCEL, PChar(Button1), GTK_RESPONSE_OK, nil]);

  g_signal_connect(SelWidget, 'response', gtk_signal_func(@Gtk2FileChooserResponseCB), FileDialog);
  g_signal_connect(SelWidget, 'notify', gtk_signal_func(@Gtk2FileChooserNotifyCB), FileDialog);

(*gtk 2.8 
  if FileDialog is TSaveDialog then begin
    gtk_file_chooser_set_do_overwrite_confirmation(SelWidget,  
      ofOverwritePrompt in TOpenDialog(theDialog).Options);
  end;
*)
  if FileDialog.InitialDir <> '' then
    gtk_file_chooser_set_current_folder (SelWidget, pgChar( FileDialog.InitialDir) );

  if FileDialog is TOpenDialog then
    InitializeOpenDialog(TOpenDialog(FileDialog), SelWidget);

  if FileDialog.InheritsFrom(TSaveDialog) then begin
    if FileDialog.InitialDir <> '' then
      gtk_file_chooser_set_current_folder (SelWidget, PChar(FileDialog.InitialDir));
  end;

  if gtk_file_chooser_get_action(SelWidget) in
    [GTK_FILE_CHOOSER_ACTION_SAVE,GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER]
  then
    gtk_file_chooser_set_current_name (SelWidget, pgChar( FileDialog.FileName) );

  InitializeCommonDialog(TCommonDialog(FileDialog), SelWidget);
end;

function TGtk2WidgetSet.CreateOpenDialogFilter(OpenDialog: TOpenDialog;
  SelWidget: PGtkWidget): string;
var
  FilterList: TFPList;
  i, j, k: integer;
  GtkFilter: PGtkFileFilter;
  MaskList: TStringList;
begin
  ExtractFilterList(OpenDialog.Filter, FilterList, false);
  if FilterList.Count > 0 then begin
    j := 1;
    MaskList := TStringList.Create;
    MaskList.Delimiter := ';';
    for i := 0 to FilterList.Count-1 do begin
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

procedure TGtk2WidgetSet.CreatePreviewDialogControl(
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

function TGtk2WidgetSet.CreateThemeServices: TThemeServices;
begin
  Result := TGtk2ThemeServices.Create;
end;

constructor TGtk2WidgetSet.Create;
begin
  inherited Create;
  im_context:=gtk_im_multicontext_new;
  g_signal_connect (G_OBJECT (im_context), 'commit',
    G_CALLBACK (@gtk_commit_cb), nil);
end;

function TGtk2WidgetSet.CreateCListBox(Sender: TObject): PGtkWidget;
var
  ScrolledWnd: PGtkScrolledWindow absolute Result;
begin
  Result := gtk_scrolled_window_new(nil, nil);//give something just in case
  GTK_WIDGET_UNSET_FLAGS(ScrolledWnd^.hscrollbar, GTK_CAN_FOCUS);
  GTK_WIDGET_UNSET_FLAGS(ScrolledWnd^.vscrollbar, GTK_CAN_FOCUS);
  gtk_scrolled_window_set_policy(ScrolledWnd, GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_widget_show(Result);

  DebugLn('TODO: TGtkWidgetSet.CreateComponent csCListBox');
end;

function TGtk2WidgetSet.CreateComponentWidget(Sender: TObject; ACompStyle: Integer; const ACaption: String): PGtkWidget;
begin
  case ACompstyle of
    csListBox, csCheckListBox:
      Result := CreateListBox(Sender);

    csCListBox:
      Result := CreateCListBox(Sender);

  else
    Result := inherited CreateComponentWidget(Sender, ACompStyle, ACaption);
  end;
end;

function TGtk2WidgetSet.CreateListBox(Sender: TObject): PGtkWidget;
var
  ScrolledWnd: PGtkScrolledWindow absolute Result;
begin
  Result := gtk_scrolled_window_new(nil, nil);//give something just in case
  GTK_WIDGET_UNSET_FLAGS(ScrolledWnd^.hscrollbar, GTK_CAN_FOCUS);
  GTK_WIDGET_UNSET_FLAGS(ScrolledWnd^.vscrollbar, GTK_CAN_FOCUS);
  gtk_scrolled_window_set_policy(ScrolledWnd, GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_widget_show(Result);

  DebugLn('TODO: TGtkWidgetSet.CreateComponent csListBox, csCheckListBox');
end;

destructor TGtk2WidgetSet.Destroy;
begin
  g_object_unref(im_context);
  im_context:=nil;
  im_context_widget:=nil;
  inherited Destroy;
end;

function TGtk2WidgetSet.LCLPlatform: TLCLPlatform;
begin
  Result:= lpGtk2;
end;

function TGtk2WidgetSet.AppHandle: Thandle;
begin
  {$ifdef windows}
  Result := GetAppHandle;
  {$else}
  Result := inherited AppHandle;
  {$endif}
end;

{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}
