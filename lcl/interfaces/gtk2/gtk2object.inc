{******************************************************************************
                                   TGtk2WidgetSet
 ******************************************************************************

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

function GTK2KeyUpDown(Widget: PGtkWidget; Event : pgdkeventkey;
  Data: gPointer) : GBoolean; cdecl;
begin
  Result := GtkWidgetIsA(Widget,GTK_APIWIDGETCLIENT_TYPE) and
            (HandleGtkKeyUpDown(Widget, Event, Data, True) or
            HandleGtkKeyUpDown(Widget, Event, Data, False));
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

function TGtk2WidgetSet.CreateComponent(Sender : TObject): THandle;
var
  Caption : ansistring;          // the caption of "Sender"
  StrTemp : PChar;               // same as "caption" but as PChar
  TempWidget,
  TempWidget2 : PGTKWidget;      // pointer to gtk-widget (local use when neccessary)
  p          : pointer;          // ptr to the newly created GtkWidget
  CompStyle,                     // componentstyle (type) of GtkWidget which will be created
  TempInt   : Integer;           // local use when neccessary
  // - for csBitBtn
  Box       : Pointer;           // currently only used for TCustomBitBtn
  pixmapwid : pGtkWidget;        // currently only used for TCustomBitBtn
  label1    : pgtkwidget;        // currently only used for TCustomBitBtn
  SetupProps : boolean;
  liststore : PGtkListStore;
  renderer : PGtkCellRenderer;
  column : PGtkTreeViewColumn;
begin
  p := nil;
  SetupProps:= false;

  CompStyle := GetCompStyle(Sender);
  Caption   := GetCaption(Sender);

  strTemp := StrAlloc(length(Caption) + 1);
  StrPCopy(strTemp, Caption);

  case CompStyle of
  csEdit :
    begin
      p :=  gtk_entry_new();
      gtk_editable_set_editable (PGtkEditable(P), not TCustomEdit(Sender).ReadOnly);
      gtk_widget_show_all(P);
    end;

  csListBox, csCheckListBox:
    begin
      p:= gtk_scrolled_window_new(nil, nil);
      GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(p)^.hscrollbar, GTK_CAN_FOCUS);
      GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(p)^.vscrollbar, GTK_CAN_FOCUS);
      gtk_scrolled_window_set_policy(PGtkScrolledWindow(p),
                                    GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
      gtk_widget_show(p);

      if (CompStyle = csListBox) then
        liststore := gtk_list_store_new (1, [G_TYPE_STRING, nil])
      else
        liststore := gtk_list_store_new (2, [G_TYPE_BOOLEAN, G_TYPE_STRING, nil]);

      TempWidget:= gtk_tree_view_new_with_model (GTK_TREE_MODEL (liststore));
      g_object_unref (G_OBJECT (liststore));

      TempInt := 0;
      if (CompStyle = csCheckListBox) then begin
        renderer := gtk_cell_renderer_toggle_new();
        column := gtk_tree_view_column_new_with_attributes('', renderer, ['active', 0,  nil]);
         gtk_cell_renderer_toggle_set_active(GTK_CELL_RENDERER_TOGGLE(renderer), True);
        gtk_tree_view_append_column (GTK_TREE_VIEW (TempWidget), column);
        gtk_tree_view_column_set_clickable (GTK_TREE_VIEW_COLUMN (column), TRUE);
        g_signal_connect (renderer, 'toggled',
       G_CALLBACK (@gtk_clb_toggle),
       Sender);
        g_signal_connect (TempWidget, 'row_activated',
       G_CALLBACK (@gtk_clb_toggle_row_activated),
       Sender);
        TempInt := 1;
      end;

      renderer := gtk_cell_renderer_text_new();
      column := gtk_tree_view_column_new_with_attributes ('LISTITEMS', renderer, ['text', TempInt, nil]);
      gtk_tree_view_append_column (GTK_TREE_VIEW (TempWidget), column);
      gtk_tree_view_column_set_clickable (GTK_TREE_VIEW_COLUMN (column), TRUE);

      gtk_tree_view_set_headers_visible(GTK_TREE_VIEW (TempWidget), False);

      gtk_container_add(GTK_CONTAINER(p), TempWidget);
      gtk_widget_show(TempWidget);

      SetMainWidget(p, TempWidget);
      GetWidgetInfo(p, True)^.CoreWidget := TempWidget;
      if Sender is TCustomListBox then
        SetSelectionMode(Sender,p,TCustomListBox(Sender).MultiSelect,
                         TCustomListBox(Sender).ExtendedSelect);
    end;

  csMemo :
    begin
      P := gtk_scrolled_window_new(nil, nil);
      TempWidget := gtk_text_view_new();
      gtk_container_add(p, TempWidget);

      GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(p)^.hscrollbar, GTK_CAN_FOCUS);
      GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(p)^.vscrollbar, GTK_CAN_FOCUS);
      gtk_scrolled_window_set_policy(PGtkScrolledWindow(p),
                                     GTK_POLICY_AUTOMATIC,
                                     GTK_POLICY_AUTOMATIC);
      gtk_scrolled_window_set_shadow_type(PGtkScrolledWindow(p),GTK_SHADOW_IN);
      SetMainWidget(p, TempWidget);
      GetWidgetInfo(p, True)^.CoreWidget := TempWidget;

      gtk_text_view_set_editable (PGtkTextView(TempWidget), not TCustomMemo(Sender).ReadOnly);
      if TCustomMemo(Sender).WordWrap then
        gtk_text_view_set_wrap_mode(PGtkTextView(TempWidget), GTK_WRAP_WORD)
      else
        gtk_text_view_set_wrap_mode(PGtkTextView(TempWidget), GTK_WRAP_NONE);

      gtk_widget_show_all(P);

      SetupProps:= true;
    end;
    else begin
      StrDispose(StrTemp);
      Result:=Inherited CreateComponent(Sender);
      Exit;
    end;
  end; //end case
  StrDispose(StrTemp);
  FinishComponentCreate(Sender, P, SetupProps);
  Result := THandle(P);
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
begin
  if (ALCLObject is TWinControl) then
     Begin
       inherited HookSignals(AGTKObject,ALCLObject);
     End;

  if (ALCLObject is TControl) then
    Begin
      case TControl(ALCLObject).FCompStyle of
        csEdit:
        begin
          SetCallback(LM_CHANGED, AGTKObject, ALCLObject);
          SetCallback(LM_ACTIVATE, AGTKObject,ALCLObject);
          SetCallback(LM_CUTTOCLIP, AGTKObject,ALCLObject);
          SetCallback(LM_COPYTOCLIP, AGTKObject,ALCLObject);
          SetCallback(LM_PASTEFROMCLIP, AGTKObject,ALCLObject);
        end;

 csMemo:
 begin
         // SetCallback(LM_CHANGED, AGTKObject,ALCLObject);
          //SetCallback(LM_ACTIVATE, AGTKObject,ALCLObject);
          SetCallback(LM_CUTTOCLIP, AGTKObject,ALCLObject);
          SetCallback(LM_COPYTOCLIP, AGTKObject,ALCLObject);
          SetCallback(LM_PASTEFROMCLIP, AGTKObject,ALCLObject);
   //SetCallback(LM_INSERTTEXT, AGTKObject,ALCLObject);
 end;
      end; //case
    end
    else
    If (ALCLObject is TMenuItem) then
      Begin
        SetCallback(LM_ACTIVATE,AGTKObject,ALCLObject);
      end;
end;

{------------------------------------------------------------------------------
  Method: TGtk2WidgetSet.IntSendMessage3
  Params:  LM_Message - message to be processed by GTK2
           Sender     - sending control
           data       - pointer to (optional)
  Returns: depends on the message and the sender

  Processes messages from different components.

  WARNING: the result of this function sometimes is not always really an
           integer!!!!!
 ------------------------------------------------------------------------------}
(*function TGtk2WidgetSet.IntSendMessage3(LM_Message : Integer; Sender : TObject;
  data : pointer) : integer;
var
  handle      : hwnd;                  // handle of sender
  pStr        : PChar;                 // temporary string pointer, must be allocated/disposed when used!
  Widget      : PGtkWidget;            // pointer to gtk-widget (local use when neccessary)
  ChildWidget : PGtkWidget;            // generic pointer to a child gtk-widget (local use when neccessary)
  aTextIter1  : TGtkTextIter;
  aTextIter2  : TGtkTextIter;
  aTextBuffer : PGtkTextBuffer;
  aTreeSelect : PGtkTreeSelection;
  aTreeModel  : PGtkTreeModel;
  aTreeIter   : TGtkTreeIter;
  aTreePath   : PGtkTreePath;
  TempInt     : Integer;
  TempBool    : Boolean;
  list        : pGList;
begin
  Result := 0;   //default value just in case nothing sets it

  Assert(False, 'Trace:Message received');
  if Sender <> nil then
    Assert(False, Format('Trace:  [TGtk2WidgetSet.IntSendMessage3] %s --> Sent LM_Message: $%x (%s); Data: %d', [Sender.ClassName, LM_Message, GetMessageName(LM_Message), Integer(data)]));

  // The following case is now split into 2 separate parts:
  //   1st part should contain all messages which don't need the "handle" variable
  //   2nd part has to contain all parts which need the handle
  // Reason for this split are performance issues since we need RTTI to
  // retrieve the handle
{  case LM_Message of
  else}
    begin
      handle := hwnd(ObjectToGtkObject(Sender));
      {Case LM_Message of}

      LM_CLB_GETCHECKED :
      begin
        Result := 0;
        if Assigned(Data)
        and (Sender is TControl)
        and (TControl(Sender).fCompStyle = csCheckListBox)
        then begin
          { Get the child in question of that index }
          Widget := GetWidgetInfo(Pointer(Handle),True)^.CoreWidget;
          aTreeModel := gtk_tree_view_get_model(GTK_TREE_VIEW(Widget));

          if (aTreeModel <> nil) and
             (gtk_tree_model_iter_nth_child(aTreeModel,@aTreeIter, nil, Integer(Data^)))
          then begin
             gtk_tree_model_get (aTreeModel, @aTreeIter, [0, @TempBool, -1]);
             if TempBool then
               result := 1;
          end;
        end;
      end;

      LM_CLB_SETCHECKED :
      begin
        if Assigned(Data)
        and (Sender is TControl)
        and (TControl(Sender).fCompStyle = csCheckListBox)
        then begin
          { Get the child in question of that index }
          Widget := GetWidgetInfo(Pointer(Handle),True)^.CoreWidget;
          aTreeModel := gtk_tree_view_get_model(GTK_TREE_VIEW(Widget));

          if (aTreeModel <> nil) and
             (gtk_tree_model_iter_nth_child(aTreeModel,@aTreeIter, nil, Integer(Data^)))
          then
              gtk_list_store_set(GTK_LIST_STORE(aTreeModel), @aTreeIter, [0, TLMSetChecked(Data^).Checked, -1]);
        end;
      end;

      LM_GETITEMINDEX :
      begin
        case TControl(Sender).fCompStyle of
           csListBox, csCheckListBox:
             begin
               if Handle<>0 then begin
                 Result:= -1;
                 Widget := GetWidgetInfo(Pointer(Handle),True)^.CoreWidget;
                 aTreeSelect := gtk_tree_view_get_selection(GTK_TREE_VIEW(Widget));
                 
                 {$IfNDef GTK2_2}
                  list := nil;
                  gtk_tree_selection_selected_foreach(aTreeSelect, TGtkTreeSelectionForeachFunc(@gtkTreeSelectionGetSelectedRows), @List);
                 {$Else}
                  list := gtk_tree_selection_get_selected_rows(aTreeSelect, aTreeModel);
                 {$EndIf}
                  if not Assigned(List) then begin
                    result := -1;
                    exit;
                  end;
                  
                  List := g_list_last(List);

                  aTreePath := PGtkTreePath(List^.Data);
                  Result := gtk_tree_path_get_indices(aTreePath)[0];

                  List := g_list_first(List);

                  g_list_foreach (list, TGFunc(@gtk_tree_path_free), nil);
                  g_list_free (list);
               end else
                 Result:=-1;
             end;
            else begin
              result := inherited  IntSendMessage3(LM_Message, Sender, data);
              exit;
            end;
        end;
      end;

      LM_GETITEMS :
      begin
        case TControl(Sender).fCompStyle of
          csComboBox:
            Result:=longint(gtk_object_get_data(PGtkObject(Handle),'LCLList'));

          csCheckListBox, csListBox:
            begin
              if TControl(Sender).fCompStyle = csCheckListBox then
                TempInt := 1
              else
                TempInt := 0;
              Widget:= GetWidgetInfo(Pointer(Handle), True)^.CoreWidget;
              aTreeModel := gtk_tree_view_get_model(GTK_TREE_VIEW(Widget));
              Data:= TGtkListStoreStringList.Create(GTK_LIST_STORE(aTreeModel),
                     TempInt, TWinControl(Sender));
              if Sender is TCustomListBox then
                TGtkListStoreStringList(Data).Sorted:=TCustomListBox(Sender).Sorted;
              Result:= Integer(Data);
            end;
        else
          raise Exception.Create('Message LM_GETITEMS - Not implemented');
        end;
      end;

      LM_GETSEL :
      begin
        Result := 0;      { assume: nothing found }
        if (Sender is TControl)
        and Assigned (data)
        then case TControl(Sender).fCompStyle of
          csListBox, csCheckListBox:
            begin
              { Get the child in question of that index }
              Widget := GetWidgetInfo(Pointer(Handle),True)^.CoreWidget;
              aTreeModel := gtk_tree_view_get_model(GTK_TREE_VIEW(Widget));
              aTreeSelect := gtk_tree_view_get_selection(GTK_TREE_VIEW(Widget));

              if (aTreeModel <> nil) and (aTreeSelect <> nil) and
                 (gtk_tree_model_iter_nth_child(aTreeModel,@aTreeIter, nil, Integer(Data^))) and
                 (gtk_tree_selection_iter_is_selected(aTreeSelect, @aTreeIter))
              then
                result := 1;
            end;
            else begin
              result := inherited  IntSendMessage3(LM_Message, Sender, data);
              exit;
            end;
          end;
      end;

      LM_GETSELSTART :
      begin
        if (Sender is TControl) then begin
          case TControl(Sender).fCompStyle of
            csMemo:
              begin
             Widget:= GetWidgetInfo(Pointer(Handle), true)^.CoreWidget;
                aTextBuffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(Widget));
                gtk_text_buffer_get_selection_bounds(aTextBuffer, @aTextIter1, nil);
                result := gtk_text_iter_get_offset(@aTextIter1);
              end;
            csEdit:
              begin
             Widget:= GTK_WIDGET(Pointer(Handle));
                if not gtk_editable_get_selection_bounds(GTK_EDITABLE(Widget),@result, nil) then
                   result := gtk_editable_get_position(GTK_EDITABLE(Widget));
              end;
            else begin
              result := inherited  IntSendMessage3(LM_Message, Sender, data);
              exit;
            end;
            end
          end
        else
          Result:= 0;
      end;

      LM_GETSELCOUNT :
      begin
        case (Sender as TControl).fCompStyle of
          csListBox, csCheckListBox :
            begin
              Widget := GetWidgetInfo(Pointer(Handle),True)^.CoreWidget;
              aTreeSelect := gtk_tree_view_get_selection(GTK_TREE_VIEW(Widget));
              {$IfNDef GTK2_2}
              Result := 0;
              gtk_tree_selection_selected_foreach(aTreeSelect, TGtkTreeSelectionForeachFunc(@gtkTreeSelectionCountSelectedRows), @Result);
              {$Else}
              Result := gtk_tree_selection_count_selected_rows(aTreeSelect);
              {$EndIf}
           end;
          else begin
            result := inherited  IntSendMessage3(LM_Message, Sender, data);
            exit;
          end;
        end;
      end;

      LM_GETSELLEN :
      begin
        if (Sender is TControl) then begin
          case TControl(Sender).fCompStyle of
            csMemo:
           begin
             Widget:= GetWidgetInfo(Pointer(Handle), true)^.CoreWidget;
                aTextBuffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(Widget));
                gtk_text_buffer_get_selection_bounds(aTextBuffer, @aTextIter1, @aTextIter2);
                result:= Abs(gtk_text_iter_get_offset(@aTextIter2) - gtk_text_iter_get_offset(@aTextIter1));
         end;
            csEdit:
              begin
             Widget:= GTK_WIDGET(Pointer(Handle));
                if gtk_editable_get_selection_bounds(GTK_EDITABLE(Widget),@result, @TempInt) then
                   result := TempInt - Result
                else
                  result := 0;
              end;
            else begin
              result := inherited  IntSendMessage3(LM_Message, Sender, data);
              exit;
            end;
          end;
          end;
      end;

      LM_SETBORDER:
      begin
        if (Sender is TWinControl) then
        begin
          if (TControl(Sender).fCompStyle in [csListBox, csCListBox, csCheckListBox]) then
            begin
              Widget:= PGtkWidget(TWinControl(Sender).Handle);
              if TListBox(Sender).BorderStyle = TBorderStyle(bsSingle)
              then
                gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(Widget), GTK_SHADOW_IN)
              else
                gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(Widget), GTK_SHADOW_NONE);
            end
         end;
      end;

      LM_SETITEMINDEX:
        if Handle<>0 then begin
          case TControl(Sender).fCompStyle of

            csListBox, csCheckListBox:
            begin
              Widget := GetWidgetInfo(Pointer(Handle), True)^.CoreWidget;
              aTreeModel := gtk_tree_view_get_model(GTK_TREE_VIEW(Widget));
              aTreeSelect := gtk_tree_view_get_selection(GTK_TREE_VIEW(Widget));

              if (aTreeModel <> nil) and (aTreeSelect <> nil) then begin
                if (Integer(Data)>=0) and
                  (gtk_tree_model_iter_nth_child(aTreeModel,@aTreeIter, nil, Integer(Data)))
                then
                  gtk_tree_selection_select_iter(aTreeSelect, @aTreeIter)
                else
                  gtk_tree_selection_unselect_all(aTreeSelect);
              end;
            end;
            
            else begin
              result := inherited  IntSendMessage3(LM_Message, Sender, data);
              exit;
            end;
          end;
        end;

      LM_SETSEL:
      begin
        if (Sender is TControl)
        and Assigned (data)
        then case TControl(Sender).fCompStyle of
          csListBox, csCheckListBox:
            begin
              Widget := GetWidgetInfo(Pointer(Handle), True)^.CoreWidget;
              aTreeModel := gtk_tree_view_get_model(GTK_TREE_VIEW(Widget));
              aTreeSelect := gtk_tree_view_get_selection(GTK_TREE_VIEW(Widget));

              if (aTreeModel <> nil) and (aTreeSelect <> nil) and
                 (gtk_tree_model_iter_nth_child(aTreeModel,@aTreeIter, nil, TLMSetSel(Data^).Index))
              then
                if TLMSetSel(Data^).Selected then
                  gtk_tree_selection_select_iter(aTreeSelect, @aTreeIter)
                else
                  if gtk_tree_selection_iter_is_selected(aTreeSelect, @aTreeIter) then
                    gtk_tree_selection_unselect_iter(aTreeSelect, @aTreeIter);
            end;
            else begin
              result := inherited  IntSendMessage3(LM_Message, Sender, data);
              exit;
            end;
        end;
      end;

      LM_SETSELSTART:
      begin
        if (Sender is TControl) then begin
          case TControl(Sender).fCompStyle of
            csMemo:
              begin
             Widget:= GetWidgetInfo(Pointer(Handle), true)^.CoreWidget;
                aTextBuffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(Widget));
                Debugln('TODO(GTK2): IntSendMessage3, LM_SETSELSTART, csMemo');
                {gtk_text_buffer_get_selection_bounds(aTextBuffer, @aTextIter1, nil);
                result := gtk_text_iter_get_offset(@aTextIter1);}
              end;

            csEdit:
              begin
             Widget:= GTK_WIDGET(Pointer(Handle));
                if gtk_editable_get_selection_bounds(GTK_EDITABLE(Widget),nil, @TempInt) then
                  If (Integer(Data) >= 0) and (Integer(Data)<=TempInt) then
                    gtk_editable_select_region(GTK_EDITABLE(Widget), Integer(Data), TempInt+1);
              end;

            else begin
              result := inherited  IntSendMessage3(LM_Message, Sender, data);
              exit;
            end;
          end;
        end
        else
          Result:= 0;
      end;

      LM_SETSELLEN :
      begin
        if (Sender is TControl) then begin
          case TControl(Sender).fCompStyle of
            csMemo:
           begin
             Widget:= GetWidgetInfo(Pointer(Handle), true)^.CoreWidget;
                aTextBuffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(Widget));
                Debugln('TODO(GTK2): IntSendMessage3, LM_SETSELLEN, csMemo');
                {gtk_text_buffer_get_selection_bounds(aTextBuffer, @aTextIter1, @aTextIter2);
                result:= Abs(gtk_text_iter_get_offset(@aTextIter2) - gtk_text_iter_get_offset(@aTextIter1));}
         end;

            csEdit:
              begin
             Widget:= GTK_WIDGET(Pointer(Handle));
                if gtk_editable_get_selection_bounds(GTK_EDITABLE(Widget),@TempInt, nil) then
                  gtk_editable_select_region(GTK_EDITABLE(Widget), TempInt, TempInt+Integer(Data)+1)
                else
                  gtk_editable_select_region(GTK_EDITABLE(Widget), gtk_editable_get_position(GTK_EDITABLE(Widget)),
                    gtk_editable_get_position(GTK_EDITABLE(Widget)) + Integer(Data)+1)
              end;

            else begin
              result := inherited  IntSendMessage3(LM_Message, Sender, data);
              exit;
            end;
          end;
        end;
      end;

      LM_SORT:
      begin
        if  (Sender is TControl) and assigned (data) then
        begin
          case TControl(Sender).fCompStyle of
            csListBox,
            csCheckListBox
                     : TGtkListStoreStringList(TLMSort(Data^).List).Sorted:=
                              TLMSort(Data^).IsSorted;
            else begin
              result := inherited  IntSendMessage3(LM_Message, Sender, data);
              exit;
            end;
          end
        end
      end;
      
      else begin
        result := inherited  IntSendMessage3(LM_Message, Sender, data);
        exit;
      end; // end of else-part of 2nd case
      {end;} // end of 2nd case
    end;   // end of else-part of 1st case
//  end;   // end of 1st case
end;*)

{------------------------------------------------------------------------------
  Function: TGtk2WidgetSet.SetCallback
  Params: Msg - message for which to set a callback
          sender - object to which callback will be send
  Returns:  nothing

  Applies a Message to the sender
 ------------------------------------------------------------------------------}
procedure TGTK2WidgetSet.SetCallback(const AMsg: LongInt; const AGTKObject: PGTKObject; const ALCLObject: TObject);

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
    ConnectSenderSignal(AnObject, 'focus-in-event', @gtk2FocusCB);
    ConnectSenderSignalAfter(AnObject, 'focus-in-event', @gtk2FocusCBAfter);
    ConnectSenderSignal(AnObject, 'focus-out-event', @gtk2KillFocusCB);
    ConnectSenderSignalAfter(AnObject, 'focus-out-event', @gtk2KillFocusCBAfter);
  end;

  procedure ConnectKeyPressReleaseEvents(const AnObject: PGTKObject);
  begin
    ConnectSenderSignal(AnObject,
      'key-press-event', @GTK2KeyUpDown, GDK_KEY_PRESS_MASK);
    //ConnectSenderSignalAfter(AnObject,
    //  'key-press-event', @GTK2KeyUpDownAfter, GDK_KEY_PRESS_MASK);
    ConnectSenderSignal(AnObject,
      'key-release-event', @GTK2KeyUpDown, GDK_KEY_RELEASE_MASK);
    //ConnectSenderSignalAfter(AnObject,
    //  'key-release-event', @GTK2KeyUpDownAfter, GDK_KEY_RELEASE_MASK);
  end;

var
  gObject, gFixed, gCore: PGTKObject;
begin
  gObject := AGTKObject;
  if gObject = nil then Exit;

  // gFixed is the widget with the client area (e.g. TGroupBox, TForm have this)
  gFixed := PGTKObject(GetFixedWidget(gObject));
  if gFixed = nil then gFixed := gObject;

  // gCore is the main widget (e.g. TListView has this)
  gCore:= PGtkObject(GetWidgetInfo(gObject, True)^.CoreWidget);

  case AMsg of
    LM_FOCUS :
    begin
      if (ALCLObject is TCustomComboBox) then begin
        ConnectFocusEvents(PgtkObject(PgtkCombo(gObject)^.entry));
        ConnectFocusEvents(PgtkObject(PgtkCombo(gObject)^.list));
      end else begin
        ConnectFocusEvents(gCore);
      end;
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
    Inherited SetCallback(AMsg, AGTKObject, ALCLObject);
  end;
end;

Function TGtk2WidgetSet.LoadStockPixmap(StockID: longint) : HBitmap;
var
  Pixmap : PGDIObject;
  StockName : PChar;
  IconSet : PGtkIconSet;
  Pixbuf : PGDKPixbuf;
begin
  Case StockID Of
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
      Result := inherited LoadStockPixmap(StockID);
      exit;
    end;
  end;

  if (StockID >= idButtonBase) and (StockID <= idDialogBase) then
    IconSet := gtk_style_lookup_icon_set(GetStyle(lgsButton), StockName)
  else
    IconSet := gtk_style_lookup_icon_set(GetStyle(lgsWindow), StockName);

  If (IconSet = nil) then begin
    Result := inherited LoadStockPixmap(StockID);
    exit;
  end;

  if (StockID >= idButtonBase) and (StockID <= idDialogBase) then
    pixbuf := gtk_icon_set_render_icon(IconSet, GetStyle(lgsbutton), GTK_TEXT_DIR_NONE, GTK_STATE_NORMAL, GTK_ICON_SIZE_BUTTON, GetStyleWidget(lgsbutton), nil)
  else
    pixbuf := gtk_icon_set_render_icon(IconSet, GetStyle(lgswindow), GTK_TEXT_DIR_NONE, GTK_STATE_NORMAL, GTK_ICON_SIZE_DIALOG, GetStyleWidget(lgswindow), nil);

  Pixmap := NewGDIObject(gdiBitmap);
  With Pixmap^ do begin
    GDIBitmapType := gbPixmap;
    visual := gdk_visual_get_system();
    gdk_visual_ref(visual);
    colormap := gdk_colormap_get_system();
    gdk_colormap_ref(colormap);
    gdk_pixbuf_render_pixmap_and_mask(pixbuf, GDIPixmapObject, GDIBitmapMaskObject, 128);
  end;

  gdk_pixbuf_unref(pixbuf);
  Result := HBitmap(Pixmap);
end;

(*
{------------------------------------------------------------------------------
  Method: TGtk2WidgetSet.SetLabel
  Params:  sender - the calling object
           data   - String (PChar) to be set as label for a control
  Returns: Nothing

  Sets the label text on a widget
 ------------------------------------------------------------------------------}
procedure TGtk2WidgetSet.SetLabel(Sender : TObject; Data : Pointer);
var
  Widget : PGtkWidget;
  aTextBuffer : PGtkTextBuffer;
  aTextIter1  : TGtkTextIter;
  aTextIter2  : TGtkTextIter;

  DC : hDC;
  aLabel, pLabel: pchar;
  AccelKey : integer;
begin
  if Sender is TMenuItem then begin
    //inherited SetLabel(Sender, Data);
    exit;
  end;

  if Sender is TWinControl
  then Assert(False, Format('Trace:  [TGtk2WidgetSet.SetLabel] %s --> label %s', [Sender.ClassName, TControl(Sender).Caption]))
  else begin
    Assert(False, Format('Trace:WARNING: [TGtk2WidgetSet.SetLabel] %s --> No Decendant of TWinControl', [Sender.ClassName]));
    RaiseException('[TGtk2WidgetSet.SetLabel] ERROR: Sender ('+Sender.Classname+')'
        +' is not TWinControl ');
  end;

  Widget := PGtkWidget(TWinControl(Sender).Handle);
  Assert(Widget = nil, 'Trace:WARNING: [TGtk2WidgetSet.SetLabel] --> got nil pointer');
  Assert(False, 'Trace:Setting Str1 in SetLabel');
  pLabel := pchar(Data);

  case TControl(Sender).fCompStyle of
  csEdit        : begin
                    gtk_entry_set_text(pGtkEntry(Widget), pLabel);
                    {LockOnChange(PGtkObject(Widget),+1);
                    gtk_editable_delete_text(pGtkEditable(P), 0, -1);
                    gtk_editable_insert_text(pGtkEditable(P), pLabel, StrLen(pLabel). 0);
                    LockOnChange(PGtkObject(Widget),-1);}
                  end;

  csLabel:
    begin
      gtk_label_set_use_underline(PGtkLabel(Widget), True);
      if TLabel(Sender).ShowAccelChar then begin
        If TLabel(sender).WordWrap and (TLabel(Sender).Caption<>'') then begin
          DC := GetDC(TLabel(Sender).Handle);
          aLabel := ForceLineBreaks(DC, pLabel, TLabel(Sender).Width, True);
          DeleteDC(DC);
        end
        else
          aLabel:= Ampersands2Underscore(pLabel);

        try
          gtk_label_set_label(pGtkLabel(Widget), aLabel);
          AccelKey:= gtk_label_get_mnemonic_keyval(pGtkLabel(Widget));
          Accelerate(TComponent(Sender),Widget,AccelKey,0,'grab_focus');
        finally
   StrDispose(aLabel);
 end;

      end else begin
        If TLabel(sender).WordWrap then begin
          DC := GetDC(TLabel(Sender).Handle);
          aLabel := ForceLineBreaks(DC, pLabel, TLabel(Sender).Width, False);
          gtk_label_set_label(PGtkLabel(Widget), aLabel);
   StrDispose(aLabel);
          DeleteDC(DC);
        end
        else
          gtk_label_set_label(PGtkLabel(Widget), pLabel);
      end;
    end;

  csMemo        : begin
                    Widget:= PGtkWidget(GetWidgetInfo(Widget, True)^.CoreWidget);
                    aTextBuffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(Widget));
                    gtk_text_buffer_begin_user_action(aTextBuffer);
                    gtk_text_buffer_get_bounds(aTextBuffer, @aTextIter1, @aTextIter2);
                    gtk_text_buffer_delete(aTextBuffer, @aTextIter1, @aTextIter2);
                    gtk_text_buffer_get_bounds(aTextBuffer, @aTextIter1, @aTextIter2);
                    gtk_text_buffer_insert(aTextBuffer, @aTextIter1, pLabel, StrLen(pLabel));
                    gtk_text_buffer_end_user_action(aTextBuffer);
                  end;
  {else
    inherited SetLabel(Sender, Data);}
  end;
  Assert(False, Format('trace:  [TGtk2WidgetSet.SetLabel] %s --> END', [Sender.ClassName]));
end;
*)


{------------------------------------------------------------------------------
  Method: TGtk2WidgetSet.SetProperties
  Params:  Sender : the lcl object which called this func via SenMessage
  Returns: currently always 0

  Depending on the compStyle, this function will apply all properties of
  the calling object to the corresponding GTK2 object.
 ------------------------------------------------------------------------------}
(*function TGtk2WidgetSet.SetProperties(Sender : TObject) : integer;
const
  cLabelAlignX : array[TAlignment] of gfloat = (0.0, 1.0, 0.5);
  cLabelAlignY : array[TTextLayout] of gfloat = (0.0, 0.5, 1.0);
  cLabelAlign : array[TAlignment] of TGtkJustification = (GTK_JUSTIFY_LEFT, GTK_JUSTIFY_RIGHT, GTK_JUSTIFY_CENTER);
var
  wHandle    : Pointer;
  Widget, ImplWidget   : PGtkWidget;
  i : Longint;
  aTextBuffer : PGtkTextBuffer;
  aTextIter1  : TGtkTextIter;
  aTextIter2  : TGtkTextIter;
begin
  Result := 0;     // default if nobody sets it

  if Sender is TWinControl
  then
    Assert(False, Format('Trace:  [TGtk2WidgetSet.SetProperties] %s', [Sender.ClassName]))
  else
    RaiseException('TGtk2WidgetSet.SetProperties: '
                   +' Sender.ClassName='+Sender.ClassName);

  wHandle:= Pointer(TWinControl(Sender).Handle);
  Widget:= GTK_WIDGET(wHandle);

  case TControl(Sender).fCompStyle of
  csEdit :
    with TCustomEdit(Sender) do
      begin
        gtk_editable_set_editable(GTK_ENTRY(wHandle), not (TCustomEdit(Sender).ReadOnly));
        gtk_entry_set_max_length(GTK_ENTRY(wHandle), TCustomEdit(Sender).MaxLength);
        gtk_entry_set_visibility(GTK_ENTRY(wHandle), (TCustomEdit(Sender).EchoMode = emNormal) and (TCustomEdit(Sender).PassWordChar=#0));
        if (TCustomEdit(Sender).EchoMode = emNone) then
          gtk_entry_set_invisible_char(GTK_ENTRY(wHandle), 0)
        else
          gtk_entry_set_invisible_char(GTK_ENTRY(wHandle), Longint(TCustomEdit(Sender).PassWordChar));
      end;

  csLabel :
     with TLabel(Sender) do
     begin
       gtk_label_set_line_wrap(GTK_LABEL(wHandle), False);
       gtk_misc_set_alignment(GTK_MISC(wHandle), cLabelAlignX[Alignment],
                              0.0);
       gtk_label_set_line_wrap(GTK_LABEL(wHandle), TRUE);
       gtk_label_set_justify(GTK_LABEL(wHandle), cLabelAlign[Alignment]);
     end;

  csMemo:
    begin
      ImplWidget:= GetWidgetInfo(wHandle, true)^.CoreWidget;

      gtk_text_view_set_editable (PGtkTextView(ImplWidget), not TCustomMemo(Sender).ReadOnly);
      if TCustomMemo(Sender).WordWrap then
        gtk_text_view_set_wrap_mode(PGtkTextView(ImplWidget), GTK_WRAP_WORD)
      else
        gtk_text_view_set_wrap_mode(PGtkTextView(ImplWidget), GTK_WRAP_NONE);


      case (Sender as TCustomMemo).Scrollbars of
        ssHorizontal:     gtk_scrolled_window_set_policy(
                            GTK_SCROLLED_WINDOW(wHandle),
                            GTK_POLICY_ALWAYS, GTK_POLICY_NEVER);
        ssVertical:       gtk_scrolled_window_set_policy(
                            GTK_SCROLLED_WINDOW(wHandle),
                            GTK_POLICY_NEVER, GTK_POLICY_ALWAYS);
        ssBoth:           gtk_scrolled_window_set_policy(
                            GTK_SCROLLED_WINDOW(wHandle),
                            GTK_POLICY_ALWAYS, GTK_POLICY_ALWAYS);
        ssAutoHorizontal: gtk_scrolled_window_set_policy(
                            GTK_SCROLLED_WINDOW(wHandle),
                            GTK_POLICY_AUTOMATIC, GTK_POLICY_NEVER);
        ssAutoVertical:   gtk_scrolled_window_set_policy(
                            GTK_SCROLLED_WINDOW(wHandle),
                            GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
        ssAutoBoth:       gtk_scrolled_window_set_policy(
                            GTK_SCROLLED_WINDOW(wHandle),
                            GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
      else
        gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(wHandle),
                                       GTK_POLICY_NEVER, GTK_POLICY_NEVER);
      end;

      If (TCustomMemo(Sender).MaxLength >= 0) then begin
          aTextBuffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(ImplWidget));
   i:= gtk_text_buffer_get_char_count(aTextBuffer);
   if i > TCustomMemo(Sender).MaxLength then begin
             gtk_text_buffer_get_bounds(aTextBuffer, nil, @aTextIter2);
             gtk_text_buffer_get_iter_at_offset(aTextBuffer, @aTextIter1, i);
             gtk_text_buffer_delete(aTextBuffer, @aTextIter1, @aTextIter2);
   end;
      end;
    end;
  else
    Result := inherited SetProperties(Sender);
  end;
end; *)

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
  procedure TGtk2WidgetSet.UpdateDCTextMetric(DC: TDeviceContext);
 ------------------------------------------------------------------------------}
procedure TGtk2WidgetSet.UpdateDCTextMetric(DC: TDeviceContext);
const
  TestString = '{Am|g_}';
var
  XT : TSize;
  UseFontDesc : PPangoFontDescription;
  UnRef : Boolean;
  AVGBuffer: array[#32..#126] of char;
  AvgLen: integer;
  c: char;

  Underline,
  StrikeOut : Boolean;

  Layout : PPangoLayout;
  AttrList : PPangoAttrList;
  Attr : PPangoAttribute;
  Extents : TPangoRectangle;
begin
  with TDeviceContext(DC) do begin
    if dcfTextMetricsValid in DCFlags then begin
      // cache valid
    end else begin
      if (CurrentFont = nil) or (CurrentFont^.GDIFontObject = nil)
      then begin
        UseFontDesc := GetDefaultFontDesc(true);
        UnRef := True;

        Underline := False;
        StrikeOut := False;
      end
      else begin
        UseFontDesc := CurrentFont^.GDIFontObject;
        UnRef := False;

        Underline := CurrentFont^.Underline;
        StrikeOut := CurrentFont^.StrikeOut;
      end;
      If UseFontDesc = nil then
        DebugLn('WARNING: [TGtk2WidgetSet.GetTextMetrics] Missing font')
      else begin
        Layout := gtk_widget_create_pango_layout (GetStyleWidget(lgsdefault), nil);
        pango_layout_set_font_description(Layout, UseFontDesc);
        AttrList := pango_layout_get_attributes(Layout);

        If (AttrList = nil) then
          AttrList := pango_attr_list_new();

        If Underline then
          Attr := pango_attr_underline_new(PANGO_UNDERLINE_SINGLE)
        else
          Attr := pango_attr_underline_new(PANGO_UNDERLINE_NONE);

        pango_attr_list_change(AttrList,Attr);

        Attr := pango_attr_strikethrough_new(StrikeOut);
        pango_attr_list_change(AttrList,Attr);

        pango_layout_set_attributes(Layout, AttrList);

        pango_layout_set_single_paragraph_mode(Layout, TRUE);
        pango_layout_set_width(Layout, -1);

        pango_layout_set_alignment(Layout, PANGO_ALIGN_LEFT);

        pango_layout_set_text(Layout,  TestString, length(TestString));

        pango_layout_get_extents(Layout, nil, @Extents);
        g_object_unref(Layout);

        If UnRef then
          pango_font_description_free(UseFontDesc);

        FillChar(DCTextMetric, SizeOf(DCTextMetric), 0);
        with DCTextMetric do begin
          IsDoubleByteChar:=False;//FontIsDoubleByteCharsFont(UseFont);

          for c:=Low(AVGBuffer) to High(AVGBuffer) do
            AVGBuffer[c]:=c;
          lbearing := PANGO_LBEARING(extents) div PANGO_SCALE;
          rBearing := PANGO_RBEARING(extents) div PANGO_SCALE;
          TextMetric.tmAscent := PANGO_ASCENT(extents) div PANGO_SCALE;
          TextMetric.tmDescent := PANGO_DESCENT(extents) div PANGO_SCALE;
          AvgLen:=ord(High(AVGBuffer))-ord(Low(AVGBuffer))+1;
          GetTextExtentPoint(HDC(DC), @AVGBuffer[Low(AVGBuffer)],
                             AvgLen, XT);
          if not IsDoubleByteChar then
            XT.cX := XT.cX div AvgLen
          else
            // Quick hack for double byte char fonts
            XT.cX := XT.cX div (AvgLen div 2);
          TextMetric.tmHeight := XT.cY;
          TextMetric.tmAscent := TextMetric.tmHeight - TextMetric.tmDescent;
          TextMetric.tmAveCharWidth :=  XT.cX;
          if TextMetric.tmAveCharWidth<1 then TextMetric.tmAveCharWidth:=1;
          {temp EVIL hack FIXME -->}
          AVGBuffer[Low(AVGBuffer)]:='M';
          GetTextExtentPoint(HDC(DC), @AVGBuffer[Low(AVGBuffer)],
                             1, XT);
          TextMetric.tmMaxCharWidth := XT.cX;
          AVGBuffer[Low(AVGBuffer)]:='W';
          GetTextExtentPoint(HDC(DC), @AVGBuffer[Low(AVGBuffer)],
                             1, XT);
          TextMetric.tmMaxCharWidth := Max(TextMetric.tmMaxCharWidth,XT.cX);
          {<--  temp EVIL hack FIXME}
          if TextMetric.tmMaxCharWidth<1 then
            TextMetric.tmMaxCharWidth:=1;
        end;
      end;
      Include(DCFlags,dcfTextMetricsValid);
    end;
  end;
end;

{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}

 { =============================================================================

  $Log$
  Revision 1.21  2005/01/23 11:43:38  mattias
  fixed expandingtabs in synedit painting

  Revision 1.20  2005/01/22 23:53:43  mattias
  fixed gtk2 intf  from Peter Vreman

  Revision 1.19  2004/09/30 10:35:50  mazen
  * Fix compile probelem under GTK2 related to
    THandle <--> Pointer conversion

  Revision 1.18  2004/09/28 16:41:54  mazen
  * Fix GTK2 compilation problem

  Revision 1.17  2004/09/12 19:09:08  mazen
  * Fix compilation problem with GTK2

  Revision 1.16  2004/09/08 16:20:24  mazen
  * Commenting removed messages
  * Can someone please verify they can be removed please?

  Revision 1.15  2004/09/02 09:17:00  mattias
  improved double byte char fonts for gtk1, started synedit UTF8 support

  Revision 1.14  2004/07/30 14:26:11  mazen
  * move HandleGtkKeyUpDown to gtkProc.inc make it visible to gtk2
    this allow saving a call in a hevely called callback

  Revision 1.13  2004/07/15 10:43:39  mattias
  added TCustomButton, TCustomBitBtn, TCustomSpeedButton

  Revision 1.12  2004/05/22 14:35:33  mattias
  fixed button return key

  Revision 1.11  2004/05/11 09:49:47  mattias
  started sending CN_KEYUP

  Revision 1.10  2004/04/13 14:03:29  marc
  Patch from Ladislav Michl

  Revision 1.9  2004/03/09 15:30:15  peter
    * fixed gtk2 compilation

  Revision 1.8  2004/03/05 00:41:15  marc
  * Renamed TGtk2Object to TGtk2WidgetSet

  Revision 1.7  2004/01/04 16:44:33  mattias
  updated gtk2 package

  Revision 1.6  2003/10/04 00:36:30  ajgenius
  partly fix csLabel for GTK2, Layout is still wacked.

  Revision 1.5  2003/10/03 01:25:01  ajgenius
  add more gtk1i<->gtk2 key & event wrappers,
  move more GTK2 workarounds from gtk to gtk2 interface,
  start GTK2 interface SetCallback

  Revision 1.4  2003/10/02 18:15:44  ajgenius
  more gtk2 (check)ListBox implementation

  Revision 1.3  2003/10/02 01:18:38  ajgenius
  more callbacks fixes for gtk2, partly fix gtk2 CheckListBox

  Revision 1.2  2003/09/24 17:23:54  ajgenius
  more work toward GTK2 - partly fix CheckListBox, & MenuItems

  Revision 1.1  2003/09/22 20:08:56  ajgenius
  break GTK2 object and winapi into includes like the GTK interface


}
