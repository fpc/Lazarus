{******************************************************************************
                                   TGTK2Object
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

{------------------------------------------------------------------------------
  procedure Tgtk2Object.AppendText(Sender: TObject; Str: PChar);
 ------------------------------------------------------------------------------}
procedure Tgtk2Object.AppendText(Sender: TObject; Str: PChar);
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
      	Widget:= GetWidgetInfo(Pointer(TWinControl(Sender).Handle), True)^.ImplementationWidget;
        aTextBuffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(Widget));
        gtk_text_buffer_begin_user_action(aTextBuffer);
        gtk_text_buffer_get_bounds(aTextBuffer, @aTextIter1, @aTextIter2);
        gtk_text_buffer_insert(aTextBuffer, @aTextIter2, str, StrLen(str));
        gtk_text_buffer_end_user_action(aTextBuffer);
      end;
      else
        inherited AppendText(Sender, Str);
    end;
  end;
end;

procedure Tgtk2Object.CreateComponent(Sender : TObject);
var
  Caption : ansistring;          // the caption of "Sender"
  StrTemp : PChar;               // same as "caption" but as PChar
  TempWidget,
  TempWidget2 : PGTKWidget;      // pointer to gtk-widget (local use when neccessary)
  p          : pointer;          // ptr to the newly created GtkWidget
  CompStyle,                     // componentstyle (type) of GtkWidget which will be created
  TempInt   : Integer;           // local use when neccessary
  // - for csBitBtn
  Box       : Pointer;           // currently only used for TBitBtn
  pixmapwid : pGtkWidget;        // currently only used for TBitBtn
  label1    : pgtkwidget;        // currently only used for TBitBtn
  ParentForm: TCustomForm;
  AccelText : PChar;
  AccelKey  : guint;
  SetupProps : boolean;
  AWindow: PGdkWindow;
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
        liststore := gtk_list_store_new (1, [G_TYPE_BOOLEAN, nil]);

      TempWidget:= gtk_tree_view_new_with_model (GTK_TREE_MODEL (liststore));
      g_object_unref (G_OBJECT (liststore));

      if (CompStyle = csListBox) then begin
        renderer := gtk_cell_renderer_text_new();
        column := gtk_tree_view_column_new_with_attributes ('LISTITEMS', renderer, ['text', 0, nil]);
      end
      else begin
        renderer := gtk_cell_renderer_toggle_new();
        column := gtk_tree_view_column_new_with_attributes ('CHECKLISTITEMS', renderer, ['active', 0, nil]);
      end;

      gtk_tree_view_append_column (GTK_TREE_VIEW (TempWidget), column);
      gtk_tree_view_set_headers_visible(GTK_TREE_VIEW (TempWidget), False);

      gtk_scrolled_window_add_with_viewport(PGtkScrolledWindow(p), TempWidget);
      gtk_container_set_focus_vadjustment(PGtkContainer(TempWidget),
                   gtk_scrolled_window_get_vadjustment(PGtkScrolledWindow(p)));
      gtk_container_set_focus_hadjustment(PGtkContainer(TempWidget),
                   gtk_scrolled_window_get_hadjustment(PGtkScrolledWindow(p)));
      gtk_widget_show(TempWidget);

      SetMainWidget(p, TempWidget);
      GetWidgetInfo(p, True)^.ImplementationWidget := TempWidget;
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
      GetWidgetInfo(p, True)^.ImplementationWidget := TempWidget;

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
      Inherited CreateComponent(Sender);
      Exit;
    end;
  end; //end case
  StrDispose(StrTemp);
  FinishComponentCreate(Sender, P, SetupProps);
end;

function TGtk2Object.GetText(Sender: TComponent; var Text: String): Boolean;
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
      	          Widget:= GetWidgetInfo(Pointer(TWinControl(Sender).Handle), True)^.ImplementationWidget;
                  aTextBuffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(Widget));
                  gtk_text_buffer_get_bounds(aTextBuffer, @aTextIter1, @aTextIter2);
                  CS := gtk_text_buffer_get_text(aTextBuffer, @aTextIter1, @aTextIter2, True);
                  Text := StrPas(CS);
                  g_free(CS);
               end;
  else
    Result := inherited GetText(Sender, Text);
  end;
end;

procedure Tgtk2Object.HookSignals(Sender: TObject);
begin
  if (Sender is TWinControl) then
     Begin
       inherited HookSignals(Sender);
     End;

  if (Sender is TControl) then
    Begin
      case TControl(sender).FCompStyle of
        csEdit:
        begin
          SetCallback(LM_CHANGED, Sender);
          SetCallback(LM_ACTIVATE, Sender);
          SetCallback(LM_CUTTOCLIP, Sender);
          SetCallback(LM_COPYTOCLIP, Sender);
          SetCallback(LM_PASTEFROMCLIP, Sender);
        end;

	csMemo:
	begin
          SetCallback(LM_CHANGED, Sender);
          SetCallback(LM_ACTIVATE, Sender);
          SetCallback(LM_CUTTOCLIP, Sender);
          SetCallback(LM_COPYTOCLIP, Sender);
          SetCallback(LM_PASTEFROMCLIP, Sender);
	  SetCallback(LM_INSERTTEXT, Sender);
	end;
      end; //case
    end;
end;

{------------------------------------------------------------------------------
  Method: TGtk2Object.IntSendMessage3
  Params:  LM_Message - message to be processed by GTK2
           Sender     - sending control
           data       - pointer to (optional)
  Returns: depends on the message and the sender

  Processes messages from different components.

  WARNING: the result of this function sometimes is not always really an
           integer!!!!!
 ------------------------------------------------------------------------------}
function Tgtk2Object.IntSendMessage3(LM_Message : Integer; Sender : TObject;
  data : pointer) : integer;
var
  handle      : hwnd;                  // handle of sender
  pStr        : PChar;                 // temporary string pointer, must be allocated/disposed when used!
  Widget      : PGtkWidget;            // pointer to gtk-widget (local use when neccessary)
  ChildWidget : PGtkWidget;            // generic pointer to a child gtk-widget (local use when neccessary)
  aTextIter1  : TGtkTextIter;
  aTextIter2  : TGtkTextIter;
  aTextBuffer : PGtkTextBuffer;
  Pos         : Integer;
begin
  Result := 0;   //default value just in case nothing sets it

  Assert(False, 'Trace:Message received');
  if Sender <> nil then
    Assert(False, Format('Trace:  [Tgtk2Object.IntSendMessage3] %s --> Sent LM_Message: $%x (%s); Data: %d', [Sender.ClassName, LM_Message, GetMessageName(LM_Message), Integer(data)]));

  // The following case is now split into 2 separate parts:
  //   1st part should contain all messages which don't need the "handle" variable
  //   2nd part has to contain all parts which need the handle
  // Reason for this split are performance issues since we need RTTI to
  // retrieve the handle
{  case LM_Message of
  else}
    begin
      handle := hwnd(ObjectToGtkObject(Sender));
      Case LM_Message of

      LM_GETITEMS :
      begin
        case TControl(Sender).fCompStyle of
          csComboBox:
            Result:=longint(gtk_object_get_data(PGtkObject(Handle),'LCLList'));

         { csCListBox:
            begin
              Widget:= GetWidgetInfo(Pointer(Handle), True)^.ImplementationWidget;

              Data := TGtkCListStringList.Create(PGtkCList(Widget));
              if Sender is TCustomListBox then
                TGtkCListStringList(Data).Sorted:=TCustomListBox(Sender).Sorted;
              Result := integer(Data);
            end;}

          csCheckListBox, csListBox:
            begin
              Widget:= GetWidgetInfo(Pointer(Handle), True)^.ImplementationWidget;
              Data:= TGtkListStoreStringList.Create(GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(Widget))),
                     TWinControl(Sender));
              if Sender is TCustomListBox then
                TGtkListStoreStringList(Data).Sorted:=TCustomListBox(Sender).Sorted;
              Result:= Integer(Data);
            end;
        else
          raise Exception.Create('Message LM_GETITEMS - Not implemented');
        end;
      end;

      LM_GETSELSTART :
      begin
        if (Sender is TControl) then begin
          case TControl(Sender).fCompStyle of
            csMemo:
              begin
    	        Widget:= GetWidgetInfo(Pointer(Handle), true)^.ImplementationWidget;
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

      LM_GETSELLEN :
      begin
        if (Sender is TControl) then begin
          case TControl(Sender).fCompStyle of
            csMemo:
    	      begin
    	        Widget:= GetWidgetInfo(Pointer(Handle), true)^.ImplementationWidget;
                aTextBuffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(Widget));
                gtk_text_buffer_get_selection_bounds(aTextBuffer, @aTextIter1, @aTextIter2);
                result:= Abs(gtk_text_iter_get_offset(@aTextIter2) - gtk_text_iter_get_offset(@aTextIter1));
  	      end;
            csEdit:
              begin
    	        Widget:= GTK_WIDGET(Pointer(Handle));
                if gtk_editable_get_selection_bounds(GTK_EDITABLE(Widget),@result, @Pos) then
                   result := Pos - Result
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
        if (Sender is TControl) then
        begin
          if (TControl(Sender).fCompStyle in [csListBox, csCListBox, csCheckListBox]) then
            begin
              {Widget:= PGtkWidget(Handle);
              if TListBox(Sender).BorderStyle = TBorderStyle(bsSingle)
              then
                gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(Widget), GTK_SHADOW_IN)
              else}
                gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(Widget), GTK_SHADOW_NONE);
            end
         end;
      end;

      LM_SETSELSTART:
      begin
        if (Sender is TControl) then begin
          case TControl(Sender).fCompStyle of
            csMemo:
              begin
    	        Widget:= GetWidgetInfo(Pointer(Handle), true)^.ImplementationWidget;
                aTextBuffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(Widget));
                Writeln('TODO(GTK2): IntSendMessage3, LM_SETSELSTART, csMemo');
                {gtk_text_buffer_get_selection_bounds(aTextBuffer, @aTextIter1, nil);
                result := gtk_text_iter_get_offset(@aTextIter1);}
              end;

            csEdit:
              begin
    	        Widget:= GTK_WIDGET(Pointer(Handle));
                if gtk_editable_get_selection_bounds(GTK_EDITABLE(Widget),nil, @Pos) then
                  If (Integer(Data) >= 0) and (Integer(Data)<=Pos) then
                    gtk_editable_select_region(GTK_EDITABLE(Widget), Integer(Data), Pos+1);
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
    	        Widget:= GetWidgetInfo(Pointer(Handle), true)^.ImplementationWidget;
                aTextBuffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(Widget));
                Writeln('TODO(GTK2): IntSendMessage3, LM_SETSELLEN, csMemo');
                {gtk_text_buffer_get_selection_bounds(aTextBuffer, @aTextIter1, @aTextIter2);
                result:= Abs(gtk_text_iter_get_offset(@aTextIter2) - gtk_text_iter_get_offset(@aTextIter1));}
  	      end;

            csEdit:
              begin
    	        Widget:= GTK_WIDGET(Pointer(Handle));
                if gtk_editable_get_selection_bounds(GTK_EDITABLE(Widget),@Pos, nil) then
                  gtk_editable_select_region(GTK_EDITABLE(Widget), Pos, Pos+Integer(Data)+1)
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

      else begin
        result := inherited  IntSendMessage3(LM_Message, Sender, data);
        exit;
      end; // end of else-part of 2nd case
      end; // end of 2nd case
    end;   // end of else-part of 1st case
//  end;   // end of 1st case
end;

Function TGtk2Object.LoadStockPixmap(StockID: longint) : HBitmap;
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
    IconSet := gtk_style_lookup_icon_set(GetStyle('button'), StockName)
  else
    IconSet := gtk_style_lookup_icon_set(GetStyle('window'), StockName);

  If (IconSet = nil) then begin
    Result := inherited LoadStockPixmap(StockID);
    exit;
  end;

  if (StockID >= idButtonBase) and (StockID <= idDialogBase) then
    pixbuf := gtk_icon_set_render_icon(IconSet, GetStyle('button'), GTK_TEXT_DIR_NONE, GTK_STATE_NORMAL, GTK_ICON_SIZE_BUTTON, GetStyleWidget('button'), nil)
  else
    pixbuf := gtk_icon_set_render_icon(IconSet, GetStyle('window'), GTK_TEXT_DIR_NONE, GTK_STATE_NORMAL, GTK_ICON_SIZE_DIALOG, GetStyleWidget('window'), nil);

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

{------------------------------------------------------------------------------
  Method: TGtk2Object.SetLabel
  Params:  sender - the calling object
           data   - String (PChar) to be set as label for a control
  Returns: Nothing

  Sets the label text on a widget
 ------------------------------------------------------------------------------}
procedure Tgtk2Object.SetLabel(Sender : TObject; Data : Pointer);
var
  Widget : PGtkWidget;
  aTextBuffer : PGtkTextBuffer;
  aTextIter1  : TGtkTextIter;
  aTextIter2  : TGtkTextIter;
  pLabel : PChar;
begin
  if Sender is TMenuItem then begin
    inherited SetLabel(Sender, Data);
    exit;
  end;

  if Sender is TWinControl
  then Assert(False, Format('Trace:  [Tgtk2Object.SetLabel] %s --> label %s', [Sender.ClassName, TControl(Sender).Caption]))
  else begin
    Assert(False, Format('Trace:WARNING: [Tgtk2Object.SetLabel] %s --> No Decendant of TWinControl', [Sender.ClassName]));
    RaiseException('[Tgtk2Object.SetLabel] ERROR: Sender ('+Sender.Classname+')'
        +' is not TWinControl ');
  end;

  Widget := PGtkWidget(TWinControl(Sender).Handle);
  Assert(Widget = nil, 'Trace:WARNING: [Tgtk2Object.SetLabel] --> got nil pointer');
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

  csMemo        : begin
                    Widget:= PGtkWidget(GetWidgetInfo(Widget, True)^.ImplementationWidget);
                    aTextBuffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(Widget));
                    gtk_text_buffer_begin_user_action(aTextBuffer);
                    gtk_text_buffer_get_bounds(aTextBuffer, @aTextIter1, @aTextIter2);
                    gtk_text_buffer_delete(aTextBuffer, @aTextIter1, @aTextIter2);
                    gtk_text_buffer_get_bounds(aTextBuffer, @aTextIter1, @aTextIter2);
                    gtk_text_buffer_insert(aTextBuffer, @aTextIter1, pLabel, StrLen(pLabel));
                    gtk_text_buffer_end_user_action(aTextBuffer);
                  end;

  else
    inherited SetLabel(Sender, Data);
  end;
  Assert(False, Format('trace:  [Tgtk2Object.SetLabel] %s --> END', [Sender.ClassName]));
end;

{------------------------------------------------------------------------------
  Method: TGtk2Object.SetProperties
  Params:  Sender : the lcl object which called this func via SenMessage
  Returns: currently always 0

  Depending on the compStyle, this function will apply all properties of
  the calling object to the corresponding GTK2 object.
 ------------------------------------------------------------------------------}
function Tgtk2Object.SetProperties(Sender : TObject) : integer;
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
    Assert(False, Format('Trace:  [Tgtk2Object.SetProperties] %s', [Sender.ClassName]))
  else
    RaiseException('Tgtk2Object.SetProperties: '
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

  csMemo:
    begin
      ImplWidget:= GetWidgetInfo(wHandle, true)^.ImplementationWidget;

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
end;

{------------------------------------------------------------------------------
  procedure Tgtk2Object.SetSelectionMode(Sender: TObject; Widget: PGtkWidget;
    MultiSelect, ExtendedSelect: boolean);
------------------------------------------------------------------------------}
procedure Tgtk2Object.SetSelectionMode(Sender: TObject; Widget: PGtkWidget;
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
    begin
       if ExtendedSelect
          then SelectionMode:= GTK_SELECTION_EXTENDED
          else SelectionMode:= GTK_SELECTION_MULTIPLE;
    end
    else
      SelectionMode:= GTK_SELECTION_BROWSE;
    case AControl.fCompStyle of

    csListBox, csCheckListBox:
      begin
        Selection := gtk_tree_view_get_selection(GTK_TREE_VIEW(
           GetWidgetInfo(Widget, True)^.ImplementationWidget));
        gtk_tree_selection_set_mode(Selection, SelectionMode);
      end;
     else
       inherited SetSelectionMode(Sender, Widget, MultiSelect, ExtendedSelect);
    end;
  end;
end;

{------------------------------------------------------------------------------
  procedure Tgtk2Object.UpdateDCTextMetric(DC: TDeviceContext);
 ------------------------------------------------------------------------------}
procedure Tgtk2Object.UpdateDCTextMetric(DC: TDeviceContext);
const
  TestString = '{Am|g_}';
var
  XT : TSize;
  dummy: LongInt;
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
        WriteLn('WARNING: [Tgtk2Object.GetTextMetrics] Missing font')
      else begin
        Layout := gtk_widget_create_pango_layout (GetStyleWidget('default'), nil);
        pango_layout_set_font_description(Layout, UseFontDesc);
        AttrList := pango_layout_get_attributes(Layout);

        If (AttrList = nil) then
          AttrList := pango_attr_list_new();

        //fix me... what about &&, can we strip and do do markup substitution?
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

        //fix me... and what about UTF-8 conversion?
        //this could be a massive problem since we
        //will need to know before hand what the current
        //locale is, and if we stored UTF-8 string this would break
        //cross-compatibility with GTK1.2 and win32 interfaces.....

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
  Revision 1.1  2003/09/22 20:08:56  ajgenius
  break GTK2 object and winapi into includes like the GTK interface


}
