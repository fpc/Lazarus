{%MainUnit gtk2wsstdctrls.pp}

{ Callbacks }

procedure Gtk2WS_MemoChanged( AGtkTextBuffer: PGtkTextBuffer; WidgetInfo: PWidgetInfo); cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Gtk2WS_MemoChanged', WidgetInfo^.LCLObject);
  Mess.Msg := CM_TEXTCHANGED;
  DeliverMessage(WidgetInfo^.LCLObject, Mess);
end;

procedure Gtk2WS_MemoCutToClip( widget: PGtkWidget; WidgetInfo: PWidgetInfo); cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Gtk2WS_MemoCutToClip', WidgetInfo^.LCLObject);
  if (Widget=nil) then ;
  Mess.msg := LM_CUTTOCLIP;
  DeliverMessage(WidgetInfo^.LCLObject, Mess);
end;

procedure Gtk2WS_MemoCopyToClip( widget: PGtkWidget; WidgetInfo: PWidgetInfo); cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Gtk2WS_MemoCopyToClip', WidgetInfo^.LCLObject);
  if (Widget=nil) then ;
  Mess.msg := LM_COPYTOCLIP;
  DeliverMessage(WidgetInfo^.LCLObject, Mess);
end;

procedure Gtk2WS_MemoPasteFromClip( widget: PGtkWidget; WidgetInfo: PWidgetInfo); cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Gtk2WS_MemoPasteFromClip', WidgetInfo^.LCLObject);
  if (Widget=nil) then ;
  Mess.msg := LM_PASTEFROMCLIP;
  DeliverMessage(WidgetInfo^.LCLObject, Mess);
end;

procedure Gtk2WS_MemoTextInserting (Textbuffer: PGtkTextBuffer; StartIter: PGtkTextIter;
             thetext: pgchar; NewTextLength: gint;  WidgetInfo: PWidgetInfo); cdecl;
var
  Memo: TCustomMemo;
  CurrLength, CutLength: integer;
begin
  { GTK2 does not provide its own max. length for memos
    so we have to do our own. }

  if TControl(WidgetInfo^.LCLObject) is TCustomMemo then begin
    Memo:= TCustomMemo(WidgetInfo^.LCLObject);
    if Memo.MaxLength < 0 then Exit;

    CurrLength:= gtk_text_buffer_get_char_count(TextBuffer);
    if CurrLength + NewTextLength <= Memo.MaxLength then Exit;

    CutLength:= CurrLength + NewTextLength - Memo.MaxLength;

    if NewTextLength - CutLength > 0 then begin
      gtk_text_buffer_insert(TextBuffer, StartIter, TheText, NewTextLength - CutLength);
    end;

    g_signal_stop_emission_by_name(PGtkObject(Textbuffer), 'insert-text');
  end;
end;



{ TGtk2WSCustomMemo }

class procedure TGtk2WSCustomMemo.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
var
TextBuf: PGtkTextBuffer;
begin
  TextBuf := gtk_text_view_get_buffer(PGtkTextView(AWidgetInfo^.CoreWidget));

  //TGtkWSBaseScrollingWinControl.SetCallbacks(AGtkWidget, AWidgetInfo);

  TGtkWSWinControl.SetCallbacks(PGtkObject(AWidgetInfo^.CoreWidget), TComponent(AWidgetInfo^.LCLObject));
  
  SignalConnect(PGtkWidget(TextBuf), 'changed', @Gtk2WS_MemoChanged, AWidgetInfo);
  SignalConnect(PGtkWidget(TextBuf), 'insert-text', @Gtk2WS_MemoTextInserting, AWidgetInfo);

  //SetCallback(LM_ACTIVATE, AGTKObject,ALCLObject);
  
  SignalConnect(AWidgetInfo^.CoreWidget, 'cut-clipboard', @Gtk2WS_MemoCutToClip, AWidgetInfo);
  SignalConnect(AWidgetInfo^.CoreWidget, 'copy-clipboard', @Gtk2WS_MemoCopyToClip, AWidgetInfo);
  SignalConnect(AWidgetInfo^.CoreWidget, 'paste-clipboard', @Gtk2WS_MemoPasteFromClip, AWidgetInfo);


end;

class function TGtk2WSCustomMemo.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
Widget,
TempWidget: PGtkWidget;
WidgetInfo: PWidgetInfo;
begin

  Widget := gtk_scrolled_window_new(nil, nil);
  Result := TLCLIntfHandle(Widget);
  if Result = 0 then Exit;
  
  WidgetInfo := CreateWidgetInfo(Pointer(Result), AWinControl, AParams);

  TempWidget := gtk_text_view_new();
  gtk_container_add(PGtkContainer(Widget), TempWidget);

  GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(Widget)^.hscrollbar, GTK_CAN_FOCUS);
  GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(Widget)^.vscrollbar, GTK_CAN_FOCUS);
  gtk_scrolled_window_set_policy(PGtkScrolledWindow(Widget),
                                     GTK_POLICY_AUTOMATIC,
                                     GTK_POLICY_AUTOMATIC);
  SetMainWidget(Widget, TempWidget);
  GetWidgetInfo(Widget, True)^.CoreWidget := TempWidget;

  gtk_text_view_set_editable (PGtkTextView(TempWidget), not TCustomMemo(AWinControl).ReadOnly);
  if TCustomMemo(AWinControl).WordWrap then
    gtk_text_view_set_wrap_mode(PGtkTextView(TempWidget), GTK_WRAP_WORD)
  else
    gtk_text_view_set_wrap_mode(PGtkTextView(TempWidget), GTK_WRAP_NONE);

  gtk_widget_show_all(Widget);

  SetCallbacks(Widget, WidgetInfo);
end;

class function TGtk2WSCustomMemo.GetStrings(const ACustomMemo: TCustomMemo
  ): TStrings;
var
TextView: PGtkTextView;
begin
  TextView := PGtkTextView(GetWidgetInfo(Pointer(ACustomMemo.Handle), False)^.CoreWidget);
  Result := TGtk2MemoStrings.Create(TextView, ACustomMemo);
end;


class procedure TGtk2WSCustomMemo.SetColor(const AWinControl: TWinControl);
var
  AWidget : PGTKWidget;
begin
  AWidget:=PGtkWidget(AWinControl.Handle);
  AWidget:=GetWidgetInfo(AWidget, True)^.CoreWidget;
  Gtk2WidgetSet.SetWidgetColor(AWidget, AWinControl.font.color, AWinControl.color,[GTK_STATE_NORMAL,GTK_STATE_ACTIVE,GTK_STATE_PRELIGHT,GTK_STATE_SELECTED,GTK_STYLE_BASE]);

end;

class procedure TGtk2WSCustomMemo.SetFont(const AWinControl: TWinControl;
  const AFont : TFont);
var
  AWidget: PGTKWidget;
begin
  if not AWinControl.HandleAllocated then exit;
  if AFont.IsDefault then exit;

  AWidget:= PGtkWidget(AWinControl.Handle);
  AWidget:= GetWidgetInfo(AWidget, true)^.CoreWidget;

  if AWidget<>nil then begin
    Gtk2WidgetSet.SetWidgetColor(AWidget, AWinControl.font.color, clNone,
       [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,GTK_STATE_PRELIGHT,GTK_STATE_SELECTED,
        GTK_STYLE_TEXT]);
    Gtk2WidgetSet.SetWidgetFont(AWidget, AFont);
  end;
end;

class procedure TGtk2WSCustomMemo.SetSelStart(const ACustomEdit: TCustomEdit;
  NewStart: integer);
var
  TextView: PGtkTextView;
  TextBuffer: PGtkTextBuffer;
  TextIter: TGtkTextIter;
begin
  if not ACustomEdit.HandleAllocated then exit;
  TextView := PGtkTextView(GetWidgetInfo(Pointer(ACustomEdit.Handle), False)^.CoreWidget);  if not ACustomEdit.HandleAllocated then exit;
  TextBuffer := gtk_text_view_get_buffer(TextView);
  gtk_text_buffer_get_iter_at_offset(TextBuffer, @TextIter, NewStart);
  gtk_text_buffer_place_cursor(TextBuffer, @TextIter);
end;

class procedure TGtk2WSCustomMemo.SetSelLength(const ACustomEdit: TCustomEdit;
  NewLength: integer);
var
  TextView: PGtkTextView;
  TextBuffer: PGtkTextBuffer;
  StartIter: TGtkTextIter;
  SelStart: Integer;
begin
  if not ACustomEdit.HandleAllocated then exit;
  TextView := PGtkTextView(GetWidgetInfo(Pointer(ACustomEdit.Handle), False)^.CoreWidget);  if not ACustomEdit.HandleAllocated then exit;
  TextBuffer := gtk_text_view_get_buffer(TextView);
  SelStart := GetSelStart(ACustomEdit);

  gtk_text_buffer_get_iter_at_offset(TextBuffer, @StartIter, SelStart);
  gtk_text_buffer_get_iter_at_offset(TextBuffer, @StartIter, SelStart+NewLength);

  // missing in the bindings!
  //gtk_text_buffer_select_range(TextBuffer, @StartIter, @EndIter);
  
  // remove this line when gtk_text_buffer_select_range is available
  gtk_text_buffer_move_mark(TextBuffer, gtk_text_buffer_get_insert(TextBuffer), @StartIter);
end;

class function TGtk2WSCustomMemo.GetSelStart(const ACustomEdit: TCustomEdit
  ): integer;
var
  TextView: PGtkTextView;
  TextBuffer: PGtkTextBuffer;
  TextMark: PGtkTextMark;
  TextIter: TGtkTextIter;
  StartIter, EndIter: TGtkTextIter;
  StartPos, EndPos: Integer;
begin
  Result := 0;
  if not ACustomEdit.HandleAllocated then exit;
  
  TextView := PGtkTextView(GetWidgetInfo(Pointer(ACustomEdit.Handle), False)^.CoreWidget);  if not ACustomEdit.HandleAllocated then exit;
  TextBuffer := gtk_text_view_get_buffer(TextView);
  TextMark := gtk_text_buffer_get_insert(TextBuffer);
  gtk_text_buffer_get_iter_at_mark(TextBuffer, @TextIter, TextMark);
  
  Result := gtk_text_iter_get_offset(@TextIter);
  if GetSelLength(ACustomEdit) = 0 then Exit;
  
  if not gtk_text_buffer_get_selection_bounds(TextBuffer, @StartIter, @EndIter) then Exit;
  
  StartPos := gtk_text_iter_get_offset(@StartIter);
  EndPos := gtk_text_iter_get_offset(@EndIter);
  
  Result := Min(StartPos, EndPos);
end;

class function TGtk2WSCustomMemo.GetSelLength(const ACustomEdit: TCustomEdit
  ): integer;
var
  TextView: PGtkTextView;
  TextBuffer: PGtkTextBuffer;
  StartIter, EndIter: TGtkTextIter;
begin
  Result := 0;
  if not ACustomEdit.HandleAllocated then exit;

  TextView := PGtkTextView(GetWidgetInfo(Pointer(ACustomEdit.Handle), False)^.CoreWidget);  if not ACustomEdit.HandleAllocated then exit;
  TextBuffer := gtk_text_view_get_buffer(TextView);
  if not gtk_text_buffer_get_selection_bounds(TextBuffer, @StartIter, @EndIter) then Exit;

  Result := Abs(gtk_text_iter_get_offset(@EndIter) - gtk_text_iter_get_offset(@StartIter));
end;



