{%MainUnit gtk2proc.pp}
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

function GTKWindowStateEventCB(widget: PGtkWidget;
  state: PGdkEventWindowState; data: gpointer): gboolean; cdecl;
var
  TheForm: TCustomForm;
  SizeMsg: TLMSize;
  GtkWidth: LongInt;
  GtkHeight: LongInt;
  {$IFDEF HasX}
  NetAtom: TGdkAtom;
  AtomType: TGdkAtom;
  AIndex, ADesktop: pguint;
  AFormat: gint;
  ALength: gint;
  {$ENDIF}
begin
  Result := CallBackDefaultReturn;

  // if iconified in changed then OnIconify...

  if GTK_WIDGET_REALIZED(Widget) then
  begin
    if (GDK_WINDOW_STATE_WITHDRAWN and state^.changed_mask) = 1 then // visibility changed - this is another message block
      exit;
    if TObject(Data) is TCustomForm then
    begin
      TheForm := TCustomForm(Data);
      //DebugLn(['GTKWindowStateEventCB ',DbgSName(TheForm),' new_window_state=',state^.new_window_state,' changed_mask=',state^.changed_mask]);
      if TheForm.Parent = nil then begin
        // toplevel window
        // send a WMSize Message (see TCustomForm.WMSize)
        // ToDo: this might be too early to use the Widget^.Allocation
        //    Either send this message later or find a better way to determine the size (including the client area)
        GtkWidth:=Widget^.Allocation.Width;
        if GtkWidth<0 then GtkWidth:=0;
        GtkHeight:=Widget^.Allocation.Height;
        if GtkHeight<0 then GtkHeight:=0;
        //debugln('GTKWindowStateEventCB ',DbgSName(TObject(Data)),' ',dbgs(state^.new_window_state),' ',WidgetFlagsToString(Widget));
        if ((GDK_WINDOW_STATE_ICONIFIED and state^.new_window_state)>0) then
        begin
          {$IFDEF HasX}
          NetAtom := gdk_atom_intern('_NET_WM_DESKTOP', True);
          if NetAtom > 0 then begin
            if gdk_property_get(Widget^.window, NetAtom, XA_CARDINAL,
              0, 4, 0, @AtomType, @AFormat, @ALength, @AIndex)
            then begin
              NetAtom := gdk_atom_intern('_NET_CURRENT_DESKTOP', True);
              if gdk_property_get(gdk_get_default_root_window, NetAtom, XA_CARDINAL,0, 4, 0, @AtomType, @AFormat, @ALength, @ADesktop)
              then if ADesktop^ <> AIndex^ then begin
                // form is not on active desktop => ignore
                g_free(ADesktop);
                g_free(AIndex);
                exit;
              end
              else begin
                g_free(ADesktop);
                g_free(AIndex);
              end;
            end;
          end;
          {$ENDIF}
          SizeMsg.SizeType:=SIZEICONIC;
        end
        else if (GDK_WINDOW_STATE_MAXIMIZED and state^.new_window_state)>0 then
        begin
          if (state^.changed_mask and GDK_WINDOW_STATE_MAXIMIZED)=0 then Exit;
          SizeMsg.SizeType:=SIZEFULLSCREEN;
        end
        else
          SizeMsg.SizeType:=SIZENORMAL;

        // don't bother the LCL if nothing changed
        case SizeMsg.SizeType of
        SIZENORMAL: if TheForm.WindowState=wsNormal then exit;
        SIZEICONIC: if TheForm.WindowState=wsMinimized then exit;
        SIZEFULLSCREEN: if TheForm.WindowState=wsMaximized then exit;
        end;

        with SizeMsg do
        begin
          Result := 0;
          Msg := LM_SIZE;
          SizeType := SizeType+Size_SourceIsInterface;
          Width := SmallInt(GtkWidth);
          Height := SmallInt(GtkHeight);
        end;
        {$IFDEF VerboseSizeMsg}
        DebugLn(['GTKWindowStateEventCB ',DbgSName(TheForm),
          ' GTK=',GtkWidth,'x',GtkHeight,
          ' LCL=',TheForm.Width,'x',TheForm.Height,
          ' SizeType=',SizeMsg.SizeType-Size_SourceIsInterface,'+Size_SourceIsInterface'
          ]);
        {$ENDIF}
        DeliverMessage(TheForm, SizeMsg);
      end;
    end;
  end;
end;

function gtkMouseWheelCB(widget: PGtkWidget; event: PGdkEventScroll;
  data: gPointer): GBoolean; cdecl;
var
  AWinControl: TWinControl;
  EventXY: TPoint;
  ShiftState: TShiftState;
  MappedXY: TPoint;
  MessE : TLMMouseEvent;
begin
  Result := CallBackDefaultReturn;

  AWinControl:=TWinControl(Data);
  EventXY:=Point(TruncToInt(Event^.X),TruncToInt(Event^.Y));
  ShiftState := GTKEventStateToShiftState(Event^.State);
  MappedXY:=TranslateGdkPointToClientArea(Event^.Window,EventXY,
                                          PGtkWidget(AWinControl.Handle));
  //DebugLn('gtkMouseWheelCB ',DbgSName(AWinControl),' Mapped=',dbgs(MappedXY.X),',',dbgs(MappedXY.Y),' Event=',dbgs(EventXY.X),',',dbgs(EventXY.Y));

  // this is a mouse wheel event
  FillChar(MessE,SizeOf(MessE),0);
  MessE.Msg := LM_MOUSEWHEEL;
  case event^.direction of
    GDK_SCROLL_UP: MessE.WheelDelta := 120;
    GDK_SCROLL_DOWN: MessE.WheelDelta := -120;
  else
    exit;
  end;
  MessE.X := MappedXY.X;
  MessE.Y := MappedXY.Y;
  MessE.State := ShiftState;
  MessE.UserData := AWinControl;
  MessE.Button := 0;

  // send the message directly to the LCL
  NotifyApplicationUserInput(MessE.Msg);
  DeliverMessage(AWinControl, MessE);
end;

