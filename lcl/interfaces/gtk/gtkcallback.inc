{
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

{$DEFINE ASSERT_IS_ON}

function DeliverPaintMessage(const Target: Pointer; var TheMessage): GBoolean;
var
  PaintMsg: TLMPaint;
begin
  if TLMessage(TheMessage).Msg=LM_GtkPAINT then
    PaintMsg:= GtkPaintMessageToPaintMessage(TLMGtkPaint(TheMessage))
  else
    PaintMsg:=TLMPaint(TheMessage);
  Result := DeliverMessage(Target,PaintMsg) = 0;
  ReleaseDC(0,PaintMsg.DC);
end;

function DeliverPostMessage(const Target: Pointer; var TheMessage): GBoolean;
begin
  if TObject(Target) is TWinControl then
  begin
    Result := PostMessage(TWinControl(Target).Handle, TLMessage(TheMessage).Msg,
      TLMessage(TheMessage).WParam, TLMessage(TheMessage).LParam);
  end
  else
  begin
    if TLMessage(TheMessage).Msg<>LM_GtkPAINT then
      Result := DeliverMessage(Target, TheMessage) = 0
    else begin
      Result := DeliverPaintMessage(Target,TheMessage);
    end;
  end;
end;

function DeliverGtkPaintMessage(Target: Pointer; Widget: PGtkWidget): GBoolean;
var
  MSG: TLMGtkPaint;
  {$IFDEF DirectPaintMsg}
  PaintMsg: TLMPaint;
  {$ENDIF}
begin
  MSG.Msg := LM_GtkPAINT;
  MSG.Widget := Widget;
  MSG.State := GtkPaint_LCLWidget;
  MSG.Unused := 0;

  {$IFDEF DirectPaintMsg}
  PaintMsg:= GtkPaintMessageToPaintMessage(Msg);
  Result := DeliverMessage(Target,PaintMsg) = 0;
  ReleaseDC(0,PaintMsg.DC);
  {$ELSE}
  Result:=DeliverPostMessage(Target,Msg);
  {$ENDIF}
end;

procedure EventTrace(const TheMessage : string; data : pointer);
begin
  if Data = nil then
    Assert(False, Format('Trace:Event [%s] fired',[Themessage]))
  else
    Assert(False, Format('Trace:Event [%s] fired for %s',
      [TheMessage, TObject(data).Classname]));
end;

{*************************************************************}
{            callback routines                                }
{*************************************************************}

{-------------------------------------------------------------------------------
  function gtkNoteBookCloseBtnClicked
  Params: Widget: PGtkWidget; Data: Pointer
  Result: GBoolean

  gtkNoteBookCloseBtnClicked is called by the gtk, whenever a close button in
  the tab of a notebook page is clicked.
-------------------------------------------------------------------------------}
function gtkNoteBookCloseBtnClicked(Widget: PGtkWidget;
  Data: Pointer): GBoolean; cdecl;
var APage: TPage;
begin
  Result:=true; // handled = true
  if ComponentIsDestroyingHandle(TWinControl(Data)) then exit;
  APage:=TPage(Data);
  TCustomNoteBook(APage.Parent).DoCloseTabClicked(APage);
end;

{-------------------------------------------------------------------------------
  function GTKRealizeCB
  Params: Widget: PGtkWidget; Data: Pointer
  Result: GBoolean
  
  GTKRealizeCB is called by the gtk, whenever a widget is realized (ie mapped),
  but before the widget itself got the realize signal.
  That means that the gdk window on the xserver has been created.
-------------------------------------------------------------------------------}
function gtkRealizeCB(Widget: PGtkWidget; Data: Pointer): GBoolean; cdecl;
var
  decor,Func : Longint;
  TheWindow, MainWindow: PGdkWindow;
  TheForm: TCustomForm;
begin
  Result := True;
  EventTrace('realize', nil);

  if (Data<>nil) then begin
    if TObject(Data) is TCustomForm then begin
      TheForm:=TCustomForm(Data);
      TheWindow:=GetControlWindow(Widget);

      // Try to make all Modal forms transient to the main form

      //apart from none and sizeable, this will
      //only work if WM supports motif flags
      //properly, which very few actually do.

      Decor := GetWindowDecorations(TheForm);
      Func := GetWindowFunction(TheForm);

      gdk_window_set_decorations(TheWindow, decor);
      gdk_window_set_functions(TheWindow, func);
      
      if (Application.MainForm<>nil)
      and (Application.MainForm.HandleAllocated) then begin
        TheWindow:=GetControlWindow(Widget);
        MainWindow:=GetControlWindow(PGtkWidget(Application.MainForm.Handle));
        if TheWindow<>MainWindow then
          {gdk_window_set_transient_for(MainWindow,TheWindow)};
      end;
    end
    else if TObject(Data) is TPopupMenu then begin
      if (Application.MainForm<>nil)
      and (Application.MainForm.HandleAllocated) then begin
        TheWindow:=GetControlWindow(Widget);
        MainWindow:=GetControlWindow(PGtkWidget(Application.MainForm.Handle));
        if TheWindow<>MainWindow then
          {gdk_window_set_transient_for(MainWindow,TheWindow)};
      end;
    end;
    
    if not (csDesigning in TComponent(Data).ComponentState) then
      RealizeAccelerator(TComponent(Data),Widget);
  end;
end;

{-------------------------------------------------------------------------------
  function GTKRealizeAfterCB
  Params: Widget: PGtkWidget; Data: Pointer
  Result: GBoolean

  GTKRealizeAfterCB is called by the gtk, whenever a widget is realized
  (ie mapped), and after the widget itself got the realize signal.
  That means that the gdk window on the xserver has been created and the widget
  has initialized the gdkwindow. This function is used for the second part of
  the initialization of a widget.
-------------------------------------------------------------------------------}
function gtkRealizeAfterCB(Widget: PGtkWidget; Data: Pointer): GBoolean; cdecl;
var
  WinWidgetInfo: PWinWidgetInfo;
  HiddenLCLObject, LCLObject: TObject;
  NewEventMask: TGdkEventMask;
  TheWinControl: TWinControl;
  ClientWidget: PGtkWidget;
  MainWidget: PGtkWidget;
begin
  Result := True;
  EventTrace('realizeafter', nil);

  HiddenLCLObject:=GetHiddenLCLObject(Widget);
  if HiddenLCLObject=nil then begin
    // this is a normal lcl wigdet
  
    MainWidget:=GetMainWidget(Widget);
    if MainWidget=nil then
      MainWidget:=Widget;
    WinWidgetInfo:=GetWidgetInfo(MainWidget,true);
    LCLObject:=GetLCLObject(MainWidget);
    if (LCLObject<>nil) and (WinWidgetInfo<>nil) then begin
      ClientWidget:=GetFixedWidget(Widget);
      if (LCLObject is TWinControl) then
        TheWinControl:=TWinControl(LCLObject)
      else
        TheWinControl:=nil;
        
      // set extra signal masks after the widget window is created
      // define extra events we're interrested in
      //write('GTKRealizeAfterCB ');
      //if TheWinControl<>nil then write(' ',TheWinControl.Name,':',TheWinControl.ClassName,' ',HexStr(Cardinal(TheWinControl.Handle),8));
      //writeln(' Widget=',HexStr(Cardinal(Widget),8),' Fixed=',HexStr(Cardinal(GetFixedWidget(Widget)),8),' Main=',HexStr(Cardinal(GetMainWidget(Widget)),8));
      if (TheWinControl<>nil) then begin
        NewEventMask:=gdk_window_get_events(GetControlWindow(Widget))
                      or WinWidgetInfo^.EventMask;
        gdk_window_set_events(GetControlWindow(Widget),NewEventMask);
        if (ClientWidget<>nil) and (GetControlWindow(ClientWidget)<>nil)
        and (ClientWidget^.Window<>Widget^.Window) then begin
          NewEventMask:=gdk_window_get_events(GetControlWindow(ClientWidget))
                        or WinWidgetInfo^.EventMask;
          gdk_window_set_events(GetControlWindow(ClientWidget),NewEventMask);
        end;
        //writeln('BBB1 ',HexStr(Cardinal(NewEventMask),8),' ',HexStr(Cardinal(gdk_window_get_events(Widget^.Window)),8));
      end;

      if TheWinControl<>nil then begin
        SetCursor(TheWinControl, nil);
        ConnectInternalWidgetsSignals(MainWidget,TheWinControl);
      end;
    end;
  end else begin
    // this is a hidden child widget of a lcl widget
    if HiddenLCLObject is TWinControl then
      ConnectInternalWidgetsSignals(Widget,TWinControl(HiddenLCLObject));
  end;
end;

function gtkshowCB( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
   Mess : TLMShowWindow;
begin
  Result := True;
  EventTrace('show', data);
  FillChar(Mess,SizeOf(Mess),0);
  Mess.Msg := LM_SHOWWINDOW;
  Mess.Show := True;

  Result := DeliverMessage(Data, Mess) = 0;
end;

function gtkHideCB( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMShowWindow;
begin
  Result := True;
  EventTrace('hide', data);
  FillChar(Mess,SizeOf(Mess),0);
  Mess.Msg := LM_SHOWWINDOW;
  Mess.Show := False;
  Result := DeliverMessage(Data, Mess) = 0;
end;

function gtkactivateCB(widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
  LCLMenuItem: TMenuItem;
begin
  Result:= True;
  EventTrace('activate', data);
  
  if LockOnChange(PgtkObject(Widget),0)>0 then exit;

  if GtkWidgetIsA(widget,GTK_MENU_ITEM_TYPE) then begin
    LCLMenuItem:=TMenuItem(GetLCLObject(Widget));
    if (LCLMenuItem<>nil) and LCLMenuItem.IsCheckItem
    and (GtkWidgetIsA(widget,GTK_CHECK_MENU_ITEM_TYPE)) then begin
      if ((PGtkCheckMenuItem(Widget)^.flag0 and bm_checkmenuitem_active)<>0)
        <>LCLMenuItem.Checked
      then begin
        if (not LCLMenuItem.AutoCheck) then begin
          // the gtk always toggles the check flag
          // -> restore 'checked' flag
          PGtkCheckMenuItem(Widget)^.flag0:=
            PGtkCheckMenuItem(Widget)^.flag0 xor bm_checkmenuitem_active;
        end;
      end;
    end;
  end;

  Mess.Msg := LM_ACTIVATE;
  Mess.Result := 0;
  Result := DeliverMessage(Data, Mess) = 0;
//writeln('gtkactivateCB ',Result);
end;

function gtkchangedCB( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  if ComponentIsDestroyingHandle(TWinControl(Data))
  or (LockOnChange(PgtkObject(Widget),0)>0) then exit;
  EventTrace('changed', data);

  Mess.Msg := LM_CHANGED;
  Result := DeliverMessage(Data, Mess) = 0;
end;

function gtkchanged_editbox( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  Result := True;
  EventTrace('changed_editbox', data);

  Mess.Msg := CM_TEXTCHANGED;
  Result := DeliverMessage(Data, Mess) = 0;
end;

function gtkdaychanged(Widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  MSG: TLMessage;
begin
  Result := True;
  EventTrace('day changed', data);
  MSG.Msg := LM_DAYCHANGED;
  Result := DeliverPostMessage(Data, MSG);
//   Result := DeliverMessage(Data, MSG) = 0;
end;

function gtkDrawAfter(Widget: PGtkWidget; area: PGDKRectangle;
  data: gPointer) : GBoolean; cdecl;
var
  DesignOnlySignal: boolean;
begin
  Result := True;
  EventTrace('DrawAfter', data);
  
  if not (csDesigning in TComponent(Data).ComponentState) then begin
    DesignOnlySignal:=GetDesignOnlySignalFlag(Widget,dstDrawAfter);
    if DesignOnlySignal then exit;
  end else begin
    {$IFDEF VerboseDesignerDraw}
    writeln('gtkDrawAfter',
      ' Widget=',HexStr(Cardinal(Widget),8),
      ' ',TComponent(Data).Name);
    {$ENDIF}
  end;
  
  DeliverGtkPaintMessage(Data,Widget);
end;

function gtkExposeEventAfter(Widget: PGtkWidget; Event : PGDKEventExpose;
  Data: gPointer): GBoolean; cdecl;
var
  DesignOnlySignal: boolean;
begin
  Result := True;
  EventTrace('ExposeAfter', data);
  if (Event^.Count > 0) then exit;

  if not (csDesigning in TComponent(Data).ComponentState) then begin
    DesignOnlySignal:=GetDesignOnlySignalFlag(Widget,dstExposeAfter);
    if DesignOnlySignal then exit;
  end else begin
    {$IFDEF VerboseDesignerDraw}
    writeln('gtkExposeAfter',
      ' Widget=',HexStr(Cardinal(Widget),8),
      ' ',TComponent(Data).Name);
    {$ENDIF}
  end;

  DeliverGtkPaintMessage(Data,Widget);
end;

function gtkfrmactivateAfter( widget: PGtkWidget; Event : PgdkEventFocus;
  data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMActivate;
{$IFDEF VerboseFocus}
  LCLObject: TControl;
{$ENDIF}
begin
  EventTrace('activate after', data);
  {$IFDEF VerboseFocus}
  write('gtkfrmactivate Widget=',HexStr(Cardinal(Widget),8),' ',Event^.theIn);
  LCLObject:=TControl(GetLCLObject(Widget));
  if LCLObject<>nil then
    writeln(' LCLObject=',LCLObject.Name,':',LCLObject.ClassName)
  else
    writeln(' LCLObject=nil');
  {$ENDIF}
  Mess.Msg := LM_ACTIVATE;
  Result := DeliverPostMessage(Data, Mess);
end;

function gtkfrmdeactivateAfter( widget: PGtkWidget; Event : PgdkEventFocus;
  data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMActivate;
{$IFDEF VerboseFocus}
  LCLObject: TControl;
{$ENDIF}
begin
  EventTrace('deactivate after', data);
  {$IFDEF VerboseFocus}
  write('gtkfrmdeactivate Widget=',HexStr(Cardinal(Widget),8),' ',Event^.theIn,
        ' GetFocus=',HexStr(Cardinal(Widget),8));
  LCLObject:=TControl(GetLCLObject(Widget));
  if LCLObject<>nil then
    writeln(' LCLObject=',LCLObject.Name,':',LCLObject.ClassName)
  else
    writeln(' LCLObject=nil');
  {$ENDIF}
  Mess.Msg := LM_DEACTIVATE;
  Result := DeliverPostMessage(Data, Mess);
end;

function GTKMap(Widget: PGTKWidget; Data: gPointer): GBoolean; cdecl;
begin
  Result := True;
  EventTrace('map', data);
end;

function GTKKeyUpDown(Widget: PGtkWidget; Event : pgdkeventkey;
  Data: gPointer) : GBoolean; cdecl;
var
  Msg: TLMKey;
  Code,
  KeyCode: Word;
  Flags: Integer;
  Toggle, Extended, SysKey: Boolean;
begin
  Result:=true;
  
  //writeln('[GTKKeyUpDown] ',TControl(Data).Name,':',TControl(Data).ClassName,' ',Event^.theType);
  FillChar(Msg,SizeOf(Msg),0);
  GetGTKKeyInfo(Event, KeyCode, Msg.CharCode, SysKey, Extended, Toggle);
  Flags := 0;
  if Extended then Flags := KF_EXTENDED;
  if SysKey then Flags := Flags or KF_ALTDOWN;
  Msg.KeyData := $00000000; //TODO: OEM char


  //writeln('[GTKKeyUpDown] ',TControl(Data).Name,':',TControl(Data).ClassName,' ',Event^.theType);

  case Event^.theType of

  GDK_KEY_RELEASE:
    begin
      {writeln('[GTKKeyUpDown] RELEASE ',TControl(Data).Name,':',TControl(Data).ClassName,
        ' Code=',KeyCode,
        ' Char=',Msg.CharCode,' Sys=',SysKey,' Ext=',Extended,' Toggle=',Toggle);}
      EventTrace('key up', data);
      if SysKey
      then Msg.msg := LM_SYSKEYUP
      else Msg.msg := LM_KEYUP;

      Flags := Flags or KF_UP or KF_REPEAT;

      Msg.KeyData := Msg.KeyData or (Flags shl 16) or $0001 {allways};

      // send the message directly to the LCL
      Msg.Result:=0;
      Code := Msg.CharCode;
      NotifyApplicationUserInput(Msg.Msg);
      Result := DeliverMessage(Data, Msg)=0;

      If Msg.CharCode <> Code then begin
        // key was handled by LCL
        gtk_signal_emit_stop_by_name (GTK_OBJECT (Widget), 'key_release_event');
        If (Msg.CharCode >= 31) or (Msg.CharCode = 0) then begin
          {$IfNDef Win32}
          If Event^.theString <> nil then
            Event^.theString[0] := Char(Msg.CharCode);
          {$EndIf}
          Event^.KeyVal := Msg.CharCode;
        end else begin
          Event^.KeyVal := 0;
        end;
      end;
    end;
    
  GDK_KEY_PRESS:
    begin
    
      {writeln('[GTKKeyUpDown] PRESS ',TControl(Data).Name,':',TControl(Data).ClassName,
        ' Widget=',HexStr(Cardinal(Widget),8));}
      {writeln('  Event^.KeyVal=',Event^.KeyVal,
        ' State=',HexStr(Cardinal(Event^.State),8),
        ' KeyCode=',KeyCode,
        ' VK=',Msg.CharCode,
        ' SysKey=',SysKey,
        ' Extended=',Extended,
        ' Toggle=',Toggle
        );}
  
      EventTrace('key down', data);
      if SysKey
      then Msg.msg := LM_SYSKEYDOWN
      else Msg.msg := LM_KEYDOWN;

      // todo  repeat
      // Flags := Flags or KF_REPEAT;

      Msg.KeyData := Msg.KeyData or (Flags shl 16) or $0001 {TODO:  repeatcount};
      Code := Msg.CharCode;
      // send the message directly to the LCL
      NotifyApplicationUserInput(Msg.Msg);
      Result := DeliverMessage(Data, Msg)=0;

      If Msg.CharCode <> Code then begin
        // key was handled by LCL
        gtk_signal_emit_stop_by_name (GTK_OBJECT (Widget), 'key_press_event');
        //writeln('  KEY PRESS WAS HANDLED');
        If ((Msg.CharCode >= 31) and (Msg.CharCode <= 255))
        or (Msg.CharCode = 0) then begin
          {$IfNDef Win32}
          Event^.theString[0] := Char(Msg.CharCode);
          {$EndIf}
          Event^.KeyVal := Msg.CharCode;
          If Msg.CharCode <> VK_UNKNOWN then
            KeyCode := Msg.CharCode
          else
            KeyCode := $FFFF;
        end else begin
          Event^.KeyVal := 0;
          KeyCode := $FFFF;
        end;
      end;

      if (KeyCode <> $FFFF)
      then begin
        EventTrace('char', data);
        if SysKey then
          Msg.msg := LM_SYSCHAR
        else
          Msg.msg := LM_CHAR;
        Msg.CharCode := KeyCode;
        Msg.Result:=0;
        // send the message directly to the LCL
        Result := DeliverMessage(Data, Msg)=0;
        If Msg.CharCode <> KeyCode then begin
          // key was handled by lcl
          gtk_signal_emit_stop_by_name (GTK_OBJECT (Widget), 'key_press_event');
          If ((Msg.CharCode >= 31) and (Msg.CharCode <= 255))
          or (Msg.CharCode = 0) then
          begin
            {$IfNDef Win32}
            Event^.theString[0] := Char(Msg.CharCode);
            {$EndIf}
            Event^.KeyVal := Msg.CharCode;
          end;
        end;
      end;
      //This is disabled on account of the
      //strange behaviour in TSynEdit
      (*If Msg.CharCode = VK_TAB then
        If TObject(Data) is TControl then begin
          Control := TControl(Data);
          Result := Control.PerformTab;
          If Result then begin
            {$IfNDef Win32}
            Event^.theString[0] := #0;
            {$EndIf}
            Event^.KeyVal := 0;
          end;
        end; *)
    end;
  end;
//writeln('AAA1 [GTKKeyUpDown] ',TControl(Data).Name,':',TControl(Data).ClassName,' Result=',Result);
end;

function GTKFocusCB( widget: PGtkWidget; event:PGdkEventFocus;
  data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
{$IFDEF VerboseFocus}
  LCLObject: TControl;
{$ENDIF}
begin
  EventTrace('focus', data);
  {$IFDEF VerboseFocus}
  write('GTKFocusCB Widget=',HexStr(Cardinal(Widget),8),' ',Event^.theIn,
        ' GetFocus=',HexStr(Cardinal(GetFocus),8));
  LCLObject:=TControl(GetLCLObject(Widget));
  if LCLObject<>nil then
    writeln(' LCLObject=',LCLObject.Name,':',LCLObject.ClassName)
  else
    writeln(' LCLObject=nil');
  {$ENDIF}

  //TODO: fill in old focus
  Mess.msg := LM_SETFOCUS;
  Assert(False, Format('Trace:TODO: [gtkfocusCB] %s  finish', [TObject(Data).ClassName]));
  Result := DeliverMessage(Data,  Mess) = 0;
end;

function GTKKillFocusCB(widget: PGtkWidget; event:PGdkEventFocus;
  data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
{$IFDEF VerboseFocus}
  LCLObject: TControl;
{$ENDIF}
begin
  EventTrace('killfocus', data);
  {$IFDEF VerboseFocus}
  write('GTKKillFocusCB Widget=',HexStr(Cardinal(Widget),8),' ',Event^.theIn,
        ' GetFocus=',HexStr(Cardinal(GetFocus),8));
  LCLObject:=TControl(GetLCLObject(Widget));
  if LCLObject<>nil then
    writeln(' LCLObject=',LCLObject.Name,':',LCLObject.ClassName)
  else
    writeln(' LCLObject=nil');
  {$ENDIF}
  Mess.msg := LM_KILLFOCUS;

  //TODO: fill in new focus
  Assert(False, Format('Trace:TODO: [gtkkillfocusCB] %s  finish', [TObject(Data).ClassName]));
  if GetFocus<>0 then
    Result := DeliverMessage(Data,  Mess) = 0;
end;

function gtkdestroyCB(widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
   Mess: TLMessage;
   Info: PWinWidgetInfo;
begin
  Result := True;
  EventTrace('destroy', data);
  Mess.msg := LM_DESTROY;
  Result := DeliverMessage(Data, Mess) = 0;

  // NOTE: if the destroy message is posted
  // we should post an info destroy message as well
  Info := GetWidgetInfo(widget, False);
  if Info <> nil then Dispose(Info);
end;

function gtkdeleteCB( widget : PGtkWidget; event : PGdkEvent; data : gPointer) : GBoolean; cdecl;
var Mess : TLMessage;
begin
  Mess.Msg:= LM_CLOSEQUERY;
  { Message results : True - do nothing, False - destroy or hide window }
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function gtkresizeCB( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
//var
//   Mess : TLMessage;
begin
   Result := True;
   EventTrace('resize', data);
//   Mess.msg := LM_RESIZE;
//   TControl(data).WindowProc(TLMessage(Mess));
  Assert(False, 'Trace:TODO: [gtkresizeCB] fix (or remove) to new LM_SIZE');
//TObject(data).Dispatch(Mess);
end;

function gtkMonthChanged(Widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  MSG: TLMessage;
begin
  Result := True;
  EventTrace('month changed', data);
  MSG.Msg := LM_MONTHCHANGED;
  Result := DeliverPostMessage(Data, MSG);
//   Result := DeliverMessage(Data, MSG) = 0;
end;

{-------------------------------------------------------------------------------
  procedure DeliverMouseMoveMessage(Widget:PGTKWidget; Event: PGDKEventMotion;
    AWinControl: TWinControl);

  Translate a gdk mouse motion event into a LCL mouse move message and send it.

  Mouse coordinate mapping:

  Why mapping:
  An lcl control can consists of several gtk widgets, and any message to them is
  send to the lcl control. The gtk sends the coordinates relative to the
  emitting gdkwindow (not relative to the gtkwidget). And the area of a lcl
  control can belong to several gdkwindows. Therefore the mouse coordinates must
  be mapped.

  What the lcl expects:
  For Delphi compatibility the mouse coordinates must be relative to the client
  area of the control.
  That means for example if the mouse is over the top-left pixel of the client
  widget (mostly a gtkfixed widget), then 0,0 is send.
  If the mouse is on the top-left pixel of the container widget then the
  coordinates can be negative, if there is frame around the client area.
-------------------------------------------------------------------------------}
procedure DeliverMouseMoveMessage(Widget:PGTKWidget; Event: PGDKEventMotion;
  AWinControl: TWinControl);
var
  Msg: TLMMouseMove;
  ShiftState: TShiftState;
  MappedXY: TPoint;
begin
  MappedXY:=TranslateGdkPointToClientArea(Event^.Window,
                                         Point(trunc(Event^.X),trunc(Event^.Y)),
                                         PGtkWidget(AWinControl.Handle));

  ShiftState := GTKEventState2ShiftState(Event^.State);
  with Msg do
  begin
    Msg := LM_MouseMove;

    XPos := MappedXY.X;
    YPos := MappedXY.Y;

    Keys := 0;
    if ssShift in ShiftState then Keys := Keys or MK_SHIFT;
    if ssCtrl in ShiftState then Keys := Keys or MK_CONTROL;
    if ssLeft in ShiftState then Keys := Keys or MK_LBUTTON;
    if ssRight in ShiftState then Keys := Keys or MK_RBUTTON;
    if ssMiddle in ShiftState then Keys := Keys or MK_MBUTTON;

    Result := 0;
  end;

  // send the message directly to the LCL
  // (Posting the message via queue
  //  has the risk of getting out of sync with the gtk)
  NotifyApplicationUserInput(Msg.Msg);
  DeliverMessage(AWinControl, Msg);
end;

{-------------------------------------------------------------------------------
  function ControlGetsMouseMoveBefore(AControl: TControl): boolean;
  
  Returns true, if mouse move event should be sent before the widget istelf
  reacts.
-------------------------------------------------------------------------------}
function ControlGetsMouseMoveBefore(AControl: TControl): boolean;
begin
  // currently there are no controls, that need after events.
  Result:=true;
end;

{-------------------------------------------------------------------------------
  GTKMotionNotify
  Params: widget: PGTKWidget; event: PGDKEventMotion; data: gPointer
  Returns: GBoolean
  
  Called whenever the mouse is moved over a widget.
  The gtk event is translated into a lcl MouseMove message.
  
-------------------------------------------------------------------------------}
function gtkMotionNotify(Widget:PGTKWidget; Event: PGDKEventMotion;
  Data: gPointer): GBoolean; cdecl;
var
  DesignOnlySignal: boolean;
begin
  Result:=true;
  
  {$IFDEF VerboseMouseBugfix}
  DesignOnlySignal:=GetDesignOnlySignalFlag(Widget,dstMouseMotion);
  writeln('[GTKMotionNotify] ',
    TControl(Data).Name,':',TControl(Data).ClassName,
    ' Widget=',HexStr(Cardinal(Widget),8),
    ' DSO=',DesignOnlySignal,
    ' Event^.X=',trunc(Event^.X),' Event^.Y=',trunc(Event^.Y)
    );
  {$ENDIF}

  UpdateMouseCaptureControl;

  if not (csDesigning in TComponent(Data).ComponentState) then begin
    DesignOnlySignal:=GetDesignOnlySignalFlag(Widget,dstMouseMotion);
    if DesignOnlySignal then exit;
    if not ControlGetsMouseMoveBefore(TControl(Data)) then exit;
  end else begin
    // stop the signal, so that the widget does not auto react
    gtk_signal_emit_stop_by_name(PGTKObject(Widget),'motion-notify-event');
  end;

  DeliverMouseMoveMessage(Widget,Event,TWinControl(Data));
end;

{-------------------------------------------------------------------------------
  GTKMotionNotifyAfter
  Params: widget: PGTKWidget; event: PGDKEventMotion; data: gPointer
  Returns: GBoolean

  Called whenever the mouse is moved over a widget as last handler.
-------------------------------------------------------------------------------}
function GTKMotionNotifyAfter(widget:PGTKWidget; event: PGDKEventMotion;
  data: gPointer): GBoolean; cdecl;
begin
  Result:=true;
  {$IFDEF VerboseMouseBugfix}
  writeln('[GTKMotionNotifyAfter] ',
    TControl(Data).Name,':',TControl(Data).ClassName);
  {$ENDIF}

  // stop the signal, so that it is not sent to the parent widgets
  gtk_signal_emit_stop_by_name(PGTKObject(Widget),'motion-notify-event');

  UpdateMouseCaptureControl;
  
  if (csDesigning in TComponent(Data).ComponentState) then exit;
  if ControlGetsMouseMoveBefore(TControl(Data)) then exit;

  DeliverMouseMoveMessage(Widget,Event,TWinControl(Data));
end;

{-------------------------------------------------------------------------------
  function ControlGetsMouseDownBefore(AControl: TControl): boolean;

  Returns true, if mouse down event should be sent before the widget istelf
  reacts.
-------------------------------------------------------------------------------}
function ControlGetsMouseDownBefore(AControl: TControl): boolean;
begin
  case AControl.fCompStyle of
  csCheckBox, csToggleBox:
    Result:=false;
  else
    Result:=true;
  end;
end;

{-------------------------------------------------------------------------------
  procedure DeliverMouseDownMessage(widget: PGtkWidget; event : pgdkEventButton;
    AWinControl: TWinControl);

  Translate a gdk mouse press event into a LCL mouse down message and send it.
-------------------------------------------------------------------------------}
procedure DeliverMouseDownMessage(widget: PGtkWidget; event : pgdkEventButton;
  AWinControl: TWinControl);
const
  WHEEL_DELTA : array[Boolean] of Integer = (-1, 1);
var
  MessI : TLMMouse;
  MessE : TLMMouseEvent;
  ShiftState: TShiftState;
  MappedXY: TPoint;
  EventXY: TPoint;

{ $DEFINE VerboseMouseBugfix}

  function CheckMouseButtonDown(var LastMouse: TLastMouseClick;
    BtnKey, MsgNormal, MsgDouble, MsgTriple, MsgQuad: longint): boolean;

    function LastClickInSameGdkWindow: boolean;
    begin
      Result:=(LastMouse.Window<>nil) and (LastMouse.Window=Event^.Window);
    end;

    function LastClickAtSamePosition: boolean;
    begin
      Result:= (Abs(EventXY.X-LastMouse.WindowPoint.X)<=DblClickThreshold)
           and (Abs(EventXY.Y-LastMouse.WindowPoint.Y)<=DblClickThreshold);
    end;

    function LastClickInTime: boolean;
    begin
      Result:=((now - LastMouse.TheTime) <= ((1/86400)*(DblClickTime/1000)));
    end;

    function TestIfMultiClick: boolean;
    begin
      Result:=LastClickInSameGdkWindow
          and LastClickAtSamePosition
          and LastClickInTime;
    end;

  var
    IsMultiClick: boolean;
  begin
    Result:=false;
    if (LastMouse.Down) and
      (not (Event^.theType in [gdk_2button_press,gdk_3button_press]))
    then begin
      {$IFDEF VerboseMouseBugfix}
      writeln(' NO CLICK: LastMouse.Down=',LastMouse.Down,
              ' Event^.theType=',Event^.theType);
      {$ENDIF}
      Exit;
    end;

    MessI.Keys := MessI.Keys or BtnKey;

    IsMultiClick:=TestIfMultiClick;

    case Event^.theType of
    gdk_2button_press:
      // the gtk itself has detected a double click
      if (LastMouse.ClickCount>=2)
      and IsMultiClick
      then begin
        // the double click was already detected and sent to the LCL
        // -> skip this message
        exit;
      end else begin
        LastMouse.ClickCount:=2;
      end;

    gdk_3button_press:
      // the gtk itself has detected a triple click
      if (LastMouse.ClickCount>=3)
      and IsMultiClick
      then begin
        // the triple click was already detected and sent to the LCL
        // -> skip this message
        exit;
      end else begin
        LastMouse.ClickCount:=3;
      end;

    else
      begin
        inc(LastMouse.ClickCount);

        if (LastMouse.ClickCount<=4)
        and IsMultiClick
        then begin
          // multi click
          {$IFDEF VerboseMouseBugfix}
          writeln('  MULTI CLICK: ',now,'-',LastMouse.TheTime,'<= ',
            ((1/86400)*(DblClickTime/1000)));
          {$ENDIF}
        end else begin
          // normal click
          LastMouse.ClickCount:=1;
        end;
      end;
    end;
    {$IFDEF VerboseMouseBugfix}
    writeln('  ClickCount=',LastMouse.ClickCount);
    {$ENDIF}

    LastMouse.TheTime := Now;
    LastMouse.Window := Event^.Window;
    LastMouse.WindowPoint := EventXY;
    LastMouse.Down := True;
    LastMouse.Component:=AWinControl;

    case LastMouse.ClickCount of
    1: MessI.Msg := MsgNormal;
    2: MessI.Msg := MsgDouble;
    3: MessI.Msg := MsgTriple;
    4: MessI.Msg := MsgQuad;
    else
      MessI.Msg := LM_NULL;
    end;

    Result:=true;
  end;

begin
  EventXY:=Point(trunc(Event^.X),trunc(Event^.Y));
  ShiftState := GTKEventState2ShiftState(Event^.State);
  MappedXY:=TranslateGdkPointToClientArea(Event^.Window,EventXY,
                                         PGtkWidget(AWinControl.Handle));
  //writeln('BBB1 MouseDown ',AWinControl.Name,':',AWinControl.ClassName,' Mapped=',MappedXY.X,',',MappedXY.Y,' Event=',EventXY.X,',',EventXY.Y);

  if event^.Button in [4,5] then begin
    // this is a mouse wheel event
    MessE.Msg := LM_MOUSEWHEEL;
    MessE.WheelDelta := WHEEL_DELTA[event^.Button = 4];
    MessE.X := MappedXY.X;
    MessE.Y := MappedXY.Y;
    MessE.State := ShiftState;
    MessE.UserData := AWinControl;
    MessE.Button := 0;

    // send the message directly to the LCL
    NotifyApplicationUserInput(MessE.Msg);
    DeliverMessage(AWinControl, MessE);
  end
  else begin
    // a normal mouse button is pressed
    MessI.Keys := 0;
    case event^.Button of

    1: if not CheckMouseButtonDown(LastLeft,
              MK_LBUTTON, LM_LBUTTONDOWN,
              LM_LBUTTONDBLCLK, LM_LBUTTONTRIPLECLK, LM_LBUTTONQUADCLK)
       then exit;

    2: if not CheckMouseButtonDown(LastMiddle,
              MK_MBUTTON, LM_MBUTTONDOWN,
              LM_MBUTTONDBLCLK, LM_MBUTTONTRIPLECLK, LM_MBUTTONQUADCLK)
       then exit;

    3: if not CheckMouseButtonDown(LastRight,
              MK_RBUTTON, LM_RBUTTONDOWN,
              LM_RBUTTONDBLCLK, LM_RBUTTONTRIPLECLK, LM_RBUTTONQUADCLK)
       then exit;

    else
      begin
        MessI.Msg := LM_NULL;
        exit;
      end;
    end; // case

    MessI.XPos := MappedXY.X;
    MessI.YPos := MappedXY.Y;

    if ssShift in ShiftState then MessI.Keys := MessI.Keys or MK_SHIFT;
    if ssCtrl in ShiftState then MessI.Keys := MessI.Keys or MK_CONTROL;
    if ssLeft in ShiftState then MessI.Keys := MessI.Keys or MK_LBUTTON;
    if ssRight in ShiftState then MessI.Keys := MessI.Keys or MK_RBUTTON;
    if ssMiddle in ShiftState then MessI.Keys := MessI.Keys or MK_MBUTTON;

    MessI.Result:=0;
    // send the message directly to the LCL
    NotifyApplicationUserInput(MessI.Msg);
    DeliverMessage(AWinControl, MessI);
  end;
end;

{-------------------------------------------------------------------------------
  gtkMouseBtnPress
  Params: widget: PGTKWidget; event: PGDKEventMotion; data: gPointer
  Returns: GBoolean

  Called whenever the mouse is over a widget and a mouse button is pressed.
-------------------------------------------------------------------------------}
function gtkMouseBtnPress(widget: PGtkWidget; event : pgdkEventButton;
  data: gPointer) : GBoolean; cdecl;
var
  DesignOnlySignal: boolean;
begin
  Result:=true;
  
  {$IFDEF VerboseMouseBugfix}
  writeln('');
  DesignOnlySignal:=GetDesignOnlySignalFlag(Widget,dstMousePress);
  writeln('[gtkMouseBtnPress] ',
    TComponent(Data).Name,':',TObject(Data).ClassName,
    ' Widget=',HexStr(Cardinal(Widget),8),
    ' ControlWidget=',HexStr(Cardinal(TWinControl(Data).Handle),8),
    ' DSO=',DesignOnlySignal,
    ' ',Trunc(Event^.X),',',Trunc(Event^.Y),
    ' Type=',Event^.theType);
  {$ENDIF}
  //writeln('DDD1 MousePress Widget=',HexStr(Cardinal(Widget),8),
  //' ClientWidget=',HexStr(Cardinal(GetFixedWidget(Widget)),8),
  //' EventMask=',HexStr(Cardinal(gdk_window_get_events(Widget^.Window)),8),
  //' GDK_BUTTON_RELEASE_MASK=',HexStr(Cardinal(GDK_BUTTON_RELEASE_MASK),8),
  //' Window=',HexStr(Cardinal(Widget^.Window),8)
  //);
  //if GetFixedWidget(Widget)<>nil then
  //  writeln('DDD2 ClientWindow=',HexStr(Cardinal(PGtkWidget(GetFixedWidget(Widget))^.Window),8));

  EventTrace('Mouse button Press', data);
  Assert(False, Format('Trace:[gtkMouseBtnPress] ', []));

  UpdateMouseCaptureControl;

  if not (csDesigning in TComponent(Data).ComponentState) then begin
    DesignOnlySignal:=GetDesignOnlySignalFlag(Widget,dstMousePress);
    if DesignOnlySignal then exit;
    if not ControlGetsMouseDownBefore(TControl(Data)) then exit;
  end else begin
    // stop the signal, so that the widget does not auto react
    if (TControl(Data).FCompStyle<>csNotebook)
    or (event^.Button<>1) then
      gtk_signal_emit_stop_by_name(PGTKObject(Widget),'button-press-event');
  end;
  
  DeliverMouseDownMessage(Widget,Event,TWinControl(Data));
end;

{-------------------------------------------------------------------------------
  gtkMouseBtnPressAfter
  Params: widget: PGTKWidget; event: PGDKEventMotion; data: gPointer
  Returns: GBoolean

  Called whenever the mouse is over a widget and a mouse button is pressed.
  This is the last handler.
-------------------------------------------------------------------------------}
function gtkMouseBtnPressAfter(widget: PGtkWidget; event : pgdkEventButton;
  data: gPointer) : GBoolean; cdecl;
begin
  Result:=true;
  {$IFDEF VerboseMouseBugfix}
  {writeln('[gtkMouseBtnPressAfter] ',
    TControl(Data).Name,':',TObject(Data).ClassName,
    ' Widget=',HexStr(Cardinal(Widget),8),
    ' ',Trunc(Event^.X),',',Trunc(Event^.Y));}
  {$ENDIF}

  UpdateMouseCaptureControl;

  // stop the signal, so that it is not sent to the parent widgets
  gtk_signal_emit_stop_by_name(PGTKObject(Widget),'button-press-event');

  if (csDesigning in TComponent(Data).ComponentState) then exit;
  if ControlGetsMouseDownBefore(TControl(Data)) then exit;

  DeliverMouseDownMessage(Widget,Event,TWinControl(Data));
end;

{-------------------------------------------------------------------------------
  function ControlGetsMouseUpBefore(AControl: TControl): boolean;

  Returns true, if mouse up event should be sent before the widget istelf
  reacts.
-------------------------------------------------------------------------------}
function ControlGetsMouseUpBefore(AControl: TControl): boolean;
begin
  case AControl.fCompStyle of
  csCheckBox,
  csRadioButton,
  csToggleBox:
    Result:=false;
  else
    Result:=true;
  end;
end;

{-------------------------------------------------------------------------------
  procedure DeliverMouseUpMessage(widget: PGtkWidget; event : pgdkEventButton;
    AWinControl: TWinControl);

  Translate a gdk mouse release event into a LCL mouse up message and send it.
-------------------------------------------------------------------------------}
procedure DeliverMouseUpMessage(widget: PGtkWidget; event : pgdkEventButton;
  AWinControl: TWinControl);
var
  MessI : TLMMouse;
  ShiftState: TShiftState;
  MappedXY: TPoint;

  function CheckMouseButtonUp(var LastMouse: TLastMouseClick;
    MsgUp: longint): boolean;
  begin
    MessI.Msg := MsgUp;
    LastMouse.Down := False;
    Result:=true;
 end;

begin
  MappedXY:=TranslateGdkPointToClientArea(Event^.Window,
                                         Point(trunc(Event^.X),trunc(Event^.Y)),
                                         PGtkWidget(AWinControl.Handle));

  case event^.Button of

  1: if not CheckMouseButtonUp(LastLeft, LM_LBUTTONUP)
     then exit;

  2: if not CheckMouseButtonUp(LastMiddle, LM_MBUTTONUP)
     then exit;

  3: if not CheckMouseButtonUp(LastRight, LM_RBUTTONUP)
     then exit;

  else
    begin
      MessI.Msg := LM_NULL;
      exit;
    end;
  end; // case

  MessI.XPos := MappedXY.X;
  MessI.YPos := MappedXY.Y;

  ShiftState := gtkeventstate2shiftstate(Event^.State);
  MessI.Keys := 0;
  if ssShift in ShiftState then MessI.Keys := MessI.Keys or MK_SHIFT;
  if ssCtrl in ShiftState then MessI.Keys := MessI.Keys or MK_CONTROL;
  if ssLeft in ShiftState then MessI.Keys := MessI.Keys or MK_LBUTTON;
  if ssRight in ShiftState then MessI.Keys := MessI.Keys or MK_RBUTTON;
  if ssMiddle in ShiftState then MessI.Keys := MessI.Keys or MK_MBUTTON;

  if MessI.Msg <> LM_NULL then begin
    // send the message directly to the LCL
    // (Posting the message via queue
    //  has the risk of getting out of sync with the gtk)
    MessI.Result := 0;
    NotifyApplicationUserInput(MessI.Msg);
    DeliverMessage(AWinControl, MessI);
  end;
end;

{-------------------------------------------------------------------------------
  gtkMouseBtnRelease
  Params: widget: PGTKWidget; event: PGDKEventMotion; data: gPointer
  Returns: GBoolean

  Called whenever the mouse is over a widget and a mouse button is released.
-------------------------------------------------------------------------------}
function gtkMouseBtnRelease(widget: PGtkWidget; event : pgdkEventButton;
  data: gPointer) : GBoolean; cdecl;
var
  DesignOnlySignal: boolean;
begin
  Result:=true;

  {$IFDEF VerboseMouseBugfix}
  DesignOnlySignal:=GetDesignOnlySignalFlag(Widget,dstMouseRelease);
  writeln('[gtkMouseBtnRelease] A ',
    TComponent(Data).Name,':',TObject(Data).ClassName,' ',
    ' Widget=',HexStr(Cardinal(Widget),8),
    ' DSO=',DesignOnlySignal,
    ' ',Trunc(Event^.X),',',Trunc(Event^.Y),' Btn=',event^.Button);
  {$ENDIF}

  //writeln('EEE1 MouseRelease Widget=',HexStr(Cardinal(Widget),8),
  //' EventMask=',HexStr(Cardinal(gdk_window_get_events(Widget^.Window)),8),
  //' GDK_BUTTON_RELEASE_MASK=',HexStr(Cardinal(GDK_BUTTON_RELEASE_MASK),8));

  UpdateMouseCaptureControl;

  if not (csDesigning in TComponent(Data).ComponentState) then begin
    DesignOnlySignal:=GetDesignOnlySignalFlag(Widget,dstMouseRelease);
    if DesignOnlySignal or (not ControlGetsMouseUpBefore(TControl(Data))) then
    begin
      ReleaseMouseCapture(true);
      exit;
    end;
  end else begin
    // stop the signal, so that the widget does not auto react
    if TControl(Data).FCompStyle<>csNotebook then
      gtk_signal_emit_stop_by_name(PGTKObject(Widget),'button-release-event');
  end;
  
  DeliverMouseUpMessage(Widget,Event,TWinControl(Data));
end;

{-------------------------------------------------------------------------------
  gtkMouseBtnReleaseAfter
  Params: widget: PGTKWidget; event: PGDKEventMotion; data: gPointer
  Returns: GBoolean

  Called whenever the mouse is over a widget and a mouse button is released.
  This is the last handler.
-------------------------------------------------------------------------------}
function gtkMouseBtnReleaseAfter(widget: PGtkWidget; event : pgdkEventButton;
  data: gPointer) : GBoolean; cdecl;
begin
  Result:=true;
  {$IFDEF VerboseMouseBugfix}
  {writeln('[gtkMouseBtnReleaseAfter] ',
    TControl(Data).Name,':',TObject(Data).ClassName,' ',
    Trunc(Event^.X),',',Trunc(Event^.Y));}
  {$ENDIF}

  // stop the signal, so that it is not sent to the parent widgets
  gtk_signal_emit_stop_by_name(PGTKObject(Widget),'button-release-event');

  UpdateMouseCaptureControl;

  if (csDesigning in TComponent(Data).ComponentState) then exit;
  if ControlGetsMouseUpBefore(TControl(Data)) then exit;

  DeliverMouseUpMessage(Widget,Event,TWinControl(Data));
end;

function gtkclickedCB(widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
   Mess : TLMessage;
begin
  //writeln('[gtkclickedCB] ',TObject(Data).ClassName);
  EventTrace('clicked', data);
  if (LockOnChange(PgtkObject(Widget),0)>0) then exit;
  Mess.Msg := LM_CLICKED;
  Result:= DeliverMessage(Data, Mess) = 0;

  Result := True;
end;

function gtkOpenDialogRowSelectCB(widget : PGtkWidget; row : gint;
  column : gint; event : pgdkEventButton; data : gPointer ) : GBoolean; cdecl;
var
  ShiftState: TShiftState;
  loop : gint;
  startRow : gint;
  endRow : gint;
begin
   // only process the callback if there is event data. If there isn't any
   // event data that means it was called due to a direct function call of the
   // widget and not an actual mouse click on the widget.
   if event <> nil then
   begin
      ShiftState := GTKEventState2ShiftState(Event^.State);
      if ssShift in ShiftState then
      begin
         if LastFileSelectRow <> -1 then
         begin
            startRow := LastFileSelectRow;
	    endRow := row;
            if LastFileSelectRow > row then
	    begin
	       startRow := row;
	       endRow := LastFileSelectRow;
	    end;
            for loop := startRow to endRow do
	    begin
               gtk_clist_select_row(PGtkCList(widget), loop, column);
	    end;
         end;
      end
      else if not (ssCtrl in ShiftState) then
      begin
         gtk_clist_unselect_all(PGtkCList(widget));
         gtk_clist_select_row(PGtkCList(widget), row, column);
      end;
      LastFileSelectRow := row;
   end;
   Result := True;
end;

function gtkDialogOKclickedCB( widget: PGtkWidget;
  data: gPointer) : GBoolean; cdecl;
var
   theDialog : TCommonDialog;
   Fpointer : Pointer;
   // colordialog
   colorArray : array[0..2] of double;
   colorsel : GTK_COLOR_SELECTION;
   newColor : TGdkColor;
   // fontdialog
   FontName : String;
   ALogFont  : TLogFont;
   // filedialog
   cListRow : PGList;
   rowNum   : gint;
   fileInfo : PGChar;
   fileList : PGTKCList;
   DirName  : string;
   FileName : string;
   Files: TStringList;
begin
  Result := True;
  theDialog := TCommonDialog(data);
  FPointer := Pointer(theDialog.Handle);
  
  if theDialog is TFileDialog then
  begin
    if theDialog is TOpenDialog then
    begin
      // check extra options
      if ofAllowMultiSelect in TOpenDialog(theDialog).Options then
      begin
        FileName:=gtk_file_selection_get_filename(
                                    PGtkFileSelection(FPointer));
        DirName:=ExtractFilePath(FileName);
        TFileDialog(data).FileName := '';
        Files:=TStringList(TFileDialog(theDialog).Files);
        Files.Clear;
        if not FileExists(FileName) then
          Files.Add(FileName);
        fileList := PGtkCList(PGtkFileSelection(FPointer)^.file_list);
        rowNum := 0;
        cListRow := fileList^.row_list;
        while cListRow <> nil do
        begin
          if PGtkCListRow(cListRow^.data)^.state = GTK_STATE_SELECTED then
	        begin
            if gtk_clist_get_cell_type(fileList, rowNum, 0) = GTK_CELL_TEXT
            then begin
	            gtk_clist_get_text(fileList, rowNum, 0, @fileInfo);
                 Files.Add(DirName+fileInfo);
            end;
          end;
          // get next row from list
          rowNum := rowNum + 1;
          cListRow := g_list_next(cListRow);
        end;
        if (Filename<>'') and (Files.IndexOf(Filename)<0) then
          Files.Add(Filename);
      end else begin
        TFileDialog(data).FileName :=
          gtk_file_selection_get_filename(PGtkFileSelection(FPointer));
      end;
    end
    else
    begin
      TFileDialog(data).FileName :=
        gtk_file_selection_get_filename(PGtkFileSelection(FPointer));
    end;
  end
  else if theDialog is TColorDialog then
  begin
    colorSel := GTK_COLOR_SELECTION((GTK_COLOR_SELECTION_DIALOG(FPointer))^.colorsel);
    gtk_color_selection_get_color(colorsel, @colorArray[0]);
    newColor.pixel := 0;
    newColor.red := Trunc(colorArray[0] * $FFFF);
    newColor.green := Trunc(colorArray[1] * $FFFF);
    newColor.blue := Trunc(colorArray[2] * $FFFF);
    TColorDialog(theDialog).Color := TGDKColorToTColor(newcolor);
  end
  else if theDialog is TFontDialog then
  begin
    Assert(False, 'Trace:Pressed OK in FontDialog');
    FontName := gtk_font_selection_dialog_get_font_name(
                                             pgtkfontselectiondialog(FPointer));
    // extract basic font attributes from the font name in XLFD format
    ALogFont:=XLFDNameToLogFont(FontName);
    TFontDialog(theDialog).Font.Assign(ALogFont);
    // set the font name in XLFD format
    // a font name in XLFD format overrides in the gtk interface all other font
    // settings.
    TFontDialog(theDialog).Font.Name := FontName;

    Assert(False, 'Trace:-----'+TFontDialog(theDialog).Font.Name+'----');
  end;

  StoreCommonDialogSetup(theDialog);
  theDialog.UserChoice := mrOK;
end;

{-------------------------------------------------------------------------------
  function gtkDialogCancelclickedCB
  Params: widget: PGtkWidget; data: gPointer
  Result: GBoolean
  
  This function is called, whenever the user clicks the cancel button in a
  commondialog
-------------------------------------------------------------------------------}
function gtkDialogCancelclickedCB(widget: PGtkWidget; data: gPointer): GBoolean;
  cdecl;
var
  theDialog : TCommonDialog;
begin
  Result := True;
  theDialog := TCommonDialog(data);
  if theDialog is TFileDialog then
  begin
    TFileDialog(data).FileName := '';
  end;
  StoreCommonDialogSetup(theDialog);
  theDialog.UserChoice := mrCancel;
end;

{-------------------------------------------------------------------------------
  function gtkDialogHelpclickedCB
  Params: widget: PGtkWidget; data: gPointer
  Result: GBoolean

  This function is called, whenever the user clicks the help button in a
  commondialog
-------------------------------------------------------------------------------}
function gtkDialogHelpclickedCB(widget: PGtkWidget; data: gPointer): GBoolean;
  cdecl;
var
  theDialog : TCommonDialog;
begin
  Result := True;
  theDialog := TCommonDialog(data);
  if theDialog is TOpenDialog then begin
    if TOpenDialog(theDialog).OnHelpClicked<>nil then
      TOpenDialog(theDialog).OnHelpClicked(theDialog);
  end;
end;

{-------------------------------------------------------------------------------
  function gtkDialogApplyclickedCB
  Params: widget: PGtkWidget; data: gPointer
  Result: GBoolean

  This function is called, whenever the user clicks the Apply button in a
  commondialog
-------------------------------------------------------------------------------}
function gtkDialogApplyclickedCB(widget: PGtkWidget; data: gPointer): GBoolean;
  cdecl;
var
  theDialog : TCommonDialog;
  FontName: string;
  ALogFont: TLogFont;
begin
  Result := True;
  theDialog := TCommonDialog(data);
  if (theDialog is TFontDialog)
  and (fdApplyButton in TFontDialog(theDialog).Options)
  and (Assigned(TFontDialog(theDialog).OnApplyClicked)) then begin
    // extract basic font attributes from the font name in XLFD format
    FontName := gtk_font_selection_dialog_get_font_name(
                                    pgtkfontselectiondialog(theDialog.Handle));
    ALogFont:=XLFDNameToLogFont(FontName);
    TFontDialog(theDialog).Font.Assign(ALogFont);
    // set the font name in XLFD format
    // a font name in XLFD format overrides in the gtk interface all other font
    // settings.
    TFontDialog(theDialog).Font.Name := FontName;
    TFontDialog(theDialog).OnApplyClicked(theDialog);
  end;
end;

{-------------------------------------------------------------------------------
  function gtkDialogCloseQueryCB
  Params: widget: PGtkWidget; data: gPointer
  Result: GBoolean

  This function is called, before a commondialog is destroyed
-------------------------------------------------------------------------------}
function gtkDialogCloseQueryCB(widget: PGtkWidget; data: gPointer): GBoolean;
  cdecl;
var
  theDialog : TCommonDialog;
  CanClose: boolean;
begin
  Result := False; // true = do nothing, false = destroy or hide window
  // data is not the commondialog. Get it manually.
  theDialog := TCommonDialog(GetLCLObject(Widget));
  if theDialog=nil then exit;
  if theDialog.OnCanClose<>nil then begin
    CanClose:=True;
    theDialog.OnCanClose(theDialog,CanClose);
    Result:=not CanClose;
  end;
  if not Result then begin
    StoreCommonDialogSetup(theDialog);
    DestroyCommonDialogAddOns(theDialog);
  end;
end;

{-------------------------------------------------------------------------------
  procedure UpdateDetailView
  Params: OpenDialog: TOpenDialog
  Result: none

  Shows some OS dependent information about the current file
-------------------------------------------------------------------------------}
procedure UpdateDetailView(OpenDialog: TOpenDialog);
var
  FileDetailLabel: PGtkWidget;
  Filename, OldFilename, Details: string;
begin
  Filename:=
    gtk_file_selection_get_filename(PGtkFileSelection(OpenDialog.Handle));
  OldFilename:=OpenDialog.Filename;
  if Filename=OldFilename then exit;
  OpenDialog.Filename:=Filename;
  // tell application, that selection has changed
  if OpenDialog.OnSelectionChange<>nil then
    OpenDialog.OnSelectionChange(OpenDialog);
  if (OpenDialog.OnFolderChange<>nil)
  and (ExtractFilePath(Filename)<>ExtractFilePath(OldFilename)) then
    OpenDialog.OnFolderChange(OpenDialog);
  // show some information
  FileDetailLabel:=gtk_object_get_data(PGtkObject(OpenDialog.Handle),
                                       'FileDetailLabel');
  if FileDetailLabel=nil then exit;
  if FileExists(Filename) then begin
    Details:=GetFileDescription(Filename);
  end else begin
    Details:=Format(rsFileInfoFileNotFound, [Filename]);
  end;
  gtk_label_set_text(PGtkLabel(FileDetailLabel),PChar(Details));
end;

{-------------------------------------------------------------------------------
  function GTKDialogKeyUpDownCB
  Params: Widget: PGtkWidget; Event : pgdkeventkey; Data: gPointer
  Result: GBoolean

  This function is called, whenever a key is pressed or released in a common
  dialog window
-------------------------------------------------------------------------------}
function GTKDialogKeyUpDownCB(Widget: PGtkWidget; Event : pgdkeventkey;
  Data: gPointer) : GBoolean; cdecl;
begin
  Result:=false;

  case Event^.theType of

  GDK_KEY_RELEASE, GDK_KEY_PRESS:
    begin
      if Event^.KeyVal=GDK_Escape then begin
        StoreCommonDialogSetup(TCommonDialog(data));
        TCommonDialog(data).UserChoice:=mrCancel;
      end;
      if (TCommonDialog(data) is TOpenDialog) then begin
        UpdateDetailView(TOpenDialog(data));
      end;
    end;
    
  end;
end;

{-------------------------------------------------------------------------------
  function GTKDialogRealizeCB
  Params: Widget: PGtkWidget; Data: Pointer
  Result: GBoolean

  This function is called, whenever a commondialog window is realized
-------------------------------------------------------------------------------}
function GTKDialogRealizeCB(Widget: PGtkWidget; Data: Pointer): GBoolean; cdecl;
begin
  gdk_window_set_events(GetControlWindow(Widget),
    gdk_window_get_events(GetControlWindow(Widget))
      or GDK_KEY_RELEASE_MASK or GDK_KEY_PRESS_MASK);
  Result:=true;
end;

{-------------------------------------------------------------------------------
  function GTKDialogFocusInCB
  Params: widget: PGtkWidget; data: gPointer
  Result: GBoolean

  This function is called, when a widget of a commondialog gets focus
-------------------------------------------------------------------------------}
function GTKDialogFocusInCB(widget: PGtkWidget; data: gPointer): GBoolean;
  cdecl;
var
  theDialog: TCommonDialog;
begin
  Result:=false;
  theDialog:=TCommonDialog(GetLCLObject(Widget));
  if (theDialog is TOpenDialog) then begin
    UpdateDetailView(TOpenDialog(theDialog));
  end;
end;

{-------------------------------------------------------------------------------
  function GTKDialogSelectRowCB
  Params: widget: PGtkWidget; data: gPointer
  Result: GBoolean

  This function is called, whenever a row is selected in a commondialog
-------------------------------------------------------------------------------}
function GTKDialogSelectRowCB(widget: PGtkWidget; Row, Column: gInt;
  bevent: pgdkEventButton; data: gPointer): GBoolean; cdecl;
var
  theDialog: TCommonDialog;
begin
  Result:=false;
  theDialog:=TCommonDialog(GetLCLObject(Widget));
  if (theDialog is TOpenDialog) then begin
    UpdateDetailView(TOpenDialog(theDialog));
  end;
end;

{-------------------------------------------------------------------------------
  function GTKDialogMenuActivateCB
  Params: widget: PGtkWidget; data: gPointer
  Result: GBoolean

  This function is called, whenever a menu of a commondialog is activated
-------------------------------------------------------------------------------}
function GTKDialogMenuActivateCB(widget: PGtkWidget; data: gPointer): GBoolean;
  cdecl;
var
  theDialog: TCommonDialog;

  procedure CheckFilterActivated(FilterWidget: PGtkWidget);
  var AFilterEntry: PFileSelFilterEntry;
  begin
    if FilterWidget=nil then exit;
    AFilterEntry:=gtk_object_get_data(PGtkObject(FilterWidget),
                                      'LCLIsFilterMenuItem');
    if (AFilterEntry<>nil) and (AFilterEntry^.Mask<>nil) then begin
      gtk_file_selection_complete(GTK_FILE_SELECTION(theDialog.Handle),
        AFilterEntry^.Mask);
      UpdateDetailView(TOpenDialog(theDialog));
    end;
  end;
  
var
  AHistoryEntry: PFileSelHistoryEntry;
  FilterMenu, ActiveFilterMenuItem: PGtkWidget;
begin
  Result:=false;
  theDialog:=TCommonDialog(GetLCLObject(Widget));
  if (theDialog is TOpenDialog) then begin
    // check if history activated
    AHistoryEntry:=gtk_object_get_data(PGtkObject(Widget),
                                       'LCLIsHistoryMenuItem');
    if (AHistoryEntry<>nil) and (AHistoryEntry^.Filename<>nil) then begin
      // use has choosen a history file
      // -> select it in the filedialog
      gtk_file_selection_complete(GTK_FILE_SELECTION(theDialog.Handle),
        AHistoryEntry^.Filename);
      // restore filter
      if DirectoryExists(AHistoryEntry^.Filename) then begin
        FilterMenu:=gtk_object_get_data(PGtkObject(theDialog.Handle),
                                        'LCLFilterMenu');
        if FilterMenu<>nil then begin
          ActiveFilterMenuItem:=gtk_menu_get_active(GTK_MENU(FilterMenu));
          CheckFilterActivated(ActiveFilterMenuItem);
        end;
      end;
      UpdateDetailView(TOpenDialog(theDialog));
    end;
    // check if filter activated
    CheckFilterActivated(Widget);
  end;
end;

{-------------------------------------------------------------------------------
  function gtkDialogDestroyCB
  Params: widget: PGtkWidget; data: gPointer
  Result: GBoolean

  This function is called, when a commondialog is destroyed
-------------------------------------------------------------------------------}
function gtkDialogDestroyCB(widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
begin
  Result := True;
  TCommonDialog(data).UserChoice := mrAbort;
  TCommonDialog(data).Close;
end;

function gtkPressedCB( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  Result := True;
  EventTrace('pressed', data);
  Mess.msg := LM_PRESSED;
  Result := DeliverMessage(Data, Mess) = 0;
end;

function gtkEnterCB(widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
   Mess : TLMessage;
begin
  Result := True;
  EventTrace('enter', data);

  if csDesigning in TControl(Data).ComponentState then begin
    // stop the signal, so that the widget does not auto react
    gtk_signal_emit_stop_by_name(PGTKObject(Widget),'enter');
  end;

  Mess.msg := CM_MOUSEENTER;
writeln('gtkEnterCB');
  Result := DeliverMessage(Data, Mess) = 0;
end;

function gtkLeaveCB(widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  Result := True;
  EventTrace('leave', data);

  if csDesigning in TControl(Data).ComponentState then begin
    // stop the signal, so that the widget does not auto react
    gtk_signal_emit_stop_by_name(PGTKObject(Widget),'leave');
  end;

  Mess.msg := CM_MOUSELEAVE;
writeln('gtkLeaveCB');
  Result := DeliverMessage(Data, Mess) = 0;
end;

function gtkMoveCursorCB(widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  Result := True;
  EventTrace('move-cursor', data);
  Mess.msg := LM_MOVECURSOR;
  DeliverMessage(Data, Mess);
end;

function gtksize_allocateCB(widget: PGtkWidget; size :pGtkAllocation;
  data: gPointer) : GBoolean; cdecl;
begin
  Result:=false;
  
  EventTrace('size-allocate', data);

  with Size^ do Assert(False, Format('Trace:[gtksize_allocateCB] %s --> X: %d, Y: %d, Width: %d, Height: %d', [TObject(data).ClassName, X, Y, Width, Height]));
  
  if not (TObject(Data) is TControl) then begin
    // owner is not TControl -> ignore
    writeln('WARNING: gtksize_allocateCB: Data is not TControl. Data=',
            HexStr(Cardinal(Data),8));
    exit;
  end;
  
  { The gtk sends the size messages after the resizing. Therefore the parent
    widget is already resized, but the parent resize message will be emitted
    after all its childs. So, the gtk resizes in top-bottom order, just like the
    LCL. But it sends size messages in bottom-top order, which results in
    many resizes in the LCL.
    Therefore all resize messages between lcl and gtk are cached.
  }
  {$IFDEF VerboseSizeMsg}
  writeln('gtksize_allocateCB: ',
    TControl(Data).Name,':',TControl(Data).ClassName,
    ' widget=',HexStr(Cardinal(Widget),8),
    ' fixwidget=',HexStr(Cardinal(GetFixedWidget(Widget)),8),
    ' OldPos=',TControl(Data).Left,',',TControl(Data).Top,',',TControl(Data).Width,',',TControl(Data).Height);
  {$ENDIF}
  {$IFDEF VerboseFormPositioning}
  if TControl(Data) is TCustomForm then
    writeln('VFP gtksize_allocateCB: ',TControl(Data).ClassName,' ',Size^.X,',',Size^.Y);
  {$ENDIF}
  SaveSizeNotification(Widget);
  Result:=true;
end;

function gtksize_allocate_client(widget: PGtkWidget; size :pGtkAllocation;
  data: gPointer) : GBoolean; cdecl;
begin
  if (TObject(Data) is TWinControl) then begin
    {$IFDEF VerboseSizeMsg}
    writeln('gtksize_allocate_client: ',
      TControl(Data).Name,':',TControl(Data).ClassName,
      ' widget=',HexStr(Cardinal(Widget),8),
      ' NewSize=',Size^.Width,',',Size^.Height,
      ' Allocation=',widget^.Allocation.Width,',',Widget^.Allocation.Height,
      ' Requisiton=',widget^.Requisition.Width,',',Widget^.Requisition.Height
      );
    {$ENDIF}
  end else begin
    // owner is not TWinControl -> ignore
    writeln('WARNING: gtksize_allocate_client: Data is not TWinControl. Data=',
            HexStr(Cardinal(Data),8));
    Result:=false;
    exit;
  end;
  
  SaveClientSizeNotification(Widget);
  Result:=true;
end;

function gtkswitchpage(widget: PGtkWidget; page: Pgtkwidget; pagenum : integer;
  data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMNotify;
  T : tagNMHDR;
begin
  Result := True;
  EventTrace('switch-page', data);
  UpdateNoteBookClientWidget(TObject(Data));
  Mess.Msg := LM_NOTIFY;
  //this is the
  T.code := TCN_SELCHANGE;//(0-550)-1;
  T.hwndfrom := longint(widget);
  T.idfrom := pagenum;  //use this to set pageindex to the correct page.
  Mess.NMHdr := @T;
  Result := DeliverMessage(Data, Mess) = 0;
end;


function gtkconfigureevent( widget: PGtkWidget; event : PgdkEventConfigure;
  data: gPointer) : GBoolean; cdecl;
var
  Allocation : PGtkAllocation;
begin

  { This signal is emitted for top level controls only, i.e. only controls
    that are not children. Thus, we register this event only for forms.
    This event is fired when the form is sized, moved or changes Z order.
  }

  New(Allocation);
  try
    with Allocation^ do begin
      X:= Event^.X;
      Y:= Event^.Y;
      Width:= Event^.Width;
      Height:= Event^.Height;
    end;  
    Result:= gtksize_allocateCB( Widget, Allocation, Data);
  finally
    Dispose(Allocation);
  end;    
end;

function gtkreleasedCB( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMEssage;
begin
  Result := True;
  EventTrace('released', data);
  Mess.msg := LM_RELEASED;
  Result := DeliverMessage(Data, Mess) = 0;
end;

function gtkInsertText( widget: PGtkWidget; char : pChar; NewTextLength : Integer; Position : pgint; data: gPointer) : GBoolean; cdecl;
var
  Memo: TCustomMemo;
  CurrLength, CutLength: integer;
begin
  Result := True;

  { GTK does not provide its own max. length for memos
    we have to do our own. }

  if TControl(Data) is TCustomMemo then begin
    Memo:= TCustomMemo(Data);
    if Memo.MaxLength < 0 then Exit;
    
    CurrLength:= gtk_text_get_length(PGtkText(widget));
    if CurrLength + NewTextLength <= Memo.MaxLength then Exit;
    
    CutLength:= CurrLength + NewTextLength - Memo.MaxLength;
    
    if NewTextLength - CutLength > 0 then begin
      gtk_editable_insert_text(PGtkEditable(widget), char, NewTextLength - CutLength, Position);
    end;
    
    gtk_signal_emit_stop_by_name(PGtkObject(widget), 'insert_text');
  end;
end;

function gtkDeleteText( widget: PGtkWidget; Startpos, EndPos : Integer; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Delete Text', data);
  Mess.msg := LM_DELETETEXT;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function gtkSetEditable( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Set Editable', data);
  Mess.msg := LM_SETEDITABLE;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function gtkMoveWord( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
   Mess : TLMessage;
begin
  EventTrace('Move Word', data);
  Mess.msg := LM_MOVEWORD;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function gtkMovePage( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Move Page', data);
  Mess.msg := LM_MOVEPAGE;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function gtkMoveToRow( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Move To Row!!', data);
  Mess.msg := LM_MOVETOROW;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function gtkMoveToColumn( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('MoveToColumn', data);
  Mess.msg := LM_MOVETOCOLUMN;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function gtkKillChar( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Kill Char', data);
  Mess.msg := LM_KILLCHAR;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function gtkKillWord( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Kill Word', data);
  Mess.msg := LM_KILLWORD;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function gtkKillLine( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Kill Line', data);
  Mess.msg := LM_KILLLINE;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function gtkCutToClip( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Cut to clip', data);
  Mess.msg := LM_CUTTOCLIP;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function gtkCopyToClip( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Copy to Clip', data);
  Mess.msg := LM_COPYTOCLIP;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function gtkPasteFromClip( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Paste from clip', data);
  Mess.msg := LM_PASTEFROMCLIP;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function gtkValueChanged(widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Value changed', data);
  Mess.msg := LM_CHANGED;
  Result := DeliverMessage(Data, Mess) = 0;
end;

{------------------------------------------------------------------------------
  Method: gtkTimerCB
  Params:  Data - pointer TGtkITimerInfo structure
  Returns: 1    - 1 tells gtk to restart the timer
           0    - 0 will stop the gtk timer

  Callback for gtk timer.
  
  WARNING: There seems to be a bug in gtk-1.2.x which breaks 
           gtk_timeout_remove so we have to dispose data here & return 0
           (s.a. KillTimer).
 ------------------------------------------------------------------------------}
function gtkTimerCB(Data: gPointer): gint; cdecl;
var
  TimerInfo: PGtkITimerinfo;
begin
  EventTrace ('TimerCB', nil);
  Result := 0;  // assume: timer will stop
  
  TimerInfo:=PGtkITimerinfo(Data);

  if (FTimerData=nil) or (FTimerData.IndexOf(Data)<0) then begin
    {$IFDEF VerboseTimer}
    writeln('gtkTimerCB Timer was killed: TimerInfo=',HexStr(Cardinal(TimerInfo),8));
    {$ENDIF}
    // timer was killed
    Result:=0; // stop timer
  end else begin
    {$IFDEF VerboseTimer}
    writeln('gtkTimerCB Timer Event: TimerInfo=',HexStr(Cardinal(TimerInfo),8));
    {$ENDIF}
    if TimerInfo^.TimerFunc <> nil
    then begin
      // Call users timer function
      TimerInfo^.TimerFunc;
      Result:=1; // timer will go on
    end
    else begin
      Result := 0;       // stop timer
    end;
  end;
  
  if (Result<>0) and (FTimerData.IndexOf(Data)<0) then begin
    // timer was killed
    // -> stop timer
    Result:=0;
  end;

  if Result=0 then begin
    {$IFDEF VerboseTimer}
    writeln('gtkTimerCB Timer was STOPPED: TimerInfo=',HexStr(Cardinal(TimerInfo),8));
    {$ENDIF}
    // timer will be stopped
    // -> free timer data, if not already done
    if (FTimerData.IndexOf(Data)>=0) then begin
      FTimerData.Remove(Data);
      Dispose (TimerInfo);     // free memory with timer data
    end;
  end;
end;

function gtkFocusInNotifyCB (widget : PGtkWidget; event : PGdkEvent;
  data : gpointer) : GBoolean; cdecl;
var
  MessI : TLMEnter;
begin
//writeln('[gtkFocusInNotifyCB] ',TObject(data).ClassName);
  EventTrace ('FocusInNotify (alias Enter)', data);
  
  if csDesigning in TControl(Data).ComponentState then begin
    // stop the signal, so that the widget does not auto react
    gtk_signal_emit_stop_by_name(PGTKObject(Widget),'focus-in-event');
  end;

  MessI.msg := LM_Enter;
  Result:= DeliverMessage(Data, MessI) = 0;
end;

function gtkFocusOutNotifyCB (widget : PGtkWidget; event : PGdkEvent;
  data : gpointer) : GBoolean; cdecl;
var
  MessI : TLMExit;
begin
//writeln('[gtkFocusOutNotifyCB] ',TObject(data).ClassName);
  EventTrace ('FocusOutNotify (alias Exit)', data);

  if csDesigning in TControl(Data).ComponentState then begin
    // stop the signal, so that the widget does not auto react
    gtk_signal_emit_stop_by_name(PGTKObject(Widget),'focus-out-event');
  end;

  MessI.msg := LM_Exit;
  Result:= DeliverMessage(Data, MessI) = 0;
end;

{
  Msg : Cardinal;
  ScrollCode : SmallInt;
  Pos : SmallInt;
  ScrollBar : HWND;
  Result : LongInt;
}

function GTKHScrollCB(Adjustment: PGTKAdjustment; data: GPointer): GBoolean; cdecl;
var
  Msg: TLMHScroll;
  OldValue,
  V, U, {L,}
  StepI, PageI{, Page}: Integer;
  Scroll: PGTKHScrollBar;  
  X, Y: GInt;
  Mask: TGdkModifierType;
begin
  Assert(False, Format('Trace:[GTKHScrollCB] Value: %d', [Round(Adjustment^.Value)]));

  OldValue := Integer(gtk_object_get_data(PGTKObject(Adjustment), 'OldValue'));
  gtk_object_set_data(PGTKObject(Adjustment), 'OldValue',
                      Pointer(Longint(Round(Adjustment^.Value))));
  Scroll := gtk_object_get_data(PGTKObject(Adjustment), 'ScrollBar');

  // Get rounded values
  with Adjustment^ do
  begin
    V := Round(Value);
    U := Round(Upper);
    //L := Round(Lower);
    StepI := Round(Step_Increment);
    PageI := Round(Page_Increment);
    //Page := Round(Page_Size);
  end;
  
  // get keystates
  if Scroll <> nil 
  then gdk_window_get_pointer(GetControlWindow(Scroll), @X, @Y, @Mask);

  with Msg do
  begin
    msg := LM_HSCROLL;
    // Get scrollcode
    if ssLeft in GTKEventState2ShiftState(Mask)
    then ScrollCode := SB_THUMBTRACK
    else if V - OldValue = StepI 
    then ScrollCode := SB_LINERIGHT
    else if OldValue - V = StepI
    then ScrollCode := SB_LINELEFT
    else if V - OldValue = PageI 
    then ScrollCode := SB_PAGERIGHT
    else if OldValue - V = PageI
    then ScrollCode := SB_PAGELEFT
    else if V >= U
    then ScrollCode := SB_ENDSCROLL
    else ScrollCode := SB_THUMBPOSITION;
    Pos := V;
    ScrollBar := HWND(Scroll);
  end;
  Result := DeliverMessage(Data, Msg) = 0;
end;

function GTKVScrollCB(Adjustment: PGTKAdjustment;
  data: GPointer): GBoolean; cdecl;
var
  Msg: TLMVScroll;
  OldValue,
  V, U, {L,}
  StepI, PageI{, Page}: Integer;
  Scroll: PGTKHScrollBar;  
  X, Y: GInt;
  Mask: TGdkModifierType;
begin
  Assert(False, Format('Trace:[GTKVScrollCB] Value: %d', [Round(Adjustment^.Value)]));

  OldValue := Integer(gtk_object_get_data(PGTKObject(Adjustment), 'OldValue'));
  gtk_object_set_data(PGTKObject(Adjustment), 'OldValue',
                      Pointer(Longint(Round(Adjustment^.Value))));
  Scroll := gtk_object_get_data(PGTKObject(Adjustment), 'ScrollBar');

  // Get rounded values
  with Adjustment^ do
  begin
    V := Round(Value);
    U := Round(Upper);
    //L := Round(Lower);
    StepI := Round(Step_Increment);
    PageI := Round(Page_Increment);
    //Page := Round(Page_Size);
  end;
  
  // get keystates
  if Scroll <> nil then
    gdk_window_get_pointer(GetControlWindow(Scroll), @X, @Y, @Mask)
  else
    Mask:=0;

  with Msg do
  begin
    msg := LM_VSCROLL;
    // Get scrollcode
    if ssLeft in GTKEventState2ShiftState(Mask)
    then ScrollCode := SB_THUMBTRACK
    else if V - OldValue = StepI 
    then ScrollCode := SB_LINEDOWN
    else if OldValue - V = StepI
    then ScrollCode := SB_LINEUP
    else if V - OldValue = PageI 
    then ScrollCode := SB_PAGEDOWN
    else if OldValue - V = PageI
    then ScrollCode := SB_PAGEUP
    else if V >= U
    then ScrollCode := SB_ENDSCROLL
    else ScrollCode := SB_THUMBPOSITION;
    Pos := V;
    ScrollBar := HWND(Scroll);
  end;
  Result := DeliverMessage(Data, Msg) = 0;
end;

{------------------------------------------------------------------------------
  Function: GTKKeySnooper
  Params:  Widget: The widget for which this event is fired
           Event:  The keyevent data
           FuncData: the user parameter passed when the snooper was installed
  Returns: True if other snoopers shouldn't handled

  Keeps track of which keys are pressed. The keycode is casted to a pointer and
  if it exists in the KeyStateList, it is pressed.
 ------------------------------------------------------------------------------}
function GTKKeySnooper(Widget: PGtkWidget; Event: PGdkEventKey;
  FuncData: gPointer): gInt; cdecl;
type
  PList = ^TList;
var
  //Msg: TLMKey;
  KeyCode, VirtKeyCode: Word;
  ListCode: Integer;
  Toggle, Extended, SysKey: Boolean;
  ShiftState: TShiftState;
  KeyStateList: TList;
  
  procedure UpdateExtraKeyState(ListCode: integer; Pressed: boolean);
  begin
    if Pressed then begin
      if KeyStateList.IndexOf(Pointer(ListCode)) = -1 then begin
        KeyStateList.Add(Pointer(ListCode));
      end;
    end else begin
      KeyStateList.Remove(Pointer(ListCode));
      // just remove the togglekey if present
      KeyStateList.Remove(Pointer(ListCode or KEYMAP_TOGGLE));
      // just remove the extendedkey if present
      KeyStateList.Remove(Pointer(ListCode or KEYMAP_EXTENDED));
    end;
  end;
  
begin
  GetGTKKeyInfo(Event, KeyCode, VirtKeyCode, SysKey, Extended, Toggle);
  KeyStateList:=PList(FuncData)^;
  
  // update extra keys like Ctrl, Shift, Alt
  ShiftState := GTKEventState2ShiftState(Event^.State);
  UpdateExtraKeyState(VK_CONTROL,ssCtrl in ShiftState);
  UpdateExtraKeyState(VK_SHIFT,ssShift in ShiftState);
  UpdateExtraKeyState(VK_MENU,ssAlt in ShiftState);

  with Event^ do
  begin
    ListCode:=KeyToListCode(KeyCode, VirtKeyCode, Extended);

    case theType of
      GDK_KEY_PRESS:
        begin
          if KeyStateList.IndexOf(Pointer(ListCode)) = -1
          then begin
            KeyStateList.Add(Pointer(ListCode));
            if Toggle then KeyStateList.Add(Pointer(ListCode or KEYMAP_TOGGLE));
          end;
          //else WriteLn(Format('WARNING: [GTKKeySnooper] Pressed key (0x%x) already pressed (LC=0x%x)', [KeyCode, ListCode]));
        end;
      GDK_KEY_RELEASE:
        begin
          {if KeyStateList.Remove(Pointer(ListCode)) = -1 then
            WriteLn(Format('WARNING: [GTKKeySnooper] Released key (0x%x) not pressed (LC=0x%x)', [KeyCode, ListCode]));}
          KeyStateList.Remove(Pointer(ListCode));
          // just remove the togglekey if present
          KeyStateList.Remove(Pointer(ListCode or KEYMAP_TOGGLE));
        end;
    else
      WriteLn(Format('ERROR: [GTKKeySnooper] Got unknown event %d', [theType]));  
    end;

    Assert(False, Format('trace:' +
//    WriteLN(Format(
      '[GTKKeySnooper] Type %d, window $%x, send_event %d, time %d, state %d, keyval %d, length %d, string %s', 
      [ thetype, Integer(window), send_event, time, state, keyval, length, thestring])
    );
  end;
  Result := 0;
end;

function gtkYearChanged(Widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  MSG: TLMessage;
begin
  Result := True;
  EventTrace('year changed', data);
  MSG.Msg := LM_YEARCHANGED;
  Result := DeliverPostMessage(Data, MSG);
end;

{------------------------------------------------------------------------------
  ClipboardSelectionReceivedHandler

  copy the received selection data record and buffer to
  internal record and buffer (ClipboardSelectionData)
 ------------------------------------------------------------------------------}
procedure ClipboardSelectionReceivedHandler(TargetWidget: PGtkWidget;
  SelectionData: PGtkSelectionData; TimeID: cardinal; Data: Pointer); cdecl;
var TempBuf: Pointer;
  c: PClipboardEventData;
  i: integer;
begin
  // at any time there can be several requests
  // find the request with the correct TimeID
  i:=ClipboardSelectionData.Count-1;
  while (i>=0) do begin
    c:=PClipboardEventData(ClipboardSelectionData[i]);
    if c^.TimeID=TimeID then break;
    dec(i);
  end;
  {$IFDEF DEBUG_CLIPBOARD}
  writeln('[ClipboardSelectionReceivedHandler] A TimeID=',TimeID,' RequestIndex=',i,
    ' selection=',SelectionData^.selection,
    ' target=',SelectionData^.Target,
    ' theType=',SelectionData^.theType,
    ' format=',SelectionData^.format,
    ' len=',SelectionData^.length
    );
  {$ENDIF}
  if i<0 then exit;
  // free old data
  if (c^.Data.Data<>nil) then FreeMem(c^.Data.Data);
  // copy the information
  c^.Data:=SelectionData^;
  // copy the raw data to an internal buffer (the gtk buffer will be destroyed
  // right after this event)
  {$IFDEF DEBUG_CLIPBOARD}
  writeln('[ClipboardSelectionReceivedHandler] B DataLen=',c^.Data.Length);
  {$ENDIF}
  if (c^.Data.Data<>nil)
  and (c^.Data.Length>0) then begin
    GetMem(TempBuf,c^.Data.Length);
    Move(c^.Data.Data^,TempBuf^,c^.Data.Length);
    c^.Data.Data:=TempBuf;
    {$IFDEF DEBUG_CLIPBOARD}
    writeln('[ClipboardSelectionReceivedHandler] C FirstCharacter=',ord(PChar(c^.Data.Data)[0]));
    {$ENDIF}
  end else
    c^.Data.Data:=nil;
end;

{------------------------------------------------------------------------------
  ClipboardSelectionRequestHandler

  This signal is emitted if someone requests the clipboard data.
  Since the lcl clipboard caches all requests this will typically be another
  application.
 ------------------------------------------------------------------------------}
procedure ClipboardSelectionRequestHandler(TargetWidget: PGtkWidget;
  SelectionData: PGtkSelectionData; Info: cardinal; TimeID: cardinal;
  Data: Pointer); cdecl;
type
  PGdkAtom = ^TGdkAtom;
var ClipboardType: TClipboardType;
  MemStream: TMemoryStream;
  FormatID: cardinal;
  Buffer: Pointer;
  BufLength: integer;
  P: PChar;
  BitCount: integer;
begin
  {$IFDEF DEBUG_CLIPBOARD}
  writeln('*** [ClipboardSelectionRequestHandler] START');
  {$ENDIF}
  if SelectionData^.Target=0 then exit;
  for ClipboardType:=Low(TClipboardType) to High(TClipboardType) do
    if SelectionData^.Selection=ClipboardTypeAtoms[ClipboardType] then begin
      if Assigned(ClipboardHandler[ClipboardType]) then begin
        // handler found for this of clipboard
        // now create a stream and find a supported format
        {$IFDEF DEBUG_CLIPBOARD}
        p:=gdk_atom_name(SelectionData^.Target);
        writeln('[ClipboardSelectionRequestHandler] ',ClipboardTypeName[ClipboardType],' Format=',p,' ID=',SelectionData^.Target);
        g_free(p);
        {$ENDIF}
        MemStream:=TMemoryStream.Create;
        try
          // the gtk-interface provides automatically some formats, that the lcl
          // does not know. Wrapping them to lcl formats ...
          FormatID:=SelectionData^.Target;
          if ((FormatID=gdk_atom_intern('COMPOUND_TEXT',1))
            and (ClipboardExtraGtkFormats[ClipboardType][gfCOMPOUND_TEXT]))
          or ((FormatID=gdk_atom_intern('STRING',1))
            and (ClipboardExtraGtkFormats[ClipboardType][gfSTRING]))
          or ((FormatID=gdk_atom_intern('TEXT',1))
            and (ClipboardExtraGtkFormats[ClipboardType][gfTEXT]))
          then
            FormatID:=gdk_atom_intern('text/plain',0);
            
          {$IFDEF DEBUG_CLIPBOARD}
          writeln('[ClipboardSelectionRequestHandler] FormatID=',FormatID,' CompoundText=',gdk_atom_intern('COMPOUND_TEXT',1),' ',ClipboardExtraGtkFormats[ClipboardType][gfCOMPOUND_TEXT]);
          {$ENDIF}
          // get the requested data by calling the handler for this clipboard type
          ClipboardHandler[ClipboardType](FormatID,MemStream);
          MemStream.Position:=0;

          // build clipboard data for gtk
          Buffer:=nil;
          BufLength:=0;
          BitCount:=8;
          
          // if the format was wrapped, transform it back
          if (FormatID=gdk_atom_intern('text/plain',1)) then begin
            if (SelectionData^.Target=gdk_atom_intern('COMPOUND_TEXT',1)) then
            begin
              // transform text/plain to COMPOUND_TEXT
              P:=StrAlloc(MemStream.Size+1);
              MemStream.Read(P^,MemStream.Size);
              P[MemStream.Size]:=#0;
              gdk_string_to_compound_text(P,@SelectionData^.theType,
                @SelectionData^.Format,@Buffer,@BufLength);
              StrDispose(P);
              gtk_selection_data_set(SelectionData,SelectionData^.Target,
                SelectionData^.Format,Buffer,BufLength);
              gdk_free_compound_text(Buffer);
              exit;
            end;
          end;
          if Buffer=nil then begin
            {$IFDEF DEBUG_CLIPBOARD}
            writeln('[ClipboardSelectionRequestHandler] Default MemStream.Size=',MemStream.Size);
            {$ENDIF}
            BufLength:=MemStream.Size;
            if MemStream.Size>0 then begin
              GetMem(Buffer,MemStream.Size);
              MemStream.Read(Buffer^,MemStream.Size);
              {SetLength(s,MemStream.Size);
              MemStream.Position:=0;
              MemStream.Read(s[1],MemStream.Size);
              writeln(' >>> "',s,'"');}
            end;
          end;
          {$IFDEF DEBUG_CLIPBOARD}
          writeln('[ClipboardSelectionRequestHandler] Len=',BufLength);
          {$ENDIF}
          gtk_selection_data_set(SelectionData,SelectionData^.Target,BitCount,
            Buffer,BufLength);
          if Buffer<>nil then
            FreeMem(Buffer);
        finally
          MemStream.Free;
        end;
      end;
      break;
    end;
end;

{------------------------------------------------------------------------------
  ClipboardSelectionLostOwnershipHandler

  This signal is emitted if another application gets the clipboard ownership.
 ------------------------------------------------------------------------------}
function ClipboardSelectionLostOwnershipHandler(TargetWidget: PGtkWidget;
  EventSelection: PGdkEventSelection;  Data: Pointer): cardinal; cdecl;
var ClipboardType: TClipboardType;
begin
  //writeln('*** [ClipboardSelectionLostOwnershipHandler] ',hexstr(cardinal(targetwidget),8));
  for ClipboardType:=Low(TClipboardType) to High(TClipboardType) do
    if EventSelection^.Selection=ClipboardTypeAtoms[ClipboardType] then begin
      {$IFDEF DEBUG_CLIPBOARD}
      writeln('*** [ClipboardSelectionLostOwnershipHandler] ',ClipboardTypeName[ClipboardType]);
      {$ENDIF}
      if (ClipboardWidget<>nil)
      and (gdk_selection_owner_get(ClipboardTypeAtoms[ClipboardType])
        <> GetControlWindow(ClipboardWidget))
      and Assigned(ClipboardHandler[ClipboardType]) then begin
        // handler found for this type of clipboard
        {$IFDEF DEBUG_CLIPBOARD}
        writeln('[ClipboardSelectionLostOwnershipHandler] ',ClipboardTypeName[ClipboardType]);
        {$ENDIF}
        ClipboardHandler[ClipboardType](0,nil);
        ClipboardHandler[ClipboardType]:=nil;
      end;
      break;
    end;
  Result:=1;
end;

{-------------------------------------------------------------------------------
  Procedure GTKStyleChanged(Widget: PGtkWidget; previous_style : PGTKStyle;
    Data: Pointer); cdecl;

  Handler for style changes. For example the user changes the the theme.
-------------------------------------------------------------------------------}
Procedure GTKStyleChanged(Widget: PGtkWidget; previous_style : PGTKStyle; Data: Pointer); cdecl;
begin
  EventTrace('style-set', nil);
      ReleaseStyle('button');
      ReleaseStyle('radiobutton');
      ReleaseStyle('checkbox');
      ReleaseStyle('menu');
      ReleaseStyle('menuitem');
      ReleaseStyle('scrollbar');
      ReleaseStyle('tooltip');
      ReleaseStyle('default');
      ReleaseStyle('window');
end;

{$I gtkDragCallback.inc}
{$I gtkListViewCallback.inc}
{$I gtkComboBoxCallback.inc}

{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}


{ =============================================================================

  $Log$
  Revision 1.170  2003/03/16 09:41:06  mattias
  fixed checking menuitems

  Revision 1.169  2003/03/09 21:13:32  mattias
  localized gtk interface

  Revision 1.168  2003/03/09 17:44:12  mattias
  finshed Make Resourcestring dialog and implemented TToggleBox

  Revision 1.167  2003/02/18 22:56:23  mattias
  fixed key grabbing

  Revision 1.166  2003/02/04 11:44:13  mattias
  fixed modified and loading xpms for button glyphs

  Revision 1.165  2003/02/03 22:28:08  mattias
  small bugfixes and fixed non checked menu items activate

  Revision 1.164  2003/01/27 13:49:16  mattias
  reduced speedbutton invalidates, added TCanvas.Frame

  Revision 1.163  2003/01/24 11:58:00  mattias
  fixed clipboard waiting and kwrite targets

  Revision 1.162  2003/01/06 10:51:41  mattias
  freeing stopped external tools

  Revision 1.161  2002/12/28 12:42:38  mattias
  focus fixes, reduced lpi size

  Revision 1.160  2002/11/23 13:48:44  mattias
  added Timer patch from Vincent Snijders

  Revision 1.159  2002/11/21 18:49:53  mattias
  started OnMouseEnter and OnMouseLeave

  Revision 1.158  2002/11/16 11:22:57  mbukovjan
  Fixes to MaxLength. TCustomMemo now has MaxLength, too.

  Revision 1.157  2002/11/05 20:03:42  lazarus
  MG: implemented hints

  Revision 1.156  2002/11/02 22:25:36  lazarus
  MG: implemented TMethodList and Application Idle handlers

  Revision 1.155  2002/10/23 15:59:25  lazarus
  MG: fixed radiobutton mousedown after

  Revision 1.154  2002/10/22 12:12:08  lazarus
  MG: accelerators are now shared between non modal forms

  Revision 1.153  2002/10/21 22:12:47  lazarus
  MG: fixed frmactivate

  Revision 1.152  2002/10/20 21:49:09  lazarus
  MG: fixes for fpc1.1

  Revision 1.151  2002/10/20 19:03:56  lazarus
  AJ: minor fixes for FPC 1.1

  Revision 1.150  2002/10/17 21:00:17  lazarus
  MG: fixed uncapturing of mouse

  Revision 1.149  2002/10/17 15:09:31  lazarus
  MG: made mouse capturing more strict

  Revision 1.148  2002/10/15 16:01:36  lazarus
  MG: fixed timers

  Revision 1.147  2002/10/15 07:01:29  lazarus
  MG: fixed timer checking

  Revision 1.146  2002/10/14 19:00:49  lazarus
  MG: fixed zombie timers

  Revision 1.145  2002/10/11 07:28:03  lazarus
  MG: gtk interface now sends keyboard events via DeliverMessage

  Revision 1.144  2002/10/10 08:51:13  lazarus
  MG: added paint messages for some gtk internal widgets

  Revision 1.143  2002/10/09 10:22:54  lazarus
  MG: fixed client origin coordinates

  Revision 1.142  2002/10/07 07:00:03  lazarus
  MG: fixed stopping keypress event if handled by LCL

  Revision 1.141  2002/10/06 20:24:27  lazarus
  MG: fixed stopping keypress event if handled by LCL

  Revision 1.140  2002/10/04 20:46:52  lazarus
  MG: improved TComboBox.SetItemIndex

  Revision 1.139  2002/10/04 16:38:15  lazarus
  MG: no OnChange event when app sets Text of TComboBox

  Revision 1.138  2002/10/03 14:47:31  lazarus
  MG: added TComboBox.OnPopup+OnCloseUp+ItemWidth

  Revision 1.137  2002/09/30 22:42:54  lazarus
  MG: deactivated transient modal forms

  Revision 1.136  2002/09/30 20:37:09  lazarus
  MG: fixed transient of modal forms

  Revision 1.135  2002/09/30 20:19:12  lazarus
  MG: fixed flickering of modal forms

  Revision 1.134  2002/09/30 09:26:42  lazarus
  MG: added DoSaveAll before CloseAll

  Revision 1.133  2002/09/29 15:08:39  lazarus
  MWE: Applied patch from "Andrew Johnson" <aj_genius@hotmail.com>
    Patch includes:
      -fixes Problems with hiding modal forms
      -temporarily fixes TCustomForm.BorderStyle in bsNone
      -temporarily fixes problems with improper tabbing in TSynEdit

  Revision 1.132  2002/09/27 20:52:23  lazarus
  MWE: Applied patch from "Andrew Johnson" <aj_genius@hotmail.com>

  Here is the run down of what it includes -

   -Vasily Volchenko's Updated Russian Localizations

   -improvements to GTK Styles/SysColors
   -initial GTK Palette code - (untested, and for now useless)

   -Hint Windows and Modal dialogs now try to stay transient to
    the main program form, aka they stay on top of the main form
    and usually minimize/maximize with it.

   -fixes to Form BorderStyle code(tool windows needed a border)

   -fixes DrawFrameControl DFCS_BUTTONPUSH to match Win32 better
    when flat

   -fixes DrawFrameControl DFCS_BUTTONCHECK to match Win32 better
    and to match GTK theme better. It works most of the time now,
    but some themes, noteably Default, don't work.

   -fixes bug in Bitmap code which broke compiling in NoGDKPixbuf
    mode.

   -misc other cleanups/ fixes in gtk interface

   -speedbutton's should now draw correctly when flat in Win32

   -I have included an experimental new CheckBox(disabled by
    default) which has initial support for cbGrayed(Tri-State),
    and WordWrap, and misc other improvements. It is not done, it
    is mostly a quick hack to test DrawFrameControl
    DFCS_BUTTONCHECK, however it offers many improvements which
    can be seen in cbsCheck/cbsCrissCross (aka non-themed) state.

   -fixes Message Dialogs to more accurately determine
    button Spacing/Size, and Label Spacing/Size based on current
    System font.
   -fixes MessageDlgPos, & ShowMessagePos in Dialogs
   -adds InputQuery & InputBox to Dialogs

   -re-arranges & somewhat re-designs Control Tabbing, it now
    partially works - wrapping around doesn't work, and
    subcontrols(Panels & Children, etc) don't work. TabOrder now
    works to an extent. I am not sure what is wrong with my code,
    based on my other tests at least wrapping and TabOrder SHOULD
    work properly, but.. Anyone want to try and fix?

   -SynEdit(Code Editor) now changes mouse cursor to match
    position(aka over scrollbar/gutter vs over text edit)

   -adds a TRegion property to Graphics.pp, and Canvas. Once I
    figure out how to handle complex regions(aka polygons) data
    properly I will add Region functions to the canvas itself
    (SetClipRect, intersectClipRect etc.)

   -BitBtn now has a Stored flag on Glyph so it doesn't store to
    lfm/lrs if Glyph is Empty, or if Glyph is not bkCustom(aka
    bkOk, bkCancel, etc.) This should fix most crashes with older
    GDKPixbuf libs.

  Revision 1.131  2002/09/18 17:07:27  lazarus
  MG: added patch from Andrew

  Revision 1.130  2002/09/16 15:56:01  lazarus
  Resize cursors in designer.

  Revision 1.129  2002/09/16 14:46:08  lazarus
  MG: renamed designerstr.pas to objinspstrconsts.pas

  Revision 1.128  2002/09/16 08:54:03  lazarus
  MG: gtk mlouse events can now be fetched before or after

  Revision 1.127  2002/09/10 06:49:19  lazarus
  MG: scrollingwincontrol from Andrew

  Revision 1.126  2002/09/09 17:41:19  lazarus
  MG: added multiselection to TTreeView

  Revision 1.125  2002/09/07 12:14:50  lazarus
  EchoMode for TCustomEdit. emNone not implemented for GTK+, falls back to emPassword
  behaviour.

  Revision 1.124  2002/09/06 19:45:10  lazarus
  Cleanups plus a fix to TPanel parent/drawing problem.

  Revision 1.123  2002/09/06 15:57:34  lazarus
  MG: fixed notebook client area, send messages and minor bugs

  Revision 1.122  2002/09/05 12:11:43  lazarus
  MG: TNotebook is now streamable

  Revision 1.121  2002/09/05 10:12:07  lazarus

  New dialog for multiline caption of TCustomLabel.
  Prettified TStrings property editor.
  Memo now has automatic scrollbars (not fully working), WordWrap and Scrollbars property
  Removed saving of old combo text (it broke things and is not needed). Cleanups.

  Revision 1.120  2002/09/03 08:07:20  lazarus
  MG: image support, TScrollBox, and many other things from Andrew

  Revision 1.119  2002/09/01 16:11:22  lazarus
  MG: double, triple and quad clicks now works

  Revision 1.118  2002/08/31 11:37:10  lazarus
  MG: fixed destroying combobox

  Revision 1.117  2002/08/31 07:58:21  lazarus
  MG: fixed resetting comobobox text

  Revision 1.116  2002/08/30 12:32:22  lazarus
  MG: MoveWindowOrgEx, Splitted FWinControls/FControls, TControl drawing, Better DesignerDrawing, ...

  Revision 1.115  2002/08/29 00:07:02  lazarus
  MG: fixed TComboBox and InvalidateControl

  Revision 1.114  2002/08/28 09:40:49  lazarus
  MG: reduced paint messages and DC getting/releasing

  Revision 1.113  2002/08/27 18:45:13  lazarus
  MG: propedits text improvements from Andrew, uncapturing, improved comobobox

  Revision 1.112  2002/08/25 13:45:58  lazarus
  MG: ignoring double clicks for components that dont want them

  Revision 1.111  2002/08/24 12:54:59  lazarus
  MG: fixed mouse capturing, OI edit focus

  Revision 1.110  2002/08/24 08:07:14  lazarus
  MG: fixed double click recognition

  Revision 1.109  2002/08/24 07:11:56  lazarus
  MG: reduced output

  Revision 1.108  2002/08/24 07:09:04  lazarus
  MG: fixed bracket hilighting

  Revision 1.107  2002/08/24 06:51:22  lazarus
  MG: from Andrew: style list fixes, autosize for radio/checkbtns

  Revision 1.106  2002/08/22 16:43:35  lazarus
  MG: improved theme support from Andrew

  Revision 1.105  2002/08/22 13:45:58  lazarus
  MG: fixed non AutoCheck menuitems and editor bookmark popupmenu

  Revision 1.104  2002/08/22 12:30:36  lazarus
  MG: fixed key events and changed pixmap loading

  Revision 1.103  2002/08/22 12:25:00  lazarus
  MG: fixed mouse events

  Revision 1.102  2002/08/22 12:13:01  lazarus
  MG: changed user input events from queue to direct

  Revision 1.101  2002/08/19 18:00:02  lazarus
  MG: design signals for gtk internal widgets

  Revision 1.100  2002/08/19 08:50:28  lazarus
  MG: fixed parser for Clx enums and empty param lists

  Revision 1.99  2002/08/17 15:45:33  lazarus
  MG: removed ClientRectBugfix defines

  Revision 1.98  2002/08/17 07:57:05  lazarus
  MG: added TPopupMenu.OnPopup and SourceEditor PopupMenu checks

  Revision 1.97  2002/08/15 13:37:57  lazarus
  MG: started menuitem icon, checked, radio and groupindex

  Revision 1.96  2002/08/04 08:17:30  lazarus
  MG: fixed normal events in design mode

  Revision 1.95  2002/08/04 07:44:44  lazarus
  MG: fixed xml reading writing of special chars

  Revision 1.94  2002/08/04 07:09:27  lazarus
  MG: fixed client events

  Revision 1.93  2002/07/29 13:26:57  lazarus
  MG: source notebook pagenames are now updated more often

  Revision 1.92  2002/07/23 07:40:51  lazarus
  MG: fixed get widget position for inherited gdkwindows

  Revision 1.91  2002/07/22 18:25:12  lazarus
  MG: reduced output

  Revision 1.90  2002/07/20 13:47:03  lazarus
  MG: fixed eventmask for realized windows

  Revision 1.89  2002/06/26 16:15:56  lazarus
  MG: fixed missing declaration

  Revision 1.88  2002/06/26 15:11:09  lazarus
  MG: added new tool: Guess misplaced $IFDEF/$ENDIF

  Revision 1.87  2002/06/21 17:54:23  lazarus
  MG: in design mode the mouse cursor is now also set for hidden gdkwindows

  Revision 1.86  2002/06/21 16:59:15  lazarus
  MG: TControl.Cursor is now set, reduced auto reaction of widgets in design mode

  Revision 1.85  2002/06/19 19:46:09  lazarus
  MG: Form Editing: snapping, guidelines, modified on move/resize, creating components in csDesigning, ...

  Revision 1.84  2002/06/11 13:41:09  lazarus
  MG: fixed mouse coords and fixed mouse clicked thru bug

  Revision 1.83  2002/06/09 14:00:41  lazarus
  MG: fixed persistent caret and implemented Form.BorderStyle=bsNone

  Revision 1.82  2002/06/09 07:08:43  lazarus
  MG: fixed window jumping

  Revision 1.81  2002/06/08 17:16:02  lazarus
  MG: added close buttons and images to TNoteBook and close buttons to source editor

  Revision 1.80  2002/06/06 14:41:29  lazarus
  MG: if completion form visible it will now get all synedit keys

  Revision 1.79  2002/06/05 12:33:57  lazarus
  MG: fixed fonts in XLFD format and styles

  Revision 1.78  2002/06/04 15:17:22  lazarus
  MG: improved TFont for XLFD font names

  Revision 1.77  2002/05/30 14:11:12  lazarus
  MG: added filters and history to TOpenDialog

  Revision 1.76  2002/05/29 21:44:38  lazarus
  MG: improved TCommon/File/OpenDialog, fixed TListView scrolling and broder

  Revision 1.75  2002/05/24 07:16:32  lazarus
  MG: started mouse bugfix and completed Makefile.fpc

  Revision 1.74  2002/05/13 14:47:01  lazarus
  MG: fixed client rectangles, TRadioGroup, RecreateWnd

  Revision 1.73  2002/05/10 06:05:56  lazarus
  MG: changed license to LGPL

  Revision 1.72  2002/05/09 12:41:29  lazarus
  MG: further clientrect bugfixes

  Revision 1.71  2002/05/06 08:50:36  lazarus
  MG: replaced logo, increased version to 0.8.3a and some clientrectbugfix

  Revision 1.70  2002/04/27 15:35:51  lazarus
  MG: fixed window shrinking

  Revision 1.69  2002/04/22 13:07:45  lazarus
  MG: fixed AdjustClientRect of TGroupBox

  Revision 1.68  2002/04/04 12:25:02  lazarus
  MG: changed except statements to more verbosity

  Revision 1.67  2002/03/31 22:01:37  lazarus
  MG: fixed unreleased/unpressed Ctrl/Alt/Shift

  Revision 1.66  2002/03/29 19:11:38  lazarus
  Added Triple Click
  Shane

  Revision 1.65  2002/03/27 00:33:54  lazarus
  MWE:
    * Cleanup in lmessages
    * Added Listview selection and notification events
    + introduced commctrl

  Revision 1.64  2002/03/25 17:59:20  lazarus
  GTK Cleanup
  Shane

  Revision 1.63  2002/03/23 19:05:52  lazarus
  MG: pascal lowercase for open new unit

  Revision 1.62  2002/03/16 21:40:55  lazarus
  MG: reduced size+move messages between lcl and interface

  Revision 1.61  2002/03/14 18:12:46  lazarus
  Mouse events fixes.

  Revision 1.60  2002/03/13 22:48:16  lazarus
  Constraints implementation (first cut) and sizig - moving system rework to
  better match Delphi/Kylix way of doing things (the existing implementation
  worked by acident IMHO :-)

  Revision 1.59  2002/01/01 18:38:36  lazarus
  MG: more wmsize messages :(

  Revision 1.58  2002/01/01 15:50:16  lazarus
  MG: fixed initial component aligning

  Revision 1.57  2001/12/28 15:12:02  lazarus
  MG: LM_SIZE and LM_MOVE messages are now send directly, not queued

  Revision 1.56  2001/12/17 11:09:48  lazarus
  MG: fixed typed but not selected filename in TOpenDialog

  Revision 1.55  2001/12/12 20:45:30  lazarus
  MG: added non existing filename to multiselection in TOpenDialog

  Revision 1.54  2001/12/12 20:19:19  lazarus

    Modified the the GTKFileSelection so that it will handle and use
    CTRL and SHIFT keys in a fashion similar to Windows.

  Revision 1.53  2001/12/12 15:12:31  lazarus
  MG: added file path to files in TOpenDialog

  Revision 1.52  2001/12/12 08:29:21  lazarus

        Add code to allow TOpenDialog to do multiple line selects.   MAH

  Revision 1.51  2001/12/07 20:12:15  lazarus
  Added a watch dialog.
  Shane

  Revision 1.50  2001/12/05 18:23:47  lazarus
  Added events to Calendar
  Shane

  Revision 1.49  2001/11/30 16:41:59  lazarus
  Improved hints with overlapping windows.
  Shane

  Revision 1.48  2001/11/29 18:41:27  lazarus
  Improved the double click.
  Shane

  Revision 1.47  2001/11/21 19:32:32  lazarus
  TComboBox can now be moved in FormEditor
  Shane

  Revision 1.46  2001/11/21 14:55:31  lazarus
  Changes for combobox to receive butondown and up events
  DblClick events now working.
  Shane

  Revision 1.45  2001/11/20 17:20:45  lazarus
  Fixed designer problem with moving controls.
  Can no longer drag controls off the form.
  Shane

  Revision 1.44  2001/11/14 17:46:58  lazarus
  Changes to make toggling between form and unit work.
  Added BringWindowToTop
  Shane

  Revision 1.43  2001/11/12 19:32:23  lazarus
  MG: fixed empty clipboard stream crashing bug

  Revision 1.42  2001/11/12 16:56:08  lazarus
  MG: CLIPBOARD

  Revision 1.41  2001/11/09 19:14:24  lazarus
  HintWindow changes
  Shane

  Revision 1.40  2001/11/01 21:30:35  lazarus
  Changes to Messagebox.
  Added line to CodeTools to prevent duplicate USES entries.

  Revision 1.39  2001/10/31 16:29:22  lazarus
  Fixed the gtk mousemove bug where the control gets the coord's based on it's parent instead of itself.
  Shane

  Revision 1.38  2001/10/16 14:19:13  lazarus
  MG: added nvidia opengl support and a new opengl example from satan

  Revision 1.35  2001/10/09 09:46:58  lazarus
  MG: added codetools, fixed synedit unindent, fixed MCatureHandle

  Revision 1.34  2001/10/03 21:03:02  lazarus
  MG: reduced repaints

  Revision 1.32  2001/06/16 09:14:38  lazarus
  MG: added lazqueue and used it for the messagequeue

  Revision 1.31  2001/06/14 14:57:59  lazarus
  MG: small bugfixes and less notes

  Revision 1.30  2001/04/06 22:25:14  lazarus
  * TTimer uses winapi-interface now instead of sendmessage-interface, stoppok

  Revision 1.29  2001/03/27 11:11:13  lazarus
  MG: fixed mouse msg, added filedialog initialdir

  Revision 1.28  2001/03/26 14:58:31  lazarus
  MG: setwindowpos + bugfixes

  Revision 1.26  2001/03/19 14:44:22  lazarus
  MG: fixed many unreleased DC and GDIObj bugs

  Revision 1.23  2001/02/28 13:17:34  lazarus
  Added some debug code for the top,left reporting problem.
  Shane

  Revision 1.22  2001/02/20 16:53:27  lazarus
  Changes for wordcompletion and many other things from Mattias.
  Shane

  Revision 1.21  2001/01/31 21:16:45  lazarus
  Changed to TCOmboBox focusing.
  Shane

  Revision 1.20  2001/01/30 18:15:02  lazarus
  Added code for TStatusBar
  I'm now capturing WMPainT and doing the drawing myself.
  Shane

  Revision 1.19  2001/01/28 03:51:42  lazarus
  Fixed the problem with Changed for ComboBoxs
  Shane

  Revision 1.18  2001/01/24 23:26:40  lazarus
  MWE:
    = moved some types to gtkdef
    + added WinWidgetInfo
    + added some initialization to Application.Create

  Revision 1.17  2001/01/24 03:21:03  lazarus
  Removed gtkDrawDefualt signal function from gtkcallback.inc
  It was no longer used.
  Shane

  Revision 1.16  2001/01/23 23:33:55  lazarus
  MWE:
    - Removed old LM_InvalidateRect
    - did some cleanup in old  code
    + added some comments  on gtkobject data (gtkproc)

  Revision 1.15  2001/01/12 18:10:54  lazarus
  Changes for keyevents in the editor.
  Shane

  Revision 1.14  2001/01/11 20:16:47  lazarus
  Added some TImageList code.
  Added a bookmark resource with 10 resource images.
  Removed some of the IFDEF's in mwCustomEdit around the inherited code.
  Shane

  Revision 1.13  2001/01/10 23:53:30  lazarus
  MWE:
    ~ minor change

  Revision 1.12  2001/01/10 20:12:29  lazarus
  Added the Nudge feature to the IDE.
  Shane

  Revision 1.11  2001/01/09 18:23:21  lazarus
  Worked on moving controls.  It's just not working with the X and Y coord's I'm getting.
  Shane

  Revision 1.10  2001/01/04 15:09:05  lazarus
  Tested TCustomEdit.Readonly, MaxLength and CharCase.
  Shane

  Revision 1.9  2000/12/19 18:43:13  lazarus
  Removed IDEEDITOR.  This causes the PROJECT class to not function.
  Saving projects no longer works.

  I added TSourceNotebook and TSourceEditor.  They do all the work for saving/closing/opening units.  Somethings work but they are in early development.
  Shane

  Revision 1.8  2000/11/29 21:22:35  lazarus
  New Object Inspector code
  Shane

  Revision 1.7  2000/10/09 22:50:32  lazarus
  MWE:
    * fixed some selection code
    + Added selection sample

  Revision 1.6  2000/09/10 23:08:31  lazarus
  MWE:
    + Added CreateCompatibeleBitamp function
    + Updated TWinControl.WMPaint
    + Added some checks to avoid gtk/gdk errors
    - Removed no fixed warning from GetDC
    - Removed some output

  Revision 1.5  2000/08/28 14:23:49  lazarus
  Added a few files for the start of creating classes for the editor.  [SHANE]

  Revision 1.4  2000/08/11 14:59:09  lazarus
  Adding all the Synedit files.
  Changed the GDK_KEY_PRESS and GDK_KEY_RELEASE stuff to fix the problem in the editor with the shift key being ignored.
  Shane

  Revision 1.3  2000/08/10 10:55:45  lazarus
  Changed TCustomDialog to TCommonDialog
  Shane

  Revision 1.2  2000/07/30 21:48:33  lazarus
  MWE:
    = Moved ObjectToGTKObject to GTKProc unit
    * Fixed array checking in LoadPixmap
    = Moved LM_SETENABLED to API func EnableWindow and EnableMenuItem
    ~ Some cleanup

  Revision 1.1  2000/07/13 10:28:29  michael
  + Initial import

}
