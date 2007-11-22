{%MainUnit gtkproc.pp}
{
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

{$IFDEF RELEASE}

{$DEFINE ASSERT_IS_ON}
{$ELSE}
{.$DEFINE EventTrace}

{$ENDIF}

function DeliverPaintMessage(const Target: Pointer; var TheMessage): GBoolean;
var
  PaintMsg: TLMPaint;
begin
  if TLMessage(TheMessage).Msg=LM_GtkPAINT then
    PaintMsg:= GtkPaintMessageToPaintMessage(TLMGtkPaint(TheMessage),true)
  else
    PaintMsg:=TLMPaint(TheMessage);
  Result := DeliverMessage(Target,PaintMsg) = 0;
  FinalizePaintMessage(PLMessage(@PaintMsg));
end;

{-------------------------------------------------------------------------------
  function DeliverPostMessage(const Target: Pointer; var TheMessage): GBoolean;

  'TheMessage' is in TLMessage format. Don't confuse this with tagMsg.
--------------------------------------------------------------------------------}
function DeliverPostMessage(const Target: Pointer; var TheMessage): GBoolean;
begin
  if TObject(Target) is TWinControl then
  begin
    // convert TLMessage into a tagMsg and push on the message queue
    Result := PostMessage(TWinControl(Target).Handle,
                          TLMessage(TheMessage).Msg,
                          TLMessage(TheMessage).WParam,
                          TLMessage(TheMessage).LParam
                          );
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

function DeliverGtkPaintMessage(Target: Pointer; Widget: PGtkWidget;
  Area: PGDKRectangle; RepaintAll, IsAfterGtk: boolean): GBoolean;
{$IFDEF Gtk2}
// the gtk2 has a working double buffering and expose event area
{$DEFINE DirectPaintMsg}
{$ENDIF}
var
  MSG: TLMGtkPaint;
  {$IFDEF DirectPaintMsg}
  PaintMsg: TLMPaint;
  {$ENDIF}
begin
  //DebugLn(['DeliverGtkPaintMessage ',DbgSName(TObject(Target)),' Widget=',GetWidgetDebugReport(Widget),' RepaintAll=',RepaintAll,' AfterGtk=',IsAfterGtk,' Area=',dbgs(Area)]);
  {$IFDEF Gtk2}
  // default is, that a control receives the paint message after gtk (including the child paints)
  // In case of TCustomControl, there is no gtk painting only the
  // child paintings. Let the TCustomControl paint the background.
  // ToDo: Eventually there must be a 'before paint message'.
  if IsAfterGtk then begin
    if TObject(Target) is TCustomControl then exit;
  end else begin
    if not (TObject(Target) is TCustomControl) then exit;
  end;
  {$ENDIF}
  
  if (not RepaintAll) and ((Area^.Width<1) or (Area^.Height<1)) then exit;

  MSG.Msg := LM_GtkPAINT;
  MSG.Data:=TLMGtkPaintData.Create;
  MSG.Data.Widget := Widget;
  MSG.Data.State := GtkPaint_LCLWidget;
  Msg.Data.Rect:=Bounds(Area^.x,Area^.y,Area^.Width,Area^.Height);
  Msg.Data.RepaintAll:=RepaintAll;

  {$IFDEF DirectPaintMsg}
  PaintMsg:= GtkPaintMessageToPaintMessage(Msg,true);
  Result := DeliverMessage(Target,PaintMsg) = 0;
  FinalizePaintMessage(PLMessage(@PaintMsg));
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
var APage: TCustomPage;
begin
  Result:=true; // handled = true
  if Widget=nil then ;
  if ComponentIsDestroyingHandle(TWinControl(Data)) then exit;
  APage:=TCustomPage(Data);
  TCustomNoteBook(APage.Parent).DoCloseTabClicked(APage);
end;

{-------------------------------------------------------------------------------
  function GTKRealizeCB
  Params: Widget: PGtkWidget; Data: Pointer
  Result: GBoolean

  GTKRealizeCB is called by the gtk, whenever a widget is realized (ie mapped),
  but before the widget itself gets the realize signal.
  That means that the gdk window on the xserver has been created.
-------------------------------------------------------------------------------}
function gtkRealizeCB(Widget: PGtkWidget; Data: Pointer): GBoolean; cdecl;
var
  decor,Func : Longint;
  TheWindow: PGdkWindow;
  TheForm: TCustomForm;
begin
  Result := CallBackDefaultReturn;

  {$IFDEF EventTrace}
  EventTrace('realize', nil);
  {$ENDIF}

  if (Data<>nil) then begin
    if TObject(Data) is TCustomForm then begin
      TheForm:=TCustomForm(Data);
      if TheForm.Parent=nil then begin
        TheWindow:=gtk_widget_get_toplevel(Widget)^.window;

        //apart from none and sizeable, this will
        //only work if WM supports motif flags
        //properly, which very few actually do.

        Decor := GetWindowDecorations(TheForm);
        Func := GetWindowFunction(TheForm);

        gdk_window_set_decorations(TheWindow, decor);
        gdk_window_set_functions(TheWindow, func);
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
  initialized the gdkwindow. This function is used for the second part of
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
  Result := CallBackDefaultReturn;

  if Data=nil then ;
  {$IFDEF EventTrace}
  EventTrace('realizeafter', nil);
  {$ENDIF}

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
      //if TheWinControl<>nil then DbgOut(' ',TheWinControl.Name,':',TheWinControl.ClassName,' ',DbgS(TheWinControl.Handle));
      //DebugLn(' Widget=',DbgS(Widget),' Fixed=',DbgS(GetFixedWidget(Widget)),' Main=',DbgS(GetMainWidget(Widget)));
      if (TheWinControl<>nil) then begin
        {$IFDEF DebugGDK}BeginGDKErrorTrap;{$ENDIF}

        NewEventMask:=gdk_window_get_events(GetControlWindow(Widget))
                      or WinWidgetInfo^.EventMask;
        {$IFNDEF Gtk1}
        gtk_widget_add_events(Widget,NewEventMask);
        {$ENDIF}
        gdk_window_set_events(GetControlWindow(Widget),NewEventMask);

        if (ClientWidget<>nil) and (GetControlWindow(ClientWidget)<>nil)
        and (ClientWidget^.Window<>Widget^.Window) then begin
          NewEventMask:=gdk_window_get_events(GetControlWindow(ClientWidget))
                        or WinWidgetInfo^.EventMask;
          {$IFNDEF Gtk1}
          gtk_widget_add_events(ClientWidget,WinWidgetInfo^.EventMask);
          {$ENDIF}
          gdk_window_set_events(GetControlWindow(ClientWidget),NewEventMask);
        end;
        //DebugLn('BBB1 ',DbgS(NewEventMask),8),' ',DbgS(Cardinal(gdk_window_get_events(Widget^.Window)));
        {$IFDEF DebugGDK}EndGDKErrorTrap;{$ENDIF}
      end;

      if TheWinControl<>nil then 
      begin
        TheWinControl.CNPreferredSizeChanged;
        TGtkPrivateWidgetClass(TheWinControl.WidgetSetClass.WSPrivate).UpdateCursor(WinWidgetInfo);
        ConnectInternalWidgetsSignals(MainWidget,TheWinControl);

        if TheWinControl is TCustomPage then
          UpdateNotebookPageTab(nil,TheWinControl);
 
        if TheWinControl is TCustomForm then
          SetFormShowInTaskbar(TCustomForm(TheWinControl),
                               TCustomForm(TheWinControl).ShowInTaskbar);
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

  {$IFDEF EventTrace}
  EventTrace('show', data);
  {$ENDIF}
  if Widget=nil then ;
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

  {$IFDEF EventTrace}
  EventTrace('hide', data);
  {$ENDIF}
  if Widget=nil then ;
  FillChar(Mess,SizeOf(Mess),0);
  Mess.Msg := LM_SHOWWINDOW;
  Mess.Show := False;
  Result := DeliverMessage(Data, Mess) = 0;
end;

function gtkactivateCB(widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess: TLMActivate;
begin
  Result:= True;
  {$IFDEF EventTrace}
  EventTrace('activate', data);
  {$ENDIF}
  
  ResetDefaultIMContext;
  
  if LockOnChange(PgtkObject(Widget),0) > 0 then Exit;

  FillChar(Mess,SizeOf(Mess),#0);
  Mess.Msg := LM_ACTIVATE;
  Mess.Active:=true;
  Mess.Minimized:=false;
  Mess.ActiveWindow:=0;
  if GtkWidgetIsA(Widget, gtk_window_get_type) then
    Mess.ActiveWindow:=HWnd(PtrUInt(PGTKWindow(Widget)^.focus_widget));
  Mess.Result := 0;
  //DebugLn('gtkactivateCB ',DbgSName(TObject(Data)));
  DeliverMessage(Data, Mess);

  Result := CallBackDefaultReturn;
end;

function GTKCheckMenuToggeledCB(AMenuItem: PGTKCheckMenuItem; AData: gPointer): GBoolean; cdecl;
// AData --> LCLMenuItem
var
  LCLMenuItem: TMenuItem;
begin
  Result := CallBackDefaultReturn;
  {$IFDEF EventTrace}
  EventTrace('toggled', AData);
  {$ENDIF}

  LCLMenuItem := TMenuItem(AData);
  // some sanity checks
  if LCLMenuItem = nil then Exit;
  if not LCLMenuItem.IsCheckItem then Exit; // ???

  // the gtk always toggles the check flag
  // -> restore 'checked' flag if needed
  if gtk_check_menu_item_get_active(AMenuItem) = LCLMenuItem.Checked then Exit;
  if LCLMenuItem.AutoCheck then Exit;

  // restore it
  LockOnChange(PgtkObject(AMenuItem), +1);
  gtk_check_menu_item_set_active(AMenuItem, LCLMenuItem.Checked);
  LockOnChange(PgtkObject(AMenuItem), -1);
end;

function gtkchangedCB( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
  AComboBox: TComboBox;
  GtkComboWidget: PGtkCombo;
begin
  Result := CallBackDefaultReturn;

  if ComponentIsDestroyingHandle(TWinControl(Data))
  or (LockOnChange(PgtkObject(Widget),0)>0) then exit;
  
  if (TObject(Data) is TComboBox) then begin
    AComboBox:=TComboBox(Data);
    GtkComboWidget:=PGtkCombo(AComboBox.Handle);
    if (GtkComboWidget^.popwin<>nil)
    and (GTK_WIDGET_VISIBLE(GtkComboWidget^.popwin)) then begin
      // Ignoring changed events during popup
      //DebugLn(['gtkchangedCB Ignoring changed events during popup']);
      exit;
    end;
  end;

  {$IFDEF EventTrace}
  EventTrace('changed', data);
  {$ENDIF}
  Mess.Msg := LM_CHANGED;
  DeliverMessage(Data, Mess);
end;

function gtkchanged_editbox( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  Result := CallBackDefaultReturn;

  if LockOnChange(PgtkObject(Widget),0)>0 then exit;
  {$IFDEF EventTrace}
  EventTrace('changed_editbox', data);
  {$ENDIF}

  Mess.Msg := CM_TEXTCHANGED;
  DeliverMessage(Data, Mess);

  Result := CallBackDefaultReturn;
end;

function gtkdaychanged(Widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  MSG: TLMessage;
begin
  Result := CallBackDefaultReturn;

  if LockOnChange(PgtkObject(Widget),0)>0 then exit;
  EventTrace('day changed', data);
  MSG.Msg := LM_DAYCHANGED;
  DeliverPostMessage(Data, MSG);

  Result := CallBackDefaultReturn;
end;

function gtktoggledCB( widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  //DebugLn('gtktoggledCB ',DbgSName(TObject(Data)));
  Result := CallBackDefaultReturn;
  {$IFDEF EventTrace}
  EventTrace('toggled', data);
  {$ENDIF}
  if LockOnChange(PgtkObject(Widget),0) > 0 then Exit;

  if GtkWidgetIsA(Widget,gtk_toggle_button_get_type) then begin
    gtk_object_set_data(PgtkObject(Widget), 'Grayed', nil);
  end;

  Mess.Msg := LM_CHANGED;
  Mess.Result := 0;
  DeliverMessage(Data, Mess);
  //DebugLn('gtktoggledCB END ',DbgSName(TObject(Data)));
end;

{$Ifdef GTK1}
function gtkDrawCB(Widget: PGtkWidget; area: PGDKRectangle;
  data: gPointer): GBoolean; cdecl;
begin
  Result := CallBackDefaultReturn;
  //DebugLn(['gtkDrawCB ',GetWidgetDebugReport(Widget),' area=',dbgs(area)]);
  {$IFDEF EventTrace}
  EventTrace('DrawAfter', data);
  {$ENDIF}
end;

function gtkDrawAfterCB(Widget: PGtkWidget; area: PGDKRectangle;
  data: gPointer) : GBoolean; cdecl;
  
  function WidgetHasDrawAllBug: boolean;
  var
    WidgetStyle: PGtkStyle;
  begin
    Result:=false;
    WidgetStyle := gtk_widget_get_style(Widget);
    if (WidgetStyle=nil) then exit;
    if (WidgetStyle^.engine=nil) then exit;
    Result:=true;
    //DebugLn(['WidgetHasDrawAllBug ',dbgs(WidgetStyle^.engine)]);
  end;
  
var
  DesignOnlySignal: boolean;
begin
  Result := CallBackDefaultReturn;
  //DebugLn(['gtkDrawAfterCB ',GetWidgetDebugReport(Widget),' area=',dbgs(area)]);
  {$IFDEF EventTrace}
  EventTrace('DrawAfter', data);
  {$ENDIF}

  if not (csDesigning in TComponent(Data).ComponentState) then begin
    DesignOnlySignal:=GetDesignOnlySignalFlag(Widget,dstDrawAfter);
    if DesignOnlySignal then exit;
  end else begin
    {$IFDEF VerboseDesignerDraw}
    DebugLn('gtkDrawAfterCB',
      ' Widget=',DbgS(Widget),'=',GetWidgetClassName(Widget),
      ' ',TComponent(Data).Name,
      ' ',area^.x,',',area^.y,',',area^.width,',',area^.height,
      '');
    {$ENDIF}
  end;
  
  // some gtk1 theme engines show a bug: they ignore the area and repaint all
  // In this case the LCL must repaint all too.
  DeliverGtkPaintMessage(Data,Widget,Area,WidgetHasDrawAllBug,true);
end;
{$ENDIF}

function gtkExposeEvent(Widget: PGtkWidget; Event : PGDKEventExpose;
  Data: gPointer): GBoolean; cdecl;
var
  DesignOnlySignal: boolean;
begin
  Result := CallBackDefaultReturn;
  {$IFDEF EventTrace}
  EventTrace('ExposeAfter', data);
  {$ENDIF}
  //DebugLn(['gtkExposeEvent ',GetWidgetDebugReport(Widget),' Event^.Count=',Event^.Count]);
  if (Event^.Count > 0) then exit;

  if not (csDesigning in TComponent(Data).ComponentState) then begin
    DesignOnlySignal:=GetDesignOnlySignalFlag(Widget,dstExposeAfter);
    if DesignOnlySignal then exit;
  end else begin
    {$IFDEF VerboseDesignerDraw}
    DebugLn('gtkExpose',
      ' Widget=',DbgS(Widget),'=',GetWidgetClassName(Widget),
      ' ',TComponent(Data).Name,
      ' ',dbgs(Event^.area.x),',',dbgs(Event^.area.y),',',dbgs(Event^.area.width),',',dbgs(Event^.area.height),
      '');
    {$ENDIF}
  end;

  //DebugLn(['gtkExposeEvent ',GetWidgetDebugReport(Widget),' ',dbgGRect(@Event^.Area)]);

  // the expose area is ok, but some gtk widgets repaint everything on expose
  // -> maximize the area
  DeliverGtkPaintMessage(Data,Widget,@Event^.Area,true,false);
end;

function gtkExposeEventAfter(Widget: PGtkWidget; Event : PGDKEventExpose;
  Data: gPointer): GBoolean; cdecl;
var
  DesignOnlySignal: boolean;
  {$IFDEF GTK2}
  //children: PGList;
  {$ENDIF}
begin
  Result := CallBackDefaultReturn;
  {$IFDEF EventTrace}
  EventTrace('ExposeAfter', data);
  {$ENDIF}
  //DebugLn(['gtkExposeEventAfter ',GetWidgetDebugReport(Widget),' Event^.Count=',Event^.Count]);
  if (Event^.Count > 0) then exit;

  if not (csDesigning in TComponent(Data).ComponentState) then begin
    DesignOnlySignal:=GetDesignOnlySignalFlag(Widget,dstExposeAfter);
    if DesignOnlySignal then exit;
  end else begin
    {$IFDEF VerboseDesignerDraw}
    DebugLn('gtkExposeAfter',
      ' Widget=',DbgS(Widget),'=',GetWidgetClassName(Widget),
      ' ',TComponent(Data).Name,
      ' ',dbgs(Event^.area.x),',',dbgs(Event^.area.y),',',dbgs(Event^.area.width),',',dbgs(Event^.area.height),
      '');
    {$ENDIF}
  end;

  //DebugLn(['gtkExposeEventAfter ',GetWidgetDebugReport(Widget),' ',dbgGRect(@Event^.Area)]);

  // the expose area is ok, but some gtk widgets repaints everything on expose
  // -> maximize the area
  DeliverGtkPaintMessage(Data,Widget,@Event^.Area,true,true);
  {$IFDEF GTK2}
  // Some widgets in gtk2 don't have their own exclusive "windows" so a synthetic event must be sent
  // MG: That is already done by the gtk2. For which widgets does this not work?
  //     Enabling this results in double painting, which is slower and
  //     wrong for anitaliased text.
  {if GTK_IS_FIXED(Widget) then begin
    children := gtk_container_get_children(PGtkContainer(Widget));
    while children <> nil do begin
      if (children^.data <> nil) then begin
        if GTK_WIDGET_NO_WINDOW(PGtkWidget(children^.data)) then
          gtk_container_propagate_expose(PGtkContainer(Widget), PGtkWidget(children^.data), Event);
        if children^.next = nil then break;
        children := children^.next;
      end;
    end;
    g_list_free(children);
  end;}
  {$ENDIF}
end;

function gtkfrmactivateAfter(widget: PGtkWidget; Event : PgdkEventFocus;
  data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMActivate;
{$IFDEF VerboseFocus}
  LCLObject: TObject;
  CurFocusWidget: PGtkWidget;
{$ENDIF}
begin
  {$IFDEF EventTrace}
  EventTrace('activate after', data);
  {$ENDIF}
  if (Widget=nil) or (Event=nil) then ;

  ResetDefaultIMContext;
  UpdateMouseCaptureControl;

  FillChar(Mess,SizeOf(Mess),#0);
  {$IFDEF VerboseFocus}
  write('gtkfrmactivateAfter Widget=',DbgS(Widget),' Event^.theIn=',Event^.theIn);
  LCLObject:=TObject(data);
  if LCLObject<>nil then begin
    if LCLObject is TComponent then begin
      write(' LCLObject=',TComponent(LCLObject).Name,':',LCLObject.ClassName)
    end else begin
      write(' LCLObject=',LCLObject.ClassName)
    end;
  end else
    write(' LCLObject=nil');
  DebugLn(''); DbgOut('    ');
  CurFocusWidget:=PGtkWidget(GetFocus);
  if CurFocusWidget<>nil then begin
    write(' GetFocus=',DbgS(CurFocusWidget));
    LCLObject:=GetNearestLCLObject(CurFocusWidget);
    if LCLObject<>nil then begin
      if LCLObject is TComponent then begin
        DbgOut(' ParentLCLFocus=',TComponent(LCLObject).Name,':',LCLObject.ClassName)
      end else begin
        DbgOut(' ParentLCLFocus=',LCLObject.ClassName)
      end;
    end else
      DbgOut(' LCLObject=nil');
  end else begin
    DbgOut(' GetFocus=nil');
  end;
  DebugLn('');
  {$ENDIF}

  Mess.Msg := LM_ACTIVATE;
  Mess.Active:=true;
  Mess.Minimized:=false;
  Mess.ActiveWindow:=0;
  if GtkWidgetIsA(Widget, gtk_window_get_type) then
    Mess.ActiveWindow:=HWnd(PtrUInt(PGTKWindow(Widget)^.focus_widget));
  Mess.Result := 0;
  DeliverMessage(Data, Mess); // send message directly (not Post)

  Result := CallBackDefaultReturn;
end;

function gtkfrmdeactivateAfter( widget: PGtkWidget; Event : PgdkEventFocus;
  data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMActivate;
{$IFDEF VerboseFocus}
  LCLObject: TControl;
{$ENDIF}
begin
  {$IFDEF EventTrace}
  EventTrace('deactivate after', data);
  {$ENDIF}
  if (Widget=nil) or (Event=nil) then ;
  {$IFDEF VerboseFocus}
  write('gtkfrmdeactivate Widget=',DbgS(Widget),' ',Event^.theIn,
        ' GetFocus=',DbgS(Widget));
  LCLObject:=TControl(GetLCLObject(Widget));
  if LCLObject<>nil then
    DebugLn(' LCLObject=',LCLObject.Name,':',LCLObject.ClassName)
  else
    DebugLn(' LCLObject=nil');
  {$ENDIF}
  ResetDefaultIMContext;
  UpdateMouseCaptureControl;

  Mess.Msg := LM_DEACTIVATE;
  DeliverMessage(Data, Mess);

  Result := CallBackDefaultReturn;
end;

function GTKMap(Widget: PGTKWidget; Data: gPointer): GBoolean; cdecl;
begin
  Result := CallBackDefaultReturn;
  if (Widget=nil) then ;
  EventTrace('map', data);
end;

function GTKKeyPress(Widget: PGtkWidget; Event: PGdkEventKey; Data: gPointer) : GBoolean; cdecl;
begin
  Result := HandleGtkKeyUpDown(Widget,Event,Data,true, True);
end;

function GTKKeyPressAfter(Widget: PGtkWidget; Event: pgdkeventkey; Data: gPointer): GBoolean; cdecl;
begin
  Result := HandleGtkKeyUpDown(Widget,Event,Data,false, True);
end;

function GTKKeyRelease(Widget: PGtkWidget; Event: PGdkEventKey; Data: gPointer) : GBoolean; cdecl;
begin
  Result := HandleGtkKeyUpDown(Widget,Event,Data,true, False);
end;

function GTKKeyReleaseAfter(Widget: PGtkWidget; Event: pgdkeventkey; Data: gPointer): GBoolean; cdecl;
begin
  Result := HandleGtkKeyUpDown(Widget,Event,Data,false, False);
end;

function GTKFocusCB( widget: PGtkWidget; event:PGdkEventFocus;
  data: gPointer) : GBoolean; cdecl;
{$IFDEF VerboseFocus}
var
  LCLObject: TObject;
  CurFocusWidget: PGtkWidget;
{$ENDIF}
begin
  {$IFDEF EventTrace}
  EventTrace('focus', data);
  {$ENDIF}
  if (Widget=nil) or (Event=nil) then ;
  //DebugLn('GTKFocusCB ',DbgSName(TObject(Data)),' ',GetWidgetDebugReport(Widget));
  {$IFDEF VerboseFocus}
  write('GTKFocusCB Widget=',DbgS(Widget),' Event^.theIn=',Event^.theIn);
  LCLObject:=TObject(data);
  if LCLObject<>nil then begin
    if LCLObject is TComponent then begin
      write(' LCLObject=',TComponent(LCLObject).Name,':',LCLObject.ClassName)
    end else begin
      write(' LCLObject=',LCLObject.ClassName)
    end;
  end else
    write(' LCLObject=nil');
  DebugLn(''); DbgOut('    ');
  CurFocusWidget:=PGtkWidget(GetFocus);
  if CurFocusWidget<>nil then begin
    write(' GetFocus=',DbgS(CurFocusWidget));
    LCLObject:=GetNearestLCLObject(CurFocusWidget);
    if LCLObject<>nil then begin
      if LCLObject is TComponent then begin
        write(' ParentLCLFocus=',TComponent(LCLObject).Name,':',LCLObject.ClassName)
      end else begin
        write(' ParentLCLFocus=',LCLObject.ClassName)
      end;
    end else
      write(' LCLObject=nil');
  end else begin
    write(' GetFocus=nil');
  end;
  DebugLn('');
  {$ENDIF}

  Result:=CallBackDefaultReturn;
end;

function GTKFocusCBAfter(widget: PGtkWidget; event:PGdkEventFocus;
  data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
{$IFDEF VerboseFocus}
  LCLObject: TObject;
  CurFocusWidget: PGtkWidget;
{$ENDIF}
begin
  {$IFDEF EventTrace}
  EventTrace('focus', data);
  {$ENDIF}
  if (Widget=nil) or (Event=nil) then ;
  //DebugLn('GTKFocusCBAfter ',DbgSName(TObject(Data)),' ',GetWidgetDebugReport(Widget));
  {$IFDEF VerboseFocus}
  write('GTKFocusCBAfter Widget=',DbgS(Widget),' Event^.theIn=',Event^.theIn);
  LCLObject:=TObject(data);
  if LCLObject<>nil then begin
    if LCLObject is TComponent then begin
      write(' LCLObject=',TComponent(LCLObject).Name,':',LCLObject.ClassName)
    end else begin
      write(' LCLObject=',LCLObject.ClassName)
    end;
  end else
    write(' LCLObject=nil');
  DebugLn(''); DbgOut('    ');
  CurFocusWidget:=PGtkWidget(GetFocus);
  if CurFocusWidget<>nil then begin
    write(' GetFocus=',DbgS(CurFocusWidget));
    LCLObject:=GetNearestLCLObject(CurFocusWidget);
    if LCLObject<>nil then begin
      if LCLObject is TComponent then begin
        write(' ParentLCLFocus=',TComponent(LCLObject).Name,':',LCLObject.ClassName)
      end else begin
        write(' ParentLCLFocus=',LCLObject.ClassName)
      end;
    end else
      write(' LCLObject=nil');
  end else begin
    write(' GetFocus=nil');
  end;
  DebugLn('');
  {$ENDIF}

  ResetDefaultIMContext;
  UpdateMouseCaptureControl;

  //TODO: fill in old focus
  FillChar(Mess,SizeOf(Mess),0);
  Mess.msg := LM_SETFOCUS;
  DeliverMessage(Data,  Mess);
  Result:=true;
end;

function GTKKillFocusCB(widget: PGtkWidget; event:PGdkEventFocus;
  data: gPointer) : GBoolean; cdecl;
{$IFDEF VerboseFocus}
var
  LCLObject: TObject;
  CurFocusWidget: PGtkWidget;
{$ENDIF}
begin
  {$IFDEF EventTrace}
  EventTrace('killfocusCB', data);
  {$ENDIF}
  if (Widget=nil) or (Event=nil) then ;
  //DebugLn('GTKKillFocusCB ',DbgSName(TObject(Data)),' ',GetWidgetDebugReport(Widget));
  {$IFDEF VerboseFocus}
  write('GTKillFocusCB Widget=',DbgS(Widget),' Event^.theIn=',Event^.theIn);
  LCLObject:=TObject(data);
  if LCLObject<>nil then begin
    if LCLObject is TComponent then begin
      write(' LCLObject=',TComponent(LCLObject).Name,':',LCLObject.ClassName)
    end else begin
      write(' LCLObject=',LCLObject.ClassName)
    end;
  end else
    write(' LCLObject=nil');
  DebugLn(''); DbgOut('    ');
  CurFocusWidget:=PGtkWidget(GetFocus);
  if CurFocusWidget<>nil then begin
    write(' GetFocus=',DbgS(CurFocusWidget));
    LCLObject:=GetNearestLCLObject(CurFocusWidget);
    if LCLObject<>nil then begin
      if LCLObject is TComponent then begin
        write(' ParentLCLFocus=',TComponent(LCLObject).Name,':',LCLObject.ClassName)
      end else begin
        write(' ParentLCLFocus=',LCLObject.ClassName)
      end;
    end else
      write(' LCLObject=nil');
  end else begin
    write(' GetFocus=nil');
  end;
  DebugLn('');
  {$ENDIF}

  // do not release capture widget here, as this will interfere
  //ReleaseCaptureWidget(Widget);

  Result:=CallBackDefaultReturn;
end;

function GTKKillFocusCBAfter(widget: PGtkWidget; event:PGdkEventFocus;
  data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
{$IFDEF VerboseFocus}
  LCLObject: TObject;
  CurFocusWidget: PGtkWidget;
{$ENDIF}
begin
  if (Widget=nil) or (Event=nil) then ;
  {$IFDEF EventTrace}
  EventTrace('killfocusCBAfter', data);
  {$ENDIF}
  //DebugLn('GTKKillFocusCBAfter ',DbgSName(TObject(Data)),' ',GetWidgetDebugReport(Widget));
  {$IFDEF VerboseFocus}
  write('GTKillFocusCBAfter Widget=',DbgS(Widget),' Event^.theIn=',Event^.theIn);
  LCLObject:=TObject(data);
  if LCLObject<>nil then begin
    if LCLObject is TComponent then begin
      write(' LCLObject=',TComponent(LCLObject).Name,':',LCLObject.ClassName)
    end else begin
      write(' LCLObject=',LCLObject.ClassName)
    end;
  end else
    write(' LCLObject=nil');
  DebugLn(''); DbgOut('    ');
  CurFocusWidget:=PGtkWidget(GetFocus);
  if CurFocusWidget<>nil then begin
    write(' GetFocus=',DbgS(CurFocusWidget));
    LCLObject:=GetNearestLCLObject(CurFocusWidget);
    if LCLObject<>nil then begin
      if LCLObject is TComponent then begin
        write(' ParentLCLFocus=',TComponent(LCLObject).Name,':',LCLObject.ClassName)
      end else begin
        write(' ParentLCLFocus=',LCLObject.ClassName)
      end;
    end else
      write(' LCLObject=nil');
  end else begin
    write(' GetFocus=nil');
  end;
  DebugLn('');
  {$ENDIF}

  ResetDefaultIMContext;
  UpdateMouseCaptureControl;

  FillChar(Mess,SizeOf(Mess),0);
  Mess.msg := LM_KILLFOCUS;
  // do not release the capture widget here

  //TODO: fill in new focus
  Assert(False, Format('Trace:TODO: [gtkkillfocusCB] %s  finish', [TObject(Data).ClassName]));
  
  DeliverMessage(Data,  Mess);

  Result:=true;
end;

function gtkdestroyCB(widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess: TLMessage;
  Info: PWidgetInfo;
begin
  Result := CallBackDefaultReturn;
  //DebugLn(['gtkdestroyCB ',GetWidgetDebugReport(Widget)]);
  
  Info:=GetWidgetInfo(Widget,false);
  if Info=nil then begin
    // this widget is already destroyed
    exit;
  end;
  if (Info^.LCLObject<>TObject(Data)) then begin
    // this LCLObject does not use this widget anymore
    exit;
  end;
  if (TObject(Data) is TWinControl)
  and ((not TWinControl(Data).HandleAllocated)
       or (PGtkWidget(TWinControl(Data).Handle)<>Widget))
  then begin
    // the TWinControl does not use this widget anymore.
    exit;
  end;

  {$IFDEF EventTrace}
  EventTrace('destroyCB', data);
  {$ENDIF}
  //DebugLn('gtkdestroyCB Data="',DbgSName(TObject(Data)),'" LCLObject="',DbgSName(Info^.LCLObject),'" ',GetWidgetDebugReport(Widget));
  FillChar(Mess,SizeOf(Mess),0);
  Mess.msg := LM_DESTROY;
  DeliverMessage(Data, Mess);

  // NOTE: if the destroy message is posted
  // we should post an info destroy message as well

  FreeWidgetInfo(Widget);
end;

function gtkdeleteCB( widget : PGtkWidget; event : PGdkEvent;
  data : gPointer) : GBoolean; cdecl;
var Mess : TLMessage;
begin
  FillChar(Mess,SizeOf(Mess),0);
  if (Widget=nil) or (Event=nil) then ;
  Mess.Msg:= LM_CLOSEQUERY;
  { Message results : True - do nothing, False - destroy or hide window }
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function gtkresizeCB( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
//var
//   Mess : TLMessage;
begin
  Result := CallBackDefaultReturn;
  if (Widget=nil) then ;
  {$IFDEF EventTrace}
  EventTrace('resize', data);
  {$ENDIF}
//   Mess.msg := LM_RESIZE;
//   TControl(data).WindowProc(TLMessage(Mess));
  Assert(False, 'Trace:TODO: [gtkresizeCB] fix (or remove) to new LM_SIZE');
//TObject(data).Dispatch(Mess);
end;

function gtkMonthChanged(Widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess: TLMessage;
begin
  Result := CallBackDefaultReturn;
  if (Widget=nil) then ;
  {$IFDEF EventTrace}
  EventTrace('month changed', data);
  {$ENDIF}
  FillChar(Mess,SizeOf(Mess),0);
  Mess.Msg := LM_MONTHCHANGED;
  DeliverPostMessage(Data, Mess);

  Result := CallBackDefaultReturn;
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
  if (Widget=nil) then ;
  MappedXY:=TranslateGdkPointToClientArea(Event^.Window,
                               Point(TruncToInt(Event^.X),TruncToInt(Event^.Y)),
                               PGtkWidget(AWinControl.Handle));

  ShiftState := GTKEventStateToShiftState(Event^.State);
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
  if (AControl=nil) then ;
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
  Result := CallBackDefaultReturn;
  MousePositionValid:=false;

  {$IFDEF VerboseMouseBugfix}
  DesignOnlySignal:=GetDesignOnlySignalFlag(Widget,dstMouseMotion);
  DebugLn('[GTKMotionNotify] ',
    DbgSName(TControl(Data)),
    ' Widget=',DbgS(Widget),
    ' DSO=',dbgs(DesignOnlySignal),
    ' Event^.X=',dbgs(TruncToInt(Event^.X)),' Event^.Y=',dbgs(TruncToInt(Event^.Y))
    );
  {$ENDIF}

  UpdateMouseCaptureControl;

  if not (csDesigning in TComponent(Data).ComponentState) then begin
    DesignOnlySignal:=GetDesignOnlySignalFlag(Widget,dstMouseMotion);
    if DesignOnlySignal then exit;
    if not ControlGetsMouseMoveBefore(TControl(Data)) then exit;
  end else begin
    // stop the signal, so that the widget does not auto react
    g_signal_stop_emission_by_name(PGTKObject(Widget),'motion-notify-event');
  end;

  DeliverMouseMoveMessage(Widget,Event,TWinControl(Data));
  {$IFDEF gtk2}
  if TControl(Data).FCompStyle = csWinControl then Result := True;
  {$ENDIF}
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
  Result := CallBackDefaultReturn;
  MousePositionValid:=false;

  {$IFDEF VerboseMouseBugfix}
  DebugLn('[GTKMotionNotifyAfter] ',
    DbgSName(TControl(Data)));
  {$ENDIF}

  // stop the signal, so that it is not sent to the parent widgets
  g_signal_stop_emission_by_name(PGTKObject(Widget),'motion-notify-event');

  UpdateMouseCaptureControl;

  if (csDesigning in TComponent(Data).ComponentState) then exit;
  if ControlGetsMouseMoveBefore(TControl(Data)) then exit;

  DeliverMouseMoveMessage(Widget,Event,TWinControl(Data));
end;

{$IFDEF Gtk2}
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
{$ENDIF}

{-------------------------------------------------------------------------------
  function ControlGetsMouseDownBefore(AControl: TControl): boolean;

  Returns true, if mouse down event should be sent before the widget istelf
  reacts.
-------------------------------------------------------------------------------}
function ControlGetsMouseDownBefore(AControl: TControl;
  AWidget: PGtkWidget): boolean;
begin
  Result:=true;
  if AControl=nil then exit;
  if GtkWidgetIsA(AWidget,gtk_toggle_button_get_type) then begin
    {$IFDEF Gtk1}
    Result:=false;
    {$ENDIF}
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

{off $DEFINE VerboseMouseBugfix}

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
      (not (gdk_event_get_type(Event) in [gdk_2button_press,gdk_3button_press]))
    then begin
      {$IFDEF VerboseMouseBugfix}
      DebugLn(' NO CLICK: LastMouse.Down=',dbgs(LastMouse.Down),
              ' Event^.theType=',dbgs(gdk_event_get_type(Event)));
      {$ENDIF}
      Exit;
    end;

    MessI.Keys := MessI.Keys or BtnKey;

    IsMultiClick:=TestIfMultiClick;

    case gdk_event_get_type(Event) of

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
          DebugLn('  MULTI CLICK: ',dbgs(now),'-',dbgs(LastMouse.TheTime),'<= ',
            dbgs((1/86400)*(DblClickTime/1000)));
          {$ENDIF}
        end else begin
          // normal click
          LastMouse.ClickCount:=1;
        end;
      end;
    end;
    {$IFDEF VerboseMouseBugfix}
    DebugLn('  ClickCount=',dbgs(LastMouse.ClickCount));
    {$ENDIF}

    LastMouse.TheTime := Now;
    LastMouse.Window := Event^.Window;
    LastMouse.WindowPoint := EventXY;
    LastMouse.Down := True;
    LastMouse.Component:=AWinControl;

    //DebugLn('DeliverMouseDownMessage ',DbgSName(AWinControl),' Mapped=',dbgs(MappedXY.X),',',dbgs(MappedXY.Y),' Event=',dbgs(EventXY.X),',',dbgs(EventXY.Y),' ',dbgs(LastMouse.ClickCount));
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
  if (Widget=nil) then ;
  MousePositionValid:=false;

  EventXY:=Point(TruncToInt(Event^.X),TruncToInt(Event^.Y));
  ShiftState := GTKEventStateToShiftState(Event^.State);
  MappedXY:=TranslateGdkPointToClientArea(Event^.Window,EventXY,
                                          PGtkWidget(AWinControl.Handle));
  //DebugLn('DeliverMouseDownMessage ',DbgSName(AWinControl),' Mapped=',dbgs(MappedXY.X),',',dbgs(MappedXY.Y),' Event=',dbgs(EventXY.X),',',dbgs(EventXY.Y));

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
function gtkMouseBtnPress(widget: PGtkWidget; event: pgdkEventButton;
  data: gPointer) : GBoolean; cdecl;
  
  procedure CheckListSelection;
  var
    List: PGlist;
    ListWidget: PGtkList;
    R: TRect;
    Info: PWinWidgetInfo;
  begin
    // When in browse mode and a listbox is focused and nothing is selected,
    // the first item is focused.
    // Clicking with the mouse on this item won't select it.

    Info := GetWidgetInfo(Widget, false);
    if Info = nil then Exit;
    if Info^.CoreWidget = nil then Exit;
    if not GtkWidgetIsA(Info^.CoreWidget, gtk_list_get_type) then Exit;
    ListWidget := PGtkList(Info^.CoreWidget);

    // Check mode
    if selection_mode(ListWidget^) <> GTK_SELECTION_BROWSE then Exit;

    // Check selection
    List := ListWidget^.selection;
    if (List <> nil) and (List^.data <> nil) then Exit;

    // Check if there are children
    List := ListWidget^.children;
    if List = nil then Exit;
    if List^.Data = nil then Exit;

    // we need only to check the first
    with PGtkWidget(List^.Data)^.allocation do
      R := Bounds(X, Y, Width, Height);
    if not PtInRect(R, Point(Trunc(event^.X), Trunc(event^.Y))) then Exit;
    // Select it
    gtk_list_item_select(PGtkListItem(List^.Data));
  end;

var
  DesignOnlySignal: boolean;
  CaptureWidget: PGtkWidget;
  EventXY: TPoint;
  MappedXY: TPoint;
begin
  Result := CallBackDefaultReturn;
  MousePositionValid:=false;

  {$IFDEF VerboseMouseBugfix}
  DebugLn('');
  DesignOnlySignal:=GetDesignOnlySignalFlag(Widget,dstMousePress);
  DebugLn('[gtkMouseBtnPress] ',
    DbgSName(TObject(Data)),
    ' Widget=',DbgS(Widget),
    ' ControlWidget=',DbgS(TWinControl(Data).Handle),
    ' DSO='+dbgs(DesignOnlySignal),
    ' '+dbgs(TruncToInt(Event^.X)),',',dbgs(TruncToInt(Event^.Y)),
    ' Type='+dbgs(gdk_event_get_type(Event)));
  {$ENDIF}
  //DebugLn('DDD1 MousePress Widget=',DbgS(Widget),
  //' ClientWidget=',DbgS(GetFixedWidget(Widget)),
  //' EventMask=',DbgS(gdk_window_get_events(Widget^.Window)),
  //' GDK_BUTTON_RELEASE_MASK=',DbgS(GDK_BUTTON_RELEASE_MASK),
  //' Window=',DbgS(Widget^.Window)
  //);
  //if GetFixedWidget(Widget)<>nil then
  //  DebugLn('DDD2 ClientWindow=',DbgS(PGtkWidget(GetFixedWidget(Widget))^.Window));

  {$IFDEF EventTrace}
  EventTrace('Mouse button Press', data);
  {$ENDIF}

  ResetDefaultIMContext;
  UpdateMouseCaptureControl;

  if not (csDesigning in TComponent(Data).ComponentState) then begin
    // fix gtklist selection first
    CheckListSelection;

    DesignOnlySignal:=GetDesignOnlySignalFlag(Widget,dstMousePress);
    if DesignOnlySignal then exit;
    if not ControlGetsMouseDownBefore(TControl(Data),Widget) then exit;

    CaptureWidget:=PGtkWidget(TWinControl(Data).Handle);
    if Event^.button=1 then begin
      EventXY:=Point(TruncToInt(Event^.X),TruncToInt(Event^.Y));
      MappedXY:=TranslateGdkPointToClientArea(Event^.Window,EventXY,CaptureWidget);
      SetCaptureControl(TWinControl(Data),MappedXY);
      //CaptureMouseForWidget(CaptureWidget,mctGTKIntf);
    end;
  end else begin
    // stop the signal, so that the widget does not auto react
    if (event^.Button<>1)
    or ((not (TControl(Data) is TCustomNoteBook))
    and (not (TControl(Data) is TCustomTabControl)))
    then begin
      g_signal_stop_emission_by_name(PGTKObject(Widget),'button-press-event');
      Result := not CallBackDefaultReturn;
    end;
  end;
  //debugln('[gtkMouseBtnPress] calling DeliverMouseDownMessage Result=',dbgs(Result));
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
  Result := CallBackDefaultReturn;
  MousePositionValid:=false;

  {$IFDEF VerboseMouseBugfix}
  debugln('[gtkMouseBtnPressAfter] ',
    DbgSName(TObject(Data)),
    ' Widget=',DbgS(Widget), ' ', gtk_type_name(Widget^.theObject.klass^.thetype),
    ' ',dbgs(TruncToInt(Event^.X)),',',dbgs(TruncToInt(Event^.Y)));
  {$ENDIF}

  ResetDefaultIMContext;
  UpdateMouseCaptureControl;

  // stop the signal, so that it is not sent to the parent widgets
  g_signal_stop_emission_by_name(PGTKObject(Widget),'button-press-event');

  if (csDesigning in TComponent(Data).ComponentState) then exit;
  if ControlGetsMouseDownBefore(TControl(Data),Widget) then exit;

  //debugln('[gtkMouseBtnPressAfter] calling DeliverMouseDownMessage');
  DeliverMouseDownMessage(Widget,Event,TWinControl(Data));
end;

{$IFDEF Gtk2}
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
    GDK_SCROLL_UP: MessE.WheelDelta := 1;
    GDK_SCROLL_DOWN: MessE.WheelDelta := -1;
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
{$ENDIF}

{-------------------------------------------------------------------------------
  function ControlGetsMouseUpBefore(AControl: TControl): boolean;

  Returns true, if mouse up event should be sent before the widget istelf
  reacts.
-------------------------------------------------------------------------------}
function ControlGetsMouseUpBefore(AControl: TControl): boolean;
begin
  Result:=true;
  if AControl=nil then ;
  {$IFDEF Gtk1}
  case AControl.fCompStyle of
  csCheckBox,
  csRadioButton,
  csToggleBox:
    Result:=false;
  end;
  {$ENDIF}
end;

{-------------------------------------------------------------------------------
  procedure DeliverMouseUpMessage(widget: PGtkWidget; event : pgdkEventButton;
    AWinControl: TWinControl);

  Translate a gdk mouse release event into a LCL mouse up message and send it.
-------------------------------------------------------------------------------}
procedure DeliverMouseUpMessage(widget: PGtkWidget; event: pgdkEventButton;
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
  if (Widget=nil) then ;
  MappedXY:=TranslateGdkPointToClientArea(Event^.Window,
                               Point(TruncToInt(Event^.X),TruncToInt(Event^.Y)),
                               PGtkWidget(AWinControl.Handle));
  //DebugLn(['DeliverMouseUpMessage ',GetWidgetDebugReport(Widget),' ',dbgsName(AWinControl),' ',dbgs(MappedXY)]);

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

  ShiftState := gtkeventstateToshiftstate(Event^.State);
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
    if MessI.Result<>0 then begin
      // handled by the LCL
      //DebugLn(['DeliverMouseUpMessage msg was handled by the LCL, Stopping signal ...']);
      g_signal_stop_emission_by_name(PGTKObject(Widget),'button-release-event');
    end;
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
  Result := CallBackDefaultReturn;
  MousePositionValid:=false;

  {$IFDEF VerboseMouseBugfix}
  DesignOnlySignal:=GetDesignOnlySignalFlag(Widget,dstMouseRelease);
  DebugLn('[gtkMouseBtnRelease] A ',DbgSName(TObject(Data)),' ',
    ' Widget=',DbgS(Widget),
    ' DSO=',dbgs(DesignOnlySignal),
    ' ',dbgs(TruncToInt(Event^.X)),',',dbgs(TruncToInt(Event^.Y)),' Btn=',dbgs(event^.Button));
  {$ENDIF}

  //DebugLn('EEE1 MouseRelease Widget=',DbgS(Widget),
  //' EventMask=',DbgS(gdk_window_get_events(Widget^.Window)),
  //' GDK_BUTTON_RELEASE_MASK=',DbgS(GDK_BUTTON_RELEASE_MASK));

  ResetDefaultIMContext;
  UpdateMouseCaptureControl;

  if not (csDesigning in TComponent(Data).ComponentState) then begin
    DesignOnlySignal:=GetDesignOnlySignalFlag(Widget,dstMouseRelease);

    ReleaseMouseCapture;
    if DesignOnlySignal or (not ControlGetsMouseUpBefore(TControl(Data))) then
    begin
      exit;
    end;
  end else begin
    // stop the signal, so that the widget does not auto react
    if (not (TControl(Data) is TCustomNoteBook))
    and (not (TControl(Data) is TCustomTabControl))
    then
      g_signal_stop_emission_by_name(PGTKObject(Widget),'button-release-event');
    Result := not CallBackDefaultReturn;
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
  Result := CallBackDefaultReturn;
  MousePositionValid:=false;

  {$IFDEF VerboseMouseBugfix}
  DebugLn('[gtkMouseBtnReleaseAfter] ',DbgSName(TObject(Data)),' ',
    ' Widget=',DbgS(Widget),
    ' ',dbgs(TruncToInt(Event^.X)),',',dbgs(TruncToInt(Event^.Y)),' Btn=',dbgs(event^.Button));
  {$ENDIF}

  // stop the signal, so that it is not sent to the parent widgets
  g_signal_stop_emission_by_name(PGTKObject(Widget),'button-release-event');

  ResetDefaultIMContext;
  UpdateMouseCaptureControl;

  if (csDesigning in TComponent(Data).ComponentState) then exit;
  if ControlGetsMouseUpBefore(TControl(Data)) then exit;

  DeliverMouseUpMessage(Widget,Event,TWinControl(Data));
end;

function gtkclickedCB(widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess: TLMessage;
begin
  Result := CallBackDefaultReturn;
  //DebugLn('[gtkclickedCB] ',TObject(Data).ClassName);
  EventTrace('clicked', data);
  if (LockOnChange(PgtkObject(Widget),0)>0) then exit;
  Mess.Msg := LM_CLICKED;
  DeliverMessage(Data, Mess);
end;

{-------------------------------------------------------------------------------
  function GTKDialogSelectRowCB
  Params: widget: PGtkWidget; data: gPointer
  Result: GBoolean

  This function is called, whenever a row is selected in a commondialog
-------------------------------------------------------------------------------}
function gtkDialogSelectRowCB(widget: PGtkWidget; Row, Column: gInt;
  bevent: pgdkEventButton; data: gPointer): GBoolean; cdecl;
var
  theDialog: TCommonDialog;
  MenuWidget: PGtkWidget;
  AFilterEntry: PFileSelFilterEntry;
  FileSelWidget: PGtkFileSelection;
  ShiftState: TShiftState;
  loop : gint;
  startRow : gint;
  endRow : gint;
begin
  //debugln('GTKDialogSelectRowCB A ');
  Result:=CallBackDefaultReturn;
  if (Data=nil) or (BEvent=nil) or (Column=0) or (Row=0) then ;
  theDialog:=TCommonDialog(GetLCLObject(Widget));
  if (theDialog is TOpenDialog) then begin
    // only process the callback if there is event data. If there isn't any
    // event data that means it was called due to a direct function call of the
    // widget and not an actual mouse click on the widget.
    FileSelWidget:=PGtkFileSelection(theDialog.Handle);
    if (bevent <> nil) and (gdk_event_get_type(bevent) = GDK_2BUTTON_PRESS)
    and (FileSelWidget^.dir_list = widget) then begin
      MenuWidget := gtk_object_get_data(PGtkObject(FileSelWidget),
                                        'LCLFilterMenu');
      if MenuWidget <> nil then begin
        AFilterEntry := gtk_object_get_data(PGtkObject(
             gtk_menu_get_active(PGtkMenu(MenuWidget))), 'LCLIsFilterMenuItem');
        if (AFilterEntry<>nil) and (AFilterEntry^.Mask<>nil) then
          PopulateFileAndDirectoryLists(FileSelWidget,AFilterEntry^.Mask);
      end;
    end
    else if (bevent <> nil)
    and (ofAllowMultiSelect in TOpenDialog(theDialog).Options)
    and (FileSelWidget^.file_list=widget) then begin
      // multi selection
      ShiftState := GTKEventStateToShiftState(BEvent^.State);
      if ssShift in ShiftState then begin
        if LastFileSelectRow <> -1 then begin
          startRow := LastFileSelectRow;
          endRow := row;
          if LastFileSelectRow > row then begin
            startRow := row;
            endRow := LastFileSelectRow;
          end;
          for loop := startRow to endRow do begin
            gtk_clist_select_row(PGtkCList(widget), loop, column);
          end;
        end;
      end
      else if not (ssCtrl in ShiftState) then begin
        gtk_clist_unselect_all(PGtkCList(widget));
        gtk_clist_select_row(PGtkCList(widget), row, column);
      end;
      LastFileSelectRow := row;
    end;
    UpdateDetailView(TOpenDialog(theDialog));
  end;
end;

function gtkDialogOKclickedCB( widget: PGtkWidget;
  data: gPointer) : GBoolean; cdecl;
var
  theDialog : TCommonDialog;
  Fpointer : Pointer;
  // colordialog
  colorsel : PGtkColorSelection;
  newColor : TGdkColor;
  // fontdialog
  FontName : String;
  ALogFont  : TLogFont;
  // filedialog
  rowNum   : gint;
  fileInfo : PGChar;
  {$IfDef GTK2}
  fileList : PPgchar;
  FontDesc: PPangoFontDescription;
  {$else}
  cListRow : PGList;
  fileList : PGTKCList;
  {$EndIf}
  DirName  : string;
  FileName : string;
  Files: TStringList;
  CurFilename: string;
  //SelectedFont: PGdkFont;

  function CheckOpenedFilename(var AFilename: string): boolean;
  {$IFDEF GTK1}
  var
    MenuWidget: PGtkWidget;
    AFilterEntry: PFileSelFilterEntry;
  {$ENDIF}
  begin
    Result:=true;
    {$IFDEF GTK1}
    // check if entered file are a real directory
    if DirectoryExists(AFileName) then
    begin
      // if only interested in directories, then it's done
      if TheDialog is TSelectDirectoryDialog then
      begin
        result := True;
        exit;
      end;
      // if allowed then jump to that directory
      if not (ofNoChangeDir in TOpenDialog(TheDialog).Options) then
      begin
        // change dir
        gtk_file_selection_set_filename(PGtkFileSelection(FPointer),
                                        PChar(AppendPathDelim(AFileName)));

        // populate file list
        MenuWidget := gtk_object_get_data(PGtkObject(PGtkFileSelection(FPointer)), 'LCLFilterMenu');
        if (MenuWidget <> nil) then begin
          AFilterEntry := gtk_object_get_data(PGtkObject(gtk_menu_get_active(
                                              PGtkMenu(MenuWidget))), 'LCLIsFilterMenuItem');
          if ((AFilterEntry<>nil) and (AFilterEntry^.Mask<>nil)) then
          PopulateFileAndDirectoryLists(PGtkFileSelection(FPointer), AFilterEntry^.Mask);
        end;
      end;
      // wait for correct input
      result:=False;
      Exit;
    end;

    // maybe user entered nonexistent dir
    if ((AFileName<>'') and (IsPathDelimiter(AFileName, Length(AFileName)))) then
    begin
      // cant jump to nonexistent dir
      if not (ofNoChangeDir in TOpenDialog(TheDialog).Options) then
        MessageDlg(rsfdDirectoryMustExist, Format(rsfdDirectoryNotExist,[AFileName]),
                   mtError, [mbCancel], 0); // GTK2 shows "The folder contents could not be displayed"
      // wait for correct input
      result:=False;
      Exit;
    end;
    
    // check if user selected a file while requesting a directory
    if (AFileName<>'') and (TheDialog is TSelectDirectoryDialog) then begin
      if DirectoryExists(ExtractFilePath(AFileName)) then begin
        AFileName:=ExtractFilePath(AFileName);
        result:=true;
        exit;
      end;
    end;
    
    {$ENDIF}
    // maybe file already exists
    if (ofOverwritePrompt in TOpenDialog(theDialog).Options)
    and FileExists(AFilename) then
    begin
      Result:=MessageDlg(rsfdOverwriteFile,
                         Format(rsfdFileAlreadyExists,[AFileName]),
                         mtConfirmation,[mbOk,mbCancel],0)=mrOk;
      if not Result then exit;
    end;
  end;

  procedure AddFile(List: TStrings; const NewFile: string);
  var
    i: Integer;
  begin
    for i:=0 to List.Count-1 do
      if List[i]=NewFile then exit;
    List.Add(NewFile);
  end;

begin
  Result := True;
  if (Widget=nil) then ;
  theDialog := TCommonDialog(data);
  FPointer := Pointer(theDialog.Handle);

  if theDialog is TFileDialog then
  begin
    {$IfDef GTK2}
    FileName:=gtk_file_chooser_get_filename(PGtkFileChooser(FPointer));
    {$ELSE}
    FileName:=gtk_file_selection_get_filename(
                                PGtkFileSelection(FPointer));
    {$ENDIF}
    if theDialog is TOpenDialog then
    begin
      // check extra options
      if ofAllowMultiSelect in TOpenDialog(theDialog).Options then
      begin
        DirName:=ExtractFilePath(FileName);
        TFileDialog(data).FileName := '';
        Files:=TStringList(TFileDialog(theDialog).Files);
        Files.Clear;
        if (Filename<>'') then begin
          Result:=CheckOpenedFilename(Filename);
          if not Result then exit;
          AddFile(Files,FileName);
        end;
        {$IfDef GTK2}
        fileList := gtk_file_selection_get_selections(PGtkFileSelection(FPointer));
        rowNum := 0;
        While FileList^ <> nil do
        begin
          fileInfo := FileList^;
          CurFilename:=fileInfo; // convert PChar to AnsiString (not typecast)
          if (CurFilename<>'') and (Files.IndexOf(CurFilename)<0) then begin
            CurFilename:=DirName+fileInfo;
            Result:=CheckOpenedFilename(CurFilename);
            if not Result then exit;
            Files.Add(CurFilename);
          end;
          inc(FileList);
          inc(rowNum);
        end;
        Dec(FileList, rowNum);
        g_strfreev(fileList);
        {$Else}
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
              CurFilename:=DirName+fileInfo;
              Result:=CheckOpenedFilename(CurFilename);
              if not Result then exit;
              AddFile(Files,CurFilename);
            end;
          end;
          // get next row from list
          rowNum := rowNum + 1;
          cListRow := g_list_next(cListRow);
        end;
        {$EndIf}
      end else begin
        Result:=CheckOpenedFilename(Filename);
        if not Result then exit;
        TFileDialog(data).FileName := Filename;
      end;
    end
    else
    begin
      TFileDialog(data).FileName := Filename;
    end;
  end
  else if theDialog is TColorDialog then
  begin
    colorSel := PGtkColorSelection(PGtkColorSelectionDialog(FPointer)^.colorsel);
    gtk_color_selection_get_current_color(colorsel, @newColor);
    TColorDialog(theDialog).Color := TGDKColorToTColor(newcolor);
    {$IFDEF VerboseColorDialog}
    DebugLn('gtkDialogOKclickedCB ',DbgS(TColorDialog(theDialog).Color));
    {$ENDIF}
  end
  else if theDialog is TFontDialog then
  begin
    Assert(False, 'Trace:Pressed OK in FontDialog');
    FontName := gtk_font_selection_dialog_get_font_name(
                                             pgtkfontselectiondialog(FPointer));
    //debugln('gtkDialogOKclickedCB FontName=',FontName);
    //SelectedFont:=gdk_font_load(PChar(FontName));
    //debugln('gtkDialogOKclickedCB ',dbgs(SelectedFont));
    
    if IsFontNameXLogicalFontDesc(FontName) then begin
      // extract basic font attributes from the font name in XLFD format
      ALogFont:=XLFDNameToLogFont(FontName);
      TFontDialog(theDialog).Font.Assign(ALogFont);
      // set the font name in XLFD format
      // a font name in XLFD format overrides in the gtk interface all other font
      // settings.
      TFontDialog(theDialog).Font.Name := FontName;
    end else begin
      {$ifdef GTK2}
      FontDesc := pango_font_description_from_string(PChar(FontName));
      with TFontDialog(theDialog).Font do
      begin
        BeginUpdate;
        Size := pango_font_description_get_size(FontDesc) div PANGO_SCALE;
        if pango_font_description_get_weight(FontDesc) >= PANGO_WEIGHT_BOLD then
          Style := Style + [fsBold]
        else
          Style := Style - [fsBold];
        if pango_font_description_get_style(FontDesc) > PANGO_STYLE_NORMAL then
          Style := Style + [fsItalic]
        else
          Style := Style - [fsItalic];
        Name := pango_font_description_get_family(FontDesc);
        EndUpdate;
      end;
      pango_font_description_free(FontDesc);
      {$else}
      debugln('Gtk1 should have valid XLogicalFontDesc!');
      {$endif}
    end;

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
  Result := CallBackDefaultReturn;
  if (Widget=nil) then ;
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
  Result := CallBackDefaultReturn;
  if (Widget=nil) then ;
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
  {$ifdef GTK2}
  FontDesc: PPangoFontDescription;
  {$endif}
begin
  Result := CallBackDefaultReturn;
  if (Widget=nil) then ;
  theDialog := TCommonDialog(data);
  if (theDialog is TFontDialog)
  and (fdApplyButton in TFontDialog(theDialog).Options)
  and (Assigned(TFontDialog(theDialog).OnApplyClicked)) then begin
    FontName := gtk_font_selection_dialog_get_font_name(
                                    pgtkfontselectiondialog(theDialog.Handle));
    if IsFontNameXLogicalFontDesc(FontName) then begin
      // extract basic font attributes from the font name in XLFD format
      ALogFont:=XLFDNameToLogFont(FontName);
      TFontDialog(theDialog).Font.Assign(ALogFont);
      // set the font name in XLFD format
      // a font name in XLFD format overrides in the gtk interface all other font
      // settings.
      TFontDialog(theDialog).Font.Name := FontName;
    end else begin
      {$ifdef GTK2}
      FontDesc := pango_font_description_from_string(PChar(FontName));
      with TFontDialog(theDialog).Font do
      begin
        BeginUpdate;
        Size := pango_font_description_get_size(FontDesc) div PANGO_SCALE;
        if pango_font_description_get_weight(FontDesc) >= PANGO_WEIGHT_BOLD then
          Style := Style + [fsBold]
        else
          Style := Style - [fsBold];
        if pango_font_description_get_style(FontDesc) > PANGO_STYLE_NORMAL then
          Style := Style + [fsItalic]
        else
          Style := Style - [fsItalic];
        Name := pango_font_description_get_family(FontDesc);
        EndUpdate;
      end;
      pango_font_description_free(FontDesc);
      {$else}
      debugln('Gtk1 should have valid XLogicalFontDesc!');
      {$endif}
    end;
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
  if (Data=nil) then ;
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
  Widget: PGtkWidget;
begin
  //DebugLn(['UpdateDetailView ']);
  Widget:=PGtkWidget(OpenDialog.Handle);
  {$IfDef GTK2}
  FileName:=gtk_file_chooser_get_filename(PGtkFileChooser(Widget));
  {$ELSE}
  FileName:=gtk_file_selection_get_filename(
                              PGtkFileSelection(Widget));
  {$ENDIF}
  OldFilename:=OpenDialog.Filename;
  if Filename=OldFilename then exit;
  OpenDialog.Filename:=Filename;
  // tell application, that selection has changed
  OpenDialog.DoSelectionChange;
  if (OpenDialog.OnFolderChange<>nil)
  and (ExtractFilePath(Filename)<>ExtractFilePath(OldFilename)) then
    OpenDialog.DoFolderChange;
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
  //debugln('GTKDialogKeyUpDownCB A ');
  Result:=CallBackDefaultReturn;

  if (Widget=nil) then ;
  case gdk_event_get_type(Event) of

  GDK_KEY_RELEASE, GDK_KEY_PRESS:
    begin
      if Event^.KeyVal = GDK_KEY_Escape
      then begin
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
var
  LCLComponent: TObject;
begin
  if (Data=nil) then ;
  gdk_window_set_events(GetControlWindow(Widget),
    gdk_window_get_events(GetControlWindow(Widget))
      or GDK_KEY_RELEASE_MASK or GDK_KEY_PRESS_MASK);
  LCLComponent:=GetLCLObject(Widget);
  if LCLComponent is TCommonDialog then
    TCommonDialog(LCLComponent).DoShow;
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
  //debugln('GTKDialogFocusInCB A ');
  Result:=CallBackDefaultReturn;
  if (Data=nil) then ;
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
  var
    AFilterEntry: PFileSelFilterEntry;
  begin
    if FilterWidget=nil then exit;
    AFilterEntry:=gtk_object_get_data(PGtkObject(FilterWidget),
                                      'LCLIsFilterMenuItem');
    if (AFilterEntry<>nil) and (AFilterEntry^.Mask<>nil) then
    begin
      PopulateFileAndDirectoryLists(PGtkFileSelection(theDialog.Handle),
        AFilterEntry^.Mask);
      TFileDialog(TheDialog).IntfFileTypeChanged(AFilterEntry^.FilterIndex + 1);
      UpdateDetailView(TOpenDialog(theDialog));
    end;
  end;

var
  AHistoryEntry: PFileSelHistoryEntry;
  {$IFDEF Gtk1}
  FilterMenu, ActiveFilterMenuItem: PGtkWidget;
  {$ENDIF}
begin
  Result:=false;
  if (Data=nil) then ;
  theDialog:=TCommonDialog(GetNearestLCLObject(Widget));
  if (theDialog is TOpenDialog) then begin
    // check if history activated
    AHistoryEntry:=gtk_object_get_data(PGtkObject(Widget),
                                       'LCLIsHistoryMenuItem');
    if (AHistoryEntry<>nil) and (AHistoryEntry^.Filename<>nil) then begin
      // user has choosen a history file
      // -> select it in the filedialog
      {$IFDEF GTK1}
      gtk_file_selection_complete(PGtkFileSelection(theDialog.Handle),
        AHistoryEntry^.Filename);
      // restore filter
      if DirPathExists(AHistoryEntry^.Filename) then begin
        FilterMenu:=gtk_object_get_data(PGtkObject(theDialog.Handle),
                                        'LCLFilterMenu');
        if FilterMenu<>nil then begin
          ActiveFilterMenuItem:=gtk_menu_get_active(GTK_MENU(FilterMenu));
          CheckFilterActivated(ActiveFilterMenuItem);
        end;
      end;
      {$ELSE}
      gtk_file_chooser_set_current_folder(PGtkFileChooser(theDialog.Handle),AHistoryEntry^.Filename);
      {$ENDIF}
      UpdateDetailView(TOpenDialog(theDialog));
    end;
    {$IFDEF GTK1}
    // check if filter activated
    CheckFilterActivated(Widget);
    {$ENDIF}
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
  if (Widget=nil) then ;
  TCommonDialog(data).UserChoice := mrAbort;
  TCommonDialog(data).Close;
end;

function gtkPressedCB( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  Result := CallBackDefaultReturn;

  if (Widget=nil) then ;
  {$IFDEF EventTrace}
  EventTrace('pressed', data);
  {$ENDIF}
  Mess.msg := LM_PRESSED;
  DeliverMessage(Data, Mess);

  Result := CallBackDefaultReturn;
end;

function gtkEnterCB(widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  Result := CallBackDefaultReturn;

  {$IFDEF EventTrace}
  EventTrace('enter', data);
  {$ENDIF}

  if csDesigning in TControl(Data).ComponentState then begin
    // stop the signal, so that the widget does not auto react
    g_signal_stop_emission_by_name(PGTKObject(Widget),'enter');
  end;

  Mess.msg := CM_MOUSEENTER;
  DeliverMessage(Data, Mess);

  Result := CallBackDefaultReturn;
end;

function gtkLeaveCB(widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  Result := CallBackDefaultReturn;

  {$IFDEF EventTrace}
  EventTrace('leave', data);
  {$ENDIF}

  if csDesigning in TControl(Data).ComponentState then begin
    // stop the signal, so that the widget does not auto react
    g_signal_stop_emission_by_name(PGTKObject(Widget),'leave');
  end;

  Mess.msg := CM_MOUSELEAVE;
  DeliverMessage(Data, Mess);

  Result := CallBackDefaultReturn;
end;

function gtkMoveCursorCB(widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  Result := CallBackDefaultReturn;

  if (Widget=nil) then ;
  EventTrace('move-cursor', data);
  Mess.msg := LM_MOVECURSOR;
  DeliverMessage(Data, Mess);
end;

function gtksize_allocateCB(widget: PGtkWidget; size: pGtkAllocation;
  data: gPointer) : GBoolean; cdecl;
{$IFDEF VerboseSizeMsg}
var
  w, h: Gint;
{$ENDIF}
begin
  Result := CallBackDefaultReturn;

  EventTrace('size-allocate', data);

  with Size^ do Assert(False, Format('Trace:[gtksize_allocateCB] %s --> X: %d, Y: %d, Width: %d, Height: %d', [TObject(data).ClassName, X, Y, Width, Height]));

  if not (TObject(Data) is TControl) then begin
    // owner is not TControl -> ignore
    DebugLn('WARNING: gtksize_allocateCB: Data is not TControl. Data=',
            DbgS(Data),' ',GetWidgetClassName(Widget));
    if Data<>nil then
      DebugLn('  Data=',TObject(Data).ClassName);
    RaiseGDBException('');
    exit;
  end;

  {$IFDEF VerboseSizeMsg}
  w:=0; h:=0;
  if Widget^.window<>nil then
    gdk_window_get_size(Widget^.window,@w,@h);
  DebugLn(['gtksize_allocateCB: ',
    DbgSName(TControl(Data)),
    ' widget=',GetWidgetDebugReport(Widget),
    ' fixwidget=',DbgS(GetFixedWidget(Widget)),
    ' NewSize=',Size^.Width,',',Size^.Height,
    ' GtkPos=',Widget^.allocation.x,',',Widget^.allocation.y,
               ',',Widget^.allocation.width,'x',Widget^.allocation.height,
    ' LCLPos=',TControl(Data).Left,',',TControl(Data).Top,
               ',',TControl(Data).Width,'x',TControl(Data).Height,
    ' gdkwindow=',w,'x',h]);
  {$ENDIF}
  {$IFDEF VerboseFormPositioning}
  if TControl(Data) is TCustomForm then
    DebugLn('VFP gtksize_allocateCB: ',TControl(Data).ClassName,' ',dbgs(Size^.X),',',dbgs(Size^.Y));
  {$ENDIF}
  if GTK_WIDGET_REALIZED(Widget) then begin
    { The gtk sends the size messages after the resizing. Therefore the parent
      widget is already resized, but the parent resize message will be emitted
      after all its childs. So, the gtk resizes in top-bottom order, just like the
      LCL. But it sends size messages in bottom-top order, which can result in
      many resizes in the LCL.
    }
    {$IFDEF Gtk1}
    // All resize messages between lcl and gtk1 are cached.
    SaveSizeNotification(Widget);
    {$ELSE}
    // The resize messages of the inner widget of a Control is cached.
    // This is the resize of a outer widget and sent to the LCL immediately.
    SendSizeNotificationToLCL(Widget);
    {$ENDIF}
  end;
end;

function gtksize_allocate_client(widget: PGtkWidget; size: pGtkAllocation;
  data: gPointer): GBoolean; cdecl;
var
  MainWidget: PGtkWidget;
  ClientWidget: PGtkWidget;
begin
  Result := CallBackDefaultReturn;

  if (Widget=nil) or (Size=nil) then ;
  if (TObject(Data) is TWinControl) then begin
    {$IFDEF VerboseSizeMsg}
    DebugLn('gtksize_allocate_client: ',
      TControl(Data).Name,':',TControl(Data).ClassName,
      ' widget=',DbgS(Widget),
      ' NewSize=',dbgs(Size^.Width),',',dbgs(Size^.Height),
      ' Allocation='+dbgs(widget^.Allocation.Width)+'x'+dbgs(Widget^.Allocation.Height),
      ' Requisiton='+dbgs(widget^.Requisition.Width)+'x'+dbgs(Widget^.Requisition.Height)
      );
    {$ENDIF}
    if not TWinControl(Data).HandleAllocated then begin
      exit;
    end;
    MainWidget:=PGtkWidget(TWinControl(Data).Handle);
    ClientWidget:=GetFixedWidget(MainWidget);
    if GTK_WIDGET_REALIZED(ClientWidget) then begin
      // the gtk resizes bottom to top, that means the
      // inner widget (client area) is resized before the outer widget
      // is resized. Because the LCL reads both sizes, keep this message back.
      SaveClientSizeNotification(ClientWidget);
    end;
  end else begin
    // owner is not TWinControl -> ignore
    DebugLn('WARNING: gtksize_allocate_client: Data is not TWinControl. Data=',
            DbgS(Data));
    exit;
  end;
end;

function gtkswitchpage(widget: PGtkWidget; page: Pgtkwidget; pagenum: integer;
  data: gPointer): GBoolean; cdecl;
var
  Mess: TLMNotify;
  NMHdr: tagNMHDR;
  SwitchAllowed: Boolean;
begin
  Result := CallBackDefaultReturn;

  if (Widget=nil) or (Page=nil) then ;

  EventTrace('switch-page', data);
  UpdateNoteBookClientWidget(TObject(Data));

  // gtkswitchpage is called before the switch

  // send first the TCN_SELCHANGING to ask if switch is allowed
  FillChar(Mess,SizeOf(Mess),0);
  Mess.Msg := LM_NOTIFY;
  FillChar(NMHdr,SizeOf(NMHdr),0);
  NMHdr.code := TCN_SELCHANGING;
  NMHdr.hwndfrom := PtrUInt(widget);
  NMHdr.idfrom := pagenum;  //use this to set pageindex to the correct page.
  Mess.NMHdr := @NMHdr;
  Mess.Result := 0;
  DeliverMessage(Data, Mess);
  SwitchAllowed:=Mess.Result=0;
  if not SwitchAllowed then begin
    debugln('gtkswitchpage A SwitchAllowed=false not yet implemented');
  end;

  // then send the new page
  FillChar(Mess,SizeOf(Mess),0);
  Mess.Msg := LM_NOTIFY;
  FillChar(NMHdr,SizeOf(NMHdr),0);
  NMHdr.code := TCN_SELCHANGE;
  NMHdr.hwndfrom := PtrUInt(widget);
  NMHdr.idfrom := pagenum;  //use this to set pageindex to the correct page.
  Mess.NMHdr := @NMHdr;
  DeliverMessage(Data, Mess);
end;


function gtkconfigureevent( widget: PGtkWidget; event : PgdkEventConfigure;
  data: gPointer) : GBoolean; cdecl;
var
  Allocation : TGtkAllocation;
begin

  { This signal is emitted for top level controls only, i.e. only controls
    that are not children. Thus, we register this event only for forms.
    This event is fired when the form is sized, moved or changes Z order.
  }

  FillChar(Allocation,SizeOf(TGtkAllocation),0);
  with Allocation do begin
    X:= Event^.X;
    Y:= Event^.Y;
    Width:= Event^.Width;
    Height:= Event^.Height;
  end;
  Result:= gtksize_allocateCB( Widget, @Allocation, Data);
end;

function gtkreleasedCB( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMEssage;
begin
  Result := CallBackDefaultReturn;

  if (Widget=nil) then ;
  {$IFDEF EventTrace}

  EventTrace('released', data);
  {$ENDIF}
  Mess.msg := LM_RELEASED;
  DeliverMessage(Data, Mess);
end;

function gtkInsertText(widget: PGtkWidget; char : pChar;
  NewTextLength : Integer; Position : pgint; data: gPointer) : GBoolean; cdecl;
var
  Memo: TCustomMemo;
  CurrLength, CutLength: integer;
begin
  Result := CallBackDefaultReturn;

  { GTK does not provide its own max. length for memos
    we have to do our own. }

  if TObject(Data) is TCustomMemo then begin
    if (NewTextLength = 1) and (char^ = #13) and (LineEnding = #10) then
      char^ := #10;
    Memo:= TCustomMemo(Data);
    if Memo.MaxLength < 0 then Exit;

    CurrLength:= gtk_text_get_length(PGtkText(widget));
    if CurrLength + NewTextLength <= Memo.MaxLength then Exit;

    CutLength:= CurrLength + NewTextLength - Memo.MaxLength;

    if NewTextLength - CutLength > 0 then begin
      gtk_editable_insert_text(PGtkEditable(widget), char,
                               NewTextLength - CutLength, Position);
    end;

    g_signal_stop_emission_by_name(PGtkObject(widget), 'insert_text');
  end;
  if TObject(Data) is TCustomEdit then
    if (NewTextLength = 1) and (char^ = #13) then
      g_signal_stop_emission_by_name(PGtkObject(widget), 'insert_text');
end;

function gtkDeleteText( widget: PGtkWidget; Startpos, EndPos : Integer;
  data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Delete Text', data);
  if (StartPos=0) or (EndPos=0) or (Widget=nil) then ;
  Mess.msg := LM_DELETETEXT;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function gtkSetEditable( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Set Editable', data);
  if (Widget=nil) then ;
  Mess.msg := LM_SETEDITABLE;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function gtkMoveWord( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
   Mess : TLMessage;
begin
  EventTrace('Move Word', data);
  if (Widget=nil) then ;
  Mess.msg := LM_MOVEWORD;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function gtkMovePage( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Move Page', data);
  if (Widget=nil) then ;
  Mess.msg := LM_MOVEPAGE;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function gtkMoveToRow( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Move To Row!!', data);
  if (Widget=nil) then ;
  Mess.msg := LM_MOVETOROW;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function gtkMoveToColumn( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('MoveToColumn', data);
  if (Widget=nil) then ;
  Mess.msg := LM_MOVETOCOLUMN;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function gtkKillChar( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Kill Char', data);
  if (Widget=nil) then ;
  Mess.msg := LM_KILLCHAR;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function gtkKillWord( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Kill Word', data);
  if (Widget=nil) then ;
  Mess.msg := LM_KILLWORD;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function gtkKillLine( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Kill Line', data);
  if (Widget=nil) then ;
  Mess.msg := LM_KILLLINE;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function gtkCutToClip( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Cut to clip', data);
  if (Widget=nil) then ;
  Mess.msg := LM_CUTTOCLIP;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function gtkCopyToClip( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Copy to Clip', data);
  if (Widget=nil) then ;
  Mess.msg := LM_COPYTOCLIP;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function gtkPasteFromClip( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Paste from clip', data);
  if (Widget=nil) then ;
  Mess.msg := LM_PASTEFROMCLIP;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function gtkValueChanged(widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  Result := CallBackDefaultReturn;
  EventTrace('Value changed', data);
  if (Widget=nil) then ;
  Mess.msg := LM_CHANGED;
  DeliverMessage(Data, Mess);
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
function gtkTimerCB(Data: gPointer): {$IFDEF Gtk2}gBoolean{$ELSE}gint{$ENDIF}; cdecl;
var
  TimerInfo: PGtkITimerinfo;
begin
  EventTrace ('TimerCB', nil);
  Result := GdkFalse;  // assume: timer will stop

  TimerInfo:=PGtkITimerinfo(Data);

  if (FTimerData=nil) or (FTimerData.IndexOf(Data)<0) then begin
    {$IFDEF VerboseTimer}
    DebugLn('gtkTimerCB Timer was killed: TimerInfo=',DbgS(TimerInfo));
    {$ENDIF}
    // timer was killed
    Result:=GdkFalse; // stop timer
  end else begin
    {$IFDEF VerboseTimer}
    DebugLn('gtkTimerCB Timer Event: TimerInfo=',DbgS(TimerInfo));
    {$ENDIF}
    if TimerInfo^.TimerFunc <> nil
    then begin
      // Call users timer function
      TimerInfo^.TimerFunc;
      Result:=GdkTrue; // timer will go on
    end
    else begin
      Result := GdkFalse; // stop timer
    end;
  end;

  if (Result<>GdkFalse) and (FTimerData.IndexOf(Data)<0) then begin
    // timer was killed
    // -> stop timer
    Result:=GdkFalse;
  end;

  if Result=GdkFalse then begin
    {$IFDEF VerboseTimer}
    DebugLn('gtkTimerCB Timer was STOPPED: TimerInfo=',DbgS(TimerInfo));
    {$ENDIF}
    // timer will be stopped
    // -> free timer data, if not already done
    if (FTimerData<>nil) and (FTimerData.IndexOf(Data)>=0) then begin
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
  Result := CallBackDefaultReturn;
  //DebugLn('[gtkFocusInNotifyCB] ',TControl(data).Name,':',TObject(data).ClassName);
  {$IFDEF EventTrace}
  EventTrace ('FocusInNotify (alias Enter)', data);
  {$ENDIF}

  if (Event=nil) then ;
  if csDesigning in TControl(Data).ComponentState then begin
    // stop the signal, so that the widget does not auto react
    g_signal_stop_emission_by_name(PGTKObject(Widget),'focus-in-event');
  end;

  MessI.msg := LM_Enter;
  DeliverMessage(Data, MessI);
end;

function gtkFocusOutNotifyCB (widget : PGtkWidget; event : PGdkEvent;
  data : gpointer) : GBoolean; cdecl;
var
  MessI : TLMExit;
begin
  Result := CallBackDefaultReturn;
  //DebugLn('[gtkFocusOutNotifyCB] ',TControl(data).Name,':',TObject(data).ClassName);
  {$IFDEF EventTrace}
  EventTrace ('FocusOutNotify (alias Exit)', data);
  {$ENDIF}

  if (Event=nil) then ;
  if csDesigning in TControl(Data).ComponentState then begin
    // stop the signal, so that the widget does not auto react
    g_signal_stop_emission_by_name(PGTKObject(Widget),'focus-out-event');
  end;

  MessI.msg := LM_Exit;
  DeliverMessage(Data, MessI);
end;

{$IFDEF gtk1}
function gtk_range_get_update_policy(range: PGTKRange): TGtkUpdateType;
begin
  result := policy(Range^);
end;
{$ENDIF}

function get_gtk_scroll_type(range: PGTKRange): TGtkScrollType;
{$IFNDEF gtk1}
type
  TUnOpaqueTimer=record
    timeout_id: guint;
    ScrollType: TGTkScrollType;
  end;
  PUnOpaqueTimer=^TUnopaqueTimer;
{$ENDIF}
begin
{$IFDEF gtk1}
  Result := Scroll_type(Range^);
{$ELSE}
  if (gtk_major_version=2) and (gtk_minor_version<6) and
     (Range^.Timer<>nil) then
    { gtk2 pre gtk2.6 ONLY, tested gtk2.0.
      Later versions (gtk2.6+) have a change-value signal that includes scrolltype anyways }

    Result := PUnOpaqueTimer(Range^.Timer)^.ScrollType
  else
    Result := GTK_SCROLL_NONE;
{$ENDIF}
end;

{$IFDEF VerboseGtkScrollbars}
procedure DebugScrollStyle(Scroll: LongInt);
begin
  DbgOut('TYPE=');
  case Scroll of
    GTK_SCROLL_NONE: DbgOut('GTK_SCROLL_NONE ');
    GTK_SCROLL_STEP_BACKWARD: DbgOut('GTK_SCROLL_STEP_BACKWARD ');
    GTK_SCROLL_STEP_FORWARD: DbgOut('GTK_SCROLL_STEP_FORWARD ');
    GTK_SCROLL_PAGE_BACKWARD: DbgOut('GTK_SCROLL_PAGE_BACKWARD ');
    GTK_SCROLL_PAGE_FORWARD: DbgOut('GTK_SCROLL_PAGE_FORWARD ');
    GTK_SCROLL_JUMP: DbgOut('GTK_SCROLL_JUMP ');
    {$ifdef gtk2}
    GTK_SCROLL_STEP_UP: DbgOut('GTK_SCROLL_STEP_UP');
    GTK_SCROLL_STEP_DOWN: DbgOut('GTK_SCROLL_STEP_DOWN');
    GTK_SCROLL_PAGE_UP: DbgOut('GTK_SCROLL_PAGE_UP');
    GTK_SCROLL_PAGE_DOWN: DbgOut('GTK_SCROLL_PAGE_DOWN');
    GTK_SCROLL_STEP_LEFT: DbgOut('GTK_SCROLL_STEP_LEFT');
    GTK_SCROLL_STEP_RIGHT: DbgOut('GTK_SCROLL_STEP_RIGHT');
    GTK_SCROLL_PAGE_LEFT: DbgOut('GTK_SCROLL_PAGE_LEFT');
    GTK_SCROLL_PAGE_RIGHT: DbgOut('GTK_SCROLL_PAGE_RIGHT');
    GTK_SCROLL_START: DbgOut('GTK_SCROLL_START');
    GTK_SCROLL_END: DbgOut('GTK_SCROLL_END');
    {$endif}
    else
      DbgOut(IntToStr(Scroll), '->?');
  end;
end;
{$ENDIF VerboseGtkScrollbars}

function ScrollTypeToSbCode(IsVertSB: boolean; ScrollType: TGtkScrollType;
                            UpdatePolicy: TGtkUpdateType): Integer;
begin
  case ScrollType of
    GTK_SCROLL_STEP_BACKWARD:
      if IsVertSB then
        Result := SB_LINEUP
      else
        Result := SB_LINELEFT;
    GTK_SCROLL_STEP_FORWARD:
      if IsVertSB then
        Result := SB_LINEDOWN
      else
        Result := SB_LINERIGHT;
    GTK_SCROLL_PAGE_BACKWARD:
      if IsVertSB then
        Result := SB_PAGEUP
      else
        Result := SB_PAGELEFT;
    GTK_SCROLL_PAGE_FORWARD:
      if IsVertSB then
        Result := SB_PAGEDOWN
      else
        Result := SB_PAGERIGHT;
    {$ifdef GTK2}
    GTK_SCROLL_STEP_UP:
      Result := SB_LINEUP;
    GTK_SCROLL_STEP_DOWN:
      Result := SB_LINEDOWN;
    GTK_SCROLL_PAGE_UP:
      Result := SB_PAGEUP;
    GTK_SCROLL_PAGE_DOWN:
      Result := SB_PAGEDOWN;
    GTK_SCROLL_STEP_LEFT:
      Result := SB_LINELEFT;
    GTK_SCROLL_STEP_RIGHT:
      Result := SB_LINERIGHT;
    GTK_SCROLL_PAGE_LEFT:
      Result := SB_PAGELEFT;
    GTK_SCROLL_PAGE_RIGHT:
      Result := SB_PAGERIGHT;
    GTK_SCROLL_START:
      if IsVertSB then
        Result := SB_TOP
      else
        Result := SB_LEFT;
    GTK_SCROLL_END:
      if IsVertSB then
        Result := SB_BOTTOM
      else
        Result := SB_RIGHT;
    {$endif}
    else
      begin
        {$IFDEF VerboseGtkScrollbars}
        debugln('ScrollTypeToSbCode: Scroll_type=', IntToStr(ScrollType));
        {$Endif}
        if UpdatePolicy=GTK_UPDATE_CONTINUOUS then
          Result := SB_THUMBTRACK
        else
          Result := SB_THUMBPOSITION;
      end;
  end;
end;

function GTKHScrollCB(Adjustment: PGTKAdjustment; data: GPointer): GBoolean; cdecl;
var
  Msg: TLMHScroll;
  Scroll: PGtkRange;
  ScrollType: TGtkScrollType;
begin
  Result := CallBackDefaultReturn;
  Assert(False, Format('Trace:[GTKHScrollCB] Value: %d', [RoundToInt(Adjustment^.Value)]));
  Scroll := PgtkRange(gtk_object_get_data(PGTKObject(Adjustment), odnScrollBar));
  if Scroll<>nil then begin
    Msg.Msg := LM_HSCROLL;
    with Msg do begin
      Pos := Round(Adjustment^.Value);
      if Pos < High(SmallPos)
      then SmallPos := Pos
      else SmallPos := High(SmallPos);
      ScrollBar := HWND(PtrUInt(Scroll));
      ScrollType := get_gtk_scroll_type(Scroll);
      ScrollCode := ScrollTypeToSbCode(False, ScrollType,
                                       gtk_range_get_update_policy(Scroll));
    end;
    DeliverMessage(Data, Msg);
  end;
end;

function GTKVScrollCB(Adjustment: PGTKAdjustment; data: GPointer): GBoolean; cdecl;
var
  Msg: TLMVScroll;
  Scroll: PGtkRange;
  ScrollType: TGtkScrollType;
begin
  Result := CallBackDefaultReturn;
  Assert(False, Format('Trace:[GTKVScrollCB] Value: %d', [RoundToInt(Adjustment^.Value)]));
  Scroll := PgtkRange(gtk_object_get_data(PGTKObject(Adjustment), odnScrollBar));
  if Scroll<>nil then begin
    Msg.Msg := LM_VSCROLL;
    with Msg do begin
      Pos := Round(Adjustment^.Value);
      if Pos < High(SmallPos)
      then SmallPos := Pos
      else SmallPos := High(SmallPos);
      //DebugLn('GTKVScrollCB A Adjustment^.Value=',dbgs(Adjustment^.Value),' SmallPos=',dbgs(SmallPos));
      ScrollBar := HWND(PtrUInt(Scroll));
      ScrollType := get_gtk_scroll_type(Scroll);
      // GTK1 has a bug with wheel mouse. It sometimes gives the wrong direction.
      ScrollCode := ScrollTypeToSbCode(True, ScrollType,
                                       gtk_range_get_update_policy(Scroll));
      //DebugLn('GTKVScrollCB B Adjustment^.Value=',dbgs(Adjustment^.Value),' ScrollCode=',dbgs(ScrollCode),' ScrollType=',dbgs(ScrollType));
    end;
    DeliverMessage(Data, Msg);
  end;
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
var
  KeyStateList: TFPList;

  procedure UpdateToggleList(const AVKeyCode: Integer);
  begin
    // Check for a toggle
    // If the remove was successfull, the key was on
    // else it was off so we should set the toggle flag
    if KeyStateList.Remove(Pointer(PtrInt(AVKeyCode or KEYMAP_TOGGLE))) < 0
    then KeyStateList.Add(Pointer(PtrInt(AVKeyCode or KEYMAP_TOGGLE)));
  end;

  procedure UpdateList(const AVKeyCode: Integer; const APressed: Boolean);
  begin
    if AVKeyCode = 0 then Exit;
    if APressed
    then begin
      if KeyStateList.IndexOf(Pointer(PtrInt(AVKeyCode))) < 0
      then KeyStateList.Add(Pointer(PtrInt(AVKeyCode)));
    end
    else begin
      KeyStateList.Remove(Pointer(PtrInt(AVKeyCode)));
    end;
  end;

const
  STATE_MAP: array[0..3] of TShiftStateEnum = (
    ssShift,
    ssCtrl,
    ssAlt,
    ssSuper
  );
  VK_MAP: array[0..3] of array[0..2] of Byte = (
    // (Main key, alt key 1, alt key 2) to check
    (VK_SHIFT, VK_LSHIFT, VK_RSHIFT),
    (VK_CONTROL, VK_LCONTROL, VK_RCONTROL),
    (VK_MENU, VK_LMENU, VK_RMENU),
    (VK_LWIN, VK_RWIN, 0)
  );
var
  KeyCode: Word;
  KCInfo: TKeyCodeInfo;
  VKey: Byte;
  Pressed, InState: Boolean;

  n: Integer;
  ShiftState: TShiftState;
begin
  Result := 0;

  // TODO: Remove when KeyStateList is obsolete

  case gdk_event_get_type(Event) of
    GDK_KEY_PRESS: Pressed := True;
    GDK_KEY_RELEASE: Pressed := False;
  else
    // not interested
    Exit;
  end;
  
  {$ifdef gtk1}
    KeyCode := XKeysymToKeycode(gdk_display, Event^.keyval);
  {$else}
    KeyCode := Event^.hardware_keycode;
  {$endif}
  //DebugLn('GTKKeySnooper: KeyCode=%u -> %s', [KeyCode, {$IFDEF GTK2} Event^._String {$ELSE} Event^.theString {$ENDIF}]);
  
  if KeyCode > High(MKeyCodeInfo)
  then begin
    if Pressed
    then DebugLn('[WARNING] Key pressed with keycode (%u) larger than expected: K=0x%x S="%s"', [
      KeyCode,
      Event^.KeyVal,
      {$IFDEF GTK2} Event^._String {$ELSE} Event^.theString {$ENDIF}
    ]);
    Exit;
  end;
  
  KCInfo := MKeyCodeInfo[KeyCode];
  if KCInfo.VKey1 = 0
  then begin
    if Pressed
    then DebugLn('[WARNING] Key pressed without VKey: K=0x%x S="%s"', [
      Event^.KeyVal,
      {$IFDEF GTK2} Event^._String {$ELSE} Event^.theString {$ENDIF}
    ]);
    Exit;
  end;

  if FuncData = nil then exit;
  KeyStateList := TObject(FuncData) as TFPList;
  
  ShiftState := GTKEventStateToShiftState(Event^.State);
  
  if (KCInfo.Flags and KCINFO_FLAG_SHIFT_XOR_NUM <> 0)
  and ((ssShift in ShiftState) xor (ssNum in ShiftState))
  then VKey := KCInfo.VKey2
  else VKey := KCInfo.VKey1;

  UpdateList(VKey, Pressed);
  if (KCInfo.Flags and KCINFO_FLAG_TOGGLE <> 0)
  then UpdateToggleList(VKey);

  // Add special left and right codes
  case Event^.KeyVal of
    GDK_Key_Shift_L:   UpdateList(VK_LSHIFT,   Pressed);
    GDK_Key_Shift_R:   UpdateList(VK_RSHIFT,   Pressed);
    GDK_Key_Control_L: UpdateList(VK_LCONTROL, Pressed);
    GDK_Key_Control_R: UpdateList(VK_RCONTROL, Pressed);
    GDK_Key_Alt_L:     UpdateList(VK_LMENU,    Pressed);
    GDK_Key_Alt_R:     UpdateList(VK_RMENU,    Pressed);
  end;

  // Recheck the list against the modifiers
  for n := 0 to High(STATE_MAP) do
  begin
    // Skip our current key, since the state is updated after the event
    if VKey = VK_MAP[n][0] then Continue;
    if VKey = VK_MAP[n][1] then Continue;
    if VKey = VK_MAP[n][2] then Continue;

    InState := STATE_MAP[n] in ShiftState;
    UpdateList(VK_MAP[n][0], InState);
    UpdateList(VK_MAP[n][1], InState);
    UpdateList(VK_MAP[n][2], InState);
  end;

  // if the VKey has multiple VK_codes then SHIFT distinguishes between them
  // In that case SHIFT is not pressed
  // On the next event the shift flag will be restored based on modifiers
  if Pressed and ((KCInfo.Flags and KCINFO_FLAG_SHIFT_XOR_NUM) <> 0)
  then begin
    UpdateList(VK_SHIFT,  False);
    UpdateList(VK_LSHIFT, False);
    UpdateList(VK_RSHIFT, False);
  end;
end;

function gtkYearChanged(Widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  MSG: TLMessage;
begin
  Result := CallBackDefaultReturn;

  if (Widget=nil) then ;
  EventTrace('year changed', data);
  MSG.Msg := LM_YEARCHANGED;
  DeliverPostMessage(Data, MSG);
end;

procedure GtkDragDataReceived(widget:PGtkWidget; context:PGdkDragContext;
  x:gint; y:gint; selection_data:PGtkSelectionData; info:guint; time:guint; Data: gPointer);cdecl;
var
  S: TStringList;
  I: Integer;
  FileName: String;
  Files: Array of String;
  Form: TControl;
  Result: Boolean;
begin
  //DebugLn('GtkDragDataReceived ' + PChar(selection_data^.data));
  Result := False;
  
  if selection_data^.data <> nil then // data is list of uri
  begin
    SetLength(Files, 0);
    S := TStringList.Create;
    try
      S.Text := PChar(selection_data^.data);

      for I := 0 to S.Count - 1 do
      begin
        FileName := S[I];

        if FileName = '' then Continue;
        // uri -> file name, maybe for GTK2: g_filename_from_uri
        // uri = protocol://hostname/file name
        // convert uri of local files to file name
        if Copy(FileName, 1, 17) = 'file://localhost/' then Delete(FileName, 1, 17)
        else
          if SameText(Copy(FileName, 1, 8), 'file:///') then
          begin
            Delete(FileName, 1, 8);
            // do not remove UNIX root path
            if (Pos(':', FileName) <> 2) and (Pos('/', FileName) <> 1) then
              FileName := '/' + FileName;
          end;
        // otherwise: protocol and hostname are preserved!

        if FileName = '' then Continue;
        SetLength(Files, Length(Files) + 1);
        Files[High(Files)] := FileName;
        //DebugLn('GtkDragDataReceived ' + DbgS(I) + ': ' + PChar(FileName));
      end;
    finally
      S.Free;
    end;
    
    if Length(Files) > 0 then
    begin
      Form := nil;
      if (TObject(Data) is TWinControl) then
        Form := (TObject(Data) as TWinControl).GetTopParent;
        
      if Form is TCustomForm then
        (Form as TCustomForm).IntfDropFiles(Files)
      else
        if (Application <> nil) and (Application.MainForm <> nil) then
          Application.MainForm.IntfDropFiles(Files);

      if Application <> nil then
        Application.IntfDropFiles(Files);
        
      Result := True;
    end;
  end;
  
  gtk_drag_finish(Context, Result, false, time);
end;

{------------------------------------------------------------------------------
  ClipboardSelectionReceivedHandler

  This handler is called whenever a clipboard owner sends data. Because the LCL
  caches all requests, this is typically data from another application.
  Copy the received selection data record and buffer to
  internal record and buffer (ClipboardSelectionData)
 ------------------------------------------------------------------------------}
procedure ClipboardSelectionReceivedHandler(TargetWidget: PGtkWidget;
  SelectionData: PGtkSelectionData; TimeID: guint32; Data: Pointer); cdecl;
var TempBuf: Pointer;
  c: PClipboardEventData;
  i: integer;
begin
  // at any time there can be several requests
  // find the request with the correct TimeID
  if (Data=nil) or (TargetWidget=nil) then ;
  i:=ClipboardSelectionData.Count-1;
  while (i>=0) do begin
    c:=PClipboardEventData(ClipboardSelectionData[i]);
    if c^.TimeID=TimeID then break;
    dec(i);
  end;
  {$IFDEF DEBUG_CLIPBOARD}
  DebugLn('[ClipboardSelectionReceivedHandler] A TimeID=',dbgs(TimeID),' RequestIndex=',dbgs(i),
    ' selection=',dbgs(SelectionData^.selection)+'='+GdkAtomToStr(SelectionData^.selection),
    ' target=',dbgs(SelectionData^.Target)+'='+GdkAtomToStr(SelectionData^.Target),
    ' theType=',dbgs(SelectionData^.{$IFDEF Gtk1}theType{$ELSE}_type{$ENDIF})+'='+GdkAtomToStr(SelectionData^.{$IFDEF Gtk1}theType{$ELSE}_type{$ENDIF}),
    ' format=',dbgs(SelectionData^.format),
    ' len=',dbgs(SelectionData^.length)
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
  DebugLn('[ClipboardSelectionReceivedHandler] B DataLen=',dbgs(c^.Data.Length));
  {$ENDIF}
  if (c^.Data.Data<>nil)
  and (c^.Data.Length>0) then begin
    GetMem(TempBuf,c^.Data.Length);
    Move(c^.Data.Data^,TempBuf^,c^.Data.Length);
    c^.Data.Data:=TempBuf;
    {$IFDEF DEBUG_CLIPBOARD}
    DebugLn('[ClipboardSelectionReceivedHandler] C FirstCharacter=',dbgs(ord(PChar(c^.Data.Data)[0])));
    {$ENDIF}
  end else begin
    {if (SelectionData^.Target <> GDK_TARGET_STRING)
    and (SelectionData^.length<0) then begin
      if gtk_selection_convert (TargetWidget, SelectionData^.selection,
        GDK_TARGET_STRING, TimeID)<>GdkFalse
      then begin
        DebugLn('[ClipboardSelectionReceivedHandler] D TimeID=',dbgs(TimeID),' RequestIndex=',dbgs(i),
          ' selection=',dbgs(SelectionData^.selection)+'='+GdkAtomToStr(SelectionData^.selection),
          ' target=',dbgs(SelectionData^.Target)+'='+GdkAtomToStr(SelectionData^.Target),
          ' theType=',dbgs(SelectionData^.theType)+'='+GdkAtomToStr(SelectionData^.theType),
          ' format=',dbgs(SelectionData^.format),
          ' len=',dbgs(SelectionData^.length)
          );
      end;
    end;}
    c^.Data.Data:=nil;
  end;
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
var ClipboardType: TClipboardType;
  MemStream: TMemoryStream;
  FormatID: cardinal;
  Buffer: Pointer;
  BufLength: integer;
  BitCount: integer;
  P: PChar;
begin
  {$IFDEF DEBUG_CLIPBOARD}
  DebugLn('*** [ClipboardSelectionRequestHandler] START');
  {$ENDIF}
  if (Data=nil) or (TimeID=0) or (Info=0) or (TargetWidget=nil) then ;
  if SelectionData^.Target=0 then exit;
  for ClipboardType:=Low(TClipboardType) to High(TClipboardType) do begin
    if SelectionData^.Selection=ClipboardTypeAtoms[ClipboardType] then begin
      if Assigned(ClipboardHandler[ClipboardType]) then begin
        // handler found for this of clipboard
        // now create a stream and find a supported format
        {$IFDEF DEBUG_CLIPBOARD}
        DebugLn('[ClipboardSelectionRequestHandler] "',ClipboardTypeName[ClipboardType],'" Format=',GdkAtomToStr(SelectionData^.Target),' ID=',dbgs(SelectionData^.Target));
        {$ENDIF}
        MemStream:=TMemoryStream.Create;
        try
          // the gtk-interface provides automatically some formats, that the lcl
          // does not know. Wrapping them to lcl formats ...
          FormatID:=SelectionData^.Target;
          if ((FormatID=gdk_atom_intern('COMPOUND_TEXT',GdkTrue))
            and (ClipboardExtraGtkFormats[ClipboardType][gfCOMPOUND_TEXT]))
          or ((FormatID=gdk_atom_intern('UTF8_STRING',GdkTrue))
            and (ClipboardExtraGtkFormats[ClipboardType][gfUTF8_STRING]))
          or ((FormatID=gdk_atom_intern('STRING',GdkTrue))
            and (ClipboardExtraGtkFormats[ClipboardType][gfSTRING]))
          or ((FormatID=gdk_atom_intern('TEXT',GdkTrue))
            and (ClipboardExtraGtkFormats[ClipboardType][gfTEXT]))
          then
            FormatID:=gdk_atom_intern('text/plain',GdkTrue);

          {$IFDEF DEBUG_CLIPBOARD}
          DebugLn('[ClipboardSelectionRequestHandler] FormatID=',dbgs(FormatID),'=',GdkAtomToStr(FormatID),' ',dbgs(ClipboardExtraGtkFormats[ClipboardType][gfCOMPOUND_TEXT]));
          {$ENDIF}
          // get the requested data by calling the handler for this clipboard type
          ClipboardHandler[ClipboardType](FormatID,MemStream);
          MemStream.Position:=0;

          // build clipboard data for gtk
          Buffer:=nil;
          BufLength:=0;
          BitCount:=8;

          // if the format was wrapped, transform it back
          if (FormatID=gdk_atom_intern('text/plain',GdkTrue)) then begin
            if (SelectionData^.Target=gdk_atom_intern('COMPOUND_TEXT',GdkTrue))
            then begin
              // transform text/plain to COMPOUND_TEXT
              BufLength:=integer(MemStream.Size);
              P:=StrAlloc(BufLength+1);
              MemStream.Read(P^,BufLength);
              P[BufLength]:=#0;

              BufLength:=0;
              gdk_string_to_compound_text(P,
                @SelectionData^.{$ifdef GTK2}_Type{$ELSE}theType{$ENDIF},
                @SelectionData^.Format,ppguchar(@Buffer),@BufLength);
              StrDispose(P);
              gtk_selection_data_set(SelectionData,SelectionData^.Target,
                SelectionData^.Format,Buffer,BufLength);
              gdk_free_compound_text(Buffer);
              exit;
            end;
          end;
          if Buffer=nil then begin
            {$IFDEF DEBUG_CLIPBOARD}
            DebugLn('[ClipboardSelectionRequestHandler] Default MemStream.Size=',dbgs(MemStream.Size));
            {$ENDIF}
            BufLength:=integer(MemStream.Size);
            if BufLength>0 then begin
              GetMem(Buffer,BufLength);
              MemStream.Read(Buffer^,BufLength);
              {SetLength(s,MemStream.Size);
              MemStream.Position:=0;
              MemStream.Read(s[1],MemStream.Size);
              DebugLn(' >>> "',s,'"');}
            end;
          end;
          {$IFDEF DEBUG_CLIPBOARD}
          DebugLn('[ClipboardSelectionRequestHandler] Len=',dbgs(BufLength));
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
end;

{------------------------------------------------------------------------------
  ClipboardSelectionLostOwnershipHandler

  This signal is emitted if another application gets the clipboard ownership.
 ------------------------------------------------------------------------------}
function ClipboardSelectionLostOwnershipHandler(TargetWidget: PGtkWidget;
  EventSelection: PGdkEventSelection;  Data: Pointer): cardinal; cdecl;
var ClipboardType: TClipboardType;
begin
  if (Data=nil) or (TargetWidget=nil) then ;
  //DebugLn('*** [ClipboardSelectionLostOwnershipHandler] ',DbgS(targetwidget));
  for ClipboardType:=Low(TClipboardType) to High(TClipboardType) do
    if EventSelection^.Selection=ClipboardTypeAtoms[ClipboardType] then begin
      {$IFDEF DEBUG_CLIPBOARD}
      DebugLn('*** [ClipboardSelectionLostOwnershipHandler] ',ClipboardTypeName[ClipboardType]);
      {$ENDIF}
      if (ClipboardWidget<>nil)
      and (gdk_selection_owner_get(ClipboardTypeAtoms[ClipboardType])
        <> GetControlWindow(ClipboardWidget))
      and Assigned(ClipboardHandler[ClipboardType]) then begin
        // handler found for this type of clipboard
        {$IFDEF DEBUG_CLIPBOARD}
        DebugLn('[ClipboardSelectionLostOwnershipHandler] ',ClipboardTypeName[ClipboardType]);
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

  Handler for style changes. For example the user changes the theme.
  But also called on every widget realize, so it should not release all styles
  everytime.
-------------------------------------------------------------------------------}
Procedure GTKStyleChanged(Widget: PGtkWidget; previous_style : PGTKStyle;
  Data: Pointer); cdecl;
begin
  if (Widget=nil) or (Data=nil) or (previous_style=nil) then ;
  {$IFDEF EventTrace}
  EventTrace('style-set', nil);
  {$ENDIF}
  //ReleaseAllStyles;
end;

function gtkListBoxSelectionChangedAfter(widget: PGtkWidget; data: gPointer
  ): GBoolean; cdecl;
var
  Mess: TLMessage;
  GtkList: PGtkList;
begin
  Result := CallBackDefaultReturn;
  {$IFDEF EventTrace}
  EventTrace('gtkListSelectionChangedAfter', data);
  {$ENDIF}
  GtkList:=PGtkList(widget);
  if (GtkList^.selection = nil) or (LockOnChange(PGtkObject(widget),0) > 0) then
    Exit;
  FillChar(Mess,SizeOf(Mess),0);
  Mess.msg := LM_SelChange;
  if gtkListGetSelectionMode(GtkList)=GTK_SELECTION_SINGLE then
    gtk_list_set_selection_mode(GtkList,GTK_SELECTION_BROWSE);
  DeliverMessage(Data, Mess);
end;

{$I gtkDragCallback.inc}
{$I gtkComboBoxCallback.inc}
{$I gtkPageCallback.inc}

{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}


