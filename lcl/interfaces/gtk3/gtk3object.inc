{%MainUnit gtk3int.pas}

type
  { TGtk3ToolWnd }

  // Used for AllocateHWnd
  TGtk3ToolWnd = class(TGtk3Widget)
  strict private
    FWndMethod: TWndMethod;
  public
    constructor Create(AWndMethod: TWndMethod);
    function DeliverMessage(var Msg; const AIsInputEvent: Boolean=False): LRESULT; override;
  end;

  TApplicationAccess = class(TApplication);

  constructor TGtk3ToolWnd.Create(AWndMethod: TWndMethod);
  begin
    CreateFrom(nil, nil);
    FWndMethod := AWndMethod;
  end;

  function TGtk3ToolWnd.DeliverMessage(var Msg; const AIsInputEvent: Boolean): LRESULT;
  var
    LMessage: TLMessage absolute Msg;
  begin
    FWndMethod(LMessage);
    Result := LMessage.Result;
  end;

{ TGtk3WidgetSet }

var
  Gtk3MPF: TGPollFunc;

function Gtk3PollFunction(ufds:PGPollFD; nfsd:guint; timeout:gint):gint; cdecl;
begin
  Result := nfsd;
  if TimeOut = -1 then
    Gtk3WidgetSet.FMainPoll := ufds
  else
    Gtk3WidgetSet.FMainPoll := nil;
  if Gtk3MPF <> nil then
  begin
    // writeln('Gtk3PollFunction timeout ',TimeOut,' nfsd ',nfsd,' ufds.revents ',ufds^.revents);
    Gtk3MPF(ufds, nfsd, timeout);
  end;
end;

{hook into gtk3 main event loop, used for testing purposes only atm.}
procedure Gtk3MainEventLoop(AEvent: PGdkEvent; {%H-}AData: gPointer); cdecl;
var
  AWS: TGtk3WidgetSet;
  ETime: guint32;
begin
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('** TGtk3WidgetSet.Gtk3MainEventLoop **');
  {$ENDIF}
  AWS := TGtk3WidgetSet(AData);

  if (AEvent^.type_ in [GDK_KEY_PRESS, GDK_BUTTON_PRESS, GDK_TOUCH_BEGIN, GDK_SCROLL]) then
  begin
    ETime := gdk_event_get_time(AEvent);
    if ETime <> 0 then
      AWS.LastUserEventTime := ETime;
  end;
  if AEvent^.type_ = GDK_FOCUS_CHANGE then
  begin
    {$IFDEF GTK3DEBUGACTIVATION}
    DebugLn(['[', GetTickCount64, '] GDK_FOCUS_CHANGE in=', AEvent^.focus_change.in_,
      ' AppActive=', Assigned(Application) and Application.Active]);
    {$ENDIF}
    if (AEvent^.focus_change.in_ <> 0) and
       Assigned(Application) and not Application.Active and not Application.Terminated then
      Application.IntfAppActivate;
  end;
  gtk_main_do_event(AEvent);
end;

procedure TGtk3WidgetSet.Gtk3Create;
var
  AGtkThread: PGThread;
  AId: String;
begin
  g_type_init;
  gtk_init(@argc, @argv);
  AGtkThread := g_thread_self();
  if not IsLibrary then
  begin
    AId := 'org.lcl.thread_' + dbgHex({%H-}PtrUInt(AGtkThread));
    FGtk3Application := {%H-}TGtkApplication.new(PgChar(AId), [G_APPLICATION_NON_UNIQUE]);
    // FGtk3Application^.set_application_id(PgChar(AId));
    FGtk3Application^.register(nil, nil);
  end;
  GTK3WidgetSet := Self;
  {$IFDEF UNIX}
  FIsWayland := CompareText(G_OBJECT_TYPE_NAME(gdk_display_get_default),'gdkwaylanddisplay') = 0;
  {$ELSE}
  FIsWayland := False;
  {$ENDIF}
  FIMContext := PGtkIMContext(gtk_im_multicontext_new);
  g_signal_connect_data(PGObject(FIMContext), 'commit',
    TGCallback(@Gtk3IMCommitCB), Self, nil, G_CONNECT_DEFAULT);
end;

procedure TGtk3WidgetSet.Gtk3Destroy;
begin
  if FIMContext <> nil then
  begin
    g_object_unref(PGObject(FIMContext));
    FIMContext := nil;
  end;
  Gtk3MPF := nil;
  GTK3WidgetSet := nil;
  WakeMainThread := nil;

  if FDockImage <> nil then
  begin
    gtk_widget_destroy(FDockImage);
    FDockImage := nil;
  end;

  if FDragIPCWidget <> nil then
  begin
    gtk_widget_destroy(FDragIPCWidget);
    FDragIPCWidget := nil;
  end;

  if Assigned(FAppIcon) then
    FAppIcon^.unref;
  FAppIcon := nil;
end;

procedure TGtk3WidgetSet.SetDefaultAppFontName;
var
  AValue: TGValue;
begin
  FillByte(AValue{%H-}, SizeOf(AValue), 0);
  AValue.init(G_TYPE_STRING);
  g_object_get_property(gtk_settings_get_default, 'gtk-font-name', @AValue);
  FDefaultAppFontName := AValue.get_string;
  AValue.unset;
end;

procedure TGtk3WidgetSet.InitSysColorBrushes;
var
  i: integer;
  LogBrush: TLogBrush;
begin
  LogBrush.lbHatch := 0;
  FillChar(LogBrush, SizeOf(TLogBrush), 0);
  for i := Low(FSysColorBrushes) to High(FSysColorBrushes) do
  begin
    LogBrush.lbColor := GetSysColor(i);
    FSysColorBrushes[i] := CreateBrushIndirect(LogBrush);
    TGtk3Brush(FSysColorBrushes[i]).Shared := True;
  end;
end;

procedure TGtk3WidgetSet.FreeSysColorBrushes;

  procedure DeleteAndNilObject(var h: HGDIOBJ);
  begin
    if h <> 0 then
      TGtk3Brush(h).Shared := False;
    DeleteObject(h);
    h := 0;
  end;

var
  i: Integer;
begin
  for i := Low(FSysColorBrushes) to High(FSysColorBrushes) do
    if FSysColorBrushes[i] <> HBRUSH(-1) then
      DeleteAndNilObject(FSysColorBrushes[i]);
end;

function TGtk3WidgetSet.IsWayland: boolean;
begin
  Result := FIsWayland;
end;

function TGtk3WidgetSet.IsKDEPlasmaWaylandSession: boolean;
var
  s: string;
begin
  //NOTE: this is hack for Qt5 KDE Plasma wayland, so might
  //be unnecessary with KDE Plasma 6.
  Result := False;
  if not FIsWayland then
    exit;
  s := SysUtils.GetEnvironmentVariable('XDG_CURRENT_DESKTOP');
  Result := SameText(s, 'KDE') or SameText(s, 'PLASMA') or SameText(s, 'KDE-Plasma');
end;

function TGtk3WidgetSet.GetWMName: string;
{$IFDEF UNIX}
var
  AScreen: PGdkScreen;
  AName: Pgchar;
{$ENDIF}
begin
  if FWMNameCached then
  begin
    Result := FWMName;
    exit;
  end;
  FWMNameCached := True;
  {$IFDEF UNIX}
  if FIsWayland then
  begin
    FWMName := 'wayland';
    Result := FWMName;
    exit;
  end;
  AScreen := gdk_screen_get_default;
  if AScreen <> nil then
  begin
    AName := gdk_x11_screen_get_window_manager_name(AScreen);
    if AName <> nil then
      FWMName := AName
    else
      FWMName := '';
  end else
    FWMName := '';
  {$ELSE}
  FWMName := '';
  {$ENDIF}
  Result := FWMName;
end;

function TGtk3WidgetSet.IsMarcoWM: boolean;
begin
  Result := Pos('marco', LowerCase(GetWMName)) > 0;
end;

{$IFNDEF UNIX}
procedure TGtk3WidgetSet.DoWakeMainThread(Sender: TObject);
begin
  g_main_context_wakeup(g_main_context_default);
  CheckSynchronize;
end;
{$ENDIF}

function TGtk3WidgetSet.CreateDCForWidget(AWidget: PGtkWidget;
  AWindow: PGdkWindow; cr: Pcairo_t): HDC;
begin
  if AWindow <> nil then
  begin
    Result := HDC(TGtk3DeviceContext.Create(AWindow, False))
  end else
  begin
    if cr <> nil then
      Result := HDC(TGtk3DeviceContext.CreateFromCairo(AWidget, Cr))
    else
      Result := HDC(TGtk3DeviceContext.Create(AWidget, False));
  end;
  TGtk3DeviceContext(Result).CanRelease := True;
end;

procedure TGtk3WidgetSet.AddWindow(AWindow: PGtkWindow);
begin
  if Assigned(FGtk3Application) then
    FGtk3Application^.add_window(AWindow);
end;

{$IFDEF UNIX}
var
  _thread_sync_pending: integer;
  threadsync_pipein, threadsync_pipeout: cint;
  threadsync_giochannel: pgiochannel;
  childsig_pending: boolean;

  procedure ChildEventHandler({%H-}sig: longint; {%H-}siginfo: psiginfo;
    {%H-}sigcontext: psigcontext); cdecl;
  begin
    childsig_pending := true;
    WakeMainThread(nil);
  end;

  procedure InstallSignalHandler;
  var
    child_action: sigactionrec;
  begin
    child_action.sa_handler := @ChildEventHandler;
    fpsigemptyset(child_action.sa_mask);
    child_action.sa_flags := 0;
    fpsigaction(SIGCHLD, @child_action, nil);
  end;

  function threadsync_iocallback({%H-}source: PGIOChannel; {%H-}condition: TGIOCondition;
    data: gpointer): gboolean; cdecl;
  var
    thrashspace: array[1..1024] of byte;
  begin
    InterlockedExchange(_thread_sync_pending, 0);
    // read the sent bytes
    fpread(threadsync_pipein, {%H-}thrashspace[1], 1);

    Result := true;
    // one of children signaled ?
    if childsig_pending then
    begin
      childsig_pending := false;
      TGtk3WidgetSet(data).ProcessChildSignal;
    end;
    // execute the to-be synchronized method
    if IsMultiThread then
      CheckSynchronize;
  end;

procedure TGtk3WidgetSet.InitSynchronizeSupport;
begin
  WakeMainThread := @PrepareSynchronize;
  assignpipe(threadsync_pipein, threadsync_pipeout);
  threadsync_giochannel := g_io_channel_unix_new(threadsync_pipein);
  g_io_add_watch(threadsync_giochannel, [G_IO_IN], @threadsync_iocallback, Self);
end;

procedure TGtk3WidgetSet.ProcessChildSignal;
var
  pid: tpid;
  reason: TChildExitReason;
  status: integer;
  info: dword;
  handler, nextHandler: PChildSignalEventHandler;
begin
  handler := FChildSignalHandlers;
  while handler <> nil do
  begin
    nextHandler := handler^.NextHandler;
    status := 0;
    pid := fpwaitpid(handler^.pid, status, WNOHANG);
    if (pid = handler^.pid) and (wifexited(status) or wifsignaled(status)) then
    begin
      if wifexited(status) then
      begin
        reason := cerExit;
        info := wexitstatus(status);
      end
      else
      begin
        reason := cerSignal;
        info := wtermsig(status);
      end;
      handler^.OnEvent(handler^.UserData, reason, info);
    end;
    handler := nextHandler;
  end;
end;

procedure TGtk3WidgetSet.PrepareSynchronize(AObject: TObject);
var
  thrash: char;
begin
  if InterlockedExchange(_thread_sync_pending, 1) = 1 then
    exit; // signal already pending

  // wake up GUI thread by sending a byte through the threadsync pipe
  thrash:='l';
  fpwrite(threadsync_pipeout, thrash, 1);
end;
{$ENDIF}

constructor TGtk3WidgetSet.Create;
var
  AValue: TGValue;
  i: Integer;
begin
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  inherited Create;
  FDockImage := nil;

  FDragImageList := nil;
  FDragImageListIcon := nil;
  FDragHotSpot := Point(0, 0);
  FDragImageLock := False;

  FActivityCounter := 0;
  FCSSTheme := TStringList.Create;
  FSystemMetricsList := TIntegerList.Create;
  for i := 0 to SM_LCLHasFormAlphaBlend do
    FSystemMetricsList.Add(0);
  FThemeName := '';
  FTimerData := TFPList.Create;
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('** TGtk3WidgetSet.Create **');
  {$ENDIF}
  LastUserEventTime := GDK_CURRENT_TIME;
  Gtk3Create;
  FMainPoll := nil;
  Gtk3MPF := g_main_context_get_poll_func(g_main_context_default);
  g_main_context_set_poll_func(g_main_context_default, @Gtk3PollFunction);

  {install our event handler, so we can see all events before its delivery to GdkWindows}
  gdk_event_handler_set(@Gtk3MainEventLoop, Self, nil);

  {$IFDEF UNIX}
  InitSynchronizeSupport;
  InstallSignalHandler;
  {$ELSE}
  WakeMainThread := @DoWakeMainThread;
  {$ENDIF}
  CharSetEncodingList := TList.Create;
  CreateDefaultCharsetEncodings;
  FillByte(AValue{%H-}, SizeOf(AValue), 0);
  AValue.init(G_TYPE_BOOLEAN);
  AValue.set_boolean(True);
  g_object_set_property(gtk_settings_get_default,'gtk-button-images',@AValue);
  AValue.unset;

  FOverlayScrolling := True;
  if GetEnvironmentVariable('GTK_OVERLAY_SCROLLING') = '0' then
    FOverlayScrolling := False;
  if FOverlayScrolling then
    g_object_get(gtk_settings_get_default,'gtk-overlay-scrolling', [@FOverlayScrolling, nil]);

  FGlobalCursor := 0;
  FAppIcon := nil;
  FStockNullBrush := 0;
  FStockBlackBrush := 0;
  FStockLtGrayBrush := 0;
  FStockGrayBrush := 0;
  FStockDkGrayBrush := 0;
  FStockWhiteBrush := 0;

  FStockNullPen := 0;
  FStockBlackPen := 0;
  FStockWhitePen := 0;
  FStockSystemFont := 0;
  FStockDefaultDC := 0;

  Styles := TStringList.Create;

  InitStockItems;
  // initialize default app font name
  SetDefaultAppFontName;
  // InitSysColorBrushes;
  for i := Low(FSysColorBrushes) to High(FSysColorBrushes) do
    FSysColorBrushes[i] := HBRUSH(-1);

  // g_object_set_property(gtk_settings_get_default,'gtk-enable-mnemonics',@AValue);
  // g_object_set_property(gtk_settings_get_default,'gtk-auto-mnemonics',@AValue);


  cssProvider := gtk_css_provider_new();
  if FileExists('theme.css') then
    gtk_css_provider_load_from_path(cssProvider, 'theme.css', nil);
  gtk_style_context_add_provider_for_screen(gdk_screen_get_default(),
                               PGtkStyleProvider(cssProvider),
                               GTK_STYLE_PROVIDER_PRIORITY_USER);
end;

destructor TGtk3WidgetSet.Destroy;
begin
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('** TGtk3WidgetSet.Destroy **');
  {$ENDIF}
  if Assigned(cssProvider) then
    cssProvider^.unref;
  ReleaseAllStyles;
  Styles.Free;
  Styles := nil;
  FreeStockItems;
  Gtk3Destroy;
  FTimerData.Free;
  Gtk3DefaultContext.Free;
  Gtk3ScreenContext.Free;
  if Assigned(CharSetEncodingList) then
  begin
    ClearCharsetEncodings;
    FreeAndNil(CharSetEncodingList);
  end;
  FreeSysColorBrushes;
  FCSSTheme.Free;
  FSystemMetricsList.Free;
  DoneGtk3KeyboardTables;
  inherited Destroy;
end;

function TGtk3WidgetSet.LCLPlatform: TLCLPlatform;
begin
  Result := lpGtk3;
end;

//This is the only way to fix missing corner widget in GtkScrolledWindow.
procedure ApplyCustomScrollbarStyles(const APixels: integer);
var
  CssProvider: PGtkCssProvider;
begin
  CssProvider := gtk_css_provider_new();
  gtk_css_provider_load_from_data(CssProvider,
    PgChar(Format('scrollbar.horizontal { margin-right: %dpx; }' +
    'scrollbar.vertical { margin-bottom: %dpx; }',[APixels, APixels])),
    -1, nil);

  gtk_style_context_add_provider_for_screen(
    gdk_screen_get_default(),
    PGtkStyleProvider(CssProvider),
    GTK_STYLE_PROVIDER_PRIORITY_APPLICATION
  );
end;

procedure ApplyCustomSpinButtonCSS;
var
  CssProvider: PGtkCssProvider;
  Screen: PGdkScreen;
  CSSData: PChar =
    'spinbutton > entry {' +
    '    min-width: 2px;' +
    '    min-height: 16px;' +
    '}';

begin
  CssProvider := gtk_css_provider_new();
  gtk_css_provider_load_from_data(CssProvider, CSSData, -1, nil);
  Screen := gdk_screen_get_default();
  gtk_style_context_add_provider_for_screen(Screen,
    PGtkStyleProvider(CssProvider), GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
  g_object_unref(CssProvider);
end;

function Gtk3RefreshThemeIdle(AData: gpointer): gboolean; cdecl;
var
  AList, AItem: PGList;
  i: Integer;
begin
  ReleaseAllStyles;
  GetStyleWidget(lgsButton);
  GetStyleWidget(lgsWindow);
  GetStyleWidget(lgsMemo);
  for i := Low(Gtk3WidgetSet.FSysColorBrushes) to High(Gtk3WidgetSet.FSysColorBrushes) do
    if (Gtk3WidgetSet.FSysColorBrushes[i] <> HBRUSH(-1)) and (Gtk3WidgetSet.FSysColorBrushes[i] <> 0) then
    begin
      TGtk3Brush(Gtk3WidgetSet.FSysColorBrushes[i]).Context := nil;
      TGtk3Brush(Gtk3WidgetSet.FSysColorBrushes[i]).Color := SysColorMap[i];
    end;
  Gtk3WidgetSet.LoadThemeMetrics;
  AList := gtk_window_list_toplevels;
  AItem := AList;
  while AItem <> nil do
  begin
    if Gtk3IsWidget(PGObject(AItem^.data)) then
    begin
      gtk_widget_queue_resize(PGtkWidget(AItem^.data));
      gtk_widget_queue_draw(PGtkWidget(AItem^.data));
    end;
    AItem := AItem^.next;
  end;
  if AList <> nil then
    g_list_free(AList);
  Result := gtk_false;
end;

procedure Gtk3ThemeNameNotify(AObject: PGObject; APSpec: gpointer; AData: gpointer); cdecl;
begin
  g_idle_add(@Gtk3RefreshThemeIdle, nil);
end;

procedure TGtk3WidgetSet.LoadThemeMetrics;

  procedure QueryIndicatorBoxSize(out W, H: gint);
  var
    Context: PGtkStyleContext;
    Path: PGtkWidgetPath;
    min_width, min_height: gint;
    Pad, Bord: TGtkBorder;
    CtxState: TGtkStateFlags;
    ChildPos: gint;
  begin
    CtxState := GTK_STATE_FLAG_NORMAL;

    Path := gtk_widget_path_new;
    gtk_widget_path_append_type(Path, gtk_check_button_get_type);
    gtk_widget_path_iter_set_object_name(Path, 0, 'checkbutton');
    gtk_widget_path_iter_set_state(Path, 0, CtxState);

    ChildPos := gtk_widget_path_append_type(Path, G_TYPE_NONE);
    gtk_widget_path_iter_set_object_name(Path, ChildPos, 'check');
    gtk_widget_path_iter_add_class(Path, ChildPos, 'check');
    gtk_widget_path_iter_set_state(Path, ChildPos, CtxState);

    Context := gtk_style_context_new;
    gtk_style_context_set_path(Context, Path);
    gtk_style_context_set_state(Context, CtxState);
    gtk_widget_path_unref(Path);

    min_width := 0;
    min_height := 0;

    gtk_style_context_get(Context, CtxState, ['min-width', @min_width, 'min-height', @min_height, nil]);

    FillChar(Pad{%H-}, SizeOf(Pad), 0);
    FillChar(Bord{%H-}, SizeOf(Bord), 0);

    gtk_style_context_get_padding(Context, CtxState, @Pad);
    gtk_style_context_get_border(Context, CtxState, @Bord);
    g_object_unref(Context);

    if min_width <= 0 then
      min_width := 16;
    if min_height <= 0 then
      min_height := 16;

    W := min_width + Pad.left + Pad.right + Bord.left + Bord.right;
    H := min_height + Pad.top + Pad.bottom + Bord.top + Bord.bottom;
  end;

var
  menuBar: PGtkMenuBar;
  menuItem: PGtkMenuItem;
  checkBox: PGtkCheckButton;
  scrollBarV: PGtkScrollbar;
  scrollBarH: PGtkScrollbar;
  MinH, PrefH: gint;
  IndW, IndH: gint;
  window: PGtkWindow;
  box: PGtkBox;
  header: PGtkHeaderBar;
begin
  window := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_decorated(window, False);
  gtk_widget_set_app_paintable(window, gtk_true);
  gtk_widget_set_size_request(window, 1, 1);

  box := gtk_box_new(GTK_ORIENTATION_VERTICAL, 4);
  gtk_container_add(Window, box);

  menuBar := gtk_menu_bar_new;
  menuItem := gtk_menu_item_new_with_label('');
  menuBar^.append(menuItem);
  gtk_box_pack_start(box, MenuBar, FALSE, FALSE, 0);

  checkBox := gtk_check_button_new_with_label('');
  gtk_box_pack_start(box, checkbox, FALSE, FALSE, 0);

  scrollBarH := gtk_scrollbar_new(GTK_ORIENTATION_HORIZONTAL, nil);
  gtk_box_pack_start(box, scrollBarH, FALSE, FALSE, 0);
  scrollBarV := gtk_scrollbar_new(GTK_ORIENTATION_VERTICAL, nil);
  gtk_box_pack_start(box, scrollBarV, FALSE, FALSE, 0);

  header := gtk_header_bar_new;
  gtk_box_pack_start(box, header, False, False, 0);

  gtk_widget_show_all(window);

  menuBar^.get_preferred_height(@MinH, @PrefH);
  FSystemMetricsList.Items[SM_CYMENU] := Max(MinH, PrefH);

  menuItem^.get_preferred_height(@MinH, @PrefH);
  FSystemMetricsList.Items[SM_CYMENUSIZE] := Max(MinH, PrefH);
  menuItem^.get_preferred_width(@MinH, @PrefH);
  FSystemMetricsList.Items[SM_CXMENUSIZE] := Max(MinH, PrefH);

  QueryIndicatorBoxSize(IndW, IndH);
  FSystemMetricsList.Items[SM_CXMENUCHECK] := IndW;
  FSystemMetricsList.Items[SM_CYMENUCHECK] := IndH;

  scrollBarH^.get_preferred_height(@MinH, @PrefH);
  FSystemMetricsList.Items[SM_CYHSCROLL] := Max(MinH, PrefH) + 2;
  FSystemMetricsList.Items[SM_CXHSCROLL] := FSystemMetricsList.Items[SM_CYHSCROLL];

  scrollBarV^.get_preferred_width(@MinH, @PrefH);
  FSystemMetricsList.Items[SM_CXVSCROLL] := Max(MinH, PrefH) + 2;
  FSystemMetricsList.Items[SM_CYVSCROLL] := FSystemMetricsList.Items[SM_CXVSCROLL];

  if OverlayScrolling then
    ApplyCustomScrollbarStyles(Max(0, Floor(PrefH / 2) - 1));

  ApplyCustomSpinButtonCSS;

  header^.get_preferred_height(@MinH, @PrefH);
  FSystemMetricsList.Items[SM_CYCAPTION] := Max(MinH, PrefH);

  gtk_widget_destroy(window);
end;

procedure TGtk3WidgetSet.AppInit(var ScreenInfo: TScreenInfo);

  procedure StripArrowBindingsFromClass(AClassType: TGType);
  var
    AClass: PGTypeClass;
    ABindingSet: PGtkBindingSet;
  begin
    AClass := g_type_class_ref(AClassType);
    if AClass = nil then
      exit;
    ABindingSet := gtk_binding_set_by_class(AClass);
    if ABindingSet = nil then
      exit;
    gtk_binding_entry_remove(ABindingSet, GDK_KEY_Up, []);
    gtk_binding_entry_remove(ABindingSet, GDK_KEY_Up, [GDK_CONTROL_MASK]);
    gtk_binding_entry_remove(ABindingSet, GDK_KEY_Down, []);
    gtk_binding_entry_remove(ABindingSet, GDK_KEY_Down, [GDK_CONTROL_MASK]);
    gtk_binding_entry_remove(ABindingSet, GDK_KEY_Left, []);
    gtk_binding_entry_remove(ABindingSet, GDK_KEY_Left, [GDK_CONTROL_MASK]);
    gtk_binding_entry_remove(ABindingSet, GDK_KEY_Right, []);
    gtk_binding_entry_remove(ABindingSet, GDK_KEY_Right, [GDK_CONTROL_MASK]);
    gtk_binding_entry_remove(ABindingSet, GDK_KEY_KP_Up, []);
    gtk_binding_entry_remove(ABindingSet, GDK_KEY_KP_Up, [GDK_CONTROL_MASK]);
    gtk_binding_entry_remove(ABindingSet, GDK_KEY_KP_Down, []);
    gtk_binding_entry_remove(ABindingSet, GDK_KEY_KP_Down, [GDK_CONTROL_MASK]);
    gtk_binding_entry_remove(ABindingSet, GDK_KEY_KP_Left, []);
    gtk_binding_entry_remove(ABindingSet, GDK_KEY_KP_Left, [GDK_CONTROL_MASK]);
    gtk_binding_entry_remove(ABindingSet, GDK_KEY_KP_Right, []);
    gtk_binding_entry_remove(ABindingSet, GDK_KEY_KP_Right, [GDK_CONTROL_MASK]);
  end;

  procedure DisableContainerArrowBindings;
  begin
    StripArrowBindingsFromClass(gtk_container_get_type);
    StripArrowBindingsFromClass(gtk_window_get_type);
    StripArrowBindingsFromClass(gtk_scrolled_window_get_type);
  end;

var
  ScreenDC: HDC;
begin
  DisableContainerArrowBindings;
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WidgetSet.AppInit');
  {$ENDIF}
  ScreenDC := GetDC(0);
  try
    ScreenInfo.PixelsPerInchX := Round(gdk_screen_get_resolution(gdk_screen_get_default));
    // GetDeviceCaps(ScreenDC, LOGPIXELSX);
    ScreenInfo.PixelsPerInchY := Round(gdk_screen_get_resolution(gdk_screen_get_default));
    // GetDeviceCaps(ScreenDC, LOGPIXELSY);
    ScreenInfo.ColorDepth := GetDeviceCaps(ScreenDC, BITSPIXEL);
  finally
    ReleaseDC(0, ScreenDC);
  end;

  LoadThemeMetrics;
  InitGtk3KeyboardTables;

  g_signal_connect_data(PGObject(gtk_settings_get_default), 'notify::gtk-theme-name',
    TGCallback(@Gtk3ThemeNameNotify), nil, nil, G_CONNECT_DEFAULT);
end;

procedure TGtk3WidgetSet.LoadCSSTheme;
var
  ACSSProvider: PGtkCssProvider;
  ASettings: PGtkSettings;
  AValue: TGValue;
  EnvTheme: string;
begin
  ACSSProvider := nil;
  EnvTheme := GetEnvironmentVariable('GTK_THEME');

  if EnvTheme <> '' then
  begin
    FThemeName := EnvTheme;
    ACSSProvider := gtk_css_provider_get_named(PGChar(FThemeName), nil);
  end;

  if not Assigned(ACSSProvider) then
  begin
    ASettings := gtk_settings_get_default;
    FillByte(AValue{%H-}, SizeOf(AValue), 0);
    AValue.init(G_TYPE_STRING);
    g_object_get_property(PGObject(ASettings), 'gtk-theme-name', @AValue);
    FThemeName := AValue.get_string;
    AValue.unset;
    //TODO: gtk-entry-select-on-focus is TRUE by Default, should be false, so LCL only is doing selections.
    //gtk-label-select-on-focus ? default is true too

    ACSSProvider := gtk_css_provider_get_named(PGChar(FThemeName), nil);
  end;

  if not Assigned(ACSSProvider) then
  begin
    DebugLn('WARNING: Could not load any GTK3 theme CSS.');
    exit;
  end;

  FCSSTheme.Text := gtk_css_provider_to_string(ACSSProvider);
end;

procedure TGtk3WidgetSet.ClearCSSTheme;
begin
  FCSSTheme.Clear;
end;

function TGtk3WidgetSet.GetCSSTheme(AList: TStrings): boolean;
begin
  Result := FCSSTheme.Count > 0;
  if not Result then
    LoadCSSTheme;
  AList.Assign(FCSSTheme);
end;

function TGtk3WidgetSet.GetThemeName: string;
begin
  if FThemeName = '' then
    LoadCSSTheme;
  Result := FThemeName;
end;

procedure TGtk3WidgetSet.AppRun(const ALoop: TApplicationMainLoop);
begin
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WidgetSet.AppRun');
  {$ENDIF}
  if Assigned(ALoop) then
    ALoop;
end;

procedure TGtk3WidgetSet.AppWaitMessage;
begin
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WidgetSet.AppWaitMessage');
  {$ENDIF}
  if not g_main_context_pending(nil) then
    g_main_context_iteration(nil, True);
end;

procedure TGtk3WidgetSet.AppProcessMessages;
begin
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WidgetSet.AppProcessMessages');
  {$ENDIF}
  while g_main_context_pending(nil) do
  begin
    g_main_context_iteration(nil, False);
    if TApplicationAccess(Application).HasAsyncCallQueued then
      Break;
  end;
  {$IFDEF GTK3USEDEFERREDRESIZING}
  Gtk3DrainResizeQueue;
  {$ENDIF GTK3USEDEFERREDRESIZING}
  if IsMultiThread and (GetCurrentThreadId = MainThreadID) then
    CheckSynchronize; //ref.to issue #27662
end;

procedure TGtk3WidgetSet.AppTerminate;
var
  AList: PGList;
begin
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WidgetSet.AppTerminate ',dbgs(gtk_main_level));
  {$ENDIF}
  // g_main_context_release(g_main_context_default);
  if Assigned(FGtk3Application) then
  begin
    FGtk3Application^.quit;
    {$IFDEF GTK3DEBUGCORE}
    AList := FGtk3Application^.get_windows;
    if Assigned(AList) then
    begin
      DebugLn('TGtk3WidgetSet.Gtk3Destroy app Windows list ',dbgs(g_list_length(AList)));
      g_list_free(AList);
    end else
      DebugLn('TGtk3WidgetSet.Gtk3Destroy app Windows list is null ');
    {$ENDIF}
    FGtk3Application^.unref;
    FGtk3Application := nil;
  end;
  if gtk_main_level > 0 then
    gtk_main_quit;
end;


procedure TGtk3WidgetSet.AppMinimize;
var
  i: Integer;
  AForm: TCustomForm;
  W: TGtk3Widget;
begin
  if Screen=nil then exit;
  for i:= 0 to Screen.CustomFormZOrderCount-1 do
  begin
    AForm := Screen.CustomFormsZOrdered[i];
    if (AForm.Parent=nil) and AForm.HandleAllocated and
      (TGtk3Widget(AForm.Handle).Visible) and
      not (AForm.FormStyle in [fsMDIChild, fsSplash]) and
      not (AForm.BorderStyle in [bsNone]) then
      begin
        W := TGtk3Widget(AForm.Handle);
        PGtkWindow(W.Widget)^.iconify;
      end;
  end;
end;


procedure TGtk3WidgetSet.AppRestore;
var
  i: Integer;
  AForm: TCustomForm;
begin
  if Screen=nil then exit;
  for i:= Screen.CustomFormZOrderCount-1 downto 0 do
  begin
    AForm:=Screen.CustomFormsZOrdered[i];
    if (AForm.Parent=nil) and AForm.HandleAllocated and
      (TGtk3Widget(AForm.Handle).Visible) and
      not (AForm.FormStyle in [fsMDIChild, fsSplash]) and
      not (AForm.BorderStyle in [bsNone]) then
        PGtkWindow(TGtk3Window(AForm.Handle).Widget)^.deiconify;
  end;
end;

procedure TGtk3WidgetSet.AppBringToFront;
var
  ATime: guint32;
begin
  if Assigned(Application.MainForm) and Application.MainForm.HandleAllocated then
  begin
    TGtk3Window(Application.MainForm.Handle).raiseWidget;
    ATime := gtk_get_current_event_time;
    if ATime = 0 then
      ATime := LastUserEventTime;
    if ATime <> 0 then
      gdk_window_focus(TGtk3Window(Application.MainForm.Handle).Widget^.window, ATime);
  end;
end;
procedure TGtk3WidgetSet.AppSetIcon(const Small, Big: HICON);
var
  DoDestroyIcon: Boolean;
  AIcon: PGdkPixbuf;
begin
  // DebugLn('TGtk3WidgetSet.AppSetIcon Small=',dbgHex(Small),' Big=',dbgHex(Big));
  DoDestroyIcon := Big = 0;
  if DoDestroyIcon then
  begin
    if Assigned(FAppIcon) then
      FAppIcon^.unref;
    FAppIcon := nil;
  end else
  begin
    AIcon := TGtk3Image(Big).Handle;
    FAppIcon := PGdkPixbuf(AIcon)^.copy;
  end;
end;

procedure TGtk3WidgetSet.AppSetTitle(const ATitle: string);
begin
  if Assigned(Application.MainForm) and (Application.MainForm.HandleAllocated) then
  begin
    TGtk3Window(Application.MainForm.Handle).Title := ATitle;
  end;
end;

type
  TGtk3TempFormStyleSet = Set of TFormStyle;
const
  TGtk3TopForms: Array[Boolean] of TGtk3TempFormStyleSet = (fsAllNonSystemStayOnTop,
    fsAllStayOnTop);

function TGtk3WidgetSet.AppRemoveStayOnTopFlags(const ASystemTopAlso: Boolean
  ): Boolean;
var
  i: Integer;
  AForm: TCustomForm;
  W: PGtkWidget;
  GdkWin: PGdkWindow;
  act: PGdkWindow;
  ActTopLvlWnd: PGdkWindow;
  FoundAct: Boolean;
  x, y, width, height: gint;
begin
  Result := True;
  if StayOnTopList = nil then
    StayOnTopList := TMap.Create(TMapIdType(ituPtrSize), SizeOf(Pointer));

  act := gdk_screen_get_active_window_raw(gdk_screen_get_default);
  try
    ActTopLvlWnd := act;
    if ActTopLvlWnd <> nil then
      ActTopLvlWnd := gdk_window_get_toplevel(ActTopLvlWnd);

    FoundAct := False;
    for i := 0 to Screen.CustomFormCount - 1 do
    begin
      AForm := Screen.CustomFormsZOrdered[i];
      if AForm.HandleAllocated and (AForm.Parent = nil) then
      begin
        W := TGtk3Window(AForm.Handle).Widget;
        if W^.get_window = ActTopLvlWnd then
          FoundAct := True;
      end;
    end;

    if (ActTopLvlWnd <> nil) and not FoundAct then
    begin
      gdk_window_get_geometry(ActTopLvlWnd, @x, @y, @width, @height);
      if (width <= 0) or (height <= 0) or (abs(x) > 100000) or (y > 100000) then
      begin
        DebugLn(['Hint: [TGtk3WidgetSet.AppRemoveStayOnTopFlags] ignoring active window: x=', x, ' y=', y, ' ', width, 'x', height]);
        ActTopLvlWnd := nil;
      end;
    end;

    for i := 0 to Screen.CustomFormZOrderCount - 1 do
    begin
      AForm := Screen.CustomFormsZOrdered[i];
      if AForm.HandleAllocated and (AForm.Parent = nil) then
      begin
        W := TGtk3Window(AForm.Handle).Widget;
        GdkWin := W^.get_window;

        if (GdkWin = nil) or gdk_window_is_destroyed(GdkWin) then
          continue;

        if not (csDesigning in AForm.ComponentState) and
          (AForm.FormStyle in TGtk3TopForms[ASystemTopAlso]) and
          W^.get_visible and
          not gtk_window_get_modal(PGtkWindow(W)) and
          not (GDK_WINDOW_STATE_ICONIFIED in gdk_window_get_state(GdkWin)) then
        begin
          gdk_window_set_keep_above(GdkWin, False);
          if Assigned(ActTopLvlWnd) then
          begin
            gdk_window_restack(GdkWin, ActTopLvlWnd, False);
            ActTopLvlWnd := GdkWin;
          end;
          if not StayOnTopList.HasId(W) then
            StayOnTopList.Add(W, W);
        end;
      end;
    end;
  finally
    if Assigned(act) then
      g_object_unref(PGObject(act));
  end;
end;

function TGtk3WidgetSet.AppRestoreStayOnTopFlags(const ASystemTopAlso: Boolean
  ): Boolean;
var
  i: Integer;
  AForm: TCustomForm;
  W: PGtkWidget;
  GdkWin: PGdkWindow;
begin
  Result := True;
  if StayOnTopList = nil then
    exit;
  for i := Screen.CustomFormZOrderCount - 1 downto 0 do
  begin
    AForm := Screen.CustomFormsZOrdered[i];
    if AForm.HandleAllocated and (AForm.Parent = nil) then
    begin
      W := TGtk3Window(AForm.Handle).Widget;
      GdkWin := W^.get_window;

      if (GdkWin = nil) or gdk_window_is_destroyed(GdkWin) then
        continue;

      if not (csDesigning in AForm.ComponentState) and
        (AForm.FormStyle in TGtk3TopForms[ASystemTopAlso]) and
        W^.get_visible and
        not gtk_window_get_modal(PGtkWindow(W)) and
        not (GDK_WINDOW_STATE_ICONIFIED in gdk_window_get_state(GdkWin)) then
      begin
        if StayOnTopList.HasId(W) then
          gdk_window_set_keep_above(GdkWin, True);
      end;
    end;
  end;
  FreeAndNil(StayOnTopList);
end;

function TGtk3WidgetSet.CreateStandardCursor(ACursor: SmallInt): HCURSOR;
var
  CursorValue: TGdkCursorType;
  CursorName: string;
  NamedHandle: PGdkCursor;
begin
  Result := 0;
  if ACursor < crLow then Exit;
  if ACursor > crHigh then Exit;

  //Try CSS/theme named cursors first for types not in the GDK legacy enum.
  CursorName := '';
  case TCursor(ACursor) of
    crNo:        CursorName := 'not-allowed';
    crNoDrop:    CursorName := 'no-drop';
    //crDrag:      CursorName := 'move'; we use LCL cursor to be compat with other ws.
    crMultiDrag: CursorName := 'copy';
    crSQLWait:   CursorName := 'wait';
    crAppStart:  CursorName := 'progress';
    crHourGlass: CursorName := 'wait';
    crHSplit:    CursorName := 'col-resize';
    crVSplit:    CursorName := 'row-resize';
    crSizeAll:   CursorName := 'all-scroll';
  end;
  if CursorName <> '' then
  begin
    NamedHandle := gdk_cursor_new_from_name(gdk_display_get_default, PChar(CursorName));
    if NamedHandle <> nil then
    begin
      Result := HCURSOR(TGtk3Cursor.Create(NamedHandle));
      exit;
    end;
  end;

  CursorValue := GDK_CURSOR_IS_PIXMAP;
  case TCursor(ACursor) of
    crDefault:  CursorValue := GDK_LEFT_PTR;
    crArrow:    CursorValue := GDK_Arrow;
    crCross:    CursorValue := GDK_Cross;
    crIBeam:    CursorValue := GDK_XTerm;
    crSizeNESW: CursorValue := GDK_BOTTOM_LEFT_CORNER;
    crSizeNS:   CursorValue := GDK_SB_V_DOUBLE_ARROW;
    crSizeNWSE: CursorValue := GDK_TOP_LEFT_CORNER;
    crSizeWE:   CursorValue := GDK_SB_H_DOUBLE_ARROW;
    crSizeNW:   CursorValue := GDK_TOP_LEFT_CORNER;
    crSizeN:    CursorValue := GDK_TOP_SIDE;
    crSizeNE:   CursorValue := GDK_TOP_RIGHT_CORNER;
    crSizeW:    CursorValue := GDK_LEFT_SIDE;
    crSizeE:    CursorValue := GDK_RIGHT_SIDE;
    crSizeSW:   CursorValue := GDK_BOTTOM_LEFT_CORNER;
    crSizeS:    CursorValue := GDK_BOTTOM_SIDE;
    crSizeSE:   CursorValue := GDK_BOTTOM_RIGHT_CORNER;
    crUpArrow:  CursorValue := GDK_LEFT_PTR;
    crHourGlass:CursorValue := GDK_WATCH; // fallback if 'wait' not in theme
    crHSplit:   CursorValue := GDK_SB_H_DOUBLE_ARROW; // fallback if 'col-resize' not in theme
    crVSplit:   CursorValue := GDK_SB_V_DOUBLE_ARROW; // fallback if 'row-resize' not in theme
    crAppStart: CursorValue := GDK_WATCH; // fallback if 'progress' not in theme
    crHelp:     CursorValue := GDK_QUESTION_ARROW;
    crHandPoint:CursorValue := GDK_Hand2;
    crSizeAll:  CursorValue := GDK_FLEUR; // fallback if 'all-scroll' not in theme
  end;
  if CursorValue <> GDK_CURSOR_IS_PIXMAP then
    Result := HCURSOR(TGtk3Cursor.Create(CursorValue));
end;

function TGtk3WidgetSet.DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor;
begin
  if IsValidDC(CanvasHandle) then
    Result := TGtk3DeviceContext(CanvasHandle).getPixel(X, Y)
  else
    Result := 0;
end;

procedure TGtk3WidgetSet.DCSetPixel(CanvasHandle: HDC; X, Y: integer;
  AColor: TGraphicsColor);
begin
  if IsValidDC(CanvasHandle) then
    TGtk3DeviceContext(CanvasHandle).drawPixel(X, Y, AColor);
end;

procedure TGtk3WidgetSet.DCRedraw(CanvasHandle: HDC);
begin
  {$IFDEF GTK3DEBUGNOTIMPLEMENTED}
  DebugLn('WARNING: TGtk3WidgetSet.DCRedraw not implemented ...');
  {$ENDIF}
end;

procedure TGtk3WidgetSet.DCSetAntialiasing(CanvasHandle: HDC; AEnabled: Boolean
  );
begin
  // inherited DCSetAntialiasing(CanvasHandle, AEnabled);
  if IsValidDC(CanvasHandle) then
  begin
    TGtk3DeviceContext(CanvasHandle).set_antialiasing(AEnabled);
  end;
end;

procedure TGtk3WidgetSet.SetDCOpacity(DC: HDC; AOpacity: Byte);
begin
  if IsValidDC(DC) then
    TGtk3DeviceContext(DC).Opacity := AOpacity;
end;

function TGtk3WidgetSet.GetDCOpacity(DC: HDC): Byte;
begin
  if IsValidDC(DC) then
    Result := TGtk3DeviceContext(DC).Opacity
  else
    Result := $FF;
end;

procedure TGtk3WidgetSet.SetDesigning(AComponent: TComponent);
begin
  // inherited SetDesigning(AComponent);
end;

function gtk3TimerProc(Data: gPointer): gBoolean; cdecl;
var
  TimerInfo: PGtkITimerinfo;
begin
  Result := False;

  TimerInfo := PGtkITimerinfo(Data);

  if (FTimerData = nil) or (FTimerData.IndexOf(Data)<0) then
  begin
    Result := False // timer was killed
  end else
  begin
    if TimerInfo^.TimerFunc <> nil then
    begin
      TimerInfo^.TimerFunc;
      Result := True; // timer will go on
    end else
    begin
      Result := False; // stop timer
    end;
  end;

  if Result and (FTimerData.IndexOf(Data)<0) then
  begin
    // timer was killed
    // -> stop timer
    Result := False;
  end;
end;

procedure gtk3TimerDestroyed(Data: gPointer); cdecl;
var
  TimerInfo: PGtkITimerinfo;
begin
  if (FTimerData <> nil) and Assigned(Data) and
    (FTimerData.IndexOf(Data) >= 0) then
  begin
    TimerInfo := PGtkITimerinfo(Data);
    FTimerData.Remove(Data);
    Dispose(TimerInfo);
  end;
end;

function TGtk3WidgetSet.CreateTimer(Interval: integer; TimerFunc: TWSTimerProc
  ): TLCLHandle;
var
  TimerInfo: PGtkITimerinfo;
begin
  if not Assigned(TimerFunc)
  then
    Result := 0
  else begin
    if Interval < 1 then
      Interval := 1;
    New(TimerInfo);
    FillByte(TimerInfo^,SizeOf(TGtkITimerinfo),0);
    TimerInfo^.TimerFunc := TimerFunc;
    {$IFDEF VerboseTimer}
    DebugLn(['TGtk3WidgetSet.CreateTimer Interval=',dbgs(Interval)]);
    {$ENDIF}
    //Result:= g_timeout_add_full(0 {G_PRIORITY_DEFAULT}, Interval, @gtk3TimerProc, TimerInfo, @gtk3TimerDestroyed);
    Result:=g_timeout_add(Interval, @gtk3TimerProc, TimerInfo);
    if Result = 0 then
      Dispose(TimerInfo)
    else begin
      TimerInfo^.TimerFunc := TimerFunc;
      TimerInfo^.TimerHandle:=Result;
      FTimerData.Add(TimerInfo);
    end;
  end;
end;

function TGtk3WidgetSet.DestroyTimer(TimerHandle: TLCLHandle): boolean;
var
  n: integer;
  TimerInfo: PGtkITimerinfo;
begin
  Result := False;
  n := FTimerData.Count;
  while (n > 0) do
  begin
    dec (n);
    TimerInfo := PGtkITimerinfo(FTimerData.Items[n]);
    if (TimerInfo^.TimerHandle = guint(TimerHandle)) then
    begin
      // in gtk3 timeout is automatically removed
      // g_timeout_remove(TimerInfo^.TimerHandle);
      g_source_remove(TimerInfo^.TimerHandle);
      TimerInfo^.TimerHandle := 0;
      FTimerData.Delete(n);
      Dispose(TimerInfo);
    end;
  end;
  Result := True;
end;

function TGtk3WidgetSet.AllocateHWnd(Method: TLCLWndMethod): HWND;
begin
  Result := HWND(TGtk3ToolWnd.Create(Method));
end;

procedure TGtk3WidgetSet.DeallocateHWnd(Wnd: HWND);
begin
  TGtk3ToolWnd(Wnd).Free;
end;

function TGtk3WidgetSet.SystemMetric(const AIndex:integer):integer;
begin
  Result := 0;
  if (AIndex >= 0) and (AIndex < FSystemMetricsList.Count) then
    Result := FSystemMetricsList[AIndex];
end;

function TGtk3WidgetSet.IsValidDC(const DC: HDC): Boolean;
begin
  Result := DC <> 0;
  if Result then
    Result := TGtk3DeviceContext(DC).pcr <> nil;
end;

function TGtk3WidgetSet.IsValidGDIObject(const AGdiObject: HGDIOBJ): Boolean;
begin
  Result := (AGdiObject <> 0) and (TObject(AGdiObject) is TGtk3ContextObject);
end;

function TGtk3WidgetSet.IsValidHandle(const AHandle: HWND): Boolean;
begin
  Result := AHandle <> 0;
end;

procedure TGtk3WidgetSet.InitStockItems;
var
  LogBrush: TLogBrush;
  logPen : TLogPen;
begin
  FillChar(LogBrush{%H-},SizeOf(TLogBrush),0);
  LogBrush.lbStyle := BS_NULL;
  FStockNullBrush := CreateBrushIndirect(LogBrush);
  TGtk3Brush(FStockNullBrush).Shared := True;

  LogBrush.lbStyle := BS_SOLID;
  LogBrush.lbColor := $000000;
  FStockBlackBrush := CreateBrushIndirect(LogBrush);
  TGtk3Brush(FStockBlackBrush).Shared := True;

  LogBrush.lbColor := $C0C0C0;
  FStockLtGrayBrush := CreateBrushIndirect(LogBrush);
  TGtk3Brush(FStockLtGrayBrush).Shared := True;

  LogBrush.lbColor := $808080;
  FStockGrayBrush := CreateBrushIndirect(LogBrush);
  TGtk3Brush(FStockGrayBrush).Shared := True;

  LogBrush.lbColor := $404040;
  FStockDkGrayBrush := CreateBrushIndirect(LogBrush);
  TGtk3Brush(FStockDkGrayBrush).Shared := True;

  LogBrush.lbColor := $FFFFFF;
  FStockWhiteBrush := CreateBrushIndirect(LogBrush);
  TGtk3Brush(FStockWhiteBrush).Shared := True;

  LogPen.lopnStyle := PS_NULL;
  LogPen.lopnWidth := Point(0, 0); // create cosmetic pens
  LogPen.lopnColor := $FFFFFF;
  FStockNullPen := CreatePenIndirect(LogPen);
  TGtk3Pen(FStockNullPen).Shared := True;

  LogPen.lopnStyle := PS_SOLID;
  FStockWhitePen := CreatePenIndirect(LogPen);
  TGtk3Pen(FStockWhitePen).Shared := True;

  LogPen.lopnColor := $000000;
  FStockBlackPen := CreatePenIndirect(LogPen);
  TGtk3Pen(FStockBlackPen).Shared := True;

  FStockSystemFont := 0; // styles aren't initialized yet

  FStockDefaultDC := 0; // app must be initialized
end;

procedure TGtk3WidgetSet.FreeStockItems;
  procedure DeleteAndNilObject(var h: HGDIOBJ);
  begin
    if h <> 0 then
      TGtk3ContextObject(h).Shared := False;
    DeleteObject(h);
    h := 0;
  end;
begin
  DeleteAndNilObject(FStockNullBrush);
  DeleteAndNilObject(FStockBlackBrush);
  DeleteAndNilObject(FStockLtGrayBrush);
  DeleteAndNilObject(FStockGrayBrush);
  DeleteAndNilObject(FStockDkGrayBrush);
  DeleteAndNilObject(FStockWhiteBrush);

  DeleteAndNilObject(FStockNullPen);
  DeleteAndNilObject(FStockBlackPen);
  DeleteAndNilObject(FStockWhitePen);

  DeleteAndNilObject(FStockSystemFont);
end;

function TGtk3WidgetSet.CreateDefaultFont: HFONT;
var
  AFont: TGtk3Font;
  cr: Pcairo_t;
begin
  Result := 0;
  cr := gdk_cairo_create(gdk_get_default_root_window);
  AFont := TGtk3Font.Create(cr, nil);
  cairo_destroy(cr);
  Result := HFONT(AFont);
end;

function TGtk3WidgetSet.DragImageList_BeginDrag(APixBuf: PGdkPixbuf;
  AHotSpot: TPoint): Boolean;
var
  w, h: gint;
begin
  if FDragImageList = nil then
  begin
    FDragImageList := gtk_window_new(GTK_WINDOW_POPUP);
    W := APixBuf^.get_width;
    H := APixBuf^.get_height;
    gtk_window_set_default_size(PGtkWindow(FDragImageList), w, h);
    gtk_widget_realize(FDragImageList);
    gdk_window_set_decorations(FDragImageList^.window, []);
    gdk_window_set_functions(FDragImageList^.window, [GDK_FUNC_RESIZE,GDK_FUNC_CLOSE]);

    FDragImageListIcon := gtk_image_new_from_pixbuf(APixBuf);
    PGtkWindow(FDragImageList)^.add(FDragImageListIcon);
    FDragImageListIcon^.show;
    gtk_widget_show(FDragImageListIcon);
    // make window transparent outside mask
    //gdk_window_shape_combine_region();
    //gdk_window_shape_combine_mask(FDragImageList^.window, APixBuf, 0, 0);
    FDragHotSpot := AHotSpot;
  end;
  Result := FDragImageList <> nil;
end;

procedure TGtk3WidgetSet.DragImageList_EndDrag;
begin
  if FDragImageList <> nil then
  begin
    if FDragImageListIcon <> nil then
      gtk_widget_destroy(FDragImageListIcon);
    gtk_widget_destroy(FDragImageList);
    FDragImageList := nil;
    FDragImageListIcon := nil;
  end;
end;

function TGtk3WidgetSet.DragImageList_DragMove(X, Y: Integer): Boolean;
begin
  Result := FDragImageList <> nil;
  if Result then
  begin
    if Gtk3IsGdkWindow(FDragImageList^.Window) then
    begin
      if FDragImageList^.Window^.is_visible then
        gdk_window_raise(FDragImageList^.Window);
      PGtkWindow(FDragImageList)^.move(X - FDragHotSpot.X, Y - FDragHotSpot.Y);
    end;
  end;
end;

function TGtk3WidgetSet.DragImageList_SetVisible(NewVisible: Boolean): Boolean;
begin
  Result := FDragImageList <> nil;
  if Result then
  begin
    if NewVisible then
      FDragImageList^.show_all
    else
      FDragImageList^.hide;
  end;
end;


function TGtk3WidgetSet.CreateThemeServices: TThemeServices;
begin
  Result := TGTK3ThemeServices.Create;
end;

