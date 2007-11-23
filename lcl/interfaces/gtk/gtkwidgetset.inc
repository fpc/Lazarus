{%MainUnit gtkint.pp}

{******************************************************************************
                                   TGtkWidgetSet
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

{------------------------------------------------------------------------------
  Procedure: GLogFunc

  Replaces the default glib loghandler. All errors, warnings etc, are logged
  through this function.
  Here are Fatals, Criticals and Errors translated to Exceptions
  Comment Ex to skip exception, comment Level to skip logging
 ------------------------------------------------------------------------------}
procedure GLogFunc(ALogDomain: Pgchar; ALogLevel: TGLogLevelFlags;
  AMessage: Pgchar; AData: gpointer);cdecl;
var
  Flag, Level, Domain: String;
  Ex: ExceptClass;
begin
(*
    G_LOG_FLAG_RECURSION = 1 shl 0;
    G_LOG_FLAG_FATAL = 1 shl 1;
    G_LOG_LEVEL_ERROR = 1 shl 2;
    G_LOG_LEVEL_CRITICAL = 1 shl 3;
    G_LOG_LEVEL_WARNING = 1 shl 4;
    G_LOG_LEVEL_MESSAGE = 1 shl 5;
    G_LOG_LEVEL_INFO = 1 shl 6;
    G_LOG_LEVEL_DEBUG = 1 shl 7;
    G_LOG_LEVEL_MASK = (1 shl 8) - 2;
*)
  if (AData=nil) then ;

  Ex := nil;
  Level := '';
  Flag := '';

  if ALogDomain = nil
  then Domain := ''
  else Domain := ALogDomain + ': ';

  if ALogLevel and G_LOG_FLAG_RECURSION <> 0
  then Flag := '[RECURSION] ';

  if ALogLevel and G_LOG_FLAG_FATAL <> 0
  then Flag := Flag + '[FATAL] ';

  if ALogLevel and G_LOG_LEVEL_ERROR <> 0
  then begin
    Level := 'ERROR';
    Ex := EInterfaceError;
  end
  else
  if ALogLevel and G_LOG_LEVEL_CRITICAL <> 0
  then begin
    Level := 'CRITICAL';
    Ex := EInterfaceCritical;
  end
  else
  if ALogLevel and G_LOG_LEVEL_WARNING <> 0
  then begin
    Level := 'WARNING';
    Ex := EInterfaceWarning;
  end
  else
  if ALogLevel and G_LOG_LEVEL_INFO <> 0
  then begin
    Level := 'INFO';
  end
  else
  if ALogLevel and G_LOG_LEVEL_DEBUG <> 0
  then begin
    Level := 'DEBUG';
  end
  else begin
    Level := 'USER';
  end;

  if Ex = nil
  then begin
    if Level <> ''
    then DebugLn('[', Level, '] ', Flag, Domain, AMessage);
  end
  else begin
    if ALogLevel and G_LOG_FLAG_FATAL <> 0
    then begin
      // always create exception
      //
      // see callstack for more info
      raise Ex.Create(Flag + Domain + AMessage);
    end
    else begin
      // create a debugger trappable exception
      // but for now let the app continue and log a line
      // in future when all warnings etc. are gone they might raise
      // a real exception
      //
      // see callstack for more info
      try
        raise Ex.Create(Flag + Domain + AMessage);
      except
        on Exception do begin
          // just write a line
          DebugLn('[', Level, '] ', Flag, Domain, AMessage);
        end;
      end;
    end;
  end;

end;

{$ifdef Unix}

// TThread.Synchronize support
var
  threadsync_pipein, threadsync_pipeout: cint;
  threadsync_giochannel: pgiochannel;
  childsig_pending: boolean;

{$if defined(ver2_0) and defined(BSD)}
procedure ChildEventHandler(sig: longint; var siginfo: tsiginfo_t;
  var sigcontext: sigcontextrec); cdecl;
{$else}
procedure ChildEventHandler(sig: longint; siginfo: psiginfo;
  sigcontext: psigcontext); cdecl;
{$endif}  
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

{$endif}

{------------------------------------------------------------------------------
  Method: TGtkWidgetSet.Create
  Params:  None
  Returns: Nothing

  Contructor for the class.
 ------------------------------------------------------------------------------}
constructor TGtkWidgetSet.Create;
begin
  if ClassType = TGtkWidgetSet
  then raise EInvalidOperation.Create('Cannot create the base gtkwidgetset, use gtk1 or gtk2 instead');

  inherited Create;

  // DCs, GDIObjects
  FDeviceContexts := TDynHashArray.Create(-1);
  FDeviceContexts.Options:=FDeviceContexts.Options+[dhaoCacheContains];
  FGDIObjects := TDynHashArray.Create(-1);
  FGDIObjects.Options:=FGDIObjects.Options+[dhaoCacheContains];

  {$Ifdef GTK2}
  FDefaultFontDesc:= nil;
  {$Else}
  FDefaultFont:= nil;
  {$EndIf}
  // messages
  FMessageQueue := TGtkMessageQueue.Create;
  WaitingForMessages := false;
  FWidgetsWithResizeRequest := TDynHashArray.Create(-1);
  FWidgetsWithResizeRequest.Options:=
    FWidgetsWithResizeRequest.Options+[dhaoCacheContains];
  FWidgetsResized := TDynHashArray.Create(-1);
  FWidgetsResized.Options:=FWidgetsResized.Options+[dhaoCacheContains];
  FFixWidgetsResized := TDynHashArray.Create(-1);

  FTimerData  := TFPList.Create;
  {$IFDEF Use_KeyStateList}
  FKeyStateList_ := TFPList.Create;
  {$ENDIF}

  DestroyConnectedWidgetCB:=@DestroyConnectedWidget;

  FRCFilename := ChangeFileExt(ParamStr(0),'.gtkrc');
  FRCFileParsed := false;

  // initialize app level gtk engine
  gtk_set_locale ();

  // call init and pass cmd line args
  PassCmdLineOptions;

  // set glib log handler
  FLogHandlerID := g_log_set_handler(nil, -1, @GLogFunc, Self);

  // read gtk rc file
  ParseRCFile;

  // Initialize Stringlist for holding styles
  Styles := TStringlist.Create;

  {$IFDEF Use_KeyStateList}
  gtk_key_snooper_install(@GTKKeySnooper, FKeyStateList_);
  {$ELSE}
  gtk_key_snooper_install(@GTKKeySnooper, nil);
  {$ENDIF}

  // Init tooltips
  FGTKToolTips := gtk_tooltips_new;
  //gtk_object_ref(PGTKObject(FGTKToolTips));
  gtk_toolTips_Enable(FGTKToolTips);

  // Init stock objects;
  InitStockItems;

  // clipboard
  ClipboardTypeAtoms[ctPrimarySelection]:=GDK_SELECTION_PRIMARY;
  ClipboardTypeAtoms[ctSecondarySelection]:=GDK_SELECTION_SECONDARY;
  ClipboardTypeAtoms[ctClipboard]:=gdk_atom_intern('CLIPBOARD',GdkFalse);

{$ifdef Unix}
  InitSynchronizeSupport;
{$ifdef UseAsyncProcess}  
  DebugLn(['TGtkWidgetSet.Create Installing signal handler for TAsyncProcess']);
  InstallSignalHandler;
{$endif}
{$endif}

  GTKWidgetSet := Self;
end;

{------------------------------------------------------------------------------
  Method: TGtkWidgetSet.PassCmdLineOptions
  Params:  None
  Returns: Nothing

  Passes command line options to the gtk engine
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.PassCmdLineOptions;

  function SearchOption(const Option: string; Remove: boolean): boolean;
  var
    i: Integer;
    ArgCount: LongInt;
  begin
    Result:=false;
    if Option='' then exit;
    i:=0;
    ArgCount:=argc;
    while i<ArgCount do begin
      if AnsiStrComp(PChar(Option),argv[i])=0 then begin
        // option exists
        Result:=true;
        if Remove then begin
          // remove option from parameters, so that no other parameter parsed
          // can see it.
          dec(ArgCount);
          while i<ArgCount do begin
            argv[i]:=argv[i+1];
            inc(i);
          end;
          argv[i]:=nil;
        end;
        exit;
      end;
      inc(i);
    end;
  end;

begin
  gtk_init(@argc,@argv);
  UseTransientForModalWindows:=not SearchOption('--lcl-no-transient',true);
end;

{------------------------------------------------------------------------------
  procedure TGtkWidgetSet.FreeAllStyles;
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.FreeAllStyles;
begin
  If Assigned(Styles) then begin
    ReleaseAllStyles;
    Styles.Free;
    Styles:=nil;
  end;
end;

{$ifdef TraceGdiCalls}
procedure DumpBackTrace(BackTrace: TCallBacksArray);
var
  func,source: shortString;
  line: longint;
  i: integer;
begin
  for i:=0 to MaxCallBacks do begin
    LineInfo.GetLineInfo(longWord(BackTrace[i]), Func, source, line);
    DebugLn('$', Hexstr(LongInt(BackTrace[i]),8),'  ', Func, ',  line ',
      dbgs(line),' of ',Source);
  end;
end;

procedure FillStackAddrs(bp: pointer; BackTraces: PCallBacksArray);
var
  prevbp: pointer;
  caller_frame,
  caller_addr : Pointer;
  i: Integer;
begin
  Prevbp := bp-1;
  i:=0;
  while (bp>prevbp)do begin
    caller_addr := get_caller_addr(bp);
    caller_frame := get_caller_frame(bp);
    BackTraces^[i] := Caller_Addr;
    inc(i);
   if (caller_addr=nil) or
      (caller_frame=nil) or
      (i>MaxCallBacks) then
     break;
   prevbp:=bp;
   bp:=caller_frame;
  end;
end;
{$endif}
{------------------------------------------------------------------------------
  Method: TGtkWidgetSet.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TGtkWidgetSet.Destroy;
const
  ProcName = '[TGtkWidgetSet.Destroy]';
var
  n: Integer;
  pTimerInfo  : PGtkITimerinfo;
  GDITypeCount: array[TGDIType] of Integer;
  GDIType: TGDIType;
  HashItem: PDynHashArrayItem;
  QueueItem  : TGtkMessageQueueItem;
  NextQueueItem : TGtkMessageQueueItem;
begin
  ReAllocMem(FExtUTF8OutCache,0);
  FExtUTF8OutCacheSize:=0;

  FreeAllStyles;
  FreeStockItems;

  if FGTKToolTips<>nil then begin
    {$IFDEF Gtk2}
    gtk_object_sink(PGTKObject(FGTKToolTips));
    {$ELSE}
    gtk_object_unref(PGTKObject(FGTKToolTips));
    {$ENDIF}
    FGTKToolTips := nil;
  end;

  // tidy up the paint messages
  QueueItem:=FMessageQueue.FirstMessageItem;
  while (QueueItem<>nil) do begin
    NextQueueItem := TGtkMessageQueueItem(QueueItem.Next);
    if QueueItem.IsPaintMessage then
      fMessageQueue.RemoveMessage(QueueItem,FPMF_All,true);
    QueueItem := NextQueueItem;
  end;

  // warn about unremoved paint messages
  if fMessageQueue.HasPaintMessages then begin
    DebugLn(ProcName, Format(rsWarningUnremovedPaintMessages,
      [IntToStr(fMessageQueue.NumberOfPaintMessages)]));
  end;

  // warn about unreleased DC
  if (FDeviceContexts.Count > 0)
  then begin
    DebugLn(ProcName, Format(rsWarningUnreleasedDCsDump,
      [FDeviceContexts.Count]));

    n:=0;
    DbgOut(ProcName,'  DCs:  ');
    HashItem:=FDeviceContexts.FirstHashItem;
    while (n<7) and (HashItem<>nil) do
    begin
      DbgOut(' ',DbgS(HashItem^.Item));
      HashItem:=HashItem^.Next;
      inc(n);
    end;
    DebugLn();
  end;

  // warn about unreleased gdi objects
  if (FGDIObjects.Count > 0)
  then begin
    DebugLn(ProcName,Format(rsWarningUnreleasedGDIObjectsDump,
       [FGDIObjects.Count]));
    for GDIType := Low(TGDIType) to High(TGDIType) do
      GDITypeCount[GDIType] := 0;

    n:=0;
    {$ifndef TraceGdiCalls}
    DbgOut(ProcName,'   GDIOs:');
    {$endif}
    HashItem := FGDIObjects.FirstHashItem;
    while (HashItem <> nil) do
    begin
      {$ifndef TraceGdiCalls}
      if n < 7
      then
        DbgOut(' ',DbgS(HashItem^.Item));
      {$endif}

      Inc(GDITypeCount[PGdiObject(HashItem^.Item)^.GDIType]);
      HashItem := HashItem^.Next;
      Inc(n);
    end;
    {$ifndef TraceGdiCalls}
    DebugLn();
    {$endif}

    for GDIType := Low(GDIType) to High(GDIType) do
      if GDITypeCount[GDIType] > 0 then
        DebugLn(ProcName,Format('   %s: %d', [dbgs(GDIType), GDITypeCount[GDIType]]));
  end;


  // tidy up messages
  if FMessageQueue.Count > 0   then begin
    DebugLn(ProcName, Format(rsWarningUnreleasedMessagesInQueue,[FMessageQueue.Count]));
    while FMessageQueue.First<>nil do
      fMessageQueue.RemoveMessage(fMessageQueue.FirstMessageItem,FPMF_All,true);
  end;

  // warn about unreleased timers
  n := FTimerData.Count;
  if (n > 0) then
  begin
    DebugLn(ProcName,Format(rsWarningUnreleasedTimerInfos,[n]));
    while (n > 0) do
    begin
      dec (n);
      pTimerInfo := PGtkITimerinfo (FTimerData.Items[n]);
      Dispose (pTimerInfo);
      FTimerData.Delete (n);
    end;
  end;

  {$ifdef TraceGdiCalls}
  if FDeviceContexts.Count>0 then begin
    //DebugLn('BackTrace for unreleased device contexts follows:');
    n:=0;
    HashItem:=FDeviceContexts.FirstHashItem;
    while (HashItem<>nil) and (n<MaxTraces) do
    begin
      DebugLn('DC: ', Dbgs(HashItem^.Item));
      DumpBackTrace(TDeviceContext(HashItem^.Item).StackAddrs);
      DebugLn();
      HashItem:=HashItem^.Next;
    end;
    if (n>=MaxTraces) then begin
      DebugLn('... Truncated dump DeviceContext leakage dump.');
      DebugLn();
    end;
  end;

  if (FGDIObjects.Count > 0)
  then begin
    //DebugLn('BackTrace for unreleased gdi objects follows:');
    for GDIType := Low(TGDIType) to High(TGDIType) do begin
      if GDITypeCount[GDIType]<>0 then begin
        n:=0;
        HashItem := FGDIObjects.FirstHashItem;
        while (HashItem <> nil) and (n<MaxTraces) do begin
          DebugLn(dbgs(gdiType),': ', dbgs(HashItem^.Item));
          DumpBackTrace(PgdiObject(HashItem^.Item)^.StackAddrs);
          DebugLn();
          HashItem := HashItem^.Next;
          inc(n);
        end;
        if (n>=MaxTraces) then begin
          DebugLn('... Truncated ',dbgs(GDIType),' leakage dump.');
          DebugLn();
        end;
      end;
    end;
  end;
  {$endif}

  FreeAndNil(FWidgetsWithResizeRequest);
  FreeAndNil(FWidgetsResized);
  FreeAndNil(FFixWidgetsResized);
  FMessageQueue.Free;
  FDeviceContexts.Free;
  FGDIObjects.Free;
  {$IFDEF Use_KeyStateList}
  FKeyStateList_.Free;
  {$ENDIF}
  FTimerData.Free;
  
  GtkDefDone;
  FreeAndNil(FDCManager);

  // finally remove our loghandler
  g_log_remove_handler(nil, FLogHandlerID);

  GTKWidgetSet := nil;
  WakeMainThread := nil;

  inherited Destroy;
end;

{$ifdef Unix}

procedure TGtkWidgetSet.PrepareSynchronize(AObject: TObject);
begin
  // wake up GUI thread by sending a byte through the threadsync pipe
  fpwrite(threadsync_pipeout, ' ', 1);
end;

procedure TGtkWidgetSet.ProcessChildSignal;
var
  pid: tpid;
  reason: TChildExitReason;
  status: integer;
  info: dword;
  handler: PChildSignalEventHandler;
begin
  repeat
    pid := fpwaitpid(-1, status, WNOHANG);
    if pid <= 0 then break;
    if wifexited(status) then
    begin
      reason := cerExit;
      info := wexitstatus(status);
    end else
    if wifsignaled(status) then
    begin
      reason := cerSignal;
      info := wtermsig(status);
    end else
      continue;
   
    handler := FChildSignalHandlers;
    while handler <> nil do
    begin
      if handler^.pid = pid then
      begin
        handler^.OnEvent(handler^.UserData, reason, info);
        break;
      end;
      handler := handler^.NextHandler;
    end;
  until false;
end;

function threadsync_iocallback(source: PGIOChannel; condition: TGIOCondition; 
  data: gpointer): gboolean; cdecl;
var
  thrashspace: array[1..1024] of byte;
begin
  // read the sent bytes
  fpread(threadsync_pipein, thrashspace[1], 1);
  
  Result := true;
  // one of children signaled ?
  if childsig_pending then
  begin
    childsig_pending := false;
    TGtkWidgetSet(data).ProcessChildSignal;
  end;
  // execute the to-be synchronized method
  if IsMultiThread then
    CheckSynchronize;
end;

procedure TGtkWidgetSet.InitSynchronizeSupport;
begin
  { TThread.Synchronize ``glue'' }
  WakeMainThread := @PrepareSynchronize;
  assignpipe(threadsync_pipein, threadsync_pipeout);
  threadsync_giochannel := g_io_channel_unix_new(threadsync_pipein);
  g_io_add_watch(threadsync_giochannel, G_IO_IN, @threadsync_iocallback, Self);
end;

{$else}

{$message warn TThread.Synchronize will not work on Gtk/Win32 }

procedure InitSynchronizeSupport;
begin
end;

{$endif}

{------------------------------------------------------------------------------
  procedure TGtkWidgetSet.UpdateTransientWindows;
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.UpdateTransientWindows;

type
  PTransientWindow = ^TTransientWindow;
  TTransientWindow = record
    GtkWindow: PGtkWindow;
    Component: TComponent;
    IsModal: boolean;
    SortIndex: integer;
    TransientParent: PGtkWindow;
  end;

var
  AllWindows: TFPList;
  List: PGList;
  Window: PGTKWindow;
  ATransientWindow: PTransientWindow;
  LCLObject: TObject;
  LCLComponent: TComponent;
  i: Integer;
  FirstModal: Integer;
  j: Integer;
  ATransientWindow1: PTransientWindow;
  ATransientWindow2: PTransientWindow;
  ParentTransientWindow: PTransientWindow;
  OldTransientParent: PGtkWindow;
begin
  if (not UseTransientForModalWindows) then exit;
  if UpdatingTransientWindows then begin
    DebugLn('TGtkWidgetSet.UpdateTransientWindows already updating');
    exit;
  end;
  UpdatingTransientWindows:=true;
  try
    {$IFDEF VerboseTransient}
    DebugLn('TGtkWidgetSet.UpdateTransientWindows');
    {$ENDIF}
    AllWindows:=nil;

    // find all currently visible gtkwindows
    List := gdk_window_get_toplevels;
    while List <> nil do
    begin
      if (List^.Data <> nil)
      then begin
        gdk_window_get_user_data(PGDKWindow(List^.Data), Pgpointer(@Window));
        if GtkWidgetIsA(PGtkWidget(Window), GTK_TYPE_WINDOW)
        and gtk_widget_visible(PGtkWidget(Window))
        then begin
          // visible window found -> add to list
          New(ATransientWindow);
          FillChar(ATransientWindow^,SizeOf(TTransientWindow),0);
          ATransientWindow^.GtkWindow:=Window;
          LCLObject:=GetLCLObject(Window);
          if (LCLObject<>nil) and (LCLObject is TComponent) then begin
            LCLComponent:=TComponent(LCLObject);
            ATransientWindow^.Component:=LCLComponent;
          end;
          if (ModalWindows<>nil) then
            ATransientWindow^.SortIndex:=ModalWindows.IndexOf(Window)
          else
            ATransientWindow^.SortIndex:=-1;
          ATransientWindow^.IsModal:=(ATransientWindow^.SortIndex>=0)
                                   and (GTK_WIDGET_VISIBLE(PGtkWidget(Window)));
          if not ATransientWindow^.IsModal then begin
            if (LCLObject is TCustomForm)
            and (TCustomForm(LCLObject).Parent=nil) then
              ATransientWindow^.SortIndex:=
                Screen.CustomFormZIndex(TCustomForm(LCLObject));
          end;
          
          if ATransientWindow^.SortIndex<0 then begin
            // this window has no form. Move it to the back.
            ATransientWindow^.SortIndex:=Screen.CustomFormCount;
          end;
          
          //DebugLn(['TGtkWidgetSet.UpdateTransientWindows LCLObject=',DbgSName(LCLObject),' ATransientWindow^.SortIndex=',ATransientWindow^.SortIndex]);
          if AllWindows=nil then AllWindows:=TFPList.Create;
          AllWindows.Add(ATransientWindow);
        end;
      end;
      list := g_list_next(list);
    end;

    if AllWindows=nil then exit;
    
    //for i:=0 to SCreen.CustomFormZOrderCount-1 do
    //  DebugLn(['TGtkWidgetSet.UpdateTransientWindows i=',i,'/',SCreen.CustomFormZOrderCount,' ',DbgSName(SCreen.CustomFormsZOrdered[i])]);
    
    // sort
    // move all modal windows to the end of the window list
    i:=AllWindows.Count-1;
    FirstModal:=AllWindows.Count;
    while i>=0 do begin
      ATransientWindow:=PTransientWindow(AllWindows[i]);
      if ATransientWindow^.IsModal
      and (i<FirstModal) then begin
        dec(FirstModal);
        if i<FirstModal then
          AllWindows.Exchange(i,FirstModal);
      end;
      dec(i);
    end;

    if FirstModal=AllWindows.Count then begin
      // there is no modal window
      // -> break all transient window relation ships
      for i:=AllWindows.Count-1 downto 0 do begin
        ATransientWindow:=PTransientWindow(AllWindows[i]);
        {$IFDEF VerboseTransient}
        DbgOut('TGtkWidgetSet.UpdateTransientWindows  Untransient ',i);
        if ATransientWindow^.Component<>nil then
          DbgOut(' ',ATransientWindow^.Component.Name,':',ATransientWindow^.Component.ClassName);
        DebugLn('');
        {$ENDIF}
        gtk_window_set_transient_for(ATransientWindow^.GtkWindow,nil);
      end;
    end else begin
      // there are modal windows
      // -> sort windows in z order and setup transient relationships

      //DebugLn(['TGtkWidgetSet.UpdateTransientWindows ModalWindows=',AllWindows.Count-FirstModal,' NonModalWindows=',FirstModal]);

      // sort modal windows (bubble sort)
      for i:=FirstModal to AllWindows.Count-2 do begin
        for j:=i+1 to AllWindows.Count-1 do begin
          ATransientWindow1:=PTransientWindow(AllWindows[i]);
          ATransientWindow2:=PTransientWindow(AllWindows[j]);
          if ATransientWindow1^.SortIndex>ATransientWindow2^.SortIndex then
            AllWindows.Exchange(i,j);
        end;
      end;

      // sort non modal windows for z order
      // ToDo: How do we get the z order?
      // For now, just use the inverse order in the Screen object
      // that means: the lower in the Screen object, the later in the transient list
      for i:=0 to FirstModal-2 do begin
        for j:=i+1 to FirstModal-1 do begin
          ATransientWindow1:=PTransientWindow(AllWindows[i]);
          ATransientWindow2:=PTransientWindow(AllWindows[j]);
          if ATransientWindow1^.SortIndex<ATransientWindow2^.SortIndex then
            AllWindows.Exchange(i,j);
        end;
      end;

      // set all transient relationships for LCL windows
      ParentTransientWindow:=nil;
      for i:=0 to AllWindows.Count-1 do begin
        ATransientWindow:=PTransientWindow(AllWindows[i]);
        if (ATransientWindow^.Component<>nil)
        and GTK_WIDGET_VISIBLE(PgtkWidget(ATransientWindow^.GtkWindow)) then
        begin
          if ParentTransientWindow<>nil then begin
            {$IFDEF VerboseTransient}
            DebugLn('Define TRANSIENT ',
            ' Parent=',
              ParentTransientWindow^.Component.Name,':',
                ParentTransientWindow^.Component.ClassName,
              ' Index=',ParentTransientWindow^.SortIndex,
              ' Wnd=',DbgS(ParentTransientWindow^.GtkWindow),
            ' Child=',ATransientWindow^.Component.Name,':',
                               ATransientWindow^.Component.ClassName,
              ' Index=',ATransientWindow^.SortIndex,
              ' Wnd=',DbgS(ATransientWindow^.GtkWindow),
              '');
            {$ENDIF}
            ATransientWindow^.TransientParent:=ParentTransientWindow^.GtkWindow;
          end;
          ParentTransientWindow:=ATransientWindow;
        end;
      end;

      // Each transient relationship can reorder the visible forms
      // To reduce flickering and creation of temporary circles
      // do the setup in two separate steps:

      // break unneeded transient relationships
      for i:=AllWindows.Count-1 downto 0 do begin
        ATransientWindow:=PTransientWindow(AllWindows[i]);
        OldTransientParent:=ATransientWindow^.GtkWindow^.transient_parent;
        if (OldTransientParent<>ATransientWindow^.TransientParent) then begin
          {$IFDEF VerboseTransient}
          DebugLn('Break old TRANSIENT i=',i,'/',AllWindows.Count,
          ' OldTransientParent=',DbgS(OldTransientParent),
          ' Child=',ATransientWindow^.Component.Name,':',
                             ATransientWindow^.Component.ClassName,
            ' Index=',ATransientWindow^.SortIndex,
            ' Wnd=',DbgS(ATransientWindow^.GtkWindow),
            '');
          {$ENDIF}
          gtk_window_set_transient_for(ATransientWindow^.GtkWindow,nil);
        end;
      end;

      // setup transient relationships
      for i:=0 to AllWindows.Count-1 do begin
        ATransientWindow:=PTransientWindow(AllWindows[i]);
        if ATransientWindow^.TransientParent=nil then continue;
        {$IFDEF VerboseTransient}
        DebugLn('Set TRANSIENT i=',i,'/',AllWindows.Count,
        ' Child=',ATransientWindow^.Component.Name,':',
                           ATransientWindow^.Component.ClassName,
          ' Index=',ATransientWindow^.SortIndex,
          ' Wnd=',DbgS(ATransientWindow^.GtkWindow),
          ' Parent=',DbgS(ATransientWindow^.TransientParent),
          '');
        {$ENDIF}
        gtk_window_set_transient_for(ATransientWindow^.GtkWindow,
                                     ATransientWindow^.TransientParent);
      end;
    end;

    // clean up
    for i:=0 to AllWindows.Count-1 do begin
      ATransientWindow:=PTransientWindow(AllWindows[i]);
      Dispose(ATransientWindow);
    end;
    AllWindows.Free;
  finally
    UpdatingTransientWindows:=false;
  end;
end;

{------------------------------------------------------------------------------
  procedure TGtkWidgetSet.UntransientWindow(GtkWindow: PGtkWindow);
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.UntransientWindow(GtkWindow: PGtkWindow);
{$IFDEF VerboseTransient}
var
  LCLObject: TObject;
{$ENDIF}
begin
  {$IFDEF VerboseTransient}
  DbgOut('TGtkWidgetSet.UntransientWindow ',DbgS(GtkWindow));
  LCLObject:=GetLCLObject(PGtkWidget(GtkWindow));
  if LCLObject<>nil then
    DbgOut(' LCLObject=',LCLObject.ClassName)
  else
    DbgOut(' LCLObject=nil');
  DebugLn('');
  {$ENDIF}
  // hide window, so that UpdateTransientWindows untransients it
  if GTK_WIDGET_VISIBLE(PgtkWidget(GtkWindow)) then
    gtk_widget_hide(PgtkWidget(GtkWindow));
  UpdateTransientWindows;
  // remove it from the modal window list
  if ModalWindows<>nil then begin
    ModalWindows.Remove(GtkWindow);
    if ModalWindows.Count=0 then FreeAndNil(ModalWindows);
  end;
end;

{------------------------------------------------------------------------------
  Method: TGtkWidgetSet.SendCachedLCLMessages
  Params:  None
  Returns: Nothing

  Some LCL messages are not sent directly to the gtk. Send them now.
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.SendCachedLCLMessages;

  procedure SendCachedLCLResizeRequests;
  var
    Widget: PGtkWidget;
    LCLControl: TWinControl;
    IsTopLevelWidget: boolean;
    TopologicalList: TFPList; // list of PGtkWidget;
    i: integer;

    procedure RaiseWidgetWithoutControl;
    begin
      RaiseGDBException('ERROR: TGtkWidgetSet.SendCachedLCLMessages Widget '
        +DbgS(Widget)+' without LCL control');
    end;

  begin
    if FWidgetsWithResizeRequest.Count=0 then exit;
    {$IFDEF VerboseSizeMsg}
    DebugLn('GGG1 SendCachedLCLResizeRequests SizeMsgCount=',dbgs(FWidgetsWithResizeRequest.Count));
    {$ENDIF}

    TopologicalList:=CreateTopologicalSortedWidgets(FWidgetsWithResizeRequest);
    for i:=0 to TopologicalList.Count-1 do begin
      Widget:=TopologicalList[i];

      // resize widget
      LCLControl:=TWinControl(GetLCLObject(Widget));
      if (LCLControl=nil) or (not (LCLControl is TControl)) then begin
        RaiseWidgetWithoutControl;
      end;
      {$IFDEF VerboseSizeMsg}
      if CompareText(LCLControl.ClassName,'TScrollBar')=0 then
      DebugLn('SendCachedLCLMessages ',LCLControl.Name,':',LCLControl.ClassName,
        ' ',dbgs(LCLControl.Left)+','+dbgs(LCLControl.Top)+','+dbgs(LCLControl.Width)+'x'+dbgs(LCLControl.Height));
      {$ENDIF}

      IsTopLevelWidget:= (LCLControl is TCustomForm)
                         and (LCLControl.Parent = nil);

      if not IsTopLevelWidget then begin
        SetWidgetSizeAndPosition(LCLControl);
      end
      else begin
        // resize form
        {$IFDEF VerboseFormPositioning}
        DebugLn('VFP SendCachedLCLMessages1 ', dbgs(GetControlWindow(Widget)<>nil));
        if (LCLControl is TCustomForm) then
          DebugLn('VFP SendCachedLCLMessages2 ',LCLControl.ClassName,' ',
            dbgs(LCLControl.Left),',',dbgs(LCLControl.Top),',',dbgs(LCLControl.Width),',',dbgs(LCLControl.Height));
        {$ENDIF}
        SetWindowSizeAndPosition(PgtkWindow(Widget),TWinControl(LCLControl));
      end;

    end;
    TopologicalList.Free;
    FWidgetsWithResizeRequest.Clear;
  end;

begin
  SendCachedLCLResizeRequests;
end;

{------------------------------------------------------------------------------
  Method: TGtkWidgetSet.LCLtoGtkMessagePending
  Params:  None
  Returns: boolean

  Returns true if any messages from the lcl to the gtk is in cache and needs
  delivery.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.LCLtoGtkMessagePending: boolean;
begin
  Result:=(FWidgetsWithResizeRequest.Count>0);
end;

{------------------------------------------------------------------------------
  Method: TGtkWidgetSet.SendCachedGtkMessages
  Params:  None
  Returns: Nothing

  Some Gtk messages are not sent directly to the LCL. Send them now.
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.SendCachedGtkMessages;
begin
  SendCachedGtkResizeNotifications;
end;

{------------------------------------------------------------------------------
  Method: TGtkWidgetSet.SetLabelCaption
  Params:  ALabel: The label to set the caption for
           ACaption: The caption to set
           AComponent: The component the label belongs to
           ASignalWidget: A widget to connect the accelerator to
           ASignal: The signal to connect
  Returns: Nothing

  Sets the Caption of a gtklabel. If a accelerator is present, it is connected.
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.SetLabelCaption(const ALabel: PGtkLabel;
  const ACaption: String
  {$IFDEF Gtk1}
  ; const AComponent: TComponent;
  const ASignalWidget: PGTKWidget; const ASignal: PChar
  {$ENDIF});
var
  Caption, Pattern: String;
  AccelKey: Char;
begin
  Caption := ACaption;
  LabelFromAmpersands(Caption, Pattern, AccelKey);
  //DebugLn(['TGtkWidgetSet.SetLabelCaption ',GetWidgetDebugReport(PGtkWIdget(ALabel)),' Caption=',dbgstr(Caption),' Pattern=',dbgstr(Pattern),' aCaption=',dbgstr(aCaption)]);
  gtk_label_set_text(ALabel, PChar(Caption));
  gtk_label_set_pattern(ALabel, PChar(Pattern));

  {$IFDEF Gtk1}
  if AComponent = nil then Exit;
  if ASignalWidget = nil then Exit;
  if ASignal = '' then Exit;

  // update the Accelerator
  if AccelKey = #0
  then Accelerate(AComponent, ASignalWidget, GDK_VOIDSYMBOL, 0, ASignal)
  else Accelerate(AComponent, ASignalWidget, Ord(AccelKey), 0, ASignal);
  {$ENDIF}
end;

procedure TGtkWidgetSet.SetWidgetColor(const AWidget: PGtkWidget;
  const FGColor, BGColor: TColor; const Mask: tGtkStateEnum);
// Changes some colors of the widget style
// IMPORTANT:
// SystemColors like clBtnFace depend on the theme and widget class, so they
// must be read from the theme. But many gtk themes do not provide all colors
// and instead only provide bitmaps.
// Since we don't have good fallbacks yet, and many controls use SystemColors
// for Delphi compatibility: ignore SystemColors.
var
  i: integer;
  xfg,xbg: TGDKColor;
  ChangeFGColor: Boolean;
  ChangeBGColor: Boolean;
  {$IFDEF Gtk1}
  WindowStyle: PGtkStyle;
  {$ENDIF}
begin
  ChangeFGColor:=((FGColor and SYS_COLOR_BASE)=0) and (FGColor<>clNone);
  ChangeBGColor:=((BGColor and SYS_COLOR_BASE)=0) and (BGColor<>clNone);
  if (not ChangeFGColor) and (not ChangeBGColor) then exit;

  if GtkWidgetIsA(AWidget,GTKAPIWidget_GetType) then begin
    // the GTKAPIWidget is self drawn, so no use to change the widget style.
    exit;
  end;

  {$IFDEF DisableWidgetColor}
  exit;
  {$ENDIF}
  
  {$IFDEF Gtk1}
  if (GTK_WIDGET_REALIZED(AWidget)) then begin
    WindowStyle := gtk_style_copy(gtk_widget_get_style (AWidget));
  end else begin
    WindowStyle := gtk_style_copy(gtk_rc_get_style (AWidget));
  end;
  if (Windowstyle = nil)  then begin
    Windowstyle := gtk_style_new;
  end;
  {$ENDIF}

  //DebugLn('TGtkWidgetSet.SetWidgetColor ',GetWidgetDebugReport(AWidget),' ',hexstr(FGColor,8),'  ',hexstr(BGColor,8));
  //RaiseGDBException('');
  if ChangeFGColor then begin
    xfg:=AllocGDKColor(colorToRGB(FGColor));
    for i := GTK_STATE_NORMAL to GTK_STATE_INSENSITIVE do begin
      if i in mask then begin
        if GTK_STYLE_TEXT in mask then begin
          {$IFDEF Gtk1}
          windowStyle^.text[i]:=xfg;
          {$ELSE}
          gtk_widget_modify_text(AWidget, i ,@xfg);
          {$ENDIF}
        end else begin
          {$IFDEF Gtk1}
          windowStyle^.fg[i]:=xfg;
          {$ELSE}
          gtk_widget_modify_fg(AWidget, i ,@xfg);
          {$ENDIF}
        end;
      end;
    end;
  end;
  if ChangeBGColor then begin
    xbg:=AllocGDKColor(colorToRGB(BGColor));
    for i := GTK_STATE_NORMAL to GTK_STATE_INSENSITIVE do begin
      if i in mask then begin
        if GTK_STYLE_BASE in mask then begin
          {$IFDEF Gtk1}
          windowStyle^.base[i]:=xbg;
          {$ELSE}
          gtk_widget_modify_base(AWidget, i ,@xbg);
          {$ENDIF}
        end else begin
          {$IFDEF Gtk1}
          windowStyle^.bg[i]:=xbg;
          {$ELSE}
          gtk_widget_modify_bg(AWidget, i ,@xbg);
          {$ENDIF}
        end;
      end;
    end;
  end;
  {$IFDEF Gtk1}
  gtk_widget_set_style(aWidget,windowStyle);
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  procedure TGtkWidgetSet.SendPaintMessagesForInternalWidgets(
    AWinControl: TWinControl);


 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.SendPaintMessagesForInternalWidgets(
  AWinControl: TWinControl);
type
  TInternalPaintContext = record
    WinControl: TWinControl;
    MainWidget: PGtkWidget;
    ClientWidget: PGtkWidget;
    MainWindow: PGdkWindow;
    ClientWindow: PGdkWindow;
    WindowList: TFPList;
  end;

var
  Context: TInternalPaintContext;

  procedure SendPaintMessageForGDkWindow(PaintWindow: PGdkWindow);
  var
    AMessage: TLMessage;
    {$IFDEF VerboseDsgnPaintMsg}
    Left, Top, Width, Height: integer;
    {$ENDIF}
    //Child: PGList;
    UserData: Pointer;
    LCLObject: TObject;
  begin
    if PaintWindow=nil then exit;
    // check if PaintWindow is only used internally
    // and was not already used for an internal paint message
    if (PaintWindow=nil) or (PaintWindow=Context.MainWindow)
    or (PaintWindow=Context.ClientWindow)
    or ((Context.WindowList<>nil)
      and (Context.WindowList.IndexOf(PaintWindow)>=0))
    then exit;

    if Context.WindowList=nil then
      Context.WindowList:=TFPList.Create;
    Context.WindowList.Add(PaintWindow);
    {$IFDEF DebugGDK}BeginGDKErrorTrap;{$ENDIF}

    if (not gdk_window_is_visible(PaintWindow))
      or (not gdk_window_is_viewable(PaintWindow))
    then begin
      {$IFDEF DebugGDK}EndGDKErrorTrap;{$ENDIF}
      exit;
    end;

    // check if window belongs to another LCL control
    gdk_window_get_user_data(PaintWindow,@UserData);
    {$IFDEF DebugGDK}EndGDKErrorTrap;{$ENDIF}

    if (UserData<>nil)
    and (GtkWidgetIsA(PGtkWidget(UserData), GTK_TYPE_WIDGET))
    then begin
      LCLObject:=GetLCLObject(UserData);
      if (LCLObject<>nil) and (LCLObject<>AWinControl) then exit;
    end;

    AMessage.Msg := LM_INTERNALPAINT;
    AMessage.WParam := CreateDCForWidget(Context.MainWidget,PaintWindow,false);
    AMessage.LParam := 0;
    AMessage.Result := 0;

    {$IFDEF VerboseDsgnPaintMsg}
    gdk_window_get_size(PaintWindow,@Width,@Height);
    gdk_window_get_origin(PaintWindow,@Left,@Top);
    DebugLn('SendInternalPaintMessage ',
      AWinControl.Name,':',AWinControl.ClassName,
      ' InternalWindow=',DbgS(PaintWindow),
      ' ',Left,',',Top,',',Width,',',Height,
      ' visible=',gdk_window_is_visible(PaintWindow),
      ' viewable=',gdk_window_is_viewable(PaintWindow),
      '');
    {$ENDIF}
    DeliverMessage(AWinControl,AMessage);

    if AMessage.WParam<>0 then
      ReleaseDC(0,HDC(AMessage.WParam));

    { Normally the childwindows should be explored too, but there are some
      widgets with bad gdkwindows. ToDo: find a way to determine, if a
      gdkwindow is good

    Child:=gdk_window_get_children(PaintWindow);
    while Child<>nil do begin
      SendPaintMessageForGDkWindow(PGdkWindow(Child^.Data));
      Child:=Child^.Next;
    end;}
  end;

  procedure ForAllChilds(PaintWidget: PgtkWidget);
  var
    LCLObject: TObject;
    {$IFDEF Gtk2}
    ChildEntry2: PGList;
    {$ELSE}
    ChildEntry: PGSList;
    {$ENDIF}
  begin
    if PaintWidget=nil then exit;
    LCLObject:=GetLCLObject(PaintWidget);
    if (LCLObject<>nil) and (LCLObject<>AWinControl) then exit;
    // send the paint message
    SendPaintMessageForGDkWindow(GetControlWindow(PaintWidget));

    // search all child widgets
    if GtkWidgetIsA(PaintWidget, GTK_TYPE_CONTAINER) then
    begin
      // this is a container widget -> go through all childs
      {$IFDEF Gtk2}
      ChildEntry2:=gtk_container_get_children(PGtkContainer(PaintWidget));
      while ChildEntry2<>nil do begin
        if PGtkWidget(ChildEntry2^.Data)<>PaintWidget then
          ForAllChilds(PGtkWidget(ChildEntry2^.Data));
        ChildEntry2:=ChildEntry2^.Next;
      end;
      {$ELSE}
      ChildEntry:=PGtkContainer(PaintWidget)^.resize_widgets;
      while ChildEntry<>nil do begin
        if PGtkWidget(ChildEntry^.Data)<>PaintWidget then
          ForAllChilds(PGtkWidget(ChildEntry^.Data));
        ChildEntry:=ChildEntry^.Next;
      end;
      {$ENDIF}
    end;
    if GtkWidgetIsA(PaintWidget, GTK_TYPE_SCROLLED_WINDOW)
    then begin
      ForAllChilds(PGtkScrolledWindow(PaintWidget)^.hscrollbar);
      ForAllChilds(PGtkScrolledWindow(PaintWidget)^.vscrollbar);
    end;
    if GtkWidgetIsA(PaintWidget, GTK_TYPE_BIN) then
    begin
      ForAllChilds(PGtkBin(PaintWidget)^.child);
    end;
    if GtkWidgetIsA(PaintWidget, GTK_TYPE_COMBO) then begin
      ForAllChilds(PGtkCombo(PaintWidget)^.entry);
      ForAllChilds(PGtkCombo(PaintWidget)^.button);
    end;
    if GtkWidgetIsA(PaintWidget, GTK_TYPE_RANGE)
    then begin
      {$IFDEF Gtk1}
      SendPaintMessageForGDkWindow(PGtkRange(PaintWidget)^.slider);
      SendPaintMessageForGDkWindow(PGtkRange(PaintWidget)^.trough);
      SendPaintMessageForGDkWindow(PGtkRange(PaintWidget)^.step_forw);
      SendPaintMessageForGDkWindow(PGtkRange(PaintWidget)^.step_back);
      {$ENDIF}
    end;
    if GtkWidgetIsA(PaintWidget, GTK_TYPE_TEXT) then
    begin
      SendPaintMessageForGDkWindow(PGtkText(PaintWidget)^.text_area);
    end;
    if GtkWidgetIsA(PaintWidget, GTK_TYPE_ENTRY) then
    begin
      SendPaintMessageForGDkWindow(PGtkEntry(PaintWidget)^.text_area);
    end;
  end;

begin
  if AWinControl=nil then exit;
  Context.WinControl:=AWinControl;
  with Context do begin
    MainWidget:=PGtkWidget(WinControl.Handle);
    if MainWidget=nil then exit;
    if MainWidget<>nil then
      MainWindow:=GetControlWindow(MainWidget)
    else
      exit;
    ClientWidget:=GetFixedWidget(MainWidget);
    if ClientWidget<>nil then
      ClientWindow:=GetControlWindow(ClientWidget)
    else
      ClientWindow:=nil;
    WindowList:=nil;
  end;
  {DebugLn('TGtkWidgetSet.SendPaintMessagesForInternalWidgets START ',
    ' ',AWinControl.Name,':',AWinControl.ClassName,
    ' ',DbgS(Context.MainWidget),
    ' ',DbgS(Context.MainWindow),
    ' ',DbgS(Context.ClientWidget),
    ' ',DbgS(Context.ClientWindow),
    '');}

  ForAllChilds(Context.MainWidget);

  Context.WindowList.Free;
end;

{------------------------------------------------------------------------------
  Method: TGtkWidgetSet.AppProcessMessages
  Params:  None
  Returns: Nothing

  Handle all pending messages of the GTK engine and of this interface
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.AppProcessMessages;

  function PendingGtkMessagesExists: boolean;
  begin
    Result:=(gtk_events_pending<>0) or LCLtoGtkMessagePending;
  end;

var
  vlItem : TGtkMessageQueueItem;
  vlMsg  : PMSg;
  i: Integer;
begin
  repeat
    // send cached LCL messages to the gtk
    //DebugLn(['TGtkWidgetSet.AppProcessMessages SendCachedLCLMessages']);
    SendCachedLCLMessages;

    // let gtk handle up to 100 messages and call our callbacks
    i:=100;
    while (gtk_events_pending<>0) and (i>0) do begin
      gtk_main_iteration_do(False);
      dec(i);
    end;

    //DebugLn(['TGtkWidgetSet.AppProcessMessages SendCachedGtkMessages']);
    // send cached gtk messages to the lcl
    SendCachedGtkMessages;

    // then handle our own messages
    while not Application.Terminated do begin
      // fetch first message
      vlItem := fMessageQueue.FirstMessageItem;
      if vlItem = nil then break;

      // remove message from queue
      if vlItem.IsPaintMessage then begin
        //DebugLn(['TGtkWidgetSet.AppProcessMessages Paint: ',DbgSName(GetLCLObject(Pointer(vlItem.Msg^.hwnd)))]);
        // paint messages are the most expensive messages in the LCL,
        // therefore they are sent after all other
        if MovedPaintMessageCount<10 then begin
          inc(MovedPaintMessageCount);
          if fMessageQueue.HasNonPaintMessages then begin
            // there are non paint messages -> move paint message to the end
            fMessageQueue.MoveToLast(FMessageQueue.First);
            continue;
          end else begin
            // there are only paint messages left in the queue
            // -> check other queues
            if PendingGtkMessagesExists then break;
          end;
        end else begin
          // handle this paint message now
          MovedPaintMessageCount:=0;
        end;
      end;

      //DebugLn(['TGtkWidgetSet.AppProcessMessages SendMessage: ',DbgSName(GetLCLObject(Pointer(vlItem.Msg^.hwnd)))]);
      vlMsg:=fMessageQueue.PopFirstMessage;

      // Send message
      with vlMsg^ do SendMessage(hWND, Message, WParam, LParam);
      Dispose(vlMsg);
    end;

    // proceed until all messages are handled
  until (not PendingGtkMessagesExists) or Application.Terminated;
end;

{------------------------------------------------------------------------------
  Method: TGtkWidgetSet.AppWaitMessage
  Params:  None
  Returns: Nothing

  Passes execution control to the GTK engine till something happens
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.AppWaitMessage;
begin
  WaitingForMessages:=true;
  gtk_main_iteration_do(True);
  WaitingForMessages:=false;
end;


procedure TGtkWidgetSet.FreeStockItems;

  procedure DeleteAndNilObject(var h: HGDIOBJ);
  begin
    DeleteObject(h);
    h:=0;
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

{------------------------------------------------------------------------------
  Method: TGtkWidgetSet.AppTerminate
  Params:  None
  Returns: Nothing

  *Note: Tells GTK Engine to halt and destroy
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.AppTerminate;
begin
  FreeAllStyles;
  // MG: using gtk_main_quit is not a clean way to close
  //gtk_main_quit;
end;

function TGTKWidgetSet.CreateThemeServices: TThemeServices;
begin
  Result := TGtkThemeServices.Create;
end;

Procedure TGtkWidgetSet.InitStockItems;
var
  LogBrush: TLogBrush;
  logPen : TLogPen;
begin
  FillChar(LogBrush,SizeOf(TLogBrush),0);
  LogBrush.lbStyle := BS_NULL;
  FStockNullBrush := CreateBrushIndirect(LogBrush);
  LogBrush.lbStyle := BS_SOLID;
  LogBrush.lbColor := $000000;
  FStockBlackBrush := CreateBrushIndirect(LogBrush);
  LogBrush.lbColor := $C0C0C0;
  FStockLtGrayBrush := CreateBrushIndirect(LogBrush);
  LogBrush.lbColor := $808080;
  FStockGrayBrush := CreateBrushIndirect(LogBrush);
  LogBrush.lbColor := $404040;
  FStockDkGrayBrush := CreateBrushIndirect(LogBrush);
  LogBrush.lbColor := $FFFFFF;
  FStockWhiteBrush := CreateBrushIndirect(LogBrush);

  LogPen.lopnStyle := PS_NULL;
  LogPen.lopnWidth.X := 1;
  LogPen.lopnColor := $FFFFFF;
  FStockNullPen := CreatePenIndirect(LogPen);
  LogPen.lopnStyle := PS_SOLID;
  FStockWhitePen := CreatePenIndirect(LogPen);
  LogPen.lopnColor := $000000;
  FStockBlackPen := CreatePenIndirect(LogPen);

  FStockSystemFont := 0;//Styles aren't initialized yet
end;

{------------------------------------------------------------------------------
  Method: TGtkWidgetSet.AppInit
  Params:  None
  Returns: Nothing

  *Note: Initialize GTK engine
  (is called by TApplication.Initialize which is typically after all
   finalization sections)
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.AppInit(var ScreenInfo: TScreenInfo);
begin
  InitKeyboardTables;
  { Compute pixels per inch variable }
  ScreenInfo.PixelsPerInchX :=
    RoundToInt(gdk_screen_width / (GetScreenWidthMM / 25.4));
  ScreenInfo.PixelsPerInchY :=
    RoundToInt(gdk_screen_height / (GetScreenHeightMM / 25.4));
  ScreenInfo.ColorDepth := gdk_visual_get_system^.depth;
end;

{------------------------------------------------------------------------------
  Method: TGtkWidgetSet.AppMinimize
  Params:  None
  Returns: Nothing

  Minimizes the application
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.AppMinimize;
var
  i: Integer;
  AForm: TCustomForm;
begin
  //debugln('TGtkWidgetSet.AppMinimize A');
  if Screen=nil then exit;
  for i:=0 to Screen.CustomFormCount-1 do begin
    AForm:=Screen.CustomForms[i];
    //debugln('TGtkWidgetSet.AppMinimize B ',DbgSName(AForm),' AForm.Parent=',DbgSName(AForm.Parent),' AForm.HandleAllocated=',dbgs(AForm.HandleAllocated));
    if (AForm.Parent=nil) and AForm.HandleAllocated then begin
      ShowWindow(AForm.Handle, SW_MINIMIZE);
    end;
  end;
end;

procedure TGTKWidgetSet.AppRestore;
begin
  DebugLn(['TGTKWidgetSet.AppRestore TODO']);
end;

{------------------------------------------------------------------------------
  Method: TGtkWidgetSet.AppBringToFront
  Params:  None
  Returns: Nothing

  Shows the application above all other non-topmost windows
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.AppBringToFront;
begin
  // TODO: Implement me!
end;

{------------------------------------------------------------------------------
  procedure TGTKWidgetSet.AppSetTitle(const ATitle: string);
-------------------------------------------------------------------------------}
procedure TGTKWidgetSet.AppSetTitle(const ATitle: string);
begin

end;

function TGTKWidgetSet.LCLPlatform: TLCLPlatform;
begin
  Result:= lpGtk;
end;

{------------------------------------------------------------------------------
  Method: TGtkWidgetSet.RecreateWnd
  Params:  Sender: TObject - the lcl wincontrol, that is to recreated
  Returns: none

  Destroys Handle and child Handles and recreates them.
-------------------------------------------------------------------------------}
function TGtkWidgetSet.RecreateWnd(Sender: TObject): Integer;
var
  aWinControl, aParent : TWinControl;
Begin
  aWinControl:=TWinControl(Sender);
  aParent := aWinControl.Parent;
  if aParent<>nil then begin
    // remove and insert the control
    // this will destroy and recreate all child handles
    aWinControl.Parent := nil;
    aWinControl.Parent := aParent;
  end;
  DebugLn(['TGtkWidgetSet.RecreateWnd ',DbgSName(Sender)]);
  ResizeChild(Sender,aWinControl.Left,aWinControl.Top,
                     aWinControl.Width,aWinControl.Height);
  ShowHide(Sender);
  Result:=0;
End;

{------------------------------------------------------------------------------
  Function: CreateTimer
  Params: Interval:
          TimerFunc: Callback
  Returns: a GTK-timer id (use this ID to destroy timer)

  This function will create a GTK timer object and associate a callback to it.

  Design: A callback to the TTimer class is implemented.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.CreateTimer(Interval: integer;
  TimerFunc: TFNTimerProc) : THandle;
var
  TimerInfo: PGtkITimerinfo;
begin
  if ((Interval < 1) or (not Assigned(TimerFunc)))
  then
    Result := 0
  else begin
    New(TimerInfo);
    TimerInfo^.TimerFunc := TimerFunc;
    {$IFDEF VerboseTimer}
    DebugLn('TGtkWidgetSet.SetTimer %p CurTimerCount=%d OldTimerCount=%d', [TimerInfo, FTimerData.Count, FOldTimerData.Count]);
    {$ENDIF}
    Result:= gtk_timeout_add(Interval, @gtkTimerCB, TimerInfo);
    if Result = 0 then
      Dispose(TimerInfo)
    else begin
      TimerInfo^.TimerFunc := TimerFunc;
      TimerInfo^.TimerHandle:=Result;
      FTimerData.Add(TimerInfo);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Function: DestroyTimer
  Params: TimerHandle
  Returns:

  WARNING: There seems to be a bug in gtk-1.2.x which breaks gtk_timeout_remove
           thus we can't dispose PGtkITimerinfo here (s.a. gtkTimerCB).
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.DestroyTimer(TimerHandle: THandle) : boolean;
var
  n : integer;
  TimerInfo : PGtkITimerinfo;
begin
  Assert(False, 'Trace:removing timer!!!');
  n := FTimerData.Count;
  while (n > 0) do begin
    dec (n);
    TimerInfo := PGtkITimerinfo(FTimerData.Items[n]);
    if (TimerInfo^.TimerHandle=guint(TimerHandle)) then
    begin
      {$IFDEF VerboseTimer}
      DebugLn('TGtkWidgetSet.KillTimer TimerInfo=',DbgS(TimerInfo),' TimerHandle=',TimerHandle,' CurTimerCount=',FTimerData.Count,' OldTimerCount=',FOldTimerData.Count);
      {$ENDIF}
      gtk_timeout_remove(TimerInfo^.TimerHandle);
      FTimerData.Delete(n);
      Dispose(TimerInfo);
    end;
  end;
  Result:=true;
end;

{------------------------------------------------------------------------------
  function TGtkWidgetSet.InternalGetDIBits(DC: HDC; Bitmap: HBitmap;
    StartScan, NumScans: UINT;
    BitSize : Longint; Bits: Pointer;
    var BitInfo: BitmapInfo; Usage: UINT; DIB : Boolean): Integer;
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.InternalGetDIBits(DC: HDC; Bitmap: HBitmap;
  StartScan, NumScans: UINT; BitSize : Longint; Bits: Pointer;
  var BitInfo: BitmapInfo; Usage: UINT; DIB : Boolean): Integer;
const
  PadLine : array[0..12] of Byte = (0,0,0,0,0,0,0,0,0,0,1,0,0);
  TempBuffer : array[0..2] of Byte = (0,0,0);
  
var
  GdiObject: PGDIObject absolute Bitmap;

  Source: PGDKPixbuf;
  rowstride, PixelPos: Longint;
  Pixels: PByte;
  FDIB: TDIBSection;
  X, Y: Longint;
  PadSize, Pos, BytesPerPixel: Longint;
  Buf16Bit: word;

  procedure DataSourceInitialize(Bitmap : PGDIObject; Width : Longint);
  begin
    Source := nil;

    case Bitmap^.GDIBitmapType of
      gbBitmap:
        if Bitmap^.GDIBitmapObject <> nil
        then begin
          {$ifdef VerboseGdkPixbuf} debugln('DataSourceInitialize A1');{$endif}
          
          Source := CreatePixbufFromDrawable(Bitmap^.GDIBitmapObject, Bitmap^.Colormap, False, 0,StartScan,0,0,Width,StartScan + NumScans);
          rowstride := gdk_pixbuf_get_rowstride(Source);
          Pixels := PByte(gdk_pixbuf_get_pixels(Source));
          
          {$ifdef VerboseGdkPixbuf} debugln('DataSourceInitialize A2');{$endif}
        end;
      gbPixmap:
        if Bitmap^.GDIPixmapObject.Image <> nil
        then begin
          {$ifdef VerboseGdkPixbuf} debugln('DataSourceInitialize B1');{$endif}
          
          Source := CreatePixbufFromDrawable(Bitmap^.GDIPixmapObject.Image, Bitmap^.Colormap, False, 0, StartScan, 0, 0, Width, StartScan + NumScans);
          {$note TODO: Apply alpha based on mask when 32bit mode is added}
            
          rowstride := gdk_pixbuf_get_rowstride(Source);
          Pixels := PByte(gdk_pixbuf_get_pixels(Source));
          
          {$ifdef VerboseGdkPixbuf} debugln('DataSourceInitialize B2');{$endif}
        end;
    end;
  end;
  
  function DataSourceGetGDIRGB(Bitmap : PGDIObject; X, Y : Longint) : TGDIRGB;
  begin
    if Bitmap <> nil then ; //Keep compiler happy..

    PixelPos := rowstride*Y + X*3;

    with Result do
    begin
      Red   := Pixels[PixelPos + 0];
      Green := Pixels[PixelPos + 1];
      Blue  := Pixels[PixelPos + 2];
    end;
  end;
  
  procedure DataSourceFinalize;
  begin
    if Source <> nil
    then gdk_pixbuf_unref(Source);
  end;

  procedure WriteData(Value : PByte; Size : Longint);
  begin
    System.Move(Value^, PByte(Bits)[Pos], Size);
    Inc(Pos, Size);
  end;

  procedure WriteData(Value : Word);
  begin
    PByte(Bits)[Pos] := Lo(Value);
    inc(Pos);
    PByte(Bits)[Pos] := Hi(Value);
    inc(Pos);
  end;

begin
  Assert(False, 'trace:[TGtkWidgetSet.InternalGetDIBits]');


  Result := 0;
  if (DC=0) or (Usage=0) then ;

  if not IsValidGDIObject(Bitmap)
  then begin
    DebugLn('WARNING: [TGtkWidgetSet.InternalGetDIBits] invalid Bitmap!');
    Exit;
  end;

  if GdiObject^.GDIType <> gdiBitmap
  then begin
    DebugLn('WARNING: [TGtkWidgetSet.InternalGetDIBits] not a Bitmap!');
    Exit;
  end;
  

  FillChar(FDIB, SizeOf(FDIB), 0);
  GetObject(Bitmap, SizeOf(FDIB), @FDIB);
  BitInfo.bmiHeader := FDIB.dsBmih;

  with GdiObject^, BitInfo.bmiHeader do
  begin
    if not DIB
    then begin
      NumScans := biHeight;
      StartScan := 0;
    end;
    BytesPerPixel := biBitCount div 8;

    if BitSize <= 0 then
      BitSize := longint(SizeOf(Byte))
                 *(longint(biSizeImage) div biHeight)
                 *longint(NumScans + StartScan);
    if MemSize(Bits) < PtrInt(BitSize)
    then begin
      DebugLn('WARNING: [TGtkWidgetSet.InternalGetDIBits] not enough memory allocated for Bits!');
      exit;
    end;

    // ToDo: other bitcounts
    if (biBitCount<>24) and (biBitCount<>16)
    then begin
      DebugLn('WARNING: [TGtkWidgetSet.InternalGetDIBits] unsupported biBitCount=',dbgs(biBitCount));
      exit;
    end;

    if NumScans = 0 then Exit;

    Pos := 0;
    PadSize := (Longint(biSizeImage) div biHeight) - biWidth * BytesPerPixel;

    {$ifdef DebugGDK} BeginGDKErrorTrap; try{$ENDIF}
    DataSourceInitialize(GdiObject, biWidth);

    if DIB
    then Y := NumScans - 1
    else Y := 0;

    case biBitCount of
      24: repeat
        for X := 0 to biwidth - 1 do
        begin
          with DataSourceGetGDIRGB(PGDIObject(Bitmap), X, Y) do
          begin
            TempBuffer[0] := Blue;
            TempBuffer[1] := Green;
            TempBuffer[2] := Red;
          end;
          WriteData(TempBuffer, BytesPerPixel);
        end;
        WriteData(PadLine, PadSize);

        if DIB
        then dec(y)
        else inc(y);
      until (Y < 0) or (y >= longint(NumScans));

      16: repeat
        for X := 0 to biwidth - 1 do
        begin
          with DataSourceGetGDIRGB(PGDIObject(Bitmap), X, Y) do
          begin
            Buf16Bit := (Blue  shr 3) shl 11
                      + (Green shr 2) shl 5
                      + (Red   shr 3);
          end;
          WriteData(Buf16Bit);
        end;
        WriteData(PadLine, PadSize);

        if DIB
        then dec(y)
        else inc(y);
      until (Y < 0) or (y >= longint(NumScans));
    end;
  end;

  DataSourceFinalize;


  {$ifdef DebugGDK}finally EndGDKErrorTrap; end;{$endif}
end;

function TGtkWidgetSet.RawImage_DescriptionFromDrawable(out ADesc: TRawImageDescription; ADrawable: PGdkDrawable; ACustomAlpha: Boolean): Boolean;
var
  Visual: PGdkVisual;
  Image: PGdkImage;
  Width, Height, Depth: integer;
  IsBitmap: Boolean;
begin
  Visual := nil;
  Width := 0;
  Height := 0;

  if ADrawable = nil
  then begin
    Visual := gdk_visual_get_system;
    IsBitmap := False;
  end
  else begin
    {$ifdef gtk1}
    gdk_window_get_geometry(ADrawable, nil, nil, @Width, @Height, @Depth);
    {$else}
    gdk_drawable_get_size(ADrawable, @Width, @Height);
    Depth := gdk_drawable_get_depth(ADrawable);
    {$endif}
    Visual := gdk_window_get_visual(ADrawable);
    // pixmaps and bitmaps dont have a visual, but for pixmaps we need one
    if Visual = nil
    then Visual := gdk_visual_get_best_with_depth(Depth);
    IsBitmap := Depth = 1;
  end;

  if (Visual = nil) and not IsBitmap // bitmaps don't have a visual
  then begin
    DebugLn('TGtkWidgetSet.RawImage_DescriptionFromDrawable: visual failed');
    Exit(False);
  end;

  ADesc.Init;
  ADesc.Width := cardinal(Width);
  ADesc.Height := cardinal(Height);
  ADesc.BitOrder := riboBitsInOrder;

  if ACustomAlpha
  then begin
    // always give pixbuf description for alpha images
    ADesc.Depth := 32;
    ADesc.BitsPerPixel := 32;
    ADesc.LineEnd := rileDWordBoundary;
    ADesc.ByteOrder := riboLSBFirst;

    ADesc.RedPrec := 8;
    ADesc.RedShift := 0;
    ADesc.GreenPrec := 8;
    ADesc.GreenShift := 8;
    ADesc.BluePrec := 8;
    ADesc.BlueShift := 16;
    ADesc.AlphaPrec := 8;
    ADesc.AlphaShift := 24;

    ADesc.MaskBitsPerPixel := 1;
    ADesc.MaskShift := 0;
    ADesc.MaskLineEnd := rileByteBoundary;
    ADesc.MaskBitOrder := riboBitsInOrder;

    Exit(True);
  end;

  // Format
  if IsBitmap
  then begin
    ADesc.Format := ricfGray;
  end
  else begin
    case Visual^.thetype of
      GDK_VISUAL_STATIC_GRAY:  ADesc.Format:=ricfGray;
      GDK_VISUAL_GRAYSCALE:    ADesc.Format:=ricfGray;
      GDK_VISUAL_STATIC_COLOR: ADesc.Format:=ricfGray; // this is not really gray, but an index in a color map, but colormaps are not supported yet, so use gray
      GDK_VISUAL_PSEUDO_COLOR: ADesc.Format:=ricfGray;
      GDK_VISUAL_TRUE_COLOR:   ADesc.Format:=ricfRGBA;
      GDK_VISUAL_DIRECT_COLOR: ADesc.Format:=ricfRGBA;
    else
      DebugLn('TGtkWidgetSet.GetWindowRawImageDescription unknown Visual type ',
              dbgs(Integer(Visual^.thetype)));
      Exit(False);
    end;
  end;

  // Palette
  if not IsBitmap
  and (Visual^.thetype in [GDK_VISUAL_GRAYSCALE,
                           GDK_VISUAL_STATIC_COLOR,GDK_VISUAL_PSEUDO_COLOR])
  then begin
    // has palette
    // ToDo
    ADesc.PaletteColorCount:=0;
  end;
                           
  // Depth
  if IsBitmap
  then ADesc.Depth := 1
  else ADesc.Depth := Visual^.Depth;

  if IsBitmap or (Visual^.byte_order = GDK_MSB_FIRST)
  then ADesc.ByteOrder := riboMSBFirst
  else ADesc.ByteOrder := riboLSBFirst;
  
  ADesc.LineOrder := riloTopToBottom;

  case ADesc.Depth of
    0..8:   ADesc.BitsPerPixel := ADesc.Depth;
    9..16:  ADesc.BitsPerPixel := 16;
    17..32: ADesc.BitsPerPixel := 32;
  else
    ADesc.BitsPerPixel := 64;
  end;

  if IsBitmap
  then begin
    ADesc.LineEnd  := rileByteBoundary;
    ADesc.RedPrec  := 1;
    ADesc.RedShift := 0;
  end
  else begin
    // Try retrieving the lineend
    Image := gdk_image_new(GDK_IMAGE_NORMAL, Visual, 1, 1);
    if Image = nil
    then begin
      DebugLn('TGtkWidgetSet.GetWindowRawImageDescription testimage creation failed ');
      Exit(False);
    end;
    try
      // the minimum alignment we can detect is bpp
      // that is no problem since a line consists of n x bytesperpixel bytes
      case Image^.bpl of
        1: ADesc.LineEnd := rileByteBoundary;
        2: ADesc.LineEnd := rileWordBoundary;
        4: ADesc.LineEnd := rileDWordBoundary;
        8: ADesc.LineEnd := rileQWordBoundary;
      else
        DebugLn('TGtkWidgetSet.GetWindowRawImageDescription Unknown line end: %d', [Image^.bpl]);
        Exit(False);
      end;
    finally
      gdk_image_destroy(Image);
      Image := nil;
    end;

    ADesc.RedPrec := Visual^.red_prec;
    ADesc.RedShift := Visual^.red_shift;
    ADesc.GreenPrec := Visual^.green_prec;
    ADesc.GreenShift := Visual^.green_shift;
    ADesc.BluePrec := Visual^.blue_prec;
    ADesc.BlueShift := Visual^.blue_shift;

    ADesc.MaskBitsPerPixel := 1;
    ADesc.MaskShift := 0;
    ADesc.MaskLineEnd := rileByteBoundary;
    ADesc.MaskBitOrder := riboBitsInOrder;
  end;

  {$IFDEF VerboseRawImage}
  //DebugLn('TGtkWidgetSet.GetWindowRawImageDescription A ',RawImageDescriptionAsString(Desc));
  {$ENDIF}

  Result := True;
end;

function TGtkWidgetSet.RawImage_FromDrawable(out ARawImage: TRawImage; ADrawable, AAlpha: PGdkDrawable; const ARect: TRect): boolean;
var
  ADesc: TRawImageDescription absolute ARawImage.Description;
  
  function GetFromPixbuf(const ARect: TRect): Boolean;
  var
    Pixbuf: PGdkPixbuf;
    pixels: pguchar;
  begin
    // create pixbuf with alpha channel first
    Pixbuf := CreatePixbufFromDrawable(ADrawable, nil, True, ARect.Left, ARect.Top, 0, 0, ADesc.Width, ADesc.Height);
    try
      pixels := gdk_pixbuf_get_pixels(Pixbuf);
      
      ARawImage.DataSize := PtrUInt(gdk_pixbuf_get_rowstride(Pixbuf)) * PtrUInt(ADesc.Height);
      ReAllocMem(ARawImage.Data, ARawImage.DataSize);
      if ARawImage.DataSize > 0 then
        System.Move(pixels^, ARawImage.Data^, ARawImage.DataSize);

      //DbgDumpPixmap(ADrawable, 'RawImage_FromDrawable - image');
      //DbgDumpBitmap(AAlpha, 'RawImage_FromDrawable - alpha');
      //DbgDumpPixbuf(Pixbuf, 'RawImage_FromDrawable - pixbuf');
    finally
      gdk_pixbuf_unref(Pixbuf);
    end;
    
    Result := RawImage_SetAlpha(ARawImage, AAlpha, ARect);;
  end;

  function GetFromImage(const ARect: TRect): Boolean;
  var
    Image: PGdkImage;
  begin
    Image := gdk_image_get(ADrawable, ARect.Left, ARect.Top, ADesc.Width, ADesc.Height);
    if Image = nil
    then begin
      DebugLn('WARNING: TGtkWidgetSet.RawImage_FromDrawable: gdk_image_get failed');
      exit(False);
    end;

    try
      {$ifdef RawimageConsistencyCheks}
      // consistency checks
      if Description.Depth <> Image^.Depth then
        RaiseGDBException('ARawImage.Description.Depth<>Image^.Depth '+IntToStr(ADesc.Depth)+'<>'+IntToStr(Image^.Depth));
      if Description.BitsPerPixel <> GetPGdkImageBitsPerPixel(Image) then
        RaiseGDBException('NewRawImage.Description.BitsPerPixel<>AnImage^.bpp');
      {$endif}

      ARawImage.DataSize := PtrUInt(Image^.bpl) * PtrUInt(Image^.Height);
      {$IFDEF VerboseRawImage}
      DebugLn('TGtkWidgetSet.RawImage_FromDrawable: G Width=',dbgs(Image^.Width),' Height=',dbgs(Image^.Height),
        ' BitsPerPixel=',dbgs(ADesc.BitsPerPixel),' bpl=',dbgs(Image^.bpl));
      {$ENDIF}

      // copy data
      ADesc.Width := Image^.Width;
      ADesc.Height := Image^.Height;
      ReAllocMem(ARawImage.Data, ARawImage.DataSize);
      if ARawImage.DataSize > 0
      then begin
        System.Move(Image^.Mem^, ARawImage.Data^, ARawImage.DataSize);
        if Image^.Depth = 1
        then CheckGdkImageBitOrder(Image, ARawImage.Data, ARawImage.DataSize);
      end;

      {$IFDEF VerboseRawImage}
      DebugLn('TGtkWidgetSet.RawImage_FromDrawable: H ',
        ' Width=',dbgs(ADesc.Width),
        ' Height=',dbgs(ADesc.Height),
        ' Depth=',dbgs(ADesc.Depth),
        ' DataSize=',dbgs(ARawImage.DataSize));
      {$ENDIF}
    finally
      gdk_image_destroy(Image);
    end;
    
    Result := True;
  end;
  
var
  R: TRect;
  UseAlpha: Boolean;
begin
  Result := False;
  if ADrawable = nil then
    RaiseGDBException('TGtkWidgetSet.RawImage_FromDrawable');
    
  ARawImage.Init;

  UseAlpha := AAlpha <> nil;

  // get raw image description
  if not RawImage_DescriptionFromDrawable(ADesc, ADrawable, UseAlpha)
  then begin
    DebugLn('WARNING: TGtkWidgetSet.RawImage_FromDrawable: RawImage_DescriptionFromDrawable failed ');
    Exit;
  end;

  // get intersection
  {$IFDEF VerboseRawImage}
  DebugLn('TGtkWidgetSet.RawImage_FromDrawable: Intersect ARect=',
    dbgs(ARect.Left),',',dbgs(ARect.Top),',',dbgs(ARect.Right),',',dbgs(ARect.Bottom),
    ' DevW=',dbgs(ADesc.Width),' DevH=',dbgs(ADesc.Height));
  {$ENDIF}

  R := ARect;
  IntersectRect(R, ARect, Rect(0, 0, ADesc.Width, ADesc.Height));
  ADesc.Width := R.Right - R.Left;
  ADesc.Height := R.Bottom - R.Top;
  
  {$IFDEF VerboseRawImage}
  DebugLn('TGtkWidgetSet.RawImage_FromDrawable get image ',
    dbgs(R.Left),',',dbgs(R.Top),',',dbgs(R.Right),',',dbgs(R.Bottom),
    ' GDKWindow=',DbgS(ADrawable));
  {$ENDIF}
  if (ADesc.Width <= 0) or (ADesc.Height <= 0)
  then begin
    //DebugLn('WARNING: TGtkWidgetSet.GetRawImageFromGdkWindow Intersection empty');
    exit;
  end;
  

  if UseAlpha
  then Result := GetFromPixbuf(R)
  else Result := GetFromImage(R);
end;

function TGTKWidgetSet.RawImage_SetAlpha(var ARawImage: TRawImage; AAlpha: PGdkPixmap; const ARect: TRect): boolean;
// ARect must have the same dimension as the rawimage
var
  ADesc: TRawImageDescription absolute ARawImage.Description;

  procedure SetAlpha_32_1(AImage: PGdkImage; AWidth, AHeight: Cardinal);
  var
    SrcPtr, DstPtr, SrcLinePtr, DstLinePtr: PByte;
    DstPtr32: PDWord absolute DstPtr;
    SrcBytesPerLine: Integer;
    DstBytesPerLine: Integer;
    SrcBit, SrcStartBit, ShiftInc: ShortInt;
    DstMask: DWord;
    DstSet: DWord;
    X, Y: Cardinal;
    {$ifdef hasx}
    XImage: PXimage;
    {$endif}
  begin
    SrcLinePtr := AImage^.mem;
    SrcBytesPerLine := AImage^.bpl;
    DstLinePtr := ARawImage.Data;
    DstBytesPerLine := ARawImage.Description.BytesPerLine;

    if ADesc.ByteOrder = DefaultByteOrder
    then DstSet := (not ($FFFFFFFF shl ADesc.AlphaPrec)) shl ADesc.AlphaShift
    else DstSet := (not ($FFFFFFFF shr ADesc.AlphaPrec)) shr ADesc.AlphaShift;
    DstMask  := not DstSet;
    
    // bit order for X11 can be normal or reversed order, win32 and direct_fb
    // is constant in reversed order
    SrcStartBit := 7;
    ShiftInc := -1;
    {$ifdef HasX}
    XImage := gdk_x11_image_get_ximage(AImage);
    if XImage^.bitmap_bit_order = LSBFirst
    then begin
      SrcStartBit := 0;
      ShiftInc := 1;
    end;
    {$endif}

    for Y := 0 to AHeight - 1 do
    begin
      SrcBit := SrcStartBit;
      SrcPtr := SrcLinePtr;
      DstPtr := DstLinePtr;
      for x := 0 to AWidth - 1 do
      begin
        if SrcPtr^ and (1 shl SrcBit) = 0
        then DstPtr32^ := DstPtr32^ and DstMask
        else DstPtr32^ := (DstPtr32^ and DstMask) or DstSet;
        Inc(DstPtr32);
        SrcBit := SrcBit + ShiftInc;
        if SrcBit and $F8 <> 0
        then begin
          SrcBit := SrcBit and 7;
          Inc(SrcPtr);
        end;
      end;
      Inc(SrcLinePtr, SrcBytesPerLine);
      Inc(DstLinePtr, DstBytesPerLine);
    end;
  end;

  procedure SetAlpha_32_8(AImage: PGdkImage; AWidth, AHeight: Cardinal);
  var
    SrcPtr, DstPtr, SrcLinePtr, DstLinePtr: PByte;
    DstPtr32: PDWord absolute DstPtr;
    SrcBytesPerLine: Integer;
    DstBytesPerLine: Integer;
    DstMask: DWord;
    DstShift: Byte;
    X, Y: Cardinal;
  begin
    SrcLinePtr := AImage^.mem;
    SrcBytesPerLine := AImage^.bpl;
    DstLinePtr := ARawImage.Data;
    DstBytesPerLine := ARawImage.Description.BytesPerLine;

    DstMask := not (((1 shl ADesc.AlphaPrec) - 1) shl ADesc.AlphaShift);
    DstShift := ADesc.AlphaShift;

    for Y := 0 to AHeight - 1 do
    begin
      SrcPtr := SrcLinePtr;
      DstPtr := DstLinePtr;
      for x := 0 to AWidth - 1 do
      begin
        DstPtr32^ := (DstPtr32^ and DstMask) or (Cardinal(SrcPtr^) shl DstShift);
        Inc(DstPtr32);
        Inc(SrcPtr);
      end;
      Inc(SrcLinePtr, SrcBytesPerLine);
      Inc(DstLinePtr, DstBytesPerLine);
    end;
  end;
  
var
  Width, Height, H, W, D: cardinal;
  Image: PGdkImage;
begin
  Result := False;

  if ARawImage.Data = nil
  then begin
    {$ifdef RawimageConsistencyChecks}
    RaiseGDBException('TGTKWidgetSet.RawImage_SetAlpha RawImage.Data = nil');
    {$else}
    DebugLn('WARNING: TGTKWidgetSet.RawImage_SetAlpha RawImage.Data = nil');
    {$endif}
    Exit;
  end;

  if ADesc.AlphaPrec = 0
  then begin
    {$ifdef RawimageConsistencyChecks}
    RaiseGDBException('TGTKWidgetSet.RawImage_SetAlpha RawImage.Description.AlphaPrec = 0');
    {$else}
    DebugLn('WARNING: TGTKWidgetSet.RawImage_SetAlpha No alpha channel defined');
    {$endif}
    Exit;
  end;

  if AAlpha = nil
  then begin
    DebugLn('WARNING: TGTKWidgetSet.RawImage_SetAlpha Alpha = nil');
    Exit;
  end;
  
  {$ifdef gtk1}
  gdk_window_get_geometry(AAlpha, nil, nil, @W, @H, @D);
  {$else}
  gdk_drawable_get_size(AAlpha, @W, @H);
  D := gdk_drawable_get_depth(AAlpha);
  {$endif}
  if (D <> 1) and (D <> 8)
  then begin
    DebugLn('WARNING: TGTKWidgetSet.RawImage_SetAlpha: Only a Depth of 1 or 8 is supported. (depth=%d)', [D]);
    Exit;
  end;
  
  if (longint(W) < ARect.Right) or (longint(H) < ARect.Bottom)
  then begin
    DebugLn('WARNING: TGTKWidgetSet.RawImage_SetAlpha: Rect(%d,%d %d,%d) outside alpha pixmap(0,0 %d,%d)', [ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, W, H]);
    Exit;
  end;

  Width := ARect.Right - ARect.Left;
  Height := ARect.Bottom - ARect.Top;

  if Width <> ADesc.Width
  then begin
    {$ifdef RawimageConsistencyChecks}
    RaiseGDBException('TGTKWidgetSet.RawImage_SetAlpha: Width <> RawImage.Description.Width');
    {$else}
    DebugLn('WARNING: TGTKWidgetSet.RawImage_SetAlpha: Width(=%d) <> RawImage.Description.Width(=%d)', [Width, ADesc.Width]);
    {$endif}
    Exit;
  end;

  if Height <> ADesc.Height
  then begin
    {$ifdef RawimageConsistencyChecks}
    RaiseGDBException('TGTKWidgetSet.RawImage_SetAlpha: Height <> RawImage.Description.Height');
    {$else}
    DebugLn('WARNING: TGTKWidgetSet.RawImage_SetAlpha: Height(=%d) <> RawImage.Description.Height(=%d)', [Height, ADesc.Height]);
    {$endif}
    Exit;
  end;

  // get gdk_image from gdkbitmap
  Image := gdk_image_get(AAlpha, ARect.Left, ARect.Top, Width, Height);
  if Image = nil
  then begin
    DebugLn('WARNING: TGtkWidgetSet.RawImage_SetAlpha: gdk_image_get failed');
    Exit;
  end;
  
  try
    case ADesc.BitsPerPixel of
      32: begin
        if D = 1
        then SetAlpha_32_1(Image, Width, Height)
        else SetAlpha_32_8(Image, Width, Height);
      end;
    else
      DebugLn('WARNING: TGtkWidgetSet.RawImage_SetAlpha: RawImage.Description.BitsPerPixel=%d not supported', [ADesc.BitsPerPixel]);
      Exit;
    end;
    
  finally
    gdk_image_destroy(Image);
  end;

  Result:=true;
end;

function TGTKWidgetSet.RawImage_AddMask(var ARawImage: TRawImage; AMask: PGdkBitmap; const ARect: TRect): boolean;
// ARect must have the same dimension as the rawimage

var
  ADesc: TRawImageDescription absolute ARawImage.Description;
  Width, Height, H: longint;
  Image: PGdkImage;
  BytesPerLine: Integer;
  SrcPtr, DstPtr: PByte;
  R: TRect;
begin
  Result := False;
  
  if ARawImage.Mask <> nil
  then begin
    {$ifdef RawimageConsistencyChecks}
    RaiseGDBException('TGTKWidgetSet.RawImage_AddMask RawImage.Mask <> nil');
    {$else}
    DebugLn('WARNING: TGTKWidgetSet.RawImage_AddMask RawImage.Mask <> nil');
    {$endif}
    Exit;
  end;
  
  if AMask = nil
  then begin
    DebugLn('WARNING: TGTKWidgetSet.RawImage_AddMask AMask = nil');
    Exit;
  end;

  R := ARect;
  Width := Min(ADesc.Width,  ARect.Right - ARect.Left);
  Height := Min(ADesc.Height, ARect.Bottom - ARect.Top);
  R.Right := R.Left + Width;
  R.Bottom := R.Top + Height;

  if cardinal(Width) <> ADesc.Width
  then begin
    {$ifdef RawimageConsistencyChecks}
    RaiseGDBException('TGTKWidgetSet.RawImage_AddMask: Width <> RawImage.Description.Width');
    {$else}
    DebugLn('WARNING: TGTKWidgetSet.RawImage_AddMask: Width(=%d) <> RawImage.Description.Width(=%d)', [Width, ADesc.Width]);
    {$endif}
    Exit;
  end;

  if cardinal(Height) <> ADesc.Height
  then begin
    {$ifdef RawimageConsistencyChecks}
    RaiseGDBException('TGTKWidgetSet.RawImage_AddMask: Height <> RawImage.Description.Height');
    {$else}
    DebugLn('WARNING: TGTKWidgetSet.RawImage_AddMask: Height(=%d) <> RawImage.Description.Height(=%d)', [Height, ADesc.Height]);
    {$endif}
    Exit;
  end;

  // get gdk_image from gdkbitmap
  Image := gdk_image_get(AMask, ARect.Left, ARect.Top, Width, Height);
  if Image = nil
  then begin
    DebugLn('WARNING: TGtkWidgetSet.RawImage_AddMask: gdk_image_get failed');
    Exit;
  end;

  try
    {$IFDEF VerboseRawImage}
    DebugLn('TGtkWidgetSet.RawImage_AddMask: A BytesPerLine=',dbgs(Image^.bpl),
      ' theType=',dbgs({$IFDEF Gtk1}Image^.thetype{$ELSE}ord(Image^._type){$ENDIF}),
      ' depth=',dbgs(Image^.depth),' AnImage^.bpp=',dbgs(Image^.bpp));
    DebugLn('RawImage=', ARawImage.Description.AsString);
    {$ENDIF}

    // See also GetWindowRawImageDescription
    ADesc.MaskBitsPerPixel := GetGdkImageBitsPerPixel(Image);
    ADesc.MaskLineEnd := rileByteBoundary;// gdk_bitmap_create_from_data expects rileByteBoundary
    BytesPerLine := GetBytesPerLine(ADesc.Width, ADesc.MaskBitsPerPixel, ADesc.MaskLineEnd);
    ARawImage.MaskSize := PtrUInt(BytesPerLine) * PtrUInt(Height);

    ReAllocMem(ARawImage.Mask, ARawImage.MaskSize);
    if ARawImage.MaskSize > 0
    then begin
      // copy data
      if BytesPerLine = Image^.bpl
      then begin
        // we can copy all in one go
        System.Move(Image^.Mem^, ARawImage.Mask^, ARawImage.MaskSize);
      end
      else begin
        // copy line by line
        SrcPtr := Image^.Mem;
        DstPtr := ARawImage.Mask;
        H := Height;
        while H > 0 do
        begin
          System.Move(SrcPtr^, DstPtr^, BytesPerLine);
          Inc(SrcPtr, Image^.bpl);
          Inc(DstPtr, BytesPerLine);
          Dec(H);
        end;
      end;
      CheckGdkImageBitOrder(Image, ARawImage.Mask, ARawImage.MaskSize);
    end;


    {$IFDEF VerboseRawImage}
    {DebugLn('TGtkWidgetSet.GetRawImageMaskFromGdkBitmap H ',
      ' Width=',dbgs(ARawImage.Description.Width),
      ' Height=',dbgs(ARawImage.Description.Height),
      ' AlphaBitsPerPixel=',dbgs(ARawImage.Description.AlphaBitsPerPixel),
      ' MaskSize=',dbgs(ARawImage.MaskSize));}
    {$ENDIF}
  finally
    gdk_image_destroy(Image);
  end;

  Result:=true;
end;

{------------------------------------------------------------------------------
  Function: TGtkWidgetSet.StretchCopyArea
  Params:  DestDC:                The destination devicecontext
           X, Y:                  The left/top corner of the destination rectangle
           Width, Height:         The size of the destination rectangle
           SrcDC:                 The source devicecontext
           XSrc, YSrc:            The left/top corner of the source rectangle
           SrcWidth, SrcHeight:   The size of the source rectangle
           Mask:                  An optional mask
           XMask, YMask:          Only used if Mask<>nil
           Rop:                   The raster operation to be performed
  Returns: True if succesful

  The StretchBlt function copies a bitmap from a source rectangle into a
  destination rectangle using the specified raster operation. If needed, it
  resizes the bitmap to fit the dimensions of the destination rectangle.
  Sizing is done according to the stretching mode currently set in the
  destination device context.
  If SrcDC contains a mask the pixmap will be copied with this transparency.

  ToDo:
    Mirroring
    Extended NonDrawable support (Image, Bitmap, etc)
    Scale mask
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.StretchCopyArea(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer;
  Mask: HBITMAP; XMask, YMask: Integer;
  Rop: Cardinal): Boolean;
var
  SrcDevContext: TGtkDeviceContext absolute SrcDC;
  DstDevContext: TGtkDeviceContext absolute DestDC;
  TempPixmap: PGdkPixmap;
  TempMaskBitmap: PGdkBitmap;
  SizeChange, ROpIsSpecial: Boolean;

  function ScaleAndROP(DestGC: PGDKGC;
    Src: PGDKDrawable; SrcPixmap: PGdkPixmap; SrcMaskBitmap: PGdkBitmap): Boolean;
  var
    Depth: Integer;
    ScaleMethod: TGdkInterpType;
    ShrinkWidth, ShrinkHeight: Boolean;
    GC: PGDKGC;
  begin
    {$IFDEF VerboseStretchCopyArea}

    DebugLn('ScaleAndROP START DestGC=',DbgS(DestGC),
      ' SrcPixmap=',DbgS(SrcPixmap),
      ' SrcMaskPixmap=',DbgS(SrcMaskPixmap));
    {$ENDIF}
    Result := False;

    if DestGC = nil
    then begin
      DebugLn('WARNING: [TGtkWidgetSet.StretchCopyArea] Uninitialized DestGC');
      exit;
    end;

    // create a temporary graphic context for the scale and raster operations
    // copy the destination GC values into the temporary GC
    GC := gdk_gc_new(DstDevContext.Drawable);
    gdk_gc_copy(GC, DestGC);

    // clear any previous clipping in the temporary GC
    gdk_gc_set_clip_region(GC, nil);
    gdk_gc_set_clip_rectangle(GC, nil);

    if SizeChange
    then begin
      {$IFDEF VerboseStretchCopyArea}
      Depth:=gdk_visual_get_system^.Depth;
      DebugLn('ScaleAndROP Scaling buffer: '+dbgs(Width),' x '+dbgs(Height),' x '+dbgs(Depth));
      {$ENDIF}

      // calculate ScaleMethod
      {$note use SetStretchBltMode(dc, mode) here}
      //GDKPixbuf Scaling is not done in the same way as Windows
      //but by rights ScaleMethod should really be chosen based
      //on the destination device's internal flag
      {GDK_INTERP_NEAREST,GDK_INTERP_TILES,
      GDK_INTERP_BILINEAR,GDK_INTERP_HYPER);}

      ShrinkWidth := Width < SrcWidth;
      ShrinkHeight := Height < SrcHeight;
      if ShrinkWidth and ShrinkHeight
      then ScaleMethod := GDK_INTERP_TILES
      else
        if ShrinkWidth or ShrinkHeight
        then ScaleMethod := GDK_INTERP_BILINEAR//GDK_INTERP_HYPER
        else ScaleMethod := GDK_INTERP_BILINEAR;

      // Scale the src part to a temporary pixmap with the size of the
      // destination rectangle

      Result := ScalePixmapAndMask(GC, ScaleMethod,
                            SrcPixmap, XSrc, YSrc, SrcWidth, SrcHeight,
                            nil, SrcMaskBitmap,
                            Width, Height, TempPixmap, TempMaskBitmap);
      if not Result
      then DebugLn('WARNING: ScaleAndROP ScalePixmap for pixmap failed');
    end
    else begin
      if ROpIsSpecial
      then begin
        // no scaling, but special ROp

        Depth:=gdk_visual_get_system^.Depth;
        {$IFDEF VerboseStretchCopyArea}
        DebugLn('ScaleAndROP Creating rop buffer: '+dbgs(Width),' x '+dbgs(Height),' x '+dbgs(Depth));
        {$ENDIF}
        TempPixmap := gdk_pixmap_new(nil,SrcWidth,SrcHeight,Depth);
        gdk_window_copy_area(TempPixmap, GC, 0, 0,
           Src, XSrc, YSrc, SrcWidth, SrcHeight);
      end;
      Result := True;
    end;

    // set raster operation in the destination GC
    if Result
    then SetGCRasterOperation(DestGC, ROP);

    gdk_gc_unref(GC);
  end;

  procedure ROPFillBuffer(DC : hDC);
  var
    OldCurrentBrush: PGdiObject;
    Brush : hBrush;
  begin
    if TempPixmap = nil then exit;

    if not ((ROp=WHITENESS) or (ROp=BLACKNESS) or (ROp=DSTINVERT)) then Exit;

    {$IFDEF VerboseStretchCopyArea}
    DebugLn('ROPFillBuffer ROp='+dbgs(ROp));
    {$ENDIF}
    with TGtkDeviceContext(DC) do
    begin
      // Temporarily hold the old brush to
      // replace it with the given brush
      OldCurrentBrush := CurrentBrush;
      if ROP = WHITENESS
      then
        Brush := GetStockObject(WHITE_BRUSH)
      else
        Brush := GetStockObject(BLACK_BRUSH);
      CurrentBrush := PGdiObject(Brush);
      SelectedColors := dcscBrush;

      if not IsNullBrush
      then begin
        gdk_draw_rectangle(TempPixmap, GC, 1, 0, 0, Width, Height);
      end;
      // Restore current brush
      CurrentBrush := OldCurrentBrush;
    end;
  end;

  function SrcDevBitmapToDrawable: Boolean;
  var
    SrcPixmap: PGdkPixmap;
    MskBitmap: PGdkBitmap;
    ClipMask: PGdkBitmap;
    SrcGDIBitmap: PGdiObject;
  begin
    Result:=true;
    {$IFDEF VerboseStretchCopyArea}
    DebugLn('SrcDevBitmapToDrawable Start');
    {$ENDIF}

    SrcGDIBitmap := SrcDevContext.CurrentBitmap;
    if SrcGDIBitmap = nil
    then begin
      DebugLn('SrcDevBitmapToDrawable NOTE: SrcDevContext.CurrentBitmap=nil');
      exit;
    end;

    SrcPixmap := SrcGDIBitmap^.GDIPixmapObject.Image;
    MskBitmap := CreateGdkMaskBitmap(HBITMAP(PtrUInt(SrcGDIBitmap)), Mask);
    
    {$IFDEF VerboseStretchCopyArea}
    DebugLn('SrcDevBitmapToDrawable SrcPixmap=[',GetWindowDebugReport(SrcPixmap),']',
      ' MaskPixmap=[',GetWindowDebugReport(MaskPixmap),']');
    {$ENDIF}

    if (MskBitmap = nil) and (not SizeChange) and (ROP=SRCCOPY)
    then begin
      // simply copy the area
      {$IFDEF VerboseStretchCopyArea}
      DebugLn('SrcDevBitmapToDrawable Simple copy');
      {$ENDIF}
      gdk_window_copy_area(DstDevContext.Drawable, DstDevContext.GC, X, Y,
        SrcPixmap, XSrc, YSrc, Width, Height);
      Exit;
    end;


    // perform raster operation and scaling into Scale and fGC
    DstDevContext.SelectedColors := dcscCustom;
    if not ScaleAndROP(DstDevContext.GC, SrcDevContext.Drawable, SrcPixmap, MskBitmap)
    then begin
      DebugLn('WARNING: SrcDevBitmapToDrawable: ScaleAndROP failed');
      Exit;
    end;
    
    {$IFDEF VerboseStretchCopyArea}
    DebugLn('SrcDevBitmapToDrawable TempPixmap=',DbgS(TempPixmap),' TempMaskPixmap=',DbgS(TempMaskBitmap));
    {$ENDIF}
    if TempPixmap <> nil
    then begin
      SrcPixmap := TempPixmap;
      XSrc := 0;
      YSrc := 0;
      SrcWidth := Width;
      SrcHeight := Height;
    end;
    if TempMaskBitmap <> nil
    then begin
      MskBitmap := TempMaskBitmap;
      XMask := 0;
      YMask := 0;
    end;

    case ROP of
      WHITENESS, BLACKNESS :
        ROPFillBuffer(DestDC);
    end;

    {$IFDEF VerboseStretchCopyArea}
    DebugLn('SrcDevBitmapToDrawable ',
      ' SrcPixmap=',DbgS(SrcPixmap),
      ' XSrc='+dbgs(XSrc),' YSrc='+dbgs(YSrc),' SrcWidth='+dbgs(SrcWidth),' SrcHeight='+dbgs(SrcHeight),
      ' MaskPixmap=',DbgS(MaskPixmap),
      ' XMask='+dbgs(XMask),' YMask='+dbgs(YMask),
      '');
    {$ENDIF}

    // set clipping mask for transparency
    MergeClipping(DstDevContext, DstDevContext.GC, X, Y, Width, Height,
                  MskBitmap, XMask, YMask, ClipMask);

    // draw image
    {$IFDEF DebugGDK}BeginGDKErrorTrap;{$ENDIF}
    gdk_window_copy_area(DstDevContext.Drawable, DstDevContext.GC, X, Y,
      SrcPixmap, XSrc, YSrc, SrcWidth, SrcHeight);
    {$IFDEF DebugGDK}EndGDKErrorTrap;{$ENDIF}

    // unset clipping mask for transparency
    DstDevContext.ResetGCClipping;
    if ClipMask <> nil
    then gdk_bitmap_unref(ClipMask);

    // restore raster operation to SRCCOPY
    gdk_gc_set_function(DstDevContext.GC, GDK_Copy);

    Result:=True;
  end;

  function DrawableToDrawable: Boolean;
  begin
    {$IFDEF VerboseStretchCopyArea}
    DebugLn('DrawableToDrawable Start');
    {$ENDIF}
    Result:=SrcDevBitmapToDrawable;
  end;

  function PixmapToDrawable: Boolean;
  begin
    {$IFDEF VerboseStretchCopyArea}
    DebugLn('PixmapToDrawable Start');
    {$ENDIF}
    Result:=SrcDevBitmapToDrawable;
  end;

  function PixmapToBitmap: Boolean;
  begin
    DebugLn('WARNING: [TGtkWidgetSet.StretchCopyArea] PixmapToBitmap unimplemented!');
    Result:=false;
  end;

  function BitmapToPixmap: Boolean;
  begin
    DebugLn('WARNING: [TGtkWidgetSet.StretchCopyArea] BitmapToPixmap unimplemented!');
    Result:=false;
  end;

  function Unsupported: Boolean;
  begin
    DebugLn('WARNING: [TGtkWidgetSet.StretchCopyArea] Destination and/or Source unsupported!!');
    Result:=false;
  end;

  //----------
  function NoDrawableToNoDrawable: Boolean;
  begin
    Result := Unsupported;
    if SrcDevContext.CurrentBitmap = nil then Exit;
    if DstDevContext.CurrentBitmap = nil then Exit;

    case SrcDevContext.CurrentBitmap^.GDIBitmapType of
      gbBitmap:
        case DstDevContext.CurrentBitmap^.GDIBitmapType of
          gbBitmap: Result:=DrawableToDrawable;
          gbPixmap: Result:=BitmapToPixmap;
        end;
      gbPixmap:
        case DstDevContext.CurrentBitmap^.GDIBitmapType of
          gbBitmap: Result:=PixmapToBitmap;
          gbPixmap: Result:=DrawableToDrawable;
        end;
    end;
  end;

  function NoDrawableToDrawable: Boolean;
  begin
    Result := Unsupported;
    if SrcDevContext.CurrentBitmap = nil then Exit;
    
    case SrcDevContext.CurrentBitmap^.GDIBitmapType of
      gbBitmap: Result:=PixmapToDrawable;
      gbPixmap: Result:=PixmapToDrawable;
    end;
  end;

  function DrawableToNoDrawable: Boolean;
  begin
    Result := Unsupported;
    if DstDevContext.CurrentBitmap = nil then Exit;
    
    case DstDevContext.CurrentBitmap^.GDIBitmapType of
      gbBitmap: Result:=Unsupported;
      gbPixmap: Result:=Unsupported;
    end;
  end;

  procedure RaiseSrcDrawableNil;
  begin
    DebugLn(['RaiseSrcDrawableNil ',GetWidgetDebugReport(SrcDevContext.Widget)]);
    RaiseGDBException(Format('TGtkWidgetSet.StretchCopyArea SrcDC=%p Drawable=nil', [Pointer(SrcDevContext)]));
  end;

  procedure RaiseDestDrawableNil;
  begin
    RaiseGDBException(Format('TGtkWidgetSet.StretchCopyArea DestDC=%p Drawable=nil', [Pointer(DstDevContext)]));
  end;

var
  NewSrcWidth: Integer;
  NewSrcHeight: Integer;
  NewWidth: Integer;
  NewHeight: Integer;
  SrcDCOrigin: TPoint;
  DstDCOrigin: TPoint;
  SrcWholeWidth, SrcWholeHeight: integer;
  DstWholeWidth, DstWholeHeight: integer;
begin
  Result := IsValidDC(DestDC) and IsValidDC(SrcDC);
  {$IFDEF VerboseStretchCopyArea}
  DebugLn('StretchCopyArea Start '+dbgs(Result));
  {$ENDIF}
  if not Result then Exit;
  if (Width = 0) or ( Height = 0) then exit;
  if (SrcWidth = 0) or (SrcHeight = 0) then exit;

  SizeChange := (Width <> SrcWidth) or (Height <> SrcHeight);
  ROpIsSpecial := (ROp <> SRCCOPY);

  SrcDCOrigin := SrcDevContext.Offset;
  Inc(XSrc, SrcDCOrigin.X);
  Inc(YSrc, SrcDCOrigin.Y);
  if SrcDevContext.Drawable = nil then RaiseSrcDrawableNil;
  gdk_window_get_size(PGdkWindow(SrcDevContext.Drawable), @SrcWholeWidth, @SrcWholeHeight);


  DstDCOrigin := DstDevContext.Offset;
  Inc(X, DstDCOrigin.X);
  Inc(Y, DstDCOrigin.Y);
  if DstDevContext.Drawable = nil then RaiseDestDrawableNil;
  gdk_window_get_size(PGdkWindow(DstDevContext.Drawable), @DstWholeWidth, @DstWholeHeight);

  {$IFDEF VerboseStretchCopyArea}
  DebugLn('TGtkWidgetSet.StretchCopyArea BEFORE CLIPPING X='+dbgs(X),' Y='+dbgs(Y),' Width='+dbgs(Width),' Height='+dbgs(Height),
    ' XSrc='+dbgs(XSrc)+' YSrc='+dbgs(YSrc)+' SrcWidth='+dbgs(SrcWidth)+' SrcHeight='+dbgs(SrcHeight),
    ' SrcDrawable=',DbgS(TGtkDeviceContext(SrcDC).Drawable),
    ' SrcOrigin='+dbgs(SrcDCOrigin),
    ' DestDrawable='+DbgS(TGtkDeviceContext(DestDC).Drawable),
    ' DestOrigin='+dbgs(DestDCOrigin),
    ' Mask='+DbgS(Mask)+' XMask='+dbgs(XMask)+' YMask='+dbgs(YMask),
    ' SizeChange='+dbgs(SizeChange)+' ROpIsSpecial='+dbgs(ROpIsSpecial),
    ' DestWhole='+dbgs(DestWholeWidth)+','+dbgs(DestWholeHeight),
    ' SrcWhole='+dbgs(SrcWholeWidth)+','+dbgs(SrcWholeHeight),
    '');
  {$ENDIF}
  {$note use intersectrect here}
  if X >= DstWholeWidth then Exit;
  if Y >= DstWholeHeight then exit;
  if X + Width <= 0 then exit;
  if Y + Height <=0 then exit;
  if XSrc >= SrcWholeWidth then Exit;
  if YSrc >= SrcWholeHeight then exit;
  if XSrc + SrcWidth <= 0 then exit;
  if YSrc + SrcHeight <=0 then exit;

  // gdk does not allow copying areas, party laying out of bounds
  // -> clip

  // clip src to the left
  if (XSrc<0) then begin
    NewSrcWidth:=SrcWidth+XSrc;
    NewWidth:=((Width*NewSrcWidth) div SrcWidth);
    {$IFDEF VerboseStretchCopyArea}
    DebugLn('StretchCopyArea Cliping Src to left NewSrcWidth='+dbgs(NewSrcWidth),' NewWidth='+dbgs(NewWidth));
    {$ENDIF}
    if NewWidth = 0 then exit;
    inc(X, Width-NewWidth);
    if X >= DstWholeWidth then exit;
    XSrc:=0;
    SrcWidth := NewSrcWidth;
  end;

  // clip src to the top
  if (YSrc<0) then begin
    NewSrcHeight:=SrcHeight+YSrc;
    NewHeight:=((Height*NewSrcHeight) div SrcHeight);
    {$IFDEF VerboseStretchCopyArea}
    DebugLn('StretchCopyArea Cliping Src to top NewSrcHeight='+dbgs(NewSrcHeight),' NewHeight='+dbgs(NewHeight));
    {$ENDIF}
    if NewHeight = 0 then exit;
    inc(Y, Height - NewHeight);
    if Y >= DstWholeHeight then exit;
    YSrc:=0;
    SrcHeight := NewSrcHeight;
  end;

  // clip src to the right
  if (XSrc+SrcWidth>SrcWholeWidth) then begin
    NewSrcWidth:=SrcWholeWidth-XSrc;
    Width:=((Width*NewSrcWidth) div SrcWidth);
    {$IFDEF VerboseStretchCopyArea}
    DebugLn('StretchCopyArea Cliping Src to right NewSrcWidth='+dbgs(NewSrcWidth),' NewWidth='+dbgs(Width));
    {$ENDIF}
    if (Width=0) then exit;
    if (X+Width<=0) then exit;
    SrcWidth:=NewSrcWidth;
  end;

  // clip src to the bottom
  if (YSrc+SrcHeight>SrcWholeHeight) then begin
    NewSrcHeight:=SrcWholeHeight-YSrc;
    Height:=((Height*NewSrcHeight) div SrcHeight);
    {$IFDEF VerboseStretchCopyArea}
    DebugLn('StretchCopyArea Cliping Src to bottom NewSrcHeight='+dbgs(NewSrcHeight),' NewHeight='+dbgs(Height));
    {$ENDIF}
    if (Height=0) then exit;
    if (Y+Height<=0) then exit;
    SrcHeight:=NewSrcHeight;
  end;

  if Mask = 0
  then begin
    XMask := XSrc;
    YMask := YSrc;
  end;

  // mark temporary scaling/rop buffers as uninitialized
  TempPixmap := nil;
  TempMaskBitmap := nil;

  {$IFDEF VerboseStretchCopyArea}
  write('TGtkWidgetSet.StretchCopyArea AFTER CLIPPING X='+dbgs(X)+' Y='+dbgs(Y)+' Width='+dbgs(Width)+' Height='+dbgs(Height),
    ' XSrc='+dbgs(XSrc),' YSrc='+dbgs(YSrc)+' SrcWidth='+dbgs(SrcWidth)+' SrcHeight='+dbgs(SrcHeight),
    ' SrcDrawable='+DbgS(SrcDevContext.Drawable),
    ' DestDrawable='+DbgS(DstDevContext.Drawable),
    ' Mask='+DbgS(Mask)+' XMask='+dbgs(XMask)+' YMask='+dbgs(YMask),
    ' SizeChange='+dbgs(SizeChange)+' ROpIsSpecial='+dbgs(ROpIsSpecial));
  write(' ROp=');
  case ROp of
    SRCCOPY     : DebugLn('SRCCOPY');
    SRCPAINT    : DebugLn('SRCPAINT');
    SRCAND      : DebugLn('SRCAND');
    SRCINVERT   : DebugLn('SRCINVERT');
    SRCERASE    : DebugLn('SRCERASE');
    NOTSRCCOPY  : DebugLn('NOTSRCCOPY');
    NOTSRCERASE : DebugLn('NOTSRCERASE');
    MERGECOPY   : DebugLn('MERGECOPY');
    MERGEPAINT  : DebugLn('MERGEPAINT');
    PATCOPY     : DebugLn('PATCOPY');
    PATPAINT    : DebugLn('PATPAINT');
    PATINVERT   : DebugLn('PATINVERT');
    DSTINVERT   : DebugLn('DSTINVERT');
    BLACKNESS   : DebugLn('BLACKNESS');
    WHITENESS   : DebugLn('WHITENESS');
  else
    DebugLn('???');
  end;
  {$ENDIF}

  {$note tode remove, earlier checks require drawable <> nil}
  if SrcDevContext.Drawable = nil
  then begin
    if DstDevContext.Drawable = nil
    then
      Result := NoDrawableToNoDrawable
    else
      Result := NoDrawableToDrawable;
  end
  else begin
    if DstDevContext.Drawable = nil
    then
      Result := DrawableToNoDrawable
    else
      Result := DrawableToDrawable;
  end;

  if TempPixmap <> nil
  then gdk_pixmap_unref(TempPixmap);
  if TempMaskBitmap <> nil
  then gdk_pixmap_unref(TempMaskBitmap);
end;

{------------------------------------------------------------------------------
  procedure TGtkWidgetSet.SetSelectionMode(Sender: TObject; Widget: PGtkWidget;
    MultiSelect, ExtendedSelect: boolean);
------------------------------------------------------------------------------}
procedure TGtkWidgetSet.SetSelectionMode(Sender: TObject; Widget: PGtkWidget;
  MultiSelect, ExtendedSelect: boolean);
{$IFdef GTK2}
begin
  DebugLn('TODO: TGtkWidgetSet.SetSelectionMode');
end;
{$Else}
var
  AControl: TWinControl;
  SelectionMode: TGtkSelectionMode;
  GtkList: PGtkList;
  GtkCList: PGtkCList;
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
    else begin
      SelectionMode:= GTK_SELECTION_BROWSE;
    end;
    case AControl.fCompStyle of

    csListBox, csCheckListBox:
      begin
        GtkList:=PGtkList(GetWidgetInfo(Widget, True)^.CoreWidget);
        if (GtkList^.selection=nil)
        and (SelectionMode=GTK_SELECTION_BROWSE) then
          SelectionMode:=GTK_SELECTION_SINGLE;
        gtk_list_set_selection_mode(GtkList,SelectionMode);
      end;

    csCListBox:
      begin
        GtkCList:=PGtkCList(GetWidgetInfo(Widget, True)^.CoreWidget);
        if (GtkCList^.selection=nil)
        and (SelectionMode=GTK_SELECTION_BROWSE) then
          SelectionMode:=GTK_SELECTION_SINGLE;
        gtk_clist_set_selection_mode(GtkCList,SelectionMode);
      end;

    else
      Assert (true, 'WARNING:[TGtkWidgetSet.IntSendMessage3] usage of LM_SETSELMODE unimplemented for actual component');
    end;
  end;
end;
{$EndIf}

{------------------------------------------------------------------------------
  procedure TGtkWidgetSet.BringFormToFront(Sender: TObject);
------------------------------------------------------------------------------}
procedure TGtkWidgetSet.BringFormToFront(Sender: TObject);
var
  AWindow: PGdkWindow;
  Widget: PGtkWidget;
begin
  Widget := PgtkWidget(TCustomForm(Sender).Handle);
  AWindow:=GetControlWindow(Widget);
  if AWindow<>nil then begin
    gdk_window_raise(AWindow);
  end;
end;

procedure TGtkWidgetSet.SetDesigning(AComponent: TComponent);
{var
  AWinControl: TWinControl absolute AComponent;
}
begin
  // change cursor
{
  Paul Ishenin:
    this will never happen

  if (AComponent is TWinControl) and (AWinControl.HandleAllocated) then
    TGtkWSWinControl(AWinControl.WidgetSetClass).SetCursor(AWinControl, Screen.Cursors[crDefault]);
}
end;

{------------------------------------------------------------------------------
  Method: TGtkWidgetSet.ResizeChild

  Params:  sender - the object which invoked this function
           Left,Top,Width,Height - new dimensions for the control
  Returns: Nothing

  *Note: Resize a child widget on the parents fixed widget
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.ResizeChild(Sender : TObject;
  Left, Top, Width, Height : Integer);
var
  LCLControl: TWinControl;
begin
  //DebugLn('[TGtkWidgetSet.ResizeChild] START ',TControl(Sender).Name,':',Sender.Classname,' Left=',Left,' Top=',Top,' Width=',Width,' Height=',Height);
  Assert(false, (Format('trace:  [TGtkWidgetSet.ResizeChild] %s --> Resize', [Sender.ClassNAme])));

  if Sender is TWinControl then begin
    LCLControl:=TWinControl(Sender);
    if LCLControl.HandleAllocated then begin
      ResizeHandle(LCLControl);
      //if (Sender is TCustomForm) then
      //if CompareText(Sender.ClassName,'TScrollBar')=0 then
      //  DebugLn(' FFF ResizeChild ',Sender.ClassName,' ',Left,',',Top,',',Width,',',Height);
    end;
  end;
  //DebugLn('[TGtkWidgetSet.ResizeChild] END ',Sender.Classname,' Left=',Left,' Top=',Top,' Width=',Width,' Height=',Height);
end;

{------------------------------------------------------------------------------
  Function: TGtkWidgetSet.SetCallbackEx
  Params: AMsg - message for which to set a callback
          AGTKObject - object to which callback will be send
          ALCLObject - for compatebility reasons provided, will be used when
                       AGTKObject = nil
          Direct - true: connect the signal to the AGTKObject
                   false: choose smart what gtkobject to use
  Returns:  nothing

  Applies a Message to the sender
 ------------------------------------------------------------------------------}
//TODO: remove ALCLObject when creation splitup is finished
procedure TGtkWidgetSet.SetCallbackEx(const AMsg: LongInt;
  const AGTKObject: PGTKObject; const ALCLObject: TObject; Direct: boolean);

  procedure ConnectSenderSignal(const AnObject:PGTKObject; const ASignal: PChar;
    const ACallBackProc: Pointer);
  begin
    ConnectSignal(AnObject,ASignal,ACallBackProc,ALCLObject);
  end;

  procedure ConnectSenderSignalAfter(const AnObject:PGTKObject;
    const ASignal: PChar; const ACallBackProc: Pointer);
  begin
    ConnectSignalAfter(AnObject,ASignal,ACallBackProc,ALCLObject);
  end;

  procedure ConnectSenderSignal(const AnObject:PGTKObject; const ASignal: PChar;
    const ACallBackProc: Pointer; const AReqSignalMask: TGdkEventMask);
  begin
    ConnectSignal(AnObject,ASignal,ACallBackProc,ALCLObject, AReqSignalMask);
  end;

  procedure ConnectSenderSignalAfter(const AnObject:PGTKObject;
    const ASignal: PChar; const ACallBackProc: Pointer;
    const AReqSignalMask: TGdkEventMask);
  begin
    ConnectSignalAfter(AnObject,ASignal,ACallBackProc,ALCLObject,
                       AReqSignalMask);
  end;

  procedure ConnectFocusEvents(const AnObject: PGTKObject);
  begin
    ConnectSenderSignal(AnObject, 'focus-in-event', @gtkFocusCB);
    ConnectSenderSignalAfter(AnObject, 'focus-in-event', @gtkFocusCBAfter);
    ConnectSenderSignal(AnObject, 'focus-out-event', @gtkKillFocusCB);
    ConnectSenderSignalAfter(AnObject, 'focus-out-event', @gtkKillFocusCBAfter);
  end;

  procedure ConnectKeyPressReleaseEvents(const AnObject: PGTKObject);
  begin
    //debugln('ConnectKeyPressReleaseEvents A ALCLObject=',DbgSName(ALCLObject));
    ConnectSenderSignal(AnObject,
      'key-press-event', @GTKKeyPress, GDK_KEY_PRESS_MASK);
    ConnectSenderSignalAfter(AnObject,
      'key-press-event', @GTKKeyPressAfter, GDK_KEY_PRESS_MASK);
    ConnectSenderSignal(AnObject,
      'key-release-event', @GTKKeyRelease, GDK_KEY_RELEASE_MASK);
    ConnectSenderSignalAfter(AnObject,
      'key-release-event', @GTKKeyReleaseAfter, GDK_KEY_RELEASE_MASK);
  end;
  
  function GetAdjustment(const gObject: PGTKObject; vertical: boolean):PGtkObject;
  var
    Scroll: PGtkObject;
  begin
    if Vertical then begin
      if ALCLObject is TScrollBar then
        result := PGtkObject(PgtkhScrollBar(gObject)^.Scrollbar.Range.Adjustment)
      else if (ALCLObject is TScrollBox) or (ALCLObject is TCustomForm) then begin
        Scroll := gtk_object_get_data(gObject, odnScrollArea);
        Result := PGtkObject(gtk_scrolled_window_get_vadjustment(
          PGTKScrolledWindow(Scroll)));
      end
      else
        Result := PGtkObject(gtk_scrolled_window_get_vadjustment(
          PGTKScrolledWindow(gObject)));

    end else begin
      if ALCLObject is TScrollBar then
        Result := PgtkObject(PgtkhScrollBar(gObject)^.Scrollbar.Range.Adjustment)
      else if (ALCLObject is TScrollBox)  or (ALCLObject is TCustomForm) then begin
        Scroll := gtk_object_get_data(gObject, odnScrollArea);
        Result := PgtkObject(gtk_scrolled_window_get_hadjustment(
          PGTKScrolledWindow(Scroll)));
      end
      else
        Result := PgtkObject(gtk_scrolled_window_get_hadjustment(
          PGTKScrolledWindow(gObject)));
    end;
  end;

var
  gObject, gFixed, gCore, gTemp, Adjustment: PGTKObject;
  Info: PWidgetInfo;
  gMain: PGtkObject;
  gMouse: PGtkObject;
begin
  //debugln('TGtkWidgetSet.SetCallback A ALCLObject=',DbgSName(ALCLObject),' AMsg=',dbgs(AMsg));
  if Direct then begin
    gMain:=AGTKObject;
    gCore:=AGTKObject;
    gFixed:=AGTKObject;
    gMouse:=AGTKObject;
  end else begin
    if AGTKObject = nil
    then gObject := ObjectToGTKObject(ALCLObject)
    else gObject := AGTKObject;
    if gObject = nil then Exit;

    // gFixed is the widget with the client area (e.g. TGroupBox, TCustomForm have this)
    gFixed := PGTKObject(GetFixedWidget(gObject));
    if gFixed = nil then
      gFixed := gObject;

    // gCore is the working widget (e.g. TListBox has a scrolling widget (=main widget) and a tree widget (=core widget))
    Info:=GetWidgetInfo(gObject, True);
    gCore:=PGtkObject(Info^.CoreWidget);
    gMain:=GetMainWidget(gObject);
    if (gMain=nil) then
      gMain:=gObject;
    if (gMain<>gObject) then
      DebugLn(['TGtkWidgetSet.SetCallback WARNING: gObject<>MainWidget ',DbgSName(ALCLObject)]);

    if (gFixed <> gMain) then
      gMouse := gFixed
    else
      gMouse := gCore;
      
    if gMouse=nil then
      DebugLn(['TGtkWidgetSet.SetCallback WARNING: gMouseWidget=nil ',DbgSName(ALCLObject)]);

    {$IFDEF GTK1}
    if ALCLObject is TCustomListBox then
      gMouse:=gMain;
    {$ELSE}
    if GTK_IS_FIXED(gMouse) and not gtk_fixed_get_has_window(PGtkFixed(gMouse)) then
    begin
      gTemp := PGtkObject(gtk_widget_get_parent(PGtkWidget(gMouse)));
      //DebugLn(gtk_type_name(g_object_type(gMouse)) + ' => ' + gtk_type_name(g_object_type(gTemp)));
      if GTK_IS_EVENT_BOX(gTemp) then
        gMouse := gTemp;
    end;
    {$ENDIF}
  end;
  //DebugLn(['TGtkWidgetSet.SetCallbackSmart MouseWidget=',GetWidgetDebugReport(PGtkWidget(gMouse))]);

  case AMsg of
    LM_SHOWWINDOW :
    begin
      ConnectSenderSignal(gObject, 'show', @gtkshowCB);
      ConnectSenderSignal(gObject, 'hide', @gtkhideCB);
    end;

    LM_DESTROY :
    begin
      //DebugLn(['TGtkWidgetSet.SetCallback ',DbgSName(ALCLObject)]);
      ConnectSenderSignal(gObject, 'destroy', @gtkdestroyCB);
    end;

    LM_CLOSEQUERY :
    begin
      ConnectSenderSignal(gObject, 'delete-event', @gtkdeleteCB);
    end;

    LM_ACTIVATE :
    begin
      if (ALCLObject is TCustomForm) and (TCustomForm(ALCLObject).Parent=nil)
      then begin
        ConnectSenderSignalAfter(gObject, 'focus-in-event', @gtkfrmactivateAfter);
        ConnectSenderSignalAfter(gObject, 'focus-out-event', @gtkfrmdeactivateAfter);
      end else if ALCLObject is TCustomMemo then
        ConnectSenderSignal(gCore, 'activate', @gtkactivateCB)
      else
        ConnectSenderSignal(gObject, 'activate', @gtkactivateCB);
    end;

    LM_ACTIVATEITEM :
    begin
      ConnectSenderSignal(gObject, 'activate-item', @gtkactivateCB);
    end;

    LM_CHANGED :
    begin
       if ALCLObject is TCustomTrackBar then
       begin
         ConnectSenderSignal(gtk_Object(
                   gtk_range_get_adjustment(GTK_RANGE(gObject))) ,
                      'value_changed', @gtkvaluechanged);
       end
       else
       if ALCLObject is TCustomNotebook then
         ConnectSenderSignal(gObject, 'switch_page', @gtkswitchpage)
       else
       if ALCLObject is TCustomCombobox then
         ConnectSenderSignal (PGtkObject(
                   PGtkCombo(gObject)^.entry), 'changed', @gtkchangedCB)
       else
       if ALCLObject is TCustomMemo then
         ConnectSenderSignal(gCore, 'changed', @gtkchanged_editbox)
       else if ALCLObject is TCustomCheckbox then
         ConnectSenderSignal(gObject, 'toggled', @gtktoggledCB)
     {$IFDEF GTK2}
       // in gtk2 callback signal of SpinEdit is 'value-changed' (in gtk1- 'changed')
       else if ALCLObject is TCustomFloatSpinEdit then
         ConnectSenderSignal(gObject, 'value-changed', @gtkchanged_editbox)
     {$ENDIF}
       else
         ConnectSenderSignal(gObject, 'changed', @gtkchanged_editbox);
    end;

    LM_CLICKED:
    begin
      ConnectSenderSignal(gObject, 'clicked', @gtkclickedCB);
    end;

    LM_CONFIGUREEVENT :
    begin
      ConnectSenderSignal(gObject, 'configure-event', @gtkconfigureevent);
    end;

    LM_DAYCHANGED :  //calendar
    Begin
      ConnectSenderSignal(gCore, 'day-selected', @gtkdaychanged);
      ConnectSenderSignal(gCore, 'day-selected-double-click', @gtkdaychanged);
    end;

    LM_PAINT :
    begin
      //DebugLn(['TGtkWidgetSet.SetCallback ',DbgSName(ALCLObject)]);
      {$Ifdef GTK1}
      //ConnectSenderSignal(gFixed, 'draw', @gtkDrawCB);
      ConnectSenderSignalAfter(gFixed, 'draw', @gtkDrawAfterCB);
      {$EndIf}
      {$Ifdef GTK2}
      ConnectSenderSignal(gFixed, 'expose-event', @GTKExposeEvent);
      {$EndIf}
      ConnectSenderSignalAfter(gFixed, 'expose-event', @GTKExposeEventAfter);
      ConnectSenderSignal(gFixed,'style-set', @GTKStyleChanged);
    end;

    LM_FOCUS :
    begin
      {$IFDEF GTK1}
      if (ALCLObject is TCustomComboBox) then begin
        ConnectFocusEvents(PgtkObject(PgtkCombo(gObject)^.entry));
        ConnectFocusEvents(PgtkObject(PgtkCombo(gObject)^.list));
      end else
      {$ENDIF}
      begin
        ConnectFocusEvents(gCore);
      end;
    end;

    LM_GRABFOCUS:
    begin
      ConnectSenderSignal(gObject, 'grab_focus', @gtkActivateCB);
    end;

    LM_KEYDOWN,
    LM_CHAR,
    LM_KEYUP,
    LM_SYSKEYDOWN,
    LM_SYSCHAR,
    LM_SYSKEYUP:
    begin
      //debugln('TGtkWidgetSet.SetCallback A KEY ALCLObject=',DbgSName(ALCLObject),' AMsg=',dbgs(AMsg));
      {$IFDEF GTK1}
      if (ALCLObject is TCustomComboBox) then begin
        ConnectKeyPressReleaseEvents(PgtkObject(PgtkCombo(gObject)^.entry));
      end
      else
      {$ENDIF}
      if (ALCLObject is TCustomForm) then begin
        ConnectKeyPressReleaseEvents(gObject);
      end;
      ConnectKeyPressReleaseEvents(gCore);
    end;

    LM_MONTHCHANGED:  //calendar
    Begin
      ConnectSenderSignal(gCore, 'month-changed', @gtkmonthchanged);
      ConnectSenderSignal(gCore, 'prev-month', @gtkmonthchanged);
      ConnectSenderSignal(gCore, 'next-month', @gtkmonthchanged);
    end;

    LM_PRESSED:
    begin
      Assert(False, 'Trace:OBSOLETE: [TGtkWidgetSet.SetCallback] LM_PRESSED');
      ConnectSenderSignal(gObject, 'pressed', @gtkpressedCB);
    end;

    LM_RELEASED:
    begin
      Assert(False, 'Trace:OBSOLETE: [TGtkWidgetSet.SetCallback] LM_RELEASED');
      ConnectSenderSignal(gObject, 'released', @gtkreleasedCB);
    end;

    LM_MOVECURSOR:
    begin
      ConnectSenderSignal(gFixed, 'move-cursor', @gtkmovecursorCB);
    end;

    LM_MOUSEMOVE:
    begin
      {$IFDEF GTK1}
      if (ALCLObject is TCustomComboBox) then
      begin
        ConnectSenderSignal(PgtkObject(PgtkCombo(gObject)^.entry),
          'motion-notify-event',
          @GTKMotionNotify, GDK_POINTER_MOTION_MASK);
        ConnectSenderSignalAfter(PgtkObject(PgtkCombo(gObject)^.entry),
          'motion-notify-event',
          @GTKMotionNotifyAfter, GDK_POINTER_MOTION_MASK);

        ConnectSenderSignal(PgtkObject(PgtkCombo(gObject)^.button),
          'motion-notify-event',
          @GTKMotionNotify, GDK_POINTER_MOTION_MASK);
        ConnectSenderSignalAfter(PgtkObject(PgtkCombo(gObject)^.button),
          'motion-notify-event',
          @GTKMotionNotifyAfter, GDK_POINTER_MOTION_MASK);
      end
      else
      {$ENDIF}
      begin
        ConnectSenderSignal(gMouse, 'motion-notify-event', @GTKMotionNotify,
                      GDK_POINTER_MOTION_MASK);
        ConnectSenderSignalAfter(gMouse, 'motion-notify-event',
                      @GTKMotionNotifyAfter, GDK_POINTER_MOTION_MASK);
      end;
    end;

    LM_LBUTTONDOWN,
    LM_RBUTTONDOWN,
    LM_MBUTTONDOWN,
    LM_MOUSEWHEEL :
    begin
      {$IFDEF GTK1}
      if (ALCLObject is TCustomComboBox) then
      begin
        ConnectSenderSignal(PgtkObject(PgtkCombo(gObject)^.entry),
              'button-press-event',  @gtkMouseBtnPress, GDK_BUTTON_PRESS_MASK);
        ConnectSenderSignalAfter(PgtkObject(PgtkCombo(gObject)^.entry),
              'button-press-event',  @gtkMouseBtnPressAfter,
              GDK_BUTTON_PRESS_MASK);
        ConnectSenderSignal(PgtkObject(PgtkCombo(gObject)^.button) ,
              'button-press-event', @gtkMouseBtnPress, GDK_BUTTON_PRESS_MASK);
        ConnectSenderSignalAfter(PgtkObject(PgtkCombo(gObject)^.button) ,
              'button-press-event', @gtkMouseBtnPressAfter, GDK_BUTTON_PRESS_MASK);
        // Connecting the list seems to cause errors. Maybe we are returning the
        // wrong boolean in the callback func
        // ConnectSenderSignal(PgtkObject(PgtkCOmbo(gObject)^.list),
        //    'button-press-event',  @gtkMouseBtnPress, GDK_BUTTON_PRESS_MASK);
      end
      else
      {$ENDIF}
      begin
        ConnectSenderSignal(gMouse, 'button-press-event', @gtkMouseBtnPress,
                            GDK_BUTTON_PRESS_MASK);
        ConnectSenderSignalAfter(gMouse, 'button-press-event',
                                 @gtkMouseBtnPressAfter, GDK_BUTTON_PRESS_MASK);
        {$IFDEF Gtk2}
        ConnectSenderSignal(gMouse, 'scroll-event', @gtkMouseWheelCB,
                            GDK_SCROLL_MASK);
        {$ENDIF}
      end;
    end;

    LM_LBUTTONUP,
    LM_RBUTTONUP,
    LM_MBUTTONUP:
    begin
      {$IFDEF GTK1}
      if (ALCLObject is TCustomComboBox) then
      begin
        ConnectSenderSignal(PgtkObject(PgtkCombo(gObject)^.entry),
          'button-release-event',  @gtkMouseBtnRelease, GDK_BUTTON_RELEASE_MASK);
        ConnectSenderSignalAfter(PgtkObject(PgtkCombo(gObject)^.entry),
          'button-release-event',  @gtkMouseBtnReleaseAfter,
          GDK_BUTTON_RELEASE_MASK);
        ConnectSenderSignal(PgtkObject(PgtkCombo(gObject)^.button) ,
          'button-release-event', @gtkMouseBtnRelease, GDK_BUTTON_RELEASE_MASK);
        ConnectSenderSignalAfter(PgtkObject(PgtkCombo(gObject)^.button) ,
          'button-release-event', @gtkMouseBtnReleaseAfter,
          GDK_BUTTON_RELEASE_MASK);
        // Connecting the list seems to cause errors. Maybe we are returning the
        // wrong boolean in the callback func
        //  ConnectSenderSignal(PgtkObject(PgtkCombo(gObject)^.list),
        //                'button-release-event',  @gtkMouseBtnRelease,
        //                GDK_BUTTON_RELEASE_MASK);
      end
      else
      {$ENDIF}
      begin
        ConnectSenderSignal(gMouse, 'button-release-event', @gtkMouseBtnRelease,
                      GDK_BUTTON_RELEASE_MASK);
        ConnectSenderSignalAfter(gMouse, 'button-release-event',
                           @gtkMouseBtnReleaseAfter,GDK_BUTTON_RELEASE_MASK);
      end;
    end;

    LM_ENTER :
    begin
      if ALCLObject is TCustomButton then
        ConnectSenderSignal(gObject, 'enter', @gtkenterCB)
      else
        ConnectSenderSignal(gObject, 'focus-in-event', @gtkFocusInNotifyCB); //TODO: check this focus in is mapped to focus
    end;

    LM_EXIT :
    begin
      if ALCLObject is TCustomButton then
        ConnectSenderSignal(gObject, 'leave', @gtkleaveCB)
      else
        ConnectSenderSignal(gObject, 'focus-out-event', @gtkFocusOutNotifyCB);
    end;

    LM_LEAVE :
    begin
      ConnectSenderSignal(gObject, 'leave', @gtkleaveCB);
    end;

    LM_WINDOWPOSCHANGED: //LM_SIZEALLOCATE, LM_RESIZE :
    begin
      ConnectSenderSignal(gObject, 'size-allocate', @gtksize_allocateCB);
      if gObject<>gFixed then begin
        ConnectSenderSignal(gFixed, 'size-allocate', @gtksize_allocate_client);
      end;
    end;

    LM_CHECKRESIZE :
    begin
      ConnectSenderSignal(gObject, 'check-resize', @gtkresizeCB);
    end;

    LM_INSERTTEXT :
    begin
      ConnectSenderSignal(gCore, 'insert-text', @gtkinserttext);
    end;

    LM_DELETETEXT :
    begin
      ConnectSenderSignal(gObject, 'delete-text', @gtkdeletetext);
    end;

    LM_SETEDITABLE :
    begin
      ConnectSenderSignal(gObject, 'set-editable', @gtkseteditable);
    end;

    LM_MOVEWORD :
    begin
      ConnectSenderSignal(gObject, 'move-word', @gtkmoveword);
    end;

    LM_MOVEPAGE :
    begin
      ConnectSenderSignal(gObject, 'move-page', @gtkmovepage);
    end;

    LM_MOVETOROW :
    begin
      ConnectSenderSignal(gObject, 'move-to-row', @gtkmovetorow);
    end;

    LM_MOVETOCOLUMN :
    begin
      ConnectSenderSignal(gObject, 'move-to-column', @gtkmovetocolumn);
    end;

    LM_MOUSEENTER:
    begin
      if gCore<>nil then
        ConnectSenderSignal(gCore, 'enter', @gtkEnterCB)
    end;

    LM_MOUSELEAVE:
    begin
      if gCore<>nil then
        ConnectSenderSignal(gCore, 'leave', @gtkLeaveCB)
    end;

    LM_KILLCHAR :
    begin
      ConnectSenderSignal(gObject, 'kill-char', @gtkkillchar);
    end;

    LM_KILLWORD :
    begin
      ConnectSenderSignal(gObject, 'kill-word', @gtkkillword);
    end;

    LM_KILLLINE :
    begin
      ConnectSenderSignal(gObject, 'kill-line', @gtkkillline);
    end;

    LM_CUTTOCLIP :
    begin
      if (ALCLObject is TCustomMemo) then
        ConnectSenderSignal(gCore, 'cut-clipboard', @gtkcuttoclip)
      else
        ConnectSenderSignal(gObject, 'cut-clipboard', @gtkcuttoclip);
    end;

    LM_COPYTOCLIP :
    begin
      if (ALCLObject is TCustomMemo) then
        ConnectSenderSignal(gCore, 'copy-clipboard', @gtkcopytoclip)
      else
        ConnectSenderSignal(gObject, 'copy-clipboard', @gtkcopytoclip);
    end;

    LM_PASTEFROMCLIP :
    begin
      if (ALCLObject is TCustomMemo) then
        ConnectSenderSignal(gCore, 'paste-clipboard', @gtkpastefromclip)
      else
        ConnectSenderSignal(gObject, 'paste-clipboard', @gtkpastefromclip);
    end;

    LM_HSCROLL:
    begin
      Adjustment := GetAdjustment(gObject, False);
      if Adjustment <> nil then
        ConnectSenderSignal(Adjustment, 'value-changed', @GTKHScrollCB);
    end;

    LM_VSCROLL:
    begin
      Adjustment := GetAdjustment(gObject, True);
      if Adjustment <> nil then
      begin
        ConnectSenderSignal(Adjustment, 'value-changed', @GTKVScrollCB);
        ConnectSenderSignal(Adjustment, 'value-changed', @GTKVScrollCB);
      end;
    end;

    LM_YEARCHANGED :  //calendar
    Begin
      ConnectSenderSignal(gCore, 'prev-year', @gtkyearchanged);
      ConnectSenderSignal(gCore, 'next-year', @gtkyearchanged);
    end;

    // Listview & Header control
    LM_COMMAND:
    begin
      if ALCLObject is TCustomComboBox then begin
        ConnectSenderSignalAfter(PgtkObject(PgtkCombo(gObject)^.popwin),
          'show', @gtkComboBoxShowAfter);
        ConnectSenderSignalAfter(PgtkObject(PgtkCombo(gObject)^.popwin),
          'hide', @gtkComboBoxHideAfter);
      end;
    end;

    LM_SelChange:
    begin
      if ALCLObject is TCustomListBox then begin
        ConnectSenderSignalAfter(PgtkObject(gCore),
          'selection_changed', @gtkListBoxSelectionChangedAfter);
      end else if ALCLObject is TCustomCombobox then begin
         ConnectSenderSignal (PGtkObject(PGtkCombo(gObject)^.list),
                              'unselect_child', @gtkListSelectChild);
      end;
    end;
    
    LM_DROPFILES:
      ConnectSenderSignal(gCore, 'drag_data_received', @GtkDragDataReceived);

(*
    LM_WINDOWPOSCHANGED:
    begin
      ConnectSenderSignal(gObject, 'size-allocate', @gtkSizeAllocateCB);
//      ConnectSenderSignal(gObject, 'move_resize', @gtkmoveresize);
    end;
*)
  else
    Assert(False, Format('Trace:ERROR:  Signal %d not found!', [AMsg]));
  end;
end;

procedure TGTKWidgetSet.SetCallbackDirect(const AMsg: LongInt;
  const AGTKObject: PGTKObject; const ALCLObject: TObject);
begin
  SetCallbackEx(AMsg,AGTKObject,ALCLObject,true);
end;

procedure TGTKWidgetSet.SetCallback(const AMsg: LongInt;
  const AGTKObject: PGTKObject; const ALCLObject: TObject);
begin
  SetCallbackEx(AMsg,AGTKObject,ALCLObject,false);
end;


{------------------------------------------------------------------------------
  Function: TGtkWidgetSet.RemoveCallBacks
  Params:   Widget
  Returns:  nothing

  Removes Call Back Signals from the Widget
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.RemoveCallbacks(Widget: PGtkWidget);
{$IFDEF Gtk1}
var
  MainWidget, ClientWidget, ImplWidget: PGtkWidget;
  WinWidgetInfo: PWinWidgetInfo;
{$ENDIF}
begin
  {$IFDEF Gtk1}
  MainWidget := Widget;
  if MainWidget = nil then Exit;
  if GtkWidgetIsA(Widget,GTK_MENU_ITEM_GET_TYPE) then exit;

  ClientWidget:=GetFixedWidget(MainWidget);
  WinWidgetInfo:=GetWidgetInfo(MainWidget,false);
  if WinWidgetInfo<>nil then
    ImplWidget:=WinWidgetInfo^.CoreWidget
  else
    ImplWidget:=nil;
  g_signal_handlers_destroy(PGtkObject(MainWidget));
  if (ClientWidget<>nil) and (ClientWidget<>MainWidget) then
    g_signal_handlers_destroy(PGtkObject(ClientWidget));
  if (ImplWidget<>nil)
  and (ImplWidget<>ClientWidget) and (ImplWidget<>MainWidget) then
    g_signal_handlers_destroy(PGtkObject(ImplWidget));
  {$ENDIF}
end;

{-------------------------------------------------------------------------------
  TGtkWidgetSet.DestroyLCLComponent
  Params: Sender: TObject

  Destroy the widget and all associated data
-------------------------------------------------------------------------------}
procedure TGtkWidgetSet.DestroyLCLComponent(Sender : TObject);
var
  handle: hwnd; // handle of sender
  Widget: PGtkWidget;
  APage: TCustomPage;
  NoteBookWidget: PGtkNotebook;
  GtkWindow: PGtkWidget;
begin
  Handle := HWnd(PtrUInt(ObjectToGtkObject(Sender)));
  if Handle=0 then exit;
  Widget:=PGtkWidget(Handle);
  if WidgetIsDestroyingHandle(Widget) then exit;
  SetWidgetIsDestroyingHandle(Widget);

  //DebugLn('TGtkWidgetSet.DestroyLCLComponent A ',GetWidgetClassName(Widget));

  // if one of its widgets has the focus then unfocus
  GtkWindow:=gtk_widget_get_toplevel(Widget);
  if GtkWidgetIsA(GtkWindow,GTK_TYPE_WINDOW)
  and (GetNearestLCLObject(PGtkWindow(GtkWindow)^.Focus_Widget)=Sender)
  then begin
    gtk_window_set_focus(PGtkWindow(GtkWindow),nil);
  end;

  if Sender is TControl then begin
    if Sender is TCustomPage then begin
      // a notebook always need at least one page
      // -> if this is the last page, then add a dummy page
      APage:=TCustomPage(Sender);
      if (APage.Parent<>nil) and APage.Parent.HandleAllocated
      and (APage.Parent is TCustomNoteBook) then begin
        NoteBookWidget:=PGtkNotebook(TCustomNoteBook(APage.Parent).Handle);
        if GetGtkNoteBookPageCount(NoteBookWidget)=1 then begin
          AddDummyNoteBookPage(NoteBookWidget);
          UpdateNoteBookClientWidget(TCustomNoteBook(APage.Parent));
        end;
      end;
    end;
  end
  else if Sender is TCommonDialog then begin
    DestroyCommonDialogAddOns(TCommonDialog(Sender));
  end;

  // destroy widget and properties
  DestroyConnectedWidget(Widget,false);

  // clean up unneeded containers
  if Sender is TMenuItem then begin
    DestroyEmptySubmenu(TMenuItem(Sender));
  end;

  // mouse click messages
  if LastLeft.Component=Sender then
    LastLeft:=EmptyLastMouseClick;
  if LastMiddle.Component=Sender then
    LastMiddle:=EmptyLastMouseClick;
  if LastRight.Component=Sender then
    LastRight:=EmptyLastMouseClick;
end;

procedure TGtkWidgetSet.DestroyConnectedWidget(Widget: PGtkWidget;
  CheckIfDestroying: boolean);
var
  FixWidget: PGtkWidget;
  {$IFNDef GTK2}
  Accelerators: PGSlist;
  AccelEntry : PGtkAccelEntry;
  {$Endif}
  QueueItem : TGtkMessageQueueItem;
  NextItem  : TGtkMessageQueueItem;
  MsgPtr: PMsg;
begin
  if CheckIfDestroying then begin
    if WidgetIsDestroyingHandle(Widget) then exit;
    SetWidgetIsDestroyingHandle(Widget);
  end;

  FixWidget:=GetFixedWidget(Widget);

  // Remove control accelerators - has to be done due to GTK+ bug?
  //DebugLn('TGtkWidgetSet.DestroyLCLComponent B  Widget=',GetWidgetDebugReport(Widget));
  {$IFDef GTK1}
  Accelerators:= gtk_accel_group_entries_from_object(PGtkObject(Widget));
  while Accelerators <> nil do begin
    AccelEntry:= Accelerators^.data;
    Accelerators:= Accelerators^.next;
    with AccelEntry^ do
      gtk_accel_group_remove(accel_group, accelerator_key, accelerator_mods,
                             PGtkObject(Widget));
  end;
  {$EndIf}
  ClearAccelKey(Widget);

  // untransient
  if GtkWidgetIsA(Widget,GTK_TYPE_WINDOW) then begin
    UntransientWindow(PGtkWindow(Widget));
  end;

  // callbacks
  RemoveCallbacks(Widget);

  // childs
  if GtkWidgetIsA(Widget,GTK_COMBO_GET_TYPE) then begin
    g_signal_handlers_destroy(PGtkObject(PGtkCombo(Widget)^.Entry));
    g_signal_handlers_destroy(PGtkObject(PGtkCombo(Widget)^.List));
    SetComboBoxText(PGtkCombo(Widget),nil);

    // MWE:
    // TODO: Check: Why is there widgetinfo on subwidgets ???
    FreeWidgetInfo(PGtkCombo(Widget)^.List);
    FreeWidgetInfo(PGtkCombo(Widget)^.Entry);
    FreeWidgetInfo(PGtkCombo(Widget)^.Button);
  end;

  // update mouse capturing
  if (MouseCaptureWidget=Widget) or (MouseCaptureWidget=FixWidget) then
    MouseCaptureWidget:=nil;

  // update clipboard widget
  if (ClipboardWidget=Widget) or (ClipboardWidget=FixWidget) then
  begin
    // clipboard widget destroyed
    if (Application<>nil) and (Application.MainForm<>nil)
    and (Application.MainForm.HandleAllocated)
    and (PGtkWidget(Application.MainForm.Handle)<>Widget) then
      // there is still the main form left -> use it for clipboard
      SetClipboardWidget(PGtkWidget(Application.MainForm.Handle))
    else
      // program closed -> close clipboard
      SetClipboardWidget(nil);
  end;

  // update caret
  if GtkWidgetIsA(Widget,GTKAPIWidget_GetType) then
    DestroyCaret(HDC(PtrUInt(Widget)));

  // remove pending size messages
  UnsetResizeRequest(Widget);
  FWidgetsResized.Remove(Widget);
  if FixWidget<>Widget then
    FFixWidgetsResized.Remove(FixWidget);

  // destroy the widget
  //DebugLn(['TGtkWidgetSet.DestroyConnectedWidget ',GetWidgetDebugReport(Widget)]);
  DestroyWidget(Widget);

  // remove all remaining messages to this widget
  QueueItem:=FMessageQueue.FirstMessageItem;
  while (QueueItem<>nil) do begin
    MsgPtr := QueueItem.Msg;
    NextItem := TGtkMessagequeueItem(QueueItem.Next);
    if (PGtkWidget(MsgPtr^.hWnd)=Widget) then
      fMessageQueue.RemoveMessage(QueueItem,FPMF_All,true);
    QueueItem := NextItem;
  end;
end;

{-------------------------------------------------------------------------------
  TGtkWidgetSet.HookSignals
  Params: ALCLObject: TObject;
          AGTKObject: PGTKObject;

  Set default Callbacks defined by AGTKObject
-------------------------------------------------------------------------------}
//TODO: Remove when the creation splitup is finished.
//      In that case all code here is moved to the specific creation parts
procedure TGtkWidgetSet.HookSignals(const AGTKObject: PGTKObject;
  const ALCLObject: TObject);
begin
  if (ALCLObject is TWinControl)
  then TGTKWSWinControl.SetCallbacks(AGTKObject, TWinControl(ALCLObject));

  if (ALCLObject is TControl)
  then begin
    case TControl(ALCLObject).FCompStyle of

      {csButton,} csBitBtn:
      Begin
        SetCallback(LM_CLICKED, AGTKObject, ALCLObject);
      End;

      csRadioButton, csCheckBox, csToggleBox:
      begin
        SetCallback(LM_CHANGED, AGTKObject, ALCLObject);
      end;

      csCalendar:
      Begin
        SetCallback(LM_MONTHCHANGED, AGTKObject, ALCLObject);
        SetCallback(LM_YEARCHANGED, AGTKObject, ALCLObject);
        SetCallback(LM_DAYCHANGED, AGTKObject, ALCLObject);
      End;

      csComboBox:
      Begin
        SetCallback(LM_CHANGED, AGTKObject, ALCLObject);
        SetCallback(LM_COMMAND, AGTKObject, ALCLObject);
        SetCallback(LM_SELCHANGE, AGTKObject, ALCLObject);
      End;

      csListBox:
      Begin
        SetCallback(LM_SELCHANGE, AGTKObject, ALCLObject);
      End;

      csNotebook,csTrackBar :
      Begin
        SetCallback(LM_CHANGED, AGTKObject, ALCLObject);
      End;

      {$IfDef GTK1}
      csEdit, csSpinEdit:
      begin
        SetCallback(LM_CHANGED,  AGTKObject, ALCLObject);
        SetCallback(LM_ACTIVATE,  AGTKObject, ALCLObject);
        SetCallback(LM_CUTTOCLIP,  AGTKObject, ALCLObject);
        SetCallback(LM_COPYTOCLIP,  AGTKObject, ALCLObject);
        SetCallback(LM_PASTEFROMCLIP,  AGTKObject, ALCLObject);
        SetCallback(LM_INSERTTEXT,  AGTKObject, ALCLObject);
      end;

      csMemo:
      begin
        SetCallback(LM_CHANGED,  AGTKObject, ALCLObject);
        SetCallback(LM_ACTIVATE,  AGTKObject, ALCLObject);
        SetCallback(LM_CUTTOCLIP,  AGTKObject, ALCLObject);
        SetCallback(LM_COPYTOCLIP,  AGTKObject, ALCLObject);
        SetCallback(LM_PASTEFROMCLIP,  AGTKObject, ALCLObject);
        SetCallback(LM_INSERTTEXT,  AGTKObject, ALCLObject);
      end;
      {$EndIf}

      csWinControl:
      begin
        SetCallback(LM_HSCROLL, AGTKObject, ALCLObject);
        SetCallback(LM_VSCROLL, AGTKObject, ALCLObject);
      end;

      csForm:
      Begin
        if (TControl(ALCLObject).Parent=nil) then begin
          SetCallback(LM_CONFIGUREEVENT, AGTKObject, ALCLObject);
          SetCallback(LM_CLOSEQUERY, AGTKObject, ALCLObject);
          SetCallBack(LM_Activate, AGTKObject, ALCLObject);
          SetCallback(LM_HSCROLL, AGTKObject, ALCLObject);
          SetCallback(LM_VSCROLL, AGTKObject, ALCLObject);
        end;
      end;

      csHintWindow:
      begin
        if (TControl(ALCLObject).Parent=nil) then begin
          SetCallback(LM_CONFIGUREEVENT, AGTKObject, ALCLObject);
        end;
      end;

      csStaticText:
      Begin
        SetCallback(LM_GRABFOCUS, AGTKObject, ALCLObject);
      end;

      {$IfDef GTK1}
      csListview:
      begin
        SetCallback(LM_HSCROLL, AGTKObject, ALCLObject);
        SetCallback(LM_VSCROLL, AGTKObject, ALCLObject);
        SetCallback(LVN_COLUMNCLICK, AGTKObject, ALCLObject);
        SetCallback(LVN_ITEMCHANGED, AGTKObject, ALCLObject);
        SetCallback(LVN_ITEMCHANGING, AGTKObject, ALCLObject);
        SetCallback(LVN_DELETEITEM, AGTKObject, ALCLObject);
        SetCallback(LVN_INSERTITEM, AGTKObject, ALCLObject);
      end;
      {$EndIf}

      csScrollBox :
      Begin
        SetCallback(LM_HSCROLL, AGTKObject, ALCLObject);
        SetCallback(LM_VSCROLL, AGTKObject, ALCLObject);
      end;

      csScrollBar:
      begin
        if TScrollBar(ALCLObject).Kind = sbHorizontal
        then SetCallback(LM_HSCROLL, AGTKObject, ALCLObject)
        else SetCallback(LM_VSCROLL, AGTKObject, ALCLObject);
      end;

    end; //case
  end
  else
  if (ALCLObject is TMenuItem)
  then begin
    SetCallback(LM_ACTIVATE, AGTKObject, ALCLObject);
  end;
end;

{------------------------------------------------------------------------------
  procedure TGtkWidgetSet.InitializeCommonDialog
  Params: ADialog: TCommonDialog; AWindow: PGtkWidget
  Result: none

  Initializes a TCommonDialog window.
------------------------------------------------------------------------------}
procedure TGtkWidgetSet.InitializeCommonDialog(ADialog: TObject;
  AWindow: PGtkWidget);
var NewWidth, NewHeight: integer;
begin
  SetLCLObject(AWindow,ADialog);

  // connect events
  g_signal_connect(gtk_object(AWindow),
    'destroy', gtk_Signal_Func(@gtkDialogDestroyCB), ADialog);
  g_signal_connect(gtk_object(AWindow),
    'delete-event', gtk_Signal_Func(@gtkDialogCloseQueryCB), ADialog);
  g_signal_connect(gtk_object(AWindow),
    'key-press-event', gtk_Signal_Func(@GTKDialogKeyUpDownCB), ADialog);
  g_signal_connect(gtk_object(AWindow),
    'key-release-event', gtk_Signal_Func(@GTKDialogKeyUpDownCB), ADialog);
  g_signal_connect(gtk_object(AWindow),
    'realize', gtk_Signal_Func(@GTKDialogRealizeCB), ADialog);

  // set default size
  NewWidth:=TCommonDialog(ADialog).Width;
  if NewWidth<=0 then NewWidth:=-2; // -2 = let the window manager decide
  NewHeight:=TCommonDialog(ADialog).Height;
  if NewHeight<=0 then NewHeight:=-2; // -2 = let the window manager decide
  if (NewWidth>0) or (NewHeight>0) then
    gtk_window_set_default_size(PgtkWindow(AWindow),NewWidth,NewHeight);
end;

{------------------------------------------------------------------------------
  Function: CreateOpenDialogHistory
  Params: OpenDialog: TOpenDialog; SelWidget: PGtkWidget
  Returns: -

  Adds a History pulldown to a gtk file selection dialog.
 ------------------------------------------------------------------------------}
procedure CreateOpenDialogHistory(OpenDialog: TOpenDialog;
  SelWidget: PGtkWidget);
var
  HistoryList: TFPList; // list of THistoryListEntry
  AHistoryEntry: PFileSelHistoryEntry;
  i: integer;
  s: string;
  HBox, LabelWidget, HistoryPullDownWidget,
  MenuWidget, MenuItemWidget: PGtkWidget;
begin
  if OpenDialog.HistoryList.Count>0 then begin

    // create the HistoryList where the current state of the history is stored
    HistoryList:=TFPList.Create;
    for i:=0 to OpenDialog.HistoryList.Count-1 do begin
      s:=OpenDialog.HistoryList[i];
      if s<>'' then begin
        New(AHistoryEntry);
        HistoryList.Add(AHistoryEntry);
        AHistoryEntry^.Filename := StrAlloc(length(s)+1);
        StrPCopy(AHistoryEntry^.Filename, s);
        AHistoryEntry^.MenuItem:=nil;
      end;
    end;

    // create a HBox so that the history is left justified
    HBox:=gtk_hbox_new(false,0);
    gtk_object_set_data(PGtkObject(SelWidget), 'LCLHistoryHBox', HBox);
    {$IFDEF GTK1}
    gtk_box_pack_start(GTK_BOX(GTK_FILE_SELECTION(SelWidget)^.main_vbox),
                       HBox,false,false,0);
    {$ELSE}
    gtk_file_chooser_set_extra_widget(PGtkDialog(SelWidget),HBox);
    {$ENDIF}

    // create the label 'History:'
    s:=rsgtkHistory;
    LabelWidget:=gtk_label_new(PChar(s));
    gtk_box_pack_start(GTK_BOX(HBox),LabelWidget,false,false,5);
    gtk_widget_show(LabelWidget);

    // create the pull down
    HistoryPullDownWidget:=gtk_option_menu_new;
    gtk_object_set_data(PGtkObject(SelWidget), 'LCLHistoryPullDown',
      HistoryPullDownWidget);
    gtk_box_pack_start(GTK_BOX(HBox),HistoryPullDownWidget,false,false,5);
    gtk_widget_show(HistoryPullDownWidget);
    gtk_widget_show_all(HBox);

    // create the menu (the content of the pull down)
    MenuWidget:=gtk_menu_new;
    SetLCLObject(MenuWidget,OpenDialog);
    for i:=0 to HistoryList.Count-1 do begin
      // create the menu items in the history menu
      MenuItemWidget:=gtk_menu_item_new_with_label(
                                PFileSelHistoryEntry(HistoryList[i])^.Filename);
      // connect the new MenuItem to the HistoryList entry
      gtk_object_set_data(PGtkObject(MenuItemWidget), 'LCLIsHistoryMenuItem',
        HistoryList[i]);
      // add activation signal and add to menu
      g_signal_connect(GTK_OBJECT(MenuItemWidget), 'activate',
                         gtk_signal_func(@GTKDialogMenuActivateCB),
                         OpenDialog);
      gtk_menu_append(MenuWidget, MenuItemWidget);
      gtk_widget_show(MenuItemWidget);
    end;
    gtk_widget_show(MenuWidget);
    gtk_option_menu_set_menu(GTK_OPTION_MENU(HistoryPullDownWidget),
                             MenuWidget);
  end else begin
    MenuWidget:=nil;
    HistoryList:=nil
  end;
  gtk_object_set_data(PGtkObject(SelWidget), 'LCLHistoryMenu', MenuWidget);
  gtk_object_set_data(PGtkObject(SelWidget), 'LCLHistoryList', HistoryList);
end;

{------------------------------------------------------------------------------
  Function: TGtkWidgetSet.CreateOpenDialogFilter
  Params: OpenDialog: TOpenDialog; SelWidget: PGtkWidget
  Returns: -

  Adds a Filter pulldown to a gtk file selection dialog. Returns the
  inital filter mask.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.CreateOpenDialogFilter(OpenDialog: TOpenDialog;
  SelWidget: PGtkWidget): string;
var
  FilterList: TFPList;
  HBox, LabelWidget, FilterPullDownWidget,
  MenuWidget, MenuItemWidget: PGtkWidget;
  i, j, CurMask: integer;
  s: String;
begin
  ExtractFilterList(OpenDialog.Filter,FilterList,false);
  if FilterList.Count>0 then begin

    // create a HBox so that the filter pulldown is left justified
    HBox:=gtk_hbox_new(false,0);
    gtk_object_set_data(PGtkObject(SelWidget), 'LCLFilterHBox', HBox);
    gtk_box_pack_start(GTK_BOX(GTK_FILE_SELECTION(SelWidget)^.main_vbox),
                       HBox,false,false,0);

    // create the label 'Filter:'
    s:=rsgtkFilter;
    LabelWidget:=gtk_label_new(PChar(s));
    gtk_box_pack_start(GTK_BOX(HBox),LabelWidget,false,false,5);
    gtk_widget_show(LabelWidget);

    // create the pull down
    FilterPullDownWidget:=gtk_option_menu_new;
    gtk_object_set_data(PGtkObject(SelWidget), 'LCLFilterPullDown',
      FilterPullDownWidget);
    gtk_box_pack_start(GTK_BOX(HBox),FilterPullDownWidget,false,false,5);
    gtk_widget_show(FilterPullDownWidget);
    gtk_widget_show(HBox);

    // create the menu (the content of the pull down)
    MenuWidget:=gtk_menu_new;
    SetLCLObject(MenuWidget,OpenDialog);
    for i:=0 to FilterList.Count-1 do begin
      // create the menu items in the filter menu
      MenuItemWidget:=gtk_menu_item_new_with_label(
                               PFileSelFilterEntry(FilterList[i])^.Description);
      // connect the new MenuItem to the FilterList entry
      gtk_object_set_data(PGtkObject(MenuItemWidget), 'LCLIsFilterMenuItem',
        FilterList[i]);
      // add activation signal and add to menu
      g_signal_connect(GTK_OBJECT(MenuItemWidget), 'activate',
                         gtk_signal_func(@GTKDialogMenuActivateCB),
                         OpenDialog);
      gtk_menu_append(MenuWidget, MenuItemWidget);
      gtk_widget_show(MenuItemWidget);
    end;
    gtk_widget_show(MenuWidget);
    gtk_option_menu_set_menu(GTK_OPTION_MENU(FilterPullDownWidget),
                             MenuWidget);
  end else begin
    MenuWidget:=nil;
  end;
  gtk_object_set_data(PGtkObject(SelWidget), 'LCLFilterMenu', MenuWidget);
  gtk_object_set_data(PGtkObject(SelWidget), 'LCLFilterList', FilterList);

  // set the initial filter
  Result := 'none'; { Don't use '' as null return as this is used for *.* }
  if FilterList.Count>0 then begin
    i:=0;
    j:=OpenDialog.FilterIndex - 1; // FilterIndex is 1 based
    if j<0 then j:=0;
    CurMask:=0;
    while (i<FilterList.Count) do begin
      if PFileSelFilterEntry(FilterList[i])^.FilterIndex=j
      then begin
        CurMask:=i;
        break;
      end;
      inc(i);
    end;
    Result := PFileSelFilterEntry(FilterList[CurMask])^.Mask;
    gtk_option_menu_set_history(GTK_OPTION_MENU(FilterPullDownWidget), CurMask);
  end;
end;

{------------------------------------------------------------------------------
  Function: TGtkWidgetSet.CreatePreviewDialogControl
  Params: PreviewDialog: TPreviewFileDialog; SelWidget: PGtkWidget
  Returns: -

  Adds a preview control to a gtk file selection dialog.
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.CreatePreviewDialogControl(
  PreviewDialog: TPreviewFileDialog; SelWidget: PGtkWidget);
var
  PreviewWidget: PGtkWidget;
  list_hbox: PGtkWidget;
  DirListWidget: PGtkWidget;
  ScrolledWin: PGtkWidget;
  AControl: TPreviewFileControl;
begin
  AControl:=PreviewDialog.PreviewFileControl;
  if AControl=nil then exit;
  // find the hbox widget of the file and directory dialog
  DirListWidget:=PGtkFileSelection(SelWidget)^.dir_list;
  ScrolledWin:=DirListWidget^.parent;
  if not GtkWidgetIsA(ScrolledWin,GTK_TYPE_SCROLLED_WINDOW) then begin
    DebugLn('NOTE: CreatePreviewDialogControl ',
      'parent widget of dir_list widget is not a scrolled window');
    exit;
  end;
  list_hbox:=ScrolledWin^.parent;
  if not GtkWidgetIsA(list_hbox,GTK_TYPE_HBOX) then begin
    DebugLn('NOTE: CreatePreviewDialogControl ',
      'parent widget of scrolled window is not a hbox');
    exit;
  end;
  // create the preview widget
  PreviewWidget:=PGtkWidget(AControl.Handle);
  gtk_object_set_data(PGtkObject(PreviewWidget),'LCLPreviewFixed',
                      PreviewWidget);
  gtk_widget_set_usize(PreviewWidget,AControl.Width,AControl.Height);
  gtk_box_pack_start(GTK_BOX(list_hbox),PreviewWidget,true,true,0);
  gtk_widget_show(PreviewWidget);
end;

{------------------------------------------------------------------------------
  Function: TGtkWidgetSet.InitializeOpenDialog
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
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.InitializeOpenDialog(OpenDialog: TOpenDialog;
  SelWidget: PGtkWidget);
var
  FileDetailLabel, HBox, FrameWidget: PGtkWidget;
  FileSelWidget: PGtkFileSelection;
  InitialFilter: string;
begin
  DebugLn(['TGtkWidgetSet.InitializeOpenDialog ']);
  FileSelWidget:=GTK_FILE_SELECTION(SelWidget);

  // Help button
  if (ofShowHelp in OpenDialog.Options)
  and (FileSelWidget^.Help_Button<>nil) then begin
    gtk_widget_show(FileSelWidget^.Help_Button);
    g_signal_connect( gtk_object(FileSelWidget^.help_button),
      'clicked', gtk_signal_func(@gtkDialogHelpclickedCB), OpenDialog);
  end;

  // connect selection entry (edit field for filename)
  if (FileSelWidget^.selection_entry<>nil) then begin
    SetLCLObject(FileSelWidget^.selection_entry,OpenDialog);
    g_signal_connect(
      gtk_object(FileSelWidget^.selection_entry),
      'key-press-event', gtk_signal_func(@GTKDialogKeyUpDownCB),
      OpenDialog);
    g_signal_connect(
      gtk_object(FileSelWidget^.selection_entry),
      'focus-in-event', gtk_signal_func(@GTKDialogFocusInCB), OpenDialog);
  end;

  // connect dir list (list of directories)
  if (FileSelWidget^.dir_list<>nil) then begin
    SetLCLObject(FileSelWidget^.dir_list,OpenDialog);
    g_signal_connect(gtk_object(FileSelWidget^.dir_list),
      'select-row', gtk_signal_func(@GTKDialogSelectRowCB), OpenDialog);
  end;

  // connect file list (list of files in current directory)
  if (FileSelWidget^.file_list<>nil) then begin
    LastFileSelectRow := -1;
    SetLCLObject(FileSelWidget^.file_list,OpenDialog);
    g_signal_connect(gtk_object(FileSelWidget^.file_list),
      'select-row', gtk_signal_func(@GTKDialogSelectRowCB), OpenDialog);
    if ofAllowMultiSelect in OpenDialog.Options then
      gtk_clist_set_selection_mode(
        PGtkCList(FileSelWidget^.file_list),GTK_SELECTION_MULTIPLE);
  end;

  // History List - a frame with an option menu
  CreateOpenDialogHistory(OpenDialog,SelWidget);

  // Filter - a frame with an option menu
  InitialFilter := CreateOpenDialogFilter(OpenDialog,SelWidget);

  // Details - a frame with a label
  if (ofViewDetail in OpenDialog.Options) then begin
    // create the frame around the information
    FrameWidget:=gtk_frame_new(PChar(rsFileInformation));
    gtk_box_pack_start(GTK_BOX(FileSelWidget^.main_vbox),
                       FrameWidget,false,false,0);
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

  // preview
  if (OpenDialog is TPreviewFileDialog) then
    CreatePreviewDialogControl(TPreviewFileDialog(OpenDialog),SelWidget);

  // set initial filename
  if OpenDialog.Filename<>'' then
    gtk_file_selection_set_filename(FileSelWidget,PChar(OpenDialog.Filename));

  if InitialFilter <> 'none' then
    PopulateFileAndDirectoryLists(FileSelWidget, InitialFilter);
end;

{------------------------------------------------------------------------------
  Function: TGtkWidgetSet.InitializeFileDialog
  Params: FileDialog: TFileDialog; var SelWidget: PGtkWidget
  Returns: -

  Creates a new TFile/Open/SaveDialog
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.InitializeFileDialog(FileDialog: TFileDialog;
  var SelWidget: PGtkWidget; Title: PChar);
begin
  //DebugLn(['TGtkWidgetSet.InitializeFileDialog ']);
  SelWidget := gtk_file_selection_new(Title);

  {****This is a major hack put by Cliff Baeseman to solve
   a gtk win32 dll implementation problem where the headers implementation
   does not match the linux version**** }
  {$IFNDEF WIN32}
    g_signal_connect(gtk_object(PGtkFileSelection(SelWidget)^.ok_button),
      'clicked', gtk_signal_func(@gtkDialogOKclickedCB), FileDialog);
    g_signal_connect(gtk_object(PGtkFileSelection(SelWidget)^.cancel_button),
      'clicked', gtk_signal_func(@gtkDialogCancelclickedCB), FileDialog);
  {$ELSE}
    g_signal_connect(gtk_object(PGtkFileSelection(SelWidget)^.cancel_button),
      'clicked', gtk_signal_func(@gtkDialogOKclickedCB), FileDialog);
    g_signal_connect(gtk_object(PGtkFileSelection(SelWidget)^.help_button),
      'clicked', gtk_signal_func(@gtkDialogCancelclickedCB), FileDialog);
  {$ENDIF}

  if FileDialog is TOpenDialog then
    InitializeOpenDialog(TOpenDialog(FileDialog),SelWidget);

  InitializeCommonDialog(TCommonDialog(FileDialog),SelWidget);
end;

{------------------------------------------------------------------------------
  Function: TGtkWidgetSet.InitializeFontDialog
  Params: FontDialog: TFontialog; var SelWidget: PGtkWidget
  Returns: -

  Creates a new TFontDialog
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.InitializeFontDialog(FontDialog: TFontDialog;
  var SelWidget: PGtkWidget; Title: PChar);
{$IFDEF GTK1}
var
  SpacingFilter: PPgchar;
  FontType: TGtkFontType;
const
  FixedFilter: array [0..2] of PChar = ( 'c', 'm', nil );
{$ELSE}
var
  FontDesc: PPangoFontDescription;
  TmpStr: pChar;
{$ENDIF}
begin
  SelWidget := gtk_font_selection_dialog_new(Title);

  // connect Ok, Cancel and Apply Button
  g_signal_connect(
    gtk_object(PGtkFontSelectionDialog(SelWidget)^.ok_button),
    'clicked', gtk_signal_func(@gtkDialogOKclickedCB), FontDialog);
  g_signal_connect(
    gtk_object(PGtkFontSelectionDialog(SelWidget)^.cancel_button),
    'clicked', gtk_signal_func(@gtkDialogCancelclickedCB), FontDialog);
  g_signal_connect(
    gtk_object(PGtkFontSelectionDialog(SelWidget)^.apply_button),
    'clicked', gtk_signal_func(@gtkDialogApplyclickedCB), FontDialog);
  if fdApplyButton in FontDialog.Options then
    gtk_widget_show(PGtkFontSelectionDialog(SelWidget)^.apply_button);

  // set preview text
  if FontDialog.PreviewText<>'' then
    gtk_font_selection_dialog_set_preview_text(
      PGtkFontSelectionDialog(SelWidget),PChar(FontDialog.PreviewText));

  // set font name in XLFD format
  if IsFontNameXLogicalFontDesc(FontDialog.Font.Name) then
    gtk_font_selection_dialog_set_font_name(PGtkFontSelectionDialog(SelWidget),
      PChar(FontDialog.Font.Name))
  else begin
    {$ifdef GTK2}
    FontDesc := pango_font_description_new;
    with FontDialog.Font do begin
      pango_font_description_set_size(FontDesc, Size * PANGO_SCALE);
      
      if fsBold in Style then
        pango_font_description_set_weight(FontDesc, PANGO_WEIGHT_BOLD)
      else
        pango_font_description_set_weight(FontDesc, PANGO_WEIGHT_NORMAL);
        
      if fsItalic in Style then
        pango_font_description_set_style(FontDesc, PANGO_STYLE_ITALIC)
      else
        pango_font_description_set_style(FontDesc, PANGO_STYLE_NORMAL);
        
      pango_font_description_set_family(FontDesc, pChar(Name));
    end;
    TmpStr := pango_font_description_to_string(FontDesc);
    gtk_font_selection_dialog_set_font_name(PGtkFontSelectionDialog(SelWidget), TmpStr);
    g_free(TmpStr);
    pango_font_description_free(FontDesc);
    {$else}
    debugln('Gtk1 should have IsFontNameXLogicalFontDesc true!');
    {$endif}
  end;

  {$IFDEF GTK1}
  { This functionality does not seem to be available in GTK2 }
  // Honor selected TFontDialogOption flags
  SpacingFilter := nil;
  if fdFixedPitchOnly in FontDialog.Options then
    SpacingFilter := @FixedFilter[0];
  FontType := GTK_FONT_ALL;
  if fdScalableOnly in FontDialog.Options then
    FontType := GTK_FONT_SCALABLE;
  gtk_font_selection_dialog_set_filter (PGtkFontSelectionDialog(SelWidget),
                                        GTK_FONT_FILTER_BASE, FontType,
                                        nil, nil, nil, nil, SpacingFilter, nil);
  {$ENDIF}

  InitializeCommonDialog(TCommonDialog(FontDialog),SelWidget);
end;


procedure TGtkWidgetSet.FinishComponentCreate(const ALCLObject: TObject; const AGTKObject: Pointer);
begin
  // MWE: next will be obsoleted by WinWidgetInfo
  if AGTKObject <> nil then
  Begin
    SetLCLObject(AGTKObject, ALCLObject);
    gtk_object_set_data(AGTKObject, 'Style',GtkNil);
    gtk_object_set_data(AGTKObject, 'ExStyle',GtkNil);
  end;
  //--------------------------

  // in the new (compatibility) situation setting the handle should not be needed
  // however lazarus fails to start, so I'm enabling it for now
  if (ALCLObject is TWinControl) then
  begin
    TWinControl(ALCLObject).Handle := THandle(PtrUInt(AGTKObject));
    if AGTKObject <> nil then begin
      gtk_object_set_data(AGTKObject, 'Sender', ALCLObject);
    end;
  end
  else
  if (ALCLObject is TMenuItem) then
    TMenuItem(ALCLObject).Handle := HMenu(PtrUInt(AGTKObject))
  else
  if (ALCLObject is TMenu) then
    TMenu(ALCLObject).Items.Handle := HMenu(PtrUInt(AGTKObject))
  else
  if (ALCLObject is TCommonDialog) then
    TCommonDialog(ALCLObject).Handle:= THandle(PtrUInt(AGTKObject));

  Set_RC_Name(ALCLObject, AGTKObject);

  if AGTKObject <> nil then begin
    {$IFNDEF NoStyle}
    if (ALCLObject is TCustomForm) and (TCustomForm(ALCLObject).Parent=nil) then
      gtk_widget_set_app_paintable(AGTKObject, true);
    {$ENDIF}
    HookSignals(AGTKObject, ALCLObject);
  end;
end;

Function TGtkWidgetSet.GetCompStyle(Sender : TObject) : Longint;
begin
  Result := csNone;
  if (Sender is TControl) then
    Result := TControl(Sender).FCompStyle
  else
    if (Sender is TMenuItem) then
      Result := TMenuItem(Sender).FCompStyle
  else
    if (Sender is TMenu) or (Sender is TPopupMenu)
    then
      Result := TMenu(Sender).FCompStyle
  else
    if (Sender is TCommonDialog)
    then
      result := TCommonDialog(Sender).FCompStyle;
end;

Function TGtkWidgetSet.GetCaption(Sender : TObject) : String;
begin
  Result := Sender.ClassName;
  if (Sender is TControl) then
    Result := TControl(Sender).Caption
  else
    if (Sender is TMenuItem) then
      Result := TMenuItem(Sender).Caption;

  if Result = '' then
    Result := rsBlank;
end;

function TGtkWidgetSet.CreateAPIWidget(
  AWinControl: TWinControl): PGtkWidget;
// currently only used for csFixed
var
  Adjustment: PGTKAdjustment;
  WinWidgetInfo: PWinWidgetInfo;
begin
  Result := GTKAPIWidget_New;
  WinWidgetInfo:=GetWidgetInfo(Result,true);
  WinWidgetInfo^.CoreWidget:=PGTKAPIWidget(Result)^.Client;
  SetLCLObject(WinWidgetInfo^.CoreWidget,AWinControl);

  gtk_scrolled_window_set_policy(PGTKScrolledWindow(Result),
    GTK_POLICY_NEVER, GTK_POLICY_NEVER);

  Adjustment :=
    gtk_scrolled_window_get_vadjustment(PGTKScrolledWindow(Result));
  if Adjustment <> nil
  then with Adjustment^ do
  begin
    gtk_object_set_data(PGTKObject(Adjustment), odnScrollBar,
                        PGTKScrolledWindow(Result)^.VScrollBar);
    Step_Increment := 1;
  end;

  Adjustment :=
    gtk_scrolled_window_get_hadjustment(PGTKScrolledWindow(Result));
  if Adjustment <> nil
  then with Adjustment^ do
  begin
    gtk_object_set_data(PGTKObject(Adjustment), odnScrollBar,
                        PGTKScrolledWindow(Result)^.HScrollBar);
    Step_Increment := 1;
  end;
  
  if AWinControl is TCustomControl then
    GTKAPIWidget_SetShadowType(PGTKAPIWidget(Result),
      BorderStyleShadowMap[TCustomControl(AWinControl).BorderStyle]);
end;

{------------------------------------------------------------------------------
  function TGtkWidgetSet.CreatePairSplitter(PairSplitterObject: TObject
    ): PGtkWidget;

  Create a TCustomPairSplitter widget set
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.CreatePairSplitter(PairSplitterObject: TObject
  ): PGtkWidget;
var
  APairSplitter: TCustomPairSplitter;
  PanedWidget: PGtkWidget;
begin
  APairSplitter:=TCustomPairSplitter(PairSplitterObject);
  // create the paned
  if APairSplitter.SplitterType=pstHorizontal then
    PanedWidget:=gtk_hpaned_new
  else
    PanedWidget:=gtk_vpaned_new;
  Result:=PanedWidget;
end;

{------------------------------------------------------------------------------
  function TGtkWidgetSet.CreateStatusBar(StatusBar: TObject): PGtkWidget;

  Create a TStatusBar widget set
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.CreateStatusBar(StatusBar: TObject): PGtkWidget;
begin
  {$IFDEF OldStatusBar}
  Result:=gtk_statusbar_new;
  {$ELSE}
  Result:=gtk_hbox_new(false,0);
  UpdateStatusBarPanels(StatusBar,Result);
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  function TGtkWidgetSet.CreateStatusBarPanel(StatusBar: TObject; Index: integer
    ): PGtkWidget;

  Creates a new statusbar panel widget.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.OldCreateStatusBarPanel(StatusBar: TObject; Index: integer
  ): PGtkWidget;
begin
  Result:=gtk_statusbar_new;
  gtk_widget_show(Result);
  // other properties are set in UpdateStatusBarPanels
end;

{------------------------------------------------------------------------------
  function TGtkWidgetSet.CreateSimpleClientAreaWidget(Sender: TObject;
    NotOnParentsClientArea: boolean): PGtkWidget;

  Create a fixed widget in a horizontal box
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.CreateSimpleClientAreaWidget(Sender: TObject;
  NotOnParentsClientArea: boolean): PGtkWidget;
var
  TempWidget: PGtkWidget;
  WinWidgetInfo: PWinWidgetInfo;
begin
  {$if defined(gtk1) or defined(GtkFixedWithWindow)}
  // Fixed + GdkWindow
  Result := gtk_hbox_new(false, 0);
  TempWidget := CreateFixedClientWidget;
  {$else}
  // Fixed w/o GdkWindow
  Result := gtk_event_box_new;
  gtk_event_box_set_visible_window(PGtkEventBox(Result), False);
  TempWidget := CreateFixedClientWidget(False);
  {$ifend}
  
  gtk_container_add(GTK_CONTAINER(Result), TempWidget);
  gtk_widget_show(TempWidget);
  if NotOnParentsClientArea then
  begin
    WinWidgetInfo:=GetWidgetInfo(Result, true);
    Include(WinWidgetInfo^.Flags, wwiNotOnParentsClientArea);
  end;
  SetFixedWidget(Result, TempWidget);
  SetMainWidget(Result, TempWidget);
  gtk_widget_show(Result);
end;

function TGTKWidgetSet.CreateStandardCursor(ACursor: SmallInt): hCursor;
var
  CursorValue: Integer;
begin
  Result := 0;
  if ACursor < crLow then Exit;
  if ACursor > crHigh then Exit;

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
    crHourGlass:CursorValue := GDK_WATCH;
    crHSplit:   CursorValue := GDK_SB_H_DOUBLE_ARROW;
    crVSplit:   CursorValue := GDK_SB_V_DOUBLE_ARROW;
    crNo:       CursorValue := GDK_LEFT_PTR;
    crAppStart: CursorValue := GDK_LEFT_PTR;
    crHelp:     CursorValue := GDK_QUESTION_ARROW;
    crHandPoint:CursorValue := GDK_Hand1;
    crSizeAll:  CursorValue := GDK_FLEUR;
  else
    CursorValue := -1;
  end;
  if CursorValue <> -1 then
    Result := hCursor(PtrUInt(gdk_cursor_new(CursorValue)));
end;

{------------------------------------------------------------------------------
  function TGtkWidgetSet.CreateToolBar(ToolBarObject: TObject): PGtkWidget;

  Creates a gtk_toolbar and puts a fixed widget as client area.
  Since we are not using the gtk tool buttons, we can put any LCL control as
  child and get all LCL TControl abilities.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.CreateToolBar(ToolBarObject: TObject): PGtkWidget;
var
  ClientWidget: PGtkWidget;
begin
  {$IFDEF GTK1}
  Result := gtk_toolbar_new();
  gtk_toolbar_set_space_size(PGTKToolbar(Result),0);
  gtk_toolbar_set_space_style(PGTKToolbar(Result),GTK_TOOLBAR_SPACE_EMPTY);
  ClientWidget := gtk_fixed_new();
  gtk_toolbar_insert_widget(PGTKToolbar(Result),ClientWidget,nil,nil,0);
  {$ELSE}
  Result := gtk_hbox_new(false,0);
  ClientWidget := CreateFixedClientWidget;
  gtk_container_add(GTK_CONTAINER(Result), ClientWidget);
  {$ENDIF}
  gtk_widget_show(ClientWidget);
  SetFixedWidget(Result,ClientWidget);
  SetMainWidget(Result,ClientWidget);
  gtk_widget_show(Result);
end;


{------------------------------------------------------------------------------
  Function: TGtkWidgetSet.CreateComponentWidget
  Params:   Ssender - object for which to create visual representation
            CompStyle - componentstyle (type) of GtkWidget which will be created
            Caption - the caption of "Sender"
  Returns:  nothing

  Tells GTK Engine to create a widget
 ------------------------------------------------------------------------------}
function TGTKWidgetSet.CreateComponentWidget(Sender: TObject; ACompStyle: Integer; const ACaption: String): PGtkWidget;
var
  p: Pointer absolute Result;
  
  TempWidget,
  TempWidget2 : PGTKWidget;      // pointer to gtk-widget (local use when neccessary)
  TempInt   : Integer;           // local use when neccessary

  Box       : Pointer;           // currently only used for MainMenu
  ParentForm: TCustomForm;
  Adjustment: PGtkAdjustment;
  LabelWidget: PGtkLabel;
begin
  Result := nil;

  case ACompStyle of
  csAlignment :
    begin
      p := gtk_alignment_new(0.5,0.5,0,0);
      gtk_widget_show(p);
    end;

  csArrow :
    begin
      P := gtk_event_box_new();
      TempWidget := gtk_arrow_new(gtk_arrow_left, gtk_shadow_etched_in);
      gtk_container_add(p, TempWidget);
      gtk_widget_show(TempWidget);
      gtk_object_set_data(P, 'arrow', TempWidget);
    end;

  csCalendar :
    begin
      p := gtk_frame_new(nil);
      TempWidget := gtk_calendar_new();
      gtk_container_add(GTK_CONTAINER(p), TempWidget);
      SetMainWidget(p, TempWidget);
      GetWidgetInfo(p, True)^.CoreWidget := TempWidget;
      gtk_widget_show_all(p);
    end;

  csCheckbox :
    begin
      p := gtk_check_button_new_with_label(PChar(ACaption));
    end;


  csColorDialog :
    begin
      P := gtk_color_selection_dialog_new(PChar(ACaption));
      g_signal_connect( gtk_object((GTK_COLOR_SELECTION_DIALOG(P))^.ok_button),
        'clicked', gtk_signal_func(@gtkDialogOKclickedCB), Sender);
      g_signal_connect( gtk_object((GTK_COLOR_SELECTION_DIALOG(P))^.cancel_button),
        'clicked', gtk_signal_func(@gtkDialogCancelclickedCB), Sender);
      InitializeCommonDialog(TCommonDialog(Sender),p);
    end;


  csFileDialog, csOpenFileDialog, csSaveFileDialog, csSelectDirectoryDialog,
  csPreviewFileDialog:
    InitializeFileDialog(TFileDialog(Sender),p,PChar(ACaption));

  csFontDialog :
    InitializeFontDialog(TFontDialog(Sender),p,PChar(ACaption));

  csWinControl:
    p:=CreateAPIWidget(TWinControl(Sender));


  csImage :
    begin
      p := gtk_image_new();
    end;

  csMainMenu:
    begin
      p := gtk_menu_bar_new();
      // get the VBox, the form has one child, a VBox
      ParentForm:=TCustomForm(TMenu(Sender).Parent);
      if (ParentForm=nil) or (not (ParentForm is TCustomForm)) then
        RaiseGDBException('MainMenu without form');
      if ParentForm.Menu<>TMenu(Sender) then
        RaiseGDBException('form has already a MainMenu');
      if ParentForm.HandleAllocated then begin
        Box := PGTKBin(ParentForm.Handle)^.Child;
        gtk_box_pack_start(Box, p, False, False, 0);
      end;
      gtk_widget_show(p);
    end;

  csMenuBar :
    begin
       P := gtk_menu_bar_new();
       gtk_container_add(
         GTK_Container(
           GetFixedWidget(Pointer(TWinControl(TMenu(Sender).Owner).Handle))), P);
       gtk_widget_show(p);
    end;


  csPage:    // TCustomPage - Notebook page
    P:=CreateSimpleClientAreaWidget(Sender, true);

  csPairSplitter:
    p:=CreatePairSplitter(Sender);

  csPairSplitterSide:
    P:=CreateSimpleClientAreaWidget(Sender,true);

  csPopupMenu :
    P := gtk_menu_new();

  csPreviewFileControl:
    P:=CreateSimpleClientAreaWidget(Sender,true);

  csProgressBar:
    with (TCustomProgressBar (Sender)) do
    begin
       { Create a GtkAdjustment object to hold the range of the progress bar }
       TempWidget := PGtkWidget( gtk_adjustment_new (Position, Min, Max, 0, 0, 0));
       { Create the GtkProgressBar using the adjustment }
       P := gtk_progress_bar_new_with_adjustment (PGtkAdjustment (TempWidget));
    end;

  csRadioButton :
    with TRadioButton(Sender) do
    begin
      // Look for our parent's control and use the first radio we find for grouping
      TempWidget:= nil;
      if (Parent <> nil) then begin
        for TempInt:= 0 to Parent.ControlCount - 1 do begin
          if (Parent.Controls[TempInt] is TRadioButton)
          and TWinControl(Parent.Controls[TempInt]).HandleAllocated then begin
            TempWidget:= PGtkWidget(TWinControl(Parent.Controls[TempInt]).Handle);
            Break;
          end;
        end;
      end;
      if TempWidget <> nil then
        P:= gtk_radio_button_new_with_label(PGtkRadioButton(TempWidget)^.group,'')
      else
        P:= gtk_radio_button_new_with_label(nil, '');
      LabelWidget:=pGtkLabel(gtk_bin_get_child(PGtkBin(@PGTKToggleButton(P)^.Button)));
      SetLabelCaption(LabelWidget, PChar(ACaption)
         {$IFDEF Gtk1}, TWinControl(Sender),PGtkWidget(p), 'clicked'{$ENDIF});
    end;

  csScrollBar :
    begin
      Adjustment := PgtkAdjustment(
                     gtk_adjustment_new(1,TScrollBar(sender).min,
                     TScrollBar(sender).max,
                     TScrollBar(sender).SmallChange, TScrollBar(sender).LargeChange,
                     TScrollBar(sender).Pagesize));
      if (TScrollBar(sender).kind = sbHorizontal) then
        P := gtk_hscrollbar_new(Adjustment)
      else
        P := gtk_vscrollbar_new(Adjustment);
      gtk_object_set_data(PGTKObject(Adjustment), odnScrollBar, P);
    end;

  csScrolledWindow :
    begin
       P := gtk_scrolled_window_new(nil,nil);
    end;

  csSpeedButton:
    Begin
      p := gtk_button_new_with_label(PChar(ACaption));
    end;

  csStatusBar :
    begin
      p:=CreateStatusBar(Sender);
    end;

  csToggleBox :
    begin
      P := gtk_toggle_button_new_with_label(PChar(ACaption));
    end;

  csToolbar:
    P:=CreateToolBar(Sender);

  csToolButton:
    begin
      p := CreateFixedClientWidget;
    end;

  csTrackBar:
    with (TCustomTrackBar (Sender)) do
    begin
       TempWidget := PGtkWidget( gtk_adjustment_new (Position, Min, Max,
                                                     linesize, pagesize, 0));
       if (Orientation = trHorizontal) then
         P := gtk_hscale_new (PGTKADJUSTMENT (TempWidget))
         else
         P := gtk_vscale_new (PGTKADJUSTMENT (TempWidget));
       gtk_scale_set_digits (PGTKSCALE (P), 0);
    end;

  csScrollBox :
    begin
      Assert(Sender is TScrollBox);

      p := gtk_frame_new(nil);
      gtk_frame_set_shadow_type(pGtkFrame(p),GTK_SHADOW_IN);

      TempWidget := gtk_scrolled_window_new(nil,nil);
      gtk_container_add(PGTKContainer(p), TempWidget);
      gtk_widget_show(TempWidget);

      GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(TempWidget)^.hscrollbar, GTK_CAN_FOCUS);
      GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(TempWidget)^.vscrollbar, GTK_CAN_FOCUS);
      gtk_scrolled_window_set_policy(PGtkScrolledWindow(TempWidget),
                                     GTK_POLICY_NEVER,
                                     GTK_POLICY_NEVER);

      gtk_object_set_data(P,odnScrollArea, TempWidget);

      Adjustment := gtk_scrolled_window_get_vadjustment(PGTKScrolledWindow(TempWidget));
      if Adjustment <> nil
      then gtk_object_set_data(PGTKObject(Adjustment), odnScrollBar,
                               PGTKScrolledWindow(TempWidget)^.vscrollbar);

      Adjustment := gtk_scrolled_window_get_hadjustment(PGTKScrolledWindow(TempWidget));
      if Adjustment <> nil
      then gtk_object_set_data(PGTKObject(Adjustment), odnScrollBar,
                               PGTKScrolledWindow(TempWidget)^.hscrollbar);

      TempWidget2 := gtk_layout_new(nil, nil);
      gtk_container_add(PGTKContainer(TempWidget), TempWidget2);
      gtk_widget_show(TempWidget2);
      SetFixedWidget(p, TempWidget2);
      SetMainWidget(p, TempWidget2);

    end;

  // All obsoletes (some of them may still be in use for gtk1/gtk2, they are overridden there)
  csClistBox, csListBox, csCheckListBox,
  csComboBox,
  csMemo,
  csSpinEdit,
  csStaticText,
  csPanel,
  csMenuItem,
  csListView,
  csHintWindow,
  csGroupBox,
  csForm,
  csEdit,
  csBitBtn,
  csButton: DebugLn('[WARNING] Obsolete call to TGTKOBject.CreateComponent for ', Sender.ClassName);
  
  end; //end case
end;


{------------------------------------------------------------------------------
  Function: TGtkWidgetSet.CreateComponent
  Params:   sender - object for which to create visual representation
  Returns:  nothing

  Tells GTK Engine to create a widget
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.CreateComponent(Sender : TObject): THandle;
var
  Caption: string;     // the caption of "Sender"
  p: pointer;          // ptr to the newly created GtkWidget
  CompStyle: Integer;  // componentstyle (type) of GtkWidget which will be created
begin
  CompStyle := GetCompStyle(Sender);
  Caption   := GetCaption(Sender);

  p := CreateComponentWidget(Sender, CompStyle, Caption);

  FinishComponentCreate(Sender, P);
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(P,dbgsName(Sender));
  {$ENDIF}
  Result := THandle(PtrUInt(P));
end;

{------------------------------------------------------------------------------
  procedure TGtkWidgetSet.DestroyEmptySubmenu(Sender: TObject);

  Used by DestroyLCLComponent to destroy empty submenus, when destroying the
  last menu item.
------------------------------------------------------------------------------}
procedure TGtkWidgetSet.DestroyEmptySubmenu(Sender: TObject);
var
  LCLMenuItem: TMenuItem;
  ParentLCLMenuItem: TMenuItem;
  ParentMenuWidget: PGtkWidget;
  ParentSubMenuWidget: PGtkWidget;
  SubMenuWidget: PGtkMenu;
begin
  if not (Sender is TMenuItem) then
    RaiseGDBException('TGtkWidgetSet.DestroyEmptySubmenu');
  // destroying a TMenuItem
  LCLMenuItem:=TMenuItem(Sender);
  // check if in a sub menu
  if (LCLMenuItem.Parent=nil) then exit;
  if not (LCLMenuItem.Parent is TMenuItem) then exit;
  ParentLCLMenuItem:=TMenuItem(LCLMenuItem.Parent);
  if not ParentLCLMenuItem.HandleAllocated then exit;
  ParentMenuWidget:=PGtkWidget(ParentLCLMenuItem.Handle);
  if not GtkWidgetIsA(ParentMenuWidget,GTK_TYPE_MENU_ITEM) then exit;
  ParentSubMenuWidget:=PGTKMenuItem(ParentMenuWidget)^.submenu;
  if not GtkWidgetIsA(ParentSubMenuWidget,GTK_TYPE_MENU) then exit;
  SubMenuWidget:=PGTKMenu(ParentSubMenuWidget);
  if SubMenuWidget^.menu_shell.children=nil then begin
    gtk_widget_destroy(PgtkWidget(SubMenuWidget));
    gtk_object_set_data(PGtkObject(ParentMenuWidget),'ContainerMenu',nil);
  end;
end;

{------------------------------------------------------------------------------
       TGtkWidgetSet AssignSelf
       *Note: Assigns a pointer to self on a widget
------------------------------------------------------------------------------}
procedure TGtkWidgetSet.AssignSelf(Child,Data : Pointer);
begin
  gtk_Object_Set_Data(Pgtkobject(Child),'Self',Data);
end;

{------------------------------------------------------------------------------
       TGtkWidgetSet ShowHide
       *Note: Show or hide a widget
------------------------------------------------------------------------------}
{$note TODO: move to wsclass }
procedure TGtkWidgetSet.ShowHide(Sender : TObject);

  procedure RaiseWrongClass;
  begin
    RaiseGDBException('TGtkWidgetSet.ShowHide Sender.ClassName='+Sender.ClassName);
  end;

var
  FormIconGdiObject: PGDIObject;
  SenderWidget: PGTKWidget;
  LCLControl: TWinControl;
  Decor, Func : Longint;
  AWindow: PGdkWindow;
  ACustomForm: TCustomForm;
begin
  if not (Sender is TWinControl) then
    RaiseWrongClass;
  if (Sender is TCustomForm) then
    ACustomForm:=TCustomForm(Sender)
  else
    ACustomForm:=nil;

  LCLControl:=TWinControl(Sender);
  if not LCLControl.HandleAllocated then exit;
  SenderWidget:=PgtkWidget(LCLControl.Handle);
  //if (Sender is TForm) and (Sender.ClassName='TForm1') then
  //  DebugLn('[TGtkWidgetSet.ShowHide] START ',TControl(Sender).Name,':',Sender.ClassName,
  //    ' Visible=',TControl(Sender).Visible,' GtkVisible=',gtk_widget_visible(SenderWidget),
  //    ' GtkRealized=',gtk_widget_realized(SenderWidget),
  //    ' GtkMapped=',gtk_widget_mapped(SenderWidget),
  //    ' Should=',LCLControl.HandleObjectShouldBeVisible);
  if LCLControl.HandleObjectShouldBeVisible then
  begin
    if (ACustomForm<>nil) and (ACustomForm.Parent=nil) then begin
      // update shared accelerators
      ShareWindowAccelGroups(SenderWidget);
    end;

    // before making the widget visible, set the position and size
    // this is not possible for windows - for windows position will be setted
    // after widget become visible
    if FWidgetsWithResizeRequest.Contains(SenderWidget) then
    begin
      if (ACustomForm<>nil) and (ACustomForm.Parent=nil) then
      begin
        // top level control (a form without parent)
        {$IFDEF VerboseFormPositioning}
        DebugLn('VFP [TGtkWidgetSet.ShowHide] A set bounds ',
          LCLControl.Name,':',LCLControl.ClassName,
          ' Window=',dbgs(GetControlWindow(SenderWidget)<>nil),
          ' ',dbgs(LCLControl.Left),',',dbgs(LCLControl.Top),
          ',',dbgs(LCLControl.Width),',',dbgs(LCLControl.Height));
        {$ENDIF}
        SetWindowSizeAndPosition(PgtkWindow(SenderWidget),LCLControl);
      end
      else
      if (LCLControl.Parent<>nil) then
      begin
        // resize widget
        {$IFDEF VerboseSizeMsg}
        DebugLn(['TGtkWidgetSet.ShowHide ',DbgSName(LCLControl)]);
        {$ENDIF}
        SetWidgetSizeAndPosition(LCLControl);
      end;
    {$ifndef windows}
      UnsetResizeRequest(SenderWidget);
    {$endif}
    end;

    if (ACustomForm<>nil) and (ACustomForm.Parent=nil) then
    begin
      If (ACustomForm.BorderStyle <> bsSizeable) or
        ((ACustomForm.FormStyle in fsAllStayOnTop)
         and (not (csDesigning in ACustomForm.ComponentState)))
      then begin
        Decor := GetWindowDecorations(ACustomForm);
        Func := GetWindowFunction(ACustomForm);
        gtk_widget_realize(SenderWidget);
        AWindow:=GetControlWindow(SenderWidget);
        gdk_window_set_decorations(AWindow, decor);
        gdk_window_set_functions(AWindow, func);
      end;
      ShareWindowAccelGroups(SenderWidget);

      // capturing is always gtkwindow dependent. On showing a new window
      // the gtk will put a new widget on the grab stack.
      // -> release our capture
      ReleaseMouseCapture;
    end;

    if gtk_widget_visible(SenderWidget) then
      exit;

    gtk_widget_show(SenderWidget);
    {$IFDEF GTK2}
    if (ACustomForm<>nil) and (ACustomForm.Parent=nil) then begin
      case ACustomForm.WindowState of
        wsNormal:
        begin
          gtk_window_deiconify(PGtkWindow(SenderWidget));
          gtk_window_unmaximize(PGtkWindow(SenderWidget));
        end;
        wsMaximized: gtk_window_maximize(PGtkWindow(SenderWidget));
        wsMinimized: gtk_window_iconify(PGtkWindow(SenderWidget));
      end;
    end;
    {$ENDIF}

    if (ACustomForm<>nil) and (ACustomForm.Parent=nil)
    then begin
      AWindow:=GetControlWindow(SenderWidget);
      if (AWindow<>nil) and (ACustomForm.Icon<>nil)
      then begin
        FormIconGdiObject := PGDIObject(ACustomForm.GetIconHandle);
        if FormIconGdiObject <> nil
        then begin
          gdk_window_set_icon(AWindow, nil,
                  FormIconGdiObject^.GDIPixmapObject.Image,
                  FormIconGdiObject^.GDIPixmapObject.Mask);
        end;
      end;
    end;
  end
  else begin
    if (ACustomForm<>nil) then begin
      UnshareWindowAccelGroups(SenderWidget);
    end;

    if not gtk_widget_visible(SenderWidget) then
      exit;

    gtk_widget_hide(SenderWidget);

    if GtkWidgetIsA(SenderWidget,GTK_TYPE_WINDOW) then begin
      {$IFDEF VerboseTransient}
      DebugLn('TGtkWidgetSet.ShowHide HIDE ',Sender.ClassName);
      {$ENDIF}
      UntransientWindow(PGtkWindow(SenderWidget));
    end;
  end;

  if GtkWidgetIsA(SenderWidget,GTK_TYPE_WINDOW) then begin
    // make sure when hiding a window, that at least the main window
    // is selectable via the window manager
    if (Application<>nil) and (Application.MainForm<>nil)
    and (Application.MainForm.HandleAllocated) then begin
      SetFormShowInTaskbar(Application.MainForm,stAlways);
    end;
  end;

  //if Sender is TCustomForm then
  //  DebugLn('[TGtkWidgetSet.ShowHide] END ',Sender.ClassName,' Window=',FormWidget^.Window<>nil);
end;

{-------------------------------------------------------------------------------
  method TGtkWidgetSet LoadPixbufFromLazResource
  Params: const ResourceName: string;
          var Pixbuf: PGdkPixbuf
  Result: none

  Loads a pixbuf from a lazarus resource. The resource must be a XPM file.
-------------------------------------------------------------------------------}
procedure TGtkWidgetSet.LoadPixbufFromLazResource(const ResourceName: string;
  var Pixbuf: PGdkPixbuf);
var
  ImgData: PPChar;
begin
  Pixbuf:=nil;
  try
    ImgData:=LazResourceXPMToPPChar(ResourceName);
  except
    on e: Exception do
      DebugLn('WARNING: TGtkWidgetSet.LoadXPMFromLazResource: '+e.Message);
  end;
  {$IFDEF DebugGDK}BeginGDKErrorTrap;{$ENDIF}
  {$IFDEF VerboseGdkPixbuf}
  debugln('LoadPixbufFromLazResource A1');
  {$ENDIF}
  pixbuf:=gdk_pixbuf_new_from_xpm_data(ImgData);
  {$IFDEF VerboseGdkPixbuf}
  debugln('LoadPixbufFromLazResource A2');
  {$ENDIF}
  {$IFDEF DebugGDK}EndGDKErrorTrap;{$ENDIF}
  FreeMem(ImgData);
end;

{-------------------------------------------------------------------------------
  procedure AddDummyNoteBookPage(NoteBookWidget: PGtkNoteBook);

  Adds the dummy page.
  A gtk notebook must have at least one page, but TCustomNoteBook also allows
  no pages at all. Therefore at least a dummy page is added. This dummy page is
  removed as soon as other pages are added.
-------------------------------------------------------------------------------}
procedure TGtkWidgetSet.AddDummyNoteBookPage(NoteBookWidget: PGtkNoteBook);
var
  DummyWidget: PGtkWidget;
  ALabel: PGtkWidget;
  MenuLabel: PGtkWidget;
  {$IFDEF Gtk}
  AWidget: PGtkWidget;
  {$ENDIF}
begin
  if NoteBookWidget=nil then exit;
  DummyWidget:=GetGtkNoteBookDummyPage(NoteBookWidget);
  if (DummyWidget=nil) then begin
    // the notebook has no pages
    // -> add a dummy page
    DummyWidget := gtk_hbox_new(false, 0);
    {$IFDEF Gtk}
    AWidget := CreateFixedClientWidget{$IFNDEF GtkFixedWithWindow}(false){$ENDIF};
    gtk_widget_show(AWidget);
    //gtk_box_pack_start_defaults(GTK_BOX(DummyWidget),AWidget);
    gtk_container_add(GTK_CONTAINER(DummyWidget), AWidget);
    {$ENDIF}
    gtk_widget_show(DummyWidget);
    ALabel:=gtk_label_new('');
    gtk_widget_show(ALabel);
    MenuLabel:=gtk_label_new('');
    gtk_widget_show(MenuLabel);
    gtk_notebook_append_page_menu(NoteBookWidget,DummyWidget,ALabel,MenuLabel);
    SetGtkNoteBookDummyPage(NoteBookWidget,DummyWidget);
  end;
end;

{------------------------------------------------------------------------------
  Method: TGtkWidgetSet.SetPixel
  Params:  Sender : the lcl object which called this func via SendMessage
           Data   : pointer to a TLMSetGetPixel record
  Returns: nothing

  Set the color of the specified pixel on the window?screen?object?
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor);
var
  DC     : TGtkDeviceContext absolute CanvasHandle;
  DCOrigin: TPoint;
  GDKColor: TGDKColor;
begin
  if (DC = nil) or (DC.Drawable = nil) then exit;

  DCOrigin := DC.Offset;
  inc(X,DCOrigin.X);
  inc(Y,DCOrigin.Y);

  DC.SelectedColors := dcscCustom;
  GDKColor := AllocGDKColor(AColor);
  gdk_gc_set_foreground(DC.GC, @GDKColor);
  {$IFDEF DebugGDK}BeginGDKErrorTrap;{$ENDIF}
  gdk_draw_point(DC.Drawable, DC.GC, X, Y);
  {$IFDEF DebugGDK}EndGDKErrorTrap;{$ENDIF}
end;

procedure TGtkWidgetSet.DCRedraw(CanvasHandle: HDC);
var
  fWindow :pGdkWindow;
  widget : PgtkWIdget;
  PixMap : pgdkPixMap;
  Child: PGtkWidget;
begin
  Assert(False, 'Trace:In AutoRedraw in GTKObject');

  Child := PgtkWidget(CanvasHandle);
  Widget := GetFixedWidget(Child);
  pixmap := gtk_Object_get_data(pgtkobject(Child),'Pixmap');
  if PixMap = nil then Exit;
  fWindow := GetControlWindow(widget);

  if fWindow<>nil then begin
    {$IFDEF DebugGDK}BeginGDKErrorTrap;{$ENDIF}
    gdk_draw_pixmap(fwindow,
      gtk_widget_get_style(widget)^.fg_gc[GTK_WIDGET_STATE (widget)],
      pixmap,
      0,0,
      0,0,
      pgtkwidget(widget)^.allocation.width,
      pgtkwidget(widget)^.allocation.height);
    {$IFDEF DebugGDK}EndGDKErrorTrap;{$ENDIF}
  end;
end;

{------------------------------------------------------------------------------
  Method: TGtkWidgetSet.GetPixel
  Params:  Sender : the lcl object which called this func via SenMessage
           Data   : pointer to a TLMSetGetPixel record
  Returns: nothing

  Get the color of the specified pixel on the window?screen?object?
 ------------------------------------------------------------------------------}
function  TGtkWidgetSet.DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor;
var
  DC    : TGtkDeviceContext absolute CanvasHandle;
  Image  : pGDKImage;
  GDKColor: TGDKColor;
  Colormap : PGDKColormap;
  DCOrigin: TPoint;
  MaxX, MaxY: integer;
  Pixel: LongWord;
begin
  Result := clNone;
  if (DC = nil) or (DC.Drawable = nil) then Exit;

  DCOrigin := DC.Offset;
  inc(X,DCOrigin.X);
  inc(Y,DCOrigin.Y);

  gdk_drawable_get_size(DC.Drawable, @MaxX, @MaxY);
  if (X<0) or (Y<0) or (X>=MaxX) or (Y>=MaxY) then exit;

  Image := gdk_drawable_get_image(DC.Drawable,X,Y,1,1);
  if Image = nil then exit;

  {$ifdef Gtk1}
  // previously gdk_image_get_colormap(image) was used, implementation
  // was casting GdkImage to GdkWindow which is not valid and cause AVs
  if gdk_window_get_type(PGdkWindow(DC.Drawable))= GDK_WINDOW_PIXMAP then
    colormap := nil // pixmaps are created with null colormap, get system one instead
  else
    colormap := gdk_window_get_colormap(PGdkWindow(DC.Drawable));
  {$else}
  colormap := gdk_image_get_colormap(image);
  if colormap = nil then
    colormap := gdk_drawable_get_colormap(DC.Drawable);
  {$endif}


  if colormap = nil then
    colormap := gdk_colormap_get_system;

  Pixel:=gdk_image_get_pixel(Image,0,0);
  FillChar(GDKColor, SizeOf(GDKColor),0);
  // does not work with TBitmap.Canvas
  gdk_colormap_query_color(colormap, Pixel, @GDKColor);

  gdk_image_unref(Image);

  Result := TGDKColorToTColor(GDKColor);
end;

{ TODO: move this ``LM_GETVALUE'' spinedit code someplace useful

  csSpinEdit :
    Begin
      Single(Data^):=gtk_spin_button_get_value_As_Float(PgtkSpinButton(Handle));
    end;
}

{------------------------------------------------------------------------------
  Function: IsValidDC
  Params:  DC: a (LCL) devicecontext
  Returns: True if valid

  Checks if the given DC is valid.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.IsValidDC(const DC: HDC): Boolean;
begin
  Result := FDeviceContexts.Contains(Pointer(DC));
end;

{------------------------------------------------------------------------------
  Function: IsValidGDIObject
  Params:  GDIObject: a (LCL) gdiObject
  Returns: True if valid

  Checks if the given GDIObject is valid (e.g. known to the gtk interface).
  This is a quick consistency check to avoid working with dangling pointers.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.IsValidGDIObject(const AGDIObj: HGDIOBJ): Boolean;
var
  GdiObject: PGdiObject absolute AGDIObj;
begin
  Result := (AGDIObj <> 0) and FGDIObjects.Contains(GDIObject);
end;

{------------------------------------------------------------------------------
  Function: IsValidGDIObjectType
  Params:  GDIObject: a (LCL) gdiObject
           GDIType: the requested type
  Returns: True if valid

  Checks if the given GDIObject is valid and the GDItype is the requested type
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.IsValidGDIObjectType(
  const GDIObject: HGDIOBJ; const GDIType: TGDIType): Boolean;
begin
  Result := IsValidGDIObject(GDIObject)
            and (PGdiObject(GDIObject)^.GDIType = GDIType);
end;



{------------------------------------------------------------------------------
  Function: NewDC
  Params:  none
  Returns: a gtkwinapi DeviceContext

  Creates a raw DC and adds it to FDeviceContexts.
  
  Used internally by: CreateCompatibleDC, CreateDCForWidget and SaveDC
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.NewDC: TGtkDeviceContext;
begin
  Assert(False, Format('Trace:> [TGtkWidgetSet.NewDC]', []));
  
  if FDCManager = nil
  then begin
    FDCManager := TDeviceContextMemManager.Create(GetDeviceContextClass);
    FDCManager.MinimumFreeCount := 1000;
  end;
  Result := FDCManager.NewDeviceContext;
  {$IFDEF DebugLCLComponents}
  DebugDeviceContexts.MarkCreated(Result,'TGtkWidgetSet.NewDC');
  {$ENDIF}

  FDeviceContexts.Add(Result);
  
  {$ifdef TraceGdiCalls}
  FillStackAddrs(get_caller_frame(get_frame), @Result.StackAddrs);
  {$endif}
  //DebugLn(['[TGtkWidgetSet.NewDC] ',DbgS(Result),'  ',FDeviceContexts.Count]);
  //  Assert(False, Format('Trace:< [TGtkWidgetSet.NewDC] FDeviceContexts[%d] --> 0x%p', [n, Result]));
end;

function TGTKWidgetSet.FindDCWithGDIObject(GDIObject: PGdiObject
  ): TGtkDeviceContext;
var
  HashItem: PDynHashArrayItem;
  DC: TGtkDeviceContext;
  g: TGDIType;
  Cnt: Integer;
begin
  Result:=nil;
  if GdiObject=nil then exit;
  HashItem:=FDeviceContexts.FirstHashItem;
  Cnt:=0;
  while HashItem<>nil do begin
    DC:=TGtkDeviceContext(HashItem^.Item);
    for g:=Low(TGDIType) to High(TGDIType) do
      if DC.GDIObjects[g]=GdiObject then exit(DC);
    inc(Cnt);
    HashItem:=HashItem^.Next;
  end;
  if Cnt<>FDeviceContexts.Count then
    RaiseGDBException('');
end;

{------------------------------------------------------------------------------
  procedure TGtkWidgetSet.DisposeDC(DC: PDeviceContext);

  Disposes a DC
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.DisposeDC(aDC: TGtkDeviceContext);
begin
  if not FDeviceContexts.Contains(aDC) then Exit;
  
  FDeviceContexts.Remove(aDC);
  
  {$IFDEF DebugLCLComponents}
  DebugDeviceContexts.MarkDestroyed(ADC);
  {$ENDIF}
  FDCManager.DisposeDeviceContext(ADC);
end;

{------------------------------------------------------------------------------
  function TGtkWidgetSet.CreateDCForWidget(TheWidget: PGtkWidget;
    TheWindow: PGdkWindow; WithChildWindows: boolean): HDC;

  Creates an initial DC
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.CreateDCForWidget(AWidget: PGtkWidget; AWindow: PGdkWindow;
  AWithChildWindows: Boolean; ADoubleBuffer: PGdkDrawable): HDC;

var
  DC: TGtkDeviceContext absolute Result;

begin
  DC := NewDC;
  DC.SetWidget(AWidget, AWindow, AWithChildWindows, ADoubleBuffer);
end;

{------------------------------------------------------------------------------
  function TGtkWidgetSet.GetDoubleBufferedDC(Handle: HWND): HDC;
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetDoubleBufferedDC(Handle: HWND): HDC;
var
  Widget: PGtkWidget;
  WidgetInfo: PWinWidgetInfo;
  AWindow: PGdkWindow;
  Width, Height: integer;
  BufferWidth, BufferHeight: integer;
  DoubleBuffer: PGdkPixmap;
  BufferCreated: Boolean;
  DevContext: TGtkDeviceContext absolute Result;
  CaretWasVisible: Boolean;
  MainWidget: PGtkWidget;
  GC: PGdkGC;
  //LCLObject: TObject;
  //x, y: integer;
begin
  Result:=0;
  Widget:=PGtkWidget(Handle);
  {$IFDEF VerboseDoubleBuffer}
  DebugLn('TGtkWidgetSet.GetDoubleBufferedDC ',GetWidgetClassName(Widget));
  {$ENDIF}
  WidgetInfo:=GetWidgetInfo(Widget,true);
  AWindow:=Widget^.Window;
  Width:=Widget^.allocation.width;
  Height:=Widget^.allocation.height;
  // create or resize DoubleBuffer
  DoubleBuffer:=WidgetInfo^.DoubleBuffer;
  if DoubleBuffer<>nil then begin
    gdk_window_get_size(DoubleBuffer,@BufferWidth,@BufferHeight);
    {$IFDEF VerboseDoubleBuffer}
    DebugLn('TGtkWidgetSet.GetDoubleBufferedDC Checking ',
      ' Width=',Width,' Height=',Height,
      ' BufferWidth=',BufferWidth,' BufferHeight=',BufferHeight
      );
    {$ENDIF}
    // lazy update of buffer
    if (BufferWidth<Width) or (BufferHeight<Height)
    or (BufferWidth>(Width*2+20)) or (BufferHeight>(Height*2+20))
    then begin
      {$IFDEF VerboseDoubleBuffer}
      DebugLn('TGtkWidgetSet.GetDoubleBufferedDC Destroying old double buffer ');
      {$ENDIF}
      gdk_pixmap_unref(DoubleBuffer);
      DoubleBuffer:=nil;
      WidgetInfo^.DoubleBuffer:=nil;
    end;
  end;
  BufferCreated:=false;
  if DoubleBuffer=nil then begin
    // create DoubleBuffer
    {$IFDEF VerboseDoubleBuffer}
    DebugLn('TGtkWidgetSet.GetDoubleBufferedDC Creating double buffer ',
      ' Width=',Width,' Height=',Height);
    {$ENDIF}
    DoubleBuffer:=gdk_pixmap_new(AWindow,Width,Height,-1);
    WidgetInfo^.DoubleBuffer := DoubleBuffer;
    BufferCreated:=true;
  end;

  // create DC for double buffer
  Result := CreateDCForWidget(Widget, Widget^.Window, False, DoubleBuffer);


  if BufferCreated
  then begin
    // create GC
    GC:=DevContext.GC;
    // copy old context to buffer
    gdk_gc_set_clip_region(GC, nil);
    gdk_gc_set_clip_rectangle(GC, nil);

    // hide caret
    HideCaretOfWidgetGroup(Widget,MainWidget,CaretWasVisible);
    // copy
    gdk_window_copy_area(DoubleBuffer, GC,0,0,
      Widget^.Window,0,0,Width,Height);

    {LCLObject:=GetParentLCLObject(Widget);
    DebugLn('TGtkWidgetSet.GetDoubleBufferedDC ',DbgS(Widget),8),'=',GetWidgetClassName(Widget),' ',DbgS(Cardinal(LCLObject));
    if (LCLObject is TPanel)
    and (csDesigning in TPanel(LCLObject).ComponentState) then begin
      gdk_window_get_origin(Widget^.Window,@x,@y);
      DebugLn('TGtkWidgetSet.BeginPaint ',TPanel(LCLObject).Name,':',TPanel(LCLObject).ClassName,
        ' Widget=',GetWidgetClassName(Widget),
        ' Origin=',x,',',y,
        ' ',Widget^.allocation.x,',',Widget^.allocation.y);
    end;}

    // restore caret
    if CaretWasVisible then
      GTKAPIWidget_ShowCaret(PGTKAPIWidget(MainWidget));
  end;
  {$IFDEF VerboseDoubleBuffer}
  DebugLn('TGtkWidgetSet.GetDoubleBufferedDC DC=',DbgS(Result));
  {$ENDIF}
end;


{------------------------------------------------------------------------------
  Function: NewGDIObject
  Params:  none
  Returns: a gtkwinapi DeviceContext

  Creates an initial GDIObject of GDIType.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.NewGDIObject(const GDIType: TGDIType): PGdiObject;
begin
  Assert(False, Format('Trace:> [TGtkWidgetSet.NewGDIObject]', []));
  Result:=GtkDef.InternalNewPGDIObject;
  {$ifdef TraceGdiCalls}
  FillStackAddrs(get_caller_frame(get_frame), @Result^.StackAddrs);
  {$endif}
  Result^.GDIType := GDIType;
  inc(Result^.RefCount);
  FGDIObjects.Add(Result);
  //DebugLn('[TGtkWidgetSet.NewGDIObject] ',DbgS(Result),'  ',FGDIObjects.Count);
  Assert(False, Format('Trace:< [TGtkWidgetSet.NewGDIObject] FGDIObjects --> 0x%p', [Result]));
end;

{------------------------------------------------------------------------------
  Function: NewGDIObject
  Params:  GdiObject: PGdiObject
  Returns: none

  Dispose a GdiObject
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.DisposeGDIObject(GDIObject: PGdiObject);
begin
  if FGDIObjects.Contains(GDIObject) then begin
    dec(GDIObject^.RefCount);
    FGDIObjects.Remove(GDIObject);
    GtkDef.InternalDisposePGDIObject(GDIObject);
  end else
    RaiseGDBException('');
end;

{------------------------------------------------------------------------------
  Function: CreateDefaultBrush
  Params:  none
  Returns: a Brush GDIObject

  Creates an default brush, used for initial values
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.CreateDefaultBrush: PGdiObject;
begin
//debugln('  TGtkWidgetSet.CreateDefaultBrush ->');
  Result := NewGDIObject(gdiBrush);
  {$IFDEF DebugGDIBrush}
  debugln('TGtkWidgetSet.CreateDefaultBrush Created: ',DbgS(Result));
  {$ENDIF}
  Result^.GDIBrushFill := GDK_SOLID;
  Result^.GDIBrushColor.ColorRef := 0;
  Result^.GDIBrushColor.Colormap := gdk_colormap_get_system;
  gdk_color_white(Result^.GDIBrushColor.Colormap, @Result^.GDIBrushColor.Color);
  BuildColorRefFromGDKColor(Result^.GDIBrushColor);
end;

{------------------------------------------------------------------------------
  Function: CreateDefaultFont
  Params:  none
  Returns: a Font GDIObject

  Creates an default font, used for initial values
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.CreateDefaultFont: PGdiObject;
var
  CachedFont: TGtkFontCacheDescriptor;
begin
  Result := NewGDIObject(gdiFont);
  Result^.GDIFontObject:=GetDefaultGtkFont(false);
  CachedFont:=FontCache.FindADescriptor(Result^.GDIFontObject);
  if CachedFont<>nil then
    FontCache.Reference(Result^.GDIFontObject)
  else
    FontCache.Add(Result^.GDIFontObject,DefaultLogFont,'');
end;

{------------------------------------------------------------------------------
  Function: CreateDefaultPen
  Params:  none
  Returns: a Pen GDIObject

  Creates an default pen, used for initial values
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.CreateDefaultPen: PGdiObject;
begin
  //write('  TGtkWidgetSet.CreateDefaultPen ->');
  Result := NewGDIObject(gdiPen);
  Result^.GDIPenStyle := PS_SOLID;
  Result^.GDIPenColor.ColorRef := 0;
  Result^.GDIPenColor.Colormap := gdk_colormap_get_system;
  gdk_color_black(Result^.GDIPenColor.Colormap, @Result^.GDIPenColor.Color);
  BuildColorRefFromGDKColor(Result^.GDIPenColor);
end;

function TGTKWidgetSet.CreateDefaultGDIBitmap: PGdiObject;
begin
  Result := NewGDIObject(gdiBitmap);
end;

{------------------------------------------------------------------------------
  procedure TGtkWidgetSet.UpdateDCTextMetric(DC: TGtkDeviceContext);

  Sets the gtk resource file and parses it.
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.UpdateDCTextMetric(DC: TGtkDeviceContext);
const
  TestString: array[boolean] of string = (
    // single byte char font
    '{ABCDEFGHIJKLMNOPQRSTUVWXYZXYZabcdefghijklmnopqrstuvwxyz|_}',
    // double byte char font
    #0'{'#0'A'#0'B'#0'C'#0'D'#0'E'#0'F'#0'G'#0'H'#0'I'#0'J'#0'K'#0'L'#0'M'#0'N'
    +#0'O'#0'P'#0'Q'#0'R'#0'S'#0'T'#0'U'#0'V'#0'W'#0'X'#0'Y'#0'Z'#0'X'#0'Y'#0'Z'
    +#0'a'#0'b'#0'c'#0'd'#0'e'#0'f'#0'g'#0'h'#0'i'#0'j'#0'k'#0'l'#0'm'#0'n'#0'o'
    +#0'p'#0'q'#0'r'#0's'#0't'#0'u'#0'v'#0'w'#0'x'#0'y'#0'z'#0'|'#0'_'#0'}'
    );
var
  UseFont : TGtkIntfFont;
  CachedFont: TGtkFontCacheItem;
  {$IFDEF Gtk1}
  AvgTxtLen: Integer;
  Width: LongInt;
  {$ELSE}
  AWidget: PGtkWidget;
  APangoContext: PPangoContext;
  APangoLanguage: PPangoLanguage;
  Desc: TGtkFontCacheDescriptor;
  APangoFontDescription: PPangoFontDescription;
  APangoMetrics: PPangoFontMetrics;
  aRect: TPangoRectangle;
  {$ENDIF}
begin
  with TGtkDeviceContext(DC) do begin
    if dcfTextMetricsValid in Flags then begin
      // cache valid
      exit;
    end;
    UseFont:=GetGtkFont(TGtkDeviceContext(DC));
    FillChar(DCTextMetric, SizeOf(DCTextMetric), 0);
    CachedFont:=FontCache.FindGTKFont(UseFont);
    if (CachedFont=nil) and (UseFont <> GetDefaultGtkFont(false)) then begin
      DebugLn(['TGtkWidgetSet.UpdateDCTextMetric no CachedFont UseFont=',dbgs(UseFont)]);
      DumpStack;
    end;
    
    if (CachedFont<>nil) and (CachedFont.MetricsValid) then begin
      DCTextMetric.lBearing:=CachedFont.lBearing;
      DCTextMetric.rBearing:=CachedFont.rBearing;
      DCTextMetric.IsDoubleByteChar:=CachedFont.IsDoubleByteChar;
      DCTextMetric.IsMonoSpace:=CachedFont.IsMonoSpace;
      DCTextMetric.TextMetric:=CachedFont.TextMetric;
    end
    else with DCTextMetric do begin
      IsDoubleByteChar:=FontIsDoubleByteCharsFont(UseFont);
      IsMonoSpace:=FontIsMonoSpaceFont(UseFont);
      {$IFDEF Gtk1}
      AvgTxtLen:=length(TestString[false]);
      if IsDoubleByteChar then begin
        gdk_text_extents(UseFont, PChar(TestString[IsDoubleByteChar]),
          AvgTxtLen, @lBearing, @rBearing, @Width,
          @TextMetric.tmAscent, @TextMetric.tmDescent);
        //debugln('TGtkWidgetSet.UpdateDCTextMetric A IsDoubleByteChar=',dbgs(IsDoubleByteChar),' Width=',dbgs(Width),' AvgTxtLen=',dbgs(AvgTxtLen));
        TextMetric.tmHeight := TextMetric.tmAscent+TextMetric.tmDescent;
                // gdk_text_height(UseFont,PChar(TestString[IsDoubleByteChar]),
                //                 AvgTxtLen*2)
                //       {$IfNDef Win32} + TextMetric.tmdescent div 2{$EndIf};
      end else begin
        gdk_text_extents(UseFont, PChar(TestString[IsDoubleByteChar]),
          AvgTxtLen, @lBearing, @rBearing, @Width,
          @TextMetric.tmAscent, @TextMetric.tmDescent);
        TextMetric.tmHeight := TextMetric.tmAscent+TextMetric.tmDescent;
                // gdk_text_height(UseFont,PChar(TestString[IsDoubleByteChar]),
                //              AvgTxtLen)
                //       {$IfNDef Win32} + TextMetric.tmdescent div 2{$EndIf};
      end;
      //if Width<AvgTxtLen then UseWidthHeuristic;
      //TextMetric.tmAscent := TextMetric.tmHeight - TextMetric.tmDescent;
      if IsDoubleByteChar then
        TextMetric.tmAveCharWidth:=Width div (AvgTxtLen div 2)
      else
        TextMetric.tmAveCharWidth:=Width div AvgTxtLen;
      if TextMetric.tmAveCharWidth<1 then TextMetric.tmAveCharWidth:=1;
      TextMetric.tmMaxCharWidth :=
        Max(gdk_char_width(UseFont, 'W'),
            gdk_char_width(UseFont, 'M')); // temp hack
      if TextMetric.tmMaxCharWidth<TextMetric.tmAveCharWidth then
        TextMetric.tmMaxCharWidth:=TextMetric.tmAveCharWidth;
      {$ELSE Gtk2}
      // get pango context (= association to a widget)
      AWidget:=Widget;
      if AWidget=nil then
        AWidget:=GetStyleWidget(lgsLabel);
      APangoContext := gtk_widget_get_pango_context(AWidget);
      if APangoContext=nil then
        DebugLn(['TGtkWidgetSet.UpdateDCTextMetric WARNING: no pango context']);
      // get pango language (e.g. de_DE)
      APangoLanguage := pango_context_get_language(APangoContext);
      if APangoLanguage=nil then
        DebugLn(['TGtkWidgetSet.UpdateDCTextMetric WARNING: no pango language']);
      // get pango font description (e.g. 'sans 12')
      if CachedFont<>nil then begin
        Desc:=FontCache.FindADescriptor(UseFont);
        if Desc<>nil then
          APangoFontDescription := Desc.PangoFontDescription;
        //DebugLn(['TGtkWidgetSet.UpdateDCTextMetric CachedFont Desc.PangoFontDescription=',GetPangoDescriptionReport(APangoFontDescription)]);
      end;
      if APangoFontDescription=nil then
        APangoFontDescription:=GetDefaultFontDesc(false);
      if APangoFontDescription=nil then
        DebugLn(['TGtkWidgetSet.UpdateDCTextMetric WARNING: no pango font description']);
      //DebugLn(['TGtkWidgetSet.UpdateDCTextMetric APangoFontDescription=',GetPangoDescriptionReport(APangoFontDescription)]);
      // get pango metrics (e.g. ascent, descent)
      APangoMetrics := pango_context_get_metrics(APangoContext,
                                       APangoFontDescription, APangoLanguage);
      if APangoMetrics=nil then
        DebugLn(['TGtkWidgetSet.UpdateDCTextMetric WARNING: no pango metrics']);

      TextMetric.tmAveCharWidth := Max(1,
                 pango_font_metrics_get_approximate_char_width(APangoMetrics)
                 div PANGO_SCALE);
      TextMetric.tmAscent := pango_font_metrics_get_ascent(APangoMetrics) div PANGO_SCALE;
      TextMetric.tmDescent := pango_font_metrics_get_descent(APangoMetrics) div PANGO_SCALE;
      TextMetric.tmHeight := TextMetric.tmAscent+TextMetric.tmDescent;

      pango_layout_set_text(UseFont, PChar(TestString[IsDoubleByteChar]),
                            length(PChar(TestString[IsDoubleByteChar])));
      pango_layout_get_extents(UseFont, nil, @aRect);

      lBearing := PANGO_LBEARING(aRect) div PANGO_SCALE;
      rBearing := PANGO_RBEARING(aRect) div PANGO_SCALE;

      pango_layout_set_text(UseFont, 'M', 1);
      pango_layout_get_pixel_size(UseFont, @aRect.width, @aRect.height);
      TextMetric.tmMaxCharWidth := Max(1,aRect.width);
      pango_layout_set_text(UseFont, 'W', 1);
      pango_layout_get_pixel_size(UseFont, @aRect.width, @aRect.height);
      TextMetric.tmMaxCharWidth := Max(TextMetric.tmMaxCharWidth,aRect.width);

      pango_font_metrics_unref(APangoMetrics);
      {$ENDIF}
      (*debugln('TGtkWidgetSet.UpdateDCTextMetric A IsDoubleByteChar=',dbgs(IsDoubleByteChar),
        ' lbearing=',dbgs(lBearing),' rbearing=',dbgs(rBearing),
        {$IFDEF Gtk1}
        ' width='+dbgs(width),
        ' AvgTxtLen='+dbgs(AvgTxtLen),
        {$ENDIF}
        ' tmAscent='+dbgs(TextMetric.tmAscent),
        ' tmDescent='+dbgs(TextMetric.tmdescent),
        ' tmHeight='+dbgs(TextMetric.tmHeight),
        ' tmMaxCharWidth='+dbgs(TextMetric.tmMaxCharWidth),
        ' tmAveCharWidth='+dbgs(TextMetric.tmAveCharWidth));*)
      if (CachedFont<>nil) then begin
        CachedFont.lBearing:=lBearing;
        CachedFont.rBearing:=rBearing;
        CachedFont.IsDoubleByteChar:=IsDoubleByteChar;
        CachedFont.IsMonoSpace:=IsMonoSpace;
        CachedFont.TextMetric:=TextMetric;
        CachedFont.MetricsValid:=true;
      end;
    end;
    Flags := Flags + [dcfTextMetricsValid];
  end;
end;

{$Ifdef GTK2}
{------------------------------------------------------------------------------
  function TGtkWidgetSet.GetDefaultFontDesc(IncreaseReferenceCount: boolean
    ): PPangoFontDescription;
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetDefaultFontDesc(IncreaseReferenceCount: boolean
  ): PPangoFontDescription;
begin
  if FDefaultFontDesc = nil then begin
    FDefaultFontDesc:=LoadDefaultFontDesc;
    if FDefaultFontDesc = nil then
      raise EOutOfResources.Create(rsUnableToLoadDefaultFont);
  end;
  Result:=FDefaultFontDesc;
  if IncreaseReferenceCount then
    Result := pango_font_description_copy(Result);
end;
{$Endif}

{------------------------------------------------------------------------------
  function TGtkWidgetSet.GetDefaultGtkFont(IncreaseReferenceCount: boolean
    ): TGtkIntfFont;
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetDefaultGtkFont(IncreaseReferenceCount: boolean
  ): TGtkIntfFont;
begin
  if FDefaultFont = nil then begin
    FDefaultFont:=LoadDefaultFont;
    if FDefaultFont = nil then
      raise EOutOfResources.Create(rsUnableToLoadDefaultFont);
    ReferenceGtkIntfFont(FDefaultFont); // mark as used globally
  end;
  Result:=FDefaultFont;
  if IncreaseReferenceCount then
    ReferenceGtkIntfFont(Result); // mark again
end;

function TGTKWidgetSet.GetGtkFont(DC: TGtkDeviceContext): TGtkIntfFont;
begin
  {$IFDEF Gtk}
  if (DC.CurrentFont = nil) or (DC.CurrentFont^.GDIFontObject = nil)
  then begin
    Result := GetDefaultGtkFont(false);
  end
  else begin
    Result := DC.CurrentFont^.GDIFontObject;
  end;
  {$ELSE}
  // create font if needed
  Result:=DC.GetFont^.GDIFontObject;
  {$ENDIF}
end;

function TGtkWidgetSet.CreateRegionCopy(SrcRGN: hRGN): hRGN;
var
  GDIObject: PGDIObject;
begin
  GDIObject := NewGDIObject(gdiRegion);
  GDIObject^.GDIRegionObject:=gdk_region_copy(PGdiObject(SrcRGN)^.GDIRegionObject);
  Result := hRgn(PtrUInt(GDIObject));
end;

function TGtkWidgetSet.DCClipRegionValid(DC: HDC): boolean;
var
  CurClipRegion: hRGN;
begin
  Result:=false;
  if not IsValidDC(DC) then exit;
  CurClipRegion:=HRGN(PtrUInt(TGtkDeviceContext(DC).ClipRegion));
  if (CurClipRegion<>0) and (not IsValidGDIObject(CurClipRegion)) then exit;
  Result:=true;
end;

function TGtkWidgetSet.CreateEmptyRegion: hRGN;
var
  GObject: PGdiObject;
begin
  GObject := NewGDIObject(gdiRegion);
  GObject^.GDIRegionObject := gdk_region_new;
  Result := HRGN(PtrUInt(GObject));
  //DebugLn('TGtkWidgetSet.CreateEmptyRgn A RGN=',DbgS(Result));
end;

{------------------------------------------------------------------------------
  Function: SetRCFilename
  Params:  const AValue: string
  Returns: none

  Sets the gtk resource file and parses it.
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.SetRCFilename(const AValue: string);
begin
  if (FRCFilename=AValue) then exit;
  FRCFilename:=AValue;
  FRCFileParsed:=false;
  ParseRCFile;
end;

{------------------------------------------------------------------------------
  procedure TGtkWidgetSet.CheckRCFilename;

  Sets the gtk resource file and parses it.
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.CheckRCFilename;
begin
  if FRCFileParsed and (FRCFilename<>'') and FileExists(FRCFilename)
  and (FileAge(FRCFilename)<>FRCFileAge) then
    FRCFileParsed:=false;
end;

{------------------------------------------------------------------------------
  Function: ParseRCFile
  Params:  const AValue: string
  Returns: none

  Sets the gtk resource file and parses it.
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.ParseRCFile;
begin
  if (not FRCFileParsed)
  and (FRCFilename<>'') and FileExists(FRCFilename) then
  begin
    gtk_rc_parse(PChar(FRCFilename));
    FRCFileParsed:=true;
    FRCFileAge:=FileAge(FRCFilename);
  end;
end;

{------------------------------------------------------------------------------
  Function: SetClipboardWidget
  Params: TargetWidget: PGtkWidget - This widget will be connected to all
                  clipboard signals which are all handled by the TGtkWidgetSet
                  itself.
  Returns: none

  All supported targets are added to the new widget. This way, no one,
  especially not the lcl, will notice the change. ;)
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.SetClipboardWidget(TargetWidget: PGtkWidget);
type
  TGtkTargetSelectionList = record
    Selection: Cardinal;
    List: PGtkTargetList;
  end;
  PGtkTargetSelectionList = ^TGtkTargetSelectionList;
const
  gtk_selection_handler_key: PChar = 'gtk-selection-handlers';

  {$IFDEF DEBUG_CLIPBOARD}
  function gtk_selection_target_list_get(Widget: PGtkWidget;
    ClipboardType: TClipboardType): PGtkTargetList;
  var
    SelectionLists, CurSelList: PGList;
    TargetSelList: PGtkTargetSelectionList;
  begin
    SelectionLists := gtk_object_get_data (PGtkObject(Widget),
                                          gtk_selection_handler_key);
    CurSelList := SelectionLists;
    while (CurSelList<>nil) do begin
      TargetSelList := CurSelList^.Data;
      if (TargetSelList^.Selection = ClipboardTypeAtoms[ClipboardType]) then
      begin
        Result:=TargetSelList^.List;
        exit;
      end;
      CurSelList := CurSelList^.Next;
    end;
    Result:=nil;
  end;

  procedure WriteTargetLists(Widget: PGtkWidget);
  var c: TClipboardType;
    TargetList: PGtkTargetList;
    TmpList: PGList;
    Pair: PGtkTargetPair;
  begin
    DebugLn('  WriteTargetLists WWW START');
    for c:=Low(TClipboardType) to High(TClipboardType) do begin
      TargetList:=gtk_selection_target_list_get(Widget,c);
      DebugLn('  WriteTargetLists WWW ',ClipboardTypeName[c],' ',dbgs(TargetList<>nil));
      if TargetList<>nil then begin
        TmpList:=TargetList^.List;
        while TmpList<>nil do begin
          Pair:=PGtkTargetPair(TmpList^.Data);
          DebugLn('    WriteTargetLists BBB ',dbgs(Pair^.Target),' ',GdkAtomToStr(Pair^.Target));
          TmpList:=TmpList^.Next;
        end;
      end;
    end;
    DebugLn('  WriteTargetLists WWW END');
  end;
  {$ENDIF}

  procedure ClearTargetLists(Widget: PGtkWidget);
  // MG: Reading in gtk internals is dirty, but there seems to be no other way
  //     to clear the old target lists
  var
    SelectionLists, CurSelList: PGList;
    TargetSelList: PGtkTargetSelectionList;
  begin
    {$IFDEF DEBUG_CLIPBOARD}
    DebugLn('  ClearTargetLists WWW START');
    {$ENDIF}
    SelectionLists := gtk_object_get_data (PGtkObject(Widget),
                                          gtk_selection_handler_key);
    CurSelList := SelectionLists;
    while (CurSelList<>nil) do begin
      TargetSelList := CurSelList^.Data;
      gtk_target_list_unref(TargetSelList^.List);
      g_free(TargetSelList);
      CurSelList := CurSelList^.Next;
    end;
    g_list_free(SelectionLists);
    gtk_object_set_data (PGtkObject(Widget),gtk_selection_handler_key,GtkNil);
    {$IFDEF DEBUG_CLIPBOARD}
    DebugLn('  ClearTargetLists WWW END');
    {$ENDIF}
  end;

var c: TClipboardType;
begin
  if ClipboardWidget=TargetWidget then exit;
  {$IFDEF DEBUG_CLIPBOARD}
  DebugLn('[TGtkWidgetSet.SetClipboardWidget] ',dbgs(ClipboardWidget<>nil),' -> ',dbgs(TargetWidget<>nil),' ',GetWidgetDebugReport(TargetWidget));
  {$ENDIF}
  if ClipboardWidget<>nil then begin
    {$IFDEF DEBUG_CLIPBOARD}
    WriteTargetLists(ClipboardWidget);
    {$ENDIF}
    ClearTargetLists(ClipboardWidget);
    {$IFDEF DEBUG_CLIPBOARD}
    WriteTargetLists(ClipboardWidget);
    {$ENDIF}
  end;

  ClipboardWidget:=TargetWidget;
  if ClipboardWidget<>nil then begin
    // connect widget to all clipboard signals
    g_signal_connect(PGtkObject(ClipboardWidget),'selection_received',
      TGTKSignalFunc(@ClipboardSelectionReceivedHandler),GtkNil);
    g_signal_connect(PGtkObject(ClipboardWidget),'selection_get',
      TGTKSignalFunc(@ClipboardSelectionRequestHandler),GtkNil);
    g_signal_connect(PGtkObject(ClipboardWidget),'selection_clear_event',
      TGTKSignalFunc(@ClipboardSelectionLostOwnershipHandler),GtkNil);
    // add all supported targets for all clipboard types
    for c:=Low(TClipboardType) to High(TClipboardType) do begin
      if (ClipboardTargetEntries[c]<>nil) then begin
        //DebugLn('TGtkWidgetSet.SetClipboardWidget ',GdkAtomToStr(ClipboardTypeAtoms[c]),' Entries=',dbgs(ClipboardTargetEntryCnt[c]));
        gtk_selection_add_targets(ClipboardWidget,ClipboardTypeAtoms[c],
                  ClipboardTargetEntries[c],ClipboardTargetEntryCnt[c]);
      end;
    end;
    {$IFDEF DEBUG_CLIPBOARD}
    WriteTargetLists(ClipboardWidget);
    {$ENDIF}
  end;
end;

{------------------------------------------------------------------------------
  procedure TGtkWidgetSet.WordWrap(AText: PChar; MaxWidthInPixel: integer;
    var Lines: PPChar; var LineCount: integer); virtual;

  Breaks AText into several lines and creates a list of PChar. The last entry
  will be nil.
  Lines break at new line chars and at spaces if a line is longer than
  MaxWidthInPixel or in a word.
  Lines will be one memory block so that you can free the list and all lines
  with FreeMem(Lines).
------------------------------------------------------------------------------}
procedure TGtkWidgetSet.WordWrap(DC: HDC; AText: PChar;
  MaxWidthInPixel: integer; var Lines: PPChar; var LineCount: integer);
var
  UseFont: TGtkIntfFont;

  function GetLineWidthInPixel(LineStart, LineLen: integer): integer;
  var
    width: LongInt;
  begin
    GetTextExtentIgnoringAmpersands(UseFont, @AText[LineStart], LineLen,
                                    nil, nil, @width, nil, nil);
    Result:=Width;
  end;

  function FindLineEnd(LineStart: integer): integer;
  var
    CharLen,
    LineStop,
    LineWidth, WordWidth, WordEnd, CharWidth: integer;
  begin
    // first search line break or text break
    Result:=LineStart;
    while not (AText[Result] in [#0,#10,#13]) do inc(Result);
    if Result<=LineStart+1 then exit;
    lineStop:=Result;

    // get current line width in pixel
    LineWidth:=GetLineWidthInPixel(LineStart,Result-LineStart);
    if LineWidth>MaxWidthInPixel then begin
      // line too long
      // -> add words till line size reached
      LineWidth:=0;
      WordEnd:=LineStart;
      WordWidth:=0;
      repeat
        Result:=WordEnd;
        inc(LineWidth,WordWidth);
        // find word start
        while AText[WordEnd] in [' ',#9] do inc(WordEnd);
        // find word end
        while not (AText[WordEnd] in [#0,' ',#9,#10,#13]) do inc(WordEnd);
        // calculate word width
        WordWidth:=GetLineWidthInPixel(Result,WordEnd-Result);
      until LineWidth+WordWidth>MaxWidthInPixel;
      if LineWidth=0 then begin
        // the first word is longer than the maximum width
        // -> add chars till line size reached
        Result:=LineStart;
        LineWidth:=0;
        repeat
          charLen:=UTF8CharacterLength(@AText[result]);
          CharWidth:=GetLineWidthInPixel(Result,charLen);
          inc(LineWidth,CharWidth);
          if LineWidth>MaxWidthInPixel then break;
          if result>=lineStop then break;
          inc(Result,charLen);
        until false;
        // at least one char
        if Result=LineStart then begin
          charLen:=UTF8CharacterLength(@AText[result]);
          inc(Result,charLen);
        end;
      end;
    end;
  end;

  function IsEmptyText: boolean;
  begin
    if (AText=nil) or (AText[0]=#0) then begin
      // no text
      GetMem(Lines,SizeOf(PChar));
      Lines[0]:=nil;
      LineCount:=0;
      Result:=true;
    end else
      Result:=false;
  end;

  procedure InitFont;
  begin
    UseFont:=GetGtkFont(TGtkDeviceContext(DC));
  end;

var
  LinesList: TFPList;
  LineStart, LineEnd, LineLen: integer;
  ArraySize, TotalSize: integer;
  i: integer;
  CurLineEntry: PPChar;
  CurLineStart: PChar;
begin
  if IsEmptyText then exit;
  InitFont;
  LinesList:=TFPList.Create;
  LineStart:=0;

  // find all line starts and line ends
  repeat
    LinesList.Add(Pointer(PtrInt(LineStart)));
    // find line end
    LineEnd:=FindLineEnd(LineStart);
    LinesList.Add(Pointer(PtrInt(LineEnd)));
    // find next line start
    LineStart:=LineEnd;
    if AText[LineStart] in [#10,#13] then begin
      // skip new line chars
      inc(LineStart);
      if (AText[LineStart] in [#10,#13])
      and (AText[LineStart]<>AText[LineStart-1]) then
        inc(LineStart);
    end else if AText[LineStart] in [' ',#9] then begin
      // skip space
      while AText[LineStart] in [' ',#9] do
        inc(LineStart);
    end;
  until AText[LineStart]=#0;

  // create mem block for 'Lines': array of PChar + all lines
  LineCount:=LinesList.Count shr 1;
  ArraySize:=(LineCount+1)*SizeOf(PChar);
  TotalSize:=ArraySize;
  i:=0;
  while i<LinesList.Count do begin
    // add  LineEnd - LineStart + 1 for the #0
    LineLen:=PtrUInt(LinesList[i+1])-PtrUInt(LinesList[i])+1;
    inc(TotalSize,LineLen);
    inc(i,2);
  end;
  GetMem(Lines,TotalSize);
  FillChar(Lines^,TotalSize,0);

  // create Lines
  CurLineEntry:=Lines;
  CurLineStart:=PChar(CurLineEntry)+ArraySize;
  i:=0;
  while i<LinesList.Count do begin
    // set the pointer to the start of the current line
    CurLineEntry[i shr 1]:=CurLineStart;
    // copy the line
    LineStart:=integer(PtrUInt(LinesList[i]));
    LineEnd:=integer(PtrUInt(LinesList[i+1]));
    LineLen:=LineEnd-LineStart;
    if LineLen>0 then
      Move(AText[LineStart],CurLineStart^,LineLen);
    inc(CurLineStart,LineLen);
    // add #0 as line end
    CurLineStart^:=#0;
    inc(CurLineStart);
    // next line
    inc(i,2);
  end;
  if PtrUInt(CurLineStart)-PtrUInt(Lines)<>TotalSize then
    RaiseGDBException('TGtkWidgetSet.WordWrap Consistency Error:'
      +' Lines+TotalSize<>CurLineStart');
  CurLineEntry[i shr 1]:=nil;

  LinesList.Free;
end;

function TGtkWidgetSet.ForceLineBreaks(DC: hDC; Src: PChar;
  MaxWidthInPixels: Longint;
  ConvertAmpersandsToUnderScores: Boolean) : PChar;
var
  Lines : PPChar;
  I, NumLines : Longint;
  TmpStr : PGString;
  Line : PgChar;
begin
  TmpStr := nil;
  WordWrap(DC, Src, MaxWidthInPixels, Lines, NumLines);
  For I := 0 to NumLines - 1 do begin
    If TmpStr <> nil then
      g_string_append_c(TmpStr, #10);

    If ConvertAmpersandsToUnderScores then begin
      Line := Ampersands2Underscore(Lines[I]);
      If Line <> nil then begin
        If TmpStr <> nil then begin
          g_string_append(TmpStr, Line);
        end
        else
          TmpStr := g_string_new(Line);
        StrDispose(Line);
      end;
    end
    else begin
      If Lines[I] <> nil then
        If TmpStr <> nil then
          g_string_append(TmpStr, Lines[I])
        else
          TmpStr := g_string_new(Lines[I]);
    end;
  end;
  ReallocMem(Lines, 0);
  If TmpStr <> nil then
    Result := StrNew(TmpStr^.str)
  else
    Result:=nil;
end;

{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}
